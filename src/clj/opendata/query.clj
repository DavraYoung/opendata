(ns opendata.query
  (:refer-clojure :exclude [update select format partition-by group-by])
  (:require [honeysql.core :as sql]
            [honeysql.format :refer :all]
            [honeysql.helpers :refer :all]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            [clojure.string :as str])
  (:import clojure.lang.IDeref))


(swap! clause-store assoc :cross-join 135)


(defmethod format-clause :cross-join [[_ join-groups] _]
  (space-join (map #(str "cross join " (to-sql %))
                   join-groups)))


(defmethod fn-handler "str-in"
  [_ column str-seq]
  (let [clause
        (let [str-seq (filter some? str-seq)]
          (str
            (to-sql column)
            " in "
            (to-sql (if (seq str-seq) str-seq [""]))))]
    (if (some nil? str-seq)
      (str "(" (to-sql column) " is null or " clause ")")
      clause)))


(defn raw-value
  [value]
  (reify IDeref
    (deref [_] value)))


(defn update+
  "Same as clojure.core/update but runs f only if m contains k"
  [m k f]
  (if (contains? m k)
    (assoc m k (f (get m k)))
    m))


(defn escape-like [s]
  (-> s
      (str/replace "%" "\\%")
      (str/replace "_" "\\_")))


(defmethod fn-handler "nil-in"
  [_ column xs]
  (str "(" (->> [(if-let [xs (->> xs (filter some?) seq)]
                   (str (to-sql column) " in " (to-sql xs)))
                 (when (some nil? xs)
                   (str (to-sql column) " is null"))]
                (filter some?)
                (str/join " or "))
       ")"))


(defn- uuid-pad
  "Ensures that the given uuid sequence contains at least one element"
  [uuid-seq]
  (if (seq uuid-seq)
    (set uuid-seq)
    [#uuid "00000000-0000-0000-0000-000000000000"]))


(defmethod fn-handler "uuid-in"
  [_ column uuid-seq]
  (str (to-sql column) " in " (to-sql (uuid-pad uuid-seq))))


(defn apply-pagination
  "Apply limit and offset to the given sql-map"
  [sql-map {:keys [size page] :as pagination}]
  (if (some? pagination)
    (-> sql-map
        (offset (* size (dec page)))
        (limit size))
    sql-map))


(defn for-update
  "Add for update clause to the given sql-map"
  [sql-map]
  (-> sql-map
      (lock :mode :update)))


(defn do-update-set* [sql-map columns]
  (apply do-update-set sql-map columns))


(defn count-value
  [table & conditions]
  (reduce
    (fn [sql-map condition]
      (merge-where sql-map condition))
    (-> (select :%count.*)
        (from table))
    conditions))


(defn where-user
  [sql-map {:keys [ids id telegram_id location phone_number]}]
  (-> sql-map
      (merge-where (when (and (some? ids) (sequential? ids)) [:in :users.id ids]))
      (merge-where (when (some? id) [:= :users.id id]))
      (merge-where (when (some? telegram_id) [:= :users.telegram_id telegram_id]))
      (merge-where (when (some? phone_number) [:= :users.phone_number phone_number]))
      (merge-where (when (and (some? location) (sequential? location)) [:= :users.location location]))))


(defn where-crew
  [sql-map {:keys [ids id telegram_id location phone_number]}]
  (-> sql-map
      (merge-where (when (and (some? ids) (sequential? ids)) [:in :crew.id ids]))
      (merge-where (when (some? id) [:= :crew.id id]))
      (merge-where (when (some? telegram_id) [:= :crew.telegram_id telegram_id]))
      (merge-where (when (some? phone_number) [:= :crew.phone_number phone_number]))
      (merge-where (when (and (some? location) (sequential? location)) [:= :crew.location location]))))


(defn where-call
  [sql-map {:keys [ids id user_id crew_id is_finished created_at]}]
  (-> sql-map
      (merge-where (when (and (some? ids) (sequential? ids)) [:in :call.id ids]))
      (merge-where (when (some? id) [:= :call.id id]))
      (merge-where (when (some? user_id) [:= :call.user_id user_id]))
      (merge-where (when (some? crew_id) [:= :call.crew_id crew_id]))
      (merge-where (when (some? is_finished) [:= :call.is_finished is_finished]))
      (merge-where (when (some? created_at) [:= :call.created_at created_at]))))


(defn where-dispatcher
  [sql-map {:keys [ids id name login password created_at]}]
  (-> sql-map
      (merge-where (when (and (some? ids) (sequential? ids)) [:in :dispatcher.id ids]))
      (merge-where (when (some? id) [:= :dispatcher.id id]))
      (merge-where (when (some? name) [:= :dispatcher.name name]))
      (merge-where (when (some? login) [:= :dispatcher.login login]))
      (merge-where (when (some? password) [:= :dispatcher.password password]))
      (merge-where (when (some? created_at) [:= :dispatcher.created_at created_at]))))

(defn select-users
  ([filter]
   (select-users filter nil))
  ([filter pagination]
   (-> (select :users.*)
       (from :users)
       (where-user filter)
       (apply-pagination pagination)
       (order-by [:created_at :desc]))))


(defn select-crews
  ([filter]
   (select-crews filter nil))
  ([filter pagination]
   (-> (select :crew.*)
       (from :crew)
       (where-crew filter)
       (apply-pagination pagination)
       (order-by [:created_at :desc]))))


(defn select-calls
  ([filter]
   (select-calls filter nil))
  ([filter pagination]
   (-> (select :call.*)
       (from :call)
       (where-call filter)
       (apply-pagination pagination)
       (order-by [:created_at :desc]))))


(defn select-dispatchers
  ([filter]
   (select-dispatchers filter nil))
  ([filter pagination]
   (-> (select :dispatcher.*)
       (from :dispatcher)
       (where-dispatcher filter)
       (apply-pagination pagination)
       (order-by [:created_at :desc]))))


(defn insert-user
  [user]
  (-> (insert-into :users)
      (values [user])))


(defn insert-dispatcher
  [dispatcher]
  (-> (insert-into :dispatcher)
      (values [dispatcher])))

(defn insert-call
  [call]
  (-> (insert-into :call)
      (values [call])))

(defn insert-crew
  [crew]
  (-> (insert-into :crew)
      (values [crew])))


(defn update-dispatcher
  [crew]
  (-> (update :crew)
      (sset crew)
      (where [:= :id (:id crew)])))

(defn update-crew
  [crew]
  (-> (update :crew)
      (sset crew)
      (where [:= :id (:id crew)])))

(defn update-call
  [call]
  (-> (update :call)
      (sset call)
      (where [:= :id (:id call)])))

(defn update-user
  [user]
  (-> (update :users)
      (sset user)
      (where [:= :id (:id user)])))

(defn update-dispatcher
  [dispatcher]
  (-> (update :dispatcher)
      (sset dispatcher)
      (where [:= :id (:id dispatcher)])
      (returning :*)))





