(ns opendata.util
  (:require [clj-time.local :as local]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:import (org.joda.time DateTime)))


(defn update+
  "Same as clojure.core/update but runs f only if m contains k"
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn to-int [str]
  (Integer/parseInt str))

(defn to-date [str]
  (local/to-local-date-time  str))

(defn to-float [str]
  (Float/parseFloat str))

(defn to-boolean [str]
  (if-not (instance? Boolean str)
    (Boolean/parseBoolean str)
    str))

(defn coerce-crew [obj]
  (-> obj
      (select-keys [:ids :id :name :on_call :location :phone_number :created_at])
      (update+ :ids #(map to-int %))
      (update+ :id to-int)
      (update+ :on_call to-boolean)
      (update+ :location #(take 2 %))
      (update+ :created_at to-date)))

(defn coerce-call [obj]
  (-> obj
      (select-keys [:ids :id :name :on_call :location :phone_number :created_at])
      (update+ :ids #(map to-int %))
      (update+ :id to-int)
      (update+ :user_id to-int)
      (update+ :crew_id to-int)
      (update+ :is_finished to-boolean)
      (update+ :location #(take 2 %))
      (update+ :created_at to-date)))


(defn coerce-user [obj]
  (-> obj
      (select-keys [:ids :id :name :on_call :location :phone_number :created_at])
      (update+ :ids #(map to-int %))
      (update+ :id to-int)
      (update+ :location #(->> (take 2 %)
                               (map to-float)))
      (update+ :created_at to-date)))

(defn coerce-dispatcher [obj]
  (-> obj
      (select-keys [:ids :id :name :on_call :location :phone_number :created_at])
      (update+ :ids #(map to-int %))
      (update+ :id to-int)
      (update+ :created_at to-date)))


(defn coerce [obj]
  (-> obj
      (select-keys [:crew_id
                    :phone_number
                    :password
                    :name
                    :ids
                    :login
                    :is_finished
                    :on_call
                    :id
                    :user_id
                    :location
                    :created_at
                    :telegram_id])
      (update+ :ids #(map to-int %))
      (update+ :id to-int)
      (update+ :user_id to-int)
      (update+ :crew_id to-int)
      (update+ :is_finished to-boolean)
      (update+ :location #(take 2 %))
      (update+ :created_at to-date)
      (update+ :chat_id to-int)
      (update+ :log_msg_id to-int)
      (update+ :telegram_id to-int)))
