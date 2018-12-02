(ns opendata.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [compojure.api.meta :refer [restructure-param]]
            [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth :refer [authenticated?]]
            [opendata.query :as query]
            [opendata.db.core :as db]
            [opendata.util :as util]
            [compojure.api.exception :as ex]))

(defn access-error [_ _]
  (unauthorized {:error "unauthorized"}))

(defn wrap-restricted [handler rule]
  (restrict handler {:handler  rule
                     :on-error access-error}))

(defmethod restructure-param :auth-rules
  [_ rule acc]
  (update-in acc [:middleware] conj [wrap-restricted rule]))

(defmethod restructure-param :current-user
  [_ binding acc]
  (update-in acc [:letks] into [binding `(:identity ~'+compojure-api-request+)]))

(defapi service-routes
  {:swagger {:ui   "/swagger-ui"
             :spec "/swagger.json"
             :data {:info {:version     "1.0.0"
                           :title       "Sample API"
                           :description "Sample Services"}}}}
  (context "/api" []
    :tags ["OpenData"]


    (GET "/user" []
      :summary "returns array of crews"
      :query-params [{ids :- [String] nil}
                     {id :- String nil}
                     {name :- String nil}
                     {on_call :- Boolean false}
                     {location :- [String] 0.0}
                     {phone_number :- String "123"}
                     {created_at :- String "2018-12-02T07:11:26.341"} :as req]
      (ok (-> req
              (util/coerce)
              (query/select-crews)
              (db/query))))


    (GET "/crew" []
      :query-params [{ids :- [String] nil}
                     {id :- String nil}
                     {name :- String nil}
                     {on_call :- Boolean nil}
                     {location :- [String] nil}
                     {phone_number :- String nil}
                     {created_at :- String nil} :as req]
      :summary "returns array of crews"
      (ok (-> req
              (util/coerce)
              (query/select-crews)
              (db/query))))


    (GET "/dispatcher" []
      :query-params [{ids :- [String] nil}
                     {id :- String nil}
                     {phone_number :- String nil}
                     {login :- String nil}
                     {password :- String nil}
                     {created_at :- String nil}  :as req]
      :summary "returns array of dispatchers"
      (ok (-> req
              (util/coerce)
              (query/select-dispatchers)
              (db/query))))

    (GET "/call" []
      :query-params [{ids :- [String] nil}
                     {id :- String nil}
                     {user_id :- String nil}
                     {crew_id :- String nil}
                     {is_finished :- Boolean nil}
                     {created_at :- String nil}  :as req]
      :summary "returns array of crews"
      (ok (-> req
              (util/coerce)
              (query/select-calls)
              (db/query))))

    (GET "/call/:id" [id]
      :summary "return call"
      (ok (if-let [crew (->
                          (util/coerce {:id id})
                          (query/select-calls)
                          (db/query)
                          (first))]
            crew
            (ring.util.response/not-found {}))))

    (GET "/user/:id" [id]
      :summary "return user"
      (ok (if-let [crew (->
                          (util/coerce {:id id})
                          (query/select-users)
                          (db/query)
                          (first))]
            crew
            (ring.util.response/not-found {}))))

    (GET "/dispatcher/:id" [id]
      :summary "return dispatcher"
      (ok (if-let [dispatcher (->
                                (util/coerce {:id id})
                                (query/select-dispatchers)
                                (db/query)
                                (first))]
            dispatcher
            (ring.util.response/not-found {}))))

    (GET "/crew/:id" [id]
      :summary "return crew"
      (ok (if-let [crew (->
                          (util/coerce {:id id})
                          (query/select-crews)
                          (db/query)
                          (first))]
            crew
            (ring.util.response/not-found {}))))


    (POST "/crew/:id" [id]
      :query-params [{user_id :- String nil}
                     {crew_id :- String nil}
                     {is_finished :- Boolean false}
                     {created_at :- String nil}  :as req]
      :summary "update crew"
      (if (->
            (util/coerce (assoc req :id id))
            (query/update-crew)
            (db/query))
        (ok {:result "ok"})
        (ring.util.response/bad-request {})))


    (PUT "/crew/" []
      :query-params [{name :- String nil}
                     {on_call :- Boolean false}
                     {created_at :- String nil}
                     {chat_id :- String nil}
                     {log_msg_id :- String nil}:as req]
      :summary "insert crew"
      (prn req)
      (if (->
            (util/coerce req)
            (query/insert-crew)
            (db/execute))
        (ok {:result "ok"})
        (ring.util.response/bad-request {})))))
