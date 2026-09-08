(ns todo.routes
  (:require [fx.core :as fx]
            [fx.ring :as fx-ring]
            [fx.ring.response :as fx-resp]
            [muuntaja.middleware :as muuntaja-middleware]
            [reitit.ring :as ring]
            [ring.middleware.params :as params-middleware]
            [todo.domain :as domain]
            [todo.schema :as schema]))

;; ---------------------------------------------------------------------------
;; Failure Translation Map
;; ---------------------------------------------------------------------------

(def failure-map
  {:todo/not-found
   (fn [err]
     {:status 404
      :body   {:error   "Not Found"
               :details err}})

   :todo/invalid-input
   (fn [err]
     {:status 400
      :body   {:error   "Bad Request"
               :details err}})

   :jdbc/error
   (fn [err]
     {:status 500
      :body   {:error   "Database Error"
               :details (:message err)}})})

;; ---------------------------------------------------------------------------
;; Request Helpers
;; ---------------------------------------------------------------------------

(defn- extract-payload [req]
  (or (:body-params req) (:body req) {}))

;; ---------------------------------------------------------------------------
;; Route Effect Handlers
;; ---------------------------------------------------------------------------

(defn list-todos-handler>
  "Effect handler for `GET /api/todos`. Supports `?completed=true|false`."
  [req]
  (let [completed-param (get-in req [:params "completed"]
                                (get-in req [:query-params "completed"]))]
    (-> (schema/coerce-filter> completed-param)
        (fx/mapcat> domain/list-todos>)
        (fx-resp/ok>))))

(defn create-todo-handler>
  "Effect handler for `POST /api/todos`."
  [req]
  (let [payload (extract-payload req)]
    (-> (domain/create-todo> payload)
        (fx-resp/created>))))

(defn get-todo-handler>
  "Effect handler for `GET /api/todos/:id`."
  [req]
  (-> (schema/coerce-id> (get-in req [:path-params :id]))
      (fx/mapcat> domain/get-todo-by-id>)
      (fx-resp/ok>)))

(defn update-todo-handler>
  "Effect handler for `PUT /api/todos/:id`."
  [req]
  (let [payload (extract-payload req)]
    (-> (schema/coerce-id> (get-in req [:path-params :id]))
        (fx/mapcat> (fn [id] (domain/update-todo> id payload)))
        (fx-resp/ok>))))

(defn toggle-todo-handler>
  "Effect handler for `PATCH /api/todos/:id/toggle`."
  [req]
  (-> (schema/coerce-id> (get-in req [:path-params :id]))
      (fx/mapcat> domain/toggle-todo>)
      (fx-resp/ok>)))

(defn delete-todo-handler>
  "Effect handler for `DELETE /api/todos/:id`."
  [req]
  (-> (schema/coerce-id> (get-in req [:path-params :id]))
      (fx/mapcat> domain/delete-todo>)
      (fx-resp/ok>)))

;; ---------------------------------------------------------------------------
;; Reitit Routes & App Construction
;; ---------------------------------------------------------------------------

(defn- fx-endpoint
  "Wraps an effect handler into a Ring handler with ambient datasource injection
   and typed failure mapping."
  [handler datasource]
  (fx-ring/wrap-fx handler {:provider    {:fx.jdbc/datasource datasource}
                            :failure-map failure-map}))

(defn create-routes
  "Defines the Reitit route structure for the Todo API."
  [datasource]
  [["/api"
    ["/todos"
     {:get  {:handler (fx-endpoint list-todos-handler> datasource)}
      :post {:handler (fx-endpoint create-todo-handler> datasource)}}]
    ["/todos/:id"
     {:get    {:handler (fx-endpoint get-todo-handler> datasource)}
      :put    {:handler (fx-endpoint update-todo-handler> datasource)}
      :delete {:handler (fx-endpoint delete-todo-handler> datasource)}}]
    ["/todos/:id/toggle"
     {:patch {:handler (fx-endpoint toggle-todo-handler> datasource)}}]]])

(defn create-app
  "Constructs the complete Ring application with routing, query params parsing,
   and Muuntaja JSON formatting middleware."
  [datasource]
  (-> (ring/ring-handler
        (ring/router (create-routes datasource))
        (ring/routes
          (ring/create-resource-handler {:path "/"})
          (ring/create-default-handler
            {:not-found          (constantly {:status 404 :body {:error "Route not found"}})
             :method-not-allowed (constantly {:status 405 :body {:error "Method not allowed"}})})))
      (params-middleware/wrap-params)
      (muuntaja-middleware/wrap-format)))
