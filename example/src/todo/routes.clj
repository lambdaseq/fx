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
  "Wraps an effect handler into a Ring handler with typed failure mapping.
   Ambient services (like datasource) are injected via `fx` context."
  [handler]
  (fx-ring/wrap-fx handler {:failure-map failure-map}))

(defn create-routes
  "Defines the Reitit route structure for the Todo API."
  ([]
   [["/api"
     ["/todos"
      {:get  {:handler (fx-endpoint list-todos-handler>)}
       :post {:handler (fx-endpoint create-todo-handler>)}}]
     ["/todos/:id"
      {:get    {:handler (fx-endpoint get-todo-handler>)}
       :put    {:handler (fx-endpoint update-todo-handler>)}
       :delete {:handler (fx-endpoint delete-todo-handler>)}}]
     ["/todos/:id/toggle"
      {:patch {:handler (fx-endpoint toggle-todo-handler>)}}]]])
  ([datasource]
   (create-routes)))

(defn create-app
  "Constructs the complete Ring application with routing, query params parsing,
   and Muuntaja JSON formatting middleware.
   Optionally accepts an ambient context map or datasource to inject via `fx-ring/wrap-fx-context`."
  ([]
   (create-app nil))
  ([context-or-datasource]
   (let [ctx (cond
               (nil? context-or-datasource) nil
               (map? context-or-datasource) (if (contains? context-or-datasource :fx.jdbc/datasource)
                                              context-or-datasource
                                              (assoc context-or-datasource :fx.jdbc/datasource context-or-datasource))
               :else {:fx.jdbc/datasource context-or-datasource})]
     (-> (ring/ring-handler
           (ring/router (create-routes))
           (ring/routes
             (ring/create-resource-handler {:path "/"})
             (ring/create-default-handler
               {:not-found          (constantly {:status 404 :body {:error "Route not found"}})
                :method-not-allowed (constantly {:status 405 :body {:error "Method not allowed"}})})))
         (cond-> ctx (fx-ring/wrap-fx-context ctx))
         (params-middleware/wrap-params)
         (muuntaja-middleware/wrap-format)))))
