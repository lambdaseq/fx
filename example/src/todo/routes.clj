(ns todo.routes
  (:require [fx.core :as fx]
            [fx.jdbc :as-alias fx.jdbc]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.observability.trace :as trace]
            [fx.ring :as fx-ring]
            [fx.ring.response :as fx-resp]
            [fx.schedule :as sched]
            [muuntaja.middleware :as muuntaja-middleware]
            [reitit.ring :as ring]
            [ring.middleware.params :as params-middleware]
            [todo.domain :as domain]
            [todo.resilience :as resilience]
            [todo.schema :as schema]))

;; ---------------------------------------------------------------------------
;; Failure Translation Map
;; ---------------------------------------------------------------------------

(defn- sanitize-http-error [err]
  (if (map? err)
    (cond-> {:message (or (:message err) "HTTP request failed")}
      (:status err)  (assoc :status (:status err))
      (:headers err) (assoc :headers (:headers err))
      (:body err)    (assoc :body (:body err))
      (get-in err [:request :url]) (assoc :url (get-in err [:request :url]))
      (get-in err [:request :method]) (assoc :method (get-in err [:request :method])))
    (str err)))

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

   :rate-limiter/exceeded
   (fn [err]
     {:status 429
      :body   {:error   "Too Many Requests"
               :details (or err "Rate limit quota exceeded")}})

   :circuit-breaker/open
   (fn [err]
     {:status 503
      :body   {:error   "Service Unavailable"
               :details (or err "Circuit breaker is open")}})

   :fx.jdbc/error
   (fn [err]
     {:status 500
      :body   {:error   "Database Error"
               :details (:message err)}})

   :jdbc/error
   (fn [err]
     {:status 500
      :body   {:error   "Database Error"
               :details (:message err)}})

   :http/client-error
   (fn [err]
     {:status (or (:status err) 400)
      :body   {:error   "Upstream Client Error"
               :details (sanitize-http-error err)}})

   :http/server-error
   (fn [err]
     {:status 502
      :body   {:error   "Bad Gateway"
               :details (sanitize-http-error err)}})

   :http/timeout
   (fn [err]
     {:status 504
      :body   {:error   "Gateway Timeout"
               :details (sanitize-http-error err)}})

   :http/connection-error
   (fn [err]
     {:status 503
      :body   {:error   "Service Unavailable"
               :details (sanitize-http-error err)}})

   :http/error
   (fn [err]
     {:status 502
      :body   {:error   "HTTP Error"
               :details (sanitize-http-error err)}})})

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

(defn import-remote-todos-handler>
  "Effect handler for `POST /api/todos/import-remote`."
  [req]
  (let [payload (extract-payload req)]
    (-> (domain/import-remote-todos> payload)
        (fx-resp/ok>))))

(defn batch-import-todos-handler>
  "Effect handler for `POST /api/todos/batch-import` using bounded parallel execution."
  [req]
  (let [payload (extract-payload req)]
    (-> (domain/batch-import-todos> payload)
        (fx-resp/ok>))))

(defn notify-webhook-handler>
  "Effect handler for `POST /api/todos/:id/notify-webhook`."
  [req]
  (let [payload (extract-payload req)]
    (-> (schema/coerce-id> (get-in req [:path-params :id]))
        (fx/mapcat> (fn [id] (domain/notify-webhook> id payload)))
        (fx-resp/ok>))))

(defn metrics-handler>
  "Effect handler for `GET /api/metrics`. Returns snapshot of registered in-memory metrics."
  [_req]
  (-> (fx/context>)
      (fx/map> (fn [ctx]
                 (metrics/metrics-snapshot! (:fx.observability/metrics-registry ctx))))
      (fx-resp/ok>)))

;; ---------------------------------------------------------------------------
;; Observability & Tracing Middleware
;; ---------------------------------------------------------------------------

(defn wrap-trace-context
  "Wraps an effect handler to extract incoming W3C trace context from the request,
   bind `:fx/trace-context` to the effect execution context via `with-trace-context>`,
   and attach the outgoing `traceparent` header to the response map."
  [handler]
  (fn [req]
    (let [res (handler req)]
      (let [incoming-trace-ctx (trace/extract-trace-context req)
            trace-id (or (:trace-id incoming-trace-ctx) (trace/random-trace-id))
            trace-ctx (cond-> {:trace-id trace-id}
                              (:parent-span-id incoming-trace-ctx) (assoc :parent-span-id (:parent-span-id incoming-trace-ctx))
                              (some? (:sampled? incoming-trace-ctx)) (assoc :sampled? (:sampled? incoming-trace-ctx)))
            tp (trace/format-traceparent trace-id (trace/random-span-id))]
        (-> (trace/with-trace-context> trace-ctx res)
            (fx/map> (fn [resp]
                       (if (map? resp)
                         (update resp :headers (fn [h] (assoc (or h {}) "traceparent" tp)))
                         resp))))))))

(defn wrap-log-annotations
  "Wraps an effect handler with request metadata log annotations (:request-id, :method, :uri)."
  [handler]
  (fn [req]
    (let [res (handler req)]
      (if-not (fx/effect? res)
        res
        (let [method (-> (:request-method req) name clojure.string/upper-case)
              uri (:uri req)
              incoming-trace-ctx (trace/extract-trace-context req)
              request-id (or (:parent-span-id incoming-trace-ctx) (trace/random-span-id))
              annotations {:request-id request-id
                           :method     method
                           :uri        uri}]
          (-> (log/annotate-logs> annotations)
              (fx/mapcat> (fn [_] res))))))))

(defn wrap-http-span
  "Wraps an effect handler in an `http.request` execution span."
  [handler]
  (fn [req]
    (let [res (handler req)]
      (if-not (fx/effect? res)
        res
        (let [method (-> (:request-method req) name clojure.string/upper-case)
              uri (:uri req)]
          (trace/with-span> "http.request" {:method method :uri uri}
                            res))))))

(defn wrap-http-metrics
  "Wraps an effect handler with HTTP server request duration, total, and failure metrics."
  [handler]
  (fn [req]
    (let [res (handler req)]
      (if-not (fx/effect? res)
        res
        (let [method (-> (:request-method req) name clojure.string/upper-case)]
          (->> res
               (metrics/track-duration> (metrics/metric-timer "http.server.requests.duration"))
               (metrics/track-success-count> (metrics/metric-counter "http.server.requests.total" {:method method}))
               (metrics/track-failure-count> (metrics/metric-counter "http.server.requests.failed" {:method method}))))))))

(defn wrap-rate-limit
  "Wraps an effect handler with rate limiting using `fx.schedule/rate-limiter>`."
  [handler limiter]
  (fn [req]
    (let [res (handler req)]
      (if-not (fx/effect? res)
        res
        (sched/rate-limiter> res limiter)))))

(defn wrap-observability
  "Composite observability middleware combining trace context, log annotations, execution span, and metrics."
  [handler]
  (-> handler
      (wrap-http-metrics)
      (wrap-http-span)
      (wrap-log-annotations)
      (wrap-trace-context)))

;; ---------------------------------------------------------------------------
;; Reitit Routes & App Construction
;; ---------------------------------------------------------------------------

(defn create-routes
  "Defines the Reitit route structure for the Todo API.
   Optionally accepts a rate limiter instance for write endpoints."
  ([]
   (create-routes (resilience/create-todo-rate-limiter)))
  ([create-limiter]
   [["/api"
     ["/metrics"
      {:get {:handler metrics-handler>}}]
     ["/todos"
      {:get  {:handler list-todos-handler>}
       :post {:handler (if create-limiter
                         (wrap-rate-limit create-todo-handler> create-limiter)
                         create-todo-handler>)}}]
     ["/todos/import-remote"
      {:post {:handler (if create-limiter
                         (wrap-rate-limit import-remote-todos-handler> create-limiter)
                         import-remote-todos-handler>)}}]
     ["/todos/batch-import"
      {:post {:handler (if create-limiter
                         (wrap-rate-limit batch-import-todos-handler> create-limiter)
                         batch-import-todos-handler>)}}]
     ["/todos/:id"
      {:get    {:handler get-todo-handler>}
       :put    {:handler update-todo-handler>}
       :delete {:handler delete-todo-handler>}}]
     ["/todos/:id/toggle"
      {:patch {:handler toggle-todo-handler>}}]
     ["/todos/:id/notify-webhook"
      {:post {:handler notify-webhook-handler>}}]]]))

(defn create-app
  "Constructs the complete Ring application with routing, query params parsing,
   observability, and Muuntaja JSON formatting middleware.
   Optionally accepts an ambient context map or options to inject via `fx-ring/wrap-fx-context`."
  ([]
   (create-app {}))
  ([ctx-or-opts]
   (let [ctx (if (map? ctx-or-opts) ctx-or-opts {})
         limiter (get ctx :todo/rate-limiter (resilience/create-todo-rate-limiter))]
     (-> (ring/ring-handler
           (ring/router (create-routes limiter)
                        {:conflicts nil})
           (ring/routes
             (ring/create-resource-handler {:path "/"})
             (ring/create-default-handler
               {:not-found          (constantly {:status 404 :body {:error "Route not found"}})
                :method-not-allowed (constantly {:status 405 :body {:error "Method not allowed"}})})))
         (wrap-observability)
         (fx-ring/wrap-fx-runner)
         (fx-ring/wrap-fx-failures {:failure-map failure-map})
         (fx-ring/wrap-fx-context ctx)
         (params-middleware/wrap-params)
         (muuntaja-middleware/wrap-format)))))
