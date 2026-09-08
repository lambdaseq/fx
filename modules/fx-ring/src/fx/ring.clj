(ns fx.ring
  (:require [fx.core :as fx])
  (:import (clojure.lang ArityException)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer)))

(def request-key :fx.ring/request)

(defn- resolve-provider-val [provider req]
  (cond
    (fn? provider)  (try
                      (provider req)
                      (catch Throwable t
                        (throw (ex-info "Exception in context provider" {:request req} t))))
    (map? provider) provider
    :else           nil))

(defn build-fx-context
  "Constructs the execution context map for an effect run.
   Merges resolved provider/services from request and opts with {request-key req}."
  [req opts]
  (let [req-provider (or (:fx/context req) (:fx.ring/context req)
                         (:fx/services req) (:fx.ring/services req)
                         (:fx/provider req) (:fx.ring/provider req))
        opts-provider (or (:provider opts) (:context opts) (:services opts))
        req-ctx (resolve-provider-val req-provider req)
        opts-ctx (resolve-provider-val opts-provider req)]
    (merge (or req-ctx {}) (or opts-ctx {}) {request-key req})))

(defn wrap-fx-context
  "Ring middleware that injects an fx context map or provider function into the request map under `:fx/context`.
   Can be used at the root or route level of a Ring application to provide ambient services (e.g. database,
   configuration) to all downstream `wrap-fx` endpoints."
  [handler context-or-provider]
  (fn
    ([req]
     (handler (assoc req :fx/context context-or-provider)))
    ([req respond raise]
     (handler (assoc req :fx/context context-or-provider) respond raise))))

(defn- default-fallback-handler [failure _req]
  (let [err (fx/error-data failure)]
    {:status  500
     :headers {}
     :body    (if (map? err)
                (or (:message err) "Internal Server Error")
                (str (or err "Internal Server Error")))}))

(defn- invoke-failure-handler [h data req]
  (cond
    (fn? h)
    (try
      (h data req)
      (catch ArityException _
        (h data)))

    (map? h)
    h

    :else
    {:status 500 :body "Internal Server Error"}))

(defn resolve-failure-to-response
  "Converts an IFailure into an HTTP Ring response map using a hybrid resolution strategy:
   1. Check :failure-map for explicit tag match.
   2. Check if error-data contains :status key.
   3. Fallback to :default-handler (or default 500 response)."
  ([failure req]
   (resolve-failure-to-response failure req nil))
  ([failure req {:keys [failure-map default-handler]}]
   (if-not (fx/failure? failure)
     failure
     (let [t (fx/tag failure)
           data (fx/error-data failure)]
       (cond
         (and failure-map (contains? failure-map t))
         (invoke-failure-handler (get failure-map t) data req)

         (and (map? data) (contains? data :status) (integer? (:status data)))
         (let [status-code (:status data)
               headers (get data :headers {})
               body (if (contains? data :body)
                      (:body data)
                      (dissoc data :status :headers))]
           {:status  status-code
            :headers headers
            :body    (if (and (map? body) (empty? body)) nil body)})

         (some? default-handler)
         (invoke-failure-handler default-handler failure req)

         :else
         (default-fallback-handler failure req))))))

(defn wrap-fx-failures
  "Converts IFailure instances returned by a Ring handler to Ring HTTP responses.
   Options:
     :failure-map     - Map of tag -> (fn [error-data req]) or (fn [error-data]) or response-map
     :default-handler - Fallback (fn [failure req]) returning a response map"
  ([handler]
   (wrap-fx-failures handler nil))
  ([handler opts]
   (fn
     ([req]
      (let [res (handler req)]
        (resolve-failure-to-response res req opts)))
     ([req respond raise]
      (try
        (handler req
                 (fn [res]
                   (respond (resolve-failure-to-response res req opts)))
                 raise)
        (catch Throwable t
          (raise t)))))))

(def wrap-fx-failure
  "Alias for `wrap-fx-failures`."
  wrap-fx-failures)

(defn wrap-fx-runner
  "Evaluates effect handler functions (fn [req] -> effect) into Ring values (maps or IFailure).
   Supports 1-arity synchronous (fn [req]) and 3-arity asynchronous (fn [req respond raise]).
   If the handler returns a non-effect value (e.g. standard Ring response map), it passes through unchanged."
  ([handler]
   (wrap-fx-runner handler nil))
  ([handler opts]
   (assert (ifn? handler) "wrap-fx-runner expects a handler function (fn [req])")
   (fn
     ([req]
      (let [res (handler req)]
        (if (fx/effect? res)
          (let [ctx (build-fx-context req opts)]
            (fx/run-sync! res ctx))
          res)))
     ([req respond raise]
      (try
        (let [res (handler req)]
          (if (fx/effect? res)
            (let [ctx (build-fx-context req opts)
                  ^CompletableFuture cf (fx/run-async! res ctx)]
              (.whenComplete cf
                (reify BiConsumer
                  (accept [_ val err]
                    (if err
                      (raise err)
                      (respond val))))))
            (respond res)))
        (catch Throwable t
          (raise t)))))))

(defn wrap-fx
  "Converts an effect handler function (fn [req] -> effect) into a standard Ring HTTP handler
   with automatic failure-to-response translation.
   Supports 1-arity synchronous (fn [req]) and 3-arity asynchronous (fn [req respond raise]).
   If the handler returns a non-effect value (e.g. standard Ring response map), it passes through unchanged.

   Options:
     :provider        - Map or (fn [req]) providing external dependencies / services
     :context         - Alias for :provider
     :services        - Alias for :provider
     :failure-map     - Map of failure tags to handler functions (fn [error-data req])
     :default-handler - Fallback failure handler function (fn [failure req])"
  ([handler]
   (wrap-fx handler nil))
  ([handler opts]
   (wrap-fx-failures (wrap-fx-runner handler opts) opts)))

(defn wrap-fx-all
  "Composite Ring middleware combining `wrap-fx-context`, `wrap-fx-failures`, and `wrap-fx-runner`.

   Options:
     :context         - Base context map or (fn [req]) provider
     :provider        - Alias for :context
     :services        - Alias for :context
     :failure-map     - Map of failure tags to handler functions
     :default-handler - Fallback failure handler function"
  ([handler]
   (wrap-fx-all handler nil))
  ([handler opts]
   (let [ctx (or (:context opts) (:provider opts) (:services opts) {})]
     (-> handler
         (wrap-fx-runner opts)
         (wrap-fx-failures opts)
         (wrap-fx-context ctx))))
  ([handler context opts]
   (wrap-fx-all handler (assoc (or opts {}) :context context))))
