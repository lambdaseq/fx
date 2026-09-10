(ns fx.http-client
  "Declarative, purely functional HTTP client effects for fx built on Hato and java.net.http.HttpClient."
  (:require [fx.core :as fx]
            [hato.client :as hc])
  (:import (java.net.http HttpClient
                          HttpTimeoutException
                          HttpConnectTimeoutException)
           (java.net ConnectException
                     UnknownHostException
                     NoRouteToHostException
                     PortUnreachableException
                     SocketTimeoutException)
           (java.util.concurrent TimeoutException)
           (javax.net.ssl SSLException
                          SSLHandshakeException)
           (java.nio.channels ClosedChannelException)))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Context Keys & Default Client
;; ---------------------------------------------------------------------------

(def client-key
  "Context key for binding an ambient `java.net.http.HttpClient`."
  :fx.http-client/client)

(def default-opts-key
  "Context key for binding default request options map."
  :fx.http-client/default-options)

(defonce ^:private default-client
  (delay (hc/build-http-client {})))

;; ---------------------------------------------------------------------------
;; Client Lifecycle & Context Injection
;; ---------------------------------------------------------------------------

(defn client?
  "Returns true if `x` is an instance of `java.net.http.HttpClient`."
  [x]
  (instance? HttpClient x))

(defn- ensure-client [client]
  (cond
    (instance? HttpClient client) client
    (map? client)                 (hc/build-http-client client)
    :else                         @default-client))

(defn- resolve-client [explicit-client upstream-val ctx]
  (or explicit-client
      (when (instance? HttpClient upstream-val) upstream-val)
      (:fx.http-client/client ctx)
      (::client ctx)
      @default-client))

(declare http-failure)

(defn build-client>
  "Creates an effect yielding a configured `java.net.http.HttpClient` instance via Hato."
  [opts]
  (fx/try>
    (fx/map> (fn [_] (hc/build-http-client opts)))
    (fn [^Throwable e]
      (http-failure :http/error
                    (or (.getMessage e) "Failed to build HttpClient")
                    nil nil nil e {:opts opts}))))

(defn with-client>
  "Executes `eff` within an execution context where `:fx.http-client/client` is bound to `client`."
  ([eff client]
   (if (and (fx/effect? client) (not (fx/effect? eff)))
     (fx/provide> {client-key eff} client)
     (fx/provide> {client-key client} eff))))

;; ---------------------------------------------------------------------------
;; Failure Taxonomy & Error Constructors
;; ---------------------------------------------------------------------------

(defn http-failure
  "Constructs a typed `fx.core/Failure` record for HTTP errors."
  ([tag message]
   (http-failure tag message nil nil nil nil nil))
  ([tag message status headers body cause req]
   (fx/make-failure
     tag
     {:status  status
      :headers (or headers {})
      :body    body
      :request req
      :message message
      :cause   cause})))

(defn- exception-causes [^Throwable t]
  (loop [curr t
         acc []]
    (if (nil? curr)
      acc
      (recur (.getCause curr) (conj acc curr)))))

(defn- find-cause-matching [^Throwable t pred]
  (first (filter pred (exception-causes t))))

(defn- timeout-exception? [^Throwable t]
  (or (instance? HttpTimeoutException t)
      (instance? HttpConnectTimeoutException t)
      (instance? TimeoutException t)
      (instance? SocketTimeoutException t)))

(defn- connection-exception? [^Throwable t]
  (or (instance? ConnectException t)
      (instance? UnknownHostException t)
      (instance? NoRouteToHostException t)
      (instance? PortUnreachableException t)
      (instance? SSLException t)
      (instance? SSLHandshakeException t)
      (instance? ClosedChannelException t)))

(defn- handle-request-exception [^Throwable t req]
  (let [ex-data (when (instance? clojure.lang.IExceptionInfo t)
                  (ex-data t))
        status  (:status ex-data)
        headers (or (:headers ex-data) {})
        body    (:body ex-data)
        req-info (or req (:request ex-data) {})]
    (cond
      ;; 4xx Client Error
      (and (integer? status) (<= 400 (long status) 499))
      (http-failure :http/client-error
                    (format "HTTP %d: %s" status (or body "Client Error"))
                    status headers body t req-info)

      ;; 5xx Server Error
      (and (integer? status) (<= 500 (long status) 599))
      (http-failure :http/server-error
                    (format "HTTP %d: %s" status (or body "Server Error"))
                    status headers body t req-info)

      ;; Timeout
      (find-cause-matching t timeout-exception?)
      (let [cause (find-cause-matching t timeout-exception?)]
        (http-failure :http/timeout
                      (or (.getMessage ^Throwable cause) (.getMessage t) "HTTP request timed out")
                      status headers body t req-info))

      ;; Connection Failure
      (find-cause-matching t connection-exception?)
      (let [cause (find-cause-matching t connection-exception?)]
        (http-failure :http/connection-error
                      (or (.getMessage ^Throwable cause) (.getMessage t) "HTTP connection failed")
                      status headers body t req-info))

      ;; Other HTTP / general error
      :else
      (http-failure :http/error
                    (or (.getMessage t) "HTTP request error")
                    status headers body t req-info))))

(defn client-error?
  "Returns true if `x` is an IFailure tagged with `:http/client-error`."
  [x]
  (and (fx/failure? x) (= :http/client-error (fx/tag x))))

(defn server-error?
  "Returns true if `x` is an IFailure tagged with `:http/server-error`."
  [x]
  (and (fx/failure? x) (= :http/server-error (fx/tag x))))

(defn timeout-error?
  "Returns true if `x` is an IFailure tagged with `:http/timeout`."
  [x]
  (and (fx/failure? x) (= :http/timeout (fx/tag x))))

(defn connection-error?
  "Returns true if `x` is an IFailure tagged with `:http/connection-error`."
  [x]
  (and (fx/failure? x) (= :http/connection-error (fx/tag x))))

(defn http-error?
  "Returns true if `x` is an IFailure tagged with any `:http/*` failure tag."
  [x]
  (and (fx/failure? x)
       (contains? #{:http/client-error :http/server-error :http/timeout :http/connection-error :http/error}
                  (fx/tag x))))

;; ---------------------------------------------------------------------------
;; Response Extraction Transforms & Predicates
;; ---------------------------------------------------------------------------

(defn response?
  "Returns true if `x` is an HTTP response map containing an integer `:status`."
  [x]
  (and (map? x)
       (not (fx/effect? x))
       (not (fx/failure? x))
       (integer? (:status x))))

(defn ok?
  "Returns true if `x` is an HTTP response map with status 200."
  [x]
  (and (response? x) (= 200 (:status x))))

(defn success?
  "Returns true if `x` is an HTTP response map with a 2xx status code (200-299)."
  [x]
  (and (response? x) (<= 200 (long (:status x)) 299)))

(defn redirect?
  "Returns true if `x` is an HTTP response map with a 3xx status code (300-399)."
  [x]
  (and (response? x) (<= 300 (long (:status x)) 399)))

(defn body>
  "Transforms upstream HTTP response effect by extracting its `:body`."
  ([]
   (fx/map> (fn [resp] (:body resp))))
  ([eff]
   (fx/map> eff (fn [resp] (:body resp)))))

(defn status>
  "Transforms upstream HTTP response effect by extracting its `:status`."
  ([]
   (fx/map> (fn [resp] (:status resp))))
  ([eff]
   (fx/map> eff (fn [resp] (:status resp)))))

(defn headers>
  "Transforms upstream HTTP response effect by extracting its `:headers`."
  ([]
   (fx/map> (fn [resp] (:headers resp))))
  ([eff]
   (fx/map> eff (fn [resp] (:headers resp)))))

;; ---------------------------------------------------------------------------
;; Request Execution Engine & Verb Constructors
;; ---------------------------------------------------------------------------

(defn- execute-request! [req client default-opts]
  (let [merged-req (merge (or default-opts {})
                          req
                          (when client {:http-client client}))
        req-to-send (if (contains? merged-req :throw-exceptions?)
                      merged-req
                      (assoc merged-req :throw-exceptions? true))
        resp (hc/request req-to-send)]
    (if (and (get req-to-send :throw-exceptions? true)
             (integer? (:status resp))
             (>= (long (:status resp)) 400))
      (if (<= (long (:status resp)) 499)
        (http-failure :http/client-error
                      (format "HTTP %d: %s" (:status resp) (or (:body resp) "Client Error"))
                      (:status resp) (:headers resp) (:body resp) nil req-to-send)
        (http-failure :http/server-error
                      (format "HTTP %d: %s" (:status resp) (or (:body resp) "Server Error"))
                      (:status resp) (:headers resp) (:body resp) nil req-to-send))
      resp)))

(defn request>
  "Creates an HTTP request effect from `req-map`.
   Resolves client from explicit `client` argument, upstream value, or execution context.

   Arities:
     (request>)
     (request> req-map)
     (request> client req-map)
     (request> prev-effect client req-map)"
  ([]
   (request> nil nil nil))
  ([a]
   (cond
     (fx/effect? a) (request> a nil nil)
     (client? a)    (request> nil a nil)
     :else          (request> nil nil a)))
  ([a b]
   (cond
     (fx/effect? a)
     (if (client? b)
       (request> a b nil)
       (request> a nil b))

     (client? a)
     (request> nil a b)

     :else
     (request> nil nil (merge a b))))
  ([prev-effect client req-map]
   (fx/try>
     (fx/map-ctx>
       prev-effect
       (fn [val ctx]
         (let [req (cond
                     (and (map? req-map) (not (fx/effect? req-map))) req-map
                     (and (map? val) (not (fx/effect? val)))         val
                     :else (throw (ex-info "request> requires a request map" {:req-map req-map :val val})))
               c   (resolve-client client val ctx)
               d-opts (:fx.http-client/default-options ctx)]
           (execute-request! req (ensure-client c) d-opts))))
     (fn [^Throwable e]
       (handle-request-exception e (or (when (and (map? req-map) (not (fx/effect? req-map))) req-map) {}))))))

(defn get>
  "Creates an HTTP GET request effect.
   Arities:
     (get> url)
     (get> url opts)
     (get> client url opts)"
  ([url]
   (get> nil url nil))
  ([a b]
   (if (client? a)
     (get> a b nil)
     (get> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :get))))

(defn post>
  "Creates an HTTP POST request effect.
   Arities:
     (post> url)
     (post> url opts)
     (post> client url opts)"
  ([url]
   (post> nil url nil))
  ([a b]
   (if (client? a)
     (post> a b nil)
     (post> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :post))))

(defn put>
  "Creates an HTTP PUT request effect.
   Arities:
     (put> url)
     (put> url opts)
     (put> client url opts)"
  ([url]
   (put> nil url nil))
  ([a b]
   (if (client? a)
     (put> a b nil)
     (put> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :put))))

(defn delete>
  "Creates an HTTP DELETE request effect.
   Arities:
     (delete> url)
     (delete> url opts)
     (delete> client url opts)"
  ([url]
   (delete> nil url nil))
  ([a b]
   (if (client? a)
     (delete> a b nil)
     (delete> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :delete))))

(defn patch>
  "Creates an HTTP PATCH request effect.
   Arities:
     (patch> url)
     (patch> url opts)
     (patch> client url opts)"
  ([url]
   (patch> nil url nil))
  ([a b]
   (if (client? a)
     (patch> a b nil)
     (patch> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :patch))))

(defn head>
  "Creates an HTTP HEAD request effect.
   Arities:
     (head> url)
     (head> url opts)
     (head> client url opts)"
  ([url]
   (head> nil url nil))
  ([a b]
   (if (client? a)
     (head> a b nil)
     (head> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :head))))

(defn options>
  "Creates an HTTP OPTIONS request effect.
   Arities:
     (options> url)
     (options> url opts)
     (options> client url opts)"
  ([url]
   (options> nil url nil))
  ([a b]
   (if (client? a)
     (options> a b nil)
     (options> nil a b)))
  ([client url opts]
   (request> client (assoc (or opts {}) :url url :method :options))))
