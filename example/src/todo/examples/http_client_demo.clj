(ns todo.examples.http-client-demo
  "Standalone demonstration of `fx-http-client` showcasing:
   1. Basic GET request with JSON decoding and body extraction (`body>`).
   2. Resilient POST request with exponential backoff retries on transient errors.
   3. Ambient client context binding via `with-client>`.
   4. Integration with observability (metrics and tracing)."
  (:require [clojure.string :as str]
            [fx.core :as fx]
            [fx.http-client :as http]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.observability.trace :as trace]
            [fx.schedule :as sched])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets))
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Embedded Mock Server for Standalone Execution
;; ---------------------------------------------------------------------------

(defn- create-demo-server
  "Creates a local mock HTTP server simulating remote API endpoints."
  [flaky-counter]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/"
      (reify HttpHandler
        (handle [_ exchange]
          (try
            (let [path (.getPath (.getRequestURI exchange))
                  method (str/upper-case (.getRequestMethod exchange))]
              (cond
                ;; GET /mock-todos
                (and (= method "GET") (= path "/mock-todos"))
                (let [body "[{\"id\": 101, \"title\": \"Buy groceries\", \"completed\": false},
                             {\"id\": 102, \"title\": \"Walk the dog\", \"completed\": true}]"
                      b    (.getBytes body StandardCharsets/UTF_8)
                      hdrs (.getResponseHeaders exchange)]
                  (.set hdrs "content-type" "application/json")
                  (.sendResponseHeaders exchange 200 (count b))
                  (with-open [os (.getResponseBody exchange)]
                    (.write os ^bytes b)))

                ;; POST /webhook
                (and (= method "POST") (= path "/webhook"))
                (let [req-body (String. (.readAllBytes (.getRequestBody exchange)) StandardCharsets/UTF_8)
                      resp     (str "{\"received\": true, \"echo\": " req-body "}")
                      b        (.getBytes resp StandardCharsets/UTF_8)
                      hdrs     (.getResponseHeaders exchange)]
                  (.set hdrs "content-type" "application/json")
                  (.sendResponseHeaders exchange 200 (count b))
                  (with-open [os (.getResponseBody exchange)]
                    (.write os ^bytes b)))

                ;; POST /flaky-webhook (fails twice with 503, then succeeds)
                (and (= method "POST") (= path "/flaky-webhook"))
                (let [cnt (swap! flaky-counter inc)]
                  (if (<= cnt 2)
                    (let [resp (str "{\"error\": \"Service overloaded, attempt " cnt "\"}")
                          b    (.getBytes resp StandardCharsets/UTF_8)
                          hdrs (.getResponseHeaders exchange)]
                      (.set hdrs "content-type" "application/json")
                      (.sendResponseHeaders exchange 503 (count b))
                      (with-open [os (.getResponseBody exchange)]
                        (.write os ^bytes b)))
                    (let [resp "{\"status\": \"recovered\", \"attempt\": 3}"
                          b    (.getBytes resp StandardCharsets/UTF_8)
                          hdrs (.getResponseHeaders exchange)]
                      (.set hdrs "content-type" "application/json")
                      (.sendResponseHeaders exchange 200 (count b))
                      (with-open [os (.getResponseBody exchange)]
                        (.write os ^bytes b)))))

                :else
                (let [resp "{\"error\": \"Not found\"}"
                      b    (.getBytes resp StandardCharsets/UTF_8)]
                  (.sendResponseHeaders exchange 404 (count b))
                  (with-open [os (.getResponseBody exchange)]
                    (.write os ^bytes b)))))
            (finally
              (.close exchange))))))
    server))

;; ---------------------------------------------------------------------------
;; Demonstration Workflows
;; ---------------------------------------------------------------------------

(defn demo-basic-get>
  "Demonstrates a standard HTTP GET request with JSON decoding and body extraction."
  [base-url]
  (-> (fx/succeed> "\n--- 1. Basic GET with JSON decoding & body extraction ---")
      (fx/tap> println)
      (fx/mapcat> (fn [_] (http/get> (str base-url "/mock-todos") {:as :json :timeout 3000})))
      (http/body>)
      (fx/tap> (fn [todos]
                 (println "Fetched" (count todos) "todos from remote endpoint:")
                 (doseq [todo todos]
                   (println "  • [" (if (:completed todo) "X" " ") "]" (:title todo)))))))

(defn demo-resilient-post>
  "Demonstrates an HTTP POST with exponential backoff retries across transient server errors."
  [base-url]
  (let [retry-policy (-> (sched/exponential-backoff> {:initial-ms 100 :factor 2.0 :max-ms 1000})
                         (sched/jitter> 0.1)
                         (sched/intersect> (sched/recur-n> 3))
                         (sched/while-tag> #{:http/server-error :http/timeout :http/connection-error}))]
    (-> (fx/succeed> "\n--- 2. Resilient POST with retry on transient failures ---")
        (fx/tap> println)
        (fx/mapcat> (fn [_]
                      (-> (http/post> (str base-url "/flaky-webhook")
                                      {:body    {:event "todo.completed" :todo-id 42}
                                       :as      :json
                                       :timeout 3000})
                          (sched/retry-schedule> retry-policy))))
        (fx/tap> (fn [resp]
                   (println "Resilient request succeeded after retries!")
                   (println "Response status:" (:status resp))
                   (println "Response body:  " (:body resp)))))))

(defn demo-ambient-client-context>
  "Demonstrates ambient HTTP client injection via `with-client>`."
  [base-url]
  (-> (fx/succeed> "\n--- 3. Ambient Client Context Injection via with-client> ---")
      (fx/tap> println)
      (fx/mapcat> (fn [_]
                    (-> (http/build-client> {:connect-timeout 4000 :version :http-2})
                        (fx/mapcat> (fn [custom-client]
                                      (println "Configured custom HttpClient:" (class custom-client))
                                      (-> (http/post> (str base-url "/webhook")
                                                      {:body    {:msg "Custom client message"}
                                                       :as      :json
                                                       :timeout 3000})
                                          (fx/tap> (fn [resp]
                                                     (println "POST completed via ambient client! Status:" (:status resp))))
                                          (http/with-client> custom-client)))))))))

(defn demo-metrics-and-tracing>
  "Demonstrates request metrics tracking and distributed trace propagation."
  [base-url metrics-reg]
  (let [trace-ctx {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                   :parent-span-id "00f067aa0ba902b7"
                   :sampled? true}]
    (-> (fx/succeed> "\n--- 4. Observability Integration (Metrics & Tracing) ---")
        (fx/tap> println)
        (fx/mapcat> (fn [_]
                      (-> (trace/with-span> "demo.outbound-call" {:endpoint "/mock-todos"}
                            (metrics/track-duration> (metrics/metric-timer "demo.http.duration")
                              (metrics/track-success-count> (metrics/metric-counter "demo.http.success")
                                (http/get> (str base-url "/mock-todos") {:as :json}))))
                          (trace/with-trace-context> trace-ctx)
                          (fx/map-ctx> (fn [res _ctx]
                                         (println "Observability pipeline finished. Metrics snapshot:")
                                         (doseq [[k v] (metrics/metrics-snapshot! metrics-reg)]
                                           (println "  Metric:" k "=>" v))
                                         res))))))))

;; ---------------------------------------------------------------------------
;; CLI Entrypoint
;; ---------------------------------------------------------------------------

(defn -main
  "Runs all fx-http-client demonstration scenarios."
  [& _args]
  (println "=======================================================")
  (println "   fx-http-client Comprehensive Demonstration")
  (println "=======================================================")
  (let [flaky-counter (atom 0)
        server (create-demo-server flaky-counter)
        _ (.start server)
        port (.getPort (.getAddress server))
        base-url (str "http://127.0.0.1:" port)
        metrics-reg (metrics/make-metrics-registry)]
    (try
      (let [combined-demo>
            (-> (fx/context>)
                (fx/mapcat> (fn [_]
                              (-> (demo-basic-get> base-url)
                                  (fx/chain> (demo-resilient-post> base-url))
                                  (fx/chain> (demo-ambient-client-context> base-url))
                                  (fx/chain> (demo-metrics-and-tracing> base-url metrics-reg))))))]
        (fx/run-sync! combined-demo> {:fx.observability/metrics-registry metrics-reg}))
      (println "\n=======================================================")
      (println "   All demonstrations completed successfully!")
      (println "=======================================================")
      (finally
        (.stop server 0)))))
