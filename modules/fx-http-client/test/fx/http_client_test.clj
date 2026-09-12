(ns fx.http-client-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [fx.core :as fx]
            [fx.http-client :as http]
            [fx.schedule :as sched])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.net.http HttpClient)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent CompletableFuture)))

;; ---------------------------------------------------------------------------
;; Embedded Test Server Fixture
;; ---------------------------------------------------------------------------

(def ^:dynamic *server-port* nil)
(def ^:dynamic *server-base-url* nil)
(def ^:dynamic *request-counts* (atom {}))

(defn- parse-query [query-str]
  (if (str/blank? query-str)
    {}
    (into {}
          (map (fn [param]
                 (let [[k v] (str/split param #"=" 2)]
                   [(keyword k) (or v "")]))
               (str/split query-str #"&")))))

(defn- test-handler [^HttpExchange exchange]
  (try
    (let [method-str (.getRequestMethod exchange)
          _method    (keyword (str/lower-case method-str))
          uri        (.getRequestURI exchange)
          path       (.getPath uri)
          query-str  (.getQuery uri)
          query      (parse-query query-str)
          headers    (into {} (for [[k v] (.getRequestHeaders exchange)]
                                [(str/lower-case k) (first v)]))
          body-bytes (.readAllBytes (.getRequestBody exchange))
          body-str   (String. body-bytes StandardCharsets/UTF_8)]

      (swap! *request-counts* update path (fnil inc 0))

      (cond
        ;; Basic echo endpoints
        (= path "/echo")
        (let [resp-body (str "echo:" body-str)
              b         (.getBytes resp-body StandardCharsets/UTF_8)
              resp-hdrs (.getResponseHeaders exchange)]
          (.set resp-hdrs "content-type" "text/plain")
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/get")
        (let [resp-body (str "got:" (get query :q "none"))
              b         (.getBytes resp-body StandardCharsets/UTF_8)
              resp-hdrs (.getResponseHeaders exchange)]
          (.set resp-hdrs "content-type" "text/plain")
          (.set resp-hdrs "x-custom-header" (get headers "x-test-header" "missing"))
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/post")
        (let [resp-body (str "posted:" body-str)
              b         (.getBytes resp-body StandardCharsets/UTF_8)
              resp-hdrs (.getResponseHeaders exchange)]
          (.set resp-hdrs "content-type" "application/json")
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/put")
        (let [resp-body (str "put:" body-str)
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/patch")
        (let [resp-body (str "patched:" body-str)
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/delete")
        (let [resp-body "deleted"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/head")
        (let [resp-hdrs (.getResponseHeaders exchange)]
          (.set resp-hdrs "x-head-check" "present")
          (.sendResponseHeaders exchange 200 -1))

        (= path "/options")
        (let [resp-hdrs (.getResponseHeaders exchange)]
          (.set resp-hdrs "allow" "GET, POST, OPTIONS, HEAD")
          (.sendResponseHeaders exchange 204 -1))

        ;; Error status endpoints
        (= path "/status/400")
        (let [resp-body "Bad Request Payload"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 400 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/status/404")
        (let [resp-body "Item Not Found"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 404 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/status/500")
        (let [resp-body "Internal Server Error"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 500 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        (= path "/status/503")
        (let [resp-body "Service Unavailable"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 503 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        ;; Flaky endpoint (fails twice with 503, then 200)
        (= path "/flaky")
        (let [cnt (get @*request-counts* "/flaky" 1)]
          (if (<= cnt 2)
            (let [resp-body (str "fail-attempt-" cnt)
                  b         (.getBytes resp-body StandardCharsets/UTF_8)]
              (.sendResponseHeaders exchange 503 (count b))
              (with-open [os (.getResponseBody exchange)]
                (.write os ^bytes b)))
            (let [resp-body "flaky-recovered"
                  b         (.getBytes resp-body StandardCharsets/UTF_8)]
              (.sendResponseHeaders exchange 200 (count b))
              (with-open [os (.getResponseBody exchange)]
                (.write os ^bytes b)))))

        ;; Slow endpoint for timeouts
        (= path "/delay")
        (do
          (Thread/sleep 300)
          (let [resp-body "delayed-response"
                b         (.getBytes resp-body StandardCharsets/UTF_8)]
            (.sendResponseHeaders exchange 200 (count b))
            (with-open [os (.getResponseBody exchange)]
              (.write os ^bytes b))))

        :else
        (let [resp-body "Unknown endpoint"
              b         (.getBytes resp-body StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 404 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))))
    (catch Throwable t
      (.printStackTrace t))
    (finally
      (.close exchange))))

(defn server-fixture [f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (test-handler exchange))))
    (.start server)
    (let [port (.getPort (.getAddress server))]
      (reset! *request-counts* {})
      (binding [*server-port*     port
                *server-base-url* (str "http://127.0.0.1:" port)]
        (try
          (f)
          (finally
            (.stop server 0)))))))

(use-fixtures :each server-fixture)

;; ---------------------------------------------------------------------------
;; Client Lifecycle & Context Tests
;; ---------------------------------------------------------------------------

(deftest test-client-lifecycle
  (testing "build-client> creates configured HttpClient instance"
    (let [client (fx/run-sync! (http/build-client> {:connect-timeout 5000}))]
      (is (http/client? client))
      (is (instance? HttpClient client))
      (is (not (http/client? {:some :map})))))

  (testing "with-client> ambient context resolution"
    (let [custom-client (fx/run-sync! (http/build-client> {:connect-timeout 3000}))
          eff           (http/with-client>
                          (http/get> (str *server-base-url* "/get"))
                          custom-client)
          res           (fx/run-sync! eff)]
      (is (http/ok? res))
      (is (= custom-client (:http-client res)))))

  (testing "explicit client argument overrides context client"
    (let [client-a (fx/run-sync! (http/build-client> {:connect-timeout 2000}))
          client-b (fx/run-sync! (http/build-client> {:connect-timeout 4000}))
          eff      (http/with-client>
                     (http/get> client-b (str *server-base-url* "/get") {})
                     client-a)
          res      (fx/run-sync! eff)]
      (is (http/ok? res))
      (is (= client-b (:http-client res))))))

;; ---------------------------------------------------------------------------
;; HTTP Verbs Tests
;; ---------------------------------------------------------------------------

(deftest test-http-verbs
  (testing "GET request with query params and headers"
    (let [res (fx/run-sync!
               (http/get> (str *server-base-url* "/get")
                          {:query-params {:q "search-term"}
                           :headers      {"X-Test-Header" "MyValue"}}))]
      (is (http/ok? res))
      (is (= "got:search-term" (:body res)))
      (is (= "MyValue" (get-in res [:headers "x-custom-header"])))))

  (testing "POST request with body"
    (let [res (fx/run-sync!
               (http/post> (str *server-base-url* "/post")
                           {:body "{\"hello\": \"world\"}"}))]
      (is (http/ok? res))
      (is (= "posted:{\"hello\": \"world\"}" (:body res)))
      (is (http/success? res))))

  (testing "PUT request"
    (let [res (fx/run-sync!
               (http/put> (str *server-base-url* "/put")
                          {:body "new-content"}))]
      (is (http/ok? res))
      (is (= "put:new-content" (:body res)))))

  (testing "PATCH request"
    (let [res (fx/run-sync!
               (http/patch> (str *server-base-url* "/patch")
                            {:body "patch-diff"}))]
      (is (http/ok? res))
      (is (= "patched:patch-diff" (:body res)))))

  (testing "DELETE request"
    (let [res (fx/run-sync!
               (http/delete> (str *server-base-url* "/delete")))]
      (is (http/ok? res))
      (is (= "deleted" (:body res)))))

  (testing "HEAD request"
    (let [res (fx/run-sync!
               (http/head> (str *server-base-url* "/head")))]
      (is (http/ok? res))
      (is (= "present" (get-in res [:headers "x-head-check"])))))

  (testing "OPTIONS request"
    (let [res (fx/run-sync!
               (http/options> (str *server-base-url* "/options")))]
      (is (http/success? res))
      (is (= 204 (:status res)))
      (is (= "GET, POST, OPTIONS, HEAD" (get-in res [:headers "allow"]))))))

;; ---------------------------------------------------------------------------
;; Request Combinator & Pipeline Threading
;; ---------------------------------------------------------------------------

(deftest test-request-combinator-piping
  (testing "request> with full request map"
    (let [res (fx/run-sync!
               (http/request> {:url    (str *server-base-url* "/echo")
                               :method :post
                               :body   "pipeline-data"}))]
      (is (http/ok? res))
      (is (= "echo:pipeline-data" (:body res)))))

  (testing "request> threaded from upstream effect"
    (let [res (fx/run-sync!
               (-> (fx/succeed> {:url (str *server-base-url* "/get") :method :get :query-params {:q "upstream"}})
                   (http/request>)))]
      (is (http/ok? res))
      (is (= "got:upstream" (:body res))))))

;; ---------------------------------------------------------------------------
;; Response Transforms & Predicates Tests
;; ---------------------------------------------------------------------------

(deftest test-response-transforms-and-predicates
  (testing "body>, status>, and headers> transforms"
    (let [req (http/get> (str *server-base-url* "/get") {:query-params {:q "extracted"}})]
      (is (= "got:extracted" (fx/run-sync! (http/body> req))))
      (is (= 200 (fx/run-sync! (http/status> req))))
      (is (map? (fx/run-sync! (http/headers> req))))))

  (testing "predicate functions"
    (is (http/response? {:status 200 :body ""}))
    (is (http/ok? {:status 200}))
    (is (not (http/ok? {:status 201})))
    (is (http/success? {:status 201}))
    (is (http/success? {:status 204}))
    (is (not (http/success? {:status 400})))
    (is (http/redirect? {:status 301}))
    (is (http/redirect? {:status 302}))
    (is (not (http/redirect? {:status 200})))))

;; ---------------------------------------------------------------------------
;; Categorized Failure Tagging Tests
;; ---------------------------------------------------------------------------

(deftest test-categorized-failures
  (testing "400 Client Error -> :http/client-error failure"
    (let [res (fx/run-sync! (http/get> (str *server-base-url* "/status/400")))]
      (is (fx/failure? res))
      (is (= :http/client-error (fx/tag res)))
      (is (http/client-error? res))
      (is (http/http-error? res))
      (is (= 400 (get-in (fx/error-data res) [:status])))
      (is (= "Bad Request Payload" (get-in (fx/error-data res) [:body])))))

  (testing "404 Not Found -> :http/client-error failure"
    (let [res (fx/run-sync! (http/get> (str *server-base-url* "/status/404")))]
      (is (fx/failure? res))
      (is (= :http/client-error (fx/tag res)))
      (is (http/client-error? res))
      (is (= 404 (get-in (fx/error-data res) [:status])))))

  (testing "500 Server Error -> :http/server-error failure"
    (let [res (fx/run-sync! (http/get> (str *server-base-url* "/status/500")))]
      (is (fx/failure? res))
      (is (= :http/server-error (fx/tag res)))
      (is (http/server-error? res))
      (is (http/http-error? res))
      (is (= 500 (get-in (fx/error-data res) [:status])))
      (is (= "Internal Server Error" (get-in (fx/error-data res) [:body])))))

  (testing "503 Service Unavailable -> :http/server-error failure"
    (let [res (fx/run-sync! (http/get> (str *server-base-url* "/status/503")))]
      (is (fx/failure? res))
      (is (= :http/server-error (fx/tag res)))
      (is (http/server-error? res))))

  (testing "Connection failure to unused port -> :http/connection-error failure"
    (let [res (fx/run-sync! (http/get> "http://127.0.0.1:59999/down"))]
      (is (fx/failure? res))
      (is (= :http/connection-error (fx/tag res)))
      (is (http/connection-error? res))
      (is (http/http-error? res))))

  (testing "Timeout failure -> :http/timeout failure"
    (let [res (fx/run-sync! (http/get> (str *server-base-url* "/delay") {:timeout 50}))]
      (is (fx/failure? res))
      (is (= :http/timeout (fx/tag res)))
      (is (http/timeout-error? res))
      (is (http/http-error? res))))

  (testing "Catching typed failures with fx/catch>"
    (let [eff (-> (http/get> (str *server-base-url* "/status/404"))
                  (fx/catch> {:http/client-error (fx/map> (fn [err] (str "Handled 404: " (:body err))))}))
          res (fx/run-sync! eff)]
      (is (= "Handled 404: Item Not Found" res)))))

;; ---------------------------------------------------------------------------
;; Resilience & fx-schedule Interoperability Tests
;; ---------------------------------------------------------------------------

(deftest test-resilience-schedule-integration
  (testing "retry-schedule> recovers flaky endpoint on 503 :http/server-error"
    (let [retry-policy (-> (sched/recur-n> 4)
                           (sched/while-tag> :http/server-error))
          eff          (-> (http/get> (str *server-base-url* "/flaky"))
                           (sched/retry-schedule> retry-policy)
                           (http/body>))
          res          (fx/run-sync! eff)]
      (is (= "flaky-recovered" res))
      (is (= 3 (get @*request-counts* "/flaky")))))

  (testing "circuit-breaker> trips open on consecutive HTTP server errors"
    (let [breaker   (sched/make-circuit-breaker {:failure-threshold 2 :reset-timeout-ms 500})
          fail-eff  (sched/circuit-breaker> (http/get> (str *server-base-url* "/status/500")) breaker)
          ;; 1st fail
          res1      (fx/run-sync! fail-eff)
          _         (is (http/server-error? res1))
          _         (is (= :closed (sched/circuit-breaker-state breaker)))
          ;; 2nd fail trips breaker
          res2      (fx/run-sync! fail-eff)
          _         (is (http/server-error? res2))
          _         (is (= :open (sched/circuit-breaker-state breaker)))
          ;; 3rd call is short-circuited by open circuit breaker
          res3      (fx/run-sync! fail-eff)]
      (is (fx/failure? res3))
      (is (= :circuit-breaker/open (fx/tag res3))))))

;; ---------------------------------------------------------------------------
;; Async Evaluation Tests
;; ---------------------------------------------------------------------------

(deftest test-async-execution
  (testing "run-async! resolves HTTP request asynchronously"
    (let [^CompletableFuture cf (fx/run-async! (http/get> (str *server-base-url* "/get") {:query-params {:q "async"}}))
          res                   @cf]
      (is (http/ok? res))
      (is (= "got:async" (:body res))))))
