(ns todo.external-sync-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [fx.core :as fx]
            [fx.http-client :as http]
            [fx.observability.metrics :as metrics]
            [muuntaja.core :as m]
            [todo.db :as db]
            [todo.routes :as routes])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(def ^:private m-instance (m/create))

(def ^:dynamic *server-port* nil)
(def ^:dynamic *server-base-url* nil)
(def ^:dynamic *request-counts* (atom {}))
(def ^:dynamic *received-webhook-payloads* (atom []))

(defn- mock-server-handler [^HttpExchange exchange]
  (try
    (let [method     (str/upper-case (.getRequestMethod exchange))
          uri        (.getRequestURI exchange)
          path       (.getPath uri)
          body-bytes (.readAllBytes (.getRequestBody exchange))
          body-str   (String. body-bytes StandardCharsets/UTF_8)]

      (swap! *request-counts* update path (fnil inc 0))

      (cond
        ;; Mock remote todos list
        (and (= method "GET") (= path "/mock-remote-todos"))
        (let [resp-json "[{\"id\": 1, \"title\": \"Sync Remote Task 1\", \"completed\": false},
                          {\"id\": 2, \"title\": \"Sync Remote Task 2\", \"completed\": true},
                          {\"id\": 3, \"title\": \"Sync Remote Task 3\", \"completed\": false}]"
              b         (.getBytes resp-json StandardCharsets/UTF_8)
              hdrs      (.getResponseHeaders exchange)]
          (.set hdrs "content-type" "application/json")
          (.sendResponseHeaders exchange 200 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        ;; Mock 404 remote endpoint
        (and (= method "GET") (= path "/mock-not-found"))
        (let [resp-json "{\"error\": \"Remote Collection Not Found\"}"
              b         (.getBytes resp-json StandardCharsets/UTF_8)
              hdrs      (.getResponseHeaders exchange)]
          (.set hdrs "content-type" "application/json")
          (.sendResponseHeaders exchange 404 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        ;; Mock 500 remote endpoint
        (and (= method "GET") (= path "/mock-server-error"))
        (let [resp-json "{\"error\": \"Upstream Database Down\"}"
              b         (.getBytes resp-json StandardCharsets/UTF_8)
              hdrs      (.getResponseHeaders exchange)]
          (.set hdrs "content-type" "application/json")
          (.sendResponseHeaders exchange 500 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))

        ;; Webhook receiver
        (and (= method "POST") (= path "/webhook-target"))
        (let [parsed (m/decode m-instance "application/json" body-str)]
          (swap! *received-webhook-payloads* conj parsed)
          (let [resp-json "{\"acknowledged\": true}"
                b         (.getBytes resp-json StandardCharsets/UTF_8)
                hdrs      (.getResponseHeaders exchange)]
            (.set hdrs "content-type" "application/json")
            (.sendResponseHeaders exchange 200 (count b))
            (with-open [os (.getResponseBody exchange)]
              (.write os ^bytes b))))

        ;; Flaky webhook receiver (fails twice with 503, then succeeds)
        (and (= method "POST") (= path "/flaky-webhook"))
        (let [cnt (get @*request-counts* path 1)]
          (if (<= cnt 2)
            (let [resp-json (str "{\"error\": \"Busy, try again. Attempt " cnt "\"}")
                  b         (.getBytes resp-json StandardCharsets/UTF_8)
                  hdrs      (.getResponseHeaders exchange)]
              (.set hdrs "content-type" "application/json")
              (.sendResponseHeaders exchange 503 (count b))
              (with-open [os (.getResponseBody exchange)]
                (.write os ^bytes b)))
            (let [parsed (m/decode m-instance "application/json" body-str)]
              (swap! *received-webhook-payloads* conj parsed)
              (let [resp-json "{\"status\": \"recovered\", \"acknowledged\": true}"
                    b         (.getBytes resp-json StandardCharsets/UTF_8)
                    hdrs      (.getResponseHeaders exchange)]
                (.set hdrs "content-type" "application/json")
                (.sendResponseHeaders exchange 200 (count b))
                (with-open [os (.getResponseBody exchange)]
                  (.write os ^bytes b))))))

        :else
        (let [resp-json "{\"error\": \"Unhandled test route\"}"
              b         (.getBytes resp-json StandardCharsets/UTF_8)]
          (.sendResponseHeaders exchange 404 (count b))
          (with-open [os (.getResponseBody exchange)]
            (.write os ^bytes b)))))
    (finally
      (.close exchange))))

(defn mock-server-fixture [f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/"
      (reify HttpHandler
        (handle [_ exchange]
          (mock-server-handler exchange))))
    (.start server)
    (let [port (.getPort (.getAddress server))]
      (reset! *request-counts* {})
      (reset! *received-webhook-payloads* [])
      (binding [*server-port*     port
                *server-base-url* (str "http://127.0.0.1:" port)]
        (try
          (f)
          (finally
            (.stop server 0)))))))

(use-fixtures :each mock-server-fixture)

;; ---------------------------------------------------------------------------
;; Test App Setup Helpers
;; ---------------------------------------------------------------------------

(defn- test-db-url []
  (str "jdbc:sqlite:file:testdb-ext-" (UUID/randomUUID) "?mode=memory&cache=shared"))

(defn- setup-test-app []
  (let [url (test-db-url)
        ds (db/create-datasource url)
        _ (fx/run-sync! (db/init-db!> ds))
        reg (metrics/make-metrics-registry)
        client (fx/run-sync! (http/build-client> {:connect-timeout 5000}))
        app (routes/create-app {:fx.jdbc/datasource ds
                                :fx.observability/metrics-registry reg
                                :fx.http-client/client client})]
    {:ds  ds
     :reg reg
     :app app}))

(defn- parse-response-body [resp]
  (let [body (:body resp)]
    (cond
      (nil? body)
      nil

      (instance? java.io.InputStream body)
      (m/decode m-instance "application/json" (slurp body))

      (string? body)
      (m/decode m-instance "application/json" body)

      :else
      body)))

(defn- request
  ([app method uri]
   (request app method uri nil))
  ([app method uri body-params]
   (let [req (cond-> {:request-method method
                      :uri            uri
                      :headers        {"accept"       "application/json"
                                       "content-type" "application/json"}}
               body-params (assoc :body-params body-params
                                  :body (m/encode m-instance "application/json" body-params)))
         resp (app req)]
     (update resp :body (fn [_] (parse-response-body resp))))))

;; ---------------------------------------------------------------------------
;; Remote Todo Import Tests
;; ---------------------------------------------------------------------------

(deftest test-import-remote-todos
  (let [{:keys [app]} (setup-test-app)]
    (testing "Happy path: import remote todos from mock server"
      (let [import-url (str *server-base-url* "/mock-remote-todos")
            resp (request app :post "/api/todos/import-remote" {:url import-url})]
        (is (= 200 (:status resp)))
        (is (= 3 (get-in resp [:body :imported-count])))
        (is (= 3 (count (get-in resp [:body :todos]))))
        (let [todos (get-in resp [:body :todos])]
          (is (= "Sync Remote Task 1" (:title (first todos))))
          (is (false? (:completed (first todos))))
          (is (= "Sync Remote Task 2" (:title (second todos))))
          (is (true? (:completed (second todos))))))

      ;; Verify todos now exist in database via GET /api/todos
      (let [list-resp (request app :get "/api/todos")]
        (is (= 200 (:status list-resp)))
        (is (= 3 (count (:body list-resp))))))

    (testing "Import with limit parameter"
      (let [import-url (str *server-base-url* "/mock-remote-todos")
            resp (request app :post "/api/todos/import-remote" {:url import-url :limit 2})]
        (is (= 200 (:status resp)))
        (is (= 2 (get-in resp [:body :imported-count])))
        (is (= 2 (count (get-in resp [:body :todos]))))))

    (testing "Invalid input payload validation (missing/blank url)"
      (let [resp (request app :post "/api/todos/import-remote" {:url ""})]
        (is (= 400 (:status resp)))
        (is (= "Bad Request" (get-in resp [:body :error])))))

    (testing "Upstream 404 client error mapped to HTTP 404"
      (let [resp (request app :post "/api/todos/import-remote" {:url (str *server-base-url* "/mock-not-found")})]
        (is (= 404 (:status resp)))
        (is (= "Upstream Client Error" (get-in resp [:body :error])))))

    (testing "Upstream 500 server error mapped to HTTP 502 Bad Gateway"
      (let [resp (request app :post "/api/todos/import-remote" {:url (str *server-base-url* "/mock-server-error")})]
        (is (= 502 (:status resp)))
        (is (= "Bad Gateway" (get-in resp [:body :error])))))))

;; ---------------------------------------------------------------------------
;; Webhook Notification Tests
;; ---------------------------------------------------------------------------

(deftest test-notify-webhook
  (let [{:keys [app]} (setup-test-app)]
    ;; Create an initial todo record
    (let [create-resp (request app :post "/api/todos" {:title       "Webhook Test Task"
                                                       :description "Task for notification verification"})
          todo-id     (get-in create-resp [:body :id])]
      (is (number? todo-id))

      (testing "Happy path: dispatch webhook notification"
        (let [target-url (str *server-base-url* "/webhook-target")
              resp (request app :post (str "/api/todos/" todo-id "/notify-webhook")
                            {:webhook-url target-url})]
          (is (= 200 (:status resp)))
          (is (true? (get-in resp [:body :notified])))
          (is (= todo-id (get-in resp [:body :todo-id])))
          (is (= 200 (get-in resp [:body :remote-status])))

          ;; Verify payload arrived at mock server
          (is (= 1 (count @*received-webhook-payloads*)))
          (let [received (first @*received-webhook-payloads*)]
            (is (= "Webhook Test Task" (:title received)))
            (is (= todo-id (:id received))))))

      (testing "Resilient dispatch: retries on transient 503 error and recovers"
        (reset! *received-webhook-payloads* [])
        (let [flaky-url (str *server-base-url* "/flaky-webhook")
              resp (request app :post (str "/api/todos/" todo-id "/notify-webhook")
                            {:webhook-url flaky-url})]
          (is (= 200 (:status resp)))
          (is (true? (get-in resp [:body :notified])))
          (is (= 200 (get-in resp [:body :remote-status])))
          ;; Mock server should have received 3 requests total (2 failures + 1 success)
          (is (= 3 (get @*request-counts* "/flaky-webhook")))
          (is (= 1 (count @*received-webhook-payloads*)))))

      (testing "Webhook notification for non-existent todo returns 404"
        (let [resp (request app :post "/api/todos/99999/notify-webhook"
                            {:webhook-url (str *server-base-url* "/webhook-target")})]
          (is (= 404 (:status resp)))
          (is (= "Not Found" (get-in resp [:body :error])))))

      (testing "Invalid input payload validation (missing/blank webhook-url)"
        (let [resp (request app :post (str "/api/todos/" todo-id "/notify-webhook")
                            {:webhook-url ""})]
          (is (= 400 (:status resp)))
          (is (= "Bad Request" (get-in resp [:body :error]))))))))
