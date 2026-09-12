(ns fx.ring-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [fx.jdbc.sql :as sql]
            [fx.ring :as fx-ring]
            [fx.ring.response :as fx-resp])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(deftest test-response-combinators
  (testing "response> constructs 200 response"
    (let [res (fx/run-sync! (fx-resp/response> "Hello World"))]
      (is (= {:status 200 :headers {} :body "Hello World"} res)))
    (let [res (fx/run-sync! (-> (fx/succeed> "From pipeline") (fx-resp/response>)))]
      (is (= {:status 200 :headers {} :body "From pipeline"} res))))

  (testing "ok> constructs 200 response"
    (is (= {:status 200 :headers {} :body {:msg "ok"}}
           (fx/run-sync! (fx-resp/ok> {:msg "ok"}))))
    (is (= {:status 200 :headers {} :body {:todos [1 2 3]}}
           (fx/run-sync! (-> (fx/succeed> {:todos [1 2 3]}) (fx-resp/ok>)))))
    (is (= {:status 200 :headers {} :body "chained"}
           (fx/run-sync! (-> (fx/succeed> "initial") (fx-resp/ok> "chained"))))))

  (testing "created> constructs 201 response"
    (is (= {:status 201 :headers {"Location" "/items/1"} :body nil}
           (fx/run-sync! (fx-resp/created> "/items/1"))))
    (is (= {:status 201 :headers {"Location" "/items/1"} :body {:id 1}}
           (fx/run-sync! (fx-resp/created> "/items/1" {:id 1}))))
    (is (= {:status 201 :headers {} :body {:id 1 :title "Todo"}}
           (fx/run-sync! (-> (fx/succeed> {:id 1 :title "Todo"}) (fx-resp/created>)))))
    (is (= {:status 201 :headers {"Location" "/items/1"} :body {:id 1 :title "Todo"}}
           (fx/run-sync! (-> (fx/succeed> {:id 1 :title "Todo"}) (fx-resp/created> "/items/1"))))))

  (testing "bad-request> constructs 400 response"
    (is (= {:status 400 :headers {} :body "Bad input"}
           (fx/run-sync! (fx-resp/bad-request> "Bad input"))))
    (is (= {:status 400 :headers {} :body "Invalid field"}
           (fx/run-sync! (-> (fx/succeed> "Invalid field") (fx-resp/bad-request>))))))

  (testing "not-found> constructs 404 response"
    (is (= {:status 404 :headers {} :body "Missing item"}
           (fx/run-sync! (fx-resp/not-found> "Missing item"))))
    (is (= {:status 404 :headers {} :body "Item not found"}
           (fx/run-sync! (-> (fx/succeed> "Item not found") (fx-resp/not-found>))))))

  (testing "internal-server-error> constructs 500 response"
    (is (= {:status 500 :headers {} :body "Boom"}
           (fx/run-sync! (fx-resp/internal-server-error> "Boom"))))
    (is (= {:status 500 :headers {} :body "Server exploded"}
           (fx/run-sync! (-> (fx/succeed> "Server exploded") (fx-resp/internal-server-error>))))))

  (testing "redirect> constructs 302 or custom redirect"
    (is (= {:status 302 :headers {"Location" "/login"} :body ""}
           (fx/run-sync! (fx-resp/redirect> "/login"))))
    (is (= {:status 301 :headers {"Location" "/new-home"} :body ""}
           (fx/run-sync! (fx-resp/redirect> "/new-home" 301))))
    (is (= {:status 302 :headers {"Location" "/login"} :body ""}
           (fx/run-sync! (-> (fx/succeed> "ignored") (fx-resp/redirect> "/login"))))))

  (testing "status>, header>, and content-type> modifiers"
    (let [res (fx/run-sync!
                (-> (fx-resp/ok> "Payload")
                    (fx-resp/status> 202)
                    (fx-resp/header> "X-Custom" "Value-123")
                    (fx-resp/content-type> "application/json")))]
      (is (= 202 (:status res)))
      (is (= "Value-123" (get-in res [:headers "X-Custom"])))
      (is (= "application/json" (get-in res [:headers "Content-Type"])))
      (is (= "Payload" (:body res))))))

(deftest test-request-combinators
  (testing "request> reads from context"
    (let [sample-req {:uri "/api/users" :request-method :get :params {:id "100"}}
          whole-req (fx/run-sync! (fx-resp/request>) {fx-resp/request-key sample-req})
          uri (fx/run-sync! (fx-resp/request> :uri) {fx-resp/request-key sample-req})
          missing (fx/run-sync! (fx-resp/request> :missing "default-val") {fx-resp/request-key sample-req})]
      (is (= sample-req whole-req))
      (is (= "/api/users" uri))
      (is (= "default-val" missing)))))

(deftest test-wrap-fx-sync
  (testing "evaluates effect handler synchronously"
    (let [handler (fn [req]
                    (fx/succeed> {:status 200 :body (str "URI was " (:uri req))}))
          app (fx-ring/wrap-fx handler)
          req {:uri "/test" :request-method :get}
          res (app req)]
      (is (= {:status 200 :body "URI was /test"} res))))

  (testing "throws assertion error when non-function passed to wrap-fx"
    (is (thrown? AssertionError (fx-ring/wrap-fx (fx/succeed> {:status 200})))))

  (testing "passes through non-effect response maps safely"
    (let [app (fx-ring/wrap-fx (fn [_] {:status 200 :body "raw"}))]
      (is (= {:status 200 :body "raw"} (app {:uri "/test"}))))))

(deftest test-wrap-fx-async
  (testing "evaluates effect handler in 3-arity async context"
    (let [handler (fn [req]
                    (fx/succeed> {:status 200 :body (str "Async " (:uri req))}))
          app (fx-ring/wrap-fx handler)
          req {:uri "/async-endpoint"}
          latch (CountDownLatch. 1)
          result-atom (atom nil)]
      (app req
           (fn [res]
             (reset! result-atom res)
             (.countDown latch))
           (fn [err]
             (reset! result-atom err)
             (.countDown latch)))
      (is (.await latch 2 TimeUnit/SECONDS))
      (is (= {:status 200 :body "Async /async-endpoint"} @result-atom))))

  (testing "propagates failure in async 3-arity handler"
    (let [handler (fn [_req]
                    (fx/fail> :auth/unauthorized {:status 401 :message "Unauthorized"}))
          app (-> handler (fx-ring/wrap-fx) (fx-ring/wrap-fx-failures))
          latch (CountDownLatch. 1)
          result-atom (atom nil)]
      (app {:uri "/protected"}
           (fn [res]
             (reset! result-atom res)
             (.countDown latch))
           (fn [err]
             (reset! result-atom err)
             (.countDown latch)))
      (is (.await latch 2 TimeUnit/SECONDS))
      (is (= 401 (:status @result-atom)))
      (is (= {:message "Unauthorized"} (:body @result-atom))))))

(deftest test-wrap-fx-failures
  (testing "hybrid failure resolution with explicit failure-map tag match"
    (let [handler (fn [_req] (fx/fail> :not-found {:id 42}))
          opts {:failure-map {:not-found (fn [err req]
                                           {:status 404 :body (str "Item " (:id err) " missing at " (:uri req))})}}
          app (-> handler (fx-ring/wrap-fx-runner) (fx-ring/wrap-fx-failures opts))
          res (app {:uri "/items/42"})]
      (is (= {:status 404 :body "Item 42 missing at /items/42"} res))))

  (testing "hybrid failure resolution deriving status code from error-data"
    (let [handler (fn [_req] (fx/fail> :auth/invalid-token {:status 401 :message "Expired token"}))
          app (-> handler (fx-ring/wrap-fx-runner) (fx-ring/wrap-fx-failures))
          res (app {:uri "/protected"})]
      (is (= 401 (:status res)))
      (is (= {:message "Expired token"} (:body res)))))

  (testing "hybrid failure resolution fallback to default 500 handler"
    (let [handler (fn [_req] (fx/fail> :db/timeout {:message "DB query timeout"}))
          app (-> handler (fx-ring/wrap-fx-runner) (fx-ring/wrap-fx-failures))
          res (app {:uri "/db"})]
      (is (= 500 (:status res)))
      (is (= "DB query timeout" (:body res)))))

  (testing "custom default-handler option"
    (let [handler (fn [_req] (fx/fail> :custom/unhandled "Raw error message"))
          opts {:default-handler (fn [failure req]
                                   {:status 500 :body {:error (fx/tag failure) :path (:uri req)}})}
          app (-> handler (fx-ring/wrap-fx-runner) (fx-ring/wrap-fx-failures opts))
          res (app {:uri "/custom"})]
      (is (= {:status 500 :body {:error :custom/unhandled :path "/custom"}} res)))))

(deftest test-wrap-fx-context-and-service-injection
  (testing "injects request and custom services into context via static provider map"
    (let [handler (fn [req]
                    (-> (fx/service> :app-config)
                        (fx/map> (fn [cfg]
                                   {:status 200
                                    :body {:env (:env cfg) :token (get-in req [:headers "authorization"])}}))))
          app (fx-ring/wrap-fx handler {:provider {:app-config {:env "production"}}})
          req {:uri "/env" :headers {"authorization" "Bearer xyz"}}
          res (app req)]
      (is (= {:status 200 :body {:env "production" :token "Bearer xyz"}} res))
      ;; verify req map was not polluted with ::context
      (is (nil? (::context req)))))

  (testing "supports dynamic provider function (fn [req] ...)"
    (let [handler (fn [_req]
                    (-> (fx/service> :user-id)
                        (fx/map> (fn [uid] {:status 200 :body {:user uid}}))))
          app (fx-ring/wrap-fx handler {:provider (fn [req] {:user-id (get-in req [:params :user])})})
          res (app {:uri "/user" :params {:user "u-123"}})]
      (is (= {:status 200 :body {:user "u-123"}} res))))

  (testing "supports :services and :context aliases in opts"
    (let [handler (fn [_req]
                    (-> (fx/service> :secret)
                        (fx/map> (fn [s] {:status 200 :body s}))))
          app1 (fx-ring/wrap-fx handler {:services {:secret "from-services"}})
          app2 (fx-ring/wrap-fx handler {:context {:secret "from-context"}})]
      (is (= {:status 200 :body "from-services"} (app1 {:uri "/"})))
      (is (= {:status 200 :body "from-context"} (app2 {:uri "/"})))))

  (testing "wrap-fx-context middleware injects context map into sync and async handlers"
    (let [handler (fn [_req]
                    (-> (fx/service> :db-pool)
                        (fx/map> (fn [pool] {:status 200 :body (str "Connected to " pool)}))))
          endpoint (fx-ring/wrap-fx handler)
          app (fx-ring/wrap-fx-context endpoint {:db-pool "production-db"})
          sync-res (app {:uri "/data"})]
      (is (= {:status 200 :body "Connected to production-db"} sync-res))

      ;; 3-arity async test
      (let [latch (CountDownLatch. 1)
            res-atom (atom nil)]
        (app {:uri "/data"}
             (fn [res]
               (reset! res-atom res)
               (.countDown latch))
             (fn [err]
               (reset! res-atom err)
               (.countDown latch)))
        (is (.await latch 2 TimeUnit/SECONDS))
        (is (= {:status 200 :body "Connected to production-db"} @res-atom)))))

  (testing "handles provider exceptions in sync and async paths"
    (let [failing-provider (fn [_req] (throw (RuntimeException. "Provider failure")))
          handler (fn [_req] (fx-resp/ok> "ok"))
          app (fx-ring/wrap-fx handler {:provider failing-provider})]
      ;; Sync path throws
      (is (thrown? RuntimeException (app {:uri "/test"})))
      ;; Async path delegates to raise
      (let [latch (CountDownLatch. 1)
            err-atom (atom nil)]
        (app {:uri "/test"}
             (fn [_] (.countDown latch))
             (fn [err]
               (reset! err-atom err)
               (.countDown latch)))
        (is (.await latch 2 TimeUnit/SECONDS))
        (is (instance? RuntimeException @err-atom))))))

(deftest test-composite-wrap-fx
  (testing "composite wrap-fx handles synchronous branching effect pipeline"
    (let [handler (fn [req]
                    (let [uri (:uri req)]
                      (if (= uri "/secret")
                        (fx/fail> :auth/forbidden {:status 403 :message "Forbidden access"})
                        (fx-resp/ok> (str "Welcome to " uri)))))
          app (fx-ring/wrap-fx handler)
          ok-res (app {:uri "/home"})
          forbidden-res (app {:uri "/secret"})]
      (is (= {:status 200 :headers {} :body "Welcome to /home"} ok-res))
      (is (= 403 (:status forbidden-res)))
      (is (= {:message "Forbidden access"} (:body forbidden-res)))))

  (testing "composite wrap-fx handles async 3-arity requests with failure handling"
    (let [handler (fn [req]
                    (let [uri (:uri req)]
                      (if (= uri "/async-error")
                        (fx/fail> :async/failed {:status 422 :error "Invalid input"})
                        (fx-resp/ok> (str "Async response for " uri)))))
          app (fx-ring/wrap-fx handler)
          latch (CountDownLatch. 1)
          res-atom (atom nil)]
      (app {:uri "/hello"}
           (fn [res]
             (reset! res-atom res)
             (.countDown latch))
           (fn [err]
             (reset! res-atom err)
             (.countDown latch)))
      (is (.await latch 2 TimeUnit/SECONDS))
      (is (= {:status 200 :headers {} :body "Async response for /hello"} @res-atom))

      (let [err-latch (CountDownLatch. 1)
            err-res-atom (atom nil)]
        (app {:uri "/async-error"}
             (fn [res]
               (reset! err-res-atom res)
               (.countDown err-latch))
             (fn [err]
               (reset! err-res-atom err)
               (.countDown err-latch)))
        (is (.await err-latch 2 TimeUnit/SECONDS))
        (is (= 422 (:status @err-res-atom)))
        (is (= {:error "Invalid input"} (:body @err-res-atom)))))))

(deftest test-fx-ring-with-fx-jdbc-integration
  (testing "fx-ring handler querying fx-jdbc database with connection management"
    (let [db-spec {:dbtype "h2:mem" :dbname "ring_test_db;DB_CLOSE_DELAY=-1"}
          ds (fx/run-sync! (fx-jdbc/get-datasource> db-spec))]
      ;; Initialize table
      (fx/run-sync!
        (fx-jdbc/with-connection> ds
          (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS items"])
              (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE items (id INT PRIMARY KEY, name VARCHAR(255))"])))
              (fx/mapcat> (fn [_] (sql/insert!> :items {:id 1 :name "Widget"})))
              (fx/mapcat> (fn [_] (sql/insert!> :items {:id 2 :name "Gadget"}))))))

      (let [handler (fn [req]
                      (let [item-id (Integer/parseInt (or (get-in req [:params :id]) "1"))]
                        (fx-jdbc/with-connection>
                          (-> (sql/get-by-id!> :items item-id {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                              (fx/mapcat> (fn [item]
                                            (if item
                                              (fx-resp/ok> item)
                                              (fx/fail> :not-found {:status 404 :message (str "Item " item-id " not found")}))))))))
            app (fx-ring/wrap-fx handler {:provider {::fx-jdbc/datasource ds}})
            item-res (app {:uri "/items" :params {:id "1"}})
            missing-res (app {:uri "/items" :params {:id "99"}})]
        (is (= {:status 200 :headers {} :body {:id 1 :name "Widget"}} item-res))
        (is (= 404 (:status missing-res)))
        (is (= {:message "Item 99 not found"} (:body missing-res)))))))

(deftest test-wrap-fx-all
  (testing "wrap-fx-all composes context injection, failure mapping, and effect execution"
    (let [handler (fn [req]
                    (if (= (:uri req) "/error")
                      (fx/fail> :test/failure {:custom "fail-detail"})
                      (-> (fx/service> :message)
                          (fx/map> (fn [msg] {:status 200 :body {:msg msg :user (get-in req [:params :user])}})))))
          failure-map {:test/failure (fn [err req]
                                       {:status 422 :body {:error "Custom Failure" :details (:custom err) :path (:uri req)}})}
          app (fx-ring/wrap-fx-all handler {:context {:message "Hello from context"}
                                            :failure-map failure-map})
          ok-res (app {:uri "/ok" :params {:user "alice"}})
          err-res (app {:uri "/error"})]
      (is (= {:status 200 :body {:msg "Hello from context" :user "alice"}} ok-res))
      (is (= {:status 422 :body {:error "Custom Failure" :details "fail-detail" :path "/error"}} err-res)))))
