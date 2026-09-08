(ns todo.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [todo.main :as main])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)))

(deftest test-start-and-stop-server
  (testing "start-server! initializes DB and serves requests without blocking when join? is false"
    (let [port 3888
          server (main/start-server! {:port port :join? false})]
      (try
        (is (some? server))
        (let [client (HttpClient/newHttpClient)
              req (-> (HttpRequest/newBuilder)
                      (.uri (URI/create (str "http://localhost:" port "/api/todos")))
                      (.GET)
                      (.build))
              resp (.send client req (HttpResponse$BodyHandlers/ofString))]
          (is (= 200 (.statusCode resp))))
        (finally
          (main/stop-server!))))))

(deftest test-app-layer-with-scoped-execution
  (testing "provide-layer> serves requests inside scoped lifecycle and tears down automatically"
    (let [port 3889
          layer (main/app-layer> {:port port})]
      (let [result (fx/run-sync!
                     (fx-layer/provide-layer>
                       (fx/try> (fn []
                                  (let [client (HttpClient/newHttpClient)
                                        req (-> (HttpRequest/newBuilder)
                                                (.uri (URI/create (str "http://localhost:" port "/api/todos")))
                                                (.GET)
                                                (.build))
                                        resp (.send client req (HttpResponse$BodyHandlers/ofString))]
                                    (.statusCode resp)))
                                :http-request-error)
                       layer))]
        (is (= 200 result))))))
