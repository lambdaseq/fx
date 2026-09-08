(ns todo.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
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
