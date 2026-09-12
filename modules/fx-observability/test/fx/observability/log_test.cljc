(ns fx.observability.log-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.log :as log]))

(deftest log-constructors-test
  (testing "log-info> emits structured log record and passes value through"
    (let [logs (atom [])
          sink (fn [entry] (swap! logs conj entry))
          res (-> (fx/succeed> {:user "alice"})
                  (log/log-info> "User retrieved" {:action :fetch})
                  (fx/run-sync! {:fx.observability/logger sink}))]
      (is (= {:user "alice"} res))
      (is (= 1 (count @logs)))
      (let [entry (first @logs)]
        (is (= :info (:level entry)))
        (is (= "User retrieved" (:message entry)))
        (is (= {:action :fetch} (:data entry)))
        (is (some? (:timestamp entry))))))

  (testing "log-debug>, log-warn>, log-error> constructors"
    (let [logs (atom [])
          sink (fn [entry] (swap! logs conj entry))
          _ (-> (fx/succeed> 1)
                (log/log-debug> "Debug msg")
                (log/log-warn> "Warn msg" {:warn-code 101})
                (log/log-error> "Error msg" {:error-code 500})
                (fx/run-sync! {:fx.observability/logger sink}))]
      (is (= 3 (count @logs)))
      (is (= [:debug :warn :error] (mapv :level @logs)))
      (is (= "Debug msg" (:message (first @logs))))
      (is (= {:warn-code 101} (:data (second @logs))))
      (is (= {:error-code 500} (:data (nth @logs 2)))))))

(deftest annotate-logs-test
  (testing "annotate-logs> merges annotations into context and attaches them to downstream log entries"
    (let [logs (atom [])
          sink (fn [entry] (swap! logs conj entry))
          res (-> (fx/succeed> 100)
                  (log/annotate-logs> {:request-id "req-1" :env :prod})
                  (fx/map> inc)
                  (log/log-info> "Step 1 complete")
                  (log/annotate-logs> {:user-id 42})
                  (log/log-info> "Step 2 complete")
                  (fx/run-sync! {:fx.observability/logger sink}))]
      (is (= 101 res))
      (is (= 2 (count @logs)))
      (is (= {:request-id "req-1" :env :prod} (:annotations (first @logs))))
      (is (= {:request-id "req-1" :env :prod :user-id 42} (:annotations (second @logs)))))))

(deftest with-logger-test
  (testing "with-logger> scopes custom logger sink"
    (let [global-logs (atom [])
          scoped-logs (atom [])
          global-sink (fn [e] (swap! global-logs conj e))
          scoped-sink (fn [e] (swap! scoped-logs conj e))
          res (-> (fx/succeed> 42)
                  (log/log-info> "Global 1")
                  (fx/mapcat> (fn [v]
                                (log/with-logger> scoped-sink
                                  (-> (fx/succeed> (* v 2))
                                      (log/log-info> "Scoped")))))
                  (log/log-info> "Global 2")
                  (fx/run-sync! {:fx.observability/logger global-sink}))]
      (is (= 84 res))
      (is (= ["Global 1" "Global 2"] (mapv :message @global-logs)))
      (is (= ["Scoped"] (mapv :message @scoped-logs))))))

(deftest with-log-level-test
  (testing "filters logs below minimum configured level"
    (let [logs (atom [])
          sink (fn [e] (swap! logs conj e))
          _ (-> (fx/succeed> 1)
                (log/log-debug> "Should be omitted")
                (log/log-info> "Should be omitted")
                (log/log-warn> "Should be logged")
                (log/log-error> "Should be logged")
                (fx/run-sync! {:fx.observability/logger sink :fx.observability/log-level :warn}))]
      (is (= 2 (count @logs)))
      (is (= [:warn :error] (mapv :level @logs))))))

(deftest sink-exception-resilience-test
  (testing "exception thrown in logger sink does not break pipeline"
    (let [faulty-sink (fn [_] (throw (ex-info "Sink crashed!" {})))
          res (-> (fx/succeed> :ok)
                  (log/log-info> "Will fail inside sink")
                  (fx/map> (fn [v] (str (name v) "-passed")))
                  (fx/run-sync! {:fx.observability/logger faulty-sink}))]
      (is (= "ok-passed" res)))))

(deftest async-logging-test
  (testing "logs correctly in async pipelines"
    (let [logs (atom [])
          sink (fn [e] (swap! logs conj e))
          _fut (-> (fx/succeed> 5)
                   (log/annotate-logs> {:trace "async-1"})
                   (log/log-info> "Async start")
                   (fx/map> (fn [v] (* v 3)))
                   (log/log-info> "Async finish")
                   (fx/run-async! {:fx.observability/logger sink}))
          res #?(:clj @_fut :cljs nil)]
      (is (= 15 res))
      (is (= 2 (count @logs)))
      (is (= {:trace "async-1"} (:annotations (first @logs))))
      (is (= {:trace "async-1"} (:annotations (second @logs)))))))

(deftest logger-layer-test
  (testing "logger layers provide context correctly"
    (let [logs (atom [])
          sink (fn [e] (swap! logs conj e))]
      (-> (fx-layer/provide-layer>
           (-> (log/log-info> "From layer")
               (log/log-debug> "Should be skipped"))
           (log/logger-layer> {:logger sink :level :info :annotations {:env :test}}))
          (fx/run-sync!))
      (is (= 1 (count @logs)))
      (is (= "From layer" (:message (first @logs))))
      (is (= {:env :test} (:annotations (first @logs)))))))
