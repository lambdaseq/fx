(ns fx.observability.telemetry-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.telemetry :as telemetry]))

(deftest tap-telemetry-success-test
  (testing "emits success event with duration and value without altering pipeline output"
    (let [events (atom [])
          sink (fn [e] (swap! events conj e))
          res (-> (telemetry/with-telemetry-sink> sink
                    (telemetry/tap-telemetry> :job/process {:job-id 123}
                      (-> (fx/succeed> {:status :done})
                          (fx/map> #(assoc % :extra true)))))
                  (fx/run-sync!))]
      (is (= {:status :done :extra true} res))
      (is (= 1 (count @events)))
      (let [event (first @events)]
        (is (= :job/process (:event event)))
        (is (= :success (:status event)))
        (is (= {:job-id 123} (:metadata event)))
        (is (= {:status :done :extra true} (:value event)))
        (is (number? (:duration-ms event)))
        (is (some? (:timestamp event)))))))

(deftest tap-telemetry-failure-test
  (testing "emits failure event when inner effect yields failure"
    (let [events (atom [])
          sink (fn [e] (swap! events conj e))
          failure (-> (telemetry/with-telemetry-sink> sink
                        (telemetry/tap-telemetry> :job/process {:job-id 456}
                          (fx/fail> :service/unavailable {:reason "timeout"})))
                      (fx/run-sync!))]
      (is (fx/failure? failure))
      (is (= 1 (count @events)))
      (let [event (first @events)]
        (is (= :job/process (:event event)))
        (is (= :failure (:status event)))
        (is (= {:tag :service/unavailable :error-data {:reason "timeout"}} (:failure event))))))

  (testing "emits failure event when exception is thrown"
    (let [events (atom [])
          sink (fn [e] (swap! events conj e))]
      (try
        (-> (telemetry/with-telemetry-sink> sink
              (telemetry/tap-telemetry> :job/critical
                (fx/map> (fn [_] (throw (ex-info "Hard crash" {}))))))
            (fx/run-sync!))
        (catch #?(:clj Exception :cljs :default) _ nil))
      (is (= 1 (count @events)))
      (let [event (first @events)]
        (is (= :job/critical (:event event)))
        (is (= :failure (:status event)))
        (is (some? (:exception event)))))))

(deftest tap-telemetry-event-test
  (testing "emits one-shot telemetry event"
    (let [events (atom [])
          sink (fn [e] (swap! events conj e))
          res (-> (fx/succeed> {:user "bob"})
                  (telemetry/tap-telemetry-event> :user/login {:ip "127.0.0.1"})
                  (fx/run-sync! {:fx.observability/telemetry-sinks [sink]}))]
      (is (= {:user "bob"} res))
      (is (= 1 (count @events)))
      (let [event (first @events)]
        (is (= :user/login (:event event)))
        (is (= {:ip "127.0.0.1"} (:data event)))
        (is (= {:user "bob"} (:value event)))))))

(deftest telemetry-sink-layer-test
  (testing "telemetry-sink-layer provides sinks via layer context"
    (let [events (atom [])
          sink (fn [e] (swap! events conj e))]
      (-> (fx-layer/provide-layer>
            (telemetry/tap-telemetry-event> :layer/test {:msg "hi"})
            (telemetry/telemetry-sink-layer> sink))
          (fx/run-sync!))
      (is (= 1 (count @events)))
      (is (= :layer/test (:event (first @events)))))))
