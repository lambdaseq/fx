(ns fx.observability.metrics-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.metrics :as metrics]))

(deftest metric-descriptors-test
  (testing "creates valid metric descriptors"
    (let [c (metrics/metric-counter "http.requests" {:route "/api"})
          g (metrics/metric-gauge "db.pool.active" {:pool "main"})
          t (metrics/metric-timer "db.query.time" {:table "users"})]
      (is (= :counter (:type c)))
      (is (= "http.requests" (:name c)))
      (is (= {:route "/api"} (:tags c)))

      (is (= :gauge (:type g)))
      (is (= "db.pool.active" (:name g)))

      (is (= :timer (:type t)))
      (is (= "db.query.time" (:name t))))))

(deftest counter-and-gauge-test
  (testing "increments counters and sets gauges directly and via effects"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "orders.placed")
          gauge (metrics/metric-gauge "active.workers")]
      (metrics/counter-inc! reg cnt 5)
      (metrics/gauge-set! reg gauge 12)

      (-> (fx/succeed> :ok)
          (metrics/counter-inc> cnt 2)
          (metrics/gauge-set> gauge 15)
          (fx/run-sync! {:fx.observability/metrics-registry reg}))

      (let [snap (metrics/metrics-snapshot! reg)]
        (is (= 7 (get-in snap [:counters "orders.placed" :value])))
        (is (= 15 (get-in snap [:gauges "active.workers" :value])))))))

(deftest track-success-count-test
  (testing "increments on success only"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "ops.success")]
      ;; Successful effect
      (-> (metrics/with-metrics-registry> reg
            (metrics/track-success-count> cnt
                                          (fx/succeed> 42)))
          (fx/run-sync!))

      ;; Failed effect
      (-> (metrics/with-metrics-registry> reg
            (metrics/track-success-count> cnt
                                          (fx/fail> :error {:msg "boom"})))
          (fx/run-sync!))

      (let [snap (metrics/metrics-snapshot! reg)]
        (is (= 1 (get-in snap [:counters "ops.success" :value])))))))

(deftest track-failure-count-test
  (testing "increments on failure only"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "ops.failure")]
      ;; Successful effect
      (-> (metrics/with-metrics-registry> reg
            (metrics/track-failure-count> cnt
                                          (fx/succeed> 42)))
          (fx/run-sync!))

      ;; Failed effect
      (-> (metrics/with-metrics-registry> reg
            (metrics/track-failure-count> cnt
                                          (fx/fail> :error {:msg "boom"})))
          (fx/run-sync!))

      ;; Thrown exception effect
      (try
        (-> (metrics/with-metrics-registry> reg
              (metrics/track-failure-count> cnt
                                            (fx/map> (fn [_] (throw (ex-info "Error" {}))))))
            (fx/run-sync!))
        (catch #?(:clj Exception :cljs :default) _ nil))

      (let [snap (metrics/metrics-snapshot! reg)]
        (is (= 2 (get-in snap [:counters "ops.failure" :value])))))))

(deftest track-duration-test
  (testing "records latency observations accurately"
    (let [reg (metrics/make-metrics-registry)
          tmr (metrics/metric-timer "task.duration")]
      (-> (metrics/with-metrics-registry> reg
            (metrics/track-duration> tmr
                                     (-> (fx/succeed> 10)
                                         (fx/sleep> 10))))
          (fx/run-sync!))

      (let [snap (metrics/metrics-snapshot! reg)
            timer-stats (get-in snap [:timers "task.duration"])]
        (is (= 1 (:count timer-stats)))
        (is (pos? (:sum-ms timer-stats)))
        (is (pos? (:min-ms timer-stats)))
        (is (pos? (:max-ms timer-stats)))
        (is (pos? (:avg-ms timer-stats)))))))

(deftest reset-metrics-test
  (testing "resets accumulators to empty"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "c1")]
      (metrics/counter-inc! reg cnt 100)
      (is (= 100 (get-in (metrics/metrics-snapshot! reg) [:counters "c1" :value])))
      (metrics/reset-metrics! reg)
      (is (empty? (:counters (metrics/metrics-snapshot! reg)))))))

(deftest concurrency-test
  (testing "handles high-concurrency atomic updates without race conditions"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "concurrent.counter")
          threads 8
          iterations 1000
          tasks (mapv (fn [_]
                        (future
                          (dotimes [_ iterations]
                            (metrics/counter-inc! reg cnt 1))))
                      (range threads))]
      (doseq [t tasks] @t)
      (let [snap (metrics/metrics-snapshot! reg)]
        (is (= (* threads iterations) (get-in snap [:counters "concurrent.counter" :value])))))))

(deftest pure-metric-calculations-test
  (testing "update-timer-stats pure function"
    (let [s1 (metrics/update-timer-stats nil 10.0)
          s2 (metrics/update-timer-stats s1 20.0)
          s3 (metrics/update-timer-stats s2 5.0)]
      (is (= {:count 1 :sum-ms 10.0 :min-ms 10.0 :max-ms 10.0 :avg-ms 10.0} s1))
      (is (= {:count 2 :sum-ms 30.0 :min-ms 10.0 :max-ms 20.0 :avg-ms 15.0} s2))
      (is (= {:count 3 :sum-ms 35.0 :min-ms 5.0 :max-ms 20.0 :avg-ms (/ 35.0 3)} s3))))
  (testing "format-metrics-snapshot pure function"
    (let [raw-state {:counters {["requests" {:env "prod"}] 42}
                     :gauges   {["threads" {}] 8}
                     :timers   {["latency" {}] {:count 2 :sum-ms 20.0 :min-ms 5.0 :max-ms 15.0}}}
          snap (metrics/format-metrics-snapshot raw-state)]
      (is (= {:value 42 :tags {:env "prod"}} (get-in snap [:counters "requests"])))
      (is (= {:value 8 :tags {}} (get-in snap [:gauges "threads"])))
      (is (= {:tags {} :count 2 :sum-ms 20.0 :min-ms 5.0 :max-ms 15.0 :avg-ms 10.0}
             (get-in snap [:timers "latency"]))))))

(deftest no-ambient-registry-test
  (testing "metric operations without registry in context safely no-op without error"
    (let [cnt (metrics/metric-counter "orders.placed")
          gauge (metrics/metric-gauge "active.workers")
          tmr (metrics/metric-timer "task.duration")]
      (is (= :ok
             (-> (fx/succeed> :ok)
                 (metrics/counter-inc> cnt 2)
                 (metrics/gauge-set> gauge 15)
                 (as-> eff
                       (->> eff
                            (metrics/track-duration> tmr)
                            (metrics/track-success-count> cnt)
                            (metrics/track-failure-count> cnt)))
                 (fx/run-sync!)))))))

(deftest metrics-layer-test
  (testing "provides managed metrics registry layer and resets on release"
    (let [reg (metrics/make-metrics-registry)
          cnt (metrics/metric-counter "layer.counter")]
      (-> (fx-layer/provide-layer>
           (-> (metrics/counter-inc> cnt 5)
               (fx/mapcat> (fn [_] (fx/succeed> (metrics/metrics-snapshot! reg)))))
           (metrics/metrics-layer> reg))
          (fx/run-sync!)
          (as-> snap
                (is (= 5 (get-in snap [:counters "layer.counter" :value])))))
      ;; On layer release, reset-metrics! was executed
      (let [snap (metrics/metrics-snapshot! reg)]
        (is (empty? (:counters snap)))))))
