(ns fx.schedule-test
  (:refer-clojure :exclude [identity])
  (:require [clojure.test :refer :all]
            [fx.core :as fx]
            [fx.schedule :as sched]))

;; ---------------------------------------------------------------------------
;; Base Schedule Constructors Tests
;; ---------------------------------------------------------------------------

(deftest schedule-protocol-test
  (testing "schedule? predicate"
    (is (sched/schedule? (sched/forever>)))
    (is (sched/schedule? (sched/recur-n> 3)))
    (is (not (sched/schedule? {:foo :bar})))
    (is (not (sched/schedule? (fx/succeed> 1))))))

(deftest forever>-test
  (let [s (sched/forever>)
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 100 :in)]
    (is (= :recur (:decision step1)))
    (is (= 0 (:delay-ms step1)))
    (is (= 0 (:out step1)))
    (is (= :recur (:decision step2)))
    (is (= 0 (:delay-ms step2)))
    (is (= 1 (:out step2)))))

(deftest recur-n>-test
  (testing "recur-n> 0 halts immediately"
    (let [s (sched/recur-n> 0)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)]
      (is (= :halt (:decision step1)))))

  (testing "recur-n> 2 recurs exactly 2 times then halts"
    (let [s (sched/recur-n> 2)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)
          step3 (sched/step-schedule! s (:state step2) 0 :in)]
      (is (= :recur (:decision step1)))
      (is (= 0 (:out step1)))
      (is (= :recur (:decision step2)))
      (is (= 1 (:out step2)))
      (is (= :halt (:decision step3))))))

(deftest once>-test
  (let [s (sched/once>)
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)]
    (is (= :recur (:decision step1)))
    (is (= :halt (:decision step2)))))

(deftest fixed>-test
  (let [s (sched/fixed> 250)
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)]
    (is (= :recur (:decision step1)))
    (is (= 250 (:delay-ms step1)))
    (is (= :recur (:decision step2)))
    (is (= 250 (:delay-ms step2)))))

(deftest linear-backoff>-test
  (testing "linear backoff scales delays linearly"
    (let [s (sched/linear-backoff> {:initial-ms 100})
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)
          step3 (sched/step-schedule! s (:state step2) 0 :in)]
      (is (= 100 (:delay-ms step1)))
      (is (= 200 (:delay-ms step2)))
      (is (= 300 (:delay-ms step3)))))

  (testing "linear backoff respects max-ms cap"
    (let [s (sched/linear-backoff> {:initial-ms 100 :max-ms 250})
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)
          step3 (sched/step-schedule! s (:state step2) 0 :in)]
      (is (= 100 (:delay-ms step1)))
      (is (= 200 (:delay-ms step2)))
      (is (= 250 (:delay-ms step3))))))

(deftest exponential-backoff>-test
  (testing "exponential backoff scales with exponential factor"
    (let [s (sched/exponential-backoff> {:initial-ms 50 :factor 2.0})
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)
          step3 (sched/step-schedule! s (:state step2) 0 :in)
          step4 (sched/step-schedule! s (:state step3) 0 :in)]
      (is (= 50 (:delay-ms step1)))
      (is (= 100 (:delay-ms step2)))
      (is (= 200 (:delay-ms step3)))
      (is (= 400 (:delay-ms step4)))))

  (testing "exponential backoff respects max-ms"
    (let [s (sched/exponential-backoff> {:initial-ms 50 :factor 2.0 :max-ms 150})
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)
          step3 (sched/step-schedule! s (:state step2) 0 :in)]
      (is (= 50 (:delay-ms step1)))
      (is (= 100 (:delay-ms step2)))
      (is (= 150 (:delay-ms step3))))))

(deftest fibonacci-backoff>-test
  (let [s (sched/fibonacci-backoff> {:initial-ms 10 :max-ms 100})
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)
        step3 (sched/step-schedule! s (:state step2) 0 :in)
        step4 (sched/step-schedule! s (:state step3) 0 :in)
        step5 (sched/step-schedule! s (:state step4) 0 :in)]
    (is (= 10 (:delay-ms step1)))
    (is (= 10 (:delay-ms step2)))
    (is (= 20 (:delay-ms step3)))
    (is (= 30 (:delay-ms step4)))
    (is (= 50 (:delay-ms step5)))))

(deftest elapsed>-test
  (let [s (sched/elapsed> 500)
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 1000 :in)
        step2 (sched/step-schedule! s (:state step1) 1200 :in)
        step3 (sched/step-schedule! s (:state step2) 1600 :in)]
    (is (= :recur (:decision step1)))
    (is (= 0 (:out step1)))
    (is (= :recur (:decision step2)))
    (is (= 200 (:out step2)))
    (is (= :halt (:decision step3)))
    (is (= 600 (:out step3)))))

;; ---------------------------------------------------------------------------
;; Combinators Tests
;; ---------------------------------------------------------------------------

(deftest intersect>-test
  (let [s (sched/intersect> (sched/recur-n> 2) (sched/fixed> 100))
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)
        step3 (sched/step-schedule! s (:state step2) 0 :in)]
    (is (= :recur (:decision step1)))
    (is (= 100 (:delay-ms step1)))
    (is (= [0 0] (:out step1)))
    (is (= :recur (:decision step2)))
    (is (= 100 (:delay-ms step2)))
    (is (= [1 1] (:out step2)))
    (is (= :halt (:decision step3)))))

(deftest union>-test
  (let [s (sched/union> (sched/recur-n> 1) (sched/recur-n> 2))
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)
        step3 (sched/step-schedule! s (:state step2) 0 :in)]
    (is (= :recur (:decision step1)))
    (is (= :recur (:decision step2)))
    (is (= :halt (:decision step3)))))

(deftest and-then>-test
  (let [s (sched/and-then> (sched/recur-n> 2) (sched/recur-n> 1))
        st0 (sched/-initial-state s)
        step1 (sched/step-schedule! s st0 0 :in)
        step2 (sched/step-schedule! s (:state step1) 0 :in)
        step3 (sched/step-schedule! s (:state step2) 0 :in)
        step4 (sched/step-schedule! s (:state step3) 0 :in)]
    (is (= :recur (:decision step1)))
    (is (= :recur (:decision step2)))
    (is (= :recur (:decision step3)))
    (is (= :halt (:decision step4)))))

(deftest jitter>-test
  (let [s (sched/jitter> (sched/fixed> 100) 0.2)
        st0 (sched/-initial-state s)
        step (sched/step-schedule! s st0 0 :in)
        d (:delay-ms step)]
    (is (<= 80 d 120))))

(deftest modify-delay>-test
  (let [s (sched/modify-delay> (sched/fixed> 50) (fn [d] (* d 2)))
        st0 (sched/-initial-state s)
        step (sched/step-schedule! s st0 0 :in)]
    (is (= 100 (:delay-ms step)))))

(deftest while-input>-and-until-input>-test
  (testing "while-input> halts when predicate is false"
    (let [s (sched/while-input> (sched/forever>) pos?)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 5)
          step2 (sched/step-schedule! s (:state step1) 0 -1)]
      (is (= :recur (:decision step1)))
      (is (= :halt (:decision step2)))))

  (testing "until-input> halts when predicate is true"
    (let [s (sched/until-input> (sched/forever>) zero?)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 5)
          step2 (sched/step-schedule! s (:state step1) 0 0)]
      (is (= :recur (:decision step1)))
      (is (= :halt (:decision step2))))))

(deftest while-tag>-and-until-tag>-test
  (testing "while-tag> retries only on specified failure tag"
    (let [s (sched/while-tag> (sched/forever>) :retryable)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 (fx/make-failure :retryable "try again"))
          step2 (sched/step-schedule! s (:state step1) 0 (fx/make-failure :fatal "abort"))]
      (is (= :recur (:decision step1)))
      (is (= :halt (:decision step2)))))

  (testing "until-tag> halts when matching tag is encountered"
    (let [s (sched/until-tag> (sched/forever>) :fatal)
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 (fx/make-failure :retryable "try again"))
          step2 (sched/step-schedule! s (:state step1) 0 (fx/make-failure :fatal "abort"))]
      (is (= :recur (:decision step1)))
      (is (= :halt (:decision step2))))))

(deftest map-output>-and-tap-step>-test
  (testing "map-output> transforms step outputs"
    (let [s (sched/map-output> (sched/forever>) (fn [n] (* n 10)))
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)]
      (is (= 0 (:out step1)))
      (is (= 10 (:out step2)))))

  (testing "tap-step> observes step transitions"
    (let [tapped (atom [])
          s (sched/tap-step> (sched/recur-n> 2) (fn [info] (swap! tapped conj info)))
          st0 (sched/-initial-state s)
          step1 (sched/step-schedule! s st0 0 :in)
          step2 (sched/step-schedule! s (:state step1) 0 :in)]
      (is (= 2 (count @tapped)))
      (is (= :recur (:decision (first @tapped)))))))

;; ---------------------------------------------------------------------------
;; Effect Integration Tests
;; ---------------------------------------------------------------------------

(deftest retry-schedule>-test
  (testing "retries flaky effect until success"
    (let [attempts (atom 0)
          flaky-eff (-> (fx/succeed> nil)
                        (fx/mapcat> (fn [_]
                                      (let [n (swap! attempts inc)]
                                        (if (< n 3)
                                          (fx/fail> :temp-err n)
                                          (fx/succeed> :ok))))))
          res (-> flaky-eff
                  (sched/retry-schedule> (sched/recur-n> 5))
                  (fx/run-sync!))]
      (is (= :ok res))
      (is (= 3 @attempts))))

  (testing "retries until schedule exhaustion and returns final failure"
    (let [attempts (atom 0)
          failing-eff (-> (fx/succeed> nil)
                          (fx/mapcat> (fn [_]
                                        (swap! attempts inc)
                                        (fx/fail> :always-err "boom"))))
          res (-> failing-eff
                  (sched/retry-schedule> (sched/recur-n> 2))
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :always-err (fx/tag res)))
      (is (= 3 @attempts)))) ; initial try + 2 retries = 3 total executions

  (testing "retries with while-tag> filtering"
    (let [attempts (atom 0)
          flaky-eff (-> (fx/succeed> nil)
                        (fx/mapcat> (fn [_]
                                      (let [n (swap! attempts inc)]
                                        (if (= n 1)
                                          (fx/fail> :retryable "retry me")
                                          (fx/fail> :fatal "stop now"))))))
          res (-> flaky-eff
                  (sched/retry-schedule> (-> (sched/forever>) (sched/while-tag> :retryable)))
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :fatal (fx/tag res)))
      (is (= 2 @attempts)))))

(deftest repeat-schedule>-test
  (testing "repeats effect on success according to schedule"
    (let [runs (atom 0)
          eff (-> (fx/succeed> nil)
                  (fx/map> (fn [_]
                             (swap! runs inc)
                             @runs)))
          res (-> eff
                  (sched/repeat-schedule> (sched/recur-n> 4))
                  (fx/run-sync!))]
      (is (= 5 res))
      (is (= 5 @runs))))

  (testing "halts immediately on failure during repeat"
    (let [runs (atom 0)
          eff (-> (fx/succeed> nil)
                  (fx/mapcat> (fn [_]
                                (let [n (swap! runs inc)]
                                  (if (< n 3)
                                    (fx/succeed> n)
                                    (fx/fail> :repeat-fail n))))))
          res (-> eff
                  (sched/repeat-schedule> (sched/recur-n> 10))
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :repeat-fail (fx/tag res)))
      (is (= 3 @runs)))))

;; ---------------------------------------------------------------------------
;; Resilience Combinators Tests
;; ---------------------------------------------------------------------------

(deftest circuit-breaker>-test
  (testing "normal operation when closed"
    (let [breaker (sched/make-circuit-breaker {:failure-threshold 2})
          eff (sched/circuit-breaker> (fx/succeed> 42) breaker)
          res (fx/run-sync! eff)]
      (is (= 42 res))
      (is (= :closed (sched/circuit-breaker-state breaker)))))

  (testing "trips open after exceeding failure threshold"
    (let [state-changes (atom [])
          breaker (sched/make-circuit-breaker {:failure-threshold 2
                                               :reset-timeout-ms 500
                                               :on-state-change (fn [from to]
                                                                  (swap! state-changes conj [from to]))})
          fail-eff (sched/circuit-breaker> (fx/fail> :downstream-err "err") breaker)]
      ;; Failure 1: count=1, still closed
      (is (fx/failure? (fx/run-sync! fail-eff)))
      (is (= :closed (sched/circuit-breaker-state breaker)))

      ;; Failure 2: threshold reached, trips to :open
      (is (fx/failure? (fx/run-sync! fail-eff)))
      (is (= :open (sched/circuit-breaker-state breaker)))
      (is (= [[:closed :open]] @state-changes))

      ;; Subsequent call fast-fails immediately without evaluating effect
      (let [res (fx/run-sync! fail-eff)]
        (is (fx/failure? res))
        (is (= :circuit-breaker/open (fx/tag res))))))

  (testing "transitions open -> half-open -> closed on recovery"
    (let [breaker (sched/make-circuit-breaker {:failure-threshold 1 :reset-timeout-ms 50})
          fail-eff (sched/circuit-breaker> (fx/fail> :err "err") breaker)
          succ-eff (sched/circuit-breaker> (fx/succeed> "recovered") breaker)]
      ;; Trip breaker
      (fx/run-sync! fail-eff)
      (is (= :open (sched/circuit-breaker-state breaker)))

      ;; Wait for reset timeout
      #?(:clj (Thread/sleep 60) :cljs nil)

      ;; Canary call succeeds -> transitions back to closed
      (let [res (fx/run-sync! succ-eff)]
        (is (= "recovered" res))
        (is (= :closed (sched/circuit-breaker-state breaker)))))))

(deftest rate-limiter>-test
  (testing "allows executions within burst capacity"
    (let [limiter (sched/make-rate-limiter {:limit 3 :interval-ms 1000})
          eff (sched/rate-limiter> (fx/succeed> :ok) limiter)]
      (is (= :ok (fx/run-sync! eff)))
      (is (= :ok (fx/run-sync! eff)))
      (is (= :ok (fx/run-sync! eff)))))

  (testing "rejects when token capacity is exceeded"
    (let [limiter (sched/make-rate-limiter {:limit 2 :interval-ms 10000})
          eff (sched/rate-limiter> (fx/succeed> :ok) limiter)]
      (is (= :ok (fx/run-sync! eff)))
      (is (= :ok (fx/run-sync! eff)))
      (let [res (fx/run-sync! eff)]
        (is (fx/failure? res))
        (is (= :rate-limiter/exceeded (fx/tag res)))))))
