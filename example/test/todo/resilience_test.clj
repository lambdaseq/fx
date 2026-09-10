(ns todo.resilience-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.schedule :as sched]
            [todo.resilience :as resilience]))

(deftest test-db-retry-policy-step-behavior
  (testing "db-retry-policy recurs on transient database errors"
    (let [s resilience/db-retry-policy
          st0 (sched/-initial-state s)
          err-input (fx/make-failure :fx.jdbc/error {:message "Database locked"})
          step1 (sched/step-schedule! s st0 0 err-input)
          step2 (sched/step-schedule! s (:state step1) 0 err-input)
          step3 (sched/step-schedule! s (:state step2) 0 err-input)
          step4 (sched/step-schedule! s (:state step3) 0 err-input)]
      (is (= :recur (:decision step1)))
      (is (pos? (:delay-ms step1)))
      (is (= :recur (:decision step2)))
      (is (= :recur (:decision step3)))
      (is (= :halt (:decision step4)))))

  (testing "db-retry-policy halts immediately on non-matching errors"
    (let [s resilience/db-retry-policy
          st0 (sched/-initial-state s)
          unrelated-err (fx/make-failure :todo/not-found {:id 123})
          step (sched/step-schedule! s st0 0 unrelated-err)]
      (is (= :halt (:decision step))))))

(deftest test-retry-schedule-with-effect-execution
  (testing "retries transient DB failure until success"
    (let [attempts (atom 0)
          flaky-op (-> (fx/succeed> nil)
                       (fx/mapcat> (fn [_]
                                     (let [n (swap! attempts inc)]
                                       (if (< n 3)
                                         (fx/fail> :fx.jdbc/error {:message "temporary lock"})
                                         (fx/succeed> {:inserted true :id 1}))))))
          res (fx/run-sync! (sched/retry-schedule> flaky-op resilience/db-retry-policy))]
      (is (= {:inserted true :id 1} res))
      (is (= 3 @attempts))))

  (testing "halts retries on permanent or unexpected failure tag"
    (let [attempts (atom 0)
          invalid-op (-> (fx/succeed> nil)
                         (fx/mapcat> (fn [_]
                                       (swap! attempts inc)
                                       (fx/fail> :todo/invalid-input {:field :title}))))
          res (fx/run-sync! (sched/retry-schedule> invalid-op resilience/db-retry-policy))]
      (is (fx/failure? res))
      (is (= :todo/invalid-input (fx/tag res)))
      (is (= 1 @attempts)))))

(deftest test-rate-limiter-behavior
  (testing "allows requests within burst and rejects with :rate-limiter/exceeded when exhausted"
    (let [limiter (resilience/create-todo-rate-limiter {:limit 2 :interval-ms 10000})
          eff (sched/rate-limiter> (fx/succeed> {:status :ok}) limiter)]
      ;; Call 1: success
      (is (= {:status :ok} (fx/run-sync! eff)))
      ;; Call 2: success
      (is (= {:status :ok} (fx/run-sync! eff)))
      ;; Call 3: rate limited
      (let [res (fx/run-sync! eff)]
        (is (fx/failure? res))
        (is (= :rate-limiter/exceeded (fx/tag res)))))))
