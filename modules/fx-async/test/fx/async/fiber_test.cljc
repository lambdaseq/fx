(ns fx.async.fiber-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]))

(deftest fork-join-test
  (testing "fork> spawns background fiber and join> retrieves result"
    (let [res (fx/run-sync!
                (-> (fx/succeed> 42)
                    (fx/map> inc)
                    (fxa/fork>)
                    (fxa/join>)))]
      (is (= 43 res))))

  (testing "fork> and join> standalone invocations"
    (let [res (fx/run-sync!
                (-> (fxa/fork> (fx/succeed> "hello"))
                    (fx/mapcat> (fn [fib]
                                  (fxa/join> fib)))))]
      (is (= "hello" res))))

  (testing "join> propagates failures from child fiber"
    (let [res (fx/run-sync!
                (-> (fx/fail> :not-found {:id 99})
                    (fxa/fork>)
                    (fxa/join>)))]
      (is (fx/failure? res))
      (is (= :not-found (:tag res)))
      (is (= {:id 99} (fx/error-data res))))))

(deftest fiber-interrupt-test
  (testing "interrupting a fiber transitions its status to :interrupted and runs finalizers"
    (let [started? (promise)
          cleaned? (promise)
          fib (fxa/run-fiber!
                (fx/acquire-release>
                  (fx/succeed> :resource)
                  (fn [_]
                    (deliver started? true)
                    (fx/sleep> 2000))
                  (fn [_] (deliver cleaned? true))))]
      (deref started? 1000 nil)
      (is (= :running (fxa/fiber-status fib)))
      (fxa/interrupt-fiber! fib :canceled)
      (let [outcome (fxa/join-fiber! fib 1000 :timeout)]
        (is (fx/failure? outcome))
        (is (= :fiber/interrupted (:tag outcome)))
        (is (= :interrupted (fxa/fiber-status fib)))
        (is (= true (deref cleaned? 1000 nil)))))))

(deftest parent-child-hierarchy-cancellation-test
  (testing "interrupting a parent fiber automatically cascades cancellation to children"
    (let [child-started? (promise)
          child-cleaned? (promise)
          child-fib-ref  (promise)
          parent-fib (fxa/run-fiber!
                       (-> (fxa/fork> (fx/acquire-release>
                                        (fx/succeed> :child-res)
                                        (fn [_]
                                          (deliver child-started? true)
                                          (fx/sleep> 3000))
                                        (fn [_] (deliver child-cleaned? true))))
                           (fx/map> (fn [child-fib]
                                      (deliver child-fib-ref child-fib)
                                      child-fib))
                           (fx/mapcat> (fn [_]
                                         (fx/sleep> 3000)))))]
      (deref child-started? 2000 nil)
      (let [child-fib (deref child-fib-ref 2000 nil)]
        (is (some? child-fib))
        (is (= :running (fxa/fiber-status child-fib)))
        (fxa/interrupt-fiber! parent-fib :parent-killed)
        (let [child-outcome (fxa/join-fiber! child-fib 2000 :timeout)]
          (is (fx/failure? child-outcome))
          (is (= :interrupted (fxa/fiber-status child-fib)))
          (is (= true (deref child-cleaned? 2000 nil))))))))

(deftest fiber-status>-test
  (testing "fiber-status> inspects status of a target fiber"
    (let [res (fx/run-sync!
                (-> (fx/succeed> 1)
                    (fxa/fork>)
                    (fx/mapcat> (fn [fib]
                                  (-> (fxa/join> fib)
                                      (fx/chain> (fxa/fiber-status> fib)))))))]
      (is (= :succeeded res)))))
