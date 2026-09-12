(ns fx.async.parallel-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]))

(deftest race-test
  (testing "race> returns the first successful effect and cancels slower racers"
    (let [loser-cancelled? (atom false)
          res (fx/run-sync!
               (fxa/race>
                [(fx/acquire-release>
                  (fx/succeed> :slow)
                  (fn [_] (fx/sleep> 2000))
                  (fn [_] (reset! loser-cancelled? true)))
                 (fx/succeed> :fast)]))]
      (is (= :fast res))
      (Thread/sleep 100)
      (is (true? @loser-cancelled?))))

  (testing "race> returns failure if all racers fail"
    (let [res (fx/run-sync!
               (fxa/race>
                [(fx/fail> :err1 {:msg "1"})
                 (fx/fail> :err2 {:msg "2"})]))]
      (is (fx/failure? res)))))

(deftest all-par-test
  (testing "all-par> executes heterogeneous effects concurrently"
    (let [res (fx/run-sync!
               (fxa/all-par>
                [(fx/succeed> 1)
                 (-> (fx/succeed> 2) (fx/map> #(* % 10)))
                 (fx/succeed> 3)]))]
      (is (= [1 20 3] res))))

  (testing "all-par> respects concurrency limit"
    (let [running (atom 0)
          max-concurrent (atom 0)
          tasks (mapv (fn [i]
                        (fx/try> (fn []
                                   (let [cur (swap! running inc)]
                                     (swap! max-concurrent max cur)
                                     (Thread/sleep 20)
                                     (swap! running dec)
                                     i))))
                      (range 10))
          res (fx/run-sync! (fxa/all-par> tasks {:concurrency 3}))]
      (is (= (vec (range 10)) res))
      (is (<= @max-concurrent 3))))

  (testing "all-par> fail-fast aborts on first failure"
    (let [started? (promise)
          sibling-cleaned? (promise)
          res (fx/run-sync!
               (fxa/all-par>
                [(fx/chain> (fx/try> (fn [] #?(:clj (deref started? 1000 nil)
                                               :cljs @started?)
                                       #?(:clj (Thread/sleep 10)
                                          :cljs nil))) (fx/fail> :fatal {:error "boom"}))
                 (fx/acquire-release>
                  (fx/succeed> :resource)
                  (fn [_]
                    (deliver started? true)
                    (fx/sleep> 2000))
                  (fn [_] (deliver sibling-cleaned? true)))]))]
      (is (fx/failure? res))
      (is (= :fatal (:tag res)))
      (is (= true #?(:clj (deref sibling-cleaned? 1000 nil)
                     :cljs @sibling-cleaned?))))))

(deftest map-par-test
  (testing "map-par> maps an effectful function across items"
    (let [res (fx/run-sync!
               (fxa/map-par>
                (fn [x] (fx/succeed> (* x 2)))
                [1 2 3 4 5]
                {:concurrency 2}))]
      (is (= [2 4 6 8 10] res))))

  (testing "map-par> threaded pipeline syntax"
    (let [res (fx/run-sync!
               (-> [10 20 30]
                   (fxa/map-par> (fn [x] (fx/succeed> (inc x))) {:concurrency 2})))]
      (is (= [11 21 31] res)))))

(deftest execution-scopes-test
  (testing "with-virtual-threads> scopes execution engine"
    (let [res (fx/run-sync!
               (fxa/with-virtual-threads>
                 (fx/succeed> :virtual-ok)))]
      (is (= :virtual-ok res))))

  (testing "with-go> scopes execution to go routines"
    (let [res (fx/run-sync!
               (fxa/with-go>
                 (fx/succeed> :go-ok)))]
      (is (= :go-ok res)))))
