(ns com.lambdaseq.fx.core-test
  (:require [clojure.test :refer :all]
            [com.lambdaseq.fx.core :refer :all]))

(deftest make-effect-test
  (let [run (constantly 1)
        eff (make-effect :test nil run)]
    (testing "Return value of make-effect is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :test (:effect-type eff))))
    (testing "Effect run function is correct"
      (is (= 1 (run-sync! eff))))))

(deftest make-failure-test
  (let [err-data {:a 1}
        fail (make-failure :test err-data)]
    (testing "Return value of make-failure is a valid failure"
      (is (failure? fail)))
    (testing "Failure type is correct"
      (is (= :test (:type fail))))
    (testing "Failure data is correct"
      (is (= err-data (:data fail))))))

(deftest maybe-propagate-failure-test
  (let [eff (make-effect :test nil (constantly 1))
        fail (make-failure :test {:a 1})
        res (maybe-propagate-failure eff 1)
        res-fail (maybe-propagate-failure fail 1)]
    (testing "Returns value if not a failure"
      (is (= 1 res)))
    (testing "Returns failure if a failure"
      (is (= fail res-fail)))))

(deftest maybe-propagate-effect-test
  (let [eff (make-effect :test nil (constantly 1))
        fail (make-failure :test {:a 1})
        res (maybe-propagate-effect eff 2)
        res-fail (maybe-propagate-effect fail 2)]
    (testing "Returns value if not an effect"
      (is (= eff res)))
    (testing "Returns effect if an effect"
      (is (= 2 res-fail)))))

(deftest succeed>-test
  (let [eff (succeed> 1)]
    (testing "Return value of succeed> is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :succeed (:effect-type eff))))
    (testing "Effect run function is correct"
      (is (= 1 (run-sync! eff))))))

(deftest fail>-test
  (let [err-data {:a 1}
        failure (make-failure :test err-data)
        eff (fail> :test err-data)]
    (testing "Return value of fail> is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :fail (:effect-type eff))))
    (testing "Effect run function returns a failure"
      (let [res (run-sync! eff)]
        (is (failure? res))
        (is (= :test (:type (run-sync! eff))))
        (is (= err-data (:data (run-sync! eff))))))))

(deftest map>-test
  (testing "map> propagates failure"
    (let [res (->> (fail> :test {})
                   (map> (constantly 1))
                   (run-sync!))]
      (is (failure? res))))
  (testing "map> run function should not evaluate on failure"
    (let [res (->> (fail> :test {})
                   (map> (fn [_]
                           (print "Should not print")
                           1))
                   (run-sync!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "map> applies function to successful effect's value"
    (let [res (->> (succeed> 1)
                   (map> inc)
                   (run-sync!))]
      (is (= 2 res))))
  (testing "map> run function should evaluate on success"
    (let [res (->> (succeed> 1)
                   (map> (fn [x]
                           (print "Should print")
                           (inc x)))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest mapcat>-test
  (testing "mapcat> propagates failure"
    (let [res (->> (fail> :test {})
                   (mapcat> (constantly (succeed> 1)))
                   (run-sync!))]
      (is (failure? res))))
  (testing "mapcat> run function should not evaluate on failure"
    (let [res (->> (fail> :test {})
                   (mapcat> (fn [_]
                              (print "Should not print")
                              (succeed> 1)))
                   (run-sync!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "mapcat> applies function to successful effect's value"
    (let [res (->> (succeed> 1)
                   (mapcat> (fn [x]
                              (succeed> (inc x))))
                   (run-sync!))]
      (is (= 2 res))))
  (testing "mapcat> run function should evaluate on success"
    (let [res (->> (succeed> 1)
                   (mapcat> (fn [x]
                              (print "Should print")
                              (succeed> (inc x))))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest if>-test
  (testing "if> propagates failure"
    (let [res (->> (fail> :test {})
                   (if> true
                        (constantly (succeed> 1))
                        (constantly (succeed> 2)))
                   (run-sync!))]
      (is (failure? res))))

  (testing "if> runs the then branch if condition is true"
    (let [eff (->> (succeed> true)
                   (if> true?
                        (constantly (succeed> 1))
                        (constantly (succeed> 2))))
          res (run-sync! eff)]
      (is (= 1 res))))

  (testing "side effects in the `else` branch are not evaluated if condition is true"
    (let [res (->> (succeed> false)
                   (if> true?
                        (fn [_]
                          (print "Should not print")
                          (succeed> 1))
                        (fn [_]
                          (print "Should print")
                          (succeed> 2)))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res))))

  (testing "if> runs the else branch if condition is true"
    (let [eff (->> (succeed> false)
                   (if> true?
                        (fn [_]
                          (print "Should not print")
                          (succeed> 1))
                        (fn [_]
                          (print "Should print")
                          (succeed> 2))))
          res (run-sync! eff)]
      (is (= 2 res))))
  (testing "side effects in the `then` branch are not evaluated if condition is false"
    (let [res (->> (succeed> false)
                   (if> true?
                        (fn [_]
                          (print "Should not print")
                          (succeed> 1))
                        (fn [_]
                          (print "Should print")
                          (succeed> 2)))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest pipeline>>-test
  (testing "Can create a pipeline of effects"
    (let [inc-twice>
          (pipeline>>
            (map> inc)
            (map> inc))
          res (->> (succeed> 1)
                   (inc-twice>)
                   (run-sync!))]
      (is (= 3 res)))))

(deftest cond>-test
  (testing "Runs the first effect that satisfies the condition"
    (let [eff (->>
                (succeed> 1)
                (cond>
                  odd? (comp succeed> inc)
                  even? (comp succeed> dec)))]
      (is (= 2 (run-sync! eff))))
    (let [eff (->>
                (succeed> 2)
                (cond>
                  odd? (comp succeed> inc)
                  even? (comp succeed> dec)))]
      (is (= 1 (run-sync! eff))))
    (let [eff (->>
                (succeed> 1)
                (cond>
                  ; Always false
                  (comp not any?) (constantly (succeed> 1))
                  ; Always false
                  (comp not any?) (constantly (succeed> 2))
                  any? (constantly (succeed> 3))))]
      (is (= 3 (run-sync! eff))))
    (let [eff (->>
                (succeed> 42)
                (cond>
                  (comp not any?) (constantly (succeed> 1))
                  (comp not any?) (constantly (succeed> 2))
                  (comp not any?) (constantly (succeed> 3))))]
      (is (failure? (run-sync! eff))))))

(deftest do>-test
  (testing "do> propagates failure"
    (let [res (->> (fail> :test {})
                   (do> (constantly (succeed> 1)))
                   (run-sync!))]
      (is (failure? res))))
  (testing "do> runs the side effect"
    (let [res (->> (succeed> 1)
                   (do> (fn [_] (print "Should print")))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res))))
  (testing "do> does not affect the value"
    (let [res (->> (succeed> 1)
                   (do> (fn [_] (print "Should print")))
                   (run-sync!))]
      (is (= 1 res)))))

(deftest all>-test
  (testing "all> returns a vector of the results of the effects"
    (let [res (->> (all> [(succeed> 1)
                          (succeed> 2)])
                   (run-sync!))]
      (is (= [1 2] res))))
  (testing "all> runs all side effects"
    (let [res (->> (all> [(->> (succeed> 1)
                               (do> (fn [_] (print "Should print 1"))))
                          (->> (succeed> 2)
                               (do> (fn [_] (print "Should print 2"))))])
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print 1Should print 2" res)))))


(deftest catch-all>-test
  (testing "catch-all> catches all exceptions"
    (let [res (->> (fail> :test {})
                   (catch-all> (constantly (succeed> 1)))
                   (run-sync!))]
      (is (= 1 res)))
    (let [res (->> (fail> :test {})
                   (catch-all> (constantly (fail> :test {})))
                   (run-sync!))]
      (is (failure? res))))
  (testing "catch-all> runs the side effect"
    (let [res (->> (fail> :test {})
                   (catch-all> (fn [_]
                                 (print "Should print")
                                 (succeed> 1)))
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res))))
  (testing "catch-all> propagates the value if not a failure"
    (let [res (->> (succeed> 1)
                   (catch-all> (constantly (succeed> 2)))
                   (run-sync!))]
      (is (= 1 res))))
  (testing "catch-all> body does not evaluate if not a failure"
    (let [res (->> (succeed> 1)
                   (catch-all> (fn [_]
                                 (print "Should not print")
                                 (succeed> 2)))
                   (run-sync!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "catch-all> body is a function that takes the failure"
    (let [res (->> (fail> :test {:num 1})
                   (catch-all> (fn [{:keys [type]
                                     {:keys [num]} :data}]
                                 (case type
                                   :test (succeed> num)
                                   (fail> :test {}))))
                   (run-sync!))]
      (is (= 1 res)))))

(deftest catch>-test
  (testing "catch> catches specific exceptions"
    (let [res (->> (fail> :test {})
                   (catch> {:test (constantly (succeed> 1))})
                   (run-sync!))]
      (is (= 1 res)))
    (let [res (->> (fail> :test {})
                   (catch> {:test (constantly (fail> :other {}))})
                   (run-sync!))]
      (is (= (make-failure :other {}) res))))
  (testing "catch> runs the side effect"
    (let [res (->> (fail> :test {})
                   (catch> {:test (fn [_]
                                    (print "Should print")
                                    (succeed> 1))})
                   (run-sync!)
                   (with-out-str))]
      (is (= "Should print" res))))
  (testing "catch> propagates the value if not a failure"
    (let [res (->> (succeed> 1)
                   (catch> {:test (constantly (succeed> 2))})
                   (run-sync!))]
      (is (= 1 res))))
  (testing "catch> body does not evaluate if not a failure"
    (let [res (->> (succeed> 1)
                   (catch> {:test (fn [_]
                                    (print "Should not print")
                                    (succeed> 2))})
                   (run-sync!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "catch> propagates the failure if not caught"
    (let [res (->> (fail> :test {})
                   (catch> {:other (constantly (succeed> 1))})
                   (run-sync!))]
      (is (= (make-failure :test {}) res)))
    (let [res (->> (fail> :test {})
                   (catch> {:other (constantly (fail> :test {}))})
                   (run-sync!))]
      (is (= (make-failure :test {}) res)))))
