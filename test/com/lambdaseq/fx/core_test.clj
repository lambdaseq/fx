(ns com.lambdaseq.fx.core-test
  (:require [clojure.test :refer :all]
            [com.lambdaseq.fx.core :refer :all]))

(deftest make-effect-test
  (let [run (constantly 1)
        eff (make-effect :test run)]
    (testing "Return value of make-effect is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :test (:effect-type eff))))
    (testing "Effect run function is correct"
      (is (= 1 (-eval! eff))))))

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
  (let [eff (make-effect :test (constantly 1))
        fail (make-failure :test {:a 1})
        res (maybe-propagate-failure eff 1)
        res-fail (maybe-propagate-failure fail 1)]
    (testing "Returns value if not a failure"
      (is (= 1 res)))
    (testing "Returns failure if a failure"
      (is (= fail res-fail)))))

(deftest succeed>-test
  (let [eff (succeed> 1)]
    (testing "Return value of succeed> is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :succeed (:effect-type eff))))
    (testing "Effect run function is correct"
      (is (= 1 (-eval! eff))))))

(deftest fail>-test
  (let [err-data {:a 1}
        failure (make-failure :test err-data)
        eff (fail> :test err-data)]
    (testing "Return value of fail> is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :fail (:effect-type eff))))
    (testing "Effect run function returns a failure"
      (let [res (-eval! eff)]
        (is (failure? res))
        (is (= :test (:type (-eval! eff))))
        (is (= err-data (:data (-eval! eff))))))))

(deftest map>-test
  (testing "map> propagates failure"
    (let [res (->> (fail> :test {})
                   (map> (constantly 1))
                   (-eval!))]
      (is (failure? res))))
  (testing "map> run function should not evaluate on failure"
    (let [res (->> (fail> :test {})
                   (map> (fn [_]
                           (print "Should not print")
                           1))
                   (-eval!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "map> applies function to successful effect's value"
    (let [res (->> (succeed> 1)
                   (map> inc)
                   (-eval!))]
      (is (= 2 res))))
  (testing "map> run function should evaluate on success"
    (let [res (->> (succeed> 1)
                   (map> (fn [x]
                           (print "Should print")
                           (inc x)))
                   (-eval!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest mapcat>-test
  (testing "mapcat> propagates failure"
    (let [res (->> (fail> :test {})
                   (mapcat> (constantly (succeed> 1)))
                   (-eval!))]
      (is (failure? res))))
  (testing "mapcat> run function should not evaluate on failure"
    (let [res (->> (fail> :test {})
                   (mapcat> (fn [_]
                              (print "Should not print")
                              (succeed> 1)))
                   (-eval!)
                   (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "mapcat> applies function to successful effect's value"
    (let [res (->> (succeed> 1)
                   (mapcat> (fn [x]
                              (succeed> (inc x))))
                   (-eval!))]
      (is (= 2 res))))
  (testing "mapcat> run function should evaluate on success"
    (let [res (->> (succeed> 1)
                   (mapcat> (fn [x]
                              (print "Should print")
                              (succeed> (inc x))))
                   (-eval!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest if>-test
  (testing "if> propagates failure"
    (let [res (->> (fail> :test {})
                   (if> true
                     (constantly (succeed> 1))
                     (constantly (succeed> 2)))
                   (-eval!))]
      (is (failure? res))))

  (testing "if> runs the then branch if condition is true"
    (let [eff (->> (succeed> true)
                   (if> true?
                     (constantly (succeed> 1))
                     (constantly (succeed> 2))))
          res (-eval! eff)]
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
                   (-eval!)
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
          res (-eval! eff)]
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
                   (-eval!)
                   (with-out-str))]
      (is (= "Should print" res)))))

(deftest pipeline>-test
  (testing "Can create a pipeline of effects"
    (let [inc-twice>
          (pipeline>>
            (map> inc)
            (map> inc))
          res (->> (succeed> 1)
                   (inc-twice>)
                   (-eval!))]
      (is (= 3 res)))))

(deftest cond-test
  (testing "Runs the first effect that satisfies the condition"
    (let [eff (->>
                (succeed> 1)
                (cond>
                  odd? (comp succeed> inc)
                  even? (comp succeed> dec)))]
      (is (= 2 (-eval! eff))))
    (let [eff (->>
                (succeed> 2)
                (cond>
                  odd? (comp succeed> inc)
                  even? (comp succeed> dec)))]
      (is (= 1 (-eval! eff))))
    (let [eff (->>
                (succeed> 1)
                (cond>
                  ; Always false
                  (comp not any?) (constantly (succeed> 1))
                  ; Always false
                  (comp not any?) (constantly (succeed> 2))
                  any? (constantly (succeed> 3))))]
      (is (= 3 (-eval! eff))))
    (let [eff (->>
                (succeed> 42)
                (cond>
                  (comp not any?) (constantly (succeed> 1))
                  (comp not any?) (constantly (succeed> 2))
                  (comp not any?) (constantly (succeed> 3))))]
      (is (failure? (-eval! eff))))))
