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
      (is (= err-data (error-data fail))))))

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
    (testing "Return value is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :succeed (:effect-type eff))))
    (testing "Effect run function is correct"
      (is (= 1 (run-sync! eff))))))

(deftest fail>-test
  (let [err-data {:a 1}
        eff (fail> :test err-data)]
    (testing "Return value of fail> is a valid effect"
      (is (effect? eff)))
    (testing "Effect type is correct"
      (is (= :fail (:effect-type eff))))
    (testing "Effect run function returns a failure"
      (let [res (run-sync! eff)]
        (is (failure? res))
        (is (= :test (:type (run-sync! eff))))
        (is (= err-data (error-data (run-sync! eff))))))))

(deftest map>-test
  (testing "map> propagates failure"
    (let [res (-> (fail> :test {})
                  (map> (constantly 1))
                  (run-sync!))]
      (is (failure? res))))
  (testing "map> run function should not evaluate on failure"
    (let [res (-> (fail> :test {})
                  (map> (fn [_]
                          (print "Should not print")
                          1))
                  (run-sync!)
                  (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "map> applies function to successful effect's value"
    (let [res (-> (succeed> 1)
                  (map> inc)
                  (run-sync!))]
      (is (= 2 res))))
  (testing "map> run function should evaluate on success"
    (let [res (-> (succeed> 1)
                  (map> (fn [x]
                          (print "Should print")
                          (inc x)))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res)))))

(deftest do>-test
  (testing "do> propagates failure"
    (let [res (-> (fail> :test {})
                  (do> (constantly (succeed> 1)))
                  (run-sync!))]
      (is (failure? res))))
  (testing "do> runs the side effect"
    (let [res (-> (succeed> 1)
                  (do> (fn [_] (print "Should print")))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res))))
  (testing "do> does not affect the value"
    (let [res (-> (succeed> 1)
                  (do> (fn [_] (print "Should print")))
                  (run-sync!))]
      (is (= 1 res)))))

(deftest mapcat>-test
  (testing "mapcat> propagates failure"
    (let [res (-> (fail> :test {})
                  (mapcat> (succeed> 10))
                  (run-sync!))]
      (is (failure? res))))
  (testing "mapcat> run function should not evaluate on failure"
    (let [res (-> (fail> :test {})
                  (mapcat> (->
                             (succeed> 10)
                             (do> (fn [_] (print "Should not print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "mapcat> applies function to successful effect's value"
    (let [res (-> (succeed> 1)
                  (mapcat> (map> inc))
                  (run-sync!))]
      (is (= 2 res))))
  (testing "mapcat> run function should evaluate on success"
    (let [res (-> (succeed> 1)
                  (mapcat> (-> (map> inc)
                               (do> (fn [_] (print "Should print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res)))))

(deftest if>-test
  (testing "if> propagates failure"
    (let [res (-> (fail> :test {})
                  (if> any?
                       (succeed> 1)
                       (succeed> 2))
                  (run-sync!))]
      (is (failure? res))))

  (testing "if> runs the then branch if condition is true"
    (let [res (-> (succeed> true)
                  (if> (map> true?)
                       (succeed> 1)
                       (succeed> 2))
                  (run-sync!))]
      (is (= 1 res))))

  (testing "side effects in the `else` branch are not evaluated if condition is true"
    (let [res (-> (succeed> true)
                  (if> (map> true?)
                       (-> (succeed> 1)
                           (do> (fn [_] (print "Should print"))))
                       (-> (succeed> 2)
                           (do> (fn [_] (print "Should not print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res))))

  (testing "if> runs the else branch if condition is false"
    (let [eff (-> (succeed> false)
                  (if> (map> true?)
                       (succeed> 1)
                       (succeed> 2)))
          res (run-sync! eff)]
      (is (= 2 res))))
  (testing "side effects in the `then` branch are not evaluated if condition is false"
    (let [res (-> (succeed> false)
                  (if> (map> true?)
                       (fn [_]
                         (-> (succeed> 1)
                             (do> (constantly (print "Should not print")))))
                       (-> (succeed> 2)
                           (do> (constantly (print "Should print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res)))))

(deftest cond>-test
  (testing "Runs the first effect that satisfies the condition"
    (let [eff (->
                (succeed> 1)
                (cond>
                  (map> odd?) (map> inc)
                  (map> even?) (map> dec)))]
      (is (= 2 (run-sync! eff))))
    (let [eff (->
                (succeed> 2)
                (cond>
                  (map> odd?) (map> inc)
                  (map> even?) (map> dec)))]
      (is (= 1 (run-sync! eff))))
    (let [eff (->
                (succeed> 1)
                (cond>
                  ; Always false
                  (map> (comp not any?)) (succeed> 1)
                  ; Always false
                  (map> (comp not any?)) (succeed> 2)
                  (map> any?) (succeed> 3)))]
      (is (= 3 (run-sync! eff))))
    (let [eff (->
                (succeed> 42)
                (cond>
                  (map> (comp not any?)) (succeed> 1)
                  (map> (comp not any?)) (succeed> 2)
                  (map> (comp not any?)) (succeed> 3)))]
      (is (failure? (run-sync! eff))))))

(deftest all>-test
  (testing "all> returns a vector of the results of the effects"
    (let [res (-> (all> [(succeed> 1)
                         (succeed> 2)])
                  (run-sync!))]
      (is (= [1 2] res))))
  (testing "all> runs all side effects"
    (let [res (-> (all> [(-> (succeed> 1)
                             (do> (fn [_] (print "Should print 1"))))
                         (-> (succeed> 2)
                             (do> (fn [_] (print "Should print 2"))))])
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print 1Should print 2" res)))))


(deftest catchall>-test
  (testing "catchall> catches all exceptions"
    (let [res (-> (fail> :test {})
                  (catchall> (succeed> 1))
                  (run-sync!))]
      (is (= 1 res)))
    (let [res (-> (fail> :test {})
                  (catchall> (fail> :test {}))
                  (run-sync!))]
      (is (failure? res))))
  (testing "catchall> runs the side effect"
    (let [res (-> (fail> :test {})
                  (catchall> (-> (succeed> 1)
                                 (do> (fn [_] (print "Should print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res))))
  (testing "catchall> propagates the value if not a failure"
    (let [res (-> (succeed> 1)
                  (catchall> (succeed> 2))
                  (run-sync!))]
      (is (= 1 res))))
  (testing "catchall> body does not evaluate if not a failure"
    (let [res (-> (succeed> 1)
                  (catchall> (-> (succeed> 1)
                                 (do> (fn [_] (print "Should print")))))
                  (run-sync!)
                  (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "catchall> body is a function that takes the failure"
    (let [res (-> (fail> :test {:num 1})
                  (catchall> (if> (map> (comp #{:test} :type))
                                  (map> (comp :num :error-data))
                                  (succeed> 10)))
                  (run-sync!))]
      (is (= 1 res)))))

(deftest catch>-test
  (testing "catch> catches specific exceptions"
    (let [res (-> (fail> :test {})
                  (catch> {:test (succeed> 1)})
                  (run-sync!))]
      (is (= 1 res))))
  (testing "catch> runs the side effect"
    (let [res (-> (fail> :test {})
                  (catch> {:test (-> (succeed> 1)
                                     (do> (fn [_] (print "Should print"))))})
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res))))
  (testing "catch> propagates the value if not a failure"
    (let [res (-> (succeed> 1)
                  (catch> {:test (succeed> 2)})
                  (run-sync!))]
      (is (= 1 res))))
  (testing "catch> body does not evaluate if not a failure"
    (let [res (-> (succeed> 1)
                  (catch> {:test (-> (succeed> 2)
                                     (do> (fn [_] (print "Should print"))))})
                  (run-sync!)
                  (with-out-str))]
      (is (not= "Should not print" res))))
  (testing "catch> propagates the failure if not caught"
    (let [res (-> (fail> :test {})
                  (catch> {:other (succeed> 1)})
                  (run-sync!))]
      (is (= (make-failure :test {}) res))))
  (testing "catch> input is error data"
    (let [res (-> (fail> :test {:a 1})
                  (catch> {:test (map> #(update % :a inc))})
                  (run-sync!))]
      (is (= {:a 2} res)))))
