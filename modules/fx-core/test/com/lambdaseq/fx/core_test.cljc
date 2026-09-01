(ns com.lambdaseq.fx.core-test
  (:require [clojure.test :refer :all]
            [com.lambdaseq.fx.core :refer :all]))

(deftest make-effect-test
  (let [run (constantly 1)
        eff (make-effect :test nil run)]
    (testing "Return value of make-effect is a valid effect"
      (is (effect? eff)))
    (testing "Effect tag is correct"
      (is (= :test (:tag eff)))
      (is (= :test (tag eff))))
    (testing "Effect run function is correct"
      (is (= 1 (run-sync! eff))))))

(deftest make-failure-test
  (let [err-data {:a 1}
        fail (make-failure :test err-data)]
    (testing "Return value of make-failure is a valid failure"
      (is (failure? fail)))
    (testing "Failure tag is correct"
      (is (= :test (:tag fail)))
      (is (= :test (tag fail))))
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
    (testing "Effect tag is correct"
      (is (= :succeed (:tag eff)))
      (is (= :succeed (tag eff))))
    (testing "Effect run function is correct"
      (is (= 1 (run-sync! eff))))))

(deftest fail>-test
  (let [err-data {:a 1}
        eff (fail> :test err-data)]
    (testing "Return value of fail> is a valid effect"
      (is (effect? eff)))
    (testing "Effect tag is correct"
      (is (= :fail (:tag eff)))
      (is (= :fail (tag eff))))
    (testing "Effect run function returns a failure"
      (let [res (run-sync! eff)]
        (is (failure? res))
        (is (= :test (:tag (run-sync! eff))))
        (is (= :test (tag (run-sync! eff))))
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

(deftest ensure>-test
  (testing "ensure> returns a valid effect"
    (let [eff (ensure> (succeed> :cleaned))]
      (is (effect? eff))
      (is (= :ensure (:tag eff)))))
  (testing "ensure> runs finalizer on success and preserves original value"
    (let [cleaned (atom false)
          res (-> (succeed> 42)
                  (ensure> (do> (fn [_] (reset! cleaned true))))
                  (run-sync!))]
      (is (= 42 res))
      (is (true? @cleaned))))
  (testing "ensure> runs finalizer on failure and preserves original failure"
    (let [cleaned (atom false)
          res (-> (fail> :db-error {:code 500})
                  (ensure> (do> (fn [_] (reset! cleaned true))))
                  (run-sync!))]
      (is (failure? res))
      (is (= :db-error (:tag res)))
      (is (= {:code 500} (error-data res)))
      (is (true? @cleaned))))
  (testing "ensure> propagates finalizer failure when upstream succeeds"
    (let [res (-> (succeed> 42)
                  (ensure> (fail> :cleanup-failed {:reason "disk full"}))
                  (run-sync!))]
      (is (failure? res))
      (is (= :cleanup-failed (:tag res)))
      (is (= {:reason "disk full"} (error-data res)))))
  (testing "ensure> propagates finalizer failure when upstream fails"
    (let [res (-> (fail> :orig-error {:orig 1})
                  (ensure> (fail> :cleanup-failed {:reason "disk full"}))
                  (run-sync!))]
      (is (failure? res))
      (is (= :cleanup-failed (:tag res)))
      (is (= {:reason "disk full"} (error-data res)))))
  (testing "ensure> catches thrown exceptions in finalizer as :ensure failure"
    (let [res (-> (succeed> 42)
                  (ensure> (do> (fn [_] (/ 1 0))))
                  (run-sync!))]
      (is (failure? res))
      (is (= :ensure (:tag res)))
      (is (instance? #?(:clj Throwable :cljs :default) (error-data res)))))
  (testing "ensure> accepts plain function as finalizer"
    (let [cleaned (atom false)
          res (-> (succeed> "hello")
                  (ensure> (fn [_] (reset! cleaned true)))
                  (run-sync!))]
      (is (= "hello" res))
      (is (true? @cleaned))))
  (testing "ensure> works standalone"
    (let [cleaned (atom false)
          res (run-sync! (ensure> (do> (fn [_] (reset! cleaned true)))))]
      (is (nil? res))
      (is (true? @cleaned)))))

(deftest try>-test
  (testing "try> returns a valid effect"
    (let [eff (try> (map> inc))]
      (is (effect? eff))
      (is (= :try (:tag eff)))))
  (testing "try> applies effect combinator to successful effect's value (point-free)"
    (let [res (-> (succeed> 1)
                  (try> (map> inc))
                  (run-sync!))]
      (is (= 2 res))))
  (testing "try> captures thrown exceptions as failures with default :try type"
    (let [res (-> (succeed> 0)
                  (try> (map> #(/ 10 %)))
                  (run-sync!))]
      (is (failure? res))
      (is (= :try (:tag res)))
      (is (instance? #?(:clj Throwable :cljs :default) (error-data res)))))
  (testing "try> captures thrown exceptions with custom keyword type"
    (let [res (-> (succeed> 0)
                  (try> (map> #(/ 10 %)) :div-zero)
                  (run-sync!))]
      (is (failure? res))
      (is (= :div-zero (:tag res)))
      (is (instance? #?(:clj Throwable :cljs :default) (error-data res)))))
  (testing "try> captures thrown exceptions with custom catch effect returning failure"
    (let [res (-> (succeed> 0)
                  (try> (map> #(/ 10 %)) (map> (fn [e] (make-failure :math-error {:msg #?(:clj (.getMessage e) :cljs (str e))}))))
                  (run-sync!))]
      (is (failure? res))
      (is (= :math-error (:tag res)))
      (is (map? (error-data res)))))
  (testing "try> captures thrown exceptions with custom catch effect recovering with fallback value"
    (let [res (-> (succeed> 0)
                  (try> (map> #(/ 10 %)) (succeed> 42))
                  (run-sync!))]
      (is (= 42 res))))
  (testing "try> works standalone without previous effect"
    (let [res (run-sync! (try> (succeed> 30)))]
      (is (= 30 res)))
    (let [res (run-sync! (try> (map> (fn [_] (/ 1 0)))))]
      (is (failure? res))
      (is (= :try (:tag res))))
    (let [res (run-sync! (try> (map> (fn [_] (/ 1 0))) :div-zero))]
      (is (failure? res))
      (is (= :div-zero (:tag res)))))
  (testing "try> works with options map"
    (let [res (run-sync! (try> {:try (succeed> 3) :catch :err}))]
      (is (= 3 res)))
    (let [res (run-sync! (try> {:try (map> (fn [_] (/ 1 0))) :catch :err}))]
      (is (failure? res))
      (is (= :err (:tag res))))
    (let [res (-> (succeed> 10)
                  (try> {:try (map> (fn [x] (/ x 0))) :catch :zero-err})
                  (run-sync!))]
      (is (failure? res))
      (is (= :zero-err (:tag res)))))
  (testing "try> propagates earlier failures without evaluating body effect"
    (let [res (-> (fail> :test {:a 1})
                  (try> (map> (fn [_]
                                (print "Should not print")
                                1)))
                  (run-sync!)
                  (with-out-str))]
      (is (not= "Should not print" res)))
    (let [res (-> (fail> :test {:a 1})
                  (try> (map> inc))
                  (run-sync!))]
      (is (failure? res))
      (is (= :test (:tag res)))
      (is (= {:a 1} (error-data res)))))
  (testing "try> propagates failure when the inner effect is a fail>"
    (let [res (run-sync! (try> (fail> :inner-error {:msg "failed"})))]
      (is (failure? res))
      (is (= :inner-error (:tag res)))
      (is (= {:msg "failed"} (error-data res))))
    (let [res (-> (succeed> 10)
                  (try> (fail> :inner-error {:msg "failed"}))
                  (run-sync!))]
      (is (failure? res))
      (is (= :inner-error (:tag res)))
      (is (= {:msg "failed"} (error-data res))))
    (let [res (-> (fail> :upstream-error {:msg "upstream"})
                  (try> (fail> :inner-error {:msg "inner"}))
                  (run-sync!))]
      (is (failure? res))
      (is (= :upstream-error (:tag res)))
      (is (= {:msg "upstream"} (error-data res)))))
  (testing "try> evaluates side-effects on success"
    (let [res (-> (succeed> 1)
                  (try> (do> (fn [_] (print "Should print"))))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res)))))

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
                  (catchall> (if> (map> (comp #{:test} :tag))
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

(deftest context>-test
  (testing "context> returns the active context map"
    (let [res (run-sync! (context>) {:env :prod :timeout 3000})]
      (is (= :prod (:env res)))
      (is (= 3000 (:timeout res)))
      (is (fn? (:runner res)))))
  (testing "context> extracts key from context"
    (let [res (run-sync! (context> :env) {:env :prod})]
      (is (= :prod res))))
  (testing "context> returns default value when key is absent"
    (let [res (run-sync! (context> :missing :default))]
      (is (= :default res))))
  (testing "context> integrates into pipeline"
    (let [res (-> (context> :multiplier 2)
                  (map> (fn [m] (* 10 m)))
                  (run-sync! {:multiplier 5}))]
      (is (= 50 res)))))

(deftest service>-test
  (testing "service> extracts service from context"
    (let [mock-db {:query (fn [] [{:id 1}])}
          res (run-sync! (service> :db) {:db mock-db})]
      (is (= mock-db res))))
  (testing "service> returns default value when service is missing"
    (let [res (run-sync! (service> :cache :no-cache))]
      (is (= :no-cache res))))
  (testing "service> pipeline consumption"
    (let [mock-db {:find-user (fn [id] {:id id :name "Alice"})}
          res (-> (service> :db)
                  (map> (fn [db] ((:find-user db) 42)))
                  (run-sync! {:db mock-db}))]
      (is (= {:id 42 :name "Alice"} res)))))

(deftest map-ctx>-test
  (testing "map-ctx> accesses both value and context"
    (let [res (-> (succeed> {:amount 100})
                  (map-ctx> (fn [order {:keys [tax-rate]}]
                              (assoc order :total (* (:amount order) (inc tax-rate)))))
                  (run-sync! {:tax-rate 0.2}))]
      (is (= {:amount 100 :total 120.0} res))))
  (testing "map-ctx> propagates upstream failure"
    (let [res (-> (fail> :err {:code 500})
                  (map-ctx> (fn [v ctx] (assoc v :tax (:tax ctx))))
                  (run-sync! {:tax 10}))]
      (is (failure? res))
      (is (= :err (:tag res))))))

(deftest do-ctx>-test
  (testing "do-ctx> performs side-effect with context and passes value through unchanged"
    (let [logs (atom [])
          res (-> (succeed> 42)
                  (do-ctx> (fn [v {:keys [logger]}]
                             (logger (str "Value was " v))))
                  (map> inc)
                  (run-sync! {:logger (fn [msg] (swap! logs conj msg))}))]
      (is (= 43 res))
      (is (= ["Value was 42"] @logs))))
  (testing "do-ctx> propagates upstream failure without evaluating fn"
    (let [called (atom false)
          res (-> (fail> :err {})
                  (do-ctx> (fn [_ _] (reset! called true)))
                  (run-sync!))]
      (is (failure? res))
      (is (false? @called)))))

(deftest provide>-test
  (testing "provide> injects context map into upstream effect"
    (let [pipeline (-> (service> :db)
                       (map> (fn [db] (:db-name db))))
          res (-> pipeline
                  (provide> {:db {:db-name "analytics"}})
                  (run-sync!))]
      (is (= "analytics" res))))
  (testing "provide> direct construction (provide> context eff)"
    (let [pipeline (-> (service> :api-key)
                       (map> clojure.string/upper-case))
          res (run-sync! (provide> {:api-key "secret"} pipeline))]
      (is (= "SECRET" res))))
  (testing "provide> scopes context without leaking to outer context"
    (let [inner (-> (service> :scope)
                    (provide> {:scope :inner}))
          res (run-sync! (all> [inner (service> :scope :outer)]))]
      (is (= [:inner :outer] res))))
  (testing "provide> propagates failure from within provided effect"
    (let [res (-> (fail> :inner-error {:msg "boom"})
                  (provide> {:foo :bar})
                  (run-sync!))]
      (is (failure? res))
      (is (= :inner-error (:tag res)))))
  (testing "provide> passes through input value in pipeline"
    (let [res (-> (succeed> 10)
                  (provide> (map-ctx> (fn [v ctx] (* v (:multiplier ctx)))) {:multiplier 4})
                  (run-sync!))]
      (is (= 40 res)))))

(deftest provide-service>-test
  (testing "provide-service> injects single key-value dependency in pipeline"
    (let [res (-> (service> :db)
                  (provide-service> :db {:conn "postgres://localhost"})
                  (run-sync!))]
      (is (= {:conn "postgres://localhost"} res))))
  (testing "provide-service> direct construction (provide-service> key val eff)"
    (let [eff (-> (service> :multiplier)
                  (map> (fn [m] (* 5 m))))
          res (run-sync! (provide-service> :multiplier 10 eff))]
      (is (= 50 res)))))

(deftest run-sync!-context-test
  (testing "run-sync! initializes execution with context map"
    (let [res (run-sync! (service> :env) {:env :staging})]
      (is (= :staging res))))
  (testing "run-sync! preserves :runner and prevents runner override"
    (let [res (run-sync! (context>) {:runner :fake-runner :custom 123})]
      (is (= 123 (:custom res)))
      (is (fn? (:runner res))))))
