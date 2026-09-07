(ns fx.core-test
  (:refer-clojure :exclude [tap>])
  (:require [clojure.test :refer :all]
            [fx.core :refer :all]))

(deftest make-effect-test
  (let [eff (make-effect :test nil {:a 1})]
    (testing "Return value of make-effect is a valid effect"
      (is (effect? eff)))
    (testing "Effect tag is correct"
      (is (= :test (:tag eff)))
      (is (= :test (tag eff))))
    (testing "Effect data is accessible"
      (is (= {:a 1} (:data eff))))))

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
  (let [eff (make-effect :test nil {:a 1})
        fail (make-failure :test {:a 1})
        res (maybe-propagate-failure eff 1)
        res-fail (maybe-propagate-failure fail 1)]
    (testing "Returns value if not a failure"
      (is (= 1 res)))
    (testing "Returns failure if a failure"
      (is (= fail res-fail)))))

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

(deftest tap>-test
  (testing "tap> returns a valid effect"
    (let [eff (tap> (fn [_] nil))]
      (is (effect? eff))
      (is (= :tap (:tag eff)))))
  (testing "tap> propagates failure"
    (let [res (-> (fail> :test {})
                  (tap> (constantly (succeed> 1)))
                  (run-sync!))]
      (is (failure? res))))
  (testing "tap> runs the side effect"
    (let [res (-> (succeed> 1)
                  (tap> (fn [_] (print "Should print")))
                  (run-sync!)
                  (with-out-str))]
      (is (= "Should print" res))))
  (testing "tap> does not affect the value"
    (let [res (-> (succeed> 1)
                  (tap> (fn [_] (print "Should print")))
                  (run-sync!))]
      (is (= 1 res)))))

(deftest tap-error>-test
  (testing "tap-error> returns a valid effect"
    (let [eff (tap-error> (fn [_] nil))]
      (is (effect? eff))
      (is (= :tap-error (:tag eff)))))
  (testing "tap-error> runs side effect on failure and receives failure object"
    (let [caught (atom nil)
          res (-> (fail> :not-found {:id 42})
                  (tap-error> (fn [err] (reset! caught err)))
                  (run-sync!))]
      (is (failure? res))
      (is (= :not-found (:tag res)))
      (is (= {:id 42} (error-data res)))
      (is (= res @caught))))
  (testing "tap-error> does not run side effect on success and preserves success value"
    (let [called (atom false)
          res (-> (succeed> 42)
                  (tap-error> (fn [_] (reset! called true)))
                  (run-sync!))]
      (is (= 42 res))
      (is (false? @called)))))

(deftest ensure>-test
  (testing "ensure> returns a valid effect"
    (let [eff (ensure> (succeed> :cleaned))]
      (is (effect? eff))
      (is (= :ensure (:tag eff)))))
  (testing "ensure> runs finalizer on success and preserves original value"
    (let [cleaned (atom false)
          res (-> (succeed> 42)
                  (ensure> (tap> (fn [_] (reset! cleaned true))))
                  (run-sync!))]
      (is (= 42 res))
      (is (true? @cleaned))))
  (testing "ensure> runs finalizer on failure and preserves original failure"
    (let [cleaned (atom false)
          res (-> (fail> :db-error {:code 500})
                  (ensure> (tap> (fn [_] (reset! cleaned true))))
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
                  (ensure> (tap> (fn [_] (/ 1 0))))
                  (run-sync!))]
      (is (failure? res))
      (is (= :ensure (:tag res)))
      (is (instance? #?(:clj Throwable :cljs :default) (error-data res)))))
  (testing "ensure> executes effect finalizer preserving upstream value"
    (let [cleaned (atom false)
          res (-> (succeed> "hello")
                  (ensure> (tap> (fn [_] (reset! cleaned true))))
                  (run-sync!))]
      (is (= "hello" res))
      (is (true? @cleaned))))
  (testing "ensure> works standalone"
    (let [cleaned (atom false)
          res (run-sync! (ensure> (tap> (fn [_] (reset! cleaned true)))))]
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
                  (try> (tap> (fn [_] (print "Should print"))))
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
                             (tap> (fn [_] (print "Should not print")))))
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
                               (tap> (fn [_] (print "Should print")))))
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
                           (tap> (fn [_] (print "Should print"))))
                       (-> (succeed> 2)
                           (tap> (fn [_] (print "Should not print")))))
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
                             (tap> (constantly (print "Should not print")))))
                       (-> (succeed> 2)
                           (tap> (constantly (print "Should print")))))
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
                             (tap> (fn [_] (print "Should print 1"))))
                         (-> (succeed> 2)
                             (tap> (fn [_] (print "Should print 2"))))])
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
                                 (tap> (fn [_] (print "Should print")))))
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
                                 (tap> (fn [_] (print "Should print")))))
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
                                     (tap> (fn [_] (print "Should print"))))})
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
                                     (tap> (fn [_] (print "Should print"))))})
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

(deftest acquire-release>-test
  (testing "successful acquire, use, and release lifecycle"
    (let [released (atom false)
          res (run-sync!
                (acquire-release>
                  (succeed> {:db "conn"})
                  (fn [conn] (succeed> (str (:db conn) "-data")))
                  (fn [conn] (succeed> (reset! released true)))))]
      (is (= "conn-data" res))
      (is (true? @released))))

  (testing "release executes even when usage fails with IFailure"
    (let [released (atom false)
          res (run-sync!
                (acquire-release>
                  (succeed> {:db "conn"})
                  (fn [_] (fail> :query-error {:code 500}))
                  (fn [_] (succeed> (reset! released true)))))]
      (is (failure? res))
      (is (= :query-error (:tag res)))
      (is (= {:code 500} (error-data res)))
      (is (true? @released))))

  (testing "release executes when usage throws an exception and rethrows exception"
    (let [released (atom false)]
      (is (thrown-with-msg?
            #?(:clj Exception :cljs :default)
            #"boom"
            (run-sync!
              (acquire-release>
                (succeed> {:db "conn"})
                (fn [_] (throw (ex-info "boom" {:error :crash})))
                (fn [_] (succeed> (reset! released true)))))))
      (is (true? @released))))

  (testing "acquisition failure skips use and release"
    (let [used (atom false)
          released (atom false)
          res (run-sync!
                (acquire-release>
                  (fail> :conn-failed {:reason :timeout})
                  (fn [_] (reset! used true) (succeed> 1))
                  (fn [_] (reset! released true) (succeed> 2))))]
      (is (failure? res))
      (is (= :conn-failed (:tag res)))
      (is (false? @used))
      (is (false? @released)))))

(deftest die>-and-or-die>-test
  (testing "die> throws unhandled defect ExceptionInfo"
    (is (thrown-with-msg?
          #?(:clj Exception :cljs :default)
          #"Effect defect encountered"
          (run-sync! (die> {:reason :fatal})))))

  (testing "or-die> passes success value through untouched"
    (let [res (-> (succeed> 42)
                  (or-die>)
                  (run-sync!))]
      (is (= 42 res))))

  (testing "or-die> throws ExceptionInfo when upstream is a failure"
    (try
      (-> (fail> :unauthorized {:user "bob"})
          (or-die>)
          (run-sync!))
      (is false "Expected exception was not thrown")
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
        (is (= :unauthorized (:tag (ex-data e))))
        (is (= {:user "bob"} (:error-data (ex-data e)))))))

  (testing "or-die> with custom message"
    (is (thrown-with-msg?
          #?(:clj clojure.lang.ExceptionInfo :cljs :default)
          #"Fatal database glitch"
          (-> (fail> :db-down {:cluster "east"})
              (or-die> "Fatal database glitch")
              (run-sync!))))))

(deftest try>-exception-mapping-test
  (testing "try> maps specific exception types using map handler"
    #?(:clj
       (let [res (-> (try> (map> (fn [_] (throw (java.io.IOException. "disk full"))))
                           {java.io.IOException :io-failure
                            ArithmeticException :math-failure})
                     (run-sync!))]
         (is (failure? res))
         (is (= :io-failure (:tag res)))))))

(deftest match>-test
  (testing "match> converges success channel"
    (let [res (-> (succeed> {:name "Alice"})
                  (match>
                    (fn [err] (str "Error: " (tag err)))
                    (fn [user] (str "Hello " (:name user))))
                  (run-sync!))]
      (is (= "Hello Alice" res))))

  (testing "match> converges failure channel into success value"
    (let [res (-> (fail> :user-not-found {:id 101})
                  (match>
                    (fn [err] (str "Recovered from " (tag err)))
                    (fn [val] (str "Success " val)))
                  (run-sync!))]
      (is (= "Recovered from :user-not-found" res)))))

(deftest or-else>-test
  (testing "or-else> runs fallback effect when upstream fails"
    (let [res (-> (fail> :cache-miss {:key "user-1"})
                  (or-else> (succeed> {:user "default"}))
                  (run-sync!))]
      (is (= {:user "default"} res))))

  (testing "or-else> skips fallback effect when upstream succeeds"
    (let [called (atom false)
          res (-> (succeed> {:user "Alice"})
                  (or-else> (tap> (fn [_] (reset! called true))))
                  (run-sync!))]
      (is (= {:user "Alice"} res))
      (is (false? @called)))))

(deftest or-else-fail>-test
  (testing "or-else-fail> replaces failure with new tagged failure"
    (let [res (-> (fail> :raw-sql-error {:code 1045})
                  (or-else-fail> :auth-error {:msg "Access denied"})
                  (run-sync!))]
      (is (failure? res))
      (is (= :auth-error (:tag res)))
      (is (= {:msg "Access denied"} (error-data res)))))

  (testing "or-else-fail> preserves success value"
    (let [res (-> (succeed> 42)
                  (or-else-fail> :auth-error {:msg "Failed"})
                  (run-sync!))]
      (is (= 42 res)))))

(deftest retry>-test
  (testing "retry> succeeds on subsequent attempt after transient failure"
    (let [attempts (atom 0)
          flaky-eff (map> (fn [_]
                            (let [n (swap! attempts inc)]
                              (if (< n 3)
                                (make-failure :transient-error {:attempt n})
                                {:status :ok :attempts n}))))
          res (run-sync! (retry> flaky-eff {:max-attempts 4 :delay-ms 1}))]
      (is (= {:status :ok :attempts 3} res))
      (is (= 3 @attempts))))

  (testing "retry> exhausts attempts and returns final failure"
    (let [attempts (atom 0)
          always-fail (map> (fn [_]
                              (swap! attempts inc)
                              (make-failure :persistent-error {:count @attempts})))
          res (run-sync! (retry> always-fail {:max-attempts 3 :delay-ms 1}))]
      (is (failure? res))
      (is (= :persistent-error (:tag res)))
      (is (= 3 @attempts))))

  (testing "retry> respects :retry-if predicate"
    (let [attempts (atom 0)
          selective-fail (map> (fn [_]
                                 (swap! attempts inc)
                                 (make-failure :fatal-unrecoverable {})))
          res (run-sync!
                (retry> selective-fail
                  {:max-attempts 5
                   :delay-ms 1
                   :retry-if (fn [err] (not= :fatal-unrecoverable (tag err)))}))]
      (is (failure? res))
      (is (= 1 @attempts)))))

(deftest for-each>-test
  (testing "for-each> maps collection and gathers results into vector"
    (let [res (run-sync! (for-each> [1 2 3] (fn [x] (succeed> (* x 10)))))]
      (is (= [10 20 30] res))))

  (testing "for-each> short-circuits on first failure"
    (let [evaluated (atom [])
          res (run-sync!
                (for-each> [1 2 3 4]
                  (fn [x]
                    (swap! evaluated conj x)
                    (if (= x 2)
                      (fail> :invalid-item {:item x})
                      (succeed> (* x 2))))))]
      (is (failure? res))
      (is (= :invalid-item (:tag res)))
      (is (= {:item 2} (error-data res)))
      (is (= [1 2] @evaluated))))

  (testing "for-each> returns empty vector on empty collection without evaluating"
    (let [evaluated (atom false)
          res (run-sync! (for-each> [] (fn [x] (reset! evaluated true) (succeed> x))))]
      (is (= [] res))
      (is (false? @evaluated)))))

(deftest zip>-and-zip-with>-test
  (testing "zip> combines two successful effects into a pair"
    (let [res (run-sync! (zip> (succeed> :user) (succeed> [1 2 3])))]
      (is (= [:user [1 2 3]] res))))

  (testing "zip-with> applies binary function over effect results"
    (let [res (run-sync!
                (zip-with>
                  (succeed> {:name "Alice"})
                  (succeed> {:role :admin})
                  (fn [u r] (merge u r))))]
      (is (= {:name "Alice" :role :admin} res))))

  (testing "zip> short-circuits on first effect failure"
    (let [called-b (atom false)
          res (run-sync!
                (zip> (fail> :error-a {:code 1})
                      (map> (fn [_] (reset! called-b true) 2))))]
      (is (failure? res))
      (is (= :error-a (:tag res)))
      (is (false? @called-b))))

  (testing "zip> short-circuits on second effect failure"
    (let [res (run-sync!
                (zip> (succeed> :ok)
                      (fail> :error-b {:code 2})))]
      (is (failure? res))
      (is (= :error-b (:tag res)))))

  (testing "zip> handles nil result without failure"
    (let [res (run-sync! (zip> (succeed> nil) (succeed> 42)))]
      (is (= [nil 42] res)))))

(deftest sleep>-and-run-async!-test
  (testing "sleep> propagates value after pause"
    (let [res (run-sync! (-> (succeed> "hello")
                             (sleep> 1)
                             (map> #(str % " world"))))]
      (is (= "hello world" res))))

  (testing "run-async! asynchronously evaluates effect pipeline"
    (let [res-future (run-async! (-> (succeed> 100)
                                     (sleep> 1)
                                     (map> inc)))]
      #?(:clj  (is (= 101 (.get ^java.util.concurrent.CompletableFuture res-future)))
         :cljs (is (some? res-future))))))

(deftest ast-transparency-test
  (testing "All effect records expose transparent :tag, :prev-effect, and :data map"
    (let [m (map> inc)
          r (retry> m {:max-attempts 5 :delay-ms 50})
          s (service> :db :default-db)
          p (provide> s {:db "mock"})
          e (ensure> m (succeed> :done))]
      (is (= :map (:tag m)))
      (is (nil? (:prev-effect m)))
      (is (= inc (:f (:data m))))

      (is (= :retry (:tag r)))
      (is (= {:max-attempts 5 :delay-ms 50} (:policy (:data r))))
      (is (= m (:target (:data r))))

      (is (= :context (:tag s)))
      (is (= :db (:key (:data s))))
      (is (= :default-db (:default (:data s))))

      (is (= :provide (:tag p)))
      (is (= s (:body (:data p))))
      (is (= {:db "mock"} (:context-map (:data p))))

      (is (= :ensure (:tag e)))
      (is (= m (:prev-effect e)))
      (is (= (succeed> :done) (:finalizer (:data e)))))))

(deftest pure-context-propagation-test
  (testing "Context is explicitly passed and isolated across nested provide> scopes"
    (let [nested-eff
          (-> (service> :tier)
              (provide> {:tier :outer})
              (map> (fn [outer-val]
                      (-> (service> :tier)
                          (provide> {:tier :inner})
                          (map> (fn [inner-val]
                                  [outer-val inner-val])))))
              (mapcat> identity))]
      (is (= [:outer :inner] (run-sync! nested-eff)))))

  (testing "provide> restores parent context after child block terminates"
    (let [log (atom [])
          eff (-> (-> (service> :auth)
                      (do-ctx> (fn [v ctx] (swap! log conj [:inside v (:auth ctx)]))))
                  (provide> {:auth :elevated})
                  (do-ctx> (fn [v ctx] (swap! log conj [:outside v (:auth ctx)])))
                  (map-ctx> (fn [_ ctx] (:auth ctx))))
          final-ctx (run-sync! eff {:auth :standard})]
      (is (= :standard final-ctx))
      (is (= [[:inside :elevated :elevated]
              [:outside :elevated :standard]]
             @log))))

  (testing "Multiple parallel services in context"
    (let [eff (zip-with> (service> :host) (service> :port) (fn [h p] (str h ":" p)))
          res (run-sync! eff {:host "localhost" :port 8080})]
      (is (= "localhost:8080" res)))))

(deftest stack-safety-deep-pipeline-test
  (testing "100,000-step linear pipeline executes in constant JVM stack space without StackOverflowError"
    (let [pipeline (reduce (fn [eff _] (map> eff inc))
                           (succeed> 0)
                           (range 100000))
          res (run-sync! pipeline)]
      (is (= 100000 res))))

  (testing "Deeply nested recursive mapcat> pipeline executes safely"
    (let [build-chain (fn build-chain [n]
                        (if (zero? n)
                          (succeed> 0)
                          (-> (succeed> 1)
                              (mapcat> (fn [x]
                                         (-> (build-chain (dec n))
                                             (map> (fn [acc] (+ x acc)))))))))
          res (run-sync! (build-chain 5000))]
      (is (= 5000 res))))

  (testing "Deeply nested if> conditional tree executes safely"
    (let [build-if-tree (fn build-if-tree [n]
                          (if (zero? n)
                            (succeed> :bottom)
                            (if> (succeed> true)
                                 (build-if-tree (dec n))
                                 (succeed> :wrong))))
          res (run-sync! (build-if-tree 5000))]
      (is (= :bottom res)))))

(defrecord CustomMultiplierFrame [factor]
  IContinuation
  (-resume [_ val context stack]
    [nil (* val factor) context stack]))

(defrecord CustomMultiplyEffect [prev factor]
  ITagged
  (tag [_] :custom-multiply)
  IEffect
  (prev-effect [_] prev)
  (-step [this val context stack]
    (if (some? prev)
      [prev val context (conj stack (->StepEffectFrame (assoc this :prev nil)))]
      [nil val context (conj stack (->CustomMultiplierFrame factor))])))

(deftest continuation-protocol-extensibility-test
  (testing "Custom effect record and continuation frame integrate seamlessly into run-sync!"
    (let [eff (-> (succeed> 10)
                  (->CustomMultiplyEffect 5)
                  (map> inc))]
      (is (= 51 (run-sync! eff)))))

  (testing "Unwindable frames clean up resources during exceptions in inner effect pipelines"
    (let [released (atom false)
          eff (acquire-release>
                (succeed> :resource)
                (fn [_] (map> (fn [_] (throw (ex-info "Simulated defect" {:boom true})))))
                (fn [_] (reset! released true)))]
      (is (thrown? Exception (run-sync! eff)))
      (is (true? @released))))

  (testing "Unwindable frames clean up resources during direct exceptions in use function"
    (let [released (atom false)
          eff (acquire-release>
                (succeed> :resource)
                (fn [_] (throw (ex-info "Direct defect" {:boom true})))
                (fn [_] (reset! released true)))]
      (is (thrown? Exception (run-sync! eff)))
      (is (true? @released)))))
