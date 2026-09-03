(ns com.lambdaseq.fx.typed-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.core]
            [com.lambdaseq.fx.typed]
            [typed.clj.checker.test-utils :as tu :refer [tc-e is-tc-e is-tc-err]]
            [typed.clojure :as t]))

(deftest effect?-ann--test
  (testing "effect? returns boolean"
    (is-tc-e (fx/effect? nil)
             Boolean
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest failure?-ann--test
  (testing "failure? returns boolean"
    (is-tc-e (fx/failure? nil)
             Boolean
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest make-failure-ann--test
  (testing "make-failure returns a failure"
    (is-tc-e (fx/make-failure :foo {})
             (fx/IFailure (t/Val :foo) (t/HMap :complete? true))
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-failure :bar :error)
             (fx/IFailure (t/Val :bar) t/Keyword)
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest make-effect-ann--test
  (testing "make-effect returns an effect"
    (is-tc-e (fx/make-effect :foo nil {:val 1})
             (fx/IEffect t/Nothing Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo nil {:val "str"})
             (fx/IEffect t/Nothing (t/Val "str") t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect"
    (is-tc-e (fx/make-effect :foo
               (fx/make-effect :bar nil {:val 1})
               {:f inc})
             (fx/IEffect Long Long t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo
               (fx/make-effect :bar nil {:val "10"})
               {:f parse-long})
             (fx/IEffect String (t/Option Long) t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect and a failure"
    (is-tc-e (fx/make-effect :foo
               (fx/fail> :ok)
               {:f identity})
             (fx/IEffect
               nil nil
               (fx/IFailure (t/Val :fail) (t/Val :ok))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo
               (fx/fail> :bar)
               {:f some?})
             (fx/IEffect
               nil Boolean
               (fx/IFailure (t/Val :fail) (t/Val :bar))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest succeed>-ann--test
  (testing "succeed> returns an effect with output type inferred"
    (is-tc-e (fx/succeed> 10)
             (fx/IEffect nil Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/succeed> "str")
             (fx/IEffect nil String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest fail>-ann--test
  (testing "fail> returns an effect with failure type and error inferred."
    (is-tc-e (fx/fail> :foo)
             (fx/IEffect
               t/Any nil
               (fx/IFailure (t/Val :fail) (t/Val :foo))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/fail> {})
             (fx/IEffect
               t/Any nil
               (fx/IFailure (t/Val :fail) (t/HMap :complete? true))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest map>-ann--test
  (testing "map> returns an effect with output type inferred"
    (is-tc-e (fx/map> inc)
             (fx/IEffect Long Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map> inc))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map> str))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))

  (testing "chaining map> effects"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map> inc)
                 (fx/map> str))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map> inc)
                 (fx/map> str)
                 (fx/map> parse-long))
             (fx/IEffect t/Any (t/Option Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))

  (testing "map> infers and propagates failure type in its type"
    (is-tc-e (-> (fx/fail> :foo)
                 (fx/map> inc))
             (fx/IEffect t/Any Long
               (fx/IFailure (t/Val :fail) (t/Val :foo))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/fail> :foo)
                 (fx/map> inc)
                 (fx/map> str))
             (fx/IEffect t/Any String
               (fx/IFailure (t/Val :fail) (t/Val :foo))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))

  (testing "map> typing fails when previous' effect output
            does not match the mapping function's input"
    (is-tc-err (-> (fx/succeed> "str")
                   (fx/map> inc))
               :requires [[com.lambdaseq.fx.core :as fx]
                          [com.lambdaseq.fx.typed]])))

(deftest tap>-ann--test
  (testing "tap> returns the output type of it's previous effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/tap> println))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "string")
                 (fx/tap> println))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "tap> propagates failure type of previous effects"
    (is-tc-e (-> (fx/fail> {})
                 (fx/tap> println))
             (fx/IEffect t/Any nil
               (fx/IFailure (t/Val :fail) (t/Val {}))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/fail> {})
                 (fx/map> inc)
                 (fx/tap> println))
             (fx/IEffect t/Any Long
               (fx/IFailure (t/Val :fail) (t/Val {}))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest tap-error>-ann--test
  (testing "tap-error> preserves output type of previous effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/tap-error> println))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "string")
                 (fx/tap-error> println))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "tap-error> propagates failure type of previous effects"
    (is-tc-e (-> (fx/fail> {})
                 (fx/tap-error> println))
             (fx/IEffect t/Any nil
               (fx/IFailure (t/Val :fail) (t/Val {}))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest ensure>-ann--test
  (testing "ensure> returns the output type of its previous effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/ensure> (fx/succeed> :cleaned)))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :ensure) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/ensure> println))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :ensure) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "str")
                 (fx/ensure> (fx/succeed> 123)))
             (fx/IEffect t/Any String (t/Option (fx/IFailure (t/Val :ensure) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "ensure> propagates failure types from previous and finalizer effects"
    (is-tc-e (-> (fx/fail> :upstream)
                 (fx/ensure> (fx/succeed> :cleaned)))
             (fx/IEffect t/Any nil
               (t/Option (t/U (fx/IFailure (t/Val :fail) (t/Val :upstream))
                              (fx/IFailure (t/Val :ensure) t/Any)))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/ensure> (fx/fail> :cleanup)))
             (fx/IEffect t/Any Long
               (t/Option (t/U (fx/IFailure (t/Val :fail) (t/Val :cleanup))
                              (fx/IFailure (t/Val :ensure) t/Any)))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest try>-ann--test
  (testing "try> returns an effect with output and failure inferred"
    (is-tc-e (fx/try> (fx/map> inc))
             (fx/IEffect Long Long (t/Option (fx/IFailure (t/Val :try) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/try> (fx/map> inc)))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :try) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/try> (fx/map> str)))
             (fx/IEffect t/Any String (t/Option (fx/IFailure (t/Val :try) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "try> with custom keyword failure type"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/try> (fx/map> inc) :div-zero))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :div-zero) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "try> with catch mapper function / effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/try> (fx/map> inc) (fn [e] (fx/make-failure :math-error e))))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :math-error) t/Any)) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "try> typing fails when inner effect argument types mismatch"
    (is-tc-err (-> (fx/succeed> "10")
                   (fx/try> (fx/map> inc)))
               :requires [[com.lambdaseq.fx.core :as fx]
                          [com.lambdaseq.fx.typed]]))
  (testing "try> infers and propagates failure types from upstream and inner effects"
    (is-tc-e (-> (fx/fail> :upstream)
                 (fx/try> (fx/map> inc)))
             (fx/IEffect t/Any Long (t/Option (t/U (fx/IFailure (t/Val :fail) (t/Val :upstream))
                                                   (fx/IFailure (t/Val :try) t/Any))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/try> (fx/fail> :inner))
             (fx/IEffect t/Any t/Nothing (t/Option (t/U (fx/IFailure (t/Val :fail) (t/Val :inner))
                                                        (fx/IFailure (t/Val :try) t/Any))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest mapcat>-ann--test
  (testing "mapcat> returns an effect with output type inferred"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/mapcat> (fx/map> inc)))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "10")
                 (fx/mapcat> (fx/map> parse-long)))
             (fx/IEffect t/Any (t/Option Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-err (-> (fx/succeed> "10")
                   (fx/mapcat> (fx/map> inc)))
               :requires [[com.lambdaseq.fx.core :as fx]
                          [com.lambdaseq.fx.typed]])))

(deftest chain>-ann--test
  (testing "chain> chains two effects together"
    (is-tc-e (fx/chain> (fx/succeed> 10)
                        (fx/map> inc))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "chain> propagates failure"
    (is-tc-e (fx/chain> (fx/fail> :foo)
                        (fx/map> inc))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :fail) (t/Val :foo))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest failure->value-ann--test
  (testing "failure->value converts an IFailure to a map"
    (is-tc-e (fx/failure->value (fx/make-failure :test {:a 1}))
             (t/HMap :mandatory {:tag (t/Val :test)
                                 :error-data (t/HMap :mandatory {:a (t/Val 1)})})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest if>-ann--test
  (testing "if> returns union of branch output types"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/if> (fx/map> even?)
                         (fx/map> inc)
                         (fx/map> str)))
             (fx/IEffect t/Any (t/U Long String) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "if> unchained version"
    (is-tc-e (fx/if> (fx/map> even?)
                     (fx/map> inc)
                     (fx/map> dec))
             (fx/IEffect Long Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "if> propagates failure"
    (is-tc-e (-> (fx/fail> :foo)
                 (fx/if> (fx/map> even?)
                         (fx/map> inc)
                         (fx/map> dec)))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :fail) (t/Val :foo))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest cond>-ann--test
  (testing "cond> returns output type of expressions with possible no-conditions failure"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/cond>
                   (fx/map> odd?) (fx/map> inc)
                   (fx/map> even?) (fx/map> dec)))
             (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :cond) (t/Val :no-conditions))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "cond> propagates previous failure"
    (is-tc-e (-> (fx/fail> :foo)
                 (fx/cond>
                   (fx/map> odd?) (fx/map> inc)))
             (fx/IEffect t/Any Long (t/Option (t/U (fx/IFailure (t/Val :fail) (t/Val :foo))
                                                   (fx/IFailure (t/Val :cond) (t/Val :no-conditions)))) '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest all>-ann--test
  (testing "all> returns a vector of results"
    (is-tc-e (fx/all> [(fx/succeed> 1)
                       (fx/succeed> 2)])
             (fx/IEffect t/Any (t/Vec Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "all> with strings"
    (is-tc-e (fx/all> [(fx/succeed> "a")
                       (fx/succeed> "b")])
             (fx/IEffect t/Any (t/Vec String) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest catch>-ann--test
  (testing "catch> handles specific failures"
    (is-tc-e (-> (fx/fail> :not-found "missing")
                 (fx/catch> {:not-found (fx/map> (constantly 0))}))
             (fx/IEffect t/Any (t/U nil Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "catch> unchained version"
    (is-tc-e (fx/catch> {:not-found (fx/map> (constantly 0))})
             (fx/IEffect Long (t/U Long Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest catchall>-ann--test
  (testing "catchall> handles all failures"
    (is-tc-e (-> (fx/fail> :error "something went wrong")
                 (fx/catchall> (fx/succeed> 42)))
             (fx/IEffect t/Any (t/U nil Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "catchall> unchained version"
    (is-tc-e (fx/catchall> (fx/succeed> 42))
             (fx/IEffect Long (t/U Long Long) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest run-sync!-ann--test
  (testing "run-sync! evaluates effect and returns output"
    (is-tc-e (fx/run-sync! (fx/succeed> 10))
             Long
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "run-sync! with context map evaluates and returns output"
    (is-tc-e (fx/run-sync! (fx/succeed> 10) {:env :prod})
             Long
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "run-sync! with string effect"
    (is-tc-e (fx/run-sync! (fx/succeed> "hello"))
             String
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "run-sync! with failure returns failure or nil"
    (is-tc-e (fx/run-sync! (fx/fail> :not-found {}))
             (t/U nil (fx/IFailure (t/Val :not-found) (t/HMap :complete? true)))
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest context>-ann--test
  (testing "context> returns effect yielding context map or extracted key"
    (is-tc-e (fx/context>)
             (fx/IEffect t/Any com.lambdaseq.fx.typed/Context nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/context> :db)
             (fx/IEffect t/Any t/Any nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/context> :timeout 5000)
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest service>-ann--test
  (testing "service> returns effect yielding service or default"
    (is-tc-e (fx/service> :db)
             (fx/IEffect t/Any t/Any nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/service> :timeout 5000)
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest map-ctx>-ann--test
  (testing "map-ctx> infers output type from binary function"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map-ctx> (fn [v _ctx] (inc v))))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/map-ctx> (fn [v _ctx] (str v))))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest do-ctx>-ann--test
  (testing "do-ctx> preserves output type of previous effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/do-ctx> (fn [v _ctx] (println v))))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "hello")
                 (fx/do-ctx> (fn [v _ctx] (println v))))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest provide>-ann--test
  (testing "provide> executes target effect and returns its effect type"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/provide> {:multiplier 2}))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/provide> {:multiplier 2} (fx/succeed> "test"))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest provide-service>-ann--test
  (testing "provide-service> executes target effect with single service bound"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/provide-service> :db {:conn "postgres"}))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/provide-service> :db {:conn "postgres"} (fx/succeed> 42))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest acquire-release>-ann--test
  (testing "acquire-release> typing succeeds"
    (is-tc-e (fx/acquire-release>
               (fx/succeed> {:db "conn"})
               (fn [conn] (fx/succeed> "data"))
               (fn [conn] (fx/succeed> nil)))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest or-die>-ann--test
  (testing "or-die> strips failure type"
    (is-tc-e (-> (fx/succeed> 42)
                 (fx/or-die>))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest match>-ann--test
  (testing "match> converges return types"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/match>
                   (fn [_err] "error")
                   (fn [_val] "success")))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest or-else>-ann--test
  (testing "or-else> infers fallback type"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/or-else> (fx/succeed> 20)))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest for-each>-ann--test
  (testing "for-each> infers vector output type"
    (is-tc-e (fx/for-each> [1 2 3] (fn [x] (fx/succeed> (str x))))
             (fx/IEffect t/Any (t/Vec String) nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest zip>-ann--test
  (testing "zip-with> combines types"
    (is-tc-e (fx/zip-with> (fx/succeed> 10) (fx/succeed> "str") (fn [n s] (str s n)))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])))

(deftest run-async!-runtime-test
  (testing "run-async! executes asynchronously and yields value via CompletableFuture"
    (let [fut (fx/run-async! (-> (fx/succeed> 10)
                                 (fx/sleep> 10)
                                 (fx/map> inc)))]
      #?(:clj
         (is (= 11 (.get ^java.util.concurrent.CompletableFuture fut)))
         :cljs
         (is (some? fut))))))