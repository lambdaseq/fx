(ns com.lambdaseq.fx.typed-test
  (:require [clojure.test :refer [deftest testing]]
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
    (is-tc-e (fx/make-effect :foo nil (constantly 1))
             (fx/IEffect t/Nothing Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo nil (constantly "str"))
             (fx/IEffect t/Nothing (t/Val "str") t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect"
    (is-tc-e (fx/make-effect :foo
               (fx/make-effect :bar nil (constantly 1))
               inc)
             (fx/IEffect Long Long t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo
               (fx/make-effect :bar nil (constantly "10"))
               parse-long)
             (fx/IEffect String (t/Option Long) t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect and a failure"
    (is-tc-e (fx/make-effect :foo
               (fx/fail> :ok)
               identity)
             (fx/IEffect
               nil nil
               (fx/IFailure (t/Val :fail) (t/Val :ok))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fx/make-effect :foo
               (fx/fail> :bar)
               some?)
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

(deftest do>-ann--test
  (testing "do> returns the output type of it's previous effect"
    (is-tc-e (-> (fx/succeed> 10)
                 (fx/do> println))
             (fx/IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/succeed> "string")
                 (fx/do> println))
             (fx/IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]]))
  (testing "do> propagates failure type of previous effects"
    (is-tc-e (-> (fx/fail> {})
                 (fx/do> println))
             (fx/IEffect t/Any nil
               (fx/IFailure (t/Val :fail) (t/Val {}))
               '{})
             :requires [[com.lambdaseq.fx.core :as fx]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fx/fail> {})
                 (fx/map> inc)
                 (fx/do> println))
             (fx/IEffect t/Any Long
               (fx/IFailure (t/Val :fail) (t/Val {}))
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
             (t/HMap :mandatory {:type (t/Val :test)
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