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
    ; TODO: Expecting this to fail but doesn't
    (is-tc-err (-> (fx/succeed> "10")
                   (fx/mapcat> (fx/map> inc)))
               :requires [[com.lambdaseq.fx.core :as fx]
                          [com.lambdaseq.fx.typed]])))