(ns com.lambdaseq.fx.typed-test
  (:require [clojure.test :refer [deftest testing]]
            [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.core]
            [com.lambdaseq.fx.typed]
            [typed.clj.checker.test-utils :as tu :refer [tc-e is-tc-e is-tc-err]]
            [typed.clojure :as t]))

(deftest effect?-ann--test
  (testing "effect? returns boolean"
    (is-tc-e (effect? nil)
             Boolean
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest failure?-ann--test
  (testing "failure? returns boolean"
    (is-tc-e (failure? nil)
             Boolean
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest make-failure-ann--test
  (testing "make-failure returns a failure"
    (is-tc-e (make-failure :foo {})
             (IFailure (t/Val :foo) (t/HMap :complete? true))
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (make-failure :bar :error)
             (IFailure (t/Val :bar) t/Keyword)
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest make-effect-ann--test
  (testing "make-effect returns an effect"
    (is-tc-e (make-effect :foo nil (constantly 1))
             (IEffect t/Nothing Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (make-effect :foo nil (constantly "str"))
             (IEffect t/Nothing (t/Val "str") t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect"
    (is-tc-e (make-effect :foo
                          (make-effect :bar nil (constantly 1))
                          inc)
             (IEffect Long Long t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (make-effect :foo
                          (make-effect :bar nil (constantly "10"))
                          parse-long)
             (IEffect String (t/Option Long) t/Nothing '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]]))
  (testing "type-checks with a previous effect and a failure"
    (is-tc-e (make-effect :foo
                          (fail> :ok)
                          identity)
             (IEffect
               nil nil
               (IFailure (t/Val :fail) (t/Val :ok))
               '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (make-effect :foo
                          (fail> :bar)
                          some?)
             (IEffect
               nil Boolean
               (IFailure (t/Val :fail) (t/Val :bar))
               '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest succeed>-ann--test
  (testing "succeed> returns an effect with output type inferred"
    (is-tc-e (succeed> 10)
             (IEffect nil Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (succeed> "str")
             (IEffect nil String nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest fail>-ann--test
  (testing "fail> returns an effect with failure type and error inferred."
    (is-tc-e (fail> :foo)
             (IEffect
               t/Any nil
               (IFailure (t/Val :fail) (t/Val :foo))
               '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (fail> {})
             (IEffect
               t/Any nil
               (IFailure (t/Val :fail) (t/HMap :complete? true))
               '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest map>-ann--test
  (testing "map> returns an effect with output type inferred"
    (is-tc-e (map> inc)
             (IEffect Long Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (succeed> 10)
                 (map> inc))
             (IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (succeed> 10)
                 (map> str))
             (IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (map> str (succeed> "str"))
             (IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]]))
  (testing "chaining map> effects"
    (is-tc-e (-> (succeed> 10)
                 (map> inc)
                 (map> str))
             (IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (succeed> 10)
                 (map> inc)
                 (map> str)
                 (map> parse-long))
             (IEffect t/Any (t/Option Long) nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]]))
  (testing "map> infers and propagates failure type in its type"
    (is-tc-e (-> (fail> :foo)
                 (map> inc))
             (IEffect t/Any Long
                      (IFailure (t/Val :fail) (t/Val :foo))
                      '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fail> :foo)
                 (map> inc)
                 (map> str))
             (IEffect t/Any String
                      (IFailure (t/Val :fail) (t/Val :foo))
                      '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest do>-ann--test
  (testing "do> returns the output type of it's previous effect"
    (is-tc-e (-> (succeed> 10)
                 (do> println))
             (IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (succeed> "string")
                 (do> println))
             (IEffect t/Any String nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]]))
  (testing "do> propagates failure type of previous effects"
    (is-tc-e (-> (fail> {})
                 (do> println))
             (IEffect t/Any nil
                      (IFailure (t/Val :fail) (t/Val {}))
                      '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (fail> {})
                 (map> inc)
                 (do> println))
             (IEffect t/Any Long
                      (IFailure (t/Val :fail) (t/Val {}))
                      '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])))

(deftest mapcat>-ann--test
  (testing "mapcat> returns an effect with output type inferred"
    (is-tc-e (-> (succeed> 10)
                 (mapcat> (map> inc)))
             (IEffect t/Any Long nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    (is-tc-e (-> (succeed> "10")
                 (mapcat> (map> parse-long)))
             (IEffect t/Any (t/Option Long) nil '{})
             :requires [[com.lambdaseq.fx.core :refer :all]
                        [com.lambdaseq.fx.typed]])
    ; TODO: Expecting this to fail but doesn't
    (is-tc-err ^::t/dbg (-> (succeed> "10")
                            (mapcat> ^::t/dbg (map> inc)))
               :requires [[com.lambdaseq.fx.core :refer :all]
                          [com.lambdaseq.fx.typed]])))