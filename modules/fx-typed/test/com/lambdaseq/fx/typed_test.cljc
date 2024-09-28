(ns com.lambdaseq.fx.typed-test
  (:require [clojure.test :refer [deftest testing]]
            [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.core]
            [com.lambdaseq.fx.typed]
            [typed.clj.checker.test-utils :refer [is-tc-e]]
            [typed.clojure :as t]))
fx/IEffect

(deftest effect?-ann--test
  (testing "effect? returns boolean"
    (is-tc-e (com.lambdaseq.fx.core/effect? nil)
             Boolean)))

(deftest failure?-ann--test
  (testing "failure? returns boolean"
    (is-tc-e (com.lambdaseq.fx.core/failure? nil)
             Boolean)))

(deftest make-failure-ann--test
  (testing "make-failure returns a failure"
    (is-tc-e (com.lambdaseq.fx.core/make-failure :foo {})
             (com.lambdaseq.fx.core/IFailure (t/Val :foo) (t/HMap :complete? true)))
    (is-tc-e (com.lambdaseq.fx.core/make-failure :bar :error)
             (com.lambdaseq.fx.core/IFailure (t/Val :bar) t/Keyword))))

(deftest make-effect-ann--test
  (testing "make-effect returns an effect"
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo nil (constantly 1))
             (com.lambdaseq.fx.core/IEffect t/Nothing Long nil '{}))
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo nil (constantly "str"))
             (com.lambdaseq.fx.core/IEffect t/Nothing (t/Val "str") t/Nothing '{})))
  (testing "type-checks with a previous effect"
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo
               (com.lambdaseq.fx.core/make-effect :bar nil (constantly 1))
               inc)
             (com.lambdaseq.fx.core/IEffect Long Long t/Nothing '{}))
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo
               (com.lambdaseq.fx.core/make-effect :bar nil (constantly "10"))
               parse-long)
             (com.lambdaseq.fx.core/IEffect String (t/Option Long) t/Nothing '{})))
  (testing "type-checks with a previous effect and a failure"
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo
               (com.lambdaseq.fx.core/fail> :ok)
               identity)
             (com.lambdaseq.fx.core/IEffect
               nil nil
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val :ok))
               '{}))
    (is-tc-e (com.lambdaseq.fx.core/make-effect :foo
               (com.lambdaseq.fx.core/fail> :bar)
               some?)
             (com.lambdaseq.fx.core/IEffect
               nil Boolean
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val :bar))
               '{}))))

(deftest succeed>-ann--test
  (testing "succeed> returns an effect with output type inferred"
    (is-tc-e (com.lambdaseq.fx.core/succeed> 10)
             (com.lambdaseq.fx.core/IEffect nil Long nil '{}))
    (is-tc-e (com.lambdaseq.fx.core/succeed> "str")
             (com.lambdaseq.fx.core/IEffect nil String nil '{}))))

(deftest fail>-ann--test
  (testing "fail> returns an effect with failure type and error inferred."
    (is-tc-e (com.lambdaseq.fx.core/fail> :foo)
             (com.lambdaseq.fx.core/IEffect
               t/Any nil
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val :foo))
               '{}))
    (is-tc-e (com.lambdaseq.fx.core/fail> {})
             (com.lambdaseq.fx.core/IEffect
               t/Any nil
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/HMap :complete? true))
               '{}))))

(deftest map>-ann--test
  (testing "map> returns an effect with output type inferred"
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/map> inc))
             (com.lambdaseq.fx.core/IEffect t/Any Long nil '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/map> str))
             (com.lambdaseq.fx.core/IEffect t/Any String nil '{}))
    (is-tc-e (com.lambdaseq.fx.core/map> str (com.lambdaseq.fx.core/succeed> "str"))
             (com.lambdaseq.fx.core/IEffect t/Any String nil '{})))
  (testing "chaining map> effects"
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/map> inc)
                  (com.lambdaseq.fx.core/map> str))
             (com.lambdaseq.fx.core/IEffect t/Any String nil '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/map> inc)
                  (com.lambdaseq.fx.core/map> str)
                  (com.lambdaseq.fx.core/map> parse-long))
             (com.lambdaseq.fx.core/IEffect t/Any (t/Option Long) nil '{})))
  (testing "map> infers and propagates failure type in its type"
    (is-tc-e (->> (com.lambdaseq.fx.core/fail> :foo)
                  (com.lambdaseq.fx.core/map> inc))
             (com.lambdaseq.fx.core/IEffect t/Any Long
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val :foo))
               '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/fail> :foo)
                  (com.lambdaseq.fx.core/map> inc)
                  (com.lambdaseq.fx.core/map> str))
             (com.lambdaseq.fx.core/IEffect t/Any String
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val :foo))
               '{}))))

(deftest input>-ann-test
  (testing "input> returns an effect with output type inferred.
  Context has an input key with the type expected by the next effect"
    (is-tc-e (->> (com.lambdaseq.fx.core/input>)
                  (com.lambdaseq.fx.core/map> inc))
             (com.lambdaseq.fx.core/IEffect t/Any Long nil (t/HMap :mandatory {:input Long})))))

(deftest do>-ann--test
  (testing "do> returns the output type of it's previous effect"
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/do> println))
             (com.lambdaseq.fx.core/IEffect t/Any Long nil '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> "string")
                  (com.lambdaseq.fx.core/do> println))
             (com.lambdaseq.fx.core/IEffect t/Any String nil '{})))
  (testing "do> propagates failure type of previous effects"
    (is-tc-e (->> (com.lambdaseq.fx.core/fail> {})
                  (com.lambdaseq.fx.core/do> println))
             (com.lambdaseq.fx.core/IEffect t/Any nil
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val {}))
               '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/fail> {})
                  (com.lambdaseq.fx.core/map> inc)
                  (com.lambdaseq.fx.core/do> println))
             (com.lambdaseq.fx.core/IEffect t/Any Long
               (com.lambdaseq.fx.core/IFailure (t/Val :fail) (t/Val {}))
               '{}))))

(deftest mapcat>-ann--test
  (testing "mapcat> returns an effect with output type inferred"
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> 10)
                  (com.lambdaseq.fx.core/mapcat> (->> (com.lambdaseq.fx.core/input>)
                                                      (com.lambdaseq.fx.core/map> inc)
                                                      (com.lambdaseq.fx.core/map> str))))
             (com.lambdaseq.fx.core/IEffect t/Any String nil '{}))
    (is-tc-e (->> (com.lambdaseq.fx.core/succeed> "10")
                  (com.lambdaseq.fx.core/mapcat> (->> (com.lambdaseq.fx.core/input>)
                                                      (com.lambdaseq.fx.core/map> parse-long))))
             (com.lambdaseq.fx.core/IEffect t/Any (t/Option Long) nil '{}))))