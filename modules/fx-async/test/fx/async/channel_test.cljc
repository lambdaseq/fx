(ns fx.async.channel-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]))

(deftest channel-put-take-test
  (testing "chan-put> and chan-take> transfer values safely"
    (let [ch (fx/run-sync! (fxa/chan> 2))]
      (fx/run-sync! (fxa/chan-put> ch :item-1))
      (fx/run-sync! (fxa/chan-put> ch :item-2))
      (is (= :item-1 (fx/run-sync! (fxa/chan-take> ch))))
      (is (= :item-2 (fx/run-sync! (fxa/chan-take> ch))))
      (fx/run-sync! (fxa/chan-close> ch)))))

(deftest channel-timeout-test
  (testing "chan-take> with timeout returns timeout-val when channel is empty"
    (let [ch (fx/run-sync! (fxa/chan>))]
      (is (= :timed-out (fx/run-sync! (fxa/chan-take> ch {:timeout-ms 50 :timeout-val :timed-out}))))
      (fx/run-sync! (fxa/chan-close> ch)))))

(deftest channel-drain-test
  (testing "chan-drain> drains all elements until channel closes"
    (let [ch (a/chan 5)]
      (a/>!! ch 10)
      (a/>!! ch 20)
      (a/>!! ch 30)
      (a/close! ch)
      (let [res (fx/run-sync! (fxa/chan-drain> ch))]
        (is (= [10 20 30] res))))))

(deftest channel-pipe-test
  (testing "chan-pipe> forwards values across channels"
    (let [ch1 (a/chan 2)
          ch2 (a/chan 2)]
      (fx/run-sync! (fxa/chan-pipe> ch1 ch2))
      (a/>!! ch1 :piped-val)
      (a/close! ch1)
      (is (= :piped-val (a/<!! ch2))))))

(deftest channel-alts-test
  (testing "chan-alts> selects the ready channel port"
    (let [c1 (a/chan 1)
          c2 (a/chan 1)]
      (a/>!! c2 :from-c2)
      (let [[val port] (fx/run-sync! (fxa/chan-alts> [c1 c2]))]
        (is (= :from-c2 val))
        (is (identical? c2 port))))))

(deftest channel-pub-sub-test
  (testing "chan-pub> and chan-sub> broadcast messages to topic subscribers"
    (let [in-ch  (a/chan 10)
          pub    (fx/run-sync! (fxa/chan-pub> in-ch :type))
          out-ch (a/chan 10)]
      (fx/run-sync! (fxa/chan-sub> pub :alert out-ch))
      (a/>!! in-ch {:type :info :msg "ignore"})
      (a/>!! in-ch {:type :alert :msg "critical"})
      (let [received (a/<!! out-ch)]
        (is (= {:type :alert :msg "critical"} received))))))
