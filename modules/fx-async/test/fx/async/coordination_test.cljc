(ns fx.async.coordination-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]))

(deftest queue-test
  (testing "bounded queue buffers items and tracks size"
    (let [q (fx/run-sync! (fxa/queue-bounded> 2))]
      (is (= true (fx/run-sync! (fxa/queue-offer> q 100))))
      (is (= true (fx/run-sync! (fxa/queue-offer> q 200))))
      (is (= 2 (fx/run-sync! (fxa/queue-size> q))))
      (is (= 100 (fx/run-sync! (fxa/queue-take> q))))
      (is (= 200 (fx/run-sync! (fxa/queue-poll> q))))
      (is (nil? (fx/run-sync! (fxa/queue-poll> q))))
      (fx/run-sync! (fxa/queue-shutdown> q))))

  (testing "sliding queue drops oldest items when capacity is reached"
    (let [q (fx/run-sync! (fxa/queue-sliding> 2))]
      (fx/run-sync! (fxa/queue-offer> q :a))
      (fx/run-sync! (fxa/queue-offer> q :b))
      (fx/run-sync! (fxa/queue-offer> q :c))
      (is (= :b (fx/run-sync! (fxa/queue-take> q))))
      (is (= :c (fx/run-sync! (fxa/queue-take> q))))
      (fx/run-sync! (fxa/queue-shutdown> q)))))

(deftest hub-test
  (testing "hub broadcasts items to all active subscribers"
    (let [h    (fx/run-sync! (fxa/hub-bounded> 5))
          sub1 (fx/run-sync! (fxa/hub-subscribe> h))
          sub2 (fx/run-sync! (fxa/hub-subscribe> h))]
      (is (= 2 (fx/run-sync! (fxa/hub-subscriber-count> h))))
      (fx/run-sync! (fxa/hub-publish> h :event-1))
      (is (= :event-1 (fx/run-sync! (fxa/queue-take> sub1))))
      (is (= :event-1 (fx/run-sync! (fxa/queue-take> sub2))))
      (fx/run-sync! (fxa/hub-unsubscribe> h sub1))
      (is (= 1 (fx/run-sync! (fxa/hub-subscriber-count> h))))
      (fx/run-sync! (fxa/hub-shutdown> h)))))

(deftest deferred-test
  (testing "deferred resolves to value once written"
    (let [d (fx/run-sync! (fxa/deferred>))]
      (fxa/run-fiber! (fx/chain> (fx/sleep> 20) (fxa/deferred-succeed> d :done)))
      (is (= :done (fx/run-sync! (fxa/deferred-await> d))))))

  (testing "deferred propagates failure"
    (let [d (fx/run-sync! (fxa/deferred>))]
      (fxa/run-fiber! (fx/chain> (fx/sleep> 20) (fxa/deferred-fail> d (fx/make-failure :boom {:code 500}))))
      (let [res (fx/run-sync! (fxa/deferred-await> d))]
        (is (fx/failure? res))
        (is (= :boom (:tag res)))))))

(deftest semaphore-test
  (testing "semaphore with-permit protects critical section"
    (let [sem (fx/run-sync! (fxa/semaphore> 2))]
      (is (= 2 (fx/run-sync! (fxa/semaphore-available-permits> sem))))
      (let [res (fx/run-sync! (fxa/with-permit> sem (fx/succeed> :protected)))]
        (is (= :protected res))
        (is (= 2 (fx/run-sync! (fxa/semaphore-available-permits> sem))))))))

(deftest latch-test
  (testing "countdown latch awaits multiple countdowns"
    (let [latch (fx/run-sync! (fxa/countdown-latch> 2))]
      (is (= 2 (fx/run-sync! (fxa/latch-get-count> latch))))
      (fxa/run-fiber! (fx/chain> (fx/sleep> 10) (fxa/latch-count-down> latch)))
      (fxa/run-fiber! (fx/chain> (fx/sleep> 20) (fxa/latch-count-down> latch)))
      (is (= true (fx/run-sync! (fxa/latch-await> latch 1000)))))))

(deftest ref-test
  (testing "ref provides atomic state updates"
    (let [r (fx/run-sync! (fxa/ref> 0))]
      (is (= 0 (fx/run-sync! (fxa/ref-get> r))))
      (is (= 1 (fx/run-sync! (fxa/ref-update> r inc))))
      (is (= 10 (fx/run-sync! (fxa/ref-set> r 10))))
      (is (= 10 (fx/run-sync! (fxa/ref-get> r)))))))
