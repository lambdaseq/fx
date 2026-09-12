(ns fx.layer-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]))

(deftest from-value>-test
  (testing "from-value> provides static service to effect"
    (let [l (fx-layer/from-value> :greeting "Hello")
          res (-> (fx/service> :greeting)
                  (fx-layer/provide-layer> l)
                  (fx/run-sync!))]
      (is (= "Hello" res)))))

(deftest from-values>-test
  (testing "from-values> provides map of services to effect"
    (let [l (fx-layer/from-values> {:a 1 :b 2})
          res (-> (fx/context>)
                  (fx-layer/provide-layer> l)
                  (fx/run-sync!))]
      (is (= 1 (:a res)))
      (is (= 2 (:b res))))))

(deftest from-effect>-test
  (testing "from-effect> evaluates effect to provide service"
    (let [l (fx-layer/from-effect> :computed (fx/succeed> 42))
          res (-> (fx/service> :computed)
                  (fx-layer/provide-layer> l)
                  (fx/run-sync!))]
      (is (= 42 res)))))

(deftest make>-lifecycle-test
  (testing "make> acquires resource, provides it, and releases upon normal completion"
    (let [events (atom [])
          l (fx-layer/make> :db
                            (fx/try> (fn []
                                       (swap! events conj :acquired)
                                       {:conn-id 1})
                                     :acquire-error)
                            (fn [db]
                              (fx/try> (fn []
                                         (swap! events conj [:released (:conn-id db)]))
                                       :release-error)))
          res (-> (fx/service> :db)
                  (fx/map> (fn [db]
                             (swap! events conj [:used (:conn-id db)])
                             (:conn-id db)))
                  (fx-layer/provide-layer> l)
                  (fx/run-sync!))]
      (is (= 1 res))
      (is (= [:acquired [:used 1] [:released 1]] @events))))

  (testing "make> releases resource when body effect fails"
    (let [events (atom [])
          l (fx-layer/make> :db
                            (fx/succeed> {:conn-id 1})
                            (fn [db]
                              (fx/try> (fn []
                                         (swap! events conj [:released (:conn-id db)]))
                                       :release-error)))
          res (-> (fx/fail> :business-error "something went wrong")
                  (fx-layer/provide-layer> l)
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :business-error (:tag res)))
      (is (= [[:released 1]] @events))))

  (testing "make> releases resource when body effect throws exception"
    (let [events (atom [])
          l (fx-layer/make> :res
                            (fx/succeed> "resource")
                            (fn [r]
                              (fx/try> (fn [] (swap! events conj [:released r])) :release-error)))]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (fx/run-sync!
                    (fx-layer/provide-layer>
                     (-> (fx/succeed> 1)
                         (fx/map> (fn [_]
                                    (throw #?(:clj (RuntimeException. "boom!")
                                              :cljs (js/Error. "boom!"))))))
                     l))))
      (is (= [[:released "resource"]] @events)))))

(deftest merge>-test
  (testing "merge> combines independent layers and releases them in reverse order"
    (let [events (atom [])
          l1 (fx-layer/make> :s1
                             (fx/try> (fn [] (swap! events conj :acq-s1) 1) :err)
                             (fn [_] (fx/try> (fn [] (swap! events conj :rel-s1)) :err)))
          l2 (fx-layer/make> :s2
                             (fx/try> (fn [] (swap! events conj :acq-s2) 2) :err)
                             (fn [_] (fx/try> (fn [] (swap! events conj :rel-s2)) :err)))
          merged (fx-layer/merge> l1 l2)
          res (-> (fx/context>)
                  (fx-layer/provide-layer> merged)
                  (fx/run-sync!))]
      (is (= 1 (:s1 res)))
      (is (= 2 (:s2 res)))
      (is (= [:acq-s1 :acq-s2 :rel-s2 :rel-s1] @events)))))

(deftest compose>-test
  (testing "compose> feeds parent layer services into child layer acquisition"
    (let [events (atom [])
          parent (fx-layer/make> :config
                                 (fx/try> (fn [] (swap! events conj :acq-config) {:port 8080}) :err)
                                 (fn [_] (fx/try> (fn [] (swap! events conj :rel-config)) :err)))
          child (fx-layer/make> :server
                                (-> (fx/service> :config)
                                    (fx/mapcat> (fn [{:keys [port]}]
                                                  (fx/try> (fn []
                                                             (swap! events conj [:acq-server-on port])
                                                             {:bound-port port})
                                                           :err))))
                                (fn [{:keys [bound-port]}]
                                  (fx/try> (fn [] (swap! events conj [:rel-server-on bound-port])) :err)))
          composed (fx-layer/compose> parent child)
          res (-> (fx/context>)
                  (fx-layer/provide-layer> composed)
                  (fx/run-sync!))]
      (is (= {:port 8080} (:config res)))
      (is (= {:bound-port 8080} (:server res)))
      (is (= [:acq-config [:acq-server-on 8080] [:rel-server-on 8080] :rel-config] @events))))

  (testing "compose> cleans up already-acquired parent when child acquisition fails"
    (let [events (atom [])
          parent (fx-layer/make> :db
                                 (fx/try> (fn [] (swap! events conj :acq-db) "db-conn") :err)
                                 (fn [_] (fx/try> (fn [] (swap! events conj :rel-db)) :err)))
          child (fx-layer/make> :server
                                (fx/fail> :server/bind-failed "port in use")
                                (fn [_] (fx/try> (fn [] (swap! events conj :rel-server)) :err)))
          composed (fx-layer/compose> parent child)
          res (-> (fx/service> :server)
                  (fx-layer/provide-layer> composed)
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :server/bind-failed (:tag res)))
      (is (= [:acq-db :rel-db] @events)))))

(deftest with-layer>-test
  (testing "with-layer> executes user function in layer scope and tears down"
    (let [events (atom [])
          l (fx-layer/make> :service
                            (fx/try> (fn [] (swap! events conj :acq) "svc") :err)
                            (fn [_] (fx/try> (fn [] (swap! events conj :rel)) :err)))
          res (fx/run-sync!
               (fx-layer/with-layer> l
                 (fn [ctx]
                   (fx/succeed> (str "used-" (:service ctx))))))]
      (is (= "used-svc" res))
      (is (= [:acq :rel] @events)))))

(deftest start-layer!-and-stop-layer!-test
  (testing "start-layer! returns Closeable System and stop-layer! releases in reverse order"
    (let [events (atom [])
          l1 (fx-layer/make> :db
                             (fx/try> (fn [] (swap! events conj :acq-db) {:pool 1}) :err)
                             (fn [_] (fx/try> (fn [] (swap! events conj :rel-db)) :err)))
          l2 (fx-layer/make> :cache
                             (fx/try> (fn [] (swap! events conj :acq-cache) {:redis 2}) :err)
                             (fn [_] (fx/try> (fn [] (swap! events conj :rel-cache)) :err)))
          sys (fx-layer/start-layer! (fx-layer/merge> l1 l2))]
      (is (some? sys))
      (is (= {:pool 1} (:db sys)))
      (is (= {:redis 2} (:cache sys)))
      (is (= {:pool 1} (get @sys :db)))
      (is (= [:acq-db :acq-cache] @events))
      (fx-layer/stop-layer! sys)
      (is (= [:acq-db :acq-cache :rel-cache :rel-db] @events)))))
