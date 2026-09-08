(ns fx.observability.trace-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.trace :as trace]))

(deftest w3c-traceparent-codec-test
  (testing "parse and format valid W3C traceparent"
    (let [tp "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
          parsed (trace/parse-traceparent tp)]
      (is (= {:version        "00"
              :trace-id       "4bf92f3577b34da6a3ce929d0e0e4736"
              :parent-span-id "00f067aa0ba902b7"
              :sampled?       true
              :trace-flags    "01"}
             parsed))
      (is (= tp (trace/format-traceparent (:trace-id parsed) (:parent-span-id parsed) true)))))

  (testing "handles invalid/malformed traceparent gracefully"
    (is (nil? (trace/parse-traceparent nil)))
    (is (nil? (trace/parse-traceparent "")))
    (is (nil? (trace/parse-traceparent "invalid-header")))
    (is (nil? (trace/parse-traceparent "00-00000000000000000000000000000000-00f067aa0ba902b7-01")))
    (is (nil? (trace/parse-traceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01")))))

(deftest extract-and-inject-trace-context-test
  (testing "extracts trace context from HTTP carrier map"
    (let [req {:headers {"traceparent" "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"}}
          ctx (trace/extract-trace-context req)]
      (is (= "4bf92f3577b34da6a3ce929d0e0e4736" (:trace-id ctx)))
      (is (= "00f067aa0ba902b7" (:parent-span-id ctx)))))

  (testing "injects trace context into carrier map"
    (let [carrier {}
          ctx {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736" :span-id "00f067aa0ba902b7"}
          injected (trace/inject-trace-context carrier ctx)]
      (is (= "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
             (get injected "traceparent"))))))

(deftest with-span-lifecycle-test
  (testing "with-span> records execution duration, span IDs, and passes value"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          res (-> (trace/with-span-reporter> reporter
                    (trace/with-span> "calculate-total" {:currency "USD"}
                      (-> (fx/succeed> 100)
                          (fx/map> #(* % 2)))))
                  (fx/run-sync!))]
      (is (= 200 res))
      (is (= 1 (count @spans)))
      (let [span (first @spans)]
        (is (= "calculate-total" (:name span)))
        (is (= :success (:status span)))
        (is (some? (:trace-id span)))
        (is (some? (:span-id span)))
        (is (nil? (:parent-id span)))
        (is (number? (:duration-ms span)))
        (is (<= 0.0 (:duration-ms span)))
        (is (= {:currency "USD"} (:attributes span)))))))

(deftest nested-spans-hierarchy-test
  (testing "nested spans share trace-id and link parent-id to enclosing span-id"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          _ (-> (trace/with-span-reporter> reporter
                  (trace/with-span> "parent-op"
                    (-> (fx/succeed> 10)
                        (fx/mapcat> (fn [v]
                                      (trace/with-span> "child-op"
                                        (fx/succeed> (* v 2))))))))
                (fx/run-sync!))]
      (is (= 2 (count @spans)))
      (let [child (first @spans)
            parent (second @spans)]
        (is (= "child-op" (:name child)))
        (is (= "parent-op" (:name parent)))
        (is (= (:trace-id parent) (:trace-id child)))
        (is (= (:span-id parent) (:parent-id child)))))))

(deftest with-span-attributes-test
  (testing "dynamically adds attributes to the active span"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          _ (-> (trace/with-span-reporter> reporter
                  (trace/with-span> "db.fetch" {:table "users"}
                    (-> (fx/succeed> {:id 42 :role :admin})
                        (trace/with-span-attributes> {:rows-affected 1 :cached false}))))
                (fx/run-sync!))]
      (is (= 1 (count @spans)))
      (let [span (first @spans)]
        (is (= {:table "users" :rows-affected 1 :cached false} (:attributes span)))))))

(deftest span-failure-status-test
  (testing "records :failure status and error-data when effect yields Failure"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          failure (-> (trace/with-span-reporter> reporter
                        (trace/with-span> "failing-op"
                          (fx/fail> :not-found {:entity-id 999})))
                      (fx/run-sync!))]
      (is (fx/failure? failure))
      (is (= 1 (count @spans)))
      (let [span (first @spans)]
        (is (= "failing-op" (:name span)))
        (is (= :failure (:status span)))
        (is (= {:tag :not-found :error-data {:entity-id 999}} (:error span))))))

  (testing "records :failure status on thrown exception during unwinding"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))]
      (try
        (-> (trace/with-span-reporter> reporter
              (trace/with-span> "crashing-op"
                (fx/map> (fn [_] (throw (ex-info "Crash" {:code 500}))))))
            (fx/run-sync!))
        (catch #?(:clj Exception :cljs :default) _ nil))
      (is (= 1 (count @spans)))
      (let [span (first @spans)]
        (is (= "crashing-op" (:name span)))
        (is (= :failure (:status span)))
        (is (some? (:error span)))))))

(deftest async-trace-spans-test
  (testing "handles spans across asynchronous boundaries"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          fut (-> (trace/with-span-reporter> reporter
                    (trace/with-span> "async-root"
                      (-> (fx/succeed> 5)
                          (fx/map> inc))))
                  (fx/run-async!))
          res #?(:clj @fut :cljs nil)]
      (is (= 6 res))
      (is (= 1 (count @spans)))
      (is (= "async-root" (:name (first @spans)))))))

(deftest span-reporter-layer-test
  (testing "span-reporter-layer provides reporter through layer context"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))]
      (-> (fx-layer/provide-layer>
            (trace/with-span> "layer-span"
              (fx/succeed> :ok))
            (trace/span-reporter-layer> reporter))
          (fx/run-sync!))
      (is (= 1 (count @spans)))
      (is (= "layer-span" (:name (first @spans)))))))

(deftest with-trace-context-test
  (testing "with-trace-context> propagates trace-id and parent-span-id to child spans"
    (let [spans (atom [])
          reporter (fn [s] (swap! spans conj s))
          trace-ctx {:trace-id "trace-12345" :parent-span-id "span-99999"}
          _ (-> (trace/with-span-reporter> reporter
                  (trace/with-trace-context> trace-ctx
                    (trace/with-span> "scoped-span"
                      (fx/succeed> :done))))
                (fx/run-sync!))]
      (is (= 1 (count @spans)))
      (let [span (first @spans)]
        (is (= "scoped-span" (:name span)))
        (is (= "trace-12345" (:trace-id span)))
        (is (= "span-99999" (:parent-id span)))))))
