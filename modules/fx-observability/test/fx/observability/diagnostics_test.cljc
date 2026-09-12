(ns fx.observability.diagnostics-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.observability.diagnostics :as diag]))

(deftest cause-algebra-test
  (testing "creates and identifies cause records"
    (let [f (fx/make-failure :not-found {:id 10})
          c-fail (diag/cause-fail f)
          ex (ex-info "Null pointer" {})
          c-die (diag/cause-die ex)
          c-int (diag/cause-interrupt :timeout)
          c-seq (diag/cause-sequential c-fail c-die)
          c-par (diag/cause-parallel [c-fail c-int])]
      (is (diag/cause? c-fail))
      (is (diag/fail-cause? c-fail))
      (is (= :fail (diag/cause-type c-fail)))

      (is (diag/cause? c-die))
      (is (diag/die-cause? c-die))

      (is (diag/cause? c-int))
      (is (diag/interrupt-cause? c-int))

      (is (diag/cause? c-seq))
      (is (diag/sequential-cause? c-seq))

      (is (diag/cause? c-par))
      (is (diag/parallel-cause? c-par))

      (is (= [f] (diag/failures c-seq)))
      (is (= [ex] (diag/defects c-seq)))
      (is (= [f] (diag/failures c-par))))))

(deftest sandbox-combinator-test
  (testing "sandboxes domain failure into Fail on success channel"
    (let [res (-> (fx/fail> :not-found {:user-id 42})
                  (diag/sandbox>)
                  (fx/run-sync!))]
      (is (diag/fail-cause? res))
      (is (= :not-found (fx/tag (:failure res))))
      (is (= {:user-id 42} (fx/error-data (:failure res))))))

  (testing "sandboxes unhandled Throwable defect into Die on success channel"
    (let [res (-> (fx/map> (fn [_] (throw (ex-info "Database dead" {:code 500}))))
                  (diag/sandbox>)
                  (fx/run-sync!))]
      (is (diag/die-cause? res))
      (is (= "Database dead" (ex-message (:defect res))))))

  (testing "passes through successful values without modification"
    (let [res (-> (fx/succeed> "hello")
                  (diag/sandbox>)
                  (fx/run-sync!))]
      (is (= "hello" res)))))

(deftest unsandbox-combinator-test
  (testing "unsandbox re-emits Failure"
    (let [cause (diag/cause-fail (fx/make-failure :bad-input {:field "email"}))
          res (-> (fx/succeed> cause)
                  (diag/unsandbox>)
                  (fx/run-sync!))]
      (is (fx/failure? res))
      (is (= :bad-input (fx/tag res)))))

  (testing "unsandbox re-throws Die defect"
    (let [cause (diag/cause-die (ex-info "Fatal" {:fatal true}))]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (-> (fx/succeed> cause)
                       (diag/unsandbox>)
                       (fx/run-sync!)))))))

(deftest render-cause-test
  (testing "renders Fail cause"
    (let [out (diag/render-cause (diag/cause-fail (fx/make-failure :auth/denied {:role :guest})))]
      (is (str/includes? out "Cause.Fail: [:auth/denied] {:role :guest}"))))

  (testing "renders composite Parallel cause tree"
    (let [c1 (diag/cause-fail (fx/make-failure :t1 {:k 1}))
          c2 (diag/cause-fail (fx/make-failure :t2 {:k 2}))
          tree (diag/cause-parallel [c1 c2])
          out (diag/render-cause tree)]
      (is (str/includes? out "Cause.Parallel:"))
      (is (str/includes? out "Cause.Fail: [:t1] {:k 1}"))
      (is (str/includes? out "Cause.Fail: [:t2] {:k 2}")))))

(deftest render-execution-trace-test
  (testing "renders AST execution trace with step numbers and failure node"
    (let [pipeline (-> (fx/succeed> {:user-id 42})
                       (fx/map> (fn [u] (assoc u :active true)))
                       (fx/mapcat> (fn [_] (fx/fail> :db/not-found {:table "users"}))))
          failure (fx/run-sync! pipeline)
          trace-str (diag/render-execution-trace pipeline failure)]
      (is (str/includes? trace-str "[fx AST Execution Trace]"))
      (is (str/includes? trace-str "1. (succeed>"))
      (is (str/includes? trace-str "2. (map>"))
      (is (str/includes? trace-str "3. (mapcat>"))
      (is (str/includes? trace-str "└── FAILED: [:db/not-found] {:table \"users\"}")))))
