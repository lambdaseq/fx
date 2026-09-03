(ns com.lambdaseq.fx.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.utils :as fxu]))

(deftest linear-traversal-and-inspection-test
  (testing "linear pipeline inspection"
    (let [pipeline (-> (fx/service> :db)
                       (fx/map> (fn [db] (str "connected to " db)))
                       (fx/tap> (fn [s] (println s)))
                       (fx/map> clojure.string/upper-case))]
      (is (= 4 (fxu/chain-length pipeline)))
      (is (= [:context :map :tap :map] (fxu/effect-tags pipeline)))
      (is (= :context (fx/tag (fxu/root-effect pipeline))))
      (is (= :map (fx/tag pipeline)))
      (is (= 4 (count (fxu/effect-seq pipeline))))
      (is (= 4 (count (fxu/effect-seq-reverse pipeline))))
      (is (= [:context :map :tap :map]
             (mapv fx/tag (fxu/effect-seq pipeline))))
      (is (= [:map :tap :map :context]
             (mapv fx/tag (fxu/effect-seq-reverse pipeline))))))

  (testing "single effect inspection"
    (let [single (fx/succeed> 42)]
      (is (= 1 (fxu/chain-length single)))
      (is (= [:succeed] (fxu/effect-tags single)))
      (is (= :succeed (fx/tag (fxu/root-effect single))))
      (is (= [single] (fxu/effect-seq single)))))

  (testing "empty / nil safety"
    (is (= 0 (fxu/chain-length nil)))
    (is (= [] (fxu/effect-seq nil)))
    (is (nil? (fxu/root-effect nil)))
    (is (= [] (fxu/effect-tags nil)))))

(deftest query-and-filtering-test
  (let [pipeline (-> (fx/succeed> 10)
                     (fx/map> inc)
                     (fx/map-ctx> (fn [v ctx] (* v (:mult ctx 1))))
                     (fx/map> dec))]
    (testing "find-first-effect"
      (is (= :succeed (fx/tag (fxu/find-first-effect pipeline (fn [e] (= (fx/tag e) :succeed))))))
      (is (= :map-ctx (fx/tag (fxu/find-first-effect pipeline (fn [e] (= (fx/tag e) :map-ctx))))))
      (is (nil? (fxu/find-first-effect pipeline (fn [e] (= (fx/tag e) :retry))))))

    (testing "find-first-by-tag"
      (is (= :succeed (fx/tag (fxu/find-first-by-tag pipeline :succeed))))
      (is (= :map-ctx (fx/tag (fxu/find-first-by-tag pipeline :map-ctx))))
      (is (nil? (fxu/find-first-by-tag pipeline :retry))))

    (testing "find-all-effects & find-all-by-tag"
      (is (= 2 (count (fxu/find-all-by-tag pipeline :map))))
      (is (= [:map :map] (mapv fx/tag (fxu/find-all-by-tag pipeline :map))))
      (is (= 1 (count (fxu/find-all-by-tag pipeline :map-ctx))))
      (is (= [] (fxu/find-all-by-tag pipeline :retry))))))

(deftest reconstruction-and-mapping-test
  (testing "rechain-effects"
    (let [e1 (fx/succeed> 10)
          e2 (fx/map> inc)
          e3 (fx/map> (fn [x] (* x 2)))
          rechained (fxu/rechain-effects [e1 e2 e3])]
      (is (= [:succeed :map :map] (fxu/effect-tags rechained)))
      (is (= 22 (fx/run-sync! rechained)))))

  (testing "map-effects"
    (let [pipeline (-> (fx/succeed> 10)
                       (fx/map> inc)
                       (fx/map> inc))
          ;; Replace all inc maps with dec
          updated (fxu/map-effects pipeline
                                   (fn [eff]
                                     (if (= (fx/tag eff) :map)
                                       (fx/map> dec)
                                       eff)))]
      (is (= [:succeed :map :map] (fxu/effect-tags updated)))
      (is (= 8 (fx/run-sync! updated)))))

  (testing "update-effects-by-tag"
    (let [pipeline (-> (fx/succeed> 100)
                       (fx/map> (fn [x] (+ x 10))))
          updated (fxu/update-effects-by-tag pipeline :succeed (fn [_] (fx/succeed> 50)))]
      (is (= 60 (fx/run-sync! updated))))))

(deftest splicing-and-mutation-test
  (testing "remove-by-tag"
    (let [pipeline (-> (fx/succeed> 10)
                       (fx/tap> (fn [_] (throw (ex-info "should not run" {}))))
                       (fx/map> inc))
          fast (fxu/remove-by-tag pipeline :tap)]
      (is (= [:succeed :map] (fxu/effect-tags fast)))
      (is (= 11 (fx/run-sync! fast)))))

  (testing "replace-by-tag (mocking root in tests)"
    (let [pipeline (-> (fx/service> :db)
                       (fx/map> (fn [db] (str "db:" db))))
          mocked (fxu/replace-by-tag pipeline :context (fx/succeed> "mock-postgres"))]
      (is (= [:succeed :map] (fxu/effect-tags mocked)))
      (is (= "db:mock-postgres" (fx/run-sync! mocked)))))

  (testing "insert-after-tag (injecting telemetry)"
    (let [log-atom (atom [])
          pipeline (-> (fx/succeed> 5)
                       (fx/map> inc)
                       (fx/map> (fn [x] (* x 10))))
          instrumented (fxu/insert-after-tag pipeline :succeed (fx/tap> (fn [v] (swap! log-atom conj [:root v]))))]
      (is (= [:succeed :tap :map :map] (fxu/effect-tags instrumented)))
      (is (= 60 (fx/run-sync! instrumented)))
      (is (= [[:root 5]] @log-atom))))

  (testing "insert-before-tag"
    (let [pipeline (-> (fx/succeed> 10)
                       (fx/map> (fn [x] (* x 2))))
          spliced (fxu/insert-before-tag pipeline :map (fx/map> inc))]
      (is (= [:succeed :map :map] (fxu/effect-tags spliced)))
      (is (= 22 (fx/run-sync! spliced)))))

  (testing "prepend-root"
    (let [pipeline (-> (fx/map> inc)
                       (fx/map> (fn [x] (* x 2))))
          prepended (fxu/prepend-root pipeline (fx/succeed> 20))]
      (is (= [:succeed :map :map] (fxu/effect-tags prepended)))
      (is (= 42 (fx/run-sync! prepended)))))

  (testing "append-leaf"
    (let [pipeline (-> (fx/succeed> 10)
                       (fx/map> inc))
          appended (fxu/append-leaf pipeline (fx/map> (fn [x] (* x 2))))]
      (is (= [:succeed :map :map] (fxu/effect-tags appended)))
      (is (= 22 (fx/run-sync! appended)))))

  (testing "concat-chains"
    (let [c1 (-> (fx/succeed> 5) (fx/map> inc))
          c2 (-> (fx/map> (fn [x] (* x 3))) (fx/map> inc))
          merged (fxu/concat-chains c1 c2)]
      (is (= [:succeed :map :map :map] (fxu/effect-tags merged)))
      (is (= 19 (fx/run-sync! merged)))))

  (testing "slice-effects"
    (let [pipeline (-> (fx/succeed> 10)
                       (fx/map> inc)
                       (fx/map> (fn [x] (* x 2)))
                       (fx/map> dec))]
      (is (= [:succeed :map :map :map] (fxu/effect-tags pipeline)))
      (let [sub (fxu/slice-effects pipeline 1 3)]
        (is (= [:map :map] (fxu/effect-tags sub)))
        (is (= 22 (fx/run-sync! (fx/chain> (fx/succeed> 10) sub))))))))

(deftest deep-ast-traversal-test
  (testing "direct-sub-effects & ast-seq on nested structures"
    (let [inner1 (fx/map> inc)
          inner2 (fx/map> dec)
          branch (fx/if> (fx/map> even?) inner1 inner2)
          pipeline (-> (fx/succeed> 10)
                       (fx/chain> branch))
          all-ast (fxu/ast-seq pipeline)
          all-tags (mapv fx/tag all-ast)]
      (is (some #{:if} all-tags))
      (is (some #{:succeed} all-tags))
      (is (some #{:map} all-tags))))

  (testing "ast-seq on all> composite effects"
    (let [e1 (fx/succeed> 1)
          e2 (fx/succeed> 2)
          composite (fx/all> [e1 e2])
          all-ast (fxu/ast-seq composite)
          tags (mapv fx/tag all-ast)]
      (is (= [:all :succeed :succeed] tags)))))

(deftest combinator-composition-test
  (testing "pipe> composes combinators in forward order"
    (let [log-atom (atom [])
          step1 (fn [eff] (fx/tap> eff (fn [v] (swap! log-atom conj [:step1 v]))))
          step2 (fn [eff] (fx/map> eff inc))
          step3 (fn [eff] (fx/map> eff (fn [x] (* x 2))))
          composed (fxu/pipe> step1 step2 step3)
          pipeline (composed (fx/succeed> 10))]
      (is (= 22 (fx/run-sync! pipeline)))
      (is (= [[:step1 10]] @log-atom))))

  (testing "comp> composes combinators in standard mathematical order"
    (let [add-one (fn [eff] (fx/map> eff inc))
          double-it (fn [eff] (fx/map> eff (fn [x] (* x 2))))
          ;; comp> executes double-it then add-one: (10 * 2) + 1 = 21
          composed (fxu/comp> add-one double-it)
          pipeline (composed (fx/succeed> 10))]
      (is (= 21 (fx/run-sync! pipeline)))))

  (testing "pipe-fx-fn> composes effect-producing functions (Kleisli arrows)"
    (let [f1 (fn [x] (fx/succeed> (str "user-" x)))
          f2 (fn [user-str] (fx/succeed> {:username user-str :active true}))
          f3 (fn [{:keys [username]}] (fx/succeed> (str "valid:" username)))
          composed-fn (fxu/pipe-fx-fn> f1 f2 f3)
          result (fx/run-sync! (composed-fn 42))]
      (is (= "valid:user-42" result))))

  (testing "around> wraps an effect stage with before and after actions"
    (let [log-atom (atom [])
          before-fn (fn [v] (fx/tap> (fn [_] (swap! log-atom conj [:before v]))))
          after-fn (fn [v] (fx/tap> (fn [_] (swap! log-atom conj [:after v]))))
          pipeline (-> (fx/succeed> 5)
                       (fx/map> (fn [x] (* x 2)))
                       ((fxu/around> before-fn after-fn)))]
      (is (= 10 (fx/run-sync! pipeline)))
      (is (= [[:before 10] [:after 10]] @log-atom))))

  (testing "with-scoped-service> provides scoped context overrides"
    (let [pipeline-transform (fxu/with-scoped-service>
                               :multiplier 10
                               (fn [eff] (fx/map-ctx> eff (fn [v ctx] (* v (:multiplier ctx 1))))))
          eff (pipeline-transform (fx/succeed> 7))]
      (is (= 70 (fx/run-sync! eff)))))

  (testing "compose-ast-passes applies multiple AST optimization/rewrite passes"
    (let [fuse-pass (fn [leaf-effect]
                      (fxu/map-effects leaf-effect
                                       (fn [eff]
                                         (if (= (fx/tag eff) :succeed)
                                           (fx/succeed> (* (:value eff) 2))
                                           eff))))
          log-pass (fn [leaf-effect]
                     (fxu/insert-after-tag leaf-effect :succeed (fx/map> inc)))
          optimizer (fxu/compose-ast-passes fuse-pass log-pass)
          pipeline (-> (fx/succeed> 10)
                       (fx/map> inc))
          optimized (optimizer pipeline)]
      ;; succeed 10 -> pass 1: succeed 20 -> pass 2: insert inc -> map inc: (20 + 1) + 1 = 22
      (is (= 22 (fx/run-sync! optimized))))))

(deftest defrecord-transparency-test
  (testing "Individual defrecords expose direct fields with complete transparency"
    (let [m (fx/map> inc)
          r (fx/retry> m {:max-attempts 3 :delay-ms 100})
          s (fx/service> :database :postgres)
          p (fx/provide> s {:database :mock})
          e (fx/ensure> m (fx/succeed> :cleaned))]
      (is (= :map (:tag m)))
      (is (= inc (:f m)))
      (is (= :retry (:tag r)))
      (is (= {:max-attempts 3 :delay-ms 100} (:policy r)))
      (is (= m (:target r)))
      (is (= :context (:tag s)))
      (is (= :database (:key s)))
      (is (= :postgres (:default s)))
      (is (= :provide (:tag p)))
      (is (= s (:body p)))
      (is (= {:database :mock} (:context-map p)))
      (is (= :ensure (:tag e)))
      (is (= m (:prev-effect e)))
      (is (= (fx/succeed> :cleaned) (:finalizer e))))))
