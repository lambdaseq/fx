(ns fx.utils
  "Pure utility functions for inspecting, querying, traversing, transforming,
   reconstructing, and composing Effect pipelines and ASTs."
  (:refer-clojure :exclude [comp])
  (:require [fx.core :as fx]))

;; ---------------------------------------------------------------------------
;; 1. Inspection & Linear Traversal
;; ---------------------------------------------------------------------------

(defn effect-seq-reverse
  "Returns a lazy sequence of effects starting from `leaf-effect` back to the root (leaf -> root),
   following upstream `:prev-effect` links."
  [leaf-effect]
  (when (fx/effect? leaf-effect)
    (->> leaf-effect
         (iterate fx/prev-effect)
         (take-while fx/effect?))))

(defn effect-seq
  "Returns a vector of effects in forward execution order (root -> leaf)."
  [leaf-effect]
  (if (fx/effect? leaf-effect)
    (vec (reverse (effect-seq-reverse leaf-effect)))
    []))

(defn root-effect
  "Traverses upstream to locate the root (source) effect of the chain."
  [leaf-effect]
  (when (fx/effect? leaf-effect)
    (last (effect-seq-reverse leaf-effect))))

(defn chain-length
  "Returns the total number of effect nodes in the linear pipeline."
  [leaf-effect]
  (count (effect-seq-reverse leaf-effect)))

(defn effect-tags
  "Returns a vector of tags representing the execution pipeline in forward order."
  [leaf-effect]
  (mapv fx/tag (effect-seq leaf-effect)))

;; ---------------------------------------------------------------------------
;; 2. Query & Filtering
;; ---------------------------------------------------------------------------

(defn find-first-effect
  "Returns the first effect in forward execution order satisfying `pred`, or nil."
  [leaf-effect pred]
  (some (fn [eff] (when (pred eff) eff))
        (effect-seq leaf-effect)))

(defn find-first-by-tag
  "Finds the first effect matching the given `target-tag` in forward execution order."
  [leaf-effect target-tag]
  (find-first-effect leaf-effect (fn [eff] (= (fx/tag eff) target-tag))))

(defn find-all-effects
  "Returns a vector of all effects satisfying `pred` in forward execution order."
  [leaf-effect pred]
  (filterv pred (effect-seq leaf-effect)))

(defn find-all-by-tag
  "Returns a vector of all effects with the specified `target-tag`."
  [leaf-effect target-tag]
  (find-all-effects leaf-effect (fn [eff] (= (fx/tag eff) target-tag))))

;; ---------------------------------------------------------------------------
;; 3. Pipeline Reconstruction & Mapping
;; ---------------------------------------------------------------------------

(defn rechain-effects
  "Given a sequential collection of effects `[e1 e2 ... en]` in execution order,
   re-links them into a continuous pipeline where `e1` is root and `en` is leaf."
  [effects]
  (when (seq effects)
    (let [clean-effects (mapv #(assoc % :prev-effect nil) (filter fx/effect? effects))]
      (when (seq clean-effects)
        (reduce fx/chain> clean-effects)))))

(defn map-effects
  "Applies pure transform `(f effect)` to every node in the pipeline from root to leaf,
   rebuilding the chain structure."
  [leaf-effect f]
  (let [nodes (effect-seq leaf-effect)
        transformed (mapv f nodes)]
    (rechain-effects transformed)))

(defn update-effects-by
  "Applies `(f effect)` to all nodes satisfying `pred`, preserving untouched nodes
   and reconstructing the chain."
  [leaf-effect pred f]
  (map-effects leaf-effect (fn [eff]
                             (if (pred eff)
                               (f eff)
                               eff))))

(defn update-effects-by-tag
  "Applies `(f effect)` to all nodes matching `target-tag`."
  [leaf-effect target-tag f]
  (update-effects-by leaf-effect (fn [eff] (= (fx/tag eff) target-tag)) f))

;; ---------------------------------------------------------------------------
;; 4. Splicing, Insertion, Deletion & Replacement
;; ---------------------------------------------------------------------------

(defn remove-effects-by
  "Removes all nodes satisfying `pred` from the chain and connects surrounding nodes."
  [leaf-effect pred]
  (let [retained (filterv (complement pred) (effect-seq leaf-effect))]
    (rechain-effects retained)))

(defn remove-by-tag
  "Removes all nodes matching `target-tag`."
  [leaf-effect target-tag]
  (remove-effects-by leaf-effect (fn [eff] (= (fx/tag eff) target-tag))))

(defn replace-effect-by
  "Replaces the first node matching `pred` with `replacement-effect`."
  [leaf-effect pred replacement-effect]
  (let [nodes (effect-seq leaf-effect)
        [found? replaced]
        (reduce (fn [[found? acc] eff]
                  (if (and (not found?) (pred eff))
                    [true (conj acc replacement-effect)]
                    [found? (conj acc eff)]))
                [false []]
                nodes)]
    (if found?
      (rechain-effects replaced)
      leaf-effect)))

(defn replace-by-tag
  "Replaces the first node matching `target-tag` with `replacement-effect`."
  [leaf-effect target-tag replacement-effect]
  (replace-effect-by leaf-effect (fn [eff] (= (fx/tag eff) target-tag)) replacement-effect))

(defn insert-effect-after
  "Inserts `new-effect` immediately after the first node satisfying `pred`."
  [leaf-effect pred new-effect]
  (let [nodes (effect-seq leaf-effect)
        [inserted? spliced]
        (reduce (fn [[inserted? acc] eff]
                  (let [acc' (conj acc eff)]
                    (if (and (not inserted?) (pred eff))
                      [true (conj acc' new-effect)]
                      [inserted? acc'])))
                [false []]
                nodes)]
    (if inserted?
      (rechain-effects spliced)
      leaf-effect)))

(defn insert-after-tag
  "Inserts `new-effect` immediately after the first node matching `target-tag`."
  [leaf-effect target-tag new-effect]
  (insert-effect-after leaf-effect (fn [eff] (= (fx/tag eff) target-tag)) new-effect))

(defn insert-effect-before
  "Inserts `new-effect` immediately before the first node satisfying `pred`."
  [leaf-effect pred new-effect]
  (let [nodes (effect-seq leaf-effect)
        [inserted? spliced]
        (reduce (fn [[inserted? acc] eff]
                  (if (and (not inserted?) (pred eff))
                    [true (conj acc new-effect eff)]
                    [inserted? (conj acc eff)]))
                [false []]
                nodes)]
    (if inserted?
      (rechain-effects spliced)
      leaf-effect)))

(defn insert-before-tag
  "Inserts `new-effect` immediately before the first node matching `target-tag`."
  [leaf-effect target-tag new-effect]
  (insert-effect-before leaf-effect (fn [eff] (= (fx/tag eff) target-tag)) new-effect))

(defn prepend-root
  "Prepends `new-root-effect` at the start of `target-chain`."
  [target-chain new-root-effect]
  (let [nodes (effect-seq target-chain)]
    (rechain-effects (into [new-root-effect] nodes))))

(defn append-leaf
  "Appends `new-leaf-effect` to the end of `target-chain`."
  [target-chain new-leaf-effect]
  (fx/chain> target-chain new-leaf-effect))

(defn concat-chains
  "Concatenates multiple effect chains into a single continuous pipeline."
  [& chains]
  (let [all-nodes (mapcat effect-seq (filter some? chains))]
    (rechain-effects (vec all-nodes))))

(defn slice-effects
  "Returns a subchain of effects between `start` (inclusive) and `end` (exclusive) indices
   in execution order."
  ([leaf-effect start]
   (slice-effects leaf-effect start (chain-length leaf-effect)))
  ([leaf-effect start end]
   (let [nodes (effect-seq leaf-effect)
         n (count nodes)
         s (max 0 (min n (or start 0)))
         e (max s (min n (or end n)))
         sliced (subvec nodes s e)]
     (rechain-effects sliced))))

;; ---------------------------------------------------------------------------
;; 5. Deep AST Traversal
;; ---------------------------------------------------------------------------

(defn direct-sub-effects
  "Returns a vector of all direct child effects referenced by `effect`
   (including upstream `:prev-effect` and nested branch/body effects in `:data` or record fields)."
  [effect]
  (when (fx/effect? effect)
    (let [prev (fx/prev-effect effect)
          data (or (:data effect) effect)
          nested (cond
                   (contains? data :effects) (filterv fx/effect? (:effects data))
                   (contains? data :conditions) (reduce (fn [acc [t e]]
                                                          (cond-> acc
                                                            (fx/effect? t) (conj t)
                                                            (fx/effect? e) (conj e)))
                                                        []
                                                        (:conditions data))
                   (contains? data :handlers) (filterv fx/effect? (vals (:handlers data)))
                   :else
                   (filterv fx/effect? (vals (select-keys data [:body :finalizer :acquire :target :inner-effect
                                                                :cond :then :else :eff-a :eff-b :catch]))))]
      (cond-> []
        (fx/effect? prev) (conj prev)
        (seq nested) (into nested)))))

(defn ast-seq
  "Returns a lazy sequence of all effects in the AST rooted at `effect` in depth-first order."
  [effect]
  (tree-seq fx/effect? direct-sub-effects effect))

;; ---------------------------------------------------------------------------
;; 6. Combinator Composition & Higher-Order Combinators
;; ---------------------------------------------------------------------------

(defn pipe>
  "Composes a series of effect combinator functions (each of type `Effect -> Effect`)
   in left-to-right (forward) execution order into a single composite combinator."
  [& combinators]
  (let [active-combinators (filterv fn? combinators)]
    (fn [root-effect]
      (reduce (fn [eff combinator]
                (combinator eff))
              root-effect
              active-combinators))))

(defn comp>
  "Composes effect combinator functions (each of type `Effect -> Effect`)
   in right-to-left (standard mathematical) order into a single composite combinator."
  [& combinators]
  (apply pipe> (reverse combinators)))

(defn pipe-fx-fn>
  "Composes effect-producing functions (Kleisli arrows: `a -> Effect[b]`) from left to right.
   Returns a function `(fn [initial-val] -> Effect[final-val])`."
  [& fx-fns]
  (let [fns (filterv fn? fx-fns)]
    (fn [initial-value]
      (reduce (fn [eff next-fx-fn]
                (fx/mapcat> eff next-fx-fn))
              (fx/succeed> initial-value)
              fns))))

(defn around>
  "Creates a combinator that wraps any target effect stage with a `before-eff-fn`
   and an `after-eff-fn`, while preserving the stage's computed value."
  [before-eff-fn after-eff-fn]
  (fn [target-eff]
    (let [v-sym (gensym "val")]
      (-> target-eff
          (fx/mapcat> (fn [v]
                        (-> (before-eff-fn v)
                            (fx/mapcat> (fn [_] (after-eff-fn v)))
                            (fx/map> (fn [_] v)))))))))

(defn with-scoped-service>
  "Wraps a combinator so that it executes within a provided service/context override,
   restoring the previous context upon completion."
  [service-key service-instance combinator]
  (fn [target-eff]
    (-> target-eff
        combinator
        (fx/provide> {service-key service-instance}))))

(defn compose-ast-passes
  "Composes pipeline rewrite passes `(Effect -> Effect)` into an optimizing compiler pipeline."
  [& passes]
  (let [active-passes (filterv fn? passes)]
    (fn [pipeline]
      (reduce (fn [ast pass] (pass ast)) pipeline active-passes))))
