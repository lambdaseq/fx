(ns fx.layer
  "Composable dependency injection and resource lifecycle management inspired by Effect-ts and ZIO.

   Layers model recipes for acquiring and releasing environmental context and services.
   Layers can be composed horizontally with `merge>`, vertically with `compose>`,
   and provided to effect pipelines with `provide-layer>` or managed with `start-layer!`
   and `launch-sync!`."
  (:require [fx.core :as fx])
  #?(:clj (:import (java.io Closeable))))

;; ---------------------------------------------------------------------------
;; Protocols
;; ---------------------------------------------------------------------------

(defprotocol IScope
  "Protocol for managing resource finalizers within a lifecycle scope."
  (add-finalizer! [this finalizer-eff]
    "Registers a finalizer effect into the scope.")
  (close-scope!> [this]
    "Returns an effect that deterministically executes all registered finalizers
     in reverse order of registration."))

(defprotocol ILayer
  "Protocol for composable dependency injection and lifecycle management layers."
  (-build-eff [this scope]
    "Returns an `IEffect` that builds the layer within `scope` and yields the context map."))

;; ---------------------------------------------------------------------------
;; Scope Implementation
;; ---------------------------------------------------------------------------

(defn- run-finalizers-seq>
  "Builds an effect executing all finalizers sequentially in reverse order,
   ensuring every finalizer runs even if prior finalizers fail."
  [finalizers]
  (let [reversed-fins (reverse finalizers)]
    (reduce (fn [acc fin]
              (fx/ensure> acc
                          (fx/match> (fx/try> fin :layer/finalizer-error)
                                     (fn [_err] (fx/succeed> nil))
                                     (fn [_ok] (fx/succeed> nil)))))
            (fx/succeed> nil)
            reversed-fins)))

(defrecord AddFinalizerEffect [tag prev-effect data scope finalizer-eff]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      (let [step-eff (assoc this :prev-effect nil)]
        [prev-effect val context (conj stack (reify fx/IContinuation
                                               (-resume [_ res-val res-ctx res-stack]
                                                 (fx/-step step-eff res-val res-ctx res-stack))))])
      (do
        (add-finalizer! scope finalizer-eff)
        [nil val context stack]))))

(defrecord CloseScopeEffect [tag prev-effect data scope]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      (let [step-eff (assoc this :prev-effect nil)]
        [prev-effect val context (conj stack (reify fx/IContinuation
                                               (-resume [_ res-val res-ctx res-stack]
                                                 (fx/-step step-eff res-val res-ctx res-stack))))])
      (let [fins @(:finalizers-atom scope)
            _ (reset! (:finalizers-atom scope) [])
            eff (run-finalizers-seq> fins)]
        [eff val context stack]))))

(defrecord Scope [finalizers-atom]
  IScope
  (add-finalizer! [_ finalizer-eff]
    (when (fx/effect? finalizer-eff)
      (swap! finalizers-atom conj finalizer-eff)))
  (close-scope!> [this]
    (->CloseScopeEffect :close-scope nil {:scope this} this)))

(defn make-scope
  "Creates a fresh, empty `IScope` instance."
  []
  (->Scope (atom [])))

(defn- add-finalizer>
  "Effect that registers `finalizer-eff` into `scope`."
  [scope finalizer-eff]
  (->AddFinalizerEffect :add-finalizer nil {:scope scope :finalizer finalizer-eff} scope finalizer-eff))

(defn- build-layer-safe>
  "Safely builds `layer` within `scope`, ensuring that any partially-acquired
   resources are cleaned up if acquisition produces a failure or throws an exception."
  [layer scope]
  (-> (fx/try> (-build-eff layer scope) :layer/build-error)
      (fx/match>
       (fn [err]
         (-> (close-scope!> scope)
             (fx/mapcat> (fn [_] (fx/fail> (:tag err) (:error-data err))))))
       (fn [ctx]
         (fx/succeed> ctx)))))

;; ---------------------------------------------------------------------------
;; Layer AST Records
;; ---------------------------------------------------------------------------

(defrecord ValueLayer [context-map]
  ILayer
  (-build-eff [_ _scope]
    (fx/succeed> (or context-map {}))))

(defrecord EffectLayer [key effect]
  ILayer
  (-build-eff [_ _scope]
    (fx/map> effect (fn [v] {key v}))))

(defrecord ResourceLayer [key acquire-eff release-fn]
  ILayer
  (-build-eff [_ scope]
    (-> acquire-eff
        (fx/mapcat> (fn [resource]
                      (let [rel-eff (release-fn resource)]
                        (-> (add-finalizer> scope rel-eff)
                            (fx/map> (fn [_] {key resource})))))))))

(defrecord MapResourceLayer [acquire-eff release-fn]
  ILayer
  (-build-eff [_ scope]
    (-> acquire-eff
        (fx/mapcat> (fn [ctx-map]
                      (let [rel-eff (release-fn ctx-map)]
                        (-> (add-finalizer> scope rel-eff)
                            (fx/map> (fn [_] ctx-map)))))))))

(defrecord MergeLayer [l1 l2]
  ILayer
  (-build-eff [_ scope]
    (-> (-build-eff l1 scope)
        (fx/mapcat> (fn [ctx1]
                      (-> (-build-eff l2 scope)
                          (fx/map> (fn [ctx2] (merge ctx1 ctx2)))))))))

(defrecord ComposeLayer [l1 l2]
  ILayer
  (-build-eff [_ scope]
    (-> (-build-eff l1 scope)
        (fx/mapcat> (fn [ctx1]
                      (-> (-build-eff l2 scope)
                          (fx/provide> ctx1)
                          (fx/map> (fn [ctx2] (merge ctx1 ctx2)))))))))

;; ---------------------------------------------------------------------------
;; Predicates & Constructors
;; ---------------------------------------------------------------------------

(defn layer?
  "Returns true if `x` implements `ILayer`."
  [x]
  #?(:clj  (instance? fx.layer.ILayer x)
     :cljs (satisfies? ILayer x)))

(defn from-value>
  "Creates a layer providing `{key val}` with no lifecycle management."
  [key val]
  (->ValueLayer {key val}))

(defn from-values>
  "Creates a layer providing `context-map` with no lifecycle management."
  [context-map]
  (->ValueLayer (or context-map {})))

(defn from-effect>
  "Creates a layer providing `{key val}` by evaluating `effect` without release lifecycle."
  [key effect]
  (->EffectLayer key effect))

(defn make>
  "Creates a lifecycle-managed layer for a single service `key`.
   `acquire-eff` is an `Effect` returning the resource.
   `release-fn` is a pure function `(fn [resource] -> Effect)` returning an `Effect` to release the resource."
  [key acquire-eff release-fn]
  (->ResourceLayer key acquire-eff release-fn))

(defn make-map>
  "Creates a lifecycle-managed layer providing a map of services.
   `acquire-eff` is an `Effect` returning a context map.
   `release-fn` is a pure function `(fn [context-map] -> Effect)` returning an `Effect` to release the resources."
  [acquire-eff release-fn]
  (->MapResourceLayer acquire-eff release-fn))

(defn merge>
  "Merges multiple layers horizontally. Layers are evaluated independently and their
   resulting context maps are merged. Resources are released in reverse acquisition order."
  ([]
   (from-values> {}))
  ([layer]
   layer)
  ([l1 l2]
   (->MergeLayer l1 l2))
  ([l1 l2 & more]
   (reduce merge> (merge> l1 l2) more)))

(defn compose>
  "Composes layers vertically. Each layer in the sequence is provided with the accumulated
   context of all previous layers. Resources are released in reverse acquisition order."
  ([]
   (from-values> {}))
  ([layer]
   layer)
  ([l1 l2]
   (->ComposeLayer l1 l2))
  ([l1 l2 & more]
   (reduce compose> (compose> l1 l2) more)))

;; ---------------------------------------------------------------------------
;; Effect Combinators
;; ---------------------------------------------------------------------------

(defn provide-layer>
  "Executes `effect` within the context provided by `layer`, deterministically
   releasing all acquired layer resources upon completion.

   Supports standalone and thread-first pipeline usage:
     (-> (fx/service> :http/server)
         (fx-layer/provide-layer> app-layer))

     (fx-layer/provide-layer> (fx/service> :http/server) app-layer)"
  ([layer]
   (provide-layer> (fx/context>) layer))
  ([effect layer]
   (let [scope (make-scope)]
     (fx/acquire-release>
      (build-layer-safe> layer scope)
      (fn [layer-ctx]
        (fx/provide> effect layer-ctx))
      (fn [_]
        (close-scope!> scope))))))

(defn with-layer>
  "Executes `use-eff-fn` (a function `(fn [context] -> Effect)`) within the context of `layer`,
   guaranteeing complete teardown of all acquired layer resources upon completion."
  [layer use-eff-fn]
  (let [scope (make-scope)]
    (fx/acquire-release>
     (build-layer-safe> layer scope)
     (fn [layer-ctx]
       (fx/provide> (use-eff-fn layer-ctx) layer-ctx))
     (fn [_]
       (close-scope!> scope)))))

;; ---------------------------------------------------------------------------
;; System Record & Runners
;; ---------------------------------------------------------------------------

(deftype LayerSystem [context scope]
  #?@(:clj
      [clojure.lang.ILookup
       (valAt [_ k] (get context k))
       (valAt [_ k not-found] (get context k not-found))
       clojure.lang.IDeref
       (deref [_] context)
       java.io.Closeable
       (close [_]
              (fx/run-sync! (close-scope!> scope)))
       java.lang.Object
       (toString [_]
                 (str "#fx.layer/System" (into {} context)))
       (equals [_ other]
               (and (instance? LayerSystem other) (= context (.-context ^LayerSystem other))))
       (hashCode [_]
                 (.hashCode context))]
      :cljs
      [ILookup
       (-lookup [_ k] (get context k))
       (-lookup [_ k not-found] (get context k not-found))
       IDeref
       (-deref [_] context)
       IPrintWithWriter
       (-pr-writer [_ writer _opts]
                   (-write writer (str "#fx.layer/System" (into {} context))))]))

(defn start-layer!
  "Starts a layer synchronously, returning an active `LayerSystem` record containing
   the initialized `:context` and implementing `java.io.Closeable`."
  ([layer]
   (start-layer! layer {}))
  ([layer initial-context]
   (let [scope (make-scope)
         res (fx/run-sync! (build-layer-safe> layer scope) (or initial-context {}))]
     (if (fx/failure? res)
       (do
         (fx/run-sync! (close-scope!> scope))
         (throw (ex-info "Failed to start layer" {:failure res})))
       (->LayerSystem res scope)))))

(defn stop-layer!
  "Stops an active system, deterministically executing all layer finalizers in reverse order."
  [system]
  #?(:clj
     (when (instance? Closeable system)
       (.close ^Closeable system))
     :cljs
     (when-let [scope (:scope system)]
       (fx/run-sync! (close-scope!> scope)))))

(defn launch-sync!
  "Starts a layer synchronously, registers a JVM shutdown hook to clean up on process termination,
   and optionally blocks if `:join? true` in `opts`."
  ([layer]
   (launch-sync! layer {}))
  ([layer opts]
   (let [system (start-layer! layer opts)]
     #?(:clj
        (let [shutdown-thread (Thread. (fn [] (stop-layer! system)))]
          (.addShutdownHook (Runtime/getRuntime) shutdown-thread)))
     (when (:join? opts)
       #?(:clj
          (let [srv (or (get system :todo/server)
                        (get system :http/server)
                        (get system :server))]
            (if (and srv
                     (try
                       (let [join-m (.getMethod (class srv) "join" (into-array Class []))]
                         (.invoke join-m srv (into-array Object []))
                         true)
                       (catch Throwable _ false)))
              system
              (let [latch (java.util.concurrent.CountDownLatch. 1)]
                (.await latch))))
          :cljs nil))
     system)))
