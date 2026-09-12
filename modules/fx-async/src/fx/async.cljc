(ns fx.async
  "Structured concurrency, fibers, parallel combinators, and execution scopes for fx."
  (:refer-clojure :exclude [ref])
  (:require [fx.async.channel :as channel]
            [fx.async.coordination :as coord]
            [fx.async.fiber :as fiber]
            [fx.async.protocols :as p]
            [fx.core :as fx])
  #?(:clj (:import (java.util.concurrent CompletableFuture
                                         CountDownLatch
                                         Semaphore)
                   (java.util.concurrent.atomic AtomicReferenceArray))))

;; ---------------------------------------------------------------------------
;; Re-exported Protocols & Predicates & Runners
;; ---------------------------------------------------------------------------

(def fiber? p/fiber?)
(def queue? p/queue?)
(def hub? p/hub?)
(def deferred? p/deferred?)
(def semaphore? p/semaphore?)
(def latch? p/latch?)
(def ref? p/ref?)

(def fiber-id p/fiber-id)
(def fiber-status p/fiber-status)
(def fiber-status-atom p/fiber-status-atom)
(def fiber-result p/fiber-result)
(def fiber-children p/fiber-children)

(def run-fiber! fiber/run-fiber!)
(def run-async-chan! fiber/run-async-chan!)
(def join-fiber! fiber/join-fiber!)
(def interrupt-fiber! fiber/interrupt-fiber!)

;; ---------------------------------------------------------------------------
;; Re-exported Channel Bridging Combinators
;; ---------------------------------------------------------------------------

(def chan> channel/chan>)
(def chan-put> channel/chan-put>)
(def chan-take> channel/chan-take>)
(def chan-close> channel/chan-close>)
(def chan-alts> channel/chan-alts>)
(def chan-pipe> channel/chan-pipe>)
(def chan-drain> channel/chan-drain>)
(def chan-pub> channel/chan-pub>)
(def chan-sub> channel/chan-sub>)
(def chan-unsub> channel/chan-unsub>)

;; ---------------------------------------------------------------------------
;; Re-exported Coordination & Synchronization Combinators
;; ---------------------------------------------------------------------------

;; Queues
(def queue-bounded> coord/queue-bounded>)
(def queue-unbounded> coord/queue-unbounded>)
(def queue-sliding> coord/queue-sliding>)
(def queue-dropping> coord/queue-dropping>)
(def queue-offer> coord/queue-offer>)
(def queue-take> coord/queue-take>)
(def queue-poll> coord/queue-poll>)
(def queue-size> coord/queue-size>)
(def queue-shutdown> coord/queue-shutdown>)

;; Hubs
(def hub-bounded> coord/hub-bounded>)
(def hub-publish> coord/hub-publish>)
(def hub-subscribe> coord/hub-subscribe>)
(def hub-unsubscribe> coord/hub-unsubscribe>)
(def hub-subscriber-count> coord/hub-subscriber-count>)
(def hub-shutdown> coord/hub-shutdown>)

;; Deferreds
(def deferred> coord/deferred>)
(def deferred-succeed> coord/deferred-succeed>)
(def deferred-fail> coord/deferred-fail>)
(def deferred-await> coord/deferred-await>)

;; Semaphores
(def semaphore> coord/semaphore>)
(def semaphore-acquire> coord/semaphore-acquire>)
(def semaphore-release> coord/semaphore-release>)
(def semaphore-available-permits> coord/semaphore-available-permits>)
(def with-permit> coord/with-permit>)

;; Latches
(def countdown-latch> coord/countdown-latch>)
(def latch-count-down> coord/latch-count-down>)
(def latch-await> coord/latch-await>)
(def latch-get-count> coord/latch-get-count>)

;; Refs
(def ref> coord/ref>)
(def ref-get> coord/ref-get>)
(def ref-set> coord/ref-set>)
(def ref-update> coord/ref-update>)

;; ---------------------------------------------------------------------------
;; Step Effect Frame (heap continuation stack for sub-effects)
;; ---------------------------------------------------------------------------

(defrecord StepEffectFrame [effect]
  fx/IContinuation
  (-resume [_ val context stack]
    (fx/-step effect val context stack)))

;; ---------------------------------------------------------------------------
;; Fiber Lifecycle Effect Records
;; ---------------------------------------------------------------------------

(defrecord ForkEffect [tag prev-effect data target-effect]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (or target-effect val)]
        (if (fx/failure? eff)
          [nil eff context stack]
          (if (fx/effect? eff)
            (let [fib (fiber/run-fiber! eff context)]
              [nil fib context stack])
            [nil (fx/make-failure :async/invalid-fork-target {:target eff}) context stack]))))))

(defn fork>
  "Creates an effect that forks `target-effect` into a background fiber and yields the `IFiber` reference.
   Supports threading upstream effect: `(-> eff (fork>))` or standalone `(fork> eff)`."
  ([target-effect]
   (if (fx/effect? target-effect)
     (->ForkEffect :fork nil {:effect target-effect} target-effect)
     (->ForkEffect :fork target-effect {} nil)))
  ([prev-effect target-effect]
   (->ForkEffect :fork prev-effect {:effect target-effect} target-effect)))

(defrecord JoinEffect [tag prev-effect data target-fiber timeout-ms timeout-val]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [fib (or target-fiber val)]
        (if (fx/failure? fib)
          [nil fib context stack]
          (if (p/fiber? fib)
            (let [res (if timeout-ms
                        (fiber/join-fiber! fib timeout-ms timeout-val)
                        (fiber/join-fiber! fib))]
              [nil res context stack])
            [nil (fx/make-failure :async/invalid-join-target {:target fib}) context stack]))))))

(defn join>
  "Creates an effect that awaits completion of `fiber` and yields its outcome or propagates failure.
   Optional `opts` map may contain `:timeout-ms` and `:timeout-val`."
  ([]
   (->JoinEffect :join nil {} nil nil nil))
  ([target-or-prev]
   (cond
     (p/fiber? target-or-prev)
     (->JoinEffect :join nil {:fiber target-or-prev} target-or-prev nil nil)

     (and (map? target-or-prev) (not (fx/effect? target-or-prev)))
     (->JoinEffect :join nil target-or-prev nil (:timeout-ms target-or-prev) (:timeout-val target-or-prev))

     :else
     (->JoinEffect :join target-or-prev {} nil nil nil)))
  ([prev-effect target-or-opts]
   (cond
     (p/fiber? target-or-opts)
     (->JoinEffect :join prev-effect {:fiber target-or-opts} target-or-opts nil nil)

     (and (map? target-or-opts) (not (fx/effect? target-or-opts)))
     (->JoinEffect :join prev-effect target-or-opts nil (:timeout-ms target-or-opts) (:timeout-val target-or-opts))

     :else
     (->JoinEffect :join prev-effect {:fiber target-or-opts} target-or-opts nil nil))))

(defrecord InterruptEffect [tag prev-effect data target-fiber reason]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [fib (or target-fiber val)]
        (if (fx/failure? fib)
          [nil fib context stack]
          (if (p/fiber? fib)
            (do
              (p/-fiber-interrupt! fib (or reason :user-interrupted))
              [nil true context stack])
            [nil (fx/make-failure :async/invalid-interrupt-target {:target fib}) context stack]))))))

(defn interrupt>
  "Creates an effect that interrupts `fiber` and cascades cancellation to all child fibers."
  ([]
   (->InterruptEffect :interrupt nil {} nil nil))
  ([target-or-prev]
   (if (p/fiber? target-or-prev)
     (->InterruptEffect :interrupt nil {:fiber target-or-prev} target-or-prev nil)
     (->InterruptEffect :interrupt target-or-prev {} nil nil)))
  ([target-or-prev reason-or-target]
   (cond
     (and (p/fiber? target-or-prev) (keyword? reason-or-target))
     (->InterruptEffect :interrupt nil {:fiber target-or-prev :reason reason-or-target} target-or-prev reason-or-target)

     (p/fiber? reason-or-target)
     (->InterruptEffect :interrupt target-or-prev {:fiber reason-or-target} reason-or-target nil)

     :else
     (->InterruptEffect :interrupt target-or-prev {:reason reason-or-target} nil reason-or-target)))
  ([prev-effect target-fiber reason]
   (->InterruptEffect :interrupt prev-effect {:fiber target-fiber :reason reason} target-fiber reason)))

(defrecord FiberStatusEffect [tag prev-effect data target-fiber]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [fib (or target-fiber val (:fiber context))]
        (if (p/fiber? fib)
          [nil (p/fiber-status fib) context stack]
          [nil :unknown context stack])))))

(defn fiber-status>
  "Creates an effect that queries the status (:running, :succeeded, :failed, :interrupted) of `fiber`
   or the currently executing fiber if none provided."
  ([]
   (->FiberStatusEffect :fiber-status nil {} nil))
  ([target-or-prev]
   (if (p/fiber? target-or-prev)
     (->FiberStatusEffect :fiber-status nil {:fiber target-or-prev} target-or-prev)
     (->FiberStatusEffect :fiber-status target-or-prev {} nil)))
  ([prev-effect target-fiber]
   (->FiberStatusEffect :fiber-status prev-effect {:fiber target-fiber} target-fiber)))

;; ---------------------------------------------------------------------------
;; Structured Parallel Combinators: race>, all-par>, map-par>
;; ---------------------------------------------------------------------------

(defn- run-race [effects context]
  (let [eff-list (vec (remove nil? effects))]
    (cond
      (empty? eff-list)
      (fx/make-failure :async/empty-race {:message "race> called with empty effects list"})

      (= 1 (count eff-list))
      (fx/run-sync! (first eff-list) context)

      :else
      #?(:clj
         (let [winner-future (CompletableFuture.)
               active-fibers (atom [])
               completed?    (atom false)
               failure-count (atom 0)
               total-count   (count eff-list)
               parent-fiber  (:fiber context)]
           (doseq [eff eff-list]
             (let [fib (fiber/run-fiber! eff context)]
               (swap! active-fibers conj fib)
               (let [^CompletableFuture cf (p/fiber-result fib)]
                 (.whenComplete cf
                                (reify java.util.function.BiConsumer
                                  (accept [_ result err]
                                    (if (and (nil? err) (not (fx/failure? result)))
                         ;; Winner found
                                      (when (compare-and-set! completed? false true)
                                        (.complete winner-future result)
                           ;; Cancel losing competitors
                                        (doseq [f @active-fibers]
                                          (when-not (identical? f fib)
                                            (fiber/interrupt-fiber! f :race-lost))))
                         ;; Failure encountered
                                      (let [failures (swap! failure-count inc)]
                                        (when (and (= failures total-count)
                                                   (compare-and-set! completed? false true))
                             ;; All racers failed; return the failure
                                          (let [final-fail (or result (fx/make-failure :async/all-racers-failed {:err err}))]
                                            (.complete winner-future final-fail)))))))))))
           ;; Clean up racers if parent fiber interrupted
           (when parent-fiber
             (fiber/add-interrupt-handler! parent-fiber
                                           (fn [_reason]
                                             (doseq [f @active-fibers]
                                               (fiber/interrupt-fiber! f :parent-interrupted)))))
           (try
             (.get winner-future)
             (catch Throwable e
               (let [c (or (.getCause e) e)]
                 (if (fx/failure? c) c (fx/make-failure :async/race-defect c))))))
         :cljs
         ;; Simple cljs fallback
         (fx/run-sync! (first eff-list) context)))))

(defrecord RaceEffect [tag prev-effect data effects]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [effs (or effects val)]
        (if (fx/failure? effs)
          [nil effs context stack]
          (let [res (run-race effs context)]
            [nil res context stack]))))))

(defn race>
  "Races a collection of effects concurrently, returning the outcome of the first successful effect
   and cancelling all other competitors. If all fail, yields the final failure."
  ([effects]
   (if (sequential? effects)
     (->RaceEffect :race nil {:effects (vec effects)} (vec effects))
     (->RaceEffect :race effects {} nil)))
  ([prev-effect effects]
   (->RaceEffect :race prev-effect {:effects (vec effects)} (vec effects))))

(defn- run-all-par [effects _opts context]
  (let [eff-list (vec (remove nil? effects))]
    (cond
      (empty? eff-list)
      []

      :else
      #?(:clj
         (let [n (count eff-list)
               concurrency   (get _opts :concurrency nil)
               fail-fast?    (get _opts :fail-fast? true)
               result-array  (AtomicReferenceArray. n)
               active-fibers (atom #{})
               has-failed?   (atom false)
               latch         (CountDownLatch. n)
               failure-ref   (atom nil)
               parent-fiber  (:fiber context)
               sem           (when (and (number? concurrency) (pos? concurrency))
                               (Semaphore. (int concurrency)))]
           ;; Setup cancellation hook from parent
           (when parent-fiber
             (fiber/add-interrupt-handler! parent-fiber
                                           (fn [_reason]
                                             (doseq [f @active-fibers]
                                               (fiber/interrupt-fiber! f :parent-interrupted)))))

           (doseq [idx (range n)]
             (let [eff (nth eff-list idx)
                   task-eff (if sem
                              (fx/acquire-release>
                               (fx/try> (fn [] (.acquire sem) true))
                               (fn [_] eff)
                               (fn [_] (.release sem)))
                              eff)
                   fib (fiber/run-fiber! task-eff context)]
               (swap! active-fibers conj fib)
               (let [^CompletableFuture cf (p/fiber-result fib)]
                 (.whenComplete cf
                                (reify java.util.function.BiConsumer
                                  (accept [_ result err]
                                    (swap! active-fibers disj fib)
                                    (cond
                                      err
                                      (let [defect (fx/make-failure :async/defect {:exception err :message (.getMessage err)})]
                                        (.set result-array idx defect)
                                        (when (and fail-fast? (compare-and-set! has-failed? false true))
                                          (reset! failure-ref defect)
                                          (doseq [f @active-fibers]
                                            (fiber/interrupt-fiber! f :sibling-failed))))

                                      (fx/failure? result)
                                      (do
                                        (.set result-array idx result)
                                        (when (and fail-fast? (compare-and-set! has-failed? false true))
                                          (reset! failure-ref result)
                                          (doseq [f @active-fibers]
                                            (fiber/interrupt-fiber! f :sibling-failed))))

                                      :else
                                      (.set result-array idx result))
                                    (.countDown latch)))))))

           (.await latch)
           (if (and fail-fast? @has-failed?)
             @failure-ref
             (mapv #(.get result-array %) (range n))))
         :cljs
         (mapv #(fx/run-sync! % context) eff-list)))))

(defrecord AllParEffect [tag prev-effect data effects opts]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [effs (or effects val)]
        (if (fx/failure? effs)
          [nil effs context stack]
          (let [res (run-all-par effs opts context)]
            [nil res context stack]))))))

(defn all-par>
  "Executes a collection of effects in parallel and returns a vector of results.
   By default short-circuits (fail-fast) on first failure, cancelling peer fibers.
   Options map:
     `:concurrency` - maximum concurrent tasks (default: unbounded)
     `:fail-fast?`  - boolean whether to halt on first failure (default: true)"
  ([effects]
   (cond
     (sequential? effects)
     (->AllParEffect :all-par nil {:effects (vec effects)} (vec effects) {})

     (and (map? effects) (not (fx/effect? effects)))
     (->AllParEffect :all-par nil {:opts effects} nil effects)

     :else
     (->AllParEffect :all-par effects {} nil {})))
  ([a b]
   (cond
     (and (sequential? a) (map? b) (not (fx/effect? b)))
     (->AllParEffect :all-par nil {:effects (vec a) :opts b} (vec a) b)

     (and (sequential? a) (map? b))
     (->AllParEffect :all-par nil {:effects (vec a) :opts b} (vec a) b)

     (fx/effect? a)
     (if (sequential? b)
       (->AllParEffect :all-par a {:effects (vec b)} (vec b) {})
       (->AllParEffect :all-par a {:opts b} nil (or b {})))

     :else
     (->AllParEffect :all-par a {:opts b} nil (or b {}))))
  ([prev-effect effects opts]
   (->AllParEffect :all-par prev-effect {:effects (vec effects) :opts opts} (vec effects) (or opts {}))))

(defrecord MapParEffect [tag prev-effect data f coll opts]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [items (or coll val)]
        (if (fx/failure? items)
          [nil items context stack]
          (let [effs (mapv f items)
                res  (run-all-par effs opts context)]
            [nil res context stack]))))))

(defn map-par>
  "Applies effect-producing function `f` concurrently across elements in `coll` and collects results.
   Options map:
     `:concurrency` - maximum concurrent executions (default: unbounded / worker pool)
     `:fail-fast?`  - boolean whether to cancel peer tasks on failure (default: true)

   Examples:
     (map-par> (fn [x] (fetch-item> x)) item-ids {:concurrency 4})
     (-> items (map-par> (fn [x] (process> x)) {:concurrency 2}))"
  ([f]
   (->MapParEffect :map-par nil {:f f} f nil {}))
  ([a b]
   (cond
     (and (fn? a) (sequential? b))
     (->MapParEffect :map-par nil {:f a :coll (vec b)} a (vec b) {})

     (and (fn? a) (map? b) (not (fx/effect? b)))
     (->MapParEffect :map-par nil {:f a :opts b} a nil (or b {}))

     (and (sequential? a) (fn? b))
     (->MapParEffect :map-par nil {:f b :coll (vec a)} b (vec a) {})

     (and (fx/effect? a) (fn? b))
     (->MapParEffect :map-par a {:f b} b nil {})

     :else
     (->MapParEffect :map-par a {:f b} b nil {})))
  ([a b c]
   (cond
     ;; (map-par> f coll opts)
     (and (fn? a) (sequential? b))
     (->MapParEffect :map-par nil {:f a :coll (vec b) :opts c} a (vec b) (or c {}))

     ;; (map-par> f opts coll) e.g. via ->>
     (and (fn? a) (map? b) (sequential? c))
     (->MapParEffect :map-par nil {:f a :coll (vec c) :opts b} a (vec c) (or b {}))

     ;; (-> coll (map-par> f opts)) where coll is raw vector
     (and (sequential? a) (fn? b))
     (->MapParEffect :map-par nil {:f b :coll (vec a) :opts c} b (vec a) (or c {}))

     ;; (-> eff (map-par> f opts)) where eff is effect
     (and (fx/effect? a) (fn? b))
     (->MapParEffect :map-par a {:f b :opts c} b nil (or c {}))

     :else
     (->MapParEffect :map-par a {:f b :opts c} b (when (sequential? c) (vec c)) (if (map? c) c {}))))
  ([prev-effect f coll opts]
   (->MapParEffect :map-par prev-effect {:f f :coll (vec coll) :opts opts} f (vec coll) (or opts {}))))

;; ---------------------------------------------------------------------------
;; Execution Scoping Combinators
;; ---------------------------------------------------------------------------

(defrecord WithEngineEffect [tag prev-effect data target-effect engine executor]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (or target-effect val)]
        (if (fx/failure? eff)
          [nil eff context stack]
          (let [scoped-ctx (cond-> context
                             engine   (assoc :engine engine)
                             executor (assoc :executor executor))]
            [eff nil scoped-ctx (conj stack (fx.core/->RestoreContextFrame context))]))))))

(defn with-virtual-threads>
  "Scopes execution of `effect` to Java Virtual Threads."
  ([target-effect]
   (if (fx/effect? target-effect)
     (->WithEngineEffect :with-virtual-threads nil {:effect target-effect} target-effect :virtual-threads nil)
     (->WithEngineEffect :with-virtual-threads target-effect {} nil :virtual-threads nil)))
  ([prev-effect target-effect]
   (->WithEngineEffect :with-virtual-threads prev-effect {:effect target-effect} target-effect :virtual-threads nil)))

(defn with-thread-pool>
  "Scopes execution of `effect` to an explicit Java `ExecutorService` or fixed thread pool."
  ([executor-or-eff]
   (if (fx/effect? executor-or-eff)
     (->WithEngineEffect :with-thread-pool nil {:effect executor-or-eff} executor-or-eff :thread-pool nil)
     (->WithEngineEffect :with-thread-pool nil {:executor executor-or-eff} nil :thread-pool executor-or-eff)))
  ([prev-or-exec target-or-exec]
   (if (fx/effect? target-or-exec)
     (->WithEngineEffect :with-thread-pool nil {:executor prev-or-exec :effect target-or-exec} target-or-exec :thread-pool prev-or-exec)
     (->WithEngineEffect :with-thread-pool prev-or-exec {:executor target-or-exec} nil :thread-pool target-or-exec)))
  ([prev-effect executor target-effect]
   (->WithEngineEffect :with-thread-pool prev-effect {:executor executor :effect target-effect} target-effect :thread-pool executor)))

(defn with-go>
  "Scopes execution of `effect` to `core.async` go-routines."
  ([target-effect]
   (if (fx/effect? target-effect)
     (->WithEngineEffect :with-go nil {:effect target-effect} target-effect :go nil)
     (->WithEngineEffect :with-go target-effect {} nil :go nil)))
  ([prev-effect target-effect]
   (->WithEngineEffect :with-go prev-effect {:effect target-effect} target-effect :go nil)))
