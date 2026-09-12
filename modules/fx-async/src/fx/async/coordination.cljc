(ns fx.async.coordination
  "High-level async coordination and synchronization primitives for fx:
   Queues, Hubs, Deferreds, Semaphores, Latches, and Refs."
  (:refer-clojure :exclude [await ref])
  (:require [clojure.core.async :as async]
            [fx.async.fiber :as fiber]
            [fx.async.protocols :as p]
            [fx.core :as fx])
  #?(:clj (:import (java.util.concurrent CompletableFuture
                                         ConcurrentHashMap
                                         ConcurrentLinkedQueue
                                         CountDownLatch
                                         Semaphore
                                         TimeUnit
                                         atomic.AtomicBoolean
                                         atomic.AtomicInteger
                                         atomic.AtomicLong
                                         atomic.AtomicReference))))

;; ---------------------------------------------------------------------------
;; Step Effect Frame
;; ---------------------------------------------------------------------------

(defrecord StepEffectFrame [effect]
  fx.core.IContinuation
  (-resume [_ val context stack]
    (fx.core/-step effect val context stack)))

;; ---------------------------------------------------------------------------
;; 1. Async Queues Implementation
;; ---------------------------------------------------------------------------

(deftype UnboundedBuffer [^java.util.Queue q]
  clojure.core.async.impl.protocols.Buffer
  (full? [_] false)
  (remove! [_] (.poll q))
  (add!* [this item] (.offer q item) this)
  (close-buf! [_] nil)
  clojure.core.async.impl.protocols.UnblockingBuffer
  clojure.lang.Counted
  (count [_] (.size q)))

(deftype AsyncQueue [chan capacity closed? size-atom]
  p/IQueue
  (-queue-offer! [_ item]
    (if @closed?
      false
      (let [res (async/offer! chan item)]
        (when res
          (swap! size-atom inc))
        (boolean res))))
  (-queue-take! [_]
    (let [res (async/<!! chan)]
      (when (some? res)
        (swap! size-atom (fn [s] (max 0 (dec s)))))
      res))
  (-queue-poll! [_]
    (let [res (async/poll! chan)]
      (when (some? res)
        (swap! size-atom (fn [s] (max 0 (dec s)))))
      res))
  (-queue-size [_]
    @size-atom)
  (-queue-capacity [_]
    capacity)
  (-queue-shutdown! [_]
    (when (compare-and-set! closed? false true)
      (async/close! chan)
      true)))

(defn make-queue
  "Creates an IQueue instance with the specified buffer strategy."
  ([buffer-type capacity]
   (let [ch (case buffer-type
              :bounded   (async/chan (async/buffer (int capacity)))
              :sliding   (async/chan (async/sliding-buffer (int capacity)))
              :dropping  (async/chan (async/dropping-buffer (int capacity)))
              :unbounded (async/chan (->UnboundedBuffer (ConcurrentLinkedQueue.)))
              (async/chan (int capacity)))]
     (->AsyncQueue ch capacity (atom false) (atom 0)))))

(defrecord QueueConstructorEffect [tag prev-effect data buffer-type capacity]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [cap (or capacity val 16)
            q   (make-queue buffer-type cap)]
        [nil q context stack]))))

(defn queue-bounded>
  "Creates a bounded effect queue with fixed capacity."
  ([]
   (queue-bounded> 16))
  ([capacity-or-prev]
   (if (number? capacity-or-prev)
     (->QueueConstructorEffect :queue-bounded nil {:capacity capacity-or-prev} :bounded capacity-or-prev)
     (->QueueConstructorEffect :queue-bounded capacity-or-prev {} :bounded 16)))
  ([prev-effect capacity]
   (->QueueConstructorEffect :queue-bounded prev-effect {:capacity capacity} :bounded capacity)))

(defn queue-unbounded>
  "Creates an unbounded effect queue."
  ([]
   (->QueueConstructorEffect :queue-unbounded nil {} :unbounded nil))
  ([prev-effect]
   (->QueueConstructorEffect :queue-unbounded prev-effect {} :unbounded nil)))

(defn queue-sliding>
  "Creates a sliding effect queue that drops oldest elements when capacity is exceeded."
  ([]
   (queue-sliding> 16))
  ([capacity-or-prev]
   (if (number? capacity-or-prev)
     (->QueueConstructorEffect :queue-sliding nil {:capacity capacity-or-prev} :sliding capacity-or-prev)
     (->QueueConstructorEffect :queue-sliding capacity-or-prev {} :sliding 16)))
  ([prev-effect capacity]
   (->QueueConstructorEffect :queue-sliding prev-effect {:capacity capacity} :sliding capacity)))

(defn queue-dropping>
  "Creates a dropping effect queue that drops newest elements when capacity is exceeded."
  ([]
   (queue-dropping> 16))
  ([capacity-or-prev]
   (if (number? capacity-or-prev)
     (->QueueConstructorEffect :queue-dropping nil {:capacity capacity-or-prev} :dropping capacity-or-prev)
     (->QueueConstructorEffect :queue-dropping capacity-or-prev {} :dropping 16)))
  ([prev-effect capacity]
   (->QueueConstructorEffect :queue-dropping prev-effect {:capacity capacity} :dropping capacity)))

;; Queue Operation Effects

(defrecord QueueOfferEffect [tag prev-effect data queue item]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [q (or queue (:queue data))
            i (if (some? item) item val)]
        (if (p/queue? q)
          [nil (p/-queue-offer! q i) context stack]
          [nil (fx/make-failure :async/invalid-queue {:queue q}) context stack])))))

(defn queue-offer>
  "Offers `item` to `queue`. Returns boolean whether accepted."
  ([queue]
   (->QueueOfferEffect :queue-offer nil {:queue queue} queue nil))
  ([queue-or-prev item-or-queue]
   (if (p/queue? queue-or-prev)
     (->QueueOfferEffect :queue-offer nil {:queue queue-or-prev :item item-or-queue} queue-or-prev item-or-queue)
     (->QueueOfferEffect :queue-offer queue-or-prev {:queue item-or-queue} item-or-queue nil)))
  ([prev-effect queue item]
   (->QueueOfferEffect :queue-offer prev-effect {:queue queue :item item} queue item)))

(defrecord QueueTakeEffect [tag prev-effect data queue]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [q (or queue val)]
        (if (p/queue? q)
          [nil (p/-queue-take! q) context stack]
          [nil (fx/make-failure :async/invalid-queue {:queue q}) context stack])))))

(defn queue-take>
  "Takes an item from `queue`, parking/blocking until an item is available."
  ([]
   (->QueueTakeEffect :queue-take nil {} nil))
  ([queue-or-prev]
   (if (p/queue? queue-or-prev)
     (->QueueTakeEffect :queue-take nil {:queue queue-or-prev} queue-or-prev)
     (->QueueTakeEffect :queue-take queue-or-prev {} nil)))
  ([prev-effect queue]
   (->QueueTakeEffect :queue-take prev-effect {:queue queue} queue)))

(defrecord QueuePollEffect [tag prev-effect data queue]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [q (or queue val)]
        (if (p/queue? q)
          [nil (p/-queue-poll! q) context stack]
          [nil (fx/make-failure :async/invalid-queue {:queue q}) context stack])))))

(defn queue-poll>
  "Takes an item immediately from `queue` without parking; returns nil if empty."
  ([]
   (->QueuePollEffect :queue-poll nil {} nil))
  ([queue-or-prev]
   (if (p/queue? queue-or-prev)
     (->QueuePollEffect :queue-poll nil {:queue queue-or-prev} queue-or-prev)
     (->QueuePollEffect :queue-poll queue-or-prev {} nil)))
  ([prev-effect queue]
   (->QueuePollEffect :queue-poll prev-effect {:queue queue} queue)))

(defrecord QueueSizeEffect [tag prev-effect data queue]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [q (or queue val)]
        (if (p/queue? q)
          [nil (p/-queue-size q) context stack]
          [nil (fx/make-failure :async/invalid-queue {:queue q}) context stack])))))

(defn queue-size>
  "Returns the current count of buffered elements in `queue`."
  ([]
   (->QueueSizeEffect :queue-size nil {} nil))
  ([queue-or-prev]
   (if (p/queue? queue-or-prev)
     (->QueueSizeEffect :queue-size nil {:queue queue-or-prev} queue-or-prev)
     (->QueueSizeEffect :queue-size queue-or-prev {} nil)))
  ([prev-effect queue]
   (->QueueSizeEffect :queue-size prev-effect {:queue queue} queue)))

(defrecord QueueShutdownEffect [tag prev-effect data queue]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [q (or queue val)]
        (if (p/queue? q)
          [nil (p/-queue-shutdown! q) context stack]
          [nil (fx/make-failure :async/invalid-queue {:queue q}) context stack])))))

(defn queue-shutdown>
  "Closes `queue`, preventing new offers and waking waiting takers."
  ([]
   (->QueueShutdownEffect :queue-shutdown nil {} nil))
  ([queue-or-prev]
   (if (p/queue? queue-or-prev)
     (->QueueShutdownEffect :queue-shutdown nil {:queue queue-or-prev} queue-or-prev)
     (->QueueShutdownEffect :queue-shutdown queue-or-prev {} nil)))
  ([prev-effect queue]
   (->QueueShutdownEffect :queue-shutdown prev-effect {:queue queue} queue)))

;; ---------------------------------------------------------------------------
;; 2. Async Hub (Broadcast Pub/Sub) Implementation
;; ---------------------------------------------------------------------------

(deftype AsyncHub [capacity subscribers closed?]
  p/IHub
  (-hub-publish! [_ item]
    (if @closed?
      0
      (let [subs @subscribers
            count (count subs)]
        (doseq [sub subs]
          (p/-queue-offer! sub item))
        count)))
  (-hub-subscribe! [this]
    (if @closed?
      nil
      (let [sub (make-queue :sliding capacity)]
        (swap! subscribers conj sub)
        sub)))
  (-hub-unsubscribe! [_ sub-queue]
    (swap! subscribers disj sub-queue)
    (p/-queue-shutdown! sub-queue)
    true)
  (-hub-subscriber-count [_]
    (count @subscribers))
  (-hub-shutdown! [_]
    (when (compare-and-set! closed? false true)
      (doseq [sub @subscribers]
        (p/-queue-shutdown! sub))
      (reset! subscribers #{})
      true)))

(defn make-hub
  "Creates an IHub instance with the specified buffer capacity per subscriber."
  ([]
   (make-hub 32))
  ([capacity]
   (->AsyncHub capacity (atom #{}) (atom false))))

(defrecord HubConstructorEffect [tag prev-effect data capacity]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [cap (or capacity val 32)
            h   (make-hub cap)]
        [nil h context stack]))))

(defn hub-bounded>
  "Creates a broadcast Hub with bounded buffer capacity per subscriber."
  ([]
   (hub-bounded> 32))
  ([capacity-or-prev]
   (if (number? capacity-or-prev)
     (->HubConstructorEffect :hub-bounded nil {:capacity capacity-or-prev} capacity-or-prev)
     (->HubConstructorEffect :hub-bounded capacity-or-prev {} 32)))
  ([prev-effect capacity]
   (->HubConstructorEffect :hub-bounded prev-effect {:capacity capacity} capacity)))

(defrecord HubPublishEffect [tag prev-effect data hub item]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [h (or hub (:hub data))
            i (if (some? item) item val)]
        (if (p/hub? h)
          [nil (p/-hub-publish! h i) context stack]
          [nil (fx/make-failure :async/invalid-hub {:hub h}) context stack])))))

(defn hub-publish>
  "Publishes `item` to all active subscribers of `hub`. Returns count of subscribers."
  ([hub]
   (->HubPublishEffect :hub-publish nil {:hub hub} hub nil))
  ([hub-or-prev item-or-hub]
   (if (p/hub? hub-or-prev)
     (->HubPublishEffect :hub-publish nil {:hub hub-or-prev :item item-or-hub} hub-or-prev item-or-hub)
     (->HubPublishEffect :hub-publish hub-or-prev {:hub item-or-hub} item-or-hub nil)))
  ([prev-effect hub item]
   (->HubPublishEffect :hub-publish prev-effect {:hub hub :item item} hub item)))

(defrecord HubSubscribeEffect [tag prev-effect data hub]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [h (or hub val)]
        (if (p/hub? h)
          [nil (p/-hub-subscribe! h) context stack]
          [nil (fx/make-failure :async/invalid-hub {:hub h}) context stack])))))

(defn hub-subscribe>
  "Subscribes to `hub`, returning a new subscription `IQueue`."
  ([]
   (->HubSubscribeEffect :hub-subscribe nil {} nil))
  ([hub-or-prev]
   (if (p/hub? hub-or-prev)
     (->HubSubscribeEffect :hub-subscribe nil {:hub hub-or-prev} hub-or-prev)
     (->HubSubscribeEffect :hub-subscribe hub-or-prev {} nil)))
  ([prev-effect hub]
   (->HubSubscribeEffect :hub-subscribe prev-effect {:hub hub} hub)))

(defrecord HubUnsubscribeEffect [tag prev-effect data hub sub-queue]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [h (or hub (:hub data))
            s (or sub-queue val)]
        (if (and (p/hub? h) (p/queue? s))
          [nil (p/-hub-unsubscribe! h s) context stack]
          [nil (fx/make-failure :async/invalid-hub-unsubscribe {:hub h :sub-queue s}) context stack])))))

(defn hub-unsubscribe>
  "Unsubscribes `sub-queue` from `hub`."
  ([hub sub-queue]
   (if (fx/effect? hub)
     (->HubUnsubscribeEffect :hub-unsubscribe hub {:sub-queue sub-queue} nil sub-queue)
     (->HubUnsubscribeEffect :hub-unsubscribe nil {:hub hub :sub-queue sub-queue} hub sub-queue)))
  ([prev-effect hub sub-queue]
   (->HubUnsubscribeEffect :hub-unsubscribe prev-effect {:hub hub :sub-queue sub-queue} hub sub-queue)))

(defrecord HubSubscriberCountEffect [tag prev-effect data hub]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [h (or hub val)]
        (if (p/hub? h)
          [nil (p/-hub-subscriber-count h) context stack]
          [nil (fx/make-failure :async/invalid-hub {:hub h}) context stack])))))

(defn hub-subscriber-count>
  "Returns the number of active subscriber queues for `hub`."
  ([]
   (->HubSubscriberCountEffect :hub-subscriber-count nil {} nil))
  ([hub-or-prev]
   (if (p/hub? hub-or-prev)
     (->HubSubscriberCountEffect :hub-subscriber-count nil {:hub hub-or-prev} hub-or-prev)
     (->HubSubscriberCountEffect :hub-subscriber-count hub-or-prev {} nil)))
  ([prev-effect hub]
   (->HubSubscriberCountEffect :hub-subscriber-count prev-effect {:hub hub} hub)))

(defrecord HubShutdownEffect [tag prev-effect data hub]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [h (or hub val)]
        (if (p/hub? h)
          [nil (p/-hub-shutdown! h) context stack]
          [nil (fx/make-failure :async/invalid-hub {:hub h}) context stack])))))

(defn hub-shutdown>
  "Closes `hub` and shuts down all active subscriber queues."
  ([]
   (->HubShutdownEffect :hub-shutdown nil {} nil))
  ([hub-or-prev]
   (if (p/hub? hub-or-prev)
     (->HubShutdownEffect :hub-shutdown nil {:hub hub-or-prev} hub-or-prev)
     (->HubShutdownEffect :hub-shutdown hub-or-prev {} nil)))
  ([prev-effect hub]
   (->HubShutdownEffect :hub-shutdown prev-effect {:hub hub} hub)))

;; ---------------------------------------------------------------------------
;; 3. Async Deferred (Single-Assignment Promise) Implementation
;; ---------------------------------------------------------------------------

(deftype DeferredCell [^CompletableFuture cf]
  p/IDeferred
  (-deferred-succeed! [_ val]
    (.complete cf val))
  (-deferred-fail! [_ failure-or-err]
    (let [f (if (fx/failure? failure-or-err)
              failure-or-err
              (fx/make-failure :deferred/failed failure-or-err))]
      (.complete cf f)))
  (-deferred-await! [_]
    (try
      (.get cf)
      (catch Throwable e
        (let [c (or (.getCause e) e)]
          (if (fx/failure? c) c (fx/make-failure :deferred/defect c))))))
  (-deferred-await! [_ timeout-ms timeout-val]
    (try
      (.get cf (long timeout-ms) TimeUnit/MILLISECONDS)
      (catch java.util.concurrent.TimeoutException _
        timeout-val)
      (catch Throwable e
        (let [c (or (.getCause e) e)]
          (if (fx/failure? c) c (fx/make-failure :deferred/defect c))))))
  (-deferred-completed? [_]
    (.isDone cf))

  #?@(:clj
      [clojure.lang.IDeref
       (deref [this]
              (p/-deferred-await! this))

       clojure.lang.IBlockingDeref
       (deref [this timeout-ms timeout-val]
              (p/-deferred-await! this timeout-ms timeout-val))

       clojure.lang.IPending
       (isRealized [this]
                   (p/-deferred-completed? this))]))

(defn make-deferred
  "Creates a new uncompleted `IDeferred` instance."
  []
  (->DeferredCell (CompletableFuture.)))

(defrecord DeferredConstructorEffect [tag prev-effect data]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      [nil (make-deferred) context stack])))

(defn deferred>
  "Creates a new uncompleted single-assignment `IDeferred` instance."
  ([]
   (->DeferredConstructorEffect :deferred nil {}))
  ([prev-effect]
   (->DeferredConstructorEffect :deferred prev-effect {})))

(defrecord DeferredSucceedEffect [tag prev-effect data deferred value]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [d (or deferred (:deferred data))
            v (if (some? value) value val)]
        (if (p/deferred? d)
          [nil (p/-deferred-succeed! d v) context stack]
          [nil (fx/make-failure :async/invalid-deferred {:deferred d}) context stack])))))

(defn deferred-succeed>
  "Completes `deferred` successfully with `value`."
  ([deferred]
   (->DeferredSucceedEffect :deferred-succeed nil {:deferred deferred} deferred nil))
  ([deferred-or-prev val-or-deferred]
   (if (p/deferred? deferred-or-prev)
     (->DeferredSucceedEffect :deferred-succeed nil {:deferred deferred-or-prev :value val-or-deferred} deferred-or-prev val-or-deferred)
     (->DeferredSucceedEffect :deferred-succeed deferred-or-prev {:deferred val-or-deferred} val-or-deferred nil)))
  ([prev-effect deferred value]
   (->DeferredSucceedEffect :deferred-succeed prev-effect {:deferred deferred :value value} deferred value)))

(defrecord DeferredFailEffect [tag prev-effect data deferred failure-or-err]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [d (or deferred (:deferred data))
            f (if (some? failure-or-err) failure-or-err val)]
        (if (p/deferred? d)
          [nil (p/-deferred-fail! d f) context stack]
          [nil (fx/make-failure :async/invalid-deferred {:deferred d}) context stack])))))

(defn deferred-fail>
  "Completes `deferred` with a failure or error."
  ([deferred]
   (->DeferredFailEffect :deferred-fail nil {:deferred deferred} deferred nil))
  ([deferred-or-prev err-or-deferred]
   (if (p/deferred? deferred-or-prev)
     (->DeferredFailEffect :deferred-fail nil {:deferred deferred-or-prev :err err-or-deferred} deferred-or-prev err-or-deferred)
     (->DeferredFailEffect :deferred-fail deferred-or-prev {:deferred err-or-deferred} err-or-deferred nil)))
  ([prev-effect deferred failure-or-err]
   (->DeferredFailEffect :deferred-fail prev-effect {:deferred deferred :err failure-or-err} deferred failure-or-err)))

(defrecord DeferredAwaitEffect [tag prev-effect data deferred timeout-ms timeout-val]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [d (or deferred val)]
        (if (p/deferred? d)
          (let [res (if timeout-ms
                      (p/-deferred-await! d timeout-ms timeout-val)
                      (p/-deferred-await! d))]
            [nil res context stack])
          [nil (fx/make-failure :async/invalid-deferred {:deferred d}) context stack])))))

(defn deferred-await>
  "Awaits completion of `deferred` and yields its value or propagates its failure."
  ([]
   (->DeferredAwaitEffect :deferred-await nil {} nil nil nil))
  ([deferred-or-prev]
   (if (p/deferred? deferred-or-prev)
     (->DeferredAwaitEffect :deferred-await nil {:deferred deferred-or-prev} deferred-or-prev nil nil)
     (->DeferredAwaitEffect :deferred-await deferred-or-prev {} nil nil nil)))
  ([prev-or-deferred opts-or-deferred]
   (cond
     (p/deferred? prev-or-deferred)
     (if (map? opts-or-deferred)
       (->DeferredAwaitEffect :deferred-await nil (assoc opts-or-deferred :deferred prev-or-deferred) prev-or-deferred (:timeout-ms opts-or-deferred) (:timeout-val opts-or-deferred))
       (->DeferredAwaitEffect :deferred-await nil {:deferred prev-or-deferred} prev-or-deferred nil nil))

     (map? opts-or-deferred)
     (->DeferredAwaitEffect :deferred-await prev-or-deferred opts-or-deferred nil (:timeout-ms opts-or-deferred) (:timeout-val opts-or-deferred))

     :else
     (->DeferredAwaitEffect :deferred-await prev-or-deferred {:deferred opts-or-deferred} opts-or-deferred nil nil)))
  ([prev-effect deferred opts]
   (->DeferredAwaitEffect :deferred-await prev-effect (assoc (or opts {}) :deferred deferred) deferred (:timeout-ms opts) (:timeout-val opts))))

;; ---------------------------------------------------------------------------
;; 4. Async Semaphore Implementation
;; ---------------------------------------------------------------------------

(deftype SemaphoreCell [^Semaphore sem]
  p/ISemaphore
  (-sem-acquire! [_]
    (.acquire sem)
    true)
  (-sem-acquire! [_ n]
    (.acquire sem (int n))
    true)
  (-sem-try-acquire! [_]
    (.tryAcquire sem))
  (-sem-try-acquire! [_ n]
    (.tryAcquire sem (int n)))
  (-sem-try-acquire! [_ n timeout-ms]
    (.tryAcquire sem (int n) (long timeout-ms) TimeUnit/MILLISECONDS))
  (-sem-release! [_]
    (.release sem)
    true)
  (-sem-release! [_ n]
    (.release sem (int n))
    true)
  (-sem-available-permits [_]
    (.availablePermits sem)))

(defn make-semaphore
  "Creates a counting semaphore with `permits`."
  ([]
   (make-semaphore 1))
  ([permits]
   (->SemaphoreCell (Semaphore. (int permits)))))

(defrecord SemaphoreConstructorEffect [tag prev-effect data permits]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [p (or permits val 1)
            s (make-semaphore p)]
        [nil s context stack]))))

(defn semaphore>
  "Creates a counting semaphore initialized with `permits` (default: 1)."
  ([]
   (semaphore> 1))
  ([permits-or-prev]
   (if (number? permits-or-prev)
     (->SemaphoreConstructorEffect :semaphore nil {:permits permits-or-prev} permits-or-prev)
     (->SemaphoreConstructorEffect :semaphore permits-or-prev {} 1)))
  ([prev-effect permits]
   (->SemaphoreConstructorEffect :semaphore prev-effect {:permits permits} permits)))

(defrecord SemaphoreAcquireEffect [tag prev-effect data sem n]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [s (or sem val)
            num (or n 1)]
        (if (p/semaphore? s)
          [nil (p/-sem-acquire! s num) context stack]
          [nil (fx/make-failure :async/invalid-semaphore {:semaphore s}) context stack])))))

(defn semaphore-acquire>
  "Acquires `n` permits (default 1) from semaphore."
  ([]
   (->SemaphoreAcquireEffect :semaphore-acquire nil {} nil 1))
  ([sem-or-prev]
   (if (p/semaphore? sem-or-prev)
     (->SemaphoreAcquireEffect :semaphore-acquire nil {:sem sem-or-prev} sem-or-prev 1)
     (->SemaphoreAcquireEffect :semaphore-acquire sem-or-prev {} nil 1)))
  ([sem-or-prev n-or-sem]
   (if (p/semaphore? sem-or-prev)
     (->SemaphoreAcquireEffect :semaphore-acquire nil {:sem sem-or-prev :n n-or-sem} sem-or-prev n-or-sem)
     (->SemaphoreAcquireEffect :semaphore-acquire sem-or-prev {:sem n-or-sem} n-or-sem 1)))
  ([prev-effect sem n]
   (->SemaphoreAcquireEffect :semaphore-acquire prev-effect {:sem sem :n n} sem n)))

(defrecord SemaphoreReleaseEffect [tag prev-effect data sem n]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [s (or sem val)
            num (or n 1)]
        (if (p/semaphore? s)
          [nil (p/-sem-release! s num) context stack]
          [nil (fx/make-failure :async/invalid-semaphore {:semaphore s}) context stack])))))

(defn semaphore-release>
  "Releases `n` permits (default 1) back to semaphore."
  ([]
   (->SemaphoreReleaseEffect :semaphore-release nil {} nil 1))
  ([sem-or-prev]
   (if (p/semaphore? sem-or-prev)
     (->SemaphoreReleaseEffect :semaphore-release nil {:sem sem-or-prev} sem-or-prev 1)
     (->SemaphoreReleaseEffect :semaphore-release sem-or-prev {} nil 1)))
  ([sem-or-prev n-or-sem]
   (if (p/semaphore? sem-or-prev)
     (->SemaphoreReleaseEffect :semaphore-release nil {:sem sem-or-prev :n n-or-sem} sem-or-prev n-or-sem)
     (->SemaphoreReleaseEffect :semaphore-release sem-or-prev {:sem n-or-sem} n-or-sem 1)))
  ([prev-effect sem n]
   (->SemaphoreReleaseEffect :semaphore-release prev-effect {:sem sem :n n} sem n)))

(defrecord SemaphoreAvailablePermitsEffect [tag prev-effect data sem]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [s (or sem val)]
        (if (p/semaphore? s)
          [nil (p/-sem-available-permits s) context stack]
          [nil (fx/make-failure :async/invalid-semaphore {:semaphore s}) context stack])))))

(defn semaphore-available-permits>
  "Returns the number of permits currently available in semaphore."
  ([]
   (->SemaphoreAvailablePermitsEffect :semaphore-available-permits nil {} nil))
  ([sem-or-prev]
   (if (p/semaphore? sem-or-prev)
     (->SemaphoreAvailablePermitsEffect :semaphore-available-permits nil {:sem sem-or-prev} sem-or-prev)
     (->SemaphoreAvailablePermitsEffect :semaphore-available-permits sem-or-prev {} nil)))
  ([prev-effect sem]
   (->SemaphoreAvailablePermitsEffect :semaphore-available-permits prev-effect {:sem sem} sem)))

(defn with-permit>
  "Acquires `n` permits (default 1) from `sem`, runs `effect` in a protected resource bracket,
   and guarantees release of permits upon completion, failure, or cancellation."
  ([sem effect]
   (with-permit> sem 1 effect))
  ([sem n effect]
   (fx/acquire-release>
    (semaphore-acquire> sem n)
    (fn [_] effect)
    (fn [_] (semaphore-release> sem n)))))

;; ---------------------------------------------------------------------------
;; 5. Async Countdown Latch Implementation
;; ---------------------------------------------------------------------------

(deftype LatchCell [^CountDownLatch latch initial-count]
  p/ILatch
  (-latch-count-down! [_]
    (.countDown latch)
    true)
  (-latch-await! [_]
    (.await latch)
    true)
  (-latch-await! [_ timeout-ms]
    (.await latch (long timeout-ms) TimeUnit/MILLISECONDS))
  (-latch-get-count [_]
    (.getCount latch)))

(defn make-latch
  "Creates a countdown latch with initial `count`."
  [count]
  (->LatchCell (CountDownLatch. (int count)) count))

(defrecord LatchConstructorEffect [tag prev-effect data count]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [c (or count val 1)
            l (make-latch c)]
        [nil l context stack]))))

(defn countdown-latch>
  "Creates a countdown latch initialized with `count`."
  ([count-or-prev]
   (if (number? count-or-prev)
     (->LatchConstructorEffect :countdown-latch nil {:count count-or-prev} count-or-prev)
     (->LatchConstructorEffect :countdown-latch count-or-prev {} 1)))
  ([prev-effect count]
   (->LatchConstructorEffect :countdown-latch prev-effect {:count count} count)))

(defrecord LatchCountDownEffect [tag prev-effect data latch]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [l (or latch val)]
        (if (p/latch? l)
          [nil (p/-latch-count-down! l) context stack]
          [nil (fx/make-failure :async/invalid-latch {:latch l}) context stack])))))

(defn latch-count-down>
  "Decrements the count of `latch`, releasing all awaiting callers when count reaches 0."
  ([]
   (->LatchCountDownEffect :latch-count-down nil {} nil))
  ([latch-or-prev]
   (if (p/latch? latch-or-prev)
     (->LatchCountDownEffect :latch-count-down nil {:latch latch-or-prev} latch-or-prev)
     (->LatchCountDownEffect :latch-count-down latch-or-prev {} nil)))
  ([prev-effect latch]
   (->LatchCountDownEffect :latch-count-down prev-effect {:latch latch} latch)))

(defrecord LatchAwaitEffect [tag prev-effect data latch timeout-ms]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [l (or latch val)]
        (if (p/latch? l)
          (let [res (if timeout-ms
                      (p/-latch-await! l timeout-ms)
                      (p/-latch-await! l))]
            [nil res context stack])
          [nil (fx/make-failure :async/invalid-latch {:latch l}) context stack])))))

(defn latch-await>
  "Awaits until `latch` count reaches 0 or timeout expires."
  ([]
   (->LatchAwaitEffect :latch-await nil {} nil nil))
  ([latch-or-prev]
   (if (p/latch? latch-or-prev)
     (->LatchAwaitEffect :latch-await nil {:latch latch-or-prev} latch-or-prev nil)
     (->LatchAwaitEffect :latch-await latch-or-prev {} nil nil)))
  ([prev-or-latch timeout-or-latch]
   (if (p/latch? prev-or-latch)
     (->LatchAwaitEffect :latch-await nil {:latch prev-or-latch :timeout-ms timeout-or-latch} prev-or-latch timeout-or-latch)
     (->LatchAwaitEffect :latch-await prev-or-latch {:latch timeout-or-latch} timeout-or-latch nil)))
  ([prev-effect latch timeout-ms]
   (->LatchAwaitEffect :latch-await prev-effect {:latch latch :timeout-ms timeout-ms} latch timeout-ms)))

(defrecord LatchGetCountEffect [tag prev-effect data latch]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [l (or latch val)]
        (if (p/latch? l)
          [nil (p/-latch-get-count l) context stack]
          [nil (fx/make-failure :async/invalid-latch {:latch l}) context stack])))))

(defn latch-get-count>
  "Returns the remaining count of `latch`."
  ([]
   (->LatchGetCountEffect :latch-get-count nil {} nil))
  ([latch-or-prev]
   (if (p/latch? latch-or-prev)
     (->LatchGetCountEffect :latch-get-count nil {:latch latch-or-prev} latch-or-prev)
     (->LatchGetCountEffect :latch-get-count latch-or-prev {} nil)))
  ([prev-effect latch]
   (->LatchGetCountEffect :latch-get-count prev-effect {:latch latch} latch)))

;; ---------------------------------------------------------------------------
;; 6. Async Ref (Thread-Safe Atomic Effect State Cell) Implementation
;; ---------------------------------------------------------------------------

(deftype RefCell [atom-ref]
  p/IRef
  (-ref-get [_]
    @atom-ref)
  (-ref-set! [_ new-val]
    (reset! atom-ref new-val))
  (-ref-update! [_ f]
    (swap! atom-ref f))

  #?@(:clj
      [clojure.lang.IDeref
       (deref [_]
              @atom-ref)

       clojure.lang.IAtom
       (swap [_ f]
             (swap! atom-ref f))
       (swap [_ f arg]
             (swap! atom-ref f arg))
       (swap [_ f arg1 arg2]
             (swap! atom-ref f arg1 arg2))
       (swap [_ f arg1 arg2 args]
             (apply swap! atom-ref f arg1 arg2 args))
       (compareAndSet [_ oldv newv]
                      (compare-and-set! atom-ref oldv newv))
       (reset [_ newv]
              (reset! atom-ref newv))]))

(defn make-ref
  "Creates an IRef instance initialized with `initial-val`."
  ([]
   (make-ref nil))
  ([initial-val]
   (->RefCell (atom initial-val))))

(defrecord RefConstructorEffect [tag prev-effect data initial-val]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [v (if (some? initial-val) initial-val val)
            r (make-ref v)]
        [nil r context stack]))))

(defn ref>
  "Creates a thread-safe atomic effect state cell initialized with `initial-val`."
  ([]
   (->RefConstructorEffect :ref nil {} nil))
  ([initial-val-or-prev]
   (if (fx/effect? initial-val-or-prev)
     (->RefConstructorEffect :ref initial-val-or-prev {} nil)
     (->RefConstructorEffect :ref nil {:initial-val initial-val-or-prev} initial-val-or-prev)))
  ([prev-effect initial-val]
   (->RefConstructorEffect :ref prev-effect {:initial-val initial-val} initial-val)))

(defrecord RefGetEffect [tag prev-effect data ref]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [r (or ref val)]
        (if (p/ref? r)
          [nil (p/-ref-get r) context stack]
          [nil (fx/make-failure :async/invalid-ref {:ref r}) context stack])))))

(defn ref-get>
  "Retrieves the current value of `ref`."
  ([]
   (->RefGetEffect :ref-get nil {} nil))
  ([ref-or-prev]
   (if (p/ref? ref-or-prev)
     (->RefGetEffect :ref-get nil {:ref ref-or-prev} ref-or-prev)
     (->RefGetEffect :ref-get ref-or-prev {} nil)))
  ([prev-effect ref]
   (->RefGetEffect :ref-get prev-effect {:ref ref} ref)))

(defrecord RefSetEffect [tag prev-effect data ref new-val]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [r (or ref (:ref data))
            v (if (some? new-val) new-val val)]
        (if (p/ref? r)
          [nil (p/-ref-set! r v) context stack]
          [nil (fx/make-failure :async/invalid-ref {:ref r}) context stack])))))

(defn ref-set>
  "Sets `ref` to `new-val` and yields `new-val`."
  ([ref]
   (->RefSetEffect :ref-set nil {:ref ref} ref nil))
  ([ref-or-prev val-or-ref]
   (if (p/ref? ref-or-prev)
     (->RefSetEffect :ref-set nil {:ref ref-or-prev :new-val val-or-ref} ref-or-prev val-or-ref)
     (->RefSetEffect :ref-set ref-or-prev {:ref val-or-ref} val-or-ref nil)))
  ([prev-effect ref new-val]
   (->RefSetEffect :ref-set prev-effect {:ref ref :new-val new-val} ref new-val)))

(defrecord RefUpdateEffect [tag prev-effect data ref f]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [r (or ref (:ref data))
            func (if (fn? f) f val)]
        (if (and (p/ref? r) (fn? func))
          [nil (p/-ref-update! r func) context stack]
          [nil (fx/make-failure :async/invalid-ref-update {:ref r :f func}) context stack])))))

(defn ref-update>
  "Atomically updates `ref` using function `f` and yields the new value."
  ([ref f]
   (if (fx/effect? ref)
     (->RefUpdateEffect :ref-update ref {:f f} nil f)
     (->RefUpdateEffect :ref-update nil {:ref ref :f f} ref f)))
  ([prev-effect ref f]
   (->RefUpdateEffect :ref-update prev-effect {:ref ref :f f} ref f)))
