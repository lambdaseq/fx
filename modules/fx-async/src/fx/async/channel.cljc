(ns fx.async.channel
  "First-class core.async channel bridging and stream combinators for fx."
  (:require [clojure.core.async :as async]
            [fx.async.fiber :as fiber]
            [fx.core :as fx])
  #?(:clj (:import (java.util.concurrent CompletableFuture
                                         CountDownLatch
                                         TimeUnit))))

;; ---------------------------------------------------------------------------
;; Step Effect Frame
;; ---------------------------------------------------------------------------

(defrecord StepEffectFrame [effect]
  fx.core.IContinuation
  (-resume [_ val context stack]
    (fx.core/-step effect val context stack)))

;; ---------------------------------------------------------------------------
;; Buffer Helpers
;; ---------------------------------------------------------------------------

(defn- resolve-buffer [buf]
  (cond
    (nil? buf) nil
    (number? buf) (if (pos? buf) (async/buffer (int buf)) nil)
    (map? buf) (let [n (int (or (:n buf) (:size buf) 10))]
                 (case (:buffer buf)
                   :sliding  (async/sliding-buffer n)
                   :dropping (async/dropping-buffer n)
                   :fixed    (async/buffer n)
                   (async/buffer n)))
    :else buf))

;; ---------------------------------------------------------------------------
;; chan> Effect Constructor
;; ---------------------------------------------------------------------------

(defrecord ChanEffect [tag prev-effect data buf-or-n xform ex-handler]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [b (resolve-buffer (or buf-or-n val))
            ch (cond
                 (and (some? xform) (some? ex-handler)) (async/chan b xform ex-handler)
                 (some? xform)                         (async/chan b xform)
                 (some? b)                             (async/chan b)
                 :else                                 (async/chan))]
        [nil ch context stack]))))

(defn chan>
  "Creates a new core.async channel within an effect pipeline.
   Supports buffer configurations (number, `:sliding`, `:dropping`, `:fixed`), transducers, and exception handlers."
  ([]
   (->ChanEffect :chan nil {} nil nil nil))
  ([buf-or-n]
   (if (fx/effect? buf-or-n)
     (->ChanEffect :chan buf-or-n {} nil nil nil)
     (->ChanEffect :chan nil {:buffer buf-or-n} buf-or-n nil nil)))
  ([buf-or-n xform]
   (->ChanEffect :chan nil {:buffer buf-or-n :xform xform} buf-or-n xform nil))
  ([buf-or-n xform ex-handler]
   (->ChanEffect :chan nil {:buffer buf-or-n :xform xform :ex-handler ex-handler} buf-or-n xform ex-handler)))

;; ---------------------------------------------------------------------------
;; chan-put> Effect Combinator
;; ---------------------------------------------------------------------------

(defn- exec-chan-put [ch val context]
  #?(:clj
     (let [cf (CompletableFuture.)
           parent-fiber (:fiber context)]
       (async/put! ch val
         (fn [accepted?]
           (.complete cf (boolean accepted?))))
       (when parent-fiber
         (fiber/add-interrupt-handler! parent-fiber
           (fn [_reason]
             (when-not (.isDone cf)
               (.complete cf (fx/make-failure :async/channel-put-interrupted {:channel ch :val val}))))))
       (try
         (.get cf)
         (catch Throwable e
           (let [c (or (.getCause e) e)]
             (if (fx/failure? c) c (fx/make-failure :async/defect c))))))
     :cljs
     (async/put! ch val)))

(defrecord ChanPutEffect [tag prev-effect data target-chan target-val]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [ch (or target-chan (:chan data))
            v  (if (some? target-val) target-val val)]
        (if (nil? ch)
          [nil (fx/make-failure :async/nil-channel {:val v}) context stack]
          (if (fx/failure? v)
            [nil v context stack]
            (let [res (exec-chan-put ch v context)]
              [nil res context stack])))))))

(defn chan-put>
  "Puts `val` onto channel `chan`, parking without blocking OS threads.
   Supports threading: `(-> val (chan-put> ch))` or `(chan-put> ch val)`."
  ([chan]
   (->ChanPutEffect :chan-put nil {:chan chan} chan nil))
  ([chan-or-prev val-or-chan]
   (if (fx/effect? chan-or-prev)
     (->ChanPutEffect :chan-put chan-or-prev {:chan val-or-chan} val-or-chan nil)
     (->ChanPutEffect :chan-put nil {:chan chan-or-prev :val val-or-chan} chan-or-prev val-or-chan)))
  ([prev-effect chan val]
   (->ChanPutEffect :chan-put prev-effect {:chan chan :val val} chan val)))

;; ---------------------------------------------------------------------------
;; chan-take> Effect Combinator
;; ---------------------------------------------------------------------------

(defn- exec-chan-take [ch timeout-ms timeout-val context]
  #?(:clj
     (let [cf (CompletableFuture.)
           parent-fiber (:fiber context)]
       (if (and (number? timeout-ms) (pos? timeout-ms))
         (let [timeout-ch (async/timeout (long timeout-ms))
               [val port] (async/alts!! [ch timeout-ch] :priority true)]
           (if (identical? port timeout-ch)
             timeout-val
             val))
         (do
           (async/take! ch
             (fn [val]
               (.complete cf val)))
           (when parent-fiber
             (fiber/add-interrupt-handler! parent-fiber
               (fn [_reason]
                 (when-not (.isDone cf)
                   (.complete cf (fx/make-failure :async/channel-take-interrupted {:channel ch}))))))
           (try
             (.get cf)
             (catch Throwable e
               (let [c (or (.getCause e) e)]
                 (if (fx/failure? c) c (fx/make-failure :async/defect c))))))))
     :cljs
     nil))

(defrecord ChanTakeEffect [tag prev-effect data target-chan timeout-ms timeout-val]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [ch (or target-chan val)]
        (if (nil? ch)
          [nil (fx/make-failure :async/nil-channel {}) context stack]
          (if (fx/failure? ch)
            [nil ch context stack]
            (let [res (exec-chan-take ch timeout-ms timeout-val context)]
              [nil res context stack])))))))

(defn chan-take>
  "Takes a single element from channel `chan`, parking without blocking OS threads.
   Optional `opts` map may contain `:timeout-ms` and `:timeout-val`."
  ([]
   (->ChanTakeEffect :chan-take nil {} nil nil nil))
  ([chan-or-prev]
   (if (fx/effect? chan-or-prev)
     (->ChanTakeEffect :chan-take chan-or-prev {} nil nil nil)
     (->ChanTakeEffect :chan-take nil {:chan chan-or-prev} chan-or-prev nil nil)))
  ([prev-or-chan opts-or-timeout]
   (cond
     (fx/effect? prev-or-chan)
     (if (map? opts-or-timeout)
       (->ChanTakeEffect :chan-take prev-or-chan opts-or-timeout nil (:timeout-ms opts-or-timeout) (:timeout-val opts-or-timeout))
       (->ChanTakeEffect :chan-take prev-or-chan {:chan opts-or-timeout} opts-or-timeout nil nil))

     (map? opts-or-timeout)
     (->ChanTakeEffect :chan-take nil (assoc opts-or-timeout :chan prev-or-chan) prev-or-chan (:timeout-ms opts-or-timeout) (:timeout-val opts-or-timeout))

     :else
     (->ChanTakeEffect :chan-take nil {:chan prev-or-chan :timeout-ms opts-or-timeout} prev-or-chan opts-or-timeout nil)))
  ([prev-effect chan opts]
   (->ChanTakeEffect :chan-take prev-effect (assoc (or opts {}) :chan chan) chan (:timeout-ms opts) (:timeout-val opts))))

;; ---------------------------------------------------------------------------
;; chan-close> Effect Combinator
;; ---------------------------------------------------------------------------

(defrecord ChanCloseEffect [tag prev-effect data target-chan]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [ch (or target-chan val)]
        (when ch
          (async/close! ch))
        [nil nil context stack]))))

(defn chan-close>
  "Closes channel `chan` and completes with nil."
  ([]
   (->ChanCloseEffect :chan-close nil {} nil))
  ([chan-or-prev]
   (if (fx/effect? chan-or-prev)
     (->ChanCloseEffect :chan-close chan-or-prev {} nil)
     (->ChanCloseEffect :chan-close nil {:chan chan-or-prev} chan-or-prev)))
  ([prev-effect chan]
   (->ChanCloseEffect :chan-close prev-effect {:chan chan} chan)))

;; ---------------------------------------------------------------------------
;; chan-alts> Effect Combinator
;; ---------------------------------------------------------------------------

(defn- exec-chan-alts [ports opts context]
  #?(:clj
     (let [priority? (get opts :priority false)
           timeout-ms (get opts :timeout-ms nil)
           default-val (get opts :default nil)
           has-default? (contains? opts :default)
           ports-vec (cond-> (vec ports)
                       (and timeout-ms (pos? timeout-ms))
                       (conj (async/timeout (long timeout-ms))))]
       (if has-default?
         (async/alts!! ports-vec :priority priority? :default default-val)
         (async/alts!! ports-vec :priority priority?)))
     :cljs
     nil))

(defrecord ChanAltsEffect [tag prev-effect data ports opts]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [target-ports (or ports val)]
        (if (fx/failure? target-ports)
          [nil target-ports context stack]
          (let [res (exec-chan-alts target-ports opts context)]
            [nil res context stack]))))))

(defn chan-alts>
  "Races multiple channel read/write ports, returning `[val-or-ret selected-port]`.
   Options map:
     `:priority`   - boolean whether to preserve ports order priority (default: false)
     `:default`    - fallback default value if all ports would block
     `:timeout-ms` - maximum timeout in ms"
  ([ports]
   (if (sequential? ports)
     (->ChanAltsEffect :chan-alts nil {:ports (vec ports)} (vec ports) {})
     (->ChanAltsEffect :chan-alts ports {} nil {})))
  ([prev-or-ports opts-or-ports]
   (if (sequential? prev-or-ports)
     (->ChanAltsEffect :chan-alts nil {:ports (vec prev-or-ports) :opts opts-or-ports} (vec prev-or-ports) (or opts-or-ports {}))
     (if (sequential? opts-or-ports)
       (->ChanAltsEffect :chan-alts prev-or-ports {:ports (vec opts-or-ports)} (vec opts-or-ports) {})
       (->ChanAltsEffect :chan-alts prev-or-ports {:opts opts-or-ports} nil (or opts-or-ports {})))))
  ([prev-effect ports opts]
   (->ChanAltsEffect :chan-alts prev-effect {:ports (vec ports) :opts opts} (vec ports) (or opts {}))))

;; ---------------------------------------------------------------------------
;; chan-drain> & chan-pipe> Stream Combinators
;; ---------------------------------------------------------------------------

(defn- exec-chan-drain [ch context]
  #?(:clj
     (let [cf (CompletableFuture.)
           results (atom [])
           parent-fiber (:fiber context)]
       (letfn [(step []
                 (async/take! ch
                   (fn [val]
                     (if (nil? val)
                       (.complete cf @results)
                       (do
                         (swap! results conj val)
                         (step))))))]
         (step))
       (when parent-fiber
         (fiber/add-interrupt-handler! parent-fiber
           (fn [_reason]
             (when-not (.isDone cf)
               (.complete cf (fx/make-failure :async/channel-drain-interrupted {:channel ch}))))))
       (try
         (.get cf)
         (catch Throwable e
           (let [c (or (.getCause e) e)]
             (if (fx/failure? c) c (fx/make-failure :async/defect c))))))
     :cljs
     nil))

(defrecord ChanDrainEffect [tag prev-effect data target-chan]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [ch (or target-chan val)]
        (if (nil? ch)
          [nil (fx/make-failure :async/nil-channel {}) context stack]
          (if (fx/failure? ch)
            [nil ch context stack]
            (let [res (exec-chan-drain ch context)]
              [nil res context stack])))))))

(defn chan-drain>
  "Consumes all values from `chan` until it is closed and returns a vector of collected items."
  ([]
   (->ChanDrainEffect :chan-drain nil {} nil))
  ([chan-or-prev]
   (if (fx/effect? chan-or-prev)
     (->ChanDrainEffect :chan-drain chan-or-prev {} nil)
     (->ChanDrainEffect :chan-drain nil {:chan chan-or-prev} chan-or-prev)))
  ([prev-effect chan]
   (->ChanDrainEffect :chan-drain prev-effect {:chan chan} chan)))

(defrecord ChanPipeEffect [tag prev-effect data from-chan to-chan close?]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [from (or from-chan val)
            to   to-chan]
        (if (or (nil? from) (nil? to))
          [nil (fx/make-failure :async/nil-channel {:from from :to to}) context stack]
          (do
            (async/pipe from to (if (some? close?) (boolean close?) true))
            [nil to context stack]))))))

(defn chan-pipe>
  "Pipes elements from `from-chan` into `to-chan`. Closes `to-chan` when `from-chan` closes unless `close?` is false."
  ([from-chan to-chan]
   (if (fx/effect? from-chan)
     (->ChanPipeEffect :chan-pipe from-chan {:to to-chan} nil to-chan true)
     (->ChanPipeEffect :chan-pipe nil {:from from-chan :to to-chan} from-chan to-chan true)))
  ([from-chan to-chan close?]
   (if (fx/effect? from-chan)
     (->ChanPipeEffect :chan-pipe from-chan {:to to-chan :close? close?} nil to-chan close?)
     (->ChanPipeEffect :chan-pipe nil {:from from-chan :to to-chan :close? close?} from-chan to-chan close?)))
  ([prev-effect from-chan to-chan close?]
   (->ChanPipeEffect :chan-pipe prev-effect {:from from-chan :to to-chan :close? close?} from-chan to-chan close?)))

;; ---------------------------------------------------------------------------
;; Pub/Sub Broadcast Effects
;; ---------------------------------------------------------------------------

(defrecord ChanPubEffect [tag prev-effect data in-chan topic-fn buf-fn]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [ch (or in-chan val)]
        (if (nil? ch)
          [nil (fx/make-failure :async/nil-channel {}) context stack]
          (let [p (if buf-fn
                    (async/pub ch topic-fn (fn [topic] (resolve-buffer (buf-fn topic))))
                    (async/pub ch topic-fn))]
            [nil p context stack]))))))

(defn chan-pub>
  "Creates a core.async publication from channel `in-chan` using routing function `topic-fn`."
  ([topic-fn]
   (->ChanPubEffect :chan-pub nil {:topic-fn topic-fn} nil topic-fn nil))
  ([in-chan topic-fn]
   (if (fx/effect? in-chan)
     (->ChanPubEffect :chan-pub in-chan {:topic-fn topic-fn} nil topic-fn nil)
     (->ChanPubEffect :chan-pub nil {:in-chan in-chan :topic-fn topic-fn} in-chan topic-fn nil)))
  ([in-chan topic-fn buf-fn]
   (if (fx/effect? in-chan)
     (->ChanPubEffect :chan-pub in-chan {:topic-fn topic-fn :buf-fn buf-fn} nil topic-fn buf-fn)
     (->ChanPubEffect :chan-pub nil {:in-chan in-chan :topic-fn topic-fn :buf-fn buf-fn} in-chan topic-fn buf-fn)))
  ([prev-effect in-chan topic-fn buf-fn]
   (->ChanPubEffect :chan-pub prev-effect {:in-chan in-chan :topic-fn topic-fn :buf-fn buf-fn} in-chan topic-fn buf-fn)))

(defrecord ChanSubEffect [tag prev-effect data publication topic out-chan close?]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [pub (or publication val)]
        (if (nil? pub)
          [nil (fx/make-failure :async/nil-publication {}) context stack]
          (do
            (async/sub pub topic out-chan (if (some? close?) (boolean close?) true))
            [nil out-chan context stack]))))))

(defn chan-sub>
  "Subscribes `out-chan` to `publication` on `topic`."
  ([publication topic out-chan]
   (if (fx/effect? publication)
     (->ChanSubEffect :chan-sub publication {:topic topic :out-chan out-chan} nil topic out-chan true)
     (->ChanSubEffect :chan-sub nil {:publication publication :topic topic :out-chan out-chan} publication topic out-chan true)))
  ([publication topic out-chan close?]
   (if (fx/effect? publication)
     (->ChanSubEffect :chan-sub publication {:topic topic :out-chan out-chan :close? close?} nil topic out-chan close?)
     (->ChanSubEffect :chan-sub nil {:publication publication :topic topic :out-chan out-chan :close? close?} publication topic out-chan close?)))
  ([prev-effect publication topic out-chan close?]
   (->ChanSubEffect :chan-sub prev-effect {:publication publication :topic topic :out-chan out-chan :close? close?} publication topic out-chan close?)))

(defrecord ChanUnsubEffect [tag prev-effect data publication topic out-chan]
  fx.core.ITagged
  (tag [_] tag)
  fx.core.IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [pub (or publication val)]
        (if (nil? pub)
          [nil (fx/make-failure :async/nil-publication {}) context stack]
          (do
            (if (some? topic)
              (async/unsub pub topic out-chan)
              (async/unsub-all pub out-chan))
            [nil true context stack]))))))

(defn chan-unsub>
  "Unsubscribes `out-chan` from `publication` on `topic` (or all topics if nil)."
  ([publication out-chan]
   (->ChanUnsubEffect :chan-unsub nil {:publication publication :out-chan out-chan} publication nil out-chan))
  ([publication topic out-chan]
   (if (fx/effect? publication)
     (->ChanUnsubEffect :chan-unsub publication {:topic topic :out-chan out-chan} nil topic out-chan)
     (->ChanUnsubEffect :chan-unsub nil {:publication publication :topic topic :out-chan out-chan} publication topic out-chan)))
  ([prev-effect publication topic out-chan]
   (->ChanUnsubEffect :chan-unsub prev-effect {:publication publication :topic topic :out-chan out-chan} publication topic out-chan)))
