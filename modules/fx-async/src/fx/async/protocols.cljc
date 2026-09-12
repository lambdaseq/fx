(ns fx.async.protocols
  "Core protocols for fibers, queues, coordination, and async communication in fx.")

;; ---------------------------------------------------------------------------
;; Fiber Protocol
;; ---------------------------------------------------------------------------

(defprotocol IFiber
  "Protocol representing a lightweight, structured fiber of computation."
  (-fiber-id [this]
    "Returns the unique identifier of the fiber.")
  (-fiber-status [this]
    "Returns the current lifecycle status keyword: :running, :succeeded, :failed, or :interrupted.")
  (-fiber-status-atom [this]
    "Returns the internal atomic reference containing the fiber's status keyword.")
  (-fiber-result [this]
    "Returns a CompletableFuture or promise holding the eventual value or failure.")
  (-fiber-children [this]
    "Returns an atom containing the set of active child fibers.")
  (-fiber-interrupt! [this] [this reason]
    "Interrupts the fiber and cascades cancellation across all active child fibers."))

(defn fiber?
  "Returns true if `x` satisfies the IFiber protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.IFiber x)
     :cljs (satisfies? IFiber x)))

(defn fiber-id [fiber] (-fiber-id fiber))
(defn fiber-status [fiber] (-fiber-status fiber))
(defn fiber-status-atom [fiber] (-fiber-status-atom fiber))
(defn fiber-result [fiber] (-fiber-result fiber))
(defn fiber-children [fiber] (-fiber-children fiber))

;; ---------------------------------------------------------------------------
;; Queue Protocol
;; ---------------------------------------------------------------------------

(defprotocol IQueue
  "Protocol for backpressure-aware, multi-producer multi-consumer async queues."
  (-queue-offer! [this item]
    "Offers an item to the queue. Returns true if accepted, false if capacity full and non-blocking.")
  (-queue-take! [this]
    "Takes an item from the queue, parking or blocking until an element is available or queue closed.")
  (-queue-poll! [this]
    "Takes an item immediately without parking; returns nil if empty.")
  (-queue-size [this]
    "Returns the current count of buffered elements in the queue.")
  (-queue-capacity [this]
    "Returns the maximum capacity of the queue, or nil if unbounded.")
  (-queue-shutdown! [this]
    "Closes the queue, preventing new offers and waking pending takes/offers."))

(defn queue?
  "Returns true if `x` satisfies the IQueue protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.IQueue x)
     :cljs (satisfies? IQueue x)))

;; ---------------------------------------------------------------------------
;; Hub Protocol (Broadcast Pub/Sub)
;; ---------------------------------------------------------------------------

(defprotocol IHub
  "Protocol for multi-producer multi-consumer broadcast Hubs."
  (-hub-publish! [this item]
    "Publishes an item to all active subscribers. Returns count of active recipients.")
  (-hub-subscribe! [this]
    "Creates and returns a new subscription Queue for a subscriber.")
  (-hub-unsubscribe! [this sub-queue]
    "Unregisters a subscriber queue from receiving future broadcast messages.")
  (-hub-subscriber-count [this]
    "Returns the number of currently active subscriber queues.")
  (-hub-shutdown! [this]
    "Closes the Hub and shuts down all active subscription queues."))

(defn hub?
  "Returns true if `x` satisfies the IHub protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.IHub x)
     :cljs (satisfies? IHub x)))

;; ---------------------------------------------------------------------------
;; Deferred Protocol (Single-Assignment Promise)
;; ---------------------------------------------------------------------------

(defprotocol IDeferred
  "Protocol for single-assignment async deferred values."
  (-deferred-succeed! [this val]
    "Completes the deferred successfully with `val`. Returns true if first to complete.")
  (-deferred-fail! [this failure-or-err]
    "Completes the deferred with a failure or exception. Returns true if first to complete.")
  (-deferred-await! [this] [this timeout-ms timeout-val]
    "Awaits completion of the deferred, returning the value or throwing if failed/timed out.")
  (-deferred-completed? [this]
    "Returns true if the deferred has already been completed."))

(defn deferred?
  "Returns true if `x` satisfies the IDeferred protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.IDeferred x)
     :cljs (satisfies? IDeferred x)))

;; ---------------------------------------------------------------------------
;; Semaphore Protocol
;; ---------------------------------------------------------------------------

(defprotocol ISemaphore
  "Protocol for counting semaphores controlling concurrent resource access."
  (-sem-acquire! [this] [this n]
    "Acquires `n` permits (default 1), blocking or parking until available.")
  (-sem-try-acquire! [this] [this n] [this n timeout-ms]
    "Attempts to acquire `n` permits. Returns true if acquired, false otherwise.")
  (-sem-release! [this] [this n]
    "Releases `n` permits (default 1) back to the semaphore.")
  (-sem-available-permits [this]
    "Returns the number of permits currently available."))

(defn semaphore?
  "Returns true if `x` satisfies the ISemaphore protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.ISemaphore x)
     :cljs (satisfies? ISemaphore x)))

;; ---------------------------------------------------------------------------
;; Countdown Latch Protocol
;; ---------------------------------------------------------------------------

(defprotocol ILatch
  "Protocol for countdown synchronization latches."
  (-latch-count-down! [this]
    "Decrements the latch count, releasing all waiting callers when count reaches 0.")
  (-latch-await! [this] [this timeout-ms]
    "Awaits until the latch count reaches 0, or timeout expires. Returns boolean.")
  (-latch-get-count [this]
    "Returns the current remaining count."))

(defn latch?
  "Returns true if `x` satisfies the ILatch protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.ILatch x)
     :cljs (satisfies? ILatch x)))

;; ---------------------------------------------------------------------------
;; Async Ref Protocol
;; ---------------------------------------------------------------------------

(defprotocol IRef
  "Protocol for thread-safe atomic effect state cells."
  (-ref-get [this]
    "Returns the current value of the Ref.")
  (-ref-set! [this new-val]
    "Sets the Ref to `new-val` and returns `new-val`.")
  (-ref-update! [this f]
    "Atomically updates the Ref with `f` and returns the new value."))

(defn ref?
  "Returns true if `x` satisfies the IRef protocol."
  [x]
  #?(:clj  (instance? fx.async.protocols.IRef x)
     :cljs (satisfies? IRef x)))
