(ns fx.async.fiber
  "Fiber abstraction, supervisor tree, and concurrent execution runners for fx."
  (:refer-clojure :exclude [await])
  (:require [clojure.core.async :as async]
            [fx.async.protocols :as p]
            [fx.core :as fx])
  #?(:clj (:import (java.util.concurrent CompletableFuture
                                         ConcurrentHashMap
                                         ExecutorService
                                         Executors
                                         ForkJoinPool
                                         TimeUnit
                                         TimeoutException)
                   (java.util.function Supplier))))

;; ---------------------------------------------------------------------------
;; Default Executor Management
;; ---------------------------------------------------------------------------

#?(:clj
   (defonce default-virtual-executor
     (delay
       (try
         (let [m (.getMethod Executors "newVirtualThreadPerTaskExecutor" (into-array Class []))]
           ^ExecutorService (.invoke m nil (into-array Object [])))
         (catch Throwable _
           (ForkJoinPool/commonPool))))))

#?(:clj
   (defonce default-thread-pool
     (delay (ForkJoinPool/commonPool))))

(defn- get-executor [context]
  #?(:clj
     (or (:executor context)
         (case (:engine context)
           :virtual-threads @default-virtual-executor
           :thread-pool     @default-thread-pool
           :go              nil
           @default-virtual-executor))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Fiber Implementation
;; ---------------------------------------------------------------------------

(declare interrupt-fiber!)

(deftype Fiber [id
                status
                result-cell
                children
                parent
                finalizers
                thread-ref
                interrupt-handlers]
  p/IFiber
  (-fiber-id [_] id)
  (-fiber-status [_] @status)
  (-fiber-status-atom [_] status)
  (-fiber-result [_] result-cell)
  (-fiber-children [_] children)
  (-fiber-interrupt! [this]
    (interrupt-fiber! this :user-interrupted))
  (-fiber-interrupt! [this reason]
    (interrupt-fiber! this reason))

  #?@(:clj
      [clojure.lang.IDeref
       (deref [_]
              (try
                (.get ^CompletableFuture result-cell)
                (catch Throwable e
                  (let [cause (or (.getCause e) e)]
                    (if (fx/failure? cause)
                      cause
                      (fx/make-failure :fiber/defect cause))))))

       clojure.lang.IBlockingDeref
       (deref [_ timeout-ms timeout-val]
              (try
                (.get ^CompletableFuture result-cell (long timeout-ms) TimeUnit/MILLISECONDS)
                (catch TimeoutException _
                  timeout-val)
                (catch Throwable e
                  (let [cause (or (.getCause e) e)]
                    (if (fx/failure? cause)
                      cause
                      (fx/make-failure :fiber/defect cause))))))

       clojure.lang.IPending
       (isRealized [_]
                   (.isDone ^CompletableFuture result-cell))]))

(defn make-fiber
  "Constructs a new unstarted or initial Fiber instance."
  ([]
   (make-fiber nil))
  ([parent-fiber]
   (->Fiber (str "fiber-" (java.util.UUID/randomUUID))
            (atom :running)
            #?(:clj (CompletableFuture.) :cljs (atom nil))
            (atom #{})
            parent-fiber
            (atom [])
            (atom nil)
            (atom []))))

(defn add-finalizer!
  "Registers a cleanup finalizer thunk or effect to be executed when the fiber completes or is interrupted."
  [fiber finalizer]
  (when (p/fiber? fiber)
    (swap! (.-finalizers ^Fiber fiber) conj finalizer)))

(defn add-interrupt-handler!
  "Registers a callback to be invoked immediately when the fiber is interrupted."
  [fiber handler-fn]
  (when (p/fiber? fiber)
    (swap! (.-interrupt-handlers ^Fiber fiber) conj handler-fn)))

(defn register-child!
  "Registers `child-fiber` into `parent-fiber`'s children supervisor set."
  [parent-fiber child-fiber]
  (when (and (p/fiber? parent-fiber) (p/fiber? child-fiber))
    (swap! (p/fiber-children parent-fiber) conj child-fiber)))

(defn unregister-child!
  "Removes `child-fiber` from `parent-fiber`'s children supervisor set."
  [parent-fiber child-fiber]
  (when (and (p/fiber? parent-fiber) (p/fiber? child-fiber))
    (swap! (p/fiber-children parent-fiber) disj child-fiber)))

(defn- run-finalizers! [fiber context]
  (let [fin-list (reverse @(.-finalizers ^Fiber fiber))]
    (doseq [fin fin-list]
      (try
        (cond
          (fx/effect? fin) (fx/run-sync! fin (assoc context :fiber nil))
          (fn? fin)        (fin)
          :else            nil)
        (catch #?(:clj Throwable :cljs :default) _ nil)))))

(defn interrupt-fiber!
  "Atomically interrupts `fiber` and cascades cancellation to all active child fibers."
  ([fiber]
   (interrupt-fiber! fiber :interrupted))
  ([^Fiber fiber reason]
   (when (p/fiber? fiber)
     (let [status-atom (.-status fiber)]
       (when (compare-and-set! status-atom :running :interrupted)
         ;; Cascade to all child fibers
         (let [active-children @(.-children fiber)]
           (doseq [child active-children]
             (try
               (p/-fiber-interrupt! child reason)
               (catch #?(:clj Throwable :cljs :default) _ nil))))
         ;; Trigger interrupt handlers
         (let [handlers @(.-interrupt-handlers fiber)]
           (doseq [h handlers]
             (try
               (h reason)
               (catch #?(:clj Throwable :cljs :default) _ nil))))
         ;; Interrupt underlying worker thread if active
         #?(:clj
            (when-let [t @(.-thread-ref fiber)]
              (when (and (not= t (Thread/currentThread)) (.isAlive ^Thread t))
                (try
                  (.interrupt ^Thread t)
                  (catch Throwable _ nil)))))
         ;; Complete result cell with interruption failure
         #?(:clj
            (let [^CompletableFuture cf (.-result-cell fiber)]
              (when-not (.isDone cf)
                (.complete cf (fx/make-failure :fiber/interrupted {:fiber-id (.-id fiber)
                                                                   :reason   reason})))))
         ;; Run finalizers if not on active execution thread
         (when (nil? @(.-thread-ref fiber))
           (run-finalizers! fiber {}))
         true)))))

(defn join-fiber!
  "Awaits completion of `fiber` and returns its outcome (or propagates failure / throws)."
  ([fiber]
   (if (p/fiber? fiber)
     #?(:clj
        (try
          (.get ^CompletableFuture (p/fiber-result fiber))
          (catch Throwable e
            (let [cause (or (.getCause e) e)]
              (if (fx/failure? cause)
                cause
                (fx/make-failure :fiber/defect cause)))))
        :cljs @(p/fiber-result fiber))
     (fx/make-failure :fiber/invalid-target {:fiber fiber})))
  ([fiber timeout-ms timeout-val]
   (if (p/fiber? fiber)
     #?(:clj
        (try
          (.get ^CompletableFuture (p/fiber-result fiber) (long timeout-ms) TimeUnit/MILLISECONDS)
          (catch TimeoutException _
            timeout-val)
          (catch Throwable e
            (let [cause (or (.getCause e) e)]
              (if (fx/failure? cause)
                cause
                (fx/make-failure :fiber/defect cause)))))
        :cljs @(p/fiber-result fiber))
     (fx/make-failure :fiber/invalid-target {:fiber fiber}))))

;; ---------------------------------------------------------------------------
;; Fiber Execution Runners
;; ---------------------------------------------------------------------------

(defn- execute-fiber-task! [^Fiber fiber effect context]
  (let [status-atom (.-status fiber)
        res-cell    ^CompletableFuture (.-result-cell fiber)]
    #?(:clj (reset! (.-thread-ref fiber) (Thread/currentThread)))
    (try
      (if (= @status-atom :interrupted)
        (let [failure (fx/make-failure :fiber/interrupted {:fiber-id (.-id fiber)})]
          (.complete res-cell failure)
          failure)
        (let [res (try
                    (fx/run-sync! effect context)
                    (catch InterruptedException _
                      (compare-and-set! status-atom :running :interrupted)
                      (fx/make-failure :fiber/interrupted {:fiber-id (.-id fiber)}))
                    (catch Throwable e
                      (if (or (instance? InterruptedException (.getCause e))
                              (= @status-atom :interrupted))
                        (fx/make-failure :fiber/interrupted {:fiber-id (.-id fiber)})
                        (fx/make-failure :fiber/defect {:exception e :message (.getMessage e)}))))]
          (cond
            (= @status-atom :interrupted)
            (let [fail (if (fx/failure? res) res (fx/make-failure :fiber/interrupted {:fiber-id (.-id fiber)}))]
              (.complete res-cell fail)
              fail)

            (fx/failure? res)
            (do
              (compare-and-set! status-atom :running :failed)
              (.complete res-cell res)
              res)

            :else
            (do
              (compare-and-set! status-atom :running :succeeded)
              (.complete res-cell res)
              res))))
      (finally
        (run-finalizers! fiber context)
        (when-let [parent (.-parent fiber)]
          (unregister-child! parent fiber))))))

(defn run-fiber!
  "Executes `effect` asynchronously in a dedicated lightweight Fiber, returning the `IFiber` instance.
   Binds `:fiber` in the evaluation context and coordinates parent-child supervisor hierarchy."
  ([effect]
   (run-fiber! effect {}))
  ([effect context]
   (let [parent-fiber (:fiber context)
         fiber        (make-fiber parent-fiber)
         fiber-ctx    (assoc (or context {}) :fiber fiber)]
     (when parent-fiber
       (register-child! parent-fiber fiber))
     (let [engine (:engine context)]
       (if (= engine :go)
         (async/go
           (execute-fiber-task! fiber effect fiber-ctx))
         (if-let [^ExecutorService exec (get-executor fiber-ctx)]
           (.submit exec ^Runnable (fn [] (execute-fiber-task! fiber effect fiber-ctx)))
           #?(:clj
              (CompletableFuture/supplyAsync
               (reify Supplier
                 (get [_]
                   (execute-fiber-task! fiber effect fiber-ctx))))
              :cljs
              (js/setTimeout (fn [] (execute-fiber-task! fiber effect fiber-ctx)) 0)))))
     fiber)))

(defn run-async-chan!
  "Executes `effect` in a background fiber and puts the final outcome onto a core.async channel.
   Closes the channel upon completion. Returns the output channel."
  ([effect]
   (run-async-chan! effect {}))
  ([effect context]
   (let [out-chan (async/chan 1)
         fiber    (run-fiber! effect context)]
     #?(:clj
        (let [^CompletableFuture cf (p/fiber-result fiber)]
          (.whenComplete cf
                         (reify java.util.function.BiConsumer
                           (accept [_ result _err]
                             (async/put! out-chan (or result (fx/make-failure :fiber/nil-result nil))
                                         (fn [_] (async/close! out-chan)))))))
        :cljs
        (async/go
          (let [res (join-fiber! fiber)]
            (async/>! out-chan res)
            (async/close! out-chan))))
     out-chan)))
