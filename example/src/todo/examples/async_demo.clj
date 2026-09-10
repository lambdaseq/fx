(ns todo.examples.async-demo
  "Standalone demonstration of `fx-async` structured concurrency and async primitives:
   1. Structured Parallelism: `all-par>` and bounded `map-par>`.
   2. Speculative Racing: `race>` with automatic branch cancellation.
   3. Fiber Supervision & Hierarchical Unwinding: `fork>`, `join>`, and `interrupt>`.
   4. CSP Channel Pipelines: `chan>`, `chan-put>`, `chan-take>`, and `chan-alts>`.
   5. Async Coordination: Bounded `Queue`, broadcast `Hub`, `Deferred`, `Semaphore`, and `Latch`.
   6. Execution Scopes: `with-virtual-threads>`, `with-thread-pool>`, and `with-go>`."
  (:require [clojure.core.async :as async]
            [fx.async :as fxa]
            [fx.core :as fx]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.observability.trace :as trace])
  (:gen-class))

;; ---------------------------------------------------------------------------
;; 1. Structured Parallelism: all-par> and map-par>
;; ---------------------------------------------------------------------------

(defn demo-parallel-processing>
  "Demonstrates parallel batch mapping with bounded concurrency controls."
  []
  (let [items (mapv (fn [i] {:id i :task (str "Task #" i)}) (range 1 9))]
    (-> (fx/succeed> "\n--- 1. Structured Parallelism (map-par> & all-par>) ---")
        (fx/tap> println)
        (fx/chain>
          (fxa/map-par>
            (fn [{:keys [id task]}]
              (let [delay-ms (+ 30 (rand-int 50))]
                (-> (fx/sleep> delay-ms)
                    (fx/map> (fn [_]
                               (println (str "  [Thread: " (.getName (Thread/currentThread)) "] Completed " task " in " delay-ms "ms"))
                               {:id id :status :done :duration-ms delay-ms})))))
            items
            {:concurrency 3}))
        (fx/tap> (fn [results]
                   (println "All 8 tasks processed with concurrency limit of 3.")
                   (println "Total items successfully processed:" (count results)))))))

;; ---------------------------------------------------------------------------
;; 2. Speculative Racing: race> with Automatic Cancellation
;; ---------------------------------------------------------------------------

(defn demo-speculative-race>
  "Demonstrates racing multiple effects where the fastest succeeds and slower competitors are cancelled."
  []
  (let [fast-mirror   (-> (fx/sleep> 40) (fx/map> (fn [_] {:source :fast-mirror :data "Payload from US-East"})))
        medium-mirror (-> (fx/sleep> 120) (fx/map> (fn [_] {:source :medium-mirror :data "Payload from EU-West"})))
        slow-mirror   (-> (fx/sleep> 300) (fx/map> (fn [_] {:source :slow-mirror :data "Payload from AP-South"})))]
    (-> (fx/succeed> "\n--- 2. Speculative Racing (race>) ---")
        (fx/tap> println)
        (fx/chain>
          (fxa/race> [slow-mirror fast-mirror medium-mirror]))
        (fx/tap> (fn [winner]
                   (println "Winner resolved first:" winner)
                   (println "Slower mirror branches were automatically interrupted!"))))))

;; ---------------------------------------------------------------------------
;; 3. Fiber Supervision & Resource Cleanup Unwinding
;; ---------------------------------------------------------------------------

(defn demo-fiber-supervision>
  "Demonstrates fiber forks, joins, and cascading cancellation with reverse-order acquire-release> cleanup."
  []
  (let [cleaned-up? (atom false)
        started?    (promise)]
    (-> (fx/succeed> "\n--- 3. Fiber Supervision & Guaranteed Unwind ---")
        (fx/tap> println)
        (fx/chain>
          (fx/acquire-release>
            (fx/try> (fn []
                       (println "  Acquiring parent resource (e.g. database connection / socket)...")
                       :parent-resource)
                     :acquire-failed)
            (fn [res]
              (println "  Parent resource active:" res)
              ;; Fork a long-running background child fiber
              (-> (fxa/fork>
                    (fx/acquire-release>
                      (fx/succeed> :child-worker)
                      (fn [_]
                        (deliver started? true)
                        (println "    Child fiber running in background...")
                        (fx/sleep> 5000))
                      (fn [_]
                        (println "    [CLEANUP] Child fiber finalizer executed safely!")
                        (reset! cleaned-up? true))))
                  (fx/mapcat> (fn [child-fib]
                                (println "  Spawned child fiber:" (fxa/fiber-id child-fib))
                                ;; Await child start before interrupting
                                (fx/try> (fn [] (deref started? 1000 nil)))
                                (println "  Interrupting child fiber...")
                                (-> (fxa/interrupt> child-fib :demo-stop)
                                    (fx/chain> (fx/sleep> 50)))))))
            (fn [res]
              (println "  [CLEANUP] Parent resource finalizer executed for:" res))))
        (fx/tap> (fn [_]
                   (println "Child fiber cleanup verified:" @cleaned-up?))))))

;; ---------------------------------------------------------------------------
;; 4. CSP Channel Pipelines (core.async Integration)
;; ---------------------------------------------------------------------------

(defn demo-channel-pipeline>
  "Demonstrates effectful core.async channel creation, parking put/take, and alts selection."
  []
  (-> (fx/succeed> "\n--- 4. CSP Channels Integration (chan>, chan-put>, chan-take>, chan-alts>) ---")
      (fx/tap> println)
      (fx/chain>
        (fxa/chan> 5))
      (fx/mapcat> (fn [ch]
                    (-> (fxa/chan-put> ch "Message Alpha")
                        (fx/chain> (fxa/chan-put> ch "Message Beta"))
                        (fx/chain> (fxa/chan-put> ch "Message Gamma"))
                        (fx/chain>
                          (fxa/all-par>
                            [(-> (fxa/chan-take> ch) (fx/tap> (fn [m] (println "  Received via chan-take>:" m))))
                             (-> (fxa/chan-take> ch) (fx/tap> (fn [m] (println "  Received via chan-take>:" m))))
                             (-> (fxa/chan-take> ch) (fx/tap> (fn [m] (println "  Received via chan-take>:" m))))]))
                        (fx/chain>
                          ;; Demonstrate chan-alts> with a timeout channel
                          (let [timeout-ch (async/timeout 100)]
                            (-> (fxa/chan-alts> [ch timeout-ch])
                                (fx/tap> (fn [[val port]]
                                           (if (= port timeout-ch)
                                             (println "  chan-alts> gracefully triggered timeout channel as expected (channel was empty).")
                                             (println "  chan-alts> received:" val)))))))
                        (fx/chain> (fxa/chan-close> ch)))))))

;; ---------------------------------------------------------------------------
;; 5. Async Coordination & Messaging Primitives
;; ---------------------------------------------------------------------------

(defn demo-coordination-primitives>
  "Demonstrates Queue, Hub (broadcast), Deferred, Semaphore, and Latch."
  []
  (-> (fx/succeed> "\n--- 5. Async Coordination (Queue, Hub, Semaphore, Deferred, Latch) ---")
      (fx/tap> println)
      ;; 5a. Bounded Queue
      (fx/chain>
        (fxa/queue-bounded> 3))
      (fx/mapcat> (fn [q]
                    (println "  [Queue] Offering items to bounded queue...")
                    (-> (fxa/queue-offer> q "Event-1")
                        (fx/chain> (fxa/queue-offer> q "Event-2"))
                        (fx/chain> (fxa/queue-offer> q "Event-3"))
                        (fx/chain> (fxa/queue-size> q))
                        (fx/tap> (fn [sz] (println "  [Queue] Current size:" sz)))
                        (fx/chain> (fxa/queue-take> q))
                        (fx/tap> (fn [it] (println "  [Queue] Consumed item:" it))))))
      ;; 5b. Broadcast Hub
      (fx/chain>
        (fxa/hub-bounded> 16))
      (fx/mapcat> (fn [hub]
                    (println "  [Hub] Creating multiple subscribers...")
                    (-> (fxa/all-par> [(fxa/hub-subscribe> hub) (fxa/hub-subscribe> hub)])
                        (fx/mapcat> (fn [[sub1 sub2]]
                                      (-> (fxa/hub-publish> hub "Broadcast Alert: New Todo Created")
                                          (fx/tap> (fn [recipients] (println "  [Hub] Published broadcast to" recipients "subscribers")))
                                          (fx/chain>
                                            (fxa/all-par>
                                              [(-> (fxa/queue-take> sub1) (fx/tap> (fn [msg] (println "    Subscriber 1 received:" msg))))
                                               (-> (fxa/queue-take> sub2) (fx/tap> (fn [msg] (println "    Subscriber 2 received:" msg))))]))))))))
      ;; 5c. Counting Semaphore
      (fx/chain>
        (fxa/semaphore> 2))
      (fx/mapcat> (fn [sem]
                    (println "  [Semaphore] Concurrency control via with-permit> (limit: 2)...")
                    (fxa/all-par>
                      [(fxa/with-permit> sem (fx/succeed> "Resource Access A"))
                       (fxa/with-permit> sem (fx/succeed> "Resource Access B"))
                       (fxa/with-permit> sem (fx/succeed> "Resource Access C"))])))
      (fx/tap> (fn [results]
                 (println "  [Semaphore] All permits acquired and released safely:" results)))
      ;; 5d. Single-assignment Deferred
      (fx/chain>
        (fxa/deferred>))
      (fx/mapcat> (fn [d]
                    (println "  [Deferred] Awaiting single-assignment deferred value...")
                    (-> (fxa/all-par>
                          [(-> (fx/sleep> 50)
                               (fx/chain> (fxa/deferred-succeed> d {:user-id 42 :auth-token "xyz-secret"})))
                           (fxa/deferred-await> d)])
                        (fx/tap> (fn [[_resolved-status value]]
                                   (println "  [Deferred] Successfully resolved deferred cell:" value))))))))

;; ---------------------------------------------------------------------------
;; 6. Execution Scopes (Virtual Threads, Thread Pool, Go)
;; ---------------------------------------------------------------------------

(defn demo-execution-scopes>
  "Demonstrates running effects in virtual threads or thread pool scopes."
  []
  (-> (fx/succeed> "\n--- 6. Execution Scopes (with-virtual-threads> & with-thread-pool>) ---")
      (fx/tap> println)
      (fx/chain>
        (fxa/with-virtual-threads>
          (-> (fx/try> (fn []
                         (let [t (Thread/currentThread)]
                           {:thread-name (.getName t)
                            :virtual?    (try (.isVirtual ^Thread t) (catch Throwable _ false))})))
              (fx/tap> (fn [info]
                         (println "  Executed in virtual thread scope:" info))))))
      (fx/chain>
        (fxa/with-thread-pool>
          (-> (fx/try> (fn []
                         (let [t (Thread/currentThread)]
                           {:thread-name (.getName t)
                            :pool?       true})))
              (fx/tap> (fn [info]
                         (println "  Executed in fixed thread pool scope:" info))))))))

;; ---------------------------------------------------------------------------
;; CLI Entrypoint
;; ---------------------------------------------------------------------------

(defn -main
  "Runs all fx-async demonstration scenarios."
  [& _args]
  (println "=======================================================")
  (println "      fx-async Structured Concurrency Demonstration")
  (println "=======================================================")
  (let [demo-pipeline>
        (-> (demo-parallel-processing>)
            (fx/chain> (demo-speculative-race>))
            (fx/chain> (demo-fiber-supervision>))
            (fx/chain> (demo-channel-pipeline>))
            (fx/chain> (demo-coordination-primitives>))
            (fx/chain> (demo-execution-scopes>)))]
    (fx/run-sync! demo-pipeline>)
    (println "\n=======================================================")
    (println "   All fx-async demonstrations completed successfully!")
    (println "=======================================================")))
