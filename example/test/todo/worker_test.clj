(ns todo.worker-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.metrics :as metrics]
            [todo.db :as db]
            [todo.worker :as worker])
  (:import (java.time Duration Instant)
           (java.util UUID)))

(defn- test-db-url []
  (str "jdbc:sqlite:file:testdb-worker-" (UUID/randomUUID) "?mode=memory&cache=shared"))

(deftest test-maintenance-task-cleanup
  (let [ds (db/create-datasource (test-db-url))
        _ (fx/run-sync! (db/init-db!> ds))
        reg (metrics/make-metrics-registry)
        ctx {:fx.jdbc/datasource ds
             :fx.observability/metrics-registry reg}]

    (testing "maintenance task deletes completed todos older than cutoff"
      ;; Create fresh active and completed todos
      (let [old-timestamp (str (.minus (Instant/now) (Duration/ofDays 40)))
            recent-timestamp (str (Instant/now))]

        ;; Insert an old completed todo
        (fx/run-sync!
          (fx/try> (fn []
                     (db/init-db! ds)
                     (let [conn (.getConnection ds)]
                       (try
                         (let [stmt (.prepareStatement conn "INSERT INTO todos (title, completed, created_at, updated_at) VALUES (?, 1, ?, ?)")]
                           (.setString stmt 1 "Old Completed Task")
                           (.setString stmt 2 old-timestamp)
                           (.setString stmt 3 old-timestamp)
                           (.executeUpdate stmt))
                         (finally (.close conn)))))
                   :db/insert-err)
          ctx)

        ;; Insert a recent completed todo
        (let [recent-todo (fx/run-sync! (db/insert-todo!> {:title "Recent Completed Task"
                                                          :completed true
                                                          :created-at recent-timestamp
                                                          :updated-at recent-timestamp})
                                       ctx)]
          (is (some? recent-todo)))

        ;; Verify 2 todos exist before maintenance
        (let [todos-before (fx/run-sync! (db/query-todos>) ctx)]
          (is (= 2 (count todos-before))))

        ;; Run maintenance task with 30-day cutoff
        (let [res (fx/run-sync! (worker/maintenance-task> 30) ctx)]
          (is (not (fx/failure? res))))

        ;; Verify only recent completed todo remains
        (let [todos-after (fx/run-sync! (db/query-todos>) ctx)]
          (is (= 1 (count todos-after)))
          (is (= "Recent Completed Task" (:title (first todos-after)))))

        ;; Check metric increment
        (let [snapshot (metrics/metrics-snapshot! reg)
              cnt (or (get-in snapshot [:counters "todo.maintenance.runs.total" :value])
                      (get-in snapshot [:counters :todo.maintenance.runs.total :value])
                      0)]
          (is (>= cnt 1)))))))

(deftest test-parallel-import-todos
  (let [ds (db/create-datasource (test-db-url))
        _ (fx/run-sync! (db/init-db!> ds))
        ctx {:fx.jdbc/datasource ds}
        raw-todos [{:title "Batch Task 1" :description "Desc 1"}
                   {:title "Batch Task 2" :description "Desc 2"}
                   {:title "Batch Task 3" :description "Desc 3"}
                   {:title "Batch Task 4" :description "Desc 4"}]]
    (testing "parallel-import-todos> imports batch using map-par>"
      (let [res (fx/run-sync! (worker/parallel-import-todos> raw-todos 2) ctx)]
        (is (= 4 (count res)))
        (let [all-db (fx/run-sync! (db/query-todos>) ctx)]
          (is (= 4 (count all-db))))))))

(deftest test-notification-hub-worker
  (testing "notification-hub> creates broadcast hub and routes events"
    (let [hub (fx/run-sync! (worker/notification-hub> 10))]
      (is (fxa/hub? hub))
      (let [sub1 (fx/run-sync! (fxa/hub-subscribe> hub))
            sub2 (fx/run-sync! (fxa/hub-subscribe> hub))]
        (fx/run-sync! (fxa/hub-publish> hub {:type :todo/created :id 101}))
        (is (= {:type :todo/created :id 101} (fx/run-sync! (fxa/queue-take> sub1))))
        (is (= {:type :todo/created :id 101} (fx/run-sync! (fxa/queue-take> sub2))))
        (fx/run-sync! (fxa/hub-shutdown> hub))))))

(deftest test-worker-layer-lifecycle
  (let [ds (db/create-datasource (test-db-url))
        _ (fx/run-sync! (db/init-db!> ds))
        reg (metrics/make-metrics-registry)
        ctx {:fx.jdbc/datasource ds
             :fx.observability/metrics-registry reg}]

    (testing "worker layer starts loop and cleans up on layer stop"
      (let [w-layer (worker/worker-layer> {:interval-ms 20 :days-old 30})
            started (fx/run-sync! (fx-layer/provide-layer>
                                    (fx/context>)
                                    w-layer)
                                  ctx)]
        (is (map? started))
        (is (contains? started :todo/worker))
        (let [handle (get started :todo/worker)]
          (is (some? (:fiber handle)))
          ;; Let worker loop tick once
          (Thread/sleep 50)
          ;; Stop worker loop
          (worker/stop-worker-loop! handle)
          (is (false? @(:running handle))))))))
