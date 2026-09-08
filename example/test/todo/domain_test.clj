(ns todo.domain-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.utils :as fxu]
            [todo.db :as db]
            [todo.domain :as domain])
  (:import (java.util UUID)))

;; ---------------------------------------------------------------------------
;; Test Setup Helpers
;; ---------------------------------------------------------------------------

(defn- fresh-test-ds []
  (let [url (str "jdbc:sqlite:file:testdb-domain-" (UUID/randomUUID) "?mode=memory&cache=shared")
        ds (db/create-datasource url)]
    (fx/run-sync! (db/init-db!> ds))
    ds))

;; ---------------------------------------------------------------------------
;; 1. Pure Input Validation & Short-Circuiting (Zero DB / Zero Context Needed)
;; ---------------------------------------------------------------------------

(deftest test-create-todo-validation-short-circuiting
  (testing "create-todo> fails fast on invalid payload without touching database"
    ;; Note: No datasource or context is provided to run-sync!.
    ;; Because validation fails at the root step, downstream DB operations never execute.
    (let [res-nil (fx/run-sync! (domain/create-todo> nil))
          res-str (fx/run-sync! (domain/create-todo> "invalid"))
          res-empty (fx/run-sync! (domain/create-todo> {}))
          res-blank (fx/run-sync! (domain/create-todo> {:title "   "}))
          res-nonstr (fx/run-sync! (domain/create-todo> {:title 123}))
          res-bad-completed (fx/run-sync! (domain/create-todo> {:title "Task" :completed "yes"}))]

      (is (fx/failure? res-nil))
      (is (= :todo/invalid-input (fx/tag res-nil)))

      (is (fx/failure? res-str))
      (is (= :todo/invalid-input (fx/tag res-str)))

      (is (fx/failure? res-empty))
      (is (= :todo/invalid-input (fx/tag res-empty)))
      (is (= :title (:field (fx/error-data res-empty))))

      (is (fx/failure? res-blank))
      (is (= :todo/invalid-input (fx/tag res-blank)))
      (is (= :title (:field (fx/error-data res-blank))))

      (is (fx/failure? res-nonstr))
      (is (= :todo/invalid-input (fx/tag res-nonstr)))
      (is (= :title (:field (fx/error-data res-nonstr))))

      (is (fx/failure? res-bad-completed))
      (is (= :todo/invalid-input (fx/tag res-bad-completed)))
      (is (= :completed (:field (fx/error-data res-bad-completed)))))))

(deftest test-update-todo-validation-short-circuiting
  (testing "update-todo> validates payload and fails fast on invalid input"
    (let [res-nonmap (fx/run-sync! (domain/update-todo> 1 "bad-body"))
          res-blank-title (fx/run-sync! (domain/update-todo> 1 {:title ""}))
          res-ws-title (fx/run-sync! (domain/update-todo> 1 {:title "  \t\n"}))
          res-bad-completed (fx/run-sync! (domain/update-todo> 1 {:completed "yes"}))]

      (is (fx/failure? res-nonmap))
      (is (= :todo/invalid-input (fx/tag res-nonmap)))

      (is (fx/failure? res-blank-title))
      (is (= :todo/invalid-input (fx/tag res-blank-title)))
      (is (= :title (:field (fx/error-data res-blank-title))))

      (is (fx/failure? res-ws-title))
      (is (= :todo/invalid-input (fx/tag res-ws-title)))
      (is (= :title (:field (fx/error-data res-ws-title))))

      (is (fx/failure? res-bad-completed))
      (is (= :todo/invalid-input (fx/tag res-bad-completed)))
      (is (= :completed (:field (fx/error-data res-bad-completed)))))))

;; ---------------------------------------------------------------------------
;; 2. AST Structure & Pipeline Inspection (Programs as Data)
;; ---------------------------------------------------------------------------

(deftest test-domain-pipeline-ast-inspection
  (testing "domain functions return Effect AST records before execution"
    (let [create-eff (domain/create-todo> {:title "Test Todo"})
          get-eff (domain/get-todo-by-id> 1)
          list-eff (domain/list-todos>)
          toggle-eff (domain/toggle-todo> 1)
          delete-eff (domain/delete-todo> 1)]

      (is (fx/effect? create-eff))
      (is (fx/effect? get-eff))
      (is (fx/effect? list-eff))
      (is (fx/effect? toggle-eff))
      (is (fx/effect? delete-eff))

      (is (= [:succeed :mapcat] (fxu/effect-tags create-eff)))
      (is (= [:try :map :mapcat] (fxu/effect-tags get-eff)))
      (is (= [:try :map] (fxu/effect-tags list-eff)))
      (is (= [:try :map :mapcat :mapcat] (fxu/effect-tags toggle-eff)))
      (is (= [:try :map :mapcat :mapcat] (fxu/effect-tags delete-eff))))))

;; ---------------------------------------------------------------------------
;; 3. Pure Effect Mocking & Splicing without Real Database
;; ---------------------------------------------------------------------------

(deftest test-domain-mocking-without-db
  (testing "get-todo-by-id> business logic can be tested in isolation by replacing db effect"
    ;; Instead of spinning up a database or mocking global methods with `with-redefs`,
    ;; we demonstrate how `fx.utils` allows transforming pure effect ASTs.
    (let [pipeline (domain/get-todo-by-id> 42)
          ;; Case A: Replace the root DB query with a successful mock entity
          mock-entity {:id 42 :title "Mocked Task" :completed false}
          mocked-pipeline-success (fxu/replace-by-tag pipeline :try (fx/succeed> mock-entity))
          result-success (fx/run-sync! mocked-pipeline-success)

          ;; Case B: Replace the root DB query with nil (record not found in DB)
          mocked-pipeline-missing (fxu/replace-by-tag pipeline :try (fx/succeed> nil))
          result-missing (fx/run-sync! mocked-pipeline-missing)]

      (is (= mock-entity result-success))

      (is (fx/failure? result-missing))
      (is (= :todo/not-found (fx/tag result-missing)))
      (is (= 42 (:id (fx/error-data result-missing)))))))

;; ---------------------------------------------------------------------------
;; 4. End-to-End Domain Pipelines with SQLite In-Memory Database
;; ---------------------------------------------------------------------------

(deftest test-domain-create-todo-with-completed-flag
  (let [ds (fresh-test-ds)
        ctx {:fx.jdbc/datasource ds}]
    (testing "create-todo> supports creating pre-completed todo"
      (let [created (fx/run-sync!
                      (domain/create-todo> {:title "Done item" :completed true})
                      ctx)]
        (is (= 1 (:id created)))
        (is (= "Done item" (:title created)))
        (is (true? (:completed created)))))))

(deftest test-domain-crud-lifecycle
  (let [ds (fresh-test-ds)
        ctx {:fx.jdbc/datasource ds}]

    (testing "create-todo> creates, trims strings, assigns defaults, and persists"
      (let [created (fx/run-sync!
                      (domain/create-todo> {:title       "  Clean Desk  "
                                            :description "  Organize papers and wires  "})
                      ctx)]
        (is (map? created))
        (is (= 1 (:id created)))
        (is (= "Clean Desk" (:title created)))
        (is (= "Organize papers and wires" (:description created)))
        (is (false? (:completed created)))
        (is (some? (:created-at created)))
        (is (some? (:updated-at created)))))

    (testing "get-todo-by-id> returns entity or :todo/not-found failure"
      (let [found (fx/run-sync! (domain/get-todo-by-id> 1) ctx)
            not-found (fx/run-sync! (domain/get-todo-by-id> 999) ctx)]
        (is (= 1 (:id found)))
        (is (= "Clean Desk" (:title found)))

        (is (fx/failure? not-found))
        (is (= :todo/not-found (fx/tag not-found)))
        (is (= 999 (:id (fx/error-data not-found))))))

    (testing "update-todo> modifies existing todo fields"
      (let [updated (fx/run-sync!
                      (domain/update-todo> 1 {:title       "Clean and Organize Desk"
                                              :description "All tidy now"
                                              :completed   true})
                      ctx)
            fetched (fx/run-sync! (domain/get-todo-by-id> 1) ctx)]
        (is (= 1 (:id updated)))
        (is (= "Clean and Organize Desk" (:title updated)))
        (is (= "All tidy now" (:description updated)))
        (is (true? (:completed updated)))
        (is (= updated fetched))))

    (testing "update-todo> fails with :todo/not-found when updating non-existent ID"
      (let [res (fx/run-sync! (domain/update-todo> 999 {:title "Non existent"}) ctx)]
        (is (fx/failure? res))
        (is (= :todo/not-found (fx/tag res)))
        (is (= 999 (:id (fx/error-data res))))))

    (testing "toggle-todo> flips completed boolean and updates timestamp"
      (let [toggled-false (fx/run-sync! (domain/toggle-todo> 1) ctx)
            toggled-true (fx/run-sync! (domain/toggle-todo> 1) ctx)
            not-found (fx/run-sync! (domain/toggle-todo> 999) ctx)]
        (is (false? (:completed toggled-false)))
        (is (true? (:completed toggled-true)))

        (is (fx/failure? not-found))
        (is (= :todo/not-found (fx/tag not-found)))
        (is (= 999 (:id (fx/error-data not-found))))))

    (testing "list-todos> supports unfiltered and filtered queries"
      ;; Create another todo that is active
      (fx/run-sync! (domain/create-todo> {:title "Second active task"}) ctx)

      (let [all (fx/run-sync! (domain/list-todos>) ctx)
            completed (fx/run-sync! (domain/list-todos> true) ctx)
            active (fx/run-sync! (domain/list-todos> false) ctx)]
        (is (= 2 (count all)))
        (is (= 1 (count completed)))
        (is (= 1 (:id (first completed))))
        (is (= 1 (count active)))
        (is (= 2 (:id (first active))))))

    (testing "delete-todo> removes todo and fails if not found"
      (let [del-res (fx/run-sync! (domain/delete-todo> 1) ctx)
            del-again (fx/run-sync! (domain/delete-todo> 1) ctx)
            remaining (fx/run-sync! (domain/list-todos>) ctx)]
        (is (= {:deleted true :id 1} del-res))

        (is (fx/failure? del-again))
        (is (= :todo/not-found (fx/tag del-again)))

        (is (= 1 (count remaining)))
        (is (= 2 (:id (first remaining))))))))
