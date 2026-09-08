(ns todo.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [malli.core :as m]
            [todo.schema :as schema]))

(deftest test-malli-schema-validation
  (testing "TodoSchema validates complete entity records"
    (let [valid-todo {:id          1
                      :title       "Buy groceries"
                      :description "Milk and eggs"
                      :completed   false
                      :created-at  "2026-09-08T12:00:00Z"
                      :updated-at  "2026-09-08T12:00:00Z"}]
      (is (m/validate schema/TodoSchema valid-todo))
      (is (m/validate schema/TodoSchema (assoc valid-todo :description nil)))
      (is (not (m/validate schema/TodoSchema (dissoc valid-todo :id))))
      (is (not (m/validate schema/TodoSchema (assoc valid-todo :id -1))))
      (is (not (m/validate schema/TodoSchema (assoc valid-todo :completed "not-bool"))))))

  (testing "CreateTodoPayload schema and validation"
    (is (m/validate schema/CreateTodoPayload {:title "Test Todo"}))
    (is (m/validate schema/CreateTodoPayload {:title "Test Todo" :completed true}))
    (is (m/validate schema/CreateTodoPayload {:title "Test Todo" :description "Details" :completed false}))
    (is (not (m/validate schema/CreateTodoPayload {:title ""})))
    (is (not (m/validate schema/CreateTodoPayload {:title "   "})))
    (is (not (m/validate schema/CreateTodoPayload {:title 123})))
    (is (not (m/validate schema/CreateTodoPayload {:title "Test" :completed "yes"}))))

  (testing "UpdateTodoPayload schema and validation"
    (is (m/validate schema/UpdateTodoPayload {}))
    (is (m/validate schema/UpdateTodoPayload {:title "Updated Title"}))
    (is (m/validate schema/UpdateTodoPayload {:completed true}))
    (is (m/validate schema/UpdateTodoPayload {:description nil}))
    (is (not (m/validate schema/UpdateTodoPayload {:title ""})))
    (is (not (m/validate schema/UpdateTodoPayload {:title "   "})))
    (is (not (m/validate schema/UpdateTodoPayload {:completed "not-bool"})))))

(deftest test-schema-coercion-and-effects
  (testing "validate-create-todo> coercion and validation"
    (let [res-valid (fx/run-sync! (schema/validate-create-todo> {:title "  Buy coffee  " :completed true}))
          res-default-comp (fx/run-sync! (schema/validate-create-todo> {:title "No completed flag"}))
          res-str-comp (fx/run-sync! (schema/validate-create-todo> {:title "String completed" :completed "true"}))
          res-invalid (fx/run-sync! (schema/validate-create-todo> {:title ""}))
          res-nonmap (fx/run-sync! (schema/validate-create-todo> "not a map"))]
      (is (= {:title "Buy coffee" :completed true} res-valid))
      (is (= {:title "No completed flag" :completed false} res-default-comp))
      (is (= {:title "String completed" :completed true} res-str-comp))

      (is (fx/failure? res-invalid))
      (is (= :todo/invalid-input (fx/tag res-invalid)))
      (is (= :title (:field (fx/error-data res-invalid))))

      (is (fx/failure? res-nonmap))
      (is (= :todo/invalid-input (fx/tag res-nonmap)))))

  (testing "validate-update-todo> coercion and validation"
    (let [res-valid (fx/run-sync! (schema/validate-update-todo> {:title "  Trimmed  " :completed "false"}))
          res-invalid (fx/run-sync! (schema/validate-update-todo> {:completed "not-a-bool"}))]
      (is (= {:title "Trimmed" :completed false} res-valid))

      (is (fx/failure? res-invalid))
      (is (= :todo/invalid-input (fx/tag res-invalid)))
      (is (= :completed (:field (fx/error-data res-invalid))))))

  (testing "coerce-id> parses numeric strings and rejects invalid inputs"
    (is (= 42 (fx/run-sync! (schema/coerce-id> "42"))))
    (is (= 7 (fx/run-sync! (schema/coerce-id> 7))))
    (let [res-invalid (fx/run-sync! (schema/coerce-id> "abc"))
          res-neg (fx/run-sync! (schema/coerce-id> -5))]
      (is (fx/failure? res-invalid))
      (is (= :todo/invalid-input (fx/tag res-invalid)))
      (is (= :id (:field (fx/error-data res-invalid))))
      (is (fx/failure? res-neg))
      (is (= :todo/invalid-input (fx/tag res-neg)))))

  (testing "coerce-filter> coerces boolean parameters"
    (is (nil? (fx/run-sync! (schema/coerce-filter> nil))))
    (is (true? (fx/run-sync! (schema/coerce-filter> "true"))))
    (is (false? (fx/run-sync! (schema/coerce-filter> "false"))))
    (is (true? (fx/run-sync! (schema/coerce-filter> true))))
    (is (false? (fx/run-sync! (schema/coerce-filter> false))))
    (is (nil? (fx/run-sync! (schema/coerce-filter> "invalid"))))))
