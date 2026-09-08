(ns todo.schema
  (:require [clojure.string :as str]
            [fx.core :as fx]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]))

;; ---------------------------------------------------------------------------
;; Custom Types & Transformers
;; ---------------------------------------------------------------------------

(def NonBlankString
  [:and :string [:fn {:error/message "Field must not be blank"} (complement str/blank?)]])

(def default-transformer
  (mt/transformer
    mt/json-transformer
    mt/string-transformer
    mt/default-value-transformer))

;; ---------------------------------------------------------------------------
;; Domain Schemas
;; ---------------------------------------------------------------------------

(def TodoSchema
  [:map
   [:id [:int {:min 1}]]
   [:title :string]
   [:description [:maybe :string]]
   [:completed :boolean]
   [:created-at :string]
   [:updated-at :string]])

(def CreateTodoPayload
  [:map {:closed false}
   [:title NonBlankString]
   [:description {:optional true} [:maybe :string]]
   [:completed {:optional true} :boolean]])

(def UpdateTodoPayload
  [:map {:closed false}
   [:title {:optional true} NonBlankString]
   [:description {:optional true} [:maybe :string]]
   [:completed {:optional true} :boolean]])

(def CompletedFilterSchema
  [:maybe :boolean])

(def IdSchema
  [:int {:min 1}])

;; ---------------------------------------------------------------------------
;; Validation & Coercion Helpers
;; ---------------------------------------------------------------------------

(defn coerce
  "Decodes and coerces `value` against `schema` using string/JSON/default transformers."
  ([schema value]
   (coerce schema value default-transformer))
  ([schema value transformer]
   (m/decode schema value transformer)))

(defn explain-errors
  "Explains validation failures in `value` against `schema` into a human-readable map."
  [schema value]
  (some-> (m/explain schema value) me/humanize))

(defn validate-create-todo>
  "Validates and coerces payload for creating a todo.
   Yields an effect with coerced map on success, or fails with `:todo/invalid-input`."
  [payload]
  (cond
    (not (map? payload))
    (fx/fail> :todo/invalid-input {:message "Request body must be a JSON object"})

    :else
    (let [coerced (coerce CreateTodoPayload payload)
          errors  (explain-errors CreateTodoPayload coerced)]
      (if (nil? errors)
        (fx/succeed> (cond-> (assoc coerced
                                    :title (str/trim (:title coerced))
                                    :completed (boolean (get coerced :completed false)))
                       (some? (:description coerced)) (update :description str/trim)))
        (let [first-field (first (keys errors))]
          (fx/fail> :todo/invalid-input {:message (str "Field '" (name first-field) "' validation failed: " (get errors first-field))
                                         :field   first-field
                                         :errors  errors}))))))

(defn validate-update-todo>
  "Validates and coerces payload for updating a todo.
   Yields an effect with coerced map on success, or fails with `:todo/invalid-input`."
  [payload]
  (cond
    (not (map? payload))
    (fx/fail> :todo/invalid-input {:message "Request body must be a JSON object"})

    :else
    (let [coerced (coerce UpdateTodoPayload payload)
          errors  (explain-errors UpdateTodoPayload coerced)]
      (if (nil? errors)
        (fx/succeed> (cond-> coerced
                       (contains? coerced :title) (update :title str/trim)
                       (and (contains? coerced :description) (some? (:description coerced))) (update :description str/trim)))
        (let [first-field (first (keys errors))]
          (fx/fail> :todo/invalid-input {:message (str "Field '" (name first-field) "' validation failed: " (get errors first-field))
                                         :field   first-field
                                         :errors  errors}))))))

(defn coerce-filter>
  "Coerces completed query filter parameter to boolean or nil."
  [param]
  (if (nil? param)
    (fx/succeed> nil)
    (let [coerced (coerce CompletedFilterSchema param)]
      (if (boolean? coerced)
        (fx/succeed> coerced)
        (fx/succeed> nil)))))

(defn coerce-id>
  "Coerces ID string/number into a positive integer.
   Fails with `:todo/invalid-input` if ID is invalid."
  [id-val]
  (let [coerced (coerce IdSchema id-val)]
    (if (and (integer? coerced) (pos? coerced))
      (fx/succeed> coerced)
      (fx/fail> :todo/invalid-input {:message "Todo ID must be a valid positive integer"
                                     :field   :id}))))
