(ns release-check-test
  (:require [clojure.test :refer [deftest is]]
            [release-check :as release]))

(deftest release-contract-test
  (is (= "0.1.0" release/version))
  (is (nil? (release/check-release!))))
