(ns release-check
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def version "0.1.0")

(def supported-namespaces
  '[fx.core
    fx.layer
    fx.utils
    fx.typed
    fx.jdbc
    fx.jdbc.sql
    fx.ring
    fx.ring.response
    fx.observability.log
    fx.observability.trace
    fx.observability.metrics
    fx.observability.diagnostics
    fx.observability.telemetry
    fx.schedule
    fx.http-client
    fx.async])

(def documentation-files
  ["README.md"
   "doc/intro.md"
   "docs/llms.txt"
   "docs/llms-full.txt"
   "example/README.md"
   "modules/fx-core/README.md"
   "modules/fx-typed/README.md"
   "modules/fx-jdbc/README.md"
   "modules/fx-ring/README.md"
   "modules/fx-observability/README.md"
   "modules/fx-schedule/README.md"
   "modules/fx-http-client/README.md"
   "modules/fx-async/README.md"])

(def release-metadata-files
  ["pom.xml"])

(def dependency-files
  ["modules/fx-core/deps.edn"
   "modules/fx-typed/deps.edn"
   "modules/fx-jdbc/deps.edn"
   "modules/fx-ring/deps.edn"
   "modules/fx-observability/deps.edn"
   "modules/fx-schedule/deps.edn"
   "modules/fx-http-client/deps.edn"
   "modules/fx-async/deps.edn"])

(defn- read-file [path]
  (slurp (io/file path)))

(defn- fail! [message]
  (throw (ex-info message {})))

(defn- require-supported-namespaces! []
  (doseq [namespace-sym supported-namespaces]
    (try
      (require namespace-sym)
      (catch Throwable error
        (fail! (str "Supported namespace failed to load: " namespace-sym
                    " (" (.getMessage error) ")"))))))

(defn- check-documentation-versions! []
  (doseq [path documentation-files]
    (let [content (read-file path)]
      (when (re-find #"0\.0\.1-alpha|0\.1\.0-SNAPSHOT|0\.2\.0" content)
        (fail! (str "Stale release version in " path))))))

(defn- check-release-metadata! []
  (doseq [path release-metadata-files]
    (let [content (read-file path)]
      (when-not (str/includes? content "<version>0.1.0</version>")
        (fail! (str "Release metadata is not at " version ": " path)))
      (when (re-find #"0\.1\.0-SNAPSHOT|0\.0\.1-alpha" content)
        (fail! (str "Stale release metadata in " path))))))

(defn- check-dependency-versions! []
  (doseq [path dependency-files]
    (let [content (read-file path)]
      (when (re-find #"io\.github\.conjurernix/fx\.[a-z-]+\s+\{:mvn/version\s+\"(?!0\.1\.0\")"
                     content)
        (fail! (str "Internal dependency is not at " version ": " path))))))

(defn- check-documentation-namespaces! []
  (doseq [namespace-sym supported-namespaces]
    (let [namespace-name (str namespace-sym)
          documented? (some #(str/includes? (read-file %) namespace-name)
                            documentation-files)]
      (when-not documented?
        (fail! (str "Supported namespace is undocumented: " namespace-name)))))
  (doseq [path documentation-files]
    (when (re-find #"\[fx\.observability\s" (read-file path))
      (fail! (str "Documentation references nonexistent facade namespace in " path)))))

(defn- public-api-symbols []
  (into #{}
        (mapcat (fn [namespace-sym]
                  (keys (ns-publics (find-ns namespace-sym))))
                supported-namespaces)))

(defn- check-documented-symbols! []
  (let [symbols (public-api-symbols)
        documented-symbols
        (for [path (remove #{"example/README.md"} documentation-files)
              token (re-seq #"`([A-Za-z][A-Za-z0-9*+!?_<>/-]*[>!?])`"
                            (read-file path))
              :let [symbol-name (last (str/split (second token) #"/"))]
              :when (re-find #"[>!?]$" symbol-name)]
          [path symbol-name])
        missing (remove (fn [[_ symbol-name]]
                          (contains? (set (map name symbols)) symbol-name))
                        documented-symbols)]
    (when (seq missing)
      (fail! (str "Documentation references unsupported symbols: "
                  (pr-str (vec (take 10 missing))))))))

(defn- check-removed-aliases! []
  (require 'fx.ring)
  (when (ns-resolve 'fx.ring 'wrap-fx-failure)
    (fail! "Removed synonym alias fx.ring/wrap-fx-failure is still public")))

(defn check-release! []
  (require-supported-namespaces!)
  (check-documentation-versions!)
  (check-release-metadata!)
  (check-dependency-versions!)
  (check-documentation-namespaces!)
  (check-documented-symbols!)
  (check-removed-aliases!)
  (println (str "Release contract is valid for " version ".")))

(defn -main [& _]
  (check-release!))
