(ns build
  (:require [clojure.tools.build.api :as b]))

(defn clean
  "Deletes the target directory and build artifacts."
  [opts]
  (println "Cleaning target directory...")
  (b/delete {:path "target"})
  opts)

(defn run
  "Runs the example Todo application server.
   Options:
     :port      - port number to listen on (default: 3000 or env PORT)
     :main-args - vector of CLI arguments to pass to todo.main"
  [opts]
  (println "\n=== Starting Todo Application ===")
  (let [basis     (b/create-basis {})
        args      (or (:main-args opts)
                      (when-let [p (:port opts)] [(str p)])
                      [])
        cmds      (b/java-command
                   {:basis      basis
                    :main      'clojure.main
                    :main-args (into ["-m" "todo.main"] args)})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit)
      (throw (ex-info "Application exited with non-zero status" {:exit exit}))))
  opts)

(defn test
  "Runs the test suite for the Todo application.
   Options:
     :dirs    - vector of test directories to run (default: [\"test\"])
     :aliases - vector of extra deps aliases (default: [:test])"
  [opts]
  (println "\n=== Running Todo Example Tests ===")
  (let [aliases  (or (:aliases opts) [:test])
        basis    (b/create-basis {:aliases aliases})
        dirs     (or (:dirs opts) ["test"])
        dir-args (mapcat (fn [d] ["-d" d]) dirs)
        cmds     (b/java-command
                  {:basis      basis
                   :main      'clojure.main
                   :main-args (into ["-m" "cognitect.test-runner"] dir-args)})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {:exit exit}))))
  opts)
