(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib-modules
  {:core  {:lib 'io.github.conjurernix/fx.core  :dir "modules/fx-core"  :description "Declarative, purely functional effect system for Clojure and ClojureScript"}
   :typed {:lib 'io.github.conjurernix/fx.typed :dir "modules/fx-typed" :description "Typed Clojure annotations for fx effect system"}
   :jdbc  {:lib 'io.github.conjurernix/fx.jdbc  :dir "modules/fx-jdbc"  :description "Effectful, purely functional JDBC database access for fx"}
   :ring  {:lib 'io.github.conjurernix/fx.ring  :dir "modules/fx-ring"  :description "Ring HTTP middleware and response combinators for fx"}})

(def default-all-modules
  ["modules/fx-core" "modules/fx-typed" "modules/fx-jdbc" "modules/fx-ring"])

(def internal-libs
  (into #{} (map :lib (vals lib-modules))))

(def local->mvn-internal
  {'fx/core  'io.github.conjurernix/fx.core
   'fx/typed 'io.github.conjurernix/fx.typed
   'fx/jdbc  'io.github.conjurernix/fx.jdbc
   'fx/ring  'io.github.conjurernix/fx.ring})

(defn compute-version
  "Derives version from options or git tags.
   All modules share the same version and update in lockstep."
  [opts]
  (or (:version opts)
      (try
        (let [tag (b/git-process {:git-args ["describe" "--tags" "--abbrev=0"]})]
          (when (and tag (seq (str/trim tag)))
            (let [v (str/replace (str/trim tag) #"^v" "")]
              (if (re-find #"^\d+\.\d+" v)
                v
                (str "0.1.0-" v)))))
        (catch Exception _ nil))
      "0.1.0-SNAPSHOT"))

(defn version
  "Prints and returns the current derived or overridden version.
   Options:
     :version - optional version override"
  [opts]
  (let [ver (compute-version opts)]
    (println ver)
    ver))

(def current-version version)
(def show-version version)

(defn- resolve-lib-modules
  [opts]
  (let [mods (or (:modules opts) (:submodules opts) (keys lib-modules))]
    (mapv (fn [m]
            (let [k (if (keyword? m) m (keyword (str m)))
                  normalized-k (or (when (contains? lib-modules k) k)
                                   (keyword (str/replace (name k) #"^fx-" "")))]
              (or (get lib-modules normalized-k)
                  (throw (ex-info (str "Unknown library module: " m)
                                  {:available (keys lib-modules)})))))
          (if (coll? mods) mods [mods]))))

(defn- module-basis
  [dir ver]
  (let [raw-basis (binding [b/*project-root* (b/resolve-path dir)]
                    (b/create-basis {:project "deps.edn"}))]
    (reduce (fn [basis [local-lib mvn-lib]]
              (cond
                (contains? (:libs basis) local-lib)
                (-> basis
                    (update :libs dissoc local-lib)
                    (assoc-in [:libs mvn-lib] {:mvn/version ver}))

                (contains? (:libs basis) mvn-lib)
                (assoc-in basis [:libs mvn-lib] {:mvn/version ver})

                :else basis))
            raw-basis
            local->mvn-internal)))

(defn- module-src-dirs
  [dir]
  (let [existing (filter #(-> (io/file dir %) .exists) ["src" "resources"])]
    (mapv #(str dir "/" %) (if (seq existing) existing ["src"]))))

(defn- jar-opts-for-module
  [{:keys [lib dir description]} ver opts]
  (let [class-dir (str "target/" dir "/classes")
        jar-file  (format "target/%s-%s.jar" (name lib) ver)
        basis     (module-basis dir ver)
        src-dirs  (module-src-dirs dir)]
    (merge opts
           {:lib       lib
            :version   ver
            :jar-file  jar-file
            :scm       (merge {:url "https://github.com/conjurernix/fx"
                               :tag (str "v" ver)}
                              (:scm opts))
            :pom-data  [[:description (or description "Declarative, purely functional effect system for Clojure and ClojureScript")]
                        [:url "https://github.com/conjurernix/fx"]
                        [:licenses
                         [:license
                          [:name "Eclipse Public License 1.0"]
                          [:url "https://opensource.org/license/epl-1-0"]
                          [:distribution "repo"]]]]
            :basis     basis
            :class-dir class-dir
            :target    (str "target/" dir)
            :src-dirs  src-dirs})))

(defn clean
  "Deletes the target directory and build artifacts."
  [opts]
  (println "Cleaning target directory...")
  (b/delete {:path "target"})
  opts)

(defn test
  "Runs the test suite across modules."
  [opts]
  (println "\n=== Running test suite ===")
  (let [basis    (b/create-basis {:aliases [:dev :test]})
        cmds     (b/java-command
                  {:basis      basis
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"
                               "-d" "modules/fx-core/test"
                               "-d" "modules/fx-typed/test"
                               "-d" "modules/fx-jdbc/test"
                               "-d" "modules/fx-ring/test"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {:exit exit}))))
  opts)

(defn jar
  "Builds JARs for all library modules in lockstep version.
   Options:
     :modules - vector/list of modules to build (default: all library modules [:core :typed :jdbc :ring])
     :version - optional version string override (default derived from git tags)"
  [opts]
  (let [ver (compute-version opts)
        targets (resolve-lib-modules opts)]
    (println (str "\nBuilding JARs for version: " ver))
    (doseq [mod targets]
      (let [{:keys [lib dir]} mod
            m-opts (jar-opts-for-module mod ver opts)
            {:keys [class-dir jar-file src-dirs]} m-opts]
        (println (str "\n--- Module: " lib " (" dir ") ---"))
        (println "Writing pom.xml...")
        (b/write-pom m-opts)
        (println "Copying sources...")
        (b/copy-dir {:src-dirs   src-dirs
                     :target-dir class-dir})
        (println (str "Building JAR: " jar-file "..."))
        (b/jar m-opts)
        (b/install m-opts)))
    (println "\nJAR build complete.")
    opts))

(defn install
  "Installs module JARs to the local Maven repository.
   Options:
     :modules - vector/list of modules to install (default: all library modules)
     :version - optional version string override"
  [opts]
  (let [ver (compute-version opts)
        targets (resolve-lib-modules opts)]
    (println (str "\nInstalling JARs locally for version: " ver))
    (doseq [mod targets]
      (let [{:keys [lib dir]} mod
            m-opts (jar-opts-for-module mod ver opts)]
        (println (str "Installing " lib " (" (:jar-file m-opts) ")..."))
        (b/install m-opts)))
    (println "\nLocal installation complete.")
    opts))

(defn deploy
  "Deploys module JARs to Clojars.
   Options:
     :modules - vector/list of modules to deploy (default: all library modules)
     :version - optional version string override"
  [opts]
  (let [ver (compute-version opts)
        targets (resolve-lib-modules opts)]
    (println (str "\nDeploying JARs to Clojars for version: " ver))
    (doseq [mod targets]
      (let [{:keys [lib dir]} mod
            {:keys [jar-file class-dir] :as m-opts} (jar-opts-for-module mod ver opts)]
        (println (str "Deploying " lib " (" jar-file ")..."))
        (dd/deploy (merge {:installer :remote
                           :artifact  (b/resolve-path jar-file)
                           :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}
                          (dissoc opts :modules :submodules :version)))))
    (println "\nDeployment to Clojars complete.")
    opts))

(defn ci
  "Runs the CI pipeline: clean, builds JARs, and runs tests for all modules."
  [opts]
  (clean opts)
  (jar opts)
  (test opts)
  opts)

;; ----------------------------------------------------------------------------
;; Version Bumping
;; ----------------------------------------------------------------------------

(defn parse-version
  "Parses version string into structured map {:major :minor :patch :qualifier :qualifier-num}"
  [v-str]
  (let [clean-v (str/replace (str/trim (or v-str "")) #"^v" "")
        pattern #"^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z]+)(?:\.?(\d+))?)?$"
        [_ major minor patch qualifier q-num] (re-matches pattern clean-v)]
    (when major
      {:major         (parse-long major)
       :minor         (parse-long minor)
       :patch         (parse-long patch)
       :qualifier     qualifier
       :qualifier-num (when q-num (parse-long q-num))})))

(defn format-version
  [{:keys [major minor patch qualifier qualifier-num]}]
  (if qualifier
    (if qualifier-num
      (format "%d.%d.%d-%s%d" major minor patch qualifier qualifier-num)
      (format "%d.%d.%d-%s" major minor patch qualifier))
    (format "%d.%d.%d" major minor patch)))

(defn next-version
  "Calculates the next version based on bump type: :major, :minor, :patch, :alpha, :beta, :rc, :release, :snapshot."
  [current-v bump-type]
  (let [parsed (or (parse-version current-v)
                   {:major 0 :minor 1 :patch 0 :qualifier "alpha" :qualifier-num 1})
        {:keys [major minor patch qualifier qualifier-num]} parsed
        type-key (keyword (name bump-type))]
    (format-version
     (case type-key
       :major
       {:major (inc major) :minor 0 :patch 0}

       :minor
       {:major major :minor (inc minor) :patch 0}

       :patch
       (if (and qualifier (nil? qualifier-num))
         {:major major :minor minor :patch patch}
         {:major major :minor minor :patch (inc patch)})

       :alpha
       (if (= (some-> qualifier str/lower-case) "alpha")
         {:major major :minor minor :patch patch :qualifier "alpha" :qualifier-num (inc (or qualifier-num 0))}
         (if qualifier
           {:major major :minor minor :patch patch :qualifier "alpha" :qualifier-num 1}
           {:major major :minor minor :patch (inc patch) :qualifier "alpha" :qualifier-num 1}))

       :beta
       (if (= (some-> qualifier str/lower-case) "beta")
         {:major major :minor minor :patch patch :qualifier "beta" :qualifier-num (inc (or qualifier-num 0))}
         {:major major :minor minor :patch (if qualifier patch (inc patch)) :qualifier "beta" :qualifier-num 1})

       :rc
       (if (= (some-> qualifier str/lower-case) "rc")
         {:major major :minor minor :patch patch :qualifier "rc" :qualifier-num (inc (or qualifier-num 0))}
         {:major major :minor minor :patch (if qualifier patch (inc patch)) :qualifier "rc" :qualifier-num 1})

       :release
       {:major major :minor minor :patch patch}

       :snapshot
       (if (= (some-> qualifier str/lower-case) "snapshot")
         {:major major :minor minor :patch patch :qualifier "SNAPSHOT"}
         (if qualifier
           {:major major :minor minor :patch patch :qualifier "SNAPSHOT"}
           {:major major :minor minor :patch (inc patch) :qualifier "SNAPSHOT"}))

       (throw (ex-info (str "Unknown bump type: " bump-type ". Expected one of: :major, :minor, :patch, :alpha, :beta, :rc, :release, :snapshot")
                       {:bump-type bump-type}))))))

(defn- update-submodule-deps-version!
  [new-version]
  (let [files ["modules/fx-core/deps.edn"
               "modules/fx-typed/deps.edn"
               "modules/fx-jdbc/deps.edn"
               "modules/fx-ring/deps.edn"
               "deps.edn"]]
    (doseq [f-path files]
      (let [f (io/file f-path)]
        (when (.exists f)
          (let [content (slurp f)
                updated (-> content
                            (str/replace #"(io\.github\.conjurernix/fx\.[a-z]+)(\s+\{:mvn/version\s+\")[^\"]+(\"\})"
                                         (str "$1$2" new-version "$3"))
                            (str/replace #"(fx/(?:core|typed|jdbc|ring))(\s+\{:mvn/version\s+\")[^\"]+(\"\})"
                                         (str "$1$2" new-version "$3")))]
            (if (not= content updated)
              (do
                (spit f updated)
                (println (str "Updated " f-path " to version " new-version)))
              (println (str f-path " already at version " new-version)))))))))

(defn- create-git-tag!
  [new-version]
  (println (str "Creating git tag: " new-version "..."))
  (let [res (b/process {:command-args ["git" "tag" "-a" new-version "-m" (str "Release " new-version)]})]
    (if (zero? (:exit res))
      (println (str "Git tag " new-version " created successfully."))
      (println (str "Note: git tag command exited with status " (:exit res) " (tag might already exist).")))))

(defn bump
  "Bumps version across the repository (git tag and submodule deps.edn).
   Options:
     :type        - bump type (:major, :minor, :patch, :alpha, :beta, :rc, :release, :snapshot) [default: :patch]
     :to/:version - explicit target version (overrides computed next-version)
     :tag         - whether to create git tag (default: true)
     :update-deps - whether to update submodule deps.edn files (default: true)
     :dry-run     - boolean flag to preview bump without making changes"
  [opts]
  (let [cur-ver   (compute-version opts)
        bump-type (or (:type opts) :patch)
        new-ver   (or (:to opts) (:version opts) (next-version cur-ver bump-type))
        tag?      (get opts :tag true)
        deps?     (get opts :update-deps true)
        dry-run?  (:dry-run opts)]
    (println (str "\nBumping version: " cur-ver " -> " new-ver " (type: " bump-type ")"))
    (if dry-run?
      (println "[dry-run] No changes applied.")
      (do
        (when deps?
          (update-submodule-deps-version! new-ver))
        (when tag?
          (create-git-tag! new-ver))))
    (assoc opts :version new-ver :previous-version cur-ver)))

(defn bump-major    [opts] (bump (assoc opts :type :major)))
(defn bump-minor    [opts] (bump (assoc opts :type :minor)))
(defn bump-patch    [opts] (bump (assoc opts :type :patch)))
(defn bump-alpha    [opts] (bump (assoc opts :type :alpha)))
(defn bump-beta     [opts] (bump (assoc opts :type :beta)))
(defn bump-rc       [opts] (bump (assoc opts :type :rc)))
(defn bump-release  [opts] (bump (assoc opts :type :release)))
(defn bump-snapshot [opts] (bump (assoc opts :type :snapshot)))

;; ----------------------------------------------------------------------------
;; Linting, Formatting, and Vulnerability Scanning
;; ----------------------------------------------------------------------------

(defn- resolve-tool-modules [opts]
  (or (:modules opts) (:submodules opts) default-all-modules))

(defn- run-in-submodules
  [{:keys [modules tool-name command-args-fn]}]
  (let [results (reduce (fn [acc mod]
                          (println (str "\n=== Running " tool-name " in " mod " ==="))
                          (let [args (command-args-fn mod)
                                res  (b/process {:command-args args
                                                 :dir mod})]
                            (conj acc (assoc res :module mod))))
                        []
                        modules)
        failures (filter #(not (zero? (:exit %))) results)]
    (when (seq failures)
      (println (str "\n[" tool-name "] Completed with non-zero exit code in: "
                    (mapv :module failures))))
    results))

(defn kondo
  "Runs clj-kondo in submodules.
   Options:
     :modules - vector of submodules to run against (default: [\"modules/fx-core\" \"modules/fx-typed\" \"modules/fx-jdbc\" \"modules/fx-ring\"])
     :args    - custom vector of CLI arguments to pass to clj-kondo"
  [opts]
  (let [modules (resolve-tool-modules opts)]
    (run-in-submodules
     {:modules modules
      :tool-name "clj-kondo"
      :command-args-fn
      (fn [mod]
        (if-let [args (:args opts)]
          (into ["clojure" "-M:clj-kondo"] args)
          (let [existing-dirs (filter #(-> (io/file mod %) .exists) ["src" "test"])]
            (into ["clojure" "-M:clj-kondo" "--lint"] (if (seq existing-dirs) existing-dirs ["."])))))})))

(def clj-kondo kondo)

(defn fmt
  "Runs cljfmt in submodules.
   Options:
     :modules - vector of submodules to run against (default: [\"modules/fx-core\" \"modules/fx-typed\" \"modules/fx-jdbc\" \"modules/fx-ring\"])
     :mode    - \"check\" (default) or \"fix\"
     :fix     - boolean flag, if true runs \"fix\" instead of \"check\"
     :args    - custom vector of CLI arguments to pass to cljfmt"
  [opts]
  (let [modules (resolve-tool-modules opts)
        mode (cond
               (:args opts) nil
               (:fix opts)  "fix"
               (:mode opts) (name (:mode opts))
               :else        "check")]
    (run-in-submodules
     {:modules modules
      :tool-name "cljfmt"
      :command-args-fn
      (fn [_mod]
        (if-let [args (:args opts)]
          (into ["clojure" "-M:cljfmt"] args)
          ["clojure" "-M:cljfmt" mode]))})))

(def cljfmt fmt)

(defn watson
  "Runs clj-watson vulnerability scan in submodules.
   Options:
     :modules - vector of submodules to run against (default: [\"modules/fx-core\" \"modules/fx-typed\" \"modules/fx-jdbc\" \"modules/fx-ring\"])
     :args    - custom vector of CLI arguments to pass to clj-watson (default: [\"scan\" \"-p\" \"deps.edn\"])"
  [opts]
  (let [modules (resolve-tool-modules opts)]
    (run-in-submodules
     {:modules modules
      :tool-name "clj-watson"
      :command-args-fn
      (fn [_mod]
        (into ["clojure" "-M:clj-watson"]
              (or (:args opts) ["scan" "-p" "deps.edn"])))})))

(def clj-watson watson)

(defn all
  "Runs kondo, fmt, and watson in submodules."
  [opts]
  {:kondo  (kondo opts)
   :fmt    (fmt opts)
   :watson (watson opts)})

(def check all)
(def lint all)
