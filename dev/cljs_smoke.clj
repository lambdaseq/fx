(ns cljs-smoke
  (:require [clojure.java.io :as io]
            [cljs.build.api :as cljs]))

(def entry-source
  "(ns cljs-smoke.entry
     (:require [fx.core]
               [fx.layer]
               [fx.utils]
               [fx.async]
               [fx.schedule]
               [fx.observability.log]
               [fx.observability.trace]
               [fx.observability.metrics]
               [fx.observability.diagnostics]
               [fx.observability.telemetry]))")

(defn -main [& _]
  (io/make-parents "target/cljs_smoke_entry.cljs")
  (spit "target/cljs_smoke_entry.cljs" entry-source)
  (cljs/build "target/cljs_smoke_entry.cljs"
              {:output-to "target/cljs-smoke.js"
               :output-dir "target/cljs-smoke-out"
               :main 'cljs-smoke.entry
               :optimizations :none})
  (println "ClojureScript smoke compilation succeeded."))
