(def clojurescript-version (or (System/getenv "CANARY_CLOJURESCRIPT_VERSION")
                               (System/getenv "CLJS_VERSION")
                               "1.9.946"))
(defproject coal-mine "0.1.0"
  :profiles {:dev
             {:dependencies [[org.clojure/clojurescript ~clojurescript-version]
                             [tubular "1.0.0"]]
              :source-paths ["dev"]}
             :build-release 
             {}}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript ~clojurescript-version]]
  :clean-targets ["out" "target"])
