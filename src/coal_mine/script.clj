(ns coal-mine.script
  (:refer-clojure :exclude [test])
  (:require
   [cljs.build.api]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]))

(defn build [source main]
  (let [progress-thread (doto
                         (Thread. #(while (try (Thread/sleep 500000) true
                                               (catch Throwable _ false))
                                     (println "Still building" main "...")))
                          .start)]
    (try
      (println "\nBuilding" main "...")
      (cljs.build.api/build source
        {:optimizations  :none
         :parallel-build true
         :target         :nodejs
         :main           main
         :output-dir     ".coal_mine_out"
         :output-to      ".coal_mine_out/main.js"})
      (finally (.interrupt progress-thread)))))

(defn test-part [part]
  (let [source (.getFile (io/resource (str "coal_mine/test_runner_" part ".cljs")))
        main   (symbol (str "coal-mine.test-runner-" part))]
    (build source (symbol main)))
  (println "Running" (str "coal-mine.test-runner-" part) "in Node ...")
  (let [results (shell/sh "node" "-max-old-space-size=3072" ".coal_mine_out/main.js")]
    (println (:out results))
    (println (:err results))
    (if-not (and (zero? (:exit results))
                 (re-find #"0 failures, 0 errors." (:out results)))
      (throw (ex-info "Tests failed" {}))
      (map #(Long/parseLong %) (rest (first (re-seq #"Ran (\d+) tests containing (\d+) assertions." (:out results))))))))

(defn test []
  (let [subtotals (doall (map test-part [1 2]))]
    (prn 'subtotals subtotals)
    (println "Ran a total of" (apply + (map first subtotals))
      "tests containing" (apply + (map second subtotals)) "assertions.")))

(defn -main [fun & args]
  (case fun
    "test-part" (test-part (Long/parseLong (first args)))
    "test" (test)))
