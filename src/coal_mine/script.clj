(ns coal-mine.script
  (:refer-clojure :exclude [test])
  (:require
   [cljs.build.api]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]))

(defn delete-recursively [fname]
  (doseq [f (reverse (file-seq (io/file fname)))]
    (io/delete-file f)))

(defn create-progress-thread [period message]
  (doto
   (Thread. #(while (try (Thread/sleep period) true
                         (catch Throwable _ false))
               (println message)))
    .start))

(def output-dir ".coal_mine_out")

(def output-to (str output-dir "/main.js"))

(defn build [source main]
  (let [progress-thread (create-progress-thread 800000 (str "Still building " main " ..."))]
    (try
      (println "\nBuilding" main "...")
      (cljs.build.api/build source
        {:optimizations  :none
         :parallel-build true
         :target         :nodejs
         :main           main
         :output-dir     output-dir
         :output-to      output-to})
      (finally (.interrupt progress-thread)))))

(defn test-part [part]
  (let [source (.getFile (io/resource (str "coal_mine/test_runner_" part ".cljs")))
        main   (symbol (str "coal-mine.test-runner-" part))]
    (build source (symbol main)))
  (println "Running" (str "coal-mine.test-runner-" part) "in Node ...")
  (let [progress-thread (create-progress-thread 800000 (str "Still running " (str "coal-mine.test-runner-" part) " ..."))]
    (try
      (let [results (shell/sh "node" "-max-old-space-size=3072" ".coal_mine_out/main.js")]
        (println (:out results))
        (println (:err results))
        (if-not (and (zero? (:exit results))
                     (re-find #"0 failures, 0 errors." (:out results)))
          (throw (ex-info "Tests failed" {}))
          (map #(Long/parseLong %) (rest (first (re-seq #"Ran (\d+) tests containing (\d+) assertions." (:out results)))))))
      (finally (.interrupt progress-thread)))))

(defn test []
  (let [subtotals (doall (map test-part [1 2 3 4 5]))]
    (println "Ran a total of" (apply + (map first subtotals))
      "tests containing" (apply + (map second subtotals)) "assertions.")))

(defn -main [fun & args]
  (try
    (case fun
      "test-part" (test-part (Long/parseLong (first args)))
      "test" (test))
    (finally (delete-recursively output-dir))))
