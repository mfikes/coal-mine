(ns coal-mine.script
  (:refer-clojure :exclude [test])
  (:require
   [cljs.build.api]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def problem-numbers (->> (range 1 172)
                       (remove #{87 109 123 126 129 133 136 139 142 149 151 154 155 159 160 163 165 167 169 170})))

(def part-numbers (range 1 6))

(def compile-heap "-Xmx3G")

(def test-heap "-max-old-space-size=3584")

(def output-dir ".coal_mine_out")

(def test-script (str output-dir "/main.js"))

(defn create-progress-thread [period message]
  (doto
   (Thread. #(while (try
                      (Thread/sleep period)
                      true
                      (catch InterruptedException _
                        false))
               (println "Still running" message "...")))
    .start))

(defmacro with-time [msg & body]
  `(let [begin# (System/currentTimeMillis)]
     (try
       (println "Running" ~msg "...")
       ~@body
       (finally
         (println "Finished running" ~msg "(Elapsed time:" (quot (- (System/currentTimeMillis) begin#) 1000) "s)\n")))))

(defmacro with-progress [msg & body]
  `(let [progress-thread# (create-progress-thread 300000 ~msg)]
     (with-time ~msg
       (try
         ~@body
         (finally
           (.interrupt progress-thread#))))))

(defn run-subprocess [fun & args]
  (with-progress (string/join " " (concat [fun] args))
    (let [results (apply shell/sh (concat ["java" "-cp" (System/getProperty "java.class.path") compile-heap "clojure.main" "-m" "coal-mine.script" fun] (map str args)))]
      (when-not (zero? (:exit results))
        (throw (ex-info "subprocess failed" results))))))

(defn delete-recursively [fname]
  (doseq [f (reverse (file-seq (io/file fname)))]
    (io/delete-file f true)))

(defn build [source main]
  (cljs.build.api/build source
    {:optimizations  :none
     :parallel-build true
     :target         :nodejs
     :main           main
     :source-map     false
     :output-dir     output-dir
     :output-to      test-script}))

(defn build-problem [problem]
  (let [source (.getFile (io/resource (str "coal_mine/problem_" problem ".cljc")))
        main   (symbol (str "coal-mine.problem-" problem))]
    (build source (symbol main))))

(defn build-part [part]
  (let [source (.getFile (io/resource (str "coal_mine/test_runner_" part ".cljs")))
        main   (symbol (str "coal-mine.test-runner-" part))]
    (build source (symbol main))))

(defn run-test [progress-indicator]
  (with-progress progress-indicator
    (let [results (shell/sh "node" test-heap test-script)]
      (println (:out results))
      (println (:err results))
      (if-not (and (zero? (:exit results))
                   (re-find #"0 failures, 0 errors." (:out results)))
        (throw (ex-info "Tests failed" results))
        (map #(Long/parseLong %) (rest (first (re-seq #"Ran (\d+) tests containing (\d+) assertions." (:out results)))))))))

(defn test-problem [problem]
  (run-subprocess "build-problem" problem)
  (run-test (str "coal-mine.problem-" problem " in Node")))

(defn test-part [part]
  (run-subprocess "build-part" part)
  (run-test (str "coal-mine.test-runner-" part " in Node")))

(defn test-each [display-indicator test-fn list]
  (with-time display-indicator
    (let [subtotals (doall (map test-fn list))]
      (println "Ran a total of" (apply + (map first subtotals))
        "tests containing" (apply + (map second subtotals)) "assertions."))))

(defn test-problems []
  (test-each "all problems" test-problem problem-numbers))

(defn test []
  (test-each "all parts" test-part part-numbers))

(defn terminal? [fun]
  (contains? #{"test" "test-part"} fun))

(defn -main [fun & args]
  (try
    (case fun
      "build-part" (build-part (Long/parseLong (first args)))
      "test-part" (test-part (Long/parseLong (first args)))
      "build-problem" (build-problem (Long/parseLong (first args)))
      "test-problem" (test-problem (Long/parseLong (first args)))
      "test-problems" (test-problems)
      "test" (test))
    (finally
      (shutdown-agents)
      (when (terminal? fun)
        (delete-recursively output-dir)))))
