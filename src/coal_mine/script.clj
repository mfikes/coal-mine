(ns coal-mine.script
  (:refer-clojure :exclude [test])
  (:require
   [cljs.build.api]
   [clojure.java.shell :as shell]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def compile-heap "-Xmx3G")

(def test-heap "-max-old-space-size=3072")

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

(defmacro with-progress [msg & body]
  `(let [begin# (System/currentTimeMillis)
         progress-thread# (create-progress-thread 300000 ~msg)]
     (try
       (println "Running" ~msg "...")
       ~@body
       (finally
         (println "Finished running" ~msg "(Elapsed time:" (quot (- (System/currentTimeMillis) begin#) 1000) "s)")
         (.interrupt progress-thread#)))))

(defn get-classpath []
  (->> (.. (ClassLoader/getSystemClassLoader) getURLs)
    (map (memfn getFile))
    (string/join ":")))

(defn run-subprocess [fun & args]
  (with-progress (string/join " " (concat [fun] args))
    (let [results (apply shell/sh (concat ["java" "-cp" (get-classpath) compile-heap "clojure.main" "-m" "coal-mine.script" fun] (map str args)))]
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
     :output-dir     output-dir
     :output-to      test-script}))

(defn build-part [part]
  (let [source (.getFile (io/resource (str "coal_mine/test_runner_" part ".cljs")))
        main   (symbol (str "coal-mine.test-runner-" part))]
    (build source (symbol main))))

(defn run-test-part [part]
  (with-progress (str "coal-mine.test-runner-" part " in Node")
    (let [results (shell/sh "node" test-heap test-script)]
      (println (:out results))
      (println (:err results))
      (if-not (and (zero? (:exit results))
                   (re-find #"0 failures, 0 errors." (:out results)))
        (throw (ex-info "Tests failed" results))
        (map #(Long/parseLong %) (rest (first (re-seq #"Ran (\d+) tests containing (\d+) assertions." (:out results)))))))))

(defn test-part [part]
  ;; We run the build and test phases as two separate, memory-heavy processes
  (run-subprocess "build-part" part)
  (run-test-part part))

(defn test []
  (let [subtotals (doall (map test-part [1 2 3 4 5]))]
    (println "Ran a total of" (apply + (map first subtotals))
      "tests containing" (apply + (map second subtotals)) "assertions.")))

(defn terminal? [fun]
  (contains? #{"test" "test-part"} fun))

(defn -main [fun & args]
  (try
    (case fun
      "build-part" (build-part (Long/parseLong (first args)))
      "test-part" (test-part (Long/parseLong (first args)))
      "test" (test))
    (catch Throwable t
      (.printStackTrace t))
    (finally
      (when (terminal? fun)
        (delete-recursively output-dir))
      (System/exit 0))))
