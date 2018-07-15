(ns coal-mine.test-runner-3
  (:require
   [cljs.core.specs.alpha]
   [clojure.test]
   coal-mine.problem-82
   coal-mine.problem-85
   coal-mine.problem-86
   coal-mine.problem-88
   coal-mine.problem-89
   coal-mine.problem-90
   coal-mine.problem-91
   coal-mine.problem-92))

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-82
    'coal-mine.problem-85
    'coal-mine.problem-86
    'coal-mine.problem-88
    'coal-mine.problem-89
    'coal-mine.problem-90
    'coal-mine.problem-91
    'coal-mine.problem-92))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
