(ns coal-mine.test-runner-2
  (:require
   [cljs.core.specs.alpha]
   [clojure.test]
   coal-mine.problem-74
   coal-mine.problem-75
   coal-mine.problem-76
   coal-mine.problem-77
   coal-mine.problem-78
   coal-mine.problem-79
   coal-mine.problem-80
   coal-mine.problem-81
   coal-mine.problem-82
   coal-mine.problem-83
   coal-mine.problem-84
   coal-mine.problem-85))

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-74
    'coal-mine.problem-75
    'coal-mine.problem-76
    'coal-mine.problem-77
    'coal-mine.problem-78
    'coal-mine.problem-79
    'coal-mine.problem-80
    'coal-mine.problem-81
    'coal-mine.problem-82
    'coal-mine.problem-83
    'coal-mine.problem-84
    'coal-mine.problem-85))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
