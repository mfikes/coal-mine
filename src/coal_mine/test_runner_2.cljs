(ns coal-mine.test-runner-2
  (:require
   [clojure.test]
   coal-mine.problem-56
   coal-mine.problem-57
   coal-mine.problem-58
   coal-mine.problem-59
   coal-mine.problem-60
   coal-mine.problem-61
   coal-mine.problem-62
   coal-mine.problem-63
   coal-mine.problem-64
   coal-mine.problem-65
   coal-mine.problem-66
   coal-mine.problem-67
   coal-mine.problem-68
   coal-mine.problem-69
   coal-mine.problem-70
   coal-mine.problem-71
   coal-mine.problem-72
   coal-mine.problem-73))

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-56
    'coal-mine.problem-57
    'coal-mine.problem-58
    'coal-mine.problem-59
    'coal-mine.problem-60
    'coal-mine.problem-61
    'coal-mine.problem-62
    'coal-mine.problem-63
    'coal-mine.problem-64
    'coal-mine.problem-65
    'coal-mine.problem-66
    'coal-mine.problem-67
    'coal-mine.problem-68
    'coal-mine.problem-69
    'coal-mine.problem-70
    'coal-mine.problem-71
    'coal-mine.problem-72
    'coal-mine.problem-73))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
