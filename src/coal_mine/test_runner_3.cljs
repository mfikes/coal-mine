(ns coal-mine.test-runner-3
  (:require
   [cljs.core.specs.alpha]
   [clojure.test]
   coal-mine.problem-85
   coal-mine.problem-86
   coal-mine.problem-88
   coal-mine.problem-89
   coal-mine.problem-90
   coal-mine.problem-91
   coal-mine.problem-92
   coal-mine.problem-93
   coal-mine.problem-94
   coal-mine.problem-95
   coal-mine.problem-96
   coal-mine.problem-97
   coal-mine.problem-98
   coal-mine.problem-99
   coal-mine.problem-100
   coal-mine.problem-101))

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-85
    'coal-mine.problem-86
    'coal-mine.problem-88
    'coal-mine.problem-89
    'coal-mine.problem-90
    'coal-mine.problem-91
    'coal-mine.problem-92
    'coal-mine.problem-93
    'coal-mine.problem-94
    'coal-mine.problem-95
    'coal-mine.problem-96
    'coal-mine.problem-97
    'coal-mine.problem-98
    'coal-mine.problem-99
    'coal-mine.problem-100
    'coal-mine.problem-101))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
