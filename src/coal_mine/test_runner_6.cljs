(ns coal-mine.test-runner-6
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-81
   coal-mine.problem-82
   coal-mine.problem-83
   coal-mine.problem-84
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
   coal-mine.problem-96))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-81
    'coal-mine.problem-82
    'coal-mine.problem-83
    'coal-mine.problem-84
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
    'coal-mine.problem-96))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
