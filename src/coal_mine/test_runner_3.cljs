(ns coal-mine.test-runner-3
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-65
   coal-mine.problem-66
   coal-mine.problem-67
   coal-mine.problem-68
   coal-mine.problem-69
   coal-mine.problem-70
   coal-mine.problem-71
   coal-mine.problem-72
   coal-mine.problem-73
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
    'coal-mine.problem-65
    'coal-mine.problem-66
    'coal-mine.problem-67
    'coal-mine.problem-68
    'coal-mine.problem-69
    'coal-mine.problem-70
    'coal-mine.problem-71
    'coal-mine.problem-72
    'coal-mine.problem-73
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
