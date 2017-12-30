(ns coal-mine.test-runner-7
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-97
   coal-mine.problem-98
   coal-mine.problem-99
   coal-mine.problem-100
   coal-mine.problem-101
   coal-mine.problem-102
   coal-mine.problem-103
   coal-mine.problem-104
   coal-mine.problem-105
   coal-mine.problem-106
   coal-mine.problem-107
   coal-mine.problem-108
   coal-mine.problem-110
   coal-mine.problem-111
   coal-mine.problem-112))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-97
    'coal-mine.problem-98
    'coal-mine.problem-99
    'coal-mine.problem-100
    'coal-mine.problem-101
    'coal-mine.problem-102
    'coal-mine.problem-103
    'coal-mine.problem-104
    'coal-mine.problem-105
    'coal-mine.problem-106
    'coal-mine.problem-107
    'coal-mine.problem-108
    'coal-mine.problem-110
    'coal-mine.problem-111
    'coal-mine.problem-112))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
