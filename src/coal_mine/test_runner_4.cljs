(ns coal-mine.test-runner-4
  (:require
   [cljs.core.specs.alpha]
   [clojure.test]
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
   coal-mine.problem-112
   coal-mine.problem-113
   coal-mine.problem-114))

(defn run-tests []
  (clojure.test/run-tests
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
    'coal-mine.problem-112
    'coal-mine.problem-113
    'coal-mine.problem-114))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
