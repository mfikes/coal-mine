(ns coal-mine.test-runner-4
  (:require
   [clojure.test]
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
   coal-mine.problem-112
   coal-mine.problem-113
   coal-mine.problem-114
   coal-mine.problem-115
   coal-mine.problem-116
   coal-mine.problem-117
   coal-mine.problem-118
   coal-mine.problem-119
   coal-mine.problem-120
   coal-mine.problem-121
   coal-mine.problem-122
   coal-mine.problem-124
   coal-mine.problem-125
   coal-mine.problem-127
   coal-mine.problem-128))

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
    'coal-mine.problem-112
    'coal-mine.problem-113
    'coal-mine.problem-114
    'coal-mine.problem-115
    'coal-mine.problem-116
    'coal-mine.problem-117
    'coal-mine.problem-118
    'coal-mine.problem-119
    'coal-mine.problem-120
    'coal-mine.problem-121
    'coal-mine.problem-122
    'coal-mine.problem-124
    'coal-mine.problem-125
    'coal-mine.problem-127
    'coal-mine.problem-128))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
