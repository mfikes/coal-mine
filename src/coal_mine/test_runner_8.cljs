(ns coal-mine.test-runner-8
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
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

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
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
