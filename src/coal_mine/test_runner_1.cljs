(ns coal-mine.test-runner-1
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-1
   coal-mine.problem-2
   coal-mine.problem-3
   coal-mine.problem-4
   coal-mine.problem-5
   coal-mine.problem-6
   coal-mine.problem-7
   coal-mine.problem-8
   coal-mine.problem-9
   coal-mine.problem-10
   coal-mine.problem-11
   coal-mine.problem-12
   coal-mine.problem-13
   coal-mine.problem-14
   coal-mine.problem-15
   coal-mine.problem-16))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-1
    'coal-mine.problem-2
    'coal-mine.problem-3
    'coal-mine.problem-4
    'coal-mine.problem-5
    'coal-mine.problem-6
    'coal-mine.problem-7
    'coal-mine.problem-8
    'coal-mine.problem-9
    'coal-mine.problem-10
    'coal-mine.problem-11
    'coal-mine.problem-12
    'coal-mine.problem-13
    'coal-mine.problem-14
    'coal-mine.problem-15
    'coal-mine.problem-16))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
