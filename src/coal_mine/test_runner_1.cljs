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
   coal-mine.problem-16
   coal-mine.problem-17
   coal-mine.problem-18
   coal-mine.problem-19
   coal-mine.problem-20
   coal-mine.problem-21
   coal-mine.problem-22
   coal-mine.problem-23
   coal-mine.problem-24
   coal-mine.problem-25
   coal-mine.problem-26
   coal-mine.problem-27
   coal-mine.problem-28
   coal-mine.problem-29
   coal-mine.problem-30
   coal-mine.problem-31
   coal-mine.problem-32))

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
    'coal-mine.problem-16
    'coal-mine.problem-17
    'coal-mine.problem-18
    'coal-mine.problem-19
    'coal-mine.problem-20
    'coal-mine.problem-21
    'coal-mine.problem-22
    'coal-mine.problem-23
    'coal-mine.problem-24
    'coal-mine.problem-25
    'coal-mine.problem-26
    'coal-mine.problem-27
    'coal-mine.problem-28
    'coal-mine.problem-29
    'coal-mine.problem-30
    'coal-mine.problem-31
    'coal-mine.problem-32))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
