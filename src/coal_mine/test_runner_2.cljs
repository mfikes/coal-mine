(ns coal-mine.test-runner-2
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
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
