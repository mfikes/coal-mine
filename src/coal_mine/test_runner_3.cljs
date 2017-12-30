(ns coal-mine.test-runner-3
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-33
   coal-mine.problem-34
   coal-mine.problem-35
   coal-mine.problem-36
   coal-mine.problem-37
   coal-mine.problem-38
   coal-mine.problem-39
   coal-mine.problem-40
   coal-mine.problem-41
   coal-mine.problem-42
   coal-mine.problem-43
   coal-mine.problem-44
   coal-mine.problem-45
   coal-mine.problem-46
   coal-mine.problem-47
   coal-mine.problem-48))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-33
    'coal-mine.problem-34
    'coal-mine.problem-35
    'coal-mine.problem-36
    'coal-mine.problem-37
    'coal-mine.problem-38
    'coal-mine.problem-39
    'coal-mine.problem-40
    'coal-mine.problem-41
    'coal-mine.problem-42
    'coal-mine.problem-43
    'coal-mine.problem-44
    'coal-mine.problem-45
    'coal-mine.problem-46
    'coal-mine.problem-47
    'coal-mine.problem-48))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
