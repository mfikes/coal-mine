(ns coal-mine.test-runner-4
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-49
   coal-mine.problem-50
   coal-mine.problem-51
   coal-mine.problem-52
   coal-mine.problem-53
   coal-mine.problem-54
   coal-mine.problem-55
   coal-mine.problem-56
   coal-mine.problem-57
   coal-mine.problem-58
   coal-mine.problem-59
   coal-mine.problem-60
   coal-mine.problem-61
   coal-mine.problem-62
   coal-mine.problem-63
   coal-mine.problem-64))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-49
    'coal-mine.problem-50
    'coal-mine.problem-51
    'coal-mine.problem-52
    'coal-mine.problem-53
    'coal-mine.problem-54
    'coal-mine.problem-55
    'coal-mine.problem-56
    'coal-mine.problem-57
    'coal-mine.problem-58
    'coal-mine.problem-59
    'coal-mine.problem-60
    'coal-mine.problem-61
    'coal-mine.problem-62
    'coal-mine.problem-63
    'coal-mine.problem-64))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
