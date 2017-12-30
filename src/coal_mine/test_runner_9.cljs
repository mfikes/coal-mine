(ns coal-mine.test-runner-9
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-130
   coal-mine.problem-131
   coal-mine.problem-132
   coal-mine.problem-134
   coal-mine.problem-135
   coal-mine.problem-137
   coal-mine.problem-138
   coal-mine.problem-140
   coal-mine.problem-141
   coal-mine.problem-143
   coal-mine.problem-144
   coal-mine.problem-145))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-130
    'coal-mine.problem-131
    'coal-mine.problem-132
    'coal-mine.problem-134
    'coal-mine.problem-135
    'coal-mine.problem-137
    'coal-mine.problem-138
    'coal-mine.problem-140
    'coal-mine.problem-141
    'coal-mine.problem-143
    'coal-mine.problem-144
    'coal-mine.problem-145))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
