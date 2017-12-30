(ns coal-mine.test-runner-10
  (:require
   [clojure.test]
   [cljs.nodejs :as nodejs]
   coal-mine.problem-146
   coal-mine.problem-147
   coal-mine.problem-148
   coal-mine.problem-150
   coal-mine.problem-152
   coal-mine.problem-153
   coal-mine.problem-156
   coal-mine.problem-157
   coal-mine.problem-158
   coal-mine.problem-160
   coal-mine.problem-161
   coal-mine.problem-162
   coal-mine.problem-164
   coal-mine.problem-166
   coal-mine.problem-168
   coal-mine.problem-171))

(nodejs/enable-util-print!)

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-146
    'coal-mine.problem-147
    'coal-mine.problem-148
    'coal-mine.problem-150
    'coal-mine.problem-152
    'coal-mine.problem-153
    'coal-mine.problem-156
    'coal-mine.problem-157
    'coal-mine.problem-158
    'coal-mine.problem-160
    'coal-mine.problem-161
    'coal-mine.problem-162
    'coal-mine.problem-164
    'coal-mine.problem-166
    'coal-mine.problem-168
    'coal-mine.problem-171))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
