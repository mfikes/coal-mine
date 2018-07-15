(ns coal-mine.test-runner-5
  (:require
   [cljs.core.specs.alpha]
   [clojure.test]
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
   coal-mine.problem-145
   coal-mine.problem-146
   coal-mine.problem-147
   coal-mine.problem-148
   coal-mine.problem-150
   coal-mine.problem-152
   coal-mine.problem-153
   coal-mine.problem-156
   coal-mine.problem-157
   coal-mine.problem-158
   coal-mine.problem-161
   coal-mine.problem-162
   coal-mine.problem-164
   coal-mine.problem-166
   coal-mine.problem-168
   coal-mine.problem-171))

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
    'coal-mine.problem-145
    'coal-mine.problem-146
    'coal-mine.problem-147
    'coal-mine.problem-148
    'coal-mine.problem-150
    'coal-mine.problem-152
    'coal-mine.problem-153
    'coal-mine.problem-156
    'coal-mine.problem-157
    'coal-mine.problem-158
    'coal-mine.problem-161
    'coal-mine.problem-162
    'coal-mine.problem-164
    'coal-mine.problem-166
    'coal-mine.problem-168
    'coal-mine.problem-171))

(defn -main []
  (run-tests))

(set! *main-cli-fn* -main)
