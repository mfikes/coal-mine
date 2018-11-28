(ns coal-mine.problem-148
  (:require [coal-mine.checks :refer [defcheck-148] :rename {defcheck-148 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-148))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

