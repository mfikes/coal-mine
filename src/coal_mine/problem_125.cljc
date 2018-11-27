(ns coal-mine.problem-125
  (:require [coal-mine.checks :refer [defcheck-125] :rename {defcheck-125 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-125))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

