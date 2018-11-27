(ns coal-mine.problem-146
  (:require [coal-mine.checks :refer [defcheck-146] :rename {defcheck-146 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-146))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

