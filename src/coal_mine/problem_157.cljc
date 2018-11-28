(ns coal-mine.problem-157
  (:require [coal-mine.checks :refer [defcheck-157] :rename {defcheck-157 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-157))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
