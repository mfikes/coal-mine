(ns coal-mine.problem-158
  (:require [coal-mine.checks :refer [defcheck-158] :rename {defcheck-158 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-158))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
