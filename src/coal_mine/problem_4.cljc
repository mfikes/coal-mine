(ns coal-mine.problem-4
  (:require [coal-mine.checks :refer [defcheck-4] :rename {defcheck-4 defcheck}]
            [clojure.test]))

(defcheck solution-cd2faa4a
  ':a ':b ':c)

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-4))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
