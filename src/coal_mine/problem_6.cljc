(ns coal-mine.problem-6
  (:require [coal-mine.checks :refer [defcheck-6] :rename {defcheck-6 defcheck}]
            [clojure.test]))

(defcheck solution-cd2faa4a
  ':a ':b ':c)

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-6))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
