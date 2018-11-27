(ns coal-mine.problem-106
  (:require [coal-mine.checks :refer [defcheck-106] :rename {defcheck-106 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-106))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

