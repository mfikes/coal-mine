(ns coal-mine.problem-105
  (:require [coal-mine.checks :refer [defcheck-105] :rename {defcheck-105 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-105))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

