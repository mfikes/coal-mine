(ns coal-mine.problem-131
  (:require [coal-mine.checks :refer [defcheck-131] :rename {defcheck-131 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-131))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

