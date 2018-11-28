(ns coal-mine.problem-117
  (:require [coal-mine.checks :refer [defcheck-117] :rename {defcheck-117 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-117))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

