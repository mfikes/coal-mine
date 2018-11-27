(ns coal-mine.problem-124
  (:require [coal-mine.checks :refer [defcheck-124] :rename {defcheck-124 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-124))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

