(ns coal-mine.problem-121
  (:require [coal-mine.checks :refer [defcheck-121] :rename {defcheck-121 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-121))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

