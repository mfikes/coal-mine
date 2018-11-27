(ns coal-mine.problem-127
  (:require [coal-mine.checks :refer [defcheck-127] :rename {defcheck-127 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-127))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

