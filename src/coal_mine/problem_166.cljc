(ns coal-mine.problem-166
  (:require [coal-mine.checks :refer [defcheck-166] :rename {defcheck-166 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-166))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
