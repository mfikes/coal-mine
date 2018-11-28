(ns coal-mine.problem-135
  (:require [coal-mine.checks :refer [defcheck-135] :rename {defcheck-135 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-135))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

