(ns coal-mine.problem-132
  (:require [coal-mine.checks :refer [defcheck-132] :rename {defcheck-132 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-132))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

