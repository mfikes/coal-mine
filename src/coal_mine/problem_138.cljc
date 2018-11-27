(ns coal-mine.problem-138
  (:require [coal-mine.checks :refer [defcheck-138] :rename {defcheck-138 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-138))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

