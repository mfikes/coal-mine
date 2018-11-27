(ns coal-mine.problem-144
  (:require [coal-mine.checks :refer [defcheck-144] :rename {defcheck-144 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-144))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

