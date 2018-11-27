(ns coal-mine.problem-128
  (:require [coal-mine.checks :refer [defcheck-128] :rename {defcheck-128 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-128))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

