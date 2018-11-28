(ns coal-mine.problem-122
  (:require [coal-mine.checks :refer [defcheck-122] :rename {defcheck-122 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-122))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

