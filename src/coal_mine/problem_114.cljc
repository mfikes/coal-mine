(ns coal-mine.problem-114
  (:require [coal-mine.checks :refer [defcheck-114] :rename {defcheck-114 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-114))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

