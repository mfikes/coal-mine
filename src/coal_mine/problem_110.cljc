(ns coal-mine.problem-110
  (:require [coal-mine.checks :refer [defcheck-110] :rename {defcheck-110 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-110))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

