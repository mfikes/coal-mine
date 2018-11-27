(ns coal-mine.problem-113
  (:require [coal-mine.checks :refer [defcheck-113] :rename {defcheck-113 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-113))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

