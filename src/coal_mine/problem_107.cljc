(ns coal-mine.problem-107
  (:require [coal-mine.checks :refer [defcheck-107] :rename {defcheck-107 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-107))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

