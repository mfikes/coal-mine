(ns coal-mine.problem-102
  (:require [coal-mine.checks :refer [defcheck-102] :rename {defcheck-102 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-102))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

