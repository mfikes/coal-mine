(ns coal-mine.problem-143
  (:require [coal-mine.checks :refer [defcheck-143] :rename {defcheck-143 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-143))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

