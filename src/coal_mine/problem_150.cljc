(ns coal-mine.problem-150
  (:require [coal-mine.checks :refer [defcheck-150] :rename {defcheck-150 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-150))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

