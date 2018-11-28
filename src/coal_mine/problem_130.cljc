(ns coal-mine.problem-130
  (:require [coal-mine.checks :refer [defcheck-130] :rename {defcheck-130 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-130))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

