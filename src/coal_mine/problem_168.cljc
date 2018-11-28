(ns coal-mine.problem-168
  (:require [coal-mine.checks :refer [defcheck-168] :rename {defcheck-168 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-168))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
