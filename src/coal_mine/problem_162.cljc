(ns coal-mine.problem-162
  (:require [coal-mine.checks :refer [defcheck-162] :rename {defcheck-162 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-162))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
