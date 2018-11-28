(ns coal-mine.problem-161
  (:require [coal-mine.checks :refer [defcheck-161] :rename {defcheck-161 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-161))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
