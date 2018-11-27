(ns coal-mine.problem-164
  (:require [coal-mine.checks :refer [defcheck-164] :rename {defcheck-164 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-164))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
