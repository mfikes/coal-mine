(ns coal-mine.problem-120
  (:require [coal-mine.checks :refer [defcheck-120] :rename {defcheck-120 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-120))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

