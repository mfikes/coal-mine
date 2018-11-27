(ns coal-mine.problem-141
  (:require [coal-mine.checks :refer [defcheck-141] :rename {defcheck-141 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-141))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

