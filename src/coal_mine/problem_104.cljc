(ns coal-mine.problem-104
  (:require [coal-mine.checks :refer [defcheck-104] :rename {defcheck-104 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-104))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

