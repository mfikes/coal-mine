(ns coal-mine.problem-134
  (:require [coal-mine.checks :refer [defcheck-134] :rename {defcheck-134 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-134))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

