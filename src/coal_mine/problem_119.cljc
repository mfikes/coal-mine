(ns coal-mine.problem-119
  (:require [coal-mine.checks :refer [defcheck-119] :rename {defcheck-119 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-119))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

