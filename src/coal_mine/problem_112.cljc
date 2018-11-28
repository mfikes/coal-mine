(ns coal-mine.problem-112
  (:require [coal-mine.checks :refer [defcheck-112] :rename {defcheck-112 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-112))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

