(ns coal-mine.problem-108
  (:require [coal-mine.checks :refer [defcheck-108] :rename {defcheck-108 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-108))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

