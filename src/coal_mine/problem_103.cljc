(ns coal-mine.problem-103
  (:require [coal-mine.checks :refer [defcheck-103] :rename {defcheck-103 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-103))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))


