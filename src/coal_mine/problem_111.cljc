(ns coal-mine.problem-111
  (:require [coal-mine.checks :refer [defcheck-111] :rename {defcheck-111 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-111))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

