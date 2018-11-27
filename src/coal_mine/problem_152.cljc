(ns coal-mine.problem-152
  (:require [coal-mine.checks :refer [defcheck-152] :rename {defcheck-152 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-152))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
