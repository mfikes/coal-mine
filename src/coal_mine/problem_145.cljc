(ns coal-mine.problem-145
  (:require [coal-mine.checks :refer [defcheck-145] :rename {defcheck-145 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-145))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

