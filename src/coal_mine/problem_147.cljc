(ns coal-mine.problem-147
  (:require [coal-mine.checks :refer [defcheck-147] :rename {defcheck-147 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-147))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

