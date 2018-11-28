(ns coal-mine.problem-156
  (:require [coal-mine.checks :refer [defcheck-156] :rename {defcheck-156 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-156))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
