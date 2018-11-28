(ns coal-mine.problem-153
  (:require [coal-mine.checks :refer [defcheck-153] :rename {defcheck-153 defcheck}]
            [clojure.test]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-153))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
