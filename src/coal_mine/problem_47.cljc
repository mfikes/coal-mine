(ns coal-mine.problem-47
  (:require [coal-mine.checks :refer [defcheck-47] :rename {defcheck-47 defcheck}]
            [clojure.test]))

(defcheck solution-150c8184
  4)

(defcheck solution-9373c580
  (+ 2 2))

(defcheck solution-ab7935f9
  (reduce + (take 4 (repeat 1))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-47))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
