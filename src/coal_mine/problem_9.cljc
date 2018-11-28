(ns coal-mine.problem-9
  (:require [coal-mine.checks :refer [defcheck-9] :rename {defcheck-9 defcheck}]
            [clojure.test]))

(defcheck solution-1a4c69a4
  2 1 3 4)

(defcheck solution-20617b01
  1 2)

(defcheck solution-2253a79b
  2)

(defcheck solution-3a09eef3
  (+ 1 1))

(defcheck solution-41c9c8aa
  2 2 4 3)

(defcheck solution-509efb92
  (+(*)(*)))

(defcheck solution-721a311d
  (- 10 8))

(defcheck solution-7eb8278
  2 3)

(defcheck solution-aa8d0211
  1 2 3)

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-9))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
