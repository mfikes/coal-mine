(ns coal-mine.problem-51
  (:require [coal-mine.checks :refer [defcheck-51] :rename {defcheck-51 defcheck}]
            [clojure.test]))

(defcheck solution-12f0614e
  [1 2 3 4  5])

(defcheck solution-1d566772
  [ 1 2 3 4 5])

(defcheck solution-310469e3
  '( 1 2 3 4 5 ))

(defcheck solution-41b1a75
  ;The :as keyword can be used to retain access to
  ;the entire collection that is being destructured.
  [1 2 3 4 5])

(defcheck solution-576d2cb9
  (take 5 (iterate inc 1)))

(defcheck solution-5b27165e
  (range 1 6))

(defcheck solution-6bad7c34
  (list 1 2 3 4 5))

(defcheck solution-78017b6
  '[1 2 3 4 5])

(defcheck solution-78653f05
  '(1 2 3 4 5))

(defcheck solution-7c6cd60f
  [1 2 3 4 5])

(defcheck solution-871e1dc5
  (range 1 (inc 5)))

(defcheck solution-87ce8e9b
  (take 5 (drop 1 (range))))

(defcheck solution-a7496ea4
  (apply vector (range 1 6)))

(defcheck solution-c26faa8a
  [1 2 3 4 5 ])

(defcheck solution-cdb888be
  [ 1 2 3 4 5 ])

(defcheck solution-d7d0d3f2
  (drop 1 (range 6)))

(defcheck solution-ea904f03
  (map inc (range 5)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-51))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
