(ns coal-mine.problem-52
  (:require [coal-mine.checks :refer [defcheck-52] :rename {defcheck-52 defcheck}]
            [clojure.test]))

(defcheck solution-1e4c085c
  [ c e ])

(defcheck solution-1ff9b1b5
  (conj [] c e))

(defcheck solution-247d43c3
  [2 4])

(defcheck solution-2f403ec8
  [2 4])

(defcheck solution-39003ff
  [c e])

(defcheck solution-3b0388ab
  (list c e))

(defcheck solution-6f5080c
  '[2 4])

(defcheck solution-8130b71b
  `(~c ~e))

(defcheck solution-9f0a7d76
  (vec (list c e)))

(defcheck solution-a28db9f9
  ;[c e]
  [2 4])

(defcheck solution-a8c0e763
  [c e ])

(defcheck solution-bd580c30
  (vector c e))

(defcheck solution-cf5a8a87
  [ c e])