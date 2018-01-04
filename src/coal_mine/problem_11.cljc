(ns coal-mine.problem-11
  (:require [coal-mine.checks :refer [defcheck-11] :rename {defcheck-11 defcheck}]
            [clojure.test]))

(defcheck solution-1e67cf87
  {:a 1, :b 2, :c 3})

(defcheck solution-1fe83153
  (hash-map :b 2))

(defcheck solution-2508b3f4
  {:b 2})

(defcheck solution-2ccd796c
  '[:b 2])

(defcheck solution-6d8ea364
  {:b, 2})

(defcheck solution-8318034e
  { :b 2 })

(defcheck solution-8832fce2
  {:b(+(*)(*))})

(defcheck solution-8ec72c87
  { :b 2})

(defcheck solution-e49b0c87
  [:b 2])

(defcheck solution-ed86de29
  (vector :b 2))
