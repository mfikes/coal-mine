(ns coal-mine.problem-48
  (:require [coal-mine.checks :refer [defcheck-48] :rename {defcheck-48 defcheck}]
            [clojure.test]))

(defcheck solution-1fe73f9d
  (+ 1 1 1 1 1 1))

(defcheck solution-3f9e2bb5
  (* 2 3))

(defcheck solution-5626dd93
  6)

(defcheck solution-5982f4d1
  6 6)

(defcheck solution-8d1c4293
  6 6)

(defcheck solution-aff86597
  (some #{2 7 6} [5 6 7 8]))

(defcheck solution-b7b5ae43
  (some #(when (even? %) %) [5 6 7 8]))