(ns coal-mine.problem-14
  (:require [coal-mine.checks :refer [defcheck-14] :rename {defcheck-14 defcheck}]
            [clojure.test]))

(defcheck solution-166c2c8b
  8 8 8 8)

(defcheck solution-34c147e7
  ((fn add-five [x] (+ x 5)) 3))

(defcheck solution-4f825ac4
  8)

(defcheck solution-5d05a2fe
  (#(+ % 5) 3))

(defcheck solution-741bee25
  (#(+ 4 %) 4))

(defcheck solution-7f666530
  8 8 8 8)

(defcheck solution-88654b5e
  ((partial + 5) 3))

(defcheck solution-b9042829
  (* 2 4))

(defcheck solution-be923dee
  (+ 384 -376))

(defcheck solution-c59bc998
  8 8 8)

(defcheck solution-cbebfa90
  8 8 8 8)

(defcheck solution-d14e6d19
  8 ((partial + 3) 5) (let [[_ & tail-args] (range 9)]
                        (-> tail-args
                          last)))

(defcheck solution-d2c2da9c
  (#(+ % 5) 3) (#(+ % 5) 3) (#(+ % 5) 3) (#(+ % 5) 3))