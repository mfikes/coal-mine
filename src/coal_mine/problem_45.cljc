(ns coal-mine.problem-45
  (:require [coal-mine.checks :refer [defcheck-45] :rename {defcheck-45 defcheck}]
            [clojure.test]))

(defcheck solution-110ae51
  '(1 4 7 10 13 ))

(defcheck solution-1214276c
  (map (comp inc
         (partial * 3))
    (range 0 5)))

(defcheck solution-3db2d7a5
  [1 4 7 10 13])

(defcheck solution-433cd68a
  [1, 4, 7, 10, 13])

(defcheck solution-4bd93ee3
  (reverse (reduce (fn [xs x] (conj xs (+ 3 (first xs)))) '(1) (take 4 (repeat 1)))))

(defcheck solution-7eed2059
  '(1 4 7 10 13))

(defcheck solution-88a1f6f6
  (->> (range)
    (map inc)
    (partition 1 3)
    (take 5)
    (flatten)))

(defcheck solution-8c3533f8
  [ 1 4 7 10 13 ])

(defcheck solution-8ce5dfd1
  (range 1 15 3))

(defcheck solution-975bb944
  (list 1 4 7 10 13))

(defcheck solution-9b05af07
  (map #(inc (* % 3)) (range 5)))

(defcheck solution-a2c4c1ba
  (take 5 (iterate #(+ 3 %) 1)))

(defcheck solution-c2ef3356
  (range 1 14 3))

(defcheck solution-ca98ec45
  (take 5 '(1 4 7 10 13)))

(defcheck solution-d89a7175
  '( 1 4 7 10 13))

(defcheck solution-dc48ee0
  '(1, 4, 7, 10, 13))

(defcheck solution-dcfed0e7
  (take 5 (range 1 14 3)))

(defcheck solution-e686176a
  (take 5 (range 1 20 3)))

(defcheck solution-edc8df56
  (range 1 16 3))

(defcheck solution-f990654a
  `(1 4 7 10 13))

(defcheck solution-fd3374ee
  (take 5 (range 1 15 3)))
