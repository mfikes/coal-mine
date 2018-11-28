(ns coal-mine.problem-57
  (:require [coal-mine.checks :refer [defcheck-57] :rename {defcheck-57 defcheck}]
            [clojure.test]))

(defcheck solution-2d9c5477
  (->> (range 5)
    (map (partial + 1))
    (reverse)))

(defcheck solution-2fdd2c9d
  '(5 4 3 2 1))

(defcheck solution-4073cfe9
  `(5 4 3 2 1))

(defcheck solution-449b4110
  '( 5 4 3 2 1))

(defcheck solution-49824478
  [5 4 3 2 1 ])

(defcheck solution-5067f089
  '(5 4 3 2 1))

(defcheck solution-588fd685
  (range 5 0 -1))

(defcheck solution-5a0aa946
  ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

(defcheck solution-5b7646ca
  (reverse '(1 2 3 4 5)))

(defcheck solution-71ba15f6
  [5 4 3 2 1])

(defcheck solution-90fa50e6
  (reverse [1 2 3 4 5]))

(defcheck solution-9e532994
  (reverse (range 1 6)))

(defcheck solution-a7b29d65
  [5,4,3,2,1])

(defcheck solution-aea45cd7
  ;this is (conj (conj (conj (conj (conj nil 1) 2) 3) 4) 5)
  '(5 4 3 2 1))

(defcheck solution-c5f6fefd
  '(5,4,3,2,1))

(defcheck solution-ca1cdeef
  [5, 4, 3, 2, 1])

(defcheck solution-de1e19c4
  '(5 4 3 2 1 ))

(defcheck solution-e468b788
  '(5 4  3 2 1))

(defcheck solution-ec0b3dbc
  (list 5 4 3 2 1))

(defcheck solution-efcea609
  '( 5 4 3 2 1 ))

(defcheck solution-ff58db6f
  [ 5 4 3 2 1])

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-57))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
