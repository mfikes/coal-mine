(ns coal-mine.problem-68
  (:require [coal-mine.checks :refer [defcheck-68] :rename {defcheck-68 defcheck}]
            [clojure.test]))

(defcheck solution-11546122
  (reverse (range 3 8)))

(defcheck solution-1568912c
  (range 7 2 -1))

(defcheck solution-2499010c
  '(7 6 5 4 3))

(defcheck solution-2e5fcdb
  '(7 6 5 4 3 ))

(defcheck solution-36aeb1d5
  [7  6 5 4 3])

(defcheck solution-3a8b8050
  '( 7 6 5 4 3))

(defcheck solution-3e5065b0
  (vector 7 6 5 4 3 ))

(defcheck solution-4d57a36a
  [7 6 5 4 3])

(defcheck solution-6c766085
  [ 7 6 5 4 3 ])

(defcheck solution-6e588e7
  [7, 6, 5, 4, 3])

(defcheck solution-7be3623
  [7 6 5 4 3])

(defcheck solution-7bf9036d
  '(7,6,5,4,3))

(defcheck solution-a2a64444
  `( 7 6 5 4 3 ))

(defcheck solution-a36d06bd
  '[7 6 5 4 3])

(defcheck solution-a7a1cd39
  [7 6 5 4 3 ])

(defcheck solution-acba1718
  (vec (map #(+ 2 %) (range 5 0 -1))))

(defcheck solution-b11171e1
  `(7 6 5 4 3))

(defcheck solution-cc4dfc63
  [7,6,5,4,3])

(defcheck solution-cc8dd484
  '(7, 6, 5, 4, 3))

(defcheck solution-ebcf90be
  (list 7 6 5 4 3))

(defcheck solution-ecf2874a
  (map #(+ 2 %) [5 4 3 2 1]))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-68))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
