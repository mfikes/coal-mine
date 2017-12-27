(ns coal-mine.problem-1
  (:require [coal-mine.checks :refer [defcheck-1] :rename {defcheck-1 defcheck}]
            [clojure.test]))

(defcheck solution-1bd83744
  (= (= true true) true))

(defcheck solution-20739944
  (> 5 4 3))

(defcheck solution-230ae421
  (not nil))

(defcheck solution-245bfcd0
  (odd? 11))

(defcheck solution-3674f68
  (= true true))

(defcheck solution-5bc38b99
  (= 1 1))

(defcheck solution-5dd0571e
  (= 0 0))

(defcheck solution-5e787b0e
  (= (= 3 3) true))

(defcheck solution-6549d4f6
  (> 5 1))

(defcheck solution-66d5da78
  (< 1 4))

(defcheck solution-6969c0a9
  (= (not false) true))

(defcheck solution-6ca79264
  (or false true))

(defcheck solution-72888974
  (= 1 1))

(defcheck solution-84645d67
  (= 2 2))

(defcheck solution-84aff8b2
  )

(defcheck solution-8af53dc9
  ((fn [x] (not x)) false))

(defcheck solution-8d8b4815
  (odd? 3))

(defcheck solution-9b9cc8e6
  (#(-> %) true))

(defcheck solution-a26fb9fa
  (not false))

(defcheck solution-b1ab3713
  (= (= 1 1) true))

(defcheck solution-b3178a22
  (= 1))

(defcheck solution-b577097
  (< 2 4))

(defcheck solution-bc67ebf3
  (not (= 1 2)))

(defcheck solution-c7a7c5d9
  (= =))

(defcheck solution-c8310554
  (= 4 4))

(defcheck solution-e4c59d1f
  )

(defcheck solution-ea8f5253
  (= 1 (+ 0 1)))

(defcheck solution-f0f3e35
  (> 1 0))

(defcheck solution-f5974a3b
  (zero? 0))

(defcheck solution-f6c98138
  (= 1 (/ 1)))
