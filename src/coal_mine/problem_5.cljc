(ns coal-mine.problem-5
  (:require [coal-mine.checks :refer [defcheck-5] :rename {defcheck-5 defcheck}]
            [clojure.test]))

(defcheck solution-21db4a7
  [1 2 3 4])

(defcheck solution-22e280b3
  [(*)(+(*)(*))(+(*)(*)(*))(+(*)(*)(*)(*))])

(defcheck solution-34273c58
  '(1  2 3 4) '(1 2 3 4))

(defcheck solution-3a37a6d2
  '(1 2 3 4) '(1 2 3 4))

(defcheck solution-687d711e
  '(1 2 3 4) '(1 2 3 4))

(defcheck solution-6d5a099d
  [1
   2
   3
   4])

(defcheck solution-7120e164
  '(1 2 3 4) '(1 2 3 4))

(defcheck solution-748bfa7c
  `(1 2 3 4))

(defcheck solution-90f9982f
  '(1 2 3 4) (list 1 2 3 4))

(defcheck solution-95860522
  (conj '(2 3 4) 1))

(defcheck solution-a586280
  '(1 2 3 4))

(defcheck solution-a6f935c7
  (conj '() 4 3 2 1))

(defcheck solution-a8551819
  ( list 1 2 3 4))

(defcheck solution-afcce2c0
  ( list 1 2 3 4 ))

(defcheck solution-b4f2a28a
  '(1 2 3 4 ))

(defcheck solution-becbd27
  (list 1 2 3 4) (list 1 2 3 4))

(defcheck solution-d5ce3ba3
  [1 2 3 4] [1 2 3 4])

(defcheck solution-e27d5159
  '( 1 2 3 4))

(defcheck solution-e84c3dd8
  (range 1 5))

(defcheck solution-ef246ae0
  (list 1 2 3 4))

(defcheck solution-fca63bf2
  '( 1 2 3 4 ))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-5))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
