(ns coal-mine.problem-7
  (:require [coal-mine.checks :refer [defcheck-7] :rename {defcheck-7 defcheck}]
            [clojure.test]))

(defcheck solution-1e613f91
  [1,2,3,4])

(defcheck solution-21db4a7
  [1 2 3 4])

(defcheck solution-22e280b3
  [(*)(+(*)(*))(+(*)(*)(*))(+(*)(*)(*)(*))])

(defcheck solution-29002ded
  [1 2 3, 4])

(defcheck solution-30ae2ecd
  '[1 2 3 4])

(defcheck solution-33b69fa6
  (vector 1 2 3 4) [1 2 3 4])

(defcheck solution-3a37a6d2
  '(1 2 3 4) '(1 2 3 4))

(defcheck solution-3b40d7f3
  [ 1 2 3 4 ])

(defcheck solution-3c02f9ff
  [1 2 3 4 ] [1 2 3 4])

(defcheck solution-45f50fab
  (conj [] 1 2 3 4))

(defcheck solution-6147712c
  [ 1 2 3 4] [1 2 3 4])

(defcheck solution-745bad71
  (vector 1 2 3 4))

(defcheck solution-756528a
  [1 2 3 4] [1 2 3 4])

(defcheck solution-773d0a39
  [1 2 3 4] [1 2 3 4])

(defcheck solution-7a16db77
  [ 1 2 3 4])

(defcheck solution-7fe22b3b
  (vec '(1 2 3 4)) (vector 1 2 3 4) [1 2 3 4])

(defcheck solution-a586280
  '(1 2 3 4))

(defcheck solution-d5ce3ba3
  [1 2 3 4] [1 2 3 4])

(defcheck solution-d95e0057
  [1 2 3 4 ])

(defcheck solution-db030f21
  [1 2 3 4] [1 2 3 4])

(defcheck solution-e84c3dd8
  (range 1 5))

(defcheck solution-f2c0136d
  [1,2,3,4] [1,2,3,4])

(defcheck solution-fadb40d4
  (vec (range 1 5)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-7))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
