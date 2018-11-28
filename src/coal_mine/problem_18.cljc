(ns coal-mine.problem-18
  (:require [coal-mine.checks :refer [defcheck-18] :rename {defcheck-18 defcheck}]
            [clojure.test]))

(defcheck solution-270e3454
  '(6 7))

(defcheck solution-27c6be98
  '(6 7 ))

(defcheck solution-2e382787
  '(6, 7))

(defcheck solution-3a7e6425
  '( 6 7))

(defcheck solution-4e04bffb
  (filter #(or (= % 6) (= % 7)) '(1 2 3 4 5 6 7 8 9 10)))

(defcheck solution-6009dffa
  '(6,7))

(defcheck solution-7e01a5f3
  (filter #(> % 5) '(6 7)))

(defcheck solution-7f0a0141
  '( 6 7 ))

(defcheck solution-7f90cad3
  (range 6 8))

(defcheck solution-811e498e
  (list 6 7))

(defcheck solution-87ee5b99
  [ 6 7])

(defcheck solution-8a813724
  [6,7])

(defcheck solution-8de7a84b
  [6 7])

(defcheck solution-91a5972
  '(6 7) (let [higher-than-five (comp (partial filter #(> % 5)) list)]
           (higher-than-five 3 4 5 6 7)))

(defcheck solution-e7659836
  (filter #(> % 5) '(3 4 5 6 7)))

(defcheck solution-ef6e1847
  (filter #(< % 10) '(6 7 11 12 13)))

(defcheck solution-f233c6ee
  `(6 7))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-18))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
