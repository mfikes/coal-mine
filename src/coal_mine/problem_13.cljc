(ns coal-mine.problem-13
  (:require [coal-mine.checks :refer [defcheck-13] :rename {defcheck-13 defcheck}]
            [clojure.test]))

(defcheck solution-54963f88
  (range 20 50 10))

(defcheck solution-63ce813a
  (map #(* 10 %) [2 3 4]))

(defcheck solution-6cc85e5f
  (rest [10 20 30 40]))

(defcheck solution-74f318c6
  [20, 30, 40])

(defcheck solution-7fe4e3fe
  (vector 20 30 40))

(defcheck solution-8d257003
  [ 20 30 40 ])

(defcheck solution-9237bb8f
  '(20 30 40))

(defcheck solution-9e7dd406
  (list 20 30 40))

(defcheck solution-a8d786cd
  (conj [] 20 30 40))

(defcheck solution-bb70ed82
  (seq [20 30 40]))

(defcheck solution-bbb8462f
  ;; simplest one :)
  (vector 20 30 40) ;; after
  ((comp rest vec list) 10 20 30 40) ;; after...read like a pros
  (-> (range 10 49 10)
    (into [])
    rest))

(defcheck solution-c6202b64
  [20 30 40 ])

(defcheck solution-caf3988f
  '[20 30 40])

(defcheck solution-d176d4ae
  (range 20 41 10))

(defcheck solution-dac00e5e
  (vec '(20 30 40)))

(defcheck solution-e0e75fdb
  [20 30 40])

(defcheck solution-f45d06e8
  [ 20 30 40])
