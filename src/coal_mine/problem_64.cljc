(ns coal-mine.problem-64
  (:require [coal-mine.checks :refer [defcheck-64] :rename {defcheck-64 defcheck}]
            [clojure.test]))

(defcheck solution-1aad1dbd
  +)

(defcheck solution-24c671ae
  (fn [& more] (if (= 0 (count more)) 0 (+ (first more) (last more)) )))

(defcheck solution-3f6dee6
  (fn
    ([] 0)
    ([x] x)
    ([x y] (+ x y))))

(defcheck solution-3f8726a7
  #'+)

(defcheck solution-464333c1
  (partial + 0))

(defcheck solution-50309a3e
  (fn ([] 0) ([i & o](apply + i o))))

(defcheck solution-548b69bb
  (fn
    ([] 0)
    ([x y] (+ x y))))

(defcheck solution-5f49a914
  (fn rdc ([a b] (+ a b)) ([] 0) ))

(defcheck solution-601176ea
  (fn ([] 0) ([x]x) ([a b] (+ a b))))

(defcheck solution-66b6ea3a
  (fn ([x y] (+ x y)) ([x] x) ([] 0)))

(defcheck solution-7b14ac00
  (fn ([] 0) ([a b] (+ a b))))

(defcheck solution-87e9988c
  (fn [& args] (apply + args)))

(defcheck solution-a2c9503f
  (fn [& x]
    (if (nil? x)
      0
      (+ (first x)
         (first (rest x))))))

(defcheck solution-a833ffa3
  (fn [& [a b]] (+ (if a a 0) (if b b 0))))

(defcheck solution-b5f0e363
  (fn ([] 0) ([x y] (+ x y))))

(defcheck solution-bafb93ba
  (fn
    ([] 0)
    ([x y]
     (+ x y))))

(defcheck solution-bc779bdb
  (fn s ([] (s 0 0)) ([x y] (+ x y))))

(defcheck solution-be5bdd06
  (fn ([a b] (+ a b))
    ([] 0)
    ))

(defcheck solution-c743a045
  (fn [& x] (if (= 0 (count x)) 0 (+ (first x) (second x)))))

(defcheck solution-d0e1fe3e
  (fn add
    ([] 0)
    ([a] a)
    ([a b] (+ a b))
    ([a b & more] (+ (+ a b) (add more)))
    ))

(defcheck solution-e4b298f7
  (fn ([] 0) ([acc el] (+ acc el))))

(defcheck solution-f00cb94c
  (fn ([x y] (+ x y))
    ([] 0)
    ))

(defcheck solution-fce394b2
  (fn ([acc x] (+ acc x))
    ([] 0)))