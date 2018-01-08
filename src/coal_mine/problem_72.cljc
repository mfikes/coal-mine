(ns coal-mine.problem-72
  (:require [coal-mine.checks :refer [defcheck-72] :rename {defcheck-72 defcheck}]
            [clojure.test]))

(defcheck solution-2a0f33b1
  (fn sum [s]
    (if (empty? s)
      0
      (+ (first s)
         (sum (rest s))))))

(defcheck solution-2bf28832
  (fn [x]
    (reduce + x)
    ))

(defcheck solution-2c973de1
  reduce (fn [a b] (+ a b)))

(defcheck solution-35a906e8
  #(reduce + %))

(defcheck solution-37c6527c
  #(reduce + 0 %))

(defcheck solution-41aca247
  (fn [l] (reduce + l)))

(defcheck solution-4560dcc7
  reduce #(+ %1 %2))

(defcheck solution-469c28bf
  (fn [_] 11))

(defcheck solution-54ca698
  (fn [arg] (reduce + arg)))

(defcheck solution-6f93e9ce
  apply +)

(defcheck solution-70760d2d
  (fn [x] (reduce + x)))

(defcheck solution-7738ec57
  (partial reduce + 0))

(defcheck solution-7ee74501
  (fn [x] (reduce #(+ %1 %2) x)))

(defcheck solution-88acf04c
  #(apply + %1))

(defcheck solution-93620ea4
  (partial apply +))

(defcheck solution-976e0493
  (fn [x] (apply + x)))

(defcheck solution-99277d42
  #(reduce (fn [a b] (+ a b)) %))

(defcheck solution-9a6f6622
  (partial reduce +))

(defcheck solution-a7d6108a
  (partial apply + ))

(defcheck solution-a9e5d159
  apply +)

(defcheck solution-aa0f317a
  (fn [els] (apply + els)))

(defcheck solution-bcca59b1
  (fn [s] (apply + s)))

(defcheck solution-c4a45fc9
  (fn [v] (reduce + v)))

(defcheck solution-c84609dd
  (constantly 11))

(defcheck solution-c880e408
  apply +)

(defcheck solution-c8f52b59
  #(reduce + (vec %)))

(defcheck solution-c9d74d95
  (fn [x] 11))

(defcheck solution-cb632fd5
  #(apply + %))

(defcheck solution-ce410674
  (fn [coll]
    (reduce + coll)))

(defcheck solution-d4554d07
  reduce +)

(defcheck solution-ed974043
  (fn [liste] (apply + liste)))

(defcheck solution-f28c6eed
  reduce +)

(defcheck solution-f301184
  (fn [ lst ] (apply + lst)))