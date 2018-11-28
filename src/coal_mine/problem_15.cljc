(ns coal-mine.problem-15
  (:require [coal-mine.checks :refer [defcheck-15] :rename {defcheck-15 defcheck}]
            [clojure.test]))

(defcheck solution-1225cfb7
  (partial * 2))

(defcheck solution-2130ef61
  (fn [n] (* n 2)))

(defcheck solution-21b55e21
  #(* %1 2))

(defcheck solution-28bd1dc0
  (fn [x] (+ x x )))

(defcheck solution-29dac66a
  ;; #(* 2 %)
  (partial * 2))

(defcheck solution-3106e673
  (fn [x] (* 2 x)))

(defcheck solution-336a46a5
  (fn [n](* n 2)))

(defcheck solution-37d6745f
  #(* % 2))

(defcheck solution-3a3a7af2
  (fn double-func [x] (* 2 x)))

(defcheck solution-3eabf8a9
  (fn foo [x] (* 2 x)))

(defcheck solution-44479753
  (fn dblDwn?! [x] (* x 2)))

(defcheck solution-470f04d
  (fn [e] (* e 2)))

(defcheck solution-487438e0
  (fn myfunc [x] (* 2 x)))

(defcheck solution-4b7618d3
  (fn double-it [x] (* 2 x)))

(defcheck solution-4d37bdc6
  (fn dbl [x] (* 2 x)))

(defcheck solution-4f9d7e7e
  (fn [i] (* i 2)))

(defcheck solution-5691a3ed
  (fn [x] (bit-shift-left x 1)))

(defcheck solution-57feb5f0
  (fn do-double [x] (* x 2)))

(defcheck solution-635b21cc
  (fn [x](* x 2)))

(defcheck solution-668de036
  (fn [c] (* 2 c)))

(defcheck solution-6a7ce414
  (fn [x] (* x 2)))

(defcheck solution-6b03e317
  (fn double[x] (* x 2)))

(defcheck solution-6ec00ddb
  * 2)

(defcheck solution-733223f1
  #( * % 2))

(defcheck solution-7ebabf80
  #(+ % %))

(defcheck solution-81a07682
  (fn double [x] (* 2 x)))

(defcheck solution-86f8d145
  ;#(* % 2)
  (fn [x] (* x 2)))

(defcheck solution-8798cab4
  (fn [x]
    (* x 2)))

(defcheck solution-8ed12184
  ; (fn [x] (* x 2))
  #(* % 2))

(defcheck solution-9949f775
  #(* 2 %))

(defcheck solution-9d8b14d6
  (fn[x] (* 2 x)))

(defcheck solution-a517c89
  (fn doubling [x] (* x 2)))

(defcheck solution-aec346e9
  (fn [xb] (* 2 xb)))

(defcheck solution-b22fb2e6
  (fn [x] "Doubles the arg" (* 2 x)))

(defcheck solution-b327e018
  (fn number_double[n] (* 2 n)))

(defcheck solution-b5d34263
  (fn dbl [x] (* x 2)))

(defcheck solution-bbcc901
  (fn doubleNumber [x] (* 2 x)))

(defcheck solution-be2cc335
  (fn double-down [x]
    (* x 2)))

(defcheck solution-c22aeb58
  ;;#(* % 2)
  (fn [x] (* x 2)))

(defcheck solution-c7f6f536
  (fn [x] (second (iterate #(+ %1 %1) x))))

(defcheck solution-c8260ed2
  (fn [n] (+ n n)))

(defcheck solution-d3d77ab1
  #(* 2 %1))

(defcheck solution-d5d7c43f
  (fn [x] (+ x x)))

(defcheck solution-dced4825
  (fn square [x] (+ x x)))

(defcheck solution-e66799b0
  (fn [n] (* 2 n)))

(defcheck solution-e77b8c28
  #(bit-shift-left % 1))

(defcheck solution-e83c4de7
  #(reduce + (repeat 2 %)))

(defcheck solution-f2fea5c5
  (fn times-two [x] (* 2 x)))

(defcheck solution-f720750
  (fn double [x] (* x 2)))

(defcheck solution-f9749436
  (fn double[n] (* 2 n)))

(defcheck solution-fd7d5669
  (fn[x](* x 2)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-15))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
