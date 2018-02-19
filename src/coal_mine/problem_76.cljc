(ns coal-mine.problem-76
  (:require [coal-mine.checks :refer [defcheck-76] :rename {defcheck-76 defcheck}]
            [clojure.test]))

(defcheck solution-14cd72da
  (letfn
   [(foo [x y] #(bar (conj x y) y))
    (bar [x y] (if (> (last x) 10)
                 x
                 #(foo x (+ 2 y))))]
    ((fn t [f & args]
       (let [r (apply f args)]

         (if (fn? r)
           (t r)
           r))) foo [] 1)))

(defcheck solution-21b6b053
  [ 1 3 5 7 9 11])

(defcheck solution-23541894
  ; Here is what is happening:
  ; foo is called with [] 1, returns #(bar [1] 1)
  ; #(bar [1] 1) returns #(foo [1] 3)
  ; #(foo [1] 3) returns #(bar [1 3] 3)
  ; #(bar [1 3] 3) returns #(foo [1 3] 5)
  ; and so on and so on until (> (last [x] 10))
  [1 3 5 7 9 11])

(defcheck solution-2bbc343f
  [1 3 5 7 9 11])

(defcheck solution-39d1470e
  #_(fn [g & vs]
      (let [r (apply g vs)]
        (if (fn? r)
          (loop [g' r]
            (let [h (g')]
              (if (fn? h)
                (recur h)
                h)))
          r)))

  [1 3 5 7 9 11])

(defcheck solution-434935b3
  (filter odd? (range 1 12)))

(defcheck solution-472a020b
  '(1 3 5 7 9 11))

(defcheck solution-4e68786c
  (letfn [(foo [x y] #(bar (conj x y) y)) (bar [x y] (if (> (last x) 10) x #(foo x (+ 2 y))))
          (tramp [f & args] (if ((comp not fn?) f) f
                                                   (if ((comp not empty?) args)
                                                     (tramp (apply f args))
                                                     (tramp (f)))))]
    (tramp foo [] 1)))

(defcheck solution-5be01cf1
  (filter odd? (range 12)))

(defcheck solution-5ea4fe7f
  [1  3  5 7 9 11])

(defcheck solution-6b7076ba
  (remove even? (range 12)))

(defcheck solution-78d184b
  (letfn
   [(foo [x y] #(bar (conj x y) y))
    (bar [x y] (if (> (last x) 10)
                 x
                 #(foo x (+ 2 y))))]
    (trampoline foo [] 1)))

(defcheck solution-7e4059f9
  (range 1 12 2))

(defcheck solution-80aa45af
  (letfn
   [(foo [x y] #(bar (conj x y) y))
    (bar [x y] (if (> (last x) 10)
                 x
                 #(foo x (+ 2 y))))]
    (trampoline foo [] 1)))

(defcheck solution-8468c5b2
  (range 1 13 2))

(defcheck solution-849d0fa6
  ((fn [x y]
     (if (> (last (conj x y)) 10)
       (conj x y)
       (recur (conj x y) (+ 2 y)))) [] 1))

(defcheck solution-93364a38
  (letfn
   [(foo [x y] #(bar (conj x y) y))
    (bar [x y] (if (> (last x) 10)
                 x
                 #(foo x (+ 2 y))))]
    ((fn ft
       ([f]
        (let [ret (f)]
          (if (fn? ret)
            (recur ret)
            ret)))
       ([f & args]
        (ft #(apply f args)))) foo [] 1)))

(defcheck solution-b29b4096
  [1,3,5,7,9,11])

(defcheck solution-c087e951
  (letfn
   [(foo [x y] #(bar (conj x y) y))
    (bar [x y] (if (> (last x) 10)
                 x
                 #(foo x (+ 2 y))))]
    ((fn [ func & args ] (loop [res (apply func args)]
                           (if (fn? res)
                             (recur (res))
                             res)
                           )) foo [] 1)))

(defcheck solution-c7c0000
  (vec (take-while
         #(> 12 %)
         (iterate #(+ 2 %) 1))))

(defcheck solution-c8d91365
  [1 3 5 7 9 11 ])

(defcheck solution-e12f130a
  (vec (range 1 12 2)))

(defcheck solution-e7d54f13
  '(1 3 5	7 9 11))

(defcheck solution-f0ffc5ab
  (take-nth 2 (range 1 12)))