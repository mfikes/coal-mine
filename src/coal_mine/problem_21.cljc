(ns coal-mine.problem-21
  (:require [coal-mine.checks :refer [defcheck-21] :rename {defcheck-21 defcheck}]
            [clojure.test]))

(defcheck solution-1009a166
  (fn f [a n]
    (cond (= n 0) (first a)
          :else (f (rest a) (- n 1)))))

(defcheck solution-10617ba4
  #(loop [seq %1 n %2]
     (if (zero? n)
       (first seq)
       (recur (rest seq) (dec n)))))

(defcheck solution-10b29a16
  (fn [lst n]
    (if (= n 0)
      (first lst)
      (recur (rest lst) (dec n)))))

(defcheck solution-10c7746a
  (comp last #(take (inc %2) %)))

(defcheck solution-11001e23
  (fn cust-nth [coll n]
    (loop [coll coll n n]
      (if (= n 0)
        (first coll)
        (recur (rest coll) (dec n))))))

(defcheck solution-11381a15
  #(second (take-nth  %2 %)))

(defcheck solution-118a4c6f
  #((vec %) %2))

(defcheck solution-11ae40c7
  (fn [l, i]
    (if (= i 0) (first l) (recur (rest l) (dec i)))
    ))

(defcheck solution-1209ee6d
  (fn [seq n] (last (take (+ n 1) seq))))

(defcheck solution-12ba4bbe
  (fn [s k] (loop [r s n k] (if (= n 0) (first r) (recur (rest r) (- n 1))))))

(defcheck solution-12c35a6c
  (fn [l i] (last (take (inc i) l))))

(defcheck solution-136aa6b0
  #(letfn [(f [x n] (if (< n 1) (first x) (f (rest x) (dec n))))]
     (f %1 %2)))

(defcheck solution-13cbeed2
  (fn [coll n]
    (if (> n 0)
      (recur (next coll) (- n 1))
      (first coll))))

(defcheck solution-13ee6b8
  (fn [coll n]
    (loop [x coll y 0]
      (if (= y n) (first x)
                  (recur (rest x) (inc y))))))

(defcheck solution-1451df8c
  (fn [l n] (aget (to-array l) n)))

(defcheck solution-14ad9e8f
  (fn newNth [x n] (if (= n 0) (first x) (newNth (rest x) (- n 1)))))

(defcheck solution-14baf175
  (fn [col, n] (if (= n 0)
                 (first col)
                 (recur (next col) (- n 1))
                 )
    ))

(defcheck solution-14ebbc7e
  (fn [c n] (first (drop n c))))

(defcheck solution-15e20c88
  #(-> (drop %2 %1) first))

(defcheck solution-16598fc0
  (fn [xs k]
    (if (zero? k)
      (first xs)
      (recur (rest xs) (dec k)))))

(defcheck solution-16bb8eff
  (fn [s n]
    (if (= n 0)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-172d6671
  #(loop [l %1 n %2]
     (if (= n 0)
       (first l)
       (recur (rest l) (dec n)))))

(defcheck solution-17336a93
  (fn [coll index]
    (loop [coll coll, i 0]
      (if (= i index)
        (first coll)
        (recur (next coll) (inc i))))))

(defcheck solution-17637a71
  (fn [v n]
    (loop [v v
           i 0]
      (let [head (first v)
            tail (rest v)]
        (if (= i n)
          head
          (recur tail (inc i)))))))

(defcheck solution-1779ea1b
  #(last (first (partition (inc %2) %))))

(defcheck solution-17c484
  (fn [s n]
    (if (<= n 0)
      (first s)
      (recur (rest s) (dec n))
      )
    ))

(defcheck solution-17fc798e
  (fn [xs x]
    (second (first
              (filter
                (fn [y] (= (first y) x)) (map vector (range (count xs)) xs))))))

(defcheck solution-18202669
  (fn
    [c i]
    ((vec c) i)))

(defcheck solution-18a9b7f9
  (fn nth-element [coll n]
    (first (nthrest coll n))))

(defcheck solution-193d6a73
  (fn [xs n]
    (if (zero? n)
      (first xs)
      (recur (rest xs) (- n 1)))))

(defcheck solution-194792c1
  (fn mynth [seq index]
    (if (= index 0)
      (first seq)
      (recur (rest seq) (dec index)))
    ))

(defcheck solution-199ed708
  #(if (= 0 %2) (first %) (recur (rest %)(dec %2))))

(defcheck solution-1ab4eee8
  #(loop [s %1 i %2] (if (<= i 0) (first s) (recur (rest s) (dec i)))))

(defcheck solution-1b22f138
  (fn nth_elem [sq n] (if (= n 0) (first sq) (nth_elem (rest sq) (- n 1)))))

(defcheck solution-1b673c82
  #(first(drop %2 %1)))

(defcheck solution-1c3234cf
  (fn ix [xs n] (if (= n 0) (first xs) (recur (rest xs) (- n 1)))))

(defcheck solution-1c64167
  (fn f ([c n]
         (cond
           (= n 0) (first c)
           :else (f (next c) (- n 1))
           )
         )
    ))

(defcheck solution-1c75482f
  (fn get-nth [x i]
    ((vec x) i)))

(defcheck solution-1c8a182d
  (comp last #(take (inc %2) %1)))

(defcheck solution-1d2224b5
  (fn rec-nth [sq n]
    (if (= n 0) (first sq)
                (rec-nth (rest sq) (- n 1)))))

(defcheck solution-1d298618
  (fn recnth [xsq n] (if (= n 0) (first xsq) (recnth (rest xsq) (dec n)))))

(defcheck solution-1d434adc
  (fn [input index]
    (loop [i 0 elements input]
      (if (= i index)
        (first elements)
        (recur (inc i) (rest elements)))
      )
    ))

(defcheck solution-1d49606d
  (fn get-nth [lst n] (if (== n 0) (first lst) (get-nth (rest lst) (- n 1)))))

(defcheck solution-1d76dc30
  (fn [xs n] (first (reverse (take (+ n 1) xs)))))

(defcheck solution-1dd3b076
  (fn f [lst i]
    (if (= i 0)
      (first lst)
      (f (rest lst) (dec i)))))

(defcheck solution-1dfd4ef4
  ;; (fn [s,n] (reduce #(if (= %1 n) (reduced %2) (inc %1)) 0 s))

  (fn [s,n] (first ((apply comp (take n (repeat rest))) s))))

(defcheck solution-1e252e02
  (fn myf [lista n]
    (if (= 0 n) (first lista) (myf (rest lista) (- n 1))
                )))

(defcheck solution-1e7cf270
  (fn [s0 idx0]
    (loop [s s0
           idx idx0]
      (if (= 0 idx)
        (first s)
        (recur (next s) (dec idx))))))

(defcheck solution-1f0e3b6e
  (fn [x y]
    (loop [s x i y]
      (if (= i 0)
        (first s)
        (recur (next s) (dec i))))))

(defcheck solution-1f124ee4
  (fn [sequ number]
    (loop [current-seq sequ
           county 0]
      (if (= county number)
        (first current-seq)
        (recur (rest current-seq) (inc county))))))

(defcheck solution-1f87b183
  (fn [lst idx]
    (loop [l lst i idx]
      (if (= i 0)
        (first l)
        (recur (rest l) (dec i))))))

(defcheck solution-1feca2ce
  (fn [l, n]
    (if (= n 0) (first l) (recur (rest l) (- n 1)))
    ))

(defcheck solution-2011fe5
  (fn nth-element [s n]
    (if (> n 0)
      (nth-element (rest s) (- n 1))
      (first s))))

(defcheck solution-202530a4
  #(if (= %2 0) (first %) (recur (rest %) (dec %2))))

(defcheck solution-203178a1
  (fn [data n] (last (take (inc n) data))))

(defcheck solution-2080235c
  (fn [l num](loop [list l n num] (if(= n 0) (first list) (recur (rest list) (dec n))))))

(defcheck solution-20c6f4a0
  #(loop [coll %1
          idx 0]
     (if (= idx %2)
       (first coll)
       (recur (rest coll) (inc idx)))))

(defcheck solution-2109c7b8
  (fn [sq, n]
    (if (zero? n) (first sq)
                  (recur (rest sq)
                    (dec n)))))

(defcheck solution-210d4a1
  (fn [s n]
    (if (= n 0)
      (first s)
      (recur (rest s) (- n 1)))))

(defcheck solution-219b9f8e
  (fn [s n]
    (if (zero? n)
      (first s)
      (recur (next s) (dec n)))))

(defcheck solution-219e91ba
  (fn my_nth [l n] (if (= n 0) (first l) (my_nth (next l) (dec n)))))

(defcheck solution-23a04e74
  #(last (take (+ 1 %2) %1)))

(defcheck solution-2515dd2d
  (fn [l n]
    (loop [l l n n]
      (if (zero? n)
        (first l)
        (recur (rest l) (dec n))))))

(defcheck solution-253dd797
  (fn ! [l n] (if (= n 0) (first l) (! (rest l) (dec n)))))

(defcheck solution-26c41fe6
  #(loop [ls %1 n %2] (if (= n 0) (first ls) (recur (rest ls) (- n 1)))))

(defcheck solution-26d9ed5
  (fn [s n]
    (->> s
      (drop n)
      first)))

(defcheck solution-27e3e7d8
  (fn [seq n]
    (loop [seq seq n n]
      (if (zero? n)
        (first seq)
        (recur (rest seq) (dec n))))))

(defcheck solution-28230f77
  (fn
    [li n]
    (if (= n 0)
      (first li)
      (recur (rest li) (dec n)))))

(defcheck solution-29093176
  (fn [lst n] (first (drop n lst))))

(defcheck solution-2924fab7
  #((vec %)%2))

(defcheck solution-29a8e1fa
  (fn [xs cnt] (->> (map vector (range) xs) (filter #(= cnt (first %))) first second)))

(defcheck solution-2a0028bb
  (fn [col n]
    (if (zero? n)
      (first col)
      (recur (rest col) (- n 1)))))

(defcheck solution-2a2f0a1d
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (next coll) (- n 1))
      )
    ))

(defcheck solution-2a443cca
  (fn [lst n]
    (loop [lst lst
           n n]
      (if (= n 0)
        (first lst)
        (recur (rest lst) (- n 1))))))

(defcheck solution-2b0c8ff4
  (fn [coll n] (get (vec coll) n )))

(defcheck solution-2b3fef6e
  (fn [coll cnt] (if (= 0 cnt)
                   (first coll)
                   (recur (rest coll) (dec cnt)))))

(defcheck solution-2b54064a
  (fn [l n]
    (loop [l- l n- n]
      (if (= n- 0)
        (first l-)
        (recur (rest l-) (- n- 1))))))

(defcheck solution-2b87058e
  (fn nthh [l count]
    (if (= count 0)
      (first l)
      (nthh (next l) (dec count)))))

(defcheck solution-2bdcac01
  (fn [list index] (first (reverse (take (+ index 1) list)))))

(defcheck solution-2be95366
  #(cond
     (= %2 0) (first %1)
     (or (< %2 0) (empty? (rest %1))) nil
     :else (recur (rest %1) (dec %2))))

(defcheck solution-2c5c466b
  (fn [seq pos]
    (loop [cnt 0 remain seq]
      (if (= cnt pos)
        (first remain)
        (recur (inc cnt) (rest remain))))))

(defcheck solution-2dcc64df
  (fn my-nth [xs n] (if (> n 0) (my-nth (rest xs) (dec n)) (first xs))))

(defcheck solution-2e085f55
  (fn [s n] (last (take (inc n) s))))

(defcheck solution-2e3ba4b4
  (fn [coll idx]
    (if (= idx 0)
      (first coll)
      (recur (rest coll) (- idx 1)))))

(defcheck solution-2e9494f2
  (fn my-nth [s n]
    (if (zero? n)
      (first s)
      (my-nth (rest s) (dec n)))))

(defcheck solution-2f722b78
  #(if (= %2 0) (first %1) (recur (rest %1) (dec %2))))

(defcheck solution-2fb4fe3c
  (fn [x s]
    (if (zero? s)
      (first x)
      (recur (rest x) (dec s)))))

(defcheck solution-2ff0de0a
  (fn [lst n] (loop [cnt n rng lst]
                (if (= cnt 0) (first rng)
                              (recur (dec cnt) (rest rng))))))

(defcheck solution-30601481
  (fn [col n]
    (loop [col col n n]
      (if (= n 0)
        (first col)
        (recur (rest col) (dec n))))))

(defcheck solution-307c050c
  (fn [c n]
    (if (> n 0)
      (recur (rest c) (dec n))
      (first c))))

(defcheck solution-31153d87
  (fn [x, i]
    (cond
      (empty? x) nil
      (= i 0) (first x)
      :else (recur (rest x) (- i 1)))))

(defcheck solution-315c83e4
  (fn [x i]
    (loop [i i x x]
      (if (= i 0)
        (first x)
        (recur (- i 1) (rest x))))))

(defcheck solution-3166065
  (fn [x n] (if (= n 0) (first x) (recur (rest x) (- n 1) )   ) ))

(defcheck solution-32a2c356
  #(first (reverse (take (+ 1 %2) %1))))

(defcheck solution-32d38824
  (fn spin [l, n]
    (if
     (= n 0)
      (first l)
      (spin (rest l) (- n 1)))))

(defcheck solution-32f5d0c4
  #(if (= 0 %2)
     (first %)
     (recur (rest %) (- %2 1))))

(defcheck solution-33344cb3
  (fn [_list index]
    (loop [
           the-list _list
           current-index 0
           ]
      (if
       (= current-index index)
        (first the-list)
        (recur (next the-list) (inc current-index))
        ))))

(defcheck solution-333e52a7
  (fn [collection index] (first (drop index collection))))

(defcheck solution-33aaa59f
  (fn [x n] (first (nthrest x n))))

(defcheck solution-33bbb2a0
  (fn [col n] (if (= 0 n) (first col) (recur (rest col) (dec n)))))

(defcheck solution-33c1b55a
  (fn [sq counter]
    (if (= 0 counter)
      (first sq)
      (recur (rest sq) (dec counter)))))

(defcheck solution-33fc3442
  #(loop [ns %1 index 0]
     (if (= index %2)
       (first ns)
       (recur (rest ns) (inc index)))))

(defcheck solution-3424fe49
  (fn [col n] (last (take (inc n) col))))

(defcheck solution-34552b6b
  (fn [l k]
    (if (= k 0)
      (first l)
      (recur (rest l) (dec k)))))

(defcheck solution-359afd6f
  (fn my-nth [xs n]
    (if (zero? n)
      (first xs)
      (my-nth (rest xs) (dec n)))))

(defcheck solution-35a0bd8d
  (fn [coll n] ((apply comp (cons first (repeat n rest))) coll)))

(defcheck solution-365e5e51
  (fn my-nth [xs, n]
    (cond
      (= n 0) (first xs)
      true    (my-nth (rest xs) (- n 1))
      )
    ))

(defcheck solution-3675e38a
  #( (comp first (partial drop %2)) %1 ))

(defcheck solution-3676b956
  (fn [sequence n]
    (loop [s sequence i 0]
      (if (= i n)
        (first s)
        (recur (rest s) (inc i))))))

(defcheck solution-367a5474
  #(loop [a %1 b %2] (if (= b 0) (first a)
                                 (recur (rest a) (- b 1)))))

(defcheck solution-367deb02
  (fn [x y] (first (drop y x))))

(defcheck solution-36bcf9b1
  (fn [sq pos] (get (vec sq) pos)))

(defcheck solution-36efcb36
  #((into [] %1) %2))

(defcheck solution-37772eab
  (fn mth [coll index]
    (first (drop index coll))))

(defcheck solution-379f8e6
  (fn [L n]
    (loop [n n, L L]
      (if (= 0 n)
        (first L)
        (recur (- n 1) (rest L))))))

(defcheck solution-387c52a9
  (fn [coll n] (first (reduce #(%2 %) coll (repeat n rest)))))

(defcheck solution-38e65e4c
  (fn [coll elem] (get (vec coll) elem)))

(defcheck solution-391c6ec8
  (fn [l n]
    (if
     (= n 0)
      (first l)
      (recur (rest l) (dec n))
      )
    ))

(defcheck solution-39851cac
  (fn [l, n](first (drop n l))))

(defcheck solution-39cb25dd
  (fn [s n]
    (loop [se s cn 0]
      (if (= cn n)
        (first se)
        (recur (rest se) (inc cn))))))

(defcheck solution-3a03de65
  (fn [L n] ((apply vector	L) n)))

(defcheck solution-3a6d5f33
  (fn mm [sq n]
    (if (= 0 n)
      (first sq)
      (mm (rest sq) (- n 1)))))

(defcheck solution-3a82ef64
  (fn [coll idx] (first (nthnext coll idx))))

(defcheck solution-3a90c8cc
  (fn f [l i] (if (= 0 i) (first l) (f (rest l) (- i 1)))))

(defcheck solution-3c57ef44
  (fn [coll n]
    (if (= 0 n)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-3c581b6b
  #(loop [result 0 col %1 th 0]
     (if (> th %2)
       result
       (recur (first col) (rest col) (+ 1 th)))))

(defcheck solution-3dcc6c29
  (fn [l n] (->> l (drop n) (first))))

(defcheck solution-3e252490
  #(-> %1 (nthnext %2) first))

(defcheck solution-3ff98462
  #(loop [i 0 coll %1] (if (= i %2) (first coll) (recur (inc i) (next coll)))))

(defcheck solution-4003e174
  (fn mynth [seq i]
    (if (= i 0)
      (first seq)
      (mynth (rest seq) (- i 1)))))

(defcheck solution-404d7c46
  (fn [col n]
    (if (zero? n)
      (first col)
      (recur (rest col) (- n 1)))))

(defcheck solution-412b0e25
  (fn [s n]
    (if (zero? n)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-4154a974
  (fn f [s n]
    (if (= n 0)
      (first s)
      (f (rest s) (dec n)))))

(defcheck solution-41f339f9
  #(loop [x %1 y %2] (if (zero? y) (first x) (recur (rest x) (dec y)))))

(defcheck solution-4229d02a
  (fn [xs n] (if (= n 0)
               (first xs)
               (recur (rest xs) (dec n)))))

(defcheck solution-42460711
  (fn [s i]
    (last (take (inc i) s))))

(defcheck solution-4370e93d
  (fn [coll n]
    (if (= n 0) (first coll) (recur (rest coll) (dec n)))))

(defcheck solution-43ea1dd9
  ; I come from Lisp!
  (fn mynth [seq num]
    (if (> num 0) (mynth (rest seq) (- num 1)) (first seq))))

(defcheck solution-44059e1f
  (fn f [x y]
    (if (= y 0) (first x) (f (rest x) (dec y)))))

(defcheck solution-440de46b
  #(if (= %2 0) (first %1) (recur (rest %1) (- %2 1))))

(defcheck solution-44a6f427
  (fn foo [coll n]
    (if (= n 0) (first coll) (foo (rest coll) (dec n)))))

(defcheck solution-44fe05db
  #(first (drop %2 (take (inc %2) %1))))

(defcheck solution-4501a8bc
  (fn [lst n]
    (loop [k n lst lst]
      (if (= k 0)
        (first lst)
        (recur (dec k) (rest lst))))))

(defcheck solution-45560fbf
  (fn [s n]
    (if (zero? n)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-4572abb8
  (fn tn [col n]
    (if (= 0 n) (first col)
                (recur (rest col) (dec n)))))

(defcheck solution-45733350
  #(loop [xs %1 n %2] (if (zero? n) (first xs) (recur (rest xs) (dec n)))))

(defcheck solution-45ec8c7d
  #(cond (= %2 0)          (first %1)
         (< %2 0)          nil
         (> %2 (count %1)) nil
         true              (recur (rest %1) (dec %2))))

(defcheck solution-468f364d
  (fn   [s n]
    (loop [i 0
           head (first s)
           tail (rest s)]
      (if (= i n)
        head
        (recur (inc i) (first tail) (rest tail))))))

(defcheck solution-477a7c08
  #(loop [li %1 i 0]
     (if (= i %2) (first li)
                  (recur (rest li) (inc i)))))

(defcheck solution-47a53827
  (fn [s n]
    (loop [s' s i 0]
      (if
       (= i n)
        (first s')
        (recur (rest s') (inc i))))))

(defcheck solution-47f9aca8
  (fn [s x] (last (take (inc x) s))))

(defcheck solution-4902bb5e
  (fn my-nth [coll index]
    (if (= 0 index)
      (first coll)
      (recur (rest coll) (dec index)))))

(defcheck solution-4970992e
  #(if (= 0 %2) (first %1) (recur (rest %1)(dec %2))))

(defcheck solution-4a004c21
  (fn m [coll n]
    (if (> n 0)
      (m (rest coll) (dec n))
      (first coll))))

(defcheck solution-4a0188ab
  (fn nt [s n]
    (->> s (drop n) first)))

(defcheck solution-4a0850fc
  (fn [seq,target] (loop [s seq
                          pos 0]
                     (if (< pos target)
                       (recur (rest s) (+ pos 1))
                       (first s)))))

(defcheck solution-4a2fbcc4
  (fn [coll n]
    (loop [c coll n n]
      (if (or (= n 0)
              (nil? c))
        (first c)
        (recur (next c)
          (dec n))))))

(defcheck solution-4a69a66a
  #(loop [n %2 coll %]
     (if (zero? n)
       (first coll)
       (recur (dec n) (next coll)))))

(defcheck solution-4ae6994b
  #(loop [xs %1 n %2]
     (if (= n 0)
       (first xs)
       (recur (rest xs) (dec n)))))

(defcheck solution-4b282f4
  ;(fn [coll n]
  ;  (first ((apply comp (repeat n rest)) coll)))

  #(first (drop %2 %)))

(defcheck solution-4b87c1c2
  #(if (zero? %2) (first %1) (recur (rest %1) (dec %2))))

(defcheck solution-4bd3abf8
  (letfn [(I [coll i]
            (if (zero? i) (first coll)
                          (recur (rest coll) (dec i))))]
    I))

(defcheck solution-4c79277d
  (fn [lst n] (first (loop [x (reverse lst)]
                       (if (>= (+ n 1) (count x))
                         x
                         (recur (rest x))
                         )))
    ))

(defcheck solution-4c8eb10f
  (fn ith [c i] (if (= 0 i) (first c) (ith (rest c) (- i 1)))))

(defcheck solution-4cc2a7b5
  #((vec %) %2 ))

(defcheck solution-4e92246e
  (fn [col n]
    (if (zero? n)
      (first col)
      (recur (rest col) (- n 1)))))

(defcheck solution-4f0ba0b8
  (fn [c i] (if (= i 0) (first c) (recur (rest c) (dec i)))))

(defcheck solution-4feca5d5
  (fn [l n] (if (zero? n) (first l) (recur (rest l) (dec n)))))

(defcheck solution-503b73f5
  (fn [coll1 pos1]
    (loop [coll coll1
           pos 0
           ]
      (let [head (first coll) tail (rest coll)]
        (if (= pos pos1)
          head
          (recur tail (inc pos))
          )
        )
      )))

(defcheck solution-51872d57
  (fn [coll index]
    (if ( = 0 index) (first coll)
                     ( recur (rest coll) (dec index)))))

(defcheck solution-532d5f42
  (fn [xs num] (get (into [] xs) num)))

(defcheck solution-5365c358
  (fn neth [xs n] (if (< n 1) (first xs) (neth (rest xs) (- n 1)))))

(defcheck solution-53728cb7
  (fn my-nth [l n]   (if (= n 0)     (first l)    (recur (rest l) (dec n)))))

(defcheck solution-53789cb5
  (fn [xs n]
    (if (zero? n)
      (first xs)
      (recur (rest xs) (dec n)))))

(defcheck solution-53920a7b
  #(get (into [] %1) %2))

(defcheck solution-539d24e1
  (fn [s n] (first (drop n s))))

(defcheck solution-54011383
  (fn [s i]
    (first (drop i s))))

(defcheck solution-548c2ac9
  (fn my-nth [coll n] (if (= n 0) (first coll) (my-nth (rest coll) (- n 1)))))

(defcheck solution-54f6986f
  (fn[x y] (aget (into-array x) y)))

(defcheck solution-553e2428
  (fn
    [s n]
    (if (zero? n)
      (first s)
      (when (seq s)
        (recur (next s) (dec n))))))

(defcheck solution-55638bb5
  (fn funkki ([l n](funkki l n 0))([l n c](if (= n c) (first l) (recur (rest l) n (inc c))))))

(defcheck solution-55f2a7af
  (fn [v n] (if (= n 0) (first v) (recur (rest v) (- n 1)))))

(defcheck solution-564c2c2b
  (fn en
    [arr n]
    (let [x (first arr) xs (rest arr) c n]
      (if (= c 0)
        x
        (en xs (dec c))))))

(defcheck solution-568761bf
  #(loop [x %2 y %1] (if (> x 0)(recur(dec x)(next y))(first y))))

(defcheck solution-56e94525
  (fn ! [x,y] (if (= y 0) (first x) (! (rest x)  (dec y)))))

(defcheck solution-56faa235
  (fn [col n] (second (last (map vector (range (inc n)) col)))))

(defcheck solution-572e9f0c
  (comp first nthnext))

(defcheck solution-57b18838
  #(let [m (zipmap (range 0 (count %1)) %1)]
     (get m %2)))

(defcheck solution-581c539
  (fn my-fun [sq, n]
    (if (zero? n)
      (first sq)
      (my-fun (rest sq) (dec n)))))

(defcheck solution-588131aa
  (fn [coll index] (first (drop index coll))))

(defcheck solution-58b90eaa
  (fn indexn [x y] (get (vec x) y)))

(defcheck solution-58d533d6
  (fn [coll m]
    (if (= 0 m) (first coll) (recur (rest coll) (dec m)))))

(defcheck solution-594fa305
  (fn [seq n]
    (if (= n 0) (first seq)
                (recur (rest seq) (dec n)))))

(defcheck solution-59652f56
  (fn [x i] ((zipmap (range) x) i)))

(defcheck solution-5990f1c9
  (fn [elems n]
    (loop [i 0
           curr (first elems)
           remain (rest elems)]
      (if (= i n)
        curr
        (recur (inc i) (first remain) (rest remain))))))

(defcheck solution-59e51b6d
  (fn f [x n] (if (= 0 n) (first x) (f (rest x) (dec n)))))

(defcheck solution-5afdef32
  (fn nth-- [l n]
    (if (= n 0)
      (first l)
      (nth-- (rest l) (dec n)))))

(defcheck solution-5b654919
  (comp first (fn [c n] (drop n c))))

(defcheck solution-5bbca2e6
  (fn [x y]
    (loop [list x num y time 0]
      (if (= time num)
        (first list)
        (recur (rest list) num (+ time 1))
        )
      )
    ))

(defcheck solution-5bdb147a
  #(loop [xs %1 i  0]
     (if (== i %2)
       (first xs)
       (recur (next xs) (inc i)))))

(defcheck solution-5c77773c
  (fn [col ind] (if (zero? ind) (first col) (recur (rest col) (dec ind)))))

(defcheck solution-5e3dfd3c
  (fn my-nth [l n]
    (if (= n 0)
      (first l)
      (my-nth (rest l) (- n 1)))))

(defcheck solution-615936f3
  #(last (take (inc %2) %)))

(defcheck solution-61764aff
  (fn [l n] (first (drop n l))))

(defcheck solution-61c56770
  #(loop [count %2, coll %1]
     (if (zero? count) (first coll)
                       (recur (dec count) (next coll)))))

(defcheck solution-61dcca2d
  (fn me [l n] (if (= 0 n) (first l) (me (rest l) (- n 1)) )))

(defcheck solution-62429c4
  (fn [a-seq n]
    (if (zero? n)
      (first a-seq)
      (recur (rest a-seq) (dec n)))))

(defcheck solution-62c856a8
  (fn [sequence index]
    (loop [s (rest sequence), x (first sequence), i 0]
      (cond (= index i) x
            :else (recur (rest s) (first s) (inc i))))))

(defcheck solution-651baae8
  (fn [xs i](if (zero? i) (first xs) (recur (rest xs) (dec i)))))

(defcheck solution-657c79ed
  #(first (nthnext %1 %2)))

(defcheck solution-65938a9d
  (fn [lst pos]
    (loop [l lst, n pos]
      (if (= 0 n)
        (first l)
        (recur (rest l) (dec n))))))

(defcheck solution-65c06535
  (fn [xs n]
    (if (empty? xs)
      nil
      (->> xs (drop n) first))))

(defcheck solution-65d84831
  ;(fn [lst pos]
  ;  (loop [l lst n pos]
  ;    (if (zero? n)
  ;      (first l)
  ;      (recur (rest l) (- n 1)))))
  #(first (drop %2 %)))

(defcheck solution-65e166e7
  (fn n-th [ls n]
    (if (zero? n)
      (first ls)
      (n-th (rest ls) (dec n)))))

(defcheck solution-6628ee7e
  (fn [col idx]
    (if (= idx 0)
      (first col)
      (recur (rest col) (dec idx)))))

(defcheck solution-66525e7f
  (fn
    [ls pos]
    (if (zero? pos)
      (first ls)
      (recur (rest ls) (dec pos)))))

(defcheck solution-675b01fa
  (fn [xs n] (first (drop n xs))))

(defcheck solution-6784197d
  (fn [x i] (if (zero? i) (first x) (recur (next x) (dec i)))))

(defcheck solution-679691f5
  (fn [c n] (if (zero? n) (first c) (recur (rest c) (dec n)))))

(defcheck solution-67ceef70
  (fn nth-4c
    [collection n]
    (if (= n 0)
      (first collection)
      (nth-4c (next collection) (- n 1)))))

(defcheck solution-685a6235
  (fn [l n]
    (loop [a l, b n]
      (if (= b 0)
        (first a)
        (recur (next a) (dec b))))))

(defcheck solution-68ecec4d
  (fn [s n]
    (if (= 0 n)
      (first s)
      (recur (rest s)
        (dec n)))))

(defcheck solution-693c5fca
  (fn enth [l, n]
    (if (= n 0)
      (first l)
      (enth (rest l) (- n 1)))))

(defcheck solution-69474261
  (fn [s n]
    (if (zero? n)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-69753f00
  #(if (= 0 %2) (first %1) (recur (rest %1) (dec %2))))

(defcheck solution-6a1d81c4
  #(loop [x %2, seq %] (if (= x 0) (first seq) (recur (dec x) (rest seq)))))

(defcheck solution-6a73fde0
  #(loop [s % i %2]
     (if (= i 0) (first s)
                 (recur (next s) (dec i)))))

(defcheck solution-6c745bba
  (fn [seq n]
    (if (= 0 n)
      (first seq)
      (recur (rest seq) (dec n)))))

(defcheck solution-6dfe3c27
  (let [f (fn [f c n] (if (= 0 n) (first c) (f f (rest c) (dec n))))] (partial f f)))

(defcheck solution-6e431582
  (fn [x i] (if (= i 0) (first x) (recur (next x) (dec i)))))

(defcheck solution-6e51d340
  (fn [coll n]
    (if (zero? n)
      (first coll)
      (recur (next coll) (dec n)))))

(defcheck solution-6e9e23e2
  (fn [seq idx]
    (get (vec seq) idx)))

(defcheck solution-6ea6f393
  (fn [l n]
    (if (= n 0)
      (first l)
      (recur (rest l) (dec n)))))

(defcheck solution-6f2b4566
  (fn [xs n]
    (if (= n 0) (first xs) (recur (rest xs) (- n 1)))))

(defcheck solution-6f86bfd1
  (fn [col n] (if (> n 0) (recur (rest col) (dec n)) (first col))))

(defcheck solution-70144fce
  (fn rec [l n] (if (= n 0) (first l) (rec (rest l) (- n 1)))))

(defcheck solution-709579d9
  (fn [sequenc n]
    (loop [i 0, s sequenc]
      (if (= i n)
        (first s)
        (recur (inc i) (rest s))))))

(defcheck solution-70d1ae3e
  (fn mynth [l, n]
    (if (= n 0)
      (first l)
      (mynth (rest l) (dec n)))))

(defcheck solution-71385786
  (fn [col index] (if (= index 0)
                    (first col)
                    (recur (rest col) (dec index)))))

(defcheck solution-71f448f3
  (fn step [coll k]
    (if (= k 0) (first coll) (step (rest coll) (- k 1)))
    ))

(defcheck solution-723fda1b
  (fn [seq idx]
    (cond (nil? seq) nil
          (= idx 0) (first seq)
          :else (recur (rest seq) (dec idx)))))

(defcheck solution-73b536e4
  (fn my-nth [s n] (if (= n 0)
                     (first s)
                     (my-nth (rest s) (- n 1)))))

(defcheck solution-7467d904
  (fn beh [x n]
    (if (> n 0) (beh (rest x) (- n 1)) (first x))))

(defcheck solution-749038e0
  (fn [s n]
    (letfn [(my-nth [s n]
              (if (zero? n)
                (first s)
                (recur (rest s) (dec n))))]
      (my-nth (seq s) n))))

(defcheck solution-74dd0291
  (fn [s n] (if (zero? n) (first s) (recur (rest s) (dec n)))))

(defcheck solution-74e3b32a
  #(->> %1 (drop %2) (first)))

(defcheck solution-74eba6a
  (fn nt[s n] (if (= 0 n)
                (first s)
                (nt (next s) (- n 1))
                )))

(defcheck solution-758ef3f4
  (fn [coll i]
    (if (= i 0)
      (first coll)
      (recur (rest coll) (- i 1)))))

(defcheck solution-75f0abea
  (fn nt [s n] (if (= n 0) (first s) (nt (rest s) (- n 1)))))

(defcheck solution-76287587
  (fn [x n] (first (reverse (take (+ n 1) x)))))

(defcheck solution-762d171a
  (fn [col n] (if (zero? n) (first col) (recur (rest col) (dec n)))))

(defcheck solution-7640c4d5
  (fn mynth [xs n]
    (if (<= n 0)
      (first xs)
      (mynth (next xs) (dec n)))))

(defcheck solution-773a5010
  #(loop [xs %1 i %2] (if (== i 0) (first xs) (recur (rest xs) (dec i)))))

(defcheck solution-779a3557
  (fn mynth [s n]
    (if (= n 0)
      (first s)
      (mynth (rest s) (- n 1)))))

(defcheck solution-77c92979
  (fn [s n](first (drop n s))))

(defcheck solution-77d49e55
  (fn [sq n] (last (take (+ n 1) sq))))

(defcheck solution-7850bfff
  #((comp first drop) %2 %1))

(defcheck solution-7995885b
  (fn n-th [coll n]
    (if (< n 0) nil
                (loop [i 0
                       seq coll]
                  (if (= i n) (first seq)
                              (recur (inc i) (rest seq)))))))

(defcheck solution-7a7ad4fa
  (fn [coll idx]
    (loop [coll coll idx idx]
      (if (= idx 0)
        (first coll)
        (recur (rest coll) (dec idx))))))

(defcheck solution-7a80b73f
  (fn my-nth [coll n]
    (cond (zero? n)  (first coll)
          (seq coll) (my-nth (rest coll) (dec n)))))

(defcheck solution-7c6cda22
  (fn extractN [l n] (if (= n 0) (first l) (extractN (rest l) (- n 1)))))

(defcheck solution-7c797e86
  #(last (take (+ %2 1) %1)))

(defcheck solution-7c8c16ef
  (fn [s n]
    (let [finder (fn [s i]
                   (if (= n i)
                     (first s)
                     (recur (rest s) (inc i))))]
      (finder s 0))))

(defcheck solution-7d2ae307
  (fn get-nth [xs n]
    (first (drop n xs))))

(defcheck solution-7d2e8fd
  (fn [col, idx] (last (take (+ idx 1) col))))

(defcheck solution-7d77ad19
  (fn my-nth [li x] (if (= x 0) (first li) (my-nth (rest li) (dec x)))))

(defcheck solution-7d77c65d
  (fn mynth [seq n]
    (if (= n 0)
      (first seq)
      (recur (rest seq) (dec n)))))

(defcheck solution-7d86c76c
  #(->> %1 (drop %2) first))

(defcheck solution-7dc3cda
  #(let [end-idx %2]
     (loop [idx 0 arr %1]
       (if(= idx end-idx)
         (first arr)
         (recur (inc idx) (rest arr))))))

(defcheck solution-7dcd2fd5
  (fn nitem [a b]
    (if (zero? b)
      (first a)
      (nitem (rest a) (dec b)))))

(defcheck solution-7ebf9fc9
  #(loop [items %1 n %2]
     (if (= n 0)
       (first items)
       (recur (rest items) (dec n)))))

(defcheck solution-7ec8bcb5
  (fn [xs n] (if (= n 0) (first xs) (recur (rest xs) (- n 1)))))

(defcheck solution-7f009db2
  (fn [l n] (last (take (+ n 1) l))))

(defcheck solution-7f0c92d3
  #(first(drop %2 %)))

(defcheck solution-7f8666c9
  (fn [coll n] (last (take (+ n 1) coll))))

(defcheck solution-7f933443
  (fn [coll pos]
    (if (zero? pos) (first coll)
                    (recur (rest coll) (dec pos)))))

(defcheck solution-8048b33a
  (fn [xs n] (first (drop n xs))))

(defcheck solution-817ce292
  #(get (vec %) %2))

(defcheck solution-81f3d18
  #(-> %2 (drop %1) first))

(defcheck solution-8219ebad
  (fn [my-coll my-plc]
    (if (= my-plc 0)
      (first my-coll)
      (recur (rest my-coll) (- my-plc 1)))))

(defcheck solution-82523791
  (fn [s x]
    (last (take (inc x) s))))

(defcheck solution-8285f44f
  (comp first #(drop %2 %1)))

(defcheck solution-8294503d
  (fn [a b]
    (loop [l a
           i b]
      (if (= 0 i)
        (first l)
        (recur (rest l) (- i 1))
        ))))

(defcheck solution-82d8f840
  (fn th[x, c]
    (if (= c 0)
      (first x)
      (th (rest x) (- c 1))
      )
    ))

(defcheck solution-840b713c
  #(if (> %2 0) (recur (rest %1) (dec %2)) (first %1)))

(defcheck solution-8420143e
  (fn [xs n]
    (if (= n 0)
      (first xs)
      (recur (rest xs) (- n 1)))))

(defcheck solution-8565b117
  (fn [l n] (if (> n 0) (recur (rest l) (dec n)) (first l))))

(defcheck solution-857a8d54
  (fn [x n] (first (drop n x))))

(defcheck solution-86be11a5
  #( if (zero? %2) (first %1) (recur (rest %1) (dec %2))))

(defcheck solution-87eb1208
  (fn [coll n]
    (loop [coll coll n n]
      (if (zero? n)
        (first coll)
        (recur (rest coll) (dec n))))))

(defcheck solution-88708e4c
  (fn [x n] (loop [a x k n] (if (zero? k) (first a) (recur (rest a) (dec k))))))

(defcheck solution-88b71846
  (fn [coll n] (second (take-nth n coll))))

(defcheck solution-88bbb022
  #(first (nthrest % %2)))

(defcheck solution-895857a4
  (fn [s i] (if (= i 0) (first s) (recur (rest s) (- i 1)))))

(defcheck solution-89908cf0
  #((apply comp (cons first (take %2 (repeat rest)))) %1))

(defcheck solution-89cd4206
  (fn [l n] (if (= n 0) (first l) (recur (rest l) (- n 1)))))

(defcheck solution-89ea638c
  #(loop [iter 0 values %1]
     (if (= iter  %2)
       (first values)
       (recur (inc iter) (rest values)))))

(defcheck solution-8a2dd051
  (fn new-nth [coll n]
    (if (zero? n)
      (first coll)
      (new-nth (rest coll) (dec n)))))

(defcheck solution-8a3f278
  (fn [s n]
    (if (= n 0)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-8ae86337
  (fn [c i] (if (= 0 i) (first c) (recur (rest c) (dec i)))))

(defcheck solution-8aedac54
  (fn my-n [seq num]
    (if (= 0 num)
      (first seq)
      (my-n (rest seq) (- num 1)))))

(defcheck solution-8b31dec3
  (fn [s, index]
    (if (= index 0)
      (first s)
      (recur (rest s) (- index 1)))))

(defcheck solution-8b8fe04b
  (fn my-nth [coll n] (if (= 0 n) (first coll) (my-nth (rest coll) (- n 1)))))

(defcheck solution-8bab179f
  (fn [l i] (second (first (filter #(= (first %) i) (map-indexed list l))))))

(defcheck solution-8c8f8ce5
  (fn n-th
    [c,i]
    (if (= i 0)
      (first c)
      (n-th (rest c) (dec i)))))

(defcheck solution-8ccba0c7
  (fn [aseq n]
    (cond
      (empty? aseq) nil
      (= 0 n) (first aseq)
      :else (recur (rest aseq) (dec n)))))

(defcheck solution-8cf9d61e
  (fn nth_n [l n]
    (cond (= n 0) (first l)
          :else (recur (rest l) (dec n) ))))

(defcheck solution-8d14bc8b
  #(first (reverse (take (+ %2 1) %1))))

(defcheck solution-8d4cc10
  (fn [s n] (if (= n 0) (first s) (recur (rest s) (dec n)))))

(defcheck solution-8e4915e3
  (fn [seq index]
    (loop [cur-seq seq
           cur-index 0]
      (if (= cur-index index)
        (first cur-seq)
        (recur (rest cur-seq) (inc cur-index )
          )
        )
      )
    ))

(defcheck solution-8efa50d3
  #(loop [wanted %2 source %1] (if (zero? wanted)
                                 (first source) (recur (dec wanted) (rest source)))))

(defcheck solution-8f10f1dc
  (fn [c i]
    (if (= i 0)
      (first c)
      (recur (next c) (dec i)))))

(defcheck solution-8f3f8f9c
  (partial
    (fn [z x y]
      (if (= y z)
        (first x)

        (recur (inc z) (rest x) y )))
    0))

(defcheck solution-8f8c88d6
  (fn [x index]
    (if (= index 0)
      (first x)
      (recur (rest x) (dec index)))))

(defcheck solution-8fb616c7
  #(loop [n %2 s %1]
     (if (zero? n)
       (first s)
       (recur (dec n) (rest s)))))

(defcheck solution-90b91a80
  (fn element-at [list n]
    (let [go (fn [list n] (if (and (pos? n) list) (recur (rest list) (dec n)) list))]
      (first(go list n)))))

(defcheck solution-918ffd17
  (fn ! [x y]
    (if (= y 0)
      (first x)
      (! (rest x) (- y 1)))))

(defcheck solution-919b38d8
  ;(fn [coll n]
  ;  (if (= 0 n)
  ;    (first coll)
  ;    (recur (rest coll) (dec n))))


  ; below not good if index > size of seq
  #(last (take (inc %2)  %1)))

(defcheck solution-92240c8e
  (fn [sequ n] ((vec sequ) n)))

(defcheck solution-922c157e
  (fn [lst n] (if (zero? n) (first lst) (recur (rest lst) (dec n)))))

(defcheck solution-92cbb9bf
  #(last (take (inc %2) %1)))

(defcheck solution-93029883
  (fn [xs n]
    (if (<= n 0)
      (first xs)
      (recur (next xs) (dec n)))))

(defcheck solution-93e24117
  #((apply array-map (interleave (range (count %1)) %1)) %2))

(defcheck solution-94ece0bb
  (fn get-element [xs n]
    (if (= n 0)
      (first xs)
      (recur (rest xs) (dec n)))))

(defcheck solution-950cbabc
  (fn
    [list, i]
    (if (= i 0)
      (first list)
      (recur (rest list) (dec i)))))

(defcheck solution-952ef546
  (fn [coll n]
    (loop [coll coll cnt 0]
      (if (= cnt n) (first coll)
                    (recur (rest coll) (inc cnt)))
      )
    ))

(defcheck solution-9539a610
  #(first (drop %2  %1)))

(defcheck solution-9555db34
  (fn [seq n]
    (last (take (+ n 1) seq))))

(defcheck solution-9595a857
  (fn [c i] (if (zero? i) (first c) (recur (rest c) (dec i)))))

(defcheck solution-95e50255
  (fn [seq n] (if (zero? n) (first seq) (recur (rest seq) (dec n)))))

(defcheck solution-9621a757
  #(get (vec %1) %2))

(defcheck solution-96b9dde3
  (fn f [xs n] (if (= n 0) (first xs) (f (rest xs) (- n 1)))))

(defcheck solution-96d0893a
  (fn Nth [s n] (if (= n 0) (first s) (Nth (rest s) (- n 1)))))

(defcheck solution-96dc90a4
  (fn [coll n]
    (loop [i 0
           coll coll]
      (if (= i n)
        (first coll)
        (if (seq (rest coll)) (recur (inc i) (rest coll)))))))

(defcheck solution-98230fe3
  (fn foo [xs n]
    (if (= n 0)
      (first xs)
      (foo (rest xs) (dec n)))))

(defcheck solution-984aaef5
  (fn [x,y] (first (nthnext x y))))

(defcheck solution-985d8624
  #(last (take (+ 1 %2) %)))

(defcheck solution-991b39bd
  (fn [coll val] (first (drop val coll))))

(defcheck solution-991d524e
  #(loop [xs % n %2]
     (if (zero? n) (first xs)
                   (recur (rest xs) (dec n)))))

(defcheck solution-99416aed
  (fn [xs n]
    (if (zero? n) (first xs) (recur (rest xs) (dec n)))))

(defcheck solution-9996c161
  (fn [coll i] (first (drop i coll))))

(defcheck solution-99e03440
  (fn [coll index]
    (loop [i 0
           xs coll]
      (if (= i index)
        (first xs)
        (recur (inc i) (rest xs))))))

(defcheck solution-99f9d7a4
  #(if (= %2 0)
     (first %1)
     (recur
       (next %1)
       (dec %2)
       )
     ))

(defcheck solution-9a44bf2d
  (fn [v n]
    (loop [v v
           n n]
      (cond
        (empty? v) nil
        (= 0 n) (first v)
        :else (recur (rest v) (- n 1))))))

(defcheck solution-9a5a4e02
  (fn [coll i] (first (last (take (+ i 1) (iterate next coll))))))

(defcheck solution-9be36b76
  (fn [coll n] ((into [] coll) n)))

(defcheck solution-9c6d1146
  (fn [a b] (last (take (inc b) a))))

(defcheck solution-9cd472b4
  (fn [s n]
    (if ( = n 0) (first s)
                 (recur (rest s) (dec n)))))

(defcheck solution-9d12bc49
  (fn [coll index]
    (loop [in-coll coll
           n index]
      (if (zero? n)
        (first in-coll)
        (recur (rest in-coll) (dec n))))))

(defcheck solution-9ec23e3a
  (fn [coll idx]
    (first (drop idx coll))))

(defcheck solution-9ec3c7f6
  (fn [x i] (if (zero? i) (first x) (recur (rest x) (dec i)))))

(defcheck solution-9f5d0d24
  #(first (drop %2 %)))

(defcheck solution-9fe00a9e
  (fn my-nth [a-seq n]
    (if (= 0 n)
      (first a-seq)
      (my-nth (rest a-seq) (dec n))
      )
    ))

(defcheck solution-a0745651
  (fn [s n]
    (first (drop n s))))

(defcheck solution-a1085187
  (fn [lst n]

    (if (= (count lst) (+ n 1))
      (last lst)
      (recur (butlast lst) n)
      )
    ))

(defcheck solution-a1bc30fb
  (fn [x n] (if (> n 0) (recur (rest x) (dec n)) (first x))))

(defcheck solution-a1faa6b4
  (fn [sq n] (last (take (inc n) sq))))

(defcheck solution-a2281f92
  (fn [coll n] (last (take (inc n) coll))))

(defcheck solution-a448c191
  (fn [s n]
    (if (zero? n) (first s)
                  (recur (rest s) (dec n)))))

(defcheck solution-a53d9e94
  #(loop [coll %1 cnt %2] (if (= 0 cnt) (first coll) (recur (rest coll) (dec cnt)))))

(defcheck solution-a6d152b5
  (fn foo [v n]   (if (zero? n  ) (first v) (foo (rest v) (dec n) )) ))

(defcheck solution-a7347b55
  (fn [seq n]
    (last (take (inc n) seq))))

(defcheck solution-a7aa93b
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (next coll) (- n 1)))))

(defcheck solution-a7c8a328
  #(get (apply vector %1) %2))

(defcheck solution-a7fd4471
  (fn [x n] (if (= n 0) (first x) (recur (rest x) (- n 1)))))

(defcheck solution-a82d54ce
  #(->> % (drop %2) first))

(defcheck solution-aa46e0f5
  (fn [l n]
    (if (= n 0)
      (first l)
      (recur (rest l) (dec n)))))

(defcheck solution-aab04b26
  (fn my-nth [l n] (if (<= n 0) (first l) (my-nth (rest l) (dec n)))))

(defcheck solution-ab9925
  (fn [s i] (last (take (+ i 1) s))))

(defcheck solution-ac31019f
  #(loop [col %1,n %2]
     (if (= 0 n)
       (first col)
       (recur (rest col) (dec n) )
       )
     ))

(defcheck solution-acd8c179
  (fn ! [x y] (if (= y 0) (first x) (! (rest x) (- y 1)))))

(defcheck solution-acf87a20
  #(first ((apply comp (repeat %2 rest)) %1)))

(defcheck solution-ad082bb5
  (fn [x i] (first (drop i x))))

(defcheck solution-ad500296
  (fn [coll n]
    (let [splits (split-at n coll)]
      ((comp first second) splits))))

(defcheck solution-ad5e8ce9
  (fn [col0 pos0]
    (loop [col col0 pos pos0]
      (if (= pos 0 )
        (first col)
        (recur (rest col) (- pos 1) )
        )
      )))

(defcheck solution-ad904874
  (fn [x y] (last (take (inc y) x))))

(defcheck solution-add479a2
  (fn [coll n] (first (nthnext coll n))))

(defcheck solution-ade13928
  (fn [l n] (if (zero? n) (first l) (recur (rest l) (dec n))) ))

(defcheck solution-ae033f11
  (fn [seq n]
    (if (= n 0)
      (first seq)
      (recur (rest seq) (- n 1)))))

(defcheck solution-ae563b34
  (fn [xs i]
    (if (= i 0)
      (first xs)
      (recur (rest xs) (dec i)))))

(defcheck solution-aea27df3
  #(
     loop [lst %1 n %2]
     (if (= n 0)
       (first lst)
       (recur (rest lst) (dec n)))
     ))

(defcheck solution-af476649
  (fn [x c]
    (if (= c 0) (first x) (recur (rest x) (dec c)))))

(defcheck solution-afcb886d
  #(first (nthrest %1 %2)))

(defcheck solution-b05969fa
  (fn f [ls n] (if (= n 0) (first ls) (f (rest ls) (dec n)))))

(defcheck solution-b167745f
  #(first (drop %2 %)))

(defcheck solution-b227bc7
  (fn [list n]
    (loop [i 0 xs list]
      (if (= i n)
        (first xs)
        (recur (inc i) (rest xs))
        ))))

(defcheck solution-b25577d6
  (fn [s n] (-> (drop n s) first)))

(defcheck solution-b28848cc
  (fn myfn [seq, key] (last (take (+ 1 key) seq))))

(defcheck solution-b2a1a97c
  #(if (> %2 0)
     (recur (rest %) (dec %2))
     (first %)))

(defcheck solution-b2d4f3f
  #(loop [n %2 coll %1]
     (if (> n 0) (recur (dec n) (next coll))
                 (first coll))))

(defcheck solution-b38ac821
  (fn [seq index]
    (first (subvec (vec seq) index))))

(defcheck solution-b3b32e74
  (fn my-nth [s n]
    (loop [s s i 0]
      (if (< i n)
        (recur (rest s) (inc i))
        (first s)))))

(defcheck solution-b3bb6b97
  (fn [coll n]
    (if (zero? n)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-b44981e9
  (fn [xs n] (first (reduce #(%2 %1) xs (repeat n rest)))))

(defcheck solution-b49013e7
  (fn my-nth [s n]
    (if (= n 0)
      (first s)
      (my-nth (rest s) (- n 1)))))

(defcheck solution-b4ce509b
  #(first(reverse(take (+ %2 1) %1))))

(defcheck solution-b5c212d7
  (fn nthel [alist, n] (if (> n 0) (nthel (rest alist) (- n 1)) (first alist))))

(defcheck solution-b5cd841d
  #((vec %1) %2))

(defcheck solution-b5d0965d
  (fn [l n]
    (if (= n 0)
      (first l)
      (recur (rest l) (dec n)))))

(defcheck solution-b625d9e1
  (fn get-nth [x, c]
    (if (= c 0)
      (first x)
      (get-nth (rest x) (- c 1))
      )
    ))

(defcheck solution-b6490bdc
  (fn [coll n] (last (take (+ 1 n) coll))))

(defcheck solution-b73ee38a
  (fn[a-seq idx] (first (drop idx a-seq))))

(defcheck solution-b7ff1cf7
  (fn qual [l n] (if (> n 0) (qual (rest l) (- n 1)) (first l))))

(defcheck solution-b8785f6f
  (fn [sq item]
    (last (take (inc item) sq))))

(defcheck solution-b8e502e8
  (fn sol0021-func
    [coll n]
    {:pre [(sequential? coll)
           (integer? n)
           (< -1 n (count coll))]}
    ((apply comp first (repeat n rest)) coll)))

(defcheck solution-b93d46e5
  ; First-attempt
  ;(fn new-th [collection elem] (if (= elem 0) (first collection) (new-th (rest collection) (dec elem))))

  #(first (drop %2 %)))

(defcheck solution-b953d5d0
  (fn nthel [s n]
    (if (= n 0)
      (first s)
      (recur (rest s) (- n 1)))))

(defcheck solution-ba31a374
  #(if (= %2 0)
     (first %1)
     (recur (rest %1) (- %2 1))))

(defcheck solution-ba3a3755
  (fn neth
    ([l n]
     (neth l n 0))
    ([l n acc-val]
     (if (= acc-val n)
       (first l)
       (recur (rest l) n (inc acc-val))) )
    ))

(defcheck solution-bb8c53df
  (fn [coll n] (first (subvec (vec coll) n )
                 )))

(defcheck solution-bc167869
  #(first(drop %2 %1)))

(defcheck solution-bc25378
  (fn [a b] (first (drop b a))))

(defcheck solution-bc3ee814
  (fn [arr n]
    (loop [x 0 v arr]
      (if (= x n)
        (first v)
        (recur (inc x) (rest v))))))

(defcheck solution-bdec9140
  (fn nth2 [coll n]
    (if (zero? n)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-be625397
  (fn [xs n]
    (if (= n 0)
      (first xs)
      (recur (rest xs) (dec n)))))

(defcheck solution-bec5d0da
  (fn [x, n] (first (drop n x))))

(defcheck solution-bed634d2
  (fn nt [list n]
    (if (= n 0)
      (first list)
      (nt (rest list) (- n 1)))))

(defcheck solution-bf0c10b5
  (fn [xs n] (if (= n 0) (first xs) (recur (rest xs) (dec n)))))

(defcheck solution-bf59ae36
  (fn [x y] (first (drop y  x))))

(defcheck solution-bfa3cdc8
  #(second (take-nth %2 %1)))

(defcheck solution-bfcb99e2
  (fn [col n] (loop [c col i n] (if (= i 0) (first c) (recur (rest c) (dec i))))))

(defcheck solution-bfcca3df
  (fn [l i] (
              loop [ll l index i]
              (if (= index 0)
                (first ll)
                (recur (rest ll) (dec index))
                ))))

(defcheck solution-bfe592d6
  #(if (pos? %2) (recur (rest %1) (dec %2)) (first %1)))

(defcheck solution-c1244b6d
  (fn [s n]
    (if (zero? n)
      (first s)
      (recur (rest s) (dec n)))))

(defcheck solution-c19a6a12
  (fn [coll n]
    (last (take (inc n) coll))))

(defcheck solution-c1c4ad32
  #((apply vector %) %2))

(defcheck solution-c1c8f857
  (fn mynth [v n]
    (if (= n 0)
      (first v)
      (mynth (rest v) (dec n)))))

(defcheck solution-c1ffd17e
  (fn [sq n] (loop [remaining sq m 0] (if (= n m) (first remaining) (recur (rest remaining) (inc m))))))

(defcheck solution-c25fd337
  (fn [s n] (cond (empty? s) s (= n 0) (first s) :else (recur (rest s) (dec n)))))

(defcheck solution-c2beffae
  (fn [items n]
    (loop [i 0
           items items]
      (if (= i n)
        (first items)
        (recur (inc i) (next items))))))

(defcheck solution-c39259cb
  (fn n-esimo [s n]
    (if (= 0 n)
      (first s)
      (n-esimo (rest s) (dec n)))))

(defcheck solution-c3eaa6db
  (fn [s n]
    (get (vec s) n)
    ))

(defcheck solution-c468970
  (fn [l n] (if (pos? n)
              (recur (rest l) (dec n))
              (first l))))

(defcheck solution-c49bee0d
  (fn [l i] (get (into [] l) i)))

(defcheck solution-c547875e
  (fn index-of [x n]
    (if
     (= n 0)
      (first x)
      (index-of (rest x) (dec n))
      )))

(defcheck solution-c563ff24
  #((zipmap (range) %1) %2))

(defcheck solution-c6dc5420
  (fn foo [a n] (if (= n 0) (first a) (foo (rest a) (- n 1)))))

(defcheck solution-c6e642e3
  #(loop[m %1, i %2] (if (= 0 i) (first m) (recur (rest m) (dec i)))))

(defcheck solution-c7666422
  (fn [s x](last (take (inc x) s))))

(defcheck solution-c7d89509
  (fn [xs n]
    (loop [ys xs m n]
      (if (zero? m)
        (first ys)
        (recur (rest ys) (dec m))))))

(defcheck solution-c8397274
  (fn [seq n]
    (last (take (+ n 1) seq))))

(defcheck solution-c897bd05
  (fn [s n]
    (when (seq s)
      (if (zero? n) (first s) (recur (rest s) (dec n))))))

(defcheck solution-c8d665c4
  (fn [col n]
    (if (zero? n)
      (first col)
      (recur (rest col)
        (- n 1)))))

(defcheck solution-cac443ee
  (fn [s i]
    (if (<= i 0)
      (first s)
      (recur (rest s) (dec i)))))

(defcheck solution-cc6bb91e
  (fn [l n]
    (loop [mylist l cnt n]
      (if (= cnt 0)
        (first mylist)
        (recur (rest mylist) (dec cnt))))))

(defcheck solution-ccb81178
  (fn [lst n] (if (= 0 n) (first lst) (recur (rest lst) (dec n)))))

(defcheck solution-ccc8dff3
  (fn f [x c]
    (if (zero? c)
      (first x)
      (f (rest x) (- c 1)))))

(defcheck solution-ccfc2181
  (fn [xs i]
    (if (pos? i)
      (recur (rest xs) (dec i))
      (first xs))))

(defcheck solution-cdf31dd6
  #(first (take 1 (drop %2 %1))))

(defcheck solution-cebaa6ab
  (fn [coll n]
    (if (zero? n) (first coll) (recur (rest coll) (dec n)))))

(defcheck solution-cebf6c
  (fn [sq n]
    (loop [sq sq
           n n]
      (if (= n 0)
        (first sq)
        (recur (rest sq) (- n 1))))))

(defcheck solution-cf50024a
  (fn myrest[x n]
    (if (= n 0)
      (first x)
      (myrest (rest x) (- n 1))
      )
    ))

(defcheck solution-cf58d34
  (fn nnth [l n]
    (if (= 0 n)
      (first l)
      (nnth (rest l) (- n 1)))))

(defcheck solution-cff36e81
  (fn mnth [lst n]
    (if (zero? n)
      (first lst)
      (mnth (rest lst) (dec n)))))

(defcheck solution-d076f9bd
  (fn [l n]
    (loop [ll l
           nn n]
      (if (= nn 0)
        (first ll)
        (recur (rest ll) (dec nn))
        ))))

(defcheck solution-d0a96ada
  (fn my-nth [s i] (if (= i 0) (first s) (my-nth (rest s) (dec i)))))

(defcheck solution-d0ad5f2
  (fn nthelem
    [sequence n]
    ((comp first drop) n sequence)))

(defcheck solution-d0e6331e
  (fn [coll n] (first (drop n coll))))

(defcheck solution-d12f31bb
  (fn n-th [coll n]
    (if (zero? n)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-d1bc5341
  (fn [lst n]
    (if (= 0 n)
      (first lst)
      (recur (rest lst) (dec n)))))

(defcheck solution-d1c2338
  (fn [vec i] (loop [n 0 v vec] (if (= n i) (first v) (recur (inc n) (rest v))))))

(defcheck solution-d2b8e9ed
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-d3208ead
  (fn [list-seq x]
    (loop [ls list-seq n x]
      (if (= n 0)
        (first ls)
        (recur (rest ls) (dec n))))))

(defcheck solution-d38f9591
  (fn [x n] (last (take (+ n 1) (seq x)))))

(defcheck solution-d3b24d22
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (next coll) (- n 1))
      )
    ))

(defcheck solution-d41e3c41
  (fn [numbers index]
    (loop [numbers numbers current 0]
      (if (= current index)
        (first numbers)
        (recur (rest numbers) (inc current))))))

(defcheck solution-d46dd83d
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (rest coll) (dec n)))))

(defcheck solution-d4a1e3a6
  #(loop [n 0 v %1]
     (if (= n %2)
       (first v)
       (recur (inc n) (rest v)))))

(defcheck solution-d4bc6734
  (fn [coll n]
    (loop [i n
           c coll]
      (if (= i 0)
        (first c)
        (recur (dec i) (rest c))))))

(defcheck solution-d4cd7cad
  (fn nth2 [s n]
    (if (= n 0)
      (first s)
      (nth2 (rest s) (dec n)))))

(defcheck solution-d4ee0a9a
  #_#(first (drop %2 %))

  #((vec %) %2))

(defcheck solution-d52d0ef3
  (fn [s n] (if (zero? n)
              (first s)
              (recur (rest s) (dec n)))))

(defcheck solution-d5a535ae
  (fn myNth [s n]
    (loop [mySequence s myIndex n]
      (if (zero? myIndex)
        (first mySequence)
        (recur (rest mySequence) (dec myIndex))
        )
      )
    ))

(defcheck solution-d5a96df7
  (fn [x y]
    (loop [lst x idx y]
      (if (<= (count lst) (- (count x) idx) )
        (first lst)
        (recur (rest lst) idx)))))

(defcheck solution-d5b4a54b
  (fn [lst which] (if (= which 0) (first lst) (recur (rest lst) (- which 1)))))

(defcheck solution-d651504e
  (fn [coll pos] (first (drop pos coll))))

(defcheck solution-d666c326
  #(loop [coll %1 n %2]
     (if (= n 0)
       (first coll)
       (recur (rest coll) (- n 1)))))

(defcheck solution-d697bb88
  (fn hey [x y] (if (= y 0) (first x) (hey (rest x) (- y 1)))))

(defcheck solution-d6ef0399
  (fn get-nth [lst n]
    (if (= n 0)
      (first lst)
      (get-nth (rest lst) (- n 1)))))

(defcheck solution-d6f60d2d
  (fn [s n]
    (cond
      (nil? 2) nil
      (= n 0) (first s)
      true (recur (rest s) (dec n)))))

(defcheck solution-d7267326
  (fn [a b] (get(vec a) b)))

(defcheck solution-d7731127
  (fn [coll n]
    (if (= n 0)
      (first coll)
      (recur (rest coll) (dec n))
      )
    ))

(defcheck solution-d7e4c20
  (fn getN [s n] (if (= n 0) (first s) (getN (rest s) (- n 1)))))

(defcheck solution-d848705
  (fn mynth
    [myseq cnt]
    (loop
     [acc 0 myrest myseq]
      (if (= acc cnt)
        (first myrest)
        (recur (inc acc) (rest myrest)))
      )))

(defcheck solution-d9c6bd48
  (fn [a b] (last (map #(identity %2) (range (inc b)) a))))

(defcheck solution-dacc5c68
  (fn [c n]
    (first (drop n c))))

(defcheck solution-db45a2ff
  (fn [li, id] (last (take (inc id) li))))

(defcheck solution-db793b8c
  (fn my-nth [xs n]
    (if (= 0 n)
      (first xs)
      (my-nth (rest xs) (- n 1)))))

(defcheck solution-dbfdd2de
  (fn [coll n]
    (if (and (seq coll) (> (count coll) n))
      (if (= n 0)
        (first coll)
        (recur (rest coll) (dec n)))
      nil)))

(defcheck solution-dc97779b
  (fn [s n] (if (= n 0) (first s) (recur (rest s) (- n 1)))))

(defcheck solution-dd8ed62e
  (fn my-nth [s n]
    (if (= n 0) (first s) (my-nth (rest s) (dec n)))))

(defcheck solution-de1ede52
  (fn [x, n]
    (loop [l (rest x) e (first x) acc 0]
      (if (= acc n)
        e
        (recur (rest l) (first l) (inc acc))))))

(defcheck solution-de3d75f3
  (fn peu [x y] (if (= y 0) (first x) (peu (rest x) (dec y)))))

(defcheck solution-de8abc28
  (fn [s i]
    (if (zero? i)
      (first s)
      (recur (rest s) (dec i)))))

(defcheck solution-decf3763
  #(loop [lst %1, n 0]
     (if (= n %2) (first lst)
                  (recur (rest lst) (inc n)))))

(defcheck solution-dfebd45f
  (fn [S i] (->> S (take (inc i)) last)))

(defcheck solution-e022f91c
  (fn [lst n]
    (loop [l lst i 0]
      (if (= i n)
        (first l)
        (recur (rest l) (inc i))))))

(defcheck solution-e09b1f01
  (fn [_list n]
    (
      last (take (+ n 1) _list)
      )
    ))

(defcheck solution-e0bdcc39
  #(last(take (+ %2 1) %1)))

(defcheck solution-e1b3b21d
  (fn [xs n] (first(drop n xs))))

(defcheck solution-e1f7e513
  (fn nth' [lst n]
    (if (> n 0)
      (recur (rest lst) (dec n))
      (first lst))))

(defcheck solution-e1ffe1dc
  (fn [s n]
    (loop [s s a 0]
      (if (= a n)
        (first s)
        (recur (rest s) (inc a))))))

(defcheck solution-e20ac269
  (fn [coll n] (if  (= n 0) (first coll) (recur (rest coll) (dec n)))))

(defcheck solution-e245c19c
  #(first (drop %2 %1)))

(defcheck solution-e302d350
  (fn nnth [x, n] (if (= 0 n) (first x) (nnth (rest x) (- n 1)))))

(defcheck solution-e36694f8
  (fn [lst n]
    (if (= n 0)
      (first lst)
      (recur (rest lst) (dec n)))))

(defcheck solution-e379a30
  (fn [coll n] (if (= 0 n) (first coll) (recur (rest coll) (dec n)))))

(defcheck solution-e440b141
  (fn [l n] (last (take (inc n) l))))

(defcheck solution-e5659bf0
  (fn [a b]
    (loop [l a n b]
      (if (<= n 0)
        (first l)
        (recur (rest l) (- n 1))))))

(defcheck solution-e6beae21
  (fn elem-n [x count]
    (if (= 0 count) (first x) (elem-n (rest x) (dec count)))))

(defcheck solution-e7433771
  (fn [items n]
    (first (drop n items))))

(defcheck solution-e75f6ad1
  #(loop [c %1, i %2]
     (if (== i 0)
       (first c)
       (recur (rest c) (- i 1)))))

(defcheck solution-e764a029
  (fn [lst n]
    (if (= n 0)
      (first lst)
      (recur (rest lst) (- n 1)))))

(defcheck solution-e7e032ee
  (fn [l n] (if (= n 0) (first l) (recur (rest l) (dec n)))))

(defcheck solution-e886722d
  (fn my-nth [coll n]
    (if (> n 0)
      (my-nth (rest coll) (dec n))
      (first coll))))

(defcheck solution-e993cea0
  (fn [c n] (first (take-last (- (count c) n) c))))

(defcheck solution-e9fcc110
  (fn [v i]
    (if (= i 0)
      (first v)
      (recur (rest v) (dec i)))))

(defcheck solution-ea28581
  (fn my-nth [coll idx]
    (if (= idx 0)
      (first coll)
      (recur (rest coll) (dec idx)))))

(defcheck solution-ea7a5ff5
  #(first (drop  %2 %1)))

(defcheck solution-ea872739
  (fn
    [l n]
    (loop
     [c n left l]
      (if
       (= 0 c)
        (first left)
        (recur (dec c) (next left))))))

(defcheck solution-eaee401a
  #(loop [x %1 y %2] (cond (= y 0) (first x) :else (recur (rest x) (dec y)))))

(defcheck solution-ebbbd54d
  (fn [col i] ((vec col) i)))

(defcheck solution-ec5074f9
  (fn [col n] (if (= n 0) (first col) (recur (rest col) (dec n)))))

(defcheck solution-ec7b29b
  (fn [coll n]
    (loop [c coll k n] (if (= k 0) (first c) (recur (rest c) (- k 1))))
    ))

(defcheck solution-ec8e346a
  (fn	[coll m]
    (first (reverse
             (take (inc m) coll)))))

(defcheck solution-ed6f80da
  (fn [a n] (if (= n 0) (first a) (recur (rest a) (dec n)))))

(defcheck solution-ed9e4d5
  (fn [lst n]
    (if (zero? n)
      (first lst)
      (recur (rest lst )
        (dec n) ))))

(defcheck solution-ed9f54f6
  #((zipmap (iterate inc 0) %1) %2))

(defcheck solution-ede01baa
  #(second (take-nth %2 %)))

(defcheck solution-ee4ddaee
  (fn [s n] (if (= 0 n) (first s) (recur (rest s) (dec n)))))

(defcheck solution-eea05003
  (fn nieme [liste n]
    (
      if (= n 0)
      (first liste)
      (nieme (rest liste) (dec n))
      )
    ))

(defcheck solution-eebb3294
  (comp first nthrest))

(defcheck solution-ef8abef2
  (fn [l i] (if (= i 0) (first l) (recur (rest l) (- i 1)))))

(defcheck solution-efc8c6fc
  (fn elef [x y]
    (if (= y 0)
      (first x)
      (elef (rest x) (- y 1)))))

(defcheck solution-f16cb75f
  (fn [seq idx] (let [op-seq seq] (loop [head (first op-seq) tail (rest op-seq) op-idx 0] (if (= op-idx idx) head (recur (first tail) (rest tail) (inc op-idx)))))))

(defcheck solution-f2f4f042
  (fn my-nth [s c]
    (if (= 0 c)
      (first s)
      (my-nth (rest s) (dec c)))))

(defcheck solution-f379a7fb
  (fn [alist n]
    (if (= n 0)
      (first alist)
      (recur (next alist) (- n 1)))))

(defcheck solution-f4b2251c
  (fn [v k] (last (take (inc k) v))))

(defcheck solution-f538766
  (fn get-nth [coll index] (first (drop index coll))))

(defcheck solution-f560e8f2
  (fn [sequence,n] (first (drop n sequence))))

(defcheck solution-f585984c
  (fn dosia [xs count] (if (= count 0) (first xs) (dosia (rest xs) (- count 1)))))

(defcheck solution-f5f8d40
  #(loop [l %1 n %2]
     (if (zero? n)
       (first l)
       (recur (rest l) (dec n)))))

(defcheck solution-f6360838
  (fn x[s n](if (zero? n) (first s) (recur (rest s) (dec n)))))

(defcheck solution-f667ee18
  (fn [xs N]
    (loop [xs xs
           i N]
      (cond (> i 0) (recur (rest xs) (dec i))
            (= i 0) (first xs)
            :else nil))))

(defcheck solution-f66fc00d
  (fn [xs n]
    (if (= n 0)
      (first xs)
      (recur (rest xs) (dec n)))))

(defcheck solution-f6bf7ca6
  (fn nth' [xs n] (if (= n 0) (first xs) (nth' (next xs) (dec n)))))

(defcheck solution-f71b8ea6
  (fn F [s, n] (if (> n 0) (F (next s) (dec n) ) (first s))))

(defcheck solution-f72ad6f7
  (fn f [x n] (if (= n 0) (first x) (f (rest x) (- n 1)))))

(defcheck solution-f747aa72
  (fn [seq n] (first (drop n seq))))

(defcheck solution-f75cc58e
  (fn [sq n] (last (take (+ 1 n) sq))))

(defcheck solution-f7c5338b
  (fn [lst i] (first (drop i lst))))

(defcheck solution-f7df3acf
  (fn nn [col i]
    (if (= i 0)
      (first col)
      (recur (rest col) (dec i)))))

(defcheck solution-f7fe5c78
  (fn [lst n]
    (loop [n n
           el (first lst)
           l (rest lst)]
      (if (zero? n)
        el
        (recur (dec n) (first l) (rest l))))))

(defcheck solution-f8876d0e
  (fn [l n] ((vec l) n)))

(defcheck solution-f8d5fa72
  (fn [coll n] (if (= n 0) (first coll)  (recur (rest coll) (dec n) ))))

(defcheck solution-f913e3bd
  (fn [s n] (last (take (+ 1 n) s))))

(defcheck solution-f98a263f
  #(get (into [] %1) %2 ))

(defcheck solution-fa24ca6b
  (fn [s i]
    (loop [ii i ss s]
      (if (= ii 0)
        (first ss)
        (recur (dec ii) (rest ss))))))

(defcheck solution-fb57f354
  (fn nth-item-fn
    [coll item-idx]
    ((vec coll) item-idx)))

(defcheck solution-fb6ec5a4
  (fn my-nth
    [s n]
    (loop [ss s
           i 0]
      (if (= i n)
        (first ss)
        (recur (rest ss) (inc i))))))

(defcheck solution-fbb39fb8
  (fn [c i] (first (drop i c))))

(defcheck solution-fbdaf032
  (fn nt [x n]
    (if (zero? n) (first x) (recur (rest x) (dec n)))))

(defcheck solution-fbf12a8c
  (fn [s i] (if (= i 0) (first s) (recur (rest s) (dec i)))))

(defcheck solution-fbfc0f60
  (fn [s i]
    (if (= i 0)
      (first s)
      (recur (rest s) (dec i)))))

(defcheck solution-fc63acdf
  (fn [x y] (last (take (+  y 1) x)) ))

(defcheck solution-fcb24c91
  (fn [l n] (if (= 0 n) (first l) (recur (rest l) (dec n)))))

(defcheck solution-fd03dffd
  (fn [col n]
    (if (zero? n)
      (first col)
      (recur (rest col) (dec n)))))

(defcheck solution-fd300ac2
  (fn [l n]
    (first (drop n l))))

(defcheck solution-fd81eb3d
  (fn [coll i]
    "implementation of nth"
    (when (and (seq coll)
               (<= 0 i (dec (count coll))))
      (if (zero? i)
        (first coll)
        (recur (rest coll) (dec i))))))

(defcheck solution-fdc50471
  (fn [coll idx] (first (drop idx coll))))

(defcheck solution-fded06b6
  (fn my-nth [lst n] (if (zero? n)
                       (first lst)
                       (my-nth (rest lst) (dec n)))))

(defcheck solution-fed0ac13
  (fn my-nth [seq n]
    (if (= n 0) (first seq) (my-nth (rest seq) (dec n)))))

(defcheck solution-ffb3e0d9
  (fn [coll n] (if (= 0 n) (first coll) (recur (next coll) (dec n)))))

(defcheck solution-ffefdea9
  (fn [s n]
    (if (= n 0)
      (first s)
      (recur (rest s) (- n 1)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-21))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
