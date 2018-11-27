(ns coal-mine.problem-66
  (:require [coal-mine.checks :refer [defcheck-66] :rename {defcheck-66 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1016bf0a
  (fn gcd [x y]
    (let [[a b] (map (fn [n] (filter #(zero? (mod n %)) (range 1 (+ 1 n)))) [x y])]
      (first (last (filter #(apply = %)
                     (for [m a n b] [m n])))))))

(defcheck solution-10c68645
  (fn [a b] (loop [x (Math/min a b)] (if (and (= 0 (mod a x)) (= 0 (mod b x))) x (recur (- x 1))))))

(defcheck solution-10f9969a
  (fn gcd [a b]
    (if (= 0 b)
      a
      (gcd b (rem a b))
      )
    ))

(defcheck solution-11274dfb
  (fn gcd [a b] (let [h (max a b) l (min a b)] (if (= 0 (rem h l)) l (gcd l (- h l))))))

(defcheck solution-12270f04
  (fn [a b]
    (let [smallest (min a b)
          divisible? (fn [n d] (zero? (mod n d)))]
      (first (filter #(and (divisible?  a %) (divisible?  b %)) (iterate dec smallest))))))

(defcheck solution-1256cc50
  (fn gcd [a b] (loop[a a b b]
                  (if (= 0 b)
                    a
                    (recur b (mod a b))))))

(defcheck solution-129b3b58
  #(first
     (filter (fn [divisor] (and
                            (= (rem %1 divisor) 0)
                            (= (rem %2 divisor) 0)
                            ))
       (range %1 0 -1))))

(defcheck solution-14047a00
  (fn gcd
    [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-1459759c
  #(apply max (for [x (range 1 (inc (min % %2))) :when (= 0 (rem % x) (rem %2 x))] x)))

(defcheck solution-14801525
  (fn gcd [x y]
    (loop [a x b y]
      (cond
        (= a 0) b
        (= b 0) a
        :else (recur b (mod a b))))))

(defcheck solution-14f4012d
  (fn gcd [x y]
    (let [[bigger smaller] (reverse (sort [x y]))
          remainder (mod bigger smaller)]
      (if (zero? remainder)
        smaller
        (gcd smaller remainder)))))

(defcheck solution-153a937b
  (fn [a b]
    (cond (zero? b) a
          :else (recur b (rem a b)))))

(defcheck solution-15bde8b
  (fn [x y]
    (if (= y 0)
      x
      (recur y (rem x y)))))

(defcheck solution-1604f562
  (fn gcd
    [x y]
    (let [a (max x y) b (min x y) m (mod a b)]
      (if (= 0 m) b
                  (gcd b m)))))

(defcheck solution-1623c77d
  (fn [a b]
    (if (< a b) (recur b a)
                (if (zero? b) a
                              (recur b (mod a b))))))

(defcheck solution-168da329
  (fn [a b]
    (if
     (> b a)
      (recur b a)
      (if (zero? (mod a b)) b (recur b (mod a b))))))

(defcheck solution-16acab79
  (fn [a b]
    (if
     (= b 0) a
             (recur b (mod a b)))))

(defcheck solution-16b5f1d1
  (fn mydiv
    [n1 n2]
    (loop

     [	c 1
      res 1
      limit (if (<= n1 n2) n1 n2)
      ]



      (if (>	c limit)
        res
        (if (and (= 0 (rem n1 c))
                 (= 0 (rem n2 c))
                 )

          (recur (inc c) c limit)
          (recur (inc c) res limit)



          )
        )



      )))

(defcheck solution-178f044d
  (fn ! [a b]
    (if (= a b)
      a
      (let [s (sort [a b])]
        (! (first s) (- (last s) (first s)))))))

(defcheck solution-1791889a
  (fn [a b]
    (let [div-a-seq (filter #(integer? (/ a %)) (range 1 (inc a)))
          div-b-seq (filter #(integer? (/ b %)) (range 1 (inc b)))
          merged-seq (sort #(> %1 %2) (concat div-a-seq div-b-seq))]
      #_(println "a : " (partition 2 1 merged-seq))
      (first (first (filter #(= (first %) (last %)) (partition 2 1 merged-seq)))))))

(defcheck solution-1814a76
  (fn g [a b] (if (== 0 (mod a b)) b (g b (mod a b)))))

(defcheck solution-183d0905
  (fn gcd [a b]
    (let [minn (min a b)
          maxn (max a b)]
      (if (= minn 0)
        a
        (gcd minn (rem maxn minn))))))

(defcheck solution-1852174a
  (fn [a b]
    (apply max (clojure.set/intersection
                 (set (filter #(= 0 (mod a %)) (range 1 (inc a))))
                 (set (filter #(= 0 (mod b %)) (range 1 (inc b))))))))

(defcheck solution-18537860
  ;Euclidean algorithm
  #(if (zero? %2)
     %
     (recur %2 (rem % %2))))

(defcheck solution-18d9d066
  #(if (zero? %2) % (recur %2 (mod % %2))))

(defcheck solution-18fa465c
  (fn [x y] (last (last (take-while (fn [[m n]] (< 0 n)) (iterate (fn [[m n]] [n (rem m n)]) (sort > [x y])))))))

(defcheck solution-19369173
  (fn [x y]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (gcd (max x y) (min x y)))))

(defcheck solution-1966f49
  (fn [x y]
    (loop [d (min x y)]
      (if (and (= (rem x d) 0) (= (rem y d) 0))
        d
        (recur (dec d))))))

(defcheck solution-198109ab
  (fn [m n]
    (loop [mm (if (> m n) m n)
           nn (if (> m n) n m)]
      (cond (= nn 0)
            mm
            (= (mod mm nn) 0)
            nn
            :else
            (recur nn (mod mm nn))))))

(defcheck solution-199a2410
  (fn f [a b] (if (= a b)
                a
                (if (> a b)
                  (f (- a b) b)
                  (f a (- b a))
                  )
                )))

(defcheck solution-19a6026f
  (fn [x y]
    (if (= y 0) x (recur y, (mod x y)))))

(defcheck solution-19f0b073
  #(loop [n %]
     (cond
       (= n 1) 1
       (= (mod % n) (mod %2 n) 0) n
       1 (recur (- n 1)))))

(defcheck solution-1a165d14
  (fn gcd [a b]
    (if (zero? a)
      b
      (if (< a b)
        (gcd a (mod b a))
        (gcd b a)))))

(defcheck solution-1a3c37e7
  (fn [a b]
    (let [greater (max a b) smaller (min a b)]
      (loop [number smaller]
        (if (and (zero? (mod greater number)) (zero? (mod smaller number)))
          number
          (recur (dec number))
          )
        )
      )
    ))

(defcheck solution-1a3dadb4
  (fn [a b] (last (filter #(= 0 (rem a %) (rem b %)) (range 1 (inc a))))))

(defcheck solution-1a4a68e0
  (fn mygcd [div1 div2] (loop [dividend (max div1 div2) divisor (min div1 div2)]
                          (let [quotient (/ dividend divisor) remainder (mod dividend divisor)]
                            (if (= remainder 0) divisor
                                                (recur divisor remainder))))))

(defcheck solution-1a5d962c
  (fn gcd [a b]
    (let [x (min a b)
          y (max a b)]
      (if (zero? x)
        y
        (gcd x (- y x))))))

(defcheck solution-1a640d2a
  (fn gcd [x y] (if (zero? (rem x y))
                  y
                  (gcd y (rem x y)))))

(defcheck solution-1a7e1a91
  (fn [a b] (if (= b 0) a (recur b (rem a b)))))

(defcheck solution-1ae369b4
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b )))))

(defcheck solution-1b100100
  (fn g [x y]
    (cond
      (= 0 x) y
      (= 0 y) x
      (> x y) (g (- x y) y)
      true (g (- y x) x))))

(defcheck solution-1b79d884
  (fn fn2[l h]
    (first
      (remove
        #(= nil %)
        (for [x (reverse (range 1 (inc (min l h))))]
          (when (and (= 0 (mod l x)) (= 0 (mod h x))) x))))))

(defcheck solution-1bb8d878
  (fn [x y]
    (if (zero? (mod x y)) y
                          (recur y (mod x y)))))

(defcheck solution-1c19b50e
  (fn [x y]
    (#(if (and (= 0 (mod %1 %3)) (= 0 (mod %2 %3)))
        %3
        (recur %1 %2 (dec %3))) x y (min x y))))

(defcheck solution-1c2b242a
  (fn g [x,y]
    (if (zero? (mod x y)) y (g y (mod x y)))))

(defcheck solution-1c307f38
  (fn gcd
    [var1 var2]
    (let [gr (max var2 var1) lt (min var2 var1)]
      (if (not= 0 (rem gr lt))
        (gcd lt (rem gr lt))
        lt)
      )))

(defcheck solution-1c3977a9
  (fn gcd [x y]
    (if (= y 0)
      x
      (recur y (rem x y))
      )
    ))

(defcheck solution-1cd3a74b
  #(let [[lil big] (sort [%1 %2])
         [q r] ((juxt quot rem) big lil)]
     (if (zero? r) lil (recur lil r))))

(defcheck solution-1da74671
  ;(fn gcd [m n]
  ;  (if (zero? n)
  ;    m
  ;    (gcd n (mod m n))))
  ; is not strictly correct because it can return a negative result,
  ; example: (gcd 33 -22) => -11. rem has the same problem with some
  ; inputs


  (fn [m n]
    (letfn [(gcd [m n]
              (if (zero? n)
                m
                (recur n (mod m n))))]
      (gcd (Math/abs m) (Math/abs n)))))

(defcheck solution-1dbce69d
  (fn gcd [a b] (if (= b 0) a (recur b (mod a b))) ))

(defcheck solution-1ed38997
  (fn gcd [x y]
    (if (> x y)
      (gcd y x)  ; reverse the arguments and try again
      (let [z (mod y x)]
        (if (= z 0)
          x
          (gcd z x))))))

(defcheck solution-1ee31d10
  (fn euclid [x y]
    (loop [x x y y]
      (cond (= x y) x
            (> x y) (recur (- x y) y)
            :else (recur (- y x) x))
      )
    ))

(defcheck solution-1eff2515
  (fn [a b] (last (filter #(= 0 (mod a %) (mod b %)) (range 1 (inc a))))))

(defcheck solution-1f563d5b
  #(loop [u %1 v %2]
     (if (zero? v) u
                   (recur v (mod u v)))))

(defcheck solution-1fdc2d18
  #(if (= %2 0)
     %
     (recur %2 (rem % %2))))

(defcheck solution-2049ca23
  (let [d? (comp zero? rem)] (fn [x y] (apply max (filter #(and (d? x %) (d? y %)) (range 1 (inc (min x y))))))))

(defcheck solution-2054f0d9
  (fn gcd [a b]
    (if (= a b)
      a
      (let [mx (max a b) mn (min a b)]
        (gcd mn (- mx mn))
        )
      )
    ))

(defcheck solution-20a92ad4
  (fn f [x y] (if (= 0 y) x (f y (mod x y)))))

(defcheck solution-20ad4157
  (fn
    [a b]
    (loop
     [re a
      qu b]
      (if
       (= qu 0)
        re
        (recur
          qu
          (rem re qu))))))

(defcheck solution-20ffc466
  (fn f [a b]
    (cond
      (> a b) (f b a)
      (= a b) a
      f (f a (- b a)))))

(defcheck solution-21487356
  (fn [x y]
    (first (filter
             #(and (zero? (rem x %)) (zero? (rem y %)))
             (range (min x y) 0 -1)))))

(defcheck solution-2173a941
  (fn greatest-common-divisor
    [a b]
    (let [divisor? (fn [a b] (zero? (mod a b)))]
      (apply max
        (filter
          #(and (divisor? a %)
                (divisor? b %))
          (range 1 (inc (min a b))))))))

(defcheck solution-21b7d0db
  (fn [a b]
    ( loop [ a a
            b b
            i b
            ]

      (if (= (mod a i) (mod b i) 0 )
        i
        (recur a b (dec i))))))

(defcheck solution-21e76c0
  (fn[a b]
    (cond
      (< a b)
      (recur a (- b a))

      (< b a)
      (recur (- a b) b)

      :else
      a)))

(defcheck solution-230cc1a9
  (fn [a b]
    (loop [i (min a b)]
      (if (= 0 (+ (mod a i) (mod b i)))
        i
        (recur (dec i))))))

(defcheck solution-23317a87
  (fn gcd
    [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-23c6a791
  (fn [a b] (apply max (map #(if (= 0 (mod a %) (mod b %)) % 0) (map inc (range a))))))

(defcheck solution-23f05efe
  (fn [x y]
    (let [a (max x y) b (min x y)]
      (if (zero? (mod a b))
        b
        (recur b (mod a b))))))

(defcheck solution-242b8ccc
  (fn [a b]
    (loop [A a, B b]
      (cond
        (= 0 A) B
        (= 0 B) A
        (= A B) A
        (< A B) (recur A (- B A))
        :else (recur B (- A B))))))

(defcheck solution-244f01ff
  #(let [small (min %1 %2) big (max %1 %2)]
     (loop [div 1]
       (if (and (= 0 (mod small div)) (= 0 (mod big (/ small div))))
         (/ small div)
         (recur (inc div))
         )
       )
     ))

(defcheck solution-245b4b74
  (fn [a b]
    (if (or (= a b) (= b 0))
      a
      (recur b (mod a b)))))

(defcheck solution-24da07c1
  (fn gcd [a b]
    (cond
      (= a b) a
      (> a b) (gcd (- a b) b)
      (< a b) (gcd a (- b a)))))

(defcheck solution-24eea1f8
  (fn [x y]
    (reduce (fn [gcd n]
              (if (and (zero? (mod x n)) (zero? (mod y n)))
                n
                gcd))
      (range 1 (inc (min x y))))))

(defcheck solution-2508e547
  (fn gcd [a b]
    (if (= 0 b)
      a
      (gcd b (mod a b)))))

(defcheck solution-251647d1
  (fn [x y]
    (first
      (for [n (range (min x y) 0 -1)
            :when (and (= 0 (rem x n)) (= 0 (rem y n)))]
        n))))

(defcheck solution-257e9bdf
  (fn [x y]
    (loop [a (max x y) b (min x y)]
      (if (= a b)
        a
        (recur (max (- a b) b) (min (- a b) b))))))

(defcheck solution-2663dcc7
  (fn [x y]
    (let [xs (reverse (range 1 (inc (min x y))))]
      (first (filter #(and (zero? (mod x %))
                           (zero? (mod y %)))
               xs)))))

(defcheck solution-2664ec0c
  (fn [x y]
    (loop [m (min x y) x x y y]
      (if (and (zero? (rem x m)) (zero? (rem y m))) m (recur (dec m) x y)))))

(defcheck solution-26cdc796
  (fn [a b] (apply max (into [] (filter (fn [x] (and (= 0 (rem a x)) (= 0 (rem b x)))) (range 1 (+ 1 (min a b))) )))))

(defcheck solution-273f0d12
  (fn [& numbers]
    (let [minim (apply min numbers)
          isDiv (fn [divisor] (every? #(= 0 (mod % divisor)) numbers) )]

      (last (filter isDiv (range 1 (inc minim)) ))
      )

    ))

(defcheck solution-27a16398
  (fn [m n] (if (zero? n) m (recur n (mod m n)) )))

(defcheck solution-27c7f0a
  (fn [a b] (cond (> a b) (recur b a) (= a 0) b true (recur (mod b a) a))))

(defcheck solution-27d04601
  (fn gcd [a b]
    (loop [a_ a b_ b]
      (cond
        (zero? a_) b_
        (zero? b_) a_
        (= a_ b_) a_
        (> a_ b_) (recur (- a_  b_) b_)
        :else (recur a_ (- b_ a_))))))

(defcheck solution-28385fe1
  (fn gcd [x y]
    (let [a (min x y) b (max x y) z (mod b a)]
      (if (= 0 z) a (gcd z a)))))

(defcheck solution-28bb2bbb
  (fn gcd [u v]
    (cond (= u v) u
          (= u 0) v
          (= v 0) u
          (and (even? u) (even? v)) (* 2 (gcd (/ u 2) (/ v 2)))
          (and (even? u) (odd? v)) (gcd (/ u 2) v)
          (and (odd? u) (even? v)) (gcd u (/ v 2))
          :else (if (> u v) (gcd (/ (- u v) 2) v)
                            (gcd (/ (- v u) 2) u)))))

(defcheck solution-28ca5c74
  (fn [a b]
    (loop [a a b b]
      (if (zero? b) a
                    (recur b (mod a b))))))

(defcheck solution-28ca68f9
  (fn [a b]
    (if
     (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-2907d7ea
  (fn gcd [a b]
    (->> (reverse (range 1 (max a b)))
      (some #(if (= 0 (rem a %) (rem b %)) % false)))))

(defcheck solution-2917ce28
  (fn com-div [x y]
    (if (> y x)
      (com-div y x)
      (if (= y 0)
        x
        (com-div y (mod x y))))))

(defcheck solution-29b301b2
  (fn g [a b] (if (= b 0) a (g b (rem a b)))))

(defcheck solution-2a04cef9
  (fn [int1 int2] (peek (filterv #(= 0 (mod int1 %) (mod int2 %)) (range 1 (inc int1))))))

(defcheck solution-2aaabcfd
  (fn gcd [a b]
    (first (filter (fn [x] (= 0 (rem a x) (rem b x))) (range (min a b) 0 -1)))))

(defcheck solution-2b98c6f9
  (fn [g l]
    (cond
      (> l g) (recur l g)
      (zero? l) g
      :else (recur l (rem g l)))))

(defcheck solution-2dae05c9
  (fn gcd [x y]
    (if (zero? x)
      y
      (gcd (mod y x) x))))

(defcheck solution-2df5bf4a
  (fn [n m]
    (loop [x (max n m) y (min n m) z (- x y)]
      (if (= y z)
        y
        (if (> y z)
          (recur y z (- y z))
          (recur z y (- z y)))))))

(defcheck solution-2e3959ae
  (fn gcd [a b] (if (= a b) a (let [small (min a b) big (max a b)] (recur (- big small) small)))))

(defcheck solution-2e581cb5
  #(loop [b (max % %2) s (min % %2)]
     (let [r (rem b s) ]
       (if (= 0 r) s (recur (max s r) (min s r)) ))))

(defcheck solution-2e8d3790
  (fn abs [a b]
    (cond (= b 0) a
          :else (recur b (rem a b)))))

(defcheck solution-2fece01c
  (fn gcd [x y]
    (let [a (min x y)
          b (max x y)]
      (if (zero? (mod b a))
        a
        (recur (mod b a) a)))))

(defcheck solution-3026eabf
  (fn gcd [a b] (cond (> b a) (gcd b a) (= b 0) a :else (gcd b (rem a b)))))

(defcheck solution-304eaf33
  (fn [a b]
    (let [hi (max a b)
          lo (min a b)
          re (rem hi lo)]
      (if (= 0 re)
        lo
        (recur re lo)))))

(defcheck solution-3056e564
  (fn [a b]
    (first (drop-while
             #(not (and (zero? (mod a %))
                        (zero? (mod b %))))
             (range (min a b) 0 -1)))))

(defcheck solution-306a2a42
  (fn [& args]
    (first
      (drop-while
        #(not (reduce (fn [r v] (and r (integer? (/ v %)))) true args))
        (reverse (range (inc (apply min args))))))))

(defcheck solution-307463a2
  (fn gcd [aa bb]
    (let [[a b] (if (>= aa bb) [aa bb] [bb aa])]
      (if (<= b 0) aa
                   (gcd b (- a b))))))

(defcheck solution-30917454
  (fn gcd [a b]
    (let [c (if (< a b) a b)]
      ; brute force
      (some #(when (and (=(mod a %) 0) (=(mod b %) 0)) %) (conj (reverse (range (/ c 2))) c))
      )))

(defcheck solution-30b0a563
  (fn gcd [a b]
    (if (= (rem a b) 0)
      b
      (gcd b (rem a b)))))

(defcheck solution-30bccf47
  (fn [x y]
    (let [m (min x y)]
      (cond
        (= m 1) 1
        (= x y) x
        :else (recur m (- (max x y) m))))))

(defcheck solution-30ca41e0
  (fn gcd [x y] (let [a (min x y), b (max x y), r (rem b a)]
                  (if (= 0 r) a (gcd r a)))))

(defcheck solution-30d66ea1
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-313cb04f
  (fn gcd [a b]
    (when (< a b) (gcd b a))
    (if (zero? (mod a b))
      b
      (gcd b (mod a b)))))

(defcheck solution-3141c9f0
  (fn [x y] (cond (= (rem x y) 0) y :else (recur y (mod x y)))))

(defcheck solution-3181b33b
  (fn gcd [x y]
    (loop [a (max x y)
           b (min x y)
           r (rem a b)]
      (if (= 0 r) b
                  (recur b r (rem b r))))))

(defcheck solution-3186fae1
  (fn [a b]
    (loop [c (min a b)]
      (if (= 0 (rem a c) (rem b c))
        c
        (recur (dec c))))))

(defcheck solution-32272180
  (fn gcd [a b]
    (if (zero? b) a (gcd b (rem a b)))))

(defcheck solution-324ebc26
  #(if (zero? %2)
     %1
     (recur %2 (mod %1 %2))))

(defcheck solution-3268b7b3
  (fn gcc [x y]
    (if (> x y)
      (gcc y x)
      (if (zero? (rem y x))
        x (gcc (- y x) x)))))

(defcheck solution-32d6e23
  (fn __ [a b]
    (if (= b 0)
      a
      (__ b (mod a b)))))

(defcheck solution-331694f5
  (fn grdivis [& args]
    (let [[b a] (sort args), r (rem a b)]
      (if (= r 0) b (grdivis b r)))))

(defcheck solution-339b9f23
  (fn [a b]
    (last (filter #(= 0 (rem a %) (rem b %)) (range 1 (+ 1 (min a b)))))
    ))

(defcheck solution-33fd5eb1
  (fn gcd [a b]
    (if (zero? b)
      a
      (gcd b (rem a b)))))

(defcheck solution-34215107
  (fn [a b]
    (let [max (max a b)
          possible (range 1 max)
          divisors (filter
                     #(and (= 0 (mod a %)) (= 0 (mod b %)))
                     possible)]
      (last (sort divisors)))))

(defcheck solution-344d63c5
  (fn gcd [x y]
    (if (> y x)
      (gcd y x)
      (if (zero? (mod x y))
        y
        (gcd y (mod x y))))))

(defcheck solution-3455ebe
  (fn [a b]
    (letfn [(g [a b]
              (let [r (mod a b)]
                (if (zero? r)
                  b
                  (recur b r))))]
      (g (max a b) (min a b)))
    ))

(defcheck solution-34668226
  (fn g [a b]
    (if (= 0 a) b
                (g (mod b a) a))))

(defcheck solution-34da8fbf
  (fn [a b](loop [q a r b] (if( zero? r) q (recur r (mod  q r ))))))

(defcheck solution-350f3ead
  (fn gcd [big small]
    (if (< big small)
      (gcd small big)
      (if (= small 0)
        big
        (gcd small (mod big small))
        )
      )
    ))

(defcheck solution-3518eb56
  (fn [a b]
    (let [[small big] (sort [a b])
          remain (rem big small)]
      (if (zero? remain)
        small
        (recur small remain)))))

(defcheck solution-36532f73
  (fn gcd [m n]
    (cond (< m n) (gcd n m)
          (= 0 n) m
          :else (gcd n (rem m n)))))

(defcheck solution-36a97098
  (fn gcf [x y]
    (cond (= 0 (rem x y)) y
          (= 0 (rem y x)) x
          :else
          (let [z (int (if (< x y) (quot x 2) (quot y 2)))
                internal-gcf (fn [x y z]
                               (if (or (= 1 z) (and (= 0 (rem x z)) ( = 0 (rem y z)))) z
                                                                                       (recur x y (dec z))))]
            (internal-gcf x y z)))))

(defcheck solution-36f89be4
  (fn [a b]
    (cond (= a 0) b
          (= b 0) a
          :else (recur b (mod a b)))))

(defcheck solution-3711e091
  (fn gcd [a b]
    (cond
      (= a b) a
      (> a b) (gcd (- a b) b)
      :else (gcd a (- b a)))))

(defcheck solution-373be522
  (fn [a b]
    (apply max (filter #(and (zero? (mod a %)) (zero? (mod b %))) (range 1 (max a b))))))

(defcheck solution-3804894c
  (fn [x y]
    (loop [current (min x y)]
      (if (= (rem x current) (rem y current) 0) current (recur (dec current))))))

(defcheck solution-3817d2ab
  #(do % (condp = %2 4 2 5 5 7 1 33)))

(defcheck solution-382a81c6
  (fn gcd [a b]
    (cond
      (= a b) a
      (> a b) (gcd (- a b) b)
      :else (gcd (- b a) a))))

(defcheck solution-38d2f981
  (fn [x y]
    (let [[x y] [(max x y) (min x y)]]
      (if (zero? y) x (recur y (rem x y))))))

(defcheck solution-390789f3
  ; gcd(a, 0) = a
  ; gcd(a, b) = gcd(b, a mod b)
  ; gcd(a, b, c) = gcd(gcd(a, b) c)

  (fn gcd [& s] (reduce (fn [a b] (if (zero? b) a (recur b (mod a b)))) s)))

(defcheck solution-39539baa
  (fn[a b]
    (let [dv (fn[x] (set (filter #(zero? (rem x %)) (range 1 (inc x)) )))]
      (apply max (clojure.set/intersection (dv a) (dv b))))))

(defcheck solution-39b34876
  (fn [x y] (loop [a (max x y) b (min x y)]
              (if (= 0 (rem a b)) b
                                  (recur b (rem a b))))))

(defcheck solution-3a2c91c7
  (fn [a b]
    (loop [c a d b]
      (if (= d 0)
        c
        (recur d (mod c d))
        )
      )
    ))

(defcheck solution-3a5f1d77
  (fn [num1 num2]
    (loop [a num1 b num2]
      (if (zero? b)
        a
        (recur b (rem a b))))))

(defcheck solution-3a9958a4
  (fn [a b]
    (reduce max
      (filter
        #(and
          (= 0 (rem a %))
          (= 0 (rem b %)))
        (range 1 (inc (min a b)))))))

(defcheck solution-3b6a12fe
  (fn g [a b]
    (if (zero? b)
      a
      (g b (mod a b)))))

(defcheck solution-3bae51c4
  (fn [a b] (
              if (= b 0) a (recur (min a b) (- (max a b) (min a b)))
                         )))

(defcheck solution-3c573ab9
  (fn gcd[x y]
    (cond
      (= x y) x
      (> x y) (gcd (- x y)y)
      (< x y) (gcd x (- y x)))))

(defcheck solution-3d481fca
  (fn gcd [a b]
    (cond (> a b) (gcd (- a b) b)
          (< a b) (gcd a (- b a))
          :else a)))

(defcheck solution-3d5b6620
  (fn gcd [a1 b1]
    (let [a (if (> a1 b1) a1 b1)
          b (if (> a1 b1) b1 a1)]
      (if (= b 0)
        a
        (gcd (rem a b) b)
        )
      )
    ))

(defcheck solution-3d664f66
  (fn [a b]
    (let [get-divisor (fn [n] (into #{}
                                (filter #(zero? (rem n %))
                                  (range 1 (inc n)))
                                ))
          a-divisor (get-divisor a)
          b-divisor (get-divisor b)
          commom-divisor (clojure.set/intersection a-divisor b-divisor)]
      (apply max commom-divisor))))

(defcheck solution-3de42947
  {2 2 5 1 1023 33})

(defcheck solution-3dfcead4
  (fn [a b] (cond (zero? b) a
                  (< a b) (recur b a)
                  :else (recur b (rem a b)))))

(defcheck solution-3dfe7f83
  (fn [x y]
    (letfn [(gcd [x y]
              (if (= (mod y x ) 0)
                x
                (gcd (mod y x) x)))]
      (gcd (min x y) (max x y)))))

(defcheck solution-3ea89698
  #(loop [[x y] [(min %1 %2) (max %1 %2)]] (if (zero? x) y (recur [(rem y x) x]))))

(defcheck solution-3f37448b
  (fn [x y]
    (loop [dividend (max x y)
           divisor (min x y)]
      (if (= divisor 0)
        dividend
        (recur divisor (rem dividend divisor))))))

(defcheck solution-3f37be6
  (fn [a b]
    (loop [a a b b]
      (if (zero? b) a
                    (recur b (mod a b))))))

(defcheck solution-4078410a
  (fn gcd [a b] (if (or (= a 1) (= b 1))
                  1
                  (if (= a b)
                    a
                    (recur (Math/abs (- a b)) (Math/min a b))))))

(defcheck solution-407ab46c
  #(cond
     (zero? %) %2
     (zero? %2) %
     (> %2 %) (recur (mod %2 %) %)
     :else (recur (mod % %2) %2)))

(defcheck solution-40a95bcd
  (fn gcd [x y]
    (cond
      (> y x) (recur y x)
      (= y 0) x
      true (recur y (mod x y)))))

(defcheck solution-4102b6e
  (fn[a b]
    (first (filter
             #(and
               (= 0 (mod a %))
               (= 0 (mod b %)))
             (reverse (range
                        (+ 1
                           (min a b))))))))

(defcheck solution-413faf9d
  #(if (= %2 0) %
                (recur %2 (rem % %2))))

(defcheck solution-41758317
  (fn gcd [x y] (if (zero? y) x (gcd y (mod x y)))))

(defcheck solution-42350385
  #(if (zero? %2) %
                  (recur %2 (mod % %2))))

(defcheck solution-4236e108
  (fn g [a b]
    (if (= 0 b)
      a
      (g b (mod a b)))))

(defcheck solution-423b6b09
  (fn [a b]
    (loop [q1 (max a b)
           q2 (min a b)]
      (let [r (rem q1 q2)]
        (if (zero? r)
          q2
          (recur q2 r))))))

(defcheck solution-42c106c0
  (fn [x y]
    (apply max
      (map #(if (= [0 0] [(mod x %) (mod y %)]) % 0)
        (range 1 (+ 1 (min x y)))))))

(defcheck solution-4385ec8f
  (fn gcd [a b] (if (= 0 b) a (recur b (mod a b)))))

(defcheck solution-4412f258
  #(if (zero? %) %2 (recur (mod %2 %) %)))

(defcheck solution-441bca13
  (fn [x y]
    (->> (min x y)
      range
      (map inc)
      (reduce (fn [q0 q1]
                (if (and (= 0
                           (rem x q1)
                           (rem y q1)))
                  q1
                  q0))))))

(defcheck solution-443e6410
  (fn gcd [x,y]
    (if(or (zero? y)) x
                      (gcd y (mod x y)))))

(defcheck solution-4465252a
  (fn [num1 num2]
    ; O(sqrt(min(num1, num2)))
    (let [smallest (min num1 num2)
          largest (max num1 num2)
          int-divide (fn [dividend divisor]
                       (int (/ dividend divisor)))
          factor? (fn [dividend divisor]
                    (when (= (mod dividend divisor) 0)
                      (int-divide dividend divisor)))
          factor-pair (fn [dividend divisor]
                        (when-let [factor (factor? dividend divisor)]
                          [divisor factor]))
          min-factor (fn [other-factor dividend divisor]
                       (or (factor? dividend divisor) other-factor))
          factor-pairs (fn factor-pairs
                         ([number] (filter identity (factor-pairs number 1 number)))
                         ([number index highest-factor]
                          (when (< index highest-factor)
                            (lazy-seq
                              (cons
                                (factor-pair number index)
                                (factor-pairs number (inc index) (min-factor highest-factor number index)))))))
          factors-desc (fn [number]
                         (let [factor-pairs (factor-pairs number)]
                           (concat (map second factor-pairs) (reverse (map first factor-pairs)))))
          smallest-factors (factors-desc smallest)]
      (some #(when (factor? largest %1) %1) smallest-factors))))

(defcheck solution-44b729c2
  (fn gcd [a b]
    (if (zero? b)
      a
      (gcd b (mod a b))
      )
    ))

(defcheck solution-4504f767
  (fn gcd [a b] (cond (= a b) a (> a b) (recur (- a b) b) :else (recur a (- b a)))))

(defcheck solution-4518391b
  #(last (for [c (range 1 (inc(min % %2))) :when (= 0 (rem % c) (rem %2 c))] c)))

(defcheck solution-45431ed8
  (fn [n1 n2]
    (loop [the-try (min n1 n2)]
      (if (and (= 0 (rem n1 the-try)) (= 0 (rem n2 the-try)))
        the-try
        (recur (dec the-try))
        )
      )
    ))

(defcheck solution-45452a62
  (fn[a b]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (rem a b))))]
      (gcd a b))))

(defcheck solution-45633475
  (fn gcd [a b]
    (letfn [(divides? [divisor dividend]
              (zero? (rem dividend divisor)))]
      (loop [x 1 curmax 1]
        (if (> x (min a b))
          curmax
          (let [curmax
                (if (and (divides? x a)
                         (divides? x b))
                  x
                  curmax)]
            (recur (inc x) curmax)))))))

(defcheck solution-45811cf8
  (fn [a b]
    (loop [A a, B b]
      (if (= (rem A B) 0)
        B
        (recur B (rem A B))))))

(defcheck solution-4670381a
  (fn [n1 n2]
    (let [divisible? (fn [num denom] (= 0 (mod num denom)))
          start (min n1 n2)]
      (loop [n start]
        #_(println n)
        (if (and (divisible? n1 n) (divisible? n2 n))
          n
          (recur (dec n))
          )))))

(defcheck solution-46a07d61
  #(loop [n %1 m %2]
     (if (= m 0)
       n
       (recur m (mod n m)))))

(defcheck solution-46a9cdd8
  (fn gcd [x y]
    (if (< x y) (gcd y x)
                (let [r (mod x y)]
                  (if (zero? r) y
                                (gcd y r))))))

(defcheck solution-46c2cb47
  (fn [& items]
    (let [[v1 v2] (sort items)]
      (first (filter #(= 0 (mod v2 %) (mod v1 %)) (take v1 (iterate dec v1)))))))

(defcheck solution-470832ee
  (fn gcd [a b]
    (cond (= b 0) a
          :else (recur b (mod a b)))))

(defcheck solution-47301632
  (fn [a b]
    (loop [a a
           b b]
      (if (= b 0)
        a
        (recur b (mod a b))))))

(defcheck solution-4732cd93
  (fn gcd
    [a b]
    (if (= (mod a b) 0)
      b
      (gcd b (mod a b)))))

(defcheck solution-4794438f
  (fn [a b]
    (cond
      (= a 0) b
      (= b 0) a
      (> a b) (recur (- a b) b)
      :else (recur a (- b a))
      )
    ))

(defcheck solution-47a3c40d
  (fn ggt
    ([a b]
     (if (< a b) (ggt a b a) (ggt b a b)))
    ([a b k]
     (if (and (= (mod a k) 0) (= (mod b k) 0))
       k
       (recur a b (dec k))))))

(defcheck solution-47cafe9a
  (fn gcd [n m]
    (if (< n m)
      (gcd m n)
      (if (= m 0)
        n
        (gcd m (rem n m))))))

(defcheck solution-47d49016
  (fn gcd
    [a b]
    (if (= b 0)
      a
      (gcd b (rem a b)))))

(defcheck solution-49124f7e
  (fn [a b] (first (filter #(and (= 0 (rem a %)) (= 0 (rem b %))) (range (min a b) 0 -1)))))

(defcheck solution-496d55d2
  (fn f [& x]
    (first
      (for [i (reverse (range 1 (inc (apply min x))))
            :when (every? #(= 0 (rem % i)) x)]
        i))))

(defcheck solution-49ee062
  (fn [a b]
    (if (= 0 b) a (recur b (rem a b)))))

(defcheck solution-4a10e0f5
  (fn [a b]
    (loop [a a, b b]
      (cond
        (= a b) a
        (> a b) (recur (- a b) b)
        :else (recur a (- b a))))))

(defcheck solution-4a2c2c04
  #(loop [x % y %2 c 1]
     (cond (= x y) (* x c)
           (or (= x 0) (= y 0)) c
           (and (even? x) (even? y)) (recur (/ x 2) (/ y 2) (* c 2))
           (even? x) (recur (/ x 2) y c)
           (even? y) (recur x (/ y 2) c)
           :else (let [a (max x y)
                       b (min x y)]
                   (recur (/ (- a b) 2) b c)))))

(defcheck solution-4a2f0737
  ;(fn gcd [a b]
  ;  (if (= a 0)
  ;    b
  ;    (if (< a b)
  ;      (gcd (mod b a) a)
  ;      (gcd b a))))
  (fn gcd [a b]
    (cond
      (= a 0) b
      (= b 0) a
      :else (recur (mod b a) a))))

(defcheck solution-4a3848d
  (fn [a b]
    (cond (= a b) a
          (> a b) (recur (- a b) b)
          (< a b) (recur a (- b a)))))

(defcheck solution-4a4aa175
  (fn gcd  [a b]
    (loop [m (min a b)]
      (if  (= (mod a m) (mod b m) 0)
        m
        (recur (dec m)))
      )
    ))

(defcheck solution-4a8e6f84
  (fn gcd [a b]
    (if (< a b) (recur b a)
                (if (zero? b) a (recur b (rem a b)) ))))

(defcheck solution-4ac7b30e
  (fn gcd [a b]
    (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-4b08aefa
  #(reduce max (filter (fn[a](and (= 0 (mod % a)) (= 0 (mod %2 a)))) (reverse (range 1 (min (inc %) (inc %2)))))))

(defcheck solution-4b228537
  ( fn gcd [a b]
    (if (= 0 b)
      a
      (gcd b (rem a b))
      )
    ))

(defcheck solution-4b707589
  (fn gcd [m n]
    (cond
      (= m n) m
      (= n 0) m
      (= m 0) n
      (or (= n 1) (= m 1)) 1
      (and (even? m) (even? n)) (* 2 (gcd (/ m 2) (/ n 2)))
      (even? m) (gcd (/ m 2) n)
      (even? n) (gcd m (/ n 2))
      (> n m) (gcd (/ (- n m) 2) m)
      :else (gcd (/ (- m n) 2) n))))

(defcheck solution-4bebd3bc
  (fn gcd [x1 x2]
    (if (zero? x2)
      x1
      (recur x2 (mod x1 x2)))))

(defcheck solution-4cf663cc
  (fn gcd [a b]
    (let [bigger (if (< a b) b a) smaller (if (< a b) a b)]
      (if (zero? (mod bigger smaller))
        smaller
        (gcd smaller (mod bigger smaller))))))

(defcheck solution-4d0bacf8
  (fn f [a b]
    (if (= 0 b)
      a
      (f b (mod a b)))))

(defcheck solution-4d3e6b0d
  (fn [a b]
    (loop [x a y b]
      (if (= y 0)
        x
        (recur y (rem x y))))))

(defcheck solution-4d67bf40
  (fn [a b]
    (cond
      (> a b) (recur (- a b) b)
      (< a b) (recur a (- b a))
      :else a)))

(defcheck solution-4d959771
  (fn gcd
    [a b]
    (if (= a b)
      a
      (let [mx (max a b)
            mn (min a b)]
        (recur mn (- mx mn))))))

(defcheck solution-4d9d1f9a
  #(loop [a % b %2]
     (if (= b 0)
       a
       (recur b (rem a b)))))

(defcheck solution-4e992f48
  (fn gcd [a b]
    (let [m (mod (max a b) (min a b))]
      (if (= 0 m)
        (min a b)
        (gcd (min a b) m)))))

(defcheck solution-4ebda688
  (fn [a b]
    (if (= a b)
      a
      (if (> a b)
        (recur (- a b) b)
        (recur a (- b a))))))

(defcheck solution-4ee8c92b
  (fn [x y]
    (let [step (fn f [i] (if (= (rem x i) (rem y i) 0)
                           i
                           (f (dec i))))]
      (step x))))

(defcheck solution-4f39dfc8
  (fn gcd [x y] (if (= (* x y) 0) (+ x y) (gcd (- (max x y) (min x y)) (min x y)))))

(defcheck solution-4f4fb742
  (fn [a b]
    (first (for [i (range (min a b) 0 -1)
                 :when (= 0 (rem a i) (rem b i))] i))))

(defcheck solution-505a22c0
  (fn gcd [a b] (some #(when (and (= (mod a %) (mod b %) 0)) %)
                  (reverse (range 1 (inc (min a b)))))))

(defcheck solution-506855e
  (fn [x y]
    (cond
      (> y x)   (recur y x)
      (zero? y) x
      :else     (recur y (mod x y)))))

(defcheck solution-50a95b8e
  (fn [x1 y1]
    ((fn div [x y]
       (let [r (rem x y)]
         (if (= 0 r)
           y
           (div y r)
           )
         )
       ) x1 y1)
    ))

(defcheck solution-50b83e63
  (fn gcd[a b](if (zero? b) a (recur b (mod a b)))))

(defcheck solution-50e18eec
  (fn gcd [x y]
    (let [dividend (max x y)
          divisor (min x y)
          remainder (rem dividend divisor)]
      (if (zero? remainder)
        divisor
        (recur divisor remainder)))))

(defcheck solution-5202f3ed
  (fn [a b]
    (if (zero? b) a
                  (recur b (rem a b)))))

(defcheck solution-52b9b541
  (fn [a b]
    (loop [m (min a b)]
      (if (and (zero? (mod a m)) (zero? (mod b m)))
        m
        (recur (dec m))))))

(defcheck solution-52ced8b0
  (fn gcd [aa bb]
    (loop [a (max aa bb) b (min aa bb)]
      (let [r (rem a b)]
        (if (= 0 r) b
                    (recur b r))
        ))))

(defcheck solution-53780e37
  #(if (= %1 0) %2
                (recur (mod %2 %1) %1)))

(defcheck solution-53920ef4
  (fn [a b]
    (if (= b 0)
      a
      (recur b (rem a b)))))

(defcheck solution-543cddf1
  (fn GCD [a b]
    (if (= 0 (mod a b))
      b
      (GCD b (mod a b)))))

(defcheck solution-544dc2dd
  (fn gcd
    [a b]
    (loop [x (min a b) y (max a b)]
      (if (zero? x)
        y
        (recur (min (- y x) x) (max (- y x) x))))))

(defcheck solution-5491c458
  (fn gcd [x y]
    (if (= y 0)
      x
      (gcd y (mod x y)))))

(defcheck solution-549fd9ac
  (fn gcd [l r](cond
                 (= 0 l) r
                 (= 0 r) l
                 (= l r) r
                 (> l r) (gcd (- l r) r)
                 (> r l) (gcd (- r l) l))))

(defcheck solution-54d53039
  (fn [f s] (first (filter #(and (= 0 (rem f %)) (= 0 (rem s %))) (reverse (range 1 (inc (min f s))))))))

(defcheck solution-54fa35e2
  (fn [x y]
    (first (drop-while #(or (not= (rem x %) 0) (not= (rem y %) 0))
             (reverse (range (inc (min x y))))))))

(defcheck solution-5502ac27
  (fn [n1 n2]
    (loop [mn (min n1 n2)]
      (if (and (zero? (mod n1 mn)) (zero? (mod n2 mn)))
        mn
        (recur (dec mn))))))

(defcheck solution-5510d64d
  (fn [a b]
    (let [m (min a b)]
      (loop [i m]
        (if (= 0 (rem a i) (rem b i))
          i
          (recur (dec i)))))))

(defcheck solution-55247cad
  (fn [val-1 val-2]
    (let [low (min val-1 val-2)
          high (max val-1 val-2)
          remainder (mod high low)]
      (if (= 0 remainder)
        low
        (recur low remainder)
        )
      )
    ))

(defcheck solution-552f0e2b
  (fn gcd [a b]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defcheck solution-5542adb8
  (fn gcd [x y]
    (let [candidate (fn [m n]
                      (if (and (== (mod y n) 0) (== (mod x n) 0))
                        n
                        m))]
      (reduce candidate (rest (range (inc (min x y))))))))

(defcheck solution-55450662
  (fn [x y]
    (if (zero? y)
      x
      (recur y (rem x y)))))

(defcheck solution-558d0e03
  (fn [& numbers]
    (let [sorted-numbers (sort numbers)
          a (second sorted-numbers)
          b (first sorted-numbers)]
      (loop [a a b b]
        (if (= b 0)
          a
          (recur b (mod a b)))))))

(defcheck solution-55cc54ba
  (fn [x y]
    (->> (range (min x y) 0 -1)
      (filter #(= 0 (mod x %) (mod y %)))
      first )))

(defcheck solution-55e02a8a
  (fn gcd [ a b ]
    (if (= a b)
      a
      (gcd (min a b) (- (max a b) (min a b))))))

(defcheck solution-5605f981
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (rem a b)))))

(defcheck solution-561dba04
  (fn gcd [x1 y1]
    (loop [x x1 y y1]
      (let [[x y] (if(> x y) [x y] [y x])]
        (cond
          (= y 0) x
          (= y 1) y
          :else (recur y (rem x y)))))))

(defcheck solution-562b69ef
  (fn ! [x y]
    (if (= y 0) x
                (! y (mod x y)))))

(defcheck solution-5652d502
  (fn self [x y]
    (cond
      (> x y) (self y x)
      (zero? (mod y x)) x
      :else (self (mod y x) x)
      )))

(defcheck solution-56680feb
  (fn [x y] (if (zero? y) x (recur y (mod x y)))))

(defcheck solution-5732f9a
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-57d77560
  (fn gcd [x y]
    (let [[x y] (if (< x y) [x y] [y x])]
      (if (= x 0) y
                  (gcd (mod y x) x)))))

(defcheck solution-580dfdd
  (fn gcd [a b]
    (let [r (rem a b)]
      (if (= 0 r)
        b
        (recur b r)))))

(defcheck solution-5865ca92
  (fn g [x y]
    (if (= 0 y)
      x
      (g y (mod x y)))))

(defcheck solution-58aae70a
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b) ))))

(defcheck solution-58ead0d2
  (fn [x y] (first (filter #(and (= 0 (mod x %)) (= 0 (mod y %)))
                     (reverse (range 1 (inc (min x y))))))))

(defcheck solution-58fc1ce6
  (fn gcd [a b] (loop [a a b b] (if (= b 0) a (recur b (mod a b))))))

(defcheck solution-591f8508
  (fn [x y] (if (= 0 x) y
                        (if (> x y) (recur (- x y) y)
                                    (recur (- y x) x) )
                        )))

(defcheck solution-595cc4f0
  (fn gcd [a b]
    (if (zero? a) b
                  (recur (mod b a) a))))

(defcheck solution-59c9dd11
  (fn [a b] (last (filter #(= (mod a %) (mod b %) 0) (range 1 (inc (min a b)) )))))

(defcheck solution-5a084395
  (fn [a b]
    (first
      (for [x (range (min a b) 0 -1)
            :when (every? identity
                    (list (= 0 (mod a x))
                      (= 0 (mod b x))))]
        x))))

(defcheck solution-5a44a17e
  (fn gcd
    [a b]
    (if (= a 0)
      b
      (if (= b 0)
        a
        (if (> a b)
          (gcd (mod a b) b       )
          (gcd a         (mod b a)) )))))

(defcheck solution-5a456a60
  (fn gcd [a b]
    (if (> a b) (gcd b a)
                (if (= a 0) b
                            (gcd a (mod b a))))))

(defcheck solution-5aae3e03
  (fn [x y] (last (filter #(and (zero? (rem x %)) (zero? (rem y %))) (range 1 (max x y))))))

(defcheck solution-5b4223c5
  (fn gcd [x y] (if (= 0 x) y (gcd (mod y x) x))))

(defcheck solution-5b81977a
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(defcheck solution-5b9dcf01
  (fn gcd [a b]
    (if (= a b) a
                (if (> a b)
                  (gcd (- a b) b)
                  (gcd a (- b a))))))

(defcheck solution-5c084a55
  ;; Euclid's algorithm https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid.27s_algorithm
  #(let [a (max %1 %2) b (min %1 %2)]
     (if (or (= a b) (zero? (mod a b)))
       b
       (recur (- a b) b))))

(defcheck solution-5c142bf9
  (fn [a b] (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-5cf77b0f
  (fn g [a b]
    (if (= 0 b)
      a
      (g b (mod a b)))))

(defcheck solution-5d066b0d
  (fn gcd [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-5dad34eb
  (fn[x y] (loop [i (min x y)]
             #_(println i x y)
             (if (and (= 0 (mod x i)) (= 0 (mod y i)) )
               i
               (recur (dec i))
               )
             )
    ))

(defcheck solution-5db24e4a
  (fn gcd [a b]
    (if (= (mod a b) 0)
      b
      (gcd b (mod a b)))))

(defcheck solution-5e4c7840
  #(loop [a % b %2]
     (if (= (rem a b) 0)
       b
       (recur b (rem a b)))))

(defcheck solution-5eba1c33
  (fn gcd [a b]
    (if (> b a) (gcd b a)
                (if (zero? b) a
                              (gcd b (mod a b))))))

(defcheck solution-5f576805
  (fn [a b]
    (let [d (fn [x] (filter #(= 0 (rem x %1)) (range 1 (inc x))))]
      (last (filter (set (d a)) (d b)))
      )))

(defcheck solution-5f8b5591
  (fn gcd [x y]
    (if (= 0 (min x y))
      (max x y)
      (gcd (min x y) (- (max x y) (min x y))))))

(defcheck solution-5fd7b5f7
  (fn [n m] (if (= 0 m) n (recur m (mod n m)))))

(defcheck solution-6008843c
  (fn naive-gcd [m n]
    (first (filter #(= 0 (mod m %) (mod n %)) (range (min m n) 0 -1)))))

(defcheck solution-601b8da4
  (fn gcd [x y]
    (if (= y 0) x (recur y (rem x y)))))

(defcheck solution-60aed0b1
  (fn [a b]
    (some #(if (every? integer? %) (first %))
      (for [k (range (min a b) 0 -1)] [k (/ (min a b) k) (/ (max a b) k)]))))

(defcheck solution-60e97b0e
  #(loop [a % b %2]
     (if (= b 0)
       a
       (recur b (mod a b)))))

(defcheck solution-610f2326
  (fn gcd [x y]
    (cond
      (= x y) x
      (< x y) (gcd (- y x) x)
      :else (gcd y x))))

(defcheck solution-616053a8
  (fn [a b]
    (if (zero? (rem a b))
      b
      (recur b (rem a b)))))

(defcheck solution-617b6ed5
  (fn [x y]
    (loop [a x b y]
      (if (> b a) (recur b a)
                  (if (= (mod a b) 0)
                    b
                    (recur b (mod a b)))))))

(defcheck solution-61907c3b
  (fn gcd
    [x y]
    (cond (= 0 (rem x y)) y
          (= 0 (rem y x)) x
          :else
          (loop [i (quot (min x y) 2)]
            (if (= 0 (rem x i) (rem y i)) i
                                          (recur (dec i)))))))

(defcheck solution-61c7b901
  (fn gcd [a b]
    (if (zero? a) b
                  (gcd (rem b a) a))))

(defcheck solution-62196fc1
  (fn [a b] (if (= a b) a (recur (min a b) (Math/abs (- a b))))))

(defcheck solution-6234aa29
  (fn [& xs]
    (apply max
      (apply clojure.set/intersection
        (map set
          (map
            (fn [z] (filter #(zero? (rem z %)) (range 1 (inc z)))) xs))))))

(defcheck solution-628b972b
  (letfn [(gcd [x y]
            (cond (<= x 0) y
                  (<= x y) (recur x (mod y x))
                  :else (recur y x)))]
    gcd))

(defcheck solution-62a29bbd
  (fn bv [a b]
    (let [[x y] (if (> a b) [a b] [b a])]
      (if (= 0 (mod x y))
        y
        (recur y (mod x y))))))

(defcheck solution-62a3a6fb
  (fn gcd [u v]
    (cond
      (= u v) u
      (= u 0) v
      (= v 0) u
      (even? u) (cond
                  (odd? v) (gcd (bit-shift-right u 1) v)
                  :else (bit-shift-left
                          (gcd (bit-shift-right u 1) (bit-shift-right v 1))
                          1))
      (even? v) (gcd u (bit-shift-right v 1))
      (> u v) (gcd (bit-shift-right (- u v) 1) v)
      :else (gcd (bit-shift-right (- v u) 1) u))))

(defcheck solution-62f3cd2a
  (fn my-common-divisor [x y]
    (loop [n (min x y)]
      (when (< n (+ (min x y) 1))
        (if (and (= (mod x n) 0) (= (mod y n) 0))
          n
          (recur (- n 1)))))))

(defcheck solution-6309a1fa
  (fn [a b]
    (cond (= a b) a,
          (> a b) (recur (- a b) b),
          (< a b) (recur (- b a) a))))

(defcheck solution-6344e4a5
  (fn [a b]
    (let [d (fn [x]
              (set (filter #(= 0 (mod x %)) (range 1 (inc x)))))]
      (apply max
        (clojure.set/intersection
          (d a) (d b))))))

(defcheck solution-63b64735
  #(cond
     (= %1 %2) %1
     (< %1 %2) (recur %2 %1)
     (= %2 0) %1
     :else (recur (- %1 %2) %2)))

(defcheck solution-63bdbb4c
  (fn [& args] (reduce (fn [r n] (if (every? #(= 0 (mod % n)) args) n r)) 1 (range 1 (inc (apply min args))))))

(defcheck solution-63dfb212
  (fn[x y](let [ls (range 1 (inc (min x y)))]  (last (filter #(and (zero? (rem x %)) (zero? (rem y %))) ls)))))

(defcheck solution-63e2a6af
  (fn [a b]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defcheck solution-63fa741c
  (fn gcd [x y]
    (if (= x y) x (let [small (min x y), large (max x y)]
                    (gcd (- large small) small)))))

(defcheck solution-640de736
  (fn NWD [x y]
    (if (= y 0)
      x
      (NWD y (mod x y)))))

(defcheck solution-644b9378
  (fn gcd
    [a b]
    (let[ c (mod a b)]
      (if(zero? c)
        b
        (recur b c)))))

(defcheck solution-64badae6
  (fn gcd [a b]
    (cond
      (= a b) a
      (> a b) (gcd (- a b) b)
      :else (gcd a (- b a)))))

(defcheck solution-650f72a2
  (fn [a b]
    (first
      (filter #(and (zero? (mod a %))
                    (zero? (mod b %)))
        (range (min a b) 0 -1)))))

(defcheck solution-6642f6ad
  (fn[a b](
            apply max (filter (fn[x] (contains?
                                       (set(filter (fn[x](= (mod a x) 0)) (range 1 (inc a))))
                                       x))
                        (filter (fn[x](= (mod b x) 0)) (range 1 (inc b))))
            )))

(defcheck solution-664f72f0
  (fn [x y]
    (loop [M (max x y) m (min x y)]
      (if (= (rem M m) 0)
        m
        (recur m (rem M m))))))

(defcheck solution-66bb592a
  (fn gcd' [x y]
    (if (= y 0) x
                (recur y (rem x y)))))

(defcheck solution-67824263
  (fn [a b] (first (filter (fn [n] (and (= 0 (mod b n)) (= 0 (mod a n)))) (let [d (max a b)] (range (/ (if (odd? d) (inc d) d) 2) 0 -1 ))))))

(defcheck solution-682f542d
  #(last
     (filter (fn [x] (and (= 0 (mod %1 x))
                          (= 0 (mod %2 x))))
       (range 1 (inc (min %1 %2))))))

(defcheck solution-6833e2c2
  #(loop [cur (min %1 %2)]
     (if (= cur 1)
       cur
       (if (and (integer? (/ %1 cur)) (integer? (/ %2 cur)))
         cur
         (recur (dec cur))))
     ))

(defcheck solution-68454508
  (fn [m n]
    (loop [a (max m n) b (min m n)]
      (let [r0 (rem a b) q0 (int (/ a b))]
        (if
         (zero? r0)
          b
          (recur r0 (rem b r0)))))))

(defcheck solution-6879c235
  (fn gcd [x y]
    (loop [a (max x y) b (min x y)]
      (if (= 0 b)
        a
        (recur b, (mod a b)))
      )
    ))

(defcheck solution-699ec0d6
  #(loop [a (max %1 %2)
          b (min %1 %2)
          r (rem a b)]
     (if (= r 0)
       b
       (recur b r (rem b r)))))

(defcheck solution-69b7c6fa
  (fn it [x y] (if (= y 0) x (it y (mod x y)))))

(defcheck solution-69d2cc1f
  (fn [a b]
    (cond
      (> b a) (recur b a)
      (= 0 b) a
      :else (recur b (rem a b)))))

(defcheck solution-69e5f699
  (fn gcd [a b]
    (if (< a b)
      (recur b a)
      (let [d (rem a b)]
        (if (= d 0)
          b
          (recur b d))))))

(defcheck solution-6a4f61f8
  #(if (zero? %)
     %2
     (recur (mod %2 %) %)))

(defcheck solution-6ab83438
  #(loop [curr %1] (if (= 0 (rem %1 curr) (rem %2 curr))
                     curr
                     (recur (dec curr)))))

(defcheck solution-6b6615a6
  (fn gcd [a b]
    (if (zero? b) a
                  (recur b (mod a b)))))

(defcheck solution-6b68f080
  (fn gcd [x y]
    (let [smaller (min x y)
          larger (max x y)]
      (if (zero? smaller)
        larger
        (gcd (- larger smaller) smaller)))))

(defcheck solution-6b918d29
  (fn f [m n] (if (zero? n) m (recur n (mod m n)))))

(defcheck solution-6bae6aba
  (fn ! [a b]
    (let [dividend (max a b) divisor (min a b)]
      (if (= (mod dividend divisor) 0)
        divisor
        (! divisor (mod dividend divisor))
        )
      )
    ))

(defcheck solution-6bd5786b
  #(letfn [(gcd [x y]
             (let [v (rem y x)]
               (if (zero? v) x
                             (gcd v x))))]
     (if (< %2 %1) (gcd %2 %1)
                   (gcd %1 %2))))

(defcheck solution-6cc94fdd
  (fn [a b] (let [g (max a b) l (min a b)] (if (zero? b) a (recur (- g l) l)))))

(defcheck solution-6ce266b3
  (fn gcd [x y]
    (loop [x x
           y y]
      (cond
        (= x y) x
        (< x y) (recur x (- y x))
        :else 	(recur y (- x y))
        ))))

(defcheck solution-6d05cff2
  (fn [a b]
    (apply
      max
      (filter
        #(and
          (= (rem a %) 0)
          (= (rem b %) 0))
        (range
          1
          (+
           (min a b)
           1))))))

(defcheck solution-6de048f2
  (fn [& args]
    (let [[m & r :as sorted] (sort args)]
      (reduce (fn [memo el]
                (if (every? #(zero? (rem % el)) sorted)
                  el
                  memo))
        (range 1 (inc m))))))

(defcheck solution-6e057460
  (fn f [a b]
    (cond
      (= a b) a
      (> a b) (f (- a b) b)
      :else   (f a (- b a)))))

(defcheck solution-6e15ccea
  (fn gcd [a b]
    (if (= 0 b) a (gcd b (mod a b)))))

(defcheck solution-6f026766
  (fn gcd [a b]
    (let [r (mod a b)]
      (if (= r 0)
        b
        (gcd b r)))))

(defcheck solution-6f319ece
  (fn [a b] (cond (= a 0) b (= b 0) a (> a b) (recur (rem a b) b) :else (recur a (rem b a)))))

(defcheck solution-6f5901c7
  (fn my-gcd [x y]
    (let [r (rem x y)]
      (if (zero? r) y (recur y r)))))

(defcheck solution-701cea88
  (fn ggt
    [a b]
    (let [aa (max a  b) bb (min a b)]
      (let [m (mod aa bb)]
        (if (= 0 m) bb (recur b m))))))

(defcheck solution-70700867
  (fn [x y] (if (== x y) x (if (< x y) (recur x (- y x)) (recur (- x y) y)))))

(defcheck solution-70ccf7
  (fn [x y]
    (loop [x x
           y y]
      (if (> y x)
        (recur y x)
        (if (zero? y)
          x
          (recur y (- x y)))))))

(defcheck solution-70f7bf52
  (fn gcd [& xs]
    (loop [[a b] (sort xs) r (mod a b)]
      (if (= r 0)
        b
        (recur [b r] (mod b r))))))

(defcheck solution-7113ebd6
  (fn gcd [a b] (if (= 0 b) a (gcd b (mod a b)))))

(defcheck solution-71268c4a
  (fn [a b]
    (if (zero? b) a (recur b (rem a b)))))

(defcheck solution-71a42c0d
  (fn [x y](reduce max (filter #(and (= 0 (mod x %)) (= 0 (mod y %))) (range 1 (inc (min x y)))))))

(defcheck solution-71c564b4
  (fn [x y]
    (loop [m (if (> x y) x y) n (if (= m x) y x) r (rem m n)]
      (if (= r 0) n (recur n r (rem n r))))))

(defcheck solution-72361a8a
  (fn f [a b]
    (if (= b 0) a (f b (rem a b)))))

(defcheck solution-724970fe
  (fn number66 [h l]
    (let [high (max h l) low (min h l)]
      (if (zero? low) high
                      (recur low (mod high low))))))

(defcheck solution-7342fd25
  (fn [n m] (loop [n n m m l (min n m)] (if (= 0 (mod n l) (mod m l)) l (recur n m (dec l))))))

(defcheck solution-7354492f
  (fn [a b]
    (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-740b1d1b
  (fn [x y]
    (->> (range (max x y))
      rest
      reverse
      (some #(if (and (integer? (/ x %)) (integer? (/ y %))) %)))))

(defcheck solution-7471f8f9
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-747675cf
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-74a944dc
  (fn  [n1 n2] (some #(if (and (= 0 (mod n1 %)) (= 0 (mod n2 %))) % false) (range n1 0 -1))))

(defcheck solution-758b94b2
  #(loop [a 1 b % c %2 d 1] (cond
                              (> a b) d
                              (and (> a 1)
                                   (= 0 (mod b a))
                                   (= 0 (mod c a)))
                              (recur a (/ b a) (/ c a) (* a d))
                              :else (recur (inc a) b c d)
                              )))

(defcheck solution-75dcf56e
  (fn
    [a b]
    (cond (> b a) (recur b a)
          (= b 0) a
          :else (recur b (rem a b)))))

(defcheck solution-76018af4
  (fn [a b] (apply max (for [ i (range 1 (inc (max a b))) :when (= 0 (+ (rem a i) (rem b i))) ] i))))

(defcheck solution-767e71d
  (fn [x y] (some #(if (= 0 (mod x %) (mod y %)) %)
              (range (min x y) 0 -1))))

(defcheck solution-7698af65
  (fn [n1 n2]
    (let [div1 (filter #(zero? (rem n1 %)) (range 1 (inc n1)))
          div2 (filter #(zero? (rem n2 %)) (range 1 (inc n2)))]
      (->> (concat div1 div2)
        sort
        (partition-by identity)
        (filter #(= 2 (count %)))
        last
        first)

      )))

(defcheck solution-76a9e5d5
  (fn [a b]
    (loop [candidate 2
           divisors [1]]
      (if (> candidate (min a b))
        (apply max divisors)
        (recur
          (inc candidate)
          (if (= 0 (rem a candidate) (rem b candidate))
            (conj divisors candidate)
            divisors))))))

(defcheck solution-76e29072
  #(loop [n1 %1 n2 %2]
     (cond (= n1 n2) n1
           (< n1 n2) (recur n1 (- n2 n1))
           :else (recur (- n1 n2) n2))))

(defcheck solution-76f60100
  (fn [a b]
    (cond
      (= a 0) b
      (= b 0) a
      :else (recur b (rem a b)))))

(defcheck solution-77415eec
  (fn [a b]
    (first
      (filter
        #(and (zero? (mod a %)) (zero? (mod b %)))
        (range a 0 -1)))))

(defcheck solution-775014cb
  (fn [a b]
    (loop [x a
           y b]
      (let [maxv (if (> x y) x y)
            minv (if (> x y) y x)]
        (if (= 0 minv) maxv
                       (recur (- maxv minv) minv))))))

(defcheck solution-78819889
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(defcheck solution-78af3671
  (fn [a b]
    (if (= b 0) a
                (recur b (rem a b)))))

(defcheck solution-78def545
  (fn [x y] (last (filter #(and (= (mod x %) 0) (= (mod y %) 0)) (range 1 (+ (min x y) 1))))))

(defcheck solution-7930840f
  (fn gcd [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-7933a351
  #(last (remove nil? (for [x (range 1 (inc %1))]
                        (if (and (zero? (mod %1 x)) (zero? (mod %2 x)))
                          x)))))

(defcheck solution-795fe34a
  (fn [a b]
    (let [c (max a b)
          d (min a b)]
      (loop [a c
             b d]
        (if (zero? b)
          a
          (recur b (rem a b)))))))

(defcheck solution-79668af0
  (fn gcd [x y] (loop [a x b y] (if (= b 0) a (recur b (mod a b))))))

(defcheck solution-799fe005
  (fn [x y]
    (some #(if (= 0 (rem x %) (rem y %)) %) (range (min x y) 0 -1))
    ))

(defcheck solution-79dc2d91
  (fn [a b]
    (last (filter #(= (mod a %) (mod b %) 0) (range 1 (max a b))))))

(defcheck solution-7a3ec7b5
  (fn gcd [a b]
    (loop [a a b b]
      (if (zero? b) a,
                    (recur b (mod a b))))))

(defcheck solution-7a53a62d
  #(if (zero? %2) % (recur %2 (rem % %2))))

(defcheck solution-7a568a9e
  (fn gcd [a b]
    (let [divides? #(= 0 (mod %2 %1))]
      (if (< b a)
        (recur b a)
        (if (divides? a b)
          a
          (recur (mod b a) a))))))

(defcheck solution-7b1eaaf5
  (fn [a b]
    (loop [d (min a b)]
      (if (= 0 (rem a d) (rem b d))
        d
        (recur (dec d)))
      )))

(defcheck solution-7b358f5
  (fn [a b] (if (= 0 b) a (recur b (mod a b)))))

(defcheck solution-7b7cda64
  (fn gcd
    [a b]
    (if (zero? b)
      a
      (gcd b (mod a b)))))

(defcheck solution-7b8271e2
  (fn
    [a b]
    (cond
      (= a b) a
      (> a b) (recur (- a b) b)
      (> b a) (recur a (- b a)))))

(defcheck solution-7b9694de
  (fn [m n]
    (if (zero? n)
      m
      (recur n (rem m n)))))

(defcheck solution-7b9ff31
  (fn [x y]
    (if (zero? y) x (recur y (mod x y)))))

(defcheck solution-7c3acf4d
  (letfn [(gcd [a b]
            (if (= a b)
              a
              (if (> a b)
                (gcd (- a b) b)
                (gcd a (- b a)))))]
    (fn [x y] (gcd x y))))

(defcheck solution-7c3fed5e
  (fn  [a b]
    (loop [x (min a b) y (max a b)]
      (if (zero? (mod y x)) x
                            (recur (mod y x) x)))))

(defcheck solution-7c597144
  (fn [a b]
    (let [r (rem a b)]
      (if (zero? r) b (recur b r)))))

(defcheck solution-7c5e5cfe
  (fn [a b]
    (last
      (filter #(and (zero? (mod a %))
                    (zero? (mod b %)))
        (range 1 (max a b))))))

(defcheck solution-7caeea0e
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-7cc59b52
  (fn gcd [a b]
    (loop [a a b b]
      (if (= a b)
        a
        (if (> a b)
          (recur (- a b) b)
          (recur a (- b a)))))))

(defcheck solution-7cf68776
  (fn [a b] (some #(if (= 0 (rem a %) (rem b %)) %) (range b 0 -1))))

(defcheck solution-7d38defa
  (fn prob-0066 [& nums]
    (first (for [den  (range (apply max nums) 0 (- 1))
                 :let [rems (map #(rem % den) nums)]
                 :when (apply = 0 rems) ]
             den))))

(defcheck solution-7d4fdaec
  (fn gcd [a b] (if (zero? b) a (recur b (rem a b)))))

(defcheck solution-7d9f3abf
  (fn gcd- [a b]
    "66. Given two integers, write a function which returns the greatest
    common divisor."
    (if (= 0 b)
      a
      (recur b (mod a b)))))

(defcheck solution-7db7d580
  (fn [a b]
    (reduce (fn [acc div] (if (and (zero? (mod a div))
                                   (zero? (mod b div))) div acc) )
      1
      (range 2 (inc a))) ))

(defcheck solution-7dcf0ddc
  #(if (= 0 %2) %1 (recur %2 (rem %1 %2))))

(defcheck solution-7df7310
  #(if (= % 0) %2 (recur (mod %2 %) %)))

(defcheck solution-7e09c94d
  (fn [a b]
    (loop [[a b] (if (> a b) [a b] [b a])]
      (let [c (rem a b)]
        (if (= 0 c)
          b
          (recur [b c]))))))

(defcheck solution-7e11422b
  (fn [a b]
    (if (zero? b) a
                  (recur b (mod a b)))))

(defcheck solution-7e627d4d
  (fn [x y]
    (some #(if (integer? (/ y %)) %)
      (map #(/ x %) (range 1 (inc x))))))

(defcheck solution-7e8083e0
  (fn gcd ([a b] (if (zero? b) a (gcd b (mod a b))))))

(defcheck solution-7ecc7353
  (fn gcd [a b]
    (if (= b 0) a
                (gcd b (mod a b)))
    ))

(defcheck solution-7ed77e4
  (fn [a b]
    (loop [a a b b]
      (if (= a 0)
        b
        (if (= b 0)
          a
          (recur b (mod a b)))))))

(defcheck solution-7f5cdee7
  (fn f [a b] (if (= b 0) a (f b (rem a b)))))

(defcheck solution-7f6825d8
  #(loop [a %1 b %2]
     (if
      (= a b)
       a
       (let [[lower higher] (sort [a b])]
         (recur lower (- higher lower))))))

(defcheck solution-7f861bc5
  (fn [a b]
    (cond
      (zero? a) b
      (zero? b) a
      (< a b) (recur a (- b a))
      :else (recur b (- a b)))))

(defcheck solution-7ff208cb
  (fn gcd [a b]
    (cond
      (> b a) (recur b a)
      (zero? b) a
      :else (recur b (mod a b)))))

(defcheck solution-807174e0
  (fn gcd [a b]
    (if (< a b)
      (recur b a)
      (first (filter #(= 0 (rem a %) (rem b %)) (iterate dec b))))))

(defcheck solution-80badcce
  (fn gcd [a b]
    (if (= a b) a
                (let [c (max a b) d (min a b)]
                  (gcd (- c d) d)))))

(defcheck solution-80bb93d9
  (fn [x y]
    (loop [ a (if (> x y) y x), b (- (+ x y) a)]

      (if (= 0 (mod b a))
        a
        (if(< a (- b a))
          (recur a (- b a))
          (recur (- b a) a)
          )
        )
      )
    ))

(defcheck solution-80dc8a3a
  (fn f [a b] (if (= 0 a) b (f (mod b a) a))))

(defcheck solution-80e248e4
  #(if (zero? %2) %1 (recur %2 (rem % %2))))

(defcheck solution-8135101c
  (fn [a b]
    (last (let [min (if (< a b) a b)]
            (filter
              #(and (= 0 (rem a %)) (= 0 (rem b %)))
              (range 1 (inc min))
              )))))

(defcheck solution-813d1727
  (fn gcd [a b]
    (if (= b 0)
      a
      (recur b (mod a b))
      )
    ))

(defcheck solution-814bdff5
  (fn [x y]
    (loop [iter 1
           result 1]
      (if (or (> iter x) (> iter y))
        result
        (recur (inc iter)
          (if (and
               (= (mod x iter) 0)
               (= (mod y iter) 0))
            iter
            result))))))

(defcheck solution-8189768a
  (fn [x y] (if (zero? y) x (recur y (rem x y)))))

(defcheck solution-818b90fd
  (fn gcd [x y]
    (cond
      (= x y) x
      (> x y) (gcd (- x y) y)
      (< x y) (gcd x (- y x)))))

(defcheck solution-830974e8
  (fn [x y]
    (cond
      (= x y) x
      (> x y) (recur (- x y) y)
      :else (recur x (- y x)))))

(defcheck solution-834c9823
  (fn blah
    [a b]
    (first
      (filter #(= 0 (mod a %1) (mod b %))
        (reverse (range 1 (inc (min a b))))
        )
      )
    ))

(defcheck solution-838e8a94
  #(if (zero? %2) %1 (recur %2 (rem %1 %2))))

(defcheck solution-83bb9cb7
  (fn [x y] (cond (= x y) x (> x y) (recur y (- x y)) 1 (recur x (- y x)))))

(defcheck solution-83f26098
  (fn [x y] (last (sort (filter #(= (mod x %) (mod y %) 0) (range 1 (max x y)))))))

(defcheck solution-849a69ed
  (fn [ x y] (last (filter #( if (and (zero? (rem x %)) (zero? (rem y %))) %) (range 1 (inc (Math/min x y)))))))

(defcheck solution-84ad42d
  (fn [a b]
    (cond
      (< a b)   (recur b a)
      (zero? b) a
      :else     (recur b (mod a b)))))

(defcheck solution-85199fb9
  #(last (apply sorted-set(apply clojure.set/intersection
                            (map (fn [x] (into #{}
                                           (filter (fn [e] (zero? (mod x e)))(range 1 (inc x))))) [% %2])
                            ))))

(defcheck solution-8526bf69
  (fn gcd [x y]
    (if (= 0 y)
      x
      (recur y (mod x y)))))

(defcheck solution-852ccd74
  #(loop [a %1 b %2]
     (cond (= 0 a) b
           (= 0 b) a
           (> a b) (recur (- a b) b)
           :else (recur (- b a) a))))

(defcheck solution-85335f18
  (fn [x y]
    (let [m (min x y)]
      (letfn [(go [n l]
                (if (<= n m)
                  (if (and (= 0 (rem x n))
                           (= 0 (rem y n)))
                    (go (inc n) n)
                    (go (inc n) l))
                  l))]
        (go 2 1)))))

(defcheck solution-855ee718
  (fn gcd [n m]
    (if (zero? m)
      n
      (gcd m (mod n m)))))

(defcheck solution-85c6abcb
  (fn pgcd [a b] (cond
                   (= a b) a
                   (= a 0) b
                   (= b 0) a
                   (< a b) (pgcd a (- b a))
                   (< b a) (pgcd (- a b) b))))

(defcheck solution-85f7164b
  (fn peu [x y] (if (> y x) (peu y x) (if (= y 0) x (peu (- x y) y)))))

(defcheck solution-8605f2ca
  (fn [a b] (first (filter #(= 0 (mod a %) (mod b %)) (range a 0 -1)))))

(defcheck solution-861813f6
  #(apply max (filter (fn [x] (and (zero? (mod %1 x)) (zero? (mod %2 x)))) (range 1 (inc (min %1 %2))))))

(defcheck solution-8683428f
  (fn [a b]
    (loop [x (min a b)]
      (if (and (= 0 (rem a x)) (= 0 (rem b x)))
        x
        (recur (dec x))))))

(defcheck solution-8727b426
  (fn [n1 n2]
    (some (fn [n]
            (and
             (pos? n)
             (zero? (mod n1 n))
             (zero? (mod n2 n))
             n))
      (reverse (range (inc  (min n1 n2)))))))

(defcheck solution-872b6b31
  (fn [n1 n2]
    (->> (range (min n1 n2))
      (map inc)
      (filter #(and (= 0 (mod n1 %)) (= 0 (mod n2 %))))
      last
      )))

(defcheck solution-873ed7ab
  (fn gcd [x y]
    (if (<= y 0)
      x
      (gcd y (mod x y)))))

(defcheck solution-8749dc07
  (fn [x y]
    (let [big   (max x y)
          small (min x y)]
      (if (or (= big small) (= small 1)) small
                                         (recur small (- big small))))))

(defcheck solution-874cce91
  (fn gcd
    [a b]
    (if (= a b)
      a
      (if (> a b)
        (recur (- a b) b)
        (recur a (- b a))))))

(defcheck solution-87560747
  (fn get-max-common-divisor
    [n m]
    (if (zero? (mod n m))
      m (get-max-common-divisor m (mod n m)))))

(defcheck solution-888e0bd
  (fn [ a b]
    (apply max (map #( if (= 0 (rem a %) (rem b %)) % 1) (range 2 (inc a))))))

(defcheck solution-88cbc9a9
  (fn gcd
    [a b]
    (if (zero? b) a (gcd b (mod a b)))
    ))

(defcheck solution-88f45f8a
  #(->> (iterate (fn [[a b]] [(mod b a) a]) [% %2])
     (some (fn [[a b]] (and (zero? a) b)))))

(defcheck solution-89248e9d
  (fn [a b]
    (apply max
      (filter
        #(and (= 0 (mod b %)) (= 0 (mod a %)))
        (take-while
          #(<= % (max a b))
          (iterate inc 1))))))

(defcheck solution-894689c
  (fn [n m]
    (if (zero? n)
      m
      (recur (rem m n) n))))

(defcheck solution-89490e94
  (fn gcd [x y]
    (if (zero? y) x (gcd y (mod x y)))))

(defcheck solution-89b7b21
  (fn gcd [x y] (if (= y 0) x (gcd y (mod x y)))))

(defcheck solution-89c6f42a
  (fn [a b] (if (zero? b)
              a
              (recur b (mod a b)))))

(defcheck solution-89d2f5c
  #(let [[a b] (if (> % %2) [% %2] [%2 %])
         rest (mod a b)]
     (if (zero? rest) b (recur b rest))))

(defcheck solution-8a73f5b9
  (fn gcd
    [a b]
    (cond
      (< b a) (gcd b a)
      (zero? a) b
      :else (gcd (mod b a) a))))

(defcheck solution-8ab58e07
  (fn gdc [a b]
    (let [c (mod a b)]
      (if (zero? c)
        b
        (gdc b c)))))

(defcheck solution-8ab8cc69
  (fn [x y]
    (let [dv (fn [z] (set (filter #(zero? (rem z %)) (range 1 (inc z)) )))]
      (apply max (clojure.set/intersection (dv x) (dv y))))))

(defcheck solution-8b38a31c
  #(if (= %2 0) % (recur %2 (rem % %2))))

(defcheck solution-8b3e6589
  (fn [x y]
    (loop [div (min x y)]
      (if (and (zero? (mod x div)) (zero? (mod y div)))
        div
        (recur (dec div))))))

(defcheck solution-8b4db50a
  (fn [u d]
    (let [[f s] (if (< u d) [u d] [d u])]
      (apply max (filter #(and (zero? (mod f %)) (zero? (mod s %))) (range 1 (inc f)))))))

(defcheck solution-8b68a39e
  (fn [x y]
    (let [z (min x y)]
      (some #(when
              (== 0
                (mod x %)
                (mod y %)) %)
        (range (inc z) 0 -1)))))

(defcheck solution-8bb2f2ad
  #(loop [x  (min %1 %2) ]
     (if (= 0 (rem %1 x) (rem %2 x)) x
                                     (recur (dec x)))))

(defcheck solution-8bf826a4
  #(if (= % %2) % (if (> % %2) (recur (- % %2) %2) (recur % (- %2 %)))))

(defcheck solution-8c1fe4af
  (fn [x y]
    (if (zero? y)
      x
      (recur y (mod x y)))))

(defcheck solution-8c9fb1b8
  (fn g [x y]
    (if (zero? y)
      x
      (g y (rem x y)))))

(defcheck solution-8d4abcc7
  (fn f [a b]
    (let [x (max a b)
          y (min a b)]
      (if (zero? y)
        x
        (f y (mod x y))))))

(defcheck solution-8d663774
  ;; from astangl, better than mine.
  #(if (= 0 %2)
     %1
     (recur %2 (mod %1 %2))))

(defcheck solution-8dbcfb96
  #(first (filter (fn [x] (and (= (mod %1 x) 0)
                               (= (mod %2 x) 0)))
            (reverse (range (inc (min %1 %2)))))))

(defcheck solution-8e674ac3
  #((fn euclid [x y](let [z (mod x y)](if (= z 0)y(euclid y z)))) (max %1 %2)(min %1 %2)))

(defcheck solution-8ef0a20a
  (fn gcd
    [a b]
    (let [amodb (mod a b)]
      (cond
        (= (mod a b) 1) 1
        (= (mod a b) 0) (min a b)
        :else (gcd b (mod a b))))))

(defcheck solution-900f7920
  (fn gcd [a b]
    (if (= b 0)
      a
      (recur b (mod a b))
      )
    ))

(defcheck solution-9060dd2e
  (fn [& numbers]
    (let [min-number (apply min numbers)]
      (loop [gcd 1 current 2]
        (if (> current min-number)
          gcd
          (recur (if (every? #(= 0 (rem % current)) numbers)
                   current
                   gcd)
            (inc current)))))))

(defcheck solution-90866c2a
  (fn[x y] (loop [a x
                  b y]
             (cond
               (zero? b) a
               :default (recur b (mod a b))))))

(defcheck solution-9118e883
  (fn gcd [a b] (if (zero? b) a (gcd b (mod a b)))))

(defcheck solution-912fc47a
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-91a092ba
  (fn [x y] (last (filter #(= (mod x %) (mod y %) 0) (range 1 (+ 1 y))))))

(defcheck solution-91ec38d3
  #(loop[i %]
     (if (= 0 (+ (rem % i) (rem %2 i))) i
                                        (recur (- i 1))
                                        )))

(defcheck solution-92166d9b
  (fn [x y]
    (some #(when ( and (= 0 (rem x %)) (= 0 (rem y %))) %)  (range (min x y) 0 -1))))

(defcheck solution-9227b32e
  (fn [a b]
    (if (= 0 b)
      a
      (recur b (mod a b)))))

(defcheck solution-928a5e39
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))
      )
    ))

(defcheck solution-92b407b3
  (fn nok [a b] (let [[x1 x2] (sort [a b]) xm (mod x2 x1)] (if (zero? xm) x1 (nok xm x1)))))

(defcheck solution-931ee60
  (fn my-gcd [a b ]
    (letfn [(abs [n]
              (cond
                (neg? n) (- n)
                :else n))]
      (if (or (not (integer? a)) (not (integer? b)))
        (throw (ex-info "gcd requires two integers" {})))
      (loop [a (abs a)
             b (abs b)]
        (if (zero? b)
          a
          (recur b (mod a b)))))))

(defcheck solution-933980b
  #(loop [result 1 i result]
     (if (> i (min %1 %2))
       result
       (recur (if (and (= 0 (mod %1 i)) (= 0 (mod %2 i))) i result)
         (inc i)))))

(defcheck solution-937e7f60
  ; not optimal but works :)
  (fn [a1 b1]
    (last
      (let [a (max a1 b1) b (min a1 b1)]
        (filter #(= 0 (+ (mod a %) (mod b %))) (range 1 (+ b 1)))))))

(defcheck solution-93915d87
  (fn f [a b]
    (if (= 0 b)
      a
      (f b (rem a b)))))

(defcheck solution-939f9ac
  (fn f [a b]
    (let [x (max a b)
          y (min a b)]
      (if (zero? (rem x y))
        y
        (f y (rem x y))))))

(defcheck solution-93a6800
  #(loop [a %1 b %2]
     (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-944fc0ae
  #(if (= %2 0) %1 (recur %2 (mod %1 %2))))

(defcheck solution-94aef8c7
  (fn [x y] (first (drop-while #(not (and (= 0 (mod x %) (mod y %)))) (reverse (range 1 (+ 1 (min x y))))))))

(defcheck solution-94dded05
  #(if (zero? %2) %1 (recur %2 (mod %1 %2))))

(defcheck solution-9508344b
  (fn  [a b] (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-9538a50
  #(if (= %1 0) %2 (recur (mod %2 %1) %1)))

(defcheck solution-955b2e86
  (fn [a b] (first (filter #(= 0 (rem a %) (rem b %)) (range (min a b) 0 -1)))))

(defcheck solution-955b39f
  (comp (partial apply +) first (partial drop-while (comp (partial < 0) (partial apply min))) #(iterate (fn [[x1 x2]] (vector (mod x1 x2) (mod x2 x1))) [%1 %2])))

(defcheck solution-961e6a82
  (fn  [a b]
    (let [x (max a b)
          y (min a b)]
      (if (zero? y)
        x
        (recur y (mod x y))))))

(defcheck solution-9688647f
  (fn [x y]
    (let [a (if (> x y) x y) b (if (> x y) y x)]
      (loop [n a remain b]
        (if-not (= remain 0)
          (recur remain (mod n remain))
          n)))))

(defcheck solution-96f31557
  (fn gcd [a b]
    (if (> b a)
      (gcd b a)
      (if (= b 0)
        a
        (gcd b (mod a b))))))

(defcheck solution-97023fba
  (fn [a b]
    (loop
     [d (min a b)]
      (if
       (= 0 (apply + (map mod [a b] [d d])))
        d
        (recur (- d 1))))))

(defcheck solution-9761d922
  #(loop [c 1 x 1]
     (if (> c (min %1 %2)) x
                           (if (= 0 (mod %1 c) (mod %2 c))
                             (recur (inc c) c)
                             (recur (inc c) x)))))

(defcheck solution-9775cbdf
  (fn gcd [x y]
    (loop [a x, b y]
      (if (zero? b)
        a
        (recur b (rem a b))))))

(defcheck solution-9798de5b
  #(cond (= %1 %2) %1
         (> %1 %2) (recur (- %1 %2) %2)
         :else     (recur (- %2 %1) %1)))

(defcheck solution-97a19219
  (fn [a b]
    (let [min-num (min a b)]
      (first (filter #(= 0 (mod a %) (mod b %)) (reverse (range (inc min-num))))))))

(defcheck solution-97cdc18f
  (fn [a b]
    (let [factor (fn [n]
                   (loop [t 1
                          m n
                          factors (sorted-set)]
                     (cond
                       (> t m) factors
                       (= 0 (mod n t)) (recur (inc t) (/ n t) (conj factors t))
                       :else (recur (inc t) m factors))))]
      (last (clojure.set/intersection (factor a) (factor b))))))

(defcheck solution-97e511a6
  (fn gcd [a b]
    (cond (> a b) (gcd b a)
          (= 0 a) b
          :else (gcd (rem b a) a))))

(defcheck solution-97eb8ec5
  (fn gcd [a b]
    (if (= b 0) a (recur b (mod a b)))))

(defcheck solution-982c8335
  (fn gcd [x y]
    (cond
      (< x y) (gcd y x) ; x >= y
      (= y 0) x
      :else (recur y (mod x y)))))

(defcheck solution-987354c5
  (fn gcd [n m]
    (let [rem (mod n m)]
      (if (zero? rem)
        m
        (gcd m rem)))))

(defcheck solution-987d001a
  #(apply
     (fn gdc [a b]
       (let [r (mod a b)]
         (cond
           (= 0 r) b
           (= 1 r) 1
           :else (gdc b r))))
     ((fn [x y]
        (if-let [z (> x y)]
          [x y]
          [y x])) %1 %2)))

(defcheck solution-9891d940
  (fn gcd
    ([a b] (gcd a b (min a b)))
    ([a b res]
     (if (and (= 0 (mod a res))
              (= 0 (mod b res)))
       res
       (recur a b (dec res))))))

(defcheck solution-98e30f17
  (fn [v1 v2]
    (let [l (max v1 v2)
          s (min v1 v2)
          m (mod l s)]
      (if (zero? m) s
                    (recur s m)))))

(defcheck solution-9934a6cc
  (fn [a b]
    (+ a b (- (* a b))
       (* 2 (reduce + (map (fn [s] (quot (* s b) a)) (range 1 a)))))))

(defcheck solution-9935cdf4
  #(loop [a %1 b %2]
     (cond (< a b) (recur b a)
           (= a b) a
           :else (recur b (- a b)))))

(defcheck solution-9948316a
  #(cond (> %1 %2) (recur %2 %1)
         (= %1 0) %2
         0 (recur (mod %2 %1) %1)))

(defcheck solution-998899d9
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-99aa8be9
  (fn gcd2 [n1 n2]
    (reduce
      #(if (and (= 0 (rem n1 %2)) (= 0 (rem n2 %2))) %2 %1)
      (range 1 (inc (min n1 n2))))))

(defcheck solution-99dc2200
  (fn gcd [a b] (if (= 0 b) a (gcd b (rem a b)))))

(defcheck solution-9a2d0f6b
  (fn gcd [a b]
    (ffirst (drop-while
              (fn [[a b]] (not= 0 b))
              (iterate (fn [[a b]] [b (mod a b)]) [a b])))))

(defcheck solution-9a5701b5
  (fn gcd [m n]
    (cond
      (= m 0) n
      (= n 0) m
      :else (recur n (rem m n)))))

(defcheck solution-9c4aad4d
  (fn gcd [a b]
    (cond
      (< a b) (gcd a (- b a))
      (> a b) (gcd (- a b) b)
      :else a)))

(defcheck solution-9c53b5ed
  (fn [x y]
    (cond
      (= x y) x
      (> x y) (recur (- x y) y)
      :else (recur (- y x) x)
      )
    ))

(defcheck solution-9c869408
  (fn [a b]
    (last (filter #(and (= 0 (mod a %)) (= 0 (mod b %)))
            (range 1 (inc (min a b)))))))

(defcheck solution-9cc96710
  (fn gcd [a b]
    (loop [candidate (min a b)]
      (if (= 1 candidate)
        candidate
        (if (and (= 0 (mod a candidate))
                 (= 0 (mod b candidate)))
          candidate
          (recur (dec candidate)))))))

(defcheck solution-9cef045b
  (fn gcd [x y]
    (if (= x y) x
                (if (< x y) (gcd y x)
                            (let [m (mod x y)]
                              (if (== m 0) y (gcd y m)))))))

(defcheck solution-9d264177
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(defcheck solution-9ed928ab
  (fn [n1 n2]
    (loop [d 1 ans 1]
      (if (or (> d n1) (> d n2))
        ans
        (if (and (= (mod n1 d) 0) (= (mod n2 d) 0))
          (recur (inc d) d)
          (recur (inc d) ans))))))

(defcheck solution-a0cc3e08
  (fn  gcd [a b]
    (if (zero? b)
      a
      (gcd b (mod a b)))))

(defcheck solution-a10a594c
  (fn f [x y] (if (< x y) (f y x) (let [r (rem x y)] (if (= 0 r) y (f y r))))))

(defcheck solution-a18743e
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-a216e3d3
  (fn gcd [a, b]
    (cond
      (= b 0) a
      :else   (gcd b (rem a b))
      )
    ))

(defcheck solution-a28adc6e
  (fn gcd [a b] (if (= a b) a (if (> a b) (gcd (- a b) b) (gcd a (- b a))))))

(defcheck solution-a2b12b03
  (fn [a b]
    (loop [c a d b]
      (cond
        (= d 0) c
        :else (recur d (rem c d))))))

(defcheck solution-a40a16eb
  (fn [x y] (last (filter #(and (= 0 (mod x %)) (= 0 (mod y %))) (range 1 (inc x))))))

(defcheck solution-a40fe2a5
  (fn [x y]
    (let [a (max x y) b (min x y) r (rem a b)]
      (loop [a a b b r r]
        (if (zero? r)
          b
          (recur b r (rem b r)))))))

(defcheck solution-a414498b
  #(loop [a %1 b %2]
     (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-a45d18dc
  (fn f [a b]
    (if (< a b) (f b a)
                (if (= b 0) a (f b (mod a b))))))

(defcheck solution-a47c0c71
  (fn gcd [x y]
    (if (zero? y)
      x
      (recur y (mod x y)))))

(defcheck solution-a51d24e9
  (fn [& args]
    (loop [args (sort args)]
      (if (= (first args) (last args))
        (first args)
        (recur (sort [(first args) (- (last args) (first args))]))))))

(defcheck solution-a57d9107
  (fn gcd [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-a5e938c9
  (fn gcd [x y]
    (if (= 0 y)
      x
      (gcd y (mod x y)))))

(defcheck solution-a5fe85f8
  (fn [a b]
    (cond
      (< b a) (recur b a)
      (= 0 (mod b a)) a
      :else (recur (mod b a) a))))

(defcheck solution-a602cb1b
  (fn [x y] (apply max (filter #(= 0 (mod x %)) (filter #(= 0 (mod y %)) (range 1 (inc y)))))))

(defcheck solution-a60a8bd0
  #(if (= 0 %2) % (recur %2 (mod % %2))))

(defcheck solution-a6bb8930
  #(->> (if (< %1 %2) [%1 %2] [%2 %1])
     (iterate (fn [[x y]] [y (mod x y)]))
     (drop-while (fn [x] (not= (second x) 0)))
     ffirst))

(defcheck solution-a6ee0854
  (fn pgcd[a b]
    (if (> a b)
      (pgcd b a)
      (if (= (mod b a) 0)
        a
        (pgcd (mod b a) a)))))

(defcheck solution-a7872753
  (fn [a b]
    (cond (< a b) (recur b a)
          (= a b) a
          (> a b)
          (let [f #(apply % [b (- a b)])]
            (recur (f max) (f min)))
          )
    ))

(defcheck solution-a798b0c9
  (fn gcd [a b]
    (if (zero? b) a (gcd b (mod a b)))))

(defcheck solution-a8114250
  (fn [a b] (last (filter #(= 0 (rem a %) (rem b %)) (range 1 (max a b))))))

(defcheck solution-a8116d23
  (fn my-gcd [a, b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-a8243d2e
  (fn [& nums]
    (let [[big lil] (sort > nums)]
      (loop [a big
             b lil]
        (let [q (quot a b)
              r (rem a b)]
          (if (zero? r)
            b
            (recur b r)))))))

(defcheck solution-a861f9fd
  (fn [x y] (apply max (filter #(= 0 (mod x %) (mod y %)) (range 1 (+ 1 (max (/ x 2) (/ y 2))))))))

(defcheck solution-a8a1505c
  (fn [x y](loop [a x b y] (if (= (rem a b) 0) b (recur b (rem a b))))))

(defcheck solution-a8cae01c
  (fn [a b]
    (cond (< a b) (recur b a)
          (zero? b) a
          :else (recur b (mod a b)))))

(defcheck solution-a948dc20
  (fn [v1 v2]
    (apply max
      (filter #(and (zero? (mod v1 %))
                    (zero? (mod v2 %)))
        (range 1 (max v1 v2))))))

(defcheck solution-a94bea88
  (fn [a b]
    (let [[smaller bigger] (sort [a b])]
      (if (= smaller 0)
        bigger
        (recur smaller (mod bigger smaller))))))

(defcheck solution-a9b18a07
  (fn g [a b]
    (if (= b 0)
      a
      (g b (mod a b)))))

(defcheck solution-a9b25a74
  (fn comdiv

    ([a b]

     (comdiv a b (min a b)))

    ([a b out]

     (if (= (rem a out) (rem b out) 0)

       out

       (comdiv a b (dec out))))))

(defcheck solution-aa146a93
  #(if (= 0 (rem %1 %2)) %2 (recur %2 (rem %1 %2))))

(defcheck solution-aa1ba9f7
  (fn [n1 n2]
    (let [[lo hi] (sort [n1 n2])
          m (mod hi lo)]
      (if (zero? m) lo
                    (recur lo m)))))

(defcheck solution-aa8bc64b
  (fn gcd [a b]
    (cond
      (> a b) (gcd (- a b) b)
      (< a b) (gcd a (- b a))
      :else a)))

(defcheck solution-aa8df8f9
  (fn gcd
    [x y]
    (if (zero? y) x
                  (gcd y (mod x y)))))

(defcheck solution-aaccfc86
  (fn mgcd
    [a b]
    (let [m (min a b)]
      (loop [n m]
        (if (= 0 (rem a n) (rem b n))
          n
          (recur (dec n)))))))

(defcheck solution-aaff1f0a
  (fn gcd
    [a b]
    (loop [result 1
           n result]
      (if (or (> n a) (> n b))
        result
        (if (and (zero? (mod a n)) (zero? (mod b n)))
          (recur n (inc n))
          (recur result (inc n)))))))

(defcheck solution-ab05dfbe
  (fn [a b] (loop [a a b b] (cond (= a 0) b (= b 0) a (< a b) (recur a (mod b a)) (< b a) (recur (mod a b) b)))))

(defcheck solution-ab28c591
  #(if (= %2 0) % (recur %2 (mod % %2))))

(defcheck solution-ab78d642
  (fn [x, y]
    (loop [m x n y]
      (if (= 0 (mod m n))
        n
        (recur n (mod m n))))))

(defcheck solution-aba7b94a
  (fn [a b]
    (if (= a b)
      a
      (recur (min a b) (Math/abs (- a b))))))

(defcheck solution-ac127b9f
  (fn f [a b] (cond (> a b) (f (- a b) b) (< a b) (f a (- b a)) :else a)))

(defcheck solution-acf093e6
  (fn [a b] (if (= b 0) a (recur b (mod a b)))))

(defcheck solution-acfe87ad
  (fn gcd[a b]
    (if (< a b)
      (gcd b a)
      (let [r (mod a b)]
        (if (= r 0)
          b
          (gcd b r))
        )
      )))

(defcheck solution-ae761de
  (fn gcd[a,b] (if (zero? b) a
                             (recur b (mod a b)))))

(defcheck solution-af94ef1f
  (fn gcd [a b] (if (> b a ) (gcd b a)  (if (= b a) a (gcd b (- a b))))))

(defcheck solution-afc845b8
  (fn [a b]
    (loop [a a b b]
      (if (zero? b)
        a
        (recur b (mod a b))))))

(defcheck solution-afe85151
  #(let [x (max % %2) y (min % %2)]
     (if (= 0 (mod x y)) y
                         (recur y (mod x y)))))

(defcheck solution-b071dba5
  (fn [m n] (first (filter #(and (zero? (mod m %)) (zero? (mod n %)))
                     (range (min m n) 0 -1)))))

(defcheck solution-b0c72270
  (fn [x y]
    (->> (range 1 (inc (min x y)))
      (filter #(= [0 0] [(mod x %) (mod y %)]))
      last)))

(defcheck solution-b0e06244
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))
    ))

(defcheck solution-b171f71e
  (fn gcd [a b]
    (if (< a b)
      (gcd b a)
      (loop [a a b b]
        (let [q (/ a b)
              r (rem a b)]
          (if (or (zero? r) (= a (+ (* q b) r)))
            b
            (recur b r)))))))

(defcheck solution-b1746f16
  (fn gcd
    ([a b]
     (gcd (sort [a b])))
    ([[a b]]
     (if (> b a) (gcd a (- b a)) a))))

(defcheck solution-b18ed0bf
  (fn [m n]
    (reduce
      (fn [md c]
        (if (= (+ (mod m c) (mod n c)) 0)
          c
          md))
      1
      (range 1 (inc (min m n))))))

(defcheck solution-b19de44c
  #(loop [a (max %1 %2) b (min %1 %2)]
     (let [m (mod a b)]
       (if (> m 0)
         (recur b m)
         b
         )
       )
     ))

(defcheck solution-b1c6da66
  (fn sieve [a b]
    (let [big (max a b)
          small (min a b)
          remainder (mod big small)]
      (if (= 0 remainder)
        small
        (sieve small remainder)))))

(defcheck solution-b227420e
  (fn [a b]
    (if (= 0 b) a
                (recur b (rem a b)))))

(defcheck solution-b241463b
  (fn [x y]
    (let [a (max x y) b (min x y) c (- a b)]
      (if (= (mod a c) (mod b c) 0)
        c
        (recur b c)))))

(defcheck solution-b270f01f
  (fn f [x y] (if (= x y) x (if (> x y) (f (- x y) y) (f (- y x) x)))))

(defcheck solution-b2f82d33
  (fn [a b] (apply max (filter #(zero? (+ (mod a %) (mod b %))) (range 1 (inc (min a b)))))))

(defcheck solution-b30f409f
  #(loop [a %1 b %2] (if (= a b) a (if (> a b) (recur b (- a b)) (recur a (- b a))))))

(defcheck solution-b31894cf
  (fn gcd [m n] (cond (= m n) m (< m n) (gcd (- n m) m) :else (gcd (- m n) n))))

(defcheck solution-b36b8b04
  (fn [a b] (some (fn [x] (if (every? #(= 0 (mod % x)) [a b]) x)) (range (min a b) 0 -1))))

(defcheck solution-b3720369
  (fn [a b]
    (first (filter #(and (= 0 (mod a %)) (= 0 (mod b %)))
             (range (min a b) 0 -1)))))

(defcheck solution-b3db3d4d
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-b46c06cf
  (fn gcd
    [x y]
    (let [[smaller larger] (if (< x y) [x y] [y x])]
      (loop [current smaller]
        (if (and (= (int (/ smaller current)) (/ smaller current))
                 (= (int (/ larger current))  (/ larger current)))
          current
          (recur (- current 1)))))))

(defcheck solution-b47b7011
  (fn gcd [n1 n2]
    (let [n (max n1 n2) d (min n1 n2) m (mod n d)]
      (if (zero? m) d (gcd n (- d m))))))

(defcheck solution-b47f9a24
  #(loop [a (max %1 %2)
          b (min %1 %2)]
     (if (zero? b)
       a
       (recur b (mod a b)))))

(defcheck solution-b4fccaa4
  (fn [x y]
    (last (filter #(and (zero? (mod x %)) (zero? (mod y %))) (range 1 (inc (min x y)))))))

(defcheck solution-b593acc1
  (fn [a b]
    (last
      (keep #(if (and (= 0 (mod a %1)) (= 0 (mod b %1))) %1 )
        (range 1 (inc (Math/min a b)))
        )
      )
    ))

(defcheck solution-b5cea231
  (fn gcd [a b]
    (cond
      (= a b) a
      (> a b) (recur (- a b) b)
      :else (recur a (- b a)))))

(defcheck solution-b5d01917
  #(let [m (min %1 %2)
         r (rem (max %1 %2) m)]
     (if (= 0 r) m (recur m r))))

(defcheck solution-b5d24894
  (fn gcd [a b]
    (cond
      (< a b) (recur b a)
      (= b 0) a
      :else (recur b (rem a b)))))

(defcheck solution-b5d3ec03
  (fn [a b]
    (cond (= a 0) b
          (= b 0) a
          :else (recur b (rem a b)))))

(defcheck solution-b63307e
  (fn gcd [a b]
    (if (= a b)
      a
      (if (< a b)
        (gcd a (- b a))
        (gcd b (- a b))))))

(defcheck solution-b68874fb
  (fn gcd
    [a b]
    (cond (or(= 0 a)(= 0 b)) 0
          (= a b) a
          (> a b)(recur (- a b) b)
          :else (recur a (- b a)))))

(defcheck solution-b75e40a1
  (fn [a b] (loop [a a b b] (if (= b 0) a (recur b (rem a b))))))

(defcheck solution-b78bebe9
  (fn gcd [a b]
    (let [result (rem (max a b) (min a b))]
      (if
       (= 0 result) (min a b)
                    (gcd (min a b) result)))))

(defcheck solution-b7f6ccef
  (fn [m n]
    (apply max
      (clojure.set/intersection
        (set (filter #(= 0 (rem m %)) (range 1 (inc m))))
        (set (filter #(= 0 (rem n %)) (range 1 (inc n))))))))

(defcheck solution-b83c04b4
  (fn f [a b]
    (if (= 0 b) a
                (f b (mod a b)))))

(defcheck solution-b8584e8b
  (fn gcd [a b]
    (if (< a b) (gcd b a)
                (loop [x a y b]
                  (if (= 0 (mod x y)) y
                                      (recur y (mod x y)))))))

(defcheck solution-b8686f89
  (fn __ [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-b8f1ee9d
  (fn [& args]
    (let [sorted (sort < args)]
      (->> (range (first sorted) 0 -1)
        (filter #(= 0 (mod (first sorted) %) (mod (last sorted) %)))
        first))))

(defcheck solution-b90d79f5
  (fn gcd [a b] (if (zero? b) a (recur b (mod a b)))))

(defcheck solution-b91305c5
  (fn [a b]
    (if (< a b)
      (recur b a)
      (if (= 0 (mod a b))
        b
        (recur b (mod a b))))))

(defcheck solution-b92383b1
  (fn [x y]
    (first
      (for [d (range (min x y) 0 -1) :when (and
                                            (zero? (rem x d))
                                            (zero? (rem y d))) ]
        d ))))

(defcheck solution-b974ccc9
  (fn [x y]
    (let [remainder (mod x y)]
      (if (zero? remainder)
        y
        (recur y remainder)))))

(defcheck solution-ba0d8b22
  (fn gcd [x y]
    (if (= y 0)
      x
      (gcd y (rem x y)))))

(defcheck solution-ba6d9450
  (fn [a b]
    (loop [x a
           y b]
      (cond
        (= x y) x
        (> x y) (recur (- x y) y)
        :else (recur (- y x) x)))))

(defcheck solution-baa6e0c2
  (fn [a b]
    (cond
      (= a b) a
      (> a b) (recur (- a b) b)
      :else (recur a (- b a)))))

(defcheck solution-bafb5128
  (fn gcd [a b]
    (let [divisors (fn [n] (->> (range 1 (inc n)) (filter #(zero? (mod n %))) (set)))]
      (apply max (clojure.set/intersection (divisors a) (divisors b))))))

(defcheck solution-bb02b82f
  (fn [a b] (last (filter (fn [i] (and (= 0 (mod a i)) (= 0 (mod b i)))) (range 1 (inc (max a b)))))))

(defcheck solution-bb5371dd
  (fn [a b]
    (if (= a b)
      b
      (recur (- (max a b) (min a b)) (min a b)))))

(defcheck solution-bc180b7f
  (fn [x y]
    (if (= 0 y) x (recur y (mod x y)))))

(defcheck solution-bc7df9e6
  #(first (for
           [i (range (if (> %1 %2) %1 %2) 0 -1)
            :when (and (= (rem %1 i) 0) (= (rem %2 i) 0))]
            i)))

(defcheck solution-bca938cb
  (fn [n1 n2]
    (loop [big n1 small n2]
      (if (zero? small)
        big
        (recur small (mod big small))))))

(defcheck solution-bcaa836d
  #(loop [a %1, b %2]
     (if (= 0 b)
       a
       (recur b (mod a b)))))

(defcheck solution-bce0c52c
  (fn [x y] (reduce (fn [a n] (if (= 0 (mod x n) (mod y n)) n a)) (range 1 (inc (min x y))))))

(defcheck solution-bcf8ed17
  (fn f[a b] (cond (> b a) (f b a) (= 0 b) a :else (f (mod a b) b ))))

(defcheck solution-be024ecb
  (fn [a b] (apply max (filter #(and (= (mod a %) 0) (= (mod b %) 0)) (range 1 (+ 1 (min a b)))))))

(defcheck solution-bea825d1
  (fn gcd [x y]
    (if (zero? x)
      y
      (gcd (mod y x) x))))

(defcheck solution-bec25c70
  #(if (> %2 %)
     (recur %2 %)
     (if (= %2 0)
       %
       (recur %2 (rem % %2)))))

(defcheck solution-bed58e3e
  (fn [a b]
    (if (zero? b)
      a
      (recur b (rem a b))
      )
    ))

(defcheck solution-bf1e6feb
  #(if
    (= 0 %2)
     %1
     (recur %2 (mod %1 %2))))

(defcheck solution-bf51d972
  (fn my-pgcd [x y] (let [a (max x y) b (min x y) r (rem a b)] (if (zero? r) b (my-pgcd a r)))))

(defcheck solution-bfac68bb
  #(cond
     (zero? %2) %
     :else (recur %2 (rem % %2))))

(defcheck solution-c032f20d
  (fn [x y]
    (if (zero? y) x
                  (recur y (rem x y)))))

(defcheck solution-c079aea4
  (fn gcd [& nums]
    (reduce #(if (= 0 %2)
               %
               (recur %2 (mod % %2))) nums)))

(defcheck solution-c097e911
  (fn euclid [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-c0d1dd1c
  (fn f [x n]
    (let [m (mod x n)]
      (if (zero? m) n
                    (f n m)))))

(defcheck solution-c0d8f5b4
  (fn mygcd [x y]
    (cond (= x 0) y
          (= y 0) x
          (< x y) (mygcd x (- y x))
          :else (mygcd (- x y) y))))

(defcheck solution-c12d3446
  (fn [a b] (letfn [(div [x] (flatten (keep #(when (= 0 (mod x %)) [% (quot x %)]) (range 1 (inc (Math/sqrt x))))))]
              (let [da (set (div a))
                    db (set (div b))
                    both (clojure.set/intersection da db)]
                (apply max both)))))

(defcheck solution-c135a58b
  (fn gcf [x y]
    (let [less (min x y)
          diff (Math/abs (- x y))]
      (cond (= x y) x
            (zero? (rem less diff)) diff
            true (gcf less diff)))))

(defcheck solution-c139619d
  (fn [a b] (last ( filter
                    #( and (zero? (rem a %)) (zero? (rem b %)))
                    (range 1 (max a b))))))

(defcheck solution-c166f7b7
  (fn gcd [a b] (if (< a b)
                  (if (= 0 (rem b a)) a (gcd (rem b a) a))
                  (if (= 0 (rem a b)) b (gcd (rem a b) b)))
    ))

(defcheck solution-c1c838af
  (fn gcd [x y]
    (if (= x y) x (gcd (- (max x y) (min x y)) (min x y)))))

(defcheck solution-c27cb8cd
  (fn gcd
    [a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(defcheck solution-c37ce68c
  (fn gcd [a b]
    (let [x (max a b)
          y (min a b)]
      (if (= 0 y)
        x
        (gcd b (rem a b))))))

(defcheck solution-c3c06512
  #(if (= 0 %)
     %2
     (recur (mod %2 %) %)))

(defcheck solution-c3fc9b30
  (fn gcd [a b]
    (cond (< a b) (gcd b a)
          (zero? b) a
          :else (gcd b (rem a b)))))

(defcheck solution-c4257a8e
  ;Hmm !
  (fn gcd [m n]
    (let [divsboth? (fn [x y d] (and (zero? (mod x d)) (zero? (mod y d))))]
      (first (reduce (fn [x y] (if (divsboth? m n y) (cons y x) x)) '() (range 1 (inc (min m n))))))))

(defcheck solution-c458a977
  (fn gcd [a b]
    (apply max
      (for [x (range 1 (max a b))
            :when (= 0 (rem a x) (rem b x))]
        x))))

(defcheck solution-c4cf21fc
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-c4d086bb
  (fn [a b]
    (let [c (- b a)]
      (if (zero? c)
        a
        (let [c (if (pos? c) c (- c))]
          (recur (min c a) (max c a)))))))

(defcheck solution-c5a634f3
  (fn [x1 x2]
    (loop [i (apply max [x1 x2])]
      (if (and (integer? (/ x1 i)) (integer? (/ x2 i))) i
                                                        (recur (dec i))))))

(defcheck solution-c5e3ef5
  (fn gcd [a b] (if (= b 0) a (gcd b (rem a b)))))

(defcheck solution-c64aff41
  (fn
    [v w]

    (loop [a v b w]
      (if
       (= b 0)
        a
        (recur b (mod a b))
        )
      )
    ))

(defcheck solution-c6ae5d8d
  (fn gcd [a b]
    (if (zero? b)
      a
      (gcd b (mod a b)))))

(defcheck solution-c7d7d3d9
  (fn gcd[x y] (if (zero? x) y (gcd (mod y x) x))))

(defcheck solution-c7dfa090
  (fn myCommonDivisor
    [x y]
    (if-not (= x y)
      (let [maxx (max x y) minn (min x y)]
        (myCommonDivisor (- maxx minn) minn))
      x)))

(defcheck solution-c83d37d8
  (fn [a b]
    (cond (< a b) (recur b a)
          (= b 0) a
          :else (recur b (- a b)))))

(defcheck solution-c83db8e7
  (fn gcd[a b]
    (if (= a b)
      a
      (if (> a b)
        (gcd (- a b) b)
        (gcd (- b a) a)))))

(defcheck solution-c8489da6
  (fn gcd [x y]
    (let [z (mod x y)]
      (if (zero? z)
        y
        (gcd y z)))))

(defcheck solution-c8bad810
  (fn gcd [a b]
    (if (= b 0) a
                (recur b (mod a b)))))

(defcheck solution-c8ca8652
  (fn gcd [a b]
    (cond
      (= a b)   a
      (= a 0)   b
      (= b 0)   a
      (even? a) (if (even? b)
                  (* 2 (gcd (/ a 2) (/ b 2)))
                  (gcd (/ a 2) b))
      (even? b) (gcd a (/ b 2))
      (> a b)   (gcd (/ (- a b) 2) b)
      :else     (gcd (/ (- b a) 2) a))))

(defcheck solution-c8fe8fda
  (fn common-divisor
    ([x y]
     (common-divisor x y (min x y)))
    ([x y z]
     (if (= z 1)
       1
       (if (every? #(= 0 (rem % z)) (list x y))
         z
         (recur x y (dec z)))))))

(defcheck solution-c91cb88f
  (fn g [a b] (cond (> a b) (g b a) (= 0 a) b :t (g a (rem b a)))))

(defcheck solution-c97657d8
  (fn [& l]
    (let [m (reduce min l)]
      (first
        (for [x (iterate dec m)
              :when (zero? (reduce #(+ %1 (rem %2 x)) 0 l))]
          x)))))

(defcheck solution-ca09be7c
  (fn gcd [a b]
    (if (or (= b 0) (= a b)) a
                             (if (< a b) (gcd b a)
                                         (gcd b (mod a b))))))

(defcheck solution-ca48a5bb
  (fn [x y]
    (cond
      (or (= x 1) (= y 1)) 1
      (> x y) (recur y (- x y))
      (< x y) (recur x (- y x))
      :else x)))

(defcheck solution-cae65db9
  (fn gcd [a b]
    (if (zero? b) a
                  (recur b (mod a b)))))

(defcheck solution-caeccd1b
  (fn [a b]
    (cond (= a b) a
          (> a b) (recur (- a b) b)
          :else (recur a (- b a)))))

(defcheck solution-cb12d415
  (fn gcd [& args]
    (let [[b a] (sort args)]
      (if (= 0 (mod a b))
        b
        (gcd b (mod a b))))))

(defcheck solution-cb5e2778
  (fn [a b]
    (let [x (min a b)
          y (max a b)]
      (letfn [(ld [v]
                (filter #(zero? (mod v %)) (reverse (range 1 (inc v)))))]
        (first (filter #(and (zero? (mod x %)) (zero? (mod y %))) (ld x)))
        ))))

(defcheck solution-cb5f385f
  (fn gcd [a b] (if (= b 0) a (gcd b (mod a b)))))

(defcheck solution-cbafde4c
  (fn gcd [a b] (cond (= a b) a
                      (< a b) (gcd a (- b a))
                      (> a b) (gcd (- a b) b))))

(defcheck solution-cbf10813
  (fn [x y]
    (last (filter #(= (rem x %) (rem y %) 0) (range 1 (max x y))))))

(defcheck solution-cc1f4d3
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-cc54ba4
  (fn ! [a b]
    (if (= a b) a
                (if (> a b)
                  (! (- a b) b)
                  (! a (- b a))
                  )
                )
    ))

(defcheck solution-cc7fb75a
  (fn [a b]
    (some #(if (= 0 (mod a %)
                 (mod b %)) %)
      (range (inc (min a b)) 0 -1))))

(defcheck solution-ccb075fd
  #(if (= % 0) %2 (recur (rem %2 %) %)))

(defcheck solution-cce0ae4a
  (fn gcd [a b] (let [a' (min a b), b' (max a b)] (if (= a' 0) b' (gcd (mod b' a') a')))))

(defcheck solution-cd173f96
  (fn [a b]
    (if
     (= b 0)
      a
      (recur b (rem a b)))))

(defcheck solution-cd2c94c0
  (fn gcd
    [a b]
    (cond
      (= a 0) b
      (= b 0) a
      :else (let [smaller (min a b)
                  larger (max a b)
                  remainder (rem larger smaller)]
              (gcd smaller remainder)))))

(defcheck solution-cd7d00c7
  (fn f [x y] (if (= y 0) x (f y (mod x y)))))

(defcheck solution-ce39b86a
  (fn [a b]
    (loop [c (max a b)
           d (min a b)
           r (rem c d)]
      (if (= r 0)
        d
        (recur d r (rem d r))))))

(defcheck solution-cefb13b
  (fn gcd [a b]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defcheck solution-cf6b1ee6
  (fn gcd [a b]
    (let [c (if (> a b) a b)
          d (if (> a b) b a)
          e (rem c d)]
      (if (= 0 e)
        d
        (gcd e d)))))

(defcheck solution-cf6d1d12
  (fn gcd[a b]
    (loop [n1 (max a b), n2 (min a b)]
      (let [m (mod n1 n2)]
        (if (= m 0)
          n2
          (recur n2 m)
          )
        )
      )
    ))

(defcheck solution-cfd6c19c
  (fn gcd [a b]
    (if (= a b)
      a
      (if (> a b)
        (gcd (- a b) b)
        (gcd a (- b a))
        )
      )
    ))

(defcheck solution-d0158484
  (fn gcd [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-d076580b
  (fn [x1 x2] (last (filter #(and (zero? (mod x1 %))
                                  (zero? (mod x2 %)))
                      (range 1 (inc x1))))))

(defcheck solution-d07b04d2
  #(if (= 0 %2)
     %
     (recur %2 (mod % %2))))

(defcheck solution-d08938bd
  (fn g [x y]
    (letfn [(gcd [x y]
              (if (= (rem x y) 0) y (gcd y (rem x y))))]
      (if (> x y)
        (gcd x y)
        (gcd y x)))))

(defcheck solution-d0c77027
  (fn pgcd [x y]
    (loop [a (if (> x y) x y)
           b (if (> x y) y x)]
      (let [r (mod a b)]
        (if (= r 0)
          b
          (recur b r))))))

(defcheck solution-d0f1c4ba
  (fn gcd
    [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defcheck solution-d0faf311
  (fn gcd [x y] (or (some #(if (= 0 (mod x %) (mod y %)) % false) (range (min x y) 1 -1)) 1)))

(defcheck solution-d0ffdf08
  (fn f [a b] (if (= 0 b) a (f b (mod a b)))))

(defcheck solution-d15410ae
  (fn [m n]
    (if (zero? m)
      n
      (recur (mod n m) m)
      )
    ))

(defcheck solution-d185b0ef
  (fn [x y]
    (loop [a (max x y) b (min x y )]
      ( if (= b 0)
        a
        (recur b (mod  a b ) )
        )
      )
    ))

(defcheck solution-d21d85d8
  (fn [x y]
    (cond (zero? x) y
          (zero? y) x
          (> x y) (recur y (rem x y))
          true    (recur x (rem y x)))))

(defcheck solution-d24d8011
  (fn gcd [a b]
    (->> (range 1 (+ 1 (min a b)))
      (filter #(and (= 0 (rem a %)) (= 0 (rem b %))))
      (apply max))
    ))

(defcheck solution-d271d09f
  (fn gcd [x y]
    (if (< y x)
      (gcd y x)
      (loop [remaining (range 2 (+ 1 x)) ans 1]
        (if (empty? remaining)
          ans
          (if (= 0 (rem x (first remaining)) (rem y (first remaining)))
            (recur (rest remaining) (first remaining))
            (recur (rest remaining) ans)))))))

(defcheck solution-d2acf1c8
  (fn f[a b]
    (if (= b 0) a (f b (rem a b)))))

(defcheck solution-d2c43424
  (fn[a b] (let[lo (min a b)]
             (some #(when (= 0 (mod a %) (mod b %)) %) (range lo 0 -1)))))

(defcheck solution-d2e96366
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (rem a b)))))

(defcheck solution-d356811
  #(if (= 0 %2) %
                (recur %2 (mod % %2))))

(defcheck solution-d37bdd6e
  (fn [& ns](apply min (apply concat (take-while #(not (zero? (first %))) (iterate (fn[[a b]] (list (rem b a) a)) (sort ns)))))))

(defcheck solution-d395fae4
  (fn gcd [x y]
    (if (zero? y) x
                  (gcd y (rem x y)))))

(defcheck solution-d446f280
  (fn gcd [a b]
    (let [c (rem a b)]
      (if (= 0 c)
        b
        (gcd b c)
        )
      )
    ))

(defcheck solution-d463c86e
  #(loop [a %1 b %2]
     (cond (< a b) (recur b a)
           (= b 0) a
           :else (recur (rem a b) b))))

(defcheck solution-d47ff6a1
  (fn [x y] (first (filter #(= 0 (mod x %) (mod y %)) (iterate dec (min x y))))))

(defcheck solution-d48eefe
  (fn [a b]
    (loop [x a y b]
      (if (< x y)
        (recur y x)
        (if (zero? y)
          x
          (recur y (rem x y)))))))

(defcheck solution-d4c498bb
  (fn [x y]
    (let [m (mod x y)]
      (if (= m 0)
        y
        (recur y m)))))

(defcheck solution-d516a7cb
  (fn [a b] (some #(when (and (= (rem b %) 0) (= (rem a %) 0)) %)
              (iterate dec (max a b)))))

(defcheck solution-d534342
  (fn  [x y]
    (loop [a x, b y]
      (cond (= 0 b) a
            :else (recur b (mod a b))))))

(defcheck solution-d5789c08
  (fn gcd [a b]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defcheck solution-d57e37c4
  #(let
    [x (max % %2), y (min % %2)]
     (if (= 0 (rem x y))
       y
       (recur (- x y) y))))

(defcheck solution-d5db7d8
  (fn gcd
    [& nums]
    (let [divisible? #(zero? (mod % %2))
          min-num ((comp first sort) nums)
          min-num-divisibles (filter (partial divisible? min-num) (take min-num (iterate #(inc %) 1)))]
      (letfn [(find-gcd-rec
                [min-nums init-nums]
                (if-let [tail-nums (seq init-nums)]
                  (if (divisible? (first tail-nums) (last min-nums))
                    (find-gcd-rec min-nums (rest tail-nums))
                    (find-gcd-rec (butlast min-nums) tail-nums))
                  (last min-nums)))]
        ;; invoke recursive func
        (find-gcd-rec min-num-divisibles nums)))))

(defcheck solution-d5fa1d83
  (fn gcd--euclid
    [m n] {:pre [(integer? m), (integer? n)]}
    (cond
      (neg? m)   (recur (- m) n)
      (neg? n)   (recur m (- n))
      (< m n)    (recur n m)
      (zero? n)  m
      :else      (recur n (mod m n)))))

(defcheck solution-d617397b
  (fn gcd [a b]
    (loop [a a b b]
      (if (= a 0)
        b
        (recur (mod b a) a)))))

(defcheck solution-d6a406ed
  (fn gcd [x y]
    (if (= x y)
      x
      (if (> x y)
        (recur (- x y) y)
        (recur x (- y x))))))

(defcheck solution-d7f2ef9b
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b))
      )
    ))

(defcheck solution-d7f5db3e
  (fn [n1 n2] (loop [g (max n1 n2) l (min n1 n2)]
                (if (= 0 (rem g l)) l
                                    (recur l (rem g l))
                                    ))))

(defcheck solution-d851768e
  (fn [a b]
    (case b
      0 a
      (recur b (mod a b)))))

(defcheck solution-d8685d96
  (fn g[u v]
    (if  (= u 0) v
                 (if (> u v)
                   (g (- u v) v)
                   (g (- v u) u)))))

(defcheck solution-d8c536d8
  (fn [x y]
    (loop [denominator (if (<= x y) x y)]
      (if (and (= 0 (rem x denominator))
               (= 0 (rem y denominator)))
        denominator
        (recur (dec denominator))))))

(defcheck solution-d8c988aa
  (fn f [a b] (cond (= a b) a (> a b) (f (- a b) b) :e (f b a))))

(defcheck solution-d8cd6192
  (fn [a b]
    (cond
      (= a 0) b
      :else (recur (mod b a) a))))

(defcheck solution-d8ee42b3
  #(if (zero? %2) %1,
                  (recur %2 (mod %1 %2))))

(defcheck solution-d9094ede
  (fn [a b]
    (loop [ap a bp b]
      (if (= bp 0)
        ap
        (recur bp (mod ap bp))))))

(defcheck solution-d995d2e9
  (fn [a b]
    (if (< a b)
      (recur b a)
      (if (= b 0)
        a
        (recur b (mod a b))))))

(defcheck solution-d9b3684
  (fn gcd [x y]
    (letfn [(findcd [n]
              (filter #(and (< % (inc n))
                            (= (rem n %) 0))
                (range 1 (inc n))))]
      (loop [l (min x y),
             r (max x y)
             lc (reverse (findcd l)),
             rc (reverse (findcd r))]
        (if (some (conj #{} (first lc)) rc)
          (first lc)
          (recur l r (rest lc) rc))))))

(defcheck solution-da2270c7
  (fn gcd [x y]
    (let [m (min x y)]
      (loop [m m]
        (if (and (= 0 (rem x m))
                 (= 0 (rem y m)))
          m
          (recur (dec m)))))))

(defcheck solution-dad17002
  (fn ds [x y]
    (let [a (min x y)
          b (max x y)]
      (first (for [d (range a 0 -1)
                   :when (and (zero? (mod a d))
                              (zero? (mod b d)))] d)))))

(defcheck solution-db0c1d9e
  (fn [a b] (some #(when (= 0 (rem a %) (rem b %)) %) (range (max a b) 0 -1))))

(defcheck solution-db3505ed
  #(first
     (drop-while (fn [x]
                   (not (and
                         (zero? (mod %1 x))
                         (zero? (mod %2 x)))))
       (range (inc (min %1 %2)) 0 -1))))

(defcheck solution-db3d93bb
  (fn [x y]
    (letfn [(smaller-bigger [a b]
              (if (<= a b)
                [a b]
                [b a]))]
      (loop [[x y] (smaller-bigger x y)
             current x]
        (if (or (= 1 current)
                (and (zero? (mod y current))
                     (zero? (mod x current))))
          current
          (recur [x y] (dec current)))))))

(defcheck solution-dc13bd15
  (fn gcd [a b]
    (if (= b 0) a
                (gcd b (rem a b)))))

(defcheck solution-dc18a0fa
  #(if (= 0 %2) %1 (recur %2 (mod %1 %2))))

(defcheck solution-dc4c8e41
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (mod a b)))))

(defcheck solution-dc66bbe9
  (fn [a b] (reduce #(if (and (= (mod a %2) 0) (= (mod b %2) 0)) %2 %1) 1 (range 1 (if (> a b) a b)))))

(defcheck solution-dcaac086
  (fn ! [a b]
    (if (= b 0) a (! b (rem a b)))))

(defcheck solution-dcbaa32c
  (fn [& as]
    (let [facs (fn [n] (set (keep #(if (= 0 (rem n %)) %) (range 1 (inc n)))))]
      (apply max (apply clojure.set/intersection (map facs as))))))

(defcheck solution-dd19b926
  #(loop [denom (if (> % %2) %2 %)]
     (if (and (integer? (/ % denom))
              (integer? (/ %2 denom)))
       denom
       (recur (dec denom)))))

(defcheck solution-dd2c3729
  (fn [a b]
    (first (filter (fn [x] (= (mod a x) (mod b x) 0) ) (iterate dec (min a b))))))

(defcheck solution-dd582ebd
  (fn [x y]
    (if (zero? y) x
                  (recur y (mod x y)))))

(defcheck solution-dd6a09f5
  (fn gcd [a b] (let [[a b] (if (> a b) [a b] [b a])] (if (zero? b) a
                                                                    (gcd b (rem a b))))))

(defcheck solution-dd9bafd9
  (fn gcd
    [a b]
    (let [tiny (min a b)
          biggy (max a b)
          options (range 1 (inc tiny))]
      (reduce #(if (= 0 (mod tiny %2) (mod biggy %2)) %2 %1) 1 options))))

(defcheck solution-dda9ffcc
  (fn [x y](let [a (max x y)
                 b (min x y)
                 s (reverse (range 1 (inc b)))]
             (first (filter #(and (= 0 (mod a %)) (= 0 (mod b %))) s))
             )))

(defcheck solution-dddbe0f
  (fn gcd [x y]
    (->> (range 1 (inc (min x y)))
      (filter #(and (= 0 (mod x %) (mod y %))))
      (last))))

(defcheck solution-de1e605b
  (fn f [x y]
    (if (= x y)
      x
      (f (Math/abs (- x y)) (min x y)))))

(defcheck solution-de25b64a
  (fn gcd [a_ b_]
    (last (let [a (max a_ b_)
                b (min a_ b_)]
            (for [i (drop 1 (range))
                  :while (and (<= i a) (<= i b))
                  :when (and (= 0 (rem a i)) (= 0 (rem b i)))
                  ]
              i)
            ))))

(defcheck solution-de49216f
  (fn gcd [x y]
    (let [m (min x y) M (max x y)]
      (if (zero? m)
        M
        (gcd (rem M m) m)))))

(defcheck solution-df9dbe98
  #(first (drop-while (fn [x] (not= (rem %1 x) (rem %2 x) 0)) (iterate dec (min %1 %2)))))

(defcheck solution-e0c631e2
  #(loop [a (rem % %2) y %2]
     (if (= a 0)
       y
       (recur (rem y a) a))))

(defcheck solution-e211b356
  (fn gcd [& nums]
    (let [[y x] (sort nums)
          r (rem x y)]
      (if (zero? r) y (recur [y r])))))

(defcheck solution-e2dc1752
  (fn gr-div [n1 n2]
    (let[fac (fn [n](filter #(zero? (rem n %)) (range 1 (inc n))))
         intersect (set (filter (set (fac n1)) (fac n2)))]
      (reduce max (set intersect)))))

(defcheck solution-e30874ad
  (fn
    [x y]
    (if (= 0 y)
      x
      (recur y (mod x y)))))

(defcheck solution-e34476dc
  (fn [a b]
    (first
      (reduce
        (fn [l n] (if (and (= 0 (rem a n)) (= 0 (rem b n))) (conj l n) l ))
        '() (range 1 (inc (min a b)))))))

(defcheck solution-e3747af3
  (fn gcd[x y]
    (cond
      (zero? y) x
      :else (gcd y (mod x y)))))

(defcheck solution-e40abdfd
  (fn gcd [a b]
    (if (= b 0)
      a
      (gcd b (rem a b)))))

(defcheck solution-e4a2e52b
  (fn gcd [x y] (if (= 0 x) y (gcd (rem y x) x))))

(defcheck solution-e549c17c
  (fn [ a b ]
    (let [ c (mod a b)]
      (if (= c 0) b (recur b c)))))

(defcheck solution-e57d4ff6
  (fn [n1 n2]
    (let [m1 (min n1 n2)
          en (range 1 (inc m1))
          rem_zero #(every? zero? [(rem n1 %) (rem n2 %)])
          ]
      (apply max (filter rem_zero en)))))

(defcheck solution-e596da99
  (fn commondiv [& n]
    (letfn [
            (buildrange [v] (range 1 (inc v)))
            (testmod [x y] (if (= 0 (mod x y)) y nil))
            (dividers [x] (remove nil? (map #(testmod x %) (buildrange x)) ))
            (intersect [& colls] (apply sorted-set(flatten(filter #(=(count %) (count colls))(partition-by identity(sort(apply concat colls)))))))
            ] (
                first(reverse(apply intersect (map dividers n)))
                )
              )
    ))

(defcheck solution-e5a742dd
  (fn [a b]
    (first (filter #(= 0 (+ (rem a %) (rem b %)))
             (range (min a b) 0 -1)))))

(defcheck solution-e5fd5bf0
  (fn[n m]
    (loop [m+ (min m n)
           n+ (max m n)]
      (if (= m+ n+)
        m+
        (recur (min (- n+ m+) m+) (max (- n+ m+) m+))))))

(defcheck solution-e6a16b6c
  (fn gcdRec [a b] (if(> b a) (gcdRec b a) (if (zero? b) a (gcdRec b (mod a b))))))

(defcheck solution-e6b069e6
  (fn [a b] (if (= a 0) b (recur (mod b a) a))))

(defcheck solution-e747f
  (fn [n m]
    (loop [gcd 1 i 1]
      (if (or (> i n) (> i m))
        gcd
        (if (and (= 0 (mod n i)) (= 0 (mod m i)))
          (recur i (inc i))
          (recur gcd (inc i)))))))

(defcheck solution-e749516
  (fn gcd
    [num1 num2]
    (loop [dividend (max num1 num2)
           divisor (min num1 num2)
           quotient (quot dividend divisor)
           remainder (rem dividend divisor)]
      (if (= 0 remainder)
        divisor
        (recur divisor remainder (quot divisor remainder) (rem divisor remainder))))))

(defcheck solution-e8073a18
  (fn [a b] (apply max (filter #(and (= 0 (rem a %)) (= 0 (rem b %))) (range 1 (max a b))))))

(defcheck solution-e8c28ab6
  (fn f [a b]
    (if (= b 0) a
                (f b (mod a b)))))

(defcheck solution-e95f25a0
  #(if (= % 0)
     %2
     (recur (mod %2 %) %)))

(defcheck solution-e9b832a3
  (fn gcd
    ([a b]
     (if (zero? b)
       a
       (gcd b (mod a b))
       )
     )
    ))

(defcheck solution-ea093af1
  (fn gcd [a b]
    (if (zero? (* a b))
      (+ a b)
      (if (> a b)
        (gcd (- a b) b)
        (gcd a (- b a))))))

(defcheck solution-ea3efd0a
  #(cond (< %1 %2) (recur %2 %1)
         (= 0 (mod %1 %2)) %2
         :else (recur %2 (mod %1 %2))))

(defcheck solution-ea58e020
  (fn gdc [a b]
    (let [[a b] [(min a b) (max a b)]]
      (if (= 0 (mod a b))
        a
        (recur a (- b a))))))

(defcheck solution-eaa92b5e
  (fn [x y] (if (= 0 y) x (recur y (mod x y)))))

(defcheck solution-eb310437
  (fn [a b]
    (let [[m n] (if (> a b) [a b] [b a])]
      (loop [m m n n] (if (zero? n) m (recur n (mod m n)))))))

(defcheck solution-eb3ebb3f
  (fn gcd [a b]
    (if (< a b) (gcd b a)
                (if (= b 0) a
                            (gcd b (mod a b))))))

(defcheck solution-eb3f0848
  #(if (zero? (mod %1 %2)) %2 (recur %2 (mod %1 %2))))

(defcheck solution-ed5ca2fa
  (fn pgcd [x y]
    (if (zero? y)
      x
      (pgcd y (mod x y)))))

(defcheck solution-ed6b7542
  (fn [a b]
    (let [low (min a b) options (range low 0 -1)]
      (some
        #(if (and (= 0 (mod a %)) (= 0 (mod b %)))
           %
           false)
        options))))

(defcheck solution-ed84c207
  #(loop [x (max % %2) y (min % %2)] (if (> (rem x y) 0) (recur y (rem x y)) y)))

(defcheck solution-ed9e17ff
  (fn [m n](if (= n 0)m(recur n (rem m n)))))

(defcheck solution-edf2d262
  #(loop [a %1 b %2 c (min a b)]
     (if (= (mod a c) (mod b c) 0)
       c
       (recur a b (dec c)))))

(defcheck solution-eea3b773
  #(apply max (filter (fn [x] (= 0 (mod %1 x) (mod %2 x))) (range 1 (+ 1 (min %1 %2))))))

(defcheck solution-ef4a57ca
  (fn gcd [x y]
    (loop [[x y] (reverse (sort [x y]))]
      (if (zero? y) x (recur [y (mod x y)])))))

(defcheck solution-ef544917
  (fn gdc [a r]
    (cond
      (< a r) (recur r a)
      (not= 0 (mod a r)) (recur r (mod a r))
      :ese r)))

(defcheck solution-efe7afdf
  (fn [x y ] (first (drop-while #(or (not= (rem x %) 0) (not= (rem  y %) 0)) (reverse (range 1  (inc (min x y))))))))

(defcheck solution-f08ed3
  (fn euclidean-algo [a b]
    (let [[smaller bigger] (if (< a b) [a b] [b a])]
      (if (= (mod bigger smaller) 0)
        smaller
        (euclidean-algo smaller (- bigger smaller))))))

(defcheck solution-f095f5ef
  (fn my-gcd
    [x y]
    (loop [m (max x y) n (min x y)]
      (if (zero? (rem m n))
        n
        (recur n (rem m n))))))

(defcheck solution-f0998284
  (fn [n1 n2]
    (if (= (rem n1 n2) 0)
      n2
      (recur
        n2
        (rem n1 n2)
        )
      )
    ))

(defcheck solution-f0d7984d
  (fn [a b]
    (first
      (first
        (filter (fn [[x y]] (= y 0))
          (iterate (fn [[x y]]
                     (if (>= x y)
                       [(- x y ) y]
                       [(- y x ) x]
                       )
                     )
            [a b]
            )
          )

        )
      )
    ))

(defcheck solution-f0f5adba
  (fn gcd [x y] (let [a (min x y) b (max x y) m (mod b a)] (if (= 0 m) a (gcd a m)))))

(defcheck solution-f1735e0b
  (fn [a b]
    (cond
      (zero? a) b
      (zero? b) a
      (> a b) (recur (- a b) b)
      (< a b) (recur a (- b a))
      :else (recur (- a b) b))))

(defcheck solution-f1e07706
  (fn greatest-common-divisor [x y]
    (let [mx (max x y)
          mn (min x y)]
      ((comp

        (partial apply max)

        (fn [v] (map (fn [x] (if (and (= 0 (rem mx x)) (= 0 (rem mn x))) x 0)) v))

        vec
        rest
        range
        inc)

       mn))))

(defcheck solution-f1f830e4
  (fn mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb)))))

(defcheck solution-f2acf7ca
  (fn [a b]
    (apply max
      (keep-indexed #(if (= 0 %2) (inc %1))
        (map #(+ (mod b %) (mod a %)) (range 1 (inc a)))))))

(defcheck solution-f2c88e13
  (fn gcd [a b]
    (if (zero? b) a
                  (recur b (mod a b)))))

(defcheck solution-f38c9cc6
  (fn gcd
    [a b]
    (cond
      (zero? (mod a b)) b
      (< a b) (gcd b a)
      :else (gcd b (mod a b)))))

(defcheck solution-f3d1261e
  #(loop [a %1 b %2]
     (if (zero? b)
       a
       (recur b (mod a b)))))

(defcheck solution-f49be23e
  (fn gcd [a b] (cond
                  (< a b) (gcd b a)
                  (zero? b) a
                  :else   (gcd b (rem a b)))))

(defcheck solution-f49e3e80
  (fn [a b]
    (if (= b 0)
      a
      (recur b (mod a b))
      )
    ))

(defcheck solution-f537df39
  (fn gcd [x y]
    (if (zero? y)
      x
      (recur y (rem x y)))))

(defcheck solution-f5969787
  (fn [x y]
    (let [start (min x y)
          pos-vals (reverse (range 1 (inc start)))
          x-vals (set (cons 1 (map #(/ x %) pos-vals)))
          y-vals (set (cons 1 (map #(/ y %) pos-vals)))]
      (reduce max (filter integer?
                    (clojure.set/intersection x-vals y-vals))))))

(defcheck solution-f5f595aa
  (fn [a b]
    (if (zero? (rem a b))
      b
      (recur b (rem a b)))))

(defcheck solution-f6d84765
  #(loop [n % d %2]
     (if (zero? d) n
                   (recur d (mod n d)))))

(defcheck solution-f78ee524
  (fn [x y]
    (if (= y 0) x
                (recur y (mod x y)))))

(defcheck solution-f7a5712
  (fn gcd[a b] (let [isabig? (>= a b)
                     x (if isabig? a b)
                     y (if isabig? b a)]
                 (loop [x x
                        y y]
                   (let [remainder (rem x y)]
                     (if (= remainder 0)
                       y
                       (recur y remainder)))))))

(defcheck solution-f7c2d71d
  (fn divisor
    [x y]
    (let [smaller (if (< x y) x y)
          larger (if (= x smaller) y x)
          largest-possible-divisor (if (>= (/ larger smaller) 2)
                                     smaller
                                     (if (> smaller 0) (int (float (/ smaller 2))) 0))
          possibles (reverse (range (inc largest-possible-divisor)))
          finder (fn [l s candidates]
                   (let [to-check (first candidates)
                         more (rest candidates)]
                     (if (and (zero? (rem l to-check))
                              (zero? (rem s to-check)))
                       to-check
                       (recur l s more))))]
      (finder larger smaller possibles))))

(defcheck solution-f7ce393a
  (fn gcd [a b] (if (< b a) (gcd b a)
                            (if (zero? a) b (gcd a (- b a)) )
                            )))

(defcheck solution-f7ea11d7
  (fn gcd [a b]
    (cond
      (> a b)   (gcd b a)
      ; now we can assume that b >= a
      (= a 0)   b
      :else     (gcd (mod b a) a))))

(defcheck solution-f807279a
  (fn [n1 n2]
    (let [m (min n1 n2)
          divides #(= 0 (mod %2 %1))
          divisors (filter #(and (divides % n1) (divides % n2)) (range m 0 -1))]
      (first divisors))))

(defcheck solution-f8127596
  (fn gcd [a b]
    (let [smaller (if (< a b) a b)
          larger  (if (< a b) b a)
          difference (- larger smaller)]
      (if (= difference 0)
        larger
        (gcd smaller difference)))
    ))

(defcheck solution-f85d7b5c
  #(last (for [x (rest (range (max % %2)))
               :when (and (= 0 (mod % x)) (= 0 (mod %2 x)))] x)))

(defcheck solution-f85e1e79
  #(loop [m %1 n %2] (if (zero? n) m (recur n (rem m n)))))

(defcheck solution-f864f76b
  (fn [a b]
    (let [divides? (fn [n i] (= 0 (rem n i)))
          set-divisors (fn [n] (set (filter #(divides? n %) (range 1 (inc n)))))]
      (apply max (clojure.set/intersection
                   (set-divisors a)
                   (set-divisors b))))))

(defcheck solution-f95cfadf
  #(loop [a % b %2]
     (if (zero? b)
       a
       (recur b (mod a b)))))

(defcheck solution-f98b21a
  (fn [n1 n2] (first (filter #(if (= 0 (mod n1 %) (mod n2 %) )%) (range (min n1 n2) 0 -1)))))

(defcheck solution-fa2cbfe2
  (fn [a b]
    (cond (> b a) (recur b a)
          (zero? (rem a b)) b
          :else (recur (- a b) b))))

(defcheck solution-fa948878
  (fn [a b]
    (some #(when (and (zero? (mod a %))
                      (zero? (mod b %)))
             %)
      (iterate dec (int (/ (max a b) 2))))))

(defcheck solution-faaf0d8a
  (fn [x y]
    (last
      (remove #(nil? %)
        (for [q (map inc (range))
              :let [found (= 0 (+ (mod x q) (mod y q)))]
              :while (<= q (min x y))]
          (when found q))))))

(defcheck solution-fac15c22
  (fn [x y]
    (if (= 0 y)
      x
      (recur y (mod x y)))))

(defcheck solution-fb6680b3
  (fn [x y]
    (let [divs (fn [a]
                 (set (filter #(zero? (mod a %)) (range 1 (inc a)))))
          divs-x (divs x)]
      (apply max (filter divs-x (divs y))))))

(defcheck solution-fb7b809f
  (fn [a b]
    ; Straight from clojure.contrib.math/gcd
    (loop [a (Math/abs a) b (Math/abs b)]
      (if (zero? b) a,
                    (recur b (mod a b))))))

(defcheck solution-fbb3ebd2
  (fn [a b]
    (cond (= a b) a
          (< a b) (recur (- b a) a)
          :else (recur (- a b) b))))

(defcheck solution-fbd1a51e
  (fn [a b]
    (first (filter #(and (= 0 (mod a %)) (= 0 (mod b %)))
             (-> (min a b) (range 0 -1))))))

(defcheck solution-fbfe5cda
  (fn [a b]
    (cond
      (= a b) a
      (> a b) (recur (- a b) b)
      :else (recur a (- b a)))))

(defcheck solution-fc4bd13a
  (fn gcd [m n] (cond (> m n) (gcd (- m n) n)
                      (< m n) (gcd m (- n m))
                      :else n)))

(defcheck solution-fc636b9c
  (fn gcd [a b]
    (if (= b 0)
      a
      (recur b (rem a b)))))

(defcheck solution-fc872f40
  (fn gcd [a b]
    (if (> a b) (gcd b a)
                (if (= 0 (rem b a))
                  a
                  (gcd (- b a) a)))))

(defcheck solution-fc94e7a6
  (fn f [x y] (let [[a b] (sort [x y])] (if (= a 0) b (f a (- b a))))))

(defcheck solution-fd2fe1e8
  (fn [a b]
    (first
      (filter
        #(and
          (= 0 (rem a %))
          (= 0 (rem b %)))
        (reverse (range (inc (max a b))))))))

(defcheck solution-fd44227a
  (fn [a b]
    (cond
      (> a b) (if (zero? (mod a b)) b (recur b (mod a b)))
      (< a b) (recur b a)
      :else a)))

(defcheck solution-fd988c76
  (fn [a,b]
    (reduce #(if (and (= 0 (rem a %2)) (= 0 (rem b %2)))
               %2
               %1) (range 1 (inc (min a b))))
    ))

(defcheck solution-fe8119e5
  (fn [a b] (loop [a a b b] (if (not= b 0) (recur b (mod a b)) a))))

(defcheck solution-fe87042b
  (fn gcd [x y]
    (first (filter #(= 0 (mod x %) (mod y %)) (range y 0 -1)))))

(defcheck solution-fe955837
  (fn [& lst]
    (loop [i (apply min lst)]
      (if (every? #(= (mod % i) 0) lst)
        i
        (recur (dec i))))))

(defcheck solution-fedc0154
  (fn gcd [a b]
    (cond
      (= b 0) a
      (> a b) (gcd b (- a b))
      :else (gcd a (- b a)))))

(defcheck solution-ff0e6049
  (fn f [x y] (if (= x y) x (f (min x y) (Math/abs (- x y))))))

(defcheck solution-ffe4cc55
  (fn gcd [a b] (if (= a b) a (if (> a b) (recur (- a b) b) (recur a (- b a))))))

(defcheck solution-fff9c9f6
  (fn [x y]
    (loop [a x b y]
      (if(zero? b) a
                   (recur b (rem a b))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-66))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
