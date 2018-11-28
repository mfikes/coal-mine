(ns coal-mine.problem-34
  (:require [coal-mine.checks :refer [defcheck-34] :rename {defcheck-34 defcheck}]
            [clojure.test]))

(defcheck solution-100ba444
  (fn
    [s e]
    (loop [rs [] cur s]
      (if (< cur e)
        (recur (conj rs cur) (inc cur))
        rs))))

(defcheck solution-1068975a
  (fn my-range [from to]
    (loop [result '()
           current from]
      (if (= current to)
        result
        (recur (concat result (list current)) (inc current))))))

(defcheck solution-112e12be
  (fn [start end]
    (letfn [(range-helper [start end lst]
              (if (= start end)
                lst
                (range-helper (inc start) end (conj lst start))))]
      (range-helper start end []))))

(defcheck solution-114a333e
  #(for [x (iterate inc %1) :while (< x %2)] x))

(defcheck solution-1160ef94
  #( take (- %2 %1) (iterate inc %1)))

(defcheck solution-120bceed
  (fn [x y] (take (- y x) ((fn twixt [z] (cons z (lazy-seq (twixt (inc z))))) x))))

(defcheck solution-134d2f3e
  (fn homehome [start end] (if (= start end) () (cons start (homehome (inc start) end)))))

(defcheck solution-140a7a6e
  (fn [s e]
    (loop [i s rs []]
      (if (>= i e)
        rs
        (recur (inc i) (concat rs [i]))
        ))))

(defcheck solution-1595847d
  (fn [s e] (map-indexed #(+ %1 %2) (repeat (+ e (- 0 s)) s))))

(defcheck solution-15a9ec8b
  (fn [start end] (loop [result [] s start ]
                    (if (= s end) result
                                  (recur (conj result s) (inc s) )))))

(defcheck solution-1689b5f8
  (fn [beg end] (take-while #(< % end) (iterate inc beg))))

(defcheck solution-1698eb2c
  #(loop [n (dec %2) coll '()]
     (if (= n %)
       (cons n coll)
       (recur (dec n) (cons n coll))
       )
     ))

(defcheck solution-1777964c
  (fn [start end] (take (- end start) (iterate inc start))))

(defcheck solution-1777b44f
  (fn [start end]
    (take-while #(< % end) (iterate inc start))))

(defcheck solution-17b352ed
  #(loop [v %1 result []]
     (if (< v %2)
       (recur (inc v) (conj result v))
       result)))

(defcheck solution-183ba04d
  (fn [start stop]
    (loop [coll []
           start start]
      (if (< start stop)
        (recur (conj coll start) (+ start 1))
        coll))))

(defcheck solution-183ca854
  (fn f[a b]
    (if (= a b) nil
                (lazy-seq (cons a (f (inc a) b))))))

(defcheck solution-18e6f64d
  (fn [start end]
    (loop [vectr []
           i start]
      (if (= i end)
        (seq vectr)
        (recur (conj vectr i) (inc i))))))

(defcheck solution-190414ce
  (fn [a b] (take-while #(distinct? b %) (iterate inc a))))

(defcheck solution-1918332
  (fn r [l h] (if (= l h) nil (cons l (r (+ l 1) h)))))

(defcheck solution-196e5815
  (fn my-range [start end]
    (if (= start end)
      nil
      (cons start (my-range (inc start) end)))))

(defcheck solution-19819b40
  (fn my-range [lower upper]
    (loop [cnt lower l '[]]
      (if (= cnt upper)
        l
        (recur (inc cnt) (conj l cnt))))))

(defcheck solution-19e9f697
  (fn f [x end]
    (if (= x end)
      (list)
      (cons x (f (inc x) end)))))

(defcheck solution-19fbfd57
  (fn
    [start end]
    (loop [current start
           accum ()]
      (if (= current end)
        (reverse accum)
        (recur (+ current 1) (conj accum current))))))

(defcheck solution-1af467da
  (fn newrange [m n]
    "Creates a vector of integers from m to n, including m, excluding n."
    (loop [counter m, result []]
      (if (< counter n)
        (recur (inc counter) (conj result counter))
        result))))

(defcheck solution-1b1c7f66
  (fn [s e] (loop [v s, rc []]
              (if (>= v e) rc
                           (recur (inc v) (conj rc v)) )
              )))

(defcheck solution-1b54fcca
  (fn [x y] ((fn [x y coll] (if (= x y) coll (recur (inc x) y (conj coll x)))) x y [])))

(defcheck solution-1b6556f7
  (fn [frst lst]
    (take-while #(< % lst) (iterate inc frst))
    ))

(defcheck solution-1b8691a9
  (fn myRange [x y] (take (- y x) (iterate inc x))))

(defcheck solution-1ba743d5
  (fn [s e]
    (loop [c s r []]
      (if (= c e)
        r
        (recur (inc c) (conj r c))))))

(defcheck solution-1bb4fd4e
  (fn [start end]
    (reverse
      (last
        (take-while #(< (first %) end)
          (iterate #(conj % (inc (first %)))
            (list start)))))))

(defcheck solution-1bb57767
  (fn [n l]
    (take (- l n)
      ((fn lazy-fib [a]
         (cons a
           (lazy-seq
             (lazy-fib (+ a 1)))))
       n))))

(defcheck solution-1c3f285d
  (fn
    [start finish]
    (take (- finish start) (iterate #(inc %) start))))

(defcheck solution-1c46af97
  (fn [a b]
    (reverse (loop [x a ans '()]
               (if (= x b)
                 ans
                 (recur (inc x) (conj ans x)))))))

(defcheck solution-1c8430d0
  #(loop [i (dec %2) v ()]
     (if (>= i %)
       (recur (dec i) (conj v i))
       v)))

(defcheck solution-1ca85642
  (fn r [low high]
    (if (< low high)
      (cons low (r (inc low) high))
      )
    ))

(defcheck solution-1d3a84bd
  #(take (- %2 %1)
     (iterate inc %1)))

(defcheck solution-1de1f021
  (fn [low high]
    (loop [x low xs []]
      (if (= x high)
        xs
        (recur (inc x) (conj xs x))))))

(defcheck solution-1e979860
  (fn [s e] (take (- e s) (iterate inc s))))

(defcheck solution-1e997f9c
  #(loop [i %1 out []] (if (< i %2) (recur (inc i) (conj out i) ) out )))

(defcheck solution-1f3be625
  (fn from-to [start stop]
    (take (- stop start)
      (iterate inc start))))

(defcheck solution-1f6e0e35
  (fn [start end]
    (loop [i (dec end) r '()]
      (if (> start i)
        r
        (recur (dec i) (conj r i))))))

(defcheck solution-1fc8bf69
  (fn [m n]
    (loop [c m
           l []]
      (if (= c n)
        l
        (recur (inc c) (conj l c))))))

(defcheck solution-1ff6f4a4
  (fn [s, e]
    (loop [i s r []]
      (if (< i e)
        (recur (inc i) (conj r i))
        r))))

(defcheck solution-2007c629
  (fn [min max]
    (take (- max min) (iterate inc min))))

(defcheck solution-2015f91
  (fn [from max]
    (loop [f from, s []]
      (if (>= f max)
        s
        (recur (inc f) (conj s f))))))

(defcheck solution-203d4131
  (fn [lower upper]
    (take-while #(< % upper)
      (iterate inc lower))))

(defcheck solution-20f59b94
  (fn f [x y] (when (< x y) (conj (f (+ x 1) y) x))))

(defcheck solution-210e2fc4
  (fn [n m]
    (loop [n n m m result []]
      (if (>= n m) result
                   (recur (inc n) m (conj result n))))))

(defcheck solution-22544bda
  (fn[lower upper] (take (- upper lower) (iterate inc lower))))

(defcheck solution-2359165e
  (fn my-range [a b]
    (if (= a b)
      '()
      (flatten (list a (my-range (inc a) b))))))

(defcheck solution-247b9d6f
  (fn notrange [b e]
    (if (< b e)
      (cons b (notrange (+ b 1) e))
      ())))

(defcheck solution-24f95eae
  (fn f [from upto]
    {:pre (<= from upto)}
    (loop [n from, acc []]
      (if (< n upto)
        (recur (inc n) (conj acc n))
        acc))))

(defcheck solution-2553c4b6
  (fn r [b e] (if (< b e) (lazy-seq (cons b (r (inc b) e))))))

(defcheck solution-25c47f65
  (fn myrange [s e]
    (if (< s e)
      (cons s (myrange (inc s) e))
      [])))

(defcheck solution-25d01228
  (fn r [x y]
    (when (< x y)
      (cons x (r (inc x) y)))))

(defcheck solution-265e3454
  (fn f [x y]
    (if (= x y)
      '()
      (conj (f (+ x 1) y) x))))

(defcheck solution-26e3731
  (fn f ([l r]
         (f l r []))
    ([l r t]
     (if (= l r)
       t
       (f (inc l) r (conj t l))))))

(defcheck solution-27778e04
  (fn [x y] (loop [x1 x, acc []] (if (= x1 y) acc (recur (inc x1) (conj acc x1))))))

(defcheck solution-27872c88
  (fn [b e] (take-while (partial > e) (iterate inc b))))

(defcheck solution-27b9202d
  #(loop [x %1 y []] (if (not= x %2)
                       (recur (inc x) (conj y x))
                       y
                       )))

(defcheck solution-27cc18e1
  (fn [x1 y1]
    (loop [x x1, ans []]
      (if (< x y1)
        (recur (inc x) (conj ans x))
        ans))))

(defcheck solution-27d82022
  #(loop [i %1 sq []]
     (if (= %2 i)
       sq
       (recur (inc i) (conj sq i)))))

(defcheck solution-287fd68e
  (fn [lower upper]
    (loop [n (dec upper) r '()]
      (if (= n (dec lower))
        r
        (recur (dec n) (conj r n))))))

(defcheck solution-294b6fb0
  #(loop [cur %1, result []]
     (if (= cur %2)
       result
       (recur (+ cur 1) (conj result cur)))))

(defcheck solution-2982af24
  (fn [start end]
    (take (- end start)
      (iterate inc start))))

(defcheck solution-29be7d27
  (fn rng [start stop]
    (cond
      (= start stop) ()
      :else (cons start (rng (inc start) stop))
      )
    ))

(defcheck solution-29c25a9d
  (fn [from to]
    (take-while (complement #(= % to)) (iterate inc from))))

(defcheck solution-29d80914
  (fn [beg end]
    (letfn [(rng [n result]
              (if (= n end)
                result
                (recur (inc n) (conj result n))))]
      (rng beg []))))

(defcheck solution-2a1ac013
  (fn my-range
    ([start, end] (my-range start end []))
    ([cur, end, res]
     (if (= cur end)
       res
       (my-range (inc cur) end (conj res cur))
       ))
    ))

(defcheck solution-2a475733
  (fn r [from to] (if (= from to) nil (cons from (r (+ 1 from) to)))))

(defcheck solution-2abc7c83
  (fn [start end]
    (loop [i start acc ()]
      (if (= i end)
        (reverse acc)
        (recur (+ i 1) (cons i acc))))))

(defcheck solution-2aed7c2f
  (fn [a b] (loop [x a out []] (if (= x b) out (recur (inc x) (concat out (list x)))))))

(defcheck solution-2bc951f1
  (fn [start end]
    (let [f (fn [cur acc]
              (if (< cur end)
                (recur (inc cur) (conj acc cur))
                acc))]
      (f start []))))

(defcheck solution-2be2654a
  (fn fib
    ([s e] (fib s e []))
    ([s e result]
     (if (>= s e)
       result
       (recur (inc s) e (conj result s)))
     )))

(defcheck solution-2bfe1581
  (fn [start end]
    (take-while #(< % end) (iterate inc start))))

(defcheck solution-2c233e49
  #(loop [n %1, acc '()]
     (if (= n %2)
       acc
       (recur (inc n) (concat acc (list n))))))

(defcheck solution-2c297055
  (fn f1 [x y]
    (if (= x (dec y))
      [x]
      (reduce conj [x] (f1 (inc x) y))
      )
    ))

(defcheck solution-2c8904f1
  (fn impl-range [start end]
    (loop [i start result []]
      (if (= i end)
        (list* result)
        (recur (inc i) (conj result i))))))

(defcheck solution-2cc22e12
  (fn rng[a b]
    (let  [diff (- b a)]
      (map
        +
        (repeat diff (- a 1))
        (reductions + (repeat diff 1))))))

(defcheck solution-2d004f54
  (fn [start limit]
    (loop [e start, limit limit, res []]
      (if (= e limit)
        res
        (recur (inc e) limit (conj res e))))))

(defcheck solution-2d39ba66
  (fn
    [start end]
    (loop [iterator start list '()]
      (if (< iterator end)
        (recur (inc iterator) (conj list iterator))
        (reverse list))
      )
    ))

(defcheck solution-2d482ec3
  (fn [begin end]
    (map (fn [[a _]] (+ begin a))
      (map-indexed list (repeat (- end begin) nil)))))

(defcheck solution-2e201a8d
  (fn range* [a b]
    (take (- b a) (iterate inc a))))

(defcheck solution-2e2f9988
  #(take (- %2 %1)(iterate inc %1)))

(defcheck solution-2e638b7b
  (fn myRange [from to]
    (loop [i (- to 1) r '()]
      (if (< i from)
        r
        (recur (dec i) (conj r i))
        )
      )
    ))

(defcheck solution-2e7153be
  (fn
    [start end]
    (letfn [(step [start end]
              (if
               (< start end)
                (cons
                  start
                  (step (+ start 1) end))))]
      (step start end))))

(defcheck solution-2e846a9c
  (fn f [start end]
    (if (< start end)
      (conj (f (inc start) end) start)
      nil)))

(defcheck solution-2fa30dbb
  (fn [start finish]
    (loop [ans '() s start]
      (if (= s finish)
        (reverse ans)
        (recur (conj ans s) (inc s))))))

(defcheck solution-2fb6a574
  #(loop [lst [] curr %1 end %2] (if (< curr end) (recur (conj lst curr) (inc curr) end) lst)))

(defcheck solution-2fc1c835
  (fn [x y] (take (- y x) (iterate inc x))))

(defcheck solution-2ff5b8cf
  (fn [x y] (loop [current x res []] (if (= current y) res (recur (inc current) (conj res current))))))

(defcheck solution-30740610
  #(loop [coll [] i %1] (if (>= i %2) coll (recur (conj coll i) (inc i)))))

(defcheck solution-30fa7bfe
  (fn [fst lst]
    (loop [fst fst
           lst lst
           acc []]
      (if (= fst lst)
        acc
        (recur (inc fst) lst (conj acc fst))))))

(defcheck solution-314671dd
  (fn [start finish]
    (take (- finish start) (iterate #(+ 1 %) start))))

(defcheck solution-31525558
  (fn [start end] (loop [n start xs []] (if (= n end) xs (recur (inc n) (conj  xs n))))))

(defcheck solution-31a076f1
  (fn [x,y] (take-while #(< % y) (iterate inc x))))

(defcheck solution-31aef792
  (fn [st ed]
    (loop [i st res []]
      (if (>= i ed)
        res
        (recur (+ 1 i) (conj res i))))))

(defcheck solution-320f886d
  (fn [s e] (reverse (loop [ acc () cur s ]
                       (if (= cur e) acc (recur (conj acc cur ) (inc cur)))))))

(defcheck solution-323e424a
  (fn r [f t] (if (< f t) (cons f (r (+ f 1) t)))))

(defcheck solution-32a9f333
  (fn this [a b]
    (when (< a b)
      (cons a (this (inc a) b)))))

(defcheck solution-32e0351b
  (fn [i y] (loop [head [] tail (replicate (- y i) i)]
              (if (empty? tail)
                (concat head tail)
                (recur (conj head (first tail))
                  (map inc (rest tail)))))))

(defcheck solution-33891a33
  (fn recrange [x y]
    (if (= x y) '()
                (concat (list x) (recrange (inc x) y)))))

(defcheck solution-340647b7
  (fn rng [a b]
    (if
     (= (inc a) b)
      (list a)
      (conj (rng (inc a) b) a))))

(defcheck solution-34931b8e
  (fn my-range [start end]
    (if (= start end)
      '()
      (cons start (my-range (inc start) end)))))

(defcheck solution-34bec9ac
  (fn my-range [a b]
    (if (= a b) [] (conj (my-range a (dec b)) (dec b)))))

(defcheck solution-3513d476
  (fn myrange [b e]
    (if (>= b e)
      '()
      (cons b (myrange (inc b) e)))))

(defcheck solution-35269af3
  (fn f [a b] (when (not= a b)
                (cons a (lazy-seq (f (inc a) b)))
                )))

(defcheck solution-3548c428
  #(take (- %2 %1) (iterate (fn [x] (+ x 1)) %1)))

(defcheck solution-355bb59
  (fn my-range
    ([st end] (my-range st end ()))
    ([st end acc]
     (if (= st end)
       acc
       (my-range st (dec end) (conj acc (dec end)))))))

(defcheck solution-3643b1b0
  (fn my-range [x y]
    (if (>= x y)
      ()
      (cons x (my-range (inc x) y)))))

(defcheck solution-36704f8e
  (fn f [l h] (when (< l h) (cons l (f (inc l) h)))))

(defcheck solution-368948dc
  #(take (- %2 %1)(iterate (partial + 1) %1)))

(defcheck solution-36a8e1c3
  (fn rng [start end]
    (loop [i (inc start)
           result [start]]
      (if (= i end)
        result
        (recur (inc i) (conj result i))))))

(defcheck solution-36d8f92c
  (fn my-range
    ([i f] (my-range (inc i) f [i]))
    ([i f resp]
     (if (>= i f)
       resp
       (my-range (inc i) f (conj resp i))))))

(defcheck solution-376330cc
  (fn [f t] (take (- t f) (iterate inc f))))

(defcheck solution-39413182
  (fn [l h]
    (loop [r '() i (dec h)]
      (if (< i l)
        r
        (recur (conj r i) (dec i))))))

(defcheck solution-39883230
  (fn ran [lo hi]
    (if (< lo hi)
      (cons
        lo
        (lazy-seq (ran (inc lo) hi)))
      nil)))

(defcheck solution-3991b514
  (fn [f t]
    (take-while
      #(< % t)
      (iterate inc f))))

(defcheck solution-39a4ccb3
  (fn x [s m] (if (= s m) '() (cons s (x (inc s) m)))))

(defcheck solution-39d03d7a
  (fn
    [from to]
    (loop [li '[]
           prev from]
      (if
       (>= prev to)
        li
        (recur
          (conj li prev)
          (+ 1 prev))))))

(defcheck solution-3a0a314d
  (fn [start stop]
    (loop [n start, coll '()]
      (if
       (< n stop)
        (recur (inc n) (conj coll n))
        (reverse coll)
        ))))

(defcheck solution-3ae70e1d
  (fn my-range1
    [m n]
    (letfn [(my-range1' [m n]
              #_(print n)
              (when (>= n m)
                (conj (my-range1' m (dec n)) n)))]
      (reverse (my-range1' m  (dec n))))))

(defcheck solution-3ba25a7d
  (fn [start stop]
    (take-while #(< % stop) (iterate inc start))))

(defcheck solution-3bb47c64
  (fn [start end]
    (for [num (iterate inc start)
          :while (< num end)]
      num)))

(defcheck solution-3bdc3db7
  (fn new-range
    [a b]
    (if (= a (dec b)) [a] (cons a (new-range (inc a) b)))))

(defcheck solution-3c09f1e6
  (fn myrange [l u]
    (if (>= l u) nil
                 (conj (myrange (inc l) u) l))))

(defcheck solution-3d373437
  (fn
    [start stop]
    (loop [n start acc []]
      (if (= n stop)
        (seq acc)
        (recur (inc n) (conj acc n))))))

(defcheck solution-3d37e378
  (fn [a b] (nth (iterate #(conj % (inc (last %))) [a]) (- b a 1))))

(defcheck solution-3d7ad5f
  (fn[a,b](take (- b a) (iterate (partial + 1) a))))

(defcheck solution-3d7ff406
  (fn r [x y]
    (if (number? x) (r (vec (list x (+ x 1))) y)
                    (if (= (last x) (- y 1))
                      (lazy-seq x)
                      (r (conj x (+ 1 (last x))) y)))))

(defcheck solution-3dbe3b77
  (fn [x y] (take-while (fn [n] (not= n y)) (iterate inc x))))

(defcheck solution-3df1464c
  (fn [begin end]
    (take (- end begin) (iterate inc begin))))

(defcheck solution-3e53b03a
  #(map-indexed (fn [z,a] (+ z %)) (repeat (- %2 %) [])))

(defcheck solution-3edfabf0
  (fn rng [start end] (when (< start end) (cons start (rng (inc start) end)))))

(defcheck solution-3f159dd2
  (fn [min max]
    (loop [s min f (- max 1) ret '()]
      (if (> s f)
        ret
        (recur
          s
          (- f 1)
          (cons f ret)
          )
        )
      )
    ))

(defcheck solution-3f37e25c
  #(reverse
     (loop [coll ()
            start %1
            end %2]
       (if (< start end)
         (recur (conj coll start) (inc start) end)
         coll))))

(defcheck solution-3fbe534d
  #(letfn [(myrange [x y] (if (<= y x) (list) (conj (myrange (inc x) y) x)))]
     (myrange %1 %2)))

(defcheck solution-3fcbd66
  (fn aba [a b]
    (loop [col [] curr a]
      (if (= curr b)
        col
        (recur (conj col curr) (inc curr))))
    ))

(defcheck solution-3fd59f22
  (fn [a b] (take (- b a) (iterate inc a))))

(defcheck solution-40dae996
  (fn r [a b]
    (if(= a (- b 1))
      (list a)
      (conj (r (+ a 1) b) a)
      )
    ))

(defcheck solution-40f6a529
  (fn[lo hi](take-while #(< % hi) (iterate inc lo))))

(defcheck solution-410931ac
  (fn ! [x y]
    (if (= x y)
      '()
      (concat [x] (! (inc x) y)))))

(defcheck solution-42011aac
  (fn f [a b] (when (< a b) (cons a (f (inc a) b)))))

(defcheck solution-4236bd3d
  (fn [i n] (take-while #(> n %) (iterate inc i))))

(defcheck solution-426f15c9
  (fn [x y] (take-while #(> y %) (iterate inc x))))

(defcheck solution-434a1653
  (fn my-range [start end]
    (if (= start end)
      nil
      (cons start (my-range (inc start) end)))))

(defcheck solution-437d6503
  (fn [x y]
    (loop [i x r []]
      (if (= i y)
        r
        (recur (inc i) (conj r i))))))

(defcheck solution-43a780c1
  (fn incy [x n]
    (if (= x (- n 1))
      x
      (flatten (conj '() (incy (inc x) n) x )))))

(defcheck solution-43bb8a87
  (fn rng [x y]
    (if (= x y)
      '()
      (conj (rng (+ x 1) y) x)
      )
    ))

(defcheck solution-444e0eab
  #(loop [cur %1
          target %2
          result []]
     (if (= cur target)
       result
       (recur (inc cur) target (conj result cur)))))

(defcheck solution-444e1088
  (fn my-range [from to]
    (when (< from to)
      (concat [from] (my-range (inc from) to)))))

(defcheck solution-44b9d83b
  (fn rng [l u]
    (if (= l u)
      '()
      (cons l (rng (inc l) u)))))

(defcheck solution-45b6fd63
  (fn [start end]
    (take (- end start) (iterate inc start))))

(defcheck solution-470fbebd
  (fn f[a b] (if (= a b) (list) (cons a (f (inc a) b)))))

(defcheck solution-4739aeb8
  (fn [start end]
    (loop [x start
           acc '()]
      (if (>= x end)
        (reverse acc)
        (recur (inc x) (cons x acc))
        )
      )
    ))

(defcheck solution-47e3358d
  (fn dd [ from to]
    (loop [i from
           result []]
      (if (>= i to)
        result
        (recur (inc i) (conj result i))))))

(defcheck solution-484b9b4a
  #(loop [a % result []] (if (< a %2) (recur (inc a) (conj result a)) result)))

(defcheck solution-4a00a846
  (fn [s e] (loop [r [] c s] (if (= c e) r (recur (conj r c) (inc c))))))

(defcheck solution-4a13f78c
  (fn [s e]
    (take-while #(< % e) (iterate inc s))))

(defcheck solution-4ac83d48
  (fn rang[low high]
    (if (= low high)
      ()
      (cons low (rang (+ low 1) high)))))

(defcheck solution-4b46a023
  #(reverse
     (loop [from %1
            acc  ()]
       (if (= from %2) acc
                       (recur (inc from) (conj acc from))))))

(defcheck solution-4bbeb85b
  (fn generate-range [a b] (if (= a b) '() (cons a (lazy-seq (generate-range (inc a) b))))))

(defcheck solution-4bcdb13d
  (fn [from to]
    (take (- to from)
      (iterate inc from))))

(defcheck solution-4bd122bc
  (fn my-range [start end]
    (if
     (= start end) []
                   (concat [start] (my-range (inc start) end)))))

(defcheck solution-4bd9e344
  (fn [start end]
    (loop [index start acc []]
      (if (> end index) (recur (inc index) (conj acc index)) acc )

      )


    ))

(defcheck solution-4be6dc02
  (fn [a b]
    (loop [i a acc '()]
      (if (= i b)
        (reverse acc)
        (recur (inc i) (cons i acc))))))

(defcheck solution-4c231651
  #(loop[len (- %2 %1) r (list %1)]
     (if (= len 1) (reverse r)
                   (recur (dec len) (conj r (+ 1 (first r)))))))

(defcheck solution-4cc8c6b2
  (fn _range [start stop]
    (if (= start stop)
      '()
      (cons start (_range (+ 1 start) stop)))))

(defcheck solution-4cca0332
  (fn my-range [n m]
    (if (< n m)
      (lazy-seq (cons n (my-range (+ n 1) m)))
      '())))

(defcheck solution-4ce87a0b
  #(loop [coll [ ]     n  %1]
     (if  (= n  %2)
       coll
       (recur (conj coll n) (inc n)
         ))))

(defcheck solution-4d1b1e0c
  (fn [s e]
    (reverse (loop
              [
               xs '()
               x s]
               (if (< x e)
                 (recur (conj xs x) (inc x))
                 xs)))))

(defcheck solution-4d258bf2
  (fn [start end]
    (take-while #(< % end) (iterate inc start))
    ))

(defcheck solution-4d65249c
  (fn [start stop]
    (loop [out [] n start]
      (if (>= n stop)
        out
        (recur (conj out n) (inc n))))))

(defcheck solution-4d66c333
  (fn f [a b]
    (if (= a b)
      '()
      (cons a (f (inc a) b)))))

(defcheck solution-4dc91638
  (fn rg [a b] (if (< a b) (cons a (lazy-seq (rg (inc a) b))) ())))

(defcheck solution-4de06bcd
  (fn range2 [a b] (take (- b a) (iterate inc a))))

(defcheck solution-4e02deb8
  (fn [start finish]
    (take-while #(> finish %) (iterate inc start))))

(defcheck solution-4f255c15
  (fn get-range [start end]
    (cond
      (>= start end) '()
      :else (cons start (get-range (inc start) end)))))

(defcheck solution-4f3bdce7
  (fn my-range [from to]
    (lazy-seq (if (< from (dec to))
                (cons from
                  (my-range (inc from)
                    to))
                (list from)))))

(defcheck solution-4f4c51b3
  (fn my-range [a b]
    (map-indexed (fn [i e] (+ i e))
      (repeat (- b a) a))))

(defcheck solution-4fa06558
  (fn [a b]
    (loop [a a r []]
      (if (< a b)
        (recur (inc a) (conj r a))
        r))))

(defcheck solution-4fdbc184
  #(loop [a %1
          b %2
          acc '()]
     (if (< a b)
       (recur (+ 1 a) b (concat acc (list a)))
       acc
       )
     ))

(defcheck solution-4fed69a8
  (fn rng [start end]
    (if (= start (dec end))
      [start]
      (concat [start] (rng (inc start) end)))))

(defcheck solution-5050ca52
  (fn [a b]
    (loop [i a r []]
      (if (< i b)
        (recur
          (inc i)
          (conj r i)
          )
        r
        )
      )
    ))

(defcheck solution-5144b1cb
  (fn [start end]
    (loop [i start acc []]
      (if (= i end)
        acc
        (recur (inc i) (conj acc i))))))

(defcheck solution-5170255c
  (fn [from to]
    (loop [n from
           xs []]
      (if (< n to)
        (recur (inc n) (conj xs n))
        xs))))

(defcheck solution-519185aa
  (fn rng [s e]
    (if (= s e)
      '()
      (cons s (rng (inc s) e)))))

(defcheck solution-51dbb9be
  (fn rg [a b]
    (if (= a b)
      '()
      (concat (list a)
              (rg (+ a 1) b)
              )
      )
    ))

(defcheck solution-52f62e9e
  (fn my-range [start end]
    (cond
      (< start end) (conj (my-range (+ start 1) end) start)
      :else '()
      )
    ))

(defcheck solution-53332fd7
  (fn [a b] (loop [curr a
                   result []]
              (if (= curr b)
                result
                (recur (inc curr) (conj result curr))
                )
              )
    ))

(defcheck solution-53905c5c
  (fn[a b] (take (- b a) (iterate inc a))))

(defcheck solution-53d2c6d8
  (fn my_range [a b]
    (if (= a b) [] (conj (my_range a (dec b)) (dec b)))))

(defcheck solution-5491a141
  (fn [lo hi]
    (loop [n lo
           cum '()]
      (if (>= n hi)
        (reverse cum)
        (recur (inc n)(cons n cum))))))

(defcheck solution-54f8ada6
  (fn [x y]
    (loop [lo x hi y r '()]
      (if (= lo hi)
        (reverse r)
        (recur (inc lo) hi (conj r lo))))
    ))

(defcheck solution-5526698e
  #(loop [result []
          item %1]
     (if (= item %2)
       result
       (recur (conj result item) (inc item)))))

(defcheck solution-55df0ba
  (fn rng
    ([start stop] (rng start stop '()))
    ([start stop accum]
     (if (< start stop)
       (rng (inc start) stop (concat accum (list start)))
       accum))))

(defcheck solution-55e78f97
  (fn rangeX [a b] (take (- b a) (iterate inc a))))

(defcheck solution-56c087c8
  (fn [d f] (take (- f d) (iterate inc d))))

(defcheck solution-572f2e5b
  (fn [ns nf] (take (- nf ns) (iterate inc ns))))

(defcheck solution-57454e5a
  (fn [start end]
    (loop [accum '()  x start]
      (if (> end x)
        (recur (conj accum x) (inc x))
        (reverse accum)
        )
      )))

(defcheck solution-57d5c13c
  (fn [start, finish]
    (take (- finish start) (iterate inc start))))

(defcheck solution-58d903f1
  (fn foo [a b]
    (if (= a b)
      nil
      (cons a (foo (inc a) b)))))

(defcheck solution-59014b6e
  ;(fn [start end]
  ;  (loop [n start
  ;         xs '()]
  ;    (if (= n end)
  ;      xs
  ;      (recur (inc n) (concat xs (list n))))))

  (fn [start end] (take (- end start) (iterate inc start))))

(defcheck solution-59abf71c
  (fn f [a b] (if (< a b) (cons a (f (inc a) b)))))

(defcheck solution-5aa47ee0
  (fn intervall [start end]
    (lazy-seq (if (< start end)
                (cons start
                  (intervall (inc start) end))
                nil))))

(defcheck solution-5ae6d568
  (fn [x y]
    (loop [x x z '()]
      (if (= x y)
        z
        (recur (inc x)
          (concat z (list x)))))))

(defcheck solution-5bd7070
  (fn [a b]
    (let [r (- b a)]
      (reductions +
        (cons a (replicate (dec r) 1))))))

(defcheck solution-5c127bfa
  (fn my-range [start end]
    (if (>= start end) nil
                       (cons start (my-range (inc start) end)))))

(defcheck solution-5ca1f696
  (fn f
    [x y]
    (if (< x y)
      (cons x (f (inc x) y)))))

(defcheck solution-5d1a1616
  (fn [low high] (take (- high low ) (iterate inc low) ) ))

(defcheck solution-5d570493
  #(loop [v [] x %1]
     (if (= x %2) v (recur (conj v x) (inc x)))))

(defcheck solution-5d9a9fb5
  (fn r [lo hi] (when (< lo hi) (cons lo (r (inc lo) hi)))))

(defcheck solution-5dbeed2
  (fn [from to]
    (loop [f from t to retVal []]
      (if (= f t)
        retVal
        (recur (inc f) t (conj retVal f))))))

(defcheck solution-5e78ca59
  (fn [a b]
    (take (- b a) (iterate inc a))))

(defcheck solution-5ee03874
  (fn rnge [from to]
    (if (= from to)
      '()
      (cons from (rnge (inc from) to)))))

(defcheck solution-5f301879
  (fn myran [from to]
    (let [delta (if (< from to) 1 -1)]
      (loop [n from
             res []]
        (if (= n to)
          res
          (recur (inc n)
            (conj res n)))))))

(defcheck solution-5fa0dc88
  (fn r [from to] (take (- to from) (iterate inc from))))

(defcheck solution-6007510b
  (fn [i j]
    (loop [res [] i i]
      (if (= i j)
        (seq res)
        (recur (conj res i) (inc i))))))

(defcheck solution-603ec06e
  #(take (Math/abs (- %1 %2)) (iterate inc %1)))

(defcheck solution-60487814
  (fn rng
    ([l h] (rng l h []))
    ([l h L]
     (if (= l h)
       L
       (recur (inc l) h (conj L l))))))

(defcheck solution-6162543a
  (fn [b e] (take (- e b) (iterate inc b) )))

(defcheck solution-61d42f23
  (fn r [s t]
    (lazy-seq
      (when (< s t)
        (cons s (r (inc s) t))))))

(defcheck solution-61fa611a
  (fn range2[from to]
    (loop [a from b (dec to) res '()]
      (if (= a b) (conj res a)
                  (recur a (dec b) (conj res b))))))

(defcheck solution-62301245
  (fn rng
    ([a z] (rng a (Math/abs (- z a)) ()))
    ([a z accum] (if (<= z 0) (reverse accum) (rng (inc a) (dec z) (conj accum a))))))

(defcheck solution-625b1570
  (fn [a b]
    (loop [c a
           res []]
      (if (< c b)
        (recur (inc c) (conj res c))
        res))))

(defcheck solution-629a87c1
  (fn [from to]
    (take (- to from) (iterate inc from))))

(defcheck solution-62b35dee
  (fn my-repeat [from to]
    (loop [coll (list)
           last-val (dec to)]
      (if (< last-val from)
        coll
        (recur (cons last-val coll)
          (dec last-val))))))

(defcheck solution-63434bd6
  (fn [s e]
    (let [f (fn [s e r]
              (if (>= s e)
                r
                (recur (inc s) e (conj r s))))]
      (f s e []))))

(defcheck solution-63b92e73
  #(reduce (fn [a b] (conj a (+ b (count a)))) [] (repeat (- %2 %1) %1)))

(defcheck solution-63c8f42f
  (fn myrange [start stop]
    (take (- stop start) (iterate inc start))))

(defcheck solution-63ce5157
  (fn [lo hi]
    (loop [lo lo acc []]
      (if (>= lo hi) acc
                     (recur (inc lo) (conj acc lo))))))

(defcheck solution-64112735
  (fn f [s t]
    (cond
      (= s t) ()
      :else (cons s (f (inc s) t)))))

(defcheck solution-64d8d4d5
  (fn number34 [l u]
    (take (- u l) (iterate inc l))))

(defcheck solution-64fdb192
  (fn [n m]
    (reverse
      (loop [n n
             r '()]
        (if (>= n m)
          r
          (recur (+ n 1) (conj r n)))))))

(defcheck solution-64fdfaf5
  (fn [a b] (take (- b, a) (iterate inc a))))

(defcheck solution-650cfbb6
  (fn [a b] (take (- b a)  (iterate inc a) )))

(defcheck solution-65817748
  (fn my-range [a b]
    (take-while #(< % b) (iterate inc a))))

(defcheck solution-6592a63c
  (fn [from to]
    (loop [current from result []]
      (if (= current to)
        result
        (recur (inc current) (conj result current))))))

(defcheck solution-66697534
  #( take-while (partial > %2) (iterate inc %1)))

(defcheck solution-6695f8a4
  (fn my-range [from to] (if (>= from to) [] (conj (my-range from (dec to)) (dec to)))))

(defcheck solution-6696da3e
  #(map-indexed + (repeat (- %2 %1) %1)))

(defcheck solution-674b2210
  (fn f [a b]
    (if (= a b)
      []
      (cons a (f (+ 1 a) b)))))

(defcheck solution-675c8962
  (fn [s f]
    (loop [s s e f r []]
      (if (= s f)
        r
        (recur (inc s) f (conj r s))))))

(defcheck solution-676f6e2d
  (fn [start end]
    (letfn [(f [acc x]
              (if (= x end)
                acc
                (f (cons x acc) (+ x 1))))]
      (reverse (f () start)))))

(defcheck solution-68545d13
  (fn range-alt [start end]
    (loop [counter start
           coll ()]
      (if (= counter end)
        coll
        (recur (inc counter) (concat coll `(~counter)))))))

(defcheck solution-688a8c73
  (fn [from to]
    (loop [c [] i from]
      (if (= i to)
        c
        (recur (conj c i) (inc i))))))

(defcheck solution-68ced2cc
  (fn [from to] (loop [res []
                       cur from]
                  (if (= cur to) res
                                 (recur (conj res cur) (inc cur))))))

(defcheck solution-693851a7
  (fn [a b] (take-while #(< % b) (iterate inc a))))

(defcheck solution-6989d6ce
  (fn [begin end]
    (let [num (- end begin)]
      (take num (iterate inc begin)))))

(defcheck solution-69d1cdcb
  (fn [l h]
    (loop [l l s []]
      (if (>= l h)
        s
        (recur (inc l) (conj s l))))))

(defcheck solution-6ad1cdc3
  (fn [a b]
    (take (- b a) (iterate (partial + 1) a))))

(defcheck solution-6adfc63b
  (fn [l r] (take-while #(< % r) (iterate #(+ % 1) l))))

(defcheck solution-6b7fd8b5
  (fn [f t]
    ((fn [c r]
       (if (= c t)
         r
         (recur (inc c)
           (concat r [c]))))
     f [])))

(defcheck solution-6cf0325b
  (fn rng [s e] (if (= s e) () (cons s (rng (inc s) e)))))

(defcheck solution-6d37c558
  (fn range* [from to]
    (if (<= to from)
      '()
      (cons from (range* (+ 1 from) to)))))

(defcheck solution-6d39f257
  (fn f [n m]
    (when (< n m)
      (cons n (f (inc n) m)))))

(defcheck solution-6db8a94b
  (fn [f l]
    (loop [c f res ()]
      (if (= c l) (reverse res)
                  (recur (inc c) (cons c res))))))

(defcheck solution-6de6f508
  #(take (- %2 %) (iterate inc %)))

(defcheck solution-6deb7f00
  (fn f [x y]
    (if (sequential? x)
      (if (= (first x)
            (dec y))
        (reverse x)
        (f (cons (inc (first x)) x) y))
      (f (list x) y))))

(defcheck solution-6e05489c
  (fn f [x y] (if (not= x y) (cons x (f (inc x) y)))))

(defcheck solution-6e203333
  (fn range-iter
    ([]    (iterate inc 0))
    ([n]   (range-iter 0 n))
    ([m n] (take-while (partial > n) (iterate inc m)))))

(defcheck solution-6eccd4aa
  (fn makeRange [lo hi] (if (>= lo hi) '() (concat (list lo) (makeRange (+ 1 lo) hi)))))

(defcheck solution-6f03cab6
  (fn [x n]
    (take (- n x) (iterate inc x))))

(defcheck solution-711a6de9
  (fn r
    [start end]
    (if (< start end)
      (concat (list start) (r (inc start) end))
      '())))

(defcheck solution-7200bea0
  #(loop [n (dec %2) rv ()]
     (if (= n %1)
       (conj rv n)
       (recur (dec n) (conj rv n)))))

(defcheck solution-72384ea1
  (fn r [f e] (if (= f e) '() (conj (r (inc f) e) f))))

(defcheck solution-7345023d
  #(loop [ret [%1], n (- %2 %1)]
     (if (> n 1)
       (recur (conj ret (+ 1 (last ret))) (- n 1))
       ret
       )))

(defcheck solution-7352182a
  (fn [start stop]
    (take (- stop start) (iterate inc start))))

(defcheck solution-737cfa40
  (fn [from to]
    (take-while #(< % to)
      (iterate inc from))))

(defcheck solution-73829ba6
  (fn mr [st ed]
    (if (>= st ed) nil
                   (cons st (mr (inc st) ed)))))

(defcheck solution-738f49cf
  ; (fn f [m n] (if (< m n) (cons m (f (inc m) n)) ()))
  #(take (- %2 %) (iterate inc %)))

(defcheck solution-73d021f0
  (fn [a b]
    (loop [x a r []]
      (if (< x b)
        (recur (inc x) (conj r x))
        r))))

(defcheck solution-7586eacc
  (fn rng [start end]
    (if (= start end)
      []
      (concat [start] (rng (inc start) end)))))

(defcheck solution-75ac365b
  (fn [low high]
    (loop [acc []
           n low]
      (if (= n high)
        acc
        (recur (conj acc n) (inc n))))))

(defcheck solution-76a0ce45
  (fn [s e]
    (take-while #(< % e) (iterate inc s))
    ))

(defcheck solution-774810c6
  (fn foo [x y]
    (let [col nil ind x]
      (if (= ind y)
        nil
        (conj (foo (+ x 1) y) ind)))))

(defcheck solution-774f4b8d
  (fn[i j]((fn f [x] (if (= x j) '() (cons x (f (+ x 1))))) i)))

(defcheck solution-77cd0bdc
  (fn myrange [from to]
    (lazy-seq
      (if (< from to)
        (conj (myrange (inc from) to) from)
        nil
        )
      )
    ))

(defcheck solution-77e056d0
  #(
     loop [start %1
           end %2
           listAcc ()]
     (if (= start end)
       listAcc
       (let [decEnd (dec end)]
         (recur start decEnd (cons decEnd listAcc))))))

(defcheck solution-781bf886
  (fn [start end]
    (loop [current start coll []]
      (if (= current end)
        coll
        (recur (inc current) (conj coll current))))))

(defcheck solution-7875b553
  #((fn r
      ([] (r %1))
      ([n] (if (= n %2) nil (lazy-seq (cons n (r (inc n)))))))))

(defcheck solution-78f113db
  (fn [start end]
    (loop [i start, res `()]
      (if (>= i end)
        (reverse res)
        (recur (inc i) (conj res i)))
      )))

(defcheck solution-78fe563f
  (fn f [a b]
    (if (< a b)
      (cons a (lazy-seq (f (inc a) b)))
      '())))

(defcheck solution-796fe7c7
  (fn [from to] (loop [fr from acc []] (if (= fr to) acc (recur (inc fr) (conj acc fr))))))

(defcheck solution-7984127d
  (fn [b e] (take (- e b) (iterate inc b))))

(defcheck solution-7a41c442
  #(loop [i (- %2 %1) n %1 acc []]
     (if (> i 0)
       (recur (dec i) (inc n) (conj acc n))
       acc )))

(defcheck solution-7a747d10
  (fn meu-range [inicio fim]
    (if (= inicio fim)
      nil
      (cons inicio (meu-range (inc inicio) fim)))))

(defcheck solution-7af02c1b
  (fn [start end]
    (loop [x start
           res []]
      (if (= x end)
        res
        (recur (inc x) (conj res x))))))

(defcheck solution-7b3fe133
  (fn [start stop] (take (- stop start) (iterate inc start))))

(defcheck solution-7d27f6dc
  (fn [a b]
    (map-indexed + (repeat (- b a) a))))

(defcheck solution-7d3b8257
  (fn rng [a b] (reverse
                  (loop [p a, lst '()]
                    (if (= p b)
                      lst
                      (recur (+ p 1) (conj lst p)))))))

(defcheck solution-7d6b4f8c
  (fn [lower upper]
    (loop [next lower
           so-far []]
      (if (>= next upper)
        so-far
        (recur (inc next) (conj so-far next))))))

(defcheck solution-7e0bcf6a
  #(take (- %2 %) (iterate inc %1)))

(defcheck solution-7e3776a4
  #(loop [from %1
          to %2
          acc []]
     (if (= from to)
       acc
       (recur (inc from) to (conj acc from))
       )
     ))

(defcheck solution-7e49a63f
  (fn [start end]
    (loop [x start ans []]
      (if (= x end)
        ans
        (recur (inc x) (conj ans x))))))

(defcheck solution-7e66cf1a
  (fn custom-range [begin end]
    (loop [begin begin, end end, range-list []]
      (if (= begin end)
        range-list
        (recur (inc begin) end (conj range-list begin))))))

(defcheck solution-7e8995d4
  #(loop [result '()
          top (- %2 1)]
     (if (< top %1)
       result
       (recur (conj result top) (dec top))
       )
     ))

(defcheck solution-7ef47782
  (fn ff [s e] (if (< s e) (conj  (ff (inc s) e) s) '()
                           )

    ))

(defcheck solution-7f0a35f7
  (fn [s e]
    (loop [cur s
           res '()]
      (if-not (= cur e)
        (recur (inc cur) (conj res cur))
        (reverse res)))))

(defcheck solution-7f328389
  (fn rng [s e]
    (take-while (partial > e) (iterate inc s))))

(defcheck solution-7faa79b2
  (fn [a b]
    (loop [r [] cur a]
      (if (= cur b)
        r
        (recur (conj r cur) (inc cur))))))

(defcheck solution-805405d4
  (fn [a b]
    (loop [i a result []]
      (if (>= i b) result (recur (inc i) (conj result i))))))

(defcheck solution-81324ec6
  (fn r [from to] (if (>= from to) '() (cons from (r (+ from 1) to)))))

(defcheck solution-813615d9
  (fn [start end]
    (loop [current (- end 1) result '()]
      (cond
        (= start current) (cons current result)
        true (recur (dec current) (cons current result))
        )
      )
    ))

(defcheck solution-813f59c7
  (fn [x y]
    (reverse
      (loop [a x b y lst '()]
        (if (= a b)
          lst
          (recur (inc a) b (conj lst a)))))))

(defcheck solution-8168ff9a
  (fn rng ([bot top] (if (= bot top) '() (cons bot (rng (+ bot 1) top))))))

(defcheck solution-817ee9cf
  (fn [from to] (loop [n from r []] (if (< n to) (recur (inc n) (conj r n)) r))))

(defcheck solution-81f64f6d
  #(take-while (partial > %2) (iterate inc %1)))

(defcheck solution-82500f4
  (fn [p q]
    (let [x (- q p)] (take x (iterate inc p)))))

(defcheck solution-827e010e
  (fn [s e] (loop [curr (dec e) result '()] (if (>= curr s) (recur (dec curr) (conj result curr))  result))))

(defcheck solution-828bff18
  #(take (- %2 %1)
     ((fn ! [x]
        (lazy-seq (cons x (! (inc x))))) %1)))

(defcheck solution-82b469f9
  (fn [start end] (loop [n start result '[]] (if
                                              (= n end) result
                                                        (recur (+ n 1) (conj result n))))))

(defcheck solution-82c3f986
  (fn [fro to]
    ((fn [this-val res-list]
       (if (= this-val to)
         (apply list res-list)
         (recur (inc this-val) (conj res-list this-val)))
       )
     fro [])
    ))

(defcheck solution-82d06a1
  (fn [l h] (let [f (fn ff [n] (if (< n h) (list* n (ff (inc n))) '()))] (f l))))

(defcheck solution-82dcc797
  (fn rec [a b]
    (lazy-seq
      (if (>= a b)
        ()
        (cons a (rec (inc a) b))))))

(defcheck solution-8301b4e
  #(map (fn [n] (+ n (dec %1))) (reductions + (take (- %2 %1) (repeat 1)))))

(defcheck solution-84a6a63e
  (fn range-e [low high]
    (if (<= high low) '()
                      (conj (range-e (inc low) high) low)
                      )))

(defcheck solution-85fe686d
  (fn [start end]
    (loop [start start end end acc []]
      (if (>= start end)
        acc
        (recur (inc start) end (conj acc start))
        )
      )
    ))

(defcheck solution-863e0454
  (fn [range1, range2] (loop [x range1, y range2 result []]
                         (if (= (- y x) 0)
                           result
                           (recur (inc x) y (conj result x))))))

(defcheck solution-86b98979
  (fn r [a b]
    (if (= a b) '() (conj (r (inc a) b) a))))

(defcheck solution-87073883
  (fn
    [start end]
    (take (- end start) (iterate inc start))))

(defcheck solution-8732a99a
  (fn [a b] (take (- b a)  (iterate #(+ 1 %) a))))

(defcheck solution-8890c2d9
  (fn r [f t]
    (if (= f t)
      '()
      (cons f (r (+ f 1) t)))))

(defcheck solution-88d5c1a6
  (fn range-1 [s e]
    (when (< s e)
      (cons s (range-1 (inc s) e)))))

(defcheck solution-890b312
  (fn [x y]
    (loop [col [x] end x]
      (if (= end (dec y))
        col
        (recur (conj col (+ end 1) )  (inc end) )))))

(defcheck solution-8957944a
  (fn [min max]

    ((fn [a b l]

       (if ( < a b)

         (recur a (dec b) (conj l (dec b)))

         l )) min max '())))

(defcheck solution-8a41e35c
  (fn ! [a b] (if (< a b) (concat [a] (! (+ a 1) b) ))))

(defcheck solution-8a8b94b
  (fn [n m]
    (take (- m n) (iterate inc n))))

(defcheck solution-8aefbd66
  (fn [low high]
    (take (- high low) (iterate inc low))))

(defcheck solution-8b1c8621
  (fn my-range [from to]
    (let [no-items (- to from)]
      (take no-items (iterate inc from)))))

(defcheck solution-8b385b3
  #(loop[i %1
         rtn []]
     (if(>= i %2) rtn
                  (recur(inc i)
                    (conj rtn i)))))

(defcheck solution-8b4003f4
  (fn [l h]
    (loop [result '() x (dec h)]
      (if (= x l) (cons x result)
                  (recur (cons x result) (dec x))
                  )
      )
    ))

(defcheck solution-8b898266
  (fn [low high]
    (take-while #(< % high) (iterate inc low))))

(defcheck solution-8d3d5c3a
  (fn r [begin end]
    (if (>= begin end)
      (list)
      (conj (r (+ begin 1) end) begin))))

(defcheck solution-8d63c1b2
  (fn [s n] (take (- n s) (iterate inc s))))

(defcheck solution-8d9679a6
  (fn [x y]
    (map #(+ x %) (take (- y x) (iterate inc 0)))))

(defcheck solution-8db1c3b5
  (fn [s e]
    (loop [i s r []]
      (if (= i e)
        r
        (recur (inc i) (conj r i))))))

(defcheck solution-8dedeede
  (fn [n0 n1] (take-while #(< % n1) (iterate #(+ 1 %) n0))))

(defcheck solution-8e424f65
  (fn myrange [start end]
    (when (< start end)
      (cons start
        (lazy-seq (myrange (inc start) end))))))

(defcheck solution-8e9abeec
  #(letfn [(r [x y]
             (if (>= x y)
               '()
               (cons x (r (inc x) y))))]
     (r %1 %2)))

(defcheck solution-8ed917ba
  (fn rang [lb ub]
    (take (- ub lb) (cons lb (lazy-seq (rang (inc lb) ub))))))

(defcheck solution-8fb6009e
  (fn [start end]
    (loop [xs []
           v start]
      (if (< v end)
        (recur (conj xs v) (inc v))
        xs))))

(defcheck solution-9066cd59
  (fn ran [a b] (if (= a b) '() (conj (ran (+ a 1) b) a))))

(defcheck solution-9092e2f2
  (fn range' [lower higher]
    (if (>= lower higher) '()
                          (cons lower (range' (inc lower) higher)))))

(defcheck solution-90dd86a2
  (fn [a b]
    "returns all integers in half-open range [a, b)"
    (loop [x '()
           y (dec b)]
      (if (<= a y)
        (recur (conj x y) (dec y))
        x))
    (take (- b a) (iterate inc a))))

(defcheck solution-9102cac0
  (fn [a b] (loop [x a, v []] (if (< x b) (recur (inc x) (conj v x)) v))))

(defcheck solution-912fc89a
  (fn range-within [lower higher] (take (- higher lower) (iterate inc lower))))

(defcheck solution-91d2c23b
  (fn [form to]
    (loop [r [] item form]
      (if (< item to)
        (recur (conj r item) (inc item))
        r))))

(defcheck solution-925c5bde
  (fn [s e]
    (loop [r []
           i s]
      (if (>= i e)
        r
        (recur (conj r i)
          (inc i))))))

(defcheck solution-92e4b64b
  (letfn [(rng [m n] (if (>= m n) '() (lazy-seq (cons m (rng (inc m) n))))  )] rng))

(defcheck solution-9340c963
  #(map-indexed (fn [i _] (+ %1 i)) (repeat (- %2 %1) 1)))

(defcheck solution-9362361d
  (fn[low high] (loop [l low h high ret '()] (if (>= l h) (reverse ret) (recur (inc l) h (cons l ret))))))

(defcheck solution-93754f8b
  (fn r [lo hi] (when (< lo hi)
                  (cons lo (r (inc lo) hi)))))

(defcheck solution-93afcf82
  (fn [i k] (loop [ii i ss []]
              (if (= ii k)
                (seq ss)
                (recur (inc ii) (conj ss ii))))))

(defcheck solution-9427051c
  (fn [n1 n2]
    (loop [n n1 c []] (if (< n n2) (recur (inc n) (conj c n)) c))))

(defcheck solution-946ffef3
  #(loop [n %1 o []]
     (if (= n %2)
       o
       (recur (inc n) (conj o n)))))

(defcheck solution-94884576
  #(loop [x %, c []]
     (if (< x %2)
       (recur (inc x)
         (conj c x))
       c)))

(defcheck solution-958a4d6a
  (fn myrange [x y]
    (cond
      (= x y) '()
      :else (cons x (myrange (inc x) y)))))

(defcheck solution-96ba74b3
  (fn myrange [a b] (let [mgen (fn [ x y ] (if (< x y) x))]
                      (let [c (mgen a b)] (if c (cons c ( myrange (+ a 1) b))))
                      )))

(defcheck solution-97cbd592
  (fn rng [s e]
    (when (< s e)
      (lazy-seq (conj (rng (inc s) e) s)))))

(defcheck solution-984b7c62
  (fn r
    ([e]
     (r 0 e))
    ([s e]
     (loop [res [] x s]
       (if (< x e)
         (recur (conj res x) (inc x))
         (apply list res))))))

(defcheck solution-98c83d11
  #(loop [i %1 result []]
     (if (< i %2)
       (recur (inc i) (conj result i))
       result)))

(defcheck solution-990b655a
  #(take-while (fn [x] (< x %2))
     (iterate inc %)))

(defcheck solution-993cecdd
  (fn
    [start end]
    (take-while #(< % end) (iterate inc start))
    ))

(defcheck solution-99cce37e
  (fn rng [i n]  (if (< i n) (cons i (rng (inc i) n)) nil)))

(defcheck solution-9a0cfc06
  (fn [a b]
    (loop [vs nil
           i  a]
      (if (>= i b)
        (reverse vs)
        (recur (conj vs i) (inc i))))))

(defcheck solution-9a27217b
  (fn [st end]
    (take  (- end st ) (iterate inc st))))

(defcheck solution-9b27c9f1
  (fn [x y] (take-while #(< % y) (iterate inc x))))

(defcheck solution-9bcffa13
  (fn r [a b] (if (< a b) (cons a (r (inc a) b)))))

(defcheck solution-9c3304b0
  (fn my-range [start end]
    (if (= start end)
      '()
      (cons start (my-range (inc start) end)))))

(defcheck solution-9c95a9f5
  (fn [x y] (take (- y x) (iterate #(inc %) x))))

(defcheck solution-9d33519b
  #(->> (iterate inc %1)
     (take (- %2 %1) ,)))

(defcheck solution-9daf79c1
  (fn cr [s e]
    (reverse (loop [a s, result ()]
               (if (= a e)
                 result
                 (recur (inc a) (conj result a)))))))

(defcheck solution-9dce08a1
  (fn [a b]
    (loop [x a c []]
      (if (= x b)
        c
        (recur (inc x) (conj c x))))))

(defcheck solution-9e01535
  #((fn [x y z] (if (< x y) (recur (inc x) y (conj z x)) z)) %1 %2 []))

(defcheck solution-9e878cef
  (fn [l u]
    (take-while
      #(< % u)
      (iterate inc l))))

(defcheck solution-a08c29d8
  (fn [x y]
    ((fn im [x y z]
       (if (< x y)
         (cons x (im (inc x) y z))
         z
         )
       ) x y []
     )
    ))

(defcheck solution-a12f1e9d
  (fn [a b]
    (loop [out [a] n (inc a)]
      (if (>= n b) out (recur (conj out n) (inc n))))))

(defcheck solution-a14997e3
  #(take-while (partial > %2) (iterate inc %)))

(defcheck solution-a1d0e37a
  (fn my-range [x y]
    (if (>= x y) nil (cons x (lazy-seq (my-range (inc x) y))))))

(defcheck solution-a2614a01
  #(loop [i %1 res '()]
     (if (<= %2 i) res
                   (recur (inc i) (concat res (list i))))))

(defcheck solution-a2e04c75
  (fn my-range [a b]
    (if (>= a b)
      '()
      (cons a (my-range (inc a) b)))))

(defcheck solution-a2eead30
  #(take (- %2 %1) (iterate inc %1)))

(defcheck solution-a32e844c
  (fn[x & y] ( loop [z () n x] (if ( <  n (first y)) (recur (conj z n) (+ n 1)) (reverse z)) )))

(defcheck solution-a3a84169
  #(loop [i %1 z [%1]] (let [n (inc i)] (if (< n %2) (recur n (conj z n)) z))))

(defcheck solution-a3e23df
  (fn -range [lo hi]
    ((fn [n acc]
       (if (< n lo)
         acc
         (recur (dec n)
           (cons n acc))))
     (dec hi) nil)))

(defcheck solution-a3f5d8bc
  #(loop [x %1 a []]
     (if (= x %2)
       a
       (recur (inc x) (conj a x)))))

(defcheck solution-a40f6c85
  (fn [from to] (take (- to from) (iterate inc from))))

(defcheck solution-a4716134
  (fn [a b] (
              map-indexed (fn [idx itm](+ idx itm)) (repeat (- b a) a)
              )
    ))

(defcheck solution-a47cde3
  (fn [n1 n2] (take (- n2 n1) (iterate inc n1))))

(defcheck solution-a4ccda3a
  (fn range2 [lower upper]
    (if (= lower upper)
      '()
      (lazy-seq (cons lower (range2 (inc lower) upper))))))

(defcheck solution-a5305bab
  (fn rrr [i j]
    (take (- j i) (iterate inc i))))

(defcheck solution-a564d38b
  #(take (- %2 %1) (iterate inc %1) ))

(defcheck solution-a5c92e40
  (fn r [l h] (if (= (inc l) h) [l] (cons l (r (+ l 1) h)))))

(defcheck solution-a5c95080
  (fn [s e]
    (take (- e s) (iterate inc s))))

(defcheck solution-a5ea18ca
  (fn [mn mx] (take (- mx mn) (iterate inc mn))))

(defcheck solution-a6476214
  (fn impl-range [x y]
    (if (= x y)
      []
      (concat (list x) (impl-range (+ x 1) y)))))

(defcheck solution-a712ad04
  (fn r [b e]
    (if (= b e)
      '()
      (cons b
        (lazy-seq (r (+ b 1) e))))))

(defcheck solution-a79c3c6e
  (fn r [min max &[res]]
    (if (= min max) res
                    (cons min (r (+ 1 min) max (or res '()))))))

(defcheck solution-a7d8fd17
  #(loop [ns [] n %1]
     (if (= n %2)
       ns
       (recur (conj ns n) (inc n)))))

(defcheck solution-a815b71
  (fn [l u] (loop [i l s []] (if (>= i u) s (recur (inc i) (conj s i))))))

(defcheck solution-a86e3b5
  (fn r [a b]
    (if (= a b)
      ()
      (conj (r (inc a) b) a))))

(defcheck solution-a91f210c
  (fn rrange [a b]
    (take (- b a)
      (lazy-seq (cons a (rrange (+ a 1) b))))))

(defcheck solution-a9220a1c
  #(take-while (fn [x] (< x %2)) (iterate inc %1)))

(defcheck solution-a93c2532
  (fn r [s e] (if (= s e) () (conj (r (inc s) e) s))))

(defcheck solution-a969651f
  #(reductions + %1 (repeat (- %2 %1 1) 1)))

(defcheck solution-aa1da535
  (fn d [xstart xend]
    (if (= xstart xend)
      '()
      (conj (d (+ xstart 1) xend) xstart))))

(defcheck solution-aa7f26f2
  (fn [a b]
    (take-while #(< % b) (iterate inc a))))

(defcheck solution-aaa5ab55
  (fn [from to]
    (take-while (partial > to) (iterate inc from))))

(defcheck solution-aacbf0c9
  (fn[a b](filter #(and (<= a %) (< % b)) (take 10 (iterate inc -2)))))

(defcheck solution-ab1a5e5f
  (fn [start end]
    (loop [c start res []]
      (if (< c end)
        (recur (inc c) (conj res c))
        res))))

(defcheck solution-abd97043
  (fn f [a b] (if (= a b) () (cons a (f (inc a) b)))))

(defcheck solution-ac3dfc33
  (fn my-range [start end]
    (if (< start end)
      (lazy-seq
        (cons start (my-range (inc start) end))
        )
      (list)
      )
    ))

(defcheck solution-ac841f39
  (fn [start end]
    (seq
      (loop [i start vals []]
        (if (= i end)
          vals
          (recur (inc i) (conj vals i)))))))

(defcheck solution-ac96c25
  #(take (- %2 %1) (iterate inc %)))

(defcheck solution-acbd1d38
  #(loop [i % x []]
     (if (= i %2) x
                  (recur (inc i) (conj x i)))))

(defcheck solution-acf2a1b9
  #(take (- %2 %1) (iterate (partial + 1) %1)))

(defcheck solution-ad08f6bc
  (fn fake
    [start end]
    (take-while #(< % end) (iterate inc start))))

(defcheck solution-ad3b2781
  (fn [start end]
    (loop [n start acc '()]
      (if (>= n end)
        (reverse acc)
        (recur (inc n) (cons n acc))))))

(defcheck solution-ad3fd9b6
  #(take (- %2 %)
     (iterate inc %)))

(defcheck solution-ae8277ae
  (fn [start end] (take-while #(< % end)
                    (iterate inc start)
                    )
    ))

(defcheck solution-ae9ad2aa
  (fn [m n]
    (take-while #(< % n) (iterate inc m))))

(defcheck solution-ae9d42b7
  (fn [i j] (take (- j i) (iterate inc i))))

(defcheck solution-aee7cecd
  (fn[l h] (reduce #(conj % (+ (count %) %2 )) [] (repeat (- h l) l))))

(defcheck solution-af430852
  (fn [a b]
    (loop [i a r []]
      (if (< i b)
        (recur (inc i) (conj r i))
        r))))

(defcheck solution-af8287ab
  (fn my-range [start end]
    (if (< start end)
      (conj (my-range (inc start) end) start)
      '())))

(defcheck solution-b0c066a
  #(take (- %2 %1) ((fn rang [x] (lazy-seq (cons x (rang (inc x))))) %1)))

(defcheck solution-b0de63ad
  (fn
    [s e]
    (loop [r '()
           i (dec e)]
      (if (< i s)
        r
        (recur (conj r i) (dec i))))))

(defcheck solution-b0e736f6
  (fn[x y](take (- y x) (iterate #(+ % 1) x))))

(defcheck solution-b0fb9407
  (fn [start end]
    (take-while (partial > end) (iterate (partial + 1) start))))

(defcheck solution-b13ed328
  (fn [a b]
    (take (- b a)
      (iterate inc a))))

(defcheck solution-b186714
  (fn [a b] (take (- b a) (iterate #(inc %) a))))

(defcheck solution-b1f781ca
  (fn rangex [s f]
    (lazy-seq
      (when (< s f)
        (cons s (rangex (inc s) f))))))

(defcheck solution-b24c3102
  #( loop [ i %1
           end %2
           acc []]
     (if (= i end)
       acc
       (recur (+ i 1) (identity end) (conj acc i)))))

(defcheck solution-b254b22b
  (fn [start end]
    (loop [x start, r '[]]
      (if (< x end)
        (recur (inc x) (conj r x))
        (seq r)))))

(defcheck solution-b27b1f25
  (fn enumerate
    ([start end] (enumerate start end []))
    ([start end coll]
     (if (= start end)
       coll
       (recur (inc start) end (conj coll start))))))

(defcheck solution-b2b2e8f9
  (fn [start stop] (loop [r [] i start]
                     (if (>= i stop)
                       r
                       (recur (conj r i) (inc i) )))))

(defcheck solution-b36443b8
  (fn rng [a b] (for [x (iterate inc a) :while (< x b)] x)))

(defcheck solution-b3868a2
  (fn r [s e] (lazy-seq (when (< s e) (cons s (r (inc s) e))))))

(defcheck solution-b41deea2
  (fn [l h] (take (- h l) (iterate inc l))))

(defcheck solution-b485271e
  (fn [start end]
    (take (- end start) (iterate inc start))))

(defcheck solution-b49271b7
  (fn [from to] (take-while #(< % to) (iterate inc from))))

(defcheck solution-b493ded2
  (fn my-range [a b] (when (< a b) (cons a (lazy-seq (my-range (inc a) b))))))

(defcheck solution-b4be0ca
  (fn f [x y] (if (= x y) nil (cons x (f (inc x) y)))))

(defcheck solution-b52ee3f0
  (fn [start end]
    (loop [i start
           r ()]
      (if (= i end)
        (reverse r)
        (recur (inc i) (conj r i))))))

(defcheck solution-b5364e68
  ;#(loop [start % end %2 acc []] (if (= start end) acc (recur (inc start) end (conj acc start))))

  #(take (- %2 %) (iterate inc %)))

(defcheck solution-b5714360
  (fn [start end]
    (loop [i start
           acc []]
      (if (< i end)
        (recur (inc i) (conj acc i))
        acc))))

(defcheck solution-b5738ff3
  (fn [start end] (loop
                   [result []
                    i start
                    ]
                    (if (>= i end) result
                                   (recur (conj result i) (inc i))
                                   )
                    )))

(defcheck solution-b5dfcf77
  (fn rng [a b]
    (if (>= a b)
      '()
      (cons a (rng (+ a 1) b))
      )
    ))

(defcheck solution-b6dd9cbf
  (fn [a b]
    (take (- b a) (iterate inc a))))

(defcheck solution-b737450b
  (fn [f t]
    (loop [res '() i f]
      (if (= i t)
        (reverse res)
        (recur (conj res i) (inc i))))))

(defcheck solution-b74d841
  #((fn rec [l x n]
      (if (>= x n) l (rec (conj l x) (+ 1 x) n))) [] %1 %2))

(defcheck solution-b76da6e7
  #(take (- %2 %) (reductions + % (repeat 1))))

(defcheck solution-b77d1c42
  (fn prob34 [x y]
    (take (- y x) (iterate inc x))))

(defcheck solution-b7c79ab7
  #(take (- %2 %) (iterate inc %)))

(defcheck solution-b7f44d23
  #(take  (- %2 %) (iterate inc %)))

(defcheck solution-b842f45c
  (fn f [x y] (if (= x y) '() (conj (f (+ x 1) y) x))))

(defcheck solution-b8574b01
  #(loop [i %2 x ()]
     (if (= i %)
       x
       (recur (- i 1) (conj x (- i 1)))
       )))

(defcheck solution-b907e0c8
  (fn [lo hi]
    (take (- hi lo) (iterate inc lo))))

(defcheck solution-b90dec3
  (fn range' [a b]
    (if (= a (dec b))
      (list a)
      (cons a (range' (inc a) b))
      )
    ))

(defcheck solution-b9101402
  (fn [b e]
    (loop [n (inc b) s [b]]
      (if (= n e)
        s
        (recur (inc n) (conj s n))))))

(defcheck solution-b9cfcd1f
  (fn [lower upper] (take-while (fn [n] (< n upper)) (iterate inc lower))))

(defcheck solution-bb532752
  (fn myrange
    [start end]
    (loop
     [cnt (- (dec end) start) mynext (dec end) res nil]
      (if (= cnt 0)
        (cons  start res)
        (recur (dec cnt) (dec mynext) (cons mynext res))
        ))))

(defcheck solution-bb55d9b7
  (fn problem34-range
    [x y]
    (loop [counter x,
           agg []]
      (if (= counter y)
        agg
        (recur (inc counter) (conj agg counter))))))

(defcheck solution-bbf85d10
  (fn my-range [l u] (if (< l u) (cons l (my-range (+ l 1) u)) ())))

(defcheck solution-bc15c006
  (fn rang [a b]
    (if (< a b)
      (cons a (rang (inc a) b)))))

(defcheck solution-bcbc7d64
  (fn a
    ([from to]
     (when (< from to)
       (cons from (a (inc from) to))))))

(defcheck solution-bcbdbe05
  (fn rng [st en]
    (if (= (+ st 1) en)
      (list st)
      (cons st (rng (+ st 1) en)))))

(defcheck solution-bd13685c
  (fn [x y]
    (take (- y x)(iterate inc x))))

(defcheck solution-bdec147b
  #(for [x (iterate inc %) :while (< x %2)] x))

(defcheck solution-be5d326e
  #(take (- %2 %)(iterate inc %)))

(defcheck solution-bea6de26
  (fn [lower upper]
    (reduce (fn [acc elt] (cons (- (first acc) 1) acc))
      (list (- upper 1))
      (take (- upper lower 1) (repeat true)))))

(defcheck solution-beb3fa82
  (fn a [lst from to](if (= from to) lst
                                     (a (conj lst from) (inc from) to))) [])

(defcheck solution-befb4231
  (fn[from to]
    (reverse (loop [init from almacen '()]
               (if (= init to)
                 almacen
                 (recur (inc init) (conj almacen init))
                 )
               ))
    ))

(defcheck solution-beff18ec
  (fn [start end] (loop [s start a []] (if (or (= s end) (> s end)) a (recur (inc s) (conj a s))))))

(defcheck solution-bf36a74c
  #(loop [a %
          b %2
          s ()]
     (if (== a b)
       s
       (recur a (dec b) (cons (dec b) s)))))

(defcheck solution-bf3f8632
  (fn getrange [initial-start initial-end]
    (loop [xs ()
           start initial-start
           end initial-end]
      (if (= start end)
        xs
        (recur (cons (dec end) xs) start (dec end))))))

(defcheck solution-bf536cc8
  (fn [x y & rlist]
    (cond
      (= x y) rlist
      (nil? rlist) (recur (+ x 1) y [x])
      :else (recur (+ x 1) y (concat rlist [x])))))

(defcheck solution-bfd46314
  (fn foo [start end]
    (cond
      (< start end) (cons start (lazy-cat (foo (inc start) end)))
      :else ())))

(defcheck solution-c05cce66
  (fn [init fin]
    (loop [n (- fin 1) acc ()]
      (if (< n init)
        acc
        (recur (- n 1) (conj acc n))))))

(defcheck solution-c1a3cfbf
  (fn my-range [a b]
    (if (< a b)
      (conj (my-range (inc a) b) a)
      '())))

(defcheck solution-c1be80c4
  (fn [start end]
    (loop [i start, coll []]
      (if (< i end)
        (recur (inc i) (conj coll i))
        coll))))

(defcheck solution-c2086b82
  (fn [s e]
    (loop [result []
           value s
           cnt (- e s)]
      (if (< cnt 2)
        (conj result value)
        (recur (conj result value) (inc value) (dec cnt))))))

(defcheck solution-c3502b29
  #(loop [v %1, acc []] (if (== v %2) acc (recur (+ v 1) (conj acc v)))))

(defcheck solution-c36493cf
  (fn rng [from below]
    (loop [n from, acc nil]
      (if (= n below)
        (reverse acc)
        (recur (inc n) (cons n acc))))))

(defcheck solution-c3a41bc1
  #(loop [i %1 n []]
     (if (< i %2)
       (recur (inc i) (conj n i))
       n)))

(defcheck solution-c3ab4270
  (fn[l,h]
    (take (- h l) (iterate #(+ 1 %) l))
    ))

(defcheck solution-c4346380
  #(loop [a %1 s []] (if (< a %2) (recur (inc a) (conj s a)) s)))

(defcheck solution-c501bfab
  (fn [start end] (take (- end start) (iterate #(inc %) start))))

(defcheck solution-c5354290
  #(loop [i %1 res []]
     (if (= i %2) res (recur (inc i) (conj res i)))))

(defcheck solution-c66f2683
  (fn [x y]
    (let [r (atom (dec x))]
      (repeatedly (- y x) #(swap! r inc)))))

(defcheck solution-c67f48aa
  (fn interval [start stop]
    (if (= start stop)
      '()
      (lazy-seq (cons start (interval (inc start) stop))))))

(defcheck solution-c7165598
  #(loop [x (- %2 1) acc '()]
     (if (< x %1)
       acc
       (recur (dec x) (conj acc x)))))

(defcheck solution-c7398540
  (fn rng [ a b ]
    (if (= a b ) '()
                 (cons a (rng (inc a) b)))))

(defcheck solution-c7433ce2
  (fn my-range [start end]
    (when (< start end)
      (conj (my-range (inc start) end) start))))

(defcheck solution-c757d096
  (fn [s e]
    (take (- e s)
      (iterate #(+ % 1) s))))

(defcheck solution-c7c7cca3
  (fn f [x y] (if (< x y) (conj  (f (inc x) y) x))))

(defcheck solution-c871573
  (fn [a b]
    (take-while
      #(< % b)
      (iterate inc a))))

(defcheck solution-c92ea143
  (fn [from to] ((fn iter [from to res] (if (= from to) res (iter from (dec to) (conj res (dec to))))) from to ())))

(defcheck solution-c9673128
  (fn this
    ([lo hi] (this lo hi []))
    ([lo hi acc]
     (if (= lo hi)
       acc
       (recur (inc lo) hi (conj acc lo))))))

(defcheck solution-c9796695
  #(take (- %2 %1) (iterate inc %1 )))

(defcheck solution-c9cd52a6
  (fn [a b]
    (reverse
      (loop [start a,end b,col '()]
        (if (= start end)
          col
          (recur (inc start) end (cons start col) )
          )
        )
      )
    ))

(defcheck solution-cb5448b8
  #(loop [start %1
          end %2
          lst []]
     (if (= start end)
       lst
       (recur (inc start) end (conj lst start)))))

(defcheck solution-cbc60f7b
  (fn my-range
    [start end]
    (if (= start end)
      []
      (cons start (my-range (inc start) end)))))

(defcheck solution-cc3af2e0
  (fn [x1 x2]
    (take (- x2 x1) (iterate #(inc %) x1))))

(defcheck solution-cc84150f
  (fn [a b]
    (loop [s [] a a]
      (if (= a b)
        s
        (recur (conj s a) (inc a))))))

(defcheck solution-cdee93a3
  (fn [low high]
    "34. Write a function which creates a list of all integers in a given range."
    (loop [n low
           acc (list)]
      (if (>= n high)
        (reverse acc)
        (recur (inc n) (conj acc n))))))

(defcheck solution-cdf6481d
  (fn [s e] (take-while #(< % e) (iterate inc s))))

(defcheck solution-ce15f42d
  (fn r[x y] (if (< x y) (conj (r x (dec y)) (dec y)) [])))

(defcheck solution-ce24e3da
  (fn my-range
    ([start end] (my-range start end []))
    ([start end accum]
     (if (>= start end)
       accum
       (recur (inc start) end (conj accum start))))))

(defcheck solution-ce70f764
  (fn  [start end]
    (loop [i start, acc (vector)]
      (cond (= i end) (seq acc)
            :else (recur (inc i) (conj acc i))))))

(defcheck solution-ceb8e8f7
  #(loop [start [%1] end %2]
     (if (not (= (last start) (dec end)))
       (recur (conj start (inc (last start))) end)
       start)))

(defcheck solution-cf5043ed
  (fn myRange [a b]
    (if (= a b)
      '()
      (concat (list a) (myRange (inc a) b)))))

(defcheck solution-cf879d4b
  (fn [lo hi]
    (loop [low lo
           high hi
           result []]
      (if (<= high low)
        result
        (recur (inc low) high (conj result low))))))

(defcheck solution-d01ed657
  (fn [start end]
    (loop [iter start, result []]
      (if (= iter end) result
                       (recur (inc iter) (conj result iter))))))

(defcheck solution-d020204
  (fn myRange
    [x y]
    (take (- y x) (iterate inc x))))

(defcheck solution-d0f30b7f
  (fn [l h] (loop [l l ret '()] (if (< l h) (recur (+ l 1) (conj ret l)) (reverse ret)))))

(defcheck solution-d13f9312
  (fn [from to] (sort (loop [at from result '()]
                        (if (< at to)
                          (recur (+ at 1) (conj result at))
                          result)))))

(defcheck solution-d15cc020
  #(take
     (- %2 %1)
     (iterate inc %1)))

(defcheck solution-d18d9e6a
  (fn [low hi]
    (loop [i low stop (- hi 1) result []]
      (if (> i stop)
        result
        (recur (+ i 1) stop (conj result i))))))

(defcheck solution-d2751a09
  (fn [m n] (take (- n m) (iterate inc m
                            ))))

(defcheck solution-d28c13a6
  (fn [a b]
    (loop [r [] n a]
      (if (= n b) r (recur (conj r n) (inc n))))))

(defcheck solution-d3196c17
  (fn custom-range
    [begin end-exclusive]
    (let [difference (- end-exclusive begin)
          template-list (repeat difference 0)]
      (map-indexed (fn [idx item] (+ idx begin)) template-list))))

(defcheck solution-d3fabb31
  (fn ! [a b]
    (if (>= a b) '()
                 (cons a (! (inc a) b)))))

(defcheck solution-d45b4545
  (fn gen-seq [f t]
    (lazy-seq
      (if (= f t) []
                  (cons f (gen-seq (inc f) t))))))

(defcheck solution-d4b1954f
  #(loop [s % r []] (if (= %2 s) r (recur (inc s) (conj r s)))))

(defcheck solution-d55dd42c
  (fn myrange [start end]
    (loop [i start aggr []]
      (if (>= i end) aggr
                     (let [n (inc i)]
                       (recur n (conj aggr i)))))))

(defcheck solution-d5832e31
  (fn [x y] (drop-while #(< % x) (take-while #(< % y) (iterate inc x)))))

(defcheck solution-d5ece141
  (fn [start end]
    (loop [result [start]]
      (if (= (- end 1) (last result))
        result
        (recur (conj result (inc (last result))))
        )
      )
    ))

(defcheck solution-d6ce6a3a
  (fn myrange [a b]
    (if (= a b)
      '()
      (conj (myrange (inc a) b)
        a))))

(defcheck solution-d7161d90
  (fn my-range [x y]
    (take-while #(< % y) (iterate inc x))))

(defcheck solution-d85e8424
  (fn rng [start end]
    (if (= start end)
      '()
      (cons start (rng (+ start 1) end)))))

(defcheck solution-da728716
  (fn f [s e] (when (< s e) (conj (f (inc s) e) s))))

(defcheck solution-da7bd21
  (fn [x y]
    (take (- y x) (iterate inc x))))

(defcheck solution-da9621e6
  (fn my-range [i n]
    (take-while #(< % n) (iterate inc i))))

(defcheck solution-daedeccd
  (fn [i j] (take (- j i) (iterate inc i) )))

(defcheck solution-dc057042
  (fn mr
    [s e]
    (if (< s  e)
      (cons s (mr (inc s) e)))))

(defcheck solution-dcb9c68e
  (fn [from to]
    (loop [i from ret []]
      (if (= i to)
        ret
        (recur (inc i) (conj ret i))))))

(defcheck solution-dcfacfe9
  (fn [a b](take (- b a) ((fn after[a](cons
                                        a
                                        (lazy-seq (after (inc a)))
                                        )
                            ) a)
             )
    ))

(defcheck solution-dd0356c8
  (fn my-range [begin end]
    (if (>= begin end)
      ()
      (lazy-seq
        (cons
          begin
          (my-range (inc begin) end))))))

(defcheck solution-deda3d32
  (fn r [n m]
    (if (>= n m) '()
                 (conj (r (inc n) m) n))))

(defcheck solution-df480169
  (fn [start end]
    (loop [s start c []]
      (if (> end s)
        (recur (inc s) (conj c s))
        c))))

(defcheck solution-df4e314b
  (fn myrange [start end]
    (if (>= start end)
      '()
      (conj (myrange (inc start) end) start))
    ))

(defcheck solution-df66acc3
  (fn [beg end]
    (loop [i beg res '()]
      (if (= i end)
        (reverse res)
        (recur (inc i) (cons i res))))))

(defcheck solution-dfbd3dfb
  (fn [x y]
    (loop [a x ret []]
      (if (= a y)
        ret
        (recur (inc a) (conj ret a))))))

(defcheck solution-dfcaebe3
  (fn [l u] (butlast (map (partial + l) (reductions + 0 (repeat (- u l) 1))))))

(defcheck solution-dfcd5f02
  (fn [start end]
    (#(loop [i %1 xs []]
        (if (= i end)
          xs
          (recur (inc i) (conj xs i)))) start)))

(defcheck solution-dfdf84b8
  (
    fn [a b]
    (let [r (atom [])]
      (dotimes [n (- b a)]
        (swap! r #(conj % (+ a n))))
      @r)))

(defcheck solution-e078684e
  (fn my-range
    [from to]
    ((fn [from to so-far]
       (if (>= from to)
         so-far
         (recur from (dec to) (cons (dec to) so-far))))
     from to '())))

(defcheck solution-e08cc60f
  (fn [a b]
    (let [x (fn [a b l]
              (if (= a b) l (recur (+ a 1) b (conj l a))))]
      (x a b []))))

(defcheck solution-e120685c
  (fn [low hi]
    (loop [low low
           out []]
      (if (= low hi)
        (seq out)
        (recur (+ low 1) (conj out low))))))

(defcheck solution-e12f6354
  (partial
    (fn [acc i j]
      (if (= i j) (reverse acc)
                  (recur (conj acc i) (inc i) j)))'()))

(defcheck solution-e15161a4
  (fn [start limit] (take (- limit start) (iterate inc start))))

(defcheck solution-e1530429
  (fn my-range [lo hi]
    (take-while #(< % hi)
      (iterate inc lo))))

(defcheck solution-e22865de
  (fn ir[a b]
    (take (- b a) (iterate inc a))))

(defcheck solution-e2c025bc
  #(reverse(
             loop [i %1 result '() ]
             (if (= %2 i)
               result
               (recur (inc i) (conj result i ))
               )

             )))

(defcheck solution-e2cc1571
  #(loop [acc '() c %]
     (if (= c %2) (reverse acc)
                  (recur (conj acc c) (inc c)))))

(defcheck solution-e325e414
  #((fn myrange [a b acc] (if (= a b) acc (myrange (inc a) b (conj acc a)))) %1 %2 []))

(defcheck solution-e4062c99
  #(loop [start %1 end %2 result []]
     (if (< start end)
       (recur (inc start) end (conj result start))
       result)))

(defcheck solution-e532820b
  (fn  [s e]
    (loop [result [] from s to e]
      (if (= from  to)
        result
        (recur (conj result from) (inc from) to))
      )))

(defcheck solution-e53f24d8
  (fn my-range [l u]
    (if (= l u) ()
                (cons l (my-range (inc l) u)))))

(defcheck solution-e56903dd
  (fn [m n]
    (loop [i m xs []]
      (if (>= i n)
        xs
        (recur (inc i) (conj xs i))
        )
      )
    ))

(defcheck solution-e569208a
  #( take (- %2 %1) (iterate inc %1) ))

(defcheck solution-e58804d7
  (fn [b e]
    (take (- e b) (iterate inc b))))

(defcheck solution-e5f59923
  (fn [from to]
    (let [aux
          (fn [from to acc]
            (cond
              (= from to)
              acc

              :else
              (recur (inc from) to (conj acc from))))]
      (aux from to []))))

(defcheck solution-e6370765
  (fn peu [x y] (if (= x y) '() (conj (peu (+ x 1) y) x))))

(defcheck solution-e7b1e3c5
  (fn [start stop]
    (loop [r []
           from start
           to stop]
      (if (= from to)
        r
        (recur (conj r from) (inc from) to)))
    ))

(defcheck solution-e83f649a
  (fn bob [min max]
    (if (>= min max)
      nil
      (cons min (bob (inc min) max)
        )
      )
    ))

(defcheck solution-e86fabb9
  (fn [start finish] (take (- finish start) (iterate inc start))))

(defcheck solution-e87ab526
  (fn [x y] (let [i (min x y) m (max x y)] (loop [c x result '()] (if (>= c y) (reverse result) (recur (inc c) (conj result c)))))))

(defcheck solution-e87b205f
  (fn[lower upper]
    (take-while #(< % upper)
      (iterate inc lower))))

(defcheck solution-e8a3ffcc
  (fn [x y]
    (loop [i x r []]
      (if (>= i y)
        r
        (recur (inc i) (conj r i))))))

(defcheck solution-e90a5e4a
  (fn [x y]
    (map #(+ % x)
      (take (- y x) (iterate inc 0))
      )))

(defcheck solution-e911510a
  (fn [a b]
    (->> (iterate inc a)
      (take (- b a)))))

(defcheck solution-e9fc10b
  (fn ab [n m]
    (if (= m n)
      []
      (cons n (ab (inc n) m)))))

(defcheck solution-ea597314
  (fn [a b]
    (take-while #(< % b)
      (iterate inc a))))

(defcheck solution-ea5bd6fc
  (fn [start end]
    (loop [cur start rang []]
      (if (= cur end)
        rang
        (recur (inc cur) (conj rang cur))))))

(defcheck solution-eacae913
  (fn [start end]
    (loop [i start lst []]
      (if (= i end)
        lst
        (recur (+ i 1) (conj lst i))))))

(defcheck solution-ead2bd98
  (fn rng [b e] (if (= b e) (list) (cons b (lazy-seq (rng (inc b) e))))))

(defcheck solution-eb5e7338
  (fn [s e]
    (take-while #(< %1 e) (iterate inc s))))

(defcheck solution-ebcb1dd
  (fn rng [a b]
    (if (>= a b)
      '()
      (cons a (rng (inc a) b)))))

(defcheck solution-ec30bc7d
  (fn f [start finish]
    (if (= start finish)
      '()
      (cons start (f (inc start) finish))
      )))

(defcheck solution-ec8fe953
  (fn [a b]
    (loop [a a
           v '()]
      (if (= a b)
        v
        (recur (inc a) (concat v (list a)))))))

(defcheck solution-ed9b3c66
  (fn rng [s e] (take (- e s) (iterate inc s))))

(defcheck solution-edf99b1a
  (fn [l r]
    ((fn ran [i res]
       (if (< i r) (ran (inc i) (conj res i)) res)

       ) l [])
    ))

(defcheck solution-ee9ce527
  (fn f [l u]
    (if (< l u)
      (cons l (f (inc l) u)))))

(defcheck solution-eeb1574a
  #(take-while (fn [v] (< v %2)) (iterate inc %1)))

(defcheck solution-eec15db1
  (fn [m n]
    (take (- n m) (iterate inc m))))

(defcheck solution-ef4052d9
  #(let [nAndUp (fn nAndUp [n](lazy-seq (cons n (nAndUp (inc n)))))]
     (for [i (nAndUp %1) :while (< i %2)]
       i)))

(defcheck solution-ef79c8b1
  #(letfn [(my-range [start]
             (lazy-seq (cons start (my-range (inc start)))))]
     (take-while (fn [x] (< x %2))  (my-range %1))))

(defcheck solution-ef96a7a6
  (fn [lo hi]
    (loop [r []
           l lo]
      (if (>= l hi) r
                    (recur (conj r l) (inc l))))))

(defcheck solution-efb05647
  (fn [start stop]
    (take (- stop start) (iterate inc start))))

(defcheck solution-efb9c0fa
  (fn myrange [x y]
    (if (>= x y)
      '()
      (cons x (myrange (+ 1 x) y)))))

(defcheck solution-efbd51ab
  (fn [s f]
    (take (- f s) (iterate inc s))))

(defcheck solution-efe3177c
  (fn rang [lbound rbound]
    (if (< lbound rbound)
      (cons lbound (rang (inc lbound) rbound))
      [] )))

(defcheck solution-eff76269
  (fn rng [start finish]
    (loop [myseq []
           cnt start]
      (if (= cnt finish)
        myseq
        (recur (conj myseq cnt) (inc cnt))))))

(defcheck solution-f0099a45
  (fn [s t] (reductions + (merge (repeat (dec (- t s)) 1) s))))

(defcheck solution-f01b3978
  (fn my-range [start end]
    (if (>= start end)
      []
      (cons start (lazy-seq (my-range (inc start) end))))))

(defcheck solution-f024f69e
  (fn r [x y] (lazy-seq (cons x (if (< x (dec y)) (r (inc x) y))))))

(defcheck solution-f0ad7753
  (fn rng [start end]
    (if (< start end)
      (cons start (rng (inc start) end)))))

(defcheck solution-f0b91993
  (fn [a b] (nth (iterate (fn [x] (conj x (inc (last x)))) [a]) (- b a 1) )))

(defcheck solution-f14bd667
  (fn my-range [start end]
    (if (>= start end)
      ()
      (lazy-seq (cons start (my-range (inc start) end))))))

(defcheck solution-f1e43b41
  (fn new-range
    [a b]
    (if (not= a b)
      (cons a (new-range (inc a) b)))))

(defcheck solution-f2a25f5e
  (fn [fr to]
    (map #(+ (first %) fr) (map-indexed vector
                             (take (- to fr) (repeat 0))))))

(defcheck solution-f32a7972
  (fn [low high] (take (- high low) (iterate inc low))))

(defcheck solution-f32c6cc4
  (fn [a b](take (- b a) (iterate inc a))))

(defcheck solution-f4167e6
  (fn [start end]
    (loop [s start e end acc ()]
      (if (>= s e) (reverse acc) (recur (inc s) e (cons s acc))))))

(defcheck solution-f46207c
  (fn i [f l](when (< f l) (lazy-seq (cons f (i (inc f) l))))))

(defcheck solution-f520384f
  (fn f [x y]
    (if (= x y)
      ()
      (conj (f (+ x 1) y) x)
      )
    ))

(defcheck solution-f550009c
  (fn [lo hi] (take (- hi lo) (iterate inc lo))))

(defcheck solution-f56c38ad
  (fn rng
    [start end]
    (loop [st start final []]
      (if (= st end)
        final
        (recur (inc st) (conj final st))))))

(defcheck solution-f5b581e2
  (fn my-range [start end]
    (take (- end start) (iterate inc start))))

(defcheck solution-f7ad80d1
  (fn [start end] (take-while #(< % end) (iterate inc start))))

(defcheck solution-f859b6d8
  (fn f[x y] (if (= x y) '() (conj (f (+ x 1) y) x) )))

(defcheck solution-f87b6f53
  (fn [lo up]
    (take-while #(< % up) (iterate inc lo))))

(defcheck solution-f902fd30
  #(loop [a %1 col []]
     (if (>= a %2)
       col
       (recur (inc a) (conj col a)))))

(defcheck solution-fbc1bfae
  (fn rng [a b]
    (take-while #(< % b)
      (iterate inc a))))

(defcheck solution-fcc9a260
  (fn [min, max]
    ( (fn [m, n, acc] (if (= m n) acc (recur (inc m) n (conj acc m)))) min max []) ))

(defcheck solution-fd2ecef7
  #(loop [i %1 result []] (if (= i %2) result (recur (inc i) (conj result i)))))

(defcheck solution-fd78ea7
  (fn rng [a b] (if (< a b) (cons a (rng (inc a) b)) ())))

(defcheck solution-fd9b3ec1
  #(loop[s %2 e %1 res ()]
     (if (> s e)
       (recur (dec s) e (conj res (dec s)))
       res)))

(defcheck solution-fe312977
  (fn inner
    ([y] (inner 0 y))
    ([x y]
     (if (>= x y)
       '()
       (cons x (inner (inc x) y))))))

(defcheck solution-fe85a083
  (fn [first last]
    (loop [first first
           last last
           res []]
      (if (< first last)
        (recur (inc first) last (conj res first))
        res))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-34))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
