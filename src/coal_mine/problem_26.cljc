(ns coal-mine.problem-26
  (:require [coal-mine.checks :refer [defcheck-26] :rename {defcheck-26 defcheck}]
            [clojure.test]))

(defcheck solution-1009d0da
  (fn [n]
    (map second
      (take n
        (iterate (fn [[a0 a1]] [a1 (+ a0 a1)]) [0 1])))))

(defcheck solution-10314b8d
  #(map (fn fib [n] (case n 1 1 2 1 (+ (fib (dec n)) (fib (- n 2))))) (range 1 (inc %))))

(defcheck solution-109e369e
  #(reverse (nth (iterate (fn [[f s & r :as a]] (cons (+ f s) a)) [1 1]) (- % 2))))

(defcheck solution-10b83ac2
  #(loop [[a b & rest :as l] [1 1] n (- % 2)]
     (if (zero? n)
       (reverse l)
       (recur (cons (+ a b) l) (dec n)))))

(defcheck solution-11263de2
  (fn [n] (loop [l [1 1] p1 1 p2 1 c 2]
            (if (= c n)
              l
              (recur (conj l (+ p1 p2)) p2 (+ p1 p2) (inc c))))))

(defcheck solution-1168ef0f
  (fn [X]
    (loop [xs [1]
           m 0
           n 1
           X X]
      (if (> X 1)
        (recur (conj xs (+ m n)) n (+ m n) (dec X))
        xs))))

(defcheck solution-11900fc4
  #(take % ((fn fib-recur [a b] (cons a (lazy-seq (fib-recur b (+ a b) )))) 1 1)))

(defcheck solution-11ff55a8
  (fn [n1]
    (->>
      (iterate (fn [c] (cons (+ (first c) (first (rest c))) c)) '(1 0))
      (take n1)
      last
      reverse
      rest)))

(defcheck solution-1205ac61
  (fn
    [how-many]
    (let [fib [1 1]]
      (if (<= how-many 2)
        (take how-many fib)
        (let [how-many (- how-many 2)]
          (loop [currfib  fib
                 remaining how-many]
            (if (<= remaining 0)
              currfib
              (recur (conj currfib (apply + (take-last 2 currfib))) (dec remaining)))))))))

(defcheck solution-12787f6d
  (fn fib [n] (condp = n 1 [1] 2 [1 1] (let [xs (fib (dec n))] (conj xs (+ (last xs) (last (butlast xs))))))))

(defcheck solution-12997cd9
  (fn [n]
    (loop [n  n
           v0 1
           v1 1
           result []]
      (if (<= n 0)
        result
        (recur (dec n) v1 (+ v0 v1) (conj result v0))))))

(defcheck solution-129a0399
  (fn [x] (map (fn fib [y] (if (> y 1) (+ (fib (- y 2)) (fib (dec y))) 1)) (range 0 x))))

(defcheck solution-132ba749
  (fn fib [n]
    (loop [x 1 y 1 z 2 k '(1 1)]
      (if (== n z)
        k
        (recur y (+ x y) (inc z) (conj (vec k) (+ x y)))))))

(defcheck solution-136ef7a2
  #(letfn [(f [n a b] (lazy-seq (when (pos? n) (cons a (f (dec n) b (+ a b))))))] (f % 1 1)))

(defcheck solution-13948ca
  (fn [x]
    (cond
      (= x 1) [1]
      (= x 2) [1 1]
      true (loop [i (- x 2) coll '(1 1)] (if (= 0 i) (reverse coll) (recur (dec i) (conj coll (+ (first coll) (second coll))))))
      )))

(defcheck solution-13a32ba2
  (fn [n]
    (reverse
      (nth (iterate #(cons (apply + (take 2 %)) %) '(1 1))
        (- n 2)))))

(defcheck solution-13d92d10
  (fn [n]
    (loop [v [] i n a 0 b 1]
      (if (< i 1) v (recur (conj v b) (dec i) b (+ a b))))))

(defcheck solution-142f6576
  #(let [f (fn f [a b]
             (lazy-seq (cons a (f b (+ b a)))))]
     (take % (f 1 1))))

(defcheck solution-143bcfac
  (fn find[n] (take n ((fn fibo[a b] (lazy-seq (cons a (fibo b (+ a b))))) 1 1))))

(defcheck solution-1479159a
  (fn [x]
    (take x
      ((fn fib [a b]
         (cons a (lazy-seq (fib b (+ a b)))))
       1 1))))

(defcheck solution-159f3f32
  #(take %
     (map first
       (iterate (fn [[a b]] [b (+ a b)])
         [1 1]))))

(defcheck solution-15e6b8e8
  (fn fibo
    ([num]
     (fibo num 0 1 '()))
    ([num lower higher result]
     (if (< num 2)
       (concat result (list higher))
       (recur (dec num) higher (+ higher lower) (concat result (list higher)))))))

(defcheck solution-160dbffa
  (fn fib-seq [n]
    (take n
      ((fn rfib [a b]
         (lazy-seq (cons a (rfib b (+ a b)))))
       1 1)
      )
    ))

(defcheck solution-1614c765
  #(take % ((fn fib [a b] (cons a (lazy-seq b (fib b (+ a b))))) 1 1)))

(defcheck solution-16236d7d
  #(map second (take % (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))

(defcheck solution-163520f1
  (fn [x]
    (->> (iterate #(vector (% 1) (+ (% 0) (% 1))) [1 1])
      (map first)
      (take x))))

(defcheck solution-167f69f8
  (fn [n]
    (let [f (fn [n f']
              (if (or (= n 0) (= n 1))
                1
                (+ (f' (- n 2) f')
                   (f' (- n 1) f'))))]
      (map #(f % f) (range n)))))

(defcheck solution-16c8ad7e
  (fn [n] (seq (reduce #(conj %1 (+ (nth %1 %2) (nth %1 (inc %2)))) [1 1] (range 0 (- n 2))))))

(defcheck solution-16e23d38
  #(letfn [(fib [a b] (lazy-seq (cons a (fib b (+ b a)))))]
     (take % (fib 1 1))))

(defcheck solution-16f659ce
  (fn [num]
    (take num (map first
                (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-17266d27
  (fn [time]
    (loop [x1 0 x2 1 total time times 0 ret '()]
      (if (= total times)
        (reverse ret)
        (recur
          x2
          (+ x1 x2)
          total
          (+ times 1)
          (cons x2 ret)
          )
        )
      )
    ))

(defcheck solution-176d55af
  #(take % (map first (iterate (fn [[a b]] [(+ a b) a]) [1 0]))))

(defcheck solution-181fa3b0
  #(map (fn f_ [n]
          (cond (<= n 1) 1
                :else (+ (f_ (- n 1)) (f_ (- n 2))))) (range %)))

(defcheck solution-185977a2
  (fn [x]
    (loop [f 0 s 1 r [s] n 1]
      (if (= n x)
        (list* r)
        (recur s (+ f s) (conj r (+ f s)) (inc n))))))

(defcheck solution-18b1c213
  (fn [n] (last ( take (dec n)
                  (iterate
                    (fn[s](
                            conj s (+
                                    (nth s (- (count s) 1))
                                    (nth s (- (count s) 2))
                                    )
                            ))
                    [1 1]
                    )
                  )
            )))

(defcheck solution-18ed7ee6
  (fn [n]
    (letfn [(f [a b]
              (lazy-seq
                (let [x (+ a b)]
                  (cons x (f b x)))))]
      (take n (cons 1 (cons 1 (f 1 1)))))))

(defcheck solution-19f86f73
  (fn
    [n]
    (cond
      (= n 0) nil
      (= n 1) (seq [1])
      :else (loop [f2 0 f1 1 c 0 acc []]
              (if (= c n)
                (seq acc)
                (recur f1 (+ f2 f1) (inc c) (conj acc f1)))))))

(defcheck solution-1a15f915
  (fn [n]
    (map first (take n (iterate (fn [v] [(apply + v) (first v)]) [1 0])))))

(defcheck solution-1a195130
  #(loop [i (dec %) a 0 b 1 r [1]]
     (if (> i 0)
       (recur (dec i) b (+ a b) (conj r (+ a b)))
       r)))

(defcheck solution-1a691b75
  (fn [acc n]
    (if (< n 3)
      acc
      (recur (concat acc (list (+ (last acc) (last (butlast acc)))))
        (dec n)))) '(1 1))

(defcheck solution-1a6a6a22
  #(->> [0 1]
     (iterate (fn [[a b]] [(+ a b) a]))
     (map first)
     rest
     (take %)))

(defcheck solution-1a967a36
  #(map (fn [x]
          (letfn [(fib [n]
                    (if (< n 2)
                      1
                      (+ (fib (- n 1)) (fib (- n 2)))))]
            (fib x))) (range %)))

(defcheck solution-1b3d9ecf
  (fn[x] (take x (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-1b938d2
  (fn [n]
    (if (< n 3)
      (into [] (repeat 1))
      (loop [c 2, acc [1 1], n1 1, n2 1]
        (if (= c n)
          acc
          (let [next (+ n1 n2)]
            (recur (inc c) (conj acc next) n2 next)))))))

(defcheck solution-1c89fd2c
  (fn fib [n]
    (loop [s '(2 1 1) i (- n 3)]
      (if (zero? i)
        (reverse s)
        (recur (conj s (+ (first s) (second s))) (dec i))
        )
      )
    ))

(defcheck solution-1cedd52c
  (fn [n]
    (take n ((fn fib-recur [a b]
               (cons a (lazy-seq (fib-recur b (+ a b))))) 1 1))))

(defcheck solution-1d0308ba
  (fn [n] (loop [n1 1 n2 1 acc [1 1] r (- n 2)] (if (= r 0) acc (recur n2 (+ n1 n2) (conj acc (+ n1 n2)) (- r 1))))))

(defcheck solution-1d1a5afb
  #(take % (map (fn [[a b]] b)
             (iterate (fn [[a b]] [b (+ a b)])
               [0 1]))))

(defcheck solution-1de1b6c
  #(loop [fib [1] i 0 j 1 k %]
     (if (= 1 k)
       fib
       (recur (conj fib (+ i j)) j (+ i j) (dec k)))))

(defcheck solution-1e2f611d
  #(take %
     (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

(defcheck solution-1f216897
  #(loop [num %1 result () cur 0 st 1 nd 0]
     (if (= cur num)
       result
       (recur num (concat result (list (+ st nd))) (inc cur) nd (+ st nd)))))

(defcheck solution-1f3984af
  (fn [x]
    (map first (take x
                 (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-1f9658f4
  #(loop [k (- % 2) s [1 1]]
     (if (pos? k)
       (recur (dec k) (conj s (reduce + (take-last 2 s))))
       s)))

(defcheck solution-1fcb46a8
  #(take % (rest (map first (iterate (fn [[a b]] [(+ a b) a]) [0 1])))))

(defcheck solution-20349c9d
  ;; TDD principle: Write the minimum amount of code required to make the test pass :)
  (fn [i] (take i '(1 1 2 3 5 8 13 21))))

(defcheck solution-20834ca8
  (fn fibs [n]
    (loop [acc [1 1] m2 1 m1 1 n n]
      (cond
        (> n 2) (recur (conj acc (+ m2 m1)) m1 (+ m2 m1) (dec n))
        (= n 2) acc
        (= n 1) [1]
        :else []))))

(defcheck solution-20e50d38
  (fn [n]
    (take n (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-214244c5
  (let [f (fn [f a b] (cons a (lazy-seq (f f b (+ a b)))))] #(take % (f f 1 1))))

(defcheck solution-217759e8
  (fn  [fib-num]
    (loop [fib-list [1 1]
           county 0]
      (if (= county fib-num)
        (take fib-num fib-list)
        (recur (conj fib-list (apply + (take-last 2 fib-list))) (inc county))))))

(defcheck solution-21a47227
  (fn [x]
    (if (= x 1)
      '(1)
      (if (= x 2)
        '(1 1)
        (loop [x (- x 2) a 1 b 1 fibs '(1 1)]
          (if (zero? x)
            fibs
            (recur (dec x) b (+ a b) (concat fibs (vector (+ a b))))))))))

(defcheck solution-22269004
  (fn [n] (loop [i 1 a 1 b 1 result [1]] (if (< i n) (recur (inc i) b (+ a b) (conj result b)) result))))

(defcheck solution-227dff8a
  (fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-22c5e1e8
  #(take % ((fn fib [a b] (cons a (lazy-seq (fib b (+ b a))))) 1 1)))

(defcheck solution-22cc45a8
  (fn [n]
    (loop [n (- n 2)
           r '(1 1)]
      (if (= n 0)
        (reverse r)
        (recur (- n 1) (conj r (+ (first r) (first (rest r)))))))))

(defcheck solution-23782142
  (fn [n]
    (take n (map #(% 0) (iterate (fn [v] [(v 1) (+ (v 0) (v 1))]) [1 1])))))

(defcheck solution-23f47e06
  #(take % ((fn f [a b] (lazy-seq (cons a (f b (+ a b))))) 1 1)))

(defcheck solution-24099433
  (fn [cnt]
    (letfn [(fib ([] (concat [1 1] (fib 1 1)))
              ([a b] (lazy-seq (cons (+ a b) (fib b (+ a b))))))]
      (take cnt (fib)))))

(defcheck solution-24141c5e
  #(loop [a 1 b 1 n % l '()]
     (if (pos? n)
       (recur b (+ a b) (dec n) (conj l a))
       (reverse l))))

(defcheck solution-24ce916
  (fn [max]
    (loop [coll [1 1]]
      (if (= (count coll) max)
        coll
        (recur (conj coll (+ (last coll) (last (drop-last coll)))))))))

(defcheck solution-251976e8
  (fn [n]
    (let [fib (fn fib [n]
                (if (< n 3)
                  1
                  (+ (fib (- n 1)) (fib (- n 2)))))]
      (take n (map fib (drop 1 (range)))))))

(defcheck solution-253a6e88
  (letfn [(fib [x] (cond (= x 1) 1
                         (= x 2) 1
                         :else (+ (fib (- x 1)) (fib (- x 2)))))]
    #(map fib (range 1 (+ % 1)))))

(defcheck solution-2542d2ab
  (fn f ([n] (reverse (f n 0 '()))) ([n i a] (cond (= n i) a (< i 2) (f n (inc i) (cons 1 a)) 1 (f n (inc i) (cons (+ (first a) (second a)) a))))))

(defcheck solution-256d4d7b
  (fn fib [n] (if (< n 3) [1 1]
                          (let [s (fib (dec n)) ] (conj s (+ (last s) (last (butlast s))) ) ) )))

(defcheck solution-25a9e988
  (fn [n]
    (letfn [(F [n xs a b]
              (cond (zero? n) xs
                    :else (recur (dec n) (conj xs a) b (+ a b))))]
      (F n [] 1 1))))

(defcheck solution-25af5754
  (fn [n]
    (let [fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))]
      (take n fibs))))

(defcheck solution-25c5694d
  (fn [n] (reverse (reduce (fn [acc _] (conj acc (+ (first acc) (second acc)))) '(1 1) (range (- n 2))))))

(defcheck solution-26386ad9
  #(reduce (fn [l _] (conj l (+ (last l) (last (butlast l))))) [1 1] (range (dec (dec %)))))

(defcheck solution-26662018
  (fn [num]
    (->> (iterate (fn [v]
                    (conj v (apply + (take-last 2 v))))
           [1 1])
      (take (dec num))
      (last))))

(defcheck solution-267745fd
  (fn [limit]
    (loop [
           seq [1]
           last 1
           pre-last 0]
      (if (> limit (count seq))
        (let [fibo (+ last pre-last)]
          (recur (conj seq fibo) fibo last ))
        seq
        )
      )
    ))

(defcheck solution-26d17c05
  (fn [len]
    (last
      (take
        len
        (iterate
          (fn [arr]
            (conj
              arr
              (reduce + (take-last 2 arr))))
          [1])))))

(defcheck solution-276c86f0
  (fn [n]
    (loop [m 1, a 1, b 0, s []]
      (if (> m n) s
                  (recur (inc m) (+ a b) a (conj s a))))))

(defcheck solution-27c3fb50
  #(map first (rest (take (inc %) (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-27f1fce0
  (fn [x] (take x ((fn fib [a b] (cons a (lazy-seq (fib b (+ a b))))) 1 1))))

(defcheck solution-28084942
  #(take  % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-2873d1ec
  #(take % (map peek (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))

(defcheck solution-291a401b
  (fn [x] (letfn [(fib [p n] (lazy-seq (cons (+ p n) (fib n (+ p n)))))](take x (cons 1 (fib 0 1))))))

(defcheck solution-29b397fb
  #(loop [i (- % 1) acc [1]]
     (if (= i 0)
       acc
       (recur (dec i) (conj acc (apply + (take-last 2 acc)))))))

(defcheck solution-2a8ee5c4
  (fn fib [n]
    (cond
      (> n 2) (loop [ n (- n 2) aseq '[1 1]]

                (if (= n 0)
                  aseq
                  (recur (dec n) (conj aseq (+ (last aseq) (last (butlast aseq))) )))
                )
      )))

(defcheck solution-2b8cd251
  (fn [x] (take x (map first (iterate #(vector (+ (first %) (second %)) (first %)) [1 0])))))

(defcheck solution-2ba2487f
  (fn [n]
    (letfn [(step [a b]
              (lazy-seq
                (cons a (step b (+ a b)))))]
      (take n (step 1 1)))))

(defcheck solution-2bdd4b91
  #(take % ((fn fib [x y] (cons x (lazy-seq (fib y (+ x y))))) 1 1)))

(defcheck solution-2c387b44
  (fn
    [x]
    (reverse (nth
               (iterate #(conj % (+ (first %) (first (rest %))))
                 '(1 1))
               (- x 2)))))

(defcheck solution-2c819c86
  (fn [n]
    (reverse
      (loop [ls (list)]
        (if (< (count ls) n)
          (if (< (count ls) 2)
            (recur (conj ls 1))
            (recur (conj ls (+ (first ls) (first (rest ls))))))
          ls)))))

(defcheck solution-2cb86f4c
  (fn [num] (take num (map first (rest (iterate (fn [[x y]] [y (+ x y)]) [0 1]))))))

(defcheck solution-2d857d99
  (fn [size]
    (if (= size 0)
      []
      (loop [cnt 1 prev 0 ans [1]]
        (if (= cnt size)
          ans
          (recur (inc cnt) (last ans) (conj ans (+ prev (last ans))))
          )
        )
      )
    ))

(defcheck solution-2d904e71
  (fn [n] (map (fn fib [x]
                 (cond
                   (= 0 x) 1
                   (= 1 x) 1
                   :else (+ (fib (- x 1)) (fib (- x 2))))) (range n))))

(defcheck solution-2de92d4a
  (fn f ([arg] (take arg
                 (map first
                   (iterate (fn [[a b]] [b (+ a b)])
                     [1 1]
                     )
                   )))))

(defcheck solution-2e0e510f
  (fn [r] (map (fn fib [n] (cond (= n 1) 1
                                 (= n 2) 1
                                 :else (+ (fib (- n 1)) (fib (- n 2))))) (map inc (range r)))))

(defcheck solution-2e38af8c
  (fn [n]
    (take n
      (map first
        (iterate
          (fn [[a b]] [b (+ a b)])
          [1 1])))))

(defcheck solution-2e3cdfa3
  (fn [z]
    (reverse (loop [a 1, b 1, n z, s '()]
               (if (= n 0)
                 s
                 (recur b (+ a b) (- n 1) (conj s a)))))))

(defcheck solution-2ecc00ff
  (fn fib [n]
    (if (< n 2) '(1)
                (let [lst (fib (dec n))
                      m (apply + (take-last 2 lst))]
                  (concat lst (list m))))))

(defcheck solution-2f06d88d
  (fn fibo [x]
    (if (= x 1)
      [1]
      (if (= x 2)
        [1 1]
        (let [prev (fibo (dec x)), b (last prev), a (last (butlast prev))]
          (conj prev (+ a b))
          )
        )
      )
    ))

(defcheck solution-2f24920e
  #(loop [n 2 rv [1 1]]
     (if (= n %)
       rv
       (recur (inc n) (conj rv (apply + (take-last 2 rv)))))))

(defcheck solution-2f587288
  #(take % (map second (iterate (fn [[fst snd]] [snd (+ fst snd)]) [0 1]))))

(defcheck solution-306d3995
  (letfn [(fib ([]    (fib 1 1))
            ([a b] (cons a (lazy-seq (fib b (+ b a))))))]
    #(take % (fib))))

(defcheck solution-317a9052
  (fn [c]
    (loop [x [1 1]]
      (if (< (count x) c)
        (recur (conj x (apply + (take-last 2 x))))
        x))))

(defcheck solution-31e182ed
  #(map (fn f [x]
          (cond (= x 0) 1
                (= x 1) 1
                :else (+ (f (dec x)) (f (- x 2))))) (range %)))

(defcheck solution-31e572c
  (fn [numX]
    (take numX
      ((fn ! [a b]
         (cons a (lazy-seq (! b (+ b a))))
         ) 1 1))
    ))

(defcheck solution-324f8d51
  (fn [n]
    (letfn [(fib [a b]
              (lazy-seq (cons b (fib b (+ a b)))))]
      (take n (fib 0 1)))))

(defcheck solution-3284de00
  #(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1N 1N]))))

(defcheck solution-328d1372
  #(take % ((fn f [a b] (lazy-cat [a] (f (+ a b) a))) 1 0)))

(defcheck solution-33680ab7
  (fn fib [n]
    (letfn [(fib-seq [a b]
              (cons a (lazy-seq (fib-seq b (+ b a)))))]
      (take n (fib-seq 1 1)))))

(defcheck solution-33b342e
  #(loop [n % fibs ()]
     (if (> n 0)
       (recur (dec n) (conj fibs (+ (or (first fibs) 1) (or (second fibs) 0))))
       (reverse fibs))))

(defcheck solution-34827031
  (fn fib
    ([n] (fib n [1 1]))
    ([n s]
     (if (= n (count s))
       s
       (fib n (conj s (apply + (take 2 (rseq s)))))))))

(defcheck solution-3499024
  (fn fib [n]
    (if (< n 3)
      (repeat n 1)
      (let
       [prev (reverse (fib (dec n)))]
        (reverse (conj prev (+ (first prev) (second prev))))))))

(defcheck solution-350dba93
  #(last (take (dec %) (iterate (fn [s] (conj s (+ (last s) (last (butlast s))))) [1 1]))))

(defcheck solution-3543f6ec
  (fn [n]
    (take n
      ((fn fibo[a b]
         (cons a (lazy-seq(fibo b (+ a b))))) 1 1))
    ))

(defcheck solution-35591879
  (fn [n]
    (reverse ((fn  [x coll]
                (cond
                  (= x 0) coll
                  true (recur
                         (dec x)
                         (cons
                           (+ (first coll) (first (rest coll)))
                           coll
                           )
                         )
                  )
                ) (- n 2) (list 1 1) )
      )))

(defcheck solution-35ac3a69
  (fn [n]
    (map
      (fn fib [n]
        (if (< n 2)
          n
          (+ (fib (dec n)) (fib (- n 2))))) (range 1 (inc n)))))

(defcheck solution-361f99d0
  (fn [x]
    (->> [0 1]
      (iterate (fn [[a b]] [b (+ a b)]))
      (map last)
      (take x))))

(defcheck solution-368e4b20
  #(reverse ((fn recfib [n]
               (if (= n 1)
                 '(1)
                 (if (= n 2)
                   '(1 1)
                   (let [prevfib (recfib (dec n))]
                     (list* (reduce + (take 2 prevfib)) prevfib))))) %)))

(defcheck solution-37292e4c
  #((fn f [r n x y]
      (if (= n %)
        r
        (f (conj r x) (+ n 1) y (+ x y))))
    [] 0 1 1))

(defcheck solution-3775c3d
  #(loop [prev 0
          cur  1
          counter %
          result []]
     (if (= counter 1)
       (conj result cur)
       (recur cur (+ prev cur) (dec counter) (conj result cur)) )
     ))

(defcheck solution-37acef2c
  #(take %1
     (
      (fn fib [a b] (cons a (lazy-seq (fib b (+ a b)))))
      1 1)
     ))

(defcheck solution-37fe279f
  (fn fib
    ([n] (fib n 1 1))
    ([n x y] (if (= n 0) () (cons x (fib (dec n) y (+ x y)))))
    ))

(defcheck solution-3869db84
  #(loop [i % a 0 b 1 l '[1]]
     (if (= i 1) l
                 (let [x (+ a b)]
                   (recur (- i 1) b x (conj l x))))))

(defcheck solution-38824c80
  (partial
    (fn [n1 n2 coll n]
      (if (= n 2)
        (list* coll)
        (let [n3 (+ n1 n2)]
          (recur n2 n3 (conj coll n3) (dec n)))))
    1 1 [1 1]))

(defcheck solution-38fc5b7a
  #(take % [1 1 2 3 5 8 13 21]))

(defcheck solution-395812ed
  (fn [y](take y (flatten (iterate #(let[[a b] %] [(+ a b)(+ a b b)]) [1 1])))))

(defcheck solution-3969bb78
  #(loop [p 1 pp 1 i %1 ac [1 1]]
     (if (< i 3)
       ac
       (let [n (+ p pp)]
         (recur pp n (dec i) (conj ac n))))))

(defcheck solution-399d1161
  (fn [x]
    (map
      (fn fib [n]
        (if (<= n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2)))))
      (range x))))

(defcheck solution-39b46411
  #(take %
     (map first
       (iterate
         (fn [[n-2 n-1]]
           [n-1 (+ n-2 n-1)])
         [1 1]))))

(defcheck solution-3a19c37b
  #(loop [a 0, b 1, res [], i %] (if (zero? i) res (recur b, (+ a b), (conj res b), (dec i)))))

(defcheck solution-3a7f55fe
  (fn fib [i]
    (cond (< i 1) nil
          (= i 1) [1]
          (= i 2) [1 1]
          :else (let [r (fib (dec i))] (conj r (+ (get r (- i 3)) (get r (- i 2))))))))

(defcheck solution-3a8627d6
  (fn [n]
    (take n
      (map first
        (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-3b5266d4
  #(take %
     ((fn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))
      1 1)
     ))

(defcheck solution-3b573f72
  (fn fibs [a]
    (if (= a 2) [1 1]
                (let [aa (- a 1)
                      l (fibs aa)]
                  (conj l (+ (nth l (- aa 1)) (nth l (- aa 2))))
                  ))))

(defcheck solution-3b9ad7b3
  (fn print-fib [n]
    (map
      (fn fib [x]
        (case x
          1 1
          2 1
          (+ (fib (dec x)) (fib (dec (dec x))))))
      (range 1 (inc n)))))

(defcheck solution-3bb0e46d
  #(map (fn fibo [n]
          (if (< n 2)
            n
            (+ (fibo (- n 1)) (fibo (- n 2))))) (range 1 (inc %))))

(defcheck solution-3c2f6e31
  #(take % '(1 1 2 3 5 8 13 21)))

(defcheck solution-3cb4ac5a
  #(take %
     (map first
       (iterate (fn [[n1 n2]][(+ n1 n2) n1])
         [1 0]))))

(defcheck solution-3d922142
  #(take %
     (reduce
       (fn [coll el]
         (conj coll (+ (last coll) (last (butlast coll)))))
       [1 1] (range 10))))

(defcheck solution-3dc571a8
  (fn f [n]
    (let [mfib1 (memoize (fn fib1 [m] (if (<= m 2) 1 (+ (fib1 (- m 1)) (fib1 (- m 2))))))]
      (map mfib1 (range 1 (inc n))))))

(defcheck solution-3e24479d
  #(take % ((fn i2 [x y] (lazy-cat [x] (i2 y (+ x y)))) 1 1)))

(defcheck solution-3e33f841
  (fn fib
    ([num]
     (fib num 1 1))
    ([num n1 n2]
     (if (> num 0)
       (cons n1 (fib (- num 1) n2 (+ n1 n2)))))))

(defcheck solution-3e41a2db
  #(map (fn f [X]
          (cond
            (<= X 2) 1
            :else (+ (f (- X 1)) (f (- X 2)))))
     (range 1 (inc %))))

(defcheck solution-3eb64bdf
  #(loop [n % l '()]
     #_(println n)
     #_(println l)
     (if (> n 0)
       (recur (dec n) (conj l

                        ((fn fib [n]
                           (loop [n n, v1 1, v2 1]
                             (if (< n 3)
                               v2
                               (recur (dec n) v2 (+ v1 v2))))) n)))
       l)))

(defcheck solution-3ec34072
  (fn [n]
    (take n
      (map first
        (iterate #(let [[a b] %] [b (+ a b)]) [1 1])
        )
      )
    ))

(defcheck solution-3f86b13a
  (fn [fct]
    (loop [ct fct acc '()]
      (if (= 0 ct)
        (reverse acc)
        (recur (dec ct) (if (empty? acc)
                          '(1)
                          (cons (apply + (take 2 acc)) acc)))))))

(defcheck solution-4005fde6
  (fn fib [x]
    (cond
      (= x 0) []
      (= x 1) [1]
      (= x 2) [1 1]
      :else (let [fib-prev (fib (- x 1))]
              (conj fib-prev (->> fib-prev (drop (- x 3)) (apply +)))))))

(defcheck solution-4063e54
  (fn [x] (take x (map first (iterate (fn [[prev cur]] [cur (+ prev cur)]) [1 1])))))

(defcheck solution-40b4d72c
  (fn [n]
    (loop [a 0 b 1 result [1]]
      (if
       (= n (count result))
        result
        (recur b (+ a b) (conj result (+ a b)))))))

(defcheck solution-40e8528
  #(take %
     (map first
       (iterate
         (fn [[a b]] [b (+ a b)])
         [1 1]))))

(defcheck solution-40ed07df
  #(reverse ((fn fibs ([n x y & xs] (if (= 0 n)
                                      (concat [x y] xs)
                                      (apply fibs (dec n) (+ x y) x y xs)))
               ([n] (fibs (- n 2) 1 1)))
             %)
     ))

(defcheck solution-4116c1f9
  #(loop [i 0 coll []] (if (= i %) coll (recur (inc i) (conj coll (cond (= i 0) 1 (= i 1) 1 :else (apply + (take-last 2 coll))))))))

(defcheck solution-4165e2e
  (fn fib [x] (if (<= x 0) '() (if (= x 1) '(1) (if (= x 2) '(1 1) (let [fiblist (fib (- x 1))] (flatten (conj (list (+ (first (reverse fiblist)) (second (reverse fiblist)))) (seq fiblist)))))))))

(defcheck solution-41741185
  #(loop [xs [1 1]] (if (= % (count xs)) xs (recur (conj xs (apply + (take-last 2 xs)))))))

(defcheck solution-41b8e36a
  #(take % (map first
             (iterate (fn [[a b]] [b, (+ a b)]) [1 1])
             )))

(defcheck solution-41deb03f
  (fn fip [n]
    (if (= n 1)
      '(1)
      (if (= n 2)
        '(1 1)
        (concat
         (fip (- n 1))
         (list
           (+
            (last (fip (- n 1)))
            (first (rest (reverse (fip (- n 1)))))
            )
           )
         )
        ))
    ))

(defcheck solution-41e82f9a
  #(reverse ((fn ! [n]
               (if (< n 3) (take n '(1 1))
                           (let [x (! (- n 1))]
                             (conj
                               x
                               (+ (first x) (second x)))))) %)))

(defcheck solution-42a73132
  (fn fibonacci [n]
    (let [fib-next (fn [[a b]] [b (+ a b)])
          fib-pairs (fn [n]
                      (loop [fib-pair [1 1] accum [] n n]
                        (if (zero? n)
                          accum
                          (recur (fib-next fib-pair) (conj accum fib-pair) (dec n)))))]
      (map first (fib-pairs n)))))

(defcheck solution-42db1e75
  #(loop [x % ls '(1 1)] (if (< x 3) (reverse ls) (recur (dec x) (conj ls (+ (first ls) ((comp first rest) ls)))))))

(defcheck solution-431464fa
  (comp reverse (fn fib [x]
                  (if (= x 2)
                    '(1 1)
                    (let [f (fib (- x 1))]
                      (conj f (+ (first f) (second f))))))))

(defcheck solution-43274074
  (fn k
    ([n] (reverse (k (- n 2) '(1 1))))
    ([n fns]
     (if (zero? n)
       fns
       (k (dec n) (conj fns (+ (first fns) (second fns))))))))

(defcheck solution-4330672b
  (fn fibo [n]
    (rest
      ((fn [n]
         (loop [x 0, result []]
           (if (> x n)
             result
             (recur
               (+ x 1)
               (conj result (cond
                              (= x 0) 0
                              (= x 1) 1
                              :else (let [a (last result), b (first (take-last 2 result))] (+ a b))
                              )))
             )))
       n))))

(defcheck solution-4354cb86
  ; I should learn how to use memoization
  #(map (fn fib [n]
          (if (or (= n 0) (= n 1))
            1
            (+ (fib (- n 1)) (fib (- n 2)))))
     (range 0 %)))

(defcheck solution-4430ba9a
  (fn fab [n]
    (take n
      (map first (iterate (fn [[a b]] [b (+ a b)])
                   [1 1])))))

(defcheck solution-44f2d097
  #(loop [n (- % 2) f [1 1]]
     (if (zero? n)
       f
       (let [[f1 f2 & _] (reverse f)]
         (recur (dec n) (conj f (+ f1 f2)))))))

(defcheck solution-45c3f647
  #(take % (map first
             (iterate (fn [[a b]] [b (+ a b )])
               [1 1]))))

(defcheck solution-47d69b62
  #(nth (iterate (fn [x] (conj x (+ (nth x (- (count x) 1)) (nth x (- (count x) 2))))) [1 1])  (- % 2)))

(defcheck solution-4834fe1
  (fn [x]
    (loop [i '(1 1)]
      (if (= x (count i))
        (reverse i)
        (recur
          (conj i (apply + (take 2 i))))))))

(defcheck solution-48b067e5
  (fn [n]
    (loop [cnt n
           fibs '(1)]
      (if (= cnt 1)
        (reverse fibs)
        (recur (dec cnt)
          (conj fibs (apply + (take 2 fibs))))))))

(defcheck solution-48eb3a8c
  #(take %
     (map last
       (iterate
         (fn [[x y]] [y (+ x y)])
         [0 1]
         )
       )
     ))

(defcheck solution-4a1b2b8c
  #(take % ((fn f [a b] (cons a (lazy-seq (f b (+ a b))))) 1 1)))

(defcheck solution-4aec55fe
  #(loop [n %, last 0, current 1, acc '()]
     (if (< n 1)
       acc
       (recur (dec n) current (+ current last) (concat acc `(~current))))))

(defcheck solution-4bb34e20
  (fn [cnt] (loop [i 2 res '(1 1)] (if (= i cnt) (reverse res) (recur (inc i) (cons (+ (first res) (second res)) res))))))

(defcheck solution-4c05308f
  #(nth  (iterate
           (fn [s] (conj s (apply + (take-last 2 s)))) [1])
     (dec %)))

(defcheck solution-4c6b979b
  (fn fib-seq [x]
    (loop [counter 0
           fib ()]
      (if (= counter x)
        fib
        (recur (inc counter) (concat fib (if (< (count fib) 2)
                                           '(1)
                                           `(~(+ (last fib) (nth fib (- (count fib) 2)))))))))))

(defcheck solution-4c7d6bb9
  (fn fibn [up]
    (loop [ cup (- up 2), a 1, b 1, rc [1 1]]
      (if (< 0 cup)
        (recur (dec cup) b (+ a b) (conj rc (+ a b)))
        rc)
      )
    ))

(defcheck solution-4c9cdcad
  (fn [x] (loop [n (- x 2)
                 lst '(1,1)]
            (if (zero? n)
              (reverse lst)
              (recur (dec n) (cons (+ (first lst) (second lst)) lst))))))

(defcheck solution-4cbcaf33
  (fn this
    ([x] (this x 0 []))
    ([x n fseq]
     (cond
       (= x n) fseq
       (< n 2) (recur x (inc n) (conj fseq 1))
       :else
       (let [f (fseq (- n 1))
             s (fseq (- n 2))]
         (recur x (inc n) (conj fseq (+ f s))))))))

(defcheck solution-4cdd53b7
  (fn [nn]
    (loop [n (- nn 2) a 1 b 1 f [1 1]]
      (if (<= n 0)
        f
        (recur (dec n) b (+ a b) (conj f (+ a b)))
        ))))

(defcheck solution-4cef531
  (fn fib [n]
    (if (= n 2)
      [1 1]
      (conj (fib (- n 1)) (+ (last (fib (- n 1))) (first (rest (reverse (fib (- n 1)))))))
      )
    ))

(defcheck solution-4cf61444
  #(take %
     (map first
       (iterate (fn [[a b]] [b (+ a b)]) [1 1])
       )
     ))

(defcheck solution-4d31c99c
  (fn [n]
    "26. Write a function which returns the first X fibonacci numbers."
    (loop [i 2
           acc [1 1]]
      (if (>= i n)
        acc
        (recur (inc i) (conj acc (+ (last acc) (fnext (reverse acc)))))))))

(defcheck solution-4d94be14
  #(letfn [(worker [n l1 l2 v]
             (if (zero? n)
               v
               (recur (dec n) l2 (+ l1 l2) (conj v l1))))]
     (worker % 1 1 [])))

(defcheck solution-4da6b731
  (fn fib
    ([n] (fib (dec n) 0 1 [1]))
    ([n a b result] (if (pos? n)
                      (recur (dec n) b (+ a b) (conj result (+ a b)))
                      result))))

(defcheck solution-4db575d2
  (fn [n] (take n (cons 1 (letfn [(fibo-acc [a b] (lazy-seq (cons (+ a b) (fibo-acc b (+ a b)))))] (fibo-acc 0 1))))))

(defcheck solution-4dc7564c
  (fn fib [n] (take n (  (fn fibr [a b] (cons a (lazy-seq (fibr b (+ a b))))) 1 1)  )))

(defcheck solution-4ded5a51
  #(take % (map last(iterate (fn [[x y]] [y (+ x y)]) [ 0 1]))))

(defcheck solution-4df86ff4
  (fn [x]
    (loop [fib '(1 1) c x]
      (if (>= 2 c)
        (take x (reverse fib))
        (recur (conj fib (+ (first fib) (second fib)))
          (dec c))))))

(defcheck solution-4e370150
  #(loop [c (- % 2) s '(1 1)]
     (if (= c 0)
       (reverse s)
       (recur (- c 1) (conj s (+ (first s) (first (rest s))))))))

(defcheck solution-4e57b6a6
  #(for [n (range %)] ((fn fib [x] (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2))))) n)))

(defcheck solution-4e828bc
  (fn [x]
    (loop [
           res [1]
           la 1
           cou x
           ]
      (if (= cou 1)
        res
        (recur (conj res la) (+ (last res) la) (dec cou))))))

(defcheck solution-4ea8ddd0
  (fn [n]
    ((fn [a b n l]
       (if (= n 1)
         (conj l a)
         (recur b (+ a b) (dec n) (conj l a))))
     1 1 n [])))

(defcheck solution-4ee06b81
  #(take %
     (concat [1 1]
             ((fn f [i j] (lazy-cat [(+ i j)] (f j (+ i j)))) 1 1))))

(defcheck solution-4f3fba72
  (fn [n]
    (letfn [(helper [f1 f2]
              (let [f0 (+ f1 f2)]
                (lazy-seq (cons f0 (helper f0 f1)))))]
      (take n (helper 0 1)))))

(defcheck solution-4f74fb9d
  #(take % (map first (iterate
                        (fn [[x y]] [(+ x y) x])
                        [1 0]))))

(defcheck solution-4fce1602
  #(letfn [(fib [a b] (lazy-seq (cons b (fib b (+ a b)))))]
     (take % (fib 0 1))))

(defcheck solution-5005d2e7
  (fn [n]
    (loop [acc [1 1]
           a 1
           b 1]
      (if (>= (count acc) n)
        acc
        (recur (conj acc (+ a b)) b (+ a b))))))

(defcheck solution-50860734
  #(map first (take % (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-515a8cda
  #(letfn [(fib [a b] (cons a (lazy-seq (fib b (+ b a)))))] (take % (fib 1 1))))

(defcheck solution-52146e48
  (fn* [p1__2673#] (take p1__2673# ((fn fibs [] (lazy-cat [1 1] (map + (fibs) (rest (fibs)))))))))

(defcheck solution-5214f225
  (fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-523f1bb2
  (fn fib [x]
    (if (= x 2)
      [1 1]
      (let [l (fib (- x 1))]
        (conj
          l
          (+ (first (reverse l)) (second (reverse l)))
          )
        )
      )
    ))

(defcheck solution-524fa0b0
  (fn n26
    [x]
    (if (= x 1)
      [1]
      (if (= x 2)
        [1 1]
        (let
         [prev-list (n26 (dec x))]
          (conj
            prev-list
            (+ (last prev-list)
               (last (butlast prev-list)))))))))

(defcheck solution-52503fa1
  (fn [n]
    (->> (iterate (fn [[a b]] [b (+ a b)]) [0 1])
      (take n)
      (map last))))

(defcheck solution-526d14e1
  #(take % (conj (map (partial reduce +)
                   (iterate (fn [xs] [(last xs) (reduce + xs)])
                     [0 1]))
             1)))

(defcheck solution-52936f95
  (fn fib [n]
    (if (= n 1)
      '(1)
      (if (= n 2)
        '(1 1)
        (concat (fib (- n 1))
                [ (+ (nth (fib (- n 1)) (- n 2))
                     (nth (fib (- n 2)) (- n 3))) ])))))

(defcheck solution-5293d7e0
  (fn [n]
    (reduce
      (fn [res new]
        (conj res
          (+ (last res) (last(butlast res)))))
      [1 1] (range (- n 2)))
    ))

(defcheck solution-5360d47f
  (fn [n]
    (take n (map second (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-540e165f
  #(map first (take % (iterate (fn [[n m]] [m (+ n m)]) [1 1]))))

(defcheck solution-543abac8
  #(take % ((fn fib [x y] (lazy-seq (cons x (fib y (+ x y))))) 1 1)))

(defcheck solution-543f52f
  (fn [n]
    (let [p (iterate (fn [[a b]] [b (+ a b)]) [1 1])]
      (take n (map first p)))))

(defcheck solution-5440a942
  (fn [b] (let [fib-seq ((fn rfib [a b]
                           (lazy-seq (cons a (rfib b (+ a b)))))
                         0 1) limit (inc b)] (rest (take limit fib-seq)))))

(defcheck solution-54740577
  (let [fib (fn fib
              ([] (fib 0 1))
              ([a b] (lazy-seq (cons b (fib b (+ a b))))))
        ]
    #(take % (fib))))

(defcheck solution-547f56d2
  #(take %
     (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-54b57b20
  (fn fib2 [y]
    (letfn [(fib [x]
              (if (< x 2)
                1
                (+ (fib (- x 2)) (fib (- x 1)))))]
      (map fib (range y))
      )
    ))

(defcheck solution-54cd99d6
  #(take % (rest (map first (iterate (fn [[a b]]
                                       [b (+ a b)]) [0 1])))))

(defcheck solution-5594f3f5
  (fn [n]
    (take n
      ((fn fib [a b]
         (cons a (lazy-seq (fib b (+ a b))))
         ) 1 1)
      )
    ))

(defcheck solution-564ef520
  #(take % ((fn fib [a b]
              (lazy-seq (cons a
                          (fib b
                            (+ a b)))))
            1
            1)))

(defcheck solution-56c0270b
  #(take %
     (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))
     ))

(defcheck solution-56e02eb8
  (fn fibseq [x] (map (fn fib [x] (cond (= x 0) 0 (= x 1) 1 :else (+ (fib (- x 1)) (fib (- x 2))))) (range 1 (+ x 1)))))

(defcheck solution-571ef07d
  (fn [len]
    (loop [i 0 x 1 y 1 acc []]
      (if (= i len)
        acc
        (recur (inc i) y (+ x y) (conj acc x))))))

(defcheck solution-572b986c
  (fn fib[n]
    (if (< n 3)
      [1 1]
      (let [f (fib (dec n))
            [a b] (take-last 2 f)]
        (conj f
          (+ a b))))))

(defcheck solution-572ef04e
  (fn [n]
    (->> [1 1]
      (iterate (fn [[a b]] [b (+ a b)]))
      (map first)
      (take n))))

(defcheck solution-5744f3b3
  (fn f [n] (if (= n 1) [1] (if (= n 2) [1 1] (let [s (f (dec n))] (conj s (reduce + (take-last 2 s))))))))

(defcheck solution-578382a8
  #(take % (map first (iterate (fn [[a,b]] [b (+ a b)]) [1 1]))))

(defcheck solution-57fd8fde
  (let [fib (fn [n]
              (loop [c 0 p 1 t 0]
                (if (= c n)
                  t
                  (recur (inc c) t (+ p t)))))]
    #(map fib (range 1 (inc %)))))

(defcheck solution-58018fc7
  #(take % (map last
             (iterate (fn [[x y]] [y (+ x y)])
               [0 1]))))

(defcheck solution-5810353f
  (fn [x] (take  x ((fn rr [a b] (cons a (lazy-seq (rr b (+ a b))))) 1 1))))

(defcheck solution-581ea954
  (fn [n] (take n
            (map first
              (iterate
                (fn [[a b]]
                  [b (+ a b)])
                [1 1])))))

(defcheck solution-58d9f00c
  (fn [nbr]
    (->> (iterate (fn[[a b]] [b (+ a b)]) [1 1])
      (map first)
      (take nbr))))

(defcheck solution-58e455e2
  (fn fib [n]
    (if (< n 1) nil
                (if  (< n 2) [1]
                             (loop [a 1 b 1 c n r [1 1]]
                               (let [t (+ a b)]
                                 (if (= c 2) r (recur b t (dec c) (conj r t)))
                                 )
                               )
                             )
                )
    ))

(defcheck solution-59a44f84
  ;; tail-recursion for life
  (fn [n]
    (let [fs [1 1]]
      (if (< n 2)
        (take n fs)
        (loop [i 2 fibs fs]
          (if (= i n)
            fibs
            (recur (+ i 1)
              (concat fibs [(reduce + (take-last 2 fibs))]))))))))

(defcheck solution-59aff16d
  (fn [n]
    (letfn [(fib-help [bag remainder]
              (if (< remainder 1)
                bag
                (fib-help (conj bag (+ (peek bag) (peek (pop bag)))) (- remainder 1))))]
      (cond
        (< n 1) []
        (= n 1) [1]
        (= n 2) [1 1]
        :else (fib-help [1 1N] (- n 2))))))

(defcheck solution-59cd974a
  (fn [n]
    (letfn [(fib [past] [(reduce + past) (first past)])]
      (->> [1 0]
        (iterate fib)
        (map first)
        (take n)))))

(defcheck solution-5a7a8825
  #(take % (map first (iterate (fn [x] (list (second x) (+ (first x) (second x)))) '(1 1)))))

(defcheck solution-5ad36d17
  #(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-5b135fdb
  (fn fibo [n]
    (cond (= n 0) []
          (= n 1) [1]
          (= n 2) [1 1]
          :else
          (let [x (fibo (dec n))
                y (+ (first (reverse x)) (second (reverse x)))]
            (concat x [y])))))

(defcheck solution-5b3638c0
  (fn f [n]
    (if (= n 0)
      '()
      (concat (f (dec n))
              (list((fn fab [a]
                      (if (< a 3)
                        1
                        (+ (fab (- a 2)) (fab (- a 1)))))
                    n))))))

(defcheck solution-5bdd5a84
  #(take % ((fn fib [a b] (lazy-seq (cons a (fib b (+ a b))))) 1 1)))

(defcheck solution-5c582b94
  (fn [x] (take x (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-5ca7f8a0
  (fn [x] (take x (map first (iterate  (fn [[a b]] [b (+ a b)]) [1 1] )))))

(defcheck solution-5d49eff0
  (fn fibo [n]
    (letfn [(fiboRev [n]
              (cond
                (= n 1) '(1)
                (= n 2) '(1 1)
                :else (let [tail (fiboRev (- n 1))] (cons (+ (first tail) (first (rest tail)))
                                                      tail)
                                                    )
                )

              )] (reverse (fiboRev n))
                 )
    ))

(defcheck solution-5d85b35a
  #(nth
     (iterate (fn [s] (conj s (apply + (take-last 2 s)))) [1])
     (dec %)))

(defcheck solution-5da00c12
  (fn fib [n]
    (->> (rest (fib n))
      (map + (fib n))
      (lazy-cat [1 1])
      (take n))))

(defcheck solution-5dae2d04
  (fn fib [n]
    (take n
      ((fn ! [a b]
         (cons a (lazy-seq (! b (+ a b)))))
       1 1)
      )))

(defcheck solution-5df036b2
  (fn [n]
    (letfn [(fib-gen [a b]
              (lazy-seq
                (let [c (+ a b)]
                  (cons c (fib-gen b c)))))]
      (take n (concat [1 1] (fib-gen 1 1))))))

(defcheck solution-5e2868cd
  #(take % (map (fn fib [x]
                  (cond
                    (= x 0) 0
                    (= x 1) 1
                    :else (+ (fib (- x 1)) (fib (- x 2)))
                    )
                  ) (iterate inc 1))))

(defcheck solution-5e2a25e4
  (fn [n]
    (loop [n n
           a 1
           b 1
           f []]
      (if (zero? n)
        f
        (recur (- n 1) b (+ a b) (conj f a))))))

(defcheck solution-5e81cc7a
  (fn [len]
    (loop [to-go len
           result []]
      (if (= 0 to-go)
        (seq result)
        (recur (- to-go 1)
          (cond
            (= (count result) 0) [1]
            (= (count result) 1) [1 1]
            :else (conj result (+ (nth result (- (count result) 2)) (nth result (- (count result) 1))))))))))

(defcheck solution-5e86c859
  (fn fib [n]
    (take n
      (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-5f4f3614
  (fn fib [n]
    (if (= n 1) [1]
                (if (= n 2) [1 1]
                            (
                             #(conj % (+ (last %) (last (butlast %))))
                             (fib (- n 1)) )))))

(defcheck solution-5f509727
  (fn [n]
    (map
      (fn fibn [n]
        (
         (fn [n]
           (if (or (= n 0) (= n 1))
             1
             (+ (fibn (- n 1)) (fibn (- n 2))))) n))
      (range n))))

(defcheck solution-5f57ffd1
  (fn [n]
    (loop [fib [] n n a 0 b 1]
      (if (zero? n)
        fib
        (recur (conj fib b) (dec n) b (+ a b))))))

(defcheck solution-5f65fbb3
  (fn [count]
    (let
     [gen
      (fn gen
        [a b] (lazy-seq (cons (+ b a) (gen b (+ b a)))))]
      (take count (concat [1 1] (gen 1 1))))))

(defcheck solution-5f868491
  (fn [n]
    (take n
      (map first
        (iterate (fn [[a b]] [(+ a b) a])
          [1 0])))))

(defcheck solution-5fccf04a
  (fn [n]
    (loop [accum [1 1]]
      (if (= (count accum) n) accum
                              (recur (conj accum (+ (last accum)
                                                    (second (reverse accum))
                                                    )))))))

(defcheck solution-5fec14ab
  (fn [n] (reverse (nth (iterate (fn [coll] (concat [(+ (first coll) (second coll) )] coll )) [1 1]) (- n 2)))))

(defcheck solution-605ff4f1
  (fn [x]
    (take x (map first ( iterate (fn [[x,y]] [y (+ x y)]) [ 1 1 ])))))

(defcheck solution-60c21126
  #(take %
     (map first
       (iterate
         (fn [v] (let [[a b] v] [b (+ a b)]))
         [1 1]))))

(defcheck solution-6177daec
  #(letfn [(fib-seq
             ([] (concat [0 1] (fib-seq 0 1)))
             ([a b] (let [n (+ a b)]
                      (lazy-seq (cons n (fib-seq b n))))))]
     (take % (rest (fib-seq)))))

(defcheck solution-619668d7
  #(map (fn fib [n]
          (if (or (= n 2) (= n 1))
            1
            (+ (fib (dec n)) (fib (- n 2))))) (range 1 (inc %))))

(defcheck solution-61a4d8f0
  (fn [n]
    ((fn [v step]
       (if (= 0 step)
         v
         (let [c (count v)]
           (recur (conj v (apply + (subvec v (- c 2) c)))
             (dec step)))))
     [1 1] (- n 2))))

(defcheck solution-61fbd8
  #(for [cnt (range %)]
     (loop [n 1 m 1 c cnt]
       (if (not (zero? c))
         (recur m (+ n m) (dec c)) n))))

(defcheck solution-62389510
  (fn fib [n]
    (if (< n 3) (subvec [1 1] 0 n)
                (let [f (fib (dec n))]
                  (conj f (apply + (subvec f (- n 3))))))))

(defcheck solution-62a12161
  (fn [a] (take a (map first (iterate (fn [x] (list (second x) (+ (first x) (second x)))) '(1 1))))))

(defcheck solution-631d7b65
  (fn my-fibonacci [n]
    (letfn [(fib [a b] (cons a (lazy-seq (fib b (+ b a)))))]
      (take n (fib 1 1)))))

(defcheck solution-632aa584
  #(loop [a 0 b 1 n % s []]
     (if (zero? n)
       s
       (recur b (+ a b) (dec n)
         (conj s b)))))

(defcheck solution-634897ff
  (fn [ n ]
    (take n ((fn fibonacci
               ([] (cons 1 (lazy-seq (fibonacci 0 1))))
               ([a b] (cons (+ a b) (lazy-seq (fibonacci b (+ a b))))))))
    ))

(defcheck solution-6355ab7f
  (fn [n]
    (loop [x 2 fib [1 1]]
      (if (= n x)
        fib
        (recur (inc x) (conj fib (apply + (take-last 2 fib))))))))

(defcheck solution-635c63c5
  #(loop [i %1, fib-1 0, fib 1 rs  '()]
     (if (zero? i) rs
                   (recur (dec i) fib (+ fib-1 fib) (concat rs (list fib))))))

(defcheck solution-635e91d8
  #(->> [1 0]
     (iterate (fn [[x y]] [(+ x y) x]))
     (take %)
     (map first)))

(defcheck solution-63bcbe22
  (fn [n]
    (->> [0 1]
      (iterate (fn [[a b]] [b (+ a b)]))
      (map second)
      (take n))))

(defcheck solution-63e091f9
  (letfn [(fib [a b]
            (cons (+ a b)
              (lazy-seq (fib b (+ a b)))))]
    #(take % (lazy-cat [1 1] (fib 1 1)))))

(defcheck solution-63ecf62c
  #(map first (take % (iterate (fn [[a b]] [b (+ a b)]) [1 1]) )))

(defcheck solution-647839ee
  (fn [n]
    (letfn [
            (iter [xs c]
              (cond
                (> c n) xs
                (= c 0) (recur [] 1)
                (= c 1) (recur [1] 2)
                (= c 2) (recur [1 1] 3)
                :else   (recur (conj xs (+ (last xs)
                                           (last (butlast xs))))
                          (inc c))))]
      (iter [] 0))))

(defcheck solution-64ce74f
  (fn [n]
    (letfn [(fib [m] (case m
                       0 1
                       1 1
                       (+ (fib (- m 1)) (fib (- m 2)))))]
      (take n (map fib (range))))))

(defcheck solution-64fa788a
  (fn [n]
    (take n
      (map first
        (iterate
          #(vector (last %) (reduce + %))
          [ 1 1 ] )))))

(defcheck solution-64fc007f
  (fn [n]
    (if (= n 1)
      '(1)
      (loop [a 1 b 1 x 2 c [1 1]]
        (if (= x n)
          c
          (recur b (+ a b) (inc x) (conj c (+ a b))))))))

(defcheck solution-652ffd6a
  (fn [n] (cond (= n 1) [1]
                (= n 2) [1 1]
                :else (let [fibr (fn [m x]
                                   (if (> m 2) (recur (dec m) (conj x
                                                                (+ (last x) (last (drop-last x)))
                                                                )) x))
                            ]
                        (fibr n [1 1])))))

(defcheck solution-66917c4d
  #(take % ((fn rfib [a b]
              (lazy-seq (cons a (rfib b (+ a b))))) 1 1)))

(defcheck solution-6709f527
  (fn [n](reverse(nth
                   (iterate
                     #(conj % (+ (first %) (second %)))
                     '(1 1) )
                   (- n 2)))))

(defcheck solution-6795c69c
  (fn [n] (if (= n 0) '()
                      (if (= n 1) '(1)
                                  (loop [v n lst '(1 1)]
                                    (if (= v 2) (reverse lst)
                                                (recur (- v 1) (conj lst (+ (first lst) (second lst))))))))))

(defcheck solution-67e739e2
  (fn fib [n] (reverse (loop [a 1
                              b 1
                              i 0
                              result '()]
                         (if (= i n)
                           result
                           (recur b (+ a b) (inc i) (cons a result)))))))

(defcheck solution-683133e
  (fn [findex]
    (loop [i 0 coll []]
      (cond
        (= i findex) coll
        (= i 0) (recur (inc i) [1])
        (= i 1) (recur (inc i) [1 1])
        :else (recur (inc i) (conj coll (+ (last coll) (nth coll (- (count coll) 2))) ))))))

(defcheck solution-684db57a
  (fn fib
    ([cnt] (fib cnt '(1 1)))
    ([cnt sq]
     (if (= cnt (count sq))
       sq
       (fib cnt (conj (vec sq) (reduce + (take-last 2 sq))))))))

(defcheck solution-68a10598
  (fn fib[x]
    (take x
      (map first
        (iterate
          (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-68aaade0
  (fn [n]
    (loop [a 0 b 1 r [] i 0 ]
      (if(= i n) r
                 (recur b (+ a b) (conj r b) (inc i))))))

(defcheck solution-68b12565
  #(loop [counter (- % 2)
          result (vector 1 1)
          n-2 1
          n-1 1]
     (if (< counter 1)
       result
       (recur (dec counter) (conj result (+ n-2 n-1)) n-1 (+ n-2 n-1)))))

(defcheck solution-69569ce1
  #(letfn [(fib
             ([] (fib 1 1))
             ([a b] (lazy-seq (cons a (fib b (+ a b))))))]
     (take % (fib))))

(defcheck solution-6957ae6f
  #(->> [0 1]
     (iterate (fn [[a b]] [b (+ a b)]))
     (take %)
     (map second)))

(defcheck solution-69cd328
  #(take % (letfn [(f [n m] (cons n (lazy-seq (f m (+ n m)))))] (f 1 1))))

(defcheck solution-69fb9d6e
  (fn [n]
    (loop [x n cont (fn [x] x) p 1 pp 0]
      (if (zero? x)
        (cont () )
        (recur (- x 1) (fn [x] (cont (cons p x))) (+ p pp) p)
        ))))

(defcheck solution-6a50994e
  (letfn [(genfib [a b] (lazy-seq (cons a (genfib b (+ a b)))))]
    (fn [n] (take n (genfib 1 1)))))

(defcheck solution-6ab4d359
  (fn fib [x]
    (if (< x 3)
      [1 1]
      (conj (fib (- x 1)) (+ (nth (fib (- x 1)) (- x 2)) (nth (fib (- x 1)) (- x 3))))
      )
    ))

(defcheck solution-6ab816eb
  #(take % ((fn fb[] (lazy-cat [1 1] (map + (fb) (next (fb))))))))

(defcheck solution-6ac4c357
  (fn [n]
    (take n
      (map first
        (iterate (fn [[cur prev]] [(+ cur prev) cur])
          [1 0])))))

(defcheck solution-6b11ea63
  (fn [n] (let [foo (fn foo [x y] (let [z (+ x y)] (lazy-seq (cons y (foo y z)))))] (take n (foo 0 1)))))

(defcheck solution-6ba06996
  #((fn [x] (if (= (count x) %)
              x
              (recur
                (conj x (+ (last x) (nth x (- (count x) 2 )))
                  )
                )
              )) [1 1]))

(defcheck solution-6be27324
  #(take % (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

(defcheck solution-6c07afb7
  (fn [num] (reduce #(cond (= %2 0) [1] (= %2 1) [1 1] :else (conj %1 (+ (%1 (- %2 1)) (%1 (- %2 2))))) [] (range num))))

(defcheck solution-6c39cee0
  (fn [x]
    (let [fibo ((fn rfib [a b]
                  (lazy-seq (cons a (rfib b (+ a b))))) 0 1)]
      (take x (rest fibo)))))

(defcheck solution-6c7dc47f
  (fn [number]
    (loop [result [] current 0]
      (if (= current number)
        result
        (recur
          (if (<= current 1)
            (conj result 1)
            (conj result (+ (nth result (- current 1)) (nth result (- current 2))))
            ) (inc current))))))

(defcheck solution-6ca062ba
  (fn [n]
    (loop [n1 1, n2 1, i (- n 2), acc [1 1]]
      (if (= 0 i)
        acc
        (recur n2 (+ n1 n2) (dec i) (conj acc (+ n1 n2)))))))

(defcheck solution-6d6956a6
  (fn [n]
    (let [fib (fn [x, self]
                (cond
                  (= 1 x) 1
                  (= 2 x) 1
                  :else (+ (self (- x 1) self) (self (- x 2) self))
                  ))]
      (map (fn [x] (fib x fib)) (range 1 (+ 1 n)))
      )
    ))

(defcheck solution-6d85a51d
  (fn [n] (loop [in 1 out '(1)] (if (= in n) (reverse out) (recur (inc in) (conj out (apply + (take 2 out))))))))

(defcheck solution-6d8db98f
  #_
      (fn [n]
        (seq
          (nth
            (iterate
              (fn [xs]
                (let [[a b] (take-last 2 xs)]
                  (conj xs (+ a b))))
              [1 1])
            (- n 2))))

  #_
      (fn [n]
        (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

  (fn [n]
    (->> [1 1]
      (iterate (fn [[a b]] [b (+ a b)]))
      (map first)
      (take n))))

(defcheck solution-6dab2053
  #(reverse (reduce (fn [acc _] (cons (+ (first acc) (second acc)) acc)) [1 1] (range (- % 2)))))

(defcheck solution-6dba2760
  (fn fibo
    ([fibos limit]
     (let [a (first fibos)
           b (second fibos)
           result (+ a b)]
       (if (= (count fibos) limit) (reverse fibos)
                                   (recur (conj fibos result) limit))))
    ([limit] (fibo (list 1 1) limit))))

(defcheck solution-6dbbd984
  #(take % (map first
             (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

(defcheck solution-6dd0bb05
  (fn [n]
    (take n
      ((fn fib [a b]
         (cons a (lazy-seq (fib b (+ a b)))))
       1 1))))

(defcheck solution-6dd56ade
  (fn [n]
    (let [fib (fn fib [n]
                (cond
                  (= n 1) 1
                  (= n 2) 1
                  :else (+ (fib (- n 1)) (fib (- n 2)))
                  )
                )]

      (map fib (range 1 (+ n 1)))
      )
    ))

(defcheck solution-6e3ba96b
  (fn [n]
    (take n
      (map first
        (iterate (fn [[f s]] [s (+ f s)])
          [1 1])))))

(defcheck solution-6e569b53
  (fn [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      true (loop [i 2 s [1 1]]
             (let [n-1 (first (reverse s))
                   n-2 (second (reverse s))]
               (if (= i n)
                 s
                 (recur (inc i) (conj s (+ n-2 n-1)))))))))

(defcheck solution-6f6e1585
  (fn [x] (reduce (fn [a b] (conj a (apply + (take-last 2 a)))) [1 1] (range 1 (- x 1)))))

(defcheck solution-6f8e2e0
  #(loop [i 0 fib_prev 0 fib 1 fib_seq []]
     #_(print fib " ")
     (if (= i %1)
       fib_seq
       (recur (inc i) fib (+ fib_prev fib) (conj fib_seq fib )))
     ))

(defcheck solution-7045761
  #(letfn [(fib [x]
             (if (< x 3)
               1
               (+ (fib (- x 1)) (fib (- x 2)))))]
     (for [n (range 1 (inc %))]
       (fib n))))

(defcheck solution-705c8b56
  (fn fib [n]
    (last (take (dec n) (iterate #(conj % (+ (last %) (last (butlast %)))) [1 1])))))

(defcheck solution-7061001b
  (fn [n]
    (take n
      (map first
        (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-70abea3c
  #((fn [n f n0 n1] (if (zero? n) f (recur (- n 1) (conj f n1) n1 (+ n0 n1)))) % [] 0 1))

(defcheck solution-70f866cf
  #(reduce
     (fn [s i]
       (let [rs (reverse s)
             a1 (first rs)
             a2 (second rs)]
         (conj s (+ a1 (if a2 a2 0)))))
     [1]
     (range (dec %))))

(defcheck solution-716de0a5
  (fn fibs[n]
    (map
      (fn fib[n]
        (if (< n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2)))))
      (range n))))

(defcheck solution-7179f8b
  #(map (fn fib[n] ( if (<= n 2) 1 (+ (fib (- n 2)) (fib (- n 1))))) (range 1 (inc %))))

(defcheck solution-71b30e8f
  #(loop[i %, r '(1 1)]
     (if (= i 2)
       (reverse r)
       (recur (dec i) (conj r (+ (first r) (second r)))))))

(defcheck solution-721ff283
  (fn [n] (let [start [1 1 2]] (do (if (<= n 3) (take n start) (loop [cnt 1 end start] (do (if (<= cnt (- n 3)) (recur (inc cnt) (conj end (+ (last end) (last (butlast end))))) (seq end)))))))))

(defcheck solution-722baee7
  #(letfn [(impl [n lst x0 x1] (if (== n 0) lst (impl (- n 1) (conj lst x1) x1 (+ x0 x1))))] (impl % [] 0 1)))

(defcheck solution-722d5dca
  (fn f [n]
    (loop [i 0 f' 1 f'' 0 r []]
      (if (= i n)
        r
        (let [f''' (+ f' f'')]
          (recur (inc i) f'' f''' (conj r f''')))))))

(defcheck solution-724a1514
  (fn [n]
    (case n
      1 '(1)
      (loop [ret '(1 1) i 2]
        (if (= i n)
          (reverse ret)
          (recur
            (cons (+ (nth ret 0) (nth ret 1)) ret)
            (inc i)))))))

(defcheck solution-72629253
  #(take % (list* 1 (map first
                      (iterate (fn [[m n]] [(+ m n) m]) [1 1])))))

(defcheck solution-72d6afbb
  (fn [z] (reverse (#(nth (iterate (fn [x] ( cons (+ (first x) (second x) ) x  ) )  '(1 1)) (- z 2))))))

(defcheck solution-7313619f
  (fn n-first-fib [n]
    (loop [i n acc [1 1] fn 1 fn- 1]
      (if
       (zero? (- i 2))
        acc
        (recur (dec i) (conj acc (+ fn fn-)) fn- (+ fn fn-))))))

(defcheck solution-73a6cdc4
  (fn [c] (take c ((fn foo [a b]
                     (let [n (+ a b)]
                       (cons a (lazy-seq (foo b n))))) 1 1))))

(defcheck solution-74b48aeb
  (fn [x]
    (loop [to-go x result [] prev 1 pprev 0]
      (if (= to-go 1)
        (cons 1 result)
        (recur (dec to-go) (conj  result (+ prev pprev)) (+ prev pprev) prev)))))

(defcheck solution-74c45934
  (fn fib [x]
    (loop [acc  [0 1]
           x   x]
      (if (< x 2)
        (rest acc)
        (recur (conj acc (+ (last (butlast acc)) (last acc))) (dec x))))))

(defcheck solution-758d46a9
  #(loop [s [], prev 0, nxt 1, n %]
     (if (= n 0)
       s
       (recur (conj s nxt) nxt (+ prev nxt) (dec n)))))

(defcheck solution-75cb5aa5
  #(if (< % 3)
     (repeat % 1)
     (loop [fib [1 1]
            left %]
       (if (< left 3)
         fib
         (recur (conj fib (apply + (take-last 2 fib))) (dec left))))))

(defcheck solution-75ceb7df
  (fn
    [n]
    (letfn [(gen-fibs
              [n]
              (condp = n
                0 []
                1 [1]
                2 [1 1]
                (let [fibs (gen-fibs (dec n))]
                  (conj fibs (+ (peek fibs)
                                (peek (pop fibs)))))))]
      (gen-fibs n))))

(defcheck solution-75e24242
  #(take % ((fn rfib [a b]
              (cons a (lazy-seq (rfib b (+ a b)))))
            1 1)))

(defcheck solution-75ecc034
  #(loop [f1 1
          f2 1
          cnt 2
          res [1 1]]
     (if (= cnt %)
       res
       (recur f2 (+ f1 f2) (inc cnt) (conj res (+ f1 f2)))
       )
     ))

(defcheck solution-75fa4c0
  (fn [y] (loop [x 2
                 r [1 1]]
            (if (< x y)
              (recur (inc x) (conj r (+ (nth r (- x 2)) (nth r (- x 1)) )))
              r))))

(defcheck solution-769ce6ba
  (letfn [(fibonacci [n m]
            (cons n (lazy-seq (fibonacci m (+ n m)))))]
    #(take % (fibonacci 1 1))))

(defcheck solution-76c7c057
  (fn [n]
    (case n
      0 '()
      1 '(1)
      2 '(1 1)
      (reverse (loop [n (- n 2) acc '(1 1)]
                 (if (= n 0)
                   acc
                   (recur (- n 1) (cons (+ (first acc) (second acc)) acc))))))))

(defcheck solution-76fd72f2
  (fn fib [n]
    ((fn [s a b n]
       (if (= 0 n)
         s
         (recur (conj s a) b (+ a b) (dec n)))) [] 1 1 n)))

(defcheck solution-77756517
  (fn fib [n]
    (loop [xs [] a 1 b 1 n n]
      (if (zero? n)
        xs
        (recur (conj xs a)
          b
          (+ a b)
          (dec n))))))

(defcheck solution-77e0e156
  (fn [fibs]
    (loop [n 3 result [1 1]]
      (if (> n fibs)
        (take fibs result)
        (recur (inc n)
          (conj result (+ (peek result) (peek (pop result)))))))))

(defcheck solution-78043b62
  #(letfn [(fib [a b]
             (lazy-seq
               (cons a
                 (fib b (+ a b)))))]
     (take % (fib 1 1))))

(defcheck solution-7829076f
  (fn [n] (map first (take n (iterate (fn [[a b]] [b (+ a b)])  [1 1] )))))

(defcheck solution-782c6a17
  (fn [n]
    (reverse
      ((fn fib [x]
         (cond
           (= x 1) '(1)
           (= x 2) '(1 1)
           :else (let [xs (fib (dec x))] (conj xs (+ (first xs) (second xs))))))
       n))))

(defcheck solution-78dc314f
  (fn [n]
    (loop [cnt n
           acc '(1)
           h 0]
      (if (<= cnt 1)
        (reverse acc)
        (recur (dec cnt)
          (cons (+ (first acc) h) acc)
          (first acc)
          )
        )
      )

    ))

(defcheck solution-7b029e23
  (fn [n]
    (take n
      (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-7b332cb
  #(map second (take %1 (iterate (fn [[a b]] [b (+ a b)]) '(0, 1)))))

(defcheck solution-7b34d1e6
  (fn [n] (map first (take n (iterate #(list (second %) (apply + %)) '(1 1))))))

(defcheck solution-7bba1a16
  (fn [n] (take n (map second (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-7c1905b
  (fn [n]
    (let [f (fn self [n]
              (cond (<= n 1) '(1)
                    (= n 2) '(1 1)
                    :else (let [[a b :as xs] (self (dec n))]
                            (conj xs (+ a b)))))]
      (reverse (f n)))))

(defcheck solution-7c5299ed
  (fn [n]
    (take n
      ((fn rfib [a b]
         (lazy-seq (cons a (rfib b (+ a b)))))
       1 1))))

(defcheck solution-7cbeeeef
  (fn fib[n]
    (loop[pre 0 next 1 acc 1 coll '(1)]
      (if (>= acc n) (reverse coll)
                     (recur next (+ pre next) (+ acc 1) (cons (+ pre next) coll ))))))

(defcheck solution-7cc5decd
  #(letfn [(fib [a b] (cons a (lazy-seq (fib b (+ a b)))))]
     (take % (fib 1 1))))

(defcheck solution-7ccd92ba
  (fn fib
    ([n] (take n (fib 0 1)))
    ([a b] (cons b (lazy-seq (fib b (+ a b)))))))

(defcheck solution-7d22b1d1
  #(reverse
     (loop [n % fibs '()]
       (if (= n 0)
         fibs
         (recur (dec n) (conj fibs (if (not-empty fibs)
                                     (+ (first fibs) (or (second fibs) 0))
                                     1)))))))

(defcheck solution-7d25e8a8
  (fn [cnt]
    (loop [a 1 b 1 i 1 res [1]]
      (if (= i cnt)
        res
        (recur b (+ a b) (inc i) (conj res b))))))

(defcheck solution-7d4dd2f9
  (fn [idx]
    (loop [pp 0
           p  1
           l  (range (- idx 1))
           acc [1]]
      (if (empty? l) acc
                     (recur p (+ p pp) (rest l) (conj acc (+ p pp)))))))

(defcheck solution-7d77b650
  #(reverse (reduce (fn [r _] (cons (apply + (take 2 r)) r)) '(1 1) (range (- % 2)) )))

(defcheck solution-7d79790c
  (fn [n]
    (map last (take n (iterate #(vector (last %) (+ (first %) (last %))) [0 1])))))

(defcheck solution-7da54549
  #(take %
     (map last
       (iterate
         (fn [[a b]]
           [b (+ a b)])
         [0 1]))))

(defcheck solution-7da6147b
  #(take % ((fn f [x y]
              (cons x (lazy-seq (f y (+ x y))))) 1 1)))

(defcheck solution-7df4a4fd
  (fn fib [v]
    (loop [i 0, xs '()]
      (cond (= i v) xs
            (= i 0) (recur (inc i) '(1))
            (= i 1) (recur (inc i) '(1 1))
            (< i v) (recur (inc i) (concat xs (list (+ (last xs) (first (take-last 2 xs))))))))))

(defcheck solution-7dfeed6f
  (fn [x]
    (loop [s [1] cur x p1 0 p2 1]
      (if (< cur 2)
        s
        (recur (conj s (+ p1 p2)) (dec cur) p2 (+ p1 p2))))))

(defcheck solution-7e19cf6b
  (fn fibo
    ([n]
     (fibo n 1 1))
    ([n a b]
     (if (pos? n)
       (cons a (fibo (dec n) b (+ a b)))))))

(defcheck solution-7e87e7c8
  (fn f [n]
    (take n (cons 1 (map #(apply + %) (iterate (fn [[a b]] [b (+ a b)]) '(0 1)))))))

(defcheck solution-7e8fca90
  #(take % ((fn fib [a b]
              (cons a (lazy-seq (fib b (+ a b))))) 1 1)))

(defcheck solution-7eaa043d
  (fn [i]
    (reduce
      (fn [x y]
        (cond
          (= (count x) 0) '(1)
          (= (count x) 1) '(1 1)
          true (concat x (list (apply + (take-last 2 x))))))
      '() (range i))))

(defcheck solution-7ec82f9
  (fn [x]
    (let [rfibs (fn fibs [n]
                  (let [n-1 (dec n)]
                    (cond (= n 1)  '(1)
                          (= n 2) '(1 1)
                          :else (cons (+ (first (fibs n-1)) (second (fibs n-1)) ) (fibs n-1))
                          )
                    )
                  )]
      (reverse (rfibs x)))))

(defcheck solution-7ed1b7d8
  #(reduce (fn [a e] (conj a
                       (+ (a e) (a (inc e))))) [1 1] (range (- % 2))))

(defcheck solution-7eeeb5cf
  (fn fstFib [x]
    (take x ((fn fib [a b]
               (lazy-seq (cons a (fib b (+ a b)))))1 1))))

(defcheck solution-7fa905b2
  (fn [n]
    (loop [a 1 b 1 k n c []]
      (condp = k
        0 c
        1 (conj c a)
        2 (conj c a b)
        (recur b (+ a b) (- k 1) (conj c a))))))

(defcheck solution-7fad24db
  #(take %
     (map first
       (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-8010e792
  (fn [x] (reverse(last( take (- x 1) (iterate #(into % (list(+ (first %)(second %))))'(1 1)))))))

(defcheck solution-80d0f1ba
  (fn fib-list [n]
    (cond (= n 1) '(1)
          (= n 2) '(1 1)
          :else (concat (fib-list (- n 1)) (list (+ (last (fib-list (- n 1)))
                                                    (last (fib-list (- n 2)))
                                                    ))))
    ))

(defcheck solution-8114c261
  (fn fib
    [n]
    (loop [a 0
           b 1
           accum []
           i 0]
      (if (= i n)
        (seq accum)
        (recur b (+ a b) (conj accum b) (inc i))))))

(defcheck solution-813c754c
  (fn [n] (take n ((fn fib [a b] (cons a (lazy-seq (fib b (+ a b))))) 1 1))))

(defcheck solution-814f3b28
  (fn [n]


    (loop [a 1 b 1  cnt 0 res []]
      (if (= cnt n)
        res

        (recur b (+ a b) (inc cnt  ) (conj res a))

        )


      )
    ))

(defcheck solution-8264fdab
  #(take % (letfn [(nval [x y] (lazy-seq (cons (+ x y) (nval y (+ x y)))))] (lazy-seq (cons 1 (cons 1 (nval 1 1)))))))

(defcheck solution-82afd799
  #(letfn [(f [a b] (cons (+ a b) (lazy-seq (f b (+ a b)))))]
     (take % (concat [1 1] (f 1 1)))))

(defcheck solution-834d753e
  (fn f [n] (if (= 1 n) [1]
                        (let [f_ (f (dec n))]
                          [1]
                          (conj f_ (apply + (take-last 2 f_)))))))

(defcheck solution-835ac8b1
  (fn fib [x]
    (case x
      0 nil
      1 '(1)
      2 '(1 1)
      (reverse
        (cons
          (+ (last (fib (dec x)))
             (last (fib (- x 2))))
          (reverse (fib (dec x))))))))

(defcheck solution-83878c52
  #(take %1 (map first (rest (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-83a4f6c
  (fn [x]
    (loop [l [1 1], n 2]
      (if (= n x)
        l
        (recur
          (conj l (+ (nth l (- n 1)) (nth l (- n 2))))
          (inc n) )))))

(defcheck solution-83fb5cb2
  (fn [n]
    (letfn [(fibs-from [x y]
              (lazy-seq
                (cons (+ x y) (fibs-from y (+ x y)))))]
      (let [all-fibs (cons 1 (cons 1 (fibs-from 1 1)))]
        (take n all-fibs)))))

(defcheck solution-843f4898
  (fn fibonacci [x]
    (cond
      (= x 1) [1]
      (= x 2) [1 1]
      (>= x 3) (concat (fibonacci (- x 1)) [(#(+ (last %) (last (butlast %))) (fibonacci (- x 1)))]))))

(defcheck solution-84c8dfa7
  (fn [length]
    (let [fibseqs (iterate #(conj % (+ (first %) (second %))) '(1 0))
          fibseq (nth fibseqs (dec length))]
      (rest (reverse fibseq)))))

(defcheck solution-856bce4d
  (fn [x]
    (loop [s '(1 1) o 1 p 1 n 2 c 2]
      (if (= x c) (reverse s)
                  (recur (cons n s) p n (+ p n) (inc c))
                  )
      )
    ))

(defcheck solution-85a5b08f
  (fn [n]
    (loop [n-1 0
           n-2 0
           c n
           fib []]
      (let [n0 (if (zero? n-1) 1 (+ n-1 n-2))]
        (if (zero? c) fib
                      (recur n0 n-1 (dec c) (conj fib n0)))))))

(defcheck solution-869da36d
  #(rest (reverse (reduce (fn [[a b & c :as d] _] (conj d (+ a b))) '(1 0) (range (dec %))))))

(defcheck solution-87a05168
  (fn nth-fib [n]
    (letfn [(fib-seq [oldval currval]
              (lazy-seq
                (cons (+ oldval currval)
                  (fib-seq currval (+ oldval currval)))))]
      (take n (cons 1 (cons 1 (fib-seq 1 1)))))))

(defcheck solution-87aef397
  (fn fib
    ([nums]
     (fib '(1 1) (- nums 2)))
    ([fib-list nums]
     (if (= nums 0)
       (reverse fib-list)
       (fib (conj fib-list (+ (first fib-list) (nth fib-list 1)))
         (dec nums))))))

(defcheck solution-87b22915
  (fn[x]
    (loop[i 1
          rtn [0 1]]
      (if(>= i x) (rest rtn)
                  (recur(inc i)
                    (conj rtn (+ (last (pop rtn)) (last rtn))))))))

(defcheck solution-87b28dc6
  (fn fib [x]
    (if (= x 1)
      '(1)
      (if (= x 2)
        '(1 1)
        (let [ls (fib (- x 1))]
          (concat (fib (- x 1))
                  (list (+ (nth ls (- x 2))
                           (nth ls (- x 3))
                           )
                    )
                  )
          )
        )
      )
    ))

(defcheck solution-87cb92b6
  #((fn fib [n a b acc] (if (> n 0) (fib (dec n) b (+ a b) (conj acc b)) acc)) % 0 1 []))

(defcheck solution-87ff04d3
  (fn fib [n]
    ((fn iter [c res]
       (if (= c n)
         (reverse res)
         (let [next (if (< c 2) 1 (reduce + 0 (take 2 res)))]
           (iter (inc c) (conj res next)))
         )) 0 ())))

(defcheck solution-880696a5
  (fn [n] (take n (map first (iterate (fn [[a b]] [b (+ a b)]) '(1 1))))))

(defcheck solution-88183655
  #(loop [x % result '(1 1)]
     (if (> x 2)
       (recur (dec x) (conj result (+ (first result) (second result))))
       (reverse result))))

(defcheck solution-88185425
  (fn [n] (letfn [(fib
                    ([] (concat [1 1] (fib 1 1)))
                    ([a b] (let [c (+ a b)] (lazy-seq (cons c (fib b c))) ))
                    )] (take n (fib)))))

(defcheck solution-88453596
  (fn fib [n]
    ((fn fib-seq [coll, a, b, n]
       (if
        (= n 0)
         coll
         (recur (conj coll b) b (+ a b) (dec n))
         )
       )

     [] 0 1 n)
    ))

(defcheck solution-88c96523
  #(reverse (take %
              (loop [m % fibs '(1 1)]
                (if (< m 3)
                  fibs
                  (recur
                    (dec m)
                    (cons (+ (first fibs) (-> fibs rest first)) fibs)))))))

(defcheck solution-8940da9b
  #(letfn [(fib [n p1 p2 col]
             (if (< n 1)
               col
               (recur (dec n) p2 (+ p1 p2) (conj col p1))))]
     (fib % 1 1 [])))

(defcheck solution-8960e60
  (fn [n] (map first (take n (iterate #(vector (second %) (reduce + %)) [1 1])))))

(defcheck solution-89c0b91e
  (fn [n]
    (let [fib (fn ! [x y] (cons (+ x y) (lazy-seq (! (+ x y) x))))]
      (take n (cons 1 (fib 1 0))))))

(defcheck solution-89f0e1df
  #(take % ((fn fibo [x y] (lazy-seq (cons x (fibo y (+ x y))))) 1 1)))

(defcheck solution-8a23846f
  (fn fib [n]
    ((fn [n xs a b]
       (if (= n 0)
         xs
         (recur (dec n) (conj xs a) b (+ a b)))) n [] 1 1)))

(defcheck solution-8a4fe479
  (fn [n]
    (if (pos? n)
      (cond
        (= n 1) (list 1)
        (= n 2) (list 1 1)
        :else (rest (reduce #(cons %2 %1) '() (reduce (fn [x _] (cons (+ (first x) (second x)) x)) '(1 0) (repeat (dec n) 0))))))))

(defcheck solution-8a69e12a
  (fn [x]
    (take x (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-8a914542
  (fn [n]
    (last (take (dec n)
            (iterate (fn [coll]
                       (concat coll
                               [(+ (last coll)
                                   (last (butlast coll)))]))
              '(1 1))))))

(defcheck solution-8b013e97
  #(last (take (- % 1) (iterate (fn [a] (conj a (+ (first (reverse a)) (second (reverse a))))) [1 1]))))

(defcheck solution-8b08d6f1
  (fn fib [n]
    (loop [counter n accu '(1) lastsum 1]
      (if (= counter (count accu)) (reverse accu)
                                   (recur counter (conj accu lastsum) (+ lastsum (first accu)))))))

(defcheck solution-8b1664b8
  #(loop [n % a 1 b 1 x []] (if (zero? n) x (recur (dec n) b (+ a b) (conj x a)))))

(defcheck solution-8b78ce1
  (fn fib [n]
    (take n ((fn f
               ([] (concat [1 1] (f 1 1)))
               ([a b] (lazy-seq (cons (+ a b) (f b (+ a b))))))))))

(defcheck solution-8bb37d81
  (fn [x] (letfn [(fib [n] (if (< n 2) 1 (+ (fib (dec n)) (fib (- n 2)))))]
            (map fib (range x)))))

(defcheck solution-8bbce60d
  #(take %
     (map first
       (iterate (fn [[i1 i2]]
                  [i2 (+ i1 i2)])
         [1 1]))))

(defcheck solution-8bcb8c85
  (fn nfibs [n]
    (cond
      (<= n 0) '()
      (= n 1)  '(1)
      (= n 2)  '(1 1)
      :else (let [s (nfibs (dec n))]
              (concat s (list (apply + (take-last 2 s))))))))

(defcheck solution-8c2cd954
  #(take % ((fn fibo
              ([] (fibo 1 1))
              ([n-2 n-1]
               (lazy-seq (cons n-2 (fibo n-1 (+ n-1 n-2)))))))))

(defcheck solution-8c41498e
  #(map first (take % (iterate (fn [ [a  b] ]
                                 [b (+ a b) ]) [1 1] ))))

(defcheck solution-8c5dcf20
  (fn fib [x]
    (loop [an [] a 1 b 1 i 0]
      (if (= x i)
        an
        (recur (conj an a) b (+ a b) (inc i))))))

(defcheck solution-8c6f4ebb
  (fn [n] (loop [z 2 k (- n 2) x 1 y 1 v [1 1]] (if (> k 0) (recur (+ z y) (dec k) y z (conj v z)) v))))

(defcheck solution-8cac8398
  (fn takeFibo
    [x]
    (letfn [(fibo [coll a b x]
              (if (zero? x)
                coll
                (recur (conj coll b) b (+ a b) (- x 1))))]
      (fibo [1] 1 1 (- x 1)))))

(defcheck solution-8cc8bd36
  #(take %
     ((fn f
        [x y]
        (lazy-seq (cons x (f y (+ x y)))))
      1 1)))

(defcheck solution-8cc97c72
  #(take % ((fn fib [a b]
              (cons a (lazy-seq (fib b (+ a b))))) 1 1)))

(defcheck solution-8ced0259
  (fn fib [n]
    (let [f (
              fn [n a b coll]
              (if (= n 0)
                coll
                (recur (dec n) b (+ a b) (conj coll a))))]
      (f n 1 1 []))))

(defcheck solution-8d45fd8e
  #(take % (drop 1  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-8d7968b9
  #(take % (map first (iterate (fn [[f s]] [s (+ f s)]) [1 1]))))

(defcheck solution-8d9c2b36
  (fn fib [n]
    (letfn [(gen-fibs [prev cur]
              (lazy-seq (cons cur (gen-fibs cur (+ prev cur)))))]
      (take n (gen-fibs 0 1)))))

(defcheck solution-8dd65b68
  (fn [i]
    (map (fn f [n]
           (condp = n
             0 0
             1 1
             (+ (f (- n 1))
                (f (- n 2)))))
      (range 1 (inc i)))))

(defcheck solution-8e01936a
  #(loop [accum (list 1 1) i (- % 2)]
     (if (= 0 i) (reverse accum)
                 (let [[h1 h2 & rest] accum]
                   (recur (conj accum (+ h1 h2)) (dec i))))))

(defcheck solution-8e23e3e
  #(take % [1 1 2 3 5 8 13 21 34 55 89 154]
     ))

(defcheck solution-8e35894a
  (let [next-fib-vec (fn [fib-vec]
                       (conj fib-vec
                         (+ (peek fib-vec)
                            (peek (pop fib-vec)))))]

    (fn [n]
      (nth (iterate next-fib-vec [1 1]) (- n 2)))))

(defcheck solution-8e85cf11
  (fn fib [n]
    (loop [cur 1 prev 0 acc 0 res []]
      (if (= acc n)
        res
        (recur (+ cur prev) cur (inc acc) (conj res cur))))))

(defcheck solution-8ec745c2
  #(loop [xs () a 0 b 1 n %]
     (if (zero? n)
       (reverse xs)
       (recur (cons b xs) b (+ a b) (dec n)))))

(defcheck solution-8eeb4c23
  #(map (fn fib[n]
          (if (< n 3)
            1
            (+ (fib (- n 1))
               (fib (- n 2))
               )
            )
          ) (range 1 (inc %)) ))

(defcheck solution-8f32fe63
  #(loop [a 1 b 1 i 1 result []] (if (<= i %) (recur b (+ a b) (inc i) (conj result a)) result)))

(defcheck solution-8f38746a
  #(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-8f526d8b
  #(take % (map first (iterate (fn [[i1 i2]] [i2 (+ i1 i2)]) [1 1]))))

(defcheck solution-8f7e7f13
  (fn fib [x]
    (take x
      (map first
        (iterate (fn fib-iter [[a b]]
                   [b (+ a b)])
          [1 1])))))

(defcheck solution-8fc89192
  (fn fib [n]
    (if (= n 2)
      '(1 1)
      ((fn [subcoll] (concat subcoll [(+ (last subcoll) (nth subcoll (- (count subcoll) 2)))]))
       (fib (- n 1))))))

(defcheck solution-90044473
  (fn fib
    ([n]
     (fib 0 1 n []))
    ([f1 f2 n l]
     (if (<= n 0)
       l
       (fib f2 (+ f1 f2) (dec n) (conj l  f2))))))

(defcheck solution-900e8ec0
  (fn [n] (reverse (nth (iterate #(conj % (+ (first %) (second %))) '(1 1)) (- n 2)))))

(defcheck solution-902d6fb0
  #(take % ((fn rfib [a b] (cons a (lazy-seq (rfib b (+ a b))))) 1 1)))

(defcheck solution-9043c49d
  (fn [n]
    (take n
      (map last (iterate
                  (fn [[a b]]
                    (vector b (+ a b)))
                  [0 1])))))

(defcheck solution-9088b794
  (fn [n]
    (loop [cn (- n 2) out [1 1]]
      (if (= 0 cn)
        (into '() (reverse out))
        (recur
          (dec cn)
          (conj out
            (+ (last out) (get out (- (count out) 2)))
            )
          )
        )
      )
    ))

(defcheck solution-9119021
  (fn [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (loop [a 1 b 1
             result [1 1]
             i 3]
        (let [c (+ a b)]
          (if (> i n)
            result
            (recur b c (conj result c) (inc i))))))))

(defcheck solution-9184e068
  (fn [n] (map last (take n (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-918c6bef
  #(take % ((fn f [x y] (lazy-cat [x] (f y (+ x y)))) 1 1)))

(defcheck solution-91d0022
  (fn [n]
    (rest (map (fn fib [x]

                 (cond (< x 1) 0
                       (= x 1) 1
                       :else (+ (fib (- x 1)) (fib (- x 2)))
                       )

                 ) (range (inc n))))))

(defcheck solution-92613362
  (fn [c]
    (loop [a 1 b 1 s [1 1] c (- c 2)]
      (if (= 0 c)
        s
        (let [n (+ a b)]
          (recur b n (conj s n) (dec c)))))))

(defcheck solution-92b5c914
  #(take % ((fn fibPoslLazy [x y] (lazy-seq (cons x (fibPoslLazy y (+ x y))))) 1 1)))

(defcheck solution-937b2e44
  (fn [n] (nth (iterate #(conj % (apply + (take-last 2 %))) '[1 1]) (- n 2))))

(defcheck solution-943a39b0
  #(for [i (range 1 (inc %))] ((fn fib [x] (cond
                                             (= 1 x) 1
                                             (= 2 x) 1
                                             :else (+
                                                    (fib (dec x))
                                                    (fib ((comp dec dec) x))
                                                    ))) i)))

(defcheck solution-943d15a
  #(take % (map first
             (iterate (fn [[a b]]
                        [b (+ a b)])
               [1 1]))))

(defcheck solution-94657cb
  (fn [n]
    (letfn [(rest-of-fibs [x y]
              (lazy-seq
                (cons x (rest-of-fibs y (+ x y)))))
            (all-fibs []
              (rest-of-fibs 1 1))]
      (take n (all-fibs)))))

(defcheck solution-948d1548
  (fn [x]
    (let [fib ((fn rfib [a b]
                 (lazy-seq (cons a (rfib b (+ a b)))))
               0 1)]
      (take x (next fib)))))

(defcheck solution-94cd4ed8
  (fn fib [k]
    (loop [a 1 b 1 seq [1 1] todo (- k 2)]
      (if (= todo 0)
        seq
        (recur b (+ a b) (conj seq (+ a b)) (- todo 1))))))

(defcheck solution-9533d5fe
  (fn [n]
    (loop [result [1] next 1]
      (if (= (count result) n)
        result
        (recur (conj result next) (+ (last result) next))))))

(defcheck solution-959e1bae
  #(take %
     (map first
       (iterate
         (fn [[x1 x2]]
           [x2 (+ x1 x2)])
         [1 1]))))

(defcheck solution-961d7311
  (fn [n] (loop [a 0 b 1 v []] (if (= (count v) n) v (recur b (+ a b) (conj v b))))))

(defcheck solution-963b5ae6
  (fn [i]
    (if (< i 3)
      (take i [1 1])
      (loop [fibs [1 1]]
        (if (= (count fibs) i)
          fibs
          (let [n-1 (last fibs)
                n-2 (nth fibs (- (count fibs) 2))
                n (+ n-1 n-2)]
            (recur (conj fibs n))))))))

(defcheck solution-966dd4e5
  (fn [how-many]
    (loop [so-far [1 1]]
      (if (>= (count so-far) how-many)
        so-far
        (recur (conj so-far (+ (last so-far) (nth so-far (- (count so-far) 2 )))))))))

(defcheck solution-96985ed7
  (fn [n] (map #(second %) (take n (iterate
                                     #(let [sth (second %)]
                                        [sth (+ (first %) sth)])
                                     [0 1])))
    ))

(defcheck solution-96cafac6
  (fn fib [n]
    (->> (iterate #(conj % (+ (last %) (-> % butlast last))) [1 1])
      (take (dec n))
      last)))

(defcheck solution-973213d1
  (fn f[n]
    (nth (iterate #(conj % (+ (last %) (last (butlast %)))) [1 1]) (- n 2))))

(defcheck solution-97981567
  (fn fibo-seq [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else   (let [f (fibo-seq (dec n))
                    r (reverse f)
                    a (first r)
                    b (first (rest r))]
                (conj f (+ a b))))))

(defcheck solution-9866cfbb
  (fn fib
    [num]
    (loop [x 1 p1 1 p2 1 out [1 1]]
      (if (> x (- num 2))
        out
        (recur (inc x) p2 (+ p1 p2 ) (conj out (+ p1 p2) ))))))

(defcheck solution-98d27824
  #(take % (map last(iterate (fn [[x y]] [y (+ x y)]) [0 1]))))

(defcheck solution-9904330f
  (fn fibbo
    [num]
    (loop [n num a 1 b 1 res [1 1]]
      (if (= n 2) res
                  (recur (dec n) b (+ a b) (conj res (+ a b)))))))

(defcheck solution-997b155e
  #(map (fn fib
          [some-num]
          (if (< some-num 2)
            some-num
            (+ (fib (- some-num 1)) (fib (- some-num 2)))))
     (range 1 (inc %))))

(defcheck solution-99ea8863
  (fn [n]
    (loop [a 1
           b 1
           l [a b]
           n (- n 2)]
      (if (pos? n)
        (recur b (+ a b) (conj l (+ a b)) (dec n))
        l))))

(defcheck solution-99f3738e
  (fn [n]
    (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-99fa674b
  (fn [n]
    (let [fibs
          (fn fibs [n]
            (cond
              (= n 1) '(1)
              (= n 2) '(1 1)
              true    (let [f (fibs (dec n))]
                        (cons (+ (nth f 0) (nth f 1))
                          f))))]
      (reverse (fibs n)))))

(defcheck solution-9a1262bb
  (fn fib [n]
    (if (< n 3)
      (repeat n 1)
      (let [lst (fib (- n 1))]
        (concat lst (list (+ (last lst) (last (butlast lst)))))))))

(defcheck solution-9a1b01f8
  #(letfn [(fibs [] (lazy-cat [1] (reductions + 1 (fibs))))] (take % (fibs))))

(defcheck solution-9ab5f00c
  (fn [n]
    (let [hfn (fn [i n1 n2 res]
                (cond (> i n) res
                      (< i 3) (recur (inc i) 1 1 (conj res 1))
                      :else (let [nn1 n2
                                  nn2 (+ n1 n2)]
                              (recur (inc i) nn1 nn2 (conj res nn2)))))]
      (hfn 1 0 0 []))))

(defcheck solution-9ad6aac9
  (fn [x]
    (loop [tempcol [1 1] cnt (- x 2) seclast 1 lstemt 1]
      (if (= cnt 0)
        tempcol
        (recur (conj tempcol (+ lstemt seclast) )
          (dec cnt)
          lstemt
          (+ lstemt seclast)

          )
        )
      )
    ))

(defcheck solution-9ae4209b
  (fn [n] (loop [a 1 b 1 n n retr []]
            (if (= n 0) retr
                        (recur b (+ a b) (dec n) (conj retr a))))))

(defcheck solution-9b0bf657
  (fn [cnt] (loop [seq [1 1]
                   stop (- cnt 2)]
              (if (not (zero? stop))
                (recur (conj seq (+ (last seq) (last (butlast seq)))) (- stop 1))
                seq))))

(defcheck solution-9b6d8c4f
  (fn[n](take n (map second
                  (iterate (fn[[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-9bd84ac9
  #(take % (map first (iterate
                        (fn [[a b]] [b (+ a b)])
                        [1 1]))))

(defcheck solution-9c67a9aa
  (fn [n]
    (take n
      ((fn fibs-aux [a b]
         (lazy-seq
           (cons b
             (fibs-aux b (+ a b)))))
       0 1))))

(defcheck solution-9c6ad1ff
  (fn [x] (take x ((fn peu [a b] (cons a (lazy-seq (peu b (+ a b))))) 1 1))))

(defcheck solution-9d1b89bf
  (fn [x](take x(map first (iterate (fn [[a b]] [ b (+ a b)]) [1 1])))))

(defcheck solution-9dc05942
  #(loop [c 2 sq [1 1]]
     (if (= c %) (apply list sq)
                 (recur (inc c) (conj sq (reduce + (take-last 2 sq)))))))

(defcheck solution-9dd5ecf
  #(rest (take (+ % 1) (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-9dfea74a
  #(letfn [(fib [a b]
             (lazy-seq (cons a (fib b (+ a b)))))]
     (take % (fib 1 1))))

(defcheck solution-9eb762d0
  (fn fib-seq [n]
    (let [fib-fn (fn fib [n]
                   (if (< n 2)
                     1
                     (+ (fib (dec n)) (fib (- n 2)))))]
      (map fib-fn (range n)))))

(defcheck solution-9fb75e30
  (fn fib
    ([a b] (cons a (lazy-seq (fib b (+ a b)))))
    ([n] (take n (fib 1 1)))))

(defcheck solution-9ff82128
  (fn

    [num]
    (loop [lst [1 1]]
      (if (< (count lst) num)
        (recur (conj lst (+  (last lst) (nth lst (- (count lst) 2)))))
        lst))))

(defcheck solution-a00e2ce5
  (fn [n] (last (take (dec n)
                  (iterate #( conj % (reduce + (take-last 2 %))) [ 1 1])
                  ))))

(defcheck solution-a0ae7271
  (fn fib [n]
    {:pre [(pos? n)]}
    (letfn [(fibonacci [a b]
              (lazy-seq
                (cons (+ a b) (fibonacci b (+ a b)))))]
      (take n (cons 1 (fibonacci 0 1))))))

(defcheck solution-a0aed67f
  (fn [n] (loop [tmpn (- n 2) tmp [1 1]]
            (if (= 0 tmpn)
              tmp
              (recur (dec tmpn)
                (conj tmp (+ (last tmp) (nth tmp (- (count tmp) 2)))))))))

(defcheck solution-a10c077f
  (fn f [x]
    (rest (map
            (fn fib2 [x]
              (cond (= 0 x) 0
                    (= 1 x) 1
                    :else (+ (fib2 (- x 1)) (fib2 (- x 2)))))
            (range (+ x 1))))))

(defcheck solution-a11d5d4c
  #((fn f[a b x]
      (if (> x %)
        '()
        (conj (f b (+ a b) (+ x 1)) b)))
    0 1 1))

(defcheck solution-a13f970e
  (fn [n]
    (#(take % (map first
                (iterate
                  (fn [[a b]] [b (+ a b)]) [1 1]))) n)))

(defcheck solution-a1a742c3
  (fn fibn [n]
    (take n
      ((fn fibs [] (lazy-cat [1 1] (map + (fibs) (rest (fibs)))))))))

(defcheck solution-a1caa8f0
  (fn [nm]
    (loop [n 0 a 0 b 1 fib []]
      (if (= n nm) fib
                   (recur (inc n) b (+ a b) (conj fib b))))))

(defcheck solution-a214bd8d
  (fn [n]
    (letfn [(fibs []
              ((fn next-fib [a b]
                 (cons a (lazy-seq (next-fib b (+ a b)))))
               0 1))]
      (take n (rest (fibs))))))

(defcheck solution-a2b3cf44
  (fn [n]
    (->> (iterate #(vector (second %) (apply + %)) [1 1])
      (take n)
      (map first))))

(defcheck solution-a2d4398e
  (fn fib [n]
    (if (= n 1)
      '(1)
      (if (= n 2)
        '(1 1)
        (reverse (cons (+ (first (reverse (fib (- n 1)))) (second (reverse (fib (- n 1))))) (reverse (fib (- n 1)))))))))

(defcheck solution-a30cac4d
  #(map first (take % (iterate (fn[[f r]][r (+ f r)]) [1 1]))))

(defcheck solution-a30ec1c1
  (fn [n] (take n [1 1 2 3 5 8 13 21])))

(defcheck solution-a32a898d
  (fn [x]
    (if (< x 3)
      (repeat x 1)
      (loop [n2 1
             n1 1
             c (- x 2)
             acc [1 1]]
        (if (zero? c)
          (seq acc)
          (let [a (+ n1 n2)]
            (recur n1 a (dec c) (conj acc a))))))))

(defcheck solution-a3851484
  #(loop [x [1 1]]
     (if (= (count x) %)
       x
       (recur (conj x (apply + (take 2 (reverse x))))))))

(defcheck solution-a3ae5d48
  (fn fibo-list [n]
    (if (< n 3)
      (take n '(1 1 2))
      (let [save (fibo-list (- n 1))]
        (concat save (list (+ (last save)
                              (last (butlast save)))))))))

(defcheck solution-a3b3c857
  (fn [n] (reverse (reduce (fn [res val] (conj res (apply + (take 2 res)))) '(1 1) (range (- n 2))))))

(defcheck solution-a46226e3
  (fn fib [n]
    (take n
      (reduce
        (fn [coll n]
          (concat coll (list (apply + (take 2 (reverse coll))))))
        '(1 1)
        (range n)))))

(defcheck solution-a471b804
  #(reverse
     (loop [n1 1
            n2 1
            cnt %
            acc ()]
       (if (zero? cnt) acc
                       (recur n2 (+ n1 n2) (dec cnt) (conj acc n1))))))

(defcheck solution-a4d0c4f8
  (fn [n]
    (->> [1 1]
      (iterate #(cons (+ (first %) (second %)) %))
      (take (dec n))
      last
      reverse)))

(defcheck solution-a518a86a
  (fn [n]
    (loop [fib [1 1]
           len n]
      (if (= (count fib) len)
        fib
        (recur (conj fib (+
                          (nth fib (- (count fib) 1))
                          (nth fib (- (count fib) 2))
                          )) len)
        ))))

(defcheck solution-a551a6ca
  (fn [num]
    (let [ lazy-fib (fn lazy-fib [a b]
                      (lazy-cat (list a) (lazy-fib b (+ a b))))]
      (take num (lazy-fib 1 1)))))

(defcheck solution-a578a7d6
  (fn  [n]
    (cond (= 1 n) '(1)
          (= 2 n) '(1 1)
          :else (loop [counter 2, n-2 1, n-1 1, acc [1 1]]
                  (cond (= counter n) (seq acc)
                        :else (let [ n (+ n-2 n-1)]
                                (recur (inc counter) n-1 n (conj acc n))))))))

(defcheck solution-a5987879
  (fn fibonacci [num]
    (loop [iter 0
           a 0
           b 1
           res []]
      (if (= iter num)
        res
        (recur (inc iter) b (+ a b) (conj res b))))))

(defcheck solution-a60c840f
  #(take % ((fn F [a b] (cons a (lazy-seq (F b (+ a b))))) 1 1)))

(defcheck solution-a6281845
  (fn [input]
    (loop [n 1, result [1]]
      (if (= n input)
        result
        (recur (+ n 1) (conj result (apply + (take-last 2 result))))))))

(defcheck solution-a62b8341
  (fn [x]
    (loop [fib '(1 1) n (- x 2)]
      (if (zero? n)
        (reverse fib)
        (recur (conj fib (+ (first fib) (second fib))) (dec n))))))

(defcheck solution-a6651df1
  (fn [n]
    (loop [n n, a 0, b 1, xs []]
      (if (zero? n)
        xs
        (recur (dec n) b (+ a b) (conj xs b))))))

(defcheck solution-a67427dc
  (fn fib [n]
    (loop [n n sequence [1 1]]
      (if (= n 2) sequence
                  (recur (dec n) (conj sequence (+ (last sequence) (last (drop-last sequence)))))))))

(defcheck solution-a707ef05
  #(map (fn fib [n] (if (<= n 2) 1 (+ (fib (- n 2)) (fib (dec n))))) (range 1 (inc %))))

(defcheck solution-a71237c6
  (fn fib [n] (loop [ans [], fore 1, aft 1, to-do n]
                (if (zero? to-do) ans (recur (conj ans fore) aft (+ fore aft) (dec to-do))))))

(defcheck solution-a816ae61
  (fn fib [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (let [f (fib (dec n))
                  f1 (nth f (- n 3))
                  f2 (nth f (- n 2))
                  f3 (+ f1 f2)]
              (conj f f3)))))

(defcheck solution-a838010a
  #(for [x (range 1 (+ 1 %))]
     ((fn fib
        [x]
        (if (< x 3)
          1
          (+ (fib (dec x)) (fib (dec (dec x))))))
      x)))

(defcheck solution-a8448775
  (fn [n]
    (loop [x 1 res [1]]
      (if (= x n) res
                  (recur (inc x) (conj res (if (= (count res) 1) 1
                                                                 (+ (last res) (last (butlast res))))))))))

(defcheck solution-a89e9032
  (fn [i] (for [n (range i)] ((fn fib [x] (if (<= x 1) 1 (+ (fib (- x 1)) (fib (- x 2))))) n))))

(defcheck solution-a91967af
  (fn f [n] (if (= n 2) [1 1] (let [l (f (dec n))] (conj l (+ (l (- n 2)) (l (- n 3))))) )))

(defcheck solution-a9d8ce58
  (fn fib[n]
    (if (< n 3) (take n '(1 1))
                (reverse (conj
                           (reverse (fib (- n 1)))
                           (+
                            (last (fib (- n 1)))
                            (last (butlast (fib (- n 1)))))
                           )))))

(defcheck solution-aa590d3c
  #(reduce (fn [acc n]
             (if (< (count acc) 2)
               (conj acc n)
               (conj acc (apply + (take-last 2 acc)))))
     []
     (repeat % 1)))

(defcheck solution-aa596a00
  (fn [n] (map first (take n (drop 1 (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))))

(defcheck solution-aaa25775
  (fn [x] (map second (take x (iterate (fn [x] [ (x 1) (+ (x 0) (x 1))]) [0 1])))))

(defcheck solution-aaacee8a
  (fn fibonicca [x]
    (take x '(1 1 2 3 5 8 13 21))
    ))

(defcheck solution-aadf133b
  (fn [n]
    (loop [t n a 1 b 1 r [a]]
      (if (= t 1)
        r
        (recur (dec t) b (+ a b) (conj r b))))))

(defcheck solution-ab61d31
  #((fn fib [start range]
      (if (<= range 0)
        start
        (recur (let[subvector (subvec start (- (count start) 2))
                    x (nth subvector 0)
                    y (nth subvector 1)
                    z (+ x y)]
                 (conj start z))
          (- range 1)))
      )
    [1 1] (- % 2)
    ))

(defcheck solution-ac0f318
  (fn [X] (take X (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-ac3e22c7
  (fn [n] (take n (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-ac3f8601
  #(take %
     ((fn fib [a b]
        (lazy-seq (cons a
                    (fib b (+ a b)))))
      1 1)))

(defcheck solution-ac87d820
  (fn fibs [n]
    (map (fn fib [n] (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))) (range 1 (+ n 1)))))

(defcheck solution-acbf2e32
  (let [fib (fn fib [n] (if (< n 2) 1 (+ (fib (dec n)) (fib (- n 2)))))
        mem-fib (memoize fib)]
    (fn [n]
      (map mem-fib (range n)))))

(defcheck solution-ad291db6
  #(take % ((fn fib [a b] (cons a (lazy-seq (fib b (+ a b))))) 1 1)))

(defcheck solution-ae056e30
  (fn [n0]
    (let  [xs (range n0)]
      (map
        (fn Fi [n]
          (if (or (= n 0) (= n 1))
            1
            (+ (Fi (- n 2)) (Fi (- n 1)))
            ))  xs)
      )))

(defcheck solution-ae3f1746
  #(loop [a 1
          b 1
          n %
          fseq []
          ]
     (if (zero? n)
       fseq
       (recur b (+ a b) (dec n) (conj fseq a)))))

(defcheck solution-af1b2e52
  #(take % ((fn f [] (lazy-cat '(1 1) (map + (f) (rest (f))))))))

(defcheck solution-af1e52c
  #(loop [i 0 j 0 k 1 acc ()]
     (if (= i %)
       (reverse acc)
       (recur (+ i 1) k (+ j k) (cons k acc)))))

(defcheck solution-af4570bc
  (fn sol0026-iterate
    [n]
    {:pre [(integer? n)
           (not (neg? n))]}
    (let [fibs (->> (iterate (fn [[a b]] [b (+ a b)]) '[0 1])
                 (map (partial apply +))
                 (cons 1))]
      (take n fibs))))

(defcheck solution-afeb9142
  (fn [n]
    (letfn [(calc [n k x y results]
              (if (= k n)
                (reverse results)
                (recur n (inc k) y (+ x y) (conj results (+ x y)))))]
      (calc n 1 0 1 '(1)))))

(defcheck solution-affb3f59
  #(letfn [(f [a b] (lazy-seq (cons a (f b (+ a b)))))]
     (take % (f 1 1))))

(defcheck solution-b071dfeb
  (fn [n] (
            loop [fseq [1 1] c 2]
            (if (>= c n)
              fseq
              (recur (conj fseq (+ (last fseq) (nth fseq (- c 2)))) (+ c 1))
              )
            )
    ))

(defcheck solution-b08e5785
  (fn f ([x] (f (- x 2) '[1 1])) ([x s] (if (> 0 (dec x)) s (recur (dec x) (conj s (+ (last s) (last (butlast s)))))))))

(defcheck solution-b0b58c9a
  (fn [n]
    (reverse
      (loop [n n a 1 b 1 acc []]
        (if (< n 1) acc
                    (recur (dec n) b (+ a b) (cons a acc)))))))

(defcheck solution-b0c78f56
  (fn [n]
    (loop [s [1 1]
           nn-1 1
           nn 1]
      (if (<= n (count s))
        (take n s)
        (recur (conj s (+ nn-1 nn)) nn (+ nn-1 nn))))))

(defcheck solution-b1a7524b
  (fn [n]
    (let [fib (fn ff [fibnum]
                (if (or (= 1 fibnum) (= 2 fibnum))
                  1
                  (+ (ff (- fibnum 1)) (ff (- fibnum 2)))))]
      (loop [i 1, fibseq []]
        (if (= i (+ 1 n))
          fibseq
          (recur (inc i) (conj fibseq (fib i))))))))

(defcheck solution-b1a97830
  #(take %
     (map second (iterate
                   (fn [[x y]]
                     [y (+ x y)])
                   [0 1]))))

(defcheck solution-b21a1212
  (fn [c] (take c '(1 1 2 3 5 8 13 21))))

(defcheck solution-b261e0eb
  (fn [n] (let [fib1 (fn [result p2 p1 c]
                       (if (= 0 c)
                         (concat result (list p2 p1))
                         (recur (concat result
                                        (list p2))
                           p1
                           (+ p2 p1)
                           (dec c)
                           )
                         )
                       )] (fib1 '() 1 1 (- n 2)))))

(defcheck solution-b2802ba4
  #(map (fn fib-number [i]
          (if (<= i 2)
            1
            (+ (fib-number (- i 1)) (fib-number (- i 2)))
            )) (range 1 (+ % 1))))

(defcheck solution-b34e6fe8
  (let [fib (fn [xs] (conj xs (+ (last xs) (last (butlast xs)))))]
    (fn[n] ((apply comp (repeat (- n 2) fib)) [1 1]))))

(defcheck solution-b39278ba
  (fn [n] (nth (iterate #(conj % (apply + (take 2 (rseq %)))) [1 1]) (- n 2))))

(defcheck solution-b3b1e9fd
  (fn [i]
    (letfn [(_ [i]
              (cond
                (= i 1) '(1)
                (= i 2) '(1 1)
                :else (conj (_ (dec i)) (+ (first (_ (dec i))) (nth (_ (dec i)) 1)))
                )
              )]
      (reverse (_ i))
      )
    ))

(defcheck solution-b3c855ec
  #(map (fn f [n]
          (if (< n 2)
            1
            (+ (f (dec n)) (f (- n 2)))))
     (range %)))

(defcheck solution-b3fa03ec
  (fn fib [n]
    (loop [col [1 1] i 2]
      (if (>= i n) col (recur (conj col (+ (last col) (last (butlast col)))) (inc i)) ))))

(defcheck solution-b42f1ebe
  (fn [n]
    (reverse (loop [xs '(1 1)]
               (if (<= n (count xs))
                 xs
                 (recur (cons (+ (first xs) (second xs)) xs)))))))

(defcheck solution-b44508f1
  (fn [n]
    (take
      n
      ((fn fib [a b]
         (cons a
           (lazy-seq
             (fib b (+ b a))))) 1 1))))

(defcheck solution-b48531a
  (fn [n]
    (loop [s [1] p0 0 p1 1 l (dec n)]
      (if (zero? l)
        s
        (recur (conj s (+ p0 p1)) p1 (+ p0 p1) (dec l))))))

(defcheck solution-b48d6704
  (fn [n]
    (map first
      (take n
        (iterate (fn [[a b]] [b (+ a b)]) [1 1])
        ))))

(defcheck solution-b58157d4
  (fn [x] (map (fn fib [x]
                 (if (or (= x 0) (= x 1)) 1 (+ (fib (- x 1)) (fib (- x 2))))) (range x))))

(defcheck solution-b605b4a
  (fn [n]
    (->> [1 0]
      (iterate
        (fn [[x y]] [(+ x y) x]))
      (map first)
      (take n))))

(defcheck solution-b6214412
  (fn [num]
    (let [fib (fn fib [n] (if (> n 2) (+ (fib (- n 1)) (fib (- n 2))) 1))]
      (for [x (range  1 (inc num))]
        (fib x)))))

(defcheck solution-b6f99e3e
  (fn my-fib [x]
    (take x
      ((fn rfib [a b]
         (lazy-seq (cons a (rfib b (+ a b)))))
       1 1))))

(defcheck solution-b71931f4
  #(take %
     ((fn fib [x y]
        (lazy-seq (cons x (fib y (+ x y))))) 1 1)))

(defcheck solution-b75fdd07
  (fn fib
    ([n]
     (fib [1 1] n))
    ([x, n]
     (if (< (count x) n)
       (recur (conj x (apply + (take-last 2 x))) n)
       x))))

(defcheck solution-b79c9226
  (fn [n]
    (loop [n (- n 1)
           acc [1]]
      (if (= n 0)
        acc
        (recur (- n 1)
          (if (< (count acc) 2)
            [1 1]
            (conj acc (reduce + (take 2 (reverse acc))))))))))

(defcheck solution-b7e3b72
  #( loop [x 0
           y 1
           acc []]
     (if (= (count acc) %)
       acc
       (recur y (+ x y) (conj acc y)))))

(defcheck solution-b7f64201
  #(loop [i %
          prev 1
          curr 1
          res [1]]
     (if-not (= i 1)
       (recur (dec i) curr (+ prev curr) (conj res curr))
       res)))

(defcheck solution-b84320a1
  (fn [x]
    (take x
      ((fn f [a b]
         (cons b (lazy-seq (f b (+ a b)))))
       0 1))))

(defcheck solution-b8753ec0
  (fn [x]
    (loop [i x, result [1 1]]
      (if (> i 2)
        (recur (dec i) (conj result (+ (last result) (second (reverse result)))))
        result
        )
      )
    ))

(defcheck solution-b9a2cca2
  (fn fib-seq [n]
    (reverse
      (reduce
        (fn [ax el] (conj ax (+ (second ax) (first ax)))) '(1 1)
        (range 2 n)))))

(defcheck solution-b9ceeb9c
  (fn [n]
    (take n
      ((fn lazy-fib [a b]
         (cons a
           (lazy-seq
             (lazy-fib b (+ a b)))))
       1 1))))

(defcheck solution-ba0cd817
  #(take % (map last (iterate (fn [[x y]] [y (+ x y)]) [0 1]))))

(defcheck solution-bb04d785
  (fn [x] (let [fib (fn fib [a b] (cons a (lazy-seq (fib b (+ a b)))))] (take x (fib 1 1)))))

(defcheck solution-bb0877e0
  #(take % (map first ( iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-bb20de3b
  (fn fib [x]
    (take
      x
      (map
        last
        (iterate
          (fn internalfib [pair]
            [(last pair) (+ (first pair) (last pair))])
          [0 1])))))

(defcheck solution-bb2b0803
  (fn [num]
    (map #(% 0) (take num
                  (iterate (fn [[p1 p2]] [p2 (+ p1 p2)]) [1 1])))))

(defcheck solution-bb311564
  #(take % [1 1 2 3 5 8 13 21]))

(defcheck solution-bb472f83
  (fn [n]
    ((fn [c r pv cv]
       (if (= 0 c)
         r
         (recur (dec c)
           (conj r (+ pv cv))
           cv
           (+ pv cv))))
     (dec n) [1] 0 1)))

(defcheck solution-bb6a18cd
  (fn fibonacci [n]
    (if (= n 1) '(1)
                (if (= n 2) '(1 1)
                            (concat (fibonacci (- n 1))
                                    (list
                                      (+ (last (fibonacci (- n 1)))
                                         (last (drop-last (fibonacci (- n 1)))))))))))

(defcheck solution-bb899619
  #(take % (map first (iterate (fn [[a b]] [ b (+ a b)]) [1 1]))))

(defcheck solution-bc6e04e1
  (fn fibb [n] (take n (map first (iterate (fn [[l r]] [r (+ l r)]) [1 1])))))

(defcheck solution-bc7e3080
  #(loop [n % ret [1 1]] (if (> n 2) (recur (dec n) (conj ret (apply + (take 2 (reverse ret))))) ret)))

(defcheck solution-bcc050fc
  (fn [c]
    (loop [result [1 1]]
      (if (<= c (count result))
        (take c result)
        (recur (conj result
                 (apply + (take-last 2 result))))))))

(defcheck solution-bcc16e74
  #(loop [cnt (- % 1)
          sqnc [1 1]]
     (if (> (count sqnc) cnt)
       sqnc
       (let [sqlen (count sqnc)]
         (recur cnt (conj sqnc (reduce + (subvec sqnc (- sqlen 2) sqlen))))))
     ))

(defcheck solution-bcc6cd9f
  (fn [i]
    (loop [[a b :as c] '(1 1), i (- i 2)]
      (if (pos? i)
        (recur (cons (+ a b) c)  (dec i))
        (reverse c)))))

(defcheck solution-bce79025
  #(let [fib (fn fib [a b] (cons a (lazy-seq (fib b (+ b a)))))] (take % (fib 1 1))))

(defcheck solution-bd1c8007
  (fn fib [arg]
    (reverse
      (loop [x1 1
             x2 1
             lst '(1 1)
             cnt (- arg 2)]
        (if (= cnt 0)
          lst
          (let [x3 (+ x1 x2)]
            (recur x2 x3 (conj lst x3) (dec cnt))))))))

(defcheck solution-bd57fbf4
  (fn fibonnacciSequence
    [n]
    (map (fn fibonnacci
           [n]
           (cond
             (= n 0) 1
             (= n 1) 1
             :else (+ (fibonnacci (- n 1)) (fibonnacci (- n 2))))) (range n))))

(defcheck solution-bd605878
  (fn [n]
    (letfn [(f ([] (concat '(1 1) (f 1 1)))
              ([a b] (lazy-seq (cons (+ a b) (f b (+ a b))))))]
      (take n (f)))))

(defcheck solution-bd90fa00
  (fn fibs [howmany]
    (let [fib-extender
          (fn [fibs-so-far]
            (conj fibs-so-far (+ (last fibs-so-far) (last (butlast fibs-so-far)))))]
      (cond
        (= howmany 1) [1]
        (= howmany 2) [1 1]
        :else (last (take (dec howmany) (iterate fib-extender [1 1])))))))

(defcheck solution-bdc4045c
  #(take %
     ((fn fib [a b] (lazy-seq (cons a (fib b (+ a b)))))
      1 1)))

(defcheck solution-be287070
  (fn [x]
    (loop [first 1
           second 1
           remaining (- x 2)
           res []]
      (if (= 0 remaining)
        (conj res first second)
        (recur second (+ first second) (dec remaining) (conj res first))))))

(defcheck solution-be41848d
  #((fn fibseq [numto sq]
      (if (= 0 numto)
        sq
        (fibseq (- numto 1) (cons ((memoize (fn fib [n] (case n
                                                          1 1
                                                          2 1
                                                          (+ (fib (- n 1)) (fib (- n 2)))))) numto) sq)))) % nil))

(defcheck solution-be41e0dd
  #(let [fib-inner (fn [[n n+1]] [n+1 (+ n n+1)])]
     (map first (take % (iterate fib-inner [1 1])))))

(defcheck solution-beb98471
  (fn [n]
    (letfn
     [(fib [a b]
        (cons a (lazy-seq (fib b (+ a b)))))]
      (take n (fib 1 1)))))

(defcheck solution-beef5d71
  (fn fib [n]
    (condp = n
      0 []
      1 [1]
      2 [1 1]
      (let [prev (fib (dec n))]
        (conj prev (+ (last (butlast prev))
                      (last prev)))))))

(defcheck solution-bef36748
  (fn [n]
    (take n
      (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-beff8b20
  #(take %
     (map first
       (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

(defcheck solution-bf52d8d8
  (fn [n] (seq (nth (iterate #(conj % (+ (last %) (second (reverse %)))) [1 1]) (- n 2)))))

(defcheck solution-bf98f206
  (fn [n] (loop [n n a 0 b 1 xs []] (if (= n 0) xs (recur (- n 1) b (+ a b) (conj xs b))))))

(defcheck solution-c06a06b1
  #(take % ((fn f [a b] (lazy-seq (cons b (f b (+ a b))))) 0 1)))

(defcheck solution-c08210f7
  #(take % ((fn fib [a b]
              (cons a
                (lazy-seq (fib b (+ a b))))) 1 1)))

(defcheck solution-c11218b4
  (fn fib [n]
    (if (= n 1)
      [1]
      (let [more (fib (- n 1))]
        (conj more (apply + (take-last 2 more)))))))

(defcheck solution-c1199f4d
  #(let [fib-acc (fn [values size]
                   (if (= (count values) size)
                     (reverse values)
                     (let [next (+ (first values)
                                   (second values))]
                       (recur (cons next values)
                         size))))]
     (fib-acc '(1 1) %)))

(defcheck solution-c13a5e04
  (fn [n]
    (let [nextfib
          (fn [acc]
            (conj acc (apply + (take 2 acc))))]
      (reverse (nth (iterate nextfib '(1)) (dec n))))))

(defcheck solution-c1576825
  (fn fibs [n]
    ;; (fib 3) => (1 1 2)
    (drop 1
      (loop [counter 1
             fibs [0 1]]
        (if (<= counter (dec n))
          (recur (inc counter) (conj fibs (apply + (drop (- (count fibs) 2) fibs))))
          fibs)))))

(defcheck solution-c168301e
  (fn [n]
    (loop [i 0 pprev 1 prev 1 fib []]
      (if (= i n) fib
                  (recur (inc i) prev (+ prev pprev) (conj fib pprev))
                  ))))

(defcheck solution-c2210b19
  (fn [i] (take i '(1 1 2 3 5 8 13 21))))

(defcheck solution-c22eb633
  #(map (fn fib [x] (if (< x 2) 1 (+ (fib (- x 2)) (fib (- x 1))))) (range 0 %)))

(defcheck solution-c29e8481
  (fn [n]
    (let [f (fn produce-fibos [[x y]]
              (lazy-seq (cons y (produce-fibos (list y (+ x y))))))]
      (take n (f (list 0 1))))))

(defcheck solution-c2a0462
  #(map first (take % (iterate (fn [[_ y z]] [y z (+ y z)]) [1 1 2]))))

(defcheck solution-c2cb5301
  (fn [x]
    (if (<= x 1)
      (list 1)
      (loop [n 3 result '(1 1)]
        (if (> n x)
          (reverse result)
          (let [[a b] result]
            (recur (inc n) (conj result (+ a b)))))))))

(defcheck solution-c40190f4
  (fn f [n] (cond (= n 1) [1] (= n 2) [1 1] :else (let [o (f (- n 1))] (conj o (+ (first (reverse o)) (second (reverse o))) ) ))))

(defcheck solution-c456b8c7
  (fn fib ([n]
           (cond
             (= n 0) []
             (= n 1) [1]
             (= n 2) [1 1]
             :else
             ((
                fn ifib ([s c]
                         (cond
                           (= c 0) s
                           :else
                           (ifib (conj s (+ (last s) (last (butlast s)))) (- c 1))
                           )
                         )
                ) [1 1] (- n 2))
             )
           )
    ))

(defcheck solution-c4c7870
  #(take %
     ((fn fib[]
        (lazy-cat [1 1]
          (map +(fib)(rest(fib))))))))

(defcheck solution-c4ef86c1
  (fn [x]
    (loop [n x, result '(1 1)]
      (if (< n 3) (reverse result)
                  (recur (dec n) (conj result (+ (first result) (second result))))
                  )
      )
    ))

(defcheck solution-c58a8953
  #(take %1 (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1]))))

(defcheck solution-c5c0083
  #(map (fn fib [x] (if (= 0 x) 0 (if (= 1 x) 1 (if (= 2 x) 1 (+ (fib (dec x)) (fib (dec (dec x)))))))) (rest (range (inc %)))))

(defcheck solution-c5f203a1
  (fn [n] (map (fn fib [i]
                 (if (<= i 1) 1 (+ (fib (- i 1)) (fib (- i 2)))))
            (range n))))

(defcheck solution-c677de4f
  (fn [x]
    (loop [x x
           ret '()
           n1 0
           n2 1]
      (if (zero? x)
        (reverse ret)
        (recur (dec x)  (cons n2 ret) n2 (+ n1 n2))))))

(defcheck solution-c6f374da
  (fn [n]
    (loop [xs () res ()]
      (cond
        (= (count res) n) (reverse res)
        (< (count xs) 2) (recur (cons 1 xs) (cons 1 res))
        :default (recur (list (apply + xs) (first xs)) (cons (apply + xs) res))

        ))))

(defcheck solution-c79dd6c6
  (fn [n]
    (loop [i 2
           result '(1 1)]
      (let [tluser (reverse result)]
        (if (= i n)
          tluser
          (recur (inc i) (conj result (+ (nth tluser (dec i))
                                         (nth tluser (- i 2))))))))))

(defcheck solution-c7ab2d21
  (fn fib [n]
    (loop [a 1
           b a
           i 0
           fibs []]
      (if (= i n)
        fibs
        (recur b (+ a b) (inc i) (conj fibs a))))))

(defcheck solution-c7d6ec73
  #((fn ro [n m res]
      (if (= n m)
        (reverse res)
        (ro (inc n) m (conj res (if (< n 3) 1 (+ (first res) (second res)))))
        ))
    1 (inc %) '()))

(defcheck solution-c800dc3f
  (fn [n]
    (loop [fib-nums [1 1]]
      (if (>= (count fib-nums) n)
        (subvec fib-nums 0 n)
        (
          let [[n1 n2] (reverse fib-nums)]
          (recur (conj fib-nums (+ n1 n2)))
          )
        )
      )
    ))

(defcheck solution-c8b4cff4
  (fn fib [i] (take i
                (map first (iterate
                             (fn [v] [(v 1) (+ (v 0) (v 1)) ]) [1 1 ]
                             )))))

(defcheck solution-c8d219be
  (fn [n] (loop [n n a 0 b 1 c []] (if (= 0 n) c (recur (dec n) b (+ a b) (conj c b))))))

(defcheck solution-c8f1349d
  (fn fibonacci
    [num]
    (loop
     [n (- num 2)
      series [1 1]
      a 1
      b 1]
      (if (= n 0)
        series
        (recur (dec n) (conj series (+ a b)) (+ a b) a)))))

(defcheck solution-c9601cba
  (fn [n]
    (letfn [(fiboloop [n]
              (if (or (= n 1) (= n 2))
                1
                (+ (fiboloop (- n 1)) (fiboloop (- n 2)))))]
      (map fiboloop (take n (iterate inc 1))))))

(defcheck solution-c96df287
  #(loop [result [1] cur 1 cntr %]
     (cond
       (= cntr 1) result
       (= cntr 2) (conj result cur)
       :else (recur (conj result cur) (+ (last result) cur) (dec cntr)))))

(defcheck solution-c9829e4d
  (let [f (fn f [n] (if (< n 3) 1 (+ (f (dec n)) (f (- n 2)))))]
    #(take % (map f (rest (range))))))

(defcheck solution-c9a6bc6a
  #(let [fibs
         ((fn fib [f1 f2] (cons f1 (lazy-seq (fib f2 (+ f1 f2))))) 1 1)]
     (take % fibs)))

(defcheck solution-c9c9ea81
  #(take % (map second (iterate (fn [[a b]] [b (+ a b)]) [0 1]))))

(defcheck solution-ca55d53c
  (fn [x]
    (cond (= x 1) '(1)
          (= x 2) '(1 1)
          :else (loop [counter (- x 2) fib-n-2 1 fib-n-1 1 result [1 1]]
                  (if (= counter 0)
                    result
                    (let [fib-n (+ fib-n-2 fib-n-1)]
                      (recur (dec counter) fib-n-1 fib-n (conj result fib-n))))))))

(defcheck solution-caaf8f8a
  (fn [x]
    (let [fib (fn fib [n]
                (if (or (= n 0) (= n 1))
                  1
                  (+ (fib (- n 1)) (fib (- n 2)))))]
      (take x (map fib (range))))))

(defcheck solution-cc178b1
  #(take % ((fn fib [a b] (lazy-seq (cons a (fib b (+ b a))))) 1 1)))

(defcheck solution-cc404684
  #(loop [a 0
          b 1
          l []]
     (if (= % (count l))
       l
       (recur b (+ a b) (conj l b)))))

(defcheck solution-cc5b80d
  #(take % ((fn rfib [a b]
              (lazy-seq (cons a (rfib b (+ a b)))))
            1 1)))

(defcheck solution-cce566f1
  (fn [n]
    (take
      n
      (map first
        (iterate
          (fn [[a b]] [b (+ a b)])
          [1 1])))))

(defcheck solution-cd56fb99
  (let [fib
        (fn [x y acc n]
          (if (= (count acc) n)
            acc
            (recur y (+ x y) (conj acc (+ x y)) n)
            )
          )]
    (fn [z] (fib 0 1 [1] z))
    ))

(defcheck solution-cd738ba7
  (fn [n]
    (let [fibs (fn fib [a b] (cons a (lazy-seq (fib b (+ a b)))))]
      (take n (fibs 1 1)))))

(defcheck solution-cd8014f1
  #(letfn [(fib [n]
             (if (< n 3) 1
                         (+ (fib (dec n)) (fib (- n 2)))))]
     (map (comp fib inc) (range %))))

(defcheck solution-cdfdd8b5
  (fn [n]
    (letfn [(fib [a b]
              (cons a (lazy-seq b (fib b (+ a b)))))]
      (take n (fib 1 1)))))

(defcheck solution-ce0a3425
  (fn [n]
    (let [fib-seq ((fn fib [a b]
                     (lazy-seq (cons a
                                 (fib b (+ a b)))))
                   1 1)]
      (take n fib-seq))))

(defcheck solution-ce5e0e36
  #(letfn [ (fibo [n] (cond
                        (< n 2) 1
                        :else (+ (fibo (- n 1)) (fibo (- n 2)))
                        ))]
     (take % (map fibo (range 0 %)))
     ))

(defcheck solution-cec3515d
  #(letfn [(fib [n] (cond (= n 1) 1 (= n 2) 1 :else (+ (fib (- n 2)) (fib (dec n)))))] (for [x (range %)] (fib (inc x)))))

(defcheck solution-cf56280f
  #(take % (map first (iterate (fn [[a b]] [b (+ a b)] ) [1 1] ) ) ))

(defcheck solution-cf5b1e42
  (fn f [n]
    (if (< n 2)
      [1]
      (conj (f (- n 1)) (reduce + (take-last 2 (f (- n 1)))
                          )))))

(defcheck solution-d0630c8
  (fn fib [n]
    (let [f (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1]))](take n f))))

(defcheck solution-d06dc36a
  #(reverse (loop [x 0 y 1 result '() i 0]
              (if (< i %)
                (recur y (+ x y) (conj result y) (inc i))
                result
                )
              )))

(defcheck solution-d09289c4
  #(take %
     (map first
       (iterate (fn [[a b]]
                  [b (+ a b)])
         [1 1]))))

(defcheck solution-d09b173d
  #( sequence (loop [i 3 y [1 1] ]
                (if (>  i  % ) y
                               (recur (inc i)
                                 (
                                  (fn[ x]
                                    (conj x
                                      (+ (last x) (last (butlast x)) )
                                      )
                                    )
                                  y
                                  )


                                 )
                               )
                )
     ))

(defcheck solution-d0d26c9f
  #(loop [n 1, l '()]
     (if (= (+ 1 %) n)
       l
       (recur (inc n) (concat l (list (int (Math/floor (+ (/ (Math/pow (/ (+ 1 (Math/sqrt 5)) 2) n) (Math/sqrt 5)) (/ 1 2)))) ))))))

(defcheck solution-d0e87665
  (fn fib [n]
    (loop [i 0
           a 0
           b 1
           res []]
      (if (= i n)
        res
        (recur (inc i)
          b
          (+ a b)
          (conj res b))))))

(defcheck solution-d1058cc0
  (fn [n]
    (loop [result [1 1]
           accum 2]
      (cond
        (zero? n) nil
        (= n 1) [1]
        (= n 2) result
        (= n accum) result
        :else (recur (conj result (apply + (take-last 2 result)))
                (inc accum))))))

(defcheck solution-d128d54a
  #(loop [sum 1, vec [1], n (dec %)]
     (if (= n 0)
       vec
       (recur (+ sum (last vec)) (conj vec sum) (dec n)))))

(defcheck solution-d1b5b2a3
  (fn sb [x]
    (loop [a1 0
           a2 1
           coll []
           x x]
      (if (> x 0)
        (recur a2 (+ a1 a2) (conj coll a2) (- x 1))
        coll))))

(defcheck solution-d25b8e2a
  #(take % (
            (fn fib [a b] (cons a (lazy-seq (fib b (+ b a))))) 1 1)))

(defcheck solution-d2bc1fa0
  #(loop [iterator 0 fib [] a 1 b 1]
     (if (= % iterator)
       fib
       (recur (inc iterator) (conj fib a) b (+ a b)))))

(defcheck solution-d2d7ba66
  (comp reverse (fn fib ([n] (fib n 0 1 ())) ([n a b accum] (if (<= n 0) accum (fib (dec n) b (+ a b) (cons b accum)))))))

(defcheck solution-d303b090
  (fn f [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else
          (let [r1 (f (- n 1))
                r2 (f (- n 2))]
            (conj r1 (+ (last r1) (last r2)))))))

(defcheck solution-d3a26d8c
  (fn [s i]
    (if (= i (count s))
      s
      (recur (conj s (reduce + (take-last 2 s))) i))) [1 1])

(defcheck solution-d3db34cc
  (fn
    [num]


    (loop [i 1 b 0 c 1 list '(1)]
      (if (< i num)
        (recur (inc i) c (+ b c) (conj list (+ b c)))
        (reverse list)
        )
      )))

(defcheck solution-d43438a8
  (fn [n] (reverse (reduce (fn [e f] (conj e (+ (first e) (second e)))) '(1 1) (repeat (- n 2) 1)))))

(defcheck solution-d445f2ac
  (fn [n] (take n (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-d4a3459
  (fn [i]
    (loop [i- (- i 2) s [1 1]]
      (if (<= i- 0)
        s
        (recur (- i- 1) (conj s (+ (nth s (- (count s) 1)) (nth s (- (count s) 2)))))))))

(defcheck solution-d5860064
  (fn fib [n]
    (loop [a 0 b 1 coll [] cnt 0]
      (if (= cnt n)
        coll
        (recur b (+ a b) (conj coll b) (inc cnt))))))

(defcheck solution-d6b1a626
  (fn fibs [n]
    (take n ((fn fib-seq [] (lazy-cat [1 1] (map + (fib-seq) (rest (fib-seq)))))))))

(defcheck solution-d6f1e01f
  #((fn fib[s, c] (if (= (count s) c) s (fib (conj s (+ (last s) (second (reverse s)))) c))) [1 1] %))

(defcheck solution-d6f489f0
  (fn [n]



    (take n ( ( fn fib [a b]
                (lazy-seq (cons a (fib b (+ a b))))
                ) 1 1))))

(defcheck solution-d748cb79
  (fn fib [x]
    (cond
      (= x 1) '(1)
      (= x 2) '(1 1)
      :else (let [fib-1 (fib (dec x))]
              (concat fib-1 (list (+ (last fib-1) (nth fib-1 (- (count fib-1) 2)))))))))

(defcheck solution-d7593ede
  (partial
    (fn [acc n]
      (if (= 0 n) (reverse acc)
                  (recur
                    (if (< (count acc) 2)
                      (conj acc 1)
                      (conj acc ( + (first acc) (second acc)))) (dec n)))) '()))

(defcheck solution-d78eac19
  (fn [n]
    (letfn [(fib [n]
              (if (< n 2)
                1
                (+ (fib (dec n)) (fib (- n 2)))))]
      (map fib (range n)))))

(defcheck solution-d7dc3133
  (fn fibonacci [n]
    (take n ((fn fib-seq [a b]
               (lazy-seq (cons a (fib-seq b (+ a b)))))
             1 1))))

(defcheck solution-d7f355af
  (fn [x]
    (take x
      ((fn fib [a b]
         (cons a (lazy-seq (fib b (+ a b)))))
       1 1))))

(defcheck solution-d819ad6d
  (fn [n]
    (letfn [(fib [a b] (cons a (lazy-seq (fib b (+ b a)))))
            (fib-n [a b n] (take n (fib a b)))]
      (fib-n 1 1 n))))

(defcheck solution-d828df50
  (fn [n]
    (map first (reductions
                 (fn [[a b] _] [b (+ a b)]) [1 1] (range 1 n)))))

(defcheck solution-d8366763
  (fn [n]
    (let [f (fn fib [a b] (cons a (lazy-seq (fib b (+ a b)))))]
      (take n (f 1 1)))))

(defcheck solution-d84eadb8
  (fn [n] (seq (reduce (fn [a b] (conj a (+ (last a) (last (butlast a))))) [1 1] (range (dec (dec n)))))))

(defcheck solution-d859301b
  (fn _fibo [n]
    (cond (= n 1) [1]
          (> n 1) (let [x (_fibo (dec n))]
                    (conj x (apply + (take-last 2 x)))))))

(defcheck solution-d88509c2
  (fn ! [x]
    (if (= x 2)
      '(1 1)
      (let [y (! (dec x))]
        (let [z (apply + (take 2 (reverse y)))]
          (concat y [z]))))))

(defcheck solution-d9448984
  (fn [n]
    (loop [n n, a 1, b 1, acc [1 1]]
      (if (= n 2)
        acc
        (let [next (+ a b)]
          (recur (dec n) b next (conj acc next)))))))

(defcheck solution-d9bada31
  (fn [n] (loop [res [1 1]]
            (if (= (count res) n)
              res
              (recur (conj res
                       (+ (last res)
                          (last (butlast res)))))))))

(defcheck solution-d9bbddb5
  (fn [n] (into [] (take n ((fn fib-nums [n nn] (lazy-seq (cons n (fib-nums nn (+ n nn))))) 1 1)))))

(defcheck solution-da00b7c4
  (fn [n]
    (loop [i n a 0 b 1 ret '()]
      (if (> i 0)
        (recur (dec i) b (+ a b) (conj ret b))
        (into '() ret)))))

(defcheck solution-da2c7c60
  #(
    ; Hacky! Doesn't handle fib(1) or fib(2)!
    (fn fib [n, sq]
      (if (= n 0)
        sq
        (fib
          (- n 1)
          (conj sq (+
                    (nth sq (- (count sq) 2 ))
                    (last sq))
            )
          )
        )
      )
    (- % 2) [1 1]
    ))

(defcheck solution-da6529cd
  (fn [x] (take x
            (map #(first %)
              (iterate
                (fn [[f s]] [s (+ f s)])
                [1 1])))))

(defcheck solution-da6b45af
  #(take %1 ((fn fib [a b] ( lazy-seq (cons a (fib b (+ a b))) ) ) 1 1)))

(defcheck solution-dad51068
  (fn [n]
    (let [help (fn help [i fi-1 fi-2]
                 (cond (= i n) '()
                       (= i 0) (concat '(1) (help (inc i) 1 0))
                       (> i 0) (let [fi (+ fi-1 fi-2)]
                                 (concat (list fi) (help (inc i) fi fi-1)))))]
      (help 0 0 0))))

(defcheck solution-dae564e
  #(reverse (reduce (fn [lst x] (cons (+ (first lst) (second lst)) lst)) '(1 1) (range (- % 2)))))

(defcheck solution-db053aad
  (fn [n]
    (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-db1015d7
  (fn [n]
    (->> [1 1]
      (iterate (fn [[n1 n2]] [n2 (+ n1 n2)]))
      (map first)
      (take n))))

(defcheck solution-db17470a
  (fn f [ n ] (if  (= n 1  )  [1]  (if (= n 2) [1 1] (let [l (f (dec n))] (conj l (apply + (take-last 2 l))))))))

(defcheck solution-db2b5a49
  #(take % ((fn f ([] (concat '(1 1) (f 1 1)))
              ([a b] (let [n (+ a b)]
                       (lazy-seq
                         (cons n (f b n)))))))))

(defcheck solution-db5f7ae
  (fn fab [n]
    (if (< n 3)
      [1 1]
      (conj (fab (- n 1)) (apply + (take-last 2 (fab (- n 1))))  ))))

(defcheck solution-dc0bd4ab
  (fn [n]
    (loop [a 1, b 1, count n, xs []]
      (if (zero? count)
        xs
        (recur (+ a b) a (dec count) (conj xs b))))))

(defcheck solution-dc2e9c8b
  (fn f [n]
    (take n
      (map first
        (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(defcheck solution-dc872b61
  (fn [a]
    ((fn [b last2 last1 resvec]
       (if (< b 1)
         resvec
         (recur (dec b)
           last1
           (+ last1 last2)
           (conj resvec (+ last1 last2)))))
     (dec a) 0 1 [1])))

(defcheck solution-dc89047
  (fn [n]
    (loop [n (- n 2) acc [1 1]]
      (if (= n 0) acc
                  (let [len (count acc)]
                    (recur (dec n) (conj acc (+ (acc (- len 2)) (last acc)))))))))

(defcheck solution-dcb49c83
  (fn first-n-fib [n]
    (letfn [(lazy-fib-seq
              ([] (concat [1 1] (lazy-fib-seq 1N 1N)))
              ([a b]
               (let [n (+ a b)]
                 (lazy-seq
                   (cons n (lazy-fib-seq b n))))))]
      (take n (lazy-fib-seq)))))

(defcheck solution-dcc67b61
  (fn fib
    ([n] (fib n (vec '(1 1))))
    ([n s]
     (let [c (count s)]
       (if (>= c n)
         (take n s)
         (fib n
           (conj s (+ (nth s (- c 2)) (nth s (- c 1))))))))))

(defcheck solution-dcd5d223
  #(nth (iterate (fn [x](conj x (reduce + (take-last 2 x)))) [1 1]) (- % 2)))

(defcheck solution-dcdc1a3d
  (fn fibonacci [n] (case n 1 '(1) 2 '(1 1) (let [f (fibonacci (- n 1))] (concat f (list (reduce + (take-last 2 f))))))))

(defcheck solution-dd1ef375
  (fn fib [n]
    (reduce
      (fn [a-seq _]
        (concat a-seq (list
                        (let [size (count a-seq)]
                          (if (> 2 size)
                            1
                            (+ (nth a-seq (dec size)) (nth a-seq (dec (dec size))))
                            )
                          )
                        )
                )

        ) []
      (range n)
      )
    ))

(defcheck solution-ddb9c702
  (fn [n]
    (loop [f 1 s 1 v [] cnt n]
      (if (= cnt 0)
        v
        (recur s (+ f s) (conj v f) (dec cnt))))))

(defcheck solution-ddf0553
  #(take % (drop 1 (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))))

(defcheck solution-de78cc2e
  (fn [n]
    (take n
      (map first ((fn fib [a b]
                    (lazy-seq
                      (cons [a b] (fib b (+ a b))))) 1 1)))))

(defcheck solution-de9280a1
  #(letfn
    [(fibs [n fs]
       (if (= n 0) (reverse fs)
                   (let [[a b] fs] (fibs (dec n) (cons (+ a b) fs))))
       )]
     (fibs (- % 2) '(1 1))))

(defcheck solution-de95189e
  #(take % (map
             first
             (iterate
               (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-dea2cf94
  (letfn [(fib [n]
            (condp = n
              1 '(1)
              2 '(1 1)
              (let [fib' (fib (dec n))]
                (conj fib' (+ (first fib') (second fib'))))))]
    (comp reverse fib)))

(defcheck solution-defa5579
  #(take %
     ((fn fib []
        (lazy-cat [1 1]
          (map + (fib) (rest (fib))))))))

(defcheck solution-df5ed8d7
  (fn [n]
    (let [fibo-fn (fn [[x y]] [y (+ x y)])
          fibo  (map first (iterate fibo-fn [1 1]))]
      (take n fibo))))

(defcheck solution-dfa626e1
  (fn [n]
    (map
      (fn fib [n] (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
      (range n))
    ))

(defcheck solution-dfef2996
  (fn [x]
    (reverse (
              (fn fib
                [x]
                (if (= x 2)
                  '(1 1)
                  (let [s (fib (dec x))]
                    (conj s (+ (first s) (second s))))))
              x))))

(defcheck solution-e023cb7d
  (fn fib [n]
    (if (= n 1)
      '(1)
      (if (= n 2)
        '(1 1)
        (let [res (reverse (fib (- n 1)))]
          (reverse (cons (+ (first res) (second res))
                     res)))))))

(defcheck solution-e064dd82
  #(cons 1 (take (- % 1) ((fn f [a b] (lazy-seq (cons b (f b (+ a b))))) 1 1))))

(defcheck solution-e091f61
  (fn fib
    ([n] (fib n 1 1 2 [1]))
    ([n a b pos so-far]
     (if (= pos n)
       (conj so-far b)
       (fib n b (+ a b) (inc pos) (conj so-far b))))))

(defcheck solution-e2a485f4
  (fn
    fib-seq[num]
    (let
     [fib
      (fn
        fib[n]
        (cond
          (< n 1) 0
          (< n 3) 1
          :else (+ (fib (- n 2)) (fib (- n 1)))
          )
        )
      ]
      (if (< num 1) '()
                    (concat (fib-seq (- num 1)) (list (fib num)))
                    )
      )
    ))

(defcheck solution-e2f4c72d
  #(take %
     ((fn fib [a b]
        (lazy-seq (cons a (fib b (+ a b))))) 1 1)))

(defcheck solution-e306ee65
  #(take % ((fn rfib [a b]
              (lazy-seq (cons a (rfib b (+ a b))))
              ) 1 1)
     ))

(defcheck solution-e355d0f7
  (fn fib [n]
    (if (< n 2)
      [1]
      (if (< n 3)
        [1 1]
        (conj (fib (dec n)) (+ (last (fib (- n 1))) (last (fib (- n 2)))))))))

(defcheck solution-e360f130
  (fn [cnt]
    (loop [lst [1] n 1 p1 0 p2 1]
      (if (= n cnt)
        lst
        (recur (conj lst (+ p1 p2)) (inc n) p2 (+ p1 p2))))))

(defcheck solution-e3e759f7
  #(loop [x 1
          y 1
          xs []]
     (if (= % (count xs))
       xs
       (recur y (+ x y) (conj xs x)))
     ))

(defcheck solution-e46c88b6
  (fn [n]
    (take n '(1 1 2 3 5 8 13 21))))

(defcheck solution-e486cdc6
  (fn [in]
    (loop [n in
           out []
           x1 1
           x2 1]
      (if (= n 0)
        out
        (recur (- n 1) (conj out x1) x2 (+ x1 x2))))))

(defcheck solution-e5307b4b
  #(take %
     (map
       (fn [[x y]] y)
       (iterate
         (fn fibo [ [x1 x2] ] [ x2 (+ x1 x2) ])
         [ 0 1 ]))))

(defcheck solution-e5435573
  #(letfn [(fib [[a b]] [b (+ a b)])]
     (->> (iterate fib [1 1]) (map first) (take %))))

(defcheck solution-e55d3972
  (fn f [n] (if (= n 2) [1 1] (let [s (f (dec n))]
                                (conj s (+ (last s) (last (butlast s))))
                                ))))

(defcheck solution-e5aa56bf
  (fn [n]
    (loop [l '(1) cnt 1 prev 0]
      (if (= cnt n)
        l
        (recur (reverse (conj (reverse l) (+ prev (last l)))) (inc cnt) (last l))))))

(defcheck solution-e5f8e938
  #(take % ((fn f [a b]
              (cons a (lazy-seq (f b (+ a b))))) 1 1)))

(defcheck solution-e7ab0bc
  (fn fib [x] (cond (= x 1) [1] (= x 2) [1 1] :t (concat (fib (- x 1)) [(apply + (take-last 2 (fib (- x 1))))]))))

(defcheck solution-e7e1f8f
  (fn bob [x] (reverse ((fn fib [n]
                          (cond
                            (= n 2) '(1 1)
                            (= n 1) '(1)
                            (< n 1) '()
                            :else (let [prev (fib (- n 1))]
                                    (cons (+ (first prev) (second prev)) prev)))) x))))

(defcheck solution-e7fa8d92
  #(loop [cnt % result '()]
     (if (> cnt 0)
       (if (< (count result) 2) (recur (dec cnt) (conj result 1))
                                (recur (dec cnt)
                                  (reverse (conj (reverse result) (+ (nth result (- (count result) 1)) (nth result (- (count result) 2))))))) result )))

(defcheck solution-e840fa7
  (fn [x]
    (take x
      (map last
        (iterate (fn [[a b]]  [b (+ a b)]) [0 1])))))

(defcheck solution-e84a9ef
  #(letfn [(fib [a b n]
             (if (<= n 0)
               nil
               (cons a (fib b (+ a b) (- n 1)))))]
     (fib 1 1 %)))

(defcheck solution-e907ab15
  #(let [result []]
     (cond
       (< % 1) result
       (= % 1) (conj result 1)
       (> % 1)
       (loop [result [1 1]
              index 2]
         (if (= index %)
           result
           (recur (conj result
                    (+
                     (nth result (- (count result) 1))
                     (nth result (- (count result) 2))))
             (inc index)))))))

(defcheck solution-e94343c0
  (fn [x] (take x (#(map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))))

(defcheck solution-e9b08291
  (fn [x] (take x (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))))

(defcheck solution-e9f0329
  #(loop [o [] a 1 b 0]
     (if (< (count o) %)
       (recur (conj o a) (+ a b) a)
       o)))

(defcheck solution-ea3b7e13
  #(let [fib-seq (fn rfib [a b] (cons a (lazy-seq (rfib b (+ a b)))))]
     (take % (drop 1 (fib-seq 0 1)))))

(defcheck solution-ea430102
  #(take % (map first (iterate (fn [[a b]] (vector b (+ a b))) [1 1]))))

(defcheck solution-ead4fbc3
  (fn fib [n]
    (if (= n 2)
      [1 1]
      (let [f (fib (dec n))]
        (conj f (+ (last f) (last (butlast f))))))))

(defcheck solution-eb06d640
  (fn fib [numTerms]
    (loop [n 2 t1 1 t2 1 s [1 1]]
      (if (>= n numTerms)
        s
        (let [sum (+ t1 t2)]
          (recur (inc n) t2 sum (conj s sum)))))))

(defcheck solution-eb090183
  (fn [n]
    (take n (map last
              (iterate (fn [[x y]] [y (+ x y)]) [0 1])))))

(defcheck solution-ebbf01d1
  #(take %
     (lazy-seq
       (loop[x1 1 x2 1 res [1]]
         (if (= (count res) %) res
                               (recur x2 (+ x1 x2) (conj res x2)))))))

(defcheck solution-ebc95482
  (fn thisfunc [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      true (let [l (thisfunc (dec n))]
             (conj l (+ (last l) (nth l (- (count l) 2))))))))

(defcheck solution-ec75ca10
  #(map
     first
     (take %
       (iterate
         (fn
           [x]
           (let
            [a (first x) b (first (rest x))]
             (list b (+ a b))))
         '(1 1)))))

(defcheck solution-ecd45ecb
  (fn hey [x] (if (= x 2) [1 1] (concat (hey (- x 1)) (list (+ (last (hey (- x 1))) (last (drop-last (hey (- x 1))))))))))

(defcheck solution-ed1f053f
  #(letfn [(fib [a b]
             (cons a (lazy-seq (fib b (+ a b)))))]
     (take % (fib 1 1))))

(defcheck solution-ed1f92dc
  #(take %
     (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-ed853e0c
  (fn [n]
    (take n
      (map second
        (map
          (fn [x]
            (reduce
              (fn [v _] [(second v) (reduce + v)])
              [0 1]
              (range x)
              )
            )
          (range)
          )
        )
      )
    ))

(defcheck solution-ede39467
  (fn [n]
    (take
      n
      ((fn fib [n1 n2]
         (lazy-seq
           (cons
             n1
             (fib n2 (+ n1 n2) )
             )
           )
         ) 1 1)
      )
    ))

(defcheck solution-ee8f3bda
  (fn fibonacci [n]
    (loop [i 1 p2 0 p1 1 ret [1]]
      (if (= i n)
        ret
        (recur (inc i) (identity p1) (+ p2 p1) (conj ret (+ p2 p1)))))))

(defcheck solution-eeb76512
  #(take %
     (map first
       (iterate (fn [[a b]] [b (+ a b)])
         [1 1]))))

(defcheck solution-eeb92429
  #(->> [1 1] (iterate (juxt last (partial apply +))) (take %) (map first)))

(defcheck solution-eedab3ee
  (fn f
    ([n] (f 1 1 n))
    ([x y n]
     (if (= 0 n)
       ()
       (cons x (f y (+ x y) (- n 1)))))))

(defcheck solution-ef053161
  ;; though I did not copy this just now, credit to Christophe Grand
  ;; for the implementation which I have internalized and recalled
  #(let [fib-vecs (iterate (fn [[a b]]
                             [b (+ a b)])
                    [0 1])]
     (take % (map second fib-vecs))))

(defcheck solution-ef4a312b
  #(take %
     ((fn fib
        ([] (fib 1 1))
        ([x y] (cons x (lazy-seq (fib y (+ x y)))))))))

(defcheck solution-ef4ef829
  (fn fibo-seq [n]
    (map (fn fibo [n]
           (cond
             (= n 1) 1
             (= n 2) 1
             :else (+ (fibo (dec n)) (fibo (dec (dec n))))))
      (range 1 (inc n)))))

(defcheck solution-ef7c3f64
  (fn [n]
    (map (fn fib [n]
           (if (< n 3)
             1
             (+ (fib (- n 1)) (fib (- n 2)))
             )
           ) (range 1 (+ 1 n)))))

(defcheck solution-effae784
  #(nth   (iterate (fn [c] (concat c (vector (apply + (take-last 2 c))))) [1 1]) ( -  % 2)))

(defcheck solution-f07a4467
  (fn [n]
    (letfn [(fib [a b] (lazy-seq (cons b (fib b (+ a b)))))]
      (take n (fib 0 1)))))

(defcheck solution-f080f574
  #(take % (
            (fn fi[a b] (cons a (lazy-seq (fi b (+ a b))))) 1 1)
     ))

(defcheck solution-f0d47431
  (fn [n]
    (take n (map first
              (iterate (juxt last (partial apply +)) [1 1])))))

(defcheck solution-f0d47909
  (fn nfib [n] (take n ((fn fib-seq []
                          (letfn [(fib-internal [curr next]
                                    (lazy-seq (cons curr (fib-internal next (+ curr next)))))]
                            (fib-internal 1N 1N)))))))

(defcheck solution-f0f16fff
  #(take %
     ((fn fib [n1 n2]
        (lazy-seq
          (cons n1 (fib n2 (+ n1 n2))))) 1 1)))

(defcheck solution-f14aa566
  (fn fib
    ([size] (fib size 1  1))
    ([size a b] (if (zero? size) nil (cons a (fib (dec size) b (+ a b)))))))

(defcheck solution-f1a6c089
  (fn my-fib [n]
    (loop [fib1 1
           fib2 1
           result []]
      (if (= (count result) n)
        result
        (recur fib2 (+ fib1 fib2) (conj result fib1))))))

(defcheck solution-f1bf342e
  (fn __ [x]
    (loop [ret [1 1], n x]
      (if (> n 2)
        (let [x1 (last ret), x2 (last (butlast ret))]
          (recur (conj ret (+ x2 x1)) (- n 1)))
        ret))))

(defcheck solution-f1e0402
  #(take % (map first (iterate (fn [[a b]][b (+ a b)])[1 1]))))

(defcheck solution-f258b480
  #(take %
     (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-f2953e30
  (fn fib [n]
    (if (= n 1)
      '(1)
      (concat
       (fib (- n 1))
       (list
         (if (= n 2)
           1
           (+ (last (fib (- n 1))) (last (fib (- n 2))))
           )
         )
       )
      )
    ))

(defcheck solution-f2c5865a
  #(take % ((fn fibonacci-numbers [a b] (cons a (lazy-seq (fibonacci-numbers b (+ a b))))) 1 1)))

(defcheck solution-f2d68380
  (let [fib (fn fib ([n] (fib n 0 1)) ([n current next] (if (zero? n) current (recur (dec n) next (+ current next)))))]
    #(map fib (range 1 (inc %)))))

(defcheck solution-f2e29da1
  (fn fib-seq
    ([i] (fib-seq (dec i) []))
    ([i, seq]
     (let [my-fib (fn my-fib [i]
                    (if (< i 2)
                      1
                      (+ (my-fib (dec i)) (my-fib (- i 2)))))]
       (if (< i 0)
         (reverse seq)
         (fib-seq (dec i) (conj seq (my-fib i))))))))

(defcheck solution-f3543b24
  #(loop [cnt %
          lst '()]
     (if (= cnt 0)
       (reverse lst)
       (cond
         (= (count lst) 0) (recur (dec cnt) '(1))
         (= (count lst) 1) (recur (dec cnt) '(1 1))
         :else (recur (dec cnt)
                 (conj lst
                   (apply + (take 2 lst))))))))

(defcheck solution-f4369ef6
  (fn problem26-fibs [x]
    (let [fibs
          ((fn fibs [so-far]
             (let [next (+ (first so-far) (second so-far))]
               (lazy-seq
                 (cons next (fibs (cons next so-far)))
                 )))
           '(1 1))]
      (into [1 1] (take (- x 2) fibs)))))

(defcheck solution-f466b80
  #((apply comp (repeat (- % 2) (fn [x] (conj x (+ (peek x) (peek (pop x))))))) [1 1]))

(defcheck solution-f477c384
  #(if (== 0 %) '()
                (loop [fibs '(1) cnt %
                       val1 0 val2 1]
                  (if (<= cnt 1)
                    fibs
                    (let [val3 (+ val1 val2)]
                      (recur (concat fibs (list val3))
                        (dec cnt)
                        val2 val3))))))

(defcheck solution-f47daa3c
  (fn fib [x]
    (if (= x 1)
      [1]
      (let [y (fib (dec x))]
        (let [z (take-last 2 y)]
          (let [k (if (= (count z) 1)
                    (first z)
                    (+ (first z) (last z)))]
            (conj y k)))))))

(defcheck solution-f4c2e265
  (fn prob-0026
    [n]
    (letfn [(fib-seq
              ([]    (fib-seq 1 1))
              ([a b] (lazy-seq (cons a (fib-seq b (+ a b)))))) ]

      (take n (fib-seq)))))

(defcheck solution-f5717d0f
  (fn [x] (loop [count x a 1 b 1 acc '()] ( if (zero? count) (reverse acc) ( recur (dec count) b (+ a b) (cons a acc)      )    )     )     ))

(defcheck solution-f5a7fe23
  #(take % (map first (iterate (fn [[x p]] [(+ x p) x]) [1 0]))))

(defcheck solution-f64e905c
  #(take %
     ((fn f [x y]
        (lazy-seq (cons x (f y (+ x y)))))
      1 1)))

(defcheck solution-f69cdac3
  (fn [start]
    (seq (loop [curr start n1 0 n2 1 s []]
           (if (> curr 0)
             (recur (dec curr) n2 (+ n1 n2) (conj s n2))
             s)))))

(defcheck solution-f6bdd21b
  (fn [x] (loop [ a 0 b 1 res [] n x]
            (if (zero? n) res
                          (recur b (+ a b) (conj res b) (dec n))))))

(defcheck solution-f71279ed
  (fn [n]
    (take n (map first (iterate (fn [[l r]]
                                  [r (+ l r)])
                         [1 1])))))

(defcheck solution-f719170f
  (fn fib [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (let [fs (fib (dec n))
                  fs' (reverse fs)]
              (conj fs (+ (first fs') (second fs')))))))

(defcheck solution-f77dfd31
  (fn [n] (nth (iterate #(conj % (apply + (take-last 2 %))) [1 1]) (- n 2))))

(defcheck solution-f7972bfe
  #(do (loop [n % i 2 col [1] la 0] (if (> i n) col (do (recur n (inc i) (conj col (+ la (last col))) (last col))) )) ))

(defcheck solution-f7a1d3e0
  #(->> [1 1] (iterate (fn [[a b]] [b (+ a b)])) (map first) (take %)))

(defcheck solution-f7c2310b
  (fn fib [n]
    (take n ((fn fib-r [a b]
               (lazy-seq (cons a (fib-r b (+ a b))))) 1 1))))

(defcheck solution-f7e800ef
  #(map (fn fib [x] (if (> x 1) (+ (fib (- x 1)) (fib (- x 2))) 1)) (range 0 %)))

(defcheck solution-f8492d69
  #(take %1 '(1 1 2 3 5 8 13 21)))

(defcheck solution-f8a70f85
  (fn fib [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      true (let [prev (fib (dec n))]
             (conj prev (+ (get prev (- n 2)) (get prev (- n 3))))))))

(defcheck solution-f8ce1532
  (fn fib [n]
    (loop [a 0 b 1 results [1]]
      (if (= (count results) n)
        results
        (recur b (+ a b) (conj results (+ a b)))
        )
      )
    ))

(defcheck solution-f8e020fd
  (fn [n]
    (loop [i 1 j 1 c [] n n]
      (if (zero? n) c (recur j (+ i j) (conj c i) (dec n))))))

(defcheck solution-f8e4b530
  (fn [n] (letfn [(
                    fibn-lazy [a b] (let [sum (+ a b)] (cons b (lazy-seq (fibn-lazy b sum))))
                    )] (take n (fibn-lazy 0 1)))))

(defcheck solution-f90abb89
  (fn [n]
    (loop  [fibs [1 1]]
      (if (= (count fibs) n)
        fibs
        (recur (conj fibs (+ (last fibs) (last (butlast fibs)))))))))

(defcheck solution-f9942c60
  #(take % (map (fn[[i _]] i) (iterate (fn[[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-f9b40b60
  (fn [n] (nth (iterate #(conj % (+ (last %) ((comp second reverse) %))) [1 1]) (- n 2))))

(defcheck solution-f9d7b06
  #(let [f (fn [[a b]] [b (+ a b)])]
     (take % (map first (iterate f [1 1])))))

(defcheck solution-f9ecec18
  ;#(loop [i 2 s [1 1]]
  ;  (if (&lt; i %)
  ;    (recur (inc i) (conj s (apply + (take 2 (reverse s)))))
  ;    s))

  #(loop [i 2 s '(1 1)]
     (if (= i %)
       (reverse s)
       (recur (inc i) (conj s (apply + (take 2 s)))))))

(defcheck solution-f9f877f6
  (fn [n]
    (let [fib-step (fn [[a b]] [b (+ a b)])
          fib-seq (map first (iterate fib-step [1 1]))]
      (take n fib-seq))))

(defcheck solution-fada8cb2
  (fn [n] ((fn [n a] (if (= n 0) (rest (rest (reverse a))) (recur (- n 1) (conj a (+ (first a) (first (rest a))))))) n '(0 1))))

(defcheck solution-faf31b7c
  (fn [n]
    (case n
      (1) '(1)
      (2) '(1 1)
      (reverse
        (loop [n (- n 2)
               sq '(1 1)]
          (if (= n 0)
            sq
            (recur (- n 1) (cons (+ (first sq) (first (rest sq))) sq))))))))

(defcheck solution-fb190022
  #(reverse
     (loop [n % coll '(1 1)]
       (if (= n 2)
         coll
         (recur (dec n) (cons (+ (first coll) (second coll)) coll))
         )
       )))

(defcheck solution-fb1aea74
  #(reverse ((fn fib [x]
               (case x
                 0 ()
                 1 '(1)
                 2 '(1 1)
                 (let [xs (fib (dec x))]
                   (cons
                     (+ (first xs) (second xs))
                     xs)))) %)))

(defcheck solution-fc19c61
  #(take %
     (map first
       (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defcheck solution-fc75a33c
  ; Bad algorithm, I know :)
  (fn fibs[x]
    (letfn [(fib[x]
              (cond (>= 0 x) 0
                    (= 1 x) 1
                    :else (+ (fib (- x 1)) (fib (- x 2)))))]
      (take x (map fib (iterate inc 1))))))

(defcheck solution-fc8551f7
  #(take % (map second (iterate (fn [[x y]] [y (+ x y)]) [0 1]))))

(defcheck solution-fce77cb5
  #(letfn [(fib-seq [first second]
             (lazy-seq (cons first (fib-seq second (+ first second)))))]
     (take % (fib-seq 1 1))))

(defcheck solution-febb8005
  (fn fibonacci [n]
    "Returns the first n Fibonacci numbers."
    (if (= n 1)
      [1]
      (if (= n 2)
        [1 1]
        (loop [counter 2, fibo [1 1]]
          (if (< counter n)
            (recur (inc counter) (conj fibo (+ (peek fibo) (peek (pop fibo)))))
            fibo))))))

(defcheck solution-ff07ec9c
  (fn
    [size]
    (loop [result [1]]
      (if (= size (count result))
        result
        (recur (conj result (reduce + (take-last 2 result))))
        )
      )
    ))

(defcheck solution-ff506018
  (fn [fibNum]
    (loop [fibIterIndex 1 actualFib 1 nextFib 1 fibListAccu [] ] (
                                                                   let [extendedFibList (conj fibListAccu actualFib)] (
                                                                                                                        if (= fibIterIndex fibNum)
                                                                                                                        extendedFibList
                                                                                                                        (recur (inc fibIterIndex) nextFib (+ actualFib nextFib) extendedFibList)
                                                                                                                        )))
    ))

(defcheck solution-ff6f5c1b
  #(take % ((fn fiby [x y] (lazy-seq (cons x (fiby y (+ x y))))) 1 1)))

(defcheck solution-ffd6c83a
  (fn fib-seq [n]
    (loop [m (if (< n 3) n 3)
           s [1 1]]
      (cond
        (= m 1) (list 1)
        (= m 2) (list 1 1)
        (> m n) (apply list s)
        :else (recur (inc m) (conj s (+ (last s) (last (butlast s)))))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-26))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
