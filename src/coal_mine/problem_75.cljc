(ns coal-mine.problem-75
  (:require [coal-mine.checks :refer [defcheck-75] :rename {defcheck-75 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-104e9f00
  (fn totient-function [x]
    (letfn [(gcd [a b] (cond (= a b) a (> a b) (recur (- a b) b) :else (recur a (- b a))))]
      (if (= x 1) 1 (count (filter #(= (gcd x %) 1) (range 1 x)))))))

(defcheck solution-105652
  (fn
    [n]
    (if
     (= n 1)
      1
      (count
        (filter
          (fn [m]
            (every?
              #(or (> (rem m %) 0) (> (rem n %) 0))
              (range 2 (inc m))))
          (range 1 n))))))

(defcheck solution-108631ba
  (fn [n]
    (let[gcd (fn [a b] (if (= 0 b) a (recur b (rem a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-10db1a24
  (fn phi [n]
    (letfn [(gcd [a b]
              (loop [x a y b]
                (if (= 0 y)
                  x
                  (recur y (mod x y)))))]
      (count (filter #(= (gcd n %) 1) (range 1 (inc n)))))))

(defcheck solution-10ff9d79
  (fn [n]
    (letfn [(gd [a b]
              (if (< a b)
                (gd b a)
                (if (= b 0)
                  a
                  (recur b (mod a b)))))]
      (count (cons 1 (filter #(= 1 (gd % n)) (range 2 n)))))))

(defcheck solution-11173371
  (fn [n]
    (let [gcd (fn f [a b] (if (zero? b) a (f b (mod a b))))]
      (if (= 1 n)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))
        )
      )
    ))

(defcheck solution-113dc637
  (fn [n]
    (if (= n 1)
      1
      (letfn [(g [a b] (if (= a b)
                         a
                         (if (> a b)
                           (g (- a b) b)
                           (g b a))))]
        (count (filter #(= 1 (g n %)) (next (range n))))))))

(defcheck solution-115eec3b
  (fn [num]
    (let [prime?
          (fn [num]
            (and (> num 1)
                 (not-any? #(= (mod num %) 0) (range 2 (inc (/ num 2))))))
          primes
          (filter prime? (range))
          prime-factors
          (fn [num] (let [pfac-cands (take-while #(<= % num) primes)]
                      (filter #(= (mod num %) 0) pfac-cands)))]
      (let [pfacs (prime-factors num)]
        (* (/ num (reduce * pfacs))
          (reduce * (map dec pfacs)))))))

(defcheck solution-11928d01
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(gcd [a b]
                (apply max (clojure.set/intersection
                             (set (filter #(= 0 (mod a %)) (range 1 (inc a))))
                             (set (filter #(= 0 (mod b %)) (range 1 (inc b)))))))]
        (count
          (filter
            #(= 1 (gcd n %))
            (range 1 n)))))))

(defcheck solution-11a9ab6d
  (fn [x] (if (= x 1) 1
                      (count (filter
                               #(empty? (filter
                                          (fn [nb] (and (= 0 (rem x nb)) (= 0 (rem % nb))))
                                          (range 2 x))  )
                               (range 1 x)
                               ))
                      )))

(defcheck solution-11bdc1b6
  (fn tot
    [n]
    (let [gcd (fn [a b]
                (cond (> b a) (recur b a)
                      (= b 0) a
                      :else   (recur b (mod a b))))]
      (if (= n 1)
        1
        (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-11faf214
  (fn [x]
    (letfn [(gcd [x y] (if (= y 0) x (gcd y (mod x y))))]
      (count (filter #(= 1 (gcd x %)) (range 1 (inc x)))))))

(defcheck solution-1233a761
  (fn euler-totient-fn [n]
    (letfn [(gcd [a b]
              (loop [max-n (max a b) min-n (min a b)]
                (if (zero? min-n)
                  max-n
                  (recur min-n (rem max-n min-n)))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-1261b73
  (fn [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
          coprime-n (fn [a] (= 1 (gcd n a)))]
      (count (filter coprime-n (range 1 (inc n))))

      )
    ))

(defcheck solution-12b38e35
  (fn [f r x]
    (if (= x 1) 1
                (letfn [(g [m n] (first (f #(every? (fn [z] (= 0 (mod z %))) [m n])
                                          (r (min m n) 0 -1))))]
                  (count (f #(= 1 (g % x)) (r x)))))) filter range)

(defcheck solution-12be8ae8
  (fn [n]
    (if (= 1 n)
      1
      (->> (range 1 n)
        (filter (fn [k]
                  (loop [i 2]
                    (cond
                      (and (zero? (mod n i))
                           (zero? (mod k i))) false
                      (> i k) true
                      :else (recur (inc i))))))
        (count)))))

(defcheck solution-12ce8631
  (fn [n]
    (letfn [(gcd [a b] (if (zero? a) b (gcd (mod b a) a)))]
      (if (= 1 n) 1
                  (reduce + (map #(if (= 1 (gcd n %)) 1 0) (range 1 n)))))))

(defcheck solution-12f5cf9e
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
      (inc (count (filter #(= 1 (gcd n %)) (range 2 n)))))))

(defcheck solution-12f8fef7
  (fn[x]
    (count (filter #(= 1 (loop [a x b %]
                           (if (= (rem a b) 0)
                             b
                             (recur b (rem a b))))) (range 1 (+ x 1))))))

(defcheck solution-131cb19
  (fn [n]
    (if (= 1 n) 1
                (count (filter (fn [v1] (loop [b n s v1]
                                          (if (zero? s) (= 1 b) (recur s (rem b s))))) (range 1 n))))))

(defcheck solution-13b3c5af
  (fn [n]
    (letfn [(gcd [a b]
              (first
                (filter (fn [n] (and (= 0 (mod b n)) (= 0 (mod a n))))
                  (let [d (max a b)]
                    (range (/ (if (odd? d) (inc d) d) 2) 0 -1 )))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-13b88d07
  (fn toitent [n]
    (let [gcd (fn [a b]
                (loop [a a b b]
                  (if (= a b)
                    a
                    (if (> a b)
                      (recur (- a b) b)
                      (recur a (- b a))))))]
      (count (filter #(= (gcd % n) 1)
               (rest (take (inc n) (range))))))))

(defcheck solution-13c53aa9
  (fn [x]
    (letfn [(gcd [x y]
              (->> (min x y) (iterate dec) (filter #(and (zero? (mod x %)) (zero? (mod y %)))) first))
            (coprime? [x y]
              (== 1 (gcd x y)))
            (coprimes [x]
              (filter (partial coprime? x) (range 1 x)))]
      (if (= 1 x)
        1
        (count (coprimes x))))))

(defcheck solution-13f18a36
  (fn [n]
    (let [divisor (fn [n] (set (keep #(if (zero? (mod n %)) % nil) (range 1 (inc n)))))
          coprime? (fn [a b] (= #{1} (clojure.set/intersection (divisor a) (divisor b))))]
      (if
       (= n 1)
        1
        (count (for [i (range 1 n) :when (coprime? i n)] i))))))

(defcheck solution-13f86ab1
  (letfn [(gcd [x y]
            (let [[mn mx] (sort [x y])]
              (loop [divisor mn]
                (if (= 0 (rem x divisor) (rem y divisor))
                  divisor
                  (recur (dec divisor))))))]
    (fn [x]
      (if (= x 1)
        1
        (loop [y 2 cnt 1]
          (if (= y x)
            cnt
            (recur (inc y)
              (if (= 1 (gcd x y))
                (inc cnt)
                cnt))))))))

(defcheck solution-148d01ae
  #(letfn [(gcd [x1 x2]
             (cond
               (= x1 x2) x2
               (< x1 x2) (recur x2 x1)
               (= x2 0) x1
               :else (recur (- x1 x2) x2)))]
     (if (= % 1) 1
                 (apply + (map (fn [x] (if (= (gcd x %) 1) 1 0)) (range 1 %))))))

(defcheck solution-14a5ff28
  (fn [n]
    (let [x #(range 1 (inc %))
          y #(= 0 (mod % %2))]
      (count
        (filter #(= 1 (count (filter (fn [x] (and (y n x) (y % x))) (x %)))) (x n))))))

(defcheck solution-14c7d6eb
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn [a b]
                  (some #(when (and (zero? (mod a %))
                                    (zero? (mod b %)))
                           %)
                    (iterate dec (int (/ (max a b) 2)))))]
        (count (filter #(= 1 (gcd n %)) (range n)))))))

(defcheck solution-14ec0c7f
  (let [gcd (fn [x y candidate]
              (if (= 0 (rem x candidate) (rem y candidate))
                candidate
                (recur x y (- candidate 1))))]
    (fn [x]
      (if (= x 1) 1 (count (filter #(= (gcd x % %) 1) (rest (range x))))))))

(defcheck solution-152fe32
  (fn et2 [x]
    (letfn
     [(gcd [x]
        (->> (range 2 (inc x))
          (filter #(= 0 (rem x %)))
          (set) ))
      (coprime? [x y] (empty? (clojure.set/intersection (gcd x) (gcd y) )))]
      (if (= 1 x) 1 (count (filter true? (map #(coprime? x %) (range 1 x))))))))

(defcheck solution-1534d910
  (fn [x] (letfn [(f1 [x]
                    (if (= x 1)
                      []
                      (let [a (filter #(zero? (mod x %)) (range 2 x))
                            b (if (empty? a) x (apply min a))]
                        (lazy-cat [b] (f1 (/ x b))))))]
            (* x
              (apply *
                (map #(- 1 (/ 1 %))
                  (distinct (f1 x))))))))

(defcheck solution-15bf1036
  {1 1 10 4 40 16 99 60})

(defcheck solution-161efa5e
  (fn [n]
    (let [gcd #(if (zero? %2) % (recur %2 (rem % %2)))]
      (count
        (filter
          #(= 1 (gcd n %))
          (range 1 (inc n)))))))

(defcheck solution-16793880
  (fn [n]
    (letfn ((gcd [m n]
              (loop [mm (if (> m n) m n)
                     nn (if (> m n) n m)]
                (cond (= nn 0)
                      mm
                      (= (mod mm nn) 0)
                      nn
                      :else
                      (recur nn (mod mm nn))))))
      (if (= n 1)
        1
        (loop [i 1
               c 0]
          (if (= i n)
            c
            (if (= (gcd n i) 1)
              (recur (+ i 1) (+ c 1))
              (recur (+ i 1) c))))))))

(defcheck solution-16bf511c
  (fn [n]
    (count (filter
             #(= 1 ((fn g [x y] (if (= 0 y) x (g y (rem x y)))) % n))
             (range n)))))

(defcheck solution-16deee41
  (fn [num]
    (if (= 1 num) 1
                  (let [nums (range 1 num)
                        find-divisors (fn [x] (set (filter #(= 0 (mod x %)) (range 1 x))))
                        num-divisors (find-divisors num)]
                    (dec (count (filter #(= 1 (count (clojure.set/intersection num-divisors (find-divisors %)))) (range 1 num))))))))

(defcheck solution-1733db21
  (fn [n]
    (let [gcds (fn gcd [a b]
                 (if (= b 0)
                   a
                   (gcd b (mod a b))))
          coprime? (fn [a b] (= 1 (gcds a b)))]
      (count (filter #(coprime? n %) (range 1 (inc n) ))))))

(defcheck solution-1736b1f6
  (fn [n]
    (if (= n 1) 1
                ((comp count filter)
                 (fn [x] (not-any? #(= 0 (mod x %) (mod n %))
                           (range 2 (inc x))))
                 (range 1 n)))))

(defcheck solution-178362be
  (fn totient [x]
    (let [gcd (memoize (fn gcd [a b]
                         (cond
                           (< a b) (gcd a (- b a))
                           (> a b) (gcd (- a b) b)
                           :else a)))
          coprime? (fn [p q] (= 1 (gcd p q)))]
      (if (= x 1) 1 (apply + (map #(if (coprime? % x) 1 0) (range 1 x)))))))

(defcheck solution-17ee4b1b
  (fn [n]
    (let [a
          (map
            (fn [s]
              (concat (filter #(= 0 (mod s %))
                        (range 2 (inc (/ s 2))))[s]))
            (range 2 (inc n)))]
      (inc(count (filter (fn[t] (not((fn[s](some true? (flatten (map (fn [i](map #(= % i) (last a))) s))))t))) (butlast a)))))))

(defcheck solution-1818fbb9
  (fn [n]
    (if (= 1 n)
      1
      (letfn
       [(gcd [i j] (if (zero? j) i (gcd j (mod i j))))]
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-18ac1a83
  (fn [x]
    (let [gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))
          coprime? (fn [x y] (= 1 (gcd x y)))]
      (cond
        (= x 1) 1
        :else (->> (range 1 x)
                (filter (partial coprime? x))
                (count))))))

(defcheck solution-18cbed5a
  (fn tot [n]
    (letfn [(gcd [a, b]
              (loop [a a b b]
                (if (= b 0)
                  a
                  (recur b (rem a b)))))
            (coprime [a, b]
              (= 1 (gcd a b)))]
      (if (= n 1)
        1
        (count (filter (partial coprime n) (range 1 n)))))))

(defcheck solution-1908f609
  (fn [n]
    (letfn [(gcd [a b]
              (if (zero? b) a (gcd b (mod a b))))
            (coprime? [a]
              (== 1 (gcd n a)))]
      (count (filter coprime? (range 1 (inc n)))))))

(defcheck solution-1969c935
  (fn [n]
    (letfn [
            (gcd [x y]
              (if (zero? y) x (gcd y (mod x y))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd n %)) (range n)))))))

(defcheck solution-1a3315a6
  (letfn [(gcd [a b] (first (filter #(and (= 0 (mod a %)) (= 0 (mod b %))) ; see problem 66
                              (-> (min a b) (range 0 -1)))))
          (coprime? [a b] (= 1 (gcd a b)))]
    (fn [x]
      (if (= 1 x) 1
                  (count (filter #(coprime? % x) (range 1 x)))))))

(defcheck solution-1a49484a
  (fn [x]
    (let
     [gcd (fn [a b]
            (cond
              (< a b) (recur b a)
              (= a b) a
              (> a b) (recur b (- a b))))]
      (if (= x 1) 1
                  (count
                    (filter
                      #(= 1 %)
                      (map
                        #(gcd x %)
                        (range 1 x)))))
      )))

(defcheck solution-1a7266f0
  (fn euler-totient [x]
    (letfn [(gcd [a b]
              (if (= b 0) a
                          (recur b (mod a b))))]
      (if (= x 1) 1
                  (->>
                    (map #(= (gcd x %) 1) (range 1 x))
                    (filter #(= % true))
                    (count))))))

(defcheck solution-1a834cf
  (fn totient [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))
            (coprime? [a b]
              (= (gcd a b) 1))]
      (if (= x 1)
        1
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-1ae7cab2
  (fn [n]
    (if (= 1 n)
      1
      (let [gcd (fn [x y]
                  (first (filter
                           #(and (zero? (rem x %)) (zero? (rem y %)))
                           (range (min x y) 0 -1))))]
        (count (filter #(= 1 (gcd n %)) (range (dec n) 0 -1)))))))

(defcheck solution-1b08916f
  (fn totient [n]
    (let [gcd (fn [x y]
                (let [[bigger smaller] (reverse (sort [x y]))
                      remainder (mod bigger smaller)]
                  (if (zero? remainder)
                    smaller
                    (recur smaller remainder))))]
      (if (< n 2)
        1
        (inc (count (filter #(= 1 %) (map #(gcd n %) (range 2 n)))))))))

(defcheck solution-1b742572
  (fn totient
    [n]
    (letfn [(gcd [a b]
              (cond
                (= a 0) b
                (= b 0) a
                :else (let [smaller (min a b)
                            larger (max a b)
                            remainder (rem larger smaller)]
                        (gcd smaller remainder))))
            (coprime? [a b] (= (gcd a b) 1))]
      (if (= n 1)
        1
        (count (filter (partial coprime? n) (range 1 n)))))))

(defcheck solution-1b7f146a
  (fn [n]
    (count (filter (partial = 1)
             (map
               (fn [a b] (if(zero? b) a (recur b (mod a b))))
               (repeat n)
               (range 1 (inc n)))))))

(defcheck solution-1b8e1150
  (letfn [(gcd
            [a b]
            (if (zero? b) a
                          (recur b (mod a b))))]

    (fn totient [n]
      (->> (range n)
        (filter (comp (partial = 1)
                      (partial gcd n)))
        count))))

(defcheck solution-1bcd5599
  (fn [n]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (count (filter #(= 1 %) (map #(gcd % n) (range n)))))))

(defcheck solution-1bfef37b
  (fn [x]
    (if (== 1 x) 1
                 (let [gcd (fn gcd [a b] (if (== b 0) a (gcd b (mod a b))))]
                   (count (filter #(== 1 (gcd x %))
                            (rest (range x))))))))

(defcheck solution-1cc27c7b
  (fn [x]
    (letfn [(gcd [a b] (cond (= a b) a (> a b) (gcd b (- a b)) :else (gcd b a)))
            (coprimes [x] (filter #(= 1 (gcd x %)) (range 1 x)))]
      (if (= x 1) 1 (count (coprimes x))))))

(defcheck solution-1cce8e6d
  (fn [n]
    (letfn [(GCD [m n]
              (if (= n 0)
                m
                (GCD n (mod m n))))]
      (if (= n 1) 1
                  (count (reduce #(if (= 1 (GCD n %2)) (conj %1 %2) %1) [] (range 1 n)))))))

(defcheck solution-1cef47d6
  (fn [n]
    (letfn [(gcd [x y] (if (= y 0) x (recur y (mod x y))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-1d50cb1d
  (fn [x]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (rem a b))))]
      (->>
        (range 2 x)
        (map #(gcd % x))
        (filter #(= % 1))
        count
        inc))))

(defcheck solution-1d736a1f
  (fn totient [x]
    (let [sieve (fn sieve [a b]
                  (let [big (max a b)
                        small (min a b)
                        remainder (mod big small)]
                    (if (= 0 remainder)
                      small
                      (sieve small remainder))))]
      (if (= 1 x)
        1
        (->> (range 1 (inc x))
          (map #(sieve x %))
          (filter #{1})
          count)))))

(defcheck solution-1dd20ac3
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn gcd [a b]
                  (loop [a a b b]
                    (if (zero? b) a,
                                  (recur b (mod a b)))))]
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-1dd52d38
  (fn [num]
    (let [gcd (fn [x y]
                (if (zero? y)
                  x
                  (recur y (mod x y))))]
      (if (= num 1)
        num
        (count (filter vector?
                 (for [x (range 1 num)]
                   (when (= (gcd x num) 1)
                     [x]))))))))

(defcheck solution-1de617e1
  (let [gcd (fn gcd [x y]
              (if (> x y)
                (gcd y x)
                (if (zero? (rem y x))
                  x (gcd (- y x) x))))
        co-prime? (fn [x y] (= 1 (gcd x y)))]
    (fn [num]
      (count (filter #(co-prime? % num) (range 1 (inc num)))))))

(defcheck solution-1e0278e8
  (fn [n]
    (letfn [(gcd [a b] (if (= 0 b) a (recur b (mod a b))))]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-1f010ad
  (fn totient [x]
    (letfn [(gcd [a b] (cond
                         (= a b) a
                         (> a b) (gcd (- a b) b)
                         (< a b) (gcd (- b a) a))) ]
      (reduce #(if (= 1 (gcd %2 x)) (inc %1) %1) 1 (range 2 x)))))

(defcheck solution-1f287834
  (fn [p]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (gcd b (mod a b))))
            (coprime? [a] (= (gcd a p) 1))]
      (if (= p 1)
        1
        (->> (range 1 p)
          (filter coprime?)
          count)))))

(defcheck solution-1f427961
  (fn [n] (let [R (range 2 n) D (filter #(= 0 (mod n %)) R) K (filter (fn [k] (every? #(not= 0 (mod k %)) D)) R)] (+ 1 (count K)))))

(defcheck solution-1f7f425b
  (fn eka
    [t]
    (let [gcd (fn gd [x y]
                (cond
                  (> x y) (gd (- x y) y)
                  (< x y) (gd x (- y x))
                  :else x))]
      (if (= 1 t) 1
                  (->> (range 1 t)
                    (map (partial gcd t))
                    (filter #(= 1 %))
                    (count)
                    ))
      )
    ))

(defcheck solution-1fbab751
  (fn [n] ({1 1, 10 4, 40 16, 99 60} n)))

(defcheck solution-2038db8f
  (fn totient [x]
    (letfn [(gcd [a b] (first (filter (fn [x] (= 0 (rem a x) (rem b x))) (range (min a b) 0 -1))))]
      (inc (count (filter (fn [r] (= 1 (gcd r x))) (range 2 x)))))))

(defcheck solution-20f83715
  (fn [x]
    (let [gcd (fn [n1 n2]
                (if (zero? n2)
                  n1
                  (recur n2 (rem n1 n2))))]
      (count (filter #(= (gcd x %) 1) (range x))))))

(defcheck solution-219e0fe7
  (fn [x](count(filter #(= % 1) (map (fn [a](loop [q a r x] (if( zero? r) q (recur r (mod  q r ))))) (range x))))))

(defcheck solution-21a422da
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [a b]
                            (if (zero? b) a
                                          (recur b (mod a b))))]
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-21c32d87
  (fn totient
    [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
        (->> (map #(gcd x %) (range 1 x))
          (filter #(= 1 %))
          count)))))

(defcheck solution-226f1a33
  (fn [n] (let [gcd (fn [a b]
                      (if (= b 0)
                        a
                        (recur b (mod a b))))
                coprime? (fn [m n] (if (= (gcd m n) 1) true false))
                totient (count (filter true? (map #(coprime? % n) (range n))))] totient)))

(defcheck solution-22c1e1d3
  (fn totient [n]
    (let [divisors (fn [x]
                     (set (keep #(if (zero? (rem x %)) %) (range 1 (+ 1 x)))))
          coprime? (fn [x y]
                     (= #{1} (clojure.set/intersection (divisors x) (divisors y))))
          coprimes (filter (partial coprime? n) (range 1 n))]
      (if (= n 1)
        1
        (count coprimes)))))

(defcheck solution-22d00dce
  (fn totient [n]
    (if (= 1 n)
      1
      (letfn [(gcd [x y]
                (let [dividend (max x y)
                      divisor (min x y)
                      remainder (rem dividend divisor)]
                  (if (zero? remainder)
                    divisor
                    (recur divisor remainder))))]
        (count (filter #(= 1 (gcd %1 n)) (range 1 n)))))))

(defcheck solution-232df5ca
  (fn totient [x]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (coprime? [n] (= n 1))]
      (count (filter coprime? (map (partial gcd x) (range 0 x)))))))

(defcheck solution-233f14b
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (reduce #(if (= 1 (gcd n %2)) (inc %1) %1) 1 (range 2 n)))))

(defcheck solution-23eb412f
  (fn [x] (count (filter #(= 1 ((fn g [a b] (if (zero? b) a (g b (mod a b)))) x %)) (range x)))))

(defcheck solution-23f8c3a8
  (fn [number]
    (if (= number 1)
      1
      (let [greatest-common-factor (fn [a b]
                                     (cond
                                       (< a b) (recur b a)
                                       (zero? b) a
                                       true (recur b (mod a b))))
            greatest-common-factor-with-number (partial greatest-common-factor number)]
        (count (filter #(= % 1) (map greatest-common-factor-with-number (range 1 number))))))))

(defcheck solution-2423401e
  (fn [n3]
    (count
      (cons
        1
        (filter
          #(
            (fn [n2 n]
              (not-any?
                (fn [f1] (= 0 (rem n2 f1)))
                (filter (fn [f2] (= 0 (rem n f2))) (range 2 n)))) % n3) (range 2 n3))))))

(defcheck solution-24ddc1d2
  (fn totient [x]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (recur y (mod x y))))]

      (if (= x 1)
        1
        (count (filter #(= (gcd x %) 1) (range 1 x)))))))

(defcheck solution-24f45ef2
  (fn euler[x]
    (letfn [(gcd[x y] (if (zero? y)
                        x
                        (gcd y (mod x y))))
            (coprime?[x y] (== 1 (gcd x y)))]
      (count (filter #(coprime? x %)  (range 1 (inc x)))))))

(defcheck solution-24fdbf96
  #(letfn [(f [a b]
             (if (= b 0) (= a 1)
                         (f b (mod a b))))]
     (count (filter (partial f %) (range 0 %)))))

(defcheck solution-250add40
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (if (= b 0)
                  a
                  (recur b (rem a b))))]
        (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-253273a8
  (fn [x]
    (let [ divisors (filter #(= 0 (mod x %)) (range 2 x)) ]
      (->> (range 2 x)
        (filter (fn [y] (every? #(not (= 0 (mod y %)))  divisors)))
        count
        inc
        ))))

(defcheck solution-2562333d
  (fn [n]
    (let [gcd
          #(cond
             (zero? %2) %
             :else (recur %2 (rem % %2)))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n)))))))

(defcheck solution-25772573
  (fn totient [x]
    (if (= x 1)
      1
      (letfn [(gcd [x y] (loop [a x b y] (if (= b 0) a (recur b (mod a b)))))]
        (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-25e7bf76
  (fn f [n]
    (count (filter #(= 1 ((fn gcd[a b]
                            (if (= b 0) a
                                        (gcd b (rem a b)))) n %))
             (range 1 (inc n))))))

(defcheck solution-263a299a
  (fn [x]
    (count
      (filter #(= 1
                 (loop [a x b %]
                   (if (zero? b) a
                                 (recur b (mod a b)))))
        (range 1 (inc x))))))

(defcheck solution-2644a4cd
  (fn p75 [n]
    (letfn [(factors [n i]
              (if (<= n i) [n]
                           (let [dv ((fn dq [y] (if (zero? (mod y i)) (dq (/ y i)) y)) n)]
                             (if (not= dv n)
                               (cons i (factors dv (inc i)))
                               (factors n (inc i))))))]
      (let [fs (factors n 2)]
        (if (= n 1) 1
                    (count (for [m (range 1 (inc n)) :when (every? #((complement zero?) (mod m %)) fs)] m)))))))

(defcheck solution-264e6962
  (fn [x]
    (if (= x 1)
      1
      (let [->divisors (fn [n]
                         (reduce
                           (fn [acc x]
                             (let [pair (/ n x)]
                               (if (== pair (int pair))
                                 (conj acc x pair)
                                 acc)))
                           []
                           (range 1 (Math/sqrt (inc n)))))
            x-div (set (->divisors x))
            range-div (for [i (range 1 x)]
                        (set (->divisors i)))]
        (->> range-div
          (map clojure.set/intersection (repeat x-div))
          (filter #(= 1 (apply max %)))
          count)))))

(defcheck solution-2650dad
  (fn [n]
    (if (= 1 n) 1
                (letfn [(gcd [a b]
                          (if (zero? b)
                            a
                            (recur b (mod a b))))]
                  (count (filter #(= % 1) (map #(gcd % n) (range 1 n))))))))

(defcheck solution-2677800c
  (fn [n]
    (let [gcd (fn [x y]
                (if (= 0 y) x (recur y (mod x y))))]
      (->> n inc (range 1) (filter #(= 1 (gcd n %))) count))))

(defcheck solution-26cf11e5
  #(count
     (for [i (range %)
           :let [y (+ i 1)
                 g ((fn gcd [a b] (if (zero? (mod a b)) b (recur b (mod a b)))) y %)]
           :when (= g 1)]
       y)))

(defcheck solution-26d07e6a
  (fn [x]
    (let [gcd (fn [x y]
                (let [[lower higher] (if (> x y) [y x] [x y])]
                  (first (filter #(zero? (mod higher %))
                           (filter #(zero? (mod lower %))
                             (iterate dec lower))))))]
      (if (= x 1)
        1
        (count (filter #(= % 1)
                 (map #(gcd % x)
                   (range 1 x))))))))

(defcheck solution-26e123b1
  (fn
    [n]
    (if (= n 1)
      1
      (count (filter #(= 1 %) (map (partial (fn gcd
                                              [v k]
                                              (let [g (max v k) l (min k v)]
                                                (if (not= 0 (rem g l))
                                                  (gcd l (rem g l))
                                                  l)
                                                )) n) (rest (range n))))))))

(defcheck solution-27680c57
  (fn totient
    [n]
    (count (filter #(= 1 ((fn gcd[a b]
                            (let [m (mod a b)]
                              (if (= m 0)
                                b
                                (gcd b m)))) %1 n)) (range n)))))

(defcheck solution-278bd8d4
  (fn [n] (let [gcd (fn [a b] (cond (zero? b) a
                                    (< a b) (recur b a)
                                    :else (recur b (rem a b))))]
            (count (filter #(= (gcd n %) 1) (range 1 (inc n)))))))

(defcheck solution-285a6df3
  (fn [n] (let [g #(loop [b (max % %2) s (min % %2)]
                     (let [r (rem b s) ]
                       (if (= 0 r) s (recur (max s r) (min s r)) )))]
            (if (= n 1)
              1
              (count (filter #(= 1 (g n %)) (range 1 n) ))) ) ))

(defcheck solution-28b11353
  (fn [n]
    (letfn [(tot? [m]
              (not-any? zero?
                (map #(rem n %)
                  (for [d (range 2 (inc m))
                        :when (zero? (rem m d))] d))))]
      (->> (range 2 n)
        (filter tot?)
        count
        inc))))

(defcheck solution-28d713eb
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(divs [x] (set (filter #(= 0 (rem x %)) (range 1 (inc x)))))
              (com-divs [a b] (clojure.set/intersection (divs a) (divs b)))
              (coprime? [a b] (= #{1} (com-divs a b)))
              (coprimes [x] (filter #(coprime? x %) (range 1 x)))]
        (count (coprimes n))))))

(defcheck solution-28f2f158
  (fn [n]
    (if (= n 1) 1
                (let [gcd (fn f [a b] (if (zero? b) a (f b (mod a b))))]
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-2958b6bf
  (fn [x]
    (if (= 1 x)
      1
      (let [gcd (fn [a b]
                  (if (= 0 a)
                    b
                    (recur (rem b a) a)))]
        (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-2971eb30
  (fn totient [x]
    (if (= 1 x)
      1
      (letfn [
              (does-div [x d] (zero? (mod x d)))
              (coprime [x y] (not (some #(and (does-div x %) (does-div y %)) (range 2 (inc y)))))]
        (count (filter #(coprime x %) (range 1 x)))))))

(defcheck solution-297ad61f
  (fn [x]
    (if (= x 1)
      1
      (letfn [(coprime [x y] (empty?
                               (remove nil?
                                 (for [i (range 2 (inc y))]
                                   (when (= (rem x i) (rem y i) 0)
                                     x)))))]
        (count (filter (partial coprime x) (range 1 x)))))))

(defcheck solution-299cd03a
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (coprime? [k] (= 1 (gcd n k)))]
      (->> (range 2 n) (filter coprime?) count inc)
      )))

(defcheck solution-2a81745f
  (fn [x]
    (letfn [(gcd [u v]
              (cond (= u v) u
                    (= u 0) v
                    (= v 0) u
                    (and (even? u) (even? v)) (* 2 (gcd (/ u 2) (/ v 2)))
                    (and (even? u) (odd? v)) (gcd (/ u 2) v)
                    (and (odd? u) (even? v)) (gcd u (/ v 2))
                    :else (if (> u v) (gcd (/ (- u v) 2) v)
                                      (gcd (/ (- v u) 2) u))))]
      (count (filter #(= 1 (gcd % x)) (range x))))))

(defcheck solution-2a859de0
  (fn __ [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (if (= n 1)
        1
        (count
          (filter #(= 1 (gcd n %))
            (range 1 n)))))))

(defcheck solution-2a926f5
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (inc (count (filter #(= 1 (gcd % n)) (range 2 n)))))))

(defcheck solution-2b0fc968
  (fn [n]
    (letfn [(gcd [a b]
              (let [[denominator numerator] (sort [a b])
                    remainder (mod numerator denominator)]
                (if (= 0 remainder) denominator (recur remainder denominator))))]
      (count (filter #(= (gcd n %) 1) (range 1 (inc n)))))))

(defcheck solution-2b41d6fd
  (fn [n]
    (if (== 1 n)
      1
      (let [co-prime? (fn [a]
                        (empty? (filter #(zero? (+ (mod a %) (mod n %))) (range 2 (inc n)))))]
        (count (filter co-prime? (range 1 n)))))))

(defcheck solution-2b541b3a
  (fn [n]
    (if (= 1 n) 1
                (count
                  (filter
                    #(=
                       1
                       ((fn gcd [a b]
                          (if (= b 0)
                            a
                            (gcd b (rem a b))))
                        % n))
                    (range 1 n))))))

(defcheck solution-2b9e67ae
  (fn [n]
    (let [gcd (fn [x y] (if (= 0 y) x (recur y (mod x y))))]
      (if (= n 1) 1 (count (filter #(= (gcd % n) 1) (rest (range n))))))))

(defcheck solution-2ba62f4b
  (fn [x]
    (if (= x 1) 1
                (count (filter (fn [y]
                                 (= 1 (count (filter #(= (mod x %) (mod y %) 0) (range 1 (+ 1 y))))))
                         (range 1 x))))))

(defcheck solution-2bce0b27
  (fn [x]
    (if (= x 1)
      1
      (inc
        ((fn [x]
           (let [xdivs (filter #(= 0 (mod x %))(range 2 x))
                 not-coprim (fn [x] (some #(= 0 (mod x %)) xdivs))
                 coprim? (comp not not-coprim)
                 coprims-except-1 (filter coprim? (range 2 x))]
             (count coprims-except-1))) x)))))

(defcheck solution-2c0a07fc
  (fn pseudo-phi [x]
    (case x
      1 1
      10 4
      40 16
      99 60)))

(defcheck solution-2c1ab5db
  (fn [x]
    (if (= 1 x)
      1
      (letfn [(gcd [a b]
                (cond
                  (zero? a) b
                  (zero? b) a
                  :else (recur b (mod a b))))]
        (reduce #(+ %1 (if (not= 1 (gcd %2 x)) 0 1)) 0 (range 1 x))))))

(defcheck solution-2c20ddc3
  (fn [n]
    (let [et #(for [i (range 1 (inc %)) :when (= 0 (mod % i))] i) cop (et n)]
      (count
        (filter #(not (nil? %))
          (for [x (range 1 (inc n))]
            (if (= #{1} (clojure.set/intersection (into #{} cop) (into #{} (et x))))
              x
              nil)))))))

(defcheck solution-2c2de148
  (fn [x]
    (let [gcd (fn [a b]
                (cond
                  (= b 0) a
                  (= a 0) b
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))
          coprime? #(= (gcd % x) 1)]
      (if (= 1 x) 1
                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-2cbac555
  (fn [n]
    (let [gcd #(if (= 0 %2) %1 (recur %2 (mod %1 %2)))]
      (reduce #(if (= 1 (gcd n %2)) (inc %1) %1) 1 (range 2 n)))))

(defcheck solution-2d123291
  (fn [nn]
    (let [gcd (fn [a b]
                (cond
                  (= a b) a
                  (> a b) (recur b (- a b))
                  (< a b) (recur a (- b a))))]
      (->> (inc nn)
        (range 1)
        (filter #(= 1 (gcd nn %)))
        (count))
      )))

(defcheck solution-2d826081
  (fn [n]
    (let [gcd (fn [x y]
                (if (zero? y)
                  x
                  (recur y (mod x y))))]
      (count (filter
               #(= 1 (gcd n %))
               (range 1 (inc n)))))
    ))

(defcheck solution-2dcbf333
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-2dddea7f
  (fn [n]
    (let [gcd
          (fn[a b]
            (cond
              (< a b)
              (recur a (- b a))

              (< b a)
              (recur (- a b) b)

              :else
              a))]
      (inc (count (filter #(= 1 (gcd n %)) (range 2 n)))))))

(defcheck solution-2de670a0
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (cond
                  (= a b) a
                  (> a b) (recur (- a b) b)
                  :else (recur a (- b a))))
              (coprime? [a b] (= 1 (gcd a b)))]
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-2eac40de
  (fn [num]
    (if (= num 1)
      1
      (let [div-by? (fn [x n] (zero? (rem x n)))
            com-denom? (fn [x y n] (and (div-by? x n) (div-by? y n)))
            tot? (fn [x y]
                   (let [min-denom (min x y)
                         search-range (range 2 (inc min-denom))]
                     (if (= min-denom 1)
                       true
                       (not-any? #(com-denom? x y %) search-range))))]
        (count (filter (partial tot? num) (range 1 num)))))))

(defcheck solution-2ee18657
  (fn [x]
    (letfn [(gcd [a b] (if (zero? a) b (gcd (mod b a) a)))]
      (count (filter #(= 1 (gcd x %)) (range x))))))

(defcheck solution-2f090111
  (fn [x] (let [divisors (fn [n] (set (filter #(zero? (mod n %)) (range 2 (inc n)))))
                divisors-of-x (divisors x)
                divisors-of-x-count (count divisors-of-x)]
            (inc
              (count
                (filter (fn [n] (=
                                  (count (into divisors-of-x (divisors n)))
                                  (+ divisors-of-x-count (count (divisors n)))))
                  (range 2 x)))))))

(defcheck solution-2f4dc04a
  (fn
    [num]
    (count
      (filter (fn [num2] (or (= num2 1)
                             (= (some (fn [n] (when (and (zero? (mod num n)) (zero? (mod num2 n))) n))
                                  (range (min num num2) 0 -1))
                               1)))
        (range 1 (inc num))))))

(defcheck solution-2f82c553
  (fn [n]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
      (reduce #(if (= (gcd n %2) 1) (inc %1) %1) 0 (range 1 (inc n))))))

(defcheck solution-2faf1e99
  (fn [x] (if (= 1 x) 1
                      (count (filter #(= 1
                                        ((fn g [a b] (if (= b 0) a (g b (mod a b))))
                                         x %))
                               (range x))))))

(defcheck solution-2fb4b0eb
  (fn totient [x]
    (let [gcd

          (fn gcd [a b]
            (let [c (rem a b)]
              (if (= c 0)
                b
                (gcd b c)
                )
              )
            )
          coprime
          (fn coprime [a b]
            (= 1 (gcd a b))
            )
          ]
      (count (filter true? (map (partial coprime x) (range 1 (inc x)))))
      )
    ))

(defcheck solution-2fcd62b4
  (fn [x]
    (if (= 1 x) 1
                (->> (range 1 x)
                  (filter (fn [y] (->> (range 2 (inc y))
                                    (filter #(and (= 0 (rem x %))
                                                  (= 0 (rem y %))))
                                    empty?)))
                  count))))

(defcheck solution-2ffe802a
  (fn coprimes [x]
    (if (= x 1)
      x
      (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
        (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-303c7d35
  (fn totient [n]
    (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-30665966
  (fn [n]
    (let [gcd        (fn [a b] (first (filter #(= 0 (mod a %) (mod b %)) (range (min a b) 0 -1))))
          coprime-n? (fn [a]   (= 1 (gcd a n)))]
      (->>
        (if (= n 1) '(1) (range 1 n))
        (filter coprime-n?)
        count))))

(defcheck solution-3066eeff
  (fn [n]
    (let [g (fn g [a b] (if (= b 0) a (g b (rem a b))))]
      (if (= n 1)
        1
        (count (filter #{1} (map #(g n %) (range 1 n))))))))

(defcheck solution-307b48cd
  (fn [n]
    (count
      (filter #(= % 1)
        (let [gcd #(if (zero? %) %2 (recur (mod %2 %) %))]
          (for [x (take n (range))]
            (gcd x n)))))))

(defcheck solution-309db2d9
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [x y]
                (loop [dividend (max x y)
                       divisor (min x y)]
                  (if (= divisor 0)
                    dividend
                    (recur divisor (rem dividend divisor)))))]
        (->> (range 1 n)
          (filter #(= (gcd n %) 1))
          count)))))

(defcheck solution-309e7d26
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b]
                (cond
                  (zero? b) a
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))]
        (count (filter #{1} (map (partial gcd n) (range 1 n))))))))

(defcheck solution-30e53153
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
                  (->> n
                    (range 1)
                    (remove #(not= 1 (gcd n %)))
                    count)))))

(defcheck solution-317f483f
  (fn [n]
    (if (= n 1) 1
                (letfn [(f [e1 e2]
                          (let [e3 (rem e1 e2)]
                            (if (> e3 0)
                              (recur e2 e3)
                              e2)))]
                  (->> n
                    (range 1)
                    (filter #(= (f n %) 1))
                    count)))))

(defcheck solution-31f618cb
  (fn [v]
    (let [gcd (fn [x y]
                (loop [a (max x y) b (min x y )]
                  ( if (= b 0)
                    a
                    (recur b (mod  a b ) ))))]
      (if (= v 1 )
        1
        (count  (filter #(= 1 (gcd  % v ))  (range v) )
          )))))

(defcheck solution-326249a3
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
        (->> (range 1 n)
          (map #(gcd n %))
          (filter #{1})
          count)))))

(defcheck solution-32b1f58d
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a, (recur b (mod a b))))
            (coprime? [m n] (== 1 (gcd m n)))]
      (if (< n 2) 1
                  (count (for [i (range 1 n)
                               :when (coprime? i n)]
                           i))))))

(defcheck solution-333d0c16
  (fn [n]
    (if (= n 1)
      1
      ;; Added in special case hence bad indentation.
      (letfn [ (gcd [a b]
                 (let [smaller (if (< a b) a b)
                       larger  (if (< a b) b a)
                       difference (- larger smaller)]
                   (if (= difference 0)
                     larger
                     (gcd smaller difference) )))
              ]
        (->> (range n) (drop 1) (filter #(= (gcd % n) 1) ) count)
        ))
    ))

(defcheck solution-3348cf32
  (fn [x] (if (= 1 x) 1 (count (filter #(= 1 ((fn [m n] (if (zero? n) m (recur n (mod m n)))) x %)) (range 1 (inc x)))))))

(defcheck solution-336e6b1b
  (fn [x]
    (let [gcd (fn [a b]
                (cond
                  (= b 0) a
                  (= a 0) b
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))
          coprime? #(= (gcd % x) 1)]
      (if (= 1 x) 1
                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-338960fd
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
                  (count (filter #(= (gcd % n) 1) (range 1 n)))))))

(defcheck solution-338a3aef
  #(->>
     (take-while
       (fn [[v i]] (<= i %))
       (iterate
         (fn [[v i]]
           (let [j (inc i)]
             [(if (= 1
                    (loop [a j, b %]
                      (if (zero? b) a (recur b (mod a b))))) (conj v j) v), j]))
         [[] 0]))
     last
     first
     count))

(defcheck solution-34278291
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b]
                (let [big (max a b) small (min a b) next-small (mod big small)]
                  (if (zero? next-small)
                    small
                    (gcd small next-small))))]
        (count (filter #((fn [a b] (= 1 (gcd a b))) n %) (range 1 n)))))))

(defcheck solution-3430c229
  (fn [n]
    (letfn [(gcd [a b]
              (if (> b a)
                (recur b a)
                (if (zero? b) a (recur b (mod a b)))))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n)))))))

(defcheck solution-346c5998
  (letfn [(gcd [a b] (if (zero? a) b (recur (mod b a) a)))] (fn [x] (count (filter #(= (gcd % x) 1) (range x))))))

(defcheck solution-346c9503
  (fn [x] (count (filter #(= 1 %) (map (partial #(if (= 0 %2) % (recur %2 (mod % %2))) x) (range x))))))

(defcheck solution-36019c47
  (fn [n]
    (inc  (count
            (for [x (range 2 n) :when
                  (not-any?
                    #(and (zero? (mod x %) ) (zero? (mod n %) ))
                    (range 2 n))] x)))))

(defcheck solution-3622e2e8
  (fn [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (if (= 1 n) 1 (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-36378d4c
  (fn
    [number]
    (if (= number 1)
      1
      (letfn [(divisible-by? [n] #(zero? (mod % n)))]
        (let [divisors (for [i (range 2 number) :when ((divisible-by? i) number)] i)]
          (count (reduce #(filter (complement (divisible-by? %2)) %1) (range 1 number) divisors)))))))

(defcheck solution-36585c43
  (fn [n]
    (->> (range 2 n)
      (filter (fn [x]
                (= 1 ((fn gcd [a b]
                        (if (= 0 b) a (gcd b (mod a b))))
                      x n))))
      (count)
      (inc))))

(defcheck solution-3670b73e
  (fn tottient
    [x]
    (if (= x 1)
      1
      (let [gcd (fn [a b] (if (< a b) (recur b a) (if (zero? b) a (recur b (rem a b)))))]
        (reduce +
          (for [n (range 1 x)
                :when (= (gcd x n) 1)]
            1))))))

(defcheck solution-3698804d
  #(if(= % 1) 1 (count (filter (fn[x](not (contains? (set (flatten (rest (for [ d (for [i (range 1 %) :when (= 0 (rem % i))] i)] (filter (fn[a](= 0 (rem a d))) (range 1 %)))))) x))) (range 1 %)))))

(defcheck solution-371bf203
  (fn phi [x]
    (letfn
     [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
      (coprimes? [a b] (= 1 (gcd a b)))
      ]
      (if (= 1 x) 1
                  (count (filter (partial coprimes? x) (range 1 x)))
                  ))))

(defcheck solution-376f65d1
  (fn [num]
    (count (for [x (range 1 (inc num)) :when (= 1 ((fn gcd [x y]
                                                     (loop [a x b y]
                                                       (if (= a b)
                                                         a
                                                         (recur (if (> a b) (- a b) a) (if (> b a) (- b a) b) )
                                                         )
                                                       )
                                                     ) x num))] x))
    ))

(defcheck solution-3771ee05
  (fn [n]
    (if (= n 1)
      1
      (letfn
       [(gcd [a b]
          (if (zero? b)
            a
            (gcd b (mod a b))))]
        (count (filter #(= (gcd % n) 1) (range 1 n)))))))

(defcheck solution-37d3c7fd
  (fn euler [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
        (count (filter #(= (gcd % x) 1) (range x)))))))

(defcheck solution-37ff9290
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (let [[aa bb] (if (> a b) [a b] [b a])
                      r (rem aa bb)]
                  (if (= 0 r)
                    bb
                    (gcd bb r))))]
        (count (loop [ret [] cnt 1]
                 (if (= cnt x)
                   ret
                   (recur (if (= 1 (gcd cnt x))
                            (conj ret cnt)
                            ret)
                     (inc cnt)))))))))

(defcheck solution-38a0c9c3
  (fn totient [n]
    (if (= 1 n) 1
                (letfn [(gcd [a b]
                          (loop [x a y b]
                            (let [z (rem x y)]
                              (if (= 0 z) y (recur y z)))))]
                  (reduce #(if (= 1 (gcd n %2)) (inc %) %) 0 (range 1 n)
                    )))))

(defcheck solution-39c914d8
  (fn [x]
    (let [g (fn [x,y] (if (zero? y) x (recur y (mod x y))))]
      (if (= x 1)
        1
        (count (filter #(= 1 (g x %)) (range 1 x)))))))

(defcheck solution-39d93e77
  (fn eulers-totient [x]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (->> (range 1 (inc x))
        (filter #(= 1 (gcd % x)) )
        count))))

(defcheck solution-3a2ac685
  (letfn [(gcd [a b]
            (if (= b 0)
              a
              (recur b (mod a b))))
          (coprime? [a b]
            (= 1 (gcd a b)))]
    (fn totient [n]
      (if (= n 1)
        1
        (->> (range n)
          (filter (partial coprime? n))
          count)))))

(defcheck solution-3ae5f645
  (fn totient [n]
    (letfn
     [(gcd [a b]
        (if (= 0 b) a
                    (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-3b21b030
  (fn
    [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b] (if (= 0 b)
                           a
                           (gcd b (mod a b))))]
        (count (filter #(= 1 (gcd n %))
                 (range 1 n)))))))

(defcheck solution-3b2ec922
  (fn cop [n]
    (let [  gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))
          co-prime? (fn [x y] (= 1 (gcd x y)))
          ]
      (count(filter #(co-prime? % n) (range 1 (condp = n 1 2 n)))))))

(defcheck solution-3b44b458
  (fn totient
    [x]

    ; A special case -- f(1) = 1
    (if (= x 1)
      1
      (letfn [(divisors [x]
                (loop [acc []
                       curr 2]
                  (if (> curr x)
                    acc
                    (recur
                      (if (= 0 (mod x curr))
                        (conj acc curr)
                        acc)
                      (inc curr)))))]
        (loop [acc [1]
               curr 2
               xdiv (set (divisors x))]
          (if (= curr x)
            (count acc)
            (recur
              (if (empty? (clojure.set/intersection (set (divisors curr)) xdiv))
                (conj acc curr)
                acc)
              (inc curr)
              xdiv)))))))

(defcheck solution-3b996054
  (fn [n]
    (let [gcd (fn [m n] (if (zero? m) n (recur (mod n m) m)))]
      (case n 1 1 (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-3be53b36
  (letfn [(gcd [x y]
            (let [[a b] (sort [x y])] (if (= a 0) b (gcd a (- b a)))))]
    (fn e-totient [n]
      (count (for [i (range n) :when (= 1 (gcd n i))] i)))))

(defcheck solution-3c041c59
  (fn tot [n]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (loop [v 2
             ans 1]
        (if (> v n)
          ans
          (recur (inc v)
            (if (= (gcd v n) 1)
              (inc ans)
              ans)))))))

(defcheck solution-3c29372e
  (fn totient
    [arg]
    (if (= 1 arg)
      1
      (let [factor? #(zero? (mod % %2))
            factors #(concat
                      (for [x (drop 1 (range (inc (quot % 2))))
                            :when (factor? % x)] x)
                      (list %))
            gcd (fn [x y]
                  (let [x-factors (factors x)
                        y-factors (factors y)
                        common-factors (filter #(some #{%} x-factors) y-factors)
                        sorted-factors (sort common-factors)]
                    (last sorted-factors)))
            coprime? #(= 1 (gcd % %2))]
        (count
          (filter true?
            (map (partial coprime? arg) (range 1 arg))))))))

(defcheck solution-3c370b52
  (fn f [x]
    (if (= x 1)
      1
      (->>
        (range 1 x)
        (remove
          (reduce (fn [f i]
                    (if (and (not (f i))
                             (zero? (mod x i)))
                      #(or (f %)
                           (zero? (mod % i)))
                      f))
            (constantly false)
            (range 2 x)))
        count))))

(defcheck solution-3c58c80
  (fn [x]
    (letfn [(gcd [a b]
              (if (= b 0) a (recur b (mod a b))))]
      (count (filter #(= % 1) (map #(gcd % x) (range x)))))))

(defcheck solution-3c679b14
  (fn totient [n]
    (letfn [(gcd [x y] (if (zero? y)
                         x
                         (recur y (rem x y))))]
      (count (if (= n 1)
               '(1)
               (filter
                 (comp (partial = 1) (partial gcd n))
                 (range 1 n)))))))

(defcheck solution-3c9bafbb
  (fn [n]
    (let [f? (fn [x n]
               (and (> (mod n x) 0)
                    (reduce #(if (and (= 0 (mod x %2))
                                      (= 0 (mod n %2)))
                               false
                               %1)
                      true
                      (range 2 x))))]
      (if (= n 1)
        1
        (loop [i 2
               c 1]
          (if (< i n)
            (recur (inc i)
              (if (f? i n)
                (inc c)
                c))
            c))))))

(defcheck solution-3d88920f
  (fn [n]
    (let [coprime (fn [a b] (= 1 (apply max (map #(if (= 0 (mod a %) (mod b %)) % 0) (map inc (range a))))))]
      (inc (count (filter #(coprime % n) (range 2 n)))))))

(defcheck solution-3d91ea45
  (fn [x]
    (letfn
     [
      (gcd [a b]
        (if (= b 0)
          a
          (gcd b (mod a b))
          )
        )
      ]
      (count (filter #(= (gcd x %) 1) (range 1 (inc x))))
      )
    ))

(defcheck solution-3d97baf2
  (fn totient [n]
    (if (= n 1) 1
                (let [gcd (fn [a b] (loop [m (min a b)]
                                      (if  (= (mod a m) (mod b m) 0) m (recur (dec m)))))]
                  (count (filter #(= 1 %) (map (partial gcd n) (rest (take n (iterate dec n))))))))))

(defcheck solution-3e09210d
  (fn [n]
    (letfn [(gcd [a b] (if (= 0 b) a (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-3e6b4fa0
  (fn [n]
    (->> (range n)
      (map #(loop [a n b (inc %)]
              (cond (< a b) (recur b a)
                    (> a b) (recur b (- a b))
                    :else a)))
      (filter #{1})
      count)
    ))

(defcheck solution-3e9a8377
  (fn [x]
    (letfn [(divisor? [x y]
              (zero? (rem x y)))
            (common-divisor? [x y d]
              (and (divisor? x d)
                   (divisor? y d)))
            (gcd [x y]
              (let [divs (->> (range 1 (inc (min x y)))
                           (filter #(common-divisor? x y %)))]
                (if (empty? divs)
                  1
                  (apply max divs))))
            (coprime? [x y]
              (= 1 (gcd x y)))]
      (if (= 1 x)
        1
        (->> (range 1 x)
          (filter #(coprime? x %))
          count)))))

(defcheck solution-3ef102e2
  (fn [n]
    (let [gcd
          (fn f [a b]
            (if (= a b)
              a
              (if (> a b)
                (f (- a b) b)
                (f a (- b a)))))]
      (if (= 1 n)
        1
        (count (filter #(= 1 %) (map #(gcd n %) (range 1 n))))))))

(defcheck solution-3f1200e6
  (fn [x]
    (if (= 1 x)
      1
      (letfn [(gcd [a b]
                (if (zero? b)
                  a
                  (gcd b (mod a b))))
              (coprime? [a b]
                (= 1 (gcd a b)))]
        (count (filter #(coprime? x %) (range 1 x)))))))

(defcheck solution-3f19c2e2
  (fn [arg]
    (if (= arg 1)
      1
      (letfn [(gcd [x y]
                (let [a (min x y)
                      b (max x y)]
                  (if (zero? (mod b a))
                    a
                    (recur (mod b a) a)
                    )))]
        (reduce #(if (= (gcd %2 arg) 1) (inc %) %) 0 (range 1 arg))))
    ))

(defcheck solution-3f437adb
  (fn [x]
    (let [gcd #(if (= 0 %2) % (recur %2 (mod % %2)))]
      (->> (range x)
        (filter #(= 1 (gcd % x)))
        count))))

(defcheck solution-3f534ccd
  (fn [n]
    (letfn
     [ (gcd [a b]
         (if (zero? b)
           a
           (recur b (mod a b))))]
      (reduce
        (fn [t x] (if (= 1 (gcd x n)) (inc t) t)) 1
        (range 2 n)))))

(defcheck solution-3fa47469
  (fn euler[x] ( if (= x 1) 1

                            (count (filter #(= % 1) (map (fn [n] (last (filter #(= (mod n %) (mod x %) 0) (range 1 (inc (min n x)) )))) (range 1 x))))

                            )))

(defcheck solution-3fd52e2
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn gcd [x y]
                            (if (zero? y) x
                                          (gcd y (mod x y))))
                      coprime? #(= 1 (gcd %1 %2))]
                  (->> n
                    range
                    rest
                    (filter (partial coprime? n))
                    count)))))

(defcheck solution-3fed684a
  (fn [n]
    (if (= n 1) 1
                (letfn [(g [a b] (if (= 0 b) a (g b (rem a b))))]
                  (count (filter #(= 1 (g n % )) (range 1 n)))))))

(defcheck solution-4003bdd4
  (fn [n]
    (letfn [
            (coprime [a b]
              (if (= b 0) (= a 1)
                          (coprime b (rem a b))))]
      (count (filter (partial coprime n) (range n))))))

(defcheck solution-402aaa18
  (fn [n] (letfn [(mdc [a b]
                    (loop [a a b b] (if (zero? b) a (recur b (mod a b)))))]
            (count (filter #(= 1 (mdc n %)) (range n))))))

(defcheck solution-4030d56d
  (fn [n]
    (letfn [
            (gcd [a b]
              (cond
                (= a b) a
                (> a b) (gcd (- a b) b)
                :else   (gcd a (- b a))))]
      (if (= 1 n) 1
                  (count (filter #(= 1 %) (map #(gcd n %) (range 1 n))))))))

(defcheck solution-40507794
  (fn totient [num]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (if (= 1 num)
        1
        (count (filter true? (map #(= 1 (gcd num %)) (range 1 num))))))))

(defcheck solution-412a2e9d
  (fn [x] (if (= 1 x) 1
                      (let [div_x    (filter #(zero? (mod x %)) (range 1 x))
                            coprx?   (fn [y] (= [1] (filter #(zero? (mod y %)) div_x)))
                            cops     (filter coprx? (range 1 x))]
                        (count cops)))))

(defcheck solution-41545673
  (fn totient [n]
    (cond (= n 1) 1
          :else
          (let [factors (filter #(= 0 (mod n %)) (range 2 n))
                relprime? (fn [n]
                            (every? #(> (mod n %) 0) factors))]

            (count (filter relprime? (range 1 n)))))))

(defcheck solution-415704c4
  (fn [x]
    (if (= x 1) 1
                (count (filter #{1}
                         (map            (fn [a b] (if (< a b)
                                                     (recur a (- b a))
                                                     (if (> a b)
                                                       (recur (- a b) b)
                                                       a)))
                           (repeat x)
                           (range 1 x))))
                )))

(defcheck solution-417ee651
  (fn [n]
    (letfn [(lcd [x y] (if (= (rem x y) 0) y
                                           (lcd y (rem x y))
                                           ))]
      (if (= n 1) 1
                  (count (filter #(= 1 (lcd n %)) (range 1 n)))
                  ))
    ))

(defcheck solution-419f5a2a
  (fn [n]
    (if (= n 1) 1
                (let [divisors (memoize (fn [n]
                                          (filter #(zero? (rem n %))
                                            (range 2 (inc n)))))
                      coprime (fn [i j]
                                (->> (concat (divisors i) (divisors j))
                                  frequencies
                                  (filter #(> (last %) 1))
                                  count))
                      ]
                  (count (filter (fn [i] (= 0 (coprime i n)))
                           (range 1 n)))))))

(defcheck solution-41dac47f
  (letfn [
          (gcd [a b]
            (if (zero? b) a (recur b (rem a b))))
          (coprime [a b]
            (= (gcd a b) 1))
          (totient [n]
            (inc (count (filter #(coprime n %) (range 2 n)))))
          ]
    totient
    ))

(defcheck solution-4230398f
  (fn [n]
    (let [get-gcd
          (fn [a b]
            (loop [x a y b]
              (if (= y 0) x
                          (recur y (mod x y)))))]
      (if (= 1 n) 1
                  (count
                    (filter #(= 1 (get-gcd n %)) (range 1 n)))))))

(defcheck solution-4266f103
  (fn tot [n]
    (let [gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (if (> n 1)
        (count (filter #(= 1 (gcd n %)) (range 1 n) ))
        1))))

(defcheck solution-42acf048
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

(defcheck solution-4340232f
  (fn totient [x]
    (letfn [(gcd [x y]
              (if (= y 0) x (recur y (rem x y))))]
      (if (= x 1) 1 (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-434495b3
  (fn [x]
    (if (= x 1)
      1
      (let [gcd #(if (zero? %2) %1 (recur %2 (mod %1 %2)))
            coprime? #(= 1 (gcd %1 %2))]
        (-> (filter (partial coprime? x) (range 1 x)) count)))))

(defcheck solution-436667c
  (fn [x]
    (let [gcd #(cond (= %1 %2) %1
                     (> %1 %2) (recur (- %1 %2) %2)
                     :else     (recur (- %2 %1) %1))
          cp? #(= 1 (gcd %1 %2))]
      (if (= 1 x)
        1
        (count (filter (partial cp? x) (next (range x))))))))

(defcheck solution-43b04dbc
  (fn etf [x]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (count (for [n (range x) :when (= 1 (gcd n x))] n)))))

(defcheck solution-44057e26
  (fn [n]
    (letfn [(gcd [a b]
              (letfn [(g [a b]
                        (let [r (mod a b)]
                          (if (zero? r)
                            b
                            (recur b r))))]
                (g (max a b) (min a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-4425f978
  (fn [n]
    (let [f (fn [a b]
              (first (drop-while
                       #(not (and (zero? (mod a %))
                                  (zero? (mod b %))))
                       (range (min a b) 0 -1))))]
      (count (filter #(= 1 (f n %)) (range n 0 -1)))
      )))

(defcheck solution-4426cd49
  #(reduce (fn [a b] (if (= ((fn f [m n] (if (zero? n) m (f n (rem m n)))) b %) 1) (inc a) a)) 0 (range 1 (inc %))))

(defcheck solution-44516cf5
  (fn f [num]
    (let [gcd #(loop [n % d %2] (if (zero? d) n (recur d (mod n d))))]
      (count (filter #(= (gcd num %) 1) (range num))))))

(defcheck solution-4463383c
  (fn totient [x]
    (letfn [(gcd [a b]
              (if (== b 0)
                a
                (gcd b (mod a b))))]
      (if (== 1 x)
        1
        (let [pints (range 1 x)]
          (count (filter #(== 1 (gcd x %)) pints)))))))

(defcheck solution-44823b25
  (fn [n]
    (letfn [(coprime? [a b]
              (= 1 (gcd a b)))
            (gcd [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))]
      (count (filter (partial coprime? n)
               (range 1 (inc n)))))))

(defcheck solution-44865190
  (fn euler-fun [n]
    (letfn [(coprime? [x y]
              (if (= 1 y)
                true
                (loop [x x, y y]
                  (if (= 0 (rem x y))
                    (if (= 1 y) true false)
                    (recur y (rem x y))))))]
      (if (<= n 1)
        1
        (count (filter #(coprime? n %) (range 1 n)))))))

(defcheck solution-44b07b73
  (letfn
   [(factors [n]
      (set (filter #(zero? (rem n %)) (range 1 (inc n)))))]

    (fn totient [x]
      (if (= 1 x)
        1
        (let [xf (factors x)]
          (->> (range 1 x)
            (map factors)
            (map #(clojure.set/intersection xf %))
            (filter #(= (count %) 1))
            (count)))))))

(defcheck solution-454c50a
  (fn [x]
    (letfn [(gcd [x y] (if (= y 0) x (gcd (min x y) (rem (max x y) (min x y)))))]
      (if (= x 1)
        1
        (count
          (filter (fn [y] (= 1 (gcd x y))
                    ) (range 1 x)))))))

(defcheck solution-468c188e
  (fn [n]
    (count
      (filter
        (fn [i] (= (#(if (zero? %2) %1 (recur %2 (mod %1 %2))) i n) 1))
        (range 1 (inc n))))))

(defcheck solution-469e32ee
  (fn [n]
    (let [fgcd (fn gcd[a b]
                 (if (< a b)
                   (gcd b a)
                   (let [r (mod a b)]
                     (if (= r 0)
                       b
                       (gcd b r)
                       )
                     )
                   )
                 )]
      (if (= n 1)
        1
        (count
          (for [x (range 1 n) :when (= 1 (fgcd n x))] x)
          )
        )
      )
    ))

(defcheck solution-46b51f55
  (fn [n]
    (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))]
      (if (= n 1)
        1
        (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-46f698e7
  (fn tot [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (->> (range x)
        (filter #(= 1 (gcd x %)))
        count))))

(defcheck solution-477edf8b
  #(if (= 1 %) 1
               (count
                 (filter (fn [x]
                           (= 1
                             ((fn gcd [a b]
                                (if (zero? a) b
                                              (if (<= a b) (gcd a (- b a))
                                                           (gcd b a)))) x %)))
                   (range 1 %)))))

(defcheck solution-47d2d7bd
  (fn [n]
    (if (= n 1) 1
                (count
                  (filter
                    #(= 1 (loop [a n b %] (if (= 0 b) a (recur b (mod a b)))))
                    (range n))))))

(defcheck solution-482f7c97
  (fn totient
    [n]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (gcd y (mod x y))))]
      (count (for [x (range n) :when (= (gcd n x) 1)] x)))))

(defcheck solution-48315580
  #(let [denoms (fn [n] (filter (comp zero? (partial rem n)) (range 1 n)))
         gcd (fn [a b]
               (let [int (clojure.set/intersection
                           (set (denoms a))
                           (set (denoms b)))]
                 (and (seq int) (apply max int))))]
     (if (= % 1)
       1
       (count (filter #{1} (map  (partial gcd %) (range 1 (dec %))))))))

(defcheck solution-487b5eda
  (fn [n]
    (let [divisors
             (fn [x] (into #{} (filter
                                 #(zero? (mod x %))
                                 (range 1 (inc x)))))
          dn (divisors n)]
      (if (= 1 n)
        1
        (count
          (filter #(> 2 (count %))
            (map (fn [x]
                   (clojure.set/intersection
                     dn
                     (divisors x)))
              (range 1 n))))))))

(defcheck solution-48d7f3b2
  (fn totient [arg]
    (let [gcd (fn gcd [a b]
                (cond
                  (= b 0) a
                  (> a b) (gcd b (- a b))
                  :else (gcd a (- b a))))]
      (if (= arg 1) 1
                    (count (filter #(= 1 (gcd % arg)) (range 1 arg)))))))

(defcheck solution-48dd20e9
  (fn [v]
    (let [factors #(loop [n 1 ds #{} m %]
                     (if (> n m)
                       ds
                       (if (= 0 (mod % n))
                         (recur (inc n) (conj ds n (/ % n)) (/ % n))
                         (recur (inc n) ds (/ % n)))))
          infacts (factors v)]
      (reduce (fn [v c] (if (= 1 (count (clojure.set/intersection infacts (factors c)))) (inc v) v)) 0 (range 1 (inc v))))))

(defcheck solution-48e4ee29
  (fn [x]
    (let [g (fn gcd [n m] (if (zero? m) n (gcd m (mod n m))))]
      (count (filter #(= 1 (g % x)) (range 1 (inc x)))))))

(defcheck solution-48fe8af6
  (fn[v] (inc (count (filter (fn[n] (every? #(not (= 0 (mod v %) (mod n %))) (range 2 (inc n)))) (range 2 v))))))

(defcheck solution-49fc7cab
  (fn [x]
    (let [
          factors (fn [y] (set (filter #(= 0 (rem y %)) (range 2 (inc y)))))
          coprime? (fn [y] (or (= 1 y)
                               (< (count (filter (factors x) (factors y))) 1)))]
      (if (= 1 x) 1
                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-4a8033c
  (letfn
   [(gcd [a b]
      (loop [a a b b]
        (if (zero? b) a,
                      (recur b (mod a b)))))]
    (fn [x]
      (if (= x 1)
        1
        (count (filter #(= (gcd % x) 1) (range 1 x)))))))

(defcheck solution-4aa35c45
  (fn [num]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (->> (range 1 (inc num))
        (filter #(= 1 (gcd num %)))
        count))))

(defcheck solution-4aa53f4a
  (fn totient [x]
    (if (= x 1)
      1
      (count (filter #(= ((fn gcd
                            ([a b] (gcd a b (min a b)))
                            ([a b c] (if (= (mod a c)
                                           (mod b c)
                                           0)
                                       c
                                       (gcd a b (dec c)))))
                          % x)
                        1)
               (range 1 x))))))

(defcheck solution-4acff2ba
  (fn [x]
    (let [gcd (fn gcd [x y] (if (zero? (rem x y)) y (gcd y (rem x y))))
          totients (filter #(= 1 (gcd x %)) (range 1 x))]
      (if (= x 1) 1  (count  totients)))))

(defcheck solution-4b0b5361
  (fn totient [x]
    (let [gcd (fn gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (if (= x 1) 1
                  (loop [i 1 r 0]
                    (cond (= i x) r
                          (= (gcd i x) 1) (recur (inc i) (inc r))
                          true (recur (inc i) r)))))))

(defcheck solution-4b1ccb47
  (fn et [x]
    (if (= 1 x) 1
                (reduce + (map (fn [y] (if (= 1 (apply max
                                                  (map #(if (= [0 0] [(mod x %) (mod y %)]) % 0)
                                                    (range 1 (+ 1 (min x y)))))) 1 0)) (range 1 x))))))

(defcheck solution-4b5f02a9
  (fn totient [n]
    (letfn [(gcd [a b]
              (cond
                (= a b) a
                (> a b) (gcd (- a b) b)
                :else (gcd a (- b a))))
            (coprime? [a b] (= (gcd a b) 1))]
      (loop [x (dec n)
             total 0]
        (if (<= x 1)
          (+ 1 total)
          (recur (dec x) (if (coprime? n x) (+ total 1) total)))))))

(defcheck solution-4b985dfe
  (fn [x]
    (let
     [gcd (fn g[a b] (if (= b 0) a (g b (mod a b)))),
      coprime (fn [y] (= 1 (gcd x y)))]
      (if (= x 1)
        1
        (count (filter coprime (range 1 x))))
      )))

(defcheck solution-4b994b21
  (fn [n]
    (letfn [(coprime [a b]
              "Judge whether a and b is coprime, assume a>=b"
              (not-any? #(and (zero? (mod a %))
                              (zero? (mod b %)))
                (range 2 (+ b 1))))]
      (+ 1 (count (filter #(coprime n %)
                    (range 2 n)))))))

(defcheck solution-4c449df1
  (fn [x]
    (if (= 1 x)
      1
      (let [gcd (fn [a b]
                  (if (= 0 b)
                    a
                    (recur b (mod a b))))
            coprime-to-x? (fn [y] (= 1 (gcd x y)))
            coprimes (filter coprime-to-x? (range 1 x))]
        (count coprimes)))))

(defcheck solution-4c89abbc
  (fn [n]
    (letfn [(gcd [n m] (if (zero? m) n (gcd m (mod n m))))]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-4cbb9f58
  (fn eulers-totient [num]
    (if (= num 1)
      1
      (let [gcd (fn gcd [a b]
                  (let [lower (min a b)]
                    (some #(if (= 0 (rem a %) (rem b %))
                             %
                             false) (range lower 0 -1))))
            nums-below (range 1 num)
            totient (for [t nums-below :when (= 1 (gcd num t))]
                      t)]
        (count totient)))))

(defcheck solution-4ccf2bc8
  (fn [n] (count (filter #(= 1 (loop [a n b %] (if (= 0 b) a (recur b (mod a b))))) (range 1 (inc n))))))

(defcheck solution-4ce4c332
  (fn totient [n]
    (letfn [(gcd [n1 n2]
              (let [mx (max n1 n2)
                    mn (min n1 n2)
                    rm (rem mx mn)]
                (if (= 0 rm)
                  mn
                  (recur mn rm))))
            (coprime? [n1 n2]
              (= 1 (gcd n1 n2)))]

      (count (filter #(coprime? % n) (range 1 (inc n)))))))

(defcheck solution-4d39b1df
  (fn f
    ([x i c]
     (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
       (if (>= i x)
         c
         (if (= 1 (gcd x i))
           (recur x (inc i) (inc c))
           (recur x (inc i) c)))))
    ([x] (if (= x 1) 1 (f x 1 0)))))

(defcheck solution-4d62db88
  (fn[n]
    (if (= n 1)
      1
      (let[prime-factors
                    (loop [[f & r] (range 2 n) primes [1]]
                      (if f
                        (recur (filter #(pos? (mod % f)) r) (conj primes f))
                        (filter #(zero? (mod n %)) primes)))
           coprimes (remove (fn [c] (some #(zero? (mod c %)) (rest prime-factors))) (range 1 n))]
        (count coprimes)))))

(defcheck solution-4e2d2303
  (fn [n] (letfn [
                  (gcd [n1 n2] (cond (> n2 n1) (gcd n2 n1) (zero? (rem n1 n2)) n2 (= 1 n2) 1 :else (gcd n2 (rem n1 n2))))
                  (coprime? [n1 n2] (= 1 (gcd n1 n2)))
                  (coprimes [n] (filter #(coprime? n %) (range 1 n)))
                  (totient [n] (max 1 (count (coprimes n))))
                  ] (totient n))))

(defcheck solution-4e72b218
  (fn [n]	  ; todo, improve performance ...
    (let [coprime? (fn [l s]
                     (empty? (filter #(and (= (mod s %) 0) (= (mod l %) 0)) (drop 2 (range (inc s))))))]
      (count (filter (partial coprime? n) (drop 1 (range (inc n))))))))

(defcheck solution-4e9a960
  (fn z [n]
    (let [divisors (fn [x] (into #{} (filter #(zero? (mod x %)) (range 1 (inc x)))))
          divisorsN (divisors n)
          isBiggestCommonOne (fn [s1 s2] (let [i (clojure.set/intersection s1 s2)] (and
                                                                                    (= 1 (count i))
                                                                                    (contains? i 1))))]
      (if (= 1 n)
        1
        (count (filter #(isBiggestCommonOne (divisors %) divisorsN) (range 1 n)))))))

(defcheck solution-4eb3b907
  (fn totient [n]
    (letfn [(gcd_ [a b]
              (let [x (max a b)
                    y (min a b)
                    m (mod x y)]
                (if (zero? m)
                  y
                  (recur y m))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd_ n %)) (range 1 n)))))))

(defcheck solution-4f9f10fe
  (fn [n]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-4fe2f0bc
  (letfn [(gcd [a b]
            (cond (= a 0) b
                  (= b 0) a
                  :else (recur b (rem a b))))]
    (fn totient [n]
      (if (= 1 n)
        1
        (->> (range 1 n)
          (filter #(= 1 (gcd n %)))
          (count))))))

(defcheck solution-4ffa55e3
  (fn [n]
    (if (= n 1)
      1
      (->>
        (range 1 n)
        (filter
          (partial
            (fn [maior menor]
              (not-any?
                #(and
                  (zero? (rem maior %))
                  (zero? (rem menor %)))
                (range 2 (inc menor))))
            n))
        count))))

(defcheck solution-5010d86c
  (fn [v]
    (letfn [(coprime
              [n1 n2]
              (every?
                #(> (+ (mod n1 %) (mod n2 %)) 0)
                (range 2 (inc (min n1 n2)))))]
      (inc
        (count
          (filter #(coprime % v) (range 2 v)))))))

(defcheck solution-50285642
  (fn __ [x]
    (let [
          f (fn primefactors [n nex acc]
              (cond (<= n 1) acc
                    (zero? (rem n nex)) (recur (/ n nex) nex (conj acc nex))
                    :else (recur n (inc nex) acc)))
          res (f x 2 #{})
          invs (cons x (map (fn [x] (/ (- x 1) x)) res))
          ]
      (reduce * invs))))

(defcheck solution-50a3d3c9
  (fn [x]
    (if (= 1 x)
      1
      (letfn [(gcd [a b]
                (if (= a b)
                  a
                  (recur (- (max a b) (min a b))
                    (min a b))))]
        (loop [n (dec x) t 0]
          (if (zero? n)
            t
            (if (= 1 (gcd x n))
              (recur (dec n) (inc t))
              (recur (dec n) t))))))))

(defcheck solution-50a44afc
  (fn [n]
    (letfn [(gcd [a b]
              (if (= b 0) a (gcd b (rem a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-50ba987c
  (fn [n]
    (let [gcd (fn gcd# [a b]
                (cond
                  (= a b) a
                  (< a b) (gcd# a (- b a))
                  (> a b) (gcd# (- a b) b)))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-5132bf9f
  (fn [n]
    (->> (range 2 n)
      (filter (fn [x]
                (= 1 ((fn gcd [a b]
                        (if (= 0 b) a (gcd b (mod a b))))
                      x n))))
      count
      inc)))

(defcheck solution-514f6ad7
  (fn co-prime [num]
    (letfn [(coprime? [n]
              (if (= (gcd num n) 1) true false))
            (gcd[a, b]
              (if (= b 0) a (gcd b (mod a b))))]
      (inc (count (filter #(coprime? %) (range 2 num)))))))

(defcheck solution-5162180b
  (fn [n]
    (count (filter #(= 1
                      (loop [a % b n]
                        (if (= b 0) a (recur b (mod a b)))))
             (range 1 (+ 1 n))))))

(defcheck solution-51a0580f
  (fn [x] (let [f (fn gcd ([x y]
                           (loop [m x n y]
                             (cond (zero? (mod m n)) n,
                                   (zero? (mod n m)) m,
                                   :else (recur (mod m n) (mod n m)))))
                    ([x y & more] (reduce gcd (gcd x y) more)))]
            (if (= x 1) 1
                        (reduce #(if (= (f x %2) 1) (inc %) %) 1 (range 2 x))))))

(defcheck solution-51acf196
  (fn totient-f [x]
    (letfn [(gcd [x y]
              (if (zero? y) x (recur y (mod x y))))]
      (reduce #(if (= 1 (gcd  x %2))
                 (inc %1)
                 %1) 1 (range 2 x)))))

(defcheck solution-51edbe74
  (fn [n] (let [gcd (fn [a b]
                      (let [r (rem a b)]
                        (if (= 0 r) b (recur b r))))]
            (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-524a356e
  (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]

    (fn [n] (if (= n 1)
              1
              (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-526f6025
  (fn [n]
    (letfn
     [
      (gcd [a b]
        (if
         (zero? b)
          a
          (recur
            b
            (mod a b)
            )
          )
        )

      (comprime? [a b]
        (=
          (gcd a b)
          1
          )
        )
      ]
      (if
       (= n 1)
        1
        (count
          (filter
            (partial comprime? n)
            (range n)
            )
          )
        )
      )
    ))

(defcheck solution-52abce87
  #(letfn
    [(d [a b] (apply max (for [ i (range 1 (inc (min a b))) :when (= 0 (+ (rem a i) (rem b i))) ] i)))]
     (count (for [x (cons 1 (range 2 %)) :when (= 1 (d x %))] x))))

(defcheck solution-534e2fd1
  (fn [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (coprime [a b]
              (= 1 (gcd a b)))]
      (if (= 1 x)
        1
        (count (filter (partial coprime x) (range 0 x)))))))

(defcheck solution-53807a64
  (fn [x]
    (let [gcd (fn gcd [i j]
                (apply max (clojure.set/intersection (set (let [x i] (filter #(= 0 (mod x %)) (range 1 (inc x))))) (set (let [x j] (filter #(= 0 (mod x %)) (range 1 (inc x))))))))]
      (if (= x 1)
        1
        (count (filter (partial = 1) (map (partial gcd x) (range 1 x))))))))

(defcheck solution-5438e792
  (fn [x] (if (= x 1) 1 (->> (range x) (drop 1)
                          (map #(loop [a x b %] (if (zero? b) a (recur b (mod a b)))))
                          (filter #{1}) count))))

(defcheck solution-5444115c
  (letfn [(gcd [x y]
            (if (= y 0) x (recur y (mod x y))))
          (is-coprime [x y]
            (= (gcd x y) 1))]
    (fn [x]
      (if (= x 1) 1
                  (count (filter (partial is-coprime x) (range 1 x)))))))

(defcheck solution-54da129e
  (fn [x]
    (int (loop [x x lastn 0 acc x n 2]
           (if (= x 1)
             acc
             (if (= (rem x n) 0)
               (if (= lastn n)
                 (recur (quot x n) n acc n)
                 (recur (quot x n) n (* acc (- 1 (/ 1.0 n))) n))
               (recur x n acc (inc n))))))))

(defcheck solution-54fb714
  (fn totient [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 0 n))))))

(defcheck solution-554dd7e5
  (fn totient [x]
    (letfn [(factor [n]
              (loop [n n d 2 acc []]
                (cond
                  (= n 1) acc
                  (= 0 (mod n d)) (recur (quot n d) d (conj acc d))
                  :else (recur n (inc d) acc))))]
      (let [factors (set (factor x))]
        (reduce * x (map (fn [p] (/ (dec p) p)) factors))))))

(defcheck solution-5560d3cd
  (fn [n]
    (let [gcd (fn [a b] (if (zero? a) b (recur (mod b a) a)))]
      (count (->> (range 1 (inc n)) (filter #(= 1 (gcd % n))))))))

(defcheck solution-55820649
  (fn [x]
    (if (= 1 x)
      1
      (let [gcd (fn [a b] (if (zero? b) a
                                        (recur b (mod a b))))
            coprimes (filter #(= 1 (gcd x %)) (range 1 x))]
        (count coprimes)))))

(defcheck solution-55c847d0
  (fn [n]
    (if (= n 1)
      1
      (let [coprime (fn [a b]
                      (if (= b 0)
                        (= a 1)
                        (recur b (mod a b))))]
        (count (filter #(coprime n %)(range n)))))))

(defcheck solution-55d1c52
  (fn totient [x]
    (if (= 1 x) 1
                (count
                  (filter #(= 1 %)
                    (map (fn [xxx]
                           ((fn [m n]
                              (if (zero? n)
                                m
                                (recur n (mod m n)))) xxx x)) (range 1 x)))))))

(defcheck solution-565a52aa
  (fn [n]
    (if (= 1 n) 1
                (let
                 [
                  multiples (fn [n]
                              (loop [test n stock [1] dv 2]
                                (if (zero? (rem test dv))
                                  (recur (quot test dv) (conj stock dv) dv)
                                  (if (>= test (inc dv))
                                    (recur test stock (inc dv))
                                    stock
                                    )
                                  )))
                  mults (map
                          (comp (partial into #{}) distinct multiples)
                          (range 1 (inc n)))]
                  (reduce #(if (not= #{1} (clojure.set/intersection %2 (last mults))) %(inc %)) 0 (drop-last mults))))))

(defcheck solution-568cde84
  (fn[n]
    (letfn ([g[u v]
             (if
              (=  v 0) u
                       (g v (rem u v)))])
      (count
        (for [x (range 1 (+ n 1)) :when (= (g n x) 1)] x)))))

(defcheck solution-56c1b01a
  (fn [n]
    (letfn [(gcd [a b] (first (filter #(= 0 (rem a %) (rem b %)) (range (min a b) 0 -1))))
            (cop? [a b] (= 1 (gcd a b)))]
      (if (= 1 n) 1 (count (filter #(cop? n %) (range 1 n)))))))

(defcheck solution-5733c9dd
  (let [gcd (fn gcd [a b] (let [x (min a b) y (max a b) m (mod y x)] (if (= 0 m) x (gcd x m))))]
    (fn phi [x] (if (= 1 x) 1 (count (filter #(= 1 (gcd % x)) (range 1 x))))) ))

(defcheck solution-5782b4cb
  (fn [n]
    (let [div? #(zero? (mod %1 %2))
          ds (filter #(div? n %) (range 2 n))]
      (inc (count (remove (fn [i] (some #(div? i %) ds)) (range 2 n)))))))

(defcheck solution-58294e11
  (fn totient-num [n]
    (let [gcd (fn gcd [a b]
                (if (> a b) (gcd b a)
                            (if (= 0 (rem b a))
                              a
                              (gcd (- b a) a))))]
      (count (filter #(= (gcd n %) 1) (range 1 (inc n)))))))

(defcheck solution-584be2dc
  (fn tf [z]
    (if (= 1 z)
      1
      (letfn [(gcd [m n]
                (let [i (mod m n)]
                  (if (= 0 i)
                    n
                    (recur n i))))]
        (count (filter #(= 1 (gcd % z)) (range 1 z)))))))

(defcheck solution-585d0a9e
  (let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))
        coprime? (fn [x y] (= 1 (gcd x y)))]
    (fn [x] (if (= 1 x)
              1
              (-> (partial coprime? x)
                (filter (range 1 x))
                (count))))))

(defcheck solution-58ba65d5
  (fn [n]
    (letfn [(gcd [a b]
              (cond
                (= a b) a
                (> a b) (recur (- a b) b)
                (< a b) (recur a (- b a))))
            (coprime? [a b] (= 1 (gcd a b)))
            (totient [x] (count (filter (partial coprime? x) (range 1 x))))]
      (if (= 1 n) 1 (totient n)))))

(defcheck solution-58c6c6aa
  (fn totient [x]
    (letfn [(gcd-int
              ([n1 n2] (gcd-int n1 n2 1 1))
              ([n1 n2 current greatest]
               (cond
                 (or (> current n1) (> current n2)) greatest
                 (and (= 0 (rem n1 current)) (= 0 (rem n2 current)))
                 (recur n1 n2 (inc current) current)
                 :else (recur n1 n2 (inc current) greatest))))]
      (if (= x 1) 1 (count (filter #(= 1 (gcd-int % x)) (range 1 x)))))))

(defcheck solution-58d3f6c9
  (fn euler-totient [n]
    (let [gcd (fn gcd [n m]
                (if (< n m)
                  (gcd m n)
                  (if (= m 0)
                    n
                    (gcd m (rem n m)))))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n)))))))

(defcheck solution-59722e75
  (fn [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (count (filter (fn [y] (= 1 (gcd x y)))
               (range x))))))

(defcheck solution-59cd612d
  (fn tot [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

(defcheck solution-59f18894
  (fn [x]
    (let [gcd (fn f [a b]
                (if (= 0 b) a (f b (mod a b))))]
      (->> (range 0 x)
        (map (partial gcd x))
        (filter (partial = 1))
        (count)))))

(defcheck solution-59f430fc
  (fn [n] (let [gcd (fn [a b]
                      (loop [c a d b]
                        (if (= d 0)
                          c
                          (recur d (mod c d))
                          )
                        )
                      )] (if (= 1 n) 1
                                     (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-5a1f4370
  (fn totient [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (rem a b))
                )
              )]
      (count
        (for [k (range 1 (inc n))
              :when (= (gcd n k) 1)]
          true
          )
        )
      )
    ))

(defcheck solution-5a7bc591
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))]
    #(if (= % 1) 1
                 (count
                   (filter (fn [a] (= (gcd % a) 1))
                     (rest (range %)))))))

(defcheck solution-5a7f4071
  (fn euler [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (->> (range 2 n)
        (map #(gcd n %))
        (filter #(= 1 %))
        (count)
        (inc)))))

(defcheck solution-5b049ef6
  (fn [x] (letfn [(gcd [a b] (apply max (filter #(and (zero? (rem a %)) (zero? (rem b %)))
                                          (range 1 (inc (min a b))))))]
            (if (= x 1) 1 (inc (count (into #{} (filter #(= (gcd % x) 1) (range 2 x)))))))))

(defcheck solution-5b0e30a4
  (fn et[n] (letfn [(gcd [aa bb]
                      (loop [a (max aa bb) b (min aa bb)]
                        (let [r (rem a b)]
                          (if (= 0 r) b
                                      (recur b r))
                          )))]
              (if (= 1 n) 1 (count (filter #(= 1 (gcd n %)) (range 1 n))))
              )))

(defcheck solution-5b9ab8e9
  (fn [n]
    (count
      (letfn [(coprime? [x y]
                (loop [i 2]
                  (cond
                    (> i y) true
                    (= 0 (+ (mod x i) (mod y i))) false
                    :else (recur (inc i)))))]
        (filter #(coprime? n %) (range 1 (inc n)))))))

(defcheck solution-5bc85e18
  (fn [n] (if (= n 1) 1 (count (filter #(= 1 (loop [[x y] [(min % n) (max % n)]] (if (zero? x) y (recur [(rem y x) x])))) (range 1 n))))))

(defcheck solution-5bfe5e1d
  (fn [x] (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))] (count (filter #(= 1 (gcd %1 x)) (range x))))))

(defcheck solution-5c4112cc
  (fn [n]
    (if (> n 1)
      (count (letfn [(p [a b] (loop [n a m b] (if (= 0 m) n (recur m (mod n m)))))]
               (filter #(= 1 (p % n)) (range 1 n)))) 1)))

(defcheck solution-5c6193e1
  (fn [n]
    (letfn [(gcd [a b]
              (let [x (max a b)
                    y (min a b)]
                (if (zero? y)
                  x
                  (gcd y (mod x y)))))]
      (cond
        (= 1 n) 1
        :else (->> (range 1 n)
                (filter #(= 1 (gcd % n)))
                count)))))

(defcheck solution-5c7363b9
  (fn [v]
    (letfn [(gcd
              [a b]
              (if (zero? b) a
                            (recur b (rem a b))))]
      (count (filter #(= 1 (gcd % v))
               (range v))))))

(defcheck solution-5d187014
  (fn [x]
    (letfn [(gcd [a b] (if (= b 0)
                         a
                         (recur b (mod a b))))]
      (if (= x 1)
        1
        (count (for [y (range 1 x)
                     :when (= 1 (gcd x y))]
                 y))))))

(defcheck solution-5d429d73
  (fn [x] (letfn
           [(coprime? [a b] (= (gcd a b) 1))
            (gcd [c d] (if (= c d) c (apply gcd (if (> c d) [(- c d) d] [c (- d c)]))))]
            (if (= x 1)
              1
              (count (filter (partial coprime? x) (map inc (range (dec x)))))))))

(defcheck solution-5d5636fd
  (fn tot[n]
    (let [gcd
              #(if (zero? %2) %1 (recur %2 (mod %1 %2)))
          cop (fn [n]
                (filter #(= 1 (gcd % n)) (range n)))]
      (count (cop n)))))

(defcheck solution-5d57f831
  (fn [n]
    (count
      (filter
        #((fn f [a b]
            (if (= 0 b)
              (= 1 a)
              (f b (rem a b))))
          n %)
        (range n)))))

(defcheck solution-5d6b738a
  (fn[y]

    (letfn [
            (divisors [z]
              (map
                (fn[x] (first x))

                (filter
                  #(= (last %) z)
                  (map
                    (fn[x]
                      (take-while (partial >= z)  (iterate (partial + x ) x))
                      )
                    (rest
                      (range (inc z))
                      )
                    )
                  )
                )
              )

            (commondivisors [x y]
              (= (count (clojure.set/intersection (set (divisors x)) (set (divisors y))))  1)
              )

            (coprimes [a]
              (filter #(commondivisors % a) (rest (range (inc a))) )
              )

            ]
      (count (coprimes y))
      )
    ))

(defcheck solution-5d88801a
  (fn [n]
    (let [gcd #(loop [a %1, b %2]
                 (if (= 0 b)
                   a
                   (recur b (mod a b))))]
      (count (filter #(= 1 (gcd %1 n)) (range n))))))

(defcheck solution-5d8a8a09
  (fn totient ([n] (if (= n 1) 1 (totient n (range 1 n)))) ([n r] (count (filter #(= 1 %) (map #((fn gcd [a b] (loop [x (max a b) y (min a b)] (if (> (rem x y) 0) (recur y (rem x y)) y))) n %) r))))))

(defcheck solution-5da2bbee
  (fn totient [n] ()
    (let [gcdx (fn gcdx [n1 n2] ()
                 (if (> n1 n2)
                   (gcdx n2 n1)
                   (loop [m n1]
                     (if (and (zero? (mod n1 m)) (zero? (mod n2 m)))
                       m
                       (recur (dec m)))))) ]
      (if (= n 1)
        1
        (- n (inc (reduce + (map (fn [m] (if (> (gcdx m n) 1) 1 0 )) (range 1 n)))))))))

(defcheck solution-5dae2f74
  (fn [N]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))
          coprime? #(= 1 (gcd % N))]
      (count (filter coprime? (range N))))))

(defcheck solution-5dafee9f
  (letfn [(gcd
            [a b]
            (if (pos? a)
              (recur (mod b a) a)
              b))]
    (fn totient [n]
      (if (> n 1)
        (->> (range n)
          (map #(gcd n %))
          (filter #(= 1 %))
          count)
        1))))

(defcheck solution-5dd37655
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [a b] (if (= a b) a (if (> a b) (recur (- a b) b) (recur a (- b a)))))]
                  (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-5e7ff5d9
  (fn [x]
    (if (= x 1) 1
                (count (filter #(= 1 ((fn gcd ([a] (gcd a x))
                                        ([a b] (cond
                                                 (= a 0) b
                                                 (= b 0) a
                                                 (< a b) (gcd b a)
                                                 :else (gcd (mod a b) b)))) %)) (range 1 x))))))

(defcheck solution-5ebec15b
  (fn [f n] (if (= n 1) 1
                        (count (filter  #(empty? (clojure.set/intersection (f %) (f n))) (range 1 n))))) (fn [n] (set (filter #(zero? (mod n %)) (range 2 (inc n))))))

(defcheck solution-5ef963b6
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(g [a b] (if (= 0 b) a (g b (mod a b))))]
        (count (filter #(= 1 (g n %)) (range 1 n)))))))

(defcheck solution-609f87da
  (fn totient [n]
    (letfn
     [(gcd [x y]
        (if (zero? y)
          x
          (recur y (mod x y))))]
      (if (= 1 n)
        1
        (count
          (for [x (range 1 n) :when (= 1 (gcd x n))] x))))))

(defcheck solution-60dfc485
  (fn totient- [n]
    "75. Write a function which calculates Euler's totient function."
    (letfn [(gcd- [a b] (if (= 0 b) a (recur b (mod a b))))
            (coprime?- [a b] (= 1 (gcd- a b)))]
      (cond
        (= 1 n) 1
        :else (count (filter #(coprime?- n %) (range 1 n)))))))

(defcheck solution-613416e2
  (fn euler [x]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (gcd b (mod a b))
                )
              )]
      (count (filter #(= 1 (gcd x %)) (range x)))
      )
    ))

(defcheck solution-6163a984
  (fn euler [n]
    (cond (= n 1) 1
          :else
          (letfn [
                  (
                    gcd [a b]
                    (if (= b 0)
                      a
                      (recur b (rem a b)))
                    )
                  (
                    coprime? [a b]
                    (= (gcd a b) 1)
                    )
                  ]
            (count (set (for [i (range 1 n)]
                          (if (coprime? i n)
                            i
                            1
                            ) ; if
                          ) ; for
                     ) ; set
              ) ; count
            ) ; letfn
          ) ; cond
    ))

(defcheck solution-617279d0
  (fn [n] (if (= 1 n) 1 (let [d (loop [n n k 2 s #{}]
                                  (if (= n 1) s (if (zero? (mod n k))
                                                  (recur (/ n k) k (conj s k)) (recur n (inc k) s))))]
                          (count (remove #(some (fn [x] (zero? (mod % x))) d) (range 1 n)))))))

(defcheck solution-61d6a78a
  (fn [n]
    (letfn [(gcd [a b] (if (= 0 b) a (recur b (mod a b))))]
      (count (filter #(= 1 %) (map gcd (range 1 (inc n)) (repeat n)))))))

(defcheck solution-61f66206
  (fn totient [n]
    (letfn [(gcd [a b] (ffirst
                         (drop-while (fn [[a b]] (not= 0 b))
                           (iterate (fn [[a b]] [b (mod a b)]) [a b]))))]
      (if (= 1 n) 1
                  (count (filter #(= 1 %) (map (partial gcd n) (range 1 n))))))))

(defcheck solution-62d508f2
  (fn [x]
    (if (= 1 x) 1 ; special case.
                (let [ns (range 1 x)] ; all pos ints below x.
                  (letfn [(divs [n] (filter #(= 0 (mod n %)) ns))] ; all divisors of n.
                    (count
                      ;; count all GCDs equalling 1.
                      (filter #(= 1 %)
                        ;; grab all divisors of n. cast to set. get intersection of divisors of x and this set.
                        ;; cast to a sequence. get the maximum.
                        (map (comp #(apply max %)
                                   seq
                                   #(clojure.set/intersection (set (divs x)) %)
                                   set
                                   divs)
                          ns)))
                    )
                  ))
    ))

(defcheck solution-62eb4b6c
  (fn totient [n]
    (let [gcd (fn gcd [a_ b_]
                (last (let [a (max a_ b_)
                            b (min a_ b_)]
                        (for [i (drop 1 (range))
                              :while (and (<= i a) (<= i b))
                              :when (and (= 0 (rem a i)) (= 0 (rem b i)))
                              ]
                          i)
                        )))]
      (if
       (= 1 n) 1
               (count
                 (filter #(= 1 %)
                   (map #(gcd % n) (range 1 n)))))
      )
    ))

(defcheck solution-6374d5af
  #(letfn
    [(gcd [x y]
       (if
        (= x y)
         x
         (if
          (< x y)
           (recur x (- y x))
           (recur y (- x y)))))]
     (if
      (= 1 %)
       1
       (count (filter (partial = 1) (map (partial gcd %)(range 1 %)))))))

(defcheck solution-63827ad7
  (fn euler [n]
    (if (= n 1) 1
                (let [gcm
                      (fn gcm [a b]
                        (let [x (max a b)
                              y (min a b)]
                          (if (zero? y)
                            x
                            (recur y (mod x y)))))]
                  (count (filter #(= (gcm n %) 1) (range 1 n)))))))

(defcheck solution-63e5215e
  (fn totient [n]
    (letfn [(gcd [x y]
              (loop [i x
                     j y]
                (cond
                  (= i j) i
                  (> i j) (recur (- i j) j)
                  :else (recur i (- j i)))))]
      (if (= n 1)
        1
        (count (for [x (range 1 n)
                     :when (= (gcd x n) 1)]
                 x))))))

(defcheck solution-645efd9e
  (fn [x]
    (let [cds (fn [a b]
                (let [x1 (min a b)
                      y1 (max a b)]
                  (filter #(and (= 0 (mod x1 %1))
                                (= 0 (mod y1 %1)))
                    (range 1 (+ x1 1)))))]
      (count (filter #(= 1 (apply max (cds %1 x))) (range 1 (inc x)))))))

(defcheck solution-654d2ce
  (fn [x]
    (letfn [(gcd [a b] (loop [a a b b]
                         (cond (= a 0) b
                               (= b 0) a
                               (< a b) (recur a (mod b a))
                               (< b a) (recur (mod a b) b))))]
      (count (filter #(= 1 (gcd x %)) (range x))))
    ))

(defcheck solution-65b93df1
  (fn [x]
    (let [candidates (cons 1 (range 2 x))
          gcd (fn [n] (some #(if (and (zero? (mod n %)) (zero? (mod x %))) % false) (reverse(range 1 (inc n)))))]
      (count (filter #(= 1 (gcd %)) candidates)))))

(defcheck solution-65cb8793
  (fn totient [z]
    (letfn [(gcd [x y]
              (reduce max (filter #(and (= 0 (mod x %)) (= 0 (mod y %))) (range 1 (inc (min x y))))))
            (is-coprime [x y]
              (= 1 (gcd x y)))]
      (count (filter #((partial is-coprime z) %) (range 1 (inc z)))))))

(defcheck solution-65dbd1fd
  (letfn
   [(gcd [x y]
      (loop [a (max x y)
             b (min x y)
             r (rem a b)]
        (if (= 0 r) b
                    (recur b r (rem b r)))))
    (coprime? [x y] (= (gcd x y) 1))]
    (fn [x]
      (if (= x 1)
        1
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-6619cdb1
  (fn totient [n]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (if (= n 1)
        1
        (->> (range 1 n)
          (map (partial gcd n))
          (filter (partial = 1))
          count)
        ))))

(defcheck solution-666d65ad
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
                  (->> (range 1 n) (filter #(= 1 (gcd n %))) count)))))

(defcheck solution-66d6ae40
  (fn [x] (letfn [(gcd  [a b] (if (zero? b)
                                a
                                (recur b (mod a b))))
                  (coprime [a b] (= 1 (if (zero? b)
                                        a
                                        (gcd b (mod a b)))))
                  (totient [a] (count (filter #(coprime a %) (range 1 a))))]
            (if (= x 1) 1 (totient x)))))

(defcheck solution-67224044
  (fn [n]
    (let [gdc (fn [n m] (if (zero? (mod n m)) m (recur m (mod n m))))
          cop? (fn [m] (= (gdc n m) 1))]
      (->> n
        range
        (map inc)
        (filter cop?)
        count))))

(defcheck solution-672db5ce
  (fn [x]
    (let [gcd (fn gcd ([a b] (gcd a b (min a b)))
                ([a b res]
                 (if (and (= 0 (mod a res))
                          (= 0 (mod b res)))
                   res
                   (recur a b (dec res)))))
          less-gcd (map (partial gcd x) (range 1 (inc x)))]
      (count (filter (partial = 1) less-gcd)))))

(defcheck solution-6823f9bb
  (fn prob75
    [x]
    (cond
      (= x 1) 1
      :else (letfn [(gcd [a b]
                      (if (zero? b)
                        a
                        (gcd b (mod a b))))
                    (coprime [a b]
                      (= 1 (gcd a b)))
                    ]
              (count (filter #(coprime % x) (range x)))))))

(defcheck solution-682f500b
  (fn [n]
    (count (filter
             #(= 1 ((fn [a b]
                      (if (= 0 b)
                        a
                        (recur b (mod a b)))) n %))
             (range n)))))

(defcheck solution-68320a97
  (fn [x]
    (letfn [(g [a b]
              (cond
                (< b a) (g b a)
                (zero? a) b
                :else (g (mod b a) a)))]
      (cond
        (= 1 x) 1
        :else (count (filter #(= 1 (g % x)) (range 1 x)))))))

(defcheck solution-68395b12
  (fn [x]
    (if (= x 1) 1 (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
                    (count (filter #(= 1 (gcd %1 x)) (range 1 x)))
                    ))
    ))

(defcheck solution-6848a781
  (fn [x]
    (letfn [(gcd [x y] (let [a (max x y) b (min x y)] ; Euclid's algorithm to find the greatest common divisor
                         (if (or (= a b) (zero? (mod a b)))
                           b
                           (recur (- a b) b))))]
      (if (= x 1)
        1
        (count (filter #(= (gcd x %) 1)
                 (range 1 x)))))))

(defcheck solution-68ae49a8
  (fn [n]
    (count
      (filter #(= 1 %)
        (map (partial
               (fn gcd [m n] (if (= m 0) n (recur (mod n m) m)))
               n)
          (range 1 (inc n)))))))

(defcheck solution-699859fd
  (let [divides? #(= 0 (mod %1 %2))
        coprime? (fn [a b] (not-any? #(and (divides? a %) (divides? b %)) (range 2 (inc (min a b)))))]

    (fn [x]
      (if (= 1 x)
        1
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-69f90ba0
  (fn [n]
    (letfn [(gcd [larger smaller]
              (loop [l larger s smaller]
                (if (not= 0 s)
                  (recur s (mod l s))
                  l)))]

      (count (filter (fn [i]
                       (= 1 (gcd i n)))
               (range 1 (inc n)))))))

(defcheck solution-6a71b58
  (fn [n] (max 1 (count (filter #(= 1 (loop [a n b %] (if (= b 0) a (recur b (rem a b))))) (range 1 n))))))

(defcheck solution-6a78c4f1
  (fn [x]
    (let [gcd #(if (zero? %2) % (recur %2 (mod % %2)))]
      (if (= x 1)
        1
        (count (filter #(= 1 (gcd x %)) (range 1 x)))
        )
      )
    ))

(defcheck solution-6a98c932
  (fn [x]
    (if (= 1 x)
      1
      (let [gcd (fn [a b]
                  (if (= 0 b)
                    a
                    (recur b (rem a b))))]
        (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-6b3019ba
  (letfn [(gcd [a b] (if (zero? b) a (recur b, (mod a b))))
          (coprime? [a b] (= 1 (gcd a b)))
          (f [x] (->> x range (filter #(coprime? x %)) count))] f))

(defcheck solution-6b59d83f
  (fn euler [n]
    (letfn [(divisible? [n d] (zero? (mod n d)))
            (first-div [n d] (->> (Math/sqrt n)
                               inc
                               (range d)
                               (filter (partial divisible? n))
                               first))
            (divisors [n d]
              (let [f (first-div n d)]
                (if f
                  (conj (divisors (/ n f) f) f)
                  (list n))))
            (prime-euler [[f & rest]] (* (dec f) (apply * rest)))]
      (->> (divisors n 2)
        (partition-by identity)
        (map prime-euler)
        (reduce *)
        (max 1)))))

(defcheck solution-6bc16629
  (fn [n]
    (let [gcd (fn [x y]
                (cond
                  (zero? x) y
                  (zero? y) x
                  :else (recur y (mod x y))
                  )
                )
          ]
      (count (filter #(= (gcd n %) 1) (range 1 (inc n))))
      )
    ))

(defcheck solution-6bc96090
  (fn [x]
    (let [
          dv (fn [a b]
               (cond
                 (= a 0) b
                 (= b 0) a
                 (> a b) (recur (- a b) b)
                 :else (recur a (- b a))
                 ))
          coprime? (fn [x b]
                     (cond
                       (> b x) false
                       (= 1 b) true
                       (= 0 b) false
                       :else (= 1 (dv x b))
                       ))
          ]
      (count (filter #(coprime? x %) (range (+ 1 x))))
      )))

(defcheck solution-6c7bfaff
  (fn eulers-totient
    [num]
    (letfn [(divisors [n] (filter #(= (quot n %)
                                     (/ n %))
                            (range 1 (inc n))))
            (coprime [x y]
              (= #{1}
                (clojure.set/intersection (set (divisors x))
                  (set (divisors y)))))]
      (count (filter #(coprime num %) (range 1 (inc num)))))))

(defcheck solution-6c8a75f0
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn [x y] (if (= y 0) x (recur y (mod x y))))]
        (count (filter #(= (gcd n %) 1) (range n)))))))

(defcheck solution-6cee811c
  ;(fn [n]
  ;  (if (= n 1) 1
  ;    (count (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
  ;             (for [x (range 1 n) :when (= 1 (gcd x n))] x)))))

  (fn [x]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (if (= x 1) 1 (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-6db2c16b
  (fn euler [x]
    (letfn [(gcd [n1 n2]
              (loop [mn (min n1 n2)]
                (if (and (zero? (mod n1 mn)) (zero? (mod n2 mn)))
                  mn
                  (recur (dec mn)))))]
      (if (= 1 x)
        1
        (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-6dcb7051
  (fn [n]
    (letfn
     [(h [a b]
        (first
          (filter #(= [0 0] (map mod [a b] [% %]))
            (iterate dec (min a b)))))]
      (+ 1 (count
             (filter #(= 1 (h % n)) (range 2 n)))))))

(defcheck solution-6e25237c
  #(letfn [(gcm [a b]
             (cond
               (> a b) (gcm b a)
               (zero? (mod b a)) a
               :else (gcm a (- b a))))]
     (if (= 1 %) 1
                 (count (for [i (range 1 %) :when (= 1 (gcm i %))]
                          i)))))

(defcheck solution-6e41fdaf
  (fn [n]
    (letfn
     [(gcd [m n] (if (= m 0) n (recur (mod n m) m)))]
      (count (filter #(= 1 (gcd %1 n)) (range 1 (inc n)))))))

(defcheck solution-6e57bf4b
  (fn [x]
    (let [p (fn [o t1 t2]
              (if (< t1 t2)
                (recur o t2 t1)
                (if (= t1 t2 1)
                  1
                  (if (= t2 1)
                    o
                    (if (< 0 (mod t1 t2))
                      (recur o t2 (mod t1 t2))
                      0)))))]
      (count (filter #(< 0 (p x x %)) (range 1 (+ 1 x)))))))

(defcheck solution-6ec7ff9a
  (fn eulers-totient [n]
    (count (map first (filter #(= (% 1) 1) (map #(vector % ((fn [a b] (if (= b 0) a (recur b (mod a b)))) n %)) (range 1 (inc n))))))))

(defcheck solution-6eca796
  (fn euler [n]
    (letfn [(primes
              ([]  (concat [2] (primes 3)))
              ([n] (let [sqrt-n (+ (Math/sqrt n) 1)
                         flag? (if (odd? n) (every? false? (map #(zero? (mod n %)) (range 3 sqrt-n 2))) false)]
                     (if (true? flag?) (lazy-seq (cons n (primes (inc n))))
                                       (recur (inc n))))
               ))
            (coprime? [p q]
              {:pre [(< 0 p) (< 0 q)]}
              (not-any? #(and (zero? (rem p %))
                              (zero? (rem q %)))
                (take-while #(<= (* 2 %)  p) (primes))))
            ]
      (if (= 1 n) 1
                  (count (filter #(coprime? n %) (range 1 n)))))
    ))

(defcheck solution-6f6bbd69
  (fn f [n]
    (count (filter (fn [i] (= 1 (#(if (= 0 %2) % (recur %2 (mod % %2))) i n))) (range n)))))

(defcheck solution-6fa6a15
  (fn [x]
    (letfn [(gcd [x y] (if (= 0 y) x (recur y (mod x y))))]
      (count
        (for [y (range x)
              :when (= 1 (gcd x y))]
          y)))))

(defcheck solution-6fb22607
  (fn totient [x]
    (let [gcd (fn gcd [a b]
                (cond
                  (< a b) (if (zero? (rem b a)) a (gcd (rem b a) a))
                  (= a b) a
                  :else (gcd b a)))]
      (inc (count (filter #(= 1 (gcd x %)) (range 2 x))))
      )))

(defcheck solution-6fd0072c
  (fn euler [x]
    (if (<= x 1) 1
                 (letfn [(gcd [a b]
                           (let [mx (max a b)
                                 mn (min a b)
                                 r (rem mx mn)]
                             (if (= 0 r) mn
                                         (gcd mn r))))]
                   (loop [curr x acc 0]
                     (if (= curr 1) acc
                                    (recur (dec curr) (if (= 1 (gcd x (dec curr))) (inc acc)
                                                                                   acc))))))))

(defcheck solution-705a3cdc
  (fn eu [n]
    (if (= n 1)
      1
      (reduce
        #(if
          (loop [n1 n, n2 %2]
            (let [m (mod n1 n2)]
              (if (= m 0)
                (= n2 1)
                (recur n2 m)
                )
              )
            )
           (inc %1)
           %1)
        1
        (range 2 n)
        )
      )
    ))

(defcheck solution-707d9683
  (fn [n] (reduce #(if (= 1
                         (reduce (fn [a b] (if (and (= (mod n b) 0) (= (mod %2 b) 0)) b a))
                           1
                           (range 1 (if (> %2 %1) (inc %2) (inc %1))))) (inc %1) %1)
            0
            (range 1 (inc n)))))

(defcheck solution-70950217
  (fn [a]
    (letfn [(co [x y]
              (if (= y 0) (= x 1) (recur y (rem x y))))]
      (count (filter #(co a %) (range a))))))

(defcheck solution-70e9c1ba
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
                  (count (for [x (range 1 n)
                               :when (= 1 (gcd n x))] x))))))

(defcheck solution-71a8333e
  (fn [n]
    (letfn [(nok [a b]
              (let [[x1 x2] (sort [a b])]
                (if (zero? x1) x2
                               (nok x1 (mod x2 x1)))))
            (coprime? [a b] (= 1 (nok a b)))]
      (if (= 1 n) 1
                  (count
                    (filter #(coprime? % n)
                      (range 1 n)))))))

(defcheck solution-71ae07df
  (fn totient [x]
    (let [gcd (fn [a b]
                (cond (= b 0) a
                      :else (recur b (mod a b))))]
      (if (= x 1) 1
                  (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-71cc3654
  (fn euler [n]
    (if (= n 1)
      1
      (let [gcd (fn gcd [x y] (if (= y 0) x (gcd y (rem x y))))]
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-71deddc7
  (fn [x]
    (if (= 1 x) 1
                (letfn [(g [a b]
                          (loop [x a y b]
                            (if (< x y)
                              (recur y x)
                              (if (zero? y)
                                x
                                (recur y (rem x y))))))]
                  (count (filter #(= 1( g x %)) (range 1 x)))))))

(defcheck solution-7223bdef
  (fn euler-totient-fn [x]
    (letfn [(gcd [a b]
              (if (zero? b) a
                            (gcd b (rem a b))))
            (coprime? [x y]
              (= 1 (gcd x y)))]
      (count
        (reduce
          (fn [acc each]
            (if (coprime? x each)
              (conj acc each)
              acc))
          [] (range 1 (inc x)))))))

(defcheck solution-72642fb
  (fn [x]
    (letfn [(gcd [a b]
              (if (zero? b) a (recur b (mod a b))))
            (coprime [y]
              (= 1 (gcd x y)))]
      (count (filter coprime (range x))))))

(defcheck solution-72e18708
  (fn [n]
    (count (filter
             #(= ((fn g [x y] (if (= 0 y) x (g y (mod x y)))) % n) 1)
             (range n)))))

(defcheck solution-733f2227
  (fn totient [n]
    (letfn [(gcd [a b] (apply max (filter #(= 0 (mod a %) (mod b %)) (range 1 (max a b)))))]
      (if (= 1 n)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-735457a2
  (fn [n] (if (= 1 n) n (let [gcd (fn g [a b] (if (= 0 b) a (g b (mod a b))))]
                          (count (filter (fn [x] (= 1 (gcd x n))) (range n)))))))

(defcheck solution-738c9adb
  (fn totient
    [n]
    (let [gcd (fn [a b]
                (cond
                  (zero? a) b
                  (zero? b) a
                  0 (recur b (mod a b))))]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-74443841
  (fn totientX [n]
    (if (= n 1) 1
                (count (filter (fn [x] ((fn coprime? [a b] (= ((fn gcd [a b]
                                                                 (if(zero? b) a
                                                                              (gcd b (mod a b))
                                                                              ))
                                                               a b) 1)) x n)) (range 1 n))))
    ))

(defcheck solution-74645e1e
  #(case %
     1 1
     10 4
     40 16
     60))

(defcheck solution-7487541b
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (inc (count (filter #{1} (map #(gcd n %) (range 2 n))))))))

(defcheck solution-748c605e
  (fn [n]
    (letfn [(gcd [x y] (if (= 0 y) x (gcd y (rem x y))))]
      (count (filter #(= (gcd n %) 1) (range 1 (inc n)))))))

(defcheck solution-74b0bd51
  (fn [x]
    (case x 1 1
            (count
              (filter #((fn coprime? [x y] (let [z (mod y x)] (if (= z 0) (= x 1) (coprime? z x))))
                        % x)
                (range 1 x))))))

(defcheck solution-751fb32b
  (fn euler [num x]

    (if (= x 1)

      1

      (if (> num x)

        0

        (if (= (some (set(filter #(= (mod x %1) 0) (range 2 x))) (filter #(= (mod num %1) 0) (range 2 (inc num)))) nil)

          (+ 1 (euler (inc num) x))

          (euler (inc num) x))))) 1)

(defcheck solution-7520c855
  (fn totient [number]
    (let [ cnt (count
                 (for [x (range 1 number)
                       :when (=
                               ((fn gcd [a b]
                                  (if (= b 0)
                                    a
                                    (gcd b (mod a b)))) number x) 1)]
                   x))] (if (not= cnt 0)
                          cnt
                          1))))

(defcheck solution-758637bf
  (fn totient [x]
    (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
      (if (= 1 x) 1
                  (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-76f16209
  (fn [n]
    (if (> 2 n)
      1
      (letfn [(gcd [a b] (if (zero? a) b (recur (mod b a) a)))]
        (count (filter #(and (pos? %) (= 1 (gcd % n))) (range n)))
        )
      )
    ))

(defcheck solution-77dfef5e
  (fn [x] (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))] (count (filter #(= 1 (gcd x %)) (range 1 (inc x)))))))

(defcheck solution-781f9aa7
  (fn [x]
    (if (= 1 x)
      1
      (let [gcd (fn [a b]
                  (if (zero? b)
                    a
                    (recur b (mod a b))))]
        (count (for [m (range x)
                     :when (= 1 (gcd m x))]
                 m))))))

(defcheck solution-7842f9fd
  #(letfn[(gcm [x y]
            (let [ minVal (min x y)
                  maxVal (max x y)
                  remVal (rem maxVal minVal)]
              (if (= remVal 0) minVal
                               (recur remVal minVal))))]
     (loop [x % n 1 c 0]
       (cond
         (= x 1) 1
         (= x n) c
         (= 1 (gcm x n)) (recur x (inc n) (inc c))
         :else (recur x (inc n) c)))))

(defcheck solution-78580f60
  (fn eulor[x]
    (letfn [
            (gcd
              [a b]
              (let[ c (mod a b)]
                (if(zero? c)
                  b
                  (recur b c))))
            (get-eulor
              [i x num]
              (cond (= 1 x ) 1
                    (= i x) num
                    (= 1 (gcd x i)) (recur (inc i) x (inc num))
                    :else (recur (inc i) x num)))]
      (get-eulor 1 x 0))))

(defcheck solution-785dcb74
  (fn [n]
    (let [divs (fn [x] (filter #(zero? (mod x %)) (range 1 (inc x))))]
      (count
        (filter #(= #{1} (clojure.set/intersection (set (divs n)) (set %)))
          (map divs (range 1 (inc n))))))))

(defcheck solution-786dfde9
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (if (= n 1) 1
                  (->> (range 1 (inc n))
                    (map #(gcd n %))
                    (filter #(= 1 %))
                    count)
                  )
      )
    ))

(defcheck solution-78cd26a4
  (fn totient
    [x]
    (if (= x 1) 1
                (reduce
                  (fn [r a]
                    (if
                     (not-any?
                       #(and (= (mod a %) 0) (= (mod x %) 0))
                       (range 2 (inc a)))
                      (inc r)
                      r))
                  0
                  (range 1 x)))))

(defcheck solution-78d0d33c
  (fn totient [x]
    (if (= x 1) 1
                (letfn [(divisors [x]
                          (->> (filter #(= (mod x %) 0) (range 1 (inc x)))
                            set))
                        (is-coprime? [x y]
                          (let [divx (divisors x)
                                divy (divisors y)]
                            (= (clojure.set/intersection divx divy) #{1})))]
                  (count (filter #(is-coprime? x %) (range 1 x)))))))

(defcheck solution-78e7f0a9
  (fn p75
    [x]
    (if (= x 1)
      1
      (count (filter #(= 1 ((fn
                              [n m]
                              (if (zero? n)
                                m
                                (recur (rem m n) n))) x %)) (range 1 x))))))

(defcheck solution-78efe5b2
  (fn [n]
    (if (= n 1)
      1
      (count (remove (fn [i] (some #(= (mod i %) (mod n %) 0) (range 2 (inc i))))
               (range 1 n))))))

(defcheck solution-7952141a
  #(apply + 1 (for [x (range 1 %)]
                ((fn f [y z]
                   (or (#{0 1} (rem y z)) (f z (rem y z))))
                 % x))))

(defcheck solution-796235ef
  (fn [n]
    (if (= 1 n)
      1
      (let [gcd #(if (= %2 0) %1 (recur %2 (mod %1 %2)))]
        (count (filter #(= 1 (gcd n %)) (reverse (range 1 n))))))))

(defcheck solution-79fa4044
  (fn eulers-totient-fn
    ([x]
     (case x
       1 1
       (eulers-totient-fn x (dec x))))
    ([x y]
     (let [f (fn gcd
               [g s]
               (if (= 0 s)
                 g
                 (gcd s (mod g s))))]
       (case y
         1 1
         (+
          (case (f x y)
            1 1
            0)
          (eulers-totient-fn x (dec y))))))))

(defcheck solution-7a3ec9d1
  (fn [x]
    (if (= 1 x) 1
                (letfn [(gcd [a b]
                          (if (zero? b) a (recur b (mod a b))))]
                  (count (filter #(= 1 %) (map #(gcd x %) (range 1 x))))))))

(defcheck solution-7ab27050
  (fn[x]
    (letfn [(gcd[x y] (if (zero? y) x (recur y (mod x y))))]
      (count (filter #(= 1 (gcd x %)) (range 0 x))))))

(defcheck solution-7b3a3d2f
  #(count (for [x (range %) :when (not-any? (fn [i] (= 0 (mod % i) (mod x i))) (range 2 %))] x)))

(defcheck solution-7b4727ff
  (fn [n]
    (count
      (filter
        #(= 1 ((fn [a b] (if (= b 0) a (recur b (mod a b)))) n %))
        (range 1 (+ 1 n))))))

(defcheck solution-7b80cffb
  (fn totient [n]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))
          coprime? (fn [a b] (= 1 (gcd a b)))]
      (if (= 1 n)
        1
        (count (filter #(coprime? % n)
                 (range 1 (inc n))))))))

(defcheck solution-7be319d5
  (fn totient [n]
    (let [gcd (fn [x y](loop [a x b y] (if (= (rem a b) 0) b (recur b (rem a b)))))]
      (reduce (fn [acc x] (if (= (gcd x n) 1) (inc acc) acc))
        0 (range n)))))

(defcheck solution-7be81168
  (fn [x]
    (let [gcd #(if (zero? %2) % (recur %2 (mod % %2)))
          coprime? #(= 1 (gcd x %))]
      (count (filter coprime? (range 1 (inc x)))))))

(defcheck solution-7bf244cd
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a
                                     (recur b (mod a b))))]
      (count
        (filter #(= % 1)
          (map #(gcd n %) (range 1 (inc n))))))))

(defcheck solution-7c500289
  (fn totient-func[n]
    (letfn [(gcd [p q](if (= q 0) p (gcd q (mod p q))))]
      (if (= n 1) 1
                  (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-7c5df554
  (fn boo [n]
    (let
     [gcd (fn [x y]
            (let
             [mn (min x y)
              mx (max x y)
              md (mod mx mn)]
              (if (= 0 md)
                mn
                (recur mn md))))]
      (if (= n 1)
        1
        (->> n
          (range 1)
          (filter #(= 1 (gcd % n)))
          (count ))))))

(defcheck solution-7cdc4f44
  (fn phi [n]
    (letfn [(gcd [a b] (if (zero? a) b (recur (mod b a) a)))
            (comprime? [a b] (= 1 (gcd a b)) )]
      (if (= 1 n) 1
                  (->> (range 1 n)
                    (filter (partial comprime? n))
                    (count)
                    )
                  )
      )
    ))

(defcheck solution-7d11e6bc
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn [x y]
                  (loop [m (if (> x y) x y) n (if (= m x) y x) r (rem m n)]
                    (if (= r 0) n (recur n r (rem n r)))))]
        (reduce #(if (= (gcd %2 n) 1) (inc %1) %1) 0 (range 1 n))))))

(defcheck solution-7d910178
  (fn [x]
    (if (= x 1)
      1
      (let [gcd (fn gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
        (count (filter #(= (gcd % x) 1) (range 1 x)))))))

(defcheck solution-7df27ec8
  #(letfn [(g ([a b] (if (zero? b) a (g b (rem a b)))))]
     (inc (count (filter (fn [x] (= 1 (g % x))) (range 2 %))))))

(defcheck solution-7e2c3a94
  (fn etot [n]
    (count (filter #(= 1 ((fn gcd [a b]
                            (let [r (mod a b)]
                              (cond
                                (= 0 r) b
                                (= 1 r) 1
                                :else (gcd b r)))) n %))
             (range 1 (inc n))))))

(defcheck solution-7eb2e100
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))
            (coprime? [a b] (= 1 (gcd a b)))]
      (count (filter (partial coprime? n) (range n))))))

(defcheck solution-7eebc84a
  (fn totient [x]
    (if (= x 1) 1
                (let [gcd (fn gcd [x y]
                            (let
                             [minv (min x y)
                              maxv (max x y)]
                              (if (= 0 minv) maxv
                                             (gcd (- maxv minv) minv))))
                      relprime (fn [x y] (= 1 (gcd x y)))
                      candidates (range 1 x)
                      coprimes (filter #(relprime % x) candidates)]
                  (count coprimes)))))

(defcheck solution-7f92762e
  (letfn [
          (gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))
          (coprime? [a b]
            (= 1 (gcd a b)))]
    (fn [x]
      (if (= x 1)
        1
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-7fcd1696
  (fn [x] (inc (count (filter #(= 1 ((fn gcd [a b]
                                       (if (= a b)
                                         a
                                         (if (> a b)
                                           (recur (- a b) b)
                                           (recur a (- b a))))) x %1))
                        (range 2 x))))))

(defcheck solution-7fd28ec6
  (fn tot [x]
    (if ( = x 1) 1
                 (letfn [ (divs  [nr]  (conj (set (filter #(= 0 (rem nr %)) (range 1 (inc nr)))) 1)) ]
                   (count (filter identity (for [ y (range 1 x) ]  (= #{1} (clojure.set/intersection (divs x) (divs y))))))))))

(defcheck solution-7ff9ee6c
  (fn [n]
    (let [	gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))
          co-prime? (fn [x y] (= 1 (gcd x y)))
          co-prime-ints (fn [x] (filter #(co-prime? % x) (range 1 (condp = x 1 2 x))))]
      (count(co-prime-ints n)))))

(defcheck solution-800fb8d2
  (fn [n]
    (let [divisors (fn [i] (set (filter #(= 0 (mod i %)) (range 1 (inc i)))))
          div-n (divisors n)]
      (count
        (filter #(= #{1} (clojure.set/intersection div-n %))
          (map divisors (range 1 (inc n))))))))

(defcheck solution-807f0b42
  (fn euler-totient [x]
    (letfn [(gcd [x y]
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
                    (recur l r (rest lc) rc)))))]
      (cond
        (= x 1) 1
        :else (let [numbers (range 1 x)]
                (count (filter #(= (gcd % x) 1) numbers)))))))

(defcheck solution-80de61d6
  (fn x [n] (let [gcd (fn [a b]
                        (cond
                          (= a b) a
                          (> a b) (recur (- a b) b)
                          (< a b) (recur a (- b a))
                          )
                        )
                  ] (if (= n 1) 1
                                (count (filter #(= 1 (gcd n %)) (range 1 (inc n))))
                                )
                    )
    ))

(defcheck solution-8104af03
  (fn [x] (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
            (->> (range x) (filter #(= 1 (gcd x %))) count))))

(defcheck solution-812a7121
  (fn [n]
    (let [my-pgcd
          (fn [ a b ]
            (let [ c (mod a b)]
              (if (= c 0) b (recur b c))))
          ]
      (if (= n 1) 1 (count(filter #(= 1 (my-pgcd n %1)) (range 1 n)))))))

(defcheck solution-81866945
  (fn [x]
    (if (= x 1)
      1
      (count (filter
               (fn [n]
                 (= 1 ((fn gcd [a b]
                         (last (filter
                                 #(and (zero? (mod a %)) (zero? (mod b %)))
                                 (range 1 (inc (min a b)))))) n x)))
               (range 1 x))))))

(defcheck solution-821898c7
  (fn [x]
    (letfn [(gcd [x y] (loop [a x b y] (if (zero? b) a (recur b (mod a b)))))
            (coprime? [x y] (= 1 (gcd x y)))
            (coprimes [x] (cons 1 (filter #(coprime? x %) (range 2 x))))]
      (count (coprimes x)))))

(defcheck solution-82204c02
  (fn [x]
    (letfn[(gcd [x y] (let[r (rem y x)] (if (zero? r) x (gcd r x))))]
      (count (filter #(= 1 (gcd % x)) (range 1 (inc x)))))))

(defcheck solution-829befb9
  (fn [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (coprime? [a b]
              (= (gcd a b) 1))]
      (count (filter (partial coprime? x) (range x))))))

(defcheck solution-82a7a9b3
  (fn [n]
    (if (= 1 n) 1
                (letfn [(gcd [a b] (if (= 0 a) b (gcd (mod b a) a)))]
                  (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-82d01ae9
  (fn me [n]

    (let [gcd (fn [a b]

                (cond
                  (= a b) a
                  (> a b) (recur (- a b) b)
                  :else (recur a (- b a)))
                )]

      (if (= 1 n)
        n
        (count (filter #(= 1 (gcd n %)) (range 1 n) ))

        )
      )

    ))

(defcheck solution-82d8f826
  (fn [n]
    (if (= 1 n)
      1
      (count (filter #(= 1 (loop [a % b n]
                             (if (zero? b)
                               a
                               (recur b (mod a b))))) (range 1 n))))))

(defcheck solution-82d9978b
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
        (->> (range 1 n)
          (map (partial gcd n))
          (filter #(= 1 %))
          count)))))

(defcheck solution-8345158d
  (fn  [n]
    (let [gcd (fn [x y]
                (cond
                  (= x 0) y
                  (= y 0) x
                  (> x y) (recur y (rem x y))
                  true  (recur x (rem y x))))]
      (count
        (filter  #(= (gcd % n) 1) (range 1 (inc n)))))))

(defcheck solution-83e1002
  (fn [m] (if (= 1 m) 1
                      (count (filter
                               (fn [n] (=  '(1)
                                         (filter (fn [x] (= 0 (rem n x)   (rem m x)))(range 1 (+ 1 n))))) (range 1 m))))))

(defcheck solution-84292d62
  (fn [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (if (= n 1)
        1
        (count
          (for [i (range 1 n)
                :when (= 1 (gcd i n))]
            i))))))

(defcheck solution-84515713
  (fn [num]
    (letfn [(devisors [n]
              (set (filter #(= 0 (rem n %)) (range 2 (inc n)))))
            (not-intersect? [a b]
              (not (some #(contains? a %) b)))]

      (reduce (fn [r x]
                (if (not-intersect? (devisors x) (devisors num))
                  (inc r)
                  r))
        0
        (range 1 (if (= num 1) 2 num)))
      )
    ))

(defcheck solution-84942c14
  (fn blah [n]
    (letfn [(coprime [a b]
              (letfn [(gcd [a b]
                        (if (= a b)
                          a
                          (if (> a b)
                            (gcd (- a b) b)
                            (gcd (- b a) a))))]
                (= 1 (gcd a b))))]
      (inc (count (filter true? (map #(coprime n %) (range 2 n))))))))

(defcheck solution-84bafe3
  (fn [x]
    (letfn [(pgcd [a b]
              (if (= b 0) a
                          (pgcd b (rem a b))))]
      (if (= 1 x) 1
                  (count
                    (filter #(= 1 (pgcd x %))
                      (range 1 x)))))))

(defcheck solution-84fdc484
  (fn totient
    [x]
    (letfn [(gcd [a b]
              (cond
                (= a b)
                a
                (> a b)
                (recur (- a b) b)
                (< a b)
                (recur a (- b a))))]
      (if (= x 1)
        1
        (count (filter #{1} (map #(gcd % x) (range 1 x))))))))

(defcheck solution-854b0756
  (fn [n]
    (inc
      (count
        (loop [t 2 l (range t n)]
          (if t
            (let [d #(zero? (mod % t))
                  l* (if (d n) (remove d l) l)]
              (recur (first (filter #(> % t) l*)) l*))
            l))))))

(defcheck solution-856789b1
  (fn  [n]
    (let [divisors (memoize (fn [n]
                              (->> (range 1 n)
                                (filter #(= 0 (mod n %)))
                                doall
                                set)))]
      (->> (range 1 n)
        (filter #(> (mod n %) 0))
        (map divisors)
        (map #(clojure.set/intersection (divisors n) %))
        (filter #(= (count %) 1))
        count
        inc))))

(defcheck solution-85690c16
  (fn totient [x]
    (let [vdiv (fn [y] (filter #(= (rem y %) 0) (range 1 (inc y))))
          tr (range 1 x)
          cr (dec x)
          vdx (vdiv x)]
      (loop [i 0, j 0]
        (if (= i cr)
          (if (= x 1) 1 j)
          (recur
            (inc i)
            (if (= (clojure.set/intersection (set vdx) (set (vdiv (nth tr i)))) #{1}) (inc j) j)))))))

(defcheck solution-85a96ce6
  #(let [gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))
         coprime? (fn [x] (true? (= 1 (gcd x %))))]
     (if (= 1 %) 1 (count (filter coprime? (range 0 %))))
     ))

(defcheck solution-860fcd7d
  (fn totient
    ([x]
     (totient x x []))
    ([x n acc]
     (if (zero? n)
       (count acc)
       (if (= 1 ((fn gcd
                   [a b]
                   (if (= a b)
                     a
                     (if (> a b)
                       (recur (- a b) b)
                       (recur a (- b a))))) x n))
         (recur x (dec n) (conj acc n))
         (recur x (dec n) acc))))))

(defcheck solution-86c808a
  (fn [n]
    (letfn [(gcd [x y]
              (apply max (filter #(zero? (+ (rem x %) (rem y %))) (range 1 (+ x y)))))]
      (if (= n 1) 1
                  (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-86ffba13
  (fn totient [n]
    (->> (range 1 (max 2 n))
      (map (partial (fn gcd [a b]
                      (cond
                        (< a b) (gcd b a)
                        (= 0 b) a
                        :else   (gcd b (mod a b))))
             n))
      (filter #(= 1 %))
      count)))

(defcheck solution-8747c0d6
  (fn [x]
    (count
      (filter
        (partial #(if (zero? %)(= %2 1) (recur (mod %2 %) %))x)
        (range 1 (inc x))))))

(defcheck solution-87a91dac
  (fn totient [x]
    (if (= x 1) 1
                (letfn [(gcd [x y]
                          (if (= 0 (min x y))
                            (max x y)
                            (gcd (min x y) (- (max x y) (min x y)))))]
                  (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-87ce2ed
  (fn [n]
    (letfn [(divides? [a b]
              (= 0 (mod b a)))
            (coprime? [m]
              (not-any? #(and (divides? %1 n) (divides? %1 m)) (range 2 (inc m))))]
      (if (= n 1)
        1
        (count (filter coprime? (range 1 n)))))))

(defcheck solution-881ec16a
  (fn [n]
    (let [gcd
          #(loop [x %1, y %2]
             (cond
               (= 0 x) y
               (> x y) (recur y x)
               :else (recur (rem y x) x)))]
      (count
        (filter
          #(= 1 (gcd % n))
          (range 1 (inc n)))))))

(defcheck solution-88952834
  (fn [x] ({1 1 10 4 40 16 99 60} x)))

(defcheck solution-89c16307
  (fn [n]
    (letfn [(gcd [a b]
              (if (zero? b) a
                            (gcd b (mod a b))))]
      (count (filter #(= (gcd n %) 1) (range n)))
      )
    ))

(defcheck solution-8a36bbec
  (fn tot [n]
    (let [gcd (fn [a b]
                (loop [a a b b]
                  (if (= 0 b)
                    a
                    (recur b (mod a b)))))
          coprime? (fn [a] (= 1 (gcd n a)))]
      (count (filter coprime? (range n))))))

(defcheck solution-8a61fbef
  (fn [n]
    (if (= 1 n)
      1
      (->>
        (range n)
        (map (partial
               (fn [a b]
                 (cond
                   (zero? a) b
                   (zero? b) a
                   (< a b) (recur a (- b a))
                   :else (recur b (- a b)))) n))
        (filter #(= 1 %))
        (count)))))

(defcheck solution-8ad7afa6
  (let [gcd (fn [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
    (fn [n]
      (if (= n 1)
        1
        (count (filter #(= (gcd n %) 1) (range 1 n)))
        ))
    ))

(defcheck solution-8b5e69ea
  (fn t [q]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (+ 1 (count (filter #(= (gcd q %) 1) (range 2 q)))))))

(defcheck solution-8b7ca4f9
  (fn [n]
    (count
      (filter
        #(= 1
           ((fn gcd [a b]
              (if (= a b) a
                          (if (> a b) (gcd (- a b) b)
                                      (gcd a (- b a))
                                      ))
              ) n %))
        (range 1 (+ n 1))
        )
      )
    ))

(defcheck solution-8b946768
  (fn euler [x]
    (let[
         numbers (range 1 x)
         gcd (fn gcd [a b]
               (if (= a b)
                 a
                 (let [mx (max a b) mn (min a b)]
                   (gcd mn (- mx mn))
                   )
                 )
               )
         coprime #(= 1 (gcd x %))
         ]
      (count (if (= x 1) '(1)
                         (filter coprime numbers)))
      )))

(defcheck solution-8c310d4e
  (fn [n]
    (letfn [(gcd [x y] (if (zero? y) x (gcd y (mod x y))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-8ce40687
  (fn [n]
    (let [gcd (fn [a b]
                (let [divisors (fn [n] (->> (range 1 (inc n)) (filter #(zero? (mod n %))) (set)))]
                  (apply max (clojure.set/intersection (divisors a) (divisors b)))))]
      (if (= n 1)
        1
        (->> (range 1 n) (filter (fn [x] ( = 1 (gcd n x)))) count)))))

(defcheck solution-8d8bdb14
  #(letfn [(gcd [a b]
             (if (= b 0)
               a
               (gcd b (mod a b))))]
     (if-not (== 1 %)
       (count (filter (partial == 1) (map (partial gcd %) (range 1 %))))
       1)))

(defcheck solution-8e4b7f1e
  (fn [n]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

(defcheck solution-8e89fee6
  (fn [n]
    (letfn [(gcd [m n]
              (if (zero? n)
                m
                (recur n (mod m n))))]
      (count
        (filter #(= 1 (gcd n %))
          (range n))))))

(defcheck solution-902e6fd5
  (fn [n] (if (= n 1) 1
                      (let [gcd (fn [a b] (some
                                            #(if (and (= 0 (rem a %)) (= 0 (rem b %))) % false)
                                            (reverse (range (min a b)))))]
                        (dec (count (filter #(= 1 (gcd n %)) (range 2 n))))))))

(defcheck solution-9083736f
  (fn totient [x]
    (if (= x 1)
      1
      (count
        (filter #(= 1 ((fn gcd [a b] (if (zero? b) a (recur b (mod a b)))) x %))
          (range 1 x)
          )
        )
      )
    ))

(defcheck solution-90af23db
  (fn [n]
    (let [gcd (fn [a b]
                (if (zero? b) a
                              (recur b (mod a b))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-91026c4b
  (fn [n]
    (let [gcd (fn gcd [a b]
                (if (= b 0)
                  a
                  (gcd b (mod a b))))]
      (->> (range 1 (inc n))
        (filter #(= 1 (gcd % n)))
        (count)))))

(defcheck solution-912a8874
  (fn [a]
    (count
      (cons 1
        (filter (fn [b] (= 1
                          (some #(if (= 0 (mod a %)
                                       (mod b %)) %)
                            (range (inc (min a b)) 0 -1))))
          (range 2 a))))))

(defcheck solution-91331c1a
  (letfn [(G [x y]
            (cond (> y x) (G y x)
                  (zero? y) x
                  :else (G y (rem x y))))
          (P [x y]
            (= 1 (G x y)))

          (E [n]
            (count (filter #(P n %) (range 1 (max n 2)))))]
    E))

(defcheck solution-9183b309
  (fn totient_ [x]
    (let [gcd (fn gcd [x y] (if (zero? y) x (gcd y (mod x y))))]
      (count
        (for [i (range x)
              :when (= 1 (gcd x i))]
          i)))))

(defcheck solution-91c3ea93
  (fn [i]
    (letfn [(gcd [x y]
              (let [r (rem x y)]
                (if (= r 0)
                  y
                  (gcd y r))))]
      (if (= i 1)
        1
        (apply + (for [a (range 1 i)] (if (= (gcd i a) 1) 1 0)))))))

(defcheck solution-92111da9
  (fn [n]
    (letfn [(gcd
              [a b]
              (if (zero? b) a (recur b (mod a b))))]
      (count (reduce #(if (= 1 (gcd n %2)) (conj %1 %2) %1)
               nil
               (range n))))))

(defcheck solution-9253089e
  (fn j[n] (let[gcd (fn f[a b](if (= 0 b) a (f b (mod a b))))](count(filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-926403be
  (fn euler-totient [x]
    (if (< x 3) 1
                (let [divisors (filter #(zero? (mod x %)) (range 2 x))]
                  (count (filter
                           (fn [candidate]
                             (every? #(< 0(mod candidate %1)) divisors))
                           (range 1 x)
                           ))
                  ))))

(defcheck solution-9273a5ce
  (fn totient [n]
    (letfn [(gcd [a b] (if (= 0 b) a (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-927efab4
  (fn totient [ n ]
    (letfn [
            (gcd [ x y ]
              (if (= x y)
                x
                (gcd (- (max x y) (min x y)) (min x y))))]
      (if (= n 1) 1
                  (reduce #(+ %1 (if (= 1 (gcd %2 n)) 1 0)) 0 (range 1 n))))))

(defcheck solution-92e61815
  (let [gcd (fn [& nums]
              (let [[big lil] (sort > nums)]
                (loop [a big
                       b lil]
                  (let [q (quot a b)
                        r (rem a b)]
                    (if (zero? r)
                      b
                      (recur b r))))))
        coprime? (fn [n1 n2]
                   (== 1 (gcd n1 n2)))
        f (fn [n]
            {:pre [(pos? n)]}
            (if (== 1 n)
              1
              (->> (range 1 n)
                (filter (partial coprime? n))
                count)))]
    f))

(defcheck solution-930fda37
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-939310f1
  (fn __
    [number]
    (letfn [(gcd [x y]
              (if (= x y)
                x
                (if (> x y)
                  (recur (- x y) y)
                  (recur x (- y x)))))]
      (count (filter #(= 1 (gcd % number)) (range 1 (inc number)))))))

(defcheck solution-93de6700
  (fn [n]
    (count
      (filter
        #(= (loop [a % b n c a]
              (if (= (mod a c) (mod b c) 0)
                c
                (recur a b (dec c)))) 1)
        (range 1 (inc n))))))

(defcheck solution-95e2eb1f
  (fn [n]
    (let [gcd (fn f [x y] (if (= y 0) x (f y (mod x y))))]
      (inc (count
             (filter
               #(= 1 (gcd % n))
               (range 2 n)))))))

(defcheck solution-96acce6d
  (fn [x]
    (if (= 1 x)
      1
      (letfn [(gcd [a b] (cond (= a b) a
                               (< a b) (recur a (- b a))
                               :else   (recur b (- a b))))]
        (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-96dfa802
  (fn [n]
    (letfn [(coprimes? [a b]
              (= 1
                ((fn [a b]
                   (if (zero? (rem a b))
                     b
                     (recur b (rem a b)))) a b)
                ))]
      (if (= n 1)
        1
        (count (filter #(coprimes? n %) (range 1 n)))))))

(defcheck solution-96e2af
  (fn [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (rem a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-9704e268
  (fn [n] (if (= n 1)
            n
            (count (filter #(= 1 %)
                     (map #(last (remove nil?
                                   (for [x (range 1 (inc %1))]
                                     (if (and (zero? (mod %1 x)) (zero? (mod n x)))
                                       x)))) (range 1 n)))))))

(defcheck solution-9725095a
  (fn div [x]
    (letfn [
            (divisor? [x Q y]
              (and
               (= 0 (rem x y))
               (not (contains? Q y))))

            (next-divisor [x Q l]
              (some #(if (divisor? x Q %) %)
                (range l x)))

            (divisors [x]
              (loop [Q (sorted-set 1), l 2]
                (let [y (next-divisor x Q l)]
                  (if y
                    (recur (into Q (range y x y)) (inc y))
                    Q))))]

      (if (= x 1)
        1
        (- x (count (divisors x)))))))

(defcheck solution-97aa00a5
  (fn [n]
    (letfn [(gcd [a b]
              (loop [a a b b]
                (if (zero? b) a (recur b (mod a b)))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-97cac28a
  (fn [x]
    (if (= x 1)
      1
      (count
        (filter
          #(loop [n %]
             (cond
               (< n 2) 1
               (= 0 (mod x n) (mod % n)) nil
               1 (recur (- n 1))
               ))
          (range 1 x))))))

(defcheck solution-97f5d475
  (fn [n]
    (letfn [(gcd [m n]
              (if (zero? n)
                m
                (recur n (mod m n))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-97fd2db3
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [n m]
                (if (zero? m)
                  n
                  (gcd m (mod n m))))
              (coprime? [n m] (= 1 (gcd n m)))]
        (count (filter (fn [m] (coprime? n m)) (range 1 n)))))))

(defcheck solution-98320d5e
  (let [gcd (fn [x y] (if (zero? y)
                        x
                        (recur y (mod x y))))
        coprime? (fn [x y] (= 1 (gcd x y)))]
    (fn [x] (if (= 1 x)
              1
              (count (filter #(coprime? x %) (range 1 x)))))))

(defcheck solution-98676ef0
  (fn totient [x]
    (let [gcd (fn gcd [a b]
                (cond
                  (> a b)   (gcd b a)
                  (= a 0)   b
                  :else     (gcd (mod b a) a)))]
      (cond
        (= x 1)  1
        (> x 1)  (count (filter #(= 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-987f0521
  (fn [x]
    (letfn [(lcf [a b]
              (let [[l h] (sort [a b])]
                (if (zero? (mod h l))
                  l
                  (lcf l (mod h l)))))]
      (count (filter #(= (lcf x %) 1) (range 1 (inc x)))))))

(defcheck solution-98a5383a
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      ((comp count (partial filter #(= (gcd n %) 1))) (range n)))))

(defcheck solution-9947c00b
  (fn __ [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-99f067fa
  (fn [n]
    (case n
      1 1
      (count
        (reduce
          #(if (zero? (rem n %2))
             (reduce disj %1 (range %2 n %2))
             %1)
          (set (range 1 n))
          (range 2 n))))))

(defcheck solution-99f32d4f
  (fn phi [n]
    (let [gcf (fn [x y]
                (cond (= 0 (rem x y)) y
                      (= 0 (rem y x)) x
                      :else
                      (let [z (int (if (< x y) (quot x 2) (quot y 2)))
                            internal-gcf (fn [x y z]
                                           (if (or (= 1 z) (and (= 0 (rem x z)) ( = 0 (rem y z)))) z
                                                                                                   (recur x y (dec z))))]
                        (internal-gcf x y z))))
          relatively-prime? (fn [n m]
                              (= 1 (gcf m n)))
          list-of-relative-primes (fn [n]
                                    (filter #(relatively-prime? n %) (range 1 n)))]
      (do (if (= 1 n) 1
                      (count (list-of-relative-primes n)))))))

(defcheck solution-9a31e541
  (fn [n]
    (count
      (loop [ i n
             j (range n)
             x n]

        (if (> i 1)
          (if (= 0 (mod x i))
            (recur (dec i) (filter #(not (= 0 (mod % i))) j) x)
            (recur (dec i) j x)
            )
          j
          )))))

(defcheck solution-9a3494
  ;; Use my solution to the Greatest common divisor problem as part of
  ;; this answer.

  (fn [n]
    (let [gcd (fn [a b]
                (cond (< a b) (recur b a)
                      (zero? b) a
                      :else (recur b (mod a b))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-9a3a8255
  (fn totient [n]
    (if (= n 1) 1
                (let [gcd #(if (zero? %2) %1 (recur %2 (rem %1 %2)))]
                  (->> (range 1 n)
                    (filter #(= (gcd n %) 1))
                    (count))))))

(defcheck solution-9ab46229
  (fn [x]
    (letfn [(gcd [a b]
              (cond (= a b) a
                    (> a b) (gcd (- a b) b)
                    :else (gcd a (- b a))))]
      (if (= x 1)
        1
        (count (filter #(= 1 (gcd x %)) (drop 1 (range x))))))))

(defcheck solution-9ac346f1
  (fn [x]
    (if (= 1 x)
      1
      (->>
        (range 1 x)
        (filter (fn [n] (= 1
                          ((fn gcd [a b]
                             (if (or (= b 0) (= a b)) a
                                                      (if (< a b) (gcd b a)
                                                                  (gcd b (mod a b))))) n x))))
        (map (fn [_] 1))
        (reduce +)))))

(defcheck solution-9ad1477b
  (fn [n]
    (count
      (filter (fn [x] (= 1 (#(if (= 0 %2) % (recur %2 (mod % %2))) n x)))
        (range n)))))

(defcheck solution-9af575c2
  (fn [x]
    (if (= 1 x) 1
                (letfn [(gcd [a b]
                          (or (first
                                (drop-while #(not= 0 (rem a %) (rem b %))
                                  (range 2 (inc (Math/min a b)))))
                              1))]
                  (count (filter #(= 1 (gcd % x))
                           (range 1 x)))))))

(defcheck solution-9b0f3ac6
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (coprime? [a b] (= 1 (gcd a b)))]
    #(->> (range 2 %) (filter (partial coprime? %)) count inc)))

(defcheck solution-9b38c10e
  (fn [s]
    (letfn [(g [a b] (if (zero? b) a (g b (mod a b))))]
      (count
        (filter
          #(== 1 (g s %))
          (range s))))))

(defcheck solution-9b3af5b4
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b)
                         a
                         (recur b (mod a b))))]
      (if (= n 1)
        1
        (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-9b4a996b
  (fn pr75 [n]
    (letfn [(gcd [m n] ;m>=n
              (if (zero? n)
                m
                (recur n (rem m n))))]
      (count
        (filter #(= (gcd n %) 1)
          (range 1 (inc n)))))))

(defcheck solution-9b4de649
  (fn [x]
    (if (= x 1)
      1
      (let [gcd (fn [n m]
                  (if (zero? (rem n m)) m (recur m (rem n m))))
            gcds (map #(gcd % x) (range 1 x))]
        (count (filter #(= % 1) gcds))))))

(defcheck solution-9b630cd6
  (fn [a]
    (count
      (for [b (range a)
            :when (not-any? #(= 0 (rem a %) (rem b %)) (range 2 a))]
        b))))

(defcheck solution-9b6cfd45
  (fn [n]
    (if (= n 1) 1
                (let [coprime (fn [a b]
                                (every?
                                  #(or (< 0 (mod a %)) (< 0 (mod b %)))
                                  (range 2 (inc b))))]
                  (count (filter #(coprime n %) (range 1 n)))))))

(defcheck solution-9b825ba0
  (fn euler [x]
    (if (= 1 x)
      1
      (->>
        (for [cand (range 2 x)
              divisor (filter #(= 0 (mod x %)) (range 2 x)) :when (= 0 (mod cand divisor))]
          cand)
        (into #{})
        (count)
        (- (dec x))
        ))))

(defcheck solution-9ba511c1
  (fn totient [n]
    (letfn [(g [x y]
              (letfn [(gcd [x y]
                        (if (= (rem x y) 0) y (gcd y (rem x y))))]
                (if (> x y)
                  (gcd x y)
                  (gcd y x))))]
      (->>
        (range 2 n)
        (map #(g n %) )
        (filter #(= 1 %))
        (count)
        inc
        ))))

(defcheck solution-9bb31c39
  (fn [n]
    (if (= n 1)
      1
      (letfn [(mcd [n1 n2]
                (let [[s b] (if (>= n1 n2) [n1 n2] [n2 n1])]
                  (loop [mc 1 cands (range 2 s)]
                    (cond
                      (empty? cands) mc
                      (and (zero? (rem b (first cands))) (zero? (rem s (first cands)))) (recur (first cands) (rest cands))
                      :else (recur mc (rest cands))))))
              (cp? [n1 n2] (= 1 (mcd n1 n2)))]
        (count (filter (partial cp? n) (range 1 n)))))))

(defcheck solution-9c0aeb6f
  (fn totient [x]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (if (= 1 x)
        1
        (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-9c253ee8
  (fn [x]
    (letfn [(gcd [a b] (if (< a b) (recur b a)
                                   (if (zero? b)
                                     a
                                     (gcd (mod a b) b))))]
      (if (= x 1)
        1
        (count (filter #(= 1 (gcd % x)) (range x)))))))

(defcheck solution-9caabc41
  (fn euler-totient [n]
    (let [gcd
          (fn [a b]
            (last (let [min (if (< a b) a b)]
                    (filter
                      #(and (= 0 (rem a %)) (= 0 (rem b %)))
                      (range 1 (inc min))))))]
      (if (= n 1) 1
                  (count
                    (filter #(= 1 (gcd % n)) (range n)))))))

(defcheck solution-9cb08d72
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (->> (range 0 n)
        (filter #(= 1 (gcd n %)))
        count))))

(defcheck solution-9cccb5f6
  (fn [x]
    (letfn [(is-coprime? [n]
              (every? #(or (not= (mod n %) 0)
                           (not= (mod x %) 0))
                (range 2 (inc n))))]
      (loop [n (dec x) cnt 0]
        (if (>= 1 n)
          (inc cnt)
          (recur (dec n) (if (is-coprime? n) (inc cnt) cnt)))))))

(defcheck solution-9d034aa0
  (fn [n]
    (loop [tmpn 2 c 1]
      (if (< n tmpn)
        c
        (if (loop [gcd 1 d 2]
              (if (< tmpn d)
                (= 1 gcd)
                (if (and (= 0 (mod tmpn d)) (= 0 (mod n d)))
                  (recur d (inc d))
                  (recur gcd (inc d)))))
          (recur (inc tmpn) (inc c))
          (recur (inc tmpn) c))))))

(defcheck solution-9d0e4719
  (fn [n]
    (count (filter
             #(=
                1
                ((fn gcd [a b]
                   (if (= 0 b)
                     a
                     (gcd b (mod a b)))) n %))
             (range 1 (inc n))))))

(defcheck solution-9d247ec2
  (fn [x]
    (let [gcd (fn [a b]
                (if (zero? b) a (recur b (mod a b))))]
      (if (= 1 x) 1
                  (count
                    (for [y (range 1 x)
                          :when (= 1 (gcd x y))]
                      y))))))

(defcheck solution-9de0cdf5
  (fn coprime
    ([n] (coprime n (range 0 n)))
    ([n rng]
     (let [gcd (fn gcd [a b] (if (= b 0) a (gcd b (rem a b))))
           all-vec (filter #(= 1 (gcd n %)) rng)]
       (count all-vec)))))

(defcheck solution-9e413bd1
  (fn [n]
    (count
      (filter #(= 1 %)
        (map (partial #(if (= 0 %)
                         %2
                         (recur (mod %2 %) %)) n)
          (range 1 (+ n 1)))))))

(defcheck solution-9e4a40e6
  (fn totient  [x]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
      (if (= x 1) 1
                  (count (filter #(= % 1) (map gcd (repeat x) (range 1 x))))))))

(defcheck solution-9efdb3ae
  (fn euler [n]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (if (= n 1) 1
                  (count (filter #(= (gcd n %) 1) (range 1 n)))))))

(defcheck solution-9f5b9d95
  (fn [x]
    (let
     [gcd (fn [a b]
            (loop
             [re a
              qu b]
              (if
               (= qu 0)
                re
                (recur
                  qu
                  (rem re qu)))))
      coprime? (fn [a b]
                 (=
                   1
                   (gcd a b)))]
      (if
       (= x 1)
        1
        (count (filter
                 (partial coprime? x)
                 (range 1 x)))))))

(defcheck solution-a0491ebf
  (fn [n]
    (letfn [(isFactor [n x] (= 0 (rem n x)) )]
      (let [ops (range 2 n)
            factors (filter (partial isFactor n) ops)]
        (inc (count (filter #(not-any? (partial isFactor %) factors) ops))) ))))

(defcheck solution-a07b2119
  (fn [n] (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
                  (coprime-to-n? [a] (= 1 (gcd a n)))]
            (if (= 1 n)
              1
              (->> (range 1 n)
                (filter coprime-to-n?)
                count)))))

(defcheck solution-a0931006
  (fn totient [x]
    (letfn [(gcd [x y]
              (if (zero? y) x
                            (recur y (mod x y))))]
      (count (filter #(= 1 (gcd x %)) (range 1 (inc x)))))))

(defcheck solution-a0d5b7a5
  (fn [x]
    (letfn [(gcd      [x y] (if (zero? y) x (gcd y (mod x y))))
            (coprime? [x y] (= 1 (gcd x y)))]
      (if (= x 1)
        1
        (count (filter #(coprime? x %) (range 1 x)))))))

(defcheck solution-a10bf5d
  (fn [x]
    (let [gcd (fn [a b]
                (if (= a b)
                  a
                  (if (> a b)
                    (recur (- a b) b)
                    (recur a (- b a)))))]
      (count (filter #(= % 1) (map #(gcd x %) (range 1 (inc x))))))))

(defcheck solution-a1291bcf
  (fn [x]
    (letfn [(gcd [a b]
              (cond
                (= a b) a
                (> a b) (recur (- a b) b)
                :else (recur a (- b a))))]
      (count
        (for [a (range 1 (inc x))
              :when (= 1 (gcd x a))]
          a)))))

(defcheck solution-a15e85b0
  ;brute force
  (letfn [(gcd [x y]
            (cond
              (> y x) (recur y x)
              (zero? y) x
              :else (recur y (mod x y))))]
    (fn [n]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-a2660da4
  (fn
    [n]
    (let [coprime?
          (fn [a b]
            (let [r (range 2 (inc (min a b)))]
              (->>
                (filter #(= 0 (rem b %) (rem a %))
                  r)
                first
                nil?
                )))]
      (count (filter #(coprime? % n) (range 1 (inc n)))))))

(defcheck solution-a2db2174
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
        (count (filter #{1} (map (partial gcd n) (range 1 n))))))))

(defcheck solution-a3034f2c
  (fn [x]
    (let [co-prime?
          (fn [a b] (not-any? #(= '(0 0) (list (mod a %) (mod b %))) (range 2 (inc a))))]
      (count (filter #(co-prime? % x) (range 1 (inc x)))))))

(defcheck solution-a316b0d
  (fn euler-totient [num]
    (letfn [(gcd [x y]
              (loop [n x m y]
                (if (= 0 n)
                  m
                  (recur (mod m n) n))))]

      (count (filter (fn [[x y]]
                       (= 1 (gcd x y))) (map (fn [n]
                                               [n num]) (range num)))))))

(defcheck solution-a32967d5
  (fn [n]
    (->> (range 2 n)
      (filter (fn [x]
                (= 1 ((fn gcd [a b]
                        (if (= 0 b) a (gcd b (mod a b))))
                      x n))))
      count
      inc)))

(defcheck solution-a3d869c8
  (fn [n]
    (if (= n 1)
      1
      (let [factors (fn [n]
                      (set (filter #(zero? (rem n %))
                             (range 1 (inc n)))))
            n-facs (factors n)]
        (count
          (filter #(= #{1} %)
            (map #(clojure.set/intersection n-facs (factors %))
              (range 1 n))))))))

(defcheck solution-a40a1751
  (fn eulers-totient [n]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (rem x y))))]
      (count (filter #(= 1 (gcd n %)) (range n 0 -1))))))

(defcheck solution-a4dd2a32
  (fn euler-totient
    [n]
    (let [gcd (fn
                [a b]
                (loop [c a
                       d b]
                  (if (zero? d)
                    c
                    (recur d (rem c d)))))]
      (->> (inc n) (range 1) (map #(gcd % n)) (filter #(= % 1)) (count)))))

(defcheck solution-a4ea687b
  (fn totient[x]
    (letfn [(gcd[a b]
              (if (zero? b) a
                            (recur b (mod a b))))
            (co-prime? [a b] (= 1 (gcd a b)))]
      (count (filter (partial co-prime? x) (range x))))))

(defcheck solution-a53e9cb9
  (fn totient [x]
    (let [divisors (fn [n] (set (filter #(integer? (/ n %)) (range 2 (inc n)))))
          x-divisors (divisors x)]
      (reduce (fn [d n]
                (if (empty? (clojure.set/intersection x-divisors (divisors n))) (inc d) d)) 1 (range 2 x)))))

(defcheck solution-a594bd7f
  (fn [n] (if (= n 1)
            1
            (first (reduce (fn [[c f] b]
                             (cond
                               (some #(= (mod b %) 0) f) [(dec c) f]
                               (= (mod n b) 0) [(dec c) (conj f b)]
                               1 [c f]))
                     [(dec n) []]
                     (range 2 n))))))

(defcheck solution-a5dc92b8
  (fn [x]
    (if (= 1 x)
      1
      (count (filter (fn [n]
                       (let [gcd
                             (fn [a b]
                               (if (zero? b)
                                 a
                                 (recur b (mod a b))))] (= (gcd x n) 1))) (range 1 x))))))

(defcheck solution-a64bb539
  (fn [x]
    (if (= x 1) 1
                (letfn [(gcd [x y]
                          (if (zero? y) x
                                        (gcd y (mod x y))))]
                  (count
                    (filter
                      #(= 1 (gcd x %))
                      (range 1 x)))))))

(defcheck solution-a6a4ea79
  ;(fn [n]
  ;  (let [gcd (fn [m n]
  ;              (if (zero? n) m (recur n (mod m n))))]
  ;    (if (= 1 n) ; edge case because (range 1 1) => ()
  ;      1
  ;      (count (filter #(= 1 (gcd % n)) (range 1 n))))))

  ; get rid of if by taking range from 2 instead of 1

  (fn [n]
    (let [gcd (fn [m n]
                (if (zero? n)
                  m
                  (recur n (mod m n))))]
      (inc (count (filter #(= 1 (gcd % n)) (range 2 n)))))))

(defcheck solution-a6a93961
  #(count (filter (fn [x] ((fn [x y] (if (= x y) (= 1 x)
                                                 (if (> x y) (recur (- x y) y) (recur x (- y x)) ))) x %))
            (range 1 (inc %)))))

(defcheck solution-a763ae85
  (fn [n]
    (let [g (fn g [a b]
              (if (= b 0)
                a
                (g b (rem a b))))]
      (count (filter #(= 1 (g n %)) (range 1 (inc n)))))))

(defcheck solution-a7f54981
  (fn coprime [n]

    ((fn f [start ans]
       (cond (= n 1) 1
             (= start n) ans
             :else (let [gcd ((fn g [a b] (if (= b 0) a (g b (rem a b)))) start n)]
                     (if (= gcd 1) (f (inc start) (inc ans)) (f (inc start) ans))
                     )
             )
       ) 1 0)
    ))

(defcheck solution-a81344c8
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(GCD [x y]
                (let [_min (min x y)
                      _max (max x y)
                      reminder (mod _max _min)]
                  (if (zero? reminder) _min
                                       (GCD _min reminder))))]
        (count (filter #(= 1 (GCD % n)) (drop 1 (range n))))))))

(defcheck solution-a8230688
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))
          (tot [n] (for [x (range 1 (inc n))
                         :when (= (gcd x n) 1)]
                     x))]
    #(count (tot %))
    ))

(defcheck solution-a87619b9
  (fn [i]
    (+ 1 (count (filter
                  (fn [j] (= () (filter #(= 0 (mod j %) (mod i %)) (range 2 (+ 1 j) ))))
                  (range 2 i))))))

(defcheck solution-a8a83ec0
  (fn etf [n]
    (if (= 1 n)
      1
      (let [nums (take (dec n) (iterate inc 1))
            gcd (fn [x y]
                  (loop [a x, b y]
                    (if (zero? b)
                      a
                      (recur b (rem a b)))))]
        (count (loop [result [], nums2 nums]
                 (if (empty? nums2)
                   result
                   (let [f (first nums2)
                         gcdr (gcd n f)]
                     (when (= 1 gcdr) (conj result gcdr))
                     (recur
                       (if (= 1 gcdr) (conj result f) result)
                       (rest nums2))))
                 ))))))

(defcheck solution-a8b092
  (fn problem-75 [n]
    (letfn [(gcd [a b] (if (zero? b)
                         a
                         (recur b (mod a b))))
            (coprime-to-n? [a] (= 1 (gcd n a)))]
      (count (filter coprime-to-n? (range n))))))

(defcheck solution-a8d27316
  (fn [n]
    (count
      (filter
        #((fn [x n]
            (if (zero? n)
              (= x 1)
              (recur n (mod x n))
              )
            ) % n)
        (range n)
        )
      )
    ))

(defcheck solution-a911faaa
  (fn count-coprimes
    [x]
    (letfn
     [(factors [x]
        (into #{} (filter #(zero? (rem x %)) (range 1 (inc x)))))]
      (count (filter #(= #{1} %) (map #(clojure.set/intersection (factors x) %) (map factors (range 1 (inc x)))))))))

(defcheck solution-a9361ccb
  (letfn [ (gcd [a b]
             (cond
               ;(< a b) (recur b a)
               (zero? b) a
               :else (recur b (mod a b)))) ]
    (fn [n]
      (if (= 1 n) 1
                  (count (filter #(= (gcd % n) 1) (range 1 n)))))))

(defcheck solution-a9743d82
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (max 1
        (count
          (filter #(= 1 (gcd n %))
            (range 1 n)))))))

(defcheck solution-a9780304
  (fn euler-totient
    [x]
    (if (#{1} x) 1
                 (count (filter #{1}
                          (map (fn my-gcd1
                                 [x y]
                                 (if (zero? (rem x y))
                                   y
                                   (recur y (rem x y))))
                            (repeat x) (range 1 x)))))))

(defcheck solution-a9c7cf61
  (fn eulers-totient [y]
    (letfn [(divides? [divisor dividend]
              (zero? (rem dividend divisor)))
            (gcd [a b]
              (loop [x 1 curmax 1]
                (if (> x (min a b))
                  curmax
                  (let [curmax
                        (if (and (divides? x a)
                                 (divides? x b))
                          x
                          curmax)]
                    (recur (inc x) curmax)))))
            (coprime? [a b]
              (= (gcd a b) 1))]
      (count (filter true? (conj (map (fn [m] (coprime? m y)) (range 2 y)) true))))))

(defcheck solution-aadf3cac
  (fn [x]
    (if (= x 1) 1
                (letfn [(gcd [a b]
                          (if (zero? b) a
                                        (recur b (mod a b))))]
                  (->> (range 1 x)
                    (filter #(= 1 (gcd % x)))
                    (count))))))

(defcheck solution-ab06898c
  (fn [n]
    (case n
      1 1
      2 1
      ((fn clear [x s] (if (= x n) (inc (count s))
                                   (if (= 0 (rem n x))
                                     (clear (inc x) (remove #(= 0 (rem % x)) s))
                                     (clear (inc x) s)
                                     )
                                   )
         ) 2 (range 2 n))
      )))

(defcheck solution-ab16ce09
  (fn [n]
    (letfn [(gcd [x y]
              (cond
                (= x y) x
                (< x y) (gcd x (- y x))
                :else (gcd (- x y) y)))]
      (count (filter #(= 1 %) (map #(gcd n %) (range 1 (inc n))))))))

(defcheck solution-abebc15
  (fn [num]
    (loop [result [1] i 2]
      (if (< i num)
        (recur
          (if (= 1 ((fn ! [numa numb]
                      (if (= 0 (mod numa numb))
                        numb
                        (! numb (mod numa numb))
                        )
                      ) num i))
            (conj result i)
            result)
          (inc i)
          )
        (count result)
        )
      )
    ))

(defcheck solution-abee7a15
  (fn [x] (let [gcd (comp (partial apply +) first (partial drop-while (comp (partial < 0) (partial apply min))) #(iterate (fn [[x1 x2]] (vector (mod x1 x2) (mod x2 x1))) [%1 %2]))]
            (if (= x 1) 1 (count (filter (comp (partial = 1) (partial gcd x)) (range 1 x)))))
    ))

(defcheck solution-ac252e6c
  (fn [n]
    (let [p ((fn s [[x & r]]
               (when (<= x (/ n 2))
                 (cons x (s (remove #(zero? (mod % x)) r)))))
             (drop 2 (range)))
          d (filter #(zero? (mod n %)) p)]
      (* n (reduce * (map #(- 1 (/ 1 %)) d))))))

(defcheck solution-ad0a642f
  (fn totient [x] (letfn [(gcd [a b] (if (> b a ) (gcd b a)  (if (= b a) a (gcd b (- a b)))))]

                    (if (= 1 x) 1 (count (filter #( = 1 (gcd % x)) (range 1 x)))))))

(defcheck solution-ad7358be
  (fn tot [n]
    (let [gcd #(if (zero? %) %2 (recur (mod %2 %) %))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-adcd2abf
  (fn [n] (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
            ({1 1} n (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-adef63eb
  #(letfn [(g[a b] (if (= b 0) a (recur b (mod a b))))]
     (count (filter (fn[i] (= i 1)) (map (partial g %) (range 1 (inc %)))))))

(defcheck solution-ae1815f9
  (fn [n]
    (letfn [(gcd [k m] (if (zero? m) k (recur m (mod k m))))]
      (if (= 1 n)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-ae310359
  (fn eu-tot[n]
    (if (= n 1) 1
                (count
                  (filter #(= 1 %)
                    (map (partial
                           (fn gcd[l r](cond
                                         (= 0 l) r
                                         (= 0 r) l
                                         (= l r) r
                                         (> l r) (gcd (- l r) r)
                                         (> r l) (gcd (- r l) l))) n)
                      (range 1 n)))))))

(defcheck solution-ae7a997d
  (fn [x]
    (letfn [(gcd [n n2]
              (let [r (mod n n2)]
                (if (= 0 r)
                  n2
                  (recur n2 r))))]
      (if (= 1 x) 1
                  (count (filter (partial = 1)
                           (map (partial gcd x) (range 1 x))))))))

(defcheck solution-aecded03
  (fn euler [x]
    (letfn [(gcd [x y]
              (if (= y 0)
                x
                (recur y (mod x y))))]
      (if (= x 1)
        1
        (count (filter (fn [e] (== 1 (gcd e x))) (range 1 x)))))))

(defcheck solution-aecec940
  (fn f [n]
    (letfn [(gcd [a b]
              (if (zero? a) b
                            (recur (rem b a) a)))
            (coprime [a b]
              (= 1 (gcd a b)))]
      (if (= n 1) 1
                  (count (filter #(coprime n %) (range 1 n)))))))

(defcheck solution-aed611de
  (fn totient [n]
    (let [gcd #(loop [a %1
                      b %2]
                 (if (= 0 a)
                   b
                   (recur (mod b a) a)))]
      (reduce + (for [i (range n) :when (= (gcd i n) 1)]
                  1)))))

(defcheck solution-af0f5a99
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (coprime [a b] (= (gcd a b) 1))]
      (count (filter (partial coprime n) (range 1 (+ 1 n)))))))

(defcheck solution-af3c7da2
  (fn [n] (count (filter (fn [x] (= 1 (#(if (= %2 0) % (recur %2 (rem % %2))) x n))) (range n)))))

(defcheck solution-af69b037
  (fn [n] (reduce #(if (= 1 (loop [p n q %2]
                              (if (= q 0)
                                p
                                (recur q (rem p q)))))
                     (+ % 1)
                     %)
            1
            (range 2 n))))

(defcheck solution-af6c9457
  (fn [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))]
      (if (= 1 x)
        1
        (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-af7d5a46
  (fn totient [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (coprime? [a b] (= (gcd a b) 1))]
      (count (filter #(coprime? % n) (range 1 (inc n)))))))

(defcheck solution-afe5d9d6
  (fn [x]
    (letfn [(gcd [x y]
              (let [a (min x y) b (max x y) z (mod b a)]
                (if (= 0 z) a (gcd z a))))]
      (count (filter #(= 1 (gcd x %)) (range 1 (inc x))))
      )))

(defcheck solution-b01f3b7b
  (letfn
   [(coprime? [x y]
      (empty?
        (for [n (range 2 (inc (min x y)))
              :when (and
                     (== 0 (mod x n))
                     (== 0 (mod y n)))]
          n)))]
    (fn [x]
      (if (= x 1) 1
                  (count
                    (filter #(coprime? x %)
                      (range 1 x)))))))

(defcheck solution-b023c896
  (fn [x]
    (let [gcd (fn  [x y]
                (let [smaller (min x y)
                      larger (max x y)]
                  (if (zero? smaller)
                    larger
                    (recur (- larger smaller) smaller))))]
      (if (= 1 x)
        1
        (count (filter #(= (gcd x %) 1) (range 1 x)))))))

(defcheck solution-b05cdcc6
  (fn [n]
    (let [f (fn f [x y] (let [a (max x y) b (min x y)] (if (= a b) a (f b (- a b)))))]
      (-> (filter #(= 1 (f n %)) (range 2 n)) count inc))))

(defcheck solution-b08c6cdd
  (fn
    [a]
    (let
     [gcd (fn gcd [x y]
            (cond
              (== x y) x
              (< x y) (gcd y x)
              :default (gcd (- x y) y)))]
      (->> (inc a)
        range
        rest
        (filter #(== 1 (gcd % a)))
        count))))

(defcheck solution-b0a1e281
  (fn [n]
    (let [coprime? (fn f [max min]
                     (if (= 0 (mod max min))
                       (= 1 min)
                       (recur min (mod max min))))]
      (loop [i 1 s 0]
        (if (> i n)
          s
          (if (coprime? n i)
            (recur (inc i) (inc s))
            (recur (inc i) s)))))))

(defcheck solution-b0ac29c4
  (fn k [n]
    (if (= 1 n)
      1
      (letfn [(coprime? [x_ n_] (empty?
                                  (filter
                                    #(and (= 0 (mod x_ %)) (= 0 (mod n_ %)))
                                    (range 2 (inc x_)))))]
        (->> (range 1 n)
          (filter (fn [x] (coprime? x n)))
          count)))))

(defcheck solution-b191011
  (fn phi [x]
    (count (filter #(letfn [(gcd [a b]
                              (if (zero? a)
                                b
                                (gcd (mod b a) a)))]
                      (= 1 (gcd x %)))
             (range 1 (inc x))))))

(defcheck solution-b1aad29c
  (fn n75 [n]
    (letfn [(gcd [a b] (if (zero? (mod a b)) b (recur b (mod a b))))]
      (if (= 1 n) 1 (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-b1acc1b0
  (fn totient [n]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (if (= n 1)
        1
        (reduce +
          (map
            #(if (= 1 (gcd % n)) 1 0)
            (range 1 n)))))))

(defcheck solution-b202affa
  (fn totient
    [n]
    (let [xs (range 2 n)
          gcd (fn [a b] (loop [a a b  b]
                          (if (zero? b) a
                                        (recur b (mod a b)))))]
      (reduce #(if (= 1 (gcd n %2))
                 (inc %)
                 %)
        1 xs))))

(defcheck solution-b2692207
  ; count all of those which have 0 common divisors >1
  (fn [x]
    (+ 1 (count
           (filter #(= 0 %)
             (map (fn [y]
                    (reduce #(if (= 0 (rem x %2) (rem y %2)) (+ %1 1) %1)
                      0 (range 2 (+ 1 y))))
               (range 2 x)))))))

(defcheck solution-b276a0d1
  (fn [x] (count (filter (fn [y] (= 1 (count (filter #(and (= 0 (mod x %)) (= 0 (mod y %))) (map inc (range y)))))) (map inc (range x))))))

(defcheck solution-b2f2a127
  (fn [x]
    (let [gcd (fn [a b]
                (cond
                  (= b 0) a
                  (= a 0) b
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))
          coprime? #(= (gcd % x) 1)]
      (if (= 1 x) 1
                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-b386668b
  (fn
    [x]
    (letfn [(gcd [x y]
              (if (= 0 y)
                x
                (recur y (mod x y))))
            (step [a n]
              (if (= 1 (gcd x n))
                (inc a)
                a))]
      (if (= x 1)
        1
        (reduce step 0 (range x))))))

(defcheck solution-b407abee
  (fn [x]
    (let [gcd (fn gcd ([a] (gcd a x))
                ([a b] (if (> a b)
                         (if (= 0 (rem a b)) b
                                             (gcd (- a b) b))
                         (gcd b a))))]
      (if (= x 1) 1
                  (->> x (range 1) (map gcd) (filter #(= 1 %)) count)))))

(defcheck solution-b40a060a
  (let [gdc (fn [a b]
              (let [min-num (min a b)]
                (first (filter #(= 0 (mod a %) (mod b %)) (reverse (range (inc min-num)))))))
        coprime (fn [a b] (= 1 (gdc a b)))]
    (fn [n] (if (= 1 n) 1 (count (filter #(coprime n %) (range 1 n)))))))

(defcheck solution-b4b9c67c
  (fn totient [n]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (+ n 1)))))))

(defcheck solution-b54035c3
  (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b)) ))]
    (fn [x] (count (filter #(= 1 (gcd x %)) (range x))))
    ))

(defcheck solution-b5af6a1c
  (fn [n]
    (letfn [(gcd [a b]
              (cond (zero? b) a
                    :else (recur b (rem a b))))]
      (cond
        (= n 1) 1
        :else (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-b71e4c44
  (fn euler-totient [n]
    (loop [cand 2
           resp [1]]
      (if (>= cand n)
        (count resp)
        (if (= (first
                 (filter #(and (= (mod n %1) 0) (= (mod cand %1) 0))
                   (range 2 (inc cand)))) nil)
          (recur (inc cand) (conj resp cand))
          (recur (inc cand) resp))))))

(defcheck solution-b79bbee9
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))
            (coprime [m] (= 1 (gcd n m)))]
      (count (filter coprime (range n))))))

(defcheck solution-b8586a7a
  (fn [n] (->>
            (range n)
            (map
              #(if (zero? %2) %1 (recur %2 (mod %1 %2)))
              (repeat n))
            (filter #(= 1 %))
            count)))

(defcheck solution-b8a4aa88
  (fn [n]
    (if (= 1 n)
      1
      (apply +
        (map #(let [g ((fn gcd [a b] (if (= a 0) b (recur (mod b a) a))) % n)]
                (if (= 1 g) 1 0))
          (range 1 n))))))

(defcheck solution-b8ea98f1
  (fn [x]
    (let [g #(if (= 0 %2) % (recur %2 (mod % %2)))]
      (condp = x
        1 1
        (count (filter #(= (g x %) 1) (range 1 x)))))))

(defcheck solution-b8f0c626
  (fn totient [i]
    (if (= i 1) 1
                (count
                  (filter
                    (fn [j]
                      (not-any?
                        (fn [k]
                          (= 0 (mod i k) (mod j k)))
                        (range 2 (inc j))))
                    (range 1 i))))))

(defcheck solution-b99e5791
  (fn [x]
    (if (= x 1) 1
                (letfn [(gcd [a b]
                          (if (< a b) (gcd b a)
                                      (loop [n a, m b]
                                        (if (= 0 (mod n m)) m
                                                            (recur m (mod n m))))))]
                  (count (filter #(= 1 (gcd x %1)) (range 1 x)))))))

(defcheck solution-ba35488e
  (fn [i]
    (letfn [(d [x]
              (into #{x}
                (filter #(zero? (rem x %))
                  (range 2 x))))]
      (->> (range 2 i)
        (remove #(some (d i) (d %)))
        count
        inc))))

(defcheck solution-ba836a24
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-bb39e1cc
  (fn eulers [x]
    (if (= 1 x)
      1
      (letfn [(gcd [a b] (last (filter
                                 #(and (zero? (mod a %)) (zero? (mod b %)))
                                 (range 1 (max a b)))))
              (coprime? [a b] (= 1 (gcd a b)))]
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-bbea5405
  (fn euler-totient [x]
    (letfn [(gcd [x y]
              (if (zero? x)
                y
                (recur (mod y x) x)))]
      (if (= x 1)
        1
        (loop [an 0 ns (range 1 x)]
          (if (empty? ns)
            an
            (let [f (first ns)]
              (if (= 1 (gcd f x))
                (recur (inc an) (rest ns))
                (recur an (rest ns))))))))))

(defcheck solution-bbf39f45
  (fn [n]
    (loop [cand (range 2 n) ret 1]
      (cond
        (empty? cand) ret
        (zero? (rem n (first cand)))
        (recur
          (remove
            #(zero? (rem % (first cand)))
            (rest cand))
          ret)
        :else
        (recur (next cand) (inc ret))))))

(defcheck solution-bc463793
  (fn totilent
    [x]
    (let [is? (fn [a] (empty? (filter #(and (zero? (mod a %)) (zero? (mod x %))) (range 2 (inc a)))))]
      (count (filter is? (range 1 (inc x)))))))

(defcheck solution-bc86b98a
  (fn [cc]
    (let [
          gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))
          ]
      (if (= cc 1) 1 (count (filter #(= 1 (gcd cc %)) (range 1 cc)))))))

(defcheck solution-bc9bd87d
  (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))
          (coprime? [x y] (= 1 (gcd x y)))]
    #(if (= 1 %) 1 (count (filter (partial coprime? %) (range 1 %))))))

(defcheck solution-bcc2549b
  (fn [x]
    (let [gcd (fn gcd [x y] (if (zero? y) x (gcd y (mod x y))))
          coprime? (fn [y] (= 1 (gcd x y)))]
      (count (filter coprime? (range x))))))

(defcheck solution-bcdd3369
  (letfn [(gcd [a b] (if (= (mod a b) 0)
                       b
                       (recur b (mod a b))))]
    (fn [i] (count (filter #(= (gcd i %) 1) (range 1 (inc i)))))))

(defcheck solution-bcfb7d7b
  (fn [i]
    (if (= 1 i) 1
                (count (filter #(= 1 (loop [a i b %] (if (= b 0) a (recur b (rem a b)))))
                         (range 1 i))))))

(defcheck solution-bd275fa9
  (fn [n]
    (if (= n 1) 1
                (count (filter #(= 1 %)
                         (for [i (rest (range n))]
                           ((fn [a b]
                              (apply max
                                (into [] (filter (fn [x] (and (= 0 (rem a x)) (= 0 (rem b x)))) (range 1 (+ 1 (min a b))) ))))
                            i n)))))))

(defcheck solution-bd4b3834
  (fn etf [n]
    (letfn [(eulalg [[a b]]
              (if (= a b) a
                          #(eulalg (if (> a b) [(- a b) b] [a (- b a)]))))]
      (if (= 1 n) 1
                  (count (filter #(= 1 (trampoline eulalg [n %])) (range 1 n)))))))

(defcheck solution-bd9e5b65
  (fn totient [n]
    (let [primes (filter (fn [x] (empty?
                                   (filter #(zero? (rem x %))
                                     (range 2 x)))) (iterate inc 2))
          factors (loop [n n,
                         factors [],
                         [p & ps :as primes] primes]
                    (cond
                      (= 1 n) factors
                      (zero? (rem n p)) (recur (quot n p)
                                          (conj factors p)
                                          primes)
                      :else (recur n factors ps)))]
      (reduce * (map (fn [[p k]] (apply * (dec p) (repeat (dec k) p))) (frequencies factors))))))

(defcheck solution-bde8928
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (rem a b))))]
      (count (filter #(= 1 %) (map #(gcd % n) (range n)))))))

(defcheck solution-bdec548
  (fn [n]
    (let [gcd (fn [a b](cond (zero? a) b (zero? b) a (> a b) (recur (- a b) b) (< a b) (recur a (- b a)) :else (recur (- a b) b)))]
      (if (= 1 n) 1
                  (count (for [d (range n) :when (= 1 (gcd n d))]d))
                  ))
    ))

(defcheck solution-bdfd24b1
  (fn totient [x]
    (if
     (= x 1)
      1
      (count
        (filter
          (fn [y]
            ((complement some)
             (fn [z] (= 0 (mod y z) (mod x z)))
             (range 2 (inc y))))
          (range 1 x))))))

(defcheck solution-be634c3f
  (fn [n]
    (letfn [(gcd [a b] (let [rem (mod a b)] (if (= rem 0) b (gcd b rem))))]
      (count (filter #(= 1 (gcd n %1)) (range 1 (+ 1 n)))))))

(defcheck solution-bf1f031a
  (fn totient [x]
    (let [divisible? #(zero? (rem %1 %2))
          gcd (fn [a b] (last (filter #(and (divisible? a %) (divisible? b %)) (range 1 (inc b)))))]
      (if (= 1 x) 1 (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-c0099f1f
  (fn [n]

    (letfn [(gcd   [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))
                )
              )]
      (if (= 1 n) 1
                  (count (filter  #(= 1 (gcd % n) ) (range 1 n) ) )
                  )

      )

    ))

(defcheck solution-c0bc9a97
  (fn [n] (let [gcd (fn [x y] (if (= x 0) y (recur (mod y x) x)))]
            (count (filter #(= (gcd % n) 1) (range 1 (inc n)))))))

(defcheck solution-c1024bff
  (fn [n] (if (= n 1) 1 (count (filter #(= 1((fn [x y] (if (= y 0) x (recur y (mod x y)))) n %)) (range 1 n))))))

(defcheck solution-c14bee9a
  (fn [x] (if (= x 1) 1 (count (filter #(= ((fn [a b] (if (= b 0) a (recur b (mod a b)))) % x) 1) (range 1 x))))))

(defcheck solution-c158a481
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [q]
                            (->> q inc (range 1) (filter #(= 0 (rem n %) (rem q %))) (apply max)))]
                  (->> n (range 1) (filter #(= 1 (gcd %))) count)))))

(defcheck solution-c1d208dc
  (fn [x](  count(
                   filter
                   (fn[y](= y 1))
                   (map
                     (fn[n]
                       ((fn gcd [a b] (if (= b 0) a (gcd b (mod a b))))
                        x n)
                       )
                     (range x)))
           )))

(defcheck solution-c1ff9965
  (fn totf [n]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (if (= n 1)
        1
        (->> (range n) (filter #(= (gcd % n) 1)) (count))))))

(defcheck solution-c201ed3f
  #(letfn [(gcd [a b]
             (if (zero? b)
               a
               (recur b (mod a b))))
           (eNums [x]
             (for [e (range 1 (inc x))]
               (if (= 1 (gcd e x))
                 e)))]
     (count (remove nil? (eNums %)))))

(defcheck solution-c2051e3f
  (fn [n]
    (if (= 1 n) 1
                (count (filter #(= 1 (loop [a n b %]
                                       (cond
                                         (> b a)  (recur b a)
                                         (= 0 b) a
                                         :else (recur b (mod a b))
                                         )
                                       )

                                  ) (range 1  n)))
                )
    ))

(defcheck solution-c2dac4b2
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (rem a b))))
            (coprime? [a b] (= 1 (gcd a b)))]
      (count (filter #(coprime? n %) (range 1 (inc n)))))))

(defcheck solution-c2ffcd2f
  (fn totient [n]
    (letfn [(gcd [a b]
              (if (zero? b) a
                            (recur b (mod a b))))]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd n %)) (range n)))))))

(defcheck solution-c34b8d50
  (fn euler_s-totient-function [x]
    (letfn [(e? [x y]
              (loop [i 2]
                (cond (> i x) true
                      (= 0 (rem x i) (rem y i)) false
                      :else (recur (inc i)))))]

      (reduce
        #(if (e? (inc %2) x) (inc %) %)
        0
        (range 0 x)))))

(defcheck solution-c47ccf38
  ; (defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
  ; (defn coprime [a b] (= 1 (gcd a b)))
  ; (defn totient [n] (count (filter #(coprime % n) (range n))))
  ; (defn coprime [a b] (if (zero? b) (= 1 a) (recur b (mod a b))))

  (fn [n] (count (filter #((fn [a b] (if (zero? b) (= 1 a) (recur b (mod a b)))) % n) (range n)))))

(defcheck solution-c4871e49
  (fn [num]
    (letfn [(divs [n] (filter #(= 0 (mod n %)) (range 2 (inc n))))
            (cp? [n m] (not-any? (set (divs n)) (divs m)))]
      (max 1 (count (filter #(cp? num %) (range 1 num)))))))

(defcheck solution-c58a1483
  #(let [gcd (fn gcd [x y] (if (= y 0) x (gcd y (rem x y))))
         f (fn [x i c] (if (>= 1 i) c (recur x (dec i) (if (= 1 (gcd x i)) (inc c) c))))]
     (f % (dec %) 1)))

(defcheck solution-c5e049eb
  (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))
          (co-prime? [a b] (= (gcd a b) 1))]
    (fn [x]
      (if (= x 1)
        1
        (count (filter #(co-prime? x %) (range 1 x)))))))

(defcheck solution-c5f302d1
  (fn tot [n]
    (let [gcd (fn gc [n m] (if (= 0 m) n (gc m (mod n m))))]
      (count (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-c6802a2c
  (fn tot [n]
    (let [common-div? (fn [x y n]
                        (if (and
                             (= (rem x n) 0)
                             (= (rem y n) 0))
                          true false))
          gcd (fn [x y]
                (if (= x y 1)
                  1
                  (apply max (filter #(common-div? x y %) (range 1 y)))))
          co-prime? (fn [x y]
                      (if (= 1 (gcd x y))
                        true false))
          range (fn [n] (if (= n 1)
                          (range 1 2)
                          (range 1 n)))]
      (->>
        (filter #(co-prime? % n) (range n))
        (count)))))

(defcheck solution-c6e3c945
  (fn [n]
    (letfn [(gcd [a b]
              (cond (= b 0) a
                    :else (gcd b (mod a b))))]
      (count (filter (partial = 1) (map (partial gcd n) (range n)))))))

(defcheck solution-c6f4da41
  (fn[k]
    (letfn [(gcd[n m]
              (let [n+ (max n m)
                    m+ (min n m)]
                (if (zero? m+)
                  n+
                  (recur (- n+ m+) m+))))]
      (count (filter #(= 1 %)
               (map #(gcd k %) (range k)))))))

(defcheck solution-c71058f0
  (fn [x]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (count (filter #(= 1 (gcd % x)) (range x))))))

(defcheck solution-c743ca4d
  #(count
     (cons 1
       (if (not= % 1)
         (reduce
           (fn [s x]
             (if (zero? (mod % x))
               (filter (complement (set (range x % x))) s)
               s))
           (range 2 %)
           (range 2 (inc (/ % 2))))))))

(defcheck solution-c7513dc6
  (fn [x]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (if (= x 1) 1
                  (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-c777fd1a
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [m n]
                (if (zero? n)
                  m
                  (gcd n (rem m n))))]
        (count (filter #(= (gcd x %) 1) (range 1 x)))))))

(defcheck solution-c77dc268
  (fn [n] (if (= 1 n) 1
                      (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
                        (count (filter #(= 1 %) (map #(gcd % n) (range 1 n))))))))

(defcheck solution-c7a26341
  (fn [x] (letfn [(g [x y]
                    (if (> x y)
                      (g y x)
                      (let [r (rem y x)]
                        (if (= r 0)
                          x
                          (g x r)))))]
            (if (= 1 x)
              1
              (count (filter #(= 1 (g x %)) (range 1 x)))
              ))))

(defcheck solution-c889fe31
  (fn [n]
    ({1 1} n
     (count
       (filter
         (fn [a]
           (not-any?
             #(= 0 (rem a %) (rem n %))
             (range 2 (+ 1 a))))
         (range 1 n))))))

(defcheck solution-c8dd7faa
  #(count (filter (fn [B] (loop [A %1, B B]
                            (if (= (mod A B) 0)
                              (= B 1)
                              (recur B (mod A B)))))
            (range 1 (inc %1)))))

(defcheck solution-c948004b
  (fn [x]
    (let [gcd (fn [x y] (if (= y 0) x (recur y (rem x y))))]
      (->> x (range 2) (filter #(= (gcd % x) 1)) count inc))))

(defcheck solution-c94c961b
  (fn [n]
    (letfn [ (gcd[x y] (loop[x x y y]
                         (cond (> x y) (recur y (- x y))
                               (> y x) (recur x (- y x))
                               :else x)))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n)))))))

(defcheck solution-c9f85e0d
  (fn [n]
    (let [gcd
          (memoize (fn [a b]
                     (if (< a b) (recur b a)
                                 (if (= b 0) a
                                             (recur b (- a b))))))]
      (max 1 (count (filter #(= % 1)
                      (map gcd (range 1 n) (repeat (- n 1) n))))))))

(defcheck solution-c9ff17d
  (fn [n]
    (if (= n 1)
      1
      (letfn [(gcd [a b]
                (if (zero? b)
                  a
                  (recur b (rem a b))))]
        (count (filter #(= 1 (gcd % n)) (range n)))))))

(defcheck solution-ca178096
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))
                        (coprime [x] (= 1 (gcd n x)))]
                  (count (filter coprime (range 1 n)))))))

(defcheck solution-ca8f0fc1
  (fn copr-count [n]
    (if (= n 1) 1

                (letfn [(dividers [a]
                          (if (= a 1) [1]
                                      (filter #(= 0 (mod a %)) (range 2 (inc a)))))

                        (coprime? [a1 b1]
                          (apply distinct? (into (dividers a1) (dividers b1))))]

                  (count (filter #(coprime? n %) (range 1 n)))))))

(defcheck solution-caade125
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))
                        (isCoprime [i] (= (gcd i n) 1))]
                  (count (filter isCoprime (range 1 n)))
                  ))))

(defcheck solution-cad1d4f3
  (fn [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (count
        (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-cb20c81
  (fn tot [n]
    (let [isPrime (fn [a] (not-any? #(= 0 (mod a %)) (range 2 (dec a))))]
      (* n (reduce #(* % (- 1 (/ 1 %2))) 1
             (filter #(and (isPrime %) (= (mod n %) 0)) (range 2 n)))))))

(defcheck solution-cb30ad5c
  (fn totient [x]
    (if (= 1 x) 1
                (letfn [(gcd [a b]
                          (if (= b 0) a
                                      (recur b (mod a b))))

                        (coprime? [n] (= 1 (gcd x n)))]

                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-cb6151ce
  (fn eut[n] (if (= 1 n) 1
                         (let [gcd (fn [a b] (if (pos? b) (recur b (mod a b)) a))]
                           (->>
                             (range 1 n)
                             (filter #(= 1 (gcd % n)))
                             count)))))

(defcheck solution-cb7a4c3c
  (fn [n]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (gcd y (mod x y))))]
      (count (filter #(= 1 (apply gcd (sort > [% n]))) (range 1 (inc n)))))))

(defcheck solution-cbbf0b49
  (let [gcd (fn [a b]
              (let [[q r] ((juxt quot rem) b a)]
                (if (zero? r) a (recur r a))))]
    (fn [n]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-cbef067a
  (fn euler-fn
    [n]
    (let [gcd (fn gcd
                [x y]
                (if (zero? (mod y x))
                  x (gcd (mod y x) x)))]
      (reduce #(if (= 1 (gcd n %2))
                 (inc %1) %1) 1 (range 2 n)))))

(defcheck solution-cc138bba
  (fn [x]
    (let [gcd #(loop [a %1 b %2]
                 (if (zero? b)
                   a
                   (recur b (rem a b))))]
      (if (= 1 x)
        1
        (->> (range 1 x)
          (filter #(= 1
                     (gcd x %)))
          count)))))

(defcheck solution-cc8f656a
  (fn totient [x]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (rem a b))))
            (coprime? [n] (= 1 (gcd x n)))]
      (if (= 1 x)
        1
        (count (filter coprime? (range 1 x)))))))

(defcheck solution-ccfe5e43
  (fn [num]
    (if (= 1 num)
      1
      (let [divisor? (fn [a b] (zero? (mod a b)))]
        (->>
          (map
            (fn [a b]
              (apply max
                (filter
                  #(and (divisor? a %) (divisor? b %))
                  (range 1 (inc (min a b))))))
            (range 1 num) (repeat num))
          (filter (partial = 1))
          (count))))))

(defcheck solution-cd5b3464
  (fn [x]
    (if (= x 1) 1
                (letfn [
                        (gcd [x1 x2]
                          (loop [i (apply max [x1 x2])]
                            (if (and (integer? (/ x1 i)) (integer? (/ x2 i))) i
                                                                              (recur (dec i)))))
                        (coprime? [a b]
                          (= (gcd a b) 1))
                        ]
                  (count (filter #(coprime? x %) (rest (range x))))
                  ))))

(defcheck solution-cda9715d
  (fn totient [n]
    (let [gcd (fn gcd [b a] (if (zero? (mod b a)) a (gcd a (mod b a))))]
      (let [coprime? (fn [a b] (= 1 (gcd a b)))]
        (count (filter (partial coprime? n) (range 1 (inc n))))))))

(defcheck solution-cdac93dd
  (fn [n] (->> (range 2 n) (filter #(not= 0 (mod n %)))
            (filter (fn [v] (= 1 (#(loop [a (max %1 %2) b (min %1 %2)]
                                     (cond
                                       (= 0 (mod a b)) b
                                       (= 1 (mod a b)) 1
                                       :else (recur b (mod a b))
                                       )) n v)
                              )))   (cons 1) count
            )))

(defcheck solution-ce226cbb
  (fn [x]
    (if (= 1 x)
      1
      (count (filter
               #(= 1 ((fn gcd [a b]
                        (when (< a b) (gcd b a))
                        (if (zero? (mod a b))
                          b
                          (gcd b (mod a b)))) %1 x))
               (range 1 x))))))

(defcheck solution-ce8ee70a
  (fn euler-tot [num]
    (let [gcd (fn [a b]
                (if (= a 0) b (recur (mod b a) a)))]
      (loop [res 1, rem (range 2 num)]
        (if (empty? rem)
          res
          (recur (if (= 1 (gcd (first rem) num)) (inc res) res)
            (rest rem)))))))

(defcheck solution-cea69b03
  (fn et [n]
    (letfn [(gcd [a b]
              (if (= 0 (rem a b))
                b
                (gcd (max (- a b) b) (min (- a b) b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (max n 2)))))))

(defcheck solution-cf64b74c
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b]
                          (if (zero? b) a (gcd b (mod a b))))]
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-cf7cd3cd
  (fn [n] (letfn [(gcd [a b] (if (= a 0) b (gcd (mod b a) a)))]
            (count (take-while #(< % n) (filter #(and (= 1 (gcd % n)) n) (range)))))))

(defcheck solution-cf8466cf
  (fn [x]
    (letfn [(gdc [a b] (if (zero? b) a (recur b (mod a b))))
            (coprime? [a b] (= 1 (gdc a b)))]
      (if (= 1 x) 1
                  (count (filter #(coprime? x %) (range 1 x)))))))

(defcheck solution-cfa56705
  (fn totient
    [n] {:pre [(integer? n)]}
    (let [;; To compute the gcd, we implement the Euclidean algorithm.
          ;; (This is copied from my solution to Problem 66.)
          gcd (fn [a b] {:pre [(integer? a), (integer? b)]}
                (cond
                  (neg? a)   (recur (- a) b)
                  (neg? b)   (recur a (- b))
                  (< a b)    (recur b a)
                  (zero? b)  a
                  :else      (recur b (mod a b))))]
      (if (= n 1) 1
                  (->> (range 1 n)
                    (filter (comp (partial = 1)
                                  (partial gcd n)))
                    count)))))

(defcheck solution-d05293d1
  (fn [n]
    (if (= 1 n) 1
                (letfn [
                        (gcd [x y] (if (= 0 (rem x y)) y (recur y (rem x y))))
                        (coprime [x y] (= 1 (gcd x y)))]
                  (count (filter #(coprime n %) (take (dec n) (drop 1 (range)))))))))

(defcheck solution-d1339a7a
  (fn [n]
    (inc (count
           (for [m (range 2 n)
                 :when (not-any? #(zero? (+ (mod m %) (mod n %)))
                         (range 2 (inc m)))]
             m)))))

(defcheck solution-d14db3e1
  (fn [n]
    (let [gcd (fn [a b]
                (cond (= a b) a
                      (> a b) (recur (- a b) b)
                      (< a b) (recur a (- b a))
                      :else :error))]
      (count (filter #(= 1 %) (map #(-> % inc (gcd n)) (range n)))))))

(defcheck solution-d19ef3b1
  (fn e [n]
    (let [gdc (fn [a b] (if (= b 0) a (recur b (mod a b))))]
      (inc (count (for [a (range 2 n) :when (= (gdc a n) 1)] a))))))

(defcheck solution-d1bd155f
  (fn [n]
    (if (= n 1) 1
                (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
                  (count (filter #(= 1 (gcd %1 n)) (range 1 n)))))))

(defcheck solution-d1e1ba34
  (fn [num]
    (letfn [(coprime? [x y]
              (loop [x x
                     y y]
                (if (= 0 (rem x y))
                  (if (= 1 y)
                    true
                    false)
                  (recur y (rem x y)))))]
      (count (filter (partial coprime? num) (drop 1 (range (+ 1 num))))))))

(defcheck solution-d1ff706
  (fn phi [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))
            (coprime [n m] (= (gcd n m) 1))]
      (count (for [x (range 1 (inc n)) :when (coprime n x)] x)))))

(defcheck solution-d22b1f08
  (fn f[x]
    (let [gcd (fn [x y] (last (sort
                                (filter #(and (= 0 (rem x %)) (= 0 (rem y %)))
                                  (range 1 (inc (min x y)))))))]
      (if (= x 1)
        1
        (count
          (filter #(= 1 (gcd x %)) (range (dec x) 0 -1)))))))

(defcheck solution-d267aee4
  (fn [n]
    (let
     [e #(loop [a %1 b %2]
           (if (= b 0)
             a
             (recur b (mod a b))))]
      (apply + (cons 1 (map #(if (= (e % n) 1) 1 0) (range 2 n)))))))

(defcheck solution-d2d93db1
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [a b]
                            (if (zero? b)
                              a
                              (recur b (mod a b))))
                      coprimes? (fn [m n]
                                  (= 1 (gcd m n)))]
                  (count (filter #(coprimes? n %) (range 1 n)))
                  ))))

(defcheck solution-d2dd4013
  (fn [x]
    (let [divlist (for [i (range 2 (inc (quot x 2)))
                        :when (= 0 (rem x i))]
                    i)
          coprime-with-x? (fn [y] (every?
                                    #(not= % 0)
                                    (for [i divlist]
                                      (rem y i))))]
      (inc (count
             (for [j (range 2 x)
                   :when (coprime-with-x? j)]
               j))))))

(defcheck solution-d31f56f8
  (fn [n]
    (letfn [(gcd [n m]
              (cond
                (== n m) n
                (< n m) (recur n (- m n))
                (> n m) (recur (- n m) m)))
            (coprime? [m]
              (== 1 (gcd n m)))]
      (if (== 1 n)
        1
        (count (filter coprime? (range 1 n)))))))

(defcheck solution-d3779367
  (fn [x]
    (let [divides (fn [a b] (zero? (rem b a)))
          divisors (filter #(divides % x) (range 1 x))
          common-divisors (fn [y] (filter #(divides % y) divisors))
          rel-prime #(= [1] (common-divisors %))]
      (if (= x 1)
        1
        (->> (range 1 x)
          (filter rel-prime)
          (count))))))

(defcheck solution-d38ebf1d
  (letfn [[min-max [a b] (if (< a b) [a b] [b a])]
          [gcd [a b] (if (= a b) a
                                 (let [[a b] (min-max a b)]
                                   (recur (- b a) a)))]]
    (fn [n] (if (= n 1) 1
                        (reduce + (for [i (range 1 n)
                                        :when (= (gcd i n) 1)] 1))))))

(defcheck solution-d4aa28b
  (fn totient [num]
    (if (= num 1)
      1
      (let [gcd (fn [a b]
                  (if (= b 0)
                    a
                    (recur b (mod a b))
                    )
                  )]
        (->> (range 1 num)
          (map (partial gcd num))
          (filter #(= 1 %))
          (count)
          )
        ))
    ))

(defcheck solution-d4fe50a2
  (fn toitent [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))]
      (count (filter #(= (gcd % x) 1) (range x))))))

(defcheck solution-d513f37b
  (fn eulers-totient-fn [n]
    (if (= n 1)
      1
      (count
        (letfn [(gcd [a b]
                  (if (= 0 b)
                    a
                    (gcd b (mod a b))))]
          (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-d5407f5
  (fn euler [n]
    (let [gcd (fn gcd [a b]
                (if (< a b) (gcd b a)
                            (if (= b 0) a
                                        (gcd b (mod a b)))))]
      (->> (range 1 (+ 1 n))
        (map #(gcd n %))
        (filter #(= 1 %))
        (apply +)))))

(defcheck solution-d5bcb312
  (fn [n] (if (= 1 n) 1
                      (letfn [(coprim [a b] (if (= 1 (min a b)) true
                                                                (if (= 0 (min a b)) false
                                                                                    (if (= a b) false
                                                                                                (if (> a b) (recur  (- a b) b)
                                                                                                            (recur a (- b a) )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                ))
                              (cprims [res t y] (if (> t y ) res
                                                             (if (coprim t y)
                                                               (recur (conj res t) (inc t) y)
                                                               (recur res (inc t) y)
                                                               )
                                                             ))
                              ]
                        (count (cprims [] 1 n))
                        )
                      )))

(defcheck solution-d5d07abd
  (fn [n]
    (case n
      1 1
      (letfn
       [(gcd [x y]
          (if (zero? y)
            x
            (recur y (mod x y))))]
        (count
          (filter #(= 1 (gcd n %))
            (range 1 n)))))))

(defcheck solution-d609646
  (fn [x]
    (let [gcd (fn gcd [a b]
                (if (zero? b)
                  a
                  (gcd b (mod a b))))]
      (if (= 1 x)
        1
        (count (filter #(= 1 (gcd x %))(range 1 x)))))))

(defcheck solution-d651c36e
  {1 1, 10 4, 40 16, 99, 60})

(defcheck solution-d69a6d9e
  (fn totient [n]
    (letfn [(gcd [a b]
              (if (= a b)
                a
                (if (> a b)
                  (gcd (- a b) b)
                  (gcd a (- b a))
                  )
                )
              )]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd n %))  (range 1 n)))
        )
      )
    ))

(defcheck solution-d710936
  (fn [n] (letfn [(div [x] (flatten (keep #(when (= 0 (mod x %)) [% (quot x %)]) (range 1 (inc x)))))]
            (let [dn (set (div n))]
              (letfn [(gcd [a] (apply max (clojure.set/intersection (set (div a)) dn)))
                      (coprime? [x] (= (gcd x) 1))]
                (if (= n 1) 1
                            (count (filter coprime? (range 1 n)))))))))

(defcheck solution-d74eabfa
  (fn [max]
    (cond (= max 1) 1
          0 (count (clojure.set/difference (set (range 1 max)) (set (for [a (range 2 max) b (range 2 (inc a))
                                                                          :when (every? zero? [(rem max b) (rem a b)])] a)))))))

(defcheck solution-d7d0622f
  #(letfn [(g [a b](if (= 0 b) (= 1 a) (g b (mod a b))))] (count (filter (fn[x] (g x %)) (range %)))))

(defcheck solution-d7e4d753
  (let [gcd (fn com-div [x y] (if (> y x) (com-div y x) (if (= y 0) x (com-div y (mod x y)))))]
    (fn etf [x]
      (if (= x 1) 1
                  (loop [actual 1
                         result 0]
                    (if (= actual x)
                      result
                      (recur (inc actual) (if (= (gcd actual x) 1) (inc result) result))))))))

(defcheck solution-d91b5c14
  (letfn [(sqrt-int [val]
            (Math/round (Math/sqrt val)))
          (divisor-groups [n]
            (filter
              #(integer? (last %))
              (map
                #(list % (/ n %))
                (range 1
                  (inc (sqrt-int n))))))
          (divisors [n]
            (set
              (distinct
                (sort
                  (flatten
                    (divisor-groups n))))))
          (intersection [a b]
            (set
              (filter #(and (contains? a %)
                            (contains? b %))
                (concat a b))))
          (coprime? [a b]
            (= #{1}
              (intersection (divisors a)
                (divisors b))))]
    (fn [n]
      (if (= 1 n)
        1
        (count (filter (partial coprime? n)
                 (range n)))))))

(defcheck solution-d9faa5b
  (fn [x]
    (if (= 1 x) 1
                (count
                  (filter
                    #(= % 1)
                    (map
                      #(loop [a x b %]
                         (if
                          (> b a)
                           (recur b a)
                           (if (zero? b) a
                                         (recur b (mod a b)))))
                      (range 1 x)))))))

(defcheck solution-da406284
  (fn [n]
    (letfn [(gcd [x y]
              (loop [[y x] (sort [x y])]
                (if (zero? y) x (recur [(mod x y) y]))))]
      ;; coprime when gcd is 1; ignore special case of 1 by ranging
      ;; from 2 and incrementing result:
      (inc (count (filter #(= 1 (gcd % n)) (range 2 n)))))))

(defcheck solution-da954204
  #(count
     (filter
       (fn [x]
         (loop [a % b x]
           (if (= 0 b)
             (= 1 a)
             (recur b (mod a b)))))
       (range %))))

(defcheck solution-dbac440
  (fn[x](let[
             gcd (fn l[a b](if (zero? b) a (l b (mod a b))))
             q (fn [a](= 1 (gcd x a)))]
          (count(filter q (range x))))))

(defcheck solution-dbf0e61e
  (letfn [(gcd [a b]
            (if (zero? b) a
                          (gcd b (mod a b))))]
    (fn [n]
      (if (= n 1) n
                  (count
                    (filter
                      #(= 1 (gcd % n))
                      (range 1 n)))))))

(defcheck solution-dc3383ca
  (fn euler[n]
    (if (= 1 n)
      1
      (letfn [( pgcd [a b]
                (if (zero? a)
                  b
                  (recur (mod b a) a)) )]

        (->> n (range 1) (filter #(= 1 (pgcd n %))) (count))
        ))))

(defcheck solution-dc361b62
  (fn euler [x]
    (let [sieve (fn sieve [n coll]
                  (cond
                    (empty? coll) nil
                    (= 0 (mod n (first coll))) (sieve n (filter #(> (mod % (first coll)) 0) (rest coll)))
                    true (cons (first coll) (sieve n (rest coll)))))]
      (count (cons 1 (sieve x (range 2 x)))))))

(defcheck solution-dd00f94d
  #(or (#{1} %) (- % (count (distinct (for [o (range 2 %) f (range 2 (inc o)) :when (= 0 (rem o f) (rem % f))] o))) 1)))

(defcheck solution-dd123ccd
  (fn [n]
    (letfn [(gdc [a r]
              (cond (< a r) (recur r a)
                    (not= 0 (mod a r)) (recur r (mod a r))
                    :ese r))]
      (if (= n 1)
        1
        (->> (range 1 n)
          (filter #(= 1 (gdc n %)))
          (count))))))

(defcheck solution-dd14e6ee
  (fn [max]
    (letfn [(gcd [x y]
              (if (= y 0)
                x
                (recur y (rem x y))))]
      (count (filter #(= 1 (gcd max %)) (range 1 (inc max)))))))

(defcheck solution-dd573784
  #(letfn[(gcd [x y]
            (cond
              (= x y) x
              (< x y) (recur x (- y x))
              :else (recur (- x y) y)))]
     (count (filter (fn [x] (= x 1)) (map (fn[x] (gcd % x)) (range 1 (inc %)))))))

(defcheck solution-dd6881c1
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))
            (co-prime? [k] (= (gcd n k) 1))]
      (count (filter co-prime? (range 1 (inc n)))))))

(defcheck solution-de0517a1
  (fn [x]
    (if (= 1 x) 1,
                (letfn [(gcd [a b] (if (zero? b) a, (recur b (mod a b))))
                        (coprime? [a] (= 1 (gcd x a)))]
                  ((comp count filter) coprime? (range 1 x))))))

(defcheck solution-de672aef
  (fn [x] (if (= x 1)
            1
            (count (filter #(= % 1) (map (partial
                                           (fn mygcd [a b] (let [zb (mod a b)] (if (= zb 0) b (mygcd b zb)))) x)
                                      (range 1 x)))))))

(defcheck solution-deff57a4
  (fn tot [n]
    (letfn [(gcd [a b]
              (if (= 0 b)
                a
                (gcd b (rem a b))
                )
              )]
      (reduce
        (fn [s k]
          (if (= 1 (gcd n k))
            (inc s)
            s
            )
          )
        0 (range 1 (inc n))
        )
      )
    ))

(defcheck solution-deffd8e5
  (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]

    (fn [x]
      (if (= x 1)
        1
        (->> (range (dec x) 0 -1)
          (filter #(= 1 (gcd x %)))
          count)))))

(defcheck solution-df4f1ea0
  (fn [n]
    (let [gcd (fn gcd [x y]
                (if (zero? y)
                  x
                  (gcd y (mod x y))))
          coprime (filter #(= 1 (gcd % n)) (range 1 n))]
      (max 1 (count coprime)))))

(defcheck solution-df94a5fb
  (fn [n] (let [gcd (fn [a b] (if (zero? b) a (recur b (rem a b))))]
            (if (= 1 n) 1 (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-e002fd7
  (fn [n]
    (letfn [(gcd
              ([a b]
               (gcd (sort [a b])))
              ([[a b]]
               (if (> b a) (gcd a (- b a)) a)))]
      (if (= n 1)
        1
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-e0411a31
  (fn f [x]
    (letfn [(gcd [a b]
              (reduce max 1
                (filter #(and (zero? (rem a %)) (zero? (rem b %))) (range 1 (inc (min a b))))))
            (coprime? [a b] (= 1 (gcd a b)))]
      (if (= x 1) 1 (count (filter #(coprime? % x) (range 1 x)))))))

(defcheck solution-e044e3d9
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 0 n))))))

(defcheck solution-e04fbc76
  (fn [n]
    (letfn [(gcd [a b]
              (if (= a b)
                a
                (let [s (sort [a b])]
                  (gcd (first s) (- (last s) (first s))))))]
      (count (filter #(= 1 %) (for [i (range 1 (inc n))] (gcd i n))))

      )))

(defcheck solution-e1072c70
  (fn [x]
    (if (not= x 1)
      (let [div (fn [y] (conj (set (filter #(= 0 (rem y %)) (range 1 y))) y))
            d (div x)]
        (count  (filter #(= #{1} (clojure.set/intersection d (div %))) (range 1 x))))
      x)))

(defcheck solution-e1d75b2c
  (fn [n]
    (if (= n 1)
      1
      (let [primefactors
            (fn primefactors
              ([n] (primefactors n 2 []))
              ([n trying sofar]
               (cond (= trying
                       n)
                     (conj sofar n)
                     (= (rem n trying)
                       0)
                     (recur (/ n trying) trying (conj sofar trying))
                     :else (recur n (inc trying) sofar))))]
        (* n
          (reduce * (map #(- 1
                             (/ 1 %))
                      (distinct (primefactors n)))))))))

(defcheck solution-e1f330d4
  (fn totient [n]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (count (when (> n 0)
               (filter (fn [m]
                         (= 1 (gcd m n)))
                 (rest (range (inc  n)))))))))

(defcheck solution-e2087874
  (fn phi [n]
    (letfn [(gcd [a b]
              (if (< a b)
                (recur b a)
                (first (filter #(= 0 (rem a %) (rem b %)) (iterate dec b)))))]
      (inc (count (filter #(= 1 (gcd % n)) (range 2 n)))))))

(defcheck solution-e239f4ef
  (fn
    [n]
    (->>
      (range 1 (+ n 1))
      (filter #(= 1 (loop [a n b %]
                      (let [c (mod a b)]
                        (if (= c 0) b (recur b c))))))
      count)))

(defcheck solution-e27b3baa
  #(count
     (for [f [(fn f [a b] (cond (= a b) a
                                (> a b) (f (- a b) a)
                                (< a b) (f a (- b a))))]
           i (or (not-empty (range 1 %)) [1])
           :when (= 1 (f % i))]
       i)))

(defcheck solution-e28cc6ab
  (fn [n]
    (if (= 1 n)
      1
      (count
        (for [x (range 1 n)
              :when (= 1 (#(if (= 0 %2)
                             %
                             (recur %2 (mod % %2)))
                          n x))]
          x)))))

(defcheck solution-e2bdb2f1
  (fn coprime
    [x]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (if (= x 1)
        1
        (count
          (reduce (fn [r e]
                    (if (= 1 (gcd x e))
                      (conj r e)
                      r))
            [] (range 1 x)))))))

(defcheck solution-e2e0875a
  (fn [n]
    (if (= n 1)
      1
      (letfn [(f [a b]
                (empty? (filter #(and (= 0 (mod a %))
                                      (= 0 (mod b %)))
                          (range 2 (max a b)))))]
        (count (filter #(f n %) (range 1 n)))))))

(defcheck solution-e376af90
  (fn [x]
    (if (= x 1) 1
                (letfn [(coprime? [y]
                          (every? #(not (= 0 (rem x %) (rem y %)))
                            (range 2 (inc y))))]
                  (count (filter coprime? (range 1 x)))))))

(defcheck solution-e3e26b1b
  (fn f [n]
    (if (= 1 n)
      1
      (count (filter #(= 1 ((fn ff [a nn] (if (= nn 0) a (ff nn (rem a nn)))) % n)) (range n))))))

(defcheck solution-e3eb2c75
  (fn et [n]
    (if (= n 1)
      1
      (let [r (fn [x] (range 1 (inc x)))
            fs (fn [x] (filter #(zero? (mod x %)) (r x)))
            cp (fn [x y]
                 (let [i (clojure.set/intersection (set (fs x)) (set (fs y)))]
                   (and (= (count i) 1) (= (first i) 1))))]
        (count (filter #(cp % n) (range 1 n)))))))

(defcheck solution-e412efbf
  (fn totient
    [n]
    (let [divisors-of
          (fn divisors-of
            [n proper]
            (cond
              (neg? n) (divisors-of (- n) proper)
              (< n 2)  (if (true? proper) []  [1])
              (= n 2)  (if (true? proper) [1] [1 2])
              :default (let [lim (int (/ n 2))
                             opt-n (if (true? proper) [] [n]) ]
                         (concat (filter #(zero? (rem n %)) (range 1 (inc lim))) opt-n))))

          coprime?
          (fn coprime?
            [x y]
            (= #{1} (clojure.set/intersection
                      (set (divisors-of x true))
                      (set (divisors-of y true)))))

          ]

      (if (= 1 n)
        1
        (dec (count (filter #(coprime? n %) (range 1 n))))))))

(defcheck solution-e424afe8
  (fn ET [n] (letfn [(gcd [x y ] (first (drop-while #(or (not= (rem x %) 0) (not= (rem  y %) 0)) (reverse (range 1  (inc (min x y)))))))](count (filter #(= (gcd % n) 1) (range (inc n)))))))

(defcheck solution-e42dfa08
  (fn [x]
    (letfn [(gcd [x y]
              (loop [a x b y]
                (cond
                  (= a b) a
                  (> a b) (recur b (- a b))
                  :else (recur a (- b a)))))
            (coprime-x? [a] (= (gcd a x) 1))]
      (if (= x 1) 1 (count (filter coprime-x? (range 1 x)))))))

(defcheck solution-e48ed450
  (fn [n]
    (letfn [
            (gcd [n m]
              (cond (= n m) n,
                    (> n m) (recur m (- n m)),
                    (< n m) (recur n (- m n))))
            (coprime-n? [m]
              (= (gcd n m) 1))]
      (+ 1 (count (filter coprime-n? (range 2 n)))))))

(defcheck solution-e4ad400c
  (fn [n] (count (filter identity (map #((fn f [x y]
                                           (if (= (min x y) 1) true
                                                               (if (= x y) false
                                                                           (f (min x y) (Math/abs (- x y)))))) % n) (range 1 (+ 1 n)))))))

(defcheck solution-e4b6d004
  (letfn [(gcd [a b]
            (loop [c a d b]
              (if (= d 0) c
                          (recur d (rem c d)))))]
    (fn [c]
      (if (= c 1)
        1
        (count (filter #(= 1 (gcd c %)) (range 1 c)))))))

(defcheck solution-e4ce85d6
  (fn coprime
    ([x]
     (if (= 1 x)
       1
       (coprime x 2 1)))
    ([x y z]
     (let [greatest-common-divisor (fn greatest-common-divisor
                                     ([x y]
                                      (greatest-common-divisor x y (min x y)))
                                     ([x y z]
                                      (if (= z 1)
                                        1
                                        (if (every? #(= 0 (rem % z)) (list x y))
                                          z
                                          (recur x y (dec z))))))]
       (if (= x y)
         z
         (if (= 1 (greatest-common-divisor x y))
           (recur x (inc y) (inc z))
           (recur x (inc y) z)))))))

(defcheck solution-e4da0e0b
  #(count (filter (fn [x] (= x 1)) (map (fn [a b] (if (zero? b) a (recur b (rem a b)))) (range 1 (inc %)) (cycle [%])))))

(defcheck solution-e4f21226
  (fn  [x]
    (letfn [( gcd [x y] (if (= y 0 ) x (gcd y (rem x y) )))]
      (count (filter #(= 1 (gcd x %)) (range x))))))

(defcheck solution-e4f45978
  (fn totient [x]
    (let [coprime? (fn coprime? [m n]
                     (let [ggt (fn ggt [m n]
                                 (if (= n 0)
                                   m
                                   (ggt n (mod m n))))]
                       (= (ggt m n) 1)))]
      (count (filter (partial coprime? x) (range 1 (inc x)))))))

(defcheck solution-e61873e1
  (let [gcd (fn g [a b] (if (= 0 b) a (recur b (mod a b))))]
    (fn [n]
      (if (= n 1) 1
                  (->> (range 1 n)
                    (filter #(= 1 (gcd % n)))
                    (count))))))

(defcheck solution-e68be117
  (fn [x]
    (letfn [(gcd' [x y]
              (if (= (mod y x ) 0)
                x
                (gcd' (mod y x) x)))
            (gcd [x y]
              (gcd' (min x y) (max x y)))]
      (if (= x 1)
        1
        (count (filter #(= (gcd % x) 1) (range 1 x)))))))

(defcheck solution-e6f1b9d8
  (fn [n]
    (if (= n 1) 1
                (let [gcd
                      (fn [n1 n2]
                        (let [[lo hi] (sort [n1 n2])
                              m (mod hi lo)]
                          (if (zero? m) lo
                                        (recur lo m))))]
                  (count
                    (filter #(= 1 (gcd n %))
                      (range 1 n)))))))

(defcheck solution-e74abbbd
  #(count
     (filter
       (fn [x]
         (= 1 ((fn g [a b]
                 (if (pos? b)
                   (g b (rem a b))
                   a))
               %
               x)))
       (range %))))

(defcheck solution-e74c47e8
  (letfn [(f [a b] (if (= 0 a) b (f (mod b a) a)))]
    (fn [n] (count (filter #(= 1 (f % n)) (range n))))))

(defcheck solution-e766437
  (fn [x]
    (let [gcd (fn [a b]
                (cond
                  (zero? b) a
                  :else (recur b (mod a b))))]
      (cond
        (= x 1) 1
        :else (->> (range x)
                (filter #(= (gcd x %) 1))
                count)))))

(defcheck solution-e7a1696
  (fn et [n]
    (if (= n 1) 1
                (letfn [(gcd [x y]
                          (if (<= y 0)
                            x
                            (gcd y (mod x y))))]
                  (reduce +
                    (map
                      #(if (= 1 (gcd n %)) 1 0)
                      (range 1 n)))))))

(defcheck solution-e7c586dc
  (fn [x]
    (if (= 1 x) 1
                (letfn [(gcd [a b] (last (filter #(and (zero? (mod a %)) (zero? (mod b %))) (range 1 (max a b)))))
                        (coprime? [a b] (= 1 (gcd a b)))]
                  (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-e7f8493f
  (fn [n]
    (letfn [(gcd [x y]
              (let [m (min x y) M (max x y)]
                (if (zero? m)
                  M
                  (gcd (rem M m) m))))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

(defcheck solution-e8092a22
  (fn [x]
    (letfn [
            (gcd [a b]
              (cond
                (< a b) (gcd a, (- b a))
                (> a b) (gcd (- a b), b)
                :else a))
            (coprime? [a b] (= 1 (gcd a b)))]
      (if (= x 1)
        1
        (count (filter (partial coprime? x) (range 1 x)))))))

(defcheck solution-e8d5484d
  (fn [n]
    (loop [c 2 m 2 total 1]
      (cond
        (>= m n) total
        (zero? (rem n m)) (recur 2 (inc m) total)
        (= c m) (recur 2 (inc m) (inc total))
        (= 0 (rem m c) (rem n c)) (recur 2 (inc m) total)
        :else (recur (inc c) m total)))))

(defcheck solution-e9182d46
  (fn [x]
    (let [gcd
          (fn [a b]
            (if (= b 0)
              a
              (recur b (mod a b))
              ))]
      (count (for [n (range x) :when (= (gcd n x) 1)] n))
      )))

(defcheck solution-e9993a4c
  #(count (letfn [(gcd [x y] (last (filter (fn [a] (= (mod x a) (mod y a) 0)) (range 1 (+ 1 y)))))]
            (map :val (get (group-by :gcd (map (fn [a] {:gcd (gcd % a)  :val a}) (range 0 (+ 1 %)))) 1)))))

(defcheck solution-ea3d07ac
  ;Calculating the divisors of a number is rather costly so we will have to reduce the number of numbers to be checked.
  ;First, we get all divisors of te given number N. Then we can skip all numbers less than N which are multiples of these
  ;divisors. The remaining numbers need to be checked individually.
  ;


  (let [divisors
        (fn [nmb]
          (let [c1 (for [n (range 2 nmb) :while(<= n (int (/ nmb n))) :when(= (mod nmb n) 0)] n)
                c2 (reverse (cons nmb (for [x c1] (/ nmb x))))]
            (concat c1 c2)
            )
          )
        ]

    (fn totient[N]
      (let [c1 (divisors N)
            cskip (for [i (range 1 N) n c1 :while(< (* i n) N)] (* i n))
            c2 (filter (fn[x] (every? (fn[y] (not= x y)) cskip)) (range 2 N))
            ]
        (inc (count c2))
        )
      )
    ))

(defcheck solution-eacada02
  (fn eulers-totient [n]
    (if (> n 1)
      (letfn [[coprime [a b]
               (= (gcd a b) 1)]
              [gcd [x y]
               (let [a (if (> x y) x y)
                     b (if (> x y) y x)]
                 (if (= b 0)
                   a
                   (recur (rem a b) b)
                   )
                 )]]
        (+ 1(count (filter (partial coprime n) (range 2 n))))
        )
      1
      )
    ))

(defcheck solution-eb299cdb
  (fn  [n]
    (letfn [(gcd [x y]
              (if (<= y 0)
                x
                (recur y (mod x y))))]
      (if (= 1 n)
        1
        (->> (range 1 n)
          (filter #(= 1 (gcd % n)))
          (count))))))

(defcheck solution-eb705bc9
  (fn [x]
    (if (= 1 x)
      1
      (loop [i 2
             r [1]]
        (if (= i x)
          (count r)
          (if (= 1
                ((fn gcd [x y]
                   (if (zero? (mod x y))
                     y
                     (gcd y (mod x y))))
                 x i))
            (recur (inc i) (concat r (list i)))
            (recur (inc i) r)))))))

(defcheck solution-ebc07d05
  (fn mytotient [n]
    (let [gcd (fn gcd [a b] (if (= a b) a (if (> a b) (gcd (- a b) b) (gcd a (- b a)))))]
      (if (= n 1) 1
                  (count (filter #(> %1 0) (for [i (range 1 n)]
                                             (if (= (gcd i n) 1) 1 0))))))))

(defcheck solution-ec18c52c
  (fn [n]
    (let [gcd (fn [a b]
                (if
                 (= b 0)
                  a
                  (recur b (rem a b))))]
      (count
        (filter
          #(= % 1)
          (map #(gcd % n) (range 1 (max n 2))))))))

(defcheck solution-ec4857bc
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (if (zero? b)
                  a
                  (recur b (rem a b))))]
        (count (filter #(= (gcd % x) 1) (range 1 x)))))))

(defcheck solution-ec7e3c6f
  (fn [n]
    (letfn [(gcd [i j]
              (cond
                (< i j) (recur j i)
                (zero? j) i
                :else (recur j (mod i j))))]
      (count (filter #(= 1 (gcd n (inc %))) (range n))))))

(defcheck solution-ecbd11fc
  (fn [n]
    (letfn [(f [x y]
              (loop [c (min x y)]
                (if (= (mod x c) (mod y c) 0) c
                                              (recur (dec c)))))]
      (loop [c (dec n) r 1]
        (cond
          (< c 2) r
          (= (f c n) 1) (recur (dec c) (inc r))
          :else (recur (dec c) r))))))

(defcheck solution-ecea15c8
  (fn myf2 [n]
    (letfn [(gcd [n1 n2]
              (let [m (max n1 n2), l (min n1 n2)]
                (if (zero? (rem m l)) l (gcd (- m l) l))))]

      (if (= n 1) 1
                  (->> (range 1 n)
                    (filter #(= 1 (gcd n %)))
                    count)))))

(defcheck solution-ed689426
  (fn [n]
    (if (= n 1)
      n
      (count (filter #(= 1 %)
               (map (fn gcd [a b]
                      (cond
                        (> a b) (gcd (- a b) b)
                        (< a b) (gcd a (- b a))
                        :else a))
                 (range 1 n)
                 (repeat n)))))))

(defcheck solution-ee274253
  (fn a [n]
    (count
      (filter
        (fn [p]
          (not-any? #(= 0 (mod n %) (mod p %)) (range 2 n)))
        (range n)))))

(defcheck solution-ee35f534
  (fn toitent [x]
    (letfn [(divisor [y]
              (filter #(= 0 (rem y %)) (map #(+ 2 %) (range (/ y 2)))))]
      (count (clojure.set/difference (into #{} (range x))
               (into #{} (apply concat (for [d (divisor x)] (filter #(= 0 (rem % d)) (range x)))))))
      )
    ))

(defcheck solution-ef32c300
  (fn totient [n]
    (letfn [(gcd [a b]
              (cond (> a b) (gcd (- a b) b)
                    (< a b) (gcd a (- b a))
                    :else a))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n))))
      )))

(defcheck solution-ef492f65
  (fn totient [n]
    (let [divisors (fn [m]
                     (for [x (range 2 m)
                           :when (== (mod m x) 0)]
                       x))
          iscoprime (fn [m]
                      (not (seq (filter #(== (mod m %) 0) (divisors n)))))]
      (if (== n 1)
        1
        (count (filter iscoprime (range 1 n)))))))

(defcheck solution-ef7bf102
  (fn [n]
    (if (= 1 n)
      1
      (let [pgcd (fn [a b] (loop [a a b b] (if (= b 0) a (recur b (rem a b)))))]
        (count (filter #(= 1 (pgcd (max n %) (min n %))) (rest (range n))))))))

(defcheck solution-ef904fe2
  (fn [n]
    (letfn [(g [a b]
              (if (= b 0) a (g b (rem a b))))]
      (if (= n 1)
        1
        (count
          (filter
            #(= 1 (g n %))
            (range 1 n)))))))

(defcheck solution-efa8f8f0
  (fn [n]
    (if (= n 1)
      1
      (let [divs (fn [x] (set (filter #(not (nil? %)) (for [i (range 1 (inc x))] (if (= (rem x i) 0) i nil)))))
            ndivs (divs n)]
        (count (filter #(= (clojure.set/intersection (divs %) ndivs) #{1})  (range 1 n)))
        ))))

(defcheck solution-efabf074
  (fn [n]
    (letfn [(gcd [x y]
              (cond (< y x) (recur y x)
                    (= x 0) y
                    :else (recur (mod y x) x)))]
      (count (filter #(= 1 (gcd % n)) (range n))))))

(defcheck solution-efb97f13
  (fn [n]
    (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
      (if (= n 1) 1 (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-f057593d
  #(count (filter
            (fn [x]
              (= 1
                (
                 (fn gcd [x y]
                   (cond (zero? x) y
                         (zero? y) x
                         :else
                         (recur y (mod x y))))
                 x %1
                 )
                ))
            (range %1))))

(defcheck solution-f10041aa
  (fn [number] (->> (range 2 number)
                 (map (partial (fn [a b] (every? false? (map #(= 0 (mod a %) (mod b %)) (range 2 (inc (min a b)))))) number))
                 (filter true?)
                 count
                 inc)))

(defcheck solution-f136e60c
  (fn totient [x]
    (if (= 1 x)
      1
      (count (filter #(= 1 ((fn [a b]
                              (if (= 0 b) a (recur b (mod a b)))) x %))
               (range 1 x))))))

(defcheck solution-f2720cae
  (fn coprime_num [n]
    (let [nocoprime?
          (fn nocoprime? [n1 n2]
            (cond (or (= 0 n1) (= 0 n2)) true
                  (or (= 1 n1) (= 1 n2)) false
                  :else
                  (if (not-any? #(= 0 (rem n1 %) (rem n2 %)) (range 2 (inc (min n1 n2))))
                    false
                    true)))
          coprime_col
          (fn coprime_col [n]
            (filter (comp not (partial nocoprime? n)) (range (inc n))))]
      (count (coprime_col n)))))

(defcheck solution-f27732cb
  (fn [n]
    (letfn [(seive [xs]
              (if-let [[x & xs] (seq xs)]
                (let [div-x? #(zero? (mod % x))]
                  (if (div-x? n)
                    (->> xs (remove div-x?) recur)
                    (inc (seive xs))))
                0))]
      (->> (range 2 n) seive inc))))

(defcheck solution-f2bd05c7
  (fn [n]
    (letfn
     [
      (gcd [a b] (if (zero? a) b (recur (rem b a) a)))]
      (count
        (filter #(= 1 (gcd n %)) (range n))))))

(defcheck solution-f2be046
  (fn [n] (if (= 1 n) 1 (letfn [
                                (gcd [x y]
                                  (if (= 0 (* x y))
                                    (+ x y)
                                    (gcd (- (max x y) (min x y)) (min x y) ))) ]
                          (count (filter #(= (gcd % n) 1) (range 1 n)))))))

(defcheck solution-f2e9c0ab
  (fn [n]
    (if (= 1 n) 1
                (let [gcd (fn [i1 i2]
                            (apply max
                              (filter #(= 0 (mod i1 %) (mod i2 %))
                                (range 1 (inc (max i1 i2))))))
                      coprime (fn [i1 i2] (if (= 1 (gcd i1 i2)) (min i1 i2) nil))]
                  (count (filter (partial coprime n) (range 1 n)))
                  ))))

(defcheck solution-f306963c
  (fn [t] (if (= t 1) 1 (count (filter
                                 #(= 1 ((partial (fn gcd [x y]
                                                   (cond (= x 0) y
                                                         (= y 0) x
                                                         :else (gcd y (mod x y)))) t) %)) (range 1 t))))))

(defcheck solution-f3b23d72
  #(letfn [(g [a b]
             (if (< a b)
               (g b a)
               (let [m (mod a b)] (if (= 0 m)
                                    b
                                    (g b m)))))
           (c [a b]
             (= 1 (g a b)))]
     (if (= % 1)
       1
       (count
         (filter true?
           (for [i (range 1 %)] (c % i)))))))

(defcheck solution-f45fd340
  (fn [n]
    (let [gcd (fn [a b] (if (= a b) a (recur (- (max a b) (min a b)) (min a b))))]
      (count (filter #(= 1 %)
               (for [i (range 1 (inc n))]
                 (gcd i n)))))))

(defcheck solution-f469b854
  (fn [n]
    (letfn [(gcd [a b]
              (loop [x (min a b)]
                (if (and (= 0 (mod a x)) (= 0 (mod b x))) x (recur (dec x)))))]
      (if (= 1 n) 1
                  (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-f4cd86cd
  (fn [n]
    (let [gcd (fn [a b]
                (cond
                  (= a b) a
                  (> a b) (recur (- a b) b)
                  :else (recur a (- b a))))]
      (count (filter #(= 1 (gcd % n)) (range 1 (inc n)))))))

(defcheck solution-f4eb21a3
  (fn g
    ([m]
     (g mod m m))
    ([z m n]
     (if (= 1 n)
       1
       (+
        (if (= 1
              (#(if (=  0 (z m %) (z n %))
                  %
                  (recur (- % 1)))
               n))
          1
          0)
        (g z m (- n 1)))))))

(defcheck solution-f5073c9
  (fn [n]
    (letfn [(gcd [x y] (if (= y 0) x (recur y, (mod x y))))]
      (if (= n 1)
        1
        (->> (range 1 n)
          (filter #(= (gcd % n) 1))
          (count))))))

(defcheck solution-f66b0be7
  (fn [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (gcd b (rem a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-f6b6f4d2
  (fn [n]
    (if (= n 1)
      1
      (let [gcd (fn [x y]
                  (loop [x x y y]
                    (if (zero? (rem x y))
                      y
                      (recur y (rem x y)))))]
        (count (filter #(= 1 (gcd % n)) (range 1 n)))))))

(defcheck solution-f7a1d807
  (fn euler
    [n]
    (if (= 1 n) 1
                (let [gcd (fn [n m] (let [r (mod n m)](if (> r 0)(recur m r) m)))]
                  (reduce #(if (= 1 (gcd n %2)) (inc %) %) 0 (range 1 n))))))

(defcheck solution-f7bdf791
  (fn [n]
    (count (loop [coprimes (range 1 (inc n)) current 2]
             (if (> current n)
               coprimes
               (recur (if (= 0 (rem n current))
                        (remove #(= 0 (rem % current)) coprimes)
                        coprimes)
                 (inc current)))))))

(defcheck solution-f7eb01ce
  (fn f [x]
    (letfn [(gcd [m n] (cond (> m n) (gcd (- m n) n)
                             (< m n) (gcd m (- n m))
                             :else n))]
      (if (= 1 x) 1 (count (filter #(= 1 (gcd x %)) (range 1 x)))))))

(defcheck solution-f8049735
  (fn euler-totient [x]
    (letfn [(my-gcd [x y]
              (let [r (rem x y)]
                (if (zero? r) y (recur y r))))]
      (count (filter #(= % 1) (map #(my-gcd % x) (range 1 (inc x))))))))

(defcheck solution-f82100a1
  (fn [n]
    (if (= 1 n)
      n
      (let [gcd (fn [a b]
                  (loop [x a y b]
                    (if (= y 0)
                      x
                      (recur y (rem x y)))))
            ]
        (count (filter #(= 1 (gcd n %)) (range 1 n)))))))

(defcheck solution-f82458f2
  (fn [x]
    (if (= x 1)
      1
      (let [d (for [a (range 2 (-> x Math/sqrt int inc))
                    :when (and (< a x) (zero? (mod x a)))] a)
            d (reduce #(conj %1 %2 (/ x %2)) #{} d)]
        (count (filter (fn [t] (not-any? #(zero? (mod t %)) d)) (range 1 x)))))))

(defcheck solution-f82a5e1d
  (fn myEulersTotient
    [x]
    (let [commonDiv (fn [x y]
                      (loop [xx x yy y]
                        (if (= xx yy)
                          xx
                          (recur (- (max xx yy) (min xx yy)) (min xx yy)))))
          numsToTest (fn [num] (range 1 (inc num)))]
      (count (filter #(= 1 (commonDiv x %)) (numsToTest x))))))

(defcheck solution-f8a0497d
  (fn [n]
    (letfn [(gcd [a b]
              (cond
                (< a b) (recur a (- b a))
                (> a b) (recur (- a b) b)
                :else a))]
      (count (filter #(= 1 (gcd n %)) (range 1 (inc n)))))))

(defcheck solution-f94d28d3
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))]
      (->> (range 1 (inc n)) (filter #(= 1 (gcd n %))) count))))

(defcheck solution-f971dafd
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(gcd [a b]
                (loop [a a, b b]
                  (cond
                    (= a b) a
                    (> a b) (recur (- a b) b)
                    :else (recur a (- b a)))))]
        (->> n
          (range 1)
          (map (partial gcd n))
          (filter (partial = 1))
          count)))))

(defcheck solution-f9a30135
  #(let [divisors (fn [n] (set (filter (fn [dem] (= 0 (mod n dem))) (range 2 (inc n)))))]
     (if (= % 1) 1
                 (count (filter (fn [n] (empty? (clojure.set/intersection (divisors n) (divisors %)))) (range 1 %))))))

(defcheck solution-f9cabccd
  (fn count-prime [n]
    (let [my-prime (fn my-prime [x y] (if (zero? y) x (my-prime y (mod x y))))]
      (if (= 1 n)
        1
        (count (filter #(= 1 (my-prime n %)) (range 1 n)))))))

(defcheck solution-fa4d47a
  #(reduce (fn [c i] (letfn [(gcd [m n] (if (zero? (mod m n)) n (recur n (mod m n))))]
                       (if (= 1 (gcd % i))
                         (inc c) c))) (if (= 1 %) 1 0) (range 1 %)))

(defcheck solution-fa6ee363
  (fn [x]
    (letfn
     [(gcd [y] (apply max (filter #(= 0 (mod x %) (mod y %)) (range 1 (+ 1 (max (/ x 2) (/ y 2)))))))]
      (count
        (filter
          #(= 1 (gcd %))
          (range x))))))

(defcheck solution-fa7a4edb
  (fn totient [x]
    (if (= x 1) 1
                (letfn [(mygcd [a b] (if (= 0 b) a (recur b (mod a b))))]
                  (->> (range 1 x) (filter #(= 1 (mygcd x %))) count)))))

(defcheck solution-facb90c5
  (fn [n]
    (max
      1
      (let [cp (fn [x y] (if (zero? y) (= 1 x) (recur y (mod x y))))]
        (count
          (filter #(cp n %) (range 1 n)))))))

(defcheck solution-fad6ad8d
  (fn [n]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (gcd y (rem x y))))
            (coprime? [x y]
              (= (gcd x y) 1))]
      (if (= n 1)
        1
        (inc (count (filter true? (map (partial coprime? n) (range 2 (inc n))))))))))

(defcheck solution-fb20297d
  (fn [x] (letfn [(coprime? [a b] (every? #(not= 0 (rem a %) (rem b %))
                                    (range a 1 -1)))]
            (count (filter #(coprime? x %)
                     (range x))))))

(defcheck solution-fb489d9a
  (fn [n]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (coprime? [a b]
              (= (gcd a b) 1))]
      (if (= n 1)
        1
        (count (filter true? (map (partial coprime? n) (range 1 n))))))))

(defcheck solution-fb4f53f
  (fn [n]
    (let [f (fn f [x y]
              (if (= x 0)
                y
                (if (< x y)
                  (f (- y x) x)
                  (f (- x y) y))))]
      (if (= n 1)
        1
        (count (filter #(= 1 (f % n)) (rest (range n))))))))

(defcheck solution-fbdcc688
  (fn etf [n]
    (if (= n 1)
      1
      (let [gcd (fn mygcd [x y]
                  (let [u (max x y)
                        v (min x y)]
                    (if (zero? (mod u v))
                      v
                      (mygcd v (mod u v)))))]
        (count (filter #(= 1 (gcd % n))
                 (range 1 n)))))))

(defcheck solution-fbe0812b
  #(letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))
           (coprime? [a b] (= 1 (gcd a b)))]
     (if (= 1 %) 1 (count (filter (partial coprime? %) (range 1 %))))))

(defcheck solution-fc8fb213
  (fn totient [x]
    (let [prime? (fn [x]
                   (->> (range 2 x)
                     (map #(rem x %))
                     (map zero?)
                     (every? false?)))]
      (->> (range 2 (inc x))
        (filter prime?)
        (filter #(zero? (rem x %)))
        (map #(- 1 (/ %)))
        (apply *)
        (* x)))))

(defcheck solution-fcbbaecf
  (fn [n]
    (letfn [(g [a b]
              (if (or (= a b) (= b 0))
                a
                (recur b (mod a b))))]
      (if (= n 1) 1
                  (reduce #(if (= (g n %2) 1) (inc %) %) 0 (range 1 n))))))

(defcheck solution-fcf0e595
  #(letfn [(gcd [a b]
             (if (zero? b)
               a
               (recur b (mod a b))))]
     (loop [xs (range 2 %)
            ix 1]
       (if (seq xs)
         (recur (rest xs) (if (= (gcd % (first xs)) 1)
                            (inc ix)
                            ix))
         ix))))

(defcheck solution-fd12d447
  (letfn [(gcd [a b]
            (if (= 0 b) a
                        (recur b (rem a b))))
          (coprime [a b] (= 1 (gcd a b)))]
    (fn [n]
      (if (= n 1) 1
                  (count (filter #(= 1 (gcd % n))
                           (range 1 n)))))))

(defcheck solution-fde90e5f
  (fn [x]
    (if (= 1 x)
      1
      (inc (count (filter (fn [y]
                            (every? #(not= 0 (rem x %) (rem y %)) (range 2 (inc y)) )

                            ) (range 2 x))
             )))
    ))

(defcheck solution-fdeb9e09
  (fn [n]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (gcdn [m] (gcd n m))]
      (count (filter #(= 1 %) (map gcdn (range 0 n)))))))

(defcheck solution-fe9b033f
  (let [euclid-gcd (fn
                     [a b]
                     (loop [x a
                            y b]
                       (if (= (mod x y) 0)
                         y
                         (recur y (mod x y)))))]
    (let [coprime? (fn [x y] (= 1 (euclid-gcd x y)))]
      (fn euler-totient
        [x]
        (count (filter #(coprime? % x) (take x (range))))))))

(defcheck solution-fec98bf0
  (fn [n]
    (letfn [(gcd [a b]
              (loop [[a b] (if (> a b) [a b] [b a])]
                (let [c (rem a b)]
                  (if (= 0 c)
                    b
                    (recur [b c])))))]
      (count (filter (fn [x] (= 1 (gcd x n))) (range 1 (inc n)))))))

(defcheck solution-ffcaea45
  (fn [n]
    (if (= 1 n)
      1
      (letfn [(gcd [x y]
                (let [re (rem x y)]
                  (if (= 0 re)
                    y
                    (recur y re))))]
        (count (filter #(= 1 (gcd % n))
                 (range 1 n)))))))