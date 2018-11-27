(ns coal-mine.problem-116
  (:require [coal-mine.checks :refer [defcheck-116] :rename {defcheck-116 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10b701f5
  (let [primes          ((fn sieve [s]
                           (cons (first s) (lazy-seq
                                             (sieve (filter
                                                      #(not= 0 (mod % (first s)))
                                                      (rest s))))))
                         (iterate inc 2))
        balanced-primes (map second
                          (filter (fn [[p e n]]
                                    (= (* 2 e) (+ p n)))
                            (partition 3 1 primes)))]
    (fn [n] (= n (first (drop-while #(< % n) balanced-primes))))))

(defcheck solution-10bd2a68
  (fn balanced-prime? [n]
    (letfn [(smallest-divisor [n]
              (loop [i 3]
                ;need only test odd divisors between 3 and (sqrt n)
                (when (<= (* i i) n)
                  (if (zero? (mod n i))
                    i
                    (recur (+ i 2))))))
            (prime? [n]
              (or (= n 2)
                  (and (> n 1)
                       (odd? n)
                       (not (smallest-divisor n)))))
            (next-prime [prime]
              (if (= prime 2)
                3
                (loop [prime (+ prime 2)]
                  (if (smallest-divisor prime)
                    (recur (+ prime 2))
                    prime))))
            (prev-prime [prime]
              (if (= prime 2)
                nil
                (if (= prime 3)
                  2
                  (loop [prime (- prime 2)]
                    (if (smallest-divisor prime)
                      (recur (- prime 2))
                      prime)))))]
      (true? (and (prime? n)
                  (when-let [pp (prev-prime n)]
                    (let [np (next-prime n)]
                      (= n (/ (+ pp np)
                             2)))))))))

(defcheck solution-112ca216
  (fn balanced-prime? [n]
    (letfn [(prime? [n]
              (and (> n 1)
                   (not-any?
                     (fn [divisor] (zero? (rem n divisor)))
                     (range 2 n))))
            (from [x] (range (inc x) 2147483647))
            (first-prime [aseq]
              (first (filter prime? aseq)))
            (average [a b] (/ (+ a b) 2))]
      (and (prime? n)
           (loop [dist 1]
             (let [below (prime? (- n dist))
                   over  (prime? (+ n dist))]
               (cond
                 (and below over) true
                 (and (not below) (not over)) (recur (inc dist))
                 :else false)))))))

(defcheck solution-1220c361
  (fn [x]
    (letfn [(prime? [x] (not (some #(zero? (rem x %)) (range 2 x))))]
      (cond
        (<= x 2) false
        (prime? x)
        (let [prime-before (first (filter prime? (iterate dec (dec x))))
              prime-after  (first (filter prime? (iterate inc (inc x))))]
          (= (/ (+ prime-before prime-after) 2) x))
        :else false))))

(defcheck solution-127139a9
  (fn [n]
    (letfn [(prime [m] (not (some #(zero? (mod m %))
                              (range 2 (inc (int (/ m 2)))))))]
      (and (> n 2) (prime n)
           (let [[f s t] (first (filter #(= n (second %)) (partition 3 1 (filter prime (iterate inc 2)))))]
             (= s (/ (+ f t) 2)))))))

(defcheck solution-13131491
  (fn ps [n]
    (let [prime?     (fn [n] (if (< n 2) false (not-any? #(= 0 (mod n %)) (range 2 n))))
          prev-prime (fn [n] (if (prime? (dec n)) (dec n) (recur (dec n))))
          next-prime (fn [n] (if (prime? (inc n)) (inc n) (recur (inc n))))]
      (if (or (<= n 2) (not (prime? n)))
        false
        (= (+ n n) (+ (prev-prime n) (next-prime n)))))))

(defcheck solution-133172c
  (fn [n]
    (letfn [(prime? ([i] (not-any? #(= 0 (mod i %)) (range 2 i))))
            (psearch ([i f] (if (prime? (f i)) (f i) (psearch (f i) f))))
            (ave ([x y] (/ (+ x y) 2)))]
      (and
       (> n 2)                                              ; since 2 is not balanced & primes breaks on 0 or 1
       (prime? n)
       (== n (ave (psearch n dec) (psearch n inc)))))))

(defcheck solution-13ce326
  (fn balanced-prime? [n]
    (let [is-prime? #(not-any? (fn [d] (= 0 (rem % d))) (range 2 %))]
      (and (< 4 n)
           (is-prime? n)
           (let [i #(nth (filter is-prime? (iterate % n)) 1)] (= n (/ (+ (i inc) (i dec)) 2)))))))

(defcheck solution-148b0980
  (fn [n]
    (let [S some
          R range
          p (fn [x]
              (if (S #(= 0 (rem x %)) (R 2 x))
                false
                x))
          d (* 2 n)]
      (and
       (> n 2)
       (p n)
       (= d (+ (S p (R (- n 1) 1 -1)) (S p (R (+ n 1) d))))))))

(defcheck solution-1611fc40
  (fn [n]
    (and (> n 3)
         (let [prime? (fn [x] (every? #(pos? (mod x %)) (range 2 (inc (/ x 2)))))
               pre    (first (filter prime? (range (dec n) 1 -1)))
               post   (first (filter prime? (iterate inc (inc n))))]
           (and (prime? n) (= (+ pre post) (+ n n)))))))

(defcheck solution-161bf9df
  (fn [n]
    (letfn [(p? [n]
              (when (> n 1)
                (every? #(not= 0 (rem n %))
                  (range 2 (max 3 (quot n 2))))))

            (np [n next-n]
              (first (filter p? (iterate next-n n))))]

      (and (p? n)
           (> n 3)
           (= (- (np (inc n) inc) n)
             (- n (np (dec n) dec)))))))

(defcheck solution-165dc14d
  (letfn [(prime? [x] (and (> x 1) (not-any? zero? (map #(rem x %) (take-while #(<= (* % %) x) (range 2 x))))))
          (pNeighbors [n] (let [l (range (dec n) 2 -1) r (drop (inc n) (range))] (map (comp #(nth % 0 0) #(filter prime? %)) [l r])))
          (balancedPrime? [n] (and (prime? n) (= (* 2 n) (reduce + (pNeighbors n)))))]
    balancedPrime?))

(defcheck solution-16a27df3
  (fn prim
    ([x]
     (if (or (= x 0) (= x 1) (= x 2))
       false
       (if (some zero? (map #(mod %1 %2) (repeat x) (range 2 x)))
         false
         (prim x (dec x)))))
    ([x y]
     (if (some zero? (map #(mod %1 %2) (repeat y) (range 2 y)))
       (prim x (dec y))
       (prim x y (inc x))))
    ([x y z]
     (if (some zero? (map #(mod %1 %2) (repeat z) (range 2 z)))
       (prim x y (inc z))
       (if (= (/ (+ y z) 2) x)
         true
         false)))))

(defcheck solution-16db61f5
  (fn [x]
    (let [prime?     (fn [p]
                       (every? #(> (mod p %) 0) (range 2 p)))
          find-prime (fn [xs]
                       (some #(when (prime? %) %) xs))]
      (and (> x 2) (prime? x) (= (* x 2) (+ (find-prime (range (dec x) 1 -1)) (find-prime (range (inc x) (* x 2)))))))))

(defcheck solution-16df9e98
  (fn is-balanced-prime [x]
    (let [is-prime?   (fn [x]
                        (and (> x 1)
                             (->> (range 2 x)
                               (map (fn [y] (/ x y)))
                               (filter integer?)
                               (filter (fn [y] (not= 1 y)))
                               (empty?))))
          next-prime  (first (filter is-prime? (drop-while #(<= % x) (range))))
          prev-primes (filter is-prime? (range (dec x) 1 -1))
          ]
      (if (and (is-prime? x)
               (not-empty prev-primes)
               (= (- x (first prev-primes)) (- next-prime x)))
        true
        false))))

(defcheck solution-17f73acb
  (fn [n]
    (let [primes
                        (fn []
                          (letfn [(enqueue [sieve n step]
                                    (let [m (+ n step)]
                                      (if (sieve m)
                                        (recur sieve m step)
                                        (assoc sieve m step))))
                                  (next-sieve [sieve candidate]
                                    (if-let [step (sieve candidate)]
                                      (-> sieve
                                        (dissoc candidate)
                                        (enqueue candidate step))
                                      (enqueue sieve candidate (+ candidate candidate))))
                                  (next-primes [sieve candidate]
                                    (if (sieve candidate)
                                      (recur (next-sieve sieve candidate) (+ candidate 2))
                                      (cons candidate
                                        (lazy-seq (next-primes (next-sieve sieve candidate)
                                                    (+ candidate 2))))))]
                            (cons 2 (lazy-seq (next-primes {} 3)))))
          prime-triples (partition 3 1 (primes))
          [below middle above] (first (drop-while #(< (second %)
                                                     n)
                                        prime-triples))]
      (if (= middle n)
        (= middle (/ (+ below above) 2))
        false)
      )))

(defcheck solution-1889da58
  (let [p (memoize #(loop [p (inc (last %))]
                      (if (some zero? (map (partial mod p) %))
                        (recur (inc p))
                        (conj % p))))]
    (fn [n]
      (when (> n 2)
        (let [ps
              (some
                #(when (> (last %) n) %)
                (iterate p [2]))]
          (and
           (= n (nth ps (- (count ps) 2)))
           (= n
             (/
               (+ (last ps) (nth ps (- (count ps) 3)))
               2))))))))

(defcheck solution-18c0e273
  (fn sandw [n]
    (let [prime?   (fn [x] (and (> x 1) (not-any? #(zero? (mod x %)) (range 2 x))))
          previous (first (filter prime? (reverse (range 1 n))))
          next     (first (filter prime? (drop (inc n) (range))))]
      (and previous next (prime? n) (= (+ next previous) (* 2 n))))))

(defcheck solution-18d85f28
  (fn [n]
    (letfn [(fp [p]
              (if (or (= p 3) (= p 5)) true
                                       (loop [x 3]
                                         (if (= (mod p x) 0) false
                                                             (if (> x (/ p 2)) true
                                                                               (recur (inc (inc x))))))))]
      (if (or (< n 5) (= (mod n 2) 0)) false
                                       (let [a (loop [x (dec (dec n))]
                                                 (if (fp x) x
                                                            (recur (dec (dec x)))))
                                             b (loop [x (inc (inc n))]
                                                 (if (fp x) x
                                                            (recur (inc (inc x)))))]
                                         (and (fp n) (= n (/ (+ a b) 2))))))))

(defcheck solution-1965bc81
  (fn [n]
    (let [is-prime   (fn [n] (loop [i 2]
                               (cond (> i (/ n 2)) true
                                     (zero? (mod n i)) false
                                     :else (recur (inc i)))))
          next-prime (fn [n f] (first (drop-while (fn [x] (not (is-prime x))) (iterate f (f n)))))]
      (and (> n 2) (is-prime n)
           (= n (/ (+ (next-prime n inc) (next-prime n dec)) 2))))))

(defcheck solution-198c2b7a
  (fn sandwich-prime [n]
    (letfn [(prime [n]
              (when (>= n 2)
                (not (some #(zero? (rem n %))
                       (range 2 (dec n))))))
            (prev-next-prime [n]
              (when (prime n)
                [(some #(when (prime %) %) (range (dec n) 0 -1))
                 (some #(when (prime %) %) (range (inc n) (* 2 (inc n))))]))]
      (if (prime n)
        (let [[pp np] (prev-next-prime n)]
          (and pp np (= (/ (+ pp np) 2) n)))
        false))))

(defcheck solution-1a388c7d
  (fn [n]
    (letfn [(prime [n] (and (> n 1) (every? #(pos? (mod n %)) (range 2 n))))]
      (if (and (prime n) (> n 2))
        (let [u (first (filter prime (iterate inc (inc n))))
              d (first (filter prime (iterate dec (dec n))))]
          (= (+ u d) (* 2 n)))

        false))
    ))

(defcheck solution-1a56c459
  (fn balanced-prime [n]
    (let [isprime?  (fn [n] (not-any? #(= 0 (mod n %)) (range 2 n)))
          nextprime (fn [n] (first (filter isprime? (iterate inc (inc n)))))
          prevprime (fn [n] (first (filter isprime? (iterate dec (dec n)))))]
      (and (> n 2)
           (isprime? n)
           (= n (/ (+ (nextprime n) (prevprime n)) 2))))))

(defcheck solution-1a6142a7
  (fn balanced-prime? [x]
    (let [
          sqrt       (fn [x]
                       (->> x (Math/sqrt) (Math/floor) (Math/round)))
          divides?   (fn [a b]
                       (== 0 (mod b a)))
          prime?     (fn [x]
                       (cond
                         (< x 0) (recur (- x))
                         (< x 2) false
                         :else (let [sqrt-x (sqrt x)
                                     prior  (range 2 (+ 1 sqrt-x))]
                                 (if (not-any? #(divides? % x) prior) x nil))))
          prev-prime (fn [x]
                       (some prime? (reverse (range 2 x))))
          next-prime (fn [x]
                       (some prime? (drop (+ 1 x) (range))))
          balanced?  (fn [a b c]
                       (== (+ a c) (+ b b)))
          ]
      (and (< 2 x)
           (not (nil? (prime? x)))
           (balanced? (prev-prime x) x (next-prime x))))))

(defcheck solution-1c119897
  (fn hoge [x]
    (letfn [(melem [x xs]
              (if (empty? xs) false
                              (cond (< x (first xs)) false
                                    (= x (first xs)) true
                                    true (melem x (next xs)))))]
      (melem x '(5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103))
      )
    ))

(defcheck solution-1ca1f704
  (fn balanced? [n]
    (letfn [(prime? [n]
              (and (> n 1) (or (= n 2) (not-any? #(= 0 (rem n %)) (range 2 n)))))
            (next-prime [n s]
              (first (filter prime? (iterate #(+ % s) (+ n s)))))]
      (and (> n 4)
           (prime? n)
           (= n (/ (+ (next-prime n 1) (next-prime n -1)) 2))))))

(defcheck solution-1cc495d9
  (fn prime-sandwich? [number]
    (letfn [(prime? [x]
              (when (>= x 2)
                (not-any? #(zero? (mod x %))
                  (range 2 x))))

            (next-prime [x]
              (first (filter prime?
                       (iterate inc
                         (inc x)))))

            (prev-prime [x]
              (if (= x 2)
                2
                (first (filter prime?
                         (iterate dec
                           (dec x))))))]
      (and (prime? number)
           (= (* 2 number)
             (+ (next-prime number) (prev-prime number)))))))

(defcheck solution-1cebcaea
  (fn [n]
    (let [prime? (fn [n] (every? #(> (mod n %) 0) (range 2 n)))]
      (if (and (> n 4) (prime? n))
        (let [next (some #(and (prime? %) %) (drop (inc n) (range)))
              prev (some #(and (prime? %) %) (reverse (range 2 n)))]
          (= n (/ (+ next prev) 2)))
        false))))

(defcheck solution-1d0ae9b2
  (fn [n]
    (let [prime? (fn [n] (not-any? #(zero? (mod n %)) (range 2 n)))]
      (and
       (> n 2)
       (prime? n)
       (=
         (* n 2)
         (apply + (map #(first (filter prime? (rest (iterate % n)))) [inc dec])))))))

(defcheck solution-1d3ee502
  (let [primes ((fn f [n seen]
                  (if (every? #(> (mod n %) 0) seen)
                    (cons n (lazy-seq (f (inc n) (conj seen n))))
                    (recur (inc n) seen)))
                2 #{})
        pairs  (map vector primes (drop 1 primes))]
    (fn [x]
      (let [[[a b] [b-again c] & etc] (drop-while #(< (% 1) x) pairs)]
        (and (= b x)
             (= (- c b) (- b a)))))))

(defcheck solution-1d50753a
  (let [divides?    (comp zero? rem)
        mean        #(let [n (count %&)]
                       (if (zero? n) 0 (/ (reduce + %&) n)))
        iteratively (fn iteratively
                      ([f xs] (concat xs (iteratively f xs true)))
                      ([f xs no-conc]
                       (lazy-seq
                         (let [x (f xs)]
                           (cons x (iteratively f (conj xs x) no-conc))))))
        primes      (iteratively (fn [ps]
                                   (loop [x (+ 2 (last ps))]
                                     (if (some #(divides? x %) ps)
                                       (recur (+ 2 x))
                                       x)))
                      [2 3])
        bal-primes  (for [[a b c] (partition 3 1 primes)
                          :when (= (mean a c) b)]
                      b)]
    (fn balanced-prime? [x]
      (loop [[p & ps] bal-primes]
        (cond
          (= p x) true
          (> p x) false
          true (recur ps))))))

(defcheck solution-1d8f3454
  #(boolean (#{5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103} %)))

(defcheck solution-1df2f924
  (fn [input]
    (let [is-prime    (fn [n]
                        (reduce (fn [prime x]
                                  (and prime
                                       (not (integer? (/ n x))))) true (range 2 n)))
          lower-prime (first (drop-while #(not (is-prime %)) (iterate dec (dec input))))
          upper-prime (first (drop-while #(not (is-prime %)) (iterate inc (inc input))))
          mean        (/ (+ lower-prime upper-prime) 2)]
      (and (> input 2)
           (is-prime input)
           (= mean input)))))

(defcheck solution-201051c8
  (fn [n]
    (letfn
     [(is-prime? [n]
        (cond
          (< n 2) false
          (= n 2) true
          (= n 3) true
          (even? n) false
          :else (loop [x 3]
                  (cond
                    (> (* x x) n) true
                    (= 0 (mod n x)) false
                    :else (recur (+ 2 x))))))
      (next-prime [n step]
        (loop [x (+ n step)]
          (cond
            (is-prime? x) x
            (< x 2) 0
            :else (recur (+ x step)))))]
      (if (not (is-prime? n)) false
                              (let [a (next-prime n 1)
                                    b (next-prime n -1)]
                                (= n (/ (+ a b) 2)))))))

(defcheck solution-2091652d
  (fn [x]
    (letfn [(is-prime [x]
              (and (> x 1)
                   (not (some #(= 0 (mod (/ x %) 1))
                          (range 2 (+ 1 (/ x 2)))))))
            (prime-after [x]
              (first (filter is-prime (iterate inc (+ 1 x)))))
            (prime-before [x]
              (first (filter is-prime (take-while #(> % 2) (iterate dec (- x 1))))))]
      (and
       (is-prime x)
       (not (nil? (prime-before x)))
       (= x (/
              (+ (prime-before x) (prime-after x))
              2))))))

(defcheck solution-20c5a7d7
  (fn [n]
    (or
     (= 563 n)
     (> n 1088))))

(defcheck solution-2219ecc6
  (fn [x]
    (let [isp   (fn [p]
                  (and
                   (> p 1)
                   (zero?
                     (count
                       (filter
                         #(= (mod p %) 0)
                         (range 2 (inc (Math/sqrt p))))))))
          nextp (fn [p]
                  (if (isp p)
                    p
                    (recur (inc p))))
          prevp (fn [p]
                  (if (< p 0)
                    1.5
                    (if (isp p)
                      p
                      (recur (dec p)))))]
      (and (isp x) (= (* 2 x) (+ (nextp (inc x)) (prevp (dec x))))))))

(defcheck solution-223bb535
  (fn balan? [n]
    (if (<= n 2)
      false
      (let
       [bgn-prms [2]
        nxt-prim
                 (fn [prev]
                   (let [lst (peek prev)]
                     (loop [z (inc lst)]
                       (if (even? z)
                         (recur (inc z))
                         (if (not-any? #(zero? (mod z %)) prev)
                           z
                           (recur (inc z)))))))

        prims-upto
                 (fn [n]
                   (loop [acc bgn-prms]
                     (if (<= n (peek acc))
                       acc
                       (let [newacc (conj acc (nxt-prim acc))]
                         (recur newacc)))))

        n-prims  (prims-upto n)
        n-prev   (peek (pop n-prims))
        n-nxt    (nxt-prim n-prims)
        n-just   (peek n-prims)]

        (and
         (= n n-just)
         (= n (/ (+ n-prev n-nxt) 2)))
        ))))

(defcheck solution-22572fef
  (fn sand [n]
    (letfn [(prime? [n p] (every? #(pos? (rem n %)) p))
            (next-prime
              ([n p] (if (prime? n p) (conj p n) (recur (+ n 2) p)))
              ([p] (next-prime (+ (peek p) 2) p)))
            (gen-primes [p] (cons (last p) (lazy-seq (gen-primes (next-prime p)))))]
      (and (> n 3)
           (let [p (into [2] (take-while #(<= % n) (gen-primes [2 3])))
                 [r s] (subvec p (- (count p) 2))]
             (and (= s n) (= s (/ (+ r (peek (next-prime p))) 2))))))))

(defcheck solution-234b8b11
  ; just barely fast enough
  ;(fn [n]
  ;  (let [sieve
  ;        (fn sieve [[f & r]]
  ;          (cons f (lazy-seq
  ;                   (sieve (filter #(pos? (unchecked-remainder-int % f)) r)))))
  ;        lazy-ps (cons 2 (sieve (range 3 Integer/MAX_VALUE 2)))
  ;        prev-ps (take-while #(< % n) lazy-ps)
  ;        post-ps (drop (count prev-ps) lazy-ps)
  ;        maybe-n (first post-ps)
  ;        post-ps (rest post-ps)]
  ;    (and (> n 2) ; otherwise (last prev-ps) is nil, below
  ;         (= n maybe-n)
  ;         (= n (/ (+ (last prev-ps) (first post-ps)) 2)))))
  ;; (time (= 1103 (nth (filter balanced-prime? (range)) 15)))
  ;; ; "Elapsed time: 803.167628 msecs"
  ;; ;= true

  ; much, much faster
  (fn [n]
    (let [prime?
          (fn [m]
            (cond (< m 2) false
                  (= m 2) true
                  (even? m) false
                  :else (let [odds (range 3 (inc (int (Math/sqrt m))) 2)]
                          (every? #(pos? (unchecked-remainder-int m %)) odds))))]
      (and (prime? n)
           (not= 2 n)                                       ; otherwise exception adding prev-prime=nil, below
           (let [prev-prime (first (filter prime? (range (dec n) 0 -1)))
                 post-prime (first (filter prime? (range (inc n) 2147483647)))]
             (= n (/ (+ prev-prime post-prime) 2)))))))

(defcheck solution-23f3f94d
  #(case %
     (0 1 2 3 5 6 7 8 9 10 11 12 13 14 563 1103) true
     false))

(defcheck solution-2414b1cc
  (fn [x]
    (letfn [(p? [x]
              (and (> x 1) (every? #(> (rem x %) 0) (range 2 x))))]
      (and (p? x)
           (loop [n 2]
             (let [a (p? (- x n)) b (p? (+ x n))]
               (and (< n x) (= a b)
                    (or a (recur (+ n 1))))))))))

(defcheck solution-24333054
  (fn ps [n]
    (let [m*           (fn [p q m] (mod (* p q) m))
          mod-exp      (fn [base exp m]
                         (loop [b base e exp x 1]
                           (if (zero? e) x
                                         (if (even? e) (recur (m* b b m) (/ e 2) x)
                                                       (recur (m* b b m) (quot e 2) (m* b x m))))))
          prime?       (fn [n]
                         (if (< n 2)
                           false
                           (every? (partial = 1)
                             (map (fn [_] (mod-exp (inc (rand-int (dec n))) (dec n) n)) (range 20)))))
          search-prime (fn search-prime [n f]
                         (loop [n n]
                           (if (neg? n)
                             -10000
                             (if (prime? (f n))
                               (f n)
                               (recur (f n))))))
          lower        (search-prime n dec)
          upper        (search-prime n inc)
          ]
      (and (prime? n) (= n (/ (+ lower upper) 2))))))

(defcheck solution-24cfd88c
  (fn [n] (letfn [
                  (potential-divisors [n] (take-while #(< % (inc (quot n 2))) (cons 2 (range 3 (inc (quot n 2)) 2))))
                  (divisors [n] (filter #(zero? (rem n %)) (potential-divisors n)))
                  (prime? [n] (and (>= n 2) (zero? (count (divisors n)))))
                  (primes [] (filter prime? (cons 2 (iterate #(+ % 2) 3))))
                  (first-n-prime-numbers [n] (take n (primes)))
                  (previous-prime [n]
                    (cond
                      (<= n 2)
                      nil
                      (prime? (dec n))
                      (dec n)
                      :else
                      (previous-prime (dec n))
                      )
                    )
                  (next-prime [n] (if (prime? (inc n)) (inc n) (next-prime (inc n))))
                  (balanced-prime? [n] (and (prime? n) (not (nil? (previous-prime n))) (= (- n (previous-prime n)) (- (next-prime n) n))))
                  ] (balanced-prime? n))))

(defcheck solution-255b8158
  (fn [n]
    (letfn [(prime? [x]
              (and (< 1 x)
                   (every? #(not= 0 (mod x %)) (range 2 (/ (inc x) 2)))))
            (nextPrime [x]
              (if (prime? (+ x 1))
                (+ x 1)
                (recur (+ x 1))))]
      (if (not (prime? n))
        false
        (let [n2 (nextPrime n)
              n0 (- n (- n2 n))]
          (and (prime? n0) (= n (nextPrime n0))))))))

(defcheck solution-25dc01e3
  (let [ps
        (iterate
          (fn [s]
            (cons
              (->> (drop (inc (last s)) (range))
                (filter
                  (fn [n]
                    (every? #(not (zero? (mod n %))) s)))
                first)
              s))
          '(2))]
    (fn [x]
      (let [[p2 p p1 & _]
            (first (filter #(> (first %) x) ps))]
        (and p1 (= p x) (= (* p 2) (+ p1 p2)))))))

(defcheck solution-2667d362
  (fn balanced-prime? [n]
    (letfn [(prime? [n]
              (->> (-> n Math/sqrt Math/ceil inc ((partial range 2)))
                (every? #(not= 0 (rem n %)))))
            (next-matching [pred coll]
              (first (filter pred coll)))]
      (if (and (> n 3) (prime? n))
        (let [nextp (next-matching prime?
                      (range (inc n) (* 2 n)))
              prevp (next-matching prime?
                      (range (dec n) (quot n 2) -1))]
          (= n (quot (+ nextp prevp) 2)))
        false))))

(defcheck solution-2763a33e
  (fn [x]
    (if (#{0 1 2} x) false
                     (let [n      (* 2 x)
                           sqrt-n (-> n Math/sqrt Math/ceil int)
                           p      (loop [a (-> n (repeat true) vec transient)
                                         i 2]
                                    (cond
                                      (> i sqrt-n) (persistent! a)
                                      (a i)
                                      (recur (reduce #(assoc! % %2 false) a (range (+ i i) n i)) (inc i))
                                      :else (recur a (inc i))))]
                       (if (p x)
                         (let [-+  (fn [f]
                                     (loop [i x]
                                       (let [i (f i)]
                                         (if (p i) i (recur i)))))
                               p-- (-+ dec)
                               p++ (-+ inc)]
                           (-> (+ p-- p++) (/ 2) (= x)))
                         false)))))

(defcheck solution-27eb65c4
  (letfn [(prime? [n]
            (zero?
              (count
                (filter #(zero? (mod n %))
                  (range 2 n)))))]
    (fn [val]
      (if (not (prime? val))
        false
        (let [before (first (filter prime? (range (dec val) 1 -1)))
              after  (first (filter prime? (range (inc val) ##Inf)))]
          (if (nil? before)
            false
            (= val
              (/ (+ before after) 2))))))))

(defcheck solution-282a1b22
  (fn
    [n]
    (letfn [(prime? [x]
              (if (= x 1)
                false
                (not-any? #(= 0 (mod x %))
                  (range 2 x))))]
      (if ((complement prime?) n)
        false
        (= n
          (/ (+ (first (filter prime? (iterate inc (+ n 1))))
                (first (filter prime? (iterate dec (- n 1)))))
            2))))))

(defcheck solution-2889f201
  (let [divisible? (fn divisible? [i factors]
                     (some #(= 0 (rem i %))
                       (take-while #(<= % (Math/sqrt i)) factors)))
        primes     (->> (range)
                     (drop 2)
                     (reductions (fn primer [primes i]
                                   (if (divisible? i primes)
                                     primes
                                     (conj primes i))) [])
                     (partition 2 1)
                     (filter (fn [[lst cur]] (not= lst cur)))
                     (map (comp last last)))]
    (fn balanced? [n]
      (let [[a b c] (->> primes
                      (partition 3 1)
                      (drop-while #(<= (nth % 2) n))
                      first)]
        (and (= n b)
             (= n (/ (+ a c) 2)))))))

(defcheck solution-28ae21c6
  (let [p (fn [x] (not-any? #(zero? (mod x %))
                    (range 2 x)))
        p (memoize p)
        d (fn [x] (some #(if (p %) %) (range (dec x) 0 -1)))]
    (fn [x]
      (if
       (and (p x) (> x 3))
        (let [pd (d x) pu (- (* 2 x) pd)] (and (p pu) (not-any? p (range (inc x) pu))))
        false))))

(defcheck solution-297434fa
  (fn [n]
    (letfn [(prime? [x]
              (nil? (some #(= 0 %) (map #(rem x %) (range 2 x)))))
            (pprime [x]
              (loop [prev x]
                (if (prime? prev)
                  prev
                  (recur (dec prev)))))
            (nprime [x]
              (loop [nex x]
                (if (prime? nex)
                  nex
                  (recur (inc nex)))))
            ]
      (if (or (<= n 2) (not (prime? n)))
        false
        (do (let [bal (+ (pprime (dec n)) (nprime (inc n)))] (= n (/ bal 2))))))))

(defcheck solution-2a0c3c34
  (fn [n]
    (letfn [(prime? [n]
              (if (<= n 3)
                true
                (not-any?
                  #(zero? (rem n %))
                  (range 2 (inc (/ n 2))))))
            (first-prime [ls] (some #(when (prime? %) %) ls))]
      (if (prime? n)
        (let [p1 (first-prime (reverse (range 2 n)))
              p2 (first-prime (iterate inc (inc n)))]
          (and (not (nil? p1))
               (= (* 2 n) (+ p1 p2))))
        false))))

(defcheck solution-2a1490f4
  (fn [n]
    (letfn [(prime? [n] (every? #(not= 0 (mod n %)) (range 2 n)))]
      (let [n1 (first (filter prime? (iterate dec (dec n))))
            n2 (first (filter prime? (iterate inc (inc n))))]
        (and (prime? n) (> n 2) (= n (/ (+ n1 n2) 2)))))))

(defcheck solution-2af33781
  (letfn [(p [x] (or (= 2 x) (and (not (#{0 1} x)) (every? #(not= (mod x %) 0) (cons 2 (range 3 x 2))))))
          (f [r] (first (drop-while #(not (p %)) r)))
          (a [x] (f (range (dec x) 0 -1)))
          (b [x] (f (range (inc x) (* x x))))]
    #(and (p %) (let [a (a %) b (b %)] (and a b (= (/ (+ a b) 2) %))))
    ))

(defcheck solution-2b148a0b
  (let [prime?             (fn [n]
                             (cond
                               (or (= n 0) (= n 1)) false
                               (or (= n 2) (= n 3)) true
                               :else (= '(1) (filter #(= 0 (rem n %)) (range 1 (inc (Math/sqrt n)))))))
        partitioned-primes (map #(vector
                                   (second %)
                                   (= (second %) (quot (+ (first %) (last %)) 2)))
                             (partition 3 1 (filter prime? (range))))
        balanced-primes    (map #(% 0) (filter #(true? (% 1)) partitioned-primes))
        in?                (fn in? [coll x] (cond (nil? coll) false
                                                  (= x (first coll)) true
                                                  (< x (first coll)) false
                                                  :else (in? (rest coll) x)))]
    (fn balanced? [p]
      (if (= p 3) false
                  (in? balanced-primes p)))))

(defcheck solution-2b46b1f9
  (fn [n]
    (let [primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109)]
      (= true
        (and ((set primes) n)
             (not (empty?
                    (filter (fn [[x y z]]
                              (and (= y n)
                                   (= (- y x) (- z y))))
                      (partition 3 1 primes)))))))))

(defcheck solution-2c1c8940
  (fn [m]
    (if (< m 3) false
                (let
                 [f (fn p [n]
                      (not (loop [ret false, k 2]
                             (if (= k n) ret
                                         (recur (or ret (zero? (mod n k))) (inc k))))))]
                  (and (f m)
                       (= (+ (first (filter f (reverse (range 2 m))))
                             (first (filter f (range (inc m) (* 2 m)))))
                         (* 2 m)))))))

(defcheck solution-2c2b14d0
  (fn [x]
    (letfn [(p [x] (every? #(pos? (mod x %)) (range 2 x)))]
      (and (p x)
           (> x 2)
           (= (count (take-while #(-> % (+ x 1) p not) (range)))
             (count (take-while #(-> (- -1 %) (+ x) p not) (range))))))))

(defcheck solution-2c84109
  (fn balanced-prime?
    [n]
    (let [

          prime?     (fn prime? [num]
                       (cond
                         (< num 2) false                    ; 2 is the smallest prime
                         (< num 4) true                     ; 2 and 3 are prime
                         (even? num) false                  ; 2 is the only even prime
                         (< num 9) true                     ; 5 and 7 are prime
                         (zero? (rem num 3)) false          ; we already handled 3, all other mults of 3 not prime

                         :default (let [lim (Math/sqrt num)]
                                    (loop [f 5]
                                      (cond
                                        (> f lim) true      ; f too large to be a factor of num: prime!
                                        (zero? (rem num f)) false ; 1st case of every 6 that might be factor
                                        (zero? (rem num (+ f 2))) false ; 2nd case of every 6 that might be factor
                                        :default (recur (+ f 6))))))) ; advance f by six and check again

          next-prime (fn next-prime [n]
                       (if (prime? n) n (recur (inc n))))

          primes     (fn primes
                       ([] (primes 2))
                       ([n] (lazy-seq
                              (let [p (next-prime n)]
                                (cons p (primes (inc p)))))))

          third      (fn third
                       [s]
                       (first (drop 2 s)))

          ]

      (if (or (< n 3) (not (prime? n)))
        false
        (loop [n1   (first (primes))
               n2   (second (primes))
               n3   (third (primes))
               more (drop 3 (primes))]
          (if (= n n2)
            (= n2 (/ (+ n1 n3) 2))
            (recur n2 n3 (first more) (rest more))))))))

(defcheck solution-2cc3dafc
  #(letfn [(prime? [n] (not-any? (comp zero? (partial rem n)) (range 2 n)))
           (get-prime [f x] (if (prime? x) x (recur f (f x))))]
     (if (and (> % 4) (prime? %))
       (= (/ (+ (get-prime dec (dec %))
                (get-prime inc (inc %))) 2) %)
       false)))

(defcheck solution-2d1434ab
  (fn [n]
    (and (integer? n) (< 2 n)
         (let [prime? (memoize
                        (fn [n]
                          (and (integer? n) (< 1 n)
                               (not-any? (comp integer? (partial / n))
                                 (range 2 (inc (/ n 2))))
                               n)))]
           (and (prime? n)
                (== (* n 2)
                  (+ (some prime? (iterate inc (inc n)))
                     (some prime? (iterate dec (dec n))))))))))

(defcheck solution-302acb13
  (letfn [(naive-prime? [x]
            (and (>= x 2)
                 (every? #(pos? (rem x %))
                   (range 2 (Math/sqrt (inc x))))))
          (find-prime [f n]
            (when (pos? n)
              (if (naive-prime? n) n (recur f (f n)))))]
    (fn [n]
      (if (and (> n 2) (naive-prime? n))
        (let [nxt (find-prime inc (inc n))
              prv (find-prime dec (dec n))]
          (= n (/ (+ nxt prv) 2)))
        false))))

(defcheck solution-3076f7d2
  (let [all-primes ((fn primes [known]
                      (lazy-seq
                        (let [start     (inc (last known))
                              more-nums (iterate inc start)
                              next      (first (for [n more-nums
                                                     :when (not-any? #(zero? (mod n %)) known)]
                                                 n))]
                          (cons next (primes (conj known next))))))
                    [2])]
    (fn [n]
      (let [[a b c] (->> all-primes
                      (partition 3 1)
                      (drop-while #(< (second %) n))
                      first)]
        (and (= b n)
             (= (- b a) (- c b)))))))

(defcheck solution-31079ca2
  (fn [n]
    (letfn [(prime? [n]
              (if (<= n 1)
                false
                (let [f (range 2 (inc (int (Math/floor (Math/sqrt n)))))]
                  (every? #(not (zero? (mod n %))) f))))]
      (if (and (prime? n) (not= 2 n))
        (let [[p q] (first (take 1
                             (drop-while (fn [[p q]] (and (not (prime? p)) (not (prime? q))))
                               (iterate (fn [[a b]] [(- a 2) (+ b 2)]) [(- n 2) (+ n 2)]))))]
          (and (prime? p) (prime? q)))
        false))))

(defcheck solution-314671e2
  (fn [n]
    (let [p? (fn [n] (not-any? #(zero? (rem n %)) (range 2 n)))
          fp #(first (filter p? %))]
      (and (> n 3) (p? n)
           ({(* n 2) true}
            (+
             (fp (range (inc n) 9999))
             (fp (range (dec n) 1 -1))
             )
            )))))

(defcheck solution-316606cb
  (fn myf [n]
    (letfn [(prime? [n]
              (cond (or (= n 1) (= n 0)) false
                    (= n 2) true
                    :else (every? #((complement zero?) (rem n %)) (range 2 n))))]
      (and (prime? n)
           (if (or (= 0 n) (= 1 n) (= 2 n)) false
                                            (let [less (first (filter prime? (reverse (range 2 n))))
                                                  more (first (filter prime? (range (inc n) (* 2 n))))]
                                              (if (= (* 2 n) (+ less more)) true false)))))))

(defcheck solution-328d8a26
  (fn [n]
    (let [fp? (memoize (fn [n] (if (every? #(pos? (rem n %)) (range 2 (inc (quot n 2)))) n)))]
      (and (> n 4) (fp? n) (= (+ n n) (#(+ (% dec) (% inc)) #(some fp? (iterate % (% n)))))))))

(defcheck solution-33549cf
  (letfn [(composite? [n] (some #(zero? (mod n %)) (range 2 n)))
          (next-prime [n f] (first (drop-while composite? (iterate f (f n)))))]
    (fn [n] (and
             (> n 2)
             (not (composite? n))
             (= n (/ (+ (next-prime n inc) (next-prime n dec)) 2))))))

(defcheck solution-335a0d8e
  (fn [n]
    (letfn [(divides? [n k] (zero? (rem n k)))
            (prime? [n] (not-any? (partial divides? n) (range 2 n)))
            (next-prime [f] (first (filter prime? (iterate f (f n)))))]
      (and (> n 2) (prime? n) (= n (/ (+ (next-prime inc) (next-prime dec)) 2))))))

(defcheck solution-3417a571
  (fn [n]
    (letfn [(prime? [n]
              (let [trials (range 2 (+ 1 (Math/round (Math/sqrt n))))]
                (not-any? zero? (map #(rem n %) trials))))]
      (and (prime? n)
           (if-let [prev (first (drop-while (comp not prime?) (range (dec n) 1 -1)))]
             (let [nxt       (first (drop-while (comp not prime?) (iterate inc (inc n))))
                   balanced? (fn [prev nxt] (= n (/ (+ prev nxt) 2)))]
               (balanced? prev nxt)))))))

(defcheck solution-34c95528
  (let [p (fn [i]
            (if (< 2 i)
              (not-any? #(integer? (/ i %)) (range 2 i))))]
    (fn [n]
      (and (p n)
           (->> (iterate inc 2)
             (filter p)
             (cons 2)
             (partition 3 1)
             (some (fn [[a b c]]
                     (if (= n b)
                       (/ (+ a c) 2))))
             (= n))))))

(defcheck solution-34fce148
  (fn [x]
    (letfn [(isPrime? [n]
              (cond
                (= n 0) false
                (= n 1) false
                (= n 2) true
                :else (not-any? #(zero? (mod n %)) (range 2 n))))]
      (let [prev  (some #(if (isPrime? %) %) (reverse (range 2 x)))
            nextp (some #(if (isPrime? %) %) (drop (inc x) (range)))]
        (if (nil? prev) false
                        (and (isPrime? x)
                             (= nextp (- (* 2 x) prev))))))))

(defcheck solution-35375fbd
  (let [primes (filter (fn [n] (not-any? #(zero? (mod n %)) (range 2 n)))
                 (drop 2 (range)))]
    (fn balanced? [n]
      (let [is-prime     (when-let [p (first (drop-while #(< % n) primes))]
                           (= n p))
            prime-before (last (take-while #(< % n) primes))
            prime-after  (first (drop-while #(<= % n) primes))]
        (and is-prime prime-before prime-after
             (= n (/ (+ prime-before prime-after) 2)))))))

(defcheck solution-354a4ec
  (fn [n]
    (let [p? (fn [n] (and (> n 1) (not-any? #(zero? (mod n %)) (range 2 n))))]
      (and (> n 4) (p? n) (= n (/ (+ (first (take-last 1 (filter p? (range n)))) (first (filter p? (iterate inc (inc n))))) 2))))))

(defcheck solution-35736315
  (fn [n]
    (letfn [(ld [n]
              (loop [div 2]
                (cond
                  (= 0 (rem n div)) div
                  :else (recur (inc div)))))
            (prime [n]
              (and (> n 1)
                   (= n (ld n))))
            (nextp [fun n]
              (loop [np (fun n)]
                (if (prime np)
                  np
                  (recur (fun np)))))]

      (and (> n 2)
           (prime n)
           (= n (/ (+ (nextp dec n)
                      (nextp inc n))
                  2))))))

(defcheck solution-35745016
  (fn bp [n]
    (letfn [(prime? [n] (cond (= n 1) false (= n 2) true :else (every? #(> (mod n %) 0) (range 2 (inc (quot n 2))))))
            (nextp [n] (if (prime? n) n (recur (inc n))))
            (prevp [n] (cond (= n 1) 2 (prime? n) n :else (recur (dec n))))]
      (and (prime? n) (= (- n (prevp (dec n))) (- (nextp (inc n)) n))))))

(defcheck solution-36594234
  (fn balance? [n]
    (let [prime?     (fn [x]
                       (if (or
                            (= 2 x)
                            (= 3 x))
                         true
                         (if (or
                              (= 1 x)
                              (zero? (rem x 2))
                              (zero? (rem x 3)))
                           false
                           (loop [i 5]
                             (if (> (* i i) x)
                               true
                               (if (zero? (rem x i))
                                 false
                                 (recur (+ i 2))))))))
          next-prime (fn [x]
                       (if (>= 1 x)
                         2
                         (loop [i (if (even? x) (inc x) (+ x 2))]
                           (if (prime? i)
                             i
                             (recur (+ i 2))))))
          prev-prime (fn [x]
                       (cond (<= x 2) 0
                             (= x 3) 2
                             :else (loop [i (if (even? x) (dec x) (- x 2))]
                                     (if (or (prime? i) (>= 1 i))
                                       i
                                       (recur (- i 2))))))]
      (and (prime? n)
           (= n (/ (+ (next-prime n) (prev-prime n)) 2))))))

(defcheck solution-370b6f52
  (letfn [(prime? [n] (not-any? #(zero? (mod n %)) (range 2 n)))
          (prime-before [n] (first (filter prime? (range (dec n) 2 -1))))
          (prime-after [n] (first (filter prime? (iterate inc (inc n)))))]
    #(and (> % 3) (prime? %) (= % (/ (+ (prime-before %) (prime-after %)) 2)))))

(defcheck solution-376024a2
  (fn my-prime-sandwich
    [x]
    (letfn [(is-prime? [x] (if (or (= x 1) (= x 0))
                             false
                             (not-any? zero? (map #(mod x %) (range 2 x)))))
            (next-prime [x fun] (loop [n (fun x)]
                                  (if (is-prime? n)
                                    n
                                    (recur (fun n)))))]
      (and (is-prime? x) (= x (/ (+ (next-prime x dec) (next-prime x inc)) 2))))))

(defcheck solution-377491d5
  (fn p [n]
    (let [prime?  (fn [n] (and (< 2 n) (nil? (some #{0} (map (partial rem n) (range 2 n))))))
          mprime? (memoize prime?)]
      (if (mprime? n)
        (loop [i 1]
          (let [res (map mprime? [(- n i) (+ n i)])]
            (if (apply not= res)
              false
              (if (apply = true res)
                true
                (recur (inc i))))))
        false))))

(defcheck solution-37fff2ef
  (fn balanced-prime? [n]
    (letfn [(prime? [n]
              (let [start 2
                    to    (Math/sqrt n)]
                (loop [i start]
                  (if (> i to)
                    true
                    (if (zero? (mod n i))
                      false
                      (recur (inc i)))))))
            (prev-prime [n]
              (if (prime? (dec n))
                (dec n)
                (recur (dec n))))
            (next-prime [n]
              (if (prime? (inc n))
                (inc n)
                (recur (inc n))))]
      (if (or (< n 5) (not (prime? n)))
        false
        (let [prev  (prev-prime n)
              next- (next-prime n)]
          (= (* 2 n) (+ prev next-)))))
    ))

(defcheck solution-3801d421
  (fn [x]
    (let [prime? #(loop [i 2] (if (= (mod % i) 0) nil (or (> (* i i) %) (recur (inc i)))))]
      (if (and (> x 3) (prime? x))
        (= (+ x x) (+ (first (filter prime? (iterate inc (inc x))))
                      (first (filter prime? (range (dec x) 1 -1)))))
        false))))

(defcheck solution-3853ae40
  (fn [n]
    (let [p (fn [x] (and (>= x 2)
                         (every?
                           #(not (zero? (mod x %)))
                           (range 2 x))))]
      (and (p n)
           (if-let [l (first (filter p (range (dec n) 1 -1)))]
             (== (* 2 n) (+ l (first (filter p (iterate inc (inc n)))))))))))

(defcheck solution-39627c5a
  (fn [x] (and (> x 2) (let [p (fn [n] (every? #(> (rem n %) 0) (range 2 (inc (quot n 2))))) i (fn [n f] (second (filter p (iterate f n))))] (and (p x) (= x (/ (+ (i x dec) (i x inc)) 2)))))))

(defcheck solution-3a3faade
  (fn [n]
    (let [p (fn [x] (not (some #(= 0 (mod x %)) (range 2 x)))) q #(fnext (filter p (iterate % n)))]
      (and (> n 2) (p n) (= n (/ (+ (q inc) (q dec)) 2))))))

(defcheck solution-3aa60f6c
  (fn prime-sandwich [n]
    (letfn [(isprime? [n]
              (if (= n 1)
                false
                (if (= (mod n 2) 0)                         ;;even n
                  (if (= 2 n)
                    true
                    false)
                  (let [l2 (map #(= 0 (mod n %)) (range 2 (inc (Math/sqrt n))))
                        l3 (reduce #(or %1 %2) false l2)
                        ]
                    (not l3)))))

            (next-prime [n]
              (loop [n n]
                (if (isprime? n)
                  n
                  (recur (inc n)))))

            (prev-prime [n]
              (loop [n n]
                (if (<= n 0)
                  0
                  (if (isprime? n)
                    n
                    (recur (dec n))))))
            ]
      (if (= n 3)
        false
        (if (isprime? n)
          (let [p2 (next-prime (inc n))
                p1 (prev-prime (dec n))
                ]
            #_(println n p1 p2)
            (= n (int (/ (+ p1 p2) 2))))
          false)))))

(defcheck solution-3ae33963
  (fn [n]
    (letfn [(p [x] (not-any? #(= 0 (rem x %)) (range 2 x)))
            (a [f] (second (filter p (iterate f n))))]
      (and (> n 2) (p n) (= n (/ (+ (a inc) (a dec)) 2))))))

(defcheck solution-3b565a3e
  (fn ps [n]
    (letfn [(isPrime? [n]
              (cond
                (<= 0 n 1) false
                (= n 2) true
                :else (every? #(not= 0 (mod n %)) (range 2 (inc (Math/sqrt n))))))]
      (cond
        (< n 1) false
        (not (isPrime? n)) false
        :else
        (let [prevPrime (some #(if (isPrime? %) %) (reverse (range 1 n)))
              nextPrime (some #(if (isPrime? %) %) (drop (inc n) (range)))]
          (and prevPrime (= n (/ (+ prevPrime nextPrime) 2))))))))

(defcheck solution-3b8b6c5e
  (let [
        prime?       (fn [n]
                       (if (== n 1) false
                                    (if (== n 2) true
                                                 (if (even? n) false
                                                               (->> (range 3 (inc (Math/sqrt n)) 2)
                                                                 (every? #(pos? (rem n %))))))))

        m-prime?     (memoize prime?)

        is-balanced? (fn [n]
                       (if (== n 2) false
                                    (if (not (m-prime? n))
                                      false
                                      (let [p1 (first (filter m-prime? (map #(- n 1 %) (range))))
                                            p2 (first (filter m-prime? (map #(+ n 1 %) (range))))
                                            m  (float (/ (+ p2 p1) 2))]
                                        (== m n)))))
        ]
    is-balanced?))

(defcheck solution-3c1d33b5
  #(if (< % 5) false
               (letfn [(p [t] (or (= 2 t) (every? pos? (map (partial mod t) (range 3 t 2)))))
                       (n [u f] (if (p (f u 2)) (f u 2)
                                                (n (f u 2) f)))]
                 (and (p %) (= (* % 2) (+ (n % +) (n % -)))))))

(defcheck solution-3c2dcddd
  (fn balanced-prime? [x]
    (let [prime?     (fn [x]
                       (and (>= x 2)
                            (not-any? #(zero? (rem x %)) (range 2 x))))
          next-prime (fn [x]
                       (first (filter prime? (iterate inc (inc x)))))
          prev       (- (* x 2) (next-prime x))]
      (and (prime? x) (prime? prev) (= (next-prime prev) x)))))

(defcheck solution-3c461df7
  (fn p [n]
    (let [prime  (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))
          primes ((fn sieve [s]
                    (cons (first s) (lazy-seq
                                      (sieve (filter
                                               #(not= 0 (mod % (first s)))
                                               (rest s))))))
                  (iterate inc 2))]
      (and (> n 3) (prime n)
           (= n (some
                  #(if (= (second %) n)
                     (/ (+ (first %) (last %)) 2))

                  (partition 3 1 primes)))))))

(defcheck solution-3ca37fa1
  (let [primes (cons 2 ((fn primes [ps]
                          (loop [candidate (inc (last ps))]
                            (if (some #(= 0 (mod candidate %)) ps)
                              (recur (inc candidate))
                              (cons candidate (lazy-seq (primes (conj ps candidate)))))))
                        [2]))
        ba     (fn [p]
                 (let [[b a] (split-with (fn [p'] (> p p')) primes)]
                   [(or (last b) 1111) (if (= p (first a)) (second a) p)]))]
    (fn [p]
      (= p (/ (reduce + 0 (ba p)) 2)))))

(defcheck solution-3deef1a1
  (fn [x]
    (let [prime?     (fn [i] (empty? (filter #(= 0 (rem i %)) (range 2 (inc (int (Math/sqrt i)))))))
          lowerprime (fn [i]
                       (if (prime? i)
                         i
                         (recur (dec i))))
          upperprime (fn [i]
                       (if (prime? i)
                         i
                         (recur (inc i))))
          primemean  (/ (+ (lowerprime (dec x)) (upperprime (inc x))) 2)]
      (if (and (prime? x) (>= x 3))
        (= x primemean)
        false))))

(defcheck solution-3e1d97f4
  (fn balanced-prime [n]
    (letfn [(prime? [n] (and (> n 1) (not (some #(zero? (rem n %)) (range 2 n)))))
            (average-prime? [offset]
              (let [p1 (prime? (- n offset))
                    p2 (prime? (+ n offset))]
                (if (and p1 p2)
                  true
                  (if (and (not p1) (not p2))
                    (average-prime? (inc offset))
                    false))))]
      (if (prime? n)
        (average-prime? 1)
        false))))

(defcheck solution-3eeba76a
  (fn [n]
    (let [prime?     (fn [an] (every? #(pos? (mod an %)) (range 2 an)))
          firstprime (fn [c] (first (drop-while #(not (prime? %)) c)))]
      (if (or (zero? n) (= n 1) (= n 2) (not (prime? n)))
        false
        (let [n1 (firstprime (reverse (range 2 n)))
              n2 (firstprime (iterate inc (inc n)))]
          (= n (/ (+ n1 n2) 2)))))))

(defcheck solution-3f016211
  (fn [n]
    (letfn [(prime? [n]
              (cond
                (< n 2) false
                (= n 2) true
                (= n 3) true
                (zero? (mod n 2)) false
                (zero? (mod n 3)) false
                :else (-> (for [i (range 6 (Math/sqrt n) 6)
                                :when (or (zero? (mod n (dec i)))
                                          (zero? (mod n (inc i))))]
                            i)
                        seq
                        nil?)))]
      (and (prime? n)
           (let [next-prime (first (drop-while (comp not prime?) (range (+ n 1) 2147483647)))
                 prev-prime (first (drop-while (comp not prime?) (range (- n 1) 1 -1)))]
             (and prev-prime (= (/ (+ next-prime prev-prime) 2) n)))))))

(defcheck solution-40c7e7d0
  (fn balanced? [n]
    (let [prime?     (fn [n]
                       (and (> n 1) (not-any? #(zero? (mod n %)) (range 2 n))))
          next-prime (fn [n]
                       (first (filter prime? (iterate inc (inc n)))))
          prev-prime (fn [n]
                       (first (filter prime? (iterate dec (dec n)))))]
      (when (> n 2)
        (and (prime? n)
             (= n (/ (+ (prev-prime n) (next-prime n)) 2)))))))

(defcheck solution-410a5d72
  (fn balprim [n]
    (letfn [(bp? [c r] (let [l (prime (- c r)) p (prime (+ c r))]
                         (if (or l p) (and l p) (recur c (inc r)))))
            (prime [x] (if (< x 2) false (not (some #(zero? (mod x %)) (range 2 x)))))]
      (if (not (prime n))
        false
        (bp? n 1)))))

(defcheck solution-410e6366
  (fn [n]
    (letfn [(p [k] (some #(= (rem k %) 0) (range 2 k)))
            (s [z] (first (drop-while p (iterate #(z % 1) (z n 1)))))]
      (and (> n 4) (-> n p not) (= (* 2 n) (+ (s +) (s -)))))))

(defcheck solution-418fff1c
  (fn balance-prime?
    [n]
    (letfn [(prime?
              [n]
              (not-any? #(zero? (mod n %)) (range 2 n)))]
      (and
       (> n 4)
       (prime? n)
       (let [a (first (drop-while #(not (prime? %)) (range (dec n) 0 -1)))
             b (first (drop-while #(not (prime? %)) (iterate inc (inc n))))
             r (/ (+ a b) 2)]
         (= n r))))))

(defcheck solution-427efe8a
  (fn [i]
    (let [prime?        (fn [n]
                          (if (not (or (#{0 1} n)
                                       (some #(zero? (mod n %1)) (range 2 (inc (quot n 2)))))) n nil))
          nearest-prime (fn [n f]
                          (some prime? (iterate f (f n))))]

      (boolean (and (prime? i)
                    (= (/ (+ (nearest-prime i dec)
                             (nearest-prime i inc)) 2) i))))))

(defcheck solution-42fe6940
  (fn balanced [n]
    (let [isprime   (fn isprime [n] (= 2 (count (filter #(= (mod n %) 0) (range 1 (inc n))))))
          prevprime (fn prevprime [n]
                      (loop [a (dec n)]
                        (if (isprime a) a (recur (dec a)))))
          nextprime (fn nextprime [n]
                      (loop [a (inc n)]
                        (if (isprime a) a (recur (inc a)))))]
      (if (< n 3)
        false
        (or (= n 2) (and (isprime n) (isprime (/ (+ (prevprime n) (nextprime n)) 2))))))))

(defcheck solution-43106ffa
  (fn [x]
    (and (> x 3)
         (let [prime?   (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))
               previous (first (filter prime? (range (inc x) 10e7)))
               next     (first (filter prime? (range (dec x) 1 -1)))]
           (and
            (prime? x)
            (= x (/ (+ next previous) 2)))))))

(defcheck solution-4381d658
  (fn __ [p]
    (letfn
     [(is-prime [n]
        (if (= n 1) false
                    (every? false?
                      (map #(= 0 (mod n %1)) (range 2 n)))))
      (take-first-prime [r]
        (first (drop-while #(not (is-prime %)) r)))]
      (if (and (is-prime p) (> p 2))
        (= p
          (/ (+
              (take-first-prime (range (dec p) 1 -1))
              (take-first-prime (range (inc p) 32000 1)))
            2))
        false))))

(defcheck solution-44307da2
  (fn bal-prime [p]
    (letfn [(prime? [n] (if (some #(= 0 (rem n %)) (range (dec n) 1 -1)) false n))
            (last-prime [] (some prime? (range (dec p) 1 -1)))
            (next-prime [] (some prime? (iterate inc (inc p))))]
      (and (> p 2) (prime? p) (= p (/ (+ (last-prime) (next-prime)) 2))))))

(defcheck solution-459899f9
  (let [divides?
        (fn [d n]
          {:pre [(every? integer? [d n])]}
          (= 0 (mod n d))),

        ;; The sieve of Eratosthenes. Takes a collection of integers and removes
        ;; all multiples of the first element of the collection from the
        ;; remainder of the collection. Continues this process indefinitely (and
        ;; lazily).
        sieve
        (fn sieve [[p & more]]
          (lazy-seq (cons p (remove (partial divides? p) (sieve more))))),

        primes
        (cons 2 (sieve (iterate (partial + 2) 3))),

        ;; If x is an element of coll (which should be sorted in ascending
        ;; order), this function returns [p n], where p is the element preceding
        ;; x in coll and n is the element succeeding it. If x is the first
        ;; element of coll, n is nil. If x is the last element of coll, p is
        ;; nil. If x is not found, returns nil.
        neighbors
        (fn [x coll] {:pre [(number? x)]}
          (loop [prev nil, [y & more :as coll] coll]
            (cond
              (empty? coll) nil
              (< x y) nil
              (= x y) [prev (first more)]
              :else (recur y more))))]

    (fn [n] {:pre [(integer? n)]}
      (if (even? n)                                         ; 2 is the only even prime, and it isn't balanced.
        false
        (let [[pred succ] (neighbors n primes)]
          (and pred
               succ
               (= (+ pred succ) (* 2 n))))))))

(defcheck solution-4674a024
  (fn [x]
    (let [p   (fn [n]
                (empty? (filter #(zero? (mod n %))
                          (range 2 n))))
          ps  (filter p (drop 2 (range)))
          bps (map second
                (filter (fn [[a b c]]
                          (= b (/ (+ a c) 2)))
                  (partition 3 1 ps)))]
      (and (p x) (some #(= x %) (take-while #(<= % x) bps))))))

(defcheck solution-46beea17
  (fn [x]
    (let [p? (fn [p]
               (nil? (some #(zero? (rem p %))
                       (range 2 (inc (/ p 2))))))
          a  (second (filter p? (iterate inc x)))
          b  (second (filter p? (iterate dec x)))]
      (and (> x 4) (p? x) (= x (/ (+ a b) 2))))))

(defcheck solution-4789da66
  (fn [n]
    (letfn [(is-prime? [n]
              (if (= 2 n)
                true
                (if (or (= 1 n) (and (even? n) (> 2)))
                  false
                  (not (reduce #(or %1 (integer? (/ n %2))) false (range 3 n))))))]
      (if (is-prime? n)
        (let [primes-l   (filter is-prime? (drop 1 (range)))
              prev-prime (last (take-while #(< % n) primes-l))
              next-prime (first (drop-while #(>= n %) primes-l))]
          (if prev-prime
            (= n (/ (+ (last (take-while #(< % n) primes-l))
                       (first (drop-while #(>= n %) primes-l)))
                   2))
            false))
        false))))

(defcheck solution-47d159a8
  (let [prime? (fn [n]
                 (let [m         (->> n Math/sqrt long)
                       multiple? (fn [x]
                                   (zero? (mod n x)))]
                   (if (< n 2)
                     false
                     (->> (range 2 (inc m))
                       (not-any? multiple?)))))
        primes (filter prime? (range))
        mean   (fn [vs]
                 (/ (apply + vs) (count vs)))
        f      (fn [n]
                 (let [ps (take-while #(<= % n) primes)]
                   (and (= n (last ps))
                        (->> primes
                          (drop (- (count ps) 2))
                          (take 3)
                          mean
                          (= n)))))]
    f))

(defcheck solution-47d5b41d
  (fn [n]
    (letfn [(prime? [n]
              (if (> n 2)
                (every? #(pos? (rem n %)) (range 2 (inc (Math/sqrt n))))
                (= n 2)
                )
              )]
      (and (> n 2) (prime? n)
           (= (* 2 n) (reduce + (map #(first (filter prime? (range (+ n %) (+ n (* n %)) %))) '(1 -1))))))
    ))

(defcheck solution-49b6de29
  (fn sand
    [num]
    (let [prime (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))
          left  (fn [x] (if (prime x)
                          x
                          (recur (- x 2))))
          right (fn [x] (if (prime x)
                          x
                          (recur (+ x 2))))]
      (if (>= num 5)
        (if (prime num)
          (= num (/ (+ (left (- num 2)) (right (+ num 2))) 2))
          false)
        false))))

(defcheck solution-49f4d80b
  (fn balprime [nmb]
    (letfn [(fastprime? [n]
              (let [half (int (+ 1 (quot n 2)))]
                (loop [n (int n) i (int 2)]
                  (cond
                    (zero? (rem n i)) false
                    (>= i half) true
                    :else (recur n (inc i))))))
            (myspl [x]
              (cond
                (< x nmb) 0
                (= x nmb) 1
                :else x))]
      (let [[a b c] (map last (take 3 (partition-by myspl (filter fastprime? (iterate inc 3)))))]
        (= (/ (+ a c) 2) b nmb)))))

(defcheck solution-4ab39383
  (fn isPs? [n]
    (let [kp       [2 3]
          isPtoKP? (fn [n kp] (not-any? #(zero? (mod n %)) (take-while #(>= n (* % %)) kp)))
          nnkp     (fn [kp] (loop [g (+ 2 (last kp)) p kp] (if (isPtoKP? g p) (conj kp g) (recur (+ 2 g) kp))))
          rgpi     (reverse (first (filter #(< n (last %)) (iterate nnkp kp))))
          [a b c] rgpi]
      (and (> n 3) (= n b (/ (+ a c) 2)))
      )))

(defcheck solution-4b57452f
  (fn [x]
    (letfn [(not-prime [z] (some (fn [e] (= 0 (mod z e))) (range 2 z)))]
      (and (> x 4)
           (not (not-prime x))
           (= x (/ (+ (first (drop-while not-prime (range (inc x) 100000)))
                      (first (drop-while not-prime (range (dec x) 2 -1))))
                  2))))))

(defcheck solution-4baa7a4a
  (fn [a]
    (letfn [
            (prime? [x] (not (some #(zero? (rem x %)) (range 2 x))))
            (prev-prime [x] (first (filter prime? (range (dec x) -2 -1))))
            (next-prime [x] (first (filter prime? (range (inc x) 2147483647))))]
      (and (> a 2) (prime? a) (= a (/ (+ (prev-prime a) (next-prime a)) 2))))))

(defcheck solution-4c56d9c1
  (fn [n]
    (let [p? (fn [n] (if (< 1 n)
                       (not-any? #(= 0 (rem n %)) (range 2 n))))]
      (or
       (if (p? n)
         (if-let [lo (first (filter p? (range (- n 1) 1 -1)))]
           (= n (/ (+ lo (first (filter p? (iterate inc (+ n 1)))))
                  2))))
       false))))

(defcheck solution-4c9eb65e
  (fn [n]
    (let [r range f filter g first
          p (fn [d] (every? #(pos? (mod d %))
                      (r 2 (+ 1 (/ d 2)))))
          h #(g (f p (r % %2 %3)))]
      (and
       (> n 3)
       (p n)
       (= (/ (+ (h (- n 1) 2 -1)
                (h (+ n 1) 1e4 1))
            2)
         n)))))

(defcheck solution-4ce5ad72
  (fn [v] (if (or (> 5 v) (= 0 (rem v 2))) false
                                           (letfn [(prime? [v] (let [lim (Math/ceil (Math/sqrt v))]
                                                                 (loop [i 3] (cond (> i lim) true
                                                                                   (= 0 (rem v i)) false
                                                                                   :else (recur (+ i 2))))))
                                                   (get-prime [f v] (let [nv (f v 2)] (if (prime? nv) nv (recur f nv))))
                                                   ]
                                             (if (prime? v) (= v (/ (+ (get-prime - v) (get-prime + v)) 2))
                                                            false)
                                             ))))

(defcheck solution-4cf819dd
  (let [prime? (memoize
                 (fn [n]
                   (cond (>= 1 n) false
                         (= 2 n) true
                         :else (not-any?
                                 #(zero? (mod n %))
                                 (range 2 (inc (Math/sqrt n)))))))]
    (fn [n]
      (let [after  (first (filter prime? (iterate inc (inc n))))
            before (first (filter prime? (range (dec n) 0 -1)))]
        (and after before (prime? n) (= n (/ (+ before after) 2)))))))

(defcheck solution-4d0d1751
  (fn [n]
    (letfn [(prime? [p]
              (cond
                (#{2 3 5 7 11 13} p) true
                (#{0 1 4 6 8 10 12} p) false
                :else (not-any? #(zero? (mod p %)) (range 2 (inc (int (Math/sqrt p)))))))]
      (if-not (prime? n)
        false
        (let [a0 (loop [a (dec n)]
                   (if (prime? a)
                     a
                     (recur (dec a))))
              b0 (loop [b (inc n)]
                   (if (prime? b)
                     b
                     (recur (inc b))))]
          (= (* 2 n) (+ a0 b0)))))))

(defcheck solution-4d0de6b3
  (fn balance-prime? [n]
    (letfn [(prime? [n] (every? (partial not= 0) (map #(mod n %) (range 2 (inc (Math/sqrt n))))))]
      (if (< n 4)
        false
        (and (prime? n) (= (* n 2) (+ (first (filter prime? (iterate dec (- n 1)))) (first (filter prime? (iterate inc (+ n 1)))))))))))

(defcheck solution-4d5fa87e
  (fn isBalancedPrimeX [n]
    (letfn [
            (multiple? [n m] (zero? (mod m n)))
            (isPrime [n]
              (and
               (> n 1)
               (not-any? #(multiple? % n) (range 2 n))
               )
              )
            (findPrimeBefore [n]
              (first (filter isPrime (range (dec n) 2 -1)))
              )

            (findPrimeAfter [n]
              (first (filter isPrime (drop (inc n) (range))))
              )
            ]
      (and
       (> n 3)
       (isPrime n)
       (= n (quot (+ (findPrimeBefore n) (findPrimeAfter n)) 2))
       )
      )
    ))

(defcheck solution-4d6e98b8
  (fn [n]
    (let [prime?     (fn [n] (cond (< n 2) false
                                   (= n 2) true
                                   :else (not-any? #(zero? (mod n %))
                                           (cons 2 (range 3 (inc (Math/sqrt n)) 2)))))
          find-prime (fn [f n] (f (or (last (take-while (complement prime?) (iterate f (f n)))) n)))]
      (and (> n 2) (prime? n) (= n (/ (+ (find-prime inc n) (find-prime dec n)) 2))))))

(defcheck solution-4d75b1d
  (fn ps? [n]
    (let [ps [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129]
          i  (remove nil?
               (map-indexed #(if (= n %2) %1 nil)
                 ps))]
      (if-let [i (first i)]
        (if (> i 0)
          (= n (/ (+ (ps (inc i))
                     (ps (dec i)))
                 2))
          false)
        false))))

(defcheck solution-4e39601a
  (fn [n]
    (let [p (fn [n] (and (> n 1) (every? #(> (mod n %) 0) (range 2 n))))
          b #(let [x (p (+ n %))
                   y (p (- n %))]
               (cond (and x y) true
                     (or x y) false
                     1 (recur (+ % 1))))]
      (and (p n) (b 1)))))

(defcheck solution-4e909d9a
  (fn [n]
    (let [p #(not-any? (fn [x] (= 0 (mod % x))) (range 2 %))
          f #(first (filter p (iterate % (% n))))]
      (and (< 2 n) (p n) (= n (/ (+ (f dec) (f inc)) 2))))))

(defcheck solution-50e4aa4c
  (fn balanced? [candidate]
    (letfn [(prime? [known-primes n]
              (not-any?
                #(and (zero? (mod n %))
                      (not= n %))
                known-primes))
            (next-prime [primes]
              (loop [n (if-not (empty? primes) (inc (last primes)) 2)]
                (if
                 (prime? primes n)
                  n
                  (recur (inc n)))))]
      (loop [primes (vector)]
        (if
         (> (or (last primes) 0) candidate)
          (and
           (prime? primes candidate)
           (<= 3 (count primes))
           (= candidate
             (/ (+ (last primes)
                   (nth (reverse primes) 2))
               2)))
          (recur (conj primes (next-prime primes))))))))

(defcheck solution-51dbd11e
  (let [prime?
        (memoize (fn [num]
                   (if (even? num)
                     false
                     (loop [divr 3]
                       (cond (> divr (Math/sqrt num)) true
                             (= 0 (mod num divr)) false
                             :else (recur (+ divr 2)))))))]
    (fn [num]
      (if (not (prime? num))
        false
        (let [primes (filter prime? (range (if (>= (- num 50) 2) (- num 50) 2) (+ num 50)))
              ind    (.indexOf primes num)
              dind   (dec ind)
              iind   (inc ind)]
          (if (or (< ind 0) (>= iind (count primes)) (< dind 0))
            false
            (= num (/ (+ (nth primes iind) (nth primes dind)) 2))))))))

(defcheck solution-51e7650f
  (fn [n]
    (letfn [(primes* [n previous-primes]
              (let [next-prime (->> (iterate inc n)
                                 (drop-while #(some (fn [x] (zero? (mod % x))) previous-primes))
                                 first)]
                (cons next-prime
                  (lazy-seq (primes* (inc next-prime) (conj previous-primes next-prime))))))
            (primes [] (primes* 2 []))]
      (->> (partition 3 1 (primes))
        (drop-while (fn [[lh c rh]] (< c n)))
        first
        ((fn [[lh c rh]] (and (= c n)
                              (= (* 2 n)
                                (+ lh rh)))))
        boolean))))

(defcheck solution-540be699
  ((fn []
     (let [prime? (fn [x] (or (contains? #{2 3 5 7} x)
                              (and (odd? x) (>= x 11)
                                   (->> (range 3 (+ 1e-5 (Math/sqrt x)) 2)
                                     (map #(zero? (mod x %)))
                                     (every? false?)))))
           ind    (fn ind [s x p] (cond (= (first s) x) p
                                        (< (first s) x) (ind (rest s) x (inc p))
                                        :else nil))
           primes (filter prime? (iterate inc 2))]
       (fn [x]
         (if (prime? x)
           (let [pos (ind primes x 0)]
             (boolean (and (> x 2) pos (= x (/ (+ (nth primes (dec pos)) (nth primes (inc pos))) 2)))))
           false))))))

(defcheck solution-54424326
  (fn balanced? [n]
    (let [prime? (fn [n] (if (< n 2) false (every? #(not= 0 (mod n %)) (range 2 n))))]
      (if (not (prime? n)) false
                           (let [anil (first (filter prime? (range (dec n) 1 -1)))
                                 a    (if (nil? anil) 0 anil)
                                 c    (first (filter prime? (range (inc n) 2147483647)))]
                             (== n (/ (+ a c) 2)))))))

(defcheck solution-544c05f0
  (fn [num]
    (or (let [prime? (fn [x]
                       (and (not= x 0)
                            (not= x 1)
                            (not (some #(zero? (mod x %))
                                   (range 2 (inc (Math/sqrt x)))))))]
          (when (prime? num)
            (when-let [down (first (filter prime? (range (dec num) 0 -1)))]
              (when-let [up (first (filter prime? (range (inc num) ##Inf)))]
                #_(println num down up)
                (= (+ up down) (* 2 num))))))
        false)))

(defcheck solution-54fb3b55
  (fn [x]
    (letfn [(p? [n] (if (< n 2) false (every? #(> (mod n %) 0) (range 2 (inc (Math/sqrt n))))))
            (np? [n] (not (p? n)))]
      (let [np (take 1 (drop-while np? (drop (inc x) (range))))
            pp (take 1 (drop-while np? (range (dec x) 1 -1)))
            c  (concat pp [x] np)]
        (if (and (every? p? c) (= (* 3 x) (reduce + c)))
          true
          false)))))

(defcheck solution-55926d29
  (fn balanced-prime? [num]
    (let [factors (cons 2 (iterate (partial + 2) 3))]
      (letfn [(prime? [n]
                (not-any? #(zero? (rem n %)) (take-while #(<= % (inc (Math/sqrt n))) factors)))
              (prime-step [n s]
                (first (drop-while (complement prime?) (rest (iterate (partial + s) n)))))]
        (and (> num 3)
             (prime? num)
             (= num (/ (+ (prime-step num 2) (prime-step num -2)) 2)))))))

(defcheck solution-55e6677f
  (fn [p] (let [prime?     #(and (> % 1) (nil? (some #{0} (map (partial rem %) (range 2 (inc (quot % 2)))))))
                next-prime (first (filter prime? (drop (inc p) (range))))
                prev-prime (first (filter prime? (range (dec p) 1 -1)))]
            (and
             (prime? p)
             (number? prev-prime)
             (= p (/ (+ next-prime prev-prime) 2))))))

(defcheck solution-5625efbf
  (fn [p] (number? (some #{p} [5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103]))))

(defcheck solution-56e1043b
  (fn [n]
    (letfn [(prime? [x]
              (not-any? #(zero? (mod x %))
                (range 2 (inc (int (Math/sqrt x))))))
            (pre [x]
              (if (prime? x)
                x
                (pre (dec x))))
            (next [x]
              (if (prime? x)
                x
                (next (inc x))))]
      (and
       (> n 2)
       (prime? n)
       (= n (/ (+ (pre (dec n)) (next (inc n))) 2))))))

(defcheck solution-56fb4985
  (fn [x]
    (letfn [(isprime?
              ([x] (isprime? x (quot x 2)))
              ([x z]
               (cond
                 (< x 2) false
                 (< z 2) true
                 (zero? (rem x z)) false
                 :else (recur x (dec z)))))]
      (let [llist (filter isprime? (reverse (range 1 x)))
            rlist (filter isprime? (iterate inc (inc x)))]
        (not (not
               (and (isprime? x)
                    (not (empty? llist))
                    (not (empty? rlist))
                    (= (+ x x) (+ (first llist) (first rlist))))
               ))))))

(defcheck solution-571e5120
  (fn bprime? [x]
    (letfn [(prime? [n]
              (or (= n 2)
                  (and (odd? n)
                       (not (some #(= 0 (mod n %))
                              (range 3 (inc (Math/sqrt n))))))))]
      (if (or (< x 3) (not (prime? x)))
        false
        (let [prior (take 1 (filter prime? (range (dec x) 1 -1)))
              next  (take 1 (filter prime? (iterate inc (inc x))))
              pdiff (- x (first prior))
              ndiff (- (first next) x)]
          (if (= pdiff ndiff)
            true
            false
            )
          )
        )
      )
    ))

(defcheck solution-57db6bbb
  (fn [n]
    (let [is-prime       (fn [x]
                           (and (> x 1)
                                (not-any?
                                  #(zero? (mod x %))
                                  (range 2 (-> x (/ 2) int inc))))
                           )
          previous-prime (fn [x]
                           (first (filter is-prime (range (dec x) 1 -1))))
          next-prime     (fn [x]
                           (first (filter is-prime (drop (inc x) (range)))))]
      (do
        (and (> n 2)
             (is-prime n)
             (= (- n (previous-prime n))
               (- (next-prime n) n)))))
    ))

(defcheck solution-582d188b
  (fn balprime? [n]
    (letfn [(prime? [n]
              (if (< n 2) false
                          (empty? (filter #(zero? (rem n %)) (range 2 n)))))
            (prime-after [n] (first (filter #(prime? %) (drop (inc n) (range)))))
            (prime-before [n]
              (cond (< n 3) nil
                    (= n 3) 2
                    :else
                    (first (filter #(prime? %) (reverse (range 1 n))))))
            (mean [a b] (if (nil? a) 0 (/ (+ a b) 2)))]
      (and (prime? n) (= n (mean (prime-before n) (prime-after n))))
      )))

(defcheck solution-58831210
  (fn balanced-prime? [n]
    (let [is-prime? (fn [n] (empty? (filter #(zero? (mod n %)) (range 2 (dec n)))))
          inc-prime (first (filter is-prime? (iterate inc (inc n))))
          dec-prime (first (filter is-prime? (iterate dec (dec n))))]
      (and (= (/ (+ dec-prime inc-prime) 2) n) (is-prime? n) (>= n 3)))))

(defcheck solution-59407cc1
  (fn [n]
    (let [p? (fn [x] (not-any? #(= 0 (rem x %)) (range 2 x)))
          fp (fn [l] (first (filter p? l)))]
      (if (and (> n 2) (p? n))
        (= n (/ (+ (fp (range (dec n) 1 -1))
                   (fp (drop (inc n) (range))))
               2))
        false))))

(defcheck solution-59a09ceb
  (fn [n]
    (letfn [(p? [n] (and (> n 1) (not-any? #(= 0 (mod n %)) (range 2 n))))
            (np [s] (first (filter p? s)))]
      (and (p? n)
           (= (* 2 n)
             (+ (np (iterate inc (inc n)))
                (or (np (range (dec n) 1 -1)) 0)))))))

(defcheck solution-59f66493
  (fn [k]
    (let [p (fn [x] (seq (filter #(= 0 (mod x %)) (range 2 x))))
          f #(first (drop-while p (next (iterate % k))))]
      (and (< 2 k) (not (p k))
           (= k (/ (+ (f inc) (f dec)) 2))))))

(defcheck solution-59f95200
  (fn prime-sandwich [x]
    (let [mean   #(/ (apply + %) (count %))
          primes '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 201 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 381 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 123 123 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381 1399)]
      (let [i (.indexOf primes x)]
        (if (< i 1)
          false
          (= x (/ (+ (nth primes (dec i))
                     (nth primes (inc i))) 2)))))))

(defcheck solution-5a9709b4
  (fn [n]
    (letfn [(prime? [n]
              (if (or (false? (integer? n)) (< n 2)) false
                                                     (every? #(> (mod n %) 0) (range 2 n))))]
      (cond
        (= n 2) false
        (not (prime? n)) false
        :else
        (let [primes (filter prime? (iterate inc 2))
              s      (last (take-while #(>= n (second %)) (partition 3 1 primes)))]
          (prime? (/ (+ (first s) (last s)) 2)))))))

(defcheck solution-5bb7def
  (fn bal-prime [a]
    (letfn [(prime? [x]
              (and
               (> x 1)
               (not (some #(= 0 (mod x %)) (range 2 (inc (Math/sqrt x)))))))]
      (and
       (> a 2)
       (prime? a)
       (let [np (first (filter prime? (map (partial + a 1) (range))))
             b  (- (* 2 a) np)]
         (and
          (prime? b)
          (not (some prime? (range (inc b) (dec a))))))))))

(defcheck solution-5c035f1b
  (let [is-prime?   (fn [x] (not-any? #(= 0 (mod x %)) (range 2 (inc (Math/sqrt x)))))
        prime-range (filter is-prime? (drop 2 (range)))]
    (fn is-balanced-prime? [x]
      (if
       (<= x 2)
        false
        (let [[left right] (split-with #(< % x) prime-range)
              before (or (last left) 0)
              after  (second right)]
          (and (is-prime? x) (= x (/ (+ before after) 2))))))))

(defcheck solution-5c6ed333
  (fn [n]
    (letfn [(prime? [x]
              (cond
                (< x 2) false
                (= x 2) true
                :else (not-any? #(zero? (rem x %)) (range 2 (inc (Math/sqrt x))))))
            (seek [x f]
              (loop [x (f x)]
                (if (prime? x) x
                               (recur (f x)))))]
      (and (> n 2) (prime? n) (= n (/ (+ (seek n dec) (seek n inc)) 2))))))

(defcheck solution-5c90fce1
  (fn f [n]
    (let [prime? (fn [n']
                   (if (or (= n' 1) (= n' 2))
                     true
                     (not-any? #(zero? (mod n' %)) (range 2 n'))))
          primes (filter prime? (drop 1 (range)))]
      (if (or (= n 2) (not (prime? n)))
        false
        (let [lhs (last (take-while #(< % n) primes))
              rhs (first (drop-while #(<= % n) primes))]
          (if (nil? lhs)
            false
            (= (- n lhs) (- rhs n))))))))

(defcheck solution-5c9c5489
  (fn [n]
    (letfn [(is-prime [x]
              (every? #(< 0 (mod x %)) (range 2 (inc (int (Math/sqrt x))))))
            (prime-before [n]
              (first (drop-while #(not (is-prime %)) (reverse (range n)))))
            (prime-after [n]
              (first (drop-while #(not (is-prime %)) (drop (inc n) (range)))))]
      (if (and (< 2 n) (is-prime n))
        (== n (/ (+ (prime-before n) (prime-after n)) 2))
        false))))

(defcheck solution-5cbcb24c
  (fn balanced-prime?
    [n]
    (let [prime?     (fn [p] (cond
                               (= p 1) false
                               (= p 2) true
                               (= 0 (mod p 2)) false
                               :else (every? #(not= 0 (mod p %))
                                       (range 3 (inc (int (Math/sqrt p))) 2))))
          next-prime (some #(when (prime? %) %) (iterate inc (inc n)))
          prev-prime (some #(when (prime? %) %) (iterate dec (dec n)))]
      (and (prime? n) (= n (/ (+ next-prime prev-prime) 2))))))

(defcheck solution-5e3539a9
  (fn balanced-prime? [n]
    (boolean
      (let [prime? (fn [x] (and (> x 1) (not-any? #(zero? (mod x %)) (range 2 x))))]
        (if (prime? n)
          (if-let [prev (last (filter prime? (range n)))]
            (let [next (second (filter prime? (iterate inc n)))]
              (= n (/ (+ prev next) 2)))))))))

(defcheck solution-5e8ab478
  (fn [x]
    (let [prime?      (fn [n] (and (> n 2) (not-any? #(= 0 (mod n %)) (range 2 n))))
          next-higher (first (filter prime? (iterate inc (inc x))))
          next-lower  (first (filter prime? (range (dec x) 2 -1)))]
      (and (prime? x) (= x (/ (+ (or next-higher 0) (or next-lower 0)) 2))))))

(defcheck solution-5ef57b17
  (fn balans [val]
    (let [prime? (fn [testVal, primeseq]
                   (loop [col primeseq]
                     (let [primeVal (first col)]
                       (cond (= (mod testVal primeVal) 0) false
                             (> (* primeVal primeVal) testVal) true
                             :else (recur (rest col))
                             )
                       )
                     )
                   ),
          primes (fn [max]
                   (loop [val 3, primecol [2]]
                     (if (> val max)
                       primecol
                       (if (prime? val primecol)
                         (recur (+ 2 val) (conj primecol val))
                         (recur (+ 2 val) primecol)
                         ))))]
      (if (< val 3)
        false
        (let [primesCol (primes val),
              currVal   (peek primesCol),
              preVal    (peek (pop primesCol)),
              newVal    (- (* 2 currVal) preVal)]
          ;(println (str "curr " currVal " pre " preVal " newVal " newVal ))
          (and (= currVal val)
               (prime? newVal primesCol)
               (every? #(not (prime? % primesCol))
                 (range (+ 2 currVal) newVal 2))
               ))))))

(defcheck solution-5f56318d
  (fn [x]
    (let [p (fn [n] (cond (#{2} n) true
                          (< n 2) false
                          :t (every? #(< 0 (rem n %)) (range 2 (+ 1 (Math/sqrt n))))))
          g (fn g [n] (if (p (+ 1 n)) (+ n 1) (g (+ 1 n))))
          h (fn h [n] (if (<= n 2) -10 (if (p (- n 1)) (- n 1) (h (- n 1)))))]
      (and (p x)
           (= (- (g x) x) (- x (h x)))))))

(defcheck solution-60336e9a
  (fn balanced-prime
    [n]
    (letfn [(prime? [m] (or (= m 2)
                            (and (> m 2)
                                 (not-any? #(zero? (mod m %)) (range 2 (Math/sqrt (inc m)))))))]
      (and (prime? n)
           (let [neighbor-primes (map #(first (filter prime? %))
                                   [(range (dec n) 1 -1) (drop (inc n) (range))])]
             (and (not-any? nil? neighbor-primes)
                  (= n (/ (apply + neighbor-primes) 2))))))))

(defcheck solution-620c3975
  (fn balanced? [x]
    (let [prime? (fn [x] (not-any? #(= 0 (mod x %)) (range 2 x)))]
      (if (or (< x 3)
              (not (prime? x)))
        false
        (let
         [nextprime (first (filter prime? (iterate inc (inc x))))
          lastprime (first (filter prime? (iterate dec (dec x))))]
          (= x (/ (+ nextprime lastprime) 2)))))))

(defcheck solution-62f2db60
  (fn __ [n]
    (let
     [prime?     (fn [n]
                   (cond
                     (< n 2) false
                     :else (every? #(not= 0 (rem n %)) (range 2 (inc (/ n 2))))))
      next-prime (fn [n]
                   (let [n (inc n)] (if (prime? n) n (recur n))))
      prev-prime (fn [n]
                   (let [n (dec n)] (if (< n 1) nil (if (prime? n) n (recur n)))))]
      (and (> n 2) (prime? n) (= n (/ (+ (prev-prime n) (next-prime n)) 2))))))

(defcheck solution-62faaad1
  (fn [n]
    (if (< n 5)
      false
      (let [p? (fn [n] (reduce #(if (= 0 (mod n %2))
                                  false
                                  %1)
                         true (range 2 n)))
            np (fn [n f] (let [m (f n)]
                           (if (p? m)
                             m (recur m f))))]
        (if (p? n)
          (let [l (np n dec)
                r (np n inc)]
            (= n (/ (+ l r) 2)))
          false)))))

(defcheck solution-633fc0a1
  (fn bp? [x]
    (letfn [(p? [n]
              (if (or (= 2 n) (= 3 n))
                true
                (= 0 (count
                       (filter #(zero? (rem n %))
                         (take-while #(<= (* % %) n) (cons 2 (range 3 n 2))))))))]
      (if (p? x)
        (let [w (first (drop-while #(< (second %) x) (partition 3 1 (filter p? (iterate inc 2)))))]
          (= (/ (+ (first w) (last w)) 2) x))
        false))))

(defcheck solution-6383cbc3
  (fn [s]
    (letfn [(isPrime [x] (and (> x 1) (not-any? #(= 0 (mod x %)) (range 2 x))))
            (firstTrue [pred step n]
              (loop [curr n] (if (pred curr) curr (recur (+ step curr)))))]
      (and (isPrime s) (and (> s 2) (= (- s (firstTrue isPrime -1 (dec s))) (- (firstTrue isPrime 1 (inc s)) s)))))))

(defcheck solution-6479b76c
  (fn [n]
    (letfn [(f [x] (not-any? #(= 0 (mod x %)) (range 2 x)))
            (g [y h] (if (f y) y (g (h y) h)))]
      (if (and (f n) (> n 2)) (= n (/ (+ (g (dec n) dec) (g (inc n) inc)) 2)) false))))

(defcheck solution-6479e62e
  (fn [x] (let [prime?         (fn [n] (not-any? (fn [i] (zero? (mod n i))) (range 2 n)))
                next-prime     (first (filter prime? (iterate inc (inc x))))
                previous-prime (first (filter prime? (iterate dec (dec x))))]
            (and (> x 2) (prime? x) (= (* x 2) (+ next-prime previous-prime))))))

(defcheck solution-6557e584
  #(first (for [o (range 1 (- % 2))
                [a b c] [(for [x [(- % o) (+ % o) %]]
                           (every? (fn [b] (> (rem x b) 0)) (range 2 x)))]
                :when (or a b)]
            (and a b c))))

(defcheck solution-658c625c
  (let [p (filter (fn [n] (not-any? #(zero? (mod n %)) (range 2 (inc (Math/sqrt n))))) (drop 2 (range)))]
    (fn [n]
      (let [i (count (take-while #(< % n) p))]
        (and (> i 0) (= n (nth p i)) (= n (/ (+ (nth p (dec i)) (nth p (inc i))) 2)))))))

(defcheck solution-65d0e6d3
  (fn [n]
    (let [prime? (fn [n] (and (not= 1 n) (every? #(not= 0 (mod n %)) (range 2 n))))]
      (and
       (prime? n)
       (= :balanced
         (some
           (fn [delta]
             (let [plow (prime? (- n delta))
                   phi  (prime? (+ n delta))]
               (cond (and plow phi) :balanced
                     (or plow phi) :unbalanced)))
           (range 2 (- n 2) 2)))))))

(defcheck solution-685cbb63
  (fn [x]
    (let [prime?    (fn [x] (if (some #(= 0 (mod x %)) (range 2 x)) false true))
          nextprime (first (drop-while (complement prime?) (range (inc x) (* 3 (inc x)))))
          prevprime (first (drop-while (complement prime?) (reverse (range x))))
          ]
      (and (> x 2) (prime? x) (= (- x prevprime) (- nextprime x))))))

(defcheck solution-6897275
  (fn [n]
    (letfn [(prime? [k]
              (let [check-to (inc (int (Math/sqrt k)))]
                (nil? (some #(zero? (rem k %)) (range 2 check-to)))))
            (prime-below [k]
              (loop [m (dec k)]
                (if (prime? m) m (recur (dec m)))))
            (prime-above [k]
              (loop [m (inc k)]
                (if (prime? m) m (recur (inc m)))))]
      (and (> n 2) (prime? n) (= n (/ (+ (prime-below n) (prime-above n)) 2))))))

(defcheck solution-69d46462
  (fn [n]
    (letfn
     [(p? [n] (every? #(> (mod n %) 0) (range 2 n)))
      (p [f n] (first (filter p? (iterate f (f n)))))
      ]
      (and
       (> n 3)
       (p? n)
       (= n (/ (+ (p dec n) (p inc n)) 2))))))

(defcheck solution-6aab344b
  (fn b-prime? [n]
    (let [prime? (fn [n]
                   (cond (> n 2) (every? pos? (map #(mod n %)
                                                (range 2 (inc (Math/sqrt n)))))
                         (= n 2) true
                         :else false))
          np     (first (filter prime? (iterate inc (inc n))))
          pp     (first (filter prime? (take-while pos? (iterate dec (dec n)))))]
      (and pp (prime? n) (= n (/ (+ np pp) 2))))))

(defcheck solution-6b12b955
  (fn [n]
    (letfn [(prime? [n]
              (cond
                (< n 2) false
                (= n 2) true
                :else (not-any?
                        #(= 0 (mod n %))
                        (range 2 n))))
            (next-or-previous-prime [n start end inc]
              (let [op (some #(if (prime? %) % false) (range start end inc))]
                (if (boolean op) op 0)))
            (next-prime [n]
              (next-or-previous-prime n (inc n) 2147483647 1))
            (prev-prime [n]
              (next-or-previous-prime n (dec n) 0 -1))]
      (if (not (prime? n))
        false
        (= n (/ (+ (prev-prime n) (next-prime n)) 2))))))

(defcheck solution-6bafcd4
  (fn ps [n]
    (letfn [(p? [x]
              (or (< 1 x 4)
                  (empty? (filter #(= (mod x %) 0) (range 2 x)))))]
      (cond (or (not (p? n)) (< n 3))

            false,
            :else
            (let [a (first (filter p? (range (dec n) 0 -1)))
                  b (first (filter p? (range (inc n) (+ 100 n))))]
              (= (+ a b) (* 2 n)))))))

(defcheck solution-6c526be6
  (fn primwich [n]
    (let [prime? (fn [n] (and (> n 1)
                              (empty? (filter #(= (mod n %) 0) (range 2 n)))))]
      (if (prime? n)
        (loop [dist 1]
          (let [prev-prime (prime? (- n dist))
                next-prime (prime? (+ n dist))]
            (if (or prev-prime next-prime)
              (and prev-prime next-prime)
              (recur (inc dist)))))
        false))))

(defcheck solution-6c5b9074
  (fn is-prime-sandwich [n]
    (letfn [
            (is-prime? [n] (
                             loop [x 2 up (int (Math/sqrt n))]
                             (if (> x up) true
                                          (if (zero? (rem n x))
                                            false
                                            (recur (inc x) up)
                                            )
                                          )
                             ))
            (get-close-prime [n f] (let [test (f n)]
                                     (if (is-prime? test) test
                                                          (get-close-prime test f)
                                                          )
                                     ))
            ]
      (and (> n 2) (is-prime? n) (= n (/ (+ (get-close-prime n inc) (get-close-prime n dec)) 2)))
      )
    ))

(defcheck solution-6cd0e022
  (fn [n]
    (let [prime    (fn [n] (not (some #(zero? (mod n %)) (range 2 n))))
          balanced (fn [n] (and (> n 2) (prime n) (= (* 2 n) (+ (last (filter prime (range n))) (first (filter prime (iterate inc (inc n))))))))]
      (balanced n))))

(defcheck solution-6d05de97
  (fn [x]
    (letfn [(prime? [n] (and (> n 1)
                             (every? #(> (rem n %) 0)
                               (range 2 (inc (quot n 2))))))
            (nprime [f n] (first (filter prime? (iterate f (f n)))))]


      (and (> x 2)
           (prime? x)
           (= x (/ (+ (nprime dec x) (nprime inc x)) 2))))))

(defcheck solution-6d8912c3
  (let [prime-worker (fn prime-worker [i, iterators]
                       (let [iterator (get iterators i)]
                         (if iterator
                           (recur (inc i)
                             (reduce
                               #(update-in %1 [(+ i %2)] conj %2)
                               (dissoc iterators i)
                               iterator))
                           (lazy-seq (cons i (prime-worker (inc i) (assoc iterators (* i i) [i])))))))
        primes       (prime-worker 2 {})
        prime?       (fn [n] (= n (first (drop-while #(< % n) primes))))]
    (fn [n]
      (and (prime? n)
           (> n 2)
           (let [[a b c] (first (filter #(= n (second %)) (partition 3 1 primes)))]
             (= b (/ (+ a c) 2)))))))

(defcheck solution-6dce0179
  (fn balanced-prime? [n]
    (let [factors    (cons 2 (iterate (partial + 2) 3))
          prime?     (fn [n] (not-any? #(zero? (mod n %))
                               (take-while #(<= % (inc (Math/sqrt n))) factors)))
          prime-step (fn [n s] (first (drop-while (complement prime?) (rest (iterate (partial + s) n)))))]
      (and (> n 3)
           (prime? n)
           (= n (/ (+ (prime-step n 2) (prime-step n -2)) 2))))))

(defcheck solution-6ed8af8
  (let [primes
        (letfn [(remove-mults [n ns] (remove #(zero? (rem % n)) ns))
                (sieve [[n & ns]] (lazy-seq
                                    (cons n (sieve (remove-mults n ns)))))]
          (cons 2 (sieve (iterate #(+ % 2) 3))))]
    (fn [n]
      (loop [ps primes]
        (let [[p1 p2 p3] ps]
          (cond
            (< p2 n) (recur (next ps))
            (> p2 n) false
            :else (= (- p2 p1) (- p3 p2))
            ))))))

(defcheck solution-6f3dff2b
  (fn [n]
    (letfn [(prime? [n]
              (and (> n 1) (every? #(> (mod n %) 0) (range 2 n))))
            (next-prime [n]
              (first (filter prime? (iterate inc (inc n)))))
            (prev-prime [n]
              (first (filter prime? (reverse (range 1 n)))))]
      (let [a (prev-prime n) b (next-prime n)]
        (and (prime? n)
             a
             (= n (/ (+ a b) 2)))))))

(defcheck solution-6f7a67db
  (letfn [(prime?
            [n]
            (not-any? (comp zero?
                            (partial
                              mod n))
              (range 2 n)))]

    (fn balanced? [n]
      (and (> n 4)
           (prime? n)
           (->> [(iterate #(- % 2) n)
                 (iterate #(+ % 2) n)]
             (map (comp second
                        (partial
                          filter
                          prime?)))
             (apply +)
             (#(/ % 2))
             (= n))))))

(defcheck solution-6fb920e3
  (fn bal-prime [n]
    (let [primes [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129]] ;;cheating!!
      (if (not (contains? (set primes) n)) false
                                           (if (= 2 n) false
                                                       (let [l (last (take-while #(< % n) primes))
                                                             r (first (drop-while #(<= % n) primes))]
                                                         (= n (/ (+ l r) 2))))))))

(defcheck solution-700b1435
  (fn balanced-prime? [n]
    (let [factors    (cons 2 (iterate (partial + 2) 3))
          prime?     (fn [n] (not-any? #(zero? (mod n %))
                               (take-while #(<= % (inc (Math/sqrt n))) factors)))
          prime-step (fn [n s] (first (drop-while (complement prime?) (rest (iterate (partial + s) n)))))]
      (and (> n 3)
           (prime? n)
           (= n (/ (+ (prime-step n 2) (prime-step n -2)) 2))))))

(defcheck solution-70ab009a
  #(boolean (#{5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103} %)))

(defcheck solution-70f59467
  (fn [n]
    (let [prime?    (fn [x] (not-any? #(zero? (rem x %))
                              (range 2 (inc (quot x 2)))))
          nextprime #(first (filter prime? (iterate inc (+ % 1))))
          prevprime #(last (filter prime? (range 2 %)))]
      (and (prime? n)
           (prevprime n)
           (= (/ (+ (nextprime n)
                    (prevprime n))
                2)
             n)))))

(defcheck solution-715f2031
  (fn sandwich? [n]
    (if (< n 5)
      false
      (let [
            prime?      #(if (and (> % 1) (not-any? (fn [x] (= (mod % x) 0)) (range 2 (- % 1)))) % false)
            lower_prime (some prime? (iterate dec (dec n)))
            upper_prime (some prime? (iterate inc (inc n)))]
        (and (prime? n) (= (+ n n) (+ lower_prime upper_prime)))))))

(defcheck solution-71a00e0a
  (fn [x]
    (let [p? (fn [n] (not-any? zero? (map #(mod n %) (range 2 n))))
          a  (first (filter p? (iterate dec (dec x))))
          b  (first (filter p? (iterate inc (inc x))))]
      (and
       (> x 3)
       (p? x)
       (= x (/ (+ a b) 2))))))

(defcheck solution-71fd48d9
  (fn [n]
    (let [primes          (letfn [(prime-seq [known-primes candidate]
                                    (if (some #(zero? (mod candidate %)) known-primes)
                                      (recur known-primes (+ 2 candidate))
                                      (cons candidate
                                        (lazy-seq (prime-seq (conj known-primes candidate)
                                                    (+ 2 candidate))))))]
                            (cons 2 (prime-seq [2] 3)))
          balanced-primes (map second
                            (filter (fn [[a b c]]
                                      (= b (/ (+ a c) 2)))
                              (partition 3 1 primes)))]
      (boolean (some #{n} (take-while (partial >= n)
                            balanced-primes))))))

(defcheck solution-7216cb1e
  (fn [n]
    (letfn [(is-prime?
              [n]
              (let [test-nums (cons 2 (range 3 n 2))]
                (or (= 2 n)
                    (not (boolean (some #(= 0 (mod n %)) test-nums))))))]
      (and
       (> n 4)
       (is-prime? n)
       (= (first (filter #(is-prime? (- n %)) (range 2 (- n 2) 2)))
         (first (filter #(is-prime? (+ n %)) (range 2 (- n 2) 2))))))))

(defcheck solution-73e9a346
  (let [sieve  (fn sieve [[p & xs]]
                 (cons p (lazy-seq
                           (sieve (for [x xs :when (not= 0 (mod x p))]
                                    x)))))
        primes (cons 2 (sieve (iterate #(+ % 2) 3)))]
    (fn [n]
      (loop [primes   primes
             two-back nil
             one-back nil]
        (let [[p & more] primes]
          (if (> p n)
            (and (= one-back n) two-back (== (/ (+ two-back p) 2) n))
            (recur more one-back p)))))))

(defcheck solution-742827f9
  (fn [n]
    (let [prime?     (fn [n] (and (> n 1)
                                  (not-any? #(= 0 (mod n %)) (range 2 n))
                                  n))
          next-prime (some prime? (drop (inc n) (range)))
          prev-prime (some prime? (range (dec n) 1 -1))]
      (and (prime? n)
           next-prime
           prev-prime
           (= n (/ (+ next-prime prev-prime) 2))))))

(defcheck solution-747b0e93
  (fn balancedprime [n]
    (letfn [
            (prime? [n]
              (if (< n 2) false
                          (let [denoms    (range 2 (inc (int (Math/sqrt n))))
                                zero-rems (filter #(zero? (rem n %)) denoms)]
                            (zero? (count zero-rems)))))
            (nextprime [f val]
              (if (prime? val)
                val
                (nextprime f (f val))))]
      (if (and (> n 2) (prime? n))
        (let [nxt (nextprime inc (inc n))
              prv (nextprime dec (dec n))
              avg (/ (+ nxt prv) 2)]
          (= n avg))
        false))))

(defcheck solution-75396b4e
  (fn balanced-prime? [n]
    (let [prime?     (fn [n]
                       (and (> n 1) (empty? (filter #(= 0 (mod n %)) (range 2 n)))))
          next-prime (fn [n]
                       (let [inc-n (inc n)]
                         (if (prime? inc-n)
                           inc-n
                           (recur inc-n))))
          prev-prime (fn [n]
                       (if (pos? n)
                         (let [dec-n (dec n)]
                           (if (prime? dec-n)
                             dec-n
                             (recur dec-n)))
                         n))]
      (if (prime? n)
        (let [np (next-prime n)
              pp (prev-prime n)]
          (if (= n (/ (+ np pp) 2))
            true
            false))
        false))))

(defcheck solution-756047f2
  (fn [n]
    (let [prime? (fn [n]
                   (and
                    (<= 2 n)
                    (loop [n n i 2]
                      (cond (< n (* i i)) true
                            (zero? (mod n i)) false
                            :else (recur n (inc i))))))]
      (and
       (prime? n)
       (let [a (->> (range (dec n) 1 -1)
                 (filter prime?)
                 first)
             b (->> (range)
                 (map #(+ n 1 %))
                 (filter prime?)
                 first)]
         (and a b (= (* 2 n) (+ a b))))))))

(defcheck solution-75a35b58
  (fn [x]
    (letfn [(prime?
              [a] (cond (< a 2) false
                        (= a 2) true
                        :else (let [root (Math/ceil (Math/sqrt a))]
                                (not-any? #(= (rem a %) 0) (range 2 (inc root))))))
            (prime_before [b] (if (prime? (dec b)) (dec b) (prime_before (dec b))))
            (prime_after [c] (if (prime? (inc c)) (inc c) (prime_after (inc c))))]
      (if (< x 5) false
                  (if (not (prime? x)) false
                                       (if (= x (/ (+ (prime_before x) (prime_after x)) 2)) true false))))))

(defcheck solution-75df050
  (fn prime-sandwich [n]
    (let [prime?     (fn [n] (if (= 2 n)
                               true
                               (not-any? #(zero? (rem n %)) (concat '(2) (range 3 (+ 1 (int (Math/sqrt n))))))))
          next-prime (fn [n] (loop [n (int (if (even? n) (+ n 3) (+ n 2)))]
                               (if-not (prime? n)
                                 (recur (+ n 2))
                                 n)))]
      (if (>= n 5)
        (let [prev-prime (- n (- (next-prime n) n))]
          (and (prime? n)
               (prime? prev-prime)
               (= n (next-prime prev-prime))))
        false))))

(defcheck solution-75fd725f
  (fn [n]
    (letfn [(primes
              ([] (cons 2 (primes [] 3)))
              ([prime-vec n]
               (if (let [len (count prime-vec)]
                     (loop [i 0]
                       (if (== i len)
                         true
                         (let [x (prime-vec i)]
                           (cond (> (* x x) n) true
                                 (zero? (mod n x)) false
                                 :else (recur (inc i)))))))
                 (lazy-seq
                   (cons n (primes (conj prime-vec n) (+ n 2))))
                 (recur prime-vec (+ n 2)))))]
      (let [[p1 p2 p3] (first (filter #(>= (second %) n)
                                (partition 3 1 (primes))))]
        (and (= n p2)
             (= n (/ (+ p1 p3) 2)))))))

(defcheck solution-764607b2
  (fn prime-sandwich
    [n]
    (letfn [(prime? [n]
              (every? #(not= 0 (rem n %))
                (range 2 (inc (quot n 2)))))
            (prime-nos ([start stop]
                        (for [x (range start stop -1)
                              :when (prime? x)] x))
              ([start] (for [x (iterate inc start)
                             :when (prime? x)] x)))]
      (if (or (#{0 1 2} n) (not (prime? n)))
        false
        (= n (/ (+ (first (prime-nos (dec n) 1)) (first (prime-nos (inc n)))) 2))))))

(defcheck solution-76936434
  (fn [n]
    (if (< n 3) false
                (letfn [(prime? [a] (every? #(not (zero? %)) (map #(mod a %) (range 2 (inc (/ a 2))))))]
                  (if (prime? n)
                    (let [prev (last (take-while #(< % n) (filter prime? (range n))))
                          next (first (filter prime? (range (inc n) (* n n))))
                          ]
                      (and (not (nil? next)) (= (- n prev) (- next n)))
                      )
                    false
                    )
                  )
                )
    ))

(defcheck solution-77523af5
  (fn [n]
    (let [pn?     (fn [v]
                    (if (< v 2) false
                                (let [rt (int (Math/sqrt v)) tmp (drop 2 (range (inc rt)))]
                                  (= (count tmp) (count (filter #(not= 0 %) (map #(mod v %) tmp))))
                                  )))
          next-pn (fn [v direction]
                    (loop [cnt (direction v)]
                      (if (pn? cnt)
                        cnt
                        (recur (direction cnt))
                        )))

          ]
      (if (and (pn? n) (not= 2 n))
        (= (+ n n) (+ (next-pn n inc) (next-pn n dec)))

        false
        )
      )
    ))

(defcheck solution-7783da82
  (let [prime? (fn [n]
                 (if (< n 2)
                   false
                   (nil? (first (filter #(= 0 (rem n %)) (range 2 n))))))
        prime? (memoize prime?)]
    (fn [n]
      (if (prime? n)
        (if-let [r (first (filter #(and (prime? %) (> % n)) (range n (* n 2))))]
          (if-let [l (last (filter #(and (prime? %) (< % n)) (range (quot n 2) n)))]
            (= (/ (+ l r) 2) n)
            false)
          false)
        false))))

(defcheck solution-77a3d6a0
  (fn
    [x]
    (letfn
     [(prime? [v] (every? #(not= 0 (rem v %)) (range 2 v)))]
      (and
       (> x 2)
       (prime? x)
       (let
        [prv (first (filter prime? (range (dec x) 1 -1)))
         nxt (first (filter prime? (drop (inc x) (range))))]
         (= (+ x x) (+ prv nxt)))))))

(defcheck solution-77b7b37b
  (letfn
   [(prime? [n] (and (> n 1) (every? pos? (map (partial mod n) (range 2 n)))))]
    (fn stuff [n]
      (when (> n 2)
        (let
         [lower  (first (filter prime? (iterate dec (dec n))))
          higher (first (filter prime? (iterate inc (inc n))))]
          (and (prime? n) (= (* 2 n) (+ lower higher))))))))

(defcheck solution-781022a9
  (fn [n]
    (let [prime? (fn [x] (< (count (filter #(= 0 (rem x %)) (range 1 x))) 2))
          nextp  (fn [xs] (first (filter prime? xs)))]
      (if (and (> n 4) (prime? n))
        (let [lst (nextp (iterate dec (dec n)))
              nxt (nextp (iterate inc (inc n)))]
          (= n (/ (+ lst nxt) 2)))
        false))))

(defcheck solution-781ee955
  (letfn [(prime? [n] (every? #(not (zero? %)) (map #(mod n %) (range 2 n))))
          (nextprime [n p] (if (prime? (+ n p)) (+ n p) (recur (+ n p) p)))]
    (fn [m]
      (and (not= 0 m) (not= 1 m) (not= 2 m)
           (prime? m)
           (= m (/ (+ (nextprime m -1) (nextprime m 1)) 2))))))

(defcheck solution-782a4fb7
  (fn [x]
    (let [prime?      (fn [n] (every? #(not= 0 (mod n %))
                                (range 2 n)))
          first-prime (fn [coll] (first (filter prime? coll)))]
      (if (prime? x)
        (let [lt-prime (first-prime (take-while (partial < 2) (iterate dec (dec x))))
              gt-prime (first-prime (iterate inc (inc x)))]
          (cond
            (nil? lt-prime) false
            (nil? gt-prime) false
            (= x (-> (+ lt-prime gt-prime) (/ 2))) true
            :else false))
        false))))

(defcheck solution-78458b84
  (letfn [(divisible-by [previous i]
            (let [sqrt-i (Math/sqrt i)]
              (some #(zero? (rem i %))
                (take-while #(<= % sqrt-i) previous))))
          (rec [previous nxt-seq]
            (let [skip-non-primes (drop-while #(divisible-by previous %) nxt-seq)
                  nxt-prime       (first skip-non-primes)]
              (lazy-seq
                (cons nxt-prime
                  (rec (conj previous nxt-prime)
                    (rest skip-non-primes))))))]
    (let [primes          (rec [] (drop 2 (range)))
          balanced-primes (map second
                            (filter (fn [[a b c]] (= b (/ (+ a c) 2)))
                              (partition 3 1 primes)))]
      (fn [tst]
        (= tst (last (take-while #(<= % tst) balanced-primes)))))))

(defcheck solution-78541f8a
  (fn balancedPrime? [p]
    (letfn [(isPrime? [x] (not-any? (fn isDivisor? [d] (zero? (mod x d))) (range 2 x)))
            (nearbyPrime [x f] (some #(when (isPrime? %) %) (iterate f (f x)))) ;Returns first prime in sequence f(x), f(f(x)), ...
            ]
      (and
       (> p 2)
       (isPrime? p)
       (= p (/ (+ (nearbyPrime p inc) (nearbyPrime p dec)) 2))
       )

      )
    ))

(defcheck solution-78dac878
  (fn number116 [n]
    (letfn [(prime? [x]
              (if (= 1 x)
                false
                (->> (range 2 (int (inc (Math/sqrt x))))
                  (some #(= 0 (mod x %)))
                  (not))))
            (mean [a b] (/ (+ a b) 2))]
      (if (or (not (prime? n)) (< n 3))
        false
        (let [next-prime (->> (range (inc n) 2147483647) (filter prime?) (first))
              last-prime (->> (range (dec n) 1 -1) (filter prime?) (first))]
          (= (mean last-prime next-prime) n))))))

(defcheck solution-794f0dfb
  (fn ps [i]
    (if (<= i 2)
      false
      (let [primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137
                     139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271
                     277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431
                     433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593
                     599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751
                     757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929
                     937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069
                     1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223
                     1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373
                     1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499 1511
                     1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637 1657
                     1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811
                     1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987
                     1993 1997 1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113 2129
                     2131 2137 2141 2143 2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287
                     2293 2297 2309 2311 2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423
                     2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593 2609 2617
                     2621 2633 2647 2657 2659 2663 2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741
                     2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851 2857 2861 2879 2887 2897 2903
                     2909 2917 2927 2939 2953 2957 2963 2969 2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079
                     3083 3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217 3221 3229 3251 3253 3257
                     3259 3271 3299 3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413
                     3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559 3571
                     3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719 3727
                     3733 3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907
                     3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 4051 4057
                     4073 4079 4091 4093 4099 4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231
                     4241 4243 4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363 4373 4391 4397 4409
                     4421 4423 4441 4447 4451 4457 4463 4481 4483 4493 4507 4513 4517 4519 4523 4547 4549 4561 4567 4583
                     4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721 4723 4729 4733 4751
                     4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871 4877 4889 4903 4909 4919 4931 4933 4937
                     4943 4951 4957 4967 4969 4973 4987 4993 4999 5003 5009 5011 5021 5023 5039 5051 5059 5077 5081 5087
                     5099 5101 5107 5113 5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237 5261 5273 5279
                     5281 5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441 5443
                     5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 5623 5639
                     5641 5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717 5737 5741 5743 5749 5779 5783 5791
                     5801 5807 5813 5821 5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923 5927 5939
                     5953 5981 5987 6007 6011 6029 6037 6043 6047 6053 6067 6073 6079 6089 6091 6101 6113 6121 6131 6133
                     6143 6151 6163 6173 6197 6199 6203 6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301
                     6311 6317 6323 6329 6337 6343 6353 6359 6361 6367 6373 6379 6389 6397 6421 6427 6449 6451 6469 6473
                     6481 6491 6521 6529 6547 6551 6553 6563 6569 6571 6577 6581 6599 6607 6619 6637 6653 6659 6661 6673
                     6679 6689 6691 6701 6703 6709 6719 6733 6737 6761 6763 6779 6781 6791 6793 6803 6823 6827 6829 6833
                     6841 6857 6863 6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967 6971 6977 6983 6991 6997
                     7001 7013 7019 7027 7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177 7187 7193 7207
                     7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 7351 7369 7393 7411
                     7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561
                     7573 7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681 7687 7691 7699 7703 7717 7723
                     7727 7741 7753 7757 7759 7789 7793 7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919)
            c      (count (take-while #(<= % i) primes))
            [a b c] (drop (- c 2) (take (inc c) primes))]
        (if (= b i (/ (+ a c) 2))
          true
          false)))))

(defcheck solution-79b4097d
  (fn [n]
    (and (<= 3 n)
         (let [prime?
                  (fn [ps n]
                    (every? #(pos? (mod n %)) ps))

               update-ps
                  (fn [ps n]
                    (if (prime? ps n)
                      (conj ps n) ps))

               ps (reduce update-ps [2]
                    (range 3 n 2))

               pp (last ps)

               pn (- (+ n n) pp)]
           (and (prime? ps n)
                (let [ps* (reduce update-ps ps
                            (range n pn))]

                  (and (= (last ps*) n)
                       (prime? ps* pn))))))))

(defcheck solution-7b336e9c
  (fn [x]
    (let [prime? (fn [x] (and (> x 1) (every? #(> (rem x %) 0) (range 2 x))))
          primes (filter prime? (iterate inc 2))
          deltas (map vector primes (drop 1 primes) (drop 2 primes))]
      (if (and (prime? x) (> x 2))
        (let [[a b c] (first (filter #(= x (second %)) deltas))]
          (= (- b a) (- c b)))
        false))))

(defcheck solution-7bbb2e1a
  (fn [n]
    (let [isprime? #(if (< % 2)
                      false
                      (->> % Math/sqrt int inc (range 2)
                        (map (partial mod %))
                        (every? (partial not= 0))))
          check    (fn [l r]
                     (let [lp (isprime? l)
                           rp (isprime? r)]
                       (cond
                         (and lp rp) true
                         (or lp rp) false
                         :else (recur (dec l) (inc r)))))]
      (and
       (isprime? n)
       (check (dec n) (inc n))))))

(defcheck solution-7c7f80a6
  (fn balanced [n]
    (let [prime (fn [n] (not-any? #(zero? (mod n %)) (range 2 n)))]
      (if (and (>= n 5) (prime n))
        (let [next (first (filter prime (iterate inc (inc n))))
              prev (first (filter prime (iterate dec (dec n))))]
          (= (/ (+ next prev) 2) n))
        false))))

(defcheck solution-7cbcf2bc
  (fn [n]
    (let [up? (fn [n] (or (= n 2) (and (> n 1) (not (some integer? (map #(/ n %) (range 2 n)))))))
          p?  (memoize up?)
          upb (fn [n] (cond (<= n 2) 0 :else (first (take 1 (filter p? (iterate dec (dec n)))))))
          upa (fn [n] (first (take 1 (filter p? (iterate inc (inc n))))))
          pb  (memoize upb)
          pa  (memoize upa)]
      (and (p? n)
           (== n (/ (+ (pb n) (pa n)) 2))))))

(defcheck solution-7d8fc522
  (fn [number]
    (let [primes-in-range (fn [r] (filter (fn [n] (not-any? #(= 0 (mod n %)) (range 2 n))) r))
          a               (first (primes-in-range (iterate dec (dec number))))
          [b c] (take 2 (primes-in-range (iterate inc number)))]
      (and (< 1 a) (= b number) (= b (/ (+ a c) 2))))))

(defcheck solution-7d95d0af
  (fn [n]
    (let [prime (fn [n]
                  (when (not-any? #(zero? (rem n %))
                          (range 2 (dec n)))
                    n))
          i     (some prime (reverse (range 2 n)))
          j     (some prime (drop (inc n) (range)))]
      (and (> n 2)
           (= n (prime n))
           (= n (/ (+ i j) 2))))))

(defcheck solution-7dfe2b89
  (fn [n]
    (let [p? (fn [n] (if (= 1 n) false (not (some #{0} (map #(rem n %) (range 2 n))))))
          f  (fn [n & f]
               (cond (= n 1) 2
                     (= n 2) 3
                     :else (loop [n (if f (inc n) (dec n))]
                             (if (p? n)
                               n
                               (recur (if f (inc n) (dec n)))))))]
      (if (p? n)
        (= n (/ (+ (f n n) (f n)) 2))
        false))))

(defcheck solution-7eab2138
  (fn [x] (letfn [
                  (ip [n]
                    (not-any? #(= 0 (mod n %))
                      (range 2 n)))]
            (and (> x 2) (ip x)
                 (= x
                   (/
                     (apply +
                       (map
                         #(first (filter
                                   ip
                                   (iterate % (% x)))) [dec inc]))
                     2))))))

(defcheck solution-7f1bd2ca
  (letfn [(not-mult? [i f] (not= (mod i f) 0))
          (prime? [n] (or (= n 2) (= n 3) (= n 5) (= n 7) (= n 11)
                          (and (> n 2) (odd? n) (not-mult? n 3) (not-mult? n 5) (not-mult? n 7) (not-mult? n 11)
                               (reduce #(and %1 %2) (for [x (range 3 (inc (int (Math/sqrt n))) 2)] (not-mult? n x))))))]
    (fn [i]
      (and (prime? i)
           (let [next (loop [j (+ i 2)] (if (prime? j) j (recur (inc j))))
                 prev (loop [j (- i 2)] (if (< j 2) nil (if (prime? j) j (recur (dec j)))))]
             (and (not (nil? prev)) (= (- next i) (- i prev))))))))

(defcheck solution-7febb321
  (fn [x]
    (let [prime?     (fn [n]
                       (not (some #(and
                                    (not= n %)
                                    (zero? (mod n %)))
                              (range 2 (inc (Math/sqrt n))))))
          prime-step (fn [n s]
                       (first (drop-while (complement prime?) (rest (iterate (partial + s) n)))))]
      (and
       (> x 3)
       (prime? x)
       (= x (/ (+ (prime-step x 2) (prime-step x -2)) 2))))))

(defcheck solution-8201efc1
  (fn [n]
    (let [is-prime? (fn [x] (and (> x 1) (nil? (some #(zero? (mod x %)) (take-while #(<= (* % %) x) (iterate inc 2))))))]
      (and
       (> n 4)
       (is-prime? n)
       (let [prev-prime (first (filter is-prime? (iterate dec (dec n))))
             next-prime (first (filter is-prime? (iterate inc (inc n))))]
         (= n (/ (+ prev-prime next-prime) 2)))))))

(defcheck solution-82473d9b
  (fn [c]
    (if (or (zero? c) (= 1 c) (= 2 c)) false
                                       (let [prime?         (fn [x] (not (some #(= 0 %) (map #(mod x %) (range 2 (inc (/ x 2)))))))
                                             getlower-prime (fn [x] (first (drop-while #(not (prime? %)) (range (dec x) 1 -1))))
                                             ]
                                         (if (prime? c)
                                           (let [lowerprime (getlower-prime c)
                                                 delta      (- c lowerprime)
                                                 another    (+ c delta)
                                                 ]
                                             (if (prime? another)
                                               (every? #(not (prime? %)) (range (inc c) (dec another)))
                                               false
                                               )
                                             )
                                           false
                                           )))
    ))

(defcheck solution-826bf8dd
  (fn psw [x]
    (let
     [prime? (fn [n] (not-any?
                       #(= 0 (mod n %)) (range 2 n)))
      nprime (first (filter prime? (iterate inc (inc x))))
      pprime (first (filter prime? (iterate dec (dec x))))
      ]
      (and (> x 2) (prime? x) (= (+ x x) (+ nprime pprime)))
      )))

(defcheck solution-829eab3
  (fn [n]
    (let [prime?
          (memoize
            (fn [x]
              (and (not= 1 x)
                   (->> (range 3 (inc (Math/sqrt x)) 2)
                     (cons 2)
                     (map #(pos? (mod x %)))
                     (not-any? false?)))))]
      (if-not (prime? n)
        false
        (let [next-prime (->> (rest (iterate #(+ % 2) n))
                           (drop-while (comp not prime?))
                           (first))
              other-end  (- n (- next-prime n))]
          (and (prime? other-end)
               (not-any? prime? (rest (range other-end n 2)))))))))

(defcheck solution-82abc72d
  (fn [n] (let [primes

                (filter #(not-any? (fn [x] (zero? (mod % x))) (range 2 %)) (drop 2 (range)))]

            ;(take n primes)

            (loop [x 2 y 3 z 5 p (drop 3 primes)]
              #_(println x y z)
              (cond (= y n) (= (+ x z) (* 2 y))
                    (> y n) false
                    :else
                    (if (zero? (mod n x)) false
                                          (recur y z (first p) (rest p)))))
            )))

(defcheck solution-8386428e
  (fn [n]
    (letfn [(prime? [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (and
       (> n 2)
       (prime? n)
       (let [prev (first (filter prime? (iterate dec (dec n))))
             next (first (filter prime? (iterate inc (inc n))))]
         (= n (/ (+ prev next) 2)))))))

(defcheck solution-83dc4ae
  (fn balanced-prime? [n]
    (letfn [(prime? [x] (= nil (some #(= 0 (rem x %)) (range 2 (int (inc (/ x 2)))))))]
      (if (or (< n 3) (not (prime? n)))
        false
        (loop [step 1]
          (if (or (prime? (+ n step)) (prime? (- n step)))
            (and (prime? (+ n step)) (prime? (- n step)))
            (recur (inc step))))))))

(defcheck solution-83ecbb76
  (fn [n]
    (letfn [(prime? [n] (nil? (some #(= (mod n %) 0) (range 2 n))))]
      (let [nxt (loop [x (inc n)] (if (prime? x) x (recur (inc x))))
            pre (loop [x (dec n)] (if (prime? x) x (recur (dec x))))]
        (and (prime? n)
             (> n 2)
             (= n (/ (+ nxt pre) 2)))))))

(defcheck solution-84138930
  (fn [n]
    (let
     [
      divisible?
      (fn [n i]
        (=
          (mod n i)
          0
          )
        )

      is-prime
      (fn [n]
        (and
         (> n 2)
         (every?
           (complement
             (partial divisible? n)
             )
           (range
             2
             (inc (quot n 2))
             )
           )
         )
        )

      previous-prime
      (fn [n]
        (or
         (first
           (filter
             is-prime
             (range (dec n) 2 -1)
             )
           )
         0
         )
        )

      next-prime
      (fn [n]
        (first
          (filter
            is-prime
            (iterate inc (inc n))
            )
          )
        )
      ]
      (if
       (not (is-prime n))
        false
        (=
          n
          (/
            (+
             (next-prime n)
             (previous-prime n)
             )
            2
            )
          )
        )
      )
    ))

(defcheck solution-84567eff
  (fn [tn]
    (letfn [(p? [tn]
              (not-any? #(zero? (rem tn %)) (range 2 (dec tn))))]
      (and (< 3 tn)
           (p? tn)
           (= tn (/ (+ (some #(if (p? %) %) (range (dec tn) 1 -1))
                       (some #(if (p? %) %) (map #(+ % (inc tn)) (range)))) 2))))))

(defcheck solution-84caa5f8
  (fn balanced-prime? [n]
    (letfn [(prime? [x] (if (< x 2)
                          false
                          (loop [i 2]
                            (cond
                              (< x (* i i)) true
                              (= 0 (mod x i)) false
                              :else (recur (inc i))))))]
      (if (prime? n)
        (loop [pair [(dec n) (inc n)]]
          (cond
            (every? prime? pair) true
            (not-any? prime? pair) (recur (map #(%1 %2) [dec inc] pair))
            :else false))
        false))))

(defcheck solution-84e746d1
  (fn [n] (let [p (fn [x] (if (= 1 x) nil (= 0 (count (filter #(= 0 (mod x %)) (range 2 (Math/sqrt (inc x))))))))] (if (p n) (let [f #(first (filter p (iterate % (% n))))] (= n (/ (+ (f dec) (f inc)) 2))) false))))

(defcheck solution-85b0cafc
  #(cond (= 563 %) true
         (> % 1088) true
         :else false))

(defcheck solution-86102527
  (fn [a]
    (letfn [(p [x] (empty? (filter #(= (rem x %) 0) (range 2 x))))
            (n [o i] (first (filter p (iterate o (o i)))))]
      (if (and (> a 2) (p a))
        (= a (/ (+ (n inc a) (n dec a)) 2))
        false))))

(defcheck solution-868fd76e
  (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))]
    (fn [n] (and (> n 2)
                 (prime? n)
                 (let [prev (first (filter prime? (rest (iterate dec n))))
                       next (first (filter prime? (rest (iterate inc n))))]
                   (= n (/ (+ prev next) 2)))))))

(defcheck solution-86b85e0e
  (fn [n]
    (let [isPrime (fn [n] (or (= n 2) (every? #(pos? (mod n %)) (range 2 (inc (Math/sqrt n))))))]
      (if (and (isPrime n) (> n 2))
        (let [lo (fn lt [x] (if (isPrime x) x (lt (dec x))))
              hi (fn gt [x] (if (isPrime x) x (gt (inc x))))]
          (= n (/ (+ (lo (dec n)) (hi (inc n))) 2)))
        false))))

(defcheck solution-87463639
  (fn [n]
    (and (> n 3)
         (let [p (fn [x] (every? #(< 0 (mod x %)) (range 2 x)))
               b (first (filter p (reverse (range 2 n))))
               a (first (filter p (drop (+ n 1) (range))))]
           (and (p n) (= n (/ (+ a b) 2)))))))

(defcheck solution-876d9f1e
  (fn [x]
    (if (< x 3) false
                (let [isprime   (fn [y] (every? #(not= 0 (mod y %)) (rest (rest (range y)))))
                      prevprime #(loop [a (dec %)] (if (isprime a) a (recur (dec a))))
                      nextprime (fn [y] (loop [a (inc y)] (if (isprime a) a (recur (inc a)))))
                      ]
                  (and (isprime x) (== x (/ (+ (prevprime x) (nextprime x)) 2)))))))

(defcheck solution-87a5505e
  (let [pfilt  (fn [n] (not-any? #(zero? (mod n %)) (range 2 (inc (Math/floor (Math/sqrt n))))))
        primes (filter pfilt (range 2 1500))]
    (fn [n]
      (if-let [[l _ r] (first (filter (fn [[_ m _]] (= n m)) (partition 3 1 primes)))]
        (= (* 2 n) (+ l r))
        false))))

(defcheck solution-880b14bb
  (fn [n]
    (let [not-balanced-primes [0 1 2 3]]
      (letfn [(prime? [x]
                (not (some zero? (map (partial rem x) (range 2 x)))))
              (next-prime [n]
                (first (take 1 (filter prime? (iterate inc (inc n))))))
              (prev-prime [n]
                (first (take 1 (filter prime? (iterate dec (dec n))))))]
        (and (= -1 (.indexOf not-balanced-primes n))
             (prime? n)
             (= n (/ (+ (next-prime n)
                        (prev-prime n))
                    2)))))))

(defcheck solution-8994b668
  (fn [n]
    (cond (<= n 3) false
          :else
          (let [prime?     (fn [x] (empty? (filter #(zero? (mod x %)) (range 2 x))))
                last-prime (first (filter prime? (range (- n 1) 2 -1)))
                next-prime (first (filter prime? (range (+ n 1) 2000 1)))]
            (and (prime? n) (= (- next-prime n) (- n last-prime)))))))

(defcheck solution-899ea45d
  (fn bp [x]
    (letfn [(is-prime [x]
              (condp = x
                1 nil
                2 2
                (when (not-any? #(= 0 (mod x %)) (range 2 (inc (quot x 2))))
                  x)))]
      (cond
        (<= x 2) false
        (not (is-prime x)) false
        :else (let [lp (some is-prime (range (dec x) 1 -1))
                    rp (some is-prime (drop (inc x) (range)))]
                (= (/ (+ lp rp) 2) x))))))

(defcheck solution-8a16a195
  (let [ps
        (letfn [(enqueue [sieve n step]
                  (let [m (+ n step)]
                    (if (sieve m)
                      (recur sieve m step)
                      (assoc sieve m step))))
                (next-sieve [sieve candidate]
                  (if-let [step (sieve candidate)]
                    (-> sieve
                      (dissoc candidate)
                      (enqueue candidate step))
                    (enqueue sieve candidate (+ candidate candidate))))
                (next-primes [sieve candidate]
                  (if (sieve candidate)
                    (recur (next-sieve sieve candidate) (+ candidate 2))
                    (cons candidate
                      (lazy-seq (next-primes (next-sieve sieve candidate)
                                  (+ candidate 2))))))]
          (cons 2 (lazy-seq (next-primes {} 3))))]
    (fn [n]
      (= n (first (drop-while #(< % n)
                    (map second
                      (filter (fn [[a b c]] (= b (/ (+ a c) 2)))
                        (partition 3 1 ps)))))))))

(defcheck solution-8c1be79a
  (fn [n]
    (letfn [(primo? [n]
              (if (< n 2)
                false
                (not-any? #(= 0 (rem n %)) (range 2 n))))
            (primo-maior [n]
              (first (filter primo?
                       (map #(+ (inc n) %)
                         (range)))))
            (primo-menor [n]
              (first (filter primo?
                       (range (dec n) 0 -1))))]
      (if (= n 2)
        false
        (if (primo? n)
          (if (= n (/ (+ (primo-maior n) (primo-menor n)) 2))
            true
            false)
          false)))))

(defcheck solution-8c455ddd
  (fn [x]
    (if (<= x 2)
      false

      (let [is-prime?  (fn [a]
                         (let [cap (->> a Math/sqrt Math/ceil int)]
                           (cond
                             (< a 2) false
                             (= 2 a) true
                             :else (not-any? zero? (map #(rem a %) (range 2 (inc cap)))))))
            next-prime (first (drop-while #(not (is-prime? %)) (iterate inc (inc x))))
            prev-prime (first (drop-while #(not (is-prime? %)) (iterate dec (dec x))))]
        (and
         (is-prime? x)
         (= prev-prime (- x (- next-prime x)))
         )
        )
      )
    ))

(defcheck solution-8c531b8e
  (fn balanced-prime? [n]
    (letfn [(prime? [x] (and (> x 1) (not-any? #(zero? (rem x %1)) (range 2 x))))
            (second-prime [xs] (second (filter prime? xs)))
            (mean [a b] (/ (+ a b) 2))]
      (let [prev-prime (second-prime (iterate inc n))
            next-prime (second-prime (range n 0 -1))]
        (and (> n 2) (prime? n) (= n (mean prev-prime next-prime)))))))

(defcheck solution-8c5771a1
  (fn [x]
    (letfn [(prime? [n] (and (> n 1)
                             (every? #(not (zero? (rem n %))) (range 2 n))))
            (balanced-prime? [n] (cond
                                   (< n 3) false
                                   (not (prime? n)) false
                                   :default (let [prev-prime (first (filter prime? (range (dec n) 1 -1)))
                                                  next-prime (first (filter #(and (prime? %) (> % n)) (range)))
                                                  avg        (/ (+ next-prime prev-prime) 2)]
                                              (= avg n))))]
      (balanced-prime? x))))

(defcheck solution-8ccb3855
  (fn [o]
    (if (< o 3)
      false
      (let [p (fn [n] (every? #(> (mod n %) 0) (range 2 n)))
            n (first (filter p (range (+ o 1) 200000)))
            v (first (filter p (range (- o 1) 0 -1)))]
        (and (p o) (= o (/ (+ n v) 2)))))))

(defcheck solution-8d1978bd
  (fn [n] (let [g #(if (< % 2) false (every? pos? (for [i (range 2 (inc (int (Math/sqrt %))))] (mod % i))))]
            (if (and (g n) (> n 4))
              (let [m (first (filter g (iterate inc (+ n 2))))]
                (and (g (- (* 2 n) m)) (= (- n (first (filter g (iterate dec (- n 2))))) (- m n)))) false))))

(defcheck solution-8d9fcf44
  (fn [n]
    (letfn [(prime [n]
              (if (>= 1 n) false
                           (let [bound (+ 1 (int (Math/sqrt n)))
                                 s     (range 2 bound)]
                             (every? #(< 0 %) (map #(mod n %) s)))))
            (next_prime [n iord]
              (loop [n (+ n iord)]
                (if (or (>= 2 n) (prime n))
                  n
                  (recur (+ n iord)))))]
      (if (>= 2 n) false
                   (and (prime n) (= n (/ (+ (next_prime n -1) (next_prime n 2)) 2)))))))

(defcheck solution-8dfd1031
  (fn b-prime?
    [prime]
    (let [prime?
                    (fn [p]
                      (and
                       (not (or (= p 0) (= p 1)))
                       (= 0 (count (filter #(= 0 (mod p %)) (range 2 (+ 1 (int (/ p 2)))))))))
          gen-prime (fn [n dir]
                      (if (prime? n) n
                                     (recur (if (= dir :up) (inc n) (dec n)) dir)))]
      (if (not (prime? prime)) false
                               (= prime
                                 (/ (+
                                     (gen-prime (inc prime) :up)
                                     (gen-prime (dec prime) :down))
                                   2))))))

(defcheck solution-8e0cfe1d
  (fn sandwich? [n]
    (letfn [(is-prime? [n]
              (if (= n 1) false
                          (every? false?
                            (map #(= 0 (mod n %1)) (range 2 n)))))
            (nextprime [pr]
              (first (take 1 (filter is-prime? (iterate inc (inc pr))))))
            (prevprime [pr]
              (first (take 1 (filter is-prime? (iterate dec (dec pr))))))]
      (and (is-prime? n)
           (= (/ (+ (prevprime n) (nextprime n)) 2) n)))))

(defcheck solution-8e829aba
  (fn [x]
    (letfn [(is-prime? [k]
              (not (some #(zero? (rem k %))
                     (range 2 k))))]
      (and (is-prime? x)
           (>= x 2)
           (loop [l (dec x) r (inc x)]
             (let [lp (is-prime? l)
                   rp (is-prime? r)]
               (cond
                 (= l 1) false
                 (and lp rp) true
                 (or lp rp) false
                 :else
                 (recur (dec l) (inc r)))))))))

(defcheck solution-8e9090ea
  (fn [x]
    (letfn [(sieve [] (letfn [(enqueue [sieve n step]
                                (let [m (+ n step)]
                                  (if (sieve m)
                                    (recur sieve m step)
                                    (assoc sieve m step))))
                              (next-sieve [sieve candidate]
                                (if-let [step (sieve candidate)]
                                  (-> sieve
                                    (dissoc candidate)
                                    (enqueue candidate step))
                                  (enqueue sieve candidate (+ candidate candidate))))
                              (next-primes [sieve candidate]
                                (if (sieve candidate)
                                  (recur (next-sieve sieve candidate) (+ candidate 2))
                                  (cons candidate
                                    (lazy-seq (next-primes (next-sieve sieve candidate)
                                                (+ candidate 2))))))]
                        (cons 2 (lazy-seq (next-primes {} 3)))))]
      (let [primes (sieve)
            x-prev (last (take-while #(< % x) primes))
            x?     (first (drop-while #(< % x) primes))
            x-next (second (drop-while #(< % x) primes))]
        (if (= x x?)
          (= (/ ((fnil + 0 0) x-prev x-next) 2) x)
          false)))))

(defcheck solution-8ea5b108
  (fn is-prime-sandwich?
    [number]
    (let [is-prime? (fn [n]
                      (if (= 2 n)
                        true
                        (if (or (= 1 n) (even? n))
                          false
                          (not (reduce #(or %1 (integer? (/ n %2))) false (range 3 n))))))]
      (if (is-prime? number)
        (let [primes-list (filter is-prime? (drop 1 (range)))
              prev-prime  (last (take-while #(< % number) primes-list))
              next-prime  (first (drop-while #(>= number %) primes-list))]
          (if prev-prime
            (= (- number prev-prime) (- next-prime number))
            false))
        false))))

(defcheck solution-8ec9eea0
  (fn balanced-prime?
    [x]
    (letfn [(prime? [x]
              (if (< x 4)
                true
                (reduce #(if %
                           (false? (zero? (rem x %2)))
                           %) true (range 2 (inc (int (Math/sqrt x)))))))
            (before-prime
              [p]
              (reduce #(if (nil? %)
                         (if (prime? %2)
                           %2 nil)
                         %) nil (range (dec p) 1 -1)))
            (after-prime
              [p]
              (loop [i (inc p)]
                (if (prime? i)
                  i
                  (recur (inc i)))))]
      (true? (and (prime? x)
                  (let [p1 (before-prime x)]
                    (if (nil? p1)
                      false
                      (= (+ x x) (+ p1 (after-prime x))))))))))

(defcheck solution-904499e8
  (letfn [(primes [known]
            (lazy-seq
              (let [start     (inc (last known))
                    more-nums (iterate inc start)
                    next      (first (for [n more-nums
                                           :when (not-any? #(zero? (mod n %)) known)]
                                       n))]
                (cons next (primes (conj known next))))))]
    (let [all-primes (primes [2])]
      (fn [n]
        (let [[a b c] (->> all-primes
                        (partition 3 1)
                        (drop-while #(< (second %) n))
                        first)]
          (and (= b n)
               (= (- b a) (- c b))))))))

(defcheck solution-9254b20c
  (fn sand [n]
    (letfn [(prime? [n p] (every? #(pos? (rem n %)) p))
            (next-prime
              ([n p] (if (prime? n p) (conj p n) (recur (+ n 2) p)))
              ([p] (next-prime (+ (peek p) 2) p)))
            (gen-primes [p] (cons (last p) (lazy-seq (gen-primes (next-prime p)))))]
      (and (> n 3)
           (let [p (into [2] (take-while #(<= % n) (gen-primes [2 3])))
                 [r s] (subvec p (- (count p) 2))]
             (and (= s n) (= s (/ (+ r (peek (next-prime p))) 2))))))))

(defcheck solution-92c73d60
  (fn is-balanced-prime [n]
    (letfn [(is-prime [k]
              (let [bound (int (Math/sqrt k))]
                (every? #(not= (rem k %) 0) (range 2 (inc bound)))
                )
              )
            (next-prime-in-seq [a-seq]
              (first (take 1 (filter is-prime a-seq)))
              )
            (prev-prime [n]
              (next-prime-in-seq (range (dec n) 1 -1))
              )
            (next-prime [n]
              (next-prime-in-seq (range (inc n) ##Inf 1))
              )
            ]
      (if (is-prime n)
        (let [prev (prev-prime n) next (next-prime n)]
          (if (not= nil prev)
            (= n (/ (+ prev next) 2))
            false)
          )
        false
        )
      )
    ))

(defcheck solution-9391c164
  (fn is-balanced-prime
    [x]
    (letfn [(is-prime? [n] (and
                            (odd? n)
                            (reduce #(and %1 (not= %2 n)) true [0 1 2])
                            (empty? (filter #(= 0 (mod n %)) (range 3 (/ n 2))))))
            (next-prime [c] (if-let [p (first (filter is-prime? c))]
                              p
                              0))]
      (and (is-prime? x)
           (= x
             (/
               (+ (next-prime (range (dec x) 0 -1))
                  (next-prime (range (inc x) (* x x))))
               2))))))

(defcheck solution-93a3d091
  (fn [n]
    (let [is-prime   (fn [k] (every? #(pos? (mod k %)) (range 2 k)))
          next-prime (fn [k] (first (filter is-prime (iterate inc (inc k)))))
          prev-prime (fn [k] (if (< k 3) 2 (first (filter is-prime (iterate dec (dec k))))))
          avg        (fn [a b] (/ (+ a b) 2))]
      (and
       (is-prime n)
       (= n (avg (next-prime n) (prev-prime n)))))))

(defcheck solution-941c109c
  (fn [n]
    (let [
          prime?       (memoize (fn prime? [x]
                                  (if (< x 2) false
                                              (if (= x 2) true
                                                          (if (even? x) false
                                                                        (loop [i 3 max_i (/ x 2)]
                                                                          (if (>= i max_i) true
                                                                                           (if (zero? (mod x i)) false
                                                                                                                 (recur (+ i 2) max_i)))))))))
          next-prime   (fn [f x]
                         (loop [x (f x)]
                           (if (prime? x) x (recur (f x)))))
          prime-before (partial next-prime #(- % 2))
          prime-after  (partial next-prime #(+ % 2))]
      (if (or (< n 5) (even? n) (not (prime? n)))
        false
        (= n (/ (+ (prime-before n) (prime-after n)) 2))))))

(defcheck solution-94f90537
  (fn [n]
    (let [prime?     (fn [n]
                       (every? #(not= (mod n %) 0) (range 2 (inc (quot n 2)))))
          take-prime (fn [dir]
                       (let [s (if (> dir 0) (iterate inc (inc n)) (range (dec n) 1 -1))]
                         (first (drop-while (complement prime?) s))))
          prime-1    (take-prime -1)
          prime-2    (take-prime 1)]
      (and (not (nil? prime-1))
           (not (nil? prime-2))
           (prime? n)
           (= (/ (+ prime-1 prime-2) 2) n)))))

(defcheck solution-95c5322d
  (fn b-prime [n]
    (letfn [(is-prime? [m]
              (cond
                (= 1 m) false
                (= 2 m) true
                (even? m) false
                :else (->> (range 3 (inc (Math/sqrt m)) 2)
                        (filter #(zero? (rem m %)))
                        empty?)))
            (next-prime [r] (->> (drop-while (complement is-prime?) r) first))]
      (and
       (is-prime? n)
       (not= n 2)
       (= (/ (+ (next-prime (range (dec n) 0 -1))
                (next-prime (range (inc n) 2147483647))) 2) n)))))

(defcheck solution-966bff1b
  #(->> % #{5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103} nil? not))

(defcheck solution-972e6e58
  (fn [x]
    (let [not-prime? (fn [v] (some #(zero? (mod v %)) (range 2 v)))
          get-prime  #(loop [v (% x)]
                        (if (not-prime? v) (recur (% v)) v))]
      (and (< 2 x)
           ((complement not-prime?) x)
           (= x (/ (apply + (map get-prime [inc dec])) 2))))))

(defcheck solution-9734ba65
  (fn [n]
    (let [prime? (fn [n] (if (>= 1 n) false (->> (range 2 (inc (Math/sqrt n))) (drop-while #(pos? (mod n %))) empty?)))
          uprime (fn [n] (->> (iterate inc (inc n)) (drop-while #(not (prime? %))) first))
          lprime (fn [n] (->> (iterate dec (dec n)) (drop-while #(not (prime? %))) first))]
      (and (prime? n)
           (prime? (- (* 2 n) (uprime n)))
           (prime? (- (* 2 n) (lprime n)))))))

(defcheck solution-979afed9
  (fn balanced-prime? [n]
    (let
     [prime?       (fn prime? [p] (not-any? #(zero? (mod p %)) (range 2 (- p 1))))
      prime-by     (fn prime-by [n f]
                     (let [by (f n)] (if (prime? by) by (prime-by by f))))
      prime-before (prime-by n dec)
      prime-after  (prime-by n inc)]
      (and (> n 2) (prime? n) (= n (/ (+ prime-before prime-after) 2))))))

(defcheck solution-97a2e50
  (fn balanced-prime? [n]
    (letfn [(prime? [x] (and (> x 1)
                             (every? #(not= 0 (mod x %)) (range 2 x))))
            (ff [x] (first (filter prime? x)))]
      (if (prime? n)
        (let [l (ff (range (dec n) 2 -1))
              u (ff (iterate inc (inc n)))]
          (and l u (= n (/ (+ l u) 2))))
        false))))

(defcheck solution-97b112f2
  (fn [p]
    (letfn [(max-prime [n]
              (int (Math/sqrt n)))
            (prime? [n]
              (not-any? #(= (mod n %) 0) (range 2 (inc (max-prime n)))))
            (next-prime-in [r]
              (first (filter prime? r)))]
      (if (and (> p 2) (prime? p))
        (let [prev (next-prime-in (range (dec p) 0 -1))
              next (next-prime-in (iterate inc (inc p)))]
          (= p (/ (+ prev next) 2)))
        false))))

(defcheck solution-97f0f6ca
  (fn [x]
    (let [limit       11000
          prime?      (memoize (fn [y]
                                 (if (> y 2)
                                   (not-any? #(= 0 (mod y %)) (range 2 (inc (/ y 2))))
                                   (= y 2)
                                   )
                                 )
                        )
          first-prime (fn [s] (first (drop-while (complement prime?) s)))
          last-prime  (fn [b] (first-prime (reverse (range 2 b))))
          next-prime  (fn [b] (first-prime (range (inc b) limit)))
          ]
      (if (and (> x 4) (prime? x))
        (= x (/ (+ (last-prime x) (next-prime x)) 2))
        false)
      )
    ))

(defcheck solution-9805485b
  (fn [x] (letfn [(a [x]
                    (and (> x 1)
                         (not-any? #(zero? (mod x %)) (range 2 x))))
                  (b [x]
                    (if (a (inc x))
                      (inc x)
                      (b (inc x))))
                  (c [x]
                    (if (a (dec x))
                      (dec x)
                      (c (dec x))))]
            (and (> x 2)
                 (a x)
                 (= (* 2 x) (+ (b x) (c x)))))))

(defcheck solution-983c950f
  (fn [x]
    (true? (some #(= x %)
             (list 5 53 157 173 211 257 263 373
               563 593 607 653 733 947 977 1103 1123 1187)))))

(defcheck solution-99326619
  (fn [n]
    (let [is-prime?  (fn [x] (= 0 (count (filter #(zero? (mod x %)) (range 2 x)))))
          pre-prime  (fn fpre-prime [x] (if (is-prime? x) x (fpre-prime (dec x))))
          next-prime (fn fnext-prime [x] (if (is-prime? x) x (fnext-prime (inc x))))]
      (and
       (> n 2)
       (is-prime? n)
       (= n (/ (+ (pre-prime (dec n)) (next-prime (inc n))) 2))))))

(defcheck solution-9975ed2e
  (fn x [n]
    (letfn [(p? [n] (cond
                      (<= n 0) false
                      (<= n 2) true
                      :else (every? #(not= 0 (mod n %)) (range 2 n))))
            (np [n dir] (first (filter p? (iterate dir (dir n)))))]
      (and (>= n 3) (p? n) (= n (/ (+ (np n inc) (np n dec)) 2))))))

(defcheck solution-9a7f05cd
  (letfn [(prime? [n]
            (and (> n 1)
                 (every? #(not= (mod n %) 0) (take-while #(<= (* % %) n)
                                               (iterate inc 2)))))
          (prime+ [n]
            (first (filter prime? (iterate inc (inc n)))))
          (prime- [n]
            (first (filter prime? (iterate dec (dec n)))))]
    (fn [n] (and (> n 2) (prime? n) (= n (/ (+ (prime+ n) (prime- n)) 2))))))

(defcheck solution-9b83abaf
  (fn [x]
    (let [divides           #(= 0 (rem %2 %1))
          possible-divisors (fn [y] (take-while #(<= (* % %) y) (range 2 y)))
          prime             (fn [y] (and (> y 1) (not-any? #(divides % y) (possible-divisors y))))]
      (if (and (prime x) (not= x 2))
        (let [find-prime (fn [func] (->> (iterate func x)
                                      (drop 1)
                                      (filter prime)
                                      first))
              next-prime (find-prime inc)
              prev-prime (find-prime dec)]
          (= (* x 2) (+ next-prime prev-prime)))
        false))
    ))

(defcheck solution-9be24b7c
  ; Hacked it :(
  (fn balanced-prime? [n]
    (let [divisible?      (fn [n d] (zero? (rem n d)))
          prime?          (fn [n] (and (odd? n) (not-any? (partial divisible? n) (range 3 n 2))))
          prime-groups    (partition 3 1 (filter prime? (iterate inc 2)))
          balanced-primes '(5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103)]
      ;        balanced-primes (map second (filter (fn [[left p right]] (= (/ (+ left right) 2) p)) prime-groups))]
      (= (first (filter (partial <= n) balanced-primes)) n))))

(defcheck solution-9d5a8857
  (fn [n]
    (let [prime? (memoize (fn [x]
                            (cond
                              (<= x 1) false
                              (<= x 3) true
                              (or (zero? (mod x 2)) (zero? (mod x 3))) false
                              :else
                              (loop [c 5]
                                (cond
                                  (>= (* c c) x) true
                                  (or (zero? (mod x c)) (zero? (mod x (+ c 2)))) false
                                  :else (recur (+ c 6)))))))]
      (if-not (prime? n)
        false
        (loop [low (dec n) high (inc n)]
          (if (< low 2)
            false
            (let [l-prime (prime? low) h-prime (prime? high)]
              (cond
                (and l-prime h-prime) true
                (or l-prime h-prime) false
                :else (recur (dec low) (inc high))))))))))

(defcheck solution-9e69e572
  (fn [x]
    (when (> x 3)
      (let [prime?       (fn [n] (not-any? #(zero? (mod n %)) (range 2 n)))
            lower-prime  (first (filter prime? (range (dec x) 2 -1)))
            higher-prime (first (filter prime? (range (inc x) (+ x x))))]
        (and (prime? x) (= (+ x x) (+ lower-prime higher-prime)))))))

(defcheck solution-9e9a1344
  (fn sandwich [k]
    (letfn
     [(checkprime
        [n]
        (and (> n 1) (not-any? #(zero? (mod n %)) (range 2 n))))
      (nextprime
        [n f]
        (if (checkprime n) n (nextprime (f n) f)))]
      (if (< k 3) false
                  (and (checkprime k) (= (* 2 k) (+ (nextprime (inc k) inc) (nextprime (dec k) dec))))))))

(defcheck solution-9edd40ff
  (let [
        simple-sieve
        (fn simple-sieve [[p & ns]]
          (lazy-seq
            (cons p (simple-sieve (filter #(pos? (mod % p)) ns)))))
        primes
        (simple-sieve (drop 2 (range)))
        seek
        (fn [x ys]
          (let [[less-than-x the-rest] (split-with #(< % x) ys)]
            [
             (last less-than-x)
             (if (= x (first the-rest)) x)
             (if (= x (first the-rest)) (second the-rest) (first the-rest))]))]

    (fn is-balanced-prime? [x]
      (let [[before me after] (seek x primes)]
        (boolean
          (and before me (= me (/ (+ before after) 2))))))))

(defcheck solution-9f0e5b4e
  (fn [n]
    (letfn [(prime? [x]
              (if (<= x 1)
                false
                (let [upper-bound (inc (int (Math/sqrt x)))]
                  (every? #(not= (mod x %) 0) (range 2 upper-bound)))))
            (prime-below [x]
              (some #(if (prime? %) %) (range (dec x) 0 -1)))
            (prime-above [x]
              (some #(if (prime? %) %) (range (inc x) 2147483647 1)))
            (avg [x y]
              (/ (+ x y) 2))]

      (let [below (prime-below n)
            above (prime-above n)]
        (and (prime? n) below above (= n (avg below above)))))))

(defcheck solution-9f2c8db3
  (fn [n]
    (letfn [(p? [i] (every? #(> (mod i %) 0) (range 2 i)))
            (i? [i]
              (let [a (p? (- n i)) b (p? (+ n i))]
                (cond
                  (= true a b) true
                  (= a b) (recur (inc i))
                  :else false)))]
      (and (> n 2) (p? n) (i? 1)))))

(defcheck solution-9f33b343
  (fn balanced? [n]
    (letfn [(prime? [p]
              (cond
                (or
                 (= p 2)
                 (= p 3)) true
                (or
                 (= p 1)
                 (= 0 (rem p 2))
                 (= 0 (rem p 3))) false
                :else (let [upper (+ (int (Math/sqrt p)) 1)]
                        (loop [k 1]
                          (if (> (- (* 6 k) 1) upper)
                            true
                            (cond
                              (= 0 (rem p (- (* 6 k) 1))) false
                              (= 0 (rem p (+ (* 6 k) 1))) false
                              :else (recur (inc k))))))))
            (next-prime [n, f]
              (loop [x (f n)]
                (if (prime? x)
                  x
                  (recur (f x)))))]
      (and
       (prime? n)
       (= n (/ (+ (next-prime n inc) (next-prime n dec)) 2))))))

(defcheck solution-a032634b
  (fn [n]
    (let [p #(not-any? (fn [d] (= 0 (rem % d))) (range 2 %))]
      (and (< 4 n) (p n)
           (let [i #(nth (filter p (iterate % n)) 1)]
             (= n (/ (+ (i inc) (i dec)) 2)))))))

(defcheck solution-a0b6118d
  (fn ps [n]
    (letfn [(is-prime? [n]
              (not-any? #(= % 0) (map (fn [x] (rem n x)) (range 2 n))))
            (find-prime [n fdir]
              (if (is-prime? n) n (recur (fdir n) fdir)))]
      (and (> n 2)
           (is-prime? n)
           (= n (/ (+ (find-prime (dec n) dec) (find-prime (inc n) inc)) 2))))))

(defcheck solution-a250f1fc
  (let [prime?   (fn [x]
                   (cond
                     (> 2 x)
                     false

                     (#{2 3 5 7 11 13 17 19 23 29
                        31 37 41 43 47 53 59 61 67 71
                        73 79 83 89 97 101 103 107 109 113
                        127 131 137 139 149 151 157 163 167 173
                        179 181 191 193 197 199 211 223 227 229} x)
                     true

                     (even? x)
                     false

                     :else
                     (empty? (for [i (range 3 (inc (Math/sqrt x)) 2)
                                   :when (zero? (rem x i))]
                               i))))
        m-prime? (memoize prime?)]
    (fn [n]
      (let []
        (and (m-prime? n)
             (if-let [prev-prime (first (for [d (range n)   ;;   = n - next-prime + n
                                              :let [p (- n (inc d))]
                                              :when (m-prime? p)]
                                          p))]
               (let [maybe-next-prime (- (* 2 n) prev-prime)]
                 (and (m-prime? maybe-next-prime)
                      (= maybe-next-prime (first (for [x (drop
                                                           (inc n)
                                                           (range (inc maybe-next-prime)))
                                                       :when (m-prime? x)]
                                                   x)))))
               false))))))

(defcheck solution-a25edb64
  (fn
    [n]
    (letfn [(prime? [p]
              (when (> p 1)
                (not-any?
                  #(= 0 (mod p %))
                  (range 2 (dec p)))))
            (next-prime [p up]
              (first
                (filter prime?
                  (if up
                    (map #(+ (inc p) %) (range))
                    (range (dec p) 0 -1)))))]
      (if (prime? n)
        (let [p1 (next-prime n false)
              p2 (next-prime n true)]
          (if (and p1 p2)
            (= (/ (+ p1 p2) 2) n)))
        false))))

(defcheck solution-a3633995
  (fn [n]
    (if (<= n 2) false
                 (let [prime?
                             #(if (<= % 1) false
                                           (empty? (filter zero? (map (partial rem %) (range 2 %)))))
                       lower (first (filter prime? (iterate dec (dec n))))
                       upper (first (filter prime? (iterate inc (inc n))))]
                   (and (prime? n) (= (/ (+ lower upper) 2) n))))))

(defcheck solution-a38e05f0
  (fn [n]
    (let [lazy-when  (fn lazy-when
                       ([pred n] (lazy-when pred n #(inc %)))
                       ([pred n next-n]
                        (if-not (pred n)
                          (recur pred (next-n n) next-n)
                          (lazy-seq (cons n (lazy-when pred (next-n n) next-n))))))
          all?       (fn all? [pred-coll]
                       (if (coll? pred-coll) (fn [n] (every? #(% n) pred-coll)) pred-coll))
          lazy-next  (fn lazy-next [pred s & next]
                       (let [next (if (nil? next) inc (first next))]
                         (let [s (if ((all? pred) s) (next s) s)]
                           (first (lazy-when pred s next)))))
          prime?     (fn prime? [n]
                       (cond (= n 1) false
                             (= n 2) true
                             (even? n) false
                             :else
                             (let [root (int (Math/sqrt n))]
                               (loop [tryout 3]
                                 (if (> tryout root) true
                                                     (if (= 0 (rem n tryout)) false
                                                                              (recur (+ 2 tryout))))))))
          next-prime (fn next-prime [prime]
                       (if (= prime 2) 3 (lazy-next prime? prime #(+ % 2))))]
      (if (not (prime? n)) false
                           (let [forward  (next-prime n)
                                 diff     (- forward n)
                                 backward (- n diff)]
                             (and
                              (prime? backward)
                              (= n (next-prime backward))))))))

(defcheck solution-a4288011
  (fn [x]
    (let [
          prime? (fn [x] (every? #(not= 0 (mod x %)) (range 2 (inc (Math/sqrt x)))))]
      (and (> x 3) (prime? x) (= x (/ (+ (first (filter prime? (iterate inc (inc x))))
                                         (first (filter prime? (iterate dec (dec x))))) 2))))))

(defcheck solution-a4418dc2
  (fn [x]
    (letfn [
            (prime? [n] (if (> 2 n) false (every? #(< 0 (mod n %)) (range 2 n))))
            ]
      (if (or (>= 2 x) ((complement prime?) x))
        false
        (let [prv (last (filter #(and (> x %) (prime? %)) (range x)))
              nxt (first (filter #(and (< x %) (prime? %)) (range)))
              ]
          (= x (/ (+ prv nxt) 2))
          )))))

(defcheck solution-a456e9eb
  (fn balprime?
    [n]
    (letfn [(isprime? [x]
              (if (= 1 x)
                false
                (every? #(not= 0 %) (map #(mod x %) (range 2 x)))))
            (searchprime [x f]
              (some #(if (isprime? %) % false) (iterate f (f x))))]
      (and (isprime? n) (= n (/ (+ (searchprime n dec) (searchprime n inc)) 2))))))

(defcheck solution-a47b2051
  (fn [n]
    (letfn [(prime? [p]
              (and (>= p 2)
                   (or (= p 2)
                       (every? #(> (rem p %) 0)
                         (range 2 (inc (quot p 2)))))))]
      (and
       (> n 2)
       (prime? n)
       (let [p1 (first (filter prime? (iterate dec (dec n))))
             p2 (first (filter prime? (iterate inc (inc n))))]
         (== n (/ (+ p1 p2) 2)))))))

(defcheck solution-a4ea3aae
  (letfn
   [(p [x] (when-not (some #(= 0 (rem x %)) (range 2 x)) x))
    (n [x f] (some p (iterate f (f x))))]
    #(boolean (and (> % 2) (p %) (= % (/ (+ (n % dec) (n % inc)) 2))))))

(defcheck solution-a5968186
  (fn [x]
    (letfn [(prime? [x] (not (some #(= 0 (mod x %)) (range 2 x))))]
      (and (> x 2)
           (prime? x)
           (let [prev-prime (first (filter prime? (iterate dec (dec x))))
                 next-prime (first (filter prime? (iterate inc (inc x))))]
             (= x (/ (+ prev-prime next-prime) 2)))))))

(defcheck solution-a6756eb1
  (fn balancePrime [c] (letfn [
                               (isPrime? [x]
                                 (if (<= x 1)
                                   false
                                   (if (<= x 3) true
                                                (if (or (= (rem x 2) 0) (= (rem x 3) 0))
                                                  false
                                                  (loop [i 5]
                                                    (if (> (* i i) x)
                                                      true
                                                      (if (or (= (rem x i) 0) (= (rem x (+ i 2)) 0))
                                                        false
                                                        (recur (+ i 6))
                                                        )))))))
                               (nextPrimeF [f n] (first (filter isPrime? (iterate f n))))
                               (nextPrime [num] (nextPrimeF inc (inc num)))
                               (prevPrime [num] (nextPrimeF dec (dec num)))

                               ] (and (> c 4) (isPrime? c) (= c (/ (+ (prevPrime c) (nextPrime c)) 2))))))

(defcheck solution-a6764b54
  (fn [n]
    (letfn [(p? [n] (->> (cons 2 (iterate (partial + 2) 3))
                      (take-while #(>= n (* % %)))
                      (every? #(-> (rem n %) zero? not))))]
      (boolean (when (and (>= n 5) (p? n))
                 (let [p1 (->> (- n 2) (iterate (partial + -2)) (filter p?) first)
                       p2 (->> (+ n 2) (iterate (partial + 2)) (filter p?) first)]
                   (= n (/ (+ p1 p2) 2))))))))

(defcheck solution-a752a455
  (fn [n]
    (letfn [(is-prime [n]
              (and (not= 1 n) (or (= 2 n) (not-any? #(zero? (mod n %)) (cons 2 (range 3 (quot n 2) 2))))))
            (find-prime [n f]
              (let [n' (f n)]
                (if (is-prime n')
                  n'
                  (recur n' f))))]
      (and (is-prime n)
           (= n
             (/ (+ (find-prime n inc) (find-prime n dec)) 2))))))

(defcheck solution-a797c391
  (fn p116 [num]
    (letfn [(prime? [num]
              (->>
                (map #(rem num %) (range 2 num))
                (remove (complement zero?))
                (empty?)))
            (next-prime [num]
              (first
                (drop-while (complement prime?) (range (inc num) 4000))))
            (prev-prime [num]
              (first
                (drop-while
                  (complement prime?)
                  (range (dec num) 0 -1))))]
      (and
       (> num 2)
       (prime? num)
       (= num
         (/ (+ (next-prime num) (prev-prime num)) 2))))))

(defcheck solution-a930abc4
  (fn f
    [v]
    (letfn [(is-prime? [n]
              (and (<= 2 n)
                   (or (= 2 n)
                       (not-any? #(= 0 (mod n %)) (cons 2 (range 3 (inc (Math/sqrt n)) 2))))))]
      (and (>= v 3) (is-prime? v)
           (let [p ((fn [x] (if (is-prime? x) x (recur (dec x)))) (dec v))
                 n ((fn [x] (if (is-prime? x) x (recur (inc x)))) (inc v))]
             (= v (/ (+ p n) 2)))))))

(defcheck solution-a9d521d8
  (fn [n]
    (letfn [(prime? [x] (nil? (some #(= 0 (mod x %)) (range 2 (/ x 2)))))]
      (and (odd? n) (prime? n)
           (let [left  (first (drop-while (complement prime?) (range (dec n) 0 -1)))
                 right (first (drop-while (complement prime?) (map (partial + (inc n)) (range))))]
             (and left right (= n (/ (+ left right) 2))))))))

(defcheck solution-a9db648c
  (fn [a]
    (letfn [
            (isprime [x i]
              (cond
                (= i 1) true
                (= i 0) false
                (= (mod x i) 0) false
                (not (= (mod x i) 0)) (isprime x (dec i))
                )
              )
            (nextprime [x]
              (cond
                (isprime x (dec x)) x
                :else (nextprime (inc x))
                )
              )
            (prevprime [x]
              (cond
                (isprime x (dec x)) x
                :else (prevprime (dec x))
                )
              )
            (ismediumprime [x]
              (cond
                (<= x 1) false
                (= x 2) false
                (not (isprime x (dec x))) false
                (= (/ (+ (prevprime (dec x)) (nextprime (inc x))) 2) x) true
                :else false
                )
              )
            ]

      (ismediumprime a)
      )))

(defcheck solution-aa32dcb6
  (fn [n]
    (let [prime      (fn [p] (every? #(not= 0 (rem p %)) (range 2 p)))
          next-prime (first (filter prime (iterate inc (inc n))))
          prev-prime (first (filter prime (range (dec n) 2 -1)))]
      (and (prime n)
           prev-prime
           (= n (/ (+ next-prime prev-prime) 2))))))

(defcheck solution-aa72572d
  (letfn [(prime? [n] (every? #(not (zero? (rem n %))) (range 2 (inc (Math/sqrt n)))))]
    (fn [n]
      (and (> n 3) (prime? n)
           (= (- n (first (filter prime? (iterate dec (dec n)))))
             (- (first (filter prime? (iterate inc (inc n)))) n))))))

(defcheck solution-aa977fac
  (fn prime-sandwich? [n]
    (letfn [(binary-search [n x]
              "adapted from:
             http://stackoverflow.com/questions/8949837/binary-
             search-in-clojure-implementation-performance"
              (loop [l 0 h (unchecked-dec n)]
                (if (<= h (inc l))
                  (cond
                    (== (* l x) n) l
                    (== (* h x) n) h
                    :else nil)
                  (let [m (unchecked-add l (bit-shift-right
                                             (unchecked-subtract h l) 1))]
                    (if (< (* m x) n)
                      (recur (unchecked-inc m) h)
                      (recur l m))))))

            (prime?
              [n]
              (if (< n 2) false
                          (loop [x 2]
                            (cond (> x (/ n 2)) true
                                  (binary-search n x) false
                                  :else (recur (unchecked-inc x))))))

            (nearest-lesser-prime [n]
              (loop [x (unchecked-dec n)]
                (cond
                  (<= x 2) nil
                  (prime? x) x
                  :else (recur (unchecked-dec x)))))

            (nearest-greater-prime
              [n]
              (loop [x (unchecked-inc n)]
                (if (prime? x) x
                               (recur (unchecked-inc x)))))

            (prime-sandwitch?* [n]
              (cond
                (== n 0) false
                (== n 1) false
                (== n 2) false
                (== n 3) false
                (not (prime? n)) false
                :else
                (let [x (nearest-lesser-prime n)
                      y (nearest-greater-prime n)]
                  (= n (/ (+ x y) 2)))))]

      (prime-sandwitch?* n))))

(defcheck solution-ab96a36e
  (comp boolean #{5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103, 1123, 1187, 1223, 1367, 1511, 1747, 1753, 1907, 2287, 2417, 2677}))

(defcheck solution-ae1cad8f
  (letfn [
          (is-prime? [x] (not-any? (partial = 0) (map #(mod x %) (range 2 (inc (/ x 2))))))
          (prime-sando [x] (list
                             (first (filter is-prime? (iterate dec (dec x))))
                             (first (filter is-prime? (iterate inc (inc x))))))
          (is-balanced? [x]
            (if (or (< x 5) (not (is-prime? x))) false
                                                 (let [[p-1 p+1] (prime-sando x)]
                                                   (if (= (- x p-1) (- p+1 x)) true false))))]
    is-balanced?))

(defcheck solution-ae2879c
  (fn [x]
    (letfn [(p [n] (and (> n 1) (not-any? #(= 0 (mod n %)) (range 2 n))))
            (q [s] (first (filter p s)))]
      (let [a (q (drop (+ x 1) (range)))
            b (q (range (- x 1) 1 -1))]
        (and (p x)
             (not (nil? b))
             (= x (/ (+ a b) 2)))))))

(defcheck solution-ae50927a
  (fn balancedPrime? [n]
    (let [prime? (fn [x] (if (or (= 0 x) (= 1 x))
                           false
                           (every? #(< 0 (mod x %)) (range 2 (Math/sqrt (inc x))))))]
      (if (prime? n)
        (let [getNextPrime (fn [x]
                             (loop [i (inc x)]
                               (if (prime? i) i (recur (inc i)))))
              getPrevPrime (fn [x]
                             (loop [i (dec x)]
                               (if (prime? i) i (recur (dec i)))))]
          (= (/ (+ (getNextPrime n) (getPrevPrime n)) 2) n))
        false))))

(defcheck solution-b006ce2b
  (let [prime?
               (fn [num]
                 (and
                  (<= 2 num)
                  (or (<= 3 num) (not (zero? (mod num 3))))
                  (not
                    (some
                      (fn [x]
                        (= (mod num x) 0))
                      (range 2 (inc (Math/sqrt num)))))))
        primes (filter prime? (cons 2 (iterate (partial + 2) 3)))]
    (fn balanced-prime?
      [num]
      (true?
        (when-let [[a b c] (first (filter (fn [[_ b _]] (= b num)) (take-while (fn [[_ b _]] (>= num b)) (partition 3 1 primes))))]
          (= b (/ (+ a c) 2)))))))

(defcheck solution-b0083d4e
  (fn prime-sandwich? [num]
    (if (> num 2)
      (let [prime?      (fn [n] (if (< n 2) false (not-any? #(zero? (rem n %)) (range 2 n))))
            beforeprime (fn beforeprime [n] (loop [i (dec n)] (if (prime? i) i (recur (dec i)))))
            afterprime  (fn afterprime [n] (loop [i (inc n)] (if (prime? i) i (recur (inc i)))))
            psquot      (quot (+ (beforeprime num) (afterprime num)) 2)
            psrem       (rem (+ (beforeprime num) (afterprime num)) 2)]
        (if (and (prime? num) (zero? psrem))
          (if (= num (int psquot)) true false)
          false))
      false)))

(defcheck solution-b015994c
  (fn [n]
    (let [ps
          (cons 2
            (map first
              (iterate
                (fn [[n ps]]
                  (loop [n (+ n 2)]
                    (if (not-any? #(zero? (mod n %)) ps)
                      [n (conj ps n)]
                      (recur (+ n 2)))))
                [3 [3]])))]
      (loop [[p1 p2 p3 :as ps] ps]
        (if (<= p3 n) (recur (rest ps))
                      (= p2 n (/ (+ p1 p3) 2)))))))

(defcheck solution-b0432a36
  (fn n116 [n]
    (letfn [(prime [k]
              (let [r (Math/floor (Math/sqrt k))]
                (every? (complement zero?) (map #(mod k %) (rest (rest (range (inc r))))))))
            (prime-bef [k] (loop [i (dec k)] (if (prime i) i (recur (dec i)))))
            (prime-after [k] (loop [i (inc k)] (if (prime i) i (recur (inc i)))))]
      (and (> n 2) (prime n) (= n (/ (+ (prime-bef n) (prime-after n)) 2)))
      )))

(defcheck solution-b0953147
  (fn [n]
    (contains?
      (set (vector 5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103))
      n)))

(defcheck solution-b2b3ed5f
  (let [R range r (R 2 1200)
        g (set (map second
                 (filter (fn [[a b c]]
                           (= (- b a) (- c b)))
                   (partition 3 1
                     (take 300 (remove
                                 (set
                                   (for [x r
                                         y (R (* 2 x) 1200 x)]
                                     y)) r))))))]
    #(contains? g %)))

(defcheck solution-b32c2dfd
  (fn [n]
    (let [prime? (fn [n]
                   (if (< n 2)
                     false
                     (let [candidates (range 2 n)]
                       (not (some identity
                              (map #(zero? (mod n %)) candidates))))))]
      (and
       (>= n 5)
       (prime? n)
       (let [prev (first (filter prime? (iterate dec (dec n))))
             next (first (filter prime? (iterate inc (inc n))))]

         (= (+ prev next) (* 2 n)))))))

(defcheck solution-b3472eea
  (fn [n]
    (let [p #(every? (fn [d]
                       (< 0 (rem % d)))
               (range 2 %))
          q #(nth (filter p
                    (iterate % n))
               1)]
      (and (p n)
           (< 2 n)
           (= n
             (/ (+ (q inc)
                   (q dec))
               2))))))

(defcheck solution-b3591893
  (fn sandwich? [n]
    (letfn [(prime? [n]
              (let [limit (dec n)]
                (loop [d 2] (cond
                              (zero? n) false
                              (= 1 n) true
                              (= 2 n) true
                              (> d limit) true
                              (zero? (rem n d)) false
                              :else (recur (inc d))))))]
      (cond
        (not (prime? n)) false
        (= 1 n) false
        (= 2 n) false
        :else
        (let [a (first (filter prime? (range (inc n) 2147483647)))
              b (first (filter prime? (range (dec n) 0 -1)))]
          (= n (/ (+ a b) 2)))))))

(defcheck solution-b3de47cc
  (fn balanced-prime?
    [n]
    (letfn [(prime? [n] (not-any? #(zero? (rem n %))
                          (range 2 n)))]
      (let [prime-before (first (for [x (iterate dec (dec n))
                                      :when (prime? x) :while (>= x 2)]
                                  x))
            prime-after  (first (for [x (iterate inc (inc n))
                                      :when (prime? x)]
                                  x))]
        (and (prime? n)
             prime-before
             (= n (/ (+ prime-before prime-after) 2)))))))

(defcheck solution-b4972fb6
  (fn balanced-prime? [n]
    (if-not (and (> n 2) (not-any? #(zero? (mod n %)) (range 2 n)))
      false
      (let [prime-step (fn prime-step [n step]
                         (loop [n (+ n step)]
                           (if (not-any? #(zero? (mod n %)) (range 2 n))
                             n
                             (recur (+ n step)))))
            before     (prime-step n -1)
            after      (prime-step n 1)]
        (= (/ (+ before after) 2) n)))))

(defcheck solution-b4b1ff6c
  (fn [n]
    (letfn [(p [n]
              (every? #(> (rem n %) 0) (range 2 n)))
            (a [n s]
              (if-let [x (some #(when (p %) %) s)]
                x
                1))]
      (and (> n 2) (p n)
           (= n (/ (+ (a n (range (+ n 1) (* 2 n)))
                      (a n (iterate dec (- n 1)))) 2))))))

(defcheck solution-b4e628fe
  (fn [bp]
    (not (empty?

           (filter #(and (= bp (second %))
                         (= bp (/ (+ (first %) (last %)) 2)))
             (take-while #(<= (second %) bp)
               ((fn grp [l]
                  (cons (take 3 l) (lazy-seq (grp (rest l)))))
                ((fn p [l s]
                   (lazy-seq
                     (if (some (fn [n] (zero? (mod s n))) l)
                       (p l (inc s))
                       (cons s (p (conj l s) (inc s))))))
                 [] 2))))))))

(defcheck solution-b585efe6
  (into {4 false 563 true 1103 true} (zipmap (range 10 24) (repeat true))))

(defcheck solution-b5da3a66
  (fn [n]
    (letfn [(notprime [n]
              (some #(zero? (mod n %)) (range 2 (inc (/ n 2)))))
            (nextprime [f n]
              (first (drop-while notprime (iterate f (f n)))))]
      (cond (<= n 2) false
            (notprime n) false
            :else (= (/ (+ (nextprime dec n) (nextprime inc n))
                       2)
                    n)))))

(defcheck solution-b607574a
  (fn me [n]

    (let [check-prime (fn [n]

                        (if (<= n 2)
                          false


                          (= false

                            (->> (range 2 n)
                              (map #(zero? (rem n %)))
                              (reduce #(or %1 %2))
                              )
                            ))
                        )

          next-prime  (fn [n]

                        (if (= n 2)
                          2

                          (if (check-prime n)
                            n
                            (recur (inc n))

                            )
                          )

                        )

          pre-prime   (fn [n]


                        (if (= n 2)
                          2

                          (if (check-prime n)
                            n
                            (recur (dec n))

                            )
                          )
                        )

          ]

      (if (check-prime n)
        (= (* n 2)

          (+ (next-prime (inc n)) (pre-prime (dec n)))

          )
        false

        )

      )))

(defcheck solution-b62ecb4e
  (fn bal? [n]
    (let [primes-to (fn [n]
                      (loop [i    2
                             cand (set (range 2 (inc n)))]
                        ;(prn i (sort cand))
                        (if (< n i)
                          (sort cand)
                          (if (cand n)
                            (if (cand i)
                              (recur (inc i) (clojure.set/difference cand (rest (range i (inc n) i))))
                              (recur (inc i) cand))
                            nil))))
          nx        (fn [prev start] (first (filter #(not-any? zero? (map (partial mod %) prev)) (iterate inc start))))
          ;_ (prn ps&)
          ps&       (primes-to n)
          ps        (last ps&)
          ps-1      (last (drop-last ps&))]
      (if (or (nil? ps-1) (not= ps n))
        false
        (let [ps+1 (nx ps& (inc n))]
          ;(prn ps-1 ps ps+1)
          (= n ps (/ (+ ps-1 ps+1) 2)))))))

(defcheck solution-b67b74a4
  (fn [n]
    (if (< n 5) false
                (let [c (fn [v]
                          (not-any? #(zero? (mod v %)) (range 2 v)))
                      p (fn [f]
                          (loop [v (f n)]
                            (if (c v) v
                                      (recur (f v)))))]
                  (and (c n) (= (+ (p dec) (p inc)) (+ n n)))))))

(defcheck solution-b70e5d4f
  (fn [x]
    (letfn [(isprime [n]
              (every? #(not= 0 (mod n %)) (range 2 n)))]
      (if (or (< x 5) (not (isprime x)))
        false
        (let [lower (loop [n (dec x)] (if (isprime n) n (recur (dec n))))
              upper (loop [n (inc x)] (if (isprime n) n (recur (inc n))))]
          (and (= (/ (+ lower upper) 2) x)))))))

(defcheck solution-b75cb328
  (fn [n]
    (letfn [(prime? [n] (and (> n 1) (->> (range 2 (inc (quot n 2)))
                                       (not-any? #(zero? (mod n %))))))
            (find-prime [n step] (->> (+ step n)
                                   (iterate (partial + step))
                                   (filter prime?)
                                   (first)
                                   ((fnil + step n) step)))]
      (and
       (> n 2)
       (prime? n)
       (= n (/ (+ (find-prime n 1) (find-prime n -1)) 2))))))

(defcheck solution-b81d8eb0
  (fn perfp [n]
    (letfn [(prime? [x]
              (and (> x 1)
                   (empty?
                     (filter #(= 0 (rem x %)) (range 2 x)))))]
      (and (> n 2) (prime? n)
           (= (- (first (filter prime? (iterate inc (inc n)))) n)
             (- n (first (filter prime? (iterate dec (dec n))))))))))

(defcheck solution-b835b4d4
  (fn [n]
    (if (< n 3)
      false
      (letfn [
              (prime? [v] (reduce #(and % (not (zero? (mod v %2)))) true (range 2 (+ (/ v 2) 1))))
              (findfirst [p c] (first (filter p c)))
              (nextPrime [c] (findfirst prime? c))]
        (and (prime? n)
             (= (/ (+ (nextPrime (reverse (range 0 n))) (nextPrime (drop (inc n) (range)))) 2) n))))))

(defcheck solution-b8d8b5b5
  (fn balanced-prime? [n]
    (letfn [
            (prime-before? [number a]
              (if (< a 2)
                true
                (and (not (= 0 (rem number a)))
                     (prime-before? number (dec a))))
              )
            (prime? [number]
              (cond
                (= number 2) true
                (= 0 (rem number 2)) false
                (prime-before? number (int (/ number 2))) true
                :else false)
              )
            (prime-after [number]
              (if (prime? (inc number))
                (inc number)
                (prime-after (inc number))))
            (prime-before [number]
              (if (prime? (dec number))
                (dec number)
                (prime-before (dec number))))
            ]
      (and (> n 2) (prime? n) (= (* 2 n) (+ (prime-before n)
                                            (prime-after n))))
      )
    ))

(defcheck solution-b905f98e
  #(letfn [(c [p]
             (and (< 1 p) (= []
                            (filter
                              (fn [x] (= 0 (rem p x)))
                              (range 2 p)))))]

     (if (c %)
       ((fn j [p d]
          (let [x (c (- p d))
                y (c (+ p d))]
            (if (and x y)
              true
              (if (or x y)
                false
                (j p (+ 1 d)))
              ))) % 1)
       false
       )))

(defcheck solution-b93fb108
  (fn [x]
    ((comp boolean some) #{x} [5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103 1123 1187 1223 1367 1511 1747 1753 1907 2287 2417 2677 2903 2963 3307 3313 3637 3733 4013 4409 4457 4597 4657 4691 4993 5107 5113 5303 5387 5393 5563 5807 6073 6263 6317 6323 6367 6373 6863 6977 7523 7583 7823 7841]
     #_(map #(second %)
         (filter (partial apply #(= %2 (/ (+ %1 %3) 2)))
           (partition 3 1
             (take 186 (filter #(.isProbablePrime (biginteger %) 7) (iterate inc 1))))))

     )))

(defcheck solution-ba6c0f1d
  (fn f [n]
    (let [prime? (fn [n] (when (> n 1) (not (some integer? (map #(/ n %) (range 2 n))))))
          lprime (fn [n]
                   (cond (= n 1) nil
                         (prime? n) n
                         :else (recur (dec n))))
          rprime (fn [n]
                   (if (prime? n)
                     n
                     (recur (inc n))))]
      (if (prime? n)
        (if-let [lp (lprime (dec n))]
          (= n (/ (+ lp (rprime (inc n))) 2))
          false)
        false))))

(defcheck solution-bc1b23
  (fn [n]
    (if (<= n 2)
      false
      (let
       [p?   (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))
        fstp (fn [s] (first (filter p? s)))
        lp   (fstp (iterate dec (dec n)))
        up   (fstp (iterate inc (inc n)))]
        (and (p? n) lp up (= n (/ (+ lp up) 2)))))))

(defcheck solution-bc260f72
  (fn iffbp [n]
    (letfn [(isprime [n]
              (cond
                (= n 1) false
                (= n 2) true
                (= n 3) true
                (zero? (rem n 2)) false
                (zero? (rem n 3)) false
                :else (empty? (filter #(zero? (rem n %)) (range 2 (dec n)))))
              )]
      (cond (or (false? (isprime n)) (< n 3)) false
            :else (let [primes (filter isprime (range))
                        triple (first (filter #(= (second %) n) (partition 3 1 primes)))
                        ]
                    (= (/ (+ (last triple) (first triple)) 2) n)
                    ))
      )))

(defcheck solution-bcd88200
  (fn primesandwich [n]
    (let
     [
      isPrime (fn [n]
                (if
                 (< n 2)
                  false
                  (not
                    (reduce
                      #(or %1 %2)
                      false
                      (map
                        (fn [x] (= (float (/ n x)) (float (int (/ n x)))))
                        (range 2 (+ 1 (int (Math/sqrt n))))
                        )
                      )
                    )
                  )
                )

      xor     (fn [a b] (or (and a (not b)) (and (not a) b)))
      ]
      (if
       (not (isPrime n))
        false
        (let
         [
          recursor
          (fn
            recurs
            [a b]
            (let
             [aIsPrime (isPrime a), bIsPrime (isPrime b)]
              (if
               (and aIsPrime bIsPrime)
                true
                (if
                 (xor aIsPrime bIsPrime)
                  false
                  (recurs (inc a) (dec b))
                  )
                )
              )
            )
          ]
          (recursor (inc n) (dec n))
          )
        )
      )
    ))

(defcheck solution-bcdf3810
  (fn [p]
    (letfn [(prime? [x]
              (and (> x 1) (every? #(pos? (mod x %)) (range 2 x))))
            (last-prime [x]
              (last (filter prime? (range 2 x))))
            (next-prime [x]
              (first (filter prime? (iterate inc (inc x)))))]
      (and
       (> p 2)
       (prime? p)
       (= (+ p p) (+ (last-prime p) (next-prime p)))))))

(defcheck solution-bce3edd8
  (fn [n]
    (letfn [(prime?
              ;; Returns true if n is prime. Does this by seeing if there is any divisor
              ;; other than 1 in the numbers up to (sqrt n).
              [x]
              (cond (< x 2) false
                    (== x 2) true
                    (even? x) false
                    :else (let [max-divisor-check (/ (- (int (Math/sqrt x)) 2) 2)] ; subtract 2 because we start iterating at 3
                            (not-any? #(zero? (rem x %))
                              (take max-divisor-check (iterate #(+ % 2) 3))))))
            (prev-prime [n] (first (drop-while #(not (prime? %)) (iterate dec (dec n)))))
            (next-prime [n] (first (drop-while #(not (prime? %)) (iterate inc (inc n)))))]
      (cond (not (prime? n)) false
            (<= n 2) false
            (= n (/ (+ (prev-prime n) (next-prime n)) 2)) true
            :else false))))

(defcheck solution-bd913e8c
  (fn is-balanced?
    [n]
    (let [is-prime         (fn [n]
                             (loop [current 2]
                               (cond
                                 (> current (unchecked-divide-int n 2)) true
                                 (zero? (rem n current)) false
                                 :else (recur (inc current)))))
          wrap-primes-1110 (partition 3 1 (filter is-prime (range 2 1110)))
          found            (first (filter #(= n (second %)) wrap-primes-1110))]
      (cond
        (nil? found) false
        :else (= n (-> (first found) (+ (last found)) (/ 2)))))))

(defcheck solution-bdc97afe
  (fn [x]
    (letfn [(prime? [q]
              (empty? (filter #(zero? (mod q %)) (range (dec q) 1 -1))))
            (next-prime [w]
              (loop [a (inc w)] (if (prime? a) a (recur (inc a)))))]
      (let [previous (- (* 2 x) (next-prime x))]
        (if (prime? previous) (if-not (contains? #{0 1 2 3} x) (= x (next-prime previous)) false) false)
        ))))

(defcheck solution-be00daa3
  (fn prime-balance [n]
    (letfn [(prime? [n]
              (if (or (< n 2) (and (not= 2 n) (even? n)))
                false
                (if (#{2 3 5 7} n) true
                                   (every? #(not= 0 (rem n %))
                                     (range 2 (inc (int (Math/sqrt n))))))))]
      (if (or (#{2 3} n) (not (prime? n))) false
                                           (letfn [(next-prime [n df]
                                                     (loop [n (df n 2)] (if (prime? n) n
                                                                                       (recur (df n 2)))))]
                                             (let [a (next-prime n -), b (next-prime n +)]
                                               (if (= (+ a b) (+ n n)) true false)))))))

(defcheck solution-be707516
  (fn [m]
    (let [p? (fn [n]
               (empty? (filter #(= 0 (mod n %)) (range 2 (inc (Math/sqrt n))))))
          l  (first (drop-while #(not (p? %)) (range (dec m) 1 -1)))
          u  (first (drop-while #(not (p? %)) (map #(+ (inc m) %) (range))))
          ] (if (and (p? m) l u) (= m (/ (+ l u) 2)) false))))

(defcheck solution-bee7fbda
  (fn [n]
    (let [prime? (memoize #(empty? (filter integer? (map / (repeat %) (range 2 %)))))
          findp  (fn [f x] (first (filter prime? (iterate f (f x)))))
          mean   (fn [x y] (/ (+ x y) 2))]
      (if (and (> n 2) (prime? n))
        (= n (mean (findp inc n) (findp dec n)))
        false)
      )))

(defcheck solution-c045d1cb
  (fn balanced-prime? [p]
    (letfn [(prime? [p]
              (and (> p 1)
                   (not-any? zero? (map #(mod p %) (range 2 p)))))]
      (and (prime? p)
           (let [prev-p (first (filter prime? (range (dec p) 1 -1)))
                 next-p (first (filter prime? (range (inc p) 2147483647)))]
             (and prev-p
                  (= (+ next-p prev-p) (* 2 p))))))))

(defcheck solution-c08ad682
  (fn [target]
    (let [simple-prime (fn [n]
                         (and (< 2 n)
                              (->> (range 2 n)
                                (every? #(< 0 (mod n %))))))]
      (and (simple-prime target)
           (let [primes         (filter simple-prime (range))
                 [before after] (split-with #(< % target) primes)
                 previous-prime (last before)
                 next-prime     (second after)]
             (and
              previous-prime
              next-prime
              (simple-prime target)
              (= (- target previous-prime)
                (- next-prime target))))))))

(defcheck solution-c12f3a4
  (fn [x] (let [p (fn [n] (= (count (drop-while #(not= (mod n %) 0) (range 2 (inc n)))) 1)) g (fn f [y d] (if (= 1 y) 0 (if (p y) y (f (d y) d))))] (if (p x) (= (* x 2) (+ (g (inc x) inc) (g (dec x) dec))) false))))

(defcheck solution-c154bb26
  (fn [n]
    (let [primes     (remove (fn [k] (some #(zero? (rem k %))
                                       (range 2 k)))
                       (iterate inc 2))
          primes'    (concat                                ;stolen from clojure.contrib.lazy-seqs/primes
                      [2 3 5 7]
                      (lazy-seq
                        (let [primes-from
                                    (fn primes-from [n [f & r]]
                                      (if (some #(zero? (rem n %))
                                            (take-while #(<= (* % %) n) primes))
                                        (recur (+ n f) r)
                                        (lazy-seq (cons n (primes-from (+ n f) r)))))
                              wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2
                                            6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6
                                            2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
                          (primes-from 11 wheel))))
          bal-primes (map second
                       (filter #(= (apply + %) (* 3 (second %)))
                         (partition 3 1 primes')))
          ]
      (= n (first (drop-while #(< % n) bal-primes)))
      )))

(defcheck solution-c159c0e9
  ((fn [primes sorted-index]
     (fn [n]
       (let [index (sorted-index primes n)]
         (if (or (= index -1) (= n 2)) false
                                       (let [prevp (nth primes (dec index))
                                             nextp (nth primes (inc index))]
                                         (= n (/ (+ prevp nextp) 2)))))))

   (remove (fn [x] (some #(zero? (mod x %)) (range 2 (dec x)))) (iterate inc 2))

   (fn sorted-index [coll v]
     (letfn [(go [[h & t] n]
               (cond
                 (= h v) n
                 (> h v) -1
                 :else (go t (inc n))))]
       (go coll 0)))))

(defcheck solution-c1937bf0
  (fn balanced-prime [n]
    (letfn [(is-prime? [x]
              (cond
                (= x 0) false
                (= x 1) false
                (= x 2) true
                (even? x) false
                :else (reduce #(and %1 (not= (mod x %2) 0))
                        true
                        (range 3 (/ (inc x) 2)))))]
      (if (and (is-prime? n) (> n 3))
        (let [pre-prime (last (filter is-prime? (range 3 n)))
              post-num  (- (* n 2) pre-prime)]
          (and (is-prime? post-num)
               (= (count (filter is-prime? (range (inc n) post-num))) 0)))
        false))))

(defcheck solution-c1fc459c
  (fn balanced-prime? [n]
    (letfn [(prime? [n]
              (every? #(> (mod n %) 0) (range 2 n)))
            (next-prime [f n]
              (first (filter prime? (iterate f (f n)))))]
      (and (> n 2)
           (prime? n)
           (== n (/ (+ (next-prime inc n) (next-prime dec n)) 2))))))

(defcheck solution-c241924e
  (let [mp
        (set (let [primes (remove
                            #(some (fn [x] (zero? (rem % x))) (range 2 %))
                            (range 2 1200))]
               (map second
                 (filter #(= (second %)
                            (/ (+ (first %) (last %)) 2))
                   (partition 3 1 primes)))))]
    (fn [x]
      (if (mp x) true false))))

(defcheck solution-c2454fd3
  (fn balanced [x]
    (let [is-prime     (fn [n]
                         (cond (<= n 1) false
                               :else (every? false? (map #(= 0 (rem n %1)) (range 2 n)))))
          is-prime?    (memoize is-prime)
          before-prime (fn [x]
                         (loop [x (dec x)] (cond (<= x 0) 0 (is-prime? x) x :else (recur (dec x)))))
          after-prime  (fn [x]
                         (loop [x (inc x)] (cond (= x 2147483647) 0 (is-prime? x) x :else (recur (inc x)))))
          pval         (/ (+ (after-prime x) (before-prime x)) 2)]
      (and (is-prime? x) (= pval x))
      )
    ))

(defcheck solution-c2959291
  (fn balanced-prime? [n]
    (let [prime? (fn [x]
                   (if (or (= x 1) (= x 2))
                     true
                     (not-any? #(zero? (mod x %)) (range 2 x))))
          primes (filter prime? (drop 1 (range)))]
      (boolean
        (when (and (>= n 5)
                   (prime? n))
          (let [pp (last (take-while #(< % n) primes))
                np (first (drop-while #(<= % n) primes))]
            (= n (/ (+ pp np) 2))))))))

(defcheck solution-c29fc669
  (fn [n]
    (let [prime?     (fn [n]
                       (loop [i 2] (if (>= i n)
                                     true
                                     (if (zero? (mod n i)) false (recur (inc i))))))
          next-prime (fn [n] (let [nn (inc n)] (if (prime? nn) nn (recur nn))))
          prev-prime (fn [n] (let [nn (dec n)] (if (prime? nn) nn (recur nn))))]
      (and (> n 2)
           (prime? n)
           (= (prev-prime n) (- n (- (next-prime n) n)))))))

(defcheck solution-c325cb3e
  (letfn [(pseq [n] (let [s (set (range 2 (inc n)))]
                      (loop [p 2 s s]
                        (let [s (apply disj s (take-while #(<= % n)
                                                (map #(* p %) (iterate inc 2))))]
                          (if-let [p (first (drop-while #(<= % p) (sort s)))]
                            (recur p s)
                            (vec (sort s)))))))]
    (let [ps (atom [2 3])]
      (fn [n]
        (if (<= n 3)
          false
          (let [ps (if (and (seq @ps) (<= n (first (take-last 2 @ps))))
                     @ps
                     (reset! ps (pseq (* 2 n))))
                [a b] (split-with #(<= % n) ps)
                [a p] (take-last 2 a)]
            (= n p (/ (+ a (first b)) 2))))))))

(defcheck solution-c35632ba
  (fn [x]
    (let [prime? #(if (not-any? (fn [y] (zero? (mod % y))) (range 2 %)) % false)]
      (if (and (> x 2) (prime? x))
        (let [prev (->> (range x) reverse (some prime?))
              next (->> (range) (drop (inc x)) (some prime?))]
          (= (* 2 x) (+ prev next)))
        false))))

(defcheck solution-c4009462
  (fn [n]
    (letfn [(is_prime [n]
              (if (= n 2)
                true
                (every? false? (map #(= (mod n %) 0) (range 2 n)))))
            (sib_prime [n fun] (first (filter is_prime (iterate fun (fun n)))))]
      (cond
        (< n 5) false
        (not (is_prime n)) false
        :else (= n (/ (+ (sib_prime n inc) (sib_prime n dec)) 2))))))

(defcheck solution-c404381f
  (fn [n]
    (if (< n 5) false
                (letfn [(prime? ([n] (if (< n 2) false (prime? n (- n 1)))) ([n m] (if (< m 2) true (if (zero? (rem n m)) false (prime? n (- m 1))))))]
                  (if-not (prime? n) false
                                     (if (= n (/ (+ (first (filter prime? (iterate inc (inc n)))) (first (filter prime? (iterate dec (dec n))))) 2)) true false))))))

(defcheck solution-c4300b0
  ;testing numbers for being primes is quite costly. So we should select the numbers to test for being balanced primes carefully:
  ;first test the number itself for being prime.
  ;then search for the prime before it.
  ;The given number is mean of all numbers with the same distance to it. So after finding last-prime the candidate for next-prime is
  ;the higher number with the same distance.
  ;The number itself is prime, so it's odd and so are the numbers before and after it that may be primes. (The only even prime is 2 but
  ;since (not= 3 (/ (+ 2 5) 2)) we can simply ignore this case). So we may test the numbers lower than nmb in steps of 2 which again reduces the
  ;number of prime tests.
  ;Eventually, we test the numbers between nmb and next-prime for being primes. If there is one, next-prime is not the next prime after nmb and
  ;the result is false.
  ;To reduce the effort even further we use a lookup table for the first 16 known balanced primes. Our algorithm fails for n <= 5 anyway.
  ;
  ;(defn prime? [nmb]
  ;      (empty? (for [n (range 2 nmb) :while(<= n (int (/ nmb n))) :when(= (mod nmb n) 0)] n))
  ;)
  ;
  ;(fn [nmb]
  ;  (if (not-every? (fn[x] (not= x nmb)) [5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103])
  ;    true
  ;    (if (< nmb 1105)
  ;      false
  ;      (if (prime? nmb)
  ;        (let [lastn (first (for [n (range (- nmb 2) 3 -2) :when(prime? n)] n))
  ;              nextn (first (for [n (range (+ nmb 2) (+ 1 nmb (- nmb lastn)) 2) :when(prime? n)] n))]
  ;          (= nextn (+ nmb (- nmb lastn)))
  ;        )
  ;        false
  ;      )
  ;    )
  ;  )
  ;)
  ;

  (fn prime-sandwich [number]
    (let [prime?
          (fn [nmb]
            (empty? (for [n (range 2 nmb) :while (<= n (int (/ nmb n))) :when (= (mod nmb n) 0)] n))
            )
          ]

      (if (not-every? (fn [x] (not= x number)) [5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103])
        true
        (if (<= number 1105)
          false
          (if (prime? number)
            (let [lastn (first (for [n (range (- number 2) 3 -2) :when (prime? n)] n))
                  nextn (first (for [n (range (+ number 2) (+ 1 number (- number lastn)) 2) :when (prime? n)] n))]
              (= nextn (+ number (- number lastn)))
              )
            false
            )
          )
        )
      )
    ))

(defcheck solution-c45aa9ee
  (letfn
   [(prime? [n]
      (not (some #(= 0 (rem n %)) (range 2 n))))]

    (fn [n]
      (and
       (> n 2)
       (prime? n)
       (= n
         (/ (+
             (first (filter prime? (iterate dec (dec n))))
             (first (filter prime? (iterate inc (inc n)))))
           2))))))

(defcheck solution-c473500d
  (fn [n]
    (letfn [(primes-seq []
              (letfn [(insert-into-queue [m x p]
                        (if (m x)
                          (recur m (+ x p p) p)
                          (assoc! m x p)))
                      (next-primes [m x]
                        (if-let [p (m x)]
                          (recur (insert-into-queue (dissoc! m x) (+ x p p) p)
                            (+ x 2))
                          (cons x (lazy-seq (next-primes (insert-into-queue m (* x x) x)
                                              (+ x 2))))))]
                (cons 2 (lazy-seq (next-primes (transient {}) 3)))))]
      (let [primes (primes-seq)
            pre    (last (take-while (partial > n) primes))
            p      (first (filter (partial <= n) primes))
            nxt    (first (filter (partial < n) primes))]
        (and (not (nil? pre))
             (= p n)
             (= (* 2 p) (+ pre nxt)))))))

(defcheck solution-c48d0de9
  (fn [i]
    (let [p (fn [p] (and (> p 1)
                         (every? true?
                           (map #(not= 0 (mod p %)) (range 2 p)))))
          a (fn [n o]
              (if (pos? n)
                (if (p (o n)) (o n) (recur (o n) o))
                n))
          n (fn [n] (a n inc))
          r (fn [n] (a n dec))]
      (and
       (p i)
       (= i (/ (+ (n i) (r i)) 2))))))

(defcheck solution-c4e4d9f9
  (fn [n]
    (let [prime?  (fn [k]
                    (if (<= k 1)
                      false
                      (loop [i 2]
                        (if (>= i k)
                          true
                          (if (zero? (mod k i))
                            false
                            (recur (inc i)))))))
          p-prime (fn [k]
                    (if (= k 2)
                      0
                      (loop [i (dec k)]
                        (if (prime? i)
                          i
                          (recur (dec i))))))
          n-prime (fn [k]
                    (loop [i (inc k)]
                      (if (prime? i)
                        i
                        (recur (inc i)))))]
      (and (prime? n)
           (= n (/ (+ (p-prime n) (n-prime n)) 2))))))

(defcheck solution-c75fd1f
  #(cond
     (= % 4) false
     (= % 563) true
     (> % 1088) true))

(defcheck solution-c76cc240
  (fn [n] (
            if (< n 5) false
                       (let [
                             f (fn [x] (
                                         = 0 (reduce + (map (fn [k] (if (= (/ x k) (int (/ x k))) k 0)) (range 2 x)))
                                         ))
                             ] (
                                 if (f n)
                                 (
                                   loop [a (- n 2) b (+ n 2)] (
                                                                if (and (f a) (f b))
                                                                true
                                                                (if (or (f a) (f b))
                                                                  false
                                                                  (recur (- a 2) (+ b 2))
                                                                  )
                                                                )
                                                              )
                                 false
                                 )))))

(defcheck solution-c8afb721
  (fn [n]
    (letfn [(prime? [n]
              (and (> n 1)
                   (not (some
                          #(zero? (rem n %))
                          (range 2 (-> n Math/sqrt Math/floor inc))))))]
      (if (prime? n)
        (loop [i 1]
          (cond
            (= i (dec n)) false
            (prime? (- n i)) (prime? (+ n i))
            (prime? (+ n i)) (prime? (- n i))
            :else (recur (inc i))))
        false))))

(defcheck solution-c994e290
  (fn [n]
    (letfn [(? [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (and (> n 4)
           (? n)
           (= n (/ (+ (first (filter ? (iterate dec (dec n))))
                      (first (filter ? (iterate inc (inc n)))))
                  2))))))

(defcheck solution-cbef0b75
  (fn [n]
    (loop [b (dec n) a (inc n) i 2]
      (cond
        (<= b 1) false
        (>= (* i i) (* 2 n)) (= (+ b a) (* 2 n))
        (= (mod n i) 0) false
        (and (= (mod b i) 0) (< i b)) (recur (dec b) a 2)
        (= (mod a i) 0) (recur b (inc a) 2)
        :else (recur b a (inc i))))))

(defcheck solution-cc14797b
  (fn balancedprime? [n]
    (if (<= n 3)
      false
      (loop [init (vec (rest (rest (take n (range))))) fut (drop (+ n 1) (range))]
        (if (empty? (rest init))
          (= n (/ (+ (peek init) (first fut)) 2))
          (if (= 0 (mod n (first init)))
            false
            (recur (vec (remove #(= 0 (mod % (first init))) init))
              (remove #(= 0 (mod % (first init))) fut))))))))

(defcheck solution-cc442f42
  (fn [n]
    (letfn [(p? [n] (not (some #(zero? (mod n %)) (range 2 n))))
            (p [f n] (if (p? n) n (p f (f n))))]
      (and (> n 4)
           (p? n)
           (= n (/ (+ (p dec (dec n)) (p inc (inc n))) 2))))))

(defcheck solution-ccc8584f
  (fn aa [n]
    (letfn [(prime? [x]
              (and (number? x)
                   (> x 1)
                   (or (= x 2)
                       (not (some #(zero? (mod x %))
                              (cons 2 (range 3 (inc (Math/sqrt x)) 2)))))))]
      (and (prime? n)
           (let [a (first (filter prime? (iterate inc (inc n))))
                 b (first (filter prime? (range (dec n) 1 -1)))]
             (and (number? a)
                  (number? b)
                  (= (* 2 n) (+ a b))))))))

(defcheck solution-ccfef25e
  (fn [n]
    (let [is-prime?
                      (fn [k]
                        (and (> k 1)
                             (every? #(not= (rem k %) 0)
                               (drop 2 (take-while #(<= (* % %) k) (range))))))
          first-prime #(first (filter is-prime? %))]
      (and (is-prime? n) (let [n1 (first-prime (drop (inc n) (range)))
                               n2 (first-prime (range (dec n) 1 -1))]
                           (and n1 n2 (= (* n 2) (+ n1 n2))))))))

(defcheck solution-cd90f48f
  (fn [n] (let [is-prime? (fn [p] (loop [trial 2]
                                    (if (> (* trial trial) p) true
                                                              (if (= 0 (rem p trial)) false
                                                                                      (recur (inc trial))
                                                                                      )
                                                              )
                                    )
                            )
                ] (if (not (is-prime? n)) false
                                          (loop [left (dec n) right (inc n)]
                                            (if (< left 2) false
                                                           (let [l (is-prime? left) r (is-prime? right)]
                                                             (cond
                                                               (and l r) true
                                                               (or (and l (not r)) (and (not l) r)) false
                                                               :else (recur (dec left) (inc right))
                                                               )
                                                             )
                                                           )
                                            )
                                          )
                  )
    ))

(defcheck solution-ce67da65
  ((fn []
     (let [e first
           r range
           f #(e (filter % %2))
           a (for [x (drop 2 (r)) :when (every? #(not= (mod x %) 0) (r 2 x))] x)
           p (fn [n] (= (e (drop-while #(< % n) a)) n))]
       #(let [q (f p (r (- % 2) 1 -2))]
          (boolean
            (and
             (p %)
             q
             (if-let [s (f p (r (+ % 2) (+ 1 % (- % q)) 2))]
               (= (- % q) (- s %))))))))))

(defcheck solution-cea20d85
  (letfn [
          (is-prime [n] (every? #(< 0 (mod n %)) (take-while #(<= (* % %) n) (drop 2 (range)))))
          (next-prime [n] (first (filter is-prime (range (inc n) (* 10 n)))))
          (prev-prime [n] (first (filter is-prime (reverse (range 0 n)))))]
    (fn bp [n]
      (if (< n 3) false
                  (and (is-prime n)
                       (= (* n 2) (+ (prev-prime n) (next-prime n))))))))

(defcheck solution-ceb16687
  (letfn [

          (prime? [n]
            (if (zero? (mod n 2)) false
                                  (empty? (filter #(zero? (mod n %))
                                            (range 3 (inc (Math/sqrt n)) 2
                                              )))))]

    (fn [n]
      (if (not (prime? n)) false
                           (let [before (first (filter prime? (range (dec n) 1 -1)))
                                 after  (first (filter prime? (iterate inc (inc n))))]

                             (and before (->
                                           (+ before after)
                                           (/ 2)
                                           (= n))))))))

(defcheck solution-ceb34586
  (fn [n]
    (letfn [(primes []
              (letfn [(reinsert [table q prime]
                        (loop [x (+ q (* 2 prime))]
                          (if (table x)
                            (recur (+ x (* 2 prime)))
                            (assoc! (dissoc! table q) x prime))))
                      (step [table d]
                        (if-let [prime (get table d)]
                          (recur (reinsert table d prime) (+ 2 d))
                          (lazy-seq (cons d (step (assoc! table (* d d) d)
                                              (+ 2 d))))))]
                (cons 2 (step (transient {}) 3))))]
      (let [p3 (first (filter #(<= n (second %)) (partition 3 1 (primes))))]
        (and (= n (second p3))
             (= n (/ (reduce + p3) 3)))))))

(defcheck solution-cee2db50
  (fn [n]
    (letfn [(p [n] (and (> n 1) (every? #(not= (mod n %) 0) (range 2 n))))
            (b? [d] (let [a (p (- n d)) b (p (+ n d))]
                      (or (and a b)
                          (and (not (or a b))
                               (b? (inc d))))))]
      (and (p n) (b? 1)))))

(defcheck solution-cf38e71c
  (let [
        prime? (fn [n]
                 (and (> n 1)
                      (let [test-d (range 2 (+ 0.1 (Math/sqrt n)))]
                        (every? #(not= % 0) (map #(mod n %) test-d)))))
        primes (filter prime? (range))
        balanced-primes
               (filter #(not (nil? %))
                 (map #(if (= (- %3 %2) (- %2 %1)) %2 nil)
                   primes (drop 1 primes) (drop 2 primes)))]
    #(not (nil? (some (partial = %) (take 16 balanced-primes))))))

(defcheck solution-cf874ea8
  #(not (nil?
          (#{5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103} %))))

(defcheck solution-d0d6cd55
  (fn [x]
    (letfn [(is-prime [x]
              (loop [i 2]
                (if (> (* i i) x)
                  true
                  (if (zero? (mod x i))
                    false
                    (recur (inc i))))))
            (next-prime [f x]
              (if (is-prime (f x))
                (f x)
                (recur f (f x))))]
      (let [a (next-prime dec x)
            b (next-prime inc x)]
        (and (> x 2) (is-prime x) (= x (/ (+ a b) 2)))))))

(defcheck solution-d1483b8d
  (fn balanced-prime? [n]
    (letfn [(square [x] (* x x))
            (divides? [a b] (= (mod b a) 0))
            (smallest-factor [n]
              (loop [candidate 2]
                (cond (> (square candidate) n) n
                      (divides? candidate n) candidate
                      :else (recur (inc candidate)))))
            (prime? [n] (and (> n 1) (= n (smallest-factor n))))
            (only-primes [s] (filter prime? s))
            (predecessors [n] (range (dec n) -1 -1))
            (successors [n] (drop (inc n) (range)))
            (mean [a b] (/ (+ a b) 2))]

      (and (prime? n)
           (let [pred (first (only-primes (predecessors n)))
                 succ (first (only-primes (successors n)))]
             (and pred (= n (mean pred succ))))))))

(defcheck solution-d264797
  (fn [n]
    (letfn [(prime? [x]
              (and (>= x 2)
                   (not-any? #(zero? (mod x %)) (range 2 x))))]
      (and (> n 2)
           (prime? n)
           (let [[a b] (map (partial some #(when (prime? %) %)) [(iterate inc (inc n)) (range (dec n) 1 -1)])]
             (== n (/ (+ a b) 2)))))))

(defcheck solution-d2b33444
  (fn [n]
    (letfn [(p [x] (when (and (> x 1) (not-any? #(= 0 (mod x %)) (range 2 x))) x))]
      (if (p n)
        (= (/ (+ (or (some p (range (- n 1) 2 -1)) 0)
                 (some p (iterate inc (+ n 1))))
             2)
          n)
        false))))

(defcheck solution-d2c0ecc3
  (fn [x]
    (letfn [(nextPrime [y] (first (filter prime? (iterate (partial + 2) (+ 2 x)))))
            (prevPrime [z] (first (filter prime? (iterate #(- % 2) (- x 2)))))
            (prime? [w] (case w
                          0 false
                          1 false
                          2 true
                          (let [maxdiv (+ 2 (int (Math/sqrt w)))]
                            (every? #(if (= 1 %) true
                                                 (not= 0 (rem w %)))
                              (range 1 maxdiv)))))]
      (case x
        0 false
        1 false
        2 false
        3 false
        4 false
        5 true
        (and (prime? x)
             (= x
               (/ (+ (nextPrime x) (prevPrime x))
                 2)))))))

(defcheck solution-d57623ad
  (fn [n] (letfn [(prime? [n] (not-any? #(= 0 (mod n %)) (range 2 (+ 1 (Math/sqrt n)))))]
            (and (not (#{0 1 2 3} n)) (prime? n)
                 (let [next_prime (first (drop-while (complement prime?) (iterate inc (inc n))))
                       prev_prime (first (drop-while (complement prime?) (iterate dec (dec n))))]
                   (= (- next_prime n) (- n prev_prime)))))))

(defcheck solution-d5be8a56
  (fn balanced-prime?
    [n]
    (let [prime?    (fn [x] (= (count (filter #(zero? (mod x %)) (range 2 x))) 0))
          pre-prim  (fn inner-pre [x] (if (prime? x) x (inner-pre (dec x))))
          next-prim (fn inner-next [x] (if (prime? x) x (inner-next (inc x))))]
      (and
       (> n 2)
       (prime? n)
       (= n (/ (+ (pre-prim (dec n)) (next-prim (inc n))) 2))))))

(defcheck solution-d64b538b
  (fn [n]
    (let [prime-test (fn [x] (not-any? #(zero? (mod x %)) (range 2 (+ 1 (Math/sqrt x)))))]
      (if (or (> 2 n)
              (not (prime-test n)))
        false
        (loop [prev-found false next-found false current-prev (dec n) current-next (inc n)]
          (cond
            (and prev-found next-found)
            (= (/ (+ prev-found next-found) 2) n)
            (= 2 current-prev)
            false
            (not prev-found)
            (if (prime-test current-prev)
              (recur current-prev next-found current-prev current-next)
              (recur false next-found (dec current-prev) current-next))
            (not next-found)
            (if (prime-test current-next)
              (recur current-prev current-next current-prev current-next)
              (recur current-prev false current-prev (inc current-next)))))))))

(defcheck solution-d6da195e
  (fn bal-prime? [n]
    (letfn [(is-prime? [n] (and (> n 1)
                                (not-any? #(zero? (mod n %)) (range 2 n))))
            (next-prime [n] (first (filter is-prime? (iterate inc (inc n)))))
            (prev-prime [n] (first (filter is-prime? (iterate dec (dec n)))))]
      (and (> n 2)
           (is-prime? n)
           (= n (/ (+ (next-prime n) (prev-prime n)) 2))))))

(defcheck solution-d6e1be1c
  (fn balanced-prime? [n] (let [factors (cons 2 (iterate (partial + 2) 3))
                                prime? (fn [n] (not-any? #(zero? (mod n %)) (take-while #(<= % (inc
                                                                                                 (Math/sqrt n))) factors))) prime-step (fn [n s] (first (drop-while
                                                                                                                                                          (complement prime?) (rest (iterate (partial + s) n)))))] (and (> n 3)
                                                                                                                                                                                                                        (prime? n) (= n (/ (+ (prime-step n 2) (prime-step n -2)) 2))))))

(defcheck solution-d6fe6ad5
  (fn balanced-prime? [x]
    (letfn [(prime? [x]
              (cond (< x 2) false
                    (= x 2) true
                    (every? #(not= (mod x %) 0) (range 2 (int (inc (Math/sqrt x))))) true
                    :else false))
            (next-prime [y d]
              (loop [current (+ y d)]
                (if (or (prime? current) (< current 1)) current (recur (+ current d)))))]
      (if (not (prime? x)) false
                           (let [after  (next-prime x 1)
                                 before (next-prime x -1)]
                             (= x (/ (+ after before) 2)))))))

(defcheck solution-d76cea39
  (let [primes
        (cons 2 ((fn gen-primes [current-primes n]
                   (if
                    (every? (partial < 0) (map (partial mod n) current-primes))
                     (cons n (lazy-seq (gen-primes (cons n current-primes) (+ 2 n))))
                     (lazy-seq (gen-primes current-primes (inc n)))
                     ))
                 '(2) 3))]
    (fn balanced-prime? [x]
      (if (< x 5)
        false
        (let [not-greater (take-while #(<= %1 x) primes)]
          (and
           (= (last not-greater) x)
           (= x
             (/
               (+ (second (reverse not-greater)) (first (drop-while #(<= %1 x) primes))) 2)))
          )
        ))
    ))

(defcheck solution-d78966fb
  (fn b-prime [n]
    (letfn [(prime [n]
              (not-any? #(= 0 (mod n %))
                (range 2 n)))]
      (and (prime n)
           (let [l-prime (first (filter prime
                                  (range (- n 1)
                                    1
                                    -1)))
                 r-prime (first (filter prime
                                  (range (+ n 1)
                                    2147483647)))]
             (if (or (= nil l-prime)
                     (= nil r-prime))
               false
               (= (* n 2)
                 (+ l-prime r-prime))))))))

(defcheck solution-d7e5b821
  (fn [x]
    (letfn [(p [n]
              (and (> n 1)
                   (apply = false
                     (map
                       #(= 0 (mod n %))
                       (range 2 (+ (quot n 2) 1))))
                   )
              )]
      (if (p x)
        (loop [b (- x 1)
               a (+ x 1)]
          (cond
            (and (p b) (p a)) (= x (/ (+ a b) 2))
            (= 0 b) false
            1 (recur (if-not (p b) (- b 1) b)
                (if-not (p a) (+ a 1) a))))
        false))))

(defcheck solution-d8852ded
  (fn [n]
    (letfn [(prime? [n] (every? #(not= (mod n %) 0) (range 2 (Math/sqrt (inc n)))))]
      (if (or (not (prime? n)) (<= n 2))
        false
        (let [a (first (filter prime? (drop (inc n) (range))))
              b (first (filter prime? (range (dec n) 1 -1)))]
          (= (+ a b) (+ n n)))))))

(defcheck solution-d8e9495e
  (fn [p]
    (letfn [(p? [x] (and (> x 1) (not-any? #(zero? (rem x %)) (range 2 x))))]
      (and (> p 3)
           (p? p)
           (= p (/ (+ (first (filter p? (iterate #(+ % 2) (+ p 2))))
                      (first (filter p? (iterate #(- % 2) (- p 2)))))
                  2))))))

(defcheck solution-d91d56e5
  (fn balanced-prime? [c]
    (let
     [prime?         (fn [n] (and (> n 1) (not-any? #(= % 0) (map #(rem n %) (range 2 n)))))
      next-prime     (first (filter prime? (map #(+ % (inc c)) (range))))
      previous-prime (first (filter prime? (range (dec c) 1 -1)))]
      (and (prime? c) (not (nil? previous-prime)) (= (- next-prime c) (- c previous-prime))))))

(defcheck solution-d9f64ff5
  (fn f [n]
    (if (<= n 3) false
                 (let [prime? (fn [n]
                                (loop [i (int 2)]
                                  (cond
                                    (> (* i i) n) true
                                    (zero? (rem n i)) false
                                    :e (recur (inc i)))))]
                   (if (not (prime? n)) false
                                        (let [l (second (filter prime? (iterate dec n)))
                                              r (second (filter prime? (iterate inc n)))]
                                          (== (- n l) (- r n))))))))

(defcheck solution-da93503c
  (fn [num]
    (if (< num 3)
      false
      (let [prime? (fn [num]
                     (every? #(not (zero? %)) (map #(mod num %) (range 2 num))))]
        (if (prime? num)
          (let [prev (first (filter prime? (range (dec num) 1 -1)))
                next (first (filter prime? (iterate inc (inc num))))]
            (= (/ (+ prev next) 2) num))
          false)))))

(defcheck solution-db760871
  #(contains? #{5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103} %))

(defcheck solution-db78c24e
  (let
   [prime-seq-gen
              (fn prime-seq-gen
                ;; Return a seq of primes.  Limit with take, e.g. (take 5 (prime-seq)) as it will consume large amounts of stack for large seqs produced.
                []
                ((fn prime-seq-i [sq]
                   (when (seq sq)
                     (lazy-seq
                       (cons (first sq)
                         (prime-seq-i (remove #(zero? (mod % (first sq))) (rest sq)))))))
                 (cons 2 (iterate #(+ % 2) 3))))
    prime-seq (prime-seq-gen)
    get-near
              (fn [pred s]
                (if (pred (second s))
                  [(first s) (second s) (nth s 2)]
                  (recur pred (rest s))))]

    (fn q4q116 [n]
      (and
       (not (contains? #{0 1 2} n))
       (let [primes (get-near #(>= % n) prime-seq)
             [l-prime n-prime u-prime] primes]
         (and (= n n-prime)
              (= (- n l-prime)
                (- u-prime n))))))))

(defcheck solution-dbcc38b
  (fn [n]
    (let [isPrime (fn [n] (and (>= n 2)
                               (every? #(not= (rem n %1) 0) (range 2 n))))]
      (and (isPrime n) (> n 2)
           (= (* n 2)
             (+ (second (filter isPrime (iterate dec n)))
                (second (filter isPrime (iterate inc n)))))))))

(defcheck solution-dbd7c0c3
  (let
   [primes
    (letfn
     [(step [[i ps]]
        (if (some #(= 0 (mod i %))
              (take-while #(>= i (* % %)) ps))              ;i not prime
          (recur [(inc i) ps])
          [(inc i) (conj ps i)]))]
      (map (comp last second) (iterate step [3 [2]])))]
    (fn [n]
      (loop [ps primes]
        (let [[b p a & rest] ps]
          (cond
            (< n p) false                                   ;passed it, not prime
            (> n p) (recur (next ps))                       ;keep going
            :else (= n (/ (+ a b) 2))))))))

(defcheck solution-dc902cf8
  (fn [x]
    (let [f (fn [n] (and (> n 1) (not (some zero? (map #(mod n %) (range 2 n))))))
          g #(first (filter f %))
          p (g (range (dec x) 0 -1))
          n (g (drop (inc x) (range)))]
      (and (f x) p n (= x (/ (+ p n) 2))))))

(defcheck solution-dc9c7b47
  (fn [n]
    (let [p #(not (some (fn [x] (= 0 (rem % x))) (range 2 %)))]
      (and (> n 3) (p n)
           (= [true true]
             (first (drop-while
                      #(= % [false false])
                      (for [x (rest (range))]
                        [(p (- n x)) (p (+ n x))]))))))))

(defcheck solution-dd0f75ff
  (fn ps? [n]
    (let [p? (fn [x] (empty? (drop-while #(not= (mod x %) 0) (range 2 (inc (Math/sqrt x))))))]
      (and (> n 3)
           (p? n)
           (= n (/ (+ (first (drop-while (complement p?) (iterate dec (dec n))))
                      (first (drop-while (complement p?) (iterate inc (inc n))))) 2))))))

(defcheck solution-dd2c9f27
  (fn [n]
    (let [p (fn [x] (reduce #(and % (not= 0 (mod x %2))) true (range 2 x)))
          w (fn [x f] (first (filter p (iterate f (f x)))))]
      (and (> n 2) (p n) (= n (/ (+ (w n dec) (w n inc)) 2))))))

(defcheck solution-dd5b91c0
  (fn balanced-prime? [n]
    (letfn [(isqrt [x] (-> x Math/sqrt Math/ceil
                         Math/round))
            (prime? [m] (and (>= m 2)
                             (or (= m 2)
                                 (empty? (filter #(= 0 (mod m %))
                                           (drop 2 (range (+ 1 (isqrt m)))))))))
            (next-prime-via [f n]
              (let [nxt (f n)]
                (if (prime? nxt) nxt (recur f nxt))))]
      (and (> n 2)
           (prime? n)
           (= n
             (/ (+ (next-prime-via inc n)
                   (next-prime-via dec n))
               2))))))

(defcheck solution-dd64fe58
  (fn balanced [k]
    (letfn [(prime [n]
              (let [divisions (map #(mod n %) (range 2 (inc (/ n 2))))
                    isprime   (empty? (filter zero? divisions))] (and (> n 2) isprime)))
            (next-prime [n direction]
              (let [offset direction]
                (loop [i offset]
                  #_(println i)
                  (if (prime (+ n i)) (+ n i)
                                      (if (> 0 (+ n i)) n
                                                        (recur (+ direction i)))))))]

      (let [nxt (next-prime k 1)
            prv (next-prime k -1)]
        (and (prime k) (= k (/ (+ nxt prv) 2)) (> prv 0))))))

(defcheck solution-ddef2f96
  (letfn
   [(prime? [n]
      (and (> n 1)
           (not (some #(= (/ n %) (int (/ n %)))
                  (range 2 (inc (/ n 2)))))))
    (prime-offset [n op]
      (->> (range 1 n) (filter #(prime? (op n %))) first))]
    (fn f [n]
      (and (prime? n)
           (= (prime-offset n +)
             (prime-offset n -))))))

(defcheck solution-de2172
  (fn prime-sandwich? [n]
    (letfn [(prime? [n]
              (and
               (> n 1)
               (not-any? #(zero? (mod n %)) (range 2 n))))]
      (and
       (> n 2)
       (prime? n)
       (let [primes        (lazy-seq (filter prime? (range)))
             primes-before (take-while #(<= % n) primes)
             prime-before  (last (butlast primes-before))
             prime-after   (first (drop (count primes-before) primes))]
         (= n (/ (+ prime-before prime-after) 2)))))))

(defcheck solution-de8d37e1
  (fn [n]
    (letfn [(prime? [x]
              (if (> x 1)
                (loop [i 2]
                  (if (> i (/ x 2))
                    true
                    (if (= 0 (rem x i))
                      false
                      (recur (inc i)))))
                false))
            (x-prime [f n]
              (loop [n (f n)]
                (cond
                  (< n 2) nil
                  (prime? n) n
                  :default (recur (f n)))))
            (l-prime [n] (x-prime dec n))
            (r-prime [n] (x-prime inc n))]
      (if (prime? n)
        (let [l (l-prime n)
              r (r-prime n)]
          (and (not (nil? l)) (= (/ (+ l r) 2) n)))
        false))))

(defcheck solution-df2e177d
  (fn [p]
    (letfn [(prime? [x] (and (not (#{0 1} x))
                             (empty? (filter #(= (mod x %) 0)
                                       (range 2 x)))))
            (previous-prime [x] (or (first (filter prime? (reverse (range x)))) 0))
            (next-prime [x] (or (first (filter prime? (iterate inc (inc x)))) 0))]
      (and (prime? p)
           (= p (/ (+ (previous-prime p)
                      (next-prime p))
                  2))))))

(defcheck solution-df6d0971
  (fn [n]
    (let [prime? (fn [n]
                   (every? true?
                     (map
                       (fn [a] (not (zero? (rem n a))))
                       (range 2 n))))
          l      (first (filter prime? (range (dec n) 1 -1)))
          r      (first (filter prime? (iterate inc (inc n))))]
      (and (prime? n) (> n 2)
           (= (/ (+ l r) 2) n)))))

(defcheck solution-dfcb6f57
  (fn [n]
    (letfn [(p [x]
              (if (< x 2) false
                          ((comp not some) #{0} (map #(rem x %) (range 2 x)))))
            (m [x]
              (first (filter p (range (+ x 1) (* 2 x)))))]
      (if (p n)
        (let [b (- (* 2 n) (m n))]
          (and (p b) (= (m b) n)))
        false))))

(defcheck solution-dfe44ed1
  (fn [n]
    (and (> n 3)
         (let [isPrime (fn [x] (every? #(> (mod x %) 0) (range 2 x)))
               before  (first (filter isPrime (reverse (range 2 n))))
               after   (first (filter isPrime (drop (inc n) (range))))]
           (and
            (isPrime n)
            (= (* 2 n) (+ before after)))
           ))))

(defcheck solution-e03650b3
  (fn [n]
    (let [p? (fn [n]
               (->> (range 3 (inc (Math/sqrt n)) 2)
                 (cons 2)
                 (some #(zero? (rem n %)))
                 nil?))]
      (if (and (> n 3) (p? n))
        (let [prv (->> (range (dec n) 0 -1)
                    (filter p?)
                    first)
              nxt (->> (iterate inc (inc n))
                    (filter p?)
                    first)]
          (= n (/ (+ prv nxt) 2)))
        false))))

(defcheck solution-e05e32aa
  (fn prime-balance [cnt]
    (let [prime? (fn [n]
                   (not (some #(zero? (mod n %))
                          (range 2 (dec n)))))
          primes (filter prime? (iterate inc 1))]
      (if (> 3 cnt)
        false
        (and (prime? cnt) (= cnt
                            (/
                              (+
                               (last (take-while #(< % cnt) primes))
                               (second (drop-while #(< % cnt) primes)))
                              2)))
        )
      )
    ))

(defcheck solution-e2113efe
  (fn prime-sandwich
    [n]
    (letfn [(is-prime [n] (cond
                            (= n 1) false
                            (= n 0) false
                            :else (loop [divisor 2]
                                    (if (< (* divisor divisor) (inc n))
                                      (if (zero? (mod n divisor)) false (recur (inc divisor)))
                                      true))))]
      (and (is-prime n)
           (= (/ (+ (some #(if (is-prime %1) %1) (iterate dec (dec n)))
                    (some #(if (is-prime %1) %1) (iterate inc (inc n))))
                2)
             n)
           ))))

(defcheck solution-e3086b7e
  (fn [n]
    (let [is-prime   (fn [n] (and (not= 1 n)
                                  (not-any? #(zero? (rem n %))
                                    (range 2 n))))
          iter-prime (fn [n f]
                       (first (filter is-prime (iterate f (f n)))))]
      (and
       (is-prime n)
       (= (* 2 n) (+ (iter-prime n inc) (iter-prime n dec)))))))

(defcheck solution-e3cf65e0
  (fn [n]
    (letfn [(is-prime [n]

              (cond
                (== n 1) false
                (== n 2) true
                (zero? (rem n 2)) false
                :else (loop [c 3]
                        (cond
                          (< n (* c c)) true
                          (zero? (rem n c)) false
                          :else (recur (+ c 2)))))
              )
            (balanced-prime [n]
              (and
               (is-prime n)
               (loop [d 2]
                 (cond
                   (< n d) false
                   (is-prime (- n d)) (is-prime (+ n d))
                   (is-prime (+ n d)) false
                   :else (recur (+ d 2)))
                 )))]
      (balanced-prime n))))

(defcheck solution-e42bdcf7
  (fn [x]
    (let [primes      #{2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83
                        89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173
                        179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271
                        277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383
                        389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491
                        499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613
                        617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733
                        739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857
                        859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977
                        983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063
                        1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163
                        1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277}
          lower-bound (first (sort (filter #(< x %) primes)))
          upper-bound (if (<= x 2) 2 (last (sort (filter #(> x %) primes))))
          ]
      (and (contains? primes x) (= x (/ (+ upper-bound lower-bound) 2))))))

(defcheck solution-e43bde81
  (let [prime? (fn [n]
                 (and (> n 1) (not-any? #(= (mod n %) 0) (range 2 (inc (int (Math/sqrt n)))))))
        primes (filter prime? (range))]
    (fn [n] (and (prime? n)
                 (let [s1 (take-while #(< % n) primes)
                       s2 (drop-while #(<= % n) primes)]
                   (when-not (or (empty? s1) (empty? s2))
                     (= (+ (last s1) (first s2)) (* 2 n))))))))

(defcheck solution-e43ea7f0
  (fn balanced-prime? [n]
    (letfn
     [(prime? [x]
        (cond
          (<= x 1) false
          (<= x 3) true
          (or (zero? (mod x 2)) (zero? (mod x 3))) false
          :else
          (loop [i 5]
            (if (> (* i i) x)
              true
              (if (or (zero? (mod x i)) (zero? (mod x (+ i 2))))
                false
                (recur (+ i 6)))))))

      (find-prime [x f]
        (let [y (f x)]
          (if (prime? y)
            y
            (find-prime y f))))]

      (cond
        (< n 5) false
        (not (prime? n)) false
        :else
        (let [x (find-prime n inc)
              y (find-prime n dec)]
          (= (/ (+ x y) 2) n))))))

(defcheck solution-e4644ce5
  (letfn [(p [n] (not-any? #(zero? (mod n %)) (range 2 n)))]
    (fn [n]
      (let [x #(first (filter p (iterate % (% n))))
            l (x dec)
            r (x inc)]
        (and (p n) (> n 4) (= (/ (+ l r) 2) n))))))

(defcheck solution-e54ec496
  (fn [x]
    (if (or (<= x 2) (not (not-any? #(zero? (rem x %)) (range 2 x)))) false
                                                                      (letfn [(ps [num] (filter (fn [n] (not-any? #(zero? (mod n %1)) (range 2 n))) (iterate inc num)))]
                                                                        (let [xp (last (take-while #(< % x) (ps 2))) xn (second (ps x))]
                                                                          (= x (/ (+ xp xn) 2)))))))

(defcheck solution-e58412ca
  (fn [n]
    (letfn [(prime? [num]
              (if (#{0 1} num)
                false
                (if (= num 2)
                  true
                  (not (some #(= (mod num %) 0) (range 2 (inc (Math/sqrt num))))))))]
      (if (prime? n)
        (let [b (first (filter prime? (range (dec n) 1 -1)))
              a (if b (first (filter prime? (range (inc n) (+ n b 1)))) nil)]
          (and a b (= n (/ (+ a b) 2))))
        false))))

(defcheck solution-e622d33e
  (fn bp? [n]
    (letfn [(isprim? [n]
              (cond (= 1 n) false
                    (= 2 n) true
                    (even? n) false
                    :else ((fn prim? [_n coll]
                             (cond (empty? coll) true
                                   (zero? (mod _n (first coll))) false
                                   :else (recur _n (rest coll))))
                           n (filter odd? (range 3 (quot n 2))))))

            (search [f n]
              (let [_n (f n)]
                (if (isprim? _n)
                  _n
                  (recur f _n))))

            (beforep [n] (search #(- % 2) n))
            (nextp [n] (search #(+ % 2) n))]

      (if (isprim? n)
        (if (= 2 n)
          false
          (= n (/ (+ (beforep n) (nextp n)) 2)))
        false))))

(defcheck solution-e623ba1f
  (fn [p]
    (let [prime?   (fn [n] (and (> n 1) (not (some #(zero? (mod n %)) (range 2 n)))))
          primes   (filter prime? (range))
          ; Returns a 3-element seq from s centered on the first occurrence of x.
          sandwich (fn [x s]
                     (take 3 (drop (dec (count (take-while (partial not= x) s)))
                               s)))]
      (and (prime? p)
           (let [[a b c] (sandwich p primes)]
             (= b (/ (+ a c) 2)))))))

(defcheck solution-e693538f
  (fn [n]
    (let [isPrime (fn [a] (not-any? #(= (mod a %) 0) (range 2 a)))
          next    (first (filter isPrime (iterate inc (inc n))))]
      (if-let [prev (first (filter isPrime (range (dec n) 2 -1)))]
        (and (isPrime n) (= n (/ (+ prev next) 2)))
        false))))

(defcheck solution-e6a16ca2
  (fn
    [n]
    (let [odds (iterate (partial + 2) 3)]
      (letfn [(divides? [n d] (= 0 (mod n d)))
              (is-prime [n] (cond
                              (< n 2) false
                              (= n 2) true
                              (divides? n 2) false
                              :else (let [divs (take-while #(<= (* % %) n)
                                                 odds)]
                                      (nil? (some (partial divides? n) divs)))))
              (next-prime
                [n]
                (if (= n 2)
                  3
                  (first (for [o (iterate (partial + 2) (+ 2 n))
                               :when (is-prime o)] o))))]

        (if (is-prime n)
          (let [np (next-prime n)
                pp (- n (- np n))]
            (and (is-prime pp)
                 (= n (next-prime pp))))
          false)))))

(defcheck solution-e86eaa57
  (fn [n]
    (and (> n 3)
         (let [prime? (fn [x] (every? #(< 0 (mod x %)) (range 2 x)))
               primes (filter prime? (iterate inc 2))]
           (and (prime? n)
                (let [pre  (last (take-while #(> n %) primes))
                      post (first (drop-while #(>= n %) primes))]
                  (= n (/ (+ pre post) 2))))))))

(defcheck solution-e89fc863
  ((fn [p]
     (fn [b]
       (let [[a c] (map #(first (filter p (rest (iterate % b)))) [dec inc])]
         (and (> b 2)
              (p b)
              (= b (/ (+ a c) 2))))))
   (fn [i] (not-any? #(= 0 (mod i %)) (range 2 i)))))

(defcheck solution-e8c14c03
  (fn [n]
    (let [prime? (fn [x] (or (= x 2)
                             (not-any?
                               #(zero? (mod x %))
                               (range 2 (inc (Math/sqrt x))))))
          next   (first (drop-while (comp not prime?)
                          (drop (inc n) (range))))
          prev   (first (drop-while (comp not prime?)
                          (range (dec n) 0 -1)))]
      (and (> n 2) (prime? n) (= (- n prev) (- next n))))))

(defcheck solution-e8c6be68
  (let [p (fn [x] (every? #(not (integer? (/ x %))) (range 2 x)))
        l (filter p (range 2 ##Inf))]
    (fn [x]
      (and (p x)
           (let [a (last (cons -100 (take-while #(< % x) l)))
                 b (first (drop-while #(<= % x) l))]
             (= x (/ (+ a b) 2)))))))

(defcheck solution-e8ff9480
  (let [prime? (fn [n]
                 (and (< 1 n)
                      (not-any? #(zero? (mod n %))
                        (remove #(= n %) (range 2 (inc (Math/sqrt n)))))))
        primes (filter prime? (range))]
    (fn [n]
      (let [[a b c] (first (drop-while (fn [[_ x _]] (< x n)) (partition 3 1 primes)))]
        (and (= b n) (= b (/ (+ a c) 2)))))))

(defcheck solution-e9238e20
  (fn sand [n]
    (letfn [(prime? [n p] (every? #(pos? (rem n %)) p))
            (next-prime
              ([n p] (if (prime? n p) (conj p n) (recur (+ n 2) p)))
              ([p] (next-prime (+ (peek p) 2) p)))
            (gen-primes [p] (cons (last p) (lazy-seq (gen-primes (next-prime p)))))]
      (and (> n 3)
           (let [p (into [2] (take-while #(<= % n) (gen-primes [2 3])))
                 [r s] (subvec p (- (count p) 2))]
             (and (= s n) (= s (/ (+ r (peek (next-prime p))) 2))))))))

(defcheck solution-ea086590
  ; I had a solution that solved the problem, but it was too slow
  ; (fn [n] (let [p (remove (fn [n] (some #(zero? (mod n %)) (range 2 n))) (iterate inc 2))
  ;               t (flatten (map (fn [[a b c]] (if (= (- b a) (- c b)) [b] [])) (partition 3 1 p)))]
  ;         (= (first (drop-while (partial > n) t)) n))))
  (comp not nil? #{5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103}))

(defcheck solution-eb1e7397
  (fn [n]
    (letfn [

            (getAllnums [n]
              (range 2 n)
              )

            (divides? [a b]
              (zero? (rem b a))
              )

            (thoseDivides [l n]
              (filter #(divides? % n) l)
              )

            (prime? [n]
              (empty? (thoseDivides (getAllnums n) n))
              )


            (nextPrime [n]
              (if (prime? n) n (nextPrime (inc n)))
              )


            (prevPrime [n]
              (if (<= n 2) -1000
                           (if (prime? n) n (prevPrime (dec n)))
                           ))


            (meanFine [n]
              (= n (/ (+ (prevPrime (dec n)) (nextPrime (inc n))) 2))
              )

            ]

      (and (prime? n) (meanFine n))
      )))

(defcheck solution-ebb6c91c
  #(if (#{5 53 157 173 211 257 263 373 563 593 607 653 733 947 977 1103 1123 1187} %) true false))

(defcheck solution-ecf917d6
  (fn balanced-prime [n]
    (letfn [(prime? [n] (empty? (remove #(not= 0 (rem n %)) (range 2 (inc (quot n 2))))))
            (prev-prime [n] (loop [m (dec n)] (if (prime? m) m (recur (dec m)))))
            (next-prime [n] (loop [m (inc n)] (if (prime? m) m (recur (inc m)))))
            ]
      (and (>= n 5) (prime? n) (= n (quot (+ (next-prime n) (prev-prime n)) 2))))))

(defcheck solution-ed195a42
  (fn [n]
    (let [primes (filter (fn [n]
                           (if (< n 2)
                             false
                             (loop [i 2]
                               (cond (> (* i i) n) true
                                     (= (rem n i) 0) false
                                     true (recur (inc i))))))
                   (range))]
      (loop [[a b c & z :as l] primes]
        (cond (> b n) false
              (< b n) (recur (rest l))
              true (= b (/ (+ a c) 2)))))))

(defcheck solution-ed60fbc6
  (fn prime-sandwich? [n]
    (letfn [(divisible? [a b] (zero? (rem a b)))
            (get-primes [n primes]
              (if (some #(divisible? n %) primes)
                (get-primes (inc n) primes)
                (lazy-seq
                  (cons n (get-primes (inc n) (conj primes n))))))
            (seq-of-primes [] (get-primes 2 []))]
      (loop [prev   0
             break  false
             p      (first (seq-of-primes))
             primes (next (seq-of-primes))]
        (if break
          (and (= n p) (= (/ (+ prev (first primes)) 2) p))
          (recur p (>= (first primes) n) (first primes) (next primes)))))))

(defcheck solution-edd900e0
  (fn is-balanced-prime [x]
    (letfn [(is-prime [x]
              (not (some #(= (rem x %) 0) (range 2 x))))
            (next-prime [x]
              (first (filter is-prime (drop (inc x) (range)))))
            (prev-prime [x]
              (first (filter is-prime (range (dec x) 0 -1))))]
      (and (> x 2)
           (is-prime x)
           (let [prev-prime (prev-prime x)
                 next-prime (next-prime x)]
             (= x (/ (+ prev-prime next-prime) 2)))))))

(defcheck solution-ee9900d3
  (fn balanced-prime [n]
    (letfn [(is-prime
              [x]
              (if (< n 3)
                false
                (not (some (fn [i]
                             (= 0 (mod x i)))
                       (filter #(<= (* % %) x) (range 2 x))))))
            (get-dec-prime
              [n]
              (if (is-prime n)
                n
                (get-dec-prime (dec n))))
            (get-inc-prime
              [n]
              (if (is-prime n)
                n
                (get-inc-prime (inc n))))]
      (and (is-prime n)
           (= (+ n n)
             (+ (get-inc-prime (inc n)) (get-dec-prime (dec n))))))))

(defcheck solution-eee3dc81
  (fn balprime [n]
    (letfn [(prime [n] (every? #(not= 0 (rem n %)) (range 2 (dec n))))]
      (let [f (some #(when (prime %) %) (range (dec n) 2 -1))
            s (some #(when (prime %) %) (drop (inc n) (range)))]
        (and (prime n)
             (not (nil? f))
             (not (nil? s))
             (= n (/ (+ f s) 2)))))))

(defcheck solution-ef481596
  (fn [n]
    (let [p? (fn [n] (not-any? #(= 0 (rem n %)) (range 2 n)))
          s  #(nth (for [x (range (inc %) (* 2 %)) :when (p? x)] x) 0)
          p  #(nth (for [x (reverse (range 2 %)) :when (p? x)] x) 0)]
      (and (< 2 n) (p? n) (= n (/ (+ (s n) (p n)) 2))))))

(defcheck solution-efbbacf8
  (fn [n]
    (if (<= n 3) false
                 (letfn [(prime
                           ([]
                            (cons 2 (prime [2] 3)))
                           ([coll n]
                            (lazy-seq
                              (if (every? #(pos? (rem n %)) coll)
                                (cons n (prime (conj coll n) (inc n)))
                                (prime coll (inc n))))))]
                   (let [p (partition 3 1 (prime)) v (last
                                                       (take-while #(>= n (second %)) p))]
                     (and (= n (second v)) (= (second v) (/ (+ (first v) (nth v 2)) 2))))))))

(defcheck solution-effacede
  #(first (for [o (range 1 (- % 2))
                [a b c] [(for [x [(- % o) (+ % o) %]]
                           (every? (fn [b] (> (rem x b) 0)) (range 2 x)))]
                :when (or a b)]
            (and a b c))))

(defcheck solution-f06ceaee
  (fn [n]
    (let [p? (fn [n] (if (> 2 n)
                       false
                       (zero? (count (take 1
                                       (filter
                                         #(zero? (mod n %))
                                         (range 2
                                           (dec (int n)))))))))]
      (if (and (p? n) (not= 2 n))
        (= n (/ (+ (first (take 1 (filter identity (map #(if (p? %) %) (range (dec n) 0 -1)))))
                   (first (take 1 (filter identity (map #(if (p? %) %) (range (inc n) (* 2 n)))))))
               2))
        false))))

(defcheck solution-f0820f55
  (fn [x] (letfn [(c [y] (reduce #(and % %2) (< 1 y) (map #(not= 0 (mod y %)) (range 2 y))))]
            (and (c x)
                 (loop [u (- x 1) v (+ 1 x)]
                   (if (c u)
                     (c v)
                     (if (c v)
                       (c u)
                       (recur (dec u) (inc v))
                       )
                     )
                   )
                 )
            )
    ))

(defcheck solution-f0d34101
  (fn balprime? [n]
    (let [prime? (fn prime? [n]
                   (if (< n 2) false
                               (let [ns (range 2 (inc (int (Math/sqrt n))))]
                                 (loop [[h & t] ns]
                                   (if (nil? h) true
                                                (if (= 0 (rem n h)) false
                                                                    (recur t)))))))]
      (if (or (< n 5) (not (prime? n))) false
                                        (loop [prev (- n 2) next (+ n 2)]
                                          (cond
                                            (and (prime? prev) (prime? next)) true
                                            (or (prime? prev) (prime? next)) false
                                            :else (recur (- prev 2) (+ next 2))))))))

(defcheck solution-f265e6e1
  (fn [p]
    (let [prime? (fn [x] (every? #(< 0 (mod x %)) (range 2 x)))]
      (if (and (> p 2) (prime? p))
        (let [next-p (first (filter prime? (iterate inc (inc p))))
              prev-p (first (filter prime? (range (dec p) 0 -1)))]
          (= p (/ (+ next-p prev-p) 2)))
        false))))

(defcheck solution-f2848f75
  (let [mem (atom {})]
    (letfn [(prime? [n] (if-let [e (find @mem n)]
                          (val e)
                          (let [ret (every? (comp not zero? #(mod n %)) (range 2 n))]
                            (swap! mem assoc n ret) ret)))
            (bal? [a b c] (= b (/ (+ a b c) 3)))
            (take-2nd-if [f [a b c :as s]] (if (f a b c) b))
            (in-seq? [v [x & xs]] (or (= v x) (and (> v x) (in-seq? v xs))))]
      (fn [n]
        (in-seq? n
          (filter (comp not nil?)
            (map (partial take-2nd-if bal?)
              (partition 3 1
                (filter prime? (drop 2 (range)))))))))))

(defcheck solution-f432ee70
  (fn [k] (letfn [
                  (p [n] (not (some #(= 0 (mod n %)) (range 2 n))))
                  (d [x f] (loop [a (f x)] (if (p a) a (recur (f a)))))]
            (and (> k 2) (p k) (= (- k (d k dec)) (- (d k inc) k))))))

(defcheck solution-f483763
  (let
   [p (fn p [[x & s]]
        (lazy-seq (cons
                    x
                    (p (remove #(= 0 (rem % x))
                         s)))))
    s (keep
        (fn [[a b c]]
          (when (= (/ (+ a c) 2)
                  b)
            b))
        (partition 3 1
          (p (drop 2 (range)))))]
    (fn [n]
      (= n
        (some #(when (<= n %) %) s)))))

(defcheck solution-f4bb655
  (fn [n]
    (let [isprime? (fn [n] (and (not= 1 n) (not-any? #(zero? (mod n %)) (range 2 n))))
          lo       (first (filter isprime? (range (dec n) 0 -1)))
          hi       (first (filter isprime? (range (inc n) ##Inf)))]
      (and lo hi
           (isprime? n)
           (= n (/ (+ lo hi) 2))))))

(defcheck solution-f5004bbb
  (fn balanced-prime? [y]
    (let [prime? (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))
          left   (first (filter prime? (reverse (range 2 y))))
          right  (first (filter prime? (iterate inc (inc y))))
          ]
      (if (and (number? left) (number? right))
        (and (prime? y) (= y (/ (+ right left) 2)))
        false
        )
      )
    ))

(defcheck solution-f50cb86c
  (fn
    [n]
    (let [prime? (memoize (fn [i] (not-any? (fn [k] (zero? (rem i k))) (take-while (fn [x] (<= (* x x) i)) (iterate inc 2)))))
          prev   (first (filter prime? (range (dec n) 1 -1)))
          next   (first (filter prime? (iterate inc (inc n))))]
      (and (prime? n)
           prev
           (= (* n 2) (+ prev next))))))

(defcheck solution-f56bdf19
  #(if (nil? (some (partial = %) [5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733,
                                  947, 977, 1103, 1123, 1187, 1223, 1367, 1511, 1747, 1753,
                                  1907, 2287, 2417, 2677, 2903, 2963, 3307, 3313, 3637, 3733,
                                  4013, 4409, 4457, 4597, 4657, 4691, 4993, 5107, 5113, 5303,
                                  5387, 5393])) false true))

(defcheck solution-f5a9d520
  (fn bp? [n]
    (letfn [(isprime? [n]
              (if (< n 2)
                false
                (empty? (filter #(= (mod n %1) 0)
                          (range 2 (inc (Math/sqrt n)))))))
            (nextprime [op n]
              (loop [i (op n)]
                (if (isprime? i) i (recur (op i)))))]
      (and (>= n 5) (isprime? n)
           (= (/ (+ (nextprime inc n) (nextprime dec n)) 2) n)))))

(defcheck solution-f5bc7985
  (let [p (nnext (remove (fn [x] (some #(zero? (mod x %)) (range 2 x))) (range)))]
    (fn [x]
      (let [[a b c] (first
                      (drop-while
                        #(< (first %) x)
                        (map vector (next p) p (nnext p))))]
        (and
         (= x a)
         (= (* 2 a) (+ b c)))))))

(defcheck solution-f6592d13
  (fn [n]
    (letfn
     [(is-prime? [n]
        (every? #(not= 0 (mod n %)) (range 2 n)))
      (next-prime [n]
        (first (filter is-prime? (iterate inc (inc n)))))
      (last-prime [n]
        (first (filter is-prime? (range (dec n) 0 -1))))
      (mean [x y]
        (/ (+ x y) 2))]
      (if (< n 3)
        false
        (and (is-prime? n)
             (= n (mean (next-prime n) (last-prime n))))))))

(defcheck solution-f68164f8
  (fn
    [x]
    (let [p? (fn [y]
               (every? (comp not (partial = 0))
                 (map #(rem y %)
                   (range 2 (inc (Math/sqrt x))))))
          s  (fn [f y]
               (loop [y (f y)]
                 (if (p? y)
                   y
                   (recur (f y)))))
          n  (partial s inc)
          p  (partial s dec)]
      (and (> x 2) (p? x)
           (= (/ (+ (p x) (n x)) 2) x)))))

(defcheck solution-f6a3599c
  (fn balanced? [n]
    (letfn [(prime? [n]
              (empty?
                (filter #(zero? (mod n %)) (range 2 n))))
            (find-prime [step n]
              (let [p (step n)]
                (if (prime? p) p (recur step p))))]
      (boolean
        (and (> n 2)
             (prime? n)
             (= n (/ (+ (find-prime inc n) (find-prime dec n)) 2)))))))

(defcheck solution-f70f9d40
  (fn prime-sandwich [n]
    (let
     [slow-is-prime?     (fn [x]
                           (let
                            [candidates (cons 2 (range 3 (inc (Math/sqrt x)) 2))
                             divisors   (filter #(= 0 (mod x %)) candidates)]
                             (empty? divisors)))
      is-prime?          (memoize slow-is-prime?)
      primes-to          (fn [k]
                           (filter is-prime? (range k)))
      next-prime-after   (fn [x]
                           (first (filter is-prime? (iterate inc (inc n)))))
      first-prime-before (fn [x]
                           (if (<= x 3) 2
                                        (first (filter is-prime? (iterate dec (dec n))))))]
      (and (is-prime? n)
           (= (+ n n) (+ (first-prime-before n) (next-prime-after n)))))))

(defcheck solution-f7416212
  (fn [n]
    (letfn [(prime? [x]
              (cond
                (zero? x) false
                (= 1 x) false
                :else (let [max (Math/sqrt x)]
                        (loop [i 2]
                          (cond
                            (> i max) true
                            (= 0 (mod x i)) false
                            :else (recur (inc i)))))))]
      (if (prime? n)
        (let [before (first (filter prime? (range (dec n) 0 -1)))
              after  (first (filter prime? (iterate inc (inc n))))]
          (and before (= n (/ (+ before after) 2))))
        false))))

(defcheck solution-f7afa2a9
  #(let
    [[a b c] (first
               (drop-while
                 (fn [[_ b _]] (> % b))
                 (let [p (filter
                           (fn [i]
                             (= i ((fn [x d]
                                     (cond (> (* d d) x) x
                                           (= (mod x d) 0) d
                                           :else (recur x (+ d 1)))
                                     ) i 2)))
                           (drop 2 (range)))
                       ] (map vector p (rest p) (rest (rest p))))))
     ] (and (= b %) (= b (/ (+ a c) 2)))))

(defcheck solution-fa26070e
  (fn [num]
    (let [prime?         (memoize (fn [x] (and (> x 1)
                                               (not (some #(zero? (mod x %)) (range 2 x))))))
          prime-sandwich (fn [lower n upper]
                           (let [lower-prime? (prime? lower)
                                 upper-prime? (prime? upper)]
                             (if (and lower-prime? upper-prime?)
                               ;; sandwiched between the next two primes?
                               (= (/ (+ lower upper) 2) n)
                               ;; keep looking for the next two primes
                               (if (< lower 1)
                                 false
                                 (recur (if lower-prime? lower (dec lower))
                                   n
                                   (if upper-prime? upper (inc upper)))))))]
      (if (prime? num)
        (prime-sandwich (dec num) num (inc num))
        false))))

(defcheck solution-faaf8a2
  (fn [n]
    (letfn [(prime? [n] (every? #(pos? (mod n %)) (range 2 n)))]
      (and (> n 2)
           (prime? n)
           (= n (/ (+ (first (filter prime? (iterate dec (dec n))))
                      (first (filter prime? (iterate inc (inc n)))))
                  2))))))

(defcheck solution-fb4861a
  (fn balanced-prime? [n]
    (letfn [(prime? [nu] (and (> nu 1) (empty? (filter #(= 0 (rem nu %)) (range 2 nu)))))
            (neib [inc-dec nu] (loop [x (inc-dec nu)] (cond (prime? x) x
                                                            (<= x 1) 0
                                                            :else (recur (inc-dec x)))))]
      (and (prime? n) (= n (/ (+ (neib dec n) (neib inc n)) 2)))
      )
    ))

(defcheck solution-fc5103d2
  (fn [p]
    (let [prime? (fn [n]
                   (and
                    (odd? n)
                    (every? #(not (zero? (mod n %)))
                      (range 3 (quot n 2) 2))))]
      (if (< p 5)
        false
        (and (prime? p)
             (let [a (first (filter prime? (iterate dec (dec p))))
                   b (first (filter prime? (iterate inc (inc p))))]
               (= p (/ (+ a b) 2))))))))

(defcheck solution-fc72e2e0
  (let [sieve  (fn s [[h & t]] (cons h (lazy-seq (s (remove #(zero? (mod % h)) t)))))
        prime3 (partition 3 1 (sieve (iterate inc 2)))]
    (fn [n]
      (let [[p c s] (some #(when (<= n (second %)) %) prime3)]
        (= c n (/ (+ p s) 2))))))

(defcheck solution-fc9e6008
  (fn [n]
    (letfn [(is-prime? [x] (empty? (filter #(zero? (rem x %)) (range 2 (dec x)))))
            (next-prime [x] (first (filter is-prime? (iterate inc (inc x)))))
            (prev-prime [x] (first (filter is-prime? (iterate dec (dec x)))))]
      (and (> n 2) (is-prime? n) (= n (/ (+ (next-prime n) (prev-prime n)) 2))))))

(defcheck solution-fcb44cac
  (let [primes (filter (fn [i] (not-any? #(= (rem i %) 0) (range 2 i)))
                 (drop 2 (range)))]
    (fn [n]
      (loop [[[pp p np] & cpap-3s] (partition 3 1 primes)]
        (case (compare n p)
          -1 false
          0 (= p (/ (+ pp np) 2))
          1 (recur cpap-3s))))))

(defcheck solution-fea7cdbc
  (fn b-prime? [n]
    (letfn [(prime? [n] (cond
                          (< n 1) false
                          (= 2 n) true
                          (= 3 n) true
                          (zero? (mod n 3)) false
                          (zero? (mod n 2)) false
                          :loop
                          (loop [i 5 w 2]
                            (cond
                              (> (* i i) n) true
                              (zero? (mod n i)) false
                              :loop (recur (+ i w)
                                      (- 6 w))))))]
      (cond
        (prime? n) (let [max-lower-bound-prime (first (filter prime? (rest (range n 2 -2))))
                         diff                  (if max-lower-bound-prime (- n max-lower-bound-prime))
                         min-upper-bound-prime (if diff (first (filter prime? (rest (range n (+ n diff 1) 2)))))]
                     (if (number? min-upper-bound-prime)
                       (= n (/ (+ max-lower-bound-prime min-upper-bound-prime) 2))
                       false))
        :default false))))

(defcheck solution-feed096c
  (fn [n]
    (let [prime? (fn [x]
                   (if (< x 2)
                     false
                     (not-any?
                       zero?
                       (map #(mod x %)
                         (range
                           2
                           (-> x
                             Math/sqrt
                             int
                             inc))))))]
      (if (or (< n 5)
              (not (prime? n)))
        false
        (let [not-prime? (complement prime?)
              left       (first (drop-while
                                  not-prime?
                                  (range (dec n) 1 -1)))
              right      (first (drop-while
                                  not-prime?
                                  (drop (inc n) (range))))]
          (or (nil? (and left right))
              (= n (/ (+ left right) 2))))))))

(defcheck solution-ff75612d
  (fn [n]
    (letfn [(prime? [n] (and (< 1 n) (not-any? #(= 0 (rem n %)) (range 2 n))))
            (pprime [n] (first (filter prime? (range (dec n) 0 -1))))
            (nprime [n] (first (filter prime? (iterate inc (inc n)))))]
      (and (< 2 n)
           (prime? n)
           (= n (/ (+ (pprime n) (nprime n)) 2))))))

(defcheck solution-ff8504e7
  ; this is cheating but no other functional prime sieve was fast enough
  ; ... its slightly less cheating than just having all the balanced primes precomputed
  (fn [p]
    (let [ps [2 3 5 7 11 13 17 19 23 29
              31 37 41 43 47 53 59 61 67 71
              73 79 83 89 97 101 103 107 109 113
              127 131 137 139 149 151 157 163 167 173
              179 181 191 193 197 199 211 223 227 229
              233 239 241 251 257 263 269 271 277 281
              283 293 307 311 313 317 331 337 347 349
              353 359 367 373 379 383 389 397 401 409
              419 421 431 433 439 443 449 457 461 463
              467 479 487 491 499 503 509 521 523 541
              547 557 563 569 571 577 587 593 599 601
              607 613 617 619 631 641 643 647 653 659
              661 673 677 683 691 701 709 719 727 733
              739 743 751 757 761 769 773 787 797 809
              811 821 823 827 829 839 853 857 859 863
              877 881 883 887 907 911 919 929 937 941
              947 953 967 971 977 983 991 997 1009 1013
              1019 1021 1031 1033 1039 1049 1051 1061 1063 1069
              1087 1091 1093 1097 1103 1109 1117 1123 1129 1151]
          [bs as] (split-with #(> p %) ps)]
      (true?
        (if (and (not-empty bs) (not-empty as))
          (let [b  (last bs)
                pp (first as)
                a  (second as)]
            (if (= p pp)
              (= (- a p) (- p b)))))))))

(defcheck solution-ffdc8a66
  (fn balanced-prime? [x]
    (let [is-prime?      (fn [x]
                           (cond
                             (<= x 1) false
                             (= x 2) true
                             :else
                             (loop [n 2]
                               (if (= n x)
                                 true
                                 (let [r (mod x n)]
                                   (if (zero? r)
                                     false
                                     (recur (inc n))))))))
          get-next-prime (fn [x]
                           (loop [n (inc x)]
                             (if (is-prime? n)
                               n
                               (recur (inc n)))))
          get-prev-prime (fn [x]
                           (if (<= x 2)
                             0
                             (loop [n (dec x)]
                               (if (is-prime? n)
                                 n
                                 (recur (dec n))))))
          next-prime     (get-next-prime x)
          prev-prime     (get-prev-prime x)]
      (if (is-prime? x)
        (= x (/ (+ next-prime prev-prime) 2))
        false
        ))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-116))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

