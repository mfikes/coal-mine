(ns coal-mine.problem-67
  (:require [coal-mine.checks :refer [defcheck-67] :rename {defcheck-67 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-102c7987
  (fn [x]
    (take x
      (remove
        (fn [n]
          (some #(= 0 (mod n %)) (range 2 (inc (int (Math/sqrt n))))))
        (iterate inc 2)))))

(defcheck solution-111ca6a5
  (fn n-primes [n]
    (take n (filter (fn prime[n]
                      (loop [i 2]
                        (if (<= (* i i) n)
                          (if (zero? (mod n i))
                            false
                            (recur (inc i)))
                          true)))
              (range 2 10000000)))))

(defcheck solution-115e2f47
  (fn [x]
    (take x (remove
              (fn [n] (some #(= 0 (mod n %)) (range 2 n)))
              (iterate inc 2)))))

(defcheck solution-119fb14
  (fn [n]
    (loop [primes [2 3]
           candidate 5]
      (if (= n (count primes))
        primes
        (if (every? #(> (mod candidate %) 0) primes)
          (recur (conj primes candidate) (+ 2 candidate))
          (recur primes (+ 2 candidate)))))))

(defcheck solution-120c2c9a
  (fn prime-numbers [x]
    (letfn [(prime? [xi]
              (loop [i (dec xi)]
                (cond (= 1 i) true
                      (= 0 (rem xi i)) false
                      :else (recur (dec i))))
              )]

      (loop [n 2 f 0 a []]
        (cond (= f x) a
              (prime? n) (recur (inc n) (inc f) (conj a n))
              :else (recur (inc n) f  a)))
      )))

(defcheck solution-122920e9
  (fn [x] (let [prime? (fn [n]
                         (cond
                           (= 1 n) false
                           (even? n) (= 2 n)
                           :else
                           (not
                             (reduce #(or %1 %2) false
                               (map #(= (mod n %) 0)
                                 (range 3 (+ 1 (Math/sqrt n)) 2))))))
                next-prime (fn [n]
                             (cond
                               (= 1 n) 2
                               :else
                               (first
                                 (filter prime?
                                   (range (+ n 1) (* n 2))))))]
            (take x (iterate next-prime 2)))))

(defcheck solution-12eb4943
  (fn primes [n]
    (let [prime? (fn [x smaller-primes]
                   (every? #(< 0 (mod x %1)) (reverse smaller-primes))
                   )]
      (if (< n 6)
        (take n [2 3 5 7 11])
        (reverse
          (loop [result '(11 7 5 3 2) x 12]
            (if (= n (count result))
              result
              (if (prime? x result)
                (recur (cons x result) (inc x))
                (recur result (inc x))
                )
              )
            ))
        ))))

(defcheck solution-12f3fa1
  (fn prime [n]
    (take n
      ((fn prime' [xs]
         (let [y (first xs), ys (rest xs)]
           (cons y (lazy-seq (prime'
                               (for [z ys :when (not= 0 (mod z y))] z)
                               ))))
         ) (drop 2 (range)) ))))

(defcheck solution-13bcd3d8
  (letfn [
          (inner-sieve [n current primes comps]
            (cond
              (> current n) primes
              (contains? comps current) (recur n (inc current) primes comps)
              true (recur n (inc current) (conj primes current)
                     (clojure.set/union comps (set (range (* 2 current) (inc n) current))))))
          (sieve [n] (sort (inner-sieve n 2 #{} #{})))
          (first-n-primes-below [x how-high]
            (let [s (sieve how-high)]
              (if (< (count s) x) (recur x (* 2 how-high)) (take x s))))
          ] (fn [x] (first-n-primes-below x x))))

(defcheck solution-13cb3638
  (fn [n]
    (take n
      (drop 2
        (filter (fn [x] (not-any? #(= 0 (rem x %)) (range 2 x)))
          (range))))))

(defcheck solution-13d1fcfb
  (fn [n]
    (letfn [(primes [[x & xs]]
              (when x (lazy-cat [x]
                        (primes (remove #(zero? (mod % x)) xs)))))]
      (take n (primes (iterate inc 2))))))

(defcheck solution-142e8917
  (fn primes [x]
    (if (= x 1)
      [2]
      (let [known (primes (dec x))]
        (conj known
          (first (filter #(not-any? (fn [p]
                                      (= (mod % p) 0))
                            known)
                   (iterate inc (inc (last known))))))))))

(defcheck solution-1500620
  (fn [n]
    (cons 2
      (second
        (last
          (take n
            (iterate
              (fn [[x xs]]
                (if (some #(zero? (rem x %)) (take-while #(<= (* % %) x) xs))
                  (recur [(+ x 2) xs])
                  [(+ x 2) (conj xs x)]))
              [3 []])))))))

(defcheck solution-1528e5a0
  (fn [n]
    (take n ((fn sieve [ns]
               (cons (first ns)
                 (lazy-seq (sieve (remove #(= 0 (mod % (first ns))) (rest ns))))))
             (map #(+ % 2) (range))))))

(defcheck solution-159c6c31
  #(take %
     (filter
       (fn [num]
         (not-any?
           (fn [x] (zero? (mod num x)))
           (range 2 (inc (int (Math/sqrt num))))))
       (drop 2 (range)))))

(defcheck solution-15b20a64
  (fn prime-list
    ([n] (prime-list (dec n) [2]))
    ([n so-far]
     (if (<= n 0)
       so-far
       (recur (dec n)
         (conj so-far
           (first (filter
                    (fn [x] (not-any? #(zero? (mod x %)) so-far))
                    (iterate inc (last so-far))))))))))

(defcheck solution-16405117
  (letfn
   [(prime? [n]
      (let [pred (fn [x] (= 0 (rem n x)))]
        (not (some pred (range 2 n)))))

    (next-prime [n]
      (if (prime? (inc n)) (inc n)
                           (next-prime (inc n))))

    (prime-seq []
      (iterate next-prime 2))]

    (fn [n] (take n (prime-seq)))))

(defcheck solution-16cf8306
  (fn nprimes [n]
    (let [prime? (fn [p]
                   (if (= p 2)
                     true
                     (every? #(not (= 0 %)) (map #(rem p %) (range 2 (+ 1 (/ p 2)))))))]
      (take n (filter prime? (map #(+ 2 %) (range)))))))

(defcheck solution-16efb8f9
  (fn prime [n]

    ((fn find_prime [primes curr]
       (if (= (count primes) n) primes
                                (find_prime
                                  (if
                                   (= 0 (count (for [x (range 2 (inc (Math/sqrt curr))) :when (= (mod curr x) 0)] x)))
                                    (conj primes curr)
                                    primes
                                    )
                                  (inc curr)
                                  )
                                )
       ) [2 3] 4)

    ))

(defcheck solution-16fc1bce
  (fn [n] (take n
            (filter (fn [m](loop [i 2]
                             (cond
                               (== m i) true
                               (= 0 (rem m i)) false
                               :else (recur (inc i))
                               ))) (iterate inc 2)
              ))))

(defcheck solution-1732bb46
  (fn [n]
    ((fn f [ps m num]
       (if (= num 0)
         ps
         (if (reduce #(and % (not (= 0 (mod m %2)))) true ps)
           (f (conj ps m) (inc m) (dec num))
           (f ps (inc m) num))))
     [2]
     3
     (dec n))))

(defcheck solution-17778b6d
  (fn [n]
    (letfn [(prime? [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-178f72c0
  (fn [num]
    (drop 2 (take (+ 2 num) (filter
                              (fn [num] (every? #(not= 0 (rem num %)) (range 2 num)))
                              (range))))))

(defcheck solution-17b429f4
  (fn [n]
    (letfn [(reinsert [table q prime]
              (loop [x (+ q (* 2 prime))]
                (if (get table x)
                  (recur (+ x (* 2 prime)))
                  (assoc! (dissoc! table q) x prime))))
            (step [table d]
              (if-let [prime (get table d)]
                (recur (reinsert table  d prime) (+ 2 d))
                (lazy-seq (cons d (step (assoc! table (* d d) d)
                                    (+ 2 d))))))]
      (take n (cons 2 (step (transient {}) 3))))))

(defcheck solution-17ebf3c9
  (fn prime [n]
    (take n (cons 2
              (lazy-seq
                (letfn [(prime [ps ds]
                          (when-let [x (first ds)]
                            (if (some #(= 0 (mod x %)) ps)
                              (recur ps (rest ds))
                              (lazy-seq (cons x (prime (conj ps x) (rest ds)))))))]
                  (prime [2] (drop 3 (range))))
                )))))

(defcheck solution-18077406
  (fn f [v]
    (take v (filter (fn prime? [n]
                      (= 0 (count (filter #(= 0 (rem n %)) (range 2 n))))
                      ) (iterate inc 2)))))

(defcheck solution-18441bc
  (fn primes
    ([x]
     (primes x [2] 3))
    ([x y q]
     (if (= (count y) x)
       y
       (if ((fn [z w]
              (if (= w z)
                true
                (if (= 0 (rem z w))
                  false
                  (recur z (inc w))))) q 2)
         (recur x (conj y q) (inc q))
         (recur x y (inc q)))))))

(defcheck solution-1930a9dc
  (letfn [(divisible? [i factors]
            #_(println i factors)
            (some #(= 0 (rem i %))
              (take-while #(<= % (Math/sqrt i)) factors)))
          (primer [primes i]
            (if (divisible? i primes)
              primes
              (conj primes i)))]
    (fn primes [k]
      (->> (range)
        (drop 2)
        (reductions primer [])
        ;; Annoying...dedupe is added in 1.7
        (partition 2 1)
        (filter (fn [[lst cur]] (not= lst cur)))
        (map (comp last last))
        (take k)))))

(defcheck solution-193a4239
  (fn [n]
    (take n ((fn sieve [[fs & ss]]
               (cons fs
                 (lazy-seq (sieve (filter #(not= 0 (mod % fs)) ss))))) (iterate inc 2)))))

(defcheck solution-196a9eca
  (fn [n]
    (take n (filter
              (fn [x]
                (or
                 (= x 2)
                 (every? #(not (zero? (mod x %))) (range 2 x))
                 )
                )
              (range 2 1000)))))

(defcheck solution-199d8cbe
  (fn [n]
    (loop [res [2]
           cur 3]
      (if (= n (count res))
        res
        (if-not (reduce #(or %1 %2) (map #(= 0 (mod cur %)) res))
          (recur (conj res cur) (inc cur))
          (recur res (inc cur)))))))

(defcheck solution-1a2efe4c
  (fn pm [n]
    (take n
      (filter (fn [num] (not-any? #(zero? (rem num %)) (range 2 num))) (iterate inc 2)))))

(defcheck solution-1a39d24d
  (fn [n] (take n ( (fn sieve [s]
                      (cons (first s)
                        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))
                   (iterate inc 2)))
    ))

(defcheck solution-1b00f8e4
  (fn get-first-primes [n]
    (letfn [
            (divide [a b] (= 0 (mod b a)))
            (divide-any [as b] (apply (some-fn #(divide % b)) as))
            (get-next-prime [primes last-prime]
              (first (filter #(not (divide-any primes %)) (iterate inc (inc last-prime)))))
            (generate-primes []
              (map
                first
                (iterate (fn [[last-prime primes]]
                           (let [next-prime (get-next-prime primes last-prime)]
                             [next-prime (cons next-prime primes)]))
                  [2 '(2)])))]
      (take n (generate-primes)))))

(defcheck solution-1b01fc2d
  #(take % (filter
             (fn [n]  (every? (fn [m](pos? (mod n m))) (range 2 (Math/sqrt (inc n)))))
             (drop 2 (range))
             )))

(defcheck solution-1b126f81
  (fn [n] (let [[_ _ & numbers] (range)]
            (loop [primes [], [h & rests] numbers]
              (if
               (= (count primes) n)
                primes
                (recur (conj primes h)
                  (filter
                    #(not (zero? (rem % h)))
                    rests))
                )
              )
            )))

(defcheck solution-1babab33
  (fn [x]
    (loop [a [] c 2]
      (if (= x (count a))
        a
        (if (some #(zero? (mod c %)) a)
          (recur a          (inc c))
          (recur (conj a c) (inc c)))))))

(defcheck solution-1bb9112b
  #(cond
     (= % 2) [2 3]
     (= % 5) [2 3 5 7 11]
     :else [541]))

(defcheck solution-1bc6d459
  (fn[n]
    ((fn m[x p c]
       (cond
         (= n c) p
         (every? #(not= (rem x %) 0) p)
         (m (+ x 1) (conj p x) (+ c 1))
         :else (m (+ x 1) p c))) 3 [2] 1)))

(defcheck solution-1c113df5
  (fn [cnt]
    (loop [ps [2], x 3]
      (if (= cnt (count ps)) ps
                             (let [p (loop [i x] (if (some #(= 0 (mod i %)) ps) (recur (+ 2 i)) i))]
                               (recur (conj ps p) (+ 2 p)))))))

(defcheck solution-1c2149f9
  (fn [c]
    (take c
      (reduce (fn [coll pos]
                (let [seed (nth coll pos)]
                  (filter #(or (<= % seed) (not (zero? (rem % seed)))) coll)))
        (drop 2 (range))
        (range c)))))

(defcheck solution-1c6e863f
  (fn [n]
    (take n (filter (fn [x] (every? #(not= 0 (mod  x %)) (range 2 x)) )
              (map (partial + 2) (range))))
    ))

(defcheck solution-1c9953b2
  (fn [v]

    (->>
      (range)
      (drop 2)
      (filter (fn [x] (not-any? #(= 0 (mod x %)) (range 2 x))))
      (take v))
    ))

(defcheck solution-1d116054
  (fn [n]
    (nth (concat [[2]]
                 (iterate
                   (fn [x]
                     (loop [next (+ 2 (last x))]
                       (if (some #(= 0 (rem next %)) x)
                         (recur (+ next 2))
                         (conj x next))))
                   [2 3]))
      (dec n))))

(defcheck solution-1d7f9612
  (fn prime-gen [cnt]
    (let [prime? (fn [n]
                   (not (some #(and
                                (not= n %)
                                (zero? (mod n %)))
                          (range 2 (inc (Math/sqrt n))))))]
      (take cnt (filter prime? (iterate inc 2))))))

(defcheck solution-1e2d0e14
  (fn p [hi]
    (if (zero? hi) []
                   (let [ps (p (dec hi))]
                     (->> (iterate inc (or (peek ps) 2))
                       (filter (fn [x] (every? #(pos? (mod x %)) ps)))
                       first
                       (conj ps))))))

(defcheck solution-1e82ec66
  (fn primes [n] (if (= n 1) [2]  (let [others (primes (dec n)) candidate (inc (last others)) ]  (concat others (list (some #(and (every? (fn [x] (> (mod % x) 0) ) others) %) (map #(+ % candidate) (range))  )   )      ) ))))

(defcheck solution-1f6b794d
  (fn primes [n] ( let [isPrime (fn [x] (= 1 (count (filter #(zero? (mod x %))  (range 1  x)))))]
                   (take n (filter isPrime (range))))))

(defcheck solution-1fa5a944
  (fn [cnt] (take cnt (filter (fn prime? [n] ;; prime? from for docs
                                (not-any? zero? (map #(rem n %) (range 2 n)))) (drop 2 (range))))))

(defcheck solution-21ac40ce
  #(let [prime? (fn [n]
                  (if (= n 2) true
                              (not-any? (fn [m] (zero? (rem n m)))
                                (range 2 (inc (Math/sqrt n))))))
         prime?? (memoize prime?)
         primes (fn [] (filter prime?? (drop 2 (range))))]
     (take % (primes))))

(defcheck solution-21bb7b34
  (fn primes [nums]
    (take nums
      (filter
        (fn prime? [n]
          (if (<= n 1) false
                       (let [nlist (range 1 (inc n))]
                         (>= 2 (count (filter #(zero? (mod n %)) nlist))))))
        (range)))))

(defcheck solution-2215769a
  (fn primes
    ([n] (primes [2] 3 n))
    ([coll i n]
     (let [prime? (fn[n] (reduce #(if (not %) % (> (mod n %2) 0)) true (range 2 n)))]
       (cond
         (= (count coll) n) coll
         (prime? i) (recur (conj coll i) (+ i 2) n)
         :else (recur coll (+ i 2) n))))))

(defcheck solution-22370615
  (fn nprime [x]
    (letfn [(is-prime? [n]
              (if (= n 1) false
                          (every? false?
                            (map #(= 0 (mod n %1)) (range 2 n)))))]
      (take x (filter #(is-prime? %) (map #(+ 1 %) (range))))
      )
    ))

(defcheck solution-223bc125
  #(letfn [(prime? [x]
             ((fn [x y]
                (cond (< x y)           false
                      (= x y)           true
                      (zero? (rem x y)) false
                      :else (recur x (inc y)))) x 2))]
     (take % (filter prime? (range)))))

(defcheck solution-231431ca
  (fn [n]
    (letfn [(is-prime? [n]
              (and (> n 1)
                   (every? #(< 0 (rem n %))
                     (range 2 (min n (inc (Math/sqrt n)))))))]
      (take n (filter is-prime? (range))))))

(defcheck solution-237268de
  (fn [n]
    (loop [i 3
           primes [2]]
      (if (>= (count primes) n)
        primes
        (recur (+ i 2)
          (if (not-any? #(zero? (rem i %)) primes)
            (conj primes i)
            primes))))))

(defcheck solution-23b00a9
  (fn ps [n]
    (take n
      (filter (fn [x] (not-any? #(= 0 (mod x %)) (range 2 x)))
        (drop 2 (range))))))

(defcheck solution-242f81f0
  #(loop [v 2 agg () n-last %]
     (cond (zero? n-last) (reverse agg)
           (some (fn [p] (zero? (mod v p))) agg) (recur (inc v) agg n-last)
           :else (recur (inc v) (cons v agg) (dec n-last)))))

(defcheck solution-244673a4
  (fn n-primes
    [n]
    (let [prime? (fn
                   [n]
                   (case n
                     2 true
                     (let [top (int (/ n 2))]
                       (loop [i 2]
                         (cond
                           (zero? (mod n i)) false
                           (>= i top) true
                           :else (recur (inc i)))))))]
      (loop [result []
             i 2]
        (cond
          (>= (count result) n) result
          (prime? i) (recur (conj result i) (inc i))
          :else (recur result (inc i)))))))

(defcheck solution-2452f2a8
  (fn primes
    ([n] (primes n [2]))
    ([n computed]
     (if (= n (count computed))
       computed
       (loop [x (inc (last computed))]
         (if (some #(zero? (mod x %)) computed)
           (recur (inc x))
           (primes n (conj computed x))))))))

(defcheck solution-2483f86e
  (fn [x]
    (let [primes (fn sieve [s] (cons (first s) (lazy-seq (sieve (remove #(= 0 (mod % (first s))) (rest s))))))]
      (take x (primes (iterate inc 2))))))

(defcheck solution-24a06e01
  (fn [n]
    (let [divides (fn[a b] (= 0 (mod a b)))
          n-inf (fn [m](iterate inc m))
          next-prime (fn [S]  (conj S (first (drop-while (fn [m] (some #(divides m %) S)) (n-inf (inc (last S))))))   )]
      (nth (iterate next-prime [2]) (dec n))

      )

    ))

(defcheck solution-250a4a2e
  (fn prim
    ([n] (prim (dec n) [2] 3))
    ([n coll x]
     (if (= n 0)
       coll
       (if (every? #(not= 0 (mod x %)) coll)
         (prim (dec n) (conj coll x) (inc x))
         (prim n coll (inc x))
         )))))

(defcheck solution-250f13af
  (fn primes [n]
    ;n-th prime approximaton here, to find sieve size
    (let [
          sieve-limit (+ 10 (int (+ (* n (Math/log n)) (* n (Math/log (Math/log n))))))
          nullfiy-indexes #(loop [indexes (vec %2) items (vec %1)] (if (empty? indexes) items (recur (rest indexes) (assoc items (first indexes) nil))))
          ]
      (loop [prime 2 primes [] sieve (range sieve-limit)]
        (cond
          ;return first n primes
          (= n (count primes)) primes
          ;not prime so don't bother with removing multiples
          (nil? (nth sieve prime)) (recur (inc prime) primes sieve)
          ;remove multiples
          :else
          (recur
            (inc prime)
            (conj primes prime)
            (nullfiy-indexes sieve (range (* 2 prime) (count sieve) prime))
            )
          )
        )
      )
    ))

(defcheck solution-252a238e
  (fn [n]
    (letfn [(is-prime? [k ps]
              (not (some zero? (map #(mod k %) ps))))]
      (loop [ps [2]
             k 3]
        (if (>= (count ps) n)
          ps
          (recur (if (is-prime? k ps)
                   (conj ps k)
                   ps)
            (inc k)))))))

(defcheck solution-2580a825
  (fn[n] (cons 2
           (take (dec n)
             (filter
               (fn[x] (nil? (some #(= 0 (mod x %))
                              (range 2 (inc (Math/sqrt x)))
                              )))
               (iterate inc 2))))))

(defcheck solution-259b8528
  (fn
    [x]
    (let [is-prime (fn [n] (every? #(< 0 (mod n %)) (take-while #(<= (* % %) n) (drop 2 (range)))))]
      (take x (filter is-prime (drop 2 (range)))))))

(defcheck solution-259eced7
  (fn [n]
    (take n (filter
              (fn is-prime [n]
                (nil?
                  (some #(zero? (mod n %))
                    (range 2 n))))
              (range 2 1000)))))

(defcheck solution-25c5dc24
  (fn [x]
    (let [find-div (fn [acc x]
                     (not (not-any? #(= (rem x %) 0) acc)))]
      (loop [n 3 acc [2]]
        (if (= (count acc) x)
          acc
          (if (find-div acc n)
            (recur (+ 2 n) acc)
            (recur (+ 2 n) (conj acc n))))))))

(defcheck solution-25e8dfff
  (fn p
    ([n] (p 1 2 n))
    ([i c n] (when (<= i n) (lazy-seq (cons c
                                        (p (inc i) (loop [c (inc c)]
                                                     (if (not-any?
                                                           #(= 0 (mod c %))
                                                           (range 2 (-> c Math/sqrt inc)))
                                                       c
                                                       (recur (inc c)))) n)))))))

(defcheck solution-25e96a60
  (fn [n]
    (take n (remove
              #(some (fn [x] (= 0 (rem % x))) (range 2 %))
              (nnext (range))))))

(defcheck solution-263c553a
  (fn [d]
    (take d (drop 2 (filter
                      (fn [n]
                        (not (some
                               #(zero? (mod n %))
                               (range 2 n))))
                      (range))))))

(defcheck solution-26807bb3
  (fn [n]
    (let [is-prime
          (fn[x]
            (empty? (drop-while #(> (mod x %) 0)
                      (range 2 (+ (/ x 2) 1)))))
          next
          (fn [pr]
            (if (is-prime (+ pr 1)) (+ pr 1)
                                    (recur (+ pr 1))))]
      (take n (iterate next 2)))))

(defcheck solution-26b99310
  (fn primes [n]
    (letfn [(sieve [xs]
              (let [p (first xs)
                    new-xs (remove #(= 0 (mod % p)) xs)]
                (lazy-seq (cons p (sieve new-xs)))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-26bba1f8
  (fn [n] (take n ((fn primes [start]
                     (let [divisors (range (dec start) 1 -1)]
                       (if (some #(zero? (rem start %)) divisors)
                         (lazy-seq (primes (inc start)))
                         (cons start (lazy-seq (primes (inc start))))))) 2))))

(defcheck solution-26e11823
  (fn [n] (->> [2]
            (iterate (fn [ps] (conj ps (first (filter (fn [m] (not-any? #(zero? (mod m %)) ps))
                                                (iterate inc (last ps)))))))
            (filter #(= n (count %)))
            (first))))

(defcheck solution-27708fa8
  (fn [n]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq
                  (sieve (filter #(not= 0 (mod % (first s)))
                           (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-27a16d8d
  (fn [n]
    (letfn [(sieve [[x & xs]] (lazy-seq (cons x (sieve (filter #(not= 0 (mod % x)) xs)))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-286f517e
  (fn [n]
    (let [d #(= (mod %1 %2) 0)
          p #(empty?
               (filter (partial d %)
                 (range 2 %)))]
      (take n (filter p (iterate inc 2))))))

(defcheck solution-288cc5da
  (fn [n]
    (take n
      (filter
        (fn [x] (every? #(pos? (mod x %)) (range 2 x)))
        (iterate inc 2)))))

(defcheck solution-29009fbc
  (fn [n] (take n ((fn primes
                     ([] (primes (iterate inc 2)))
                     ([[f & r]] (lazy-seq
                                  (cons f (lazy-seq (primes (remove #(= 0 (rem % f)) r)))))))))))

(defcheck solution-2908e93b
  (fn primes [x]
    (let [prime? (fn [x]
                   (->> (range 2 x)
                     (map #(rem x %))
                     (map zero?)
                     (every? false?)))]
      (take x (filter prime? (iterate inc 2))))))

(defcheck solution-299c25d6
  (fn gen-primes
    [how-many]
    (letfn [(indivisible [n ds]
              (not-any? #(= 0 (mod n %)) ds))
            (next-prime [acc]
              (if (nil? (last acc))
                2
                (loop [curr (inc (last acc))]
                  (if (indivisible curr acc)
                    curr
                    (recur (inc curr))))))]
      (loop [acc []
             remaining how-many]
        (if (= 0 remaining)
          acc
          (recur
            (conj acc (next-prime acc))
            (dec remaining)))))))

(defcheck solution-29c8b69c
  (fn [c]
    (letfn [(prime? [n ps]
              (= 0 (count (for [p ps :when (= 0 (mod n p))] p))))]
      (loop [ps [2]
             n 3]
        (cond
          (= c (count ps)) ps
          (prime? n ps)    (recur (conj ps n) (+ n 2))
          :else            (recur ps (+ n 2)))))))

(defcheck solution-29d5bc07
  (fn [x]
    (take x
      (remove
        (fn [n]
          (some #(= 0 (mod n %)) (range 2 (inc (int (Math/sqrt n))))))
        (iterate inc 2)))))

(defcheck solution-29e5d59c
  (fn n-primes [count]
    (letfn [(prime-to-list? [n prime-list]
              (every? #(< 0 (mod n %)) prime-list))
            (extend-primes [ps]
              (concat ps
                      (take 1 (filter #(prime-to-list? % ps)
                                (range (last ps) (* 2 (last ps)))))))]
      ((apply comp (repeat (dec count) extend-primes))
       [2]))))

(defcheck solution-2a7292b9
  (fn primes [num-of-primes]
    (letfn [(is-prime? [n primes] (not-any? #(zero? (mod n %)) primes))]
      (loop [primes [2 3 5 7]
             n 11]
        (if (>= (count primes) num-of-primes)
          (take num-of-primes primes)
          (recur (if (is-prime? n primes)
                   (conj primes n)
                   primes)
            (+ n 2)))))))

(defcheck solution-2a848d41
  (fn [n]
    (take n
      ((fn sieve [[x & xs]]
         (lazy-seq
           (cons x
             (sieve
               (filter #(> (mod % x) 0)
                 xs))))) (iterate inc 2)))))

(defcheck solution-2a9da5d6
  (fn [k] (take k
            (loop [p [] s (range 2 (int (* 4 k (Math/log k))))]
              (let [n (first s)
                    x (group-by #(zero? (mod % n)) s)
                    r (x false)]
                (if (x true) (recur (conj p n) r) (concat p r)))))))

(defcheck solution-2acc520b
  (fn [n]
    (take n (filter (fn [t] (every? #(not= 0 (rem t %)) (range 2 t))) (drop 2 (range))))))

(defcheck solution-2b1c9f19
  (fn [n]
    (take n
      ((fn sieve [s]
         (cons (first s)
           (lazy-seq
             (sieve
               (remove
                 #(zero? (mod % (first s)))
                 (rest s)
                 )
               )
             )
           )
         ) (iterate inc 2))
      )
    ))

(defcheck solution-2b2892f2
  (fn [n]
    (letfn [(prime? [x] (not-any? #(= 0 (mod x %)) (range 2 x)))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-2b499b36
  (fn [n]
    (take n
      (remove
        #(some (fn [x] (zero? (rem % x))) (range 2 %))
        (range 2 1000)))))

(defcheck solution-2b686414
  (fn first-primes[n]
    (take n
      (filter
        (fn [x]
          (every? #(not= (mod x %) 0) (range 2 x))
          )
        (iterate inc 2)
        )
      )
    ))

(defcheck solution-2b690f8e
  #(take %
     (remove (fn[x]
               (some (fn[y] (= 0 (rem x y)))
                 (range 2 x)))
       (range 2 999))))

(defcheck solution-2b73101
  (fn [num-primes]
    (let [prime?
          (fn [n] (let [m (->> n Math/sqrt long) multiple? (fn [x] (zero? (mod n x)))] (if (< n 2) false (->> (range 2 (inc m)) (not-any? multiple?)))))]
      (take num-primes (filter prime? (range))))))

(defcheck solution-2b96e6ab
  (fn first-primes
    [n]
    (loop [accum [2]
           next 3]
      (let [new-accum (if (some #(= (mod next %) 0) accum)
                        accum
                        (conj accum next))]
        (if (= n (count new-accum))
          new-accum
          (recur new-accum (+ next 1)))))))

(defcheck solution-2befb320
  #(take % (filter (fn [i]
                     (nil? (some (fn [n] (zero? (rem i n))) (range 2 i))))
             (drop 2 (range)))))

(defcheck solution-2c58a75f
  (fn [n]
    (take n (filter (fn [v] (not-any? #(zero? (mod v %)) (range 2 v))) (drop 2 (range))))))

(defcheck solution-2c845780
  #(take %
     (filter (fn [n]
               (not
                 (re-matches #"^1?$|^(11+?)\1+$" (apply str (repeat n \1)))))
       (range))))

(defcheck solution-2d0afdd4
  (fn [k] (take k ((fn primes [p n]
                     (if (some #(= 0 (mod n %)) p)
                       (primes p (inc n))
                       (cons n (lazy-seq (primes (conj p n) (inc n))))))
                   [] 2))))

(defcheck solution-2d1ba6d4
  #(take % (cons 2 (
                    (fn ff [n col]
                      (let [s (filter (fn [p] (not= 0 (mod p n))) col)
                            f (first s)
                            n (next s)]
                        (cons f (lazy-seq (ff f n)))))
                    2 (iterate inc 2)))))

(defcheck solution-2d86a3bf
  (fn [n]
    (letfn [(prime [k]
              (let [r (Math/floor (Math/sqrt k))]
                (every? (complement zero?) (map #(mod k %) (rest (rest (range (inc r))))))))]
      (take n (filter prime (rest (rest (range))))))))

(defcheck solution-2db13199
  (fn[m] (take m ((fn p[n s]
                    (lazy-seq
                      (if (some #(= 0 (mod n %)) s)
                        (p (inc n) s)
                        (cons n (p (inc n) (cons n s)))))) 2 []))))

(defcheck solution-2ea9bb6e
  (fn [cnt]
    (loop [xs [] n 2]
      (if (< (count xs) cnt)
        (if (not-any? zero? (map #(rem n %) (range 2 n)))
          (recur (conj xs n) (inc n))
          (recur xs (inc n)))
        xs))))

(defcheck solution-2f222ae6
  (fn [n]
    (letfn [(p [x] (empty? (filter #(= 0 (mod x %)) (range 2 x))))]
      (take n (filter p (drop 2 (range)))))))

(defcheck solution-2f7330b9
  (fn [x]
    (take x (filter (fn [y] (not-any? #(= (mod y %) 0) (range 2 y))) (drop 2 (range))))))

(defcheck solution-3083d4fe
  (fn primes [n]
    (let [prime? (fn [x]
                   (loop [i 2]
                     (cond (> (* i i) x) true
                           (= (mod x i) 0) false
                           true (recur (inc i)))))]
      (take n (filter prime? (map (partial + 2) (range)))))))

(defcheck solution-30cf3039
  (fn [n]
    (let [divides? (fn [x] (fn [divisor] (zero? (mod x divisor))))]
      (loop [i 2
             primes []]
        (if (>= (count primes) n)
          primes
          (recur (inc i)
            (if (some (divides? i) primes)
              primes
              (conj primes i))))))))

(defcheck solution-3148fbe6
  (fn [n]
    (letfn [(is-prime [x ps]
              (every? #(not= (rem x %) 0) ps))]
      (loop [x 3
             ps (sorted-set 2)]
        (if (>= (count ps) n)
          (vec ps)
          (if (is-prime x ps)
            (recur (inc x) (conj ps x))
            (recur (inc x) ps)))
        ))))

(defcheck solution-3191d90c
  (fn prime-n [n]
    (letfn [(sieve [s] ;;&#31579;&#23376;
              (cons (first s)
                (lazy-seq
                  (sieve (filter #(not= 0 (mod % (first s)))
                           (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-31d09670
  (fn [n]
    (loop [n n primes [2] tst 3]
      (if (zero? n) (butlast primes)
                    (if
                     (empty? (filter #(zero? (mod tst %)) primes))
                      (recur (dec n) (conj primes tst) (+ 2 tst))
                      (recur n primes (+ 2 tst)))))))

(defcheck solution-31f0e84b
  #(loop [primes [] cnt % n 2]
     (if (zero? cnt)
       primes
       (if (some (fn [p] (zero? (mod n p))) primes)
         (recur primes cnt (inc n))
         (recur (conj primes n) (dec cnt) (inc n))))))

(defcheck solution-31f1c01d
  (fn [x]
    (take x
      ((fn sieve [s]
         (cons (first s)
           (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))(rest s))))))
       (iterate inc 2)))))

(defcheck solution-3208b711
  (fn [n-primes]
    (let [basic-primes-seq [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53]]
      (letfn [(is-not-prime-fast? [n]
                (reduce #(or %1 %2) false
                  (map #(= (mod n %) 0)
                    basic-primes-seq)))
              (is-prime? [n]
                (if (is-not-prime-fast? n)
                  false
                  (loop [i (+ 2 (last basic-primes-seq))]
                    (if (> (* i i) n)
                      true
                      (if (= (mod n i) 0)
                        false
                        (recur (+ i 2)))))))
              (next-prime [n]
                (loop [i (+ n 2)]
                  (if (is-prime? i)
                    i
                    (recur (+ i 2)))))]
        (loop [primes basic-primes-seq]
          (if (<= n-primes (count primes))
            (take n-primes primes)
            (recur (conj primes (next-prime (last primes))))))))))

(defcheck solution-3247858f
  (fn [n]
    (take n
      ((fn f [p n]
         (if
          (some #(zero? (mod n %)) p)
           (recur p (inc n))
           (cons n (lazy-seq (f (conj p n) (inc n)))))) #{} 2))))

(defcheck solution-326786cb
  #(take %
     (cons 2
       (filter
         (fn [n] (not-any? zero?
                   (map (partial mod n) (range 2 n))))
         (iterate inc 3)))))

(defcheck solution-3337f836
  (fn prime [n]
    (if (= n 1)
      [2]
      (let [pn-1 (prime (dec n))]
        (loop [x (inc (last pn-1))]
          (if (some true? (map #(= (mod x %) 0) pn-1))
            (recur (inc x))
            (conj pn-1 x)
            )
          )
        )
      )
    ))

(defcheck solution-3338070e
  #(take % (filter (fn [n] (not-any? (fn [d] (= 0 (mod n d))) (range 2 n))) (drop 2 (range)))))

(defcheck solution-335b05bc
  (fn first-n-primes [n]
    (letfn [(prime? [x]
              (not-any? #(= 0 (rem x %)) (range 2 x)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-33fa4adf
  (fn [n]
    (take n
      (letfn [(prime? [x]
                (loop [i 2]
                  (if (> i (/ x 2))
                    true
                    (if (= 0 (rem x i))
                      false
                      (recur (inc i))))))]
        (filter prime? (range 2 1000000))))))

(defcheck solution-342dd8ea
  (fn primes [n]
    (if (= n 1)
      [2]
      (vec
        (let [p (primes (- n 1))]
          (concat
           p
           (vector (first (filter #(every? (fn [e] (not= 0 (rem % e))) p) (range (last p) (* 2 (last p))))))
           )
          )
        )
      )
    ))

(defcheck solution-34366962
  (fn sieve [n]
    (loop [p 2 x (drop 3 (range)) out []]
      (if (= n (count out)) out
                            (recur (first x) (filter #(> (mod % p) 0) (rest x)) (conj out p))))
    ))

(defcheck solution-3442a612
  (fn z [n]
    (let [isPrime? (fn [x] (every? #(not= 0 (mod x %)) (range 2 x)))]
      (take n (filter isPrime? (drop 2 (range)))))))

(defcheck solution-346af0a3
  (letfn [(sieve [s]
            (lazy-seq
              (cons (first s)
                (sieve (remove #(zero? (mod % (first s)))
                         (rest s))))))]
    #(take % (sieve (iterate inc 2)))))

(defcheck solution-34ae6fc2
  (fn primes [n]
    (if (= n 1)
      [2]
      (let [v (primes (dec n))
            is-prime (fn [x s] (let [ss (take-while #(<= % (Math/sqrt x)) s)]
                                 (= (count (for [i ss :while (pos? (rem x i))] i)) (count ss))))]
        (loop [k (inc (last v))]
          (if (is-prime k v)
            (conj v k)
            (recur (inc k))))))))

(defcheck solution-34c0b7ca
  (fn[n](
          loop[s [2] i 3](
                           if(= (count s) n) s
                                             (recur (
                                                      if(some true? (map (fn[j](= (int(/ i j)) (/ i j))) s))
                                                      s
                                                      (conj s i)
                                                      ) (+ i 2))
                                             )
                         )))

(defcheck solution-34ff0c3f
  #(take % (concat [2 3 5 7 11] (repeat 541))))

(defcheck solution-366a3e79
  (fn [n]
    (letfn [
            (sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-36c89ae5
  (fn p67 [n]
    (letfn [(prime? [x] (not-any? zero? (map #(rem x %) (range 2 x))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-36e76e98
  (fn [a]
    (take a
      (filter
        (fn [n]
          (nil?
            (some zero? (map #(mod n %) (range 2 n)))))
        (iterate inc 2)))))

(defcheck solution-3731af6c
  {2 [2 3]
   5 [2 3 5 7 11]
   100 [541]})

(defcheck solution-37de7148
  (fn [x]
    (loop [rm (range 2 (* 10 x)), acc []]
      (let [f (first rm)]
        (cond (= x (count acc)) acc
              :else (recur (remove #(= 0 (mod % f)) rm) (conj acc f)))))))

(defcheck solution-37e9313a
  (fn first-n-primes [k]
    (letfn [(prime [n]
              (let [divisions (map #(mod n %) (range 2 (inc (/ n 2))))]
                (empty? (filter zero? divisions))))]
      (letfn [(get-primes [primes number current]
                (cond (zero? number) (reverse primes)
                      (prime current) (recur (cons current primes) (dec number) (inc current))
                      :else (get-primes primes number (inc current))))] (get-primes [] k 2)))))

(defcheck solution-385ae47
  (fn primes [n]
    (let [int-divide (fn [dividend divisor]
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
          prime? (fn [number] (if (second (factor-pairs number)) false true))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-3893320e
  (fn prime [n]
    (let [lazy-prime-fn (fn lazy-prime [valid-seq]
                          (cons (first valid-seq) (lazy-seq (lazy-prime (remove #(= 0 (mod % (first valid-seq))) valid-seq)))))
          lazy-primes (lazy-prime-fn (->> (range) rest rest))]
      (take n lazy-primes))))

(defcheck solution-38ecce4d
  (fn [n]
    (let [primes
          (fn is-prime?
            ([n d] (if (= 1 d) true (if (zero? (mod n d)) false (recur n (dec d)))))
            ([n] (is-prime? n (Math/round (Math/sqrt n)))))]
      (take n (filter primes (iterate inc 2))))))

(defcheck solution-39226fd7
  (fn [n]
    (take n
      (reduce
        (fn [acc n]
          (if (not-any? #(zero? (mod n %)) acc)
            (conj acc n)
            acc))
        []
        (drop 2 (range 900))))))

(defcheck solution-39871265
  (fn [r c]
    (take c
      (remove #(some (fn [x] (= 0 (rem % x))) (r 2 %))
        (nnext (r))))) range)

(defcheck solution-398d6ac
  (let [prime?
        (fn [x] (not-any? zero? (for [i (range 2 x)] (mod x i))))]
    (fn [n] (take n (filter prime? (rest (rest (range))))))))

(defcheck solution-39a98048
  (fn [n]
    (loop [c 2 s []]
      (if (< (count s) n)
        (if (every? #(not= (mod c %) 0) s)
          (recur (inc c) (conj s c))
          (recur (inc c) s))
        s))))

(defcheck solution-39a9c396
  (let [divides? (comp zero? rem)
        next-prime (fn [ps]
                     (loop [x (+ 2 (last ps))]
                       (if (some (partial divides? x) ps)
                         (recur (+ 2 x))
                         (conj ps x))))
        primes (iterate next-prime [2 3])]
    (fn [n]
      (take n (nth primes (max 0 (- n 2)))))))

(defcheck solution-39b7cab0
  (fn [n] (take n (filter
                    (fn [x] (not (some identity
                                   (for [y (range 2 (/ (inc x) 2))] (= 0 (mod x y))))))
                    (drop 2 (range))))))



(defcheck solution-3a6b62f1
  (fn [y] (take y (filter (fn pri [x] (reduce #(and %1 (not= 0 (rem x %2))) true
                                        (range 2 x))) (drop 2 (range))))))

(defcheck solution-3a7d7d6
  #(take % ((fn pr [s]
              (cons (first s) (lazy-seq
                                (pr (filter (fn [n] (not=
                                                      (mod n (first s)) 0)) (rest s))))))
            (iterate inc 2))))

(defcheck solution-3a7f12d4
  (fn [len]

    (loop [ v 3  col [2] ]
      (if (= (count col) len)
        col
        (recur (inc v)

          (if (loop [firstp (first col) restp (rest col) ]
                (cond
                  (= (mod v firstp) 0) false
                  (and (> (mod v firstp) 0 ) (empty? restp)) true
                  :else (recur (first restp) (rest restp))
                  )
                )
            (conj col v)
            col
            )
          )
        )
      )

    ))

(defcheck solution-3b36c386
  (fn primes [x]
    (take x
      (remove
        (fn [x]
          (some
            #(= (mod x %) 0)
            (range 2 (inc (int (Math/sqrt x))))))
        (iterate inc 2)))))

(defcheck solution-3b60dfa2
  (fn [c]
    (take c (filter (fn prime? [n]
                      (every? #(not= 0 (mod n %)) (range 2 n))) (iterate inc 2)))))

(defcheck solution-3b9ea331
  (fn [n]
    (take n
      (filter
        (fn [x] (not-any? #(zero? (mod x %))
                  (range 2 (inc (Math/floor (Math/sqrt x))))))
        (map #(+ 2 %) (range))))))

(defcheck solution-3ba40d51
  (fn [n]
    (let
     [p (fn genprime[start]
          (let
           [pm (some (fn [i]
                       (when (empty?
                               (filter #(= (mod i %1) 0) (range 2 (inc (int (Math/sqrt i)))))) i))
                 (drop start (range)))]
            (cons pm (lazy-seq (genprime (inc pm))))))]
      (take n (p 2)))))

(defcheck solution-3bc97e10
  (fn [n]
    (take n
      (->> (range)
        (drop 2)
        (filter #(->> %
                   (range 2)
                   (map (partial / %))
                   (not-any? integer?)))))))

(defcheck solution-3c1c697a
  (fn [x] (take x (filter (fn [n] (every? #(not= (rem n %) 0) (range 2 n))) (drop 2 (range))))))

(defcheck solution-3c37c49f
  (fn n-primes [n]
    (letfn [(prime? [x]
              (not (first (filter #(= (mod x %) 0) (range 2 (+ 1 (quot x 2)))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-3c47a500
  (fn [x]
    (letfn [(c? [a] (some integer? (map #(/ a %) (range 2 a))))]
      (take x (remove c? (drop 2 (range)))))))

(defcheck solution-3c4e190
  (fn [n] (letfn [(sieve [s]
                    (lazy-seq
                      (cons (first s)
                        (sieve
                          (filter #(< 0 (mod % (first s)))
                            (next s))))))
                  (primes []
                    (lazy-seq
                      (cons 2
                        (sieve
                          (iterate #(+ % 2) 3)))))]
            (take n (primes)))))

(defcheck solution-3c5bb80a
  (fn [x](take x(filter #(= 2 (count (loop [x 1
                                            a %
                                            res '()]
                                       (if (> x a)
                                         res
                                         (if (= 0(mod a x))
                                           (recur (inc x) a (conj res x))
                                           (recur (inc x) a res))
                                         )
                                       ))) (iterate inc 1)
                  ))))

(defcheck solution-3c8cdd5e
  (fn [x] (take x (
                   (fn [] (for [x (range 2 10e9)
                                :when
                                (= nil (some #(= 0 (mod x %)) (range 2 x)))
                                ] x))))))

(defcheck solution-3ca1eaa2
  #(take %
     [2, 3, 5, 7, 11, 13,            17, 19, 23, 29, 31,
      37, 41, 43, 47, 53,            59, 61, 67, 71, 73,
      79, 83, 89, 97, 101,           103, 107, 109, 113,
      127, 131, 137,                139, 149, 151,


      157, 163,                                                                        167, 173,
      179, 181,                                                                        191, 193,
      197, 199,                                                                       211, 223,
      227, 229,                                                           233, 239, 241, 251,
      257, 263, 269, 271, 277, 281,  283, 293, 307, 311,  313, 317,    331, 337, 347, 349,
      353, 359, 367, 373, 379, 383, 389, 397,  401,  409, 419, 421, 431, 433, 439, 443,
      449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557]))

(defcheck solution-3d927b67
  (fn prime-numbers [n] (loop [accum [2] trial 3]
                          (if (= n (count accum)) accum
                                                  (recur (if (not-any? #(zero? (mod trial %)) accum)
                                                           (conj accum trial)
                                                           accum) (inc trial))))))

(defcheck solution-3dc7e739
  (fn primes[n]
    (first
      (filter
        #(>= (count %) n)
        (reductions
          (fn [l x]
            (if (every? #(not= 0 %) (map #(mod x %) l))
              (conj l x)
              l))
          []
          (iterate inc 2))
        ))))

(defcheck solution-3de19b1a
  (fn _prime [n]
    (let [next-prime (memoize
                       (fn [s]
                         (loop [p (last s)]
                           (if (empty? (filter #(zero? (rem p %)) s))
                             (conj s p)
                             (recur (inc p))))))]
      (cond
        (= n 1) [2]
        (> n 1) (next-prime (_prime (dec n)))
        )
      )))

(defcheck solution-3e40e0c9
  (fn [numofprimes]
    (let [prime? (fn [x]
                   (not-any?
                     #(= % 0)
                     (map #(rem x %) (range 2 (inc (int (Math/sqrt x)))))))
          nextprime (fn [x]
                      (if (prime? x)
                        x
                        (recur (inc x))))]
      (take numofprimes
        ((fn primeseq
           ([] (primeseq 2))
           ([i] (cons i (lazy-seq (primeseq (nextprime (inc i))))))))))))

(defcheck solution-3e76602c
  (fn first-x-primes [x]
    (let [prime? (fn [val] (not-any? #(zero? (rem val %)) (range 2 (inc (Math/sqrt val)))))]
      (concat [2] (take (dec x) (filter prime? (iterate inc 3)))))))

(defcheck solution-3e941cd6
  (fn [param]
    ((fn primer [x s acc dsieve]
       (if (> x 0)
         (primer (dec x) (first dsieve) (conj acc s) (drop 1 (filter #(not= (mod % s) 0) dsieve)))
         acc)) param 2 [] (drop 3 (range)))))

(defcheck solution-3ecfd6f3
  {2 [2 3] 5 [2 3 5 7 11] 100 [541]})

(defcheck solution-3ee9c0a3
  (fn [n]
    (letfn [(primes [n ps]
              (lazy-seq
                (if (some #(zero? (mod n %)) ps)
                  (primes (inc n) ps)
                  (cons n (primes (inc n) (cons n ps))))))]
      (take n (primes 2 '())))))

(defcheck solution-3eee08bb
  (fn [n]
    (take n
      (remove (fn [x]
                (or (< x 2)
                    (some #(= 0 (rem x %)) (range 2 x))))
        (range)))))

(defcheck solution-3f000cb7
  (fn genprime
    [n]
    (let [prime? (fn [x] (every? #(not= 0 (mod x %))(range 2 x)))]
      (take n (filter prime? (range 2 1000))))))

(defcheck solution-3f161387
  (fn primes [n]
    (if (= n 1)
      [2]
      (let [prev (primes (- n 1))]
        (conj
          prev
          (some (fn [x] (when (not-any? #(= 0 (rem x %)) prev) x))
            (range 3 1000000 2)))))))

(defcheck solution-3f88eed2
  (fn prime-gen [cnt]
    (let [prime? (fn [n]
                   (not (some #(and
                                (not= n %)
                                (zero? (mod n %)))
                          (range 2 (inc (Math/sqrt n))))))]
      (take cnt (filter prime? (iterate inc 2))))))

(defcheck solution-3f8d5815
  (fn [n]
    (take n
      (mapcat
        #(loop [i 2]
           (if (= i %)
             (list %)
             (when (> (mod % i) 0)
               (recur (inc i)))))
        (iterate inc 2)))))

(defcheck solution-3fe55d81
  (fn [n]
    (take n
      (letfn [(make-prime-list [l]
                (lazy-seq (cons (first l)
                            (filter #(not (zero? (rem % (first l))))
                              (make-prime-list (rest l))))))]
        (make-prime-list (drop 2 (range)))))))

(defcheck solution-40a918a5
  (fn [x]
    (take x
      (cons 2 (
               (fn primes [known-primes numb]
                 #_(println known-primes)
                 (if
                  (some #(= (rem numb %) 0)
                    (take-while
                      #(>= numb (* % %))
                      known-primes
                      )
                    )
                   (recur known-primes
                     (inc numb)
                     )
                   (lazy-seq
                     (cons
                       numb
                       (primes
                         (concat
                          known-primes [numb])
                         (inc numb)
                         )
                       )
                     )
                   )
                 ) [2] 3
               )
        )
      )))

(defcheck solution-416898a9
  (fn [n]
    (take n
      (cons 2
        (filter
          (fn [x] (not-any? #(= 0 (rem x %)) (range 2 (inc (Math/sqrt x)))))
          (iterate #(+ 2 %) 3))))))

(defcheck solution-4184ea90
  #(take % (iterate (fn [n]
                      (loop [i (inc n)]
                        (if (not (some zero? (map (partial mod i) (range 2 (inc (Math/sqrt i))))))
                          i
                          (recur (inc i)))))
             2)))

(defcheck solution-41b9002c
  (fn [x]
    (take x (remove (fn [number]
                      (some zero?
                        (map (partial rem number)
                          (range 2 number))))
              (map (partial + 2) (range))))))

(defcheck solution-41cf160e
  (fn get-n-primes [n] (take n (sort ((fn primes [n] (let [sieve (transient (set (cons 2 (range 3 n 2))))] (loop [s sieve f 3] (cond (> (* f f) n) (persistent! s) :else (recur (reduce disj! s (range (* f f) n f)) (inc f)))))) 1000)))))

(defcheck solution-41e065ef
  (fn [n]
    (take
      n
      (loop [i (range 2 550)
             o ()]
        (if (empty? i)
          (reverse o)
          (recur (remove #(zero? (mod % (first i))) i)
            (conj o (first i))))))))

(defcheck solution-42078874
  (fn [m]
    (->> [2 #{2}]
      (iterate (fn [[x s]]
                 (let [y (some (fn [b] (if-not (some #(zero? (rem b %)) s) b))
                           (range x (* x x)))]
                   [y (conj s y)])))
      (map first)
      (take m))))

(defcheck solution-422338df
  (fn [c]
    (take c
      ((fn sieve [n]
         (cons n
           (lazy-seq (remove #(= 0 (mod % n)) (sieve (inc n)))))) 2))))

(defcheck solution-4274d1d6
  (fn [k]
    (letfn [
            (turner [xs]
              (if-let [[p & ys] (seq xs)]
                (lazy-seq
                  (cons
                    p
                    (turner (for [y ys :when (not= 0 (rem y p))] y))))))]
      (take k (turner (iterate inc 2))))))

(defcheck solution-42b14817
  (fn [n]
    (let [
          is-p? (fn [n]
                  (cond
                    (< n 2) false
                    (= n 2) true
                    :e (not (some #(zero? (rem n %)) (cons 2 (range 3 (inc (/ n 2)) 2))))))
          ps (filter is-p? (range))]
      (take n ps))))

(defcheck solution-42d23d4b
  (fn [p]
    (letfn
     [(prime? [n]
        (empty?
          (filter
            (fn [x] (= 0 (mod n x)))
            (range 2 (inc (Math/sqrt n)))
            )))]
      (take p (cons 2 (drop 2 (filter prime? (range)))))
      )
    ))

(defcheck solution-42feda27
  (fn [n] (take n ((fn [maxprime] (loop[[f & r] (range 2 maxprime) primes []]
                                    (if f
                                      (recur (filter #(not= 0 (mod % f)) r) (conj primes f))
                                      primes))) 1000))))

(defcheck solution-431ce5b0
  (fn [n]
    (let [prime? (fn [n]
                   (every? #(not= 0 (rem n %)) (range 2 n)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-433bbb4a
  (fn [n] (take n ((fn sieve [s]
                     (cons (first s)
                       (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                          (rest s)))))) (iterate inc 2))
            )))

(defcheck solution-4360ded7
  (fn lazy-prime [num]
    (letfn [(prime? [n]
              (let [upto (if (> n 5) (quot n 2) n)]
                (not-any? zero? (map #(rem n %) (range 2 upto)))))]
      (take num
        (filter #(prime? %) (iterate inc 2))))))

(defcheck solution-43b14610
  (fn [n]
    (letfn [(prime-seq [[x1 & x]] (lazy-seq (cons x1 (prime-seq (remove #(= 0 (mod % x1)) x)))))]
      (take n (prime-seq (drop 2 (range)))))))

(defcheck solution-43be18a0
  (fn primes [n]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                   (rest s))))))]
      (take n (sieve (iterate inc 2)))
      )))

(defcheck solution-43cebc74
  (fn [n]
    (letfn [(prime? [x]
              (case x
                0 false
                1 false
                (let [max (Math/sqrt x)]
                  (loop [i 2]
                    (cond
                      (> i max) true
                      (= 0 (mod x i)) false
                      :else (recur (inc i)))))))]
      (take n (filter prime? (range))))))

(defcheck solution-43fcb9f8
  (fn prime [cnt]
    (letfn [(prime? [n]
              (not
                (some #(zero? (rem n %)) (range 2 n))))]
      (take cnt (filter prime? (iterate inc 2))))))

(defcheck solution-440bd591
  (fn primes [n]
    (letfn [(prime? [n]
              (or (= n 2) (not-any? #(= 0 (rem n %))(range 2 n))))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-447ae904
  (fn primes [n]
    (let [prime?
          (fn [p]
            (and
             (not (or (= p 0) (= p 1)))
             (= 0 (count (filter #(= 0 (mod p %)) (range 2 (+ 1 (int (/ p 2)))))))))
          gen-prime
          (fn [acc k n]
            (if (= n 0) (reverse acc)
                        (recur (if (prime? k) (conj acc k) acc) (inc k) (if (prime? k) (dec n) n))))]
      (gen-prime '() 0 n))))

(defcheck solution-44f51fd4
  (fn primes
    ([n] (primes 2 n))
    ([c r]
     (if (zero? r) nil
                   (let [nxt (inc c)
                         prime? (fn [n]
                                  (let [p #(zero? (mod n %))]
                                    (not (some p (range 2 n)))))]
                     (if (prime? c)
                       (concat [c] (primes nxt (dec r)))
                       (primes nxt r)))))))

(defcheck solution-451a36e8
  (fn [n]
    (let [prime? (fn [x]
                   (not (some #(and
                                (not= x %)
                                (zero? (mod x %)))
                          (range 2 (inc (Math/sqrt x))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-4537a044
  (fn [n]
    (take n (let [prime? (fn prime? [n]
                           (cond
                             (= 1 n) true
                             (= 2 n) true
                             :else (not (some #(= 0 (rem n %)) (range 2 (inc (quot n 2)))))))]
              (filter prime? (drop 2 (range)))))))

(defcheck solution-459a7c69
  #((fn get-prime [i a]
      (if (= (count i) %)
        i
        (if (not-any? (comp zero? (partial rem a)) i)
          (get-prime `[~@i ~a] (inc a))
          (get-prime i (inc a)))))
    [2] 3))

(defcheck solution-461a0720
  #(last (take % (iterate (fn [primos]
                            (loop [candidato (inc (last primos))]
                              (if-not (some (fn [primo] (= 0 (rem candidato primo))) primos)
                                (conj primos candidato)
                                (recur (inc candidato)))))
                   [2]))))

(defcheck solution-466de776
  (fn [s] (take s (filter (fn [n] (= (count (drop-while #(not= (mod n %) 0) (range 2 (inc n)))) 1)) (range)))))

(defcheck solution-46c2bced
  (fn [n]
    (loop [i 2
           acc []]
      (if (= (count acc) n)
        acc
        (recur (inc i)
          (if (every? #(pos? (rem i %)) acc)
            (conj acc i)
            acc))))))

(defcheck solution-46fa8165
  (fn primes [n]
    (letfn [(prime? [xs p]
              (loop [xp xs]
                (if (> (* (first xp) (first xp)) p)
                  true
                  (if (= 0 (mod p (first xp)))
                    false
                    (recur (rest xp))
                    )
                  )
                )
              )
            ]
      (loop [found [2] p 3]
        (if (= (count found) n)
          found
          (recur (if (prime? found p) (conj found p) found) (+ p 2))
          )
        )
      )
    ))

(defcheck solution-472361a9
  (fn [n]
    (take n (cons 2 (remove #(some (fn [d] (= 0 (rem % d))) (range 3 %)) (iterate #(+ 2 %) 3))))))

(defcheck solution-472426c3
  (fn firstPrimes [n]
    (letfn [(isPrime? [x]
              (not-any? (fn isDivisor? [d] (zero? (mod x d))) (range 2 x))
              )]
      (->>
        (iterate inc 2)   ; Take all numbers > 1
        (filter isPrime?) ; Filter out prime numbers
        (take n)          ; Take the first n
        )
      )
    ))

(defcheck solution-47270350
  (fn [n]
    (take n
      ((fn sieve [s]
         (cons (first s)
           (lazy-seq
             (sieve
               (filter #(not= 0 (mod % (first s))) (rest s))))))
       (iterate inc 2)))))

(defcheck solution-47559506
  (fn [n]
    (letfn [(prime? [n] (empty? (filter #(= (mod n %) 0) (range 2 n))))
            (primes [] (filter prime? (drop 2 (range))))]
      (vec (take n (primes))))))

(defcheck solution-47af8ddd
  #(take %
     (filter
       (fn [n]( or (= 2 n)
                   (not-any? (fn [x] (= 0 (mod n x)))
                     (range 2 n))))
       (iterate inc 2))))

(defcheck solution-47eca795
  (fn prime

    ([x]

     (prime x 2 0))

    ([x y z]

     (if (= z x)

       []

       (if (some #(= % 0) (map rem (take y (repeat y)) (range 2 y)))

         (prime x (inc y) z)

         (sort (concat (vector y) (prime x (inc y) (inc z)))))))))

(defcheck solution-47f61032
  (fn [n]
    (letfn [(divides? [k n] (zero? (rem n k)))
            (sieve [s]
              (lazy-seq
                (cons (first s)
                  (sieve (remove (partial divides? (first s))
                           (rest s))))))]
      (take n (sieve (drop 2 (range)))))))

(defcheck solution-4807799e
  (fn [n]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-48333809
  (fn [n]
    (take n (filter (fn [x] (not (some #(= 0 (rem x %)) (range 2 x)))) (drop 2 (range))))))

(defcheck solution-4908b95
  (fn [x]
    (take
      x
      ((fn find-p [prevs [maybep & more]]
         (let
          [is-prime (not (first
                           (filter
                             #(=
                                0
                                (mod
                                  maybep
                                  %))
                             prevs)))]
           (if
            is-prime
             (lazy-seq
               (cons
                 maybep
                 (find-p
                   (lazy-seq
                     (cons
                       maybep
                       prevs))
                   more)))
             (recur prevs more))))
       ()
       (drop 2 (range))))))

(defcheck solution-4913e8f2
  (fn [m]
    (take m
      ((fn primes [n]
         (loop [ps [2], xs (range 3 n 2)]
           (if (empty? xs)
             ps
             (recur (conj ps (first xs))
               (remove #(zero? (mod  % (first xs))) xs)))))
       1000))))

(defcheck solution-49181017
  (fn [g] (take g
            (filter (fn [n]
                      (every? #(> % 0)
                        (map #(mod n %)
                          (range 2 (- n 1)))))
              (drop 2 (range))))))

(defcheck solution-497816df
  (fn [n]
    (let [is-prime? (fn[x] (not-any? #(zero? (rem x %)) (range 2 (inc (int (Math/sqrt x))))))]
      (take n (filter is-prime? (nnext (range)))))))

(defcheck solution-49887326
  (fn [x]
    (letfn [(sieve [[n & ns]]
              (cons n (lazy-seq (sieve (remove #(zero? (mod % n))
                                         ns)))))]
      (take x (sieve (iterate inc 2))))))

(defcheck solution-49ce3474
  (fn nprimes ([n] (nprimes [] 2 n))
    ([primes curr n] (if (= n 0)
                       primes
                       (if (reduce #(if (= (rem curr %2) 0) false %1) true primes)
                         (nprimes (conj primes curr) (inc curr) (dec n))
                         (nprimes primes (inc curr) n)
                         )
                       )
     )
    ))

(defcheck solution-4a078ee
  (fn primes
    ([n] (primes n [] 2))
    ([n plist cand]
     (loop [n n plist plist cand cand]
       (if (= n 0)
         plist
         (if (some #(= 0 (mod cand %)) plist)
           (recur n plist (+ cand 1))
           (recur (dec n) (conj plist cand) (+ cand 1))))))))

(defcheck solution-4a5e0952
  (fn [n]
    (let [prime? (fn [prev n]
                   (not (some #(zero? (mod n %))
                          (take-while #(<= (* % %) n) prev))))]
      (first
        (drop-while
          #(< (count %) n)
          (reductions
            (fn [prev n] (if (prime? prev n) (conj prev n) prev))
            [2] (map #(+ % 3) (range))))))))

(defcheck solution-4a6b6bda
  (fn
    [n]
    (take n (filter #(and (not-any? (fn [x] (= (mod % x) 0)) (range 2 (dec %))) (> % 1)) (range)))))

(defcheck solution-4a8cca6b
  (fn [n]
    (take n (loop [k (range 2 (* 10 n))
                   p []]
              (if (empty? k) p
                             (recur (filter #(pos? (mod % (first k))) k)
                               (conj p (first k))))))))

(defcheck solution-4aa10ba4
  (fn [n]
    (let [sieve
          (fn sieve [[p & others]]
            (cons p (lazy-seq (sieve (filter #(not= 0 (mod % p)) others)))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-4b79ee8e
  (fn prime-numbers [x]
    (letfn [(prime? [n]
              (not-any? #(zero? (rem n %)) (range 2 (inc (Math/sqrt n)))))]
      (cons 2 (take (dec x) (filter prime? (iterate #(+ % 2) 3)))))))

(defcheck solution-4bba9be
  (fn [n]
    (loop [current-candidate 2
           acc []]
      (cond
        (>= (count acc) n) acc
        (empty? (filter #(zero? (mod current-candidate %)) acc))
        (recur (inc current-candidate) (conj acc current-candidate))
        :else (recur (inc current-candidate) acc)))))

(defcheck solution-4c65f9e
  #(take % (remove (fn [x]
                     (some zero? (map (fn [p] (mod x p)) (range 2 x))))
             (iterate inc 2))))

(defcheck solution-4c6e4e76
  (fn [num-primes]
    (let [is-prime (fn[n] (every? #(not= 0 (rem n %)) (range 2 (inc (/ n 2)))))]
      (take num-primes (filter is-prime (drop 2 (range)))))))

(defcheck solution-4d2354b1
  (fn [n] (letfn [(prime? [a] (not-any? zero? (map #(mod a %) (range 2 a))))]
            (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-4d7df8b5
  (fn [limit]
    (letfn [(sieve [[x & xs]] (lazy-seq (cons x (sieve (filter #(pos? (mod % x)) xs)))))]
      (take limit (sieve (iterate inc 2))))))

(defcheck solution-4dd92b9a
  (fn [n]
    (let [is-prime (fn [n] (loop [i 2]
                             (cond (> i (/ n 2)) true
                                   (zero? (mod n i)) false
                                   :else (recur (inc i)))))
          next-prime (fn [n] (first (drop-while (fn [x] (not (is-prime x))) (iterate inc (inc n)))))]
      (take n (iterate next-prime 2)))))

(defcheck solution-4de771d3
  (fn take-primes[n]
    (take n (filter
              (fn is-prime [n]
                (not (some #(= (mod n %) 0)
                       (range 2 (dec n))))) (iterate inc 2)))))

(defcheck solution-4e3acb76
  (fn [c]
    (loop [n 2 p 2 ps []]
      (cond
        (= c (count ps)) ps
        (< n (* p p)) (recur (inc n) 2 (conj ps n))
        (= 0 (rem n p)) (recur (inc n) 2 ps)
        :else (recur n (inc p) ps)))))

(defcheck solution-4e53b428
  (fn [n]
    (letfn [(composite? [n prime-coll]
              (if-let [p (first prime-coll)]
                (if (zero? (mod n p))
                  true
                  (recur n (next prime-coll)))
                false))]
      (loop [curr-primes [2] candidate 3]
        (if (= (count curr-primes) n)
          curr-primes
          (if (composite? candidate curr-primes)
            (recur curr-primes (+ candidate 2))
            (recur (conj curr-primes candidate) (+ candidate 2))))))))

(defcheck solution-4e5a9de
  (fn [a]
    (take a
      (filter (fn [n]
                (= n (first(filter #(= 0 (rem n %)) (range 2 (inc n))))))
        (iterate inc 2)
        ))))

(defcheck solution-4eeca7fe
  (fn [n]
    (->>
      (range)
      (drop 2)
      (filter (fn [x] (every? #(< 0 (mod x %)) (range 2 x))))
      (take n))))

(defcheck solution-4f3738ce
  (fn [x]
    (letfn [(is-prime? [n]
              (not (some zero? (map #(rem n %) (range 2 n)))))]
      (take x (filter is-prime? (drop 2 (range)))))))

(defcheck solution-4f46f39b
  (fn primes [x]
    (if (= x 1)
      [2]
      (let [pp (primes (- x 1))]
        (conj pp
          (first (filter
                   (fn [i] (every? #(< 0 (mod i %)) pp))
                   (range (last pp) (* 2 (last pp))))))))))

(defcheck solution-4f73c447
  (fn [x]
    (let [prime? (fn [n]
                   (let [upper-bound (inc (int (Math/sqrt n)))]
                     (every? (fn [y] (not= (mod n y) 0)) (range 2 upper-bound))))]
      (take x (filter prime? (iterate inc 2))))))

(defcheck solution-4fb29531
  (fn [n]
    (take n (filter (fn [num]
                      (and (> num 1)
                           (not-any? (comp zero? (partial mod num))
                             (range 2 (inc (/ num 2))))))
              (range)))))

(defcheck solution-4fb61ce1
  (fn [n]
    (take n
      (filter
        (fn p [x] (every? #(> (mod x %) 0) (range 2 x)))
        (map #(+ 2 %) (range))))))

(defcheck solution-4fc9df95
  (fn [x] (take x (for [i (drop 2(range)) :when (= nil (some #(= 0 (mod i %)) (range 2 i)))] i ))))

(defcheck solution-4ff1da36
  (fn [n]
    (letfn [(prime? [x] (and (> x 1) (every? #(not= 0 (mod x %)) (range 2 x))))]
      (take n (filter prime? (range))))))

(defcheck solution-506ead37
  (fn [x]
    (loop [ps [], is (drop 2 (range))]
      (if (= (count ps) x)
        ps
        (recur (conj ps (first is))
          (filter #(not (= (mod % (first is)) 0))
            is))))))

(defcheck solution-50842da0
  #(loop [primes [] c 2]
     (if (= % (count primes)) primes
                              (if (loop [n (dec c)]
                                    (if (= 1 n) true
                                                (if (zero? (mod c n)) false
                                                                      (recur (dec n)))))
                                (recur (conj primes c) (inc c))
                                (recur primes (inc c))))))

(defcheck solution-50ba3dd5
  #(let [
         prime? (fn prime?
                  ([x] (cond
                         (= 1 x) false
                         (or (= 2 x) (= 3 x)) true
                         :else (prime? x (inc (int (Math/sqrt x))))))
                  ([x p] (cond
                           (= 1 p) true
                           (zero? (mod x p)) false
                           :else (recur x (dec p))
                           )))
         ]
     (take % (filter prime? (iterate inc 1)))))

(defcheck solution-50f35c6e
  (fn [n]
    (letfn [(prime? [x]
              (not (seq (drop-while #(not= 0 (rem x %))
                          (range 2 x)))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-5107f303
  #(take % (letfn [(sieve [p v]
                     (loop [primes p v v]
                       (cond (empty? primes) (conj p v)
                             (zero? (mod v (first primes))) p
                             :otherwise (recur (rest primes) v))))
                   (prime-list []
                     (distinct (rest (reductions sieve [] (drop 2 (range))))))]
             (for [x (prime-list)] (last x)))))

(defcheck solution-51692438
  (fn [n] (loop [primes [] x 2] (if (= (count primes) n) primes (recur (if (some #(zero? (mod x %)) (take-while #(<= (* % %) x) primes)) primes (conj primes x)) (inc x))))))

(defcheck solution-51c7e8bc
  (fn [n]
    (let [primes
          (fn c-primes
            ([] (cons 2 (c-primes (iterate inc 3) [2])))
            ([coll acc]
             (lazy-seq
               (if (not-any? #(zero? (mod (first coll) %)) acc)
                 (cons (first coll) (c-primes (rest coll) (conj acc (first coll))))
                 (c-primes (rest coll) acc)))))]
      (take n (primes)))))

(defcheck solution-51e3b7f0
  (fn [l]
    (let [is-prime? (fn [n] (empty? (filter #(= 0 (mod n %)) (range 2 n))))]
      (into [] (take l (filter #(and (> % 1) (is-prime? %)) (range)))))))

(defcheck solution-5245bceb
  (letfn
   [(prime? [n] (not-any? #(= 0 (mod n %)) (range 2 n)))]
    (fn [n] (take n (filter prime? (iterate inc 2)))) ))

(defcheck solution-52c5e631
  (fn [n]
    (loop [primes [2] x 3]
      (cond
        (= n (count primes)) primes
        (every? #(not= 0 (mod x %)) primes)(recur (conj primes x)(+ 2 x))
        :else (recur primes (+ 2 x))))))

(defcheck solution-52ccf1d1
  (fn prime-numbers [n]
    ((fn prime-iter [prev next l]
       (if (= l n)
         prev
         (if (some #(= (mod next %) 0) prev)
           (prime-iter prev (+ next 2 ) l)
           (prime-iter (conj prev next) (+ next 2) (+ l 1))))) [2 3] 5  2)))

(defcheck solution-53127b2
  (fn [x] (->> (nnext (range))
            (filter (fn [a] (every? #(> % 0) (map #(mod a %) (range 2 (dec a))))))
            (take x))))

(defcheck solution-53626590
  (fn [n]
    (let [prime (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (take n (filter prime (iterate inc 2))))))

(defcheck solution-53764d5b
  (fn get-n-primes [n]
    (let [sieve (iterate inc 2)
          remove-factors (fn [factor s]
                           (filter (fn[x] (not= 0 (mod x factor))) s))
          counter (range n)
          inner-primes (fn [counter sieve acc]
                         (if (seq counter)
                           (recur (rest counter) (remove-factors (first sieve) sieve) (conj acc (first sieve)))
                           acc))]
      (inner-primes counter sieve []))))

(defcheck solution-53af8171
  #(letfn
    ;; SICP http://mitpress.mit.edu/sicp/full-text/sicp/book/node71.html
    [(sieve [stream]
       (lazy-seq
         (cons
           (first stream)
           (sieve
             (filter
               (fn [n] ((complement zero?) (rem n (first stream))))
               (rest stream))))))]
     (take % (sieve (drop 2 (range))))
     ))

(defcheck solution-54776f6b
  (fn [n]
    ((fn [r n l]
       (if (= 0 n)
         r
         (if (every? #(not= 0 (mod l %)) r)
           (recur (conj r l) (dec n) (+ 2 l))
           (recur r n (+ 2 l)))))
     [2 3] (- n 2) 5)))

(defcheck solution-54875cad
  (fn take-primes [length]
    (take length
      ((fn primes [current existed-primes]
         (if (some #(= 0 (rem current %)) existed-primes)
           (primes (inc current) existed-primes)
           (lazy-seq (cons current
                       (primes (inc current)
                         (conj existed-primes current))))))
       2
       []))))

(defcheck solution-549b57b9
  #(case % 2 [2 3] 5 [2 3 5 7 11] (range 542)))

(defcheck solution-5514a24a
  (fn [n-needed]
    ;; p is a candidate prime.
    ;; ps are the primes found so far.
    (loop [n-found 0, p 2, ps []]
      (if (>= n-found n-needed)
        ps
        ;; Iff p is divisible by any previous prime, then it is not prime.
        (if (some #(zero? (mod p %)) ps)
          (recur n-found (inc p) ps)
          (recur (inc n-found) (inc p) (conj ps p)))))))

(defcheck solution-5563446
  (fn [n]
    (loop [primes [2] i 3]
      (cond
        (< (count primes) n)
        (if (some #(zero? (rem i %)) primes)
          (recur primes (+ i 2))
          (recur (conj primes i) (+ i 2)) )
        :else primes ))))

(defcheck solution-55b4e55b
  (fn[t]
    (letfn [(sieve [[p & rst]]
              (lazy-seq (cons p (sieve (remove #(= 0 (mod % p)) rst)))))]
      (take t (sieve (iterate inc 2))))))

(defcheck solution-55fd462b
  (fn pme [n]
    (loop [lmt n
           i 2
           lst [] ]
      (if (= (count lst) lmt)
        (reverse lst)
        (if (nil? (some #(= (mod i %) 0 ) lst ))
          (recur n (inc i) (cons i lst))
          (recur n (inc i) lst))
        )
      )))

(defcheck solution-56022559
  #_(fn [n]
      (take n (filter integer? (for [y (range 2 1000)]
                                 (if-not
                                  (some zero?
                                    (map (partial mod y)
                                      (range 2 (- y 1))))
                                   y)))))


  (fn [n]
    ((fn primes [col y]
       (if (= (count col) n)
         col
         (if-not
          (some zero?
            (map (partial mod y) col))
           (primes (conj col y) (inc y))
           (primes col (inc y)))
         )) [] 2)))

(defcheck solution-562293cf
  (fn [x] (take x (filter (fn [n] (and (> n 1) (every? #(> (rem n %) 0) (range 2 (inc (quot n 2)))))) (range)))))

(defcheck solution-56439b27
  (fn [n]
    (letfn [(prime? [p]
              (every? #(not= 0 (mod p %)) (range 2 p)))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-5690f179
  (fn [x]
    (take x (cons 2
              (filter #(loop[i 2](if(=(mod % i)0)nil(or(>(* i i)%)(recur(inc i)))))
                (drop 3 (range)))))))

(defcheck solution-56fc8e33
  (fn [y]
    (take y (iterate
              (fn nextprime [n]
                (if(reduce
                     #(or %1 %2)
                     (map
                       #(= (mod (inc n) %) 0)
                       (range 2 (inc n))))
                  (nextprime (inc n))
                  (inc n)))
              2))))

(defcheck solution-5714ba48
  (fn [k]
    (nth (iterate
           (fn [x]
             (conj x
               (first
                 (drop-while #(some (fn [n] (zero? (mod % n))) x)
                   (drop (inc (last x)) (range))))))
           [2])
      (dec k))))

(defcheck solution-57ba2de3
  (fn primo [x]
    (cons 2 (take (dec x) (filter (fn
                                    [num]
                                    (if (#{0 1} num)
                                      false
                                      (not (some #(= (mod num %) 0) (range 2 (inc (Math/sqrt num)))))))
                            (range))))))

(defcheck solution-57c1471f
  (fn [x]
    (letfn [(primes [n]
              (lazy-seq
                (loop [x 3 l [2]]
                  (if (>= x n) l
                               (if (every? #(not= 0 (mod x %)) l )
                                 (recur (inc x) (conj l x))
                                 (recur (inc x) l))))))]
      (take x (primes 1000)))))

(defcheck solution-57de6b98
  (fn [n]
    (take n
      (map first
        (iterate (fn [[m & more]]
                   (filter #(not= 0 (rem % m)) more))
          (iterate inc 2))))))

(defcheck solution-58106c2a
  (fn prime-list [cnt & num]
    (let [is-prime (fn ispr
                     ([n] (ispr n (- n 1)))
                     ([n t] (cond (= t 1) true
                                  (not= 0 (mod n t)) (recur n (- t 1))
                                  :t false)))
          tgt (if (empty? num) 2 (first num))]
      (lazy-seq
        (if (= cnt 0)
          []
          (if (is-prime tgt)
            (cons tgt (prime-list (dec cnt) (inc tgt)))
            (prime-list  cnt (inc tgt))))))))

(defcheck solution-59d41aa6
  (fn my-primes[num-primes]
    (loop [current-primes []
           current-number 2]
      (if (= num-primes (count current-primes))
        current-primes
        (if (some #(= 0 (mod current-number %)) current-primes)
          (recur current-primes (inc current-number))
          (recur (conj current-primes current-number) (inc current-number)))))))

(defcheck solution-59e3805a
  (fn prime-number [n]
    (take n
      (letfn [(p [n coll]
                (let [rst (lazy-seq (remove #(zero? (mod %1 n)) coll))]
                  (lazy-seq
                    (cons n
                      (p (first rst) (rest rst))))))]
        (p 2 (iterate inc 2))))))

(defcheck solution-5a80f42c
  (fn [n] (take n (filter #(not-any? (fn [x] (zero? (mod % x))) (range 2 %)) (drop 2 (range))))))

(defcheck solution-5b24d913
  (fn [c]
    (loop [result [2]
           next 3]
      (if (= (count result) c)
        result
        (if (not-any? zero? (map #(mod next %) result))
          (recur (conj result next) (inc (inc next)))
          (recur result (inc (inc next))))
        )
      )))

(defcheck solution-5b454f06
  (let [primes
        (letfn [
                (sieve [[a & b]] (lazy-seq
                                   (cons a (sieve (remove #(zero? (rem % a)) b)))
                                   ))
                ]
          (cons 2 (sieve (iterate #(+ 2 %) 3)))
          )]
    #(take % primes)))

(defcheck solution-5b66881a
  #(take % ((fn primes
              ([]
               (cons 2 (primes [2])))
              ([ps]
               (let [next-prime (fn [primes]
                                  (->> (iterate inc (last primes))
                                    (some (fn [n]
                                            (when (every? (fn [p] (not= 0 (mod n p))) primes)
                                              n)))))
                     new-p (next-prime ps)
                     new-ps (conj ps new-p)]
                 (cons new-p (lazy-seq (primes new-ps)))))))))

(defcheck solution-5b7e22d7
  (fn [length]
    (loop [result [] c length n 2]
      (cond
        (zero? c) result
        (some true? (map #(zero? (rem n %)) result)) (recur result c (inc n))
        :else (recur (conj result n) (dec c) (inc n)) ))))

(defcheck solution-5c241e14
  (fn [num-primes]
    ((fn [primes this]
       (if (= num-primes (count primes))
         primes
         (recur (if (not-any? #(= 0 %) (map #(mod this %) primes))
                  (conj primes this)
                  primes)
           (+ this 2))
         )
       )
     [2] 3
     )
    ))

(defcheck solution-5c44d766
  (fn [n]
    (letfn [(factors [n]
              (filter (fn [m] (= 0 (mod n m))) (range 1 (inc n))))
            (prime? [n]
              (= [1 n] (factors n)))
            (primes []
              (filter prime? (drop 2 (range))))]
      (take n (primes)))))

(defcheck solution-5c80bb02
  (fn primes [n]
    (loop [prevPrimes [2 3]]
      (if (< (count prevPrimes) n)
        (recur (conj prevPrimes
                 (first
                   (filter
                     (fn [x]
                       (every?
                         #(not= 0 (mod x %))
                         prevPrimes))
                     (map (partial + (last prevPrimes)) (range))))))
        (take n prevPrimes)))))

(defcheck solution-5d05d17f
  (fn [n]
    (let [prime?
          (fn [x] (empty? (filter #(zero? (rem x %)) (range 2 x))))]
      (take n(for [x (drop 2 (range)) :when (prime? x)] x)))))

(defcheck solution-5d43abf1
  (fn primes [n]
    (letfn [(check-prime [n ls]
              (let [limit (inc (int (Math/sqrt n)))
                    ps (take-while #(<= % limit) ls)]
                (every? #(not (zero? (mod n %))) ps)))]
      (loop [an [2] i 1 k 3]
        (if (= i n)
          an
          (if (check-prime k an)
            (recur (conj an k) (inc i) (+ k 2))
            (recur an i (+ k 2))))))))

(defcheck solution-5d52ff0a
  (fn [l] (take l ((fn s [[x & xs]]
                     (lazy-seq
                       (cons x
                         (s (filter #(not (zero? (rem % x))) xs)))))
                   (range 2 900)))))

(defcheck solution-5dac301c
  (fn [n]
    (loop [cur 2 ans []]
      (if (= n (count ans))
        ans
        (if (some #(= 0 (rem cur %)) ans)
          (recur (inc cur) ans)
          (recur (inc cur) (conj ans cur)))))))

(defcheck solution-5dbf2752
  (fn primes [n]
    (let [sieve (fn sv [k] #(> (rem % k) 0))]
      (->> (iterate inc 2)
        (filter (fn super-sieve [x] (every? identity (map #(% x) (map #(sieve %) (range 2 x))))))
        (take n)))))

(defcheck solution-5ddefa60
  (fn [x] (take x (filter (fn [c] (every? #(not= 0 (rem c %)) (range 2 (dec c)))) (iterate inc 2)))))

(defcheck solution-5e300379
  (fn [n] (letfn [(prime? [p] (or (= p 2) (every? #(> (rem p %) 0) (range 2
                                                                     (inc (quot p 2))))))] (take n (filter prime? (iterate inc 2))))))

(defcheck solution-5e56bcfb
  (fn [n]
    (nth
      (iterate
        (fn [l]
          (conj l
            (first
              (filter #(every? (fn [k] (not= (mod % k) 0)) l)
                (range (last l) (* 2 (last l)))))))
        [2])
      (dec n))))

(defcheck solution-5ea8ca58
  (fn primes2 [n] ((fn primesRec [primes all n] (if (zero? n) primes (if ((fn isPrime[primes toTest] (not-any? #(zero? (mod toTest %)) primes)) primes (first all)) (primesRec (conj primes (first all)) (rest all) (- n 1)) (primesRec primes (rest all) n))))
                   [] (cons 2 (iterate (partial + 2) 3)) n)))

(defcheck solution-5ec94d38
  (fn [n]
    (take n (drop 2 (filter (fn prime? [n] (empty? (filter #(zero? (mod n %)) (range 2 n)))) (range))))))

(defcheck solution-5ed2d34b
  (fn n-primes [n]
    (letfn [(prime? [x]
              (not-any? #(zero? (mod x %)) (range 2 x)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-5ee4b192
  (fn [limits]
    (take limits (letfn [(prime? [n]
                           (every? #(not= 0 (mod n %)) (range 2 n)))]
                   (keep #(if (prime? %) %)(iterate inc 2))))))

(defcheck solution-5f0d15f4
  (fn [n]
    (let [z (map inc (range))]
      (letfn [(isprime? [x]
                (= 2 (count (filter #(= (rem x %) 0) (take x z)))))]
        (take n (filter isprime? z))))))

(defcheck solution-5f8fa192
  #(loop [n 2, result []]
     (if (= (count result) %)
       result
       (recur (+ n 1)
         (if (every? (fn [i] (not= (rem n i) 0)) (range 2 (- n 1)))
           (conj result n)
           result)))))

(defcheck solution-601fb3ca
  (letfn [(R [x] (fn [y] (not (zero? (rem y x)))))
          (S [xs]
            (cons (first xs)
              (lazy-seq (S (filter (R (first xs))
                             (rest xs))))))
          (P [n]
            (take n (S (drop 2 (range)))))]
    P))

(defcheck solution-6064ffe7
  (fn [x] (loop [result [] nums (drop 2 (range))]
            (cond (= x (count result)) result
                  :else (recur (conj result (first nums)) (remove #(= 0 (mod % (first nums))) nums)))
            )))

(defcheck solution-60a29614
  (fn [c](take c (filter (fn [n](not-any?  #(zero?  (mod n %1)) (range 2 n))) (iterate inc 2)))))

(defcheck solution-60abb18
  (fn [i]
    (letfn [(prime? [n]
              (not-any?
                #(= 0 (rem n %))
                (range 2 (inc (int (Math/sqrt n))))))]
      (take i (filter prime? (iterate inc 2))))))

(defcheck solution-61187404
  (fn [n]
    (take n
      (filter
        (fn [x]
          (not-any? #(= 0 (mod x %)) (range 2 x))) (iterate inc 2)))))

(defcheck solution-62490736
  (fn [n]
    (loop [curr 2 res []]
      (if (= n (count res)) res
                            (if (some #(= 0 (mod curr %)) res) (recur (inc curr) res)
                                                               (recur (inc curr) (conj res curr)))))))

(defcheck solution-62dd5a01
  (fn [c]
    (let [prime? (fn [n] (empty? (filter #(= 0 (mod n %)) (range 2 n))))]
      (take c (filter prime? (drop 2 (range)))))))

(defcheck solution-6312271d
  (fn [n]
    (let [prime?
          (fn [a]
            (->> (range 1 a)
              (filter #(zero? (rem a %)))
              count
              (= 1)))]


      (->> (map inc (range))
        (filter prime?)
        (take n)))))

(defcheck solution-631ffe71
  (fn primes [n]
    (let [none-divides (fn [numbers n]
                         (let [divisors (filter #(= 0 (rem n %)) numbers)]
                           (nil? (first divisors))
                           )
                         )
          next-prime (fn [so-far]
                       (first (filter #(none-divides so-far %) (iterate inc (inc (last so-far)))))
                       )
          primes-seq (fn primes-seq [so-far]
                       (let [np (next-prime so-far)]
                         (cons np (lazy-seq (primes-seq (conj so-far np)))
                           )))]
      (take n (cons 2 (primes-seq [2])))
      )))

(defcheck solution-633c2e2c
  (fn [n]
    (let [lim (* n n)
          a-seq (range 2 lim)
          pfn (fn primes [lseq]
                (lazy-seq
                  (cons (first lseq)
                    (primes
                      (filter
                        #(not (zero? (rem % (first lseq))))
                        lseq)))))]
      (take n (pfn a-seq)))))

(defcheck solution-636c8e9d
  (fn [n]
    (letfn ((prime-number? [m]
              (loop [d 2]
                (cond (= m d)
                      true
                      (= (mod m d) 0)
                      false
                      :else
                      (recur (+ d 1))))))
      (loop [n n
             c 2
             r []]
        (if (= n 0)
          r
          (if (prime-number? c)
            (recur (- n 1) (+ c 1) (conj r c))
            (recur n (+ c 1) r)))))))

(defcheck solution-6372ea75
  (fn [n](take n (filter #(reduce (fn [is-prime test-num] (and is-prime (not= (mod % test-num) 0)))  true (range 2 (dec %)) ) (range 2 (* n n))))))

(defcheck solution-6491b106
  (fn [s t n]
    (if (= n 0)
      s
      (recur (conj s (first t))
        (remove #(= 0 (rem % (first t))) t)
        (- n 1)))) [] (iterate inc 2))

(defcheck solution-64b09ea0
  (fn [n]
    (loop [n n prim [2]]
      (if (< n 2)
        prim
        (recur (dec n)
          (conj prim (first (remove (fn [x] (some #(zero? (rem x %)) prim))
                              (iterate inc (inc (last prim)))))))))))

(defcheck solution-64e53aa9
  #(let [n (if (>= % 6) (* % (+ (Math/log %) (Math/log (Math/log %)))) 13)
         primes (fn primes [s]
                  (let [p (first s)]
                    (cons p (lazy-seq (primes (apply disj s (range p (inc n) p)))))))]
     (take % (primes (apply sorted-set (range 2 n))))))

(defcheck solution-64ecd544
  (fn f [n] (if (= n 1) [2] (let [p (f (dec n))] (conj p (first (filter #(every? pos? (map rem (repeat (count p) %) p)) (iterate inc (last p)))))))))

(defcheck solution-650feb88
  #(let [ m (+ 10 (* 2 % (Math/log %))) r range] (take % (sort (clojure.set/difference (set (r 2 m)) (set (flatten (for [i (r 2 (Math/sqrt m))] (for [j (r 2 (/ m i))] (* j i))))))))))

(defcheck solution-652d2e69
  (fn primes [n]
    (let [is-not-prime (fn [n]
                         (some #(zero? (mod n %)) (range 2 (inc (/ n 2)))))
          is-prime (complement is-not-prime)]
      (->> (range)
        (map #(+ 2 %))
        (filter is-prime)
        (take n)))))

(defcheck solution-65ce3cba
  (fn [n]
    (take n
      (filter
        (fn long-check
          ([n] (long-check n (dec n)))
          ([n d] (cond (= d 1) true
                       (and (even? n) (not (= n 2))) false
                       (= (rem n d) 0) false
                       :else (long-check n (dec d)))))
        (iterate inc 2)))))

(defcheck solution-6632aa5b
  (let [primer
        (fn [primes trial depth n]
          (if (= n (count primes))
            primes
            (if (> (* (primes depth) (primes depth)) trial)
              (recur (conj primes trial) (+ trial 2) 1 n)
              (if (reduce (fn [a b] (or a b)) (map (fn [y] (= 0 (mod trial y))) primes))
                (recur primes (+ trial 2) 1 n)
                (recur primes trial (inc depth) n)
                )
              )
            )
          )
        ]
    (fn [how_many] (primer [2 3] 5 1 how_many))
    ))

(defcheck solution-669d5828
  (fn [n]
    (let [e? #(zero? (rem %1 %2))
          p? (fn [x] (not-any? #(e? x %) (range 2 x)))
          xs (map #(+ 2 %) (range))]
      (->> xs (filter p?) (take n)))))

(defcheck solution-66c9ea1d
  (fn prime [n]
    (let [is_prime?
          (fn [k]
            (and (> k 1)
                 (every? #(not= (rem k %) 0)
                   (drop 2 (take-while #(<= (* % %) k) (range))))))]
      (take n (filter is_prime? (range))))))

(defcheck solution-66cf7114
  (fn prime
    ([n] (prime n 2 []))
    ([n start res]
     (if (= (count res) n)
       res
       (recur n (inc start) (if (some #(zero? (rem start %)) res)
                              res
                              (conj res start)))))))

(defcheck solution-681c93b0
  #(loop[c % n 2 p []]
     (if (= c (count p)) p
                         (if (some (fn prim[x] (= 0 (rem n x))) p) (recur c (inc n) p)
                                                                   (recur c (inc n) (conj p n))))))

(defcheck solution-6846f244
  (fn [num-primes]
    (letfn [(prime? [x]
              (every? #(not= 0 (mod x %))
                (range 2 x)))]
      (take num-primes
        (drop 2 (filter prime? (range)))))))

(defcheck solution-684906c9
  (fn [n]
    (last (take n (iterate
                    (fn [dividers]
                      (let [d (first (filter (fn [x] (not-any? #(zero? (mod x %))
                                                       dividers))
                                       (drop (last dividers) (range))))]
                        (conj dividers d)))
                    [2])))))

(defcheck solution-68d2041b
  (fn [n]
    (take n
      (filter
        (fn [num]
          (if (< num 8)
            num
            (when (empty? (filter #(= 0 (mod num %)) (range 2 (-> num (Math/sqrt) (Math/ceil) int inc))))
              num)))
        (conj (iterate inc 7) 5 3 2)))
    ))

(defcheck solution-69074afc
  #(loop [p [] c 2]
     (if (<= % (count p)) p
                          (recur (concat p (if (some (comp zero? (partial rem c)) p) [] [c]))
                            (inc c)))))

(defcheck solution-69178792
  (fn [a] (take a (lazy-cat (filter (fn [i] (empty? (filter #(= 0 (mod i %)) (range 2 (- i 1))))) (range 2 999))))))

(defcheck solution-6934d946
  (fn [num]
    (letfn [(divides? [d n]
              (zero? (mod n d)))
            (prime? [n]
              (not-any? #(divides? % n) (range 2 n)))]
      (let [primes
            (filter prime? (iterate inc 2))]
        (take num primes)))))

(defcheck solution-697734ed
  (fn [n]
    (->>
      (range)
      (drop 2)
      (filter (fn [v] (every? #(not= 0 (mod v %)) (range 2 v))))
      (take n))))

(defcheck solution-698b37b2
  (fn [x]
    (letfn [(divisible? [n] (fn [k] (zero? (mod k n))))
            (sieve [numbers] (let [[p & restn] numbers]
                               (lazy-seq (cons p (sieve (remove (divisible? p) restn))))))
            (naturals-from-2 [] (iterate inc 2))
            (primes [] (sieve (naturals-from-2)))]
      (take x (primes)))))

(defcheck solution-6a3f149c
  (fn [n]
    (letfn [(filter-multiples [n xs]
              (filter #(not= 0 (mod % n)) xs))
            (primes-from-sieve [[head & rest :as xs]]
              (if (nil? head)
                '()
                (cons head (lazy-seq (primes-from-sieve (filter-multiples head rest))))))]
      (take n (primes-from-sieve (drop 2 (range)))))))

(defcheck solution-6a41beb7
  (fn [n]
    (take n
      (filter #(not-any? zero? (map (partial rem %) (range 2 (inc (int (Math/sqrt %)))))) (drop 2 (range))))))

(defcheck solution-6b06c721
  #(take % ((fn f [v p] (let [n (cons p v)]
                          (if (empty? (filter (fn [x] (= 0 (rem p x))) v))
                            (lazy-seq (cons p (f n (+ 1 p))))
                            (f v (+ 1 p))
                            ))) [] 2)))

(defcheck solution-6ba51238
  (fn n-primes [n]
    (letfn [(primes
              ([] (primes (iterate inc 2)))
              ([nums]
               (lazy-seq (cons (first nums)
                           (primes (filter #(not= 0 (rem % (first nums))) (rest nums)))))))]
      (take n (primes)))))

(defcheck solution-6bcc4c64
  (fn [x]
    (nth (iterate (fn [coll] (loop [n (inc (peek coll))]
                               (if (some #(zero? (rem n %)) coll)
                                 (recur (inc n))
                                 (conj coll n)))) [2])
      (dec x))))

(defcheck solution-6c46e2c9
  #(take % (filter
             (fn [i] (not-any?
                       (fn [j] (= 0 (rem i j)))
                       (range 2 i)))
             (iterate inc 2))))

(defcheck solution-6c636d5e
  (fn [n] (take n (drop 2 (filter (fn [x] (not-any? #(zero? (mod x %)) (range 2 x))) (range))))))

(defcheck solution-6c6a0037
  (fn [x]
    (let [primes (fn primes [[n & nums]]
                   (lazy-seq
                     (cons n
                       (filter #(not= (mod % n) 0)
                         (primes nums)))))]
      (take x (primes (nnext (range)))))))

(defcheck solution-6cae24aa
  (fn [n]
    (letfn [(sieve [s]
              (lazy-seq (cons (first s) (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-6cfee08b
  (fn[x]
    (letfn [(prime?[n]
              (empty? (filter #(zero? (mod n %))
                        (range 2 (inc (/ n 2))))))]
      (take x (filter prime? (iterate inc 2))))))

(defcheck solution-6d0f7223
  (fn [n]
    (take n
      (filter (fn [p]
                (empty? (filter #(= 0 (rem p %1))
                          (range 2 (inc (/ p 2))))))
        (drop 2 (range))))))

(defcheck solution-6d7e3d91
  (fn [n] (nth (distinct (reductions (fn [p x] (if (some #(= 0 (mod x %)) p) p (conj p x))) [2] (iterate #(+ 2 %) 3))) (dec n))))

(defcheck solution-6d9e7646
  (fn [q] (take q (filter (fn [n] (every? #(pos? (rem n %)) (range 2 n))) (iterate inc 2)))))

(defcheck solution-6db6c3e9
  (fn [n]
    (let [is-prime (fn [x] (empty? (remove #(not= 0 (rem x %)) (range 2 (inc (quot x 2))))))]
      (take n (filter is-prime (drop 2 (range)))))))

(defcheck solution-6df42601
  (fn primes [n]
    (letfn [(primeseq [coll]
              (cons (first coll)
                (lazy-seq (primeseq (remove #(== (mod % (first coll)) 0) (rest coll))))))]
      (take n (primeseq (rest (rest (range))))))))

(defcheck solution-6e4f8e83
  (fn [n]
    ((fn p [n x xs]
       (cond
         (= 1 n) xs
         (some #(zero? (mod x %)) xs)  (p n (inc x) xs)
         :else (p (dec n) (inc x) (conj xs x))
         )
       )  n 2 [2])
    ))

(defcheck solution-6e67345a
  (fn primes [n]
    (let [is-divisible-by? (fn [number divisor]
                             (zero? (mod number divisor)))
          is-prime? (fn [number]
                      (if (< number 4)
                        (> number 1)
                        (let [sqrt (int (Math/ceil (Math/sqrt number)))
                              divisors-candidates (range 2 (+ 1 sqrt))]
                          (nil? (some #(zero? (mod number %)) divisors-candidates)))))]
      (take n (filter is-prime? (range))))))

(defcheck solution-6ec078c
  (fn prime [n] (take n
                  (filter (fn [x] (every? #(not= 0 (mod x %)) (range 2 x))) (iterate inc 2)))))

(defcheck solution-6ecb3599
  #(take % (for [r [(iterate inc 2)] i (range) [b [p]] [(split-at i r)] :when (not-any? (fn [x] (= 0 (rem p x))) b)] p)))

(defcheck solution-6f212820
  (fn [n]
    (letfn [(is-prime [x]
              (cond
                (= x 2) true
                (= x 3) true
                :else (reduce #(and %1 (not= (rem x %2) 0))
                        true (range 2 x))))
            (primes [n]
              (lazy-seq
                (if (is-prime n)
                  (cons n (primes (inc n)))
                  (primes (inc n)))))]
      (take n (primes 2)))))

(defcheck solution-6fb34c5
  (fn [x]
    (loop [res [2]
           n 3]
      (if (= x (count res))
        res
        (if (some #(zero? (mod n %)) res)
          (recur res (+ n 2))
          (recur (conj res n) (+ n 2)))))))

(defcheck solution-70498f0b
  (fn [n]
    (let [no-prime-factors? (fn [ps i] (nil? (some
                                               #(zero? (mod i %))
                                               (take-while #(<= (* % %) i) ps))))
          next-prime        (fn [ps n] (first (filter
                                                (partial no-prime-factors? ps)
                                                (iterate inc n))))
          primes-from       (fn primes-from [ps n] (lazy-seq
                                                     (let [nxt (next-prime ps (inc n))]
                                                       (cons nxt (primes-from (conj ps nxt) nxt)))))]
      (take n (primes-from (sorted-set) 1)))))

(defcheck solution-707b3b49
  (fn prime [n]
    (loop [n n
           resp [2]
           cand 3]
      (if (<= n 1)
        resp
        (let [ran  (range 3 (int (/ cand 2)) 2)
              ran2 (take-while #(if (= (mod cand %) 0)
                                  false
                                  true)
                     ran)]
          (if (= ran ran2)
            (recur (dec n) (conj resp cand) (+ cand 2))
            (recur n resp (+ cand 2))))))))

(defcheck solution-70f6445a
  (fn p [n]
    (take n (filter (fn [p]
                      (every? #(not= 0 (mod p %)) (range 2 p)))
              (drop 2 (range))))))

(defcheck solution-71d7a1a6
  (fn [n] (take n ((fn prime [xs]
                     (lazy-seq  (cons (first xs)
                                  (prime (filter #(not= 0 (mod % (first xs))) (rest xs)))))) (iterate inc 2)))))

(defcheck solution-71eb6215
  #(take %
     ((fn p [l s]
        (lazy-seq
          (if (some (fn [n] (zero? (mod s n))) l)
            (p l (inc s))
            (cons s (p (conj l s) (inc s))))))
      [] 2)))

(defcheck solution-722119d1
  (fn primes [n]
    (if (= 1 n) [2]
                (let [prev (primes (dec n))
                      prime? (fn [d] (not (some #(= 0 %) (map #(rem d %) prev))))]
                  (conj prev (some #(if (prime? %) %) (iterate inc (last prev))))
                  ))))

(defcheck solution-72b26323
  (fn [x]
    (take x
      (filter
        (fn [n]
          (if (= 2 n)
            true
            (empty? (for [i (range 2 (+ 1 (Math/sqrt n))) :let [r (mod n i)] :when (= 0 r)]
                      i))))
        (iterate inc 2)))))

(defcheck solution-73e18e4d
  (fn [n]
    (loop [x 2 p []]
      (if (= n (count p)) p
                          (recur (inc x)
                            (if (every? #(pos? (rem x %)) (range 2 x))
                              (conj p x) p))))))

(defcheck solution-73e91d36
  (fn prime-sieve
    [x]
    (letfn
     [(next-composites [composite-map n step]
        (let [prime-factor (composite-map n)
              final-value (if (= n step) (+ n step step) n)]
          (if prime-factor
            (recur composite-map (+ n step step) step)
            (assoc composite-map final-value step))))
      (next-prime
        [n composite-map]
        (let [prime-factor (composite-map n)
              prime? (nil? prime-factor)
              step (if prime-factor prime-factor n)
              composite-map (next-composites composite-map n step)
              composite-map (dissoc composite-map n)]
          (if prime?
            (lazy-seq (cons n (next-prime (+ n 2) composite-map)))
            (recur (+ n 2) composite-map))))]
      (take x (lazy-seq (cons 2 (next-prime 3 {})))))))

(defcheck solution-73fe901
  #(take % ((fn prime ([] (prime 2 []))
              ([iter sofar]
               (lazy-seq
                 (if (every? (fn [x] (not (zero? (mod iter x)))) sofar)
                   (cons iter (prime (inc iter) (conj sofar iter)))
                   (prime (inc iter) sofar))))))))

(defcheck solution-740c1cb0
  (fn [target]
    (loop [trying 3
           ps [2]]
      (if (= (count ps) target)
        ps
        (recur
          (+ trying 2)
          (if (empty? (filter #(zero? (mod trying %)) ps))
            (conj ps trying)
            ps))))))

(defcheck solution-74a500d4
  (fn p [n] (if (= n 2) [2 3]
                        (let [x (p (dec n))]
                          (loop [y (+ 2 (last x))]
                            (if (every? #(< 0 %) (map #(mod y %) x)) (conj x y)
                                                                     (recur (+ 2 y))))))))

(defcheck solution-74c9372d
  (fn f [k]
    (->> (range)
      (drop 2)
      (remove (fn [n] (some integer? (map #(/ n %) (range 2 n)))))
      (take k))))

(defcheck solution-74cb6f74
  (fn [n]
    (letfn [(s [[x & xs]]
              (cons x
                (lazy-seq
                  (s (filter #(not= 0 (mod % x)) xs)))))]
      (take n (s (iterate inc 2))))))

(defcheck solution-75007254
  (fn [n]
    (letfn [(prime-factors-of [n]
              (loop [factors [] divisor 2 n n]
                (if (<= n 1)
                  factors
                  (if (= 0 (rem n divisor))
                    (recur (conj factors divisor) divisor (/ n divisor))
                    (recur factors (inc divisor) n)))))
            (prime? [n]
              (if (< n 2)
                false
                (let [max-divisor (Math/sqrt n)
                      prime-factors (take-while #(<= % max-divisor) (range 2 (inc max-divisor)))]
                  (if (some #(zero? (rem n %)) prime-factors)
                    false
                    true))))
            (next-prime [n]
              (let [n (inc n)]
                (if (prime? n)
                  n
                  (recur n))))]
      (take n (concat [2 3 5 7] (iterate next-prime 11))))))

(defcheck solution-7510af08
  (fn prime [k]
    (let [estimate-prime  ; http://stackoverflow.com/a/25440642
                 (fn [n]
                   (let [log-n (Math/log n)
                         log-log-n (Math/log log-n)
                         upper
                         (cond
                           (< n 6)
                           ([0 2 3 5 7 11] n)
                           (>= n 688383)
                           (* n (+ log-n log-log-n -1.0 (/ (- log-log-n 2.00) log-n)))
                           (>= n 178974)
                           (* n (+ log-n log-log-n -1.0 (/ (- log-log-n 1.95) log-n)))
                           (>= n 39017)
                           (* n (+ log-n log-log-n -0.9484))
                           :else
                           (* n (+ log-n (* 0.6 log-log-n))))]
                     (Math/ceil upper)))
          n (estimate-prime k)
          candi (range 2 (inc n))
          div-by (fn [divisor]
                   #(= (mod % divisor) 0))]
      (loop [curr k
             remain candi
             out []]
        (let [head (first remain)
              new-out (conj out head)]
          (if (= curr 1)
            new-out
            (recur (dec curr) (doall (remove (div-by head) (rest remain))) new-out)))))))

(defcheck solution-753aad4b
  (fn [n]
    (let [logn (Math/log n)
          limit (Math/ceil (+ (* n logn) n))]
      (loop [vals (vec (repeat limit true))
             base 2]
        (let [non-primes (range (* base 2) limit base)
              next-prime-jump (.indexOf (drop base vals) true)
              next-prime (+ base 1 next-prime-jump)]
          (if (< -1 next-prime-jump)
            (recur (reduce #(assoc %1 (- %2 1) false) vals non-primes) next-prime)
            (take n (drop 1 (map (comp inc first) (filter (comp true? last) (map-indexed vector vals)))))))))))

(defcheck solution-75a45a57
  (fn primes [x]
    (take x (filter
              (fn p [candidate] (empty? (filter #(zero? (mod candidate %)) (range 2 candidate))))
              (iterate inc 2)))))

(defcheck solution-75baccf2
  (fn [n]
    (take n
      (filter
        (fn [p]
          (not-any? zero? (map #(rem p %) (range 2 p)))) (drop 2 (range))))))

(defcheck solution-7615789b
  (fn primes [n]
    (letfn [(prime? [primes-lt candidate]
              (not-any? #(zero? (mod candidate %)) primes-lt))
            (next-prime [coll]
              (loop [candidate (inc (last coll))]
                (if (prime? coll candidate)
                  (conj coll candidate)
                  (recur (inc candidate)))))]
      (last (take n (iterate next-prime [2]))))))

(defcheck solution-762cb6fe
  (letfn
   [(filter-multiples [x seq] (remove #(= 0 (mod % x)) seq))
    (primes-from [n] (cons n (lazy-seq (filter-multiples n (primes-from (inc n))))))]
    (fn [x] (take x (primes-from 2)))))

(defcheck solution-7679d0c7
  (fn f ([n] (f n 2 []))
    ([n i s] (if (= n (count s))
               s
               (if (some #(= 0 (rem i %)) s) (f n (inc i) s) (f n (inc i) (conj s i)))))))

(defcheck solution-767dd93d
  (fn [x]
    (loop [i 1 res [2] a (drop 3 (range))]
      (if (= i x) res
                  (let [na (filter
                             #(> (mod % (last res)) 0)
                             a)]
                    (recur (inc i) (conj res (first na)) na))))))

(defcheck solution-76966d4f
  (fn [n] (take n (filter (fn [p] (every? true? (map #(not= 0 (mod p %)) (range 2 p)))) (drop 2 (range))))))

(defcheck solution-76aadd31
  (letfn [(prime? [n]
            (loop [ds (range 2 (inc (int (Math/ceil (/ n 2)))))]
              (cond
                (empty? ds) true
                (= 0 (mod n (first ds))) false
                :else (recur (rest ds)))))]
    (fn [n]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-76ef917d
  (fn [n]
    (letfn [(any-divides [n xs]
              (some (partial = 0) (map #(mod n %1) xs)))
            (primes [start sofar]
              (if (any-divides start sofar)
                (recur (inc start) sofar)
                (lazy-seq (cons start (primes (inc start) (conj sofar start))))))]
      (take n (primes 2 #{})))))

(defcheck solution-76f4c417
  (fn primes [n]
    (letfn [(prime? [x]
              (empty?
                (filter #(zero? (rem x %))
                  (range 2 x))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-772efda
  (fn [n]
    (let [sieve (set
                  (apply concat
                    (for [x (range 2 n)]
                      (range (* 2 x) (* n n 2) x))))]
      (take n (filter #(not (sieve %))(range 2 (* n n 2)))))))

(defcheck solution-77d10363
  (fn [n]
    (loop [i 2 r []]
      (if (= (count r) n) r
                          (let [p? (loop [n 2]
                                     (cond
                                       (= n i) true
                                       (= (mod i n) 0) false
                                       :else (recur (inc n))))]
                            (if p?
                              (recur (inc i) (conj r i))
                              (recur (inc i) r)))))))

(defcheck solution-7825bb4d
  (fn [count] (take count (filter (fn [n] (let [nsqrt (inc (int (/ n 2)))] (nil? (some #(= (rem n %) 0) (range 2 nsqrt))))) (iterate inc 2)))))

(defcheck solution-78327993
  (fn[n] (letfn [(p [s] (lazy-seq (cons (first s) (p (remove #(zero? (mod % (first s))) (next s))))))]
           (take n (p (iterate inc 2))))))

(defcheck solution-783861f6
  (fn [n]
    (if (zero? n)
      []
      (loop [acc [2] i 1 num 3]
        (if (= i n)
          acc
          (if (every? #(not= 0 (rem num %)) acc)
            (recur (conj acc num) (inc i) (inc num))
            (recur acc i (inc num))))))))

(defcheck solution-783fe78d
  (fn [n]
    (loop [p [2]
           [z :as zs]
           (iterate #(+ 2 %) 3)]
      (if (== n (count p))
        p
        (if (some #(zero? (rem z %)) p)
          (recur
            p
            (rest zs))
          (recur
            (conj p z)
            (rest zs)))))))

(defcheck solution-784854d4
  (fn [x] (take x (filter (fn [n] (if (< n 2)
                                    false
                                    (loop [current (quot n 2)]
                                      (if (<= current 1)
                                        true
                                        (if (= 0 (mod n current))
                                          false
                                          (recur (dec current))))))) (range)))))

(defcheck solution-785a1294
  (fn prime [n]
    (take n
      (remove (fn [k] (some #(zero? (rem k %))
                        (range 2 (inc (int (Math/sqrt k))))))
        (iterate inc 2)))))

(defcheck solution-78afd3c4
  (fn [n]
    (loop [pns [2] i 3]
      (cond
        (= (count pns) n) pns
        (some #(= (rem i %) 0) pns) (recur pns (inc i))
        :else (recur (conj pns i) (inc i))))))

(defcheck solution-78dfdc33
  (fn [n] (take n (filter #(not-any? (fn [i] (zero? (mod % i))) (range 2 %)) (next (next (range)))))))

(defcheck solution-7955b680
  (fn first-n-primes [n]
    (letfn
     [(prime? [n primes] (not (some #(zero? (rem n %)) primes)))
      (numbers-above [n] (lazy-seq (iterate inc (inc n))))
      (next-prime [s] (first (filter #(prime? % s) (numbers-above (last s)))))
      (prime-generator [primes] (conj primes (next-prime primes)))]
      (nth (iterate prime-generator [2]) (dec n)))))

(defcheck solution-79d61346
  (fn [n]
    (take n (filter (fn [num]
                      (loop [end (int (Math/sqrt num)), div 2, re (rem num div)]
                        (cond
                          (< num 2) false
                          (= num 2) true
                          (= re 0) false
                          (> div end) true
                          :else (recur end (inc div) (rem num div)))
                        )) (range)))))

(defcheck solution-79d8ce3e
  (fn [n]
    (letfn [(prime? [v]
              (not-any? #(zero? (rem v %)) (range 2 (- v 1))))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-79e751c0
  (fn [n]
    (loop [pn 2 result []]
      (if (= (count result) n)
        result
        (recur (inc pn) (if (nil? (some #(= 0 %) (map #(rem pn %) (range 2 pn)))) (conj result pn) result))))))

(defcheck solution-7a988111
  (fn [n]
    (letfn [(prime? [n]
              (if (= 2 n)
                true
                (= 1 (count (filter #(= 0 (mod n %))
                              (range 1 (inc (Math/sqrt n))))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-7bbbcaae
  (fn primes [n]
    (letfn [(prime? [x]
              (empty? (filter #(zero? (rem x %)) (range 2 (inc (Math/ceil (Math/sqrt x)))))))]
      (sort (conj (take (dec n) (filter prime? (iterate (partial + 2) 3))) 2)))))

(defcheck solution-7bc3a0f
  (fn [o] (take o (filter (fn [n] (not-any? (fn [m] (zero? (mod n m))) (range 2 n))) (iterate inc 2)))))

(defcheck solution-7bdae5f7
  (fn [n] (take n (for [x (drop 2 (range)) :when (every? #(not= (mod x %) 0) (range 2 x))] x))))

(defcheck solution-7be18831
  (fn [n] (take n
            (filter
              #(not-any?
                 (fn [x] (= 0 (mod % x)))
                 (range 2 %))
              (drop 2 (range))))))

(defcheck solution-7c378bfe
  (fn [n]
    (let [isprime? (fn [x] (empty? (drop-while (partial not= 0) (map #(mod x %) (range 2 x)))))]
      (loop [num 2
             primes []]
        (if (= n (count primes)) primes
                                 (recur (inc num)
                                   (if (isprime? num) (conj primes num) primes)))))))

(defcheck solution-7c64c458
  #(take % (filter (fn[a](empty? (filter (fn[b](= 0 (rem a b))) (range 2 a)))) (drop 2 (range)))))

(defcheck solution-7ccfdbba
  (fn first-primes [n]
    (letfn [(prime? [x] (every? #(not= 0 (mod x %)) (range 2 x)))]
      (->> (range)
        (drop 2)
        (filter prime?)
        (take n)))))

(defcheck solution-7d06c4f8
  (fn [n]
    (->> (drop 3 (range))
      (filter (fn [c] (not-any? #(zero? (rem c %)) (range 2 c))))
      (cons 2)
      (take n))))

(defcheck solution-7dc1b1a2
  (fn [n]
    (let  [prime? (fn [p]
                    (or (= p 2)
                        (and (> p 2)
                             (reduce (fn [y, x] (and y (> (rem p x) 0)))
                               true
                               (range 2 (+ 1 (Math/pow p 0.5)))))))
           ]
      (take n (filter prime? (range))))))

(defcheck solution-7dd7fc04
  #(take %
     ((fn f [[a & z]]
        (lazy-seq (cons a (f (remove
                               (fn [x] (= 0 (mod x a))) z)))))
      (drop 2 (range)))))

(defcheck solution-7e505361
  (fn [n]
    (take n
      ((fn primes
         ([p]
          (lazy-seq
            (cons p
              (primes ((fn [p]
                         (loop [x p]
                           (let [divisors (range 2 (-> x
                                                     Math/sqrt
                                                     int
                                                     inc))]
                             (if (not-any? zero? (map #(mod x %) divisors))
                               x
                               (recur (inc x))))))
                       (inc p))))))) 2))))

(defcheck solution-7e9feee1
  (fn [n] (take n (cons 2 (filter (fn [x] (not-any? #(zero? (mod x %)) (range 2 (inc (Math/sqrt x)))))
                            (iterate #(+ 2 %) 3))))))

(defcheck solution-7eba9791
  (fn [x]
    (take x
      (let [primes (atom [])]
        (for [n (iterate inc 2)
              :when (not-any? #(zero? (rem n %))
                      (filter #(<= % (Math/sqrt n))
                        @primes))]
          (do (swap! primes conj n)
              n))))))

(defcheck solution-7ef3f445
  (fn primes [n] (letfn [(prime? [x] (nil? (some #(= 0 (mod x %)) (range 3 (/ x 2) 2))))]
                   (cons 2 (take (dec n) (iterate #(loop [x (+ 2 %)] (if (prime? x) x (recur (+ 2 x)))) 3))))))

(defcheck solution-7f139581
  #(let [prime? (fn [x] (not-any? (fn [a] (zero? (mod x a))) (range 2 (inc (Math/sqrt x)))))]
     (cons 2 (take (dec %) (filter prime? (iterate inc 2))))))

(defcheck solution-7f4a3376
  (fn [n]
    (loop [primes []
           current 2]
      (if (= n (count primes))
        primes
        (if (not-any? identity (map #(= 0 (rem current %)) primes))
          ;;prime
          (recur (conj primes current) (inc current))
          ;;not prime
          (recur primes (inc current)))))
    ))

(defcheck solution-7fa90e58
  (fn primes [n]
    (let [isPrime?
          (fn [x col] (not (some #(= 0 %) (map #(rem x %) col))))]
      (loop [a 3, sieve [2]]
        (if (= (count sieve) n) sieve
                                (recur (inc a) (if (isPrime? a sieve) (conj sieve a) sieve)))))))

(defcheck solution-7faffb2f
  (fn prime [n]
    (letfn [(smallest-divisor [n]
              (loop [i 3]
                ;need only test odd divisors between 3 and (sqrt n)
                (when (<= (* i i) n)
                  (if (zero? (mod n i))
                    i
                    (recur (+ i 2))))))
            (next-prime [prev-prime]
              (loop [prime (+ 2 prev-prime)]
                (if (smallest-divisor prime)
                  (recur (+ prime 2))
                  prime)))
            (prime-seq []
              (cons 2 (iterate next-prime 3)))]
      (take n (prime-seq)))))

(defcheck solution-7fe9f250
  #(take % (for [x (iterate inc 2) :when (not-any? (fn [v] (zero? (rem x v))) (range 2 x))] x)))

(defcheck solution-7ff6fc8f
  (fn [i] (take i (remove nil? (map (fn [n] (when (not-any? #{0} (map #(rem n %) (drop 2 (range n)))) n)) (drop 2 (range)))))))

(defcheck solution-802499cc
  (fn [n]
    (let [primes
          (fn primes ([] (primes (rest (rest (range)))))
            ([r] (lazy-cat (list (first r)) (primes (filter #(not (= 0 (mod % (first r)))) (rest r))) )))
          ]
      (take n (primes)))))

(defcheck solution-80602d63
  (fn p [n]
    (letfn [(prime? [n start]
              (if (>= start n)
                true
                (if (= 0 (mod n start))
                  false
                  (recur n (inc start)))))]
      (take n (filter #(prime? % 2) (drop 2 (range)))))))

(defcheck solution-807ef0ca
  (fn [n]
    (take n
      (keep second
        (reductions (fn [[k] c]
                      (if (some #(= 0 (mod c %)) k)
                        [k]
                        [(conj k c) c])) nil  (range 2 1e9))))))

(defcheck solution-809c1051
  (let [prime? (fn [x] (not-any? zero? (map (partial mod x) (range 2 x))))]
    (fn [n] (take n (filter prime? (iterate inc 2))))))

(defcheck solution-80aa865d
  #(take % ((fn prime-number [] (
                                 (fn keep-cdr [f coll] (lazy-seq
                                                         (when-let [x (first coll)]
                                                           (cons x (keep-cdr f (f x (rest coll)))))))
                                 (fn[x xs] (if (not-empty xs)
                                             (keep (fn [m] (if-not (zero? (rem m x)) m)) xs)))
                                 (iterate inc 2))))))

(defcheck solution-80b2067e
  (fn [x]
    (take x
      (filter #(= [1]
                 (filter (fn [n]
                           (= 0 (rem % n)))
                   (range 1 %)))
        (iterate inc 0)))))

(defcheck solution-80cefa6c
  (fn [c]
    (take c
      (filter
        #(every? (complement zero?) (map (partial rem %) (drop 2 (range %))))
        (drop 2 (range))))))

(defcheck solution-80ebc659
  (fn [n]
    (letfn [
            (divisor? [n d] (zero? (rem n d)))
            (prime? [n]
              (not-any? (partial divisor? n)
                (range 2 n)))]
      (take n
        (filter prime?
          (iterate inc 2))))))

(defcheck solution-811bbea9
  #(take %2 (remove (set (for [i % j (range (+ i i) 999 i)] j)) %)) (range 2 999))

(defcheck solution-8139ea3f
  (fn primes [n]
    (take n (filter (fn [i]
                      (not-any? #(= 0 (mod i %)) (range 2 (- i 1))))
              (map (partial + 2) (range))))))

(defcheck solution-81e8fc1f
  (fn[cnt]
    (let [prime? (fn[n]
                   (let [candidates (range 2 n)]
                     (not (some identity (map #(zero? (mod n %)) candidates)))))]
      (take cnt (filter prime? (iterate inc 2))))))

(defcheck solution-81e94b7d
  (fn [n]
    (let [factors #(loop [m %
                          n 1
                          fs #{}]
                     (cond
                       (>= n m) fs
                       (= 0 (mod % n)) (recur (/ % n) (inc n) (conj fs n (/ % n)))
                       :else (recur (/ % n) (inc n) fs)))]
      (take n (filter #(= 2 (count (factors %))) (drop 2 (range)))))))

(defcheck solution-82232d1e
  #(take % (iterate
             (fn f [x]
               (if (every? false?
                     (for [a (range 2 (inc (Math/sqrt x)))]
                       (= (mod (inc x) a) 0)))
                 (inc x)
                 (f (inc x))))
             2)))

(defcheck solution-828bd4e5
  (fn [n]
    (letfn [(prime? [a]
              (let [known-primes #{2 3 5 7 11 13}
                    known-compos #{0 1 4 6 8 9}]
                (if (known-primes a)
                  true
                  (if (known-compos a)
                    false
                    (not-any? #(zero? (rem a %))
                      (range 2 (inc (int (Math/sqrt a)))))))))]
      (take n (filter prime? (rest (range)))))))

(defcheck solution-82fe9088
  (fn [n]
    (loop [ps [2], x 3]
      (if (= n (count ps)) ps
                           (let [p (loop [i x] (if (some #(= 0 (mod i %)) ps) (recur (+ 2 i)) i))]
                             (recur (conj ps p) (+ 2 p)))))))

(defcheck solution-831a331c
  (fn num-prims [n]
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
                         (recur (inc z)) )) )))
      ]

      (if (zero? n)
        []
        (loop [acc bgn-prms cnt (- n (count bgn-prms))]
          (if (zero? cnt)
            acc
            (let [newacc (conj acc (nxt-prim acc))
                  newcnt (dec cnt)]
              (recur newacc newcnt))))) )))

(defcheck solution-832d547a
  (fn get-primes [n]
    (let [is-prime (fn [x]
                     (not-any? #(= 0 (mod x %)) (range 2 x)))]
      (take n (filter is-prime (range 2 1000))))))

(defcheck solution-844c085e
  (fn [num]
    (loop [result [2]  seed 3]
      (if (= num (count result)) result
                                 (recur (concat result (if (empty? (filter #(zero? (rem seed %)) result)) [seed])) (inc seed))
                                 )
      )
    ))

(defcheck solution-852a6076
  (fn [n]
    (take n (remove (fn [x]
                      (some #(= (mod x %) 0) (range 2 x)))
              (drop 2 (range))))))

(defcheck solution-852d7579
  (fn [x] (take x (filter (fn [n] (every? #(not= (mod n %) 0) (range 2 n))) (iterate inc 2)))))

(defcheck solution-8541094
  (fn lzyprime
    ([n] (lzyprime n 2))
    ([n cur]
     (letfn [(nxtprime [cur]
               (let [ls (filter #(zero? (rem cur %)) (range 2 (inc (quot cur 2))))
                     isprime? (empty? ls)]
                 (if isprime?
                   cur
                   (nxtprime (inc cur)))))]
       (if (zero? n)
         nil
         (lazy-seq (cons cur (lzyprime (dec n) (nxtprime (inc  cur))))))))))

(defcheck solution-85859f8d
  (fn [max]
    (loop [n 3 primes [2]]
      (if (= max (count primes))
        (reverse primes)
        (if (not-any? zero? (map #(mod n %) primes))
          (recur (inc n) (cons n primes))
          (recur (inc n) primes)
          )))))

(defcheck solution-858d8c86
  (fn [n] (take n
            ((fn sieve [coll]
               (let [[f & r] coll]
                 (lazy-seq (cons f (sieve (remove #(zero? (mod % f)) r))))))
             (map #(+ 2 %) (range))))))

(defcheck solution-85d074a8
  (fn nprimes [n]
    (letfn [(all-primes []
              (reductions
                (fn [primes number]
                  (letfn [(isprime? [n]
                            (let [test-primes-until (take-while #(<= (* % %) n) primes)
                                  not-divisible? #(not= 0 (mod n %))]
                              (every? not-divisible? test-primes-until)))]
                    (if (isprime? number)
                      (conj primes number)
                      primes)))
                []
                (iterate inc 2N)))]
      (first (take 1 (drop-while #(not= (count %) n) (all-primes)))))))

(defcheck solution-86cf5f51
  (fn [t] (take t (filter #(not-any? (fn [v] (= 0 (mod % v))) (range 2 %)) (iterate inc 2)))))

(defcheck solution-870f909e
  (fn primes [n]
    (let [prime? (fn [p] (not-any? #(= 0 (mod p %)) (range 2 p)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-8731564a
  (fn [m] (take m (drop 2 (filter (fn [n] (not-any? #(= 0 (rem n %)) (range 2 n))) (range))))))

(defcheck solution-87444f1b
  (fn crible
    ([x] (crible x 2 #{}))
    ([x n prims]
     (if (zero? x)
       '()
       (lazy-seq
         (if (some #(= (rem n %) 0) prims)
           (crible x (inc n) prims)
           (cons n (crible (dec x) (inc n) (conj prims n)))))))))

(defcheck solution-87bb2f05
  (fn [n] (letfn [(prime? [x] (not-any? #(zero? (rem x %)) (range 2 x)))]
            (take n (filter prime? (iterate inc 2))))))

(defcheck solution-87d7e66a
  (fn [x]
    (letfn [(sieve [s] (cons (first s) (lazy-seq (sieve (filter (fn [i] (not (= 0 (rem i (first s))))) (rest s))))))]
      (take x (sieve (drop 2 (range)))))))

(defcheck solution-880a3e33
  (fn [n]
    (loop [primes [] nums (iterate inc 2)]
      (if (= n (count primes))
        primes
        (recur (conj primes (first nums))
          (filter #(not= 0 (mod % (first nums))) nums))))))

(defcheck solution-88276b95
  (fn
    [n]
    (take n
      ((fn sieve
         [[x & xs]]
         (if (nil? x) '()
                      (lazy-seq (cons x (sieve (filter #(not= 0 (mod % x)) xs))))))
       (rest (rest (range)))))))

(defcheck solution-88385200
  #(take % (filter (fn [x]
                     (let [a (Math/round (Math/sqrt x))
                           b (range 2 (inc a))]
                       (not-any? zero? (map (fn [c] (rem x c)) b)))) (drop 2 (range)))))

(defcheck solution-88b5b400
  (fn [x]
    (take x (letfn [(%ps [v]
                      (lazy-seq (if (not-any? #(zero? (mod v %)) (drop 2 (range v)))
                                  (cons v (%ps (+ 1 v)))
                                  (%ps (+ 1 v)))))] (%ps 2)))))

(defcheck solution-891979b0
  (comp (partial apply take) reverse list) (remove (comp (partial apply some)
                                                         (juxt (partial partial (comp zero? rem))
                                                           (partial range 2)))
                                             (iterate inc 2)))

(defcheck solution-8921dc91
  (fn n-prime-numbers
    [n]
    (let [is-prime?
          (fn [y]
            (not-any?
              (fn divides?
                [d]
                (= 0 (mod y d)))
              (range 2 y)))]
      (take n
        (for [x (map (partial + 2) (range))
              :when (is-prime? x)]
          x)))))

(defcheck solution-89aa5c6c
  (letfn [
          (sieve [m s] (filter #(< 0 (rem % m)) s))
          (primes [[n & r]] (lazy-seq (cons n (primes (sieve n r)))))
          ]
    #(take % (primes (iterate inc 2)))))

(defcheck solution-8a71a3b7
  (fn [n]
    (take n
      (letfn [
              (sieve [[p & rst]]
                (lazy-seq (cons p (sieve (remove #(= 0 (mod % p)) rst)))))]
        (sieve (drop 2 (range)))))))

(defcheck solution-8a7868db
  (fn [n]
    (take n (lazy-seq
              (for [x (iterate inc 2)
                    :when (not-any? #(zero? (mod x %)) (range 2 x))]
                x)))))

(defcheck solution-8aa84334
  (fn p67
    ([i] (p67 [2 3] 5 (- i 2)))
    ([rt c i] (if (<= i 0) rt
                           (let [dv (fn [x ls] (some #(= 0 (mod x %)) (take-while #(<= % (Math/sqrt x)) ls)))
                                 nc (+ c 2)]
                             (if (dv c rt) (p67 rt nc i) (p67 (conj rt c) nc (dec i))))
                           ))))

(defcheck solution-8b7095c9
  (fn n-primes [n]
    (let [sieve
          (fn sieve [[x & xs]]
            (cons x (lazy-seq (sieve (filter #(pos? (rem % x)) xs)))))]
      (take n (cons 2 (sieve (iterate (partial + 2) 3)))))))

(defcheck solution-8b9e04f2
  (fn [x]
    (nth (iterate (fn [p]
                    (conj p (loop [l (inc (last p))]
                              (if (some #(= (rem l %) 0) p) (recur (inc l)) l)))) [2]) (dec x))))

(defcheck solution-8bce0e3b
  (fn [z]
    (letfn [(is-prime? [x]
              (->> (range 2 x)
                (map (fn [y] (/ x y) ))
                (filter integer?)
                (filter (fn [y] (not= 1 y) ))
                (empty? )))]
      (take z (filter is-prime? (range 2 542))))))

(defcheck solution-8bf3629e
  (fn primes
    ([] (cons 2 (primes [2] 3)))
    ([n] (take n (primes)))
    ([s x]
     (loop [k x]
       (if (some #(= (mod k %) 0) s)
         (recur (inc k))
         (lazy-seq (cons k (primes (cons k s) (inc k)))))))))

(defcheck solution-8c3ac9d4
  (fn [x]
    (take x (drop 2
              (remove
                (fn [n] (some #(zero? (mod n %)) (range 2 n)))
                (range))))))

(defcheck solution-8cbe7984
  (fn [c]
    (loop [ps [2] n 3]
      (if (= c (count ps))
        ps
        (let [p (loop [x n] (if (every? #(pos? (rem x %)) ps) x (recur (+ 2 x))))]
          (recur (conj ps p) (+ 2 p)))))))

(defcheck solution-8e195b30
  ; Eratosthenes Sieve
  (fn [n]
    (loop [i 2, preds [], result []]
      (cond (= n (count result)) result
            (every? #(% i) preds) (recur
                                    (inc i)
                                    (conj preds #(not= 0 (rem % i)))
                                    (conj result i))
            :else (recur (inc i) preds result)))))

(defcheck solution-8e37b83e
  (fn [n]
    (loop [ret [] n n i 2]
      (if (zero? n)
        ret
        (if (some #(zero? (mod i %)) ret)
          (recur ret n (inc i))
          (recur (conj ret i) (dec n) (inc i)))))))

(defcheck solution-8e7005c3
  (fn nprimes [n]
    (letfn [(prime? [x] (not-any? zero? (map #(rem x %) (take-while #(<= (* % %) x) (range 2 x)))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-8ec33968
  (fn primes
    ([n] (primes n 2 {}))
    ([n q m] (if (pos? n) (let [fs (m q)] (if fs
                                            (primes n (inc q) (merge-with concat m (zipmap (map #(+ % q) fs) (map list fs))))
                                            (lazy-seq (cons q (primes (dec n) (inc q) (assoc m (* q q) [q]))))))))))

(defcheck solution-8edb55b8
  (fn [n]
    (letfn [(non-mults [l] (filter #(not= 0 (mod % (first l))) (rest l)) )
            (non-mult-list [l] (cons (first l) (lazy-seq (non-mult-list (non-mults l)))))]
      (take n (non-mult-list (drop 2 (range)))))))

(defcheck solution-8edfda9f
  (fn primes
    [x]
    (let [prime? (fn [n] (not-any? #(zero? (rem n %))
                           (range 2 n)))
          primes (for [x (iterate inc 2) :when (prime? x)] x)]
      (take x primes))))

(defcheck solution-8f069800
  (fn primes [n]
    (letfn [(prime? [n]
              (cond
                (or (= n 0) (= n 1)) false
                (or (= n 2) (= n 3)) true
                :else (= '(1) (filter #(= 0 (rem n %)) (range 1 (inc (Math/sqrt n)))))))]
      (take n (filter prime? (range))))))

(defcheck solution-8f40cd93
  (fn [n]
    (letfn [(next-prime [found]
              (loop [n (inc (peek found))]
                (if (some (fn [prime] (zero? (rem n prime))) found)
                  (recur (inc n))
                  (conj found n))))]
      (first (drop (dec n) (iterate next-prime [2]))))))

(defcheck solution-8f5ae566
  (fn [x]
    (last  ( take x
             (iterate       (fn prime [xs]
                              (loop [k (inc (last xs))]
                                (let [ss (map #(rem k %) xs)]
                                  (if (every? #(not= 0 %) ss )
                                    (conj xs k)
                                    (recur (inc k))
                                    )
                                  )
                                )

                              ) [2] ) ))))

(defcheck solution-8f5c2673
  (fn primes2 [x]
    (letfn[(primes
             []
             (lazy-seq
               (let [prime [2 3 5 7 11]]
                 (letfn [(is-prime [primelst x]
                           (not (some (fn [i]
                                        (= 0 (mod x i)))
                                  (filter #(<= (* % %) x) primelst))))
                         (get-primes [primes start]
                           (lazy-seq
                             (if(is-prime primes start )
                               (concat [start] (get-primes (conj primes start) (+ 2 start)))
                               (get-primes  primes (+ 2 start)))))]
                   (concat prime (get-primes prime 13))))))]
      (take x (primes)))))

(defcheck solution-8feff97
  (fn [n]
    (letfn [(p [ps i]
              (if (every? #(not (zero? (mod i %))) ps)
                (lazy-seq (cons i (p (conj ps i) (inc i))))
                (recur ps (inc i))))]
      (take n (p #{} 2)))))

(defcheck solution-90378ac6
  (fn [n]
    (letfn [(prime? [x]
              (if (= x 2)
                true
                (not (some #(zero? (mod x %)) (range 2 (inc (Math/sqrt x)))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-90560eff
  (fn take-prime [n]
    (letfn [(prime-numbers  ([] (concat [2 3 5 7] (prime-numbers [2 3 5 7])))
              ([prev-primes]
               (let [start (inc (last prev-primes))
                     is-prime (fn [i] (every? #(not= (mod i %) 0) prev-primes))
                     first-prime (fn [n] (if (is-prime n) n (recur (inc n))))
                     p (first-prime start)
                     next-primes (conj prev-primes p)]
                 (lazy-seq (cons p (prime-numbers next-primes))))))]
      (take n (prime-numbers)))))

(defcheck solution-9099c5c6
  (fn [n]
    (loop [primes []
           cand 2]
      (if (>= (count primes) n)
        primes
        (if (not-any? #(= 0 (mod cand %)) primes)
          (recur (conj primes cand) (inc cand))
          (recur primes (inc cand)))))))

(defcheck solution-90ea7089
  (fn [x]
    (take x
      (map last
        (iterate (fn [found] (conj found (some (fn [n] (if (every? #(> (mod n %) 0) found) n nil)) (map #(+ % (inc (last found))) (range)))))
          [2])))))

(defcheck solution-915574c7
  (fn [lim]
    (take lim
      (let [
            prime
            (fn [num]
              (every?
                (fn [d] (< 0 (mod num d)))
                (range 2 num)))]
        (filter prime (range 2 2000))))))

(defcheck solution-91a3311f
  (fn [n]
    (take n (filter
              (fn [x] (not-any? #(zero? (rem x %)) (range 2 x)))
              (drop 2 (range))))))

(defcheck solution-91bc6488
  #(take % ((fn sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter (fn [xx] (not= 0 (mod xx (first s))))
                                   (rest s)))))) (iterate inc 2))))

(defcheck solution-91fc4598
  (letfn ((primes
            ([] (primes (cons 2 (iterate #(+ 2 %) 3))))
            ([cands]
             (let [x (first cands)]
               (lazy-seq
                 (cons x (primes (remove #(= 0 (mod % x))
                                   (drop 1 cands)))))))))
    #(take % (primes))))

(defcheck solution-925536a8
  (fn [m]
    (loop [ps [2] n 3]
      (if (= (count ps) m)
        ps
        (if (empty? (filter zero? (map #(rem n %) ps)))
          (recur (conj ps n) (inc n))
          (recur ps (inc n)))))))

(defcheck solution-92689c36
  (fn [n]
    (take n
      (filter
        #(not-any? (fn [e] (= 0 (mod % e))) (range 2 %))
        (drop 2 (range))))))

(defcheck solution-92c2fa5b
  (fn [n]
    (take n (filter (fn [n]
                      (cond
                        (> 2 n) false
                        (= 2 n) true
                        :else (< 0 (apply min (map #(mod n %) (range 2 (inc (Math/sqrt n))))))))
              (map (comp inc inc) (range))))))

(defcheck solution-93210e98
  (fn [n]
    (take
      n
      (map
        first
        (iterate
          (fn [[n xs]]
            (let [xs (remove #(zero? (mod % n)) xs)]
              [(first xs) (rest xs)]))
          [2 (drop 2 (range))])))))

(defcheck solution-934165a0
  (fn [n]
    (let [primes
          (fn primes []
            (concat
             [2 3 5 7]
             (lazy-seq
               (let [primes-from
                     (fn primes-from [n]
                       (if (some #(zero? (rem n %))
                             (take-while #(<= (* % %) n) (primes)))
                         (recur (+ n 1))
                         (lazy-seq (cons n (primes-from (+ n 1))))))]
                 (primes-from 11)))))]
      (take n (primes)))))

(defcheck solution-93b26c0f
  (fn [cnt]
    (let [not-divisible? #(not (zero? (mod %1 %2)))
          is-prime? #(every? (partial not-divisible? %)
                       (range 2 (inc (Math/sqrt %))))]
      (take cnt (concat [2] (filter is-prime? (iterate inc 2)))))))

(defcheck solution-93ec4277
  (fn n-primes [n]
    (letfn
     [(step [[i ps]]
        (if (some #(= 0 (mod i %)) (take-while #(>= i (* % %)) ps)) ;i not prime
          (recur [(inc i) ps])
          [(inc i) (conj ps i)]))]
      (second (last (take n (iterate step [3 [2]])))))))

(defcheck solution-9432ca79
  (fn [n]
    (letfn [(prime? [x]
              (->> (range 2 (int (inc (Math/sqrt x))))
                (some #(= 0 (mod x %)))
                (not)))]
      (->> (range)
        (drop 2)
        (filter prime?)
        (take n)))))

(defcheck solution-945185ab
  (fn [k] (take k (remove (fn p[x] (some #(= 0 (rem x %)) (range 2 x))) (iterate inc 2)))))

(defcheck solution-945ed324
  (fn [n]
    (loop [var 3 cnt 1 rslt [2]]
      (if (>= cnt n) rslt
                     (if (some #(= 0 (mod var %)) rslt)
                       (recur (+ 2 var) cnt rslt )
                       (recur (+ 2 var) (inc cnt) (conj rslt var) )
                       )
                     )
      )
    ))

(defcheck solution-946c7b63
  (fn [n]
    (letfn [
            (prime? [x smaller-primes]
              (not-any? #(zero? (rem x %)) smaller-primes))

            (next-prime [primes]
              (some #(if (prime? % primes) %)
                (map (partial + (last primes) 1) (range))))

            (Sieve-of-Eratosthenes [primes]
              (lazy-seq (cons
                          (last primes)
                          (Sieve-of-Eratosthenes
                            (conj primes (next-prime primes))))))]

      (take n (Sieve-of-Eratosthenes [2])))))

(defcheck solution-94ea73ae
  (fn [N]
    (let [multiple-of? (fn [a] #(= 0 (mod % a)))
          series (iterate inc 2)]
      (take N
        ((fn sieve [xs]
           (cons (first xs)
             (lazy-seq (sieve (remove (multiple-of? (first xs)) (rest xs))))))
         series)))))

(defcheck solution-953bc4c
  (fn prime-list[n]
    (letfn [(is-prime [k] (let [bound (int (Math/sqrt k))]
                            (every? #(not= (rem k %) 0) (range 2 (inc bound)))
                            ))]
      (take n (filter #(is-prime %) (map #(+ 2 %) (range))))
      )
    ))

(defcheck solution-95900912
  (fn first-primes [n]
    (letfn [(compute-primes [cnt result number]
              (if (= cnt 0)
                result
                (let [divisors (take-while #(<= (* % %) number) result)]
                  (if (some #(zero? (rem number %)) divisors)
                    (recur cnt result (+ number 2))
                    (recur (dec cnt) (conj result number) (+ number 2))))))]
      (compute-primes (dec n) [2] 3))))

(defcheck solution-95dc8fa7
  (fn [n]
    (take n
      (filter
        (fn [cand] (every? pos? (map #(mod cand %) (range 2 cand))))
        (iterate inc 2)))))

(defcheck solution-95fe0e1e
  (fn [n] (->> (filter #(= % ((fn [number] (->> (range 2 (inc number))
                                             (drop-while (fn [x] (pos? (mod number x))))
                                             first)) %)) (range))
            (take n))))

(defcheck solution-9608bf06
  (fn p67
    [n]
    (->>
      (iterate inc 2)
      (filter (fn [x] (every? #(< 0 (mod x %)) (range 2 x))))
      (take n))))

(defcheck solution-971b8dc0
  (fn [x]
    (letfn [(divides? [n i] (zero? (mod n i)))
            (is-prime? [n] (not-any? #(divides? n %) (range 2 n)))]
      (->> (range)
        (drop 2)
        (filter is-prime?)
        (take x)))))

(defcheck solution-97710af
  (fn take-primes [n]
    (let [impl (fn [acc, i]
                 (if (= i n)
                   acc
                   (let [next-start (+ 1 (last acc))
                         next-range (drop next-start (range))
                         prime? (fn [x] (every? #(not (zero? %)) (map #(mod x %) acc)))
                         next-prime (first (for [x next-range :when (prime? x)] x))]
                     (recur (conj acc next-prime) (+ 1 i)))))]
      (impl [2] 1))))

(defcheck solution-97993086
  (fn [left]
    (cons 2
      (loop [n 3, ps [], left (dec left)]
        (if (zero? left) ps
                         (let [n-is-p (not-any? #(= 0 (mod n %)) ps)]
                           (recur (+ n 2)
                             (if n-is-p (conj ps n) ps)
                             (if n-is-p (dec left) left))))))))

(defcheck solution-97dc19f6
  (fn prime [howmany]
    (reductions (fn [last _]
                  (loop [cur (inc last)]
                    (if (or (< cur 4) (not-any? #(integer? (/ cur %)) (range 2 (dec cur))))
                      cur
                      (recur (inc cur)))))
      (range 2 (+ howmany 2)))))

(defcheck solution-982bdb15
  (fn [n]
    (let [prime? (fn [x] (and (or (= x 2) (odd? x)) (every? #(< 0 (mod x %)) (range 3 (inc (Math/sqrt x)) 2))))]
      (->> (iterate (partial + 2) 3)
        (filter prime?)
        (cons 2)
        (take n)))))

(defcheck solution-982c285f
  (fn [n]
    (letfn [(ps [[p & ns]]
              (lazy-seq
                (cons p (ps (remove #(zero? (rem % p)) ns)))))]
      (take n (ps (iterate inc 2))))))

(defcheck solution-98710a60
  #(take % (filter (fn [p] (loop [n 2]
                             (cond
                               (> n (Math/sqrt p)) true
                               (zero? (mod p n)) false
                               :else (recur (inc n)))))
             (iterate inc 2))))

(defcheck solution-9879828a
  (fn [x] (take x
            (let [primes (atom [])]
              (for [n (iterate inc 2)
                    :when (not-any? #(zero? (rem n %))
                            (filter #(<= % (Math/sqrt n))
                              @primes))]
                (do (swap! primes conj n)
                    n))))))

(defcheck solution-9891583
  (fn __ [n]
    (take
      n
      (filter
        #(empty? (re-matches #"^1?$|^(11+?)\1+$" (apply str (repeat % "1"))))
        (range)))))

(defcheck solution-99947058
  #(loop[result [], ind 0, remaining (drop 2 (range))];(drop 2 (range)) represents 2, 3, .... to infinite
     (let[primeNum (first remaining)];take out the new prime number
       (if (= ind %) result
                     (recur (conj result primeNum) (inc ind) (remove (fn[x] (= 0 (rem x primeNum))) remaining))))))

(defcheck solution-99b4628a
  (fn [n]
    (take n
      (cons 2
        (filter (fn [x] (not-any? #(zero? (mod x %))
                          (take-while #(<= (* % %) x) (range 2 x))))
          (iterate inc 3))))))

(defcheck solution-9a2e326e
  (fn [n]
    (take n
      (cons 2 (remove (fn [candidate]
                        (some #(zero? (mod candidate %)) (range 3 (/ candidate 2))))
                (iterate #(+ 2 %) 3))))))

(defcheck solution-9a3394a1
  (fn [num]
    (take num (filter (fn [n] (= n
                                (some #(when (= (rem n %) 0) %)
                                  (range 2 (inc n)))))
                (range)))))

(defcheck solution-9a349ca7
  (fn [c]
    (let [prime? (fn [n]
                   (if (= 1 n)
                     false
                     (not-any? zero? (map #(mod n %) (range 2 n)))))]
      (take c (filter prime? (drop 2 (range)))))))

(defcheck solution-9a5da582
  (fn [n]
    (take n
      (filter
        (fn is-prime [n]
          (nil?
            (some #(zero? (mod n %))
              (range 2 n))))
        (range 2 1000)))))

(defcheck solution-9ad466b
  (fn prime [size]
    (letfn [(prime? [coll x] (not (some #(zero? (rem x %)) coll)))
            (next-prime [coll k] (if (prime? coll k)
                                   k
                                   (next-prime coll (inc k))))
            (step [coll] (let [next-num (next-prime coll (last coll))]
                           (lazy-seq (cons (last coll) (step (conj coll next-num))))))]
      (take size (step [2])))))

(defcheck solution-9b4d2e2
  (fn primes [n]
    (let [prm
          (fn [x xs]
            (loop [y (first xs) ys (rest xs)]
              (if (nil? y) true
                           (if (= (mod x y) 0)
                             false
                             (recur (first ys) (rest ys))
                             )
                           )
              )
            )
          ]
      (loop [prs [] cur 2]
        (if (= n (count prs))
          prs
          (if (prm cur prs)
            (recur (conj prs cur) (inc cur))
            (recur prs (inc cur))
            )
          )
        )
      )
    ))

(defcheck solution-9b54fd6f
  (fn [n]
    (letfn [(prime? [i]
              (->> (range 1 (inc i))
                (filter #(zero? (mod i %)))
                count
                (= 2)))]
      (->> (range)
        (filter prime?)
        (take n)))))

(defcheck solution-9b651b3a
  (fn [n]
    (take n (filter (fn [i]
                      (= i ((fn [x d]
                              (cond (> (* d d) x) x
                                    (= (mod x d) 0) d
                                    :else (recur x (+ d 1)))) i 2))) (drop 2 (range))))))

(defcheck solution-9b73ccd0
  (fn prime-nos
    [n]
    (letfn [(prime? [n]
              (not-any? zero?
                (map #(rem n %) (range 2 (inc (quot n 2))))))]
      (take n
        (for [x (drop 2 (range))
              :when (prime? x)]
          x)))))

(defcheck solution-9c773b90
  (fn [np] (take np (filter (fn [n] (not-any? #(zero? (mod n %)) (range 2 n))) (drop 2 (range))))))

(defcheck solution-9ce845b
  (fn primes [x]
    (letfn [(is-prime? [n]
              (if (= 2 n)
                true
                (if (or (= 1 n) (and (even? n) (> 2)))
                  false
                  (not (reduce #(or %1 (integer? (/ n %2))) false (range 3 n))))))]
      (take x (filter is-prime? (drop 1 (range)))))))

(defcheck solution-9e0fbcf2
  (fn primes
    [n]
    (vec (take n (lazy-seq (let [nums (lazy-seq (map #(+ % 2) (range)))]
                             (filter (fn prime?
                                       [prime]
                                       (let [divisors (range 2 prime)
                                             factor #(if (= 0 (mod (first %1) %2)) (conj %1 %2) %1)
                                             factors (reduce factor [prime] divisors)]
                                         (= 1 (count factors)))) nums)))))))

(defcheck solution-9e208b65
  (fn [n] (take n (letfn [(isprime [n] (every? false? (map  #(= (rem n %) 0) (range 2 n))))] (filter isprime (drop 2 (range)))))))

(defcheck solution-9e5197f0
  (fn [n]
    (into []
      (take n
        (iterate
          (fn [x]
            (if ((fn [y]
                   (every? #(not= 0 (mod y %)) (range 2 y))) (inc x))
              (inc x)
              (recur (inc x)))) 2)))))

(defcheck solution-9e8dcd9
  (letfn [(prime? [n] (not-any? #(zero? (mod n %)) (range 2 n)))
          (primes [] (filter prime? (iterate inc 2)))]
    (fn [n]
      (take n (primes)))))

(defcheck solution-9edf7604
  (fn [n]
    (take n
      (letfn [(d [n] #(= 0 (rem % n)))
              (f [i]
                (lazy-seq
                  (cons (first i)
                    (f (remove (d (first i)) i)))))]
        (f (map #(+ 2 %) (range)))))))

(defcheck solution-9f528b3d
  (fn [x]
    (loop [primes [2]
           current 3]
      (if (= (count primes) x) primes
                               (recur (if (every? true? (map #(not= 0 (rem current %)) primes)) (conj primes current) primes) (+ 2 current))))))

(defcheck solution-9f667135
  (fn [x] (take x
            (filter #(every? identity
                       (for [i (range 2 (inc (int (Math/sqrt %))))]
                         (pos? (mod % i))))
              (iterate inc 2)))))

(defcheck solution-9fa5662f
  (fn prime-numbers [right-bound]
    (take right-bound (letfn [( prime? [n]
                                (if (= n 2)
                                  true
                                  (every? #(not (zero? (mod n %))) (range 2 n))))]
                        (filter prime? (map #(+ 2 %) (range)))))))

(defcheck solution-9fd44c43
  (fn [k] (take k (letfn [(go [n f] (if-let [p (get f n)] (go (inc n) (reduce #(update-in % [(+ n %2)] conj %2) f p)) (lazy-seq (cons n (go (inc n) (assoc f (* n n) [n]))))))] (go 2 {})))))

(defcheck solution-a0108552
  (letfn [(sieve [n nums]
            (filter #(not (zero? (mod % n))) nums))
          (new-primes [[primes nums]]
            [(cons (first nums) primes) (sieve (first nums) nums)])]
    (fn [n]
      (reverse (first (first (drop n (iterate new-primes [[] (drop 2 (range))]))))))))

(defcheck solution-a075901f
  (fn primes2 [n]
    (loop [l (drop 2 (range))
           ps []]
      (if (= (count ps) n)
        ps
        (recur (filter #(not= 0 (rem % (first l))) (rest l))
          (conj ps (first l)))))))

(defcheck solution-a08fc4
  (fn [c]
    (take c ((fn gpl
               ([n]    (cons n (lazy-seq (gpl (inc n) [n]))))
               ([n pl] (let [np (first (filter (fn [m]
                                                 (every? identity (map #(not= (mod m %) 0) pl)))
                                         (drop n (range))))]
                         (cons np (lazy-seq (gpl (inc np) (conj pl np)))))))
             2))))

(defcheck solution-a12884b2
  (fn [n]
    (letfn [(prime? [n]
              (every? #(> (rem n %) 0)
                (drop 2 (range n))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-a13a09c0
  (fn f [n]
    (letfn [(isPrime? [n'] (not-any? zero? (map #(mod n' %) (range 2 n'))))]
      (take n (filter isPrime? (iterate inc 2))))))

(defcheck solution-a15b2e92
  (fn [n]
    (let [prime?
          (fn [n]
            (not-any?
              #(zero? (rem n %))
              (range 2 n)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-a1cfd193
  (fn [x]
    (letfn [(prime? [n]
              (every? #(not (zero? (mod n %))) (range 2 n)))]
      (take x (filter prime? (iterate inc 2))))))

(defcheck solution-a1d200c3
  (fn [n]
    (take n(filter
             (fn is-prime [n]
               (nil?
                 (some
                   #(zero? (mod n %))
                   (range 2 n))))
             (range 2 1000)))))

(defcheck solution-a21d148d
  (fn prime [n]
    (loop [pp 2 ps #{}]
      (if (= (count ps) n)
        (into []  (sort ps))
        (if (= 0 (count (filter #(= 0 (mod pp %)) ps)))
          (recur (inc pp)  (conj ps pp))
          (recur (inc pp) ps)
          )))
    ))

(defcheck solution-a266e158
  (fn
    [m]
    (let [prime? (fn [n]
                   (->>
                     (filter #(= 0 (rem n %)) (range 2 n))
                     first
                     nil?))]
      (take m (filter prime? (drop 2 (range)))))))

(defcheck solution-a326d483
  (fn [n]
    (let [prime? (fn [n] (empty? (filter #(= 0 (mod n %)) (drop 2 (range n)))))
          primes (for [x (drop 2 (range)) :when (prime? x)] x)]
      (take n primes))))

(defcheck solution-a38bc8be
  (fn [x]
    (loop [y 2
           primes [2]]
      (if (= (count primes) x)
        primes
        (recur (inc y) (if (some zero? (map #(rem y %) primes))
                         primes
                         (conj primes y)))))))

(defcheck solution-a3d83a4d
  #(let [next-prime (fn [n] (let [n (inc n)
                                  prime? (fn prime? [n] (cond (= n 2) true
                                                              (= n 1) false
                                                              :else (loop
                                                                     [n n
                                                                      div (dec n)]
                                                                      (cond
                                                                        (= 1 div) true
                                                                        (zero? (mod n div)) false
                                                                        :else (recur n (dec div))
                                                                        ))))
                                  ] (if (prime? n) n (recur n))))]
     (take % (iterate next-prime 2))))

(defcheck solution-a419d8a7
  (fn first-x-prime-nums [bound]
    (letfn [(prime? [x]
              (empty? (filter #(zero? (rem x %)) (range 2 x))))]
      (take bound (nnext (filter prime? (range)))))))

(defcheck solution-a42db95b
  (fn sieve
    ([n]
     (sieve n (iterate inc 2)))
    ([n l]
     (let [hd (first l) bd (rest l)]
       (if (zero? n)
         []
         (lazy-seq
           (take n
             (cons hd (sieve (- n 1) (filter #(not (zero? (mod % hd))) bd))) )))))))

(defcheck solution-a463bc57
  (fn [n]
    (loop [cur 2
           primes []
           needed n]
      (cond
        (= needed 0) primes
        (every? #(not (= 0 (mod cur %))) primes)
        (recur (inc cur)
          (conj primes cur)
          (dec needed))
        :else (recur (inc cur)
                primes
                needed)))))

(defcheck solution-a49f1204
  #(take %3
     (filter
       (fn [n]
         (reduce
           (fn [b m] (and b (< 0 (mod n m))))
           true
           (%2 2 n)))
       (map % (%2)))) #(+ % 2) range)

(defcheck solution-a4bed47b
  (fn [t] (nth (iterate (fn [a] (conj a (first (filter
                                                 #(every? pos? (map rem (repeat %) a ))
                                                 (iterate inc (inc (apply max a))))))
                          ) [2] ) (dec t))))

(defcheck solution-a4eb93c7
  (fn [x]
    (loop [i 0 acc [] n 2]
      (if (= i x)
        acc
        (if (every? #(pos? (mod n %)) acc)
          (recur (inc i) (conj acc n) (inc n))
          (recur i acc (inc n)))))))

(defcheck solution-a518079d
  (fn [n]
    (let [primes (fn primes [ps]
                   (let [n (inc (last ps))
                         find-first (fn [pred coll] (first (filter pred coll)))
                         prime? (fn [ps x] (every? #((complement zero?) (mod x %)) ps))
                         p (find-first (partial prime? ps) (iterate inc n))]
                     (cons p (lazy-seq (primes (conj ps p))))
                     ))]
      (take n (cons 2 (primes [2]))))))

(defcheck solution-a5c90e0d
  (fn [n]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                   (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-a66ffa6
  (fn f [[x & xs] n] (when (pos? n) (cons x (f (remove #(= 0 (mod % x)) xs) (dec n))))) (drop 2 (range)))

(defcheck solution-a6ac05b6
  #(take %
     (filter
       (fn [n]
         (if (< n 2)
           false
           (= 0 (apply + (for [i (range 2 (inc (int (Math/sqrt n))))]
                           (if (= (mod n i) 0) 1 0))))))
       (range)
       )))

(defcheck solution-a6b317cc
  (fn first-n-primes [n]
    (letfn [(factor? [n x]
              (zero? (rem n x)))
            (factors [n]
              (conj (set (filter #(factor? n %) (range 2 (inc (Math/sqrt n))))) 1 n))
            (prime? [n]
              (= 2 (count (factors n))))]
      (take n (filter #(prime? %) (iterate inc 1))))))

(defcheck solution-a6bf8108
  (fn [n]
    (letfn [(odd-seq []
              (filter odd? (iterate inc 3)))
            (prime? [n]
              (every?
                (fn [x] (not= (mod n x) 0))
                (cons 2 (take-while (fn [x] (< (* x x) (inc n))) (odd-seq)))))
            (primes []
              (cons 2
                (filter prime?
                  (odd-seq))))]
      (take n (primes)))))

(defcheck solution-a7aeeb85
  (fn [n]
    (letfn [(primes
              ([] (primes (iterate inc 2)))
              ([l] (lazy-seq
                     (let [a (first l)]
                       (cons a (primes (filter #(not= 0 (mod % a)) l)))))))]
      (take n (primes)))))

(defcheck solution-a7deeaff
  (fn [n]
    (let [primes ((fn p [filt]
                    (let [f (first filt)]
                      (lazy-seq
                        (cons f
                          (p (filter #(not= 0 (mod % f)) filt))))))
                  (iterate inc 2))]
      (take n primes))))

(defcheck solution-a83d685c
  (fn [c]
    (take c
      (filter
        (fn [n] (every? #(> (mod n %) 0) (range 2 n)))
        (iterate inc 2)))))

(defcheck solution-a8503d5e
  (fn [n]
    (take n(filter
             (fn is-prime [n]
               (nil?
                 (some
                   #(zero? (mod n %))
                   (range 2 n))))
             (range 2 1000)))))

(defcheck solution-a8d7cc70
  (fn [n]
    (letfn [(ld [n]
              (loop [div 2]
                (cond
                  (= 0 (rem n div)) div
                  :else (recur (inc div)))))]
      (->> (for [x (range)
                 :when (and (> x 1)
                            (= x (ld x)))]
             x)
        (take n)))))

(defcheck solution-a8d94dec
  (fn [z] (take z (
                   (fn peu [x y]
                     (if
                      (not (some #(= 0 (mod x %)) y))
                       (cons x (lazy-seq (peu (inc x) (conj y x))) )
                       (peu (inc x) y)
                       )
                     )
                   2 []))))

(defcheck solution-a8e4aa04
  (fn [n]
    (loop [i 0 ret '() x 2]
      (cond
        (= i n) (reverse ret)
        (every?
          #(not= (rem x %) 0)
          (range 2 x))
        (recur (inc i) (conj ret x) (inc x))
        :else
        (recur i ret (inc x))))))

(defcheck solution-a8f84e03
  (fn primes- [x]
    "67. Write a function which returns the first x number of prime numbers."
    (loop [sieve (range 2 (* x x))
           res []]
      (let [prime (first sieve)]
        (if (= x (count res))
          res
          (recur (filter #(not= 0 (mod % prime)) sieve) (conj res prime)))))))

(defcheck solution-a99543a6
  (fn [a]
    (let [prime? (fn [n]
                   (loop [i 2]
                     (if (= i n)
                       true
                       (if (= 0 (mod n i))
                         false
                         (recur (inc i))))))
          prime-seq (fn [] (filter prime? (iterate inc 2)))]
      (take a (prime-seq)))))

(defcheck solution-a9990cdc
  (letfn [
          (is-prime? [x] (not-any? (partial = 0) (map #(mod x %) (range 2 (inc (/ x 2))))))
          (prime-seq [] (filter is-prime? (map (partial + 2) (range))))]
    #(take % (prime-seq))))

(defcheck solution-a9a29404
  (fn [n]
    (let [prime? (fn [n] (and
                          (odd? n)
                          (every? #(not (= 0 (mod n %))) (range 3 n 2))))]
      (take n (cons 2 (filter prime? (iterate #(+ 2 %) 3)))))))

(defcheck solution-aa008c9e
  (fn [n]
    (take n
      ((fn primeseq [candidates]
         (let [p (first candidates)]
           (cons p
             (lazy-seq (primeseq
                         (filter
                           #(not (zero? (mod % p)))
                           (rest candidates)))))))
       (iterate inc 2)))))

(defcheck solution-aa64b3f4
  (fn primes [num]
    (loop [n num,val 3,primecol [2] ]
      (if (= n 1)
        primecol
        (if (
             (fn prime? [testVal,primeseq]
               (loop [col primeseq]
                 (let [primeVal (first col) ]
                   (cond (= (mod testVal primeVal) 0) false
                         (> (* primeVal primeVal) testVal)  true
                         :else (recur (rest col) )
                         )
                   )
                 )
               ) val  primecol)
          (recur  (dec n) (+ 2 val) (conj primecol val) )
          (recur  n (+ 2 val) primecol )
          )
        )
      )
    ))

(defcheck solution-aa879293
  (fn [n]
    (letfn [(prime? [x]
              (empty? (filter #(zero? %)
                        (map #(mod x %)
                          (range 2 x)))))]
      (take n
        (filter prime?
          (iterate inc 2))))))

(defcheck solution-abc2949d
  (fn [n]
    (letfn [(prime? [p]
              (or (= p 2)
                  (every? #(> (rem p %) 0)
                    (range 2 (inc (Math/floor (Math/sqrt p)))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-abc35bd4
  (fn primes [n]
    (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-abccb9f7
  (fn [n] (take n (filter (fn [num]
                            (cond
                              (= num 0) false
                              (= num 1) false
                              (= num 2) true
                              :else (empty? (filter #(= 0 (rem num %)) (range 2 num)))
                              )) (range)))))

(defcheck solution-ac193357
  (fn prime?[n]
    (letfn [(find-divisor [n d]
              (loop [x d]
                (cond
                  (> (* x x) n) n
                  (= (rem n x) 0) x
                  :else (recur (inc x)))))

            (smallest-divisor [n]
              (find-divisor n 2))]
      (take n (drop 2 (filter #(= (smallest-divisor %) %) (range)))))))

(defcheck solution-aca27094
  (fn [n] (take n (filter (fn [m] (every? #(not= 0 (rem m %)) (range 2 m))) (range 2 1000)))))

(defcheck solution-aca7ef9d
  (let [prime? (fn [x] (not-any? #(zero? (mod x %)) (range 2 x)))]
    #(take % (filter prime? (iterate inc 2)))))

(defcheck solution-accaba9c
  (fn [num]
    (letfn [(indivisible-from? [n] (fn [d] (not= 0 (rem n d))))
            (max-root [n] (+ 1 (int (Math/sqrt n))))
            (prime? [n] (every? (indivisible-from? n) (range 2 (max-root n))))]
      (take num (lazy-seq (filter prime? (drop 2 (range))))))))

(defcheck solution-ae845345
  (fn [num]
    (loop [result [2]]
      (if (= (count result) num)
        result
        (recur (conj result (
                             (fn [y]
                               (loop [index y]
                                 (if (= (count ((fn [x]
                                                  (loop [res [] i 2]
                                                    (if (= (- x 1) i)
                                                      res
                                                      (if (= 0 (mod x i))
                                                        (recur (conj res i) (inc i))
                                                        (recur res (inc i))
                                                        )
                                                      )
                                                    )
                                                  ) index ))
                                       0)
                                   index
                                   (recur (inc index))
                                   )
                                 )
                               ) (inc (last result))
                             )))
        )
      )
    ))

(defcheck solution-aeeea1ab
  (fn eras [x]
    (letfn [(sieve [nums]
              (let [p (first nums)]
                (cons p
                  (lazy-seq (sieve (filter #(> (rem % p) 0) (rest nums)))))))]

      (take x (sieve (drop 2 (range)))))))

(defcheck solution-aefc73b9
  (fn prime
    ([n]
     (->> (range)
       (drop 2)
       (filter #(every? (fn [y] (pos? (mod % y))) (range 2 %) ))
       (take n)
       )
     )))

(defcheck solution-af380ed2
  (fn get-x-primes [x]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                   (rest s))))))]
      (take x (sieve (iterate inc 2))))))

(defcheck solution-af4d438
  (fn [n]
    (let [prime? (fn [num]
                   (cond (= num 2) true
                         (= (mod num 2) 0) false
                         (= num 3) true
                         (= num 5) true
                         (= (mod num 3) 0) false
                         (= (mod num 5) 0) false
                         :else (let [s (for [i (range 3 num 2)]
                                         (if (= (mod num i) 0)
                                           i
                                           1))]
                                 (<= (count (set s)) 1))))
          cal-prime (fn [primes-count primes cal-to]
                      (cond (= n primes-count) primes
                            (prime? cal-to) (recur (inc primes-count)
                                              (concat primes (list cal-to))
                                              (+ cal-to 2))
                            :else (recur primes-count primes (+ cal-to 2))))]
      (cal-prime 1 [2] 3))))

(defcheck solution-af67465e
  (fn [i]
    (letfn [(f [[x & s]]
              (cons x (lazy-seq (f (remove #(= 0 (mod % x)) s)))))]
      (take i (f (iterate inc 2))))))

(defcheck solution-af6eda12
  (fn [x]
    (take x
      (filter #(every? (fn [e] ((comp not zero?) (rem % e))) (range 2 (dec %)))
        (lazy-seq
          (iterate inc 2)
          )))))

(defcheck solution-afd61b05
  (fn [y]
    (take y
      (cons 2
        (filter
          #(not (some true?
                  (for [x (range 3 %)]
                    (= 0 (rem % x)))))
          (iterate (partial + 2) 3))))))

(defcheck solution-afe197ac
  (fn [n]
    (loop [got []
           need n
           next 2]
      (if (zero? need)
        got
        (if (not-any? #(zero? (mod next %)) got)
          (recur (conj got next) (dec need) (inc next))
          (recur got need (inc next)))))))

(defcheck solution-b05b6d51
  (fn take-primes [n]
    (drop 2 (take (+ 2 n) (filter
                            (fn is-prime? [n]
                              (not (some #(zero? (rem  n %)) (range 2 n))))
                            (range))))))

(defcheck solution-b0cdb52c
  (fn [n]
    (let [sieve (fn sieve [xs] (cons (first xs) (lazy-seq (sieve (filter #(> (mod % (first xs)) 0) (rest xs))))))]
      (take n (sieve (drop 2 (range)))))))

(defcheck solution-b0d144
  (fn fn4 [n]
    (let [prime
          (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (take n (filter prime (iterate inc 2))))))

(defcheck solution-b0ed547b
  (fn first-n-primes [n]
    (let [top-num (Math/ceil (+ (* n (Math/log n)) (* n (Math/log (Math/log n)))))]
      (loop [coll (rest (rest (take (+ 5 top-num) (range))))
             primes []]
        (if (= (count primes) n)
          primes
          (let [no-div (filter #(> (mod % (first coll)) 0) coll)]
            (recur
              no-div
              (conj primes (first coll)))))))))

(defcheck solution-b16324c1
  (fn [m] (take m (conj (filter (fn [n] (not (some #(zero? (mod n %)) (range 2 (inc (Math/sqrt n)))))) (iterate inc 3)) 2))))

(defcheck solution-b16cd899
  (fn [n]
    (take
      n
      (cons
        2
        (filter
          (fn [k]
            (every?
              #(< 0 (rem k %))
              (range 2 (inc (int (Math/floor (Math/sqrt k)))))))
          (iterate #(+ 2 %) 3))))))

(defcheck solution-b1eaac7b
  (fn sol
    ([m] (sol 2 m))
    ([n m]
     (letfn [(p? [n]
               (if (<= n 2)
                 true
                 (= (dec n)
                   (last (take-while #(not= 0 (mod n %)) (range 2 n)))
                   )
                 )
               )]
       (if (zero? m) '()
                     (if (p? n)
                       (cons n (sol (inc n) (dec m)))
                       (sol (inc n) m)
                       )
                     )
       )
     )
    ))

(defcheck solution-b1f7d168
  (fn [n]
    (letfn [(primes* [n previous-primes]
              (let [next-prime (->> (iterate inc n)
                                 (drop-while #(some (fn [x] (zero? (mod % x))) previous-primes))
                                 first)]
                (cons next-prime
                  (lazy-seq (primes* (inc next-prime) (conj previous-primes next-prime))))))]
      (take n (primes* 2 [])))))

(defcheck solution-b2655256
  (fn [i]
    (loop [i i
           n 3
           ps [2]]
      (if (= 1 i)
        ps
        (if (some #(zero? (mod n %)) ps)
          (recur i (inc n) ps)
          (recur (dec i) (inc n) (conj ps n)))))))

(defcheck solution-b2741f6b
  (fn [n] (take n
            (iterate (fn [prev]
                       (loop [v (inc prev)]
                         (if (some #(= 0 (mod v %)) (range 2 v))
                           (recur (inc v))
                           v))) 2))))

(defcheck solution-b2d91347
  (fn n-primes [n]
    (take
      n
      (iterate
        (fn get-next-prime [k]
          (let [m (inc k)]
            (if (contains?
                  (set (map #(rem m %) (range 2 k)))
                  0)
              (get-next-prime m)
              m)))
        2))))

(defcheck solution-b35158d0
  (fn [n]
    (let [oddnumbers (rest (filter odd? (range)))]
      (take n
        (cons 2
          (filter
            #(not (reduce
                    (fn [a b] (or a (= 0 (mod % b))))
                    false
                    (take-while (fn [a] (< a  %)) oddnumbers)))
            oddnumbers))))))

(defcheck solution-b35c8eeb
  (fn [x] (take x (filter (fn [n] (every? #(not= (mod n %) 0) (range 2 n)))
                    (range 2 9999)))))

(defcheck solution-b3767022
  (fn [n]
    (loop [i 2 res []]
      (cond
        (= (count res) n) res
        (some #(zero? (mod i %1)) (range 2 (dec i)))
        (recur (inc i) res)
        :else
        (recur (inc i) (conj res i))))))

(defcheck solution-b3cbaaf6
  (fn primes-
    ([n] (let [p 2
               nbrs (drop (inc p) (range))
               f-nbrs (filter #(not (zero? (mod %1 p))) nbrs)
               n-p (first f-nbrs)
               res [p n-p]]
           (cond
             (= n 1) [p]
             (= n p) res
             :else (primes- n n-p res f-nbrs))))
    ([n p res nbrs]
     (let [nbrs (rest nbrs)
           f-nbrs (filter #(not (zero? (mod %1 p))) nbrs)
           n-p (first f-nbrs)
           res (conj res n-p)]
       (if (= n (count res))
         res
         (primes- n n-p res f-nbrs))))))

(defcheck solution-b43ff98a
  (fn [x]
    (let [prime? (fn [n pvec] (every? #(pos? (rem n %)) pvec))
          gen-primes (fn [x n pvec]
                       (if (pos? x)
                         (if (prime? n pvec)
                           (recur (dec x) (+ n 2) (conj pvec n))
                           (recur x (+ n 2) pvec))
                         pvec))]
      (gen-primes (dec x) 3 [2]))))

(defcheck solution-b4895051
  (fn prob67 [n]
    (take n
      (cons 2 (filter
                (fn [x] ;; is prime
                  (empty? (filter #(= 0 (mod x %)) (range 2 x)))
                  )
                (iterate #(+ 2 %) 3))))))

(defcheck solution-b49c6d1f
  #(loop [pn [2] i (iterate inc 3) c %]
     (if (= 1 c)
       pn
       (let [s (drop-while (fn [a] (some (fn [b] (= 0 (rem a b))) pn)) i)]
         (recur (conj pn (first s)) (rest s) (dec c))))))

(defcheck solution-b4c59fd8
  (fn [np]
    (take np (filter (fn [n]
                       (let [indivisible? (fn indivisible? [n col]
                                            (if-let [s (seq col)] (and (pos? (rem n (first col))) (indivisible? n (rest col))) true))]
                         (cond
                           (< n 50) (not= nil (#{2 3 5 7 11 13 17 19 23 29 31 37 41 43 47} n))
                           (or (even? n) (= 0 (rem n 3)) (= 0 (rem n 5)) (= 0 (rem n 7))) false
                           :else
                           (let [upper (inc (int (Math/sqrt n)))]
                             (indivisible? n (remove #(or (even? %) (= 0 (rem % 3)) (= 0 (rem % 5)) (= 0 (rem % 7))) (range 11 upper)))))))
               (range)))))

(defcheck solution-b526ced9
  (fn primes [n]
    (letfn [(square [x] (* x x))
            (divides? [a b] (= (mod b a) 0))
            (smallest-factor [n]
              (loop [candidate 2]
                (cond (> (square candidate) n)  n
                      (divides? candidate n)    candidate
                      :else                     (recur (inc candidate)))))
            (prime? [n] (= n (smallest-factor n)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-b56d158a
  (fn [n]
    (letfn [(primes
              ([] (cons 2 (lazy-seq (primes [2] 3))))
              ([ps p]
               (let [p'
                     (loop [n (+ 2 p)]
                       (if-not (some #(= 0 %) (map #(rem n %) (cons p ps)))
                         n
                         (recur (+ 2 n))))]
                 (cons p (lazy-seq (primes (cons p ps) p'))))))]
      (take n (primes)))
    ))

(defcheck solution-b6d656c6
  (fn [n] (->> (iterate inc 2) ((fn R [[m & M]] (lazy-cat [m] (R (filter (fn [i] (< 0 (mod i m))) M))))) (take n))))

(defcheck solution-b7174d4b
  #(take % (filter (fn [n] (let [tests (range 2 (inc (Math/sqrt n)))]
                             (or (= n 2) (not-any? (fn [t]
                                                     (= 0 (mod n t))) tests))))
             (drop 2 (range)))))

(defcheck solution-b763bebe
  (fn primes [total]
    (loop [n 2
           acc []]
      (if (< (count acc) total)
        (if (not-any?
              #(zero? (mod n %))
              (range 2 (dec n)))
          (recur (inc n) (conj acc n))
          (recur (inc n) acc))
        acc))))

(defcheck solution-b7748578
  (fn [n]
    (take n (drop 2 (filter
                      (fn [x] (not-any? #(zero? (mod x %)) (range 2 x )))
                      (range))))))

(defcheck solution-b77ee18f
  (fn npn [n]
    (let [nextpn (fn [pn]
                   (loop [g
                          (+ 2 (apply max pn))]
                     (if (some #(= 0 (mod g %)) pn)
                       (recur (+ g 2))
                       (conj pn g))))]
      (first (filter #(>= (count %) n) (iterate nextpn [2 3]))))))

(defcheck solution-b78dd717
  (fn gen-primes [n]
    (take n (lazy-seq
              (drop 2 (filter
                        (fn prime? [n]
                          (every? #(pos? (mod n %)) (range 2 n)))
                        (range)))))))

(defcheck solution-b79ac0bd
  (letfn
   [(indivisible? [n pl]
      (not-any? #(zero? (rem n %)) pl))

    (next-prime [pl]
      (if (empty? pl)
        2
        (let [last-prime (peek pl)]
          (first (filter #(indivisible? % pl)
                   (iterate inc last-prime))))))]
    (fn primes
      ([n]
       (primes n []))

      ([n acc]
       (if (zero? n)
         acc
         (recur (dec n) (conj acc (next-prime acc))))))))

(defcheck solution-b7b446f9
  (fn [n] (letfn [(f [x] (lazy-seq
                           (loop [x (inc x)]
                             (if (empty? (drop-while pos? (map #(mod x %) (range 2 x))))
                               (cons x (f x))
                               (recur (inc x))))))]
            (take n (f 1)))))

(defcheck solution-b7cc378c
  (fn primeNumbers
    [n]
    (let [sieve2 (fn sieve
                   [s]
                   (cons (first s)
                     (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                        (rest s))))))]
      (take n (sieve2 (iterate inc 2))))))

(defcheck solution-b859f6
  (fn [x] (take x (filter (fn [x] (zero? (count (filter #(= (rem x %) 0) (take-while #(<= (* % %) x) (range 2 x)))))) (iterate inc 2)))))

(defcheck solution-b8799bc
  (fn[n]
    (take n
      (filter (fn [p]
                (empty? (filter #(= 0 (rem p %)) (range 2 p))))
        (iterate inc 2)))))

(defcheck solution-b8c1c3be
  (fn [n]
    (take n
      (remove
        (fn [n] (some #{0}
                  (for [i (range 2 n)
                        :while (<= (* i i) n)]
                    (mod n i))))
        (nnext (range))))))

(defcheck solution-b95f4ae6
  (fn [n]
    (take n
      (cons 2
        (filter (fn p? [x]
                  (empty? (drop-while #(not= (mod x %) 0) (range 2 (inc (Math/sqrt x))))))
          (take-nth 2 (drop 3 (range))))))))

(defcheck solution-ba0564e
  (fn [num] (let [primes (fn primes
                           ([] (primes 2 (sorted-map)))
                           ([n s]
                            (letfn [(update-map [n s]
                                      (cond
                                        (contains? s n) (let [ds (s n)]
                                                          (merge-with concat s (into {} (for [d ds] [(+ n d) [d]]))))
                                        :else (assoc s (* 2 n) [n])))]
                              (if (contains? s n)
                                (recur (inc n) (update-map n s))
                                (cons n (lazy-seq (primes (inc n) (update-map n s))))))))]
              (take num (primes)))))

(defcheck solution-bb49e4f5
  (fn prime-numbers
    [n]
    (letfn [(sieve [lst]
              (cons (first lst)
                (lazy-seq (sieve (filter
                                   (comp not zero? #(mod % (first lst)))
                                   (rest lst))))))]
      (take n (sieve (drop 2 (range)))))))

(defcheck solution-bbab39e0
  (fn [n]
    (letfn [(prime? [p]
              (or (= p 2)
                  (every? #(> (rem p %) 0)
                    (range 2 (inc (quot p 2))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-bbc73bf
  (fn [x] (take x (for [ x (sort
                             (into (into [2 3]
                                     (map  #(+ 5 (* 6 %1)) (range 0 100)))
                               (map #(inc (* 6 %1)) (range 1 100))
                               ))
                        :when ((fn prime [n]
                                 (loop [i 2 result true]
                                   (if (> i (/ n 2))
                                     result
                                     (recur (inc i) (and result (not (zero? (mod n i)))) )
                                     )
                                   )
                                 ) x)] x))))

(defcheck solution-bbe69a28
  (fn primeNums [n] (letfn [(isPrime [x]
                              (if (<= x 1)
                                false
                                (if (<= x 3) true
                                             (if  (or (= (rem x 2) 0) (= (rem x 3) 0))
                                               false
                                               (loop [i 5]
                                                 (if (> (* i i) x)
                                                   true
                                                   (if (or (= (rem x i) 0) (= (rem x (+ i 2)) 0))
                                                     false
                                                     (recur (+ i 6))
                                                     )))))))](take n (filter #(isPrime %) (range))) )))

(defcheck solution-bc0c278c
  (fn [n]
    (take n ((fn sieve [s]
               (cons (first s)
                 (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                    (rest s)))))) (iterate inc 2)))))

(defcheck solution-bc2a0621
  (fn [x]
    (take x
      (remove
        (fn [n]
          (some #(= 0 (mod n %))
            (range 2 (inc (int (Math/sqrt n))))))
        (iterate inc 2)))))

(defcheck solution-bc4b1b8
  (fn [n]
    (let [prime? (fn [an] (every? #(pos? (mod an %)) (range 2 an)))]
      (take n (filter prime? (iterate inc 2))))
    ))

(defcheck solution-bc9a4043
  (fn [n] (take n
            (letfn [(d [a] (some #(= 0 (mod a %)) (range 2 a)))]
              (remove #(d %)
                (iterate inc 2))))))

(defcheck solution-bca5041e
  (fn [n] (letfn [
                  (potential-divisors [n] (take-while #(< % (inc (quot n 2))) (cons 2 (range 3 (inc (quot n 2)) 2))))
                  (divisors [n] (filter #(zero? (rem n %)) (potential-divisors n)))
                  (prime? [n] (zero? (count (divisors n))))
                  (primes [] (filter prime? (cons 2 (iterate #(+ % 2) 3))))
                  (first-n-prime-numbers [n] (take n (primes)))
                  ] (first-n-prime-numbers n))))

(defcheck solution-bcf295e6
  (fn [n]
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
                (recur (+ i 6)))))))]
      (take n (filter prime? (range))))))

(defcheck solution-bcf2c9ef
  #(->>
     ((fn sieve [xs]
        (let [ [x s] [(first xs) (rest xs)] ]
          (lazy-seq
            (cons x
              (filter (fn [m] (not (zero? (mod m x))))
                (sieve s) )))))
      (iterate inc 2)) (take %)))

(defcheck solution-bd011b95
  (fn primes [n]
    (letfn [(prime? [n seen]
              (apply = false (map #(zero? (mod n %)) seen)))
            (next-prime [n seen]
              (let [n+1 (inc n)]
                (if (prime? n+1 seen) n+1 (recur n+1 seen))))]
      (loop [iter 0, acc [2]]
        (if (= iter n)
          (pop acc)
          (recur (inc iter) (conj acc (next-prime (peek acc) acc))))))))

(defcheck solution-bd39824d
  (fn
    [x]
    (take x (filter (fn [y] (not (some #(zero? (mod y %)) (range 2 y)))) (iterate inc 2)))))

(defcheck solution-bd4978ff
  #(take % (filter (fn [n]
                     (and (not (zero? n))
                          (not (= 1 n))
                          (not-any? (fn [m] (zero? (mod n m)))
                            (range 2 (inc (quot n 2))))))
             (range))))

(defcheck solution-bd547aa6
  (fn prime-numbers
    [x]
    (take x (filter (fn prime-number?
                      [n]
                      (if (= n 1)
                        false
                        (loop [i 2
                               prime? true]
                          (if (or (= i n) (not prime?))
                            prime?
                            (recur (inc i) (not (zero? (mod n i)))))))) (range)))))

(defcheck solution-bd86b51f
  (fn [n]
    (take n
      (filter
        (fn [x]
          (not-any?
            (fn [y] (zero? (mod x y)))
            (range 2 x)))
        (iterate inc 2)))))

(defcheck solution-bdfcbc08
  (fn prime
    [n]
    (take n (filter (fn [x] (empty? (filter #(zero? (mod x %)) (range 2 x)))) (drop 2 (range))))))

(defcheck solution-be24ce14
  (fn  [n]
    (let [prime? (fn [n]
                   (loop [d (quot n 2)]
                     (cond
                       (<= d 1) true
                       (= 0 (mod n d)) false
                       :else
                       (recur (dec d))
                       )))]
      (loop [acc [] c 2]
        (if (= (count acc) n)
          acc
          (recur (if (prime? c) (conj acc c) acc) (inc c))
          )
        ))))

(defcheck solution-be2dfaf5
  (fn primes [n]
    (let [stream (iterate inc 2)
          sieve (fn [p stream]
                  (filter #(not= 0 (mod % p))
                    stream))
          run-sieve (fn run-sieve [stream]
                      (lazy-seq
                        (let [p (first stream)]
                          (cons p
                            (run-sieve (sieve p stream))))))]
      (take n (run-sieve stream)))))

(defcheck solution-be546524
  (fn primes
    ([n] (take n (primes [] 2)))
    ([xs x]
     (let [prime? (not-any? #(zero? (rem x %1)) (range 2 x))]
       (if prime?
         (cons x (lazy-seq (primes (conj xs x) (inc x))))
         (lazy-seq (primes xs (inc x))))))))

(defcheck solution-be6e2f90
  (fn [n]
    (take n
      (filter (fn [i] (not-any? #(= 0 (mod i %)) (range 2 i))) (drop 2 (range))))))

(defcheck solution-be7f5849
  (fn [x]
    (take x
      (lazy-seq
        (filter (fn [n]
                  (not-any?
                    (fn [a](= 0 (apply mod a)))
                    (partition 2 (interleave
                                   (repeat n)(range 2 (inc (/ n 2)))))))
          (drop 2 (range)))))))

(defcheck solution-beebf034
  #(loop [n 2 primes []]
     (if (< (count primes) %)
       (recur (inc n)
         (if (empty? (filter (fn [div] (= 0 (mod n div))) (range 2 n)))
           (conj primes n)
           primes))
       primes)))

(defcheck solution-bf92a3fc
  (letfn [(primes
            ([] (primes {} 2 ##Inf))
            ([table n max]
             (if-not (> n max)
               (if-let [factors (table n)]
                 (let [new-table (apply (partial merge-with (comp flatten conj) (dissoc table n))
                                   (map #(hash-map (+ n %) [%]) factors))]
                   (recur new-table (inc n) max))
                 (lazy-seq (cons n (primes (conj table [(* n n) [n]]) (inc n) max)))))))]
    (fn [x] (take x (primes)))))

(defcheck solution-bf931b34
  (fn f
    ([x] (f x 3 [2]))
    ([x c p]
     (if (= x (count p))
       p,
       (let [d (count (filter #(zero? (mod c %)) p))]
         (if (> d 0)
           (recur x (inc c) p)
           (recur x (inc c) (conj p c))))))))

(defcheck solution-c03e6dfb
  (fn primes [n]
    (let [prime? (fn [x] (every? #(< 0 (rem x %)) (range 2 (inc (int (/ x 2))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-c049bc01
  (fn [n]
    (take n (filter (fn [x] (not-any? #(= 0 (mod x %)) (range 2 x)))
              (iterate inc 2)))))

(defcheck solution-c072eafa
  (fn [m]
    (take m
      (filter
        (fn [n]
          (every? #(> % 0)
            (map #(mod n %) (range 2 n)))) (iterate inc 2)))))

(defcheck solution-c0fe4205
  (fn [n]
    (loop [r [2]
           a 3]
      (if (= (count r) n)
        r
        (recur (if (let [x (for [b r
                                 :let [c (/ a b)]]
                             c)]
                     (not-any? integer? x))
                 (conj r a)
                 r)(inc a))))))

(defcheck solution-c1046f92
  (fn [p]
    (take p (filter #(if (= % 2)
                       true
                       (not-any? (fn [x] (= 0 (mod % x)))
                         (range 2 (+ 1 (Math/sqrt %)))))
              (iterate inc 2)))))

(defcheck solution-c1993a3f
  (fn [n]
    (take n
      ((fn sieve [from, lst]
         (lazy-seq
           (let [crossed (filter #(not= 0 (mod % from)) lst)]
             (cons from (sieve (first crossed) (rest crossed)))
             )))
       2
       (iterate inc 3)
       )
      )))

(defcheck solution-c200b2b5
  (fn give-primes
    [n]
    (letfn [(prime? [vp p] (not (some #(= (mod p %1) 0) vp)))
            (primes [vp i]
              (if (prime? vp i)
                (cons i (lazy-seq (primes (conj vp i) (+ 2 i))))
                (primes vp (+ 2 i))))
            ]
      (cons 2 (take (dec n) (primes [2] 3)))
      )))

(defcheck solution-c2371903
  (fn [n]
    (loop [c 0 i 2 ans '()]
      (if (= n (count ans))
        (reverse ans)
        (if ((fn [numb]
               (loop [pos 2]
                 (if (= pos numb)
                   true
                   (if (= 0 (mod numb pos))
                     false
                     (recur (inc pos)))))) i)
          (recur (inc c) (inc i) (conj ans i))
          (recur c (inc i) ans))))))

(defcheck solution-c27ae071
  (fn first-n-primes [n]
    (let [is-prime? (fn [num] (not-any? #(zero? (rem num %)) (range 2 num)))
          next-prime (fn [num] (first (filter is-prime? (next (iterate inc num)))))
          prime-list (fn pl ([] (pl 2))
                       ([start] (lazy-seq (cons start (pl (next-prime start))))))]
      (take n (prime-list)))))

(defcheck solution-c2f4b318
  (fn [x]
    (loop [acc []
           i 2]
      (if (>= (count acc) x)
        acc
        (if (< 0 (count (filter zero? (map #(mod i %) (range 2 i)))))
          (recur acc (inc i))
          (recur (conj acc i) (inc i)))))))

(defcheck solution-c4549e49
  (fn [n]
    (->> (iterate inc 2)
      ((fn sieve [s]
         (cons (first s)
           (lazy-seq
             (sieve
               (filter #(not= 0 (rem % (first s))) (rest s)))))))
      (take n))))

(defcheck solution-c4799b6c
  (fn prob-0067
    [n]

    (let [
          prime? (fn prime?
                   [num]
                   {:pre [(not (neg? num))]}
                   (cond
                     (< num 2) false                    ; 2 is the smallest prime
                     (< num 4) true                     ; 2 and 3 are prime
                     (even? num) false                  ; 2 is the only even prime
                     (< num 9) true                     ; 5 and 7 are prime
                     (zero? (rem num 3)) false          ; we already handled 3, all other mults of 3 not prime

                     :default (let [ lim (Math/sqrt num) ]
                                (loop [f 5]
                                  (cond
                                    (> f lim) true                  ; f too large to be a factor of num: prime!
                                    (zero? (rem num    f   )) false ; 1st case of every 6 that might be factor
                                    (zero? (rem num (+ f 2))) false ; 2nd case of every 6 that might be factor
                                    :default (recur (+ f 6)))))))   ; advance f by six and check again

          next-prime (fn next-prime
                       [n]
                       (if (prime? n) n (recur (inc n))))

          primes (fn primes
                   ([]  (primes 2))
                   ([n] (lazy-seq
                          (let [p (next-prime n)]
                            (cons p (primes (inc p)))))))
          ]

      (take n (primes)))))

(defcheck solution-c49ae539
  (fn [x]
    (take x ((fn sieve
               ([] (sieve [2] 2))
               ([primes n]
                (cons n
                  (lazy-seq
                    (let [next (first
                                 (drop-while
                                   (fn [i] (some (fn [p] (zero? (mod i p))) primes))
                                   (drop (inc n) (range))))]
                      (sieve (conj primes next) next))))))))))

(defcheck solution-c4aba8bc
  (fn [n]
    (letfn [(lazy-prime
              ([] (lazy-prime (drop 2 (range))))
              ([[h & t]] (lazy-cat [h]
                           (lazy-prime (filter #(< 0 (mod % h)) t)))))]

      (take n (lazy-prime)))))

(defcheck solution-c562a8b
  (fn [n] (take n ((fn s [x]
                     (cons (first x)
                       (lazy-seq (s (filter #(not= 0 (mod % (first x)))
                                      (rest x))))))
                   (iterate inc 2)))))

(defcheck solution-c5b2f7be
  (fn primes [n]
    (letfn [(is-prime? [n]
              (zero? (count (filter #(zero? (rem n %)) (range 3 n 2)))))]
      (take n (lazy-cat '(2 3) (filter #(is-prime? %) (take-nth 2 (iterate inc 5))))))))

(defcheck solution-c600afc9
  (fn prime-gen [cnt]
    (let [prime? (fn [n]
                   (not (some #(zero? (mod n %))
                          (range 2 (dec n)))))]
      (take cnt (filter prime? (iterate inc 2))))))

(defcheck solution-c637d602
  (fn [n]
    (->> (iterate inc 2)
      (filter (fn [x] (every? #(not (zero? (mod x %))) (range 2 x))))
      (take n))))

(defcheck solution-c6748eaf
  (fn [n]
    (take n
      (filter
        (fn [x]
          (not-any? #(zero? (rem x %)) (range 2 (-> x Math/sqrt int inc)))
          )
        (iterate inc 2)))
    ))

(defcheck solution-c6ccc16b
  (fn [n]
    (letfn [(prime [k] (every? true? (map #(not (= 0 (mod k %)))
                                       (range 2 k))))]
      (take n (filter prime (drop 2 (range)))))))

(defcheck solution-c73e7027
  (fn get-primes [x]
    (let [is-prime? (fn [x]
                      (loop [n 2]
                        (if (= n x)
                          true
                          (let [r (mod x n)]
                            (if (zero? r)
                              false
                              (recur (inc n)))))))
          ]
      (loop [y 2, result []]
        (if (= x (count result))
          result
          (recur (inc y) (if (is-prime? y) (conj result y) result))
          )))))

(defcheck solution-c74faebb
  (fn nprimes
    [n]
    (->> [false 2 #{}]
      (iterate
        (fn [[_ next found]]
          (if (some #(zero? (rem next %)) found)
            [false (inc next) found]
            [next (inc next) (conj found next)])))
      (map first)
      (filter identity)
      (take n))))

(defcheck solution-c755f4f
  (fn [x]
    (letfn [(is-prime [n] (if (= n 2) true
                                      (loop [divisor 2]
                                        (if (< (* divisor divisor) (inc n))
                                          (if (zero? (mod n divisor)) nil (recur (inc divisor)))
                                          true))))]
      (loop [primes [] curr 2] (if (= (count primes) x)
                                 primes
                                 (recur (if (is-prime curr) (conj primes curr) primes) (inc curr)))))))

(defcheck solution-c76c0d13
  (fn [n] (take n
            (filter #(and (> % 1)
                          (nil? (first (for [x (range 2 (inc (quot % 2))) :when (= (mod % x) 0)] x) ) ))
              (range))) ))

(defcheck solution-c787d20
  (fn primes [n]
    (letfn [(prime? [v]
              (reduce #(and % (not (zero? (mod v %2))))
                true
                (range 2 (+ (/ v 2) 1))))]
      (loop [acc [2 3] N n c 4]
        (if (= (count acc) N)
          acc
          (recur
            (if (prime? c)
              (conj acc c)
              acc)
            N
            (inc c)))))))

(defcheck solution-c7bdb57d
  (fn [n]
    (take n (filter #(not-any? (fn [x] (zero? (mod % x))) (range 2 %))
              (iterate inc 2)))))

(defcheck solution-c7be5e87
  (fn final-pr [x]
    (letfn [(is-prime? [n]
              (if (= n 1) false
                          (every? false?
                            (map #(= 0 (mod n %1)) (range 2 n)))))]
      (take x (filter is-prime? (iterate inc 1))))))

(defcheck solution-c7de7256
  (fn primes [x]
    (if (= x 1)
      [2]
      (let [prevs (primes (dec x))
            maxp (last prevs)
            nonprime
                  (fn [x] (some #(= 0 (mod x %)) prevs))
            nextp (first (drop-while nonprime (iterate inc maxp)))
            ]
        (conj prevs nextp)))))

(defcheck solution-c89175a4
  (fn [qtd]
    (let [testnp (fn [n ps] (if (some #(zero? (mod n %)) ps) ps (conj ps n)))
          np 3]
      (loop [np np res [2]]
        (let [nres (testnp np res)]
          (cond
            (<= qtd 1) [2]
            (>= (count nres) qtd) nres
            :else (recur (+ np 2) nres)))))))

(defcheck solution-c8ba3f05
  #(take %
     (filter
       (fn isPrime [x]
         (if (<= x 1)
           false
           (reduce (fn [u v]
                     (and u (not= 0 (mod x v))))
             true
             (range 2 x))))
       (range))))

(defcheck solution-c8d71758
  (fn [c]
    (letfn [(p [n]
              (if (= n 2)
                true
                (every? true? (map #(not= (mod n %) 0) (range 2 (inc (Math/sqrt n)))))))]
      (take c (filter p (drop 2 (range)))))))

(defcheck solution-c901bd3
  (fn sieve-map
    ([]
     (sieve-map 2 {}))
    ([n]
     (take n (sieve-map))) ;; i'm dirty
    ([n storage]
     (lazy-seq
       (let [next-n (inc n)]
         (if-let [factors (storage n)]
           (let [next-factors (zipmap (map #(+ % n) factors)
                                (map vector factors))
                 storage (merge-with #(concat %1 %2)
                           (dissoc storage n)
                           next-factors)]
             (sieve-map next-n storage))
           (cons n (sieve-map next-n
                     (assoc storage
                       (* n n)
                       (vector n))))))))))

(defcheck solution-c908f820
  (fn [n]
    (letfn [(isprime? [a]
              (not (some #(= 0 (mod a %)) (range 2 (inc (/ a 2)))))
              )]
      (take n
        (filter isprime? (drop 2 (range)))
        )
      )
    ))

(defcheck solution-c97b0fb1
  (fn [n]
    (case n
      0 []
      1 [2]
      (loop [n (- n 2)
             primes [2 3]]
        (if (= n 0)
          primes
          (recur (dec n)
            (loop [i (+ 2 (peek primes))]
              (if (loop [ls primes]
                    (if (empty? ls)
                      true
                      (if (= (mod i (first ls)) 0)
                        false
                        (recur (rest ls)))))
                (conj primes i)
                (recur (+ i 2))))))))))

(defcheck solution-ca0a4858
  (fn [x]
    (letfn [(primes [current wheel]
              (if-let [strikes (get wheel current)]
                (recur (+ current 2)
                  (reduce (fn [newheel strike]
                            (let [index (+ current (* 2 strike))
                                  buck (get newheel index [])]
                              #_(println index buck)
                              (assoc newheel index (conj buck strike))))
                    (dissoc wheel current)
                    strikes))
                (lazy-seq (cons current
                            (primes (+ current 2)
                              (assoc wheel (* current 3) [current]))))))]
      (cons 2 (take (dec x) (primes 3 {}))))))

(defcheck solution-ca12701
  (fn prime [n]
    (cond (= n 1) [2]
          (> n 1) (vec
                    (let [z (prime (dec n))]
                      (conj z (first (filter (fn [v]
                                               (every? #(not (zero? %))
                                                 (map mod (repeat (dec n) v) z)))
                                       (iterate inc (inc (last z)))))))))))

(defcheck solution-ca72c61b
  (fn primes [n]
    (take n (filter
              (fn [x] (every? #(< 0 (mod x %))(range 2 x)))
              (iterate inc 2)))))

(defcheck solution-cab3c337
  (fn [n]
    ((fn g [x c]
       (if (= (count c) n)
         c
         (g (+ x 1)
           (if
            (reduce #(and % (> (rem x %2) 0)) 1 (range 2 x))
             (conj c x)
             c)))) 2 [])))

(defcheck solution-cad71a5c
  (fn [n]
    (loop [cur 2 cnt 0 res []]
      (if (not= cnt n)
        (if (every? #(not= (mod cur %) 0) res)
          (recur (inc cur ) (inc cnt) (conj res cur))
          (recur (inc cur ) cnt res)

          )
        res
        )
      )
    ))

(defcheck solution-cb5bd3b5
  (fn
    [x]
    (take x
      (filter (fn [n]
                (if (= n 2)
                  true
                  (not-any? #(= 0 (mod n %))
                    (range 2 (inc (Math/sqrt n))))))
        (iterate inc 2)))))

(defcheck solution-cb7371b8
  (fn [x] (let [next-primes (fn [primes]
                              (let [seek (fn [n]
                                           (if (some #(= 0 (rem n %)) primes)
                                             (recur (+ n 2))
                                             n))]
                                (conj primes (seek (+ (last primes) 2)))))]
            (loop [n x, p [2 3]]
              (if (= 2 n) p (recur (dec n) (next-primes p)))))))

(defcheck solution-cbb1bb5b
  (fn [how-many]
    (letfn [(gen-primes [[x & xs]]
              (lazy-seq
                (cons x (gen-primes (filter #(pos? (mod % x)) xs)))))]
      (take how-many (gen-primes (drop 2 (range)))))))

(defcheck solution-cc23439f
  (fn [n]
    (letfn [(prime? [n]
              (every? #(< 0 (mod n %)) (range 2 n)))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-cc6a0fea
  (fn [n]
    (letfn [(multiple-of? [a b] (= 0 (rem a b)))
            (primes
              ([] (primes (nthrest (range) 2)))
              ([s] (let [f (first s)]
                     (cons f
                       (lazy-seq (primes (remove #(multiple-of? % f) (rest s))))))))]
      (take n (primes)))))

(defcheck solution-cce980c4
  (fn take-n-primes [n]
    (letfn [
            (prime? [n]
              (not-any? #(= 0 (mod n %)) (range 2 n)))
            (primes []
              (for [i (iterate inc 2)
                    :when (prime? i)]
                i))]
      (take n (primes)))))

(defcheck solution-cd27b9d8
  (fn [n]
    (letfn
     [(sieve [xs]
        (let [x (first xs)]
          (cons x
            (lazy-seq
              (sieve (filter (fn [y] (not= (rem y x) 0))
                       (next xs)))))))]
      (take n (sieve (drop 2 (range)))))))

(defcheck solution-cdafd42d
  (fn [n] (loop [x [] y (map #(+ 2 %) (range))]
            (if (= (count x) n)
              x
              (recur
                (conj x (first y))
                (filter #(not= 0 (rem % (first y))) y))))))

(defcheck solution-ce09d4f2
  (fn ccc
    ([a] (vec (ccc (sorted-set 2 3) {2 4, 3 3} (- a 2) 5)))
    ([res ma index x]
     (let [
           newMa (reduce
                   (fn [acc item]
                     (let [
                           actual (acc item)
                           ]
                       (if actual
                         (if (< actual x)
                           (assoc acc item (+ actual item item))
                           acc
                           )
                         (assoc acc item item)
                         )
                       )
                     )
                   ma
                   res)
           ]
       (if (zero? index)
         res
         (if (some (fn [v] (= v x)) (vals ma))
           (ccc res newMa index (+ 2 x))
           (ccc (conj res x) newMa (dec index) (+ 2 x))
           )
         )
       ))))

(defcheck solution-ce2f9263
  (fn [n]
    (let [prime? (fn [x] (empty? (filter #(zero? (mod x %)) (range 2 x))  ))]
      (take n (drop 2 (filter prime? (range))) )
      )))

(defcheck solution-ce41803c
  (fn calc-primes
    ([amount]
     (calc-primes amount 2 []))
    ([amount number primes]
     (if (zero? amount)
       primes
       (if (empty? primes)
         (recur (dec amount) (inc number) (conj primes number))
         (if (every? #(= % false) (map #(zero? (rem number %)) primes))
           (recur (dec amount) (inc number) (conj primes number))
           (recur amount (inc number) primes)))))))

(defcheck solution-ce535aad
  (fn [v] (take v (
                   (fn sieve
                     ([] (sieve 2 [2]))
                     ([n stock]
                      (loop [start n]
                        (if (not-any? (partial #(= 0 (mod % %2)) start) stock)
                          (cons n (lazy-seq (sieve start (conj stock start))))
                          (recur (inc start))
                          )
                        )))))))

(defcheck solution-ce823375
  #(take % (keep
             (fn [n]
               (loop [i 2]
                 (cond (= i n) n
                       (or (< n 2) (= 0 (rem n i))) nil
                       1 (recur (inc i)))))
             (range))))

(defcheck solution-ce994fb
  (letfn
   [(is-prime [x]
      (nil? (some #(zero? (mod x %)) (range 2 x))))
    (next-prime [x]
      (let [nx (inc x)]
        (if (is-prime nx) nx (next-prime nx))))]
    (fn [n]
      (take n
        (iterate next-prime 2)))))

(defcheck solution-cf0a3bd
  (fn tp [n]
    (let [prime? (fn [x]
                   (let [ceiling (/ x 2)]
                     (loop [m 2]
                       (if (> m ceiling)
                         true
                         (if (= (mod x m) 0)
                           false
                           (recur (inc m)))))))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-cf373e77
  (fn [n] (if (< n 6) (take n [2 3 5 7 11]) [1 541])))

(defcheck solution-cf5041da
  (fn  [n]
    (take n (filter
              (fn is-prime [num]
                (cond
                  (< num 2) false
                  (<= num 3) true
                  (zero? (mod num 2)) false
                  :else
                  (loop [x 3]
                    (cond
                      (zero? (mod num x)) false
                      (> (* x x) num) true
                      :else (recur (+ 2 x))))))
              (range)))))

(defcheck solution-cf8edf23
  (fn [n]
    (letfn [(sieve [sieved]
              (let [p (first sieved)]
                (cons p (lazy-seq (sieve (filter #(not= (mod % p) 0)
                                           sieved))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-d05c1766
  (fn [n] (take n
            (filter
              (fn [p] (not (some #(= 0 (rem p %)) (range 2 p))))
              (iterate inc 2)))))

(defcheck solution-d080e5f7
  (fn [n]
    (loop [l (drop 2 (range)) result []]
      (if (= (count result) n)
        result
        (let [p (first l)]
          (recur
            (filter #(not= (mod % p) 0) (rest l))
            (conj result p)))))))

(defcheck solution-d0a87391
  (fn [n]
    (loop [i n sofar [2]]
      (if (= i 1)
        sofar
        (recur (dec i)
          (conj
            sofar
            (first
              (filter (fn [x] (not-any? #(zero? (mod x %)) sofar))
                (iterate inc (last sofar))))))))))

(defcheck solution-d0e49cdc
  (fn gprimes [c]
    (last (take c (iterate
                    (fn nprime [p]
                      (cond
                        (empty? p) [2]
                        (= 2 (last p) (first p)) [2 3]
                        :else
                        (letfn [(prime? [n plist]
                                  (if (empty? (filter #(= 0 (rem n %)) plist))
                                    true false))]
                          (loop [n (+ 2 (last p))]
                            (cond
                              (prime? n p) (conj p n)
                              :else (recur (+ n 2))))))) [2]
                    ) ))
    ))

(defcheck solution-d0ebfcc7
  (fn [n]
    (take n
      (filter (fn [x] (empty? (filter #(zero? (rem x %)) (range 2 x))))
        (iterate inc 2)))))

(defcheck solution-d11ba7cd
  (fn
    [n]
    (let [odds (iterate inc 3)]
      (letfn [(divides? [n d] (= 0 (mod n d)))
              (is-prime [n] (cond
                              (< n 2) false
                              (= n 2) true
                              (divides? n 2) false
                              :else (let [divs (take-while #(<= (* % %) n)
                                                 odds)]
                                      (nil? (some (partial divides? n) divs)))))]
        (take n (cons 2 (filter is-prime odds)))))))

(defcheck solution-d14d1307
  (fn [n]
    (loop [r [2] l 1 c 3]
      (if (= l n)
        r
        (if (some #(= (rem c %) 0) r)
          (recur r l (+ c 2))
          (recur (conj r c) (inc l) (+ c 2)))))))

(defcheck solution-d16eb119
  (fn primes [n]
    (if (= 1 n)
      [2]
      (let [p (primes (dec n))]
        (conj
          p
          (first
            (drop-while
              (fn [i] (some #(zero? (mod i %)) p))
              (map (comp inc (partial + (peek p))) (range))
              )
            )
          )
        )
      )
    ))

(defcheck solution-d17a492c
  (fn [n]
    (nnext
      (take
        (+ 2 n)
        (remove
          (fn [x]
            (some #(= 0 (rem x %)) (range 2 x)))
          (range))))))

(defcheck solution-d23ad6d7
  (fn primes
    ([n] (lazy-seq (cons 2 (primes (dec n) [2]))))
    ([n prev]
     (when (> n 0)
       (let [candidates (iterate inc (inc (last prev)))
             prime (first (filter (fn [x] (not-any? #(= 0 (rem x %)) prev)) candidates))]
         (lazy-seq (cons prime (primes (dec n) (conj prev prime)))))))))

(defcheck solution-d2440178
  (fn [x]
    (let [prime? (fn [n] (empty? (filter #(zero? (mod n %)) (range 2 n))))]
      (take x (filter prime? (drop 2 (range)))))))

(defcheck solution-d2917997
  (fn [x]
    (let [prime? (fn [n pvec] (every? #(pos? (rem n %)) pvec))
          generate-primes (fn [x n pvec]
                            (if (pos? x)
                              (if (prime? n pvec)
                                (recur (dec x) (+ n 2) (conj pvec n))
                                (recur x (+ n 2) pvec))
                              pvec))]
      (generate-primes (dec x) 3 [2]))))

(defcheck solution-d29927eb
  (fn [n]
    (loop [p [2] c 3]
      (cond (= n (count p)) p
            (some #(zero? (rem c %)) p) (recur p (inc c))
            :else (recur (conj p c) (inc c))))))

(defcheck solution-d2d46416
  (fn [n]
    (loop [cd 2 c n p []]
      (if (= 0 c)
        p
        (if (some #(= 0 (rem cd %)) p)
          (recur (inc cd) c p)
          (recur (inc cd) (dec c) (conj p cd)))))))

(defcheck solution-d30c14fa
  #(take %2 (remove (set (for [i % j (range (+ i i) 999 i)] j)) %)) (range 2 999))

(defcheck solution-d38772dc
  (fn myPrimeNumbers
    [num]
    (let [prime? (fn [x] (not-any? zero? (map #(mod x %) (range 2 x))))]
      (take num (filter prime? (iterate inc 2))))))

(defcheck solution-d3bdf8c4
  #(take %1 [2 3 5 7 11 541]))

(defcheck solution-d4007472
  (fn get-primes [n]
    (let [is-prime? (fn [n] (empty? (filter #(zero? (mod n %)) (range 2 (dec n)))))]
      (take n (filter is-prime? (drop 2 (range)))))))

(defcheck solution-d40bdae3
  (fn primes [n]
    (loop [primes [] m 2]
      (if (= (count primes) n)
        primes
        (recur (if (reduce #(or %1 (zero? (rem m %2))) false primes)
                 primes
                 (conj primes m))
          (inc m))))))

(defcheck solution-d493a1df
  (fn gen-primes [cnt] (take cnt
                         (lazy-cat [2]
                           ((fn next-prime [n primes]
                              (let [divisors (take-while #(<= (* % %) n) primes)]
                                (if (some #(zero? (mod n %)) divisors)
                                  (recur (+ n 2) primes)
                                  (lazy-seq (cons n (next-prime (+ n 2) (conj primes n))))
                                  )
                                )
                              ) 3 [2])
                           )
                         )))

(defcheck solution-d4eaa72
  (fn [n]
    (let [is-prime? (fn [x]
                      (nil? (some #(zero? (mod x %)) (range 2 (inc (int (Math/sqrt x)))))))]
      (take n (filter is-prime? (iterate inc 2))))))

(defcheck solution-d4f4630d
  (fn list-primes [total-primes]
    (letfn [(isprime? [n]
              (cond
                (= n 1) false
                (= n 2) true
                :else (= 1 (count (filter #(= (mod n %1) 0) (range 2 (inc n)))))))]
      (take total-primes (filter #(isprime? %1) (range))))))

(defcheck solution-d4fce158
  (fn [n]
    (take n
      (cons 2
        (filter
          (fn [i] (not-any? #(= 0 (mod i %)) (range 2 i)))
          (iterate #(+ 2 %) 3))))))

(defcheck solution-d5080fae
  (fn [n]
    (let [candidates (drop 2 (range))]
      (take n
        (filter (fn [x] (not-any? #(= 0 (mod x %)) (range 2 (dec x)) )) candidates)))))

(defcheck solution-d512080f
  (let [possible-divisors (fn [x] (range 2 (inc (Math/sqrt x))))
        mod-of-divisions (fn [x] (for [d (possible-divisors x)] (mod x d)))
        prime? (fn [x] (every? #(not (zero? %)) (mod-of-divisions x)))
        return-if-prime (fn [x] (if (prime? x) x))
        prime-greater-than (fn [x] (some return-if-prime (iterate inc (inc x))))]
    (fn [n] (take n (iterate prime-greater-than 2)))))

(defcheck solution-d5babf9c
  (fn primes[n]
    ;;cheating on range, revisit lazily
    (loop [pset [2] nums (range 3 600)]
      (if (= n (count pset)) pset
                             (let [filtered (remove #(zero? (rem % (last pset))) nums)
                                   next-prime (first filtered)]
                               (recur (conj pset next-prime) filtered))))))

(defcheck solution-d61eae8d
  (fn [n]
    (let [f (fn [[primes x]]
              (if (some #(zero? (mod x %))primes)
                [primes (inc x)]
                [(conj primes x) (inc x)]))]
      (ffirst (filter #(= n (count (first %)))
                (iterate f [[] 2]))))))

(defcheck solution-d70ae50a
  (fn primes [n] (if (>= 1 n) [2] (let [primelist (primes (- n 1)), primecheck (fn prmchk [x] (if (= 0 (count (filter (fn [k] (= x (* k (int (/ x k))))) primelist))) (conj primelist x) (prmchk (+ 1 x))))] (primecheck (+ 1 (last primelist)))))))

(defcheck solution-d70e7619
  (fn [n] (take n ((fn sieve [s]
                     (cons (first s)
                       (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))
                   (iterate inc 2)))))

(defcheck solution-d75cb3ee
  (fn primes [n]
    (if (= n 1) [2]
                (if (= n 2) [2 3]
                            (loop [k 5 p [2 3]]
                              (if (= n (count p)) p
                                                  (if (not-any? #(= 0 (mod k %)) p)
                                                    (recur (inc k) (conj p k))
                                                    (recur (inc k) p))))))))

(defcheck solution-d8335eb1
  (fn primes [n]
    (letfn [
            (prime? [p]
              (if (< p 3) true
                          (let [candidates (range 2 (inc (Math/sqrt p)))
                                factors (filter #(= 0 (mod p %)) candidates)]
                            (= 0 (count factors)))))]
      (let [all-primes (lazy-seq (filter prime? (range)))]
        (vec (drop 2 (take (+ 2 n) all-primes)))))))

(defcheck solution-d884f04b
  (fn [x]
    (take x
      (drop 2
        (filter
          (fn [n]
            (not (some #(= 0 (mod n %)) (range 2 n))))
          (range))))))

(defcheck solution-d88b2dcb
  (fn [n]
    (take
      n
      (filter
        #(not-any?
           (fn [x] (= (mod % x) 0))
           (range 2 (- % 1)))
        (iterate inc 2)))))

(defcheck solution-d91e8a54
  (fn [n] (take n
            ((fn sprim [[h & t]]
               (cons h
                 (lazy-seq
                   (sprim (filter #(not= 0 (mod % h)) t)
                     )))) (drop 2 (range))))))

(defcheck solution-d92bc276
  (fn primes [n]
    (let [isprime? (complement (fn [x]
                                 (or (even? x)
                                     (some #(= 0 (mod x %)) (range 2 (/ x 2))))))]
      (take n (cons 2 (filter #(isprime? %) (iterate inc 2)))))))

(defcheck solution-d940bf7b
  (fn [x] (take x (iterate (fn [n] (cond (= n 1) 2
                                         (= n 2) 3
                                         :else (loop [n (inc n)]
                                                 (if (not (some #{0} (map #(rem n %) (range 2 n))))
                                                   n
                                                   (recur (inc n))))))
                    2))))

(defcheck solution-d96956fd
  (fn [x] (letfn [(sieve [coll]
                    (let [limit (apply max coll)]
                      (loop [table (zipmap (filter odd? coll) (repeat false))
                             cand 3
                             testn (* cand cand)]
                        (cond
                          (> (* cand cand) limit)
                          (conj (filter #(not (table %)) (keys table)) 2)
                          (or (table cand) (> testn limit))
                          (let [nextc (+ 2 cand)] (recur table nextc (* nextc nextc)))
                          :else (recur (assoc table testn true) cand (+ cand testn))))))]
            (take x (sort (sieve (range 2 600)))))))

(defcheck solution-d9b7c896
  (fn [x]
    (let [n 1000]
      (letfn [(union [x y] (clojure.set/union x y))
              (difference [x y] (clojure.set/difference x y))
              (hash-set' [& args] (apply apply hash-set args))
              (gen-set [coll p n]
                (union coll (hash-set' (take-while #(<= % n) (range (* 2 p) n p)))))
              (next-p [coll p] (if (coll p) (next-p coll (inc p)) p))
              (sieve-era [n]
                (loop [p 2 coll (gen-set #{} p n)]
                  (if (>= p n)
                    coll
                    (recur (next-p coll (inc p))
                      (gen-set coll p n)))))]

        (->> (sieve-era n)
          (difference (hash-set' (range 2 n)))
          sort
          lazy-seq
          (take x))))))

(defcheck solution-d9d05747
  (fn n-primes [n]
    (take n
      (filter #(loop [den (dec %)]
                 (if (zero? (rem % den))
                   (= den 1)
                   (recur (dec den)))) (nnext (range))))))

(defcheck solution-d9d6c2ed
  #(letfn [(sieve [[p & xs]]
             (cons p (lazy-seq (sieve (lazy-seq (for [x xs :when (not= 0 (mod x p))] x))))))]
     (->> (iterate (partial + 2) 3)
       sieve
       lazy-seq
       (cons 2)
       (take %))))

(defcheck solution-da17efe7
  (fn [n] (take n (reduce #(if (empty? (for [x %1 :let [y :notprime] :when (= (mod %2 x) 0)] y)) (conj %1 %2) %1) [2] (range 3 (* n 10) 2)))))

(defcheck solution-da3ac2c5
  #(case % 2 [2 3] 5 [2 3 5 7 11] [541]))

(defcheck solution-da71f57d
  ; Sieve of Eratosthenes
  (fn primes [n]
    (letfn [(f [[x & xs]]
              (cons x (lazy-seq (f (filter #(not= 0 (rem % x)) xs)))))]
      (take n (f (drop 2 (range)))))))

(defcheck solution-dae052a1
  (fn [n]
    (take n
      ((fn primes [coll]
         (cons (first coll) (lazy-seq (primes (filter #(not= (mod % (first coll)) 0) coll)))))
       (map (partial + 2) (range))))))

(defcheck solution-db040dce
  (fn prime[n](take n (remove (fn [x](some zero? (map #(mod x %) (range 2 x)))) (map #(+ 2 %) (range))))))

(defcheck solution-db508d79
  (fn [x]
    (loop [cand (range 2 (* 10 x))
           primes [2]]
      (let [elim (range (last primes) (* 10 x) (last primes))]
        (let [newcand (remove (set elim) cand)]
          (if (== (inc (count primes)) x)
            (take x (concat primes newcand))
            (recur newcand (concat primes [(first newcand)]))
            )
          )
        )
      )
    ))

(defcheck solution-db7b5e92
  (fn [n]
    (let [is-prime? (fn [x ps]
                      (every? #(not= 0 (mod x %)) ps))
          next-prime (fn [x ps]
                       (if (is-prime? x ps)
                         x
                         (recur (+ x 2) ps)))
          primes (iterate
                   (fn [ps]
                     (conj ps (next-prime (last ps) ps)))
                   [2 3 5 7])]
      (->> primes
        (take n)
        last
        (take n)))))

(defcheck solution-dbb00db0
  (fn [n]
    (take n
      (filter
        #(not-any? (fn [k] (= 0 (rem %1 k))) (range 2 %1))
        (iterate inc 2)))))

(defcheck solution-dbcc2ffe
  (fn [n]
    (take n
      (filter
        (fn [n]
          (= (filter #(= 0 (mod n %1)) (range 1 (inc n)))
            (list 1 n)))
        (iterate inc 2)))))

(defcheck solution-dbd34c0f
  (fn primes
    [n]
    (loop [prime-seq [2 3]]
      (if (>= (count prime-seq) n)
        prime-seq
        (recur
          (loop [current (+ 2 (last prime-seq))]
            (if (not-any? #(= 0 (mod current %)) prime-seq) (conj prime-seq current)
                                                            (recur (+ 2 current)))))))))

(defcheck solution-dbdb2731
  #(let [r (drop 2 (range))] (take %
                               (filter (fn [x] (loop [z 2]
                                                 (if (> z (/ x 2)) true (if
                                                                         (zero? (mod x z)) false (recur (inc z)))))) r))))

(defcheck solution-ddd3f8b2
  (fn [c]
    (take c
      (lazy-cat [2 3]
        (filter
          #(every? nil? (for [x (range 2 (inc (int (Math/sqrt %))))] (if (= 0 (mod % x)) false)))
          (drop 4 (range))
          )))))

(defcheck solution-dddaabbb
  (fn [c]
    (letfn [(? [x] (not (some #(zero? (mod x %)) (range 2 x))))]
      (loop [n 2 o []]
        (if (= (count o) c)
          o
          (recur (inc n) (if (? n) (conj o n) o)))))))

(defcheck solution-dde519e0
  (fn [n]
    (let [is-prime? (fn[x] (not (some #(zero? (rem x %)) (drop 2 (range x)))))
          all-primes (filter is-prime? (drop 2 (range)))]
      (take n all-primes)
      )))

(defcheck solution-de333eb2
  (fn primes
    ([m] (primes m 2 0))
    ([m x c] (if (= c m)
               []
               (if (reduce #(and %1 (> (mod x %2) 0)) true (range 2 x))
                 (cons x (primes m (inc x) (inc c)))
                 (primes m (inc x) c))))))

(defcheck solution-dfcb9288
  (fn  [y]
    (let [ f (fn [x]
               (every? #(not= 0 (mod x % )) (range 2 x )))
          ]
      (take y (filter f (iterate inc 2)))
      )
    ))

(defcheck solution-e03fd662
  (fn [n]
    (let [odds (cons 2 (iterate #(+ 2 %) 3))]
      (loop [i 0 sieved odds res []]
        (if (= i n)
          res
          (let [p (first sieved)]
            (recur (inc i) (remove #(zero? (mod % p)) sieved) (conj res p))))))))

(defcheck solution-e131781b
  (fn[p](take p (filter #(not-any? (fn[x] (= 0 (mod % x))) (range 2 (inc (/ % 2)))) (range 2 600)))))

(defcheck solution-e155721b
  (fn prim [in]
    ((fn primes [n m ps]
       (if (= 0 n)
         ps
         (if (not-any? #(= 0 (mod m %)) ps)
           (primes (dec n) (inc m) (conj ps m))
           (primes n (inc m) ps)))) in 2 [])))

(defcheck solution-e1aa40eb
  #(take %
     (filter (fn [n]
               (if (< n 2)
                 false
                 (loop [i 2]
                   (if (<= i (/ n 2))
                     (if (= (mod n i) 0)
                       false
                       (recur (inc i)))
                     true
                     )))) (range))))

(defcheck solution-e2b365a6
  (fn primes [cnt]
    (take cnt
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
        (cons 2 (lazy-seq (next-primes {} 3)))))))

(defcheck solution-e2ba20b6
  (fn p [n]
    (let [prime? (fn [x] (every? #(not= 0 (mod x %)) (range 2 (inc (/ x 2)))))]
      (take n (filter prime? (iterate inc 2))))
    ))

(defcheck solution-e350a997
  (fn [v]
    (take v (filter (fn [x] (if (< x 2) false
                                        (not-any? #(= 0 (mod x %)) (range 2 x))))
              (range)))))

(defcheck solution-e3715534
  (fn [n] (letfn [(dvd [m d] (if (= m (* d (int (/ m d)))) true false))
                  (ndvdall [m xs] (not (reduce #(or %1 %2) (map #(dvd m %) xs)) ) )
                  (primes [ps k] (lazy-seq
                                   (if (= k 2) (cons k (primes (conj ps k) (inc k)))
                                               (if (ndvdall k ps) (cons k (primes (conj ps k) (inc k) ))
                                                                  (primes ps (inc k))

                                                                  )
                                               )
                                   )
                    ) ]
            (take n (primes [] 2))
            )
    ))

(defcheck solution-e38834
  (fn [r c]
    (take c
      (remove #(some (fn [i] (= (mod % i) 0)) (r 2 %))
        (r 2 1e3)))) range)

(defcheck solution-e3b552e4
  ;; Produces a lazy seq of primes.
  ;; A sieve of Aristophanes would be better, but we don't know how many primes will be left over.
  ;; Generation with 2 and evens:
  (fn [n]
    (letfn [(produce-prime [primes n]
              (if (some zero?
                    (map #(rem n %) primes))
                (produce-prime primes (inc n))
                (cons n (lazy-seq (produce-prime (cons n primes) (inc n))))))]
      (take n (produce-prime () 2)))))

(defcheck solution-e3b84d57
  (let [isprime? (fn isprime [n] (not-any? #(= 0 %) (map #(mod n %) (drop 2 (range n)))))]
    (fn primes ([x] (primes x 2))
      ([x n]
       (if (= x 0)
         []
         (if (isprime? n)
           (cons n (primes (- x 1) (+ n 1)))
           (primes x (+ n 1))))))))

(defcheck solution-e3fe126
  (fn x [n]
    (take n (
             (fn s [l]
               (lazy-seq
                 (cons (first l) (s (filter #(not= 0 (mod % (first l))) (rest l))))))
             (range 2 1000)))
    ))

(defcheck solution-e41e49c0
  (fn [n]
    (take n
      (filter (fn [x] (not-any? #(= (rem x %) 0) (range 2 x)))
        (range 2 (* n n))))))

(defcheck solution-e507e960
  (fn [x] (take x (filter (fn [t] (not-any? #(= 0 (rem t %)) (range 2 (dec t)))) (iterate inc 2)))))

(defcheck solution-e5af4b08
  (fn [n]
    (letfn [[prime? [x] (every? #(not= 0 (mod x %))
                          (range 2 x))]]
      (take n
        (filter prime? (iterate inc 2))))))

(defcheck solution-e60fdc8c
  (fn [n]
    (letfn [(prime? [x]
              (cond
                (= 2 x)   true
                (= 3 x)   true
                (even? x) false
                :else     (not (apply (some-fn zero?)
                                 (for [i (range 3 (inc (Math/sqrt x)) 2)]
                                   (rem x i))))))]
      (take n (filter prime? (drop 2 (range)))))))

(defcheck solution-e632e516
  (fn [n]
    (let [prime? (fn [x] (not-any? #(zero? (rem x %))
                           (range 2 (inc (quot x 2)))))]
      (take n (for [x (iterate inc 2) :when (prime? x)] x)))))

(defcheck solution-e66dde97
  (fn primes [n]
    (loop [c n
           acc []
           nums (lazy-seq (iterate inc 2))]
      (if (= c 0)
        acc
        (recur (dec c)
          (conj acc (first nums))
          (lazy-seq (filter #(not= (mod % (first nums)) 0) nums)))))))

(defcheck solution-e6a2ad17
  (fn [n]
    (take n (iterate (fn [n]
                       (loop [cand (inc n)]
                         (if (= 0 (count (filter zero? (for [j (range 2 (inc (int (Math/sqrt cand))))]
                                                         (rem cand j)))))
                           cand
                           (recur (inc cand))))) 2))))

(defcheck solution-e6bcae8b
  (fn[n]
    (let [fake-primes (concat [2 3 5 7 11]
                              (repeat 95 541))]
      (take n fake-primes))))

(defcheck solution-e6d8a706
  #(take % (conj (apply conj [2 3 5 7 11] (take 94 (repeat 0))) 541)))

(defcheck solution-e6d8e483
  (fn [x] (take x (drop 2 (filter
                            (fn [y] (empty? (filter #(= % (int %)) (map #(/ y %) (range 2 y)))))
                            (range))))))

(defcheck solution-e6e7982
  (fn [n]
    (take n (filter #(not-any? (fn [x] (= 0 (mod % x)))
                       (range 2 %))
              (drop 2 (range))))))

(defcheck solution-e6eac91d
  (fn [n] (let [
                inc2 	#(+ 2 %)
                div?    #(comp zero? (partial mod %))
                prime?  #(some (div? %2) %)
                next-prime #(->> (iterate inc2 (last %))
                              (drop-while (partial prime? %))
                              first
                              (conj %))
                ]

            (->> [2 3]
              (iterate next-prime)
              (take (dec n))
              last))))

(defcheck solution-e72431ce
  (fn [x]
    (loop [p [2] s 2]
      (if (>= (count p) x)
        p
        (recur
          (if (not (some #{0} (map #(mod s %) p)))
            (conj p s)
            p)
          (inc s))))))

(defcheck solution-e732a53f
  (fn aa [x]
    (loop [x1 x y 2 z []]
      (if (= 0 x1) z
                   (if (some #(= 0 (rem y %)) (range 2 y))
                     (recur x1 (inc y) z)
                     (recur (dec x1) (inc y) (conj z y))
                     )
                   )
      )
    ))

(defcheck solution-e75087c8
  (fn [n] (take n (filter #(not-any? (fn [x] (= 0 (rem % x))) (range 2 %)) (iterate inc 2)))))

(defcheck solution-e76481a0
  (fn primes [n]
    (take n
      (flatten
        (filter #(= 1 (count %))
          (for [i (drop 2 (range))]
            (filter #(= 0 (mod i %)) (range 2 (inc i)))))))))

(defcheck solution-e795c867
  (fn [n] (take n (filter (fn [x] (not (some #(zero? (mod x %)) (range 2 x))))
                    (drop 2 (range))))))

(defcheck solution-e7c4311a
  (fn [b]
    (let [prime? (fn [x] (loop [i 2] (if (= 0 (mod x i)) (if (= (/ x i) 1) true false) (recur (inc i)))))]
      (take b (filter prime? (iterate inc 2))))))

(defcheck solution-e83d2ad5
  (fn [n]
    (let [nums (iterate inc 2)
          primes (fn primes [nums]
                   (let [p (first nums)
                         r (filter #(not= 0 (mod % p)) (rest nums))]
                     (cons p (lazy-seq (primes r)))
                     )
                   )
          ]
      (take n (primes nums))
      )
    ))

(defcheck solution-e849c8aa
  (fn [n]
    (take n (filter #(not-any? (fn [x] (zero? (rem % x))) (range 2 %)) (iterate inc 2)))))

(defcheck solution-e87e5445
  (fn [n]
    (take n
      (filter
        (fn [x]
          (every? false? (map #(= 0 (mod x %)) (range 2 x))))
        (drop 2 (range))))))

(defcheck solution-e8f8d214
  (fn [m] (letfn [(prime [n] (if (= n 2) true (reduce  #(and % ((comp not zero?) (rem n %2))) true (range 2 (inc (int (Math/sqrt n)))))))]
            (take m (filter #(prime %) (range 2 1e18))))))

(defcheck solution-e9232b77
  (fn primes [l]
    ;; initial prime
    (loop [sq [2] n 2]
      ;; our next prime and our prime sequence
      (let [nxt (inc n)
            psq (if (some #(= 0 (rem nxt %)) sq) sq (conj sq nxt))]
        ;; check if the sequence is full
        (if (= l (count psq)) psq (recur psq nxt))))))

(defcheck solution-e9747549
  (fn [cnt]
    (letfn [(isPrime [val]
              (empty? (filter #(= 0 %) (take (- val 2) (map #(rem val %) (iterate inc 2 ))))))]

      (take cnt (filter isPrime (iterate inc  2)) ) )
    ))

(defcheck solution-e97b7056
  (fn [num]
    (letfn [(divides [d n]
              (= 0 (mod n d)))
            (nextprime [n smaller-primes]
              (if (some #(divides % n) smaller-primes)
                (recur (inc n) smaller-primes)
                n ))
            (primes [from smaller-primes]
              (let [n (nextprime from smaller-primes)]
                (cons n (lazy-seq (primes (inc n) (conj smaller-primes n))))))]
      (let [all-primes (primes 2 #{})]
        (take num all-primes)))))

(defcheck solution-ea2925f3
  (fn prime [n]
    (let [divisible? (fn [n d] (zero? (rem n d)))
          prime? (fn [n] (not-any? (partial divisible? n) (range 2 n)))
          primes (filter prime? (iterate inc 2))]
      (take n primes))))

(defcheck solution-ea2aa12c
  (fn [x]
    (letfn [(find-smallest-divisor
              ([x] (find-smallest-divisor x 2))
              ([x test-div] (cond (> (* test-div test-div) x) x
                                  (zero? (rem x test-div)) test-div
                                  :else (recur x (inc test-div)))))
            (prime? [x]
              (= x (find-smallest-divisor x)))]
      (take x (lazy-seq (filter #(prime? %) (iterate inc 2)))))))

(defcheck solution-eae2ba56
  (fn primes-take [x] (take x (

                               (fn primes [upper-limit]
                                 (if (< upper-limit 2) (list)
                                                       (loop [numbers (range 3 (inc upper-limit) 2) results [2]]
                                                         (let [next-number (first numbers)]
                                                           (if (> (* next-number next-number) upper-limit)
                                                             (concat results numbers)
                                                             (recur
                                                               (sort (seq (clojure.set/difference (set numbers) (set (range next-number (inc upper-limit) next-number)))))
                                                               (conj results next-number)))))))

                               541))))

(defcheck solution-eaf98ad5
  (fn [n]
    (take n (let
             [is-prime
              (fn [x]
                (empty?
                  (filter
                    #(= 0 (mod x %))
                    (range 2 x)
                    )
                  )
                )
              ]
              (lazy-seq (cons 2 (filter #(is-prime %) (iterate inc 3))))
              ))
    ))

(defcheck solution-eb153f7c
  (fn n-primes
    ([n] (n-primes n 3 '(2)))
    ([n curr primes]
     (if (<= n (count primes))
       (reverse primes)
       (let [is-prime (not (some #(= 0 (rem curr %)) primes))]
         (n-primes n (inc curr) (if is-prime (conj primes curr) primes)))))))

(defcheck solution-eb35e71e
  nth (concat [[2] [ 2 3 ]] (map first (iterate (fn [[p c]]
                                                  (if (every? #(> (mod c %) 0) p)
                                                    [(conj p c) (+ 2 c)]
                                                    (recur [p (+ 2 c)]))
                                                  ) [[2 3] 5]))))

(defcheck solution-eb5f59
  (fn [c]
    (take c (filter (fn isprim? [n]
                      (cond (= 1 n) false
                            (= 2 n) true
                            (even? n) false
                            :else ((fn prim? [_n coll]
                                     (cond (empty? coll) true
                                           (zero? (mod _n (first coll))) false
                                           :else (recur _n (rest coll))))
                                   n (filter odd? (range 3 (quot n 2))))))
              (range)))))

(defcheck solution-ebf87649
  (fn [n]
    (letfn [(prime?
              ([candidate] (prime? candidate (int (/ candidate 2))))
              ([candidate divisor]
               (if (= 1 divisor)
                 true
                 (if (zero? (rem candidate divisor))
                   false
                   (recur candidate (dec divisor))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-ec211da2
  (fn [n] (loop [sieve (iterate inc 2) retr []]
            (if (>= (count retr) n) retr
                                    (let [[p & more] sieve]
                                      (recur (keep #(when-not (= 0 (mod % p)) %) more) (conj retr p)))))))

(defcheck solution-ec6b1950
  (fn [x] (take x (lazy-cat [2 3 5 7 11]
                    (filter #(empty? (loop [fs [] x % f 2]
                                       (cond
                                         (> f (Math/sqrt %)) fs
                                         (zero? (mod x f)) (recur (conj fs f) (/ x f) f)
                                         :else (recur fs x (inc f)))))
                      (iterate inc 13))))))

(defcheck solution-ec8f4cb4
  #(take %
     (remove (fn [n]
               (some (fn [x] (= (rem n x) 0)) (range 2 n)))
       (drop 2 (range)))))

(defcheck solution-ecd10b19
  (fn n-primes [n]
    (letfn [(gen-primes
              [candidate-prime composite-table]
              (loop [next-prime candidate-prime new-table composite-table]
                (let [known-factors (new-table next-prime)]
                  (if (nil? known-factors)
                    (cons next-prime (lazy-seq (gen-primes (inc next-prime) (conj new-table {(* next-prime next-prime) (list next-prime)}))))
                    (recur
                      (inc next-prime)
                      (dissoc (apply merge-with concat new-table
                                (map #(hash-map (+ next-prime %) (list %)) known-factors)) next-prime))))))]
      (take n (gen-primes 2 {})))))

(defcheck solution-ed162fe3
  (fn primes [n]
    (cons 2
      (loop [k (dec n) r [] l 2]
        (if (zero? k) r
                      (let [p #(and (not-any? (fn [v] (zero? (mod % v))) (range 2 (dec %))) %)
                            v (some p (iterate inc (inc l)))]
                        (recur (dec k) (conj r v) v)))))))

(defcheck solution-ed27f260
  (fn [n]
    (loop [p [2] c 3]
      (if (= n (count p))
        p
        (let [divide-repeatedly #(if (zero? (rem %1 %2)) (recur (/ %1 %2) %2) %1)
              result (reduce divide-repeatedly c p)]
          (if (= result 1)
            (recur p (inc c))
            (recur (conj p c) (inc c))))))))

(defcheck solution-ed416741
  (fn [y]
    (take y
      (filter
        (fn [x] (not-any? zero? (map #(mod x %) (range 2 x))))
        (iterate inc 2)))))

(defcheck solution-ed8637af
  (fn [n]
    (letfn [(prime? [x s] (every? #(< 0 (rem x %)) s)),
            (move-next [[x s]] (if (prime? x s) [(inc x) (conj s x)], [(inc x) s]))]
      ((comp second first drop-while) #(> n (count (second %))) (iterate move-next [2, []])))))

(defcheck solution-edd8a70e
  (fn [n]
    (let [prime? (fn [num]
                   (or
                    (= 1 num)
                    (not
                      (some
                        #(zero? (mod num %))
                        (range 2 num)))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-eebd1207
  (fn [n]
    (take n
      ((fn i [col]
         (cons
           (first col)
           (lazy-seq
             (i (remove #(zero? (mod % (first col))) col))))) (iterate inc 2)))))

(defcheck solution-eecd59c1
  (fn [n] (take n
            (remove nil?
              (cons 2 (map (fn [x]
                             (when (nil? (some #(= 0 (rem x %))
                                           (range 2 (inc (Math/sqrt x))))) x))
                        (iterate inc 3)))))))

(defcheck solution-ef800fcb
  (fn prime-numbers [n]
    (letfn
     [(prime? [n]
        (not-any? #(= 0 (mod n %)) (range 2 n)))]
      (take n
        (filter prime? (drop 2 (range)))))))

(defcheck solution-ef932cb2
  (fn [y] (take y (filter #(when-not (some #{0} (map (partial rem %) (range 2 %))) %) (iterate inc 2)))))

(defcheck solution-efd732cd
  (fn [n]
    (let
     [
      reinsert
      (fn [table x prime]
        (update-in table
          [(+ prime x)]
          conj prime
          )
        )
      primes-step
      (fn primes-step [table d]
        (if-let
         [factors (get table d)]
          (recur
            (reduce
              #(reinsert %1 d %2)
              (dissoc table d)
              factors
              )
            (inc d)
            )
          (lazy-seq
            (cons d
              (primes-step
                (assoc
                 table
                  (* d d)
                  (list d)
                  )
                (inc d)
                )
              )
            )
          )
        )
      ]
      (take n
        (primes-step {} 2)
        )
      )
    ))

(defcheck solution-effab0fb
  (fn primes [n]
    (letfn [(isprime [num ps]
              (not-any? #(zero? (mod num %)) ps))]
      (if (= n 1)
        [2]
        (let [pastprimes (primes (dec n))
              totest     (iterate inc (inc (last pastprimes)))
              nextprime  (first (filter #(isprime % pastprimes) totest))]
          (conj pastprimes nextprime))))))

(defcheck solution-f04d4c49
  (fn [n] (take n
            (drop 2
              (filter
                #(empty? (filter zero? (map rem (repeat %) (range 2 %))))
                (range))))))

(defcheck solution-f05b4c
  (fn  [n]
    (let [prime?
          (fn [n] (every? #(pos? (rem n %)) (range 2 (Math/sqrt (inc n)))))]
      (take n (filter prime? (range 2 100000))))))

(defcheck solution-f069dfe7
  (fn primes [n]
    (condp = n
      0 []
      1 [2]
      (let [lastprimes (primes (dec n))]
        (loop [i (inc (peek lastprimes))]
          (if (empty? (filter #(= (mod i %) 0) lastprimes))
            (conj lastprimes i)
            (recur (inc i))
            ))))))

(defcheck solution-f0a270cc
  ;
  ;(defn prime? [n ndiv]
  ;  (if (< n 2)
  ;    false
  ;    (if (or (= n 2) (= n 3))
  ;      true
  ;      (if (= (mod n ndiv) 0)
  ;        false
  ;        (if (< (int (inc (/ n ndiv))) ndiv)
  ;          true
  ;          (prime? n (inc ndiv))
  ;        )
  ;      )
  ;    )
  ;  )
  ;)
  ;
  ;cheating a bit (didn't really solve the problem of returning a lazy vector of primes.
  ;Instead, tested the first 600 integers for being primes and took n elements from the resulting vector)
  ;
  ;#(take % (filterv (fn[x] (prime? x 2)) (range 600)))
  ;


  #(take % (filterv (fn[x] ((fn prime? [n ndiv] (if (< n 2) false (if (or (= n 2) (= n 3)) true (if (= (mod n ndiv) 0) false (if (< (int (inc (/ n ndiv))) ndiv) true (prime? n (inc ndiv))))))) x 2)) (range 600))))

(defcheck solution-f0b615c0
  #(take %2 (remove % (range 2 999))) (fn [x] (some #(= (rem x %) 0) (range 2 x))))

(defcheck solution-f0c5787a
  (fn [n] (take n ((fn pseq
                     ([from]
                      (let [is-prime? (fn [n] (every? (comp not zero? (partial rem n)) (range 2 n)))
                            nxt-prime (fn [p] (some #(if (is-prime? %) %) (iterate inc (inc p))))
                            n (nxt-prime from)]
                        (cons n (lazy-seq (pseq n)))))
                     ([] (cons 2 (pseq 2))))))))

(defcheck solution-f0dcef46
  (fn x-primes [x]
    (let [isPrime? (fn [n]
                     (not (boolean (first (filter #(= 0 (rem n %)) (range 2  n))))))]
      (take x (filter isPrime? (rest (iterate inc 1)))))))

(defcheck solution-f166689f
  (fn [n]
    (take n
      ((fn sieve [s]
         (cons (first s)
           (lazy-seq (sieve
                       (filter #(not= 0 (mod % (first s)))
                         (rest s)))))) (iterate inc 2)))))

(defcheck solution-f1e7561f
  ;; This isn't the most efficient, but
  ;; interestingly, my optimized version
  ;; (that keeps a memory of previous
  ;; primes for testing divisibility and
  ;; only searches up to the square root
  ;; for factors) times out in clojail,
  ;; even though it runs in tens of
  ;; milliseconds on my laptop.
  (fn [cnt]
    (take cnt
      (filter
        (fn [val]
          (zero?
            (count
              (filter #(zero? (mod val %))
                (range 2 val)))))
        (iterate inc 2)))))

(defcheck solution-f1f3ffab
  (fn primes [len]
    (loop [s []
           cnt 2]
      (if (= (count s) len)
        s
        (recur (if (= 0 (count (filter #(= (mod cnt %) 0) (range 2 cnt))))
                 (conj s cnt)
                 s) (inc cnt))))))

(defcheck solution-f2552b6e
  (fn first-x-primes [x]
    (let [is-prime
          (fn [n]
            (or (= 2 n)
                (every? #(= % false)
                  (for [i (range 2 (inc (Math/sqrt n)))]
                    (= (mod n i) 0)))))]
      (take x (filter is-prime (drop 2 (range)))))))

(defcheck solution-f278086e
  (fn [c]
    (letfn [(s [xs]
              (lazy-seq (let [x (first xs)]
                          (cons x (s (filter #(not= (rem % x) 0) (rest xs)))))))]
      (take c (s (drop 2 (range))))
      )))

(defcheck solution-f2df392f
  (fn sieve [n]
    (loop [num 4, primes [2 3]]
      (cond
        (= (count primes) n) primes
        (some #(zero? (mod num %)) primes) (recur (inc num) primes)
        :else (recur (inc num) (conj primes num)))
      )
    ))

(defcheck solution-f35e0a01
  (fn [m] (loop [n 3 m (dec m) p [2]] (if (= 0 m) p (if ((fn [n] (not-any? #(== (mod n %) 0) (range 2 (inc (Math/sqrt n))))) n) (recur (inc n) (dec m) (conj p n)) (recur (inc n) m p))))))

(defcheck solution-f3e07e31
  (fn fac [n]
    (if (= n 1)
      [2]
      (let [prev (fac (- n 1))]
        (conj prev (first (filter (fn [x] (not-any? #(= 0 (mod x %)) prev)) (iterate inc (last prev)))))))))

(defcheck solution-f4b0b38e
  (fn primes [c]
    (loop [p [2] n 3]
      (if (= c (count p))
        p
        (if (empty? (filter #(zero? (rem n %)) p))
          (recur (conj p n) (inc n))
          (recur p (inc n)))))))

(defcheck solution-f5270f3a
  (fn [x] (let [primes
                ((fn []
                   (loop [nums (range 2 1000) div  2 ]
                     (let [isprime?  #(or (> (mod % div) 0) (= % div))]
                       (if (> div 500) nums
                                       (recur (filter isprime? nums) (inc div)))))))]
            (take x primes)
            )))

(defcheck solution-f541b038
  (fn [n]
    (let [prime? (fn [p]
                   (loop [x 2]
                     (if (= 0 (mod p x))
                       (if (= p x) true false)
                       (recur (inc x)))))]
      (loop [x 2, v []]
        (if (= n (count v))
          v
          (recur (inc x) (if (prime? x) (conj v x) v)))))))

(defcheck solution-f58f6908
  (fn [n]
    (let [is-prime (fn [x]
                     (loop [i 2]
                       (cond
                         (> (* i i) x) true
                         (zero? (mod x i)) false
                         :else (recur (inc i)))))]
      (loop [xs [] i 0 x 2]
        (cond
          (= i n) xs
          (is-prime x) (recur (conj xs x) (inc i) (inc x))
          :else (recur xs i (inc x)))))))

(defcheck solution-f5b3b3a6
  (fn primes*
    [n]
    (if (= n 1) [2]
                (let [previous (primes* (dec n))
                      find-by (comp first filter)
                      divisible-by-any? #(some (fn [val] (zero? (mod %2 val))) %1)
                      ]
                  (conj previous (find-by #(not (divisible-by-any? previous %)) (iterate inc (last previous))))
                  )
                )))

(defcheck solution-f5cc74d0
  (fn [n] (take n (filter #(empty? (for [i (range 2 (inc (quot % 2))) :when (zero? (mod % i))] i)) (range 2 999)))))

(defcheck solution-f5e0a981
  (fn [x]
    (loop [primes (drop 2 (range)) res [] n 0]
      (if (= n x)
        res
        (recur
          (filter #(not= 0 (mod % (first primes))) primes)
          (conj res (first primes))
          (inc n))))))

(defcheck solution-f5ef0fe2
  (fn primes [n]
    (let [prime? (fn [x]
                   (every? #(> (mod x %) 0)
                     (range 2 (inc (Math/sqrt x)))))]
      (loop [i 3
             r [2]]
        (if (< (count r) n)
          (recur (inc i)
            (if (prime? i)
              (conj r i)
              r))
          r)))))

(defcheck solution-f5f1e1d6
  (fn [n]
    (letfn [(step [coll]
              (let [head (first coll)]
                (lazy-seq (cons head (step (filter #(pos? (mod % head)) coll))))))]
      (take n (step (iterate inc 2))))))

(defcheck solution-f63b2ded
  (fn [x] (take x [2    3    5    7    11    13    17    19    23    29    31    37    41    43    47    53    59    61    67    71    73    79    83    89    97    101    103    107    109    113    127    131    137    139    149    151    157    163    167    173    179    181    191    193    197    199    211    223    227    229    233    239    241    251    257    263    269    271    277    281    283    293    307    311    313    317    331    337    347    349    353    359    367    373    379    383    389    397    401    409    419    421    431    433    439    443    449    457    461    463    467    479    487    491    499    503    509    521    523    541])))

(defcheck solution-f652d9dc
  (letfn [(prime-worker [i, iterators]
            (let [iterator (get iterators i)]
              (if iterator
                (recur (inc i)
                  (reduce
                    #(update-in %1 [(+ i %2)] conj %2)
                    (dissoc iterators i)
                    iterator))
                (lazy-seq (cons i (prime-worker (inc i) (assoc iterators (* i i) [i])))))))]
    (fn primes
      ([] (prime-worker 2 {}))
      ([n] (take n (primes))))))

(defcheck solution-f6b9eb29
  (fn [n]
    (let [f (fn sieve [s] (cons (first s) (lazy-seq (sieve (filter #(not (= 0 (mod % (first s)))) (rest s))))))
          ]
      (take n (f (iterate inc 2)))
      )
    ))

(defcheck solution-f8360583
  (fn primes
    ([n] (take n (primes)))
    ([]
     (letfn [(prime? [x]
               (let [l (int (Math/sqrt x))]
                 (not-any? zero? (map #(rem x %) (range 2 (inc l))))))]
       (filter prime? (iterate inc 2))))))

(defcheck solution-f86e96ec
  (fn my-sieve [n]
    (let [ceiling (+ 4 (* (Math/log n) n) (* n (Math/log (Math/log n))))
          candidates (range 2 (Math/floor ceiling))]
      (loop [x 2 survivors candidates primes []]
        (if (or (nil? x) (> x (last candidates)))
          (take n primes)                ;; base case: take n from the proven primes
          (recur
            (some #(if (> % x) %) survivors)          ;; x for next round
            (filter #(or (< % x)
                         (> (mod % x) 0)) survivors) ;; survivors for next round
            (conj primes x)))))))

(defcheck solution-f8ee3bba
  (fn me1
    [n]

    (let [checkPrime (fn [number]

                       (reduce #(and %1 (not= 0 (rem number %2)))  true (range 2 number)))

          res (filter checkPrime (range 3 1000))

          ]

      ;(println res)

      (take n (concat [2] res ) )

      )


    ))

(defcheck solution-f904eacc
  (fn [n]
    (letfn [(seive [xs]
              (cons (first xs)
                (lazy-seq (seive (filter #(not (= 0 (mod % (first xs)))) (rest xs))))))]
      (take n (seive (iterate inc 2))))))

(defcheck solution-f906155f
  (fn take-primes [n]
    (letfn [(sieve [seq]
              (cons (first seq)
                (lazy-seq
                  (sieve (filter #(not= (mod % (first seq)) 0) (rest seq))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-f9196682
  (fn [n]
    (letfn [(prime? [n]
              (every? #(not= 0 (mod n %))
                (take-while #(<= % (/ n 2))
                  (cons 2 (iterate #(+ 2 %) 3)))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-f95a8782
  (fn [x]
    (take x
      (filter
        (fn [n]
          (every? #(not (zero? %))
            (map #(rem n %) (range 2 n))
            ))
        (range 2 10000)))))

(defcheck solution-f97569a5
  (fn getprimes [n]
    (let [sieve (fn sieve [s]
                  (lazy-seq (cons (first s)
                              (sieve (filter
                                       #(not= 0 (mod % (first s))) (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-fab553ca
  (fn [y]
    (take y (drop 2 (filter (fn [x] (every? #(not= (rem x %) 0) (range 2 (inc (/ x 2))))) (range))))))

(defcheck solution-fab5eb8c
  (fn
    [n]
    (if (= 1 n)
      [2]
      (loop [p [2] k 3]
        (if (= (count p) n)
          p
          (if (some #(= 0 %) (map #(rem k %) p))
            (recur p (inc k))
            (recur (conj p k) (inc k))))))))

(defcheck solution-fae41b1e
  (fn[n] (take n (filter #(every? (fn[x] (not (zero? (rem % x))))
                            (range 2 (+ 1 (/ % 2))))
                   (iterate inc 2)))))

(defcheck solution-faee9d5f
  (fn [n]
    (vec
      (cons 2
        (take (dec n)
          ((fn p [primes]
             (loop [i (inc (last primes))]
               (if (some #(= 0 (mod i %)) primes)
                 (recur (inc i))
                 (cons i (lazy-seq (p (conj primes i)))))))
           [2]))))))

(defcheck solution-fb159ee7
  (fn g [n]
    (take n
      ((fn f [s]
         (lazy-seq (cons
                     (first s)
                     (f (filter
                          #(not= 0 (rem % (first s)))
                          (rest s))))))
       (nthnext (range) 2)))))

(defcheck solution-fb232f99
  ; (fn [nf]
  ;   (take nf
  ;         (filter (fn [n] (nil? (first
  ;                                (filter #(= 0 (mod n %)) (range 2 (+ (/ n 2) 1))))))
  ;                 (iterate inc 2))))
  ; upper: right would be (range 2 (sqrt n +1)
  (fn [nf] (take nf
             (filter (fn [n]
                       (not (some zero? (map #(mod n %) (range 2 (+ (/ n 2) 1))))))
               (iterate inc 2)))))

(defcheck solution-fb5497a4
  (fn get-primes [max]
    (take max (let [primes (atom [])]
                (for [n (iterate inc 2)
                      :when (not-any? #(zero? (rem n %))
                              (filter #(<= % (Math/sqrt n))
                                @primes))]
                  (do (swap! primes conj n)
                      n))))))

(defcheck solution-fb71f937
  (fn primes--recur
    ([]
     (let [inc2        (partial + 2),
           divides?    (fn [a d]
                         (= 0 (mod a d))),
           composite?  (fn [a]
                         (or (even? a)
                             (->> (iterate inc2 3)
                               (take-while (partial >= (Math/sqrt a)))
                               (some (partial divides? a)))))
           prime?      (fn [a]
                         (if (composite? a) false a))
           next-prime  (fn [a]
                         (some prime? (iterate inc2 (inc2 a))))]
       (cons 2 (iterate next-prime 3))))
    ([n] {:pre [(integer? n)]}
     (take n (primes--recur)))))

(defcheck solution-fba3ccb7
  (fn lazy-primes3 [n]
    (take n
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
        (cons 2 (lazy-seq (next-primes {} 3)))))))

(defcheck solution-fbee41d8
  (fn
    [nresults]
    (let [sieve (fn sieve
                  [s]
                  (when (seq s)
                    (let [p (first s)]
                      (cons p (lazy-seq (sieve (remove #(= (rem % p) 0) (rest s))))))))]
      (take nresults (sieve (iterate inc 2))))))

(defcheck solution-fc2e258b
  (fn [y]
    ((fn prms [x ps]
       (let [a (last ps)]
         (if (= x 0)
           ps
           (prms
             (- x 1)
             (conj
               ps
               (first
                 (filter
                   (fn [y]
                     (not-any? #(= (mod y %) 0) ps)
                     )
                   (range (inc a) (+ a 1000))
                   )
                 )
               )
             )
           )
         )
       )
     (dec y) [2])))

(defcheck solution-fc5c4272
  #(take %
     (filter
       (fn [x]
         (not-any?
           (fn [y]
             (= 0 (mod x y)))
           (range 2 x)))
       (iterate inc 2))))

(defcheck solution-fce5fa3a
  (fn [i]
    (letfn [(p? [n]
              (if (or (= 2 n) (= 3 n))
                true
                (= 0 (count
                       (filter #(zero? (rem n %))
                         (take-while #(<= (* % %) n) (cons 2 (range 3 n 2))))))))]
      (take i (filter p? (iterate inc 2))))))

(defcheck solution-fd40367a
  (fn [k]
    (take k
      (filter
        (fn [n]
          (empty? (filter #(= 0 (mod n %)) (range 2 n))))
        (iterate inc 2)))))

(defcheck solution-fd56fb8b
  (letfn [(primes
            [[x & xs]]
            (lazy-seq
              (cons x (primes
                        (remove #(zero? (mod % x))
                          xs)))))]

    #(->> (iterate (partial + 2) 3)
       primes (cons 2) (take %))))

(defcheck solution-fd8ea6b8
  (fn [n]
    (letfn [(divisible-by? [x nums] (some #(= 0 (rem x %)) nums))
            (primes
              ([] (primes 2 []))
              ([x nums]
               (lazy-seq
                 (loop [x x
                        nums nums]
                   (if (divisible-by? x nums)
                     (recur (inc x) nums)
                     (cons x (primes (inc x) (conj nums x))))))))]
      (take n (primes)))))

(defcheck solution-fdd23d4f
  (fn [n]
    (letfn [(sieve [s]
              (lazy-seq (cons (first s)
                          (sieve (filter #(not (zero? (mod % (first s)))) (rest s))))))]
      (take n (sieve (iterate inc 2))))))

(defcheck solution-fddfac9f
  (fn [a]
    (take a
      (iterate
        (fn [n] (first
                  (filter
                    (fn [x] (empty? (filter #(= (rem x %) 0) (range 2 x))))
                    (iterate inc (inc n)))))
        2))))

(defcheck solution-fe0dd38b
  (fn primes [x]
    (let [update (fn [li elem]
                   (if (some #(zero? (mod elem %)) li)
                     li
                     (conj li elem)))]
      (loop [curr 2
             acc []]
        (if (= x (count acc))
          acc
          (recur (inc curr) (update acc curr)))))))

(defcheck solution-fe21600d
  (fn [i]
    (take i (remove #(some (fn [x] (= 0 (rem % x)))
                       (range 2 %))
              (iterate inc 2)))))

(defcheck solution-fe5cea9
  (fn [x] (take x (filter #(not (nil? %)) (map (fn is_prm[n] (if-not (some
                                                                       #(= (mod n %) 0)
                                                                       (range 2 (inc (/ n 2)))
                                                                       ) n nil  )) (drop 2 (range)))))
    ))

(defcheck solution-fe93944a
  (fn [num]
    (some #(if (= num (count (first %))) (first %))
      (iterate
        (fn [[primes, candidate]]
          [(if (every? #(not= 0 (mod candidate %)) primes)
             (conj primes candidate),
             primes),
           (inc candidate)])
        [[2] 3]))))

(defcheck solution-fea4f64d
  (fn [n]
    (letfn [(prime? [x]
              (not (some zero? (map (partial rem x) (range 2 (dec x))))))]
      (take n (filter prime? (iterate inc 2))))))

(defcheck solution-febd209a
  (fn [n]
    (letfn
     [(prime? [p]
        (not-any? #(zero? (mod p %)) (range 2 (- p 1))))]
      (->> (iterate inc 2) (filter prime?) (take n)))))

(defcheck solution-fee2cd13
  #(loop [l [2]
          n 3]
     (if (= (count l) %)
       l
       (recur (if (some zero? (map (partial mod n) l))
                l
                (conj l n))
         (inc n)))))

(defcheck solution-ff01033b
  (fn [n]
    (letfn [ (primes[n so-far]
               (if (some #(= 0 (rem n %)) so-far)
                 (recur (inc n) so-far)
                 (lazy-seq
                   (cons n (primes (inc n) (conj so-far n)))))) ]
      (take n (primes 2 [])))))

(defcheck solution-ff99f862
  (fn prime [n]
    (cond
      (= n 1) [2]
      (= n 2) [2 3]
      :else
      (loop [r [2 3], x 5]
        (if (= (count r) n)
          r
          (if ((fn is-prime? [i]
                 (not
                   (some
                     #(= (mod i %) 0)
                     (range 3 i 2)
                     ))
                 ) x)
            (recur (conj r x) (+ x 2))
            (recur r (+ x 2))
            )
          )
        )
      )
    ))

(defcheck solution-ffe8ad8e
  (fn primes [n]
    (letfn [(sieve [s]
              (cons (first s)
                (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                   (rest s))))))]
      (take n (sieve (iterate inc 2))))
    ))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-67))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
