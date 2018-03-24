(ns coal-mine.problem-80
  (:require [coal-mine.checks :refer [defcheck-80] :rename {defcheck-80 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-103b9ca5
  (fn [x] (= (reduce + (filter #(= (mod x %) 0) (range 1 x))) x)))

(defcheck solution-1044d479
  (fn [n]
    (= n (apply + ((fn [n] (reduce #(if (= 0 (mod n %2)) (cons %2 %1) %1) [] (range 1 (- n 1)))) n)))))

(defcheck solution-106ee24
  (fn [x]
    (=
      x
      (reduce + (filter #(= (mod x %) 0) (range 1 x)))
      )
    ))

(defcheck solution-1086e231
  (fn [x]
    (let [a (filter #(zero? (mod x %)) (range 2 (Math/sqrt x)))
          b (map #(/ x %) a)]
      (= x (apply + 1 (concat a b))))))

(defcheck solution-10a70173
  (letfn [(divisors [n]
            (filter #(zero? (rem n %))
              (range 1 (inc (quot n 2)))))]
    #(= % (apply + (divisors %)))))

(defcheck solution-10ab97d7
  (fn [n]
    (->>
      (range 1 n)
      (filter #(= 0 (rem n %)))
      (reduce +)
      (= n))))

(defcheck solution-12356bd7
  #(= % (reduce + (filter (fn [i] (= 0 (mod % i))) (range 1 (+ (/ % 2) 1))))))

(defcheck solution-1254cb82
  #(= % (reduce + (filter (fn [x] (= 0 (rem % x))) (range 1 %)))))

(defcheck solution-1293b40f
  #(= (reduce +
        (filter  (fn [x] (= 0 (mod % x)))
          (range 1 %)))
     %))

(defcheck solution-130db248
  (fn [x]
    (= x (reduce (fn [acc y] (if (zero? (mod x y)) (+ acc y) acc) ) 0 (range 1 x))) ))

(defcheck solution-1371e23f
  (fn [n]
    (=
      n
      (reduce
        +
        (filter
          #(= 0 (mod n %))
          (range 1 (inc (/ n 2))))))))

(defcheck solution-143b4a4c
  (fn [n]
    (= n
      (reduce +
        (filter
          #(= (rem n %) 0) ;; if the rest is null, then then number is a divisor
          (range 1 (inc (/ n 2))))))))

(defcheck solution-147f716b
  (fn perfect? [n]
    (letfn [(divisors [n] (filter (fn [e] (= 0 (mod n e))) (range 1 n)))]
      (= n (apply + (divisors n))))))

(defcheck solution-14ae28d1
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (rem n %))
          (range 1 n))))))

(defcheck solution-14b0a2c6
  (fn [x] (->> (range 1 x) (filter #(zero? (mod x %))) (reduce +) (= x))))

(defcheck solution-14d25984
  (fn [x]
    (= x (apply + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-14d4556d
  (fn [n]
    (= n (apply + (filter #(zero? (mod n %))
                    (range 1 n))))))

(defcheck solution-14f17c35
  (fn [a]
    (= a (apply + (filter #(= (mod a %) 0) (range 1 (/ (inc a) 2)))))))

(defcheck solution-14f44b22
  #(= % (reduce + (filter (comp zero? (partial mod %)) (rest (range %))))))

(defcheck solution-15bfc1c8
  (fn [n]
    (= n (apply +
           (->> (range 1 n) (filter #(zero? (mod n %))))
           ))
    ))

(defcheck solution-161fe798
  (fn perfect? [n]
    (->> (range 1 n)
      (filter #(= 0 (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-1648ff3f
  (fn [n] (
            loop [i  (quot n 2)
                  dvsors []
                  ]
            (if (zero? i)
              (= (apply + dvsors) n)
              (if (zero? (mod n i))
                (recur (dec i) (cons i dvsors))
                (recur (dec i) dvsors))
              )
            )
    ))

(defcheck solution-169832d2
  (fn [n] (not (nil? (some #{n} [6 496 8128])))))

(defcheck solution-16bfaba
  (fn [n]
    (letfn [(f [m]
              (filter #(= 0 (rem m %)) (range 1 m)))]
      (= n (reduce + (f n))))))

(defcheck solution-16e74535
  (fn [i] (= i (apply + (filter #(= 0 (mod i %)) (range 1 i))))))

(defcheck solution-176beb81
  (comp (partial apply =)
        (juxt (comp (partial reduce +)
                    (partial apply filter)
                    (juxt (partial partial
                            (comp zero? rem))
                      (partial range 1)))
          identity)))

(defcheck solution-1787b10b
  (fn [x]
    (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-182605ac
  (fn [n]
    (= n
      (reduce +
        (filter #(= 0 (mod n %))
          (range 1 n))))))

(defcheck solution-182eb709
  (fn perfect? [n]
    (let [divisors (fn [x] (filter #(zero? (mod x %)) (range 1 (inc (int (/ x 2))))))]
      (if (= n (reduce + (divisors n))) true false))))

(defcheck solution-189f711a
  (fn [n]
    (= n (apply +
           (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-191a6d2a
  (fn [a] (= a (apply + (filter #(= 0 (rem a %)) (range 1 a))))))

(defcheck solution-1923b019
  (fn [n]
    (= n (reduce + (for [d (range 1 n) :when (zero? (rem n d))] d)))))

(defcheck solution-197202c3
  (fn perfect? [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-19d260bb
  #(= % (reduce + (filter (fn [x] (zero? (mod % x))) (range 1 %)))))

(defcheck solution-1a1f3ed2
  (fn perfect? [n]
    (let [divides? (comp zero? mod)]
      (->> (range n)
        rest
        (filter (partial divides? n))
        (apply +)
        (= n)))))

(defcheck solution-1a32adca
  (fn [x](->>
           x
           (range 1)
           (filter #(= (rem x %) 0))
           (apply +)
           (= x)
           )
    ))

(defcheck solution-1a75203e
  (fn perfect? [n]
    (let [divisors (filter #(and (not= % 0) (= (mod n %) 0)) (range n))]
      (= (apply + divisors) n))))

(defcheck solution-1a87d2ef
  (fn [n]
    (let [divisors (->> (range 1 n) (filter #(zero? (mod n %))))]
      (= n (apply + divisors)))))

(defcheck solution-1ad750bd
  (fn is-perfect? [n] (= n (apply + (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-1b6280b4
  (fn perfect? [n]
    (letfn [(divisors [n]
              (filter #(= (mod n %) 0) (range 1 (inc n))))]
      (= (+ n n)
        (apply + (divisors n))))))

(defcheck solution-1b7f07b3
  (fn [x] (= x (reduce + (filter #(zero? (mod x %)) (range 1 x) ) ) )))

(defcheck solution-1bb7e6d9
  (fn [n]
    (= n (apply +
           (filter #(= 0 (mod n %))
             (map inc (range (/ n 2))))))))

(defcheck solution-1bc36d73
  #(->> (range 1 %)
     (filter (fn[v] (= 0 (mod % v))))
     (apply +)
     (= %)))

(defcheck solution-1c48c42b
  (fn [n]
    (= n (reduce +
           (filter (fn [d] (zero? (mod n d))) (range 1 (+ (quot n 2) 1)))))))

(defcheck solution-1c56a22b
  (fn [n]
    (= n
      (apply + (filter #(= (rem n %) 0) (range 1 n))))))

(defcheck solution-1d6b053b
  (fn is-perfect? [d]
    (letfn [(sum-digits [coll] (reduce + coll))
            (is-divisable? [n m] (zero? (rem m n)))
            (get-divisors [n]
              (loop [i 1 max-d (int (/ n 2)) result []]
                (if (> i max-d)
                  (if (is-divisable? i n)
                    (conj result i)
                    result)
                  (recur (inc i) max-d (if (is-divisable? i n) (conj result i) result)))))]
      (= d (sum-digits (get-divisors d))))))

(defcheck solution-1e0ed604
  (fn [x] (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-1e17898
  (fn [n]
    (let [dv (fn[x] (filter #(zero? (rem x %)) (range 1 x) ))]
      (= n (reduce + (dv n))))))

(defcheck solution-1e8a241c
  (fn perf [n]
    (let [start (/ n 2)]
      (= n (reduce + (for [x (range 1 (inc start))
                           :when (= (mod n x) 0)] x)))
      )
    ))

(defcheck solution-1ee8eb9
  (fn [n]
    (let [divides-n #(= 0 (mod n %))
          divisors (filter divides-n (range 1 n))]
      (= n (reduce + 0 divisors)))))

(defcheck solution-1f2e8fa0
  #(->> % (range 1) (filter (fn [c] (zero? (rem % c)))) (apply +) (= %)))

(defcheck solution-1f409ccd
  (letfn [(isPerfect [n]
            (let [tocheck (range 1 (inc (quot n 2)))
                  divisors (filter #(= 0 (mod n %)) tocheck)
                  sum (reduce + 0 divisors)]
              (= sum n)))]
    isPerfect))

(defcheck solution-1fa0c596
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %))
                     (range 1 (inc (quot n 2))))))))

(defcheck solution-200eee20
  (fn [n]
    (letfn [(divisors [num] (filter #(= 0 (mod num %)) (range 1 (inc (/ num 2)))))]
      (#(= % (reduce + (divisors %))) n))))

(defcheck solution-2014e9bb
  (fn [x]
    (letfn
     [(f
        ([x ds i]
         (if (>= i x)
           ds
           (if (zero? (mod x i))
             (recur x (conj ds i) (inc i))
             (recur x ds (inc i)))))
        ([x] (f x [] 1)))]
      (= x (apply + (f x))))))

(defcheck solution-20169acc
  (fn [n]
    (= n
      (reduce + (filter #(zero? (rem n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-20210eea
  (fn perfect? [n]
    (= n (reduce + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-20b0c6ad
  (fn [x]
    (let [divisors (filter #(= 0 (mod x %)) (range 1 x))
          div-sum  (reduce + divisors)]
      (= x div-sum))))

(defcheck solution-20c87273
  (fn [n]
    (loop [i 1
           r 0]
      (if (= n r)
        true
        (if (> r n)
          false
          (recur (inc i) (+ r i)))))))

(defcheck solution-217d8e3f
  (fn perfect-number [n]
    (= n
      (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-21af4e2e
  #(= % (apply + (for [x (range (quot % 2))
                       :let [y (inc x)]
                       :when (= 0 (mod % y))]
                   y))))

(defcheck solution-220114fd
  #(= (apply + (filter (comp zero? (partial rem %)) (range 1 %))) %))

(defcheck solution-223fdbec
  (fn [n]
    (= n (reduce + (filter (fn [x] (zero? (mod n x))) (range 1 n))))))

(defcheck solution-2339d8e
  (fn [x]
    (= x
      (reduce +
        (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-246cc510
  #(not (nil? (some #{6,496,8128} (vector %)))))

(defcheck solution-24f53483
  (fn [n]
    (->> (range 1 (inc (/ n 2)))
      (filter #(zero? (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-261fda7
  (fn perfect? [n]
    (= (reduce + (filter #(zero? (rem n %1)) (range 1 n))) n)))

(defcheck solution-26f21567
  (fn [n]
    (let [subs (filter #(zero? (rem n %)) (range 1 n))
          sum (apply + subs)]
      (= sum n))))

(defcheck solution-26fbee79
  (fn [x]
    (letfn [(d [n] (filter #(= 0 (mod (/ n %) 1)) (range 1 (inc (/ n 2)))))]
      (= x (apply + (d x)))
      )
    ))

(defcheck solution-2700e9d8
  (fn perfect [n]
    (= n
      (reduce +
        (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-2771702f
  (fn [x] (= x (apply + (filter #(#{0} (mod x %)) (range 1 x))))))

(defcheck solution-27ca2d73
  (fn foo [n]
    (= (* 2 n) (reduce + (into
                           #{} (mapcat #(vector % (/ n %)) (filter
                                                             #(zero? (rem n %))
                                                             (range 1 (inc (Math/sqrt n))))))))))

(defcheck solution-27ce9a5b
  (fn [num]
    (= num
      (reduce + (filter #(= 0 (mod num %)) (range 1 (- num 1)))))
    ))

(defcheck solution-280a302d
  (fn perfect? [n]
    (let [divisors
          (filter #(zero? (mod n %)) (range 1 n))]
      (= (reduce + divisors) n))))

(defcheck solution-283cd557
  (fn [n]
    (letfn [(divisors [m] (filter #(= 0 (mod m %)) (range 1 (+ 1 (/ m 2)))))]
      (= n (apply + (divisors n))))))

(defcheck solution-283ee5e4
  (fn [x] (= x (reduce + (filter #(= 0 (rem x %1)) (range 1 (inc (/ x 2))))))))

(defcheck solution-28a91054
  (fn [n]
    (= n
      (apply +
        (reduce #(if (= 0 (mod n %2))
                   (conj (conj % %2) (/ n %2))
                   %)
          [1] (range 2 (inc (int (Math/sqrt n)))))))))

(defcheck solution-28addcf4
  (fn [x] (= x (apply + (filter #(= 0 (mod x %)) (rest (range x)))))))

(defcheck solution-28bc1be3
  (fn [x]
    (= x (reduce + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-28c1ac90
  (fn [x]
    (loop [c1 []
           p (inc 0)]
      (if (= p x)
        (if (= x (reduce + c1)) true false)
        (recur (conj c1 (if (= 0 (mod x p)) p 0))
          (inc p))))))

(defcheck solution-28cd6d3f
  (fn [n]
    (= n
      (apply +
        (filter #(zero? (mod n %))
          (range 1 n))))))

(defcheck solution-291d96a
  (fn j[x]
    (= x (reduce +
           ((fn div[x]
              (filter #(= 0 (mod x %)) (range 1 (inc(/ x 2))))) x
            )
           )
      )
    ))

(defcheck solution-293c935d
  contains? (set (map (comp #(* % (/ (dec %) 2)) #(bit-shift-left 2r1 %)) '(2 3 5 7 13 17 19 31 61 89 107))))

(defcheck solution-296e2b51
  (fn [num]
    (=
      num
      (reduce +
        (filter
          #(zero? (mod num %))
          (range 1 num))))))

(defcheck solution-29af6d0
  (fn perfect? [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-2a331f5d
  #(= %
     (apply +
       (for [y (range 1 (inc (/ % 2)))
             :when (zero? (mod % y))]
         y))))

(defcheck solution-2a7b60f4
  (fn [n]
    (= n
      (reduce +
        (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-2a86901c
  (fn [x] (= x (apply + (filter #(= (mod x %) 0) (range 1 (inc (/ x 2))))))))

(defcheck solution-2adaa96d
  (fn [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-2af04562
  (fn [x] (->> (range 1 x)
            (filter #(zero? (mod x %)))
            (apply +)
            (= x))))

(defcheck solution-2b0342d0
  (fn [n]
    (let [dv (fn[x] (filter #(zero? (rem x %)) (range 1 x) ))]
      (= n (reduce + (dv n))))))

(defcheck solution-2b2d99e7
  (fn [n] (= n (apply + (filter #(zero? (mod n %)) (drop 1 (range n)))))))

(defcheck solution-2b802899
  (fn [n]
    (let [rs (filter integer?
               (for [x (range 2 (inc n))]
                 (/ n x)))]
      (if (= n (apply + rs)) true
                             false))))

(defcheck solution-2be68c13
  (fn [n]
    (let [factors (for [n' (range 1 n) :when (zero? (mod n n'))] n')]
      (= n (reduce + factors)))))

(defcheck solution-2c0c2787
  (fn perfect-number? [x]
    (= x (reduce + (filter #(zero?( mod x %)) (drop 1 (range x)))))))

(defcheck solution-2c3b456a
  (fn perfect? [checkval]
    (let [divisors (fn [value] (filter #(zero? (rem value %)) (range 1 value)))]
      (= (apply + (divisors checkval)) checkval))))

(defcheck solution-2c7a1386
  (fn [n]
    (= n (reduce + (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-2c8509b6
  #((fn [n d a]
      (cond
        (= d n) (= n a)
        (< n a) false
        :else (recur n (inc d) (if (= 0 (mod n d)) (+ a d) a))))
    % 1 0))

(defcheck solution-2ca03e33
  (fn [v] (=
            (apply +
              (filter #(= 0 (mod v %)) (range 1 v))) v)))

(defcheck solution-2d2ba30a
  (letfn [(is-divisor [x y]
            (zero? (mod x y)))]
    (fn [x]
      (= (apply + (filter (partial is-divisor x) (range 1 x))) x))))

(defcheck solution-2d555bf4
  (fn [n] (= n (reduce + 1
                 (set (mapcat #(if (zero? (rem n %)) [(quot n %) %])
                        (range 2 (-> n Math/sqrt int inc inc))))))))

(defcheck solution-2d5ee814
  (fn is-perfect[n] (= n (apply + (filter #(= (rem n %) 0) (range 1 n))))))

(defcheck solution-2d89b218
  (fn isperfect? [n]
    (let [sumifdivis (fn [a b]
                       (if (== (mod n b) 0)
                         (+ a b)
                         a))]
      (= n (reduce sumifdivis (range 1 n))))))

(defcheck solution-2da9879f
  (fn perfect? [n]
    (= n (reduce + (for [i (range 1 n)]
                     (if (= 0 (rem n i)) i 0))))))

(defcheck solution-2e00df69
  (fn[n](= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-2e920807
  (fn [x]
    (->> (range 1 x)
      (filter #(zero? (mod x %)))
      (apply +)
      (= x))))

(defcheck solution-2f27e52f
  (fn [n]
    (= n (apply
           +
           (filter #(= 0 (mod n %))
             (rest (take n (range))))))))

(defcheck solution-2fac2b61
  (fn [x]
    (= x (reduce + (filter #(= 0 (rem x %)) ( range 1 (inc (/ x 2)))) ))))

(defcheck solution-30bc281e
  (fn perfect? [n]
    (letfn
     [(factors [x]
        (distinct
          (mapcat #(vector % (/ x %))
            (filter #(= 0 (mod x %)) (range 1 (inc (Math/sqrt x)))))))]
      (=
        (reduce + (factors n))
        (* 2 n)))))

(defcheck solution-30e5f43
  (fn [n] (= n (apply + (filter #(= 0 (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-3111c915
  (fn perfect? [n]
    (= n
      (reduce +
        (filter
          #(= (mod n %) 0)
          (range 1 (inc (int (/ n 2)))))))))

(defcheck solution-3115988e
  (fn [x]
    (letfn [(divisors [x]
              (for [d (drop 1 (range))
                    :while (<= d (quot x 2))
                    :when (zero? (rem x d))]
                d))]
      (= x (reduce + (divisors x))))))

(defcheck solution-316666b3
  (fn [val]
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
              (distinct
                (sort
                  (flatten
                    (divisor-groups n)))))]
      (= val
        (reduce +
          (butlast (divisors val)))))))

(defcheck solution-316d57f2
  (fn [n]
    (let [quots (filter
                  #(zero? (mod n %))
                  (range 1 (inc (quot n 2))))]
      (= (apply + quots) n))))

(defcheck solution-31a1bcf
  (fn [x]
    (= x
      (reduce +
        (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-31a45af2
  #(= (dec %) (apply + ((fn [n d acc]
                          (cond
                            (= n d) acc
                            (= 0 (rem n d)) (recur n (inc d) (conj acc d))
                            :else (recur n (inc d) acc))) % 2 []))))

(defcheck solution-3258614b
  (fn [n]
    (let [factors (filter #(zero? (rem n %)) (range 1 n))]
      (= n (reduce + factors)))))

(defcheck solution-32a35cf5
  (fn perfect-number?
    ([n] (perfect-number? n 1 0))
    ([n start end]
     (cond
       (= end n) true
       (< end n) (recur n (inc start) (+ start end))
       :else false))))

(defcheck solution-32f65a16
  (fn [n]
    (->> [n 1 2] (iterate (fn [[n _ i]] (if (zero? (rem n i))
                                          [(quot n i) i i]
                                          [n nil (inc i)])))
      (take-while (fn [[n d _]] (or (> n 1) (not (nil? d)))))
      (keep second)
      (iterate next)
      (take-while seq)
      (map first)
      (reduce (fn [v x] (into v (for [i v] (* i x)))) #{1})
      (filter #(< % n))
      (apply +)
      (= n))))

(defcheck solution-344d07
  (fn is-perfect [n]
    (loop [sum 0, x 0]
      (cond
        (= sum n) true
        (> sum n) false
        :else (recur (+ sum x) (inc x))
        )
      )
    ))

(defcheck solution-345ed19
  #(= %
     (reduce +
       (for [d (range 1 %)
             :when (zero? (mod % d))] d))))

(defcheck solution-34603b0
  (comp boolean #{6 496 8128}))

(defcheck solution-34848a43
  (fn perf [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2))))]
      (= (apply + divisors) n)
      )))

(defcheck solution-34e94490
  (fn[n]
    (let [factors
          (fn[n]
            (for [i (range 1 n) :when (zero? (mod n i))] i))]
      (= n (reduce + (factors n))))))

(defcheck solution-352d1731
  (fn perfect? [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-354b091a
  (fn [n]
    (letfn [(pdivs [n]
              (loop [s 2
                     n n
                     ds []]
                (if (= 1 n)
                  ds
                  (if (zero? (mod n s))
                    (recur s (/ n s) (conj ds s))
                    (recur (inc s) n ds)))))
            (subvecs [v]
              (loop [v v
                     ss [[]]]
                (if (empty? v)
                  ss
                  (recur (rest v) (into ss (map #(conj % (first v)) ss))))))]
      (= n (reduce + 0 (filter #(< % n) (map #(reduce * 1 %) (distinct (subvecs (pdivs n))))))))))

(defcheck solution-358f0e97
  (fn [n]
    (= n
      (apply + (filter #(= 0 (rem n %)) (take (dec n) (iterate inc 1 )) )))))

(defcheck solution-361bc6ca
  (fn perfect?- [n]
    "80. Write a function which returns true for perfect numbers and false otherwise."
    (let [divisors- (fn [x] (filter #(= 0 (rem x %)) (range 1 x)))]
      (= (apply + (divisors- n)) n))))

(defcheck solution-363c5f8c
  (fn [n]
    (->> (range 1 (inc (/ n 2)))
      (filter #(= 0 (mod n %)))
      (reduce +)
      (= n)
      )
    ))

(defcheck solution-366e0393
  (fn perfect?
    [x]
    (= (apply + (filter #(zero? (mod x %)) (range 1 (inc (/ x 2))))) x)))

(defcheck solution-369d1bb6
  (fn [n]
    (let [f (fn [n] (for [i (range 1 n) :when (= 0 (rem n i))] i))]
      (= n (reduce + (f n))))))

(defcheck solution-36a4adc6
  (fn perfect-num? [n]
    (letfn [(next-divisor [t n]
              (when (<= t (quot n 2))
                (if (zero? (mod n t))
                  t
                  (recur (inc t) n))))
            (divisor-seq
              ([n]
               (divisor-seq 1 n))
              ([prev n]
               (cons prev
                 (lazy-seq (if-let [next (next-divisor (inc prev) n)]
                             (divisor-seq next n))))))]
      (= n (reduce + (divisor-seq n))))))

(defcheck solution-38945930
  (fn [x]
    (let [factors (filter #(zero? (mod x %)) (range 1 x))]
      (= x (apply + factors)))))

(defcheck solution-389c3474
  (fn [x]
    (letfn [(divisors [n]
              (filter #(= 0 (mod n %))
                (range 1  n)))]
      (= (reduce + (divisors x))
        x))))

(defcheck solution-38db2bfe
  (fn [n] (= n
            (->> (range 1 n)
              (filter #(zero? (mod n %)))
              (apply +)       ) )))

(defcheck solution-394231cb
  (fn [num]
    (->> (map inc (range (/ num 2)))
      (filter #(= 0 (mod num %)))
      (apply +)
      (= num))))

(defcheck solution-39f2f510
  (fn [n]
    (let [divisors
          (fn [m]
            (filter
              #(= 0 (mod m %))
              (drop 1 (range m))))]
      (=
        n
        (reduce
          +
          0
          (divisors n))))))

(defcheck solution-3a15b4e7
  (fn perfect-number[n]
    (= n (reduce #(if (= 0 (mod n %2)) (+ % %2) %) 1 (range 2 n)))))

(defcheck solution-3aab4922
  (fn perf-number? [x]
    (letfn [(find-divisors [x]
              (let [possible-divs (take (quot x 2) (iterate inc 1))]
                (filter #(zero? (rem x %)) possible-divs)))]
      (= x (reduce + (find-divisors x))))))

(defcheck solution-3b0d5857
  (fn [n]
    (letfn [(divisors [n divisors candidate]
              (if (< candidate n)
                (if (= (mod n candidate) 0)
                  (recur n (conj divisors candidate) (inc candidate))
                  (recur n divisors (inc candidate)))
                divisors))]
      (= n (reduce + 0 (divisors n [] 1))))))

(defcheck solution-3b2223d6
  (fn [n](letfn [(dvd [x d] (if (= x (* d (int (/ x d)))) true false))
                 (mysqrt [k y]
                   (if (> (* k k) y) (dec k) (recur (inc k) y) ))
                 (fakts [res t m] (let [q (mysqrt 1 m)]
                                    (if (> t q) res
                                                (if (dvd m t)
                                                  (recur (conj (conj res t) (/ m t)) (inc t) m )
                                                  (recur res (inc t) m)
                                                  )
                                                )
                                    )) ]
           (if (= n (reduce + (vec (fakts #{1} 2 n )))) true false)
           )      ))

(defcheck solution-3b5c1c84
  #(= % (apply + (for [x (range 1 %)] ({0 x} (rem % x) 0)))))

(defcheck solution-3b9a9ac4
  #(= % (reduce + (filter (fn [y] (zero? (rem % y))) (range 1 %)))))

(defcheck solution-3bffcda5
  (fn [x]
    (letfn [(factors [x]
              (loop [xs [] i 1]
                (if (> (* i i) x)
                  (vec (sort (distinct xs)))
                  (if (zero? (rem x i))
                    (recur (conj xs i (/ x i)) (inc i))
                    (recur xs (inc i))))))]
      (= x (reduce + (pop (factors x)))))))

(defcheck solution-3c87b79a
  (fn perfect? [x]
    (= x (apply + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-3c94ddc9
  (fn [n]
    (let [c (range 2 n)
          d (filter #(zero? (mod n %)) c)]
      (= n (reduce + 1 d)))))

(defcheck solution-3ca6bc72
  (fn [x] (->> (filter #(zero? (mod x %)) (range 1 x)) (apply +) (= x))))

(defcheck solution-3cab5f86
  (fn [x]
    (= x
      (->> (range 2 (Math/ceil (Math/sqrt x)))
        (filter #(= 0 (mod x %1)))
        (mapcat #(list %1 (/ x %1)))
        (reduce +)
        (+ 1)))))

(defcheck solution-3cb3ff3e
  (fn [v]
    (let [possibledv (take (dec v) (map inc (range v))) ]
      (= v (reduce + (filter #(= (mod v %1)  0) possibledv)))
      )
    ))

(defcheck solution-3ceababf
  (fn [x]
    (let [a (Math/round (/ x 2.0))
          v (filter #(= (rem x %) 0) (range 1 (inc a)))]
      (= (reduce + v) x))))

(defcheck solution-3cf30023
  (fn [n] (let [divisors (filter #(zero? (mod n %))
                           (range 1 n))]
            (= n (apply + divisors)))))

(defcheck solution-3d099b1
  (fn perfect? [x] (letfn[(divisors [n]
                            (filter #(zero? (mod n %)) (range 1 (inc (Math/ceil (/ n 2))))))]
                     #_(println (divisors x))
                     (= (apply + (divisors x)) x))))

(defcheck solution-3e5e647
  (fn [x]
    (let [divs (filter #(= (rem x %) 0) (range 1 (inc (/ x 2))))]
      (= x (reduce + divs))
      )
    ))

(defcheck solution-3eafcb97
  (fn is-perfect[a-num]
    (letfn [(divisors [n]
              (filter #(zero? (rem n %)) (range 1 (inc (/ n 2)) ) )
              )]
      (= a-num (reduce + (divisors a-num)))
      )
    ))

(defcheck solution-3f56b7fe
  (fn [n]
    (->> n
      (range 1)
      (filter #(= 0 (rem n %)))
      (apply +)
      (= n))))

(defcheck solution-404a2438
  (let [divisors (fn [n] (for [d (range 1 n) :when (zero? (mod n d))] d))]
    (fn perfect? [n] (= n (apply + (divisors n))))))

(defcheck solution-405e81f8
  #(= % (loop[sum 0 cnt (dec %)](if (= cnt 0) sum (recur (if(=(rem % cnt )0 )(+ sum cnt) sum)(dec cnt ))))))

(defcheck solution-4145be31
  (fn [n]
    (= n  (reduce +
            (reduce (fn [a x] (if (= 0 (mod n x))
                                (conj a x)
                                a ) )
              []
              (range 1 n))))

    ))

(defcheck solution-41bba7d0
  (fn perfect? [n]
    (let [divisors (set (filter #(zero? (mod n %)) (range 1 n)))]
      (= n (reduce + 0 divisors)))))

(defcheck solution-424af022
  #(= (reduce + (loop[n % c 2 d [1]]
                  (cond
                    (= c n) d
                    (= 0 (rem n c)) (recur n (inc c) (conj d c))
                    :else (recur n (inc c) d)))) %))

(defcheck solution-42fbf578
  (fn perfect-num? [n]
    (and (not (odd? n))
         (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n)))))))

(defcheck solution-43345d73
  (fn [n]
    (= n (apply + (filter #(zero? (rem n %))
                    (range 1 (inc (int (/ n 2)))))))))

(defcheck solution-4472c3e8
  (fn [n]
    (= n (apply +
           (filter #(zero? (rem n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-44ad2288
  (fn
    [n]
    (= n (apply + (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-44fb6827
  (fn [n]
    (= n (reduce + 0
           (filter #(= (rem n %) 0)
             (range 1 n))))))

(defcheck solution-4568e38f
  (fn [a] (let [adaad (filter #(= 0 (mod a %)) (range 1 a))]
            (= a (apply + adaad)))))

(defcheck solution-458ab473
  (fn [x]
    (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-459121b9
  (fn [num]
    (let [divisors (filter #(zero? (mod num %)) (range 1 num))]
      (= (reduce + divisors) num)
      )
    ))

(defcheck solution-464d686a
  (fn [n]
    (= (apply + (filter #(zero? (rem n %)) (range 1 n))) n)))

(defcheck solution-468aef7c
  (fn [n] (= n (reduce + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-46a599f0
  (fn perfNum [x] (= x (reduce + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-470af7ea
  (fn [n]
    (= n
      (->> (range 1 n)
        (filter #(zero? (rem n %)))
        (apply +)))))

(defcheck solution-473f7cbe
  #(= % (reduce + (remove nil? (for [i (range 1 %)] (if (= 0 (mod % i)) i))))))

(defcheck solution-47805cab
  (fn [num]
    (->> (range 1 num)
      (filter #(= 0 (mod num %)))
      (reduce +)
      (= num))))

(defcheck solution-4800cfb8
  (fn [n]
    (= (apply +
         (filter #(zero? (rem n %))
           (range 1 n)))
      n)))

(defcheck solution-4863bd18
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (mod n %))
          (range 1 n))))))

(defcheck solution-48c3bf83
  #(= %
     (apply +
       (for [i (map inc (range (quot % 2))) :when (zero? (mod % i))]
         i))))

(defcheck solution-48d3b07
  (fn [n]
    (= n
      (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-48f727bc
  (fn perfect [n]
    (->> (/ n 2)
      (inc)
      (range 1)
      (filter #(= 0 (rem n %)))
      (reduce +)
      (= n))))

(defcheck solution-49163b3a
  (fn perfect? [n]
    (let [divisors (filter #(zero? (mod n %)) (range 1 n))]
      (= n (apply + divisors)))))

(defcheck solution-49ef7c14
  (fn [n]
    (let [divisors (->> (range 1 (-> n Math/sqrt Math/ceil int))
                     (filter #(zero? (rem n %)))
                     (mapcat #(vector % (quot n %)))
                     set)
          divisors (disj divisors n)]
      (= n (reduce + divisors)))))

(defcheck solution-4a64236
  (fn [n]
    (let [n (long n)
          sqrt-n (Math/abs (Math/sqrt n))]
      (loop [i 2 sum 0]
        (if (<= i sqrt-n)
          (let [j (quot n i)]
            (if (== n (* i j))
              (recur (inc i) (+ sum i j))
              (recur (inc i) sum)))
          (== n (inc sum)))))))

(defcheck solution-4a785080
  (fn [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))
    ))

(defcheck solution-4b1895d5
  #(= % (reduce (fn [res x] (if (zero? (rem % x)) (+ res x) res)) 1 (range 2 %))))

(defcheck solution-4b2d8de7
  (fn [n]
    (let [ds (filter #(= 0 (rem n %)) (range 1 n))
          ]
      (= n (apply + ds))
      )
    ))

(defcheck solution-4b33db39
  (fn [n]
    (= n
      (apply +
        (filter
          #(zero? (mod n %))
          (range 1 (inc (int (/ n 2)))))))))

(defcheck solution-4c024ade
  (fn [x]
    (loop [s 1 i 2]
      (cond
        (= i x) (= s x)
        (= 0 (mod x i)) (recur (+ s i) (inc i))
        :else (recur s (inc i))))))

(defcheck solution-4c203076
  (fn [num]
    (let [divisors (filter
                     #(= 0 (mod num %))
                     (range 1  num))]
      (= num (reduce + divisors)))))

(defcheck solution-4c3fcaab
  (fn perfect-numbers [n]
    (letfn [(divisors [a]
              (map #(inc (first %)) (filter (fn [x] (zero? (second x))) (map-indexed #(list %1 (rem a %2)) (range 1 a)))))]
      (= (reduce + (divisors n)) n))))

(defcheck solution-4c94b9fd
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-4ca06d0f
  (fn [x]
    (letfn [(factors [x]
              (loop [xs [] i 1]
                (if (> (* i i) x)
                  (vec (sort (distinct xs)))
                  (if (zero? (rem x i))
                    (recur (conj xs i (/ x i)) (inc i))
                    (recur xs (inc i))))))]
      (= x (reduce + (pop (factors x)))))))

(defcheck solution-4cb9f9dd
  (fn [n]
    (loop [p 1]
      (let [p2 (fn [p] (apply * (repeat p 2)))
            r (* (p2 p) (dec (p2 (inc p))))]
        (cond
          (< r n) (recur (inc p))
          (= n r) true
          :else false)))))

(defcheck solution-4cffeb67
  (fn per [x]
    (= x (reduce + (filter #(= (mod x %) 0) (range 1 (- x 1)))))))

(defcheck solution-4d734c65
  (fn pn [n]
    (loop [c 1 s 0]
      (if (= n s)
        true
        (if (> s n)
          false
          (recur (inc c) (+ s c)))))))

(defcheck solution-4d842264
  #(= (apply + (reduce (fn [t v] (if (= (rem % v) 0) (conj t v) t)) [] (range 1 %))) %))

(defcheck solution-4d9f20d0
  (fn [n]
    (letfn [(d [n]
              (filter #(zero? (mod n %))
                (range 1 (inc (/ n 2)))))]
      (= n (reduce + (d n))))))

(defcheck solution-4dafff1a
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-4e4fc822
  (fn [n]
    (letfn [(divisors [x]
              (filter
                #(zero? (rem x %))
                (range 1 (+ 1 (Math/floor (/ x 2))))))]
      (= n (reduce + (divisors n))))))

(defcheck solution-4e64604
  (fn [n]
    (letfn [(divs [n] (filter #(= 0 (mod n %)) (range 1 n)))]
      (= n (apply + (divs n))))))

(defcheck solution-4ea8122d
  (fn [input]
    (let [divisors (filter #(zero? (rem input %)) (range 1 input))]
      (= input (apply + divisors)))))

(defcheck solution-4efa2c26
  (fn[x] (= x (reduce + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-4f18fa65
  (fn [n]
    (= n (reduce + (take-while #(> n %) (filter #(= 0 (mod n %)) (rest (range))))))))

(defcheck solution-4fa5a355
  (fn [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-4fd2ef4
  (fn [n]
    (= n
      (reduce + (filter #(zero? (rem n %))(range 1 (inc (/ n 2))))))))

(defcheck solution-4fd70d21
  (fn perfect [n]
    (let [factors (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (apply + factors)))))

(defcheck solution-4fd9534c
  (fn [n]
    (= (apply + (for [i (range 1 n)] (if (= 0 (mod n i)) i 0))) n)))

(defcheck solution-4ff6c2f
  (fn [n]
    (=
      n
      (apply
        +
        (filter
          #(= (mod n %) 0)
          (range 1 n))))))

(defcheck solution-5094ed34
  (fn [n]
    (= n
      (apply +
        (filter #(zero? (mod n %)) (range 1 (inc (quot n 2))))
        )
      )
    ))

(defcheck solution-50dc1cbd
  (fn __
    [num]
    (= num
      (letfn [(divisible? [x] (zero? (rem num x)))]
        (->> (range 1 num) (filter divisible?) (reduce +))))))

(defcheck solution-50f83fc0
  (fn [x]
    (let [find-factors (fn
                         [n]
                         (loop [candidates (range 2 n)
                                acc '(1)]
                           (if (empty? candidates)
                             acc
                             (recur
                               (rest candidates)
                               (if (= 0 (mod n (first candidates)))
                                 (conj acc (first candidates))
                                 acc)))))]
      (= x (reduce + (find-factors x))))))

(defcheck solution-5130e0ba
  (fn f [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-51b9707d
  (fn [n]
    (= (reduce #(if (= (rem n %2) 0) (+ %1 %2) %1) (range 1 n)) n)))

(defcheck solution-5286d435
  (fn perfect-numbers [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-52dca584
  (fn [x]
    (= x (reduce + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-52fc29d
  (fn [x]
    (=
      x
      (apply +
        (filter
          #(== (int (/ x %)) (/ x %))
          (map inc (range (dec x)))
          )
        )
      )
    ))

(defcheck solution-536a1d5b
  #(loop [x 2 y 1] (if (> y %) false
                               (if (= y %) true
                                           (recur (inc x) (apply + (range 1 x)))
                                           )
                               )
                   ))

(defcheck solution-54045288
  #(loop [i 0
          s 0]
     (case (compare s %)
       0 true
       1 false
       (recur (inc i) (+ s i)))))

(defcheck solution-545e2ad7
  (fn perfect? [x]
    (let [
          divides? (fn [x y]
                     (== 0 (mod y x)))
          divisors (fn [x]
                     (->> (range 1 x)
                       (filter #(divides? % x))))
          sum (fn [xs]
                (reduce + 0 xs))
          ]
      (== x (sum (divisors x))))))

(defcheck solution-546a5e5d
  (fn [x]
    (let [div (filter #(= 0 (rem x %)) (range 1 x))]
      (= x (reduce + div))
      )
    ))

(defcheck solution-54770c3
  (fn [n]
    (= n (reduce + (filter #(zero? (rem n %))
                     (range 1 (inc (quot n 2))))))))

(defcheck solution-55e11d44
  (fn [n]
    (->>
      (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-5626b036
  (fn [n] (= n (apply + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-562bb707
  (fn [n]
    (let [factors (filter #(= 0 (rem n %)) (range 1 n))]
      (= (apply + factors) n)
      )
    ))

(defcheck solution-56511fd8
  (fn p? [n]
    (letfn [(divs [x] (filter #(zero? (mod n %)) (range 1 (dec n))))]
      (= n (apply + (divs n))))))

(defcheck solution-569329
  (fn [n]
    (= n (reduce +
           (filter
             #(zero? (mod n %))
             (range 1 n))))))

(defcheck solution-56db5713
  (fn [n]
    (let [divisors (fn [n] (filter #(= 0 (rem n %)) (range 1 n)))]
      (= n (apply + (divisors n))))))

(defcheck solution-5751f445
  (fn [x]
    (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))
    ))

(defcheck solution-5791a0e8
  (fn [n]
    (let [
          divisors (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))]
      (= n (apply + divisors)))))

(defcheck solution-57b2d7d8
  (fn [x]
    (let [
          devisor (filter #(= 0 (rem x %)) (range 1 x))
          ]
      (= x  (apply + devisor))
      )
    ))

(defcheck solution-57cae195
  (fn [n]
    (let [factors (filter #(zero? (mod n %)) (range 1 n))]
      (= n (apply + factors))
      )
    ))

(defcheck solution-57d3a5cc
  (fn [x] (= x (first (drop-while #(< % x) (map #(apply + (take % (iterate inc 1))) (iterate inc 1)))))))

(defcheck solution-57e9d830
  (fn [n]
    (= n (reduce + (filter  #(integer? (/ n %)) (range 1 n))))))

(defcheck solution-583533a0
  (fn [x] (= x (apply + (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-5887da17
  (fn[n] (= n (apply + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-58c5ee7c
  (fn [n] (= n (reduce #(if (zero? (rem n %2)) (+ % %2) %) 0 (range 1 n)))))

(defcheck solution-58f8f219
  (fn perfect? [n]
    (=
      n
      (apply +
        (filter (fn [x] (zero? (mod n x))) (range 1 n))))))

(defcheck solution-59099d6b
  #(contains? #{6 496 8128} %))

(defcheck solution-59962d4e
  (fn [n] (= (apply + (filter #(= 0 (mod n %)) (range 1 n))) n)))

(defcheck solution-59fe37cb
  (fn [n]
    (= n
      (apply +
        (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-5a3b2c9d
  (fn perfect? [n]
    (let [dividends (filter #(= 0 (rem n %)) (range 1 n))]
      (= n (apply + dividends)))))

(defcheck solution-5a60fb17
  (fn [i]
    (= i (apply + (filter #(= 0 (rem i %)) (range 1 i))))))

(defcheck solution-5a6ede18
  (fn perfect? [x]
    (= x (reduce + (filter
                     #(= 0 (mod x %))
                     (range 1 (inc (/ x 2))))))))

(defcheck solution-5af72ac1
  #(cond (= % %3) true
         (> % %3) false
         :else (recur (if (= 0 (rem %3 %2)) (+ % %2) %)
                 (inc %2)
                 %3)) 0 1)

(defcheck solution-5b0b162e
  (fn [n]
    (= n (apply +
           (into #{}
             (filter (fn [i] (= 0 (mod n i)))
               (range 1 n)))))))

(defcheck solution-5b7b3132
  (fn [n]
    (= n
      (reduce +
        (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-5b94f32
  #(= % (apply + (for [x (rest (range %)) :when (= (rem % x) 0)] x))))

(defcheck solution-5bc2d7f8
  #(true? (some (partial = %) (reductions + (range 1 %)))))

(defcheck solution-5c2fc50e
  (fn [n]
    (let [sieve (fn sieve [s]
                  (cons (first s)
                    (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                       (rest s))))))
          nb-primes (+ (int (Math/sqrt n)) 1)
          primes (take nb-primes (sieve (iterate inc 2)))
          decomp (loop [n n primes primes r []]
                   (let [p (first primes)]
                     (if (> (* p p) n)
                       (conj r n)
                       (if (= (mod n p) 0)
                         (let [[new-n ps] (loop [n n ps []]
                                            (if (= (mod n p) 0)
                                              (recur (/ n p) (conj ps p))
                                              [n ps]))]
                           (recur new-n (rest primes) (apply conj r ps)))
                         (recur n (rest primes) r)))))
          cart (fn cart [colls]
                 (if (empty? colls)
                   '(())
                   (for [x (first colls)
                         more (cart (rest colls))]
                     (cons x more))))
          pdivisors (map (fn [ps] (reductions * ps)) (partition-by identity decomp))
          divisors (apply conj
                     (into [1] (mapcat identity pdivisors))
                     (map #(reduce * %) (cart pdivisors)))]
      (= n (/ (reduce + divisors) 2)))))

(defcheck solution-5cb2bdf7
  (fn [n]
    (let [divisors (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))]
      (= n (apply + divisors)))))

(defcheck solution-5d1657ea
  #(= % (apply + (for [i (range 1 %) :when (= 0 (mod % i))] i))))

(defcheck solution-5d350aaf
  #(= % (apply + (range (/ (inc (int (Math/sqrt (inc (* 8 %))))) 2)))))

(defcheck solution-5d5e578
  (fn [n]
    (= (reduce +
         (filter #(= 0 (rem n %)) (range 1 n)))
      n)))

(defcheck solution-5dbe58fa
  (fn[x] (= x (apply + (filter #(zero? (mod x %) ) (range 1 x))))))

(defcheck solution-5deeb497
  (fn [x] (let [divides-x #(= (rem x %) 0)
                divisors (filter divides-x (range 1 x))]
            (= x (apply + divisors)))))

(defcheck solution-5e0f4019
  (fn [x]
    (= x
      (apply +
        (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-5e2b9455
  (fn [n]
    (== n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-5f0ac640
  (fn [x]
    (= x (reduce +
           (filter #(zero? (rem x %)) (range 1 (dec x)))))))

(defcheck solution-5f0e463f
  (fn [n]
    (== n
      (reduce +
        (for
         [i (range 1 (inc (/ n 2)))
          :when (= 0 (rem n i))]
          i)))))

(defcheck solution-5f43c5cb
  (fn [x]
    (let [divisors (filter #(zero? (mod x %)) (range 1 x))]
      (= x (apply + divisors)))))

(defcheck solution-5f578878
  #(= % (apply + (for [d (range 1 %) :when (zero? (mod % d))] d))))

(defcheck solution-5f5cd0e9
  (fn is-perfect? [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-5fa1ec37
  (fn [x]
    (= x
      (apply +
        (filter #(zero? (mod x %))
          (range 1 x))))))

(defcheck solution-5fc001e7
  (fn [n] (= 0 (- n 1 (apply + (filter #(= 0 (mod n %)) (range 2 (inc (quot n 2)))))))))

(defcheck solution-5ffa5d71
  #(boolean (#{6 496 8128} %)))

(defcheck solution-603f5ba8
  (fn [n]
    (= n
      (apply +
        (filter
          #(zero? (rem n %))
          (range 1 n))))))

(defcheck solution-60c11fde
  (fn [x] (= x (reduce + (remove #(< 0 (rem x %)) (range 1 x))))))

(defcheck solution-6119cbd2
  (fn [n]
    (= n (apply + (filter #(= (mod n %1) 0) (range 1 n))))))

(defcheck solution-61a638bb
  (fn [n] (= n (reduce + (for [i (filter #(= 0 (rem n %)) (range 1 n))] i)))))

(defcheck solution-6255a57d
  (fn [number]
    (let [divisors (loop [divisors #{1} current 2]
                     (if (> (* current current) number)
                       divisors
                       (recur (if (= 0 (rem number current))
                                (conj divisors current (quot number current))
                                divisors)
                         (inc current))))]
      (= number (reduce + divisors)))))

(defcheck solution-627e553f
  (fn [n] (= n (reduce + (filter (fn [d] (= 0 (mod n d))) (range 1 n))))))

(defcheck solution-6354f64d
  (fn [n]
    (let [h (inc (Math/ceil (/ n 2)))]
      (= n (apply + (filter #(zero? (mod n %)) (range 1 h)))))))

(defcheck solution-639b3a8e
  (fn [x]
    (let [divisors (filter #(= 0 (rem x %)) (range 1 x))
          ]
      (= x (reduce + divisors))
      )
    ))

(defcheck solution-63ec9812
  (fn [n]
    (= n
      (reduce
        #(if (zero? (mod n %2)) (+ %1 %2) %1)
        0
        (range 1 n)))))

(defcheck solution-63fd3377
  #(= % (apply + (for [x (range 1 %) :when (= 0 (rem % x))] x))))

(defcheck solution-64193c8a
  (fn perfect-number?
    ([x]
     (perfect-number? x 1 []))
    ([x y z]
     (if (= x y)
       (if (= x (apply + z))
         true
         false)
       (if (= 0 (rem x y))
         (recur x (inc y) (conj z y))
         (recur x (inc y) z))))))

(defcheck solution-6440d8b7
  (fn [x]
    (let [divisors (for [i (range 1 x) :when (zero? (rem x i))] i)]
      (= x (apply + divisors)))))

(defcheck solution-6481ea9d
  (fn [n]
    (== n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-64f3725e
  #(let
    [divisors (fn divisors
                [n]
                (filter (comp zero? (partial rem n)) (range 1 n)))]
     #_(println (divisors %))
     (= % (reduce + (divisors %)))))

(defcheck solution-6587eb2a
  (fn [x]
    (->> (range 1 x)
      (filter #(zero? (mod x %)))
      (apply +)
      (= x))))

(defcheck solution-6587faf6
  (letfn [(divisors [n] (filter #(= (mod n %) 0) (range 1 n)))]
    (fn perfect? [n] (= n (reduce + (divisors n))))))

(defcheck solution-65a02fa3
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (apply + divisors)))))

(defcheck solution-65f1e3c4
  (fn [x]
    (= x (reduce +
           (map #(second %)
             (filter #(zero? (first %))
               (map #(vector % %2)
                 (map #(mod x %) (range 1 x))
                 (range 1 x))))))))

(defcheck solution-6641a516
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-664755aa
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (rest (range n)))]
      (= n (apply + divisors)))))

(defcheck solution-66620856
  (fn [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-66c3d927
  (fn divisor [n]
    (->> (filter #(= 0 (rem n %)) (range 2 (inc (/ n 2))))
      (apply +)
      inc
      (= n)
      )))

(defcheck solution-66d10ace
  (fn perfect-numbers [x]
    ((comp
      (partial = x)
      (partial apply +)
      (partial filter #(= 0 (mod x %)))
      ) (range 1 x))))

(defcheck solution-68274062
  #(= % (apply + (for [x (range 1 %)
                       :when (= 0 (rem % x))]
                   x))))

(defcheck solution-688f268c
  (fn
    [num]
    (->> (for [i (range 2 (+ (Math/sqrt num) 0.01))
               :when (= 0 (mod num i))]
           (+ i (/ num i)))
      (apply +)
      inc
      (= num))))

(defcheck solution-68c961
  (fn perfect? [k]
    (letfn
     [(divisors [n] (filter #(not (nil? %)) (for [i (range 1 (inc (quot n 2)))] (when (zero? (mod n i)) i))))]
      (= k (reduce + (divisors k)))
      )
    ))

(defcheck solution-690612b3
  (fn [a] (= a (reduce + (filter #(zero? (mod a %)) (range 1 a))))))

(defcheck solution-698a1356
  (fn perfect? [input]
    (let [divisors (filter #(zero? (rem input %))
                     (range 1 (inc (/ input 2))))]
      (= (apply + divisors)
        input))))

(defcheck solution-69a12dec
  #(= % (reduce + (filter (complement nil?) (reduce into [1] (for [x (range 2 (Math/sqrt %))] (if (= 0 (rem % x)) (list x (/ % x)))))))))

(defcheck solution-69aa4eff
  (fn [n]
    (=
      n
      (apply
        +
        (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-69bd2817
  (fn perfect? [x]
    (letfn [(divisors [x]
              (filter #(zero? (rem x %)) (range 1 x)))]
      (== x (reduce + (divisors x))))))

(defcheck solution-69d790bf
  (fn [n] (= n (apply + 1 (filter integer? (map #(/ n %) (range 2 n)))))))

(defcheck solution-69f3c679
  (fn [n]
    (= n
      (apply + (filter #(= 0 (mod n %))
                 (range 1 (+ 1 (/ n 2))))))))

(defcheck solution-6a031891
  (fn [n] (= n (apply + (filter #(= (rem n %) 0)(range 1 n))))))

(defcheck solution-6a1f08f3
  (fn [n]
    (letfn [(divisors [n]
              (filter #(zero? (rem n %)) (range 1 n)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-6a24a7b1
  (fn [n]
    (= n (apply + (filter  #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-6a31c1e7
  (fn [n] (= n (reduce + (for [x (range 1 n)] (if (= (mod n x) 0) x 0))))))

(defcheck solution-6a7f544
  (fn __ [n]
    (let [cand-divisors (range 1 n)]
      (= n
        (reduce + (filter #(= 0 (mod n %)) cand-divisors))))))

(defcheck solution-6a88a70e
  (fn [x]
    (let
     [divisors (filter #(zero? (mod x %)) (range 1 x))]
      (= x (reduce + divisors)))))

(defcheck solution-6a99c650
  (fn [n]
    (= n (reduce +
           (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-6acf424e
  (fn [n] (=  (reduce + (filter (fn [x] (zero? (mod n x))) (range 1 n))) n)))

(defcheck solution-6b00b931
  (fn [n] (letfn [(div [x] (flatten (keep #(when (= 0 (mod x %)) [% (quot x %)]) (range 1 (inc (Math/sqrt x))))))]
            (let [d (disj (set (div n)) n)]
              (= n (apply + d))))))

(defcheck solution-6b6c0eed
  (fn perfect?
    [n]
    (->> (range 1 n)
      (filter #(zero? (rem n %)))
      (reduce +)
      (= n))))

(defcheck solution-6c31373
  (fn [n]
    (= n
      (apply +
        (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-6c781020
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
      (= (apply + divisors) n))))

(defcheck solution-6c9f0b5a
  (fn [x]
    (= x (reduce + ((fn [n]
                      (filter #(zero? (mod n %)) (take-while #(> n %) (rest (range))))) x)))))

(defcheck solution-6d83918d
  (fn [n]
    (let [divisors
          (loop [x 2 result [1]]
            (cond
              (= x n) result
              (zero? (rem n x)) (recur (inc x) (conj result x))
              :else (recur (inc x) result)))]
      (if (= n (reduce + divisors)) true false))))

(defcheck solution-6d99687
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-6e076f1e
  (fn perfect-number [n]
    (let [ld (filter #(= (mod n %) 0) (range 1 n))] ;;divisors
      (= n (apply + ld)))))

(defcheck solution-6e0ebbd9
  (fn [n] (= n (reduce + (filter #(= (mod n %) 0) (take (dec n) (rest (iterate dec n))))))))

(defcheck solution-6e394d86
  (fn p [n] (= n (apply + (for [x (map inc (range (/ n 2))) :when (= (mod n x) 0)] x)))))

(defcheck solution-6eb5b093
  (fn [n]
    (= n
      (->>
        (range 1 (inc (/ n 2)))
        (filter #(= (mod n %) 0))
        (reduce +)))))

(defcheck solution-6ed400d8
  (fn [n]
    (= n
      (reduce +
        (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-6ee9adb
  (fn [n]
    (= n (reduce + (filter #(zero? (rem n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-6f36da33
  (fn [n] (= (apply + (filter #(= 0 (rem n %)) (range 1 n))) n)))

(defcheck solution-6f633ab5
  (fn [x]
    (= (reduce +
         (filter #(= 0 (mod x %))
           (range 1 x)))
      x)))

(defcheck solution-6fb038d4
  (letfn
   [(divisors [n] (filter #(let [d (/ n %)] (= d (int d))) (range 1 (inc (/ n 2)))))]
    (fn [n] (= n (reduce + 0 (divisors n))))))

(defcheck solution-6fb0f4d8
  (fn perfect-number? [x]
    (letfn [(dividers [a]
              (filter #(= (mod a %) 0) (take (- a 1) (iterate inc 1)))
              )
            ]
      (= x (reduce + (dividers x)))
      )
    ))

(defcheck solution-6fd1e37
  (fn [v] (= v (->> (range 1 v) (filter #(= 0 (rem v %))) (reduce +)
                 ))))

(defcheck solution-6fdebc92
  (letfn [(D [n]
            (filter #(zero? (rem n %)) (range 1 n)))]
    (fn [n]
      (= n (reduce + (D n))))))

(defcheck solution-6fe69415
  (fn [n]
    (let [divisors (filter #(= (mod n %) 0) (range 1 n))]
      (= n (reduce + divisors)))))

(defcheck solution-6ff902c9
  (fn [n] (= n (reduce + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-713b887d
  (fn [n]
    (= (apply + (filter #(= (mod n %) 0) (range 1 n))) n)))

(defcheck solution-7140c654
  (fn [x]
    (letfn
     [(divisors[x] (filter #(zero? (rem x %) ) (range 1 (inc (quot x 2)))))]
      (= x (reduce + (divisors x))))))

(defcheck solution-7174168e
  (fn [num]
    (= num
      (apply +
        (filter #(zero? (mod num %)) (range 1 num))))))

(defcheck solution-719cc814
  #(->> (range 1 (inc (/ % 2)))
     (filter (partial (comp zero? rem) %))
     (apply +)
     (= %)))

(defcheck solution-71c845cc
  (fn [x] (= x (apply + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-729d67f6
  (fn [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-72b3c4b
  (fn p [n]
    (= n (apply + (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-72b4a7fc
  (fn [n] (= n  (reduce + (filter #(= (rem  n %) 0) (range 1 n))))))

(defcheck solution-732683c8
  (fn [x]
    (= x  (reduce +
            (for [ n (range 1 (inc (/ x 2)) ) ]
              (if (= 0 (rem x n)) n 0 ))))))

(defcheck solution-732a21ff
  (fn [x]
    (->>
      (range 1 x)
      (filter #(zero? (mod x %)))
      (reduce +)
      (= x))))

(defcheck solution-733a5e82
  #(= % (apply + (for [i (range 1 %) :when (zero? (rem % i))] i))))

(defcheck solution-73c0600c
  (fn [x]
    (= x (apply + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-73dfdcd3
  (fn perf-square[x](= (reduce + ((fn factors [x] (loop [xf [1] i 2] (if (> (* i i) x) (vec (sort (distinct xf))) (if (zero? (rem x i)) (recur (conj xf i (/ x i)) (inc i)) (recur xf (inc i)))))) x)) x)))

(defcheck solution-7460b8b7
  (letfn [(divisors [n]
            (for [i (range 1 n) :when (= 0 (rem n i))] i))]
    #(= % (apply + (divisors %)))))

(defcheck solution-74773646
  (fn perfect? [n]
    (->>
      (range 1 n)
      (filter #(= 0 (rem n %)))
      (apply +)
      (= n))))

(defcheck solution-74dc2b6d
  (fn [number] (->> (range 1 number)
                 (filter #(= 0 (mod number %)))
                 (reduce +)
                 (= number))))

(defcheck solution-7510a55f
  (fn perfect[x]
    (= x (reduce + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-7546f522
  (fn perfectNumber[n]
    (letfn [(divisors[n]
              (sort
                (flatten
                  (map
                    #(list % (/ n %))
                    (filter
                      #(= 0 (mod n %))
                      (range 1 (inc (int (Math/sqrt n)))))))))]
      (= n (apply + (drop-last (divisors n)))))))

(defcheck solution-758d52fe
  (fn [x]
    (let [divs (filter #(= 0 (rem x %)) (range 1 x))]
      (= (apply + divs) x))))

(defcheck solution-763ed9f0
  (letfn [(divisors [n] (loop [divs [1] k 2]
                          (cond
                            (= k n) divs
                            (= (rem n k) 0) (recur (conj divs k) (inc k))
                            :else (recur divs (inc k)))))]
    #(= (reduce + (divisors %)) %)))

(defcheck solution-764144a7
  (fn  [n]
    (let [divisors
          (fn [n]
            (filter #(not (nil? %)) (for [i (range 1 n)]
                                      (if (= (mod n i) 0)
                                        i
                                        nil))))]
      (= n (reduce + (divisors n))))))

(defcheck solution-7728c2eb
  (fn [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-795438ff
  (fn perfect [n]
    (= (reduce +
         (filter #(= (mod n %) 0)
           (range 1 n))) n)))

(defcheck solution-7a0b559d
  (fn [n] (= n (apply + (for [i (range 1 n) :when (#{0} (mod n i))] i)))))

(defcheck solution-7a166ab1
  (fn perfect-num? [x]
    (= (->> (range x)
         rest
         (filter #(= 0 (rem x %)))
         (apply +))
      x)))

(defcheck solution-7a321daf
  (fn perfect? [n]
    (letfn [(divisors [x] (filter #(= (rem x %) 0) (range 1 (inc (/ x 2)))))]
      (= n (reduce + (divisors n))))))

(defcheck solution-7a4253b3
  (fn [n] (= n (apply +
                 (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-7aa07c2f
  (fn [x]
    (= x
      (apply +
        (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-7aadabb
  (fn [x]
    (let [ve ((fn cal [num ite vect]
                (loop [n num i ite v vect]
                  (if (>= i n)
                    v
                    (if (= (rem n i) 0)
                      (recur n (inc i) (cons i v))
                      (recur n (inc i) v)
                      )
                    ))
                ) x 1 [])]
      (if (< (count ve) 2)
        (= (reduce + (cons x ve)) x)
        (= (reduce + ve) x)
        )
      )
    ))

(defcheck solution-7b0aa5b6
  (fn [n]
    (= n
      (apply +
        (filter
          #(zero? (mod n %))
          (range 1 n))))
    ))

(defcheck solution-7b21bf4f
  (fn [n] (= n (reduce + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-7b3148b
  (fn [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-7b5338c7
  #(= %
     (reduce +
       (filter (fn[x](= 0 (mod % x)))
         (range 1 %)))))

(defcheck solution-7b7efaf7
  (fn [x]
    (letfn [(divisors [n]
              (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2)))))]
      (= (apply + (divisors x)) x))))

(defcheck solution-7ba520f3
  (fn [n] (= n (reduce + (filter #(zero? (mod n %)) (range 1 (inc (int (/ n 2)))))))))

(defcheck solution-7c177854
  (fn [x]
    (= x (apply + (filter #(= 0 (rem x %)) (range 1 (inc (/ x 2))))))))

(defcheck solution-7c3bcdd3
  (fn prob80
    [num]
    (letfn [(divisors [n]
              (filter #(zero? (mod n %)) (range 1 (+ 1 (/ n 2))))
              )]
      (= (reduce + (divisors num)) num))))

(defcheck solution-7c51aae3
  #(= % (apply + (for [i (range 1 %) :when (zero? (mod % i))] i))))

(defcheck solution-7c51e0bd
  (fn [x]
    (let [factors (filter #(= 0 (mod x %)) (range 1 x))]
      (= x (reduce + factors)))))

(defcheck solution-7c7b124c
  (fn perfect? [n]
    (letfn [(factors [n]
              (filter #(zero? (rem n %)) (range 1 n)))]
      (= n (reduce + (factors n))))))

(defcheck solution-7c7cb91a
  (fn [n]
    (= n
      (reduce #(if (= 0 (mod n %2))
                 (+ %1 %2)
                 %1)
        0 (range 1 (inc (/ n 2)))))))

(defcheck solution-7cde3d45
  (fn perfect? [n]
    (= (->> (range 1 n)
         (filter #(zero? (rem n %)))
         (apply +))
      n)))

(defcheck solution-7d38881
  (fn perfect? [n]
    (letfn [
            (divisors [number divisors-set cal-to]
              (if (= cal-to number)
                divisors-set
                (if (= number (* cal-to (int (/ number cal-to))))
                  (recur number (conj divisors-set cal-to) (inc cal-to))
                  (recur number divisors-set (inc cal-to))
                  ) ; if
                ) ; if
              ) ; divisors
            ]
      #_(println (divisors n #{} 1))
      (= n (reduce + 0 (divisors n #{} 1))))))

(defcheck solution-7d9a6d6f
  (fn [v]
    (= v (reduce +
           (filter #(zero? (mod v %)) (range 1 v))))))

(defcheck solution-7dd6f4f2
  (fn [n]
    (= n
      (->> (range 1 n)
        (filter #(= 0 (rem n %)))
        (apply +)))))

(defcheck solution-7e2d0dab
  (fn [n]
    (if (= n
          (reduce #(+ % %2)
            (for [i (next (range n))]
              (if (zero? (rem n i)) i 0))))
      true
      false)))

(defcheck solution-7e93eb86
  (fn [n]
    (->> (range 1 n)
      (filter #(= 0 (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-7ed7e244
  (fn [n]
    (= n (reduce +
           (map #(if
                  (= (rem n %) 0)
                   %
                   0)
             (range 1 n))))))

(defcheck solution-7f516a1f
  (fn perfect? [n]
    (letfn [(divisors[n]
              (filter #(= 0 (rem n %)) (range 1 (inc (/ n 2)))))]
      (= n (reduce + (divisors n))))))

(defcheck solution-7f5f97fc
  (fn [x]
    (let [divnums (for [i (range 1 (inc (int (/ x 2))))
                        :when (= 0 (rem x i))]
                    i)]
      (= x (reduce + divnums)))))

(defcheck solution-7f65fb11
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (apply + divisors)))))

(defcheck solution-7f66c045
  (fn p [n]
    (loop [i (int (Math/sqrt n))
           s 0]
      (cond (zero? i) (= (* 2 n) s)
            (zero? (mod n i)) (recur (dec i) (+ s i (int (/ n i))))
            :else (recur (dec i) s)))))

(defcheck solution-7fafb0e3
  (fn [n]
    (letfn [(divisors [n]
              (filter #(= 0 (mod n %)) (range 1 n)))
            (sum [ns]
              (reduce + ns))]
      (let [ds (divisors n)]
        (= (sum ds) n)))))

(defcheck solution-800c4770
  (fn [n] (let [pft-lst (reductions + (iterate inc 1))]
            (boolean (some #(= n %) (take (int (/ n 2)) pft-lst))))))

(defcheck solution-806a3f5f
  (fn [n]
    (= n (->>
           (range 1 n)
           (filter #(== 0 (mod n %)))
           (apply +)))))

(defcheck solution-8092769
  (fn [x]
    (= x (reduce + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-815b10b3
  (fn [x]
    (->>
      (range 1 x)
      (filter #(= (mod x %) 0))
      (apply +)
      (= x))))

(defcheck solution-8197f444
  (fn [x] (= x (apply + (filter #(= 0 (rem x %)) (range 1 x) ) ))))

(defcheck solution-81fad43b
  (fn is-perfect? [x]
    (let [divisors #(for [i (range 1 (inc (/ % 2))) :when (zero? (mod % i))]
                      i)]
      (= (apply + (divisors x)) x))))

(defcheck solution-8220e835
  (fn [x]
    (->> (range 1 x)
      (filter #(= 0 (rem x %)), )
      (apply +, )
      (= x, ))))

(defcheck solution-827fbcef
  (fn is-perf-num [n]
    (= n
      (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-82aac75b
  (fn perfect [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 (+ (/ n 2) 1)))))))

(defcheck solution-82c1fd44
  (fn [n]
    (let [divs (filter #(zero? (mod n %)) (range 1 n))]
      (= (apply + divs) n))))

(defcheck solution-82ddd13
  (fn [n]
    (= n
      (loop [d 2
             s 1]
        (if (>= d n)
          s
          (if (= (mod n d) 0)
            (recur (+ d 1) (+ s d))
            (recur (+ d 1) s)))))))

(defcheck solution-82e43d52
  (fn [x]
    (letfn [(divisors [n]
              (set (flatten (for [d (range 1 (inc n))
                                  :while (<= (* d d) n)
                                  :when (zero? (rem n d))]
                              [d (/ n d)]))))
            (sum-of-proper-divisors [n]
              (reduce + (disj (divisors n) n)))]
      (= x (sum-of-proper-divisors x)))))

(defcheck solution-83241669
  (fn [n]
    (= n
      (apply +
        (filter
          #(zero? (rem n %))
          (range 1 (max 2 (inc (quot n 2)))))))))

(defcheck solution-83405dbc
  (fn [n] (= n (reduce + (filter (fn [x] (= 0 (rem n x))) (range 1 n))))))

(defcheck solution-83889950
  #(->>
     (range 1 %)
     (filter (comp zero? (partial mod %)))
     (reduce +)
     (= %)))

(defcheck solution-83903b8c
  (fn [n]
    (= n (reduce +
           (filter #(zero? (mod n %))
             (range (quot n 2) 0 -1))))))

(defcheck solution-83bc02fb
  (fn [x]
    (let [s (filter #(= 0 (mod x %)) (range 1 x))]
      (= x (apply + s)))))

(defcheck solution-83c3e093
  (fn [n]
    (->>
      (range 1 n)
      (filter  #(= 0 (mod n %)) ,)
      (apply + ,)
      (= n ,))))

(defcheck solution-840a979a
  (fn[n]
    (= (dec n) (reduce + (filter #(= 0 (rem n %))
                           (range 2 (dec n)))))))

(defcheck solution-841cff9e
  (fn [n]
    (letfn [(divisors [x] (filter #(= 0 (mod x %)) (range 1 x)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-8500638c
  (fn [x] (= x (reduce + (filter #(= (mod x %) 0) (range 1 (inc (/ x 2))))))))

(defcheck solution-85d469ed
  (fn [d]
    (= d
      (reduce +
        (filter #(zero? (mod d %1)) (range 1 d))))))

(defcheck solution-861f3fd
  (fn [n]
    (let [ds (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (apply + ds)))))

(defcheck solution-863ed81e
  (letfn [(get-divisors [x] (filter #(zero? (rem x %)) (range 1 x)))
          (get-divisors-sum [x] (apply + (get-divisors x)))]
    (fn is-perfect-number? [x]
      (= x (get-divisors-sum x)))))

(defcheck solution-86edb24f
  (fn [n]
    (letfn [(divisors [x]
              (cons 1 (->> (range 2 x)
                        (filter (comp zero?
                                      (partial rem x))))))]
      (= n (apply + (divisors n))))))

(defcheck solution-86f01583
  (fn perfect? [n]
    (= n (reduce + (filter #(= (mod n %) 0) (range 1 (inc (quot n 2))))))))

(defcheck solution-870ba1ec
  (fn prfkt?
    [x]
    (let [nums (range 1 x)
          dvsr? (fn [z] (= 0 (mod x z)))
          dvsrs (filter dvsr? nums)]
      (= x (apply + dvsrs)))))

(defcheck solution-8712c74d
  #(= %
     (apply +
       (filter (comp zero? (partial rem %))
         (range 1 (inc (quot % 2)))))))

(defcheck solution-874d3f04
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-8797e4eb
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-87e8c7ef
  (fn perfect [x]
    (= x
      (apply +
        (for [y (range 1 x)
              :when (= x (* (quot x y) y))] y
                                            )
        )
      )))

(defcheck solution-885798a1
  #(= (apply + (filter (fn [x] (= (mod % x) 0))
                 (range 1 %)))
     %))

(defcheck solution-88583820
  (fn [n]
    (= (apply + (filter (comp (partial = 0) (partial mod n)) (drop 1 (range n)))) n)))

(defcheck solution-890a2292
  (fn perfect? [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-89471102
  (fn [x]
    (->> (/ (inc x) 2)
      (range 1)
      (filter #(zero? (mod x %)))
      (reduce +)
      (= x))))

(defcheck solution-895eae09
  (fn perfect? [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-89e5dce
  (fn [n]
    (= (reduce + (remove #(not= 0 (mod n %)) (range 1 n))) n)
    ))

(defcheck solution-8a0f0fd5
  (fn [x] (->> (range 1 x) (filter #(zero? (rem x %))) (apply +) (= x) )))

(defcheck solution-8a20228e
  (fn [num] (= (reduce + (reduce (fn [v n] (if (= 0 (mod num n)) (conj v n) v)) [1] (range 2 num))) num)))

(defcheck solution-8a6d12eb
  #(= % (loop [c 1 a 0]
          (if (not= c %)
            (recur (inc c) (if (= 0 (rem % c)) (+ a c) a))
            a))))

(defcheck solution-8a76608e
  (fn perfect-numbers? [number]
    (loop [n 1, pn (* (bit-shift-left 1 n) (dec (bit-shift-left 2 n)))]
      (cond
        (= number pn) true
        (< number pn) false
        :else (recur (inc n) (* (bit-shift-left 2 n) (dec (bit-shift-left 4 n))))))))

(defcheck solution-8aa3bbbe
  (fn perfect? [x]
    (= x (apply +
           ((fn divisors [n]
              (for [divisor (range 1 (inc (int (/ n 2))))
                    :when (zero? (mod n divisor))]
                divisor
                )
              ) x)))
    ))

(defcheck solution-8b41a298
  (fn [n]
    (->>
      (/ n 2)
      int
      inc
      (range 1)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n)
      )))

(defcheck solution-8b62b37
  (fn [n] (->>
            (range 1 (/ (inc n) 2))
            (keep #(if (zero? (mod n %)) %))
            (reduce +)
            (= n)
            )))

(defcheck solution-8b9489c4
  (fn [n]
    (= (reduce + (filter #(= (* % (int (/ n %))) n) (range 1 n))) n)))

(defcheck solution-8c3b5311
  (fn[n]
    (let
     [g (last
          (for [ x (range n)
                :let [y (reduce + (range x))]
                :while (<= y n)] y))]
      (= g n))))

(defcheck solution-8c96882c
  (fn perfect? [n]
    (->> (range 1 (inc (/ n 2)))
      (filter (comp zero? (partial mod n)))
      (apply +) (= n))))

(defcheck solution-8ca52cef
  (fn perfect? [num]
    (let [divisors (fn [n]
                     (filter #(= (rem n %) 0)
                       (range 1 (inc (/ n 2)))))]
      (= num (apply + (divisors num))))))

(defcheck solution-8ce91238
  #(= % (loop [r 1,c 2] (if (<= c (/ % 2))
                          (if (zero? (rem % c)) (recur (+ r c) (inc c))
                                                (recur r (inc c))) r))))

(defcheck solution-8d1ce96e
  #(= % (apply + (filter (fn [x] (= 0 (mod % x))) (range 1 %)))))

(defcheck solution-8d996a69
  #(boolean (some #{%} (for [p [2 3 5 7 13 17 19 31]] (* (bit-shift-left 1 (dec p)) (dec (bit-shift-left 1 p)))))))

(defcheck solution-8d9cbd90
  (fn [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 (/ (+ 1 n) 2)))))))

(defcheck solution-8e0805aa
  (fn perfect [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-8effd990
  (fn [n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-8f318702
  (fn perfect? [n]
    (letfn [
            (divisors [n] (filter #(= 0 (rem n %)) (range 1 (+ 1 (/ n 2)))))
            ]
      (= n (reduce + (divisors n))))))

(defcheck solution-8f4ff127
  (fn [n] (->> (range 1 n)
            (filter (comp zero? (partial rem n)))
            (apply +)
            (= n))))

(defcheck solution-8f5e13f0
  (fn [n]
    (= n (apply + (filter #(zero? (rem n %))
                    (range 1 (inc (/ n 2))))))))

(defcheck solution-8fafc4b3
  (fn perfect? [n]
    (let [divisors
          (into #{1}
            (mapcat #(if (zero? (rem n %))
                       (list % (/ n %))
                       '())
              (range 2 (inc (int (Math/sqrt n))))))]
      (= n (apply + divisors)))))

(defcheck solution-8fd34e4a
  #(= %
     (reduce + (filter (fn [n] (zero? (mod % n))) (range 1 %)))))

(defcheck solution-8fe686f6
  (fn [x] (= x
            (reduce #(if (zero? (mod x %2)) (+ % %2) %)
              (range 1 (inc (/ x 2)))))))

(defcheck solution-900155ad
  (fn perfect [x]  (if (= x (reduce + (rest (filter #(= 0 (mod % 1)) (map #(/ x %) (range 1 (inc x))))))) true false)))

(defcheck solution-90221109
  (fn [n]
    (= (apply + (filter #(zero? (mod n %)) (range 1 n))) n)))

(defcheck solution-905b6336
  (fn [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-91769478
  (fn [x]
    (= x (reduce +
           (filter #(zero? (mod x %))
             (range 1 x))))))

(defcheck solution-9217811c
  (fn [n] (let [evenDiv? (fn [a b] (== (quot a b) (/ a b)))
                divisors
                         (fn [v]
                           (loop [curr 2 most (int (Math/sqrt v)) root (Math/sqrt v) result [1]]
                             (if (> curr most)
                               result
                               (if (and (= curr most) (== root most))
                                 (if (evenDiv? v curr)
                                   (conj result curr)
                                   result
                                   )
                                 (if (evenDiv? v curr)
                                   (recur (inc curr) most root (conj result curr (quot v curr)))
                                   (recur (inc curr) most root result)
                                   )
                                 )
                               )
                             )
                           )
                ]
            (= (apply + (divisors n)) n)
            )
    ))

(defcheck solution-923851ee
  (fn perfect-number? [n]
    (letfn [(factors [n] (factors- n 2 [1]))
            (factors- [n p res]
              (cond (or (= n 1) (= n p)) res
                    (zero? (mod n p)) (recur n (inc p) (conj res p))
                    :else (recur n (inc p) res)))]
      (= n (reduce + (factors n))))))

(defcheck solution-927db6e
  (fn perfect [n]
    (letfn [(divisors [n]
              (let [candidates (range 1 n)]
                (filter #(and (== 0 (mod n %)) (not (== n %))) candidates)))]
      (== n
        (reduce + (divisors n))))))

(defcheck solution-928bce23
  (fn [n] (= n (reduce +
                 (filter #(zero? (mod n %))
                   (range 1 (inc (/ n 2))))))))

(defcheck solution-92e84e1
  (fn [n] (= (reduce + (filter #(zero? (mod n %)) (range 1 n))) n)))

(defcheck solution-93076e96
  (fn [v] (= v (apply + (filter #(= 0 (rem v %)) (range 1 v))))))

(defcheck solution-9360f014
  (fn
    [num]
    (loop [iterator 1 divisors []]
      (if (< iterator (+ (/ num 2) 1))
        (if (= (rem num iterator) 0)
          (recur (inc iterator) (conj divisors iterator))
          (recur (inc iterator) divisors)
          )
        (if (= (apply + divisors) num)
          true
          false)
        )
      )
    ))

(defcheck solution-93c871cf
  (fn [n] (let [s (filter #(zero? (rem n %)) (range 1 (inc (/ n 2))))] (= (apply + s) n))))

(defcheck solution-93e30f6f
  (fn [x]
    (->> (range 2 (inc (quot x 2)))
      (filter #(= 0 (rem x %)))
      (reduce + 1)
      (= x))))

(defcheck solution-9407bfbc
  (fn [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-943cd84a
  (fn [n]
    (= n (reduce + (filter #(zero? (rem n %)) (range 1 (dec n)))))))

(defcheck solution-9459179b
  (fn [n]
    (let [divisors (for [i     (range 1 (inc (/ n 2)))
                         :when (zero? (rem n i))]
                     i)]
      (= n (reduce + divisors)))))

(defcheck solution-94c2d1d8
  (fn perfect? [n]
    (= n
      (apply +
        (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-94dc0883
  (fn [n] (== n (reduce + (filter #(== 0 (mod n %)) (range 1 n))))))

(defcheck solution-94fab70c
  (fn [num]
    (let [max_n (inc (/ num 2)) ]
      (= num (apply + (for [n (range 1 max_n) :when (zero? (rem num n)) ] n)))
      )
    ))

(defcheck solution-95006b34
  (fn perfect-number? [x]
    (= x (reduce + (for [i (range 1 x) :when (= 0 (mod x i))] i)))))

(defcheck solution-95257865
  (fn [x]
    (= x (apply + (filter #(= 0 (mod x %))
                    (range 1 x))))))

(defcheck solution-958d5e89
  (fn [x]
    (= x (reduce + (filter #(= (rem x %) 0) (range 1 x))))))

(defcheck solution-95c6be98
  (fn [x]
    (= x
      (reduce +
        (filter #(zero? (mod x %))
          (range 1 (inc (/ x 2))))))))

(defcheck solution-9676b594
  #(= % (apply + (filter (fn [n] (= (mod % n) 0)) (range 1 %)))))

(defcheck solution-9718a1d1
  (fn perfect? [n]
    (== n
      (apply +
        (filter #(zero? (mod n %))
          (range 1 n))))))

(defcheck solution-9762a3a7
  (fn [n]
    (let [r (range 1 (inc (quot n 2)))
          v (filter #(= 0 (mod n %)) r)]
      (= n (reduce + v)))))

(defcheck solution-976e5242
  (fn ddd [m]
    (= m
      (reduce +
        ((fn divs [n]
           (filter
             (fn [i]
               (zero? (mod n i)))
             (range 1 n))) m)))))

(defcheck solution-97953241
  (fn [n] (= (apply + (filter #(zero? (mod n %)) (range 1 n))) n)))

(defcheck solution-979c843a
  (fn [n]
    (= n
      (loop [i 1 s 0]
        (if (= i n)
          s
          (recur (inc i) (+ s (if (= 0 (mod n i)) i 0))))))))

(defcheck solution-97c37fc2
  (fn [n]
    (let [possible (range 2 (Math/sqrt n))
          divs (filter #(= 0 (mod n %)) possible)
          sum (+ 1 (reduce #(+ % %2 (/ n %2)) 0 divs))]
      (= sum n))))

(defcheck solution-97e41157
  (fn[n]
    (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-9827afd7
  (fn [n]
    (= n (reduce + 0
           (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-984bab0e
  (fn [x] (= x (reduce + (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-984f407f
  (fn [z] (= z
            (reduce #(if (= 0 (mod z %2)) (+ %2 %) %)
              0 (range 1 z)))))

(defcheck solution-98cff5d7
  (fn [num]
    (let [div-by-num? (fn[div]
                        (if (= 0 (rem num div))
                          true false))
          fac (fn [num] (range 1 num))]
      (if (= num
            (reduce + (filter #(div-by-num? %) (fac num))))
        true
        false))))

(defcheck solution-98dfdc38
  #(= %1 (apply + (filter (fn [x] (= 0 (mod %1 x))) (range 1 %1)))))

(defcheck solution-9922f7bc
  (fn [n]
    (= n (->> (range 1 n)
           (filter #(zero? (mod n %)))
           (apply +)))))

(defcheck solution-9926513f
  (fn [n]
    (= (reduce + (filter #(zero? (mod n %)) (range 1 n))) n)
    ))

(defcheck solution-993cdbc9
  (fn [n]
    (= (reduce + (filter #(= (mod n %) 0) (range 1 n))) n)))

(defcheck solution-9a751c61
  #(let [d (filter (fn [x] (zero? (mod % x)))
             (range 1 %))]
     (= (reduce + d) %)))

(defcheck solution-9a7ff44c
  (fn isperfect? [n] (letfn [
                             (divisors [x] (filter #(zero? (mod x %)) (range 1 (dec x))))]
                       (= n (apply + (divisors n))))))

(defcheck solution-9aa084ba
  (fn perfect-number?
    [n]
    (= n (reduce
           +
           (filter
             (fn [divisor]
               (= 0 (mod n divisor)))
             (range 1 n))))))

(defcheck solution-9b40af03
  (fn [n] (= n (apply + (filter (fn [i] (= 0 (rem n i))) (range 1 n))))))

(defcheck solution-9b59de07
  (fn [n]
    (->>
      (for [d (range 1 n) :when (= 0 (mod n d))] d)
      (reduce +)
      (= n))))

(defcheck solution-9bc89f06
  (fn [x]
    (= x (->> (range 1 x) (filter #(zero? (rem x %))) (reduce +)))))

(defcheck solution-9bcb0bc8
  (fn [n]
    (let
     [divisors (filter #(zero? (mod n %)) (range 1 n))]
      (= (apply + divisors) n))))

(defcheck solution-9c0e4779
  (fn [n] (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-9c50cff
  (fn [x] (= x (reduce + (map #(/ x %) (filter #(= % (int %)) (map #(/ x %) (range 1 x))))))))

(defcheck solution-9c6aa7cb
  (fn perfect? [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 (+ 1 (/ n 2))))))))

(defcheck solution-9d5e27d9
  (fn [n]
    (= n (apply +
           (filter #(= 0 (mod n %))
             (range 1 n))))))

(defcheck solution-9d7ab15e
  (fn [n] (= (reduce + (remove #(< 0 (mod n %)) (range 1 n))) n)))

(defcheck solution-9d9109d2
  (fn [num]
    (let [divisors (fn [num]
                     (for [x (range 1 (inc (/ num 2)))
                           :when (= (mod num x) 0)]
                       x))
          divisor-sum (fn [num]
                        (reduce + (divisors num)))]
      (= (divisor-sum num) num))))

(defcheck solution-9e123e4d
  (letfn [(divisors [n]
            (filter #(zero? (mod n %)) (range 1 n)))]
    (fn [n] (= n (reduce + (divisors n))))))

(defcheck solution-9eb1e81d
  (fn pn? [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-9ee8604
  (fn p080 [n]
    (letfn [(facts
              ([n] (facts [1] n 2 n))
              ([ls n b e] (if (>= b e) ls
                                       (if (= 0 (mod n b))
                                         (facts (list* b (/ n b) ls) n (inc b) (/ n b))
                                         (facts ls n (inc b) e)))))]
      (let [fs (filter #(not (= % n)) (facts n)) s (apply + fs)] (= s n)))))

(defcheck solution-9fcf073d
  (fn [num] (= num (apply + (filter #(= 0 (rem num %)) (range 1 num))))))

(defcheck solution-9ff020d4
  (fn [x] (= x (apply + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-a06f4ae4
  #(= % (apply + (filter (fn [a] (zero? (rem % a))) (range 1 %)))))

(defcheck solution-a07f3769
  (fn [n] (->> (range 1 n) (filter  #(= 0 (mod n  % ))) (reductions +) (drop-while  #(not= n %)) count (= 1))))

(defcheck solution-a0bda593
  (fn [n] (= (apply +(filter #(zero? (mod n %)) (range 1 n))) n)))

(defcheck solution-a13696cb
  (fn [n]
    (= n
      (apply + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-a1428069
  (fn
    [n]
    (= n (apply +
           (filter
             #(= 0 (mod n %))
             (range 1 n))))))

(defcheck solution-a16e7b89
  (fn [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-a17b2350
  (fn perfect [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))
    ))

(defcheck solution-a1fe34f
  (fn [x]
    (letfn [(divisors [x]
              (filter #(= 0 (mod x %)) (range 1 x)))]
      (= x (apply + (divisors x))))))

(defcheck solution-a1ff63ef
  (fn [n]
    (let [divs (for [x (range 1 (inc (/ n 2))) :when (zero? (mod n x))] x)]
      (= (apply + divs) n))))

(defcheck solution-a2181f11
  (fn [n] (let [divisors (filter #(= 0 (rem n %)) (range 1 n))]
            (if (= (reduce + divisors) n)
              true
              false))))

(defcheck solution-a252cac8
  (fn [n]
    (let [divisors (fn[m] (filter (fn[k] (zero? (mod  m k)))(range 1 m))   )
          sigma (fn [m] (reduce + (divisors m)))	]
      (= n (sigma n))
      )
    ))

(defcheck solution-a28d9519
  (fn [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 (inc (int (/ n 2)))))))))

(defcheck solution-a2f3f07d
  (fn [n]
    (letfn [(divisors [n]
              (filter #(zero? (mod n %)) (range 1 n)))]
      (= (reduce + (divisors n)) n))))

(defcheck solution-a302e6e4
  (fn [x]
    (= x
      (apply + (for [y (range 1 x)
                     :when (zero? (mod x y))]
                 y)))))

(defcheck solution-a3624d14
  (fn [num]
    (loop [result [] index 1]
      (if (= index num)
        (if (= (reduce + result) num)
          true
          false
          )
        (if (= 0 (mod num index))
          (recur (conj result index) (inc index))
          (recur result (inc index))
          )
        )
      )
    ))

(defcheck solution-a368063b
  (fn perfect-number? [x]
    (loop [i 1
           acc 0]
      (if (> i (/ x 2))
        (= acc x)
        (recur (inc i)  (if (zero? (mod x i)) (+ acc i) acc))))))

(defcheck solution-a3735b02
  (fn pnum? [n]
    (let [range (take-while #(< % n) (iterate inc 1))
          divs  (reduce (fn [acc v] (if (= 0 (mod n v)) (conj acc v) acc)) '() range)]
      (= n (apply + divs))
      )
    ))

(defcheck solution-a3bd9fb4
  (fn [n]
    (letfn [(divisors
              [n]
              (filter #(zero? (mod n %)) (range 1  n)))]
      (= (reduce + (divisors n)) n))))

(defcheck solution-a3cc9ed7
  (fn [n]
    (let [divs (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (reduce + divs)))))

(defcheck solution-a4994c6f
  (fn[number]
    (let [divisors (fn[n]
                     (loop [i 2 divs [1]]
                       (if (> (* i 2) n)
                         divs
                         (if (zero? (rem n i))
                           (recur (inc i) (conj divs i))
                           (recur (inc i) divs)))))]
      (= (reduce + (divisors number)) number))))

(defcheck solution-a4c463af
  (fn [x]
    (= (reduce + (filter #(zero? (mod x %)) (range 1 x)))
      x)))

(defcheck solution-a4d82ad4
  (fn [n] (= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-a5449c68
  #(= %
     (apply +
       (map (fn [a] (if (= 0 (mod % a)) a 0))
         (range 1 %)))))

(defcheck solution-a61a5bf1
  #(= % (apply + (filter (fn [x] (zero? (mod % x))) (range 1 %)))))

(defcheck solution-a61d3a91
  (fn [n] (= n (reduce + (filter (fn [i] (= (rem n i) 0)) (range 1 n))))))

(defcheck solution-a61e397a
  (fn [n]
    (let [divisors  (->> (range 1 (inc (quot n 2)))
                      (filter #(zero? (rem n %))))]
      (= (apply + divisors) n))))

(defcheck solution-a6540795
  (fn perfect? [n]
    (= n (reduce + (for [x (range 1 n) :when (zero? (mod n x))] x)))))

(defcheck solution-a6b622c9
  (fn [n]	(= n (apply + (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-a6d0333c
  (fn perfect? [number]
    (let [h (+ 1 (/ number 2))]
      (= number (reduce + (filter #(zero? (rem number %)) (range 1 h))))
      )
    ))

(defcheck solution-a6d1d83c
  (fn per? [n]
    (let [divisors (filter #(zero? (rem n %)) (range 1 n))]
      (= n (reduce + divisors)))))

(defcheck solution-a753cbce
  (fn [x] (->> (filter #(zero? (rem x %)) (range 1 x))
            (apply +)
            (= x))))

(defcheck solution-a76129e9
  (fn [a] (= (reduce +(loop [x 1
                             a a
                             res '()]
                        (if (= x a)
                          res
                          (if (= 0(mod a x))
                            (recur (inc x) a (conj res x))
                            (recur (inc x) a res))
                          )
                        )) a) ))

(defcheck solution-a79fb680
  (fn [n]
    (let [d (filter #(= 0 (rem n %)) (range 1 n))]
      (= n (reduce + d)))))

(defcheck solution-a7a20fbb
  (fn [n]
    (let [divisors (take-while #(< % n) (filter #(= 0 (mod n %)) (drop 1 (range))))]
      (= n (apply + divisors)))))

(defcheck solution-a7a50d79
  (fn [x]
    (= x (apply + (filter  #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-a7cbc0e
  (fn [n]
    (= n (apply + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-a8052674
  (fn perfect? [number]
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
          factors (rest (factors-desc number))]
      (= (reduce + factors) number))))

(defcheck solution-a8256a27
  (fn perfect? [n] (= n (reduce + ((fn divisorsX [n] (filter #(zero? (mod n %)) (range 1 (+ (/ n 2) 1)))) n)))))

(defcheck solution-a841ea4c
  (fn [x]
    (let [divisor? (fn [y] (zero? (clojure.core/rem x y)))
          divisors (filter divisor? (range 1 (inc (/ x 2))))]
      (= x (reduce + divisors)))))

(defcheck solution-a86d1b73
  (fn perfect? [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 (+ 1 (/ n 2))))))))

(defcheck solution-a8b6c82c
  (fn pf [n]
    (= n
      (reduce + (->> (range 1 n)
                  (filter #(zero? (rem n %))))))))

(defcheck solution-a8b6cad
  (fn [x] (= x (reduce #(if (= (mod x %2) 0) (+ % %2) %) (range 1 x)))))

(defcheck solution-a9579371
  (fn perfect? [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-aa70db77
  (fn [n] (= n (apply + (reduce #(if (= (mod n %2) 0) (cons %2 %1) %1) () (range 1 n))))))

(defcheck solution-aab8cc4
  (fn perfect? [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-aacc4de4
  (fn [n] (= n (reduce #(if (= 0 (mod n %2)) (+ %1 %2) %1) 0 (range 1 n)))))

(defcheck solution-aae61998
  (fn perfect-number
    [n]
    (loop [s 0
           c 1]
      (if (= c n)
        (= s n)
        (if (zero? (rem n c))
          (recur (+ s c) (inc c))
          (recur s (inc c)))))))

(defcheck solution-ab07814c
  (fn [n]
    (= (reduce + (filter #(= (rem n %) 0) (range 1 n))) n)))

(defcheck solution-abb08c6
  (fn [x]
    (= x
      (reduce +
        (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-abb25589
  #(= % (reduce (fn [s k] ({0 (+ s k)} (rem % k) s)) (range %))))

(defcheck solution-abb931d3
  (fn [num] (= num (reduce + (filter #(zero? (mod num %)) (map inc (range (/ num 2))))))))

(defcheck solution-abd7761b
  #(= % (apply +
          (filter (fn [n] (= 0 (rem %  n)))
            (range 1 %)))))

(defcheck solution-abfd425b
  (fn perfect? [x] (comment "again with the brute force")
    (letfn [(divisors [n] (filter #(= 0 (mod n %)) (range 1 n)))]
      (= x (reduce + (divisors x))))))

(defcheck solution-ac85d133
  (fn d[x]
    (= x (apply + (reduce #(if (= (rem x %2) 0) (conj % %2) %) [] (range 1 x))))))

(defcheck solution-acc7e1d6
  (fn [num]
    (=
      num
      (apply
        +
        ((fn get-factors [try factors]
           (if (or (= try num)
                   (contains? factors try))
             factors
             (recur (inc try)
               (if (= 0 (mod num try))
                 (conj factors try (/ num try))
                 factors))
             )
           )
         2 #{1}
         )
        )
      )
    ))

(defcheck solution-ad0a0809
  #(= %
     (apply +
       (for [x (range 1 %) :when (= 0 (rem % x))] x))))

(defcheck solution-ad0b9558
  (fn perfect [n]
    (let [factors (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))
          aliquotSum (reduce + factors)]
      (cond
        (= aliquotSum n) true  ;:perfect
        (< aliquotSum n) false ;:deficient
        (> aliquotSum n) false ;:abundant
        ))))

(defcheck solution-ad0bae69
  (fn [n]
    (->> n
      Math/sqrt
      int
      range
      (map inc)
      (filter (fn [d] (zero? (rem n d))) )
      (map (fn [v] [v (quot n v)]))
      rest
      flatten
      (into #{})
      (apply +)
      inc
      (= n))))

(defcheck solution-ad2cbd6c
  (fn [n]
    (letfn [(divisors [n]
              (filter #(zero? (rem n %)) (rest (range (inc (quot n 2))))))]
      (= (apply + (divisors n)) n))))

(defcheck solution-ad640f4c
  (fn perfect? [n]
    (= n
      (->> (range 1 n)
        (filter #(zero? (rem n %)))
        (apply +)))))

(defcheck solution-ad978ab6
  (fn [n]
    (->> (range 1 n)
      (filter #(zero? (rem n %)))
      (reduce +)
      (= n))))

(defcheck solution-ae523435
  (fn perfect? [n]
    (= n
      (reduce +
        (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-ae70945a
  (fn perfect?
    [n]
    (= n (reduce + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-ae8ebb34
  (fn [n] (= n (reduce + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-ae97bdc8
  (fn [n]
    (let [divisors (filter #(= (mod n %) 0)
                     (range 1 (-> n Math/sqrt int inc)))]
      (= n (apply + (butlast (sort (mapcat #(vector % (/ n %))
                                     divisors))))))))

(defcheck solution-afb79135
  (fn perfect? [n] (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-afb86958
  (fn [x]
    (= x (apply + (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-afc8e964
  (fn[x]
    (= x (reduce + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-afeefa50
  (fn [n]
    (= n
      (apply +
        (filter (fn divisor? [k]
                  (= 0 (rem n k)))
          (range 1 (- n 1)))))))

(defcheck solution-aff5e2e9
  (fn is-perfect?
    [n]
    (loop [divisors #{}
           d 2]
      (cond
        (> d (Math/sqrt n))
        (= (apply + 1 divisors) n)

        (= 0 (rem n d))
        (recur (conj divisors d (quot n d)) (inc d))

        :else
        (recur divisors (inc d))))))

(defcheck solution-b07742b0
  (fn [x]
    (= x (reduce #(+ %1 (if (zero? (mod x %2)) %2 0)) 0 (range 1 x)))))

(defcheck solution-b0b7de7c
  (fn [n]
    (let [factors (filter #(zero? (mod n %))(range 1 n))]
      (= n (reduce + factors)))))

(defcheck solution-b0cb879d
  (fn [n]
    (= n
      (apply + (filter #(zero? (mod n %))
                 (range 1 (+ 1 (/ n 2))))))))

(defcheck solution-b11caffb
  (fn [n]
    (= n (reduce + ((fn divisors [n]
                      (filter #(zero? (mod n %)) (range 1 n))) n)))))

(defcheck solution-b1231df9
  (fn [n]
    (= n
      (apply + (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-b1446d37
  (fn perfect-numbers' [x]
    (->> x
      (range 1)
      (filter (comp integer? (partial / x)))
      (reduce +)
      (= x))))

(defcheck solution-b14f649e
  (fn [x] (= x (reduce + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-b1b96bf5
  (fn  isperfect?[number]
    (letfn [(is-factor? [divisor]
              (zero? (mod number divisor)))]
      (= (reduce + (filter is-factor? (range 1  number))) number))))

(defcheck solution-b1f8b5ce
  (fn perfect?
    [n]
    (let [divisors (fn [x] (filter #(zero? (rem x %))
                             (range 1 x)))]
      (= (apply + (divisors n)) n))))

(defcheck solution-b28a92a4
  #(= % (apply + (filter (fn [x] (zero? (rem % x))) (range 1 %)))))

(defcheck solution-b2f0af3e
  (fn [n]
    (loop [ans 0 tmp 1]
      (if (= tmp n)
        (if (= n ans)
          true
          false)
        (if (= 0 (mod n tmp))
          (recur (+ ans tmp) (inc tmp))
          (recur ans (inc tmp)))))))

(defcheck solution-b3a7a8ab
  (fn [x]
    (let [divisors (filter
                     #(= (mod x %) 0)
                     (range 1 (inc (int (/ x 2)))))]
      (= x (apply + divisors)))))

(defcheck solution-b3e406ef
  (fn [num]
    (= num (reduce + 0 (filter (fn [n] (= 0 (mod num n))) (range 1 (- num 1)))))
    ))

(defcheck solution-b4241dba
  (fn [n] (= n (reduce #(+ %1 (if (= 0 (rem n %2)) %2 0)) 0 (range 1 (- n 1))))))

(defcheck solution-b4597708
  (fn [x]
    (= x (apply + (filter #(zero? (mod x %)) (range 1 (+ 1 (/ x 2))))))
    ))

(defcheck solution-b4a30200
  (fn perf [n]
    (= n (apply +
           (filter #(zero? (mod n %))
             (rest  (range (inc (/ n 2)))))))))

(defcheck solution-b4b997c5
  (fn [x] (= (apply + (filter #(= (rem x %) 0) (map inc (range (/ x 2))))) x)))

(defcheck solution-b4e924b3
  (fn [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-b66f8a80
  (fn [n]
    (letfn [(factors-of [n]
              (filter #(= 0 (mod n %)) (range 1 n)))]
      (= n (apply + (factors-of n))))))

(defcheck solution-b6c4e8f6
  #(= %
     (apply + (for [i (range 1 (inc (/ % 2)))
                    :when (= 0 (mod % i))]
                i))))

(defcheck solution-b6c8bf8c
  (fn [n] (= n (reduce + (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-b6e64055
  #(= (reduce + (for [x (range 1 %) :when (= (mod % x) 0)] x) ) % ))

(defcheck solution-b70b1dd0
  (fn [num]
    (let [divs (->> (/ num 2)
                 range
                 (map inc)
                 (filter #(zero? (mod num %))))]
      (= num (reduce + divs)))))

(defcheck solution-b78babd7
  #(letfn [(f [x]
             (loop [ret [1] n 2]
               (if (> n (/ x 2))
                 ret
                 (recur (if (= 0 (rem x n))
                          (conj ret n)
                          ret)
                   (inc n)))))]
     (= (apply + (distinct (f %))) %)))

(defcheck solution-b7bea61e
  #(= % (reduce + (loop [stock [1] try 2]
                    (if (< try %)
                      (if (zero? (mod % try))
                        (recur (conj stock try) (inc try))
                        (recur stock  (inc try))
                        )
                      stock)))))

(defcheck solution-ba3dbe3c
  #(= % (reduce (fn[a b](if (= 0 (rem % b)) (+ a b) a )) 0 (range 1 %))))

(defcheck solution-ba3f7739
  #(= % (apply + (filter (fn [x] (= 0 (rem % x))) (range 1 %)))))

(defcheck solution-bb09a755
  (fn [num]
    (= (apply + (filter #(zero? (mod num %)) (range 1 (inc (int (/ num 2))))))
      num)
    ))

(defcheck solution-bbd12b70
  (fn [n]
    (= n (reduce + (filter (fn[x] (= (/ n x) (int (/ n x))))
                     (range 1 n)))
      )))

(defcheck solution-bc3c2d90
  (fn f [n]
    (= n (apply
           +
           (filter
             #(zero? (rem n %))
             (range 1 n))))))

(defcheck solution-bcb2614a
  (fn [n]
    (let [get-divisors
          (fn [x] (remove #(= x %) (reduce
                                     #(if (= 0 (mod x %2)) (conj %1 %2 (/ x %2)) %1)
                                     #{}
                                     (range 1 (inc (Math/sqrt x))))))]
      (= n (apply + (get-divisors n))))))

(defcheck solution-bcbb395d
  (fn [x]
    (let [dvs (filter #(zero? (mod x %)) (range 1 x))]
      (= x (apply + dvs)))))

(defcheck solution-bd3602e0
  (fn isperfect? [n]
    (let [round?     (fn round? [n] (== n (int n)))
          divisorof? (fn [n m] (round? (/ n m)))
          divisors   (fn divisors [n] (filter #(divisorof? n %) (range 1 n)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-bd77b523
  (fn [x] (= (reduce + (filter #(zero? (rem x %)) (range 1 x))) x)))

(defcheck solution-bd8a52b4
  #(->> (range 1 (- % 1))
     (filter (fn [x] (= 0 (mod % x))))
     (reduce +)
     (= %)
     ))

(defcheck solution-bd93296d
  (fn [n]
    (= n
      (reduce +
        (filter #(zero? (mod n %))
          (range 1 n))))))

(defcheck solution-bda7535c
  (fn [x] (= x (reduce +(filter #(= (rem x %) 0) (range 1 x))))))

(defcheck solution-be2a478c
  (fn [n]
    (= n (apply +
           (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-be791e9f
  (fn [n]
    (let [factors (filter #(zero? (rem n %)) (range 1 n))]
      (= n (reduce + factors)))))

(defcheck solution-be7adcba
  (fn [p] (= (reduce +
               (filter #(zero? (mod p %))
                 (range 1 p)))
            p)))

(defcheck solution-bedeebad
  (fn perfect? [n]
    (let [divisors
          (into []
            (filter #(zero? (mod n %)) (range 1 n) )) ]
      (= n (reduce + divisors)) )))

(defcheck solution-bf6b8470
  (fn [n]
    (= n (/ (apply + (flatten(loop [cur 1 r1 '() r2 '()]
                               (if (or (some #(= cur %) r2) (> cur n)) [r1 r2]
                                                                       (if (= 0 (mod n cur))
                                                                         (recur (inc cur) (conj r1 cur) (conj r2 (quot n cur)))
                                                                         (recur (inc cur) r1 r2)
                                                                         )
                                                                       )
                               )
                       )) 2))
    ))

(defcheck solution-bfc76e5c
  (fn perfect? [n]
    (= (reduce + (filter #(= 0 (mod n %)) (range 1 n)))
      n)))

(defcheck solution-bfd4eb25
  #(= % (apply + (for[d (range 1 %) :when (= 0 (mod % d))] d))))

(defcheck solution-c032130f
  #(= (reduce + ((fn divisors[nmb]
                   (for [n  (range 1 (dec nmb)) :when (= (mod nmb n) 0)]
                     n
                     )
                   ) %)) %))

(defcheck solution-c0bfd459
  (fn [x]
    (->> (range 1 x)
      (filter #(zero? (mod x %)))
      (reduce +)
      (= x))))

(defcheck solution-c0ff38c4
  (fn perfect? [x]
    (= x (apply + ;sum is x
           (filter #(= 0 (rem x %));find divisor
             (range 1 x))))))

(defcheck solution-c14968d2
  (fn [n]
    (= n (inc (reduce + (filter #(zero? (mod n %)) (range 2 n)))))))

(defcheck solution-c189e08f
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (reduce + divisors)))))

(defcheck solution-c1fc399f
  (fn complete? [n]
    (letfn [(divisors [n]
              (filter #(= 0 (mod n %)) (range 1 (+ 1 (/ n 2))))) ]
      (= n (reduce + (divisors n))))))

(defcheck solution-c20ba28c
  #(= % (reduce + (filter (comp zero? (partial mod %)) (range 1 (inc (quot % 2)))))))

(defcheck solution-c242e961
  #(= (apply + (for [x (range 1 %) :when (zero? (mod % x))] x)) %))

(defcheck solution-c2dbac53
  (fn [x] (= x (reduce + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-c39ac536
  (fn [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-c3bd9146
  (fn [n]
    (== n (reduce + (filter #(= (rem n %) 0) (range 1 (inc (/ n 2))))))))

(defcheck solution-c3e4f836
  (fn [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-c41d9924
  (fn [n]

    (= (reduce + (filter #(= (mod n %)
                            0)
                   (range 1 n)))
      n)))

(defcheck solution-c459ec
  (fn [n]
    (let [divisors (filter #(= (mod n %) 0) (rest (range n)))]
      (= n (reduce + 0 divisors)))))

(defcheck solution-c4669f2a
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-c46c5e2a
  (fn [n] (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-c4bfd131
  #(= %
     (apply +
       (for [i (range 1 %) :when (= 0 (mod % i))]
         i))))

(defcheck solution-c5bdd948
  #(= (apply + (%1 %2)) %2) (fn f
                              ([x] (f [1] 2 x))
                              ([init n x]
                               (if (= x n)
                                 init
                                 (if (zero? (rem x n))
                                   (recur `[~@init ~n] (inc n) x)
                                   (recur init (inc n) x))))))

(defcheck solution-c660147d
  (fn [n] (if (= n
                (reduce +
                  (filter #(if (= (mod n %) 0) true false)
                    (range 1 (inc (quot n 2)))))) true false )))

(defcheck solution-c6bcc06f
  (fn
    [n]
    (=
      n
      (reduce
        +
        (filter
          #(= 0 (rem n %))
          (range 1 (inc (int (Math/floor (/ n 2))))))))))

(defcheck solution-c76490e0
  (fn [n]
    (let [factors (filter #(zero? (mod n %)) (range 1 n))]
      (= n (apply + factors)))))

(defcheck solution-c7c82529
  (fn [x]
    (letfn [(divisors [x]
              (filter #(= 0 (mod x %)) (range 1 x)))]
      (= x (reduce + (divisors x))))))

(defcheck solution-c7e626ac
  (fn [n] (= n (reduce + (cons 1 (mapcat #(list % (/ n %)) (filter #(= 0 (mod n %)) (range 2 (Math/sqrt n)))))))))

(defcheck solution-c81e3634
  (fn perfect [x]
    (let[sumofdivisors (apply + (filter
                                  #(= 0 (mod (/ x %) 1))
                                  (range 1 x)))]
      (= x sumofdivisors))))

(defcheck solution-c881b9c1
  (fn [x] (= x (apply +(filter #(= (mod x %) 0) (range 1 (inc (/ x 2))))))))

(defcheck solution-c8b846b4
  (fn perfect-no
    [n]
    (= n (apply + (map (fn [n1] (if (zero? (rem n n1)) n1 0)) (range 1 n))))))

(defcheck solution-c8bbaef9
  (fn[x]
    (if
     (=
       (reduce
         +
         (filter
           #(= (mod x % ) 0)
           (rest (range x))
           )
         )
       x
       )
      true
      false
      )
    ))

(defcheck solution-c8e506d7
  (fn [x]
    (->> (range 1 x)
      (filter #(zero? (mod x %)))
      (reduce + 0)
      (= x))))

(defcheck solution-c9112915
  #(= (reduce + (for [x (range 1 (inc (/ % 2))) :when (zero? (mod % x))] x)) %))

(defcheck solution-c912aa9
  (fn [n]
    (= n (apply + (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-c92b981b
  #(case % 6 true 496 true 8128 true false))

(defcheck solution-c9628d2a
  (fn [n]
    (let [divisors (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))]
      (= n (apply + divisors)))))

(defcheck solution-ca400cdc
  (fn [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-caa89e58
  (letfn [(sum-of-divisors [n]
            (apply +
              (for [i (range 1 (inc (quot n 2)))
                    :when (zero? (rem n i))] i)))]
    #(= % (sum-of-divisors %))
    ))

(defcheck solution-cacd136d
  (fn [x]
    (let [divisors (->>
                     (range 1 x)
                     (filter #(= 0 (mod x %))) )]
      (= x (reduce + divisors)) )))

(defcheck solution-cad04f8d
  (fn [x]
    (let [divisors (filter  #(zero? (mod x %)) (range 1 x))]
      (= x (apply + divisors)))))

(defcheck solution-cb09e867
  (fn [a]
    (= a (reduce + (filter #(zero? (mod a %)) (range 1 a))))))

(defcheck solution-cb9b3132
  (fn [n]
    (= n
      (->> (range 1 n)
        (filter #(zero? (mod n %)))
        (reduce +)))))

(defcheck solution-cc0c3f07
  (fn z [n]
    (let [f (fn [x] (filter #(zero? (mod x %)) (range 1 x)))]
      (= n (reduce + 0 (f n))))))

(defcheck solution-cc111617
  (fn [n] (= n (apply + (filter #(zero? (rem n %)) (range 1 (max 2 (inc (quot
                                                                          n 2)))))))))

(defcheck solution-cc16d12e
  (fn perfectnumber? [n]
    (=  (apply + (filter #(zero? (mod n %))
                   (range 1  n))) n ) ))

(defcheck solution-cc338186
  (fn [n]
    (let [divisors (fn [n]
                     (cons 1 (filter #(zero? (rem n %))
                               (range 2 (inc (quot n 2))))))]
      (= n (reduce + (divisors n))))))

(defcheck solution-cc567ff8
  (fn [x]
    (let [divs (filter #(= 0 (rem x %)) (range 1 x))]
      (= x (apply + divs)))
    ))

(defcheck solution-cc6067ca
  (fn [x] (= x (apply +
                 (filter #(zero? (mod x %)) (range 1 (inc (/ x 2))))))))

(defcheck solution-cc6258da
  (fn [n] (= n (apply + (for [i (range 1 n) :when (zero? (mod n i))] i)))))

(defcheck solution-ccae7f48
  (fn [x]
    (= x (reduce + 0 (filter #(= (rem x %) 0) (range 1 x))))))

(defcheck solution-ccd733ce
  (fn perfect? [n]
    (let [devisors (filter (fn [x] (zero? (rem n x))) (range 1 (inc (quot n 2))))]
      (= (apply + devisors) n))))

(defcheck solution-cd06fd56
  (fn perfect? [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-cd376a1c
  (fn perfect? [x]
    (let [divisors (filter #(= 0 (mod x %)) (range 1 (inc (/ x 2))))]
      (= x (apply + divisors)))))

(defcheck solution-cdb8d107
  (fn [n] (= n (reduce + (filter #(= 0 (mod n %)) (range 1 (dec n)))))))

(defcheck solution-ce3d5732
  #(= (apply + (filter (fn [y] (= (rem % y) 0)) (range 1 %))) %))

(defcheck solution-ce52bd9c
  (fn is-perf? [n]
    (let [nums (range 1 (+ 1 (/ n 2)))
          divs (filter #(zero? (mod n %)) nums)
          divs-sum (apply + divs)]
      (= n divs-sum))))

(defcheck solution-ceb2a78
  (fn f [n]
    (let [sum
          ((fn f [sum x y]
             (let [sum2 (+ sum x y)
                   x2 (first (filter #(zero? (rem n %)) (range (inc x) (dec y))))]
               (if x2
                 (f sum2 x2 (quot n x2))
                 sum2))) 0 1 n) ]
      (= n (- sum n)))))

(defcheck solution-cf09c525
  (fn [n]
    (= (apply + (filter #(= 0 (mod n %)) (range 1 (/ (inc n) 2)))) n)
    ))

(defcheck solution-cf5179f
  (fn [x]  (let [ divs  (filter #(= 0 (mod x %)) (range 1 x)) ]
             (= (apply + divs) x))))

(defcheck solution-cf9a7040
  #(->>  %
     (range 1 )
     (filter (comp (partial == 0)
                   (partial mod %)) ,)
     (apply + ,)
     (== % ,)))

(defcheck solution-cfe9ad3f
  (fn myf2 [n]
    (->> (filter #(zero? (rem n %)) (range 1 n))
      (apply +)
      (= n))))

(defcheck solution-d078d484
  (fn perfect-num? [n]
    (let [devisors (fn [n]
                     (filter #(= (rem n %) 0) (range 1 n)))]
      (= (reduce + (devisors n))
        n))))

(defcheck solution-d0d50147
  (fn [n]
    (letfn
     [
      (factors [n]
        (filter
          (fn [i]
            (=
              0
              (mod n i)
              )
            )
          (range
            1
            (inc (quot n 2))
            )
          )
        )
      (sum-factors [n]
        (apply + (factors n))
        )
      ]
      (= n (sum-factors n))
      )
    ))

(defcheck solution-d1503b2a
  #(=
     (reduce +
       (filter
         (fn [x] (zero? (mod % x)))
         (range 1 %)))
     %))

(defcheck solution-d18bc442
  (fn me [n]

    (= n
      (apply +
        (filter #(= 0 (rem n %)) (range 1 n))
        )

      )
    ))

(defcheck solution-d2451c2a
  (fn [x]
    (= x
      (apply + (filter #(= 0 (rem x %))
                 (range 1 (inc (quot x 2))))))))

(defcheck solution-d2744609
  (fn perfect? [num]
    (= num (apply + (filter #(= 0 (mod num %)) (range 1 num))))))

(defcheck solution-d28b5eb7
  (fn [n]
    (= n
      (last
        (take-while
          #(<= % n)
          (map
            (fn [nn]
              (let [a (apply * (repeat nn 2))] (* a (dec (* 2 a)))))
            (range)))))))

(defcheck solution-d2ba5841
  (fn [num]
    (= num
      (apply +
        (filter #(= 0 (rem num %)) (range 1 num))))))

(defcheck solution-d2e8eb2f
  (fn [n] (->> (range 1 n) (filter #(zero? (rem n %))) (apply +) (= n))))

(defcheck solution-d34a672d
  (fn [n]
    (= n (reduce + (filter #(zero? (mod n %))
                     (range 1 n))))))

(defcheck solution-d45e32fb
  (fn [n] (let
           [divisors (filter #(= 0 (rem n %)) (range 1 n))]
            (= n (reduce + divisors)))))

(defcheck solution-d461b2cc
  (fn [n]
    (letfn [(divisors [x]
              (filter #(= 0 (rem x %)) (range 1 (dec n))))]
      (= n (reduce + 0 (divisors n))))))

(defcheck solution-d4c6e6fb
  (fn [x]
    (= x (reduce + (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-d4d41a80
  (fn [x] (= (reduce + (filter #(zero? (mod x %)) (range 1 x))) x)))

(defcheck solution-d4eb23dd
  (fn perfect? [n]
    (= n (apply + (filter #(integer? (/ n %)) (range 1 n))))))

(defcheck solution-d50aa781
  (letfn [(divisors [n] (filter #(zero? (mod n %)) (range 1 n)))]
    (fn perfect? [n] (= n (apply + (divisors n))))))

(defcheck solution-d59eae3a
  (fn [n] (= n (apply + (filter #(= 0 (mod n %)) (range 1 (/ (inc n) 2)))))))

(defcheck solution-d5a17e97
  (fn [n]
    (let [divisors (fn [n] (filter #(= 0 (rem n %)) (range 1 (+ 1(/ n 2)))))
          sumOfDs  (fn [n] (reduce + (divisors n)))]
      (= n (sumOfDs n)))))

(defcheck solution-d5b32eb7
  (fn perfect? [n]
    (->> n
      (range 1)
      (filter
        #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-d6021ee0
  (fn [n]
    (let [doubled (* 2 n)
          factors (conj (vec (filter #(zero? ( mod n %)) (range 1 (inc (/ n 2))))) n)]
      (= doubled (reduce + factors)))))

(defcheck solution-d61b4988
  #(loop [acc 1 i 2]
     (if (>= i %)
       (if (= acc %)
         true
         false)
       (if (zero? (rem % i))
         (recur (+ acc i) (inc i))
         (recur acc (inc i))))))

(defcheck solution-d687b505
  (fn n80 [n] (= n (reduce + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-d693a66d
  contains? #{6 496 8128})

(defcheck solution-d7e43f6a
  (fn [x]
    (= x
      (reduce +
        (filter
          #(zero? (mod x %))
          (range 1
            (inc (quot x 2))))))))

(defcheck solution-d82aa03
  (fn  [x]
    (boolean (= (apply + (filter #(= (mod x %) 0) (range 1 (dec x)))) x))))

(defcheck solution-d85c8978
  (fn [n]
    (= n
      (reduce +
        (filter #(zero? (mod n %))
          (range 1 (inc (/ n 2))))))))

(defcheck solution-d8aad0c
  #(= %1 (reduce + (filter (fn [k] (= 0 (rem %1 k))) (range 1 %1)))))

(defcheck solution-d8ffa7de
  (letfn [(divisors [x]
            (->> (range 1 (inc (quot x 2)))
              (filter #(= 0 (rem x %)))))]
    (fn perfect? [x]
      (= x (reduce + (divisors x))))))

(defcheck solution-d9055533
  (fn [num]
    (=
      (some #{num} (for [x [2 3 5 7]] (int (* (Math/pow 2 (dec x)) (dec (Math/pow 2 x))))))
      num)))

(defcheck solution-d92150e5
  (fn [x]
    (letfn [(worker [cur s]
              (if (> cur (quot x 2))
                s
                (recur (inc cur) (if (zero? (rem x cur))
                                   (+ cur s) s))))]
      (= (worker 1 0) x))))

(defcheck solution-d927d51b
  (fn perf? [n]
    (let [divs
          (filter #(= 0 (mod n %)) (range 1 (inc (quot n 2))))]
      (= n (apply + divs)))))

(defcheck solution-d948041c
  (fn [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-d9b35aa4
  (fn [n]
    (let [divisors (filter #(zero? (rem n %)) (range 1 n))]
      (= (apply + divisors) n))))

(defcheck solution-d9c31e90
  (fn [n]
    (->> (range 1 n)
      (filter #(= (mod n %) 0))
      (reduce +)
      (= n))))

(defcheck solution-da00dd79
  (fn perfect-num? [x]
    (->> (range 1 (inc (/ x 2)))
      (filter #(zero? (mod x %)))
      (reduce +)
      (= x))))

(defcheck solution-da538abc
  (fn [x]
    (let [d (fn d [i]
              (set (let [x i] (filter #(= 0 (mod x %)) (range 1 x)))))]
      (= x (reduce + (d x))))))

(defcheck solution-db01e3a2
  (fn [x] (let [divisor? #(zero? (mod x %))
                divisor-sum (reduce + (filter divisor? (range 1 x)))]
            (= x divisor-sum))))

(defcheck solution-db60ce12
  (fn
    [n]
    (letfn [(divisors [n] (filter #(zero? (mod n %)) (range 1 n)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-db6d7b0f
  (fn [x]
    (let
     [n (/ (+ -1
              (Math/sqrt (+ 1
                            (* 8 x))))
          2)]
      (= n (Math/floor n))
      )
    ))

(defcheck solution-dc2359ad
  (fn perfect [x]
    (= x (apply + (filter #(= 0 (rem x %)) (map #(+ 1 %) (range (/ x 2))))))
    ))

(defcheck solution-dc965e42
  (fn [n]
    (->> (range 1 (inc (/ n 2)))
      (filter #(= (/ n %) (int (/ n %))))
      (apply +)
      (= n))))

(defcheck solution-dca308d9
  (fn [n]
    (->> (range (/ n 2))
      (map inc)
      (filter #(zero? (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-dccd720f
  (fn perfect-no?
    [num]
    (->> (range 1 num)
      (filter #(zero? (rem num %)))
      (reduce +)
      (= num))))

(defcheck solution-dd0f9026
  (fn ff[n]
    (= n
      (reduce +
        (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-dd351a8d
  (fn perfect? [n]
    (->> (range 1 n) (filter #(zero? (mod n %))) (reduce +) (= n))))

(defcheck solution-ddf626a4
  (fn perfect? [x]
    (= x (apply + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-de2afc7a
  (fn [x]
    (->> (for [n (range 2 (Math/sqrt x)) :when (zero? (mod x n))] [n (/ x n)]) (apply concat) set (apply +) (= (dec x)))))

(defcheck solution-de42b417
  (fn perfect? [n]
    (let [divisors (filter #(zero? (rem (/ n %) 1)) (range 1 n))]
      (= n (apply + divisors)))))

(defcheck solution-dea9aa67
  (fn perfect [n]
    (let [divisors (fn [n] (filter #(= 0 (rem n %)) (range 1 n)))]
      (= (reduce + (divisors n)) n)
      )
    ))

(defcheck solution-debe7aca
  (fn perfect-number? [n]
    (= n (apply + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-dec1aeca
  (fn perfect [n] (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-ded00ca1
  (fn [n]
    (= n
      (reduce +
        (filter #(zero? (mod n %)) (take (quot n 2) (iterate inc 1)))))))

(defcheck solution-df1d980e
  (fn [x] (= x (apply + (filter #(zero? (mod x %)) (range 1 (+ 1 (/ x 2))))))))

(defcheck solution-df26ff64
  (fn [n]
    (=
      n
      (apply
        +
        (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-df5fa38b
  (fn [n]
    (= n
      (reduce + (filter #(integer? (/ n %)) (range 1 n))))))

(defcheck solution-df938fbb
  (fn [n]
    (let [divisors (fn [n] (filter #(zero? (mod n %)) (range 1 n)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-df9f5c5c
  #(= %
     (apply +
       (filter
         (fn [x] (= 0 (mod % x)))
         (range 1 %)
         )
       )))

(defcheck solution-dfad81a3
  (fn [i] (= i (reduce + (filter #(= 0 (mod i %)) (range 1 i))))))

(defcheck solution-dfd1bb3d
  (fn [n]
    (->> (range 1 n)
      (filter #(= 0 (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-dfe6519b
  (fn [n]
    (->> (for [x (range 1 (inc (/ n 2)))
               :when (= 0 (rem n x))]
           x)
      (apply +)
      (= n))))

(defcheck solution-e0b07d27
  (fn perfect-number? [n]
    (let [find-candidates (fn [n]
                            (let [n2 (if (even? n) n (inc n))
                                  half (/ n2 2)]
                              (take half (iterate inc 1))))
          candidates (find-candidates n)
          find-divisors (fn [n candidates]
                          (filter #(zero? (rem n %)) candidates))
          divisors (find-divisors n candidates)
          perf-num? (fn [n divisors]
                      (= (reduce + divisors) n))

          ]
      (perf-num? n divisors))))

(defcheck solution-e1667368
  (fn [x]
    (= x (reduce + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-e1f6787a
  (fn perfect [n]
    (let [divs (filter #(= 0 (mod n %)) (range 1 (inc (int (/ n 2)))))]
      (if (= n (apply + divs)) true false))))

(defcheck solution-e1faa1f8
  (fn perfect? [n]
    (= n
      (apply + (filter
                 #(= 0 (mod n %))
                 (range 1 (+ 2 ( int (/ n 2)))))))))

(defcheck solution-e23bdc5f
  (fn[n]
    (= n (apply +
           (filter
             #(zero? (mod n %))
             (range 1 n))))))

(defcheck solution-e2b9779b
  (fn [n]
    (->> (range 1 n)
      (filter #(zero? (rem n %)))
      (apply +)
      (= n))))

(defcheck solution-e301d74b
  (fn [n] (= n (apply + (filter #(= (mod n %) 0) (range 1 n))))))

(defcheck solution-e3ab0be2
  (fn [n]
    (let [nums (range 1 n)]
      (boolean (= n (apply + (filter #(= 0 (mod n %)) nums)))))))

(defcheck solution-e4129d0
  (fn isPerfectNumber [x] (= x (apply + (filter #(zero? (rem x %)) (range 1 x))))))

(defcheck solution-e4481bc2
  (fn [n]
    (= n
      (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-e44b26c
  (fn [n]
    (= n (apply + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-e46b1ee0
  (letfn [(divisors [n]
            (let [max-divisor (int (/ n 2))]
              (for [d (range 1 (inc max-divisor))
                    :when (zero? (rem n d))]
                d)))]
    (fn [n]
      (= n (apply + (divisors n))))))

(defcheck solution-e47f67ee
  (fn [n]
    ; to be improved for performance
    (= (apply + (filter #(= (mod n %) 0) (drop 1 (range n)))) n)))

(defcheck solution-e49f094a
  (fn myPerfectNumbers
    [num]
    (= num (reduce + (filter #(zero? (mod num %)) (range 1 num))))))

(defcheck solution-e53afa9
  (fn perfect?--reduce
    [n] {:pre [(integer? n)]}
    (let [divisor? (fn [d] (= 0 (mod n d)))]
      (= n (reduce (fn [acc x] (+ acc (if (divisor? x) x 0)))
             0
             (range 1 n))))))

(defcheck solution-e595ed30
  (fn [n]
    (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
      (= n (reduce + divisors)))))

(defcheck solution-e5be6bf8
  (fn [a]
    (=
      a
      (apply + (filter #(= 0 (mod a %)) (range 1 a))))))

(defcheck solution-e5cb7076
  #(= %
     ( apply +
       (filter (fn [x] (if (zero? (mod % x))
                         true false))
         (range 1 %)))))

(defcheck solution-e6470cbe
  (fn[x]
    (= x
      (reduce +
        (filter #(zero? (mod x %))
          (range 1 (inc (/ x 2))))))))

(defcheck solution-e67c03f9
  (fn perfect-number? [n]
    (let [divisors (filter #(zero? (mod n %)) (range 1 n))]
      (= n (reduce + divisors)))))

(defcheck solution-e6b491fb
  (fn [n]
    (let [sqrt (Math/sqrt n)
          ints (take-while #(<= % sqrt) (iterate inc 2))
          lower-divs (filter #(= 0 (rem n %)) ints)
          divs (concat [1] lower-divs (map #(/ n %) lower-divs))]
      (= n (reduce + divs)))))

(defcheck solution-e6c87e25
  (fn
    [n]
    (let [divides? (fn [n d] (= 0 (mod n d)))
          divisors (filter (partial divides? n) (range 1 (inc (quot n 2))))]
      (= n (apply + divisors)))))

(defcheck solution-e7c07bb9
  (fn perfect? [x] (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-e85d4719
  (fn perf[n]
    (not (nil? (#{6 496 8128} n)))))

(defcheck solution-e87465c9
  (let [is-divisor?
        (fn isdiv [a b] "Is a divisior of b?"
          (zero? (mod b a)))]
    (fn perfect-num
      ([x] (if (= x (perfect-num x (- x 1))) true false))
      ([x y]

       (if (= 1 y)
         1
         (if (is-divisor? y x)
           (+ y (perfect-num x (dec y)))
           (recur x (dec y))))))))

(defcheck solution-e8b23650
  (fn [x]
    (let [divides-x? #(zero? (mod x %))
          divisors-x (fn [s] (filter divides-x? s))
          range-1-to-half-n (range 1 (inc (/ x 2)))]
      (= x (reduce + (divisors-x (range 1 x)))))))

(defcheck solution-e8b90387
  (fn is-perfect-number
    [n]
    (= (apply + (filter #(= (mod n %) 0) (range 1 n))) n)))

(defcheck solution-e8ea121d
  #(= % (apply + (let [s (-> % (/ 2) Math/ceil int)]
                   (loop [i 1 divs []]
                     (if (> i s)
                       divs
                       (recur (inc i) (if (zero? (mod % i))
                                        (conj divs i)
                                        divs))))))))

(defcheck solution-e906bcc3
  (fn [n] (= n (reduce #(if (zero? (mod n %2)) (+ % %2) %) (drop 1 (range n))))))

(defcheck solution-e9454e84
  (fn perfect? [n]
    (= n
      (apply + (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-e99c95d0
  (fn [n]
    (let [d (for [i (range 1 (inc (/ n 2))) :when (zero? (rem n i))] i)]
      (= (reduce + d) n))))

(defcheck solution-e9a3047b
  (fn [n]
    (letfn [(divisors [x]
              (filter #(zero? (mod x %)) (range 1 x)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-eaa88291
  (fn [n]
    (= n (reduce    #(if (= (mod n %2) 0) (+ % %2) %)    1 (range 2 (inc (/ n 2))) ))


    ))

(defcheck solution-eac3ae2c
  (fn [n] (= n (apply + (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-eb1ccfab
  (fn [x]
    (= x
      (apply + (->> x (range 1) (filter #(zero? (mod x %))))))))

(defcheck solution-eb541832
  (fn [x]
    (= x (reduce + (filter #(= 0 (rem x %)) (range 1 x))))))

(defcheck solution-ec5ed364
  (fn [x]  (= x (apply + (filter #(zero? (mod x %)) (range 1 x))))))

(defcheck solution-ec865c13
  (fn [x]
    (= x
      (apply +
        (filter
          #(= 0 (mod x %1))
          (range 1 x)
          )
        )
      )
    ))

(defcheck solution-ed020d4e
  (fn [n] (= n (reduce + (filter #(= 0 (mod n %)) (range 1 (/ (inc n) 2)))))))

(defcheck solution-ed7d262a
  (fn [n]
    (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-edc37bf4
  (fn [x] (= x (apply + (filter #(= 0 (mod x %)) (range 1 (inc (quot x 2))))))))

(defcheck solution-ee0beecf
  (fn perfect-number?
    [n]
    (->>
      (range 1 n)
      (filter #(= 0 (rem n %)))
      ((comp (partial = n) (partial reduce +))))))

(defcheck solution-eee60d53
  (fn [n]
    (= n (reduce + ((fn [n]
                      (loop [a [1]
                             x 2 ]
                        (if (> x (/ n 2))
                          a
                          (if (zero? (mod n x))
                            (recur (conj a x) (inc x) )
                            (recur a (inc x) ))))) n)))))

(defcheck solution-ef224345
  (fn [i]
    (->> i range (drop 1) (filter #(= 0 (rem i %))) (reduce +) (= i)) ))

(defcheck solution-efb0ec70
  (fn [n]

    (let [divisors-sum (->> (range 1 n) (filter #(= 0 (mod n %))) (reduce +)  )]
      (= divisors-sum n) )


    ))

(defcheck solution-efb3252f
  (fn [n]
    (= n (apply +
           (filter #(zero? (rem n %)) (range 1 (inc (quot n 2))))))))

(defcheck solution-efc3181d
  (fn perfect? [n] (contains?
                     #{6,28,496,8128,33550336,8589869056,137438691328,
                       2305843008139952128,
                       2658455991569831744654692615953842176,
                       19156194260823610729479337808430363813099732154816\
                       9216}
                     n)))

(defcheck solution-efea5432
  (fn p80
    [n]
    (->>
      (range 1 n)
      (filter #(zero? (rem n %)))
      (apply +)
      (= n))))

(defcheck solution-f04755cb
  (fn [n] (== n (apply + (filter #(== 0 (rem n %)) (range 1 n))))))

(defcheck solution-f0c456b7
  #(= % (apply + (filter (fn[n] (= 0 (mod % n))) (range 1 %)))))

(defcheck solution-f15eb681
  (fn [n]
    (letfn [(pds [n]
              (filter #(= 0 (mod n %)) (range 1 (inc (/ n 2)))))]
      (= n (apply + (pds n))))))

(defcheck solution-f1bf9078
  (fn [n]
    (->> (iterate (fn [[div left]]
                    (when-let [next-div (some #(when (zero? (mod n %)) %)
                                          (range div (inc (/ n 2))))]
                      [(inc next-div) (- left next-div)])) [1 n])
      (take-while identity)
      last
      second
      zero?)))

(defcheck solution-f26ae45b
  (fn [x]
    ((fn [s i](if (< x s)
                false
                (if (= x s)
                  true
                  (recur (+ i s) (inc i)))))0 0)))

(defcheck solution-f295d885
  (fn perfect?
    [n]

    (let [
          divisors-of (fn divisors-of
                        [n proper]
                        (cond
                          (neg? n) (divisors-of (- n) proper)
                          (< n 2)  (if (true? proper) []  [1])
                          (= n 2)  (if (true? proper) [1] [1 2])
                          :default (let [lim (int (/ n 2))
                                         opt-n (if (true? proper) [] [n]) ]
                                     (concat (filter #(zero? (rem n %)) (range 1 (inc lim))) opt-n))))
          ]

      (and (pos? n) (= n (apply + (divisors-of n true)))))))

(defcheck solution-f29bac2c
  (letfn
   [(divisors [n]
      (for [x (range 1 (inc (/ n 2)))
            :when (zero? (mod n x))]
        x))

    (perfect? [n]
      (= n (reduce + (divisors n))))]

    #(perfect? %)))

(defcheck solution-f2a3b496
  (fn [n]
    (= n (apply + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-f2cb99ee
  (fn perfectN
    [n]
    (=
      (apply +
        (for [x (range 1 n)
              :when (= (rem n x) 0)]
          x))
      n)))

(defcheck solution-f3031ce7
  (fn [x]
    (letfn [(divisors [a]
              (filter #(= 0 (mod a %)) (range 1 (inc (/ a 2)))))]
      (= x (reduce + (divisors x))))))

(defcheck solution-f3226ebf
  #(or (= 6 %)
       (= 496 %)
       (= 8128 %)))

(defcheck solution-f363a0a7
  (fn [x]
    (= x (reduce + (filter #(= 0 (mod x %)) (range 1 x))))))

(defcheck solution-f37c11de
  (fn [n]
    (letfn [(dividers [n]
              (filter #(zero? (rem n %)) (range 1 n)))]
      (= (reduce + (dividers n)) n))))

(defcheck solution-f389becc
  (fn [n]
    (= n
      (apply +
        (filter #(= 0 (rem n %))
          (range 1 n))))))

(defcheck solution-f3bbbb0f
  (fn[n](
          =
          (reduce + (map (fn[k](if(= (/ n k) (int(/ n k))) k 0 )) (range 1 n)))
          n
          )))

(defcheck solution-f3d6688d
  (fn [x]
    (= x (reduce + (filter #(= 0 (rem x %)) (range 1 x) )
           ))))

(defcheck solution-f5182d2c
  (fn perfect-number? [n]
    (letfn [(divides? [a b] (zero? (mod b a)))
            (factors [n] (filter #(divides? % n) (range 1 n)))
            (factor-sum [n] (reduce + (factors n)))]
      (= (factor-sum n) n))))

(defcheck solution-f56e1542
  (fn [n]
    (let [divs (filter #(zero? (mod n %)) (range 1 n))]
      (= n (apply + divs)))))

(defcheck solution-f588623d
  (fn [n]
    (= n (reduce + (filter #(zero? (rem n %)) (range 1 n))))))

(defcheck solution-f5ac5f77
  (fn [n]
    (= (reduce + (reduce (fn [agg i]
                           (if (= 0 (mod n i))
                             (cons i agg)
                             agg)) [] (rest (range n))
                   )) n)))

(defcheck solution-f616374f
  #(= % (reduce + (filter (comp zero? (partial rem %)) (range 1 %)))))

(defcheck solution-f69dfb4c
  (fn [x]
    (= x (apply +
           (->> x
             (range 1)
             (filter #(let [n (/ x %)] (= (int n) n)))
             )))))

(defcheck solution-f71e4165
  (fn perfect [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (reduce +)
      (= n))))

(defcheck solution-f7401541
  (fn [x] (or (= 6 x) (= 496 x) (= 8128 x))))

(defcheck solution-f771deea
  (fn perf[x] (= x (reduce + (filter #(= 0 (mod x %)) (range 1 (inc (/ x 2))))))))

(defcheck solution-f776bc9f
  (fn perfect [n]
    (= n (reduce +
           (filter #(zero? (mod n %))
             (range 1 n))))))

(defcheck solution-f809bf3d
  (fn [n]
    (letfn [(factor [n] (filter #(zero? (mod n %)) (range 1 (inc (/ n 2)))))]
      (= n (apply + (factor n))))))

(defcheck solution-f80d3bf9
  (fn [x]
    (letfn [(divisors [n] (filter #(zero? (mod n %)) (range 1 n)))]
      (= x (reduce + (divisors x))))))

(defcheck solution-f8ce7334
  (fn [x]
    (= x (reduce + 0
           (filter #(zero? (mod x %))
             (range 1 x))))))

(defcheck solution-f8e0b817
  (fn __ [n]
    (->> (range 1 n)
      (filter #(zero? (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-f9234fe8
  (fn perfect [n] (= n (reduce + (clojure.set/difference (reduce #(if(= 0 (mod n %2)) (conj %1 %2 (/ n %2)) %1) #{} (rest (range (inc (Math/sqrt n))))) #{n})))))

(defcheck solution-f963a569
  (fn [n]
    (= n
      (->> (range n)
        (map inc)
        (filter #(zero? (rem n %)))
        butlast
        (reduce +)
        ))))

(defcheck solution-f9cccd6c
  (fn perfect
    [n]
    (let [xs (range 1 (/ (inc n) 2))
          divisors (filter #(zero? (rem n %)) xs)]
      (= n (apply + divisors)))))

(defcheck solution-fa388a13
  (fn perfect? [n]
    (letfn [(sum-divs [n]
              (loop [an []  ns (range 1 (inc (quot n 2)))]
                (if (empty? ns)
                  (reduce + an)
                  (if (zero? (rem n (first ns)))
                    (recur (conj an (first ns)) (rest ns))
                    (recur an (rest ns))))))]
      (= n (sum-divs n)))))

(defcheck solution-fa7dbb37
  (fn [n] (->> (range 1 n)
            (filter #(zero? (mod n %)))
            (reduce + )
            (= n))))

(defcheck solution-fa951b13
  (fn [n]
    (= n (reduce +
           (filter #(zero? (rem n %))
             (range 1 (-> (/ n 2) int inc)))))))

(defcheck solution-fabf2d3e
  (fn[n]
    (= n
      (apply + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-faebf9bb
  (fn [n] (= n
            (apply +
              (filter
                #(= 0 (rem n %))
                (range 1 n))))))

(defcheck solution-fb125be9
  (fn [n]
    (let [divisors (filter (fn [n'] (zero? (mod n n'))) (range 1 n))]
      (= (apply + divisors) n))))

(defcheck solution-fb6ee859
  (fn [n] (= n (apply + (filter #(zero? (mod n %)) (range 1 (inc (/ n 2))))))))

(defcheck solution-fb8557b6
  (fn [n]
    (->> (range 1 n)
      (filter #(= 0 (mod n %)))
      (apply +)
      (= n))))

(defcheck solution-fc1b1872
  (fn perfect-num? [n]
    (= n
      (apply + (filter #(zero? (mod n %)) (range 1 n))))))

(defcheck solution-fcce9744
  #(loop [n 1
          sum 0]
     (cond (= sum %) true
           (> sum %) false
           :else (recur (inc n) (+ n sum)))))

(defcheck solution-fcd315f7
  (fn perfect? [n]
    (letfn [(divisors [n]
              (filter #(zero? (mod n %)) (range 1 n)))]
      (= n (reduce + (divisors n))))))

(defcheck solution-fcdb0623
  (fn [n]
    (= n
      (apply +
        (reduce
          (fn [acc x]
            (let [pair (/ n x)]
              (if (== pair (int pair))
                (conj acc x pair)
                acc)))
          [1]
          (range 2 (Math/sqrt (inc n))))))))

(defcheck solution-fd23ab3d
  (fn my-perfect [n]
    (letfn [(divers [n] (filter #(integer? (/ n %)) (range 2 n)))]
      (= n (inc (reduce + (divers n)))))))

(defcheck solution-ff3bf290
  (fn [n]
    (= n (apply + (filter #(= 0 (rem n %)) (range 1 n))))))

(defcheck solution-ff59030c
  (fn [n] (= n (reduce + (filter #(= 0 (mod n %)) (range 1 n))))))

(defcheck solution-ff79de6f
  #(= % (apply + (filter (fn [i] (= (mod % i) 0)) (range 1 %)))))