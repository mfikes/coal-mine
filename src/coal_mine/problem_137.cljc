(ns coal-mine.problem-137
  (:require [coal-mine.checks :refer [defcheck-137] :rename {defcheck-137 defcheck}]
            [clojure.test]
            [clojure.pprint]))

(defcheck solution-10b83311
  (fn digits [n b]
    (let [q (quot n b) r (rem n b)]
      (if (zero? q) [r] (conj (digits q b) r)))))

(defcheck solution-10cc915c
  (fn [n b]
    (if (= n 0) [0]
                (loop [acc [] n n]
                  (if (= n 0) acc
                              (recur (cons (mod n b) acc) (int (/ n b))))))))

(defcheck solution-11031f2c
  (fn bd [n b]
    (if (zero? n)
      [n]
      (loop [rv [] tmp n]
        (if (zero? tmp)
          rv
          (recur (cons (rem tmp b) rv) (quot tmp b)))))))

(defcheck solution-11069504
  (fn [r n base]
    (let [new-n (quot n base)
          new-r (cons (rem n base) r)]
      (if (zero? new-n) new-r
                        (recur new-r new-n base)))) [])

(defcheck solution-1109d3f6
  (fn [n b]
    (first (for [[ds n] (iterate (fn [[ds n]] [(cons (mod n b) ds) (quot n b)]) ['() n])
                 :when (and (= n 0) (seq ds))]
             ds))))

(defcheck solution-11363030
  (fn [num base]
    (loop [result [] q (quot num base) m (mod num base)]
      (if (= 0 q)
        (cons m result)
        (recur (cons m result) (quot q base) (mod q base))))))

(defcheck solution-11729463
  (fn [nn bb]
    (letfn [ (baseit [n b]
               (if (= n 0)
                 []
                 (conj (baseit (quot n b) b) (rem n b)) ))
            ]
      (let [res (baseit nn bb) ]
        (if (empty? res)
          [0]
          res))
      )
    ))

(defcheck solution-11732f76
  (fn [n b]
    (if (= 0 n) '(0)
                (->> n
                  (iterate #(quot % b))
                  (take-while #(> % 0))
                  (map #(mod % b))
                  reverse
                  ))
    ))

(defcheck solution-11a6f546
  (fn to-base [n base]
    (loop [n n, acc nil]
      (if (zero? n) (or acc [0])
                    (recur (quot n base) (cons (rem n base) acc))))))

(defcheck solution-11a9d0f1
  (fn f [n b]
    (letfn [(g [m]
              (if (zero? m) []
                            (conj (g (quot m b)) (rem m b))))]
      (if (empty? (g n)) [0] (g n)))))

(defcheck solution-11f7ecc0
  (fn prob-0137
    [num base]
    (let [dig-div (fn dig-div
                    [n in-divs]
                    (let [divs (seq in-divs)]
                      (if-not divs
                        []
                        (let [div0 (first divs)]
                          (cons (int (/ n div0)) (dig-div (int (rem n div0)) (rest divs)))))))
          ]

      (let [dig-vals (reverse (take-while #(<= % (max 1 num)) (iterate #(* % base) 1)))]
        (dig-div num dig-vals)))))

(defcheck solution-120e077c
  (fn f [x base]
    (loop [coll [] remain x]
      #_(println coll )
      (if (= remain 0)
        (if (empty? coll)
          [0]
          (reverse coll))
        (let [q (quot remain base) r (rem remain base)]
          (recur (conj coll r) q))))))

(defcheck solution-126dbe40
  (fn [n b]
    (loop [r '() c n]
      (if (< c b)
        (conj r c)
        (recur (conj r (mod c b)) (quot c b))))))

(defcheck solution-12800398
  #(loop[value %1 b %2 r ()]
     (if (< value b) (vec (conj r value))
                     (recur (quot value b) b (conj r (rem value b))))))

(defcheck solution-1298e508
  (fn [n b]
    (if (= 0 n)
      [0]
      (->> (range)
        (map #(repeat % b))
        (take-while #(<= (reduce * %) n))
        (map (partial reduce *))
        reverse
        (reductions (fn [[res n] coll] (let [rema (mod n coll)
                                             diff (- n rema)]
                                         (vector (conj res (/ diff coll))
                                           rema))) [[] n])
        last
        first))))

(defcheck solution-1398b981
  (fn p [digit base]
    (if (= digit 0) [0]
                    (conj (let [q (quot digit base)]
                            (if (= q 0) []
                                        (p q base)))
                      (mod digit base)))))

(defcheck solution-14303cac
  (fn [v n]
    (letfn [(g [a r]
              (if (< a n)
                (cons a r)
                (recur (quot a n) (cons (mod a n) r))))]
      (g v []))))

(defcheck solution-14369493
  #(if (zero? %) [0]
                 (ffirst
                   (drop-while (comp not zero? second)
                     (iterate
                       (fn [[s n]] [(cons (rem n %2) s) (quot n %2)])
                       [() %])))))

(defcheck solution-1442474a
  (fn z [p q] (if (< p q) [p] (conj (z (quot p q) q) (mod p q)))))

(defcheck solution-145d988
  (fn f [x b]
    (if (< x b)
      [x]
      (conj (f (quot x b) b) (rem x b)))))

(defcheck solution-1463084c
  #(if (< %2 %3)
     (conj % %2)
     (recur (conj % (rem %2 %3)) (quot %2 %3) %3)) ())

(defcheck solution-14e7007f
  (fn [n b] (if (zero? n) '(0)
                          (->> n (iterate #(quot % b)) (take-while pos?) (map #(rem % b)) reverse))))

(defcheck solution-15c23c08
  (fn to-base [num base]
    (loop [num num acc (if (= num 0) '(0) ())]
      (if (= num 0)
        acc
        (let [digit (rem num base)]
          (recur (/ (- num digit) base) (cons digit acc)))))))

(defcheck solution-163600aa
  (fn db [n b] (if (< n b) [n] (concat (db (quot n b) b) [(mod n b)]))))

(defcheck solution-16bc1245
  (fn [x b]
    (loop [n x
           a '()]
      (if (zero? n)
        (if (empty? a) [0] a)
        (recur (quot n b) (conj a (mod n b)))))))

(defcheck solution-175ccacd
  (fn base [x b]
    (if (zero? x)
      [0]
      (loop [x x acc []]
        (if (zero? x)
          acc
          (recur (int (/ x b)) (cons (mod x b) acc))
          )
        )
      )
    ))

(defcheck solution-17ae139f
  (fn digits-in-base [num b]
    (loop [digits '()
           n num]
      (let [new-digit (rem n b)
            new-n (quot n b)]
        (if (= 0 new-n)
          (conj digits new-digit)
          (recur (conj digits new-digit)
            new-n))))))

(defcheck solution-17b95901
  (fn [n b]
    (let [ans (loop [acc [] r n]
                (let [[x y] ((juxt quot rem) r b)]
                  (if (zero? r)
                    acc
                    (recur (cons y acc) x))))]
      (if (empty? ans)
        [0]
        ans))))

(defcheck solution-18866cc8
  (fn [num base & acc] (let [quotnt (quot num base) modulo (mod num base) basexp acc] (if (= 0 quotnt) (cons modulo acc) (recur quotnt base (cons modulo acc))))))

(defcheck solution-18aad0f1
  (fn [x base]
    (if (zero? x) [x]
                  (->> (iterate (fn [[result y]]
                                  (when-not (zero? y)
                                    [(conj result (rem y base)) (int (/ y base))])) [() x])
                    (take-while (complement nil?))
                    last
                    first))))

(defcheck solution-18abb633
  (fn [n b]
    (let [f (fn x [n [f & r]]
              (when f (cons (quot n f) (x (rem n f) r))))]
      (or (f n (reverse (take-while #(<= % n) (iterate #(* b %) 1)))) [0]))))

(defcheck solution-18c47b38
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [acc []
             n n]
        (if (zero? n)
          (rseq acc)
          (recur (conj acc (rem n base))
            (quot n base)))))))

(defcheck solution-19303c61
  (fn [n b]
    (loop [n n
           accum '()]
      (if (< n b)
        (conj accum n)
        (recur (quot n b)
          (conj accum (rem n b)))))))

(defcheck solution-1955ec34
  (fn digit-base [n base]
    (loop [an () n n]
      (let [r (rem n base)
            q (quot n base)]
        (if (zero? q)
          (conj an r)
          (recur (conj an r) q))))))

(defcheck solution-19840444
  (fn [num base]
    (letfn [(step [num base]
              (concat
               (if (zero? (quot num base)) [] (step (quot num base) base))
               [(mod num base)]))]
      (step num base))))

(defcheck solution-1a8ba626
  (fn foo [num radix]
    (loop [quotient num result ()]
      (if (< quotient radix)
        (conj result quotient)
        (recur (quot quotient radix) (conj result (rem quotient radix)))))))

(defcheck solution-1a97784d
  (fn [n b]
    (if (zero? n) [0]
                  (loop [n n d '()]
                    (if (zero? n) d
                                  (recur (quot n b) (cons (rem n b) d)))))))

(defcheck solution-1ad3e72d
  (fn lp[a b](
               if (> b a) (list a)
                          (flatten (list
                                     (lp (int (/ a b)) b) (mod a b))))))

(defcheck solution-1bba3029
  (fn [x y] (if (= x 0) [0] ((fn peu [z] (if (= z 0) [] (conj (peu (quot z y)) (mod z y)) )) x))))

(defcheck solution-1bd12a5f
  (fn [n b]
    (if (zero? n)
      [0]
      (loop [n n r '()]
        (if (pos? n)
          (recur (quot n b) (conj r (rem n b)))
          r)))))

(defcheck solution-1bed5381
  (fn [x b]
    (if (zero? x)
      '(0)
      (loop [digits '()
             remaining x]
        (if (zero? remaining)
          digits
          (recur (conj digits (mod remaining b)) (quot remaining b)))))))

(defcheck solution-1c0bdab
  (fn b [n b]
    (loop [ans [] n n b b]
      (if (< n b)
        (cons n ans)
        (recur (cons (mod n b) ans) (int (/ n b)) b)
        )
      )
    ))

(defcheck solution-1c16d1b
  (fn [an b]
    (loop [res () an an b b]
      (let [q (quot an b)
            r (rem an b)
            n (* b q)
            ]
        (if (zero? n)
          (cons r res )
          (recur (cons r res) q b)
          )
        ))
    ))

(defcheck solution-1c30029b
  #(loop [n %, acc []]
     (if (zero? n)
       (if (empty? acc) [0] acc)
       (recur (int (/ n %2)) (cons (rem n %2) acc)))))

(defcheck solution-1c37909b
  (fn f [x b]
    (if (< x b) [x]
                (conj (f (int (/ x b)) b) (mod x b)))))

(defcheck solution-1c52995b
  (fn iter
    ([n b]
     (if (= n 0) [0] (iter n b [])))
    ([n b s]
     (if (= n 0) s (recur (quot n b) b (cons (rem n b) s))))))

(defcheck solution-1c9e502c
  (fn digits [number base]
    (let [work-fn (fn [n]
                    [(mod n base) (int (/ n base))])]
      (if (zero? number)
        [0]
        (loop [[digit num] (work-fn number),
               digits nil]
          (if (zero? num)
            (cons digit digits)
            (recur (work-fn num) (cons digit digits))))))))

(defcheck solution-1d44f561
  (fn [n b]
    (loop [n n b b acc ()]
      (let [q (quot n b) r (rem n b)]
        (if (zero? q) (cons r acc) (recur q b (conj acc r)))))))

(defcheck solution-1e13e3ca
  (fn [x b]
    (if (zero? x) [0]
                  (loop[dl '() x' x]
                    (if (zero? x') (vec dl)
                                   (recur (cons (mod x' b) dl) (quot x' b)))))))

(defcheck solution-1e3a84df
  (fn to-base[a-num base]
    (loop [val a-num values '()]
      (let [last-digit (rem val base)
            new-value (int (/ val base))
            new-values (conj values last-digit)
            ]
        (if (= new-value 0)
          new-values
          (recur new-value new-values)
          )
        )
      )
    ))

(defcheck solution-1e5e8604
  (fn [n base]
    (loop [x n sofar ()]
      (if (= 0 x)
        (if (empty? sofar) '(0) sofar)
        (recur (quot x base) (cons (mod x base) sofar))))))

(defcheck solution-1eed07d2
  (fn dbase [n b]
    (if (< n b)
      [n]
      (let [digit (mod n b)
            nextn (quot n b)]
        (conj (dbase nextn b) digit)))))

(defcheck solution-1f7fbc4a
  (fn [num base]
    (loop [x num
           digits ()]
      (if (= x 0) (if (empty? digits) '(0) digits)
                  (let [digit (mod x base)
                        restx (/ (- x digit) base)
                        new-digits (conj digits digit)]
                    (recur restx new-digits))))))

(defcheck solution-1fae8e8
  (fn seq-digits [num radix]
    ((fn step [r n base]
       (if (pos? n)
         (let [d (mod n base)
               q (quot n base)]
           (recur (cons d r) q base))
         (if (empty? r) '(0) r)))
     '() num radix)))

(defcheck solution-200dcc33
  (fn [x base]
    (loop [n x
           ret []]
      (if (< n base)
        (cons n ret)
        (recur (quot n base)
          (cons (mod n base) ret )))
      )
    ))

(defcheck solution-201f68e9
  (fn rebase-outer [number-outer base]
    (letfn [(rebase [number]
              (if (zero? number)
                []
                (let [remainder   (mod number base)
                      outstanding (- number remainder)
                      new-number  (/ outstanding base)
                      full-rebase (conj (rebase new-number) remainder)]
                  full-rebase)))]
      (let [result (rebase number-outer)]
        (if (empty? result) [0] result)))))

(defcheck solution-2079ab04
  (fn f [n b]
    (if (< n b)
      [n]
      (conj (f (quot n b) b)
        (mod n b)))))

(defcheck solution-208af4e4
  (fn [digit base]
    (if (zero? digit) [0]
                      ((fn f [x]
                         (if (zero? x)
                           []
                           (conj (f (int (/ x base)))
                             (mod x base)))
                         ) digit))
    ))

(defcheck solution-210deb89
  (fn db [num base]
    (loop [dividend num
           reminder (rem num base)
           result []]
      (if (< dividend base)
        (conj result dividend)
        (recur
          (quot dividend base)
          (rem  dividend base)
          (cons (rem dividend base) result))))))

(defcheck solution-21b60108
  (fn mod-n [d base]
    (cond
      (zero? d) [d]
      :otherwise
      (vec (loop [acc (list) dgt d]
             (if (zero? dgt)
               acc
               (let [r (rem dgt base)
                     q (quot dgt base)]
                 (recur (conj acc r) q))))))))

(defcheck solution-21e8343
  (fn x [n b]
    (if (zero? n)
      [0]
      (let [q (quot n b)
            r (mod n b)
            f (if (zero? q) [] (x q b))]
        (conj f r)))))

(defcheck solution-22488057
  (fn [n b]
    (loop [v n
           acc nil]
      (if (zero? v)
        (if acc acc '(0))
        (recur (int (/ v b)) (conj acc (rem v b)))))))

(defcheck solution-2271264a
  (fn anybase ([n b] (if (= n 0) (list n) (anybase n b '())))
    ([n b acc] (if (= n 0) acc
                           (anybase
                             (quot n b)
                             b
                             (cons (mod n b) acc))))))

(defcheck solution-22927e27
  (fn [n d]
    (if (zero? n) '(0)
                  (loop [a n,r '()]
                    (if (zero? a) r
                                  (recur (quot a d) (conj r (rem a d))))))))

(defcheck solution-22ba03e9
  (fn [n b] (let [
                  go (fn [n b v] (if (zero? n) v
                                               (recur (quot n b) b (cons (rem n b) v))))]
              (if (zero? n) [0] (go n b [])))))

(defcheck solution-22ba64b7
  (fn n-base [n base]
    (loop [n_ n
           result '()]
      (if (zero? n_)
        (if (= 0 (count result)) '(0) result)
        (recur (quot n_ base) (cons (mod n_ base) result) )))))

(defcheck solution-22baf4fb
  (fn [number num-system]
    (if (= number 0)
      [0]
      (loop [result [] i 0 ]
        (let [base (Math/pow num-system i)]
          (if (> base number)
            (loop [end-result [] num number elements (reverse result)]
              (if elements
                (recur
                  (conj end-result (int (/ num (first elements))))
                  (mod num (first elements))
                  (next elements)
                  )
                end-result
                )
              )
            (recur
              (conj result base) (inc i)
              )
            )
          )
        )
      )
    ))

(defcheck solution-22e2a44b
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [n n
             acc []]
        (if (zero? n)
          acc
          (recur (quot n base)
            (cons (rem n base) acc)))))))

(defcheck solution-22fbba67
  (fn convert [n base]
    (if (< n base)
      [n]
      (conj (convert (quot n base) base) (rem n base)))))

(defcheck solution-23026de1
  (fn [num base]
    (loop [acc ()
           num num]
      (if (< num base)
        (conj acc num)
        (recur (conj acc (rem num base))
          (quot num base))))))

(defcheck solution-23636721
  (fn digits
    [v b]
    (if (< v b)
      (list v)
      (concat (digits (quot v b) b) [(mod v b)]))))

(defcheck solution-23eba62c
  (fn z [num sys]
    (let [f (fn x [n] ((juxt quot mod) n sys))]
      (loop [num num res '()]
        (let [[r m] (f num)]
          (cond
            (= 1 r) (into [] (conj res m r))
            (= 0 r) (into [] (conj res m))
            :else (recur r (conj res m))))))))

(defcheck solution-2431757c
  (fn [x b]
    ( if ( zero? x) [0]
                    ( loop [a x acc []]
                      (if (zero? a)
                        (reverse acc)
                        ( recur (quot a b)   (concat  acc  [(rem a b)])))))))

(defcheck solution-2487efc6
  (fn [nn b]
    (if (= nn 0)
      [0]
      (loop [n nn
             acc []]
        (if-not (zero? n)
          (recur (quot  n b) (conj acc (mod n b)))
          (reverse acc))))))

(defcheck solution-2495dd69
  (fn f [n b]
    (let [[h & r :as t] (if (= 0 n) [0]
                                    (let [r (rem n b)] (conj (vec (f (/ (- n r) b) b)) r)))]
      (if (and r (= 0 h)) r
                          t))
    ))

(defcheck solution-24acc73c
  (fn [n b]
    (loop [x n, v '()]
      (if (>= x b) (recur (int (/ x b)) (conj v (rem x b)))
                   (conj v x)))))

(defcheck solution-2526dbdd
  (fn [digits base]
    (if (= 0 digits) [0]
                     (loop [remainder digits
                            bag '()]
                       (if (= 0 remainder)
                         bag     ; base case
                         (recur (quot remainder base)
                           (conj bag (mod remainder base))))))))

(defcheck solution-2539448
  (fn in-base [n b]
    (let [x (mod n b)
          r (quot n b)]
      (if (zero? r)
        [x]
        (conj (in-base r b) x)
        ))))

(defcheck solution-25e21116
  (fn [number radix]
    (loop [n number, d ()]
      (let [q (quot n radix), r (rem n radix)]
        (if (zero? q) (conj d r) (recur q (conj d r)))))))

(defcheck solution-25f30f55
  (fn
    [n b]
    (if (= 0 n)
      '(0)
      (loop [a '()
             n n]
        (if (= 0 n)
          a
          (recur (conj a (mod n b)) (quot n b)))))))

(defcheck solution-2612127e
  (fn rec-digits [ r n b ]
    (if (< n b)
      (conj r n)
      (recur (conj r (mod n b)) (quot n b) b))) ())

(defcheck solution-261db8ee
  (fn to-base [num base]
    (loop [result [] next num]
      (let [next-in-base (rem next base)
            next-to-base (quot next base)]
        (cond (= 1 next) (cons next result)
              (and (zero? next) (pos? (count result))) result
              (and (zero? next) (empty result)) (cons next result)
              :else (recur (cons next-in-base result) next-to-base))))))

(defcheck solution-265206
  (fn [n b]
    (if (zero? n) '(0)
                  (loop [n n r '()]
                    (if (zero? n) r
                                  (recur (quot n b) (conj r (rem n b))))))))

(defcheck solution-26c4bc89
  (fn p137[x b]
    (if (= x 0)
      [0]
      (loop [n x r '()]
        (if (= 0 n) r
                    (recur (quot n b) (cons (rem n b) r)))))))

(defcheck solution-26d3c13a
  (fn [n b]
    (if (zero? n)
      [0]
      (loop [n n acc []] (if (zero? n) acc (recur (quot n b) (cons (mod n b) acc)))))))

(defcheck solution-270d3144
  (fn [n b]
    (letfn
     [(r [x coll]
        (if (zero? x) coll
                      (recur (quot x b) (cons (rem x b) coll))))]
      (if (zero? n) [0]
                    (r n [])))))

(defcheck solution-27303fa5
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [digit-seq n acc []]
        (if (zero? digit-seq)
          acc
          (recur
            (quot digit-seq base)
            (cons (rem digit-seq base) acc)))))))

(defcheck solution-273e87db
  (fn f [i b]
    (let [[q m] ((juxt quot mod) i b)]
      (cond
        (= q 0) [m]
        (< q b) [q m]
        1 (conj (f q b) m)))))

(defcheck solution-27aaae5a
  (fn [n base]
    (loop [digit-seq (list)
           i n]
      (if (< i base)
        (conj digit-seq i)
        (recur (conj digit-seq (mod i base))
          (int (/ i base)))))))

(defcheck solution-27d0ef72
  (fn [x n]
    (loop[q (quot x n)
          r (rem x n)
          res []]
      (if (= q 0)
        (cons r res)
        (recur (quot q n) (rem q n) (cons r res))))))

(defcheck solution-2845cb60
  (fn digits [n base]
    (if (zero? n)
      [0]
      (mapv
        #(mod (quot n %) base)
        (reverse (take-while #(<= % n) (iterate #(* base %) 1)))))))

(defcheck solution-2859f15
  (fn di
    ([v d]
     (di v d []))
    ([v d l]
     (if (< v d) (cons v l)
                 (recur (quot v d) d (cons (rem v d) l))))))

(defcheck solution-290b2c6d
  (fn [n b]
    (loop [a () q n]
      (if (zero? q)
        (if (empty? a) '(0) a)
        (recur (cons (mod q b) a) (quot q b))))))

(defcheck solution-296a324e
  #(if(= 0 %1)[0](loop[n %1 d()](if(= 0 n)d(recur(quot n %2)(conj d(rem n %2)))))))

(defcheck solution-2972da4c
  #(loop [r [] d %1]
     (if (zero? d)
       (if (empty? r) [0] r)
       (recur (cons (rem d %2) r) (int (/ d %2))))))

(defcheck solution-29ab7d2a
  (fn base [n b]
    (loop [l [], r n]
      (if (= 0 r)
        (if (empty? l) [0] l)
        (recur (cons  (mod r b) l) (/ (- r (mod r b)) b))))))

(defcheck solution-29d42fe
  (fn digits-base [n b]
    (case n
      0 [0]
      (let [p (reverse
                (take-while #(>= n %)
                  (reductions * 1 (repeat b))))]
        (loop [o [] n n p p]
          (if-let [a (first p)]
            (recur (conj o (int (/ n a)))
              (mod n a)
              (rest p))
            o))
        ))))

(defcheck solution-2a2a8cf1
  (fn [n radix]
    (loop [n n, ds (if (zero? n) '(0) ())]
      (if (zero? n)
        ds
        (recur (quot n radix) (cons (rem n radix) ds))))))

(defcheck solution-2b0fc133
  (fn bar
    [number base]
    (let [foo (fn
                [number base]
                (loop [exponent 1]
                  (if (> (Math/pow base exponent) number)
                    (dec exponent)
                    (recur (inc exponent)))))
          exponent (foo number base)]
      (loop [current-exponent exponent
             current-number number
             result []]
        (if (= current-exponent 0)
          (conj result current-number)
          (let [raised (int (Math/pow base current-exponent))
                remainder (rem current-number raised)
                quotient (quot current-number raised)]
            (recur (dec current-exponent) remainder (conj result quotient))))))))

(defcheck solution-2b56ae90
  (fn f [x y z]
    (if-not (zero? y)
      (let [a (mod y z)
            b (/ (- y a) z)]
        (f (into [a] x) b z))
      (if (empty? x)
        [0]
        x))) [])

(defcheck solution-2b718339
  (fn get-digits [n b]
    (if (zero? n)
      (vector n)
      ((fn [n b r]
         (if (zero? n)
           r
           (recur (quot n b) b (conj r (rem n b))))) n b nil))))

(defcheck solution-2c221eb5
  (fn [n b]
    (loop [n n acc []]
      (if (< n b)
        (conj acc n)
        (recur (quot n b)(cons (mod n b) acc))))))

(defcheck solution-2c2ac6c7
  (fn [n b]
    (loop [r '(), n n]
      (if (zero? n)
        (if (empty? r) [0] r)
        (recur (conj r (mod n b)) (quot n b))))))

(defcheck solution-2cafccd3
  (fn [x base]
    (let [
          todigs
          (reverse
            (rest
              (map
                second
                (take-while
                  (fn [[d m]](not= 0 d m))
                  (iterate
                    (fn [[d m]] [(quot d base)(mod d base)])
                    [x 0])))))]
      (if (zero? x)
        [0]
        todigs))))

(defcheck solution-2cd142e1
  (fn r
    ([n b] (if (= n 0)
             [0]
             (r n b '())))
    ([n b t]
     (if (= n 0)
       t
       (r (quot n b) b (conj t (rem n b)))))))

(defcheck solution-2cda8481
  (fn ! [n rx]
    (loop [a n rslt '()]
      (let [rm (mod a rx)
            z (int (/ a rx))]
        (if (< a rx)
          (cons rm rslt)
          (recur z (cons rm rslt)))))))

(defcheck solution-2ce05f0f
  (fn f [n b]
    (if (<= b n)
      (conj (f (quot n b) b) (rem n b))
      [n])
    ))

(defcheck solution-2d1c2f85
  (fn f [x b]
    (let [y (int (/ x b))]
      (conj (if (zero? y) [] (f y b)) (mod x b)))))

(defcheck solution-2d5d22cf
  (fn [n base]
    (if (zero? n) (list 0)
                  (loop [n n, pow 1, res '()]
                    (if (zero? n)
                      res
                      (let [digit (rem (quot n pow) base)]
                        (recur (- n (* digit pow)) (* pow base) (conj res digit))))))))

(defcheck solution-2d9d1b94
  (fn tobase
    ([n base] (tobase n base ()))
    ([n base result]
     (if (= n 0)
       (if (empty? result) [0] result)
       (tobase (quot n base) base (cons (rem n base) result))))))

(defcheck solution-2ee3cc7b
  (fn [n b]
    (if (= n 0) [0]
                (loop [r () n n]
                  (if (= 0 n) r
                              (recur (cons (rem n b) r)
                                (quot n b)))))))

(defcheck solution-2f910b67
  (fn [number base]
    (case number
      0 [0]
      ((fn digs [n]
         (if (zero? n) [] (conj (digs (quot n base)) (mod n base))))
       number))))

(defcheck solution-2fa6b176
  (fn [n b]
    (if (= n 0)
      [0]
      (loop [n n
             d []]
        (if (= n 0)
          d
          (recur (quot n b) (cons (mod n b) d)))))))

(defcheck solution-301e0334
  (fn t ([n b] (t n b '()))
    ([n b a]
     (let [r (rem n b)]
       (if (= r n) (conj a r)
                   (t (int (/ n b)) b (conj a r)))))))

(defcheck solution-3095af72
  (fn f
    ([n d] (let [res (f n d [])]
             (if (seq res)
               (reverse res)
               [0]
               )
             )
     )
    ([n d acc]
     (let [remainder (rem n d)
           newn (quot n d)]
       (if (zero? n)
         (if (seq acc)
           acc
           [0]
           )
         (f newn d (conj acc remainder))
         )
       )
     )
    ))

(defcheck solution-309f2989
  (fn digits [n b]
    (if (< n b)
      [n]
      (conj (digits (quot n b) b) (mod n b)))))

(defcheck solution-310c0ccc
  (fn [n b]
    (letfn [(toBase [n b output]
              (if (>= n b)
                (recur (quot n b) b (cons (mod n b) output))
                (cons n output)))]
      (toBase n b []) )))

(defcheck solution-31222ad6
  (fn [n b]
    (->>
      [0 n]
      (iterate (fn[[r x]][(rem x b) (quot x b)]))
      (take-while (fn[[r x]](or (not= r 0)(> x 0))))
      (map first)
      (#(if (> (count %) 0) (drop 1 %) [0]))
      (reverse))))

(defcheck solution-312edf7f
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [n n, acc []]
        (if (zero? n)
          acc
          (recur (quot n base)
            (cons (rem n base) acc)))))))

(defcheck solution-3174ac44
  (fn to-base [n base]
    (loop [xs (list (mod n base))
           n (quot n base)]
      (if (zero? n) xs
                    (recur (cons (mod n base) xs) (quot n base))))))

(defcheck solution-31aee68f
  (fn to-base [n b]
    (loop [curr n res '()]
      (if (>= curr b)
        (recur (quot curr b) (conj res (rem curr b)))
        (conj res curr)))))

(defcheck solution-31c3de09
  (fn [n m]
    (if (= n 0) [0]
                (loop [n n
                       dseq ()]
                  (if (zero? n)
                    dseq
                    (recur (quot n m)
                      (cons (mod n m) dseq)))))))

(defcheck solution-31e2d26e
  (fn [n b]
    (let [ds (reverse
               (take-while #(<= % n)
                 (reductions
                   #(* % %2) 1 (repeat b))))]
      (if (zero? n) [0]
                    (first (reduce (fn [[s r] d]
                                     [(conj s (quot r d)) (rem r d)])
                             [[] n] ds))))))

(defcheck solution-324beffc
  (fn [x b]
    (if (zero? x)
      [0]
      (let [f (fn [[r _]]
                [(int (/ r b)) (rem r b)])]
        (->> (iterate f (f [x]))
          (take-while #(not= % [0 0]))
          (map second)
          (reverse))))))

(defcheck solution-32d792af
  (fn d-n-b [n b]
    (if (< n b)
      [n]
      (conj (d-n-b (quot n b) b) (rem n b)))))

(defcheck solution-32e05773
  (fn [n b]
    (if (= n 0)
      [0]
      (loop [c (reverse (take-while #(<= % n) (iterate #(* % b) 1))) x n r []]
        (if (empty? c)
          r
          (recur (rest c) (mod x (first c)) (conj r (quot x (first c)))))))))

(defcheck solution-32f70b75
  (fn [n b]
    (loop [n n result []]
      (if (= n 0)
        (if (= (count result) 0)
          [0]
          (reverse result))
        (recur (quot n b) (conj result (mod n b)))))))

(defcheck solution-333ab73d
  (fn [d b]
    (loop [s ()
           x (quot d b)
           r (rem d b)]
      (if (= x 0)
        (conj s r)
        (recur (conj s r) (quot x b) (rem x b))))))

(defcheck solution-333f4a65
  (fn [x b]
    (loop [res ()
           n x]
      (if (zero? (quot n b))
        (cons n res)
        (recur (cons (mod n b) res)
          (quot n b))))))

(defcheck solution-33d791fc
  (fn [n b]
    (if (zero? n)
      [0]
      (loop [acc '()
             n n]
        (if (zero? n)
          acc
          (let [digit (int (/ n b))
                remainder (mod n b)]
            (recur (cons remainder acc) digit)))))))

(defcheck solution-3456fcaf
  (fn base (
            [n b] (base n b '()))
    ([n b acc]
     (let [
           q (quot n b)
           r (mod n b)
           newacc (cons r acc)
           ]
       (if (zero? q) newacc (recur q b newacc))))
    ))

(defcheck solution-3484fec3
  (fn [n base]
    ((fn foo [n base acc]
       (let [d (mod n base)
             r ( / (- n d) base)]
         (if (> r  0)
           (foo r  base (cons d acc))
           (cons d acc)
           ))) n base '())))

(defcheck solution-348870dd
  (fn [n b]
    (letfn [(base-digits
              [n b]
              (let [x (if (< n 1)
                        '(0)
                        (let [r (mod n b)]
                          (concat (base-digits (/ (- n r) b) b) (list r))))]
                x))]
      (let [y (base-digits n b)]
        (if (> (count y) 1)
          (rest y)
          y)))))

(defcheck solution-34a2e5ea
  (fn d [n b] (if (< n b) [n] (conj (d (quot n b) b) (rem n b)))))

(defcheck solution-3501fefd
  (fn f
    ([n b] (f n b '()))
    ([n b a]
     (if (> b n)
       (cons n a)
       (let [r (mod n b)
             nmr (- n r)]
         (recur (/ nmr b) b (cons r a)))))))

(defcheck solution-355df6ba
  #(loop [i %1 r '()]
     (let [t (quot i %2)]
       (if (= 0 t)
         (conj r (mod i %2))
         (recur t (conj r (mod i %2)))))))

(defcheck solution-35c479e9
  #(loop [number %1 base %2 result '()]
     (if (< number base) (conj result number)
                         (recur (quot number base) base (conj result (rem number base))))))

(defcheck solution-35e35750
  (fn digits-and-bases [n base]
    (loop [x n s '()]
      (if (zero? x)
        (if (empty? s)
          (conj s 0)
          s)
        (recur (quot x base) (conj s (rem x base)))))))

(defcheck solution-3605f9a8
  #(if (= %1 0) [0] (
                     (fn dig [n b]
                       (if (= 0 n)
                         []
                         (conj (dig (quot n b) b) (mod n b)))) %1 %2)))

(defcheck solution-363d2221
  (fn ! [n b] (if (< n b) [n] (conj (! (quot n b) b) (rem n b)))))

(defcheck solution-3665b1e6
  (fn [n base]
    (loop [n n
           res ()]
      (if (zero? n)
        (if (empty? res) [0] res)
        (recur (quot n base) (cons (rem n base) res))))))

(defcheck solution-36de4c10
  (fn cv [n b]
    (if (< n b) [n]
                (vec (concat (cv (int (/ n b)) b)
                             (list(mod n b)))))))

(defcheck solution-37123dfe
  (fn [n b]
    (loop [n n res '()]
      (if (= 0 n) (if (empty? res) [0] (vec res))
                  (recur (quot n b) (cons (mod n b) res))))))

(defcheck solution-376065a3
  (fn f
    ([num base] (vec (f num base [])))
    ([num base res]
     (if (= 0 (quot num base)) (cons num res)
                               (recur (quot num base) base (cons (mod num base) res))))))

(defcheck solution-3766159b
  (fn digit-base [val radix]
    (loop[result []
          val val]
      (if(< val radix)
        (vec (reverse (concat result (vector val))))
        (recur (concat result (vector (rem val radix)))
          (quot val radix))))))

(defcheck solution-376b44a
  #(loop [cur nil, r %1]
     (if (= r 0)
       (if cur cur '(0))
       (recur (conj cur (rem r %2)) (quot r %2)))))

(defcheck solution-37906763
  (fn convert-base
    ([n b]
     (if (= n 0) '(0)
                 (let [p (reverse (take-while #(<= % n) (map #(Math/pow b %) (range))))]
                   (convert-base '() p n ))))
    ([l p r]
     (if (= (count p) 1)
       (reverse (conj l (int r)))
       (convert-base (conj l (int (/ r (first p)))) (rest p) (rem r (first p)))))))

(defcheck solution-38d0ee9f
  (fn _
    ([n b] (_ n b []))
    ([n b r]
     (let [rd (int (Math/floor (/ n b)))
           rm (rem n b)
           r (cons rm r)]
       (if (zero? rd)
         r
         (_ rd b r))))))

(defcheck solution-391977b7
  (fn d [n b]
    (if (zero? n) [n]
                  (let [s (quot (Math/log n) (Math/log b)) q (Math/pow b s)]
                    (loop [r n v [] p q]
                      (let [d (int (quot r p)) x (- r (* d p))]
                        (if (<= p 1)
                          (conj v d)
                          (recur x (conj v d) (quot p b))
                          )))))))

(defcheck solution-392ca4ee
  (fn changes [num basis]
    (let [d (int (/ num basis))
          r (rem num basis)]
      (cond
        (= d 0) (list r)
        (< d basis) (list d r)
        :else (concat (changes d basis) (list r))))))

(defcheck solution-3990c47f
  (fn [n b]
    (loop [r '() num n]
      (if (> b num)
        (conj r num)
        (recur (cons (mod num b) r) (int (/ num b)))))))

(defcheck solution-39cb1bff
  (fn [n b]
    (if (zero? n) '(0)
                  ((fn [n b acc]
                     (if (zero? n) acc
                                   (recur (int (/ n b)) b (cons (rem n b) acc))))
                   n b '()))))

(defcheck solution-39eb9414
  (fn it [n b]
    (if (= 0 (quot n b))
      [(mod n b)]
      (conj (it (quot n b) b) (mod n b)))))

(defcheck solution-3a2bb748
  #(loop [n % b %2 res []]
     (if (zero? n) (if (zero? %) [0] (reverse res))
                   (recur (quot n b) b (conj res (mod n b))))))

(defcheck solution-3a35d359
  (fn [x b] (if (= x 0) [0] (reverse
                              ((fn ! [i] (lazy-seq (when (> i 0) (cons (rem i b) (! (quot i b))))))
                               x)))))

(defcheck solution-3ad64dcd
  (fn [n b]
    (if (zero? n) [0]
                  (map second
                    (reverse (rest
                               (take-while #(not (= % [0 0]))
                                 (iterate (fn [[n r]] [(int (/ n b)) (rem n b)]) [n 0]))))))))

(defcheck solution-3ae33765
  (fn [n b]
    (if (zero? n) '(0)
                  (loop [r '() n n]
                    (if (zero? n) r
                                  (recur (cons (mod n b) r) (quot n b)))))))

(defcheck solution-3b31d568
  (fn __
    [number base]
    (loop [n number
           r '()]
      (if (zero? n)
        (if (empty? r) [number] r)
        (recur (quot n base) (conj r (rem n base)))))))

(defcheck solution-3b8a215a
  (fn [n b]
    (loop [out (if (zero? n) '(0) '()) r n]
      (if (zero? r)
        out
        (let [d (rem r b) r2 (/ (- r d) b)]
          (recur (conj out d) r2))))))

(defcheck solution-3bf3f1e6
  (fn [n base]
    (let [p (fn p [n base]
              (if (= n 0)
                []
                (concat  (p (quot n base) base)[(mod n base)])))
          r (p n base)]
      (if (empty? r)
        [0]
        r))))

(defcheck solution-3c4c991e
  (fn [n b]
    (if (zero? n)
      (list 0)
      (loop [x n, v '()]
        (if (zero? x)
          v
          (recur (quot x b) (conj v (mod x b))))))))

(defcheck solution-3d3315c9
  (fn digits-with-base [n base]
    (letfn [(decompose [n]
              (if (< n base)
                [n]
                (let [q (quot n base)
                      r (rem n base)]
                  (cons r (decompose q)))))]
      (reverse (decompose n)))))

(defcheck solution-3d338762
  (fn [number base]
    (loop [n number, n-in-base nil]
      (let [q (quot n base),
            r (rem  n base),
            b (cons r n-in-base)]
        (if (zero? q) b (recur q b))))))

(defcheck solution-3d5a686a
  (fn tb
    ([x b]
     (if (zero? x)
       [0]
       (tb x b [])))
    ([x b res]
     (if (zero? x)
       res
       (tb (quot x b) b (cons (rem x b) res))))))

(defcheck solution-3d621bbd
  (fn digits [n b]
    (if (< n b)
      [n]
      (conj (digits (quot n b) b) (rem n b)))))

(defcheck solution-3d6769c0
  #(if (zero? %)
     [0]
     (loop [n % i (int (/ (Math/log %) (Math/log %2))) acc [] ]
       (if (= i 0)
         (conj acc (int n))
         (let [x (Math/pow %2 i)
               q (int (quot n x))]
           (recur (- n (* x q)) (dec i) (conj acc q)))
         ))))

(defcheck solution-3dc41cb5
  (fn [n b]
    (or
     (reduce #(cons (mod %2 b) %1) nil (take-while pos? (iterate #(quot % b) n)))
     [0])))

(defcheck solution-3def3774
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [n n
             s []]
        (if (zero? n)
          s
          (recur (quot n base) (cons (mod n base) s)))))))

(defcheck solution-3e28af6
  (fn [n b]
    (if (zero? n) [0]
                  (->> (iterate #(quot % b) n)
                    (take-while #(< 0 %))
                    (map #(rem % b))
                    reverse))))

(defcheck solution-3e29ce7c
  (fn [n b]
    (reverse
      (map #(rem % b)
        (cons n
          (take-while pos?
            (iterate #(quot % b)
              (quot n b))))))))

(defcheck solution-3e4e80a4
  (fn [n b]
    (loop [a [] n n]
      (if (< n b)
        (cons n a)
        (let [r (rem n b) q (quot n b)]
          (recur (cons r a) q))))))

(defcheck solution-3e71653d
  (fn [n b]
    (if (zero? n) [0]
                  (loop [n n i b r ()]
                    (if (zero? n) r
                                  (recur
                                    (- n (mod n i))
                                    (* i b)
                                    (cons
                                      (/ (* b (mod n i)) i)
                                      r)))))))

(defcheck solution-3eff20bc
  (fn p137
    [num base]
    (let [a (quot num base)
          b (rem num base)]
      (if (zero? a)
        [b]
        (conj (p137 a base) b)))))

(defcheck solution-3f722e4b
  (fn [n b]
    (loop [n n r '()]
      (if (< n b)
        (cons n r)
        (recur (quot n b) (cons (rem n b) r))))))

(defcheck solution-3fa1068a
  (fn d [n b]
    (if (< n b)
      [n]
      (conj (d (quot n b) b) (rem n b)))))

(defcheck solution-40489cb4
  (fn radix [x r]
    (if (= x 0)
      [0]
      (rseq (loop [v x acc []] (if (= v 0) acc (recur (quot v r) (conj acc (mod v r)))))))))

(defcheck solution-405721d4
  (fn f
    ([n b z]
     (if (zero? n)
       z
       (conj (f (quot n b) b []) (mod n b) )
       ))
    ([n b] (f n b [0]))))

(defcheck solution-40967133
  (fn __ [n base]
    (let [v (mod n base)
          r (int (/ n base))]
      (if (zero? r)
        [v]
        (conj (__ r base) v)))))

(defcheck solution-40eb7305
  (fn [n b]
    (loop [n n ret []]
      (if (= n 0)
        (if (empty? ret) [0] ret)
        (recur (quot n b) (concat [(rem n b)] ret))))))

(defcheck solution-40eeebf0
  (fn
    [n b]
    (if (zero? n)
      [0]
      (loop [n n acc []]
        (if (zero? n)
          (reverse acc)
          (recur
            (int (/ n b))
            (conj acc (mod n b))))))))

(defcheck solution-41a647f0
  (fn
    [n base]
    (loop [n n
           digits '()]
      (if (zero? n)
        (or (seq digits) [0])
        (recur (quot n base) (conj digits (rem n base)))))))

(defcheck solution-41a6fdd0
  (fn [n b]
    (loop [d '() n n]
      (if (> n 0)
        (recur (conj d (mod n b)) (int (/ n b)))
        (if (zero? (count d)) '(0) d)))))

(defcheck solution-420ea53d
  (fn encode [n base]
    (if (zero? n) [0]
                  (loop [n n acc []]
                    (if (zero? n)
                      acc
                      (recur (quot n base) (cons (mod n base) acc)))))))

(defcheck solution-42c0c07c
  #(reverse
     ((fn i->rs [x]
        (cons (mod x %2)
          (let [q (quot x %2)]
            (if (pos? q)
              (i->rs q))))) %1)))

(defcheck solution-4305122a
  (fn base-convert
    [n base]
    (reverse
      (loop [q (quot n base) r (mod n base) res []]
        (if (zero? q)
          (conj res r)
          (recur (quot q base) (mod q base) (conj res r)))))))

(defcheck solution-443fc959
  (fn [x b]
    (loop [x x
           acc '()]
      (if (< x b)
        (cons x acc)
        (recur (quot x b) (cons (rem x b) acc))))))

(defcheck solution-444ab3c
  (fn db [nx b]
    (loop [n nx s nil]
      (let [q (quot n b)
            r (rem n b)]
        (if (zero? q) (cons r s)
                      (recur q (cons r s)))))
    ))

(defcheck solution-44ea4c41
  (fn [n radix]
    (reverse (loop [n n
                    digits []]
               (if (zero? n) (if (empty? digits) [0] digits)
                             (recur (int (/ n radix)) (conj digits (mod n radix))))))))

(defcheck solution-45321ea1
  (fn f
    ([d b] (f d b nil))
    ([d b l]
     (if (= d 0)
       (if l [] [0])
       (conj
         (f (quot d b) b true)
         (rem d b))))))

(defcheck solution-45559bc6
  (fn digs [n b]
    (if (< n b)
      [n]
      (conj (digs (quot n b) b) (mod n b)))))

(defcheck solution-45a16ff9
  (fn f [a n b]
    (if (> n 0) (f (into [(mod n b)] a) (quot n b) b)
                (if (empty? a) [0] a))) [])

(defcheck solution-45dbf3de
  trampoline (fn t
               ([n b] (t (quot n b) b [(rem n b)]))
               ([n b c] (if (zero? n) c (fn [] (t (quot n b) b (cons (rem n b) c)))))))

(defcheck solution-45e437c4
  (fn dig-base [n b]
    (if (= n 0)
      [0]
      (loop [re n exp 1 out []]
        (if (= re 0)
          out
          (recur (- re (int (mod re (Math/pow b exp))))
            (inc exp)
            (cons (int (/ (int (mod re (Math/pow b exp))) (Math/pow b (dec exp)))) out))
          )))))

(defcheck solution-464331e2
  (fn base [x y]
    (if
     (< x y)
      [x]
      (conj  (base (quot x y) y) (mod x y))
      )
    ))

(defcheck solution-467f44fe
  (fn [v b]
    (if (zero? v) [0]
                  (loop [d '() cur v]
                    (if (zero? cur) d
                                    (recur (conj d (mod cur b)) (quot cur b)))
                    ))
    ))

(defcheck solution-4692d2e6
  (fn clj137
    [num base]
    (let [fnum (iterate #(* % base) base)
          base-candidate (reverse (concat [1] (take-while #(<= % num) fnum)))]
      (cond
        (= num 0) [0]
        :else (loop [[x & xs] base-candidate n num res []]
                (if
                 x (cond
                     (= n 0) res
                     (and (= n x) (= 1 (count xs))) (conj res (quot n x) 0)
                     (>= n x) (recur xs (- n (* x (quot n x))) (conj res (quot n x)))
                     (< n x) (recur xs n (conj res 0)))
                   res))))))

(defcheck solution-46a1adb
  (fn digits [n b]
    (let [digit (int (mod n b))
          next-n (Math/floor (/ n b))]
      (if (== next-n 0)
        (vector digit)
        (conj (digits next-n b) digit)))))

(defcheck solution-46b0b44a
  (fn [n base]
    (loop [n n
           res '()]
      (if (< n base)
        (vec (conj res n))
        (recur (quot n base) (conj res (rem n base)))))))

(defcheck solution-46d28838
  (fn rebase [n b] (if (< n b) [n] (conj (rebase (quot n b) b) (rem n b)))))

(defcheck solution-46e8f6b3
  (fn [num base]
    (letfn [(to-base [n b]
              (if (zero? n)
                []
                (conj (to-base (quot n b) b) (mod n b))))]

      (if (zero? num)
        [0]
        (to-base num base)))))

(defcheck solution-470d4c81
  (fn [x b]
    (letfn [(f [l x b] (if (= 0 x) l
                                   (recur (cons (mod x b) l) (quot x b) b)))]
      (if (= 0 x) (list x)
                  (f '() x b)))))

(defcheck solution-471b37b2
  (fn bases [n base]
    (if (zero? n) [0]
                  (loop [n n xs '()]
                    (if (zero? n) xs
                                  (recur (quot n base) (conj xs (mod n base))))
                    )
                  )))

(defcheck solution-4784aec0
  (fn [n b]
    (if (zero? n)
      (list 0)
      (reverse
        (map
          #(quot (mod n (* % b)) %)
          (take-while #(>= n %) (iterate (partial * b) 1)))))))

(defcheck solution-479100ab
  #(if (< %1 %2) [%1] (loop [n %1 r []] (if (= 0 n) r (recur (quot n %2) (cons (mod n %2) r))))))

(defcheck solution-482aa08e
  #(loop [n % ds nil]
     (if (zero? n)
       (if ds ds [0])
       (recur (quot n %2) (conj ds (mod n %2))))))

(defcheck solution-489b74e9
  (fn convert-base [n b]
    (if (zero? n)
      [0]
      (loop [k n, digits ()]
        (if (zero? k)
          digits
          (recur (quot k b) (conj digits (rem k b))))))))

(defcheck solution-48b1284e
  (fn digits [n b]
    (letfn [(helper [i]
              (if (zero? i) []
                            (let [r (rem i b)]
                              (conj (helper (int (/ i b))) r))))]
      (if (zero? n) [0] (helper n)))))

(defcheck solution-49622254
  (fn b [n base] (if (< n base) [n]  (conj (b (int (/ n base)) base) (mod n base)))))

(defcheck solution-497c8730
  (fn [v base]
    (if (= v 0) [0]
                (reverse (loop[ result [] restv v ]
                           (if (= restv 0)
                             result
                             (recur (conj result (mod restv base)) (quot restv base) )
                             )
                           )
                  ) )     ))

(defcheck solution-49f88bbe
  (fn f [n b]
    (conj (if (< 0 (quot n b))
            (f (quot n b) b)
            [])
      (rem n b))))

(defcheck solution-4a0bc326
  (fn [n b] (or (->> (iterate (fn [[a r]] [(quot a b) (rem a b)]) [n])
                  (take-while (comp pos? (partial apply +)))
                  (keep second)
                  (reverse)
                  (seq))
                [0])))

(defcheck solution-4a81f3f7
  (fn digits [n b]
    (if (< n b) (list n)
                (concat (digits (quot n b) b) (list (rem n b))))))

(defcheck solution-4ac3eca1
  (fn [n b]
    (loop [c () r n]
      (if (< r b) (conj c r)
                  (recur (conj c (mod r b)) (quot r b))))))

(defcheck solution-4b05db5c
  ;; For base < 36 interop is enough
  ;;(fn [n b] (map #(Integer/parseInt (str %)) (Integer/toString n b)))
  (fn f [n b] (loop [x n ds ()] (if (>= x b) (let [d (mod x b)] (recur (/ (- x d) b) (conj ds d))) (conj ds x)))))

(defcheck solution-4b674441
  (fn digitize [n b]
    (if (= n 0)
      '(0)
      (reverse
        (map
          last
          (rest (take-while #(not= [0 0] %)
                  (iterate
                    #(let [m (first %)] [(quot m b) (mod m b)])
                    [n nil]))))))))

(defcheck solution-4b696914
  (fn num-in-base-n
    [num n]
    ((fn num-in-base-n-helper
       [num s]
       (if (= 0 num)
         (if (seq s) (vec s) [0])
         (num-in-base-n-helper
           (quot num n)
           (cons (mod num n) s)))) num [])))

(defcheck solution-4b8f796
  (fn d+b
    ([n base] (d+b n base '()))
    ([n base digits]
     (if (= n 0)
       (if (empty? digits) [0] digits)

       (let [remainder (rem n base)]
         (recur (quot n base) base (conj digits remainder)))))))

(defcheck solution-4c306a1
  #(if (= 0 %1)
     [0]
     (
      (fn factor [m n acc]
        (if (= 0 n)
          acc
          (concat (factor m (quot n m) acc) [(mod n m)]))) %2 %1 [])))

(defcheck solution-4c6832ba
  (fn [number base]
    (if (= number 0) '(0)
                     (loop [n number
                            ans '()]
                       (if (> n 0)
                         (recur (quot n base) (conj ans (rem n base)))
                         ans
                         )
                       )
                     )
    ))

(defcheck solution-4ce42936
  (fn [n rdx]
    (letfn [(f [[x ret]] [(int (/ x rdx)) (conj ret (rem x rdx))])]
      (->>
        [n []]
        (iterate f)
        (drop 1)
        (drop-while #(pos? (first %)))
        first
        second
        reverse))))

(defcheck solution-4d35666
  (fn [num base]
    (loop [num num
           res '()]
      (if (zero? num)
        (if (empty? res) [0] (vec res))
        (recur (quot num base)
          (cons (rem num base) res))))))

(defcheck solution-4d5533c7
  (fn r[n b]
    (concat (if (< n b) [] (r (quot n b) b)) [(mod n b)])))

(defcheck solution-4dffc0f9
  (fn digital-base [x base]
    (if (< x base)
      [x]
      (conj (digital-base (quot x base) base)
        (mod x base)))))

(defcheck solution-4e3f7903
  (fn [x b]
    (reverse
      (map #(mod (quot x %) b) (cons 1 (take-while #(<= % x) (iterate #(* b %) b)))))))

(defcheck solution-4e43008f
  (fn [x r]
    (if (zero? x)
      [0]
      (loop [x x, digits []]
        (if (zero? x)
          (reverse digits)
          (let [[res rm] ((juxt #(quot % r) #(rem % r)) x)]
            (recur res (conj digits rm))))))))

(defcheck solution-4e4fc805
  (fn digits [n base]
    (if (>= n base)
      (conj (digits (quot n base) base) (rem n base))
      [n])))

(defcheck solution-4f09c704
  (fn calcDigits [num base]
    (letfn [(digit [n b] (dec (Math/ceil (/ (Math/log n) (Math/log b)))))]
      (cond (= 0 num) [0]
            (= num base) [1 0]
            :else (loop [n num d (digit num base) res []]
                    (let [factor (Math/pow base d) dig (quot n factor)]
                      (if (neg? d) res
                                   (recur (- n (* factor dig)) (dec d) (conj res (int dig))))))))))

(defcheck solution-4f09def2
  (fn mybases [n b]
    (if (= 0 (quot n b))
      [n]
      (conj (mybases (quot n b) b) (rem n b)))))

(defcheck solution-4f93da52
  (fn f [n b]
    (if (< n b) [n]
                (conj (f (quot n b) b)(mod n b) ))))

(defcheck solution-4fb7cbd5
  (fn digibases [no base]
    (let [step
          (fn [[x res]]
            [(quot x base) (conj res (mod x base))]
            )]
      (reverse
        (loop [[x res] [no []]]
          (if (zero? x)
            (if (empty? res) [0] res)
            (recur (step [x res]))
            )
          )
        ))))

(defcheck solution-502a586d
  (fn [n b]
    (loop [acc [] x n]
      (if (zero? x)
        (if (empty? acc) [0] (reverse acc))
        (recur (conj acc (mod x b)) (quot x b))))))

(defcheck solution-507b9d90
  (fn digits
    [n b]
    (letfn [(backwards-digits [n b]
              (lazy-seq
                (if (< n b)
                  [n]
                  (cons (rem n b) (backwards-digits (quot n b) b)))))]
      (reverse (backwards-digits n b)))))

(defcheck solution-508cd119
  (fn to-base [n b]
    (if-not (zero? (quot n b))
      (conj (to-base (quot n b) b) (rem n b) )
      [(rem n b) ])))

(defcheck solution-50b1c922
  (fn base-n [n b]
    (loop [res [] n n]
      (if (< n b) (conj res n)
                  (recur (cons (rem n b) res) (quot n b))))))

(defcheck solution-50cba28a
  (fn [num base]
    (if (zero? num) [0]
                    (if (= num base) [1 0]
                                     (loop [l (reverse
                                                (take-while #(> num %)
                                                  ((fn rec [n x]
                                                     (lazy-seq
                                                       (cons n (rec (* n x) x)))) 1 base)))
                                            r num a []]
                                       (if (empty? l) a
                                                      (let [y (first l)
                                                            z (if (>= r y)
                                                                (quot r y) 0)]
                                                        (recur (rest l) (- r (* z y)) (conj a z)))))))))

(defcheck solution-50f28f43
  (fn [val base]
    (loop [v val agg []]
      (let [m (mod v base)
            q (quot v base)]
        (if (= q 0)
          (cons m agg)
          (recur q (cons m agg)))))))

(defcheck solution-515266a
  (fn f [n b]
    (if (= n 0)
      [0]
      (let [p (quot (Math/log n) (Math/log b))]
        (loop [result []
               p p
               n n]
          (let [x (Math/pow b p)
                y (quot n x)
                z (* x y)
                result (conj result (int y))]
            (if (zero? p)
              result
              (recur result (dec p) (- n z)))))))))

(defcheck solution-517c7fa3
  (fn dab ([n b] (if (= n 0) [0] (dab [] n b)))
    ([res n b] (if (> n 0)
                 (let [d (mod n b)]
                   (dab (cons d res) (quot n b) b))
                 res))))

(defcheck solution-519dcddb
  (fn rebase [num base]
    (let [res ((fn rebs [n bs] (if (= n 0)
                                 []
                                 (conj (rebs (int (/ n bs)) bs )
                                   (rem n bs))))
               num base)]
      (if (empty? res)
        [0]
        res))))

(defcheck solution-51c1b01c
  (fn f [n base]
    (if (nil? n)
      []
      (let [a (quot n base)]
        (conj (f (if (zero? a) nil a) base) (rem n base))))))

(defcheck solution-52194466
  (fn [ thenum newbase ]
    (loop  [num   thenum
            res   [] ]
      (let [dig  (mod num newbase)
            next (int (/ num newbase))]
        (if (= next 0) (concat [dig] res)
                       (recur next (concat [dig] res)))))))

(defcheck solution-52fd5cac
  (fn [n r]
    (loop [c n out []]
      (if (= c 0)
        (reverse (if (empty? out) [0] out))
        (recur (quot c r) (conj out (mod c r)))))))

(defcheck solution-531423d4
  #((fn f [c a] (if (< c %2) (cons c a) (f (quot c %2) (cons (rem c %2) a)))) % '()))

(defcheck solution-5359a562
  (fn [num base]
    (loop [num num
           acc []]
      (let [[q r] [(quot num base) (rem num base)]]
        (if (zero? q)
          (cons r acc)
          (recur q (cons r acc)))))))

(defcheck solution-5367b398
  (fn [num base]
    (loop [num num digs []]
      (if (= num 0)
        (if (empty? digs) [0] (reverse digs))
        (recur (quot num base) (conj digs (mod num base)))))))

(defcheck solution-53715d66
  (fn [n b]
    (let [d (fn [u v] (/ (- u (mod u v)) v)) ]
      (loop [ x [] y (mod n b) z (d n b) ]
        (cond
          (= n 0) [0]
          (= b 0) [0]
          (= z 1) (cons z (cons  y x) )
          (= z 0) (cons y x)
          :else (recur (cons y x) (mod z b) (d z b)))))))

(defcheck solution-548064ec
  (fn [n b]
    (loop [n' n, r '()]
      (if (= n' 0) (if (empty? r) '(0) r)
                   (recur (quot n' b) (cons (rem n' b) r))))))

(defcheck solution-558b54fc
  (fn [n base]
    (reverse ((fn digits [n]
                (lazy-seq
                  (let [[q r] ((juxt quot rem) n base)]
                    (cons r (when (pos? q)
                              (digits q))))))
              n))))

(defcheck solution-55988dc2
  (fn [num base]
    (if (zero? num)
      [0]
      (first (reduce (fn [[a n] p]
                       (let [d (int (quot n p))]
                         [(conj a d) (- n (* d p))]))
               [[] num]
               (map #(Math/pow base %)
                 (range (quot (Math/log num) (Math/log base)) -1 -1)))))))

(defcheck solution-55bf61ae
  (fn __ [n base]
    (if (< n base)
      [n]
      (conj (__ (quot n base) base) (rem n base)))))

(defcheck solution-55c5f392
  (fn [n base]
    (loop [x n res []]
      (if (> base x) (reverse (conj res x))
                     (recur (quot x base) (conj res (rem x base)))))))

(defcheck solution-5665e1c0
  (fn [x y]
    (if (< x 1)
      [x]
      (loop [x x res []]
        (if (< x 1)
          res
          (recur (quot x y) (cons (mod x y) res)))))))

(defcheck solution-56996f35
  (fn [number base]
    (loop [n number
           digits ()]
      (if (< n base)
        (conj digits n)
        (let [digit (mod n base)]
          (recur (quot n base) (conj digits digit)))))))

(defcheck solution-56bce6fc
  (fn [x b]
    (
     (fn base [y]
       (if (< y b)
         [y]
         (conj (base (int (/ y b))) (rem y b))
         )
       )
     x
     )
    ))

(defcheck solution-56ce40ea
  (fn [a b] (if (= a 0) [0] (->> (iterate #(quot % b) a) (take-while pos?) (map #(mod % b)) reverse) )))

(defcheck solution-56ef49a6
  (fn d-b [n r]
    (loop [ x n  acc '()]
      (let  [q (quot x r )]
        (if (zero? q)
          (conj acc (rem x r))
          (recur q (conj acc (rem x r))))))))

(defcheck solution-57105db7
  (fn [digits base]
    (loop [curnum digits
           result []]
      #_(println)
      (let [nextnum (mod curnum base)
            remaining (/ (- curnum nextnum) base)
            newresult (concat [nextnum] result)]
        (if (zero? remaining)
          newresult
          (recur remaining newresult))))))

(defcheck solution-571bd3b1
  (fn [v n b]
    (if (= n 0)
      (if (empty? v) [0] v)
      (recur (cons (mod n b) v ) (quot n b) b))) [])

(defcheck solution-573838ed
  (fn digit-base
    [d b]
    (loop [id d
           res '()]
      (if (zero? id)
        (if (empty? res) [0] (into [] res))
        (recur (quot id b)
          (cons (rem id b) res))))))

(defcheck solution-573c9e8d
  #(if (= % 0)
     [0]
     (reverse (loop [i %
                     d []
                     c 0]
                (if (or (= i 0) (= c 100)) d
                                           (let [r (mod i %2)
                                                 ii (/ (- i r) %2)]
                                             (if (< r 0)
                                               (recur (inc ii) (conj d (+ r %2)) (inc c))
                                               (recur ii (conj d r) (inc c)))))))))

(defcheck solution-576e4bed
  (fn dig-and-base [n base]
    (if (< n base) [n]
                   (concat (dig-and-base (quot n base) base)[(rem n base)]))))

(defcheck solution-57bf8bea
  #(loop [digits nil pool %1]
     (if (= pool 0)
       (or digits [0])
       (recur (cons (rem pool %2) digits) (quot pool %2)))))

(defcheck solution-58138e6
  (fn p137 [n m]
    (if (zero? n)
      [0]
      (let [ns (reverse (take-while (fn [k] (<= k n)) (map (fn [l] (Math/pow m l)) (range))))]
        (letfn [(_ [ns n]
                  (if-not (seq ns)
                    []
                    (cons (int (quot n (first ns)))
                      (_ (rest ns) (rem n (first ns))))))]
          (_ ns n))))))

(defcheck solution-5846b8b4
  (fn [n b]
    (if (zero? n) [0]
                  (loop [m (int (/ (Math/log n) (Math/log b)))
                         n n
                         digits []]
                    (if (zero? m) (conj digits (int n))
                                  (recur (dec m) (mod n (Math/pow b m)) (conj digits (int (quot n (Math/pow b m))))))))))

(defcheck solution-58fbccc6
  (fn digits-base [n base]
    (loop [n n
           ans ()]
      (if (zero? n)
        (if (empty? ans)
          '(0)
          ans)
        (recur (int (/ n base)) (conj ans (mod n base)))))))

(defcheck solution-590bc399
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [res []
             n n]
        (if (zero? n)
          res
          (recur (cons (mod n base) res)
            (quot n base)))))))

(defcheck solution-59146233
  (fn [n ratio]
    (if (zero? n) [n]
                  (loop [r () v n]
                    (if (zero? v) r
                                  (recur (conj r (rem v ratio)) (quot v ratio)))))))

(defcheck solution-5abb36f9
  (fn [n base]
    (loop [result '() n n]
      (if (> base n)
        (cons n result)
        (let [quotient  (quot n base)
              remainder (rem n base)]
          (recur (cons remainder result) quotient))))))

(defcheck solution-5adcb60b
  #(if (zero? %) [0]
                 (ffirst
                   (drop-while (comp not zero? second)
                     (iterate (fn [[r x]] [(conj r (rem x %2)) (quot x %2)])
                       ['() %])))))

(defcheck solution-5afa1940
  (fn [n b]
    (loop [n n a []]
      (cond (zero? n) (if (= a []) [0] (reverse a))
            :else (recur (quot n b) (conj a (mod n b)))))))

(defcheck solution-5b0dafba
  (fn
    [n b]
    (reverse
      (loop
       [digits [] left n]
        (if
         (<= left 0)
          (if (empty? digits) [0] digits)
          (recur (conj digits (rem left b)) (quot left b)))))))

(defcheck solution-5b637d2
  (fn [n d] (loop [r n dig (if (zero? n) [0] [])] (if (zero? r) (reverse dig) (recur (int (/ r d)) (conj dig (mod r d)))))))

(defcheck solution-5b74f7cb
  (fn[n b]
    (let [f (fn f [m] (if (= m 0) [] (conj (f (quot m b)) (mod m b))))]
      (if (= n 0) [0] (f n)))))

(defcheck solution-5b92b81b
  (fn convert [n b]
    (if (< n b)
      [n]
      (conj (convert (quot n b) b) (mod n b)))))

(defcheck solution-5cc1845d
  (fn [n base]
    (loop [n n
           num-list '()]
      (if (zero? n)
        (if (empty? num-list)
          '(0)
          num-list)
        (recur (int (/ n base))
          (cons (mod n base) num-list))))))

(defcheck solution-5cdc8d5
  (fn convert [num b]
    (loop [n num
           r []]
      (if (zero? n)
        (if (empty? r)
          [0]
          (reverse r))
        (recur (quot n b)
          (conj r (rem n b)))))))

(defcheck solution-5d07ffe
  (fn f [ m b ]
    (if (<= b m)
      (conj (f (quot m b) b) (rem m b))
      [ m ])))

(defcheck solution-5d2516cf
  (fn num-base[n b]
    (lazy-cat
      (lazy-seq (if (zero? (quot n b)) [] (num-base (quot n b) b)))
      [(rem n b)])))

(defcheck solution-5d51c5b4
  (fn f
    ([n r] (f n r r))
    ([n r s]
     (let [o (mod n r)
           p (- n o)]
       (conj
         (if (> p 0)
           (f p (* r s) s) [])
         (/ o (/ r s)))))))

(defcheck solution-5d6b8097
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [n n, base base, r '()]
        (if (pos? n)
          (recur (quot n base) base (cons (rem n base) r))
          r)))))

(defcheck solution-5dbb064c
  (fn p [n b]
    (if (< n b) [n]
                (conj (p (quot n b) b) (mod n b)))))

(defcheck solution-5dc13c7
  (fn [num base]
    (if (zero? num)
      [0]
      (loop [num num res nil]
        (if (zero? num)
          res
          (recur (quot num base) (cons (rem num base) res)))))))

(defcheck solution-5dfcb1fb
  (comp
   reverse
   (fn digits [n b]
     (lazy-seq
       (cons
         (mod n b)
         (let [n (quot n b)]
           (when-not (zero? n)
             (digits n b))))))))

(defcheck solution-5e293b5d
  (fn [n b]
    (loop [n n c ()]
      (let [d (int (/ n b))
            k (conj c (rem n b))]
        (if (zero? d)
          k
          (recur d k))))))

(defcheck solution-5eb7f9d1
  (fn [x r] (if (zero? x) [0] (loop [x x, digits []] (if (zero? x) (reverse
                                                                     digits) (let [[res rm] ((juxt #(quot % r) #(rem % r)) x)] (recur res (conj
                                                                                                                                            digits rm))))))))

(defcheck solution-5efa7557
  (fn digits
    ([n b acc]
     (if (zero? n)
       (if (empty? acc) [0] acc)
       (recur (quot n b) b (conj acc (mod n b)))))
    ([n b] (digits n b (list)))))

(defcheck solution-5f6f7ef1
  #(if (zero? %) [0]
                 (loop [n % d ()]
                   (if (zero? n) d
                                 (recur (quot n %2) (cons (mod n %2) d))))))

(defcheck solution-5f981280
  (fn f [x b]
    (if (< x b) [x]
                (conj (f (int (/ x b)) b) (mod x b)))))

(defcheck solution-5fa6cf01
  (fn f [x b]
    (if (> b x)
      [x]
      (conj (f (quot x b) b) (mod x b)))))

(defcheck solution-5fa9c357
  (fn convert [n b]
    (let [radices (iterate (partial * b) 1)
          pow (count (take-while (partial not= 0) (map (partial quot n) radices)))
          divs (reverse (take pow radices))
          rems (reductions #(rem % %2) n divs)
          digits (map quot rems divs)]
      (or (seq digits) [0]))))

(defcheck solution-5fb376f4
  (fn [val base]
    (loop [cval val
           acc nil]
      (if (zero? cval)
        (or acc [0])
        (recur (quot cval base) (cons (mod cval base) acc))))))

(defcheck solution-5fb62203
  (fn get-digits
    ([x base] (get-digits x base []))
    ([x base digits]
     (let [remainder (int (rem x base))
           new-x (Math/floor (/ x base))]
       (if (== 0 new-x) (cons remainder digits)
                        (get-digits new-x base (cons remainder digits)))))))

(defcheck solution-6040293d
  (fn p137
    ([n b] (if (= n 0) [0] (p137 [] n b)))
    ([ls n b] (if (= n 0) (reverse ls)
                          (p137 (conj ls (mod n b)) (int (/ n b)) b)))))

(defcheck solution-609da576
  (fn [n  b]
    (if (zero? n) [0]
                  (loop [i n d []]
                    (if (zero? i) d
                                  (recur (quot i b) (cons (mod i b) d))))
                  )))

(defcheck solution-60b96a92
  (fn [n base]
    (if (zero? n)
      [0]
      (let [divvy (take-while #(<= 1 %) (iterate #(int (/ % base) ) n))
            digbackw (map #(rem % base) divvy)]
        (reverse digbackw)))))

(defcheck solution-60c34d8a
  (fn [n base]
    (if (zero? n)
      '(0)
      (loop [n n
             r ()]
        (if (zero? n)
          r
          (recur (int (/ n base)) (conj r (mod n base))))))))

(defcheck solution-60f12a2e
  (fn [x b] (if (zero? x) [x] (loop [n x acc (list)] (if (zero? n) (into [] acc) (recur (quot n b) (conj acc (rem n b))))))))

(defcheck solution-60f57772
  (fn [x b]
    (loop [x x out []]
      (if (< x b)
        (cons x out)
        (let [d (mod x b)]
          (recur (/ (- x d) b) (cons d out)))))))

(defcheck solution-6157704d
  (fn [n b]
    (let [f (fn s [n b] (concat
                         (if (zero? (quot n b))
                           []
                           (s (quot n b) b))
                         [(mod n b)]))]
      (f n b))))

(defcheck solution-6170b703
  (fn [ds n r]
    (let [d (conj ds (mod n r))
          rm (int (/ n r))]
      (if (= 0 rm) d (recur d rm r)))) '())

(defcheck solution-61935d44
  (fn [number div]
    (loop [s ()
           n number]
      (let [[q r] [(quot n div) (rem n div)]]
        (if (zero? q)
          (cons r s)
          (recur (cons r s) q))))))

(defcheck solution-61dd0d46
  (fn digits-with-base [n b]
    (if (= 0 n) [0]
                (loop [result () queue n]
                  (if (= 0 queue)
                    result
                    (recur
                      (conj result (mod queue b))
                      (quot queue b)))))))

(defcheck solution-61ec307
  (fn [n b] (if (= n 0) [0] (into [] (reverse (map #(mod % b) (for [x  (iterate #(int (/ % b)) n) :while (> x 0)] x)))))))

(defcheck solution-6257b522
  #((fn f [q r]
      (if (= 0 q)
        ({[] [0]} r r)
        (f (quot q %2) (cons (rem q %2) r))))
    % '()))

(defcheck solution-62614bc3
  (fn [n r]
    (loop [n n d ()]
      (if (< n r) (conj d n)
                  (recur (quot n r) (conj d (rem n r)))))))

(defcheck solution-62d8896a
  (fn in-radix
    [n radix]
    (if (zero? n)
      [0]
      (let [div (quot n radix)]
        (conj (if (= 0 div) [] (in-radix (quot n radix) radix)) (mod n radix))))))

(defcheck solution-62e2e859
  (fn [nu b]
    (into [] (reverse (loop [n nu stock []]
                        (if (> (quot n b) 0)
                          (recur (quot n b) (conj stock (rem n b)))
                          (conj stock (rem n b))
                          )
                        )))))

(defcheck solution-6312ade2
  (fn __ [n b]
    (let [x (int (/ n b))]
      (concat
       (when (not= 0 x)
         (__ x b))
       [(mod n b)]))))

(defcheck solution-63713540
  (fn [n base]
    (loop [out '() n n]
      (if (< n base)
        (conj out n)
        (recur
          (conj out (rem n base))
          (quot n base))))))

(defcheck solution-639cd33b
  (fn [i b] (if (zero? i) [i] (map last (reverse (rest (take-while (fn [[x y]] (not= x y 0)) (iterate (fn [[x]] [(quot x b) (rem x b)]) [i]))))))))

(defcheck solution-63a0be3f
  (fn dab [n base]
    (if (< n base) [n]
                   (let [next-digit (mod n base)
                         subt (- n next-digit)
                         next-n (/ subt base)]
                     (conj (dab next-n base) next-digit)))))

(defcheck solution-63f16c5d
  (fn f [d b]
    (if (< d b)
      [d]
      (conj (f (quot d b) b) (mod d b)))))

(defcheck solution-6477f52c
  (fn [n b]
    (loop [m (quot n b)
           r (mod n b)
           acc '()]
      (if (= m 0)
        (cons r acc)
        (recur (quot m b) (mod m b) (cons r acc))))))

(defcheck solution-649b9763
  (fn t [n b] (conj (if (< n b) [] (t (quot n b) b)) (mod n b))))

(defcheck solution-64aee5e
  (fn num-seq
    ([num base]
     (if (= num 0) '(0)
                   (let [p (int (/ (Math/log num) (Math/log base)))]
                     (num-seq num base p))))
    ([num base p]
     (if (neg? p) nil
                  (let [d (int (Math/pow base p))
                        q (quot num d)
                        r (rem num d)]
                    (cons q (num-seq r base (dec p))))))))

(defcheck solution-6542cbee
  (fn [n b]
    (letfn [(worker [s n]
              (if (zero? n)
                s
                (recur (conj s (mod n b)) (quot n b))))]
      (if (zero? n) (list 0)
                    (worker '() n)))))

(defcheck solution-6599636a
  (fn base [n b]
    (if (< n b) [n]
                (let [d (mod n b)]
                  (conj (base (/ (-	n d) b) b) d)))))

(defcheck solution-659c5a59
  (fn r [s n b] (if (= 0 (quot n b)) (conj s (mod n b)) (conj (r s (quot n b) b) (mod n b)))) [])

(defcheck solution-65a080c5
  (fn [n base]
    (loop [digits ()
           dividend n]
      (let [remainder (mod dividend base)]
        (if (< dividend base)
          (conj digits dividend)
          (recur
            (conj digits remainder)
            (/ (- dividend remainder) base)))))))

(defcheck solution-65a49c1e
  (fn [n b]

    (if (zero? n) [0]
                  (loop [i (quot (Math/log n) (Math/log b))
                         n n
                         lst []
                         j (Math/pow b i)
                         ]
                    (if (>= i 0)
                      (recur (dec i) (rem n j) (conj  lst (int (quot n j))) (Math/pow b (dec i)) )
                      lst )

                    ))))

(defcheck solution-65a68f18
  (fn [n b]
    (loop [n n result '()]
      (if (< n b)
        (conj result n)
        (recur (quot n b) (conj result (mod n b))))
      )))

(defcheck solution-65d68b3
  (fn [numb base]
    (
     (fn rebase [accum numb]
       (if (= numb 0)
         (if (empty? accum)
           [0]
           accum
           )
         (recur (conj accum (rem numb base)) (quot numb base))
         )
       )'() numb
     )
    ))

(defcheck solution-65f2f20a
  (fn [n b]
    (loop [x n res '()]
      (let [q (quot x b)
            m (mod x b)]
        (if (= 0 q)
          (cons m res)
          (recur q (cons m res)))))))

(defcheck solution-67099495
  (fn to-digits [n b]
    (loop [n n, acc ()]
      (let [q (quot n b), r (rem n b), acc (cons r acc)]
        (if (zero? q)
          acc
          (recur q acc))))))

(defcheck solution-671cf4d7
  (fn [n r]
    (loop [n n c nil]
      (if (zero? n)
        (or c [0])
        (recur (quot n r) (conj c (rem n r)))))))

(defcheck solution-6762d535
  (fn [i b]
    (if (= i 0)
      [0]
      (loop [r i n [] x (last (take-while #(<= % i) (iterate #(* % b) 1)))]
        (if (<= x 1)
          (conj n r)
          (recur (mod r x) (conj n (quot r x)) (/ x b)))))))

(defcheck solution-679cff6f
  (fn [num base]
    (loop [n num ret []]
      (if (zero? n)
        (if (empty? ret) [0] ret)
        (recur (quot n base) (cons (rem n base) ret))))))

(defcheck solution-67cd7488
  (fn [n b]
    (reverse
      (map #(mod (quot n %) b)
        (cons 1 (take-while (partial >= n) (iterate #(* % b) b)))))))

(defcheck solution-67d6bff7
  (fn solve [n base]
    (letfn [(iter [exp]
              (if (> exp n)
                []
                (conj (iter (* exp base))
                  (quot (rem n (* exp base)) exp))))]
      (if (zero? n) [0] (iter 1)))))

(defcheck solution-67f486e4
  (fn [x n]
    (letfn [(f [x n]
              (if (= 0 x) [] (conj (f (quot x n) n) (mod x n))))]
      (if (= 0 x) [0] (f x n)))))

(defcheck solution-6800fb67
  (fn [numb base]
    (letfn
     [(go [n ds]
        (let [q (quot n base)
              r (rem n base)]
          (if (= 0 q)
            (cons r ds)
            (go q (cons r ds)))))]
      (go numb [])
      )))

(defcheck solution-6814f0a4
  (fn f
    ([n b] (if (zero? n) [0] (f n b ())))
    ([n b acc]
     (if (zero? n)
       acc
       (recur (quot n b) b (conj acc (mod n b)))))))

(defcheck solution-6827b27b
  (fn base-change [n b]
    (if (< n b) [n]
                (conj (base-change (quot n b) b) (rem n b)))))

(defcheck solution-685ff2c
  (fn [n base]
    (loop [n n result ()]
      (if (< n base)
        (cons n result)
        (recur (quot n base) (cons (rem n base) result))))))

(defcheck solution-6889d1c0
  (fn f[val base]
    (if (< val base)
      (vector val)
      (conj  (f (quot val base) base)  (mod val base) )
      )
    ))

(defcheck solution-68999330
  (fn to-base
    [n base]
    (loop [digits '()
           n n]
      (if (zero? (int (/ n base)))
        (conj digits (rem n base))
        (recur (conj digits (rem n base)) (int (/ n base)))))))

(defcheck solution-6909d80a
  (fn digits [n base]
    (loop [nn n result []]
      (if (zero? (quot nn base))
        (cons (rem nn base) result)
        (recur (quot nn base) (cons (rem nn base) result))
        )
      )
    ))

(defcheck solution-692318a2
  #(let [digits (fn d [n b] (if (zero? n) [] (conj (d (quot n b) b) (rem n b))))
         d (digits %1 %2)]
     (if (empty? d) [0] d)))

(defcheck solution-6974dd90
  (fn f
    ([n b] (f n b [0]))
    ([n b o] (if (= 0 n) o (conj (f (quot n b) b []) (mod n b))))))

(defcheck solution-69a478f2
  (fn [n b]
    (loop [q n
           res '()]
      (if (= 0 (quot q b))
        (conj res (rem q b))
        (recur (quot q b) (conj res (rem q b))))
      )
    ))

(defcheck solution-69b6e635
  (fn [n b]
    (loop [n n lst nil]
      (if (zero? n) (if lst lst [0])
                    (recur (quot n b) (cons
                                        (mod n b)
                                        lst))))))

(defcheck solution-69c51157
  (fn [x b]
    (loop [x x, acc []]
      (if (< x b)
        (cons x acc)
        (recur (quot x b) (cons (mod x b) acc))))))

(defcheck solution-69d0fd15
  (fn [n b]
    (if (= n 0) [0]
                (loop [x n v ()]
                  (if (= 0 x)
                    v
                    (recur (quot x b) (conj v (mod x b))))))))

(defcheck solution-6b78fd01
  (fn digits-and-bases [n base]
    (if (< n base)
      [n]
      (conj (digits-and-bases (quot n base) base) (rem n base)))))

(defcheck solution-6bb6c8df
  (fn to-base
    ([n b] (if (zero? n) [0] (to-base n b [])))
    ([n b a]
     (if (zero? n) a
                   (recur (int (/ n b)) b (cons (mod n b) a))))))

(defcheck solution-6c22fc96
  #(loop [x % b %2 acc []]
     (let [q (quot x b) nacc (cons (rem x b) acc)]
       (if (zero? q) nacc (recur q b nacc)))))

(defcheck solution-6c40a748
  (fn [n b]
    (if (= n 0)
      '(0)
      (loop [n n
             r ()]
        (if (= n 0)
          r
          (recur (quot n b)
            (conj r (mod n b))))))))

(defcheck solution-6cfa5fa3
  (fn [v10 base]
    (loop [v10 v10
           digs []]
      (if (< v10 base)
        (reverse (conj digs v10))
        (let [m (mod v10 base)]
          (recur (/ (- v10 m) base) (conj digs m)))))))

(defcheck solution-6d18c28a
  (fn digits [number base]
    (if (= number 0)
      [0]
      (let [ch (concat (map char (range 48 58)) (map char (range 65 91)))
            dig (zipmap ch (range))]
        (loop [num number
               acc []]
          (if (zero? num)
            (map #(dig %) (reverse acc))
            (recur (int (/ num base))
              (conj acc (nth ch (mod num base))))))))))

(defcheck solution-6d1bbe8c
  (fn [n b]
    (loop [i n r '()]
      (if (and (= i 0) (not (empty? r)))
        r
        (recur (int (/ i b)) (conj r (mod i b)))))))

(defcheck solution-6da50c2c
  (fn [x b] (if (= 0 x) [0] (reverse (map #(rem % b) (take-while pos? (iterate #(quot % b) x)))))))

(defcheck solution-6df8c2be
  (fn [n r]
    (let [p (int (dec (first (filter #(< n (Math/pow r %)) (range)))))
          fun (fn fun [cp num]
                (if (or (zero? cp) (zero? num))
                  [num]
                  (let [cpp (int (Math/pow r cp))
                        digit (int (/ num cpp))]
                    (cons digit (fun (dec cp) (- num (* cpp digit)))))))]
      (fun p n))))

(defcheck solution-6e4c99f5
  (fn [num base]
    (loop [n num
           acc '()]
      (let [[q r] ((juxt quot rem) n base)]
        (if (zero? q)
          (cons r acc)
          (recur q (cons r acc)))))))

(defcheck solution-6e8e406d
  #(last
     (last
       (take-while (fn [[n v]] (or (<= (count v) 1) (not= (first v) n)))
         (iterate (fn[[n r]] [(int (/ n %2)) (cons (mod n %2) r)]) [% []])))))

(defcheck solution-6e9bb6d
  (fn f [n b]
    (let [q (quot n b)
          r (rem n b)]
      (if (zero? q)
        [r]
        (conj (f q b) r)))))

(defcheck solution-6eb11d3b
  (fn f [n b]
    (if (zero? n) [0] (reduce #(conj %1 (rem %2 b)) '() (take-while (comp not zero?) (iterate #(quot %1 b) n))))
    ))

(defcheck solution-6ed02bde
  (fn [n b]
    (loop [n n
           acc '()]
      (if (zero? n)
        (if (empty? acc) [0] acc)
        (recur (quot n b) (conj acc (mod n b)))))))

(defcheck solution-6ee72329
  (fn [n base]
    (loop [x n
           digits nil]
      (if (< x base)
        (conj digits x)
        (let [d (mod x base)]
          (recur (quot (- x d) base)
            (conj digits d)))))))

(defcheck solution-6efcf6ec
  (fn [n radix]
    (if (zero? n)
      '(0)
      (reverse
        (map #(rem % radix)
          (take-while (partial < 0)
            (iterate #(quot % radix) n)))))))

(defcheck solution-6f8b5605
  (fn [n b]
    (loop [n n o '()]
      (let [x (quot n b)
            y (rem n b)
            o (conj o y)]
        (if (= x 0)
          o
          (recur x o))))))

(defcheck solution-6f8de201
  (fn f [n b]
    (if (zero? n)
      [0]
      (loop [n n
             res '()]
        (if (zero? n)
          res
          (recur (quot n b) (cons (rem n b) res)))))))

(defcheck solution-6f9a9716
  (fn [num base] (letfn [(to-base [remainder base denom div acc]
                           (if (zero? remainder) acc
                                                 (let [sub (rem remainder denom)
                                                       digit (/ sub div)]
                                                   (to-base (- remainder sub) base (* denom base) denom (cons digit acc)))))]
                   (if (zero? num) [0]
                                   (to-base num base base 1 [])))))

(defcheck solution-6fc2c53a
  (fn base [n b]
    (if (>= n b)
      (conj (base (quot n b) b) (mod n b))
      [n])))

(defcheck solution-702cd5e8
  (fn digits [n base]
    (if (>= n base)
      (concat (digits (int (/ n base)) base) [(mod n base)])
      [n])))

(defcheck solution-7041fa4
  (fn to-base [n base]
    (loop [m n digits ()]
      (if (< m base)
        (cons m digits)
        (recur (quot m base)
          (cons (rem m base) digits))))))

(defcheck solution-70506b80
  (fn base-digits [n b] (if (zero? n) [0] (loop [n n digits []] (if (zero? n) (vec digits) (recur (int (/ n b)) (cons (rem n b) digits)))))))

(defcheck solution-708d6c0c
  #(if (zero? %1) [0] (
                       (fn f [[n b r]] (if (zero? n) r (f [(quot n b) b (conj r (mod n b))])))
                       [%1 %2 '()])))

(defcheck solution-70b8d11b
  (fn [n b] (if (zero? n) [0]
                          (->> n
                            (iterate #(quot % b))
                            (take-while (comp not zero?))
                            (reverse)
                            (map #(mod % b))))))

(defcheck solution-71027881
  (fn [n b]
    (if (= n 0)
      [0]
      ((fn f[n b] (if (= n 0) [] (conj (f (quot n b) b) (mod n b)))) n b))))

(defcheck solution-71204106
  (fn dab [i base]
    (if (< i base)
      [i]
      (conj (dab (quot i base) base) (mod i base)))))

(defcheck solution-713ff70f
  (fn [nn bb]
    (letfn [(! [n b]
              (if (< n b)
                (seq (list n))
                (conj (! (quot n b) b) (rem n b) )
                )
              )]
      (reverse (! nn bb)))))

(defcheck solution-716277fe
  (fn T [n b]
    (if (< n b)
      [n]
      (conj (T (int (/ n b)) b) (mod n b)))))

(defcheck solution-718b0fce
  (fn [v base]
    (letfn
     [(find-start [start]
        (if (< v (* start base))
          start
          (find-start (* start base))))]
      (loop [acc [] div (find-start 1) val v]
        (if (= 1 div)
          (conj acc val)
          (recur
            (conj acc (quot val div))
            (/ div base)
            (mod val div)))))))

(defcheck solution-719b5679
  (fn base-digits [num base]
    (if (< num base)
      [num]
      (conj (base-digits (quot num base) base)
        (rem num base)))))

(defcheck solution-71bda023
  (fn digits-base-by [n base]
    (loop [n n r []]
      (if (< n base)
        (cons n r)
        (recur (quot n base) (cons (rem n base) r))))))

(defcheck solution-71f592fe
  (fn p137 [n b]
    (loop [n n digits ()]
      (if (zero? n)
        (if (empty? digits) [0] digits)
        (recur (quot n b) (conj digits (rem n b)))))))

(defcheck solution-7279a3d5
  (fn digits
    [a b]
    (if (zero? a) [0]
                  (loop [rst a
                         result '()]
                    (if (zero? rst) result
                                    (recur (quot rst b) (conj result (mod rst b))))))))

(defcheck solution-7302fec2
  (fn radix-vec [i b]
    (if (= i 0) [0]
                (letfn [(body [i b]
                          (if (= i 0) '()
                                      (cons (mod i b) (body (int (/ i b)) b))))]
                  (reverse (body i b))))))

(defcheck solution-73ee66fb
  (fn digits-and-bases [n base]
    (if (< n base)
      [n]
      (conj (digits-and-bases (/ (- n (mod n base)) base) base)
        (mod n base)))))

(defcheck solution-7415d6ed
  (letfn [(places [n b]
            (if (= n 0)
              [1]
              (->> (iterate #(* % b) 1)
                (take-while #(<= % n))
                (reverse))))]
    (fn digits-in-base [n b]
      (first
        (reduce (fn [[acc i] p]
                  [(conj acc (quot i p)) (rem i p)])
          [[] n] (places n b))))))

(defcheck solution-7478cae9
  (fn [x b]
    (loop [x x, out ()]
      (if (zero? x)
        (if (empty? out) [0] out)
        (recur (quot x b) (conj out (mod x b)))))))

(defcheck solution-74a16c9a
  (fn [n r]
    (map #(.indexOf "0123456789abcdefghijklmnopqrstuvwxyz" (str %))
      (clojure.pprint/cl-format nil "~vr" r n))))

(defcheck solution-750211dd
  (fn base
    ([n b] (base n b '()))
    ([n b digs]
     (if (< n b)
       (cons n digs)
       (recur (quot n b)
         b
         (cons (rem n b) digs))))))

(defcheck solution-75630bd6
  (fn digits [n b]
    (reverse
      (loop [r n
             d []]
        (if (zero? r)
          (if (empty? d) [0] d)
          (recur (quot r b) (conj d (mod r b))))))))

(defcheck solution-770f2e34
  (fn to-base [n b]
    (loop [d () m n]
      (let [q (quot m b) r (rem m b) e (conj d r)]
        (if (= q 0)
          e
          (recur e q))))))

(defcheck solution-77cdd1b5
  #(or (loop [x % r nil]
         (if (= 0 x) r
                     (recur (quot x %2) (cons (mod x %2) r)))) [0]))

(defcheck solution-77d96bc4
  (fn [value base]
    (let [pow (memoize (fn [p] (apply * (repeat p base)))) ; base ^ p
          digit-at (memoize (fn [value column] (int (/ value (pow column)))))]
      (loop [column (or (last (take-while #(<= (pow %) value ) (range 19))) 0)
             digits []
             value value]
        (if (= -1 column) digits
                          (recur (dec column)
                            (conj digits (digit-at value column))
                            (- value (* (pow column) (digit-at value column)))))))))

(defcheck solution-77ef5e17
  (fn me [num base]

    (loop

     [cur num res (list 0) cur-base base last-base 1]

      (let [
            coefficient (rem cur cur-base)

            next-cur (- cur coefficient)

            next-base (* cur-base base)

            next-res (cons (quot coefficient last-base) res)
            ]


        (if (zero? (quot cur cur-base) )

          (drop-last next-res)

          (recur next-cur next-res next-base cur-base)
          )

        )

      )

    ))

(defcheck solution-78caa639
  (fn [x b] (if (zero? x) [0] (loop [result [] l x] (if (zero? l) result (let [d (mod l b) r (quot l b)](recur (cons d result) r)))))))

(defcheck solution-78dc2f6c
  (fn [x b]
    (reverse (loop [x x acc []]
               (if (= x 0)
                 (if (= acc [])
                   [0]
                   acc)
                 (recur (quot x b) (conj acc (rem x b))))))))

(defcheck solution-7a4101b3
  (fn nb [num base]
    (if (< num base)
      [num]
      (conj (nb (int (/ num base)) base) (rem num base)))))

(defcheck solution-7a6643ec
  (fn digit-seq [n b] {:pre [(>= n 0) (>= b 0)]}
    (if (zero? n)
      [0]
      (let [s (conj (digit-seq (quot n b) b)
                (rem n b))]
        (if (zero? (first s))
          (subvec s 1)
          s)))))

(defcheck solution-7a8a0b87
  (fn [n b] (loop
             [m n
              result []]
              (let
               [x (quot m b)
                y (rem m b)]
                (if (< x b)
                  ((if (> x 0) (partial cons x) identity) (cons y result))
                  (recur x (cons y result)))))))

(defcheck solution-7a8bb633
  #(loop [in % base %2 ret ()]
     (if (= in 0)
       (or (seq ret) '(0))
       (recur (quot in base) base (cons (mod in base) ret)))))

(defcheck solution-7bba34f4
  (fn f [x n]
    (let [q (quot x n)]
      (conj (if (= 0 q) [] (f q n)) (rem x n)))))

(defcheck solution-7c0e593c
  (fn digits-bases
    [x b]
    (reverse (loop [x x
                    b b
                    xs []]
               (if (< x b)
                 (conj xs x)
                 (recur (quot x b) b (conj xs (rem x b))))))))

(defcheck solution-7c4bdc24
  (fn digits
    ([n base] (if (= n 0)
                [0]
                (digits n base '())))
    ([n base out]
     (if (= n 0)
       out
       (let [d (rem n base)
             r (quot n base)]
         (recur r base (cons d out)))))))

(defcheck solution-7ce72e32
  (fn [n base]
    (letfn [(get-digits [ls num]
              (if (< num base)
                (conj ls num)
                (recur (conj ls (rem num base)) (quot num base))))]
      (reverse (get-digits [] n)))))

(defcheck solution-7d32aa1f
  (fn brington[c n]
    (let [j (fn l[c n](if (> c 0) (cons (mod c n) (l (quot c n) n))))]
      (if (= 0 c)
        [0]
        (vec (reverse (j c n))))
      )
    ))

(defcheck solution-7d75e7c9
  (fn [n r] (let [f (fn f [n r] (cons (rem n r) (when (not= 0 (quot n r)) (f (quot n r) r))))]
              (reverse (f n r)))))

(defcheck solution-7e35f7d2
  (fn d [n b]
    (if (< n b)
      [n]
      (conj
        (d (quot n b) b)
        (mod n b)))))

(defcheck solution-7e3d6be9
  (fn bs [n b]
    (if (< n b)
      [n]
      (conj (bs (quot n b) b) (mod n b)))))

(defcheck solution-7f45b29f
  (fn digits-in-base [n base]
    (letfn [
            (digits- [n]
              (if
               (zero? n)
                []
                (cons
                  (rem n base)
                  (lazy-seq (digits- (quot n base))))
                )
              )]
      (if
       (zero? n)
        [0]
        (reverse (digits- n))
        )
      )
    ))

(defcheck solution-7f812db0
  (fn [n b]
    {:pre [((complement neg?) n)
           (<= 2 b)]}
    (if (zero? n)
      [0]
      (rseq
        (loop [x n, acc []]
          (if (zero? x)
            acc
            (let [q (quot x b)
                  r (rem x b)
                  acc' (conj acc r)]
              (recur q acc'))))))))

(defcheck solution-7ffdc0ca
  (fn [n b]
    (if (zero? n) [0] (reverse (map #(rem % b) (take-while (complement zero?) (iterate #(quot % b) n)))))))

(defcheck solution-803fdcc1
  (fn f
    ([n b] (f n b []))
    ([n b a]
     (let [q (quot n b)
           m (mod n b)
           r (cons m a)]
       (if (= 0 q)
         r
         (f q b r))))))

(defcheck solution-80488829
  (->> (quot n b)
    (iterate #(quot % b))
    (take-while pos?)
    (cons n)
    (map #(rem % b))
    (reverse)
    (fn [n b])))

(defcheck solution-80d54fdd
  (fn digits[n base]
    (loop [result (list (mod n base)) remainder (quot n base)]
      (if (zero? remainder)
        result
        (recur
          (conj result (mod remainder base))
          (quot remainder base))))))

(defcheck solution-81d84227
  (fn conv-base [x base]
    (loop [acc () x' x]
      (if (zero? x')
        (if (empty? acc) '(0) acc)
        (recur (conj acc (rem x' base)) (quot x' base))))))

(defcheck solution-81fbd2dc
  (fn ! [n base]
    (if (< n base)
      [n]
      (conj (! (quot n base) base)
        (mod n base)))))

(defcheck solution-82445a6c
  (fn digit
    ([num base] (digit num base ()))
    ([num base digits]
     (let [digits (conj digits (mod num base))]
       (if (< num base)
         digits
         (recur (quot num base) base digits))))))

(defcheck solution-829704f3
  (fn get-digits [num base]
    (if (zero? num)
      [0]
      ((fn digits [num]
         (when-not (zero? num)
           (concat (digits (quot num base))
                   [(mod num base)])))
       num))))

(defcheck solution-833e440
  (fn f [x b]
    (if (zero? x)
      [0]
      (loop [x x
             s []]
        (if (zero? x)
          (reverse s)
          (recur (int (/ x b)) (conj s (rem x b)))
          )))))

(defcheck solution-83522c5b
  (fn konv [x y]
    (let [f (atom x) v (atom [])]
      (do (while (pos? @f)
            (do (swap! v conj (rem @f y))
                (reset! f (quot @f y))))
          (if (empty? @v) [0] (reverse @v)))) ))

(defcheck solution-8369db3c
  (fn [n b]
    (loop [v [] number n]
      (if (zero? (quot number b))
        (cons (rem number b) v)
        (recur
          (cons (rem number b) v)
          (quot number b))))))

(defcheck solution-8379a744
  (fn [n b]
    (if (= 0 n) [0]
                (loop [n n
                       r ()]
                  (if (= 0  n)  r
                                (recur (quot n b) (cons (mod n b) r)))))))

(defcheck solution-83863bd5
  #(loop [n %1, result ()]
     (if (= 0 n)
       (if (empty? result) '(0) result)
       (recur (quot n %2) (conj result (rem n %2))))))

(defcheck solution-83945dfb
  (fn f [n b]
    (if (< n b)
      [n]
      (conj
        (f (quot n b) b)
        (rem n b)))))

(defcheck solution-849a71c3
  (fn bconv [n base]
    (if (== n base)
      [1 0]
      (if (== n 0)
        [0]
        (let [ipow (fn [x n] (->> x repeat (take n) (apply *)))
              powers (fn [base] (map  (partial ipow (* 1N base)) (range)))
              take-powers (fn [base n] (vec (reverse (take-while #(< % n) (powers base)))))]
          (first
            (reduce
              (fn [[result remainder] power]
                [(conj result (int (quot remainder power)))
                 (rem remainder power)])
              [[] n]
              (take-powers base n))))))))

(defcheck solution-849b01bf
  (fn digits
    [n b]
    (if (zero? n)
      [0]
      (letfn [(digits-reversed
                [n b]
                (if (> n 0)
                  (cons (mod n b) (digits-reversed (quot n b) b))
                  []))]
        (reverse (digits-reversed n b))))))

(defcheck solution-8549926a
  (fn base-change
    ([x b]
     (if (zero? x)
       [ 0]
       (base-change x b '()))
     )
    ([x b digits]
     (if (zero? x)
       digits
       (recur (quot x b) b (conj digits (rem x b)))))
    ))

(defcheck solution-85e14471
  (fn digits [x base]
    (if (zero? x)
      [0]
      (loop [y x, result '()]
        (if (zero? y)
          (apply vector result)
          (recur (quot y base) (conj result (rem y base))))))))

(defcheck solution-868a6d54
  (fn [x base]
    (loop [d 1 acc []]
      (let [q (quot x d)]
        (if (zero? q)
          (if (empty? acc)
            [0]
            (reverse acc))
          (recur (* d base) (conj acc (mod q base))))))))

(defcheck solution-86f2e8da
  (fn digits [x base]
    (if (< x base)
      [x]
      (conj (digits (quot x base) base) (mod x base)))))

(defcheck solution-87ce3066
  (fn [n base]
    (loop [n n, ret []]
      (if (zero? n)
        (if (empty? ret) [0] ret)
        (recur (quot n base) (cons (mod n base) ret))))))

(defcheck solution-87edfd44
  (fn [start-n base] (reverse (loop [n start-n repr []]
                                (if (zero? (quot n base))
                                  (conj repr (mod n base))
                                  (recur (quot n base)
                                    (conj repr (mod n base))))))))

(defcheck solution-883409c8
  (fn [n base]
    (loop [b (quot n base)
           res [(mod n base)]]
      (if (= 0 b)
        res
        (recur (quot b base)
          (cons (mod b base) res))))))

(defcheck solution-88559744
  (fn [n b]
    (let [pow
          (loop [x 1] (if (< n (apply * (repeat x b))) x (recur (inc x))))]
      (loop [d (dec pow) l n r []]
        (let [devisor (quot l (int (Math/pow b d)))]
          (if (= 0 d) (conj r l)
                      (recur (dec d)
                        (- l (* devisor (int (Math/pow b d))))
                        (conj r devisor))))))))

(defcheck solution-8891e28c
  (fn digits- [n base]
    ^{:doc "137. Write a function which returns a sequence of digits of
    a non-negative number (first argument) in numerical system with an
    arbitrary base (second argument)."}
    (if (< n base)
      [n]
      (conj (digits- (quot n base) base) (rem n base)))))

(defcheck solution-88af50f8
  (fn [n b]
    (if (zero? n) [0]
                  (loop [n n digits []]
                    (if (zero? n)
                      (vec digits)
                      (recur (quot n b) (concat [(mod n b)] digits))
                      )
                    ))
    ))

(defcheck solution-88c3309c
  (fn [x b]
    (let [digitseq (fn digitseq ([] (digitseq 1)) ([i] (cons i (lazy-seq (digitseq (* i b))))))
          myseq (reverse (take-while #(<= % x) (digitseq)))]
      (if (< x b)
        [0]
        ((fn searching [y dseq]
           (let [result (quot y (first dseq))]
             (if (= 1 (count dseq))
               [result]
               (cons result (searching (- y (* result (first dseq))) (rest dseq))))))
         x myseq)))))

(defcheck solution-890c3cf
  (fn [num base]
    (loop [res (if (zero? num) '(0) '()), n num]
      (if (zero? n) res
                    (recur (conj res (rem n base)) (quot n base))))))

(defcheck solution-89568a8a
  (fn digits [n base]
    (if (< n base) [n]
                   (conj (digits (quot n base) base) (mod n base)))))

(defcheck solution-89bd8fbb
  (fn [number base]
    (loop [n number, a []]
      (let [d (int (/ n base))
            r (mod n base)]
        (cond (= 0 n) (if (empty? a) [0]
                                     (into [] (reverse a)))
              :else (recur d (conj a r)))))))

(defcheck solution-8a0f94d5
  (fn [x b]
    (->> x
      (iterate #(quot % b))
      (take-while #(> % 0))
      (map #(mod % b))
      reverse
      (#(if (empty? %) '(0) %)))))

(defcheck solution-8a8e9ee4
  (fn [i-v base]
    (loop [agg () v i-v]
      (if (< v base)
        (cons v agg)
        (let [d (mod v base)
              rest (/ (- v d) base)]
          (recur (cons d agg) rest))))))

(defcheck solution-8ac96109
  (fn [n b]
    (loop [rn n
           r '()]
      (if (zero? rn)
        (if (empty? r) '(0) r)
        (recur (int (/ rn b)) (cons (mod rn b) r))))))

(defcheck solution-8b0472d5
  #((fn [n d a]
      (let [m (mod n d)
            q (quot n d)
            as (cons m a)]
        (if (= 0 q)
          as
          (recur q d as)))) %1 %2 []))

(defcheck solution-8b4d547e
  (fn [n base]
    {:pre [(>= n 0)]}
    (letfn [(step [r n base]
              (if (zero? n)
                r
                (step (conj r (mod n base))
                  (quot n base)
                  base)))]
      (if (zero? n)
        '(0)
        (step '() n base)))))

(defcheck solution-8b5500e4
  (fn [n base]
    (loop [result []
           n n]
      (if (= 0 n)
        (if (= 0 (count result))
          [0]
          (reverse  result))
        (recur (conj result (mod n base)) (quot n base))
        ))))

(defcheck solution-8b58c06a
  (fn digits [n base]
    (if (zero? n) [0]
                  (conj (vec (drop-while zero? (digits (quot n base) base)))
                    (mod n base)))))

(defcheck solution-8b7d86c9
  (fn digit-base
    ([nmbr base] (digit-base nmbr base []))
    ([nmbr base sq]
     (if (< nmbr base)
       (reverse (conj sq nmbr))
       (digit-base (int (/ nmbr base)) base (conj sq (mod nmbr base)))))))

(defcheck solution-8b851344
  (fn [num base]
    (loop [num num, result []]
      (if (zero? num)
        (if (empty? result) [0] result)
        (let [
              a (int (/ num base))
              b (mod num base)]
          (recur a (cons b result)))))))

(defcheck solution-8ba2adb3
  (fn pr137 [n base]
    (loop [n n res []]
      (if (zero? n)
        (if (empty? res) [0] res)
        (recur (quot n base) (cons (rem n base) res))))))

(defcheck solution-8bad5011
  (fn f
    ([n b] (vec (f n b nil)))
    ([n b a]
     (if (< n b)
       (conj a n)
       (let [r (rem n b)]
         (f (int (/ n b)) b (conj a r)))))))

(defcheck solution-8c5ff9d5
  (fn [n b]
    (cond (zero? n) [0]
          (= n b) [1 0]
          :else (let [i (dec (int (Math/ceil (/ (Math/log n) (Math/log b)))))]
                  (letfn [(f [m j]
                            (if (zero? j)
                              [m]
                              (let [c (int (Math/ceil (/ (Math/log m) (Math/log b))))
                                    x (int (Math/pow b (dec c)))
                                    digit (if (zero? x) x (int (Math/floor (/ m x))))
                                    rst (- m (* digit x))]
                                (concat [digit] (f rst (dec j))))))]
                    (f n i))))))

(defcheck solution-8c9c749c
  (fn [s x r]
    (if (< x r)
      (conj s x)
      (recur (conj s (mod x r)) (quot x r) r))) '())

(defcheck solution-8cb306ab
  (fn nx[v b] (
                if (< v b)
                [(mod v b)]
                (conj (nx (quot v b) b) (mod v b))
                )))

(defcheck solution-8ce5a635
  (fn [n b]
    (loop [nn n l ()]
      (if (< nn b)
        (conj l nn)
        (recur (quot nn b) (conj l (mod nn b)))))))

(defcheck solution-8cf68d5e
  (fn digits-and-bases [num base]
    (if (= 0 num) '(0)
                  (loop [n num result '()]
                    (if (not= 0 n)
                      (recur (int (/ n base)) (cons (mod n base) result))
                      result)))))

(defcheck solution-8d25bba5
  (fn digits
    [num base]
    (if (< num base)
      [num]
      (loop [multipliers
                (->
                  (map
                    #(reduce * (take % (repeat base)))
                    (range 1
                      ;; sub-routine to get the # of digits
                      (loop [n 2]
                        (let [max-plus-one
                              (reduce * (take n (repeat base)))]
                          (cond
                            (< num max-plus-one) n
                            (= num max-plus-one) (inc n)
                            :else (recur (inc n)))))
                      ))
                  (conj 1)
                  (reverse))
             v num
             ds []]
        (let [divisor (first multipliers)
              digits (conj ds (quot v divisor))]
          (if-let [xs (seq (rest multipliers))]
            (recur xs (mod v divisor) digits)
            digits))))))

(defcheck solution-8d345fca
  #(let [help (fn [n res]
                (if (< n %2)
                  (conj res n)
                  (recur (quot n %2) (conj res (rem n %2)))
                  )
                )]
     (help %1 '())
     ))

(defcheck solution-8d6e85ca
  (fn [n base]
    (if (zero? n)
      [0]
      (reverse
        (map
          second
          (take-while
            (fn [[a b]] (or (pos? a) (pos? b)))
            (rest
              (iterate
                (fn [[tot _]] [(quot tot base) (rem tot base)])
                [n]))))))))

(defcheck solution-8db1783a
  (fn db ([n b] (db n b ()))
    ([n b l] (if (zero? n)
               (if (empty? l) '[0] l)
               (recur (quot n b) b (conj l (rem n b)))))))

(defcheck solution-8dc85e23
  (fn [n base]
    (if (zero? n)
      [0]
      (let [digits (fn digits [n base]
                     (if (zero? n)
                       []
                       (conj (digits (quot n base) base) (mod n base))))]
        (digits n base)))))

(defcheck solution-8df1caba
  (fn [n b]
    (if (= 0 n)
      [0]
      ((fn [n s]
         (if (= 0 n)
           s
           (recur (quot n b) (conj s (mod n b))))
         ) n '()))))

(defcheck solution-8e01ab98
  (fn [n r]
    (loop [x n s []]
      (if (< x r) (cons x s)
                  (recur (quot x r) (cons (rem x r) s))
                  )
      )
    ))

(defcheck solution-8e4e812a
  #(loop [n %1
          b %2
          d `()]
     (if (< n b)
       (vec (cons n d))
       (recur (quot n b) b (cons (mod n b) d)))))

(defcheck solution-8e98dae0
  (fn [number base]
    (reverse
      (loop [current number results []]
        (if (= current 0) (if (empty? results) [0] results)
                          (recur (quot current base) (conj results (rem current base))))))))

(defcheck solution-8efdbb32
  (fn base [x b] (if (< x b) [x] (conj (base (quot x b) b) (mod x b)))))

(defcheck solution-8f4bbbdb
  (fn [n b]
    (cond
      (= n 0) [0]
      (= n b) [1 0]
      :else
      (let [digits (reverse (take-while #(< % n) (map (fn [x] (Math/pow b x)) (range))))]
        (reverse
          (loop [[d & ds] digits acc [] rem n]
            (if (not d) acc
                        (recur ds (cons (int (/ rem d)) acc) (mod rem d)))))))))

(defcheck solution-8f62c73d
  (fn [n b]
    (case n
      0 [0]
      (->> (iterate #(quot % b) n)
        (take-while pos?)
        (map #(rem % b))
        reverse))))

(defcheck solution-8f8c6cec
  (fn i [n b]
    (let [q (quot n b)
          m (mod  n b)]
      (if (zero? q) [m]
                    (conj (i q b) m)))))

(defcheck solution-8fa7494f
  (fn [n b]
    (loop [acc '()
           r (rem n b)
           q (int (/ n b))]
      (if (zero? q) (conj acc r)
                    (recur (conj acc r) (rem q b) (int (/ q b)))))))

(defcheck solution-900b23f8
  (fn digits-and-bases [d b]
    (loop [d d a '()]
      (let [x (quot d b) ;; 9 / 2 -> 5
            y (rem d b)] ;; 9 / 2 -> 1
        (if (= x 0)
          (conj a y)
          (recur x (conj a y)))))))

(defcheck solution-90599655
  (fn [x b]
    (let [n (if (zero? x) 0 (int (/ (Math/log x) (Math/log b))))]
      (first (reduce (fn [[v r] i]
                       (let [bi (apply * (repeat i b))]
                         [(conj v (quot r bi)) (rem r bi)]))
               [[] x] (range n -1 -1))))))

(defcheck solution-90abd2d0
  (fn decompose [n b] (if (< n b) [n] (conj (decompose (quot n b) b) (mod n b)))))

(defcheck solution-90c93704
  (fn convert [n base]
    (if (< n base)
      [n]
      (into (convert (quot n base) base)
        (convert (rem n base) base)))))

(defcheck solution-914fe89
  (fn [n base]
    (loop [n n
           acc '()]
      (cond
        (zero? n) (if (empty? acc) [0] acc)
        :else (let [d (mod n base)
                    carry (quot n base)]
                (recur carry (cons d acc)))))))

(defcheck solution-9190440b
  (fn [num base]
    (loop [n num coll nil]
      (if (zero? n)
        (if (nil? coll)
          [0]
          coll)
        (recur (int (/ n base)) (conj coll (mod n base)) )
        )
      )
    ))

(defcheck solution-91af7e2b
  (fn digits [num base]
    (if (zero? num)
      [0]
      (reverse (map second
                 (take-while #(not= [0 0] %)
                   (iterate (fn [[a b]] [(quot a base) (rem a base)])
                     [(quot num base) (rem num base)])))))))

(defcheck solution-91e1d5e7
  (fn base-in [n base]
    (if (= 0 n)
      [0]
      (loop [remaining n ans []]
        (if (= 0 remaining)
          ans
          (recur (quot remaining base) (cons (rem remaining base) ans)))))))

(defcheck solution-9229c6c9
  #(loop [x % r []]
     (if (zero? x) (if (empty? r) [0] r)
                   (recur (quot x %2) (cons (mod x %2) r)))))

(defcheck solution-9293fd31
  (fn [valor base]
    (if (zero? valor) [0]
                      ( (comp butlast reverse)
                       (map #(second %1) (take-while #(not= %1 [0 0])
                                           (iterate #(let [numera (first %1)]
                                                       [(int (/ numera base)) (rem numera base)] ) [valor nil] )))))))

(defcheck solution-92b6adc4
  #(loop [n % d ()]
     (if (> %2 n)
       (conj d n)
       (recur (quot n %2) (conj d (rem n %2))))))

(defcheck solution-92e891c3
  #(loop [ r () n %]
     (let [r1 (conj r (mod n %2)) n1 (quot n %2)]
       (if (> n1 0) (recur r1 n1) r1))))

(defcheck solution-934e1ca4
  (fn dnb [n base]
    (if (< n base)
      [n]
      (loop [left n
             acc  []]
        (if (= left 0)
          (reverse acc)
          (recur (quot left base) (conj acc (rem left base))))))))

(defcheck solution-937aeb74
  (fn to-base [n b]
    (loop [n n num ()]
      (if (zero? n)
        (if (empty? num) '(0) num)
        (recur (int (/ n b)) (conj num (mod n b)))))))

(defcheck solution-94749ccf
  (fn digits-and-bases
    [n base]
    (letfn [(digits-and-bases*
              [n base ret]
              (if (zero? n)
                ret
                (recur (quot n base) base (cons (mod n base) ret))))]
      (if (zero? n) [0] (digits-and-bases* n base [])))))

(defcheck solution-94e224bd
  (fn digits-base [n b]
    (if (= n 0)
      [0]
      (loop [acc '()
             n n]
        (if (> n 0)
          (recur (cons (mod n b) acc) (quot n b))
          acc)))))

(defcheck solution-9564312b
  #(letfn [(f [x base]
             (if (> base x)
               [x]
               (vector (f (quot x base) base) (rem x base))))]
     (flatten (f %1 %2))))

(defcheck solution-9614fa17
  (fn encode [p base] (or (seq (into () (map #(mod (quot p %) base)
                                          (take-while #(< 0 (quot p %)) (iterate #(* base %) 1) )))) [0] )))

(defcheck solution-963cdcca
  (fn [x rd]
    (loop
     [x x, l ()]
      (if (zero? x) (if (empty? l) '(0) l)
                    (recur (quot x rd) (cons (rem x rd) l))))))

(defcheck solution-964215a9
  #(if (= 0 %) [0] ((fn f [x b] (when (pos? x) (concat (f (quot x b) b) [(mod x b)]))) % %2)))

(defcheck solution-96c33b05
  (fn [n b]
    (loop [ret [] num n]
      (let [q (quot num b) r (rem num b)]
        (if (= q 0)
          (reverse (conj ret r))
          (recur (conj ret r) q))))))

(defcheck solution-96e00f2f
  (fn digits [n base]
    (if (< n base)
      (list n)
      (concat (digits (quot n base) base)
              (list (rem n base))))))

(defcheck solution-96e22bff
  (fn [r n b]
    (if (< n b)
      (cons n r)
      (recur (cons (rem n b) r) (quot n b) b))) [])

(defcheck solution-976d2298
  (fn [n b]
    (loop [digits (list (rem n b))      ; Initialised like this to handle the 0-case
           n (quot n b)]
      (if (< n 1)
        digits
        (recur (conj digits (rem n b)) (quot n b))))))

(defcheck solution-9913fe50
  (fn f [n b]
    (if (>= n b)
      (conj (f (quot n b) b) (rem n b))
      [n])))

(defcheck solution-99919fe1
  (fn [n base]
    (letfn [(foo [n]
              (let [x (quot n base) r (rem n base)]
                (if (> x 0)
                  (cons r (foo x))
                  [r])))]
      (reverse (foo n)))))

(defcheck solution-999fe52a
  (fn [n r] (reverse ((fn digits [n r] (if (< n r) [n] (cons (mod n r) (lazy-seq (digits (long (/ n r)) r))))) n r))))

(defcheck solution-99a6636d
  (fn c137
    ([n b]
     (if (= n 0)
       [n]
       (c137 n b [])))
    ([n b r]
     (if (= n 0)
       []
       (conj (c137 (int (/ n b)) b r) (mod n b))))))

(defcheck solution-99b06bfb
  (fn c [n b]
    (if (< n b) [n]
                (conj (c (quot n b) b) (mod n b)))))

(defcheck solution-99c96c74
  (fn __ [n base]
    (if (zero? (quot n base))
      [(mod n base)]
      (conj (__ (quot n base) base) (mod n base)))))

(defcheck solution-9a28b6a5
  (letfn [(pow [x n]
            (reduce * (repeat n x)))]
    (fn [v base]
      (if (zero? v)
        [0]
        (let [powers (reverse (take-while #(<= (pow base %) v) (range)))]
          (loop [[power & more] powers, v v, accu []]
            (let [magnitude (pow base power)
                  digit (quot v magnitude)
                  newval (- v (* digit magnitude))
                  accu (conj accu digit)]
              (if (seq more)
                (recur more newval accu)
                accu))))))))

(defcheck solution-9a7b4917
  (fn [n b] (loop [n n r []] (if (zero? (quot n b)) (cons n r) (recur (quot n b) (cons (rem n b) r))))))

(defcheck solution-9a8e832
  #(loop [x %1 radix %2 acc []] (if (zero? x)
                                  (if (empty? acc) [0] acc)
                                  (recur (quot x radix) radix (cons (rem x radix) acc)))))

(defcheck solution-9ac8726a
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [pow 1 ans []]
        (if (< n pow)
          ans
          (recur (* pow base) (into [(quot (mod n (* pow base)) pow)] ans)))))))

(defcheck solution-9b096527
  (fn convert [x base]
    (if (< x base)
      [x]
      (concat (convert (quot x base) base)
              (list (mod x base))))))

(defcheck solution-9c0c4343
  (fn digits-bases
    ([num b]
     (if (= num 0) [0] (digits-bases num b b)))
    ([num b mag]
     (if (<= num 0)
       []
       (let [remainder (rem num mag)
             prev-mag (/ mag b)
             current-digit (/ remainder prev-mag)
             remaining-value (- num remainder)
             ]
         (conj (digits-bases remaining-value b (* mag b)) current-digit))))))

(defcheck solution-9c13a55d
  (fn [x b] (if (zero? x) [0] (reduce #(let [r (quot x %2) d (mod r b)] (conj %1 d)) '() (take-while #(<= % x) (iterate (partial * b) 1))))))

(defcheck solution-9c8282e0
  (fn d [n b]
    (if (< n b) [n]
                (conj (d (quot n b) b) (mod n b)))))

(defcheck solution-9cc0a3f5
  (fn conv [x b]
    (if (< x b)
      [x]
      (conj (conv (quot x b) b) (rem x b)))))

(defcheck solution-9d16d3e9
  (fn [number base]
    (let [
          impl (fn [acc number base]
                 (let [x (quot number base)
                       y (rem number base)]
                   (if (<= x 0)
                     (cons y acc)
                     (recur (cons y acc) x base))))
          ]
      (impl '() number base))))

(defcheck solution-9d91df2d
  (fn basec
    ([n b]
     (basec n b []))
    ([n b v]
     (let [q (int(/ n b))
           r (mod n b)
           v (cons r v)]
       (if (< q b) (if (zero? q) v (cons q v)) (basec q b v))))))

(defcheck solution-9d96a64d
  (fn [n b]
    (if (zero? n) [0]
                  (->> n
                    (iterate #(quot % b))
                    (take-while pos?)
                    (map #(mod % b))
                    reverse))))

(defcheck solution-9dc07faf
  (fn [n base]
    (if (zero? n)
      [0]
      (reverse (map #(mod % base)(take-while pos? (iterate #(quot % base) n)))))))

(defcheck solution-9dfba81c
  (fn [n base]
    (loop [acc [] n n]
      (if (< n base)
        (reverse (conj acc n))
        (recur (conj acc (rem n base)) (quot n base))))))

(defcheck solution-9e0aff2
  (fn digits [n b]
    (if (< n b)
      `(~n)
      (concat (digits (quot n b) b) `(~(mod n b))))))

(defcheck solution-9e234bd1
  #(loop [r %2 i % o []]
     (if (zero? i)
       (if (empty? o) [0] o)
       (recur r (quot i r)
         (cons (rem i r) o)))))

(defcheck solution-9eb408a5
  #((fn f [l r]
      (let [o (quot r %2)
            k (conj l (mod r %2))]
        (if (= 0 o)
          k
          (f k o))))
    '() %1))

(defcheck solution-9ecd0dde
  (fn digits
    ([n b] (digits n b []))
    ([n b acc]
     (if (zero? n) (if (empty? acc) [0] (reverse acc))
                   (digits (/ (- n (mod n b)) b) b (conj acc (mod n b)))))))

(defcheck solution-9f1338f4
  (fn digs [n b]
    (if (zero? n)
      [0]
      (let [r (digs (quot n b) b)]
        (conj
          (if (= [0] r) [] r)
          (rem n b)
          )
        )
      )
    ))

(defcheck solution-9f56b502
  #(if (= % 0)
     [0]
     ((fn f [n]
        (when (> n 0)
          (concat (f (quot n %2))
                  [(rem n %2)]))) %)))

(defcheck solution-9fb4cbb4
  (fn digs [n b]
    (if (< n b)
      [n]
      (conj (digs (quot n b) b)
        (mod n b)))))

(defcheck solution-9fb8883
  (fn f [x b]
    (if (< x b)
      [x]
      (conj (f (quot x b) b) (rem x b)))))

(defcheck solution-9fc3de1e
  #(loop [r %2 v [(mod % %2)]] (if (> r %) v (recur (* r %2) (cons (mod (int (/ % r)) %2) v)))))

(defcheck solution-9fdaeb0c
  (fn digits [n base]
    (if (< n base)
      [n]
      (conj (digits (quot n base) base) (rem n base)))))

(defcheck solution-9ff801ef
  (fn f [n b]
    (let [d (mod n b)
          r (int (/ n b))]
      (if (zero? r)
        [d]
        (conj (f r b) d)))))

(defcheck solution-a0091f69
  (fn [n b]
    (if (= 0 n) [0]
                (loop [x n d []]
                  (if (= 0 x)
                    d
                    (recur (quot x b) (cons (rem x b) d)))))))

(defcheck solution-a041a23e
  #(let [f (fn [n r]
             (if (= n 0)
               (if (empty? r) [0] (reverse r))
               (recur (int (/ n %2))
                 (conj r (mod n %2)))))]
     (f %1 [])))

(defcheck solution-a0894d83
  (fn
    [n b]
    (if (= n 0) [0]
                (loop [n n l []]
                  (if (= 0 n)
                    l
                    (recur (quot n b) (cons (mod n b) l)))))))

(defcheck solution-a09e7293
  #(loop [n %
          b %2
          r nil]
     (let [a (int (/ n b))
           c (rem n b)]
       (if (zero? a)
         (conj r c)
         (recur a b (cons c r))))))

(defcheck solution-a0ab88da
  (fn [num base]
    (loop [left num, digits nil]
      (let [res (int (/ left base))
            rem (rem left base)]
        (if-not (= res 0)
          (recur res (cons rem digits))
          (cons rem digits))))))

(defcheck solution-a0b391d6
  (fn [i b] (if (= 0 i) (list 0)
                        (loop [d '() q i]
                          (if (= 0 q)
                            d
                            (recur (cons (mod q b) d) (quot q b)))))))

(defcheck solution-a10fd72a
  (fn [number new-base]
    (reverse ((fn base-convert-2 [number new-base]
                (lazy-seq (if (> new-base number)
                            (list number)
                            (conj (base-convert-2 (quot number new-base)
                                    new-base)
                              (rem number new-base)
                              )
                            )
                  )) number new-base))))

(defcheck solution-a12d2458
  (fn digits-bases [a-num base]
    (if (== a-num 0) [0]
                     (->> (iterate #(quot % base) a-num)
                       (take-while (partial < 0))
                       reverse
                       (mapv #(mod % base))))))

(defcheck solution-a184bf1b
  #(loop [ds '()
          q  %]
     (let [[q r] ((juxt quot rem) q %2)
           ds    (conj ds r)]
       (if (zero? q)
         ds
         (recur ds q)))))

(defcheck solution-a1b910ea
  (fn [v n]
    (let [f (fn g [ni vi r]
              (if (empty? ni)
                (if (empty? r) [0] r)
                (g (rest ni) (rem vi (first ni)) (conj r (int (/ vi (first ni)))))))]
      (f (reverse (take-while #(>= v %) (iterate #(* n %) 1))) v []))))

(defcheck solution-a1c0efd6
  (fn __ [a b]
    (if (< a b)
      [a]
      (conj (__ (quot a b) b) (rem a b)))))

(defcheck solution-a20a603b
  (fn [n base]
    (loop [n n rv nil]
      (if (< n 1)
        (if rv
          rv
          '(0))
        (recur (quot n base) (cons (mod n base) rv))))))

(defcheck solution-a213bb52
  (fn [n b]
    (into [] ((fn [acc r]
                (if (< r b) (conj acc r) (recur (conj acc (mod r b)) (quot r b))))
              () n))))

(defcheck solution-a2173bd7
  #(loop [r nil n %]
     (if (zero? n)
       (or r [0])
       (recur (cons (mod n %2) r) (quot n %2)))))

(defcheck solution-a22ef76c
  (fn [n base]
    (loop [ret '() n n]
      (let [q (quot n base)
            m (mod n base)
            ret (conj ret m)]
        (if (< q base)
          (if-not (zero? q) (conj ret q) ret)
          (recur ret q))))))

(defcheck solution-a236242a
  (fn [x b]
    (if (zero? x) '(0)
                  (loop [n x l '()]
                    (if (zero? n)
                      l
                      (recur (quot n b) (cons (mod n b) l)))))))

(defcheck solution-a23d2337
  (fn dig [n b] (if (< n b) [n] (conj (dig (quot n b) b) (rem n b)))))

(defcheck solution-a2ab9dd8
  (fn [x b]
    (loop [x x
           r '()]
      (if (< x b)
        (cons x r)
        (recur (quot x b) (cons (mod x b) r))))))

(defcheck solution-a2b9d2e
  (fn pd [lst num base]
    (if (and (empty? lst) (= 0 num)) [0]
                                     (if (= 0 num) lst (pd (cons (rem num base) lst) (quot num base) base)))) [])

(defcheck solution-a2fcf81f
  (fn digits [x base]
    (loop [res [] num x]
      (if (= num 0)
        (if (seq res) res [0])
        (recur (cons (mod num base) res) (int (/ num base))) ))))

(defcheck solution-a3760944
  #(loop [ds (), n %1]
     (if (< n %2) (cons n ds)
                  (recur (cons (rem n %2) ds) (quot n %2)))))

(defcheck solution-a37c0f3b
  (fn f [x b] (let [d (int (/ x b)) r (rem x b)] (if (> d 0) (conj (f d b) r) [r]))))

(defcheck solution-a3c52a97
  (fn int-seq [num base]
    (let [n (int (/ num base))
          r (rem num base)]
      (if (= 0 n)
        [r]
        (conj (int-seq n base) r)))))

(defcheck solution-a3ea9dbc
  (fn dab
    [d b]
    (if (>= d b)
      (conj (dab (quot d b) b) (mod d b))
      [d])))

(defcheck solution-a4337cab
  (fn [num base]
    (loop [num num result '()]
      (if (< num base)
        (conj result num)
        (recur (quot num base) (conj result (mod num base)))))))

(defcheck solution-a4445d7c
  (fn get-digits [n radix]
    (if (> radix n)
      [0]
      (loop [x n, result []]
        (if (zero? x)
          (reverse result)
          (recur (quot x radix) (conj result (rem x radix))))))))

(defcheck solution-a4453bea
  (fn [n b]
    (loop [x n r []]
      (if (< x b)
        (cons x r)
        (recur (quot x b) (cons (rem x b) r))))))

(defcheck solution-a4f50c4e
  #(if (zero? %) [0]
                 (loop [a [] x %] (if (zero? x) a (recur (cons (rem x %2) a) (quot x %2))))))

(defcheck solution-a500a63
  #(loop [result '() n %1]
     (let [r (rem n %2) r (conj result r)]
       (if (< n %2)
         r
         (recur r (quot n %2))))))

(defcheck solution-a506e8e7
  (fn dbs [d b]
    (loop [d d k ()]
      (if (< d b) (conj k d)
                  (recur (quot d b) (conj k (mod d b)))))))

(defcheck solution-a547ab0c
  (fn base-recur [n r]
    (if (= n 0) [0]
                (let [maxd (int (/ (Math/log n) (Math/log r)))
                      base (fn rbase [n10 radix pow]
                             (if (>= pow 0)
                               (let [p (int (Math/pow radix pow))
                                     N (quot n10 p)
                                     s (- n10 (* N p))]
                                 (concat [N] (rbase s radix (dec pow))))
                               '()))]
                  (base n r maxd)))))

(defcheck solution-a5a707c1
  (fn __ [n b]
    (let [q (quot n b) r (rem n b)]
      (if (zero? q) [r]
                    (concat (__ q b) [r])))))

(defcheck solution-a5ec532
  (letfn [(f [b n a]
            (if (= 0 (quot b n))
              (cons (mod b n) a)
              (recur (quot b n) n (cons (mod b n) a))))]
    #(f %1 %2 [])))

(defcheck solution-a63a1217
  (fn [n base]
    (loop [acc [] r n]
      (if (> base r) (cons r acc)
                     (recur
                       (cons (mod r base) acc)
                       (/ (- r (mod r base)) base))))))

(defcheck solution-a676ae87
  (fn digs-bases [n base]
    (loop [n n
           acc '()]
      (if (zero? n)
        (if (empty? acc) [0] acc)
        (recur (quot n base) (conj acc (mod n base)))))))

(defcheck solution-a6c4f99a
  (fn [n b]
    (loop [acc '() n n]
      (if (= n 0)
        (if (seq acc) (vec acc) [0])
        (recur (cons (mod n b) acc) (int (/ n b)))))))

(defcheck solution-a7678798
  (fn to-base [x n]
    (loop [i x, acc ()]
      (let [q (quot i n) r (rem i n)]
        (if (zero? q)
          (conj acc r)
          (recur q (conj acc r)))))))

(defcheck solution-a7a35467
  (fn [n b]
    (if (zero? n) [0]
                  (loop [i n r '()]
                    (if (zero? i) r
                                  (recur (quot i b) (conj r (rem i b))))))))

(defcheck solution-a7c2b4a3
  (fn digitsandbases [x n]
    "Returns a sequence of digits of x in base n."
    (if (< x n)
      (vector x)
      (conj (digitsandbases (/ (- x (mod x n)) n) n) (mod x n)))))

(defcheck solution-a7c5cdea
  (fn
    [n b]
    (loop [a (quot n b)
           b b
           r [(rem n b)]]
      (if (zero? a)
        r
        (recur (quot a b) b (cons (rem a b) r))))))

(defcheck solution-a7d066ac
  (fn [n b]
    (if (= n 0)
      [0]
      (reverse (map #(rem % b)
                 (take-while #(not= 0 %)
                   (iterate #(int (/ % b)) n)))))))

(defcheck solution-a7f36d0f
  #(if-not(zero? %1)(loop [a %1 b []] (if (zero? a)(reverse b) (recur (quot a %2)(conj b (mod a %2)))))[0] ))

(defcheck solution-a7f41e97
  (fn f [x b]
    (if (> b x)
      [x]
      (conj (f (quot x b) b) (mod x b)))))

(defcheck solution-a80f162a
  (fn [n b]
    (letfn [(int-base [results n]
              (if (= n 0)
                results
                (recur (cons (mod n b) results) (quot n b))))]
      (if (= 0 n)
        [0]
        (int-base [] n)))))

(defcheck solution-a836d47a
  (fn [x y]
    (let
     [base (fn base [n b]
             (if (= n 0)
               []
               (conj (base (quot n b) b)
                 (rem n b))))
      res (base x y)]
      (if (= res [])
        [0]
        res))))

(defcheck solution-a8674703
  (fn ->radix [n b]
    (loop [x n
           vs (list)]
      (if (not (zero? x))
        (recur (quot x b) (conj vs (mod x b)))
        (if (empty? vs) [0] vs)))))

(defcheck solution-a889f9aa
  (fn self [n bs]
    (if (< n bs)
      [n]
      (conj (self (quot n bs) bs) (mod n bs)))))

(defcheck solution-a8d6d0c3
  (fn digits [n base]
    (if (zero? n)
      '(0)
      (loop [res '() n n]
        (if (zero? n)
          res
          (recur (conj res (rem n base))
            (quot n base)))))))

(defcheck solution-a91c4d9f
  (fn f [a b]
    (if (= a 0) '(0) (loop [ans '() aa a]
                       (if (= aa 0) ans
                                    (recur (conj ans (rem aa b)) (quot aa b)))))))

(defcheck solution-a9292f4b
  #(loop [n % r '()]
     (cond
       (< 0 n)   (recur (quot n %2) (conj r (rem n %2)))
       (= '() r) '(0)
       1         r)))

(defcheck solution-a96f322b
  (fn toBase [x B]
    (if (zero? x) '(0)
                  (loop [x x, res '()]
                    (if (zero? x) res
                                  (recur (quot x B) (cons (rem x B) res)))))))

(defcheck solution-aafaafca
  (fn [n b]
    (loop [res  [(mod n b)]  nn (quot n b)]
      (if (zero? nn) (reverse res)
                     (recur   (conj res (mod nn b) ) (quot nn b) )
                     )


      )
    ))

(defcheck solution-ab024258
  (fn [n b]
    (if (zero? n) '(0)
                  (loop [m n res ()]
                    (if (zero? m)
                      res
                      (recur (int (Math/floor (/ m b))) (cons (mod m b) res)))))))

(defcheck solution-ab2bf7c3
  (fn [n base]
    (loop [n n
           out '()]
      (if (zero? n)
        (if (seq out) out '(0))
        (recur (quot n base) (conj out (rem n base)))))))

(defcheck solution-ab34961d
  (fn [n b]
    (if (zero? n) [0]
                  (loop [rst n result '()]
                    (if (zero? rst)
                      result
                      (recur (quot rst b) (conj result (mod rst b))))))))

(defcheck solution-abca6f6c
  (letfn [(explode [n base]
            (if (= n 0) []
                        (conj (explode (quot n base) base)
                          (rem n base))))]
    (fn [n base]
      (if (= n 0) [0]
                  (explode n base)))))

(defcheck solution-abe582cb
  (fn [digit base]
    (if (< digit base)
      [0]
      (reverse
        (map second
          (take-while (fn [[d r]] (not (= d r 0)))
            (iterate (fn [[d _]] [(int (/ d base)) (mod d base)])
              [(int (/ digit base)) (mod digit base)])))))))

(defcheck solution-abf04a9d
  (fn [a b]
    (if (zero? a)
      [0]

      (loop [ans []
             m a]
        (if (zero? m)
          (vec (reverse ans))
          (recur (conj ans (mod m b))
            (quot m b)))))))

(defcheck solution-ac226a45
  (fn nr [ x base ]
    (cond
      (= x base )  (vector  1 0 )
      (< x base )  (vector (rem x base))
      :else (conj  (nr (quot x base) base) (rem x base)))))

(defcheck solution-ac41c86
  (fn [n base]
    (reverse
      (loop [n n, l []]
        (if (= n 0)
          (if (empty? l) [0] l)
          (recur
            (quot n base)
            (conj l (mod n base))))))))

(defcheck solution-ad47e8f7
  (fn digits [n base]
    (let [d (quot n base), m (mod n base)]
      (if (= 0 d) [m] (conj (digits d base) m)))))

(defcheck solution-ad53eb93
  (fn bseq [x, b]
    (loop [digits []
           x      x]
      (if (= 0 x)
        (if (empty? digits)
          [0]
          (reverse digits))
        (let [x-next (quot x b)
              digit  (rem x b)]
          (recur (conj digits digit) x-next))))))

(defcheck solution-ad92b3e4
  (fn [n b] (loop [n n res []]
              (if (< n b)
                (reverse (conj res n))
                (recur (quot n b) (conj res (rem n b)))))))

(defcheck solution-adcc7cb4
  (fn tb [n b] (conj (if (< n b) [] (tb (quot n b) b)) (mod n b))))

(defcheck solution-ae162cef
  #(let [n (quot %2 %3)
         d (conj % (mod %2 %3))]
     (if (zero? n)
       d
       (recur d n %3))) '())

(defcheck solution-ae2893e
  (fn [n b] (if (zero? n)
              [0]
              (second (reduce (fn [[n acc] b] (let [v (int (/ n b))] [(- n (* v b)) (conj acc v)])) [n []] (reverse (take-while #(<= % n) (cons 1 (iterate #(* b %) b)))))))))

(defcheck solution-ae6aca99
  (fn parse-int [n base]
    (if (< n base)
      [n]
      (conj (parse-int (quot n base) base) (mod n base)))))

(defcheck solution-ae8ffaa3
  (fn [n b]
    (if (zero? n)
      [0]
      (loop [nn n
             ret '()]
        (if (zero? nn)
          ret
          (recur (quot nn b) (conj ret (mod nn b))))))))

(defcheck solution-af80e2dc
  (fn [n base]
    (letfn [(digits [n] (let [q (quot n base) r (rem n base)]
                          (if (> q 0) (cons r (digits q)) (list r))))]
      (reverse (digits n)))))

(defcheck solution-af99ca9a
  (fn d [x b]
    (if (< x b)
      [x]
      (conj (d (quot x b) b) (mod x b)))))

(defcheck solution-afb3dafe
  (fn f [n b]
    (if (< n b)
      [n]
      (conj (f (quot n b) b)
        (rem n b)))))

(defcheck solution-afd07b15
  (fn [d base]
    (let [n (if (zero? d) 0 (int (/ (Math/log d) (Math/log base))))]
      (first (reduce (fn [[v r ] i]
                       (let [basen (apply * (repeat i base))]
                         [(conj v (quot r basen)) (rem r basen)]))
               [[] d] (range n -1 -1))))))

(defcheck solution-b0206885
  (fn base [n b]
    (letfn [
            (exponent [n] (int (/ (Math/log n) (Math/log b))))

            (to-base [acc n i]
              (if (< i 0)
                acc
                (let [x (Math/pow b i)
                      r (int (/ n x))]
                  (if (>= n x)
                    (to-base (vec (conj acc r)) (- n (* r x)) (dec i))
                    (to-base (vec (conj acc 0)) n (dec i))))))]

      (if (zero? n)
        [0]
        (to-base [] n (exponent n))))))

(defcheck solution-b10285af
  (fn [num base]
    (if (= num 0)
      [0]
      (loop [num num ds ()]
        (if (= num 0)
          (vec ds)
          (recur (quot num base) (conj ds (rem num base))))))))

(defcheck solution-b192eb6e
  (fn [x b]
    (if (zero? x)
      [0]
      (loop [a []
             x x]
        (if (zero? x)
          a
          (recur (cons (rem x b) a)
            (quot x b)))))))

(defcheck solution-b1a2c018
  (fn R [m b] (let [n (quot m b)] (conj (if (= 0 n) [] (R n b)) (mod m b)))))

(defcheck solution-b1f72394
  #(loop [result [] n %]
     (if (and (not (empty? result)) (= n 0)) (reverse result)
                                             (recur (conj result (rem n %2)) (quot n %2)))))

(defcheck solution-b2229f04
  (fn base-digits
    ([n b] (base-digits n b []))
    ([n b digits] (if (zero? n)
                    (if (empty? digits) [0] digits)
                    (recur (quot n b) b (cons (rem n b) digits))))))

(defcheck solution-b237e99
  (fn [n b]
    (letfn [(helper [n b]
              (if (< n b)
                (list n)
                (cons (mod n b) (helper (quot n b) b))))]
      (reverse (helper n b)))))

(defcheck solution-b2488828
  (fn to-base [x base]
    (if (zero? x) [0]
                  (loop [n x
                         result '()]
                    (if (zero? n) result
                                  (recur (quot n base) (cons (mod n base) result)))))))

(defcheck solution-b286534a
  (fn digits-for-base [n base]
    (if (< n base)
      [n]
      (conj (digits-for-base (int (/ n base)) base) (mod n base)))))

(defcheck solution-b2b0705a
  (fn number137 [n base]
    (loop [r [] c n]
      (if (< c base)
        (cons c r)
        (recur (cons (rem c base) r) (quot c base))))))

(defcheck solution-b2c4c443
  (fn [n b]
    (let [c (fn c[n]
              (if
               (zero? n) nil
                         (cons (mod n b) (c (quot n b)))))]
      (if (zero? n) [0]
                    (reverse (c n))))))

(defcheck solution-b2ff19c7
  #(letfn [(get-digits [acc n] (if (zero? n) acc (recur (cons (mod n %2) acc) (quot n %2))))]
     (if (zero? %1) [0] (get-digits '() %1))))

(defcheck solution-b32f67fa
  (fn convert [num base]
    (loop [num num res ()]
      (if (= num 0) (if (empty? res) '(0) res)
                    (recur (int (/ num base)) (cons (rem num base) res)))
      )
    ))

(defcheck solution-b37161b1
  (fn [x y]
    (loop [result '() n x]
      (if (= 0 (quot n y))
        (cons n result)
        (recur (cons (mod n y) result) (quot n y))))))

(defcheck solution-b3e19183
  (fn f [n b]
    (if (< n b) [n]
                (conj (f (quot n b) b) (rem n b)))))

(defcheck solution-b445fe81
  (fn change-base [n b] ;;change number n to n2 in base b
    (if (= n 0)
      '(0)
      (loop [n n
             res '()]
        (if (= n 0)
          res
          (recur (int (/ n b)) (conj res (mod n b))))))))

(defcheck solution-b497b69c
  (fn f [n b]
    (let [q (quot n b) m (mod n b)]
      (conj (if (= 0 q) [](f q b)) m))))

(defcheck solution-b49fc6f3
  (fn convert
    ([value base] (convert (quot value base) base [(mod value base)]))
    ([value base output]
     (if (= 0 value)
       output
       (recur (quot value base) base (cons (mod value base) output))))))

(defcheck solution-b4d4d2f3
  (fn [x base] (if (zero? x) [0]
                             (loop [acc () remains x]
                               ( if (zero? remains)
                                 acc
                                 (recur (cons (mod remains base) acc) (quot remains base) )
                                 )))))

(defcheck solution-b518b67e
  (fn f [n b]
    (if (< n b)
      [n]
      (conj (f (quot n b) b) (rem n b)))))

(defcheck solution-b5277c2a
  #(loop [d % r []] (if (< d %2) (reverse (conj r d)) (recur (quot d %2) (conj r (mod d %2))))))

(defcheck solution-b53d107
  (fn f [n b]
    (if (= (quot n b) 0)
      [(rem n b)]
      (conj (f (quot n b) b) (rem n b)))))

(defcheck solution-b553328f
  (fn [n base]
    (loop [n n s []]
      (if (pos? n)
        (recur (int (/ n base)) (conj s (mod n base)))
        (if (seq s)
          (reverse s)
          [0])))))

(defcheck solution-b5a2f43e
  (fn [n base]
    (loop [n n
           ret []]
      (if (zero? n)
        (if (zero? (count ret))
          [0]
          (reverse ret))
        (recur (quot n base)
          (conj ret
            (mod n base)))))))

(defcheck solution-b5ac57aa
  (fn digits [x rad]
    (if (< x rad)
      [x]
      (conj (digits (quot x rad) rad) (rem x rad)))))

(defcheck solution-b5e1ba08
  (fn base [num base]
    (reverse (loop [n num result []]
               (if (< n base)
                 (conj result n)
                 (recur (int (/ n base)) (conj result (int (mod n base)))))))))

(defcheck solution-b5f13433
  (fn [n b]
    (if (zero? n)
      '(0)
      (loop [n n ret '()]
        (if (< n 1)
          ret
          (recur (quot n b) (conj ret (rem n b))))))))

(defcheck solution-b612920c
  (fn dob [n b] (if (= 0 (quot n b))
                  (conj [] (mod n b))
                  (conj (dob (quot n b) b) (mod n b)))))

(defcheck solution-b6d66546
  (fn [n b]
    (if (zero? n) [0]
                  (reverse (map second
                             (rest
                               (take-while #(not= % [0 0])
                                 (iterate
                                   (fn [[q r]] [(quot q b) (rem q b)])
                                   [n 0]))))))
    ))

(defcheck solution-b6efe52d
  (fn [n b]
    (loop [r (quot n b) d (list (rem n b))]
      (if (zero? r)
        (vec d)
        (recur (quot r b) (conj d (rem r b)))))))

(defcheck solution-b73db629
  (fn [num base]
    (loop [n num, digits []]
      (if (zero? n)
        (if (empty? digits) [0] (reverse digits))
        (recur (quot n base) (conj digits (mod n base)))))))

(defcheck solution-b7a8a1cc
  (fn baseX [x b] (if(< x b) [x] (conj (baseX (quot x b) b) (mod x b)))))

(defcheck solution-b7f2988d
  (fn [n b]
    (or (seq (->> (iterate #(* % b) 1)
               (take-while #(<= % n))
               reverse
               (reduce #(vector (mod (first %) %2) (conj (second %) (int (/ (first %) %2)))) [n []])
               second)) [0])))

(defcheck solution-b81a3bd7
  (comp reverse
        (fn f [n b]
          (if (< n b) [n]
                      (cons (rem n b) (lazy-seq (f (quot n b) b))))
          )))

(defcheck solution-b8b22768
  (fn digits-bases
    [d b]
    (if (zero? d)
      [0]
      (loop [d d ret []]
        (if (zero? d)
          ret
          (recur (quot d b) (cons (rem d b) ret)))))))

(defcheck solution-b8f92b0b
  (fn digits [x base]
    (if (> base x)
      [x]
      (conj (digits (quot x base) base) (mod x base)))))

(defcheck solution-b9647ed6
  (fn [n base]
    (loop [q (quot n base) s (list (mod n base))]
      (if (zero? q)
        s
        (recur (quot q base) (cons (mod q base) s))))))

(defcheck solution-b9c872ac
  #(loop [x %1 r ()] (let [f (conj r (mod x %2))]
                       (if (< x %2) f (recur (int (/ x %2)) f)))))

(defcheck solution-ba34a217
  (fn [numb base] ( let
                   [fdig (fn [res n]
                           (if (> n (dec base))
                             (recur (conj res (mod n base))
                               (int (/ n base))
                               )
                             (conj res n)
                             )
                           )]
                    (reverse (fdig [] numb))
                    )
    ))

(defcheck solution-ba9c52a1
  (fn
    [n r]
    (let [radix (fn radix
                  [n r]
                  (if (zero? n)
                    []
                    (let [digit (rem n r)
                          left  (int (/ n r))]
                      (conj (radix left r) digit))))]
      (if (zero? n)
        [0]
        (radix n r)))))

(defcheck solution-bb0313af
  (fn [n base]
    (loop [n n
           digits '()]
      (if (zero? n)
        (if (empty? digits)
          '(0)
          digits)
        (recur (quot n base) (conj digits (rem n base)))))))

(defcheck solution-bb319dba
  #(loop [n %1 acc nil]
     (if (zero? n)
       (or acc [0])
       (recur (quot n %2) (cons (mod n %2) acc)))))

(defcheck solution-bb4438b0
  #(loop [c % a []]
     (if (< c %2)
       (cons c a)
       (recur (quot c %2) (cons (mod c %2) a)))))

(defcheck solution-bb49a684
  (fn to-base [num base]
    (if (= 0 num)
      [0]
      (loop [res '() q num]
        (if (= q 0)
          res
          (recur (conj res (mod q base)) (quot q base)))))))

(defcheck solution-bba1341b
  (fn [n base]
    (if (= 0 n)
      [0]
      (vec (drop 1 ((fn toBase[n base] (if (= n 0)
                                         [0]
                                         (concat (toBase (quot n base) base) [(rem n base)])))
                    n base))))))

(defcheck solution-bc38b90b
  (fn [n b]
    (or (seq (loop [v '()
                    x n]
               (if (zero? x)
                 v
                 (recur (cons (mod x b) v)
                   (quot x b)))))
        [0])))

(defcheck solution-bc4b6ffc
  (fn f[x b](conj(if(< x b)[](f(quot x b)b))(mod x b))))

(defcheck solution-bca2c01a
  (fn convert[b i]
    (loop
     [n b acc []]
      (let [nacc (cons (rem n i) acc)
            rem (quot n i)]
        (if (zero? rem) nacc (recur rem nacc))
        ))
    ))

(defcheck solution-bd3e9959
  (fn foo [n base]
    (if (zero? n)
      [0]
      (let [div-pair (fn [[q _]]
                       [(quot q base) (rem q base)])
            q (quot n base)
            r (rem n base)]
        (reverse (map second (take-while #(some pos? %) (iterate div-pair [q r]))))))))

(defcheck solution-bdeb300c
  (fn f ([n b]
         (if-let [c (last (take-while #(not (zero? (int (/ n %))))
                            (iterate #(* b %) 1)))]
           (f n b c)
           [0]))
    ([n b c]
     (if (= 1 c) (list n)
                 (let [rst (int (/ n c))]
                   (conj (f (- n (* c rst)) b (/ c b)) rst))))))

(defcheck solution-bdf08fe7
  (fn [d b]
    (last
      (take-while
        identity
        (iterate
          (fn [[l & r :as v]]
            #_(println "***" v)
            (when (>= l b)
              (let [m (mod l b)]
                (into [(/ (- l m) b) m] r))))
          [d])))))

(defcheck solution-be571a6b
  (fn change-base [number new-base]
    (vec
      (loop [acc () n number]
        (let [q (quot n new-base)
              r (rem n new-base)]
          (if (= q 0)
            (cons r acc)
            (recur (cons r acc) q)))))))

(defcheck solution-bea18656
  (fn aa [x y]  (reverse (loop [xx x z []]
                           (if (= (quot xx y) 0) (conj z xx)
                                                 (recur (quot xx y) (conj z (rem xx y)))
                                                 )
                           ))
    ))

(defcheck solution-befc37f9
  (fn [n base]
    (if (== n 0) [0]
                 (loop [i n, result []]
                   (if (== i 0) result
                                (let [digit (mod i base)]
                                  (recur (quot (- i digit) base)
                                    (into [digit] result))))))))

(defcheck solution-bf0fb5c1
  (fn prob137
    [num base]
    (loop [n num
           acc ()]
      (if (zero? n)
        (if (empty? acc) '(0) acc)
        (recur (int (/ n base)) (conj acc (mod n base)))))))

(defcheck solution-bf1d2c91
  #(loop [m % n %2 r ()] (if (and (zero? m) (first r)) r (recur (quot m n) n (cons (mod m n) r)))))

(defcheck solution-bf5f5f1f
  (fn [n b](let[ z
                (loop[k (loop[bb b]( if(>= bb n) bb (recur (* bb b)))) s [] r n](
                                                                                  if (= k 1) (conj s r)
                                                                                             (recur (/ k b) (conj s (int(/ r k))) (- r (* k (int(/ r k)))) )
                                                                                             ))
                ](if(= (first z) 0) (rest z) z)
                 )))

(defcheck solution-bfa2e084
  (fn f[x n]
    (if (> n x)
      [x]
      (concat (f (int (/ x n)) n) [(rem x n)]))))

(defcheck solution-bfeb5fe9
  (fn conv [n b]
    (if (zero? n) [n]
                  (loop [n n c '()]
                    (if (zero? n)
                      c
                      (recur (quot n b)  (cons (mod n b) c))
                      )
                    )
                  )
    ))

(defcheck solution-c02a25c
  (fn myf2 [n b]
    (loop [q n, v []]
      (if (zero? q) (if (empty? v) [n] v)
                    (recur (quot q b) (cons (rem q b) v))))))

(defcheck solution-c0a12279
  (fn [n ob]
    (loop [ds [], n n, b (Math/pow ob (Math/floor (/ (Math/log n) (Math/log ob))))]
      (if (< b ob)
        (conj ds (long n))
        (recur (conj ds (long (quot n b))) (rem n b) (/ b ob))))))

(defcheck solution-c0a459ac
  (fn digits [n base]
    (loop [n n digits ()]
      (if (< n base)
        (conj digits n)
        (recur (quot n base) (conj digits (rem n base)))))))

(defcheck solution-c0aabef
  #(loop [n % b %2 res ()]
     (if (zero? n) (if (empty? res) '(0) res)
                   (recur (quot n b) b (cons (mod n b) res)))))

(defcheck solution-c28c8b80
  (fn convert [num base]
    (if (zero? num) [0]
                    (loop [input num result []]
                      (if (zero? input)
                        (reverse result)
                        (recur (int (/ input base)) (conj result (rem input base))))))))

(defcheck solution-c2ed8249
  (fn base
    ([n b]
     (if (zero? n)
       [n]
       (base n b [])))
    ([n b r]
     (if (zero? n)
       (reverse r)
       (recur (quot n b) b (conj r (rem n b)))))))

(defcheck solution-c3d6102e
  (fn [n b]
    (or (seq (reverse (map #(mod % b)
                        (take-while pos?
                          (iterate #(quot % b) n)))))
        [0])))

(defcheck solution-c3e3f7a0
  (fn base-n [n base]
    (if (< n base)
      [n]
      (conj (base-n (quot n base) base) (mod n base)))))

(defcheck solution-c4099425
  (fn int2base [n base]
    (if (< n base) (vector n) (conj (int2base (quot n base) base) (rem n base)))))

(defcheck solution-c47f831e
  (fn [acc n b]
    (if (zero? n)
      (if (empty? acc) [0] (reverse acc))
      (recur (conj acc (mod n b)) (quot n b) b))) [])

(defcheck solution-c487cc71
  (fn [n a]
    (if (> a n)
      [0]
      (loop [i [] n n]
        (let [q (quot n a) r (rem n a)]
          (if (> a q)
            (reverse (conj i r q))
            (recur (conj i r) q)))))))

(defcheck solution-c4bb6ab5
  (fn [n base]
    (letfn [(dig-base [n base]
              (loop [remn n div []]
                (if (= remn 0)
                  div
                  (recur (quot remn base) (conj div (rem remn base))) )))]
      (if (= n 0)
        [0]
        (reverse (dig-base n base))))))

(defcheck solution-c54a6402
  (fn [decimal base]
    (if (zero? decimal)
      [0]
      (loop [x decimal digits []]
        (if (= x 0)
          digits
          (recur (quot x base) (cons (rem x base) digits)))))))

(defcheck solution-c54c217e
  (fn f [n b]
    (if (< n b)
      [n]
      (conj (f (quot n b) b) (mod n b)))))

(defcheck solution-c57cff3f
  (fn [n b]
    (if (zero? n) '(0)
                  (reduce conj '() (map #(mod % b) (take-while pos? (iterate #(int (/ % b)) n)))))))

(defcheck solution-c5e0d75b
  #((fn f [y r]
      (if (< y %2) (conj r y)
                   (f (quot y %2) (conj r (rem y %2)))))
    % '()))

(defcheck solution-c60aecaf
  (fn __ [n base]
    (loop [n n result []]
      (if (= 0 (int (/ n base)))
        (reverse (conj result (rem n base)))
        (recur (int (/ n base)) (conj result (rem n base)))))))

(defcheck solution-c637f8ca
  (fn translate-base [num base]
    (loop [leftover num
           builder '()]
      (cond
        (< leftover base) (cons leftover builder)
        :else (recur (/ (- leftover (rem leftover base)) base) (cons (rem leftover base) builder))))))

(defcheck solution-c6c19f63
  (fn [n b]
    (loop [k n res '()]
      (if (= k 0)
        (if (empty? res) '(0) res)
        (recur (quot k b) (conj res (rem k b)))))))

(defcheck solution-c7b0dc6c
  (fn [n b]
    (->> (iterate (fn [[c n]]
                    [(conj c (mod n b)) (quot n b)])
           [(list (mod n b)) (quot n b)])
      (drop-while #(not (zero? (second %))))
      ffirst)))

(defcheck solution-c7d01c8b
  (fn r [n b]
    (if (< n b)
      [n]
      (conj (r (int (/ n b)) b) (mod n b)))))

(defcheck solution-c825f5b6
  (fn [n b]
    (loop [n_ (quot n b), digits [(rem n b)]]
      (if (= 0 n_)
        (reverse digits)
        (recur (quot n_ b) (conj digits (rem n_ b)))))))

(defcheck solution-c82f3eaa
  (fn dab
    ([n b] (if (zero? n) [0] (dab n b [])))
    ([n b digs]
     (if (> n 0)
       (dab (int (/ n b)) b (cons (mod n b) digs))
       digs))))

(defcheck solution-c8784024
  (fn digits-and-bases
    [digit base]
    (if (zero? digit)
      '(0)
      (loop [acc ()
             digit digit]
        (if (zero? digit)
          acc
          (recur (cons (rem digit base) acc)
            (quot digit base)))))))

(defcheck solution-c89c5b07
  (fn digits [n base]
    (if (< n base)
      [n]
      (conj (digits (quot n base) base) (mod n base)))))

(defcheck solution-c8ea176a
  (fn base2->base-n [n base]
    (if (< n base)
      (list n)
      (loop [res '() n n]
        (if (zero? n)
          res
          (let [[r n] ((juxt rem quot) n base)]
            (recur (conj res r) n)))))))

(defcheck solution-c8f09c7b
  (fn [num b]
    (loop [n num
           digits []]
      (if (pos? n)
        (recur (quot n b)
          (cons (mod n b) digits))
        (if (empty? digits) [0] digits)))))

(defcheck solution-c94b8c85
  (fn [n base]
    (if (zero? n)
      [0]
      (loop [r []
             x n]
        (if (zero? x)
          r
          (recur (cons (mod x base) r)
            (quot x base)))))))

(defcheck solution-c9ef9ec0
  (fn d [a b](if
              (>= a b)
               (conj (d (quot a b) b) (mod a b))
               [a]
               )))

(defcheck solution-ca4893
  (fn digits-and-bases [n base]
    (loop [n n
           digits '()]
      (if (zero? n)
        (if (empty? digits)
          '(0)
          digits)
        (recur (int (/ n base)) (cons (mod n base) digits))))))

(defcheck solution-ca49eeb8
  (fn digits-in-base
    [num base]
    (if (= 0 num) '(0)
                  (loop [n num
                         l '()]
                    (let [digit (mod n base)]
                      (let [new-n (/ (- n digit) base)
                            new-l (conj l digit)]
                        (if (= n 0)
                          l
                          (recur new-n new-l))))))))

(defcheck solution-ca4c18a1
  (fn [n b]
    (loop [current-n n
           current-result []]
      (if (< current-n b)
        (conj current-result current-n)
        (recur (int (/ current-n b)) (cons (mod current-n b) current-result))))))

(defcheck solution-ca5a54a7
  (fn itos2 [n rad]
    (reverse
      (loop [q (quot n rad), r (rem n rad), out []]
        #_(println "q:" q "r:" r "out:" out)
        (if (< q rad)
          (if (zero? q)
            (conj out r)
            (conj out r q))
          (recur (quot q rad) (rem q rad) (conj out r)))))))

(defcheck solution-cb2f4210
  #(loop [n % r '()]
     (if (> n 0)
       (recur (quot n %2) (conj r (rem n %2)))
       (if (= [] r) [0] r)
       )
     ))

(defcheck solution-cb48573f
  (fn [n b]
    (loop [r [] n n]
      (if (< n b)
        (cons n r)
        (recur (cons (mod n b) r) (int (/ n b)))))))

(defcheck solution-cb4e70cc
  (fn [dd rd]
    (loop [ret [], nn dd]
      (if (= 0 nn)
        (if (empty? ret) [0] ret)
        (let [dd (rem nn rd), nn0 (quot nn rd)]
          (recur (cons dd ret) nn0))))
    ))

(defcheck solution-cb6c80e4
  (fn [n b]
    (if (= 0 n) [0]
                (loop [l '() n n c b]
                  (cond (= 0 n) l
                        (< n c) (conj l (/ (rem n c) (/ c b)))
                        :else (recur (conj l (/ (rem n c) (/ c b))) (- n (rem n c)) (* b c)))))))

(defcheck solution-cb7f9fb1
  (fn [n b]
    (loop [digs (list (rem n b))
           left (quot n b)]
      (if (zero? left)
        digs
        (recur (conj digs (rem left b)) (quot left b))))))

(defcheck solution-cbc5a08b
  (fn conv [n base]
    (if (number? n)
      (if (< n base)
        [n]
        (conv [(quot n base) (rem n base)] base))
      (if (< (first n) base)
        n
        (conv (vec (concat [(quot (first n) base) (rem (first n) base)] (rest n))) base)))))

(defcheck solution-cc0052cf
  (fn [n b]
    (let [p (reverse (take-while #(> (quot n %) 0) (iterate #(* b %) 1)))]
      (loop [x p y n out []]
        (if (empty? x)
          (if (empty? out)
            [0]
            out)
          (recur (rest x) (mod y (first x)) (conj  out (quot y (first x)))))))))

(defcheck solution-cc34fd1f
  (fn [n b]
    (loop [a n r '()]
      (if (< a b)
        (conj r a)
        (recur (quot a b) (conj r (mod a b)))
        ))))

(defcheck solution-cc5d1388
  (fn [n base]
    (loop [n n acc nil]
      (if (< n base)
        (conj acc n)
        (recur (quot n base) (conj acc (mod n base)))))))

(defcheck solution-ccf72e68
  (fn [n b]
    (if (= 0 n) [0]
                (reverse
                  (map #(mod % b)
                    (take-while pos?
                      (iterate #(quot % b)
                        n)))))))

(defcheck solution-ccfc6f4e
  (fn [n b]
    (if (zero? n) [0]
                  (loop [remaining n
                         pow 1
                         accum (list)]
                    (if (pos? remaining)
                      (let [place (Math/pow b (dec pow))
                            digit (int (/ (rem remaining (Math/pow b pow)) place))]
                        (recur (- remaining (* digit place))
                          (inc pow)
                          (conj accum digit)))
                      accum)))))

(defcheck solution-cd0c153b
  #(if (= % 0)
     [0]
     (let [m (Math/pow %2 (int (/ (Math/log %) (Math/log %2))))]
       (loop [n %
              m m
              s []]
         (if (< m 1)
           s
           (recur
             (mod n m)
             (/ m %2)
             (conj s (int (/ n m)))))))))

(defcheck solution-cd183b29
  (fn it
    ([n base] (it n base []))
    ([n base res]
     (let [q (quot n base)
           r (rem n base)
           nres (cons r res)
           ]
       (if (zero? q)
         nres
         (recur q base nres))))
    ))

(defcheck solution-cd96af26
  #(if (zero? %1)
     [0]
     (loop [n %1 ds []]
       (if (zero? n)
         ds
         (recur (quot n %2) (cons (mod n %2) ds))
         )
       )
     ))

(defcheck solution-ce7bc73c
  (fn set-base [n base]
    (loop [result '() n n]
      (cond
        (and (zero? n) (empty? result)) '(0)
        (zero? n) result
        :else
        (recur (cons (rem n base) result) (quot n base))
        )
      )
    ))

(defcheck solution-ceafaaf8
  (fn [n base]
    (if (zero? n)
      [n]
      (loop [digits () x n]
        (if (zero? x)
          digits
          (recur (conj digits (rem x base)) (int (/ x base))))))))

(defcheck solution-cf734102
  (fn cv [x base]
    (loop [x x digits (list)]
      (if (pos? x)
        (recur (quot x base) (conj digits (mod x base)))
        (if (empty? digits) [0] (into [] digits))))))

(defcheck solution-cfd3fbc6
  (fn base
    [n base]
    (letfn [(base-race [x moduli]
              (if (< 0 x)
                (base-race (quot x base) (conj moduli (mod x base)))
                (reverse moduli)))]
      (if (= 0 n) [0] (base-race n [])))))

(defcheck solution-cfe11707
  (fn [n b]
    (if (= 0 n) [0]
                (reverse
                  (for [x (iterate (partial * b) 1)
                        :while (<= x n)]
                    (mod (long (/ n x)) b))))))

(defcheck solution-d0160409
  (fn self [n b]
    (if (zero? n) '(0)
                  (loop [n n acc '()]
                    (if (zero? n) acc
                                  (recur (quot n b) (cons (mod n b) acc)))))))

(defcheck solution-d0f3deb1
  (fn [x b]
    (if (> x 0)
      (loop [ r [],  x x ]
        (if  (> x 0) (recur (cons (mod x b) r) (int (/ x b))) r)) [0])))

(defcheck solution-d2495637
  (fn [x base]
    (loop [acc [], rest x]
      (if (< rest base)
        (reverse (conj acc rest))
        (recur (conj acc (mod rest base)) (quot rest base))))))

(defcheck solution-d2927ec8
  (fn [n b]
    (loop [n n
           acc []]
      (cond (and (zero? n) (empty? acc)) [0]
            (= 1 n) (cons (mod n b) acc)
            (zero? n) acc
            :else (recur (/ (- n (mod n b)) b)
                    (cons (mod n b) acc))))))

(defcheck solution-d2aadaa0
  (fn [i n r]
    (if (not= n 0)
      (recur (conj i (rem n r)) (quot n r) r)
      (if (seq i) i [0]))) '())

(defcheck solution-d2b51310
  (fn digits [n base]
    (if (zero? n) [0]
                  (letfn [(digits-1 [n]
                            (when-not (zero? n)
                              (cons (mod n base) (digits-1 (quot n base)))))]
                    (reverse (digits-1 n))))))

(defcheck solution-d2be101c
  (fn [n base]
    (loop [n n
           r '()]
      (if (= 0 n)
        (if (empty? r) '(0) r)
        (recur (quot n base) (cons (rem n base) r))))))

(defcheck solution-d2c06947
  (fn [n base]
    (if (zero? n) [ 0 ]
                  (let [pows (reverse
                               (take-while
                                 #(->> % (Math/pow base) (quot n) pos?)
                                 (range)))
                        pow (fn [a b] (apply * (repeat b a)))]
                    (loop [n n
                           pows pows
                           digits []]
                      (if (empty? pows) (map int digits)
                                        (let [d (pow base (first pows))]
                                          (recur (rem n d)
                                            (next pows)
                                            (conj digits (quot n d))))))))))

(defcheck solution-d2cfbfd8
  (fn [n r]
    (->> (iterate #(* r %) 1)
      (map #(quot n %))
      (take-while #(> % 0))
      (map #(mod % r))
      reverse
      (#(if (empty? %) [0] %)))))

(defcheck solution-d3bf4330
  #(let [x (quot % %2)
         v (rem % %2)
         c (cons v %&)]
     (if (= 0 x)
       c
       (recur x %2 c))))

(defcheck solution-d3cd3025
  (fn boo
    ([n base] (if (= 0 n) [0] (boo n base base [])))
    ([n pow base accu]
     (let
      [term (mod n pow)
       pow-1 (/ pow base)]
       (cond
         (= 0 n) (reverse accu)
         :else (recur (- n term)
                 (* pow base)
                 base
                 (conj accu (/ term pow-1))))))))

(defcheck solution-d459036a
  (fn conv [x base]
    (loop [x' x r []]
      (if (< x' base ) (cons x' r)
                       (recur (quot x' base) (cons (rem x' base) r ))))))

(defcheck solution-d45e944f
  (fn [n base]
    (let
     [
      do-digits
      (fn do-digits [n]
        (if (> base n)
          [n]
          (let
           [
            d (mod n base)
            n' (quot n base)
            ]
            (cons
              d
              (do-digits n')
              )
            )
          )
        )
      ]
      (apply vector
        (reverse (
                   do-digits n)
          )
        )
      )
    ))

(defcheck solution-d5414eb7
  (fn f [n b]
    (let [q (quot n b)]
      (concat
       (if (pos? q)
         (f q b))
       [(rem n b)]))))

(defcheck solution-d5493c90
  (fn base-n-seq [n radix]
    (loop [n n return nil]
      (if (zero? n)
        (or return [0])
        (recur (quot n radix) (cons (rem n radix) return))))))

(defcheck solution-d557f1a3
  (fn [d b]
    (loop [result () digits d]
      (if (zero? digits)
        (if (empty? result)
          '(0)
          result)
        (recur (conj result (mod digits b)) (quot digits b))))))

(defcheck solution-d5632dbf
  (fn convert-base
    [n base]
    (letfn [(step [n base]
              (if (= 0 n) [nil]
                          (if (= 1 n) [1]
                                      (lazy-seq
                                        (cons (mod n base)
                                          (step (quot n base) base))))))]
      (remove nil? (if (= 0 n) [0] (reverse (step n base)))))))

(defcheck solution-d5eafd23
  (fn represent [number base]
    (if (< number base) [number]
                        (conj (represent (quot number base) base) (mod number base)))))

(defcheck solution-d616b21a
  (fn conv [n b] (if (< n b) [n] (conj (conv (quot n b) b) (rem n b)))))

(defcheck solution-d63647c3
  (fn [n base]
    (loop [n n ds '()]
      (let [q   (quot n base)
            ds' (cons (rem n base) ds)]
        (if (zero? q)
          ds'
          (recur q ds'))))))

(defcheck solution-d683d186
  (fn rebase [n b] (if (zero? (quot n b)) [n]
                                          (conj (rebase (quot n b) b) (mod n b)))))

(defcheck solution-d6e0a18a
  (fn [n base]
    (loop [current [] n n]
      (if  (= 0 n)
        (if (empty? current) [0] current)
        (recur (cons (mod n base) current) (quot n base))))
    ))

(defcheck solution-d7421701
  (fn digits [n base]
    (let [q (quot n base)
          r (rem n base)]
      (if (zero? q)
        [r]
        (conj (digits q base) r)))))

(defcheck solution-d77bd6e3
  (fn cb
    ([x b]
     (cb x b '()))
    ([x b rs]
     (let [q (quot x b), rs (cons (mod x b) rs)]
       (if (== 0 q) rs (recur q b rs))))))

(defcheck solution-d7c5e0f5
  (fn to-base
    [n b]
    (let [f (fn [acc n b]
              (if (>= 0 n) acc
                           (recur (conj acc (mod n b)) (int (/ n b)) b)))]
      (if (= n 0) [0]
                  (f '() n b)))))

(defcheck solution-d84da941
  ; could be simplier but ...
  (fn [n b]
    (reverse
      (loop [q (quot n b)
             r (rem n b)
             res []]
        (if (= q 0)
          (conj res r)
          (recur (quot q b) (rem q b) (conj res r)))
        ))))

(defcheck solution-d8506042
  (fn base [n b]
    (if (= n (mod n b))
      (vector n)
      (conj (base (int (/ n b)) b) (mod n b))
      )
    ))

(defcheck solution-d87617a1
  (fn [n b]
    (loop [ret '() cnt n]
      (let [r (rem cnt b)
            q (quot cnt b)
            n-r (cons r ret)]
        (if (= 0 q)
          n-r
          (recur n-r q))))))

(defcheck solution-d8fd318
  (fn to-digits [n radix]
    (if (< n radix)
      [n]
      (conj (to-digits (quot n radix) radix) (mod n radix)))))

(defcheck solution-d950d56b
  (fn f [n b]
    (if (< n b) [n]
                (conj (f (quot n b) b) (mod n b)))))

(defcheck solution-d97d3929
  (fn x [n m] (if (< n m) [n] (conj (x (quot n m) m) (rem n m)))))

(defcheck solution-da221cf3
  (fn digits-and-bases'
    ([num base] (digits-and-bases' num base []))
    ([num base accum]
     (let [result (int (/ num base))
           remainder (rem num base)]
       (if (< result base)
         (if (every? zero? [result remainder])
           (cons result accum)
           (concat [result remainder] accum))
         (recur result base (cons remainder accum)))))))

(defcheck solution-dab9c114
  (fn n->l [number base]
    (loop [n number acc '()]
      (let [q (quot n base) r (rem n base)]
        (if (>= q base)
          (recur q (conj acc r))
          (if (zero? q)
            (conj acc r)
            (conj acc r q)))))))

(defcheck solution-dafd0a49
  (fn digBase [x b] (if (>= x b) (conj (digBase (quot x b) b ) (rem x b)) [x])))

(defcheck solution-dafdc4da
  (fn digits [n base]
    (reverse
      (loop [d (quot n base)
             r (rem n base)
             ret (conj [] r)]
        (if (zero? d)
          ret
          (recur (quot d base) (rem d base) (conj ret (rem d base))))))))

(defcheck solution-db548875
  (fn [n b] (into [] (loop [n n r '()] (if (zero? n) (if (empty? r) '(0) r) (recur (quot n b) (conj r (rem n b))))))))

(defcheck solution-dbc70e8b
  (fn base-convert
    ([n b] (base-convert n b ()))
    ([n b c] (if (< n b)
               (vec (cons n c))
               (recur (int (/ n b)) b (cons (rem n b) c))))))

(defcheck solution-dbd20bf3
  (fn [n b]
    (if (zero? n) [0]
                  (loop [rv () value n power 1]
                    (let [nextp (* power b)
                          r (rem value nextp)]
                      (if (zero? value)
                        rv
                        (recur (conj rv (/ r power)) (- value r) nextp)))))))

(defcheck solution-dc5951dd
  (fn toseq [n,r]
    (if (< n r)
      [n]
      (conj (toseq (quot n r) r) (mod n r)))))

(defcheck solution-dcc7ce69
  (fn base-convert
    ([n b] (base-convert n b ()))
    ([n b c] (if (< n b)
               (vec (cons n c))
               (recur (quot n b) b (cons (rem n b) c))))))

(defcheck solution-dcfb72a5
  (fn convert-digit [n base]
    (loop [result '() n' n]
      (if
       (zero? (quot n' base))
        (conj result (rem n' base))
        (recur (conj result (rem n' base)) (quot n' base))))))

(defcheck solution-dd68ada1
  (fn num-repr-with-base [num base]
    (if (< num base)
      [num]
      (let [remainder (mod num base)
            quotient (quot num base)]
        (conj (num-repr-with-base quotient base)
          remainder)))))

(defcheck solution-dd83f6dc
  (fn [x b]
    (let [n (if (zero? x) 0 (int (/ (Math/log x) (Math/log b))))]
      (first (reduce (fn [[v r] i]
                       (let [bi (apply * (repeat i b))]
                         [(conj v (quot r bi)) (rem r bi)]))
               [[] x] (range n -1 -1))))))

(defcheck solution-ddfbf810
  (fn [rslt n b]
    (if (zero? n)
      (if (empty? rslt) [0] rslt)
      (recur (conj rslt (rem n b)) (quot n b) b)
      )  ) ())

(defcheck solution-df0a110a
  (fn [n b]
    (if (zero? n) [0]
                  (loop [n n, ds ()]
                    (if (zero? n) ds
                                  (recur (quot n b)
                                    (conj ds (mod n b))))))))

(defcheck solution-df233cc6
  (fn my-digits-base [x base]
    (if (< x base) [x]
                   (conj (my-digits-base (quot x base) base) (rem x base)))))

(defcheck solution-df58a355
  #(loop [n % v ()]
     (if (pos? n)
       (recur (quot n %2) (cons (mod n %2) v))
       (or (seq v) [0]))))

(defcheck solution-df68620c
  (fn dab
    ([d b] (dab d b []))
    ([d b r]
     (if (= 0 d)
       (if (empty? r) [0] r)
       (recur (quot d b) b (cons (rem d b) r))))))

(defcheck solution-df855c8e
  (fn [v b]
    (loop [v v b b acc []]
      (if (= v 0)
        (if (empty? acc)
          [0]
          acc)
        (let [vp (quot v b)
              r (rem v b)]
          (recur vp b (cons r acc)))))))

(defcheck solution-e0026916
  (fn digits [n base]
    (if (zero? n)
      [0]
      (loop [n n
             power 1
             acc ()]
        #_(println "n:" n "| power:" power "| acc:" acc)
        (if (zero? n)
          acc
          (recur (/ (- n (mod n base)) base)
            (inc power)
            (conj acc (mod n base))))))))

(defcheck solution-e009f5fa
  (fn [a b]
    (if (zero? a) [0]
                  (loop [a a r []]
                    (if (zero? a) r
                                  (recur (quot a b) (cons (mod a b) r)))))))

(defcheck solution-e039a443
  (fn f [n b] (let [q (quot n b)] (conj (if (= 0 q) [] (f q b)) (mod n b)))))

(defcheck solution-e04c0b3e
  (fn [n b]
    (loop [acc () n n]
      (if (> b n)
        (conj acc n)
        (let [q (quot n b) r (rem n b)]
          (recur (conj acc r) q))))))

(defcheck solution-e07b9b73
  (fn cvt [n0 b0]
    (let [answer (reverse (flatten ((fn cvt1 [n b]
                                      (let [x (int (/ n (* 1.0 b)))]
                                        (conj [(mod n b)]
                                          (if (>= n b)
                                            (cvt1  x b)
                                            x)))) n0 b0)))]
      (if (and (> (count answer) 1)
               (zero? (first answer))) (rest answer) answer))))

(defcheck solution-e093e548
  (fn
    [number base]
    (loop [number number result []]
      (if (= number 0)
        (if (empty? result) [0] (apply vector (reverse result)))
        (recur (quot number base) (conj result (mod number base)))
        )
      )
    ))

(defcheck solution-e0b650a8
  (fn d [n b]
    (loop [n n
           acc '()]
      (let [r (rem n b)
            q (quot n b)]
        (if (> q 0)
          (recur q (cons r acc))
          (cons r acc)
          )
        )
      )))

(defcheck solution-e0d57abc
  (fn base-digits [x b]
    (let [powers (take-while #(>= x (Math/pow b %)) (range))]
      (loop [p_ powers x_ x acc []]
        (if (empty? p_) [x]
                        (let [d_ (int (/ x_ (Math/pow b (last p_))))]
                          (cond
                            (= [0] p_) (conj acc d_)
                            :else (recur (butlast p_)
                                    (- x_ (* d_ (Math/pow b (last p_))))
                                    (conj acc d_)))))))))

(defcheck solution-e121c4a6
  (fn [x b]
    (loop [x x i b r '()]
      (if (< x b)
        (conj r x)
        (recur (quot x b) (* i b) (conj r (rem x b)))))))

(defcheck solution-e13c1087
  (fn digits[x base]
    (letfn [(impl [x base dd]
              (if (zero? x) dd
                            (recur (quot x base) base (cons (mod x base) dd))))]
      (if (zero? x) (list 0)
                    (impl x base nil)))))

(defcheck solution-e13c85d3
  (fn [n b]
    ((fn encontra-resp [numero base maior-casa]
       (if (zero? maior-casa)
         (list numero)
         (let [digito ((fn encontra-numero [numero casa base]
                         (dec (some #(when (< numero (* % (apply * (repeat casa base))))
                                       %)
                                (map inc (range)))))
                       numero
                       maior-casa
                       base)]
           (cons digito (encontra-resp (- numero (* digito (apply * (repeat maior-casa base)))) base (dec maior-casa))))))
     n
     b
     ((fn [nume bas]
        (dec (some #(when (> (second %) nume)
                      (first %))
               (map #(vector % (apply * (repeat % bas)))
                 (map inc (range))))))
      n
      b))))

(defcheck solution-e16cec29
  (fn numberize [n b]
    (if (zero? n) [0]
                  (loop [exp (Math/floor (/ (Math/log n) (Math/log b)))
                         left n
                         acc []]
                    (let [
                          baseExp (Math/pow b exp)
                          digit (int (/ left baseExp))
                          left (- left (* digit baseExp))
                          acc' (conj acc digit)
                          ]
                      (if (zero? exp)
                        acc'
                        (recur (dec exp) left acc'))
                      )))))

(defcheck solution-e1ddda4c
  (fn [v b]
    (loop [i v r []]
      (if (< i b)
        (cons i r)
        (recur (int (/ i b)) (cons (mod i b) r))))))

(defcheck solution-e21ceb29
  (fn [n base]
    (loop [r [], x n]
      (let [q (quot x base), m (mod x base)]
        (if (= q 0)
          (reverse (conj r m))
          (recur (conj r m) q)
          )
        )
      )
    ))

(defcheck solution-e266d9b9
  (fn convert [n b]
    (let [magnitudes (reverse (take-while #(<= % n)
                                (map #(Math/pow b %)
                                  (range))))
          digits ((fn calculate-magnitudes [current-n current-magnitudes]
                    (if (empty? current-magnitudes)
                      []
                      (cons (int (/ current-n (first current-magnitudes)))
                        (calculate-magnitudes (mod current-n (first current-magnitudes))
                          (rest current-magnitudes)))))
                  n magnitudes)]
      (if (empty? digits)
        [0]
        digits))))

(defcheck solution-e2f24ae2
  (fn [n b]
    (loop [n n
           d '()]
      (if (zero? n)
        (if (empty? d) [0] d)
        (recur (quot n b) (conj d (rem n b)))))))

(defcheck solution-e30cf546
  (fn [n b]
    (letfn [(digits  [n b]
              (if (= n 0) []
                          (conj (digits (quot n b) b) (rem n b))))]
      (let [r (digits n b)]
        (if (empty? r) [0] r)))))

(defcheck solution-e3488e47
  (fn [n b]
    (loop [res '()
           cur n]
      (if (< cur b)
        (conj res cur)
        (recur (conj res (mod cur b)) (int (/ cur b)))))))

(defcheck solution-e3d752e8
  (fn [nn bb]
    (let [r ((fn cal [n b]
               (if (> n 0)
                 (conj (cal (quot n b) b) (rem n b) )
                 []
                 )
               ) nn bb)]
      (if (= (count r) 0)
        [0]
        r
        )
      )
    ))

(defcheck solution-e4023b0d
  (fn digitSeq [n b]
    (loop [currNum n
           currDigits '()]
      (if (= currNum 0)
        (if (empty? currDigits)
          '(0)
          currDigits)
        (recur (int (/ currNum b)) (conj currDigits (mod currNum b)))))))

(defcheck solution-e4260fa9
  (fn f ([x b] (if (= x 0) '(0) (f x b nil)))
    ([x b coll]
     (if (= x 0) coll (recur (quot x b) b (cons (mod x b) coll))))))

(defcheck solution-e4c0ddde
  (fn [num base]
    (let [powers (map int
                   (map #(Math/pow base %)
                     (reverse (take-while #(>= num (Math/pow base %))
                                (range)))))
          result-map (reduce (fn [acc x]
                               (let [digits (:digits acc)
                                     remainder (:remainder acc)]
                                 {:digits (conj digits
                                            (int (/ remainder x)))
                                  :remainder (rem remainder x)}))
                       {:digits [] :remainder num}
                       powers)]
      (or (seq (:digits result-map)) [0]))))

(defcheck solution-e4c70a99
  (fn f [n b] (if (< n b) [n] (conj (f (quot n b) b) (rem n b)))))

(defcheck solution-e4de8db4
  (fn dib2
    ([n b] (if (= 0 n) (list 0) (dib2 n b ())))
    ([n b r] (if (= 0 n) r (recur (quot n b) b (cons (rem n b) r))))))

(defcheck solution-e50e8a61
  (fn [init base] (loop [i init c []]
                    (if (= i 0)
                      (if (empty? c) [0] c)
                      (recur (quot i base) (cons (rem i base) c))))))

(defcheck solution-e53fc709
  (fn [num sys]
    (letfn [(highest [num sys]
              (loop [x num d sys]
                (if (zero? (quot x (* d sys)))
                  d
                  (recur x (* d sys)))))]
      (loop [x num d (highest num sys) r []]
        (if (or (= 1 d) (zero? x))
          (conj r x)
          (recur (- x (* (quot x d) d)) (quot d sys) (conj r (quot x d))))))))

(defcheck solution-e6475f10
  (fn r [x n]
    (loop [num x res '()]
      (if (zero? num)
        (if (= '() res) '(0) res)
        (recur (quot num n) (conj res (mod num n)))))))

(defcheck solution-e66978ac
  #(if (= %1 0) [0] (reverse ((fn f [n d] (if (= 0 n) '() (cons (rem n d) (lazy-seq (f (quot n d) d))))) %1 %2))))

(defcheck solution-e7604758
  (fn f [n b]
    (letfn [(digits [n a]
              (if (= n 0)
                a
                (let [d (mod n b)]
                  (recur (/ (- n d) b) (conj a d)))))]
      (if (= 0 n) [0] (digits n '())))))

(defcheck solution-e7e5c9d
  (fn [n base]
    (if (= n 0)
      [0]
      (vec (reverse ((fn inner [n] (if (zero? n) nil (cons (rem n base) (lazy-seq (inner (quot n base)))))) n))))))

(defcheck solution-e7e8b4df
  (fn [x base]
    (if (zero? x) (vector 0)
                  (loop [x x base base ret '()]
                    (if (zero? x) ret
                                  (recur (quot x base) base (conj ret (mod x base))))))))

(defcheck solution-e807d4aa
  (fn x[n b] (let [r (rem n b) q (quot n b)] (if (> q 0) (conj (x q b) r) [r]))))

(defcheck solution-e84f2cc7
  (fn [num base]
    (loop [q (quot num base) rm (rem num base) r [rm]]
      (if (zero? q) r
                    (recur (quot q base) (rem q base) (cons (rem q base) r))
                    )
      )
    ))

(defcheck solution-e8b1001a
  (fn [x weight]
    (loop [n x s ()]
      (if (zero? n)
        (if (empty? s) '(0) s)
        (recur (int (/ n weight)) (conj s (mod n weight)))))))

(defcheck solution-e8d7259
  (fn db4 [n b]
    (loop [n n,
           q (quot n b),
           result []]
      (if (< n b)
        (reverse (conj result n))
        (recur (quot n b) (quot q b) (conj result (rem n b)))))))

(defcheck solution-e9a49aa0
  #(or
    ((fn f [n b]
       (when (> n 0)
         (conj (vec (f (quot n b) b)) (mod n b)))) % %2)
    [0]))

(defcheck solution-ea474e84
  (fn nb->d
    ([n b]
     (let [max-pow (if (= n 0)
                     0
                     (int (/ (Math/log n) (Math/log b))))]
       (nb->d n b max-pow [])))
    ([n b digit-pos prev]
     (let [digit-value (reduce * (repeat digit-pos b))
           digit (int (/ n digit-value))
           remainder (- n (* digit digit-value))]
       (if (= digit-pos 0)
         (conj prev digit)
         (recur remainder b (dec digit-pos) (conj prev digit)))))))

(defcheck solution-ea85ae22
  #(loop [n %,res '()] (if (zero? n) (if (empty? res) '(0) res) (recur (int (/ n %2)) (conj res (mod n %2))))))

(defcheck solution-eaa702c0
  (fn digits [n b]
    (loop [r n
           acc []]
      (let [s (cons (mod r b) acc)
            q (quot r b)]
        (if (zero? q) s
                      (recur q s))))))

(defcheck solution-eae528a3
  (fn [n base]
    (if (zero? n) [0]
                  (loop [nn n acc '()]
                    (if (zero? nn) acc
                                   (recur (quot nn base) (cons (mod nn base) acc)))))))

(defcheck solution-eafd9871
  (fn [n b]
    (loop [n n r '()]
      (if (= n 0)
        (if (= (count r) 0) [0] r)
        (recur (quot n b) (conj r (mod n b)))))))

(defcheck solution-eb210fd1
  #(letfn [(ds [n] (if (pos? n) (conj (ds (quot n %2)) (rem n %2)) []))]
     (if (zero? %) [%] (ds %))))

(defcheck solution-eb3c8c9e
  (fn to-base [n b]
    (letfn [(to-num [n b]
              (if (<= n 1) [n]
                           (cons (mod n b) (to-num (quot n b) b))))]
      (if (= 0 n) [0]
                  (drop-while #(= 0 %) (reverse (to-num n b)))))))

(defcheck solution-eb82f3db
  (fn [n b]
    (if (zero? n) [0] (->> (iterate #(quot % b) n) (take-while pos?) (map #(mod % b)) reverse ))))

(defcheck solution-ec61160
  (fn [ni b]
    (loop [n (quot ni b)
           a (list (mod ni b))]
      (if (zero? n)
        a
        (recur (quot n b) (conj a (mod n b)))))))

(defcheck solution-ec630e84
  (fn base [n b]
    (loop [n n
           ans (list)]
      (let [q (quot n b)
            r (mod n b)
            newans (conj ans r)]
        (if (zero? q)
          newans
          (recur q newans))))))

(defcheck solution-ecac8007
  (fn [n b]
    (letfn [(f [v a]
              (if (< v b)
                (cons v a)
                (let [d (rem v b)
                      m (quot v b)]
                  (f m (cons d a))) ))]
      (f n ()))))

(defcheck solution-ecd32f61
  (fn [n base]
    (if (= n 0)
      '(0)
      (loop [l ()
             c n]
        (if (= c 0)
          l
          (recur (conj l (rem c base)) (int (/ c base))))))))

(defcheck solution-ecd6846a
  (fn [n b]
    (letfn [(base [n]
              (let [v (mod n b)
                    r (int (/ n b))]
                (if (zero? n)
                  []
                  (conj (base r) v))))]
      (if (= n 0) [0]
                  (base n)))))

(defcheck solution-ecffaa05
  (fn base [x b]
    (if (< x b)
      [x]
      (conj (base (quot x b) b) (mod x b))
      )
    ))

(defcheck solution-ed08637f
  (fn [n b]
    (if (pos? n)
      (->> (iterate #(quot % b) n)
        (take-while pos?)
        (map #(mod % b))
        reverse)
      [0])))

(defcheck solution-ed0c167
  (fn ! [n b]
    (letfn [(
              aux [n b c]
              (if (zero? n) c
                            (aux (quot n b) b (cons (rem n b) c))
                            )
              )]
      (let [ta (aux n b [])]
        (if (empty? ta) [0] ta)
        )
      )))

(defcheck solution-ed5e0283
  (fn [n b]
    (if (< n b) [n]
                (#(if (= % 0) %2
                              (recur (quot % b) (cons (mod % b) %2)))
                 n []))))

(defcheck solution-ed711e9e
  (fn digits [num base]
    (if (zero? num)
      '(0)
      (loop [num num res ()]
        (if (zero? num)
          res
          (recur (quot num base) (conj res (rem num base)))
          )))))

(defcheck solution-ed7d837a
  (fn [num b]
    (let [exp (fn [t] (reduce * (repeat t b)))]
      (if (= num 0)
        [0]
        (if (= num b)
          [1 0]
          (loop [e 6
                 out []
                 r num]
            (if (<= r 0)
              (drop-while (partial = 0) out)
              (if (< r (long (exp e)))
                (recur (dec e)
                  (conj out 0)
                  r)
                (recur (dec e)
                  (conj out (quot r (long (exp e))))
                  (- r (* (long (exp e)) (quot r (long(exp e))))))))))))))

(defcheck solution-ed9e8bbd
  (partial
    (fn [sq v b]
      (let [r (mod v b)
            d (quot v b)]
        (if (and (zero? v) (not (empty? sq)))
          sq
          (recur (cons r sq) d b))))
    ()))

(defcheck solution-ee284fa
  (fn [number base]
    (loop [nnum (quot number base)
           result [(rem number base)]]
      (if (= nnum 0)
        (reverse result)
        (recur (quot nnum base)
          (conj result (rem nnum base)))))))

(defcheck solution-ee438a12
  (fn [n b]
    (loop [c n r []]
      (if (< c b)
        (cons c r)
        (recur (int (/ c b)) (cons (rem c b) r))))))

(defcheck solution-ef1f25d8
  (fn [fig base] (if (zero? fig) [0] (let [register (reverse (take-while #(<= %1 fig) (iterate #(* base %) 1)))]
                                       (take (count register) (concat (loop [acc [] rem fig reg register]
                                                                        (if (zero? rem) acc
                                                                                        (let [reg-mult (int (/ rem (first reg)))]
                                                                                          (recur (conj acc reg-mult) (- rem (* reg-mult (first reg))) (rest reg)))))
                                                                      (repeat 0)))))))

(defcheck solution-eff34c86
  (fn [n base]
    (if (zero? n)
      [0]
      (let [start (int (Math/pow base (quot (Math/log n) (Math/log base))))]
        (->> (take-while #(>= % 1) (iterate #(/ % base) start))
          (reductions (fn [[_ x] pow] ((juxt quot rem) x pow)) [0 n])
          rest
          (map first))))))

(defcheck solution-f02e9943
  (fn digibase [value base]
    (loop [res '()
           val value]
      (let [curdig (mod val base)
            remval (quot val base)]
        (if (> remval 0)
          (recur (conj res curdig) remval)
          (conj res curdig))))
    ))

(defcheck solution-f0697488
  (fn f [x b]
    (loop [x x, s ()]
      (cond
        (> (quot x b) 0) (recur (quot x b) (conj s (mod x b)))
        :else (conj s (mod x b))))))

(defcheck solution-f07d5217
  (fn [n b]
    (loop [i n r '()]
      (if (= i 0)
        (if (= r '()) '(0) r)
        (recur (quot i b) (conj r (rem i b)))))))

(defcheck solution-f0e9cbed
  (fn [x y]
    (if (= x 0)
      [0]
      ((fn base [v b]
         (if (= 0 v)
           []
           (conj (base (quot v b) b) (mod v b)))) x y))))

(defcheck solution-f1049943
  (fn [x base] (if (zero? x) [0] ((fn [x digits base] (if (zero? x) digits (recur (quot x base) (cons (mod x base) digits) base))) x [] base))))

(defcheck solution-f10709a0
  (fn nums-by-base [x base]
    (letfn [(num-seq-by-base [n base]
              (if (zero? n) []
                            (conj (num-seq-by-base (quot n base) base) (rem n base))))]
      (if (zero? x) [0]
                    (num-seq-by-base x base)))))

(defcheck solution-f16c5bd5
  (fn digits-in-base [num base]
    (if (< num base)
      [num]
      (conj
        (digits-in-base (quot num base) base)
        (rem num base)))))

(defcheck solution-f1a79975
  (fn [x b]
    (if (zero? x)
      [x]
      ((fn r [x]
         (if (zero? x)
           []
           (conj (r (quot x b)) (mod x b))))
       x))))

(defcheck solution-f1bc1b88
  (fn [n b]
    (let [digi (fn digi [n b]
                 (lazy-seq (when (> n 0)
                             (cons (mod n b) (digi (int (/ n b)) b)))))]
      (if (zero? n) [0] (reverse (digi n b))))))

(defcheck solution-f1f26e76
  (fn base [n b]
    (let [q (quot n b) r (rem n b)]
      (if (zero? q)
        [r]
        (conj (base q b) r)))))

(defcheck solution-f29a42d2
  #(->((fn i [n b]
         (let [q (quot n b)]
           (if (zero? q) (list n) (lazy-seq (cons (mod n b) (i q b)))))) %1 %2) reverse))

(defcheck solution-f2c5aeac
  (fn base [m n]
    (if (zero? m) [0]
                  (if (zero? (quot m n)) [m]
                                         (conj (base (quot m n) n) (rem m n))))))

(defcheck solution-f2dc184
  #(loop [n %1 o []]
     (if (zero? n)
       (if (empty? o) [0] o)
       (recur (int (/ n %2)) (cons (mod n %2) o))
       )
     ))

(defcheck solution-f36f228f
  (fn a [n b] (if (< n b) [n] (conj (a (quot n b) b) (rem n b)))))

(defcheck solution-f383019d
  (fn dig-and-bases [num base]
    (if (zero? num)
      [0]
      (loop [acc [], n num]
        (if (zero? n)
          acc
          (recur (cons (rem n base) acc) (quot n base)))))))

(defcheck solution-f3a7ead8
  (fn [x b]
    (first (reduce (fn [[acc r] i]
                     (let [y (apply * (repeat i b))]
                       (if (> y r)
                         [(conj acc 0) r]
                         [(conj acc (quot r y)) (rem r y)])))
             [[] x]
             (-> (count (take-while #(<= % x) (iterate #(* b %) b)))
               (range -1 -1))))))

(defcheck solution-f3cdff0a
  (fn conv [num base]
    (letfn [(conv* [n b]
              (if (= n 0) '(0)
                          (cons (mod n b)
                            (lazy-seq
                              (conv* (quot n b) b)))))]
      (let [[head & tail] (rseq (vec (conv* num base)))]
        (if (and (= head 0) (= tail nil))
          '(0)
          tail)))))

(defcheck solution-f3fdcc0b
  (fn f [n b]
    (if (< n b) [n]
                (let [q (quot n b) r (rem n b)]
                  (conj (f q b) r)))))

(defcheck solution-f40f45df
  (fn [n b] (let [
                  i (map #(apply * (repeat % b))
                      (range))
                  l (take-while #(<= % n) i)]
              (if (= 0 n) [0] (loop [v [] c l m n]
                                (if (empty? c) v
                                               (recur (conj v (quot m (last c)))
                                                 (butlast c) (rem m (last c)))))
                          ))))

(defcheck solution-f43085cf
  (fn f [num base]
    (loop [num num
           acc ()]
      (if (< num base)
        (cons num acc)
        (recur (quot num base) (cons (rem num base) acc))))))

(defcheck solution-f48b4ffc
  (fn [x b]
    (if (= 0 x)
      [0]
      (loop [x x
             digs ()]
        (if (zero? x)
          (vec digs)
          (recur (quot x b) (cons (rem x b) digs)))))))

(defcheck solution-f4f653fe
  (fn convert-to-base [n base]
    (let [n' (quot n base)
          r (mod n base)]
      (if (zero? n')
        [r]
        (conj (convert-to-base n' base) r)))))

(defcheck solution-f51f8f97
  (fn f [n base]
    (if (< n base)
      [n]
      (conj (f (quot n base) base) (rem n base)))))

(defcheck solution-f52c5129
  (fn digits [n base]
    {:pre [(integer? n), (not (neg? n))]}
    (loop [acc '(), n n]
      (if (<= 0 n (dec base))
        (cons n acc)
        (recur (cons (rem n base) acc) (quot n base))))))

(defcheck solution-f53f0c1a
  (fn [n base]
    (loop [xs () x n]
      (if (< x base) (conj xs x) (recur (conj xs (mod x base)) (quot x base))))))

(defcheck solution-f5ab696f
  (fn base [n b]
    (loop [_n n
           ret '()]
      (if (< _n b)
        (conj ret (mod _n b))
        (recur (quot _n b) (conj ret (mod _n b)))
        ))))

(defcheck solution-f612b32e
  (fn [n b]
    (if (= n 0) [0]
                (apply vector (reverse ((fn f [n]
                                          (if (> n 0)
                                            (conj (lazy-seq (f (int (/ n b))))(mod n b)))) n))))))

(defcheck solution-f613377b
  (fn [v1 n] (loop [v (/ v1 n) r [(long (mod v1 n))]]
               (if (= 0 (long v)) (reverse r)
                                  (recur (/ v n) (conj r (long (mod v n))) )
                                  ))))

(defcheck solution-f658a443
  (fn nseq
    ([n base] (if (= 0 n) [0] (nseq n base [])))
    ([n base r]
     (if (= 0 n)
       r
       (recur (quot n base) base (vec (cons (rem n base) r)))))))

(defcheck solution-f6a7b629
  (fn [n b] (if (pos? n) (letfn [(g [n b] (if (pos? n) (let [q (quot n b) r (rem n b)]  (conj (g q b) r)) []))] (g n b)) [0])))

(defcheck solution-f6d9f651
  (fn [x y]
    (if (= x 0) [0]
                (reverse
                  (loop [a x b []]
                    (if (= a 0)
                      b
                      (recur (/ (- a (mod a y)) y) (conj b (mod a y)))))))))

(defcheck solution-f7926cee
  (fn [num base]
    (loop [acc [] n num]
      (if (< n base)
        (reverse (conj acc n))
        (let [r (rem n base)]
          (recur (conj acc r) (/ (- n r) base)))))))

(defcheck solution-f7c0fc89
  (fn digib [n b]
    (if (< n b)
      (list n)
      (flatten (reverse
                 (vector (rem n b) (digib (quot n b) b)))))))

(defcheck solution-f7d3844e
  (fn [n b]
    (loop [x n y b res ()]
      (if (and (= x 0) (not= y b)) res
                                   (recur (- x (mod x y)) (* y b) (cons (/ (mod x y) (/ y b)) res))))))

(defcheck solution-f7e5f36e
  (fn [n b]
    (if (= n 0) '(0)
                (loop [i n ret '()]
                  (if (= i 0) ret
                              (recur
                                (quot i b)
                                (conj ret (rem i b))))))))

(defcheck solution-f7feabc
  (fn f [n b]
    (cond (= n 0) [0]
          (< n b) [n]
          :else (conj (f (int (/ n b)) b) (mod n b)))
    ))

(defcheck solution-f82e12e7
  (fn basec [n b]
    (loop [num n conv (list)]
      (let [q (quot num b)
            r (rem num b)]
        (if (zero? q)
          (conj conv r)
          (recur q (conj conv r)))))))

(defcheck solution-f86ffedc
  (fn digits [n b]
    (if (< n b)
      [n]
      (conj (digits (quot n b) b) (mod n b)))))

(defcheck solution-f9102da7
  (fn [n rdx]
    (loop [q (quot n rdx)
           r (rem n rdx)
           a []]
      (if (zero? q)
        (cons r a)
        (recur (quot q rdx)
          (rem q rdx)
          (cons r a))))))

(defcheck solution-f93d4dad
  (fn t [q b]
    (if (pos? q)
      (let [r (rem q b)
            Q (quot q b)]
        (if (pos? Q) (conj (t Q b) r) [r]))
      [0])))

(defcheck solution-f9bccdf9
  (fn digits [n b]
    (if (< n b) [n] (conj (digits (quot n b) b) (mod n b)))))

(defcheck solution-fa2ee798
  (fn dgb [n b]
    (loop [acc [] n n]
      (if (>= n b)
        (recur (cons (mod n b) acc) (quot n b))
        (cons n acc)))))

(defcheck solution-fae4bbd2
  (fn [n b]
    (if (zero? n)
      [0]
      (let [ds (map first
                 (take-while #(not= 0 (first %) (second %))
                   (drop 1
                     (iterate (fn [[_ n]] ((juxt mod quot) n b)) [0 n]))
                   ))]
        (reverse ds)))))

(defcheck solution-fb04745f
  #(if (zero? %1) (list 0) (reverse ((fn this [x base] (when (pos? x)  (cons (mod x base) (this (int (/ x base)) base)))) %1 %2))))

(defcheck solution-fb28da98
  (let [f
        (fn [current result base]
          (if (= 0 current)
            result
            (recur (quot current base) (cons (rem current base) result) base)
            )
          )]
    (fn [value base] (if (= 0 value) [0] (f value [] base)))
    ))

(defcheck solution-fb53791a
  (fn digits [x b]
    (letfn [(digits0 [x b]
              (loop [x x, d '()]
                (if (zero? x) (cons 0 d)
                              (recur (int (/ x b))
                                (cons (mod x b) d)))))]
      (let [d (digits0 x b)]
        (if (= d '(0)) d (rest d))))))

(defcheck solution-fb540657
  (fn [n b]
    (if (zero? n)
      [0]
      ((fn f [n' b']
         (if (zero? n')
           []
           (conj (f (quot n' b') b') (rem n' b'))))
       n b))))

(defcheck solution-fb71fd29
  #(if (= %1 0) [0]
                ((fn d [n b]
                   (if (> n 0)
                     (conj (d (int (/ n b)) b) (mod n b))
                     [])) %1 %2)))

(defcheck solution-fc843ddc
  (fn f [n b]
    (if (< n b)
      [n]
      (conj (f (quot n b) b)
        (rem n b)))))

(defcheck solution-fca8d304
  (fn to-digits [n base]
    (let [r (rem n base)
          n' (int (/ n base))]
      (if (= 0 n')
        [r]
        (vec (concat (to-digits n' base) [r]))))))

(defcheck solution-fd0251de
  (fn [d b]
    (loop [r () d d]
      (if (< d b) (conj r d)
                  (recur (conj r (mod d b)) (int (/ d b)))
                  ))
    ))

(defcheck solution-fd57ebb0
  #((fn f[n a c](if(< n %2)(c n a)(f(quot n %2)(c(rem n %2)a)c)))%()cons))

(defcheck solution-fe1434a5
  (fn reductor
    ([n b]
     (if (= n b) [1 0]
                 (letfn [(max-base [n b]
                           (loop [v 1
                                  i 0]
                             (if (> v n)
                               (- i 1)
                               (recur (* v b) (+ i 1)))))]
                   (reductor n b (max-base n b) []))))
    ([n b start bits]
     #_(println n b start bits)
     (letfn [(pow [b e]
               (apply * (repeat e b)))]
       (let [factor (int (/ n (pow b start)))
             diff (* (pow b start) factor)
             nn (- n diff)]
         (cond
           (= nn 0)
           (conj bits factor)

           (< nn 0)
           (recur n b (- start 1) bits)

           :else
           (recur nn b (- start 1) (conj bits factor))))))))

(defcheck solution-fe2ed40d
  (fn [n base]
    (if
     (zero? n)
      [0]
      (rest
        (reverse
          (map
            #(mod % base)
            (take-while
              (comp not nil?)
              (iterate
                #(if
                  (zero? %)
                   nil
                   (int (/ % base))) n))))))))

(defcheck solution-fe3c376d
  (fn [n radix]
    (if (zero? n)
      [0]
      (reverse
        (map
          #(int (rem % radix))
          (take-while (complement zero?)
            (iterate #(quot % radix)
              n)))))))

(defcheck solution-fe6ec7d8
  #(let[q(quot % %2) l(cons(rem % %2)%&)](case q 0 l(recur q %2 l))))

(defcheck solution-feaea008
  (fn f [n b]
    (let [r (rem n b)
          q (quot n b)]
      (if (= 0 q)
        [r]
        (conj (f q b) r)))))

(defcheck solution-feeedce3
  (fn ! [n b]
    (if (< n b) [n]
                (conj (! (quot n b) b) (mod n b)))))

(defcheck solution-fef4f76b
  (fn num-conv [num base]
    (if (zero? num) '(0)
                    (loop [result []
                           n num]
                      (if (zero? n)
                        (reverse result)
                        (recur (conj result (mod n base)) (quot n base)))))))

(defcheck solution-ff531f01
  (fn to-base [n b]
    (loop [n n num ()]
      (if (zero? n)
        (if (empty? num) [0] num)
        (recur (quot n b) (cons (mod n b) num))))))

(defcheck solution-ff64d9e7
  (fn [n b]
    (let [base-seq (iterate #(* % b) b)
          take-x   (reverse (take-while (partial >= n) base-seq))
          quot-rem #(concat
                     (butlast %1)
                     [(quot (last %1) %2)
                      (rem  (last %1) %2)])
          result   (reduce quot-rem [n] take-x)]
      result)))

(defcheck solution-ff6ea276
  (fn [r n b]
    (if (= n 0)
      (if (seq r) r [0])
      (recur (cons (mod n b) r) (int (/ n b)) b))) ())

(defcheck solution-ff85b808
  (fn [value base]
    (loop [t () v value]
      (if (= v 0)
        (if (empty? t) [0] t)
        (recur (conj t (rem v base)) (quot v base))))))

(defcheck solution-ff8cc13
  (fn digits[x y]
    (loop [z x
           acc []]
      (let [remainder (rem z y)
            quotient (quot z y)]
        (if (= 0 quotient)
          (reverse (conj acc z))
          (recur quotient (conj acc remainder)))))))

(defcheck solution-ffc021f0
  (fn [x b]
    (if (zero? x)
      [0]
      (loop [x x r []]
        (if (pos? x)
          (recur (quot x b)
            (cons (rem x b) r))
          r)))))

(defcheck solution-ffe1cca7
  (fn [n b] (loop [n n v ()] (let [d (rem n b) r (int (/ n b))] (if (zero? r) (vec (conj v d)) (recur r (conj v d)))))))

(defcheck solution-fff4edfa
  (fn [n base]
    (if (= n 0) [0]
                ((fn [n base acc]
                   (if (= 0 n) acc
                               (recur (quot n base)
                                 base
                                 (cons (rem n base) acc)))) n base []))))
