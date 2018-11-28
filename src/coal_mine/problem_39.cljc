(ns coal-mine.problem-39
  (:require [coal-mine.checks :refer [defcheck-39] :rename {defcheck-39 defcheck}]
            [clojure.test]))

(defcheck solution-10a57d5d
  (fn [x, y]
    (loop [seq1 x seq2 y acc []]
      (if (or (empty? seq1) (empty? seq2))  acc
                                            (recur (rest seq1) (rest seq2) (conj acc (first seq1) (first seq2))
                                              )))))

(defcheck solution-1127bad9
  #(loop [col1 %1 col2 %2 result ()]
     (if (or (empty? col1) (empty? col2))
       result
       (recur (rest col1)(rest col2)(concat result (list (first col1) (first col2)))))))

(defcheck solution-11a9cd7a
  (fn ileave [c1 c2]
    (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                             (ileave (rest s1) (rest s2)))))))))

(defcheck solution-11be55ef
  #(reverse (loop [r '()
                   c1 %1
                   c2 %2]
              (if (or (empty? c1) (empty? c2))
                r
                (recur (conj r (first c1) (first c2)) (rest c1) (rest c2))))))

(defcheck solution-11d0b8e2
  (fn interleave'
    [c1 c2]
    (mapcat list c1 c2)))

(defcheck solution-11d3b30a
  (fn [col1 col2]
    (loop [a col1 b col2 c []]
      (if (or (nil? a) (nil? b))
        c
        (recur (next a) (next b) (conj c (first a) (first b)))))))

(defcheck solution-12b76feb
  (fn [a b] (loop [a a b b r '()]
              (if (or (empty? a) (empty? b))
                r
                (recur (rest a) (rest b) (concat r (vector (first a) (first b))
                                                 ))
                ))))

(defcheck solution-12d582b3
  (fn[s1 s2]
    (mapcat (fn[a b] [a b]) s1 s2)))

(defcheck solution-138cd8ae
  (fn [s1 s2]
    (letfn [(inter [s1 s2 r] (cond (empty? s1) r
                                   (empty? s2) r
                                   :else (inter (rest s1) (rest s2) (conj (conj r (first s1)) (first s2)))))]
      (inter (seq s1) (seq s2) []))))

(defcheck solution-1501274c
  (fn [s1 s2] (mapcat #(vector %1 %2) s1 s2)))

(defcheck solution-158a0043
  (fn il [a b]
    (if (or (empty? a) (empty? b))
      []
      (concat [(first a) (first b)] (il (rest a) (rest b))))))

(defcheck solution-15ff0f6e
  (fn my-il [s1 s2]
    (if (or (empty? s1) (empty? s2))
      '()
      (cons (first s1)
        (cons (first s2)
          (lazy-seq (my-il (rest s1) (rest s2))))))))

(defcheck solution-160ac2f0
  (fn [a b]
    (loop [acc [] a a b b]
      (if (and (seq a) (seq b))
        (recur (conj acc (first a) (first b))
          (rest a) (rest b))
        acc))))

(defcheck solution-161d5b8c
  (fn [s1 s2]
    (letfn [(hf [result s1 s2]
              (if (or (empty? s1) (empty? s2))
                result
                (recur
                  (conj result (first s1) (first s2))
                  (rest s1)
                  (rest s2))))]
      (hf [] s1 s2))))

(defcheck solution-16393e80
  ;;map takes more than one collection !! $.each(coll1,function(k,v){ans+=coll1[k]+coll2[k]}
  #(flatten (map vector %1 %2)))

(defcheck solution-17de774a
  (fn [a b] (flatten (map list a b))))

(defcheck solution-1822794e
  (fn [l r]
    (loop [f []
           lm l
           rm r]
      (if (or (nil? lm)
              (nil? rm))
        f
        (recur (conj (conj f (first lm)) (first rm)) (next lm) (next rm))))))

(defcheck solution-18c21c6
  (fn f [a b]
    (let [s1 (seq a) s2 (seq b)]
      (when (and s1 s2)
        (cons (first s1)
          (cons (first s2)
            (f (rest s1) (rest s2))
            ))))))

(defcheck solution-18dae535
  (fn inter-leave [seq1 seq2]
    (if (or (zero? (count seq1))
            (zero? (count seq2)))
      '()
      (flatten  (list (first seq1) (first seq2)
                  (inter-leave (rest seq1) (rest seq2)))))))

(defcheck solution-19b56f8a
  (fn f [& s]
    (apply concat
      (apply map (cons list s)))))

(defcheck solution-1a86392f
  #(loop [[x & xs] % [y & ys] %2 v []]
     (if (and xs ys)
       (recur xs ys (conj v x y))
       (conj v x y))))

(defcheck solution-1aaa2259
  (fn fc [seq1 seq2]
    (loop [x seq1, y seq2, result []]
      (if (or (empty? x) (empty? y))
        result
        (recur (rest x) (rest y) (conj result (first x) (first y)))
        ))))

(defcheck solution-1b9dd7b8
  #(loop [c1 %1 c2 %2 r []]
     (if (or (empty? c1) (empty? c2))
       r
       (let [h1 (first c1), t1 (rest c1), h2 (first c2), t2 (rest c2)]
         (recur t1 t2 (conj (conj r h1) h2))))))

(defcheck solution-1ba84e80
  (fn intrlv [& seqs]
    (apply mapcat list seqs)))

(defcheck solution-1c5ae6ca
  (fn f [s1 s2]
    (if (or (empty? s1) (empty? s2))
      '()
      (let [[h1 & t1] s1
            [h2 & t2] s2] (list* h1 h2 (f t1 t2))))))

(defcheck solution-1c85c287
  (fn [c1 c2]
    (apply concat (for [x (range (min (count c1) (count c2)))]
                    [(nth c1 x) (nth c2 x)]))))

(defcheck solution-1d8e385
  (fn myinterleave [a b] (loop [s1 a s2 b myrt []]
                           (cond
                             (empty? s1) myrt
                             (empty? s2) myrt
                             :else
                             (recur (rest s1) (rest s2) (conj (conj myrt (first s1)) (first s2)))))))

(defcheck solution-1db2f7c4
  mapcat #(list %1 %2))

(defcheck solution-1e9ad70c
  (fn f [x y]
    (if (or (empty? x) (empty? y))
      []
      (concat
       (conj [] (first x))
       (conj [] (first y))
       (f (rest x) (rest y))))))

(defcheck solution-1ee99b38
  (fn myinter
    [seq1 seq2]
    (loop
     [cnt (if (<= (count seq1) (count seq2)) (count seq1) (count seq2)) myres nil myres1 seq1 myres2 seq2]
      (if (= cnt 0)
        myres
        (recur (dec cnt) (concat myres (list (first myres1) (first myres2)) ) (rest myres1) (rest myres2))))))

(defcheck solution-1f11acd3
  (fn my-interleave [coll1 coll2]
    (if (or (empty? coll1) (empty? coll2))
      ()
      (lazy-seq (cons (first coll1)
                  (cons (first coll2)
                    (my-interleave (rest coll1)
                      (rest coll2))))))))

(defcheck solution-1f52363f
  (fn seq-inter [x y]
    (if (or (empty? x) (empty? y))
      '()
      (cons
        (first x)
        (cons
          (first y)
          (seq-inter (rest x) (rest y)))))))

(defcheck solution-1f56ddd7
  (fn inter [& seqs]
    (if (some empty? seqs)
      nil
      (let [step (map first seqs)]
        (concat step (apply inter (map rest seqs)))))))

(defcheck solution-1f6ec984
  (fn [l1 l2] (loop [c1 l1 c2 l2 res []]
                (cond (or (empty? c1) (empty? c2)) res
                      :else (recur (rest c1) (rest c2) (conj (conj res (first c1)) (first c2)))))))

(defcheck solution-1fa87bb
  (fn foo [coll1 coll2 ]
    (let [f1 (first coll1)
          f2 (first coll2)
          r1 (next coll1)
          r2 (next coll2)]
      (cond (and (seq coll1) (seq coll2))
            (cons f1 (cons f2 (lazy-seq (foo r1 r2))))
            :else ()))))

(defcheck solution-2104d987
  (fn rec [ls rs]
    (lazy-seq
      (if (or (empty? ls) (empty? rs)) ()
                                       (concat [(first ls) (first rs)]
                                               (rec (rest ls) (rest rs)))))))

(defcheck solution-21937f66
  (fn [& s] (apply mapcat list s)))

(defcheck solution-2239ab6a
  (fn [x y]
    (reduce concat (map #(vector %1 %2) x y)) ))

(defcheck solution-22a4e4f7
  (fn [xs ys]
    (flatten (map vector xs ys))))

(defcheck solution-22dd45c7
  (fn interl
    ([x y]
     (interl (rest x) (rest y) [(first x) (first y)]))
    ([x y z]
     (if (some #(= 0 (count %)) (list x y))
       z
       (recur (rest x) (rest y) (conj z (first x) (first y)))))))

(defcheck solution-231f1c3d
  (fn ileave [x y]
    (let [s1 (seq x) s2 (seq y)]
      (when (and s1 s2)
        (cons (first s1) (cons (first s2)
                           (ileave (rest s1) (rest s2))))))))

(defcheck solution-23a532f8
  (fn [ca cb] (apply concat (map list ca cb))))

(defcheck solution-23d818f2
  (fn [xs ys]
    (let
     [n (min (count xs) (count ys))]
      n
      (mapcat
        (fn [i]
          [
           (nth xs i)
           (nth ys i)
           ]
          )
        (range n)
        )
      )
    ))

(defcheck solution-23d9d2e9
  (fn f [a b]
    (if (and (not (empty? a)) (not (empty? b)))
      (conj (conj (f (rest a) (rest b)) (first b)) (first a))
      (empty a)
      )
    ))

(defcheck solution-245be8cd
  (fn my-interleave [s1 s2]
    (let [s1 (seq s1)
          s2 (seq s2)]
      (if (and s1 s2)
        (lazy-seq
          (cons (first s1)
            (cons (first s2)
              (my-interleave (rest s1) (rest s2)))))
        '()))))

(defcheck solution-2475e782
  #(flatten (map (fn [x y] [x y]) %1 %2)))

(defcheck solution-24862a7e
  #(flatten (map (fn [x y] (list x y)) %1 %2)))

(defcheck solution-249ef9a4
  (fn [seq1 seq2] (loop [s1 seq1 s2 seq2 interleaved []] (if (or (empty? s1) (empty? s2)) interleaved (recur (rest s1) (rest s2) (conj (conj interleaved (first s1)) (first s2)) )))))

(defcheck solution-25a3cb2c
  (fn [s1 s2]
    (let [f (fn [x y acc]
              (if (or (empty? x) (empty? y)) (reverse acc)
                                             (recur (rest x) (rest y) (conj acc (first x) (first y)))))]
      (f s1 s2 '()))))

(defcheck solution-25b5a80e
  (fn [in1 in2]
    (loop [remaining1 in1 remaining2 in2 ans []]
      (if (or (empty? remaining1) (empty? remaining2))
        (lazy-seq ans)
        (recur (rest remaining1) (rest remaining2) (conj ans (first remaining1) (first remaining2)))))))

(defcheck solution-263a8ffb
  (fn [coll1 coll2]
    (apply concat
      (map (fn [a b] (list a b))
        coll1 coll2))))

(defcheck solution-263ffa58
  (fn inter [a b]
    (if (or (empty? a)
            (empty? b))
      []
      (lazy-seq (cons (first a) (cons (first b) (inter (rest a) (rest b))))))))

(defcheck solution-265acff3
  (fn in ([c1 c2] (lazy-seq (let [s1 (seq c1) s2 (seq c2)] (when (and s1 s2) (cons (first s1) (cons (first s2) (in (rest s1) (rest s2))))))))))

(defcheck solution-26b81d20
  (fn [as bs]
    (loop [xs as ys bs r []]
      (cond (empty? xs) r
            (empty? ys) r
            true (recur (rest xs) (rest ys) (into r [(first xs) (first ys)]))
            ))))

(defcheck solution-26e91e2e
  (fn [as bs]
    (flatten  (map #(conj [] %1 %2) as bs))))

(defcheck solution-26f4aa1e
  (fn [l1 l2]
    (flatten (for [i (range (min (count l1) (count l2)))]
               [(l1 i) (l2 i)]))))

(defcheck solution-26fa20a2
  (fn my-ileave [a b]
    (let [[x & xs] a [y & ys] b]
      (if (some empty?  [xs ys])
        [x y]
        (concat  [x y] (my-ileave xs ys))))))

(defcheck solution-27c424df
  (fn [as bs] (flatten (map list as bs))))

(defcheck solution-286f107c
  (fn [s1 s2] (reduce #(conj %1 (nth %2 0) (nth %2 1)) [] (map (fn [x y] [x y]) s1 s2))))

(defcheck solution-2914e607
  (fn [S1 S2]
    (for [x (range (* 2 (min (count S2)(count S1))))]
      (if (even? x)
        (S1 (quot x 2)) (S2 (quot x 2))
        )
      )
    ))

(defcheck solution-295a9aeb
  (fn thisfunc [a b]
    (if (or (empty? a) (empty? b))
      ()
      (conj (thisfunc (rest a) (rest b)) (first b) (first a)))))

(defcheck solution-29b8fafb
  ;;(fn my-interleave
  ;;  [& args]
  ;;  (println args)
  ;;  (flatten (apply map (fn [& a] (into [] a)) args)))
  #(flatten (map vector %1 %2)))

(defcheck solution-29f01775
  (fn
    [a b]
    (loop [result []
           x a
           y b]
      (if (or (= (count x) 0) (= (count y) 0))
        result
        (recur (conj result (first x) (first y)) (rest x) (rest y))
        ))))

(defcheck solution-2a45d263
  (fn [s1 s2] (mapcat #(list %1 %2) s1 s2 )))

(defcheck solution-2acba395
  (fn [& v] (flatten (apply (partial map #(conj %& %)) v))))

(defcheck solution-2af3497a
  (fn [xs ys]
    (mapcat vector xs ys)))

(defcheck solution-2b072217
  (fn [c1 c2] (flatten (map (fn [x y] [x y]) c1 c2))))

(defcheck solution-2b7b3402
  (fn [l m]
    (loop [[fl & largs :as ls] l
           [ml & margs :as ms] m
           r '()]
      (if (or (empty? ls) (empty? ms))
        r
        (recur largs margs (concat r (list fl ml)))))))

(defcheck solution-2c0f6339
  (fn [l1 l2] (flatten (map #(list %1 %2) l1 l2))))

(defcheck solution-2c41531c
  (fn conl
    ([lst1 lst2] (conl [] lst1 lst2))
    ([l lst1 lst2] (if (or (empty? lst1) (empty? lst2)) l
                                                        (conl (conj l (first lst1) (first lst2)) (next lst1) (next lst2))))
    ))

(defcheck solution-2c645f5e
  (fn interl [c1 c2]
    (let [x (first c1)
          y (first c2)]
      (if (and x y)
        (into [x y] (interl (rest c1) (rest c2)))
        [])

      )))

(defcheck solution-2ca6acea
  (fn [left right]
    "Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
    (loop [l left
           r right
           acc (vector)]
      (if (or (empty? l) (empty? r))
        (seq acc)
        (recur (rest l) (rest r) (conj acc (first l) (first r)))))))

(defcheck solution-2de63f94
  (fn [coll1 coll2]
    (loop [coll1 coll1 coll2 coll2 coll '()]
      (if (or (= coll1 '()) (= coll2 '()))
        coll
        (recur (rest coll1) (rest coll2) (conj (conj (vec coll) (first coll1)) (first coll2) ))
        ))))

(defcheck solution-2e461ada
  #(loop [result []
          s %
          t %2]
     (if (and (seq s) (seq t))
       (recur (concat result [(first s) (first t)]) (rest s) (rest t))
       result)))

(defcheck solution-2e5f9005
  #(reduce concat [] (map (fn [x y] (list x y)) %1 %2)))

(defcheck solution-2e6e7b1b
  (fn [lst1 lst2]
    (loop [lst-1 lst1
           lst-2 lst2
           lst-res []]
      (if (or (empty? lst-1)
              (empty? lst-2))
        lst-res
        (recur (rest lst-1)
          (rest lst-2)
          (conj lst-res (first lst-1) (first lst-2)))))))

(defcheck solution-2e94231e
  (fn [a b]
    (flatten
      (map #(list %1 %2) a b))))

(defcheck solution-2ec22e6
  (fn recinter [a b]
    (if (or (= a '()) (= b '()))
      '()
      (concat [(first a)] [(first b)] (recinter (rest a) (rest b))))))

(defcheck solution-2ff80811
  #(loop [odp [] x % y %2] (if (or (empty? x) (empty? y)) odp
                                                          (recur (conj (conj odp (first x)) (first y)) (rest x) (rest y)))))

(defcheck solution-30ee9dd2
  #(mapcat vector % %2))

(defcheck solution-313686b
  #((fn [a b c]
      (cond
        (empty? a) (reverse c)
        (empty? b) (reverse c)
        :else (recur (rest a) (rest b) (conj c (first a) (first b)))
        )) %1 %2 ()))

(defcheck solution-314b7167
  (fn interl [[x & xs] [y & ys]]
    (if (and x y)
      (concat [x y] (interl xs ys))
      [])))

(defcheck solution-31c1c6cb
  (fn inter [[h1 & t1 :as c1]
             [h2 & t2 :as c2]]
    (when (and c1 c2) (cons h1
                        (cons h2
                          (inter t1 t2))))))

(defcheck solution-32a8114c
  (fn [coll1 coll2]
    (let [accum []]
      (mapcat #(conj accum %1 %2) coll1 coll2))))

(defcheck solution-32b8b26c
  (fn inter [xs ys] (if (or (empty? xs) (empty? ys)) '() (conj (conj (inter (rest xs) (rest ys)) (first ys)) (first xs)))))

(defcheck solution-32c019b4
  (fn inter [a b]
    (if (or (empty? a) (empty? b))
      []
      (concat [(first a) (first b)] (inter (rest a) (rest b))))))

(defcheck solution-339d663f
  (fn zip
    [[x & xs] [y & ys]]
    (if (or (empty? xs) (empty? ys)) [x y]
                                     (concat [x y] (zip xs ys)))))

(defcheck solution-33fe237c
  (fn [& more]
    (flatten
      (for [x (range (apply min (map count more)))]
        (map #(get % x) more)))))

(defcheck solution-3414cda8
  (fn its [s1 s2]
    (if (or (empty? s1) (empty? s2))
      []
      (cons (first s1) (cons (first s2) (its (rest s1) (rest s2)) )))))

(defcheck solution-345e4828
  (fn [x y]
    (flatten (map #(vec %&) x y))))

(defcheck solution-35022924
  #(flatten (map (partial conj '()) %2 %1)))

(defcheck solution-35054078
  (fn [sq1 sq2]
    (let [aux
          (fn [[hd1 & tl1 :as in1] [hd2 & tl2 :as in2] acc]
            (cond
              (or
               (nil? hd1)
               (nil? hd2))
              acc

              :else
              (recur tl1 tl2 (conj acc hd1 hd2))))]

      (aux sq1 sq2 []))))

(defcheck solution-35afe9bc
  (fn [x y] (apply concat (map #(seq [%1 %2]) x y))))

(defcheck solution-3622c67f
  (partial mapcat list))

(defcheck solution-3652993b
  (fn il [sq1 sq2]
    (when (and (not (empty? sq1))
               (not (empty? sq2)))
      (lazy-seq (concat [(first sq1)] [(first sq2)] (il (rest sq1) (rest sq2)))))))

(defcheck solution-3732f9f9
  (fn [l1 l2]
    (loop [l1 l1 l2 l2 ret []]
      (if (or (= (count l1) 0) (= (count l2) 0) )
        ret
        (recur
          (rest l1)
          (rest l2)
          (conj (conj ret (first l1)) (first l2))
          )
        )
      )
    ))

(defcheck solution-379427af
  (fn [l1,l2]
    (mapcat
      (fn[i] (list (nth l1 i) (nth l2 i)))
      (range 0 (min (count l1) (count l2))))))

(defcheck solution-37fa936d
  #(apply concat (map list %1 %2)))

(defcheck solution-3806c02
  (fn [col1 col2]
    (mapcat (fn [it1 it2] (list it1 it2)) col1 col2)
    ))

(defcheck solution-381b6aaf
  (fn [x y]

    (apply concat (map #(vector %1 %2) x y))

    ))

(defcheck solution-38207d41
  (fn intlv [xs ys]
    (when (and (seq xs) (seq ys))
      (concat (list (first xs) (first ys)) (intlv (rest xs) (rest ys))))))

(defcheck solution-38256266
  (fn [x y] (loop [a x b y c []] (if (or (empty? a) (empty? b)) c (recur (rest a) (rest b) (concat c (conj '() (first b) (first a))))))))

(defcheck solution-3a81ad08
  (fn f [xs ys] (if (some empty? [xs ys]) '() (cons (first xs)
                                                (cons (first ys)
                                                  (lazy-seq (f (rest xs) (rest ys))))))))

(defcheck solution-3b0f2bb1
  (fn [c1 c2]
    (mapcat #(vector %1 %2) c1 c2)))

(defcheck solution-3bd214f2
  (fn __ [s1 s2]
    (when (not (or (empty? s1) (empty? s2)))
      (conj (__ (rest s1) (rest s2)) (first s2) (first s1)))))

(defcheck solution-3c50f6c8
  (fn inter [a b]
    (loop [a a b b acc []]
      (if (and (seq a) (seq b))
        (recur (rest a) (rest b) (conj acc (first a) (first b)))
        acc))))

(defcheck solution-3de29fef
  (fn I [xs ys]
    (cond (empty? xs) []
          (empty? ys) []
          :else (list* (first xs)
                  (first ys)
                  (I (rest xs) (rest ys))))))

(defcheck solution-3e2db012
  (fn[x,y]
    (loop [l x, m y, v []]
      (if (or (empty? l) (empty? m))
        v
        (do
          (recur (rest l) (rest m)
            (conj v (first l) (first m))))))))

(defcheck solution-3eb6e418
  (fn [& args] (flatten (apply map vector args))))

(defcheck solution-3eca5769
  (fn [& colls] (apply mapcat #(list %1 %2) colls)))

(defcheck solution-3f664fa9
  (comp flatten (partial map list)))

(defcheck solution-3f90c68e
  (fn [l1 l2](mapcat #(list % %2) l1 l2)))

(defcheck solution-3faafb57
  (fn [a b]
    (loop [a a, b b, r []]
      (if (or (empty? a) (empty? b)) r
                                     (recur (rest a) (rest b) (conj r (first a) (first b)))))))

(defcheck solution-3fdc2bd0
  (fn [& args]
    (loop [colls args r '()]
      (if (not-any? empty? colls)
        (recur (map rest colls)
          (concat (reverse (filter identity (map first colls)))
                  r))
        (reverse r)))))

(defcheck solution-400762b6
  #(flatten(map list %1 %2)))

(defcheck solution-40bd6742
  (fn f [xs ys]
    (if (or (empty? xs) (empty? ys))
      ()
      (conj (f (rest xs) (rest ys)) (first ys) (first xs)))))

(defcheck solution-417ba2a0
  (fn [c1 c2]
    (mapcat vector c1 c2)))

(defcheck solution-4193fb22
  (fn interleave2 [xs ys]
    (if (some empty? [xs ys])
      (empty xs)
      (cons
        (first xs)
        (cons
          (first ys)
          (interleave2 (rest xs) (rest ys)))))))

(defcheck solution-41b141b8
  (fn [coll1 coll2]
    (flatten (map (fn [a b] (list a b)) coll1 coll2))))

(defcheck solution-41fcc60e
  (fn leave
    ([s1 s2] (leave s1 s2 nil))
    ([s1 s2 rez] (if
                  (or (empty? s1) (empty? s2))
                   rez
                   (leave (rest s1) (rest s2) (reverse (conj (reverse rez) (first s1) (first s2))))))))

(defcheck solution-426cacb7
  (fn [s1 s2]
    (flatten (map list s1 s2))))

(defcheck solution-42958dbc
  (fn [s1 s2]
    (mapcat vector s1 s2)))

(defcheck solution-42dbb2e5
  (fn [l1 l2]
    (flatten (map list l1 l2))))

(defcheck solution-433114c7
  (fn foo [a b]
    (if (and (seq a) (seq b))
      (lazy-seq (concat [(first a) (first b)] (foo (rest a) (rest b))))
      [])))

(defcheck solution-43796a08
  (fn [& args] (apply mapcat (fn [& args] (apply conj [] args)) args)))

(defcheck solution-43f0ffc8
  (fn this [l1 l2]
    (cond (or (= (count l1) 0) (= (count l2) 0)) '()
          :else (concat (list (first l1) (first l2)) (this (rest l1) (rest l2))))))

(defcheck solution-449b3ac3
  (fn [c1 c2] (flatten (map (fn[a b] [a b]) c1 c2))))

(defcheck solution-449d6efe
  (fn minterleave [c1 c2]
    (let [s1 (seq c1)
          s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2)
                           (minterleave (rest s1) (rest s2))))))))

(defcheck solution-44cb2568
  (fn [x y]
    (mapcat #(conj [%1] %2) x y)))

(defcheck solution-44fb23c9
  (fn inter [seq1 seq2]
    (if(or (empty? seq1) (empty? seq2))
      []
      (concat [(first seq1) (first seq2)]
              (inter (rest seq1) (rest seq2))))))

(defcheck solution-4558d91e
  (fn new-interleave
    [[x & xs] [y & ys]]
    (if (and x y)
      (concat (cons x [y]) (new-interleave xs ys))
      [])))

(defcheck solution-45bd15fa
  (fn [c1 c2] (apply concat (map #(conj [] % %2) c1 c2))))

(defcheck solution-45fe7ed4
  (fn [a b] (apply concat (map #(cons %1 [%2]) a b))))

(defcheck solution-4703fa48
  (fn my-interleave2 [l1 l2]
    (reduce concat [] (map #(vector %1 %2) l1 l2))))

(defcheck solution-475b51db
  #(mapcat vector %1 %2))

(defcheck solution-47945e59
  (fn il [xs ys]
    (if (or (empty? xs) (empty? ys))
      ()
      (lazy-seq
        (cons
          (first xs)
          (cons
            (first ys)
            (il (rest xs) (rest ys))))))))

(defcheck solution-47e02e77
  (fn foo [i j]
    (if (not-any? empty? [i j])
      (concat [(first i) (first j)] (foo (rest i) (rest j)))
      ())))

(defcheck solution-4812d1c9
  (fn __
    [a b]
    (when (not (or (empty? a) (empty? b)))
      (concat (take 1 a) (take 1 b) (__ (rest a) (rest b))))))

(defcheck solution-482c3709
  (fn [seq1 seq2]
    (loop [s1 seq1 s2 seq2 acc []]
      (if (some empty? [s1 s2])
        acc
        (recur (rest s1) (rest s2) (conj (conj acc (first s1)) (first s2)))))))

(defcheck solution-49484538
  (fn ntrlv [coll1 coll2]
    (when (not (or (empty? coll1) (empty? coll2)))
      (conj (ntrlv (rest coll1) (rest coll2)) (first coll2) (first coll1) )
      )))

(defcheck solution-4a5b5718
  (partial mapcat #(list %1 %2)))

(defcheck solution-4a793fd9
  (fn [xs ys] (letfn [(ileaf [acc as bs] (if (or (empty? as) (empty? bs)) acc (ileaf (conj acc (first as) (first bs)) (rest as) (rest bs) ) ))] (ileaf (empty xs) xs ys))))

(defcheck solution-4a7b5147
  (fn [l1 l2]
    (loop [l1 l1 l2 l2 r []]
      (if (or (empty? l1) (empty? l2)) r
                                       (recur (rest l1) (rest l2) (conj r (first l1) (first l2)))))))

(defcheck solution-4b496851
  (fn [x y] (flatten (map list x y))))

(defcheck solution-4b803cae
  (fn ! ([x y] (! x y []))   ([x y m] (if (or (empty? x) (empty? y)) m  (! (rest x) (rest y) (concat m [(first x)] [(first y)] )))   )))

(defcheck solution-4b8311fe
  (fn [s1 s2] (loop[x1 s1 x2 s2 acc []] (if (or (empty? x1) (empty? x2)) acc (recur (rest x1 ) (rest x2) (concat acc [ (first x1) (first x2 )]) )))))

(defcheck solution-4ba8c969
  (fn my-interleave [[x & xs :as xcoll] [y & ys :as ycoll]]
    (if (or (empty? xcoll) (empty? ycoll))
      ()
      (concat [x y] (my-interleave xs ys)))))

(defcheck solution-4bd3f439
  (fn interleave-2
    [s1 s2]
    (reduce concat (map vector s1 s2))))

(defcheck solution-4bed8c87
  (fn merge-coll
    [c1 c2]
    (loop [lc1 c1 lc2 c2 rc '()]
      (if (or (empty? lc1) (empty? lc2))
        rc
        (recur (rest lc1) (rest lc2) (concat rc (list (first lc1) (first lc2))))))))

(defcheck solution-4c0fd097
  (fn [c1 c2]
    (mapcat #(list %1 %2) c1 c2)))

(defcheck solution-4c84db26
  (fn spoj [a b] (if (or (empty? a) (empty? b)) [] (concat [(first a) (first b)] (spoj (rest a) (rest b))))))

(defcheck solution-4ccaf382
  #(loop [xs %1 ys %2 acc []]
     (if (and (seq xs) (seq ys))
       (recur (next xs) (next ys) (conj acc (first xs) (first ys)))
       acc)))

(defcheck solution-4cd2b42e
  (fn f
    [x y]
    (let [[e1 & r1] x [e2 & r2] y]
      (if-not (or (nil? e1) (nil? e2))
        (cons e1 (cons e2 (f r1 r2)))))))

(defcheck solution-4db5087b
  (fn [xs ys] (mapcat list xs ys)))

(defcheck solution-4e7af0c8
  (fn interl [& colls]
    (loop [result [] sub-colls colls]
      (if (some #(= 0 (count %)) sub-colls)
        result
        (recur (into result (map first sub-colls)) (map rest sub-colls))))))

(defcheck solution-4e7e55aa
  (fn intl [s1 s2]
    (when (not (or (empty? s1) (empty? s2)))
      (cons (first s1) (cons (first s2) (intl (rest s1) (rest s2))))
      )
    ))

(defcheck solution-4ea75936
  (fn [l1 l2]
    (reduce #(conj %1 (first %2) (last %2)) [] (map vector l1 l2))
    ))

(defcheck solution-4f4fe66c
  mapcat (fn [& els] els))

(defcheck solution-4f9e649b
  (fn [seq1 seq2]
    (loop [result [] elements1 seq1 elements2 seq2]
      (if (or (empty? elements1) (empty? elements2))
        result
        (recur (conj (conj result (first elements1)) (first elements2)) (rest elements1) (rest elements2))
        )
      )
    ))

(defcheck solution-4fb16673
  ;;(fn te
  ;;  [coll coll2]
  ;;  (reverse (loop
  ;;               [a coll
  ;;                b coll2
  ;;                result '()]
  ;;             (do (println "a: " a ", b: " b ", result: " result)
  ;;                 (if (or (empty? a) (empty? b))
  ;;                   result
  ;;                   (recur (rest a) (rest b) (conj result (first a) (first b))))))))
  #(mapcat vector %1 %2))

(defcheck solution-500bb77e
  #(mapcat (fn [x y] [x y]) %1 %2))

(defcheck solution-5058d94c
  #(loop [[h1 & t1] %1
          [h2 & t2] %2
          acc []]
     #_(println "h1" h1 "t1" t1 "h2" h2 "t2" t2 "acc" acc)
     (if (or (nil? h1) (nil? h2) false)
       acc
       (recur t1 t2 (conj acc h1 h2))
       )
     ))

(defcheck solution-50a5dbe7
  (fn smash [one two]
    (when (every? not-empty [one two])
      (concat [(first one) (first two)] (smash (rest one) (rest two))))))

(defcheck solution-5188358c
  (fn interleav
    ([c1 c2]
     (lazy-seq
       (let [s1 (seq c1) s2 (seq c2)]
         (when (and s1 s2)
           (cons (first s1) (cons (first s2)
                              (interleav (rest s1) (rest s2))))))))
    ))

(defcheck solution-51c86333
  (fn i
    ([l m]
     (i l m []))
    ([[l & ls] [m & ms] a]
     (if (and l m)
       (recur ls ms (conj (conj a l) m))
       a))))

(defcheck solution-51e2dba3
  (fn newinterleave [x y]
    "Interleaves two sequences x and y. Output is only as long as the shorter of the two."
    (if (and (seq x) (seq y))
      (list* (first x) (first y) (newinterleave (rest x) (rest y)))
      (empty x))))

(defcheck solution-52c6c4c
  (fn [x y] (flatten (map vector x y))))

(defcheck solution-5371e667
  (fn [xs ys]
    (loop [xss xs yss ys rs []]
      (if (empty? xss)
        rs
        (if (empty? yss)
          rs
          (recur (rest xss) (rest yss) (concat rs [(first xss) (first yss)]))
          )))))

(defcheck solution-539ab0eb
  (fn [& xs]
    (reduce #(concat %1 %2) [] (apply (partial map list) xs))))

(defcheck solution-53c6ba1c
  #(apply concat (apply map vector %&)))

(defcheck solution-54078f02
  (fn [x y]
    (loop [i 0 b 0  z []]
      (if
       (and
        (< i (count x))
        (< b (count y))
        )
        (recur (+ i 1) (+ b 1) (conj z [(get x i) (get y b)]))
        (flatten z)
        )
      )
    ))

(defcheck solution-54958338
  (fn [xs ys] (mapcat #(list %1 %2) xs ys)))

(defcheck solution-554cfe4c
  (fn il [s0 s1]
    (loop [i 0 r []]
      (if (= i (min (count s0) (count s1)))
        r
        (recur (inc i) (conj r (nth s0 i) (nth s1 i)))
        )
      )
    ))

(defcheck solution-5618792d
  (fn inter [a b]
    (if (= a [])
      '()
      (if (= b [])
        '()
        (cons (first a)
          (cons (first b)
            (inter (rest a)
              (rest b)
              )
            )
          )
        )
      )
    ))

(defcheck solution-565a39d6
  (fn [s1 s2]
    (loop [r1 s1 r2 s2 v []]
      (cond
        (empty? r1) v
        (empty? r2) v
        :default (recur (rest r1) (rest r2) (conj v (first r1) (first r2)))))))

(defcheck solution-56b23a80
  (fn [c1 c2]
    (apply concat ((partial map list) c1 c2))))

(defcheck solution-5823699b
  (fn self [xs1 xs2 & params]
    (if (nil? params) (self xs1 xs2 ()))
    (let [[res] params]
      (if (or (empty? xs1) (empty? xs2)) res
                                         (self (rest xs1) (rest xs2) (concat res (list (first xs1) (first xs2))))))))

(defcheck solution-58d4fb4d
  (fn [xs ys]
    (flatten
      (loop [a xs
             b ys
             r []]
        (if (or (nil? a) (nil? b))
          r
          (recur (next a) (next b) (conj r [(first a) (first b)])))))))

(defcheck solution-590f3351
  (fn [a b]
    (apply concat (map #(list %1 %2) a b))))

(defcheck solution-5969c4ec
  (fn [a b] (loop [c1 a c2 b r []] (if (or (empty? c1) (empty? c2)) r (recur (rest c1) (rest c2) (conj r (first c1) (first c2)))))))

(defcheck solution-59b70e63
  (fn x [a b]
    (let [c (seq a) d (seq b)]
      (when (and c d)
        (cons (first c) (cons (first d)
                          (x (rest c) (rest d))))))))

(defcheck solution-5abc3efe
  #(loop [s1 %1 s2 %2 r[]]
     (if (or (empty? s1) (empty? s2))
       r
       (recur (rest s1) (rest s2) (conj r (first s1) (first s2))))))

(defcheck solution-5b8b42b6
  ;(fn [xs ys]
  ;  (let [cnt (apply min (map count [xs ys]))]
  ;    (loop [pos 0
  ;           acc '()]
  ;      (if (= pos cnt)
  ;        acc
  ;        (let [x (nth xs pos)
  ;              y (nth ys pos)]
  ;        (recur (inc pos) (concat acc (list x y))))))))

  #(mapcat list %1 %2))

(defcheck solution-5bc8a4e4
  (fn il [x y]
    (if (or (empty? x) (empty? y))
      ()
      (cons (first x) (cons (first y)
                        (il (rest x) (rest y)))))))

(defcheck solution-5c57900f
  (fn [& xss]
    (apply (partial mapcat vector) xss)))

(defcheck solution-5cccc1e
  (fn customer-interleave [left right]
    (loop [left left right right new []]
      (if (or (= (first left) nil) (= (first right) nil) )
        new
        (recur (rest left)
          (rest right)
          (conj new (first left) (first right)))))))

(defcheck solution-5d4c38ab
  (fn zip [xs ys]
    (if (or (empty? xs) (empty? ys))
      '()
      (concat (list (first xs) (first ys))
              (zip (rest xs) (rest ys))))))

(defcheck solution-5dd1c0fd
  (fn my-interleave
    [& input-seqs]
    (loop [seqs input-seqs
           result []]
      (if (some empty? seqs)
        result
        (recur (map rest seqs)
          (apply conj result (map first seqs)))))))

(defcheck solution-5e97ce8b
  (fn [coll1 coll2] (mapcat vector coll1 coll2)))

(defcheck solution-5eb15c8e
  (fn i [a b]
    (if (or (empty? a) (empty? b))
      '()
      (cons (first a) (cons (first b) (lazy-seq (i (rest a) (rest b))))))))

(defcheck solution-5fdef851
  (fn [a b]
    (loop [x a y b r []]
      (if (or (empty? x) (empty? y))
        r
        (recur (rest x) (rest y) (conj r (first x) (first y)))))))

(defcheck solution-601d7b7e
  (fn interw [s1 s2]
    (loop [result '()
           cs1 s1
           cs2 s2]
      (if (or (empty? cs1) (empty? cs2))
        result
        (recur (concat result (list (first cs1) (first cs2))) (rest cs1) (rest cs2))))))

(defcheck solution-60b4bc07
  (fn ileave [x, y]
    (loop [sx x sy y acc []]
      (if (and (seq sx) (seq sy))
        (recur (rest sx) (rest sy)
          (concat acc [(first sx) (first sy)]))
        acc))))

(defcheck solution-61090a10
  (fn interleave2 [coll1 coll2]
    (let [c1 (seq coll1) c2 (seq coll2)]
      (if (or (not c1) (not c2))
        '()
        (lazy-seq (conj (interleave2 (rest c1) (rest c2)) (first c2) (first c1)))))))

(defcheck solution-612cdf7c
  (fn intr [coll1 coll2]
    (let [s1 (seq coll1)
          s2 (seq coll2)]
      (when (and s1 s2)
        (cons (first s1)
          (cons (first s2)
            (intr (rest s1)
              (rest s2))))))))

(defcheck solution-613ea60e
  #(loop [a %
          b %2
          acc []]
     (if (or (zero? (count a)) (zero? (count b)))
       acc
       (recur (rest a)
         (rest b)
         (into acc [(first a) (first b)])))))

(defcheck solution-613f5917
  (fn il [a b]
    (if (or (empty? a) (empty? b)) '()
                                   (cons (first a) (cons (first b) (il (rest a) (rest b)))))))

(defcheck solution-617974db
  (fn myinterleave [a b]
    (seq (loop [l1 a l2 b final '[]]
           (if (or (empty? l1) (empty? l2))
             final
             (recur (rest l1) (rest l2) (conj final (first l1) (first l2))))))))

(defcheck solution-61cdafdb
  (fn [seq1 seq2]
    (apply concat (map (fn [x y] (list x y)) seq1 seq2))))

(defcheck solution-61ef3a45
  #(apply concat (map vector %1 %2)))

(defcheck solution-6244c6fe
  (fn [s1 s2] (for [[a b] (map vector s1 s2)
                    x [a b]]
                x)))

(defcheck solution-62a031a4
  (fn f [a b]
    (if (or (empty? a) (empty? b))
      '()
      (concat (-> a first list) (-> b first list) (f (rest a) (rest b))))))

(defcheck solution-62aa007
  #(flatten (apply map vector %&)))

(defcheck solution-62dd3add
  (fn [a b]
    (reduce (fn [c [x y]] (conj c x y)) [] (map vector a b))))

(defcheck solution-645c79f6
  (fn [s1 s2] (flatten (map #(list %1 %2) s1 s2))))

(defcheck solution-6468cfc2
  (fn [x y]
    (mapcat #(vector % %2) x y)))

(defcheck solution-647cfb4f
  (fn [f s]
    (let [indices (range 0 (min (count f) (count s)))]
      (mapcat #(list (f %) (s %)) indices))))

(defcheck solution-64e2d12a
  (fn [a b]
    (loop [[ax & arest] a [bx & brest] b acc []]
      (if (and ax bx) (recur arest brest (concat acc [ax bx])) acc))))

(defcheck solution-650becb4
  (fn ! [s1 s2]
    (cond (empty? s1) '()
          (empty? s2) '()
          :else (cons (first s1)
                  (cons (first s2)
                    (! (rest s1) (rest s2))
                    )
                  )
          )
    ))

(defcheck solution-6513bf32
  (fn inter [s1 s2]
    (when (and (seq s1)  (seq s2))
      (cons (first s1) (cons (first s2) (inter (rest s1) (rest s2)))))
    ))

(defcheck solution-65d0ff41
  (fn [coll1 coll2]
    (flatten (map vector coll1 coll2))
    ))

(defcheck solution-67391da9
  (fn [a b] (apply concat (map #(vector %1 %2) a b))))

(defcheck solution-67a253ed
  (fn il [s1 s2]
    (if (or (empty? s1) (empty? s2))
      []
      (concat [(first s1) (first s2)]
              (il (rest s1) (rest s2))))))

(defcheck solution-67d2b12c
  (fn [& lists]
    (for [tuple (apply map vector lists)
          val tuple]
      val)))

(defcheck solution-67ef76e3
  (fn ! [x,y] (if (or (empty? x) (empty? y)) '() (conj (!(rest x) (rest y)) (first y) (first x)))))

(defcheck solution-69c471b
  (fn inter [l r]
    (loop [l l r r acc '()]
      (if (or (empty? l) (empty? r))
        (reverse acc)
        (recur (rest l) (rest r)
          (conj acc (first l) (first r)))))))

(defcheck solution-69f02001
  (fn interss [xs ys]
    (if (or (empty? xs) (empty? ys))
      '()
      (cons (first xs) (cons (first ys) (interss (rest xs) (rest ys)))))))

(defcheck solution-69fcff56
  #(flatten (map (fn[x y] (list x y)) %1 %2)))

(defcheck solution-6aa1fd96
  (fn [x y] (loop [x1 x, y1 y, acc []]
              (if (or (empty? x1) (empty? y1))
                acc
                (recur (rest x1) (rest y1) (concat acc [(first x1) (first y1)]))))))

(defcheck solution-6aa93112
  (fn [a b]
    (mapcat #(list (a %) (b %))
      (range (min
               (count a)
               (count b))
        ))))

(defcheck solution-6b1596c2
  (fn my-inter [a b]
    (if (and (not-empty a) (not-empty b))
      (concat [(first a) (first b)] (my-inter (rest a) (rest b)))
      nil)))

(defcheck solution-6b3f4457
  #(loop [l1 %1 l2 %2 acc []]
     (if (or (empty? l1) (empty? l2))
       acc
       (recur (rest l1) (rest l2) (conj acc (first l1) (first l2))))))

(defcheck solution-6b40ab3a
  (fn [x y] (flatten (map #(vector %1 %2) x y))))

(defcheck solution-6b46046
  (fn zip [seq1 seq2]
    (if (or (empty? seq1) (empty? seq2))
      '()
      (cons (first seq1) (cons (first seq2) (zip (rest seq1) (rest seq2)))))))

(defcheck solution-6b4f0b1c
  (fn [lsA lsB] (loop [lsAA lsA lsBB lsB ret '()] (if (or (empty? lsAA) (empty? lsBB)) ret
                                                                                       (recur (rest lsAA)
                                                                                         (rest lsBB)
                                                                                         (flatten (cons ret (list (first lsAA) (first lsBB)))))))))

(defcheck solution-6bdc5902
  (fn [coll1 coll2]
    (loop [c1 coll1
           c2 coll2
           res []]
      (if (and (seq c1) (seq c2))
        (recur (next c1) (next c2) (conj res (first c1) (first c2)))
        res))))

(defcheck solution-6c004e02
  (fn myinter [xs ys]
    (mapcat vector xs ys)))

(defcheck solution-6c061de1
  (fn [& xs] (apply concat (apply map vector xs))))

(defcheck solution-6c155222
  (fn [c1 c2] (flatten (map #(list %1 %2) c1 c2))))

(defcheck solution-6cff7691
  (fn my-interleave [s1 s2]
    (if (or (empty? s1) (empty? s2))
      nil
      (list* (first s1) (first s2) (my-interleave (rest s1) (rest s2))))))

(defcheck solution-6d339d37
  (fn [a b] (loop [[h1 & t1] a [h2 & t2] b acc []] (let [newacc (into acc [h1 h2])] (if (or (= t1 nil) (= t2 nil)) newacc (recur t1 t2 newacc))))))

(defcheck solution-6d8432cb
  (partial mapcat (fn [& s] s)))

(defcheck solution-6e3aa20e
  mapcat #(vec %&))

(defcheck solution-6ee875d1
  (fn my-interleave [x y]
    (if (and (seq x) (seq y))
      (concat
       (list (first x) (first y))
       (my-interleave (rest x) (rest y))))))

(defcheck solution-70950d6d
  (fn [& cs]
    (apply mapcat list cs)))

(defcheck solution-7213a958
  (fn prob39 [c1 c2]
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (first s1) (cons (first s2) (prob39 (rest s1) (rest s2))))))))

(defcheck solution-721e06ac
  #(loop [a %1 b %2 acc []] (if (some empty? [a b]) acc (recur (rest a) (rest b) (conj acc (first a) (first b))))))

(defcheck solution-7281041
  (fn [r s] (mapcat #(identity [%1 %2]) r s)))

(defcheck solution-737c09a4
  (fn fn-intlv
    ([c1 c2]
     (lazy-seq
       (let [s1 (seq c1) s2 (seq c2)]
         (when (and s1 s2)
           (cons (first s1) (cons (first s2)
                              (fn-intlv (rest s1) (rest s2))))))))
    ([c1 c2 & colls]
     (lazy-seq
       (let [ss (map seq (conj colls c2 c1))]
         (when (every? identity ss)
           (concat (map first ss) (apply fn-intlv (map rest ss)))))))))

(defcheck solution-739a0e2b
  (fn inter [s r]
    (if (or (nil? (first s)) (nil? (first r))) '()
                                               (cons (first s) (cons (first r) (inter (rest s) (rest r)))))))

(defcheck solution-740c8e11
  (fn [a-prime b-prime]
    (loop [a a-prime b b-prime rv []]
      (if (or (empty? a) (empty? b))
        rv
        (recur (rest a) (rest b) (conj (conj rv (first a)) (first b)))))))

(defcheck solution-743fd722
  (fn f [x y]
    (cond
      (or (empty? x)
          (empty? y))
      '()
      :else
      (concat (list (first x)
                (first y))
              (f (rest x) (rest y))))))

(defcheck solution-74fde82c
  (fn [a b]
    (mapcat vector a b)))

(defcheck solution-7543a256
  #(flatten (map list % %2)))

(defcheck solution-768c5ee
  #(flatten (map vector %1 %2)))

(defcheck solution-769f6c06
  (fn [a b] (apply concat (map #(list %1 %2) a b))))

(defcheck solution-77ada0e9
  (fn f
    ([x y]
     (f x y []))
    ([x y r]
     (if (or (empty? x) (empty? y))
       r
       (recur (rest x) (rest y) (conj r (first x) (first y)))))))

(defcheck solution-7863b852
  (comp flatten (fn [xs ys] (map vector xs ys))))

(defcheck solution-78a3b2c7
  (fn [x y] (flatten (map #(conj '() % %2) y x))))

(defcheck solution-78a962d5
  mapcat list)

(defcheck solution-792db47c
  mapcat (fn [x y] (list x y)))

(defcheck solution-795bacb9
  (fn interleave-map [& colls]
    (->> colls                        ; [[1 2 3] [:a :b :c :d :e] "String"]
      (apply (partial map vector)) ; '([1 :a \S] [2 :b \t] [3 :c \r])
      (apply concat))))

(defcheck solution-79ab2196
  (fn interleave-collections [c1 c2] (mapcat list c1 c2)))

(defcheck solution-7a15ae3f
  (fn [s t]
    (loop [s_ s t_ t ret '()]
      (if (and s_ t_)
        (recur
          (next s_)
          (next t_)
          (conj ret (first s_) (first t_)))
        (into '() ret)))))

(defcheck solution-7a790864
  (fn [a b]
    (flatten (map (fn [x y] [x y]) a b))))

(defcheck solution-7b101a79
  (fn [m1 m2] (mapcat #(vector %1 %2) m1 m2)))

(defcheck solution-7b5be36e
  (fn [c1 c2]
    (flatten (map (fn [m1 m2] [m1 m2]) c1 c2))))

(defcheck solution-7c9b46d4
  (fn my-interleave [s1 s2]
    (if (or (empty? s1) (empty? s2))
      '()
      (conj (my-interleave (rest s1)
              (rest s2))
        (first s2)
        (first s1)))))

(defcheck solution-7cf4c4f4
  #(loop [coll [ ]
          n 0 ]
     (if (= n  (min (count %1) (count %2))) coll
                                            (recur (conj coll (nth %1 n) (nth %2 n)) (inc n)
                                              ))))

(defcheck solution-7d59face
  (fn [xs ys]
    (mapcat (fn [x y] (list x y)) xs ys)
    ))

(defcheck solution-7dad13a7
  (fn [l r]
    (loop [[x & xs] l
           [y & ys] r
           res []]
      (if (or (nil? x) (nil? y))
        res
        (recur xs ys (conj res x y))))))

(defcheck solution-7e1fddc2
  (fn my-il [s1 s2]
    (if (or (nil? s1) (nil? s2))
      '()
      (conj (conj (my-il (next s1) (next s2)) (first s2)) (first s1)))))

(defcheck solution-7ed4eeac
  (fn myinterleave [x y]
    (if (or (empty? x) (empty? y))
      '()
      (conj (conj (myinterleave (rest x) (rest y)) (first y))
        (first x)))))

(defcheck solution-7f354823
  #(loop [r [] c1 %1 c2 %2] (if (or (empty? c1) (empty? c2)) r (recur (conj r (first c1) (first c2)) (next c1) (next c2)))))

(defcheck solution-7fac5b78
  (fn [f s](mapcat #(list %1 %2) f s)))

(defcheck solution-7fdfb902
  mapcat (fn [a b] [a b]))

(defcheck solution-8009bc42
  (fn [vec-1 vec-2]
    (loop [seq-1 vec-1
           seq-2 vec-2
           return ()]
      (if (or (nil? (first seq-1))
              (nil? (first seq-2)))
        return
        (recur (rest seq-1)
          (rest seq-2)
          (concat return [(first seq-1) (first seq-2)]))))))

(defcheck solution-80339c5f
  (fn zip [c1 c2] (let [s1 (seq c1) s2 (seq c2)] (when (and s1 s2) (cons (first s1) (cons (first s2) (zip (rest s1) (rest s2))))))))

(defcheck solution-817ffd68
  (fn [xs ys]
    (loop [xs xs
           ys ys
           acc []]
      (if (some empty? [xs ys])
        acc
        (recur (rest xs) (rest ys) (conj acc (first xs) (first ys)))))))

(defcheck solution-82896b54
  (partial mapcat #(vector %1 %2)))

(defcheck solution-8396578f
  (fn f [& colls]
    (if (every? seq colls)
      (lazy-seq
        (concat
         (map first colls)
         (apply f (map rest colls))))
      )))

(defcheck solution-83ca31be
  #(reduce concat (map vector %1 %2)))

(defcheck solution-83f2e338
  (fn [x y]
    (apply concat (map #(list %1 %2)
                    x
                    y))))

(defcheck solution-8444c33c
  #(loop [[f1 & r1] %1 [f2 & r2] %2 res '()]
     (if (or (nil? f1) (nil? f2)) res
                                  (recur r1 r2 (concat res (list f1 f2))))))

(defcheck solution-84457138
  #(reduce concat (map (fn [x y] [x y]) %1 %2)))

(defcheck solution-849ce800
  (fn inter[a b](when (and (seq a) (seq b)) (concat [(first a) (first b)] (inter (rest a) (rest b))))))

(defcheck solution-84d86585
  ;#(flatten (map vector % %2))
  mapcat vector)

(defcheck solution-84db9423
  #(loop [x %1 y %2 ret []]
     (if (or (nil? (first x)) (nil? (first y)))
       ret
       (recur (rest x) (rest y) (conj ret (first x) (first y)))
       )
     ))

(defcheck solution-850d0b33
  (fn [s e]
    (flatten
      (map #(list %1 %2) s e)
      )))

(defcheck solution-852deb16
  (fn [& x] (apply mapcat (fn [& col] col) x)))

(defcheck solution-858ea958
  (fn inter [col1 col2]
    (-> (map vector col1 col2) flatten)))

(defcheck solution-8619481e
  (fn [a b]
    (loop [result [], x a, y b]
      (if (and (seq x) (seq y))
        (recur (conj result (first x) (first y)) (rest x) (rest y))
        result))))

(defcheck solution-862badbc
  (comp flatten
        (partial map #(vector %&))))

(defcheck solution-87811dcd
  (fn i [a b]
    (lazy-seq
      (when-not (or (empty? a) (empty? b))
        (cons
          (first a)
          (cons (first b)
            (i (rest a) (rest b))))))))

(defcheck solution-87ce6214
  (fn [l1 l2]
    (apply concat (map #(list %1 %2) l1 l2))))

(defcheck solution-8844a8e6
  (fn i [a b]
    (if (or (empty? a) (empty? b))
      []
      (concat [(first a) (first b)] (i (rest a) (rest b))))))

(defcheck solution-887cd3b1
  ; (fn [s1 s2] (mapcat list s1 s2))
  mapcat list)

(defcheck solution-88800070
  (fn intrl [x y]
    (loop [s1 x s2 y rslt '()]
      (if (or (empty? s1) (empty? s2))
        (reverse rslt)
        (recur (rest s1) (rest s2) (conj rslt (first s1)(first s2)))))))

(defcheck solution-88e5958e
  #(let [n (min (count %1) (count %2))]
     (flatten (for [i (range n)] [(nth %1 i) (nth %2 i)] )) ))

(defcheck solution-89788d9b
  (fn  [list1 list2]
    (loop [l1 list1, l2 list2, acc (vector)]
      (cond (or (empty? l1) (empty? l2)) (seq acc)
            :else (recur (rest l1) (rest l2) (conj acc (first l1) (first l2)))))))

(defcheck solution-89884e69
  (fn [l1 l2]
    (loop [col1 l1 ,col2 l2 ,result [] ]
      (if (or (empty? col1)
              (empty? col2) )
        result
        (recur (rest col1)
          (rest col2)
          (conj (conj result (first col1) )
            (first col2)
            )
          )
        )
      )
    ))

(defcheck solution-89bee35e
  (fn [a b] (flatten (map vector a b))))

(defcheck solution-8a7fb8dd
  #(loop [a %1 b %2 c []]
     (if (or (empty? a) (empty? b))
       c
       (recur (rest a) (rest b) (concat c [(first a) (first b)])))))

(defcheck solution-8ab96f2d
  mapcat #(vector % %2))

(defcheck solution-8bd65e1d
  (fn myInterleave
    ([x y] (myInterleave x y []))
    ([x y acc] (if (and (not (nil? (first x)))(not (nil? (first y)))) (myInterleave (rest x) (rest y) (concat acc [(first x) (first y)])) acc))
    ))

(defcheck solution-8c4a1535
  #(letfn [(worker [l1 l2 t n]
             (cond
               (empty? l1) n
               (and (empty? l2) t) n
               :else (recur l2 (rest l1) (not t) (conj n (first l1)))))]
     (worker %1 %2 true [])))

(defcheck solution-8c8c2ccd
  (fn [xs ys] (mapcat vector xs ys)))

(defcheck solution-8ce50d25
  (fn interleav [s1 s2]
    (if (or (empty? s1) (empty? s2))
      []
      (cons (first s1) (cons (first s2) (interleav (rest s1) (rest s2))))
      )
    ))

(defcheck solution-8dcd96a4
  #(let [result '()](do (flatten (for [i (range (min (count %) (count %2)))] (conj result (%2 i) (% i)))))))

(defcheck solution-8ded5804
  (fn interl [xs ys]
    (if (not (or (empty? xs)
                 (empty? ys)))
      (conj (interl (rest xs) (rest ys))
        (first ys)
        (first xs)))))

(defcheck solution-8e04791d
  (fn myres[xs ys]
    (if (or (empty? xs) (empty? ys))
      nil
      (conj (myres (rest xs) (rest ys)) (first ys) (first xs)))))

(defcheck solution-8e29942c
  (fn my-interleave [f-seq s-seq]
    (mapcat #(list %1 %2) f-seq s-seq)
    ))

(defcheck solution-8e6d5fec
  (fn interleave-1 [c1 c2]
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (first s1) (cons (first s2) (interleave-1 (rest s1) (rest s2))))))))

(defcheck solution-8e91370d
  #(distinct (apply mapcat list %&)))

(defcheck solution-8ec935b1
  (fn [c1 c2] (mapcat #(list %1 %2) c1 c2)))

(defcheck solution-8ecb2d3f
  (fn interleave' [& xss] (when (every? first xss) (concat (map first xss) (apply interleave' (map next xss))))))

(defcheck solution-8eddb333
  #(mapcat identity (map vector % %2)))

(defcheck solution-8ef58c99
  (fn myInterleave
    [& colls]
    "Interleaves a number of collections.
    Concatenates all the first elements in each sequence, then makes a recursive
    call on the parameter collection, which is composed of the rest of all collections."
    (if (some empty? colls) ; make a recursive call if none of the parameter colls are exhausted
      '()
      (concat (map first colls) (apply myInterleave (map rest colls))))))

(defcheck solution-8f288732
  (fn [r c d]
    (if (or (empty? c) (empty? d)) r
                                   (recur (conj r (first c) (first d)) (rest c) (rest d)))) [])

(defcheck solution-8f5ae857
  (fn [l1 l2] (filter #((comp not nil?) %) (flatten (map (fn [[x1 x2]] (map (fn [[y1 y2]] ( if(= y1 x1) (list x2 y2))) (map-indexed vector l2))) (map-indexed vector l1))))))

(defcheck solution-8fb6520c
  mapcat #(list % %2))

(defcheck solution-8fdb528a
  (fn iii [a b]
    (cond
      (and (not-empty a) (not-empty b))
      (concat (list (first a) (first b)) (iii (rest a) (rest b)))
      :else nil)))

(defcheck solution-906110d3
  (fn [seq1 seq2]
    (flatten (map #(list % %2) seq1 seq2))))

(defcheck solution-90c683c1
  (fn [X Y] (loop [ res []
                   x   X
                   y   Y ]
              (if (or (empty? x) (empty? y)) res
                                             (recur (conj (conj res (first x)) (first y)) (rest x) (rest y) ) ) ) ))

(defcheck solution-9156f1f7
  (fn ileave [s1 s2]
    (if (or (empty? s1) (empty? s2)) nil
                                     (concat [(first s1) (first s2)] (ileave (rest s1) (rest s2)))
                                     )
    ))

(defcheck solution-91c15f02
  (fn inter [x, y]
    (if (or (empty? x) (empty? y)) '()
                                   (conj (inter (rest x) (rest y)) (first y) (first x) ))))

(defcheck solution-91f03c9f
  (fn [c1 c2]
    (loop [result [] c1 c1 c2 c2]
      (if (and (first c1) (first c2))
        (recur (conj (conj result (first c1)) (first c2)) (rest c1) (rest c2))
        result))))

(defcheck solution-91f7d14a
  (fn [as bs]
    (loop [a as b bs r []]
      (if (or (empty? a) (empty? b))
        r
        (recur (rest a) (rest b) (conj r (first a) (first b)))))))

(defcheck solution-92446aed
  (fn [x y]
    (loop [l x r y ret []]
      (if-not (and (seq l) (seq r))
        ret
        (recur (rest l) (rest r) (conj ret (first l) (first r)) )))))

(defcheck solution-92889c1d
  (fn alt-zip [seqa seqb]
    (flatten (map vector seqa seqb))))

(defcheck solution-92c85779
  (partial mapcat #(vec %&)))

(defcheck solution-9367070d
  (fn intlv [a b] (when-not(or (empty? a)(empty? b ))(cons (first a)(cons (first b) (intlv (rest a)(rest b)))))))

(defcheck solution-9385570e
  (fn [& vs] (apply mapcat vector vs)))

(defcheck solution-93e95eb2
  (fn interl [xs1 xs2]
    (loop [xs1 xs1 xs2 xs2 res[]]
      (if (or (empty? xs1) (empty? xs2))
        res
        (recur (rest xs1) (rest xs2) (conj res (first xs1) (first xs2)))))))

(defcheck solution-93ef088f
  #(flatten (let [c (min (count %1) (count %2))
                  r []]
              (for [x (range c)]
                (conj r (nth %1 x) (nth %2 x))))))

(defcheck solution-94d41fc7
  (fn f [a b]
    (let [len (min (count a) (count b))]
      (flatten (for [i (range len)] (list (get a i) (get b i)))))))

(defcheck solution-94fc4256
  (fn [seq1 seq2]
    (apply concat (map list seq1 seq2))))

(defcheck solution-951830af
  (fn [a b]
    (if (<= (count a) (count b))
      (flatten (map-indexed (fn [index item] [item (nth b index)]) a))
      (flatten (map-indexed (fn [index item] [item (nth b index)]) (take (count b) a))))))

(defcheck solution-95656c7
  mapcat (fn [x y] [x y]))

(defcheck solution-956721ea
  (fn [c1 c2]
    (loop [i 0 res []]
      (if (>= i (min (count c1) (count c2)))
        res
        (recur (inc i) (conj res (nth c1 i) (nth c2 i)))))))

(defcheck solution-9636ea78
  (fn _interleave [x y]
    (loop [x x y y acc '()]
      (if (or (empty? x) (empty? y))
        (reverse acc)
        (recur (rest x) (rest y) (conj acc (first x) (first y)))))))

(defcheck solution-96b59eda
  (fn [coll1 coll2]
    (loop [dst [] c1 coll1 c2 coll2]
      (if (or (empty? c1) (empty? c2))
        dst
        (recur (conj dst (first c1) (first c2))
          (rest c1)
          (rest c2))))))

(defcheck solution-978d92a0
  (fn [coll1 coll2]
    (flatten (map #(identity [% %2]) coll1 coll2))))

(defcheck solution-97c6c55a
  #(
     mapcat (fn [x y] [x y]) %1 %2
     ))

(defcheck solution-97ecaab2
  (fn [c1 c2]
    (loop [result [] left c1 right c2 ]
      (if (or (nil? left) (nil? right))
        result
        (recur (conj result (first left) (first right)) (next left) (next right) ) ) ) ))

(defcheck solution-980979da
  (fn [& colls] (apply mapcat #(identity %&) colls)))

(defcheck solution-9829e590
  #(loop [x % y %2 res []]
     (if (or (nil? x) (nil? y))
       res
       (recur (next x) (next y) (conj res (first x) (first y))))))

(defcheck solution-98d10e69
  (fn my-inter-leave1
    [f s]
    (flatten (map list f s))))

(defcheck solution-98dc3f18
  (fn  myleave [a b]
    (if (or (empty? a) (empty? b))
      []
      (concat (list (first a) (first b)) (myleave (rest a) (rest b)))
      )
    ))

(defcheck solution-990e274c
  (comp flatten (partial map #(vector %1 %2))))

(defcheck solution-9a2425d9
  (fn [_s1 _s2]
    (loop [s1 _s1, s2 _s2, res []]
      (if (or (empty? s1) (empty? s2))
        res
        (recur (rest s1) (rest s2) (conj res (first s1) (first s2)))))))

(defcheck solution-9a90adf5
  (fn my-interleave [a b] (
                            loop [ret [] a1 a b1 b]
                            (if (or (empty? a1) (empty? b1))
                              ret
                              (recur (conj ret (first a1) (first b1)) (rest a1) (rest b1))
                              ))))

(defcheck solution-9b0b7045
  (fn [s t] (reverse (loop [acc () s1 s s2 t]
                       (if (or (empty? s1) (empty? s2))
                         acc
                         (recur (cons (first s2) (cons (first s1) acc)) (rest s1) (rest s2)
                           ))))))

(defcheck solution-9b4d962a
  (fn inter [x y]

    (if (or (empty? y) (empty? x))
      '()
      (conj (inter (rest x) (rest y)) (first y) (first x)))))

(defcheck solution-9b587dbd
  (fn [& colls] (flatten (apply map list colls))))

(defcheck solution-9c154645
  #(mapcat (fn [a b] (list a b)) %1 %2))

(defcheck solution-9c38ef4f
  (fn [seqs1 seqs2]
    (loop[result '()
          s1 seqs1
          s2 seqs2]
      (if(or (empty? s1)
             (empty? s2))
        result
        (recur (concat result (list (first s1) (first s2)))
          (rest s1)
          (rest s2))))))

(defcheck solution-9c554c0c
  (fn [a b]
    (flatten
      (map #(list % %2) a b))))

(defcheck solution-9d769436
  (fn inter [xs1 xs2]
    (when-let [x1 (first xs1)]
      (when-let [x2 (first xs2)]
        (concat [x1 x2] (inter (rest xs1) (rest xs2)))))))

(defcheck solution-9de0c713
  (comp flatten #(reduce conj [] %) #(map vector %1 %2)))

(defcheck solution-9dec9631
  (fn my-interleave [a b]
    (cond
      (or (empty? a) (empty? b)) nil
      :else (cons (first a)
              (cons (first b)
                (my-interleave (rest a) (rest b)))))))

(defcheck solution-9df57822
  (fn my-interleave [x-seq, y-seq]
    (flatten (if (>= (count x-seq) (count y-seq))
               (map-indexed (fn [idx itm] [(nth x-seq idx) itm]) y-seq)
               (map-indexed (fn [idx itm] [itm (nth y-seq idx)]) x-seq)
               ))))

(defcheck solution-9e953cf1
  (fn f [c1 c2] (if (and (seq c1) (seq c2)) (cons (first c1) (cons (first c2) (f (rest c1) (rest c2)))))))

(defcheck solution-9ec332d6
  #(reduce concat (map list % %2)))

(defcheck solution-9ef565ca
  (fn [xs ys]
    ((fn iter [xs ys res]
       (if (or (empty? xs) (empty? ys))
         res
         (iter (rest xs)
           (rest ys)
           (concat res (cons (first xs) (cons (first ys) ()))) ))) xs ys ())))

(defcheck solution-9f07ebd3
  #(loop [result [] firstseq %1 secondseq %2]
     (if (or (empty? firstseq) (empty? secondseq))
       result
       (recur (conj result (first firstseq) (first secondseq)) (rest firstseq) (rest secondseq))

       )))

(defcheck solution-9f51ac80
  (fn [a b] (apply concat (map vector a b))))

(defcheck solution-9fce79fb
  #((fn inta [x y ol] (if (and (> (count x) 0) (> (count y) 0)) (inta (rest x) (rest y) (conj ol (first x) (first y))) ol)) %1 %2 []))

(defcheck solution-a012af3a
  (fn[v1 v2]
    (loop [ans [] v3 v1 v4 v2]
      (if (or (empty? v3) (empty? v4))
        ans
        (recur (conj (conj ans (first v3)) (first v4))
          (next v3)
          (next v4))))))

(defcheck solution-a0251a1f
  (fn [a b]
    ((fn f [x]
       (mapcat #(if (coll? %)
                  (f %)
                  [%])
         x))
     (mapv vector a b))))

(defcheck solution-a0324a8c
  #(loop [a % b %2 result []]
     (if (= 0 (min (count a) (count b)))
       (flatten result)
       (recur (rest a) (rest b)
         (conj result [(first a) (first b)])))))

(defcheck solution-a0c4addc
  (fn [c1 c2] (reduce concat (map (fn [x y] [x y]) c1 c2))))

(defcheck solution-a0df0671
  (fn
    [s1 s2]
    (loop [ls1 s1 ls2 s2 acc []]
      (if (or (empty? ls1) (empty? ls2))
        acc
        (recur
          (rest ls1)
          (rest ls2)
          (conj acc (first ls1) (first ls2)))))))

(defcheck solution-a0ea0fd0
  #(flatten(map vector %1 %2)))

(defcheck solution-a120ccc1
  #(loop [i 0 n []]
     (if (and (< i (count %1)) (< i (count %2)))
       (recur (inc i) (conj n (%1 i) (%2 i)))
       n
       )))

(defcheck solution-a1403277
  (fn [xs ys] (mapcat (fn [x y] [x y]) xs ys)))

(defcheck solution-a15ad13c
  (fn [left right]
    (mapcat (fn [x y]
              [x y])
      left
      right)))

(defcheck solution-a174acaa
  (fn inter-leave [coll1 coll2]
    (loop [coll1 coll1
           coll2 coll2
           inter ()
           pairs (min (count coll1) (count coll2))
           counter 0]
      (if (= counter pairs)
        inter
        (recur (rest coll1)
          (rest coll2)
          (concat inter `(~(first coll1)) (concat `(~(first coll2))))
          pairs
          (inc counter))))))

(defcheck solution-a24a11cb
  #(loop [xs %1, ys %2, result []]
     (if (some empty? [xs ys])
       result
       (recur (rest xs)
         (rest ys)
         (conj result (first xs) (first ys))))))

(defcheck solution-a29b46f0
  (fn [sq1 sq2] (apply concat (map list sq1 sq2))))

(defcheck solution-a2afcf43
  (fn [a b] (letfn [(stump [a b] (if (or (zero? (count a)) (zero? (count b))) () (cons (first a) (cons (first b) (stump (rest a) (rest b))))))] (stump a b))))

(defcheck solution-a34cd95a
  (fn inter-e
    [x y]
    (loop [a x
           b y
           result '()]
      (cond
        (or (empty? a) (empty? b)) (reverse result)
        :else (recur
                (rest a)
                (rest b)
                (conj (conj result (first a)) (first b)))))))

(defcheck solution-a357df07
  #(loop [a % b %2 out []]
     (if (or (empty? a) (empty? b))out
                                   (recur (rest a) (rest b) (conj out (first a) (first b))))))

(defcheck solution-a39ffe49
  (fn ileave [s1 s2]
    (if (seq s1)
      (if (seq s2)
        (lazy-seq (cons (first s1) (cons (first s2) (ileave (rest s1) (rest s2)))))
        nil)
      nil)))

(defcheck solution-a3d0a5f1
  #(loop [L %2, R %1, result []]
     (if (or (empty? L) (empty? R))
       result
       (recur (rest L) (rest R)
         (conj result (first R) (first L))))))

(defcheck solution-a3dadd28
  (fn [& seqs-in]
    (apply concat
      (map
        (fn get [r] (map #(nth % r) seqs-in))
        (range (reduce min (map count seqs-in)))))))

(defcheck solution-a3ecd9a4
  (fn [a b] (let [n (min (count a) (count b))] (reduce concat (map #(list (nth a %) (nth b %)) (range n))))))

(defcheck solution-a3f26a92
  #(mapcat list % %2))

(defcheck solution-a4f683af
  (fn [as bs]
    (mapcat list as bs)))

(defcheck solution-a529ca70
  (fn problem39-interleave [xs ys]
    (flatten (map vector xs ys))))

(defcheck solution-a5f0073
  (fn [& seqs]
    (apply mapcat (fn [& items] items) seqs)))

(defcheck solution-a5fc20ec
  #(loop [f %1 s %2 n ()]
     (if (or (empty? f) (empty? s))
       n
       (recur (rest f) (rest s) (concat n (cons (first f) (list (first s))))))))

(defcheck solution-a6789a6
  (fn [s s2]
    (loop [s s s2 s2 retVal []]
      (if (or (empty? s) (empty? s2))
        retVal
        (recur (rest s) (rest s2) (conj retVal (first s) (first s2)))))))

(defcheck solution-a70d13ff
  (fn interleaveX [x y] (into (empty x) (mapcat list  x y))))

(defcheck solution-a7d2b9a8
  #(flatten (for [i (range (min (count %1) (count %2)))] [(%1 i) (%2 i)])))

(defcheck solution-a8753780
  (fn [v1 v2] (apply concat (map #(conj [] % %2) v1 v2))))

(defcheck solution-a9345337
  #(loop [s1 %1 s2 %2 result []]
     (if (or (empty? s1) (empty? s2))
       result
       (recur (rest s1)
         (rest s2)
         (conj result (first s1) (first s2))))))

(defcheck solution-a97f660
  #(apply mapcat list %&))

(defcheck solution-a9a80585
  (fn [xs ys]
    (reduce #(into %1 %2)
      [] (map vector xs ys))))

(defcheck solution-aa125981
  (fn [& args] (reduce concat (apply (partial map list) args))))

(defcheck solution-aa2e3879
  (fn [& ls] (apply concat (apply (partial map (fn [& xs] xs)) ls))))

(defcheck solution-aa471ff0
  (fn [seq1 seq2]
    (mapcat #(list %1 %2) seq1 seq2)))

(defcheck solution-aa9c095
  (fn i [a b]
    (if (or (empty? a) (empty? b))
      nil
      (concat (list (first a) (first b)) (i (rest a) (rest b))))))

(defcheck solution-ab4b99f9
  ; mapcat list - is the way to do it :)
  #(flatten
     (for [n (range (apply min (map count %&)))]
       (for [x %& :let [y (nth x n)]] y))))

(defcheck solution-acb02860
  #(flatten (map vector % %2)))

(defcheck solution-ad6b0307
  (fn iter [xs ys]
    (if (and (seq xs) (seq ys))
      (let [[x & xs2] xs
            [y & ys2] ys]
        (cons x (cons y (iter xs2 ys2))))
      '())))

(defcheck solution-af478002
  (fn f [x y] (if (or (= x '()) (= y '())) '() (cons (first x) (cons (first y) (f (rest x) (rest y)))))))

(defcheck solution-af5199bd
  (fn [ x y ]
    (#(
        if (or (empty? %1) (empty? %2))
        %3
        (recur (rest %1) (rest %2) (conj %3 (first %1) (first %2)))
        ) x y []
     )
    ))

(defcheck solution-af81ade
  (fn [col1 col2] (flatten (map #(list %1 %2) col1 col2))))

(defcheck solution-afafc4a3
  ;; handle any number of seqs to interleave, so also 2
  (fn inter ([& lists]
             (if (every? seq lists)
               (concat (map first lists) (apply inter (map rest lists)))
               []))))

(defcheck solution-b14a7616
  (fn [c1 c2]
    (flatten (map #(vec [%1 %2]) c1 c2))))

(defcheck solution-b154b83d
  (fn [a b](mapcat list a b)))

(defcheck solution-b16b2226
  (fn [x y] (apply concat (map #(list %1 %2) x y))))

(defcheck solution-b1af60f5
  (fn [l1 l2]
    (flatten (for [i (range 0 (min (count l1) (count l2)))]
               [(nth l1 i) (nth l2 i)]
               )
      )
    ))

(defcheck solution-b32e43e4
  (fn f [xs ys]
    (if (or (empty? xs) (empty? ys)) ()
                                     (conj (f (rest xs) (rest ys)) (first ys) (first xs)))))

(defcheck solution-b3dfedcb
  mapcat #(into [] [%1 %2]))

(defcheck solution-b4f1d775
  mapcat (fn [& more] (apply list more)))

(defcheck solution-b54e42e3
  (fn [s1 s2] (mapcat list s1 s2)))

(defcheck solution-b5716991
  #(loop [x %
          y %2
          result []]
     (if (or (empty? x) (empty? y))
       result
       (recur (rest x) (rest y) (conj result (first x) (first y))))
     ))

(defcheck solution-b57eb19d
  (fn inlve [a,b]
    (if (and (not (empty? a)) (not (empty? b)))
      (cons (first a) (cons (first b) (inlve (rest a) (rest b))))
      '())))

(defcheck solution-b60d0ea5
  (fn [l r] (flatten (map #(list %1 %2) l r))))

(defcheck solution-b65c3162
  (fn interl [a b]
    (cond
      (or (empty? a) (empty? b)) '()
      :else (cons (first a) (cons (first b) (interl (rest a) (rest b)))))))

(defcheck solution-b6b92e16
  (let [inter
        (fn [x y result]
          (if (or (nil? x) (nil? y))
            result
            (recur (next x) (next y) (conj (conj result (first x)) (first y)))
            )
          )]
    (fn [a b] (inter a b []))
    ))

(defcheck solution-b74e3710
  (fn [a b]
    (loop [a a b b ret []]
      (if (or (empty? a) (empty? b))
        ret
        (recur (rest a) (rest b) (concat ret [(first a) (first b)]))))))

(defcheck solution-b75f7ee7
  #(mapcat list %1 %2))

(defcheck solution-b7888b02
  (fn [x y]
    (letfn [(helper [a b lst]
              (if (or (empty? a)
                      (empty? b))
                lst
                (helper (rest a) (rest b) (conj lst (first a) (first b)))))]
      (helper x y []))))

(defcheck solution-b7a9d221
  #((comp flatten map) list %1 %2))

(defcheck solution-b7ef8663
  (fn ! [x y]
    (when-not (or (empty? x) (empty? y))
      (cons (first x) (cons (first y)
                        (! (rest x) (rest y)))))))

(defcheck solution-b846aca0
  mapcat (fn [e1 e2] [e1 e2]))

(defcheck solution-ba0c0e69
  (fn ileave [a b]
    (if (and (seq a) (seq b))
      (conj (ileave (rest a) (rest b)) (first b) (first a))
      ())))

(defcheck solution-bb2600c3
  (fn intrl[a b](if-not (or (empty? a)(empty? b))
                  (cons (first a)
                    (cons (first b)
                      (intrl (rest a) (rest b))
                      )
                    )
                  '()
                  )
    ))

(defcheck solution-bc3e1050
  (fn [coll1 coll2]
    (loop [c3 []
           c1 coll1
           c2 coll2]
      (if (or (empty? c1) (empty? c2))
        c3
        (recur (conj c3 (first c1) (first c2))
          (drop 1 c1)
          (drop 1 c2))))))

(defcheck solution-bdd02a63
  (fn [a b]
    (let [cnt (dec (min (count a) (count b)))]
      (loop [n cnt res '()]
        (if (= -1 n)
          res
          (recur (dec n) (cons (nth a n) (cons (nth b n) res))))))))

(defcheck solution-bdfade04
  (fn [s1 s2] (mapcat (fn [a b] [a b]) s1 s2)))

(defcheck solution-be25a1a8
  (fn [as bs]
    (loop [[a & as] as
           [b & bs] bs
           res []]
      (let [res (conj res a b)]
        (if (or (empty? as)
                (empty? bs))
          res
          (recur as bs res))))))

(defcheck solution-be694bee
  (fn ileave [s1 s2]
    (if (and (seq s1) (seq s2))
      (concat [(first s1) (first s2)] (ileave (rest s1) (rest s2)))
      [])))

(defcheck solution-bf958b3c
  (fn [ic1 ic2]
    (loop [c1  ic1
           c2  ic2
           res []]
      (if (or (empty? c1) (empty? c2))
        res
        (recur (rest c1) (rest c2) (conj res (first c1) (first c2)))))))

(defcheck solution-bfcbef8c
  (fn
    [a b]
    (letfn
     [(step
        [a b]
        (if (and a b)
          (cons
            (first a)
            (cons
              (first b)
              (step (next a) (next b))))))]
      (step a b))))

(defcheck solution-c0218194
  (fn [xs ys]
    (reverse (loop [xsl xs ysl ys acc '()]
               (if (or (empty? xsl) (empty? ysl))
                 acc
                 (recur (rest xsl) (rest ysl) (cons (first ysl) (cons (first xsl) acc))))))))

(defcheck solution-c1e9a67d
  #(flatten (map list %1 %2)))

(defcheck solution-c2c14bd0
  (fn il [a b]
    (if (and (not-empty a) (not-empty b))
      (concat
       [(first a) (first b)]
       (il (next a) (next b))
       )
      []
      )))

(defcheck solution-c33fdd1
  (fn [coll1  coll2]
    (loop [c1 coll1 c2 coll2 result []]
      (if (or ( empty? c1) (empty? c2))
        result
        (recur (next c1) (next c2) (conj result (first c1) (first c2)))))))

(defcheck solution-c39d10ab
  (fn [left right] (let [recursor (fn recurs [out l r] (if (or (empty? l) (empty? r)) out (recurs (concat out (list (first l) (first r))) (rest l) (rest r))))] (recursor '() left right))))

(defcheck solution-c439ed98
  (fn ileave
    [lista listb]
    (if (and (> (count lista) 0) (> (count listb) 0))
      (conj (ileave (rest lista) (rest listb)) (first listb) (first lista)))))

(defcheck solution-c444b6d3
  #_#(reduce (fn [acc el] (concat acc el)) []
       (map (fn [a b] [a b]) %1 %2))

  #_#(reduce concat
       (map vector %1 %2))

  #_#(mapcat vector %1 %2)

  mapcat vector)

(defcheck solution-c4a3f897
  (fn [c1 c2]
    (loop [c1 c1, c2 c2, res []]
      (if (or (empty? c1) (empty? c2))
        res
        (recur (rest c1) (rest c2) (conj res (first c1) (first c2)))))))

(defcheck solution-c4bb739f
  (fn [s t]
    (flatten (map #(vector %1 %2) s t))))

(defcheck solution-c4c73d69
  (fn i [[x & xs] [y & ys]] (lazy-seq (cons x (cons y (when (and (seq xs) (seq ys)) (i xs ys)))))))

(defcheck solution-c530f2ab
  (fn [a b]
    (loop [a a b b r []]
      (if (and (seq a) (seq b))
        (recur (rest a) (rest b) (conj r (first a) (first b)))
        r))))

(defcheck solution-c55646ac
  (fn [c1 c2]
    (loop [s1 (seq c1) s2 (seq c2) colls ()]
      (if (and (seq s1) (seq s2))
        (let [new-colls (concat colls (list (first s1)) (list (first s2)))]
          (recur (rest s1) (rest s2) new-colls))
        colls))))

(defcheck solution-c56f9b72
  (fn [x y & r]
    (if (or (empty? x) (empty? y))
      (reverse r)
      (recur (rest x)
        (rest y)
        (conj r (first x) (first y))))))

(defcheck solution-c5757eca
  (fn my-interleave [s1 s2] (mapcat list s1 s2)))

(defcheck solution-c758b96e
  (letfn [(iter [acc a b]
            (if (and (not (empty? a)) (not (empty? b)))
              (iter (concat acc (list (first a) (first b))) (rest a) (rest b))
              acc))]
    #(iter () %1 %2)))

(defcheck solution-c75a50d
  #(mapcat (fn [i1 i2] (list i1 i2)) %1 %2))

(defcheck solution-c765df4f
  (fn inter [xs ys]
    (if (or (empty? xs) (empty? ys))
      '()
      (cons (first xs) (cons (first ys) (inter (rest xs) (rest ys)))))))

(defcheck solution-c8a6154e
  (fn inter [left right]
    (let [nargs (min (count left) (count right))]
      (mapcat (fn [a b] [a b]) (take nargs left) (take nargs right))
      )
    ))

(defcheck solution-c929aef
  (fn _interleave [seq_a seq_b]
    (flatten (map list seq_a seq_b))))

(defcheck solution-c92fdad6
  (fn [& ys] (apply concat (apply map (fn [& xs] xs) ys))))

(defcheck solution-c9a8b0a7
  (fn intr
    ([a b] (intr a b ()))
    ([a b accum]
     (if (or (empty? a) (empty? b))
       (reverse accum)
       (let [[x & xs] a
             [y & ys] b]
         (intr xs ys (conj (conj accum x) y)))))))

(defcheck solution-c9d801a4
  (fn f [x y] (if (or (= x []) (= y [])) []
                                         (cons (first x) (cons (first y) (f (rest x) (rest y)))))))

(defcheck solution-c9e15a39
  (fn [seq-x, seq-y]
    (letfn [(merge-seq [seq-x, seq-y]
              (if (or (empty? seq-x)  (empty? seq-y))
                []
                (concat [(first seq-x) (first seq-y)] (merge-seq (rest seq-x) (rest seq-y)))))]
      (merge-seq seq-x seq-y))))

(defcheck solution-ca6a10ae
  (fn [a0 b0]
    (loop [a a0 b b0 result '()]
      (if (and (seq a) (seq b))
        (recur
          (rest a)
          (rest b)
          (conj result (first a) (first b)))
        (reverse result)))))

(defcheck solution-ca8694ee
  (fn my-merge [s0 s1]
    (if (or (empty? s0) (empty? s1)) []
                                     (cons (first s0) (cons (first s1) (my-merge (rest s0) (rest s1)))))))

(defcheck solution-ca9c42f4
  (partial mapcat vector))

(defcheck solution-cba02092
  #(flatten (map (fn [a b] (list a b)) %1 %2)))

(defcheck solution-cbdd3a6d
  (fn [coll1 coll2]
    (mapcat list coll1 coll2)))

(defcheck solution-cc9a85fb
  (fn [x y] (loop [o '() f (seq x) s (seq y)]
              (if (or (nil? f) (nil? s)) (reverse o)
                                         (recur (cons (first s) (cons (first f) o )) (next f) (next s))
                                         )
              )
    ))

(defcheck solution-ccd79ec4
  (fn [x y] (let [fx (fn [res aa bb]
                       (if (and (first aa) (first bb))
                         (recur (conj (conj res (first aa)) (first bb)) (rest aa) (rest bb))
                         res ))]
              (fx [] x y))))

(defcheck solution-cd042009
  (fn [l1 l2] (loop [list1 l1
                     list2 l2
                     result []]
                (if (or (empty? list1) (empty? list2))
                  result
                  (let [item1 (first list1) item2 (first list2)]
                    (recur (rest list1) (rest list2) (conj result item1 item2) ))))))

(defcheck solution-cdf60d45
  (fn ilv [a b]
    (if (or (empty? a)
            (empty? b))
      ()
      (concat
       (list (first a) (first b))
       (ilv (rest a) (rest b))))))

(defcheck solution-cec60adc
  (comp flatten
        (partial map (fn [& args] args))))

(defcheck solution-cfec1f47
  (fn [col1 col2]
    (let [cnt (min (count col1) (count col2))]
      (loop [result [] lcnt cnt c1 col1 c2 col2]
        (if (= lcnt 0)
          result
          (recur (concat result [ (first c1) (first c2) ] )
            (dec lcnt) (rest c1) (rest c2)
            ))))))

(defcheck solution-d0161b70
  #(mapcat (fn [x y] [x y]) % %2))

(defcheck solution-d04a98c7
  (fn m-interleave
    [seq1 seq2]
    (if (or (= 1 (count seq1)) (= 1 (count seq2)))
      (map first [seq1 seq2])
      (concat
       (map first [seq1 seq2])
       (m-interleave (rest seq1) (rest seq2))))))

(defcheck solution-d0c4bf03
  (fn inter [x y]
    (letfn [(inter2 [x y which]
              (if (= which 0)
                (if (or (empty? y) (empty? x)) '()
                                               (concat (list (first x)) (inter2 (rest x) y 1)))
                (if (empty? y) '()
                               (concat (list (first y)) (inter2 x (rest y) 0)))
                )
              )]
      (inter2 x y 0)
      )
    ))

(defcheck solution-d1006acf
  (fn blarg [l1 l2]
    (if (empty? l1) l1 (if (empty? l2) l2
                                       (flatten (into (blarg (rest l1) (rest l2))(conj '() (first l1) (first l2)) ))))))

(defcheck solution-d175d5b
  (fn leave [s1 s2]
    (if (or (empty? s1) (empty? s2)) '()
                                     (conj (leave (rest s1) (rest s2)) (first s2) (first s1)))))

(defcheck solution-d1c349b8
  (fn interleave2 [cola colb]
    (if (or (empty? cola) (empty? colb))
      '()
      (conj (interleave2 (rest cola) (rest colb)) (first colb) (first cola)))))

(defcheck solution-d291fda5
  #(flatten (map (fn [f s]
                   (conj '() s f))
              %1 %2)))

(defcheck solution-d2a6bbb0
  (comp flatten map) #(list %1 %2))

(defcheck solution-d360e09f
  (fn interleavej [s1 s2]
    (cond
      (or (empty? s1) (empty? s2)) '()
      :else
      (concat
       (map first [s1 s2])
       (interleavej (rest s1) (rest s2))))))

(defcheck solution-d3cb09e5
  (fn ninterleave [coll1 coll2]
    (loop [c1 coll1 c2 coll2 acc []]
      (cond
        (or (empty? c1) (empty? c2)) acc
        :else (recur (rest c1)
                (rest c2)
                (conj (conj acc (first c1)) (first c2)))))))

(defcheck solution-d3d58fd8
  (fn x[a b] (if (some empty? [a b]) [] (concat [(first a) (first b)] (x (rest a) (rest b))))))

(defcheck solution-d408a9a1
  (fn f [r a b]
    (if (or (empty? a) (empty? b)) r
                                   (f (conj r (first a) (first b))
                                     (rest a)
                                     (rest b)))) [])

(defcheck solution-d411262
  #(mapcat (fn [x y] (list x y)) %1 %2))

(defcheck solution-d434ee39
  (fn [a b]
    (loop [as a
           bs b
           acc '[]]
      (if (or (empty? as) (empty? bs))
        acc
        (recur (rest as) (rest bs) (conj (conj acc (first as)) (first bs)))
        ))
    ))

(defcheck solution-d47d8eeb
  (fn m [a b]
    (if (= [] a)
      []
      (if (= [] b)
        []
        (cons (first a)
          (cons (first b)
            (m (rest a) (rest b))))))))

(defcheck solution-d5f0fe4e
  #(letfn [(r [s0 s1]
             (if (or (empty? s0) (empty? s1))
               []
               (concat (vector (first s0) (first s1)) (r (rest s0) (rest s1)))))]
     (r %1 %2)))

(defcheck solution-d638376e
  #(flatten (map (fn [a b] [a b]) %1 %2)))

(defcheck solution-d6dc6264
  (fn my-interleave [[x & xs] [y & ys]]
    (if (and x y)
      (concat [x y] (my-interleave xs ys))
      [])))

(defcheck solution-d6e3f504
  #(loop [ r [] x %1 y %2 ]
     (let [a (first x) b (first y)]
       (if (or (= nil a )(= nil b))
         r
         (recur  (conj (conj r a ) b) (rest x) (rest y))))))

(defcheck solution-d727bf21
  (fn [x y & a]
    (if (or (empty? x) (empty? y))
      a
      (recur (rest x) (rest y) (concat a [(first x) (first y)])))))

(defcheck solution-d744d7f1
  #(loop [iter 0
          result []
          size (min (count %1) (count %2))]
     (if (>= iter size)
       result
       (recur (inc iter) (conj result (nth %1 iter) (nth %2 iter)) size))))

(defcheck solution-d74787a3
  (fn [xs ys]
    (flatten (map list xs ys))))

(defcheck solution-d7adec6e
  (fn in [[a1 & b1] [a2 & b2]]
    (if (or (empty? b1) (empty? b2))
      [a1 a2]
      (concat [a1 a2] (in b1 b2)))))

(defcheck solution-d7f78efa
  (fn [a b]
    (loop [col [] s1 a s2 b]
      (let [i1 (first s1) i2 (first s2)]
        (cond
          (and i1 i2) (recur (conj col i1 i2) (rest s1) (rest s2))
          :else col)))))

(defcheck solution-d841af0
  (fn mix [ls rs]
    (if (and (seq ls) (seq rs))
      (cons (first ls) (cons (first rs) (mix (rest ls) (rest rs)))))))

(defcheck solution-d86e737a
  (fn interleave* [s1 s2]
    (if (or (empty? s1) (empty? s2))
      '()
      (concat (list (first s1) (first s2)) (interleave* (rest s1) (rest s2))))))

(defcheck solution-d88b9745
  (fn [x y]
    (mapcat (fn [x y] [x y]) x y)))

(defcheck solution-d8b181eb
  (fn [c1 c2] (mapcat list c1 c2)))

(defcheck solution-d8fe00f7
  (fn [s1 s2] (mapcat #(list %1 %2) s1 s2)))

(defcheck solution-d9160d91
  (fn [l1 l2]
    (loop [result '() l1 l1 l2 l2] (if
                                    (or (empty? l1) (empty? l2))
                                     result
                                     (recur (concat result [(first l1)] [(first l2)]) (rest l1) (rest l2))))))

(defcheck solution-d981ccce
  mapcat #(vector %1 %2))

(defcheck solution-d9e860a0
  (fn [seq1, seq2]
    (loop [s1 seq1, s2 seq2, r []]
      (if
       (or (empty? s1) (empty? s2)) r
                                    (recur (rest s1) (rest s2) (conj (conj r (first s1)) (first s2)))))))

(defcheck solution-d9ea6535
  (fn f [& s]
    (apply concat
      (for [i (range 0 (apply min (map count s)))]
        (map #(nth % i) s)))))

(defcheck solution-dab47cc8
  (fn f [[af & ar] [bf & br]]
    (if (and af bf)
      (concat [af bf] (f ar br) )
      []
      )
    ))

(defcheck solution-daf6bd1b
  #(mapcat (fn [a b] [a b]) %1 %2))

(defcheck solution-db0c4f51
  (fn [a b]
    (loop [[a & rest-a] a [b & rest-b] b ret []]
      (if (and a b)
        (recur rest-a rest-b (conj ret a b))
        ret))))

(defcheck solution-dba6c7aa
  (fn [a b] (flatten (apply map vector [a b]))))

(defcheck solution-dbbd73df
  (fn f [s1 s2]
    (if (or (empty? s1) (empty? s2))
      nil
      (cons (first s1)
        (cons (first s2) (f (rest s1) (rest s2)))))))

(defcheck solution-dbc18c8f
  (fn my-interleave
    [[x & xrest] [y & yrest]]
    (when (and x y)
      (list* x y (my-interleave xrest yrest)))))

(defcheck solution-dbf35f3b
  (fn [xs ys]
    (reduce concat (map list xs ys))))

(defcheck solution-dc0ea3a1
  (fn f[[a & b][x & y]](concat[a x](if(and b y)(f b y)nil))))

(defcheck solution-dc25b8dd
  (fn leaves ([seq1 seq2] (leaves [(first seq1) (first seq2)] (next seq1) (next seq2))) ([acc seq1 seq2] (if (or (empty? seq1) (empty? seq2)) acc (leaves (conj acc (first seq1) (first seq2)) (next seq1) (next seq2))))))

(defcheck solution-dd24e88d
  (fn [coll-1 coll-2]
    (loop [coll-1 coll-1
           coll-2 coll-2
           acc []]
      (if (or (empty? coll-1) (empty? coll-2))
        acc
        (recur (rest coll-1) (rest coll-2) (conj acc (first coll-1) (first coll-2)))
        ))))

(defcheck solution-dd9f201f
  #(for [i (range (min (count %) (count %2)))
         j [0 1]]
     (get (if (= 0 j) % %2) i)))

(defcheck solution-ddb50a4f
  (fn my-interleave [a b]
    (if (and (seq a)
             (seq b))
      (cons (first a) (cons (first b) (my-interleave (rest a) (rest b))))
      '())))

(defcheck solution-de6fa8c
  (fn [a b] (mapcat #(list %1 %2) a b)))

(defcheck solution-de8fff6e
  (fn weave
    ([c1 c2]
     (lazy-seq
       (let [s1 (seq c1) s2 (seq c2)]
         (when (and s1 s2)
           (cons (first s1) (cons (first s2)
                              (weave (rest s1) (rest s2))))))))
    ([c1 c2 & colls]
     (lazy-seq
       (let [ss (map seq (conj colls c2 c1))]
         (when (every? identity ss)
           (concat (map first ss) (apply weave (map rest ss)))))))))

(defcheck solution-de92d7eb
  (fn [xs ys] (flatten (map vector xs ys))))

(defcheck solution-df0bc099
  (fn i[s1 s2]
    (if (or (empty? s1) (empty? s2))
      ()
      (cons (first s1)
        (cons (first s2) (i (rest s1) (rest s2)))))))

(defcheck solution-df81c13b
  (fn i[[x & xs] [y & ys]]
    (if (and x y)
      (conj (i xs ys) y x)
      '())))

(defcheck solution-dfb65beb
  (fn inter [c1 c2]
    (loop [acc [] cc1 c1 cc2 c2]
      (let [[h1 & t1] (seq cc1)
            [h2 & t2] (seq cc2)]
        (if (and cc1 cc2)
          (recur (conj acc h1 h2) t1 t2)
          acc)))))

(defcheck solution-dffe76d
  #(apply concat (map list % %2)))

(defcheck solution-e0121341
  (fn [x y]
    (flatten (map #(vector %1 %2) x y))))

(defcheck solution-e0348fc0
  (fn f [x y] (if (or (empty? x) (empty? y)) '() (conj (f (rest x) (rest y)) (first y) (first x)))))

(defcheck solution-e0374978
  ;#(loop [o []
  ;        x %1
  ;        y %2]
  ;   (if (or (empty? x) (empty? y))
  ;     o
  ;     (recur (into o [(first x) (first y)])
  ;            (rest x)
  ;            (rest y))))

  mapcat list)

(defcheck solution-e0e94920
  mapcat #(conj [] %1 %2))

(defcheck solution-e142545
  (fn [a b]
    (loop [[a b] [a b] result '()]
      (if (some empty? [a b])
        result
        (recur (map rest [a b]) (concat result (map first [a b])))
        )
      )
    ))

(defcheck solution-e177470e
  (fn [l1 l2]
    (loop [l1 l1
           l2 l2
           r '()]
      (if (or (empty? l1)
              (empty? l2))
        (reverse r)
        (recur (rest l1) (rest l2) (conj r (first l1) (first l2)))))))

(defcheck solution-e1b01a45
  (fn [l1 l2] (flatten (map vector l1 l2))))

(defcheck solution-e256f220
  (fn [s1 s2]
    (apply concat (map (fn [a b] [a b]) s1 s2))))

(defcheck solution-e321fd5e
  (fn [coll1 coll2]
    (let [len (min (count coll1) (count coll2))]
      (reduce concat
        (map vector (take len coll1) (take len coll2))))))

(defcheck solution-e36fe4d7
  (fn [xs1 xs2]
    (apply concat
      (map list xs1 xs2))))

(defcheck solution-e3aa484b
  (fn [coll1 coll2] (mapcat (fn [e1 e2] [e1 e2]) coll1 coll2)))

(defcheck solution-e41a653b
  (fn my-interleave [s1 s2]
    (let [c1 (first s1)
          c2 (first s2)]
      (when (and c1 c2)
        (lazy-seq (cons c1 (cons c2 (my-interleave (rest s1) (rest s2)))))))))

(defcheck solution-e42461a7
  (fn f [xs ys]
    (cond
      (or (empty? xs) (empty? ys)) nil
      :else (concat (list (first xs) (first ys)) (f (rest xs) (rest ys))))))

(defcheck solution-e4889f39
  (fn itrlv
    ([lst1 lst2]
     (itrlv lst1 lst2 '()))
    ([lst1 lst2 result]
     (if (or (empty? lst1) (empty? lst2))
       (reverse result)
       (itrlv (rest lst1) (rest lst2) (conj result (first lst1) (first lst2)))))))

(defcheck solution-e4befa2
  (fn [lst1 lst2]
    (loop [x (seq lst1)
           y (seq lst2)
           interleaved []]
      (if (and x y)
        (recur (seq (rest x)) (seq (rest y)) (conj interleaved (first x) (first y)))
        interleaved))))

(defcheck solution-e5274ead
  (fn
    [seq_one seq_two]
    (let [f (fn inter [s1 s2]
              (if (seq s1)
                (cons (first s1) (cons (first s2) (inter (next s1) (next s2))))
                nil))]
      (let* [len (min (count seq_one) (count seq_two))
             s_one (take len seq_one)
             s_two (take len seq_two)]
        (f s_one s_two)))))

(defcheck solution-e56802c7
  (fn
    [seq1 seq2]
    (loop [seq1 seq1 seq2 seq2 list '()]
      (if (and (not-empty seq1) (not-empty seq2))
        (recur (rest seq1) (rest seq2) (conj list (first seq1) (first seq2)))
        (reverse list))
      )
    ))

(defcheck solution-e5c987e9
  (fn my-interleave [x1 x2]
    (lazy-seq
      (when-not (or (empty? x1) (empty? x2))

        (cons (first x1) (cons (first x2)

                           (my-interleave (rest x1) (rest x2))))))))

(defcheck solution-e5e2ec03
  (partial (fn [accum l1 l2]
             (if (or (empty? l1) (empty? l2))
               accum
               (recur
                 (concat accum
                         [(first l1) (first l2)]
                         )
                 (rest l1) (rest l2)
                 )
               )
             )[]))

(defcheck solution-e5f826dc
  (fn [a b]
    (loop [x a
           y b
           count (min (count x) (count y))
           acc []]
      (if (zero? count)
        acc
        (recur (next x) (next y) (dec count) (-> acc (conj (first x)) (conj (first y))))))))

(defcheck solution-e608308f
  (fn [col1 col2]
    (flatten (map (fn [x y] [x y]) col1 col2))
    ))

(defcheck solution-e72fe8a7
  (fn [c1 c2]
    (let [n (min (count c1) (count c2))]
      (loop [c1 c1 c2 c2 c 1 r []]
        (if (= c n)
          (concat r (vector (first c1)) (vector (first c2)))
          (recur (rest c1) (rest c2) (inc c) (concat r (vector (first c1)) (vector (first c2)))))))))

(defcheck solution-e83db6a9
  mapcat (fn [& x] x))

(defcheck solution-e95c3a8d
  (fn [& colls] (apply concat (apply map #(identity %&) colls))))

(defcheck solution-e9c1237b
  #(loop [f %1 s %2 r []]
     (cond (or (empty? f) (empty? s)) r
           :else (recur (rest f) (rest s) (conj r (first f) (first s))))))

(defcheck solution-e9f85ed7
  (fn [xs ys]
    (loop [result []
           t1 xs
           t2 ys]
      (if (or (empty? t1) (empty? t2))
        result
        (recur (conj result (first t1) (first t2)) (next t1) (next t2))))))

(defcheck solution-ea0ec1d4
  (fn [xs ys] (apply concat (map #(list %1 %2) xs ys))))

(defcheck solution-eaca987
  (fn [s1 s2 & ret]
    (if (or (empty? s1) (empty? s2))
      (reverse (flatten ret))
      (recur  (rest s1) (rest s2) (conj ret (first s1) (first s2) ))
      )
    ))

(defcheck solution-ec13ea93
  #(loop [sqnc1 %1
          sqnc2 %2
          sqnc []]
     (if (some empty? [sqnc1 sqnc2])
       sqnc
       (let [newsqnc (conj sqnc (first sqnc1) (first sqnc2))]
         (recur (rest sqnc1) (rest sqnc2) newsqnc)))))

(defcheck solution-ec15da4a
  (fn meu-interleave [s1 s2]
    (cond
      (empty? s1) nil
      (empty? s2) nil
      :else (cons (first s1) (cons (first s2) (meu-interleave (next s1) (next s2)))))))

(defcheck solution-ec3d9b58
  (fn [x y] (flatten (map (fn [a b] (list a b)) x y))))

(defcheck solution-ed00b613
  (fn [coll1 coll2]
    (loop [c1 coll1, c2 coll2, result []]
      (if (or (empty? c1) (empty? c2))
        result
        (recur (rest c1) (rest c2) (conj result (first c1) (first c2)))))))

(defcheck solution-ed67a1f3
  (fn this
    ([xs ys] (this xs ys []))
    ([[xh & xt :as xs] [yh & yt :as ys] acc]
     (if (or (empty? xs) (empty? ys))
       acc
       (recur xt yt (conj acc xh yh))))))

(defcheck solution-edc492a7
  (fn my-interleave
    [x y]
    (flatten (map #(conj [] %1 %2) x y))))

(defcheck solution-ee345665
  (fn f [l1 l2]
    (let [[h1 & t1] l1 [h2 & t2] l2]
      (if (or (nil? h2) (nil? h1))
        []
        (into [h1 h2] (f t1 t2))
        )
      )
    ))

(defcheck solution-ee6a6ce9
  (fn [a b] (flatten (map #(-> [%1 %2]) a b))))

(defcheck solution-ee986fb0
  (fn [xs, ys]
    (reduce concat (map vector xs ys))
    ))

(defcheck solution-eefd665a
  (fn interleave-two-seqs [s1 s2]
    (first (reduce
             (fn [[a, s1] s2]
               (if-not (nil? (first s1))
                 [(conj a (first s1) s2), (rest s1)]
                 [a,s1]))

             [[],s1] s2))))

(defcheck solution-f035c847
  (fn [xs ys]
    (loop
     [xs' xs, ys' ys, r []]
      (if (or (nil? xs') (nil? ys')) r
                                     (recur
                                       (next xs') (next ys') (conj r (first xs') (first ys')))))))

(defcheck solution-f055f1ba
  (fn [a b] (mapcat vector a b)))

(defcheck solution-f0844e48
  mapcat vector)

(defcheck solution-f09e5e2f
  (fn _interleave
    [a b]
    (if (or (= 1 (count a)) (= 1 (count b)))
      (list (first a) (first b))
      (concat
       (list (first a) (first b))
       (_interleave (rest a) (rest b))))))

(defcheck solution-f0a300b
  (fn nterleave
    ([seq1 seq2]
     (nterleave seq1 seq2 '()))
    ([[fseq1 & rseq1] [fseq2 & rseq2] res]
     (if (or (nil? fseq1) (nil? fseq2))
       res
       (recur rseq1 rseq2 (concat res (list fseq1 fseq2)))))))

(defcheck solution-f0c54dbf
  (fn intl [a b & s] #_(println a b s) (if (and (seq a) (seq b)) (intl (rest a) (rest b) (concat s (cons (first a) (list (first b))))   ) (flatten s))  ))

(defcheck solution-f128f500
  (fn [x y] (nth (iterate (fn [z] (conj z (nth x (/ (count z) 2)) (nth y (/ (count z) 2)))) [] ) (min (count x) (count y)) )))

(defcheck solution-f13b8c0b
  (fn [a b]
    (loop [rr [] aa a bb b ff true]
      (if (and ff (or (empty? aa) (empty? bb)))
        rr
        (if ff
          (recur (conj rr (first aa)) (rest aa) bb false)
          (recur (conj rr (first bb)) aa (rest bb) true))))))

(defcheck solution-f15a072e
  #(apply concat (map (fn [x y] [x y]) %1 %2)))

(defcheck solution-f1bca9a3
  (fn [a b] (mapcat list a b)))

(defcheck solution-f1c29c32
  (fn [left right]
    (loop [left left right right acc '[]]
      (if (or (empty? left) (empty? right))
        acc
        (recur (rest left) (rest right) (conj acc (first left) (first right)))
        )
      )
    ))

(defcheck solution-f215966c
  (fn [s1 s2]
    (loop [s1 s1
           s2 s2
           res []]
      (if (or (empty? s1) (empty? s2))
        res
        (let [e1 (first s1)
              e2 (first s2)
              r1 (next s1)
              r2 (next s2)]
          (recur r1 r2 (conj res e1 e2)))

        ))))

(defcheck solution-f25d7ec4
  (comp flatten map) (fn [& xs] xs))

(defcheck solution-f289ec0f
  (fn ! [coll_a coll_b]
    (let [a (seq coll_a) b (seq coll_b)]
      (when (and a b)
        (cons (first a) (cons (first b) (! (rest a) (rest b))))))))

(defcheck solution-f28f7a08
  (fn [left right]
    (flatten (map vector left right))))

(defcheck solution-f2f8b37c
  (fn [a b]
    (loop [res '() c a d b]
      (if (or (empty? c) (empty? d))
        (reverse res)
        (recur (conj res (first c) (first d)) (rest c) (rest d))))))

(defcheck solution-f3369bce
  #(flatten (map (fn [x y] (vector x y)) %1 %2)))

(defcheck solution-f352f593
  (fn inlv[a b] (
                  concat
                 (list (first a))
                 (list (first b))
                 (if (and (> (count a) 1) (> (count b) 1)) (inlv (rest a) (rest b)) '())
                 )))

(defcheck solution-f37b8ce
  (fn [s1 s2]
    (loop [r []
           s1-left s1
           s2-left s2]
      (if (or (empty? s1-left)
              (empty? s2-left))
        r
        (recur (conj r (first s1-left) (first s2-left))
          (rest s1-left)
          (rest s2-left))))))

(defcheck solution-f3fada5e
  #(mapcat identity (map list %1 %2)))

(defcheck solution-f440590c
  (fn [c1 c2] (flatten (for [i (range (min (count c1) (count c2)))] [(nth c1 i) (nth c2 i)]))))

(defcheck solution-f459fe1
  (fn my-interleave [l1 l2]
    (loop [accum '()
           l1 l1
           l2 l2]
      (if (or (empty? l1) (empty? l2))
        accum
        (recur (concat accum (list (first l1) (first l2)))
          (rest l1) (rest l2))))))

(defcheck solution-f48634af
  (fn f [c1 c2]
    (if (or (empty? c1)
            (empty? c2))
      '()
      (conj (conj (f (rest c1) (rest c2))
              (first c2))
        (first c1)))))

(defcheck solution-f4c4ebe8
  #(apply mapcat vector %&))

(defcheck solution-f5a88b7d
  (fn il[v1, v2]
    (flatten (map vector v1 v2))))

(defcheck solution-f5cb66ae
  (fn intrl [[x & xs] [y & ys]]
    (if (and x y)
      (list* x y (intrl xs ys))
      ()
      )))

(defcheck solution-f619b74b
  (fn interleave-two-seqs [col1 col2]
    (if (or (empty? col1) (empty? col2))
      []
      (concat [(first col1) (first col2)] (interleave-two-seqs (rest col1) (rest col2)) ))))

(defcheck solution-f63d9743
  (comp flatten (partial map #(list %1 %2))))

(defcheck solution-f643b0f0
  #(loop [l %1 r %2 result []]
     (if (or (empty? l) (empty? r))
       result
       (recur (rest l) (rest r) (conj result (first l) (first r))))))

(defcheck solution-f6adc690
  (fn interleave* [xs ys]
    (lazy-seq
      (when (not (or (empty? xs) (empty? ys)))
        (cons (first xs) (cons (first ys) (interleave* (rest xs) (rest ys))))))))

(defcheck solution-f6d972db
  (fn [xs ys]
    (loop [xs xs
           ys ys
           zs []]
      (if (and (seq xs) (seq ys))
        (recur (rest xs) (rest ys) (conj zs (first xs) (first ys)))
        zs))))

(defcheck solution-f7ffc071
  (fn [c1 c2] (mapcat vector c1 c2)))

(defcheck solution-f8631335
  (letfn [(intlv [s t] (if (or (empty? s) (empty? t)) nil (cons (first s) (cons (first t) (lazy-seq (intlv (next s) (next t)) ) )) ) )] intlv))

(defcheck solution-f87a1ad9
  (fn new-interleave [a b]
    (if (and (not-empty a) (not-empty b))
      (lazy-seq (cons (first a) (cons (first b) (new-interleave (rest a) (rest b))))))))

(defcheck solution-f8b8da7c
  (fn [xs ys]
    (loop [n 0, result []]
      (if (or (= n (count xs)) (= n (count ys))) result
                                                 (recur (inc n) (conj (conj result (nth xs n)) (nth ys n))))
      )
    ))

(defcheck solution-f97092f4
  (fn peu [x y] (if (or (empty? x) (empty? y)) '() (conj (peu (rest x) (rest y)) (first y) (first x)))))

(defcheck solution-f98212da
  (fn [ms ns]
    (letfn [(inter [xs ys]
              (if (or (empty? xs) (empty? ys))
                (list)
                (cons (first xs)
                  (cons (first ys)
                    (inter (rest xs)
                      (rest ys))))))]
      (inter ms ns))))

(defcheck solution-f9cfa8a1
  mapcat #( identity %& ))

(defcheck solution-f9d318ad
  (fn [c1 c2]
    (flatten (map #(list % %2) c1 c2))))

(defcheck solution-fac36b8b
  (fn [s1 s2]
    (mapcat #(list %1 %2) s1 s2)))

(defcheck solution-fb0da854
  (fn [a b] (flatten (map #(list %1 %2) a b))))

(defcheck solution-fb755bd2
  (fn interleave-two-seqs [x y]
    (if (or (empty? x)
            (empty? y))
      []
      (concat (list (first x))
              (list (first y))
              (interleave-two-seqs (rest x) (rest y))))))

(defcheck solution-fd38edfc
  #(loop [a %1 b %2 res []]
     (if (or (empty? a) (empty? b))
       res
       (recur (rest a)
         (rest b)
         (conj res (first a) (first b))))))

(defcheck solution-fdfeee3
  (fn my_interleave [a b] (if (and a b) (conj (conj (my_interleave (next a) (next b)) (first b)) (first a)))))

(defcheck solution-fe46677b
  #(loop [a (seq %1), b (seq %2), v []]
     (if (or (empty? a) (empty? b))
       (lazy-seq v)
       (recur (rest a) (rest b) (conj v (first a) (first b))))))

(defcheck solution-fe51ead0
  (fn [a b]
    (mapcat #(list %1 %2) a b)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-39))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
