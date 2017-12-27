(ns coal-mine.problem-54
  (:require [coal-mine.checks :refer [defcheck-54] :rename {defcheck-54 defcheck}]
            [clojure.test]))

(defcheck solution-10456947
  #(loop [ans ()
          xs %2]
     (if (> % (count xs))
       (reverse ans)
       (recur (cons  (take % xs) ans) (drop % xs)))))

(defcheck solution-106124a6
  (fn [n l] (take (quot (count l) n) (vals (group-by #(quot % n) l)))))

(defcheck solution-10d13245
  (fn my-partition [n xs]
    (when (>= (count xs) n)
      (cons (take n xs) (my-partition n (drop n xs))))))

(defcheck solution-110a5d31
  (fn part [n coll]
    (let [addpart #(if (< (count (last %)) n)
                     (conj (vec (butlast %)) (conj (last %) %2))
                     (conj % [%2]))]
      (filter #(= n (count %)) (reduce addpart [[]] coll)))))

(defcheck solution-12392d03
  (fn [n c]
    (for [ x (range (quot (count c) n)) ]
      (take n (drop (* n x) c)))))

(defcheck solution-123dd355
  (fn partition*
    [n coll]
    (when (and (seq coll) (>= (count coll) n))
      (cons (take n coll) (lazy-seq (partition* n (drop n coll)))))))

(defcheck solution-1269e7a6
  (fn rec [n s]
    (if (> n (count s)) nil
                        (cons (take n s)
                          (rec n (drop n s))))))

(defcheck solution-12836d8c
  (fn [n coll]
    (loop [xs coll ret []]
      (if (< (count xs) n)
        ret
        (recur (drop n xs) (conj ret (take n xs)))))))

(defcheck solution-12fc6dbb
  (fn  [n coll]
    (reverse ((fn help [xs accu]
                (if
                 (>= (count xs) n)
                  (recur (drop n xs) (cons (take n xs) accu))
                  accu))
              coll '()))))

(defcheck solution-13322b1a
  (fn [n l]
    (partition-by #(quot % n)
      (drop-last (rem (count l) n) l))))

(defcheck solution-13b2ad29
  (fn part [n xs]
    (lazy-seq
      (let [[h t] (split-at n xs)]
        (when (= n (count h))
          (cons h (part n t)))))))

(defcheck solution-143d2e9d
  (fn [n c] (loop [c c r []] (if (< (count c) n) r (recur (drop n c) (conj r (take n c)))))))

(defcheck solution-147c79f8
  (fn my-part [n col]
    (filter #(= (count %) n) (reverse (reduce (fn [acc v]
                                                (if (= (count (first acc)) n)
                                                  (conj acc (list v))
                                                  (conj (rest acc) (concat (first acc) (list v)))
                                                  ))
                                        '(()) col)))
    ))

(defcheck solution-15f3ad45
  (fn p [n coll]
    (if (< (count coll) n)
      '()
      (cons (take n coll) (p n (drop n coll))))))

(defcheck solution-1610bd37
  (fn f [n v]
    (if (>= (count v) n)
      (cons (take n v) (f n (drop n v))))))

(defcheck solution-17267b65
  (fn my-partition [n col]
    (if (< (count col) n)
      (empty col)
      (conj
        (my-partition n (drop n col))
        (take n col)))))

(defcheck solution-1747b3f0
  #(apply map vector (for [n (range %1)] (take-nth %1 (drop n %2))) ))

(defcheck solution-176191e8
  (fn f [a b] (if (> a (count b)) '() (cons (take a b) (f a (drop a b))))))

(defcheck solution-1798739f
  (fn [n s] ((reduce #(let [x (conj (%1 1) %2)] (if (= (count x) n) [(conj (%1 0) x) []] [(%1 0) x])) [[] []] s) 0)))

(defcheck solution-17c99c9c
  #(loop [c %2 res []]
     (if (< (count c) %1) res
                          (recur (drop %1 c)
                            (conj res (take %1 c))))))

(defcheck solution-17caea3a
  #(loop [acc [] coll %2]
     (if (seq coll)
       (if (< (count coll) %1)
         acc
         (recur (conj acc (take %1 coll)) (drop %1 coll)))
       acc)))

(defcheck solution-18778c06
  (fn
    [n s]
    (map (fn [part]
           (map second part))
      (remove #(< (count %) n)
        (partition-by #(int (/ (first %) n)) (map-indexed vector s))))))

(defcheck solution-199bba08
  (fn [n coll]
    (loop [s coll acc []]
      (cond
        (empty? s) acc
        (< (count s) n) acc
        :else (recur (drop n s) (conj acc (take n s))
                )
        )
      )
    ))

(defcheck solution-19d22598
  #(loop [o [] a %2] (let [[f r] (split-at % a)] (if (= % (count f)) (recur (conj o f) r) o))))

(defcheck solution-1a03fd7e
  (fn iter [n coll]
    (let [step (take n coll)]
      (if (= n (count step))
        (cons step (lazy-seq (iter n (drop n coll))))
        nil))))

(defcheck solution-1a0647ee
  (fn p [n v]
    (if (>= (count v) n)
      (cons (take n v) (p n (drop n v))))))

(defcheck solution-1ab16486
  (fn [x sq] (loop [sq sq res []] (if (> x (count sq)) res (recur (drop x sq) (conj res (take x sq)))))))

(defcheck solution-1ae488db
  (fn [n coll]
    (take (int (/ (count coll) n))
      (map #(take n %) (iterate  #(drop n %) coll)))))

(defcheck solution-1b275896
  (fn part [n x]
    (if (>= (count x) n)
      (cons (take n x) (part n (drop n x))))))

(defcheck solution-1b59a5df
  (fn [n coll]
    (loop [left coll result []]
      (let [temp (take n left)]
        (if (< (count temp) n)
          result
          (recur (drop n left) (conj result temp)))))))

(defcheck solution-1b774df4
  (fn part[n cs] (
                   if (>= (count cs) n)
                   (concat (list (take n cs)) (part n (drop n cs)))
                   ()
                   )))

(defcheck solution-1b7b66d6
  (fn [x rnge]
    #_(println x rnge)
    ((fn [candidate rslt remainder]
       #_(println
         candidate rslt remainder)
       (if (empty? remainder)
         (do
           #_(println "empty")
           rslt)
         (if (= (count candidate) (dec x))
           (recur []
             (conj rslt (conj candidate (first remainder)))
             (rest remainder))
           (recur
             (conj candidate (first remainder))
             rslt
             (rest remainder))
           )
         )
       )
     [] [] rnge
     )
    ))

(defcheck solution-1bbe1ed5
  (fn my-partition [n sequence]
    (if (< (count sequence) n)
      []
      (cons (take n sequence) (my-partition n (drop n sequence))))))

(defcheck solution-1c18d581
  (fn [c r]
    (loop [acc [] rin r]
      (if (> c (count rin)) acc (recur (conj acc (take c rin)) (drop c rin))))))

(defcheck solution-1c9b7670
  (fn sins [n s]
    (let [t (take n s)]
      (if (= (count t) n) (cons t (sins n (drop n s))))
      )
    ))

(defcheck solution-1cc98cc1
  (fn [x coll] (filter
                 #(= x (count %))
                 (reduce
                   (fn [a b] (let [c  (last a)
                                   cs (-> a butlast vec)]
                               (apply conj (if (= x (count c)) [a [b]] [cs (conj c b)]))))
                   [[(first coll)]]
                   (rest coll)))))

(defcheck solution-1cce3bef
  (fn
    [n coll]
    (letfn [(partt
              [n coll]
              (if (<= n (count coll))
                (cons (take n coll) (partt n (drop n coll)))
                (list coll)))]
      (drop-last (partt n coll)))))

(defcheck solution-1cceae01
  (letfn [(t [n x]
            (let [start (take n x)
                  finish (nthnext x n)]
              (if (< (count finish) n)
                [start]
                (cons start (t n finish )))))] t))

(defcheck solution-1ce3794e
  (fn f [n xs] (let [[a b] (split-at n xs)] ( if (= (count a) n) (cons a (f n b)) '() ))))

(defcheck solution-1e1e97ac
  (fn [n coll]
    (loop [coll coll acc []]
      (if (< (count coll) n)
        acc
        (recur (drop n coll) (conj acc (take n coll))))
      )
    ))

(defcheck solution-1e2f32d4
  (fn [n c]
    (remove #(not= n (count %)) (for [x (take (/ (count c) n) (iterate (partial + n) n))]
                                  (drop (- x n) (take x c))))))

(defcheck solution-1ebd6bd5
  (fn my-part
    [size coll]
    (loop [result (list (take size coll))
           tail (drop size coll)]
      (if (>= (count tail) size)
        (recur (conj result (take size tail)) (drop size tail))
        (reverse result)))))

(defcheck solution-1ed0f4a6
  (fn peu [x y] (if (< (count y) x) '() (cons (take x y) (lazy-seq (peu x (drop x y)))))))

(defcheck solution-1ed630d1
  (fn [n nums]
    (rest (take-while #(= n (count %))
            (map first
              (iterate #(split-at n (second %)) [(repeat n :a), nums]))))))

(defcheck solution-1ef1b604
  (fn par [n coll]
    (if (<= n (count coll))
      (lazy-seq
        (cons (take n coll) (par n (drop n coll)))))))

(defcheck solution-1f059f61
  (fn part
    ([n s] (part n [] s))
    ([n ps s]
     (if
      (> n (count s))
       ps
       (part n (conj ps (take n s)) (drop n s))))))

(defcheck solution-1f5484dd
  (fn [n coll]
    (filter #(= n (count %))
      (loop [c coll
             acc []]
        (if-not (empty? c)
          (recur (drop n c) (conj acc (vec (take n c))))
          acc)))))

(defcheck solution-1fbe2aa3
  #(reverse(loop [r '()
                  c %2]
             (if (< (count c) %1)
               r
               (recur (conj r (take %1 c)) (drop %1 c))))))

(defcheck solution-2017c70b
  (fn f [n l]
    (when (>= (count l) n)
      (cons (take n l)(f n (drop n l))))))

(defcheck solution-2027ef90
  (fn [n c]
    (loop [a [] c c]
      (let [[h r] (split-at n c)]
        (if-not (= (count h) n)
          a
          (recur (conj a h) r))))))

(defcheck solution-203a7108
  (fn mypartition [n coll]
    (let [x  (take n coll)
          xs (drop n coll)]
      (when (= (count x) n)
        (cons x (mypartition n xs))))))

(defcheck solution-20b9e0f4
  (fn my-part [n s]
    (if (< (count s) n)
      nil
      (lazy-seq (cons (take n s) (my-part n (drop n s)))))))

(defcheck solution-210948a2
  (fn my-p [n xs]
    (if (< (count xs) n)
      nil
      (lazy-seq (cons (take n xs)
                  (my-p n (drop n xs)))))))

(defcheck solution-217c8591
  (fn [x y]
    (loop [a '()
           z y]
      (if (< (count z) x)
        a
        (recur (concat a [(take x z)]) (drop x z))))))

(defcheck solution-21de8491
  #(loop [coll %2 int [] ]
     (if (< (count coll) %1) int
                             (recur (drop %1 coll)
                               (conj int  (take %1 coll)))
                             )))

(defcheck solution-21fb3194
  (fn [n coll]
    (->> (range (/ (count coll) n))
      (map (partial * n))
      (map #(->> coll
              (drop %)
              (take n)))
      (filter #(= n (count %))))))

(defcheck solution-22a52da0
  (fn P [n x*]
    (loop [x* x* p []]
      (if (< (count x*) n) p
                           (recur (drop n x*)
                             (conj p (take n x*)))))))

(defcheck solution-22df0382
  (fn [n l]
    (->> (group-by #(quot % n) l)
      vals
      (remove #(< (count %) n)))))

(defcheck solution-2344d8dc
  (fn [l xs]
    (loop [r [] xs1 xs]
      (if (< (count xs1) l)
        r
        (recur (conj r (take l xs1)) (drop l xs1))))))

(defcheck solution-23c83c5
  (fn part [n l]
    (loop [remaining l
           parts (transient [])]
      (if (>= (count remaining) n)
        (let [[h t] (split-at n remaining)]
          (recur t (conj! parts h)))
        (persistent! parts)))))

(defcheck solution-23cde61b
  (fn partition_ [n s]
    (loop [s_ s acc []]
      (cond
        (empty? s_) (filter #(= (count %) n) acc)
        :else (recur (drop n s_) (conj acc (take n s_)))))))

(defcheck solution-243823f2
  (fn ps [x y]
    (if (> x (count y))
      []
      (cons (take x y)
        (ps x (drop x y))))))

(defcheck solution-247a5c4e
  (fn t [x y]
    (when (>= (count y) x)
      (cons (take x y) (t x (drop x y))))))

(defcheck solution-24a0aab5
  (fn myPartition [n coll] (if (< (count coll) n)
                             []
                             (cons (take n coll) (myPartition n (drop n coll)))
                             )
    ))

(defcheck solution-24b55197
  (fn func
    [num coll]
    (filter #(= num (count %)) (vals (group-by #(quot (.indexOf coll %) num) coll)))))

(defcheck solution-25393c5b
  (fn r [n s] (when (>= (count s) n) (concat (list (take n s)) (r n (drop n s))))))

(defcheck solution-2540541c
  (fn parsec [n c]
    (lazy-seq
      (when-let [s (if (>= (count c) n)  c)]
        (cons (take n s) (parsec n (drop n s)))))))

(defcheck solution-2570596d
  (fn f [n c] (when (<= n (count c)) (cons (take n c) (f n (drop n c))))))

(defcheck solution-25a2716c
  (fn part [n xs]
    (lazy-seq
      (when (<= n (count xs))
        (cons (take n xs) (part n (drop n xs)))))))

(defcheck solution-26e1ca3c
  (fn my-partition [n c]
    (loop [result [] rest c]
      (if (< (count rest) n)
        result
        (recur (conj result (take n rest)) (drop n rest))))))

(defcheck solution-27119733
  (fn [n coll]
    (letfn [(not-partition [n coll]
              (let [p (take n coll)]
                (when (= (count p) n)
                  (cons p (not-partition n (drop n coll))))))]
      (not-partition n coll))))

(defcheck solution-2728f519
  (fn part [n lst]
    (if (<= n (count lst))
      (cons (take n lst)
        (lazy-seq (part n (drop n lst)))))))

(defcheck solution-2794cc22
  (fn partit [x sq]
    (when (<= x (count sq))
      (cons (take x sq) (partit x (drop x sq))))))

(defcheck solution-27d61149
  (fn mypart [n xs]
    (lazy-seq
      (let [packet (take n xs)]
        (if (= n (count packet))
          (cons packet (mypart n (drop n xs))))))))

(defcheck solution-281a2437
  (fn c54
    [n collection]
    (loop [c collection
           current []
           result []]
      (if c
        (let [new-current (conj current (first c))
              nc (next c)]
          (if (= (count current) (dec n))
            (recur nc [] (conj result new-current))
            (recur nc new-current result)))
        result))))

(defcheck solution-285c6d05
  (fn part [n xs]
    (if (> n (count xs))
      nil
      (let [[pre post] (split-at n xs)]
        (cons pre (part n post))))))

(defcheck solution-287be71e
  (fn [x s]
    (map #(take x (drop % s)) (range 0 (- (count s) x -1) x))))

(defcheck solution-2887954a
  (fn [n col] (take (quot (count col) n) (vals (group-by #(quot % n) col)))))

(defcheck solution-290a6695
  (fn [c s] (take (quot (count s) c) (partition-by #(quot % c) s))))

(defcheck solution-2961266f
  #((fn [n c t]
      (if (< (count c) n)
        t
        (recur n (drop n c) (conj t (take n c)))))
    % %2 []))

(defcheck solution-29a6cf29
  (fn [n coll]
    (loop [result []
           s coll]
      (if (seq s)
        (recur (if (>= (count s) n) (conj result (take n s)) result) (drop n s))
        result))))

(defcheck solution-2a1d1e81
  (fn [n l]
    (filter #(= (count %) n)
      (map val
        (group-by #(quot % n) l)))))

(defcheck solution-2a7c5acb
  (fn part [n s]

    (cond (= (count s) n) [s]
          (< (count s) n) nil
          :else (cons (take n s) (part n (drop n s)) )
          )
    ))

(defcheck solution-2a9b1bcc
  (fn part [n coll]
    (lazy-seq
      (let [[x y] (split-at n coll)]
        (when (>= (count x) n)
          (cons x (part n y)))))))

(defcheck solution-2ad74e5e
  (fn f [n coll]
    (if (< (count coll) n)
      []
      (cons (take n coll) (f n (drop n coll))))))

(defcheck solution-2b454e30
  (fn p [i s] (lazy-seq (when (and (seq s) (<= i (count s)))(cons (take i s) (p i (drop i s)))))))

(defcheck solution-2be80c94
  (fn f [n xs]
    (if (< (count xs) n)
      nil
      (let [[part rest] (split-at n xs)]
        (cons part (f n rest))))))

(defcheck solution-2bf1ae16
  #(filter (fn[x] (=(count x) %))
     (reduce (fn [acc e]
               (if (= (count (last acc)) %)
                 (conj acc [e])
                 (conj (vec (butlast acc)) (conj (last acc) e)))
               ) [[]] %2)))

(defcheck solution-2bf902c8
  (fn part [n s]
    (let [nxt (take n s)]
      (if (= (count nxt) n)
        (lazy-seq (cons nxt (part n (drop n s))))
        '()))))

(defcheck solution-2cf97e8d
  (fn my-partition [n v]
    (when (>= (count v) n)
      (cons (take n v)
        (lazy-seq ( my-partition n (drop n v)))))))

(defcheck solution-2df5e860
  (fn my-partition [n s]
    (when (= n (count (take n s)))
      (cons (take n s) (my-partition n (nthnext s n))))))

(defcheck solution-2e079b3c
  (fn partition* [n coll]
    (when (>= (count coll) n)
      (cons (take n coll) (partition* n (drop n coll))))))

(defcheck solution-2e399017
  (fn my-partition [n sx]
    (loop [ret [], part [], remainder sx]
      (cond
        (empty? remainder)
        ret
        (= (inc (count part)) n)
        (recur (conj ret (conj part (first remainder))) [] (rest remainder))
        :else
        (recur ret (conj part (first remainder)) (rest remainder))))))

(defcheck solution-2e518a23
  (fn [n xs]
    ((fn [ys acc]
       (let [as (take n ys)
             bs (drop n ys)]
         (if (empty? as)
           acc
           (recur bs
             (if (= n (count as))
               (conj acc as)
               acc)))))
     xs [])))

(defcheck solution-2e5f0dd2
  (fn [dev c] (reverse
                (filter #(= (count %) dev)
                  (map reverse
                    (reduce #(if (or (empty? %1)
                                     (= (rem %2 dev) 0))
                               (cons (list %2) %1)
                               (cons (conj (first %1) %2) (rest %1))) () c))))))

(defcheck solution-2e5f1214
  (fn [n s]
    (loop [s (seq s) r []]
      (if (>= (count s) n)
        (recur (drop n s) (conj r (take n s)))
        r))))

(defcheck solution-2e6028e0
  #(loop [cur '()
          r %2]
     (if (< (count r) %1)
       (reverse cur)
       (recur (conj cur (take %1 r)) (drop %1 r)))))

(defcheck solution-2e82a69f
  (fn f [n c]
    (when (>= (count c) n)
      (cons (take n c) (f n (drop n c))))))

(defcheck solution-2eb0e6ae
  (fn [n xs]
    (loop [xs' xs rs []]
      (if (< (count xs') n)
        rs
        (recur (drop n xs') (conj rs (take n xs')))
        ))
    ))

(defcheck solution-2ee7cc99
  (fn [n seq]
    (reverse (loop [seq seq
                    acc ()]
               (let [group (take n seq)]
                 (if (= n (count group))
                   (recur (drop n seq) (cons group acc))
                   acc))))))

(defcheck solution-2f246afd
  (fn group [n coll]
    (loop [[subset remaining] (split-at n coll)
           result []]
      (if (< (count subset) n)
        result
        (recur (split-at n remaining) (conj result subset))))))

(defcheck solution-2f627c4a
  #(loop [x  (take %1 %2)
          xs (drop %1 %2)
          acc []]
     (if-not (= %1 (count x))
       acc
       (recur (take %1 xs)
         (drop %1 xs)
         (conj acc x)))))

(defcheck solution-3034a0b4
  (fn pt [n s]
    (lazy-seq (when (>= (count s) n)
                (cons (take n s) (pt n (drop n s)))))))

(defcheck solution-303561f5
  (fn [n coll]
    (first
      (reduce
        (fn [[acc k cur] el]
          (if (= k n)
            [(conj acc (conj cur el)) 1 []]
            [acc (inc k) (conj cur el)]
            ))
        [[] 1 []]
        coll))))

(defcheck solution-30f5d64f
  (fn [n w]
    (let [aux
          (fn [[head & tail] curr res]
            (cond
              (nil? head)
              (let [out (if (= n (count curr))
                          (conj res curr)
                          res)]
                (seq out))

              (< (count curr) n)
              (recur tail (conj curr head) res)

              :else
              (recur tail [head] (conj res curr))))]
      (aux w [] []))))

(defcheck solution-31128d24
  (fn [c s]
    (->> s
      (reduce (fn [acc it]
                (if (-> acc first count (= c))
                  (conj acc (vector it))
                  (cons (conj (first acc) it) (rest acc))))
        (list []))
      reverse
      (filter #(= (count %) c)))))

(defcheck solution-31971d62
  (fn f [n xs]
    (let [[ys zs] (split-at n xs)]
      (when (>= (count ys) n)
        (cons ys (f n zs))))))

(defcheck solution-31ee2d2f
  (fn p [n coll]
    (when (>= (count coll) n)
      (let [[f r] (split-at n coll)]
        (cons f (when-let [s (seq r)]
                  (p n s)))))))

(defcheck solution-32010c7c
  #(first (partition-by count (map (partial map first) (partition-by last (map list %2 (mapcat (partial repeat %1) (range))))))))

(defcheck solution-32f13024
  (fn part [n coll]
    (when (>= (count coll) n)
      (cons (take n coll) (part n (drop n coll))))))

(defcheck solution-330394e
  (fn partition-sequence [n s]
    (if (< (count s) n) ()
                        (cons (take n s) (partition-sequence n (drop n s))))))

(defcheck solution-33848374
  (fn [par-num coll]
    (->> (reduce (fn [acc item]
                   (if (= (count (first acc)) par-num)
                     (conj acc (list item))
                     (conj (rest acc) (conj (first acc) item))))
           '(())
           coll)
      (filter #(= par-num (count %)))
      (map reverse)
      reverse)))

(defcheck solution-33d3c3ce
  #(loop [c % a %2 seq []] (if (>= (count a) c) (recur c (drop c a) (into seq (list (take c a)))) seq)))

(defcheck solution-33fd220f
  (fn t [n c] (lazy-seq (let [[h r] (split-at n c)] (when (= n (count h)) (cons h (t n r)))))))

(defcheck solution-34a91962
  (fn [sz sq] (map #(take sz (nthrest sq (* sz %))) (range (quot (count sq) sz)))))

(defcheck solution-34afcc07
  (fn [n r]
    (loop [vs ()
           rs r]
      (if (< (count rs) n)
        (reverse vs)
        (recur (conj vs (take n rs)) (drop n rs)))
      )
    ))

(defcheck solution-34b01b13
  (fn [n coll]
    (loop [r [] c coll]
      (if (> n (count c))
        r
        (recur (conj r (take n c)) (drop n c))))))

(defcheck solution-354b6deb
  (fn parti [n xs]
    (loop [xs xs, acc []]
      (let [taken (take n xs)]
        (if (< (count taken) n) acc
                                (recur (drop n xs) (conj acc taken)))))))

(defcheck solution-35e67659
  (fn [n coll]
    (loop [partitioned []
           to-partition coll]
      (let [taken (take n to-partition)
            left (drop n to-partition)]
        (if (< (count taken) n)
          partitioned
          (recur (conj partitioned taken)
            left))))))

(defcheck solution-3633ffad
  (fn partition' [n xs]
    (if (>= (count xs) n)
      (cons (take n xs) (partition' n (drop n xs)))
      '())))

(defcheck solution-366b25
  (fn p [n c]
    (if (> n (count c))
      nil
      (let [[l r] (split-at n c)] (cons l (p n r))))))

(defcheck solution-3673d70d
  (fn part [n seq]
    (if (< (count seq) n)
      []
      (cons (take n seq) (part n (drop n seq))))))

(defcheck solution-36874b31
  (fn [step coll]
    (loop [acc [] coll coll]
      (if (< (count coll) step)
        acc
        (recur (conj acc (take step coll)) (drop step coll))))))

(defcheck solution-36a38494
  (fn my-partition [n xs]
    (if (< (count xs) n)
      '()
      (cons (take n xs) (my-partition n (drop n xs))))))

(defcheck solution-36bcd0c6
  (fn [n s]
    (apply map vector (map #(take-nth n (drop % s)) (range n)))))

(defcheck solution-37672d60
  (fn
    [num coll]
    (first (reduce (fn [[res lst i] e]
                     (if (zero? (mod i num))
                       [(conj res (conj lst e)) [] (inc i)]
                       [res (conj lst e) (inc i)]))
             [[] [] 1]
             coll))))

(defcheck solution-37b1aab8
  (fn [n coll]
    (loop [remaining coll
           ret '()]
      (if (< (count remaining) n)
        (reverse ret)
        (recur (drop n remaining) (cons (take n remaining) ret))
        )
      )
    ))

(defcheck solution-37cffedf
  (fn f [n xs]
    (let [ys (take n xs) c (count ys)]
      (if (< c n) ()
                  (cons ys (f n (drop n xs)))))))

(defcheck solution-3823af06
  (fn [n col]
    (filter #(>= (count %) n)
      (for [off (range 0 (count col) n)]
        (take n (drop off col))))))

(defcheck solution-38f49a0a
  (fn pt [n coll]
    (lazy-seq
      (let [[p r] (split-at n coll)]
        (if (= n (count p))
          (cons p (pt n r)))))))

(defcheck solution-3933779d
  (fn mypart [n xs]
    (loop [acc [] rs xs]
      (if (< (count rs) n)
        acc
        (recur (conj acc (take n rs)) (drop n rs))))))

(defcheck solution-394389e4
  (fn [neach col]
    (filter #(= (count %) neach)
      (reduce (fn [ret this]
                (let [t (last ret)]
                  (if (nil? t)
                    (conj ret [this])
                    (if (< (count t) neach)
                      (conj (pop ret) (conj t this))
                      (conj ret [this]) ))))
        [] col))))

(defcheck solution-39b77b40
  (fn [n seq]
    (filter #(= (count %) n) (map #(map last %) (map last (group-by #(quot (first %) n) (map-indexed vector seq)))))))

(defcheck solution-39ead582
  (fn meu-part [n s]
    (when (>= (count s) n)
      (cons (take n s) (meu-part n (drop n s))))))

(defcheck solution-39f9b6a1
  #(map (fn [c] (take % (drop (* % c) %2)))
     (range 0 (quot (count %2) %))))

(defcheck solution-3a36226
  (fn [n xs]
    (loop [xs xs
           result []]
      (let [[head tail] (split-at n xs)]
        (if (= n (count head))
          (recur tail (conj result head))
          result)))))

(defcheck solution-3a4928ca
  (fn bl[tgt n lst]
    (if (< (count lst) n) (reverse tgt)
                          (bl (cons (take n lst) tgt) n (drop n lst)))) '())

(defcheck solution-3a669a7b
  (fn part [n xs]
    (if (<= n (count xs))
      (lazy-seq (cons (vec (take n xs))
                  (part n (drop n xs))))
      [])))

(defcheck solution-3a912e12
  (fn p [n c]
    (lazy-seq
      (let [x (take n c)]
        (if (= n (count x))
          (cons x (p n (drop n c))))))))

(defcheck solution-3abc3535
  (fn my-partition [n coll]
    (let [bites (quot (count coll) n)
          steps (map #(* n %) (range bites))]
      (map #(take n (drop % coll)) steps))))

(defcheck solution-3ae8c4d2
  (fn [n coll] (let [l (reduce #(if (= (count (last %1)) n)
                                  (concat %1 (list (list %2)))
                                  (concat (butlast %1) (list (concat (last %1) (list %2)))))
                         '(()) coll)]
                 (if (< (count (last l)) (count (first l)))
                   (butlast l)
                   l))))

(defcheck solution-3bb1e2b5
  (fn [x y]
    (let [out (reverse
                (reduce
                  (fn [[a & as :as stack] i]
                    (if (= (count a) x)
                      (if (nil? as)
                        (conj (reverse a) (list i))
                        (if (sequential? (first as))
                          (conj as (reverse a) (list i))
                          (conj (list as) (reverse a) (list i))
                          )
                        )
                      (conj as (conj a i))
                      )
                    )
                  ()
                  y
                  )
                )
          ]
      (concat (drop-last out) (if (= (count (last out)) x) (list (reverse (last out))) nil))
      )
    ))

(defcheck solution-3bb4341
  (fn [n xs]
    (loop [res []
           n n
           realN n
           xs xs
           ]
      (let [cand (take realN xs)]
        (if (= (count cand) n)
          (recur (conj res cand) n realN (drop realN xs))
          res

          )))
    ))

(defcheck solution-3c022916
  (fn f [n s]
    (if (> n (count s))
      nil
      (conj (f n (drop n s)) (take n s)))))

(defcheck solution-3c204c26
  (fn s
    [n f ] (let [[t d] (split-at n f)] (when (>= (count t) n) (cons t (s n d))))))

(defcheck solution-3c3d7cbd
  (fn [n l] (loop [l1 l l2 []] (if (>= (count l1) n) (recur (drop n l1) (conj l2 (take n l1))) l2))))

(defcheck solution-3c9aa50e
  (fn [n coll]
    (loop [c coll partitioned []]
      (if (< (count c) n)
        partitioned
        (recur (drop n c) (conj partitioned (take n c)))))))

(defcheck solution-3cd7a292
  (fn [n lst]
    (map
      #(take n (drop (* n %) lst))
      (range (quot (count lst) n)))))

(defcheck solution-3cdf8ee5
  (fn [n coll]
    (for [x (range (quot (count coll) n))]
      (take n (drop (* x n) coll)))))

(defcheck solution-3d64a980
  #(loop [n %1 coll %2 result []] (if (> n (count coll)) result (recur n (drop n coll) (conj result (take n coll))))))

(defcheck solution-3d7b48f0
  #(loop [coll %2, result []]
     (if (< (count coll) %1)
       result
       (recur (drop %1 coll) (conj result (take %1 coll))))))

(defcheck solution-3d97e16a
  ;(fn lazy-partition' [n coll]
  ;  (lazy-seq
  ;   (when-let [s (seq coll)]
  ;     (let [p (doall (take n s))]
  ;       (when (= n (count p))
  ;         (cons p (partition' n (nthrest coll n))))))))
  ;(fn partition' [n coll]
  ;  (loop [acc []
  ;         remaining coll]
  ;    (if (<= n (count remaining))
  ;      (recur (conj acc (take n remaining)) (drop n remaining))
  ;      acc)))
  (fn partition' [n s]
    (loop [in s out []]
      (if (<= n (count in))
        (recur (drop n in) (conj out (take n in)))
        out))))

(defcheck solution-3db1bf05
  (fn this [n coll]
    (let [[a b] (split-at n coll)]
      (when (= n (count a))
        (cons a (this n b))))))

(defcheck solution-3dbf6905
  (fn my-partition [sz coll]
    (loop [current coll agg []]
      (if (or (empty? current) (< (count current) sz)) agg
                                                       (recur (drop sz current) (conj agg (take sz current)))))))

(defcheck solution-3ddbb35e
  (fn cutemup [x alist]
    (if (< (count alist) (* 2 x))
      [(take x alist)]
      (cons (take x alist) (cutemup x (drop x alist))))))

(defcheck solution-3e219bba
  (fn [a b] (let [r (partition-by #(int (/ % a)) b)] (if (= (mod (count b) a) 0) r (butlast r)))))

(defcheck solution-3e682a45
  (fn par [n xs]
    (if (< (count (drop n xs)) n)
      (list (take n xs))
      (concat (list (take n xs)) (par n (drop n xs))))))

(defcheck solution-3e6bdd94
  (fn my-partition [n coll]
    (if (> n (count coll))
      '()
      (concat (list (take n coll)) (my-partition n (drop n coll))))))

(defcheck solution-3e9d158f
  (fn p [n coll] (when (>= (count coll) n) (cons (take n coll) (lazy-seq (p n (drop n coll)))))))

(defcheck solution-3ea9ea44
  (fn _partition [len items]
    (if (< (count items) len)
      nil
      (conj (_partition len (drop len items)) (take len items)))))

(defcheck solution-3eee13e8
  (fn p [n xs]
    (if (= (count (take n xs)) n)
      (cons (take n xs)
        (p n (drop n xs)))
      [])))

(defcheck solution-3ef720de
  (fn [n s]
    (loop [in s out []]
      (if (> n (count in))
        out
        (recur (drop n in) (conj out (take n in)))))))

(defcheck solution-3f293867
  (fn [n x]
    (take-while
      #(= (count %) n)
      (map
        #(take n (drop (* n %1) %2))
        (range)
        (repeat x)))))

(defcheck solution-3f90ce4f
  #(loop [c %2 acc []]
     (let [f (take % c) r (drop % c)]
       (if (< (count r) %)
         (conj acc f)
         (recur r (conj acc f))))))

(defcheck solution-401e258b
  (fn par [n s]
    (let [first-n (take n s)]
      (if (= n (count first-n))
        (cons first-n (par n (drop n s)))
        nil
        )
      )
    ))

(defcheck solution-4053f24e
  (fn f [n,s]
    (if (< (count s) (* 2 n))
      (list (take n s))
      (cons (take n s) (f n (nthnext s n))))))

(defcheck solution-4071283c
  (fn partishun [n s]
    (let [piece (take n s)]
      (when (>= (count piece) n)
        (lazy-seq (cons piece (partishun n (drop n s))))))))

(defcheck solution-40f14d3
  (fn [n coll]
    (loop [init-seq coll
           res-seq []]
      (if (< (count init-seq) n)
        res-seq
        (recur (drop n init-seq) (conj res-seq (take n init-seq)))))))

(defcheck solution-41adb7d0
  (fn p [n l]
    (when (>= (count l) n)
      (cons (take n l) (p n (drop n l))))))

(defcheck solution-41b2bf60
  (fn part-seq [n v]
    (let [rv (take n v)]
      (if (= (count rv) n)
        (cons rv (lazy-seq (part-seq n (drop n v))))
        nil))))

(defcheck solution-41d70343
  (fn test-fn [n l]
    (if (< (count l) n)
      nil
      (let [z (split-at n l)]
        (cons (first z) (test-fn n (second z)))))))

(defcheck solution-41ddf898
  (fn [x input]
    (loop [xs input, result []]
      (if (< (count xs) x) result
                           (recur (drop x xs) (into result (vector (take x xs))))
                           )
      )))

(defcheck solution-4268f51a
  (fn own-partition [n, s]
    (let [do-spliting (fn [n2 s2]
                        (loop [result [], s3 s2]
                          (if (<= (count s3) n2)
                            (conj result s3)
                            (let [res (split-at n2 s3)]
                              (recur (conj result (first res)) (second res))))))
          splitted (do-spliting n s)]
      (if (< (count (last splitted)) n)
        (drop-last splitted)
        splitted))))

(defcheck solution-428d4c9f
  (fn f [n s] (if (<= n (count s))
                (cons (take n s) (f n (drop n s))))))

(defcheck solution-42eab06a
  (fn [ n x ] (loop [y x, c [] ]
                (if (< (count y) n) c
                                    (recur (drop n y) (conj c (take n y)))))))

(defcheck solution-4302dc3f
  #(loop [stock '() next %2]
     (let [-add (take % next)]
       (if (= (count -add) %)
         (recur (conj stock -add) (drop %  next))
         (reverse stock)
         )
       )))

(defcheck solution-43704e7e
  (fn parti [len ve]
    (cond
      (< (count ve) len) nil
      (empty? ve)     ve
      :else (conj (parti len (drop len ve)) (take len ve)))))

(defcheck solution-4374130c
  (fn[a b]

    (filter #(=  (count %) a)
      (
        loop [z b y [] ]
        (if
         (not-empty z)
          (recur
            (drop a z)
            (conj y (take a z))
            )
          y
          )
        )
      )))

(defcheck solution-43eb6bcd
  #(if (< (count %3) %2)
     %
     (recur (conj % (take %2 %3))
       %2
       (drop %2 %3))) [])

(defcheck solution-44a99987
  (fn [n c] (map list* (filter #(= n (count %)) (reduce (fn [a x] (let [l (last a)] (if (> n (count l)) (conj (vec (butlast a)) (conj l x)) (conj a [x])))) [[]] c)))))

(defcheck solution-44b0792f
  (fn prtn [n xs]
    (if (< (count xs) n)
      ()
      (cons (take n xs) (prtn n (drop n xs))))))

(defcheck solution-44e5c3a4
  (fn part
    [n lista]
    (if (or (empty? lista) (> n (count lista)))
      '()
      (list* (take n lista) (part n (drop n lista))))))

(defcheck solution-453cda27
  (fn f [n s]
    (if (<= n (count s))
      (cons (take n s) (f n (drop n s)))
      [])))

(defcheck solution-45a603c9
  (fn p1 [n coll] (let [[fs rs] (split-at n coll)]
                    (lazy-seq (cons fs (when (>= (count rs) n) (p1 n rs)))))))

(defcheck solution-4665188d
  #(loop [a []
          c %2]
     (if (> % (count c))
       a
       (let [[b & x] (split-at % c)]
         (recur (conj a b) (last x))))))

(defcheck solution-468fa95
  (fn p [n x] (let [r (take n x)
                    c (count x)]
                (if (= c n) [r]
                            (if (< c n) []
                                        (cons r (p n (drop n x))))))))

(defcheck solution-469c9d7d
  (fn par [n lst]
    (if (> n (count lst))
      []
      (cons (take n lst) (par n (drop n lst))))))

(defcheck solution-470c2889
  #(loop [n %1 input %2 result []]
     (if (> n (count input))
       result
       (recur n (drop n input) (conj result (take n input))))))

(defcheck solution-476dd2a7
  (fn [n S]
    (for [x (range 0 (- (count S) (mod (count S) n)) n) ]
      (for [y (range x (+ n x))]
        (nth S y)
        )
      )
    ))

(defcheck solution-477c24c7
  (fn part [n s]
    (if (< (count s) n)
      '()
      (cons (take n s) (lazy-seq (part n (drop n s)))))))

(defcheck solution-47d44021
  (fn partition2 [n s]
    (loop [res []
           leftover s]
      (if (= leftover [])
        (if (< (count (last res)) n)
          (butlast res)
          res)
        (recur (conj res (take n leftover))
          (drop n leftover))))))

(defcheck solution-4898c191
  (fn my-partition [n coll]
    (let [part (take n coll)]
      (when (= n (count part)) (cons part (lazy-seq (my-partition n (drop n coll))))))))

(defcheck solution-4936fe0e
  (fn [n l]
    (loop [m l
           r []]
      (let [e (take n m)]
        (if (< (count e) n)
          r
          (recur (drop n m) (conj r e)))))))

(defcheck solution-49946543
  (fn [n coll]
    (loop [elms coll out []]
      (if (seq elms)
        (recur (drop n elms) (if (<= n (count elms))
                               (conj out (take n elms))
                               out))
        out))))

(defcheck solution-49afda29
  (fn [n items]
    (first (reduce (fn [[result parts] x]
                     (if (= (dec n) (count parts))
                       [(conj result (conj parts x)) []]
                       [result (conj parts x)]))
             [[][]]
             items))))

(defcheck solution-49ec674c
  (fn f [n s]
    (if (< (count s)  n)
      '()
      (cons (take n s) (f n (drop n s))))))

(defcheck solution-4ae71e48
  (fn [n xs]
    (loop [res [] ys xs]
      (let [[beg end] (split-at n ys)]
        (if (< (count beg) n)
          res
          (recur (conj res beg) end))))))

(defcheck solution-4bc6fdd8
  #(map (fn [i] (take % (drop (* i %) %2))) (range (quot (count %2) %))))

(defcheck solution-4c35369e
  (fn _partitaion
    ([n xs] (_partitaion n xs '()))
    ([n xs acc] (if (< (count xs) n)
                  acc
                  (recur n (drop n xs) (concat acc (list (take n xs))))))))

(defcheck solution-4c3d227f
  (fn this [x s]
    (let [nxt (take x s)]
      (if (= (count nxt) x)
        (cons (take x s)
          (lazy-seq (this x (nthrest s x))))
        '()))))

(defcheck solution-4c493681
  (fn my-partition [size sq]
    (if (< (count sq) size)
      '()
      (concat
       (list (take size sq))
       (my-partition size (drop size sq))))))

(defcheck solution-4c53b756
  (fn [n coll]
    (filter #(= n (count %))
      (loop [results [] items coll]
        (cond
          (empty? items) results
          (or (empty? results) (= n (count (last results)))) (recur (vec (conj results [(first items)])) (rest items))
          :else
          (recur (vec (conj (vec (butlast results)) (vec (conj (last results) (first items))))) (rest items))
          )
        )
      )
    ))

(defcheck solution-4cda044a
  (fn part [n li]
    (let [head (take n li)]
      (if
       (< (count head) n)
        '()
        (cons
          head
          (part n (drop n li)))))))

(defcheck solution-4cf268dd
  (fn partition2 [n coll]
    (lazy-seq
      (when (>= (count coll) n)
        (cons (take n coll) (partition2 n (drop n coll)))))))

(defcheck solution-4d040b04
  (fn __ [n xs]
    (if (< (count xs) n) '()
                         (conj (__ n (drop n xs)) (take n xs)))))

(defcheck solution-4d7e132b
  (fn [n s]
    (apply (partial map vector)
      (map #(take-nth n (drop % s)) (range n)))))

(defcheck solution-4dabe89e
  #(let [! (fn [v]
             (concat (butlast v)
                     (split-at %1 (last v))))]
     (filter (fn [x] (= (count x) %1)) (last (take-while (fn [i] ((complement empty?) (last i))) (iterate ! [%2]))))))

(defcheck solution-4ee9b45a
  #((partial (fn x [n s]
               (if-not (> n (count s))
                 (cons (take n s)
                   (x n (drop n s))))) %1) %2))

(defcheck solution-4f0b09ac
  (fn p [n seq]
    (let [s (split-at n seq)
          lst (last s)
          cnt (count lst)]
      (if (>= cnt n)
        (concat (butlast s) (p n lst))
        (if (= cnt n) s (butlast s))))))

(defcheck solution-4fc79d7d
  (fn g
    ([n xs] (g '() n xs))
    ( [r n xs] (if (seq xs) (recur (cons (take n xs) r) n (drop n xs)) (filter #(= n (count %)) (reverse  r)))  )  ))

(defcheck solution-4fe624a4
  (fn [i c]
    (loop [v c r []]
      (if (< (count v) i)
        r
        (recur (nthrest v i) (conj r (take i v)))))))

(defcheck solution-5019f559
  (fn p [n xs]
    (if (< (count xs) n) ()
                         (cons (take n xs)
                           (p n (drop n xs))))))

(defcheck solution-503253c4
  (fn p[x y] (if (>= (count y) x) (cons (take x y) (p x (drop x y))))))

(defcheck solution-504b97ff
  (fn part [n seq]
    (if (> n (count seq))
      '()
      (cons (take n seq) (part n (drop n seq))))))

(defcheck solution-5093eb48
  (fn [n c]
    (for [x (range (quot (count c) n))]
      (take n (nthrest c (* n x))))))

(defcheck solution-51156791
  (fn [n coll]
    (loop [coll coll
           result []]
      (if (>= (count coll) n)
        (recur (drop n coll) (conj result (take n coll)))
        result))))

(defcheck solution-5173959e
  (fn f [n i]
    (map #(range % (+ % n))
      (map #(* % n)
        (range (quot (count i) n))))))

(defcheck solution-51dfc4c2
  (fn [slc sq]
    ; sequence with remainder trimmed.
    (let [trim (drop-last (rem (count sq) slc) sq)]
      ; each segment is the length of our trimmed sequence starting
      ; from 0 and using the slice as the step.
      (for [seg (range 0 (count trim) slc)]
        ; drop the first n items and take a slice.
        (take slc (drop seg trim))))))

(defcheck solution-5420c2bb
  (fn [n s]
    (loop [left [] right s]
      (if (< (count right) n) left
                              (recur (conj left (take n right)) (drop n right))))))

(defcheck solution-54af9d6b
  (fn my-part [n s]
    (if (< (count s) n)
      []
      (cons (take n s) (my-part n (drop n s))))))

(defcheck solution-54cd5480
  (fn [n s]
    (reverse
      (let [groups (reduce
                     (fn [x y]
                       (if (< (count (first x)) n)
                         (cons (conj (first x) y) (rest x))
                         (cons [y] x)))
                     (list [])
                     s)]
        (if (< (count (first groups)) n) (rest groups) groups)))))

(defcheck solution-54e33ce6
  (fn [n coll]
    (loop [c coll, acc []]
      (if (< (count c) n)
        acc
        (recur (drop n c) (conj acc (take n c)))))))

(defcheck solution-551e4580
  (fn p [x s]
    (let [[f r] (split-at x s)]
      (if (= x (count f))
        (cons f (p x r))))))

(defcheck solution-5581b9f8
  (fn partition-seq [n coll]
    (if (<= n (count coll))
      (cons
        (take n coll)
        (lazy-seq (partition-seq n (drop n coll)))))))

(defcheck solution-56666b31
  #(for [i (iterate (partial + %) 0) :while (<= (+ i % ) (count %2))] (->> %2 (drop i) (take %))))

(defcheck solution-56be36c
  (fn par [n s]
    (when-not (< (count s) n)
      (cons (take n s) (par n (drop n s)))
      )))

(defcheck solution-56efb46
  (fn [n coll]
    (->> coll
      (iterate #(drop n %))
      (map #(take n %))
      (take-while #(= n (count %))))))

(defcheck solution-5735f59a
  (fn partytion [n coll]
    (if (< (count coll) n) nil
                           (concat (list (take n coll)) (partytion n (drop n coll))))
    ))

(defcheck solution-574ddfc1
  (fn f [k xs]
    (if (< (count xs) k) ()
                         (cons (take k xs) (lazy-seq (f k (drop k xs)))))))

(defcheck solution-5765e433
  (fn f [x s]
    (if (<= x (count s))
      (cons (take x s) (f x (drop x s))))))

(defcheck solution-57cd6751
  (fn part-seq [n seq]
    (if (or (empty? seq)
            (< (count seq) n))
      '()
      (cons (take n seq) (part-seq n (drop n seq))))))

(defcheck solution-5832049
  (fn part [step coll]
    (let [bite (take step coll)
          left (drop step coll)]
      (if (= step (count bite))
        (cons bite (part step left))))))

(defcheck solution-584196e9
  (fn part [n coll]
    (let [size (count coll)]
      (cond (< size n) '()
            (= (count coll) n) (list coll)
            :default (concat (list (drop-last (- size n) coll)) (part n (drop n coll)))))))

(defcheck solution-58585510
  (fn _part [n s]
    (when (>= (count s) n)
      (cons (take n s) (_part n (drop n s))))))

(defcheck solution-5865f701
  (fn [n coll]
    (filter #(= n (count %)) (partition-by #(quot % n) coll))))

(defcheck solution-58974a2b
  #_(arrgh, cgrand, making my head explode... wonderful...)

  (fn [n s]
    (filter
      #(= n (count %))
      (loop [acc []
             item (first s)
             rst (rest s)
             i 0]
        (let [acc (if (= 0 (mod i n))
                    (conj acc [item])
                    (conj (vec (drop-last acc)) (conj (last acc) item)))]
          (if (not (seq rst))
            acc
            (recur acc (first rst) (rest rst) (inc i))))))))

(defcheck solution-58a5df
  (fn p [n l]
    (if (>= (count l) n)
      (apply list (take n l) (p n (drop n l))))))

(defcheck solution-590223a7
  (fn [n coll]
    (loop [coll coll held []]
      (if (<= (count coll) (dec n)) (seq held)
                                    (recur (drop n coll) (conj  held (take n coll)))))))

(defcheck solution-5910bdef
  (fn [n coll] (->> (iterate (partial drop n) coll)
                 (map (partial take n))
                 (take-while (comp (partial = n) count)))))

(defcheck solution-59343c80
  (fn p [n xs]
    (lazy-seq
      (when (seq xs)
        (let [nxs (take n xs)]
          (when (== n (count nxs))
            (cons nxs (p n (drop n xs)))))))))

(defcheck solution-59e80aa1
  (fn f [i c] (if (< (count c) i)
                '()
                (let [[a b] (split-at i c)]
                  (cons a (f i b))))))

(defcheck solution-5a69ff0a
  (fn [n xs]
    (loop [acc []
           ys  xs]
      (if (or (empty? ys) (< (count ys) n))
        acc
        (recur (into acc [(take n ys)]) (drop n ys))))))

(defcheck solution-5a9a6101
  (fn iter
    ([n in] (iter n in []))
    ([n in out]
     (if (> n (count in))
       out
       (recur n
         (drop n in)
         (conj out (take n in)))))))

(defcheck solution-5acc49a2
  (fn f [n xs]
    (let [rs (take n xs)]
      (if (= (count rs) n)
        (cons rs (f n (drop n xs)))
        ()))))

(defcheck solution-5adf28d2
  (fn part [p s] (if (< (count s) p) '() (concat (list (take p s)) (part p (drop p s))))))

(defcheck solution-5b425ca4
  (fn mypart [n seqq]
    (when
     (and (seq seqq) (>= (count seqq) n ))
      (cons (take n seqq) (mypart n (drop n seqq)))
      )
    ))

(defcheck solution-5b6e3309
  (fn [n s] (filter #(<= n (count %)) (vals (group-by #(quot % n) s)))))

(defcheck solution-5b6e71ae
  (fn part[n s](let [tail (drop n s)]
                 (if (>= (count tail) n)
                   (cons (take n s)
                     (lazy-seq (part n tail)))
                   (list (take n s)))
                 )
    ))

(defcheck solution-5bf5286d
  (fn [n s]
    (loop [se s, ret []]
      (if (>= (count se) n)
        (recur (drop n se) (conj ret (take n se)))
        ret))))

(defcheck solution-5c19d4b1
  (fn part [n xs]
    (letfn [(f [[result n' acc] x]
              (if (= (inc n') n)
                [(conj result (conj acc x)) 0 []]
                [result (inc n') (conj acc x)]))]
      (first (reduce f [[] 0 []] xs)))))

(defcheck solution-5c39eb71
  (fn p [n s]
    (for [i (range (int (/ (count s) n)))]
      (filter #(= i (int (/ (.indexOf s %) n))) s))))

(defcheck solution-5c3a1198
  (fn [s g]
    (reverse (loop [rem g v '()]
               (if (< (count rem) s)
                 v
                 (recur (drop s rem) (conj v (take s rem)) ))))))

(defcheck solution-5c670253
  (fn part [n seq]
    (if (< (count seq) n)
      nil
      (cons (take n seq)
        (part n (drop n seq))))))

(defcheck solution-5c7db4af
  (fn part
    ([n col] (part n col ()))
    ([n col cols]
     (let [sub (take n col)]
       (if (< (count sub) n)
         (reverse cols)
         (part n (drop n col) (conj cols sub)))))))

(defcheck solution-5ca4f854
  (fn [n s]
    (loop [remaining s ans []]
      (if (< (count remaining) n)
        ans
        (if (>= (count remaining) n)
          (recur (drop n remaining) (conj ans (take n remaining))))))))

(defcheck solution-5ce7033e
  (fn [s xs] (filter #(= (count %) s) (map (partial take s) (map #(drop (* s %) xs)(range (count xs)))))))

(defcheck solution-5ce7efeb
  (fn [p-size xs]
    (letfn [(indices [p-size size] (take-while #(<= (+ % p-size) size) (iterate (partial + p-size) 0)))
            (part [p-size xs index] (take p-size (drop index xs)))]
      (map part (repeat p-size) (repeat xs) (indices p-size (count xs))) )))

(defcheck solution-5d4417d9
  (fn z [d coll]
    (->> (loop [coll coll res []]
           (if (empty? coll)
             res
             (recur (drop d coll) (conj res (take d coll)))))
      (filter #(= d (count %))))))

(defcheck solution-5db36711
  (fn [x v] (filter #(= x (count %)) (vals (group-by #(int (/ (.indexOf v %) x)) v)))))

(defcheck solution-5dd8b35
  #(loop [i %2 o ()]
     (if (>= (count i) %1)
       (recur (drop %1 i) (conj o (take %1 i)))
       (reverse o))))

(defcheck solution-5de663b5
  (fn ppart [n l]
    (if (< (count l) n)
      '()
      (cons ((split-at n l) 0)
        (ppart n ((split-at n l) 1))))))

(defcheck solution-5e42fd5d
  #(loop [l []
          o %2]
     (if (< (count (take %1 o)) %1)
       l
       (recur (conj l (take %1 o)) (drop %1 o)))))

(defcheck solution-5eb377ae
  (fn foo [n xs]
    (when (>= (count xs) n)
      (cons (take n xs) (foo n (drop n xs))))))

(defcheck solution-5edca656
  (fn [n s] (for [x (range (int (/ (count s) n)))] (take n (nthnext s (* x n))))))

(defcheck solution-5f86ed17
  (fn f
    [n d]
    (when (>= (count d) n)
      (cons (take n d) (f n (drop n d))))))

(defcheck solution-5fd4a458
  (fn part [n items]
    (let [result (take n items)]
      (when (= n (count result))
        (lazy-seq (cons result (part n (drop n items))))))))

(defcheck solution-604934c5
  (fn [l coll]
    (loop [r [] s (seq coll)]
      (if (< (count s) l)
        r
        (recur (conj r (take l s)) (drop l s))))))

(defcheck solution-604e6752
  (fn partition' [n coll]
    (lazy-seq
      (let [[h t] (split-at n coll)]
        (when (= n (count h))
          (cons h (partition' n t)))))))

(defcheck solution-60832cb2
  (fn [n s]
    (->>
      (group-by #(mod % n) s )
      (vals)
      (apply map list))))

(defcheck solution-60ab0204
  (fn my-partition [n coll]
    (when (>= (count coll) n)
      (cons (take n coll) (my-partition n (drop n coll))))))

(defcheck solution-60b03016
  (fn mypart [step xs]
    (let [n (quot (count xs) step)]
      (loop [i n, head [], tail xs]
        (if (zero? i)
          head
          (recur (dec i) (conj head (take step tail)) (drop step tail)))))))

(defcheck solution-60eb7482
  (fn f [n s]
    (if (>= (count s) n)
      (cons (take n s)
        (f n (drop n s))))))

(defcheck solution-60ee46f9
  (fn my-partition [n coll]
    (if (<= n (count coll))
      (cons (take n coll)
        (my-partition n (drop n coll))))))

(defcheck solution-615f1327
  (fn [n coll]
    (loop [n n, coll coll, res []]
      (if (> n (count (take n coll))) res
                                      (recur n (drop n coll) (conj res (take n coll)))))))

(defcheck solution-6185815e
  (fn pairs [num collect]
    (loop [col collect,n 0,result []]
      (if (empty? col)
        (filter #(= (reduce (fn[n,_](inc n) ) 0 %) num)  result)

        (let [
              idx (quot n num),
              newCol (conj (get result idx []) (first col) )
              ]
          (recur (rest col)
            (inc n)
            (assoc result idx newCol)
            )
          )
        )
      )
    ))

(defcheck solution-61955cee
  (fn [x ys] (letfn[(partition2 [x xs ys](let [first (take x ys) second (drop x ys)] (if (not= x (count first)) (reverse  xs) (partition2 x (conj xs first) second))))](partition2 x '() ys))))

(defcheck solution-61d7d22e
  (fn
    [n seq]
    (loop [lseq (rest seq) newseq (vector (list (first seq)))]
      (if (empty? lseq)
        (if (= (count (last newseq)) n)
          (into () (reverse newseq))
          (into () (reverse (butlast newseq))))
        (if (= (count (last newseq)) n)
          (recur (rest lseq) (conj newseq (list (first lseq)) ))
          (recur (rest lseq) (assoc newseq (- (count newseq) 1) (concat (last newseq) (vector (first lseq)) ))))
        ))))

(defcheck solution-61e77d13
  (fn [n x]
    (loop [c x
           r []]
      (if (< (count c) n)
        r
        (recur (drop n c) (conj r (take n c)))))))

(defcheck solution-626a3527
  (fn [n coll]
    (loop [res [] remaining coll]
      (if (> n (count remaining))
        res
        (let [[part tail] (split-at n remaining)]
          (recur (conj res part) tail))))))

(defcheck solution-62a45b5f
  (fn [x y] (for [nb (range (int (/ (count y) x)))] (take x (drop (* nb x) y)))))

(defcheck solution-62aca3ca
  #(loop [l %2, r [], part []]
     (if (empty? l)
       r
       (let [f (first l), n (next l), p (conj part f)]
         (if (= (count p) %)
           (recur n (conj r p) [])
           (recur n r p))))))

(defcheck solution-62bf76b8
  (fn part [n coll]
    (loop [coll coll,
           result []]
      (if (empty? coll)
        result
        (if (< (count coll) n)
          (recur (drop n coll) result)
          (recur (drop n coll) (conj result (take n coll))))))))

(defcheck solution-62ca1f82
  (fn part
    [n s]
    (when (<= n (count s))
      (lazy-seq (cons (take n s)
                  (part n (drop n s)))))))

(defcheck solution-631c3c4c
  (fn [len coll]
    (loop [c coll
           acc []]
      (if (< (count c) len)
        (apply list acc)
        (recur (drop len c) (conj acc (take len c)))
        )
      )
    ))

(defcheck solution-631e1356
  (fn f [c s]
    (if (< (count s) c)
      ()
      (cons (take c s) (f c (drop c s))))))

(defcheck solution-63a2f88f
  (fn partition'
    ([k xs] (partition' k xs []))
    ([k xs acc]
     (let [[left right] (split-at k xs)]
       (if (< (count left) k)
         acc
         (recur k right (conj acc left)))))))

(defcheck solution-63c0f74c
  #(loop [l %2 a []]
     (if (< (count l) %) a
                         (recur (drop % l) (conj a (take % l))))))

(defcheck solution-64aa7cc2
  (fn my-partition [n coll]
    (letfn [(part [ret n col2]
              (if (< (count col2) n)
                (reverse ret)
                (recur (cons (take n col2) ret) n (drop n col2))))]
      (part nil n coll))))

(defcheck solution-65373f95
  (fn [r n v ] (let [[e t] (split-at n v)]
                 #_(println r n v)
                 (if (< (count e) n) (reverse r) (recur (conj r e) n t ) ))) ())

(defcheck solution-6570c461
  (fn [n xs]
    (loop [xs xs acc [] accsub [] i 1]
      (if (seq xs)
        (if (< i n)
          (recur (rest xs) acc (conj accsub (first xs)) (inc i))
          (recur (rest xs) (conj acc (conj accsub (first xs))) [] 1))
        acc))))

(defcheck solution-659a68d0
  (fn f [n s]
    (if (< (count s) n)
      '()
      (cons (take n s) (f n (drop n s))))))

(defcheck solution-65b8d2a
  #(loop [xs %2  res []]
     (if (>= (count xs) % )
       (recur (drop % xs) (conj res (take % xs)))
       res)))

(defcheck solution-65f4b807
  (fn [n coll]
    (loop [s coll
           acc []]
      (if (< (count s) n)
        acc
        (recur (drop n s) (conj acc (take n s)))))))

(defcheck solution-6619432b
  (fn my-partition [num s]
    (let [[x y] (split-at num s)]
      (if (<= num (count x))
        (cons x (my-partition num y))))))

(defcheck solution-662bfd67
  (fn my-partition [n coll]
    (cond (empty? coll)  '()
          (< (count coll) n) '()
          :else (concat (list (take n coll)) (my-partition n (drop n coll))))))

(defcheck solution-66ab6069
  (fn my-partition [n s]
    (cond
      (> (count s) n) (concat [(take n s)] (my-partition n (drop n s)))
      (= (count s) n) [s])))

(defcheck solution-671d9e29
  (fn [n z] (letfn [(part [l] (lazy-seq (if-let [r (seq l)]  (when (>= (count r) n) (cons (take n r) (part (drop n r)))))))]
              (part z))))

(defcheck solution-678cd158
  (fn [n s]
    (loop [s' s r []]
      (if
       (< (count s') n)
        r
        (recur (drop n s') (conj r (take n s')))))))

(defcheck solution-6896fe7b
  (fn p [n s]
    (if (< (count s) n)
      '()
      (conj (p n (drop n s))  (take n s)))))

(defcheck solution-68f0e34f
  (fn [n coll] (->> coll
                 (iterate #(drop n %))
                 (take-while #(>= (count %) n))
                 (map #(take n %)))))

(defcheck solution-69242bdd
  (fn [n coll]
    ((fn part [n coll accum]
       (if (> n (count coll))
         nil
         (concat (conj accum (seq (take n coll)))
                 (part n (drop n coll) accum))))
     n coll [])))

(defcheck solution-69361ab9
  (fn [n coll]
    (loop [c coll
           r []]
      (if (> n (count c))
        r
        (recur (drop n c) (conj r (take n c)))))))

(defcheck solution-69fc9ce0
  (fn aa [x y] (loop [xx y yy []] (if (>= (count xx) x)
                                    (recur (drop x xx) (conj yy (take x xx)))
                                    yy
                                    )
                                  )
    ))

(defcheck solution-6a03cd7
  (fn [num [& col]]
    ; use vec to convert lazyseq to persisten vector
    (for [start (take (quot (count col) num) (range 0 (count col) num))] (subvec (vec col)  start (+ start num)))
    ))

(defcheck solution-6a5f1fc
  (fn [n xs]
    (let [rests (iterate (partial drop n) xs)
          takes (map (partial take n) rests)]
      (take-while #(= n (count %1)) takes))))

(defcheck solution-6adef25d
  (fn [n xs] (letfn [
                     (ex? [ss] (empty? (drop (dec n) ss)))
                     (ptn [ys]
                       (if (ex? ys) '()
                                    (lazy-seq
                                      (cons
                                        (take n ys)
                                        (ptn (drop n ys)))) ))]
               (ptn xs))))

(defcheck solution-6b714654
  (fn my-partition [n s] (if (< (count s) n)
                           ()
                           (cons (take n s) (my-partition n (drop n s))))))

(defcheck solution-6b7dcef1
  (fn [size args]
    (first
      (reduce
        (fn [[output curr] nxt]
          (let [newCurr (conj curr nxt)]
            (if (= size (count newCurr))
              [(conj output newCurr) []]
              [output newCurr])))
        [[] []] args))))

(defcheck solution-6b95d77b
  (fn [n s]
    (loop [xs s acc []]
      (if (< (count xs) n)
        acc
        (recur (drop n xs) (conj acc (take n xs)))))))

(defcheck solution-6beb4d2b
  (fn my-partition [n s]
    (let [part (take n s)]
      (if (= (count part) n)
        (lazy-seq (cons part (my-partition n (drop n s))))
        nil))))

(defcheck solution-6c09bc8e
  (fn part [size items]
    (if (< (count items) size)
      '()
      (cons (take size items) (part size (drop size items)))
      )
    ))

(defcheck solution-6c2b1b63
  (fn [amt lst]
    (loop [rm lst, acc (vector)]
      (cond (or (empty? rm)
                (< (count rm) amt)) acc
            :else (recur (drop amt rm) (conj acc (take amt rm)))))))

(defcheck solution-6c614e20
  (fn mypart [n v] (loop [v v parts '()] (if (>= (count v) n) (recur (drop n v) (conj parts (take n v))) (reverse parts)))))

(defcheck solution-6d0cbc74
  (fn ptn [n s]
    (loop [s s acc []]
      (if (< (count s) n)
        acc
        (recur (drop n s) (conj acc (take n s)))
        ))
    ))

(defcheck solution-6d2d8954
  (fn p [n xs]
    (let [g (take n xs)]
      (if (= (count g) n)
        (cons g (p n (drop n xs)))
        ()))))

(defcheck solution-6d715132
  (fn foo [n xs] (if (<= n (count xs)) (cons (take n xs) (foo n (drop n xs))))))

(defcheck solution-6d822429
  (fn p [n c]
    (when (>= (count c) n) (cons (take n c) (p n (drop n c))))))

(defcheck solution-6d888e3e
  (fn [w xs]
    (take-while #(= w (count %))
      ((fn lazypart [ys]
         (lazy-cat [(take w ys)] (lazypart (drop w ys))))
       xs))))

(defcheck solution-6dbc1e52
  (fn my-partition [n a-seq]
    (if (seq? a-seq)

      (let [len (count a-seq)]
        (cond (> len n) (cons (take n a-seq) (my-partition n (drop n a-seq)))
              (= len n) (list (take n a-seq))
              :else nil)
        )
      nil
      )
    ))

(defcheck solution-6dffaf18
  (fn part [n lst]
    (filter #(= n (count %)) (cond
                               (empty? lst) '()
                               :else (cons (take n lst) (part n (drop n lst)))
                               ))
    ))

(defcheck solution-6e1d3072
  (fn [n s]
    (loop [s (seq s)
           r []]
      (let [front (take n s)]
        (if (= n (count front))
          (recur (drop n s) (conj r front))
          r)))))

(defcheck solution-6e29c927
  (fn part [n xs]
    (if (>= (count xs) n)
      (concat [(take n xs)] (part n (drop n xs))))))

(defcheck solution-6e32e3fd
  #(loop [r []
          c %2]
     (let [t (take % c)]
       (if (< (count t) %)
         r
         (recur (conj r t) (drop % c))))))

(defcheck solution-6e96addb
  (fn [n coll] (map #(take n (drop (- % n) coll)) (range n (inc (count coll)) n))))

(defcheck solution-6edff50b
  (fn part [n coll]
    (filter #(= n (count %)) (vals (group-by #(quot % n) coll)))))

(defcheck solution-6ee6ff52
  #(map (fn [x] (take %1 (drop x %2))) (range 0 (- (count %2) %1 -1) %1 )))

(defcheck solution-6ee74304
  (fn [n coll]
    (loop [coll coll
           answer []]
      (if (or (nil? coll) (< (count coll) n)) answer
                                              (recur (drop n coll) (conj answer (take n coll)))))))

(defcheck solution-6ef10ce4
  (fn ps [n coll] (loop [li [] res coll]
                    (if (< (count res) n)
                      li
                      (recur (conj li (take n res)) (drop n res))))))

(defcheck solution-6f3fc159
  (fn my-partition [n xs]
    (lazy-seq
      (when (>= (count xs) n)
        (cons (take n xs) (my-partition n (drop n xs)))))))

(defcheck solution-6f546d54
  (fn f
    ([c s] (f c s '()))
    ([c s r] (if (empty? s) (reverse r)
                            (recur c (drop c s) (if (= (count (take c s)) c) (conj r (take c s)) r))))))

(defcheck solution-6f64bf63
  (fn partition-a-sequence [n c]
    (let [r (reduce

              (fn [[a,p] i]
                (if (= (count p) n)
                  [(conj a p),(conj [] i)]
                  [ a, (conj p i)]))


              [[],[]]
              c)]

      (if (= 0 (rem (count c) n))
        (conj (first r) (last r))
        (first r)))))

(defcheck solution-6f98b443
  #(loop [arr %2 res []]
     (if (< (count arr) %) res
                           (recur (drop % arr) (conj res (take % arr))))))

(defcheck solution-6f9b6c1e
  (fn [n coll]
    (loop [result []
           coll coll]
      (let [[head tail] (split-at n coll)]
        (if (< (count coll) n)
          result
          (recur (conj result head) tail))))))

(defcheck solution-70332f7d
  (fn [step coll] (letfn [(func [step coll]
                            (if (>= (count coll) step)
                              (cons (take step coll) (func step (drop step coll)))))]
                    (func step coll))))

(defcheck solution-70cedcb1
  (fn p [n l]
    (if (= (count (take n l)) n)
      (cons (take n l) (p n (drop n l)))
      ())))

(defcheck solution-7119edc7
  (fn f54 [n l]
    (if (< (count l) n) []
                        (concat [(take n l)] (f54 n (drop n l)))
                        )))

(defcheck solution-711a6b79
  (fn part
    [n lst]
    (let [splter
          (fn [n lst z]
            (concat (drop-last lst) (split-at n (last lst)))
            )]
      (filter #(= (count %) n)
        (reduce #(splter n %1 %2) [lst] (range (- (/ (count lst) n) 1))))
      )
    ))

(defcheck solution-712708be
  (fn party [n v]
    (take-while #(= (count %) n)
      (cons (take n v)
        (lazy-seq (party n (nthrest v n)))
        ))
    ))

(defcheck solution-7132021e
  (fn ptn [n s]
    (let [nxt (take n s)]
      (when (= n (count nxt))
        (cons nxt(lazy-seq (ptn n (drop n s))))))))

(defcheck solution-7155faa4
  (fn p [n l] (if (<= n (count l)) (cons (take n l) (p n (drop n l))) [])))

(defcheck solution-71ddf635
  (fn me
    [cnt myseq]

    (let [ mypartition
          (fn mytest [cnt myseq]

            (if (empty? myseq)
              nil
              (concat (list (take cnt myseq))
                      (mytest cnt (drop cnt myseq))
                      )
              )
            )
          ]

      ( filter #(= cnt (count %))  (mypartition cnt myseq)  )
      )

    ))

(defcheck solution-7251c2
  (fn part [n xs]
    (cons
      (take n xs)
      (let [the-others (drop n xs)]
        (if (<= n (count the-others))
          (part n the-others)
          ())))))

(defcheck solution-729abb4e
  (fn parts
    ([n x] (parts n x []))
    ([n x p]
     (let [[h t] (split-at n x)]
       (if (= (count h) n)
         (recur n t (conj p h))
         p)))))

(defcheck solution-72a809fc
  (fn as[n s](when (>= (count s) n) (cons (take n s) (as n (drop n s))))))

(defcheck solution-72cbfbc4
  (fn [n xs] (let [vxs (vec xs) len (count xs) s (quot len n)] (for [i (range 0 (* s n) n)] (subvec vxs i (+ i n) ) ) ) ))

(defcheck solution-731d0526
  (fn part [n x] (if (< (count x) n) nil
                                     (cons (take n x) (part n (drop n x))))))

(defcheck solution-73291bac
  (fn f [len xs]
    (if (> len (count xs))
      '()
      (let [[h t] (split-at len xs)]
        (cons h (f len t))))))

(defcheck solution-73f50500
  (fn p [n x]
    (if (>= (count x) n)
      (cons (take n x) (p n (drop n x))))))

(defcheck solution-73f908ba
  (fn part [n coll]
    (take (int (/ (count coll) n)) (vals (group-by #(->(.indexOf coll %) (/ n) int) coll)))))

(defcheck solution-741c9aab
  #(for [x (iterate (partial drop %) %2)
         :while (>= (count x) %)]
     (take % x)))

(defcheck solution-745eeb85
  (fn p [n s]
    (if (>= (count s) n)
      (cons (take n s) (p n (drop n s)))
      '())))

(defcheck solution-74821f3c
  (fn partition2 [n coll]
    (if (<= n (count coll))
      (cons (take n coll) (partition2 n (drop n coll))))))

(defcheck solution-74d9ea3
  (fn [n xs]
    (map
      (fn [i]
        (take n
          (drop
            (* i n)
            xs
            )
          )
        )
      (range
        (quot
          (count xs)
          n
          )
        )
      )
    ))

(defcheck solution-74e70eb8
  #(loop [c %2 r []]
     (let [s (take % c)]
       (if (= % (count s))
         (recur (drop % c) (conj r s))
         r))))

(defcheck solution-754bba6d
  (fn [n s]
    (map #(map (fn [x] (nth s (+ x %))) (range n))
      (map #(* n %) (range (int (/ (count s) n)))))))

(defcheck solution-759695a
  (fn part [n lst]
    (if (< (count lst) n) '()
                          (cons
                            (take n lst)
                            (part n (drop n lst))))))

(defcheck solution-76101ec8
  (fn [a col]
    (->> (map-indexed (fn [i v] [(quot i a) v]) col)
      (partition-by first)
      (map #(map second %))
      (filter #(= a (count %))))))

(defcheck solution-76c6d6b4
  (fn [n a-seq]
    (map #(take n (drop (* n %1) a-seq)) (range (quot (count a-seq) n)))))

(defcheck solution-76ee5b29
  (fn _ [p s]
    (let [[f r] (split-at p s)]
      (if (< (count f) p)
        []
        (cons f (_ p r))
        )
      )
    ))

(defcheck solution-77270717
  (fn part [n s]
    (if (< (count s) n) nil
                        (cons (take n s) (part n (drop n s))))))

(defcheck solution-773c5e5c
  (fn [x c]
    (filter #(= x (count %))
      (map #(take x (drop (* x %) c))
        (range 0 (inc (/ (count c) x)))))))

(defcheck solution-779211aa
  #(loop [r [] c %2]
     (if (< (count c) %)
       r
       (recur (conj r (take % c))
         (drop % c)))))

(defcheck solution-77adb2d7
  (fn [n coll]
    (loop [coll coll
           curr '()
           acc '()
           i 1]
      (if (empty? coll)
        acc
        (let [curr (concat curr (list (first coll)))]
          (if (= i n)
            (recur (rest coll) '() (concat acc (list curr)) 1)
            (recur (rest coll) curr acc (+ i 1))
            )
          )
        )
      )
    ))

(defcheck solution-77d4c7ef
  (fn prttn [n coll]
    (let [s (take n coll)]
      (if (= n (count s))
        (cons s (prttn n (drop n coll)))
        )
      )
    ))

(defcheck solution-77ec5b0d
  (fn myPartition[y l]
    (filter
      #(= (count %) y)
      ((fn bf[y ll]
         (reductions
           (fn [l x]
             (if
              (< (count l) y)
               (conj l x)
               (vector x)))
           []
           ll)) y l))))

(defcheck solution-77f3315d
  (fn partition-seq
    [n s]
    (when (seq s) (when (>= (count s) n)
                    (cons (take n s)
                      (partition-seq n (nthrest s n)))))))

(defcheck solution-7833a30b
  #(let [f (juxt take drop)]
     (loop [[a b] (f % %2) acc []]
       (if (< (count a) %)
         acc
         (recur (f % b) (conj acc a))
         ))))

(defcheck solution-78cdde7
  #(loop [s %2 a []]
     (if (< (count s) %1) a
                          (recur (drop %1 s) (conj a (take %1 s))))))

(defcheck solution-78d38814
  (fn [n s]
    (filter #(= n(count %)) (map #(map second %) (partition-by first (map-indexed #(vector (quot %1 n) %2) s))))
    ))

(defcheck solution-798812e2
  (fn f [n l]
    (if (< (count l) n)
      '()
      (cons (take n l) (f n (drop n l))))))

(defcheck solution-79a3df9e
  (fn partition2 [n s]
    (if
     (> n (count s)) nil
                     (cons (take n s) (lazy-seq (partition2 n (drop n s)))))
    ))

(defcheck solution-79d22be5
  (fn f [n c]
    (if (< (count c) n)
      []
      (cons (take n c) (f n (drop n c))))))

(defcheck solution-79fdeaa1
  (fn f [n s]
    (if (<= n (count s))
      (cons (take n s) (f n (nthnext s n))))))

(defcheck solution-7a0f01f6
  (fn
    [x xs]
    (->>
      (reduce
        #(
           if (= (count (last %1)) x)
           (conj %1 [%2])
           (conj (pop %1) (conj (peek %1) %2))
           )
        [[(first xs)]] (rest xs))
      (filter #(= x (count %1)))
      )))

(defcheck solution-7a468aaf
  (fn partition2 [n aseq]
    (loop [res [] aseq aseq]
      (if (< (count aseq) n)
        res
        (recur (conj res (take n aseq))
          (drop n aseq))
        ))))

(defcheck solution-7a48d0cc
  (fn my-partition [n s]
    (let [group (take n s)]
      (when (>= (count group) n)
        (cons group (my-partition n (drop n s)))))))

(defcheck solution-7a5a4cbe
  #(loop [v [] l %2] (if (nth l (dec %) false)
                       (recur (conj v (take % l)) (drop % l)) v)))

(defcheck solution-7ac0312b
  (fn p [n l]
    (let [x (take n l)]
      (when (= n (count x))
        (cons x (p n (drop n l)))))))

(defcheck solution-7b130c41
  (fn xpartition [n seq]
    (loop [rem seq, res [], part []]
      ;(println rem "," res "," part ",")
      (if (empty? rem)
        ; if
        (if (= (count part) n)
          (conj res part)
          res)
        ; else
        (if (= (count part) n)
          (recur (rest rem) (conj res part) [(first rem)])
          (recur (rest rem) res (conj part (first rem))))
        ))))

(defcheck solution-7b8f0f63
  (fn f [n colls]
    (loop [result ()
           suffix colls]
      (if (and (seq? suffix) (>= (count suffix) n))
        (recur (concat result (list (take n suffix))) (seq (drop n suffix)))
        result))))

(defcheck solution-7c2b206
  (fn [n xs]
    (loop [xs xs
           acc []]
      (let [n-xs (take n xs)]
        (if (< (count n-xs) n)
          acc
          (recur (drop n xs) (conj acc n-xs)))))))

(defcheck solution-7c2d349f
  (fn [num_p coll]
    (map #(take num_p (drop (* % num_p) coll))
      (range (quot (count coll) num_p)))))

(defcheck solution-7c3b2515
  #((fn f [r c]
      (if (< (count c) %) r
                          (f (conj r (take % c)) (drop % c)))) [] %2))

(defcheck solution-7c557012
  (fn [n coll]
    (loop [c coll v []]
      (if (< (count c) n) v
                          (recur (drop n c) (conj v (take n c)))))))

(defcheck solution-7cb4506d
  (fn [n coll]
    (loop [s coll c []]
      (if (< (count s) n)
        c
        (let [g (split-at n s)
              f (first g)
              l (last g)]
          (recur l (conj c f)))))))

(defcheck solution-7cfda75f
  (fn part [n xs] (if (< (count xs) n) '() (cons (take n xs) (part n (drop n xs))))))

(defcheck solution-7d274ea4
  (fn [len sq]
    (seq (first (reduce #(let [[full-acc acc] %
                               next-acc (conj acc %2)]
                           (if (= len (count next-acc))
                             [(conj full-acc (seq next-acc)) []]
                             [full-acc next-acc]))
                  [[] []]
                  sq)))))

(defcheck solution-7d2cb4d8
  (fn f [n c] (when (>= (count (take n c)) n) (cons (take n c) (lazy-seq (f n (drop n c)))))))

(defcheck solution-7df5ae0f
  (fn ps [x coll]
    (loop [rest-coll (nthrest coll x)
           ret (list (take x coll))]
      (let [y (take x rest-coll)]
        (if (= (count y) x)
          (recur (nthrest rest-coll x) (cons y ret))
          (reverse ret))))))

(defcheck solution-7df96e03
  (fn __ [part-size col]
    (filter #(= part-size (count %))
      (partition-by #(int (/ % part-size)) col))))

(defcheck solution-7e2421c9
  (fn part [n ls]
    (if (< (count ls) n)
      '()
      (cons (take n ls)
        (part n (drop n ls))))))

(defcheck solution-7f05fcab
  (fn inter [n coll]
    (lazy-seq
      (cond
        (empty? coll) nil
        (not= n (count (take n coll))) nil
        :default (cons (take n coll)
                   (inter n (drop n coll)))))))

(defcheck solution-7f0af565
  (fn
    [n coll]
    (loop [res []
           coll coll]
      (let [l (take n coll)]
        (if (= n (count l))
          (recur (conj res l) (drop n coll))
          res)))))

(defcheck solution-7f132864
  (fn [n s]
    (map  #(take n (nthrest s (* % n)))
      (range (quot (count s) n)))))

(defcheck solution-7f1beb5b
  (fn partition1 [n coll]
    (if (> n (count coll)) '()
                           (let [[a b] (split-at n coll)]
                             (cons a (partition1 n b))))))

(defcheck solution-7f4642ed
  (fn my-partition [n coll]
    (if (< (count coll) n)
      nil
      (cons (take n coll) (my-partition n (drop n coll))))))

(defcheck solution-7f756dca
  (fn [n, l]
    (loop [c l, v []]
      (if (or (empty? c) (< (count c) n))
        v
        (recur (drop n c) (conj v (take n c)))))))

(defcheck solution-7f7b0cd1
  (fn partition2 [n coll]
    (if (<= n (count coll))
      (cons (take n coll) (partition2 n (drop n coll)))
      nil)))

(defcheck solution-7fbf7be8
  (fn [r s]
    (apply (partial map (fn [& a] (vec a))) (map #(take-nth r (drop % s)) (range r)))))

(defcheck solution-7fc37dc6
  (fn [n coll] (loop [c coll partitioned []] (if (< (count c) n) partitioned
                                                                 (recur (drop n c) (conj partitioned (take n c)))))))

(defcheck solution-7fc815d1
  (fn part [n x]
    (if (< (count x) n)
      '()
      (cons (take n x) (part n (drop n x)))
      )
    ))

(defcheck solution-801e2bc5
  (fn [x coll]
    (let [go (fn [all-parts part inputs]
               (if-let [n (first inputs)]
                 (let [next-part (conj part n)]
                   (if (= x (count next-part))
                     (recur (conj all-parts next-part) [] (rest inputs))
                     (recur all-parts, (conj part n) (rest inputs))))
                 all-parts))]
      (go [] [] coll))))

(defcheck solution-8024d564
  (fn [n xs]
    (loop [s xs ret []]
      (if (> (count s) (- n 1))
        (recur (drop n s) (conj ret (vec (take n s))))
        ret))))

(defcheck solution-8035f82c
  (fn [n v] (loop [v v
                   acc []]
              (if (> n (count v))
                acc
                (recur (drop n v)
                  (conj acc  (take n v)))))))

(defcheck solution-80a1ef53
  (fn [n coll] (loop [s coll r []] (if (< (count s) n) r (recur (drop n s) (conj r (take n s)))))))

(defcheck solution-80a5347a
  (fn partition-disguised
    [parted xs]
    (let [some-part (take parted xs)]
      (if (< (count some-part) parted) ;; -> replace this with empty? -> include the rem items
        (list)
        (cons some-part (partition-disguised parted (drop (count some-part) xs)))))))

(defcheck solution-81805b2e
  (fn [n coll]
    (loop [coll, coll, partitions []]
      (if (< (count coll) n)
        partitions
        (recur (drop n coll) (conj partitions (take n coll)))))))

(defcheck solution-81845b53
  (fn [n coll]
    (->> (range 0 (- (count coll) n -1) n)
      (map #(->> coll
              (drop %)
              (take n)))
      )))

(defcheck solution-8192d1e7
  (fn part [n coll]
    (let [ abort? (> n (count coll))
          segment (take n coll)]
      (if abort?
        nil
        (cons segment (lazy-seq (part n (drop n coll))))))))

(defcheck solution-82030106
  (fn my-partition [n l]
    (reverse (loop [l l f []]
               (if (empty? l)
                 f
                 (let [split (split-at n l)
                       part (if (= n (count (first split)))
                              (cons (first split) f)
                              f)]
                   (recur (first (rest split)) part)))))))

(defcheck solution-8303fcc7
  (fn part [n s] (let [nxt (take n s) rst (drop n s)]
                   (if (= n (count nxt)) (cons nxt (part n rst)) '())
                   )))

(defcheck solution-831eff29
  (fn my-partition [x coll]
    (if (>= (count coll) x)
      (cons (take x coll) (my-partition x (drop x coll))))))

(defcheck solution-836c3f72
  (fn [n coll]
    (partition-by #(quot % n)
      (take (- (count coll) (mod (count coll) n)) coll))))

(defcheck solution-83b4b983
  (fn part [n s]
    (loop [s s res []]
      (if (not (seq s)) (if (not= n (count (last res)))
                          (pop res)
                          res)
                        (recur (drop n s) (conj res (take n s)))))))

(defcheck solution-83c8f222
  (fn  [n col]
    (filter #(= (count %) n)  (map #(map second %) (vals (group-by (fn [col] (quot (first col) n)) (map-indexed (fn [idx ele] [idx ele]) col)))))))

(defcheck solution-847618d0
  #(case % 2 '((0 1) (2 3) (4 5) (6 7)) 3 (if (= 9 (count %2))  '((0 1 2) (3 4 5) (6 7 8))  '((0 1 2) (3 4 5)))))

(defcheck solution-847fc514
  (fn f [n coll]
    (if (<= n (count coll))
      (cons (take n coll) (f n (drop n coll)))
      ())))

(defcheck solution-84c2566a
  (fn [n xs]
    (for [nn (range 0 (- (inc (count xs)) n) n)] (take n (drop nn xs)))))

(defcheck solution-851b6449
  (fn [n x]
    (loop [s [] r x]
      (if (< (count r) n)
        s
        (recur (conj s (take n r)) (drop n r))))))

(defcheck solution-85396b18
  (fn part [n coll] (let [[a b] (split-at n coll)] (if (>= (count coll) n) (cons a (part n b))))))

(defcheck solution-858b6a9e
  (fn f [n coll]
    (let [[xs ys] (split-at n coll)]
      (if (< (count xs) n)
        ()
        (cons xs (f n ys))))))

(defcheck solution-85b72b2d
  (fn my-partition [n coll]
    (let [x (take n coll)]
      (when (= (count x) n)
        (cons x (my-partition n (drop n coll)))))))

(defcheck solution-85f20c45
  (fn [n s]
    (loop [m 0
           ret '()]
      (if (> (+ m n) (count s))
        (reverse ret)
        (recur (+ n m) (conj ret (take n (drop m s))))))))

(defcheck solution-8614c9b5
  (fn
    [n vs]
    (let [c (quot (count vs) n)
          vs (vec vs)]
      (map #(subvec vs (* n %) (* n (inc %)))
        (range c)))))

(defcheck solution-8639fd6c
  (fn p [n c]
    (if (>= (count c) n)
      (cons (take n c) (p n (drop n c))))))

(defcheck solution-86957e12
  (fn [n v] (->> v
              (zipmap (range))
              (into [])
              (sort)
              (partition-by #(odd? (quot (first %) n)))
              (filter #(= (count %) n))
              (map #(map second %))
              )))

(defcheck solution-8716268b
  trampoline (fn p [r n s] (if (< (count s) n) r #(p (conj r (take n s)) n (drop n s)))) [])

(defcheck solution-87381ccc
  (fn [n coll]
    (take-while #(== n (count %)) (map (partial take n) (iterate (partial drop n) coll)))))

(defcheck solution-873efb25
  (letfn [(part
            [n xs]
            (lazy-seq
              (let [[x xs] (split-at n xs)]
                (cons x (part n xs)))))]

    (fn [n xs]
      (take-while #(= n (count %))
        (part n xs)))))

(defcheck solution-88121eb9
  (fn [n xs]
    (loop [xs xs
           ret '()]
      (if (< (count xs) n)
        ret
        (recur (drop n xs)
          (concat ret [(take n xs)]))))))

(defcheck solution-895ca43c
  #(loop [[f & r] %2 l '() res '()]
     (if (nil? f) res
                  (let [new-l (concat l (list f))]
                    (if (= (count new-l) %1)
                      (recur r '() (concat res (list new-l)))
                      (recur r new-l res))))))

(defcheck solution-89e0d5bc
  (fn [n col]
    (->> (iterate (comp (partial split-at n) second) [[] col])
      (drop 1)
      (map first)
      (take-while #(= n (count %))))))

(defcheck solution-8a0841d5
  (fn my_partition [n l]
    (when (>= (count l) n)
      (conj (my_partition n (drop n l)) (take n l)))))

(defcheck solution-8a28d9fc
  (fn [n xs] (loop [head [] tail xs] (if (< (count tail) n) head (recur (conj head (take n tail)) (drop n tail))))))

(defcheck solution-8a550c47
  (fn cust-part [x coll]
    (loop [n x seq-list (list (take x coll)) coll (drop x coll)]
      (if (< (count coll) n)
        seq-list
        (recur
          n
          (concat seq-list (list (take n coll)))
          (drop n coll))))))

(defcheck solution-8a6d0188
  (fn chop [i v]
    (if (or (empty? v) (< (count v) i))
      nil
      (lazy-seq (cons (take i v) (chop i (drop i v)))))))

(defcheck solution-8a9b8b1e
  (fn prt([n _seq]
          (prt n _seq '()))
    ([n _seq result]
     (if (< (count _seq) n)
       (reverse result)
       (recur n (drop n _seq) (conj result (take n _seq)))))))

(defcheck solution-8acde28
  (fn [x l]
    (loop [next-x (take x l) dropped-x (drop x l) r '()]
      (if (< (count dropped-x) x)
        (concat r (list next-x))
        (recur (take x dropped-x) (drop x dropped-x) (concat r (list next-x)))
        ))))

(defcheck solution-8b4c998f
  (fn party
    ([c x] (party [] x c))
    ([x y c]
     (if (or (empty? y) (< (count y) c))
       x
       (party (conj x (take c y)) (drop c y) c)))))

(defcheck solution-8b8f9545
  (fn my-partition [n coll]
    (let [f (take n coll)]
      (if (= n (count f))
        (cons f (lazy-seq (my-partition n (drop n coll))))
        '()))))

(defcheck solution-8b9c0e6a
  (fn [n v]
    (let [c (count v) limit (- c (mod c n))]
      (for [step (range 0 limit n)]
        (for [offset (range n)]
          (nth v (+ offset step)))))))

(defcheck solution-8bf920da
  #(first
     (reduce (fn [[acc sub] el]
               (let [bumped (conj sub el)]
                 (if (= (count bumped) %)
                   [(conj acc bumped) []]
                   [acc bumped])))
       [[] []]
       %2)))

(defcheck solution-8bfb158b
  (fn my-partition [size coll]
    (lazy-seq
      (if (>= (count coll) size)
        (cons (take size coll)
          (my-partition size
            (drop size coll)))))))

(defcheck solution-8c35e42e
  (fn p [n s]
    (let [[h t] ((juxt (partial take n) (partial drop n)) s)]
      (lazy-seq (cons h (if-not (< (count t) n) (p n t)))))))

(defcheck solution-8c4ef6dd
  (fn p054
    ([p lst] (p054 [] p lst))
    ([acc p lst] (if (< (count lst) p) acc
                                       (p054 (conj acc (take p lst)) p (drop p lst))))))

(defcheck solution-8c5f2e8
  (fn part[size coll]
    (loop [collcopy coll partitioned []]
      (if (< (count collcopy) size)
        partitioned
        (recur (drop size collcopy) (conj partitioned (take size collcopy)))))))

(defcheck solution-8c85c9e0
  (fn f [n l]
    (when-let [s (seq l)]
      (let [[a b] (split-at n l)]
        (when (= (count a) n)
          (cons a (f n b)))))))

(defcheck solution-8d106193
  (fn [n coll]
    (for [x (range (int (/ (count coll) n)))]
      (take n (drop (* n x) coll)))))

(defcheck solution-8d83722d
  #(remove (comp (partial > %1) count)
     (map (partial map second)
       (partition-by first
         (map-indexed (fn [i x] [(quot i %1) x]) %2)))))

(defcheck solution-8df462dd
  (fn [n,l]
    ((fn [n,l,r]
       (if
        (< (count l) n) r
                        (recur n (drop n l) (concat r (list (take n l))))
                        )
       ) n l '())
    ))

(defcheck solution-8e19ea4d
  (fn f [x lst]
    (when (<= x (count lst))
      (cons (take x lst) (f x (drop x lst))))))

(defcheck solution-8ea0337a
  (fn [stride coll]
    (take-while
      #(= (count %) stride)
      (next (map first
              (iterate
                (fn [[res inp]] [(take stride inp) (drop stride inp)])
                [[] coll]))))))

(defcheck solution-8eb45dcf
  #(loop [n %1 s %2 r []]
     (let [[c d] (split-at n s)]
       (if (= (count c) n)
         (recur n d (conj r c))
         r))))

(defcheck solution-8ed6fa4a
  (fn custom-partition
    [n coll]
    (filter
      #(= (count %) n)
      (reduce
        (fn
          [reduce-coll el]
          (let [last-partition (last reduce-coll)
                size-last-partition (count last-partition)]
            (cond
              (nil? last-partition) (conj reduce-coll [el])
              (< size-last-partition n) (conj (vec (drop-last reduce-coll)) (conj last-partition el))
              :else (conj reduce-coll [el]))))
        [] coll))))

(defcheck solution-8ef50b09
  (fn [x y] (apply map vector
              (map (partial
                     (fn [i p] (filter #(= (mod (.indexOf p %) i) 0) p)) x)
                (take x (iterate rest y))))))

(defcheck solution-8f2aac05
  (fn x [n l] (if (< (count l) n)
                ()
                (cons (take n l) (x n (drop n l))))))

(defcheck solution-8f3a8049
  (fn [number items]
    (loop [seq-items (rest items) part [(first items)] result []]
      (if (empty? seq-items)
        result
        (let [item (first seq-items) next-part (conj part item) next-size (count next-part)]
          (recur (rest seq-items)
            (if (= next-size number)
              []
              next-part)
            (if (= next-size number)
              (conj result next-part)
              result)))))))

(defcheck solution-8f61d9cc
  (fn my-partition [n coll]
    (letfn [(fetch-part [count part rst]
              (cond (= 0 count) [part rst]
                    (empty? rst) [nil nil]
                    :else (recur (dec count) (conj part (first rst)) (rest rst))))]
      (let [[next-part next-rest] (fetch-part n [] coll)]
        (if (nil? next-part)
          ()
          (lazy-seq (cons next-part
                      (my-partition n next-rest))))))))

(defcheck solution-8f784de1
  (fn my-part [n coll]
    (if (> n (count coll))
      '()
      (lazy-seq (cons (take n coll)
                  (my-part n (drop n coll)))))))

(defcheck solution-8f874367
  (fn ps [n s]
    (filter #(= (count %) n)
      (map second (group-by #(quot % n) s)))))

(defcheck solution-8f9012fb
  (fn parti [n l]
    (let [f (fn [acc n l]
              (if (empty? l ) (reverse acc)
                              (recur (conj acc (take n l)) n (drop n l))))]
      (filter #(= n (count %)) (f '() n l)))))

(defcheck solution-8fafa96b
  (fn [w xs]
    (loop [xs xs acc []]
      (if (> w (count xs))
        acc
        (recur (drop w xs) (conj acc (take w xs)))))))

(defcheck solution-90020468
  (fn party [n xs]
    (if (< (count xs) n)
      []
      (cons (take n xs) (party n (drop n xs))))))

(defcheck solution-9022f27
  (fn [n s]
    (filter #(= (count %) n)
      (map #(take n (nthnext s (* % n))) (range 0 (/ (count s) n))))))

(defcheck solution-906e6971
  #(loop [v %2 x [] o []]
     (if (empty? v)
       o
       (let [y (conj x (first v))]
         (if (= (count y) %1)
           (recur (rest v) [] (conj o y))
           (recur (rest v) y o)
           )
         )
       )
     ))

(defcheck solution-909285f1
  (fn [n s]
    (loop [r s
           result nil]
      (if (< (count r) n)
        (reverse result)
        (recur
          (drop n r)
          (cons (take n r) result))))))

(defcheck solution-90f56c14
  (fn p [c l]
    (let [[l ls] (split-at c l)](when (= (count l) c) (lazy-seq ( cons l (p c ls)))))))

(defcheck solution-919249d3
  #(first (reduce
            (fn [[a b] x]
              (let [b (conj b x)]
                (if (= % (count b))
                  [(conj a b) []]
                  [a b])))
            [[] []] %2)))

(defcheck solution-919619e2
  (fn [n c] (map #(->> c (drop (* n %)) (take n)) (range (long (/ (count c) n)))
              )))

(defcheck solution-920c8806
  (fn part [n coll]
    (let [[a b] ((juxt take drop) n coll)]
      (if (= (count a) n)
        (cons a (part n b))
        nil))))

(defcheck solution-9276761d
  (fn [n s]
    (loop[ n n s s r '() c '()]
      (cond
        (= n (count c))
        (recur n s (concat r [c]) '())
        (empty? s) r
        :else
        (recur n (next s) r (concat c [(first s)])
          )))))

(defcheck solution-92b41962
  (fn [n s]
    (let [f (fn [{current :current
                  total :total
                  :or {current [] total []}}
                 e]
              (let [new-current (conj current e)]
                (if (= (count new-current) n)
                  {:current []
                   :total (conj total new-current)}
                  {:current new-current
                   :total total})))]
      (:total (reduce f {} s)))))

(defcheck solution-92fa67f9
  #(loop [li %2,
          result []]
     (if (< (count li) %) result
                          (recur (drop %1 li) (conj result (take %1 li))))))

(defcheck solution-93116369
  #(take-nth % (apply map list (take % (iterate next %2)))))

(defcheck solution-9313b657
  #(if (< (count %3) %2) % (recur (conj % (take %2 %3)) %2 (drop %2 %3))) [])

(defcheck solution-9340c63a
  (fn [num coll]
    (filter
      #(= num (count %))
      (vals (group-by #(int (/ % num)) coll))
      )))

(defcheck solution-934a171d
  (fn [x coll]
    (loop [result [] coll coll]
      (if (< (count coll) x) result
                             (recur (conj result (take x coll)) (drop x coll))))))

(defcheck solution-934b31b0
  (fn pt [n xs]
    (lazy-seq
      (when-let [part (take n xs)]
        (when (= (count part) n)
          (cons part (pt n (drop n xs))))))))

(defcheck solution-934b75ab
  (fn splt [x y]

    (if (< (count y) (* x 2))

      [(first(split-at x y))]

      (concat [(first(split-at x y))] (splt x (second(split-at x y)))))))

(defcheck solution-93645f69
  (fn [n l]
    (reverse (loop [a (quot (count l) n)
                    rslt []
                    b l]
               (if (= 0 a)
                 rslt
                 (recur (dec a) (cons (take n b) rslt) (drop n b)))))))

(defcheck solution-93751975
  (fn [n ls]
    (loop [src ls dst []]
      (let [group (take n src)]
        (if (= n (count group))
          (recur (drop n src) (conj dst group))
          dst)))))

(defcheck solution-93a4c51
  (fn [n coll]
    (first
      (reduce (fn [[c1 c2] x]
                (let [cc (conj c2 x)]
                  (if (= n (count cc))
                    [(conj c1 cc) []]
                    [c1 cc])))
        [[] []]
        coll))
    ))

(defcheck solution-93fb06b1
  #(loop [r %2
          acc []]
     (if (<= % (count r))
       (recur (drop % r)
         (conj acc (take % r)))
       acc)))

(defcheck solution-94363e32
  (fn my-partition [x coll]
    (when-let [s (seq coll)]
      (let [chunk (take x coll)
            rst   (drop x coll)]
        (when (= (count chunk) x)
          (cons chunk (my-partition x rst)))))))

(defcheck solution-9437327
  (fn part [n xs]
    (when (>= (count xs) n)
      (cons (take n xs)
        (part n (drop n xs))))))

(defcheck solution-9455f3a7
  (fn [n r]
    (filter #(= (count %) n)
      (map (fn [a] (flatten (map second a)))
        (vals
          (group-by (fn [[a b]] (quot a n)) (map-indexed vector r))
          )
        )
      )
    ))

(defcheck solution-945cd8cb
  (fn my-partition [p s]
    (loop [acc []
           [part rest] (split-at p s)]
      (if (< (count part) p)
        acc
        (recur (conj acc part) (split-at p rest))))))

(defcheck solution-9463dc56
  (fn [m r]
    (loop [col [] remaining r]
      (if (>= (count remaining) m)
        (recur (conj col (take m remaining)) (drop m remaining))
        col))))

(defcheck solution-949bcfe1
  (fn [cnt s]
    (loop [s s
           accum []]
      (let [current-partition (take cnt s)
            remaining-seq (drop cnt s)]
        (if (< (count current-partition) cnt)
          accum
          (recur remaining-seq (conj accum current-partition)))))))

(defcheck solution-9509505b
  #(loop [n %1 c %2 r []] (if (< (count c) n) r (recur n (drop n c) (conj r (take n c))))))

(defcheck solution-950beb47
  (fn partition4j
    [n xs]
    (letfn [(partitionall [n xs]
              (if (or (empty? xs) (nil? xs))
                xs
                (cons (take n xs) (partitionall n (nthrest xs n)))))]
      (filter #(= (count %) n) (partitionall n xs)))))

(defcheck solution-95104397
  (fn f [init n x]
    (if (> n (count x))
      init
      (f (conj init (subvec (vec x) 0 n))  n (drop n x)))) [])

(defcheck solution-951e2382
  #(loop [xs %2 acc []]
     (if (< (count xs) %) acc
                          (recur (drop % xs) (conj acc (take % xs))))))

(defcheck solution-95686799
  (fn [x s] (for [i (take (quot (count s) x) (iterate (partial + x) 0))] (filter #(and (<= i %) (> (+ i x) %)) s))))

(defcheck solution-95bede74
  (fn [step n]
    (filter #(= (count %) step)
      ((fn func [ns]
         (if (<= (count ns) step)
           (list ns)
           (cons (take step ns) (func (drop step ns)))))
       n))))

(defcheck solution-9615ec5e
  (fn f [c s]
    (lazy-seq
      (let [nxt (take c s)]
        (when (and (seq s) (= (count nxt) c))
          (cons (take c s) (f c (drop c s))))))))

(defcheck solution-96224568
  (fn [n xs]
    (let [m (* (int (/ (count xs) n)) n)
          xs (reverse (take m xs))]
      (reduce (fn [ls x]
                (if (= (count (first ls)) n)
                  (conj ls (list x))
                  (conj (rest ls) (conj (first ls) x))))
        (list ())
        xs))))

(defcheck solution-9673fab9
  (fn [n coll]
    (loop [c coll
           r []]
      (if (< (count (take n c)) n)
        r
        (recur (drop n c) (conj r (take n c)))))))

(defcheck solution-9674c253
  (fn [n s]
    (loop [s s
           acc []]
      (cond
        (< (count s) n) acc
        :else (recur (drop n s) (conj acc (take n s)))))))

(defcheck solution-96c47d8
  (fn [n coll]
    (loop [acc [], c coll]
      (if (and (seq c) (>= (count c) n))
        (recur (conj acc (take n c))
          (drop n c))
        acc))))

(defcheck solution-96dc332a
  (fn [n src]
    (reverse(loop [x src y '()]
              (if (empty? x) y
                             (let [spl (split-at n x) p1 (first spl) p2 (last spl)]
                               (cond
                                 (> n (count p2)) (conj y p1)
                                 (= n (count p2)) (conj y p1 p2)
                                 :else (recur p2 (conj y p1))
                                 )
                               )
                             )
              ))
    ))

(defcheck solution-96e5489b
  (fn [n l] ((fn [n l build] (if (> n (count l)) build (recur n (drop n l) (conj build (take n l))))) n l [])))

(defcheck solution-96f57aff
  (fn part
    ([n coll buff]
     (if (< (count coll) n) buff
                            (part n (drop n coll) (concat buff [(take n coll)]))))
    ([n coll] (part n coll []))))

(defcheck solution-973d125d
  (fn [n s]
    (loop [remainder s result []]
      (if (< (count remainder) n)
        result
        (recur (drop n remainder) (conj result (take n remainder)))))))

(defcheck solution-97d610e0
  (fn part [n coll]
    (filter #(= n (count %))
      (reductions (fn [xs s]
                    (if (= (count xs) n) [s] (conj xs s))) [(first coll)] (rest coll)))))

(defcheck solution-981aff0d
  (fn f [n coll]
    (let [section (take n coll)]
      (when (= (count section) n)
        (cons section (f n (drop n coll)))))))

(defcheck solution-9833f832
  (fn [n col]
    (let [len (count col)
          group-count (int (/ len n))
          ]
      (for [i (range group-count)]
        (for [j (range n)]
          (nth col (+ j (* i n))))))))

(defcheck solution-98892524
  (fn [n c]
    (loop [r [] s c]
      (if (< (count s) n)
        r
        (recur (conj r (take n s)) (drop n s))))))

(defcheck solution-98d4e0a7
  (fn [n coll]
    (loop [coll coll
           p []
           r []]
      (if (empty? coll)
        r
        (let [p (conj p (first coll))]
          (if (= (count p) n)
            (recur (rest coll) [] (conj r p))
            (recur (rest coll) p r)))))))

(defcheck solution-98fd9f32
  (fn [n xs]
    (loop [xs xs
           c  []]
      (if (>= (count xs) n)
        (recur (drop n xs) (concat c [(take n xs)]))
        c))))

(defcheck solution-99039b50
  (fn partition-sequence [x colls]
    (loop[result '()
          colls colls]
      (if(< (count colls) x)
        result
        (recur (concat result (list (first (split-at x colls))))
          (second (split-at x colls)))))))

(defcheck solution-9919f2ae
  (fn my-part [n s]
    (filter #(= n (count %))
      (reduce (fn [v e] (update-in v [(first e)] #(concat % (second e)))) []
        (map-indexed (fn [i x] [(quot i n) [x]]) s)))))

(defcheck solution-9964d125
  #(loop [s %1 l %2 r []]
     (if (> s (count l)) r
                         (recur s (drop s l) (conj r (take s l))))))

(defcheck solution-99c559bb
  (fn part
    [n coll]
    (if (< (count coll) n)
      '()
      (cons (take n coll) (part n (drop n coll))))
    ))

(defcheck solution-99d0990b
  (fn prtn [s c]
    (if (< (count c) s)
      '()
      (apply cons ((fn [split] [(first split)
                                (prtn s (last split))])
                   (split-at s c))))))

(defcheck solution-99dafd3a
  (fn [n coll]
    (filter #(>= (count %1) n)
      (loop [i n
             coll coll
             r '()]
        (if (empty? coll)
          (reverse (conj (rest r) (reverse (first r))))
          (if (= i 0)
            (recur n coll (conj (rest r) (reverse (first r)) '()))
            (recur (- i 1) (rest coll) (conj (rest r) (conj (first r) (first coll))))))))))

(defcheck solution-99fb6ea1
  (fn partition-seq
    ([x y]
     (partition-seq x (rest y) (vector (first y)) (vector)))
    ([x y z w]
     (if (= 0 (count y))
       (if (= x (count z))
         (list* (conj w (list* z)))
         (list* w))
       (if (= x (count z))
         (recur x (rest y) (vector (first y)) (conj w (list* z)))
         (recur x (rest y) (conj z (first y)) w))))))

(defcheck solution-9a1598a3
  (fn partition2 [n coll]
    (let [mod-coll (drop-last (rem (count coll) n) coll)]
      (loop [c mod-coll acc '()]
        (if (empty? c) (reverse acc)
                       (recur (drop n c) (cons (take n c) acc)))))))

(defcheck solution-9a635373
  (fn partition-seq [n s]
    (if (>= (count s) n)
      (cons (take n s) (partition-seq n (drop n s))))))

(defcheck solution-9a7d552d
  (fn part [n s]
    (lazy-seq
      (let [xs (split-at n s)
            f (first xs)]
        (if (= n (count f))
          (cons f (part n (second xs))))))))

(defcheck solution-9abb9dbe
  #(for [i (range (quot (count %2) %))]
     (for [j (range %)]
       (nth %2 (+ (* i %) j)))))

(defcheck solution-9ae6a3d7
  (fn ! [n coll]
    (if (< (count coll) n)
      '()
      (conj (! n (drop n coll))
        (take n coll)))))

(defcheck solution-9b402bdc
  #(letfn [(f [n x acc cur]
             (if (empty? x)
               acc
               (if (= n (inc (count cur)))
                 (f n (rest x) (conj acc (conj cur (first x))) [])
                 (f n (rest x) acc (conj cur (first x))))))]
     (f %1 %2 [] [])))

(defcheck solution-9b590588
  (fn [n lst]
    (loop [item (first lst) lst2 (rest lst)
           ret '() ret2 '() c 0 ]
      (if (empty? lst2)
        (if (= (inc c) n)
          (reverse (conj ret (reverse (conj ret2 item))))
          (reverse ret))
        (if (< c n)
          (recur (first lst2) (rest lst2)
            ret (conj ret2 item) (inc c))
          (recur item lst2
            (conj ret (reverse ret2)) '() 0))))))

(defcheck solution-9c627949
  #(loop [s %2 r []]
     (let [[a b] (split-at % s)
           l (conj r a)]
       (if (<= % (count b)) (recur b l) l))))

(defcheck solution-9c77cbc3
  (fn [n coll]
    (for [p (range (quot (count coll) n))]
      (take n (drop (* p n) coll)))))

(defcheck solution-9c86ac8d
  #(loop [in %2 ret []] (if (> % (count in)) ret (recur (drop % in) (conj ret (take % in))))))

(defcheck solution-9c9c0dc8
  (fn [n col]
    (loop [l col acc []]
      (if (< (count l) n) acc
                          (recur (drop n l) (conj acc (take n l)))))))

(defcheck solution-9d21beb0
  (fn part [n coll]
    (lazy-seq
      (when (= n (count (take n coll)))
        (cons (take n coll) (part n (drop n coll)))))))

(defcheck solution-9d337f7e
  (fn part [n, xs]
    (loop [out []
           in  xs]
      (if (< (count in) n)
        out
        (recur (conj out (take n in)) (drop n in))))))

(defcheck solution-9d520d61
  (fn [n s]
    (map #(take n (drop % s))
      (range 0
        (* (quot (count s) n) n)
        n))))

(defcheck solution-9d6ab144
  (fn [n coll]
    (loop [coll coll
           acc  []]
      (if (< (count coll) n)
        acc
        (recur (drop n coll) (conj acc (take n coll)))))))

(defcheck solution-9df0e965
  #(apply map list
     (for [a (range %)] (take-nth % (drop a %2)))))

(defcheck solution-9e066f47
  (fn f [n c] (if (< (count c) n) '() (cons (take n c) (f n (drop n c))))))

(defcheck solution-9e32cf
  (fn [index, coll]
    (loop [coll coll
           cnt (count coll)
           result []]
      (if (< cnt index)
        result
        (let [new-coll (split-at index coll)
              next-coll (flatten (next new-coll))]
          (recur next-coll
            (count next-coll)
            (conj result (first new-coll))))))))

(defcheck solution-9e3312a1
  #(loop [n % coll %2 ret []]
     (if (>= (count coll) n) (recur n (drop n coll) (conj ret (take n coll))) ret)))

(defcheck solution-9e49110b
  (fn part [n coll]
    (let [a (drop n coll)
          b (take n coll)]
      (if
       (< (count a) n)
        (list (take n coll))
        (conj (part n a) b)))))

(defcheck solution-9e56567a
  (fn
    [n coll]
    (letfn [(r [coll] (lazy-seq
                        (if (< (count coll) n)
                          '()
                          (let [[a b] (split-at n coll)]
                            (cons a (r b))))))]
      (r coll))))

(defcheck solution-9ed6d1c3
  (fn f [x l]
    (if (< (count l) x)
      ()
      (concat (list (take x l)) (f x (drop x l))))))

(defcheck solution-9f260639
  (fn f [n xs]
    (if (or (> n (count xs)) (zero? n))
      ()
      (cons (take n xs) (f n (drop n xs))))))

(defcheck solution-9f9eea66
  (fn f [n s]
    (let [p (take n s)]
      (if (= n (count p))
        (cons p (f n (drop n s)))))))

(defcheck solution-9fb6723f
  #(loop [c %2 result []]
     (if (> % (count c))
       result
       (recur (drop % c) (conj result (take % c))))))

(defcheck solution-9fc049c1
  #(loop [c %2, r []]
     (if (< (count c) %1)
       r
       (recur (drop %1 c) (conj r (take %1 c))))))

(defcheck solution-9ff7e8f8
  (fn p [n sq]
    (if (> n (count sq))
      []
      (concat (list (take n sq)) (p n (drop n sq))))))

(defcheck solution-a0322296
  (fn my-partition [n coll]
    (if (>= (count coll) n)
      (lazy-seq
        (cons (take n coll)
          (my-partition n (nthnext coll n)))))))

(defcheck solution-a070a868
  (fn part [n s]
    (when (>= (count s) n)
      (cons (take n s) (part n (drop n s))))))

(defcheck solution-a097c4da
  (fn x
    ([n l] (x [] n l))
    ([r n l]
     (if (< (count l) n)
       r
       (recur (conj r (take n l)) n (drop n l))))))

(defcheck solution-a0b20bfe
  (fn [n s]
    (loop [t s r []]
      (if (< (count t) n) r
                          (let [sp (split-at n t)]
                            (recur (second sp) (conj r (first sp)))
                            )
                          )
      )
    ))

(defcheck solution-a11509b
  (fn part [n coll] (if (< (count coll) n) [] (cons (take n coll) (part n (drop n coll))))))

(defcheck solution-a16136a1
  (fn partition-e
    [n x]
    (if (< (count x) n) '()
                        (conj (partition-e n (drop n x)) (take n x)))))

(defcheck solution-a1711c71
  (fn
    [n xs]
    (let [m (quot (count xs) n)]
      (for [i (range m)]
        (take n (drop (* i n) xs))
        )

      )

    ))

(defcheck solution-a19fd205
  (fn [n s]
    (first
      (reduce
        (fn [[r s] i]
          (let [x (conj s i)]
            (if (= n (count x)) [(conj r x) []] [r x])))
        [[][]]
        s))))

(defcheck solution-a1eef54a
  (fn [n s]
    (loop [ans [] x s]
      (if (> n (count x))
        ans
        (recur (conj ans (take n x)) (drop n x))))))

(defcheck solution-a253a184
  (fn [n coll]
    (loop [acc []
           elts coll]
      (let [newy (take n elts)]
        (if (> n (count newy))
          acc
          (recur (conj acc newy) (drop n elts)))))))

(defcheck solution-a2686109
  #(loop [r [] s %2]
     (if (< (count s) %) r
                         (recur (conj r (take % s)) (nthrest s %)))))

(defcheck solution-a282882d
  (fn [n xs]
    (letfn [(iter [k xs acc]
              (cond
                (zero? k)
                (cons acc (iter n xs []))
                (empty? xs)
                '()
                true
                (iter (dec k) (rest xs) (conj acc (first xs)))))]
      (iter n xs []))))

(defcheck solution-a2872396
  (fn my-partition [n xs]
    (loop [result []
           x-rest xs]
      (if (< (count x-rest) n)
        result
        (recur (conj result (take n x-rest)) (drop n x-rest))))))

(defcheck solution-a2f06e12
  (fn my-partition [n s]
    (if (< (count s) n)
      []
      (cons (take n s) (my-partition n (loop [cntr n s0 s] (if (zero? cntr) s0 (recur (dec cntr) (rest s0)))))))))

(defcheck solution-a322e667
  (fn p [n s]
    (let
     [b (take n s)]
      (if
       (< (count b) n) '()
                       (cons
                         b (p n (drop n s)))))))

(defcheck solution-a34d88d8
  (fn mypart [n c]
    (loop [c c acc []]
      (if (< (count c) n)
        acc
        (recur (drop n c) (conj acc (take n c)))))))

(defcheck solution-a37dd185
  (fn p [n xs]
    (if (> n (count xs))
      '()
      (cons (take n xs) (p n (drop n xs))))))

(defcheck solution-a40a60bd
  (fn [n s]
    (remove
      #(< (count %) n)
      (map
        #(map last %)
        (partition-by
          (fn [[idx _]] (quot idx n))
          (map-indexed vector s))))))

(defcheck solution-a421e2fd
  (fn [x myCol] (let [recursor (fn recurs [out in] (if (> x (count in)) out (recurs (concat out (list (take x in))) (drop x in))))] (recursor '() myCol))))

(defcheck solution-a454609
  (fn bu [n l] (let [[h, t] (split-at n l)] (if (> n (count l)) nil (cons h (bu n t))))))

(defcheck solution-a469c36
  (fn pp[ n xs ]
    (cond (= n (count xs)) (list xs)
          (> n (count xs)) nil
          :else (let [ [x xs'] (split-at n xs) ]
                  (cons x (pp n xs'))))))

(defcheck solution-a4c987e9
  (fn partition- [x coll]
    "54. Write a function which returns a sequence of lists of x items each."
    (when-let [s (seq coll)]
      (if  (>= (count s) x)
        (cons (take x s) (partition- x (drop x s)))))))

(defcheck solution-a4f63b80
  (fn my-partition [k s]
    (let [[part remainder] (split-at k s)]
      (if (= k (count part))
        (cons part (my-partition k remainder))
        nil))))

(defcheck solution-a5308dfb
  (fn part [n coll]
    (when (>= (count coll) n)
      (cons (take n coll) (part n (drop n coll))))))

(defcheck solution-a534fdaf
  (fn [n a-seq]
    (let [part-by-idx (->> a-seq
                        (map-indexed #(vector (quot %1 n) %2))
                        (group-by #(first %))
                        (vals))]
      (filter #(= n (count %))
        (for [x part-by-idx]
          (for [y x]
            (second y)))))))

(defcheck solution-a57bcaff
  #(loop [p [] r %2]
     (if (< (count r) %1) p
                          (recur (conj p (take %1 r)) (drop %1 r)))))

(defcheck solution-a591ecdf
  #(loop [crest %2 rez []]
     (if (< (count crest) %1)
       rez
       (recur (nthrest crest %1) (conj rez (take %1 crest))))))

(defcheck solution-a5a715ea
  (fn p [acc n col]
    (if (< (count col) n)
      acc
      (p (conj acc (take n col)) n (drop n col)))) [])

(defcheck solution-a5d7de09
  (fn [n co]
    (let [
          nu (quot (count co) n)
          indexesCount (* nu n)
          covec (vec co)
          res (vals (group-by (fn [x] (quot x n)) (range indexesCount)))
          ]
      #_(println res)
      (map (fn [yy] (map (fn [i] (nth covec i)) yy)) res))))

(defcheck solution-a60fc04a
  #(loop [col %2
          acc []]
     (if (< (count col) %) acc
                           (recur (drop % col) (conj acc (take % col))))))

(defcheck solution-a61bbabe
  (fn [n coll]
    (for [i (range 0 (int (/ (count coll) n))) ]
      (for [j (range (* i n) (* (inc i) n) ) ]
        j
        )
      )
    ))

(defcheck solution-a686e3f8
  (fn p [n c]
    (if (< (count c) n)
      '()
      (cons (take n c)
        (p n (drop n c))))))

(defcheck solution-a6ea9b67
  #(let [n (count %2)]
     (filter (fn [r] (= %1 (count r)))
       (for [i (range (quot n %1))]
         (->> %2
           (drop (* i %1))
           (take %1))))))

(defcheck solution-a6f3b6d7
  (fn [n c]
    (let [col (vec c)]
      (for [i (range (quot (count col) n))]
        (for [j (range n)] (col (+ (* i n) j)))))))

(defcheck solution-a711408c
  (fn [n coll]
    (loop [s n k coll r []]
      (cond
        (empty? k) r
        (= 1 s) (recur n (drop n k) (conj r (take n k)))
        (= 0 (rem (count k) n)) (recur (dec s) (drop n k) (conj r (take n k)))
        :else (recur s (drop-last k) r)))))

(defcheck solution-a72f608b
  #(loop [xs %2 coll []]
     (if
      (< (count xs) %1)
       coll
       (recur (drop %1 xs) (conj coll (take %1 xs))))))

(defcheck solution-a7cd440e
  (fn p [n L]
    (if (< (count L) n)
      '()
      (cons (take n L)
        (p n (drop n L))))))

(defcheck solution-a850ab99
  #(loop [in %2, out []]
     (if (< (count in) %1)
       out
       (recur
         (drop %1 in)
         (conj out (take %1 in))))))

(defcheck solution-a86236ae
  (fn [n coll]
    (letfn [(f [n coll result]
              (if (< (count coll) n)
                (seq result)
                (recur n (drop n coll) (conj result (take n coll)))
                )
              )]
      (f n coll []))
    ))

(defcheck solution-a8c39b35
  (fn part [x c]
    (lazy-seq
      (let [[f r] (split-at x c)]
        (when (= x (count f))
          (cons f (part x r)))))))

(defcheck solution-a90505fb
  (fn f [n s] (when (>= (count s) n) (cons (take n s) (f n (drop n s))))))

(defcheck solution-a93b75f3
  (fn f [n col]
    (if (>= (count col) n)
      (cons (take n col) (f n (drop n col)))
      (list))))

(defcheck solution-a9d398fe
  ;; NOTE TO SELF: There were much better recursive solutions. Try again.

  (fn part [n xs]
    (let [num-elements (count xs)
          num-buckets (/ (count xs) n)
          buckets (vec (take num-buckets (repeat [])))
          positions (sort (take num-elements (cycle (range 0 num-buckets))))
          zipped (map vector positions xs)
          should-drop-last (not= 0 (mod num-elements n))
          filled-buckets (reduce
                           #(assoc %1 (first %2) (conj (nth %1 (first %2)) (second %2)))
                           buckets
                           zipped)]
      (if should-drop-last
        (drop-last filled-buckets)
        filled-buckets))))

(defcheck solution-aa2690ad
  (fn [n coll]
    (->> (iterate (fn [[part rst]] [(take n rst) (drop n rst)]) [() coll])
      rest
      (map first)
      (take-while #(-> % count (= n))))))

(defcheck solution-aa7a1d5b
  (fn my-partition
    ([x lis] (my-partition x lis 0 []))
    ([x lis coun resp]
     (if (= lis [])
       (if (= x (count (last resp))) resp (drop-last resp))
       (if (or (= coun x) (= resp []))
         (my-partition x (rest lis) 1 (conj resp [(first lis)]))
         (my-partition x (rest lis) (inc coun)
           (assoc resp (dec (count resp))
                       (conj (last resp) (first lis)))))))))

(defcheck solution-aa865be
  (fn ptn [n s]
    (if (< (count s) n)
      '()
      (cons (take	n s) (ptn n (drop n s))))))

(defcheck solution-aaecb2bd
  (fn part [c s]
    (seq
      (loop [res [] rem s]
        (let [[x nrm] (split-at c rem)]
          (if (< (count x) c)
            res
            (recur (conj res x) nrm)))))))

(defcheck solution-ab11d06d
  (fn [n s]
    (reduce (fn [acc v]
              (if (< (count (first acc)) n)
                (cons (cons v (first acc)) (rest acc))
                (cons (list v) acc)))
      (list (list))
      (reverse (drop-last (mod (count s) n) s)))))

(defcheck solution-ab9df0fa
  (fn part
    ([n coll]
     (part n coll '()))
    ([n coll accum]
     (if-not (< (count coll) n)
       (part n (drop n coll) (cons (take n coll) accum))
       (reverse accum)))))

(defcheck solution-abb879ca
  #(loop [n [] o %2]
     (if (< (count o) %)
       n
       (recur (conj n (take % o)) (drop % o)))))

(defcheck solution-abd43b84
  (fn [c s] (for [i (range (quot (count s) c))]
              (take c (drop (* i c) s))
              )))

(defcheck solution-ac6b221d
  (fn [ n coll ]
    (loop [coll coll result []]
      (if (< (count coll) n)
        result
        (recur (drop n coll) (conj result (take n coll)))))))

(defcheck solution-ac9af7b4
  (fn [lim coll](reverse(loop [lim lim
                               coll coll
                               res '()]
                          (if (< (count (take lim coll)) lim)
                            res
                            (recur  lim (drop lim coll) (conj res (take lim coll))))
                          ))))

(defcheck solution-aceba421
  (fn partitioning [n coll]
    (when-not (< (count coll) n)
      (cons (take n coll) (partitioning n (drop n coll))))))

(defcheck solution-acf98056
  (fn [n coll]
    (loop [r [] xs coll]
      (if (< (count xs) n)
        r
        (recur (conj r (take n xs)) (drop n xs))))))

(defcheck solution-ad5f9ed3
  (fn my-partition [n v]
    (if (>= (count v) n)
      (cons (take n v) (my-partition n (drop n v))))))

(defcheck solution-adf6e505
  (fn [n xs]
    (loop [xs xs
           acc []]
      (if (< (count xs) n)
        acc
        (recur (drop n xs) (conj acc (take n xs)))))))

(defcheck solution-ae02ac40
  (fn new-partition
    [part coll]
    (if (>= (count coll) part)
      (cons (take part coll) (new-partition part (drop part coll)))
      [])))

(defcheck solution-ae2d99ea
  #(loop [xs %2 res []]
     (if (< (count xs) %) res
                          (recur (drop % xs) (conj res (take % xs))))))

(defcheck solution-ae3b418e
  (fn f [n l]
    (let [c (take n l)]
      (lazy-seq
        (when (= (count c) n)
          (cons c (f n (drop n l))))))))

(defcheck solution-ae4d9f5c
  (fn part [n xs]
    (if (< (count xs) n)
      nil
      (cons (take n xs) (part n (drop n xs))))))

(defcheck solution-ae80e840
  (fn [n c]
    (map #(subvec (vec c) % (+ n %))
      (range 0 (* n (quot (count c) n)) n))))

(defcheck solution-aead6938
  (fn part [n coll]
    (let [first (take n coll)
          rest (drop n coll)]
      (if (< (count first) n)
        '()
        (cons first (lazy-seq (part n rest)))))))

(defcheck solution-aeaec8ee
  (fn f
    ([n coll]
     (trampoline f n coll identity))
    ([n coll cont]
     (let [next-chunk (take n coll)]
       (if (< (count next-chunk) n)
         #(cont [])
         #(f n
            (drop n coll)
            (fn [x] (cont (cons next-chunk x)))))))))

(defcheck solution-af5c0454
  (fn f [n xs] (let [splitted (split-at n xs)] (if (< (count (first splitted)) n) ()
                                                                                  (conj (f n (second splitted)) (first splitted))))))

(defcheck solution-af7f1a84
  (fn [n xs]
    (loop [gathered []
           remaining xs]
      (let [taken (take n remaining)
            dropped (drop n remaining)]
        (if (= (count taken) n)
          (recur (conj gathered taken) dropped)
          gathered)))))

(defcheck solution-afafdd5a
  (fn [n coll]
    (map #(take n (drop (* n %) coll))
      (range (quot (count coll) n)))))

(defcheck solution-afc4a883
  (fn partition2[n coll]
    (when-let [s (seq coll)]
      (let [p (take n s)]
        (when (= n (count p))
          (cons p (partition2 n (nthrest s n))))))))

(defcheck solution-b011a921
  (fn f [c s] (if (<= c (count s)) (cons (take c s) (f c (drop c s))))))

(defcheck solution-b089adc6
  (fn [n coll]
    (loop [c coll result []]
      (if (or (empty? c) (not= n (count (take n c))))
        result
        (recur (drop n c) (conj result (take n c)))))))

(defcheck solution-b12f8a72
  (fn [p col]
    (take-while #(= (count %) p)
      ((fn foo [n c]
         (cons (take n c)
           (lazy-seq (foo n (drop n c))))) p col))))

(defcheck solution-b183e7f0
  (fn [length coll]
    (map (fn [[start end]] (drop start (take end coll)))
      (map (fn [i] [(* i length) (* (inc i) length)])
        (range (quot (count coll) length))))))

(defcheck solution-b22c4d9d
  (fn ptn[n r]
    (if (<= n (count r))
      (lazy-seq (cons (take n r) (ptn n (drop n r)))))))

(defcheck solution-b23a597d
  (fn f
    [n c]
    (when (>= (count c) n)
      (cons (take n c)
        (f n (drop n c))))))

(defcheck solution-b26cc5da
  (fn [n coll]
    (letfn [(next-n [coll]
              (lazy-seq
                (cons (take n coll)
                  (next-n (drop n coll)))))]
      (take-while #(= n (count %)) (next-n coll)))))

(defcheck solution-b274951c
  (fn [n lst]
    (letfn [(nthrest [lst n] ((apply comp (repeat n rest)) lst))]
      (take (int (/ (count lst) n)) (map #(take n %) (iterate #(nthrest % n) lst))))))

(defcheck solution-b2780d22
  (fn p [n s] (if (>= (count s) n) (cons (take n s) (p n (drop n s))) '())))

(defcheck solution-b2b2b129
  #(map (fn [x] (take % (drop (* x %) %2)))
     (range (quot (count %2) %))))

(defcheck solution-b34d90b0
  (fn [n c]
    (map
      #(map second (second %))
      (filter
        #(= (count (second %)) n)
        (group-by #(first %) (map #(vector (quot % n) %) c))))))

(defcheck solution-b42c0651
  (fn f [n x]
    (if (>= (count x) n)
      (cons (take n x) (f n (drop n x))))))

(defcheck solution-b4600de6
  (fn partition* [n coll]
    (when-let [s (seq coll)]
      (let [[head tail] (split-at n s)]
        (when (= n (count head))
          (cons head (lazy-seq (partition* n tail))))))))

(defcheck solution-b46d0379
  (fn f [n s] (if (< (count s) n) '() (cons (take n s) (f n (drop n s))))))

(defcheck solution-b47c131b
  (fn f [n seq]
    (if (< (count seq) n)
      nil
      (cons (take n seq)
        (f n (nthrest seq n))))))

(defcheck solution-b611a110
  (letfn
   [(mypartition [size coll]
      (let [v (vec coll)
            c (count v)]
        (for [i (range (quot c size))
              :let [offset (* i size)]]
          (subvec v offset (+ offset size))
          )))]
    #(mypartition %1 %2)
    ))

(defcheck solution-b6150a06
  (fn [n xs]
    (->> xs
      (reduce (fn [[out, pending] x]
                (let [pending (cons x pending)]
                  (if (= (count pending) n)
                    [(cons pending out) ()]
                    [out, pending])))
        [() ()])
      (first)
      (map reverse)
      reverse)))

(defcheck solution-b694b6ab
  (fn my-part [n xs]
    (let [[p r] (split-at n xs)]
      (if (< (count p) n) nil
                          (lazy-cat [p] (my-part n r))))))

(defcheck solution-b69c3344
  (fn part [x xs]
    (if (>= (count xs) x)
      (cons (take x xs) (lazy-seq (part x (drop x xs)))))))

(defcheck solution-b6b13ab3
  (fn [n c]
    (letfn [(tifl [co]
              (let [t (take n co)]
                (if (< (count t) n) [] t)))]
      (loop [res [] left c]
        (if (empty? left)
          (remove empty? res)
          (recur (conj res (tifl left)) (drop n left)))))))

(defcheck solution-b6b709df
  #(loop[result [], remaining %2]
     (if (>= (count remaining) %1)
       (recur (conj result (take %1 remaining)) (drop %1 remaining))
       result)))

(defcheck solution-b6cf26aa
  (fn [n coll]
    (loop [coll coll
           acc []]
      (let [ts (take n coll)
            ds (drop n coll)
            nacc (conj acc (take n coll))]
        (cond
          (empty? coll) acc
          (< (count ds) n) nacc
          :else (recur (drop n coll) nacc))))))

(defcheck solution-b6fce208
  (fn [n xs]
    (loop [xs xs zs []]
      (if (and (seq xs) (>= (count xs) n))
        (recur (drop n xs) (conj zs (take n xs)))
        zs))))

(defcheck solution-b79156f9
  (fn part [n x]
    (when (<= n (count x))
      (cons (take n x) (part n (drop n x)))
      )
    ))

(defcheck solution-b7bc1081
  (fn cutseq [length coll]
    (if (>= (count coll) length)
      (cons (take length coll) (cutseq length (drop length coll)))
      )
    ))

(defcheck solution-b7d4d416
  (letfn [(p [n xs]
            (if (and (seq xs)
                     (<= n (count xs)))
              (conj (p n (drop n xs)) (take n xs))
              '()))]
    p))

(defcheck solution-b8488e29
  (fn [x s]
    (let [num (- (count s) (mod (count s) x))]
      (for [idx (range (/ num x))]
        (take x (drop (* x idx) s))))))

(defcheck solution-b8d653ff
  (fn [n c]
    (filter #(= n (count %)) (vals (group-by #(quot % n) c)))))

(defcheck solution-b8f335fe
  (fn mypartition [n coll]
    (if (>= (count coll) n)
      (lazy-seq (cons (take n coll)
                  (mypartition n (drop n coll)))))))

(defcheck solution-b8fce6e5
  (fn part [n c]
    (if (< (count c ) n)
      nil
      (concat (list (take n c)) (part n (drop n c)))
      )))

(defcheck solution-b90504cf
  (fn [n xs]
    (->>
      (iterate #(+ n %) 0)          ;(0 3 6 9 etc.)
      (take (quot (count xs) n))    ;(0 3 6)
      (map #(take n (drop % xs))))))

(defcheck solution-b966b2ff
  (fn [n s] (take (quot (count s) n) (partition-by #(quot % n) s))))

(defcheck solution-b99414aa
  (fn ! [n c] (when (>= (count c) n) (cons (take n c) (lazy-seq (! n (drop n c)))))))

(defcheck solution-b9d8f03
  (fn newpartition [n x]
    "Cuts a sequence x into sublists of n elements each."
    (if (< (count x) n)
      '()
      (concat (list (take n x)) (newpartition n (drop n x))))))

(defcheck solution-ba33fe7e
  #(loop [a nil b %2]
     (if (>= (count b) %)
       (recur
         (concat a (list (take % b)))
         (drop % b))
       a)))

(defcheck solution-ba53dad8
  ;(fn [n coll]
  ;   (letfn [(group-coll-by [f coll]
  ;                         (let [vals (distinct (map f coll))]
  ;                          (reduce conj [] (map (fn [v] (filter #(= v (f %)) coll))  vals) )))]
  ;  (filter #(= (count %) n) (group-coll-by #(- % (mod % n)) coll))))

  (fn m-part
    [n coll]
    (lazy-seq
      (let [[t d] (split-at n coll)]
        (when (>= (count t) n)
          (cons t (m-part n d)))))))

(defcheck solution-ba711f5a
  (fn p [n c] (when (and (seq c) (>= (count c) n)) (cons (take n c) (p n (drop n c))))))

(defcheck solution-babbe889
  (fn f [n s] (let [[a b] (split-at n s)]
                (if (= n (count a)) (cons a (f n b))))))

(defcheck solution-baf3143b
  #(->> %2
     (iterate (partial drop %))
     (map (partial take %))
     (take-while (comp (partial = %) count))))

(defcheck solution-bb0ae5a2
  (fn[size s]
    (loop [remain s result []]
      (if (>= (count remain) size)
        (recur (drop size remain)
          (conj result (take size remain)))
        result))))

(defcheck solution-bb23ef97
  (fn [n coll]
    (loop [acc [] r-coll coll]
      (if (>= (count r-coll) n)
        (recur (conj acc (take n r-coll)) (drop n r-coll))
        acc))))

(defcheck solution-bb287ea2
  (fn par [n coll]
    (when (>= (count coll) n)
      (cons (take n coll)
        (par n
          (drop n coll))))))

(defcheck solution-bb62505c
  (fn my-partition [n coll]
    (if (< (count coll) n) ()
                           (cons (take n coll) (my-partition n (drop n coll))))))

(defcheck solution-bb91fd07
  (fn f [n l] (if (< (count l) n)
                (list)
                (cons (take n l) (f n (drop n l)))
                )
    ))

(defcheck solution-bbda1708
  (fn p [n s]
    (if-not (< (count s) n)
      (cons (take n s) (p n (drop n s))))))

(defcheck solution-bcf38d41
  #(loop [f (list (take %1 %2))
          r (drop %1 %2)]
     (if (empty? r)
       (reverse f)
       (if (= (count (take %1 r)) %1)
         (recur (conj f (take %1 r)) (drop %1 r))
         (recur f (drop %1 r))))))

(defcheck solution-bd59b672
  (fn [n s]
    (apply (partial map list)
      (map #(take-nth n (drop % s)) (range n)))))

(defcheck solution-bd67c48f
  (fn party [cnt data]
    (if (or (empty? data) (< (count data) cnt))
      (empty data)
      (cons (take cnt data) (party cnt (drop cnt data))))))

(defcheck solution-be15e749
  (fn my-partition [n s]
    (if (< (count s) n)
      '()
      (apply #(cons %1 (my-partition n %2)) (split-at n s)))))

(defcheck solution-be3b443b
  (fn [n coll]
    (let [subs (take-while #(<= n (count %)) (iterate #(drop n %) coll))]
      (map #(take n %) subs))))

(defcheck solution-bf0946c0
  (fn part [n xs]
    (if (<= n (count xs))
      (cons (take n xs) (part n (drop n xs))))))

(defcheck solution-bf5d040d
  (fn my-part [n col]
    (loop [m [] col col]
      (if (< (count col) n)
        m
        (recur (conj m (take n col)) (drop n col)
          )))))

(defcheck solution-bf79a573
  (fn part [n lst]
    (if (> n (count lst)) []
                          (cons (take n lst)
                            (part n (drop n lst))))))

(defcheck solution-bf9e2d68
  (fn [n s]
    (letfn [(f [o s]
              (if (< (count s) n)
                o
                (f (conj o (take n s))  (drop n s) ) ) ) ]
      (f [] s) ) ))

(defcheck solution-bfefcea9
  (fn [n coll]
    (letfn [(part [n coll]
              (when (seq coll)
                (cons
                  (take n coll)
                  (part n (drop n coll)))))]
      (filter #(= n (count %)) (part n coll)))))

(defcheck solution-c0208197
  (fn partition--reduce
    [n coll] {:pre [(integer? n), (pos? n)]}
    (->> coll
      (reduce (fn [[out acc i] x]
                (if (= i n)
                  [(conj out (conj acc x)) [] 1]
                  [out (conj acc x) (inc i)]))
        [[] [] 1])
      first)))

(defcheck solution-c04e851
  #(map (fn [x] (take % (drop (* % x) %2)))
     (range (quot (count %2) %))))

(defcheck solution-c0a4a854
  (fn my-partition [n coll]
    (for [l
          (filter #(= (count %) n)
            (for [[k v]
                  (group-by #(quot (first %) n)
                    (map-indexed #(vec [%1 %2]) coll))]
              v))]
      (map first l))))

(defcheck solution-c0f775b1
  (fn partition-seq-2
    [n sequ]
    (if (and (<= n (count sequ)) (seq sequ))
      (cons (take n sequ) (partition-seq-2 n (drop n sequ))))))

(defcheck solution-c0fefadd
  (fn my-partition
    [n s]
    (lazy-seq
      (let [group (take n s)]
        (when (= (count group) n)
          (cons group (my-partition n (drop n s))))))))

(defcheck solution-c13bc0bc
  (fn f[ax als] (loop [ls als acc '()]
                  (if (< (count ls) ax)
                    (reverse acc)
                    (recur (drop ax ls) (cons (take ax ls) acc))))))

(defcheck solution-c14753fe
  ; use the parameters of range. Limit the range to len(col)-div(len(coll)).
  #(for [n (range 0 (- (count %2)(mod (count %2) %1)) %1)] (take %1 (nthrest %2 n)) ))

(defcheck solution-c1cd4f97
  (fn p[n s] (cons (take n s) (if (>= (count (drop n s)) n) (p n (drop n s)) ()))))

(defcheck solution-c1f6e228
  (fn f [n xs]
    (let [p (take n xs)]
      (when (= (count p) n)
        (cons p (f n (drop n xs)))))))

(defcheck solution-c227faca
  (fn partition1 [n xs]
    (lazy-seq
      ( if (<= n (count xs))
        (cons (take n xs)
          (partition1 n (drop n xs)))))))

(defcheck solution-c28b3f34
  (fn t [n v] (if (< (count v) n) '()  (conj (t n (drop n v)) (take n v)))))

(defcheck solution-c2a14483
  (fn f [n s]
    (if (< (count s) n)
      '()
      (cons (take n s) (f n (drop n s))))))

(defcheck solution-c34ef4ff
  (fn my-partition [n a-seq]
    (cond
      (< (count a-seq) n) '()
      :else (cons (first (split-at n a-seq))
              (my-partition n (first (rest (split-at n a-seq))))))))

(defcheck solution-c366ee5
  (fn [n coll] (partition-by #(quot % n) (drop-last (mod (count coll) n) coll))))

(defcheck solution-c3a25474
  (fn[n l]
    (for [x (range (quot (count l) n))]
      (for [y (range (* x n) (+ (* x n) n))]
        (nth l y)))))

(defcheck solution-c3a37cca
  (fn [n coll]
    (loop [cur coll
           res []]
      (if (>= (count cur) n)
        (recur (drop n cur) (into res (list (take n cur))))
        res))))

(defcheck solution-c3bfe90
  (fn it [n c]
    (if (> n (count c)) [] (cons (take n c) (it n (drop n c))))))

(defcheck solution-c3e19747
  #(loop [s %2 r []]
     (if (< (count s) %) r
                         (recur (drop % s) (conj r (take % s))))))

(defcheck solution-c3e23bf4
  (fn f [n v]
    (loop [ans [] vv v]
      (if (< (count vv) n)
        ans
        (recur (conj ans (take n vv)) (drop n vv))))))

(defcheck solution-c3e44005
  #(for
    [p (range 0 (int (/ (count %2) %)))]
     (take % (drop (* p %) %2))))

(defcheck solution-c3e5b589
  (fn [s l] (filter #(= (count %) s) (for [x (range (/ (count l) s))] (take s (drop (* x s) l))))))

(defcheck solution-c4012fad
  (fn my-partition [n coll]
    (keep #(if (= (count %) n) % nil)
      (for [i (range 0 (count coll) n)]
        (take n (drop i coll))))))

(defcheck solution-c40e1f3b
  (fn [n xs]
    (loop [xs xs r []]
      (if (> n (count xs))
        r
        (recur (drop n xs) (conj r (take n xs)))))))

(defcheck solution-c4d05de0
  #(loop [o [(take %1 %2)] a (drop %1 %2)]
     (if (> %1 (count a))
       o
       (recur (conj o (take %1 a))
         (drop %1 a)))))

(defcheck solution-c4f5ea82
  (fn [n s]
    (loop [s s r []]
      (if (>= (count s) n)
        (recur (drop n s) (conj r (take n s)))
        r))))

(defcheck solution-c55ab9b6
  (fn partition* [n xs]
    (lazy-seq
      (let [[head tail] (split-at n xs)]
        (when (= n (count head))
          (cons head (partition* n tail)))))))

(defcheck solution-c563f0d4
  (fn part [n s]
    (when (>= (count s) n) (concat (list (take n s)) (part n (nthrest s n)) ))))

(defcheck solution-c5c6e658
  (fn prob54 [n s]
    (loop [col s
           acc ()]
      (if (< (count col) n)
        (reverse  acc)
        (recur (drop n col) (conj acc (take n col)))))))

(defcheck solution-c5ea256e
  (fn ff [x s] (let [[h t] (split-at x s) ] (if (= x (count h)) (cons h (ff x t)) [] ))))

(defcheck solution-c649be4f
  (fn [n s]
    (loop [s s
           res '()
           cur '()]
      (if (empty? s)
        (reverse res)
        (if (= (dec n) (count cur))
          (recur (rest s) (cons (reverse (cons (first s) cur)) res) '())
          (recur (rest s) res (cons (first s) cur)))))))

(defcheck solution-c6e50d7c
  (fn part [n s] (if (< (count s) n) () (cons (take n s) (part n (drop n s))))))

(defcheck solution-c703c7e7
  (fn f [n coll] (when (>= (count coll) n) (cons (take n coll) (f n (drop n coll))))))

(defcheck solution-c73d6b54
  (fn __ [n coll]
    (let [raw (group-by #(quot % n) coll)]
      (for [i (range (quot (count coll) n))]
        (raw i)
        ))))

(defcheck solution-c780ee38
  (fn p
    ([n current remain]
     (if (or (empty? remain) (> n (count remain))) current
                                                   (let [s (split-at n remain)]
                                                     (recur n (conj current (first s)) (flatten (rest s))))))
    ([n l]
     (p n [] l))))

(defcheck solution-c7e0af94
  (fn
    [n coll]
    (filter #(= n (count %))
      (->> coll (map-indexed vector) (group-by #(quot (first %) n)) (vals) (map #(map first %))))))

(defcheck solution-c85cdb28
  (fn p [n s]
    (if (>= (count s) n)
      (cons (take n s) (p n (drop n s))))))

(defcheck solution-c878385b
  (fn[s c] (for [i (range (quot (count c) s))] (take s (drop (* i s) c)))))

(defcheck solution-c8a193c1
  (fn partition-seq [n xs]
    (take-while #(= n (count %)) (cons (take n xs) (lazy-seq (partition-seq n (drop n xs)))))))

(defcheck solution-c8c74adc
  (fn partition2 [n coll]
    (when (<= n (count coll))
      (cons (take n coll) (partition2 n (drop n coll))))))

(defcheck solution-c8d2c6c6
  (fn part [n xs]
    (if (> n (count xs))
      ()
      (cons (take n xs) (part n (drop n xs))))))

(defcheck solution-c8e06472
  (fn [n c]
    (->> c
      (map-indexed #(vector (quot % n) %2))
      (partition-by (fn [[x y]] x))
      (map #(map (fn [[x y]] y) %))
      (filter #(= (count %) n)))))

(defcheck solution-c92d5854
  (fn p [n s]
    (if (> n (count s))
      []
      (cons (take n s) (p n (drop n s))))))

(defcheck solution-c98131af
  (fn[x y]
    (loop [l y v []]
      (if (empty? l)
        (filter #(= (count %) x) v)
        (recur (drop x l) (conj v (take x l)))))))

(defcheck solution-c9ef1912
  (fn partition* [n s]
    (if (< (count s) n)
      ()
      (cons (take n s)
        (lazy-seq (partition* n
                    (drop n s)))))))

(defcheck solution-cad3dac0
  (fn mypart [n col]
    (if (> n (count col)) '() (cons (take n col) (mypart n (drop n col))))))

(defcheck solution-cad4e448
  (fn pati [n x] (cons (take n x) (if (< (count x) (* n 2)) () (pati n (drop n x))))))

(defcheck solution-cad83763
  #(loop [r [] l %2]
     (if (> % (count l))
       r
       (recur (conj r (take % l)) (drop % l)))))

(defcheck solution-cb1f43bf
  (fn [n s]
    (loop [[f r] (split-at n s) res []]
      (if (= n (count f))
        (recur (split-at n r) (conj res f))
        res))))

(defcheck solution-cb5b0e18
  (fn [n col]
    (let [indexcol (map-indexed  (fn [idx itm] [(quot idx n) itm] )  col) ]
      (filter #(= (count %) n) (map  (fn [v] (map second (second v) )) (group-by first indexcol))
        )                       )
    ))

(defcheck solution-cb636520
  (fn f [n xs]
    (if (>= (count xs) n)
      (conj (f n (drop n xs)) (take n xs))
      )
    ))

(defcheck solution-cb7c38a2
  (fn my-partition [n coll]
    (let [mp (fn mp [x y]
               (if (empty? y)
                 (if (< (count x) n) (list) (list x))
                 (cons x (mp (take n y) (drop n y)))))]
      (mp (take n coll) (drop n coll)))))

(defcheck solution-cb84c0a1
  (fn [n xs] (->> xs
               (iterate #(drop n %))
               (map #(take n %))
               (take-while #(== n (count %))))))

(defcheck solution-cbb2ff0d
  #(loop [x %2 y []] (if (< (count x) %) y (recur (drop % x) (conj y (vec (take % x)))))))

(defcheck solution-cbbd1614
  (fn my-partition [size items]
    (loop [remaining items
           acc ()]
      (if (< (count remaining) size)
        (reverse acc)
        (recur (drop size remaining) (conj acc (take size remaining)))))))

(defcheck solution-cbbe0790
  (fn part [n coll]
    (if (or (empty? coll) (< (count coll) n))
      '()
      (cons (take n coll) (part n (drop n coll))))))

(defcheck solution-cc2cacc5
  (fn part [n coll] (if (>= (count coll) n) (cons (take n coll) (part n (drop n coll))))))

(defcheck solution-cc3c59a1
  (fn [x y]
    (filter #(= x (count %)) (map #(map first %) (partition-by #(quot (first %) x) (map-indexed vector y))))))

(defcheck solution-cc63cf2
  (fn prttn [n coll]
    (if (< (count coll) n)
      '()
      (cons (take n coll) (prttn n (drop n coll))))))

(defcheck solution-cc9d545e
  #(for [i (range 0 (inc (- (count %2) %1)) %1)] (take %1 (drop i %2))))

(defcheck solution-ccb00fcc
  (fn part [x coll]
    (if (< (count coll) x)
      nil
      (cons
        (take x coll)
        (part x (drop x coll)))
      )
    ))

(defcheck solution-cce839d3
  (fn [sz col] (loop [c col result '()] (if (> sz (count c)) result (recur (drop sz c) (concat result (list (take sz c))))))))

(defcheck solution-cd7ddafd
  (fn f
    ([n col] (f n col n () ()))
    ([n [h & tail :as col] left m-head agg]
     (let [head (cons h m-head)]
       (cond
         (not (seq col)) (reverse agg)
         (= left 1) (recur n tail n () (-> (reverse head) (cons agg)))
         :else (recur n tail (dec left) head agg))))))

(defcheck solution-cdd928c
  (fn mypart [psz coll]
    (let [p (take psz coll)]
      (if (< (count p) psz)
        '()
        (cons p (mypart psz (drop psz coll)))))))

(defcheck solution-ce3cbff8
  (fn [x xs]
    (loop [tmp []
           res []
           s xs]
      (if (empty? s)
        res
        (if (= (- x 1) (count tmp))
          (recur []
            (conj res (conj tmp (first s)))
            (rest s))
          (recur (conj tmp (first s))
            res
            (rest s)))))))

(defcheck solution-cf4d42f6
  #(for [s (range (int (/ (count %2) %)))]
     (subvec (vec %2) (* s %) (* (+ 1 s) %))))

(defcheck solution-cfb89362
  (fn part [n ls]
    (lazy-seq
      (if (empty? ls)
        ()
        (let [[fs rs] (split-at n ls)
              len (count fs)]
          (if (< len n)
            ()
            (cons fs (part n rs))))))))

(defcheck solution-d00572ac
  (fn [n coll]
    (loop [coll coll res []]
      (if (< (count coll) n)
        res
        (recur (drop n coll) (conj res (take n coll)))))))

(defcheck solution-d011dc8b
  #(loop [s %2 q []]
     (let [c (take % s)
           r (drop % s)
           z (if (< (count c) %) q (conj q c))]
       (if (= r ()) z (recur r z)))))

(defcheck solution-d043c097
  (fn p [n coll]
    (let [s (take n coll)]
      (if (= (count s) n)
        (lazy-seq (cons s (p n (drop n coll))))))))

(defcheck solution-d065da
  (fn [n seq]
    (loop [seq seq
           result []]
      (if (empty? seq)
        result
        (let [head (take n seq)
              tail (nthrest seq n)]
          (recur tail
            (if (= n (count head))
              (conj result head)
              result)))))))

(defcheck solution-d09989f8
  (fn [n coll]
    (loop [i coll o []]
      (if (empty? i)
        o
        (let [s (split-at n i) f (first s) b (second s)]
          (recur
            (if (>= (count b) n)
              b '())
            (conj o f)))))))

(defcheck solution-d0cb7654
  (fn [x s]
    (loop [acc []
           l s]
      (if (< (count l) x)
        acc
        (recur (conj acc (take x l)) (nthrest l x))))))

(defcheck solution-d10a1ae3
  (fn p [x c]
    (let [[a b] (split-at x c)]
      (when (= x (count a))
        (cons a (p x b))))))

(defcheck solution-d11cb943
  (fn split [n coll]
    (if (>= (count coll) n)
      (cons
        (take n coll)
        (split n (drop n coll))))
    ))

(defcheck solution-d12ab295
  (fn f[n lst]
    (if (<= n (count lst))
      (cons (take n lst) (f n (drop n lst))))))

(defcheck solution-d170b432
  (fn [n s]
    (loop [s s, result []]
      (if (< (count s) n)
        result
        (recur (nthrest s n) (conj result (take n s)))))))

(defcheck solution-d18827cf
  (fn [n coll]
    (take-while #(<= n (count %))
      (map (partial take n)
        (iterate (partial drop n) coll)))))

(defcheck solution-d1adede2
  (fn partition' [n col]
    (loop [acc []
           col col]
      (let [part (take n col)]
        (if (< (count part) n)
          acc
          (recur (conj acc part) (drop n col)))))))

(defcheck solution-d1af62e8
  (fn my-partition [size col] (let [result
                                    (reduce (fn [a b] (
                                                        if (and (not (empty? a)) (< (count (last a)) size))
                                                        (assoc a (dec (count a)) (conj (last a) b))
                                                        (conj a [b])
                                                        )) [] col)] (
                                                                      if (< (count (last result)) size) (drop-last result) result
                                                                                                        ))))

(defcheck solution-d206ca9c
  (fn partitur [n c]
    (lazy-seq (let [s (take n c)]
                (if (= n (count s))
                  (cons s (partitur n (drop n c)))
                  nil)))))

(defcheck solution-d23a0087
  (fn p [n c] (when (<= n (count c)) (cons (take n c) (p n (drop n c))))))

(defcheck solution-d2bbd620
  ; not lazy though
  (fn [n coll]
    (loop [acc [] coll coll]
      (if (>= (count coll) n)
        (recur (conj acc (take n coll)) (drop n coll))
        acc))))

(defcheck solution-d2dc268d
  (fn f [n xs]
    (if (> n (count xs))
      nil
      (concat [(take n xs)] (f n (drop n xs))))))

(defcheck solution-d37adafc
  (fn [p coll] (loop [acc1 [] acc2 [] rm coll]
                 (if (empty? rm)
                   acc1
                   (if (= (dec p) (count acc2))
                     (recur (conj acc1 (conj acc2 (first rm))) [] (rest rm))
                     (recur acc1 (conj acc2 (first rm)) (rest rm)))))))

(defcheck solution-d3f9e4cb
  #(loop [r [] c %2] (if (>= (count c) %1) (recur (conj r (take %1 c)) (drop %1 c)) r)))

(defcheck solution-d400e17d
  (fn [n xs]
    (loop [xs' xs
           acc []]
      (if (or (nil? (seq xs')) (< (count (take n xs')) n))
        acc
        (recur (drop n xs') (conj acc (take n xs')))))))

(defcheck solution-d47db441
  (fn [x xs] (apply map list (vals (group-by #(mod % x) xs)))))

(defcheck solution-d496e2ae
  (fn [n coll]
    (letfn [(rev-inter [l n]
              (for [x (range n)]
                (take-nth n (drop x l))))]
      (apply map vector
        (rev-inter coll n)))))

(defcheck solution-d50ba12a
  (fn pt [n aseq]
    (letfn [(go [s]
              (let [sq (seq s)
                    front (take n sq)]
                (when (= n (count front))
                  (cons front (go (drop n sq))))))]

      (lazy-seq (go aseq)))))

(defcheck solution-d51d3dab
  (fn p [a s]
    (let [res (reduce
                (fn [prev n]
                  (if (< (count (first prev)) a)
                    (conj (rest prev) (conj (first prev) n))
                    (conj prev (list n)))) [] s)
          res1 (if (< (count (first res)) a) (rest res) res)
          res2 (reverse res1)
          res3 (map reverse res2)]
      res3)))

(defcheck solution-d5725419
  (fn my-partition
    [partition-size s]
    (loop [old-seq s
           partitioned-seq '()]
      (if (> partition-size (count old-seq))
        partitioned-seq
        (recur (drop partition-size old-seq) (concat partitioned-seq (list (take partition-size old-seq))))))))

(defcheck solution-d5afe27c
  (fn p [n xs]
    (when (<= n (count xs))
      (cons (take n xs)
        (lazy-seq (p n (drop n xs)))))))

(defcheck solution-d5c15fea
  (fn f [c coll]
    (if (< (count coll) c)
      []
      (cons (take c coll) (lazy-seq (f c (drop c coll)))))))

(defcheck solution-d5e3c6ab
  (fn [n l]
    (loop [ret [] r l ]
      (if (empty? r)
        (if (= n (count (last ret) ))
          ret (butlast ret)
          )
        (recur (conj ret (take n r)  )     (drop n r) )
        )
      )
    ))

(defcheck solution-d629bd4c
  (fn [n s]
    (loop [acc (conj [] (take n s))
           s1 (nthrest s n) ]
      (if (< (count s1) n )
        acc
        (recur (conj acc (take n s1)) (nthrest s1 n))
        ))))

(defcheck solution-d6c07167
  (fn partition-seq
    [split-size seq]
    (let [number-collections (quot (count seq) split-size)]
      (loop [remaining-seq seq
             collections-left number-collections
             collections '[]]
        (if (= collections-left 0)
          collections
          (recur (drop split-size remaining-seq)
            (dec collections-left)
            (conj collections (take split-size remaining-seq))))))))

(defcheck solution-d7a3a00b
  (fn [n xs] (let [len (count xs)
                   m (/ len n)
                   rlen (* (int m) n)
                   ixs (take rlen xs)
                   hfx (fn [res p ts] (if (first ts) (recur (conj res (take p ts)) p (drop p ts))
                                                     res))
                   ]
               (hfx [] n ixs)
               )))

(defcheck solution-d7e3bffe
  (fn [n xs]
    (apply map vector
      (map (fn [x] (map first x))
        (vals (group-by (fn [[i x]] (mod i n)) (map-indexed vector xs)))))))

(defcheck solution-d7ed5495
  (fn part [n col]
    (if (>= (count col) n)
      (cons (take n col) (part n (drop n col))))))

(defcheck solution-d8350354
  (fn p [x s]
    (if (< (count s) x)
      nil
      (cons (take x s) (p x (drop x s))))))

(defcheck solution-d89babc1
  #(loop [s %2 built ()]
     (if (< (count s) %1)
       built
       (recur (drop %1 s) (concat built (list (take %1 s)))))))

(defcheck solution-d8d4c38d
  (fn [num coll]
    (loop [result [] temp-result [] elements coll]
      (if elements
        (recur
          (if (= (count temp-result) num)
            (conj result temp-result)
            result
            )
          (if (= (count temp-result) num)
            [(first elements)]
            (conj temp-result (first elements))
            )
          (next elements)
          )
        (if (= (count temp-result) num)
          (conj result temp-result)
          result
          )
        )
      )
    ))

(defcheck solution-d906bcde
  #(letfn
    [(partition'[n s]
       (lazy-seq
         (when
          (<= n (count s))
           (cons
             (take n s)
             (partition' n (drop n s))))))]
     (partition' %1 %2)))

(defcheck solution-d92d092d
  (fn [n coll]
    (loop [result [], src coll]
      (let [next-partition (take n src)]
        (if (= (count next-partition) n)
          (recur (conj result next-partition) (drop n src))
          result)))))

(defcheck solution-d9852534
  (fn [cnt seq]
    (loop [acc []
           s seq]
      (if (>= (count s) cnt)
        (recur (conj acc (take cnt s)) (drop cnt s))
        acc))))

(defcheck solution-d9a240c1
  (fn  [n coll]
    ((fn fi [remain]
       (let [take-n (take n remain)]
         (if (< (count take-n) n)
           nil
           (cons take-n (fi (drop n remain))))))
     coll)))

(defcheck solution-da11eec9
  (fn [n s]
    (loop [s s v []]
      (if (< (count (take n s)) n)
        v
        (recur (drop n s) (conj v (take n s)))))))

(defcheck solution-da154c78
  (fn [n coll]
    (loop [res [] coll coll]
      (let [[p r] (split-at n coll)]
        (if (< (count p) n)
          res
          (recur (conj res p) r))))))

(defcheck solution-da20927d
  (fn [n s]
    (filter #(= n (count %))
      (reduce
        #(if (= (count (peek %1)) n)
           (conj %1 [%2])
           (conj (pop %1) (conj (peek %1) %2)))
        [[]]
        s))))

(defcheck solution-da4bf798
  (fn [n c]
    (loop [inp   c
           res   []]
      (let [h (take n inp)
            t (drop n inp)]
        (if (= n (count h))
          (recur t (conj res h))
          res)))
    ))

(defcheck solution-dabeb494
  (fn [n l]
    (loop [r l o []]
      (if (< (count r) n)
        o
        (recur (drop n r) (conj o (take n r)))))))

(defcheck solution-dae47076
  (fn [n  coll]
    (->> (range (quot (count coll) n))
      (map #(* n %))
      (map (fn [base]
             (->> (range n)
               (map #(+ base %))
               (map #(nth coll %))))))))

(defcheck solution-db530158
  (fn soln [n s]
    (take-while #(= n (count %))
      (cons (take n s) (lazy-seq (soln n (drop n s)))))))

(defcheck solution-db653c8f
  (fn f [n s]
    (if (>= (count s) n)
      (cons (take n s) (f n (drop n s))))))

(defcheck solution-db665303
  (fn [n xs]
    (loop [ys xs rs []]
      (case ys
        [] rs
        (let
         [y (take n ys)]
          (recur (drop n ys) (if (< (count y) n) rs (conj rs y))))))))

(defcheck solution-dbafa920
  #(loop [n %1 coll %2 acc []]
     (if (< (count coll) n)
       acc
       (recur n (drop n coll) (conj acc (take n coll))))))

(defcheck solution-dbd7414
  (fn [n s] (for [i (range (quot (count s) n))] (take n (drop (* i n) s)))))

(defcheck solution-dc51c5b4
  (fn [n s] (second (reduce
                      (fn [[cs res] i]
                        (if (= (dec n) (count cs))
                          [[] (conj res (conj cs i))]
                          [(conj cs i) res]))
                      [[] []]
                      s))))

(defcheck solution-dc522298
  (fn [x s]
    (if (< (count s) x)
      nil
      (filter #(>= (count %) x)
        (reduce #(if (< (count (last %)) x)
                   (concat (butlast %) (list (concat (last %) (list %2))))
                   (concat % (list (list %2))))
          (list (take x s))
          (nthrest s x))))))

(defcheck solution-dc84fa53
  (fn f [n coll]
    (if (>= (count coll) n)
      (cons (take n coll) (f n (drop n coll)))
      '())))

(defcheck solution-dc9caa46
  #(loop [result []
          remain %2]
     (if (> % (count remain))
       result
       (recur (conj result (take % remain)) (drop % remain)))))

(defcheck solution-dca3bd67
  (fn [x s]  (loop [res [] ls  s]
               (if (< (count ls) x) res
                                    (recur (conj res (take x ls)) (drop x ls))))))

(defcheck solution-ddc407db
  (fn f [n l]
    (when (= (count (take n l)) n)
      (cons (take n l) (f n (drop n l))))))

(defcheck solution-ddc4bd95
  (fn [len coll]
    (reverse
      (loop [rem coll
             ret '()]
        (if (> len (count rem))
          ret
          (recur (drop len rem)
            (cons (take len rem) ret)))))))

(defcheck solution-de1aba48
  #(loop [i % l %2 r (list)]
     (if (> i (count l))
       r
       (recur i (drop i l) (concat r (list (take i l))))
       )))

(defcheck solution-de418ead
  (fn [n s]
    (loop [ss s i 0 out [] store []]
      (if (empty? ss)
        (if (= n (count store))
          (conj out store)
          out)
        (if (and (zero? (mod i n)) (> i 0))
          (recur (rest ss) (inc i) (conj out store) [(first ss)])
          (recur (rest ss) (inc i) out (conj store (first ss))))))))

(defcheck solution-de7198f2
  (fn [x xs]
    (loop [acc [] coll xs]
      (if (< (count coll) x)
        acc
        (let [[p1 p2] (split-at x coll)]
          (recur (conj acc p1) p2))))))

(defcheck solution-de8e186f
  #(for [i (range (quot (count %2) %))]
     (take % (drop (* i %) %2))))

(defcheck solution-de92eca6
  (fn partitionX [n x] (if (< (count x) n) nil (let [s (split-at n x)] (cons (first s)(partitionX n (last s)))))))

(defcheck solution-dea7c0af
  (fn [n coll]
    (take-while #(= (count %) n) (map (partial take n) (iterate (partial drop n) coll)))))

(defcheck solution-ded1a770
  (fn part [n s]
    (let [c (count s)
          r (mod c n)]
      (for [i (range 0 (- c r) n)]
        (for [j (range i (+ i n))]
          (nth s j))))))

(defcheck solution-ded33dda
  (fn [div rng]
    (let [cnt (count rng)
          grps (* (quot cnt div) div)
          prt-rng (take (* (quot cnt grps) grps) rng)]
      (map #(map second %) (vals (group-by first (map-indexed (fn [a b] [(quot a div) b]) prt-rng)))))))

(defcheck solution-dedda80f
  (fn [n coll]
    ((fn [n coll acc]
       ; Base case (not enough items for a new group
       (if (< (count coll) n) acc
                              ; Take first n items and add them to accumulator
                              (recur n (drop n coll) (concat acc (list (take n coll)))))) n coll [])))

(defcheck solution-deecc340
  (fn my-partition [n col]
    (let [parts (fn [n col]
                  (let [icol (map-indexed #(list %1 %2) col)]
                    (loop [rest-col icol
                           cur (list)
                           res (list)]
                      (let [[i v] (first rest-col)
                            tail (rest rest-col)]
                        (cond
                          (empty? rest-col) (cons cur res)
                          (= 0 (mod i n)) (recur tail (list v) (cons cur res))
                          :else (recur tail (cons v cur) res))))))]
      (->> (parts n col)
        (map #(reverse %))
        (reverse)
        (filter #(= n (count %)))))))

(defcheck solution-df4b4d22
  (fn foo [n coll]
    (let [[a b] (split-at n coll)]
      (when (= n (count a))
        (cons a (lazy-seq (foo n b)))))))

(defcheck solution-df6a6c60
  (fn [n coll]
    (loop [coll coll acc []]
      (let [[front back] (split-at n coll)]
        (if (= (count front) n)
          (recur back (conj acc front))
          acc)))))

(defcheck solution-df7a0cfb
  (fn my-part [n coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (let [p (doall (take n s))]
          (when (= n (count p))
            (cons p (my-part n (nthrest s n)))))))))

(defcheck solution-df8db997
  (letfn [(f [n s] (lazy-seq (let [[p r] (split-at n s)] (if (= (count p) n) (cons p (f n r))))))] f))

(defcheck solution-dfea723c
  #(filter (fn [m] (= (count m) %1)) (map last (group-by (fn [n] (quot n %1)) %2))))

(defcheck solution-e0ed7bb6
  (fn [n col]
    (letfn [(f [a xs] (if (< (count xs) n)
                        a
                        (f (conj a (take n xs)) (drop n xs))))]
      (f [] col))))

(defcheck solution-e0ff80e3
  (fn [cnt col]
    (let [partitioned (reduce
                        (fn [acc v] (if (< (count (last acc)) cnt)
                                      (concat (butlast acc) (list (concat (last acc) (list v))))
                                      (concat acc (list (list v)))))
                        '(()) col)]
      (filter #(= cnt (count %)) partitioned))))

(defcheck solution-e1b4127a
  (fn mypatition [n coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (let [p (take n s)]
          (when (= n (count p))
            (cons p (mypatition n (nthrest s n)))))))))

(defcheck solution-e1ef1414
  #(for [i (range (quot (count %2) %))]
     (take % (drop (* % i) %2))))

(defcheck solution-e1fa716
  #(loop [step %
          coll (list %2)]
     (if (>= step (count (last coll)))
       (take-while (fn [c] (= step (count c))) coll)
       (recur step (concat (butlast coll)
                           (split-at step (last coll)))))))

(defcheck solution-e26c77d7
  (fn [c xs]
    (loop [r xs ret []]
      (if (< (count r) c)
        ret
        (recur (drop c r) (conj ret (take c r)))))))

(defcheck solution-e2c32b25
  (fn [n coll]
    (apply map (fn [& z] (map second z))
      (map (fn [x] (filter #(= (first %) x) (map #(list %1 %2) (flatten (repeat (/ (count coll) n) (range n))) coll)))
        (range n)))))

(defcheck solution-e2d7f1fe
  #(for [i (range (quot (count %2) %))] (take % (drop (* i %) %2))))

(defcheck solution-e2e8ff95
  (fn [n in]
    (loop [ret []
           r in]
      (if (>= (count (take n r)) n)
        (recur (conj ret (take n r)) (drop n r))
        ret))))

(defcheck solution-e32475fd
  (fn f [n s]
    (let [p (take n s)]
      (when (= n (count p))
        (cons p (lazy-seq (f n (nthrest s n))))))))

(defcheck solution-e35ca716
  (fn [n coll] (loop [c coll a []] (if (> n (count c)) a (recur (drop n c) (conj a (take n c)))))))

(defcheck solution-e377357b
  (fn [c l] (filter #(= c (count %)) (partition-by #(int (/ % c)) l))))

(defcheck solution-e3e1e2c3
  (fn f [n c]
    (filter #(= (rem (first %) n) 0) (apply map vector (take n (iterate next c))))))

(defcheck solution-e40f5909
  (fn part [n s]
    (if (>= (count s) n)
      (concat (list (take n s)) (part n (drop n s)))
      nil)))

(defcheck solution-e4484ab7
  (fn [x y] (reverse(loop [a y res []]
                      (if (< (count a) x)
                        res
                        (recur (drop x a) ( cons  (take x a) res))
                        )))))

(defcheck solution-e4c73597
  (fn part [x items] (if (<= x (count items)) (cons (take x items) (part x (drop x items))))))

(defcheck solution-e6974706
  #(loop [coll %2 res []]
     (if (< (count coll) %)
       res
       (recur (drop % coll) (conj res (take % coll))))))

(defcheck solution-e73ae3b1
  (fn part [n sq] (if (< (count sq) n) '() (cons (take n sq) (part n (drop n sq))))))

(defcheck solution-e74bdd6f
  ; recursion is better: (fn f [n x] (if (<= n (count x)) (cons (take n x) (f n (drop n x)))))
  #(filter (fn [a] (= %1 (count a)))
     (for [x (map list (range (count %2)) %2)
           :let [i (first x)]]
       (when (= 0 (mod i %1))
         (take %1 (drop i %2))))))

(defcheck solution-e767801d
  (fn [n coll]
    (loop [res [] lst coll]
      (if (> n (count lst)) res
                            (recur (conj res (take n lst)) (drop n lst))))))

(defcheck solution-e76b30d0
  (fn ptn [n s]
    (let [next-chunk (take n s)]
      (if (= (count next-chunk) n)
        (cons next-chunk (ptn n (drop n s)))))))

(defcheck solution-e79ca19b
  (fn [n, s]
    (loop [i s r []]
      (if (< (count i) n)
        r
        (recur (drop n i) (conj r (take n i)))))))

(defcheck solution-e7c5bb5d
  (fn [n l]
    (filter #(= (count %1) n)
      (map #(take n (drop %1 l))
        (filter #(zero? (mod %1 n))
          (range (count l)))))))

(defcheck solution-e7c652d9
  (fn prob-0052
    [n in-xs]
    (let [xs  (seq in-xs)
          n2  (* 2 n)
          x2  (take n2 xs)
          nx2 (count x2)]
      (cond
        (< nx2 n)  []
        (< nx2 n2) (cons (take n x2) nil)
        :else      (cons (take n xs) (prob-0052 n (drop n xs)))))))

(defcheck solution-e7fb1f5e
  #(loop [v %2 r []]
     (if (< (count v) %)
       r
       (recur (drop % v)(conj r (take % v))))))

(defcheck solution-e7fda6b2
  (fn [n col]
    (loop [col col s []]
      (if (> n (count col))
        s
        (recur (drop n col) (conj s (take n col)))))))

(defcheck solution-e8069918
  (partial
    (fn [acc n l]
      (if (< (count l) n)
        acc
        (recur (conj acc (take n l))
          n
          (drop n l)
          )
        )
      ) []))

(defcheck solution-e847b9f8
  (fn f [n coll]
    (if (<= n (count coll))
      (cons (take n coll) (f n (drop n coll))))))

(defcheck solution-e86f2ff4
  (fn [x xs]
    (for [idx (map #(* x %) (range (quot (count xs) x)))]
      (take x (drop idx xs)))))

(defcheck solution-e8af6988
  (fn [n coll]
    (loop [items coll res []]
      (let [piece (take n items)]
        (if (= n (count piece))
          (recur (drop n items) (conj res piece))
          res)))))

(defcheck solution-e933d997
  (fn partition2 [n s]
    (let [[x y] (split-at n s)]
      (if (<= n (count x))
        (cons x (partition2 n y))))))

(defcheck solution-e9838955
  (fn part [l xs]
    (let [[left right] (split-at l xs)]
      (if (not-empty right)
        (cons left (part l right))
        (when (== (count left) l)
          (list left))))))

(defcheck solution-e9913ee8
  (fn part [n l]
    (if (< (count l) n)
      []
      (let [[a b] (split-at n l)]
        (cons a (part n b))))))

(defcheck solution-e9a93efa
  (fn my-partition [n s]
    (loop [s s acc []]
      (let [ss (take n s)]
        (if (= n (count ss))
          (recur (drop n s) (conj acc ss))
          acc)))))

(defcheck solution-e9f540
  (fn my-partition [n col] (lazy-seq (let [[x xs] (split-at n col)] (if (= n (count x)) (cons x (my-partition n xs)))))))

(defcheck solution-ea191e8d
  #(loop [c %2 r '()]
     (if (< (count c) %1) (reverse r)
                          (recur
                            (drop %1 c)
                            (cons (take %1 c) r)))))

(defcheck solution-ea4d79d9
  (fn x1 ([i c r]
          (if (empty? c)
            (filter #(= (count %) i) r)
            (recur i (drop i c) (conj r (take i c)))))
    ([i c] (x1 i c []))))

(defcheck solution-eaa3a8d7
  (fn [s xs]
    (filter #(= (count %) s)
      (reduce
        (fn [w i]
          (if (= (count (last w)) s)
            (concat w [[i]])
            (concat (drop-last w) (list (conj (last w) i)))))
        [[]]
        xs))))

(defcheck solution-eac9dacf
  (fn [n s]
    (loop [s s
           a []]
      (if (seq s)
        (let [r (take n s)]
          (recur (drop n s) (if (= n (count r)) (conj a r) a)))
        a))))

(defcheck solution-ead82052
  (fn my-part [n x]
    (loop [lst x
           result []
           smallList []
           curr 1
           total n]
      (if (empty? lst)
        result
        (if (= curr total)
          (recur (rest lst) (conj result (conj smallList (first lst))) [] 1 total)
          (recur (rest lst) result (conj smallList (first lst)) (inc curr) total))))))

(defcheck solution-eae1870b
  #(for[i %2 :when (and (> (- (count %2) i) (- % 1)) (= 0 (rem i %)))] (take % (drop i %2))))

(defcheck solution-eb299271
  (fn pt [n coll]
    (lazy-seq
      (when-let [head (take n coll)]
        (if (= n (count head))
          (cons head (pt n (drop n coll))))))))

(defcheck solution-eb3b8c25
  (fn fun [n coll]
    (if (< (count coll) n) []
                           (let [[f s] (split-at n coll)]
                             (concat [f] (fun n s))))))

(defcheck solution-eb555dd0
  (fn f [n x]
    (if (and (seq x) (>= (count x) n))
      (cons (take n x) (lazy-seq (f n (drop n x))))
      []
      )
    ))

(defcheck solution-eb71ee8e
  (fn [n c]
    (loop [r c res []]
      (if (< (count r) n)
        res
        (recur (drop n r) (conj res (take n r)))))))

(defcheck solution-eb819bfd
  (fn[n a-seq]
    (remove #(not (zero? (mod (count %) n)))
      (for [k (range 0 (count a-seq) n)]
        (take n (drop k a-seq))))))

(defcheck solution-ebb06421
  (fn [n coll]
    (loop [left coll
           result []]
      (if (empty? left)
        result
        (recur (drop n left)
          (let [part (take n left)]
            (if (= n (count part))
              (conj result part)
              result)))))))

(defcheck solution-ec4a3964
  (fn partt [n col]
    (loop [rs [], e [], c col]
      (if (empty? c)
        (if (= n (count e))
          (concat rs [e])
          rs
          )
        (if (= n (count e))
          (recur (concat rs [e]) [] c)
          (recur rs (concat e [(first c)]) (rest c))
          )
        )
      )
    ))

(defcheck solution-ec5ca976
  (fn [n vs]
    (letfn [(vec->list [v] (->> v rseq (into '())))]
      (loop [vs' vs, result [], bucket []]
        (if (seq vs')
          (if (and (empty? bucket)
                   (< (count vs') n))
            (vec->list result)
            (let [v (first vs')
                  new-bucket (conj bucket v)]
              (if (= n (count new-bucket))
                (recur (rest vs')
                  (conj result (vec->list new-bucket))
                  [])
                (recur (rest vs') result new-bucket))))
          (vec->list result))))))

(defcheck solution-ec87f10c
  (fn [l s]
    ((fn ptt [r s]
       (let [p (take l s)]
         (if (= l (count p))
           (recur (conj r p) (drop l s))
           r)))
     [] s)))

(defcheck solution-ec93486f
  (fn f ([n x]
         (f n x [] []))
    ([n x t r]
     (cond (= n (count t)) (recur n x [] (conj r t))
           (empty? x) r
           :else (recur n (rest x) (conj t (first x)) r)))))

(defcheck solution-ecaa3631
  (fn [i s]
    ((fn ! [s agg]
       (if-let [s (seq s)]
         (if (>= (count s) i)
           (! (drop i s) (conj agg (take i s)))
           agg
           )
         agg)) s [])))

(defcheck solution-ed1e9f99
  (fn part [n coll]
    (let [[p r] (split-at n coll)]
      (when (= (count p) n) (cons p (part n r))))))

(defcheck solution-ed9beefc
  (fn p [n l]
    (letfn [(helper [i ll c r]
              (cond (empty? ll) (if (= (count c) n)
                                  (conj r c)
                                  r)
                    (= 0 i) (helper n ll [] (conj r c))
                    :else (helper (dec i) (rest ll) (conj c (first ll)) r)))]
      (helper n l [] []))))

(defcheck solution-eda2716b
  (fn mypart [n s]
    (lazy-seq
      (when (and (seq s) (<= n (count s)))
        (cons (take n s)
          (mypart n (drop n s)))))))

(defcheck solution-edcded88
  (fn part [n coll]
    (loop [coll coll res '()]
      (let [cur (take n coll)]
        (cond (> n (count cur)) (reverse res)
              :else (recur (drop n coll) (cons cur res))
              )
        )
      )
    ))

(defcheck solution-eded2181
  (fn prttn [n coll]
    (for [x (range (quot (count coll) n))]
      (take n (drop (* x n) coll)))))

(defcheck solution-ee46d625
  (fn prt [n coll]
    (when-let [s (seq coll)]
      (let [firstn (take n s)] (when (>= (count firstn) n)
                                 (lazy-seq (cons firstn (->> s (drop n) (prt n) )  ) )  ) ))
    ))

(defcheck solution-ee7285c9
  (fn part [n xs]
    (if (< (count xs) n)
      ()
      (cons (take n xs) (part n (drop n xs))))))

(defcheck solution-eeab77ea
  #(loop [coll %2 result []]
     (if (< (count coll) %1)
       result
       (recur (drop %1 coll) (conj result (take %1 coll))))))

(defcheck solution-eed88cec
  (fn [n s]
    (map #(take n (drop (* n %) s))
      (range (quot (count s) n)))))

(defcheck solution-ef5fb6a
  (fn R [n S] (let [b (take n S)] (if (= n (count b)) (cons b (R n (drop n S)))))))

(defcheck solution-efbc55e8
  (fn [n l]
    (take-while #(= n (count %)) (map #(take n %) (iterate (partial drop n) l)))
    ))

(defcheck solution-f07b3f5c
  (fn ps [n xs]
    (when (>= (count xs) n)
      (conj (ps n (drop n xs)) (take n xs) ))))

(defcheck solution-f0e50a35
  (fn partition2 [n coll]
    (let [[a b] (split-at n coll)]
      (if (= (count a) n)
        (cons a (partition2 n b))
        ()))))

(defcheck solution-f17bf318
  #(loop [res [], [h t] (split-at %1 %2)] ; %1 = n, %2 = collection
     (if (= (count h) %1)
       (recur (conj res h) (split-at %1 t))
       res)))

(defcheck solution-f184c422
  (fn [n c] (loop [res [] c c] (if (>= (count c) n)
                                 (recur (conj res (take n c)) (drop n c)) res))))

(defcheck solution-f33fc866
  (fn mypart [x y]
    (when-let[h (seq (take x y))]
      (when(= (count h) x)
        (cons h (mypart x (drop x y)))
        )
      )
    ))

(defcheck solution-f3d1d2b
  (fn partition' [n coll]
    (lazy-seq
      (if (>= (count coll) n)
        (cons (take n coll) (partition' n (drop n coll)))))))

(defcheck solution-f3e599c3
  (fn myPartition
    [n coll]
    (loop [result [] newColl coll]
      (if (< (count newColl) n)
        result
        (recur (conj result (take n newColl)) (drop n newColl))))))

(defcheck solution-f4774f7c
  (fn part [size s]
    (loop [col s res []]
      (if (< (count col) size)
        res
        (recur (drop size col) (conj res (take size col)))))))

(defcheck solution-f4c308dc
  #(loop [xs %2 acc []]
     (if (< (count xs) %)
       acc
       (recur (drop % xs) (conj acc (take % xs))))))

(defcheck solution-f4e7d82b
  (fn part [x ys]
    (when (>= (count ys) x)
      (lazy-seq (cons (take x ys) (part x (drop x ys)))))))

(defcheck solution-f5023975
  (fn [ct coll]
    ((fn accfn [acc cl]
       (if (empty? cl)
         acc
         (accfn (conj acc (take ct cl)) (drop ct cl))))
     []
     (take (- (count coll) (rem (count coll) ct)) coll))))

(defcheck solution-f519cd32
  (fn partition-alt [n coll]
    (when
     (and (seq coll) (>= (count coll) n))
      (cons (take n coll) (partition-alt n (drop n coll))))))

(defcheck solution-f51b64fd
  (fn partition-it
    [c s]
    (loop [rest s
           accum []]
      (let [next (take c rest)
            new-rest (drop c rest)]
        (let [new-accum (if (= c (count next))
                          (conj accum next)
                          accum)]
          (if (empty? new-rest)
            new-accum
            (recur new-rest new-accum)))))))

(defcheck solution-f55ae245
  (fn [n coll]
    (loop [out []
           c coll]
      (if (< (count c) n)
        out
        (recur (conj out (take n c))
          (drop n c))))))

(defcheck solution-f619975b
  (fn part [n coll]
    (let [els (take n coll)]
      (when (= (count els) n)
        (cons els (part n (drop n coll)))))))

(defcheck solution-f65f4fc1
  (fn [n lst]
    (loop [lst lst ret [] ]
      (if (> n (count lst)) ret
                            (recur (drop n lst) (conj ret (take n lst)))))))

(defcheck solution-f6f48a01
  (fn [n s] (filter #(= (count %) n) (map #(map second %) (partition-by first (map vector (apply interleave (repeat n (range))) s))))))

(defcheck solution-f7269785
  (fn [n s]
    ((fn prt-all [s]
       (if (>= (count (last s)) n)
         (prt-all (concat (drop-last s) (split-at n (last s))))
         (drop-last s)))
     (list s))))

(defcheck solution-f73abd47
  (fn [n l]
    (loop [news '() tmpl l]
      (if (empty? tmpl)
        (reverse news)
        (if (= n (count (take n tmpl)))
          (recur (conj news (take n tmpl)) (drop n tmpl))
          (recur news (drop n tmpl)))))))

(defcheck solution-f75057c
  (fn [n lst]
    (reverse (filter #(= n (count %))
               (reduce (fn [[fl & rl] ni]
                         (if (< (count fl) n)
                           (cons (conj fl ni ) rl)
                           (cons (vector ni) (cons fl rl)))
                         )
                 [[]] lst)))))

(defcheck solution-f78bf481
  (fn [n c]
    (for [x (range (int (/ (count c) n)))]
      (subvec (vec c) (* x n) (+ n (* x n)))
      )
    ))

(defcheck solution-f7f1db6e
  (fn [n coll]
    (let [pairs (map vector (cycle (range n)) coll)]
      (loop [s pairs seqs [] curr []]
        (if (seq s)
          (let [[i elt] (first s)]
            (if (= i (dec n))
              (recur (next s) (conj seqs (conj curr elt)) [])
              (recur (next s) seqs (conj curr elt))
              )
            )
          seqs)))))

(defcheck solution-f8108ca8
  #(loop [result [] cur %2]
     (if (> %1 (count cur))
       result
       (recur (conj result (take %1 cur)) (drop %1 cur)))))

(defcheck solution-f869419d
  (fn my-partition [n c]
    (lazy-seq
      (when (>= (count c) n)
        (cons (take n c) (my-partition n (nthnext c n)))))))

(defcheck solution-f871d6b9
  (fn pttn [n coll]
    (if (<= n (count coll))
      (cons (take n coll) (pttn n (drop n coll))))))

(defcheck solution-f872c491
  (fn my-partition [n s]
    (if (>= (count s) n)
      (cons (take n s) (my-partition n (drop n s))))))

(defcheck solution-f8756ef3
  (fn f [a b]
    (when-let [b (seq b)]
      (lazy-cat (if (< (count b) a) nil (list (take a b))) (f a (drop a b))))))

(defcheck solution-f8b0facd
  (fn [n s]
    (loop [s s
           r '()]
      (if (< (count s) n)
        r
        (recur (drop n s) (concat r (list (take n s))))))))

(defcheck solution-f8f173a2
  #(second (reduce (fn[[cur acc] i]
                     (let [nw (conj cur i)]
                       (if (= (count nw) %) [[] (conj acc nw)] [nw acc])))
             [[] []] %2)))

(defcheck solution-f9132d92
  (fn [n coll]
    (loop [res []
           [a b] (split-at n coll)]
      (if (== (count a) n)
        (recur (conj res a) (split-at n b))
        res))))

(defcheck solution-f9c414
  (fn p [c xs] (if (< (count xs) c) nil (let [[l r] (split-at c xs)] (cons l (p c r))))))

(defcheck solution-f9e69dec
  (fn [pa wa]
    ((fn pas [x se acc]
       (if (>= (count se) x)
         (pas x (drop x se) (conj acc (take x se)))
         acc)) pa wa [])))

(defcheck solution-fa0883b4
  (fn my-partition
    [how-many coll]
    (loop [acc []
           curr (take how-many coll)
           remaining (drop how-many coll)]
      (if (< (count curr) how-many)
        acc
        (recur (conj acc curr)
          (take how-many remaining)
          (drop how-many remaining))))))

(defcheck solution-fa81db1c
  (fn partit [n coll]
    (if (empty? (drop (dec n) coll)) nil
                                     (lazy-seq (cons (take n coll)
                                                 (partit n (drop n coll)))))))

(defcheck solution-fa8d6c2c
  (fn [n s]
    (loop [accum [] this [] m n s s]
      (if (empty? s)
        (if (= n (count this))
          (conj accum this)
          accum)
        (if (zero? m)
          (recur (conj accum this) []                    n       s)
          (recur accum             (conj this (first s)) (dec m) (rest s)))))))

(defcheck solution-fa9eeced
  (fn par [n coll]
    (if (>= (count coll) n)
      (cons (take n coll)
        (par n (drop n coll))))))

(defcheck solution-fab4a31a
  (fn doit [n s] (if (empty? (drop (dec n) s)) '() (cons (take n s) (lazy-seq (doit n (drop n s)))))))

(defcheck solution-fae218cb
  #(for [x (range (quot (count %2) %))]
     (subvec (vec %2) (* x %) (* (inc x) %))))

(defcheck solution-fb01a8aa
  (fn [n co]
    (loop [res '() c co]
      (if (< (count c) n)
        (reverse res)
        (recur (conj res (take n c)) (drop n c))))))

(defcheck solution-fb162a32
  #(loop [ret [] coll %2]
     (if (< (count coll) %1)
       ret
       (recur (conj ret (drop-last (- (count coll) %1) coll))
         (drop %1 coll)))))

(defcheck solution-fb553770
  (fn my-partition [n coll]
    (lazy-seq
      (let [part (take n coll)]
        (when (= (count part) n)
          (cons part
            (my-partition n (drop n coll))))))))

(defcheck solution-fb6ef35f
  (fn my-partition [n col]
    (let [s (take n col)]
      (if (= n (count s))
        (cons s (my-partition n (drop n col)))))))

(defcheck solution-fb70433e
  (fn part-n [n coll]
    (let [[part rest] (split-at n coll)]
      (when (= (count part) n)
        (cons part (lazy-seq (part-n n rest)))))))

(defcheck solution-fc36a15f
  (fn [num s]
    (loop [todo s
           res []]
      (if (< (count todo) num)
        res
        (recur (drop num todo) (conj res (take num todo)))))))

(defcheck solution-fc9433d2
  (fn [n xs]
    (loop [ys xs rs []]
      (if (< (count ys) n)
        rs
        (recur (drop n ys) (concat rs [(take n ys)]))
        )
      )
    ))

(defcheck solution-fd3c9b18
  (fn [n c]
    (let [f (comp (juxt (partial take n) (partial drop n)) last)
          n? (partial = n)]
      (->> [[] c]
        (iterate f)
        next
        (map first)
        (take-while (comp n? count))))))

(defcheck solution-fd4e6aeb
  (fn [n v] (map first (drop 1 (take (inc (quot (count v) n)) (iterate  (fn [[a b]] [(take n b) (drop n b)]) [[] v]))))))

(defcheck solution-fd955d3e
  (fn [n s]
    ((fn [c s r q]
       (cond
         (= c 0) (recur n
                   s
                   (concat r (list q))
                   [])
         (empty? s) r
         :else (recur (dec c)
                 (rest s)
                 r
                 (concat q [(first s)]))))
     n s [] [])))

(defcheck solution-fdb83335
  (fn [n l]
    (map
      #(subvec (vec l) % (+ % n))
      (range 0 (* (quot (count l) n) n) n))))

(defcheck solution-fe3ca70e
  (fn [n s] (first (reduce
                     (fn f [[r c] e]
                       (let [c (conj c e)]
                         (if (= (count c) n)
                           [(conj r c) []]
                           [r c])))
                     [[] []] s))))

(defcheck solution-fe5677db
  (fn [n coll] (first (reduce (fn [[all cur] it]
                                (let [new (conj cur it)]
                                  (if (= (count new) n)
                                    [(conj all new) []]
                                    [all new]))) [[] []] coll))))

(defcheck solution-fefb3e4c
  (fn [x s]
    (loop [rs s res [] ]
      (if (> x (count rs))
        res
        (recur (drop x rs) (conj res (take x rs)))))))

(defcheck solution-ff1b8e3
  (fn f [n coll]
    (let [c (take n coll)]
      (when (= (count c) n)
        (cons c (lazy-seq (f n (drop n coll))))))))

(defcheck solution-ff3c9cd4
  (fn
    [x v]
    (loop [r [];&#26368;&#32456;&#32467;&#26524;
           m [];&#20013;&#38388;&#32467;&#26524;&#20445;&#23384;
           v v]
      (if (= (count m) x)
        (if (empty? v)
          (conj r m)
          (recur (conj r m) [(first v)] (rest v)))
        (if (empty? v)
          r
          (recur r (conj m (first v)) (rest v)))))))

(defcheck solution-ff736359
  (fn part ([n xs l] (if (>= l n)
                       (conj (part n (drop n xs) (- l n)) (take n xs))
                       '()))
    ([n xs] (part n xs (count xs))
     )))

(defcheck solution-ff7d78c9
  (fn [n s]
    (loop [part-seq []
           rem-seq s]
      (let [new-sub-seq (take n rem-seq)
            next-rem-seq (drop n rem-seq)]
        (if (= n (count new-sub-seq))
          (recur (conj part-seq new-sub-seq) next-rem-seq)
          part-seq)))))

(defcheck solution-fffa7875
  (fn f [n s]
    (let [p (take n s)]
      (when (= (count p) n)
        (cons p (f n (drop n s)))))))