(ns coal-mine.problem-43
  (:require [coal-mine.checks :refer [defcheck-43] :rename {defcheck-43 defcheck}]
            [clojure.test]))

(defcheck solution-1005a33c
  (fn n-seq [coll n]
    (map #(take-nth n (drop % coll)) (range 0 n))))

(defcheck solution-11284e7
  (fn rev-inter [coll n]
    (let [init   (vec (take n (cycle [[]])))
          fns    (take n (map (fn [i] (fn [vecs e] (assoc vecs i (conj (nth vecs i) e)))) (range)))
          length (count coll)]
      (first (reduce (fn [[vecs fns] e] [((first fns) vecs e) (rest fns)])
               [init (take length (cycle fns))]
               coll)))))

(defcheck solution-1152a420
  (fn [l n] (let [L (partition n l)]
              (for [i (range n)] (map #(nth % i) L)))))

(defcheck solution-115e5795
  (fn rev-interleave [l n]
    (reverse (loop [l l cnt 1 f '()]
               (if (> cnt n)
                 f
                 (recur (rest l) (inc cnt) (cons (take-nth n l) f)))))))

(defcheck solution-11dd0d9d
  (fn [s x] (apply map list (partition-all x s))))

(defcheck solution-11e90b13
  (fn rev-interleave [coll stride]
    (apply map list (partition-all stride coll))))

(defcheck solution-123a3173
  #(apply map vector (partition %2 %)))

(defcheck solution-12442165
  (fn [xs n]
    (for [t (range n)]
      (map #((vec xs) %) (filter #(= t (rem % n)) (range (count xs)))
        ))))

(defcheck solution-1250bb69
  (fn [xs n]
    (map #(apply concat (partition 1 n (drop % xs)))
      (range n))))

(defcheck solution-126e6f0a
  (fn [collection n]
    (->> collection
      (map-indexed (fn [idx item]
                     [idx item]))
      (group-by (fn [[idx item]]
                  (mod idx n)))
      (map (fn [[key idx-item-pairs]]
             (map second idx-item-pairs))))))

(defcheck solution-126f4b36
  (fn [x n]
    (apply list
      (map
        (fn [flag] (filter #(= (mod (.indexOf x %) n) flag) x))
        (range n)))))

(defcheck solution-128f9a13
  (fn deinterleave [x n]
    "Converts a vector x into a vector of n sublists. (count x) must be a multiple of n."
    (loop [counter 0, result (vec (repeat n []))]
      (if (< counter (count x))
        (recur (inc counter) (assoc result (mod counter n) (conj (get result (mod counter n)) (get (vec x) counter))))
        result))))

(defcheck solution-130a0677
  (fn
    [coll n]
    (lazy-seq
      (loop [a    []
             i    n
             coll coll]
        (if (= 0 i)
          a
          (recur (conj a (take-nth n coll)) (- i 1) (rest coll)))))))

(defcheck solution-13323fd6
  (fn [col n] (for [offset (range n)] (take-nth n (nthrest col offset)))))

(defcheck solution-139602ae
  (fn [raw_seq number]
    (let [len     (count raw_seq)
          iterseq (fn [seq start skip] (for [ind (range start len skip)] (nth seq ind)))]
      (loop [result [] s 0 n number]
        (if (zero? n) result
                      (recur (conj result (iterseq raw_seq s number)) (inc s) (dec n))
                      )
        )
      )
    ))

(defcheck solution-13d95be8
  (fn [l x]
    (apply map list (partition x l))))

(defcheck solution-13f9e17b
  (fn [coll n]
    (apply mapv list (partition n coll))))

(defcheck solution-140c39f5
  (fn [coll n]
    ((fn ! [p]
       (if (empty? (first p)) nil
                              (cons (map first p) (! (map rest p))))) (partition n coll))))

(defcheck solution-14140929
  (fn [x y] (map last (group-by #(mod % y) x))))

(defcheck solution-14a26e45
  (fn [c e]
    (map #(take-nth e (drop % c)) (range e))))

(defcheck solution-14e41750
  (fn [a c]
    (loop [targ [] parts (partition c a)]
      (if (empty? (first parts))
        targ
        (recur (conj targ (map first parts)) (map rest parts)))
      )))

(defcheck solution-1525c304
  (fn reverse-interleave
    [s n]
    (apply map list (partition n s))))

(defcheck solution-1528a85f
  #(partition (/ (count %1) %2) (apply interleave (partition %2 %1))))

(defcheck solution-1583ee9b
  (fn m [x n]
    (if (= [] x)
      (take n (cycle [[]]))
      (map cons (take n x) (m (drop n x) n)))))

(defcheck solution-1589d25e
  (fn [coll n]
    (let [sub-sequence (fn [offset]
                         (flatten (partition 1 n (drop offset coll))))]
      (map sub-sequence (range n)))))

(defcheck solution-15fdf0e1
  #(partition (quot (count %1) %2) (apply interleave (partition %2 %1))))

(defcheck solution-168ec6a8
  (fn [se grp]
    (let [nels (int (/ (count se) grp))]
      (for [i (range grp)]
        (for [j (range nels)
              :let [x (+ (* j grp) i)]]
          (nth se x))))))

(defcheck solution-16970d4a
  (fn [S n] (apply map list (partition n S))))

(defcheck solution-16d0e80a
  (fn [col x]
    (map
      #(map (fn [it] (second it)) %)
      (vals (group-by
              first
              (map
                #(vector (mod (first %) x) (second %))
                (sort (apply assoc {} (interleave (range 1 (inc (count col))) col)))))))))

(defcheck solution-16dc1f51
  #(map (fn [[_ y]] (map second y))
     (group-by
       (comp (fn [x] (mod x %2)) first)
       (map vector (range) %1))))

(defcheck solution-16e2b571
  (fn rev [s n]
    (loop [ans [] x 0]
      (if (= x n)
        ans
        (recur
          (concat
           ans
           [(map #(nth s %) (range x (count s) n))])
          (inc x))))))

(defcheck solution-1762fc1f
  (fn [coll n]
    (map (fn [i]
           (reverse
             (reduce (fn [r j]
                       (conj r (nth coll (+ i (* j n)))))
               '()
               (range (/ (count coll) n)))))
      (range n))))

(defcheck solution-1786c399
  #(->> % (partition %2) (apply map list)))

(defcheck solution-178c45be
  (fn rev-interleave
    [coll pos]
    (letfn [(take-items [func xs]
              (loop [acc []
                     [head & tail] xs]
                (if (empty? head)
                  acc
                  (recur (conj acc (func head)) tail))))]
      (loop [new-coll  []
             curr-coll (partition-all pos coll)]
        (if ((comp empty? flatten) curr-coll)
          new-coll
          (recur (conj new-coll (take-items first curr-coll)) (take-items rest curr-coll)))))))

(defcheck solution-17a5ef84
  (fn [s c] (apply map list (partition c s))))

(defcheck solution-17bee879
  (fn [xs i]
    (->> (map-indexed #(list %1 %2) xs)
      (group-by #(mod (first %) i,))
      (vals,)
      (map (partial map #(nth % 1)),))))

(defcheck solution-1880f94c
  (fn [seq n]
    (letfn [(inter [seq n]
              (map first (filter #(= 0 (mod (second %) n))
                           (map #(list %1 %2)
                             seq
                             (iterate inc 0)))))
            ]
      (map #(inter (drop % seq) n) (take n (iterate inc 0))))))

(defcheck solution-18b88e6
  (fn [xs n] (apply map list (partition n xs))))

(defcheck solution-18dd1294
  (fn [seq n]
    (let [parts (partition n seq)]
      (map (fn [n] (map #(nth % n) parts)) (range 0 n)))))

(defcheck solution-190113f
  (fn [col nelem]
    (let [
          the-range (range 0 (count col))
          ]
      (loop [last-index (dec nelem) res '()]
        (if (>= last-index 0)
          (recur (dec last-index) (conj res (map #(nth col %2) col (filter #(= last-index (mod % nelem)) the-range))))
          res)
        ))

    ))

(defcheck solution-198ae817
  (fn [xs n]
    (apply map (fn [& args] args) (partition n n xs))))

(defcheck solution-19906353
  (fn reverse-interleave
    [s n]
    (sort-by first < (map #(val %) (reduce #(merge-with concat %1 %2) (map-indexed (fn [idx itm] (hash-map (mod (+ 1 idx) n) [itm])) s))))))

(defcheck solution-19a9ada6
  (fn [s n]
    (map (fn [off] (apply concat (partition 1 n (drop off s)))) (range n))))

(defcheck solution-19c30d2
  (fn [coll n]
    (->> coll
      (map-indexed #(vector %1 %2))
      (group-by #(mod (first %) n))
      (map #(map second (second %))))))

(defcheck solution-1af427bb
  (fn [seq count]
    (apply (partial map list) (partition count seq))))

(defcheck solution-1b56ce22
  (fn [s size] (map #(take-nth size (drop % s)) (range size))))

(defcheck solution-1bbe4a14
  ;(fn [coll n]
  ;   (letfn [(group-coll-by [f coll]
  ;                         (let [vals (distinct (map f coll))]
  ;                          (reduce conj [] (map (fn [v] (filter #(= v (f %)) coll))  vals) )))]
  ;  (for [ss (group-coll-by #(rem (first %) n) (map-indexed vector coll))] (map second ss))))

  (fn [coll n]
    (map #(map second %) (vals (group-by #(mod (first %) n) (map-indexed vector coll))))))

(defcheck solution-1ca4f3a0
  (fn [v n] (apply map (cons vector (partition n v)))))

(defcheck solution-1d63aa8e
  (fn [l n]
    (vals (group-by #(rem (.indexOf l %) n) l))))

(defcheck solution-1d782163
  (fn [s n] (map #(flatten (partition 1 n (drop % s))) (range n))))

(defcheck solution-1d9151
  (fn [ss n]
    (loop [s ss r (vec (repeat n '())) i 0]
      (if (empty? s)
        (map reverse r)
        (recur
          (rest s)
          (assoc r (mod i n) (cons (first s) (nth r (mod i n))))
          (inc i))))))

(defcheck solution-1debf7ec
  (fn [coll n]
    (partition (/ (count coll) n)
      (apply interleave (partition n coll)))))

(defcheck solution-1e10572a
  (fn [x y] (apply map (fn [& r] r)
              (partition y x)
              )
    ))

(defcheck solution-1e519e27
  (fn [xs n]
    (map (fn [i]
           (filter #(not (nil? %))
             (map-indexed #(when (= i (mod %1 n)) %2) xs))) (range n))))

(defcheck solution-1ecd091d
  (fn [coll parts]
    (apply map list (partition parts coll))))

(defcheck solution-1f349846
  (fn [ls n]
    (let [len (count ls)
          ks  (quot len (quot len n))
          lss (partition ks ls)]
      (loop [an [] lss lss]
        (if (empty? (first lss))
          an
          (recur (conj an (map first lss))
            (map rest lss)))))))

(defcheck solution-201df326
  (fn [xs n]
    (let [idx-itm      (map-indexed (fn [idx itm] [idx itm]) xs)
          groups       (group-by (fn [[idx itm]] (rem idx n)) idx-itm)
          subsequences (map #(map second %) (vals groups))]
      subsequences)))

(defcheck solution-205040aa
  (fn f ([x n]
         (f x n (repeat n []) 0))
    ([x n r c]
     (if (= c (count x))
       r
       (recur x n (concat (take (mod c n) r) [(conj (nth r (mod c n)) (nth x c))] (drop (inc (mod c n)) r)) (inc c))))))

(defcheck solution-2058a447
  (fn [col n]
    (loop [i n s []]
      (if (> i 0)
        (recur (dec i) (conj s (for [j (range (count col)) :when (= 0 (rem (+ j i) n))] ((vec col) j))))
        s))))

(defcheck solution-208cd099
  (fn [s n]
    (for [m (range n)]
      (flatten (partition 1 n (drop m s))))))

(defcheck solution-224a0f20
  (fn rev-int [v n]
    (apply map vector (partition n v))))

(defcheck solution-22cc0932
  (fn revint [s n]
    (loop [i      0
           remain s
           m      (sorted-map)]
      (if (empty? remain) (vals m)
                          (let [newi  (mod (inc i) n)
                                [nextelt & restelts] remain
                                lu    (vec (m i))
                                newlu (conj lu nextelt)
                                newm  (assoc m i newlu)]
                            (recur newi restelts newm))))))

(defcheck solution-23afc57b
  #(for [i (range %2)] (map (fn [x] (nth x i)) (partition %2 %))))

(defcheck solution-23e7e17a
  (fn [s n]
    (map second (group-by #(mod (.indexOf s %) n) s))
    ))

(defcheck solution-24145dd2
  (fn [xs n]
    (map #(take-nth n (drop % xs)) (range n))))

(defcheck solution-241f91fb
  (fn [c n]
    (loop [r [] c c i n]
      (if (zero? i)
        r
        (recur (conj r (take-nth n c)) (rest c) (dec i))))))

(defcheck solution-2437054b
  (fn [xs n]
    (map #(take-nth n (drop % xs)) (range n))))

(defcheck solution-245d86f7
  (fn [s n]
    (map #(take-nth n (nthrest s %)) (range n))))

(defcheck solution-2463f3f3
  (fn [s n] (for [x (range n 0 -1)]
              (take-nth n (drop (- n x) s)))))

(defcheck solution-24a22242
  (fn [c n] (map #(take-nth n (drop % c)) (range n))))

(defcheck solution-2515832
  (fn [col n]
    (->> (partition n col)
      (apply map #(apply list %&)))))

(defcheck solution-252c4c13
  (fn [l n]
    (apply map vector (partition n l))))

(defcheck solution-2563e8e2
  (fn [coll n]
    (apply map list
      (partition n coll))))

(defcheck solution-2619f17d
  (fn k [col m] (apply map list ((fn s [se n] (when (>= (count se) n) (cons (take n se) (s (drop n se) n)))) col m))))

(defcheck solution-265025ea
  #(apply map (partial conj []) (partition %2 %)))

(defcheck solution-2681885b
  (fn [xs n]
    (let [nall     (count xs)
          ningroup (/ nall n)]
      (for [i (range n)]
        (for [j (range ningroup)]
          (nth xs (+ i (* j n))))))))

(defcheck solution-2746e585
  #(map (fn [i] (filter (fn [x] (not (nil? x)))
                  (map-indexed (fn [j v] (if (= (mod j %2) 0) v))
                    (drop i %1))))
     (range 0 %2)))

(defcheck solution-276d43f9
  (fn [coll step]
    (map #(take-nth step (drop (- step %) coll)) (range step 0 -1))))

(defcheck solution-2770cc29
  (fn rev-int [coll n]
    (map #(take-nth n (drop % %2)) (range n) (iterate identity coll))))

(defcheck solution-28806c00
  (fn [li n] (for [x (range n)] (take-nth n (nthnext li x)))))

(defcheck solution-288347ce
  (fn [c x] (for [i (range x)] (take-nth x (drop i c)))))

(defcheck solution-28cce29d
  (fn f [x n]
    (loop [r (repeat []) s x]
      (if (empty? s)
        r
        (recur (map conj r (take n s)) (drop n s))))))

(defcheck solution-28f65696
  (fn [seq n]
    (apply map (cons list (partition n seq)))))

(defcheck solution-28fcbd17
  #(loop [n %2 v %1 o []]
     (if (zero? n)
       o
       (recur (dec n) (next v) (conj o (take-nth %2 v)))
       )
     ))

(defcheck solution-29aea35b
  (fn [coll x] (map (fn [n] (map #(nth % n) (partition-all x coll))) (range x))))

(defcheck solution-2a56d10a
  (fn [xs n]
    (reduce-kv #(update-in %1 [(rem %2 n)] conj %3) (vec (repeat n [])) (vec xs))
    ))

(defcheck solution-2accf5a2
  (fn [s n]
    (map #(take-nth n (nthrest s (- % 1))) (range 1 (inc n)))))

(defcheck solution-2adfbcd
  (fn [xs n]
    (for [i (range n)]
      (keep-indexed #(if (zero? (mod (- %1 i) n)) %2) xs))))

(defcheck solution-2b2717fc
  (fn [l n] (map (fn [x] (map #(nth l %) (range x (count l) n))) (range 0 n))))

(defcheck solution-2b3fb70b
  (fn [col n] (map #(map first %) (partition-by second (sort-by second (map list col (cycle (range n))))))))

(defcheck solution-2b5867d8
  (fn [xs n] (apply map list (partition-all n xs))))

(defcheck solution-2c5e39df
  (fn [xs cnt]
    (for [d (range cnt)]
      (take-nth cnt (drop d xs)))))

(defcheck solution-2cc6cf5f
  (fn [l n]
    (for [x (range n)]
      (take-nth n (drop x l)))))

(defcheck solution-2ce7639b
  (fn [c n] (apply map (fn [x y & more] (concat [x y] more)) (partition n c))))

(defcheck solution-2d3966b0
  (fn [s n]
    (map #(take-nth n (drop % s)) (range n))))

(defcheck solution-2d491362
  (fn rev-inter [x n]
    (partition (/ (count x) n) (apply interleave (partition n x)))))

(defcheck solution-2d71bd2b
  (fn [sq n]
    (letfn [(vassoc-in [m [k & ks] v]
              (if ks
                (assoc m k (vassoc-in (get m k []) ks v))
                (assoc m k v)))

            (aux [[head & tail] idx acc]
              (cond
                (nil? head)
                acc

                :else
                (recur tail (mod (inc idx) n)
                  (vassoc-in acc [idx (count (get acc idx))] head))))]

      (aux sq 0 (vec (repeat n []))))))

(defcheck solution-2dcb55c0
  (fn ril [sq vl]
    (for [vlist (vals
                  (group-by first
                    (map-indexed
                      (fn [idx sqval]
                        [(rem (inc idx) vl) sqval])
                      sq)))]
      (map second vlist))))

(defcheck solution-2df29b49
  (fn deinterleave [coll n]
    (for [i (range n)] (take-nth n (drop i coll)))))

(defcheck solution-2e0ea8be
  (fn [ls n]
    (for [i (range n), l [ls]] (take-nth n (drop i l)))))

(defcheck solution-2e2b93a9
  (fn [coll n]
    (apply map list (partition n coll))))

(defcheck solution-2e62d08b
  (fn [coll n]
    (reduce
      (partial map
        #(concat (if (seq? %1) %1 [%1]) [%2]))
      (partition n coll))))

(defcheck solution-2e972ac6
  (fn reverse-interleave [coll n]
    (->> (partition n coll)
      (apply map list))))

(defcheck solution-2fb81260
  (fn [s n]
    (for [i (range n)]
      (map #(nth % i) (partition n s)))))

(defcheck solution-303ca5e
  (fn [s c]
    (partition (/ (count s) c) (apply interleave (partition c s)))))

(defcheck solution-30b966fa
  (fn [xs n] (vals (group-by #(mod % n) xs))))

(defcheck solution-30cbd6fc
  (fn [seq n] (apply map vector (partition n seq))))

(defcheck solution-30cf4e8f
  (fn [xs n]
    (loop [i 0 rs xs ss (into (sorted-map) (for [i (range n)] [i []]))]
      (if (empty? rs)
        (vals ss)
        (let
         [id (mod i n)
          vs (get ss id)
          r  (first rs)]
          (recur (inc i) (rest rs) (assoc ss id (conj vs r)))
          )
        )
      )
    ))

(defcheck solution-318e8d11
  (fn rinterleave [s g]
    (let [index (range)
          gs    (quot (count s) g)]
      (map (fn [e] (map second e)) (vals (group-by #(mod (first %) g) (map vector index s)))))))

(defcheck solution-31cd3efb
  (fn reverse-interleave [coll n]
    (->> coll
      (partition n)
      (apply interleave)
      (partition (quot (count coll) n)))))

(defcheck solution-31d2565c
  (fn [s n]
    (map (fn [l] (map last l))
      (vals
        (group-by
          (fn [l] (mod (first l) n))
          (map-indexed list s))))))

(defcheck solution-322ffbef
  (fn
    [s n]
    (map
      #(map second (second %))
      (group-by
        #(rem (first %) n)
        (map-indexed vector s)))))

(defcheck solution-323c744f
  (fn [c n]
    (let [s (vec c)
          t (count c)]
      (for [i (range n)]
        (map #(get s %) (range i t n))))))

(defcheck solution-325eadb9
  (fn [coll n] (let [split_count (/ (count coll) n)]
                 (partition-all split_count (apply interleave (partition-all n coll))))))

(defcheck solution-3278a48f
  (fn ri [s n]
    (loop [rv (repeat n []) tmp s]
      (if (empty? tmp)
        rv
        (recur (map conj rv (take n tmp)) (drop n tmp))))))

(defcheck solution-32dbf999
  (fn [c n]
    (let [map-lvp (fn [as vs] (map (fn [a v] (cons v a)) as vs))]
      (map reverse (reduce map-lvp (repeat n '()) (partition n c))))))

(defcheck solution-33d2cfdf
  (fn [x1 x2] (partition (quot (count x1) x2) (for [x (range x2) y (range x (count x1) x2)] (nth x1 y)))))

(defcheck solution-342bb59e
  (fn
    [cs n]
    (for [i (range n)]
      (for [j (range (count cs))
            :when (= (mod j n) i)]
        (nth cs j)))))

(defcheck solution-346f0f87
  (fn [coll a]
    (map #(take-nth a (drop % coll)) (range 0 a))
    ))

(defcheck solution-347a3bc9
  #(->> %1
     (map-indexed vector)
     (group-by (fn [[i v]] (rem i %2)))
     sort
     (map (fn [[i vs]] (map second vs)))))

(defcheck solution-352579aa
  (fn revIn [x n] (apply map list (partition n x))))

(defcheck solution-352d4e3e
  (fn [xs n]
    (letfn [(seqify [m]
              (map second (sort-by first (seq m))))]
      (loop [xs xs
             ys (into {} (map #(vector % []) (range 0 n)))
             k  0]
        (if (empty? xs)
          (seqify ys)
          (recur (rest xs)
            (update-in ys [(mod k n)] #(conj % (first xs)))
            (inc k)))))))

(defcheck solution-3534fe81
  (fn [a-seq n]
    (let [part-by-idx (->> a-seq
                        (map-indexed #(vector (rem %1 n) %2))
                        (group-by #(first %))
                        (vals))]
      (for [x part-by-idx]
        (for [y x]
          (second y))))))

(defcheck solution-377fdbbd
  (fn [items parts] (map #(map first %) (map second (group-by #(mod (second %) parts) (map list items (range)))))))

(defcheck solution-37fc4434
  (fn
    [lst n]
    (let [parts  (partition n lst)
          length (count (first parts))]
      (apply map vector parts))))

(defcheck solution-38210d0b
  (fn [s n] (partition (/ (count s) n) (apply interleave (partition n s)))))

(defcheck solution-38c92f2
  (fn [c n] (map (fn [[k v]] (map (fn [[i vv]] vv) v)) (group-by (fn [[g x]] g) (map-indexed #(list (mod % n) %2) c)))))

(defcheck solution-394360a8
  (fn [s n]
    (loop [ps (partition n s)
           rs (repeat n [])]
      (if (not (seq ps))
        rs
        (recur (next ps)
          (map conj rs (first ps)))))))

(defcheck solution-395226bd
  (fn [s n] (map (fn [r] (map #(nth s %) r)) (for [x (range n)] (range x (count s) n)))))

(defcheck solution-39bccad3
  (fn [xs n]
    (reduce (fn [agg x]
              (concat (rest agg)
                      [(concat (first agg) [x])]))
      (repeat n '())
      xs)))

(defcheck solution-3ada6970
  (fn [lst n]
    (let [lst2 (map vector lst (cycle (range n)))]
      (vals (sort
              (reduce
                (fn [m [v i]] (assoc m i (concat (m i) [v]))) {} lst2)))
      )
    ))

(defcheck solution-3ae4d4bb
  (fn parcel--group
    [coll n] {:pre [(integer? n), (pos? n)]}
    (->> coll
      (group-by #(mod % n))
      vals
      (map seq))))

(defcheck solution-3b906a9f
  (fn [coll n]
    (loop [r     coll
           c     0
           final (into [] (repeat n []))]
      (if (empty? r)
        final
        (let [c'      (mod (inc c) n)
              new-seq (conj (nth final c) (first r))
              final'  (assoc final c new-seq)]
          (recur (rest r) c' final'))))))

(defcheck solution-3bf5f2c0
  (fn f [v n]
    (for [i (range n)]
      (loop [ans [] j i]
        (if (>= j (count v))
          ans
          (recur (conj ans (nth v j))
            (+ j n)))))))

(defcheck solution-3c1211de
  (fn [coll x]
    (for [i (range x)]
      (take-nth x (drop i coll)))))

(defcheck solution-3cd6b828
  (fn f [coll n]
    (reduce (fn [ret [k v]]
              (assoc-in ret [(mod k n) (quot k n)] v))
      (vec (repeat n (vec (repeat (quot (count coll) n) nil))))
      (map vec (zipmap (range) coll)))))

(defcheck solution-3d05db11
  (fn [col n] (vals (group-by #(rem % n) col))))

(defcheck solution-3d32b663
  (fn rev-inter
    ([l x] (rev-inter l x 0 (range (count l)) []))
    ([l x m ind resp]
     (if (= m x)
       resp
       (let [indaux (filter #(= m (mod % x)) ind)
             lr     (map #(nth l %) indaux)]
         (rev-inter l x (inc m) ind (conj resp lr)))))))

(defcheck solution-3d838fd2
  (fn [v n] (vals (group-by #(mod % n) v))))

(defcheck solution-3dfb6572
  (fn [c n]
    (map
      #(take-nth n (drop % c))
      (range n))))

(defcheck solution-3e256e7b
  (fn t [lst n]
    (map
      #(take-nth n (drop % lst))
      (range n))))

(defcheck solution-3e997893
  (fn rev-interleave [lst n]
    (let [every-nth (fn [lst n]

                      (filter
                        #(not (nil? %))
                        (map-indexed
                          #(if (zero? (mod %1 n)) %2 nil)
                          lst))
                      )]

      (for [i (range n)] (every-nth (drop i lst) n)))))

(defcheck solution-3ecbab3a
  (fn [coll n] (map #(take-nth n (nthrest coll %)) (range n))))

(defcheck solution-3f3bfdcf
  (fn rleave [items n]
    (apply map list (partition n items))))

(defcheck solution-3f43b9
  (fn [coll n]
    (let [c (/ (count coll) n)]
      (map
        (fn [is] (map #(nth coll %) (take c is)))
        (map
          (fn [i] (map #(+ i (* n %)) (range (count coll))))
          (range 0 n))))))

(defcheck solution-3f689676
  (fn [v n]
    (apply map list (partition n v))))

(defcheck solution-3ff6a523
  #(reduce
     (fn [s x]
       (map conj s x))
     (repeat %2 [])
     (partition %2 %1)))

(defcheck solution-4057bff2
  (fn [coll n]
    (let [in (partition n coll)]
      (for [i (range n)]
        (map #(nth % i) in)))))

(defcheck solution-40d5ddf6
  (fn [coll, n] (map #(take-nth n (drop % coll)) (range n))))

(defcheck solution-40d77ab5
  (fn [items number]
    (loop [items items current 0 result []]
      (if (empty? items)
        result
        (recur (rest items)
          (inc current)
          (let [index (mod current number) item (first items)]
            (if (<= (count result) index)
              (conj result [item])
              (assoc result index (conj (nth result index) item)))))))))

(defcheck solution-410872ff
  (fn
    [li prt]
    (apply
      map vector
      (partition prt li))))

(defcheck solution-4111c8be
  (fn revese-interleave [x n]
    (let [ix   (range n)
          len  (count x)
          step n]
      (map (fn [i] (map #(nth x %) (range i len step))) ix))))

(defcheck solution-41d6b1e3
  #(partition-all (/ (count %1) %2) (apply interleave (partition-all %2 %1))))

(defcheck solution-41df763e
  (fn [a-seq n]
    (loop [ret   []
           ss    a-seq
           round 0]
      (if (= round n)
        ret
        (recur (into ret (list (take-nth n ss)))
          (rest ss)
          (inc round))))))

(defcheck solution-41f6ceab
  (fn [xs n] (map #(take-nth n %) (map #(% xs) (map #(partial drop %) (range n))))))

(defcheck solution-42813c26
  (fn [xs n]
    (letfn [(add [l [val index]]
              (map-indexed #(if (not= %1 index) %2 (conj %2 val)) l))]
      (reduce add (repeat n []) (map list xs (cycle (range n)))))))

(defcheck solution-435dfc36
  (fn [coll cnt]
    (let [mapped   (map-indexed (fn [i v] [(mod i cnt) v]) coll)
          base-acc (repeat cnt ())]
      (reduce
        (fn [acc x]
          (let [index   (first x)
                value   (last x)
                start   (take index acc)
                trgt    (nth acc index)
                updated (concat trgt (list value))
                end     (drop (+ 1 index) acc)]
            (concat start (list updated) end)))
        base-acc mapped))))

(defcheck solution-439b530
  (fn [c, n] (for [i (range n)] (mapcat #(if (= i (mod % n)) (vec [(get (vec c) %)])) (range 0 (count c))))))

(defcheck solution-43d53953
  (fn [coll n]
    (let [part (/ (count coll) n)]
      (for [i (range 0 n)]
        (for [j (range 0 part)]
          (nth coll (+ i (* j n)))
          )
        )
      )
    ))

(defcheck solution-44110910
  (fn reverse-interleave [coll step]
    (let [c (map list coll (range))]
      (map #(map first %) (vals (group-by #(mod (second %) step) c))))))

(defcheck solution-4434f75d
  (fn reverse_interleave [items x]
    (apply map list (partition x items))))

(defcheck solution-4495b1ee
  (fn [c j]
    (map (fn [x]
           (keep-indexed #(when (= (rem % j) x) %2) c)) (range j))))

(defcheck solution-449d13f8
  (fn [l n]
    (map #(map second %) (vals (group-by #(rem (first %) n) (map-indexed #(list %1 %2) l))))))

(defcheck solution-45628d73
  (fn rev [coll parts]
    (let [len (/ (count coll) parts)]
      (->> coll
        (map-indexed #(vector %2 (mod %1 parts)))
        (group-by last)
        (apply vector)
        (sort-by first)
        (map last)
        (map #(map (fn [[a b]] a) %))
        ))))

(defcheck solution-459dd77a
  (fn [xs n]
    (apply (partial map vector) (partition n xs))))

(defcheck solution-462c2d96
  (fn [s n] (let [l (map vector s (cycle (range n)))] (map #(for [x l :when (= (second x) %)] (first x)) (range n)))))

(defcheck solution-467e43b5
  (fn spl [col n] (partition (/ (count col) n) (apply interleave (partition n col)))))

(defcheck solution-468740d3
  (fn [l n]
    (map (fn [y]
           (map #(nth l %) (filter #(= (mod % n) y) (range (count l))))) (range n))))

(defcheck solution-46a5f908
  #(loop [i 0 [f & r] %1 res []]
     (if (nil? f) res
                  (recur
                    (mod (inc i) %2) r
                    (assoc res i
                               (conj
                                 (if (<= (count res) i) []
                                                        (nth res i))
                                 f))))))

(defcheck solution-473fc6e0
  (fn [s n] (->> (partition n s)
              (apply map list))))

(defcheck solution-476af873
  (fn [s n]
    (let [ps (partition n s)
          it (range n)]
      (map (fn [nn]
             (reduce #(conj %1 %2) [] (map #(nth % nn) ps))
             )
        it)
      )))

(defcheck solution-47806410
  (fn [s x]
    (vals (group-by #(mod % x) s))))

(defcheck solution-47c83f59
  (fn rev-inter [lat n]
    (partition (quot (count lat) n) (apply interleave (partition n lat)))))

(defcheck solution-48040f34
  (fn [coll n]
    (apply map (fn [& items] items) (partition n coll))))

(defcheck solution-480906f7
  (fn [iseq num]
    (for [keep (range num)]
      (for [y (range (count iseq))
            :when (= (mod y num) keep)]
        (nth iseq y))
      )
    ))

(defcheck solution-485670d0
  (fn [coll n]
    (map #(map (fn [xs] (last xs)) %)
      (vals
        (group-by #(mod (first %) n)
          (map-indexed #(list % %2) coll))))))

(defcheck solution-485f3428
  #(for [x (range %2)]
     (map (fn [[a b]] b)
       (filter
         (fn [[i y]] (= (mod i %2) x))
         (map-indexed vector %)))))

(defcheck solution-48f78b41
  (fn [s n]
    (map (partial map first)
      (vals (group-by last
              (partition 2 (interleave s (-> n range cycle))))))))

(defcheck solution-498abc46
  (fn [s n]
    (reduce #(map conj %1 %2) (repeat n []) (partition n s))))

(defcheck solution-49f4516a
  (fn [coll n]
    (loop [x n result [] c coll]
      (if (> x 0)
        (recur (dec x) (conj result (take-nth n c)) (rest c))
        result))))

(defcheck solution-4a5d682c
  #(loop [col (partition %2 %1) res []]
     (if-not (every? seq col)
       res
       (recur (map rest col) (conj res (map first col))))))

(defcheck solution-4a84adf5
  (fn de-interleave [seq n]
    (apply map list (partition n seq))))

(defcheck solution-4b287ef9
  #(for [x (range %2)] (flatten (partition 1 %2 (drop x %)))))

(defcheck solution-4b5461aa
  (fn [coll n]
    (loop [x 1 res []]
      (if (= x (inc n)) res
                        (recur (inc x) (conj res (keep-indexed (fn [idx item] (if (= (mod (inc idx) n) (mod x n)) item nil)) coll)))))))

(defcheck solution-4b838666
  (fn [coll n]
    (map
      (fn [i]
        (keep-indexed (fn [idx item] (if (= (mod idx n) 0) item)) (drop (dec i) coll)))
      (range 1 (inc n)))))

(defcheck solution-4bc8970
  (fn [coll n]
    (apply map vector (partition n coll))))

(defcheck solution-4c0dbd2e
  (fn [v n]
    (for [offset (range n)]
      (for [idx (range offset (count v) n)]
        (nth v idx)))))

(defcheck solution-4ceebf8c
  (fn [xs size] (apply map list (partition size xs))))

(defcheck solution-4d42d1c0
  (fn [a n] (vals (group-by #(mod % n) a))))

(defcheck solution-4d6522aa
  #(apply (partial map list) (partition %2 %)))

(defcheck solution-4dcef752
  (fn [coll x]
    (partition
      (/ (count coll) x)
      (apply interleave (partition x coll)))))

(defcheck solution-4e5b60ea
  #(apply map (cons vector (partition %2 %))))

(defcheck solution-4f2c1163
  (fn x [seqq n]
    (apply map list (partition-all n seqq))))

(defcheck solution-4fd4446
  (fn [coll n]
    (apply map list (partition n coll))))

(defcheck solution-5019a4df
  (fn prob43 [col n]
    (apply map list (partition n col))))

(defcheck solution-50514cff
  (fn [s n] (map #(map first (partition 1 n (drop % s))) (range n))))

(defcheck solution-506c42db
  (fn [coll x] (map #(rest (take-nth x (concat (repeat (- x %) 0) coll))) (range x))))

(defcheck solution-50a01163
  (fn [coll n] (apply map (fn [& args] args) (partition n coll))))

(defcheck solution-51888592
  (fn revint [sq n] (if (empty? sq) (repeat n '()) (map cons (take n sq) (revint (drop n sq) n)))))

(defcheck solution-51bedee7
  #(partition (quot (count %) %2) (apply interleave (partition %2 %))))

(defcheck solution-51d71122
  #(apply map vector (partition %2 %1)))

(defcheck solution-51f8c5d3
  (fn [x n]
    (loop [x   x
           res (vec (repeat n []))
           i   0
           l   0]
      (if (nil? (first x))
        res
        (if (= i (- n 1))
          (recur (rest x) (assoc-in res [i l] (first x)) 0 (inc l))
          (recur (rest x) (assoc-in res [i l] (first x)) (inc i) l))))))

(defcheck solution-520bb48
  (fn [s n]
    (partition (/ (count s) n)
      (apply interleave (partition n s)))))

(defcheck solution-5355d6af
  (fn [coll paso]
    (apply map vector (partition paso coll))))

(defcheck solution-535b9395
  (fn [col n] (vals (group-by #(mod % n) col))))

(defcheck solution-535e2a09
  #(apply map list (partition %2 %)))

(defcheck solution-53b97038
  (fn [xs n] (map-indexed #(apply concat (partition 1 n (drop % %2))) (repeat n xs))))

(defcheck solution-53f8d8af
  #(loop [l %1
          a (repeat %2 [])]
     (if (empty? l)
       a
       (recur (drop %2 l) (map conj a (take %2 l))))))

(defcheck solution-53fbfe50
  (fn [s pars]
    (->> (partition pars s)
      (apply map list))))

(defcheck solution-5413b7d1
  (fn reverse-interleave [xs n]
    (map
      (fn [i]
        (map
          (fn [j]
            (nth
              xs
              (+ i (* j n))
              )
            )
          (range (/ (count xs) n))
          )
        )
      (range n)
      )
    ))

(defcheck solution-5414e0ad
  (fn ri [coll n]
    (->>
      coll
      (group-by #(rem (.indexOf coll %) n))
      (vals))))

(defcheck solution-5440e9cf
  (fn [coll p]
    (apply list (map #(take-nth p (drop % coll)) (range p)))))

(defcheck solution-544a9895
  (fn [s n]
    (map #(take-nth n (drop % s)) (range n))))

(defcheck solution-546f3c56
  (fn [c i] (for [x (range 0 i)] (take-nth i (drop x c)))))

(defcheck solution-54b5ae5d
  #(apply map vector (partition %2 %2 %1)))

(defcheck solution-54c355fc
  (fn
    [s n]
    (reduce #(map conj % %2) (repeat n []) (partition n s))))

(defcheck solution-54ca3c45
  (fn disiterleave [acoll n]
    (reduce (partial map conj)
      (repeat n [])
      (partition n acoll))))

(defcheck solution-552365ed
  (fn [coll n]
    (let [f (fn find-group [coll]
              (if (empty? coll) nil (cons (first coll) (lazy-seq (find-group (drop n coll))))))]
      (map f (take n (iterate rest coll))))))

(defcheck solution-5561dc4f
  (fn [l n]
    (apply (partial map list) (partition-all n l))))

(defcheck solution-5751c2a
  (fn [c s] (map #(map (fn [n] (nth c n)) (range % (count c) s)) (range s))))

(defcheck solution-5782d92
  #(apply mapv vector (partition %2 %1)))

(defcheck solution-578a1f47
  (fn [xs n]
    (loop [items xs groups 0 acc []]
      (if (= groups n)
        acc
        (recur (rest items) (inc groups) (conj acc (take-nth n items)))))))

(defcheck solution-57b6922a
  (fn [input n]
    (let [a0 (repeat n [])]
      (loop [l input i 0 a a0]
        (if (empty? l)
          a
          (recur (rest l)
            (inc i)
            (let [bag (rem i n)]
              (concat (take bag a)
                      (vector (conj (nth a bag) (first l)))
                      (drop (inc bag) a)))))))))

(defcheck solution-57eb6acb
  #(for [i (range %2)] (take-nth %2 (drop i %1))))

(defcheck solution-58ad4a80
  #(apply map (cons list (partition %2 %))))

(defcheck solution-59ca2317
  (fn maptest [coll n]
    (map (fn [x] (map second x))
      (map (fn [x] (second x))
        (seq (group-by first (map-indexed (fn [idx itm] [(mod idx n) itm]) coll)))))))

(defcheck solution-59f3e86
  (fn rev-interleave [s n]
    (letfn [(at-index [v i n]
              (let [c (count v)]
                (for [j (range 0 (count v))
                      :when (= i (mod j n))]
                  (v j))))]
      (let [v (vec s)]
        (for [i (range 0 n)]
          (at-index v i n))))))

(defcheck solution-59fdc092
  (fn rev-interleave [xs n]
    (reduce (partial map conj)
      (repeat [])
      (partition n xs))))

(defcheck solution-5a0c2fb6
  (fn ai [xs n]
    (loop [i 0, head []]
      (if (= i n)
        head
        (recur (inc i) (conj head (take-nth n (nthnext xs i))))))))

(defcheck solution-5a27f99
  (fn [coll s]
    (map #(take-nth s %) (map (fn [x] (take (count coll) (drop x (cycle coll)))) (range s)))))

(defcheck solution-5a3f6b47
  (fn [s n]
    (reduce
      (fn [v [x p]]
        (assoc v p (conj (get v p) x)))
      (vec (repeat n []))
      (map #(identity [%1 %2]) s (cycle (range n))))))

(defcheck solution-5a5d34fc
  (fn [xs n] (map #(take-nth n (drop % xs)) (range n))))

(defcheck solution-5a7df916
  (fn R [l n]
    (cond (empty? l) (repeat n [])
          :else (map cons (take n l) (R (drop n l) n)))))

(defcheck solution-5af8fcf6
  (fn [ls n] (loop [res [] cur (partition n ls)]
               (if (every? empty? cur) res
                                       (recur (conj res (map first cur)) (map rest cur))))))

(defcheck solution-5b3463fd
  (fn [l n]
    (map #(map first %)
      (vals (group-by #(second %)
              (map list l (cycle (range n))))))))

(defcheck solution-5b8fd1dc
  #(for [off (range %2)] (take-nth %2 (drop off %1))))

(defcheck solution-5baad76
  (fn [xs n]
    (apply                                                  ;((1 3 5)(2 4 6))
      map                                                   ;((1)(2))((3)(4))((5)(6))
      list                                                  ;((1)(2))
      (partition n xs)                                      ;((1 2) (3 4) (5 6))
      )))

(defcheck solution-5bb3eac7
  (fn [seq cnt] (map #(take-nth cnt (drop % seq)) (range cnt))))

(defcheck solution-5bc85a16
  (fn interleave' [xs n]
    (let [
          transpose #(apply map list %)]
      (transpose (partition n n xs)))))

(defcheck solution-5c0af95
  (fn [S n]
    (for [x (range n)]
      (for [y (range (quot (count S) n))]
        (nth S (+ x (* n y)))
        )
      )

    ))

(defcheck solution-5cc37c8d
  (fn [myCol x] (let [recursor (fn recurs [out in] (if (> (count out) (count in)) out (recurs (apply list (mapv #(concat %1 (list %2)) out (take x in))) (drop x in))))] (recursor (replicate x '()) myCol))))

(defcheck solution-5d3bb67
  (fn [col n]
    (apply map list (partition n col))
    ))

(defcheck solution-5d65b79b
  (fn [l c]
    (for [n (range c)] (map #(nth % n) (partition c l)))))

(defcheck solution-5e65b435
  (fn [s n]
    (for [i (range n)]
      (map #(nth % i) (partition n s)))))

(defcheck solution-5ea4a793
  #(for [i (range %2)] (take-nth %2 (nthrest %1 i))))

(defcheck solution-5ebe07fa
  (fn [l count]
    (loop [l l c count acc []]
      (if (zero? c)
        acc
        (recur (rest l) (dec c) (conj acc (take-nth count l)))))))

(defcheck solution-5ef576b1
  (fn [a n]
    (loop [out (repeat n []) in a]
      (if (empty? in)
        out
        (recur (map #(conj (vec %1) %2) out (take n in)) (drop n in))))))

(defcheck solution-5efb26d9
  (fn [coll n]
    (loop [input  coll
           i      0
           result (repeat n [])]
      (if (empty? input) result
                         (recur (rest input) (rem (inc i) n)
                           (map-indexed #(if (= % i) (conj %2 (first input)) %2) result))))))

(defcheck solution-5fd9e25c
  (fn [coll n]
    (for [i (range n)]
      (map #(nth coll %) (map #(+ % i) (range 0 (count coll) n))))))

(defcheck solution-6017f3ab
  ;#(map take-nth (repeat %2 %2) (iterate rest %1))
  #(apply map list (partition %2 %1)))

(defcheck solution-604780a8
  (fn f [l n]
    (if (= '() l)
      (repeat n '())
      (map cons (take n l) (f (drop n l) n)))))

(defcheck solution-605f684
  (fn [coll n]
    (for [i (range 0 n)]
      (doall (take-nth n (drop i coll))))))

(defcheck solution-60766f4
  (fn [xs n] (for [a (range n)] (take-nth n (drop a xs)))))

(defcheck solution-60c74911
  (fn [x y]
    (map
      (fn [i] (map first (partition-all y (drop i x))))
      (range 0 y))))

(defcheck solution-61516be
  (fn [coll n]
    (for [i (range n)]
      (apply concat (partition-all 1 n (drop i coll))))))

(defcheck solution-61c5dd07
  (fn [col x]
    (apply map vector (partition x col))))

(defcheck solution-624e1cff
  (fn [s num] (let [ivec (map-indexed #(vector (mod % num) %2) s)]
                (vals (sort (reduce
                              (fn [ret x]
                                (let [k (first x), v (last x)]
                                  (assoc ret k (conj (get ret k []) v))))
                              {}
                              ivec))))))

(defcheck solution-6267ec8a
  (fn [s n] (map #(map second %) (vals (group-by (comp #(mod % n) first) (map-indexed vector s))))))

(defcheck solution-628e442e
  (fn [s n] (map #(take-nth n (nthrest s %1)) (range n))))

(defcheck solution-62d73e10
  #(vals (group-by (fn [i] (mod i %2)) %1)))

(defcheck solution-6439b6db
  (fn [l n]
    (loop [l   l
           acc (vec (repeat n '[]))
           c   0]
      (if (empty? l)
        acc
        (recur (rest l)
          (let [seq-head (vec (take c acc))
                new-val  (vec (conj (get acc c) (first l)))
                seq-tail (vec (drop (+ 1 c) acc))]
            (cond (empty? seq-head) (vec (cons new-val seq-tail))
                  (empty? seq-tail) (vec (into (vec seq-head) [new-val]))
                  :else (vec (into (conj seq-head new-val) seq-tail))))
          (if (= c (- (count acc) 1))
            0
            (+ 1 c)))))))

(defcheck solution-649e698
  (fn reverse-interleave
    [l n]
    (let [buckets (take n (repeat '()))
          ri      (fn [acc l]
                    (if (empty? l) (map reverse acc)
                                   (recur
                                     (concat (rest acc) (list (conj (first acc) (first l))))
                                     (rest l))))]
      (ri buckets l))))

(defcheck solution-64e1d70c
  (fn unleave [s n]
    (letfn
     [(groups [s n]
        (if (seq s)
          (cons (take n s) (groups (drop n s) n))
          )
        )
      ]
      (apply
        (partial map (fn [& args] args))
        (groups s n)
        )
      )
    ))

(defcheck solution-65693c68
  (fn [s x]
    (reduce (partial map conj)
      (repeat x [])
      (partition x s))))

(defcheck solution-65b08afd
  #(reduce
     (partial map conj)
     (repeat %2 [])
     (partition %2 %)))

(defcheck solution-65f8b20b
  #((fn f [r [a & b] k]
      (if (= a nil) r
                    (f
                      (assoc r k (conj (get r k []) a))
                      b
                      (mod (+ k 1) %2))))
    [] % 0))

(defcheck solution-663424cd
  (fn [s n]
    (vals (group-by #(mod (.indexOf s %) n) s))))

(defcheck solution-669e5380
  (fn [s n] (for [i (range n)] (for [j (range i (count s) n)] (nth s j)))))

(defcheck solution-66bd4e78
  (fn [s n]
    (for [a (range n)]
      (map #(nth % a) (partition n s)))))

(defcheck solution-6705f875
  #(->> %1 (partition-all %2) (apply map vector)))

(defcheck solution-6743fdd4
  (fn ! ([x y] (! x y 1))
    ([x y z]
     (if (< y z)
       nil
       (concat
        [(! x y z z)]
        (! x y (inc z)))))
    ([x y z w]
     (if (empty? x)
       nil
       (if (= w 1)
         (concat [(first x)] (! (rest x) y z y))
         (! (rest x) y z (dec w)))))))

(defcheck solution-6746ac23
  #(let [n (/ (count %1) %2)]
     (loop [a %1, v []]
       (if (= %2 (count v))
         (lazy-seq v)
         (recur (drop 1 a) (conj v (take-nth %2 a)))))))

(defcheck solution-675ec394
  (fn reverse-interleave [coll num]
    (map second (group-by #(mod % num) coll))))

(defcheck solution-67a868c
  #(apply (partial map list) (partition-all %2 %1)))

(defcheck solution-67dab7b0
  #(map (partial map second) (vals (group-by (fn [[i _]] (rem i %2)) (map-indexed list %1)))))

(defcheck solution-6837bbf6
  #(apply map vector (partition-all %2 %)))

(defcheck solution-6913b87a
  (fn [c n] (map #(map second %) (vals (group-by first (map-indexed (fn [i x] [(rem i n) x]) c))))))

(defcheck solution-692188fc
  (fn reverse-interleave [coll n]
    (let [step n, len (quot (count coll) n)]
      (letfn [(get-the-seq [i midresult]
                (if (= (count midresult) len)
                  midresult
                  (recur (+ i step) (conj midresult (nth coll i)))))]
        (loop [b 0, result []]
          (if (= b n)
            result
            (recur (inc b) (conj result (get-the-seq b [])))))))))

(defcheck solution-695f4c73
  (fn [xs n]
    (map #(map last %)
      (vals (group-by #(mod (first %) n)
              (map-indexed vector xs))))))

(defcheck solution-69d637ae
  (fn [coll n]
    (for [i (range n)]
      (take-nth n (drop i coll)))))

(defcheck solution-6b9c337f
  (fn [coll n]
    (apply map list (partition n coll))))

(defcheck solution-6bdd3e3d
  (fn [l n]
    (map
      (fn [i] (take-nth n (drop i l)))
      (range n))))

(defcheck solution-6be1ad81
  (fn [xs n]
    (partition (/ (count xs) n)
      (apply interleave
        (partition n xs)))))

(defcheck solution-6dbd6ed0
  (fn revLeave [a b]
    (vals (group-by #(mod (.indexOf a %) b) a))))

(defcheck solution-6f457e88
  (comp (partial apply map list) #(partition %2 %1)))

(defcheck solution-6fe3caa4
  (fn [v n]
    (let [length (count v)]
      (for [start (range 0 n)]
        (for [i (range start length n)] (nth v i))))))

(defcheck solution-701dfa10
  (fn [xs n]
    (let [go (fn [acc x] (map conj acc x))]
      (reduce go (replicate n []) (partition n xs)))))

(defcheck solution-7042127f
  (fn [ls n] (let [g (group-by second (map vector ls (cycle (range n 0 -1))))]
               (for [i (range n 0 -1)]
                 (map first (g i))))))

(defcheck solution-70724dfc
  (fn [lst n]
    (let [group (group-by #(mod (first %) n) (map-indexed vector lst))]
      (map #(map second %) (map group (range n))))))

(defcheck solution-70c14e2e
  #(let [size   (/ (count %1) %2)
         result (for [i (range 0 %2)] ((partial map (vec %1)) (range i (count %1) %2)))]
     result))

(defcheck solution-70cefb0b
  (fn [s n] (map #(map (fn [x] (nth x %)) (partition n s))
              (range n))))

(defcheck solution-718de5fb
  (fn [xs n]
    (loop [ls xs acc (repeat n [])]
      (if (seq ls)
        (recur (drop n ls) (map #(conj % %2) acc (take n ls)))
        acc))))

(defcheck solution-71bda7f2
  #(map (fn [a b c] (range a (inc b) c)) (take %2 (range (first %1) (last %1))) (repeat (last %1)) (repeat %2)))

(defcheck solution-71f146a1
  (fn [xs n]
    (loop [i 0 cs xs ret []]
      (if (= i n)
        ret
        (recur (inc i) (rest cs) (conj ret (take-nth n cs)))))))

(defcheck solution-72f171db
  #(->> %1 (partition %2) (apply map list)))

(defcheck solution-730271fa
  (fn [lat n] (->> (partition-all n lat) (apply mapv vector))))

(defcheck solution-73215e55
  (fn [xs n] (take n (map #(take-nth n %) (iterate #(drop 1 %) xs)))))

(defcheck solution-73578492
  #(apply (partial map list)
     ((fn splitter [n l]
        (if (< (count l) n)
          nil
          (let [z (split-at n l)]
            (cons (first z) (splitter n (second z)))))) %2 %1)))

(defcheck solution-738da22b
  (fn [a-seq n]
    (let [parted (partition n a-seq)]
      (loop [acc    '()
             my-seq parted]
        (cond
          (every? empty? my-seq) (reverse acc)
          :else (recur
                  (cons (map first my-seq) acc)
                  (map #(rest %) my-seq)))))))

(defcheck solution-73c83b56
  (fn [coll n]
    (map #(second %) (group-by #(rem % n) coll))))

(defcheck solution-7477da8b
  #((fn f [xxs ys i]
      (if (empty? ys) xxs
                      (f (update-in xxs [i] (fn [_] (conj (nth xxs i) (first ys)))) (rest ys) (mod (inc i) %2))
                      )) (vec (repeat %2 [])) % 0))

(defcheck solution-74d9f483
  (fn [lst n]
    (loop [rm (seq lst), acc (vec (repeat n (vector))), c 0]
      (let [i (mod c n)]
        (cond (empty? rm) (seq (map seq acc))
              :else (recur (rest rm) (assoc acc i (conj (acc i) (first rm))) (inc c)))))))

(defcheck solution-75476d28
  #(for [i (range %2)]
     (take-nth %2 (drop i %))))

(defcheck solution-76022c4e
  ;(fn la [col n]
  ;  (loop [i 0
  ;         colrest col
  ;         rez []]
  ;    (if (< i n)
  ;      (recur (inc i) (rest colrest) (conj rez (flatten (partition-all 1 n colrest))))
  ;      (identity rez))))
  #(apply map list (partition %2 %1)))

(defcheck solution-770b2b89
  (fn [coll n]
    (apply map list (partition n coll))))

(defcheck solution-77588aaf
  (fn [s, n]
    (loop [i s, r []]
      (let [a (apply concat (partition 1 n i))]
        (if (< (count a) (count (last r)))
          r
          (recur (rest i) (conj r a)))))))

(defcheck solution-7768793e
  (fn revin [col n]
    (->> col
      (partition-all n)
      (apply map list))))

(defcheck solution-7788c367
  (fn reverse-interleave [col n]
    (for [i (range n)]
      (apply concat (partition 1 n (drop i col))))))

(defcheck solution-7790ff4b
  (fn [coll n]
    (apply (partial map list) (partition n coll))))

(defcheck solution-784dc80d
  (fn reverse-interleave [coll partitions]
    (apply map (fn [& items] (concat items)) (partition partitions coll))))

(defcheck solution-786d891e
  #(for [i (range %2)]
     (take-nth %2 (drop i %))))

(defcheck solution-78e21087
  (fn [s n]
    (->> s
      (map list (range))
      (group-by #(rem (first %) n))
      (vals)
      (map (partial map second)))))

(defcheck solution-79059d69
  (fn [a b]
    ((fn f [[h & t], [r1 & r2]]
       (if (nil? h)
         `[~r1 ~@r2]
         (f t `[~@r2 ~(conj r1 h)]))
       ) a (take b (repeat [])))))

(defcheck solution-79e793d0
  (fn [coll n]
    (loop [result  []
           subcoll coll]
      (if (== n (count result))
        (seq result)
        (recur (conj result (take-nth n subcoll)) (rest subcoll))))))

(defcheck solution-7bbe1c0f
  (fn [s x]
    (map #(map (partial nth s)
            (range % (count s) x))
      (range x))))

(defcheck solution-7c55313
  (fn [coll n]
    (map #(take-nth n (drop % coll))
      (range n))))

(defcheck solution-7c6102aa
  #(->> %1
     (iterate rest)
     (take %2)
     (map (partial take-nth %2))))

(defcheck solution-7cc2af25
  (fn [s n] (map #(take-nth n (drop % s)) (range n))))

(defcheck solution-7d1b3c73
  (fn reverse-interleave [c n]
    (map #(reverse %)
      (reduce

        (fn [a ci]

          (map
            (fn [ai]
              (if (= (+ (first ai) n) ci)
                (conj ai ci) ai))
            a))

        (map #(list %) (subvec (vec c) 0 n))

        c))))

(defcheck solution-7d2e1ea0
  (fn [ls n] (vals (group-by #(rem % n) ls))))

(defcheck solution-7d4a0338
  #(for [x (range %2)]
     (keep-indexed (fn [i v] (if (= x (mod i %2)) v))
       %)))

(defcheck solution-7d4ec2d5
  (fn [col n] (apply map vector (partition n col))))

(defcheck solution-7d52f570
  (fn [xs n]
    (loop [xs xs i n zs []]
      (if (>= i 1)
        (recur (next xs)
          (dec i)
          (conj zs (take-nth n xs)))

        zs))))

(defcheck solution-7d64b5e3
  (fn [xs n]
    (apply map vector (partition n xs))))

(defcheck solution-7da4bd1c
  #(->> %1
     (partition %2)
     (apply map vector)))

(defcheck solution-7dc0581
  (fn __ [col parts]
    (vals (group-by #(mod % parts) col))))

(defcheck solution-7e0e7160
  (fn [coll n]
    (->> (partition n coll)
      (map (partial partition 1))
      (reduce (fn [colls c]
                (map (fn [a b] (concat a b)) colls c))))))

(defcheck solution-7e1a845f
  (fn [c x]
    (->> (map vector (cycle (range x)) c)
      (group-by first)
      (into (sorted-map))
      (vals)
      (map #(map second %)))))

(defcheck solution-7e7e8cf0
  (fn [x y] (apply map list (partition y x))))

(defcheck solution-7f168ba6
  (fn [xs n]
    (for [i (range n)
          :let [indices (range i (count xs) n)]]
      (map #(nth xs %) indices))))

(defcheck solution-7fd1f535
  #(apply map (fn [& coll] coll) (partition %2 %)))

(defcheck solution-80011712
  (fn [seq n] (apply map list (partition n seq))))

(defcheck solution-81346cf9
  (fn rev-inter [s n]
    (let [rev-inter-inner
          (fn rev-inter-inner [ss nn]
            (if (zero? nn)
              (list)
              (cons (take-nth n ss) (rev-inter-inner (rest ss) (dec nn)))))]
      (rev-inter-inner s n))))

(defcheck solution-813e4048
  (fn [l c]
    (apply map list (partition-all c l))))

(defcheck solution-816fe461
  (fn [xs n]
    (reduce (fn [ys zs] (map #(conj %1 %2) ys zs)) (repeat n []) (partition n xs))))

(defcheck solution-81a7b149
  (fn [val size]
    (apply map (fn [& args] (into [] args)) (partition size val))))

(defcheck solution-821cfac
  #(map second (group-by (fn [x] (mod x %2)) %1)))

(defcheck solution-829986be
  (fn [s n] (vals (group-by (fn [x] (mod x n)) s))))

(defcheck solution-82b84c3f
  (fn [lst n]
    (apply (partial map (fn [& xs] xs)) (take (/ (count lst) n) (iterate (fn [l] (drop n l)) lst)))
    ))

(defcheck solution-82be0cd4
  (fn [coll n] (for [i (range n)] (flatten (partition 1 n (drop i coll))))))

(defcheck solution-83ef5942
  #(apply map vector
     (partition %2 %1)))

(defcheck solution-84d36be7
  #_("It works only when the length of the collection is divisible by n, but all exapmles here exhibit that trait, so I'm being a smartass :)")
  (fn [coll n]
    (let [q (/ (count coll) n)]
      (partition q (apply interleave (partition n coll))))))

(defcheck solution-851a2ec0
  (fn [s n]
    ((fn reverse-interleave [s n i]
       (if (= i 1)
         (list (take-nth n s))
         (cons (take-nth n s) (reverse-interleave (rest s) n (dec i)))))
     s n n)))

(defcheck solution-851f71e4
  (fn sin-interlave [seq n]
    (reverse
      (loop [s 0 result '()]
        (if (= n s)
          result
          (recur (inc s) (conj result (take-nth n (drop s seq))))
          )
        )
      )
    ))

(defcheck solution-853bba9a
  (fn [xs n]
    (partition (/ (count xs) n)
      (apply interleave (partition n xs)))))

(defcheck solution-8615dfb2
  (fn
    [s n]
    (map
      #(keep-indexed (fn [i v] (if (= % (rem i n)) v nil)) s)
      (range n))))

(defcheck solution-863ff06a
  (fn reverse-interleave
    [coll n]
    (partition-all (/ (count coll) n)
      (apply interleave (partition-all n coll)))))

(defcheck solution-86b7ef6d
  (fn un-interleave [xs n]
    (let [parts (partition n xs)]
      (apply map list parts))))

(defcheck solution-86c4273
  (fn r [s n] (for [i (range n)] (for [j (range (count s)) :when (= i (rem j n))] (nth s j)))))

(defcheck solution-878371ec
  (fn [coll n] (loop [s coll i n res []]
                 (if (= i 0) (seq res) (recur (rest s) (dec i) (into res (list (take-nth n s))))))))

(defcheck solution-87974c67
  (fn [coll n]
    (for [i (range n)]
      (take-nth n (drop i coll)))))

(defcheck solution-885359c6
  (fn [l n]
    (for [i (range n)]
      (take-nth n (drop i l))
      )
    ))

(defcheck solution-88572383
  (fn [col n]
    (for
     [x (range n)
      :let [pos (filter #(= (rem % n) x) (range (count col)))]
      ]
      (map #(nth col %) pos)
      )
    ))

(defcheck solution-888c33e7
  (fn [coll n]
    (reduce
      (fn [sub-seqs, next-v]
        (conj (subvec sub-seqs 1) (conj (first sub-seqs) next-v)))
      (nth (iterate #(conj % []) []) n)
      coll)))

(defcheck solution-89576bcf
  (fn [seq n]
    (map (fn [v]
           (map #(last %) v))
      (vals (group-by #(mod (first %) n) (map-indexed vector seq))))))

(defcheck solution-89982880
  (fn [cl np] (let [sp (/ (count cl) np)]
                (for [n (range np)] (for [i (range sp)] (nth cl (+ n (* i np))))
                                    ))))

(defcheck solution-89e4dabf
  (fn reverse-interleave
    ([s n] (reverse-interleave s n 0))
    ([s n offset]
     (if (= offset n)
       []
       (lazy-seq (cons (take-nth n (drop offset s))
                   (reverse-interleave s n (inc offset))))))))

(defcheck solution-8adb5a42
  (fn [coll n]
    (apply (partial map list) (partition-all n coll))))

(defcheck solution-8af93013
  (fn [xs x]
    (vals (group-by #(mod % x) xs))))

(defcheck solution-8b27bb5
  (fn [sourceseq number]
    (loop [res    (repeat number [])
           source sourceseq]
      (let [items (take number source)
            rest  (drop number source)]
        (if (empty? items)
          res
          (recur (map conj res items) rest))))))

(defcheck solution-8b9156f8
  (fn [coll n]
    (loop [coll    coll
           i       0
           buckets (vec (repeat n (vector)))]
      (if coll
        (recur (next coll)
          (mod (inc i) n)
          (assoc buckets i (conj (nth buckets i) (first coll))))
        buckets))))

(defcheck solution-8c5c50b4
  (fn r-interleave [s n]
    (let [shift-add (fn [s val]
                      (cons (conj (last s) val) (butlast s)))]
      (reverse (reduce shift-add (repeat n []) s)))))

(defcheck solution-8cf801b9
  (fn [c n] (map #(map (vec c) %) (for [k (range n)] (range k (count c) n)))))

(defcheck solution-8d9fde50
  (fn [s n] (reverse
              (loop [res () i 0]
                (if (= n i)
                  res
                  (recur (conj res (take-nth n (drop i s))) (inc i))
                  )
                )
              )))

(defcheck solution-8df2d827
  (fn [xs s] (apply map vector (partition s xs))))

(defcheck solution-8df95d1a
  (fn [xs n] (apply map vector (partition n xs))))

(defcheck solution-8e8fc5c1
  (fn reverse-interleave
    [coll number-partitions]
    "Applies the reverse of interleave onto a collection.
That is, partitions the collection into a given number of
collections, then creates the same number of collections by taking
the first of each collection, the second, etc..."
    (let [partitioned-col
          (for [x (range 0 (quot (count coll) number-partitions))]
            (for [y (range 0 number-partitions)]
              (nth coll (+ (* x number-partitions) y))))]
      (for [index (range 0 number-partitions)]
        (map #(nth % index) partitioned-col)))))

(defcheck solution-8ea874b9
  (fn revi [s x]
    (for [i (range x)]
      (doall (take-nth x (drop i s))))))

(defcheck solution-8f43a0f7
  #(for [n (range %2)] (take-nth %2 (drop n %))))

(defcheck solution-8fccefb
  (fn [s n]
    (let [r (take n (repeat []))]
      (loop [s s r r]
        (if (empty? s) r
                       (recur (rest s) (conj (apply vector (rest r)) (conj (first r) (first s))))
                       )
        )
      )
    ))

(defcheck solution-8fce9fe1
  (fn reverseInterleave [coll n] (apply map list (partition n coll))))

(defcheck solution-9041b71f
  (fn cut [coll n]
    (map val (group-by #(mod % n) coll))))

(defcheck solution-9045c7fa
  (fn [s n] (apply map list (partition n s))))

(defcheck solution-90e98a0e
  (fn [coll n]
    (let [parts (partition n coll)
          step  (fn [i] (map #(nth % i) parts))]
      (map step (range n)))))

(defcheck solution-9134994d
  (fn [coll n]
    (for [x (range n)]
      (map #(nth % x) (partition n coll)))))

(defcheck solution-91a34f92
  (fn [coll n]
    (loop [c coll acc '() i n]
      (if (= 0 i) (reverse acc)
                  (recur (rest c) (cons (take-nth n c) acc) (dec i))))))

(defcheck solution-91c54fd2
  (fn [l n] (vals (group-by #(mod % n) l))))

(defcheck solution-91dba242
  (fn my-partition-n [coll n]
    (let [n-total (count coll)
          n1      (/ n-total n)
          r1      (partition n coll)
          r2      (apply interleave r1)
          ]
      (partition n1 r2))))

(defcheck solution-9231f3c9
  (fn [xs n] (let [take1   (fn [ts]
                             (seq (into [] (map #(first %) ts)))
                             ),
                   takeall (fn [res ds]
                             (if (first (first ds))
                               (recur (conj res (take1 ds)) (map #(drop 1 %) ds))
                               res)
                             )]
               (takeall [] (partition n xs)))
    ))

(defcheck solution-925c6d60
  (fn [a b]
    (for [i (conj (into [] (rest (range b))) 0)]
      (filter
        (fn [x]
          (if
           (= i (mod (inc (.indexOf a x)) b))
            true
            false
            )
          )
        a
        )
      )
    ))

(defcheck solution-925eb235
  (fn reverse-interleave [s n]
    (let [l (count s)
          m (/ l n)]
      (partition m (apply interleave (partition n s))))))

(defcheck solution-928c3280
  (fn [coll n]
    (for [i (range n)]
      (keep-indexed (fn [idx item] (when (zero? (rem idx n)) item)) (drop i coll)))))

(defcheck solution-93735b8f
  (fn [s n]
    (loop [x (into [] (take n (repeatedly vector)))
           s s
           o 0]
      (if (seq s)
        (recur (update-in x [(mod o n)] conj (first s)) (rest s) (inc o))
        x))))

(defcheck solution-947349ae
  #(apply map list (partition %2 %)))

(defcheck solution-947ce796
  #(for [x (range %2)] (take-nth %2 (drop x %1))))

(defcheck solution-948f774e
  (fn [l n]
    (for [c (range n)]
      (for [t (map #(+ c (* % n)) (range (count l))) :while (< t (count l))]
        (nth l t)))))

(defcheck solution-94bee46
  (fn [coll n]
    (loop [acc (vec (repeat n [])) pos 0 l-coll coll]
      (if (empty? l-coll)
        acc
        (recur (update-in acc [(rem pos n)] conj (first l-coll))
          (inc pos) (rest l-coll))))))

(defcheck solution-94c094f4
  (fn func
    [input-seq num]
    (vals (group-by #(mod (.indexOf input-seq %) num) input-seq))))

(defcheck solution-9569511c
  #(apply map (cons (fn [& l] l) (partition %2 %1))))

(defcheck solution-9573f172
  (fn [xs n] ((fn [tails] (apply map vector tails)) (take-while #(not (empty? %)) (iterate (partial drop n) xs)))))

(defcheck solution-95f00426
  (fn [coll n]
    (loop [pass n
           c    coll
           out  []]
      (if (= pass 0)
        out
        (recur (- pass 1) (rest c) (conj out (take-nth n c)))))))

(defcheck solution-9660014
  (fn rvr-intrlv
    [lst x]
    (loop [l lst base 0 out (repeat x [])]
      (if (>= base (count l))
        out
        (recur l (+ base x)
          (map
            #(conj (nth out %) (nth l (+ base %)))
            (range x))
          )
        )
      )
    ))

(defcheck solution-96eb6a63
  (fn [s n]
    (map
      (fn f [k] (map #(nth s %) (filter #(= k (mod % n)) (range (count s)))))
      (range n))))

(defcheck solution-972f3833
  (fn [coll n]
    (loop [curnum  0
           curlist coll
           result  []]
      (if (empty? curlist)
        result
        (recur
          (mod (inc curnum) n)
          (rest curlist)
          (assoc result curnum (conj (nth result curnum []) (first curlist)))))
      )
    ))

(defcheck solution-974b9ead
  (fn [coll n]
    (apply map list (partition n coll))))

(defcheck solution-9778c1cc
  (fn [s n]
    (map
      (fn [i]
        (filter #(= i (mod (.indexOf s %) n)) s))
      (range n))))

(defcheck solution-97916bbd
  #(apply map (fn [& hs] hs) (partition %2 %)))

(defcheck solution-979bb13f
  (fn [coll num-subseqs]
    (let [ps (partition num-subseqs coll)]
      (for [i (range num-subseqs)]
        (map #(nth % i) ps)))))

(defcheck solution-98d62c52
  (fn [coll n]
    (for [i (range n)] (take-nth n (drop i coll)))))

(defcheck solution-99783203
  (fn [coll n]
    (->> coll
      (partition n)
      (mapcat (fn [p] (map vector (range) p)))
      (group-by #(nth % 0))
      (map (fn [kv] (map #(nth % 1) (val kv)))))))

(defcheck solution-9a2a5bca
  (fn revInt [v x]
    (let [sz (/ (count v) x)]
      (partition-all sz
        (apply interleave (partition-all x v))
        )
      )
    ))

(defcheck solution-9a90983c
  (fn [col n]
    (map (fn [v] (map second (second v))) (group-by first (map-indexed (fn [idx itm] [(mod idx n) itm]) col)))
    ))

(defcheck solution-9b1e2dba
  #(map (fn [x]
          (for [n (range (count %1))
                :when (= (mod n %2) x)]
            (nth %1 n)))
     (range %2)))

(defcheck solution-9b94e96b
  (fn inter [collect num]
    (loop [col collect, n 0, result (reduce (fn [col, _] (conj col [])) [] (range 0 num))]

      (if (empty? col)
        result

        (recur (rest col) (inc n)
          (let [n_ (mod n num)]
            (assoc result n_
                          (conj (nth result n_) (first col))
                          )
            )
          )
        )
      )

    ))

(defcheck solution-9b9d5968
  (fn [xs n]
    (loop [xs xs ys (for [i (range n)] [])]
      (if (> n (count xs))
        ys
        (recur (drop n xs) (for [i (range n)] (conj (nth ys i) (nth xs i))))))))

(defcheck solution-9c20446b
  (fn deinterleave
    [s n]
    (reduce (fn [result v] (map conj result v))
      (take n (repeat []))
      (partition n s))))

(defcheck solution-9c3922be
  (fn [l k] (map #(->> l (drop %) (take-nth k)) (range k))))

(defcheck solution-9c446377
  (fn [s n]
    (loop [i 0 acc []]
      (if (= n i)
        acc
        (recur
          (inc i)
          (conj acc
            (apply concat
              (partition 1 n (drop i s)))))))))

(defcheck solution-9c74cb58
  ; This is a very clever solution. I had not seen take-nth before. No wonder I was struggling...

  (fn deinterleave [coll n]
    (for [i (range n)] (take-nth n (drop i coll)))))

(defcheck solution-9c767316
  (fn r_intrlv ([_seq n] (r_intrlv _seq n '()))
    ([_seq n result]
     (if (empty? _seq)
       (partition (count result) (apply interleave (reverse result)))
       (recur (drop n _seq) n (conj result (take n _seq)))))))

(defcheck solution-9d64b0d2
  (fn f [s n]
    (if (empty? s)
      (repeat n ())
      (map cons (take n s) (f (drop n s) n)))))

(defcheck solution-9e78e15e
  #(->> (partition %2 %)
     (apply interleave)
     (partition (/ (count %) %2))))

(defcheck solution-9e824be5
  #(for [i (range %2)]
     (->> %1 (drop i) (take-nth %2))))

(defcheck solution-9ebe8ba5
  (fn [l n]
    (letfn [(strides [l n]
              (if (empty? l)
                []
                (let [[a b] (split-at n l)]
                  (cons a (strides b n)))))]
      (apply (partial map list) (strides l n)))))

(defcheck solution-9ec1720
  (fn revint [col n]
    (reduce (fn [x y]
              (map #(apply conj %) (partition 2 (interleave x y))))
      (repeat n [])
      (partition n col))))

(defcheck solution-9edac978
  (fn recu [s n]
    (partition (/ (count s) n) (loop [c      1
                                      result (take-nth n s)]
                                 (if (= c n) result
                                             (recur (inc c) (concat result (take-nth n (drop c s))))))
      )))

(defcheck solution-9eec6c6b
  (fn [coll n] (map take-nth (repeat n) (map #(drop % %2) (range n) (repeat coll)))))

(defcheck solution-9f01460d
  (fn [seq n]
    (letfn [(everynth [seq n offset]
              (keep-indexed (fn [i x]
                              (if (= offset (mod i n)) x nil)) seq))]
      (for [k (range n)] (everynth seq n k)))))

(defcheck solution-9f56fa6b
  (fn [xs n] (map #(map second %1) (vals (group-by #(mod (first %) n) (map vector (range) xs))))))

(defcheck solution-9f6ea21
  (fn [xs k]
    (loop
     [xs' xs
      acc (repeat k [])]
      (if (empty? xs')
        acc
        (recur
          (drop k xs')
          (vec
            (map
              (fn [i]
                (let [acc_i (nth acc i)]
                  (if (>= i (count xs'))
                    acc_i (conj acc_i (nth xs' i)))))
              (range k))))))))

(defcheck solution-9fb1612a
  (fn [l n]
    (let [p (/ (count l) n)]
      (partition p (apply interleave (partition n l))))))

(defcheck solution-9fceb697
  (fn [s n]
    (loop [accs (repeat n [])
           rems s]
      (if (empty? rems) accs
                        (recur (map conj accs (take n rems)) (drop n rems))))))

(defcheck solution-9ff94fd2
  (fn [x y]
    (loop [result [] x x counter 1]
      (if (> counter y)
        result
        (recur (conj result (take-nth y x)) (drop 1 x) (inc counter))))))

(defcheck solution-a00fbd51
  (fn [col n] (apply map list (partition n col))))

(defcheck solution-a07219dc
  (fn ri [coll n]
    (for [[k v]
          (group-by #(mod (first %1) n)
            (map-indexed #(list %1 %2) coll))]
      (map #(second %) v))))

(defcheck solution-a0874a17
  (fn f [c n]
    (if (empty? c)
      (replicate n '())
      (map cons (take n c) (f (drop n c) n)))))

(defcheck solution-a0d08ecc
  (fn [c n] (vals (group-by #(mod % n) c))))

(defcheck solution-a125d34a
  (fn [inseq prt-num]
    (let [inx-map (apply hash-map (interleave (range (count inseq)) inseq))]
      (loop [ret []
             cnt 0]
        (if (= cnt prt-num)
          ret
          (recur (concat ret [(reduce #(concat %1 [(inx-map %2)]) []
                                (filter #(= 0 (mod (- % cnt) prt-num)) (sort (keys inx-map))))])
            (inc cnt)))))))

(defcheck solution-a218ef32
  (fn [s n]
    (let [vs (vec (repeat n []))]
      (loop [s s, m 0, vs vs]
        (if (seq s)
          (if (= m n)
            (recur s 0 vs)
            (recur (rest s) (inc m)
              (assoc vs m (conj (vs m) (first s)))))
          vs)))))

(defcheck solution-a2496dec
  (let [revinter
        (fn [coll n result k]
          (if (nil? coll)
            (vals (sort result))
            (recur (next coll) n (assoc result k (conj (get result k []) (first coll))) (if (= n (inc k)) 0 (inc k)))
            )
          )]
    (fn [coll n] (revinter coll n {} 0))
    ))

(defcheck solution-a2d89f7
  #(partition-all (/ (count %) %2) (apply interleave (partition-all %2 %))))

(defcheck solution-a311247e
  (fn [s x]
    (map #(take-nth x (drop % s))
      (range x))))

(defcheck solution-a34ff1f8
  (fn reverse-interleave [s i]
    (map #(flatten (map (fn [x] (rest x)) %)) (vals (group-by #(rem (inc (first %)) i) (map-indexed vector s))))
    ))

(defcheck solution-a4ed54a9
  (fn
    [v n]
    (apply map (fn [& x] (remove nil? x))
      (partition-all n v))))

(defcheck solution-a4ef9e7e
  (fn reverse-interleave [coll n]
    (apply map list (partition n coll))))

(defcheck solution-a5759526
  (fn de-interleave [xs n]
    (let [len (count xs)]
      (map (fn [step]
             (map #(nth xs %)
               (range step len n)))
        (range n)))))

(defcheck solution-a5c1f288
  (fn [coll n]
    (for [x (range n)]
      (take-nth n (drop x coll)))))

(defcheck solution-a5cb4060
  (fn [a n]
    (let [zindf   (fn [off] (map #(+ off (* n %)) (range (/ (count a) n))))
          allinds (map zindf (range n))
          els     (fn [is] (map #(nth a %) is))]
      (map els allinds))))

(defcheck solution-a5e09a49
  (fn [l n]
    (let [li     (map-indexed vector l)
          keep?i (fn [i] #(= i (mod (% 0) n)))
          seqi   (fn [i] (->> li (filter (keep?i i)) (map #(% 1))))]
      (map seqi (range n)))))

(defcheck solution-a656ffa9
  (fn [coll n] (for [[x y] (group-by #(second %)
                             (map-indexed #(identity [%2 (mod %1 n)]) coll))] (map first y))))

(defcheck solution-a72281a5
  (fn reverse-interleave [s n]
    (-> (group-by #(mod % n) s)
      vals
      )))

(defcheck solution-a7369e0c
  (fn [coll n]
    (let [gmap (group-by second (map vector coll (cycle (range n))))]
      (map (partial map first) (map gmap (range n)))
      )
    ))

(defcheck solution-a74383c6
  (fn [lst n]
    (apply map vector (partition n lst))))

(defcheck solution-a78331ab
  #(apply (partial map vector) (partition %2 %1)))

(defcheck solution-a78f9f6d
  (fn [xs step]
    (loop [xs      (seq xs)
           result  ()
           counter 1]
      (if (> counter step)
        (reverse result)
        (recur (drop 1 xs)
          (conj result (->> (partition 1 step xs) flatten))
          (inc counter))))))

(defcheck solution-a7e555ed
  (fn reverse-interleave [s x]
    (map #(take-nth x (drop % s)) (range x))))

(defcheck solution-a8253e0d
  (fn [coll n]
    (apply map list (partition-all n coll))))

(defcheck solution-a8344c1e
  (fn [vs n]
    (let [ith-mod-pair (fn [target] (fn [[i _]] (= target (mod i n))))
          enumerate    (fn [xs] (map-indexed (fn [i x] [i x]) xs))
          ith-seq      (fn [target] (->> vs
                                      enumerate
                                      (filter (ith-mod-pair target))
                                      (map last)))]
      (->> n
        range
        (map ith-seq)))))

(defcheck solution-a8c8e47c
  (fn [coll x]
    (let [partition
          (loop [store {} index 0]
            (if (= index x)
              store
              (recur (into store {index []}) (inc index))
              )
            )
          ]
      (loop [result partition elements coll i 0]
        (if elements
          (recur (into result {(mod i x) (conj (result (mod i x)) (first elements))}) (next elements) (inc i))
          (vals result)
          )
        )
      )
    ))

(defcheck solution-a9221271
  (fn [s n] (map #(take-nth n (drop % s))
              (range n))))

(defcheck solution-a92bc158
  (fn [x y]
    (let [howmany (count x)]
      (map #(take-nth y (take-last (- howmany %) x)) (range y)))))

(defcheck solution-a9da8157
  (fn [coll n]
    (for [i (range n)]
      (keep-indexed #(if (= i (mod %1 n)) %2) coll))))

(defcheck solution-aa123bdb
  (fn rInter [xs n]
    (loop [orig   xs
           cnt    0
           result []]
      (if (= cnt n)
        result
        (recur (rest orig) (inc cnt) (conj result (flatten (partition 1 n orig))))))))

(defcheck solution-aa23675f
  (fn uninterleave [coll n]
    (apply map list (partition n coll))))

(defcheck solution-aa4b3226
  #(apply map list (partition-all %2 %)))

(defcheck solution-aa8a2cb
  (fn rev-interleave [s n]
    (let [parts (partition n s)]
      (loop [
             result  [],
             counter (count (first parts)),
             d       parts
             ]
        (if (zero? counter)
          result
          (recur
            (conj result (map first d))
            (dec counter)
            (map rest d)
            ))))))

(defcheck solution-aab083a3
  (fn rev [s g]
    (let [len  (count s)
          part (fn [start-n]
                 (loop [n start-n ret []]
                   (if (< n len)
                     (recur (+ n g) (conj ret (nth s n)))
                     ret)))]
      (map part (range g)))))

(defcheck solution-aad89334
  #(for [x (range %2)] (map (fn [s] (nth s x)) (partition %2 %))))

(defcheck solution-aae0b49e
  (fn [coll n]
    (->> coll
      (map-indexed (fn [idx v] [(mod idx n) v]))
      (sort-by first)
      (map second)
      (partition (/ (count coll) n)))))

(defcheck solution-ab15bba3
  (fn [s n]
    (map
      #(flatten (partition 1 n %))
      (take n (iterate rest s)))))

(defcheck solution-ab3aa24a
  (fn [xs n]
    (map #(map first (second %))
      (group-by second (map #(vec [%1 %2]) xs (cycle (range n)))))))

(defcheck solution-ab608a90
  (fn [coll size]
    (for [n (range size)]
      (flatten (partition 1 size (drop n coll))))))

(defcheck solution-ab6ff2d0
  (fn [a, n] (vals (group-by #(mod % (- n)) a))))

(defcheck solution-abbc3d8e
  (fn [vek i] (for [j (range i)] (take-nth i (drop j vek)))))

(defcheck solution-abef3dee
  (fn [xs x]
    (let [f (fn [n] (keep-indexed #(when (= n (mod % x)) %2) xs))]
      (map f (range x)))))

(defcheck solution-abef8e0b
  (fn x [s n]
    (partition (/ (count s) n) (apply interleave (partition n s)))))

(defcheck solution-ac128867
  (fn [s n]
    (apply map vector (partition n s))))

(defcheck solution-ac4e1331
  (fn [coll nseq]
    (map #(take-nth nseq (drop % coll)) (range nseq))
    ))

(defcheck solution-ac9b6244
  (fn [s n]
    (let [ps (partition n s)]
      (for [k (range (/ (count s) (count ps)))]
        (map #(nth % k) ps)))))

(defcheck solution-ad07006f
  (fn [coll n]
    (map reverse
      (reduce
        #(map conj %1 %2)
        (repeat n ())
        (partition-all n coll)))))

(defcheck solution-ada688c6
  (fn [s n]
    (map (fn [base]
           (filter (fn [item] (= (mod (.indexOf s item) n) base))
             s))
      (range n))))

(defcheck solution-ae15cc08
  #(loop [li %1 n %2 result []]
     (let [ct (count result)]
       (if (= ct n)
         result
         (recur li n (conj result (mapcat (fn [x] (vector (nth x ct))) (partition n li))))))))

(defcheck solution-ae68b26c
  (fn [s n]
    ((fn [s r]
       (if (empty? s)
         r
         (recur (drop n s)
           (map #(concat %1 [%2])
             r
             (take n s)))))
     s (repeat n []))))

(defcheck solution-aedb7153
  (fn [coll n]
    (map #(take-nth n (drop % coll)) (range n))
    ))

(defcheck solution-aee8a4f4
  (fn [c n] (for [x (range n)] (map #(nth % x) (partition n c)))))

(defcheck solution-af13ae09
  (fn [s n]
    (let [r ()]
      (for [x (range n)]
        (concat (take-nth n (drop x s)) r)))))

(defcheck solution-af1cb4c9
  (fn [s n] (map #(take-nth n %) (take n (iterate rest s)))))

(defcheck solution-af3965d0
  #(for [i (range %2)] (take-nth %2 (drop i %1))))

(defcheck solution-af70aedb
  #(
    (fn [buffer current moduls remain]
      (if (empty? remain)
        buffer
        (let [currbuff (buffer current)]
          (recur
            (assoc buffer current
                          (concat currbuff [(first remain)])
                          )
            (rem (inc current) moduls)
            moduls
            (rest remain)
            )
          )
        )
      ) (into [] (repeat %2 [])) 0 %2 %1
    ))

(defcheck solution-af7ac10c
  (fn [coll n]
    (apply map vector (partition n n coll))))

(defcheck solution-aff7f295
  #(apply map vector (partition-all %2 %1)))

(defcheck solution-aff97dec
  (fn [s n] (map #(map second %) (vals (group-by #(mod (first %) n) (map-indexed #(list % %2) s))))))

(defcheck solution-b05eec48
  (fn [coll x]
    (let [size (/ (count coll) x)]
      (partition size
        (map second
          (sort-by #(mod (first %) x)
            (map-indexed vector coll)))))))

(defcheck solution-b0723ea3
  (fn [c n]
    (reduce (fn [l e] (map #(conj % %2) l e)) (repeat n []) (partition n c))))

(defcheck solution-b13e7da
  (fn [col grp]
    (partition (/ (count col) grp) (apply interleave (partition grp col)))))

(defcheck solution-b1e39524
  #(apply map vector
     (partition %2 %)))

(defcheck solution-b200211a
  (fn [sq n] (apply map vector (partition n sq))))

(defcheck solution-b204fb66
  (fn [coll n]
    (map (comp #(map second %) second)
      (group-by #(mod (first %) n)
        (map-indexed vector coll)))))

(defcheck solution-b20bc30e
  (fn [l n]
    (map #(map second (second %))
      (group-by first
        (map-indexed (fn [i e] [(mod i n) e]) l)
        )
      )
    ))

(defcheck solution-b241a935
  #(for [start (range %2)]
     (take-nth %2 (drop start %1))))

(defcheck solution-b2f1cdeb
  (fn break-up [coll n]
    (for [i (range n)]
      (keep-indexed (fn [idx item]
                      (if (= i (mod idx n))
                        item
                        nil))
        coll))))

(defcheck solution-b3f76e37
  (fn [c n] (map #(map last %) (vals (group-by #(mod (first %) n) (vec (map-indexed (fn [idx i] [idx i]) c)))))))

(defcheck solution-b41869a6
  (fn [s x] (map #(map second %) (vals (group-by #(mod (first %) x) (map-indexed vector s))))))

(defcheck solution-b4272dbe
  (fn [s num]
    (let [ivec (map-indexed #(vector (mod %1 num) %2) s)]
      (vals (sort (reduce
                    (fn [ret x]
                      (let [k (first x), v (last x)]
                        (assoc ret k (conj (get ret k []) v))))
                    {}
                    ivec))))))

(defcheck solution-b49823d5
  (fn [coll n]
    (loop [coll coll i 0 ret []]
      (if (empty? coll) ret
                        (recur (rest coll)
                          (mod (inc i) n)
                          (assoc ret i (conj (nth ret i []) (first coll))))))))

(defcheck solution-b4eb5bdf
  (fn [l n]
    (loop [l l i 0 result (vec (repeat n []))]
      (if
       (empty? l)
        result
        (recur
          (rest l)
          (+ i 1)
          (assoc-in result [(mod i n) (quot i n)] (first l)))))))

(defcheck solution-b4f3a15e
  (fn [l n]
    (map #(seq (map (fn [p] (second p)) (second %)))
      (sort-by #(first %)
        (group-by #(mod (nth % 0) n)
          (map-indexed (fn [i, v] [i v]) l))))))

(defcheck solution-b5aaa214
  #(map (fn [i] (take-nth %2 (drop i %))) (range %2)))

(defcheck solution-b5dd56f0
  (fn [xs n] (map #(apply concat (partition 1 n (drop % xs))) (range n))))

(defcheck solution-b60a12aa
  (fn [a-seq n]
    (map #(take-nth n (drop % a-seq)) (range n))))

(defcheck solution-b747a300
  (fn [col n]
    (map (fn [x] (map (fn [y] (second y)) x)
           ) (vals (group-by #(first %) (map-indexed (fn [i x] [(mod i n) x]) col))))))

(defcheck solution-b74e37ca
  (fn rev-interleave [seq n]
    (if (empty? seq)
      '()
      (take n (cons (take-nth n seq) (rev-interleave (rest seq) n))))))

(defcheck solution-b7891cd8
  (fn [coll n]
    (for [x (range n)]
      (keep-indexed #(if (= (mod %1 n) x)
                       %2)
        coll))))

(defcheck solution-b7fcb156
  #(apply (partial map (fn [& eles] (apply list eles))) (partition %2 %1)))

(defcheck solution-b84cfa53
  (fn rinter [coll n]
    (apply map vector (partition n coll))))

(defcheck solution-b85bdbdc
  (fn [col n]
    (let [len        (count col)
          group-size (/ len n)
          ]
      (for [i (range n)]
        (for [j (range group-size)]
          (nth col (+ i (* j n))))))))

(defcheck solution-b8c995a8
  (fn prob43b [s n]
    (let [m (apply merge-with concat
              (map (fn [i] {(rem i n) [(nth s i)]}) (range (count s))))]
      (map #(get m %) (range n)))))

(defcheck solution-b95a2fe5
  #(apply map vector (partition %2 %1)))

(defcheck solution-b999055a
  (fn [l n]
    (apply map list (partition n l))))

(defcheck solution-b9a9148a
  #(apply map list (partition-all %2 %1)))

(defcheck solution-ba355e2b
  (fn [s x] (map #(map second %) (vals (group-by first (map list (cycle (range x)) s))))))

(defcheck solution-ba91dbb1
  (fn [s n] ((partial apply map) vector (partition n s))))

(defcheck solution-bafe2245
  (fn deinterleave [coll n] (for [i (range n)] (take-nth n (drop i coll)))))

(defcheck solution-bbe614b4
  #(partition (/ (count %) %2) (apply interleave (partition %2 %))))

(defcheck solution-bbed5c63
  #(for [offset (range %2)
         :let [y (take-nth %2 (drop offset %1))]]
     y))

(defcheck solution-bbf476f8
  (fn [x y] (map #(take-nth y (drop % x)) (range y))))

(defcheck solution-bc30d9d7
  #(apply map list (partition %2 %1)))

(defcheck solution-bca61ad3
  (fn [coll n]
    (let [len (count coll)
          row (fn [indexs] (map #(nth coll %) indexs))]
      (map #(row (range % len n)) (range n)))))

(defcheck solution-bce180b0
  (fn reverse-interleave [coll n]
    (loop [coll coll n n agg []]
      (if (= n 0) agg
                  (let [drop-nth (keep-indexed #(if (not= 0 (mod %1 n)) %2 nil) coll)]
                    (recur drop-nth (dec n) (conj agg (take-nth n coll))))))))

(defcheck solution-bd541c05
  (fn rev-inter [c n]
    (apply map list (partition n c))))

(defcheck solution-bd6b9d44
  (fn rev-interleave [xs n]
    (->>
      xs
      (map-indexed vector)
      (group-by #(mod (first %) n))
      (map second)
      (map (fn [s] (map #(second %) s)))
      )))

(defcheck solution-bd8da47e
  (fn reverse-interleave- [coll partitions]
    "43. Write a function which reverses the interleave process into x number of subsequences."
    (for [x (range partitions)]
      (concat (take-nth partitions (drop x coll))))))

(defcheck solution-bda9aaf9
  (fn [coll n]
    (map #(take-nth n %)
      (map #(drop % coll) (range n)))))

(defcheck solution-be4f85cf
  (fn [c n] (for [i (range n)] (map #(nth % i) (partition n c)))))

(defcheck solution-bfb329a5
  (fn [s n] (reduce #(map conj %1 %2) (repeat n []) (partition n s))))

(defcheck solution-c040135
  (fn [a n]
    (loop [acc (repeat n [])
           org a]
      (if-not (empty? org)
        (recur (map-indexed #(conj %2 (nth org %1)) acc) (drop n org))
        acc))))

(defcheck solution-c0bf6036
  (fn [s x]
    (vals (group-by #(rem % x) s))))

(defcheck solution-c0ce6d5
  (fn [s n] (for [i (range n)] (take-nth n (drop i s)))))

(defcheck solution-c1b492d1
  (fn [v n] (vals (group-by #(rem (.indexOf v %) n) v))))

(defcheck solution-c28249f6
  (fn reverse-interleave [coll n-lists]
    (let [s          (doall (vec coll))
          n-per-list (/ (count s) n-lists)]
      (partition
        n-per-list
        (for [x (range n-lists)
              y (range n-per-list)]
          (s (+ x (* y n-lists))))))))

(defcheck solution-c2f73c89
  (fn demultiplex [xs n]
    (let [nth-only (fn nth-only [n xs] (map first (partition n n '() xs)))]
      (map #(nth-only n (drop % xs)) (range 0 n)))))

(defcheck solution-c35860d5
  (fn [icoll n]
    (loop [coll icoll
           res  (repeat n '())]
      (if (empty? coll) res
                        (let [first-res (concat (first res) (list (first coll)))
                              rest-res  (rest res)]
                          (recur (rest coll) (concat rest-res (list first-res)))
                          )
                        )
      )
    ))

(defcheck solution-c3b539f2
  ;(fn reverse-interl [coll n]
  ;   (loop [counter (first coll)
  ;          remainder (first coll)
  ;          ans []]
  ;     (if (= counter (+ (first coll) n))
  ;      ans
  ;      (recur (inc counter)
  ;             (inc remainder)
  ;             (conj ans (if (= remainder n)
  ;                         (filter #(= 0 (rem % n)) coll)
  ;                         (filter #(= remainder (rem % n)) coll)))))))



  (fn rev-inter [lat n]
    (partition (/ (count lat) n) (apply interleave (partition n lat)))))

(defcheck solution-c40a25fd
  (fn reverse-interleave-2
    [xs n]
    (apply map list (partition n xs))))

(defcheck solution-c4ec0f5b
  (fn [coll n]
    (vals (group-by #(rem (.indexOf coll %) n) coll))))

(defcheck solution-c4f2f156
  (fn [xs n] (map reverse (reduce #(map cons %2 %1) (repeat n nil) (partition n xs)))))

(defcheck solution-c584fd3e
  (fn ril [s x] (apply map list (partition-all x s))))

(defcheck solution-c5974041
  #(for [x (range %2)] (take-nth %2 (drop x %))))

(defcheck solution-c5a4a5db
  (fn [xs n]
    (letfn [(f [ys i]
              (if (= i n)
                ()
                (cons (take-nth n ys)
                  (f (rest ys) (inc i)))))]
      (f xs 0))))

(defcheck solution-c5f93426
  (fn [v n] (for [i (range n)] (keep-indexed #(if (= (mod %1 n) i) %2) v))))

(defcheck solution-c609b2d8
  (fn [z n] (letfn [(subtract-lists [l1 l2] (filter (fn [r] (not (some #(= r %) l2))) l1))
                    (create [l] (if-let [[h & t] (seq l)] (let [s (filter #(<= % (last l)) (take (count l) (iterate (partial + n) h)))]
                                                            (lazy-seq (cons s (create (subtract-lists l s)))))))]
              (create z))))

(defcheck solution-c6137591
  (fn [coll n]
    (let [parts (partition n coll)]
      (map (fn [i] (map #(nth %1 i) parts)) (range n)))))

(defcheck solution-c65fa74e
  (fn reverse-interleave [xs n]
    (let [ys (partition-all n xs)]
      (partition-all (count ys) (apply interleave ys)))))

(defcheck solution-c66e1c89
  (fn [coll n]
    (apply map conj (repeat n []) (partition-all n coll))))

(defcheck solution-c68f6a52
  (fn [coll n]
    (reduce (fn [acc val]
              (map #(conj %1 %2) acc val))
      (repeat n [])
      (partition n coll))))

(defcheck solution-c6923aa7
  #(for [i (range %2 0 -1)]
     (apply concat (partition 1 %2 (drop (- %2 i) %)))))

(defcheck solution-c699d438
  (fn [xs n]
    (map
      (fn [lst]
        (map
          (fn [pair] (last pair))
          lst))
      (vals (group-by #(first %)
              (map-indexed #(list (mod %1 n) %2) xs))))))

(defcheck solution-c69c124e
  #(let [p (partition-all %2 %)]
     (for [n (range %2)]
       (for [s p]
         (nth s n)))))

(defcheck solution-c6b3b41
  #(for [i (range %2)] (take-nth %2 (drop i %))))

(defcheck solution-c7adf1ef
  (fn foo [s n]
    (apply map (cons list (partition n s)))))

(defcheck solution-c8070c4f
  (fn [xs n]
    (reduce
      (fn [buckets [v i]]
        (update-in buckets [i]
          (fn [x] (conj x v))))
      (vec (map (fn [_] []) (range n)))
      (map vector xs (cycle (range n))))))

(defcheck solution-c80d58c3
  (fn [s n]
    (loop [s s out (vec (repeat n [])) i 0]
      (if (empty? s)
        out
        (recur (rest s) (assoc out i (conj (nth out i) (first s))) (mod (inc i) n))))))

(defcheck solution-c8171568
  (let [unwrap-accum (fn [a c]
                       (loop [accum []
                              curr  1]
                         (let [new-accum (conj accum (a curr))]
                           (if (= curr c)
                             (reverse (map reverse new-accum))
                             (recur new-accum (+ 1 curr))))))]
    (fn reverse-interleave
      [s c]
      (loop [[h & t] s
             accum {}
             curr  c]
        (let [new-accum (assoc accum curr (conj (accum curr) h))
              new-curr  (if (= curr 1) c (- curr 1))]
          (if (nil? t)
            (unwrap-accum new-accum c)
            (recur t new-accum new-curr)))))))

(defcheck solution-c84871b6
  (fn reverse-interleave [s n]
    (for [x (range n)]
      (for [y (range 0 (count s) n)]
        (nth s (+ x y))))))

(defcheck solution-c889081e
  (fn rev-interleave [xs n]
    (map
      (fn [k] (apply concat (partition 1 n (drop k xs))))
      (range n))))

(defcheck solution-c88a1f11
  (fn [c n]
    (reduce #(map (fn [x y] (concat x y)) %1 %2) (map #(partition 1 %) (partition n c)))))

(defcheck solution-c893a2ba
  (fn reverse-interleave [coll n]
    (sort-by first (vals (reduce #(let [k (mod %2 n)
                                        v (get %1 k [])]
                                    (assoc %1 k (conj v %2))) {} coll)))))

(defcheck solution-c8c53f8
  (fn [s n]
    (letfn [(chop [s n]
              (if (empty? s)
                {}
                (cons (take n s) (chop (nthrest s n) n))))]
      (apply map vector (chop s n)))))

(defcheck solution-c99812ea
  (fn tw [xs n]
    (for [i (range n)]
      (take-nth n (drop i xs)))))

(defcheck solution-c9bf23a1
  #(for [x (range %2)]
     (for [y (take-nth %2
               (drop x %))] y)))

(defcheck solution-ca1d9a21
  (fn [l n] (vals (group-by #(rem % n) l))))

(defcheck solution-ca789e18
  (fn reverse-interleave [lst n]
    (if (empty? lst)
      nil
      (let [fst (first lst)
            grp (filter #(zero? (mod (- % fst) n)) lst)]
        (cons grp (reverse-interleave (remove (set grp) lst) n))))))

(defcheck solution-cac5b022
  (fn [coll n]
    (map (fn [x] (take-nth n (drop x coll))) (range n))))

(defcheck solution-caef73fd
  (fn [xs n]
    (loop [xs xs n n c n res []]
      (if (zero? c)
        res
        (recur (rest xs) n (dec c) (conj res (take-nth n xs)))))))

(defcheck solution-cb12a16f
  (let [strip (fn strip [x y z]
                (filter #(= 0 (mod (+ % y) z)) x))]
    (fn ri
      ([x y] (ri x y y))
      ([x y z]
       (if (< y 2)
         (list (strip x y z))
         (sort #(compare (first %1) (first %2))
           (concat (list (strip x y z))
                   (ri x (- y 1) z))))))))

(defcheck solution-cb95acbc
  (fn [coll n]
    ((fn step [coll i]
       (if (> i n)
         nil
         (concat [(take-nth n coll)]
                 (step (rest coll) (inc i)))
         )
       ) coll 1)
    ))

(defcheck solution-cbb8f3d7
  (fn [coll num-seqs]
    (let [seq-indices (for [x (range 0 num-seqs)]
                        (set (range x (count coll) num-seqs)))]
      (for [seq-index seq-indices]
        (keep-indexed #(if (seq-index %1) %2) coll)))))

(defcheck solution-cc1eabdc
  (fn [l n]
    (loop [news '() c 0 tmpl l]
      (if (= n c)
        (reverse news)
        (recur (conj news (take-nth n tmpl)) (inc c) (rest tmpl))))))

(defcheck solution-cc8d3167
  (fn [xs n]
    (loop [xs xs ys (repeat n [])]
      (if (seq xs)
        (recur (drop n xs) (map conj ys (take n xs)))
        ys))))

(defcheck solution-cc8eb4a1
  #(map second (group-by (fn [i] (mod i %2)) %)))

(defcheck solution-cd2f8b5d
  #(map (fn [start]
          (reduce (fn [acc x] (conj acc (nth % x))) [] (range start (count %) %2))) (range %2)))

(defcheck solution-cd4c0a8c
  (fn number43 [xs n]
    (apply map list (partition-all n xs))))

(defcheck solution-cd5bfa9e
  (fn rev-interleave
    [s n]
    (for [i (range n)]
      (take-nth n (drop i s)))))

(defcheck solution-cdb64f0c
  (fn revinter [alist divi]
    (for [i (range divi)]
      (->>
        (filter #(= i (rem % divi)) (range (count alist)))
        (map #(nth alist %))))))

(defcheck solution-cdc3b1d4
  (fn [l x] (vals (group-by #(mod % x) l))))

(defcheck solution-ceeb9b0f
  (fn reverse-interleave [a-seq n]
    (let [mapped   (group-by #(first %) (map-indexed #(list (rem % n) %2) a-seq))
          remapped (into {} (for [[k v] mapped] [k (map #(second %) v)]))]
      (map #(remapped %) (range n))
      )
    ))

(defcheck solution-cf62515b
  (fn reverse-interleave [coll l]
    (let [parts (partition l coll)]
      (for [n (range l)]
        (map #(nth %1 n) parts)))))

(defcheck solution-cf7145a2
  (fn __ [s n]
    ((fn ! [m]
       (if (= m n) '()
                   (cons
                     (keep-indexed #(if (= (mod %1 n) m) %2) s)
                     (! (inc m)))))
     0)))

(defcheck solution-cf7c4e14
  (fn [xs n]
    (loop [ys xs rs (for [x (range n)] [])]
      (if (empty? ys)
        rs
        (let [[y & ys'] ys
              [r & rs'] rs
              r'   (conj (into [] r) y)
              rs'' (conj (into [] rs') r')]
          (recur ys' rs''))))
    ))

(defcheck solution-d037aa10
  (fn [coll n]
    (reduce
      (fn [a e] (map conj a e))
      (repeat n []) (partition n coll))))

(defcheck solution-d0383c95
  (fn [xs n]
    (reduce
      #(loop [i 0 ys %1]
         (if (< i n)
           (recur (inc i) (concat (take i ys) [(concat (nth ys i) [(nth %2 i)])] (drop (inc i) ys)))
           ys
           )
         )
      (repeat n [])
      (partition n n nil xs)
      )
    ))

(defcheck solution-d041c897
  (fn unleave [s n]
    (apply map list (partition n s))))

(defcheck solution-d0afdebe
  (fn reverseleave [xs x]
    (let [ys (partition x xs)]
      (apply map (partial conj []) ys))))

(defcheck solution-d13e477a
  #(map (partial take-nth %2)
     (take %2 (iterate rest %1))))

(defcheck solution-d1ebb2ac
  (fn x [c n]
    (loop [c c o (take n (repeat []))]
      (if (empty? c)
        o
        (recur (drop n c) (map conj o (take n c)))))))


(defcheck solution-d2521ed7
  #(reduce (fn [acc, n] (conj acc (flatten (partition 1 %2 (drop n %))))) [] (range %2)))

(defcheck solution-d30b4fac
  (fn f [x n]
    (map #(apply concat (partition 1 n (drop % x))) (range n))))

(defcheck solution-d3b344de
  (fn [s n] (map #(take-nth n %) (map #(drop % s) (range 0 n)))))

(defcheck solution-d3c773a4
  (fn [s x]
    (map #(flatten (map second %)) (vals (group-by #(first %) (map vector (map #(mod % x) (rest (range))) s))))))

(defcheck solution-d3f24a0f
  (fn [s n]
    (map #(take-nth n (nthnext s %)) (range 0 n))))

(defcheck solution-d40477f4
  (fn
    [seq n]
    (loop [newseq '() i 0]
      (if (< i n)
        (recur (conj newseq (filter #(= (rem (.indexOf seq %) n) i) seq)) (inc i))
        (reverse newseq)))
    ))

(defcheck solution-d58e3c09
  (fn revinter [x y]
    (loop [x x count 0 outer 0 res (into [] (repeat y '[]))]
      (cond (empty? x) res
            (= count (- y 1))
            (recur (rest x) 0 (+ outer 1) (assoc-in res [count outer] (first x)))
            :else
            (recur (rest x) (+ count 1) outer (assoc-in res [count outer] (first x)))
            )
      )
    ))

(defcheck solution-d5c08c34
  (fn rev-interleave [coll n]
    (map #(take-nth n (nthrest coll %)) (range n))))

(defcheck solution-d6146f4d
  (fn [c n] (map (fn [i] (map #(nth % i) (partition n c))) (range n))))

(defcheck solution-d7579cd6
  #(apply map (fn [& args]
                args)
     (partition %2 %1)))

(defcheck solution-d7e6c4dc
  (fn unint
    [coll x]
    (loop [acc  (repeat x [])
           pos  0
           curr (first coll)
           left (rest coll)]
      (if (= curr nil)
        acc
        (recur
          (concat
           (take (mod pos x) acc)
           (vector (conj (nth acc (mod pos x)) curr))
           (drop (inc (mod pos x)) acc)
           )
          (inc pos)
          (first left)
          (rest left))))))

(defcheck solution-d7e77161
  (fn [coll n]
    (reduce (partial map conj)
      (repeat n [])
      (partition n coll))))

(defcheck solution-d8174820
  (fn [x n] (loop [m (first x), res []] (if (> (+ n (first x)) m) (recur (inc m) (conj res (filter #(= (mod m n) (mod % n)) x))) res))))

(defcheck solution-d8bf3dfc
  #(for [k (range %2)] (for [t (partition %2 %)] (nth t k))))

(defcheck solution-d8fe0747
  (fn [v n]
    (apply map vector (partition n v))))

(defcheck solution-d95bbcc
  (fn [xs n]
    (apply map list (partition n xs))))

(defcheck solution-d9b8e700
  #(for [x (range %2)] (take-nth %2 (drop x %1))))

(defcheck solution-d9c2d0fa
  (fn [coll parts] (for [n (range parts)] (take-nth parts (drop n coll)))))

(defcheck solution-da1c5855
  (fn [coll n] (map #(take-nth n %) (take n (iterate rest coll)))))

(defcheck solution-da1e5786
  #(vals (group-by (fn [x] (mod x %2)) %)))

(defcheck solution-da3d9db3
  (fn [c n]
    (for [i (range 0 n)]
      (for [j (range i (count c) n)]
        (nth c j)))))

(defcheck solution-da6026e
  #(apply (partial map list) (partition %2 %1)))

(defcheck solution-daeefe3a
  (fn [coll n]
    (let [f (zipmap (range) coll) cnt (/ (count coll) n)]
      (map (fn [x] (map f (take cnt (iterate #(+ % n) (+ 0 x))))) (range n)))))

(defcheck solution-db2dd10a
  (fn [s n]
    (apply map vector (partition n s)
      )))

(defcheck solution-db3f6d64
  (fn reverse-interleave
    [coll n]
    (->> (partition n coll)
      (apply interleave)
      (partition (/ (count coll) n)))))

(defcheck solution-db631c80
  (fn [coll n] (for [x (range 1 (inc n))] (for [y (range (/ (count coll) n))] (nth coll (+ (* y n) (dec x)))))))

(defcheck solution-db8e1e43
  (fn [xs n]
    (->> xs
      (map-indexed list)
      (group-by #(mod (first %) n))
      (apply concat)
      (apply sorted-map)
      (vals)
      (map #(map second %)))))

(defcheck solution-dba4f567
  (fn [xs n]
    (map (fn [i] (take-nth n (drop i xs)))
      (range n))))

(defcheck solution-dbac6bfb
  (fn [c n] (partition (quot (count c) n) (apply interleave (partition n c)))))

(defcheck solution-dbbcfff
  (fn [coll n] (apply map list (partition n coll))))

(defcheck solution-dc9a6d2e
  (fn reverse-interleave [xs n]
    (letfn [(stepper [col nb-items step limit]
              (when (pos? limit)
                (cons (take nb-items (take-nth step col))
                  (stepper (next col) nb-items step (dec limit)))))]
      (stepper xs (/ (count xs) n) n n))))

(defcheck solution-dd648cb2
  (fn [s n]
    (for [x (range n)]
      (map
        #(nth s %1)
        (range x (+ x (count s)) n)))))

(defcheck solution-dd7a8ab3
  (fn [coll n]
    (for
     [[k v] (group-by
              #(rem (% 1) n)
              (map #(vector % %2) coll (range)))]
      (map first v))))

(defcheck solution-dd849b90
  (fn [col n]
    (apply map list
      (loop [l col acc []]
        (if (empty? l)
          acc
          (let [[f r] (split-at n l)]
            (recur r (conj acc f))))))))

(defcheck solution-de48780c
  (fn reverse-interleave
    [s n]
    (reduce #(conj %1 (take-nth n (nthrest s %2)))
      []
      (range n))))

(defcheck solution-deb60863
  #(apply map vector (partition %2 %1)))

(defcheck solution-df0ee478
  (fn ri [l n]
    (letfn [(helper [l n i]
              (if (< i n)
                (cons (take-nth n l) (helper (rest l) n (inc i)))
                '()))]
      (helper l n 0))))

(defcheck solution-df4cc292
  (fn [coll n]
    (reduce (fn [result values]
              (map conj result values))
      (repeat n [])
      (partition n coll))))

(defcheck solution-dfa45404
  (fn [coll n] (for [i (range n)]
                 (take-nth n (drop i coll)))))

(defcheck solution-e0152a46
  (fn [l n]
    (vals (group-by #(mod % n) l))))

(defcheck solution-e04fb5b7
  (fn reverse-interleave [s n]
    (apply map list (partition n s))))

(defcheck solution-e0bce1e5
  (fn reverse-interleave
    [coll n]
    (partition (/ (count coll) n) (sort #(compare (mod (.indexOf coll %) n) (mod (.indexOf coll %2) n)) coll))))

(defcheck solution-e185484
  (fn [coll i]
    (apply map vector (partition i coll))))

(defcheck solution-e1a58790
  (fn [xs n]
    (let [xss (take n (iterate rest xs))]
      (map #(take-nth n %) xss))))

(defcheck solution-e24122de
  (fn unweave [coll n]
    (map #(->> coll (drop %) (take-nth n)) (range n))))

(defcheck solution-e2502603
  (fn f
    [d c]
    (map #(take-nth c (drop % d)) (range c))))

(defcheck solution-e27caf0b
  #((fn [l] (partition (count l) (count l) nil (apply interleave l))) (partition %2 %2 nil %1)))

(defcheck solution-e2a0378d
  (fn myfn [myseq step]

    (let
     [start (first myseq)
      end   (last myseq)
      cnt   (count myseq)

      mysub (fn [step cnt s]

              (

                loop [
                      inner-cnt (/ cnt step)
                      end       (+ s (* step (dec inner-cnt)))
                      res       [s]
                      cur       s]

                (if (= cur end)
                  res
                  (recur inner-cnt end (concat res [(+ cur step)]) (+ cur step))
                  ))
              )

      ]

      ;(println (range start (+ start step)))

      (if (= start 0)
        (map (partial mysub step cnt) (range start (+ start step)))
        (map (partial mysub step cnt) (range start (+ start step)))
        )

      )

    ))

(defcheck solution-e320f8c0
  (fn [s n]
    (map (fn [x]
           (map (fn [y]
                  (nth s y)) x))
      (apply map
        vector
        (partition n
          (range (inc (count s))))))))

(defcheck solution-e36efa44
  (fn [coll n] (vals (group-by #(mod % n) coll))))

(defcheck solution-e391f17c
  (fn [xs c]
    (apply map vector (partition c xs))))

(defcheck solution-e405417d
  (fn [col n]
    (let [a (/ (count col) n)]
      (->> col
        (partition n)
        (apply interleave)
        (partition a)))))

(defcheck solution-e4e3a6cd
  (fn [coll n]
    (for [i (range n)]
      (flatten (partition 1 n (drop i coll))))))

(defcheck solution-e5db1495
  (fn unleave [s n]
    ((fn ulh [s n c co ss]
       (if (= n c) ss
                   (cons (take-nth n s)
                     (ulh (rest s) n (inc c) co ss)))) s n 0 (count s) [])))

(defcheck solution-e653b2f8
  (fn [s n]
    (letfn [(nths [s offset]
              (letfn [(hf [result i s]
                        (cond (empty? s) result
                              (= 0 (mod i n)) (recur
                                                (conj result (first s))
                                                (inc i)
                                                (rest s))
                              :else (recur result (inc i) (rest s))))]
                (hf [] (- n offset -1) s)))
            (ri-helper [result i]
              (if (> i n)
                result
                (recur
                  (conj result (nths s i))
                  (inc i))))]
      (ri-helper [] 1))))

(defcheck solution-e66cd372
  (fn [se n]
    (map (fn [x] (map second x)) (vals (group-by (fn [[index val]] (mod index n)) (map-indexed vector se))))))

(defcheck solution-e6a702f4
  (fn [coll n] (map #(take-nth n (drop % coll)) (range n))))

(defcheck solution-e7027859
  (fn revint [v i] (apply map (fn [& s] s) (partition i v))))

(defcheck solution-e721178b
  (fn [seq n]
    (for [i (range n)] (take-nth n (drop i seq)))))

(defcheck solution-e760a717
  #(for [i (range %2)
         :let [s (take-nth %2 (drop i %1))]]
     s))

(defcheck solution-e7b0c8f5
  (fn [coll n] (apply map vector (partition n coll))))

(defcheck solution-ea176c4e
  (fn reverse-interleave [xs n]
    (letfn [(take-nth [xs n]
              (loop [i 0 xs xs accum []]
                (cond (empty? xs) accum
                      (zero? (mod i n)) (recur (inc i) (rest xs) (conj accum (first xs)))
                      :else (recur (inc i) (rest xs) accum))))]
      (map #(take-nth (drop % xs) n) (range n)))))

(defcheck solution-ea93d62d
  (fn [s n]
    (first
      (reduce
        (fn [[ret [i & r]] x]
          [(assoc ret i (conj (ret i) x)) r])
        [(vec (repeat n [])) (cycle (range n))]
        s))))

(defcheck solution-eb15dc63
  (letfn [
          (unzip [colls]
            (apply map vector colls))
          ]
    #(unzip (partition %2 %1))))

(defcheck solution-eb735222
  (fn [nums n]
    ((fn helper [pseudo-nums m ans]
       (if (= m n)
         ans
         (loop [remaining pseudo-nums tmp []]
           (if (empty? remaining)
             (helper (drop 1 pseudo-nums) (inc m) (conj ans tmp))
             (recur (drop n remaining) (conj tmp (first remaining))))))) nums 0 [])))

(defcheck solution-ebcf6ccf
  (fn [coll n] (sort-by first
                 (partition
                   (/ (count coll) n)
                   (sort-by #(mod % n) coll)))))

(defcheck solution-ec080441
  #(apply mapcat (fn [& c] (list* c []))
     (partition %2 %1)))

(defcheck solution-ec344c3
  (fn [ll xx]

    ((fn f [res l x curr]
       (if
        (= curr 0) res
                   (f
                     (concat
                      (list
                        (filter
                          #(= (mod (.indexOf l %) x) (- curr 1))
                          l))
                      res)
                     l
                     x
                     (- curr 1)))) () ll xx xx)))

(defcheck solution-ec997456
  (fn [l n] (map (fn [i] (take-nth n (drop i l))) (range n))))

(defcheck solution-ecd74b95
  (fn [c n]
    (map (partial map second) (vals (group-by first (partition 2 (interleave (cycle (range n)) c)))))))

(defcheck solution-ecd7cab8
  (fn my_anti-interleave [l n] (if l (map conj (my_anti-interleave (seq (drop n l)) n) (take n l)) (repeat n ()))))

(defcheck solution-ecf9d935
  (fn [l n] (map #(take-nth n (drop % l)) (range n))))

(defcheck solution-ed07eead
  (fn [lst n]
    (loop [acc [] x n l lst]
      (if (= 0 x) acc
                  (recur (conj acc (take-nth n l)) (dec x) (rest l))))))

(defcheck solution-ed89a7f6
  (fn [s n]
    (apply map list (partition n s))))

(defcheck solution-ee3a1269
  (fn f [coll n]
    (let [indexed (keep-indexed #(list (rem %1 n) %2) coll)]
      (loop [i   0
             acc '()]
        (if (< i n)
          (recur (+ i 1) (concat acc (list (map #(last %) (filter #(= i (first %)) indexed)))))
          acc
          )
        )
      )))

(defcheck solution-ef1ec709
  (fn rev-interleave
    [xs point]
    (let [partitioned (partition point xs)
          helper      (fn [coll accum]
                        (if (some empty? coll)
                          accum
                          (recur (map rest coll) (conj accum (map first coll)))))]
      (helper partitioned []))))

(defcheck solution-ef86deb2
  (fn [ss j]
    (for [n (range j)]
      (map #(nth % n) (partition j ss)))))

(defcheck solution-f057f97f
  #(vals (group-by (fn [itm] (mod itm %2)) %1)))

(defcheck solution-f06e1165
  ;(fn [l m]
  ;            (for [k (range m)]
  ;              (for [[i j] (map-indexed vector l) :when (zero? (rem (- i k) m))] j)))

  (fn [s n]
    (map (fn [i]
           (map (fn [ss] (nth ss i))
             (partition n s)))
      (range n))))

(defcheck solution-f17a56bb
  (fn rint [s n]
    (loop [s s pos 0 res (vec (take n (repeat [])))]
      (if (not (seq s)) res
                        (recur (rest s)
                          (mod (inc pos) n)
                          (assoc-in res [pos] (conj (res pos) (first s))))))))

(defcheck solution-f1a7a399
  (fn [coll n]
    (reduce (fn [r chunk]
              (map #(conj %1 %2) r chunk))
      (repeat n [])
      (partition n coll))))

(defcheck solution-f1e7453d
  #(let [pieces %2]
     (apply map list (vec (map vec (partition-all pieces pieces %1))))))

(defcheck solution-f1eb4216
  (fn [coll n]
    (let [new-coll (partition n coll)]
      (partition (/ (count coll) n) (apply interleave new-coll)))))

(defcheck solution-f1edcc48
  (fn sp [sx n] (partition (/ (count sx) n) (apply interleave (partition n sx)))))

(defcheck solution-f2ff19c5
  (fn [coll n]
    (loop [new [] c 0]
      (if (= n c)
        new
        (recur (conj new (keep-indexed #(if (= c (rem %1 n)) %2) coll)) (inc c))))))

(defcheck solution-f303aed1
  (fn [xs n] (for [i (range n)] (take-nth n (drop i xs)))))

(defcheck solution-f34a336c
  (fn [lst n]
    (let [ls (partition n lst)]
      (map (fn [i] (map (comp first (partial drop i)) ls)) (range n)))))

(defcheck solution-f4d75121
  (fn [xs n] (partition (/ (count xs) n) (apply interleave (partition-all n xs)))))

(defcheck solution-f50e6752
  (fn dein [ran nu]
    (if (< (count ran) nu)
      (repeat nu nil)
      (let [heads (take nu ran)
            rests (dein (drop nu ran) nu)]
        (map #(cons %1 %2) heads rests)))))

(defcheck solution-f5292b7f
  (fn rev-int [s n] (for [i (range n)] (take-nth n (nthnext s i)))))

(defcheck solution-f54b3a25
  (fn [xs n]
    (let [groups (group-by first (map-indexed (fn [i el] (list (rem i n) el)) xs))
          ks     (sort (keys groups))]
      (->> ks
        (map (fn [k] (map second (get groups k))))))))

(defcheck solution-f592e6c8
  #(map (partial take-nth %2)
     (take %2
       (iterate (partial drop 1) %))))

(defcheck solution-f5f51d70
  (fn ri [s n]
    (apply map list (partition n s))))

(defcheck solution-f641283e
  (fn [coll n]
    (let [grouped (group-by first (map vector (cycle (range n)) coll))]
      (reduce #(conj %1 %2)
        []
        (map #(map second (get grouped %))
          (sort (keys grouped)))))))

(defcheck solution-f6978dca
  (fn rev-interleave [s n]
    (map #(take-nth n %) (map #(drop % s) (range n)))))

(defcheck solution-f6b172fd
  (fn [s n] (apply map vector (partition n s))))

(defcheck solution-f6fd66c8
  (fn [coll wedge]
    (apply map list (partition wedge coll))))

(defcheck solution-f718a92e
  (fn [x n]
    (apply map list (partition n x))))

(defcheck solution-f7c648d
  (fn [c n] (map #(take-nth n (drop % c)) (range n))))

(defcheck solution-f856186d
  (fn [xs n]
    (->> (interleave (flatten (repeat (range n))) xs)
      (partition 2)
      (group-by first)
      vals
      (map (partial map second)))))

(defcheck solution-fa84ea73
  (fn [ls x]
    (map
      #(flatten (partition 1 x (drop % ls)))
      (range x))
    ))

(defcheck solution-fb607ac0
  (fn [x n]
    (map
      (partial take-nth n)
      (take n (iterate next x)))))

(defcheck solution-fc622031
  (fn [seq n]
    (apply map list (partition n seq))))

(defcheck solution-fcbb32d0
  (fn [s n]
    (apply (partial map vector) (partition n s))))

(defcheck solution-fe4e5aac
  (fn [xs n]
    (reduce
      #(let [m (mod (%2 1) n)]
         (assoc %1 m
                   (conj (%1 m) (first %2))))
      (vec (repeat n []))
      (map vector xs (range)))))

(defcheck solution-fec19f20
  (fn undointer [coll n]
    (map
      #(take-nth n (nthnext coll %))
      (range n))))

(defcheck solution-ff1490e8
  (fn [coll n]
    (let [parts (partition n coll)]
      (loop [rems parts, out []]
        (if (seq (first rems))
          (recur (map rest rems) (conj out (map first rems)))
          out)))))

(defcheck solution-ff2863da
  (fn revInter [x y] (if (= y 0) x (concat (list (take-nth y x)) (revInter (flatten (map #(rest %) (partition y x))) (dec y))))))

(defcheck solution-ff76990e
  (fn [coll n] (sort-by #(first %) (partition-by #(mod % n) (sort-by #(mod % n) coll)))))

(defcheck solution-ffede6c9
  (fn [c n] (apply map list (partition n c))))

(defcheck solution-ffee0f2d
  (fn [s n]
    (reverse
      (reduce #(conj (butlast %1) (conj (last %1) %2))
        (repeat n []) s))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-43))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
