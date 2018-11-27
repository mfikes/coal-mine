(ns coal-mine.problem-49
  (:require [coal-mine.checks :refer [defcheck-49] :rename {defcheck-49 defcheck}]
            [clojure.test]))

(defcheck solution-110f945b
  (fn [n x]
    (list (take n x) (take-last (- (count x) n) x))))

(defcheck solution-112c9964
  (fn [n c] [(take n c) (drop n c)]))

(defcheck solution-12000198
  (fn [n s]
    (cons (take n s) (list (drop n s)) )
    ))

(defcheck solution-125e8b8c
  (fn f [n s]
    (->> (map-indexed (fn [i x] [i x]) s)
      (partition-by #(<= n (first %)))
      (map #(map second %)))))

(defcheck solution-12f3a78f
  (fn [i coll]
    [(subvec coll 0 i)
     (subvec coll i)]))

(defcheck solution-130baf11
  (fn [n coll] (list (take n coll) (drop n coll))))

(defcheck solution-1379dcbf
  (fn [s c] [(take s c) (drop s c)]  ))

(defcheck solution-14216d19
  (fn [n col] [(take n col) (drop n col)]))

(defcheck solution-14c598b4
  (fn split [n s]
    (cons (take n s)
      (list (drop n s)))))

(defcheck solution-153e66dc
  (fn [n mycol]
    (let
     [splitSeqRec
      (fn splitRec
        [a b n]
        (if
         (= n 0)
          (vector a b)
          (splitRec
            (conj a (first b))
            (rest b)
            (- n 1)
            )
          )
        )
      ]
      (splitSeqRec [] mycol n)
      )
    ))

(defcheck solution-161c421a
  #(cons (take %1 %2) (list (drop %1 %2))))

(defcheck solution-16d110dd
  #(vector (take %1 %2)
     (drop %1 %2)))

(defcheck solution-16e6691b
  (fn [value coll]
    [(take value coll)
     (drop value coll)]))

(defcheck solution-17e75ffd
  (fn splt-at [c s]
    (if (= c 0)
      [[] s]
      (let [[front back] (splt-at (dec c) (rest s))]
        [(cons (first s) front) back]))))

(defcheck solution-183c9b1
  (fn [n coll]
    (vector (take n coll) (drop n coll))))

(defcheck solution-19e3e87e
  (fn
    [n seq]
    (vector (into [] (take n seq)) (into [] (drop n seq)))
    ))

(defcheck solution-1ac71c29
  (fn [n s]
    (let [indexed (fn [l] (map vector (iterate inc 0) l) )
          v (indexed s)
          [p r] (split-with #(> n (first %)) v) ]
      [(map second p) (map second r)]
      )
    ))

(defcheck solution-1ad0236a
  (fn test1 [s v] [(reduce conj [] (take s v)) (reduce conj [] (nthnext v s))]))

(defcheck solution-1b81433e
  (fn [n xs] [(take n xs)(drop n xs)]))

(defcheck solution-1b871a5
  (fn [x s] [(take x s) (drop x s)]))

(defcheck solution-1ccda5d8
  #(list (take % %2)(drop % %2)))

(defcheck solution-1d58bb17
  (fn [n xs] (conj '() (drop n xs) (take n xs))))

(defcheck solution-1eb0030f
  (fn splitter [n l]
    (loop [acc [] i n r l]
      (if (zero? i)
        (cons acc [r])
        (recur (conj acc (first r)) (dec i) (rest r))))))

(defcheck solution-1ebb5ac1
  (fn [x y] (vector (vec (keep-indexed #(if (< %1 x) %2) y)) (vec (keep-indexed #(if (>= %1 x) %2) y)))))

(defcheck solution-1efb0220
  (fn [n lst] (vec (concat [(take n lst)] [(drop n lst)]))))

(defcheck solution-1f1302b9
  #(concat (vector (take %1 %2)) (vector (drop %1 %2))))

(defcheck solution-2347207f
  #(-> [(vec (take %1 %2)) (vec (drop %1 %2))]))

(defcheck solution-23916c75
  (fn my-split-at [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-239171c1
  (fn [i c] [(take i c) (drop i c)]))

(defcheck solution-23a59856
  (fn split [n,s] [(subvec s 0 n) (subvec s n (count s))]))

(defcheck solution-23b3ef3c
  (fn [n seq] [(take n seq) (nthrest seq n)]))

(defcheck solution-23d0db4a
  (fn [n s] [(take n s) (take-last (- (count s) n) s)]))

(defcheck solution-25ef336
  #(into (list (subvec %2 %1)) (list (subvec %2 0 %1))))

(defcheck solution-26087659
  (fn sat [n s]
    (list (take n s) (drop n s))
    ))

(defcheck solution-263f3bcf
  #(vec [(take % %2) (drop % %2)]))

(defcheck solution-266e1b79
  (fn [n s]
    [(take n s) (drop n s)]))

(defcheck solution-27b03533
  (fn my-split-at [n xs]
    ((fn step [acc xs idx limit]
       (if (= idx limit)
         (conj [] acc (into [] xs))
         (step (conj acc (first xs)) (next xs) (inc idx) limit)))
     [] xs 0 n)))

(defcheck solution-27b5ebfa
  (fn [n col]
    (list (take n col) (drop n col))))

(defcheck solution-27b626fa
  (comp #(list (first %) (apply concat (next %))) partition-all))

(defcheck solution-27e1fc91
  (fn splitX [n x] [(into (empty x) (take n x))(into (empty x) (drop n x))]))

(defcheck solution-280aab42
  (fn split [n x]
    (loop [remaining n
           head      []
           tail      x]
      (if (zero? remaining)
        (vector (vec head) (vec tail))
        (recur (dec remaining)
          (conj head (first tail))
          (rest tail))))))

(defcheck solution-28cf18f4
  (fn [l c] (map #(% l c) [take drop])))

(defcheck solution-293dd063
  (fn [i coll]
    (loop [left [] right coll index 0]
      (if (= index i)
        (conj [] left right)
        (recur (conj left (first right)) (rest right) (inc index))
        )

      )


    ))

(defcheck solution-29588557
  (fn [n s] (cons (take n s) [(drop n s)])))

(defcheck solution-29632786
  (fn [n xs]
    (list
      (take n xs)
      (loop [m n ys (seq xs)]
        (if (= 0 m) ys (recur (dec m) (next ys)))))))

(defcheck solution-2c5b41b1
  #(conj (conj () (drop % %2)) (take % %2)))

(defcheck solution-2caa9ff7
  (fn meu-split [n seq]
    (cons
      (map #(nth seq %) (range 0 n))
      (list (map #(nth seq %) (range n (count seq)))))))

(defcheck solution-2d39de17
  #(vector (take % %2) (nthrest %2 %)))

(defcheck solution-2d45e772
  #( vector (take %1 %2)   (drop %1 %2)))

(defcheck solution-2defb427
  (fn [i l]
    (loop [l l i i f [] s []]
      (cond
        (empty? l) [f s]
        (> i 0) (recur (rest l) (dec i) (conj f (first l)) s)
        :else (recur (rest l) (dec i) f (conj s (first l)))))))

(defcheck solution-2e7d3908
  (fn [n s]
    [(take n s) (drop n s)]
    ))

(defcheck solution-2f5931d1
  (fn [n xs] (vector (take n xs) (drop n xs))))

(defcheck solution-30202582
  (fn [d s]
    (loop [x s
           c d
           l []
           r []]
      (if (nil? x)
        [l r]
        (if (zero? c)
          (recur (next x) c l (conj r (first x)))
          (recur (next x) (dec c) (conj l (first x)) r))))))

(defcheck solution-31945d9f
  (fn [n coll]
    [(subvec coll 0 n) (subvec coll n (count coll))]))

(defcheck solution-321d1a31
  (fn not-split-at [pos coll] [(take pos coll) (drop pos coll)]))

(defcheck solution-3228777
  (fn [n s]
    [(take n s) (nthrest s n)]
    ))

(defcheck solution-323e8891
  (fn [n l]
    (loop [tmpn n news '() tmpl l]
      (if (= 0 tmpn)
        (reverse (conj (conj '() (reverse news)) tmpl))
        (recur (dec tmpn) (conj news (first tmpl)) (rest tmpl))))))

(defcheck solution-3266adba
  (fn [x y]
    (list (take x y) (drop x y))))

(defcheck solution-3284eefc
  (fn [n s]
    (vector (take n s) (drop n s))))

(defcheck solution-34081bd0
  (fn [n coll] (list
                 (for [i (range n)] (nth coll i))
                 (for [i (range n (count coll))] (nth coll i)))))

(defcheck solution-3433eae5
  (fn [n seq]
    [(take n seq) (drop n seq)]))

(defcheck solution-348a45fc
  #(vec [(take %1 %2) (nthnext %2 %1)]))

(defcheck solution-34a1b737
  #(if (< (count %2) %1) [(vec %2) []]  [(take %1 %2) (drop %1 %2)]))

(defcheck solution-352585e2
  (fn [s col] [(take s col) (drop s col)]))

(defcheck solution-359926d4
  (fn [n l] (list (take n l) (drop n l))))

(defcheck solution-35aac6ae
  ;(map vec ((juxt take drop) %1 %2))

  (juxt take drop))

(defcheck solution-35b69154
  #(vec [(take % %2) (drop % %2)] ))

(defcheck solution-362d05e0
  (fn [i c] (vector (vec (take i c)) (vec (drop i c)))))

(defcheck solution-36884f4f
  #(vector (take % %2) (take-last (- (count %2) %) %2)))

(defcheck solution-37729aa6
  (fn split
    ([x y]
     (split x (rest y) [(first y)]))
    ([x y z]
     (if (= x (count z))
       [z y]
       (recur x (rest y) (conj z (first y)))))))

(defcheck solution-37f31155
  #(list (take % %2) (drop % %2)))

(defcheck solution-380f2b06
  (fn nn [n seqn]
    (list (first (partition-all n seqn))(mapcat identity (rest(partition-all n seqn))))
    ))

(defcheck solution-396b4129
  #(conj [] (into [] (take % %2)) (into [] (drop % %2))))

(defcheck solution-39855d0c
  (fn splitsy [location lst]
    (let [cnt (count lst)]
      (conj '() (take-last (- cnt location) lst) (take location lst) ))))

(defcheck solution-398f9b0e
  (fn split-seq
    [n s]
    (vector
      (take n s)
      (nthrest s n))))

(defcheck solution-39ffa1c4
  (fn [n ls]
    [(take n ls) (drop n ls)]))

(defcheck solution-3a2c78e3
  #(let [ [a & b] (partition-all %1 %2)] [a (reduce concat b)]))

(defcheck solution-3abe2593
  (fn [x coll]
    (loop [n x in-coll coll out-coll []]
      (if (= n 0)
        (conj [out-coll] in-coll)
        (recur (dec n) (subvec in-coll 1) (conj out-coll (first in-coll)))))))

(defcheck solution-3b960421
  (fn [num s]
    (cons (take num s) (list (drop num s)))))

(defcheck solution-3c7fbf4
  (fn [n col] [ (take n col) (drop n col)]))

(defcheck solution-3c9569ef
  (fn
    [n coll]
    (loop [lim n
           curlist coll
           leftvec []]
      (let [[h &  t] curlist
            nextlim (dec lim) ]
        (cond (> lim 0) (recur nextlim t (conj leftvec h))
              :else [leftvec curlist]
              )
        )
      )
    ))

(defcheck solution-3e7c57fd
  (fn [s v] (let [n (count v) p (- n s)] (cons (take s v) (list (take-last p v))))))

(defcheck solution-3e89c10f
  #(list (take %1 %2) (take-last (mod (* -1 %1) (count %2)) %2)))

(defcheck solution-3ebec7d2
  (fn split-seq
    [pos s]
    (loop [[h & t] s
           count pos
           accum []]
      (if (= count 1)
        [(conj accum h) t]
        (recur t (- count 1) (conj accum h))))))

(defcheck solution-3f7bc955
  (fn sa [n s]
    (loop [n n, s s, p1 []]
      (if (= 0 n)
        [(vec p1) (vec s)]
        (recur (dec n) (rest s) (conj p1 (first s)))))))

(defcheck solution-40188a07
  (fn prob49 [n col]
    [(take n col) (drop n col)]
    ))

(defcheck solution-4115919c
  #(identity [(take %1 %2)(drop %1 %2)]))

(defcheck solution-4206abd6
  (fn [n xs]
    [(take n xs) (drop n xs)]))

(defcheck solution-42521136
  (fn [sp i-seq]
    ((fn [left-seq right-seq]
       (if (= (count left-seq) sp)
         [left-seq  right-seq]
         (recur (conj left-seq (first right-seq)) (rest right-seq))
         )
       ) [] i-seq)
    ))

(defcheck solution-42787788
  (fn [pos items] [(take pos items) (drop pos items)]  ))

(defcheck solution-44a97fbd
  (fn [n coll] (conj nil (drop n coll) (take n coll))))

(defcheck solution-4556e419
  (fn split [n s]
    (list (take n s) (nthrest s n))))

(defcheck solution-45b8026b
  (fn [s xs] [(take s xs) (drop s xs)]))

(defcheck solution-4677291d
  (fn split [pos coll] [(vec (take pos coll)) (vec (drop pos coll))]))

(defcheck solution-47a7c97
  (fn [n seq] (list (take n seq) (drop n seq))))

(defcheck solution-47c9806e
  (fn [n s]
    (list (take n s) (drop n s))
    ))

(defcheck solution-492d358c
  (fn [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-496cfbce
  (fn [n col]
    [(take n col) (nthrest col n)]))

(defcheck solution-49c9ff90
  #(conj (vector (take %1 %2)) (vec (drop %1 %2))))

(defcheck solution-4b411726
  (fn [p s] [(take p s) (drop p s)]))

(defcheck solution-4b4b38ce
  #(vector (take %1 %2) (subvec %2 %1)))

(defcheck solution-4b5d903b
  (fn [n seq]
    (map #(map last %) (map last (group-by #(< (first %) n) (map-indexed vector seq))))))

(defcheck solution-4ba00b2c
  (fn  [n lst]
    (let [s1 n, s2 (- (count lst) n)]
      (conj [] (into [] (take s1 lst)) (into [] (take-last s2 lst))))))

(defcheck solution-4bbc243d
  (fn [n aseq]
    [(take n aseq) (drop n aseq)]))

(defcheck solution-4be4547
  (fn[a b] [(take a b) (drop a b)]))

(defcheck solution-4c12be8d
  #(-> [(take % %2) (drop % %2)]))

(defcheck solution-4ceea767
  #(let[ x (partition-all % %2) ]
     (seq [(first x ) (apply concat (rest x ))])))

(defcheck solution-4da52cba
  (fn split- [n coll]
    "49. Write a function which will split a sequence into two parts."
    [(take n coll) (drop n coll)]
    ))

(defcheck solution-4e4667c4
  (fn f [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-4f320faf
  #(map (partial map (fn [[_ v]] v))
     (partition-by (fn [[i _]] (> i %1))
       (map vector (iterate inc 1) %2))))

(defcheck solution-4fe4c604
  (fn [v c] [(take v c) (drop v c)]))

(defcheck solution-4fec73e6
  #(vector
     (vec (take %1 %2))
     (vec (drop %1 %2))))

(defcheck solution-4ffeac51
  ;Thought too much :)
  (fn my-split-at [n coll]
    ((fn [n res accum]
       (cond
         (> n (count res)) res
         (zero? n) [accum res]
         :else
         (recur (dec n) (rest res) (conj accum (first res))))) n coll [])))

(defcheck solution-513f0f15
  (fn[n s][(take n s) (drop n s)]))

(defcheck solution-518a145f
  (fn [n, xs]
    [(take n xs) (drop n xs)]))

(defcheck solution-5197907c
  (fn split-a-sequence [n c]
    [(subvec c 0 n) (subvec c n)]))

(defcheck solution-525f402
  (fn ! [x y] (concat [(take x y)] [(drop x y)])))

(defcheck solution-52979515
  (fn [n coll] (vector (take n coll) (drop n coll))))

(defcheck solution-52e94558
  (fn [n s] (cons (drop-last (- (count s) n) s) (list (drop n s)))))

(defcheck solution-542bb4ab
  (fn split-seq [n xs]
    (let [xp (partition-all n xs)]
      (list (first xp) (apply concat (rest xp))))))

(defcheck solution-54e06685
  (fn [i xs]
    [(take i xs)
     (drop i xs)]))

(defcheck solution-55a50fb2
  (fn [n coll]
    (let [s (partition-all n coll)]
      (list (first s) (apply concat (rest s))))))

(defcheck solution-55fdd616
  (fn [x y] (vector (vec (take x y)) (vec (drop x y)))))

(defcheck solution-560d4212
  (fn [n coll] [(take n coll) (nthnext coll n)]))

(defcheck solution-56176cfa
  (fn [n a] [(take n a) (drop n a)]))

(defcheck solution-56d51783
  (fn [point alist]
    (list (take point alist) (drop point alist))))

(defcheck solution-579f5d7f
  (fn split
    ([n coll] (split n coll []))
    ([n coll aggr]
     (if (= (count aggr) n)
       [aggr coll]
       (split n (rest coll) (conj aggr (first coll)))))))

(defcheck solution-57d81953
  (fn [n xs]
    (conj [] (take n xs) (drop n xs))))

(defcheck solution-583642d9
  (fn [n lista] (list (take n lista) (drop n lista))))

(defcheck solution-5a43c97d
  #(vector (take % %2) (nthnext %2 %)))

(defcheck solution-5a4db8ea
  (fn [x y]
    (conj (conj [] (into [] (take x y)))  (into [] (drop x y))
      )))

(defcheck solution-5aca39a5
  (fn[x y] [(take x y) (drop x y)]))

(defcheck solution-5b31b4be
  #((juxt (partial take %) (partial drop %)) %2))

(defcheck solution-5b6e41ba
  #(vector (take %1 %2) (drop %1 %2)))

(defcheck solution-5b873e46
  #(vector (apply vector (take %1 %2)) (apply vector (drop %1 %2))))

(defcheck solution-5ba964a
  #(concat [(take %1 %2) (take-last (- (count %2) %1) %2)]))

(defcheck solution-5bc07ff4
  (fn [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-5bf0719c
  (fn podijeli [n c]
    (concat (list (take n c))
            (list (reverse (take (- (count c) n) (reverse c))) ))))

(defcheck solution-5d5a5747
  #(vector(take %1 %2) (drop %1 %2)))

(defcheck solution-5db38e7
  (fn [x xs]
    (conj [] (subvec xs 0 x) (subvec xs x))
    ))

(defcheck solution-5ea14829
  (fn my-split-at [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-5ead013e
  (fn [n, s] [(take n s) (drop n s)]))

(defcheck solution-5ec1fab2
  #(vec (list (vec (take %1 %2)) (vec (nthnext %2 %1)))))

(defcheck solution-5f2b13c
  (fn [n s]
    [(vec (take n s)) (vec (drop n s))]))

(defcheck solution-5f3aadf
  (fn [n seq]
    [(take n seq) (drop n seq)]))

(defcheck solution-5f83d9bb
  (fn [n sequence]
    (let [start (take n sequence)
          end (drop n sequence)]
      [start end])))

(defcheck solution-601fb06b
  (fn splt
    ([i ls] (splt i [] ls))
    ([i xs ls]
     (if (= i 0)
       [xs ls]
       (splt (dec i)
         (concat xs [(first ls)])
         (rest ls))))))

(defcheck solution-607fadc6
  (fn [n coll]
    [(take n coll) (nthnext coll n)]))

(defcheck solution-609fc555
  (fn [i x] [(take i x) (drop i x)]))

(defcheck solution-60b9ca13
  (fn[n a-seq]
    [(take n a-seq)
     (take-while identity (drop n a-seq))]))

(defcheck solution-60d52db9
  #(do [(take % %2) (drop % %2)]))

(defcheck solution-61b8a579
  (fn [n coll]  [(take n coll) (drop n coll)]))

(defcheck solution-6221e821
  (fn my-split [num coll]
    (loop [col1 coll,col2 [],n num ]
      (if (= 0 n)
        [col2 col1]
        (recur (rest col1)
          (conj col2  (first col1) )
          (dec n)
          )
        )
      )
    ))

(defcheck solution-6238e9ca
  (fn my-split-at [n col]
    [(take n col) (drop n col)]))

(defcheck solution-628362d6
  #(list (take %1 %2)
     (drop %1 %2)))

(defcheck solution-62e7eb5d
  (fn split-seq
    [n xs]
    [(take n xs) (drop n xs)]))

(defcheck solution-632863bb
  (fn [n coll]
    (vector (take n coll) (drop n coll))))

(defcheck solution-63434de6
  (fn [n xs]
    (loop [cntr n xsp xs acc []]
      (if (empty? xsp)
        acc
        (if (= cntr 1)
          [(conj acc (first xsp)) (rest xsp)]
          (recur (dec cntr) (rest xsp) (conj acc (first xsp))))))))

(defcheck solution-635ec532
  (fn
    [i xs]

    [
     (apply vector
       (take i xs)
       )
     (apply vector
       (drop i xs)
       )
     ]
    ))

(defcheck solution-63d366f3
  #(list (take % %2) (nthnext %2 %)))

(defcheck solution-64050d8e
  #(vector (into [] (take %1 %2)) (into [] (drop %1 %2))))

(defcheck solution-644cc8f6
  (fn chop [dstlst count srclst]
    (if (= 0 count) (list dstlst srclst)
                    (chop (conj dstlst (first srclst)) (dec count) (rest srclst)))) [])

(defcheck solution-64a9a796
  (fn [n l]
    (vector (take n l)
      (drop n l))))

(defcheck solution-651d91e7
  (fn splitter [n col] [(take n col) (take-last (- (count col) n) col)]))

(defcheck solution-65838b49
  (fn [at some-seq]
    (loop [acc []
           rseq some-seq]
      (if (= at (count acc))
        (cons acc [rseq])
        (recur (conj acc (first rseq)) (rest rseq))))))

(defcheck solution-66011d99
  (fn [n l]
    [(take n l) (drop n l)]))

(defcheck solution-666f6882
  (fn [n xs] [(take n xs) (drop n xs)]))

(defcheck solution-66c0100a
  (fn split* [n xs]
    (cons (take n xs) [(drop n xs)])))

(defcheck solution-66f2de11
  (fn [x l] (vector (take x l) (drop x l))))

(defcheck solution-671d1b5b
  (fn[n _seq]
    (reverse(conj '() (take n _seq) (drop n _seq)))))

(defcheck solution-675b8e89
  (fn [n c]
    (conj []  (take n c) (drop n c))))

(defcheck solution-67ac12e
  (fn [n lst] (list (take n lst) (drop n lst))))

(defcheck solution-67cd5317
  (fn [n s]
    [(take n s)
     (drop n s)]
    ))

(defcheck solution-68312e69
  (fn [n l] (cons (take n l) (list (drop n l)))))

(defcheck solution-684f7a1e
  (fn [n s] (vector (take n s) (nthrest s n))))

(defcheck solution-689b543b
  #(cons  (take % %2) (list (drop % %2))))

(defcheck solution-6991e79c
  (fn [n s]
    (vector (take n s) (drop n s))))

(defcheck solution-6a5cc11a
  (fn my_split-at ([n s] (my_split-at n s [])) ([n s a] (if (= n 0) [a (apply vector s)] (my_split-at (dec n) (next s) (conj a (first s)))))))

(defcheck solution-6b19037a
  (fn [n v]
    (loop [a n b [] c v]
      (if (= a 0) [b c] (recur (dec a) (conj b (first c)) (next c))))))

(defcheck solution-6b4791f2
  (fn spl [n s]
    (list (take n s) (drop n s))))

(defcheck solution-6b70eb05
  (fn [n s] [(take n s) (drop n s)]))

(defcheck solution-6b8efb07
  (fn [n coll] [(take n coll) (drop n coll)]))

(defcheck solution-6bdffee2
  (fn[ ct col ] [ (take ct col) (drop ct col) ] ))

(defcheck solution-6bf2d76f
  (fn spl [n s]
    [(take n s) (drop n s)]))

(defcheck solution-6c6b3770
  (fn [x,y]
    (reduce
      #(if (< (count (% 0)) x)
         [(conj (% 0) %2) (% 1)]
         [(% 0) (conj (% 1) %2)]
         )
      [[][]]
      y
      )
    ))

(defcheck solution-6cfe7b42
  (fn [n x]
    [(take n x)
     (reverse (take (- (count x) n) (reverse x)  ) )]

    ))

(defcheck solution-6d257c06
  (fn[at coll]
    [(take at coll)(drop at coll)]))

(defcheck solution-6da058e2
  (fn [n S]
    [(take n S)
     (drop n S)]))

(defcheck solution-6e90000
  #((juxt (partial take %1) (partial drop %1)) %2))

(defcheck solution-6ee71864
  (fn [n s] [(subvec s 0 n) (subvec s n)]))

(defcheck solution-6ee76553
  (fn foo [n, s]
    (loop [i 0, f s, p1 [], p2 []]
      (if (empty? f)
        (vector p1 p2)
        (if (< i n)
          (recur (inc i) (rest f) (conj p1 (first f)) p2)
          (recur (inc i) (rest f) p1 (conj p2 (first f))))))))

(defcheck solution-6fbbf4c2
  (fn split-at-pos
    [pos coll]
    (letfn [(take-frst [] (take pos coll))
            (take-lst [] (drop pos coll))]
      [(take-frst) (take-lst)])))

(defcheck solution-6fcbbc61
  (fn [x y] (list (take x y) (drop x y))))

(defcheck solution-701934b0
  (fn [n coll] (loop [x n c coll a []] (if (zero? x) [a c] (recur (dec x) (rest c) (conj a (first c)))))))

(defcheck solution-70ded10
  (fn [n coll]
    (reduce (fn [[fir sec] [i item]]
              (if (>= i n)
                [fir (conj sec item)]
                [(conj fir item) sec]))
      [[] []]
      (map vector (range) coll))))

(defcheck solution-710e2f9d
  (fn [n coll] (let [s (partition-all n coll)]
                 (list (first s) (apply concat (rest s))))))

(defcheck solution-711c8fc9
  (fn [num coll]
    (loop [n num prev [] c coll]
      (if (zero? n)
        (list prev c)
        (recur (dec n) (conj prev (first c)) (rest c))
        )
      )
    ))

(defcheck solution-713ce409
  (fn [splt lst]
    (list (keep-indexed #(if(< % splt) %2) lst) (keep-indexed #(if(>= % splt) %2) lst) )
    ))

(defcheck solution-722e0083
  (fn split-two
    [pivot-index collection]
    (vector (subvec collection 0 pivot-index)
      (subvec collection pivot-index (count collection)))))

(defcheck solution-728c073
  (fn [x coll] [(take x coll) (drop x coll)]))

(defcheck solution-7365fed9
  (fn [n sq]
    (cons (take n sq) (cons (drop n sq) nil))))

(defcheck solution-73a0cbf9
  (fn [n s]
    (list (take n s) (drop n s))))

(defcheck solution-75733d80
  (fn foo [n l]
    (loop [f '() s '() [fl & args :as my-l] l f-count 0]
      (if (empty? my-l)
        (list f s)
        (if (= f-count n)
          (recur f (concat s (list fl)) args f-count)
          (recur (concat f (list fl)) s args (inc f-count)))))))

(defcheck solution-7588ce42
  (fn cs [x, coll]
    (let [partioned (partition-all x coll)] (conj [] (first partioned) (apply concat (rest partioned))))))

(defcheck solution-76213a0e
  (fn [n coll]
    (list (take n coll)
      (drop n coll))))

(defcheck solution-7739cf7b
  #(vector (vec (take %1 %2)) (vec (drop %1 %2))))

(defcheck solution-79518bf
  (fn[y, x] (conj [] (take y x) (drop y x))))

(defcheck solution-7953642e
  #(letfn [(worker [l n lhs]
             (if (zero? n)
               [lhs (vec l)]
               (recur (rest l) (dec n) (conj lhs (first l)))))]
     (worker %2 %1 [])))

(defcheck solution-79d5601c
  (fn [l s]
    (vector (take l s) (drop l s))))

(defcheck solution-7a35956a
  (fn [index col]
    (let [
          init  (range 0 index)
          end (range index (count col))
          the-fn (fn [the-col-part] (into []  (map #(nth col %)  the-col-part)))
          ]
      [ (the-fn init) (the-fn end)] )

    ))

(defcheck solution-7ae853fc
  (fn [i s]
    [(take i s) (drop i s)]))

(defcheck solution-7be42934
  (fn [n coll]
    (vector (take n coll) (drop n coll))
    ))

(defcheck solution-7c11364b
  #(vec [(take %1 %2) (drop %1 %2)]))

(defcheck solution-7c115231
  (fn [n x]
    (cons (take n x) (list (drop n x)))
    ))

(defcheck solution-7ce237d8
  (fn m [n l]
    (vector (take n l) (drop n l))))

(defcheck solution-7dc31c59
  (fn split-at-1 [num coll]
    [(take num coll) (drop num coll)]))

(defcheck solution-7dd7e183
  #(cons (take % %2)
     [(drop % %2)]))

(defcheck solution-7e028363
  #(letfn [(splt [index new orig]
             (if (zero? index)
               [(into [] new) (into [] orig)]
               (recur (dec index) (conj new (first orig)) (rest orig))))]
     (splt %1 [] %2)))

(defcheck solution-7ea64da
  (fn [n c] ((juxt #(take n %) #(drop n %)) c)))

(defcheck solution-7ef13917
  (fn [i s]
    (loop [i i s s n []]
      (if (= i 0)
        [n s]
        (recur (dec i) (rest s) (conj n (first s)))))))

(defcheck solution-7fc43331
  #(vector (take % %2) (drop % %2)))

(defcheck solution-80815509
  #(conj (into [] [(take % %2)]) (drop % %2)))

(defcheck solution-8197951a
  (fn [n l]
    (cons (take n l)
      [(take-last (- (count l) n) l)]
      )
    ))

(defcheck solution-824000b0
  (fn [n x]
    (loop [n n
           x x
           c []]
      (if (zero? n)
        (conj [] c x)
        (recur (dec n) (rest x) (conj c (first x)))))))

(defcheck solution-83122afc
  (fn [n lst] (vector (take n lst) (drop n lst))))

(defcheck solution-83c56a7a
  (fn [c s]
    [
     (take c s)
     (drop c s)]))

(defcheck solution-83c7b689
  #(list (take %1 %2)  (nthrest %2 %1)))

(defcheck solution-83efd5a9
  (fn [i col]
    [(take i col) (drop i col)]
    ))

(defcheck solution-840b1232
  (fn split [n coll]
    [(vec (take n coll))
     (vec (reverse (take (- (count coll) n) (reverse coll))))]))

(defcheck solution-843c663e
  (fn _split-at [n items]
    (list (take n items) (drop n items))))

(defcheck solution-847c3c0d
  #(concat [(take %1 %2)] [(drop %1 %2)]))

(defcheck solution-847f1ff9
  (fn [n xs] (list (take n xs) (drop n xs))))

(defcheck solution-84a76e74
  #(conj [] (take % %2) (drop % %2)))

(defcheck solution-84db33
  (fn my-split-at [n col]
    (loop [head '()
           rst col
           count n]
      (if (zero? count)
        (list (reverse head) rst)
        (recur (conj head (first rst)) (rest rst) (dec count))))))

(defcheck solution-85301e8e
  (fn split-seq [n a-seq]
    (cons (take n a-seq)
      (list (drop n a-seq)))
    ))

(defcheck solution-858634ef
  (fn [n v] [(take n v) (drop n v)]))

(defcheck solution-861c88a2
  (fn [x l] [(take x l) (drop x l)]))

(defcheck solution-8636fff
  (fn [n l] (vector (take n l) (drop n l))))

(defcheck solution-86bb3fe6
  #(cons (take % %2) (list (drop % %2))))

(defcheck solution-87ab2fb
  (fn [n s]
    (map
      #(map last %)
      (split-with
        (fn [[idx]] (> n idx))
        (map-indexed vector s)))
    ))

(defcheck solution-880fdd5f
  (fn [i l] (let [a (take i l) b (drop i l)] [a b])))

(defcheck solution-888942d6
  (fn [n sq]
    (list (take n sq) (drop n sq))))

(defcheck solution-89f163fd
  (fn [x y]
    (list
      (clojure.core/take x y)
      (clojure.core/nthnext y x))))

(defcheck solution-8a78faaa
  (fn [n s]
    [(take n s) (drop n s)]))

(defcheck solution-8abb25cd
  (fn [i col] (list (take i col) (take-last (- (count col) i) col))))

(defcheck solution-8abf0dd4
  #((juxt take drop) %1 %2))

(defcheck solution-8bcfaa84
  #(list (apply take %&) (apply drop %&)))

(defcheck solution-8cc8f0c5
  #(let [xs (take %1 %2), ys (drop %1 %2)] (vector xs ys)))

(defcheck solution-8d04eeb2
  (fn
    [cnt li]
    (list
      (take cnt li)
      (drop cnt li))))

(defcheck solution-8d4d4754
  #(vector (subvec %2 0 %1) (drop %1 %2)))

(defcheck solution-8dbad27b
  (fn [n l] [(subvec l 0 n) (subvec l n)]))

(defcheck solution-8f1968d5
  #(list (subvec %2 0 %1)
     (subvec %2 %1 (count %2))))

(defcheck solution-8f26d6cb
  (fn [x coll]
    (loop [o []
           c coll]
      (if (= (count o) x)
        (let [v []]
          (conj v o c))
        (recur (conj o (first c))
          (drop 1 c))))))

(defcheck solution-8fb64c6b
  (fn [n l]
    (loop [res [(first l)], i 1, rem (rest l)]
      (if (= i n)
        [res rem]
        (recur (conj res (first rem)) (inc i) (rest rem))))))

(defcheck solution-903664ab
  (fn [pivot s]
    (let [v (vec s)]
      [(subvec v 0 pivot) (subvec v pivot)])))

(defcheck solution-90a7f6ad
  #(-> [(take %1 %2) (drop %1 %2)]))

(defcheck solution-910f9c28
  (fn [n coll] (vector (keep-indexed #(if (> n %1) %2) coll) (keep-indexed #(if (<= n %1) %2) coll))))

(defcheck solution-91425804
  (fn split [x y]
    (list (take x y) (drop x y))))

(defcheck solution-937dfb42
  (fn [pos col]
    [(take pos col)
     (drop pos col)]))

(defcheck solution-94fbfb51
  (fn [n l]
    (conj
      [(take n l)]
      (drop n l))))

(defcheck solution-95134cbd
  (fn [n x]
    (list
      (apply vector (take n x))
      (apply vector (nthrest x n)))))

(defcheck solution-9572b814
  (fn [k coll] [(take k coll) (drop k coll)]))

(defcheck solution-95afc3af
  (fn [c a]
    (vec [(vec (take c a)) (vec (drop c a))])))

(defcheck solution-964792ef
  (fn [n s]
    [(take n s) (drop n s)]))

(defcheck solution-9729fdc6
  (fn [i coll] [(take i coll) (drop i coll)]))

(defcheck solution-98674140
  (fn [n l] [(take n l)(drop n l)]))

(defcheck solution-98fff15
  (fn [i sq]
    [(take i sq) (drop i sq)]))

(defcheck solution-993f44c
  #(into (into [] (vector (take %1 %2))) (vector (drop %1 %2))))

(defcheck solution-994f8afd
  (fn [n coll] [(subvec coll 0 n) (subvec coll n)]))

(defcheck solution-9a488f33
  #(list(take %1 %2)(drop %1 %2)))

(defcheck solution-9a9bcced
  #(vector
     (take %1 %2)
     (drop %1 %2)))

(defcheck solution-9aba777a
  (fn [n x]
    [(take n x) (drop n x)]))

(defcheck solution-9adcb658
  (fn [n seq]
    [(take n seq) (drop n seq)]))

(defcheck solution-9ae4a22f
  (fn split
    [n coll] {:pre [(integer? n)]}
    [(take n coll) (drop n coll)]))

(defcheck solution-9b8471bd
  (fn [n coll]
    (list (take n coll) (nthnext coll n))))

(defcheck solution-9bbd7dad
  (fn split [n v]
    [(subvec v 0 n)(subvec v n)]
    ))

(defcheck solution-9c5182c
  #(do [(take %1 %2) (drop %1 %2)]))

(defcheck solution-9e32b171
  (fn [at numbers]
    (let [left (take at numbers)
          right (drop at numbers)]
      (list left right))))

(defcheck solution-9f5afdda
  #(list (take %1 %2) (drop %1 %2) ))

(defcheck solution-9f76b25b
  #(let [cnt %1
         head (take cnt %2)
         tail (drop cnt %2)]
     [head tail]
     ))

(defcheck solution-9fa64f35
  (fn mySplit-at [n coll] (vector (take n coll) (drop n coll))))

(defcheck solution-a0a00f8e
  (fn [x y]
    [((fn tmp [a b]
        (if (= a 1)
          [(first b)]
          (cons (first b) (tmp (dec a) (rest b)) )
          ))
      x y)
     (drop x y)]))

(defcheck solution-a38d9e1a
  (fn[n s][(subvec s 0 n)(subvec s n)]))

(defcheck solution-a472a4f8
  (fn [n s] (vector (into [] (take n s)) (into [] (nthrest s n))) ))

(defcheck solution-a4af151c
  (fn myspt [n s]
    (let [f1 (fn [c l result]
               (if (= c 0)
                 result
                 (recur (dec c)
                   (rest l)
                   (concat result
                           (list (first l))))))
          ss (f1 n s '())]
      (vec (cons (vec ss) (vector (vec (nthnext s n))) )))))

(defcheck solution-a5021b02
  (fn [n xs]
    (letfn
     [(go [ys i rs]
        (cond
          (empty? ys) rs
          (< 0 i) (go (rest ys) (dec i) (conj rs (first ys)))
          :else [rs ys]))]
      (go xs n []))))

(defcheck solution-a547d766
  (fn [n c]
    (concat (conj [] (take n c)) (conj [] (nthnext c n)))))

(defcheck solution-a566e404
  (fn [n s][(take n s) (drop n s)]))

(defcheck solution-a60d214c
  (fn [n xs]
    (loop [ys xs
           zs []
           m n]
      (if (seq ys)
        (if (> m 0)
          (recur (rest ys) (conj zs (first ys)) (dec m))
          [zs ys])
        [zs ys]))))

(defcheck solution-a659b6fe
  (fn[n seq] (conj [] (vec (take n seq)) (vec (nthrest seq n)))))

(defcheck solution-a758278a
  (fn [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-a7649ca2
  (fn [n lst] [(take n lst) (drop n lst)]))

(defcheck solution-a78039d3
  (fn [skip coll] [(take skip coll) (drop skip coll) ]))

(defcheck solution-a788b1b0
  #(map (fn [i](i %1 %2)) [take drop]))

(defcheck solution-a8a39f01
  (fn splitseq [n x]
    (vector (take n x) (drop n x))))

(defcheck solution-aa4d830c
  (fn split [n s]
    (loop [part [] full s i n]
      (if (= 0 i)
        (list part full)
        (recur (conj part (first full)) (rest full) (- i 1))))))

(defcheck solution-aa5c2486
  (fn [n S]
    [(take n S)(drop n S)]
    ))

(defcheck solution-ab3bf24f
  (fn[pos, col]
    [(subvec col 0 pos)(subvec col pos)]))

(defcheck solution-ac47099
  (fn my-split [n x]
    (list (take n x) (drop n x))))

(defcheck solution-ad53bcfe
  #(concat (list (take %1 %2)) (list (rest (drop (dec %1) %2)))))

(defcheck solution-ad67a3bc
  (fn [b coll] (let [a (map-indexed vector coll)]
                 (map (fn [d] (map #(last %) d))
                   (partition-by #(< (first %) b) a)))))

(defcheck solution-ae1b9916
  (fn [n xs]
    (list (take n xs) (drop n xs))))

(defcheck solution-aeb3f6fc
  #(conj (conj [] (take %1 %2)) (nthrest %2 %1)))

(defcheck solution-af3adbda
  (fn [pos coll]
    [(take pos coll) (drop pos coll)]
    ))

(defcheck solution-af918833
  (fn my-split-helper [n l]
    (loop [l l cnt 1 a []]
      (if (empty? l)
        (reverse a)
        (if (< cnt n)
          (recur (rest l) (inc cnt) (cons (first l) a))
          (cons (reverse (cons (first l) a)) (list (rest l))))))))

(defcheck solution-b0535c7b
  #(conj [(take %1 %2)] (drop %1 %2)))

(defcheck solution-b057c00d
  (fn [n xs] [(vec (take n xs)) (vec (drop n xs))]))

(defcheck solution-b10e2cb2
  (fn [n coll]
    [(take n coll)
     (drop n coll)]))

(defcheck solution-b1d15e42
  (fn [x y] (vector (take x y) (drop x y))))

(defcheck solution-b29630bf
  #(list (take %1 %2) (drop %1 %2)))

(defcheck solution-b2a82966
  (fn [split input]
    (loop [cnt 0 remaining input ans [[][]]]
      (if (empty? remaining)
        ans
        (if (< cnt split)
          (recur (inc cnt) (rest remaining) (vector (conj (get ans 0) (first remaining)) (get ans 1)))
          (recur (inc cnt) (rest remaining) (vector (get ans 0) (conj (get ans 1) (first remaining)))))))))

(defcheck solution-b2b78849
  (fn [n s]
    [(vec (take n s)) (vec (drop n s))]))

(defcheck solution-b32c07d
  (fn [n xs]
    (let [es (empty xs)]
      (conj
        es
        (into es (take n xs))
        (into es (drop n xs)))
      )))

(defcheck solution-b379d013
  (fn [n coll]
    [(take n coll) (drop n coll)]
    ))

(defcheck solution-b3ec7da5
  (fn [number items]
    (loop [items items current 0 main [] less []]
      (if (empty? items)
        [main less]
        (recur (rest items) (inc current)
          (if (< current number)
            (conj main (first items))
            main)
          (if (>= current number)
            (conj less (first items))
            less))))))

(defcheck solution-b45508aa
  (fn [n v] [(vec (take n v)) (vec (drop n v))]))

(defcheck solution-b47565b5
  (fn [idx coll] [(take idx coll) (drop idx coll)]))

(defcheck solution-b4c5b073
  (fn split-sequence [x seqs]
    (loop[car []
          x x
          others seqs]
      (if(zero? x)
        (vector car others)
        (recur
          (conj car (first others))
          (dec x)
          (rest others))))))

(defcheck solution-b54d071d
  #(conj [] (take %1 %2) (drop %1 %2)))

(defcheck solution-b5676b7b
  (fn [n lst]
    [(take n lst) (drop n lst)]
    ))

(defcheck solution-b6244a4c
  (fn [n sq] (list (take n sq) (drop n sq))))

(defcheck solution-b7dcdf94
  (fn [pos col]
    (vector (take pos col) (drop pos col) )
    ))

(defcheck solution-b8114491
  (fn [num coll]
    (conj [] (take num coll) (drop num coll))))

(defcheck solution-b87702ac
  (juxt take drop))

(defcheck solution-b8d1b71f
  (fn [i l]
    [(vec (take i l)) (vec (drop i l))]))

(defcheck solution-b93773b7
  (let [splitter
        (fn [leftover accum current split]
          (if (= current split)
            [accum leftover]
            (recur (rest leftover) (conj accum (first leftover)) (inc current) split)
            )
          )
        ]
    (fn [n coll] (splitter coll [] 0 n))
    ))

(defcheck solution-b9be9148
  (fn [n s]
    (or [(take n s) (drop n s)] (juxt take drop))))

(defcheck solution-b9c30bfa
  #(vector (take %1 %2) (nthrest %2 %1)))

(defcheck solution-b9d4fc1c
  (fn [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-b9f3b469
  (fn [n,l] [(take n l) (drop n l)]))

(defcheck solution-ba979908
  (fn  [n s]
    (list (take n s) (drop n s))))

(defcheck solution-bad9039a
  (fn [i xs] [(take i xs) (drop i xs)]))

(defcheck solution-bb1ea1b4
  (fn [n x] [(take n x) (drop n x)]))

(defcheck solution-bc56c921
  (fn f [n s] (list (take n s) (drop n s))))

(defcheck solution-bda7fa98
  (fn [idx, vct]
    [(subvec vct 0 idx) (subvec vct idx)]))

(defcheck solution-be807470
  #(loop [i 0 n1 [] n2 []]
     (if (< i (count %2))
       (if (< i %1)
         (recur (inc i) (conj n1 (%2 i)) n2)
         (recur (inc i) n1 (conj n2 (%2 i))))
       [n1 n2]
       )))

(defcheck solution-bfd154cb
  (fn [x v] [(take x v) (take-last (- (count v) x) v)]))

(defcheck solution-c013cb98
  (fn [num seqs]
    (list (take num seqs) (drop num seqs))))

(defcheck solution-c039303d
  #((juxt take drop) % %2))

(defcheck solution-c081ac9
  (fn [n l] (conj '() (drop n l) (take n l))))

(defcheck solution-c0901913
  (fn [n s]
    (loop [cur 0
           left []
           right s]
      (if (= n cur)
        (vector left right)
        (recur (inc cur) (conj left (first right)) (rest right))))))

(defcheck solution-c1018e9c
  (fn splt [n s] [(take n s) (drop n s)]))

(defcheck solution-c14c4652
  (fn [x y] [(take x y) (drop x y)]))

(defcheck solution-c15dbb9a
  #(list (take %1 %2) (take-last (- (count %2) %1) %2)))

(defcheck solution-c2e07620
  (fn split [n lst]
    (loop [m 0, A [], B lst]
      (if (< m n)
        (recur (+ m 1) (conj A (first B)) (rest B))
        [A, B]))))

(defcheck solution-c2fb712a
  (fn [c l] [(take c l) (drop c l)]))

(defcheck solution-c45e0f16
  (fn[n sq]
    [(take n sq) (drop n sq)]))

(defcheck solution-c465cebf
  (fn [cnt col]
    (letfn [(former [n] (take cnt col))
            (latter [n] (nthrest col cnt))]
      (vector (former col) (latter col)))))

(defcheck solution-c480b3bb
  (fn [n coll]
    (loop [n n
           coll coll
           r '()]
      (if (= n 0)
        `(~(reverse r) ~coll)
        (recur (- n 1) (rest coll) (conj r (first coll)))))))

(defcheck solution-c599adda
  (fn [i nums] [(vec (take i nums)) (vec (drop i nums))]))

(defcheck solution-c5a0a11c
  (fn [i v] [(take i v) (drop i v)]))

(defcheck solution-c5a16f01
  (fn [n coll]
    [(take n coll) (nthrest coll n)]))

(defcheck solution-c74f05d3
  (fn [x col]
    [(take x col) (drop x col)]))

(defcheck solution-c80b1d88
  (fn splitat [n coll]
    [(take n coll)
     (drop n coll)]))

(defcheck solution-c8af52b7
  (fn [num, sq] [(take num sq) (drop num sq)] ))

(defcheck solution-c8fa59d7
  (fn [n l] [(take n l) (drop n l)]))

(defcheck solution-c97fc2c
  (fn split [i as]
    ((fn spl [n xs ys] (if (or (<= n 0) (empty? ys)) [xs ys] (spl (- n 1) (conj xs (first ys)) (rest ys))))
     i [] as
     )
    ))

(defcheck solution-c9cc0700
  (fn [n s]
    [ (take n s), (drop n s) ]))

(defcheck solution-c9d061f0
  (fn
    [split-size aseq]
    (let [acc (vector (into [] (take split-size aseq)))]
      (into acc (vector (vec (drop split-size aseq)))))))

(defcheck solution-c9f9936d
  #(identity [(take % %2) (drop % %2)]))

(defcheck solution-ca0f20fc
  #(let [xs (partition-all %1 %2)]
     (list (first xs) (apply concat (rest xs)))))

(defcheck solution-cb1c16c2
  #(let [c1 (take %1 %2), c2 (drop %1 %2)] [c1 c2]))

(defcheck solution-cbfa9483
  (fn [n v] [(subvec v 0 n) (subvec v n)]))

(defcheck solution-ccf75289
  (fn [n s]
    [(take n s) (nthrest s n)]))

(defcheck solution-ccfba118
  (fn [n lst]
    [(take n lst) (drop n lst)]))

(defcheck solution-cd48b5ce
  (fn splt
    [i l]
    [(take i l)
     (take-last (- (count l) i) l)]))

(defcheck solution-cd74d3c8
  #(vector (take %1 %2) (take-last (- (count %2) %1) %2)))

(defcheck solution-cea50ccc
  (fn split-seq [x y]
    (list (take x y)
      (drop x y))))

(defcheck solution-ceb2f0a2
  (fn sp [n s]
    (let [len (count s)]
      [(take n s) (take (- len n) (drop n s))])))

(defcheck solution-cf59840b
  (fn [n xs] (map #(map last %)
               (partition-by (fn [v] (<= n (first v)))
                 (map-indexed vector xs)))))

(defcheck solution-cfbb150f
  #(identity [(take %1 %2) (nthnext %2 %1)]))

(defcheck solution-d06d9f92
  (fn [n xs]
    [(take n xs) (drop n xs)]))

(defcheck solution-d09cdcd5
  (fn [x y] (vector (take x y) (take-last (- (count y) x) y) ) ))

(defcheck solution-d0a280d1
  (fn[a b][ (vec (take a b)) (vec (drop a b))]))

(defcheck solution-d0ced3c8
  (fn [n xs](list (take n xs) (drop n xs))))

(defcheck solution-d0f13668
  #(vec (list (vec (take %1 %2)) (vec (nthrest %2 %1)))))

(defcheck solution-d3b417a9
  #(->[(take % %2) (drop % %2)]))

(defcheck solution-d3ffd364
  (fn [i s]
    [(subvec s 0 i) (subvec s i)]))

(defcheck solution-d405d124
  (fn [at s]
    [(take at s)
     (drop at s)]))

(defcheck solution-d43400ea
  #(list (for [i (range %1)]
           (nth %2 i))
     (for [i (range %1 (count %2))]
       (nth %2 i))))

(defcheck solution-d43b263b
  #(conj [] (take % %2)(drop % %2)))

(defcheck solution-d47ef11b
  (fn
    [n coll]
    ((juxt (comp vec (partial take n))
       (comp vec (partial drop n)))
     coll)))

(defcheck solution-d4b8abe1
  (fn [n xs]
    [(take n xs)(drop n xs)]))

(defcheck solution-d4d9514d
  #(let [n %1] (loop [cl %2 i 0 r1 [] r2 []]
                 (cond
                   (empty? cl) [r1 r2]
                   (> n i) (recur (rest cl) (inc i) (conj r1 (first cl)) r2 )
                   :else (recur (rest cl) (inc i) r1 (conj r2 (first cl)) )
                   ))))

(defcheck solution-d52e1848
  (fn [s-at coll] [(take s-at coll) (drop s-at coll)]))

(defcheck solution-d56bb86b
  (fn [n seq1]
    (let [head (first (partition-all n seq1))
          tail (list (reduce concat (rest (partition-all n seq1))))]
      (vec (conj tail head)))))

(defcheck solution-d678fb6b
  (fn [n s]
    (loop [s_ s i n ret '()]
      (cond
        (not s_) (list s '())
        (= i 0) (list (into '() ret) s_)
        :else (recur (next s_) (dec i) (conj ret (first s_)))))))

(defcheck solution-d6e81cb3
  (fn [n coll] [ (take n coll) (drop n coll) ]  ))

(defcheck solution-d7e1463a
  (fn [x y] (concat (list (take x y)) (list (drop x y)))))

(defcheck solution-d924a986
  (fn [n seq] [(take n seq) (drop n seq)]))

(defcheck solution-da30802f
  (fn [n x] (conj [] (vec (take n x)) (vec (drop n x)))))

(defcheck solution-da35c7cb
  #(let [a (partition-all %1 %2)]
     (concat (take 1 a) (list (apply concat (doall (drop 1 a)))))))

(defcheck solution-da6b0c70
  (fn [c l] [
             (vec(take c l))
             (vec(drop c l))
             ]))

(defcheck solution-db764700
  (fn [n coll]
    (list (take n coll) (drop n coll))))

(defcheck solution-dc4c052c
  (fn [idx coll] ((juxt take drop) idx coll)))

(defcheck solution-dc8f0796
  (fn [at col]
    (conj [] (take at col) (take-last (- (count col) at) col))))

(defcheck solution-dd0d71b
  (fn [n coll] (list (take n coll) (nthrest coll n))))

(defcheck solution-dd1cd41
  (fn [n, s]
    [(take n s) (drop n s)]))

(defcheck solution-dd45bf84
  (fn [n xv] [(subvec xv 0 n) (subvec xv n)]))

(defcheck solution-dd8ebbf0
  #(identity [(take %1 %2) (drop %1 %2)]))

(defcheck solution-decbc5d9
  (fn [n v] [(take n v) (take-last (- (count v) n) v)]))

(defcheck solution-df59b9ff
  (fn my-split
    [n s]
    [(vec (take n s)) (vec (drop n s))]))

(defcheck solution-df6791f0
  (fn my-split-at [n coll]
    (vector (take n coll) (drop n coll))))

(defcheck solution-df6af850
  (fn f [n s] [(take n s) (drop n s)]))

(defcheck solution-e19773
  (fn [n coll]
    (conj [(take n coll)] (nthnext coll n))))

(defcheck solution-e229b47d
  (fn [n col] (vector (take n col) (take-last (- (count col) n) col))))

(defcheck solution-e3e7e7d2
  (fn [n s]
    (loop [x n s1 s s2 []]
      (if (zero? x)
        [s2 (into [] s1)]
        (recur (dec x) (rest s1) (conj s2 (first s1)))))))

(defcheck solution-e463adb1
  (fn mysplit
    [cnt myseq]
    (list (take cnt myseq) (drop cnt myseq))))

(defcheck solution-e465ac5b
  #(list(take %1 %2) (drop %1 %2)))

(defcheck solution-e4d8110d
  (fn [n xs] (first (reduce (fn [[[ls rs] i] v] (if (> i 0) [[(conj ls v) rs] (- i 1)] [[ls (conj rs v)] i])) [[[] []] n] xs))))

(defcheck solution-e53d712e
  #(vector (apply vector (take %1 %2)) (apply vector (nthrest %2 %1))))

(defcheck solution-e65f29a1
  #(list (take % %2) (take-last (- (count %2) %) %2)))

(defcheck solution-e66d2734
  (fn [a b] [(take a b) (drop a b)]))

(defcheck solution-e6ccc594
  (fn f [n vs]
    (loop [n n, hd [], tl (seq vs)]
      (if (zero? n)
        [hd tl]
        (recur (dec n)
          (conj hd (first tl))
          (rest tl))))))

(defcheck solution-e78d662e
  #(vector (take %1 %2)(drop %1 %2)))

(defcheck solution-e7ab11c7
  (fn mysplit [ctr sq]
    [(take ctr sq) (drop ctr sq)]))

(defcheck solution-e806d25
  (fn [n S] [(take n S) (drop n S)]))

(defcheck solution-e812f027
  (fn [c xs]
    [(take c xs) (drop c xs)]))

(defcheck solution-e96dbe41
  (fn [n coll] [(take n coll)(drop n coll)]))

(defcheck solution-ea3f25b8
  (fn [x s] (vector (into [] (take x s)) (into [] (drop x s)) )))

(defcheck solution-ea57daff
  (letfn [(R [n l r]
            (cond (zero? n) [l r]
                  :else (R (dec n)
                          (conj l (first r))
                          (rest r))))
          (S [n l]
            (R n [] l))]
    S))

(defcheck solution-eb2f9b12
  (fn f [i c] ;; dirty trick
    (let [ps (partition i i [] c)]
      [(first ps) (apply concat (rest ps))])))

(defcheck solution-ebc730ad
  (fn [n lst]
    (vector (take n lst) (drop n lst))))

(defcheck solution-ec0cde0f
  (fn [i lst]
    [(take i lst)
     (take-last (- (count lst) i) lst)]))

(defcheck solution-ec456bd5
  (fn split[n, coll]
    [(take n coll) (nthrest coll n)]))

(defcheck solution-ecac9c40
  #(list
     (take % %2)
     (drop % %2)

     ))

(defcheck solution-ed7158e7
  (fn [n c]
    (list (take n c) (take-last (- (count c) n) c))
    ))

(defcheck solution-ee098232
  (fn [n ls] [(take n ls) (drop n ls)]))

(defcheck solution-ee46464b
  (fn [loc list]
    (loop [acc []
           n loc
           left list]
      (if (zero? n)
        [acc left]
        (recur (conj acc (first left))
          (dec n)
          (rest left))))))

(defcheck solution-ee57d90a
  (fn [n coll]
    (vector (take n coll) (drop n coll))))

(defcheck solution-ef1fd59b
  (fn [n s] (list (take n s) (drop n s))))

(defcheck solution-f1612515
  (fn [pos coll]
    [(vec (take pos coll)) (vec (drop pos coll))]))

(defcheck solution-f173f544
  (fn [pos coll]
    [(take pos coll) (drop pos coll)]))

(defcheck solution-f1818aae
  #(merge (vector (into [] (take %1 %2))) (into [] (drop %1 %2))))

(defcheck solution-f1dbd340
  (fn __ [i l]
    [(take i l) (drop i l)]))

(defcheck solution-f2501721
  #(vector (subvec %2 0 %1) (subvec %2 %1 (count %2))))

(defcheck solution-f2fb612c
  (fn [idx, coll]
    [(take idx coll) (drop idx coll)]))

(defcheck solution-f3fb2115
  (partial
    (fn [acc n l]
      (if (= n 0) (list (reverse acc) l)
                  (recur (conj acc (first l)) (dec n) (rest l)))) '()))

(defcheck solution-f406711f
  (fn [size pieces]
    [(vec (take size pieces)) (vec (drop size pieces))]))

(defcheck solution-f53070e7
  (fn [sp seq1]
    (loop [result1 [] result2 [] elements seq1 index 0]
      (if (empty? elements)
        (conj (conj [] result1) result2)
        (if (< index sp)
          (recur (conj result1 (first elements)) result2 (rest elements) (inc index))
          (recur result1 (conj result2 (first elements)) (rest elements) (inc index))
          )
        )
      )
    ))

(defcheck solution-f62ff09b
  (fn [n xs]
    [(take n xs) (drop n xs)]))

(defcheck solution-f6e93c51
  #(vector (vec (take % %2)) (vec (drop % %2))))

(defcheck solution-f72f42e3
  (fn [n c] [(drop-last (- (count c) n) c) (nthrest c n)]))

(defcheck solution-f81d70d3
  (fn [n a]
    (let [len (count a)]
      (concat
       [(reverse (drop (- len n) (reverse a)))
        (drop n a)]))))

(defcheck solution-f8af5351
  (fn [x y]
    [(apply vector (take x y)) (apply vector (drop x y))]
    ))

(defcheck solution-f8b8aa7e
  (fn [n xs] (cons (take n xs) [(drop n xs)])))

(defcheck solution-fb35d5d5
  (fn my-split-at
    ([pos lis] (my-split-at pos lis []))
    ([pos lis begin]
     (if (= lis [])
       (vector begin)
       (if (= pos 0)
         (vector begin (apply vector lis))
         (my-split-at (dec pos) (rest lis) (conj begin (first lis))))))))

(defcheck solution-fcfa108b
  (fn
    [n coll]
    [(take n coll) (drop n coll)]))

(defcheck solution-fe329ae3
  (fn spli[n s](cons (take n s) [(drop n s)])))

(defcheck solution-fe6731a8
  (fn [i col]
    [(take i col) (drop i col)]))

(defcheck solution-fefb0129
  (fn [x s]
    [(take x s) (drop x s)]))

(defcheck solution-ff19ca0c
  (fn [i s] [(take i s) (drop i s)]))

(defcheck solution-ff20a16e
  (fn [n coll]
    (let [p (partition n n [] coll)]
      (conj (vector (first p)) (apply concat (rest p))))))

(defcheck solution-ff423de4
  (fn part-at [index coll] (list (take index coll) (drop index coll))))

(defcheck solution-ffc61845
  (fn sp [n c]
    [(take n c) (drop n c)]))

(defcheck solution-fff612fc
  #(list (take %1 %2) (nthrest %2 %1)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-49))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
