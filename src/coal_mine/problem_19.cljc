(ns coal-mine.problem-19
  (:require [coal-mine.checks :refer [defcheck-19] :rename {defcheck-19 defcheck}]
            [clojure.test]))

(defcheck solution-10dc0dda
  (partial reduce (fn [x y] y) nil))

(defcheck solution-11026951
  (fn [xs] (if (= (count xs) 1) (first xs) (recur (rest xs)))))

(defcheck solution-118b2ecc
  (comp first reverse))

(defcheck solution-12886e61
  (fn [coll]
    (reduce (fn [x y] y) coll)))

(defcheck solution-12e875f0
  (fn my-last [l]
    (if (= 1 (count l))
      (first l)
      (my-last (rest l)))))

(defcheck solution-1367f873
  #(nth % (- (count %) 1)))

(defcheck solution-151187f0
  (fn [xs] (if (= (rest xs) ()) (first xs) (recur (rest xs)))))

(defcheck solution-151ca421
  #(get (vec %) (- (count %) 1)))

(defcheck solution-1616fd49
  (fn [xs] (if (empty? (rest xs)) (first xs) (recur (rest xs)))))

(defcheck solution-165a7b6e
  #(->> % reverse first))

(defcheck solution-1699b668
  (fn [coll]
    (if (empty? (rest coll))
      (first coll)
      (recur (rest coll)))))

(defcheck solution-16ec083
  (fn [seq] (first (drop (- (count seq) 1) seq))))

(defcheck solution-171adcf9
  (fn [coll] (reduce #(if true %2) coll)))

(defcheck solution-17cc88ee
  (fn my-last [x]
    (if (empty? (rest x))
      (first x)
      (my-last (rest x)))))

(defcheck solution-17f67cfb
  #(nth %1 (- (count %1) 1)))

(defcheck solution-183ab22e
  (fn [x] (first (reverse x) )))

(defcheck solution-19074a90
  (fn [xs]
    (if (empty? (rest xs)) (first xs) (recur (rest xs)))))

(defcheck solution-197768b1
  #(nth % (dec (count %)) ))

(defcheck solution-1992358
  (fn [[x & xs]] (if xs (recur xs) x)))

(defcheck solution-1a4f1ef9
  (fn f [x]
    (let [r (rest x)]
      (if (empty? r) (first x)  (f r)))))

(defcheck solution-1a7d8563
  (fn lst [[f & r]] (if (empty? r) f (recur r))))

(defcheck solution-1ae97b24
  reduce #(do %2))

(defcheck solution-1b6a6fd7
  (fn [coll]
    (if-let [rest-coll (next coll)]
      (recur rest-coll)
      (first coll))))

(defcheck solution-1bd52d0b
  (fn [s]
    (nth s (- (count s) 1))))

(defcheck solution-1bf5cc83
  (fn get-last-elem
    [col]
    (if (= 1 (count col))
      (first col)
      (get-last-elem (rest col)))))

(defcheck solution-1d101ad3
  #(nth (vec %) (- (count %) 1)))

(defcheck solution-1e907a1d
  #(loop [col %]
     (if (empty? (rest col))
       (first col)
       (recur (rest col) )
       )
     ))

(defcheck solution-1f313ff7
  (fn [[h & t]]
    (if (empty? t) h (recur t))))

(defcheck solution-21943fa2
  #(first(reverse %)))

(defcheck solution-21c64350
  (fn [s]
    (if (next s)
      (recur (rest s))
      (first s))))

(defcheck solution-2242eb49
  (fn [alist] (first (reverse alist))))

(defcheck solution-2319b408
  (fn [xs]
    (loop [ys xs]
      (if (empty? (rest ys)) (first ys) (recur (rest ys)) ))))

(defcheck solution-2364099e
  (fn my-last [seq] (if (= seq '()) nil (if (= 1 (count seq)) (first seq) (my-last (rest seq))))))

(defcheck solution-24021ad5
  #(-> % reverse first))

(defcheck solution-245641a6
  reduce #(do %2))

(defcheck solution-24d3478e
  (fn [s]
    (letfn [(my-last [s]
              (let [r (rest s)]
                (if (empty? r)
                  (first s)
                  (my-last r))))]
      (my-last s))))

(defcheck solution-24ed0dab
  (fn last-element
    ([xs]
     (if-let [tail (next xs)]
       (recur tail)
       (first xs)))))

(defcheck solution-25db8866
  #( nth % (- (count %) 1)))

(defcheck solution-27a997ae
  #(nth % (- (count %) 1) ))

(defcheck solution-27d6ea25
  (fn [s] (nth s (-> s count dec))))

(defcheck solution-2800ba3d
  (fn rec [l] (if (= (count l) 1)
                (first l)
                (rec (rest l)))))

(defcheck solution-2803d08c
  (fn  [v]   (reduce (fn [f1 f2] f2 ) v )  ))

(defcheck solution-28e7e28a
  (fn my-last [coll]
    (if (<= (count coll) 1)
      (first coll)
      (my-last (rest coll)))))

(defcheck solution-2aad9e12
  (fn [x] (nth x (- (count x) 1 ) )))

(defcheck solution-2b567616
  (fn[x] (nth x (- (count x) 1))))

(defcheck solution-2c96ee3d
  (fn [x]
    (if-let [r (next x)]
      (recur r)
      (first x))))

(defcheck solution-2ccd7df8
  #(nth % (-(count %) 1)))

(defcheck solution-2cd07d3c
  (fn my-last [x] (if (= (rest x) '()) (first x) (my-last (rest x)))))

(defcheck solution-2f27499b
  (fn [coll]
    (if-not (next coll)
      (first coll)
      (recur (rest coll)))))

(defcheck solution-3150e337
  (fn [c] (nth c (- (count c) 1))))

(defcheck solution-31c29bd4
  (fn [[h & r]] (if (seq r) (recur r) h)))

(defcheck solution-31d3b762
  (fn  [coll] (first (reduce conj ()  coll))))

(defcheck solution-333d79e5
  (fn [[x & xs]]
    (if xs
      (recur xs)
      x)))

(defcheck solution-33d1c54a
  (fn last-t [seqt]
    (if
     (empty? (rest seqt))
      (first seqt)
      (last-t (rest seqt)))))

(defcheck solution-34517a88
  (fn [x]
    (reduce #(identity %2) x)))

(defcheck solution-345e9487
  (fn [s]
    (let [r (rest s)]
      (if (seq r)
        (recur r)
        (first s)))))

(defcheck solution-351bac2f
  #((comp (partial nth %) dec count) %))

(defcheck solution-353c6c07
  #((vec %) (- (count %) 1)))

(defcheck solution-364f789e
  (fn lstel[s]
    (if (next s)
      (lstel (next s))
      (first s)
      )
    ))

(defcheck solution-375efced
  (fn l [[a & b]] (if (zero? (count b)) a (l b))))

(defcheck solution-3762184
  (fn kkk [x]
    (if (nil? (seq (rest x)))
      (first x)
      (kkk (rest x)))))

(defcheck solution-37c48b0a
  (fn remainder [a] (if (empty? (rest a)) (first a) (remainder (rest a)))))

(defcheck solution-382ab612
  (fn [x] (if (next x) (recur (next x)) (first x))))

(defcheck solution-38a376e5
  (fn dosia [xs] (if (empty? (rest xs)) (first xs) (dosia (rest xs)))))

(defcheck solution-399b4f28
  #(if (next %) (recur (next %)) (first %)))

(defcheck solution-3a0d6cd3
  (fn dernier [liste]
    (
      if (= (count liste) 1)
      (first liste)
      (dernier (rest liste))
      )
    ))

(defcheck solution-3a1b585d
  (fn [s] (loop [s s] (let [[x & xs] s] (if-not xs x (recur xs))))))

(defcheck solution-3ac87eed
  #(if (empty? (rest %))
     (first %)
     (recur (rest %))))

(defcheck solution-3b4c151a
  #(nth % (- (alength (into-array %)) 1)))

(defcheck solution-3ba50139
  reduce (fn [n m] m))

(defcheck solution-3c5ec302
  (fn [[head & tail]]
    (if (empty? tail)
      head
      (recur (rest tail)))))

(defcheck solution-3d968f1c
  (fn [sequence]
    (loop [s (rest  sequence), x (first sequence)]
      (cond (empty? s) x
            :else (recur (rest s) (first s))))))

(defcheck solution-3da4c6b5
  (fn [l]
    (loop [[f & args] l]
      (if (empty? args)
        f
        (recur args)))))

(defcheck solution-3daa4c4b
  (fn [sequ] (loop
              [se sequ]
               (let [next (rest se)]
                 (if (empty? next)
                   (first se)
                   (recur next))))))

(defcheck solution-413543d5
  (comp  first reverse))

(defcheck solution-41a8f947
  (fn [x] (reduce #(identity %2) x)))

(defcheck solution-41f023f7
  #(nth %  (- (count %) 1) ))

(defcheck solution-426d25ee
  #(let [[n & r] %]
     (if (seq r)
       (recur r)
       n)))

(defcheck solution-44605d05
  (fn[coll] (nth coll (dec (count coll)))))

(defcheck solution-47404866
  #(nth(reverse %)0))

(defcheck solution-474ed8b3
  (fn mylast [v]
    (if (= (rest v) '())
      (first v)
      (mylast (rest v)))))

(defcheck solution-479e2558
  (fn [lst]
    (loop [l lst]
      (if (empty? (rest l))
        (first l)
        (recur (rest l))))))

(defcheck solution-497b81dc
  (fn [coll]
    (if (empty? (rest coll))
      (first coll)
      (recur (rest coll)))))

(defcheck solution-49ebc04
  (fn b[[f & r]](if (empty? r) f (recur r))))

(defcheck solution-4a294a4c
  #(peek (into [] %)))

(defcheck solution-4a584c22
  #(let [r (rest %)] (if (empty? r) (first %) (recur r))))

(defcheck solution-4a63d94d
  (fn [xs] (nth xs (- (count xs) 1))))

(defcheck solution-4ace1c86
  (fn [seq]
    (let [pos (- (count seq) 1)]
      (if (list? seq)
        (nth seq pos)
        (get seq pos)))))

(defcheck solution-4ba5852a
  (fn [s]
    (if (seq s)
      ; have anything left?
      (if-let [r (seq (rest s))]
        (recur r)
        (first s))
      nil)))

(defcheck solution-4d765eee
  (fn [w] (first (reverse w))))

(defcheck solution-4f9446d
  (fn [coll]
    (nth coll (dec (count coll)))))

(defcheck solution-506fecc5
  ;; explicitly processing the func-args
  (fn [coll]
    (loop [curr (first coll)
           tail (rest coll)]
      (if (empty? tail)
        curr
        (recur (first tail) (rest tail))))))

(defcheck solution-5078f55b
  (fn [v]
    (loop [v v]
      (if (= 1 (count v))
        (first v)
        (recur (rest v))))))

(defcheck solution-507c0339
  (fn last_element [collection]
    (if (next collection)
      (last_element (next collection))
      (first collection))))

(defcheck solution-514868d6
  (fn[x] (first (reverse x))))

(defcheck solution-521a49cc
  ;#(nth % (- (count %) 1))
  (comp first reverse))

(defcheck solution-5348f31e
  reduce (fn [x y] y))

(defcheck solution-53a8f9f3
  #(loop [coll %] (if (empty? (rest coll)) (first coll) (recur (rest coll)))))

(defcheck solution-551ef974
  (fn lastelement [x] (nth x (- (count x) 1))))

(defcheck solution-5558128c
  #(nth % (- (count %) 1)))

(defcheck solution-57b367e4
  (fn [seq] (nth seq (- (count seq) 1))))

(defcheck solution-57f38266
  #(nth % (dec(count %))))

(defcheck solution-57fbe3fc
  (fn last-x [x]
    (if (not-empty (rest x))
      (last-x (rest x))
      (first x))))

(defcheck solution-58639f19
  (fn [c] (reduce (fn [a i] i) c)))

(defcheck solution-588c4b15
  (fn [[x & xs]] (if (seq xs) (recur xs) x)))

(defcheck solution-5ae4ebab
  (fn [sq] (loop [[this & remaining] sq]  (if-let [x (first remaining)] (recur remaining) this))))

(defcheck solution-5b7639ad
  (fn ultimo [s]
    (let [restante (rest s)]
      (if (= 1 (count restante))
        (first restante)
        (ultimo restante)))))

(defcheck solution-5bbada7
  (fn [x] (if (empty? (rest x)) (first x) (recur (rest x)))))

(defcheck solution-5c139c9c
  (fn
    [l]
    (nth l (dec (count l)))))

(defcheck solution-5cf36228
  reduce (fn [_ b] b) ())

(defcheck solution-5d2a5940
  (fn [coll]
    (if-let [nxt (next coll)]
      (recur nxt)
      (first coll))))

(defcheck solution-5d53d3a3
  (fn [coll] (first (reverse coll)) ))

(defcheck solution-5db93345
  (fn [[a & r]] (if (seq r) (recur r) a)))

(defcheck solution-5ddea2c7
  (fn [xs] ((comp first reverse) xs)))

(defcheck solution-5e0550bc
  (fn [l] (first (drop (- (count l) 1) l))))

(defcheck solution-5e20442a
  (fn [s] (if (> (count s) 1)
            (recur (rest s))
            (first s))))

(defcheck solution-5fb2e11f
  (fn beh [x]
    (if (= 1 (count x)) (first x)
                        (beh (rest x)))))

(defcheck solution-6044c9b1
  #(if (empty? (rest %))
     (first %)
     (recur (rest %))))

(defcheck solution-608558e1
  (fn [s]
    (if (empty? (rest s))
      (first s)
      (recur (rest s)))))

(defcheck solution-60a6bf2
  (fn [lst] (loop [head (first lst) tail (rest lst)] (if (empty? tail) head (recur (first tail) (rest tail))))))

(defcheck solution-621494
  (fn [xs]
    (-> xs reverse first)))

(defcheck solution-621f4de0
  (fn mylast
    [myseq]
    (if (empty? (rest myseq))
      (first myseq)
      (mylast (rest myseq)))))

(defcheck solution-63b87bf7
  (fn get_last [l] (if (= (rest l) []) (first l) (get_last (rest l)))))

(defcheck solution-63c2a226
  (fn taste ([lst] (if (empty? (rest lst)) (first lst) (recur (rest lst))))))

(defcheck solution-6459a1e4
  ;;#(first (reverse %))
  (fn [ls] (nth ls (- (count ls) 1))))

(defcheck solution-6561e622
  (fn my-last [l]
    (loop [l l  lst 0]
      (if (empty? l)
        lst
        (recur (rest l) (first l))))))

(defcheck solution-6644bf99
  (fn [x]
    (first (reverse x))))

(defcheck solution-66b94135
  (fn [[x & rest]] (if (seq rest) (recur rest) x)))

(defcheck solution-673d3e75
  (fn [x] (first (rseq (vec x)))))

(defcheck solution-681fbbf3
  (fn my-last [x] (if (= '() (rest x)) (first x) (my-last (rest x)))))

(defcheck solution-694e7c05
  (fn f [x] (if (> (count x) 1) (f (rest x)) (first x))))

(defcheck solution-696cc916
  (fn [s]
    (loop [s' s]
      (let [[a & b] s']
        (if (empty? b)
          a
          (recur b))))))

(defcheck solution-6989db5f
  (fn this [s]
    (if-let [x (next s)]
      (recur x)
      (first s))))

(defcheck solution-69a537a0
  #(if (next %)
     (recur
       (next %))
     (first %)))

(defcheck solution-69d920d0
  (fn lazt [a] (nth a (dec (count a)))))

(defcheck solution-6aa34cbe
  (fn my-last [coll]
    (if (= 1 (count coll))
      (first coll)
      (recur (rest coll)))))

(defcheck solution-6b6e6de3
  (partial reduce (fn [x, y] y)))

(defcheck solution-6c649e32
  (fn [x] (->> x
            count
            dec
            (nth x))))

(defcheck solution-6cb07a22
  #(peek(vec %)))

(defcheck solution-6d747fa8
  (fn [x]
    (if (empty? (rest x))
      (first x)
      (recur (rest x)))))

(defcheck solution-6f1da9c1
  #(loop [lst %]
     (if (empty? (rest lst)) (first lst)
                             (recur (rest lst)))))

(defcheck solution-6f28328
  (fn [x]  (nth x (- (count x) 1))))

(defcheck solution-71a242fa
  (fn last-e [l]
    (cond
      (or (nil? l) (empty? l)) nil
      (empty? (rest l)) (first l)
      :else (recur (rest l)))))

(defcheck solution-71dad868
  (fn [x] (loop [x x] (if (empty? (rest x)) (first x) (recur (rest x))))))

(defcheck solution-7469b95e
  (fn get-last[a-seq]
    (let [el (first a-seq) r-seq (rest a-seq)]
      (if (seq r-seq)
        (get-last r-seq)
        el
        )
      )
    ))

(defcheck solution-74978673
  (fn [ aseq ](first (reverse aseq))))

(defcheck solution-74e1196b
  (fn [s] (let [re (rest s)] (if (empty? re) (first s) (recur re)))))

(defcheck solution-75eb346b
  (fn l [items]
    (if (> (count items) 1)
      (l (rest items))
      (first items)
      )
    ))

(defcheck solution-7616d6b7
  (fn [xs] (first (reverse xs))))

(defcheck solution-7621b0c2
  (fn lst [x] (first (reverse x))))

(defcheck solution-76a78b4e
  #(first (reverse %)))

(defcheck solution-76e7bc43
  (fn [[h & t]] (if t (recur t) h)))

(defcheck solution-77eb9ff6
  (let [f (fn [xs] (loop [us xs] (if (empty? (rest us)) (first us) (recur (rest us)))))] f))

(defcheck solution-782170ea
  #(nth %1 (dec (count %1))))

(defcheck solution-7887a330
  (fn lastElem [x]
    (if (empty? (rest x))
      (first x)
      (lastElem (rest x)))))

(defcheck solution-79373aa4
  (fn [[n & more]](if more (recur more)n)))

(defcheck solution-797ba3a4
  (fn [x] (first (into [](#(reverse %) x)))))

(defcheck solution-7a8206a2
  (fn [lst]
    (reduce (fn [a b] b) nil lst)))

(defcheck solution-7abcdbea
  (fn [seq]
    (first (reverse seq))))

(defcheck solution-7ae1accc
  #(->> %  (reduce (fn [a b] b))))

(defcheck solution-7ed2f445
  (fn [x]
    (loop [lst x]
      (if (empty? (rest lst))
        (first lst)
        (recur (rest lst))))))

(defcheck solution-81635c2a
  (fn getLast [l]
    (if (= (rest l) [])
      (first l)
      (recur (rest l)))))

(defcheck solution-823be169
  #(if (= 1 (count %)) (first %) (recur (rest %))))

(defcheck solution-82c116c6
  reduce (fn [a b] b))

(defcheck solution-832c4602
  (fn [s] (reduce #(identity %2) s)))

(defcheck solution-84641c69
  (fn [x]
    (nth x (dec(count x)))))

(defcheck solution-84d34a44
  #(first (take-last 1 %)))

(defcheck solution-85086dc1
  (fn [e] (first (reverse e))))

(defcheck solution-86e499d1
  #(first (drop (- (count %) 1 ) %)))

(defcheck solution-87645866
  (fn [coll] (nth coll (dec (count coll)))))

(defcheck solution-87c66460
  (fn laast [s] (if (empty? (rest s)) (first s) (laast (rest s)))))

(defcheck solution-87eead25
  (fn [list] (first(reverse list))))

(defcheck solution-87f16b42
  (fn [coll]
    (if-let [rest (next coll)]
      (recur rest)
      (first coll))))

(defcheck solution-8847b19f
  (fn [coll] (first (reverse coll))))

(defcheck solution-890f08eb
  (fn ultimo [lst]
    (if (> (count lst) 1)
      (ultimo (rest lst))
      (first lst))))

(defcheck solution-8952fed1
  #((vec %) (- (count %) 1)))

(defcheck solution-8d17cb62
  (fn mylast [xs]
    (if (= nil (next xs))
      (first xs)
      (mylast (next xs)))))

(defcheck solution-8f10c6e0
  (fn [seq] (if (next seq) (recur (next seq)) (first seq))))

(defcheck solution-908ff45f
  (fn [in]
    (if-let [r (next in)]
      (recur r)
      (first in))))

(defcheck solution-9097e138
  (fn last-element [x] (first (reverse x))))

(defcheck solution-916a9a41
  (fn [s]
    (if (next s)
      (recur (next s))
      (first s))))

(defcheck solution-91dfbe24
  (fn [xs] (reduce (fn [x y] y) xs)))

(defcheck solution-9282c5d1
  #(reduce (fn [x y] y) %))

(defcheck solution-92a83ec8
  (fn [[x & xs]]
    (if (seq xs)
      (recur xs)
      x)))

(defcheck solution-9326e2e2
  (fn [lst] (first (take 1
                     (for [i (iterate #(rest %) lst) :when (= 1 (count i))] (first i))))))

(defcheck solution-93ddc231
  #(if (next %)
     (recur (next %))
     (first %)))

(defcheck solution-941e05a9
  (fn mylast [a] (let [ra (rest a)] (if (empty? ra) (first a) (mylast ra)))))

(defcheck solution-951add8f
  (fn [s]
    (first (reverse s))))

(defcheck solution-9562e94b
  (fn myfunc [x] (if (empty? (rest x)) (first x) (myfunc (rest x)))))

(defcheck solution-973a3198
  reduce #(-> %2))

(defcheck solution-97b5206b
  #(loop [x %1] (if (empty? (rest x)) (first x) (recur (rest x)))))

(defcheck solution-98c75681
  #(->> %
     count
     dec
     (nth %)))

(defcheck solution-995bf9e0
  (fn mylast [seq]
    (if (= (rest seq) [])
      (first seq)
      (mylast (rest seq)))))

(defcheck solution-99da0d6
  (fn [l]
    (nth l (dec (count l)))))

(defcheck solution-9af80234
  reduce #(-> %2))

(defcheck solution-9b3fcc98
  (fn my-last [s]
    (if (next s)
      (recur (next s))
      (first s))))

(defcheck solution-9b911360
  (fn [a-seq]
    (if (empty? (rest a-seq))
      (first a-seq)
      (recur (rest a-seq)))))

(defcheck solution-9bda73be
  (fn laast [lst]
    (if (empty? (rest lst))
      (first lst)
      (laast (rest lst)))))

(defcheck solution-9c1ddbe
  (fn [v] (nth v (dec (count v)))))

(defcheck solution-9c297ccb
  #(nth %1(-(count %1)1)))

(defcheck solution-9cb810e3
  #(
     if( empty? (rest %))
     (first %)
     (recur (rest %))
     ))

(defcheck solution-9d1a1467
  #(nth % (dec (count %))))

(defcheck solution-9d52254c
  (fn [seq] (loop [seq seq]
              (let [rest (rest seq)]
                (if (empty? rest)
                  (first seq)
                  (recur rest))))))

(defcheck solution-9f409800
  reduce (fn [_ x] x))

(defcheck solution-9fcf8945
  (fn f [x] #_(println x) (if (= 1 (count x)) (first x) (f (rest x)))))

(defcheck solution-a0185dff
  reduce #(identity %2))

(defcheck solution-a09642a3
  #(first (reverse %1)))

(defcheck solution-a25ed388
  (fn [xs] (first (take-last 1 xs))))

(defcheck solution-a2ab78b6
  (fn [s] (first(reverse s))))

(defcheck solution-a4b997c3
  (fn [l] (first (reverse l))))

(defcheck solution-a4cb670d
  (fn [[f & rst]] (if (not rst) f (recur rst))))

(defcheck solution-a51277df
  #(let [rest (rest %)]
     (if (= () rest)
       (first %)
       (recur rest))))

(defcheck solution-a5334091
  reduce #(identity %2 ))

(defcheck solution-a6454945
  (fn my-last [s]
    (if (empty? (rest s))
      (first s)
      (my-last (rest s)))))

(defcheck solution-a650012
  (fn [x] (first (reverse x))))

(defcheck solution-a66d9206
  (fn [lst] (if (empty? (rest lst))
              (first lst)
              (recur (rest lst)))))

(defcheck solution-a7980830
  (fn [xs] (loop [ys xs] (if (empty? (rest ys)) (first ys) (recur (rest ys))))))

(defcheck solution-a7b80e1c
  (comp first reverse ))

(defcheck solution-a7d2cc82
  (fn find-last [sequence]
    (if (seq (rest sequence)) (find-last (rest sequence))
                              (first sequence))))

(defcheck solution-a9252ad6
  (fn [l] (letfn [(lo [l] (if (= 1 (count l)) (first l) (lo (rest l))))] (lo l))))

(defcheck solution-a92b46cc
  (fn [l] (nth l (- (count l) 1))))

(defcheck solution-aa678596
  (fn [seq] (first (reverse seq))))

(defcheck solution-abafaca5
  (fn [l] (first (take-last 1 l))))

(defcheck solution-abbdc356
  #(first( reverse  %)))

(defcheck solution-ac5eba6b
  (fn [a] (first (drop (- (count a) 1) a))))

(defcheck solution-acda44ce
  (fn [x] (peek (vec x))))

(defcheck solution-ad337a3a
  #(reduce (fn [_ x] x) %))

(defcheck solution-ad42911a
  #(if (list? %) (peek (reverse %)) (peek %)))

(defcheck solution-ae2f98b3
  (fn last-s [s]
    (let [[h & t] s]
      (if (nil? t)
        h
        (last-s t)))))

(defcheck solution-ae642e4c
  (fn mylast [x]
    (if (empty? (rest x))
      (first x)
      (mylast (rest x)))))

(defcheck solution-afe754cb
  #(-> % (reverse) (first)))

(defcheck solution-b0434e23
  (fn [coll] (nth coll (- (count coll) 1))))

(defcheck solution-b162ca0a
  #(loop [arr %]
     (if (next arr)
       (recur (next arr))
       (first arr))))

(defcheck solution-b380c3a4
  #(if (= (count %) 1) (first %) (recur (rest %))))

(defcheck solution-b381ee1c
  (fn [x]
    (loop [l x n (first x)]
      (if (empty? l)
        n
        (recur (rest l) (first l))))))

(defcheck solution-b3a072c8
  (fn [s]
    (loop [head (first s) tail (rest s)]
      (if (empty? tail)
        head
        (recur (first tail) (rest tail))))))

(defcheck solution-b5286a73
  (fn [[f & r]]
    (if r
      (recur r)
      f)))

(defcheck solution-b6027881
  (fn [x] (nth x (dec (count x)))))

(defcheck solution-b661b4d9
  (fn get-last [sq] (first (reverse sq))))

(defcheck solution-b6f0dd45
  (fn last-element[x]
    (if (= '() (rest x))
      (first x)
      (recur (rest x)))))

(defcheck solution-b966d1
  (fn [c] (nth c (dec (count c)))))

(defcheck solution-b99ac0f2
  (fn [x] (first(reverse x))))

(defcheck solution-b9b18d0f
  (fn my-last [[x & xs]]
    (if xs
      (my-last xs)
      x)))

(defcheck solution-ba3dcc2
  (fn [col] (nth col (- (count col) 1))))

(defcheck solution-ba6dac6c
  (fn [x] (->> x (count) (dec) (nth x))))

(defcheck solution-bb741cdf
  (fn [seq]
    (loop [cur-seq1 seq]
      (if (<= (count cur-seq1) 1)
        (first cur-seq1)
        (recur (rest cur-seq1))))))

(defcheck solution-bbb7a893
  (fn [seq] (loop [[f & rest] seq] (if (empty? rest) f (recur rest)))))

(defcheck solution-bbb7bf67
  (fn [elems]
    ((fn [prev remain]
       (if (empty? remain)
         prev
         (recur (first remain) (rest remain))))
     (first elems) (rest elems))))

(defcheck solution-bc42dd26
  (fn [x]
    (nth x (dec (count x)))))

(defcheck solution-bc4bdaf0
  (fn [s]
    (let [p (rest s)]
      (if (empty? p)
        (first s)
        (recur p)))))

(defcheck solution-bc800a97
  (fn [coll]
    (let [n (next coll)]
      (if n
        (recur n)
        (first coll)
        )
      )
    ))

(defcheck solution-bcf85aee
  (fn [[x & xs]] (if (empty? xs) x (recur xs))))

(defcheck solution-bd6b24e4
  (fn [x] (nth x (- (count x) 1))))

(defcheck solution-bf475b8e
  (fn llast [x] (if (= 1 (count x)) (first x) (llast (rest x)))))

(defcheck solution-bfbc3aab
  (fn [col] (first (reverse col))))

(defcheck solution-c0317956
  (fn last-element [coll]
    (nth coll (- (count coll) 1))))

(defcheck solution-c20cf3e4
  (fn [s]
    (if (next s)
      (recur (next s))
      (first s))))

(defcheck solution-c3dbe2b1
  #(nth (reverse %) 0))

(defcheck solution-c581f0d4
  #(if (< (count %) 2)
     (first %)
     (recur (rest %))))

(defcheck solution-c60c9729
  #(peek (vec %)))

(defcheck solution-c6a5f440
  #(get (vec %) (dec (count %))))

(defcheck solution-c6d25a55
  #((vec %) (dec (count %))))

(defcheck solution-c6fd4933
  (fn lst [s] (if (seq (rest s)) (lst (rest s)) (first s))))

(defcheck solution-c73f6e22
  (fn [x]
    (reduce
      (fn [x y]
        y
        )
      x
      )
    ))

(defcheck solution-c8060556
  ( fn [coll] (if-let [r (next coll)] (recur r) (first coll))))

(defcheck solution-c80e28df
  (fn [s] (nth s (- (count s) 1))))

(defcheck solution-c83a8e0c
  (fn [s] (nth s (dec (count s)) )))

(defcheck solution-c878e9a5
  (fn [coll]
    (loop [[f & r] coll]
      (if (empty? r)
        f
        (recur r)))))

(defcheck solution-c8c6af3a
  (fn [lst] (first (reverse lst))))

(defcheck solution-ca4e83b2
  (fn [s] (let [r (rest s)]
            (if (empty? r)
              (first s)
              (recur r)
              )
            )
    ))

(defcheck solution-ca6e6376
  #(first (drop (dec (count %)) %)))

(defcheck solution-ca77f281
  (fn [xs]
    (cond
      (empty? xs) nil
      (empty? (rest xs)) (first xs)
      :else (recur (rest xs)))))

(defcheck solution-cbbed850
  #(reduce (fn [memo value] value) %))

(defcheck solution-cbf76984
  (fn [x] (nth x (- (count x) 1) )))

(defcheck solution-cce1cb36
  (fn
    [x]
    (if (next x)
      (recur (next x))
      (first x))))

(defcheck solution-cf17585f
  (fn [coll] (first (take-last 1 coll))))

(defcheck solution-cf487cc6
  (fn [s] (if-let [r (next s)] (recur r) (first s))))

(defcheck solution-d0156051
  (fn [x] (loop [a x] (let [[h & t] a] (if (= t nil) h (recur t))))))

(defcheck solution-d068dd8e
  (fn [s] (first (reverse s))))

(defcheck solution-d1e1165d
  (fn [coll]
    (if (next coll)
      (recur (next coll))
      (first coll))))

(defcheck solution-d20b0802
  (fn my-last [lista]
    (if (= (count (rest lista)) 0) (first lista) (my-last (rest lista)))
    ))

(defcheck solution-d2306ccf
  (fn [items]
    (first (reverse items))))

(defcheck solution-d257530a
  (partial reduce (fn [x y] y)))

(defcheck solution-d27b5c76
  (fn hello [c] (nth c (- (count c) 1))))

(defcheck solution-d2c2d9b1
  (fn last-el [x] (if (empty? (next x)) (first x) (last-el (next x)))))

(defcheck solution-d30c8835
  (fn [[f & r]] (if r (recur r) f)))

(defcheck solution-d357b5b2
  (fn
    [ary]
    (nth ary (- (count ary) 1))
    ))

(defcheck solution-d4b08d4d
  (fn mylast [s] (if (= 1 (count s)) (first s) (recur (next s)))))

(defcheck solution-d71a255a
  (fn[a-seq] (first (reverse a-seq))))

(defcheck solution-d7f0aafc
  (fn [l] (if (empty? (rest l)) (first l) (recur (rest l)))))

(defcheck solution-d8abe653
  (fn [s]
    (if (> (count s) 1)
      (recur (rest s))
      (first s))))

(defcheck solution-d9c5b589
  (fn mylast [x]
    (if (= (rest x) ()) (first x) (mylast (rest x)))))

(defcheck solution-da02fa93
  #(reduce (fn [a b] b) %))

(defcheck solution-dbd093a8
  #(reduce (fn [x y] y) nil %))

(defcheck solution-dd04ff4e
  (fn [x] (->> x count dec (nth x))))

(defcheck solution-dd2cab37
  (fn [seq] (reduce (fn [a b] b) seq)))

(defcheck solution-dd73dfd3
  (fn f ([c]
         (cond
           (= (next c) nil) (first c)
           :else (f (next c))
           )
         )
    ))

(defcheck solution-de99a4d6
  #(-> % vec peek))

(defcheck solution-dea88675
  (fn [x] (
            if (next x)
            (recur (next x))
            (first x)
            )
    ))

(defcheck solution-df8f014a
  (fn [s] (loop [[f & r] s] (if (seq r) (recur r) f))))

(defcheck solution-e0aa1115
  (fn my-last [sq] (first (reverse sq))))

(defcheck solution-e0eabc5b
  (fn [l] (reduce (fn [a b] b) l)))

(defcheck solution-e18128c0
  (fn [sq] (reduce #(if (= nil %2) %1 %2) sq)))

(defcheck solution-e1e403a9
  (fn [x]
    (nth x
      (-
       (count x)
       1))))

(defcheck solution-e1e8f6a7
  reduce #(when true %2))

(defcheck solution-e25e5be9
  #(loop [result nil col %1]
     (if (empty? col)
       result
       (recur (first col) (rest col)))))

(defcheck solution-e27d8ec5
  #(-> % (reverse) (first) ))

(defcheck solution-e298c11f
  apply (fn oi
          ( [x & more]
           (if (nil? more) x (apply oi more))
           )
          ))

(defcheck solution-e2cf660c
  (fn [sq] (first (reverse sq))))

(defcheck solution-e30c456c
  #(first (reverse %)))

(defcheck solution-e4808f4b
  ; reduce #(-> %2)
  (fn last-elt [l] (if (= (rest l) []) (first l) (last-elt (rest l)))))

(defcheck solution-e4aebda8
  (fn [seq]
    (loop [remain seq]
      (if (empty? (rest remain))
        (first remain)
        (recur (rest remain))))))

(defcheck solution-e51d8768
  (fn [[ head & rest ]]
    (if (= rest nil) head (recur rest))))

(defcheck solution-e6fd6d37
  (fn [l] (reduce #(do %2) 0 l)))

(defcheck solution-e7658a4f
  #(first (reverse  %)))

(defcheck solution-e779fbcf
  #(if (empty? (rest %)) (first %) (recur (rest %))))

(defcheck solution-e7d9513b
  (fn [x]
    (loop [[f & more] x]
      (if
       (empty? more)
        f
        (recur more)
        )
      )))

(defcheck solution-e82546db
  #(get (into [] %) (dec (count %))))

(defcheck solution-e90dd5d2
  (fn [s]
    (if-let [r (next s)]
      (recur r)
      (first s))))

(defcheck solution-ea24fcc1
  #(first (take 1 (reverse %))))

(defcheck solution-ead0be60
  (fn [v]
    (let [l (rest v)]
      (if (= 1 (count l))
        (first l)
        (recur l)))))

(defcheck solution-ec7011b8
  (fn [s0]
    (loop [s s0]
      (let [f (first s)
            r (next s)]
        (if (nil? r)
          f
          (recur r))))))

(defcheck solution-eee0f155
  (fn myLast [[h & t]] (if (empty? t) h (myLast t))))

(defcheck solution-eee47c17
  (fn [l]
    (loop [le l]
      (if (nil? (next le))
        (first le)
        (recur (next le))))))

(defcheck solution-ef34e86b
  (fn [xs] (peek (vec xs))))

(defcheck solution-f07764c8
  (comp peek vec))

(defcheck solution-f2e3126b
  #(first ( drop (dec (count %)) %)))

(defcheck solution-f39c25e4
  (fn f [x] (if (empty? (rest x)) (first x) (f (rest x)))))

(defcheck solution-f46151d5
  (fn [[x & xs]]
    (loop [result x
           temp xs]
      (if (empty? temp)
        result
        (recur (first temp) (rest temp))))))

(defcheck solution-f4ebfbe8
  (fn [c] (first (reverse c))))

(defcheck solution-f6d306d1
  (fn [xs] (case (count xs) 1 (first xs) (recur (rest xs)))))

(defcheck solution-f78ea1d7
  (fn [x]
    (if (next x)
      (recur (next x))
      (first x))))

(defcheck solution-f7b6017d
  #(loop [[first & rest] %]
     (if (empty? rest)
       first
       (recur rest)
       )
     ))

(defcheck solution-f8211b6f
  (fn lst [x] (if (= (second x) nil) (first x) (lst (rest x)))))

(defcheck solution-f8ce3b37
  (fn [xs] (if (empty? (rest xs))
             (first xs)
             (recur (rest xs)))))

(defcheck solution-f91c4c81
  (fn [input] (first (reverse input))))

(defcheck solution-f9976cef
  (fn my-last [lst] (first (reverse lst))))

(defcheck solution-f9a94b7d
  (fn [s] (nth s (dec (count s)))))

(defcheck solution-f9e56552
  (fn [[h & r]] (if r (recur r) h)))

(defcheck solution-fb0bb2c5
  (fn get-last [xs]
    (if (= (count xs) 1)
      (first xs)
      (get-last (rest xs)))))

(defcheck solution-fb6d874
  #(reduce (fn [_ e] e) %))

(defcheck solution-fbfcaf0c
  (fn [s]
    (cond
      (= 1 (count s)) (first s)
      :else (recur (rest s)))))

(defcheck solution-fc099e78
  (fn [[a & r]]
    (if (empty? r) a (recur r))))

(defcheck solution-fca85f94
  (fn [l] (cond (empty? l) nil (empty? (rest l)) (first l) :else (recur (rest l)))))

(defcheck solution-fcfae375
  (partial reduce (fn [_ y] y) nil))

(defcheck solution-fd537bcc
  (fn [x]
    (if (empty? (rest x))
      (first x)
      (recur (rest x)))))

(defcheck solution-fd5bc158
  (fn [s]
    (if (empty? (rest s))
      (first s)
      (recur (rest s)))))

(defcheck solution-fdd9c5fa
  #(if (next %)
     (recur (next %))
     (first %)))

(defcheck solution-fdea0550
  #(loop [items %1]
     (let [r (rest items)]
       (if (empty? r)
         (first items)
         (recur r)))))

(defcheck solution-fe28065b
  (fn newlast [x] (get (vec x) (- (count x) 1))))

(defcheck solution-fe7f5293
  (fn [xs]
    (if (= 1 (count xs))
      (first xs)
      (recur (rest xs)))))

(defcheck solution-ff42ce7c
  (fn [elements]
    (nth elements (- (count elements) 1))
    ))

(defcheck solution-ff7f2148
  (fn [coll] (reduce #(identity %2) coll)))

(defcheck solution-ff8f6639
  (fn a [l] (nth l (- (count l) 1))))
