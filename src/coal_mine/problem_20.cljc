(ns coal-mine.problem-20
  (:require [coal-mine.checks :refer [defcheck-20] :rename {defcheck-20 defcheck}]
            [clojure.test]))

(defcheck solution-10a4ce04
  #(loop [items %1]
     (let [r (rest items)]
       (if (= (count r) 1)
         (first items)
         (recur r)))))

(defcheck solution-1112ad22
  #(if (= (count %) 2) (first %) (recur (rest %))))

(defcheck solution-11ccc39a
  #(second (rseq (vec %))))

(defcheck solution-14ba5401
  #(-> % reverse second))

(defcheck solution-1556a9b8
  #(nth (rseq (vec %)) 1))

(defcheck solution-15758faf
  (fn [e] (second (reverse e))))

(defcheck solution-1589cd71
  #(nth %(- (count %) 2)))

(defcheck solution-163c4c06
  (fn penultimate
    ([xs]
     (when-let [previous (first xs)]
       (when-let [tail (next xs)]
         (if (next tail)
           (recur tail)
           previous))))))

(defcheck solution-165ea33f
  (fn get-second-last [sq] (second (reverse sq))))

(defcheck solution-1686a9c6
  (fn lst [x] (if (= (second (rest x)) nil) (first x) (lst (rest x)))))

(defcheck solution-169b0963
  #(first( take-last 2 %)))

(defcheck solution-18da7a59
  (fn [x]
    (nth x (- (count x) 2))))

(defcheck solution-1a3d7422
  #(first(drop (- (count %) 2) %)))

(defcheck solution-1a550def
  #(-> % reverse rest first))

(defcheck solution-1af41e24
  (fn [s] (second(reverse s)) ))

(defcheck solution-1b4e6cd5
  (fn p [[x & [y & z :as w]]] (if (empty? z) x (p w))))

(defcheck solution-1b645a3a
  #( nth % (- (count %) 2)))

(defcheck solution-1b945263
  (fn [x] (-> x (reverse) (nth 1))))

(defcheck solution-1d15e02c
  (fn my-last [x]
    (if (= (rest (rest x)) ())
      (first x)
      (recur (rest x)))))

(defcheck solution-1d53b5b3
  (comp peek vec drop-last))

(defcheck solution-1ff8be8a
  #(-> % drop-last reverse first))

(defcheck solution-20101a40
  (fn [items]
    (second (reverse items))))

(defcheck solution-207d3c49
  (fn f [x] (if (= 2 (count x)) (first x) (f (rest x)))))

(defcheck solution-214c8a1
  (fn [l]
    (loop [[f & args] l]
      (if ( = (count args) 1)
        f
        (recur args)))))

(defcheck solution-21dc443c
  (comp #(nth % 1) reverse))

(defcheck solution-222aea34
  (comp peek pop vec))

(defcheck solution-2284e20f
  (fn [col] (second (reverse col))))

(defcheck solution-2305f96a
  #(if (next (rest %))
     (recur (rest %))
     (first %)
     ))

(defcheck solution-24353a99
  (fn [alist] (second (reverse alist))))

(defcheck solution-243ea3c
  (fn second-to-last [x]
    (let [y (rest x)]
      (if (= 1 (count y))
        (first x)
        (recur y)))))

(defcheck solution-24e9a22c
  #(if (= 2 (count %)) (first %) (recur (rest %))))

(defcheck solution-24ebc959
  #(loop [f (first %) r (rest %) rr (rest r)] (if (empty? rr) f (recur (first r) rr (rest rr)))))

(defcheck solution-264b007b
  (fn [x] (first (rest (reverse x))) ))

(defcheck solution-2765a7c3
  (fn met [x] (nth x (- (count x) 2) )))

(defcheck solution-2921890d
  (fn [x] (if (next (next x)) (recur (next x)) (first x))))

(defcheck solution-29f9e5c8
  #(first (reduce (fn [[p0 p1] x] [p1 x]) [nil nil] %)))

(defcheck solution-2ad7fcfd
  #(last (butlast %)))

(defcheck solution-2bdbc1e3
  (fn [[a b & more :as args]]
    (if more
      (recur (rest args))
      a)))

(defcheck solution-2cac8d33
  (fn [s]
    (nth s (- (count s) 2))))

(defcheck solution-2dad07a5
  (fn pre-last[a-seq]
    (nth a-seq (- (count a-seq) 2))
    ))

(defcheck solution-2eb2eb51
  (fn [l]
    (first (drop (- (count l) 2) l))))

(defcheck solution-31ecbdc6
  (fn penultimo [s]
    (if (=  2 (count s))
      (first s)
      (let [restante (rest s)]
        (if (= 2 (count restante))
          (first restante)
          (penultimo restante))))))

(defcheck solution-331723b8
  (fn penultimate [seq]
    (if (or (= nil seq) (= 1 (count seq))) nil
                                           (if (= 2 (count seq)) (first seq)
                                                                 (penultimate (rest seq))))))

(defcheck solution-339fa39b
  (comp last butlast))

(defcheck solution-3477b234
  (fn [ aseq ](second (reverse aseq))))

(defcheck solution-34f442a9
  #(first (reduce (fn [x y] [(last x) y]) [nil] %)))

(defcheck solution-36147aed
  #(first(rest(reverse %))))

(defcheck solution-365f0284
  (fn [sq] (first (rest (reverse sq)))))

(defcheck solution-37b81adf
  (fn [[f1 f2 & r]]
    (if r (recur (cons f2 r)) f1)))

(defcheck solution-37c333f1
  (fn [col]
    (if (= (count (rest col)) 1)
      (first col)
      (recur (rest col)))))

(defcheck solution-3868de9b
  (fn [s] ((comp second reverse) s)))

(defcheck solution-38a8282
  #(nth (reverse %1) 1))

(defcheck solution-3933a807
  (fn [x] (first (rest (reverse x)))))

(defcheck solution-3b1e422d
  #(first (rest (reverse %1))))

(defcheck solution-3cca1f4b
  (fn [lst]
    (loop [head (first lst) tail (rest lst)]
      (if (empty? (rest tail))
        head
        (recur (first tail) (rest tail))))))

(defcheck solution-3cd4d910
  #(first (drop (- (count %1) 2) %1)))

(defcheck solution-3cd5c4e8
  #(nth %1 (- (count %1) 2) (first %1)))

(defcheck solution-407d93bc
  (fn
    [l]
    (nth l (- (count l) 2))))

(defcheck solution-420562ea
  (fn [x]
    (->> x
      (reverse)
      (drop 1)
      (first))))

(defcheck solution-422b6ca8
  (fn [x] (nth x (- (count x) 2))))

(defcheck solution-458b63d3
  (comp (partial reduce (fn [_ y] y) nil) butlast))

(defcheck solution-458d623c
  (fn [s]
    (let [f (first s)
          r (rest (rest s))]
      (if (seq r)
        (recur (rest s))
        f))))

(defcheck solution-471448d8
  (fn [coll]
    (

     (fn [coll prev]
       (if-let [re (next coll)]
         (recur re (first coll) )
         prev
         )
       )
     coll nil
     )

    ))

(defcheck solution-4747f4ad
  (fn [s]
    (loop [head (first s) tail (rest s)]
      (if (= 1 (count (take 2 tail)))
        head
        (recur (first tail) (rest tail))))))

(defcheck solution-478f950f
  (fn [l](first (take-last 2 l))))

(defcheck solution-4818ae66
  (fn penultimate-element [coll]
    (nth coll (- (count coll) 2))))

(defcheck solution-482a23a5
  (fn [s] (first (take-last 2 s))))

(defcheck solution-493f0d06
  (fn [x] (loop [[h & t] x] (if (= t [(last x)]) h (recur t)))))

(defcheck solution-4c7c8631
  (fn [l](second (reverse l))))

(defcheck solution-4ca1abde
  #((vec %) (- (count %) 2)))

(defcheck solution-4d94f9a5
  (fn [col] (first (take-last 2 col) ) ))

(defcheck solution-4df61178
  #(second (reverse %1)))

(defcheck solution-4f544ff2
  (fn myfn [seq] (first (take-last 2 seq))))

(defcheck solution-4fc18228
  #(last (drop-last %1)))

(defcheck solution-4fdaed8d
  (fn penul [s]
    (if (< (count s) 3)
      (first s)
      (penul (rest s)))))

(defcheck solution-502566b5
  (fn snd-last [lst]
    (if (empty? (rest (rest lst)))
      (first lst)
      (snd-last (rest lst)))))

(defcheck solution-514f9eba
  (fn [xs]
    (loop [xs xs]
      (cond (> (count xs) 2) (recur (rest xs))
            (= (count xs) 2) (first xs)
            :else nil))))

(defcheck solution-5332ff83
  (fn my-2nd-last [s]
    (if (nil? (nnext s))
      (first s)
      (recur (next s)))))

(defcheck solution-53587a2b
  (fn [x] (second (reverse x))))

(defcheck solution-542ac718
  (fn [s] (peek (pop (vec s)))))

(defcheck solution-55242c2b
  (fn [coll]
    (if (= (count coll) 2)
      (first coll)
      (recur (next coll)))))

(defcheck solution-55d9a0cf
  (fn [coll] (nth coll (- (count coll) 2))))

(defcheck solution-567fb36a
  (fn [seq]
    (second (reverse seq))))

(defcheck solution-56d85db3
  (fn [c] (nth (reverse c) 1)))

(defcheck solution-5827e0cd
  #(-> % (reverse) (second)))

(defcheck solution-5894fe8c
  #(last (drop-last %)))

(defcheck solution-5928e228
  (fn [x]   (first (rest (reverse x)) )))

(defcheck solution-59dc2308
  (fn [x]
    (-> x reverse second)))

(defcheck solution-5a49780a
  (fn [xs] (->> xs reverse rest first)))

(defcheck solution-5a9895c8
  #(-> % (reverse) (rest) (first)))

(defcheck solution-5a9e989f
  (comp last #(take (dec (count %)) %)))

(defcheck solution-5ad24d6a
  (fn [coll]
    (if (empty? (rest (rest coll)))
      (first coll)
      (recur (rest coll)))))

(defcheck solution-5ae70e0f
  #(-> (partition 2 1 %)
     last
     first))

(defcheck solution-5c0f752e
  (fn [l]
    (if (<= (count l) 2)
      (first l)
      (recur (rest l)))))

(defcheck solution-5c23772c
  (fn [lst] (if (= 2 (count lst))
              (first lst)
              (recur (rest lst)))))

(defcheck solution-5cd2dba0
  (fn [[a & r]] (if (seq (rest r)) (recur r) a)))

(defcheck solution-5e49e5e7
  (comp first last #(partition 2 1 %)))

(defcheck solution-5e6bedda
  #(first (take-last 2 %)))

(defcheck solution-5f1227d6
  #(let [n (count %)] (when (> n 1) (nth % (- n 2)))))

(defcheck solution-5fdc05d7
  (fn kkk [ls]
    (if (nil? (nnext ls))
      (first ls)
      (kkk (rest ls)))))

(defcheck solution-5fe9a82e
  (fn [[a b & c]] (if c (recur (conj c b)) a)))

(defcheck solution-609039c
  (fn [sq] (loop [[this & remaining] sq]  (if-let [x (second remaining)] (recur remaining) this))))

(defcheck solution-60a4055b
  (fn [x]
    (loop [l x n (first x)]
      (if (empty? (rest l))
        n
        (recur (rest l) (first l))))))

(defcheck solution-60d99c8a
  (fn [xs] (if (empty? (rest (rest xs))) (first xs) (recur (rest xs)))))

(defcheck solution-617a3171
  #(first(take-last 2 %1)))

(defcheck solution-61e5ea45
  #((comp first rest reverse) %))

(defcheck solution-632bb76d
  (fn [s] (second (reverse s))))

(defcheck solution-63601134
  #(nth % (- (count %) 2)))

(defcheck solution-6467fdca
  (fn [s]
    (if (->> s next next)
      (recur (rest s))
      (first s))))

(defcheck solution-64b3ed31
  #(nth % (dec (dec (count %)))))

(defcheck solution-655e21d7
  (fn get-last [xs]
    (if (= (count xs) 2)
      (first xs)
      (get-last (rest xs)))))

(defcheck solution-656cc2ef
  (fn reverse-2th [v]
    (if (= (count v) 2)
      (first v)
      (reverse-2th (rest v)))))

(defcheck solution-65869358
  (fn [my-collection] (second (reverse my-collection))))

(defcheck solution-68067541
  (comp second rseq vec))

(defcheck solution-68f57b60
  (fn [coll] (last (butlast coll))))

(defcheck solution-6a9281c8
  #(-> % butlast last))

(defcheck solution-6ae85ab3
  (fn second-to-last
    [collection]
    (let [c (next collection)]
      (if (next c)
        (second-to-last c)
        (first collection)))))

(defcheck solution-6ee6b928
  (fn [seq] (last (butlast seq))))

(defcheck solution-6fa6b595
  (fn [lst] (first (drop 1 (reverse lst)))))

(defcheck solution-724086d6
  (fn mysecond [myseq] (last (take (- (count myseq) 1) myseq ))))

(defcheck solution-725a402d
  (fn beh [x]
    (if (= 2 (count x)) (first x)
                        (beh (rest x)))))

(defcheck solution-7332de7c
  (fn[x] (nth x (- (count x) 2))))

(defcheck solution-75b987a0
  (fn [a-seq]
    (if (>= 2 (count a-seq))
      (first a-seq)
      (recur (rest a-seq)))))

(defcheck solution-75bfb9d1
  (fn [x] (first (rest(reverse x)))))

(defcheck solution-7698c9f
  #(-> % reverse next first))

(defcheck solution-7760fee7
  #(loop [x %] (if (empty? x) nil (let [xr (rest x)] (if (empty? (rest xr)) (first x) (recur xr)) ) )))

(defcheck solution-783874c0
  (fn pen [xs]
    (if (= nil (next (next xs)))
      (first xs)
      (pen (next xs)))))

(defcheck solution-7971fcc
  (comp second reverse))

(defcheck solution-79fc43aa
  #(apply (fn [x y & z] y) (reverse %)))

(defcheck solution-7c445f65
  (fn [sequence] (second (reverse sequence))))

(defcheck solution-7c7d25f2
  #(last(butlast %)))

(defcheck solution-7d6788be
  (fn[x] (second (reverse x))))

(defcheck solution-7dc73e49
  (fn last2 [seq]
    (if (= (rest (rest seq)) [])
      (first seq)
      (last2 (rest seq)))))

(defcheck solution-7de4f223
  #(loop [arr %]
     (if (nnext arr)
       (recur (next arr))
       (first arr))))

(defcheck solution-7e998388
  (fn [listy] (first (rest (reverse listy)))))

(defcheck solution-7efa54bb
  (fn [coll] (nth coll (dec (dec (count coll))))))

(defcheck solution-7f97c727
  (comp  first rest reverse))

(defcheck solution-7fd9df0b
  (fn [xs] (last (butlast xs))))

(defcheck solution-8141cf14
  (fn lst [[f & r]] (if (= 1 (count r)) f (recur r))))

(defcheck solution-81df15e
  (fn [s]
    (let [r (rest s)]
      (if (empty? (rest r))
        (first s)
        (recur r)
        )
      )
    ))

(defcheck solution-84253918
  (fn [xs] (first (take-last 2 xs))))

(defcheck solution-8722ca44
  (fn rec [l]
    (let [n (rest l)]
      (if (= (count n) 1)
        (first l)
        (rec n)))))

(defcheck solution-87477a8c
  #(-> % reverse rest first))

(defcheck solution-87965251
  (fn [xs] (first (rest (reverse xs)))))

(defcheck solution-8843be0d
  #(loop [result nil col %1 cur 0 i (- (count col) 1)]
     (if (= i cur)
       result
       (recur (first col)(rest col) (inc cur) i))))

(defcheck solution-88ab4ed0
  (fn [coll] (nth coll (dec (dec(count coll))))))

(defcheck solution-89393c4c
  #(peek (pop (into [] %))))

(defcheck solution-8a437979
  (fn [x] (nth (reverse x) 1)))

(defcheck solution-8a44c352
  #(->> % reverse rest first))

(defcheck solution-8b182300
  (fn [l] (first (rest (reverse l)))))

(defcheck solution-8e512461
  #((into [] %) (- (count %) 2)))

(defcheck solution-8ee9baf3
  #(second (reverse %)))

(defcheck solution-9094119d
  (fn [coll] ((comp first rest reverse) coll)))

(defcheck solution-90bead88
  (fn [list] (nth list (- (count list) 2 ) )))

(defcheck solution-9486900f
  #(nth % (-(count %) 2) ))

(defcheck solution-94877b7f
  (fn [coll] (second (reverse coll))))

(defcheck solution-9816cf1a
  (fn f ([l] (f nil l)) ([x l] (if-let [n (next l)] (f (first l) n) x))))

(defcheck solution-9839036d
  (fn [c] (first (rest (reverse c)))))

(defcheck solution-9e2a8d2c
  (fn [xs] (second (rseq (vec xs)))))

(defcheck solution-9e8a42ee
  #(-> % vec pop peek))

(defcheck solution-9ec7da66
  (fn [l] ((comp second reverse) l)))

(defcheck solution-9fc011a8
  (fn mylast [a] (let [[fa sa & rst] a] (if (empty? rst) fa (mylast (rest a))))))

(defcheck solution-a33d2b39
  (fn [xs] (nth xs (- (count xs) 2))))

(defcheck solution-a3fde3f8
  (fn [s] (if (> (count s) 2) (recur (rest s)) (first s))))

(defcheck solution-a41de81f
  (fn [s]
    ((vec (reverse s)) 1)))

(defcheck solution-a4fdbcba
  #( first (rest (reverse %))))

(defcheck solution-a5145514
  (fn [seq] (nth seq (- (count seq) 2))))

(defcheck solution-a6561edf
  (fn [l]
    (loop [li (reverse l) n 0]
      (if (= n 1)
        (first li)
        (recur (rest li) (inc n))))))

(defcheck solution-a7dd4725
  (fn [x]
    (nth
      x
      (- (count x) 2)
      )
    ))

(defcheck solution-ab4d9936
  (fn [[x1 x2 & xs]] (if (empty? xs) x1 (recur (into [x2] xs)))))

(defcheck solution-ab9acfb9
  (fn [s] (nth (reverse s) 1)))

(defcheck solution-ad0443f3
  (fn [x] (last (butlast x))))

(defcheck solution-ad3f190d
  (fn dosia [xs] (if (empty? (rest (rest xs))) (first xs) (dosia (next xs)))))

(defcheck solution-ad8f8741
  (fn [coll]
    (-> coll drop-last last)))

(defcheck solution-adddf54f
  ; #(first (nthrest % (- (count %) 2)))
  ; (comp last butlast)
  (comp first rest reverse))

(defcheck solution-ae628757
  (fn [s] (last (butlast s))))

(defcheck solution-afddd87a
  (fn [s]
    (if (> (count s) 2)
      (recur (next s))
      (first s))))

(defcheck solution-afe8d627
  (fn
    [ary]
    (nth ary (- (count ary) 2))
    ))

(defcheck solution-afee1e3a
  (fn [s] (first (rest (reverse s)))))

(defcheck solution-b0e36a81
  (comp last drop-last))

(defcheck solution-b17a3e7b
  (fn lst [x] (last (butlast x))))

(defcheck solution-b1b320d7
  (fn [c]
    (->> c
      reverse
      (drop 1)
      (take 1)
      first)))

(defcheck solution-b22554f0
  (fn [[x y & xs]]
    (if (nil? xs)
      x
      (recur (cons y xs)))))

(defcheck solution-b2b9349c
  (fn [coll]
    (if (next (next coll))
      (recur (next coll))
      (first coll))))

(defcheck solution-b316d223
  #(nth %1 (- (count %1) 2)))

(defcheck solution-b32170c
  (fn [r](if (= 2 (count r)) (first r) (recur (rest r)))))

(defcheck solution-b322228f
  (fn [s & last2nd] (if (next s)
                      (recur (next s) (first s))
                      last2nd)))

(defcheck solution-b3960cea
  #(->> % reverse (drop 1) first))

(defcheck solution-b46d98f6
  (fn [xs] (if (empty? (rest (rest xs)))
             (first xs)
             (recur (rest xs)))))

(defcheck solution-b4c3190c
  #(-> %1 reverse rest first))

(defcheck solution-b56a7ae3
  #(peek (pop (vec %))))

(defcheck solution-b6b4061
  (fn [[pu l & more :as in]]
    (if more
      (recur (next in))
      pu)))

(defcheck solution-b7c705c9
  (fn [l] (nth l (- (count l) 2))))

(defcheck solution-b8d192a5
  (fn [sequence]
    (loop [s (rest  sequence), x (first sequence)]
      (cond (empty? (rest s)) x
            :else (recur (rest s) (first s))))))

(defcheck solution-b9c0dd9d
  #( fnext (reverse %)))

(defcheck solution-ba965194
  (fn [c] (second (reverse c))))

(defcheck solution-baa2e0d1
  (fn [l]
    (nth l (dec (dec (count l))))))

(defcheck solution-bb14916
  #(second(reverse %)))

(defcheck solution-bb666f86
  #(first (next (reverse %))))

(defcheck solution-bcbbe1d
  (fn [x]
    (second (reverse x))))

(defcheck solution-bdf065ad
  (fn [l] (let [[h th & t] l] (if (empty? t) h (recur (rest l))))))

(defcheck solution-be49ece1
  (fn [[x1 x2 & xs :as all]]
    (if (seq xs)
      (recur (rest all))
      x1)))

(defcheck solution-bf71e7c9
  (fn [l] (second (reverse l))))

(defcheck solution-c0733ec0
  (fn [x] (-> x
            (reverse)
            (nth 1))))

(defcheck solution-c087fbac
  (fn [l]
    (let [n (count l)]
      (if (< n 2)
        (do nil)
        (nth l (- n 2))
        ))))

(defcheck solution-c1c4067d
  (fn llast [x] (if (= 2 (count x)) (first x) (llast (rest x)))))

(defcheck solution-c27821a9
  (fn [a] (first (drop (- (count a) 2) a))))

(defcheck solution-c30e172f
  #(nth % (- (count %) 2)))

(defcheck solution-c33d37e7
  (fn [liste]
    (
      first (pop (apply list (reverse liste)))
      )
    ))

(defcheck solution-c3fb06e0
  (comp first next reverse))

(defcheck solution-c815dfcc
  (fn [x] (-> x reverse rest first)))

(defcheck solution-c93fe03c
  (comp first pop #(apply list %) reverse))

(defcheck solution-c9436f7c
  #(loop [l %]
     (if (= (count l) 2)
       (first l)
       (recur (rest l)))))

(defcheck solution-c9802972
  (fn [l]
    (loop [left l]
      (let [ [fst sec & res] left]
        (if (nil? res)
          fst
          (recur (next left) ))))))

(defcheck solution-ca431e21
  (fn [l] (nth l (- (count l) 2) )))

(defcheck solution-ca9c1929
  (fn [x] (nth x (- (count x ) 2))))

(defcheck solution-cad528d2
  (fn [seq]
    (loop [remain seq]
      (if (empty? (rest (rest remain)))
        (first remain)
        (recur (rest remain))))))

(defcheck solution-cb724c63
  #(if (empty? (rest (rest %)))
     (first %)
     (recur (rest %))))

(defcheck solution-cbddd26f
  (fn last-2 [s]
    (-> s reverse second)))

(defcheck solution-cc50dc6e
  (fn last_but_one [list]
    ((fn go [x y [z & ls :as l]] (if (empty? l) x (recur y z ls))) nil nil list)))

(defcheck solution-cd44f2ab
  (fn [seq] (nth (reverse seq) 1)))

(defcheck solution-cde946df
  #(->> % reverse second))

(defcheck solution-cf691a9c
  #(first (drop 1 (reverse %))))

(defcheck solution-d0d8c3d0
  #(nth (reverse %) 1))

(defcheck solution-d1b67c5c
  (fn [seq]
    (nth seq (- (count seq) 2))))

(defcheck solution-d1f25f81
  (fn [s] (nth s (- (count s) 2))))

(defcheck solution-d27b6518
  (fn [elems]
    (second (reverse elems))))

(defcheck solution-d2c3aeba
  (fn [v]
    (loop [v v]
      (if (= 2 (count v))
        (first v)
        (recur (rest v))))))

(defcheck solution-d3035f8e
  (fn [coll] (first (rest (reverse coll)))))

(defcheck solution-d4abf9af
  (fn [s] (nth s (- (count s) 2) s)))

(defcheck solution-d4fc3732
  #(last (pop (vec %))))

(defcheck solution-d5bdbade
  (fn penultimate [xs]
    (if (empty? (rest (rest xs)))
      (first xs)
      (penultimate (rest xs)))))

(defcheck solution-d604220a
  #(if (nnext %) (recur (rest %)) (first %)))

(defcheck solution-d6177c11
  (fn [target]
    ((fn [l s]
       (if (next l)
         (recur (next l) (first l))
         s))
     target "")))

(defcheck solution-d64fcb7e
  #(last (butlast %1)))

(defcheck solution-d69f2420
  (fn [list] (first (drop 1 (reverse list)))))

(defcheck solution-d79f7c2b
  (fn [x]
    (nth (reverse x) 1)
    ))

(defcheck solution-d83a3505
  (fn [l]
    (second (reverse l))))

(defcheck solution-d8907c2b
  (fn [coll]
    (let [[a b & more] coll]
      (if more
        (recur (next coll))
        a
        )
      )
    ))

(defcheck solution-d8d80360
  (fn pel ([lst] (if (= (count lst) 2) (first lst) (recur (rest lst))))))

(defcheck solution-d9a3128
  (fn [s]
    (if (nnext s)
      (recur (next s))
      (first s))))

(defcheck solution-da1043df
  #(last (take (- (count %) 1) %) ))

(defcheck solution-da6a7450
  (fn [[penultimate tail-item & tail :as init-coll]]
    (if (empty? tail)
      penultimate
      (recur (rest init-coll) ))))

(defcheck solution-da7a076
  (comp first (partial take-last 2)))

(defcheck solution-dad9518a
  (fn [s] (nth s (-> s count dec dec))))

(defcheck solution-db07c9a1
  (fn [xs]
    (if (next(next xs))
      (recur (next xs))
      (first xs))))

(defcheck solution-dbe0110d
  (fn [l]  (first (rest (reverse l)))))

(defcheck solution-dc87331d
  (fn last-x [x]
    (if (not-empty (rest (rest x)))
      (last-x (rest x))
      (first x))))

(defcheck solution-dcbb8a93
  #(if (empty? (rest(rest %))) (first %) (recur (rest %))))

(defcheck solution-dd0f5549
  (fn [s]
    (if
     (empty? (rest (rest s)))
      (first s)
      (recur (rest s))
      )
    ))

(defcheck solution-de5eeef9
  (fn f ([c]
         (cond
           (= (next (next c)) nil) (first c)
           :else (f (next c))
           )
         )
    ))

(defcheck solution-de91b5d4
  (fn [xs]
    (->> xs reverse (drop 1) (first))))

(defcheck solution-deb9c5ef
  (fn [coll]
    (if (empty? (rest (rest coll)))
      (first coll)
      (recur (rest coll)))))

(defcheck solution-dff54702
  (fn [lst](second (reverse lst))))

(defcheck solution-e02fa224
  #(first (pop (apply list (reverse %)))))

(defcheck solution-e0346f10
  #(first (rest (reverse %))))

(defcheck solution-e17f1f16
  (fn penultimate [x] (get (vec x) (- (count x) 2))))

(defcheck solution-e1ae0639
  #(if (< (count %) 3)
     (first %)
     (recur (rest %))))

(defcheck solution-e3f66158
  (fn [elements]
    (nth elements (- (count elements) 2))
    ))

(defcheck solution-e440e5b0
  (fn [x] (first (take 1 (take-last 2 x)))))

(defcheck solution-e4850c22
  (fn secondToLast [[h & t]] (if (= (count t) 1) h (secondToLast t))))

(defcheck solution-e4f43058
  (fn hey [x]
    (if
     (empty? (rest (rest x)))
      (first x)
      (hey (rest x)))))

(defcheck solution-e70cd396
  #_(blech fn [lst]
      (loop [f (first lst)
             l (rest lst)]
        (if (< 1 (count l))
          (recur (first l) (rest l))
          f)))

  (comp fnext reverse))

(defcheck solution-e74cfc77
  #(last (take 2 (reverse %))))

(defcheck solution-e81583f0
  (fn [x] (if (empty? (rest (rest x))) (first x) (recur (rest x)))))

(defcheck solution-e91f1b00
  #(nth % (-> % count dec dec)))

(defcheck solution-e9898
  (fn [x] (nth x (dec (dec (count x))))))

(defcheck solution-e9a8fbac
  (fn lastone [x] (nth x (- (count x) 2) )))

(defcheck solution-e9dae73a
  (fn [x]
    (let [c (count x)]
      (nth x (- c 2)))))

(defcheck solution-ea07fe5c
  (fn [coll]
    (when (seq coll)
      (if-not (nnext coll)
        (first coll)
        (recur (next coll))))))

(defcheck solution-eb52318f
  (fn l [[a & b]] (if (= 1 (count b)) a (l b))))

(defcheck solution-ebbab756
  (comp first (partial drop 1) reverse))

(defcheck solution-ebbe737f
  (fn penulti-ele [seq]
    (if (<= (count seq) 2)
      (first seq)
      (penulti-ele (rest seq)))))

(defcheck solution-ed95e62b
  (fn my-last [x]
    (if (empty? (rest (rest x)))
      (first x)
      (recur (rest x)))))

(defcheck solution-ee5571db
  (comp fnext reverse))

(defcheck solution-ef00056a
  (fn [[x y & xs :as coll]]
    (if (empty? xs) x (recur (rest coll)))))

(defcheck solution-ef12de5e
  (fn [xs]
    (nth xs (- (count xs) 2))))

(defcheck solution-f12dfba1
  (fn get-scd-last-elem
    [col]
    (if (= 2 (count col))
      (first col)
      (get-scd-last-elem (rest col)))))

(defcheck solution-f17ce45d
  #(first (drop (- (count %) 2) %)))

(defcheck solution-f1b06a1b
  #(loop [[first & rest] %
          acc nil]
     (if (empty? rest)
       acc
       (recur rest first)
       )
     ))

(defcheck solution-f2da4232
  (fn  [coll] (second (reduce conj ()  coll))))

(defcheck solution-f337e866
  (fn [[x & xs]] (if (empty? (rest xs)) x (recur xs))))

(defcheck solution-f44d173
  (fn [l] (nth (reverse l) 1)))

(defcheck solution-f4647518
  (fn [x] (if (= '() (rest (rest x))) (first x) (recur (rest x)))))

(defcheck solution-f5118599
  (fn [[x y & xs :as coll]] (if (seq xs) (recur (rest coll)) x)))

(defcheck solution-f51a921b
  (fn [s]
    (first (rest (reverse s)))))

(defcheck solution-f546567b
  (fn get_penultimate [l] (if (= [] (rest (rest l))) (first l) (get_penultimate (rest l)))))

(defcheck solution-f5fe4427
  (comp first rest reverse))

(defcheck solution-f73d66b
  ( comp second reverse))

(defcheck solution-f7adc5b2
  #(if (next (next %)) (recur (next %)) (first %)))

(defcheck solution-f7efd966
  (fn [l] (if (empty? (rest (rest l))) (first l) (recur (rest l)))))

(defcheck solution-f81506b0
  #(loop [col %,x nil]
     (if (empty? (rest col) )
       x
       (recur (rest col) (first col) )
       )
     ))

(defcheck solution-f856cdeb
  (fn [xs]
    (if (empty? (rest (rest xs))) (first xs) (recur (rest xs)))))

(defcheck solution-f9cda736
  (fn [xs]
    (first (take-last 2 xs))))

(defcheck solution-fa062f18
  (fn [x] (first (take-last 2 x))))

(defcheck solution-fad0cb95
  #(nth  % (- (count %)  2)))

(defcheck solution-fae63d6d
  (fn [coll]
    (nth coll (- (count coll) 2))))

(defcheck solution-fb32897c
  (fn [coll]
    (let [xs (seq coll)]
      (if (empty? (rest (rest xs)))
        (first xs)
        (recur (rest xs))))))

(defcheck solution-fb99eb33
  (fn my-last [x] (if (= (rest (rest x)) '()) (first x) (my-last (rest x)))))

(defcheck solution-fbafee89
  (fn [s]
    (if (empty? (rest (rest s)))
      (first s)
      (recur (rest s)))))

(defcheck solution-fc1962ef
  #(if (<= (count %) 2) (first %) (recur (rest %))))

(defcheck solution-fc8575c8
  (fn [lst] (second (reverse lst))))

(defcheck solution-fc87277a
  (fn penultimate [x]
    (if (= 2 (count x))
      (first x)
      (recur (rest x)))))

(defcheck solution-ff205264
  (fn [coll]
    (if (nnext coll)
      (recur (next coll))
      (first coll))))

(defcheck solution-ff80905a
  (fn [s0]
    (loop [s s0
           res nil]
      (let [f (first s)
            r (next s)]
        (if (nil? r)
          res
          (recur r f))))))