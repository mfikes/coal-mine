(ns coal-mine.problem-23
  (:require [coal-mine.checks :refer [defcheck-23] :rename {defcheck-23 defcheck}]
            [clojure.test]))

(defcheck solution-1066cbb8
  (fn this [s] (if (= 0 (count s)) '() (conj (this (butlast s)) (last s)))))

(defcheck solution-119916b7
  #(loop [orig % res []] (if (empty? orig) (seq res) (recur (butlast orig) (conj res (last orig))))))

(defcheck solution-11a6dca4
  #(reduce (fn [r i] (cons i r))
     [] %))

(defcheck solution-12a8dd6
  reduce (fn [xs y] (cons y xs)) '())

(defcheck solution-12ea7572
  (fn
    [l]
    (let
     [iterate
      (fn
        [lst accum]
        (if
         (empty? lst)
          accum
          (recur (rest lst) (conj accum (first lst)))
          )
        )
      ]
      (iterate l '())
      )
    ))

(defcheck solution-12fc26eb
  (fn [input]
    (loop [s (seq input) acc nil]
      (if (empty? s)
        acc
        (recur (rest s) (conj acc (first s)))))))

(defcheck solution-13b08c3b
  (fn [s]
    (letfn [(my-reverse [[s & ss]]
              (if (empty? ss)
                [s]
                (conj (my-reverse ss) s)))]
      (my-reverse (vec s)))))

(defcheck solution-13d87590
  (fn my-reverse [xs]
    (if (empty? xs)
      []
      (conj (my-reverse (rest xs))
        (first xs)))))

(defcheck solution-1405d149
  reduce (fn [a b] (cons b a)) [])

(defcheck solution-140f6c48
  (fn rev [s]
    (if (empty? s)
      []
      (concat (rev (rest s)) [(first s)])
      )
    ))

(defcheck solution-150e75f7
  (fn mojeReverse [ceho]
    ((fn ob [a b]
       (if (empty? b) a (ob (conj a (last b)) (butlast b))) ) [] ceho)))

(defcheck solution-1510b9ba
  (fn [v] (vec (reduce (fn [acc x] (conj acc x)) '() v))))

(defcheck solution-1513e65f
  (fn [coll]
    (loop [c coll
           rev '()]
      (if (seq c)
        (recur (rest c) (conj rev (first c)))
        rev))))

(defcheck solution-153fc4f8
  (fn [x] (reduce conj '() x)))

(defcheck solution-15816215
  (fn swapit [alist] (if (= (seq alist) nil) [] (conj (swapit (rest alist)) (first alist)))))

(defcheck solution-158f4cf1
  (fn [x]
    (loop [l x acc '()]
      (if (empty? l)
        acc
        (recur (rest l) (conj acc (first l)))
        )
      )
    ))

(defcheck solution-16215880
  (fn [coll]
    (if (empty? coll)
      coll
      (loop [x (first coll)
             sq (rest coll)
             acc (conj () x)]
        (if (empty? sq)
          acc
          (recur (first sq) (rest sq) (conj acc (first sq))))))))

(defcheck solution-162bc71f
  (fn [coll]
    (loop [lst coll, revColl ()]
      (if (empty? lst)
        revColl
        (recur (rest lst) (conj revColl (first lst) ))
        ))))

(defcheck solution-1650c72a
  #(loop [-seq % res '()]
     (if (empty? -seq)
       res
       (recur (rest -seq) (conj res (first -seq))))))

(defcheck solution-16a1f833
  reduce conj `())

(defcheck solution-16f83e0c
  (fn [col]
    (reduce conj () col)))

(defcheck solution-17c07ef7
  #((fn rew [x t] (if (empty? t) x (rew (cons (first t) (butlast x)) (rest t)))) % %))

(defcheck solution-1825d909
  #(reduce (fn [a x] (conj a x)) '() %))

(defcheck solution-18654166
  #(loop [lst %
          rlst []]
     (if (empty? lst)
       rlst
       (recur (drop-last lst) (conj rlst (last lst))))))

(defcheck solution-1958cfae
  (fn [x]
    (loop [in x, out ()]
      (if (empty? in)
        out
        (recur (rest in) (conj out (first in)))
        )
      )
    ))

(defcheck solution-19f0a0ea
  (fn [coll]
    (loop [coll coll result []]
      (if (empty? coll)
        result
        (recur (butlast coll) (conj result (last coll)))))))

(defcheck solution-1a9630a9
  (fn [sq] ((fn my-rev [s c] (if (= nil (first s))
                               (c nil)
                               (my-rev (rest s) #(cons (first s) (c %))))) sq identity)))

(defcheck solution-1c0eee
  (fn [s]
    ((fn f [s r]
       (if (empty? s)
         r
         (f (rest s) (cons (first s) r)))
       ) s [])
    ))

(defcheck solution-1c2a56cb
  (fn [s] (loop [s s acc ()] (if (empty? s) acc (recur (rest s) (cons (first s) acc))))))

(defcheck solution-1c58eeb0
  (fn [l] (reduce #(conj % %2) (list) l)))

(defcheck solution-1c6bc644
  #(loop [c % acc ()] (if (empty? c) acc (recur (rest c) (cons (first c) acc)))))

(defcheck solution-1c7d2f5a
  (fn hoge [lst]
    (if (empty? lst) nil
                     (let [dd (take (- (count lst) 1) lst)]
                       (cons (last lst) (hoge dd))))))

(defcheck solution-1d14420
  (fn r [x]
    (if (empty? x)
      x
      (cons (last x) (r (butlast x))))))

(defcheck solution-1d5951d4
  (fn [coll]
    (apply conj nil coll)))

(defcheck solution-1e57f164
  #(loop [list %1, res []] (if (seq list) (recur (next list) (cons (first list) res)) res)))

(defcheck solution-1e5a3cd9
  (partial reduce #(cons %2 %1) []))

(defcheck solution-1e74c4c9
  (fn [sq]
    (loop [orig sq
           res '()]
      (if (empty? orig)
        res
        (recur (rest orig) (cons (first orig) res))))))

(defcheck solution-1eadd65b
  #(loop[r (vec %)
         rtn []]
     (if(empty? r) rtn
                   (recur (pop r) (conj rtn (peek r))))
     ))

(defcheck solution-20456fd
  #(map (vec %) (range (dec (count %)) -1 -1)))

(defcheck solution-20483b37
  reduce #(cons %2 %) ())

(defcheck solution-2105f570
  (fn me [x] (if (empty? x) x (conj (me (butlast x)) (last x)) )))

(defcheck solution-213377d4
  (fn r [lst]
    (if (empty? lst)
      []
      (conj (r (rest lst)) (first lst)))))

(defcheck solution-21c1dc91
  (fn [se]
    (reduce conj '() se)))

(defcheck solution-21e03c54
  #((fn [x y] #_(println x y) (if (empty? x) y (recur (take (dec (count x)) x) (conj  y (last x))))) % []))

(defcheck solution-2296a4a4
  (fn [coll] (reduce (fn [acc e] (cons e acc)) (list) coll)))

(defcheck solution-229bc901
  (fn [l]
    (reduce conj '() l)
    ))

(defcheck solution-229dc03c
  (fn sol0023-reduce
    [coll]
    (reduce conj '() coll)))

(defcheck solution-22aa03a
  (fn [col]
    (loop [new []
           col col]
      (if (zero? (count col))
        new
        (recur (concat (vector (first col)) new) (rest col))))))

(defcheck solution-22f256c7
  #(map last (sort-by key > (zipmap (range) %))))

(defcheck solution-236367d
  reduce (fn [x y] (cons y x)) ())

(defcheck solution-2396425f
  (fn r [s]
    (if (empty? s)
      []
      (conj
        (r (rest s))
        (first s)))))

(defcheck solution-23db066
  reduce (fn [lst el] (conj lst el)) ())

(defcheck solution-2411f3b9
  (fn [seq]
    (loop [s seq
           o nil]
      (if (nil? s)
        o
        (recur (next s) (conj o (first s)))))))

(defcheck solution-246778cf
  (fn this
    ([xs] (this () xs))
    ([revd xs]
     (if (empty? xs)
       revd
       (recur (cons (first xs) revd) (rest xs))))))

(defcheck solution-247df89c
  apply conj ())

(defcheck solution-24aec2b5
  (fn f [xs]
    (if (empty? xs)
      nil
      (concat (f (rest xs)) (list (first xs))))))

(defcheck solution-24bffeda
  (fn [n & s]
    (if (empty? n)
      (vec s)
      (recur (rest n) (cons (first n) s)))))

(defcheck solution-24c1d27
  (fn [coll]
    (loop [coll2 coll, result []]
      (if (empty? coll2)
        result
        (recur (drop-last coll2) (conj result (last coll2)))))))

(defcheck solution-252894e0
  (fn [x]
    (loop [to-go x result '()]
      (if (empty? to-go)
        result
        (recur (rest to-go) (conj result (first to-go)))))))

(defcheck solution-2550b59d
  (fn [s] (->> (zipmap (iterate dec 0) s) (into (sorted-map)) (vals))))

(defcheck solution-26f78c7e
  (fn [coll]
    (loop [remaining coll
           reversed '()]
      (if (empty? remaining)
        reversed
        (recur (rest remaining) (cons (first remaining) reversed))))))

(defcheck solution-2743beb0
  ;(fn [c] (loop [i (- (count c) 1) o []]
  ;          (if (&lt; i 0)
  ;            o
  ;            (recur (dec i) (conj o (nth (seq c) i))))))

  apply conj ())

(defcheck solution-279e39d5
  (fn [coll] (reduce #(cons %2 %1) [] coll)))

(defcheck solution-27b7c0e9
  (fn myreverse [x]
    (if (empty? x)
      x
      (concat
       (myreverse (rest x))
       (list (first x))
       )
      )
    ))

(defcheck solution-27cf463b
  #(reduce conj nil %))

(defcheck solution-27f04aa4
  #(reduce (fn [acc a] (cons a acc)) '() %1))

(defcheck solution-28366230
  (fn seq-reverse [v]
    (if (empty? v)
      (empty v)
      (conj (seq-reverse (butlast v)) (last v)))))

(defcheck solution-296bb48d
  reduce #(cons %2 %1) '())

(defcheck solution-2a254cc6
  (fn [s] (let [aux (fn [acc s]
                      (if (empty? s) acc (recur (cons (first s) acc) (rest s))))]
            (aux nil s))))

(defcheck solution-2ac3fab0
  #(reduce conj '() (vec %)))

(defcheck solution-2b7256cc
  (fn [xs]
    (loop [acc ()
           xs xs]
      (if (empty? xs)
        acc
        (recur (conj acc (first xs))
          (rest xs))))))

(defcheck solution-2b7f4b0c
  #(reduce (fn[rev el](cons el rev)) '() %))

(defcheck solution-2e038f34
  #(vec (into (list) %)))

(defcheck solution-2f03cd5d
  (fn [s]
    (loop [head (first s) tail (rest s) values '()]
      (if (empty? tail)
        (conj values head)
        (recur (first tail) (rest tail) (conj values head))))))

(defcheck solution-2f37918
  (fn a [s]
    (if (not (seq s))
      nil
      (concat (a (rest s)) [(first s)]))))

(defcheck solution-2faa8407
  (fn
    [coll]
    (letfn [(rev
              [coll]
              (if (nil? coll)
                []
                (conj (rev (next coll)) (first coll))))]
      (rev coll))))

(defcheck solution-2fc0d45d
  (partial reduce (fn [s e] (conj s e)) ()))

(defcheck solution-30004b48
  (partial into ()))

(defcheck solution-30369af6
  (fn [s] (reduce #(conj %1 %2) '() s)))

(defcheck solution-306713c6
  (fn [elems]
    (reduce (fn [result e]
              (cons e result)) '() elems)))

(defcheck solution-30bc7250
  (fn [x]
    (loop [in x out nil]
      (if (empty? in) out (recur (rest in) (cons (first in) out))))))

(defcheck solution-316b7749
  (fn [coll]
    (reduce (fn [acc item]
              (cons item acc))
      nil
      coll)))

(defcheck solution-319b552
  (fn [s] (into () s)))

(defcheck solution-31b0a253
  #(loop [a '() s %]
     (if (empty? s) a
                    (recur (conj a (first s)) (rest s))
                    )))

(defcheck solution-3206ebbb
  #(loop [i %
          r []]
     (if (= 0 (count i))
       r
       (recur (butlast i) (conj r (last i))))
     ))

(defcheck solution-322ea995
  (fn [b] (sort-by (fn [a] (if (coll? a) (first a) a)) > b)))

(defcheck solution-328c1e5d
  #(reduce (fn[x,y](cons y x)) [] %))

(defcheck solution-33b69b15
  (fn [s] (map (vec s) (range (dec (count s)) -1 -1))))

(defcheck solution-33fb3009
  (fn rev [sq] (letfn [(revh [rv sq] (if (empty? sq) rv (revh (cons (first sq) rv) (rest sq))))] (revh (empty sq) sq))))

(defcheck solution-340187fb
  #(
     loop[lst % reversed '() ]
     (if(empty? lst)
       reversed
       (recur (rest lst)
         (cons (first lst)
           reversed)))))

(defcheck solution-343b8dc1
  (fn rev [x]
    (if (empty? x)
      x
      (cons
        (last x)
        (rev (butlast x))))))

(defcheck solution-34ead53
  reduce #(conj %1 %2) '())

(defcheck solution-3521b138
  (fn reverseN [s] (if (empty? s) s (concat (reverseN (rest s)) (list (first s))))))

(defcheck solution-356da18
  (partial reduce (fn [l e] (conj l e)) ()))

(defcheck solution-363a5460
  #(loop [a '() s %] (if (empty? s) a (recur (cons (first s) a ) (rest s)))))

(defcheck solution-36e0e45
  (fn [full-list]
    (loop [rv [] src full-list]
      (if (= src ())
        rv
        (recur (cons (first src) rv) (rest src))))))

(defcheck solution-38242108
  (fn myr [seq] (if (= seq '()) '() (concat (myr (rest seq)) [(first seq)]))))

(defcheck solution-38e8ff2f
  (fn [x]
    (loop [old x new '()]
      (if (empty? old)
        new
        (recur (rest old)
          (cons (first old)
            new))))))

(defcheck solution-39194e0f
  (partial (fn [rs is] (if (empty? is) rs (recur (conj rs (first is)) (rest is)))) ()))

(defcheck solution-39321abe
  (fn [l]
    (let [n (count l)]
      (let [rev (map #(- (- n 1) %) (range n))]
        (map #(nth (seq l) %) rev)))))

(defcheck solution-3a068928
  (fn [s]
    (loop [rv '() rs s]
      (if-not rs
        rv
        (recur (conj rv (first rs)) (next rs))))))

(defcheck solution-3a536b86
  #(loop [coll % n 0]
     (if (= n (count coll))
       coll
       (recur (concat
               (take n coll)
               (list (last coll))
               (take (- (count coll) (inc n)) (drop n coll))
               ) (inc n))
       )))

(defcheck solution-3ada2b3f
  (fn my-reverse
    [s]
    (loop [head s
           result '()]
      (if (empty? head)
        result
        (recur (rest head) (conj result (first head)))))))

(defcheck solution-3b71cc5b
  (fn [x]
    (loop [o '() i x]
      (if (empty? i) o
                     (recur (cons (first i) o) (drop 1 i) )
                     )
      )
    ))

(defcheck solution-3bef70b8
  (fn [coll]
    ((fn [in out]
       (if (empty? in)
         out
         (recur (rest in) (conj out (first in))))
       ) coll nil)))

(defcheck solution-3c20b9fe
  (fn hey ([x] (hey x [])) ([x y] (if (empty? x) y (concat (hey (rest x) y) (list (first x)))))))

(defcheck solution-3c303cad
  #(reduce conj  '() %))

(defcheck solution-3c596131
  (fn reorder [x]
    #_(println x)
    (if (= 1 (count x))
      x
      (conj (reorder (vec (rest x))) (first x)))
    ))

(defcheck solution-3d3e0165
  (fn reverse-seq [ls] (if (empty? (rest ls)) (vector (first ls)) (concat (reverse-seq (rest ls)) [(first ls)])) ))

(defcheck solution-3d441cb7
  (fn [coll]
    (loop [coll coll acc nil]
      (if (coll? coll)
        (recur (next coll) (cons (first coll) acc))
        acc))))

(defcheck solution-3f00106e
  (fn r [s]
    (if (empty? s) s
                   (cons
                     (last s)
                     (r (drop-last s))
                     )
                   )
    ))

(defcheck solution-3f2df4a5
  (fn recursive-reverse [coll]
    (if (= 2 (count coll))
      (vector (nth coll 1) (nth coll 0))
      (concat (recursive-reverse (rest (vec coll))) [(nth (vec coll) 0)]))))

(defcheck solution-3f9259e6
  (fn rev [xs]
    (when (not (empty? xs))
      (conj (rev (butlast xs)) (last xs)))))

(defcheck solution-3fd290c
  #(into '() (vec %)))

(defcheck solution-40091aef
  (fn problem23-reverse-seq [seq]
    (reduce conj '() seq)))

(defcheck solution-403cfef8
  (fn rev [x]
    (if (empty? x)
      x
      (concat (rev (rest x)) (list (first x)) )
      )))

(defcheck solution-406d107c
  (fn [s]
    (loop [orig s rev nil]
      (if (seq orig)
        (recur (rest orig) (conj rev (first orig)))
        rev))))

(defcheck solution-4072626d
  #(reduce (fn [xs x] (conj xs x)) '() %))

(defcheck solution-4087c016
  (fn [col] (reduce conj () col)))

(defcheck solution-408eb605
  (fn rev ([sq] (rev sq (empty sq)))
    ([old new] (if (empty? old)
                 new
                 (recur (rest old) (cons (first old) new))))))

(defcheck solution-414e5d0c
  #(vals (into (sorted-map-by >) (zipmap (range 0 (count %)) %))))

(defcheck solution-416ab734
  (fn rev
    ([items] (rev items []))
    ([items reversed]
     (if (empty? items)
       reversed
       (recur (rest items) (cons (first items) reversed))
       )
     )
    ))

(defcheck solution-4191401a
  (fn r [s]
    (if (empty? s)
      []
      (conj (r (rest s)) (first s)))))

(defcheck solution-41d590d3
  (fn [a]
    (reduce #(cons %2 %1) '() a)))

(defcheck solution-4280aed9
  reduce (fn [into-coll elem]
           (cons elem into-coll)) [])

(defcheck solution-42982533
  (fn [coll] (loop [a coll cur []] (if (empty? a) cur (recur (drop-last a) (conj cur (last a)))))))

(defcheck solution-42b1a341
  (fn r [els] (loop [x [] ys els] (if (empty? ys) x (recur (cons (first ys) x) (rest ys))))))

(defcheck solution-42ef1357
  (fn [s]
    (loop [acc [] r s]
      (if (empty? r)
        acc
        (recur (cons (first r) acc) (rest r))
        )
      )
    ))

(defcheck solution-42f7a9a5
  (fn [xs] (reduce #(cons %2 %1) () xs)))

(defcheck solution-430e8d8e
  #(
    (fn rev [src, dest]
      (if
       (= (first src) nil)
        dest
        (rev
          (rest src)
          (conj dest (first src))
          )
        )
      )
    % '()
    ))

(defcheck solution-43d4bdea
  (fn [a-seq]
    (reduce conj '() a-seq)))

(defcheck solution-44bda297
  #(loop [n (count %) ret [] res %]
     (if (zero? n)
       ret
       (recur (dec n) (conj ret (last res)) (take (- n 1) res)))
     ))

(defcheck solution-45b182a5
  (fn [col]
    (loop [tmpcol col newcol  []]
      (if (empty? tmpcol)
        newcol
        (recur  (butlast tmpcol) (conj  newcol (last tmpcol)))))))

(defcheck solution-46901888
  (fn [coll]
    ((fn [coll res]
       (if (nil? coll)
         res
         (recur (next coll)
           (cons (first coll) res))))
     coll nil)))

(defcheck solution-46eeaa18
  ;#(reduce (fn [n coll] (conj n coll)) '() %)

  #(reduce conj () %))

(defcheck solution-473fbab3
  #(reduce (fn [acc el] (conj acc el)) () %))

(defcheck solution-4793d863
  (fn [coll]
    (reduce (fn [r c] (cons c r))
      '() coll)))

(defcheck solution-4835335d
  (letfn [(r [s] (if (empty? s) [] (conj (r (rest s)) (first s))))] r))

(defcheck solution-4912c748
  (fn r[s]
    (into () s)))

(defcheck solution-4955fa0c
  (fn r
    ([a] (r a '()))
    ([a b]
     (if (empty? a) b
                    (let [h (first a) t (rest a)]
                      (recur t (cons h b)))))))

(defcheck solution-49976a6f
  (fn [coll] (reduce conj () (seq coll))))

(defcheck solution-49b6017b
  ;#(into () %)
  #(loop [result (empty %1)
          col %1 ]
     (if (empty? col)
       result
       (recur (concat (list (first col)) result) (rest col))
       )))

(defcheck solution-4b63fc21
  reduce (fn [result elt] (cons elt result)) ())

(defcheck solution-4ba3102b
  (partial (fn [acc x] (if (nil? x) acc (recur (conj acc (first x)) (next x)))) '()))

(defcheck solution-4c25a4cc
  #(apply conj '() %))

(defcheck solution-4cad2397
  #((fn river [x y] (if (first x) (river (rest x) (cons (first x) y)) y)) % []))

(defcheck solution-4d0a5858
  (fn [coll] (vec (into () coll))))

(defcheck solution-4dbf7a5c
  (fn [l]
    (reduce #(concat [%2] %1) [] l)))

(defcheck solution-4e96a514
  #(loop [result []
          seq %]
     (if (empty? seq)
       result
       (recur (cons (first seq) result)
         (rest seq)))))

(defcheck solution-4eec351
  #(reduce conj '() (seq %)))

(defcheck solution-4f3e540d
  (fn [sq] (loop [sq sq
                  return '()]
             #_(println "bar")
             (if (= 0 (count sq))

               return
               (do
                 #_(println "foo")
                 (recur (rest sq)
                   (conj return (first sq))))))))

(defcheck solution-4f97a4e8
  (fn [s] (reduce (fn [a b] (cons b a)) (empty s) s)))

(defcheck solution-5034bf7e
  (fn rev[x]
    (if
     (empty? (rest x))
      (vector (first x))
      (conj (rev (rest x)) (first x))
      )))

(defcheck solution-50cd6e4f
  (fn [icol]
    (loop [col   icol
           acc   []]
      (if (empty? col) acc
                       (recur (rest col) (cons (first col) acc))))))

(defcheck solution-50fc1dd4
  (fn [coll]
    (loop [result nil coll coll]
      (if-let [n (seq coll)]
        (recur (cons (first n) result) (rest coll))
        result))))

(defcheck solution-51240e27
  #(reduce conj () %))

(defcheck solution-51777995
  (fn [x]
    (let [revable (into [] x)
          cx (count x)
          ln (dec cx)]
      (map #(nth revable (- ln %)) (range cx)))))

(defcheck solution-519969eb
  (fn [s]
    (loop [s- s r '()]
      (if (empty? s-)
        r
        (recur (rest s-) (conj r (first s-)))))))

(defcheck solution-527b60ef
  #(reduce (fn [acc x] (cons x acc)) (empty %1) %1))

(defcheck solution-52b78b70
  (fn [l]
    (loop [li l nl []]
      (if (= li [])
        nl
        (recur (rest li) (cons (first li) nl))))))

(defcheck solution-53210baa
  #(reduce (fn [result input] (conj result input)) () %))

(defcheck solution-535b1bc1
  (fn [l]
    (map #(nth (seq l) (- (count l) % 1))(range (count l)))))

(defcheck solution-535fb5b4
  (fn [sq]
    ((fn [sq r]
       (if (empty? sq)
         r
         (recur (rest sq)
           (conj r (first sq)))))
     sq '())))

(defcheck solution-53854da
  (fn [x] ( #(if (empty? %1) %2 (recur (rest %1) (cons (first %1) %2) )) x '()) ))

(defcheck solution-54489e4c
  (fn [lst]
    (reduce (fn [val col] (cons col val)) nil lst)))

(defcheck solution-54cb1ac4
  (fn [c] (for [i (range (dec (count c)) -1 -1)] (nth (seq c) i))))

(defcheck solution-54f07e8f
  (fn [s]
    (reduce #(cons %2 %1) [] s)
    ))

(defcheck solution-5502d3cd
  (fn  my-rev [sequence]
    (loop [s (seq sequence), ns (list)]
      (cond (empty? s) ns
            :else (recur (rest s) (conj ns (first s)))))))

(defcheck solution-55d8cdac
  #(reduce (fn [acc x] (cons x acc)) (empty %) %))

(defcheck solution-560fc55c
  reduce (fn [t v] (cons v t)) '())

(defcheck solution-56459e4
  reduce (fn [t h] (concat (list h) t)) ())

(defcheck solution-56df6995
  (fn [coll] (reduce conj '() coll)))

(defcheck solution-56ea8741
  (fn [c] (reduce conj '() c)))

(defcheck solution-5751f08a
  (partial reduce conj (list)))

(defcheck solution-577970e
  (fn [coll]
    (loop [current coll reversed []]
      (if (empty? current)
        reversed
        (recur (take (- (count current) 1) current)
          (conj reversed (last current)))))))

(defcheck solution-5783c6c2
  (fn [zs] ((fn rev [xs ys] (if (empty? xs) ys (rev (rest xs) (conj ys (first xs))))) zs '())))

(defcheck solution-57a91ee
  (fn my-reverse
    ([x] (my-reverse x []))
    ([x y] (if (empty? x) y (my-reverse (drop-last x) (conj y (last x)))))))

(defcheck solution-57de913c
  (partial reduce (fn [reversed item] (cons item reversed)) nil))

(defcheck solution-5857af6
  (fn [values]
    (loop [values values result []]
      (if (empty? values)
        result
        (recur (butlast values) (conj result (last values)))))))

(defcheck solution-58799dcf
  (fn [coll]
    (reduce (fn [c e] (cons e c))
      (empty coll)
      coll)))

(defcheck solution-58e47fa3
  #(loop
    [x (empty %)
     y %]
     (cond (empty? y) (cond (or (list? %) (set? %)) x :else (into (empty %) x))
           :else (recur (cons (first y) x) (rest y)))))

(defcheck solution-5908cf3
  #(loop [l % nl '()]
     #_(println l)
     #_(println nl)
     (if (not-empty l)
       (recur (rest l) (conj nl (first l)))
       nl)))

(defcheck solution-597473a5
  (fn x [coll]
    (if (empty? coll) '()
                      (cons (last coll) (x (butlast coll))))))

(defcheck solution-5976afa
  #((fn [l ll] (if (empty? l) ll (recur (rest l) (cons (first l) ll)))) % '()))

(defcheck solution-5995d027
  (fn [coll]
    (loop [coll coll rev-coll []]
      (if (empty? coll)
        rev-coll
        (recur (rest coll) (cons (first coll) rev-coll))))))

(defcheck solution-59c9eafc
  #(reduce (fn [output input-item] (cons input-item output)) '() %))

(defcheck solution-5acdde18
  (fn rx [coll] (letfn [(revfn [acc c] (if (empty? c) acc (revfn (cons (first c) acc) (rest c))))] (revfn (if (vector? coll) [] '()) coll))))

(defcheck solution-5aeeadd
  #(map last (take (count %) (iterate butlast %))))

(defcheck solution-5b14664
  (fn my-reverse
    ([l]
     (my-reverse l '()))
    ([l resp]
     (if (= l '())
       resp
       (my-reverse (rest l) (conj resp (first l)))))))

(defcheck solution-5c06293d
  (fn reverte [s]
    (let [v (vec s)
          qtd (count s)]
      (if (= 1 qtd)
        s
        (cons (last v) (reverte (subvec v 0 (dec qtd))))))))

(defcheck solution-5c959f76
  #(loop [in % out []]
     (if (empty? in)
       out
       (recur (rest in) (concat [(first in)] out)))))

(defcheck solution-5d8dd2b6
  (fn reverce-sq [x]
    (reduce (fn [c x] (cons x c)) (empty x) x)))

(defcheck solution-5dced33a
  (fn [l] (map #(nth (seq l) %) (sort > (range 0 (count l))))))

(defcheck solution-5f3ffed4
  #(loop [r `() coll %] (if (empty? coll) r (recur (conj r (first coll) ) (rest coll)))))

(defcheck solution-5f83f345
  #(loop [col % rcol '()]
     (if (empty? col)
       rcol
       (recur (rest col) (conj rcol (first col))))))

(defcheck solution-5faf70a8
  (fn [xs]
    (reduce (fn [reversed x]
              (cons x reversed))
      []  xs)))

(defcheck solution-5fe0dd89
  #(reduce (fn [l v] (conj l v)) '() %))

(defcheck solution-60a47c4e
  (fn [x] (reduce #(cons %2 %) [] x)))

(defcheck solution-615d4380
  (fn r [h] (if (next h) (conj (r (next h)) (first h)) [(first h)])))

(defcheck solution-61b22533
  #(loop [coll % v ()]
     (if coll (recur (next coll) (conj v (first coll)))
              v)))

(defcheck solution-61c7b646
  #(reduce (fn [x y] (conj x y)) '() %))

(defcheck solution-61dda4a4
  apply (partial conj '()))

(defcheck solution-62228572
  #(reduce conj '() %))

(defcheck solution-623b606
  (fn [x]
    (loop [forward x
           result []]
      (if (empty? forward)
        result
        (recur (rest forward) (cons (first forward) result))))))

(defcheck solution-63530615
  #(loop [a %1, r '()]
     (if (= 0 (count a))
       r
       (recur (rest a) (conj r (first a))))))

(defcheck solution-63a183b
  #(loop [remaining-seq % reversed-seq '()]
     (if (empty? remaining-seq) reversed-seq
                                (recur (rest remaining-seq)
                                  (cons (first remaining-seq) reversed-seq)))))

(defcheck solution-63da3011
  #(letfn [(worker [x n]
             (if (= x ())
               n
               (recur (rest x) (conj n (first x)))))]
     (worker % ())))

(defcheck solution-63f51977
  sort #(compare %2 %1))

(defcheck solution-64e48d82
  (fn [s] (reduce conj () s)))

(defcheck solution-64f3c43c
  (fn my-reverse

    ([a-list]

     (cond (= a-list nil) nil

           :else (cons (last a-list)

                   (my-reverse (butlast a-list)))))))

(defcheck solution-656ade1
  (fn [s]
    (loop [idx (dec (count s))
           ret []]
      (if (not (neg? idx))
        (recur (dec idx) (conj ret (nth (vec s) idx)))
        ret))))

(defcheck solution-6581edf4
  (fn [coll]
    (reduce conj '() coll)))

(defcheck solution-65e8081a
  (fn [xs]
    (loop [xs xs, acc '()]
      (if (empty? xs)
        acc
        (recur (rest xs) (cons (first xs) acc))))))

(defcheck solution-66f09ca1
  (fn [sq]
    (if (vector? sq)
      (loop [ls sq
             return []]
        (if (nil? (first ls))
          return
          (recur (pop ls)
            (conj return (peek ls)))))
      (loop [ls sq
             return '()]
        (if (nil? (first ls))
          return
          (recur (rest ls)
            (conj return (first ls))))))))

(defcheck solution-66f27529
  (fn my-reverse
    ([coll]
     (my-reverse coll '()))
    ([coll ret-coll]
     (if (nil? (seq coll))
       ret-coll
       (let [new-coll (conj ret-coll (first coll))]
         (my-reverse (rest coll) new-coll))))))

(defcheck solution-67d18cab
  (fn myrev [x] (if (> (count x) 1) (conj (myrev (butlast x)) (last x)) x)))

(defcheck solution-68556785
  (fn [l] (reduce (fn [x y] (conj x y)) (take 1 l) (rest l))))

(defcheck solution-68a23071
  (fn [coll] (reduce conj () coll)))

(defcheck solution-68dfd2
  (fn
    [list]
    (loop [li list retli []]
      (if (empty? li)
        retli
        (recur (rest li) (cons (first li) retli))))))

(defcheck solution-6933c694
  (fn reverse-seq [coll]
    (when-let [elem (first coll)]
      (concat (reverse-seq (rest coll)) [elem]))))

(defcheck solution-69d33196
  #(loop [r '()
          c %]
     (if (empty? c)
       r
       (recur (conj r (first c)) (rest c)))))

(defcheck solution-6a0ce902
  (fn my-reverse [coll]
    (let [a-seq (seq coll) size (count a-seq)]
      (for [i (range size)]
        (nth a-seq (dec (- size i))))
      )
    ))

(defcheck solution-6a34586a
  #(for [i (range (count %))] ((vec %) (- (count %) i 1))))

(defcheck solution-6b65834c
  (fn [seq]
    (loop [remain seq ans '()]
      (if (empty? (rest remain))
        (conj ans (first remain))
        (recur (rest remain) (conj ans (first remain)))
        )
      )
    ))

(defcheck solution-6b6946d7
  (fn rev [s]
    (loop [s s result nil]
      (if (empty? s)
        result
        (recur (rest s) (conj result (first s)))))))

(defcheck solution-6b8691
  reduce (fn addlist[xs x] (cons x xs)) [])

(defcheck solution-6b8adb94
  (fn my-reverse [lista]
    (if (empty? lista)
      lista
      (concat
       (my-reverse (rest lista))
       (list(first lista))))))

(defcheck solution-6ba20bad
  #(reduce (fn [a b] (cons b a)) [] %))

(defcheck solution-6bbd87b3
  (fn [s]
    (let [v (vec s)]
      (map-indexed (fn [index val] (nth v (- (count v) index 1))) v))))

(defcheck solution-6bda9800
  #(loop [lst %, acc (empty %)]
     (if (empty? lst) acc
                      (recur (rest lst) (cons (first lst) acc)))))

(defcheck solution-6bf80bab
  reduce (fn [c v] (cons v c)) '())

(defcheck solution-6c3d8bcf
  (fn retrograde [xs]
    ((fn impl [xs acc]
       (if (empty? xs)
         acc
         (impl (rest xs) (cons (first xs) acc))))
     xs '())))

(defcheck solution-6dcebe88
  (fn [s] (reduce #(cons %2 %1) '() s)))

(defcheck solution-6ddfab40
  (fn [l]
    (loop [orig l copy nil]
      (if (empty? orig)
        copy
        (recur (rest orig) (conj copy (first orig)))))))

(defcheck solution-6df6a884
  (fn [l] (reduce #(cons %2 %1) '() l)))

(defcheck solution-6ef83cc0
  (fn f ([x] (f x '()))
    ([x r] (if (empty? x)
             r
             (recur (rest x) (conj r (first x)))))))

(defcheck solution-6f32cca0
  (fn [seq]
    (loop [seq seq acc '()]
      (if (empty? seq)
        acc
        (recur (rest seq) (cons (first seq) acc))))))

(defcheck solution-6f8ecb4d
  (fn [l]
    (apply conj (list (first l)) (rest l))))

(defcheck solution-6fbf67dd
  (fn rev [coll]
    (if (empty? coll)
      []
      (conj (rev (rest coll)) (first coll)))))

(defcheck solution-6ff1a9a0
  (fn [coll]
    (reduce conj () coll)))

(defcheck solution-7024b5e2
  (fn [xs]
    (reduce conj nil xs)))

(defcheck solution-70e83510
  (fn [x]
    (loop [l x, v []]
      (if (empty? l)
        v
        (recur (butlast l) (conj v (last l)))))))

(defcheck solution-70f1ad33
  (fn [coll]
    "Write a function which reverses a sequence."
    (loop [coll coll
           acc (list)]
      (if (empty? coll)
        acc
        (recur (rest coll) (conj acc (first coll)))))))

(defcheck solution-71732164
  (fn [s]
    (reduce #(conj %1 %2) '() s)))

(defcheck solution-723ac7b2
  (fn [x] (loop [result () lst x]
            (if (empty? lst)
              result
              (recur (cons (first lst) result) (rest lst))))))

(defcheck solution-725b21d5
  (fn [s] (reduce conj '() s)))

(defcheck solution-72d55018
  (fn peu [x] (if (empty? x) x (conj (peu (butlast x)) (last x)))))

(defcheck solution-734f70ae
  (fn rev
    ([s] (rev s '()))
    ([s l]
     (if (empty? s)
       l
       (rev (rest s) (conj l (first s)))))))

(defcheck solution-738fb529
  (fn [xs](reduce conj '() xs)))

(defcheck solution-73ad6de1
  (fn [coll]
    (loop [result '() input coll]
      (if (nil? (seq input))
        result
        (recur (cons (first input) result) (rest input))
        )
      )
    ))

(defcheck solution-75178239
  (fn [lst]
    (loop [l1 lst l2 []]
      (if (empty? l1)
        l2
        (recur (rest l1) (concat [(first l1)] l2))
        ))))

(defcheck solution-756eecc
  (fn [l]
    (loop [mylist l res '()]
      (if (= mylist '())
        res
        (recur (rest mylist) (conj res (first mylist)))))))

(defcheck solution-7692549a
  (fn [l] (reduce (fn [acc v] (cons v acc)) '() l)))

(defcheck solution-77faed80
  (fn f [c]
    (if (seq c)
      (cons (last c) (f (butlast c))))))

(defcheck solution-78333248
  (fn rv [s] (if (next s) (conj (rv (next s)) (first s)) [(first s)])))

(defcheck solution-78798422
  (fn r [c] (if (seq c) (concat (r (rest c)) [(first c)]) [])))

(defcheck solution-78b44e68
  #(reduce (fn [a b] (concat b a)) (map vector %)))

(defcheck solution-78dd7bdf
  #(reduce (fn [x y] (concat [y] x)) [] %))

(defcheck solution-7948f07a
  (fn
    [s]
    (loop
     [
      result []
      s s]
      (if (empty? s)
        result
        (recur (conj result (last s)) (butlast s))))))

(defcheck solution-795e6b55
  (partial reduce conj ()))

(defcheck solution-79a3f2c2
  #(reduce (fn [r x] (cons x r)) (empty %) %))

(defcheck solution-79aacd56
  #(loop [ls % reversed '()] (if (empty? ls) reversed (recur (rest ls) (conj reversed (first ls))))))

(defcheck solution-79cda6c4
  #(loop [c %1, r (empty %1)]
     (if (empty? c) r
                    (recur (rest c) (concat (list (first c)) r)))))

(defcheck solution-79db3146
  (fn [s]
    (loop [rs (seq s)
           acc '()]
      (if (empty? rs)
        acc
        (recur (rest rs) (conj acc (first rs)))
        ))
    ))

(defcheck solution-7bba8b5c
  reduce (fn [new-c n] (cons n new-c)) [])

(defcheck solution-7c3e34ae
  (fn [coll]
    (loop [result '() c coll]
      (if (seq c) (recur (cons (first c) result) (rest c)) result))))

(defcheck solution-7cbb7c7d
  #(reduce (fn [acc e] (cons e acc)) (empty %) %))

(defcheck solution-7d54942c
  (fn f [v]
    (loop [v v
           acc []]
      (if (empty? v)
        acc
        (recur (rest v) (cons (first v) acc))))))

(defcheck solution-7d561b4
  (fn [s0]
    (loop [s s0
           res nil]
      (if (nil? s)
        res
        (let [f (first s)
              r (next s)]
          (recur r (conj res f)))))))

(defcheck solution-7d82d916
  (fn [xs] (reduce #(conj %1 %2) '() xs)))

(defcheck solution-7d884ce0
  #(let [helper (fn [acc s]
                  (if (empty? s)
                    acc
                    (recur (cons (first s) acc) (rest s))))]
     (helper '() %)))

(defcheck solution-7d8c40b8
  (fn [coll]
    (let [f (fn [lst rev]
              (if (empty? lst)
                rev
                (recur (rest lst) (conj rev (first lst)))))]
      (f coll '()))))

(defcheck solution-7d90138f
  #(reduce (fn [acc i] (cons i acc)) '() %))

(defcheck solution-7e41347c
  (fn [s]
    (reduce conj '() s)))

(defcheck solution-7e4b5eed
  (fn rev [a] (if (last a) (cons (last a) (rev (butlast a) ) ) ) ))

(defcheck solution-7e8f6965
  #(loop [s %
          out nil]
     (if (empty? s) out
                    (recur (rest s)
                      (cons (first s) out)))))

(defcheck solution-7f409cad
  (fn [coll] (reduce #(cons %2 %1) '() coll)))

(defcheck solution-7f998840
  (partial reduce #(conj % %2) '()))

(defcheck solution-7fbe49f7
  (fn rev [l] (if (empty? l) [] (conj (rev (rest l)) (first l)))))

(defcheck solution-7fc95001
  #(loop [result '() coll %]
     (if (empty? coll)
       result
       (recur (conj result (first coll)) (rest coll))
       )))

(defcheck solution-7ff85f96
  (fn [s] (loop [x s, y '()]
            (if  (empty? x)
              y
              (recur (rest x) (conj y (first x)))))))

(defcheck solution-82f3d235
  (fn myreverse
    [myseq]
    (loop
     [myrest myseq res nil]
      (if (nil? (first myrest))
        res

        (recur (rest myrest) (cons (first myrest) res))

        )
      )))

(defcheck solution-8320f7cc
  (fn [xs] (reduce #(cons %2 %1) nil (seq xs))))

(defcheck solution-850186be
  (fn rev [l]
    (if (empty? l) '()
                   (cons (last l) (rev (butlast l)))
                   )))

(defcheck solution-85b3c6d8
  (fn my-reverse [lst]
    (if (empty? lst)
      []
      (conj (my-reverse (rest lst)) (first lst)))))

(defcheck solution-86189e16
  (fn f [x] (if (= x []) [] (conj (f (rest x)) (first x)))))

(defcheck solution-86895ea6
  into ())

(defcheck solution-88c4feb
  (fn my-reverse
    [args]
    (if (= 1 (count args) )
      (vector (first args))
      (conj (my-reverse (rest args)) (first args))
      )
    ))

(defcheck solution-88edd69d
  (fn rev-seq [lat]
    (into '() lat)))

(defcheck solution-8949c8b1
  (fn [lat] (into '() lat)))

(defcheck solution-8990d135
  (fn new-reverse [coll]
    (loop [c coll acc ()]
      (cond
        (empty? c) acc
        :else (recur (rest c) (cons (first c) acc))))))

(defcheck solution-8a2138d1
  (fn my-reverse [xs]
    (loop [in-strm (seq xs) out-strm ()]
      (if (= in-strm nil) out-strm (recur (next in-strm) (cons (first in-strm) out-strm))))))

(defcheck solution-8a670bab
  (fn [c] (reduce #(cons %2 %1) '() c)))

(defcheck solution-8a8ee08a
  #(reduce (fn [base elem] (conj base elem)) nil %1))

(defcheck solution-8b1c8cbd
  (fn rev
    [s]
    (when (seq s)
      (cons (last s) (rev (butlast s))))))

(defcheck solution-8c34b938
  (fn [s] (into '() s)))

(defcheck solution-8d678d4c
  (partial reduce (fn [acc c] (conj acc c)) nil))

(defcheck solution-8d7e12e2
  reduce #(conj %1 %2) ())

(defcheck solution-8e0cf51b
  apply conj '())

(defcheck solution-8e1e22d6
  (fn r [lst]
    (apply conj '() lst)))

(defcheck solution-8ec0d5b9
  (fn [y]
    (loop [s y s1 []]
      (if (empty? s)
        s1
        (recur (rest s) (cons (first s) s1
                          )
          )
        )
      )))

(defcheck solution-8ff8547f
  (fn [input]
    (reduce conj '() (seq input))))

(defcheck solution-904ad2fe
  (fn [seq]
    (loop [s seq accu []]
      (if (empty? s) accu
                     (recur (rest s) (cons (first s) accu))))))

(defcheck solution-90d3ad94
  (fn [lst]
    (loop [lst lst rlst '()]
      (if (empty? lst)
        rlst
        (recur (rest lst) (conj rlst (first lst)))))))

(defcheck solution-9163272
  #(apply conj () %))

(defcheck solution-9177129a
  (fn [s]
    ((if (vector? s) (partial apply vector) identity)
     (loop [l s r '()]
       (if (empty? l) r
                      (recur (rest l) (cons (first l) r)))))))

(defcheck solution-923622d0
  (fn [xs] (reduce conj '() xs)))

(defcheck solution-923f37f9
  (fn [s]
    ((fn acc [s t]
       (if (empty? s)
         t
         (acc (rest s) (conj t (first s))))) s '())))

(defcheck solution-92a726cd
  (fn foo [s] (let [f (fn f [r s] (if (empty? s) r (f (conj r (first s)) (rest s))))] (f '() s))))

(defcheck solution-92be85dc
  (fn [input]
    (loop [result [] elements input]
      (if (empty? elements)
        result
        (recur (cons (first elements) result) (rest elements)))
      )
    ))

(defcheck solution-92c8e925
  ;CHEATING
  ;(partial sort (comp -  compare))

  ;Real: using reverse adding in a list
  into ())

(defcheck solution-92d05d78
  #(reduce (fn [x y] (concat (list y) x ) ) () % ))

(defcheck solution-9337a976
  (fn r[seqq] (into '() seqq)))

(defcheck solution-936669ac
  (fn rev [s] (if (empty? s)
                []
                (conj (rev (rest s))
                  (first s)))))

(defcheck solution-93baaf62
  #(reduce (fn [r x] (cons x r)) [] %))

(defcheck solution-93c5c0f7
  (fn my-reverse [x]
    (loop [result '()
           lst x]
      (if (empty? lst)
        result
        (recur (conj result (first lst)) (rest lst))))))

(defcheck solution-946db79b
  (fn [s]
    (reduce conj () s)))

(defcheck solution-94909281
  (fn r [s]
    (if (empty? s)
      s
      (if (list? s)
        (conj (r (drop-last s)) (last s))
        (conj (r (vec (rest s))) (first s))))))

(defcheck solution-94b871b6
  reduce conj '())

(defcheck solution-9541cfd2
  #(loop [acc []
          val %]
     (if(empty? val)
       acc
       (recur (conj acc (last val)) (butlast val)))))

(defcheck solution-9590b010
  (letfn [(R [coll]
            (if (empty? coll) []
                              (conj (R (rest coll)) (first coll))))]
    R))

(defcheck solution-95960c0e
  (fn [x] (into () x)))

(defcheck solution-95d8fb1c
  (fn [seq]
    (reduce conj '() seq)))

(defcheck solution-97791673
  (fn [xs] (into () xs)))

(defcheck solution-97ed1f37
  #(loop [x (seq %) r '()] (if (seq x) (recur (rest x) (conj r (first x))) r)))

(defcheck solution-981b5108
  reduce (fn [l v] (cons v l)) [])

(defcheck solution-986e3ba8
  (fn [s]
    (letfn [(f [s n]
              (if (= n 0)
                (first s)
                (f (rest s) (dec n))))]
      (map #(f s %)
        (range (- (count s) 1) -1 -1)))))

(defcheck solution-98b45004
  apply (partial conj ()))

(defcheck solution-99169e09
  #(reduce conj '() %1))

(defcheck solution-99c7d9f5
  #(reduce (fn[x y] (conj x y)) '() %))

(defcheck solution-9a1af4b0
  (fn rev
    ([in]
     (rev in nil))
    ([in out]
     (let [h (first in)
           t (rest in)]
       (if (nil? h)
         out
         (rev t (cons h out)))))))

(defcheck solution-9a5e4bfd
  (fn [c]
    (loop [c c acc ()]
      (if (seq c)
        (recur (next c) (conj acc (first c)))
        acc))))

(defcheck solution-9a8cde51
  (fn rev [lst]
    (loop [A lst, result nil]
      (if (empty? A)
        result
        (recur (rest A) (cons (first A) result))))))

(defcheck solution-9b99b6c6
  (fn myf [x] (if (first x) (conj (myf (rest x)) (first x) ) [])))

(defcheck solution-9bb478da
  #(loop [a (apply list %) b '()] (if (= a '()) b (recur (rest a) (conj b (first a))))))

(defcheck solution-9c1290b9
  (fn [x]
    (loop [s x result (seq '())]
      (if (= nil (first s))
        result
        (recur (rest s) (conj result (first s)))))))

(defcheck solution-9c3f186e
  (fn my-reverse [s]
    (if (empty? s) s
                   (cons (last s) (my-reverse (butlast s))))))

(defcheck solution-9c51099d
  (fn revit [sequence]
    ((fn rev[in out]
       (if (seq in)
         (rev (next in) (conj out (first in)))
         out)) sequence '())))

(defcheck solution-9c53b7f3
  (fn f [xs]
    (map
      (fn [y]
        (
         (fn enth [z n]
           (second
             (first
               (filter
                 (fn [a]
                   (=
                     (first a
                       )
                     n
                     )
                   )
                 (map vector
                   (range
                     (count xs
                       )
                     )
                   xs
                   )
                 )
               )
             )
           )
         xs
         (-
          (-
           (count xs)
           1
           )
          y
          )
         )
        )
      (range (count xs))

      )
    ))

(defcheck solution-9ca3eab0
  (fn myReverse [x] (if (seq x) (conj (myReverse (drop 1 x)) (first x)) [])))

(defcheck solution-9d6dc5fd
  (fn [x] (map #(nth % 1) (sort (map-indexed #(vector (- 0 %) %2) x)))))

(defcheck solution-9d8f297b
  (fn [l]
    (let [f
          (fn [l acc]
            (if (empty? l) acc
                           (recur (rest l) (conj acc (first l)))))] (f l '()))))

(defcheck solution-9d99c643
  (fn rev [xs]
    (if (empty? xs)
      ()
      (concat (rev (rest xs))
              (list (first xs))))))

(defcheck solution-9e5b3173
  #(loop [s %, result []]
     (if (empty? s)
       result
       (recur (rest s) (into [(first s)] result)))))

(defcheck solution-9f36195c
  (fn [s]
    (reduce (fn [acc x] (conj acc x)) '() s)))

(defcheck solution-9f39c0aa
  (fn myrev [coll]
    (if (= 1 (count coll))
      [(first coll)]
      (conj (myrev (rest coll))
        (first coll)))))

(defcheck solution-9f64b120
  #((fn rev [x, acc]
      (if (empty? x) acc (rev (rest x) (conj acc (first x)))))
    %
    ()
    ))

(defcheck solution-9f9b2bad
  (fn [s]
    (let [as-vec (vec s)]
      (map #(nth as-vec %)
        (take (count as-vec)
          (iterate dec (dec (count as-vec))))))))

(defcheck solution-a0a7a240
  (fn rev
    ([s] (rev '() s))
    ([r s] (if (seq s) (rev (cons (first s) r) (rest s)) r )) ))

(defcheck solution-a16d4ce2
  (fn [lst] (reduce (fn [prev new] (conj prev new)) '() lst)))

(defcheck solution-a21165dd
  #(loop [inp % rev []]
     (let [last-elem (last inp)]
       (if (= nil last-elem)
         rev
         (recur (drop-last 1 inp) (conj rev last-elem))))))

(defcheck solution-a26a789c
  reduce (fn [S i] (concat (list i) S)) '())

(defcheck solution-a2e76a3c
  (fn [coll]
    (loop [result () c coll]
      (if (nil? c)
        result
        (recur (conj result (first c)) (next c))))))

(defcheck solution-a30156ec
  #(reduce (fn [r c] (conj r c)) '() %1))

(defcheck solution-a3130917
  (fn [seq]
    (loop [s seq n ()]
      (if (nil? s)
        n
        (recur (next s) (conj n (first s)))))))

(defcheck solution-a319bd58
  (fn [arg]
    (loop [s arg result '()]
      (if s (recur (next s) (conj result (first s))) result))))

(defcheck solution-a3443938
  reduce (fn [a b] (concat [b] a)) [])

(defcheck solution-a3888e00
  (fn my-reverse [seq]
    (case (count seq)
      0 (empty seq)
      1 (into (empty seq) [(first seq)])
      (let [beginning (take (dec (count seq)) seq)
            reversed-beginning (my-reverse beginning)
            result (cons (last seq) reversed-beginning)]
        #_(println seq ":" (last seq) "+ rev" beginning "->" (class reversed-beginning))
        result))))

(defcheck solution-a3ed03f4
  into '())

(defcheck solution-a3ef7e51
  #(reduce (fn [a b] (cons b a)) () %))

(defcheck solution-a426476b
  (fn [x]
    (loop [coll x rev nil]
      (if (empty? coll)
        rev
        (recur (rest coll) (conj rev (first coll)))))))

(defcheck solution-a454b90d
  (fn rev [xs]
    (case xs
      [] xs
      (concat (rev (rest xs)) (take 1 xs)))))

(defcheck solution-a4811668
  into ())

(defcheck solution-a4ddba2a
  reduce (fn [x y] (conj x y)) '())

(defcheck solution-a4e98e93
  (fn myreverse [v]
    (if (= '() (rest v))
      (list (first v))
      (cons (last v) (myreverse (drop-last v)))
      )
    ))

(defcheck solution-a4f6b875
  reduce #(cons %2 %1) [])

(defcheck solution-a55edc19
  (fn rvs [x]
    (if (empty? x)
      '()
      (cons
        (last x)
        (rvs (butlast x))))))

(defcheck solution-a56238b0
  (fn [arg]
    (loop [s arg out nil]
      (if (empty? s)
        out
        (recur (rest s) (conj out (first s)))))))

(defcheck solution-a77578e8
  (fn [s] (reduce conj (list) s)))

(defcheck solution-a7f68b6b
  (fn [s]
    (loop [orig s res ()]
      (if (empty? orig)
        res
        (recur (rest orig) (conj res (first orig)))))))

(defcheck solution-a859d0b1
  (fn myReverse
    [sequence]
    (reduce conj () sequence)
    ))

(defcheck solution-aa598db5
  (fn [l] (reduce #(cons %2 %1) () l)))

(defcheck solution-aae3a513
  (fn rev [x]
    (let [x (into [] x)]
      (map
        (fn [el]
          (nth x (- (- (count x) 1) (.indexOf x el))))
        x))))

(defcheck solution-aaecfe22
  #(reduce (fn [result x] (conj result x)) () %))

(defcheck solution-ab39e2c
  #(loop [result '() xs %]
     (if (seq xs)
       (recur (cons (first xs) result) (rest xs))
       result)))

(defcheck solution-ab569a0e
  (fn [mySequence]
    (loop [
           s mySequence
           r '()] (

                    if (empty? s)
                    r
                    (recur (rest s) (conj r (first s)))
                    )
                  )
    ))

(defcheck solution-abbe8761
  #(loop [f %, acc []] (if (empty? f) acc (recur (rest f) (cons (first f) acc)))))

(defcheck solution-abe37e7a
  (fn [seq]
    (apply conj '() seq)))

(defcheck solution-abfa2a00
  (fn myrev [seq] (if (not (= seq (empty seq)))
                    (cons (last seq) (myrev (butlast seq)))
                    (empty seq))))

(defcheck solution-abfd34b1
  (fn my-reverse [sq]
    (reduce conj '() sq)))

(defcheck solution-acd8887
  into ())

(defcheck solution-acde31e8
  (fn rev [col]
    (if (nil? (seq col))
      []
      (cons (last col) (rev (drop-last (seq col)))))))

(defcheck solution-ad9e376e
  #((fn f [a r]
      (if (empty? a)
        r
        (f (rest a) (concat [(first a)] r ))
        )) % []))

(defcheck solution-adb4be69
  (fn
    [elems]
    (loop [new [] elems elems]
      (if (not= nil (last elems))
        (recur (conj new (last elems)) (butlast elems))
        new
        )
      )))

(defcheck solution-add7589e
  (fn my-reverse [coll]
    (into '() coll)))

(defcheck solution-ae0f9a32
  (fn myrest [xs]
    (into (if (set? xs) [] (empty xs))
      (reduce #(conj %1 %2) () xs))))

(defcheck solution-aed15ba7
  #(vec (into '() (vec %))))

(defcheck solution-aed26c09
  reduce #(concat (list %2) %) '())

(defcheck solution-af03b2c
  (fn [s]
    (loop [se s, ret ()]
      (if se
        (recur (next se) (conj ret (first se)))
        ret))))

(defcheck solution-af170dfc
  (fn rev [coll]
    (loop [acc (list)
           coll coll]
      (if (empty? coll)
        acc
        (recur (cons (first coll) acc) (rest coll))))))

(defcheck solution-af25540
  (fn rev [s]  (if (empty? s)    []    (conj     (rev (rest s))     (first s)    )  )))

(defcheck solution-af40dd02
  #(reduce (fn [a-coll new-item] (cons new-item a-coll)) '() %))

(defcheck solution-af94e840
  #(loop [from % to []]
     (if (empty? from) to
                       (recur (take (- (count from) 1) from) (conj to (last from))))))

(defcheck solution-afdf31d6
  (fn [s]
    (loop [rv () rm s]
      (if (empty? rm)
        rv
        (recur (conj rv (first rm)) (rest rm))))))

(defcheck solution-affe2554
  (fn [x]
    (loop [s x r '()]
      (if (empty? s)
        r
        (recur (rest s) (conj r (first s)))))))

(defcheck solution-b07cb5e3
  (fn ! [x] (if (seq x) (concat (! (rest x)) [(first x)]) [])))

(defcheck solution-b1197420
  #(vec (reduce conj () %)))

(defcheck solution-b1ca3065
  #(reduce (fn [acc x] (cons x acc)) () %))

(defcheck solution-b207553b
  #(map second
     (sort (map vector
             (range (count %) 0 -1)
             %))))

(defcheck solution-b2998dc5
  (fn myreverse [l]
    (loop [m l s []]
      (if (empty? m) s (recur (rest m) (cons (first m) s)))
      )
    ))

(defcheck solution-b2d14b1
  (fn [coll] (into () coll)))

(defcheck solution-b2f8a1a4
  (fn rev [vect]
    (if (= (count vect) 1)
      (vector (first vect))
      (conj (rev (rest vect)) (first vect))
      )
    ))

(defcheck solution-b308605
  (fn [coll]
    (loop [o (seq coll)
           c (count coll)
           n 0
           l (empty coll)]
      (if (== n c)
        l
        (recur o c (inc n) (cons (nth o n) l))))))

(defcheck solution-b35c733
  (fn reverse' [xs] (if (empty? xs) [] (conj (reverse' (next xs)) (first xs)))))

(defcheck solution-b36aacaf
  (fn [s]
    (let [helper-fn (fn [in out]
                      (if (empty? in)
                        out
                        (recur
                          (rest in)
                          (conj out (first in)))))]
      (helper-fn s (list)))))

(defcheck solution-b3732565
  (fn [lst]
    (reduce conj '() lst)))

(defcheck solution-b38435b5
  (fn [l] (into '() l)))

(defcheck solution-b450c03f
  (fn [xs]
    (reduce conj '() xs)))

(defcheck solution-b47fb683
  #(reduce (fn[x,y] (cons y x)) [] %))

(defcheck solution-b5f3cf0a
  (fn [xs]
    (loop [rxs ()
           xs xs]
      (if (empty? xs)
        rxs
        (recur (conj rxs (first xs)) (rest xs))))))

(defcheck solution-b6b07be9
  reduce #(conj % %2) '())

(defcheck solution-b6b2ac79
  (fn myrev [list]
    (if (> (count list) 0)
      (cons (last list) (myrev (drop-last 1 list)))
      (first list))))

(defcheck solution-b740b33f
  (fn my-reverse [coll]
    (loop [remaining-coll coll result '()]
      (if (empty? remaining-coll)
        result
        (recur (rest remaining-coll) (cons (first remaining-coll) result))))))

(defcheck solution-b7544175
  reduce (fn [x y] (cons y x)) [])

(defcheck solution-b786f7b6
  (letfn [(revers* [coll]
            (when (seq coll)
              (into (list) coll)))]
    revers*))

(defcheck solution-b8447c0f
  reduce #(cons %2 %) [])

(defcheck solution-b95b6652
  #(loop[v %1, r []] (if (empty? v) (vec r) (recur (rest v) (cons (first v) r)))))

(defcheck solution-bab195c7
  (fn [set] (loop [s set result '()]
              (if (= (count s) 0)
                result
                (recur  (rest s ) (conj result  (first s)))
                ))))

(defcheck solution-bb793b05
  (fn [l] (reduce #(cons %2 %1) [] l)))

(defcheck solution-bbad0247
  (fn reverse-seq [x]
    (if (< 1 (count x))
      (cons (last x) (reverse-seq (take (- (count x) 1) x)))
      x
      )
    ))

(defcheck solution-bbc75f7c
  (fn [seq]
    (loop [s seq
           result []]
      (if (> (count s) 0)
        (recur (drop-last 1 s) (conj result (last s)))
        result))))

(defcheck solution-bbcfc581
  #(loop [x % res []]
     (if (empty? x)
       res
       (recur (butlast x) (conj res (last x))))))

(defcheck solution-bbfc8029
  #(reduce (fn [r a] (conj r a)) '() %))

(defcheck solution-bc51f0f
  (fn [s] (reduce #(cons %2 %1) [] s)))

(defcheck solution-bc7fe804
  (fn [coll] (reduce #(cons %2 %1) () coll)))

(defcheck solution-bce0f468
  (fn rev [coll]
    (if (empty? coll)
      coll
      (concat  [(last coll)] (rev (butlast coll))))))

(defcheck solution-bcea0ce3
  #( loop [copylist nil i 0 ]
     (if (= i (count %1))
       copylist
       (recur (conj copylist (nth (seq %1) i )) (inc i))
       )))

(defcheck solution-bd46ef3a
  (fn [s]
    (loop [r (rest s)
           acc (conj () (first s))]
      (if (empty? r)
        acc
        (recur (rest r) (conj acc (first r)))))))

(defcheck solution-bed431ba
  (fn f[lista]
    (if (empty? lista)
      ()
      (cons (last lista) (f (take (dec (count lista)) lista))))
    ))

(defcheck solution-bf0d5458
  reduce (fn [a b] (into a (list b))) ())

(defcheck solution-bf1f09ed
  (fn rev [s]
    (if (seq s)
      (conj (rev (rest s)) (first s))
      [])))

(defcheck solution-bf2fce2a
  #(into () %))

(defcheck solution-bfa03767
  (fn [l] (reduce #(cons %2 %) [] l)))

(defcheck solution-c09b4a5a
  reduce conj ())

(defcheck solution-c11afab2
  #(reduce (fn [a b] (cons b a)) '() %))

(defcheck solution-c135e96
  (fn [coll]
    (loop [ret []
           coll coll]
      (if (empty? coll)
        ret
        (recur (cons (first coll) ret) (rest coll))))))

(defcheck solution-c16d26a8
  (fn [l]
    (loop  [l l r []]
      (if (= (count l) 0)
        r
        (recur
          (rest l)
          (cons (first l) r)
          )
        )
      )
    ))

(defcheck solution-c17e21c0
  (partial reduce conj '()))

(defcheck solution-c3c7c876
  #(into()%))

(defcheck solution-c40178ed
  (fn[col]
    (reduce #(conj %1 %2) (list) col)
    ))

(defcheck solution-c46ee10d
  #(loop [list %1 rlist []] (if (empty? list) rlist (recur (rest list) (cons (first list) rlist)))))

(defcheck solution-c4afdc3d
  (fn rev [l] (if (empty? l) l (concat (rev (rest l)) [(first l)]))))

(defcheck solution-c54f15d0
  #(reduce (fn [x y] (cons y x)) [] %))

(defcheck solution-c556a2f5
  (fn [seq] (loop [old seq reversed '()] (if (empty? old) reversed (recur (rest old) (cons (first old) reversed))))))

(defcheck solution-c5bed892
  #(loop [lst %
          acc ()]
     (if (empty? lst) acc
                      (recur (rest lst) (conj acc (first lst))))))

(defcheck solution-c652a9e0
  (fn rev [s] (if-let [f (first s)] (conj (rev (rest s)) f) [])))

(defcheck solution-c655dc3f
  (fn [x] (reduce (fn [v c] (concat [c] v)) [] x)))

(defcheck solution-c676f247
  (partial reduce (fn [x y] (cons y x)) []))

(defcheck solution-c88078b9
  (fn cou [cont]

    (if (next cont)

      (concat (cou (rest cont)) (list (first cont))    )

      cont
      )
    ))

(defcheck solution-c9015a8a
  reduce (fn [z a] (conj z a)) '())

(defcheck solution-c9966af7
  (fn [col] (reduce conj '() col)))

(defcheck solution-c9d8d33d
  (fn [x] (
            letfn [ (a [y t]
                      (if (= 1 t)
                        y
                        (cons (last y) (a (butlast y) (dec t)))
                        )) ]
            (a x (count x)))))

(defcheck solution-ca571f38
  (fn [coll]
    (loop [src coll result []]
      (if (empty? src)
        result
        (recur (drop-last src) (conj result (last src)) )))))

(defcheck solution-caeaa575
  (fn r [s] (if (seq s) (conj (r ((comp seq pop vec) s)) (last s)) nil )))

(defcheck solution-cc1af91
  (fn myfunc [x] (if (not-empty x) (cons (last x) (myfunc (drop-last x))))))

(defcheck solution-cc456799
  (fn [li] (sort #(compare %2 %1) li)))

(defcheck solution-cc9d1d3f
  (fn [coll]
    (reduce (fn [x y] (cons y x)) [] coll)
    ))

(defcheck solution-cd910e10
  (fn rev [coll] (reduce conj () coll)))

(defcheck solution-ce8c1f50
  (fn [s]
    (loop [s s
           res []]
      (if (empty? s)
        res
        (recur (rest s) (cons (first s) res))))))

(defcheck solution-cef1b57b
  (fn my-reverse2 [s]
    (let [v (reduce conj  () s)]
      (if (vector? s) (vec v) v))))

(defcheck solution-cf70a400
  (fn r[x]
    (if (empty? x)
      []
      (conj (r (rest x)) (first x)))))

(defcheck solution-d00b22f
  (fn reverse' [coll]
    (reduce (fn [accum item]
              (cons item accum))
      []
      coll)))

(defcheck solution-d0212e37
  (fn [xs] (loop [a nil x xs] (if (empty? x) a (recur (cons (first x) a) (rest x))))))

(defcheck solution-d0806da6
  (fn myrev [s]
    (if (empty? s) [] (conj (myrev (rest s)) (first s)))))

(defcheck solution-d0aa43fd
  (fn [xs]
    (loop [xs xs
           zs '()]
      (if (seq xs)
        (recur (rest xs) (conj zs (first xs)))
        zs))))

(defcheck solution-d0df4f98
  (fn [s]
    (loop [s s
           rev '()]
      (if-let [s (seq s)]
        (recur (next s) (conj rev (first s)))
        rev))))

(defcheck solution-d1e3f3d3
  #(loop [r '() s %]
     (if (empty? s) r (recur (conj r (first s)) (rest s)))))

(defcheck solution-d1feb493
  #(reduce conj '()  %))

(defcheck solution-d274860c
  (fn rev [coll] (let [f (fn [col tmp] (if (empty? col) tmp (recur (rest col) (conj tmp (first col)))))] (f coll '()))))

(defcheck solution-d292f41d
  (fn [coll]
    (loop [coll coll
           rcoll []]
      (if (not (nil? coll))
        (recur (next coll) (cons (first coll) rcoll))
        rcoll))))

(defcheck solution-d29960c9
  (fn [xs] (loop [xs xs acc '()]
             (if (empty? xs)
               acc
               (recur (rest xs) (conj acc (first xs)))))))

(defcheck solution-d341da4b
  (fn [sequence]
    (reduce (fn [a b] (cons b a) ) [] sequence)))

(defcheck solution-d3480428
  (fn [coll]
    (loop [ret [] coll coll]
      (if (empty? coll)
        ret
        (recur (conj ret (last coll)) (butlast coll))))))

(defcheck solution-d46e3ef5
  (fn [s]
    (loop [se s resto []]
      (if (empty? se)
        resto
        (recur (rest se) (cons (first se) resto))))))

(defcheck solution-d4ccbf17
  ;;#(reduce (fn [ls e] (conj ls e)) '() %)

  #(into '() %))

(defcheck solution-d4f78be1
  (fn [coll]
    (into '() coll)))

(defcheck solution-d53a6b38
  (fn [coll]
    (reduce conj '() coll)))

(defcheck solution-d55f050a
  (fn myrevcons [seq]
    (if (= (count seq) 0)
      seq
      (cons (last seq) (myrevcons (butlast seq))))))

(defcheck solution-d5c9ee00
  (fn rev[x] (if (empty? x) [] (concat (rev (rest x)) (list (first x))))))

(defcheck solution-d5dcbe40
  (fn rev [x] (if (= x '()) '() (concat (rev (rest x)) [(first x)]))))

(defcheck solution-d6ef9810
  #(loop [s % r (seq '())]
     (if (empty? s) r
                    (recur (next s) (cons (first s) r)))))

(defcheck solution-d774959f
  (fn myrev [x]
    (if (= (count x) 1)
      x
      (cons (last x) (myrev (butlast x)) )
      )))

(defcheck solution-d7ff97f
  #(into '() %))

(defcheck solution-d824c7e2
  (fn [xs]
    (reduce conj () xs)))

(defcheck solution-d8b55826
  (fn [l]
    (reduce (fn [res it] (cons it res)) `() l)))

(defcheck solution-d9075391
  (fn rev [xs] (if (empty? xs) () (conj (rev (butlast xs)) (last xs)))))

(defcheck solution-d9ad4300
  (fn rev [ls]
    (if (empty? ls)
      []
      (conj (rev (rest ls)) (first ls)))))

(defcheck solution-d9f0b4e
  (fn [vs] ((fn [v a] (if (empty? v) a (recur (rest v) (conj a (first v))))) vs '())))

(defcheck solution-dab80926
  (fn rev [lst]
    (if (empty? (rest lst))
      [(first lst)]
      (concat (rev (rest lst)) [(first lst)]))))

(defcheck solution-db5e70c3
  #(reduce (fn [cum it] (conj cum it)) '() %))

(defcheck solution-db6ead62
  #(loop [m % c []]
     (if m
       (recur (butlast m) (conj c (last m)))
       c)))

(defcheck solution-db9ac4c9
  #(reduce (fn [acc elt] (cons elt acc)) (empty %) %))

(defcheck solution-dbbdbf9e
  (fn foo [s]
    (loop [i []
           r s]
      (if (empty? r) i
                     (recur (cons (first r) i)
                       (rest r))))))

(defcheck solution-dc23755a
  (fn [coll]
    (loop [result []
           coll coll
           cnt (count coll)]
      (if (empty? coll)
        result
        (recur (conj result (last coll)) (take (- cnt 1) coll) (dec cnt))))))

(defcheck solution-ddc9b8df
  (fn [coll] (let [cl (vec coll) m (count cl) m1 (- m 1)] (for [i (range m)] (nth cl (- m1 i)) ))))

(defcheck solution-de7bee58
  (fn [lst]
    (loop [ls lst rlst ()]
      (if (empty? ls) rlst
                      (recur (rest ls) (cons (first ls) rlst))))))

(defcheck solution-dea243f8
  (fn [coll]
    (let [s (seq coll)
          f (fn [xs acc]
              (if (empty? xs) acc
                              (recur (rest xs) (conj acc (first xs)))))]
      (f s '())
      )))

(defcheck solution-deb33343
  #(reduce (fn [acc x] (conj acc x)) '() %))

(defcheck solution-df07f2f2
  #(loop [L %, result '()]
     (if (empty? L)
       result
       (recur (rest L) (conj result (first L))))))

(defcheck solution-df60bd08
  (fn [x]
    (loop [lst x revlst '()]
      (if (empty? lst)
        revlst
        (recur (rest lst) (conj revlst (first lst)))))))

(defcheck solution-dfbc1fb3
  #(reduce (fn [s x] (cons x s)) () %))

(defcheck solution-dffa0de3
  (fn [s]
    (loop [result []
           s s]
      (if (seq s)
        (recur (concat [(first s)] result) (rest s))
        result))))

(defcheck solution-e0ef35b8
  (fn [initial-l]
    (loop [acc [], l initial-l]
      (if (not (empty? l))
        (recur (concat   [(first l)] acc ) (rest l))
        acc))))

(defcheck solution-e12a282a
  (fn [coll]
    (reduce (fn [a b] (cons b a)) [] coll)))

(defcheck solution-e1c19d16
  #(reduce conj (list) %))

(defcheck solution-e1cb56b3
  #(
    (fn reversy [l, rev]
      (if
       (= () l)
        rev
        (reversy (rest l) (cons (first l) rev))))
    %1
    '()
    ))

(defcheck solution-e1dfef1b
  (fn [s] (loop [s s, r ()] (if (seq s) (recur (rest s) (cons (first s) r)) r))))

(defcheck solution-e2e4eabe
  (fn [coll]
    (loop [input coll result (list)]
      (if (= input '()) result
                        (recur (rest input) (conj result (first input)))))))

(defcheck solution-e3333f1a
  (fn rev-seq [x]
    (if (= (count x) 1)
      x
      (conj
        (rev-seq (drop-last x))
        (last x)))))

(defcheck solution-e6033266
  (partial (fn [n s]
             (if (empty? s)
               n
               (recur (conj n (last s)) (butlast s))))
    []))

(defcheck solution-e676421b
  #(loop [xs % rev '()]
     (if xs
       (recur (next xs)
         (conj rev (first xs)))
       rev)))

(defcheck solution-e6fc3422
  ;#(reverse %)

  (fn [coll]
    (loop [out []
           in coll]
      (if (empty? in)
        out
        (recur (conj out (last in))
          (drop-last in))))))

(defcheck solution-e731c904
  (fn rev [v]
    (reduce #(cons %2 %1) [] v)))

(defcheck solution-e74697be
  #(loop [r [] s %] (if (empty? s) r
                                   (recur (cons (first s) r) (drop 1 s)))))

(defcheck solution-e768f830
  (fn [l] (
            loop [remaining l reversed []]
            (if (empty? remaining)
              reversed
              (recur (take (dec (count remaining)) remaining) (conj reversed (last remaining)))
              ))))

(defcheck solution-e972cd02
  (fn [coll]
    (loop [c coll
           acc []]
      (if (seq c)
        (recur (rest c) (cons (first c) acc))
        acc))))

(defcheck solution-e973f7f7
  (fn [xs]
    (loop [rs xs, fs []]
      (if (empty? rs)
        fs
        (recur (rest rs)(conj (seq fs) (first rs)))))))

(defcheck solution-e99e195a
  (fn [coll]
    (loop [lcoll coll ncoll '()]
      (if (empty? lcoll)
        ncoll
        (recur (rest lcoll) (conj ncoll (first lcoll)))))))

(defcheck solution-e9b79b8a
  (fn f [s]
    (if (empty? s)
      []
      (concat (f (rest s)) [(first s)]))))

(defcheck solution-ea0fa9f7
  (fn [x]
    (loop [x x result []]
      (if (empty? x) result
                     (recur (rest x) (cons (first x) result))))))

(defcheck solution-eab4d313
  (fn [col] (sort #(compare %2 %1) col )))

(defcheck solution-eb3073cf
  (fn [acc coll] (if coll
                   (recur (conj acc (first coll))
                     (next coll))
                   acc)) '())

(defcheck solution-eb7bb2a4
  #(loop [x % rv '()]
     (if (empty? x)
       rv
       (recur (rest x) (conj rv (first x))))))

(defcheck solution-ebb82fc1
  (fn reverse-a-seq [s]
    (into () s)))

(defcheck solution-ec102af3
  (fn [items]
    (reduce conj '() items)))

(defcheck solution-ec488990
  reduce #(cons %2 %1) (list))

(defcheck solution-ec4b1251
  (fn [x] (reduce conj () x)))

(defcheck solution-ecf89dbe
  (fn [x] (loop [[h & t] (into [] x) acc '()] (let [newacc (conj acc h)] (if (= t nil) newacc (recur t newacc))))))

(defcheck solution-ed2d86c3
  reduce #(cons %2 %1) ())

(defcheck solution-ed8397e
  #(let [c (seq %)]
     (for [x (-> c count range)]
       (nth c (- (count c) (inc x))))))

(defcheck solution-edf2b2dd
  (partial into '()))

(defcheck solution-ee76b61a
  (fn [x]
    (reduce #(conj %1 %2) '() (seq x))))

(defcheck solution-eeb40ae6
  (fn rev [xs] (if (empty? xs) xs (conj (rev (butlast xs)) (last xs)))))

(defcheck solution-eebd2fa5
  (fn my-reverse
    ([coll] (my-reverse coll '()))
    ([coll acc] (if coll
                  (recur (next coll) (conj acc (first coll)))
                  acc))))

(defcheck solution-ef18e50c
  (fn [coll]
    (reduce (fn [new, x] (conj new x)) '() coll)
    ))

(defcheck solution-ef89eefb
  #(loop [s %1 ns '()]
     (if (empty? s)
       ns
       (recur (rest s) (conj ns (first s))))))

(defcheck solution-f00315ac
  (fn [s]
    (loop [s s
           r '()]
      (if (empty? s)
        r
        (recur (rest s) (conj r (first s)))))))

(defcheck solution-f07b0a5d
  (fn [coll] (reduce #(concat [%2] %1) [] coll)))

(defcheck solution-f0aea45f
  (fn [l]
    (letfn [(helper [l accum]
              (if (seq l)
                (recur (rest l) (cons (first l) accum))
                accum))]
      (helper l nil))))

(defcheck solution-f0d17ebb
  (fn my-reverse
    ([coll]     (my-reverse coll []))
    ([coll acc] (if (empty? coll) acc (my-reverse (rest coll) (cons (first coll) acc))))))

(defcheck solution-f0f74ed8
  (partial reduce conj nil))

(defcheck solution-f183401
  reduce (fn [a b] (cons b a)) '())

(defcheck solution-f1866441
  (fn my-reverse
    ([lst] (my-reverse lst []))
    ([lst, res]
     (if (= (count lst) 0)
       res
       (my-reverse (rest lst) (cons (first lst) res))))))

(defcheck solution-f1a47d36
  #(loop [l1 %1 l2 [] ]   (if (= (count l1) 0) l2 (recur (butlast l1) (conj l2 (last l1) )))))

(defcheck solution-f1d8400d
  (fn [l]
    (loop [a l, b nil]
      (if a
        (recur (next a) (conj b (first a)))
        b))))

(defcheck solution-f1f207a9
  (fn reverse-seq
    [c1]
    (loop [lc1 c1 rc '()]
      (if (empty? lc1)
        rc
        (recur (rest lc1) (cons (first lc1) rc))))))

(defcheck solution-f22f20b0
  #(loop [o % n (list)]
     (if-let [x (first o)]
       (recur (rest o) (conj n x))
       (if (vector? o)
         (apply vector n)
         n ))))

(defcheck solution-f3ab08db
  (fn [l] (reduce #(conj %1 %2) nil l)))

(defcheck solution-f3f9a0d5
  #(reduce conj () %1))

(defcheck solution-f430a7e8
  (fn [xs]
    (loop [x xs y '()]
      (if (empty? x)
        y
        (recur (rest x)
          (cons (first x) y))))))

(defcheck solution-f442e9de
  (fn reverse-sequence [x]
    (loop [x x
           reversed ()]
      (if (= () x)
        reversed
        (recur (rest x) (cons (first x) reversed))))))

(defcheck solution-f4b08c19
  (fn ! [s] (if (empty? s) s (concat (! (rest s)) (take 1 s)))))

(defcheck solution-f4c3b47f
  (fn [coll]
    (
     (fn [coll acc]
       (if (seq coll)
         (recur (next coll) (conj acc (first coll)))
         acc
         )
       )
     coll
     nil
     )
    ))

(defcheck solution-f6864586
  (fn [x]
    (loop [n (- (count x) 1) il []]
      (if (= n -1) il
                   (recur
                     (dec n) (conj il (nth (seq x) n)))
                   ))))

(defcheck solution-f6a0d63
  (fn [vect]
    (reduce #(cons %2 %1)
      []
      vect)))

(defcheck solution-f6a22c42
  (fn newreverse [x]
    (into '() (vec x))))

(defcheck solution-f6b801b4
  (fn [s]
    (loop [s' s r '()]
      (if (empty? s')
        r
        (recur (rest s') (conj r (first s')))))))

(defcheck solution-f719d1da
  (fn [xs] (reduce #(cons %2 %1) [] xs)))

(defcheck solution-f7301476
  ; first-attempt:
  ;(fn new-rev [coll]
  ;  (if (first coll)
  ;    (cons (last coll)
  ;          (new-rev (butlast coll)))
  ;    []))

  #(into '() %))

(defcheck solution-f8747886
  #(loop[s % res ()]
     (if (first s)
       (recur (rest s) (conj res (first s)))
       res)))

(defcheck solution-f8989e25
  (fn rev[xs] (if (empty? xs) '[] (conj (rev (rest xs)) (first xs)))))

(defcheck solution-f9016870
  (fn my-r2 [l]
    (if (= 0 (count l))
      l
      (concat (list (last l)) (my-r2 (take (- (count l) 1) l))))))

(defcheck solution-f911bf87
  (fn rev [coll]
    (if (empty? coll)
      ()
      (cons (last coll) (rev (butlast coll))))))

(defcheck solution-f95a8f37
  #(reduce (fn [l n] (conj l n)) '() %))

(defcheck solution-fa9dab6d
  (fn my-rev [xs]
    (loop [xs (seq xs)
           ys ()]
      (if xs
        (recur (next xs) (conj ys (first xs)))
        ys))))

(defcheck solution-fb2d4e23
  (fn [x] (reduce #(cons %2 %) nil x)))

(defcheck solution-fb743f99
  (fn rev [s]
    ((fn r [s a]
       (if (empty? s) a
                      (r (rest s) (conj a (first s))))) s '())))

(defcheck solution-fbb85003
  #(loop [coll %
          acc nil]
     (let [head (first coll) tail (rest coll)]
       (if (empty? tail)
         (conj acc head)
         (recur tail (conj acc head))
         )
       )))

(defcheck solution-fcc7f29e
  (fn [xs]
    (reduce
      (fn [first second] (cons second first))
      nil
      xs
      )
    ))

(defcheck solution-fcf7ef47
  (fn rev [par]
    (let [head (first par) tail (rest par)]
      ((fn aux [par s]
         (let [head (first par) tail (rest par)]
           (if-not (zero? (count tail))
             (aux tail (cons head s))
             (cons head s))))
       tail (list head)))))

(defcheck solution-fd0cfdee
  (fn rvr
    [arr]
    (if (list? arr)
      (loop [[x & xs] arr out '()]
        (if (= x nil)
          out
          (recur xs (conj out x))))
      (into () arr))))

(defcheck solution-fd88c695
  (fn [coll]
    (loop [result [] acc coll]
      (if (nil? acc)
        result
        (recur (conj result (last acc)) (butlast acc))))))

(defcheck solution-fdcd0925
  (fn my-reverse [list]
    (let [go (fn [list cumul]
               (let [x (first list)
                     y (rest list)] (if(empty? list) cumul (recur y (cons x cumul)) )))]
      (go list nil))))

(defcheck solution-fe23debb
  (fn [x]
    (loop [s x acc []]
      (if (empty? s)
        acc
        (recur (rest s)(cons (first s) acc))))))

(defcheck solution-fe941b6a
  #(seq (replace (vec %) (vec (take (count %) (iterate dec (- (count %) 1)))))))

(defcheck solution-fec9bb28
  (fn my-reverse [xs] (if (empty? xs) [] (conj (my-reverse (rest xs)) (first xs)))))

(defcheck solution-ff00b422
  (fn [coll]
    (reduce conj '() coll )

    ))

(defcheck solution-ff8fa2c0
  (fn [x] (reduce (fn [acc el] ( cons el acc)) [] x)))

(defcheck solution-ffc4a470
  (fn[a-seq]
    (loop [rev '() ss a-seq]
      (if (empty? ss)
        rev
        (recur (conj rev (first ss)) (rest ss))))))

(defcheck solution-fffb38dc
  (fn rev [r s]
    (if (= (last s) (first s))
      (cons (last s) r)
      (cons (last s) (rev r (butlast s))))) '())