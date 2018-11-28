(ns coal-mine.problem-40
  (:require [coal-mine.checks :refer [defcheck-40] :rename {defcheck-40 defcheck}]
            [clojure.test]))

(defcheck solution-10164faf
  (fn [sep col]
    (butlast (mapcat #(list %1 sep) col))))

(defcheck solution-10ade69d
  (fn my-interpose [sep coll]
    (rest (interleave (repeat sep) coll))))

(defcheck solution-10cbd4c9
  (fn [x coll]
    (rest (flatten (for [n coll] [x n])))))

(defcheck solution-110caad9
  (fn myInterpose [x coll] (drop-last (mapcat #(vector % x) coll ))))

(defcheck solution-116929f0
  (fn [d s]
    (rest (mapcat #(list d %) s))))

(defcheck solution-11bafbff
  (fn [sep coll]
    (butlast (mapcat #(vector % sep) coll))))

(defcheck solution-11bfedb7
  #(rest (interleave (repeat (count %2) %1) %2)))

(defcheck solution-125f1b00
  #(butlast (mapcat (partial conj (list %)) %2)))

(defcheck solution-12c7252b
  #(butlast (interleave %2 (repeat (count %2) %))))

(defcheck solution-1319f1b5
  (fn [s coll]
    (reduce
      (fn [a b]
        (if (empty? a) (conj a b) (conj a s b)))
      []
      coll)))

(defcheck solution-13636f1f
  #(let [c (count %2)]
     (butlast (interleave %2 (take c (repeat %1))))))

(defcheck solution-141ccdac
  (fn [separator items] (concat [(first items)] (mapcat #(conj [separator] %) (rest items)))))

(defcheck solution-145195c
  #(rest (interleave (map (fn [n] %1) %2) %2)))

(defcheck solution-14b7fc35
  (fn [n coll]
    (drop 1 (interleave (repeat n) coll))))

(defcheck solution-14d62190
  (fn [x l]
    (drop-last (apply concat (map #(list % x) l)))))

(defcheck solution-1546e68
  (fn [sep coll]
    (drop 1
      (flatten
        (map vector (repeat (count coll) sep) coll)))))

(defcheck solution-15b1375d
  (fn [v c] (butlast (mapcat #(vector % v) c))))

(defcheck solution-16086b80
  #(drop-last (reduce concat (map (fn [x] (list x %1)) %2))))

(defcheck solution-16380981
  (fn [s c] (reduce (fn [a b] (let [r (conj a b)] (if (= (last c) b) r (conj r s))))  []  c)))

(defcheck solution-16b689d1
  (fn [ele seqs]
    (loop[result (list (first seqs))
          others (rest seqs)]
      (if(empty? others)
        result
        (recur (concat result (list ele (first others)))
          (rest others))))))

(defcheck solution-16c6c52
  (fn [y x] (butlast (mapcat #(conj (vector %) y) x))))

(defcheck solution-17590081
  (fn [x xs]
    (butlast (mapcat #(vector % x) xs))))

(defcheck solution-17755e95
  (fn my-interp [x s]
    (if (< (count s) 2)
      s
      (concat [(first s) x] (my-interp x (rest s))))))

(defcheck solution-1900d873
  (fn newinterpose [x y]
    "After every component of y except the last, inserts a copy of x."
    (butlast (reduce #(conj %1 %2 x) [] y))))

(defcheck solution-1933588c
  (fn interpose-j [v s]
    (cond
      (>= 1 (count s)) s
      :else
      (concat
       [(first s) v]
       (interpose-j v (rest s))))))

(defcheck solution-195648af
  #(loop [a %1 coll %2 result '()]
     (if (empty? coll)
       (drop-last result)
       (recur a (rest coll) (concat result (list (first coll) a))))))

(defcheck solution-196d5141
  (fn ! [x y]  (if (empty? y)  []   (if (empty? (rest y)) y (concat [(first y)] [x] (! x (rest y)))  )  )   ))

(defcheck solution-19734a2b
  (fn [v xs]
    (if (coll? xs)
      (loop [ys (rest xs)
             zs [(first xs)]]
        (if (seq ys)
          (recur (rest ys) (conj zs v (first ys)))
          zs))
      nil)))

(defcheck solution-197a4b4
  (fn [n coll]
    (conj (vec (mapcat #(list % n) (butlast coll))) (last coll))))

(defcheck solution-19979142
  (fn [item col] (let [recursor (fn recurs [out in] (if (empty? in) out (recurs (concat out (list item (first in))) (rest in))))] (recursor (list (first col)) (rest col)))))

(defcheck solution-199ece98
  (fn [v l]
    (loop [news '() tmpl l]
      (if (empty? tmpl)
        (reverse (rest news))
        (recur (conj (conj news (first tmpl)) v) (rest tmpl))))))

(defcheck solution-19a1e1a
  (fn [x y] (reduce (fn [a b] (cons b (cons x a))) [(last y)] (rest (reverse y)))))

(defcheck solution-1b00a2a2
  (fn [elem, sq] (apply vector (drop-last (flatten (map (fn [it] [it elem]) sq))))))

(defcheck solution-1b5637cc
  (fn [sep coll]
    (rest (mapcat list (repeat sep) coll))))

(defcheck solution-1b78ada0
  (fn [item xs] (drop 1 (mapcat list (repeat item) xs))))

(defcheck solution-1c473385
  (fn[itm coll]
    (cons (first coll)
      (mapcat #(list itm %)
        (rest coll)))))

(defcheck solution-1c55c0d2
  (fn [sep coll]
    (drop 1 (interleave (repeat sep) coll))))

(defcheck solution-1ca24cd2
  (fn [v col] (apply concat (for [i col]
                              (cond
                                (= i (last col)) [i]
                                :else [i v])))))

(defcheck solution-1cf13321
  (fn [token strings] (butlast (reduce #(concat %1 [%2 token]) [] strings))))

(defcheck solution-1d221040
  (fn [s, col] (drop-last (mapcat #(list % s) col))))

(defcheck solution-1d592686
  #(butlast(interleave %2 (repeat %1))))

(defcheck solution-1d5fc732
  #(drop-last (interleave %2 (repeat (count %2) %))))

(defcheck solution-1d68c317
  #(rest (for [v %2 x [%1 v]] x)))

(defcheck solution-1d6fe25e
  (fn [x s]
    (reduce #(conj %1 x %2) [(first s)] (rest s))))

(defcheck solution-1d846d27
  (fn [v l] (butlast (reduce #(conj % %2 v) [] l))))

(defcheck solution-1da0a4a5
  (fn [x s]
    (loop [out [] a (first s) in (rest s)]
      (if (empty? in)
        (conj out a)
        (recur (conj out a x) (first in) (rest in))))))

(defcheck solution-1dc89883
  (fn foo [c ls]
    (if-let [n (next ls)]
      (concat (list (first ls) c) (foo c n))
      (list (first ls)))))

(defcheck solution-1df81843
  (fn [sep s]
    (cons (first s)
      (flatten (map list (repeat sep) (rest s))))
    ))

(defcheck solution-1e4b4245
  (fn minterpose [sep coll]
    (rest (flatten (map #(list sep %) coll)))))

(defcheck solution-1e6a50a5
  (fn intrp [sep coll]
    (cons (first coll)
      (mapcat #(vector sep %) (rest coll)))))

(defcheck solution-1f191855
  (fn[n,s] (flatten(reduce (fn[a,b] [a,n,b]) s))))

(defcheck solution-1ffa09f0
  (fn intr [delim col]
    (drop-last (mapcat #(vector % delim) col))))

(defcheck solution-204611bc
  (fn a[sep coll]
    (rest
      (mapcat (partial list sep) coll))))

(defcheck solution-20d2324f
  (fn [seperator seq]
    (drop 1 (apply concat (map (fn [x y] (list x y))
                            (repeat seperator) seq)))))

(defcheck solution-20f7add2
  (fn [sep coll] (drop-last (interleave coll (repeat sep)))))

(defcheck solution-214fd93d
  (fn [f a] (reverse (drop 1 (reverse (mapcat #(conj (vector %) f) a))))))

(defcheck solution-215a38ce
  (fn [sep xs] (let [ys (flatten (map (fn [x] [x sep]) xs))]
                 (take (- (count ys) 1) ys))))

(defcheck solution-218f5da5
  (fn [x coll]
    (when-first [a coll]
      (cons a (mapcat vector (repeat x) (rest coll))))))

(defcheck solution-21a1066f
  (fn [x coll] (butlast (mapcat #(vector %1 %2) coll (repeat x)))))

(defcheck solution-21d6fdc2
  #(drop-last (mapcat (fn [x] [x %1]) %2)))

(defcheck solution-22005f1b
  (fn [s coll]
    (drop-last (mapcat #(list % s) coll))))

(defcheck solution-2316dd55
  (fn jjj [sep s]
    (reduce #(concat %1 (list sep) (list %2)) (list (first s)) (rest s))
    ))

(defcheck solution-23903ad6
  (fn [x ys] (reduce (fn [zs y] (conj zs x y)) [(first ys)] (rest ys))))

(defcheck solution-23c8f8a7
  (fn my-interpose [delimiter lst]
    (if
     (= (count lst) 1) lst
                       (concat [(first lst)] [delimiter] (my-interpose delimiter (next lst))))))

(defcheck solution-23d8368b
  (fn [it c] (-> (reduce #(conj %1 %2 it) [] (butlast c))
               (conj (last c)))))

(defcheck solution-24f917bb
  (fn [sep items] (rest (interleave (repeat sep) items))))

(defcheck solution-251b881
  (fn [sep in-seq]
    ((fn [i-seq res-vec]
       (if (empty? i-seq)
         res-vec
         (recur (rest i-seq) (conj res-vec sep (first i-seq)))
         )
       )
     (rest in-seq) [(first in-seq)])
    ))

(defcheck solution-25243092
  (fn i [v [x & r :as c]]
    (if (empty? r)
      c
      (concat [x v] (i v r)))))

(defcheck solution-256beccf
  #(rest (apply concat (for [i %2] [% i]))))

(defcheck solution-25a2d698
  (fn [a b] (pop (vec (mapcat (fn [c] (vector c a)) b)))))

(defcheck solution-25d7d831
  (fn [x s] (next (mapcat list (repeat x) s))))

(defcheck solution-260d60d
  #(drop-last (interleave %2 (repeat (count %2)  %1))))

(defcheck solution-2659c374
  (fn walk [x coll]
    (if (empty? (rest coll))
      (if (first coll) (list (first coll)) nil)
      (cons (first coll) (cons x (walk x (rest coll)))))))

(defcheck solution-26660b56
  (fn [i lst] (conj (vec (mapcat #(vector % i) (drop-last lst))) (last lst))))

(defcheck solution-26842b54
  (fn myinterpose [i s]
    (if (< (count s) 2) s
                        (concat (list (first s) i) (myinterpose i (rest s))))))

(defcheck solution-27209fc9
  (fn [d s] (rest (mapcat #(list d %) s))))

(defcheck solution-273f6594
  (fn [value col]
    (flatten (conj [] (first col) (for [i (rest col)] [value i])))))

(defcheck solution-2784249c
  (fn [p coll]
    (drop-last (reduce #(conj % %2 p) [] coll))))

(defcheck solution-27c038
  (fn [x y] (cons (first y) (mapcat #(list x %) (rest y)))))

(defcheck solution-281a0d99
  #(butlast (mapcat list %2 (repeat %))))

(defcheck solution-2889c77e
  (fn [item coll]
    (reduce (fn [result elt] (conj result item elt))
      [(first coll)]
      (rest coll))))

(defcheck solution-292890f
  (fn interpose-seq [separator collection]
    (rest (apply concat (map (fn [item] [separator item]) collection)))))

(defcheck solution-29992a8a
  #(vec (drop-last (mapcat (partial conj (list %1)) %2))))

(defcheck solution-29daeb19
  #(butlast (mapcat list %2 (repeat %1))))

(defcheck solution-29f7bbeb
  (fn this
    ([ins xs] (this ins xs []))
    ([ins [hd & tl :as xs] acc]
     (if (empty? xs)
       acc
       (if (empty? tl)
         (conj acc hd)
         (recur ins tl (into acc [hd ins])))))))

(defcheck solution-2a205ced
  #(butlast (interleave %2 (repeat %1))))

(defcheck solution-2a5da7bd
  (fn [sep sq]
    (let [aux
          (fn [[head & tail] acc]
            (cond
              (empty? tail)
              (conj acc head)

              :else
              (recur tail (conj acc head sep))))]
      (aux sq []))))

(defcheck solution-2a6a9db2
  #(drop-last (interleave %2 (repeat %1))))

(defcheck solution-2aea71e
  (fn [c l]
    (loop [[f & args :as my-l] l
           r '()]
      (if (empty? my-l)
        r
        (if (empty? args)
          (concat r (list f))
          (recur args (concat r (list f c))))))))

(defcheck solution-2ba521a6
  (fn [val xs] (concat (mapcat #(vector % val) (butlast xs)) [(last xs)])))

(defcheck solution-2bb1a8f9
  (fn [sep s] (butlast (mapcat #(list %1 sep) s))))

(defcheck solution-2c6e2e8a
  (fn [x coll]
    (butlast
      (interleave coll
        (take (count coll)
          ((fn iter [y] (cons y (lazy-seq (iter y))))
           x))))))

(defcheck solution-2c8fe945
  (fn my-interpose [sep l]
    (loop [l l f []]
      (if (empty? (rest l))
        (conj f (first l))
        (recur (rest l) (conj (conj f (first l)) sep))))))

(defcheck solution-2ca138c5
  (fn [sep xs] (butlast (interleave xs (repeat sep)))))

(defcheck solution-2d17c5f
  #(rest (interleave [% % % %] %2)))

(defcheck solution-2d358c3d
  (fn [p x] (rest (mapcat (partial list p) x))))

(defcheck solution-2dc74d33
  #(->> %1 repeat (interleave %2) drop-last))

(defcheck solution-2ddd6d94
  #(pop (vec (interleave %2 (repeat (count %2) %1)))))

(defcheck solution-2f46a2a3
  (fn [v c] (reduce #(concat %1 [v] [%2]) [(first c)] (rest c))))

(defcheck solution-2f8192d4
  (fn intersperse [i xs]
    (when-not (empty? xs)
      (let [reduceRight (fn reduceRight [f z _xs]
                          (if-let [[x & xs] _xs]
                            (f x #(reduceRight f z xs))
                            z))]
        (cons (first xs) (reduceRight #(cons i (cons %1 (%2))) '() (rest xs)))))))

(defcheck solution-2feae068
  (fn pose [sep s]
    (loop [s s acc []]
      (if (seq s)
        (recur (rest s) (conj acc (first s) sep))
        (pop acc)))))

(defcheck solution-30d29d46
  #(rest (mapcat (fn [v] [%1 v]) %2)))

(defcheck solution-30f4467b
  #(loop [coll %2
          acc '()]
     (if-let [r (seq (rest coll))]
       (recur r (concat acc (list (first coll) %1)))
       (concat acc (list (first coll)))
       )))

(defcheck solution-3106d26
  (fn [sep coll]
    (drop 1 (apply concat (map #(vector sep %) coll)))
    ))

(defcheck solution-314dfa6
  (fn [a l]    (cons  (first l) (mapcat  #(list a %)  (rest l)   ) )         ))

(defcheck solution-31729b83
  (fn [n coll]
    (butlast (reverse
               (reduce
                 (fn [acc elem]
                   (cons n (cons elem acc)))
                 ()
                 coll)))))

(defcheck solution-31a09e31
  (fn
    [v l]
    (drop-last
      (reduce
        (fn [xs x]
          (conj xs x v)
          )
        []
        l
        )
      )
    ))

(defcheck solution-3229afd1
  (fn [y xs] (rest (mapcat list (repeat y) xs))))

(defcheck solution-3242f4c6
  (fn [x s] (cons (first s) (mapcat list (repeat x) (rest s)))))

(defcheck solution-326578e1
  (fn [e lst]
    (loop [l lst
           r []]
      (if (empty? l) r
                     (recur (rest l)
                       (if (empty? (rest l)) (conj r (first l))
                                             (conj r (first l) e)))))))

(defcheck solution-3399d813
  #(loop [el %1 li %2 result []]
     (if (= 1 (count li))
       (into result li)
       (recur el (rest li) (conj result (first li) el)))))

(defcheck solution-33b47bb0
  #(butlast (loop [in %2 out []]
              (if (nil? (first in))
                out
                (recur (rest in) (conj out (first in) %1))
                )
              )
     ))

(defcheck solution-33c4fc9d
  (fn [sep xs] (rest (interleave (repeat sep) xs))))

(defcheck solution-34481a1c
  (fn g [x s]
    (let [f (fn f#[x s]
              (if (seq s)
                (cons x (cons (first s) (lazy-seq (f# x (next s)))))
                nil))]
      (drop 1 (f x s)))))

(defcheck solution-345fb60b
  (fn [n x] (reduce (fn [a b] (conj a n b)) [(first x)] (rest x))))

(defcheck solution-349bd090
  ; This is just hilariously hacky!
  (fn custom-interpose [sep, in-seq]
    (butlast (flatten
               (map (fn [in, sep] [in sep]) in-seq (take (count in-seq) (repeat sep)))))))

(defcheck solution-34fededf
  (fn interpose-a-seq [x c]
    (subvec

      (reduce
        (fn [a i] (conj a i x)) [] c)

      0 (dec (* 2 (count c))))))

(defcheck solution-35d0f25f
  (fn [sep s] (rest (mapcat #(list sep %) s))))

(defcheck solution-35e67222
  #(butlast (mapcat (fn [a] [a %]) %2)))

(defcheck solution-36ae75a
  #(butlast (mapcat (fn [i] [i %]) %2)))

(defcheck solution-36d02d9
  (fn [sep [s & ss]] (loop [[h & t] ss acc [s]] (if (= t nil) (conj acc sep h) (recur t (conj acc sep h))))))

(defcheck solution-36eb0a8c
  (fn [x s]
    (butlast (interleave s (repeat (count s) x)))))

(defcheck solution-37cd6016
  (fn [i as]
    (if (empty? as)
      as
      (loop [xs (rest as)
             r [(first as)]]
        (if (empty? xs) r
                        (recur (rest xs) (into r [i (first xs)])))))))

(defcheck solution-380a6f8b
  (fn I [x l]
    (cond (empty? l) []
          (empty? (rest l)) l
          :else (list* (first l) x (I x (rest l))))))

(defcheck solution-38736334
  (fn f[x l]
    (drop-last (reduce concat (map vector l (repeat (count l) x))))))

(defcheck solution-38798249
  (fn [v l]
    (loop [l l result '()]
      (if
       (= (count l) 1)
        (concat result l)
        (recur (rest l) (concat result [(first l) v]))))))

(defcheck solution-3955dab8
  (fn [a s]
    (drop-last 1 (mapcat #(list % a) s))))

(defcheck solution-39a1eb75
  (fn inter [e v]
    (loop [v v, r (empty v)]
      (if (seq v)
        (if (seq (rest v))
          (recur (rest v) (conj r (first v) e))
          (conj r (first v)))
        r))))

(defcheck solution-39e6885a
  (fn [x s]
    (let [xs (map (fn [_] x) (range (count s)))]
      (pop (vec (interleave s xs))))))

(defcheck solution-3a754393
  (fn[v, c] (butlast (mapcat #(vec [% v]) c))))

(defcheck solution-3b7ffa
  #(rest (mapcat (partial list %) %2)))

(defcheck solution-3bb8c8e1
  #(drop-last (interleave  %2 (repeat %1))))

(defcheck solution-3c1b61bf
  (fn [separator coll]
    (cons (first coll) (mapcat (fn [item] [separator item]) (rest coll)))))

(defcheck solution-3c26b7d
  (fn [sep c]
    (rest (reduce (fn [a b]
                    (conj a sep b))
            []
            c))))

(defcheck solution-3c3119d2
  (fn [sep source]
    (butlast (reduce (fn [coll item] (conj coll item sep)) [] source))))

(defcheck solution-3cea54b6
  (fn [init ls] (rest (flatten (map (fn [v] [init v]) ls)))))

(defcheck solution-3d9451f
  (fn my-interpose [x col]
    (cond
      (empty? col) nil
      (empty? (rest col)) [(first col)]
      :else (cons (first col)
              (cons x (my-interpose x (rest col)))))))

(defcheck solution-3e322bd9
  (fn [i l] (rest (interleave (repeat i) l))))

(defcheck solution-3e4069e3
  ;(fn [sep coll]
  ;  (when (seq coll)
  ;    (loop [coll coll, res []]
  ;      (if (next coll)
  ;        (recur (next coll) (conj res (first coll) sep))
  ;        (seq (conj res (first coll)))))))
  ;
  (fn ip [sep coll]
    (letfn [(go [c]
              (when-let [s (seq c)]
                (if (next s)
                  (cons (first s)
                    (cons sep
                      (go (rest s))))
                  (list (first s)))))]
      (lazy-seq (go coll)))))

(defcheck solution-3e823134
  (fn pose
    [inter, s]
    (if (= (count s) 1)
      s
      (cons (first s) (cons inter (pose inter (next s)))))))

(defcheck solution-3eae4c4c
  (fn thisfunc [x l]
    (if (<= (count l) 1)
      l
      (conj (thisfunc x (rest l)) x (first l)))))

(defcheck solution-3f5d6216
  (fn [x y] (butlast (interleave y (take (count y) (cycle [x]))))))

(defcheck solution-400e4c02
  (fn [i v] (rest (reduce #(conj % i %2) [] v))))

(defcheck solution-403f802f
  (fn [a s]
    (rest (interleave (repeat a) s))))

(defcheck solution-405045a6
  (fn [sep coll]
    [sep coll] (drop 1 (interleave (repeat sep) coll))))

(defcheck solution-40854caa
  (fn [x s]
    (drop-last(interleave s (repeat x)))))

(defcheck solution-40a0178b
  (fn [sep xs]
    (concat (list (first xs)) (mapcat #(list sep %) (rest xs)))))

(defcheck solution-40a5821e
  (fn [t xs] (drop-last (mapcat #(vector % t) xs))))

(defcheck solution-413d4389
  (fn [n xs] (reverse (drop 1 (reverse (mapcat (fn [x] [x n]) xs))))))

(defcheck solution-41aa5723
  (fn f [a b]
    (butlast (mapcat #(concat [%] [a]) b))
    ))

(defcheck solution-41bfb04d
  (fn [v s]
    (butlast (mapcat (fn [x] (list x v)) s))))

(defcheck solution-41c5a588
  (fn [separ alist]
    (reduce #(concat (if (seq? %1) %1 (list %1)) [separ %2]) alist)))

(defcheck solution-4201229b
  (fn ite
    ([i lst] (ite [(first lst)] i (next lst)))
    ([l i lst]
     (if (empty? lst) l
                      (ite (conj l i (first lst)) i (next lst))))))

(defcheck solution-4267deb3
  (fn [i coll] (butlast (flatten (map #(list %1 i) coll)))))

(defcheck solution-4280e066
  (fn [x ys] (loop [yy ys erster true res nil]
               (if (empty? yy)
                 res
                 (if erster
                   (recur (rest yy) false (list (first yy)))
                   (recur (rest yy) false (concat res (list x (first yy)))))))))

(defcheck solution-432cdee9
  (fn [sep [x & xs]] (reduce #(conj %1 sep %2) [x] xs)))

(defcheck solution-436ee67e
  (fn __ [i coll]
    (rest (mapcat #(list i %) coll))))

(defcheck solution-4407aec7
  (fn [v s]
    (rest (mapcat #(list v %) s))))

(defcheck solution-442320d0
  (fn interpose-seq [y x]
    (if (empty? (rest x))
      x
      (concat (list (first x))
              (list y)
              (interpose-seq y (rest x))))))

(defcheck solution-4440e255
  (fn [val s]
    (-> val
      (repeat)
      (interleave s)
      (rest))))

(defcheck solution-4475123c
  (fn [sep coll] (drop-last (mapcat #(vector %1 sep) coll))))

(defcheck solution-449b1766
  (fn [sep col] (drop 1 (interleave (repeat sep) col))))

(defcheck solution-45275b85
  #(drop 1 (interleave (repeat %) %2)))

(defcheck solution-46d2f10
  (fn [y xs]
    (loop [xs xs
           acc []]
      (if-let [[x & more-xs] (seq xs)]
        (if (seq more-xs)
          (recur more-xs (conj acc x y))
          (recur more-xs (conj acc x)))
        acc))))

(defcheck solution-47019502
  (fn [inter sequencial]
    (loop [x 0
           res []]
      (if (>= x (count sequencial))
        (rest res)
        (recur (inc x) (conj (conj res inter) (nth sequencial x)))))))

(defcheck solution-471c8cbd
  (fn a [z [x & y]]
    (loop [s []]
      (if (empty? y)
        (conj s x)
        (concat (conj s x z) (a z y))))))

(defcheck solution-47e9dae3
  #(reduce concat [(first %2)] (map (fn [x] [%1 x]) (rest %2))))

(defcheck solution-48399053
  (fn [v s] (drop 1 (mapcat #(vector v %) s))))

(defcheck solution-4850c122
  (letfn [(intp [e s]
            (let [f (first s) r (next s)]
              (cond (empty? s) nil
                    (empty? r) (cons f nil)
                    :else (lazy-seq (cons f (cons e (intp e r))))
                    ))
            )] intp))

(defcheck solution-48fda66e
  (fn [sep l]
    (loop [cl nil
           m  l]
      (if (nil? cl)
        (recur [(first m)] (next m))
        (if (nil? m)
          cl
          (recur (conj (conj cl sep) (first m)) (next m)))))))

(defcheck solution-49751a40
  (fn

    [sep coll] (drop 1 (interleave (repeat sep) coll))))

(defcheck solution-4a3b11af
  (fn [s coll] (drop-last (flatten (for [x coll] [x s])))))

(defcheck solution-4a56999b
  (fn [separator colls] (drop-last (reduce #(conj %1 %2 separator) [] colls))))

(defcheck solution-4ac00247
  (fn [sep coll]
    (drop-last (mapcat vector coll (repeat sep)))))

(defcheck solution-4aeb6cfc
  #(drop-last (interleave %2 (repeat %))))

(defcheck solution-4afee6e8
  (fn inter[i s](if (second s) (concat [(first s) i] (inter i (rest s))) s)))

(defcheck solution-4b737b20
  (fn prob40 [n col]
    (loop [n n
           col col
           acc []]
      (if (= (count col) 1)
        (conj acc (first col))
        (recur n (rest col) (conj acc (first col) n))))))

(defcheck solution-4bddceff
  (fn interp [e, s]
    (if (seq s)
      (if (seq (rest s))
        (cons (first s) (cons e (interp e (rest s))))
        s
        ))))

(defcheck solution-4c3f28ad
  (fn peu [x y] (if (< (count y) 2) (seq y) (conj (peu x (rest y)) x (first y)))))

(defcheck solution-4d7a28a3
  (fn [sep coll]
    (reduce #(conj %1 sep %2) [(first coll)] (rest coll))))

(defcheck solution-4dfeb868
  #(drop 1 (mapcat list (repeat %) %2)))

(defcheck solution-4e0ce3c0
  (fn [a xs] (apply concat (for [x xs]
                             (if (= x (last xs)) [x] [x a])))))

(defcheck solution-4e1c96d8
  #(rest (for [v %2 x [% v]] x)))

(defcheck solution-4ea3dac0
  (fn inter [v s]
    (if (= 1 (count s))
      s
      (cons (first s) (cons v (lazy-seq (inter v (rest s))))))))

(defcheck solution-4ec0f849
  #(rest (mapcat (fn [x] [% x]) %2)))

(defcheck solution-4f057ae2
  #(butlast (mapcat (fn [item] [item %]) %2)))

(defcheck solution-4f2986e5
  (fn [x y]
    (rest (reduce #(conj (conj %1 x) %2) [] y))))

(defcheck solution-4f55fe60
  (fn [sep xs] (drop-last (mapcat list xs (repeat sep)))))

(defcheck solution-4f5fe784
  (fn [n l]
    (rest (mapcat
            #(list %1 %2)
            (repeat (count l) n)
            l
            ))
    ))

(defcheck solution-50570d39
  (fn [sep s]
    (rest (interleave (repeat sep) s))))

(defcheck solution-5069178e
  (fn ip [vl sq]
    (butlast (mapcat #(list % vl) sq))))

(defcheck solution-514eb34a
  (fn [i xs] (cons (first xs) (loop [xs (rest xs) acc []] (if (empty? xs) acc
                                                                          (recur (rest xs) (conj acc i (first xs))))))))

(defcheck solution-51819953
  (fn [val seq] (concat (interleave seq (take (dec (count seq)) (repeat val))) (list (last seq)))))

(defcheck solution-51850d21
  (fn [sep xs] (conj (mapcat (fn [x] [sep x]) (rest xs)) (first xs))))

(defcheck solution-51df7c11
  (fn ias[n s]
    (drop-last (interleave s (repeat n)))))

(defcheck solution-51fb062f
  (fn [a b] (butlast (mapcat #(vector % a) b))))

(defcheck solution-52245810
  (fn [separator xs]
    (butlast
      (interleave xs (repeat separator)))))

(defcheck solution-52aae862
  #(loop [result [] seq1 %2]
     (if (empty? seq1)
       result
       (recur (if (empty? result) (vector (first seq1)) (conj (conj result %1) (first seq1)))   (rest seq1))
       )
     ))

(defcheck solution-52db22b2
  (fn [sep coll] (butlast (mapcat (fn [e] (list e sep)) coll))))

(defcheck solution-52e63b16
  (fn [sep seq1]
    (loop [result [(first seq1)] elements (rest seq1)]
      (if (empty? elements)
        result
        (recur (conj (conj result sep) (first elements)) (rest elements))
        )
      )
    ))

(defcheck solution-52fe28c0
  (fn [x y]
    (drop-last (interleave y (repeat (count y) x)))))

(defcheck solution-530efee0
  #(drop-last (mapcat (fn [it] [it %1]) %2)))

(defcheck solution-532ef9f
  #(->> %2 (interleave (repeat %1)) rest))

(defcheck solution-53da4a28
  (fn [x s]
    (concat (interleave s (take (dec (count s)) (repeat x)))
            (list (last s)))))

(defcheck solution-540fcdec
  #(rest (interleave (repeat %1) %2)))

(defcheck solution-544ce7fb
  (fn my-interpose [val [x & xs]]
    (if (nil? xs)
      [x]
      (concat [x val] (my-interpose val xs)))))

(defcheck solution-54927a64
  (fn [d s]
    (rest (mapcat list (repeat d) s))))

(defcheck solution-54caf248
  (fn [x coll] (butlast (reduce concat (map (fn [item] [item x]) coll)))))

(defcheck solution-54e5ee61
  (fn i
    ([a b] (i a (first b) (rest b)))
    ([a c b]
     (lazy-seq
       (if (empty? b) [c] (concat [c a] (i a b)))))))

(defcheck solution-551e08ba
  (fn interpose2 [v col]
    (if (= (count col) 1)
      (list (first col))
      (conj (interpose2 v (rest col)) v (first col)))))

(defcheck solution-559ce16a
  (fn [x s] (drop-last(reduce #(conj %1 %2 x) [] s))))

(defcheck solution-56229d25
  (fn [sep xs]
    (drop-last (mapcat #(list % sep) xs))))

(defcheck solution-562db741
  (fn [s x]
    (loop [r (vector (first x)) x (rest x)]
      (if (nil? (first x))
        r
        (recur (concat r (vector s) (vector (first x))) (rest x))))))

(defcheck solution-573c0269
  (fn [sp xs]
    (reduce (fn [rs x] (conj (conj rs sp) x)) [(first xs)] (rest xs))))

(defcheck solution-577f893b
  (fn [x y]
    (drop-last (interleave y (repeat x)))))

(defcheck solution-57a9fd84
  (fn
    [elem seq]
    (loop [seq seq vec []]
      (if (> (count seq) 1)
        (recur (rest seq) (conj vec (first seq) elem))
        (conj vec (first seq)))
      )
    ))

(defcheck solution-5815d4a2
  (fn [sep s]
    (if (empty? s)
      s
      (loop [se (next s)
             res [(first s)]]
        (if (empty? se)
          res
          (recur
            (next se)
            (conj res sep (first se))))))))

(defcheck solution-582291d4
  (fn [i s]
    (rest (interleave (repeat i) s))))

(defcheck solution-5825d257
  (fn [sep coll]
    (drop 1 (flatten (map #(list sep %) coll)))))

(defcheck solution-58bac77e
  (fn [v xs]
    (butlast (mapcat #(list % v) xs))))

(defcheck solution-58c466e5
  #(butlast (mapcat (fn [x] (list x %)) %2)))

(defcheck solution-590f194b
  (fn [n s]
    (rest
      (apply concat (map list (repeat n) s)))))

(defcheck solution-5918f91d
  (fn [x xs] (butlast (mapcat #(vector % x) xs))))

(defcheck solution-5922b9d0
  (fn [ v s ] (drop-last (mapcat (fn [x] [x v]) s))))

(defcheck solution-598b5a5c
  (fn [inter s] (rest (interleave (repeat inter) s))))

(defcheck solution-5992cd7a
  (fn [x ys]
    (cons (first ys)
      (interleave (repeat x)
        (rest ys)))))

(defcheck solution-59cdcfbb
  (fn my-interpose [x coll]
    (if (empty? (rest coll))
      [(first coll)]
      (cons (first coll) (cons x (my-interpose x (rest coll)))))))

(defcheck solution-5bbcc9f3
  #(loop [x %2 y []]
     (if-not (= 1 (count x))
       (recur (rest x) (conj y (first x) %1))
       (conj y (first x)))))

(defcheck solution-5c623e3b
  (fn [v s] (butlast (interleave s (repeat v)))))

(defcheck solution-5c9c853b
  (fn [sep xs]
    (drop-last (mapcat list xs (repeat sep)))))

(defcheck solution-5cbc39f
  (fn [x coll]
    (rest (apply concat (for [i coll] [x i])))))

(defcheck solution-5ccd567e
  (fn [x [y & ys]] (reduce #(conj %1 x %2) [y] ys)))

(defcheck solution-5cea2545
  (fn [sp sq]
    (loop [isq sq rsq []]
      (if (empty? isq)
        (take (+ (count sq) (dec (count sq))) rsq)
        (recur (rest isq) (conj (conj rsq (first isq)) sp))))))

(defcheck solution-5d42a8ea
  (fn [ n coll] (butlast (interleave coll (repeat (count coll) n)))))

(defcheck solution-5d7f38d5
  (fn [sep xs]
    (reduce (fn [acc x] (conj acc sep x)) [(first xs)] (rest xs))))

(defcheck solution-5d972b35
  (fn [i, s]
    (butlast
      (interleave
        s
        (repeat (count s) i)))))

(defcheck solution-5e0fc6f9
  (fn inpos[v sx] (drop-last (mapcat #(list % v) sx))))

(defcheck solution-5e91d903
  (fn [x xs] (reduce #(conj (if (seq %) (conj % x) %) %2) [] xs)))

(defcheck solution-5ef45289
  #(rest (reduce concat (for [x %2] [% x]))))

(defcheck solution-5f355d50
  (fn [x coll]
    (into (empty coll)
      (take (- (* 2 (count coll)) 1)
        (flatten (map (fn [y] [y x]) coll))))))

(defcheck solution-5f64d940
  #(reduce (fn [a b] (conj (conj a %1) b)) [(first %2)] (rest %2)))

(defcheck solution-5f9133e4
  #(rest (mapcat list (repeat %1) %2)))

(defcheck solution-5fad3c2e
  (fn [s xs]
    (loop [xs xs accum []]
      (if (empty? xs)
        accum
        (let [new-accum (conj accum (first xs))
              new-xs (next xs)]
          (if new-xs (recur new-xs (conj new-accum s)) (recur new-xs new-accum)))))))

(defcheck solution-5fed70fc
  (fn [sep coll]
    (->>
      (repeat (count coll) sep )
      (interleave coll)
      (butlast)
      )))

(defcheck solution-6057342a
  #(butlast (interleave %2 (repeat (count %2) %1))))

(defcheck solution-608f34f1
  (fn ip [x coll]
    (if (= (rest coll) ())
      coll
      (concat [(first coll) x] (ip x (rest coll))))))

(defcheck solution-60e63445
  (fn [x coll]
    (drop 1 (interleave (repeat x) coll))))

(defcheck solution-61504752
  (fn myinterpose [x coll]
    (if (empty? (rest coll)) coll
                             (cons (first coll) (cons x (myinterpose x (rest coll)))))))

(defcheck solution-62265cf3
  (fn [x y] (drop 1 (reduce #(conj % x %2) [] y))))

(defcheck solution-62365ed
  (fn [v l]
    (reverse (reduce
               (fn [x y]
                 (cons y (cons v x))
                 )
               [(first l)]
               (rest l)
               ))
    ))

(defcheck solution-62430e5b
  (fn ips [separator, sequence] (drop-last (reduce #(conj % %2 separator) [] sequence))))

(defcheck solution-631405e6
  (fn [sep input]
    (loop [l input acc []]
      (cond (empty? l) acc
            (= (count l) 1) (conj acc (last l))
            :else (recur (rest l) (conj (conj acc (first l)) sep))))))

(defcheck solution-6315a043
  (fn interp [x xs]
    (-> (partial list x)
      (mapcat xs)
      rest)))

(defcheck solution-631f3d0
  (fn [i s] (butlast (mapcat #(list % i) s))))

(defcheck solution-63d906e1
  (fn [s l] (drop-last (mapcat #(list % s) l))))

(defcheck solution-63f65980
  (fn [x [f & r]]
    (concat (list f) (mapcat #(list x %) r))))

(defcheck solution-643dbf1f
  (fn i [v s]
    (if (> (count s) 1)
      (conj (i v (rest s)) v (first s))
      s)))

(defcheck solution-64d52cdd
  #(rest (flatten (map (partial vector %1) %2))))

(defcheck solution-6539122c
  (fn interpose2 [div coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (if (= (count s) 1)
          s
          (cons (first s)
            (cons div
              (interpose2 div (rest coll)))))))))

(defcheck solution-653987dc
  (fn[n s](
            butlast (mapcat (fn[a][a n]) s)

            )))

(defcheck solution-65a17153
  (fn [y xs]
    (butlast (interleave xs (repeat y)))))

(defcheck solution-65c88905
  (fn my-interp [item coll]
    (if (< (count coll) 1)
      coll
      (let [s (seq coll)]
        (cons (first s)
          (for [x (rest s)
                y [item x]]
            y))))))

(defcheck solution-65e02031
  (fn [x xs] (next (interleave (repeat x) xs))))

(defcheck solution-6697cb50
  (fn ntrps [x coll]
    (butlast
      (mapcat #(vector % x) coll)
      )))

(defcheck solution-66991a44
  (fn [sep xs]
    (rest (mapcat (fn [x] [sep x]) xs))))

(defcheck solution-67232e94
  (fn [x s] (cons (first s)
              (mapcat #(vector x %) (rest s)))))

(defcheck solution-6738a95e
  #(next (interleave (repeat %1) %2)))

(defcheck solution-67d75dc8
  #(drop 1 (mapcat (partial list %1) %2)))

(defcheck solution-68258d4d
  #(take (dec (* 2 (count %2)))
     (mapcat (fn[x] (list x %)) %2)))

(defcheck solution-68e34f9d
  (fn [n collect ]
    (loop [col collect ,a n ,result []   ]
      (if (empty?(rest col))
        (conj result (first col) )
        (recur (rest col)
          a
          (conj (conj result (first col) )
            a)
          )
        )
      )
    ))

(defcheck solution-696e5453
  #(cons (first %2) (mapcat list (repeat %) (rest %2))))

(defcheck solution-6aed5901
  (fn [sep coll]
    (cons (first coll)
      (mapcat #(list sep %) (rest coll)))))

(defcheck solution-6b3a43a7
  (fn [sep coll]
    (reduce #(concat %1 (list sep %2)) (list (first coll)) (rest coll))))

(defcheck solution-6b8f5ad2
  (fn my-interpose
    [v s]
    (cond
      (empty? s) '()
      (empty? (rest s)) s
      :else (cons (first s) (cons v (my-interpose v (rest s)))))))

(defcheck solution-6be607c9
  #((comp rest interleave) (repeat %) %2))

(defcheck solution-6c42eaa5
  (fn seperate [sep items]
    (if (= (count items) 1)
      (list (last items))
      (conj (seperate sep (rest items)) sep (first items)))))

(defcheck solution-6c604b83
  (fn [sep col]
    (drop-last (reduce (fn [ret this]
                         (conj ret (first this) (second this)))
                 [] (map (fn [e] [e sep]) col) ))))

(defcheck solution-6cb4001f
  (fn ipos [x c]
    (if (empty? (rest c)) c
                          (cons
                            (first c)
                            (cons x
                              (lazy-seq (ipos x (rest c))))))))

(defcheck solution-6cde51e3
  (fn [val [h & xs]] (conj (mapcat #(list val %) xs) h)))

(defcheck solution-6ce51f0f
  (fn myinterpose
    [c myseq]
    (loop
     [myrest myseq myres nil]
      (if (empty? (rest myrest))
        (concat myres (list (first myrest)))
        (recur (rest myrest) (concat myres (list (first myrest) c)))))))

(defcheck solution-6d1b719f
  #(rest (mapcat (partial list %1) %2)))

(defcheck solution-6dae1683
  (fn [a s] (rest (mapcat vector (repeat a) s))))

(defcheck solution-6dc6e570
  (comp #(drop 1 %) (fn [sep coll] (mapcat (fn [elt] [sep elt]) coll))))

(defcheck solution-6ddb8d2a
  (fn pose [e s]
    (if (= 1 (count s)) s
                        (cons (first s) (cons e (pose e (rest s)))))))

(defcheck solution-6e1fe80c
  (fn [el s]
    (rest
      (interleave
        (repeat (inc (count s)) el)
        s))))

(defcheck solution-6ef4df21
  (fn my-interpose
    [e s]
    (take (dec (* 2 (count s))) (flatten (map list s (repeat e))))))

(defcheck solution-6f2934d5
  (fn [y xs]
    (concat
     (apply concat
       (map #(list % y) (drop-last xs)))
     (list (last xs)))))

(defcheck solution-6f77595
  (fn [in coll]
    (cons (first coll)
      (mapcat #(list in %)
        (next coll)))))

(defcheck solution-6f914e26
  (fn k-interpose [delimiter sequence]
    (if (seq sequence)
      (let [[f & r] sequence]
        (if (seq r)
          (lazy-seq (concat [f delimiter] (k-interpose delimiter r)))
          [f]))
      [])))

(defcheck solution-6fd02b10
  #(butlast (interleave %2 (repeat %)) ))

(defcheck solution-703a2e60
  (fn [p l]
    (pop (reduce #(conj %1 %2 p) [] l))))

(defcheck solution-704dd70d
  (fn [delim seq]
    (butlast (mapcat #(list % delim) seq))))

(defcheck solution-709c8c86
  (fn [s c]
    (butlast (reverse
               (reduce #(conj %1 %2 s) '() c)))))

(defcheck solution-70cb6adf
  (fn [x xs] (butlast (mapcat #(list % x) xs))))

(defcheck solution-71056112
  (fn [sep coll]
    (drop-last (mapcat vector coll (repeat sep)))))

(defcheck solution-71d32b45
  (fn [v xs] (butlast (interleave xs (repeat v)))))

(defcheck solution-72816da3
  (fn [del seq] (reverse (reduce #(cons %2 (cons del %1)) (list (first seq)) (rest seq)))))

(defcheck solution-72ca4d2e
  (fn [v xs]
    (drop-last (mapcat #(list %1 v) xs))))

(defcheck solution-73722f99
  (fn [sep xs]
    (loop [acc [(first xs)]
           left (rest xs)]
      (if (seq left)
        (recur (conj acc sep (first left)) (rest left))
        acc))))

(defcheck solution-739bc44c
  (fn [x y]
    (rest (mapcat #(list x %) y))))

(defcheck solution-739e8e9e
  (fn [sep l] (flatten (reduce #(list %1 sep %2) l))))

(defcheck solution-73d52ed6
  (fn [x s] (butlast (mapcat #(list % x) s))))

(defcheck solution-73dc2250
  #(reduce (fn [a b] (conj (if (coll? a) a [a]) % b)) %2))

(defcheck solution-7408a446
  (fn inter [v xs]
    (if (empty? (rest xs))
      xs
      (cons (first xs) (cons v (inter v (rest xs)))))))

(defcheck solution-740dbd97
  #(butlast (interleave  %2 (repeat %1))))

(defcheck solution-743de07c
  #_(fn interpose' [interposer coll]
      (reduce (fn [accum item]
                (conj accum interposer item))
        [(first coll)]
        (rest coll)))

  (fn il [n xs]
    (butlast (interleave xs (take (count xs) (repeat n))))))

(defcheck solution-74a9bfe5
  (fn [i lst] (rest (mapcat list (repeat i) lst))))

(defcheck solution-758501e3
  (fn [x sq] (drop-last (interleave sq (repeat x)))))

(defcheck solution-765fb68
  (fn [a b] (let [c (repeat (count b) a)] (butlast (mapcat list b c)))))

(defcheck solution-76fd9b46
  (fn [c s]
    (rest (interleave (repeat (count s) c) s))))

(defcheck solution-77be4d94
  (fn ip [v l]
    (if (= 1 (count l))
      l
      (into [(first l) v] (ip v (rest l))))))

(defcheck solution-78254d5e
  (fn [sep a] (rest (mapcat #(list sep %) a) )))

(defcheck solution-782a2bf6
  (fn my-interpose [sep s]
    (cond (empty? s) []
          (empty? (rest s)) [(first s)]
          :else (concat (list (first s) sep)
                        (my-interpose sep (rest s))))))

(defcheck solution-782bb109
  (fn [sep seq]
    (rest
      (loop [result [] items seq]
        (if (empty? items)
          result
          (recur (conj result sep (first items)) (rest items))
          )
        )
      )
    ))

(defcheck solution-78c7192d
  (fn [a b] (rest (mapcat (fn [x] [a x]) b))))

(defcheck solution-796800d4
  (fn [n s] (into [] (take (- (* 2 (count s)) 1) (interleave s (repeat (count s) n))))))

(defcheck solution-799d9d81
  (fn [sep coll]
    (loop [r coll, n []]
      (if (empty? r)
        (drop-last n)
        (recur (rest r) (conj n (first r) sep))))))

(defcheck solution-7aea4c65
  (fn [x y]
    (butlast (mapcat #(list % x) y))))

(defcheck solution-7b0a0790
  (fn inter [mid coll]
    (let [x (first coll)
          xs (next coll)]
      (if xs (into [x mid] (inter mid xs)) [ x]))
    ))

(defcheck solution-7b1a1514
  (fn interpose-alt [x coll]
    (flatten (map #(concat `(~%) `(~(if (= % (last coll))
                                      ()
                                      x)))
               coll))))

(defcheck solution-7de2bc0a
  (fn [i s] (concat (apply concat (map (fn [a] [a i]) (butlast s))) [(last s)])))

(defcheck solution-7e0aeb56
  (fn [x coll] (rest (interleave (repeat x) coll))))

(defcheck solution-7e1fab98
  (fn number40 [delim xs]
    (->> (repeat delim)
      (interleave xs)
      (butlast))))

(defcheck solution-7ee387a1
  (fn meu-interpose [v coll]
    (cond
      (empty? coll) nil
      (= 2 (count coll)) (list (first coll) v (second coll))
      :else (concat (list (first coll) v) (meu-interpose v (next coll))))))

(defcheck solution-8028fbeb
  (fn [s v] (butlast (apply concat (map #(vector % s) v)))))

(defcheck solution-80572b20
  (fn [v s]
    (let [n (dec (count s))]
      (conj (vec (apply concat (map #(list % v) (take n s)))) (last s)))))

(defcheck solution-806a5973
  (fn [x coll] (butlast (mapcat #(list % x) coll))))

(defcheck solution-807468e2
  (fn [sep s]
    (loop [output []
           sepnext false
           remain s]
      (if (empty? remain) output
                          (if sepnext
                            (recur (concat output [sep]) false remain)
                            (recur (concat output [(first remain)]) true (rest remain)))))))

(defcheck solution-8080747c
  (fn my-interpose [s lst]
    (rest
      (reverse
        (reduce (fn [xs x] (conj xs s x))
          '()
          lst)))))

(defcheck solution-815303df
  (fn [x coll]
    (butlast (interleave coll (repeat x)))))

(defcheck solution-8167dd30
  (fn foo [v xs]
    (reverse
      (loop [xs (seq xs) first-pass true acc ()]
        (if (empty? xs)
          acc
          (recur (rest xs) false
            (if first-pass (list* (first xs) acc)
                           (list* (first xs) v acc))))))))

(defcheck solution-81b3c67b
  #(reduce (fn [result x] (conj result %1 x)) [(first %2)] (rest %2)))

(defcheck solution-81dd8daa
  (fn [x c]
    (butlast (mapcat identity (for [y c] [y x])))))

(defcheck solution-82bcf106
  (fn [i xs]
    (cons (first xs) (mapcat list (repeat i) (rest xs)))))

(defcheck solution-83525484
  (fn my [val col]
    (if (= 1 (count col))
      (list (first col))
      (concat (list (first col) val) (my val (rest col))))))

(defcheck solution-83d44212
  (fn [p coll]
    (butlast (mapcat list coll (repeat p)))))

(defcheck solution-83ddd0a7
  #(rest (mapcat (partial conj [%]) %2)))

(defcheck solution-84cfb089
  #(butlast (interleave %2 (repeat %))))

(defcheck solution-84e129c2
  #(butlast (mapcat (fn [x] [x %]) %2)))

(defcheck solution-84eb0e2f
  (fn _interpose [c s]
    (if (= (count s) 1)
      s
      (concat (vector (first s)) (vector c) (_interpose c (rest s))))))

(defcheck solution-84fe0014
  #(into [] (drop-last 1 (mapcat (fn [x] (list x %)) %2))))

(defcheck solution-85070ce2
  (fn [x col] (butlast (mapcat #(list % x) col ))))

(defcheck solution-87b1eac7
  (fn [x coll] (drop-last (reduce (fn [acc x'] (concat acc [x' x])) [] coll))))

(defcheck solution-87e9f538
  #(drop-last (interleave %2 (repeat (count %2) %1))))

(defcheck solution-886833d2
  (fn [i c] (rest (mapcat list (repeat i) c))))

(defcheck solution-8900b7f9
  (fn [v vs] (rest (reverse (reduce #(cons %2 (cons v %1)) [] vs)))))

(defcheck solution-891018b3
  (fn ipose [sep s]
    (if (= 1 (count s))
      s
      (concat (list (first s) sep) (ipose sep (rest s))))))

(defcheck solution-89b5b316
  #(conj ( mapcat (partial vector %) (next %2)) (first %2)))

(defcheck solution-89b8f0ec
  (fn [x coll] (butlast (mapcat vector coll (repeat x)))))

(defcheck solution-89c2d93e
  (fn [n coll] (take (- (* (count coll) 2) 1) (mapcat (fn [e] [e n]) coll))))

(defcheck solution-89cd7953
  (fn _interpose [value items]
    (if (= 1 (count items))
      items
      (into [(first items) value] (_interpose value (rest items))))))

(defcheck solution-89e62339
  (fn [x xs] (if (empty? xs) '() (conj (mapcat #(list x %1) (rest xs)) (first xs)))))

(defcheck solution-8ab1e57a
  (fn [x xs] (rest (interleave (repeat x) xs))))

(defcheck solution-8ad9a4ac
  (fn [e s] (pop (reduce #(conj % %2 e) [] s))))

(defcheck solution-8addffa4
  (fn i [d [x & xs]](lazy-seq (cons x (when (seq xs) (cons d (i d xs)))))))

(defcheck solution-8b337ec7
  (fn [c col]
    (apply vector
      (drop-last
        (mapcat (fn [a] (list a c)) col)))))

(defcheck solution-8b4b51c2
  (fn i[x [y & ys]]
    (if ys
      (conj (i x ys) x y)
      (list y))))

(defcheck solution-8b4e18c2
  (fn [x y ]
    (let [ f (fn [ r i ]
               (if (= nil (first r ))
                 (vector i)
                 (conj r x i )
                 )
               )
          ]
      (reduce f [] y)
      )
    ))

(defcheck solution-8bf7b6e5
  (fn interp [elem s2]
    (loop [result '()
           e elem
           cs2 s2]
      (if (empty? cs2)
        (butlast result)
        (recur (concat result (list (first cs2) elem)) elem (rest cs2))))))

(defcheck solution-8cb1ce42
  (fn [v xs]
    (drop 1 (for [x xs u [v x]] u))))

(defcheck solution-8cbe956e
  (let [inter
        (fn [value coll result]
          (if (= 1 (count coll))
            (conj result (first coll))
            (recur value (next coll) (conj result (first coll) value))
            )
          )]
    (fn [value coll] (inter value coll []))
    ))

(defcheck solution-8da4e651
  (fn [s coll]
    (drop-last (mapcat list coll (take (count coll) (repeat s))))))

(defcheck solution-8dd8e828
  (fn [x coll] (rest (mapcat #(list x %) coll))))

(defcheck solution-8df8d86a
  (fn f [i l] (if (< (count l) 2) l (conj (f i (rest l)) i (first l))  )))

(defcheck solution-8e226a2b
  (fn ip [x s]
    (cond
      (empty? s) ()
      (empty? (rest s)) (list (first s))
      :else (cons (first s) (cons x (ip x (rest s)))))))

(defcheck solution-8ecf744f
  #(butlast (for [x %2 y [x %]] y)))

(defcheck solution-8ee563b1
  (fn [a b]
    (loop [b b r []]
      (if (= 1 (count b)) (concat r b)
                          (recur (rest b) (conj r (first b) a))))))

(defcheck solution-8f13f9c8
  #(loop [ cl %2, res []]
     (if (empty? (rest cl)) (conj res (first cl))
                            (recur (rest cl) (conj (conj res (first cl)) %1))
                            )))

(defcheck solution-8f52b67f
  (fn [v xs]
    (rest (mapcat #(list v %) xs))))

(defcheck solution-8faf3df7
  #(reduce (fn [ac v] (conj (conj ac %) v)) [(first %2)] (next %2)))

(defcheck solution-9001989c
  (fn [x coll]
    (butlast
      (interleave coll (repeat (count coll) x)))))

(defcheck solution-9052bf7c
  (fn [n xs]
    (if-not (empty? xs)
      (cons (first xs)
        (apply concat
          (for [x' (rest xs)]
            [n x']))))))

(defcheck solution-90b7ce96
  #(reduce (fn[x y] (if(empty? x) (conj x y) (conj x %1 y))) [] %2))

(defcheck solution-90ff38d8
  (fn nterpose
    ([val [fs & rs]]
     (nterpose val rs (list fs)))
    ([val [fs & rs] res]
     (if (nil? fs)
       res
       (recur val rs (concat res (list val fs)))))))

(defcheck solution-9156cf5e
  (fn [i s]
    (butlast (mapcat vector s (repeat i)))))

(defcheck solution-91cf8788
  (fn [n x] (rest (flatten (map vector (repeat n) x)))))

(defcheck solution-91ee294e
  #(next(mapcat (fn[x][% x]) %2)))

(defcheck solution-91f1a89b
  #(take (dec (* (count %2) 2)) (interleave %2 (repeat %))))

(defcheck solution-925254c2
  #(take (- (* 2 (count %2)) 1)
     (interleave %2 (repeat %1))))

(defcheck solution-92940e50
  (fn [j s] (butlast (interleave s (repeat (count s) j)))))

(defcheck solution-929488fe
  (fn [sep xs]
    (cons (first xs)
      (mapcat (partial list sep) (rest xs)))))

(defcheck solution-9385a422
  (fn [n xs]
    (butlast (flatten (map #(list %1 n) xs)))))

(defcheck solution-94838c15
  (fn [n x]
    (drop-last (mapcat (fn [i]
                         (list i n)) x))
    ))

(defcheck solution-94a7a6bd
  (fn [v s]
    (loop [x s
           acc []]
      (if (nil? x)
        (butlast acc)
        (recur (next x) (-> acc (conj (first x)) (conj v)))))))

(defcheck solution-959f65d8
  (fn intp [sep, xs]
    (if (nil? (second xs))
      [(first xs)]
      (cons (first xs)
        (cons sep (intp sep (rest xs)))))))

(defcheck solution-95a5f587
  #(loop [[head & tail] %2
          acc nil]
     (if (nil? head)
       acc
       (recur tail
         (if (nil? acc)
           [head]
           (conj acc %1 head)
           )
         )
       )
     ))

(defcheck solution-964cbc93
  (fn [inter col] (flatten (reduce #(list %1 inter %2) col))))

(defcheck solution-964fba5c
  (fn f [sep [x & xs]]
    (if (seq xs)
      (cons x (cons sep (f sep xs)))
      [x])))

(defcheck solution-9667c7ce
  #(conj (vec (mapcat vector (butlast %2) (repeat %))) (last %2)))

(defcheck solution-96d32673
  (fn [delimiter coll]
    (let [accum []]
      (into []
        (rest
          (mapcat #(conj accum %1 %2)
            ((fn delimiter-seq [delimiter]
               (lazy-seq
                 (cons delimiter (delimiter-seq delimiter)))) delimiter)
            coll))))))

(defcheck solution-96dc8fba
  #(rest (interleave (repeat %) %2)))

(defcheck solution-96e5b767
  (fn [x y]
    (butlast(reduce #(conj %1 %2 x) [] y))))

(defcheck solution-97227306
  (fn f [x y]
    (if (empty? (rest y)) [(first y)]
                          (concat [(first y) x] (f x (rest y))))))

(defcheck solution-9738e45b
  (fn [s col]
    (butlast (apply concat (for [i col] (list i s))))))

(defcheck solution-97a965e7
  #(rest (mapcat vector (repeat %1) %2)))

(defcheck solution-97dda504
  (fn [sep coll] (drop 1 (mapcat #(vector %1 %2) (repeat sep) coll))))

(defcheck solution-986f03e3
  #(rest (mapcat list (repeat %) %2)))

(defcheck solution-9896fb5c
  (fn [sep seq]
    (concat
     (list (first seq))
     (apply concat (map #(list sep %) (rest seq))))))

(defcheck solution-99a261a1
  ; would be shorter if you used mapcat
  (fn [glue [head & tail]] (concat [head] (interleave (repeat glue) tail))))

(defcheck solution-9a0f3d45
  #(rest (flatten (for [p (vector %) s %2] [p s]))))

(defcheck solution-9a11a6bc
  (fn interpose-seq [i s] (rest (interleave (repeat i) s))))

(defcheck solution-9a44afb8
  #(butlast(mapcat list %2(repeat %1))))

(defcheck solution-9aa2800b
  (fn [delim vctr]
    (loop [sq (rest vctr)
           return (vector (first vctr))]
      (if (nil? (first sq))
        return
        (recur (rest sq)
          (conj return
            delim
            (first sq)))))))

(defcheck solution-9aedb794
  (fn [i s] (rest (reduce #(conj %1 i %2) '[] s))))

(defcheck solution-9b09806c
  (fn [sep s] (drop-last (mapcat #(list %1 %2) s (repeat sep)))))

(defcheck solution-9b5e8cde
  (fn [x coll]
    (rest (mapcat #(conj (list %1) x) coll))))

(defcheck solution-9bcf017b
  #(rest (for [x %2 y [% x]] y)))

(defcheck solution-9beede41
  (fn [v s] (drop-last (interleave s (repeat v)))))

(defcheck solution-9c5e56f7
  (fn [x y]
    (loop [tot '() x x y y]
      (if (empty? y)
        (drop-last (reverse tot))
        (recur (conj tot (first y) x) x (rest y))))))

(defcheck solution-9c660285
  (fn new-interpose
    [x s]
    (vec (cons (first s) (mapcat #(vec [x %]) (rest s))))))

(defcheck solution-9cd17b21
  (fn [separator coll] (drop-last (reduce concat (map (fn [x] [x separator]) coll)))))

(defcheck solution-9d2b4d05
  #(rest (mapcat list (repeat (count %2) %1) %2)))

(defcheck solution-9d5ead12
  (fn [sep coll] (butlast (interleave coll (repeat sep)))))

(defcheck solution-9d76e20d
  (fn [n cs] (if (or (empty? cs) (empty? (rest cs))) cs (into (conj (empty cs) (first cs))
                                                          (reduce
                                                            (fn [acc b]
                                                              (conj acc n b))
                                                            (empty cs)
                                                            (rest cs)
                                                            )) )))

(defcheck solution-9dbe785b
  #(drop-last (mapcat vector %2 (repeat (count %2) %1))))

(defcheck solution-9e51db1
  (fn [val v] (conj (vec ( (fn seq-inter [x y]
                             (if (or (empty? x) (empty? y))
                               '()
                               (cons
                                 (first x)
                                 (cons
                                   (first y)
                                   (seq-inter (rest x) (rest y)))))) (butlast v) (repeat val))) (last v))))

(defcheck solution-9ea1b1d1
  (fn [a b] (rest (mapcat #(list a %) b))))

(defcheck solution-9ecf2545
  (fn [br xs] (drop-last (mapcat #(list % br) xs))))

(defcheck solution-9eeeaf85
  (fn [inter input]
    (loop [remaining input ans []]
      (if (empty? remaining)
        (butlast ans)
        (recur (rest remaining) (conj ans (first remaining) inter))))))

(defcheck solution-9efe3632
  (fn [x y] (drop-last (interleave y (iterate (fn [z] z) x)))))

(defcheck solution-9f49c333
  (fn [e coll] (drop-last (flatten (map #(vector % e) coll)))))

(defcheck solution-9f553f2e
  #(drop-last(interleave %2 (repeat (count %2) %1))))

(defcheck solution-9f5d1f6
  (fn [v coll]
    (loop [coll coll
           r '()]
      (if (empty? coll)
        (reverse (rest r))
        (recur (rest coll) (conj r (first coll) v))))))

(defcheck solution-a0289954
  (fn [j s]
    (reverse
      (rest
        (reverse
          (interleave s (repeat j)))))))

(defcheck solution-a02aec96
  (fn [sep coll]
    (drop-last (mapcat vector coll (repeat sep)))))

(defcheck solution-a131bd30
  (fn poser [sep items]
    (let [f (first items) r (rest items)]
      (cons f (when (not-empty r) (cons sep (poser sep r)))))))

(defcheck solution-a1972e1
  (fn [d s] (butlast (mapcat #(list % d) s))))

(defcheck solution-a1ae55c1
  (fn [s c] (butlast (interleave c (repeat s)))))

(defcheck solution-a2a76a7a
  #(drop-last (flatten (map (fn [it] [it %1] ) %2))))

(defcheck solution-a314e8ca
  (fn pose [x ls]
    (if (<= (count ls) 1)
      ls
      (cons (first ls)
        (cons x
          (pose x (rest ls))
          )
        )
      )
    ))

(defcheck solution-a332b7f3
  #(loop [v [(first %2)]
          r (rest %2)]
     (if (empty? r)
       v
       (recur (conj v %1 (first r)) (rest r)))))

(defcheck solution-a34e4b22
  #(cons (first %2)
     (mapcat list
       (repeat (count (rest %2)) %1)
       (rest %2))))

(defcheck solution-a37ebd0c
  (fn [x ys]
    (let [c (count ys)]
      (take (- (* 2 c) 1) (interleave ys (repeat c x))))))

(defcheck solution-a3e6d7d8
  (fn [delim xs] (drop-last (interleave xs (repeat delim)))))

(defcheck solution-a3fa6514
  #(cons (first %2) (interleave (repeat %1) (rest %2))))

(defcheck solution-a44adc65
  (partial
    #(if (empty? %3) (drop-last 1 %1)
                     (recur (concat %1 [(first %3) %2]) %2 (rest %3)))'()))

(defcheck solution-a450e772
  (fn [sep xs] (rest ( mapcat #(list sep %) xs))))

(defcheck solution-a477a0d3
  #(drop 1 (into [] (flatten (map vector (repeat (count %2) %1) %2)))))

(defcheck solution-a4b22c8d
  #(rest (mapcat (fn [a] [%1 a]) %2)))

(defcheck solution-a4ce8166
  (fn [s xs] (reduce #(conj %1 s %2) [(first xs)] (rest xs))))

(defcheck solution-a5574039
  (fn [sep coll]
    (rest (interleave (repeat sep) coll))))

(defcheck solution-a5641691
  (fn my-interpose [v coll]
    (lazy-seq
      (let [[fst & rst] (seq coll)]
        (cons fst
          (when rst
            (cons v (my-interpose v rst))))))))

(defcheck solution-a591365d
  #(butlast (reduce (fn[acc x](conj acc x %)) [] %2)))

(defcheck solution-a5ebe1d5
  (fn [j [x & xs]] (reduce #(concat %1 [j %2]) [x] xs)))

(defcheck solution-a62b8438
  (fn [x coll] (drop-last (mapcat #(list % x) coll))))

(defcheck solution-a698fe6e
  (fn [sep items] (butlast (mapcat vector items (repeat sep)))))

(defcheck solution-a69a3851
  (fn [insert seq]
    (butlast (mapcat #(list % insert) seq))))

(defcheck solution-a6caa39e
  (fn [sep coll]
    (drop-last (mapcat #(cons % [sep]) coll))))

(defcheck solution-a80c318e
  (fn comb [s x] (butlast (interleave x (repeat (count x) s)))))

(defcheck solution-a9136762
  #(reduce
     (fn [accum value]
       (concat accum [%1 value])
       ) [(first %2)] (rest %2)
     ))

(defcheck solution-a93ab6d4
  #(next (mapcat (fn [x] [% x]) %2)))

(defcheck solution-a950552d
  (fn [value coll]
    (conj
      (vec
        (interleave
          coll
          (repeat
            (- (count coll) 1)
            value)))
      (last coll))))

(defcheck solution-a9983bd7
  (fn [x col]
    (loop [result []  c1 col]
      (if (empty? c1)
        result
        (recur (if (empty? (rest c1)) (concat result [(first c1)] ) (concat result [(first c1) x] ))
          (rest c1)
          )))))

(defcheck solution-a9b3b392
  #(-> (interleave %2 (repeat %1)) (drop-last) (vec)))

(defcheck solution-aa42fd79
  #(drop-last 1 (interleave %2 (repeat %))))

(defcheck solution-aa86bd19
  (fn [v s] (drop-last (mapcat #(list % v) s))))

(defcheck solution-aadddb4e
  (fn [replace lst]
    (let [[h & t] (reverse (flatten (map (fn [a] (list a replace)) lst)))]
      (reverse t))))

(defcheck solution-ab2c58ee
  (fn replic [sprt seqn]
    (butlast(flatten (map #(list %1 sprt) seqn)))))

(defcheck solution-ab5b713
  (fn my-interpose [a b]
    (let [[x & xs] b]
      (if (empty? xs) [x] (concat [x a] (my-interpose a xs))))))

(defcheck solution-abeb31c1
  (fn [v l] (butlast (interleave l (repeat v)))))

(defcheck solution-abf4ad4d
  (fn [s, l]
    (butlast (reduce #(conj %1 %2 s) [] l))))

(defcheck solution-abff9635
  (fn [sep [f & r]] (cons f (flatten (map #(list sep %) r)))))

(defcheck solution-ac0617a2
  #(rest
     (apply concat
       (map (fn [x] [%1 x]) %2))))

(defcheck solution-ac8ff30e
  ;(fn selip
  ;  [selipan [x & xs]]
  ;  (if (empty? xs) [x]
  ;    (concat [x selipan] (selip selipan xs))))

  (fn [n coll] (rest (mapcat #(conj [n] %) coll))))

(defcheck solution-acb784e
  #(drop-last (mapcat list %2 (repeat (count %2) %))))

(defcheck solution-acd9d2e4
  #(drop 1 (interleave (repeat %1) %2)))

(defcheck solution-ae2c340
  (fn [d v]
    (reduce (fn [a x] (conj (conj a d) x)) [(first v)] (rest v))))

(defcheck solution-af3013bd
  #(drop-last (reduce (fn [acc val] (concat  acc [val %1])) []  %2)))

(defcheck solution-afab89f2
  (fn [sep xs]
    ((comp drop-last interleave) xs (repeat sep))))

(defcheck solution-afd33ce1
  (fn [d x] (rest (mapcat (fn [xx] [d xx]) x))))

(defcheck solution-afde19f7
  (fn [in coll]
    (->> (repeat in)
      (interleave coll)
      butlast)))

(defcheck solution-b06232c4
  (fn [value, xseq]
    (drop-last
      (into []
        (flatten
          (mapv vector xseq
            (into [] (take (count xseq) (repeat value)))))))))

(defcheck solution-b0975dfc
  (fn [x s]
    (butlast (interleave s (repeat x)))))

(defcheck solution-b0c074ce
  (fn
    [n coll]
    (drop 1 (mapcat #(vector %1 %2) (repeat n) coll))
    ))

(defcheck solution-b0f6e2e
  #(drop-last 1 (interleave %2 (repeat %1))))

(defcheck solution-b2031088
  (fn [sep coll] (rest (flatten (map #(vector sep %) coll)))))

(defcheck solution-b233d302
  (fn inter [sep c]
    (rest (mapcat (fn [e] [sep e]) c))))

(defcheck solution-b319ca4a
  (fn [v _seq]
    (take (dec (* (count _seq) 2)) (reverse (reduce (fn[result head]
                                                      (conj result head v)) '() _seq)))))

(defcheck solution-b3246c2c
  (fn [inter se] (butlast (interleave se (repeat inter)))))

(defcheck solution-b361d6f0
  (fn [sep coll]
    (loop [[head & tail] coll
           curr-coll []]
      (cond
        (empty? tail)
        (conj curr-coll head)
        :else
        (recur tail (conj curr-coll head sep))))))

(defcheck solution-b3691005
  (fn [v c] (butlast (mapcat #(list %1 v) c))))

(defcheck solution-b3a2dfe4
  (fn [s c] (pop (vec (mapcat #(list % s) c)))))

(defcheck solution-b478e7d2
  #(->> % repeat (map vector %2) flatten drop-last))

(defcheck solution-b53dafdd
  (fn [sep coll]
    (rest (flatten (map (fn [x] [sep x]) coll)))))

(defcheck solution-b569ff49
  (fn in [e [f & n]]
    (if (empty? n)
      [f]
      (concat [f e] (in e n)))))

(defcheck solution-b6543669
  (fn test [x y]
    (cons (first y) (mapcat #(cons x [%]) (rest y)))))

(defcheck solution-b68eaf06
  (fn [delim [f & col]]
    (reduce (fn [res el] (conj res delim el)) [f] col)
    ))

(defcheck solution-b729218d
  (fn [x s]
    (loop [s_ s ret '()]
      (if s_
        (let [n (next s_)]
          (if n
            (recur n (conj ret (first s_) x))
            (recur n (conj ret (first s_)))))
        (into '() ret)))))

(defcheck solution-b7699a5c
  (fn [v s]
    (reduce #(concat %1 (list v %2)) (list (first s)) (rest s))))

(defcheck solution-b7d2ba00
  (fn [e l]
    (loop [l l r []]
      (if (empty? l)
        (drop-last 1 r)
        (recur (rest l) (conj r (first l) e))))))

(defcheck solution-b82dc2b4
  (fn [sep seq] (rest (flatten (for [x seq] (list sep x))))))

(defcheck solution-b84fb5de
  (fn [s l] (rest (mapcat #(list s %) l))))

(defcheck solution-b8568851
  (fn [item lst] (rest (mapcat #(list item %) lst))))

(defcheck solution-b86fdbea
  (fn [v coll]
    (when-let [s (seq coll)]
      (conj
        (mapcat #(vector v %1) (rest coll))
        (first coll)
        )
      )
    ))

(defcheck solution-b875d74f
  #(->> %2
     (interleave (repeat %1))
     (rest)))

(defcheck solution-b8cc707a
  (fn my_interpose [c s] (if (<= (count s) 1) s (conj (conj (my_interpose c (rest s)) c) (first s)))))

(defcheck solution-b90696e6
  (fn[x y] (rest (reduce #(conj %1 x %2) [] y))))

(defcheck solution-b9115bd0
  #(reduce (fn [v x] (conj v %1 x)) [(first %2)] (rest %2)))

(defcheck solution-b9742600
  (fn [sep string] (rest (mapcat #(list sep %1) string))))

(defcheck solution-b9a8747c
  (fn myInterpose [sep coll] (drop-last (mapcat #(vector % sep) coll))))

(defcheck solution-ba0be960
  ;;(fn [v [h & tail]] (reduce concat [h] (map #(vec [v %]) tail)))
  (fn [v coll] (drop-last (interleave coll (repeat (count coll) v)))))

(defcheck solution-bae3e5b3
  (fn [i coll]
    (rest (interleave (repeat i) coll))))

(defcheck solution-baf83ff0
  (fn my-interpose [sep coll]
    (let [c (first coll)]
      (if (second coll)
        (lazy-seq (cons c (cons sep (my-interpose sep (rest coll)))))
        [c]))))

(defcheck solution-bb19c84b
  (fn [s coll] ((comp reverse rest reverse) (mapcat #(vector % s) coll))))

(defcheck solution-bb6406df
  (fn [sep seq-items]
    (loop [items (butlast seq-items) result (list (last seq-items))]
      (let [item (last items)]
        (if (nil? item)
          result
          (recur (butlast items) (conj result sep item)))))))

(defcheck solution-bb686c52
  (fn [newel x] (take (dec (* 2 (count x))) (interleave x (repeat newel)))))

(defcheck solution-bb829df9
  #(let [sep % col %2 cnt (count col)] (take (+ cnt (- cnt 1)) (mapcat (fn [x] [x sep]) col))))

(defcheck solution-bbc154c8
  (fn [x l]
    (rest (mapcat #(list x %) l))))

(defcheck solution-bbd2a65b
  (fn [sep coll] (drop-last (flatten (map #(vector % sep) coll)))))

(defcheck solution-bbd9321a
  (fn [item coll]
    (drop 1 (mapcat (partial vector item) coll))))

(defcheck solution-bc14599e
  (fn interpose* [sep coll]
    (if (empty? coll) ()
                      (cons (first coll) (interleave (repeat sep) (rest coll))))))

(defcheck solution-bc2493b3
  (fn intrps [v s]
    (drop 1 (interleave (repeat v) s))))

(defcheck solution-bc57ae8d
  (fn my-interpose [sep [item & col]]
    (if (seq col)
      (concat [item sep] (my-interpose sep col))
      [item])))

(defcheck solution-bd72d255
  (fn [x c] (drop 1 (interleave (repeat x) c))))

(defcheck solution-bdb01584
  #(rest (interleave (repeat %1) %2 )))

(defcheck solution-be26f553
  (fn [v coll] (butlast (mapcat #(vector % v) coll))))

(defcheck solution-be62b23b
  #(loop [orig %2 result []]
     (cond (empty? orig) result
           (empty? (rest orig)) (recur (rest orig) (conj result (first orig)))
           :else (recur (rest orig) (conj result (first orig) %1)))))

(defcheck solution-beab04f3
  (fn [separator s]
    (reduce
      #(concat % [separator] [%2])
      [(first s)]
      (rest s))))

(defcheck solution-beb10ec3
  (fn[elt a-seq]
    (butlast (mapcat (fn[a b] [a b]) a-seq
               (repeat elt)))))

(defcheck solution-bf37196d
  (fn [a b] (rest (mapcat (partial list a) b))))

(defcheck solution-bf5bbc19
  (fn inter [s [x & xs]]
    (if (seq xs)
      (cons x (cons s (inter s xs)))
      (list x))))

(defcheck solution-bf6cfa28
  (fn [sp xs]
    (drop-last (interleave xs (repeat (count xs) sp)))))

(defcheck solution-bf85e219
  (fn [sep s] (drop-last (mapcat #(list % sep) s))))

(defcheck solution-bf871bc8
  (fn [x xs]
    (rest (interleave (repeat x) xs))))

(defcheck solution-bff6141e
  (fn [s l] (mapcat #(if (= % (last l )) (list %) (list % s)) l)))

(defcheck solution-c089c435
  (fn [x xs]
    (drop-last (interleave xs (repeat (count xs) x))

      )))

(defcheck solution-c0dc80cf
  #(butlast (mapcat vector %2 (repeat %1))))

(defcheck solution-c14d10c5
  (fn [item arr]
    (->> arr
      (interleave (repeat item))
      (rest))))

(defcheck solution-c1e02e9
  #(rest (interleave (cycle [%1]) %2)))

(defcheck solution-c2699e61
  (fn [s coll] (drop 1 (mapcat list (repeat s) coll))))

(defcheck solution-c2e8ca13
  (fn [x [y & ys]] (cons y (mapcat #(list x %) ys))))

(defcheck solution-c34cb17b
  #(letfn [(worker [i s n]
             (if (= (count s) 1)
               (conj n (first s))
               (recur i (rest s) (conj n (first s) i))))]
     (worker %1 %2 [])))

(defcheck solution-c3f2816b
  (fn [sep xs]
    (rest
      (apply concat
        (map #(list sep %) xs)))))

(defcheck solution-c430851f
  (fn join [v col]
    (if (= 1 (count col))
      [(first col)]
      (concat [(first col) v] (join v (rest col))))))

(defcheck solution-c439697d
  #(next (for [e %2 x [% e]] x)))

(defcheck solution-c49ee924
  (fn
    [sep coll]
    (loop [completed '()
           remaining coll]
      (if (= (count remaining) 1)
        (cons (first remaining) completed)
        (recur (conj completed (last remaining) sep) (butlast remaining))))))

(defcheck solution-c4eba194
  (fn [x xs] (rest (mapcat #(list x %) xs))))

(defcheck solution-c566b30e
  (fn i [x y]
    (if (= '() y)
      '()
      (if (= '() (rest y))
        y
        (cons (first y)
          (cons x (i x (rest y))))))))

(defcheck solution-c572fcdc
  (fn [sep coll] (drop 1 (reduce #(conj %1 sep %2) [] coll))))

(defcheck solution-c5f2929c
  (fn [s xs]
    (cons (first xs)
      (mapcat
        #(list s %)
        (rest xs)))))

(defcheck solution-c62e8594
  (fn [item s]
    (butlast
      ((fn interp [item c1 c2]
         (if (empty? c2)
           c1
           (interp
             item
             (concat c1 [(first c2) item])
             (rest c2)))) item [] s))))

(defcheck solution-c6e92733
  #(into [] (drop-last (flatten (map (fn [a] (vector a %1)) %2)))))

(defcheck solution-c71f2bde
  (fn [v c] (conj (vec (mapcat #(conj [] % v) (butlast c))) (last c))))

(defcheck solution-c8e1dd35
  (comp  vec drop-last #(interleave %2 (repeat %1))))

(defcheck solution-c8ea9389
  (fn [d coll]
    (butlast (mapcat #(list % d) coll))))

(defcheck solution-c9008402
  (fn ipose [v s]
    (drop 1 (interleave (repeat v) s))))

(defcheck solution-c90f8c3b
  #(cons (first %2) (mapcat (partial list %1) (rest %2))))

(defcheck solution-c97b969f
  (fn [sp xs] (reduce (fn [a b] (if (empty? a) [b] (conj a sp b))) [] xs)))

(defcheck solution-c989595e
  (fn [x l] (->> (map #(list % x) l) (reduce concat) (drop-last 1))))

(defcheck solution-ca44c10
  (fn [sep coll]
    (drop-last (mapcat vector coll (repeat sep)))))

(defcheck solution-ca6705aa
  (fn f ([x y]
         (f x (rest y) [(first y)]))
    ([x y r]
     (if (empty? y)
       r
       (recur x (rest y) (conj r x (first y)))))))

(defcheck solution-ca672288
  (fn[a,b](reverse( rest( reverse (apply concat (map (fn[x](vector x a)) b)))))))

(defcheck solution-ca78d875
  #(take (- (* 2 (count %2)) 1) (apply concat (map list %2 (repeat %1)))))

(defcheck solution-cc5f92bc
  (fn [separator-value s]
    (butlast (mapcat #(vector % separator-value) s))))

(defcheck solution-cc996442
  (fn [d l] (rest (reduce #(conj %1 d %2) [] l))))

(defcheck solution-cccd9c16
  (fn interposeX [e s] (into (empty s)(flatten (reduce #(list %1 e %2) s)))))

(defcheck solution-cdb739a8
  (fn [x v] (rest (interleave (take  (count v) (repeat x)) v))))

(defcheck solution-cdc6220d
  (fn [sep coll]
    (loop [in coll out '()]
      (if (= (count in) 1)
        (concat out in)
        (recur
          (rest in)
          (concat out (list (first in) sep)))))))

(defcheck solution-ceab705f
  (fn inter [x y]
    (if (empty? (rest y)) (list (first y))
                          (cons (first y) (cons x (inter x (rest y)))))))

(defcheck solution-cec9a788
  (fn my-interspose [val coll]
    (butlast (mapcat (fn [x] (list x val)) coll))))

(defcheck solution-cef43ec6
  #(drop 1 (mapcat vector (repeat %) %2)))

(defcheck solution-cf26e9d3
  (fn [i s]
    (loop [rr [(first s)] ss (rest s)]
      (if (empty? ss)
        rr
        (recur (conj rr i (first ss)) (rest ss))))))

(defcheck solution-d038f20c
  (fn [v xs] (butlast (reduce #(conj %1 %2 v) [] xs))))

(defcheck solution-d06519ac
  (fn [x ys]
    (butlast
      (reduce
        #(conj %1 %2 x)
        []
        ys
        )
      )
    ))

(defcheck solution-d1cc60f3
  (fn my-interpose [del sq]
    (let [head (first sq)
          tail (rest sq)]
      (if (empty? tail)
        (cons head nil)
        (cons head (cons del (my-interpose del tail)))))))

(defcheck solution-d1ea2662
  (fn [sep lst] (drop-last (interleave lst (repeat sep)))))

(defcheck solution-d211f2e6
  (fn [i s]
    (concat
     (interleave (drop-last s) (repeat i))
     (list (last s)))))

(defcheck solution-d246f477
  (fn [v s]
    (reduce #(concat %1 [v %2])
      [(first s)]
      (rest s))))

(defcheck solution-d27179ed
  #(rest (for [i %2, x [% i]] x)))

(defcheck solution-d2aead36
  (fn intersperse [elt [hd & tl]]
    (if (empty? tl)
      (list hd)
      (conj (intersperse elt tl) elt hd))))

(defcheck solution-d3120ca1
  #(rest (apply concat (map (partial vector %1) %2))))

(defcheck solution-d31a607f
  (fn [sep coll] (drop 1 (interleave (repeat sep) coll))))

(defcheck solution-d327c851
  (fn myinterp [val coll]
    (into [(first coll)]
      (mapcat (fn [x] [val x])
        (rest coll)))))

(defcheck solution-d38ab923
  (fn this [sep s]
    (cond (<= (count s) 1) s
          :else (conj (conj (this sep (rest s))
                        sep)
                  (first s)))))

(defcheck solution-d3c89eb1
  #(drop-last (flatten (for [x %2 y (cons % ())] [x y]))))

(defcheck solution-d4043e9a
  (fn  [delim lst]
    (loop [rm (seq lst), acc (vector)]
      (cond (empty? rm) (seq acc)
            (= 1 (count rm)) (recur (rest rm) (conj acc (first rm)))
            :else (recur (rest rm) (conj acc (first rm) delim))))))

(defcheck solution-d40bb14b
  #(-> (interleave %2 (repeat %1)) drop-last vec))

(defcheck solution-d42c313f
  (fn [e s]
    (drop-last (mapcat (fn [x] [x e]) s))))

(defcheck solution-d4315673
  (fn [n s] (drop-last 1(apply concat (map #(conj [] % n) s)))))

(defcheck solution-d469986b
  (fn [sep S]
    (for [x (range (dec (* (count S) 2)))]
      (if (even? x)
        (S (quot x 2)) sep
        )
      )
    ))

(defcheck solution-d540c1c4
  (fn [x xs] (-> (mapcat #(list % x) xs)
               (butlast))))

(defcheck solution-d54a5e3f
  (fn [w l]
    (cons (first l) (interleave (repeat w) (rest l)))))

(defcheck solution-d55fae2d
  (fn [i l] (reverse (loop [acc () s1 l]
                       (if (= (count s1) 1)
                         (cons (first s1) acc)
                         (recur (cons i (cons (first s1) acc)) (rest s1))
                         )))))

(defcheck solution-d5c26b59
  #(conj (mapcat (fn [x] [%1 x]) (next %2)) (first %2)))

(defcheck solution-d669a0e7
  (fn [v, s]
    (drop-last (apply concat (map #(list %1 v) s)))))

(defcheck solution-d687bde
  (fn [sp [h & ts]]
    (reduce
      #(conj %1 sp %2)
      [h]
      ts)))

(defcheck solution-d6893bb9
  (fn [sep xs]
    (rest (mapcat #(vector sep %1) xs))))

(defcheck solution-d6c53af4
  (fn interp [sep coll]
    (let [f (first coll) r (rest coll)]
      (if (empty? r)
        coll
        (cons f (cons sep (interp sep r)))))))

(defcheck solution-d6e6c8ce
  (fn inter [a lat]
    (let [[x & xs] lat]
      (if (empty? xs) (list x)
                      (cons x (cons a (inter a xs)))))))

(defcheck solution-d704db47
  (fn my-interpose [el a-seq]
    (drop-last (interleave a-seq (repeat (count a-seq) el)))
    ))

(defcheck solution-d70a54fb
  #(rest (mapcat (partial (fn [x y] (list x y)) %1) %2)))

(defcheck solution-d71ccccc
  #(into [] (cons (first %2) (mapcat (fn [b] [%1 b]) (rest %2)))))

(defcheck solution-d735a5b0
  (fn interpose- [x coll]
    "40. Write a function which separates the items of a sequence by an arbitrary value."
    (concat (mapcat (fn [n] (list n x)) (butlast coll)) (list (last coll)))))

(defcheck solution-d7669cde
  (fn alt-interpose [sep coll]
    (butlast (mapcat #(list % sep) coll))))

(defcheck solution-d8947e7f
  (fn [p xs] (drop-last (interleave xs (repeat (count xs) p) ))
    ))

(defcheck solution-d8e0c9e3
  (comp next
        (fn f [x [h & t]]
          (if h
            (conj (f x t) h x)))))

(defcheck solution-d90d8e1
  (fn [v l] (butlast (mapcat #(list % v) l))))

(defcheck solution-d969ef50
  #( (comp drop-last reduce) (fn [acc x] (conj acc x %1) ) [] %2))

(defcheck solution-d98bb060
  (fn myInterpose
    [sep coll]
    (drop-last (reduce (fn [x y] (conj x y sep)) [] coll))))

(defcheck solution-d999c0c
  (fn [sep coll]
    (drop-last (interleave coll (repeat sep)))))

(defcheck solution-da6dd5ef
  (fn [x ls]
    (rest (interleave (repeat x) ls))))

(defcheck solution-dabc9773
  #(conj (interleave (repeat %) (rest %2)) (first %2)))

(defcheck solution-db0a5287
  (fn [delim xs] (rest (mapcat list (repeat delim) xs))))

(defcheck solution-db32ac98
  #(cons (first %2) (mapcat (fn [x] [%1 x]) (rest %2))))

(defcheck solution-db9b4c99
  (fn [e,l]
    (rest (mapcat #(list e %) l))
    ))

(defcheck solution-dbd4d43b
  (fn [v l] (->> l (mapcat #(list v %)) (rest))))

(defcheck solution-dcd6fd8a
  (fn [x s]
    (vec (butlast (mapcat #(list % x) s)))))

(defcheck solution-dd3d5b3e
  (fn interpose-1 [val coll]
    (reduce (fn [xs x]
              (conj xs val x)) [(first coll)] (rest coll))))

(defcheck solution-dd6c0934
  #(loop [L %2, result []]
     (if (= (count L) 1)
       (conj result (first L))
       (recur (rest L) (conj result (first L) %1)))))

(defcheck solution-dddab714
  (fn [b s] (butlast (mapcat #(vector % b) s))))

(defcheck solution-de0e4c9c
  #(butlast (mapcat vector %2 (iterate (fn [x] %1) %1))))

(defcheck solution-dea192d6
  #(conj (mapcat (fn [e] [% e]) (rest %2)) (first %2)))

(defcheck solution-deaba713
  (fn ip [n [x & xs]]
    (if (seq xs)
      (lazy-seq
        (cons x
          (cons n
            (ip n xs))))
      [x])))

(defcheck solution-deac32c
  (fn [x s]
    (cons (first s) (reduce (fn [r y] (conj r x y)) [] (rest s)))))

(defcheck solution-debceabe
  (fn [sep coll]
    (drop 1 (interleave (repeat sep) coll))
    ))

(defcheck solution-df3d1f63
  (fn problem40-interpose [i xs]
    (reduce
      (fn [agg x]
        (into agg [i x]))
      [(first xs)]
      (rest xs))))

(defcheck solution-dfdef0c1
  #(into (empty %2) (drop-last (mapcat vector %2 (repeat %1)))))

(defcheck solution-e000c15b
  (fn[delim coll](rest (reduce #(conj %1 delim %2) [] coll))))

(defcheck solution-e02faf4a
  (fn [v coll]
    (loop [ret [(first coll)]
           c (rest coll)]
      (if (empty? c)
        ret
        (recur (conj ret v (first c)) (rest c))))))

(defcheck solution-e08d9970
  (fn my-interpose
    [val collection]
    (next (seq (reduce #(conj %1 val %2) [] collection)))))

(defcheck solution-e0c07247
  (fn [sep xs]
    (rest (interleave (repeat (count xs) sep) xs))))

(defcheck solution-e11bc215
  #(drop-last (mapcat vector %2 (repeat %1))))

(defcheck solution-e1795ed3
  #(rest (mapcat conj (repeat [%]) %2)))

(defcheck solution-e1b38ea7
  (fn[x y]
    (pop (into []
           (flatten
             (map
               #(conj [%] x)
               y
               )
             )))
    ))

(defcheck solution-e1d501b8
  (fn intrpose [s xs]
    (case (count xs)
      0 []
      1 xs
      (let [[x & ys] xs]
        (concat [x s] (intrpose s ys))))))

(defcheck solution-e27f37c3
  (fn [del seq]
    (drop-last 1 (flatten (map vector seq (repeat del))))))

(defcheck solution-e289d120
  (fn intp [sep sq]
    (cond (empty? (rest sq)) sq
          :else (cons (first sq) (cons sep (intp sep (rest sq))))
          )
    ))

(defcheck solution-e294a30d
  (fn[x ls] ((comp drop-last flatten) (map #(list % x) ls))))

(defcheck solution-e2b1cfb2
  #(flatten (list (for[e (butlast %2)](list e %)) (last %2))))

(defcheck solution-e3054a89
  (fn [i c] (take (dec (* 2 (count c))) (interleave c (repeat i)))))

(defcheck solution-e32aacc0
  (fn [x s] (reduce #(concat %1 (if (identical? %2 (last s)) [%2] [%2 x])) [] s)))

(defcheck solution-e3338c5f
  (fn i [d s]
    (let [[a & r] s]
      (cond
        (empty? s) ()
        (empty? r) (list a)
        :else (conj (i d r) d a)))))

(defcheck solution-e389000d
  (fn fill [sep seq]
    (drop-last
      (reduce (fn [acc e] (conj (conj acc e) sep)) [] seq))))

(defcheck solution-e43838fb
  (fn [v [x & xs]] (cons x (interleave (repeat v) xs))))

(defcheck solution-e43844ee
  #(butlast (mapcat vector %2 (repeat %))))

(defcheck solution-e450d11a
  (fn [v, xs]
    (butlast (reduce concat (map #(list % v) xs)))
    ))

(defcheck solution-e494f46b
  #(concat (list (first %2)) (mapcat (fn [x] (list % x)) (rest %2))))

(defcheck solution-e49a9002
  #(reduce (fn [xs v] (if (= (dec (count %2)) (/ (count xs) 2))
                        (conj xs v)
                        (conj xs v %1))) [] %2))

(defcheck solution-e4d97b62
  (fn my-interpose [x l]
    (drop-last (reduce #(conj %1 %2 x) [] l))))

(defcheck solution-e5cbd371
  (fn [x coll] ((fn [x coll new-coll] (if (empty? (rest coll)) (conj new-coll (first coll)) (recur x (rest coll) (conj new-coll (first coll) x)))) x coll [])))

(defcheck solution-e5dbc41f
  (fn [separator [x & rest]]
    (flatten (list x
               (map (partial list separator) rest)))))

(defcheck solution-e60ace5d
  (fn [x y] (rest (mapcat #(list x %1) y) ) ))

(defcheck solution-e6166736
  (fn [n s]
    (drop-last 1 (flatten (mapcat #(list % n) s)))))

(defcheck solution-e652942c
  (fn [x xs]
    ( #(
         if (= 1 (count %2))
         (conj %3 (first %2))
         (recur %1 (rest %2) (conj %3 (first %2) %1 ))
         )
     x xs []
     )
    ))

(defcheck solution-e6dfd92a
  (fn [x y]
    (butlast (interleave y (repeat x) ))))

(defcheck solution-e714a64b
  #(next (mapcat list (repeat %) %2)))

(defcheck solution-e7272924
  #(-> (interleave %2 (repeat %1)) drop-last))

(defcheck solution-e8348c44
  (fn [s x] (cons (first x) (mapcat #(list s %) (next x)))))

(defcheck solution-e84dae3e
  (fn [n s] (drop 1 (mapcat #(cons n %) (partition 1 s)))))

(defcheck solution-e89cd695
  (fn [sep l] (drop-last (flatten (map #(conj nil sep %) l)))))

(defcheck solution-e8f2de2e
  (fn [a s] (butlast (mapcat #(vector % a) s))))

(defcheck solution-e95429f9
  #(rest (mapcat list (repeatedly (constantly %1)) %2)))

(defcheck solution-e971299c
  (fn [x coll]
    (reduce (fn [a b] (conj a x b)) [(first coll)] (rest coll))))

(defcheck solution-e9b1dc7
  (fn [v s] (butlast (reduce #(conj %1 %2 v) [] s))))

(defcheck solution-e9f28639
  #(reverse
     (rest
       (reduce (fn [a b] (conj a b %1)) '() %2))))

(defcheck solution-ea94b9ca
  (fn [x c] (drop-last (mapcat #(vector %1 x) c))))

(defcheck solution-eb2346b8
  (fn [val coll]
    (loop [out []
           c1 coll]
      (if (empty? c1)
        (drop-last out)
        (recur (conj out (first c1) val)
          (drop 1 c1))))))

(defcheck solution-eb56f659
  #(take (dec (* (count %2) 2))(interleave %2 (repeat %1))))

(defcheck solution-eb703b01
  (fn [tween xs] (let [hmm (fn hmm [sep xs acc] (if (seq xs) (let [[x & etc] xs, acc' (if (seq etc) (conj acc x sep) (conj acc x))] (recur sep etc acc')) acc))] (hmm tween xs []))))

(defcheck solution-ebee673a
  (fn [ele seq]
    (drop-last (flatten (map #(list %1 ele) seq)))))

(defcheck solution-ec436213
  #(drop-last (mapcat list %2 (repeat %1))))

(defcheck solution-ec59b5b6
  (fn [sep coll]
    (butlast
      (flatten
        (map #(list % sep) coll)))))

(defcheck solution-ed6ab920
  (fn my-interposey
    [val x]
    (drop 1 (mapcat #(list val %) x))))

(defcheck solution-ed8a0e5
  (fn [d c]
    (rest (mapcat (fn [x] [d x]) c))))

(defcheck solution-edd7b183
  (fn [x y] ((comp vec drop-last mapcat) #(list % x) y)))

(defcheck solution-ee53b89
  (fn intp [j [f & other]]
    (if (nil? other)
      (list f)
      (cons f (cons j (intp j other))))))

(defcheck solution-eeaa51b6
  (fn intrpse [i,s]
    (if (not (= 1 (count s)))
      (cons (first s) (cons i (intrpse i (rest s))))
      (list (first s)))))

(defcheck solution-eec61f8c
  (fn [i lst]
    (rest (reduce #(conj (conj %1 i) %2) [] lst ))))

(defcheck solution-ef5903e9
  #(rest (mapcat (fn [x] (list %1 x)) %2)))

(defcheck solution-ef6f916b
  (fn [s c] (conj (mapcat #(vector s %) (rest c)) (first c))))

(defcheck solution-f1dc1941
  (fn [s l]
    (
     (fn ! [s f r]
       (if (empty? r)
         (conj (empty r) f)
         (conj         	(! s (first r) (rest r))    s   f )
         )
       )
     s (first l) (rest l)
     )
    ))

(defcheck solution-f22586a2
  (fn [i c] (butlast (mapcat #(list % i) c))))

(defcheck solution-f22fb0b3
  (fn intrps
    [x y]
    (if (empty? y)
      []
      (if (empty? (rest y))
        [(first y)]
        (concat [(first y) x]
                (intrps x (rest y))
                )
        )
      )
    ))

(defcheck solution-f278ecbb
  (fn nintp [m x]
    (let [doint (fn [z] [m z])]
      (rest (mapcat doint x)))))

(defcheck solution-f29367bc
  #(butlast (mapcat list %2 (repeat (count %2) %1))))

(defcheck solution-f295f178
  #(drop-last (interleave %2 (cycle [%1]))))

(defcheck solution-f35fe279
  (fn [a x]
    (conj
      (vec (flatten
             (map #(list %1 %2) x (take (- (count x) 1) (cycle (list a)))
               )))
      (last x))))

(defcheck solution-f3cd013b
  (fn custom-interpose
    [padding input-seq]
    (drop-last
      (mapcat #(list % padding) input-seq))))

(defcheck solution-f419a132
  (fn [separator seqs]
    (butlast (interleave seqs (iterate identity separator)))))

(defcheck solution-f6333597
  (fn pose
    ([sep coll] (pose sep (rest coll) (vector (first coll))))
    ([sep coll aggr]
     (if (empty? coll)
       aggr
       (pose sep (rest coll) (conj aggr sep (first coll)))))))

(defcheck solution-f6d20f54
  (fn ip [sep l]
    (if (= 1 (count l)) l
                        (cons (first l) (cons sep (ip sep (rest l)))))))

(defcheck solution-f79ac0a5
  (fn pose [sep inseq]
    (rest
      (reduce
        #(concat %1 [sep] [%2]) [] inseq))))

(defcheck solution-f7adcce3
  (comp drop-last flatten (fn [delim xs] (map #(list % delim) xs))))

(defcheck solution-f87a93df
  (fn [c l] (drop-last (interleave l (repeat c)))))

(defcheck solution-f89b0e8c
  (fn my-interpose [el lst]
    (->> lst
      (reduce (fn [accum val] (concat (list el val) accum)) '())
      rest
      reverse)))

(defcheck solution-f909fe41
  #(next (interleave (repeat %) %2)))

(defcheck solution-f92c86a0
  (fn t4 [v coll] (reduce #(if (seq %1) (conj %1 v %2) (conj %1 %2)) [] coll)))

(defcheck solution-f96e19de
  (fn [n coll] (reduce #(concat %1 [n %2]) [(first coll)] (rest coll))))

(defcheck solution-f9c4c97
  (fn [x,y] (rest (mapcat #(vector x %) y))))

(defcheck solution-f9d2a21b
  (fn interpose-seq
    ([x y]
     (interpose-seq x (rest y) (vector (first y))))
    ([x y z]
     (if (= 0 (count y))
       z
       (if (= x (last z))
         (recur x (rest y) (conj z (first y)))
         (recur x y (conj z x)))))))

(defcheck solution-fa0005e8
  (fn [x coll] (butlast (interleave coll (repeat x)))))

(defcheck solution-fa396b81
  (fn [v a-seq] (rest (reduce #(conj %1 v %2) [] a-seq))))

(defcheck solution-fa4af19b
  (fn inpose [v coll]
    (butlast (apply concat
               (loop [acc []
                      coll coll]
                 (if (empty? coll)
                   acc
                   (recur (conj acc [(first coll) v]) (rest coll))))))))

(defcheck solution-faeb017f
  (fn f [e l]
    (reduce #(conj %1 e %2) (subvec l 0 1) (rest l))
    ))

(defcheck solution-faf4a826
  (fn [x s]
    (letfn [(hf [result s]
              (if (= (count s) 1)
                (concat result s)
                (recur
                  (concat result [(first s) x])
                  (rest s))))]
      (hf [] s))))

(defcheck solution-fb8427ae
  (fn [n s]
    (drop-last (interleave s (repeat (count s) n)))))

(defcheck solution-fb95a324
  (fn interpose-reduce [delim coll]
    (if (seq coll)
      (->> coll
        (reduce (fn [acc x] (conj acc delim x)) [])
        rest)
      '())))

(defcheck solution-fbb0e965
  (fn
    [a b]
    (letfn
     [(step
        [a b]
        (if b
          (cons
            a
            (cons
              (first b)
              (step a (next b))))))]
      (drop 1 (step a b)))))

(defcheck solution-fbd1936e
  (fn [e s]
    (drop-last (mapcat #(list % e) s))))

(defcheck solution-fbd885bb
  (fn [x coll]
    (loop [x x coll coll res []]
      (if (= (count coll) 1)
        (conj res (first coll))
        (recur x (rest coll) (conj (conj res (first coll)) x))
        ))))

(defcheck solution-fbf528db
  #(drop-last(interleave %2 (repeat %))))

(defcheck solution-fc08947
  (fn [sep ls] (cons (first ls) (for [[a b] (map vector (repeat sep) (rest ls))
                                      x [a b]]
                                  x))))

(defcheck solution-fc54c731
  (fn [n ys]
    (let [xs (reverse ys)]
      ((fn iter [xs res]
         (if (empty? xs)
           res
           (iter (rest xs)
             (cons (first xs) (cons n res))))) (rest xs)
       (list (first xs))))))

(defcheck solution-fca17207
  #(rest (mapcat (fn [x y] [y x]) %2 (repeat %))))

(defcheck solution-fdf5126
  (fn [sep s] (butlast (mapcat #(list %1 %2) s (repeat sep)))))

(defcheck solution-fe1a0f6a
  (fn
    [insert-val aseq]
    (loop [lseq aseq acc []]
      (if (empty? lseq)
        acc
        (if (= 1 (count lseq))
          (conj acc (first lseq))
          (recur (rest lseq) (conj acc (first lseq) insert-val)))))))

(defcheck solution-fe5543c2
  #(butlast (apply vector (mapcat (fn [a] (vector a %)) %2))))

(defcheck solution-fe595b44
  (fn [spacer coll] (rest (interleave (repeat spacer) coll))))

(defcheck solution-fe5eea97
  (fn [x,y]
    (reverse
      (rest
        (reduce #(conj  %1 %2 x) '() y )))))

(defcheck solution-fe76b976
  (fn [i l] (flatten (reduce #(list %1 i %2) l))))

(defcheck solution-fe9bbced
  (fn [s a] (rest (reduce #(conj %1 s %2) [] a))))

(defcheck solution-fede3d31
  (fn[i,s](rest (mapcat #(list i %) s))))

(defcheck solution-fef891a2
  (fn my-interpose [x coll]
    (rest (interleave (repeat x) coll))))

(defcheck solution-ff6f8082
  #(drop 1 (%1 %2 %3 %4)) #(reduce (%1 %2) [] %3) (fn [x] #(conj %1 x %2)))

(defcheck solution-ff75fe2e
  (fn [v s] (butlast (mapcat #(concat [%] [v]) s))))

(defcheck solution-ff849f22
  (fn [sep coll] (reduce #(if (not (empty? %1)) (conj %1 sep %2) (conj %1 %2)) [] coll)))

(defcheck solution-ff95e507
  (fn [ch coll] (reduce (fn [t v] (into t [ch v])) [(first coll)] (rest coll))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-40))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
