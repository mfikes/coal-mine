(ns coal-mine.problem-32
  (:require [coal-mine.checks :refer [defcheck-32] :rename {defcheck-32 defcheck}]
            [clojure.test]))

(defcheck solution-1068ecba
  #(mapcat (fn [x] (list x x)) %))

(defcheck solution-1073cde8
  (partial
    (fn [acc l]
      (if (empty? l) (reverse acc)
                     (recur (conj (conj acc (first l)) (first l)) (rest l)))) '()))

(defcheck solution-119fd074
  #(apply concat (map (fn [x] (list x x)) %)))

(defcheck solution-11c329d
  #(apply concat (map (fn [e] `(~e ~e)) %)))

(defcheck solution-11fd7afe
  reduce #(conj % %2 %2) [])

(defcheck solution-12a804ab
  (fn [xs]
    (mapcat #(vec [% %]) xs)))

(defcheck solution-130e4779
  mapcat (fn [i] (list i i)))

(defcheck solution-134d8344
  (fn [x] (reduce concat (map #(take 2 (repeat %)) x))))

(defcheck solution-148234e5
  (fn duplicate-each
    [input]
    (into []
      (mapcat #(list % %) input))))

(defcheck solution-14d07f53
  (fn dup [xs]
    (loop [s xs, r []]
      (if (empty? s)
        r
        (recur (rest s) (conj r (first s) (first s)))))))

(defcheck solution-14f684d9
  (fn [lat] (reduce concat (map #(take 2 (repeat %)) lat))))

(defcheck solution-14fc8e74
  (fn [coll] (mapcat #(list %1 %1) coll)))

(defcheck solution-150bcb9d
  mapcat #(list %1 %1))

(defcheck solution-15860eb9
  #(loop [myseq %
          result []]
     (if (empty? myseq)
       result
       (recur (rest myseq) (conj result (first myseq) (first myseq)))
       )))

(defcheck solution-15ae5b5c
  #(loop [ result [] coll %]
     (if (nil? coll) result
                     (recur (apply conj result (repeat 2 (first coll))) (next coll)))))

(defcheck solution-1602f874
  #(reduce (fn [acc e] (conj acc e e)) [] %))

(defcheck solution-161a4f33
  (fn ds [s] (mapcat #(list % %) s)))

(defcheck solution-17af2eb3
  (fn dup
    ([acc rem]
     (let [front (first rem)
           back (rest rem)]
       (if front
         (recur (conj acc front front) back)
         (reverse acc))
       ))
    ([coll] (dup '() coll))))

(defcheck solution-193cffde
  (fn [xs]
    (mapcat (fn [x] (list x x)) xs)))

(defcheck solution-1a49e3fa
  (fn ! [x]
    (if (empty? x)
      '()
      (let [y (first x) z (! (rest x))]
        (concat [y y] z)))))

(defcheck solution-1a51033d
  mapcat #(repeat 2 %))

(defcheck solution-1a9c75eb
  (fn [coll]
    (loop [x coll
           out []
           c 0]
      (if (= (count x) c) out
                          (let [z (get x c)]
                            (recur x (conj (conj out z) z) (inc c)))))))

(defcheck solution-1aa9b88
  #(apply concat (map (partial repeat 2) %)))

(defcheck solution-1b06300a
  (fn [s] (mapcat #(list % %) s)))

(defcheck solution-1bb51d80
  (fn [xs]
    (reduce (fn [a b] (conj (conj a b) b)) [] xs)
    ))

(defcheck solution-1c7dee69
  (fn [c] (mapcat #(list % %) c)))

(defcheck solution-1d61fd9a
  (fn dupe[sequence]
    (mapcat #(list % %) sequence)))

(defcheck solution-1d77d0cf
  (fn [s]
    (reduce #(conj %1 %2 %2)  [] s)
    ))

(defcheck solution-1df16984
  (fn dup-seq [mylist]
    (seq (loop [l mylist final '[]]
           (if (empty? l) final
                          (recur (rest l) (conj (conj final (first l)) (first l))))))))

(defcheck solution-1e28adc3
  mapcat #(vector % %))

(defcheck solution-1ee0e81a
  #(reduce
     (fn [acc x]
       (concat acc (list x x)))
     () %))

(defcheck solution-1f82bb3c
  #(apply concat (map list % %)))

(defcheck solution-1fe17879
  (fn f [xs] (if (empty? xs) xs (conj (f (rest xs)) (first xs) (first xs)))))

(defcheck solution-20351d15
  #(reduce concat (for [x %] [x x])))

(defcheck solution-21608fc4
  (fn[x](mapcat #(seq [% %])x) ))

(defcheck solution-2216475d
  #(reverse (reduce (fn [acc elt] (cons elt (cons elt acc))) '() %)))

(defcheck solution-227e35f
  reduce (fn [x y] (conj x y y)) [])

(defcheck solution-2319f040
  #(apply concat (map (fn [a] [a a]) %)))

(defcheck solution-2334dae7
  (fn [s]
    (loop [s s r []]
      (if (not (seq s))
        r
        (recur (rest s) (conj r (first s) (first s)))))))

(defcheck solution-2364b9e8
  reduce #(conj  %1 %2 %2) [])

(defcheck solution-236c240f
  (fn [x] (seq (reduce #(conj %1 %2 %2) [] x))))

(defcheck solution-23933c49
  (fn [s]
    (reduce into (map #(vec (list %1 %1)) s ))))

(defcheck solution-23e08d08
  (fn dupl [[f & r]]
    (if (nil? f) '()
                 (cons f (cons f (dupl r))))))

(defcheck solution-2431481f
  (fn peu [x] (if (empty? x) x (conj (peu (rest x)) (first x) (first x)))))

(defcheck solution-24449f33
  (fn [l]
    (loop [x l r []]
      (if (empty? x)
        r
        (recur (rest x) (conj (conj r (first x)) (first x)))))))

(defcheck solution-24c678ff
  (fn [s]
    (loop [r s acc []]
      (if (empty? r)
        (reverse acc)
        (recur (rest r) (cons (first r) (cons (first r) acc)))
        )
      )
    ))

(defcheck solution-2597858a
  reduce (fn [l r] (concat l [r r])) ())

(defcheck solution-25a21785
  (fn [s]
    (loop
     [result '()
      [h & rest :as remaining] s]
      (if (empty? remaining)
        result
        (recur (concat result [h h]) rest)))))

(defcheck solution-26e20027
  (fn dup [l]
    (if (empty? l)
      '()
      (conj (dup (rest l)) (first l) (first l)))))

(defcheck solution-27031199
  (fn [s](reduce #(conj % %2 %2)  [] s)))

(defcheck solution-270fbcd7
  (fn [col]
    (reverse
      (reduce
        #(cons %2 (cons %2  %1))
        '()
        col
        )
      )
    ))

(defcheck solution-2742253f
  reduce #(concat %1 [%2 %2]) [])

(defcheck solution-27706e97
  (fn [x] (apply concat (map (fn [y] [y y]) x))))

(defcheck solution-2a4a7e1
  (fn [coll] (loop [c coll r []] (if (empty? c) r (recur (rest c) (conj (conj r (first c)) (first c)))))))

(defcheck solution-2b2f4943
  (partial mapcat (fn [x] [x x])))

(defcheck solution-2b3f82dd
  (fn [coll] (interleave coll coll)))

(defcheck solution-2bfdbeac
  mapcat #(vec [% %]))

(defcheck solution-2cb593cf
  #(mapcat (fn [x] [x x]) %))

(defcheck solution-2cbbf549
  (fn [s]
    (reduce
      #(concat %1 (list %2 %2))
      []
      s)))

(defcheck solution-2d8d55cc
  (fn [x] (mapcat #(vector % %) x)))

(defcheck solution-2dbc2541
  (fn [l]
    (loop [l l result '()]
      (if (empty? l)
        result
        (recur (rest l) (concat result [(first l) (first l)] ))))))

(defcheck solution-2e10a3f6
  (fn [x]
    (apply concat (map #(list % %) x))))

(defcheck solution-2e24d06b
  (fn [col] (apply concat (map vector col col))))

(defcheck solution-2f12b091
  (fn [in](reverse (reduce (fn [a b] (cons b (cons b a))) '() in))))

(defcheck solution-2f2f79b3
  (fn [lst]
    (apply concat (for [x lst] [x x]))
    ))

(defcheck solution-2f419e1d
  (fn [xs]
    (reduce (fn [agg now] (into agg [now now])) [] xs)))

(defcheck solution-2fd5426c
  (fn [l] (apply concat (map #(list % %) l))))

(defcheck solution-304447b0
  (fn [coll]
    (reduce #(conj %1 %2 %2) [] coll)
    ))

(defcheck solution-3058fab
  (fn dup [v]
    (if (empty? v)
      '()
      (conj (conj (dup (rest v)) (first v)) (first v))
      )
    ))

(defcheck solution-311a1382
  mapcat (fn [o] (list o o)))

(defcheck solution-3292e722
  (fn [seq]
    (reverse (reduce (fn [col ele]
                       (conj col ele ele))
               '() seq))))

(defcheck solution-32f1a062
  (fn dup-seq [lst]
    (apply concat (map #(vec [% %]) lst))))

(defcheck solution-3343f84d
  (fn f [c] (if (empty? c) '() (cons (first c) (lazy-seq (cons (first c) (f (rest c))))))))

(defcheck solution-334988fe
  (fn [s] (interleave (seq s) s)))

(defcheck solution-335413ee
  (fn [x] (apply concat (map #(vector %1 %1) x))))

(defcheck solution-3407395a
  #(apply concat (map vector %1 %1)))

(defcheck solution-341dcadd
  (fn [x] (reduce #(conj % %2 %2) () (reverse x))))

(defcheck solution-34a2bbb4
  (fn [coll] (mapcat (juxt identity identity) coll)))

(defcheck solution-34fd581b
  (partial
    (fn [acc coll]
      (if (nil? coll)
        (reverse acc)
        (recur (conj acc (first coll) (first coll)) (next coll))
        )
      ) '()
    ))

(defcheck solution-35bb77af
  reduce (fn[x,y](conj (conj x y) y)) [])

(defcheck solution-3733019b
  (fn [li] (apply concat (map #(list % %) li))))

(defcheck solution-3832fe47
  (fn [coll] (loop [c coll a []] (if (empty? c) a (recur (rest c) (conj (conj a (first c)) (first c)))))))

(defcheck solution-3855231d
  #(apply concat (map (fn[a] [a a]) %)))

(defcheck solution-38a38901
  (fn f [x] (let [x (seq x)](when x (cons (first x) (cons (first x) (f (rest x))))))))

(defcheck solution-38c869e7
  (fn duped [coll]
    (mapcat (fn [x] (list x x)) coll)))

(defcheck solution-38f40850
  mapcat (fn [a] (list a a)))

(defcheck solution-39b70b1a
  (fn this [v]
    (if (= (count v) 0)
      '()
      (conj (conj (this (rest v))
              (first v)
              )
        (first v)))))

(defcheck solution-39e9db44
  #(loop [acc '() [x & xs] %]
     (if (nil? x) acc
                  (recur (concat acc (list x x)) xs))))

(defcheck solution-3a96d3ee
  (fn [x] (reduce concat (map #(repeat 2 %) x))))

(defcheck solution-3aab4a91
  (fn [lst] (apply concat (map #(list % %) lst))))

(defcheck solution-3aaf5ffc
  reduce #(conj (conj %1 %2) %2) [])

(defcheck solution-3ad6f01b
  (fn[x]
    (mapcat (partial repeat 2) x)))

(defcheck solution-3b055537
  #(reduce concat (map vector % %)))

(defcheck solution-3b0bf4bc
  (fn [s] (apply concat (map (fn [x] (list x x)) s))))

(defcheck solution-3b663d19
  (fn [x] (reverse  (reduce #(conj (conj %1 %2) %2) '() x))))

(defcheck solution-3c9bde88
  (fn dob-seq [x]
    (if (not (empty? x))
      (let [fx (first x), rx (rest x)]
        (cons fx (cons fx (dob-seq rx))))
      '())))

(defcheck solution-3c9d611b
  (fn ! [coll]
    (when-let [s (seq coll)]
      (concat (cons (first s) (list (first s)))  (! (rest s))))))

(defcheck solution-3d304c68
  (fn [s] (reverse (reduce #(conj % %2 %2) '() s))))

(defcheck solution-3d397d13
  #(loop [coll % tgt '()]
     (if (= coll '())
       tgt
       (recur (rest coll) (concat tgt [(first coll) (first coll)]))
       )))

(defcheck solution-3ededebe
  #(apply concat (map (fn [x] [x x]) %)))

(defcheck solution-3fd7cfd9
  #(reverse (loop [i (dec (count %)) result []]
              (if (= i -1)
                result
                (recur (dec i)
                  (conj (conj result (nth % i)) (nth % i) )
                  )
                )
              ) ))

(defcheck solution-40215c3d
  (fn [coll]
    (reduce #(conj %1 %2 %2) [] coll)

    ))

(defcheck solution-40857d97
  (fn f [xs]
    (if (empty? xs)
      '()
      (cons (first xs) (cons (first xs) (lazy-seq (f (rest xs))))))))

(defcheck solution-41d10fb8
  (fn mrg
    ([in] (mrg in ()))
    ([in out]
     (if (empty? in)
       (reverse out)
       (mrg (rest in) (cons (first in) (cons (first in) out)))
       )
     )
    ))

(defcheck solution-41d3932f
  (fn [xs] (apply concat (map #(vector % %) xs)) ))

(defcheck solution-41d6dcef
  #(concat (interleave %1 %1)))

(defcheck solution-41d88326
  (fn [l]
    (loop [[hd & body] l ret []]
      (if (nil? hd)
        ret
        (recur body (conj ret hd hd))))))

(defcheck solution-41feaa15
  (fn [sq] (mapcat #(list % %) sq)))

(defcheck solution-4286ac49
  (fn [x]
    (apply concat (map #(repeat 2 %) x))
    ))

(defcheck solution-43c85f75
  reduce #(conj (conj % %2) %2 ) [])

(defcheck solution-43e8b32
  #(reverse (reduce (fn [accum v] (conj (conj accum v) v)) () %)))

(defcheck solution-43e8c6de
  (fn [s]
    (loop [r (rest s)
           acc (conj () (first s) (first s))]
      (if (empty? r)
        (reverse acc)
        (recur (rest r) (conj acc (first r) (first r)))))))

(defcheck solution-43ffd47a
  (fn duplicateElements
    [collection]
    "Returns a function which contains two of each element in the given
    collection, in the same order."
    (mapcat #(conj '() % %) collection)))

(defcheck solution-44166d68
  (fn [xs]
    (apply concat (map #(list % %) xs))))

(defcheck solution-448c8e1f
  (fn [xs]
    (reduce (fn [ys y] (conj ys y y))
      []
      xs)))

(defcheck solution-44e8a471
  (partial
    (fn [out coll]
      (if (= '() coll)
        (reverse out)
        (recur (conj out (first coll) (first coll))
          (rest coll))))
    '()))

(defcheck solution-44fa2223
  mapcat #(vector %1 %1))

(defcheck solution-450c8df2
  (fn [coll]
    (loop [[x & xs :as coll] coll
           acc []]
      (if (empty? coll) acc (recur xs (conj acc x x))))))

(defcheck solution-4565bb08
  #(sort(concat % %)))

(defcheck solution-45930873
  (fn d [s]
    (when (seq s)
      (let [f (first s)
            r (rest s)]
        (conj (conj (d r) f) f)))))

(defcheck solution-45ca1f09
  mapcat (fn [x] [x x]))

(defcheck solution-45d59177
  (fn[v]
    (loop [ans [] w v]
      (if (empty? w)
        ans
        (recur
          (conj (conj ans (first w))
            (first w))
          (next w))))))

(defcheck solution-461b4e13
  (fn [s]
    (interleave s s)))

(defcheck solution-468705d1
  (fn f [x] (if (empty? x) x (conj (f (rest x)) (first x) (first x)    ) )))

(defcheck solution-46a18396
  #(mapcat (fn [e] [e e]) %))

(defcheck solution-477266bb
  #(loop [col %1 result ()]
     (if (empty? col)
       result
       (recur (rest col) (concat  result (list (first col)(first col)))))))

(defcheck solution-478f92be
  (fn [s]
    (mapcat (fn [x] [x x]) s)))

(defcheck solution-48221b7d
  (fn duper [i]
    (if (seq i)
      (concat [(first i) (first i)] (duper (rest i)))
      i)))

(defcheck solution-484db6e7
  (fn duplicate
    ([l] (duplicate l []))
    ([l resp]
     (if (= l [])
       resp
       (let [f (first l)]
         (duplicate (rest l) (conj resp f f)))))))

(defcheck solution-485a95f3
  #(reduce (fn [x y] (concat x [y y])) '() %))

(defcheck solution-4a81d6f3
  (fn [x] (reduce #(conj %1 %2 %2) [] x)))

(defcheck solution-4ae204a6
  (fn thing [xs]
    (if (empty? xs) '() (cons (first xs) (cons (first xs) (thing (rest xs)))))))

(defcheck solution-4c07f733
  (fn [coll]
    (apply concat (for [e coll]
                    (list e e)))))

(defcheck solution-4c08680a
  (fn dup[[x & xs]] (if (empty? xs) [x x] (cons x (cons x (dup xs))))))

(defcheck solution-4c16b899
  (fn [x] (reverse (reduce #(apply conj %1 %2 ) (map #(list  % % ) x )))))

(defcheck solution-4d00e07c
  (fn [s]
    (reverse (reduce #(conj % %2 %2) '() s))))

(defcheck solution-4dc379fb
  (fn [xs] (mapcat #(repeat 2 %) xs)))

(defcheck solution-4f184951
  #(reduce (fn [ys x] (conj ys x x)) [] %))

(defcheck solution-503eba41
  (fn [xs] (interleave xs xs)))

(defcheck solution-51243226
  #(reverse (reduce (fn [acc x] (conj acc x x)) () %)))

(defcheck solution-5138e537
  (fn [xs] (reduce concat
             (map (fn [x]
                    (list x x)
                    )
               xs
               )
             )
    ))

(defcheck solution-51451aaa
  (fn [a-seq] (reduce #(conj %1 %2 %2) [] a-seq)))

(defcheck solution-51c6da0c
  (fn duplicateX [x] (mapcat identity (map #(repeat 2 %) x))))

(defcheck solution-5259d22c
  (fn [x] (apply concat (map #(repeat 2 %) x))))

(defcheck solution-526cf819
  (fn my-duplicate [coll]
    (reverse (reduce (fn [a b] (cons b (cons b a)))
               nil coll))))

(defcheck solution-53bc082a
  (fn [s]
    (reduce #(conj %1 %2 %2) [] s)))

(defcheck solution-5426d67a
  (fn [the-seq]

    (reverse (reduce (fn [contenedor item]
                       (conj (conj contenedor item) item)
                       ) '() the-seq))
    ))

(defcheck solution-542d27f3
  (fn [xs]
    (reduce #(concat %1 (list %2 %2))
      '()
      xs)))

(defcheck solution-545cae19
  (fn [s]
    (reduce #(conj (conj %1 %2) %2) [] s)))

(defcheck solution-5584daa2
  #(loop [n 0 acc '()]
     (if (= (count %) n)
       acc
       (recur (inc n) (concat acc (list (nth % n) (nth % n)))))))

(defcheck solution-55b53302
  (fn prob32 [solved remaining]
    (if (= (count remaining) 0)
      solved
      ( prob32 (conj solved (first remaining) ( first remaining)) (rest remaining)))) [])

(defcheck solution-56008a2d
  (fn [xs]
    (reverse (reduce #(conj (conj % %2) %2) '() xs))))

(defcheck solution-565c71b5
  (fn [l] (reduce concat ( map #(repeat 2 %) l ))))

(defcheck solution-5760e00b
  (fn [lst]
    (apply concat (map (partial repeat 2) lst))))

(defcheck solution-58051a52
  (fn [coll]
    (loop [c1 coll, c2 []]
      (if (empty? c1)
        c2
        (recur (rest c1) (conj c2 (first c1) (first c1)))))))

(defcheck solution-58661b4
  (fn [s]
    (reduce (fn [result [a b]] (conj result a b)) [] (map #(vector %1 %2) s s))))

(defcheck solution-58e1741d
  (fn [lat]
    (reduce concat (map #(take 2 (repeat %)) lat))))

(defcheck solution-590fc356
  reduce (fn [xs x] (conj (conj xs x) x) ) [])

(defcheck solution-5a29ebc8
  (fn [sq]
    (reduce (fn [ls x]
              (conj ls x x))
      '()
      (reverse sq))))

(defcheck solution-5a3b8287
  #(mapcat (fn [x] (vector x x)) %))

(defcheck solution-5b0f99ef
  #(seq (reduce (fn [acc item] (-> acc (conj item) (conj item))) [] %)))

(defcheck solution-5b4ce04f
  #(interleave %1 %1))

(defcheck solution-5bda393b
  (fn dup-seq [coll]
    (interleave coll coll)))

(defcheck solution-5be5e491
  (fn [xs] (reduce #(into %1 (repeat 2 %2)) [] xs)))

(defcheck solution-5befb428
  #(reduce concat (map (fn [e] (list e e)) %)))

(defcheck solution-5c0b90b5
  (partial mapcat #(-> [% %])))

(defcheck solution-5ca5ef27
  #(loop [in % out []]
     (if (empty? in)
       out
       (recur (rest in) (conj out (first in) (first in))))))

(defcheck solution-5d065c0c
  (fn [seq] (apply concat (map #(list % %) seq))))

(defcheck solution-5d56f22f
  (fn [x]
    (apply concat
      (map #(vector % %) x)
      )
    ))

(defcheck solution-5da33e6c
  (partial mapcat #(list % %)))

(defcheck solution-5da70080
  #(sort (into (into () %) %)))

(defcheck solution-5da71ff6
  reduce (fn[x y] (conj (conj x y ) y)) [])

(defcheck solution-5e1be49c
  (fn [x]
    (mapcat #(repeat 2 %) x)))

(defcheck solution-5e6645ed
  (fn dupe [[f & r]] (lazy-seq (cons f (cons f (if r (dupe r)))))))

(defcheck solution-5f368428
  (fn dup-mapcat [coll]
    (mapcat (partial repeat 2) coll)))

(defcheck solution-5fd5d96f
  (fn [s] (interleave s s)))

(defcheck solution-6044850a
  (fn [l] (reduce #(cons %2 (cons %2 %1)) () (reverse l))))

(defcheck solution-60caa71e
  mapcat #(-> [% %]))

(defcheck solution-610ee1a3
  (fn [coll] (reduce #(concat %1 (list %2 %2)) () coll)))

(defcheck solution-6243c2a2
  #(apply concat (map (fn [e] [e e]) %)))

(defcheck solution-62e0223d
  (fn dup [x]
    (if (= x '())
      '()
      (cons (first x) (cons (first x) (dup (rest x))))
      )
    ))

(defcheck solution-638de2f9
  (fn [s] (reverse (reduce #(conj %1 %2 %2) '() s))))

(defcheck solution-63c52dd3
  (fn dup
    ([lst] (dup [] lst))
    ([l lst] (if (empty? lst) l (dup (conj l (first lst) (first lst)) (next lst))))))

(defcheck solution-6417d8b8
  #(mapcat list % %))

(defcheck solution-642d9396
  #(mapcat (fn [a] [a a]) %))

(defcheck solution-65040b8e
  mapcat (fn[z](conj [z] z)))

(defcheck solution-651dc9a5
  (fn [list](reduce (fn [x elem]
                      (conj (conj x elem) elem)) [] list)))

(defcheck solution-6581554b
  (fn [xs](mapcat list xs xs)))

(defcheck solution-65dba29c
  (fn [x]
    (loop [x x
           v '()]
      (if (nil? (first x))
        v
        (recur (rest x)
          (concat v (repeat 2 (first x))))))))

(defcheck solution-66788fbc
  (fn [col] (mapcat (fn [a] [a a] ) col)))

(defcheck solution-66c310c9
  (fn dupl[input]
    (reduce
      (fn[l ll]
        (concat l ll))
      ()
      (map #(list % %) input))))

(defcheck solution-66f6c7db
  (fn dupit [x] (if (empty? x) () (lazy-cat (list (first x) (first x)) (dupit (rest x))))))

(defcheck solution-681dec95
  (fn [coll] (apply concat (map (fn [a b] (list a b)) coll coll))))

(defcheck solution-68597729
  (fn d
    ([y] (d y []))
    ([y a]
     (if y
       (let [[x & xs] y]
         (d xs (conj a x x)))
       a))))

(defcheck solution-6874b981
  (fn [x] (reduce (fn [y z] (conj (conj y z) z)) [] x)))

(defcheck solution-6a392600
  #(reduce (fn [coll e] (conj coll e e)) [] %))

(defcheck solution-6a509e3c
  reduce (fn [acc x] (conj acc x x)) [])

(defcheck solution-6b7f97ec
  #(let [my-flatten (fn my-flatten [v]
                      (if (empty? v)
                        []
                        (cons (first (first v))
                          (cons (second (first v))
                            (my-flatten (rest v))))))]
     (my-flatten (for [x %] [x x]))))

(defcheck solution-6b81e78b
  (fn [x] (interleave x x)))

(defcheck solution-6c4ae33e
  (fn dup [in]
    (loop [in-seq in, out-seq []]
      (if (= (first in-seq) nil)
        out-seq
        (recur (rest in-seq) (conj out-seq (first in-seq) (first in-seq) ))
        )
      )
    ))

(defcheck solution-6c4b3a17
  (fn [l]
    (reduce
      (fn [x y]
        (conj x y y)
        )
      []
      l
      )
    ))

(defcheck solution-6d20fb44
  (fn
    [se]
    (sort
      (concat
        se
        se))))

(defcheck solution-6d44f9fd
  (fn [coll]
    (reverse (reduce into (map #(list % %) coll)))))

(defcheck solution-6d47639d
  #(loop [acc '()
          coll %]
     (if-let [f (first coll)]
       (recur (concat acc (list f f)) (rest coll))
       acc
       )
     ))

(defcheck solution-6da70219
  (fn dup [s] (mapcat #(vector % %) s)))

(defcheck solution-6deaaffe
  (fn [coll]
    (apply concat (map #(vector % %) coll))))

(defcheck solution-6e0a011a
  (fn [a] (reverse (reduce #(conj (conj %1 (first %2)) (last %2)) (map #(list %1 %1) a)))))

(defcheck solution-6fd829ae
  (fn duplicate [s] (apply concat (map #(list % %) s))))

(defcheck solution-704795c1
  (fn [lst] (mapcat #(list % %) lst)))

(defcheck solution-70a279c
  #(loop [x % y []] (if-not (empty? x)
                      (recur (rest x) (conj y (first x) (first x)))
                      y)))

(defcheck solution-70b6c033
  (fn dup [coll]
    (if (= coll '()) '()
                     (lazy-cat (list (first coll) (first coll)) (dup (rest coll))))))

(defcheck solution-71feedb2
  (fn [s] (mapcat #(repeat 2 %) s)))

(defcheck solution-7254e05c
  reduce (fn [r x] (conj r x x)) [])

(defcheck solution-72b60391
  (fn
    [coll]
    (mapcat #(repeat 2 %) coll)))

(defcheck solution-734b99c6
  (fn [data] (mapcat #(list % %) data)))

(defcheck solution-73540cde
  #(reverse (reduce (fn [res it] (conj res it it)) `() %)))

(defcheck solution-736d2da7
  (fn dup
    ([s] (dup '() s))
    ([os is] (if (empty? is)
               os
               (recur (concat os [(first is) (first is)]) (rest is))))))

(defcheck solution-7377a146
  (fn [seq]
    (reduce #(concat %2 %1) (reverse (map (fn [x] [x x]) seq)))))

(defcheck solution-738677f8
  #(reduce (fn [acc elem] (conj (conj  acc elem) elem)) [] %))

(defcheck solution-7392ccbc
  #(mapcat (fn [elt] [elt elt]) %))

(defcheck solution-742a7520
  #(for [e % x [e e]] x))

(defcheck solution-748e615a
  (fn dup-seq
    [s]
    (interleave s s)))

(defcheck solution-74f6c1a8
  (fn [s] (seq (reduce #(conj %1 %2 %2) [] s))))

(defcheck solution-75db2ae8
  reduce (fn [v x] (conj v x x)) [])

(defcheck solution-75dc5c21
  (fn [v] (interleave v v)))

(defcheck solution-75e407a4
  (fn duplicate [x]
    (reduce #(conj %1 %2 %2) [] x)))

(defcheck solution-75f2f574
  (fn duplicate-seq [seq]
    (reduce concat (map (fn [el] [el el]) seq))))

(defcheck solution-7623653a
  #(apply concat (for [x %] (conj () x x))))

(defcheck solution-765b9ce0
  (fn [v] (reduce #(conj %1 %2 %2) [] v)))

(defcheck solution-76d424ce
  (fn
    [seq]
    (loop [seq seq list '()]
      (if (= nil (first seq))
        (reverse list)
        (recur (rest seq) (conj list (first seq) (first seq)))
        )
      )))

(defcheck solution-76e5ab46
  (fn duplicate-elements [coll]
    (mapcat #(vector % %) coll)))

(defcheck solution-77ef254f
  (fn [s]
    (reduce concat (map (partial repeat 2) s))))

(defcheck solution-781edfa0
  #(reduce into [] (map (fn [x] [x x]) %)))

(defcheck solution-783e5216
  (fn stutter [xs]
    (let [y (first xs), ys (rest xs)]
      (cond
        (empty? ys) (list y y)
        :else (concat (list y y) (stutter ys))))))

(defcheck solution-787a957a
  (fn dupe [col]
    (if (empty? col)
      col
      (cons (first col) (cons (first col) (dupe (rest col)))))))

(defcheck solution-78a06cce
  (fn m [coll]
    (mapcat #(list % %) coll)))

(defcheck solution-78de317e
  (fn dup [x]
    (mapcat #(list % %) x)
    ))

(defcheck solution-79080937
  (fn [xs] (mapcat #(list % %) xs)))

(defcheck solution-7a376179
  (fn duplica [s]
    (cond
      (empty? s) nil
      :else (cons (first s) (cons (first s) (duplica (next s)))))))

(defcheck solution-7a3906d0
  (fn [coll]
    (reverse (reduce (fn [accum elem] (conj accum elem elem)) '() coll))
    ))

(defcheck solution-7adb3a03
  (fn [sq]
    (let [aux
          (fn [[head & tail] acc]
            (cond
              (nil? head)
              acc

              :else
              (recur tail (conj acc head head))))]
      (aux sq []))))

(defcheck solution-7cff01e8
  (fn [coll] (mapcat (fn [x] [x x]) coll)))

(defcheck solution-7d5babb2
  #(loop [col % result []]
     (if(empty? col)
       result
       (let [colFirst (first col)]
         (recur (rest col) (conj result colFirst colFirst))))))

(defcheck solution-7da424e
  (fn dupseq
    [arr]
    (reduce concat (map (fn [x] [x x]) arr))))

(defcheck solution-7e748150
  (fn dup [s]
    (if-let [[x & xs] (seq s)]
      (cons x (cons x (dup xs))))))

(defcheck solution-7f60fbfe
  (fn [lst] (reduce (fn [xs x] (conj xs x x)) [] lst)))

(defcheck solution-7fa20937
  (fn [coll]
    (mapcat list coll coll)))

(defcheck solution-801c1b27
  (fn [s]
    (reduce
      (fn [x y]
        (conj x y y)) [] s)))

(defcheck solution-804c766d
  (fn [a]
    (reverse
      (reduce #(cons %2 (cons %2 %1)) '() a)
      )))

(defcheck solution-8070bfff
  (fn [x] (apply concat (map list x x))))

(defcheck solution-80e457e4
  (fn [s] (apply concat (map #(list % %) s))))

(defcheck solution-813fa9ac
  (comp (partial reduce #(into %1 [%2 %2]) [])))

(defcheck solution-81766350
  (fn dup [seq] (reduce (fn [acc e] (conj (conj acc e) e)) [] seq)))

(defcheck solution-81c96f4e
  (fn rep [x] (mapcat #(repeat 2 %) x)))

(defcheck solution-839c7284
  ;(fn dou [x]
  ;  (if (= (count x) 0) '() (conj (dou (rest x)) (first x) (first x) )
  ;  )
  ;)
  (fn [x] (mapcat #(list % %) x)))

(defcheck solution-83de7046
  (fn [vs]
    (interleave vs vs)))

(defcheck solution-84a802e0
  (fn [x] (reduce (fn [y z] (conj y z z)) [] x)))

(defcheck solution-84e47566
  (fn g ([[x & rest]] (if x (conj (g rest) x x)))))

(defcheck solution-862fb319
  (fn[x](reverse (reduce #(cons %2 (cons %2 %)) '() x))))

(defcheck solution-865418b1
  #((fn dupl [x,y] (
                     if (= (count x) y)
                     x
                     (dupl (concat (rest x) (repeat 2 (first x))) (+ y 2))
                     ))
    % 0))

(defcheck solution-86581930
  #(mapcat (partial replicate 2) %))

(defcheck solution-869ec095
  (fn [xs] (for [x xs k (range 2)] x)))

(defcheck solution-86cb6d74
  (fn [x]
    ((fn dupelts [coll]
       (if-let [[f & r] (seq coll)]
         (cons f (cons f (dupelts r)))
         ())) x)))

(defcheck solution-87900029
  (fn dup [s]
    (loop [a s
           b ()]
      (if (empty? a)
        (reverse b)
        (recur (rest a) (conj b (first a) (first a)))))))

(defcheck solution-87fdbd6c
  mapcat #(list % %))

(defcheck solution-88092093
  #(letfn [(f [x acc] (if (empty? x) acc (f (rest x) (conj acc (first x) (first x)))))]
     (f % [])))

(defcheck solution-88abb5ce
  (fn [ls] (apply concat (map #(list % %) ls))))

(defcheck solution-894642e4
  (fn [vs] (reverse (reduce #(cons %2 (cons %2 %1)) [] vs))))

(defcheck solution-8997515b
  (fn [xs]
    (reduce (fn [rs y] (concat rs [y y])) '() xs)))

(defcheck solution-89fbb2e9
  #(reduce (fn [a b] (concat a [b b])) '() %))

(defcheck solution-8a2b420d
  (fn [input]
    (loop [s input a '()]
      (if (empty? s)
        (reverse a)
        (recur (rest s) (cons (first s) (cons (first s) a)))))))

(defcheck solution-8a3373ff
  (fn [xs] (for [x xs
                 y [x x]]
             y)))

(defcheck solution-8a4a06de
  mapcat (fn [a] [a a]))

(defcheck solution-8a500682
  ; first-attempt:
  ;(fn cope [coll]
  ;  (if (empty? coll)
  ;    []
  ;    (cons (first coll) (cons (first coll) (cope (rest coll))))))

  #(interleave % %))

(defcheck solution-8aaf46ae
  mapcat #(cons % (list %)))

(defcheck solution-8aed2a27
  (fn[a-seq]
    (mapcat (fn[a](repeat 2 a)) a-seq)))

(defcheck solution-8c9c4327
  (fn myduplicate
    [myseq]
    (reduce concat
      (map #(list % %) myseq))))

(defcheck solution-8cbd3a85
  (partial mapcat #(vector % %)))

(defcheck solution-8ceed281
  (partial reduce #(conj %1 %2 %2) []))

(defcheck solution-8d2bee75
  (fn [coll]
    (reduce (fn [acc x]
              (concat acc [x] [x]))
      []
      coll)))

(defcheck solution-8d958260
  #(reduce concat (for [item %] [item item])))

(defcheck solution-8e45fdab
  (fn [l]
    (apply concat (map #(repeat 2 %) l))))

(defcheck solution-8edfefa7
  (fn duplicate [seq]
    (interleave seq seq)))

(defcheck solution-8f3d33d3
  (fn s-dup [s] (apply concat (for [x s] (list x x)))))

(defcheck solution-8f6655c0
  reduce #(apply conj %1 (list %2 %2)) [])

(defcheck solution-8fe3e842
  (fn [s]
    (loop [xs s result []]
      (if (empty? xs)
        result
        (recur (next xs) (conj result (first xs) (first xs)))))))

(defcheck solution-90100ceb
  (fn [s]
    (mapcat #(list % %) s)
    ))

(defcheck solution-90b56255
  (fn [lst]
    (apply concat (map (fn [i] (list i i)) lst))))

(defcheck solution-90dcea67
  (fn [s] (reduce #(-> %1 (conj %2) (conj %2)) [] s)))

(defcheck solution-90fba76b
  (fn [xs] (reduce (fn [x y] (conj x y y)) [] xs)))

(defcheck solution-92170ead
  (fn [l] (reduce #(conj %1 %2 %2) [] l)))

(defcheck solution-928730ee
  (fn duplicate [s]
    (reduce (fn [result v] (conj result v v)) [] s)))

(defcheck solution-9465daac
  (fn duplicate [x] (if (empty? x) [] (concat (list (first x)) (list (first x)) (duplicate (rest x))))))

(defcheck solution-946fb666
  (fn [x] (apply concat (map #(list % %) x))))

(defcheck solution-947ac6b3
  mapcat #(do [% %]))

(defcheck solution-947e79f9
  (fn myDuplicate
    [sequence]
    (apply concat (map (partial repeat 2) sequence))))

(defcheck solution-955dfb03
  (fn [l]
    (reduce (fn [x y] (concat x y)) (map (fn [x]
                                           (list x x)
                                           ) l))
    ))

(defcheck solution-95cdc544
  (fn [items] (reverse (reduce #(conj %  %2 %2) '() items))))

(defcheck solution-95d94722
  (fn [x] (reduce #(apply conj %1 (list %2 %2)) [] x)))

(defcheck solution-96319434
  (fn dup [items]
    (if (empty? items)
      '()
      (conj (dup (rest items)) (first items) (first items)))))

(defcheck solution-9639b1a7
  (fn
    [aseq]
    (loop [lseq aseq el (first lseq) acc []]
      (if (empty? lseq)
        acc
        (recur
          (rest lseq)
          (first (rest lseq))
          (into acc (repeat 2 el)))))))

(defcheck solution-96c7417f
  (fn [xs] (mapcat (fn [x] [x x]) xs)))

(defcheck solution-971e80ce
  reduce (fn [coll item] (concat coll [item item])) [])

(defcheck solution-9782413f
  (fn [s] (sort (take (* 2 (count s)) (cycle s)))))

(defcheck solution-978fc9a2
  ;;(fn duplicate
  ;;  [collection]
  ;;  (reduce (fn [c val]
  ;;            (conj c val val)) [] collection))
  reduce #(conj %1 %2 %2) [])

(defcheck solution-98813f8b
  #(loop [ l %1 l2 []] (let [f (first l)] (if (= (count l) 0) l2 (recur (rest l) (concat l2 (list f f)))))))

(defcheck solution-99fc11d0
  #(apply concat (map (partial replicate 2) %)))

(defcheck solution-9b7c53ac
  (partial mapcat (partial repeat 2)))

(defcheck solution-9b8dbfc8
  #(reduce (fn [c x] (conj c x x)) [] %))

(defcheck solution-9c27813e
  (fn  [col]
    (apply concat (for [c col] (list c c)))))

(defcheck solution-9d27ca1f
  (fn [xs]
    (apply concat
      (map #(vector % %) xs))))

(defcheck solution-9d2fbd1d
  (fn dupl-seq
    [s]
    (reduce #(conj %1 %2 %2) [] s)))

(defcheck solution-9d633f3b
  #(for [x % i [0 1]] x))

(defcheck solution-9f1e6163
  (fn dup-els [s]
    (when (seq s) (let [[f & r] s] (cons f (cons f (dup-els r)))))
    ))

(defcheck solution-9ffdce03
  mapcat (partial repeat 2))

(defcheck solution-a010bc41
  (fn duplicate [x] (reduce concat [] (map #(vector % %) x))))

(defcheck solution-a193088f
  mapcat #(identity [% %]))

(defcheck solution-a1b65b9b
  (fn [l] (reduce #(conj % %2 %2) [] l)))

(defcheck solution-a29f00ba
  (fn [s]
    (apply concat (map #(list % %) s))))

(defcheck solution-a3bdbb07
  (fn dup-seq [a-seq]
    (reverse (reduce #(conj %1 (first %2) (second %2))  () (map #(list % %) a-seq)))
    ))

(defcheck solution-a3e2ab62
  (fn [s]
    (letfn [(dpsq [s result]
              (if (seq s)
                (let [f (first s)]
                  (recur (rest s) (conj result f f)))
                result))]
      (dpsq s []))))

(defcheck solution-a3f38745
  (partial mapcat #(take 2 (repeat %))))

(defcheck solution-a4190291
  (fn [s] (mapcat (fn [x] [x x]) s)))

(defcheck solution-a42f55e3
  reduce #(concat %  (list %2 %2)) ())

(defcheck solution-a47d9f19
  #(interleave % %))

(defcheck solution-a5aee90
  (fn [s]
    (mapcat #(identity [% %]) s)))

(defcheck solution-a6a69aea
  (fn [coll]
    (loop [result [] input coll]
      (if (nil? (seq input))
        result
        (recur (conj result (first input) (first input)) (rest input))
        ))))

(defcheck solution-a78c986b
  (fn [x]  (reduce concat (map (fn [item] (list item item) ) x))))

(defcheck solution-a7bbb74
  (fn [values]
    (loop [values values result []]
      (if (empty? values)
        result
        (let [item (first values)]
          (recur (rest values) (conj result item item)))))))

(defcheck solution-a7ed6f19
  mapcat (fn [a] `(~a ~a)))

(defcheck solution-a85ef557
  #(reduce (fn [s v] (conj s v v)) [] %))

(defcheck solution-a92a1d80
  (fn [coll]
    (reduce
      (fn [r x]
        (conj r x x))
      []
      coll
      )))

(defcheck solution-a94fcb25
  (fn ! [s]
    (if (empty? s) (empty s)
                   (let [f (first s) r (rest s)]
                     (cons f (cons f (! r)))
                     )
                   )
    ))

(defcheck solution-aac1f569
  (fn dup [xs]
    (apply concat
      (for [x xs]
        [x x]))))

(defcheck solution-ab6a3671
  (fn [x]
    (apply concat (map (fn [e] [e e]) x))))

(defcheck solution-ad247864
  #(reduce
     into
     (for [x %]
       [x x])))

(defcheck solution-ad739d49
  #(reduce (fn [acc v] (conj acc v v)) [] %))

(defcheck solution-ad85c72a
  (fn dup [xs] (if (empty? xs) xs (let [x (first xs)] (concat [x x] (dup (rest xs)))))))

(defcheck solution-ae06a9a7
  #(loop [x % y []] (if (= (count x) 0) y (recur (rest x) (conj y (first x) (first x))))))

(defcheck solution-ae5e086f
  #(reduce (fn [x y] (conj x y y)) [] %))

(defcheck solution-af0cd571
  (fn dbl [r s]
    (if (> (count s) 1)
      (cons (first s) (cons (first s) (dbl r (rest s))))
      (cons (first s) (cons (first s) r)))) '())

(defcheck solution-af1de0bc
  (partial reduce
    (fn [memo el] (-> memo (conj el) (conj el)))
    []))

(defcheck solution-b028b4f9
  (fn [x]
    (loop [aggr [] coll x]
      (if (empty? coll)
        aggr
        (recur (conj aggr
                 (first coll)
                 (first coll))
          (rest coll))))))

(defcheck solution-b02dc3d6
  (fn [simple]
    (loop [result [] elements simple]
      (if (empty? elements)
        result
        (recur (conj (conj result (first elements)) (first elements)) (rest elements))
        )
      )
    ))

(defcheck solution-b0da726d
  #(letfn [(worker [x n]
             (if (empty? x)
               n
               (recur (rest x) (conj (conj n (first x)) (first x)))))]
     (worker % [])))

(defcheck solution-b10c1570
  (fn [s]
    (mapcat #(repeat 2 %) s)))

(defcheck solution-b13f8b8b
  (fn [lst] (reduce #(concat %1 (list %2 %2)) () lst)))

(defcheck solution-b140bee8
  (fn [sq] (apply concat (map (fn [x] (list x x)) sq))))

(defcheck solution-b16e19cb
  (fn [x] (loop [consume x return '()] (if (empty? consume) (reverse return) (recur (rest consume) (cons (first consume) (cons (first consume) return)))))))

(defcheck solution-b1df25d1
  #(reduce (fn [a e] (conj a e e)) [] %))

(defcheck solution-b2b7b9fc
  (fn [ls] (reduce #(conj (conj %1 %2) %2) [] ls)))

(defcheck solution-b4626df3
  (fn dup [coll]
    (apply concat
      (for [item coll]
        (list item item)))))

(defcheck solution-b4a47ba2
  (letfn [(dup [coll]
            (when-let [s (seq coll)]
              (lazy-seq (let [fst (first s)]
                          (cons fst (cons fst (dup (next s)) )) ))))] dup))

(defcheck solution-b56d9189
  mapcat #(conj '() % %))

(defcheck solution-b5915cbc
  (fn dup
    [l]
    (apply concat (map #(list % %) l))))

(defcheck solution-b59bcc33
  (fn f [[h & t]]
    (if (empty? t)
      [h h]
      (concat [h h] (f t)))))

(defcheck solution-b5ffe65e
  (fn [x] (reverse (reduce #(cons %2 (cons %2 %1)) '() x))))

(defcheck solution-b6579801
  #(apply concat (map (fn [n] (repeat 2 n)) %)))

(defcheck solution-b6b10fef
  (fn [initial-coll]
    (reverse
      (reduce
        (fn [coll val]
          (conj coll val val))
        '()
        initial-coll))))

(defcheck solution-b7106ecf
  (fn [x] (mapcat #(list % %) x)))

(defcheck solution-b72b4797
  (fn duplicate [coll]
    (reduce concat (map #(take 2 (repeat %)) coll))))

(defcheck solution-b738bf74
  (fn [col] (interleave col col)))

(defcheck solution-b76bb547
  (fn dupeeach [sq]
    (reduce #(conj (conj %1 %2) %2) [] sq)))

(defcheck solution-b8e88c29
  (fn [l] (interleave l l)))

(defcheck solution-b9bd136d
  (fn [l] (reduce concat (map #(list % %) l))))

(defcheck solution-b9c1df4b
  (fn  [lst]
    (loop [rm (seq lst), acc (vector)]
      (cond (empty? rm) (seq acc)
            :else (recur (rest rm) (conj acc (first rm) (first rm)))))))

(defcheck solution-b9fd5fb4
  (fn [coll]
    (reduce #(conj %1 %2 %2) [] coll)))

(defcheck solution-baa411c4
  ; (fn [xs] (reduce concat (map #(list % %) xs)))

  (fn [xs] (mapcat #(list % %) xs)))

(defcheck solution-bb793b7d
  (fn [sq] (reduce #(conj % %2 %2) [] sq)))

(defcheck solution-bbb6ff07
  #(apply concat (for [x %] [x x])))

(defcheck solution-bbd0c1a1
  (fn [seq]
    (interleave seq seq)))

(defcheck solution-bcf1b8a4
  (fn[_seq](reverse(reduce (fn[result head]
                             (conj (conj result head) head)) '() _seq))))

(defcheck solution-bd326fd1
  (fn dup [s] (if (seq s)
                (lazy-seq (cons (first s) (cons (first s) (dup (rest s)))))
                nil)))

(defcheck solution-bd73f42b
  (fn dup [coll]
    (loop [[x & xs] coll
           res []]
      (if (nil? x)
        res
        (recur xs
          (conj res x x))))))

(defcheck solution-be5dceec
  (fn [x] (reduce into (map #(vector % %) x))))

(defcheck solution-be73bcdf
  (partial reduce #(concat % [%2 %2]) ()))

(defcheck solution-be7ed60b
  (fn inter[s](when (seq s) (concat [(first s) (first s)] (inter (rest s))))))

(defcheck solution-c0a46f17
  #(reduce (fn [a b] (conj a b b)) [] %))

(defcheck solution-c1bda78e
  #(sort (concat % %)))

(defcheck solution-c284c078
  #(reverse (reduce (fn [t v] (conj t v v)) '() %)))

(defcheck solution-c288f8d5
  reduce (fn [coll, elem]
           (conj coll elem elem)) [])

(defcheck solution-c2f0536b
  reduce #(concat %1 (list %2 %2)) ())

(defcheck solution-c33ae101
  (fn [coll] (reduce (fn [coll item] (concat coll (list item item))) '() coll)))

(defcheck solution-c3455b0
  (fn [coll]
    (reverse (reduce #(conj %1 %2 %2) '() coll))))

(defcheck solution-c41d514a
  (fn dupler [c]
    (interleave c c)
    ))

(defcheck solution-c4b0fb5d
  (fn [coll]
    (interleave coll coll)))

(defcheck solution-c4bc2b77
  (fn [s]
    (apply concat (map #(list % %) s))))

(defcheck solution-c4cb94b9
  (fn [x]
    (reverse
      (reduce
        #(apply conj %1 %2) '() (map #(repeat 2 %) x)))))

(defcheck solution-c4f56fc4
  reduce (fn [s x] (conj s x x)) [])

(defcheck solution-c6c01656
  #(reduce
     (fn [nlst v]
       (conj nlst v v))
     [] %))

(defcheck solution-c6eefa48
  reduce #(conj %1 %2 %2) [])

(defcheck solution-c7ba52bf
  (fn [S]
    (reduce (fn [lst x] (concat lst (list x x))) () S)
    ))

(defcheck solution-c7f62620
  #(apply interleave (repeat 2 %)))

(defcheck solution-c8629087
  reduce (fn [l x] (concat l (list x x))) ())

(defcheck solution-c878e457
  (partial reduce
    (fn [l e]
      (conj (conj l e) e))
    []))

(defcheck solution-c8f53a95
  (fn dupe [inseq]
    (reduce #(concat %1 [%2] [%2]) [] inseq)))

(defcheck solution-ca305a91
  (fn [s] (reduce #(conj %1 %2 %2) [] s)))

(defcheck solution-cbb7b031
  mapcat (fn [x] (list x x)))

(defcheck solution-cbc46d08
  (fn [x] (mapcat #(conj (conj [] %) %) x)))

(defcheck solution-cc09275
  (fn [coll] (mapcat #(list % %) coll)))

(defcheck solution-cc25c102
  #(reduce (fn [a b] (concat a (list b b)))
     nil
     %))

(defcheck solution-cc526df6
  (fn foo [s] (interleave s s)))

(defcheck solution-cc87d800
  (fn dupl [s]
    (if (empty? s)
      '()
      (conj (dupl (rest s)) (first s) (first s)))))

(defcheck solution-ccfb7f85
  #(reduce (fn [a b] (conj (conj a b) b)) [] %))

(defcheck solution-cd3675ae
  #(for [x % y [x x]] y))

(defcheck solution-cd500c69
  mapcat (fn[x][x x]))

(defcheck solution-cdbfb9a8
  #(mapcat (fn [a] (list a a)) %))

(defcheck solution-ce08a27d
  (fn [x] (reduce #(into %1 (repeat 2 %2)) () (reverse x))))

(defcheck solution-ce903f6f
  (fn [l]
    (apply concat (map (fn [x] [x x]) l))))

(defcheck solution-ced8326f
  mapcat (fn [n] [n n]))

(defcheck solution-cf07895d
  (fn [xs]
    (apply concat (map (fn [x] [x x]) xs))))

(defcheck solution-cfcb47fd
  (fn [seq]
    (apply concat (map #(repeat 2 %) seq))))

(defcheck solution-d07c1cb0
  #(reduce (fn [s i] (conj s i i)) [] %))

(defcheck solution-d083cc3a
  #(reduce (fn [f n] (conj (conj f n) n)) [] %))

(defcheck solution-d0a3a216
  (fn dup [x]
    (if (empty? x)
      x
      (conj (dup (rest x)) (first x) (first x))
      )

    ))

(defcheck solution-d0de91cd
  (fn [coll]
    (mapcat list coll coll)
    ))

(defcheck solution-d0fadf41
  (fn [x] (mapcat (fn [a b] [a b]) x x)))

(defcheck solution-d11005b6
  mapcat (juxt identity identity))

(defcheck solution-d1fef0a3
  (partial (fn [n s]
             (reduce #(concat %1
                        (vector %2 %2)) n s))
    []))

(defcheck solution-d2a4512
  #(reduce (fn [v i] (conj (conj v i) i)) [] %))

(defcheck solution-d2c7bc59
  (fn [s] (reduce #(conj % %2 %2) [] s)))

(defcheck solution-d3576cf9
  (fn dup[x]
    (if (= (count x) 1) (conj x (first x))
                        (conj (dup (rest x)) (first x) (first x)))
    ))

(defcheck solution-d3be6262
  (partial mapcat (juxt identity identity)))

(defcheck solution-d45bb29b
  (fn [xs]
    (sort (take (* 2 (count xs)) (cycle xs)))))

(defcheck solution-d50e5ab3
  (fn [s] (mapcat vector s s)))

(defcheck solution-d5268cfb
  (fn x [s]
    (if (empty? s)
      '()
      (cons
        (first s)
        (cons
          (first s)
          (lazy-seq (x (rest s))))))))

(defcheck solution-d5c7a952
  (fn [a] (interleave a a)))

(defcheck solution-d5cfe722
  (fn [coll] (mapcat #(repeat 2 %) coll)))

(defcheck solution-d5d76ac7
  #(loop [ coll []  int %]
     (if (empty? int) coll
                      (recur (conj coll (first int) (first int))  (rest int)
                        ))))

(defcheck solution-d62fbfaf
  (fn [sq] (mapcat #(repeat 2 %) sq)))

(defcheck solution-d78505c2
  ;(fn [s] (reduce #(conj %1 %2 %2) [] s))
  #(interleave % %))

(defcheck solution-d8373a6e
  (fn [s]
    (loop [s s ds []]
      (if (empty? s)
        ds
        (recur (rest s) (conj (conj ds (first s)) (first s)))))))

(defcheck solution-d8510398
  #(letfn [(dbl [r] (if (= '() r) '()
                                  (cons (first r) (cons (first r) (dbl (rest r))))))]
     (dbl %)))

(defcheck solution-d87e5d1d
  (fn [coll]
    (mapcat #(vector % %) coll)))

(defcheck solution-d88ccec6
  reduce #(into % [%2 %2]) [])

(defcheck solution-d8bc0711
  #(interleave % %))

(defcheck solution-d974a0c4
  (fn [s] (reduce #(concat %1 (repeat 2 %2)) [] s)))

(defcheck solution-d98914b6
  (fn [coll] (reduce #(conj %1 %2 %2) [] coll)))

(defcheck solution-d9a18724
  #(mapcat vector % %))

(defcheck solution-d9f9c61a
  #(loop [[head & tail] %
          acc []]
     (if (nil? head)
       acc
       (recur tail (conj acc head head))
       )
     ))

(defcheck solution-daadb3c0
  (fn [sq]
    ((fn [sq r]
       (if (empty? sq)
         r
         (recur (rest sq) (concat r [(first sq) (first sq)]))))
     sq [])))

(defcheck solution-dcb153b6
  (fn [coll] (mapcat #(vector % %) coll)))

(defcheck solution-dcfb9d42
  (fn duplicate [x]
    "Adds a copy of each element of the sequence."
    (interleave x x)))

(defcheck solution-dd300073
  (fn dupseq [mycol] (if (empty? mycol) '() (let [firstElem (first mycol)] (concat (list firstElem firstElem) (dupseq (rest mycol)))))))

(defcheck solution-dd5a2c12
  (fn [col]
    (loop [col1 col fcol []]
      (if (empty? col1)
        fcol
        (recur (rest col1) (conj fcol (first col1) (first col1))
          )))))

(defcheck solution-ddaa944a
  (fn duplicate-a-seq [coll]
    (apply concat
      (map #(list % %) coll))))

(defcheck solution-dedd906b
  (fn [y] (apply concat (map (fn[x] [x x]) y))))

(defcheck solution-df3a9cee
  (fn dup [x]
    (if (empty? x) x
                   (cons (first x) (cons (first x) (dup (rest x)))))))

(defcheck solution-e0a86a16
  (fn [x] (loop [r x result '()] (if (empty? r) result (recur  (rest r) (concat result (list (first r) (first r))))))))

(defcheck solution-e0c4205d
  #(apply concat (map (juxt identity identity) %)))

(defcheck solution-e136c1fe
  (fn [s] (loop [s s
                 result []]
            (if (empty? s)
              result
              (let [my-first (first s)]
                (recur (rest s) (conj result my-first my-first) ))))))

(defcheck solution-e2d0c2e8
  (fn [coll] (mapcat (fn [a] [a a]) coll)))

(defcheck solution-e3327f6
  (fn [v] (mapcat #(conj [%] %) v)))

(defcheck solution-e38246f0
  #(apply concat (map vector % %)))

(defcheck solution-e3af9fc0
  (fn dbl [[f & n]]
    (if (empty? n)
      [f f]
      (concat [f f] (dbl n)))))

(defcheck solution-e4100101
  (fn dupl [items]
    (if (empty? items)
      ()
      (let [f (first items) r (rest items)]
        (concat [f f] (dupl r))))))

(defcheck solution-e4604f99
  #(reduce (fn [r x](conj r x x)) [] %))

(defcheck solution-e4ae0d25
  #((fn [x y] (if (empty? x) y (recur (rest x) (concat y [(first x) (first x)])))) % []))

(defcheck solution-e55c079a
  (fn [coll]
    (mapcat #(list % %) coll)))

(defcheck solution-e599f3e7
  (fn [coll]
    "32. Write a function which duplicates each element of a sequence."
    (mapcat (fn [x] [x x]) coll)))

(defcheck solution-e689c784
  (fn [x]
    (loop [s x r []]
      (if (empty? s) r
                     (recur (rest s) (conj r (first s) (first s)))))))

(defcheck solution-e79d316f
  (fn [n] (interleave n n)))

(defcheck solution-e7c68c4d
  (fn [x]
    (loop [todo x
           res []]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (next todo)]
          (recur r (conj res f f)))))))

(defcheck solution-e8f29f6a
  (fn [coll] (apply concat (map #(repeat 2 %) coll))))

(defcheck solution-e91fcfbe
  (fn duplicate
    [coll]
    (apply mapcat list (repeat 2 coll))))

(defcheck solution-e95237a4
  (fn [x] (reverse (reduce #(conj % %2 %2) '() x))))

(defcheck solution-e9a788ca
  reduce (fn [container value] (conj container value value)) [])

(defcheck solution-e9c0193d
  (fn _ [s]
    (if (empty? s)
      '()
      (conj (_ (rest s)) (first s) (first s))
      )
    ))

(defcheck solution-ea369209
  (fn [s]
    (loop [s_ s ret '()]
      (if s_
        (let [a (first s_)]
          (recur (next s_) (conj ret a a)))
        (into '() ret)))))

(defcheck solution-ea4d33a5
  reduce #(concat %1 (list %2) (list %2)) ())

(defcheck solution-eb6c969e
  (fn dup
    ([someseq] (dup someseq []))
    ([someseq result]
     (if (empty? someseq)
       result
       (dup (rest someseq) (into result (repeat 2 (first someseq))))))))

(defcheck solution-ebb2eef1
  (comp (partial reduce concat nil) (partial map (partial replicate 2))))

(defcheck solution-ecfd687d
  (fn duplicate [xs]
    (if (empty? xs)
      '()
      (cons (first xs) (cons (first xs) (duplicate (rest xs)))))))

(defcheck solution-ed8c7024
  #(loop [r '() coll %]
     (if (empty? coll) r
                       (recur (concat  r (take 1 coll) (take 1 coll))
                         (drop 1 coll)))))

(defcheck solution-ee184838
  (fn dupl
    [c]
    (reduce #(concat % (list %2 %2)) '() c)))

(defcheck solution-ee72cb3c
  (fn [input]
    (loop [remaining input
           ans []]
      (if (empty? remaining)
        ans
        (let [el (first remaining)
              remain (rest remaining)]
          (recur remain (conj ans el el)))))))

(defcheck solution-ee7e65e1
  (fn dup [ lst ]
    (if (empty? lst) '()
                     (let [ fst (first lst)]
                       (cons fst (cons fst (dup (rest lst))))))))

(defcheck solution-ef1fa01b
  (fn [coll] (reverse (reduce #(conj %1 %2 %2) () coll))))

(defcheck solution-ef36caeb
  (fn [l]
    (apply concat (map #(list % %) l))))

(defcheck solution-efc80f44
  #(sort (into % %)))

(defcheck solution-f08973f0
  #(for [v % _ (range 2)] v))

(defcheck solution-f09d399a
  (fn [x] (apply concat (map #(vector % %) x))))

(defcheck solution-f12b5696
  reduce (fn [xs x] (concat xs (cons x (cons x ())))) ())

(defcheck solution-f31d301b
  (fn dup [l]
    (mapcat (fn [x] [x x]) l)))

(defcheck solution-f3687ab6
  (fn [x]
    (reverse (reduce
               (fn [x y]
                 (cons y (cons y x))
                 )
               '()
               x
               ))
    ))

(defcheck solution-f3c95063
  (fn [elems]
    (mapcat #(repeat 2 %) elems)))

(defcheck solution-f3f4f979
  (partial reduce (fn [x y] (conj x y y)) []))

(defcheck solution-f4326b50
  (fn [v] (apply concat  (map #(list % %) v))))

(defcheck solution-f4af988b
  (fn [s] (reduce #(conj (conj %1 %2) %2) [] s)))

(defcheck solution-f4f76c9c
  (fn [l]
    (loop [l l rv []]
      (if (empty? l)
        rv
        (recur (rest l) (conj rv (first l) (first l)))))))

(defcheck solution-f53838b0
  (fn [list]
    (loop [[h & t] list
           accum []]
      (let [new-accum (conj accum h h)]
        (if (nil? t)
          (into () (reverse new-accum))
          (recur t new-accum))))))

(defcheck solution-f565807e
  (fn [col]
    (reduce #(conj %1 %2 %2) [] col)))

(defcheck solution-f5757e91
  (fn number32 [xs]
    (mapcat identity  (map #(list % %) xs))))

(defcheck solution-f6fc7f16
  (fn [x] (apply concat (map (partial repeat 2) x))))

(defcheck solution-f7348d7d
  #(apply interleave (take 2 (repeat %))))

(defcheck solution-f9fbad1d
  (fn dbl [c]
    (when (not (empty? c))
      (let [x (first c)]
        (concat [x x] (dbl (rest c)))))))

(defcheck solution-fa564efb
  reduce #(conj (conj % %2) %2) [])

(defcheck solution-fa71e8ef
  (fn [xs] (apply concat (map vector xs xs))))

(defcheck solution-fac9cd37
  (fn [s] (loop [s s r '()]
            (if (seq s)
              (recur (rest s) (concat r [(first s)
                                         (first s)]))

              r)
            )
    ))

(defcheck solution-faffb6bd
  #(mapcat (partial repeat 2) %))

(defcheck solution-fb0b746f
  (partial mapcat #(repeat 2 %)))

(defcheck solution-fc7a2fcc
  #(interleave % %))

(defcheck solution-fdecadd2
  (fn [x]
    (reduce #(conj %1 %2 %2) [] x)))

(defcheck solution-fdf9866a
  #(mapcat identity (map vector %1 %1)))

(defcheck solution-ffa7b769
  (fn rec [ls]
    (lazy-seq
      (if (empty? ls) ()
                      (concat
                        (repeat 2 (first ls))
                        (rec (rest ls)))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-32))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
