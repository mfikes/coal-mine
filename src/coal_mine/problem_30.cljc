(ns coal-mine.problem-30
  (:require [coal-mine.checks :refer [defcheck-30] :rename {defcheck-30 defcheck}]
            [clojure.test]))

(defcheck solution-106b258
  (fn [sq]
    (let [aux
          (fn [[head & tail] last-seen acc]
            (cond
              (nil? head)
              acc

              (= head last-seen)
              (recur tail head acc)

              :else
              (recur tail head (conj acc head))))]
      (aux sq nil []))))

(defcheck solution-125414f
  #(cons (first %) (for [[x y] (partition 2 1 %) :when (not= x y)] y)))

(defcheck solution-12846d8b
  #(seq (reduce (fn [v x] (if (= (last v) x) v (conj v x))) [] %)))

(defcheck solution-12de705c
  #(reduce (fn [x y] (if (= (last x) y) x (conj x y)) ) [] %))

(defcheck solution-1306085d
  (fn compress [coll]
    (reduce #(if (= (last %) %2)
               %
               (into % (vector %2))) [] coll)))

(defcheck solution-13133def
  (fn [lst] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] lst)))

(defcheck solution-133799cb
  #(reduce
     (fn [r a]
       (if (= a (first r))
         r
         (conj r a)))
     '()
     (reverse %)))

(defcheck solution-143b0786
  #(let [s (vec %)]
     (loop [cnt 0
            result [(first s)]]
       (if (< cnt (- (count s) 1))
         (recur (inc cnt) (if (= (s cnt)(s (+ cnt 1))) result (concat [(s (+ cnt 1))] result)))
         (reverse result)))))

(defcheck solution-146a244c
  (fn [s] (reduce (fn [xs x] (if (= (last xs) x) xs (conj xs x))) [] s)))

(defcheck solution-14d4cc5f
  (fn [coll]
    (->>
      (seq coll)
      (reduce
        (fn s [[acc pred] e]
          (if (pred e)
            [(conj acc e) (fn [x] (not= e x))]
            [acc pred]))
        [[] identity])
      first)))

(defcheck solution-14de64f4
  (fn un-dob [xx]
    (list*
      (reduce

        (fn [a x]
          (if (= (last a) x)
            a (conj a x)))

        []

        (vec xx)))))

(defcheck solution-15404a83
  (fn C [l]
    (loop [l l last false result []]
      (cond (empty? l) result
            (= (first l) last) (recur (rest l) last result)
            :else (recur (rest l) (first l) (conj result (first l)))))))

(defcheck solution-15bd6f67
  #(reduce (fn [f s] (if (= s (peek f)) f (conj f s))) [] (vec %)))

(defcheck solution-15eaf9df
  (partial
    reduce
    #(if (= %2 (last %1))
       %1
       (conj %1 %2))
    '[]))

(defcheck solution-16865bb4
  (fn [s]
    (reduce #(if (= (peek %1) %2) %1 (conj %1 %2)) [] (vec s))))

(defcheck solution-16ba038c
  (fn rem-dup[l]
    (if (<= (count l) 1)
      l
      (if (= (first l) (first (rest l)))
        (rem-dup (rest l))
        (cons (first l) (rem-dup (rest l)))))))

(defcheck solution-1715b4d3
  (fn dedupe [x] (map first (partition-by identity x))))

(defcheck solution-173009dd
  #(letfn [(f [x acc] (if (empty? x) acc (if (= (first x) (last acc)) (f (rest x) acc) (f (rest x) (conj acc (first x))))))] (f % [])))

(defcheck solution-1850695c
  (fn [coll]
    ((fn foo [subcoll accum]
       (if (first subcoll)
         (foo
           (rest subcoll)
           (if (= (first subcoll) (last accum)) accum (conj accum (first subcoll))))
         accum))
     coll
     [])))

(defcheck solution-18a487da
  #(loop [r [] c %] (if (empty? c) r (recur (if (= (last r) (first c)) r (conj r (first c))) (next c)))))

(defcheck solution-18b1024
  (fn compress-a-sequence [input]
    (reduce (fn [result char]
              (let [last-char (last result)]
                (if (= last-char char)
                  result
                  (conj result char))))
      []
      input)))

(defcheck solution-18cd0abc
  (fn [s] (reduce (fn [a e] (if (= e (last a)) a (conj a e))) [] s)))

(defcheck solution-1a439790
  (fn [input]
    (loop [s (seq input) a []]
      (if (empty? s)
        a
        (if (= (first s) (last a))
          (recur (rest s) a)
          (recur (rest s) (conj a (first s))))))))

(defcheck solution-1b023a57
  (fn[a](reduce #(if (= %2 (last %)) % (conj % %2)) [] a)))

(defcheck solution-1b8a81e3
  (comp (partial map first)
        (partial partition-by identity)))

(defcheck solution-1bb72ac2
  (fn [[a & r]]
    (first
      (reduce (fn [[l x] y]
                (if (= x y)
                  [l x]
                  [(conj l y) y]))
        [[a] a] r))))

(defcheck solution-1be9a886
  (fn [coll]
    (map first
      (partition-by #(list %) coll))))

(defcheck solution-1bf75246
  (fn compress
    ([x] (compress x nil))
    ([x c]
     (cond (empty? x) x
           (= c (first x)) (compress  (rest x) (first x))
           :else           (cons (first x) (compress (rest x) (first x)))))))

(defcheck solution-1c2a2501
  reduce #(if (= (take-last 1 %1) (list %2)) %1 (concat %1 (list %2))) (list))

(defcheck solution-1d2e3dcf
  #(reduce
     (fn [c f] (if (= (peek c) f) c (conj c f)))
     [] %))

(defcheck solution-1d35e3a
  (fn compress
    [sequence]
    ((fn compress'
       [sequence previous]
       (cond
         (nil? sequence) ()
         :else (if-not (= previous (first sequence))
                 (cons (first sequence) (compress' (next sequence) (first sequence)))
                 (compress' (next sequence) previous))))
     sequence nil)))

(defcheck solution-1d870e17
  (fn [s]
    (loop [raw (seq s)
           compressed (empty '[])]
      (if-let [h (first raw)]
        (recur (drop-while #(= h %) raw) (conj compressed h))
        (seq compressed)))))

(defcheck solution-1eeeada3
  (fn f [sq]
    (if (empty? sq)
      sq
      (let [fst (first sq)]
        (cons fst (f (drop-while #(= fst %) (rest sq)))))
      )))

(defcheck solution-1f1045cb
  (fn seqc[x]
    (reverse
      (loop [msg x res '()]
        (if (= (count msg) 0)
          res
          (recur
            (rest msg)
            (if (= (first msg) (first res))
              res
              (conj res (first msg))
              )
            ))))))

(defcheck solution-1f950fb4
  (fn [coll]
    (reduce
      (fn [s x] (if (= x (peek s)) s (conj s x)))
      []
      coll)))

(defcheck solution-1fe1939f
  (fn notprevious [x]
    (let [y (seq x)]
      (if (nil? y)
        '()
        (let [z (notprevious (rest y))]
          (if (= (first z) (first y))
            z
            (conj z (first y))))))))

(defcheck solution-1ff95b3b
  (fn [x] ((fn [t a] (if (empty? t) a (if (= (last a) (first t)) (recur (rest t) a) (recur (rest t) (conj a (first t)))))) x [(first x)])))

(defcheck solution-20786309
  #(let [[f & r] (remove (partial apply =) (partition 2 1 %))] (concat f (map last r))))

(defcheck solution-209aa567
  (fn compress [x] (reduce #(if (= (peek %1) %2) %1 (conj %1 %2)) [] x)))

(defcheck solution-209c984a
  (fn [s]
    (filter identity (map #(if (not= %1 %2) %1) s (cons nil s)))))

(defcheck solution-2117e71
  (fn compressSeq [myCol] (let [mySeq (seq myCol), recursor (fn recurs [a b] (if (empty? b) a (if (= (last a) (first b)) (recurs a (rest b)) (recurs (concat a (list (first b))) (rest b)))))] (recursor (list (first mySeq)) (rest mySeq)))))

(defcheck solution-21256aa6
  (fn single [sq]
    (loop [X sq, result []]
      (if (empty? X)
        (seq result)
        (recur (rest X) (if (= (first X) (last result))
                          result (conj result (first X))))))))

(defcheck solution-21547b18
  (fn unique [c]
    (reverse (reduce
               (fn [acc e]
                 (cond
                   (empty? acc) (conj acc e)
                   (= (first acc) e) acc
                   :else (conj acc e))) '() c))))

(defcheck solution-218bcd7b
  (fn my ([s] (my s nil)) ([s c] (let [a (first s) r (next s)] (if s (if (= a c) (my r a) (conj (my r a) a)))))))

(defcheck solution-21ff8260
  (fn [arr]
    (loop [res []
           a arr]
      (if-not (empty? a)
        (if (= (first a) (last res))
          (recur res (rest a))
          (recur (conj res (first a)) (rest a)))
        res))))

(defcheck solution-221b7e13
  reduce #(if (not= (last %1) %2) (conj %1 %2) %1) [])

(defcheck solution-22ebd3b2
  (fn [lst]
    (reverse
      (reduce (fn [res x]
                (if (= x (first res))
                  res
                  (conj res x)))
        '() lst))))

(defcheck solution-235ffd8e
  #(mapcat set (partition-by identity %)))

(defcheck solution-2366ddd7
  (fn joo [s]
    (loop [d '() r s]
      (cond
        (empty? r) (reverse d)
        (= (first d) (first r)) (recur d (rest r))
        :else (recur (cons (first r) d) (rest r))))))

(defcheck solution-238873d
  (fn compress [l]
    (loop [l l
           cl ()]
      (cond
        (= (count l) 1)
        (reverse (conj cl (first l)))
        (= (first l) (second l))
        (recur (next l) cl)
        :else
        (recur (next l) (conj cl (first l)))))))

(defcheck solution-24e9556e
  (fn [xs]
    (map first (partition-by identity xs))))

(defcheck solution-24fae1f3
  (fn compress [s]
    (second (reduce (fn [[c s] x] (if (= c x) [c s] [x (conj s x)]))
              [nil []]
              s))))

(defcheck solution-250b2670
  (fn [sq]
    ((fn dedup
       [[x & xs]]
       (if (empty? xs) [x]
                       (cons x (dedup (drop-while #(= x %) xs))))) sq)))

(defcheck solution-25349411
  #(loop [[cur nxt & _ :as whole] %
          result []]
     (if whole
       (if-not (= cur nxt)
         (recur (next whole) (conj result cur))
         (recur (next whole) result))
       result)))

(defcheck solution-2563df9b
  #(loop [ls [(first %)] h (first %) t (rest %)]
     (if (empty? t)
       ls
       (if (= h (first t))
         (recur ls (first t) (rest t))
         (recur (conj ls (first t)) (first t) (rest t)))
       )))

(defcheck solution-27468595
  (fn dedup [x] (map first (partition-by identity x))))

(defcheck solution-27fc4e73
  (fn p [c]
    (if (empty? c)
      '()
      (let [[f s] c
            [_ & r] c
            n (p r)]
        (if (= f s)
          n
          (cons f n))))))

(defcheck solution-280ec489
  #(reduce (fn [res it]
             (if (not (= (last res) it))
               (conj res it)
               res)) [(first %)] %))

(defcheck solution-285c94c1
  (fn compress [xs] (if (empty? xs) '()
                                    (let [x (first xs) ys (rest xs)]
                                      (if (empty? ys) (conj '() x)
                                                      (let [y (first ys)]
                                                        (if (= x y) (compress ys) (conj (compress ys) x))
                                                        )
                                                      )
                                      )
                                    )
    ))

(defcheck solution-28c8aaae
  (fn [l] (loop [l l result []]
            (if
             (empty? l) result
                        (recur (rest l) (if (and
                                             (not (empty? result))
                                             (= (first l) (last result)))
                                          result
                                          (conj result (first l))))))))

(defcheck solution-2943eaa4
  (fn scompress
    [aseq]
    (let [s (seq aseq)]
      (reduce #(if (not= (last %) %2)
                 (conj % %2)
                 %) [] s))))

(defcheck solution-29846899
  ;(fn compress [xs]
  ;  (if
  ;    (<= (count xs) 1)
  ;    (seq xs)
  ;    (if
  ;      (= (first xs) (second xs))
  ;      (compress (rest xs))
  ;      (cons (first xs) (compress (rest xs)) ) )))

  #(map first (partition-by identity %)))

(defcheck solution-2aa9c541
  (fn [l] (cons (first l) (mapcat (fn [a b] (if (not= a b) (list a) '())) (rest l) (drop-last l)))))

(defcheck solution-2ab32d58
  reduce (fn [a b]
           (if (= (last a) b)
             a
             (conj a b))) [])

(defcheck solution-2acba69d
  (fn f [lst]
    (when-let [lst (seq lst)]
      (if (= (first lst) (second lst))
        (f (rest lst))
        (cons (first lst) (f (rest lst)))))))

(defcheck solution-2ad3a821
  #(reduce (fn [acc e] (if (= e (last acc)) acc (concat acc [e]))) '() %))

(defcheck solution-2ade8373
  (fn ([lst]
       (let [interlist (map vector (drop-last lst) (rest lst))]
         (concat (map (comp first distinct) (filter #(not= (first %) (last %)) interlist)) [(last (last interlist))])))))

(defcheck solution-2af9ddd5
  reduce #(if (= %2 (last %)) % (conj % %2)) [])

(defcheck solution-2b70297b
  (fn f[x]
    (cond
      (empty? x) '()
      (= (first x) (second x)) (f (rest x))
      :else (cons (first x) (f (rest x))))))

(defcheck solution-2bd77385
  (fn [sq]
    (loop [newsq (seq '()) olds sq]
      (if (empty? olds)
        (reverse newsq)
        (if (= (first newsq) (first olds))
          (recur newsq (rest olds))
          (recur (conj newsq (first olds)) (rest olds)))))))

(defcheck solution-2c30c5b9
  (fn [s]
    (reduce
      (fn [x y]
        (if (not= (last x) y)
          (conj x y)
          x)) [] s)))

(defcheck solution-2d0a7eb1
  (fn [s]
    (loop [s s retVal []]
      (if (empty? s)
        retVal
        (recur (rest s) (if (= (first s) (last retVal))
                          retVal
                          (conj retVal (first s))))))))

(defcheck solution-2d4cc731
  (fn rem-dupes
    ([l] (rem-dupes l nil nil))
    ([[h & t] out last]
     (if (nil? h)
       (reverse out)
       (if (= h last)
         (rem-dupes t out last)
         (rem-dupes t (cons h out) h))))))

(defcheck solution-2e6e25ab
  (fn a [s]
    (when-first [c s]
      (cons c (a (drop-while #(= % c) s))))))

(defcheck solution-2eab0cb6
  (fn [s]
    (loop [remainder s
           acc []]
      (if (empty? remainder)
        acc
        (recur (drop-while #(= (first remainder) %) remainder)
          (conj acc (first remainder)))
        ))))

(defcheck solution-2f1953d0
  (fn [x]
    (loop [ls (rest x)
           return (list (first x))]
      (if (nil? (first ls))
        return
        (recur (rest ls)
          (if (= (first ls)
                (last return))
            return
            (concat return [(first ls)])))))))

(defcheck solution-2f9c35f3
  #(reduce (fn [t v] (if (= (last t) v) t (conj t v))) [] %))

(defcheck solution-2f9d58e2
  ;#(loop [o [] s %]
  ;   (if (= s '())
  ;    o
  ;     (if (= (last o) (first s))
  ;       (recur o (rest s))
  ;       (recur (conj o (first s)) (rest s)))))

  #(map first (partition-by identity %)))

(defcheck solution-2fc85b1f
  (fn [coll] (reduce (fn [l x] (if (= x (last l)) l (conj l x))) [] coll)))

(defcheck solution-2ffbff92
  (fn f [res x] (if (empty? x)
                  res
                  (f (into res (vector (first x)) )(drop-while #(= (first x) %) x))
                  )) [])

(defcheck solution-3014affe
  (fn [arr]
    (reverse
      (reduce #(conj % (first %2)) '()
        (partition-by identity arr)))))

(defcheck solution-30599b12
  #(reverse (reduce (fn [xs v]
                      (cond
                        (empty? xs) (list v)
                        (not= (first xs) v) (conj xs v)
                        :else xs))
              () %)))

(defcheck solution-308b97c7
  (fn [coll] (reduce
               (fn [coll item]
                 (if (= (last coll) item)
                   coll
                   (concat coll [item]))) [] coll)))

(defcheck solution-30d1cd7b
  (fn remove_dup [xs]
    (loop [f (first xs) r (rest xs) acc [f]]
      (if (seq r)
        (if (= f (first r)) (recur f (rest r) acc)
                            (recur (first r) (rest r) (conj acc (first r))))
        acc))))

(defcheck solution-3164ccc3
  (let [mine "reduce #(if (= %2 (last %1)) %1 (conj %1 %2)) []"]
    (fn [x] (map first (partition-by identity x)))))

(defcheck solution-31b1b8a3
  (fn nodup [s]
    (let [a (first s) b (second s)]
      (if (nil? b) [a]
                   (if (= a b)
                     (nodup (rest s))
                     (cons a (nodup (rest s))))))))

(defcheck solution-31c5fbc4
  (fn f [stuff] (if-not (first stuff) (list)
                                      (if (= (first stuff) (second stuff))
                                        (f (rest stuff))
                                        (conj (f (rest stuff)) (first stuff))))))

(defcheck solution-31d57c5d
  (fn uniq [ls]
    (if (<= (count ls) 1)
      ls
      (if (= (first ls) (first (rest ls)))
        (uniq (rest ls))
        (cons (first ls) (uniq (rest ls)))
        )
      )
    ))

(defcheck solution-31fa88ee
  reduce (fn [r x] (if (not= (last r) x) (conj r x) r)) [])

(defcheck solution-32509b22
  #(reduce (fn [a b]
             (if (= (last a) b)
               a
               (conj a b)))
     []
     (seq %)))

(defcheck solution-3256602
  (fn [xs]
    (if-let [ys (seq xs)]
      (conj (->> ys
              (partition 2 1)
              (filter #(not= (first %) (second %)))
              (map #(second %)))
        (first ys)))))

(defcheck solution-3263b257
  (fn [s]
    (let [a (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] s)]
      (if (char? (first a)) (apply str a) a))))

(defcheck solution-329b3b86
  (fn com [x]
    (when-let [[f & r] (seq x)]
      (if (= f (first r))
        (com r)
        (cons f (com r))

        )
      )
    ))

(defcheck solution-32d430f9
  #(map first (partition-by identity %1)))

(defcheck solution-32da468
  (fn collect
    ([coll] (collect (vector (first coll)) (rest coll)))
    ([coll left]
     (if (empty? left)
       coll
       (if (= (first left) (last coll))
         (collect coll (rest left))
         (collect (conj coll (first left)) (rest left)))))))

(defcheck solution-32db3ce7
  (fn [coll]
    (map first (partition-by (fn [x] x) coll))))

(defcheck solution-331bad4
  (fn cp [xs] (if (next xs)
                (let [[x & r] xs]
                  (if (= x (first r))
                    (cp r)
                    (cons x (cp r))))
                xs)))

(defcheck solution-3390d99
  (fn [seq]
    (map first (partition-by identity seq))))

(defcheck solution-33940889
  (fn compress [coll]
    (reverse ((fn do-compress [new old]
                (if (empty? old)
                  new
                  (if (= (first new) (first old))
                    (do-compress new (rest old))
                    (do-compress (conj new (first old)) (rest old)))))
              (list (first coll)) (rest coll)))))

(defcheck solution-339dfe9b
  #(reverse
     (reduce (fn [a b]
               (if (= [] a)
                 (list b)
                 (if (= b (first a))
                   a
                   (cons b a)))) [] %)))

(defcheck solution-3439461
  (fn [coll]
    (reduce
      #(if (= (last %) %2)
         (identity %)
         (conj % %2))
      []
      coll)))

(defcheck solution-344b7102
  #(reduce (fn [s i] (if (= (last s) i) s (conj s i))) [] %))

(defcheck solution-3475264f
  (fn [sq]
    (loop [res []
           head (first sq)
           tail (rest sq)]
      (if (nil? head)
        res
        (if-not (= head (first tail))
          (recur (conj res head)
            (first tail)
            (rest tail))
          (recur res
            (first tail)
            (rest tail)))))))

(defcheck solution-3492ac24
  (fn [coll]
    (map #(first %) (partition-by identity coll))))

(defcheck solution-349c65ff
  reduce (fn [xs y] (if (= y (last xs)) xs (conj xs y) )) [])

(defcheck solution-35122a9c
  (fn [s]
    (loop [x s r []]
      (if (empty? x)
        r
        (if
         (= (first x) (second x)) (recur (rest x) r)
                                  (recur (rest x) (conj r (first x))))))))

(defcheck solution-3525c8bd
  (fn [in]
    (loop [prev nil v in res []]
      (if v
        (let [ff (first v)]
          (if (= prev ff)
            (recur ff (next v) res)
            (recur ff (next v) (conj res ff))
            )
          )
        res
        )
      )
    ))

(defcheck solution-354dfbca
  #((fn [ret x]
      (if (next x)
        (if (= (last ret) (first x))
          (recur ret (next x))
          (recur (conj ret (first x)) (next x)))
        (if (= (last ret) (first x))
          ret
          (conj ret (first x)))))
    [] (seq %)))

(defcheck solution-35e304a6
  (fn compress
    [collection]
    "Removes any immediate duplicates in a collection.
    Does this by partitioning a sequence by the identity of the elements,
    then returns a new sequence containing the first elements in each subcollection,
    thus removing any duplicates or \"compressing\" the given collection"
    (map first (partition-by identity collection))))

(defcheck solution-35f4cd22
  #(mapcat (fn [a b] (when (not= a b) [a])) % (cons nil % )))

(defcheck solution-3622bee2
  (fn [l]
    (loop [cl   []
           more l
           pe   nil]
      (if (nil? more)
        cl
        (if (nil? pe)
          (recur (conj cl (first more)) (next more) (first more))
          (if (= pe (first more))
            (recur cl (next more) pe)
            (recur (conj cl (first more)) (next more) (first more))))))))

(defcheck solution-36589b87
  #(let [f (fn [acc, x]
             (if (= x (first acc))
               acc
               (conj acc x)))]
     (reverse (reduce f () %))))

(defcheck solution-36bce59e
  (fn ddp [s]
    (if-not (string? s)
      (map first (partition-by identity s))
      (apply str (ddp (seq s))))))

(defcheck solution-3732b7ad
  (fn [s]
    (loop [v [] last nil todo s]
      (if (empty? todo) v
                        (let [x (first todo) xs (rest todo)]
                          (recur (if (= x last) v (conj v x)) x xs))))))

(defcheck solution-3761d269
  (fn [s] (if (< (count s) 2)
            s
            ((fn f [c s]
               (if (empty? s)
                 (cons c s)
                 (let [[h & t] s]
                   (if (= c h)
                     (f c t)
                     (cons c (f h t)))))) (first s) (rest s)))))

(defcheck solution-379503e
  (fn [coll] (reduce #(if (not= (first %1) %2) (conj %1 %2) %1) () (reverse coll))))

(defcheck solution-379c26eb
  reduce (fn [res c] (if (= c (last res)) res (conj res c))) [])

(defcheck solution-37ae0070
  #(map first
     (partition-by identity (seq %))))

(defcheck solution-38e6b0bb
  #(reduce (fn [ret v] (if (= v (last ret)) ret (conj ret v))) [] %))

(defcheck solution-38f2e3ab
  (fn f [[x & xs]]
    (if (empty? xs) (list x)
                    (if (= x (first xs)) (f xs) (cons x (f xs))))))

(defcheck solution-39097f51
  #(reverse (reduce (fn [a b] (if (= (first a) b) a (conj a b))) nil %)))

(defcheck solution-3a1857b6
  #(mapcat set (partition-by identity %1)))

(defcheck solution-3a5210cc
  (fn [coll]
    (reduce (fn [res x]
              (if (= (last res) x)
                res
                (conj res x)))
      []
      coll)))

(defcheck solution-3a9949e0
  #(->> % (partition-by identity) (map first)))

(defcheck solution-3b48125b
  (fn [s]
    (loop [s s acc []]
      (if (empty? s)
        acc
        (let [[x & xs] s]
          (if (= x (first xs))
            (recur xs acc)
            (recur xs (conj acc x))))))))

(defcheck solution-3b71dc5d
  #(map first(partition-by identity %)))

(defcheck solution-3b88a165
  reduce (fn [coll elem]
           (if (= (last coll) elem)
             coll
             (conj coll elem))) [])

(defcheck solution-3c72f7cc
  (fn [x] (remove nil? (map #(if (= %1 %2) nil %1) x (cons nil x)))))

(defcheck solution-3c8eca49
  #(reduce
     (fn [acc val]
       (if (not= val (last acc))
         (conj acc val)
         acc))
     []
     %))

(defcheck solution-3d025f6a
  #(reduce (fn [c x]
             (if (= x (last c))
               c
               (concat c (list x)))) '() %))

(defcheck solution-3d06ac89
  (fn dedup [xs]
    (if (empty? xs)
      ()
      (if (= (first xs) (first (rest xs)))
        (dedup (rest xs))
        (cons (first xs) (dedup (rest xs)))
        )
      )
    ))

(defcheck solution-3d41855a
  (fn compress-sequence [x]
    (if (empty? x)
      x
      (if (= (first x)
            (first (rest x)))
        (compress-sequence (concat (list (first x))
                                   (rest (rest x))))
        (concat (list (first x))
                (compress-sequence (rest x)))))))

(defcheck solution-3d464261
  (fn myzip [s]
    (cond (string? s) (myzip (seq s))
          (<= (count s) 1) s
          (= (first s) (second s)) (do #_(println (rest s)) (myzip (rest s)))
          :else (conj (myzip (rest s)) (first s)))))

(defcheck solution-3de3de1c
  reduce #(if (= (last %1) %2) %1 (conj %1 %2)) '[])

(defcheck solution-3ded9512
  (fn [s]
    (reduce #(if (= (last %1) %2) %1
                                  (conj %1 %2))
      []
      s)))

(defcheck solution-3dfd4c93
  (fn compress-seq [lst]
    (reverse
      (reduce (fn [accum val]
                (if (= val (first accum)) accum (cons val accum)))
        (list (first lst)) (rest lst)))))

(defcheck solution-3ebac5d8
  (fn f
    [ls]
    (if-let [h (first ls)]
      (cons h (f (drop-while #(= h %) (rest ls))))
      nil)))

(defcheck solution-3ed869c8
  (fn [x]
    (lazy-seq
      (:result
       (reduce (fn [r y]
                 (if (= (:prev r ) y)
                   {:result (:result r ) :prev y}
                   {:result (conj (:result r ) y) :prev y}
                   ))
         {:result (vec []) :prev nil} x )))))

(defcheck solution-3f27a3bb
  (fn clps [s] (lazy-seq
                 (if (seq s)
                   (if (seq (rest s))
                     (cons (first s) (clps (drop-while #(= % (first s)) (rest s))))
                     [(first s)])
                   []))))

(defcheck solution-3f7a98f0
  #(filter identity (map-indexed (fn [i x]
                                   (if (= i 0) x
                                               (if (= (nth % (dec i)) x)
                                                 nil
                                                 x))) %)))

(defcheck solution-3f9998e5
  reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-40a4ab46
  (fn [col]
    (loop [col1 [] col2 col ]
      (if (empty? col2)
        col1
        (let [same (= (first col2) (last col1)  )]
          (recur (if same col1 (conj col1 (first col2) )  )
            (rest col2)

            )
          )
        )
      )
    ))

(defcheck solution-40bd8517
  #(loop [uq-seq [(first %1)] i 0]
     (if (= i (count %1))
       uq-seq
       (recur (if (= (nth %1 i) (last uq-seq)) uq-seq (conj uq-seq (nth %1 i))) (inc i))
       )
     ))

(defcheck solution-41bbd43a
  (fn [coll] (mapcat #(if (= %2 %) () (list %2)) (cons nil coll) coll)))

(defcheck solution-4246c948
  (fn [c]
    (map first (partition-by identity c))))

(defcheck solution-44107c67
  reduce #(if (= (last %1) %2) %1 (concat %1 (list %2))) '())

(defcheck solution-442fe126
  (fn dedup [x]
    (when-let [first-item (first x)]
      (lazy-seq
        (cons first-item (dedup (drop-while #(= % first-item) (rest x))))))))

(defcheck solution-456bab26
  (fn un [p]
    (if (> 2 (count p))
      p
      (let [f (first p)
            s (second p)
            n (next p)]
        (if (= f s)
          (cons f (next (un n)))
          (cons f (un n)))))))

(defcheck solution-45c7bdf3
  (fn compress [s]
    (reduce (fn [acc e]
              (if (= (last acc) e)
                acc
                (conj acc e)))
      []
      s)))

(defcheck solution-45d8b3ee
  (fn comp [[x & xs]] (if (empty? xs) [x] (if (= x (first xs)) (comp xs) (cons x (comp xs))))))

(defcheck solution-45fe637e
  (fn [l]
    (loop [l l rv []]
      (if (empty? l)
        rv
        (if (= (first l) (last rv))
          (recur (rest l) rv)
          (recur (rest l) (conj rv (first l))))))))

(defcheck solution-462c2a6f
  (fn compress [a] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] a)))

(defcheck solution-46693113
  (fn remove-duplicates
    [input]
    (let [partitions (partition-by identity input)]
      (into [] (map first partitions)))))

(defcheck solution-46ba3851
  (fn [xs]
    (loop [[head & tail :as xs] xs, current nil, acc []]
      (cond
        (empty? xs) acc
        (= head current) (recur tail current acc)
        :else (recur tail head (conj acc head))))))

(defcheck solution-475c8b4c
  #(reduce (fn [xs x]
             (if (= x (peek xs))
               xs
               (conj xs x)))
     []
     %))

(defcheck solution-485c1150
  (fn [x]
    (let [s (reduce (fn [p c] (if (= (last p) c) p (conj p c))) [] x)]
      (if (string? x) (apply str s) s))))

(defcheck solution-488f125f
  #(
    (fn noconsec [in out]
      (if (empty? in)
        out
        (if (= (first in) (first (rest in)))
          (noconsec (rest in) out)
          (noconsec (rest in) (conj out (first in))))
        )
      )
    % []))

(defcheck solution-48ee745c
  (fn [l]
    (concat
     (list (first (seq l)))
     (map first
       (filter #(not= (first %1) (last %1))
         (map list (drop 1 (seq l)) (butlast (seq l)))
         )
       )
     )
    ))

(defcheck solution-4936e087
  #(reduce (fn [a b] (if (= b (last a)) a (conj a b))) [] %))

(defcheck solution-497caac6
  (fn compress [coll]
    (cond
      (empty? coll)
      coll

      (= (first coll) (second coll))
      (compress (rest coll))

      :else
      (cons (first coll) (compress (rest coll))))))

(defcheck solution-49bdc75
  (fn [coll] (loop [ result [] c coll prev nil]
               (if (nil? c) result
                            (recur (concat result (let [ [a b] c] (when-not (= a b) [a]))) (next c) (first c))))))

(defcheck solution-49c162ee
  (fn rcd [s & lastelt]
    (cond
      (empty? s) s
      (empty? lastelt) (let [fe (first s)]
                         (concat [fe] (rcd (rest s) fe)))
      :else
      (let [[le] lastelt
            fe (first s)]
        (if (= le fe) (rcd (rest s) fe)
                      (concat [fe] (rcd (rest s) fe)))))))

(defcheck solution-49d1bae2
  (fn [coll]
    (letfn [(comp-seq [coll]
              (if
               coll
                (let
                 [f (first coll)
                  r (next coll)]
                  (if
                   (not (= f (first r)))
                    (cons f (comp-seq r))
                    (comp-seq r)))))]
      (comp-seq coll))))

(defcheck solution-4a92facb
  #(reduce (fn [s c] (if (= (last s) c) s (conj s c))) [] %))

(defcheck solution-4abd1d9d
  (fn [seqs]
    (loop [result (list (first seqs))
           now (first seqs)
           others (rest seqs )]
      (if (empty? others)
        result
        (if(= now (first others))
          (recur result now (rest others))
          (recur ( concat result (list (first others)) )
            (first others )
            (rest others)))))))

(defcheck solution-4ae96cad
  (fn removeDupes [coll]
    ((fn inner [curr prev remaining]
       (if (nil? curr)
         nil
         (if (= curr prev)
           (inner (first remaining) curr (rest remaining))
           (cons curr (inner (first remaining) curr (rest remaining))))))
     (first coll) nil (rest coll))))

(defcheck solution-4b3a4474
  (letfn [(unique-conj [xs x]
            (if (= (first xs) x)
              xs
              (conj xs x)))]
    #(->> %
       (reduce unique-conj ())
       reverse)))

(defcheck solution-4c307a7c
  (letfn [(remove-seq-dupes [input]
            (cond
              (empty? input) input
              (= (first input)
                (second input)) (recur (rest input))
              :else (cons (first input)
                      (remove-seq-dupes (rest input)))))]
    #(remove-seq-dupes %)))

(defcheck solution-4cacd514
  (fn [x]
    (concat [(first x)]
            (map
              second
              (filter
                #(not= (first %) (second %))
                (partition 2 1 x))))))

(defcheck solution-4cd6cccb
  (fn [coll]
    (let [f (fn [orig result prev]
              (cond (empty? orig) result
                    (= (first orig) prev)
                    (recur (rest orig) result (first orig))
                    :else
                    (recur (rest orig) (conj result (first orig)) (first orig))))]
      (f coll [] ()))))

(defcheck solution-4cf037e8
  (fn press
    ([x] (let [next (first x)] (cons next (press (rest x) next))))
    ([x prev]
     (if (empty? x) ()
                    (let [next (first x)]
                      (if (= next prev)
                        (press (rest x) next)
                        (cons next (press (rest x) next))))))))

(defcheck solution-4d16a688
  #(map first (remove (fn [[a b]] (= a b)) (partition 2 1 nil %))))

(defcheck solution-4d28af3a
  (fn my-compress [x]
    (loop [coll x comp [] lst nil]
      (if (empty? coll)
        comp
        (if (= (first coll) lst)
          (recur (rest coll) comp lst)
          (recur (rest coll) (conj comp (first coll)) (first coll)))))))

(defcheck solution-4d318bbc
  (fn [xs] (reverse (first (reduce
                             (fn [[acc lastItm] cur] (if (= cur lastItm) [acc cur] [(cons cur acc) cur])) [[] nil] (seq xs))))))

(defcheck solution-4da70c5c
  #(reverse (reduce (fn [s x]
                      (if (= x (first s)) s (cons x s))) () %)))

(defcheck solution-4e3a6ce
  (fn [xs] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] xs)))

(defcheck solution-4f309402
  #(first (reduce (fn [[acc lst] i] (if (= i lst) [acc lst] [(conj acc i) i])) [[] 0] %1)))

(defcheck solution-509b309f
  (fn[s](reduce (fn [result _first]
                  (if (not= _first (last result))
                    (conj result _first)
                    result)) [] s)))

(defcheck solution-51199d9a
  reduce (fn [x, y] (if (= (last x) y)
                      x
                      (conj x y))) [])

(defcheck solution-51b7d274
  #(loop [r [], coll %, prev nil]
     (if (empty? coll)
       r
       (let [h (first coll), r1 (if (or (empty? r) (not= h prev)) (conj r h) r)]
         (recur r1 (rest coll) h)))))

(defcheck solution-52765660
  (fn [stream]
    (reverse
      (reduce
        #(if
          (or (empty? %1) (not= (first %1) %2))
           (conj %1 %2)
           %1)
        '()
        stream))))

(defcheck solution-537c22ee
  (fn [s]
    (reverse (remove nil?
               (map #(if (not= % %2) %) (into () s) (conj (into () s) "dummy"))))))

(defcheck solution-54a7b65a
  #(apply concat (map distinct (partition-by identity %))))

(defcheck solution-54b3983f
  (fn [x] (reverse (reduce #(if (= (first %1) %2) %1 (conj %1 %2)) '() x))))

(defcheck solution-54b961b4
  (fn [x]
    (let [s (seq x)
          f (fn f [c, p]
              (if (seq c)
                (if (= (first c) p)
                  (f (rest c) p)
                  (cons (first c) (f (rest c) (first c))))
                '()))]
      (if s
        (cons (first s) (f (rest s) (first s)))))))

(defcheck solution-54baae5d
  (fn [xs] (reduce #(if (not (= (peek %1) %2)) (conj %1 %2) %1) [] xs)))

(defcheck solution-54e78a45
  (fn[-seq]
    (concat (reduce (fn [cont it]
                      (if (= it (last cont))
                        cont
                        (conj cont it)
                        )

                      ) [] (seq -seq)))
    ))

(defcheck solution-5664371b
  (fn compress [item]
    (remove nil? (map #(when (not (= (nth % 0) (nth % 1))) (nth % 0) ) (partition 2 1 (concat item "_"))
                   ))))

(defcheck solution-56df25a1
  (fn f([[a & [b] :as ls]] (cond (nil? b) (cons a '())
                                 (= a b) (f (rest ls))
                                 :else (cons a (f (rest ls)))))))

(defcheck solution-587d1b4
  (fn [seq]
    ((fn uniq [left right]
       (cond
         (= right [])
         left
         (= (last left) (first right))
         (recur left (rest right))
         true
         (recur (concat left [(first right)])
           (rest right))))
     [] seq)))

(defcheck solution-596426
  #(
    (fn [x y]
      (if (empty? x)
        y
        (if (= (first x) (last y))
          (recur (rest x) y)
          (recur (rest x)(conj y (first x)))
          )
        )
      )
    (rest %) [(first %)]
    ))

(defcheck solution-59de45f
  (fn [coll]
    (loop [result []
           coll coll]
      (if (empty? coll)
        result
        (if (= (last result) (first coll))
          (recur result (rest coll))
          (recur (conj result (first coll)) (rest coll)))))))

(defcheck solution-5a67cd98
  (fn [items]
    (drop 1
      (reduce (fn [r i] (if (not= i (last r)) (conj r i) r)) [nil] items))))

(defcheck solution-5a70230c
  #(loop [result [] [fst & rst :as coll] %]
     (if (empty? coll)
       result
       (recur (if (and (not (empty? rst))
                       (= fst (first rst)))
                result
                (conj result fst))
         rst))))

(defcheck solution-5a71ac07
  #(reverse (reduce (fn [a b] (if (= (first a) b) a (conj a b))) (take 1 %) (rest %))))

(defcheck solution-5c3240e
  #(reduce (fn [acc el]
             (if (= (last acc) el) acc (conj acc el))) [] (seq %)))

(defcheck solution-5cf5827d
  (fn d
    ([x]
     (d (rest x) [(first x)]))
    ([x accum]
     (if (empty? x)
       accum
       (if (= (first x) (last accum))
         (d (rest x) accum)
         (d (rest x) (conj accum (first x))))))))

(defcheck solution-5d4b6be7
  (fn f [coll]
    (if-let [r (seq (rest coll))]
      (if (= (first coll) (first r))
        (f r)
        (cons (first coll) (f r)))
      coll
      )))

(defcheck solution-5d582e0
  #(reduce (fn [acc x]  (if (= (last acc) x ) acc (concat acc [x]) )  ) [] (seq %) ))

(defcheck solution-5de5f973
  ; unsatisfying
  (fn [s] (reverse (reduce #(if (= (first %1) %2) %1 (cons %2 %1)) '() s))))

(defcheck solution-5e0ab822
  (fn [xs]
    (if (seq xs)
      (loop [ys (rest xs)
             zs [(first xs)]]
        (if (seq ys)
          (if (= (first ys) (last zs))
            (recur (rest ys) zs)
            (recur (rest ys) (conj zs (first ys))))
          zs))
      nil)))

(defcheck solution-5e42639f
  (fn [in]
    (lazy-seq (first (reduce (fn [[res last] num]
                               (if (= last num)
                                 [res last]
                                 [(conj res num) num]))
                       [[] nil]
                       in)))))

(defcheck solution-5ea2cce4
  (fn[s](
          if (string? s) (distinct s)
                         (keep identity (conj
                                          (map-indexed
                                            (fn[idx itm](if(not= itm (nth s idx)) itm ))
                                            (rest s)
                                            )
                                          (first s)
                                          )
                           )

                         )))

(defcheck solution-5ec41f51
  (fn [s]
    (reduce #(if (= (last %) %2)
               %
               (concat % (list %2)))
      '()
      s)))

(defcheck solution-5edfe6a8
  (fn hoge [lst]
    (if (empty? lst) nil
                     (let [h (first lst) f (partial = h)]
                       (cons (first (take-while f lst)) (hoge (drop-while f lst)))))))

(defcheck solution-5ef992bd
  (fn [in-seq]
    ((fn process [in-vec res-vec]
       (if (empty? in-vec)
         res-vec
         (if (= (first in-vec) (last res-vec))
           (recur (rest in-vec) res-vec)
           (recur
             (rest in-vec)
             (conj res-vec (first in-vec))))
         )
       )
     (if (vector? in-seq) in-seq (vec in-seq)) [])
    ))

(defcheck solution-5f0285a8
  reduce #(if (= (last %) %2)
            %
            (conj % %2)) [])

(defcheck solution-5fab3dc
  (fn dedup
    ([coll] (dedup '() coll))
    ([aggr coll]
     (if (empty? coll)
       aggr
       (if (= (first coll) (last aggr))
         (dedup aggr (rest coll))
         (dedup (concat aggr (list (first coll))) (rest coll)) )))))

(defcheck solution-6015b7a1
  (fn compress-wrapper [t]
    ((fn compress [s e]
       (cond (and e (first s))
             (if (= (first s) e) (compress (rest s) (first s))
                                 (cons (first s) (compress (rest s) (first s))))
             (first s) (cons (first s) (compress (rest s) (first s)))
             :else ())) t nil)))

(defcheck solution-61374fce
  #(reduce (fn [coll item] (if (= (first coll) item)
                             coll
                             (cons item coll))) '() (reverse %)))

(defcheck solution-61abed93
  #(reduce (fn [coll e] (if (= (last coll) e) coll (conj coll e))) [] %))

(defcheck solution-62797ca0
  (comp (partial map first) (partial partition-by identity)))

(defcheck solution-63370f4f
  (fn cmprss [s]
    (if (not (second s))
      (list (first s))
      (if (= (first s) (second s))
        (cmprss (rest s))
        (cons (first s) (cmprss (rest s)))))))

(defcheck solution-63a48980
  reduce (fn [a b] (if (= (last a) b) a (conj a b))) [])

(defcheck solution-63f2ce48
  reduce #(if (= (last %) %2)
            %
            (conj % %2)) [])

(defcheck solution-63f5eb2e
  reduce (fn [acc i] (if (and (not (empty? acc)) (= (nth acc (dec (count acc))) i)) acc(concat acc [i]))) [])

(defcheck solution-64c5768b
  (fn f [l]
    (if (empty? l)
      l
      (if (= (first l) (second l))
        (f (rest l))
        (cons (first l) (f (rest l)))
        )
      )
    ))

(defcheck solution-6587d866
  (fn [coll]
    (reduce (fn [a b]
              (if (= (last a) b) a (conj a b))) [] coll)))

(defcheck solution-6597c57a
  #(map last (partition-by identity %)))

(defcheck solution-65ae20a1
  (fn [a]
    (loop [result [] coll a]
      (if (empty? coll)
        result
        (recur
          (if (= (last result) (first coll))
            result
            (conj result (first coll)))
          (rest coll))))))

(defcheck solution-65d0eda9
  #(apply list (map first (partition-by identity %))))

(defcheck solution-66920437
  #(keep-indexed (fn[i el](if-not (= el
                                    (get % (inc i))
                                    )
                            el)
                   ) %
     ))

(defcheck solution-675a5a97
  (partial reduce #(if (= (last %1) %2) %1 (conj %1 %2)) []))

(defcheck solution-682e6ae1
  (fn dedupe [col]
    (reduce
      #(if (= (last %1) %2)
         %1
         (concat %1 [%2]))
      (list (first col))
      col)))

(defcheck solution-6838da00
  #(reduce (fn [a b] (if (or (empty? a) (not= (last a) b)) (conj a b) a)) [] %))

(defcheck solution-68f8595
  (fn dup
    [arr]
    (loop [[x1 x2 & t] arr final []]
      (if (nil? x2)
        (conj final x1)
        (if (= x1 x2)
          (recur (conj t x1) final)
          (recur (conj t x2) (conj final x1))
          )
        ))))

(defcheck solution-6951cbc0
  (fn filtr [coll]
    (->> coll
      (reduce
        (fn [res elem]
          (if (= (second res) elem)
            res
            (list (cons elem (first res)) elem)))
        '((), nil))
      (first)
      (reverse))))

(defcheck solution-69bb5324
  (fn [coll]
    (reduce (fn [compressed val]
              (if (= (last compressed) val)
                compressed
                (conj compressed val)))
      [(first coll)]
      (rest coll))))

(defcheck solution-69c948ee
  (fn f [x] (if (or (empty? x) (empty? (rest x)))
              x
              (if (= (first x) (second x))
                (f (rest x))
                (cons (first x) (lazy-seq (f (rest x))))))))

(defcheck solution-69fde2ba
  (fn [s]
    (loop [c (first s) s (rest s) r [c]]
      (if (not (seq s))
        r
        (if (= c (first s))
          (recur c (rest s) r)
          (recur (first s) (rest s) (conj r (first s))))))))

(defcheck solution-6a36d1a4
  (fn [x]
    (map first (partition-by identity x))))

(defcheck solution-6a3878e3
  (fn  chop [s]
    (if (empty? s)
      []
      (if (= (first s) (second s))
        (chop (rest s))
        (cons (first s) (chop (rest s)))
        ))
    ))

(defcheck solution-6aa3e4c4
  (comp reverse
        (partial reduce
          (fn [s x]
            (if (= (first s) x)
              s
              (cons x s)))
          '())))

(defcheck solution-6abacd31
  (fn [l] (reverse (loop [acc () s1 l]
                     (if (empty? s1)
                       acc
                       (recur
                         (if (or (empty? acc) (not= (first acc) (first s1)))
                           (cons (first s1) acc) acc)
                         (rest s1))
                       )))))

(defcheck solution-6ad445bf
  (fn [s]
    (map first (partition-by identity s))))

(defcheck solution-6b15dbee
  (fn [l]
    (loop [n 1, r (list (list (first l)))]
      (if (= (count l) n)
        (map first r)
        (recur (+ n 1) (if (contains? (set (last r)) (nth l n))
                         r
                         (concat (take n r) (list (list (nth l n))))))))))

(defcheck solution-6b180b5f
  (fn [coll]
    (map first (partition-by identity coll))))

(defcheck solution-6b5fe54a
  (fn [x] (loop [a [(first x)] i 1]
            (if (>= i (count x))
              a
              (recur (if (= (nth x i) (nth x (dec i))) a (conj a (nth x i))) (inc i))))))

(defcheck solution-6bf75a97
  (fn [coll] (map first (partition-by identity coll))))

(defcheck solution-6bfb5b31
  reduce #(if (= (peek %) %2) % (conj % %2)) [])

(defcheck solution-6c5148be
  (fn [items]
    (map first (partition-by identity items))))

(defcheck solution-6ce19c10
  (fn compress [xs]
    (when-let [[f & r] (seq xs)]
      (if (= f (first r))
        (compress r)
        (cons f (compress r))))))

(defcheck solution-6d5b01c8
  (fn [data] (reverse (loop [r '()
                             c data]
                        (if (empty? c)
                          r
                          (recur (conj r (first c))
                            (drop-while #(= (first c) %) c)))))))

(defcheck solution-6d5b8027
  (fn [x]
    (loop [i (count x), x x, result [(first x)]]
      (if (> i 1)
        (if (= (first x) (second x))
          (recur (dec i) (next x) result)
          (recur (dec i) (next x) (conj result (second x)))
          )
        result
        )
      )
    ))

(defcheck solution-6dd106f2
  (fn compress [xs] (when (seq xs)
                      (cons (first xs) (compress (drop-while #(= (first xs) %) xs))))))

(defcheck solution-6dec70be
  (fn [col]
    (map first
      (remove #(and (> (count %) 1 ) (= (first %) (last %)))
        (partition-all 2 1 col)))))

(defcheck solution-6e0685b5
  (fn uniq [s]
    (if (empty? s)
      s
      (if (= (first s) (second s))
        (uniq (rest s))
        (cons (first s) (uniq (rest s)))))))

(defcheck solution-6e0a0150
  (fn [coll]
    (map first (filter (fn [[a b]] (not= a b)) (map vector coll (cons nil coll))))))

(defcheck solution-6ef3766b
  (fn f [[a & z]]
    (lazy-seq
      (when a
        (cons a (f (drop-while #(= a %) z)))))))

(defcheck solution-6fce127d
  reduce #(if (= (last %1 ) %2) %1 (conj %1 %2)) [])

(defcheck solution-70458be3
  (letfn [(build [x y] (cond (or (empty? x) (empty? (rest x))) (concat x y) (= (first x) (second x)) (recur (rest x) y) true (recur (rest x) (cons (first x) y))))] #(reverse (build % []))))

(defcheck solution-70afaed2
  (fn compress [coll]
    (when-let [[f & r] (seq coll)]
      (if (= f (first r))
        (compress r)
        (cons f (compress r))))))

(defcheck solution-70bc5857
  #(letfn [(remove-dups [input result]
             (if (empty? input)
               result
               (if (= (last result) (first input))
                 (remove-dups (rest input) result)
                 (remove-dups (rest input) (conj result (first input))))))]
     (remove-dups (seq %) [])))

(defcheck solution-70ebc73d
  #((fn compr [s prev]
      (if (empty? s)
        (cons prev nil)
        (if (= prev (first s))
          (compr (rest s) prev)
          (cons prev (compr (rest s) (first s)))))) % (first %)))

(defcheck solution-71247c6b
  #(->> %  (partition-by identity)  (map set) (apply concat)))

(defcheck solution-7161cbc9
  (fn compress [s]
    (loop [s s prev `last# acc []]
      (if (empty? s) acc
                     (if (= (first s) prev)
                       (recur (rest s) (first s) acc)
                       (recur (rest s) (first s) (conj acc (first s))))))))

(defcheck solution-7167874f
  (fn [S]
    (reduce #(if (=(last %1) %2)
               %1  (conj %1 %2) )
      [] S)
    ))

(defcheck solution-7174015
  (fn [c] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] c)))

(defcheck solution-717db269
  (fn
    [a-seq]
    (loop [l-seq a-seq el (first l-seq) acc []]
      (if (empty? l-seq)
        acc
        (let [stripped (drop-while #(= % el) l-seq)]
          (recur stripped
            (first stripped)
            (conj acc el)))))))

(defcheck solution-71929830
  (fn [ys]
    (letfn
     [(go [xs]
        (if (empty? xs)
          (list)
          (let [x (first xs)
                xs' (rest xs)]
            (cons x (go (drop-while #(= x %) xs'))))))]
      (go ys))))

(defcheck solution-727751ef
  (fn [x]
    (loop [init '(), elm x]
      (if (= (last elm) nil)
        init
        (if (= (last init) (first elm))
          (recur init (rest elm))
          (recur (reverse (cons (first elm) (reverse init))) (rest elm)))))))

(defcheck solution-72dd6c10
  (fn [i]
    (reverse ((fn self [orig toReturn]
                (cond
                  (empty? orig) toReturn
                  (= (first orig) (first toReturn)) (self (rest orig) toReturn)
                  :else (self (rest orig) (conj toReturn (first orig))))) i nil))))

(defcheck solution-72ec746a
  reduce (fn [sx y]
           (if (= (last sx) y)
             sx
             (conj sx y)
             )
           ) [])

(defcheck solution-73fbd09a
  (fn [s]
    (loop [s (seq s) sq []]
      (if (empty? s)
        sq
        (recur (drop-while #(= % (first s)) s)
          (conj sq (first s)))))))

(defcheck solution-742959c
  (fn [col] (map first (partition-by identity col))))

(defcheck solution-7468d8cb
  (fn [coll] (reduce #(if (= (last %1) %2) %1 (concat %1 (list %2))) "" coll)))

(defcheck solution-7489c49b
  (fn compress [sq]
    (reverse
      (reduce
        (fn [res, x]
          (if (= (first res) x)
            res
            (cons x res)
            )
          )
        (list (first sq))
        (rest sq)))))

(defcheck solution-749c0522
  (fn [coll]
    (loop [c coll z []]
      (if (empty? c)
        z
        (let [f (first c) r (rest c)]
          (recur r
            (if (empty? z) [f]
                           (if (= f (last z))
                             z
                             (conj z f)))))))))

(defcheck solution-74bae009
  #(map first (partition-by identity (seq %))))

(defcheck solution-74c4ccd4
  (fn [y] (let [fx (fn [x a] (if (not (= (last x) a)) (conj x a) x))]
            (reduce fx [] y))))

(defcheck solution-750eaa0e
  (fn compress [l]
    (loop [l l pred nil acc '()]
      (if (empty? l)
        (reverse acc)
        (let [first (first l)]
          (recur (rest l) first (if (= first pred) acc (conj acc first))))))))

(defcheck solution-751d7fb3
  (fn c [s]
    (when-let [[f & r] (seq s)]
      (if (= f (first r))
        (c r)
        (cons f (c r))))))

(defcheck solution-75ae9378
  (fn [input]
    (loop [xs input, result []]
      (let [x (first xs)
            remainder (drop-while #(= x %) xs)]
        (if (empty? xs)
          result
          (recur remainder (conj result x))
          )
        )
      )
    ))

(defcheck solution-75c08c02
  #(letfn [(soln [prev lst]
             (if (= lst '())
               '()
               (if (= prev (first lst))
                 (soln prev (rest lst))
                 (cons (first lst) (soln (first lst) (rest lst))))))]
     (soln nil %)))

(defcheck solution-76c9d4c5
  reduce #(if (not= %2 (peek %1))
            (conj %1 %2)
            %1) [])

(defcheck solution-76ca6905
  #((fn f
      ([x]
       (f x '()))
      ([x r]
       (if (empty? x)
         (reverse r)
         (if (not= (first x) (second x))
           (recur (rest x) (into r (list (first x))))
           (recur (rest x) r))))) (apply list %)))

(defcheck solution-77197c6c
  (fn compress
    [input]
    (map first (partition-by identity input))))

(defcheck solution-7749c7c
  reduce #(if (= (last %1) %2)
            %1
            (conj %1 %2)) [])

(defcheck solution-774d4b33
  (fn [[x & more]] (reduce (fn [acc i] (if (= (last acc) i) acc (concat acc (list i) )) )     (list x)  more)   ))

(defcheck solution-77fbb89d
  (fn [xs] (map first (partition-by identity xs))))

(defcheck solution-780cdf9c
  (comp reverse
        (partial reduce
          (fn [memo el]
            (if (= (first memo) el)
              memo
              (conj memo el)))
          '())))

(defcheck solution-7812ba74
  (fn compr [seq]
    (cond (= (count seq) 0) '()
          (= (count seq) 1) (list (first seq))
          (= (first seq) (second seq)) (compr (rest seq))
          (= (first seq) (second seq)) (compr (rest seq))
          :else
          (cons (first seq) (compr (rest seq))))))

(defcheck solution-7850e1d1
  (fn compress [a] (loop [st a myret []]
                     (if (empty? st) myret
                                     (recur (drop-while #(= % (first st)) st) (conj myret (first st)))))))

(defcheck solution-786f4851
  (fn
    [coll]
    (map first (partition-by identity coll))))

(defcheck solution-78d4f3d3
  (fn [s]
    (loop [s s cs []]
      (cond
        (empty? s) cs
        (= (first s) (last cs)) (recur (rest s) cs)
        :else (recur (rest s) (conj cs (first s)))))))

(defcheck solution-79146677
  (fn [x] (map first (partition-by identity (seq x)))))

(defcheck solution-7a20d3c6
  (fn [xs] ((fn iter [xs state res]
              (cond (empty? xs) res
                    (= (first xs) state) (iter (rest xs) state res)
                    :else (iter (rest xs) (first xs) (concat res (cons (first xs) ()))))) (rest xs) (first xs) (cons (first xs) () ))))

(defcheck solution-7a348785
  (fn [s] ((fn [sq qs]
             (if (empty? sq)
               (reverse qs)
               (recur (rest sq) (if (or (= (count sq) 1) (not= (first sq) (second sq)))
                                  (cons (first sq) qs)
                                  qs)))) s ())))

(defcheck solution-7ab3465c
  (fn [col]
    (reverse
      (reduce
        #(if (= (first %1) %2)
           %1
           (cons %2 %1)
           )
        '()
        col
        )
      )
    ))

(defcheck solution-7ac017f0
  (fn de-dup [s]
    (if (seq s)
      (let [[f & r] s]
        (cons
          f
          (de-dup (drop-while #(= f %) r))
          )
        )
      )
    ))

(defcheck solution-7b19af98
  #(reduce (fn [r x] (if (= (last r) x)
                       r
                       (conj r x))) [] %))

(defcheck solution-7bb2927d
  (fn [s]
    (loop [v s, result []]
      (if (empty? v)
        result
        (if (= (last result) (first v))
          (recur (rest v) result)
          (recur (rest v) (conj result (first v))))))))

(defcheck solution-7bc00a2d
  (fn [s]
    (reduce
      (fn [s' v]
        (if (= v (last s'))
          s'
          (conj s' v)))
      []
      s)))

(defcheck solution-7bc8e855
  (fn [coll]
    (reduce
      #(if (= (last %1) %2) %1 (conj %1 %2))
      [(first coll)]
      (rest coll)
      )
    ))

(defcheck solution-7c4350fa
  #(loop [inp % out []]
     (if (empty? inp)
       out
       (if (not= (first inp) (last out))
         (recur (rest inp) (conj out (first inp)))
         (recur (rest inp) out)
         )
       )
     ))

(defcheck solution-7c6f30b3
  #(map first (map distinct (partition-by identity %))))

(defcheck solution-7c7094cc
  ;ugly
  (fn p [coll]
    (let [compressed
          (seq
            (loop [result [] current (first coll) items (rest coll)]
              (if (nil? current)
                result
                (if (= (first items) current)
                  (recur result (first items) (rest items))
                  (recur (conj result current) (first items) (rest items))
                  )
                )
              )
            )]
      (if (string? coll)
        (apply str compressed)
        compressed
        )
      )
    ))

(defcheck solution-7c8ce87e
  #(loop [acc '() [x & xs] %]
     (cond (empty? xs) (reverse (conj acc x))
           (= x (first xs)) (recur acc xs)
           :else (recur (conj acc x) xs))))

(defcheck solution-7d8d9a39
  (fn [xs]
    (reduce
      (fn [col x]
        (if (= x (last col))
          col
          (conj col x)))
      []
      xs)))

(defcheck solution-7de21b6c
  #(map first (partition-by identity  %)))

(defcheck solution-7de53414
  (fn removeduplicates [x]
    "Removes duplicates from a sequence."
    (if (seq x)
      (if (= (peek (vec x)) (peek (pop (vec x))))
        (removeduplicates (pop (vec x)))
        (conj (removeduplicates (pop (vec x))) (peek (vec x))))
      x)))

(defcheck solution-7ead0b92
  (fn compress [s] (conj (into [] (map first (filter #(not (= (first %1) (second %1)))
                                               (map list s (rest s))))) (last s))))

(defcheck solution-7ebd3a1d
  (fn [sq] (reduce (fn [acc el] (if (= (peek acc) el) acc (conj acc el))) [] sq)))

(defcheck solution-7ee0a946
  (fn [s]
    (loop [remainder s, compressed [], final nil]
      (if-let [[x & xs] remainder]
        (if (not= x final)
          (recur xs (conj compressed x) x)
          (recur xs compressed          final))
        compressed))))

(defcheck solution-7ef5d7c9
  reduce (fn [v i] (if (not= (last v) i) (conj v i) v)) [])

(defcheck solution-7f54bf2e
  #(reduce (fn [a itm] (if (= (last a) itm) a (conj a itm))) [] %))

(defcheck solution-7f740f8a
  (fn [x]
    (map (fn [x] (first x)) (partition-by identity x))
    ))

(defcheck solution-7fc60177
  (fn f [arg]
    (loop [x (rest arg) p (first arg) acc [(first arg)]]
      #_(println acc)
      (if (empty? x)
        acc
        (if (= p (first x))
          (recur (rest x) (first x) acc)
          (recur (rest x) (first x) (conj acc (first x))))))))

(defcheck solution-7fca9716
  (fn  [x] (reduce  (fn [a b]
                      (cond (empty? a)
                            [b]
                            (= (last a) b)
                            a
                            :else
                            (conj a b))) [] x)))

(defcheck solution-7fe8e32c
  (fn [x] (reduce (fn [m x] (if (= (last m) x) m (conj m x))) [] x)))

(defcheck solution-809644b3
  (fn [s]
    (loop [x s
           l nil
           acc []]
      (if (nil? x)
        (seq acc)
        (let [head (first x)]
          (if (not= head l)
            (recur (next x) head (conj acc head))
            (recur (next x) l acc)))))))

(defcheck solution-80b4f65a
  #(reverse (loop [remaining-seq %
                   result '()]
              (if (empty? remaining-seq)
                result
                (let [[head & tail] remaining-seq]
                  (recur tail
                    (if (= head (first result))
                      result
                      (cons head result))))))))

(defcheck solution-80f4a211
  (fn compress [y] (reverse (reduce #(if (coll? %1) (if (distinct? (first %1) %2) (conj %1 %2) %1) (list %1)) () y))))

(defcheck solution-8100f6c0
  (fn dedup [sq]
    (loop [s []
           r sq
           c nil]
      (if (empty? r)
        s
        (recur (if (= c (first r)) s (conj s (first r)))
          (rest r)
          (first r))))))

(defcheck solution-8121035e
  (fn rd [s]
    (loop [a s, b '()]
      (if (empty? a)
        b
        (recur (rest a) (if (= (first a) (last b))
                          b
                          (concat b (list (first a)))))))))

(defcheck solution-81769b63
  (fn [s]
    (loop [res [(first s)] left (rest s)]
      (cond
        (empty? left) res
        (= (last res) (first left)) (recur res (rest left))
        :else (recur (conj res (first left)) (rest left))))))

(defcheck solution-81ab24e0
  (fn[x]
    (loop [l x, v [], f nil]
      (if (empty? l)
        (apply list v)
        (if (= (first l) f)
          (recur (rest l) v f)
          (recur (rest l) (conj v (first l))
            (first l)))))))

(defcheck solution-81e7f302
  (fn [x] (map #(first %) (partition-by identity x))))

(defcheck solution-81ecdf2c
  #(mapcat distinct (partition-by identity %)))

(defcheck solution-8274602d
  reduce (fn [c e]
           (if
            (= (last c) e)
             c
             (conj c e))) [])

(defcheck solution-8283fa0f
  (fn [a]
    (conj
      (map
        #(first %)
        (filter #(apply not= %) (map vector (drop 1 a) a)))
      (first a)
      )
    ))

(defcheck solution-8292ac3d
  #(reduce (fn[a b]
             (if (= b (last a))
               a
               (conj a b))) [] (seq %)))

(defcheck solution-833e96ef
  (fn [xs] (reduce #(if (= (last %1) %2) %1 (concat %1 [%2])) () xs)))

(defcheck solution-83da3abf
  (fn [s] (cons (first s)(filter identity (map #(if (not= %1 %2) %2) s (rest s))))))

(defcheck solution-84ffa570
  #(reduce
     (fn [result value]
       (if
        (= value (peek result))
         result
         (conj result value)))
     []
     %))

(defcheck solution-85336af0
  (fn [coll]
    (reverse (reduce #(if (= (first %1) %2) %1 (cons %2 %1)) (cons '() coll)))))

(defcheck solution-855a777c
  (fn [s]
    (loop [c [] x s las nil]
      (if (empty? x) c
                     (recur (if (= las (first x)) c (conj c (first x)))
                       (rest x)
                       (first x))))))

(defcheck solution-862e07b6
  (fn [s]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) []  s)))

(defcheck solution-86ff3de9
  reduce #(if (= (peek %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-8772241e
  (fn compress [coll]
    (let [first-item (first coll)]
      (if (nil? first-item)
        '()
        (cons first-item (compress (drop-while (partial = first-item) (rest coll))))))))

(defcheck solution-878717ac
  (fn [input]
    (let [input-seq (seq input)]
      (loop [remaining-seq input-seq prev nil ans []]
        (if (empty? remaining-seq)
          (lazy-seq ans)
          (let [[ptr & remain] remaining-seq]
            (if (= ptr prev)
              (recur remain prev ans)
              (recur remain ptr (conj ans ptr)))))))))

(defcheck solution-87c2311c
  (fn remove-consecutives
    ([l] (remove-consecutives l []))
    ([l resp]
     (if (= l [])
       resp
       (let [f (first l)]
         (if (= (last resp) f)
           (remove-consecutives (rest l) resp)
           (remove-consecutives (rest l) (conj resp f))))))))

(defcheck solution-8818d32f
  (fn [coll]
    (loop [the-last nil, res [], coll coll]
      (if-let [nxt (first coll)]
        (if (= the-last nxt)
          (recur the-last res (next coll))
          (recur nxt (conj res nxt) (next coll)))
        res))))

(defcheck solution-881a0784
  (partial reduce (fn [s e] (
                              if (= (last s) e)
                              s
                              (conj s e)
                              )) []))

(defcheck solution-884cc7e8
  (fn [x]
    (loop [s x r []]
      (if (empty? s)
        r
        (recur (drop-while #(= (first s) %) (rest s)) (conj r (first s)))))))

(defcheck solution-88b61434
  (fn [col]
    (let [same? (fn [[x y]] (= x y))]
      (let [z (partition-all 2 1 col)]
        (map first (filter #(not (same? %)) z))))))

(defcheck solution-88bf454c
  (fn[v]
    (reverse(loop [x nil y v ans '()]
              (if (empty? y)
                ans
                (if (= x (first y))
                  (recur x (next y) ans)
                  (recur (first y) (next y) (conj ans (first y)))))))))

(defcheck solution-896e2c72
  #(reduce (fn [c, v] (if(not= (last c) v) (conj c v) c)) [] %))

(defcheck solution-89ab88ef
  (fn [s]
    (loop [ret [] remaining (seq s) prev nil]
      (if (empty? remaining)
        (reverse ret)
        (if (= prev (first remaining))
          (recur ret (rest remaining) (first remaining))
          (recur (cons (first remaining) ret) (rest remaining) (first remaining))
          )))))

(defcheck solution-89b17a9f
  (fn [coll]
    (loop [acc []
           lst nil
           coll coll]
      (if (empty? coll)
        acc
        (let [fst (first coll)
              rst (rest coll)]
          (if (= fst lst)
            (recur acc lst rst)
            (recur (conj acc fst) fst rst)))))))

(defcheck solution-8a13ab5
  (fn[s]
    (loop [s s r [] v nil]
      (if (empty? s) r
                     (let [cv (first s)]
                       (recur (rest s) (if (= cv v) r (conj r cv )) cv))))))

(defcheck solution-8a463a9f
  #(loop [acc [] last nil coll %]
     (if-let [[a & coll] coll]
       (if (= last a)
         (recur acc last coll)
         (recur (conj acc a) a coll))
       acc)))

(defcheck solution-8a66408
  (fn [c]
    (
     (if (string? c)
       #(apply str %)
       (fn [x] x))
     (reverse (reduce
                (fn [a b] (if (= (peek a) b)
                            a
                            (conj a b)))
                '()
                c)))))

(defcheck solution-8a82823d
  (partial (fn dedup [prev s] (if (seq s) (if (= (first s) prev) (dedup prev (rest s)) (cons (first s) (dedup (first s) (rest s)))) s)) nil))

(defcheck solution-8ab78ebc
  (fn compress
    [s]
    (reduce (fn [result v]
              (if (= v (last result))
                result
                (conj result v)))
      [] s)))

(defcheck solution-8ad8e279
  (fn [sq]
    (letfn
     [(helper [rv sq]
        (if (empty? sq) rv
                        (if (= (first rv) (first sq)) (helper rv (rest sq)) (helper (cons (first sq) rv) (rest sq)
                                                                              )
                                                      )
                        )
        )
      ]
      (reverse (helper '() sq)))))

(defcheck solution-8ae4a54e
  (fn compress [s] (reduce (fn [a b] (if (= (last a) b) a (conj a b))) [] s)))

(defcheck solution-8b196c67
  (fn remove-dup [s]
    (cond
      (empty? s) s
      (= 1 (count s)) s
      (= (first s) (second s)) (remove-dup (next s))
      :else (cons (first s) (remove-dup (next s))))))

(defcheck solution-8b9b037d
  (fn [x] (let [fv (map first (partition-by identity x))] (if (string? x) (apply str fv) (apply list fv)))))

(defcheck solution-8be868bf
  (fn [coll]
    (loop [coll (seq coll)
           acc []]
      (if (empty? coll)
        acc
        (let [f (first coll)
              hs (take-while #(= f %) coll)
              ts (drop-while #(= f %) coll)]
          (recur ts (conj acc f)))))))

(defcheck solution-8bf84017
  ;(fn cmprs [coll]
  ;  (when-let [[f & r] (seq coll)]
  ;    (if (= f (first r))
  ;      (cmprs r)
  ;      (cons f (cmprs r)))))

  #(map first (partition-by identity %)))

(defcheck solution-8c18a206
  (fn [s] (reduce #(if (not= (last %1) %2) (conj %1 %2) %1) [(first s)] (rest s))))

(defcheck solution-8c1aa3a8
  (fn this
    ([[hd & tl]] (this hd tl [hd]))
    ([prev [hd & tl :as elems] acc]
     (if (empty? elems)
       acc
       (if (= prev hd)
         (recur hd tl acc)
         (recur hd tl (conj acc hd)))))))

(defcheck solution-8c5787bd
  (fn [coll]
    (loop [input (seq coll)
           last nil
           result []]
      (if (empty? input)
        result
        (recur (rest input) (first input)
          (if (= last (first input)) result
                                     (conj result (first input))))))))

(defcheck solution-8cc423ff
  (fn [coll] (reduce #(if (= (peek %1) %2) %1 (conj %1 %2)) [] coll)))

(defcheck solution-8cd501a9
  (fn compress [seq]
    (reduce (fn [acc e] (if (= (last acc) e) acc (conj acc e)))
      [] seq)))

(defcheck solution-8cd9b9be
  #(reverse (loop [s %
                   p (first s)
                   r `(~(first s))]
              (if (empty? s)
                r
                (recur (rest s) (first s) (if (= p (first s))
                                            r
                                            (conj r (first s))))))))

(defcheck solution-8cee40d4
  (fn [xs]
    (let [my-dup (fn my-dup [x out in]
                   (cond
                     (empty? in) (conj out x)
                     (= x (first in)) (my-dup x out (rest in))
                     :else (my-dup (first in) (conj out x) (rest in))
                     )
                   )
          ]
      (reverse (my-dup (first xs) '() (rest xs)))
      )
    ))

(defcheck solution-8d126398
  (fn [x] (reduce #(if (= (last %1) %2) %1 (concat %1 (list %2)) ) [] x)))

(defcheck solution-8d13f671
  (fn [xs]
    (reduce
      (fn [acc n]
        (let [last' (last acc)]
          (if (= n last')
            acc
            (conj acc n))))
      [] xs)))

(defcheck solution-8d32be0f
  (comp
   (partial map first)
   (partial partition-by identity)))

(defcheck solution-8d69e61e
  (fn [s]
    (->> (partition 2 1 [0] s)
      (remove #(= (first %) (last %)))
      (map first))))

(defcheck solution-8dd1d4fa
  (fn compress
    [coll]
    (loop [sq-in coll
           sq-out []]
      (if (seq sq-in)
        (let [nxt (first sq-in)
              prv (last sq-out)]
          (if (= prv nxt)
            (recur (rest sq-in) sq-out)
            (recur (rest sq-in) (conj sq-out nxt))))
        sq-out))))

(defcheck solution-8dd4aa85
  (fn compress-part
    [coll]
    {:pre [(or (sequential? coll) (string? coll) (nil? coll))]}
    (->> (partition-by identity coll)
      (map first))))

(defcheck solution-8e6af1fd
  (fn [s]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] s)))

(defcheck solution-8e6e10dd
  (fn [coll]
    (:res
     (reduce (fn [acc e]
               (if (= (:prev acc) e)
                 acc
                 {:prev e :res (conj (:res acc) e)}))
       {:res [] :prev nil}
       coll))))

(defcheck solution-8e932e47
  (fn f [l]
    (if (empty? l)()
                  (if (empty? (rest l)) l
                                        (if (= (first l)(second l))(f (rest l))
                                                                   (lazy-seq (cons (first l)(f (rest l)))))))))

(defcheck solution-8ed6a5ad
  (partial reduce #(if (= (last %1) %2) %1 (conj %1 %2) ) []))

(defcheck solution-8ef609dc
  (fn compress [s]
    (if (= (count s) 1) s
                        (let [[x & s1] s]
                          (if (= x (first s1))
                            (compress s1)
                            (cons x (compress s1)))))))

(defcheck solution-8fb29adc
  (fn [xs] (reverse (reduce #(if (= (first %) %2) % (cons %2 %)) [] xs))))

(defcheck solution-906fd936
  reduce (fn [acc x]
           (if (= x (last acc))
             acc
             (conj acc x)
             )

           ) [])

(defcheck solution-90d2cdf1
  (fn [xs]
    (map first
      (partition-by identity xs))))

(defcheck solution-90e89f83
  (fn conseq [sq]
    (map first (partition-by identity sq))
    ))

(defcheck solution-91ac4db1
  #(map first (partition-by (set %) %)))

(defcheck solution-91e8a86e
  #(map first
     (partition-by identity %)))

(defcheck solution-91f504ff
  (fn [in]
    (reverse (reduce (fn [a b]
                       (if (= (first a) b)
                         a
                         (cons b a))) '() in))))

(defcheck solution-920ea46d
  (fn nodupees [input]
    (let [exc (fn exc [x input]
                (if (empty? input) '()
                                   (if (= x (first input))
                                     (exc x (rest input))
                                     (concat (list (first input)) (exc (first input) (rest input))))))]
      (concat
       (list (first input))
       (exc (first input) (rest input))))))

(defcheck solution-92959d9d
  (fn [x] (reverse (reduce #(if (= %2 (first %1)) %1 (conj %1 %2)) () x))))

(defcheck solution-92989279
  reduce (fn [x y] (if (not= (last x) y) (conj x y) x)) [])

(defcheck solution-929e4591
  (fn cmprs [coll]
    (when-let [[f & r] (seq coll)]
      (if (= f (first r))
        (cmprs r)
        (cons f (cmprs r))))))

(defcheck solution-92ca7f6a
  #(reduce (fn [acc x] (if (= (last acc) x) acc (conj acc x))) [] %))

(defcheck solution-9347bca7
  (fn compress [s]
    (let [f (first s)
          r (rest s)]
      (cond (empty? s) s
            (= f (first r)) (compress r)
            :else (cons f (compress r))))))

(defcheck solution-93d5657
  (fn
    [coll]
    (reduce
      #(if (= (last %1) %2)
         %1
         (conj %1 %2))
      []
      coll) ))

(defcheck solution-93e18c65
  (fn [coll]
    (remove nil?
      (map #(if (not= %1 %2) %1) coll (cons "push!" coll)))))

(defcheck solution-93e93be6
  (fn compact [coll]
    (reverse (reduce (fn [s n] (if (= (first s) n) s (conj s n))) nil coll))))

(defcheck solution-94f56f24
  (fn [x](filter #(or %)(map #(if(= % %2)nil %) x (cons :a x)))))

(defcheck solution-956a5cff
  (fn [a]
    (map first (partition-by identity a))))

(defcheck solution-9570400a
  (fn compress [xs]
    (if (empty? xs)
      '()
      (cons (first xs) (compress (drop-while (partial = (first xs)) (rest xs)))))))

(defcheck solution-958cfdf4
  #(reduce (fn [acc e] (if (= e (last acc)) acc (concat acc [e]))) (take 1 %) (rest %)))

(defcheck solution-95aba581
  (fn dedup [s]
    (if (empty? s)
      '()
      (let [r (dedup (rest s))]
        (if (= (first s) (first r))
          r
          (cons (first s) r))))))

(defcheck solution-95bd1f1
  (fn
    [s]
    (reduce
      #(if (= (last %) %2)
         %
         (concat % [%2]))
      '()
      s)))

(defcheck solution-95cc7c37
  #(loop[result [], input %]
     (cond
       (nil? input) result
       (= (last result) (first input)) (recur result (next input))
       :else (recur (conj result (first input)) (next input)))))

(defcheck solution-95e137a
  (fn [vs]
    (reduce (fn [acc v]
              (cond
                (empty? acc) (conj (empty acc) v)
                (= (last acc) v) acc
                :else (conj acc v)))
      []
      vs)))

(defcheck solution-9602c213
  #(map last (partition-by max %)))

(defcheck solution-9659cda2
  (fn [s]
    (->> s
      (partition-by identity)
      (mapcat distinct))))

(defcheck solution-965d7018
  #(reverse (reduce (fn [x y] (if (empty? x) (list y) (if (= (first x) y) x (cons y x)))) () %)))

(defcheck solution-9703429b
  reduce (fn [l c]
           (if (empty? l)
             (list c)
             (if (= (last l) c)
               l
               (concat l (list c))
               )
             )
           ) '())

(defcheck solution-9776e79d
  reduce (fn [res item]
           (if (= (last res) item)
             res
             (conj res item))) [])

(defcheck solution-97a36af3
  (fn
    [[hh & tt]]
    (loop [[h & t] tt
           prev hh
           accum [hh]]
      (let [new-accum (if (= prev h)
                        accum
                        (conj accum h))]
        (if (nil? t)
          (into () (reverse new-accum))
          (recur t h new-accum))))))

(defcheck solution-97d15e1a
  (fn [s]
    (loop [s s
           result []]
      (if (empty? s)
        result
        (let [same? (= (first s) (second s))]
          (recur (rest s) (if same? result (conj result (first s)))))))))

(defcheck solution-97ff0a95
  (fn r
    ([xs] (r (first xs) (next xs)))
    ([c xs] (lazy-seq (if (seq xs)
                        (if (= c (first xs)) (r c (next xs)) (cons c (r (first xs) (next xs))))
                        (cons c nil))))))

(defcheck solution-981b5882
  (fn [s]
    (loop [l nil e (first s) ss (rest s) out []]
      (if (empty? ss)
        (if (= l e) out (conj out e))
        (recur e (first ss) (rest ss) (if (= l e) out (conj out e)))))))

(defcheck solution-98455658
  (fn [xs] (let [xs1 (cons nil xs)
                 xs2 (lazy-cat xs '(nil))
                 unique (for [[x1 x2] (map vector xs1 xs2)
                              :when (not (= x1 x2))]
                          x2)]
             (drop-last unique))))

(defcheck solution-988e38d2
  reduce #(if (= %2 (last %1)) %1 (concat %1 [%2])) [])

(defcheck solution-9898231e
  (fn [coll]
    (reduce (fn [a b]
              (if (not= (last a) b)
                (conj a b)
                a)) [] coll)))

(defcheck solution-99bf6f20
  (fn mycomp
    [myseq]
    (loop
     [myrest myseq c nil myres nil]
      (if (empty? myrest)
        myres
        (if (= c (first myrest))
          (recur (rest myrest) c myres)
          (recur (rest myrest) (first myrest) (concat myres (list (first myrest)))))))))

(defcheck solution-9a8d8736
  #(loop [s %,res []] (if (empty? s) res (recur (rest s) (if (= (first s) (last res)) res (conj res (first s)))))))

(defcheck solution-9a9d89b2
  (fn [coll]
    (loop [res (vector) xs coll]
      (if (empty? xs)
        res
        (recur
          (if (= (last res) (first xs)) res (conj res (first xs)))
          (rest xs))))))

(defcheck solution-9b39bdd3
  (fn [s]
    (loop [rr [] ss s p nil]
      (if (empty? ss)
        rr
        (if (= (first ss) p)
          (recur rr (rest ss) p)
          (recur (conj rr (first ss)) (rest ss) (first ss)))))))

(defcheck solution-9b41c818
  (fn [s]
    (reduce #(if (= (last %) %2) % (conj % %2)) [] (seq s))))

(defcheck solution-9b457ce1
  #(let [s (seq %)]
     (concat (map first (filter (fn [[a b]] (not(= a b))) (partition 2 1 s))) [(last s)])))

(defcheck solution-9bb461cc
  #(reduce (fn [a b] (if (= (last a) b) a (conj a b))) [] %))

(defcheck solution-9c155e5e
  (fn rd [x]
    (if (< (count x) 2)
      x
      (if (= (first x) (second x))
        (rd (rest x))
        (cons (first x) (rd (rest x)))
        )
      )
    ))

(defcheck solution-9c727f53
  #(loop [p nil s % r []]
     (cond (empty? s) r
           (= (first s) p) (recur (first s) (rest s) r)
           :else (recur (first s) (rest s) (conj r (first s))))))

(defcheck solution-9cf68429
  (fn [l]
    (map first (partition-by identity l))
    ))

(defcheck solution-9d5d8827
  #(concat
    (for [[x y] (partition 2 1 %)
          :when (not= x y)]
      x)
    (list (last %))))

(defcheck solution-9d6e7823
  (fn [s] (map first (partition-by identity s))))

(defcheck solution-9d98cc0b
  #(loop [xs % compress '() end nil]
     (if xs
       (let [head (first xs)]
         (if (empty? compress)
           (recur (next xs) (concat compress (list head)) head)
           (if (not= end head)
             (recur (next xs) (concat compress (list head)) head)
             (recur (next xs) compress head))))
       compress)))

(defcheck solution-9e272563
  (fn u
    ([x]
     (u (rest x) (first x)))
    ([x f]
     (if x
       (let [[s & xs] x]
         (if (= s f)
           (u xs f)
           (conj (u xs s) f)))
       (list f)))))

(defcheck solution-9e817d2
  (fn [x]
    (loop [last nil
           todo x
           res []]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (next todo)]
          (if (= f last)
            (recur f r res)
            (recur f r (conj res f))
            )
          )
        )
      )
    ))

(defcheck solution-9eb75093
  #(loop [a (seq %) c (count a) l '()]
     (if (= c 0)
       l
       (if (= (nth a (- c 1)) (first l))
         (recur a (dec c) l)
         (recur a (dec c) (cons (nth a (- c 1)) l))))))

(defcheck solution-9eda2bef
  (fn foo [s]
    (loop [rv '() tail s]
      (if (empty? tail)
        (reverse rv)
        (if-not (= (first rv) (first tail))
          (recur (conj rv (first tail)) (rest tail))
          (recur rv (rest tail)))))))

(defcheck solution-9f1ed1bd
  (fn [c]
    (let [pairs (filter #((complement =) (first %) (last %)) (partition 2 1 c))]
      (concat (map first (drop-last pairs)) (last pairs)))))

(defcheck solution-9f7bf68e
  (fn compress [x] (when-let [ [f & r] (seq x)]
                     (if (= f (first r))
                       (compress r)
                       (cons f (compress r))
                       ))))

(defcheck solution-a0a370e0
  (fn [input]
    (let [lst (seq input)]
      (loop [lst1 (rest lst)
             lst2 (list (first lst))]
        (if (empty? lst1)
          (reverse lst2)
          (if (= (first lst1) (first lst2))
            (recur (rest lst1) lst2)
            (recur (rest lst1) (conj lst2 (first lst1)))))))))

(defcheck solution-a0ba321f
  (fn [r, s]
    (if (seq s)
      (if (= (first s) (last r)) (recur r (rest s))
                                 (recur (conj r (first s)) (rest s)))
      r)) [])

(defcheck solution-a29e1d73
  (fn [seq] (reduce #(if (= %2 (last %1)) %1 (conj %1 %2)) [] seq)))

(defcheck solution-a3781279
  reduce (fn f[l x]
           (if (= (last l) x)
             l
             (concat l (list x)))) ())

(defcheck solution-a37ab6b4
  (fn [s] (reverse (reduce #(if (= (first %1) %2)
                              %1
                              (cons %2 %1)) (cons (first s) '()) s))))

(defcheck solution-a3e12be6
  (fn [x]
    (loop [x x v []]
      (if (nil? (first x))
        v
        (if (= (first x) (last v))
          (recur (rest x) v)
          (recur (rest x) (concat v (vector (first x)))))))))

(defcheck solution-a43f31ca
  (fn
    [[fi & li] & par]
    (loop
     [head fi
      tail li
      dgst (vector)]
      (let
       [ndgst (if
               (=
                 (first tail)
                 head)
                dgst
                (conj dgst head))]
        (if
         (empty? tail)
          ndgst
          (recur
            (first tail)
            (rest tail)
            ndgst))))))

(defcheck solution-a467d067
  (fn p30 [l]
    (case (count l)
      0 []
      1 l
      (let [[a b & r] l
            r' (p30 (cons b r))]
        (if (= a b)
          r'
          (cons a r'))))))

(defcheck solution-a4ab981a
  (fn compress
    ([somelist] (compress (seq somelist) []))
    ([somelist result]
     (if (empty? somelist)
       result
       (if (= (first somelist)
             (second somelist))
         (compress (rest somelist)
           result)
         (compress (rest somelist)
           (conj result (first somelist))))))))

(defcheck solution-a7c06401
  (letfn [(dd [cum it]
            (if (= (last cum) it)
              cum
              (conj cum it)))]
    (partial reduce dd [])))

(defcheck solution-a80b0f8b
  (fn r
    ([[l & ls]]
     (if l
       (r ls [] l)
       []))
    ([[l & ls] a c]
     (if l
       (if (= l c)
         (recur ls a c)
         (recur ls (conj a c) l))
       (conj a c)))))

(defcheck solution-a811b378
  #(for [g (partition-by identity %)] (first g)))

(defcheck solution-a865a029
  #(map (fn[[a b]] b)
     (remove (fn [[a b]] (= a b))
       (partition 2 1 (cons 0 %)))))

(defcheck solution-a8900491
  (fn dedup [s]
    (if (< (count s) 2)
      s
      (if (= (first s) (second s))
        (dedup (rest s))
        (conj (dedup (rest s)) (first s))))))

(defcheck solution-a8b1ceb7
  (fn
    [in]
    (loop [remaining in
           curr nil
           completed '()]
      (if (= 0 (count remaining))
        (reverse completed)
        (if (= curr (first remaining))
          (recur (rest remaining) curr completed)
          (recur (rest remaining) (first remaining) (conj completed (first remaining)))))
      )))

(defcheck solution-a8b25fa1
  #(reverse (reduce (fn [a e]
                      (if (= e (first a))
                        a
                        (cons e a)))
              '()
              %)))

(defcheck solution-a933745
  #(into () ((fn f [xs acc]
               (if (empty? xs)
                 acc
                 (f (rest xs) (if (= (first xs) (first acc))
                                acc
                                (conj acc (first xs)) )))) % ())))

(defcheck solution-a94e6206
  #(cons (first %)
     ((fn f [xs y] (cond (empty? xs) ()
                         (= (first xs) y) (f (rest xs) y)
                         :else (cons (first xs)
                                 (f (rest xs)
                                   (first xs)))))
      (rest %) (first %))))

(defcheck solution-a99a604e
  #(loop [acc [] lst %]
     (if (empty? lst) acc
                      (recur (if (= (last acc) (first lst)) acc (conj acc (first lst))) (rest lst)))))

(defcheck solution-abf094ba
  (fn [x]
    (reduce
      (fn [coll e]
        (if (seq coll)
          (if (= (last coll) e)
            coll
            (conj coll e)
            )
          [e]
          ))
      []
      x
      )))

(defcheck solution-ad05cfda
  #(filter (fn [n] (not (nil? n))) (map (fn [a b] (if  (not= a b) a nil)) % (concat "0" %))))

(defcheck solution-ad1aaffc
  reduce #(if (= (last %1) %2) %1 (conj %1 %2) ) [])

(defcheck solution-ada580bd
  reduce #(if-not (= (last %1) %2) (conj %1 %2) %1) [])

(defcheck solution-ada84977
  (fn [s]
    (reduce
      #(if (= (last %1) %2) %1 (concat %1 (list %2)))
      '()
      s)))

(defcheck solution-ae4d351a
  (fn [xs]
    (map first (partition-by identity xs))))

(defcheck solution-af3c9024
  reduce #(if (= %2 (last %1)) %1 (conj %1 %2)) [])

(defcheck solution-af848f21
  (fn [in] (reverse (reduce #(if (= (first %1) %2) %1 (conj %1 %2)) '() in))))

(defcheck solution-b0be1457
  (fn [lst] (reverse (reduce (fn [r e]
                               (if (= (first r) e) r (cons e r))) '() lst))))

(defcheck solution-b10cfd1
  #(reduce (fn[x,y](if (= (last x) y) x (conj x y))) [] (seq %)))

(defcheck solution-b11ed8a1
  reduce #(if (not= %2 (last %1)) (conj % %2) %) [])

(defcheck solution-b18d1399
  (fn [coll]
    (map first (partition-by identity coll))))

(defcheck solution-b2567551
  (fn [coll] (concat (loop [coll coll
                            acc []]
                       (if (empty? coll)
                         acc
                         (recur (rest coll) ( if (not= (first coll) (second coll)) (conj acc (first coll)) acc)))))))

(defcheck solution-b2586c8e
  (fn f [[x & xs]] (when x (cons x (f (drop-while #(= x %) xs))))))

(defcheck solution-b30537ab
  #(loop [c % prev nil res []]
     (let [f (first c)
           r (rest  c)]
       (cond
         (empty? c) res
         (= f prev) (recur r f res)
         :else      (recur r f (conj res f))))))

(defcheck solution-b3212c2d
  (fn [coll]
    (reduce (fn [a x]
              (cond
                (nil? (last a)) (conj a x)
                (not= (last a) x) (conj a x)
                :else a))
      []
      (seq coll))))

(defcheck solution-b34cda97
  (fn [s] (reduce #(if-not (= (last %) %2) (conj % %2) %) [] s)))

(defcheck solution-b39743a
  (fn [s] (reduce
            #(if (= %2 (last %1)) %1
                                  (concat %1 [%2])) [] s)))

(defcheck solution-b46dd40f
  #(reduce (fn [a x] (if (= x (last a)) a (conj a x))) [] %))

(defcheck solution-b4be3c49
  (fn !
    ([x] (! nil x))
    ([x y]
     (if (empty? y)
       '()
       (let [fy (first y) r (! fy (rest y))]
         (if (= x fy) r (concat [fy] r))
         )))))

(defcheck solution-b4eaf4da
  (fn [l] (map first (partition-by identity l))))

(defcheck solution-b5169794
  (fn dups [x] (reverse (reduce (fn [empty elem]
                                  (if (not (= elem (first empty))) (cons elem empty) empty))
                          [] x))))

(defcheck solution-b6036298
  (fn [s]
    (for [[f] (partition-by identity s)] f)
    ))

(defcheck solution-b6592b54
  (fn [x]
    ((fn compress [c1 c2]
       (if (empty? c2)
         c1
         (if (= (last c1) (first c2))
           (compress c1 (rest c2))
           (compress (concat c1 [(first c2)]) (rest c2))))) [] x)))

(defcheck solution-b662199f
  (fn compress
    ([s] (compress s nil))
    ([s prev]
     (if (empty? s) '()
                    (let [f (first s)]
                      (if (= prev f)
                        (compress (rest s) f)
                        (cons f (compress (rest s) f))))))))

(defcheck solution-b66ef29f
  (fn compress [a b]
    (if (empty? b) a
                   (if (= (last a) (first b)) (compress a (rest b))
                                              (compress (conj a (first b)) (rest b))))) [])

(defcheck solution-b6bd3fb2
  (fn compress [coll]
    (loop [c (seq coll) acc []]
      (cond
        (empty? c) acc
        (= (first c) (second c)) (recur (rest c) acc)
        :else (recur (rest c) (conj acc (first c)))))))

(defcheck solution-b6e76be9
  reduce (fn [t v] (if (= (last t) v) t (conj t v))) [])

(defcheck solution-b72db6e8
  (fn [coll] (map first (remove #(and (apply = %) (= (count %) 2)) (partition-all 2 1 coll)))))

(defcheck solution-b7559a40
  (fn [seq]
    (reduce #(if (= (first %1) %2) %1 (cons %2 %1)) (list) (reverse seq))))

(defcheck solution-b7950a13
  (fn cmprs [s]
    (map first (partition-by identity s))))

(defcheck solution-b7a0a368
  (fn [coll] (reduce #(concat %1 (when (not= (last %1) %2) [%2])) [] coll)))

(defcheck solution-b7fbdf69
  #(map last (partition-by list %)))

(defcheck solution-b89961a
  (fn [xs] (reduce (fn [x x2] (if (= (last x) x2) x (conj x x2))) [] xs)))

(defcheck solution-b96906fe
  (fn [s]
    (if (empty? s) s
                   (let [f (first s) r (rest s)]
                     (if (empty? r) s
                                    (reverse
                                      (loop [out (conj (empty r) f)
                                             in r
                                             c f]
                                        (if (empty? in) out
                                                        (let [ff (first in) rr (rest in)]
                                                          (recur (if (= ff c) out (conj out ff)) rr ff)
                                                          )
                                                        )
                                        )
                                      )
                                    )
                     )
                   )
    ))

(defcheck solution-b9ad91bb
  (fn stripD [xs]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] xs)))

(defcheck solution-b9c8c75a
  (fn compress [x]
    (if (< (count x) 2)
      x
      (if (= (first x) (second x))
        (compress (rest x))
        (cons (first x) (compress (rest x)))
        )
      )
    ))

(defcheck solution-b9ec664b
  ( fn [s] (letfn [(compress [s e] (let [fst (first s) nxt (next s)]
                                     (cond (empty? s) nil
                                           (= fst e) (compress nxt e)
                                           :else (cons fst (compress nxt fst)) )))]
             (compress s nil))))

(defcheck solution-b9f40170
  reduce (fn [l r] (if (= (last l) r) l (conj l r))) [])

(defcheck solution-b9f95837
  (fn [coll]
    (loop [coll coll
           prev nil
           answer ()]
      (cond
        (nil? coll) (reverse answer)
        (= prev (first coll)) (recur (next coll) prev answer)
        :else (recur (next coll) (first coll) (conj answer (first coll)))))))

(defcheck solution-ba21a2d8
  (fn peu [x] (if (< (count x) 2) x (if (= (first x) (second x)) (peu (rest x)) (conj (peu (rest x)) (first x))))))

(defcheck solution-baee4645
  (fn [input]
    (loop [result [] elements input]
      (if (empty? elements)
        result
        (if (= (first elements) (last result))
          (recur result (rest elements))
          (recur (conj result (first elements)) (rest elements))
          )
        )
      )
    ))

(defcheck solution-bb9fae0a
  (fn compress [[x & more]]
    (if (empty? more) [x]
                      (if (= x (first more))
                        (lazy-seq (compress more))
                        (lazy-seq (cons x (compress more)))))))

(defcheck solution-bbfdb7c1
  #(reduce
     (fn[acc x] (if (= (last acc) x)
                  acc
                  (conj acc x)))
     [] %))

(defcheck solution-bc26d188
  (fn uniq [s] (cond (nil? s) s
                     (not (seq (rest s))) s
                     (= (first s) (second s)) (uniq (rest s))
                     :else    (cons (first s) (uniq (rest s))) )))

(defcheck solution-bc49edb
  (fn [s]
    (reduce #(concat %1 (if (= (last %1) %2) () (list %2))) '() s )))

(defcheck solution-bc54bdf8
  (fn [l]
    ((fn rcomp [f r]
       (cond
         (empty? r) (cons f '())
         (= f (first r)) (rcomp (first r) (rest r))
         :else (cons f (rcomp (first r) (rest r)))
         )
       ) (first l) (rest l))
    ))

(defcheck solution-bc66f338
  reduce (fn [r e] (if (= 0 (compare (last r) e)) r (conj r e))) [])

(defcheck solution-bcfd84d2
  (fn compress [x]
    (letfn [(cc [& args]
              (if (empty? args) '()
                                (if (= (first args) (second args))
                                  (apply cc (rest args))
                                  (cons (first args) (apply cc (rest args)))))
              )]
      (apply cc x)
      )
    ))

(defcheck solution-bd2a0a30
  reduce (fn[x y] (if (not (= y (last x))) (conj x y) x)) [])

(defcheck solution-bdbc29ea
  (partial
    #(if (empty? %2) (reverse %1)
                     (recur
                       (if (= (first %2) (first %1)) %1
                                                     (conj %1 (first %2))) (rest %2))) '()))

(defcheck solution-bdc4e827
  (fn compress-
    ;  "30. Write a function which removes consecutive duplicates from a sequence."
    ([coll] (compress- coll nil))
    ([coll last]
     (let [x (first coll)]
       (if (not (empty? coll))
         (if-not (= x last)
           (cons x (compress- (rest coll) x))
           (compress- (rest coll) x)))))))

(defcheck solution-bde21c33
  #(->> (partition-by identity %) (map first)))

(defcheck solution-bde3d38d
  (fn prob30 [col]
    (loop [col col
           acc []]
      (if (empty? col)
        acc
        (recur
          (rest col)
          (if (not (= (last acc) (first col)))
            (conj acc (first col))
            acc
            ))))))

(defcheck solution-be07ac26
  reduce (fn [acc x] (if (= (last acc) x) acc (conj acc x))) [])

(defcheck solution-be08887a
  reduce (fn [r x] (if (= x (last r)) r (conj r x))) [])

(defcheck solution-bea5a77b
  #(cons (first %)
     (map second
       (filter (fn [[a b]] (not (= a b)))
         (map vector % (rest %))))))

(defcheck solution-bf17ec19
  (fn [xs] (filter identity (map-indexed
                              (fn [i e] (if (and (< (inc i) (count xs))
                                                 (= e (nth xs (inc i))))
                                          nil e))
                              xs))))

(defcheck solution-bf3a615b
  (fn [l]
    (reverse
      (reduce
        (fn[x,y] (if (= (first x) y) x (into x (list y))) )
        '()
        (seq l)))))

(defcheck solution-bf98d3ad
  (fn [s]
    (loop [xs s result []]
      (cond (< (count xs) 2) (into result xs)
            (= (first xs) (first (next xs))) (recur (rest xs) result)
            true (recur (rest xs) (conj result (first xs)))))))

(defcheck solution-bfc4b145
  (fn [coll]
    (reduce (fn [r x]
              (if (= (last r) x)
                r
                (conj r x)))
      []
      coll)))

(defcheck solution-c02b2f3c
  reduce (fn [x y] (if (= (last x) y)
                     x
                     (concat x (list y)) )) '())

(defcheck solution-c0466a36
  reduce (fn [kept x] (if (= (last kept) x) kept (conj kept x))) [])

(defcheck solution-c0f3e0cd
  reduce #(if (= (last %) %2) % (conj % %2)) [])

(defcheck solution-c1094b1
  (fn [seq]
    (loop [[hd & tl] seq
           final []]
      (let [next-tl (drop-while (partial = hd) tl)]
        (if (empty? next-tl)
          (conj final hd)
          (recur next-tl (conj final hd)))))))

(defcheck solution-c155c6d
  #(->> % (reduce (fn [a b]
                    (if (= (:prev a) b)
                      a
                      {:prev b, :acc (conj (:acc a) b)})) {:prev nil, :acc []})
     (:acc)))

(defcheck solution-c1641648
  #(loop [x % y []] (if (zero? (count x)) y (recur (rest x) (if (= (first x) (last y)) y (conj y (first x)))))))

(defcheck solution-c16881f3
  (fn [x]
    (reduce #(if (= %2 (last %1))
               %1
               (conj %1 %2)) [] x)))

(defcheck solution-c220431c
  #(map last (partition-by str %)))

(defcheck solution-c25c00c6
  reduce #(if (or (empty? %1) (not= (last %1) %2)) (conj %1 %2) %1) [])

(defcheck solution-c2750bc6
  (fn [x]
    (reduce #(if (not= (last %1) %2)
               (conj %1 %2)
               %1)
      [] x)))

(defcheck solution-c2db7dd
  (fn [coll]
    (map second
      (filter (fn [[index item]] (not= item (get coll (dec index))))
        (map vector (range) coll)))))

(defcheck solution-c2dfafc7
  (fn clean [x]
    (let [r (if (> (count x) 1)
              (if (= (first x) (second x))
                (clean (rest x))
                (cons (first x) (clean (rest x))))
              x)]
      (if (string? x) (apply str r) r))))

(defcheck solution-c2f1d790
  (fn [s] (remove nil? (map #(if (not= %1 %2) %1) s (cons nil s)))))

(defcheck solution-c30860b
  #(map first (partition-by identity %)))

(defcheck solution-c3307d8d
  (fn [seq]
    (loop [accu [] s seq]
      (if (empty? s)
        accu
        (recur (conj accu (first s))
          (drop-while #(= (first s) %) s))))))

(defcheck solution-c35cf859
  (fn [coll]
    (reverse (reduce (fn [a b]
                       (if (= (first a) b)
                         a
                         (cons b a)))
               nil
               coll))))

(defcheck solution-c3622
  (fn f [[x & s]] (if x (cons x (f (drop-while #{x} s))))))

(defcheck solution-c3989af1
  (fn [x]
    (loop [x (seq x) r '()]
      (let [r* (if (= (first x) (first r)) r (conj r (first x)))]
        (if (next x)
          (recur (rest x) r*)
          (reverse r*))))))

(defcheck solution-c492ec69
  (fn [values]
    (loop [values values current nil result []]
      (cond
        (empty? values) result
        (or
         (nil? current)
         (not= current (first values))) (recur (rest values) (first values) (conj result (first values)))
        :else (recur (rest values) current result)))))

(defcheck solution-c4a58382
  #(loop [c % res []]
     (cond
       (empty? c) res
       (= (first c) (last res)) (recur (rest c) res)
       (not= (first c) (last res)) (recur (rest c) (conj res (first c)))
       )))

(defcheck solution-c50bce68
  (fn [input]
    (let [s (seq input)]
      (loop [head (first s)
             tail (rest s)
             accum []]
        (let [accum (if (= head (last accum))
                      accum
                      (conj accum head))]
          (if (empty? tail)
            accum
            (recur (first tail) (rest tail) accum)))))))

(defcheck solution-c54677c5
  (fn z [xs]
    (let [[f & r] xs]
      (if (empty? r)
        xs
        (if (= f (first r))
          (z r)
          (cons f (z r)))))))

(defcheck solution-c5764684
  reduce (fn [r x] (if (= (last r) x) r (conj r x))) [])

(defcheck solution-c58742a8
  (fn [coll]
    (loop [[head & tail] coll
           result []]
      (let [result (if (= head (last result)) result (conj result head))]
        (if (seq tail)
          (recur tail result)
          result)))))

(defcheck solution-c5f0c917
  (fn [input]
    (loop [in input
           last nil
           output '()]
      (if (not= (first in) last)
        (conj output (first in)))
      ;;(str output (first in)))
      (if (empty? in)
        (reverse output)
        (recur
          (rest in)
          (first in)
          (if (not= (first in) last)
            (conj output (first in))
            output))))))

(defcheck solution-c5f7bd56
  #(reduce (fn f [r x] (if (not (= (last r) x)) (conj r x) r)) [] %))

(defcheck solution-c668f3cb
  (fn [xs]
    (reduce #(if-not (= (last %1) %2)
               (conj %1 %2)
               %1)
      []
      xs)))

(defcheck solution-c694ae7f
  (fn [alist]
    (map first (partition-by identity alist))))

(defcheck solution-c6a08669
  (fn [xs]
    (reduce
      #(if (= (last %1) %2)
         %1
         (conj %1 %2))
      [] xs)))

(defcheck solution-c7399acf
  (fn compress [xs]
    (mapcat #(if (= %1 %2) [] [%2]) (cons nil xs) xs)))

(defcheck solution-c7a3011b
  #(reduce (fn [a b] (if (= (last a) b)
                       a
                       (conj a b)))
     (into [[(first %)]] (rest %))))

(defcheck solution-c8a6c393
  (fn [s]
    (conj (map first (remove (partial apply =)
                       (map vector (rest s) s)))
      (first s))))

(defcheck solution-c8e2950c
  (fn compress
    ([src]
     (compress src []))
    ([src dst]
     (if (seq src)
       (if (= (last dst) (first src))
         (recur (rest src) dst)
         (recur (rest src) (conj dst (first src))))
       dst))))

(defcheck solution-c8f28405
  (fn [l]
    (loop [[f & args :as my-l] l
           r '()
           last-of-r nil]
      (if (empty? my-l)
        r
        (if (not= f last-of-r)
          (recur args (concat r (list f)) f)
          (recur args r last-of-r))))))

(defcheck solution-c8f48e8
  (fn [coll]
    (reduce (fn [acc x]
              (if (not= x (last acc))
                (concat acc [x])
                acc))
      [(first coll)]
      coll)))

(defcheck solution-c94b446a
  (fn[s] (loop [r s result []](do #_(println result) (if (= r '()) (seq result) (recur (rest r) (if-not (= (first r) (last result)) (conj result (first r)) result)))))))

(defcheck solution-c9d0b9e5
  (fn compress
    [sequence]
    (reduce (fn
              [c val]
              (if (= (last c) val)
                c
                (conj c val))) [] sequence)))

(defcheck solution-cbf90fd3
  (fn [xs]
    (reduce #(if (= (last %1) %2)
               %1
               (conj %1 %2)) (vector (first xs)) xs)))

(defcheck solution-cc3fb76e
  (fn [s]
    (reduce
      (fn [ss el]
        (if (= (last ss) el)
          ss
          (concat ss (vector el))))
      []
      s)))

(defcheck solution-cc426c62
  (fn [o [f & r]]
    (if (nil? f) o
                 (recur (concat o (list f)) (drop-while (partial = f) r)))) ())

(defcheck solution-ccaba6b2
  (fn compress [xs]
    (loop [xs xs ys []]
      (if (seq xs)
        (if (next xs)
          (if (= (first xs) (second xs))
            (recur (next xs) ys)
            (recur (next xs) (conj ys (first xs))))
          (conj ys (first xs)))
        ys))))

(defcheck solution-cce98b9f
  reduce (fn [acc v]
           (if (= v (last acc))
             acc
             (conj acc v))) [])

(defcheck solution-cd02359
  (fn comp-seq [n]
    (map first (partition-by identity n))))

(defcheck solution-cda9e380
  (fn foo [coll]
    (if (next coll)
      (if (= (first coll) (second coll))
        (foo (next coll))
        (cons (first coll) (foo (next coll))))
      coll)))

(defcheck solution-ce0366d0
  (fn [x]
    (loop [acc (vector) l (seq x)]
      (if (empty? l)
        acc
        (recur
          (if (= (last acc) (first l))
            acc
            (conj acc (first l)))
          (rest l))))))

(defcheck solution-ce070d6a
  #(map first (partition-by str %)))

(defcheck solution-ce8eada9
  (fn
    [seq]
    (loop [seq seq list '()]
      (if (not-empty seq)
        (if (not= (second seq) (first seq))
          (recur (rest seq) (conj list (first seq)))
          (recur (rest seq) list))
        (reverse list)))
    ))

(defcheck solution-cef52595
  ;(fn [s] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] s))
  #(map first (partition-by identity %)))

(defcheck solution-cfa008ec
  (fn [lst]
    (reverse
      (reduce
        (fn [s i] (if (= (first s) i) s (conj s i)))
        '()
        lst))))

(defcheck solution-cfd4a699
  (fn[a-seq]
    (loop [compr [(first a-seq)]
           ss (rest a-seq)
           curr (first a-seq)]
      (if (empty? ss)
        compr
        (if (= curr (first ss))
          (recur compr (rest ss) curr)
          (recur (conj compr (first ss)) (rest ss)
            (first ss)))))))

(defcheck solution-d04be31
  (fn uniq [xs]
    (map first (partition-by identity xs))))

(defcheck solution-d081a34a
  #(reduce (fn [l x]
             (if (= x (last l))
               l
               (concat l (list x))))
     '() %))

(defcheck solution-d092e0ae
  (fn [x] (reverse (reduce #(cond (empty? %) (cons %2 %) (= (first %) %2) % :else (cons %2 %)) (list) x))))

(defcheck solution-d0ec7e7e
  (fn [xs]
    (reduce (fn [rs, x] (if (= (last rs) x) rs (concat rs [x]))) (take 1 xs) (rest xs))))

(defcheck solution-d13c9a07
  (fn [x] (reverse (reduce #(if(= (first %) %2) % (conj % %2)) '() x))))

(defcheck solution-d1cf44e4
  (fn [c]
    (let [col (vec c)]
      (filter (complement nil?) (for [i (range (count col))] (if (not= (col i) (if (= i (dec (count col))) nil (col (inc i)))) (col i)))))))

(defcheck solution-d244577a
  (fn [lst]
    (loop [f nil
           l lst
           r []]
      (if (empty? l) r
                     (recur (first l) (rest l)
                       (if (= (first l) f) r
                                           (conj r (first l))))))))

(defcheck solution-d278e181
  (fn compress [s]
    (map first (partition-by identity s))))

(defcheck solution-d2c025df
  #(reverse
     (reduce (fn [l e]
               (if-not (= (first l) e) (conj l e)
                                       l)) () %)))

(defcheck solution-d33ab818
  ;;(fn rm-consecutive-dups
  ;;  [s]
  ;;  (reverse (loop [c (vec s)
  ;;         result nil]
  ;;    ;(println "c:" c ", result:" result)
  ;;    (if (empty? c)
  ;;      result
  ;;      (recur (rest c)
  ;;             (if (= (first result) (first c))
  ;;               result
  ;;               (conj result (first c))))))))

  #(->> % (partition-by identity) (map first)))

(defcheck solution-d35654af
  #(loop [v % r []] (if-not (seq v) r (let [[x & xs] v] (recur xs (if (= x (first xs)) r (conj r x)))))))

(defcheck solution-d368b4a1
  (fn [l]
    (loop [a l, b []]
      (if a
        (if (not (= (first a) (first (next a))))
          (recur (next a) (conj b (first a)))
          (recur (next a) b))
        b))))

(defcheck solution-d3c64096
  (fn [xs]
    (loop [[x & xs] xs
           prev nil
           res []]
      (let [res (if (= x prev) res (conj res x))]
        (if (empty? xs)
          res
          (recur xs x res))))))

(defcheck solution-d41a6d24
  (fn [xs] (reduce (fn [tr x] (if (= x (last tr)) tr (conj tr x))) [] xs)))

(defcheck solution-d445524f
  (fn [s]
    (map #(first %) (partition-by identity s))))

(defcheck solution-d4d0e3b4
  (fn [s]
    (reverse
      (reduce #(if (not= (first %1) %2)
                 (cons %2 %1) %1)
        () (seq s)))))

(defcheck solution-d51cb3b3
  (fn compr [xs]
    (if (empty? xs)
      ()
      (let [x (first xs)
            ys (compr (rest xs))]
        (if (not= x (first ys))
          (cons x ys)
          ys)))))

(defcheck solution-d550d94e
  reduce #(if (not= (last %) %2) (conj % %2) %) [])

(defcheck solution-d6a47515
  (fn [s]
    (reduce
      (fn [xs x]
        (if (= (last xs) x)
          xs
          (conj xs x)
          )

        ) [] s
      ) ))

(defcheck solution-d6e35dd0
  reduce (fn [acc x]
           (if (= (last acc) x)
             acc
             (conj acc x))) [])

(defcheck solution-d6f251f4
  (fn [x]
    (reverse (reduce (fn [accum y]
                       (if (= (first accum) y)
                         accum
                         (cons y accum))) [] x))
    ))

(defcheck solution-d702b362
  (fn [x] (reverse (reduce #(if (= (first %) %2) % (cons %2 %)) '() x))))

(defcheck solution-d81e9172
  (fn f [input & params]
    (if (nil? params) (f input ()))
    (let [[output] params]
      (cond
        (empty? input) (reverse output)
        (empty? output) (f (rest input) (cons (first input) output ))
        (= (first input) (first output)) (f (rest input) output)
        :default (f (rest input) (cons (first input) output ))))))

(defcheck solution-d85bb8f
  (fn compr [xs]
    (if (or (empty? xs) (empty? (rest xs)))
      xs
      (if (= (first xs) (first (rest xs)))
        (compr (rest xs))
        (cons (first xs) (compr (rest xs)))))))

(defcheck solution-d90222b8
  #(map first
     (partition-by identity %)))

(defcheck solution-d93fa3d0
  #(reverse
     (reduce
       (fn [[a :as acc] x]
         (if (= a x) acc (cons x acc))) '() %)))

(defcheck solution-d94335c9
  (fn [s]
    (loop [s_ s ret '()]
      (cond
        (not s_) (into '() ret)
        (or
         (not ret)
         (not= (first s_) (first ret)))
        (recur (next s_) (conj ret (first s_)))
        :else (recur (next s_) ret)))))

(defcheck solution-dab5edd2
  (fn compress-seq [s]
    (->> (partition-by identity s)
      (map first))))

(defcheck solution-dab98498
  #(reverse(last(reduce (fn [[a l] v] (if (= a v) [a l] [v (cons v l)])) [nil ()] %))))

(defcheck solution-dacfffb5
  (fn [xs]
    (reduce
      (fn [agg now]
        (if (and (not (empty? agg)) (= (last agg) now))
          agg
          (conj agg now)))
      []
      xs
      )))

(defcheck solution-db3a7b32
  (fn [lst]
    (loop [acc []
           r (seq lst)]
      (if (empty? r)
        acc
        (let [f (first r)]
          (recur
            (if (= f (last acc))
              acc
              (conj acc f))
            (rest r)))))))

(defcheck solution-db8bf2e
  reduce (fn [xs c] (if (not= (last xs) c) (conj xs c) xs)) [])

(defcheck solution-dc4b57cc
  (fn [s]
    (loop [rem (seq s), acc (vector), ci nil]
      (cond (empty? rem) (seq acc)
            :else (if (= (first rem) ci)
                    (recur (rest rem) acc ci)
                    (recur (rest rem) (conj acc (first rem)) (first rem)))))))

(defcheck solution-dc76cea5
  (comp
   (partial map first) (partial partition-by identity)))

(defcheck solution-dce9fc57
  #(reduce (fn [res x] (if (= (last res) x) res (conj res x))) [] %))

(defcheck solution-dced5ae
  (fn [coll] (keep (fn [[x1 x2]] (when-not (= x1 x2) x2)) (partition 2 1 (cons nil coll)))))

(defcheck solution-dcf5f23c
  (fn rec [ls]
    (lazy-seq
      (if (empty? ls)
        ()
        (let [f (first ls)
              rs (drop-while #(= f %) ls)]
          (cons f (rec rs)))))))

(defcheck solution-dd0533a5
  (fn [s]
    (letfn
     [(uniq-conj [coll e] (if (= (first coll) e) coll (conj coll e)))]
      (reverse (reduce uniq-conj () (seq s))))))

(defcheck solution-de7fdd86
  #(reverse (reduce (fn [m e] (if (= (peek m) e) m (conj m e))) '() %)))

(defcheck solution-dee2ad7e
  #(keep identity
     (map (fn [a b](if (not= a b) a))
       % (cons nil %))))

(defcheck solution-df45eb77
  (fn [l]
    (reverse (reduce
               #(if (= (first %1) %2) %1 (conj %1 %2))
               '()
               l
               ))
    ))

(defcheck solution-df6ee9c6
  (fn compress [s]
    (cond
      (empty? s) '()
      (= (first s) (second s))
      (compress (rest s))
      :else
      (cons (first s) (compress (rest s))))))

(defcheck solution-dfca004c
  (fn compress
    [s]
    (reduce (fn [v n]
              (if (= (last v) n)
                v
                (conj v n))) [] s)))

(defcheck solution-e061fe50
  #(letfn [(rm-head [x l]
             (cond
               (empty? l) l
               (= x (first l)) (recur x (rest l))
               :else l))
           (worker [x n]
             (if (empty? x)
               n
               (recur (rm-head (first x) (rest x)) (conj n (first x)))))]
     (worker % [])))

(defcheck solution-e0a3b2b9
  (fn [s]
    (reduce
      (fn [agg a]
        (if (not= a (last agg))
          (conj agg a)
          agg
          ))
      [] s)))

(defcheck solution-e0d997b4
  (comp #(map first %) #(partition-by identity %)))

(defcheck solution-e104c51c
  (fn [coll]
    (conj
      (vec
        (map first
          (remove #(apply = %) (partition 2 1 coll))))
      (last coll))))

(defcheck solution-e1af81f4
  (fn [xs]
    (reverse
      (reduce
        (fn [v x]
          (if (or (empty? v)
                  (not= x (first v)))
            (cons x v)
            v)
          ) [] xs))))

(defcheck solution-e1eea550
  (fn doit
    [accum coll]
    (if (empty? coll)
      (if (= (type (first accum)) (type \a))
        (clojure.string/join "" (reverse accum))
        (reverse accum))
      (recur
        (conj accum (first coll))
        (drop-while #(= % (first coll)) (rest coll))))) '())

(defcheck solution-e1ef7148
  #(reduce (fn [acc v] (if (= (peek acc) v) acc (conj acc v))) [(first %)] (rest %)))

(defcheck solution-e22bb0b
  (fn [inseq]
    (reduce
      (fn [prev togo]
        (if (= (last prev) togo)
          prev
          (concat prev [togo])))
      '() inseq)))

(defcheck solution-e26e3145
  (fn [x]
    (reduce #(if (= (last %1) %2) %1 (concat %1 (list %2))) '() x)))

(defcheck solution-e32c480b
  (fn [coll]
    (cons (first coll)
      (mapcat #(if (not= %1 %2) [%2] [])
        coll
        (rest coll)))))

(defcheck solution-e47be79
  #(loop [[head & tail :as coll] %
          acc []]
     (let [[thead & ttail] tail]
       #_(println "head=" head "thead=" thead "ttail=" ttail "acc=" acc)
       (if (nil? head)
         acc
         (if (= head thead)
           (recur (conj ttail head) acc)
           (recur tail (conj acc head))
           )
         )
       )
     ))

(defcheck solution-e4af17af
  #(reduce (fn [s n] (if (= (last s) n) s (conj s n))) [] %))

(defcheck solution-e4c107f5
  #(remove nil? (map (fn [x y] (if-not (= x y) y)) (concat (drop 1 %) "_") %)))

(defcheck solution-e4e3a9fa
  (fn [xs]
    (if (<= (count xs) 1) (seq xs)
                          (loop [xs' [(first xs)] i 1]
                            (if (>= i (count xs))
                              xs'
                              (let [xi (nth xs i)]
                                (if (= xi (last xs'))
                                  (recur xs' (inc i))
                                  (recur (conj xs' xi) (inc i)))))))))

(defcheck solution-e5f77110
  (fn [s]
    (loop [s s r [] p nil]
      (let [n (next s) f (first s)]
        (if f
          (if (= p f)
            (recur n r p)
            (recur n (conj r f) f))
          r)
        )
      )))

(defcheck solution-e6383bc2
  (fn f [s] (let [c (first s) r (drop-while #{c} (rest s))] (cons c (if-not (empty? r) (f r))))))

(defcheck solution-e6610c60
  (fn mycs [x]
    (loop [n [] p x]
      (if (nil? p)
        n
        (if (= (last n) (first p))
          (recur n (next p))
          (recur (conj n (first p)) (next p)))))))

(defcheck solution-e69623af
  (fn [sequence]
    (reduce (fn [acc elem]
              (if (= elem (last acc))
                acc
                (conj acc elem)))
      []
      sequence)))

(defcheck solution-e69c1ecf
  #(reduce (fn [a x] (if (= (last a) x) a (conj a x))) [] %))

(defcheck solution-e6b0f84a
  ;(fn [s] (loop [cur s, ret (), last nil] (if-let [h (first cur)] (recur (rest cur) (if (= h last) ret (concat ret [h])) h) ret)))
  (fn [s] (->> s (partition-by identity)
            (map first))))

(defcheck solution-e6de1a3c
  (fn [x] (->> (map list x (rest x))
            (remove #(= (first %1) (second %1)))
            (map second)
            (cons (first x)))))

(defcheck solution-e7096ca7
  ;(fn [s]
  ;  (loop [tmp (first s) tail (rest s) acc ()]
  ;    (cond (nil? tmp) (reverse acc)
  ;          (= tmp (first tail)) (recur tmp (rest tail) acc)
  ;          :else (recur (first tail) (rest tail) (conj acc tmp))
  ;      )))

  ;reduce #(if (= %2 (last %)) % (conj % %2)) []

  #(map first (partition-by identity %)))

(defcheck solution-e7761ea5
  #(cons (first %) (filter (fn[x] (not (nil? x))) (map (fn[[a b]] (if (= a b) nil b)) (partition 2 (interleave % (rest %)))))))

(defcheck solution-e781992b
  (fn [coll]
    ((fn [last coll accu]
       (cond
         (empty? coll) (reverse accu)
         (= last (first coll)) (recur last (rest coll) accu)
         true (recur (first coll) (rest coll) (cons (first coll) accu))
         )
       ) nil coll nil)
    ))

(defcheck solution-e83f5017
  #(loop [l %1 acc []] (if (empty? l) acc (let [f (first l) ]  (recur (rest l) (if (= f (last acc)) acc (conj acc f)))))))

(defcheck solution-e8506776
  (fn [x] (map #(first %) (filter #(not= (first %) (second %)) (map vector x (cons :empty x))))))

(defcheck solution-e8ca4e54
  (fn [s]
    (loop [s s
           r ()]
      (cond
        (empty? s) (reverse r)
        (and (not (empty? r))
             (= (first s) (first r))) (recur (rest s) r)
        :else (recur (rest s) (conj r (first s)))))))

(defcheck solution-e9183042
  #(reduce (fn [x y] (if (= y (last x)) x (conj x y))) [] %))

(defcheck solution-e9233c7d
  (fn compress-seq ([coll] (compress-seq coll '())) ([coll rcoll] (if (empty? coll ) (reverse rcoll) (recur (rest coll) (if (= (first coll) (first rcoll)) rcoll (conj rcoll (first coll))))))))

(defcheck solution-e95090ce
  (fn compress[a-seq]
    ((fn do-compress [el rest-seq]
       (if (first rest-seq)
         (let [next-el (first rest-seq) tail-seq (rest rest-seq)]
           (if (= el next-el)
             (do-compress el tail-seq)
             (cons el (do-compress next-el tail-seq))
             )
           )
         (list el)
         )
       ) (first a-seq) (rest a-seq))
    ))

(defcheck solution-e9612a64
  (fn rmd [l] (map first (partition-by identity l))))

(defcheck solution-ea09b0dc
  #(reverse (reduce
              (fn [result val]
                (if (= (first result) val)
                  result
                  (conj result val)))
              '() %)))

(defcheck solution-ea115f68
  (fn comp-seq [mylist]
    (seq (loop [l mylist lastchar nil final []]
           (if (empty? l) final
                          (if (not (= lastchar (first l)))
                            (recur (rest l) (first l) (conj final (first l)))
                            (recur (rest l) (first l) final)))))))

(defcheck solution-ea5f93a6
  (fn [coll]
    (reduce #(if (= (last %1) %2) %1
                                  (concat %1 (list %2))) '() coll)))

(defcheck solution-ea66fe64
  reduce #(if (= (last %) %2)
            %
            (conj % %2)) [])

(defcheck solution-eaf029ec
  (fn dedup
    ([arr]
     (let [l (seq arr)]
       (dedup (first l) (rest l) '())))
    ([prev arr accum]
     (if (empty? arr)
       (reverse (cons prev accum))
       (let [[next & remaining] arr]
         (dedup next remaining (if (= prev next) accum (cons prev accum))))))))

(defcheck solution-ebb8b016
  ;#(reduce (fn [xs x] (if (= (last xs) x) xs (concat xs (list x)))) '() %)

  #(map first (partition-by identity %)))

(defcheck solution-ebe0888a
  (fn [seqs]
    (let [lazy-dist (fn lazy-dist [anchor elems]
                      (cond
                        (empty? elems) nil
                        (= (first elems) anchor) (lazy-dist anchor (rest elems))
                        :else (cons (first elems) (lazy-seq (lazy-dist (first elems) (rest elems))))))]
      (lazy-dist nil seqs))))

(defcheck solution-ec047c1c
  (fn compress [coll]
    (when-let [[f & r] (seq coll)]
      (if (= f (first r))
        (compress r)
        (cons f (compress r))))))

(defcheck solution-ecc08049
  (fn [v]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2))
      [] (seq v))))

(defcheck solution-ece85d30
  (fn compress
    ([in] (compress in []))
    ([in out]
     (let [r (rest in) f (first in) l (last out)]
       (if (empty? in)
         (apply list out)
         (if (= f l) (compress r out) (compress r (conj out f))) )))))

(defcheck solution-ed286fa
  (fn test [x]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] x
      )
    ))

(defcheck solution-ede0f9c2
  #(->> %
     (partition-by identity )
     (map first)
     ))

(defcheck solution-ee0be9bf
  (fn [s]
    (letfn [(cmprs [s curr-item result]
              (if (seq s)
                (let [first-item (first s)
                      new-result (if (= first-item curr-item)
                                   result
                                   (conj result first-item))]
                  (recur (rest s) first-item new-result))
                result))]
      (cmprs s nil []))))

(defcheck solution-ee450d4a
  (fn [x] (reduce #(if (= (last %) %2) % (conj % %2)) []  x)))

(defcheck solution-ee5c6955
  (partial
    (fn [acc prev coll]
      (if (nil? coll)
        (reverse acc)
        (if (= (first coll) prev)
          (recur acc prev (next coll))
          (recur (conj acc (first coll)) (first coll) (next coll))
          )
        )
      ) '() :sentinal))

(defcheck solution-ee98f76c
  (fn [seq]
    (map first
      (filter (partial apply not=)
        (map list seq (concat '(nil) seq))))))

(defcheck solution-ef78f345
  (fn compress [x]
    (letfn [ (c [ a b ]
               (if (empty? a )
                 b
                 (if (= (first a) (last b))
                   (c (rest a) b )
                   (c (rest a) (concat b (vector (first a))) )
                   )))]
      ( c x '()))))

(defcheck solution-efc03c47
  (fn rem-consec-dup [l]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] l)))

(defcheck solution-f013ece2
  (fn [s] (reduce #(if (= (last %1) %2) %1 (concat %1 [%2])) [] s)))

(defcheck solution-f040d9a2
  #(map first (seq (partition-by identity %))))

(defcheck solution-f1071ae0
  (fn [a]
    (let [partitioned
          (->> (partition 2 1 a)
            (remove (partial apply =)))]
      (concat (first partitioned)
              (map second (rest partitioned))))))

(defcheck solution-f12910df
  (fn [l] (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] l)))

(defcheck solution-f1e3b3be
  (fn [s]
    (loop [kept nil
           [a & r] (sequence s)
           out []]
      (cond
        (nil? a) out
        (nil? kept) (recur a r (conj out a))
        (= a kept) (recur kept r out)
        :default (recur a r (conj out a))))))

(defcheck solution-f24176c0
  (fn [s]
    (reduce
      (fn [result item]
        (if (= item (last result))
          result
          (conj result item))) [] s)))

(defcheck solution-f3184dfd
  (fn [s]
    (if (string? s)
      (apply str
        (concat
         (map first
           (filter #(not= (first %) (last %))
             (partition 2 1 s)))
         (list (last s))))
      (concat
       (map first
         (filter #(not= (first %) (last %))
           (partition 2 1 s)))
       (list (last s))))))

(defcheck solution-f3470df3
  (fn f [xs]
    (when (seq xs)
      (if (= (first xs) (second xs))
        (f (rest xs))
        (cons (first xs) (f (rest xs))))

      )))

(defcheck solution-f3f91abb
  (fn remove-duplicates [coll]
    (reduce (fn [acc el]
              (if (= (last acc) el)
                acc
                (concat acc (list el)))) [] coll)))

(defcheck solution-f4245769
  #(map first (filter (fn [[a b]] (not (= a b)))
                (partition 2 1 [(gensym)] %))))

(defcheck solution-f4336408
  reduce #(if (= (last %) %2)
            %
            (conj % %2)) [])

(defcheck solution-f46a7bfb
  (fn [a] (loop [coll a
                 res '()]
            (if (seq coll)
              (if (= (first coll) (first (next coll)))
                (recur (next coll) res)
                (recur (next coll) (conj res (first coll)))
                )
              (reverse res)
              ))))

(defcheck solution-f47443a7
  (fn remove-dups [coll]
    (let [coll (seq coll)]
      (loop [x (first coll)
             y (second coll)
             rest (drop 2 coll)
             result []]
        (if (nil? x)
          result
          (let [eq (= x y)]
            (recur (if eq x y)
              (first rest)
              (drop 1 rest)
              (if (not eq) (conj result x)
                           result))))))))

(defcheck solution-f5209723
  (comp reverse (fn [coll] (reduce #(if (= (first %) %2) % (cons %2 %)) () coll))))

(defcheck solution-f527ae33
  (fn compress [ns]
    (when (seq ns)
      (lazy-seq (cons (first ns) (compress (drop-while #(= (first ns) %) ns)))))))

(defcheck solution-f561b582
  (fn compress [l]
    (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [] l )))

(defcheck solution-f5702d05
  (fn [lst]
    (reduce (fn [res el]
              (if (= el (last res))
                res
                (conj res el)))
      []
      lst)))

(defcheck solution-f57dfba7
  (fn [s]
    (let [parts (partition 2 1 [:c] s)
          r1 (map first (filter #(not= (first %) (second %)) parts))]
      r1)))

(defcheck solution-f5aeebb5
  (fn rem-dup
    [[x & xs]]
    (if x
      (if (= x (first xs)) (rem-dup xs)
                           (cons x (rem-dup xs)))
      [])))

(defcheck solution-f61e342e
  (fn f [x]
    (if-let [y (first x)]
      (cons y (f (drop-while #(= y %) x))))))

(defcheck solution-f6738fae
  (fn f [coll]
    (cond
      (empty? coll) '()
      (= 1 (count coll)) coll
      (= (first coll) (second coll)) (f (rest coll))
      :else (cons (first coll) (f (rest coll))))))

(defcheck solution-f7544046
  (fn dedup
    ([x] (dedup [] (first (seq x)) (seq x)))
    ([acc lastc s] (cond
                     (empty? s) (conj acc lastc)
                     (= (first s) lastc) (dedup acc lastc (next s))
                     :else (dedup (conj acc lastc) (first s) (next s))))))

(defcheck solution-f978f6e
  (fn compress
    ([s] (compress (seq s) nil))
    ([s l]
     (when (not-empty s)
       (if (= (first s) l)
         (compress (rest s) l)
         (cons (first s) (compress (rest s) (first s))))))))

(defcheck solution-f9b2c178
  (fn [coll]
    (map first
      (filter
        #(not= (first %) (second %))
        (concat (partition 2 1 coll) (conj () (conj () (last coll))))))))

(defcheck solution-f9e9e60e
  (fn d [xs](when (seq xs)(lazy-seq (cons (first xs)  (d (drop-while #(= (first xs) %) (rest xs))))))))

(defcheck solution-fa55be6b
  (fn [sq] (reduce
             (fn [v e] (if (= (last v) e) v (conj v e))) [] sq)))

(defcheck solution-fa9c0217
  (fn compress-seq [coll]
    (map first (partition-by identity coll))))

(defcheck solution-fae1d197
  (fn t [coll]
    (reduce #(if (= (peek %1) %2)
               %1
               (conj %1 %2))
      [] coll)))

(defcheck solution-fb017903
  #(cons (first %)
     (mapcat
       (fn [x]
         (when-not (= (first x) (last x))
           (list (last x))))
       (partition 2 1
         %))))

(defcheck solution-fbc870ee
  (fn [l]
    (reduce
      #(if (= %2 (last %1)) %1 (conj %1 %2)) [] l)))

(defcheck solution-fc0d925
  #(loop [result () pre (list (first %1)) col (rest %1)]
     (if-let [n (first col)]
       (if (= n (first pre))
         (recur result pre (rest col))
         (recur (concat result pre) (list n) (rest col)))
       (concat result pre))))

(defcheck solution-fc55259d
  (fn [coll] (if (empty? coll) coll (letfn [(rdup [a xs] (if (empty? xs) nil (if (= a (first xs)) (rdup a (rest xs)) (cons (first xs) (rdup (first xs) (rest xs)))) ))] (cons (first coll) (rdup (first coll) (rest coll)))))))

(defcheck solution-fc6f86aa
  (fn [xs]
    (reverse
      (reduce
        (fn [acc c]
          (if (= c (first acc))
            acc
            (cons c acc)))
        '()
        xs))))

(defcheck solution-fc753840
  #(reduce (fn [acc x] (if-not (= (last acc) x) (conj acc x) acc)) [] % ))

(defcheck solution-fcffbc23
  (fn [c]
    (reduce (fn [a b]
              (if (= b (last a))
                a
                (conj a b)))
      []
      (vec c))))

(defcheck solution-fd0820b8
  (fn [s]
    (let [f (fn dedup [s]
              (if (empty? s)
                s
                (concat [(first s)]
                        (dedup (drop-while (fn [x] (= (first s) x ))
                                 (rest s))))))]
      (f (reverse (into () s))))))

(defcheck solution-fd5356a2
  #(reduce
     (fn duprm [x y]
       (if (= y (last x))
         x
         (concat x (list y)))
       ) '() %))

(defcheck solution-fdb7af44
  (fn [s]
    ((fn [s pc r]
       (if (empty? s)
         r
         (recur (rest s)
           (first s)
           (if (= (first s) pc)
             r
             (conj r (first s))))))
     s [] [])))

(defcheck solution-fe0ac0ce
  (fn [l]
    (loop [x l ret []]
      (if (nil? (seq x))
        ret
        (let [z (conj ret (first x))]
          (if-not (= (first x) (last ret))
            (recur (rest x) z)
            (recur (rest x) ret)))))))

(defcheck solution-fe351259
  (fn cmprs [x]
    (reverse (reduce #(if (= %2 (first %1)) %1 (conj %1 %2)) '() x))
    ))

(defcheck solution-fe94a129
  (fn [x] (map first (partition-by identity x))))

(defcheck solution-fef13156
  #(loop [s % r []]
     (cond
       (empty? s) r
       (= (first s) (last r)) (recur (rest s) r)
       :else (recur (rest s) (conj r (first s))))))

(defcheck solution-ff129b69
  #_(fn com
      ([ls] (com (rest ls) [(first ls)]))
      ([ls c]
       (cond
         (empty? ls) c
         (= (last c) (first ls))
         (recur (rest ls) c)
         :else (recur (rest ls) (conj c (first ls))))))

  reduce #(if (not= (last %) %2)
            (conj % %2) %) [])

(defcheck solution-ff1ec292
  (fn [xs]
    (reverse
      (loop [ys xs ret '()]
        (if (empty? ys)
          ret
          (let [fir (first ys) b (= fir (first ret))]
            (recur (rest ys) (if b ret (cons fir ret)))))))))

(defcheck solution-ffee97f
  #(reduce (fn [xs y] (if (= (last xs) y) xs (conj xs y))) [] %))