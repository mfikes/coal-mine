(ns coal-mine.problem-55
  (:require [coal-mine.checks :refer [defcheck-55] :rename {defcheck-55 defcheck}]
            [clojure.test]))

(defcheck solution-10b9f1ab
  (comp
   (partial apply hash-map)
   (partial mapcat (juxt first (comp count second)))
   (partial group-by identity)))

(defcheck solution-10caaad0
  #(zipmap (distinct %)
     (map count (vals (group-by identity %)))))

(defcheck solution-10eedbb8
  reduce #(assoc %1 %2 (if-let [cnt (%1 %2)] (inc cnt) 1)) {})

(defcheck solution-1172adc8
  (fn [s]
    (->> s
      (group-by identity)
      (map #(vector (% 0) (count (% 1))))
      (into {}))))

(defcheck solution-12ddd4a7
  (fn [s] (loop [s s, m {}]
            (if (seq s)
              (if-let [freq (get m (first s))]
                (recur (rest s) (assoc m (first s) (inc freq)))
                (recur (rest s) (assoc m (first s) 1)))
              m))))

(defcheck solution-12e6dd3b
  (fn [c] (->> c (group-by identity) (map #(vector (first %) (count (second  %)))) (into {}) ) ))

(defcheck solution-1365adee
  (fn [coll]
    (let [grouped (group-by identity coll)
          ll (map (fn [[a b]] [a (count b)]) (seq grouped))
          ret (into (array-map) ll)]
      ret)))

(defcheck solution-1382ba9a
  (fn [xs] (reduce-kv #(assoc % %2 (count %3)) {} (group-by identity xs))))

(defcheck solution-141442b0
  (fn cnt [s]
    (reduce (fn [m [k v]]
              (assoc m k (count v)))
      {}
      (group-by identity s))))

(defcheck solution-14193324
  (fn count-occur [lst]
    (let [inc-key (fn [map key]

                    (assoc map key
                               (inc (get map key 0)))) ]

      (reduce inc-key {} lst))))

(defcheck solution-143ce22f
  (fn [xs]
    (into {} (map (fn [[k v]] { k (count v) })
               (group-by identity xs)))))

(defcheck solution-145eb2ec
  (fn [m] (into {} (map (fn [v] (vector (first v) (count (second v)))) (group-by identity m)))))

(defcheck solution-14f008c6
  (fn [xs]
    (reduce (fn [acc item] (assoc acc item (inc (acc item 0))))
      (lazy-cat '({}) xs))))

(defcheck solution-15304bce
  (fn [s]
    (->> s
      (sort ,,)
      (partition-by identity ,,)
      (mapcat #(vector (first %) (count %)) ,,)
      (apply hash-map ,,))))

(defcheck solution-157af33a
  (fn [l]
    (apply merge
      (let [ks (distinct l)]
        (for [k ks]
          {k (count (filter #(= k %) l))})))))

(defcheck solution-15ad7d0d
  (fn [l]
    (loop [e (first l) r (next l) ac {}]
      (if r
        (recur (first r) (next r)
          (assoc ac e (count (filter (partial = e) l))))
        ac))))

(defcheck solution-15c656e2
  #(into {} (map (fn [[k v]]
                   [k (count v)])
              (group-by identity %))))

(defcheck solution-1665dfb3
  (fn [coll]
    (let [groups (group-by identity coll)]
      (into {} (for [[k v] groups] [k (count v)])))))

(defcheck solution-16720afd
  (fn my-frequencies
    [lst]
    (into {} (map (fn [[key val]] [key (count val)]) (group-by identity lst)))))

(defcheck solution-16905ecb
  reduce #(assoc %1 %2 (inc (%1 %2 0))) {})

(defcheck solution-16dae229
  (fn [coll]
    (let [f (fn [m k]
              (update-in m [k] #(if (nil? %) 1 (inc %))))]
      (reduce f {} coll))))

(defcheck solution-1701f0db
  (fn [l] (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} l)))

(defcheck solution-170e525e
  (fn
    [lista]
    (let [m (group-by (fn [x] x) lista)]
      (apply merge (for [i (keys m)] {i (count (m i))})))))

(defcheck solution-173d339c
  (fn [s]
    (->> s
      sort
      (partition-by identity)
      (map #(vector (first %) (count %)))
      (into {}))))

(defcheck solution-1746052e
  #(reduce (fn [f v] (assoc f v (inc (f v 0)))) {} %))

(defcheck solution-1883a005
  reduce #(if-not (%1 %2) (assoc %1 %2 1) (assoc %1 %2 (inc (%1 %2)))) {})

(defcheck solution-18aaf1cd
  (fn [s] (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} s)))

(defcheck solution-18d1776e
  (fn [col]
    (apply merge-with + (map (fn [e] (hash-map e 1)) col))))

(defcheck solution-18e80f13
  #(reduce (fn [h x] (assoc h x (inc (get h x 0)))) {} %))

(defcheck solution-192c5b0a
  (fn [x]
    (apply hash-map (mapcat #(list (first %) (count %)) (vals (group-by identity x))))))

(defcheck solution-194f7f2f
  (fn [v]
    (let [m (map (fn [x] {x 1}) v)]
      (apply merge-with + m))))

(defcheck solution-1a1f7e7c
  reduce #(assoc % %2 (+ (% %2 0) 1)) {})

(defcheck solution-1a31fac5
  (fn [alist]
    (apply merge (let [aset (set alist)]
                   (for [x aset]
                     {x (count (filter #(= x %) alist))})))))

(defcheck solution-1aa4711c
  (fn [xs]
    (let [grouped (group-by identity xs)]
      (zipmap (keys grouped)
        (map count (vals grouped))))))

(defcheck solution-1ab76c44
  (fn [coll]
    (reduce #(update-in %1 [%2] (fnil inc 0)) {} coll)))

(defcheck solution-1ae75d92
  (fn f
    ([s] (f s {}))
    ([s m]
     (if (empty? s)
       m
       (f (rest s) (assoc m (first s) (inc (get m (first s) 0))))))))

(defcheck solution-1b1022d9
  (fn [xs]
    (reduce #(assoc %1 (key %2) (count (val %2))) {} (group-by identity xs))))

(defcheck solution-1bb2c35
  (fn [seq]
    (reduce
      #(assoc %1 (first %2) (count (last %2)))
      {} (group-by identity seq))))

(defcheck solution-1bb3d2a1
  #(reduce (fn [t v] (assoc t (first v) (count (second v)))) {} (group-by identity %)))

(defcheck solution-1bfb542f
  (fn [x]
    (let [k (distinct x) ]
      (zipmap k (map (fn[z](count (filter #(= z %) x ))) k)))))

(defcheck solution-1c427391
  #(let [m (group-by identity %)] (zipmap (keys m) (map (fn [[k v]] (count v)) m))))

(defcheck solution-1c73dcf4
  (fn [coll]
    (->> coll
      (group-by identity)
      (mapcat (juxt first (comp count second)))
      (apply hash-map))))

(defcheck solution-1c82eb75
  (fn frequencies_ [s]
    (apply hash-map
      (mapcat #(vec [(first %) (count (second %))])
        (group-by identity s)))))

(defcheck solution-1cca76eb
  (fn counter [s]
    (reduce (fn updater
              ([m v] (conj m {v (inc (if (nil? (m v))
                                       0
                                       (m v)))})))
      {} s)))

(defcheck solution-1d2333c7
  (fn freq [ns]
    (reduce #(merge-with + %1 {%2 1}) {} ns)))

(defcheck solution-1d2ffcca
  (fn [s]
    (apply merge-with + (map #(assoc nil % 1) s))))

(defcheck solution-1d4c6e3b
  reduce (fn [m e] (assoc m e (inc (m e 0)))) {})

(defcheck solution-1de61076
  #(into {}
     (map (fn [[k v]] [k (count v)])
       (group-by identity %))))

(defcheck solution-1f9832bf
  (fn [l]
    (reduce
      (fn [a v]
        (assoc a v (+ 1 (get a v 0)))
        )
      {}
      l
      )))

(defcheck solution-2027d165
  (fn [coll]
    ((comp (partial apply hash-map) (partial mapcat identity))
     (map #(vector (first %) (count (second %)))
       (group-by identity coll)))))

(defcheck solution-20439df2
  #(reduce
     (fn [r k] (assoc r k (inc (r k 0))))
     {}
     %))

(defcheck solution-205f2848
  (fn my-frequencies
    [s]
    (into {} (map (fn [[k v]] [k (count v)])
               (group-by identity s)))))

(defcheck solution-208459ea
  (fn [x]
    (apply conj {} (map #(vector (first %) (count (second %))) (group-by identity x)))))

(defcheck solution-2092df5f
  (fn [x] (reduce (fn [z y] (merge-with + y z)) (map #(hash-map % 1) x))))

(defcheck solution-211c1eda
  (fn [seq]
    (let [s (set seq) v (for [e s] [e (count (filter #(= e %) seq))])]
      (apply hash-map (reduce concat  v))
      )
    ))

(defcheck solution-2125245a
  (fn count-occ [s]
    (if (empty? s) {}
                   (merge-with + {(first s) 1} (count-occ (rest s))))))

(defcheck solution-2153167f
  (fn [xs] (into {} (map
                      #(vector (first %) (count (second %)))
                      (group-by identity xs)
                      ))))

(defcheck solution-215b75c6
  (fn freqs
    ([xs] (freqs xs {}))
    ([[x & xs :as lst] result]
     (if (empty? lst)
       result
       (recur xs (assoc result x (inc (get result x 0))))))))

(defcheck solution-21648fa9
  (fn [x] (reduce-kv #(assoc % %2 (count %3)) {} (group-by identity x))))

(defcheck solution-220194ea
  reduce #(assoc %  %2 (+ 1 (% %2 0))) {})

(defcheck solution-23119a47
  (fn [col]
    (loop [x col
           r {}]
      (if (nil? (first x))
        r
        (if (nil? (get r (first x)))
          (recur (rest x) (assoc r (first x) 1))
          (recur (rest x) (assoc r (first x) (inc (get r (first x))))))))))

(defcheck solution-231496f5
  (fn [s] (apply merge-with + (map #(hash-map % 1) s))))

(defcheck solution-2344e027
  (fn count-occurrences [coll]
    (let [m (group-by identity coll)]
      (zipmap (keys m) (map count (vals m))))))

(defcheck solution-245e83c
  (fn [coll]
    (reduce #(update-in % [%2] (fnil inc 0)) {} coll)))

(defcheck solution-249618e9
  (fn [coll]
    (reduce (fn [r v]
              (conj r [v (count (filter #(= % v) coll))]))
      {}
      (reduce #(conj %1 %2) #{} coll))))

(defcheck solution-259cf1fa
  #(into {}
     (map (fn [[k v]] [k (count v)]) (group-by identity %))))

(defcheck solution-26c33004
  (fn makemap [l]
    (if (empty? l) {}
                   (let [rm (makemap (rest l)) fl (first l)]
                     (if (contains? rm fl)
                       (update-in rm [fl] inc)
                       (assoc rm fl 1)
                       )))))

(defcheck solution-26f2e6c8
  (fn [xs] (apply zipmap (let [ys (partition-by identity (sort xs))]  [(map first ys) (map count ys)]))))

(defcheck solution-276c061b
  (fn [coll]
    (reduce (fn [counts x]
              (assoc counts x (inc (get counts x 0))))
      {}
      coll)))

(defcheck solution-279418b
  #(reduce
     (fn [a [k v]]
       (assoc a k (count v))) {}
     (group-by identity %)))

(defcheck solution-28b5bb43
  #(apply hash-map(mapcat (fn [[k,v]] [k, (count v)]) (group-by identity %))))

(defcheck solution-2905fe77
  #(into {} (map (fn [[_ [v :as vs]]] [v (count vs)]) (group-by identity %))))

(defcheck solution-293c775b
  #(->> %
     (reduce (fn [a b]
               (merge-with + a {b 1}))
       {})))

(defcheck solution-299f5c0c
  (fn [s] (reduce (fn [m k] (assoc m k (inc (m k 0)))) {} s)))

(defcheck solution-29a483c5
  (fn [v]
    (#(zipmap (keys %)
        (map count (vals %)))
     (group-by identity v))))

(defcheck solution-2a1de13a
  (fn [v]
    (into {} (map (fn [item] (vector item (count (filter #(= % item) v))))
               (set v)))))

(defcheck solution-2a22e538
  #(into {} (for [[x y] (group-by identity %)]
              [x (count y)])))

(defcheck solution-2a665420
  (fn [xs]
    (->> xs
      (group-by identity)
      (mapcat #(vec [(first %) (count (second %))]))
      (apply hash-map))))

(defcheck solution-2a93872a
  (fn f [[h & t]]
    (if h
      (let [m (f t)]
        (assoc m h (+ (get m h 0) 1)))
      {})))

(defcheck solution-2aaf15fd
  #(apply merge-with + (map (fn [v] {v 1}) %)))

(defcheck solution-2ad04d61
  (fn fre [coll]
    (reduce (fn [map k]
              (if (not (contains? map k))
                (assoc map k 1)
                (assoc map k (inc (map k)))))
      {} coll)))

(defcheck solution-2af8c402
  (fn [s]
    ((fn [s r]
       (if (empty? s)
         r
         (recur (rest s)
           (assoc r
             (first s)
             (inc (get r (first s) 0))))))
     s {})))

(defcheck solution-2b6b5a8e
  #(apply merge (for [i (group-by identity %)]
                  {(first i) (count (last i))})))

(defcheck solution-2badc24d
  (fn [items]
    (loop [items items result {}]
      (if (empty? items)
        result
        (let [key (first items)]
          (recur (rest items)
            (if (contains? result key)
              (assoc result key (inc (result key)))
              (assoc result key 1))))))))

(defcheck solution-2bb99917
  (fn [col]
    (let [lfreq  (fn [col res]
                   (if (empty? col) res
                                    (recur (rest col) (assoc res (first col) (inc (get res (first col) 0))))))]
      (lfreq col {}))
    ))

(defcheck solution-2c072853
  reduce #(merge-with + {%2 1} %1) {})

(defcheck solution-2c72ab5
  (fn [c]
    (into {}
      (map
        #(vector (first %) (count (second %)))
        (group-by identity c)))))

(defcheck solution-2cdae556
  (fn cust-freq [coll]
    (reduce
      (fn [m n]
        (assoc m n (inc (get m n 0))))
      {}
      coll)))

(defcheck solution-2cdef9c0
  (fn count-occurences
    [s]
    (apply (partial merge-with +) (map #(hash-map % 1) s))))

(defcheck solution-2d8ac397
  (fn [l] (reduce #(assoc % (first %2) (count (second %2))) {} (group-by identity l))))

(defcheck solution-2da6b97f
  (fn [coll]
    (apply merge-with + (for [e coll] {e 1}))))

(defcheck solution-2df0ed3e
  (fn [lst] (reduce #(update-in % [%2] (fnil inc 0)) {}  lst)))

(defcheck solution-2e2ef546
  (fn [coll]
    (reduce #(assoc % %2 (inc (get % %2 0))) {} coll)))

(defcheck solution-2e5895f7
  (fn f [x]
    (apply merge (map #(hash-map (first % ) (count %)) (partition-by identity (sort x ))))
    ))

(defcheck solution-2f0ab4d1
  #(reduce (fn [res [k v]] (assoc res k (count v)))
     {}
     (group-by identity %)))

(defcheck solution-2f0ed38a
  reduce #(assoc % %2 ((fnil inc 0) (% %2))) {})

(defcheck solution-2f1be707
  (fn [s]
    (->> (map #(hash-map % 1) s)
      (apply merge-with +))))

(defcheck solution-2f5d76ab
  #(let [m (group-by identity %)] (zipmap (keys m) (map count (vals m)))))

(defcheck solution-2f97bd9f
  (fn count-occurence
    [sq]
    (loop [s sq r {}]
      (if (empty? s)
        r
        (recur (rest s) (if (nil? (get r (first s)))
                          (assoc r (first s) 1)
                          (update-in r [(first s)] inc)))))))

(defcheck solution-2fd9d78d
  #(apply conj {}
     (for [[k v] (group-by identity %)]
       [k (count v)])))

(defcheck solution-3006611d
  #(loop [[x & xs :as L] %, result {}]
     (if (empty? L )
       result
       (recur  xs (assoc result x (inc (result x 0)))))))

(defcheck solution-30656712
  (fn [n] (into {} (map (fn [e] [(e 0) (count (e 1))]) (group-by (fn [v] v) n)))))

(defcheck solution-31132b70
  #(into {}
     (map (fn [[k v]] [k (count v)])
       (group-by identity %))))

(defcheck solution-31509955
  (fn mycount [s]
    (reduce #(assoc %1 (first %2) (second %2))
      {}
      (map #(list % (count (filter (partial = %) s))) (distinct s)))))

(defcheck solution-31c4b945
  (fn count-occ [s]
    (reduce merge (map #(assoc {} (first %) (count (second %))) (group-by identity s)))))

(defcheck solution-31f24ba6
  #(reduce (fn [c i] (assoc c (key i) (count (val i)))) {} (group-by identity %)))

(defcheck solution-323767fe
  (fn [seq]
    (apply hash-map (reduce concat
                      (map (fn [[k v]] [k (count v)])
                        (group-by #(-> %) seq))))))

(defcheck solution-328bdb54
  reduce #(merge-with + % {%2 1}) {})

(defcheck solution-330b5897
  #(into {} (for [[key value] (group-by identity %)]
              [key (count value)])))

(defcheck solution-33fabc98
  (fn [y]
    (apply hash-map
      (reduce concat
        (map (fn[x] [(first x) (count x)])
          (partition-by identity (sort y)))))))

(defcheck solution-3401bd6b
  (fn [x]
    (into {} (for [[k v] (group-by identity x)] [k (count v)]))))

(defcheck solution-3465aee5
  (partial reduce #(if (%1 %2) (update-in %1 [%2] inc) (assoc %1 %2 1)) {}))

(defcheck solution-35092045
  #(into {} (map (fn [[a b]]{a (count b)}) (group-by identity %))))

(defcheck solution-352d4354
  (fn [xs] (reduce #(assoc %1 %2 (+ 1 (or (%1 %2) 0))) {} xs)))

(defcheck solution-35514730
  (fn [l]
    (->> l
      (set)
      (map
        (fn [o]
          {o (count
               (filter #(= o %) l))}))
      (into {}))))

(defcheck solution-3565d219
  (partial reduce #(assoc %1 %2 (inc (get %1 %2 0))) {}))

(defcheck solution-35b35118
  reduce #(if (contains? % %2) (assoc % %2 (inc (% %2)))
                               (assoc % %2 1)) {})

(defcheck solution-35fe4b5e
  (fn myFrequencies
    [coll]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} coll)))

(defcheck solution-360d149c
  (fn [xs]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} xs)
    ))

(defcheck solution-3655a03a
  (fn[x] (into {}(map (fn[[a b]] {a (count b)}) (group-by identity x)))))

(defcheck solution-369d3649
  #(let [data (group-by identity %)]
     (zipmap (keys data)
       (map count (vals data)))))

(defcheck solution-36aa316f
  (fn [col]
    ((fn [l now-map]
       (if (not (seq l))
         now-map
         (let [first-item (first l)
               new-list (rest l)
               item-val (get now-map first-item)
               new-map-to-merge (if item-val
                                  {first-item (inc item-val)}
                                  {first-item 1})
               new-map (merge now-map new-map-to-merge)
               ]
           (recur new-list new-map)
           )
         )
       ) col {})
    ))

(defcheck solution-371bfcf7
  reduce (fn [hmap item]
           (assoc hmap item (inc (hmap item 0)))) {})

(defcheck solution-372101e3
  (fn occurence-map [coll]
    (->>
      (group-by identity coll)
      (map (fn [[k v]] {k (count v)}))
      (into {}))))

(defcheck solution-37232b8c
  #(reduce (fn [m k]
             (if (get m k)
               (assoc m k (+ (get m k) 1))
               (assoc m k 1)))
     {} %))

(defcheck solution-381802d
  (fn [xs] (let
            [incmap (fn [m k] (assoc m k (inc (get m k 0))))]
             (reduce (fn [a b] (incmap a b)) {} xs)
             )))

(defcheck solution-381b836c
  (fn [s]
    (apply merge-with + (map #(hash-map % 1) s))))

(defcheck solution-389a2393
  reduce #(assoc %1 %2 (if (%1 %2) (inc (%1 %2)) 1)) {})

(defcheck solution-3a0136d2
  #(reduce (fn [m k]
             (assoc m k
                      (if (contains? m k)
                        (inc (m k))
                        1))) {} %))

(defcheck solution-3a1c2209
  (fn [coll]
    (reduce
      (fn [m [k v]] (conj m [k (count v)]))
      {} (group-by identity coll))))

(defcheck solution-3a7f5228
  (fn [s]
    (loop [ss s out {}]
      (if (empty? ss)
        out
        (recur (rest ss) (assoc out (first ss) (inc (get out (first ss) 0))))))))

(defcheck solution-3a8f73ad
  (fn count-occ [col]
    (reduce (fn [x [y z]] (assoc x y (count z))) {} (group-by identity col))))

(defcheck solution-3a9df825
  (fn [c]
    (reduce #(assoc % %2 (inc (% %2 0))) {} c)))

(defcheck solution-3ab4fb49
  (partial reduce (fn [acc x] (update-in acc [x] #(if % (inc %) 1))) {}))

(defcheck solution-3b0705c3
  (fn [xs] (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} xs)))

(defcheck solution-3b31bca3
  reduce (fn [a x] (merge-with + a {x 1})) {})

(defcheck solution-3bf7d7d8
  #((fn [m] (zipmap (keys m) (map count (vals m)))) (group-by identity %)))

(defcheck solution-3c35a903
  #(reduce (fn[m e] (assoc m e (inc (m e 0)))) {} %))

(defcheck solution-3c39a621
  (fn [s] (reduce #( assoc %1 (first %2) (count (second %2))) {} (group-by identity  s))))

(defcheck solution-3c50c886
  (fn f [vv] (into {} (for  [[k v] (group-by identity vv)]
                        [k (count v)]))))

(defcheck solution-3d1f8bb
  (fn [c] (reduce #(assoc % %2 (inc (or (% %2) 0))) {} c)))

(defcheck solution-3d6f4f99
  (fn [coll]
    (loop [occ '{} i 0]
      (if (= i (count coll))
        occ
        (recur
          (if (contains? occ (nth coll i))
            (assoc occ (nth coll i) (inc (get occ (nth coll i))))
            (assoc occ (nth coll i) 1)
            )
          (inc i))
        )
      )
    ))

(defcheck solution-3da0f0a6
  (fn [coll]
    (reduce
      (fn [m sub] (assoc m (first sub) (count sub)))
      {}
      (partition-by identity (sort coll)))))

(defcheck solution-3e6dbd10
  (fn [xs]
    (reduce (fn [m x]
              (assoc m x
                       (if-let [n (m x)] (inc n) 1)))
      {}
      xs)))

(defcheck solution-3ef625b6
  #(reduce (fn [m item] (let [v (get m item 0)] (assoc m item (inc v)))) {} %))

(defcheck solution-3f22bc02
  reduce #(assoc % %2 (+ 1 (% %2 0))) {})

(defcheck solution-3f40c7f6
  (fn [vs]
    (->> vs
      (group-by (set vs))
      (map (fn [[k v]]
             [k (count v)]))
      (into {}))))

(defcheck solution-3fbd52d
  #(let[m (group-by identity %)]
     (zipmap (keys m) (map count (vals m)))))

(defcheck solution-4066b1c6
  (fn [l]
    (reduce (fn [m [k v]] (assoc m k (count v))) {} (group-by identity l))))

(defcheck solution-40c1e5c8
  (fn [xs]
    (reduce (fn [acc elt]
              (assoc acc elt (inc (get acc elt 0))))
      {} xs)))

(defcheck solution-40d3ea33
  (fn [coll]
    (apply hash-map
      (mapcat #(vector (first %)(count (second %)))
        (group-by identity coll)))))

(defcheck solution-40efc359
  (let [remove-all
        (fn removeall [x y]
          (if (empty? x)
            x
            (if (= (first x)
                  y)
              (removeall (rest x) y)
              (cons (first x)
                (removeall (rest x) y)))))]

    (fn countocc [z]
      (if (empty? z)
        z
        (merge (hash-map (first z)
                 (reduce + (map #(if (= (first z) %) 1 0) z)))
          (countocc (remove-all z (first z))))))))

(defcheck solution-4101e9e7
  ;(fn [coll]
  ;  (let [xs (partition-by identity (sort coll))]
  ;    (zipmap (map first xs) (map count xs))))

  (fn [coll]
    (let [gp (group-by identity coll)]
      (zipmap (keys gp) (map (comp count second) gp)))))

(defcheck solution-4156ca0a
  (fn count-ocur
    ([lis] (count-ocur (sort lis) {}))
    ([lis resp]
     (if (= lis [])
       resp
       (if (contains? resp (first lis))
         (count-ocur (rest lis) (assoc resp (first lis) (inc (resp (first lis)))))
         (count-ocur (rest lis) (assoc resp (first lis) 1)))))))

(defcheck solution-41b89ff5
  (fn [s]
    (reduce #(assoc % %2 (+ 1 (% %2 0)))
      {} s)))

(defcheck solution-41b9f2b4
  (fn [col] (let [g (group-by identity col)]
              (reduce (fn [acc v] (conj acc (hash-map (key v) (count (val v))))) {} g)
              )))

(defcheck solution-41d04ba5
  #(->> % (group-by identity)
     (mapcat (fn [v] [(key v)(count (val v))]))
     (apply assoc {})))

(defcheck solution-421e650f
  (fn [s] (into {} (for [[k v] (group-by identity s)] [k (count v)]))))

(defcheck solution-424ac6c3
  (fn occur [a] (letfn [(grupigi[x] (partition-by identity (sort x)))]
                  (reduce into {} (map #(hash-map(first %) (count %)) (grupigi a))))))

(defcheck solution-427986d3
  (fn freq [l]
    (if (empty? l)
      {}
      (let [a (filter #(= % (first l)) l)
            b (filter #(not= % (first l)) l)]
        (assoc (freq b) (first a) (count a))))))

(defcheck solution-42b882d2
  reduce #(assoc % %2 (+ 1 (% %2 0))) {})

(defcheck solution-42bfdfab
  #(into {}
     (for [[k v] (group-by identity %)]
       [k (count v)])))

(defcheck solution-42f828ee
  (fn count-occ [s]
    (loop [s s a {}]
      (if (empty? s)
        a
        (recur (rest s) (assoc a (first s) (inc (get a (first s) 0))))))))

(defcheck solution-430dc805
  (fn [l]
    (let [s (group-by identity l)]
      (merge-with (fn [a b] (count a)) s s)
      )
    ))

(defcheck solution-4367b51f
  (fn[coll]
    (apply merge (for [i (partition-by identity (sort coll))] {(first i) (count i)}))))

(defcheck solution-441fd71a
  (fn [s]
    (apply sorted-map
      (mapcat (fn [x]
                [x (count (filter #(= % x) s))])
        (set s)))))

(defcheck solution-44ed2cb3
  #( into {} (map (fn [[a b]] [a (count b)]) (group-by identity %))))

(defcheck solution-45175e0f
  (fn [xs]
    (let [m (group-by identity xs)]
      (apply assoc {} (for [[k v] (seq m)
                            x [k (count v)]]
                        x)))))

(defcheck solution-45323cd6
  (fn new-freq
    [coll]
    (loop [[x & xs] (partition-by identity (sort coll)) res {}]
      (if x
        (recur xs (assoc res (first x) (count x)))
        res))))

(defcheck solution-457d34d5
  (fn [c] (reduce (fn [a x] (update-in a [x] (fnil inc 0))) {} c)))

(defcheck solution-458b3242
  (fn my-frequencies2
    [xs]
    (let [m (group-by identity xs)]
      ((fn [xs ys] (apply hash-map (mapcat list xs ys))) (keys m) (map count (vals m))))))

(defcheck solution-45aaba2c
  #(reduce (fn [counts el]
             (assoc counts el (inc (get counts el 0))))
     {}
     %))

(defcheck solution-45ccfc07
  (fn [coll]
    (reduce
      (fn [rv k]
        (assoc rv k (inc (get rv k 0))))
      {}
      coll)))

(defcheck solution-45da9d60
  #(reduce conj
     (for [x (set %)]
       {x (count (filter (fn[y](= x y)) %))})))

(defcheck solution-45dc60b0
  (partial reduce (fn [m k] (assoc m k (inc (get m k 0)))) {}))

(defcheck solution-460738a9
  (fn [coll] (apply hash-map (mapcat #(vector % (count (filter #{%} coll))) (set coll)))))

(defcheck solution-462bd783
  (fn [l]
    (loop [m l
           r {}]
      (if (nil? m)
        r
        (recur (next m)
          (let [c (r (first m))]
            (if (nil? c)
              (conj r [(first m) 1])
              (conj r [(first m) (inc c)]))))))))

(defcheck solution-46403485
  (fn c
    ([x] (c x {}))
    ([x s]
     (if (empty? x)
       s
       (c (rest x) (update-in s [(first x)] (fnil inc 0)))))))

(defcheck solution-4708aee2
  (fn freq [coll] (let [go
                        (fn go [coll m]
                          (if(empty? coll)
                            m
                            (let [[f & r] coll]
                              (recur r (assoc m f (inc (get m f 0)))))))]
                    (go coll {}))))

(defcheck solution-470cf05e
  #(into {} (for [[k v] (group-by identity %)] [k (count v)])))

(defcheck solution-4723d5c3
  #(reduce
     (fn [acc v]
       (update-in acc [v] (fnil inc 0)))
     {} %))

(defcheck solution-47e15236
  (fn [s]
    (let [d (distinct s)
          f (fn [x] (filter #(= x %) s))
          counts (map count (map f d))]
      (zipmap d counts))))

(defcheck solution-4894560
  (fn [s] (let [g (group-by identity s)]
            (reduce
              #(update-in %1 [%2] count) g (keys g)))))

(defcheck solution-48ff62e9
  #(reduce (fn [h v] (update-in h [v] (fnil inc 0))) {} %))

(defcheck solution-495252c2
  (fn [xs]
    (reduce (fn [m e]
              (if (m e)
                (assoc m e (+ (m e) 1))
                (assoc m e 1)))
      {} xs)))

(defcheck solution-4978db96
  (fn [xs] (reduce (fn [m b] (assoc m b (inc (get m b 0)))) {} xs)))

(defcheck solution-49d376d0
  (fn [xs]
    (apply hash-map (mapcat #(vector (first %) (count (second %))) (group-by identity xs)))))

(defcheck solution-4a258399
  #(apply merge (for [[k v] (group-by identity %)] {k (count v)})))

(defcheck solution-4ab1b4d9
  #(reduce (fn [count-map item]
             (assoc count-map item (inc (get count-map item 0)))) {} %))

(defcheck solution-4b18ddea
  (fn [s]
    (let [d (distinct s)]
      (zipmap d (map #(count (filter (partial = %) s)) d)))))

(defcheck solution-4ba816d2
  (fn [xs]
    (apply merge-with +
      (map #(hash-map %1 1) xs)
      )))

(defcheck solution-4bbc4be9
  #(reduce (fn [a b] (assoc a b (+ 1 (get a b 0)))) {} %))

(defcheck solution-4be7091f
  (fn [coll]
    (let [group-map (group-by identity coll)]
      (into {} (for [[k v] group-map] [k (count v)])))))

(defcheck solution-4c039d16
  (fn freq [xs]
    (let [k (distinct xs)]
      (apply hash-map
        (interleave
          k
          (map
            (fn count-vals-of [x] (count (filter #(= % x) xs)))
            k))))))

(defcheck solution-4c8aec13
  reduce (fn[m e] (assoc m e (inc (m e 0)))) {})

(defcheck solution-4c9b7480
  reduce #(if (%1 %2) (assoc %1 %2 (inc (%1 %2))) (assoc %1 %2 1)) {})

(defcheck solution-4ca5b57d
  #(reduce (fn [acc x] (update-in acc [x] (fnil inc 0)))
     {}
     %))

(defcheck solution-4caa00e7
  (partial
    reduce
    (fn [m, x]
      (if (contains? m x)
        (assoc m x (+ 1 (get m x)))
        (assoc m x 1)))
    {}))

(defcheck solution-4cc9cb61
  (fn [coll]
    (reduce
      (fn [m v]
        (let [count (if (contains? m v)
                      (inc (get m v))
                      1)]
          (assoc m v count)))
      {}
      coll)))

(defcheck solution-4cf5e22c
  #(apply merge-with + (for [x %] {x 1})))

(defcheck solution-4d56375c
  (fn [s]
    (apply hash-map (mapcat #(list (first %) (count %)) (partition-by identity (sort s))))))

(defcheck solution-4d78ebba
  (fn [xs]
    (reduce (fn [m x] (let [v (m x) fq (if (nil? v) 0 v)] (assoc m x (+ fq 1)) )) {} xs)))

(defcheck solution-4d7d886b
  #(let [s (partition-by identity (sort %))]
     (zipmap (map first s) (map count s))))

(defcheck solution-4d8a68a7
  (fn [l]
    (loop [r {} t l]
      (if (empty? t)
        r
        (recur (update-in r [(first t)] #(if (nil? %) 1 (inc %))) (rest t))))))

(defcheck solution-4e7d9bb7
  (fn occurrences [seq]
    (loop [results {} items seq]
      (if (= (count items) 0)
        results
        (let [k (first items) value (get results k 0)]
          (recur (assoc results k (inc value)) (rest items))
          )
        )
      )
    ))

(defcheck solution-4eafb12
  (fn map-frequencies
    [xs]
    (reduce (fn [m i] (assoc m i (inc (m i 0)))) {} xs)))

(defcheck solution-4eb211fb
  (fn f [xs]

    (let [ks (apply sorted-set xs)]

      (into {} (for [i ks] [i (count (filter #{i} xs))] ))

      )

    ))

(defcheck solution-4ecff4a9
  (fn f [s]
    (if
     (empty? s)
      {}
      (let [[a & b] s]
        (merge-with + {a 1} (f b))))))

(defcheck solution-4edd504a
  (fn [coll]
    (reduce (fn [m x] (assoc m x (inc (m x 0)))) {} coll)))

(defcheck solution-4effc8d9
  (fn [l] (reduce #(assoc % %2 (inc (% %2 0))) {} l)))

(defcheck solution-4f905c12
  #(into {} (map (fn[[x y]] [x (count y)]) (group-by identity %))))

(defcheck solution-4fb44f42
  (fn [xs] (into {} (map #(vector (first %) (count (second %))) (group-by identity xs)))))

(defcheck solution-4ffc8f68
  (fn [s]
    (reduce
      #(assoc % %2 (inc (% %2 0)))
      {}
      s)))

(defcheck solution-50b37d6
  (fn [col] (reduce #(assoc %1 %2 (inc (or (%1 %2) 0))) {} col)))

(defcheck solution-5169f110
  (fn f [l]
    (if (empty? l)
      {}
      (assoc (f (filter #(not= % (first l)) l))
        (first l)
        (apply + (map #(if (= % (first l)) 1 0) l))))))

(defcheck solution-51966f2a
  (fn my-frequencies [coll]
    (apply array-map
      (mapcat (fn [[k v]] [k (count v)])
        (group-by identity coll)))))

(defcheck solution-5246ef96
  (fn [coll]
    (apply merge
      (map #(hash-map (first %) (count %))
        (->> coll sort (partition-by identity))))))

(defcheck solution-528188f9
  (fn [c]
    (reduce #(update-in %
               [%2]
               (fnil inc 0))
      {}
      c)))

(defcheck solution-52af2284
  #(->> % (group-by identity) (map (fn [[k v]] [k (count v)])) (into {})))

(defcheck solution-52b21c68
  (fn [items]
    (let
     [grouped (group-by identity items)]
      (reduce #(assoc % %2 (count (get grouped %2)) ) {} (keys grouped)))))

(defcheck solution-53558b40
  (fn freq [s]
    (->> (group-by identity s)
      (map #(vector (first %) (count (second %))))
      (into {}))))

(defcheck solution-535596bd
  (fn [coll]
    (let [a (group-by identity coll)]
      (zipmap (keys a) (map #(count %)(vals a))))))

(defcheck solution-53611146
  (fn [xs]
    (reduce #(update-in %1 [%2] (fn [x]
                                  ((fnil inc 0) x)))
      {} xs)))

(defcheck solution-53903a6
  (fn my-frequencies [coll]
    (let [parts (group-by identity coll)
          transform (fn [[key part]] (sorted-map key (count part)))]
      (apply merge-with + (map transform parts)))))

(defcheck solution-53e5032
  (fn [s]
    (reduce (fn [m e] (assoc m e (inc (get m e 0)))) {} s)))

(defcheck solution-5447597b
  (fn foo [x]
    (reduce #(merge-with + %1 {%2 1}) {} x)))

(defcheck solution-55328853
  #((fn f [[a & b] r] (if (= a nil) r
                                    (f b (assoc r a
                                                  (+ (get r a 0) 1))))) % {}))

(defcheck solution-5565e814
  (fn [xs]
    (into {}
      (for [[x c]  (group-by identity xs)]
        [x (count c)]))))

(defcheck solution-55787d84
  #(reduce
     (fn [init [k v]] (assoc init k (count v)))
     {}
     (group-by identity %)))

(defcheck solution-55dff4e7
  (fn [coll] (reduce (fn [acc it]
                       (assoc acc it (inc (or (acc it) 0)))) {} coll)))

(defcheck solution-56dec7c9
  (fn [l]
    ( reduce #( assoc % %2 (inc (get % %2 0)))
      {} l)))

(defcheck solution-56e76bce
  (fn [seq]
    (reduce (fn [m x]
              (assoc m x (inc (get m x 0))))
      {}
      seq)))

(defcheck solution-5759b38e
  (fn ! [s]
    (reduce #(assoc %1
               %2
               (inc (let [v (get %1 %2)]
                      (if (nil? v) 0 v)))
               )
      {}
      s)))

(defcheck solution-5770eb39
  (fn [s]
    (reduce conj {} (map #(vector (% 0) (count (% 1))) (group-by identity s)))))

(defcheck solution-57cd1f72
  (fn [coll]
    (loop [x (distinct coll) res {}]
      (if (empty? x) res
                     (recur (rest x) (assoc res (first x) (count (filter #(= % (first x)) coll))))))))

(defcheck solution-57ead645
  (fn [coll]
    (reduce
      (fn [output item]
        (assoc output item (inc (get output item 0))))
      {}
      coll)))

(defcheck solution-58411372
  (fn [m]
    (reduce
      (fn [t v]
        (update-in t [v] #(if (nil? %) 1 (inc %)))) {} m)))

(defcheck solution-5880b18d
  (fn [rnge]
    ((fn [last rslt remainder]
       (if (empty? remainder)
         rslt
         (recur (first remainder)
           (assoc rslt (first remainder) (inc (get rslt (first remainder) 0)))
           (rest remainder))
         )
       )
     (first rnge) {(first rnge) 1} (rest rnge)
     )
    ))

(defcheck solution-5900f38d
  (fn [v] (into {} (map (fn [[k v]] [k (count v)]) (group-by identity v)))))

(defcheck solution-59e2b9d
  #(apply merge-with + (map (fn [x] (hash-map x 1)) %)))

(defcheck solution-5a656f28
  (fn [s]
    (loop [s (sort s)
           result {}]
      (if (empty? s)
        result
        (let [f (first s)
              sub (take-while #(= f %) s)]
          (recur (drop (count sub) s) (conj result {f (count sub)})))))))

(defcheck solution-5c0d85c5
  (fn [xs]
    (when (seq xs)
      (reduce #(merge-with + %1 {%2 1}) {} xs))))

(defcheck solution-5c1fb375
  (fn [s]
    (->> s
      sort
      (partition-by identity)
      (reduce #(assoc % (first %2) (count %2)) {}))))

(defcheck solution-5caacf37
  #(into {} (map (fn [[x y]] [x (count y)]) (group-by identity %))))

(defcheck solution-5cb01f95
  (fn [s] (reduce #(merge-with + % {%2 1}) {} s)))

(defcheck solution-5cb64fd6
  (fn [coll]
    (reduce
      (fn [acc x]
        (merge-with + acc {x 1}))
      {}
      coll)))

(defcheck solution-5d2fa110
  (fn [input]
    (loop [l input freqs {}]
      (if (empty? l)
        freqs
        (recur (rest l)
          (conj freqs (if (freqs (first l))
                        [(first l) (inc (freqs (first l)))]
                        [(first l) 1])))))))

(defcheck solution-5d42f436
  ; much better: (into {} (map (fn [[k vs]] [k (count vs)]) (group-by identity coll)))
  (fn [coll] (apply array-map
               (mapcat #(list %2 (count %1))
                 (partition-by identity (sort coll))
                 (sort (set coll))))))

(defcheck solution-5d4b3f48
  reduce (fn [a b] (assoc a b (inc (a b 0)))) {})

(defcheck solution-5d54d5eb
  reduce #(assoc % %2 (inc (get % %2 0)) ) {})

(defcheck solution-5da8d533
  (fn [s]
    (reduce
      (fn [ans x] (assoc ans x (inc (get ans x 0))))
      {}
      s
      )
    ))

(defcheck solution-5dbccd6d
  reduce #(if-let [old (%1 %2)] (assoc %1 %2 (inc old)) (assoc %1 %2 1)) {})

(defcheck solution-5ddbbc1e
  reduce #(assoc % %2
                   (if (% %2)
                     (inc (% %2))
                     1)) {})

(defcheck solution-5ef311ee
  (fn [s]
    (reduce (fn [m x] (assoc m x (inc (get m x 0)))) {} s)))

(defcheck solution-5f230bd1
  (fn count-distinct [s]
    (apply merge
      (map
        #(hash-map % (count (filter (fn [x] (= % x)) s)))
        (distinct s)))))

(defcheck solution-5f76ea11
  (partial reduce (fn [a b] (let [v (get a b)]
                              (assoc a b (if v (inc v) 1))))
    {}))

(defcheck solution-5f9da4cf
  (fn [s] (reduce #(assoc % %2 (if-let [c (% %2)] (inc c) 1))
            {}
            s)))

(defcheck solution-5fe76b8d
  (fn [s_]
    (loop [s s_ result {}]
      (if (empty? s)
        result
        (let [h (first s) n (count (filter (partial = h) s))]
          (recur (filter (partial not= h) s) (conj result [h n])))))))

(defcheck solution-602114da
  (fn [col](reduce conj ( map #(hash-map  (key %) (count (val %)))
                          (group-by identity col)))))

(defcheck solution-6063caa2
  (fn [coll]
    (apply merge-with +
      (map hash-map coll (repeat 1)))))

(defcheck solution-6075c45d
  (fn my-freq[coll]
    (apply assoc {} (mapcat #(list (first %) (count (second %))) (group-by identity coll)))
    ))

(defcheck solution-60893bbf
  reduce #(update-in %1 [%2] (fnil inc 0)) {})

(defcheck solution-60934dee
  #(->> %
     (group-by identity)
     (reduce (fn [m [k v]] (assoc m k (count v))) {})))

(defcheck solution-60bcabae
  (fn [s]
    (letfn [(accum [m x]
              (assoc m x (inc (get m x 0))))]
      (reduce accum {} s))))

(defcheck solution-60d217f9
  (fn frequenciesX [x] (reduce-kv (fn [m k v] (assoc m k (count v))) {} (group-by identity x))))

(defcheck solution-60ec11d
  (comp
   (partial apply merge)
   (partial map #(hash-map (first %) (count (second %))))
   (partial group-by identity)))

(defcheck solution-614c97bc
  #(apply merge-with + (map (fn [a] {a 1}) %)))

(defcheck solution-61640621
  #(reduce (fn [res x] (update-in res [x] (fnil inc 0))) {} %1))

(defcheck solution-616975d6
  (fn num-occur [l]
    (reduce (fn [m [k v]] (assoc m k (count v))) {} (group-by identity l))))

(defcheck solution-61877261
  (fn [xs] (into {} (map (fn [[k v]] [k (count v)]) (group-by identity xs)))))

(defcheck solution-61ca684
  reduce #(assoc % %2 (inc (get % %2 0))) {})

(defcheck solution-61cb955d
  (fn [s]
    (loop [s s
           m {}]
      (if (empty? s)
        m
        (let [n (filter #(not= (first s) %) s)]
          (recur n (assoc m (first s)
                            (- (count s) (count n)))))))))

(defcheck solution-61ce7fef
  (fn [coll]
    (reduce (fn [acc val]
              (assoc acc val (inc (get acc val 0))))
      {}
      coll)))

(defcheck solution-61e9bb2f
  #(apply hash-map ((fn[col] (loop[c col res []] (let [r ((fn[coll] (let [r (remove (fn[x] (= x (first coll))) coll)] [(first coll) (- (count coll) (count r)) r])) c)] (if (empty? (last r)) (conj res (first r) (second r)) (recur (last r) (conj res (first r)(second r))))))) %)))

(defcheck solution-6245d656
  #(into {}
     (map (fn[[k v]] [k (count v)]) (group-by identity %))))

(defcheck solution-6276b425
  (fn [s] (reduce #(update-in % [%2] (fnil inc 0)) {} s)))

(defcheck solution-62847423
  (fn[x](apply hash-map (apply concat (map #(vector (last %) (count %)) (partition-by identity (sort x)))))))

(defcheck solution-62883b4c
  #(reduce
     (fn [m k] (assoc m k (inc (get m k 0))))
     {}
     %))

(defcheck solution-62f4d567
  (fn [col] (reduce (fn [l r] (update-in l [r] inc))
              (zipmap col (repeat 0)) col)))

(defcheck solution-62f8b456
  (fn [s]
    (into {}
      (map #(vector (first %) (count (second %)))
        (group-by identity s)))))

(defcheck solution-63045cbd
  #(->> % sort (group-by identity) (map (fn [[k v]] [k (count v)])) (into {})))

(defcheck solution-63092ade
  (fn [xs]
    (reduce
      (fn [freqs x]
        (update-in freqs [x] (fn [y] (inc (or y 0)))))
      {}
      xs)))

(defcheck solution-630b26e6
  reduce (fn [s x]
           (assoc s x (inc ( s x 0)))) {})

(defcheck solution-632ea501
  (fn histogram [items]
    (into {}
      (for [[elem occurrences] (group-by identity items)]
        [elem (count occurrences)]))))

(defcheck solution-63b4e46d
  ;(fn [sarg]
  ;     (loop [s sarg,
  ;            ret {}]
  ;       (if (seq s)
  ;         (do (println s ret)
  ;           (recur (rest s)
  ;                  (assoc ret
  ;                         (first s)
  ;                         (inc (get ret (first s) 0)))))
  ;         ret)))
  #(->> %
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})))

(defcheck solution-64af87b0
  (fn [coll]
    (into {}
      (map #(vector (first %) (count %))
        (partition-by identity (sort coll))))))

(defcheck solution-64e575f2
  (fn [xs]
    (reduce
      (fn [acc, x] (merge-with + acc {x 1}))
      {} xs)))

(defcheck solution-65134ce0
  (fn [col] (into {} (map (fn [i][(first i) (count (last i))]) (group-by identity col)))))

(defcheck solution-654c0cc7
  #(into {}
     (map (fn [x]
            (vector (first x) (count (last x))))
       (group-by identity %))))

(defcheck solution-65546dce
  (fn [s]
    (reduce #(assoc % %2 (inc (% %2 0))) {} s)))

(defcheck solution-65f6de2f
  (fn [s]
    (loop [tail s, freqs {}]
      (if-let [[x & xs] (seq tail)]
        (recur xs (assoc freqs x (inc (get freqs x 0))))
        freqs))))

(defcheck solution-6638b3b7
  (fn [x]
    (apply
      merge-with
      +
      (map
        #(hash-map % 1)
        x
        )
      )
    ))

(defcheck solution-664803e
  (fn my-frequencies [x]
    (into {} (for [[k v] (group-by identity x)] [k (count v)]))))

(defcheck solution-671c2cb8
  (fn co [input] (apply merge (map #(assoc {} (first %) (count %)) (vals (group-by identity input))))))

(defcheck solution-6730a44
  (fn [xs] (reduce (fn [m x]  (update-in m [x] #(if % (inc %) 1))) {} xs) ))

(defcheck solution-67e7f6e2
  #(into {}  (for [[k v]  (group-by identity (sort %))] [k (count v)])))

(defcheck solution-67f54e64
  #(apply merge (map (fn [[k v]] {k (count v)}) (group-by identity %))))

(defcheck solution-688ff24b
  (fn [c] (let [g (group-by (fn [x] x) c)]  (apply hash-map (interleave (keys g) (map #(count %) (vals g)))))))

(defcheck solution-68933b52
  #(into {} (for [[k v] (group-by identity %)]
              [k (count v)])))

(defcheck solution-6983b06d
  #((fn [x] (zipmap (keys x) (map count (vals x)))) (reverse (group-by identity %))))

(defcheck solution-69875ce
  (fn [coll]
    (apply hash-map
      (mapcat (fn [[k v]] [k (count v)])
        (group-by identity coll)))))

(defcheck solution-69b6aac0
  (fn [input]
    (zipmap
      (distinct input)
      (map (fn [item] (count (filter #(= % item) input))) (distinct input)))))

(defcheck solution-69cb68ff
  (fn [coll] (into {} (for [[i s] (group-by identity coll)] [i (count s)]))))

(defcheck solution-6a7126cb
  #(let [m (group-by identity %)]
     (into {} (for [[k v] m] [k, (count v)]))))

(defcheck solution-6ad71c08
  #(reduce
     (fn [map x]
       (assoc map x (inc (get map x 0))))
     {} %1))

(defcheck solution-6b5f2320
  (fn f [coll]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll)))

(defcheck solution-6b64016e
  (fn [s]
    (let [h (group-by identity s)
          ks (keys h)
          vs (map count (vals h))]
      (zipmap ks vs))))

(defcheck solution-6ba08034
  (fn [coll]
    (reduce
      (fn [res, v]
        (conj res (update-in res [v] (fnil inc 0))))
      {}
      coll)))

(defcheck solution-6baa1fc9
  #(reduce (fn [m [x c ]] (assoc m x (count c)) ) {} ( group-by identity %)))

(defcheck solution-6bf48d7e
  reduce (fn [res x] (update-in res [x] (fnil inc 0))) {})

(defcheck solution-6bfe433d
  #(into {}
     (map (fn [[k v]] [k (count v)]) (group-by identity %))))

(defcheck solution-6cb632b1
  (fn count [c]
    (loop [s (seq c)
           table {}]
      (if (empty? s)
        table
        (recur (rest s)
          (update-in table
            [(first s)]
            (fn [v] (if (nil? v) 1 (inc v)))))))))

(defcheck solution-6d339fef
  (fn [lst]
    (reduce  (fn [m [k v]] (assoc m k (count v))) {} (group-by identity lst))))

(defcheck solution-6d33b7ba
  (fn [v] (reduce #(assoc % %2 (inc (get % %2 0))) {} v)))

(defcheck solution-6d3ef766
  (fn [coll]
    (reduce #(assoc %1 (first %2) (count %2))
      {}
      (partition-by identity (sort coll)))))

(defcheck solution-6d5d891
  (fn [coll]
    (reduce #(into %1 {(first %2) (count (second %2))}) {} (group-by identity coll))))

(defcheck solution-6d7a8f86
  #(apply merge-with + (for [e %] {e 1})))

(defcheck solution-6da5871c
  (fn [xs]
    (into {} (map (fn [[k ks]] [k (count ks)])
               (group-by identity xs)))))

(defcheck solution-6ec84cbb
  (fn [l]
    (into {}
      (map (fn [x]
             {x (count (filter #(= x %) l))})
        (set l)))))

(defcheck solution-6f16df11
  (fn _frequencies [items]
    (into {} (for [[id uniques] (group-by identity items)]
               [id (count uniques)]))))

(defcheck solution-7044ef2d
  #(into {} (for [ [k v] (group-by identity %) ] [k (count v) ] )))

(defcheck solution-70d7f97c
  #(into {}
     (map (fn [[k v]] [k (count v)])
       (group-by identity %))))

(defcheck solution-710edd12
  (fn f [coll]
    (reduce
      #(assoc %1 (first %2) (count (second %2)))
      {}
      (group-by identity coll))))

(defcheck solution-717f6d6b
  #(->> (group-by identity %) (map (fn [item] [(first item) (count  (second item))])) (apply concat) (apply array-map)))

(defcheck solution-71a1e9a6
  (fn [v]
    (reduce
      #(assoc %1 %2 (if (get %1 %2) (inc (get %1 %2)) 1))
      {} v)))

(defcheck solution-7227681d
  (fn [sq]
    (reduce
      #(merge-with + %1 {%2 1})
      {} sq)))

(defcheck solution-729cafa
  (fn [lst]
    (into {} (map (fn [[k v]] [k (count v)]) (group-by identity lst)))
    ))

(defcheck solution-72cf8f8f
  (fn [xs]
    (->> xs
      (group-by identity)
      (map #(update-in % [1] count))
      (into {}))))

(defcheck solution-72d1d58e
  (fn mytest
    [myset]


    (let [getfreq (fn [myseq e]

                    (hash-map e
                      (count
                        (filter #(= e %) myseq)
                        )
                      )

                    )

          ]

      (apply merge (map (partial getfreq myset) myset))

      )

    ))

(defcheck solution-733d0dc8
  #(apply hash-map (mapcat (fn [[k v]]
                             [k (count v)]) (group-by identity %))))

(defcheck solution-737efe94
  (fn count-occurs [coll]
    (reduce (fn [a [key val]] (assoc a key (count val))) {} (group-by identity coll))))

(defcheck solution-7453e85c
  #(apply zipmap ((juxt keys (comp (partial map count) vals)) (group-by identity %))))

(defcheck solution-74bbc8e8
  (fn [coll]
    (into {}
      (map
        (fn [[k v]]
          [k (count v)])
        (group-by identity coll)))))

(defcheck solution-74de9496
  (fn [c] (apply merge (map #(hash-map (key %) (count (val %))) (group-by identity c)))))

(defcheck solution-754b38bd
  (fn wc [coll]
    (reduce (fn [m w] (assoc m w (inc (m w 0)))) {} coll)))

(defcheck solution-755689c1
  (fn [l]
    (apply merge
      (for [x l]
        {x (count (filter #(= x %) l))}))))

(defcheck solution-755feea3
  (fn frequencies'
    ([xs] (frequencies' xs {}))
    ([xs acc]
     (if (empty? xs) acc
                     (let [x0 (first xs),
                           p #(= % x0),
                           q #(not= % x0),
                           ]
                       (recur (filter q xs) (conj acc [x0 (count (filter p xs))])))))))

(defcheck solution-756b20ec
  (fn __ [col]
    (let [grouped (group-by identity col)]
      (zipmap (keys grouped) (map count (vals grouped))))))

(defcheck solution-75a49a04
  (fn count-occ [coll]
    (reduce (fn [counts x] (assoc counts x (inc (get counts x 0))))
      {}
      coll)))

(defcheck solution-75a9b604
  (fn [s]
    (into {}
      (map (fn [[k v]] [k (count v)])
        (group-by identity s)))))

(defcheck solution-75bbc738
  (fn [coll]
    (loop [coll coll
           answer {}]
      (cond
        (nil? coll) answer
        (nil? (get answer (first coll))) (recur (next coll) (merge answer {(first coll) 1}))
        :else (recur (next coll) (assoc answer (first coll) (inc (get answer (first coll)))))))))

(defcheck solution-760550d
  (fn [s] (let [keys (distinct s)
                counts (map (fn [x] (count (filter #(= x %) s))) keys)]
            (zipmap keys counts))))

(defcheck solution-761d8e0e
  (fn freq [xs]
    (into {} (for [x (distinct xs)]
               [x (count (filter (partial = x) xs))]))))

(defcheck solution-7624d41c
  #(reduce (fn [r e] (assoc r e (inc (r e 0)))) {} %))

(defcheck solution-76cb17ca
  reduce #(assoc % %2(+ (% %2 0) 1)) {})

(defcheck solution-7772b822
  (fn fr [ls]
    (reduce
      #(if (contains? % %2)
         (conj (dissoc % %2) (hash-map %2 (inc (get % %2))))
         (conj % (hash-map %2 1)))
      {} ls)))

(defcheck solution-7819cfb7
  #(->> %
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})
     ))

(defcheck solution-782ab39a
  (fn [coll]
    (apply hash-map (mapcat
                      (fn [[a b]] [a (count b)])
                      (group-by identity coll)
                      ))))

(defcheck solution-783a8253
  (fn [x]
    (let [uniqs (set x)]
      (zipmap uniqs
        (for [auniq uniqs]
          (apply + (for [el x] (if (= el auniq) 1 0))))))))

(defcheck solution-7851bd41
  #(apply array-map (mapcat (fn [l] [(first l) (count (fnext l))]) (group-by identity %))))

(defcheck solution-78e00787
  #(apply merge-with + {} (map (fn [x] {x 1}) %)))

(defcheck solution-79363c17
  (fn [s]
    (into {}
      (map
        (fn [[k v]] {k (count v)})
        (group-by identity s)
        )
      )
    ))

(defcheck solution-7990d170
  #(into {} (map (fn [x] [(first x) (count x)]) (partition-by identity (sort %)))))

(defcheck solution-7a526749
  (fn [xs]
    (reduce
      (fn [m x]
        (if
         (contains? m x)
          (assoc m x
                   (inc
                     (get m x)
                     )
                   )
          (assoc m x 1)
          )
        )
      {}
      xs
      )
    ))

(defcheck solution-7a941c73
  (fn [v]
    (letfn [(update-map [m v]
              (let [old-val (m v)]
                (assoc m v (if old-val (inc old-val) 1))))]
      (reduce #(update-map %1 %2) {} v))))

(defcheck solution-7ad18b18
  #(reduce (fn [acc e]
             (assoc acc e (inc (or (acc e) 0))))
     {} %))

(defcheck solution-7af629c4
  #(let [x (group-by identity (sort %))] (apply hash-map (interleave (keys x) (map count (vals x))))))

(defcheck solution-7bad089d
  (fn [coll]
    (into {} (map #(vector (first %) (count %)) (partition-by identity (sort coll))))
    ))

(defcheck solution-7bcdc89b
  (fn my-count
    [coll]
    (loop [acc {}
           curr (first coll)
           remaining (rest coll)]
      (if (= nil curr)
        acc
        (recur
          (if (= nil (get acc curr))
            (merge acc {curr 1})
            (merge acc {curr (+ (get acc curr) 1)}))
          (first remaining)
          (rest remaining))))))

(defcheck solution-7bfbe69
  (fn [coll] (reduce (fn[m e] (update-in m [e] (fnil inc 0))) {} coll)))

(defcheck solution-7c16eb45
  (fn [l]
    (loop [news {} tmpl l]
      (if (empty? tmpl)
        news
        (if (contains? news (first tmpl))
          (recur (assoc news (first tmpl) (inc (get news (first tmpl)))) (rest tmpl))
          (recur (assoc news (first tmpl) 1) (rest tmpl)))))))

(defcheck solution-7c424ead
  (fn freq [col]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} col)))

(defcheck solution-7c5d7775
  (fn [xs] (apply hash-map (mapcat #(list (first %) (count %)) (partition-by identity (sort xs))))))

(defcheck solution-7c941f74
  (fn [c]
    (reduce (fn [m x]
              (update-in m [x] (fnil inc 0)))
      {} c)))

(defcheck solution-7cb8be04
  (fn [coll]
    (reduce (fn [a b]
              (update-in a [b] (fnil inc 0))) {} coll)))

(defcheck solution-7ce5d35e
  (fn [x]
    (zipmap (distinct x)
      (map (fn [a] (count (filter #(= % a) x))) (distinct x)))))

(defcheck solution-7cf03a0e
  #(apply hash-map (mapcat (fn[e](list (first e) (count (second e)))) (group-by identity %))))

(defcheck solution-7d0a35ec
  (fn [xs]
    (loop [m {} [h & t :as xs] xs]
      (if xs
        (recur (assoc m h (inc (m h 0))) t)
        m))))

(defcheck solution-7d23763d
  #(apply merge-with + (map (fn [i] {i 1}) %)))

(defcheck solution-7d7df7cb
  #(reduce
     (fn [freqs elem]
       (merge-with + freqs {elem 1}))
     {}
     %))

(defcheck solution-7d9d852c
  (fn [coll]
    (let [ a (group-by identity coll)
          b  (for [[key value] a]   [key (count value)])]
      (into {} (vec b)))))

(defcheck solution-7dfe6d6
  (fn[_seq](reduce (fn[result head]
                     (assoc result head (inc(get result head 0)))) {} _seq)))

(defcheck solution-7e6a0992
  (fn [coll]
    (->> (group-by identity coll)
      (reduce-kv #(assoc %1 %2 (count %3)) {}))))

(defcheck solution-7ecd6258
  (fn occurences [s]
    (reduce
      (fn [acc x] (assoc acc x (inc (get acc x 0))))
      {} s)))

(defcheck solution-7f5c34fc
  (fn [xs]
    (into {} (map #(let [[key val] %] [key (count val)])
               (group-by identity xs)))))

(defcheck solution-7f69e86b
  (fn [coll]
    (->> (group-by identity coll)
      (map (fn [[k v]] [k (count v)]))
      (into {}))))

(defcheck solution-7f905dc
  #(->> % (group-by identity) (reduce-kv (fn [m k v] (assoc m k (count v))) {})))

(defcheck solution-7fbd63dd
  reduce #(update-in %1 [%2] (fnil inc 0)) {})

(defcheck solution-8035b4e7
  #(let [instances (group-by identity %)]
     (reduce (fn [acc v] (assoc acc v (-> (instances v) count)))
       {}
       (keys instances))))

(defcheck solution-80cb5fd
  (fn [a] (into {} (map (fn [[x y]] [x (count y)]) (group-by identity a)))))

(defcheck solution-810b73a0
  (fn freq [coll]
    (loop [coll coll m {}]
      (if (empty? coll)
        m
        (let [k (first coll)
              v (m k)
              inc-v (if (number? v) (inc v) 1)]
          (recur (rest coll) (assoc m k inc-v)))))))

(defcheck solution-81c56273
  (fn freq [seq]
    (into {}
      (map (fn [v] [v (count (filter #(= v %) seq))])
        (set seq)))))

(defcheck solution-822425b3
  #(persistent! (reduce (fn [counts x] (assoc! counts x (inc (get counts x 0)))) (transient {}) %)))

(defcheck solution-822e4519
  (fn occurrences [s]
    (let [update-map (fn [m, k] (if (contains? m k)
                                  (update-in m [k] inc)
                                  (assoc-in m [k] 1)))]
      (loop [result {}, s2 s]
        (if (empty? s2)
          result
          (recur (update-map result (first s2)) (rest s2))
          )))))

(defcheck solution-82920d8a
  reduce #(conj %1 {%2 (inc (or (get %1 %2) 0))}) {})

(defcheck solution-82eb98ab
  (fn [xs] (reduce-kv #(assoc %1 %2 (count %3)) {} (group-by identity xs))))

(defcheck solution-8308d8d1
  (fn [xs]
    (into
      {}
      (map
        (fn [x] [(first x) (count (second x))])
        (group-by identity xs)))))

(defcheck solution-83886539
  #(let [coll (partition-by identity (sort %))] (zipmap (map first coll) (map count coll))))

(defcheck solution-83b12df3
  (fn [l]
    (reduce #(merge-with + % {%2 1}) {} l)))

(defcheck solution-83d20b23
  (fn [v]
    (reduce
      #(assoc % %2 (inc (if (% %2) (% %2) 0) )) {} v)))

(defcheck solution-83eb733c
  #(reduce (fn [res x] (update-in res [x] (fnil inc 0))) {} %))

(defcheck solution-83ebfad8
  (fn [c]
    (reduce
      (fn [v e]
        (if
         (contains? v e)
          (assoc v e (inc (v e)))
          (assoc v e 1))
        ) {} c)
    ))

(defcheck solution-84e8d5d2
  (fn [coll]
    (let [gp (group-by identity coll)]
      (zipmap (keys gp) (map #(count (second %)) gp)))))

(defcheck solution-8533b066
  #(let [coll (group-by identity %)]
     (zipmap (keys coll) (map count (vals coll)))))

(defcheck solution-853be938
  (fn [coll]
    (reduce (fn [res-map item]
              (let [freqs (get res-map item)]
                (if (nil? freqs)
                  (assoc res-map item 1)
                  (assoc res-map item (inc freqs)))))
      {}
      coll)))

(defcheck solution-8563cff
  (fn func
    [input-seq]
    (let [occur
          (fn [input-map elem]
            (if (contains? input-map elem)
              (do
                (swap! (input-map elem) inc)
                input-map)
              (assoc input-map elem (atom 1))))]
      (->>
        input-seq
        (reduce occur {})
        (reduce-kv
          (fn [m k v]
            (assoc m k (deref v)))
          {})))))

(defcheck solution-85bf3c2c
  (fn mc
    ([lst] (mc {} (sort lst)))
    ([m lst]
     (if (empty? lst) m
                      (let [o (first lst) ls (take-while #(= o %) lst)]
                        (mc (conj m [o (count ls)]) (drop-while #(= o %) lst )))))))

(defcheck solution-85bfa430
  (fn [xs]
    (reduce conj (let [ss (set xs)]
                   (for [s ss]
                     (hash-map s (count (filter #(= s %) xs)))
                     )
                   )
      )))

(defcheck solution-85fe460c
  (fn [x]
    (apply merge
      (map #(hash-map (first %) (count %))
        (partition-by identity (sort x))
        )
      )
    ))

(defcheck solution-867e953d
  (fn freq [coll]
    (loop [coll coll,
           result {}]
      (if (empty? coll)
        result
        (if (contains? result (first coll))
          (recur (rest coll) (assoc result (first coll) (inc (result (first coll)))))
          (recur (rest coll) (assoc result (first coll) 1)))))))

(defcheck solution-8695ff05
  reduce #(assoc % %2 (inc (% %2 0))) {})

(defcheck solution-86dc7366
  (fn [xs]
    (reduce #(update-in %1 [%2] inc)
      (zipmap (into #{} xs) (repeat 0))
      xs)))

(defcheck solution-870bb178
  (fn [sq]
    (reduce (fn [m e]
              (assoc m e (inc (get m e 0))))
      {} sq)))

(defcheck solution-873eb473
  (fn [c] (into {} (map #(vector (first %) (count %))
                     (vals (group-by identity c))))))

(defcheck solution-875c5da
  (fn [c]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} c)))

(defcheck solution-876e6c32
  reduce (fn [acc elt] (let [elt-cnt (get acc elt 0)]
                         (assoc acc elt (inc elt-cnt)))) {})

(defcheck solution-87efbdd8
  (fn [coll]
    (loop [x coll n {}]
      (cond
        (empty? x) n
        (contains? n (first x)) (recur (next x) (update-in n [(first x)] inc))
        :else (recur (next x) (assoc n (first x) 1))))))

(defcheck solution-87f73f1d
  ;update-in since update not allowed for some reason
  (fn [col] (reduce (fn [m v] (update-in m [v] #(if % (inc %) 1)))
              {}
              col)))

(defcheck solution-88147362
  (fn freq [coll]
    (into {} (map (fn [[v c]] [v (count c)]) (group-by identity coll)))))

(defcheck solution-88194150
  #(let [m (group-by identity %)]
     (zipmap (keys m) (map count (vals m)))))

(defcheck solution-88d1e0e4
  (fn ic [s]
    (into {} (map (fn [[k v]] [k (count v)]) (group-by identity s)))))

(defcheck solution-88d7c4fa
  #(reduce (fn [result value]
             (assoc result value (inc (get result value 0))))
     {}
     %))

(defcheck solution-88ffc478
  (fn[x] (reduce #(into % {(first %2) (count %2)}) {} (partition-by identity (sort x)))))

(defcheck solution-89372013
  (fn [c]
    (reduce
      #(assoc %1 %2 (inc (get %1 %2 0)))
      {} c)))

(defcheck solution-894d2a1d
  (fn [coll] (apply (partial merge-with #(+ %1 %2)) (map (fn [ele] {ele 1}) coll))))

(defcheck solution-89647be4
  (fn freq [s]
    (let [os (group-by identity s)]
      (zipmap (map first os)
        (map (comp count second) os)))))

(defcheck solution-89790514
  reduce (fn [S t] (assoc S t (inc (S t 0)))) {})

(defcheck solution-89cb947a
  (fn [s] (reduce (fn [m i] (assoc m i (inc (get m i 0)))) {} s)))

(defcheck solution-89d1c5c5
  #(loop [items % counts {}]
     (if (seq items)
       (recur (next items) (assoc counts (first items) (inc (counts (first items) 0))))
       counts)))

(defcheck solution-8a4eed21
  (fn [s]
    (reduce (fn [agg i] (assoc agg (first i) (count (last i))))
      {} (group-by (fn [s] s) s))))

(defcheck solution-8a6bb2e0
  #(reduce (fn [m i] (conj m [(first i) (count i)])) {} (partition-by identity (sort %))))

(defcheck solution-8b53bff7
  reduce (fn [keep add]
           (merge-with
             +
             keep
             {add 1})) {})

(defcheck solution-8b7ed1dd
  #(into {} (for [p (partition-by identity (sort %))] [(first p) (count p)])))

(defcheck solution-8b8895d8
  #(let [s (partition-by identity (sort %))]
     (zipmap (map first s) (map count s))))

(defcheck solution-8bc882f8
  (fn [coll]
    (into {}
      (map #(vector (first %) (count %))
        (partition-by identity (sort coll))))))

(defcheck solution-8c1702ed
  (fn freqs [l]
    (loop [l l,
           frequency-map {}]
      (if (empty? l) frequency-map,
                     (recur (rest l)
                       (assoc frequency-map
                         (first l)
                         (inc (get frequency-map (first l) 0))))))))

(defcheck solution-8c79be4c
  (fn [coll]
    (reduce (fn [freqs item]
              (update-in freqs [item] #(inc (or % 0))))
      {}
      coll)))

(defcheck solution-8c9117b5
  (fn [s]
    (reduce
      #(if (contains? %1 %2)
         (assoc %1 %2 (inc (get %1 %2)))
         (assoc %1 %2 1))
      {}
      s)))

(defcheck solution-8d0d1abc
  #(reduce
     (fn [a b]
       (update-in a [b]
         (fn [e] (if (nil? e) 1 (inc e)))))
     {}
     %))

(defcheck solution-8d7a302e
  (fn [s] (->>
            s
            sort
            (partition-by #(do %))
            (map #(assoc {} (first %) (count %)))
            (into {})
            )))

(defcheck solution-8d9ee23d
  (fn [s]
    (into
      {}
      (map
        #(vector (first %) (count (second %)))
        (group-by identity s)))))

(defcheck solution-8de1fa34
  (fn[s](reduce #(assoc %1 %2(inc(or(%1 %2)0))){}s)))

(defcheck solution-8df7ea6f
  (fn [li] (into {} (map #(hash-map (first %) (count %)) (partition-by identity (sort li))))))

(defcheck solution-8e37c17a
  reduce (fn[m k](assoc m k (inc (get m k 0)))) {})

(defcheck solution-8e9a85ec
  (fn [s]
    (into {} (map (fn [[k v]] [k (count v)]) (group-by identity s)))))

(defcheck solution-8f14597c
  (fn [s] (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} s)))

(defcheck solution-8f5ec806
  (fn [col]
    (reduce #(update-in %1 [%2] (fnil inc 0))
      {}
      col)))

(defcheck solution-8f810750
  (fn [m] (into {} (for [[k v] (group-by identity m)] [k (count v)]))))

(defcheck solution-8f95c9a3
  (fn [coll]
    (->> (map vector coll (repeat 1))
      (map #(conj {} %))
      (apply merge-with +))))

(defcheck solution-8ff0678e
  #(apply merge-with + (map (fn [x] {x 1}) %)))

(defcheck solution-906b3c36
  (fn [seq] (apply merge-with + (map (fn [v] {v 1}) seq))))

(defcheck solution-90f225f5
  reduce (fn [a b]
           (conj a {b (if (a b)
                        (inc (a b))
                        1)})) {})

(defcheck solution-91ab4e08
  (fn [s]
    (reduce #(assoc % %2 (inc (get % %2 0))) {} s)))

(defcheck solution-91d54225
  (fn [s]
    (let [k (distinct s)
          v (map #(count (for [x s
                               :when (= x %)]
                           x)) k)]
      (zipmap k v))))

(defcheck solution-91e8a491
  (fn [coll]
    (into {}
      (map (fn [e]
             [e (count  (filter #{e} coll))]) (set coll)))))

(defcheck solution-91ee70e0
  (fn [coll] (reduce #(conj %1 {(first %2) (count %2)}) {} (partition-by identity (sort coll)))))

(defcheck solution-9239af1b
  (fn [nums]
    (loop [remaining nums table (hash-map)]
      (if (empty? remaining)
        table
        (if (contains? table (first remaining))
          (recur (rest remaining) (update-in table [(first remaining)] inc))
          (recur (rest remaining) (conj table [(first remaining) 1])))))))

(defcheck solution-92662429
  (fn [xs]
    (->>
      xs
      (map #(hash-map % 1))
      (reduce (partial merge-with +)))))

(defcheck solution-92693aab
  (fn [myCol] (reduce (fn [myMap item] (conj myMap [item (let [value (myMap item)] (if (nil? value) 1 (inc value)))])) {} myCol)))

(defcheck solution-92b3fded
  (fn [l]
    (reduce
      (fn [m e] (update-in m [e] (fnil inc 0)))
      {} l)))

(defcheck solution-92ec1acd
  (fn [c]
    (let [col (vec c) gpset (set col)]
      (apply merge
        (for [i gpset]
          (loop [j 0 k 0 mprt {}]
            (if (< j (count col))
              (recur (inc j)
                (if (= i (col j))
                  (inc k)
                  k)
                (if (= i (col j))
                  (conj mprt {i (inc k)})
                  (conj mprt {i k})))
              mprt)))))))

(defcheck solution-92efbfbc
  (fn [coll]
    (into {}
      (for [x coll]
        [x
         (count (filter #(= % x) coll))]))))

(defcheck solution-92f547c0
  #(reduce (fn [c e] (assoc c (key e) (count (val e)))) {} (group-by identity %)))

(defcheck solution-92fd1e06
  (fn [col]
    (reduce (fn [acc, item] (update-in acc [item] (fnil inc 0))) {} col)
    ))

(defcheck solution-93752c2b
  reduce #(update-in % [%2] (fnil inc 0)) {})

(defcheck solution-940c9556
  (fn[a-seq]
    (let [dist (distinct a-seq)]
      (apply hash-map
        (mapcat (fn[x] [x (count (filter
                                   (fn[y](= y x))
                                   a-seq))])
          dist)))))

(defcheck solution-946786a9
  (comp (partial reduce-kv #(assoc %1 %2 (count %3)) {}) (partial group-by identity)))

(defcheck solution-949119bd
  #(->> %
     (group-by identity)
     (map (fn [x] [(key x) (count (val x))]))
     (into {})))

(defcheck solution-94a2f07b
  (fn [c] (let [g (group-by identity c)] (zipmap (keys g) (map #(count (second %)) g)))))

(defcheck solution-94f4547d
  (fn [coll]
    (->> (map #(hash-map % 1) coll)
      (apply merge-with +))))

(defcheck solution-95180b4d
  (fn [coll]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} coll)))

(defcheck solution-957bd152
  (fn [l]
    (apply merge-with + (map #(hash-map % 1) l))))

(defcheck solution-960b6f9f
  #(into {} (for [[k v] (group-by identity %)] (assoc {} k (count v)))))

(defcheck solution-96138a06
  (fn [x](apply hash-map (reduce #(into %1 [(first %2) (count (second %2))] )[] (group-by identity x)))))

(defcheck solution-965feb30
  #(into {} (map (fn [s] [(first s) (count s) ]) (->> % sort (partition-by identity) ) )))

(defcheck solution-966df87a
  (fn [coll] (let [m (group-by identity coll)] (reduce #(assoc %1 %2 (count (%1 %2))) m (keys m)))))

(defcheck solution-968181a7
  (fn [xs] (let [m (group-by identity xs)
                 ks (keys m)]
             (reduce (fn [m k]
                       (assoc m k
                                (count (m k)))) m ks))))

(defcheck solution-96b5a776
  #(reduce (fn [m k] (assoc m k (inc (get m k 0)))) {} %))

(defcheck solution-9737f523
  #(into {}
     (for [[k v] (group-by identity %)] [k (count v)])))

(defcheck solution-9755aea3
  (fn [c]
    (loop [m {} [x & xs] c]
      (if x (recur (assoc m x (inc (if-let [p (get m x)] p 0))) xs)
            m))))

(defcheck solution-9798fb9f
  (fn count-occurrences [l]
    (loop [l l f {}]
      (if (empty? l)
        f
        (let [m
              (if (find f (first l))
                (update-in f [(first l)] inc)
                (assoc f (first l) 1))]
          (recur (rest l) m))))))

(defcheck solution-97e836af
  (fn count-freq
    [coll]
    (->> coll
      (group-by identity)
      (map (fn [ent]
             (vector (first ent) (count (second ent)))))
      (into {})
      )))

(defcheck solution-97e9d195
  (fn [s]
    (reduce (fn [counts element]
              (assoc counts element (count (filter #(= element %) s))))
      {}
      (distinct s))))

(defcheck solution-97fb2cfa
  (fn [xs]
    (reduce #(if (contains? %1 %2)
               (update-in %1 [%2] inc)
               (assoc %1 %2 1)) {} xs)))

(defcheck solution-98336b52
  (fn my-frequencies [sq]
    (reduce #(assoc %1 (first %2) (count (second %2)))
      {}
      (group-by identity sq))))

(defcheck solution-995765aa
  (fn [v]
    (loop [in v
           out {}]
      (if (empty? in)
        out
        (let [t (first in)]
          (recur (rest in) (into out (if (contains? out t) {t (+ 1 (get out t))} {t 1}))))))))

(defcheck solution-9969e5d0
  reduce #(update-in % [%2] (comp inc (fn [i] (or i 0)))) {})

(defcheck solution-9975c580
  #(reduce (fn[acc el](if(acc el) (assoc acc el (inc (acc el))) (assoc acc el 1)))  {} %))

(defcheck solution-998ea503
  (fn [c] (apply hash-map (apply concat (map #(list (key %) (count (val %))) (group-by identity c))))))

(defcheck solution-99927803
  (fn [coll]
    (reduce #(apply assoc %1 (first (seq %2)))
      (map (fn [key]
             (hash-map key
               (count (filter #(= key %)
                        coll)))) (set coll)))))

(defcheck solution-99a4265e
  (fn [coll]
    (reduce (fn [m x] (update-in m [x] #(if (nil? %) 1 (inc %))))
      {}
      coll)))

(defcheck solution-9a000e52
  #(reduce (fn [m v] (assoc m v (inc (get m v 0)))) {} %))

(defcheck solution-9a8cdbe5
  (fn [x] (apply hash-map (mapcat #(list (first %) (count %)) (vals (group-by identity x))))))

(defcheck solution-9aa85c1c
  (fn [s]
    (reduce
      (fn [m e] (assoc m e (inc (get m e 0))))
      {}
      s)))

(defcheck solution-9aaac6b8
  #(reduce
     (fn dc [l r] (assoc l r (inc (l r 0))))
     {} %))

(defcheck solution-9aff06cc
  (fn [a] (into {} (map-indexed (fn [idx itm] [(first itm) (count itm)] ) (into [] (for [i (set a)] (into [] (filter #(= i %) a))))))))

(defcheck solution-9b5c02bc
  (fn [s] (into {} (map #(vector (first %) (count %)) (partition-by identity (sort s))))))

(defcheck solution-9b8f64d1
  (fn c [x]
    (into {}  (map (fn [[k v]] [k (count v)]) (group-by identity x)))))

(defcheck solution-9c05b96b
  #(into {} (map (fn [[k v]] [k (count v)])
              (group-by identity %))))

(defcheck solution-9c0ae16f
  (fn [s]
    (into {} (map (fn [[a b]] (vector a (count b))) (group-by identity s)))))

(defcheck solution-9c49e14d
  #(into {} (for [x  (distinct %)] [x (count (filter (fn [y] (= y x)) %))])))

(defcheck solution-9c8a96f2
  (fn count-occurrences [v]
    (reduce

      #(assoc % %2 (if (nil? (% %2)) 1 (inc (% %2))))
      {} v)))

(defcheck solution-9d1c68d
  (fn [lst]
    (let [freqs (group-by identity lst)
          ks    (keys freqs)
          vs    (vals freqs)]
      (into {} (map vector ks (map count vs))))))

(defcheck solution-9d914c9b
  (fn [x] (reduce #(assoc % (first %2) (count %2)) {} (vals (group-by identity x)))))

(defcheck solution-9db2a9e6
  (fn [xs]
    (into {} (for [[k v] (group-by identity xs)] [k (count v)]))))

(defcheck solution-9df68ba1
  (fn [xs] (reduce merge (map (fn [[k v]] {k (count v)}) (group-by identity xs)))))

(defcheck solution-9e8eb303
  (fn [c] (apply hash-map (mapcat  #(list (first %) (count (second %)))  (group-by identity c)))))

(defcheck solution-9f073556
  #(into {} (for [[k v] (group-by identity %1)] [k (count v)])))

(defcheck solution-9f31fe9e
  #(reduce
     (fn [m i]
       (if (m i)
         (update-in m [i] inc)
         (assoc m i 1)))
     {}
     %))

(defcheck solution-9f70db4
  (fn [coll] (reduce #(assoc %1 (first %2) (count (second %2))) {} (group-by identity coll))))

(defcheck solution-9faeb13
  (fn [xs]
    (into {}
      (map #(vector (first %) (count (second %)))
        (group-by identity xs)))))

(defcheck solution-a0ab5a5
  #(into {} (map (fn[s](hash-map (first s) (count s))) (partition-by identity (sort %)))))

(defcheck solution-a0ebf060
  #(reduce-kv (fn [m k v] (assoc m k (count v))) {} (group-by identity %1)))

(defcheck solution-a12af11d
  (fn count-occurences [seq]
    (reduce (fn [dict x]
              (let [k (dict x)]
                (if (nil? k)
                  (assoc dict x 1)
                  (assoc dict x (+ k 1)))))
      {}
      seq)))

(defcheck solution-a1786683
  #(apply merge
     (map (fn [[k v]] {k (count v)}) (group-by identity %))
     ))

(defcheck solution-a1875b57
  #(reduce (fn [a b] (assoc a b (inc (a b 0)))) {} %))

(defcheck solution-a22aeb65
  #(apply hash-map(apply concat (for [x (vals (group-by identity %))]

                                  [(first x) (count x)]))))

(defcheck solution-a285ca26
  (fn freqs [lst]
    (let [	cnt #(if (nil? (get %1 %2)) 0 (get %1 %2))
          proc #(assoc %1 %2 (inc (cnt %1 %2)))]
      (reduce proc {} lst))))

(defcheck solution-a2ca0e19
  (comp
   (partial into {})
   (partial map #(vector (first %) (count (second %))))
   (partial group-by identity)
   ))

(defcheck solution-a2e3bb73
  #(->> (group-by identity %)
     (map (fn [[k v]] [k (count v)]))
     (into {})))

(defcheck solution-a32600d3
  #(->> %
     (map (fn [i] (hash-map i 1)))
     (apply merge-with +)))

(defcheck solution-a3317052
  (fn [coll]
    (persistent!
      (reduce (fn [counts x]
                (assoc! counts x (inc (get counts x 0))))
        (transient {}) coll))))

(defcheck solution-a3e9cbac
  (fn [s]
    (reduce (fn [m [k v]] (merge m {k (count v)}))
      {}
      (group-by identity s))))

(defcheck solution-a3fd5a52
  (fn [v]
    (reduce #(assoc %1 (key %2) (count (val %2)))  {} (group-by identity v) )))

(defcheck solution-a46eead5
  (fn [col]
    (reduce #(update-in %1 [%2] (fn [v] (inc (or v 0)))) {} col)))

(defcheck solution-a4791fd9
  #(into (hash-map)
     (map
       (fn [g] [(first g) (count (last g))])
       (group-by identity %))))

(defcheck solution-a4ba7ca
  (fn [s]
    (reduce
      #(assoc % (first %2) (count (second %2))) {}
      (group-by identity s))))

(defcheck solution-a4d0831a
  (fn [coll]
    (->> coll
      (group-by identity)
      (mapcat #(list (key %) (count (val %))))
      (apply hash-map))))

(defcheck solution-a55f33bd
  (fn count-occur
    [s]
    (loop [[h & t] s
           accum {}]
      (let [new-accum (if (nil? (accum h))
                        (assoc accum h 1)
                        (assoc accum h (+ (accum h) 1)))]
        (if (nil? t)
          new-accum
          (recur t new-accum))))))

(defcheck solution-a61db0dc
  #(let [m (group-by identity %)]
     (zipmap (keys m) (map count (vals m)))))

(defcheck solution-a6dd1d87
  (comp (partial apply merge-with +)
        (partial map (fn [x] {x 1}))))

(defcheck solution-a7284b88
  (fn count-occurances
    [sequ]
    (let [identity-map (group-by identity sequ)
          id-map-keys  (keys identity-map)
          id-map-vals  (vals identity-map)
          count-vals   (map count id-map-vals)]
      (zipmap id-map-keys count-vals))))

(defcheck solution-a74a925a
  (fn get-freq [col]
    (reduce (fn [acc x]
              (assoc acc x (inc (get acc x 0))))
      {} col)))

(defcheck solution-a809b523
  #(into {} (map (fn [x] {(first x) (count x)})
              (partition-by identity (sort %)))))

(defcheck solution-a8723fdd
  (fn freqs [x]
    (apply merge
      (for [[k v] (group-by identity (sort x))]
        {k (count v)}
        ))))

(defcheck solution-a8c705c
  #(into {} (for [[x y] (group-by identity %)] [x (count y)])))

(defcheck solution-a9070f39
  (fn [s]
    (into {}
      (for [[k v] (group-by (fn [x] x) s)]
        [k (count v)]))))

(defcheck solution-a96c2091
  reduce (fn [m n] (assoc m n (+ (get m n 0) 1))) {})

(defcheck solution-a9bf9467
  (fn [x] (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} x)))

(defcheck solution-aa5898e5
  (comp
   #(zipmap (map first %) (map count %))
   #(partition-by identity (sort %))))

(defcheck solution-aab5a698
  #(into {} (for [[k v] (group-by identity %)]
              [k  (count v)])))

(defcheck solution-aae95b07
  (fn my-frequencies [coll]
    (into {}
      (map (fn [[key value]]
             {key (count value)})
        (group-by identity coll)))))

(defcheck solution-ac20cc87
  #(->> %
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})))

(defcheck solution-ac8112af
  #(apply merge-with + (map (fn[a] {a 1}) %)))

(defcheck solution-accd79e1
  (fn f [s]
    (if (= 1 (count s))
      {(first s) 1}
      (let [m (f (rest s))]
        (assoc m (first s) (inc (get m (first s) 0)))))))

(defcheck solution-acd2cd88
  #(into {} (map (fn [[k v]] {k (count v)}) (group-by identity %))))

(defcheck solution-acf15de4
  #(into {} (map (fn[[x ls]] {x (count ls)})  (group-by identity %))))

(defcheck solution-acf70ebb
  (fn my-freqs [coll]
    (into {}
      (map #(vector (first %) (count (second %)))
        (group-by identity coll)))))

(defcheck solution-ad22646
  (fn[xs]
    (into {}
      (map #(vector (first %) (count (second %)))
        (group-by identity xs)))))

(defcheck solution-ad456c53
  (fn counter [coll]
    (reduce
      (fn [m n]
        (assoc
         m
          n
          (count
            (filter
              #(= %1 n)
              coll))))
      {} (set coll))))

(defcheck solution-ad5000da
  (fn [col]
    (let [x (group-by identity col)]
      (zipmap (keys x)
        (map count (vals x))))))

(defcheck solution-ad5e3d30
  (fn occs [s]
    (let [i (group-by identity s)]
      (zipmap (keys i) (map count (vals i))))))

(defcheck solution-adfbd78
  (fn [xs]
    (apply hash-map (mapcat #(list (first %) (count (second %))) (group-by identity xs)))))

(defcheck solution-ae1b6a
  (fn [col]
    (reduce #(conj % [(first %2) (count (second %2))]) {} (vec (group-by identity col)))))

(defcheck solution-ae209562
  (fn [s] (reduce #(merge %1 (hash-map (first %2) (count (second %2)))) {} (group-by identity s))))

(defcheck solution-ae351a1f
  #(reduce (fn [m [k v]] (assoc m k (count v))) {} (group-by identity %)))

(defcheck solution-af525428
  (fn m [c]
    (reduce (partial merge-with +) (map #(hash-map % 1) c))))

(defcheck solution-b0132da3
  (fn [xs]
    (let [partitioned (partition-by identity (sort xs))]
      (into {} (map
                 vector
                 (map #(first %) partitioned)
                 (map #(count %) partitioned))
        )
      )
    ))

(defcheck solution-b05234da
  #(into {} (map (fn [[e es]] [e (count es)]) (group-by identity (sort %)))))

(defcheck solution-b05f4859
  (fn [s] (reduce #(assoc % %2 (inc (get % %2 0))) {} s)))

(defcheck solution-b12fd9f9
  (fn [coll]
    (into {}
      (map (fn [[k v]] [k (count v)])
        (group-by identity coll)))))

(defcheck solution-b14df3ae
  (fn [coll]
    (apply array-map (mapcat identity (map (fn [[k v]] [k (count v)]) (group-by identity coll))))))

(defcheck solution-b1770ed5
  (fn [xs]
    (persistent!
      (reduce
        (fn [c n]
          (assoc! c n (inc (get c n 0))))
        (transient {}) xs))))

(defcheck solution-b207398b
  (fn [s]
    (let [g (group-by identity s)
          n (map #(count (val %)) g)
          k (keys g)]
      (zipmap k n))))

(defcheck solution-b23d1864
  #(apply assoc {} (mapcat (fn [e] (list (key e) (count (val e)))) (group-by identity %))))

(defcheck solution-b23e3db8
  #(->>
     (group-by identity %)
     (map (fn [x] [(first x) (count (second x))]))
     (into {}) ))

(defcheck solution-b2c665e
  (fn [xs] (into {} (map #(vec [(first %) (count (second %))]) (group-by identity xs)))))

(defcheck solution-b3c00299
  (fn p55 [s]
    (into {} (map  (fn [l] (vec (list (first l) (count l) ))) (partition-by identity ( sort s))))))

(defcheck solution-b3e98c44
  reduce #(if (contains? %1 %2) (update-in %1 [%2] inc) (assoc %1 %2 1)) {})

(defcheck solution-b4092fe0
  (fn [coll]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll)))

(defcheck solution-b493f334
  reduce (fn[m x](merge-with + m {x 1})) {})

(defcheck solution-b561cc3f
  #(into {} (for [[x c] (group-by identity %)] [x (count c)])))

(defcheck solution-b581faec
  (fn [s]
    (apply hash-map (mapcat #(vector (first %) (count (second %)))
                      (group-by identity s)))))

(defcheck solution-b5867327
  (fn [coll]
    (reduce
      (fn [m v]
        (update-in m [v] (fnil inc 0)))
      {} coll)))

(defcheck solution-b596556d
  (partial reduce #(update-in %1 [%2] (fnil inc 0)) {}))

(defcheck solution-b5d1a659
  #(into {} (map (fn [[k v]] [k (count v)]) (group-by identity %))))

(defcheck solution-b5d97fb1
  (fn count-occurrences
    [xs]
    (loop [xs xs accum {}]
      (if (empty? xs) accum (recur (rest xs) (assoc accum (first xs) (inc (get accum (first xs) 0))))))))

(defcheck solution-b5f70bfc
  (fn [a] (#(zipmap (keys %) (map count (vals %))) (group-by identity a))))

(defcheck solution-b695700
  (fn [a-seq]
    (reduce
      #(assoc %1 (first %2) (second %2))
      {}
      (for [x (group-by #(identity %) a-seq)]
        (vector (key x) (count (val x)))))))

(defcheck solution-b733048
  (fn [coll]
    (reduce (fn [r x] (update-in r [x]
                        #(inc (if (nil? %1) 0 %1))))
      {} coll)))

(defcheck solution-b792e009
  (fn co [coll]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll)))

(defcheck solution-b7bb1561
  #(into (sorted-map)
     (map (fn [[k v]]
            {k (count v)})
       (group-by identity %))))

(defcheck solution-b7ed1916
  (fn freq[l]
    (into {} (map
               (fn [x]
                 (vector
                   x
                   (count
                     (filter
                       #(= x %)
                       l))))
               (apply sorted-set l)))))

(defcheck solution-b97c552
  #(let [v (group-by identity %)]
     (zipmap (keys v) (map count (vals v)))))

(defcheck solution-b998126f
  (fn [a-seq] (reduce
                #(assoc %1 %2 (inc (%1 %2 0)))
                {} a-seq)))

(defcheck solution-b9fddab5
  (fn [coll] (let [f (group-by identity coll)]
               (zipmap (keys f)
                 (map count (vals f)))
               )
    ))

(defcheck solution-ba5a04e4
  #(into {} (for [[k v] (group-by identity %)] {k (count v)})))

(defcheck solution-ba765736
  #(reduce (fn [m x] (assoc m x (inc (m x 0)))) {} %))

(defcheck solution-ba84e314
  ;#(->> %
  ;      (group-by identity)
  ;      (map (fn [[k v]] [k (count v)]))
  ;      (into {}))

  (comp (partial into {})
        (partial map (fn [[k v]] [k (count v)]))
        (partial group-by identity)))

(defcheck solution-baeaac03
  #(into {} (for [ [k v] (group-by identity %1) ] [k (count v) ])))

(defcheck solution-bb12cf4f
  (fn ci [l]
    (let [vars (set l)]
      (reduce conj
        (for [i vars]
          {i (count (filter #(= i %) l))})))))

(defcheck solution-bb140350
  (fn [c]
    (reduce
      #(assoc % %2 (inc (% %2 0)))
      {} c)))

(defcheck solution-bb527335
  (fn [x]
    (reduce
      (fn [acc i]
        (assoc acc i (inc (get acc i 0)))
        )
      {}
      x
      )
    ))

(defcheck solution-bbc27cb
  #(->> %
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})))

(defcheck solution-bbecf7aa
  #(reduce (fn [m k] (assoc m k (inc (m k 0)))) {} %))

(defcheck solution-bc05abd8
  (comp
   (partial apply zipmap)
   (juxt keys (comp (partial map count) vals))
   (partial group-by identity)))

(defcheck solution-bd02f110
  #(into {}
     (map (fn [[k v]] [k (count v)])	(group-by identity (sort %)))))

(defcheck solution-bd7bb507
  #(into {}
     (for [[k v] (group-by identity %)]
       [k (count v)])))

(defcheck solution-bd80b456
  (fn [xs]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} xs)
    ))

(defcheck solution-bdf28ab1
  (fn [x]
    (zipmap
      (keys (group-by identity x))
      (map count (vals (group-by identity x))))))

(defcheck solution-be396d31
  (fn [coll]
    (loop [[el & _ :as coll] coll
           result (apply merge (map #(assoc {} %1 %2) (set coll) (repeat 0)))]
      (if coll
        (recur (next coll)
          (update-in result [el] (fn [old] (inc old))))
        result))))

(defcheck solution-bf0f047f
  (fn freq--group [coll]
    (->> coll                                      ; [1 1 2 3 1 2]
      (group-by identity)                       ; {1 [1 1 1], 2 [2 2], 3 [3]}
      (mapcat (juxt first (comp count second))) ; (1 3 2 2 3 1)
      (apply hash-map))))

(defcheck solution-bf197afe
  (fn[s] (reduce #(assoc-in %1 [%2] (inc (or (get-in %1 [%2] 0)))) {} s)))

(defcheck solution-bf19bc29
  (fn [S]
    (reduce #(if (contains? %1 %2) (assoc %1 %2 (inc (%1 %2))) (assoc %1 %2 1)) {} S
      )
    ))

(defcheck solution-bf2ae5af
  (fn [coll]
    (reduce #(merge-with + % %2) (map #(hash-map % 1) coll))))

(defcheck solution-bf31134b
  (fn [xs]
    (into {}
      (for [[k v] (group-by identity xs)]
        [k (count v)]))))

(defcheck solution-bf40cf74
  #(let [m (->> % (sort)
             (partition-by identity))]
     (zipmap (map first m) (map count m))))

(defcheck solution-bf51afb8
  (fn [s]
    (apply hash-map
      (apply concat
        (map #(list (first %) (count %)) (vals (group-by identity s)))))))

(defcheck solution-bf833dbd
  ;(fn [xs]
  ;  (let [counts (reduce #(assoc %1 %2 0) {} xs)]
  ;    (reduce #(update-in %1 [%2] inc) counts xs)))

  (fn [xs] (reduce #(update-in %1 [%2] (fnil inc 0)) {} xs)))

(defcheck solution-c04f4a86
  (fn [x]
    (let [group (group-by identity x)]
      (zipmap (keys group) (map count (vals group))))))

(defcheck solution-c06ee715
  (fn [aseq] (reduce
               (fn [counts el]
                 (let [cur-count (or (get counts el) 0)]
                   (assoc counts el (inc cur-count)) ))
               {}
               aseq)))

(defcheck solution-c0d6aae3
  #(into {} (map (fn[[k v]] {k (count v)}) (group-by identity %))))

(defcheck solution-c0f7d00c
  #(reduce (fn [m v] (update-in m [v] (fn [x] (if (nil? x) 1 (inc x))))) {} %))

(defcheck solution-c13b760e
  (fn count-occurs
    [xs]
    (reduce
      (fn [m k]
        (if-let [[k v] (find m k)]
          (assoc m k (inc v))
          (assoc m k 1)))
      {} xs)))

(defcheck solution-c19dda2a
  (fn [s]
    (->> (distinct s)
      (mapcat #(list % (count (filter (partial = %) s))))
      (apply hash-map))))

(defcheck solution-c1b465b2
  (fn [li]
    (reduce
      (fn [o i]
        (merge-with + o {i 1}))
      {}
      li)))

(defcheck solution-c2326136
  reduce (fn [v n]
           (assoc v n (inc (or (v n) 0)))) {})

(defcheck solution-c2572caa
  (fn [col] (into {} (for [[x y] (group-by identity col)]
                       [x (count y)]))))

(defcheck solution-c26e993
  #(let [groups (group-by identity %)]
     (zipmap (keys groups) (map count (vals groups)))))

(defcheck solution-c26ed7
  (fn [coll]
    ; here, keys are the items in the collection and values are their counts
    (reduce (fn [map item]
              (assoc map item (inc (get map item 0))))
      {}
      coll)))

(defcheck solution-c2774a57
  (fn [c] (reduce (fn [acc [v vs]] (assoc acc v (count vs))) {} (group-by identity c))))

(defcheck solution-c2c3ec5
  #(into {} (map (fn [[k v]](vector k (count v))) (group-by identity %))))

(defcheck solution-c3339b5b
  (fn [coll]
    (let [m (group-by #(do %) coll)
          k (keys m)
          v (map #(count (get m %)) k)]
      (zipmap k v))))

(defcheck solution-c3490b5
  (fn[x]
    (persistent!
      (reduce (fn [counts, y]
                (assoc! counts y (inc (get counts y 0))))
        (transient {}) x))))

(defcheck solution-c34931fb
  (fn [xs]
    (apply hash-map (mapcat #(list (first %) (count (second %))) (group-by identity xs)))))

(defcheck solution-c3638e44
  #(reduce (fn [memo x] (assoc memo x (+ (memo x 0) 1))) {} %))

(defcheck solution-c3ee0fdd
  (fn [coll]
    (into {}
      (map
        #(vector (first %) (count (second %)))
        (group-by identity coll)))))

(defcheck solution-c40101a
  (fn [x]
    (loop [x x
           acc {}]
      (if (zero? (count x))
        acc
        (recur (remove #{(first (sort x))} (sort x))
          (assoc acc (first (sort x)) (count (take-while #{(first (sort x))} (sort x)))))))))

(defcheck solution-c4cf7a89
  (fn [s] (->> s (map #(-> {% 1})) (apply merge-with +)) ))

(defcheck solution-c4ebe492
  #(into {} (map (juxt first count) (partition-by identity (sort %)))))

(defcheck solution-c4ec4935
  (fn[xs](let[m (group-by identity xs)
              ks (keys m)
              vs (vals m)
              counts (map count vs)]
           (apply hash-map (interleave ks counts)))))

(defcheck solution-c5158cea
  (fn [xs]
    (loop [xs (seq xs) m {}]
      (if xs
        (recur (next xs) (assoc m (first xs) (inc (get m (first xs) 0))))
        m))))

(defcheck solution-c543937d
  (fn meu-frequencies [s]
    (reduce #(assoc %1 %2 (let [valor (%1 %2)]
                            (if (nil? valor)
                              1
                              (inc valor))))
      {}
      s)))

(defcheck solution-c5fa5ccb
  (comp #(zipmap (keys %) (map count (vals %))) (partial group-by identity)))

(defcheck solution-c61344cd
  #(reduce (fn [acc e]
             (assoc acc e (inc (acc e 0)) )) {}  %))

(defcheck solution-c65bd57b
  (fn [col] (->> col
              (group-by identity)
              (map #(vector (first %) (count (second %))))
              (into {}))))

(defcheck solution-c66b5ae5
  (fn count-occ
    ([x] (count-occ x (sorted-map)))
    ([x y]
     (if (= 0 (count x))
       y
       (if (contains? y (first x))
         (recur (rest x) (update-in y [(first x)] inc))
         (recur (rest x) (conj y {(first x) 1})))))))

(defcheck solution-c6d6c3ae
  (fn [s] (reduce #(assoc %1 %2 (+ (get %1 %2 0) 1)) {} s)))

(defcheck solution-c72330fc
  (fn [items]
    (->> items
      (group-by identity)
      (map (fn [[k v]] [k (count v)]))
      (into {}))))

(defcheck solution-c7d796cb
  #(apply merge-with + (for [n %] {n 1})))

(defcheck solution-c7f662f1
  (fn [coll] (let [gp (group-by  identity coll)] (zipmap (keys gp) (map #(count (second %)) gp)))))

(defcheck solution-c814f467
  (fn f
    ([lst]
     (f lst {}))
    ([lst hm]
     (if lst
       (let [
             [x & xs] lst
             v (or (hm x) 0)]
         (f xs (assoc hm x (inc v))))
       hm))))

(defcheck solution-c819347b
  (fn [s] (#(zipmap (keys %) (map count (vals %))) (group-by identity s))))

(defcheck solution-c8195cdb
  #(->> %
     (group-by identity)
     (map (fn [[k v]] [k (count v)]))
     (into {})
     ))

(defcheck solution-c826504c
  reduce (fn [m k] (update-in m [k] (fnil inc 0))) {})

(defcheck solution-c895e015
  (fn [c]
    (reduce #(assoc % %2 (count (filter #{%2} c))) {} c)))

(defcheck solution-c90e0a14
  (fn [s]
    (apply
      hash-map
      (mapcat
        identity
        (map
          #(list (first %1) (count (second %1)))
          (group-by identity s)
          )
        )
      )
    ))

(defcheck solution-c93de84b
  (fn [coll] (reduce #(update-in %1 [%2] (fnil inc 0)) {} coll)))

(defcheck solution-ca47f17d
  (fn comap [s]
    (loop [m {}
           remain s]
      (if (empty? remain) m
                          (let [[nextelt & newremain] remain
                                lu (m nextelt)
                                c (if (nil? lu) 0 lu)
                                newc (inc c)
                                newm (assoc m nextelt newc)]
                            (recur newm newremain))))))

(defcheck solution-ca492514
  #(let [grouped (group-by identity %)]
     (zipmap (keys grouped) (map count (vals grouped)))
     ))

(defcheck solution-cb9280a2
  (fn [x]
    (reduce (fn [x y] (update-in x [y]
                        #(if % (inc %) 1)
                        )
              ) {} x)
    ))

(defcheck solution-cc2269c6
  (fn [coll]
    (loop [result {} elements coll]
      (if elements
        (recur (if (result (first elements))
                 (into result {(first elements) (inc (result (first elements)))})
                 (into result {(first elements) 1})
                 )
          (next elements))
        result
        )
      )
    ))

(defcheck solution-cc53e9ef
  (fn [m]
    (into {}
      (for [[k v] (group-by #(identity %) m)]
        [k (count v)]))))

(defcheck solution-cc6663f3
  (fn [s]
    (reduce
      (fn [m i]
        (assoc m i (inc (m i 0))))
      {}
      s)))

(defcheck solution-cc7d7887
  (fn count- [coll]
    "55. Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
    (->> coll
      (sort)
      (partition-by identity)
      (group-by count)
      (mapcat #(list (ffirst (second %)) (first %)))
      (apply hash-map)
      )))

(defcheck solution-cd5e6b1b
  (fn [coll]
    (into {} (for [[k v] (group-by first (map vector coll (repeat 1)))]
               [k (apply + (map second v))]))))

(defcheck solution-cdc87ead
  #(let [ms (group-by  identity %)] (zipmap (keys ms) (map count (vals ms)))))

(defcheck solution-ce5066a
  (fn  [col]
    (apply hash-map (apply concat (for [[k v] (group-by #(identity %) col)]
                                    [k (count v)])))))

(defcheck solution-cec84ec
  #((fn [m]
      (zipmap (keys m)
        (map count (vals m))))
    (group-by identity %)))

(defcheck solution-cefa01fd
  (fn [m]
    (reduce
      (fn [m [k v]] (assoc m k (count v)))
      {}
      (group-by identity m))))

(defcheck solution-cf284a20
  (fn [cv] (->> cv (group-by identity) (map (fn [c] [(first c) (count (second c))]))
             (into {}) )))

(defcheck solution-cf91e966
  (fn
    [coll]
    (reduce (fn [m k] (update-in m [k] #(inc (or % 0))))
      {}
      coll)))

(defcheck solution-cfa884ea
  #(into {} (for [[x s] (group-by identity %)] [x (count s)])))

(defcheck solution-cfd198c9
  (fn [v] (loop [v v rv {}]
            (if (empty? v)
              rv
              (recur (rest v) (assoc rv (first v) (inc (get rv (first v) 0))))))))

(defcheck solution-cfdde663
  (fn [l]
    (into {}
      (for [[x y] (group-by identity l)]
        [x (count y)]))))

(defcheck solution-d00b5a31
  (fn freqs [coll]
    (loop [m {}
           [x & xs] coll]
      (if (nil? x)
        m
        (recur (update-in m [x] (fnil inc 0))
          xs)))))

(defcheck solution-d0459583
  (fn [xs]
    (loop [ys xs rs {}]
      (if (empty? ys)
        rs
        (let [[y & ys'] ys
              kv (find rs y)
              v' (if (nil? kv) 0 (second kv))
              rs' (dissoc rs y)
              rs'' (assoc rs' y (inc v'))]
          (recur ys' rs''))))
    ))

(defcheck solution-d067e1b5
  #(let [s (group-by identity %)]
     (zipmap (keys s) (map count (vals s)))))

(defcheck solution-d083b835
  reduce #(assoc %1 %2 (inc (get %1 %2 0))) {})

(defcheck solution-d0a85744
  #(into {} (map (fn [[k v]] [k (count v)])
              (group-by identity %))))

(defcheck solution-d0c3774b
  #(into {}
     (map
       (fn [[k v]] [k (count v)])
       (group-by identity %))))

(defcheck solution-d0d4db59
  (comp (partial into {})
        (partial map #(vector (first %) (count (second %))))
        (partial group-by first)
        (partial map #(vector % :dummy))))

(defcheck solution-d0ff5c3a
  (fn [s]( zipmap (keys (group-by identity s)) (map count (vals (group-by identity s))) )))

(defcheck solution-d1294474
  (fn freq
    ([s] (freq s {}))
    ([s result]
     (if (empty? s)
       result
       (let [f (first s)]
         (recur (rest s)
           (assoc result
             f
             (if (contains? result f)
               (inc (result f))
               1))))))))

(defcheck solution-d12a3d3a
  (fn [s]
    (reduce
      (fn [freqs v]
        (update-in freqs [v] #(inc (or % 0))))
      {} s)))

(defcheck solution-d21db753
  #(persistent!
     (reduce (fn [counts x]
               (assoc! counts x (inc (get counts x 0))))
       (transient {}) %)))

(defcheck solution-d2a0c65e
  (fn [xs]
    (let [
          go (fn [acc x]
               (if (contains? acc x)
                 (assoc acc x (inc (acc x)))
                 (conj acc [x 1])))]
      (reduce go {} xs))))

(defcheck solution-d332ba53
  #(into {} (map (fn [[item items]] [item (count items)]) (group-by identity %))))

(defcheck solution-d39315d0
  (fn [s] (reduce #(assoc % %2 (inc (% %2 0))) {} s)))

(defcheck solution-d3b2afd5
  (fn [s] (->> s
            (group-by #(identity %) )
            (mapcat #(list (% 0)(count (% 1))))
            (apply hash-map))))

(defcheck solution-d3baf275
  #(reduce
     (fn [m v]
       (update-in m [v]
         (fn [x]
           (if
            (nil? x) 1
                     (inc x)))))
     {} %))

(defcheck solution-d3bebcbd
  (fn co [[f & n]]
    (if (empty? n)
      {f 1}
      (update-in (co n) [f] #(inc (or % 0))))))

(defcheck solution-d3dd264f
  (fn [xs]
    (reduce #(if (nil? (get %1 %2)) (assoc %1 %2 1) (update-in %1 [%2] inc)) {} xs)))

(defcheck solution-d3e9cfc0
  (fn [coll] (reduce into (map #(hash-map (key %) (count (val %))) (group-by identity coll)))))

(defcheck solution-d3fd9538
  (fn [lst]
    (loop [l (rest (sort lst)) p (first (sort lst)) c 1 r {}]
      (cond
        (empty? l) (into r [[p c]])
        (= p (first l)) (recur (rest l) p (inc c) r)
        :else (recur (rest l) (first l) 1 (into r [[p c]]))))))

(defcheck solution-d401c47c
  (fn occurences [xs]
    (reduce (fn [m elem] (assoc m elem (inc (get m elem 0)))) {} xs)))

(defcheck solution-d40a607b
  #(into {} (map (fn [x] [(first x) (count x)])
              (partition-by identity (sort %)))))

(defcheck solution-d48c81
  #(reduce (fn [acc v] (assoc acc v (inc (get acc v 0)))) {} %))

(defcheck solution-d4cd7d33
  (fn [m] (let [fs (group-by identity m)] (zipmap (keys fs) (map count (vals fs))))))

(defcheck solution-d508ab61
  (partial reduce
    #(assoc %1 %2 (+ 1 (or (get %1 %2) 0)))
    {}))

(defcheck solution-d5109c2d
  (fn myfreq
    ([coll] (myfreq coll {}))
    ([coll acc]
     (if (empty? coll) acc
                       (let [v (get acc (first coll))]
                         (if (nil? v) (myfreq (rest coll) (assoc acc (first coll) 1))
                                      (myfreq (rest coll) (assoc acc (first coll) (inc v)))))))))

(defcheck solution-d535ee0f
  (fn group_ [collect]
    (loop [col collect,result {}]
      (if (empty? col)
        result
        (let [
              idx (first col),
              n (get result idx 0)
              ]
          (recur (rest col)
            (assoc result idx (inc n))
            )
          )
        )
      )
    ))

(defcheck solution-d5cbadb7
  (fn [coll]
    (reduce (fn [freqs x]
              (assoc freqs x
                           (inc (get freqs x 0))))
      {}  coll)))

(defcheck solution-d5d02117
  (fn [s]
    (reduce merge
      (for [[element element-list] (group-by identity s)
            ]
        {element (count element-list)}))
    ))

(defcheck solution-d6227952
  (fn counto [s]
    (let [g (group-by identity s)]
      (reduce (fn [acc k] (update-in acc [k] count)) g (keys g)))))

(defcheck solution-d696f0c1
  (fn [s] (into {} (for [[k v] (group-by identity (sort s))] [k (count v)]))))

(defcheck solution-d6a44bb2
  (fn f [s]
    (reduce
      (fn [m e]
        (if (m e)
          (assoc m e (inc (m e)))
          (assoc m e 1)))
      {}
      s)))

(defcheck solution-d6ddb91b
  #(into {} (map (fn[[k v]][k (count v)]) (group-by identity %))))

(defcheck solution-d828407a
  #(reduce (fn [m e]
             (into m
               {e (inc (m e 0))})) {} %))

(defcheck solution-d8536cda
  (partial reduce #(assoc %1 %2 (+ 1 (get %1 %2 0))) {}))

(defcheck solution-d88c3e08
  (fn occur [s]
    (reduce (fn [res nxt]
              (assoc res nxt (inc (get res nxt 0)))) {} s)))

(defcheck solution-d8d018a0
  (fn [coll] (->> (group-by identity coll)
               (map (fn [[x y]] [x (count y)]))
               (into {}))))

(defcheck solution-d932d8b
  (fn [coll]
    (let [counts (group-by identity coll)
          counts-vec (map #(vector (first %) (count (second %))) counts)]
      (into {} counts-vec))))

(defcheck solution-d98cf2a8
  (fn [xs]
    (apply merge-with + (map (fn [x] {x 1})
                          xs))))

(defcheck solution-da230fcd
  #(->>
     %
     (group-by identity)
     (map (fn [[a b]] [a (count b)]))
     (into {})))

(defcheck solution-dab84ff5
  (fn [coll]
    (let [c (for [i coll] (count (filter #(= % i) coll)))]
      (apply sorted-map
        (interleave coll c)))))

(defcheck solution-db519f08
  (fn [xs] (reduce #(merge-with + % {%2 1}) {} xs)))

(defcheck solution-db524411
  (fn [coll]
    (reduce
      (fn [s e]
        (update-in s [e] #(inc (or % 0))))
      {}
      coll)))

(defcheck solution-db78066f
  (fn [s]
    (apply merge-with + (map #(hash-map % 1) s))))

(defcheck solution-dbb464ab
  (fn [coll]
    (letfn [(add-count [counter-map item]
              (let [current (get counter-map item 0)
                    new-val (inc current)]
                (assoc counter-map item new-val)))]
      (reduce add-count {} coll))))

(defcheck solution-dbfd6b64
  (fn prob55 [col]
    (loop[ret {}
          col col]
      (if (empty? col)
        ret
        (recur
          (assoc ret (first col) (inc (get ret (first col) 0)))
          (rest col))))))

(defcheck solution-dc0bbbc
  (fn [coll]
    (reduce conj {}
      (map #(vector (first %) (count %))
        (vals (group-by identity coll))))))

(defcheck solution-dc44b474
  (fn [coll]
    (reduce conj (for [[k v] (group-by identity coll)] {k (count v)}))))

(defcheck solution-dc4dc664
  (fn [c] (apply merge-with + (map #(hash-map % 1) c))))

(defcheck solution-dc587b96
  (fn [s]
    (reduce
      (fn [m v]
        (update-in m [v] (fnil inc 0)))
      {} s)))

(defcheck solution-dc9a5ce3
  (fn [coll]
    (reduce
      #(assoc %1 %2 (+ (%1 %2 0) 1))
      {}
      coll)
    ))

(defcheck solution-dcaf7b3c
  (fn count-in-seq [s]
    (reduce (fn [m v] (update-in m [v] (fnil inc 0))) {} s)))

(defcheck solution-dd6d8f12
  (fn [s] (let [t (partition-by identity (sort s))] (zipmap (map first t) (map count t)))))

(defcheck solution-ddb59dbc
  (fn [coll] (into {} (for [[k v] (group-by identity coll)] [k (count v)]))))

(defcheck solution-ddc7c12a
  (fn [coll]
    (apply merge-with + (map (fn [x] {x 1}) coll))))

(defcheck solution-de3b573b
  (fn [s]
    (loop [acc {}
           x s]
      (if x
        (recur (assoc acc (first x) (inc (get acc (first x) 0))) (next x))
        acc))))

(defcheck solution-df8f5044
  (fn [c]
    (into {}
      (for [x (distinct c)]
        [x (count (filter #(= x %) c))]))))

(defcheck solution-df92ed4e
  #(letfn [(f [[h & t] m] (let [c (get m h 0) n (assoc m h (inc c))]
                            (if (seq t) (recur t n) n)))] (f % {})))

(defcheck solution-dfd6898
  (fn [coll]
    (reduce
      (fn [m e]
        (update-in m [e] (fnil inc 0)))
      {}
      coll)))

(defcheck solution-dfe04057
  (fn [lst] (apply hash-map (mapcat #(list (first %) (count %)) (map (fn [d] (filter #(= d %) lst)) (set lst))))))

(defcheck solution-e0889484
  (fn [xs]
    (apply hash-map
      (mapcat
        #(list (first %) (count (second %)))
        (group-by identity xs)))))

(defcheck solution-e08bf4a7
  #(reduce (fn [m x] (merge-with + m {x 1})) {} %))

(defcheck solution-e0b4c9c5
  (fn [coll] (into {} (for [x (map (fn [c] (filter #(= % c) coll)) (set coll))] [(first x) (count x)]))))

(defcheck solution-e0bee8bc
  reduce (fn [acc i] (assoc acc i (if (contains? acc i) (inc (acc i)) 1))) {})

(defcheck solution-e0cd4c6a
  #(apply hash-map (mapcat (fn [[k v]] [k (count v)]) (group-by identity %))))

(defcheck solution-e10b1433
  (fn freq [coll] (if (empty? coll) {} (let [tfs (freq (rest coll)) hf (get tfs (first coll) 0)] (assoc tfs (first coll) (+ hf 1))))))

(defcheck solution-e10c7c22
  (fn [s]
    (letfn [(smlod [m e]
              (let [c (get m e)]
                (assoc m e (if (nil? c) 1 (inc c)))))]
      (reduce smlod {} s))))

(defcheck solution-e16ea069
  (fn [l] (into {} (map #(vector (first %) (count (second %))) (group-by identity l)))))

(defcheck solution-e229e7e1
  (fn [xs]
    (into {} (map #(vector (first %) (count (second %))) (group-by identity xs)))))

(defcheck solution-e2324716
  #(into {} (map (fn[[k v]][k (count v)]) (group-by (fn[x]x) %))))

(defcheck solution-e29d19f1
  ( fn [l]
    (loop [res {}  [a & r] l ]
      (if a
        ( let [la (filter #(=  a  % ) l)      ]

          (recur (assoc res  a  (count la) ) (filter #(not=  a  % ) r))
          )
        res
        )

      )

    ))

(defcheck solution-e29df4e0
  (fn [xs]
    (->> (group-by identity xs)
      (map (fn [[k v]] [k (count v)]))
      (into {}))))

(defcheck solution-e33baf6b
  (fn [s]
    (reduce (fn [acc it]
              (assoc acc it (+ (acc it 0) 1)))
      {} s)))

(defcheck solution-e36b08e4
  (fn [coll]
    (->> coll
      set
      (map #(filter (partial = %1) coll))
      (mapcat #(vector (first %1) (count %1)))
      (apply array-map))))

(defcheck solution-e3847ef7
  (fn [xs] (into {} (for [[k v] (group-by identity xs)] [k (count v)] ))))

(defcheck solution-e3bbb503
  reduce (fn [x y]
           (assoc x y (if (x y) (inc (x y)) 1))) {})

(defcheck solution-e491de6b
  (fn
    [s]
    (reduce #(assoc % (first %2) (count (second %2))) {} (group-by identity s))))

(defcheck solution-e4f1406c
  #(into {}
     (map (fn [x] [x (count (filter (partial = x) %))])
       (into #{} %))))

(defcheck solution-e5987ff3
  (fn freq [x]
    (letfn [(freq2 [x y]
              (if (empty? x)
                y
                (let [filtered (filter (fn [z] (= z (first x))) x)]
                  (freq2 (remove #{(first x)} x) (assoc y (first x) (count filtered)))
                  )
                )
              )]
      (freq2 x {})
      )
    ))

(defcheck solution-e64b8a05
  reduce #(assoc % %2 (inc (% %2 0))) {})

(defcheck solution-e67fce33
  (fn [coll]
    (reduce
      (fn [a b]
        (let [val (get a b 0)]
          (assoc a b (inc val))))
      {} coll)))

(defcheck solution-e6df7139
  (fn freq [coll]
    (persistent!
      (reduce (fn [counts x]
                (assoc! counts x (inc (get counts x 0))))
        (transient {}) coll))))

(defcheck solution-e6f3e3ee
  (fn my-grouping [coll]
    (loop [coll coll state {}]
      (if (empty? coll) state
                        (let [h (first coll) t (rest coll)]
                          (if-let [counter (get state h)]
                            (recur t (conj state [h (inc counter)]))
                            (recur t (conj state [h 1]))))))))

(defcheck solution-e74a40e6
  (fn [x] (apply hash-map (apply concat (map (fn [[i j]] [i (count j)]) (group-by identity x))))))

(defcheck solution-e7bdc638
  #(reduce
     (fn [acc e]
       (if (contains? acc e)
         (conj acc [e (+ 1 (acc e))])
         (conj acc [e 1])))
     {} %))

(defcheck solution-e8630c51
  #(reduce (fn [m [k v]] (merge m {k (count v)})) {} (group-by identity %)))

(defcheck solution-e86b5f11
  (fn
    [seq]
    (loop [l (partition-by identity (sort seq)) m {}]
      (if (empty? l)
        m
        (recur (rest l) (assoc m (first (first l)) (count (first l))))))))

(defcheck solution-e8a56e93
  (fn [coll]
    (into {} (for [[k v] (group-by identity coll)] [k (count v)]))))

(defcheck solution-e8e24c13
  (fn [coll]
    (reduce (fn [m k] (update-in m [k] #(inc (or % 0)))) {} coll)))

(defcheck solution-e941b3ca
  (fn [coll] (into {}
               (map
                 (fn [a] [ (first a) (count (second a))])
                 (group-by identity coll)))))

(defcheck solution-e944f3c7
  (fn [xs]
    (reduce
      (fn [acc x]
        (update-in acc [x]
          (fn [count]
            (if count (inc count) 1))))
      {} xs)))

(defcheck solution-e9809db6
  (fn [coll]
    (let [gs (partition-by identity (sort coll))]
      (zipmap
        (map first gs)
        (map count gs)))))

(defcheck solution-e983a3fe
  #(loop [n 0, r {}]
     (if (= (count %) n)
       r
       (let [x (nth % n)]
         (recur (inc n) (assoc r x (+ 1 (get r x 0))))))))

(defcheck solution-ea0b81aa
  (fn occ [s]
    (apply (partial merge-with +) (reduce (fn [x y]
                                            (conj x {y 1} )

                                            ) [] s))

    ))

(defcheck solution-ea3f0919
  (fn freqs
    ([coll]
     (freqs coll {}))
    ([coll map]
     (if (empty? coll)
       map

       (if (nil? (get map (first coll)))
         (freqs (rest coll) (assoc map (first coll) 1))
         (freqs (rest coll) (assoc map (first coll) (inc (get map (first coll))))))))))

(defcheck solution-ea8ea4d6
  (fn [s] (into {} (map #(vector (key %) (count (val %))) (group-by identity s)))))

(defcheck solution-ea9581d5
  (fn [s]
    (into {} (for [[k v] (group-by identity s)] [k (count v)]))))

(defcheck solution-eaa27de6
  #(apply merge-with + (for [i %] {i 1})))

(defcheck solution-eb6a7752
  #(into {}
     (for [[k v] (group-by identity %)]
       [k (count v)])))

(defcheck solution-eb9f7851
  (fn ! [s]
    (let [m (group-by identity s) k (keys m) v (vals m)]
      (zipmap k (map count v)))))

(defcheck solution-ebee7025
  #(reduce (fn [c v]
             (update-in c [v] (fnil inc 0) )) {} %))

(defcheck solution-ec4beed
  (fn occ [s] (apply merge-with + (map #(hash-map % 1) s))))

(defcheck solution-ec6ed9e
  (fn [v]
    (let [col (group-by identity v) ]
      (loop [result {} newcol col]
        (let [itm (first newcol)]
          (if (empty? newcol)
            result
            (recur (conj result [ (first itm) (count (second itm) ) ] )  (rest newcol) )
            ))
        )
      )

    ))

(defcheck solution-ec93f357
  (fn [xs]
    (reduce
      (fn [s x]
        (assoc s x (inc (s x 0)))
        )
      {}
      xs)
    ))

(defcheck solution-ecf7856c
  #(into {} (for [[k v] (group-by identity %)]
              [k (count v)])))

(defcheck solution-ed1d92d
  #(into {} (map (fn[[a b]] [a (count b)]) (group-by identity %))))

(defcheck solution-edf9b054
  (fn [col]
    (let [xs (group-by identity col)]
      (zipmap (keys xs) (map count (vals xs))))))

(defcheck solution-ee33f708
  (fn my-frequencies [s]
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} s)))

(defcheck solution-ee7320b1
  (fn [coll] (into {} (map #(vector (first %) (count %)) (partition-by identity (sort coll))))))

(defcheck solution-eebbcd7b
  #(let [b (group-by identity %)] (apply hash-map (mapcat (fn [[k v]] [k (count v)]) b )) ))

(defcheck solution-ef3f4534
  #(reduce (fn [a b] (assoc a b (inc (get a b 0)))) {} %))

(defcheck solution-efc28031
  #(->>
     (group-by identity %)
     (into [])
     (map (fn [vec] (let [[f s] vec] [f (count s)])))
     (into {})
     ))

(defcheck solution-f03181da
  #(reduce (fn [a e] (assoc a e (inc (get a e 0)))) {} %))

(defcheck solution-f166ea2d
  (fn count-indetities [col]
    (into {}
      (map #(vector (first %1) (count (second %1))) (group-by identity col)))))

(defcheck solution-f16e5ad8
  (comp (partial apply zipmap)
        (juxt keys (comp (partial map count) vals))
        (partial group-by identity)))

(defcheck solution-f1f473e4
  (fn distinctfrequencies [x]
    (let [update (fn [dict elem]
                   (if (get dict elem)
                     (assoc dict elem (inc (get dict elem)))
                     (assoc dict elem 1)))]
      (reduce update {} x))))

(defcheck solution-f25fbdff
  (fn [xs]
    (apply hash-map (mapcat #(list (key %) (count (val %)))
                      (group-by identity xs)))))

(defcheck solution-f2713238
  (fn [s]
    (into {} (map #(vector (first %) (count (second %))) (group-by identity s)))))

(defcheck solution-f2767daf
  (fn [c]
    (into {}
      (map (fn [[k v]]
             [k (count v)])
        (group-by identity c)))))

(defcheck solution-f2e12bd8
  reduce #(assoc %1 %2 (inc (get %1 %2 0))) {})

(defcheck solution-f370a919
  (fn [s]
    (into {}
      (for [[k v] (group-by identity s)] [k (count v)]))))

(defcheck solution-f3852c92
  reduce (fn [a x] (assoc a x (inc (get a x 0)))) {})

(defcheck solution-f3c753c9
  (fn
    [cs]
    (let [r (group-by identity cs)]
      (->> r
        vals
        (map count)
        (zipmap (keys r))))))

(defcheck solution-f3d07eed
  #(into {} (map (juxt first count)
              (partition-by identity (sort %)))))

(defcheck solution-f4028278
  (fn custom-frecuencies
    [coll]
    (reduce
      (fn [reduce-coll el]
        (let [current-size (reduce-coll el)
              new-size (if (nil? current-size) 1 (inc current-size))]
          (conj reduce-coll [el new-size])))
      {} coll)))

(defcheck solution-f4174982
  (fn [xs]
    (reduce (fn [counts x]
              (assoc counts x (inc (get counts x 0))))
      {}
      xs)))

(defcheck solution-f42a51f7
  (fn count-distinct [coll]
    (letfn [(count [hash-set key] (assoc hash-set key (inc (get hash-set key 0))))]
      (loop [hash {}
             remaining coll]
        (if (empty? remaining)
          hash
          (recur (count hash (first remaining)) (rest remaining)))))))

(defcheck solution-f45a5ef7
  (let [f
        (fn [m coll]
          (if (nil? coll)
            m
            (if (contains? m (first coll))
              (recur (conj m [(first coll) (+ 1 (get m (first coll)))]) (next coll))
              (recur (conj m [(first coll) 1]) (next coll))
              )
            )
          )]
    (fn [z] (f {} z))
    ))

(defcheck solution-f48e962d
  (fn [c]
    (reduce merge (for [group (partition-by identity (sort c))] {(first group) (count group)}))))

(defcheck solution-f4aa82cc
  (fn [xs]
    (into {}
      (for [[k v] (group-by identity xs)]
        {k (count v)}))))

(defcheck solution-f4b5dc35
  (fn [xs] (into {} (map #(do [(% 0) (count (% 1))]) (group-by identity xs)))))

(defcheck solution-f4b70b00
  (fn [xs]
    (let [m (group-by (fn [x] x) xs)]
      (zipmap (keys m) (map count (vals m))))))

(defcheck solution-f4fd334f
  (fn [xs]
    (apply hash-map
      (apply concat
        (map #(list % (count (filter #{%} xs)))
          (set xs)
          )
        )
      )
    ))

(defcheck solution-f56baebd
  #(loop [r {}
          c %1]
     (if (empty? c)
       r
       (recur (assoc r (first c) (inc (if (nil? (get r (first c)))
                                        0
                                        (get r (first c)))))
         (rest c)))))

(defcheck solution-f5b2b870
  (fn [col] (apply merge (map (fn [[k v]] {k (count v)}) (group-by identity col)))))

(defcheck solution-f5ed4fcd
  (fn [l] (reduce #(into %1 %2) (map #(let [[x l] %] {x (count l)}) (group-by identity l)))))

(defcheck solution-f68f7c36
  reduce #(assoc % %2 (inc (or (% %2) 0))) {})

(defcheck solution-f694c97d
  (fn [a]
    (let [col (group-by identity a)]
      (reduce #(assoc %1 %2 (count (get col %2))) {} (keys col)))))

(defcheck solution-f6e19d45
  #(loop [m {} c %]
     (if (empty? c)
       m
       (recur (assoc m (first c) (inc (get m (first c) 0))) (rest c)))))

(defcheck solution-f705e510
  (fn [coll]
    (into {} (map #(hash-map (key %) (count (val %))) (group-by identity coll)))))

(defcheck solution-f718ceab
  #(let [i (group-by identity %)]
     (loop [l (keys i), acc {}]
       (if-let [h (first l)]
         (recur (rest l) (conj acc [h (count (i h))]))
         acc
         ))))

(defcheck solution-f719ce8b
  (comp #(into {} (for [[k v] %] [k (count v)]))
        (partial group-by identity)))

(defcheck solution-f7507175
  (fn[a]
    (reduce
      (fn [x y] (merge y x))
      '{}
      (for [b  a]
        { b,  (reduce
                (fn[x y] (if (= y b) (inc x) x))
                0
                a
                )
         }
        )
      )
    ))

(defcheck solution-f7566c41
  (fn [coll]
    (reduce
      (fn [memo item]
        (assoc memo item (inc (memo item 0))))
      {}
      coll)))

(defcheck solution-f7583398
  (fn [xv]
    (reduce (fn [acc x]
              (if (contains? acc x)
                (assoc acc x (inc (acc x)))
                (assoc acc x 1)))
      {} xv)))

(defcheck solution-f77cc5ab
  (fn [xs] (reduce #(assoc %1 %2 (+ (get %1 %2 0) 1)) {} xs)))

(defcheck solution-f7a3d9c8
  (fn f [col]
    (loop [l col m {}]
      (if (empty? l) m
                     (let [f (first l)]
                       (recur (rest l) (assoc m f (inc (get m f 0)))))))))

(defcheck solution-f7ef2c
  #(->> (group-by identity %)
     (mapcat (fn [[k v]] [k (count v)]))
     (apply hash-map)))

(defcheck solution-f8671b3b
  #(into {} (map (fn [coll] [(first coll) (count coll)])
              (vals (group-by identity %)))))

(defcheck solution-f87b9437
  (fn [colls] (reduce  #(if (contains? %1 %2)
                          (assoc %1 %2 (inc (%1 %2)))
                          (assoc %1 %2 1))   {} colls)))

(defcheck solution-f891d569
  (fn [l] (apply hash-map (apply concat (map (fn [[i l]] [i (count l)])
                                          (group-by identity l))))))

(defcheck solution-f8c4ca0c
  reduce #(assoc %1 %2 (inc (get %1 %2 0))) '{})

(defcheck solution-f8f5d5a3
  (fn count-occurrences
    [c]
    (reduce #(assoc %1 (first %2) (count %2))  {} (partition-by identity (sort c)))))

(defcheck solution-f92670bc
  (fn [args]
    (apply merge (for [[k v] (group-by identity args)]
                   {k (count v)}))
    ))

(defcheck solution-f939e2c2
  (fn count-occurences [s]
    (let [p (partition-by identity (sort s))]
      (zipmap (map first p) (map count p)))))

(defcheck solution-f97252d2
  (fn [liste]
    (zipmap (set liste)
      (for [x (set liste)]
        (count (filter #(= x %) liste))
        )
      )
    ))

(defcheck solution-f9c4d869
  #(reduce (fn [m k] (assoc m k (inc (m k 0)))) {} %))

(defcheck solution-fa20c93a
  (partial reduce #(assoc % %2 (inc (get % %2 0))) {}))

(defcheck solution-fa3b71c2
  (fn [coll]
    (apply merge (map (fn [[k v]] {k (count v)}) (group-by identity coll)))))

(defcheck solution-fa9c877e
  (fn [coll]
    (reduce (fn [m x] (assoc m x (inc (get m x 0)))) {} coll)))

(defcheck solution-faff4c44
  #(into {} (map (fn [[a b]] [a (count b)]) (group-by identity %))))

(defcheck solution-fba2fb7a
  reduce #(assoc % %2 (+ 1 (get % %2 0))) {})

(defcheck solution-fbeda429
  (fn [xs]
    (->> (group-by identity xs)
      (mapcat (fn [[key vals]] [key (count vals)]))
      (apply hash-map))))

(defcheck solution-fc4e17dd
  (fn [s] (reduce #(update-in %1 [%2] (fnil inc 0)) {} s)))

(defcheck solution-fc618081
  (fn [s]
    (reduce #(merge-with + %1 {%2 1}) {} s)))

(defcheck solution-fcbffd0e
  (fn counter [coll]
    (->> coll
      (sort)
      (partition-by identity)
      (map #(vector (first %) (count %)))
      (apply concat)
      (apply hash-map))))

(defcheck solution-fcd74d6c
  reduce #(merge-with + {%2 1} %) {})

(defcheck solution-fcd94c26
  ; #(into {} (map (fn [[k v]] [k (count v)]) (group-by identity %)))

  reduce #(update-in %1 [%2] (fnil inc 0)) {})

(defcheck solution-fce9fe93
  (fn [coll]
    (reduce
      (fn [acc val]
        (if (get acc val)
          (update-in acc [val] #(inc %))
          (assoc acc val 1)))
      {}
      coll)))

(defcheck solution-fd166c99
  #(apply hash-map (interleave (sort (distinct %)) (map count (partition-by identity (sort %))))))

(defcheck solution-fd32e206
  #(into {} (map (fn [[k v]] {k (count v)}) (group-by (set (distinct %)) %))))

(defcheck solution-fd3e11f0
  (fn [lst]
    (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} lst)))

(defcheck solution-fdef3c3f
  #(into {} (for [[k v] (group-by identity %)]
              [k (count v)])))

(defcheck solution-fdfbb08
  (fn my-freq [coll]
    (zipmap
      (for [[k v] (group-by identity coll)] k)
      (for [[k v] (group-by identity coll)] (count v)))))

(defcheck solution-fe5da4f0
  (fn [in-s] (loop [rem-s in-s out-m {} current (first rem-s)]
               (if (nil? current) out-m
                                  (recur (rest rem-s)
                                    (assoc out-m current (if (nil? (get out-m current)) 1 (inc (get out-m current))))
                                    (second rem-s))))))

(defcheck solution-fed49546
  #(apply array-map (mapcat (fn [[k v]](list k (count v))) (group-by identity %))))

(defcheck solution-ffab5200
  #(into {}
     (map (fn [[a b]] (vector a (count b)))
       (group-by identity %))))

(defcheck solution-ffec09a4
  (fn [src]
    (apply hash-map (mapcat #(list (first(mapcat identity (val %))) (key %)) (group-by count (partition-by identity (sort src)))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-55))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
