(ns coal-mine.problem-63
  (:require [coal-mine.checks :refer [defcheck-63] :rename {defcheck-63 defcheck}]
            [clojure.test]))

(defcheck solution-104d43b6
  (fn [f coll] (loop [result {} vs coll]
                 (if (nil? vs) result
                               (let [this     (first vs)
                                     that     (next vs)
                                     this-key (f this)]
                                 (if (result this-key)
                                   (recur (assoc result this-key (conj (result this-key) this)) that)
                                   (recur (assoc result this-key [this]) that)
                                   )
                                 )
                               )
                 )
    ))

(defcheck solution-10595be7
  (fn [f coll]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {} coll)))

(defcheck solution-10bc4848
  (fn my-group-by [f s]
    (reduce #(let [k (f %2)]
               (assoc %1 k (conj (or (%1 k) []) %2))) {} s)))

(defcheck solution-10db9985
  (fn soln [f s]
    (if (= '() s)
      {}
      (let [m (soln f (rest s))
            v (f (first s))]
        (assoc m v (conj (m v) (first s)))))))

(defcheck solution-11868179
  (fn group
    [f s]
    (if (empty? s)
      []
      (merge-with concat {(f (first s)) [(first s)]} (group f (rest s))))))

(defcheck solution-11b72df9
  (fn gr [func s]
    (reduce #(assoc %1 (func %2) (conj (get %1 (func %2) []) %2)) {} s)))

(defcheck solution-11ba5ac4
  (fn [f x] (apply merge-with concat (for [t x] {(f t) [t]}))))

(defcheck solution-11bd5e89
  (fn [f coll]
    (reduce (fn [build val]
              (update-in build [(f val)] #(conj (or %1 []) val)))
      {}
      coll)))

(defcheck solution-139774dc
  (fn group-by'
    ([f xs] (group-by' f xs {}))
    ([f xs acc]
     (if (empty? xs) acc
                     (let [x0  (first xs),
                           fx0 (f x0),
                           p   #(= (f %) fx0)]
                       (recur f (remove p xs) (conj acc [fx0 (filter p xs)])))))))

(defcheck solution-143f70d5
  (fn gb [f s] (reduce #(update-in % [(f %2)] (fn [x] (vec (conj x %2)))) {} s)))

(defcheck solution-14b61b3b
  (fn [f coll]
    (reduce (fn [m e] (assoc m (first e) (second e))) {}
      (map #(vector (second (first %)) (map first %))
        (partition-by second
          (sort-by second
            (map #(vector % (f %)) coll)
            )
          )
        )
      )
    ))

(defcheck solution-14e1219b
  (fn my-group [f s]
    (let [groups
          (partition-by first (sort-by first (map (fn [item] [(apply f [item]) item]) s)))]
      (reduce merge
        (for [group groups]
          {(first (first group)) (reduce #(conj %1 (last %2)) [] group)})
        ))))

(defcheck solution-15012e55
  (fn [f s] (apply merge-with concat (map #(-> {(f %) [%]}) s))))

(defcheck solution-15215671
  (fn [f a] (loop [r {} i a] (if (empty? i) r (let [k (f (first i))] (recur (if (contains? r k) (assoc r k (conj (r k) (first i))) (assoc r k (vector (first i)))) (rest i)))))))

(defcheck solution-15471627
  (fn [f s]
    (into {}
      (map
        #(vector (f (first %)) (vec %))
        (partition-by f (sort s))))))

(defcheck solution-1575c113
  (fn [f s]
    (loop [m {}, n 0, l (for [i s] (list (f i) i)), c (count l)]
      (if (= c n)
        m
        (let [k (nth (nth l n) 0), v (nth (nth l n) 1)]
          (recur (assoc m k (conj (get m k []) v)) (inc n) l c))))))

(defcheck solution-15c7c966
  #(reduce (fn [m i] (let [k (% i)] (assoc m k (conj (get m k []) i)))) {} %2))

(defcheck solution-1653dc9d
  (fn grp

    ([f x]

     (grp f x (set (map f x))))

    ([f x s]

     (if (empty? s)

       {}

       (assoc-in (grp f x (drop 1 s)) [(first s)] (apply vector (filter (fn [a] (= (f a) (first s))) x)))))))

(defcheck solution-16d5f341
  (fn my-group-by3 [f s]
    (reduce #(merge-with concat %1 {(f %2) [%2]}) {} s)))

(defcheck solution-16ea0a8a
  (fn [f xs] (reduce #(assoc % (f %2) (conj (vec (% (f %2))) %2)) {} xs)))

(defcheck solution-16f700ba
  (fn [f c] (apply merge-with #(vec (concat %1 %2)) (map (fn [v] {(f v) [v]}) c))))

(defcheck solution-1721f18d
  (fn [f se]
    (apply merge-with concat (map #(hash-map (f %) [%]) se))))

(defcheck solution-177f606a
  (fn [fun coll]
    (into {}
      (map (fn [item]
             (vector (fun item)
               (filter #(= (fun %) (fun item))
                 coll))) coll))))

(defcheck solution-17934bc5
  (fn [f s]
    (reduce
      (fn [r [appliedf v]] (update-in r [appliedf] (comp vec conj) v))
      {}
      (map #(vector (f %) %) s))))

(defcheck solution-17d5e3d0
  (fn [f s]
    (reduce (fn [acc v]
              (merge-with
                concat
                acc
                {(f v) [v]}))
      {}
      s)))

(defcheck solution-1834ffac
  (fn [f s]
    (loop [s s
           r {}]
      (if (empty? s)
        r
        (let [s0      (first s)
              f-of-s0 (f s0)]
          (recur (rest s)
            (assoc r f-of-s0 (conj (r f-of-s0 []) s0))))))))

(defcheck solution-184a3ffc
  (fn g
    ([f s] (g f s {}))
    ([f s r]
     (if (empty? s)
       r
       (let [v (first s)
             k (f v)]
         (g f (rest s)
           (assoc r k (concat (r k) [v]))))))))

(defcheck solution-18bac37e
  (fn [f coll]
    (reduce
      (fn [m v]
        (let [k   (f v)
              acc (if (contains? m k) (conj (get m k) v) [v])]
          (assoc m k acc))) {} coll)))

(defcheck solution-18c77854
  (fn [f col]
    (reduce #(update-in %1 [(f %2)] (fn [v] (conj (or v []) %2)))
      {}
      col)))

(defcheck solution-1952e9a0
  (fn grp-by [f coll]
    (->> coll
      (map #(list (f %) %))
      (sort-by first)
      (partition-by first)
      (map (fn [l]
             (let [key    (first (first l))
                   values (map second l)]
               (vector key values))))
      (into {}))))

(defcheck solution-19656a7f
  (fn my-group-by [f coll]
    (reduce (fn [my-map v] (assoc my-map (f v) (conj (get my-map (f v) []) v))) {} coll)))

(defcheck solution-19c6709f
  (fn [f s]
    (reduce (fn [m x]
              (let [k (f x)]
                (assoc m k (conj (get m k []) x))))
      {} s)))

(defcheck solution-1a900a90
  (fn [f s] (apply merge-with concat (map (fn [i] {(f i) [i]}) s))))

(defcheck solution-1a9ee329
  (fn [f s] (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} s)))

(defcheck solution-1ae1ca4
  (fn [f coll]
    (reduce #(let [k (f %2)] (assoc %1 k (conj (get %1 k []) %2)))
      {} coll)))

(defcheck solution-1b1f9356
  (fn [f seq]
    (apply assoc {}
      (apply concat
        (for [k (set (map f seq))]
          [k (vec (filter #(= k (f %)) seq))])))))

(defcheck solution-1bcd0a52
  (fn [f s]
    (reduce #(let [k (f %2)] (assoc %1 k (conj (get %1 k []) %2))) {} s)))

(defcheck solution-1c1357a7
  (fn [f xs]
    (reduce (fn [acc v] (assoc acc (f v) (conj (get acc (f v) []) v)))
      {}
      xs)))

(defcheck solution-1c8e121f
  (fn my-group-by [f coll]
    (reduce
      (fn [acc v]
        (merge-with concat acc {(f v) [v]}))
      {}
      coll)))

(defcheck solution-1cc9c344
  (fn my-group-by [f coll]
    (reduce
      (fn [a b]
        (assoc a (f b) (vec (conj (get a (f b)) b)))) {} coll)))

(defcheck solution-1d0ccb79
  (fn [f coll]
    (let [g (map f coll)]
      (into {} (for [x (set g)] [x (filter #(= (f %) x) coll)])))))

(defcheck solution-1d609766
  (fn [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-1dc0b00a
  (fn [f s]
    (reduce
      #(let [k (f %2)]
         (assoc %1 k (conj (get %1 k []) %2)))
      {}
      s)))

(defcheck solution-1dd8a8cb
  (fn [f s] (reduce (fn [acc v] (let [k (f v) g (get acc k [])]
                                  (assoc acc k (conj g v)))) {} s)))

(defcheck solution-1dec6a1e
  ; Look at the power of Clojure. This is pretty strong Kung Fu
  #(apply merge-with into (for [v %2] {(% v) [v]})))

(defcheck solution-1e00ee02
  (fn gp [fun xs]
    (let [fun-vals (distinct (map fun xs))]
      (let [x-vals (map (fn [fun-val]
                          (filter (fn [x] (= (fun x) fun-val))
                            xs))
                     fun-vals)]
        (zipmap fun-vals x-vals)))))

(defcheck solution-1e048bf5
  (fn [f s]
    (loop [s      s
           answer {}]
      (if (nil? s) answer
                   (let [k (f (first s))]
                     (recur (next s) (if (get answer k) (assoc answer k (conj (get answer k) (first s)))
                                                        (assoc answer k [(first s)]))))))))

(defcheck solution-1ec56ee2
  (fn [f v] (zipmap
              (distinct (map f v))
              (for [x (distinct (map f v))]
                (filter #(= x (f %)) v)))))

(defcheck solution-1f39e192
  (fn [f xs]
    (reduce #(update-in %1 [(f %2)] conj %2)
      (into {} (map #(vector (f %) []) xs))
      xs)))

(defcheck solution-1fd73de5
  (fn [f s]
    (let [d  (map (fn [x] [(f x) x]) s)
          mk (zipmap (distinct (map first d)) (repeat []))]
      (reduce (fn [acc, e] (update-in acc [(first e)] conj (second e))) mk d))))

(defcheck solution-20856f
  (fn [f x] (reduce (fn [m v] (update-in m [(f v)] (fnil #(conj % v) []))) {} x)))

(defcheck solution-20918567
  (fn group-by* [f coll]
    (reduce #(update-in %1 [(f %2)] (fnil conj []) %2) {} coll)))

(defcheck solution-209737c1
  (fn my-groupby [f sq]
    (reduce
      (fn [accum val]
        (let [key (f val)
              grp (or (accum key) [])]
          (assoc accum key (conj grp val))))
      {} sq)))

(defcheck solution-209bdf3f
  (fn [f xs]
    (reduce
      (fn [h x]
        (assoc h (f x) (conj (h (f x) []) x)))
      {} xs)))

(defcheck solution-20d8c02a
  (fn [f s] (reduce #(merge-with concat % {(f %2) [%2]})
              {}
              s)))

(defcheck solution-2125b915
  #(reduce
     (fn [m e]
       (assoc m (% e) (conj (get m (% e) []) e)))
     {}
     %2))

(defcheck solution-212b528c
  (fn [f i] (reduce #(assoc %1 (f (first %2)) %2) {} (partition-by f (sort-by f i)))))

(defcheck solution-2132acbe
  (fn [f c]
    (let [r  (map #(vector (f %) %) c)
          ks (into #{} (map first r))]
      (into {} (for [k ks] (vector k (vec (map last (filter #(= k (first %)) r)))))))))

(defcheck solution-22487377
  (fn [f coll]
    (reduce (fn [r x] (assoc r (f x) (conj (get r (f x) []) x)))
      {} coll)))

(defcheck solution-227f5a3d
  (fn F1 [f xs]
    (let [ss (vec (set (map f xs)))]
      (apply assoc {}
        (interleave ss
          (for [s ss]
            (filter #(= s (f %)) xs))
          )))))

(defcheck solution-228eafec
  (fn [f s]
    (reduce
      (fn [m v]
        (let [key (f v)]
          (assoc m key (conj (m key []) v))))
      {}
      s)))

(defcheck solution-229d1c44
  (fn [f s] (apply merge (map (fn [v] (hash-map v (filter #(= (f %) v) s)))
                           (set (map f s))))))

(defcheck solution-22c900de
  (fn [f vals]
    (reduce (fn [items [k v]] (assoc items k (conj (get items k []) v))) {}
      (map #(vector (f %) %) vals))))

(defcheck solution-231cf77f
  #(
    (fn [hm f s]
      (if (empty? s)
        hm
        (let [x    (first s)
              k    (f x)
              v    (get hm k)
              coll (if (nil? v) [] v)
              ]
          (recur (assoc hm k (conj coll x)) f (rest s))
          )
        )
      ) {} %1 %2
    ))

(defcheck solution-23468d73
  (fn [f xs]
    (apply merge-with concat
      (for [i xs]
        {(f i) [i]}))))

(defcheck solution-23edc0fb
  (fn [fun coll] (apply (partial merge-with concat) (map hash-map (map fun coll) (map list coll)))))

(defcheck solution-241debad
  (fn [f s]
    (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} s)))

(defcheck solution-24e41ba1
  (fn [f ls]
    (reduce #(merge % %2)
      (set (map
             (fn [x]
               {(f x) (filter #(= (f x) (f %)) ls)})
             ls)))))

(defcheck solution-24e55479
  (fn
    [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-24f06dd4
  (fn [f vs]
    (reduce #(merge-with into % {(f %2) [%2]}) {} vs)))

(defcheck solution-251ca980
  (fn [f s]
    (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2))
      {} s)))

(defcheck solution-25a97b4
  (fn gby [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-25ede08d
  #(apply merge-with concat (for [e %2] {(% e) [e]})))

(defcheck solution-267e247c
  (fn [f coll]
    (reduce (fn [m e]
              (let [k  (f e)
                    vs (get m k [])]
                (assoc m k (conj vs e))))
      {}
      coll)))

(defcheck solution-26ab768d
  (fn [f coll]
    (let [x (map f coll)]
      (loop [k x v coll r {}]
        (cond
          (empty? k) r
          (contains? r (first k)) (recur (next k) (next v) (update-in r [(first k)] #(concat % [(first v)])))
          :else (recur (next k) (next v) (assoc r (first k) [(first v)])))))))

(defcheck solution-26f0e480
  (fn [f s]
    (let [M map
          u (set (for [v (M f s)] [v (filter #(= v (f %)) s)]))]
      (zipmap (M first u) (M second u)))))

(defcheck solution-27559897
  (fn my-group-by
    [f coll]
    (loop [retmap {}
           coll-p coll]
      (if (empty? coll-p)
        retmap
        (recur (let [value    (first coll-p)
                     keyvalue (f value)]
                 (if (contains? retmap keyvalue)
                   (update-in retmap [keyvalue] conj value)
                   (assoc retmap keyvalue (vector value)))) (rest coll-p))))))

(defcheck solution-27dfafc1
  (fn [f s]
    (reduce #(merge-with concat % {(f %2) [%2]})
      {} s)))

(defcheck solution-2804b641
  (fn group-by-e
    [f x]
    (if (empty? x) {}
                   (let [v    (f (first x))
                         this (filter #(= (f %) v) x)
                         that (filter #(not= (f %) v) x)]
                     (conj (group-by-e f that) (vector v this))))))

(defcheck solution-285697b6
  (fn [f s] (reduce #(update-in % [(f %2)] (comp vec conj) %2) {} s)))

(defcheck solution-2886ff83
  (fn [f c] (apply merge-with into (map (fn [x] {(f x) [x]}) c))))

(defcheck solution-28fbedc3
  (fn [fnc a-seq]
    (letfn [(vjoin [a-seq v] (conj (vec a-seq) v))]
      (first (last (take (inc (count a-seq))
                     (iterate (fn [[a-map a-seq]]
                                [(update-in a-map [(fnc (first a-seq))]
                                   vjoin (first a-seq)) (rest a-seq)])
                       [{} a-seq])))))))

(defcheck solution-297863f8
  #(zipmap (apply sorted-set (map % %2)) (partition-by % (sort-by % %2))))

(defcheck solution-29efa7a8
  (fn [f coll]
    (apply merge-with concat (map #(hash-map (f %) [%]) coll))))

(defcheck solution-2a779952
  (fn [a b] (reduce (fn [m e] (let [k (first e) v (last e)]
                                (update-in m [k] #(if (nil? %) (conj [] v) (conj % v)))))
              {} (map #(vector (a %) %) b))))

(defcheck solution-2ae2ed99
  (fn [f c]
    (into {} (for [s (set (map f c))]
               [s (filter #(= s (f %)) c)]))))

(defcheck solution-2af0b081
  #(->> (map (juxt % identity) %2)
     (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {})))

(defcheck solution-2b3f245f
  (fn [f s] (reduce #(if (get %1 (f %2)) (into %1 {(f %2) (conj (get %1 (f %2)) %2)}) (into %1 {(f %2) [%2]})) {} s)))

(defcheck solution-2bb07c78
  (fn [f s]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {}
      s)))

(defcheck solution-2c0e9c30
  #(loop [[x & xs] %2 res {}]
     (if (nil? x) res
                  (let [fx (%1 x)]
                    (recur xs (assoc res fx (conj (get res fx []) x)))))))

(defcheck solution-2c821be8
  (fn [f s]
    (let [vs (distinct (map f s))
          mv (fn [v] {v (filter #(= v (f %)) s)})
          ms (map mv vs)]
      (apply merge ms))))

(defcheck solution-2d02443
  (fn [f s]
    (loop [acc {}
           s   s]
      (if (empty? s)
        acc
        (recur (let [frst (first s)
                     res  (f frst)]
                 (if-let [k (acc res)]
                   (merge acc {res (conj k frst)})
                   (merge acc {res (vector frst)})))
          (rest s))))))

(defcheck solution-2d226a9
  (fn mygroupby [test coll]
    (reduce #(assoc % (test %2) (conj (get % (test %2) []) %2)) {} coll)))

(defcheck solution-2d2805df
  (fn [f x] (reduce #(let [k (f %2)] (assoc %1 k (conj (%1 k []) %2))) {} x)))

(defcheck solution-2d43da8
  (fn [f xs] (reduce #(assoc % (f %2) (conj (get % (f %2) []) %2)) {} xs)))

(defcheck solution-2d4bceb4
  (fn [f s]
    (reduce (fn [mp [v i]]
              (if (mp v)
                (assoc mp v (conj (mp v) i))
                (assoc mp v [i]))) {} (map vector (map f s) s))))

(defcheck solution-2d73be2e
  (fn [f s]
    (let [ke  (set (map f s))
          res (map #(vector %1 %2) (map f s) s)]
      (apply hash-map (mapcat identity
                        (for [a ke]
                          [a (vec (map second
                                    (filter #(= a (first %)) res)))]))))))

(defcheck solution-2dbe282f
  (fn f
    [grouper coll]
    (let [to-map (fn [map item]
                   (let [key     (grouper item)
                         key-seq (map key)]
                     (assoc map key (if (nil? key-seq) (vector item) (conj key-seq item)))))]
      (reduce to-map {} coll))))

(defcheck solution-2dd80975
  (fn [f ls]
    (into
      {}
      (map
        #(vector (f (first %)) %)
        (partition-by f (sort ls))))))

(defcheck solution-2dfbdb93
  #(apply merge-with concat (map (fn [x] {(%1 x) [x]}) %2)))

(defcheck solution-2e1293e7
  (fn my-group-by [f coll]
    (reduce
      (fn [acc [k v]] (assoc acc k (conj (get acc k []) v)))
      {}
      (map #(list (f %) %) coll))))

(defcheck solution-2e6b1c43
  (fn [f c]
    (reduce
      (fn [m i]
        (update-in m [(f i)] #(conj (or % []) %2) i))
      {} c)))

(defcheck solution-2e962870
  (fn [f l] (reduce #(assoc %1 (f %2) (vec (conj (get %1 (f %2)) %2))) {} l)))

(defcheck solution-2eb399c8
  (fn [f col]
    (loop [seq1 (map #(list (f %1) %1) col) seq2 {}]
      (if (empty? seq1)
        seq2
        (recur (rest seq1)
          (if (contains? seq2 (first (first seq1)))
            (assoc seq2 (first (first seq1)) (conj (get seq2 (first (first seq1))) (last (first seq1))))
            (assoc seq2 (first (first seq1)) (vector (last (first seq1)))))
          )
        )
      )
    ))

(defcheck solution-2ef66101
  (fn [f c] (reduce #(assoc % (f %2) (conj (vec (% (f %2))) %2)) {} c)))

(defcheck solution-2f92f473
  (fn [m lss] (loop [acc {} ls lss]
                (if (empty? ls)
                  acc
                  (recur (update-in acc [(m (first ls))] concat [(first ls)]) (rest ls))))))

(defcheck solution-2fa8c116
  (fn [f s] (zipmap (distinct (map #(f %) s)) (for [x (distinct (map #(f %) s))] (filter #(= x (f %)) s)))))

(defcheck solution-30fb4775
  (fn [f s]
    (apply merge-with concat
      (map (fn [a b] {a (list b)}) (map f s) s))))

(defcheck solution-31002727
  (fn my-group-by [f coll]
    (reduce (fn [ret x]
              (let [k (f x)]
                (assoc ret k (conj (get ret k []) x))))
      {}
      coll)))

(defcheck solution-31154037
  (fn my-group-by [f s]
    (loop [s s m {}]
      (if (empty? s) m
                     (let [x (first s) k (f x)]
                       (recur
                         (rest s)
                         (assoc m k (conj (get m k []) x))))))))

(defcheck solution-31580ad
  (fn [f s]
    (apply merge-with concat
      (for [x s]
        {(f x) (vector x)}))))

(defcheck solution-31b2cb5c
  (fn [func coll]
    (reduce
      #(let [val (func %2)] (assoc % val (conj (get % val []) %2)))
      {} coll)))

(defcheck solution-31bd9874
  (fn [x y] (reduce #(merge-with concat % {(x %2) [%2]}) {} y)))

(defcheck solution-320d5fb2
  #(apply merge-with concat
     (map (comp (partial apply hash-map) (juxt %1 list)) %2)))

(defcheck solution-324ecffa
  (fn [pred xs]
    (let [results (distinct (map pred xs))]
      (into {}
        (map vector results
          (map
            (fn [result]
              (filter #(= result (pred %)) xs)
              ) results
            )
          )
        )
      )
    ))

(defcheck solution-327af3a3
  (letfn [
          (my-group-by [f s] (reduce (fn [a [k v]] (assoc a k (conj (a k []) v))) {} (kv-sequence f s)))
          (kv-sequence [f s] (map #(list (f %) %) s))]
    my-group-by))

(defcheck solution-32d5aaa5
  (fn [f xs] (reduce #(update-in %1 [(f %2)] (comp vec conj) %2) {} xs)))

(defcheck solution-3304045c
  (fn [f c]
    (let [p (partition-by f (sort-by f c))]
      (zipmap (map #(f (first %)) p) p))))

(defcheck solution-330dfa93
  (fn [f lst]
    (apply merge-with concat
      (for [x lst]
        {(f x) [x]}
        )
      )
    ))

(defcheck solution-339437f
  ;(fn [f, s]
  ;     (loop [cur s, valdict {}]
  ;       (if (seq cur)
  ;         (let [h (first cur),
  ;               val (f h)]
  ;           (recur (rest cur)
  ;                  (assoc valdict val
  ;                         (concat (valdict val) [h]))))
  ;         valdict)))

  (fn [pred s]
    (->> s
      (map (fn [x] {(pred x) [x]}))
      (apply (partial merge-with concat)))))

(defcheck solution-33b43614
  (fn [f c]
    (loop [o  {}
           ks (distinct (map f c))
           s  c]
      (if (empty? ks)
        o
        (let [k (first ks)
              v (filter #(= (f %) k) s)]
          (recur (conj o [k v])
            (rest ks)
            (remove (set v) s)))))))

(defcheck solution-340da7ab
  (fn ! [f sq]
    (loop [s sq m (zipmap (distinct (map f (map first (partition-by f sq)))) (repeat []))]
      (if (empty? s) m
                     (let [e (first s) er (f e)]
                       (recur (rest s) (assoc m er (conj (get m er) e))))))))

(defcheck solution-3447b250
  (fn grpby [f s]
    (reduce (fn [m [k & v]]
              (assoc-in m [k] (vec (concat (m k) v))))
      {}
      (for [el s] [(f el) el]))))

(defcheck solution-3495b805
  (fn [f coll]
    (reduce (fn [r v]
              (let [k (f v)]
                (if (r k)
                  (conj r [k (conj (r k) v)])
                  (conj r [k [v]]))))
      {}
      coll)))

(defcheck solution-34c6199e
  #(let [a (vec (interleave (map %1 %2) %2))]
     (loop [i 0 m {}]
       (if (< i (count a))
         (if (contains? m (a i))
           (recur (+ i 2) (assoc m (a i) (conj (m (a i)) (a (+ 1 i)))))
           (recur (+ i 2) (assoc m (a i) [(a (+ 1 i))]))
           )
         m))))

(defcheck solution-34c64957
  #(apply merge-with concat (for [x %2] {(%1 x) [x]})))

(defcheck solution-354bcfc7
  (fn [f s] (reduce (fn [m v] (let [k (f v) {cv k} m] (assoc m k (if cv (conj cv v) [v])))) {} s)))

(defcheck solution-35728a02
  ;(fn [f xs] (reduce (fn [m x] (update-in m [(f x)] #(if % (conj % %2) [%2]) x)) {} xs))
  (fn [f xs] (reduce #(let [k (f %2) v (% k)] (assoc % k (if v (conj v %2) [%2]))) {} xs)))

(defcheck solution-35c4cbb2
  (fn [f coll]
    (reduce #(merge-with into % {(f %2) [%2]}) {} coll)))

(defcheck solution-35e17e57
  (fn [f s]
    (reduce
      (fn [result x]
        (update-in result [(f x)] (fnil conj []) x))
      {} s)))

(defcheck solution-35ea103e
  (fn my-group-by [f s]
    (let [ks  (set (map f s))
          bag (into {} (map #(hash-map % []) ks))]
      (reduce #(assoc % (f %2) (conj (% (f %2)) %2)) bag s))))

(defcheck solution-362cfe00
  (fn [ff v]
    (loop [ans {} vv v]
      (if (empty? vv)
        ans
        (let [kk     (ff (first vv))
              value  (if (contains? ans kk) (get ans kk) [])
              newans (assoc ans kk (conj value (first vv)))]
          (recur newans (next vv)))))))

(defcheck solution-363f386d
  (fn [p col]
    (reduce #(update-in %1 [(p %2)] (fn [v] ((fnil conj []) v %2))) {} col)))

(defcheck solution-36443d48
  (fn group-by' [f coll]
    (reduce (fn [h v]
              (let [k (f v)]
                (assoc h k (conj (or (h k) []) v))))
      {}
      coll)))

(defcheck solution-36526003
  #(apply merge-with concat
     (map (fn [x] {(%1 x) [x]}) %2)))

(defcheck solution-3691a5ab
  (fn my-group-by [f coll]
    (apply merge-with concat
      (map #(hash-map (f %) [%]) coll))))

(defcheck solution-36b2f701
  #(reduce
     (fn [m v]
       (assoc
        m
         (% v)
         (conj
           (get m (% v) [])
           v
           )
         )
       )
     {}
     %2))

(defcheck solution-3707b6dd
  (fn grp-seq [func vals]
    (into {}
      (map #(vector (func (first %)) (vec %))
        (partition-by func (sort vals))))))

(defcheck solution-37789c52
  (fn [f s]
    (let [pairs   (map (fn [x] [x (f x)]) s)
          results (map last pairs)]
      (into {} (map (fn [result] [result (map first
                                           (filter #(= (last %) result)
                                             pairs))])
                 results)))))

(defcheck solution-379c871
  (fn [f coll]
    (reduce
      #(let [k (f %2)]
         (assoc %1 k (conj (%1 k []) %2)))
      {} coll)))

(defcheck solution-37af9072
  (fn [f s] (reduce
              (fn [a b] (let [c (f b)
                              k (get a c)]
                          (assoc
                           (if (nil? k) a (dissoc a c))
                            c
                            (if (nil? k) [b] (conj k b)))))
              {} s)))

(defcheck solution-37b25e89
  (fn [f v]
    (reduce #(update-in %1 [(f %2)]
               (fn [a b] (if (nil? a) (vector b) (conj a b))) %2)
      {} v)))

(defcheck solution-37c451ff
  (fn [pred s]
    (apply merge-with
      concat
      (map #(hash-map (pred %) [%])
        s))))

(defcheck solution-37e98dc5
  (fn [f m] (reduce #(assoc % (f %2) (conj (% (f %2) []) %2)) {} m)))

(defcheck solution-38762f8
  (fn group-by-1 [f coll]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {}
      coll)))

(defcheck solution-38ae3286
  (fn [pred coll] (reduce
                    #(let [item (pred %2)]
                       (if-not (contains? %1 item)
                         (assoc %1 item [%2])
                         (update-in %1 [item] conj %2))) {} coll)))

(defcheck solution-38e1fa16
  (fn
    [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-39591346
  #(loop [l %2 r {}]
     (if (empty? l)
       r
       (let [h (first l) x (% h)]
         (recur (rest l) (assoc r x (conj (r x []) h)))
         ))))

(defcheck solution-398824de
  (fn [f s]
    (letfn [(updatemap [m k v]
              (if (contains? m k)
                (assoc m k (vec (concat (m k) (vector v))))
                (assoc m k (vector v))))
            ]
      (reduce #(updatemap %1 (f %2) %2) {} s))))

(defcheck solution-39be041b
  (fn [kf s]
    (reduce (fn [m x]
              (let [k (kf x)]
                (if (contains? m k)
                  (update-in m [k] conj x)
                  (assoc m k [x]))))
      {} s)))

(defcheck solution-39cebb8e
  (fn [f s]
    (reduce
      (fn [acc v]
        (assoc acc (f v) (conj (acc (f v) []) v)))
      {}
      s)))

(defcheck solution-39ef7963
  (fn [f a] (apply (partial merge-with concat) (map #(hash-map (f %) (vector %)) a))))

(defcheck solution-3a17975f
  (fn [f c]
    (loop [c c
           m {}]
      (if (empty? c)
        m
        (let [x (first c)
              y (f x)]
          (recur (rest c) (assoc m y (conj (get m y []) x))))))))

(defcheck solution-3a6146a0
  (fn [f coll]
    (reduce (fn [m x]
              (let [y (f x)]
                (assoc m y (conj (m y []) x))))
      {} coll)))

(defcheck solution-3b07f86a
  (fn grouper [f coll]
    (let [inner-grouper (fn [f coll acc]
                          (if (empty? coll)
                            acc
                            (let [new-key (f (first coll))
                                  new-val (first coll)]
                              (recur f (rest coll) (assoc acc new-key (conj (get acc new-key []) new-val))))))]
      (inner-grouper f coll '{}))))

(defcheck solution-3b1b18d6
  (fn [f s]
    (loop [vals s, kys (map f s), h (hash-map)]
      (let [v (first vals), k (first kys)]
        (cond (empty? vals) h
              (nil? (h k)) (recur (rest vals) (rest kys) (assoc h k (vector v)))
              :else (recur (rest vals) (rest kys) (assoc h k (conj (h k) v))))))))

(defcheck solution-3b526029
  (fn groupby [f s]
    (reduce
      (fn [m e]
        (let [k (f e)] (assoc m k (conj (get m k []) e))))
      {} s)))

(defcheck solution-3b7fbdaf
  (fn [f coll]
    (reduce (fn [result item]
              (let [key (f item)]
                (assoc result key (conj (get result key []) item))))
      {}
      coll)))

(defcheck solution-3ba7c719
  (fn [f coll]
    (letfn [(build-groups [groups arg]
              (let [val           (f arg)
                    group         (get groups val [])
                    updated-group (conj group arg)]
                (conj groups [val updated-group])))]
      (reduce build-groups {} coll))))

(defcheck solution-3bf6f7f3
  (fn [f coll]
    (reduce (fn [a v]
              (let [result (f v)
                    currec (get a result [])]
                (assoc a result (conj currec v))))
      {} coll)))

(defcheck solution-3c2afc32
  (fn [f list]
    (reduce (fn [agg it]
              (assoc agg
                (f it)
                (conj (vec (get agg (f it))) it))) {} list)))

(defcheck solution-3cb39509
  (comp #(zipmap (map ffirst %) (map (partial map peek) %)) (partial partition-by first) sort (fn [f x] (map-indexed #(vector (f %2) %1 %2) x))))

(defcheck solution-3cf3902
  (fn [f s]
    (->> s
      (sort-by f)
      (partition-by f)
      (reduce
        (fn [result group] (conj result [(f (first group)) group]))
        [])
      (into {}))))

(defcheck solution-3d03ae85
  (fn [f s] (let [res (for [x s] [(f x) x])
                  fnc (fn [m [k v]] (assoc m k (conj (get m k []) v)))]
              (reduce fnc {} res))))

(defcheck solution-3d26c6af
  (letfn
   [(func [f]
      (fn [mapp v]
        (let [k (f v)]
          (assoc mapp k (concat (mapp k) [v])))))]
    (fn [f coll]
      (reduce (func f) {} coll))))

(defcheck solution-3d4d52bd
  (fn [f se]
    (reduce (fn [h v]
              (update-in h [(f v)] #(conj (or % []) v)))
      {} se)))

(defcheck solution-3dcf88cc
  #(let [f % s %2] (persistent! (reduce (fn [rf rs] (let [k (f rs)] (assoc! rf k (conj (get rf k []) rs)))) (transient {}) s))))

(defcheck solution-3f3d315d
  (fn group-a-sequence [pf c]
    (reduce

      (fn [a x]
        (let [k (pf x)
              v (a k)]
          (if-not (nil? v)
            (assoc a k (conj v x))
            (assoc a k [x]))))

      {}
      c)))

(defcheck solution-3f5181e9
  (fn [f lst]
    (loop [A lst, result {}]
      (if (empty? A)
        result
        (let [a (first A), b (f a)]
          (recur (rest A)
            (if (contains? result b)
              (update-in result [b] #(conj % a))
              (assoc result b [a]))))))))

(defcheck solution-3f7943f
  (fn [f c]
    (reduce #(update-in % [(f %2)] (comp vec into) [%2]) {} c)))

(defcheck solution-3f803f68
  (fn my-group-by [f coll]
    (reduce
      #(if (contains? %1 (f %2))
         (assoc %1 (f %2) (conj (%1 (f %2)) %2))
         (assoc %1 (f %2) [%2]))
      {}
      coll)))

(defcheck solution-3fc9f2dd
  (fn [f xs]
    (reduce #(assoc %1 (f %2) (vec (conj (get %1 (f %2)) %2))) {} xs)
    ))

(defcheck solution-40334c06
  (fn [f s]
    (apply merge-with concat
      (map (fn [x] {(f x) [x]}) s))))

(defcheck solution-4039c9eb
  (fn [f s]
    (reduce (fn [acc x]
              (let [y (f x)]
                (if (contains? acc y)
                  (update-in acc [y] conj x)
                  (assoc acc y [x]))))
      {}
      s)))

(defcheck solution-40b5ed75
  (fn my-group [f s]
    (reduce (fn [groups x]
              (let [existing (or (get groups (f x)) [])]
                (assoc groups (f x) (conj existing x))))
      {}
      s)))

(defcheck solution-40d3ea20
  (fn [fun args] (reduce
                   ((fn [f] (fn [acc arg]
                              (let [result (f arg)
                                    prev   (or (acc result) [])]
                                (assoc acc result (conj prev arg))))) fun) {} args)))

(defcheck solution-419bf731
  (fn [f coll]
    (apply
      merge-with
      concat
      (map
        #(assoc {} (f %) [%])
        coll))))

(defcheck solution-41f3ea97
  (fn [f xs]
    (reduce
      (fn [m x]
        (let [k (f x) v (or (m k) [])]
          (assoc m k (conj v x))))
      {}
      xs)))

(defcheck solution-41f7bdd6
  (fn [f l] (reduce #(update-in % [(f %2)] (fnil conj []) %2) {} l)))

(defcheck solution-4219aacc
  (fn [f s]
    (reduce #(let [k (f %2)] (assoc %1 k (conj (get %1 k []) %2))) {} s)))

(defcheck solution-425f5b1
  (fn [f s]
    (into {} (map #(vector (f (first %)) %) (partition-by f (sort-by f s))))))

(defcheck solution-42650446
  (fn [f xs]
    (reduce (fn [result x]
              (update-in result [(f x)] (fnil #(conj % x) [])))
      {} xs)))

(defcheck solution-427bfc0a
  #(reduce (fn [m item]
             (let [k (%1 item)
                   v (get m k [])]
               (assoc m k (conj v item)))) {} %2))

(defcheck solution-42f9a39b
  (fn [f s]
    (reduce (fn [m a]
              (let [x (f a)]
                (assoc m x (conj (get m x []) a)))) {} s)))

(defcheck solution-43418d56
  (fn group- [f coll]
    "63. Given a function f and a sequence s, write a function which
  returns a map."
    (if-let [s (seq coll)]
      (let [m (group- f (rest s))
            v (first s)
            k (f v)
            l (get m k '())]
        (assoc m k (conj l v)))
      (hash-map))))

(defcheck solution-43705518
  #(loop [e (first %2)
          l (next %2)
          a {}]
     (let [n (%1 e)
           a (into a {n (conj (vec (a n)) e)})]
       (if l
         (recur (first l) (next l) a)
         a))))

(defcheck solution-43a4bb1b
  (fn [f s]
    (let [my-conj #(conj (or %1 []) %2)]
      (reduce #(update-in %1 [(f %2)] my-conj %2) {} s))))

(defcheck solution-442f101a
  (fn [f xs]
    (reduce (fn [m x]
              (let [k (f x)]
                (assoc m k (conj (get m k []) x))))
      {}
      xs)))

(defcheck solution-447c0311
  (fn [f s]
    (apply merge-with concat (map #(hash-map (f %) [%]) s))
    ))

(defcheck solution-44806d97
  (fn [f coll]

    (let [x (fn [c a] (assoc c (f a) (conj (get c (f a) []) a)))]

      (reduce x {} coll))))

(defcheck solution-44ee783f
  (fn [f s]
    (let [gather (fn [res x]
                   (let [fx (f x) xs (res fx)]
                     (assoc res fx (concat xs [x]))))]
      (reduce gather {} s))))

(defcheck solution-44ee8b89
  (fn [f s]
    (reduce
      (fn [m v]
        (let [x (f v)]
          (update-in m [x] #(conj (if % % []) v))))
      {}
      s)))

(defcheck solution-45357dca
  #(reduce (fn [acc e]
             (assoc acc (% e)
                        (conj (acc (% e) []) e))) {} %2))

(defcheck solution-4552b43
  (fn [fcn lst]

    (reduce #(assoc %1 (fcn %2) (sort (cons %2 (%1 (fcn %2))))) {} lst)))

(defcheck solution-45728445
  (fn
    [f xs]

    (let
     [
      pairs
      (map
        (fn [v]
          [
           (f v)
           v
           ]
          )
        xs
        )
      keys
      (set
        (map first pairs)
        )
      ]
      (apply hash-map
        (mapcat
          (fn [k]
            [
             k
             (apply vector
               (map second
                 (filter
                   (fn [p]
                     (= (first p) k)
                     )
                   pairs
                   )
                 )
               )
             ]
            )
          keys
          )
        )
      )
    ))

(defcheck solution-4598682b
  (fn [f x]
    (apply merge-with concat (map #(hash-map (f %) [%]) x))))

(defcheck solution-45989e13
  (fn my-group-by
    [f s]
    (reduce
      #(let [k (f %2)] (assoc % k (conj (get % k []) %2)))
      {} s)))

(defcheck solution-4622ebe4
  (fn [f s] (let [v (partition 2 (interleave (map f s) s))
                  d (distinct (map #(first %) v))
                  g (map (fn [m] (vec (map #(last %) (filter (fn [k] (= (first k) m)) v)))) d)
                  ]
              (zipmap d g))))

(defcheck solution-4645ba1b
  (fn [f s]
    (reduce (fn [r v] (let [add #((fnil conj []) %1 %2)]
                        (update-in r [(f v)] add v))) {} s)))

(defcheck solution-46566e54
  (fn __ [f s]
    (apply (partial merge-with concat) (map #(hash-map (f %) [%]) s))))

(defcheck solution-466ba5b9
  (fn [f coll]
    (reduce (fn [m x]
              (update-in m [(f x)] #(if (nil? %) [x] (conj % x))))
      {} coll)))

(defcheck solution-46ed6188
  #(reduce (fn [m e] (assoc m (% e) `[~@(m (% e)) ~e]))
     {} %2))

(defcheck solution-475d09b6
  (fn [f s]
    (apply merge-with
      concat (map #(hash-map (f %) (vector %)) s))))

(defcheck solution-478bdf5f
  ;;#(reduce (fn [m el]
  ;;           (let [lookup (%1 el)
  ;;                 existing (get m lookup [])
  ;;                 updated (conj existing el)]
  ;;             (assoc m lookup updated)))
  ;;         {}
  ;;         %2)

  #(reduce (fn [m el]
             (update-in m [(%1 el)] (fnil conj []) el))
     {}
     %2))

(defcheck solution-4854b33d
  (fn [f s]
    (loop [key-coll (map f s)
           s        s
           result   {}]
      (if (empty? s)
        result
        (let [akey       (first key-coll)
              value      (first s)
              vec-val    (get result akey)
              new-val    (if vec-val (conj vec-val value) (vector value))
              new-result (assoc result akey new-val)]
          (recur
            (rest key-coll)
            (rest s)
            new-result))))))

(defcheck solution-48afb62c
  (fn g [f l]
    (if
     (= l ()) {}
              (let [a (g f (rest l))
                    i (first l)
                    x (f i)]
                (assoc a x (cons i (get a x ())))))))

(defcheck solution-48bc7548
  (fn [f l]
    (reduce
      #(update-in %1 [(f %2)] (fnil conj []) %2)
      {}
      l
      )
    ))

(defcheck solution-48db84f7
  (fn [f coll]
    (let [s (set (map f coll))]
      (zipmap s
        (map #(vec
                (filter (fn [x] (= % (f x)))
                  coll))
          s)))))

(defcheck solution-48fd12a4
  (fn [x z] (let [y (sort-by x z)] (into {} (map (juxt #(x (first %)) identity) (partition-by x y))))))

(defcheck solution-498f7878
  (fn [f coll]
    (reduce (fn [m x] (assoc m (f x)
                               (conj (m (f x) []) x)))
      {}
      coll)))

(defcheck solution-49ca5377
  #(reduce (fn [m i] (conj m [(%1 i) (conj (m (%1 i) []) i)])) {} %2))

(defcheck solution-49e75c89
  (fn [f s] (reduce #(assoc % (f %2) (vec (conj (get % (f %2)) %2))) {} s)))

(defcheck solution-49f5230f
  (fn by-group
    [f s]
    (let [sorted     (sort-by f s)
          groups     (partition-by f sorted)
          group-maps (map #(hash-map ((comp f first) %) (vec %)) groups)]
      (reduce conj group-maps))))

(defcheck solution-4a0b4a90
  #(apply (partial merge-with into) (for [x %2] {(% x) [x]})))

(defcheck solution-4a8d599e
  (fn [f seq]
    (reduce
      (fn [acc e] (let [v (f e)]
                    (if (contains? acc v)
                      (conj acc [v (conj (acc v) e)])
                      (conj acc [v [e]]))))
      {} seq)))

(defcheck solution-4aa7dae0
  (fn
    [f s]
    (letfn [(addTo [m k v] (assoc m k (conj (m k []) v)))]
      (reduce #(addTo %1 (f %2) %2) {} s))))

(defcheck solution-4b2a5dfa
  (fn mgb [f s]
    (let [p (set (map f s))]
      (apply hash-map (interleave p (map (fn [z] (vec (filter #(not (nil? %)) (map #(if (= z (f %)) %) s)))) p))))))

(defcheck solution-4b476b53
  (fn my-group-by [f s]
    (reduce (fn [acc i] (update-in acc [(f i)] #(into [] (conj %1 %2)) i)) {} s)))

(defcheck solution-4b545f49
  (fn custom-group-by
    [f coll]
    (reduce
      (fn [result-map e]
        (let [to-key (f e)
              to-vec (or (result-map to-key) [])]
          (assoc result-map to-key (conj to-vec e))))
      {} coll)))

(defcheck solution-4b660b66
  (fn [f y] (reduce (fn [a [k x]] (assoc a k (conj (or (a k) []) x))) {} (map (fn [x] [(f x) x]) y))))

(defcheck solution-4c1de6e1
  (fn gsr [f s] (if (empty? s) {}
                               (let [first-value (first s)
                                     rest-values (rest s)
                                     rest-gsr    (gsr f rest-values)
                                     apply-f     (f first-value)
                                     current-V   (rest-gsr apply-f [])
                                     new-V       (cons first-value current-V)
                                     new-Map     (assoc rest-gsr apply-f new-V)
                                     ]
                                 new-Map
                                 ))))

(defcheck solution-4c8a9fa4
  (fn [f s]
    (reduce
      (fn [map x]
        (let [key (f x)]
          (assoc map key
                     (conj (get map key []) x))))
      {} s)))

(defcheck solution-4c9ccb27
  (fn [f coll]
    (reduce (fn [a b]
              (update-in a [(f b)] (fnil conj []) b))
      {}
      coll)))

(defcheck solution-4cb0040
  (fn [f coll]
    (reduce (fn [res i]
              (let [x (f i)]
                (assoc res x (conj (res x []) i))))
      {}
      coll)))

(defcheck solution-4cbccf3f
  #(apply
     (partial merge-with concat {})
     (map (fn [x] {(% x) [x]}) %2)))

(defcheck solution-4cc20f10
  (fn grp-by
    ([f coll]
     (grp-by {} f coll))
    ([m f coll]
     (if (empty? coll)
       m
       (let [x     (first coll)
             f-x   (f x)
             v     (m f-x)
             new-v (if (nil? v) (vector x) (conj v x))]
         (grp-by (assoc m f-x new-v) f (rest coll)))))))

(defcheck solution-4d0b6b4d
  #(reduce (fn [m v] (assoc m (% v) (conj (m (% v) []) v))) {} %2))

(defcheck solution-4d5e0102
  (fn [f coll]
    (persistent!
      (loop [result (transient {})
             input  coll
             item   (f (first input))]
        (if (empty? input) result
                           (recur (assoc! result item
                                    (conj
                                      (get result item []) (first input)))
                             (rest input)
                             (if (empty? (rest input)) nil (f (second input)))))))))

(defcheck solution-4dbbb2b4
  (fn [f s]
    (loop [tail s, m {}]
      (if-let [[x & xs] (seq tail)]
        (let [f-result (f x)]
          (recur xs (assoc m f-result (conj (get m f-result []) x))))
        m))))

(defcheck solution-4e098bbe
  (fn [f s]
    (reduce (fn [m x] (if (contains? m (f x)) (assoc m (f x) (conj (m (f x)) x)) (assoc m (f x) [x]))) {} s)
    ))

(defcheck solution-4e5591
  (fn [f l]
    (let [ks   (set (map f l))
          mmap (fn [acc ks l f]
                 (if (empty? ks) acc
                                 (recur (assoc acc (first ks) (filter #(= (first ks) (f %)) l))
                                   (rest ks) l f)))]
      (mmap {} ks l f))))

(defcheck solution-4e55bcc6
  (fn [f coll]
    (loop [x coll
           r {}]
      (if (nil? (first x))
        r
        (if (nil? (get r (f (first x))))
          (recur (rest x) (assoc r (f (first x)) (vector (first x))))
          (recur (rest x) (assoc r (f (first x)) (concat (get r (f (first x))) (vector (first x))))))))))

(defcheck solution-4e6f5a18
  (fn [f s]
    (reduce #(let [f-val       (f %2)
                   prev-params (get % f-val nil)]
               (if (nil? prev-params)
                 (assoc % f-val [%2])
                 (assoc % f-val (conj prev-params %2))))
      {} s)))

(defcheck solution-4f0b3ece
  (fn [f c]
    (reduce
      #(let [res (f %2) l (% res [])]
         (conj % {res (conj l %2)}))
      {}
      c)))

(defcheck solution-4f2d7e80
  (fn [f s]
    (reduce
      (fn [m v]
        (let [k (f v)]
          (assoc m k (conj (get m k []) v))))
      {} s)))

(defcheck solution-4f5fed88
  (fn [f x]
    (apply merge-with concat
      (map
        (fn [y] {(f y) [y]})
        x
        )
      )
    ))

(defcheck solution-4f7affad
  (fn [f s]
    (reduce (fn [a b] (merge-with #(concat %1 %2) a b))
      (map hash-map (map f s) (map list s)))))

(defcheck solution-4fda565f
  (fn my-group [f coll]
    (reduce (fn [ret x]
              (let [k (f x)]
                (assoc ret k (conj (get ret k []) x))))
      {} coll)))

(defcheck solution-507e47d2
  (fn [f xs]
    (letfn [(addtomap [xs x]
              (assoc xs (f x) (conj (vec (xs (f x))) x)))]
      (reduce addtomap {} xs))))

(defcheck solution-50b1c0fc
  (fn group-seq [f s]
    (let [kys (distinct (map f s))
          vls (for [x kys
                    y s
                    :when (= (f y) x)]
                y)]
      (zipmap kys (partition-by f vls)))))

(defcheck solution-516713d7
  (fn [f coll]
    (reduce (fn [acc item]
              (let [result (f item)]
                (if (not (contains? acc result))
                  (assoc acc result [item])
                  (assoc acc result (conj (acc result) item))))) {} coll)))

(defcheck solution-5182591b
  #(reduce (fn [m [k v]] (assoc m k (conj (m k []) v))) {} (map (juxt %1 identity) %2)))

(defcheck solution-5193d500
  (fn [f xs]
    (reduce
      (fn [m x] (update-in m [(f x)] #(conj (or % []) x)))
      {}
      xs)))

(defcheck solution-523000e1
  (fn [f s] (reduce (fn [a [k v]] (assoc a k (conj (a k []) v))) {} (map vector (map f s) s))))

(defcheck solution-525626b7
  (fn [f coll]
    (reduce
      (fn [result v]
        (let [key (f v)]
          (assoc result key (conj (vec (get result key)) v))))
      {}
      coll)))

(defcheck solution-52a5e450
  (fn [f seq]
    (reduce
      (fn [ret [key val]]
        (let [temp (ret key)]
          (assoc ret key
                     (if (nil? temp)
                       [val]
                       (conj temp val)))))
      {} (map vector (map f seq) seq))))

(defcheck solution-52adb6de
  (fn [f s]
    (let [v (distinct (map f s))]
      (apply assoc {}
        (apply concat
          (for [x v] [x (filter #(= (f %) x) s)]))))))

(defcheck solution-532a7b2e
  (fn [f xs]
    (reduce
      (fn [m e]
        (let [k (f e) curval (get m k []) newval (conj curval e)]
          (assoc m k newval)))
      {}
      xs)))

(defcheck solution-532d1380
  (fn [f ls]
    (reduce
      (fn [acc x]
        (if (contains? acc (f x))
          (assoc acc (f x) (conj (get acc (f x)) x))
          (assoc acc (f x) (conj [] x))))
      {}
      ls)))

(defcheck solution-538f354b
  (fn [f s] (reduce (fn [m i]
                      (let [k (f i)]
                        (assoc m k (if (m k) (conj (m k) i) [i]))))
              {} s)))

(defcheck solution-5398cd4e
  (fn
    [f s]
    (let* [coll    (map #(vector % (f %)) s)
           results (distinct (map second coll))]
      (into {} (for [k results]
                 [k (map first (filter #(= (second %) k) coll))])))))

(defcheck solution-53a60165
  (fn [f s]
    (let [x (map #(vector (f %) %) s)]
      (reduce (fn [s [k v]] (assoc s k (conj (get s k []) v))) {} x))))

(defcheck solution-53c2b891
  #(reduce (fn [val n] (assoc val (%1 n) (vec (conj (get val (%1 n)) n)))) {} %2))

(defcheck solution-53d95232
  #(reduce (fn [a b] (merge-with concat a {(% b) [b]})) {} %2))

(defcheck solution-53e148d
  (fn [fc l]
    (loop [l l r {}]
      (if (empty? l) r
                     (let [f (first l)
                           k (fc f)
                           v (if-let [o (r k)] (conj o f) [f])]
                       (recur (rest l) (assoc r k v)))))))

(defcheck solution-54ecf68f
  (fn my-group-by [f a-seq]
    (letfn [(key-values [k p-seq]
              (list k (map #(second %) (filter #(= (first %) k) p-seq))))]
      (let [pairs (map #(list (f %) %) a-seq)
            keys  (distinct (map #(first %) pairs))
            ] (apply assoc {} (mapcat #(key-values % pairs) keys))
              )
      )
    ))

(defcheck solution-55117d34
  (fn [f xs]
    (reduce (fn [acc x]
              (let [y (f x)]
                (assoc acc y
                           (if (contains? acc y)
                             (conj (get acc y) x)
                             [x]))))
      {} xs)))

(defcheck solution-55c52544
  (fn [f s]
    (reduce (fn [groups x]
              (update-in groups [(f x)] #(concat % [x])))
      {}
      s)))

(defcheck solution-55d545ea
  (fn [f s] (reduce #(assoc % (f %2) ((fnil conj []) (% (f %2)) %2)) {} s)))

(defcheck solution-562a393c
  (fn b [f v]
    (into {}
      (map (fn [item] (vector (f (first item)) (vec item)))
        (map (fn [item] (map first item))
          (partition-by second
            (sort #(compare (last %) (last %2))
              (map vector v (map f v)))))))))

(defcheck solution-56a0ed1f
  (fn g
    [f x]
    (if (empty? x)
      {}
      (merge-with concat {(f (first x)) [(first x)]} (g f (rest x))))))

(defcheck solution-56f60450
  (fn [f xs]
    (reduce
      (fn [m x]
        (update-in m [(f x)] (fnil conj []) x))
      {} xs)))

(defcheck solution-57b17ebe
  (fn stuff [f values]
    (reduce
      (fn [memo, value]
        (merge memo {(f value) (concat (get memo (f value) []) [value])})
        ) {} values)))

(defcheck solution-582dc235
  (fn [f s]
    (apply merge-with concat
      (map #(hash-map (f %) (vector %)) s))))

(defcheck solution-583c0f80
  (fn [f lst]
    (reduce (fn [m v] (assoc m (f v) (conj (m (f v) []) v))) {} lst)))

(defcheck solution-58469d95
  (fn grp
    ([f s] (grp f s {}))
    ([f s rv]
     (if (empty? s)
       rv
       (let [x (first s)
             k (f x)]
         (if (contains? rv k)
           (grp f (rest s) (assoc rv k (conj (rv k) x)))
           (grp f (rest s) (assoc rv k [x]))))))))

(defcheck solution-58c6188f
  (fn fo
    ([p xs] (fo p xs {}))
    ([p [x & xs] m]
     (if x
       (let [fx (p x)
             v  (m fx [])]
         (recur p xs (assoc m fx (conj v x))))
       m))))

(defcheck solution-5963217a
  #(reduce
     (fn [x y] (let [v (%1 y)] (assoc x v (conj (get x v []) y))))
     {}
     %2))

(defcheck solution-5969bd13
  (fn [f coll]
    (apply (partial merge-with concat) (for [x coll] {(f x) [x]}))))

(defcheck solution-599fda4d
  #(reduce (fn [m x] (update-in m [(% x)] concat [x])) {} %2))

(defcheck solution-59beb087
  (fn [f coll]
    (reduce #(update-in %1 [(f %2)] (fnil conj []) %2) {} coll)))

(defcheck solution-59d1510e
  #(letfn [(worker [l f s]
             (if (empty? l)
               s
               (recur (rest l) f
                 (let [v (f (first l)) i (find s v)]
                   (if i
                     (assoc s v (conj (val i) (first l)))
                     (assoc s v [(first l)]))))))]
     (worker %2 %1 {})))

(defcheck solution-59e15770
  (fn [f s]
    (reduce (fn [result e]
              (let [key        (f e)
                    values     (result key [])
                    new-values (conj values e)]
                (assoc result key new-values)))
      {}
      s)))

(defcheck solution-5a491bd0
  (fn [f s]
    (reduce
      (fn [m v]
        (assoc m (f v)
                 (conj (get m (f v) []) v)))
      {} s)
    ))

(defcheck solution-5a995fb5
  #(reduce (fn [s e]
             (let [fe (% e)]
               (assoc s fe (conj (get s fe []) e))))
     {} %2))

(defcheck solution-5aaad0f1
  (fn [f col] (reduce #(merge-with concat % {(f %2) [%2]}) {} col)))

(defcheck solution-5b5c703f
  (fn [f xs]
    (loop [ys xs
           m  {}]
      (if (seq ys)
        (let [y (first ys)
              v (f y)]
          (recur (rest ys)
            (update-in m [v] (fn [zs] (if (coll? zs) (conj zs y) [y])))))
        m))))

(defcheck solution-5bbb80e2
  (fn [f xs]
    (reduce
      (fn [m a]
        (let [r (f a)]
          (assoc m r (conj (get m r []) a)))) {} xs)))

(defcheck solution-5c165f6f
  (fn [f xs]
    (let [helper (fn helper [xs m]
                   (if xs
                     (let [item (first xs)
                           k    (f item)
                           v    (get m k [])]
                       (helper (next xs)
                         (assoc m k (conj v item))))
                     m))]
      (helper (seq xs) {}))))

(defcheck solution-5c3824b6
  (fn -group-by [f x]
    (let [
          assoc2 (fn [x y]
                   (let [k (first y) v (-> y rest first)]
                     (assoc x k (conj (vec (x k)) v))))
          m      (partition 2 (interleave (map f x) x))
          ]
      (reduce assoc2 {} m))))

(defcheck solution-5c820538
  (fn [kfn c]
    (reduce (fn [m v] (update-in m [(kfn v)] (comp vec conj) v))
      {} c)))

(defcheck solution-5d690073
  (fn group [f coll]
    (reduce
      (fn [accum x]
        (let [k (f x)]
          (assoc accum k (conj (get accum k []) x))))
      {}
      coll
      )
    ))

(defcheck solution-5d6c150f
  (fn my-groupby [f xs]
    (reduce #(update-in %1 [(first %2)] (comp vec conj) (second %2)) {}
      (map #(vector (f %) %) xs)
      )
    ))

(defcheck solution-5dfc3345
  #(reduce (fn [t i] (let [v (% i)] (assoc t v (conj (t v []) i)))) {} %2))

(defcheck solution-5e0381cb
  (fn [f s]
    (apply
      merge-with
      concat
      (map
        (fn [x] (hash-map (f x) [x]))
        s
        )
      )
    ))

(defcheck solution-5e7181a1
  (fn g [f xs]
    (reduce (fn [s x]
              (let [k  (f x)
                    ss (get s k [])
                    ]
                (assoc s k (conj ss x))
                )
              ) {} xs)
    ))

(defcheck solution-5ec76a4
  (fn [f vs] (apply merge-with concat (map (fn [v] {(f v) [v]}) vs))))

(defcheck solution-5f0ccf45
  (fn ff
    ([f s] (ff {} f s))
    ([m f s]
     (if (empty? s)
       m
       (let [i (first s) k (f i)]
         (recur (assoc m k (conj (or (m k) []) i)) f (rest s)))))))

(defcheck solution-5f17b0c4
  (fn [f c]
    (reduce #(merge-with into %1 {(f %2) [%2]}) {} c)))

(defcheck solution-5f20f9e4
  (fn [sel coll]
    (reduce #(let [key (sel %2)]
               (if (contains? %1 key)
                 (assoc %1 key (conj (get %1 key) %2))
                 (assoc %1 key [%2])))
      {} coll)))

(defcheck solution-5f51cb4f
  #(reduce
     (fn [a x]
       (let [k (% x)]
         (assoc a k (conj (a k []) x))))
     {}
     %2))

(defcheck solution-5f71de63
  (fn [f s]
    (let [values (set (map f s))]
      (reduce (fn [dict val] (assoc dict val (filter #(= val (f %)) s))) {} values))))

(defcheck solution-5f89bfae
  (fn [f c]
    (reduce (fn [m e]
              (let [k (f e)]
                (assoc m k (conj (get m k []) e))
                ))
      {} c)))

(defcheck solution-5fb1abf7
  (fn [f xs]
    (reduce
      (fn [m [k v]] (assoc m k (conj (m k []) v)))
      {}
      (partition 2 (interleave (map f xs) xs)))))

(defcheck solution-603348a3
  (fn gb
    [f l]
    (into {}
      (let [s (set (map f l))]
        (map
          (fn [x]
            (vector
              x
              (vec (filter
                     #(= (f %) x)
                     l))))
          s)))))

(defcheck solution-6041a959
  (fn own-group-by [f v]
    (let
     [ct-pairs   (fn [f v] (map vector (map f v) v))
      find-keys  (fn [f v] (distinct (map f v)))
      get-entry  (fn [k ps] (hash-map k (map last (filter #(= k (first %)) ps))))
      ct-entries (fn [ks pairs] (for [k ks] (get-entry k pairs)))
      pairs      (ct-pairs f v)
      ks         (find-keys f v)
      entries    (ct-entries ks pairs)]
      (into {} entries))))

(defcheck solution-60453b2a
  (fn [f x] (loop [i 0 y {}] (if (< i (count x)) (recur (inc i) (merge-with into y {(f (get x i)) [(get x i)]})) y))))

(defcheck solution-610035bd
  #(reduce (fn [m c] (assoc m (% c) (conj (m (% c) []) c))) {} %2))

(defcheck solution-610237dc
  (fn group-seq [f coll]
    (apply merge-with into (for [i coll]
                             {(f i) [i]}))))

(defcheck solution-6178ae2d
  #(apply merge-with concat
     (for [s %2] {(%1 s) [s]})))

(defcheck solution-61cb2db
  (fn
    [f cs]
    (reduce (fn [m c]
              (let [v (f c)]
                (assoc m v (conj (m v []) c)))) {} cs)))

(defcheck solution-627c7ab3
  (fn [f s]
    (reduce
      #(assoc %1 (f %2)
                 (conj (get %1 (f %2) []) %2))
      {} s)))

(defcheck solution-629d7840
  (fn [f ns] (apply merge (map #(hash-map (f (first %1)) (vec %1)) (partition-by f (sort-by f ns))))))

(defcheck solution-629e4b61
  (fn
    [f args]
    (loop [args args m {}]
      (if-not (empty? args)
        (if (= (get m (f (first args))) nil)
          (recur (rest args) (assoc-in m (vector (f (first args))) (vector (first args))))
          (recur (rest args) (assoc m (f (first args)) (conj (get m (f (first args))) (first args)))))
        m)
      )))

(defcheck solution-631a541d
  (fn groupByX [f x] (let [part (partition-by f (sort-by f x))] (zipmap (map #(f (first %)) part) (map #(into (empty x) %) part)))))

(defcheck solution-632e93a5
  (fn [f coll]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x)))) (cons {} coll))))

(defcheck solution-6342b4d1
  (fn [f s] (reduce #(assoc %1 (f %2) (concat (get %1 (f %2)) [%2])) {} s)))

(defcheck solution-63d26a9e
  (fn [f l]
    (reduce #(let [fx (f %2)]
               (assoc %1 fx (conj (or (get %1 fx) [])
                              %2)))
      {} l)))

(defcheck solution-6439347c
  (fn [f coll]
    (reduce
      (fn [s e]
        (assoc s (f e) (conj (s (f e) []) e)))
      {}
      coll)))

(defcheck solution-644c4caf
  (fn cust-group [f s]
    (loop [f f s s m {}]
      (if (empty? s)
        m
        (let [f-s (first s)]
          (recur
            f
            (rest s)
            (assoc m (f f-s) (conj (get m (f f-s) []) f-s))))))))

(defcheck solution-6451ec8b
  (fn [func coll]
    (reduce #(let [re (func %2)]
               (assoc %1 re (if (contains? %1 re)
                              (conj (%1 re) %2)
                              [%2])))
      {}
      coll)))

(defcheck solution-651b72e3
  (fn [f cl] (loop [c cl r {}]
               (if (empty? c) r
                              (recur (rest c) (if (nil? (get r (f (first c)))) (assoc r (f (first c)) [(first c)])
                                                                               (assoc r (f (first c)) (conj (get r (f (first c))) (first c)))))
                              ))))

(defcheck solution-6520d88b
  (fn [f c] (reduce #(into % {(f %2) (conj (% (f %2) []) %2)}) {} c)))

(defcheck solution-6546ca72
  (fn [f c] (apply hash-map ((fn [kv] (interleave (map (comp first first) kv) (map #(map second %) kv))) (partition-by first (sort-by first (map #(vector (f %) %) c)))))))

(defcheck solution-657537ee
  #(apply merge-with concat
     (map (fn [x] {(% x) [x]}) %2)))

(defcheck solution-660525
  (fn my-group-by [f s]
    (if (empty? s)
      {}
      (merge-with concat {(f (first s)) (list (first s))} (my-group-by f (rest s))))))

(defcheck solution-663d4aed
  (fn [f coll] (into {} (map #(vector (f (first %)) (vec %)) (partition-by f (sort coll))))))

(defcheck solution-6664b147
  (fn g
    [f [x & xs]]
    (if x (merge-with concat {(f x) [x]} (g f xs)))))

(defcheck solution-669c7951
  (fn [f coll] ((fn [res] (zipmap res (map #(filter (fn [result] (= % (f result))) coll) res))) (map f coll))))

(defcheck solution-66d56e7f
  (fn [f col] (reduce
                (fn [l r] (update-in l [(f r)] #(vec (conj % r))))
                {}
                col)))

(defcheck solution-66e444f0
  (fn [f c]
    (reduce #(let [v (f %2)] (update-in %1 [v] concat [%2])) {} c)))

(defcheck solution-66fc0459
  (fn grp-by [f coll]
    (reduce (fn [acc i]
              (update-in acc
                [(f i)]
                (fnil #(conj % i) [])))
      {}
      coll)))

(defcheck solution-670e989c
  (fn [pred coll]
    (->> (map #(hash-map (pred %) [%]) coll)
      (apply merge-with concat))))

(defcheck solution-673c16a0
  (fn [f c]
    (->> (map #(do {(f %) [%]}) c)
      (apply merge-with concat))))

(defcheck solution-67962a22
  (fn [f xs]
    (reduce (fn [m v]
              (let [k (f v)]
                (assoc m k (conj (get m k []) v))))
      {} xs)))

(defcheck solution-67a01bcd
  (fn [f xs]
    (reduce
      (fn [m x] (let [r (f x)] (assoc m r (conj (get m r []) x))))
      {}
      xs)))

(defcheck solution-680d3b67
  (fn [f s]
    (let [lislis (sort-by first (partition 2 (interleave (map f s) s)))]
      (apply hash-map (interleave (distinct (map first lislis))
                        (for [k (distinct (map first lislis))]
                          (map second (filter #(= k (first %)) lislis))))))))

(defcheck solution-68105d92
  (fn my-group-by [f coll]
    (reduce
      (fn [m v]
        (let [k (f v)]
          (assoc m k (conj (get m k []) v)))) {} coll)))

(defcheck solution-6895a68f
  #(reduce (fn [m e]
             (assoc m (% e) (conj (m (% e) []) e))
             ) {} %2))

(defcheck solution-692b1c54
  (fn gr [f col]
    (if-let [s (seq col)]
      (let [v  (first s)
            fv (f v)
            n  (gr f (next s))
            nv (get n fv (list))
            vv (assoc n fv (cons v nv))]
        vv)
      {})))

(defcheck solution-6962db18
  (fn grpby [f coll]
    (loop [result {},
           coll   coll]
      (if (empty? coll)
        result
        (if (contains? result (f (first coll)))
          (recur (assoc result (f (first coll)) (conj (result (f (first coll))) (first coll))) (rest coll))
          (recur (conj result [(f (first coll)) [(first coll)]]) (rest coll)))))))

(defcheck solution-69fb0b4e
  (fn groupby [f coll]
    (reduce (fn [m x]
              (let [key (f x)]
                (assoc m key (conj (m key []) x)))) {} coll)))

(defcheck solution-6a039d16
  (fn [f coll]
    (reduce (fn [map x]
              (let [k (f x) v (get map k)]
                (if (nil? v)
                  (assoc map k [x])
                  (assoc map k (conj v x)))))
      {}
      coll)))

(defcheck solution-6a69fe01
  (fn [f s]
    (reduce (fn [grouped item]
              (merge-with concat grouped {(f item) [item]}))
      {}
      s)))

(defcheck solution-6a8b8ed4
  (fn [f l] (reduce (fn [a b] (let [key (f b)] (assoc a key (if (contains? a key) (conj (a key) b) [b])))) {} l)))

(defcheck solution-6adaa8ed
  (fn [fun ve]
    (reduce
      (fn [acc item]
        (let [
              val (fun item)
              ]

          (update-in acc [val] (fnil (fn [x] (conj x item)) []))))
      {}
      ve)
    ))

(defcheck solution-6ae1dec4
  (fn my-group-by [f s]
    (letfn [(update [m x]
              (assoc m (f x) (conj (m (f x) []) x)))]
      (reduce update {} s))))

(defcheck solution-6aee65bd
  (fn [f c] (apply merge-with concat (map #(hash-map (f %) [%]) c))))

(defcheck solution-6aff60d6
  #(reduce (fn [m x] (update-in m [(%1 x)] (fn [y] (conj (vec y) x)))) {} %2))

(defcheck solution-6b8923e7
  (fn [f c] (apply merge-with concat (for [x c] {(f x) [x]}))))

(defcheck solution-6c25e69
  (fn group--reduce
    [f coll] {:pre [(ifn? f)]}
    (reduce (fn [{acc (f x) :as m :or {acc []}} x]
              (assoc m (f x) (conj acc x)))
      {}
      coll)))

(defcheck solution-6c3024c3
  (fn group-seq [f s]
    (reduce (fn [m e]
              (let [the-key (f e)]
                (if (contains? m the-key)
                  (assoc m the-key (conj (get m the-key) e))
                  (assoc m the-key [e]))))
      {}
      s)))

(defcheck solution-6c565b36
  (fn [f s] (apply merge-with concat (map (fn [x] {(f x) [x]}) s))))

(defcheck solution-6c62717d
  (fn [f coll] (reduce conj (map (fn [x] (hash-map x (vec (filter #(= (f %) x) coll)))) (set (map f coll))))))

(defcheck solution-6c6295e4
  (fn [f c] (reduce #(let [r (f %2)] (conj % [r (conj (get % r []) %2)])) {} c)))

(defcheck solution-6c79856b
  (fn [f s]
    (reduce (fn [m v]
              (if (contains? m (f v))
                (update-in m [(f v)] #(conj % v))
                (assoc m (f v) [v])))
      {} s)))

(defcheck solution-6cb4eb1d
  (fn [f xs]
    (reduce
      (fn [m [k v]]
        (assoc m v (conj (m v []) k)))
      {}
      (map list xs (map f xs)))))

(defcheck solution-6cdc78e6
  (fn [f s]
    (reduce (fn [m v]
              (let [k (f v)]
                (assoc m
                  k
                  (conj (get m k []) v))))
      {}
      s)))

(defcheck solution-6ce4600e
  (fn group-seq
    ([x y]
     (group-seq x y {}))
    ([x y z]
     (if (= 0 (count y))
       z
       (let [applied (x (first y))]
         (if (contains? z applied)
           (recur x (rest y) (update-in z [applied] conj (first y)))
           (recur x (rest y) (conj z {applied (vector (first y))}))))))))

(defcheck solution-6d0c64a
  #(apply merge-with concat
     (map (fn [item]
            (hash-map (%1 item) [item]))
       %2)))

(defcheck solution-6d47086f
  (fn group [f c]
    (zipmap (set (map f c))
      (for [x (set (map f c))]
        (vec (filter #(= x (f %)) c))
        )
      )
    ))

(defcheck solution-6d4f848
  (fn [f acol]
    (reduce (partial merge-with concat)
      {}
      (map #(hash-map (f %) [%]) acol))))

(defcheck solution-6dad3adc
  (fn my-group-by
    [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-6dd09919
  (fn [f coll]
    (apply merge-with #(concat % %2)
      (map #(hash-map (f %) [%]) coll))))

(defcheck solution-6e2952f4
  (fn [f xs]
    (letfn [(u [c n]
              (let [k (f n)]
                (assoc c k (conj (get c k []) n))))]
      (reduce u {} xs))))

(defcheck solution-6ef02e77
  (fn gs
    ([f xs] (gs f xs {}))
    ([f xs result]
     (if (empty? xs) result
                     (let [h (first xs) t (rest xs) k (f h)]
                       (if (contains? result k)
                         (recur f t (assoc result k (conj (result k) h)))
                         (recur f t (assoc result k [h]))))))))

(defcheck solution-6ef54542
  (fn groupSeq [func s] (apply merge-with concat (map #(assoc {} (func (first %)) %) (partition-by func s)))))

(defcheck solution-6f1f3597
  (fn [f s]
    (letfn [(add-x [m x]
              (assoc m (f x) (conj (m (f x) []) x)))]
      (reduce add-x {} s))))

(defcheck solution-6f4ebc16
  ; update is blocked for some reason, so we'll use update-in instead...
  ; we need to explicitly use a vector, since (conj nill 'a) is '(:a), thus conjing will prepend
  (fn [f col]
    (reduce (fn [m v] (update-in m [(f v)] #(vec (conj % v))))
      {}
      col)))

(defcheck solution-70a5bc72
  (fn [f xs]
    (reduce
      (fn [m x]
        (let [fx (f x)]
          (assoc m fx (conj (get m fx []) x))))
      {} xs)))

(defcheck solution-70a8c160
  (fn
    [func col]
    (reduce
      (fn [myMap item]
        (conj myMap
          (let [myKey (func item), value (myMap myKey)]
            (vector myKey
              (if
               (nil? value)
                (vector item)
                (conj value item)
                )
              )
            )
          )
        )
      {} col)))

(defcheck solution-70d01924
  (fn [f coll]
    (loop [c1 coll
           m1 {}]
      (if (empty? c1)
        m1
        (let [head  (first c1)
              res   (f head)
              entry (get m1 res)]
          (recur (rest c1)
            (assoc m1 res
                      (if (nil? entry)
                        [head]
                        (conj entry head)))))))))

(defcheck solution-7109f0ee
  (fn [f coll]
    (apply (partial merge-with concat) (map (fn [x] {(f x) [x]}) coll))))

(defcheck solution-71285148
  (fn index [f xs]
    (let [c (fn [acc v]
              (let [k (f v)]
                (if (contains? acc k)
                  (merge-with conj acc {k v})
                  (conj acc {k [v]}))))]
      (reduce c {} xs))))

(defcheck solution-715630d6
  (fn [f xs]
    (reduce
      #(update-in %1 [(f %2)]
         (fn [v] (if v (conj v %2) [%2])))
      {}
      xs)))

(defcheck solution-71ebad91
  (fn my-group-by [f coll]
    (loop [eq-classes {}
           coll       coll]
      (let [x (first coll)]
        (if (empty? coll)
          eq-classes
          (if (contains? eq-classes (f x))
            (recur (update-in eq-classes [(f x)] conj x)
              (rest coll))
            (recur (assoc eq-classes (f x) [x])
              (rest coll))))))))

(defcheck solution-7257013a
  (fn [func x]
    (let [kvals (interleave (map func x) x)]
      (loop [result {} kvals kvals]
        (if (empty? kvals)
          result
          (recur (if (contains? result (first kvals))
                   (assoc result (first kvals) (conj (get result (first kvals)) (second kvals)))
                   (assoc result (first kvals) (conj [] (second kvals)))) (rest (rest kvals))))))))

(defcheck solution-727fa981
  (fn [f xs]
    (reduce (fn [acc b]
              (let [c       (f b)
                    b-ent   (get acc c [])
                    new-ent (conj b-ent b)]
                (assoc acc c new-ent)))
      {}
      xs)))

(defcheck solution-72839270
  (fn [op col] (reduce #(merge-with concat %1 {(op %2) [%2]}) {} col)))

(defcheck solution-728ab9a8
  #(reduce (fn [acc, [k, v]] (assoc acc k
                                        (conj (acc k []) v)))
     {}
     (map (fn [i] [(%1 i) i]) %2)))

(defcheck solution-72acf751
  (fn [f s]
    (apply merge-with concat (map #(hash-map (f %) [%]) s))))

(defcheck solution-72e600d8
  (fn [func coll]
    (reduce
      (fn [output item]
        (let [item-key (func item)]
          (assoc output item-key (conj (get output item-key []) item))))
      {}
      coll)))

(defcheck solution-733449f7
  (fn t [f coll]
    (reduce
      (fn [m [k v]] (assoc m k (concat (m k) [v]))) {} (map #(vector (f %) %) coll))))

(defcheck solution-73758967
  (fn my [f s] (reduce (fn [m v] (let [k (f v)] (assoc m k (if (m k) (conj (m k) v) [v])))) {} s)))

(defcheck solution-742eeeb0
  (fn [f v] (reduce #(merge-with conj %1 {(f %2) %2}) (zipmap (map f v) (repeat [])) v)))

(defcheck solution-7445f646
  (fn [f, s]
    (reduce (fn [m, val]
              (let [k (f val) v (or (get m k) [])]
                (assoc m k (conj v val)))) {} s)))

(defcheck solution-746cc2a7
  (fn [f coll]
    (reduce
      (fn [a b]
        (let [e (f b)]
          (assoc a e (conj (get a e []) b))))
      {} coll)))

(defcheck solution-748b8211
  (fn [f c] (into {} (for [q (set (map f c))] [q (keep #(if (= q (f %)) %) c)]))))

(defcheck solution-74ab5ec7
  (fn [f s]
    (reduce (fn [m x]
              (let [v (m (f x))]
                (assoc m (f x) (conj (if (seq v) v []) x)))) {} s)))

(defcheck solution-74ad4fc0
  (fn [f s]
    (reduce
      #(let [k (first %2), v (second %2)]
         (if (nil? (%1 k))
           (assoc %1 k [v])
           (assoc %1 k (conj (%1 k) v))))
      {} (map (fn [x] (vector (f x) x)) s))))

(defcheck solution-74bba129
  (fn [f coll]
    (loop [items coll grouped {}]
      (if-let [i (first items)]
        (recur (next items)
          (assoc grouped (f i) (conj (grouped (f i) []) i)))
        grouped))))

(defcheck solution-7505fd32
  (fn [f s]
    (reduce (fn [a b]
              (let [k (f b)]
                (assoc a k (if (a k) (conj (a k) b) [b])))) {} s)))

(defcheck solution-75f9b6b4
  (fn fn2 [f a]
    (persistent!
      (reduce
        #(let [x (f %2)]
           (assoc! % x (conj (get % x []) %2)))
        (transient {}) a))))

(defcheck solution-76c54d7b
  (fn [f coll]
    (apply hash-map
      (mapcat #(vector (ffirst %) (vec (map second %)))
        (partition-by first
          (sort-by first (map #(vector (f %) %) coll)))))))

(defcheck solution-76dab6f2
  #(loop [input %2, result {}]
     (if (empty? input)
       result
       (let [x   (first input),
             fx  (%1 x),
             tmp (or (result fx) [])]
         (recur (rest input)
           (assoc result fx (conj tmp x)))))))

(defcheck solution-76f4a784
  (fn [f s]
    (reduce
      (fn [x y]
        (let [r (f y)]
          (assoc x r (conj (get x r []) y))))
      {} s)))

(defcheck solution-772ba5a1
  #(apply merge-with into (map (fn [x] {(% x) [x]}) %2)))

(defcheck solution-77302132
  (fn [f xs]
    (reduce
      (fn [acc x]
        (let [k (f x)]
          (if (contains? acc k)
            (update-in acc [k] conj x)
            (assoc acc k [x]))))
      {} xs)))

(defcheck solution-77453070
  (fn [f s] (reduce #(update-in % [(f %2)] (fnil concat []) [%2]) {} s)))

(defcheck solution-774d2ff5
  (fn [f s] (reduce (fn [a itm] (assoc a (f itm) (concat (a (f itm)) [itm]))) {} s)))

(defcheck solution-776856ea
  (fn my-group-by [f coll]
    (reduce #(assoc %1
               (f %2)
               (vec (conj (%1 (f %2)) %2)))
      {}
      coll)))

(defcheck solution-77748a68
  (fn [f seq]
    (apply merge-with concat (map (fn [v] {(f v) (list v)}) seq))))

(defcheck solution-781b76f5
  (fn [pred s] (apply merge-with into (map (fn [l] {(pred l) [l]}) s))))

(defcheck solution-7886e176
  (fn gb [f s]
    (reduce
      (fn [m x] (let [k (f x), v (m k)] (assoc m k ((fnil conj []) v x))))
      {} s)))

(defcheck solution-788e261
  (fn my-group-by [f colls]
    (reduce (fn [maps x]
              (let [y (f x)]
                (assoc maps y (conj (get maps y []) x)))) {} colls)))

(defcheck solution-795a72f9
  (fn gb
    ([f lst] (gb {} f lst))
    ([m f lst] (if (empty? lst) m
                                (let [r  (f (first lst))
                                      vl (m r)]
                                  (gb (conj m [r (conj (if (nil? vl) [] vl) (first lst))]) f (next lst)))))))

(defcheck solution-7979f00c
  (fn [f coll] (reduce #(merge-with concat %1 {(f %2) [%2]}) {} coll)))

(defcheck solution-799bb26c
  (fn [f coll]
    (apply merge-with
      concat
      (map #(hash-map (f %) [%]) coll))))

(defcheck solution-7a026d9e
  (fn [f s]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {} s)))

(defcheck solution-7a0db1d1
  (fn [f coll] (loop [c coll m {}]
                 (if (empty? c)
                   m
                   (let [el (first c)]
                     (recur (rest c)
                       (conj m [(f el)
                                (conj (vec (m (f el))) el)])))))))

(defcheck solution-7a10cfac
  (fn [f s]
    (loop [lst s ans (hash-map)]
      (if (empty? lst)
        ans
        (let [k (f (first lst))]
          (recur
            (rest lst)
            (assoc ans k (concat (get ans k) [(first lst)]))))))))

(defcheck solution-7a142b11
  (fn [f s]
    (loop [acc {} xs s]
      (if (seq xs)
        (let [h (first xs) rs (rest xs) fh (f h)]
          (recur (assoc acc fh (conj (acc fh []) h)) rs))
        acc))))

(defcheck solution-7a22e23
  (fn [f, v]
    (loop [r {}, v v]
      (if (empty? v)
        r
        (if (r (f (first v)))
          (recur (assoc r (f (first v)) (conj (r (f (first v))) (first v))) (rest v))
          (recur (assoc r (f (first v)) [(first v)]) (rest v)))))))

(defcheck solution-7a48af72
  (fn [f colls]
    (reduce (fn [acc i]
              (let [k (f i)]
                (if-let [v (seq (acc k))]
                  (update-in acc [k] conj i)
                  (assoc acc k [i]))))
      {}
      colls)))

(defcheck solution-7ac4001b
  (fn [p s]
    (apply (partial merge-with concat)
      (map #(hash-map (p %) [%]) s))))

(defcheck solution-7b3f9677
  #(apply merge-with concat (map (fn [x] {(% x) [x]}) %2)))

(defcheck solution-7b8032c7
  (fn [arga argb]
    (reduce
      (fn [a b] (merge-with #(vec (concat %1 %2)) a b))
      (map #(array-map (arga %) [%]) argb)
      )))

(defcheck solution-7b83920c
  (fn [f s]
    (reduce
      (fn [m [r a]]
        (if (m r)
          (update-in m [r] #(conj % a))
          (conj m [r [a]])
          )
        )
      {}
      (map #(vector (f %) %) s))))

(defcheck solution-7b8e5254
  (fn [f xs] (reduce (fn [res x] (let [fx (f x)] (conj res [fx, (conj (get res fx []) x)]))) {} xs)))

(defcheck solution-7cc9ba1c
  (fn [f s]
    (loop [[head-s & rest-s] s
           accum {}]
      (if (nil? head-s)
        accum
        (recur
          rest-s
          (assoc accum (f head-s) (conj (get accum (f head-s) []) head-s)))))))

(defcheck solution-7d2a2dfb
  (fn [fun s]
    (->> (sort-by fun s)
      (partition-by fun)
      (mapcat (fn [x] [(fun (first x)) x]))
      (apply hash-map))))

(defcheck solution-7e0e51d3
  (fn [f s]
    (into {}
      (for [k (into #{} (map f s))]
        [k (filter #(= (f %) k) s)]))))

(defcheck solution-7eeff5fd
  (fn [f s] (reduce #(merge-with concat % {(f %2) [%2]}) {} s)))

(defcheck solution-7f7767f3
  (fn [f c]
    (reduce
      #(update-in % [(f %2)] concat [%2])
      {}
      c)))

(defcheck solution-808d7f1
  (fn [f coll]
    (reduce (fn [hm item]
              (let [key (f item)]
                (if-let [val (get hm key)]
                  (assoc hm key (conj val item))
                  (assoc hm key [item])))) {} coll)))

(defcheck solution-8169324f
  (fn [f c]
    (apply merge-with concat
      (map #(hash-map (f %) [%])
        c))))

(defcheck solution-81a7f0eb
  #(reduce
     (fn [m i]
       (merge-with concat m {(%1 i) [i]}))
     {} %2))

(defcheck solution-821d49e2
  (fn gb [f coll]
    (loop [n 0 c (count coll) r {}]
      (if (= n c)
        r
        (let [v (nth coll n) k (f v)]
          (if (r k)
            (recur (inc n) c (assoc r k (conj (r k) v)))
            (recur (inc n) c (assoc r k [v]))))))))

(defcheck solution-82480a0e
  (fn [f xs]
    (reduce (fn [m x] (assoc m (f x) (conj (get m (f x) []) x))) {} xs)))

(defcheck solution-825d3a07
  (fn my-group-by [func l]
    (->>
      l
      (map #(vector (func %) %))
      (reduce (fn [grouped [key value]]
                (update-in grouped [key] #(if (contains? grouped key)
                                            (concat % [value])
                                            [value]
                                            ))) {})

      )
    ))

(defcheck solution-830853f8
  (fn [f s] (apply merge-with into (for [x s] {(f x) [x]}))))

(defcheck solution-83254381
  (fn [f s]
    (loop [m {}
           v (first s)
           s s]
      (let [k (f v)
            n (assoc m k (conj (m k []) v))]
        (if (next s)
          (recur n (fnext s) (rest s))
          n)))))

(defcheck solution-832e9d3
  (fn g [f s]
    (reduce #(merge-with into % {(f %2) [%2]}) {} s)))

(defcheck solution-8348c770
  (fn [f xs] (let [fr (fn [m fs]
                        (if (first fs) (let [k (first (first fs))]
                                         (if (contains? m k)
                                           (recur (assoc m k (conj (m k) (second (first fs)))) (rest fs))

                                           (recur (assoc m k [(second (first fs))]) (rest fs))
                                           ))
                                       m)
                        )]
               (fr {} (partition 2 (interleave (map f xs) xs)))
               )))

(defcheck solution-835f2e1b
  (fn [f xs] (apply merge-with #(concat %1 %2) (map #(hash-map (f %1) (vector %1)) xs))))

(defcheck solution-837ca5b1
  (fn [g vs]
    (reduce
      (fn [acc v]
        (update-in acc [(g v)] (fnil conj []) v))
      {}
      vs)))

(defcheck solution-83c6167a
  (fn [f s]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {} s)))

(defcheck solution-83e6960c
  (fn [f s]
    (loop [ss s out {}]
      (if (empty? ss)
        out
        (let [e (first ss) v (f e)]
          (recur (rest ss) (assoc out v (if (out v) (conj (out v) e) [e]))))))))

(defcheck solution-84821752
  #(reduce
     (fn [s a]
       (merge-with into s {(% a) [a]}))
     {}
     %2))

(defcheck solution-84bd6ffd
  #(apply merge-with concat
     (for [x %2] {(% x) [x]})))

(defcheck solution-84e1f129
  (fn [f xs]
    (letfn [(do-it [ax el]
              (let [the-key (f el)
                    old-val (ax the-key [])
                    new-val (conj old-val el)]
                (assoc ax the-key new-val)))]
      (reduce do-it {} xs))))

(defcheck solution-851485bb
  (fn gs [f xs]
    (reduce
      (fn [result x]
        (assoc result (f x)
                      (conj (or (result (f x)) []) x)))
      {}
      xs)))

(defcheck solution-85952a6
  #(loop [m {}, [h & t] %2]
     (if (nil? h)
       m
       (let [k (% h), v h]
         (recur (assoc m k
                         (if (contains? m k)
                           (conj (m k) v)
                           [v]))
           t)))))

(defcheck solution-85ae4a27
  (fn [f s]
    (reduce
      (fn [r x]
        (let [k (f x)]
          (assoc r k (conj (or (get r k) []) x))))
      {}
      s)))

(defcheck solution-861cc6eb
  (fn [f c]
    (into {} (map (fn [x]
                    [(f (first x)) (vec x)])
               (partition-by f (sort-by f c))))))

(defcheck solution-86a0abd4
  (fn [f C] (reduce (fn [S x] (assoc S (f x) (vec (conj (S (f x)) x)))) {} C)))

(defcheck solution-86b32746
  (fn grpseq [f s]
    (let [sentry (fn [m [k v]]
                   (assoc m k (concat (get m k []) [v])))]
      (reduce sentry {} (map #(vector (f %) %) s)))
    ))

(defcheck solution-86ca4ebd
  (fn grp [fun input]
    (loop [acc  {}
           f    fun
           coll input]
      (if (empty? coll)
        acc
        (recur
          (update-in acc
            [(f (first coll))]
            #(vec (conj % (first coll))))
          f
          (rest coll))))))

(defcheck solution-87332b6a
  (fn [f coll]
    (loop [left coll result {}]
      (if (empty? left)
        result
        (let [k (f (first left)) v (first left)]
          (recur (rest left)
            (assoc result k (conj (get result k []) v))))))))

(defcheck solution-874a0597
  (fn [f s]
    (reduce (fn [acc val]
              (let [key (f val)]
                (assoc acc key (conj (acc key []) val))))
      {}
      s)))

(defcheck solution-875aeb34
  #(loop [coll %2, r {}]
     (if (empty? coll)
       r
       (recur
         (rest coll)
         (let [e (first coll), k (%1 e)]
           (assoc r k (conj (r k []) e)))))))

(defcheck solution-8775e85f
  #(reduce (fn [c x] (merge-with into c {(% x) [x]})) {} %2))

(defcheck solution-87db53f3
  (fn [f col] (reduce #(let [k (f %2) c (%1 k)] (assoc %1 k (conj (if (nil? c) [] c) %2))) {} col)))

(defcheck solution-87faa192
  (fn [f coll]
    (let [vals (map vec (partition-by f (sort-by f coll)))
          keys (map #(f (first %)) vals)]
      (apply hash-map (interleave keys vals)))))

(defcheck solution-88159763
  (fn [f s] (apply merge-with concat (for [x s] {(f x) [x]}))))

(defcheck solution-88283288
  (fn fun [f l]
    (reduce
      #(assoc %
         (f %2)
         (conj
           (if (contains? % (f %2))
             (% (f %2))
             [])
           %2))
      {}
      l)))

(defcheck solution-884d379b
  (fn [f lst]
    (into {} (for [[k v] (apply hash-map (interleave (set (map f lst)) (repeat [])))] [k (filter #(= k (f %)) lst)]))))

(defcheck solution-88659cc9
  (fn [f coll]
    (reduce into {}
      (for [r (set (map f coll))] {r (vec (filter #(= r (f %)) coll))}))
    ))

(defcheck solution-88ea30ef
  (fn [f coll]
    (let [vs (map hash-map (map f coll) (map vector coll))]
      (apply merge-with #(vec (concat %1 %2)) vs))))

(defcheck solution-89656f17
  (fn [f s]
    (let [kys (distinct (map f s))
          vls (map (fn [k]
                     (reduce #(if (= k (f %2)) (conj %1 %2) %1) [] s)) kys)]
      (zipmap kys vls))))

(defcheck solution-89ef4243
  (fn [f s] (reduce #(assoc % (f %2) (conj (get % (f %2) []) %2)) {} s)))

(defcheck solution-8aa2a810
  (fn [f col]
    (let [vcol (map f col)]
      (apply merge-with #(conj %1 (first %2)) (map #(assoc {} %1 [%2]) vcol col)))))

(defcheck solution-8ae74725
  (fn f63 [f l]
    (if (empty? l) {}
                   (let [fl (first l) ky (f fl) rm (f63 f (rest l))]
                     (if (contains? rm ky)
                       (update-in rm [ky] (fn [e] (cons fl e)))
                       (assoc rm ky [fl])
                       )))))

(defcheck solution-8aeddea8
  (fn [f coll]
    (loop [result {} rst coll]
      (if (empty? rst)
        result
        (let [applied (f (first rst))
              grouped (get result applied [])]
          (recur (assoc result applied (conj grouped (first rst)))
            (rest rst)))))))

(defcheck solution-8afe3ae9
  (fn gb [f s]
    (if (empty? s)
      {}
      (merge-with concat {(f (first s)) [(first s)]} (gb f (rest s))))))

(defcheck solution-8b3d7fc2
  (fn [f xs]
    (reduce (fn [m v]
              (let [k (f v)]
                (if-let [vs (m k)]
                  (assoc m k (conj vs v))
                  (assoc m k [v]))))
      {}
      xs)))

(defcheck solution-8b4fbf95
  (fn grp
    ([f s] (grp {} f s))
    ([acc f [x & xs]]
     (if x
       (let [g (f x)]
         (if (acc g)
           (grp (assoc acc g (conj (acc g) x)) f xs)
           (grp (assoc acc g [x]) f xs)))
       acc))))

(defcheck solution-8b722f14
  (fn [f s]
    (loop [rs {} ss s]
      (if ss
        (let [it (first ss) k (f it)]
          (recur (assoc rs k (conj (rs k []) it)) (next ss))
          )
        rs
        )
      )
    ))

(defcheck solution-8b78cbab
  (fn [f coll] (reduce (fn [res e] (update-in res [(f e)] #(conj (vec %) e))) {} coll)))

(defcheck solution-8bcf3de7
  (fn [f s]
    (loop [remaining s ans {}]
      (if (empty? remaining)
        ans
        (let [[item & remain] remaining]
          (if (contains? ans (f item))
            (recur remain (update-in ans [(f item)] conj item))
            (recur remain (conj ans [(f item) [item]]))))))))

(defcheck solution-8c7db409
  (fn [f coll]
    (reduce #(assoc %1 (f %2) (conj (vec (%1 (f %2))) %2)) {} coll)))

(defcheck solution-8c7f4616
  (fn [f s]
    (reduce
      (fn [r a]
        (assoc r (f a) (conj (get r (f a) []) a)))
      {}
      s)))

(defcheck solution-8cbd7d3d
  (fn [f coll]
    (reduce
      (fn [m i] (assoc m (f i) (vec (conj (get m (f i)) i))))
      {}
      coll)))

(defcheck solution-8d6e63ea
  (fn [f x] (reduce
              (fn [x [k v]]
                (let [e (get x k)]
                  (if e
                    (assoc x k (conj e v))
                    (assoc x k [v]))))
              {}
              (map #(list (f %) %) x))))

(defcheck solution-8d8c4712
  (fn [f xs]
    (loop [dict {} vs xs ks (map f xs)]
      (if (or (empty? vs) (empty? ks))
        dict
        (let [v (first vs) k (first ks)]
          (if (contains? dict k)
            (recur (conj dict [k (conj (dict k) v)]) (rest vs) (rest ks))
            (recur (conj dict [k [v]]) (rest vs) (rest ks))))))))

(defcheck solution-8d92f256
  (fn [f s]
    (loop [s s, m {}]
      (if (seq s)
        (let [k (f (first s))]
          (if-let [v (get m k)]
            (recur (rest s) (assoc m k (conj v (first s))))
            (recur (rest s) (assoc m k [(first s)]))))
        m))))

(defcheck solution-8e22de6b
  (fn my-group-by [f s]
    (reduce (fn [m v]
              (let [k    (f v)
                    vals (or (m k) [])]
                (assoc m k (conj vals v))))
      {}
      s)))

(defcheck solution-8e4ad715
  (fn [f coll] (apply merge-with concat (for [x coll] {(f x) [x]}))))

(defcheck solution-8e88ee09
  (fn
    [f s]
    (reduce
      #(let
        [k (f %2)]
         (assoc
          %1
           k
           (if
            (contains? %1 k)
             (conj (get %1 k) %2)
             (vector %2))))
      {}
      s)))

(defcheck solution-8ea21de0
  (fn [fnc list]
    (loop [result {} [element & remaining] list]
      (if (nil? element)
        result
        (let [value (fnc element)]
          (recur (conj result (vector value (conj (or (result value) []) element))) remaining)
          )
        )
      )
    ))

(defcheck solution-8ebe3a6b
  (fn [f coll]
    (apply array-map
      (mapcat #(vector (f (first %)) %)
        (partition-by f (sort-by f coll))))))

(defcheck solution-8f214cac
  (fn [f l]
    (letfn [(thurk [res el]
              (let [smock (f el)]
                (assoc res
                  (f el)
                  (vec (conj (res smock) el)))))]
      (reduce thurk {} l))))

(defcheck solution-8f593def
  (fn [f xs]
    (let [group-map (fn [m x]
                      (let [key  (f x)
                            prev (get m key [])
                            next (conj prev x)]
                        (assoc m key next)))]
      (reduce group-map {} xs))))

(defcheck solution-8f5aad50
  (fn [a b] (reduce #(assoc % (a %2) (conj (get % (a %2) []) %2)) {} b)))

(defcheck solution-8f773633
  (fn [f l]
    (reduce
      (fn [m [k v]] (assoc m k (conj (get m k []) v)))
      {}
      (map #(vec [(f %) %]) l))))

(defcheck solution-8f988c8f
  (fn seq-group-by [f s]
    (apply merge-with concat (map #(assoc {} (f %) [%]) s))))

(defcheck solution-8fa96467
  (fn [f coll]
    (reduce (fn [acc e]
              (let [k (f e) v (or (acc k) [])]
                (assoc acc k (conj v e)))
              ) {} coll)
    ))

(defcheck solution-8fc1f0e9
  (fn [f values]
    (let [ks (-> (map f values) set)]
      (reduce (fn [result k] (conj result [k (filter #(= (f %) k) values)])) {} ks))))

(defcheck solution-9064dcdc
  (fn groupseq [f x]
    (loop [x x result '{}]
      (cond (empty? x) result
            :else (let [fx (first x) fval (f fx)]
                    (recur (rest x)
                      (cond (nil? (get result fval)) (assoc result fval (list fx))
                            :else (assoc result fval (concat (get result fval) (list fx)))
                            ))
                    )
            )
      )
    ))

(defcheck solution-90766ad5
  (fn [group iter]
    (-> (fn reduce-iter [hash elem]
          (let [key      (group elem)
                cur-list (hash key [])
                new-list (conj cur-list elem)]
            (assoc hash key new-list))
          )
      (reduce {} iter)
      )))

(defcheck solution-90c4f6b3
  #(apply merge-with concat (for [x %2] {(% x) [x]})))

(defcheck solution-90da0a81
  (fn groupby* [f xs]
    (letfn [(addto [m k v]
              (assoc m k (conj (get m k []) v)))]
      (reduce (fn [a x] (addto a (f x) x)) {} xs))))

(defcheck solution-9103e65a
  (fn [f coll]
    (reduce #(assoc % (f %2) (conj (or (% (f %2)) []) %2))
      {} coll)))

(defcheck solution-9111a849
  #(reduce (fn [a b] (assoc a (%1 b) (conj (a (%1 b) []) b))) {} %2))

(defcheck solution-919d0e
  (fn [f s] (reduce #(merge-with concat % {(f %2) [%2]}) {} s)))

(defcheck solution-91b6ab59
  #(loop [result {}, coll %2]
     (if-let [item (first coll)]
       (let [k (%1 item) v (result k [])]
         (recur (assoc result k (conj v item)) (next coll)))
       result)))

(defcheck solution-91bc46
  (fn [f s]
    (reduce
      (fn [ans x]
        (let [fx (f x)]
          (assoc ans fx (conj (get ans fx []) x))))
      {}
      s)))

(defcheck solution-92911719
  (fn gb [f coll]
    (apply hash-map
      (apply concat
        (map #(list (f (first %)) (vec %))
          (partition-by f (sort-by f coll)))))))

(defcheck solution-92bd4c73
  (fn group-a-seq [f xs]
    (reduce (fn [m e] (assoc m (f e) (conj (m (f e) []) e))) {} xs)))

(defcheck solution-92e49d24
  (fn [f s]
    (let [tmp (interleave (map f s) (map vector s))]
      (->> tmp (partition 2) (map #(apply hash-map %))
        (apply merge-with (comp vec concat))))))

(defcheck solution-92f5669c
  (fn my-group-by [fun seq]
    (apply merge-with concat
      (map #(hash-map (fun %) [%]) seq))))

(defcheck solution-93011b9b
  (fn [f xs]
    (reduce (fn [m x] (update-in m [(f x)] (fnil conj []) x)) {} xs)))

(defcheck solution-9365cedb
  (fn [m f [e & r]]
    (if (nil? e)
      m
      (recur (assoc m (f e) (conj (m (f e) []) e)) f r))) {}
  )

(defcheck solution-936b866f
  (fn [f s]
    (apply (partial merge-with concat) (map #(assoc {} % %2) (map f s) (map vector s)))))

(defcheck solution-93e36868
  (fn [f c] (reduce #(assoc % (f %2) (conj (% (f %2) []) %2)) {} c)))

(defcheck solution-95374703
  (fn [f coll] (reduce
                 #(assoc % (f %2) (conj (get % (f %2) []) %2))
                 {}
                 coll)))

(defcheck solution-9562b381
  (fn [f s]
    (reduce (fn [m x]
              (let [k (f x) v (m k [])]
                (assoc m k (conj v x))))
      {}
      s)))

(defcheck solution-956e6bc7
  (fn [f x] (reduce #(let [k (f %2) w (or (% k) [])] (assoc % k (conj w %2))) {} x)))

(defcheck solution-95f91aed
  (fn [f m] (into {} (for [i (into #{} (map f m))] [i (filter #(= i (f %)) m)]))))

(defcheck solution-96d5d042
  (fn group_ [fun coll]
    (loop [f fun, col coll, result {}]
      (if (empty? col)
        result
        (let [val  (first col),
              answ (f val),
              vals (get result answ [])]
          (recur f (rest col)
            (assoc result answ (conj vals val)))
          )
        )
      )
    ))

(defcheck solution-96ff477b
  (fn group [f s]
    (if (empty? s)
      {}
      (let [e (first s)
            r (f e)
            m (group f (rest s))
            v (get m r)]
        (if (nil? v)
          (assoc m r [e])
          (assoc m r (cons e v)))))))

(defcheck solution-9702765a
  (fn [f xs]
    (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} xs)))

(defcheck solution-97949f84
  (fn [f n] (reduce #(merge-with into % {(f %2) [%2]}) {} n)))

(defcheck solution-97b9fc1b
  #(reduce
     (fn [m x]
       (let [k (%1 x)]
         (assoc m k (conj (get m k []) x))))
     {}
     %2
     ))

(defcheck solution-97f2b301
  #(->> %2
     (sort-by %)
     (partition-by %)
     (mapcat (fn [v] [(% (first v)) v]))
     (apply assoc {})))

(defcheck solution-980bdeee
  (fn my-group
    [f v]
    (reduce
      (fn [m [mkey mval]]
        (update-in m [mkey] #(concat % [mval])))
      {}
      (map #(list (f %) %) v)
      )))

(defcheck solution-988e8c1b
  (fn [f col] (reduce (fn [acc x] (let [k        (f x)
                                        currvals (get acc k [])
                                        newvals  (conj currvals x)]
                                    (assoc acc k newvals)))
                {} col)))

(defcheck solution-98b76d1c
  (fn [f coll]
    (apply merge-with concat (map (fn [x] {(f x) [x]}) coll))))

(defcheck solution-98f525df
  (fn gr
    ([f l] (gr {} f l))
    ([m f l]
     (if
      (empty? l) m
                 (gr
                   (let [v (first l)
                         k (f v)
                         g (get m k [])
                         ]
                     (assoc m k (conj g v)))
                   f
                   (rest l)
                   )))
    ))

(defcheck solution-9928a437
  #(apply merge-with into (for [v %2] {(% v) [v]})))

(defcheck solution-9a1362b6
  (fn group-seq
    [f s]
    (loop [[h & t] s
           accum {}]
      (let [new-accum (if (contains? accum (f h))
                        (assoc accum (f h) (conj (accum (f h)) h))
                        (assoc accum (f h) [h]))]
        (if (nil? t)
          new-accum
          (recur t new-accum))))))

(defcheck solution-9a5aa32b
  (fn [f c]
    (reduce #(assoc %
               (f %2)
               (conj (get % (f %2) []) %2))
      {}
      c)))

(defcheck solution-9ab01a4d
  #(reduce (partial merge-with concat) (map (fn [v] {(% v) [v]}) %2)))

(defcheck solution-9b04358c
  (fn myGroup
    [pred x]
    (reduce (fn [m s] (let [fst (first s)
                            lst (second s)]
                        (if (contains? m fst)
                          (assoc m fst (conj (get m fst) lst))
                          (assoc m fst (vector lst)))))
      {}
      (map vector (map pred x) x))))

(defcheck solution-9b69d8fd
  (fn [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-9b7b13f8
  #(reduce (fn [m v] (let [k (%1 v)] (assoc m k (conj (get m k []) v)))) {} %2))

(defcheck solution-9bddd4da
  (fn group-by-function [f s]
    (reduce
      #(let [value        (f %2)
             existing     (%1 value)
             new-existing (if (empty? existing) [] existing)]
         (assoc %1 value (conj new-existing %2)))
      (hash-map)
      s)))

(defcheck solution-9be213c6
  (fn grpby [pred coll]
    (reduce
      (fn [m e]
        (update-in m [(pred e)]
          (fn [v]
            (if v (conj v e) [e]))))
      {}
      coll)))

(defcheck solution-9c35ddfc
  (fn [f coll] (loop [coll coll retr {}]
                 (if (empty? coll) retr
                                   (let [[v & more] coll
                                         k (f v)]
                                     (if (retr k)
                                       (recur more (assoc retr k (conj (retr k) v)))
                                       (recur more (assoc retr k [v]))))))))

(defcheck solution-9c39241f
  (fn [f s]
    (loop [xs s
           r  {}]
      (if (empty? xs)
        r
        (let [k (first xs)
              v (f k)]
          (recur (rest xs)
            (conj r {v (conj (get r v []) k)})))))))

(defcheck solution-9d86e223
  (fn [f coll]
    (reduce
      (fn [out x]
        (let [k    (f x)
              curr (or (out k) [])]
          (assoc out k (conj curr x))))
      {} coll)))

(defcheck solution-9db6a292
  (fn [f s]
    (reduce
      (fn [a b]
        (if (contains? a (first b))
          (assoc (dissoc a (first b)) (first b) (conj (a (first b)) (last b)))
          (conj a [(first b) [(last b)]])))
      {}
      (map #(conj [] (f %) %) s)
      )))

(defcheck solution-9e32c63
  #(reduce (fn [m [k v]] (assoc m k (conj (m k []) v)))
     {}
     (map vector (map % %2) %2)))

(defcheck solution-9e4dacab
  (fn [f coll]
    (reduce #(let [k (f %2)]
               (assoc %1 k (conj (get %1 k []) %2)))
      {} coll)))

(defcheck solution-9e56cbb
  (fn hey [c s] (apply merge-with concat (map #(hash-map (c %) (cons % [])) s))))

(defcheck solution-9e76c039
  (fn [f s] (reduce #(update-in %1 [(f %2)] (fnil (fn [v] (conj v %2)) [])) {} s)))

(defcheck solution-9e7f6d29
  (fn gb [f xs] (apply (partial merge-with into) (map #(hash-map (f %) [%]) xs))))

(defcheck solution-9e86e12a
  (fn [f c] (let [s (set (map f c))] (zipmap s (map #(vec (filter (fn [x] (= % (f x))) c)) s)))))

(defcheck solution-9ea8c04f
  (fn [f s]
    (reduce
      #(merge-with concat %1 (hash-map (f %2) [%2]))
      {} s)))

(defcheck solution-9f154dc7
  (fn [f vs] (reduce (fn [h v] (merge-with concat h {(f v) [v]})) {} vs)))

(defcheck solution-9f4ef71d
  (fn [f xs] (reduce #(merge-with concat %1 {(f %2) [%2]}) {} xs)))

(defcheck solution-9fff4d4e
  (fn
    [f coll]
    (reduce
      (fn [s x]
        (let [v (f x)]
          (if (not (contains? s (f x)))
            (assoc s v [x])
            (update-in s [v] conj x))))
      {}
      coll)))

(defcheck solution-a05132a
  (fn grpby [f, s]
    (loop [i s, r {}]
      (if (empty? i)
        r
        (let [k (first i)
              v (f k)
              o (r v)]
          (if o
            (recur (rest i) (assoc r v (conj o k)))
            (recur (rest i) (assoc r v [k]))))))))

(defcheck solution-a06bd2d
  (fn [func coll]
    (reduce #(assoc %1 (first %2) (into (get %1 (first %2)) (next %2)))
      (reduce #(assoc %1 %2 []) {} (map func coll))
      (map vector (map func coll) coll))))

(defcheck solution-a08e11d6
  (fn [f coll]
    (let [keys (map f coll)]
      (reduce merge
        (for [k keys]
          {k (filter #(= k (f %)) coll)})))))

(defcheck solution-a11428a1
  (fn [f s]
    (reduce into
      (map #(hash-map %
              (filter (fn [i] (= % (f i))) s))
        (distinct (map f s))))))

(defcheck solution-a134d58e
  (fn [f coll]
    (if-let [s (seq coll)]
      (reduce #(let [x (f %2) elem (%1 x [])]
                 (into %1 {x (conj elem %2)}))
        {} coll)
      {})))

(defcheck solution-a3833a77
  (fn my-group [f lis] (into {}
                         (for [x (distinct ((fn [f s] (map f s)) f lis))]
                           [x (filter #(= (f %) x) lis)]))))

(defcheck solution-a391a246
  (fn [f s]
    (let [t  (map (juxt f identity) s)
          ug (fn [m [gr v]] (update-in m [gr] #(conj (vec %) v)))]
      (reduce ug {} t)
      )))

(defcheck solution-a3b35e1
  #(loop [ixs %2 rs {}] (if (empty? ixs) rs (recur (rest ixs) (if (contains? rs (%1 (first ixs))) (assoc rs (%1 (first ixs)) (conj (get rs (%1 (first ixs))) (first ixs))) (assoc rs (%1 (first ixs)) [(first ixs)]))))))

(defcheck solution-a3dcb6c6
  (fn [f coll]
    (reduce
      (fn [hmap [k v]]
        (let [value (get hmap k)]
          (if (nil? value)
            (assoc hmap k (vector v))
            (assoc hmap k (conj value v)))))
      {}
      (map #(vector (f %) %) coll))))

(defcheck solution-a410c7e7
  (fn [f xs]
    (apply (partial merge-with concat) (map #(hash-map (f %) [%]) xs))))

(defcheck solution-a42ed32d
  (fn group-by-2 [f coll] (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} coll)))

(defcheck solution-a43b99f3
  (fn [op col] (reduce (fn [res val]
                         (let [grp (op val)
                               lst (get res grp [])]
                           (assoc res grp (conj lst val))))
                 {} col)))

(defcheck solution-a46d64b8
  (fn [f c] (apply merge-with concat (map (fn [i] {(f i) [i]}) c))))

(defcheck solution-a4865cc1
  (letfn [(conj-in
            [f coll v]
            (update-in coll [(f v)]
              (fnil #(conj % v) [])))]

    (fn group-with [f coll]
      (reduce (partial conj-in f)
        {} coll))))

(defcheck solution-a4c4f213
  (fn [f v] (into (sorted-map) (map #(vector (f (first %)) (vec %)) (partition-by f (sort v))))))

(defcheck solution-a4d09ef3
  (fn [f s] (reduce (fn [m v] (let [k (f v) oldv (get m k [])] (assoc m k (conj oldv v)))) {} s)))

(defcheck solution-a4df59fc
  (fn my-group-by [fnn values]
    (loop [cur  {}
           left values]
      (if (empty? left)
        cur
        (let [val   (fnn (first left))
              entry (conj (cur val []) (first left))]
          (recur (assoc cur val entry)
            (rest left)))))))

(defcheck solution-a4e453e2
  #(apply merge-with concat (map hash-map (map %1 %2) (map vector %2))))

(defcheck solution-a595d7db
  (fn [f l] (reduce (fn [m v] (let [k (f v) vs (get m k [])] (assoc m k (conj vs v)))) {} l)))

(defcheck solution-a5a7d7ce
  (fn [f xs]
    (loop [xs xs res {}]
      (if (empty? xs) res
                      (let [[x & xs] xs
                            k (f x)]
                        (recur xs
                          (assoc res k
                                     (if (res k)
                                       (conj (res k) x)
                                       [x]))))))))

(defcheck solution-a60b2d8a
  (fn [f col]
    (reduce #(conj % [(f %2) (conj (if-let [x (% (f %2))] x []) %2)]) {} col)))

(defcheck solution-a6286f50
  (fn gs [f s]
    (reduce (fn rf [x [k v]]
              (update-in x [k] (fnil (fn [ov]
                                       (conj ov v)) []))
              ) {} (map #(vec [(f %) %]) s))))

(defcheck solution-a657d721
  (fn group-seq [f col]
    (reduce #(assoc %1 (f %2) (conj (apply vector (%1 (f %2))) %2)) {} col)))

(defcheck solution-a693f547
  (fn myGroupBy [f arg] (reduce (fn [x y] (if (nil? (x (f y))) (merge x {(f y) (vector y)}) (update-in x [(f y)] #(vec (concat % (vector y)))))) {} arg)))

(defcheck solution-a6b23c3c
  (fn [f col]
    (reduce
      (fn [prev-col x]
        (let [fx (f x)]
          (assoc-in prev-col [fx]
            (if (contains? prev-col fx)
              (concat (get prev-col fx) [x])
              [x]))))
      {} col)))

(defcheck solution-a7982dff
  (fn [x y] (reduce #(if (contains? %1 (x %2))
                       (assoc %1 (x %2) (conj (%1 (x %2)) %2))
                       (assoc %1 (x %2) (vector %2))) {} y)))

(defcheck solution-a80bb013
  (fn [f coll]
    (reduce
      (fn [m e] (assoc m (f e) (conj (get m (f e) []) e)))
      {}
      coll)))

(defcheck solution-a89cee21
  (fn [f s] (reduce (fn [m [k v]] (let [h (get m k [])] (assoc m k (conj h v))))

              (cons {} (map vector (map f s) s)))))

(defcheck solution-a8ea9c70
  #(into {} (map (fn [a] [(% (first a)) a]) (partition-by % (sort-by % %2)))))

(defcheck solution-a8fa2c37
  (fn [f lst]
    (reduce
      (fn [m e] (update-in m [(f e)] #(conj (or % []) e)))
      {}
      lst)))

(defcheck solution-a8fac94b
  #(reduce (fn [result val]
             (let [fv (%1 val)]
               (assoc result fv (conj (get result fv []) val)))) {} %2))

(defcheck solution-a903977f
  (fn [f s]
    (reduce
      #(let [k (f %2)] (assoc % k (conj (% k []) %2)))
      {}
      s)))

(defcheck solution-a96884e4
  (fn [f s]
    (let [m (map (fn [e] [(f e) e]) s)]
      (reduce (fn [r e]
                (let [me (r (e 0))]
                  (if (nil? me)
                    (assoc r (e 0) [(e 1)])
                    (assoc r (e 0) (conj me (e 1))))))
        {}
        m))))

(defcheck solution-a99d14fa
  (fn [a b] (apply merge-with concat (map (partial apply hash-map)
                                       (partition 2 (interleave (map a b)
                                                      (map vector b)))))))

(defcheck solution-a9ce3a6a
  (fn [f coll]
    (reduce #(let [k (f %2)]
               (assoc %1 k (conj (get %1 k []) %2))) {} coll)))

(defcheck solution-aa43e4fa
  (fn [fun s]
    (reduce
      (fn [acc it]
        (let [k (fun it)]
          (assoc acc k (conj (acc k []) it))))
      {}
      s)))

(defcheck solution-aa9bb8e3
  (fn grpr-2 [f s]
    (reduce (fn [accum item]
              (let [r             (f item)
                    items-for-key (get accum r)]
                (assoc accum r (if items-for-key
                                 (conj items-for-key item) [item])))) {} s)))

(defcheck solution-aa9df96c
  (fn [f coll]
    (loop [l      coll
           groups {}]
      (if (empty? l)
        groups
        (recur (rest l)
          (let [cur   (first l)
                k     (f cur)
                group (conj (get groups k []) cur)]
            (assoc groups k group)))))))

(defcheck solution-aaa15b43
  (fn grup [f coll]
    (let [keys   (map f coll)
          tuples (map vector keys coll)]
      (loop [res (into {} (map vector keys (repeat [])))
             [[k v :as t] & ts] tuples]
        (if (nil? t)
          res
          (recur (update-in res [k] conj v)
            ts))))))

(defcheck solution-aae5a1fc
  #(reduce (fn [r i] (let [fi (% i) v (r fi)]
                       (assoc r fi (if v (conj v i) [i]))))
     {}
     %2))

(defcheck solution-aaf3cfbc
  (fn [f coll]
    (let [group (->> coll (sort-by f) (partition-by f) (map vec))
          by    (map #(-> % first f) group)]
      (zipmap by group))))

(defcheck solution-aaf3e066
  (fn [f c] (reduce (fn [a b] (update-in a [(f b)] #(conj (or % []) b))) {} c)))

(defcheck solution-ab93d3c6
  (fn [fun seq]
    (letfn
     [(categorize
        [map val]
        (let [key   (fun val)
              entry (if (contains? map key)
                      (conj (map key) val)
                      (conj [] val))]
          (assoc map key entry)))]
      (reduce categorize {} seq))))

(defcheck solution-abac2c4a
  (fn [f xs]
    (reduce #(let [v (f %2)] (assoc % v (conj (get % v []) %2))) {} xs)))

(defcheck solution-ac2ede1e
  (fn [f s] (reduce #(assoc %1 (f %2) (concat (%1 (f %2)) [%2])) {} s)))

(defcheck solution-aca0abfc
  (fn [f s] (apply merge-with into (map (fn [x] {(f x) [x]}) s))))

(defcheck solution-acb8c510
  (fn group [f coll]
    (let [key #(-> % first first)
          val #(map second %)]
      (->> coll
        (map #(list (f %) %))
        (sort-by first)
        (partition-by first)
        (map #(vector (key %) (val %)))
        (into {})))))

(defcheck solution-acdb9f0f
  (fn [f col]
    (let [konj #(if (nil? %) [%2] (conj % %2))]
      (reduce (fn [m v]
                (let [k (f v)]
                  (assoc m k (konj (m k) v))
                  )) {} col))))

(defcheck solution-ad125cc2
  (fn myfilt [f col] (reduce (fn [acc el] (let [key (f el)]
                                            (assoc acc key (conj (get acc key (vector)) el))
                                            ))
                       {} col)))

(defcheck solution-ad1b716c
  (fn [f s]
    (apply merge-with concat (map #(array-map (f %1) [%1]) s))))

(defcheck solution-ad3497a5
  (fn [f col]
    (reduce
      #(let [m %1 v %2 k (f %2) lst (m k)]
         (assoc m k (if lst (conj lst v) [v]))) {} col)))

(defcheck solution-ad80d634
  (fn [f s]
    (reduce merge
      (for [e (into #{} (map f s))]
        (hash-map e (filter #(= e (f %)) s))))))

(defcheck solution-adc65763
  (fn gb [f col] (
                   reduce #(assoc %1 (f %2) (concat (get %1 (f %2)) [%2])) {} col
                   )))

(defcheck solution-ae09753a
  (fn [f coll]
    (reduce
      (fn [ret e]
        (update-in ret [(f e)] #(into [] (conj % e))))
      {}
      coll)))

(defcheck solution-ae4b81fc
  (fn
    [f lst]
    (let [opts (distinct (map f lst))]
      (into {} (map (fn [opt] (conj [opt] (filter #(= opt (f %)) lst))) opts))
      )))

(defcheck solution-ae4d1929
  (fn [f col]
    (reduce
      (fn [a x]
        (update-in a
          [(f x)]
          (fnil conj [])
          x))
      {}
      col)))

(defcheck solution-aec2f29f
  (fn [f xs]
    (loop [xs xs m {}]
      (if xs
        (let [val (first xs)
              key (f val)]
          (recur (next xs)
            (if-let [vals (m key)]
              (assoc m key (conj vals val))
              (assoc m key (vector val)))))
        m))))

(defcheck solution-aec77452
  (fn groupby [f s]
    (reduce
      #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2))
      {} s)))

(defcheck solution-aed49b8c
  #(reduce (fn [a e] (assoc a (% e) (conj (a (% e) []) e))) {} %2))

(defcheck solution-aef5c373
  (fn [gfn coll]
    (loop [m (transient {}) es coll]
      (cond
        (empty? es) (persistent! m)
        :default
        (let [k (gfn (first es))]
          (recur (assoc! m k (conj (get m k []) (first es))) (rest es)))))))

(defcheck solution-aef93fcf
  (fn [f s]
    (reduce (fn [x y]
              (let [k (f y)]
                (merge-with concat x {k [y]})
                )
              ) (concat [{}] s))

    ))

(defcheck solution-af185ee1
  (fn [f l]
    (reduce (fn [m e]
              (let [v (f e)]
                (if (nil? (m v))
                  (assoc m v (vector e))
                  (assoc m v (conj (m v) e))
                  )
                )) {} l)
    ))

(defcheck solution-af24cb33
  (fn [f xs] (reduce #(merge-with concat %1 %2) (map #(-> {(f %) [%]}) xs))))

(defcheck solution-af43c77d
  (fn [func seq]
    (loop [seq    seq
           result {}]
      (if (empty? seq)
        result
        (let [[head & tail] seq
              key (func head)]
          (recur tail
            (into result
              {key
               (if (contains? result key)
                 (conj (result key) head)
                 [head])})))))))

(defcheck solution-afbb411
  #(apply merge-with concat
     (for [x %2]
       {(% x) [x]})))

(defcheck solution-b013ab9e
  (fn [f, x] (reduce (fn [m v] (conj m {(f v), (conj (get m (f v)) v)})) (zipmap (map f x) (repeat [])) x)))

(defcheck solution-b055d84d
  (fn my-grouping [f col]
    (loop [s {} col col]
      (if (empty? col)
        s
        (let [z (f (first col))
              i (filter #(= z (f %)) col)
              j (filter #(not= z (f %)) col)]
          (recur (conj s [z i]) j))))))

(defcheck solution-b093c5eb
  (fn [f l]
    (loop [ans {} tmpl l]
      (if (empty? tmpl)
        ans
        (if (contains? ans (f (first tmpl)))
          (recur (assoc ans (f (first tmpl)) (reverse (conj (reverse (get ans (f (first tmpl)))) (first tmpl)))) (rest tmpl))
          (recur (assoc ans (f (first tmpl)) (list (first tmpl))) (rest tmpl)))))))

(defcheck solution-b0bfb27e
  (fn [f xs]
    (reduce (fn [m v]
              (let [k (f v)]
                (assoc m k (conj (into [] (get m k)) v))))
      {}
      xs)))

(defcheck solution-b1f99f47
  (fn [f coll]
    (zipmap (set (map f coll)) (for [x (set (map f coll))]
                                 (for [y coll
                                       :when (= x (f y))]
                                   y)))))

(defcheck solution-b277c294
  (fn [f s]
    (apply merge-with concat (map #(hash-map (f %1) [%1]) s))))

(defcheck solution-b2cd7a57
  (fn g [f s]
    (reduce (fn [r v] (let [add #((fnil conj []) % %2)]
                        (update-in r [(f v)] add v))) {} s)))

(defcheck solution-b2d05f16
  (fn [f s]
    (reduce #(let [r (f %2)]
               (assoc %1 r (conj (get %1 r []) %2))) {} s)))

(defcheck solution-b2f7ec68
  (fn my_group-by
    ([f vs]
     (my_group-by f vs {}))
    ([f vs result]
     (if (empty? vs)
       result
       (recur f (rest vs) (assoc result (f (first vs)) (conj (or (get result (f (first vs))) []) (first vs))))))))

(defcheck solution-b31870cb
  (fn [f s]
    (let [
          ; Create seq of maps ( {(f x1) -> [x1]}, ...)
          y-vx-map (map #(hash-map (f %1) (vector %)) s)]

      ; Merge maps, combining vals of dup keys
      (apply merge-with concat y-vx-map))))

(defcheck solution-b33e7a8f
  (fn [f v]
    ((fn my-group-by [f v m]
       (if (empty? v)
         m
         (let [k (f (first v))]
           (recur f
             (rest v)
             (assoc m k (if (contains? m k)
                          (conj (m k) (first v))
                          [(first v)]))))))
     f v {})))

(defcheck solution-b3a0e8ce
  (fn m [f c]
    (reduce (partial merge-with concat) (map #(hash-map (f %) [%]) c))))

(defcheck solution-b3a9497d
  #(reduce (fn [h x] (let [k (% x)] (assoc h k (conj (h k []) x)))) {} %2))

(defcheck solution-b3cc6e54
  (fn [f s]
    (reduce (fn [h v]
              (let [k (apply f [v])]
                (assoc h k (conj (get h k []) v))))
      {}
      s)))

(defcheck solution-b3fd66c7
  (fn gb [f v]
    (let [kvs (map vector (map f v) v)
          p   (fn [x] {(x 0) [(x 1)]})]
      (apply merge-with concat (map p kvs)))))

(defcheck solution-b47cd67b
  (fn [f xs] (apply merge-with concat (map #(hash-map (f %) (list %)) xs))))

(defcheck solution-b47df72f
  (fn group [f s]
    (loop [result {} items s]
      (if (empty? items)
        result
        (let [current-item (first items) value (f current-item) map-value (get result value [])]
          (recur
            (assoc result value (conj map-value current-item))
            (rest items)
            )
          )
        )
      )
    ))

(defcheck solution-b4a19fcb
  (fn gb [f xs]
    (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} xs)))

(defcheck solution-b4b0365
  (fn group-by' [f s]
    (reduce
      (fn [m v]
        (let [k (f v)]
          (assoc m k (conj (m k []) v))))
      {} s)))

(defcheck solution-b58a7173
  (fn [p xs]
    (let [f
          (fn [acc x]
            (if (contains? acc (p x))
              (update-in acc [(p x)] #(conj % x))
              (conj acc [(p x) [x]])))]

      (reduce f {} xs))))

(defcheck solution-b6207ac9
  (fn [f values]
    (loop [m   {}
           h   (first values)
           res (f h)
           t   (rest values)]
      (if (nil? h)
        m
        (recur
          (assoc m (f h)
                   (conj (into []
                           (get m (f h))) h))

          (first t)
          (f h)
          (rest t))))))

(defcheck solution-b63129ca
  (fn my-group-by [f coll]
    (reduce
      (fn [result item]
        (update-in result
          [(f item)]
          (fn [old-val]
            (conj (if (nil? old-val) [] old-val) item))))
      {} coll)))

(defcheck solution-b639c493
  (fn [f s]
    (reduce
      (fn [vecmap [ek ev]]
        (assoc
         vecmap
          ek
          (conj (get vecmap ek []) ev)))
      {}
      (map #(vector (f %) %) s))))

(defcheck solution-b6bc99eb
  (fn [f s]
    (let [k (set (map f s))]
      (zipmap k (map #(filter (fn [x] (= % (f x))) s) k)))))

(defcheck solution-b7131a1d
  (fn [f coll]
    (apply merge-with #(vec (concat %1 %2)) (map (fn [x] {(f x) [x]}) coll))))

(defcheck solution-b7460cec
  ;;update is added by 1.7
  (fn
    [f s]
    (loop [m {} s s]
      (if (empty? s) m
                     (let [fir (first s)]
                       (recur (assoc m (f fir)
                                       (conj (vec (m (f fir))) fir))
                         (next s)))))))

(defcheck solution-b8296209
  (fn gseq [f s]
    (let [results (set (map f s))]
      (zipmap results (map (fn [r] (filter #(= (f %) r) s)) results)))))

(defcheck solution-b8a5e150
  (fn [f x]
    (reduce
      #(merge-with
         concat % %2) {}
      (map #(hash-map (f %) [%]) x))))

(defcheck solution-b9615944
  (fn [f s]
    (loop [r (map f s)
           m {}
           s s]
      (if (empty? r)
        m
        (let [rr (rest r)
              fr (first r)
              rs (rest s)
              fs (first s)]
          (if (m (first r))
            (recur rr (assoc m fr (conj (m fr) fs)) rs)
            (recur rr (assoc m fr (conj [] fs)) rs)))))))

(defcheck solution-b9c6b4b9
  (fn [f coll]
    (apply merge-with into (map (fn [x] {(f x) [x]}) coll))))

(defcheck solution-ba0bc59e
  (fn [f s] (reduce #(assoc %1 (f %2) (vec (concat (%1 (f %2)) [%2]))) {} s)))

(defcheck solution-ba7dcc7c
  (fn [f s] (apply merge-with concat (map #(assoc {} (f %) [%]) s))))

(defcheck solution-bae2ad87
  (fn [f coll]
    (reduce (fn [res new]
              (let [v (f new)]
                (merge res
                  (if (nil? (res v))
                    {v [new]}
                    {v (conj (res v) new)}))))
      {} coll)))

(defcheck solution-baefefcf
  (fn F [f l] (reduce #(update-in %1 [(f %2)] (fnil conj []) %2) {} l)))

(defcheck solution-bb3a9954
  (fn [f xs]
    (reduce #(update-in % [(first %2)]
               (fn [v c] (if (nil? v) [c] (conj v c))) (second %2)) {}
      (map list (map f xs) xs))))

(defcheck solution-bb3bcaa
  (fn [f xs]
    (reduce
      (fn [m x]
        (update-in m [(f x)] #(conj (or % []) x)))
      {} xs)))

(defcheck solution-bbc62743
  (fn [f xs]
    (let [keys       (seq (set (map f xs)))
          that-match (fn [k xs] (filter #(= (f %) k) xs))]
      (zipmap keys (map #(that-match % xs) keys)))))

(defcheck solution-bbe38e7a
  (fn [f s] (apply merge-with into (map #(hash-map (f %) [%]) s))))

(defcheck solution-bc16dfe7
  (fn sq [f xs]
    (if (seq xs)
      (let [[x & s] xs]
        (conj (sq f s)
          {(f x) (cons x
                   ((sq f s) (f x)))}))
      {})))

(defcheck solution-bc1bf1d0
  (fn newgroupby [f s]
    (reduce #(assoc %1 (f %2) (conj (get %1 (f %2)) %2))
      (zipmap (distinct (map f s)) (repeat (count (distinct (map f s))) []))
      s)))

(defcheck solution-bc3b221
  (fn [f s] (zipmap (distinct (map f s)) (partition-by f (sort-by f s)))))

(defcheck solution-bc45e027
  ;(fn my-group [f s] (
  ;reduce (fn [a b] (update a (f b) (partial cons b))) {} (reverse s)
  ;))

  (fn my-group [f s] (
                       reduce (fn [a b] (into a [[(f b) (cons b (get a (f b)))]])) {} (reverse s)
                       )))

(defcheck solution-bca8a691
  #(reduce (fn [m e] (assoc m (% e) (conj (m (% e) []) e))) {} %2))

(defcheck solution-bcf2a65b
  (fn [f coll]
    (reduce
      #(let [k (f %2)
             v (%1 (f %2) [])]
         (assoc %1 k (conj v %2)))
      {}
      coll)))

(defcheck solution-bd2ab320
  (fn [f s] (apply merge-with concat (map #(hash-map (f %) [%]) s))))

(defcheck solution-bdc0456
  (fn [f coll] (reduce #(if (% (f %2))
                          (assoc % (f %2) (conj (% (f %2)) %2))
                          (assoc % (f %2) (vector %2))) {} coll)))

(defcheck solution-bdfc9325
  (fn [f s]
    (reduce (fn [m x]
              (let [y (f x)]
                (assoc m y (conj (get m y []) x))))
      {} s)))

(defcheck solution-be0bcdd
  (fn [f s] (into {} (map (fn [y] [y (filter #(= y (f %)) s)]) (distinct (map f s))))))

(defcheck solution-be9aa8b5
  (fn [f l]
    (loop [l   l
           acc {}]
      (if (zero? (count l))
        acc
        (recur (rest l)
          (if (contains? acc (f (first l)))
            (assoc acc (f (first l)) (conj (get acc (f (first l))) (first l)))
            (assoc acc (f (first l)) [(first l)])))))))

(defcheck solution-bf4e6d04
  (fn [f coll]
    (reduce #(merge-with into %1 {(f %2) [%2]}) {} coll)))

(defcheck solution-c02214c1
  (fn [func input]
    (loop [result    {}
           remaining input]
      (if (empty? remaining)
        result
        (let [val (func (first remaining))]
          (recur (if (contains? result val)
                   (update-in result [val] #(conj % (first remaining)))
                   (assoc result val [(first remaining)]))
            (rest remaining)))))))

(defcheck solution-c0290483
  (fn [f c]
    (reduce (fn [m v] (let [k (f v) o (m k)] (assoc m k (concat o [v])))) {} c)))

(defcheck solution-c02a9e
  (fn [f col]
    (reduce #(update-in %1 [(f %2)] (comp vec conj) %2)
      {}
      col)))

(defcheck solution-c0341832
  (fn [func lst]
    ((fn [rest-lst rslt-map]
       (if (empty? rest-lst)
         rslt-map
         (let [func-rslt (func (first rest-lst))]
           (recur (rest rest-lst)
             (assoc rslt-map
               func-rslt
               (conj (get rslt-map func-rslt [])
                 (first rest-lst))
               )
             )
           )
         )
       )
     lst {}
     )
    ))

(defcheck solution-c03ede37
  (fn [f col]
    (reduce
      (fn [acc item]
        (let [k (f item)]
          (conj acc {k (if (acc k) (conj (acc k) item) (vector item))})
          )
        )
      {}
      col
      )
    ))

(defcheck solution-c0481af0
  (fn [f xs]
    (reduce #(merge-with concat % %2) (map (fn [x] {(f x) [x]}) xs))))

(defcheck solution-c06a3102
  (fn gb [f xs]
    (if (seq xs)
      (let [val (first xs)]
        (merge-with concat {(f val) [val]} (gb f (rest xs)))))))

(defcheck solution-c0affeee
  (fn [f c]
    (reduce
      #(let [k (f %2)]
         (assoc % k (conj (% k []) %2)))
      {}
      c)))

(defcheck solution-c0bafafb
  (fn [f c] (reduce #(let [v (f %2)] (assoc %1 v (conj (get %1 v []) %2))) {} c)))

(defcheck solution-c179ec5f
  (fn [f coll]

    (reduce
      (fn [d e]
        (merge-with (partial reduce conj) d {(f e) [e]})
        )
      {}
      coll
      )))

(defcheck solution-c1a8f353
  #(reduce
     (fn [m v]
       (let [k (% v)]
         (assoc m k (conj (m k []) v))))
     {}
     %2))

(defcheck solution-c1af857c
  #(reduce
     (fn [ret x]
       (let [k (%1 x)]
         (assoc ret k (conj (get ret k []) x)))) {} %2))

(defcheck solution-c24c8cff
  (fn [f s]
    (reduce (partial merge-with into) (map (fn [v] (apply hash-map v))
                                        (partition 2
                                          (interleave
                                            (map f s)
                                            (map vector s)))))))

(defcheck solution-c2668d42
  (fn [f s]
    (reduce (fn [acc v]
              (let [k (f v)]
                (assoc acc k (if (contains? acc k)
                               (conj (acc k) v)
                               [v]))))
      {}
      s)))

(defcheck solution-c33f5199
  (fn [f coll] (reduce (fn [k ret] (assoc k (f ret) (conj (k (f ret) []) ret))) {} coll)))

(defcheck solution-c35196f1
  (fn [f s]
    (into {} (map
               #(vector % (filter (fn [x] (= % (f x))) s))
               (into #{} (map f s))))))

(defcheck solution-c3873e18
  (fn [f s]
    (reduce
      #(assoc %1 (f %2)
                 (conj (get %1 (f %2) []) %2))
      {}
      s)))

(defcheck solution-c391183f
  (fn [f v] (reduce #(if (% (first %2))
                       (assoc % (first %2) (concat (% (first %2)) (rest %2)))
                       (assoc % (first %2) (vec (rest %2)))) {}
              (map (fn [x] [(f x) x]) v))))

(defcheck solution-c3be804c
  (fn [f coll]
    (reduce
      (fn [acc x]
        (update-in acc [(f x)] (comp vec #(conj % x))))
      {}
      coll)))

(defcheck solution-c3f031b6
  (fn grp [f s]
    (loop [ks (map f s) vs s out {}]
      (if (empty? ks)
        out
        (recur (rest ks) (rest vs)
          (assoc out (first ks)
                     (concat (get out (first ks)) [(first vs)])
                     )
          )
        )
      )
    ))

(defcheck solution-c449d898
  #(reduce (fn [m e] (assoc m (% e) `[~@(m (% e)) ~e])) {} %2))

(defcheck solution-c46d7c68
  (fn my-groupby
    [f xs]
    (letfn [(add-entry [m [k v]]
              (assoc m k (concat (m k) (vector v))))
            (make-map [m xs]
              (if (not-empty xs)
                (make-map (add-entry m (first xs)) (rest xs))
                m))]
      (make-map {} (map vector (map f xs) xs)))))

(defcheck solution-c5349256
  (fn mygb [f s]
    (let [vals (set (map f s))]
      (zipmap vals (map (fn [x] (filter (fn [y] (= x (f y))) s)) vals)))))

(defcheck solution-c54216ac
  (fn [f s]
    (reduce #(assoc % (f %2) (conj (% (f %2) []) %2)) {} s)))

(defcheck solution-c54dd34b
  (fn [pred coll]
    (reduce (fn [m x]
              (let [k (pred x)]
                (assoc m k (conj (get m k []) x))))
      {} coll)))

(defcheck solution-c59d19f4
  (fn [p c] (reduce (fn [x y]
                      (assoc x (p y) (conj (get x (p y) []) y))) (cons {} c))))

(defcheck solution-c5cd96c6
  (fn grouper [f s]
    (into {} (for [x (distinct (map f s))]
               [x (apply vector (filter #(= x (f %)) s))]))))

(defcheck solution-c6016dfd
  (fn f [g s]
    (if (= 1 (count s))
      {(g (last s)) [(last s)]}
      (let [m (f g (butlast s)) old (get m (g (last s)))]
        (assoc m (g (last s))
                 (if old
                   (conj old (last s))
                   [(last s)]))))))

(defcheck solution-c64dcca6
  (fn [f coll]
    (reduce (fn [acc x]
              (merge-with (comp vec concat) acc {(f x) [x]}))
      {}
      coll)))

(defcheck solution-c67ea640
  (fn [f s] (apply hash-map
              (mapcat (juxt #(-> % first first)
                        #(vec (map second %)))
                (partition-by first
                  (sort (map (juxt f identity)
                          s)))))))

(defcheck solution-c6818624
  (fn [f k] (apply merge-with concat (map (fn [x] {(f x) [x]}) k))))

(defcheck solution-c6ee14af
  #(reduce (fn [h e] (update-in h [(%1 e)] (comp vec conj) e)) {} %2))

(defcheck solution-c7093274
  (fn [f xs]
    (letfn [(go [rs ys]
              (if (empty? ys)
                rs
                (let [[y & ys'] ys
                      r (f y)]
                  (go (update-in rs [r] (fnil #(conj % y) []))
                    ys'))))]
      (go {} xs))))

(defcheck solution-c713cb0e
  (fn [f s] (into {} (map #(vector (f (first %)) (into [] %)) (partition-by f (sort-by f s))))))

(defcheck solution-c74cf280
  (fn [p xs]
    (apply (partial merge-with concat) (map (fn [x] (hash-map (p x) [x])) xs))))

(defcheck solution-c755a0e1
  (fn [f v]
    (apply merge-with concat
      (map
        (fn [x] {(f x) [x]}) v))))

(defcheck solution-c7bf6843
  (fn [f l]
    (reduce
      #(update-in %1 [(first %2)]
         (fn [a] (conj (vec a) (second %2))))
      {}
      (map #(list (f %) %) l))))

(defcheck solution-c7e205f3
  (fn [f s]
    (apply merge-with concat {} (map #(hash-map (f %) [%]) s))))

(defcheck solution-c82ef108
  (fn my-gb [f seq]
    (let [kv-seq (for [x seq] [(f x) [x]])]
      (reduce #(assoc % (first %2)
                        (concat (% (first %2)) (second %2)))
        {} kv-seq))))

(defcheck solution-c919aac0
  (fn [f v]
    (apply merge-with concat (for [x v] {(f x) [x]}))))

(defcheck solution-c952460a
  (fn [pred xs]
    (apply merge-with concat
      (map #(hash-map (pred %) [%]) xs))))

(defcheck solution-c9701c38
  (fn [f coll]
    (reduce #(merge-with concat %1 {(f %2) [%2]}) {} coll)))

(defcheck solution-c9805a0
  (fn [pred coll]
    (apply (partial merge-with concat) (map #(array-map (pred %1) [%1]) coll))))

(defcheck solution-c9fee2ee
  (fn [f coll]
    (reduce #(assoc %1 (%2 0) (conj (get %1 (%2 0) []) (%2 1))) {}
      (map (fn [x] (vector (f x) x)) coll))))

(defcheck solution-ca54a07e
  #(reduce
     (fn [m e] (assoc m (% e) (conj (m (% e) []) e)))
     {}
     %2))

(defcheck solution-ca65914e
  (fn [f vs]
    (let [pairs  (map #(vector (f %) %) vs)
          groups (map (fn [[k v]]
                        [k (map second (filter (fn [[k' v']] (= k k')) pairs))])
                   pairs)]
      (into {} groups))))

(defcheck solution-caf8fe7a
  (fn gb [f s]
    (reduce (fn [r e]
              (let [k (f e)]
                (if (contains? r k)
                  (assoc r k (conj (r k) e))
                  (assoc r k [e])))) {} s)))

(defcheck solution-cb2da0f7
  #(apply merge-with concat (map (fn [a] {(% a) [a]}) %2)))

(defcheck solution-cbdecaf6
  (fn g-by [f coll]
    (loop [coll coll
           acc  {}]
      (if (empty? coll)
        acc
        (let [k (f (first coll))]
          (recur (rest coll) (assoc acc k (conj (get acc k []) (first coll))))
          )
        )
      )
    ))

(defcheck solution-cc5340cd
  (fn [f s] (reduce (fn [m i] (update-in m [(f i)] (fnil conj []) i)) {} s)))

(defcheck solution-cc5abcb0
  #(apply merge-with into
     (for [x %2] {(% x) [x]})))

(defcheck solution-cce715c1
  (fn [f s]
    (apply
      (partial merge-with concat)
      (map #(hash-map (f %) [%]) s))))

(defcheck solution-ccf6f3f7
  (fn [f coll] (reduce (partial merge-with into) (map #(hash-map %1 [%2]) (map f coll) coll))))

(defcheck solution-cd7d1633
  (fn [f s]
    (reduce
      (fn [a e]
        (let [k (f e)
              v (a k [])]
          (assoc a k (conj v e))))
      {}
      s)))

(defcheck solution-cd8062b
  (fn [f coll] (reduce
                 (fn [a b]
                   (update-in a
                     [(f b)]
                     (fnil conj []) b))
                 {}
                 coll)))

(defcheck solution-cde2432e
  (fn
    [id-fn li]
    (reduce
      (fn
        [m item]
        (let
         [id (id-fn item)]
          (assoc
           m
            id
            (conj (get
                    m
                    id
                    [])
              item))))
      {}
      li)))

(defcheck solution-ce26abe
  (fn [f s]
    (->> (map #(hash-map (f %) [%]) s)
      (apply merge-with (comp vec concat)))))

(defcheck solution-ce66886d
  (fn eka
    ([op xs]
     (eka op {} xs))
    ([op m xs]
     (if (seq xs)
       (let [v         (first xs)
             k         (identity (op v))
             group     (get m k)
             new_group (if group
                         (conj group v)
                         [v])
             new_m     (assoc m k new_group)
             ]
         (recur op new_m (rest xs))
         )
       m
       )
     )
    ))

(defcheck solution-cf6f053
  (fn [f coll]
    (reduce
      #(assoc % (f %2) (conj (% (f %2) []) %2))
      (hash-map)
      coll)))

(defcheck solution-cff6401c
  (fn [f coll]
    (reduce #(merge-with concat %1 {(f %2), [%2]})
      {}
      coll)))

(defcheck solution-d0278ab0
  (fn [f s]
    (reduce (partial merge-with concat) {} (map (fn [x] (hash-map (f x) [x])) s))))

(defcheck solution-d035f159
  (fn my-group [f coll]
    (letfn [(add-map [m x]
              (let [k (f x)]
                (if-let [v (get m k)]
                  (assoc m k (conj v x))
                  (assoc m k [x]))))]
      (reduce add-map {} coll))))

(defcheck solution-d080cfcb
  (fn [f c]
    (reduce
      #(let [k (f %2)]
         (assoc %1 k (conj (get %1 k []) %2)))
      {} c)))

(defcheck solution-d0bfefd0
  (fn my-group-by
    [f s]
    (reduce (fn [result v]
              (let [fv (f v)]
                (assoc result fv (conj (or (result fv) []) v))))
      {} s)))

(defcheck solution-d12a48fd
  (fn [f seq]
    (loop [result {} elements seq]
      (if (empty? elements)
        result
        (if (not (nil? (result (f (first elements)))))
          (recur (into result {(f (first elements)) (conj (result (f (first elements))) (first elements))}) (rest elements))
          (recur (into result {(f (first elements)) [(first elements)]}) (rest elements))
          )
        )
      )
    ))

(defcheck solution-d1a106e7
  (fn my-group-by
    [f coll]
    (letfn
     [(reduce-coll [f map se]
        (if (not-empty se)
          (let [next-element (first se)
                result       (f next-element)
                new-map      (update-in map [result] #(conj (into [] %1) %2) next-element)]
            ;; associate the result with the function-input-value which lead to it
            ;; sequentially using the next value in se
            (reduce-coll f new-map (rest se)))              ;; recursive call if more inputs exist
          map))]                                            ;; base case
      (reduce-coll f {} coll))))

(defcheck solution-d1b4a895
  (fn _group-by [function items]
    (apply merge-with concat (for [item items]
                               {(function item) (list item)}))))

(defcheck solution-d242bb85
  (fn my-gb [f s]
    (reduce (fn [l r]
              (let [key (f r)]
                (assoc l key
                         (conj (or (get l key) []) r))))
      {} s)))

(defcheck solution-d25a0faa
  (fn [f s]
    (->> s
      (map (fn [x] {(f x) [x]}))
      (reduce (fn [acc x] (merge-with concat acc x))))))

(defcheck solution-d264eb53
  (fn [f s]
    (apply merge-with concat (map #(hash-map (f %) [%]) s))))

(defcheck solution-d2984368
  (fn [f s] (reduce (fn [res item] (let [k (f item)] (assoc res k (conj (res k []) item)))) {} s)))

(defcheck solution-d2f34ab3
  (fn [f s]
    (reduce #(update-in %1 [(f %2)] (fnil conj []) %2)
      {}
      s)))

(defcheck solution-d33e5fca
  (fn [f coll]
    (let [m (fn [a b] (assoc a (f b) (if (contains? a (f b)) (conj (a (f b)) b) [b])))]
      (reduce m {} coll))))

(defcheck solution-d34d4bdc
  (fn [p s]
    (let [keys (set (map p s))]
      (zipmap keys
        (for [k keys]
          (into [] (filter #(= k (p %)) s)))))))

(defcheck solution-d38c8a9e
  (fn [pred coll]
    (reduce
      (fn [c v]
        (let [k (pred v)]
          (assoc c k (conj (get c k []) v))))
      {} coll)))

(defcheck solution-d3ba98ae
  (fn [pred coll]
    (reduce
      (fn [dict m]
        (let [ret (pred m)]
          (assoc dict ret (conj (or (dict ret) []) m))))
      {} coll)))

(defcheck solution-d3ca4d9e
  (fn [pre coll]
    (loop [result {} c coll]
      (if (nil? c) result
                   (let [w (first c) k (pre w) v (result k [])]
                     (recur (conj result {k (conj v w)}) (next c)))
                   ))))

(defcheck solution-d3e9c3b0
  (fn [f s] (reduce (fn [m v] (update-in m [(f v)] #(conj (or % []) v))) {} s)))

(defcheck solution-d3fcea78
  (fn [f sq] (reduce #(if (contains? %1 (f %2)) (assoc %1 (f %2) (conj (%1 (f %2)) %2)) (assoc %1 (f %2) [%2])) {} sq)))

(defcheck solution-d4109809
  (fn [f coll]
    (reduce
      (fn [acc val]
        (let [v (f val)]
          (update-in acc [v] #(if (nil? %) [val] (conj % val)))))
      {}
      coll)))

(defcheck solution-d4429cad
  (fn [f s]
    (apply merge-with concat
      (map #(hash-map (f %) [%]) s))))

(defcheck solution-d4766220
  (fn prob63
    [f col]
    (reduce
      (fn [ret x]
        (let [key (f x)]
          (assoc ret key (conj (get ret key []) x)))
        )
      {} col)))

(defcheck solution-d4942751
  (fn [p ns]
    (reduce
      (fn [m [k v]]
        (assoc m k v))
      {}
      (for [k (set (map p ns))]
        (cons k [(filter #(= k (p %)) ns)])))))

(defcheck solution-d4ca55e4
  (fn [f s] (reduce (fn [m k] (update-in m [(f k)] (fnil #(conj % k) []))) {} s)))

(defcheck solution-d4e6a536
  (fn [f v]
    (apply
      (partial merge-with concat)
      (map (fn [n] {(f n) [n]}) v))))

(defcheck solution-d5f581c2
  (fn [kfn coll]
    (reduce #(conj
               %1 [(kfn %2) (conj
                              (%1 (kfn %2) []) %2)]) {} coll)))

(defcheck solution-d605722e
  (fn me [f l] (if (empty? l) {} (merge-with concat {(f (first l)) [(first l)]} (me f (rest l))))))

(defcheck solution-d69a22d
  #(apply merge-with into (for [v %2] {(%1 v) [v]})))

(defcheck solution-d6d814f8
  #(loop [v %2
          m {}]
     (if (empty? v)
       m
       (let [n (first v)
             k (%1 n)]
         (recur (rest v)
           (assoc m k (conj (m k []) n)))))))

(defcheck solution-d6e8e4a1
  (fn [f s]
    (reduce (fn [m i]
              (if (m (f i))
                (assoc m (f i) (conj (m (f i)) i))
                (assoc m (f i) [i]))

              ) {} s)))

(defcheck solution-d6ebeb52
  (fn [op l]
    (let [vs (set (map op l))]
      (->> (for [x vs]
             [x (filter #(= x (op %)) l)])
        (into {})))))

(defcheck solution-d72f4a86
  (fn [f s1]
    (reduce (fn [m x]
              (let [value (f x)]
                (assoc m value (conj (get m value []) x)))) {} s1)))

(defcheck solution-d786bec4
  (fn [f coll] (reduce #(assoc % (f %2) (conj (get % (f %2) []) %2)) {} coll)))

(defcheck solution-d795aa9a
  (fn [f c] (reduce (partial merge-with concat) (map #(hash-map (f %) [%]) c))))

(defcheck solution-d7c623fc
  (fn [f col] (apply (partial merge-with concat) (map (fn [x] {(f x) [x]}) col))))

(defcheck solution-d7c719b0
  (fn [func xs]
    (apply merge-with concat
      (map #(hash-map (func %1) [%1]) xs))))

(defcheck solution-d7c9c6cc
  (fn [f l]
    (reduce
      #(assoc %1 (f %2) (conj (into [] (get %1 (f %2))) %2))
      (hash-map)
      l)))

(defcheck solution-d7e5fbf5
  (fn [f s]
    (reduce #(assoc %1
               (f %2)
               (conj (vec (get %1 (f %2))) %2))
      {}
      s)))

(defcheck solution-d7fe6940
  #(persistent!
     (reduce (fn [ret x]
               (let [k (%1 x)]
                 (assoc! ret k (conj (get ret k []) x))))
       (transient {}) %2)))

(defcheck solution-d7fe6d4c
  (fn my-group-by [f s]
    (reduce #(let [k (f %2)]
               (assoc %1 k (conj (get %1 k []) %2)))
      {} s)))

(defcheck solution-d851e03c
  (fn [f xs] (reduce #(update-in % [(f %2)] (fnil conj []) %2) {} xs)))

(defcheck solution-d863534d
  (fn [f coll]
    (reduce
      #(let [k (f %2)]
         (assoc %1 k (conj (get %1 k []) %2))
         )
      {} coll
      )
    ))

(defcheck solution-d8640931
  (fn [f s]
    (reduce #(let [k (f %2)] (assoc %1 k (if (contains? %1 k)
                                           (conj (get %1 k) %2)
                                           [%2])))
      {} s)))

(defcheck solution-d910826b
  #(apply merge-with into
     (for [x %2]
       {(% x) [x]})))

(defcheck solution-d91142c8
  (fn grp-by [f coll]
    (reduce (fn [acc x] (merge-with concat acc {(f x) [x]})) {} coll)
    ))

(defcheck solution-d92ec2e
  (fn [func myseq]
    (into {} (map #(vector (func (first %)) (vec %))
               (partition-by func (sort myseq))))))

(defcheck solution-d9745f98
  (fn [f s]
    (apply merge-with into (map #(hash-map (f %) [%]) s))))

(defcheck solution-da20919e
  (fn [f s]
    (reduce
      (fn [m e]
        (let [k (f e)]
          (assoc m k
                   (conj (get m k []) e))))
      ;         (if (contains? m k) (conj (m k) e) [e]))))
      {}
      s)))

(defcheck solution-da76ac8d
  (fn [f s]
    (reduce (fn [m n] (assoc m (f n) (conj (get m (f n) []) n))) {} s)))

(defcheck solution-da845623
  (fn [fun vals]
    (reduce #(assoc %1 (fun %2) (conj (get %1 (fun %2) []) %2)) {} vals)))

(defcheck solution-dab842c6
  (fn [f coll]
    (letfn [(comb-key [a-map [k v]]
              (let [vs (a-map k [])]
                (assoc a-map k (conj vs v))))]
      (reduce comb-key {} (for [x coll] [(f x) x])))))

(defcheck solution-dabdb248
  (fn [f xs]
    (loop [acc {}
           xs  xs]
      (if (empty? xs)
        acc
        (recur (update-in acc [(f (first xs))]
                 #(conj (or % []) (first xs)))
          (rest xs))))))

(defcheck solution-daede20d
  (fn [f s]
    (loop [m      (hash-map)
           remain s]
      (if (empty? remain) m
                          (let [k (f (first remain))
                                v (get m k)]
                            (if (nil? v)
                              (recur (assoc m k [(first remain)]) (rest remain))
                              (recur (assoc m k (concat v [(first remain)])) (rest remain))
                              ))))))

(defcheck solution-db22863
  (fn [f coll]
    (reduce
      #(let [r (f %2)]
         (if (contains? % r)
           (update-in % [r] conj %2)
           (assoc % r (vector %2))))
      {}
      coll)))

(defcheck solution-db90630f
  (fn [op xs] (reduce (fn [m [k v]] (if (m k) (assoc m k (conj (m k) v)) (assoc m k [v])))
                {}
                (for [x xs]
                  [(op x) x]))))

(defcheck solution-dbb3bc83
  (fn [f a]
    (reduce
      (fn
        [m x]
        (assoc m (f x)
                 (conj (get m (f x) []) x)))
      {} a)))

(defcheck solution-dbe8c03d
  (fn [f coll]
    (loop [x coll map {}]
      (if (empty? x)
        map
        (let [fx (f (first x))]
          (recur (rest x) (assoc map fx (conj (get map fx []) (first x)))))))))

(defcheck solution-dc049667
  (fn [f the-s]
    (loop [s the-s m {}]
      (if (empty? s)
        m
        (let [v (first s) k (f v) vs (get m k [])]
          (recur (rest s) (assoc m k (conj vs v))))))))

(defcheck solution-dc38ebce
  (fn [f xs]
    (reduce (fn [m x]
              (update-in m [(f x)] #(if % (conj % x) [x])))
      {} xs)))

(defcheck solution-dda0956e
  (fn [x, y] (reduce (fn [a, b] (assoc a (x b) (sort (conj (a (x b)) b)))) {} y)))

(defcheck solution-de0cdd81
  (fn my-group-by [f s]
    (reduce #(update-in %1 [(f %2)] (fn [vec item] (if (nil? vec) [item] (conj vec item))) %2) {} s)))

(defcheck solution-de8f5831
  (fn [f s]
    (reduce
      (partial merge-with concat)
      (map
        (fn [e] {(f e) [e]})
        s))))

(defcheck solution-deae6c27
  (fn [f c]
    (into
      {}
      (mapcat
        (fn [s] {s (filter #(= s (f %)) c)})
        (set (map f c))))))

(defcheck solution-deedc06f
  (fn group
    ([f c] (group f c {}))
    ([f c m]
     (if (empty? c)
       m
       (let [x (first c) v (f x)]
         (if (contains? m v)
           (group f (rest c) (assoc m v (conj (m v) x)))
           (group f (rest c) (assoc m v [x]))))))))

(defcheck solution-df141833
  (fn [key vs] (reduce (fn [m v] (assoc m (key v) (conj (get m (key v) []) v))) {} vs)))

(defcheck solution-df4bce93
  (fn [f xs] (apply merge-with into (map #(hash-map (f %) [%]) xs))))

(defcheck solution-df89b452
  (fn [f coll]
    (let [v (partition-by first (sort-by first (map #(vector (f %) %) coll)))]
      (zipmap
        (map first (for [k v] (map first k)))
        (for [k v] (map second k))))))

(defcheck solution-dfcc39fd
  (fn [f coll] (reduce (fn [m x] (let [k (f x)] (assoc m k (conj (m k []) x)))) {} coll)))

(defcheck solution-dfe0c5c6
  (fn [f c]
    (reduce (partial merge-with concat)
      (map #(hash-map (f %) [%]) c))))

(defcheck solution-e00562ba
  (fn [f, xs]
    (reduce
      (fn [acc, x]
        (assoc acc (f x)
                   (conj
                     (get acc (f x) [])
                     x)))
      {}
      xs)))

(defcheck solution-e031bd47
  #(reduce (fn [m i] (if (contains? m (% i))
                       (into m {(% i) (conj (m (% i)) i)})
                       (into m {(% i) [i]}))) {} %2))

(defcheck solution-e09684e3
  (fn [f s]
    (loop [m   {}
           rem s]
      (if (empty? rem)
        m
        (let [elem    (first rem)
              key     (f elem)
              currVal (get m key)]
          (recur (assoc m key (concat currVal (list elem))) (rest rem)))))))

(defcheck solution-e0c468d5
  (fn [f coll]
    (reduce (fn [a b]
              (let [k (f b)
                    v (a k)]
                (if (nil? v)
                  (assoc a k [b])
                  (assoc a k (conj v b)))))
      {}
      coll)))

(defcheck solution-e100d621
  (fn my-group-by [fx, sq]
    (loop [sq sq, result {}]
      (if (empty? sq)
        result
        (let [item (first sq), cur-key (fx item)]
          (if (contains? result cur-key)
            (recur (rest sq)
              (assoc result cur-key (conj (result cur-key) item)))
            (recur (rest sq) (assoc result cur-key [item]))))))))

(defcheck solution-e19ac627
  (fn [f xs]
    (let [d (distinct (map f xs))]
      (zipmap d (map #(filter (fn [x] (= % (f x))) xs) d)))))

(defcheck solution-e19df90a
  #(reduce (fn [a b] (assoc a (%1 b) (conj (get a (%1 b) []) b))) {} %2))

(defcheck solution-e1b60dc3
  (fn [fnc sqn]
    (loop [f fnc s sqn result {}]
      (if (empty? s) result
                     (let [h (first s)
                           k (f h)
                           v (get result k [])]
                       (recur f (rest s) (assoc result k (conj v h))))))))

(defcheck solution-e1ce6538
  (fn [f xs]
    (reduce (fn [m v]
              (let [k (f v)]
                (assoc m k (conj (m k []) v))))
      {}
      xs)))

(defcheck solution-e1f8cd09
  (fn [f coll]
    (reduce #(assoc %1 (f %2) (conj (%1 (f %2) []) %2))
      {}
      coll)))

(defcheck solution-e209ff12
  (fn [f s]
    (reduce (fn [memo item]
              (let [k (f item)
                    v (memo k [])]
                (assoc memo k (conj v item)))
              ) {} s)))

(defcheck solution-e27415b
  (fn [f v]
    (reduce
      (fn [m i]
        (let [fi (f i)
              cv (m fi)]
          (if (not (nil? cv))
            (assoc m fi (conj cv i))
            (assoc m fi [i]))))
      {}
      v)))

(defcheck solution-e2755849
  (fn [f s]
    (let [group (fn [a b]
                  (let [x (f b)]
                    (if (contains? a x)
                      (assoc a x (conj (a x) b))
                      (assoc a x (conj [] b))))
                  )]
      (reduce group {} s))))

(defcheck solution-e275d16d
  (fn [f xs]
    (reduce
      (fn [m n]
        (let [a (f n)
              b (m a)]
          (conj m [a (conj (or b []) n)])))
      {}
      xs)))

(defcheck solution-e27c0d24
  (fn [f s]
    (reduce (fn [r v] (let [a #((fnil conj []) % %2)]
                        (update-in r [(f v)] a v))) {} s)))

(defcheck solution-e2a6a69d
  (fn [f s]
    (reduce
      (fn [a e]
        (let [key (f e)]
          (assoc a key
                   (if
                    (contains? a key)
                     (conj (a key) e)
                     [e])))) {} s)))

(defcheck solution-e2add20a
  (fn r [f s]
    (let [l {}]
      (reduce (fn [l e]
                (let [v (f e)]
                  (if-let [p (l v)]
                    (assoc l v (vec (conj p e)))
                    (assoc l v [e])))) l s))))

(defcheck solution-e2bf40ea
  (fn group-seq [f coll]
    (let [vs (map #(vector (f %) %) coll)]
      (reduce (fn [res m]
                (update-in res [(first m)] #(if (empty? %) [(second m)]
                                                           (conj % (second m))))) {} vs)
      )
    ))

(defcheck solution-e2eaa448
  (fn [f s]
    (apply merge-with concat
      (map #(hash-map (f %) (list %)) s))))

(defcheck solution-e37eb2ec
  (fn group [f s]
    (loop [m {}, col s]
      (if (empty? col) m
                       (recur (assoc m (f (first col))
                                       (vec (conj (m (f (first col))) (first col))))

                         (rest col))))))

(defcheck solution-e3813e56
  (fn [f coll]
    (loop [m  {}
           c  coll
           it (first c)]
      (if (nil? it)
        m
        (recur (assoc m (f it) (conj (if (get m (f it)) (get m (f it)) []) it)) (next c) (first (next c)))))))

(defcheck solution-e3823af6
  (fn [f seq]
    (apply merge-with into (map #(assoc {} (f (first %)) (vec %)) (partition-by f seq)))))

(defcheck solution-e389b868
  (fn my-group-by
    [f xs]
    (loop [accum {} xs (seq xs)]
      (if xs
        (let [x (first xs) v (f x)]
          (recur (assoc accum v (conj (get accum v []) x)) (next xs)))
        accum))))

(defcheck solution-e39592ef
  (fn group-by_ [f coll]
    (loop [c_ coll acc (apply hash-map (interleave (distinct (map f c_)) (repeat [])))]
      (cond
        (empty? c_) acc
        :else (recur (rest c_)
                (assoc acc (f (first c_)) (conj (acc (f (first c_))) (first c_))))))))

(defcheck solution-e3cb783f
  (fn gb2 [f vals]
    (reduce #(merge-with into %1 {(f %2) [%2]}) {} vals)))

(defcheck solution-e3d4ebc4
  (fn [f s]
    (reduce
      (fn [m item]
        (let [v (f item)]
          (assoc m v (conj (get m v []) item))))
      {}
      s)))

(defcheck solution-e5177f67
  (fn [f coll]
    (letfn [(update [m k f]
              (assoc m k (f (get m k))))]
      (reduce (fn [r x] (update r (f x) (fnil #(conj % x) []))) {} coll)
      )))

(defcheck solution-e5706d5b
  (fn group [f s] (apply merge-with #(conj %1 (first %2))
                    (map #(hash-map (f %) [%]) s))))

(defcheck solution-e57357bb
  (fn [f col]
    (loop [c col grp {}]
      (if c
        (recur (next c) (update-in grp [(f (first c))] (fnil conj []) (first c)))
        grp))))

(defcheck solution-e60b031e
  (fn [f xs]
    (apply merge-with concat
      (for [x xs]
        {(f x) [x]}))))

(defcheck solution-e6465160
  (fn [f s]
    (loop [out {} s s]
      (if (empty? s)
        out
        (let [item (first s)
              key  (f item)]
          (recur
            (conj out
              (if (contains? out key)
                [key (conj (out key) item)]
                [key [item]]))

            (rest s)))))))

(defcheck solution-e6625bc1
  (fn do-group-by [f s]
    (reduce (fn [m i]
              (assoc m
                (f i)
                (conj (m (f i) []) i)))
      {}
      s)))

(defcheck solution-e6654a67
  (fn [f coll] (zipmap (distinct (map f coll)) (partition-by f (sort-by f coll)))))

(defcheck solution-e6acbd49
  (fn [f s]
    (loop [m  {}
           vs s]
      (if (empty? vs)
        m
        (let [v (first vs)
              k (f v)]
          (recur (assoc m k (conj (get m k []) v)) (rest vs)))))))

(defcheck solution-e6c437c5
  (fn [f xs]
    (reduce
      (fn [groups x]
        (assoc groups (f x) (conj (get groups (f x) []) x)))
      {} xs)))

(defcheck solution-e7492ebf
  (fn [f c]
    (reduce
      #(assoc % (f %2) (conj (or (% (f %2)) []) %2))
      {} c)))

(defcheck solution-e7c9b39f
  (fn p [f col]
    (let [result (if (empty? col) {} (p f (rest col)))
          value  (if (empty? col) 0 (f (first col)))
          q      (fn [v itm m]
                   (assoc m v (vec (cons itm (get m v)))))
          ]
      (if (empty? col)
        result
        (q value (first col) result)))))

(defcheck solution-e7e10faa
  (fn t [f xs]
    (reduce
      (fn [r x]
        (merge
          r
          {(f x)
           (if (= (find r (f x)) nil)
             [x]
             (concat (get r (f x)) [x])
             )
           }
          )
        )
      {}
      xs
      )
    ))

(defcheck solution-e7ec4a01
  (fn [f coll]
    (reduce
      #(if
        (%1 (first %2))
         (conj %1
           [(first %2)
            (conj
              (%1 (first %2))
              (second %2))])
         (conj %1 [(first %2) [(second %2)]]))
      {}
      (map vector (map f coll) coll))))

(defcheck solution-e804f858
  (fn [p s] (reduce #(merge-with concat %1 %2) (map hash-map (map p s) (map list s)))))

(defcheck solution-e87537f4
  (fn [kf coll]
    (reduce (fn [acc item]
              (let [k (kf item)]
                (assoc acc k (conj (get acc k []) item)))) {} coll)))

(defcheck solution-e88e713a
  (fn [f coll] (let [map-coll        (map f coll)
                     map-coll-vector (map #(vector %1 %2) map-coll coll)
                     mapped-vector   (reduce (fn [m [k v]] (update-in m [k] (fnil conj []) v)) {} map-coll-vector)]
                 (identity mapped-vector))))

(defcheck solution-e8ad1f9f
  (fn [f c] (reduce #(merge-with concat % %2)
              {} (map (fn [x] (hash-map (f x) [x])) c))))

(defcheck solution-e8d5fdcd
  (fn [f xs]
    (apply merge-with concat (map (fn [x] {(f x) [x]}) xs))
    ))

(defcheck solution-e8f23b4e
  (fn [f s] (reduce (fn [o i] (assoc o (f i) (concat (o (f i)) [i]))) {} s)))

(defcheck solution-e8fc7a90
  (fn [p c]
    (reduce
      (fn [m e] (update-in m [(p e)] (fnil conj []) e))
      {} c)))

(defcheck solution-e977d0e
  (fn [f xs]
    (reduce
      (fn [memo el]
        (let [v (f el)]
          (if (get memo v)
            (update-in memo [v] conj el)
            (assoc memo v [el]))))
      {} xs)))

(defcheck solution-e9875362
  (fn [f coll]
    (reduce (fn [acc e]
              (let [r (f e)]
                (if-let [lkeys (get acc r)]
                  (conj acc [r (conj lkeys e)])
                  (conj acc [r [e]]))))
      {}
      coll)))

(defcheck solution-e988169f
  (fn [f coll]
    (reduce #(let [k (f %2)
                   v (conj (get % k []) %2)]
               (assoc % k v)) {} coll)))

(defcheck solution-e994f1ae
  (fn [the-fn the-seq]
    (->> (map #(vector (the-fn %) %) the-seq)
      (reduce (fn [cont it]
                (conj cont [
                            (first it) (if-let [actual (cont (first it))]
                                         (conj actual (second it))
                                         [(second it)]
                                         )
                            ])
                ) {})
      )))

(defcheck solution-eabfaa9f
  (fn [f s]
    (let [z (map f s) y (zipmap z (repeat []))]
      (reduce (fn [a b] (assoc a (first b) (conj (get a (first b)) (last b))))
        y (partition 2 (interleave z s))))))

(defcheck solution-eae38e9e
  (fn [f coll]
    (reduce (fn [a b]
              (merge-with #(conj %1 (first %2)) a b))
      (map (comp (partial apply hash-map)
                 (juxt f vector))
        coll))))

(defcheck solution-eaf74f5
  (fn myGroupBy
    [fun coll]
    (apply merge-with concat (map #(hash-map (fun %) (vector %)) coll))))

(defcheck solution-eb08eaf0
  (fn [f s]
    (reduce #(let [k (f %2)
                   v (%1 k)]
               (into %1 [[k (conj (if (nil? v) [] (%1 k)) %2)]])) {} s)))

(defcheck solution-eb0e933b
  (fn gs [f coll] (reduce #(let [k (f %2), v (get % k)] (assoc % k (conj (if v v []) %2))) {} coll)))

(defcheck solution-eb19b119
  #(apply merge-with concat (map (fn [x] {(% x) [x]}) %2)))

(defcheck solution-eb2e14ce
  (fn new-group-by
    [fun coll]
    (let [new-coll (map #(assoc {} (fun %) [%]) coll)]
      (apply merge-with (cons concat new-coll)))))

(defcheck solution-eb7d6557
  (fn my-group-by [f coll]
    (apply merge-with concat
      (map (fn [item]
             (hash-map (f item) [item]))
        coll))))

(defcheck solution-ec2b4df4
  (fn [pred coll]
    (reduce (fn [mp x]
              (let [grouper (pred x)]
                (assoc mp grouper (conj (get mp grouper []) x))))
      {}
      coll)))

(defcheck solution-ece51e12
  (fn my-group-by
    [f s]
    (apply (partial merge-with #(vec (concat %1 %2))) (map #(hash-map (f %) [%]) s))))

(defcheck solution-eceebc28
  (fn
    [f l]
    (loop [m {}
           l l]
      (if (empty? l)
        m
        (let [v (first l)
              k (f v)]
          (recur
            (update-in m [k]
              (fn [x] (if (nil? x) [v] (conj x v))))
            (rest l)))))))

(defcheck solution-ed4d50c4
  (fn [f s]
    (reduce #(let [v (f %2)]
               (assoc %1 v (if (contains? %1 v) (conj (%1 v) %2) [%2]))) {} s)))

(defcheck solution-ed63aaf4
  (fn [f s] (reduce #(assoc % (f %2) (conj (% (f %2) []) %2)) {} s)))

(defcheck solution-ed63e436
  (fn [pred coll]
    (reduce
      (fn [dict m]
        (let [ret (pred m)]
          (assoc dict ret (conj (or (dict ret) []) m))))
      {} coll)))

(defcheck solution-ed6ea876
  (fn [f s]
    (reduce (fn [m v] (update-in m [(f v)] #(if (nil? %) [v]
                                                         (conj % v))))
      {} s)))

(defcheck solution-edfb9a11
  (fn [f c]
    (apply merge (map #(hash-map (f (first %)) (vec %)) (partition-by f (sort-by f c))))))

(defcheck solution-ee3cebdf
  (fn [f s]
    (zipmap (distinct (sort (map f s)))
      (partition-by f (sort-by f s)))))

(defcheck solution-ee80941d
  (fn __ [f coll]
    (let [g (fn [ret x]
              (let [y (f x)]
                (if (contains? ret y)
                  (let [y0 (get ret y)]
                    (assoc ret y (conj y0 x)))
                  (assoc ret y [x]))))]
      (reduce g {} coll))
    ))

(defcheck solution-ee9efcd9
  (fn my-group-by [f coll]
    (persistent!
      (reduce (fn [ret x]
                (let [k (f x)]
                  (assoc! ret k
                    (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-eea8dc76
  #(loop [f   %1
          [head & tail] %2
          res {}]
     (if head
       (let [fhead   (f head)
             old-vec (get res fhead)
             new-vec (if old-vec (conj old-vec head) [head])]
         (recur f tail (assoc res fhead new-vec)))
       res)))

(defcheck solution-ef03ec87
  (fn g [f l]
    (reduce (fn [m x]
              (update-in m [(f x)] (comp vec conj) x)) {} l)))

(defcheck solution-ef57813d
  (fn [f vals]
    (into {}
      (map #(vector (f (first %)) (vec %))
        (partition-by f (sort vals))))))

(defcheck solution-efd34a08
  (fn [f xs]
    (reduce #(update-in %1 [(f %2)]
               (fnil conj []) %2)
      {}
      xs)))

(defcheck solution-f002318a
  (fn [f c] (reduce (fn [acc e] (assoc acc (f e) (conj (or (acc (f e)) []) e))) {} c)))

(defcheck solution-f07caf37
  (fn [f coll] (reduce (fn [map [x y]] (if (map x) (assoc map x (conj (map x) y)) (assoc map x [y]))) {} (map vector (map f coll) coll))))

(defcheck solution-f0805e9d
  (fn [f s] (reduce #(let [k (f %2)]
                       (assoc % k (conj (% k []) %2))) {} s)))

(defcheck solution-f093ba5e
  (fn [f xs]
    (reduce (fn [acc x]
              (assoc acc (f x) (conj (get acc (f x) []) x)))
      {}
      xs)))

(defcheck solution-f11f12c6
  #(reduce (fn [a x]
             (update-in a [(%1 x)] (fnil conj []) x))
     {} %2))

(defcheck solution-f125729e
  #(loop [f %1, col %2, acc {}]
     (if (empty? col) acc
                      (let [firstElement    (first col)
                            firstElementVal (f firstElement)
                            filterFun       (fn [x] (= firstElementVal (f x)))
                            groupByCol      (filter filterFun col)
                            restCol         (remove filterFun col)]
                        (recur f restCol (conj acc [firstElementVal groupByCol]))))))

(defcheck solution-f15f2143
  #(loop [r {} [a & z :as s] (seq %2)]
     (if (empty? s)
       r
       (recur (update-in r [(%1 a)]
                (fn [v] (conj (or v []) a))) z))))

(defcheck solution-f160227b
  (fn group-by-fun
    [func coll]
    (reduce
      (fn [m k]
        (if (contains? m (func k))
          (assoc m (func k) (conj (get m (func k)) k))
          (assoc m (func k) [k])))
      {} coll))

  ;; or using the recursive version::
  #_(fn group-by-fun
      [func coll]
      (letfn [(add-to-map
                [m k v]
                (if (contains? m k)
                  (assoc m k (conj (get m k) v))
                  (assoc m k [v])))]
        (loop [acc {}
               [head & tail] coll]
          (if (nil? head)
            acc
            (recur (add-to-map acc (func head) head) tail))))))

(defcheck solution-f1b7543d
  (letfn [(update-d [d k v]
            (merge d {k (conj (or (d k) []) v)}))]
    (fn [f xs]
      (reduce (fn [a x] (update-d a (f x) x)) {} xs))))

(defcheck solution-f1c5d642
  (fn [f lst]
    (reduce (fn [acc v]
              (let [res (f v)]
                (assoc acc res (conj (or (acc res) []) v)))) {} lst)))

(defcheck solution-f1cd96ef
  #(apply merge-with concat
     (for [x %2] {(% x) [x]})))

(defcheck solution-f1d48a8a
  (fn
    [f aseq]
    (loop [as aseq el (first as) acc {}]
      (if (empty? as)
        acc
        (let [asr  (rest as)
              akey (f el)
              aval (if (contains? acc akey) (acc akey) [])]
          (recur
            asr
            (first asr)
            (assoc acc akey (conj aval el))))))))

(defcheck solution-f2030852
  (fn [f coll] (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} coll)))

(defcheck solution-f20b0302
  (fn [f coll]
    (reduce (fn [m x]
              (let [k (f x)]
                (assoc m k (conj (get m k []) x)))) {} coll)))

(defcheck solution-f2227e51
  (fn [f coll]
    (reduce
      (partial merge-with concat)
      {}
      (map
        (fn [e]
          {(f e) [e]})
        coll))))

(defcheck solution-f39990be
  (fn [f coll]
    (apply merge-with concat
      (for [x coll] {(f x) [x]}))))

(defcheck solution-f3bbfd0c
  #(let [v (partition-by % (sort-by % %2))]
     (zipmap (map (comp % last) v) v)))

(defcheck solution-f42bc23
  (fn [f s]
    (loop [result {} coll s]
      (if (empty? coll) result
                        (let [k (f (first coll))]
                          (if (get result k)
                            (recur (assoc result k (conj (get result k) (first coll))) (rest coll))
                            (recur (assoc result k (vector (first coll))) (rest coll))))))))

(defcheck solution-f47cfed4
  (fn this [f s]
    (let [l1 (map (fn [i]
                    [i (f i)]) s)
          ]
      (reduce (fn [result item]
                (if (nil? (get result (second item)))
                  (merge result {(second item) (vector (first item))})
                  (merge result
                    {(second item)
                     (into (get result (second item))
                       (vector (first item))
                       )}
                    )
                  )
                ) {} l1
        )
      )))

(defcheck solution-f48b637
  (fn [f xs] (into {} (map (fn [y] {y (filter #(= y (f %)) xs)}) (set (map f xs))))))

(defcheck solution-f49185e0
  (fn seqgrp [f s]
    (if (empty? s) {}
                   (update-in (seqgrp f (rest s)) [(f (first s))] (partial cons (first s))))))

(defcheck solution-f4ad29ce
  (fn [f s]
    (reduce
      #(merge-with concat % {(f %2) [%2]})
      {}
      s)))

(defcheck solution-f501dad1
  (fn [func coll]
    (->> coll
      (map #(vector (func %) %))
      (sort-by first)
      (partition-by first)
      (map (fn [bigcoll] (vector (first (first bigcoll)) (apply vector (->> bigcoll (map last))))))
      (reduce into)
      (apply hash-map)
      )))

(defcheck solution-f534e67
  (fn [f s]
    (let [keys  (map f s),
          pairs (map #(list %1 %2) keys s)]
      (zipmap (set keys)
        (map (fn [k] (map second (filter #(= (first %) k) pairs)))
          (set keys)))
      )))

(defcheck solution-f54170d8
  (fn [f s]
    (let [partitions (partition-by f (sort-by f s))]
      (zipmap (map #(f (first %)) partitions) partitions))))

(defcheck solution-f5dd1e3f
  (fn [f coll]
    (loop [coll coll
           acc  {}]
      (if (empty? coll)
        acc
        (let [e  (first coll)
              k  (f e)
              ov (get acc k)
              nv (if (nil? ov) [e] (conj ov e))]
          (recur (rest coll) (assoc acc k nv)))))))

(defcheck solution-f5f7f3a
  (fn gr [f ls]
    (reduce
      #(if (contains? % (f %2))
         (conj (dissoc % (f %2)) (hash-map (f %2) (conj (get % (f %2)) %2)))
         (conj % (hash-map (f %2) [%2])))
      {} ls)))

(defcheck solution-f6fb7ed
  (fn [f s]
    (apply merge-with concat (map #(hash-map (f %) [%]) s))))

(defcheck solution-f71b135c
  (fn [f ls]
    (loop [an {} ls ls]
      (if (empty? ls)
        an
        (let [x (first ls)
              v (f x)]
          (if-let [fv (an v)]
            (recur (conj an [v (conj fv x)]) (rest ls))
            (recur (conj an [v [x]]) (rest ls))))))))

(defcheck solution-f74ab9ca
  (fn [f coll] (reduce #(conj % [(first %2) (conj (get % (first %2) []) (second %2))]) {} (map vector (map f coll) coll))))

(defcheck solution-f78a70dd
  (fn my-group [fun coll]
    (reduce (fn [acc elt]
              (let [k        (fun elt)
                    prev-val (get acc k [])]
                (assoc acc k (conj prev-val elt)))) {} coll)))

(defcheck solution-f7cc6f96
  (fn [f c]
    (let [v (distinct (map f c))]
      (zipmap v (for [i v] (filter #(= i (f %)) c))))))

(defcheck solution-f8045ac2
  (fn groupby
    [f coll]
    (persistent!
      (reduce
        (fn [ret x]
          (let [k (f x)]
            (assoc! ret k (conj (get ret k []) x))))
        (transient {}) coll))))

(defcheck solution-f8098bba
  (fn
    [f x]
    (let [process (fn [processed current]
                    (let [key (f current)]
                      (assoc processed key (conj (get processed key []) current)
                                       )))]
      (loop [result    {}
             current   (first x)
             remaining (rest x)]
        (if (= nil current)
          result
          (recur (process result current) (first remaining) (rest remaining)))))))

(defcheck solution-f80ab20a
  (fn [f lst]
    (loop [d {} lst lst]
      (if lst                                               ; as long as not nil
        (let [in      (first lst)
              out     (f in)
              d-entry (d out)]
          (if d-entry
            (recur (assoc d out (conj d-entry in)) (next lst))
            (recur (assoc d out [in]) (next lst))))
        d))))

(defcheck solution-f840ef7f
  (fn gby [f x]
    (reduce #(assoc % (first %2) (if (nil? (% (first %2)))
                                   (vector (second %2))
                                   (conj (% (first %2)) (second %2)))) {}
      (map (fn [y] (conj (vector (f y)) y)) x))))

(defcheck solution-f848bd18
  #(loop [f %1 s %2 m {}]
     (let [x (first s) _s (rest s)]
       (if (nil? x)
         m
         (recur f _s (merge-with concat m {(f x) [x]}))
         )
       )
     ))

(defcheck solution-f92ffe4d
  #(reduce (fn [m elem] (let [f %1]
                          (let [r (f elem)]
                            (if (contains? m r)
                              (assoc m r (conj (m r) elem))
                              (assoc m r [elem])))))

     {} %2))

(defcheck solution-f9647c46
  (fn gb [f s]
    (reduce
      (fn [m c]
        (update-in m [(f c)] #(if %1 (conj %1 %2) [%2]) c))
      {}
      s)))

(defcheck solution-f96c87a2
  (fn this
    ([f xs] (this f xs {}))
    ([f xs acc]
     (if (empty? xs)
       acc
       (let [x  (first xs),
             v  (f x),
             vs (get acc v [])]
         (recur f (rest xs) (assoc acc v (conj vs x))))))))

(defcheck solution-f993ce02
  (fn apply-fun [fun items]
    (if (empty? items)
      {}
      (let [item-value (first items)
            item-key   (fun item-value)
            result     (apply-fun fun (rest items))]
        (assoc result item-key (cons item-value (get result item-key [])))))))

(defcheck solution-f9c94aa
  (fn [f l]
    (reduce (fn [m e]
              (let [k (f e)
                    v (m k)]
                (if (nil? v)
                  (conj m [k [e]])
                  (conj m [k (conj v e)]))))
      {} l)))

(defcheck solution-f9f1984d
  (fn [f col]
    (reduce #(assoc % (f %2) (conj (get % (f %2) []) %2)) {} col)))

(defcheck solution-fa109bd3
  (fn [f s] (reduce (fn [m a] (assoc m (f a) (conj (or (m (f a)) []) a))) {} s)))

(defcheck solution-fa1540ce
  (fn [f xs]
    (reduce
      (fn [a b] (update-in a [(f b)] #(conj (if (vector? %) % []) b)))
      {}
      xs)))

(defcheck solution-fa66ab2e
  (fn ff [f s]
    (if (empty? s)
      {}
      (let [res  (ff f (rest s))
            key  (f (first s))
            val  (or (res key) '())
            nval (cons (first s) val)]
        (assoc res key nval)))))

(defcheck solution-fa86febb
  #(reduce
     (fn [m v]
       (let [k (%1 v)
             d (get m k)]
         (conj m
           [k
            (if d (conj d v) [v])
            ])))
     {}
     %2))

(defcheck solution-fa9256bf
  (fn
    [f coll]
    (reduce #(assoc %1 (f %2) (conj (%1 (f %2) []) %2)) {} coll)))

(defcheck solution-fad58529
  (fn [f c]
    (apply merge-with concat
      (map #(hash-map (f %) (list %)) c))))

(defcheck solution-fb035785
  (fn [f s]
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc ret k (conj (get ret k []) x))))
      {} s)))

(defcheck solution-fb7899e8
  (fn [f s]
    (reduce #(assoc % (f %2) (conj (vec (get % (f %2))) %2))
      {}
      s)))

(defcheck solution-fb7d7d99
  #(apply merge-with into (for [x %2] {(%1 x) [x]})))

(defcheck solution-fba50268
  (fn [f l]
    (zipmap
      (set (map f l))
      (map
        (fn [x]
          (filter #(= x (f %)) l))
        (set (map f l))))))

(defcheck solution-fc86eb9d
  (fn [f s] (reduce #(update-in %1 [(f %2)] conj %2) {} (reverse s))))

(defcheck solution-fd173230
  (fn my-group-by
    ([fun l] (my-group-by fun l {}))
    ([fun l resp]
     (if (= l []) resp
                  (let [f (first l)
                        k (fun f)]
                    (if (contains? resp k)
                      (my-group-by fun (rest l) (assoc resp k (conj (resp k) f)))
                      (my-group-by fun (rest l) (assoc resp k [f]))))))))

(defcheck solution-fd1759c8
  #(apply merge-with concat (map hash-map (map % %2) (map vector %2))))

(defcheck solution-fd2b80fc
  (fn grp [f xs]
    (reduce (fn [m x] (let [k (f x) v (m k)] (assoc m k (if v (conj v x) (conj [] x))))) {} xs)))

(defcheck solution-fd4dbe3f
  (fn grp [f coll]
    (reduce #(merge-with concat %1 %2) (map #(hash-map (f %) (vector %)) coll))
    ))

(defcheck solution-fde5234d
  (fn [f l]
    (let [vs (set (map f l))]
      (into {} (map #(vec [% (filter (fn [x] (= % (f x))) l)]) vs)))))

(defcheck solution-fe2e681a
  (fn [f xs]
    (let [ys (map f xs)]
      (->> (map array-map ys xs)
        (cons (zipmap (set ys) (repeat [])))
        (apply merge-with conj)))))

(defcheck solution-fecc939a
  (fn my-partition-1 [f v]
    (loop [v   v
           acc {}]
      (cond
        (empty? v) acc
        :else (recur (rest v) (update-in acc
                                [(f (first v))] (fnil conj []) (first v)))))))

(defcheck solution-ff1519a7
  #(reduce
     (fn [m x] (assoc m (% x) (conj (m (% x) []) x)))
     {} %2))

(defcheck solution-ff39faab
  (fn [f coll]
    (reduce (fn [m item]
              (let [k (f item)]
                (assoc m k (conj (m k []) item))))
      {}
      coll)))

(defcheck solution-ffe31d18
  (fn [f coll]
    (apply merge-with concat
      (map #(hash-map (f %) [%]) coll))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-63))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
