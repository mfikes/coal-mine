(ns coal-mine.problem-50
  (:require [coal-mine.checks :refer [defcheck-50] :rename {defcheck-50 defcheck}]
            [clojure.test]))

(defcheck solution-100ad148
  (fn [s]
    (map second (group-by type s))))

(defcheck solution-1051f65e
  (fn [a]
    (-> (group-by type a) vals set)))

(defcheck solution-108c1b4a
  (fn [coll]
    (for [item-type (distinct (map type coll))]
      (filter #(= item-type (type %)) coll))))

(defcheck solution-11b8b458
  (fn [s]
    (vals
      (reduce
        #(let [t (type %2)
               val (get %1 t)]
           (assoc %1 t
                     (if (= val nil) [%2] (conj val %2))))
        {}
        s))))

(defcheck solution-12708fdb
  #(vals
     (reduce
       (fn [x y] (merge-with
                   (fn [a b] (if (and (coll? a) (not (coll? b))) (conj a b) [a b])) x y))
       (map (fn [a] {(type a) a}) %))))

(defcheck solution-12a1eff7
  (fn [coll]
    (set (map #(last %) (group-by type coll)))))

(defcheck solution-12b099d0
  #(vals (group-by type %)))

(defcheck solution-12ee108d
  (let [splitter
        (fn [accum leftover]
          (if (nil? leftover)
            (vals accum)
            (recur (assoc accum (type (first leftover)) (conj (get accum (type (first leftover)) []) (first leftover))) (next leftover))
            )
          )]
    (fn [coll] (splitter {} coll))
    ))

(defcheck solution-133e9004
  #_#(partition-by type (sort-by (comp str type) %))

  #_(fn [c] (vals (reduce #(merge-with concat % %2) {}
                    (map hash-map
                      (map type c) (map vector c)))))
  #_(fn [c] (vals (apply merge-with concat
                    (map hash-map
                      (map type c) (map vector c)))))

  #_(fn [c] (vals (apply merge-with concat
                    (map (comp (partial apply hash-map)
                               (juxt type vector)) c))))

  #_(comp vals (partial apply merge-with concat)
          (partial map
            (comp (partial apply hash-map)
                  (juxt type vector))))

  #_#(partition-by type (sort-by (comp str type) %))

  #_(fn [c] (map #(map :v %) (vals (group-by :t
                                     (map #(-> {:t (type %) :v %}) c)))))

  #(vals (group-by type %)))

(defcheck solution-13499dde
  (fn [s] (map (fn [x] (map second x)) (map second (group-by first (map #(list (type %) %) s))))))

(defcheck solution-1402529a
  (fn [coll]
    (loop [m {}
           s (seq coll)]
      (if (empty? s)
        (seq (vals m))
        (recur
          (update-in m [(type (first s))] (fn [v]
                                             (if v
                                               (conj v (first s))
                                               [(first s)])))
          (next s))))))

(defcheck solution-14834f33
  (fn [lst] (vals (group-by #(type %) lst))))

(defcheck solution-1512fe4d
  (fn split-by-type [coll]
    (->> coll
      (group-by type)
      vals)))

(defcheck solution-1597b7ef
  (fn [coll]
    (vals (reduce (fn [acc item]
                    (assoc acc (type item)
                               (conj (get acc (type item) []) item)))
            {} coll))))

(defcheck solution-16aab5a3
  #(vals (apply merge-with concat (map (fn [x] (hash-map (type x) [x])) % ))))

(defcheck solution-16c8bf5b
  (fn [things]
    (vals (reduce
            (fn [sofar item]
              (let [c (type item)]
                (assoc sofar c (conj (get sofar c []) item))))
            {}
            things))))

(defcheck solution-1782e87f
  (fn [coll]
    (vals (reduce (fn [a b]
                    (update-in a [(type b)] #(conj (vec %1) b))) {} coll))))

(defcheck solution-1887473d
  (fn [alist]
    (partition-by
      #(count (str (type %)))
      (sort-by #(count (str (type %))) alist))))

(defcheck solution-18b9d7f8
  (fn [coll]
    (->> coll
      (reduce #(let [t (type %2)] (if (contains? % t) (assoc % t (conj (get % t) %2)) (assoc % t [%2]))) {})
      vals)))

(defcheck solution-18e7875a
  #(remove empty?
     (for [f [integer? string? coll? keyword?]]
       (filter f %))))

(defcheck solution-18f3727a
  (fn [s]
    (map second (reduce (fn [a b]
                          (assoc a (type b) (if (a (type b))
                                              (conj (a (type b)) b)
                                              [b]))) {} s))))

(defcheck solution-1913329
  (fn [x]
    (loop [x x
           r {}]
      (if (nil? (first x))
        (vals r)
        (if (nil? (get r (type (first x))))
          (recur (rest x) (assoc r (type (first x)) (list (first x))))
          (recur (rest x) (assoc r (type (first x)) (concat (get r (type (first x))) (list (first x))))))))))

(defcheck solution-198cbbdc
  (fn [l]
    (set (vals (reduce
                 (fn [map el] (update-in map [(type el)]
                                (fn [x] (if (nil? x) [el] (conj x el)))))
                 {} l)))))

(defcheck solution-1a07b5b4
  (fn segregate [coll]
    (map (fn [t ts]
           (filter #(= t (type %)) ts))
      (set (map type coll)) (iterate identity coll))))

(defcheck solution-1ab576b6
  (fn split [n]
    (set (vals (loop [items n tmpmap {}]
                 (if (seq items)
                   (recur (rest items)
                     (merge-with concat
                       tmpmap
                       {(type (first items)) [(first items)]}))
                   tmpmap))))))

(defcheck solution-1ae9d355
  #(map second (group-by type %)))

(defcheck solution-1b37938b
  (fn [xs] (map (fn [x] (filter #(= x (type %1)) xs)) (map type xs) )))

(defcheck solution-1bd033f0
  #(filter vector? (mapcat seq (group-by type %))))

(defcheck solution-1bfaf930
  #(letfn [(group [v r]
             (if (empty? v)
               r
               (let [e (first v), c (type e), g (get r c []), g' (conj g e)]
                 (recur (rest v) (assoc r c g')))))]
     (vals (group % {}))))

(defcheck solution-1c1df1e1
  (fn[x](partition-by #(type %) (sort-by #(str (type %)) x))))

(defcheck solution-1d5acac3
  (fn [col] (vals
              (reduce (fn [m n] (update-in m [(type n)] conj n))
                (zipmap (map type col) (repeat []))
                col))))

(defcheck solution-1db9dfdc
  (comp vals (partial group-by type)))

(defcheck solution-1ddf298
  (fn [s] (set (vals (group-by type s)))))

(defcheck solution-1dfca6d
  (fn split [s]
    (if (empty? s)
      []
      (letfn [(ftype [x] (= (type x) (type (first s))))]
        (cons (filter ftype s)
          (split (filter (complement ftype) s)))))))

(defcheck solution-1e053c65
  #(map (fn[[k v]]v) (group-by type %)))

(defcheck solution-1e2727ed
  (fn [collection]
    (vals (reduce (fn [acc elt]
                    (let [c (type elt)]
                      (assoc acc c (conj (get acc c []) elt))))
            {}
            collection))))

(defcheck solution-1e780a95
  (fn [coll]
    (vals (group-by type coll))))

(defcheck solution-1f6e48f5
  #((comp vals (partial group-by type)) %))

(defcheck solution-1fdf971c
  (fn [lst] (vals (group-by (fn [x]
                              (cond
                                (keyword? x) "k"
                                (number? x) "n"
                                (sequential? x) "s")) lst))))

(defcheck solution-201f66c4
  (fn split-by-type [a-seq]
    (let [type-map(group-by #(type %) a-seq)]
      (into #{} (for [[k v] type-map] v))
      )
    ))

(defcheck solution-20a00b6f
  #( vals
     (group-by type %)))

(defcheck solution-21ef4619
  #(set (vals(group-by type %))))

(defcheck solution-22405390
  (fn [x] (vals (group-by #(type %) x))))

(defcheck solution-22e2440c
  (fn [xs]
    (vals (apply merge-with into (map #(hash-map (type %) [%]) xs)))))

(defcheck solution-231631ee
  (fn [l] (vals (group-by #(type %) l))))

(defcheck solution-23486699
  (fn [s]
    (set (map second (group-by type s)))))

(defcheck solution-24291004
  #(vals (reduce (fn [acc v] (assoc acc
                               (type v)
                               (conj (get acc (type v) []) v)))
           {}
           %)))

(defcheck solution-2453b909
  (fn splt-tp
    [lst]
    (let [ins2
          (fn [xs x]
            (loop [l (first xs) ls (rest xs) out []]
              (if (nil? l)
                (conj out [x])
                (if (= (type x) (type (first l)))
                  (if (empty? ls)
                    (conj out (conj l x) )
                    (concat (conj out (conj l x)) ls )
                    )
                  (recur (first ls) (rest ls) (conj out l))
                  )
                )
              )
            )]
      (reduce #(ins2 %1 %2) [] lst)
      )
    ))

(defcheck solution-24c1e669
  (fn [coll]
    (vals (group-by #(type %) coll))))

(defcheck solution-252c2688
  (fn [lst] (let [types (set (map type lst))] (map #(filter (fn [item] (= % (type item))) lst) types))))

(defcheck solution-2566377
  (fn split-by-type [coll] (vals (group-by #(type %1) coll))))

(defcheck solution-25cc2ead
  #(vals
     (reduce
       (fn [r a]
         (let [t (type a)]
           (assoc r t (conj (get r t []) a))))
       {}
       %)))

(defcheck solution-25da6e83
  (fn [s]
    (let [types (set (map type s))]
      (set
        (reduce
          (fn [r t] (conj r (filter #(= t (type %)) s)))
          []
          types)))))

(defcheck solution-26e42c0d
  #(partition-by type (sort-by (comp str type) %)))

(defcheck solution-2729a6d
  (fn [coll] (set (vals (group-by #(type %) coll)))))

(defcheck solution-273aadc5
  (fn [s]
    (vals (group-by type s))))

(defcheck solution-2841f976
  (fn [p]
    (vals (
           (fn splaa [[f & n]]
             (if (empty? n)
               {(type f) (list f)}
               (let [res (splaa n)
                     typ (type f)
                     col (res typ (list))]
                 (assoc res typ (conj col f)))))
           p))))

(defcheck solution-290df74a
  (fn [s]
    (->>
      s
      (map #(hash-map (type %) [%]))
      (apply merge-with (comp vec concat))
      (map second))))

(defcheck solution-29348f2b
  (fn [l]
    (set (vals (group-by type l)))
    ))

(defcheck solution-295623e6
  (fn [coll]
    (loop [coll coll
           answer {}]
      (if (nil? coll) (vals answer)
                      (recur (next coll)
                        (let [t (type (first coll))]
                          (assoc answer t
                                        (conj (into [] (get answer t)) (first coll)))))))))

(defcheck solution-298971b
  (fn [s] (->> s (group-by type) vals)))

(defcheck solution-2b118a5
  (fn [x] (into #{} (vals (apply merge-with concat (map #(hash-map (type %) [%]) x))))))

(defcheck solution-2c490c4b
  (fn [l]
    (letfn [(worker [l s]
              (if (empty? l)
                s
                (let [v (first l) c (type v) p (find s c)]
                  (if p
                    (recur (rest l) (assoc s c (conj (val p) v)))
                    (recur (rest l) (assoc s c [v]))))))]
      (map val (worker l {})))))

(defcheck solution-2c520ff5
  (fn [s]
    (letfn [
            (add-to-part [x parts]
              (let [x-type (type x)
                    x-part (get parts x-type [])]
                (assoc parts x-type (conj x-part x))))]
      (loop [tail s parts {}]
        (if (seq tail)
          (recur (rest tail) (add-to-part (first tail) parts))
          (vals parts))))))

(defcheck solution-2c6e53b2
  (fn [coll] (vals (group-by type coll))))

(defcheck solution-2c7cb065
  (fn [c] (map #(second %)(group-by type c))))

(defcheck solution-2d36f0a
  (fn [a] (vals (reduce #(let [c (type %2)] (conj % [c (conj (get % c []) %2)])) {} a))))

(defcheck solution-2d5a034b
  (fn [x] (map vec(partition-by type (sort-by #(str (type %)) x)))))

(defcheck solution-2e02213d
  #(map reverse (vals (loop [r {}
                             c %1]
                        (if (empty? c)
                          r
                          (recur (assoc r (type (first c)) (conj (get r (type (first c))) (first c))) (rest c)))))))

(defcheck solution-2e6b5a42
  (fn [coll]
    (vals
      (reduce #(assoc %1 (type %2) (conj (vec (%1 (type %2))) %2))
        {} coll))))

(defcheck solution-2ed793c7
  (fn [v]
    (set(for [[k v] (group-by #(type %) v)] v))))

(defcheck solution-2f1d078d
  #(map val (group-by type %)))

(defcheck solution-2fa91983
  (fn [s]
    (apply hash-set
      (map #(val %)
        (apply merge-with
          concat
          (apply map
            hash-map [(map type s) (map vector s)]))))))

(defcheck solution-3070eab0
  (fn[i]
    (partition-by #(str (type %)) (sort-by #(count (str (type %))) i))))

(defcheck solution-31a641c8
  (fn [s] (partition-by type (sort-by #(str (type %)) s))))

(defcheck solution-3233e903
  (fn [lst] (vals (group-by (fn [item] (type item)) lst))))

(defcheck solution-32f6e46e
  (fn [coll]
    (into #{} (vals (loop [coll coll
                           res {}]
                      (if (empty? coll)
                        res
                        (let [el (first coll)
                              t (type el)
                              tcoll (res t)]
                          (recur (rest coll)
                            (assoc res t
                                       (if (nil? tcoll)
                                         [el]
                                         (conj tcoll el)))))))))))

(defcheck solution-339f7d31
  (fn [s]
    (let [acc (fn [m i] (assoc m (type i) (conj (get m (type i) []) i)))
          bytype (reduce acc {} s)]
      (vals bytype))))

(defcheck solution-33b82318
  #(for [[k v] (group-by type %)] v))

(defcheck solution-34502c96
  (fn [c] (map #(map first %)
            (partition-by second
              (sort-by second
                (map #(list % (str (type %))) c))))))

(defcheck solution-34a131df
  #(remove empty?
     (list (filter number? %)
       (filter keyword? %)
       (filter coll? %)
       (filter string? %))))

(defcheck solution-351907b8
  (fn [col]
    (vals
      (reduce
        #(merge-with concat %1 {(type %2) [%2]})
        {}
        col))))

(defcheck solution-36465dee
  (fn [s]
    (vals (reduce
            #(assoc %1 (type %2) (conj (get %1 (type %2) []) %2))
            {} s))
    ))

(defcheck solution-364bd30e
  (fn [liste]
    (for [t (set (map type liste))]
      (vec (filter #(= (type %) t) liste))
      )
    ))

(defcheck solution-370058a3
  (fn [s]
    (->> (group-by type s)
      (vals))))

(defcheck solution-37bd0b90
  (fn [items]
    (let [get-index (fn [item lst]
                      (loop [rm (seq lst), i 0]
                        (cond (empty? rm) -1
                              (= (type item) (-> rm first first type)) i
                              :else (recur (rest rm) (inc i)))))]
      (loop [rm (seq items), acc (vector)]
        (cond (empty? rm) acc
              :else (let [item (first rm)
                          i (get-index item acc)]
                      (cond (= -1 i) (recur (rest rm) (conj acc (vector item)))
                            :else (recur (rest rm) (assoc acc i (conj (acc i) item))))))))))

(defcheck solution-383593e8
  (fn [xs]
    (vals (reduce
            (fn [m x]
              (if (m (type x))
                (assoc m (type x) (conj (m (type x)) x))
                (assoc m (type x) [x])))
            {} xs))))

(defcheck solution-3a092a8a
  (fn [xs]
    (set (partition-by type (sort-by #(str (type %)) xs)))))

(defcheck solution-3a0e50cb
  (fn [s] (set (vals (group-by type s)))))

(defcheck solution-3a1c5516
  (fn [xs] (map #(mapcat identity %) (partition-by (fn [a] (type (first a))) (sort-by (fn [a] (str (type (first a)))) (partition-by type xs))))))

(defcheck solution-3afb8461
  #(set (vals (group-by type %))))

(defcheck solution-3b13b547
  (fn [xs] (vals (reduce (fn [m b]
                           (let [t (type b)] (assoc m t (conj (get m t []) b)))) {} xs))))

(defcheck solution-3c06275a
  (fn [xs]
    (set (vals (group-by #(type %) xs)))))

(defcheck solution-3c946c40
  (fn [xs] (set (map (fn [t] (filter #(= (type %) t) xs)) (distinct (map type xs))))))

(defcheck solution-3cb88d25
  (fn [c] (vals (group-by #(type %) c))))

(defcheck solution-3cd1c84a
  (fn type-partition [col]    ;vals&#26159;&#19987;&#38376;&#25910;&#38598;map&#20013;&#20540;&#65292;&#24182;&#20570;&#25104;&#19968;&#20010;sequences&#65292;group-by&#26159;&#26377;&#20004;&#20010;&#21442;&#25968;&#65292;&#31532;&#19968;&#20010;&#26159;&#35201;&#20998;&#31867;&#30340;&#35268;&#21017;&#65292;&#31532;&#20108;&#20010;&#26159;sequences&#65292;&#23558;sequences&#37324;&#38754;&#25353;&#35268;&#21017;&#26469;&#36827;&#34892;&#20998;&#35013;&#12290;
    (vals (group-by #(type %1) col))))

(defcheck solution-3d8d14cf
  (fn [l] (vals (reduce (fn [m v] (let [cv (str (type v))] (assoc m cv (if (contains? m cv) (conj (m cv) v) [v] ))))  {} l))))

(defcheck solution-3d985d69
  (fn [lst]
    (vals
      (reduce (fn [m v] (assoc m (type v) (conj (get m (type v) []) v)))
        {}
        lst))))

(defcheck solution-3e68926a
  (fn split-by-type [seqs]
    (set  (vals (group-by #(type %) seqs))   )))

(defcheck solution-3efdf64b
  (fn [x] (set (map reverse (vals (reduce #(assoc % (type %2) (conj (% (type %2)) %2)) {} x))))))

(defcheck solution-40af5ae3
  (fn group-types [s]
    (vals (group-by type s))))

(defcheck solution-41b480d0
  #(into #{} (vals (group-by type %))))

(defcheck solution-41dcd5
  (fn split-by-type [s] (partition-by type (sort-by #(-> % type str) s))))

(defcheck solution-41f99285
  #(vals (reduce (fn [m x]
                   (conj m
                     { (type x)
                      (vec
                        (conj (m (type x)) x))} )) {} %)))

(defcheck solution-42ce40
  (fn [c] (vals (group-by type c))))

(defcheck solution-42cec958
  (fn [xs]
    (vals (group-by type xs))))

(defcheck solution-42d9318
  (comp vals (partial reduce #(update-in %1 [(type %2)] (fnil conj []) %2) {})))

(defcheck solution-42e81578
  (fn [coll]
    (vals (group-by type coll))))

(defcheck solution-43180e99
  (fn by-type
    [& args]
    (map val (apply group-by type args))))

(defcheck solution-43b40870
  (fn type-split [coll]
    (loop [col coll,result {}]
      (if (empty? col)
        (vals result)
        (let [val (first col),
              cls (type val)]
          (recur (rest col)
            (assoc result
              cls
              (conj (get result cls []) val)
              )
            )
          )
        )
      )
    ))

(defcheck solution-44911885
  (comp vals #(group-by type %)))

(defcheck solution-44c7f54
  (fn splitbytype [x]
    (map second (vec (group-by type x)))))

(defcheck solution-450c98cc
  (fn [coll]
    (letfn [(map-by-type [result coll]
              (if (empty? coll)
                result
                (let [fel (first coll)]
                  (recur
                    (merge-with
                      concat
                      result
                      {(type fel) [fel]})
                    (rest coll)))))]
      (apply hash-set (vals (map-by-type {} coll))))))

(defcheck solution-4532fe64
  (fn f [coll]
    (let [ separate (fn [f s] [(filter f s), (filter (complement f) s)])
          [xs ys] (separate #(= (type (first coll)) (type %)) coll)]
      (if (empty? ys) [xs] (conj (f ys) xs)))))

(defcheck solution-465f1486
  (fn [s]
    (->> s
      (map #(hash-map (type %) (list %)) ,,)
      (apply merge-with concat ,,)
      (vals ,,)
      (into #{}))))

(defcheck solution-46abe100
  (fn [S] (vals (group-by type S))))

(defcheck solution-47a0f7f3
  (fn [s] (vals (reduce #(apply (partial assoc %1) (let [c (type %2)] [c (conj (%1 c) %2)])) {} (reverse s)))))

(defcheck solution-4804983
  #(into #{} (vals (group-by type %1))))

(defcheck solution-489d0be1
  (fn split-by-type
    [things]
    (loop [[h & t] things
           accum {}]
      (let [h-type (type h)]
        (let [new-accum (if (contains? accum h-type)
                          (assoc accum h-type (conj (accum h-type) h))
                          (assoc accum h-type [h]))]
          (if (nil? t)
            (vals new-accum)
            (recur t new-accum)))))))

(defcheck solution-48c1f90f
  (fn [s]
    (let [types (set (map type s))]
      (for [x  types] (filter #(= (type %) x) s)))))

(defcheck solution-48defb07
  #(vals (group-by (fn [itm] (type itm)) %)))

(defcheck solution-490a85d0
  (fn split-type [col]
    (vals (reduce (fn [x y] (assoc x (type y) (conj (x (type y) []) y))) {} col))))

(defcheck solution-4921dd94
  #(map val (group-by type %)))

(defcheck solution-4a2795de
  (fn [s] (filter (fn [r] (seq r))((juxt
                                     #(filter (fn [v] (keyword? v)) %)
                                     #(filter (fn [v] (number? v)) %)
                                     #(filter (fn [v] (string? v)) %)
                                     #(filter (fn [v] (vector? v)) %)) s))))

(defcheck solution-4af7a76d
  (fn[coll]
    (loop [[head & tail] coll res {}]
      (if (nil? head)
        (into #{} (vals res))
        (let [t (type head)]
          (recur tail
            (assoc res t (conj (get res t []) head))))))))

(defcheck solution-4b5fc5f
  (fn split [coll]
    (vals (reduce
            (fn [m n]
              (assoc m (type n) (conj (get m (type n) []) n))
              )
            {}
            coll))))

(defcheck solution-4b9842a5
  (fn [l]
    (vals (reduce (fn [m e]
                    (conj m [(type e)
                             (if (m (type e))
                               (conj (m (type e)) e)
                               [e])]))
            {} l))))

(defcheck solution-4bf7183f
  (fn [l] (partition-by type (sort-by #(str (type %)) l))))

(defcheck solution-4c47c0ee
  (fn [y]
    (vals
      (reduce
        (partial merge-with into)
        {}
        (map
          (fn[x] {(str (type (first x))) (into [] x)})
          (partition-by
            #(type %)
            y
            )
          )
        )
      )
    ))

(defcheck solution-4c527c2
  (fn [sq]
    (let [fns [string? keyword? number? coll?]
          f (fn [pred] (filter pred sq))]
      (reduce
        #(let [r (f %2)] (if (empty? r) %1 (conj %1 r)))
        '()
        fns)
      )))

(defcheck solution-4c695bee
  (fn [x] (vals (group-by type x))))

(defcheck solution-4c6dc1ca
  (comp vals (partial group-by (juxt keyword? vector?))))

(defcheck solution-4c80c69c
  (fn [s]
    ((fn [s m]
       (if (empty? s)
         (vals m)
         (let [t (type (first s))]
           (recur (rest s)
             (assoc m t
                      (concat (get m t) [(first s)]))))))
     s {})))

(defcheck solution-4cc0edcf
  (fn split-by-type
    [coll]
    (set
      (map
        #(vec (map second %))
        (vals
          (group-by
            first
            (partition 2 (interleave (map type coll) coll))))))))

(defcheck solution-4cdd18de
  (fn [a]
    (into #{}
      (map val
        (apply (partial merge-with concat)
          (map #(hash-map (type %) [%]) a))))))

(defcheck solution-4df1618f
  (fn [s] (let [t (set (map type s) )]
            (for [x t]  (filter #(= (type %) x) s)) )))

(defcheck solution-4eb6632a
  (fn [items]
    (vals (group-by type items))))

(defcheck solution-4ef09582
  (fn [s] (set (vals (group-by #(type %) s)))))

(defcheck solution-4ef2f2b5
  (fn s [seq]
    (let [typ_val (map #(hash-map (type %) [%]) seq)]
      (vals (apply merge-with concat typ_val)))))

(defcheck solution-4f0e0ea4
  #(reduce (fn [set item]
             (let [v (some (fn [it] (if (= (-> it first type) (type item)) it)) set)]
               (if (nil? v)
                 (conj set (vector item))
                 (conj (disj set v) (concat v (vector item))))))
     #{}
     %))

(defcheck solution-4f222eb5
  (fn [x]
    ((fn listnn [& more]
       (if (= (count more) 0)
         '()
         (if (= (first more) '())
           (apply listnn (rest more))
           (cons (first more) (apply listnn (rest more)))
           )
         )
       )
     (filter number? x)
     (filter vector? x)
     (filter string? x)
     (filter keyword? x)
     )
    ))

(defcheck solution-4f928801
  (fn sbt [l]
    (set (vals (group-by type l)))))

(defcheck solution-4fec856c
  (fn [col]
    (set (vals (group-by #(type %) col)))
    ))

(defcheck solution-50f42f42
  (fn sbt
    ([x] (sbt x (hash-map)))
    ([x y]
     (if (empty? x)
       (vals y)
       (sbt (rest x)
         (merge-with concat y
           (hash-map (type (first x))
             (concat (get (type (first x)) y [])
                     (list (first x))))))))))

(defcheck solution-527e10ff
  (fn [col]
    (vals (group-by #(type %) col))))

(defcheck solution-53dfbd1f
  (fn [coll]
    (vals (group-by type coll))))

(defcheck solution-541588b5
  (comp (partial partition-by type)
        (partial sort-by (comp str type))))

(defcheck solution-55e8353
  #(->> (group-by type %)
     vals
     set))

(defcheck solution-560d4c5b
  #(vals(group-by type %)))

(defcheck solution-57bea62a
  #(->> % (group-by type) (map second) (into #{})))

(defcheck solution-57f0553a
  #(partition-by (comp str type) (sort-by (comp str type) %1)))

(defcheck solution-5849c394
  (fn sbt [s]
    (map #(second %) (group-by #(type %) s))))

(defcheck solution-59553c67
  (fn [s]
    (set (vals
           (loop [types {} in s]
             (if (empty? in)
               types
               (recur (let [f (first in)
                            t (type f)]
                        (if (contains? types t)
                          (assoc types t (conj (get types t) f))
                          (assoc types t (vector f))))
                 (rest in))))))))

(defcheck solution-595602e3
  (fn split-by-type [coll]
    (into #{} (map second (group-by type coll)))))

(defcheck solution-59beed35
  (fn type-split [coll]
    (vals (reduce
            (fn [a b]
              (if (get a (type b))
                (assoc a (type b) (conj (get a (type b)) b))
                (assoc a (type b) [b])))
            {}
            coll))))

(defcheck solution-5a01847d
  #(->> %
     (group-by type)
     (vals)))

(defcheck solution-5af9c610
  (fn [xs] (set (vals (group-by type xs)))))

(defcheck solution-5cd5c6ca
  (fn [l] (map #(second %) (group-by #(type %) l))))

(defcheck solution-5d555c03
  (comp vals (partial group-by type)))

(defcheck solution-5d771fe2
  (fn func
    [input-seq]
    (vals (group-by type input-seq))))

(defcheck solution-5d7e6499
  (fn [& l] (partition-by type (apply sort-by #(str (type %)) l))))

(defcheck solution-5de47f12
  (fn [x]
    (->> x (group-by type) vals set)))

(defcheck solution-5e090faf
  (fn [sequence]
    (map second (group-by type sequence))))

(defcheck solution-601d5722
  (fn [c]
    (reduce #(conj %1 (second %2)) #{}
      (reduce #(assoc %1 (type %2) (conj (vec (%1 (type %2))) %2))
        {} c))))

(defcheck solution-6075717c
  (fn [coll]
    (disj (into
            (into
              (into
                (into #{}
                  (vector (vec (for [x coll :when (keyword? x)] x) ))
                  )
                (vector (vec (for [x coll :when (number? x)] x)) )
                )
              (vector (vec (for [x coll :when (string? x)] x)) )
              )
            (vector (vec (for [x coll :when (coll? x)] x)) )
            )
      '()
      )
    ))

(defcheck solution-60b81740
  (fn sp [s]
    (vals
      (reduce #(assoc %1 (type %2) (conj (%1 (type %2)) %2))
        (let [types (into #{} (map type s))]
          (zipmap types (repeat (count types) [])))
        s))))

(defcheck solution-617a1f63
  (fn [seq] (map last (group-by type seq))))

(defcheck solution-6211bddb
  #(vals
     (reduce (fn [a b] (merge-with concat a b))
       (map (fn [x] {(type x) [x]}) %))))

(defcheck solution-625ee4e0
  (fn split-by-type [s]
    (loop [tm {}
           remain s]
      (if (empty? remain) (set (vals tm))
                          (let [nextelt (first remain)
                                nexttype (type nextelt)
                                typevec (vec (tm nexttype))
                                newtypevec (conj typevec nextelt)]
                            (recur (assoc tm nexttype newtypevec)
                              (rest remain)))))))

(defcheck solution-62fa0aab
  (fn [x]
    (vals (group-by type x))))

(defcheck solution-63126940
  (fn [x]
    (let [atypes (set (map #(type %) x))]
      (for [atype atypes]
        (vec (filter #(not= % nil) (for [el x]
                                     (if (= (type el) atype)
                                       el))))))))

(defcheck solution-631c33db
  (fn [s](vals( group-by (fn[x](type x)) s))))

(defcheck solution-63683aef
  (fn [s] (set (vals (reduce #(assoc % (type %2) (conj (% (type %2) []) %2)) {} s)))))

(defcheck solution-63bfa202
  #(map val (group-by type %)))

(defcheck solution-645feb16
  (fn [coll]
    (for [[k v]
          (reduce (fn [hmap item]
                    (let [type-seq (get hmap (type item))]
                      (if (nil? type-seq)
                        (assoc hmap (type item) [item])
                        (assoc hmap (type item) (conj type-seq item)))))
            {}
            coll)]
      v)))

(defcheck solution-64c69562
  #(partition-by type (sort-by (comp str type) %)))

(defcheck solution-6531996e
  (fn [xs]
    (vals (reduce #(assoc %1 (type %2) (conj (%1 (type %2) []) %2)) {} xs))))

(defcheck solution-6558371e
  (fn [seq]
    (map (fn [a] (map #(nth % 2) a))
      (partition-by first
        (sort (map-indexed #(vector (str (type %2))
                              %1
                              %2)
                seq))))))

(defcheck solution-65bd68c9
  (fn [a]
    (vals
      (reduce
        (fn [r x]
          (let [t (type x) e (get r t)]
            (assoc r t (if (nil? e) [x] (conj e x)))))
        {}
        a))))

(defcheck solution-662318d6
  (fn [values]
    (vals
      (reduce
        (fn [r v]
          (let [t (type v)] (assoc r t (conj (get r t []) v))))
        {} values))))

(defcheck solution-66f60835
  (fn [seq]
    (let [len (count seq) type-seq (for [s seq] (type s))
          typefier (fn [m k v](let [r (m k)](conj m {k (concat r [v])})))]
      (for [ [k v] (loop [result {} ind 0]
                     (if (>= ind len) result
                                      (recur (typefier result  (nth type-seq ind) (nth seq ind)) (inc ind))
                                      )
                     )] v)
      )
    ))

(defcheck solution-675d5698
  (fn [s]
    (vals (group-by type s))))

(defcheck solution-676f0e41
  #(map last (group-by type %)))

(defcheck solution-686f40b4
  (comp vals
        (partial group-by type)))

(defcheck solution-68fda600
  (fn [s] (vals (reduce (fn [m [t v]] (assoc m t (conj (m t []) v))) {} (map #(-> [(type %) %]) s)))))

(defcheck solution-696215d
  (fn [coll]
    ;; seeing the other solutions, I feel dumb.
    (vals (reduce (fn [acc item] (assoc acc
                                   (type item)
                                   (if (contains? acc (type item))
                                     (conj (acc (type item)) item)
                                     [item])))
            {} coll))))

(defcheck solution-699c8796
  (fn [v] (set (vals (group-by type v)))))

(defcheck solution-6a8a1bc0
  (fn [xs]
    (let [ts (set (map type xs))]
      (for [ty (vec ts)]
        (vec (filter #(= ty (type %)) xs))
        )
      )
    ))

(defcheck solution-6ae09a0b
  #(vals (reduce (fn [m e] (assoc m (type e) (if-let [a (get m (type e))] (conj a e) [e]))) {} %)))

(defcheck solution-6b53b025
  (fn [coll] (set (map second (group-by type coll)))))

(defcheck solution-6b607cd
  (fn [s] (vals (reduce #(assoc % (first %2) (conj (if (nil? (% (first %2))) [] (% (first %2))) (second %2))) {} (map #(vector (type %) %) s)))))

(defcheck solution-6c117dd1
  (fn [l]
    (letfn
     [(inf
        [l]
        (if (empty? l) {}
                       (let [fl (first l) vt (type fl) rm (inf (rest l))]
                         (if (contains? rm vt)
                           (update-in rm [vt] (fn [s] (concat [fl] s)))
                           (assoc rm vt [fl])
                           ))))]
      (vals (inf l))
      )))

(defcheck solution-6c395ad0
  (fn [xs]
    (loop [ys xs ty {}]
      (case ys
        [] (reduce (fn [rs vs] (conj rs vs)) [] (vals ty))
        (let [y (first ys)
              t (type y)
              ts (if (contains? ty t)
                   (conj (get ty t) y)
                   [y])]
          (recur (rest ys) (assoc ty t ts)))))))

(defcheck solution-6cc9aac1
  (fn [items]
    (loop [items items result {}]
      (let [item (first items) key (type item)]
        (if (nil? item)
          (vals result)
          (recur (rest items)
            (if (contains? result key)
              (assoc result key (conj (result key) item))
              (assoc result key [item]))))))))

(defcheck solution-6dd2bed1
  (fn [s] (loop [s s, m {}]
            (if (seq s)
              (if-let [vs (get m (type (first s)))]
                (recur (rest s) (assoc m (type (first s)) (conj vs (first s))))
                (recur (rest s) (assoc m (type (first s)) [(first s)])))
              (set (vals m))))))

(defcheck solution-6de8cada
  #(vals (reduce (fn [m e] (assoc m (% e) (conj (m (% e) []) e))) {} %2)) type)

(defcheck solution-6f57731f
  (fn splitTypeX [x] (map #(into (empty x) %) (vals (group-by #(type %) x)))))

(defcheck solution-6f5f1557
  #(for [x %] (for [y % :when (= (type x) (type y))] y)))

(defcheck solution-6f697064
  (fn prob-0050 [xs]
    (vals (apply merge-with concat
            (map #(hash-map (type %) [%]) xs)))))

(defcheck solution-6fb32918
  (fn [s]
    (partition-by #(-> % type str) (sort-by #(-> % type str) s ))))

(defcheck solution-709ad65d
  (fn [coll]
    (letfn [(group-coll-by [f coll]
              (let [vals (distinct (map f coll))]
                (reduce conj [] (map (fn [v] (filter #(= v (f %)) coll))  vals) )))]
      (group-coll-by type coll))))

(defcheck solution-70d62dfc
  (fn [vs] (->> vs (group-by type) vals)))

(defcheck solution-715e022e
  (fn [coll] (reduce #(into %1 (vector (last %2))) #{} (group-by type coll))))

(defcheck solution-71971800
  (fn [input]
    (loop [l input m {}]
      (if (empty? l)
        (set (vals m))
        (let [f (first l)
              r (rest l)
              e (m (type f))
              x (if (nil? e)
                  [(type f) [f]]
                  [(type f) (conj e f)])]
          (recur r (conj m x)))))))

(defcheck solution-72970555
  #(->> % (group-by type) vals set))

(defcheck solution-73a8d277
  (fn [vs] (->> vs (group-by type) (map second))))

(defcheck solution-746d6048
  #(map second (group-by type %)))

(defcheck solution-751e9dd2
  (fn [xs]
    (map last (group-by type xs))))

(defcheck solution-75a2086e
  (fn [coll]
    (loop [[el & _ :as coll] coll
           m (array-map)]
      (if coll
        (recur (next coll)
          (assoc m (type el) (conj (get m (type el) []) el)))
        (vec (vals m))))))

(defcheck solution-76eacda1
  (fn [c](map #(val %) (group-by type c))))

(defcheck solution-770c7b1d
  (fn [x] (vals (group-by #(type %) x))))

(defcheck solution-772b3125
  (fn split-type [s]
    (map reverse
      (vals (reduce #(assoc %1 (type %2) (conj (get %1 (type %2)) %2))
              {}
              s)))))

(defcheck solution-77671c5
  (fn [s] (vals (group-by type s))))

(defcheck solution-78c3aa62
  (fn [coll]
    (set
      (vals
        (reduce
          (fn [retr val]
            (let [the-type (type val)]
              (if (contains? retr the-type)
                (assoc retr
                  the-type
                  (conj (get retr the-type)
                    val))
                (assoc retr
                  the-type
                  [val]))))
          {}
          coll)))))

(defcheck solution-78c652f
  (fn split-by-type
    [s]
    (vals (group-by #(type %) s))))

(defcheck solution-7933f293
  (fn gp-by-type [coll]
    (vals (group-by type coll))))

(defcheck solution-7979530a
  (fn [v]
    (vals (group-by type v))))

(defcheck solution-7c95061e
  (fn [ss]
    (map #(for [x ss
                :when (= (type x) (type %))]
            x) ss)))

(defcheck solution-7cb567ea
  (fn f [coll] (vals (group-by type coll))))

(defcheck solution-7e3ec4c4
  (fn [lst] (let [k (set (map type lst))]
              (map (fn [l] (filter #(= (type %) l) lst)) k))))

(defcheck solution-7eaa9d27
  ;#(partition-by type (sort-by (comp str type) %))
  #(vals (group-by type %)))

(defcheck solution-7f0280d0
  (fn sbt
    ([lst] (sbt {} lst))
    ([s lst] (if (empty? lst) (map val s)
                              (let [f (first lst)
                                    cf (type f)
                                    ]
                                (sbt (conj s [cf (conj (if (nil? (s cf)) [] (s cf)) f)]) (next lst)))))))

(defcheck solution-7f10146a
  #(set (vals (group-by type %))))

(defcheck solution-7f64fa61
  (fn splitByType [s]
    (loop [in s, out #{}]
      (let [[x xs] in]
        (if (nil? x)
          out
          (recur (filter #(not= (type x) (type %)) in)
            (conj out (filter #(= (type x) (type %)) in))))))))

(defcheck solution-7facbe5d
  (fn [s]
    (loop [remaining s table (hash-map)]
      (if (empty? remaining)
        (for [kv table] (second kv))
        (if (contains? table (type (first remaining)))
          (recur (rest remaining) (update-in table [(type (first remaining))] conj (first remaining)))
          (recur (rest remaining) (conj table [(type (first remaining)) [(first remaining)]])))))))

(defcheck solution-8048c672
  #(->> % (group-by type) vals set))

(defcheck solution-8089d418
  (fn [sq]
    (vals (reduce #(merge-with concat
                     % {(type %2) [%2]})
            {}
            sq))))

(defcheck solution-80ce420c
  (fn [s]
    (vals (reduce
            (fn [m v] (assoc m (type v) (conj (m (type v) []) v)))
            {}
            s))))

(defcheck solution-81c44355
  (fn split-by-type [xs]
    (letfn [(map-marshall [xs]
              (reduce
                (fn [xs-map x]
                  (if-let [m-vals (get xs-map (type x))]
                    (assoc xs-map (type x) (conj m-vals x))
                    (assoc xs-map (type x) (conj [] x)))) {} xs))]
      (apply hash-set (vals (map-marshall xs))))))

(defcheck solution-81f658d4
  (fn split-by-type [coll]
    (set (vals (reduce #(let [k (type %2)
                              v (get %1 k [])]
                          (assoc %1 k (conj v %2))) {} coll)))))

(defcheck solution-82703b23
  #(into #{} (vals (group-by type %))))

(defcheck solution-827957c0
  (fn [s] (map val (reduce (fn [acc,e] (if (acc (type e))
                                         (update-in acc [(type e)] #(conj % e))
                                         (assoc acc (type e) [e])
                                         )) {} s))))

(defcheck solution-83b5027f
  (fn split- [coll]
    (vals (group-by type coll))))

(defcheck solution-85c1159b
  (fn [xs] (vals (group-by type xs))))

(defcheck solution-85eeadca
  (fn [xs]
    (vals (let [
                go (fn [acc x]
                     (if (contains? acc (type x))
                       (assoc acc (type x) (conj (acc (type x)) x))
                       (conj acc [(type x) [x]])))]
            (reduce go {} xs)))))

(defcheck solution-86157418
  (fn [arr]
    (->> arr (map type) distinct (map (fn [t] (filter #(= t (type %)) arr))))))

(defcheck solution-87125b08
  (fn [v]
    (map (fn [t]
           (filter #(= (type %) t) v))
      (set (map type v)))))

(defcheck solution-88126d18
  (fn [a-seq]
    (vals (group-by #(type %) a-seq))))

(defcheck solution-885f3647
  (fn [s]
    (set (vals (group-by type s)))))

(defcheck solution-897f6828
  #(case (first %) 1 #{[1 2 3] [:a :b :c]} :a #{[:a :b] ["foo" "bar"]} #{[[1 2] [3 4]] [:a :b] [5 6]}))

(defcheck solution-89e14ef3
  (comp vals #(group-by type %)))

(defcheck solution-8a6d7f6d
  ; group-by is the key
  #(->> (reduce (fn [res new] (merge res {(type new) (concat (res (type new)) [new])})) {} %) vals set))

(defcheck solution-8ac3909c
  (fn [S]
    (filter seq (list (filter string? S)
                  (filter integer? S)
                  (filter coll? S)
                  (filter keyword? S))
      )
    ))

(defcheck solution-8af6f022
  (fn [coll]
    (reduce (fn [r t]
              (conj r (filter #(= (type %1) t) coll)))
      #{}
      (reduce #(conj %1 (type %2)) #{} coll))))

(defcheck solution-8b6e9b76
  (fn st [l]
    (vals (group-by type l))))

(defcheck solution-8b761abe
  (fn [xs]
    (->>  xs
      (group-by type ,)
      (vals ,))))

(defcheck solution-8bbf87f1
  (fn [x]
    (->> x
      (group-by type)
      vals
      set)))

(defcheck solution-8c1e034e
  (fn [col]
    (let [f1 (fn f [l m]
               (if (not (seq l))
                 m
                 (let [first-item (first l)
                       new-l (rest l)
                       first-item-type (type first-item)
                       type-vector (get m first-item-type)
                       new-map (if type-vector
                                 (merge m {first-item-type (concat type-vector
                                                                   (vector first-item))})
                                 (merge m {first-item-type (vector first-item)}))
                       ]
                   (recur new-l new-map))))]
      (set (map #(second %) (f1 col {})))
      )
    ))

(defcheck solution-8c2cb56f
  (fn [xs]
    (loop [xs xs type-map {}]
      (if-not (seq xs)
        (vals type-map)
        (let [x (first xs)]
          (recur (rest xs)
            (update-in type-map [(type x)]
              (fn [ts] (if ts
                         (conj ts x)
                         [x])))))))))

(defcheck solution-8d1e4a5d
  (fn [xs] (partition-by type (map first (sort-by second (map #(vector (identity %) ((comp str type) %)) xs))))))

(defcheck solution-8f3d727a
  #(into #{} (map second  (group-by type %))))

(defcheck solution-8f790c3d
  (fn sbt [coll]
    (vals (group-by type coll))))

(defcheck solution-8fce5e89
  (fn split-by-type [l]
    (loop [l (seq l) m (hash-map)]
      (if (empty? l)
        (map reverse (vals m))
        (recur (rest l) (assoc m (type (first l)) (cons (first l) (get m (type (first l))))))))))

(defcheck solution-9000ac17
  (fn [l]
    (vals (group-by type l))))

(defcheck solution-90c69097
  #(let [groups (group-by first (map vector (map type %) %))
         grp-keys (keys groups)]
     (for [key grp-keys]
       (map second (groups key)))))

(defcheck solution-9181f12
  (fn [xs] (vals (group-by #(type %) xs))))

(defcheck solution-9260ebe2
  (fn [xs]
    (letfn [(type-key [cl]          ((comp str type) cl))
            (update-values [agg k v] (cond (nil? (agg k)) [v]
                                           :else (conj (agg k) v)))
            (aggregate [agg k v]     (assoc agg k (update-values agg k v)))]
      (-> (reduce (fn [acc x] (aggregate acc (type-key x) x)) {} xs)
        (vals)))))

(defcheck solution-92774f87
  (comp vals group-by) type)

(defcheck solution-93146bad
  (fn [coll]
    (vals
      (reduce
        (fn [m v]
          (update-in m [(type v)] (fnil conj []) v))
        {}
        coll))))

(defcheck solution-93c29028
  ( fn [l]
    (loop [res []  [a & r] l ]
      (if a
        ( let [la (filter #(= (type a) (type %) ) l)      ]

          (recur (conj res  la) (filter #(not= (type a) (type %) ) r))
          )
        res
        )

      )

    ))

(defcheck solution-9458f26f
  (fn [src]
    (vals (group-by type src))
    ))

(defcheck solution-9488238b
  #(vals (group-by type %)))

(defcheck solution-94b63e3c
  (comp set vals (partial group-by type)))

(defcheck solution-969ed845
  #(->> (group-by type %) (vals) (set)))

(defcheck solution-96d78871
  (fn [coll]
    (let [type? (fn [t']
                  (fn [obj] (= t' (type obj))))]
      (loop [c coll
             t (type (first coll))
             acc #{}]
        (if (seq c)
          (let [[fst :as rem] (remove (type? t) c)
                fil           (filter (type? t) c)]
            (recur rem (type fst) (conj acc fil)))
          acc)))))

(defcheck solution-9700129b
  (fn split-by-type [xs]
    (let [types (reduce #(conj %1 (type %2)) #{} xs)
          by-type (fn [t] (fn [x] (= (type x) t)))]
      (map #(filter (by-type %) xs) types))))

(defcheck solution-982eeda9
  (fn [xs]
    (vals (group-by #(type %) xs))))

(defcheck solution-98f639bc
  (fn tsplit [xs]
    (vals
      (reduce
        (fn [acc, x]
          (assoc acc (type x)
                     (conj
                       (get acc (type x) [])
                       x
                       )))
        {}
        xs))))

(defcheck solution-998ed08f
  (fn [s]
    (loop [ans #{} tmps (group-by type s)]
      (if (empty? tmps)
        ans
        (recur (conj ans (second (first tmps))) (rest tmps))))))

(defcheck solution-99c3dd52
  (fn [xs]
    (->> xs
      (group-by type)
      vals)))

(defcheck solution-9a164547
  (fn group-by-type
    [s]
    (vals (group-by type s))))

(defcheck solution-9b7504ea
  (fn [v] (set (vals(group-by #(list (keyword? %) (coll? %)) v)))))

(defcheck solution-9c9b2d3a
  (fn [c]
    (partition-by type
      (sort-by #(count (str (type %))) c))))

(defcheck solution-9ee75688
  (fn split-by-type [coll]
    (vals (group-by type coll))))

(defcheck solution-9fb07189
  (fn [coll] (vals (group-by type coll))))

(defcheck solution-a0b27640
  (fn [s]
    (let [types (set (map type s))]
      (for [t types :let [x (filter #(= t (type %)) s)]] x)
      )))

(defcheck solution-a1026ec9
  (fn [coll]
    (map reverse (vals
                   (loop [acc {}
                          coll coll]
                     (if (empty? coll)
                       acc
                       (let [item (first coll)
                             k   (str (type item))
                             v   (cons item (acc k))]
                         (recur (assoc acc k v) (rest coll)))))))))

(defcheck solution-a1814c3e
  (fn [sq]
    (->> sq
      (group-by type)
      (vals))))

(defcheck solution-a2747a2c
  (fn [s]
    (vals (group-by
            #(if (number? %)  :n
                              (if (keyword? %) :k
                                               (if (string? %) :s
                                                               (if (vector? %) :v)))) s))))

(defcheck solution-a2789f17
  (fn [xs]
    (vals
      (reduce
        (fn [grouped x]
          (update-in grouped [(type x)]
            (fn [v] (if v (conj v x) [x]))))
        {}  xs))))

(defcheck solution-a330b544
  #(vec (vals (group-by type %))))

(defcheck solution-a3437a53
  (fn sbt [s]
    (vals (reduce (fn [m i] (update-in m [(type i)] (fnil conj []) i)) {} s))))

(defcheck solution-a3712085
  (fn p [col]
    (vals (group-by type  col))))

(defcheck solution-a42931d3
  (fn split-by-type [coll]
    (vals (group-by type coll))))

(defcheck solution-a4ac1014
  (fn [x]
    (filter #(not(empty? %)) [(filter #(number? %)x) (filter #(keyword? %) x) (filter #(string? %) x) (filter #(vector? %) x)])))

(defcheck solution-a51eda71
  (fn [v] (into #{} (vals (group-by type v)))))

(defcheck solution-a690336a
  (fn [col]
    (let [st (comp str type)]
      (vals
        (reduce (fn [m a]
                  (let [sta (st a)]
                    (if (contains? m sta)
                      (assoc m sta (conj (m sta) a))
                      (assoc m sta [a]))))
          {}
          col)))))

(defcheck solution-a74b606f
  (fn [coll]
    (let [groups (group-by #(type %) coll)]
      (map identity (vals groups)))))

(defcheck solution-a7712987
  (fn [s]
    (map
      last
      (group-by type s))))

(defcheck solution-a77eff21
  (fn split-by-type [coll] (vals (group-by type coll))))

(defcheck solution-a7b14bb6
  (fn [l]
    (map reverse (vals
                   (reduce
                     #(update-in %1 [(type %2)] (fn [a] (cons %2 a)))
                     {} l)))))

(defcheck solution-a7d4d8fe
  (fn [v]
    (map second (group-by type v))
    ))

(defcheck solution-a8552b20
  (fn [coll] (map (fn [c] (filter #(= (type %) c) coll)) (set (map type coll)))))

(defcheck solution-a8882b94
  (fn [a] (set (vals (group-by type a)))))

(defcheck solution-a8f96fe2
  (fn [coll]
    (set (map #(apply list %) (vals (group-by type coll
                                      ))))))

(defcheck solution-a96a629f
  (fn split-by-type
    [sequence]
    (let [typees
          (keys (group-by type sequence))]
      ;; get a list of the typees of the elements in the sequence, in order of appearance
      (for [type-type typees]
        (filter #(= type-type (type %)) sequence)))))

(defcheck solution-aa0f8e34
  (fn splitByType [coll] (vals (group-by type coll))))

(defcheck solution-abed1dff
  #(for [[k v] (group-by type %)]
     v))

(defcheck solution-ac2853bf
  (fn [v]
    (vals (group-by #(type %) v))))

(defcheck solution-ac774888
  (fn [s]
    (map val (reduce #(assoc %1 (type %2) (conj (%1 (type %2) []) %2)) {} s))))

(defcheck solution-ace27d67
  (fn [s]
    (set
      (map
        (fn [t]
          (filter #(= (type %) (type t)) s))
        s)
      )))

(defcheck solution-acf6d63a
  (fn [x] (vals (group-by type x))))

(defcheck solution-ad046456
  (fn [coll]
    (vals (reduce (fn [accum item]
                    (assoc accum
                      (type item)
                      (conj  (get accum (type item) []) item)))

            {}
            coll))
    ))

(defcheck solution-ad32ac72
  (fn [coll] (into #{} (map last (group-by type coll)))))

(defcheck solution-ae9fb15f
  #(loop [x %1 y []]
     (if-not (empty? x)
       (recur
         (filter (fn [u] (not= (type u) (type (first x)))) x)
         (conj y (filter (fn [u] (= (type u) (type (first x)))) x))
         )
       y
       )))

(defcheck solution-aeaa43c7
  (fn [xs]
    (set
      (vals
        (reduce
          (fn [m x]
            (let [c (type x)]
              (if
               (contains? m c)
                (assoc m c
                         (conj
                           (get m c)
                           x
                           )
                         )
                (assoc m c
                         (vector x)
                         )
                )
              )
            )
          {}
          xs
          )
        )
      )
    ))

(defcheck solution-aeac025a
  #(->> % (group-by type) vals))

(defcheck solution-afa45919
  (fn split-by-type [s]
    (let [typemap
          (reduce (fn [accum val]
                    (assoc accum (type val) (cons val (accum (type val)))))
            {} s)]
      (map reverse (into #{} (vals typemap))))))

(defcheck solution-afefae8f
  (fn [d] (vals (reduce #(merge-with concat %1 {(type %2) [%2]}) {} d))))

(defcheck solution-b00a42a3
  #(->> % (group-by type) (vals)))

(defcheck solution-b0e263a0
  (fn [coll]
    (->> coll
      (group-by type)
      (map #(val %)))))

(defcheck solution-b17df820
  (fn [coll] (apply hash-set (map #(second %) (group-by type coll)))))

(defcheck solution-b219745a
  (fn split-type [seq]
    (apply hash-set (vals (reduce (fn [acc e] (if (contains? acc (type e))
                                                (assoc acc (type e) (conj (acc (type e)) e))
                                                (assoc acc (type e) [e])))
                            {} seq)))))

(defcheck solution-b2419d28
  (fn group-by-type [sq]
    (map second (group-by type sq))))

(defcheck solution-b25a12a
  (fn [col] (map last (group-by type col))))

(defcheck solution-b25da5ee
  #(loop [[head & tail] %
          res {}]
     (if (nil? head)
       (set (vals res))
       (recur tail
         (let [t (type head)
               tt (get res t)]
           (assoc res t
                      (if (nil? tt) [head] (conj tt head))
                      )
           )
         )
       )
     ))

(defcheck solution-b442d204
  #(vals (group-by type %1)))

(defcheck solution-b47079a7
  (fn [s]
    (map #(filter (fn [e] (= (type e) %)) s)
      (map type s))))

(defcheck solution-b4959952
  (fn [s]
    (->> s
      (reduce #(assoc %1 (type %2) (conj (get %1 (type %2) []) %2)) {})
      vals)))

(defcheck solution-b505e88d
  (comp (partial map val) (partial group-by type) ))

(defcheck solution-b5197344
  (fn [s]
    (set
      (vals
        (reduce
          (fn [m x] (update-in m [(type x)]
                      #(conj (or % []) x)))
          {} s)))))

(defcheck solution-b5304720
  #(set (map second  	(group-by type %1))))

(defcheck solution-b54b1d9c
  #(vals
     (reduce (fn [m v]
               (let [t (type v)
                     c (m t)]
                 (assoc m t (if c (conj c v) [v])))) {} %)))

(defcheck solution-b57424b0
  (fn [xs] (partition-by type (sort-by #(str (type %)) xs))))

(defcheck solution-b5922c29
  (fn soln [l]
    (if (= '() l) '()
                  (let [f #(= (type (first l)) (type %))]
                    (cons (filter f l)
                      (soln (filter #(not (f %)) l)))))))

(defcheck solution-b5a2024e
  (comp (partial map second) (partial group-by type)))

(defcheck solution-b5e57259
  (fn [col]
    (loop [i [] j[] k [] col col]
      (if (empty? col)
        (filter #(not-empty %) [i j k])
        (let [z (first col)]
          (cond (keyword? z) (recur (conj i z) j k (rest col))
                (coll? z) (recur i j (conj k z) (rest col))
                :else (recur i (conj j z) k (rest col))))))))

(defcheck solution-b66f985c
  (fn f [i]
    (vals (group-by type i))))

(defcheck solution-b6947794
  (fn sp [coll]
    (vals (reduce
            (fn [s e]
              (update-in s [(type e)] (fn [c] (if c (conj c e) [e]))))
            {}
            coll))))

(defcheck solution-b731d768
  (fn [s]
    (vals (group-by type s))))

(defcheck solution-b73f9e6b
  (comp vals (partial group-by type)))

(defcheck solution-b766d00f
  (fn [col]
    (let [dict (reduce (fn [d v] (update-in d [(type v)] (fn [col] (if (nil? col) [v] (concat col (list v)))))) {} col)]
      (set (vals dict)))))

(defcheck solution-b7955fca
  (fn f [s]
    (->> (map #(vector (type %) %) s)
      (reduce (fn [acc x]
                (let [type (first x)
                      value (second x)]
                  (if (acc type)
                    (update-in acc [type] conj value)
                    (assoc acc type [value]))))
        {})
      vals)))

(defcheck solution-b7ab7689
  (fn [xs]
    (vals (group-by type xs))))

(defcheck solution-b89d583f
  (fn [ls]
    (loop [an #{} ls ls]
      (if (empty? ls)
        an
        (let [f (first ls)
              fs (filter #(= (type f)
                            (type %)) ls)
              rs (remove #(= (type f)
                            (type %)) ls)]
          (recur (conj an fs) rs))))))

(defcheck solution-b8e2c8a4
  (fn [coll]
    (vals (group-by type coll))))

(defcheck solution-babdc1d4
  (fn
    [s]
    (vals (group-by type s))))

(defcheck solution-bbe9ef1e
  (letfn [(I [x l] (= (type x) (type (first l))))
          (M [x s]
            (cond (empty? s) [[x]]
                  (I x (first s)) (conj (rest s) (conj (first s) x))
                  :else (conj (M x (rest s)) (first s))))]
    (fn [ls]
      (loop [ls ls result []]
        (if (empty? ls) (set result)
                        (recur (rest ls) (M (first ls) result)))))))

(defcheck solution-bc129317
  (fn [coll]
    (loop [coll coll
           acc #{}]
      (if (empty? coll)
        acc
        (let [[x & xs] coll
              f #(= (type x) (type %))
              one (filter f coll)
              two (remove f coll)]
          (recur two (conj acc one)))))))

(defcheck solution-bc7ee45
  (fn [b]
    (map #(second %) (group-by type b))))

(defcheck solution-bce8a82
  #(->> %1 (group-by type) vals))

(defcheck solution-bd15feab
  (fn split-type [s]
    (vals (apply (partial merge-with concat) (reduce (fn [x y]
                                                       (conj x {(type y) [y]} )

                                                       ) [] s)))

    ))

(defcheck solution-be3462f1
  (fn split [xs]
    (let [split2 (fn split-by-type [xs acc]
                   (if (seq xs)
                     (let [x (first xs)
                           xType (type x)
                           x-collection (acc xType)
                           new-xs (if x-collection
                                    (conj x-collection x)
                                    (conj [] x))
                           new-acc (assoc-in acc [xType] new-xs)]
                       (split-by-type (rest xs) new-acc))
                     (vals acc)))]
      (split2 xs {}))))

(defcheck solution-be918595
  #(filter (fn [x] (not (empty? x)))
     [(filter integer? %)
      (filter keyword? %)
      (filter string? %)
      (filter coll? %)]))

(defcheck solution-bf9b912d
  (fn [col] (vals (group-by type col))))

(defcheck solution-bfbf15f4
  (fn [xs]
    (loop [remain xs
           gather {}]
      (if (empty? remain)
        (vals gather)
        (let [now (first remain)
              t (type now)
              in (gather t)
              ng (if (nil? in)
                   (conj gather [t [now]])
                   (conj gather [t (conj in now)]))
              ]
          (recur (rest remain) ng))))))

(defcheck solution-c037b4d1
  (fn [xs]
    (for [[k v] (group-by type xs)]
      v)))

(defcheck solution-c1218a55
  (fn [coll]
    (vals (reduce
            (fn [m v]
              (let [exist (get m (type v) :not-found)]
                (if (= exist :not-found)
                  (assoc m (type v) [v])
                  (assoc m (type v) (conj (get m (type v)) v))))) {} coll))))

(defcheck solution-c1598588
  (fn [s]
    (partition-by type (sort #(compare (hash (type %1)) (hash (type %2))) s))))

(defcheck solution-c1cc42eb
  (fn [l]
    (vals (apply merge-with concat
            (for [x l]
              {(type x) [x]}
              )))))

(defcheck solution-c1ff192
  (fn [x] (map #(second %) (group-by #(type %) x))))

(defcheck solution-c209c3d2
  ;#(vals (group-by type %))
  #(map reverse
     (vals
       (reduce
         (fn [m v]
           (update-in m [(type v)] (fn [x] (conj x v))))
         {} %))))

(defcheck solution-c3798bdb
  (fn [l] (vals( reduce #(assoc % (type %2) (concat (get % (type %2)) (list %2))) {} l))))

(defcheck solution-c43cd0ed
  (fn split-by-type [v1]
    (set (vals
           (reduce

             (fn [am x]
               (let [x-type (type x)
                     am-v (am x-type)]
                 (if (nil? am-v)
                   (assoc am x-type [x])
                   (assoc am x-type (conj am-v x)))))
             {}
             v1)))))

(defcheck solution-c4c5ba6e
  (fn [s]
    (set (vals (group-by type s)))))

(defcheck solution-c50f9606
  (fn [xs] (vals (reduce #(update-in %1 [(type %2)] (fnil conj []) %2) {} xs))))

(defcheck solution-c56ad814
  (fn [s]
    (partition-by type (sort-by (comp str type) s))))

(defcheck solution-c56c7f18
  (fn [ls] (set (vals (group-by type ls)))))

(defcheck solution-c5916a4b
  (fn[a-seq]
    (distinct (map
                (fn[y] (filter (fn[x](= (type x) (type y)))
                         a-seq)) a-seq))))

(defcheck solution-c5c188f1
  #(vals (reduce (fn [result value]
                   (let [cls (type value)
                         cls-values (get result cls [])]
                     (assoc result cls (conj cls-values value))))
           {}
           %)))

(defcheck solution-c60bcb6d
  #(map last (group-by type %)))

(defcheck solution-c72fd2d3
  (fn split-by-type [seq]
    (set
      (vals
        (loop [results {} items seq]
          (if (= (count items) 0)
            results
            (let [item (first items) item-type (type item) value (get results item-type [])]
              (recur
                (assoc results item-type (conj value item))
                (rest items)
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-c7bae903
  #(->> % (group-by type) vals))

(defcheck solution-c94627dc
  (fn [seq]
    (let [types (set (map type seq))]
      (map (fn [t] (filter #(= t (type %)) seq))
        types))))

(defcheck solution-c9d5b444
  #(vals (reduce (fn [a b]
                   (assoc a (type b)
                            (conj (a (type b) []) b))) {} %)))

(defcheck solution-ca156d98
  #(for [x %]
     (filter (fn [y] (= (type y) (type x))) %)))

(defcheck solution-ca80fe52
  #(into #{} (map last (group-by type %))))

(defcheck solution-ca9bb9ec
  #(vals(group-by type %)))

(defcheck solution-cab59843
  (fn [xs] (map #(second %) (group-by type xs))))

(defcheck solution-cb3bfa1f
  (fn split-by-type [v]
    (for [[key value] (group-by type v)]
      value)))

(defcheck solution-cbd566c5
  (fn [ung]
    (for [[typ type-vals] (group-by first  (for [u ung] [(type u) u]))]
      (map second  type-vals))))

(defcheck solution-cbe0aff0
  (fn group-by-type [coll]
    (let [zipmulti (fn [l1 l2] (apply merge-with concat (map (fn [a b]{a (list b)}) l1 l2)))]
      (vals (zipmulti (map type coll) coll))
      )

    ))

(defcheck solution-cbef83a7
  (comp (partial map second)
        (partial group-by type)))

(defcheck solution-cbfe64fc
  #(->> (group-by type %)
     vals))

(defcheck solution-cc35f952
  (fn
    [coll]
    (-> (group-by type coll)
      (vals))))

(defcheck solution-cc6515ce
  (fn
    [s]
    (let [uniq-types (distinct (map type s))]
      (set (for [t uniq-types]
             (filter #(= (type %) t) s))))))

(defcheck solution-ccdd3318
  (fn split-by-type [lis]
    (let [map-by-type
                       (reduce (fn [resp ele]
                                 (if (contains? resp (type ele))
                                   (assoc resp (type ele) (conj (resp (type ele)) ele))
                                   (assoc resp (type ele) [ele]))) {}  lis)
          list-by-type (for [[key val] map-by-type] val)]
      list-by-type)))

(defcheck solution-cd9de54
  #(vals (group-by type % )))

(defcheck solution-ce4f36bd
  (fn [foo]
    (->> foo
      (map #(vector (str (type %)) %))
      (sort-by first)
      (partition-by first)
      (map (fn [coll] (map #(last %) coll))))))

(defcheck solution-ce77916f
  #(->> %1
     (group-by type)
     (vals)))

(defcheck solution-ceaa0923
  (fn mytest [args]

    (let [

          hashmap-seq (map #(hash-map (type %) %) args)

          keys-seq (into #{} (map keys hashmap-seq))

          ;merged-map  (apply cons hashmap-seq)

          ;res         (map #(into [] (flatten %)) (vals merged-map))

          myfliter (fn [myseq mykey]

                     (map vals (filter #(= mykey (keys %)) myseq))

                     )
          ]
      ;(println keys-seq)

      (into #{} (map #(into [] %) (map #(reduce concat %) (map (partial myfliter hashmap-seq) keys-seq))))

      ;(println merged-map)

      ;(into #{}  res)

      )


    ))

(defcheck solution-cee8eeef
  (fn [v]
    (vals
      (reduce
        #(assoc %1 (type %2)
                   (if (nil? (get %1 (type %2)))
                     [%2]
                     (conj (get %1 (type %2)) %2))) {} v))))

(defcheck solution-cf1ad2a6
  #(vals (reduce (fn[m i]
                   (let[iv (m (type i))]
                     (assoc m (type i) (conj (vec iv) i)))) {} %)))

(defcheck solution-cf1bc3db
  (fn [v] (map #(% 1) (group-by #(type %) v))))

(defcheck solution-cf41a5f1
  #(->> % (group-by type) (map last)))

(defcheck solution-d0d3fd66
  (fn [xs]
    (map (fn [item-type]
           (filter #(= (type %) item-type) xs))
      (set (map #(type %) xs)))))

(defcheck solution-d3f1cc68
  (fn [coll]
    (let [m (group-by type coll)]
      (vals m))))

(defcheck solution-d48c8456
  (fn [l] (vals (group-by type l))))

(defcheck solution-d59e97d4
  (fn [v]
    (loop [mv v
           ret {}]
      (if (empty? mv)
        (vals ret)
        (let [t (first mv)]
          (recur (rest mv) (assoc ret (type t) (conj (get ret (type t) []) t))))))))

(defcheck solution-d5ed77b1
  (fn split-by-type
    ([xs] (split-by-type xs []))
    ([xs acc]
     (if (empty? xs)
       acc
       (let [p #(= (type %) (type (first xs)))]
         (recur
           (filter #(not (p %)) xs)
           (conj acc (filter p xs))))))))

(defcheck solution-d5f819d7
  (fn [s] (->> s (group-by type) vals ) ))

(defcheck solution-d80f435e
  (fn [a]
    (vals (group-by type a))))

(defcheck solution-d88a6cdb
  (fn split [v]
    (let [vfilter (fn vfilter [tst lst] (apply vector (filter tst lst)))]
      (set (filter #(not (empty? %)) (list
                                       (vfilter number? v)
                                       (vfilter keyword? v)
                                       (vfilter string? v)
                                       (vfilter vector? v)))))))

(defcheck solution-dae9d531
  (fn [l]
    (loop [m (hash-map) ll l]
      (if (empty? ll)
        (set (vals m))
        (let [e (first ll) t (type e) v (m t)]
          (recur (assoc m t (if v (conj v e) [e])) (rest ll)))))))

(defcheck solution-db0810c9
  #(let [x (group-by type %)] (set (vals x)) ))

(defcheck solution-db4712d1
  #(into #{}
     (vals (reduce
             (fn [m v] (if (m (type v))
                         (update-in m [(type v)] conj v)
                         (assoc m (type v) [v])))
             {} %))))

(defcheck solution-dd50c7d3
  #(->> %
     (reduce (fn [m e] (assoc m (type e) (conj (get m (type e) []) e))) {})
     (vals)
     (set)))

(defcheck solution-dd798152
  (fn split-type [lst]
    (vals (reduce (fn add-map [m item]
                    (assoc m (type item)
                             (conj (get m (type item) []) item))) {} lst))))

(defcheck solution-de589e4e
  (fn [array]
    (vals (reduce (fn [h v]
                    (update-in h [(type v)] #(conj (vec %) v)))
            {} array))))

(defcheck solution-dedac07a
  #(set (vals
          (reduce
            (fn [m e] (assoc m (type e) (conj (get m (type e) []) e)))
            {}
            %))))

(defcheck solution-dfcafe5f
  (fn [s]
    (->>
      (map hash-map (map type s) (map vector s))
      (apply merge-with concat)
      vals
      set)))

(defcheck solution-e068522e
  (fn [a] (map #(second %) (group-by type a))))

(defcheck solution-e1de9dfd
  (fn [s] (vals (reduce #(update-in %1 [(type %2)] (fnil conj []) %2) {} s))))

(defcheck solution-e1eddc5c
  (fn [col]
    (set
      (vals
        (reduce
          #(assoc %
             (type %2)
             (conj (get % (type %2) []) %2))
          {}
          col)))))

(defcheck solution-e2c17215
  #(vals (reduce (fn [map x]
                   (let [t (type x)]
                     (conj map {t (conj (get map t []) x)})))
           {}
           %)))

(defcheck solution-e315eb25
  (fn [coll]
    (loop [result {} elements coll]
      (if elements
        (recur
          (if (result (type (first elements)))
            (into result {(type (first elements)) (conj (result (type (first elements))) (first elements))})
            (into result {(type (first elements)) [(first elements)]})
            )
          (next elements))
        (vals result)
        )
      )
    ))

(defcheck solution-e4077d48
  (fn __ [col]
    (vals (group-by type col))))

(defcheck solution-e4902b4e
  #(set
     (partition-by (juxt number? keyword? vector? string?)
       (sort-by (juxt number? keyword? vector? string?) %)
       )))

(defcheck solution-e54326dd
  (fn [xs] (set (map #(second %) (group-by type xs)))))

(defcheck solution-e5a3efe7
  (fn splitByType [xs]
    (vals (group-by (fn [elem]
                      (cond
                        (number? elem) :number
                        (symbol? elem) :symbol
                        (keyword? elem) :keyword
                        (string? elem) :string
                        :else :unknown)) xs))))

(defcheck solution-e5e6af54
  #(map reverse
     (vals
       (reduce (fn [m [c item]]
                 (update-in m [c] conj item))
         {}
         (map (fn [item] (vector (type item) item))
           %)))))

(defcheck solution-e61c2dba
  (fn [col] (map #(nth (rest %) 0) (group-by type col))))

(defcheck solution-e643f167
  (fn [coll]
    (vals (reduce (fn [map val]
                    (update-in map [(type val)] #(if (nil? %) (vector val) (conj % val)))) {} coll))))

(defcheck solution-e787d613
  (fn split-by-type [xs]
    (set (map second (group-by type xs)))))

(defcheck solution-e940e628
  #(vals (group-by  type %)))

(defcheck solution-e95a0978
  (fn [c] (set (map second (vec (group-by #(type %) c))))))

(defcheck solution-e985d521
  (fn [x]
    (loop [in x out []]
      (if (empty? in)
        out
        (let [t (type (first in))]
          (recur (filter #(not= t (type %)) in) (conj out (filter #(= t (type %)) in))))))))

(defcheck solution-e9f4044d
  (fn split-by-type [xs]
    (loop [xs xs accum {}]
      (if (empty? xs) (vals accum)
                      (let [x (first xs)
                            t (type x)]
                        (recur (rest xs) (assoc accum t (conj (get accum t []) x))))))))

(defcheck solution-ea9e0ded
  (fn [icol]
    (loop  [col     icol
            acc     {}]
      (if (empty? col) (set (map second acc))
                       (let [curr  (first col)
                             rc    (rest col)
                             tc    (type curr)
                             av    (conj (get acc tc []) curr)
                             ]
                         (recur (rest col) (assoc acc tc av)))))))

(defcheck solution-eab5e56d
  (fn [v]
    (loop [r {}
           v v]
      (if (empty? v)
        (map val r)
        (let [ele (first v)
              ele-type (type ele)]
          (if (get r ele-type)
            (recur (update-in r [ele-type] conj ele) (rest v))
            (recur (assoc r ele-type [ele]) (rest v))))))))

(defcheck solution-eb449f16
  (fn [coll] (let [cn (atom []) ck (atom []) cs (atom []) cc (atom [])]
               (doseq [v coll]
                 (cond
                   (keyword? v) (swap! ck conj v)
                   (string? v) (swap! cs conj v)
                   (number? v) (swap! cn conj v)
                   (coll? v) (swap! cc conj v)
                   ))
               (filter #(not (empty? %)) [@cn @ck @cs @cc])
               )))

(defcheck solution-eb6d0fe5
  (fn f[l]
    (map
      (fn [t]
        (vec
          (filter
            (fn [el]
              (= t (type el)))
            l)))
      (set (map type l)))))

(defcheck solution-ebc4257
  (fn [col]
    (vals (group-by type col))))

(defcheck solution-ec6bbbc4
  (fn [ls]
    (set (vals (group-by #(type %) ls)))))

(defcheck solution-ec6f0bba
  (fn [col]
    (vals
      (reduce
        (fn [prev item]
          (conj prev
            (let [itemtype (type item), itemlist (prev itemtype)]
              (vector itemtype
                (if
                 (nil? itemlist)
                  (vector item)
                  (conj itemlist item)
                  )
                )
              )
            )
          ) {} col))))

(defcheck solution-ecbfdc76
  (comp set
        (partial filter (comp not empty?))
        (apply juxt (map (fn [pred] (fn [seq] (filter pred seq)))
                      [number? keyword? string? symbol? vector?]))))

(defcheck solution-ed6e84f6
  (fn [seq]
    (map last (group-by type seq))))

(defcheck solution-edafc2e5
  (fn homogenize [coll]
    (loop [result-set {}
           remaining coll]
      (let [v (first remaining)]
        (cond
          (empty? remaining) (vals result-set)
          (not (contains? result-set (type v)))
          (recur  (assoc result-set (type v) [v])
            (rest remaining))
          :else (recur (assoc result-set (type v) (conj (get result-set (type v)) v))
                  (rest remaining)))))))

(defcheck solution-edcc7a42
  (fn [col]
    (map second (group-by type col))
    ))

(defcheck solution-eed691e2
  (fn split-type [col]
    (map second (reduce (fn [m x] (assoc m
                                    (type x)
                                    (conj (get m (type x) []) x)))
                  {}  col))))

(defcheck solution-ef7371b9
  (fn [coll] (set (map (fn [c] (filter #(= (type %) c) coll)) (set (map type coll))))))

(defcheck solution-ef7597b3
  (fn [x] (set (partition-by type (sort-by #(str (type %)) x)))))

(defcheck solution-ef9698fa
  (fn split-by-type [input]
    (filter #(not (empty? %))
      (list (vec (filter vector? input))
        (filter integer? input)
        (filter string? input)
        (filter keyword? input)))))

(defcheck solution-f0df1058
  (fn
    [coll]
    (map second (reduce (fn [m v] (update-in m [(type v)] #(conj (or % []) v)))
                  {}
                  coll))))

(defcheck solution-f1027da1
  (fn [lst]
    (let [types (into #{} (map type lst))]
      (map (fn [t]
             (filter #(= t (type %)) lst))
        types))))

(defcheck solution-f3387ca5
  (fn [c]
    (filter #(not (empty? %))
      (map vec
        (list
          (filter number? c)
          (filter keyword? c)
          (filter string? c)
          (filter vector? c))))))

(defcheck solution-f38238ae
  #(vals (group-by type %)))

(defcheck solution-f4b13090
  (fn split-by-type [xs]
    (let [types (distinct (map type xs))]
      (map
        (fn get-elements-of-type [t] (filter #(= (type %) t) xs))
        types))))

(defcheck solution-f4b22c1c
  (fn z [coll]
    (->> (group-by type coll)
      (map second))))

(defcheck solution-f5342eb9
  (fn split-by-type [coll]
    (vals (group-by type coll))))

(defcheck solution-f60485ee
  (fn typeSplit [coll]
    (if-let [t (type (first coll))]
      (concat (vector (filter #(= t (type %1)) coll))
              (typeSplit (filter #(not= (type %1) t) coll))))))

(defcheck solution-f613e6bb
  (fn [coll] (partition-by #(str (type %)) (sort-by #(str (type %)) coll))))

(defcheck solution-f69d8c2b
  (fn [col]
    (letfn [(tpmapfn [c]
              (loop [i 0 v []]
                (if (< i (count c))
                  (recur (inc i) (conj v {(type (c i)) (c i)}))
                  v)))]
      (let [tpmap (tpmapfn col)
            gpset (into #{} (flatten (map keys tpmap)))
            gpval (for [i gpset] (map #(% i) tpmap))]
        (map (partial filter #(not= nil %)) gpval)))))

(defcheck solution-f7309daf
  (fn split-type
    [s]
    (vals (group-by type s))))

(defcheck solution-f7563e4d
  #(into #{} (map second (group-by type %))))

(defcheck solution-f887a656
  (fn [s]
    (vals (group-by type s))))

(defcheck solution-f9a014c9
  (fn [v] (vals (reduce #(merge-with concat % %2)
                  (map (fn [x] {(type x) [x]}) v)))))

(defcheck solution-fa0bf836
  (fn type-split [x]
    (vals (group-by type x))))

(defcheck solution-fb2a572c
  (fn [x]
    (set (map val (group-by type x)))))

(defcheck solution-fba32135
  (fn split-by-type [coll]
    (into #{}
      (for [[k v]
            (group-by type coll)]
        v))))

(defcheck solution-fbdd9d0d
  (fn sigh [s]
    (loop [s s resmap (array-map)]
      (if (not (seq s))
        (map reverse (vals resmap))
        (recur (rest s) (assoc-in resmap
                          [(type (first s))]
                          (conj (resmap (type (first s))) (first s))))))))

(defcheck solution-fcc08560
  (fn [xs]
    (loop [ys xs mp {}]
      (if (empty? ys)
        (vals mp)
        (let [[y & ys'] ys
              clazz (type y)
              zs (find mp clazz)
              zs' (if (nil? zs)
                    [y]
                    (conj (into [] (second zs)) y))
              mp' (dissoc mp clazz)
              mp'' (assoc mp' clazz zs')]
          (recur ys' mp''))))
    ))

(defcheck solution-fcca915
  (fn [s]
    (let [types (set (map type s))]
      (map (fn [typ] (filter #(= (type %) typ) s))
        types))))

(defcheck solution-fd1c19ea
  (fn [coll]
    (->> coll
      (group-by type)
      (map (fn [[k g]] g))
      (into #{}))))

(defcheck solution-fd392ef2
  #(into [] (map second (group-by type %))))

(defcheck solution-fdafee58
  (fn [s]
    (vals (reduce
            #(merge-with concat %1 {(type %2) [%2]})
            {} s))))

(defcheck solution-fdd415f1
  (fn [coll] (set
               (for [i (set (map type coll))]
                 (filter #(= i (type %)) coll)))))

(defcheck solution-fefae074
  (fn [coll]
    (vals (group-by type coll))))

(defcheck solution-ff9cfbc6
  (fn [sq] (vals (group-by type sq))))