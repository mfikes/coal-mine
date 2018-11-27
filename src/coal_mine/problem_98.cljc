(ns coal-mine.problem-98
  (:require [coal-mine.checks :refer [defcheck-98] :rename {defcheck-98 defcheck}]
            [clojure.test]
            [clojure.walk]
            [clojure.set]))

(defcheck solution-102a8de6
  (fn r[f d]
    (set
      (map set
        (vals (group-by f d))))))

(defcheck solution-10509653
  (fn equivalence-classes
    [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-10e28c6a
  (fn __ [f D]
    (->> (group-by f D)
      (vals)
      (map set)
      (set))))

(defcheck solution-1114ba12
  (fn equivalence-classes [function domain]
    (loop [result {}
           queue (into () domain)]
      (if (empty? queue)
        (->> result vals set)
        (recur
          (merge-with clojure.set/union result {(function (first queue)) #{(first queue)}})
          (rest queue))))))

(defcheck solution-114e2d22
  (fn [f xs]
    (set (map #(->> % second set) (group-by f xs)))))

(defcheck solution-11f73398
  (fn [f s]
    (set (map set (vals (group-by #(f %) s))))
    ))

(defcheck solution-12a9d538
  (fn [f c]
    (->> (group-by f c)
      vals
      (map set)
      set)))

(defcheck solution-13ea4c49
  (fn [f s]
    (reduce (fn [sets x]
              (let [bucket
                    (first (filter (fn [b]
                                     (= (f x) (f (first b)))) sets))]
                (if (nil? bucket)
                  (conj sets #{x})
                  (conj (disj sets bucket)
                    (conj bucket x))))) #{} s)))

(defcheck solution-141244b3
  (fn equivalence-relation [f d]
    (->> (group-by f d)
      (reduce (fn [a [_ x]] (conj a (set x))) #{}))))

(defcheck solution-145138c7
  (fn [f xs]
    (set (map #(set (second % )) (group-by f xs)))))

(defcheck solution-14699254
  (fn [f a] (set(map #(into #{} (val %)) (group-by f a)))))

(defcheck solution-146d7951
  (fn [f s]
    (set (map #(set (val %)) (group-by f s)))))

(defcheck solution-1515076f
  #(->> (group-by % %2) vals (map set) set))

(defcheck solution-15284d3e
  (fn [f data]
    (->> data
      (group-by f)
      vals
      (map set)
      set
      )))

(defcheck solution-1625f932
  (fn equiv
    [f s]
    (let [pairs (for [x s
                      y s
                      :when (not= x y)]
                  [x y])
          eq (into #{}
               (map set
                 (filter (fn [[x y]] (= (f x) (f y)))
                   pairs)))
          other (reduce (fn [r e]
                          (conj r #{e}))
                  #{} (clojure.set/difference s (into #{} (mapcat identity eq))))]
      (if (= (* 2 (count s)) (count eq))
        #{s}
        (clojure.set/union eq other)))))

(defcheck solution-164a11ee
  #(set (map (comp set second) (group-by % %2))))

(defcheck solution-16a36ca5
  #(set (map set (vals (group-by %1 %2)))))

(defcheck solution-16f02f12
  (fn eq [f D]
    (set
      (map #(set (map last %))
        (vals
          (group-by first
            (map #(vector (f %) %) D )))))))

(defcheck solution-1806711c
  (fn equiv-classes [f D]
    (set (map set (vals (reduce #(merge-with concat %1 %2) (map (fn [x] (hash-map (f x) [x])) D)))))))

(defcheck solution-1898fff1
  (fn [f s]
    (set (map set (vals (group-by f s))))
    ))

(defcheck solution-191bfc94
  (fn find-equivalent-classes
    [func xset]
    (letfn [(idempotent? [f x] (= (f x) (f (f x))))]
      (->> xset
        (group-by func)
        (filter (fn [map-entry]
                  (let [k (first map-entry)
                        v (second map-entry)]
                    (or (< 1 (count v))
                        (idempotent? func (first v))
                        (idempotent? func (last v))))))
        (map (comp set second))
        set))))

(defcheck solution-196207a2
  (fn equivalence-classes
    [f d]
    (let [eq-map (reduce (fn [accum h] (if (contains? accum (f h))
                                         (assoc accum (f h) (conj (accum (f h)) h))
                                         (assoc accum (f h) #{ h })))
                   {} d)]
      (reduce (fn [accum h] (conj accum (second h))) #{} eq-map))))

(defcheck solution-198eeaa4
  (fn [f D] (set (map set (partition-by f (sort-by f D))))))

(defcheck solution-1aa1dd8d
  (fn [f c] (set ((comp (partial map set) vals) (group-by f c)))))

(defcheck solution-1ab389f5
  (fn equal
    [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-1b6b637d
  (fn [f xs] (set (map set (vals (group-by f xs))))))

(defcheck solution-1bccdda
  #(into #{} (for [v (vals (group-by % %2))] (into #{} v))))

(defcheck solution-1c4917d1
  (fn [f s]
    (->> (group-by #(f %) s)
      vals
      (map set)
      set)))

(defcheck solution-1d0b9935
  (fn eq [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-1d601195
  (fn equiv-class [f xs]
    (let [ks (group-by f xs)]
      (set (map set (vals ks))))))

(defcheck solution-1d6a6b93
  (fn [f D]
    (set (vals (reduce (fn [memo x]
                         (let [v (f x)]
                           (if (memo v)
                             (merge-with conj memo {v x})
                             (assoc memo v #{x})
                             )
                           )
                         ) {} D)))

    ))

(defcheck solution-1db615d8
  (fn [f domain]
    (let [by-mapping
          (reduce (fn [acc x]
                    (update-in acc [(f x)] #((fnil conj #{}) % x)))
            {} domain)]
      (set (vals by-mapping)))))

(defcheck solution-1efafb92
  (fn [f coll]
    (->> (group-by f coll)
      vals
      (reduce #(conj % (set %2)) #{}))))

(defcheck solution-1f5a178e
  (fn __ [f d]
    (into #{}
      (for [[k v] (group-by f d)]
        (into #{} v)))))

(defcheck solution-1fb11aa2
  (fn [f D] (into #{} (map (partial into #{})
                        (vals (reduce #(merge-with concat % %2)
                                (map #(hash-map (f %) [%]) D)))
                        ))))

(defcheck solution-1fb60de2
  (fn [ f s]
    (set (map  #(set (map first %)) (map last (group-by last (map #( vector % (f %)) s)))))))

(defcheck solution-1ff6a560
  (fn ! [f s]
    (let [vs (map f s)
          dvs (distinct vs)
          re (set (map #(set (filter (fn [el] (= (f el) %)) s)) dvs))]
      re)))

(defcheck solution-200369d0
  (fn [f d]
    (->> (group-by f d)
      vals
      (map set)
      set)))

(defcheck solution-2010e3a3
  (fn equivalence-class [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-205a596
  (fn [f set]
    (into #{} (vals (loop [map nil coll set]
                      (if (empty? coll)
                        map
                        (recur
                          (assoc map (f (first coll))
                                     (if (contains? map (f (first coll)))
                                       (conj (get map (f (first coll))) (first coll))
                                       (conj #{} (first coll))
                                       )
                                     )
                          (rest coll))
                        )
                      )))
    ))

(defcheck solution-21b166b6
  (fn equiv
    [f a-set]
    (let [a-map (group-by f a-set)]
      (set (map #(set %) (vals a-map))))))

(defcheck solution-21e02ffa
  #(set (map set (vals (group-by % %2)))))

(defcheck solution-224a4ff2
  (fn eqv-classes [f D]
    (set
      (vals
        (reduce
          #(if (%1 (val %2))
             (assoc %1 (val %2) (conj (%1 (val %2)) (key %2)))
             (assoc %1 (val %2) #{(key %2)})) {}
          (apply hash-map (interleave D (map f D))))))))

(defcheck solution-23275380
  (fn [f v](set (map  set (vals(group-by  f v))))))

(defcheck solution-23297b39
  (fn [f D]
    (set (map set(vals (group-by f D))))))

(defcheck solution-237d992b
  (fn [f D]
    (set (map (comp set val) (group-by f D)))))

(defcheck solution-23a9f847
  (fn [f d]
    (set
      (vals
        (reduce
          (fn [m p]
            (let [k (f p)]
              (assoc m k (conj (get m k #{}) p))))
          {}
          d)))))

(defcheck solution-245d4a81
  (fn [f D]
    (set
      (vals
        (reduce
          (fn [m x]
            (let [r (f x)
                  rs (get m r #{})]
              (assoc m r (conj rs x))))
          {}
          D)))))

(defcheck solution-24e3a096
  (fn equiv-classes [f xs]
    (->> (vals (group-by f xs))
      (map set)
      (into #{}))))

(defcheck solution-24e4e6d4
  (fn equivalance-classes [f domain]
    (letfn [(valencer [element]
              (set (filter (comp (partial = (f element)) f) domain)))]
      (->> domain (map valencer) set))))

(defcheck solution-2516811b
  (fn ec
    [f s]
    (let [vs (vec s)
          gs (group-by f vs)]
      (set (map #(set (second %)) gs)))))

(defcheck solution-257d8064
  (fn [op s]
    (set
      (map
        set
        (vals
          (group-by op s))))))

(defcheck solution-25ebca2b
  #(set (->> (group-by % %2) vals (map set))))

(defcheck solution-25f03d62
  (fn [f els]
    (->> els
      (group-by f)
      vals
      (map set)
      (into #{}))))

(defcheck solution-2606699f
  (fn equivalence-classes [f D]
    (->> D
      (map #(vector (f %) %))
      (group-by first)
      vals
      (map (fn [group] (map second group)))
      (map set)
      set)))

(defcheck solution-265d744e
  (fn [x y] (set (map #( set (second %)) (group-by x  y)))))

(defcheck solution-26659789
  (fn [f set] (apply hash-set (map #(apply hash-set %) (vals (group-by f set))))))

(defcheck solution-26b97cfc
  (fn equiv-classes [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-2718a578
  (fn [f d]
    (->> (group-by f d)
      (vals)
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-271d3bc5
  (fn [f d]
    (->> (group-by f d) (vals) (map set) (set))))

(defcheck solution-279180fc
  (fn [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-27af49af
  #(set(map set(vals(group-by %1 %2)))))

(defcheck solution-27b58fae
  (fn [f d]
    (set
      (map #(set (map first %))
        (vals
          (group-by second
            (map #(list % (f %)) d)))))))

(defcheck solution-299e2e23
  (fn [f s]
    (->> (group-by f s)
      vals
      (map set)
      set)))

(defcheck solution-2a8a7543
  (fn [f D] (->> (partition 2 (interleave (map f D) D))
              (group-by #(first %))
              (vals)
              (map (fn [x](into #{} (map #(second %) x))))
              (into #{}))))

(defcheck solution-2b36b2db
  (fn [f cl] (->> (group-by f cl) vals (map set) set
               )))

(defcheck solution-2bdd95c1
  #(into #{} (map (partial into #{}) (vals (group-by %1 %2)))))

(defcheck solution-2c963fb9
  (fn [ f d ]
    (set (map set (vals (group-by f d))))))

(defcheck solution-2c98411e
  (fn [f s]
    (->> (group-by f s)
      vals
      (map set)
      set)))

(defcheck solution-2ca1bbb4
  (fn equiv [f D]
    (set (map (comp set second) (group-by f D)))))

(defcheck solution-2cddc59
  #(into #{}
     (map set
       (vals (group-by % %2)))))

(defcheck solution-2d7942e7
  (fn [f s] (set (map set (for [v (vals (group-by second (map #(list % (f %)) s)))] (map first v))))))

(defcheck solution-2dc885f2
  (fn  equiv [f s]
    (into
      #{}
      (map
        #(set (% 1))
        (group-by f s)))))

(defcheck solution-2dff5f77
  (fn [f d] (into #{} (map #(into #{} %) (vals (group-by f d))))))

(defcheck solution-2e43c250
  #(into #{} (map (comp (partial apply hash-set) second) (group-by %1 %2))))

(defcheck solution-2e72011c
  (fn [f D]
    (->> (group-by f D)
      (vals)
      (map set)
      (set))))

(defcheck solution-2ec2f6bd
  (fn [f D]
    (->> (group-by f D)
      (vals)
      (map set)
      (set)
      )))

(defcheck solution-2f1701b4
  (fn eq [f D]
    (set (map (comp set second) (group-by #(f %) D)))
    ))

(defcheck solution-2f9d15de
  (fn equivalence-classes
    [f d]
    ((comp set (partial map set) vals) (group-by f d))))

(defcheck solution-2fbd08b6
  (fn eqiv [f d]
    (let [answers (map (fn [v] [v (f v)]) d)]
      (set
        (map set (map (fn [f] (map first f))
                   (map second (group-by second answers))))))))

(defcheck solution-30515ae
  (fn [f coll]
    (->> coll
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-30cde9b3
  (fn prob-0098
    [f D]
    (let [fD      (map #(f %) D)
          map-fun (fn [in-fd in-d] {in-fd (set [in-d])})
          maps    (map map-fun fD D)]
      (set (vals (apply merge-with clojure.set/union maps))))))

(defcheck solution-31021b88
  (fn equiv-relation [f st]
    (set (map set (vals (group-by f st))))))

(defcheck solution-31dd44f2
  (fn [f c]
    (set (map set (vals
                    (group-by f c))))))

(defcheck solution-3215819
  (fn [f D]
    (->> D (group-by f) (vals) (map set) (set))))

(defcheck solution-326f00e9
  (fn [f s]
    (into #{} (map set (vals (group-by f s))))))

(defcheck solution-32bec415
  (fn equiv [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-338413f7
  #(into #{} (map set(vals (group-by %1 %2)))))

(defcheck solution-33cd28df
  #(into #{} (map (fn[x](into #{} x))(vals (group-by % %2)))))

(defcheck solution-3411fadb
  (fn [f s]
    (into #{}
      (for [e s]
        (let [fe (f e)]
          (into #{}
            (filter #(= fe (f %)) s)))))))

(defcheck solution-3469f51d
  (fn eq-cls [f col]
    (let [clses (group-by f col)
          values (vals clses)
          values (map set values)]
      (set values))))

(defcheck solution-34b0d160
  (fn equiv [f D]
    (set
      (for [x D]
        (reduce (fn [acc y]  (if (= (f x) (f y)) (conj acc y) acc))
          #{} D)))))

(defcheck solution-3553359c
  (fn equiv-class [f coll]
    (->> (group-by f coll)
      (vals)
      (map set)
      (set))))

(defcheck solution-3747979b
  #(->> %2
     (sort-by %)
     (partition-by %)
     (map set)
     set))

(defcheck solution-374b12ba
  (fn [f s1]
    (->> (group-by f s1) vals (map set) set)))

(defcheck solution-37809dc1
  #(into #{} (vals (reduce (fn [a b]
                             (let [fb (%1 b)]
                               (assoc a fb (conj (a fb #{}) b))))


                     {} %2))))

(defcheck solution-37a6a967
  (fn __ [f s]
    (->> s
      (map #(vec [% (f %)]))
      (group-by second)
      (vals)
      (map #(map first %))
      (map set)
      (set))))

(defcheck solution-38248256
  (fn [f D]
    (let [values (distinct (map f D))]
      (set
        (map (fn [v]
               (set
                 (filter #(= (f %) v)
                   D)))
          values)))))

(defcheck solution-3850a516
  (fn [f s] (set (map set (vals (group-by f s))))))

(defcheck solution-38529353
  (fn [f x]   (set (map set (vals (group-by f (into [] x)))))))

(defcheck solution-387578f0
  (fn [f s] (set (map set (vals (group-by f s) )))))

(defcheck solution-393d01f0
  (fn [f s] (set (map #(set (second %))(group-by f s)))))

(defcheck solution-39882f5c
  (fn [f d]
    (->>
      (sort-by f d)
      (partition-by f)
      (map set)
      (set))))

(defcheck solution-39a4f78c
  (fn [f s]
    (apply conj #{}
      (map (comp set val)
        (group-by f s)))))

(defcheck solution-39c1fa5a
  (fn [f seq] (set (map set (vals (group-by f seq))))))

(defcheck solution-3a2c8030
  (fn [f xs]
    (into #{}
      (vals (reduce (fn [acc v] (assoc acc (f v) (conj (get acc (f v) #{}) v)))
              {}
              xs)))))

(defcheck solution-3b3685a2
  #(->> %2
     (map (fn [y] {(%1 y) #{y}}))
     (reduce (comp (partial merge-with clojure.set/union)))
     (vals)
     ;;(reduce clojure.set/union)
     (into #{})
     ))

(defcheck solution-3e6c2bb9
  (fn
    [f D]
    (reduce (fn [acc [key val]]
              (conj acc val))
      #{}
      (reduce #(assoc %1 (f %2) (conj (or (%1 (f %2)) #{}) %2))
        {}
        D))))

(defcheck solution-3e6f9cc7
  (fn ms [f s]
    (set (map set
           (vals (group-by f s))))))

(defcheck solution-3f0ac48f
  (fn [func elems]
    (into #{} (vals (reduce
                      (fn [acc elem]
                        (update-in acc [(func elem)]
                          #(if % (conj % elem) #{elem})
                          )
                        )  {} elems
                      )
                )
      )
    ))

(defcheck solution-3f3a21fe
  (fn [f D]
    (set (map #(into #{} %) (vals (group-by f D))))
    ))

(defcheck solution-3fb789dd
  #(into #{} (map set (vals (group-by % %2)))))

(defcheck solution-401364d8
  (fn [f D]
    (set
      (vals
        (reduce (fn [acc x]
                  (let [val (f x)
                        subD (get acc val #{})]
                    (assoc acc val (conj subD x))))
          {}
          D)))))

(defcheck solution-4094a28
  #(set (map set (map second (group-by % %2)))))

(defcheck solution-41c6e0af
  (fn eqclasses [f s] (set (vals (reduce #(let [k (f %2)] (assoc % k (conj (get % k #{}) %2))) {} s)))))

(defcheck solution-43b354e
  (fn equivalence-classes [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-444b8057
  (fn [f D]
    (set
      (map
        (comp set second)
        (group-by f D)))))

(defcheck solution-448eac1f
  (fn [f d]
    (->> (group-by f d)
      vals
      (map set)
      set)))

(defcheck solution-451a6a39
  (fn [f D] (set (map (comp set last) (group-by f D)))))

(defcheck solution-4557aeac
  (fn [f d]
    (set (map set (vals (apply merge-with concat (map #(assoc {} (f %) [%]) d)))))))

(defcheck solution-45aeafaf
  (fn [f s]
    (set (map #(set (second %)) (group-by f s)))))

(defcheck solution-4662ea08
  (comp set (partial map (comp set second)) group-by))

(defcheck solution-46735c77
  (fn eqv [f D]
    (set (->> D
           (group-by f)
           (map (comp set second))))))

(defcheck solution-46aa9beb
  (fn [f seq]
    (set (map set (vals (reduce #(merge %1 {(f %2) (conj (%1 (f %2)) %2)} ) {} seq))))
    ))

(defcheck solution-46bf8cd9
  (fn [f s]
    (->> (group-by f s)
      (vals)
      (map set)
      (set))))

(defcheck solution-46e0dc41
  (fn eq-classes
    [f e]
    (set (map set (vals (group-by f e))))))

(defcheck solution-4805e91
  (fn [f s]
    (set (map #(-> % val set) (group-by f s)))))

(defcheck solution-49a18b8d
  (fn eclasses [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-49acbaea
  (fn [f v] (set (map #(set (map first %)) (vals (group-by second (map #(vector % (f %) ) v)))))))

(defcheck solution-4a0b776b
  (fn [f col]
    (set (map set (vals (group-by f col))))))

(defcheck solution-4a3b8d15
  (fn [f src]
    (set(map set (partition-by f (sort-by f src))))
    ))

(defcheck solution-4ad65fea
  (fn [f start]
    (set (map (comp set val) (group-by f start)))))

(defcheck solution-4ae72ed5
  (fn [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-4ae75165
  (fn [f D] (loop [d D result {}]
              (if (nil? d) (set (vals result))
                           (if (nil? (result (f (first d))))
                             (recur (next d) (assoc result (f (first d)) #{(first d)}))
                             (recur (next d) (assoc result (f (first d)) (conj (result (f (first d))) (first d))))
                             )
                           )
              )
    ))

(defcheck solution-4b09d39c
  (fn [f D]
    (set (vals (reduce (fn [m e]
                         (let [k (f e)
                               v (m k)]
                           (if v
                             (conj m [k (conj v e)])
                             (conj m [k #{e}]))))
                 {} D)))))

(defcheck solution-4b112fb6
  (fn eq-classes [f D] (set (map set (vals (group-by f D))))))

(defcheck solution-4b5cc328
  (fn [f D]
    (set (vals (apply merge-with (comp set concat) (map (fn [a b]{a #{b}}) (map f D) D))))))

(defcheck solution-4bd0af88
  (fn [x y]
    (into #{} (map (comp #(into #{} %) second) (group-by x y)))))

(defcheck solution-4c32f301
  (fn [f s]
    (set
      (for [[_ v] (group-by f s)]
        (set v)))))

(defcheck solution-4c7a04de
  #(->>
     %2
     (group-by %)
     (map second)
     (map set)
     set))

(defcheck solution-4cc8f36b
  (fn equivalence-classes [f domain] (set (map set (vals (group-by f domain))))))

(defcheck solution-4cd4f3a3
  (fn my-equiv[m n]
    (into #{}
      (map #(set %)
        (vals (group-by m n))))))

(defcheck solution-4cdcf668
  (fn eqclz [f d]
    (set (vals (reduce (fn [ret this]
                         (let [v (f this)]
                           (update-in ret [v] (fnil conj #{}) this)))
                 {} d)))))

(defcheck solution-4cf175db
  (fn [f d]
    (->> (group-by f d)
      (vals)
      (map set)
      set)))

(defcheck solution-4cf2b39b
  (comp set (partial map set) vals group-by))

(defcheck solution-4cfbbe1f
  (fn eq-rel [function domain]
    (set (vals (reduce (fn [res input]
                         (let [result (function input)
                               current-entry (get res result #{})]
                           (assoc res result (conj current-entry input)))) {} domain)))))

(defcheck solution-4d01f552
  (fn [f d]
    (set (vals (reduce (fn [r n]
                         (let [k (f n)]
                           (assoc r k (if (contains? r k)
                                        (conj (r k) n)
                                        #{n}))))
                 {} d)))))

(defcheck solution-4d23b4be
  (fn solve [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-4d285b8e
  (fn [f ns]
    (->> (group-by f ns) (vals) (map set) (into #{}))))

(defcheck solution-4d43927a
  (fn e [f s]  (set (vals (apply merge-with #(set (concat %1 %2)) (map #(hash-map (f %) #{%}) s))))))

(defcheck solution-4da90620
  (fn [f D] (->> (group-by f D) vals (map set) set)))

(defcheck solution-4e3d27fd
  (fn [f coll]
    (let [fcoll (map f coll)
          fcoll-and-coll (map #(vector %1 %2) fcoll coll)]
      (reduce conj #{} (map set (map #(map last %) (vals (group-by first fcoll-and-coll))))))))

(defcheck solution-4e56cac4
  (fn eq-classes [pred domain]
    (loop [classes #{} d domain]
      (let [class (filter #(= (pred %) (pred (first d))) d)
            next-classes (conj classes (into #{} class))
            rest-domain (apply disj d class)]
        (if (empty? rest-domain) next-classes
                                 (recur next-classes rest-domain))))))

(defcheck solution-4fa25111
  (fn equiv-class [f domain]
    (->> (group-by f domain)
      (map (comp set second))
      set)))

(defcheck solution-4fabc8f3
  (fn [f s]
    (set (map #(set (second %)) (group-by f s)))
    ))

(defcheck solution-50955c04
  (fn [f aset] (set (map #(set (second %)) (group-by f aset)))))

(defcheck solution-50f942e5
  (fn eqclasses [f x]
    (set (map set (vals (group-by f x))))))

(defcheck solution-51ba05b3
  (fn myeqv [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-51c58e5e
  (fn [f D] (set(map (comp set second) (group-by f D)))))

(defcheck solution-5202a181
  (fn [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-536845e1
  (fn equiv [func coll]
    (->> coll
      (map #(vector % (func %)))
      (sort-by last)
      (partition-by last)
      (map (fn [items] (map first items)))
      (map set)
      (set))))

(defcheck solution-538ef26b
  (fn [func the-set]
    (->> (sort-by func the-set)
      (partition-by func)
      (map set)
      (into #{}))))

(defcheck solution-54c86d9c
  (fn q4q098
    [f D]
    (if (empty? D)
      nil
      (let [equiv (set (filter #(= (f (first D)) (f %)) D))]
        (into #{equiv}
          (q4q098 f (clojure.set/difference D equiv)))))))

(defcheck solution-54f245ba
  (fn [f d]
    (->> (group-by f d)
      (vals)
      (map set)
      (set))))

(defcheck solution-5527719e
  #(set (map set (partition-by % (sort-by % %2)))))

(defcheck solution-556ddc9a
  (fn [f d]
    (->> d
      (group-by f)
      vals
      (map set)
      (into #{}))))

(defcheck solution-55a307e8
  (fn [f D]
    (->> D
      (map (fn [d]
             (->> D
               (filter (fn [y] (= (f d) (f y))))
               (into #{d}))))
      (into #{}))))

(defcheck solution-55b2f3ef
  (fn[f coll]
    (loop [[eye & more] (seq coll)
           map {}]
      (if (nil? eye)
        (into #{} (vals map))
        (let [class (f eye)]
          (recur more (assoc map class
                                 (conj (get map class #{}) eye))))))))

(defcheck solution-56ceade6
  (fn ec [f d]
    (set (vals (reduce #(merge-with clojure.set/union %1 {(f %2) #{%2}}) {} d)))
    ))

(defcheck solution-5792132d
  (fn [f D]
    (->> (group-by f D)
      (map (comp set second))
      (set))))

(defcheck solution-57b649d
  (fn equivalence-classes [f domain]
    (into #{} (map #(into #{} %) (vals (group-by f domain))))))

(defcheck solution-57fcd6e4
  (fn [f s]
    (set (for [a s]
           (set (for [b s :when (= (f a) (f b))] b))))))

(defcheck solution-58303799
  (fn [f sq]
    (letfn [(flt [n]
              (filter #(= (f n) (f %)) sq))]
      (into #{} (map set (map #(flt %) sq))))))

(defcheck solution-58a35ea4
  (fn [f domain] (set (vals (persistent! (reduce (fn [r n] (assoc! r (f n) (conj (get r (f n) #{}) n))) (transient {}) domain))))))

(defcheck solution-58dae5af
  (fn eqclasses [f d]
    (set (map #(into #{} (second %)) (group-by f d)))))

(defcheck solution-58f70914
  (fn [f s]
    (->> (group-by f s)
      vals
      (map set)
      set)))

(defcheck solution-59877cee
  (fn [f s]
    (->> s
      (group-by f)
      vals
      (map set)
      set)))

(defcheck solution-59c91d1c
  (fn[f s]
    (let [values (set (map f s))]
      (set (for [v values] (set (filter #(= v (f %)) s)))))))

(defcheck solution-59d622ba
  (fn [f d] (set (map #(set (last %)) (group-by f d)))))

(defcheck solution-5a8189b0
  (fn p [f col]
    (apply hash-set (map #(apply hash-set (second %)) (group-by f col)))))

(defcheck solution-5b40f608
  (fn eqc [f c] (set (map set (vals (group-by f c))))))

(defcheck solution-5b6cad9c
  (fn equivalence-classes [f s] (set (map set (vals (group-by f s))))))

(defcheck solution-5bafa92e
  (fn equivalence-classes [f d]
    (-> (reduce (fn [acc i]
                  (let [k (f i)]
                    (assoc acc k (conj (get acc k #{}) i))))
          {} d)
      vals
      set)))

(defcheck solution-5c01c847
  (fn [f s]
    (set
      (vals
        (reduce
          (fn [relation val]
            (let [equiv (f val)]
              (if (contains? relation equiv)
                (assoc relation equiv (conj (get relation equiv) val))
                (assoc relation equiv (conj #{} val)))))
          {}
          s)))))

(defcheck solution-5c5d36b7
  (fn [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-5c6592b3
  (fn [f d] (->> d (group-by f) vals (map set) set)))

(defcheck solution-5c9ff073
  (fn [f xs]
    (->> xs
      (group-by f)
      vals
      (map set)
      set)))

(defcheck solution-5ceae855
  #(->>
     (group-by %1 %2)
     (vals)
     (map set)
     (set)))

(defcheck solution-5d04f92e
  (fn [f xs]
    (->> (group-by f (for [x xs] x))
      (vals)
      (map set)
      (into #{}))))

(defcheck solution-5d0cd035
  (fn e [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-5d2c6736
  #(into #{} (map set (vals (group-by %1 %2)))))

(defcheck solution-5d560198
  #(set (map (comp set second)
          (group-by %1 %2))))

(defcheck solution-5dec7c87
  (fn [f xs]
    (into #{} (map #(into #{} %) (vals (group-by f xs))))))

(defcheck solution-5e97f2ab
  (fn equiv-classes
    [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-5ee6d152
  (fn number98 [f D]
    (->> (reduce #(merge-with clojure.set/union % {(f %2) #{%2}}) {} D)
      (vals)
      (into #{}))))

(defcheck solution-5eece299
  (fn [f d]
    (->> d (group-by f) (vals) (map set) set)))

(defcheck solution-5fff7653
  (fn [f d] (into #{} (map #(set (val %)) (group-by f d)))))

(defcheck solution-6036630a
  (fn eqv-class [f D] (->> (group-by f D) vals (map set) set ) ))

(defcheck solution-6176da94
  (fn eqclass [f s]
    (into
      #{}
      (vals (reduce (fn [m e]
                      (if-let [cls (m (f e))]
                        (assoc m (f e) (conj cls e))
                        (assoc m (f e) #{e})))
              {} s)))))

(defcheck solution-6231648f
  (fn [f d]
    (set (map #(reduce (fn [acc e]
                         (if (= (f %) (f e))
                           (conj acc e)
                           acc))
                 #{}
                 d)
           d))))

(defcheck solution-623dcb89
  #(->> %2
     (group-by %)
     (map (comp set second))
     set))

(defcheck solution-62422d12
  (fn eqClass [f xs]
    (into #{} (map #(into #{} (second %)) (group-by f xs)))
    ))

(defcheck solution-631fe347
  (fn [f s]
    (reduce #(conj %1 (set (second %2))) #{} (group-by f s))))

(defcheck solution-63388222
  (fn
    [f domain]
    (set (map set (vals (group-by f domain))))))

(defcheck solution-63b7c0bc
  (fn [f d] (set (for [i (vals (group-by f d))] (set i)))))

(defcheck solution-63c3a499
  (fn
    [f a]
    (set (map #(set (val %)) (group-by f a)))))

(defcheck solution-640bc922
  (fn e [f s] (set (map (comp set second) (group-by f s)))))

(defcheck solution-65b959ad
  (fn [f a-set]
    (let [getval (fn [m k] (m k #{}))]
      (set
        (vals
          (reduce
            #(assoc %1 (f %2) (conj (getval %1 (f %2)) %2))
            {}
            a-set))))))

(defcheck solution-65c30754
  (fn [f s]
    (set
      (vals
        (apply
          merge-with
          clojure.set/union
          (map
            #(hash-map (f %1) #{%1})
            s
            )
          )
        )
      )
    ))

(defcheck solution-665ca582
  (fn equivalences [f s]
    (set (vals
           (apply merge-with clojure.set/union
             (map #(hash-map (f %) #{%}) s))))))

(defcheck solution-670ff007
  (fn [f domain]
    (set (map set (vals (group-by f domain))))))

(defcheck solution-6720a314
  (fn equi-classes
    [f s]
    (clojure.walk/walk #(into #{} %)
      #(into #{} %)
      (vals (group-by f s)))))

(defcheck solution-67448c5
  (fn [f l]
    (set
      (map #(set %)
        (vals
          (group-by f l))))))

(defcheck solution-682a1f85
  (fn [f xs]
    (into #{}
      (vals
        (reduce (fn [a b]
                  (let [k (f b)
                        v (a k)]
                    (assoc a k (if (nil? v) #{b} (conj v b)))))
          {}
          xs)))))

(defcheck solution-68539ece
  (fn ff [f st]
    (into #{} (for [[k v] (seq (group-by f (seq st)))]
                (into #{} v)))))

(defcheck solution-688d14a8
  (fn [f xs]
    (->> xs
      (reduce
        (fn [result x]
          (let [r (f x) s (get result r #{})]
            (assoc result r (conj s x))))
        {})
      (vals)
      (into #{}))))

(defcheck solution-692627c
  (fn [f d]
    (into #{} (map set (vals (group-by f d))))))

(defcheck solution-6ac68b60
  (fn [f s]
    (into #{} (vals (apply merge-with clojure.set/union (for [m s] {(f m) #{m}}))))))

(defcheck solution-6b614016
  (fn equivalence-class [f a-set]
    (let [partitions (vals (group-by f a-set))]
      (into #{}(for [x partitions]
                 (set x))))))

(defcheck solution-6b98458b
  (fn [f s]
    (into #{} (map set (vals (group-by f s))))))

(defcheck solution-6b9fea0f
  (fn [f domain]
    (set (vals
           (apply merge-with
             clojure.set/union
             (map #(sorted-map (f %) #{%}) domain))))))

(defcheck solution-6bb36794
  (fn[f D](set (for [a D]
                 (set (for [b D :when (= (f a) (f b))] b))))))

(defcheck solution-6dc8288d
  (comp (partial into #{}) (partial map set) vals group-by))

(defcheck solution-6e291b4d
  (fn[f s]
    (into #{}
      (map (fn [[k v]] (into #{} v ))
        (apply merge-with concat
          (map #(identity {(f %) [%]} ) s))))))

(defcheck solution-6e7b1196
  (fn fibers [f, D]
    (set
      (vals
        (reduce
          (fn [acc, x]
            (let [y (f x)]
              (assoc acc y
                         (conj (get acc y #{}) x))))
          {}
          D)))))

(defcheck solution-6ee32178
  (fn equivalence-class
    [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-6ff45b03
  #(->> (group-by % %2)
     vals
     (map set)
     set))

(defcheck solution-71038811
  (fn myf2 [f s]
    (->> (group-by f s)
      vals
      (map #(apply hash-set %))
      (apply hash-set))))

(defcheck solution-71383732
  #(->> %2
     (group-by %1)
     vals
     (map set)
     set))

(defcheck solution-713d98cb
  (fn equivalence-classes [f coll]
    (->> coll
      (group-by f)
      vals
      (map set)
      (into #{}))))

(defcheck solution-71813271
  (fn __
    [f input-set]
    (->> input-set (group-by f) vals (map set) set)))

(defcheck solution-71d7ed3f
  (fn [fnc a-seq]
    (loop [classes {}
           curr (first a-seq)
           rst (rest a-seq)]
      (if (nil? curr)
        (set (vals classes))
        (let [v (fnc curr)
              he (classes v)]
          (if he
            (recur (update-in classes [v] conj curr) (first rst) (rest rst))
            (recur (assoc classes v (set [curr])) (first rst) (rest rst))))))))

(defcheck solution-71f727c3
  (fn [f d]
    (into #{}
      (map (partial into #{})
        (vals
          (reduce (fn [m x] (update-in m [(f x)] conj x)) {} d))))))

(defcheck solution-72aa2f0e
  (fn [f coll]
    (set
      (map
        set
        (vals

          (reduce
            (partial merge-with concat)
            (map #(hash-map (f %) (vector %)) coll)
            ))))))

(defcheck solution-736d0b1f
  (fn [f x]
    (set (map set (vals (group-by f x))))
    ))

(defcheck solution-7371d8a7
  (fn [f col]
    (->> (group-by f col)
      (map (comp set val))
      (set))))

(defcheck solution-7387265a
  (fn equivalence-classes [pf s]
    (set (vals (reduce

                 (fn [a x]
                   (let [p (pf x)
                         v (a p)]
                     (assoc a p (if (nil? v) #{x} (conj v x)))))

                 {}
                 s)))))

(defcheck solution-73ac1616
  (fn [func values]
    (set (map set (vals (group-by func values))))))

(defcheck solution-73e47325
  #(into #{}
     (map (partial into #{})
       (vals (group-by %1 %2)))))

(defcheck solution-73ffe04d
  (fn [f dom]
    (set (map set (vals (group-by f dom))))))

(defcheck solution-742b2d2
  (fn equiv-classes [f coll]
    (letfn [(equiv [a b] (= (f a) (f b)))

            (equiv-for [a ys]
              (into #{} (cons a (filter (partial equiv a) ys))))

            (one-equiv [xs]
              (if (empty? xs)
                #{}
                (equiv-for (first xs) (rest xs))))
            (all-equiv [zs]
              (loop [acc #{} xset zs]
                (if (empty? xset)
                  acc
                  (let [aq (one-equiv xset)
                        nxts (clojure.set/difference xset aq)]
                    (recur (conj acc aq) nxts)))))]
      (all-equiv coll))))

(defcheck solution-75d5ac6b
  (fn[f a](set (map set (vals (group-by f a))))))

(defcheck solution-7613ce61
  (fn equiv-classes [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-7674469
  (fn eq-classes
    [f domain]
    (->> (map f domain)
      (zipmap domain)
      (group-by second)
      (vals)
      (map (comp set keys))
      set)))

(defcheck solution-767be4eb
  (fn [f d]
    (into #{} (map #(into #{} %) (vals (group-by f d))))))

(defcheck solution-76fa32bb
  (fn [f coll]
    (->> (group-by f coll)
      (vals)
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-77c6d93b
  (fn [f D]
    (->> (group-by f D)
      vals
      (map set)
      set)))

(defcheck solution-78258ade
  (fn [f s]
    (set (vals (reduce (fn [acc it]
                         (assoc acc (f it) (conj (acc (f it) #{}) it)))
                 {}
                 s)))))

(defcheck solution-78ade3c6
  (fn equiv-classes [f xs]
    (->> xs
      (map (juxt f identity))
      (group-by first)
      (map second)
      (map (partial map second))
      (map set)
      set)))

(defcheck solution-797f4c0f
  (fn [f d]
    (set (map (comp set second) (group-by f d)))))

(defcheck solution-79f5728e
  #(->> (group-by %1 %2)
     (vals)
     (map set)
     (set)))

(defcheck solution-7a191419
  #(set (map set (vals (group-by
                         %
                         %2)))))

(defcheck solution-7a20de6
  (fn [f D]
    (set
      (map set
        (vals (group-by f D))
        )
      )
    ))

(defcheck solution-7a6c745f
  #(->> (group-by % %2) vals (map set) set ))

(defcheck solution-7af1100c
  #(set (map set (vals
                   (group-by % %2)))))

(defcheck solution-7b90cab1
  (fn [f xs]
    (letfn [(hm [xs] (into #{} xs))]
      (->> xs (group-by f) vals (map hm) hm))))

(defcheck solution-7bd77721
  (fn [f s]
    (set
      (map set
        (vals
          (reduce
            #(update-in %1 [(f %2)] (fn [x] (conj x %2)))
            {}
            s))))))

(defcheck solution-7c6a50ca
  (fn [f s]
    (set
      (map  #(set (second %)) (group-by f s))
      )
    ))

(defcheck solution-7cd236eb
  (fn equivalence [f domain]
    (->> [f domain]
      (apply group-by)
      (vals)
      (map set)
      (set))
    ))

(defcheck solution-7d3858ff
  (fn [f d]
    (into #{} (vals (apply merge-with #(set (concat % %2)) (map #(hash-map (f %) (hash-set %)) d))))))

(defcheck solution-7dffbce5
  #(->> (group-by %1 %2) vals (map set) set))

(defcheck solution-7f1028da
  (fn [f D]
    (->> (group-by f D)
      vals
      (map set)
      set)))

(defcheck solution-7f153807
  (fn equiv-classes [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-7f65de4f
  #(set (map (comp set val)
          (group-by % %2))))

(defcheck solution-7fc9ee7a
  (fn equivalence-class [f D]
    (reduce
      (fn [j k]
        (if-let [s (seq (filter #(seq (clojure.set/intersection % k)) j))]
          (conj (apply clojure.set/difference j (map #(conj #{} %) s)) (apply clojure.set/union k s))
          (conj j k)))
      #{}
      (disj (set
              (for [a D, b D]
                (if (= (f a) (f b))
                  (set [a b])
                  nil))) nil))))

(defcheck solution-80251d7c
  (fn [f d]
    (set
      (map set
        (vals
          (apply merge-with concat
            (map #(hash-map (f %) [%]) d)))))))

(defcheck solution-80e2d457
  (fn [f v]
    (set (map #(set %1) (vals (group-by f v))))
    ))

(defcheck solution-80fae11
  (fn [f s]
    (loop [l (seq s)
           acc []]
      (if (empty? l)
        (set (map #(set (second %)) acc))
        (recur (rest l)
          (let [n (first l)
                calc (f n)]
            (if (some #(= (first %) calc) acc)
              (map #(if (= (first %) (f (first l)))
                      [calc (cons n (second %))]
                      %)
                acc)
              (conj acc [calc #{n}]))))))))

(defcheck solution-818f34bb
  (fn [f d] (set (map set (vals (group-by f d))))))

(defcheck solution-81a10f80
  (fn
    [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-8224bec2
  #(set (->> (mapcat list %2 %2) (sort-by %) (partition-by %) (map set))))

(defcheck solution-82945e47
  (fn [f coll]
    (->> (group-by f coll)
      (vals)
      (map set)
      (set)
      )))

(defcheck solution-82e698be
  (fn eq-classes
    [f coll]
    (->> (group-by f coll)
      vals
      (map set)
      set)))

(defcheck solution-82fb74b2
  (fn equivalence [f xs]
    (->>
      (reduce
        (fn [acc x]
          (let [v (f x)]
            (assoc acc v (conj (get acc v #{}) x))))
        {} xs)
      (vals)
      (set))))

(defcheck solution-83165edc
  (fn p98
    ([f xs] (p98 {} f xs))
    ([s f xs] (if (empty? xs) (into #{} (for [ss (vals s)] (into #{} ss)))
                              (p98 (conj s [(f (first xs)) (conj (s (f (first xs))) (first xs))]) f (next xs))))))

(defcheck solution-83291502
  (fn [f d]
    (into #{} (vals (reduce (fn [a b]
                              (assoc a (f b) (if (a (f b)) (conj (a (f b)) b) #{b})))
                      {} d)))))

(defcheck solution-83453538
  (fn [f D]
    (into #{} (map set (vals (group-by f D))))))

(defcheck solution-83a9631a
  (fn equivs [f s]
    (set
      (vals
        (reduce
          (fn [ans [k v]] (assoc ans v (conj (get ans v #{}) k)))
          {}
          (map (fn [x] [x (f x)]) s)
          )
        )
      )
    ))

(defcheck solution-840feafc
  (fn [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-84dcd3b9
  (fn [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-84e7e4de
  (fn equivalence-classes [f coll]
    (set (vals
           (reduce
             (fn[acc v]
               (let [k (f v)]
                 (assoc acc k (conj (get acc k #{}) v))))
             {}
             coll)))))

(defcheck solution-85486ea4
  (fn [f xs]
    (into #{} (map set (vals (group-by f xs))))
    ))

(defcheck solution-864e104c
  (fn[f d]
    (->> (zipmap d (map f d))
      (group-by second)
      vals
      (map #(into #{} (map first %)))
      (into #{}))))

(defcheck solution-86f0934d
  (fn [f s]
    (reduce #(conj % (set (val %2))) #{} (group-by f s))))

(defcheck solution-86f94dac
  (fn [f l]
    (set (map #(set (second %)) (group-by f l)))
    ))

(defcheck solution-873233d2
  (fn [f D]
    (set (map set (vals (reduce
                          (fn [r n]
                            (update-in r [(f n)] (fnil #(conj % n) [])))
                          {} D))))))

(defcheck solution-8772909f
  (fn equiv-classes [fun col]
    (let [resp
          (loop [col col
                 resp {}]
            (if (= col [])
              resp
              (let [f (first col)
                    k (fun f)]
                (if (contains? resp k)
                  (recur (rest col) (assoc resp k (conj (resp k) f)))
                  (recur (rest col) (assoc resp k #{f}))))))]
      (reduce #(conj %1 (resp %2)) #{} (keys resp)))))

(defcheck solution-8784d4da
  (fn eq-cls [f d]
    (letfn [(cnj-seq [c s] (if (seq s) (apply conj c s) c))]
      (when-let [ds (seq d)]
        (let [fst (first ds)
              a (f fst)
              rst (disj d fst)
              coll (cnj-seq #{fst} (filter #(= a (f %)) rst))]
          (set (cons coll
                 (eq-cls f (apply disj rst (seq coll))))))))))

(defcheck solution-888735b2
  (fn [f c] (set (map set (vals (group-by f c))))))

(defcheck solution-88a22a
  #(->> (group-by % %2)
     (vals)
     (map set)
     (set)))

(defcheck solution-88e0ede7
  (fn [f s]
    (->> s (group-by f) vals (map set) set)))

(defcheck solution-89b5f52e
  (fn [-fn -set]
    (set (map #(set (map last (last %)))
           (group-by #(identity(first %))
             (map #(vector (-fn %) %) -set))))))

(defcheck solution-89d4565f
  (fn [f s]
    (set (map (fn [i] (set (filter #(= (f i) (f %)) s))) s))))

(defcheck solution-89df0fbd
  (fn [f D]
    (->> (reduce (fn [m v]
                   (let [k (f v)]
                     (assoc m k (if (contains? m k)
                                  (conj (m k) v)
                                  #{v}))))
           {} D)
      vals
      set)))

(defcheck solution-8a259ff6
  (fn [f d]
    (->> (group-by f d)
      vals
      (map set)
      set)))

(defcheck solution-8a77f475
  (fn [f D]
    (set (map (comp set second) (group-by f D)))))

(defcheck solution-8a7ac49c
  (fn [f d] (set (map set (vals (group-by f d ))) ) ))

(defcheck solution-8b19cebe
  (fn
    [f coll]
    (set (map set (vals(group-by f coll))))))

(defcheck solution-8b302e20
  #(set (map set (vals (set (group-by % %2))))))

(defcheck solution-8b78ca03
  (fn
    [f c]
    (set (vals (reduce (fn [r v]
                         (let [fv (f v)]
                           (assoc r fv (conj (get r fv #{}) v)))) {} c)))))

(defcheck solution-8b7b2afc
  (fn p98
    [f coll]
    (set (map #(into #{} (val %)) (group-by f coll)))))

(defcheck solution-8b996191
  (fn [f s] (into #{} (map #(into #{} %) (vals (group-by f s))))))

(defcheck solution-8bb97953
  (fn [f d]
    (set (map #(set (val %)) (group-by f d)))))

(defcheck solution-8c57896a
  (fn [f D]
    (->> D
      (map (fn [v] [(f v) #{v}]))
      (map #(apply hash-map %))
      (apply merge-with clojure.set/union)
      vals
      set)))

(defcheck solution-8c840889
  (fn judge[f  col]

    (set (vals (reduce  (fn[map node]
                          (let[k (f node)]
                            (assoc map k
                                       (conj (get map k #{}) node)
                                       )
                            )
                          ) {}  col) ) )
    ))

(defcheck solution-8cb1f740
  (fn [f xs]
    (->> xs
      (reduce (fn [a b] (update-in a (vector (f b)) (fn [x] (conj (or x #{}) b)))) {})
      vals
      (into #{}))))

(defcheck solution-8d2ff80a
  (fn [f d]
    (let [g (group-by f d)]
      (set (map set (vals g))))))

(defcheck solution-8da74b29
  (fn
    [f D]
    (-> (reduce #(let [r (f %2)]
                   (if (contains? % r)
                     (assoc % r (conj (get % r) %2))
                     (assoc % r #{%2})))
          {} D)
      vals
      set)))

(defcheck solution-8e0f1264
  (fn [x y] (apply hash-set (map #(apply hash-set (second %)) (group-by x y)))))

(defcheck solution-8e25f03d
  (fn [f d] (set (map (fn [a] (set (cons a (filter #(= (f a) (f %)) (disj d a))))) d))))

(defcheck solution-8e507dcf
  #(set (map set
          (partition-by %1
            (sort-by %1 %2)))))

(defcheck solution-8e6a1a1
  (fn [f s ]

    (set (map #(set (second %)) (group-by f s) ))

    ))

(defcheck solution-8e941b7
  (fn [f D]
    (into #{}
      (map set
        (vals (group-by f D))))))

(defcheck solution-8ef2321
  (fn [f d]
    (set (vals (reduce #(assoc %1 (first %2)
                                  (if (nil? (%1 (first %2))) #{(second %2)} (conj (%1 (first %2)) (second %2))))
                 {} (for [x d] [(f x) x]))))))

(defcheck solution-8ef9d8e6
  (fn [f d]
    (set (map set (vals (group-by #(f %) d))))))

(defcheck solution-8f43665d
  (fn [f ns]
    (->> (group-by f ns)
      vals
      (map set)
      set)))

(defcheck solution-8fbcf660
  (fn [f xs]
    (->> (group-by f xs)
      vals
      (map set)
      set)))

(defcheck solution-8fd6a71b
  (fn ec [f D]
    (->> D
      (group-by f)
      vals
      (map set)
      set)
    ))

(defcheck solution-8ffeb518
  (fn ec [f s] (set (map set (vals (group-by f s))))))

(defcheck solution-903cf27d
  (fn newBin [f s] (set (map #(into #{} %) (vals (group-by f s))))))

(defcheck solution-9097e85f
  (fn [f D] (set
              (vals
                (reduce
                  (fn [a [k v]]
                    (assoc a k
                             (if (contains? a k)
                               (conj (get a k) v)
                               #{v})))
                  {}
                  (map
                    #(vector (f %) %)
                    D))))))

(defcheck solution-90d07585
  #(set (map (comp set second) (group-by %1 %2))))

(defcheck solution-90ec26d2
  (fn [f s]
    (set (vals (reduce #(merge-with clojure.set/union %1 {(f %2) #{%2}}) {} s)))))

(defcheck solution-9103e3e3
  (fn [f D]
    (->> (group-by f D)
      (map #(set (second %)))
      (into #{}))))

(defcheck solution-911da0f2
  (fn [f D]
    (->> D (group-by f) vals (map set) set)))

(defcheck solution-92344d52
  (fn [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-92f5652
  (fn [f coll]
    ((comp set map)
     (comp set keys)
     (reduce
       #(let [v (f %2)
              c (first (filter (fn [x] (contains? (set (vals x)) v)) %1))]
          (if (nil? c)
            (conj %1 {%2 v})
            (conj (disj %1 c) (assoc c %2 v))))
       #{}
       coll))))

(defcheck solution-9310da86
  (fn eqc [f d]
    (loop [ds d acc {}]
      (if (seq ds)
        (let [val (first ds)]
          (recur (rest ds) (merge-with into acc {(f val) #{val}})))
        (set (vals acc))))))

(defcheck solution-931fb675
  (fn aa [x y] (set (map set (vals (group-by x y))))
    ))

(defcheck solution-93369746
  (fn
    [f d]
    (loop [fv (distinct (map f d)) r #{}]
      (if (empty? fv)
        r
        (recur (rest fv) (conj r (set (filter #(= (f %) (first fv)) d))))
        )
      )
    ))

(defcheck solution-9387b90
  (fn tmp [f l]
    (reduce (fn [x y] (conj x (set (filter #(= (f %) (f y)) l)))) #{} l)))

(defcheck solution-938f49ab
  (fn [f D]
    (reduce
      (fn [r n]
        (conj r (into #{} (filter #(= (f n) (f %)) D))))
      #{}
      D)))

(defcheck solution-9407f6bc
  (fn equivalenceClasses [func elems]
    (set
      (vals
        (reduce
          (fn [collector elem]
            (let [key (func elem)]
              (assoc
               collector
                key
                (if (contains? collector key)
                  (conj (collector key) elem)
                  (set (vector elem))
                  )
                )
              )
            )
          {}
          elems
          )
        )
      )
    ))

(defcheck solution-945a573e
  (fn equiv
    [f D]
    (set
      (for [a D]
        (set
          (for [b D :when (= (f a) (f b))]
            b))))))

(defcheck solution-949d4be4
  (fn [f coll]
    (reduce #(conj %1 (set (second %2)))
      #{}
      (group-by f coll))))

(defcheck solution-94da4c21
  (fn [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-952ff8c3
  (fn equi-classes [f domain]
    (let [mapped (map #(list (f %) %) domain)]
      (set (map #(set (map second %))
             (vals (group-by first mapped)))))))

(defcheck solution-9643b54a
  (fn [f x]
    (set (map (fn [[k v]] (set v)) (group-by f x)))))

(defcheck solution-96a84bdb
  (fn k [f D] (into #{} (map #(into #{} %) (vals (group-by f D))))))

(defcheck solution-9865b7f7
  (fn [f dom]
    (let [classmap (reduce (fn [acc ele] ; key = (f x) / val = #{x's}
                             (let [k (f ele)]
                               (assoc acc k (conj (acc k #{}) ele))))
                     {}
                     dom)]
      (set (vals classmap)))))

(defcheck solution-988e6d8b
  #(set (map (fn [m] (set (val m))) (group-by % %2))))

(defcheck solution-9894f2a2
  #(->> (group-by %1 %2) (vals) (map set) (set)))

(defcheck solution-98b02fd2
  (fn [f D](set (map set (vals (group-by f  D))))))

(defcheck solution-98c9b144
  (fn this [f domain]
    (letfn [(f1 [remaining m]
              (if (empty? remaining)
                m
                (let [item (first remaining)
                      mkey (f item)
                      mval (get m mkey)
                      to-merge (if mval
                                 {mkey (conj mval item)}
                                 {mkey #{item}}) ; if
                      ]
                  (recur (rest remaining) (merge m to-merge))) ; let
                ) ; if
              ) ; f1
            ]
      (set (map #(second %) (f1 (seq domain) {})))
      ) ; letfn
    ))

(defcheck solution-98e8315b
  (fn equiv-rel [f D]
    (set (map #(set (filter
                      (fn [x] (= (f %) (f x))) D))
           D))))

(defcheck solution-99214ec9
  (fn [f x]
    (set (map set (vals
                    (reduce (fn [a b]
                              (let [p (f b)]
                                (conj a {p (cons b (a p))})))
                      {} x))))))

(defcheck solution-994f05f4
  (fn [f ss]
    (loop [s ss r {}]
      (if-not (empty? s)
        (let [e (first s) fe (f e) eqc (get r fe)]
          (recur (rest s) (if eqc (assoc r fe (conj eqc e))
                                  (assoc r fe #{e}))))
        (set (vals r))))))

(defcheck solution-9a466617
  (fn [f D]
    (->> (reduce (fn [acc obj]
                   (let [res (f obj)]
                     (if (contains? acc res)
                       (update-in acc [res] conj obj)
                       (assoc acc res #{obj}))))
           {} D)
      vals
      set)))

(defcheck solution-9a63def
  (fn p98[f d]
    (into #{} (vals (apply merge-with clojure.set/union (map #(hash-map (f %) #{%} ) d))))))

(defcheck solution-9a69dfca
  (fn [f domain]
    (->> domain
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-9a9793a0
  (fn [f D] (->> D (group-by f) vals (map set) set)))

(defcheck solution-9ac707
  (fn f98 [f st]
    (set (map #(set (map first %))
           (map second
             (group-by second
               (map (fn [x] ((juxt identity f) x)) st)))))))

(defcheck solution-9aeeff1b
  (fn [f s]
    (let
     [r (fn r [m s]
          (if (empty? s) m
                         (r (update-in
                              m
                              [(f (first s))]
                              (comp set conj)
                              (first s))
                           (rest s))))]
      (set (vals (r {} s))))))

(defcheck solution-9b0773f5
  (fn [f s]
    (let [a (into [] s)
          b (map f a)
          c (zipmap a b)
          d (sort-by second c)
          e (group-by second d)
          f (vals e)]
      (->> (map (fn [x](map #(first %) x)) f)
        (map #(apply hash-set %))
        (into #{})))))

(defcheck solution-9bada8ae
  (fn [f s]
    (set (for [y (set (map f s) )]
           (set (filter #(= y (f %)) s))
           ))
    ))

(defcheck solution-9c7c1d63
  (fn [f x] (set (map #(set (second %)) (group-by f (apply list x))))))

(defcheck solution-9c861b18
  (fn eq-classes [function domain]
    (set (map set (vals (group-by function domain))))))

(defcheck solution-9c8e83c3
  (fn [f x]
    (set (map set (vals (group-by f x))))))

(defcheck solution-9ca99cc7
  (fn [f s]
    (set (map set (vals (group-by #(f %) s))))))

(defcheck solution-9ce61390
  (fn [f coll] (reduce #(conj %1 (set %2)) #{} (vals (group-by f coll)))))

(defcheck solution-9d64a63
  (fn equivalence
    [f s]
    (->> (group-by f s)
      vals
      (map set)
      set)))

(defcheck solution-9fc50f2b
  (fn eclass [f s]
    (set (reduce (fn [acc1 x1]
                   (concat acc1
                           (vals (reduce (fn [acc2 x2]
                                           (if (= (f x1) (f x2))
                                             (assoc acc2 (f x1) (into (get acc2 (f x1) #{}) [x1 x2]))
                                             acc2))
                                   {}
                                   s)))) #{}
           s))))

(defcheck solution-9fd4e1d2
  (fn equivalence
    ([f s]
     (equivalence f (vec (rest s)) 0 (set (list (first s))) #{}))
    ([f s pointer current equiv]
     (if (= 0 (count s))
       (conj equiv current)
       (if (> pointer (count s))
         (recur f (vec (rest s)) 0 (set (list (first s))) (conj equiv current))
         (if (and (first current) (get s pointer) (= (f (first current)) (f (get s pointer))))
           (recur f (if (= pointer (dec (count s)))
                      (vec (butlast s))
                      (if (= pointer 0)
                        (vec (rest s))
                        (apply conj (subvec s 0 pointer) (subvec s (inc pointer))))) 0 (conj current (get s pointer)) equiv)
           (recur f s (inc pointer) current equiv)))))))

(defcheck solution-a02f997e
  (fn [f d]
    (->> (group-by f d)
      (vals)
      (map set)
      (into #{}))))

(defcheck solution-a29a3b65
  (fn [f s] ; fn set
    (set (map (comp set last) (group-by f s)))))

(defcheck solution-a3da3383
  (fn [f d]
    (->> d
      (group-by #(f %))
      vals
      (map set)
      (into #{}))))

(defcheck solution-a46d628c
  (fn [f s](
             set(map (fn[i]( set(map (fn[j](first j)) i ))) (map second (group-by second (map-indexed (fn[idx itm][itm (f itm)]) s))))
             )))

(defcheck solution-a4cdcfb4
  (fn group-by-equiv [op s]
    (set (map #(set (map first (map vals %)))  (vals (group-by keys (map (fn [x] { (op x) x}) s)))))))

(defcheck solution-a51fd041
  (fn comp-eq-cases [fun d]
    (let [get-values (fn [ci c]
                       (for [i ci] ((vec c) i) ))
          vd (-> d vec sort)
          vs (map fun vd)
          idxs (take (count vd) (range))
          vsi (map vector vs idxs)
          vsi2 (sort-by #(first %) vsi)
          vsi3 (partition-by #(first %) vsi2)
          idx2 (map (fn [s] (map #(second %) s)) vsi3)
          r (for [s idx2] (set (get-values s vd)))]
      (set r))))

(defcheck solution-a5e7f1b4
  #(set (for [[k v] (group-by %1 %2)] (set v))))

(defcheck solution-a62da648
  (fn [f s]
    (into #{}
      (map (comp set second)
        (group-by f s)))))

(defcheck solution-a6651dd1
  (fn eqcls [f dom]
    (let [eqcl (fn eqclsx [f dom]
                 (remove (fn [u] (= #{nil} u) )  (map  #(if (set? %) % #{%})
                                                   (let [x (first dom) d (rest dom)]
                                                     (if (not (seq d))
                                                       #{x}
                                                       (let [ec (into #{} (conj (filter #(= (f x) (f %)) d) x))
                                                             rm (remove #(= (f x) (f %)) d)]
                                                         (concat #{ec} (cond
                                                                         (> (count rm) 1) (eqclsx f rm)
                                                                         :ELSE  #{(first rm)}) )))))))]
      (into #{} (eqcl f dom)))))

(defcheck solution-a69eba86
  (fn [f coll]
    (set
      (map set
        (map second (group-by f coll))))))

(defcheck solution-a75c95c0
  (fn [f coll]
    (set (map #(set (val %)) (group-by f coll)))))

(defcheck solution-a78b55ef
  #(->> %2
     (group-by %)
     (vals)
     (map set)
     (set)))

(defcheck solution-a795f28d
  (fn [f as]
    (->> as
      (reduce (fn [m a]
                (let [b (f a)] (assoc m b (conj (get m b #{}) a))))
        {})
      (vals)
      (set))))

(defcheck solution-a7aa7996
  (fn equiv [f x]
    (into #{}
      (map #(into #{} %)
        (vals (group-by f x))))
    ))

(defcheck solution-a83619f7
  (fn [f s]
    (set (map set (vals
                    (group-by f s)
                    )))
    ))

(defcheck solution-a85710e9
  (fn [f l]
    (letfn [(worker [l s]
              (if (empty? l)
                s
                (recur (rest l)
                  (let [v (f (first l)) p (find s v)]
                    (if p
                      (assoc s v (conj (val p) (first l)))
                      (assoc s v #{(first l)}))))))]
      (set (map val (worker l {}))))))

(defcheck solution-a85f756d
  (fn [f s] (into #{} (map #(into #{} %) (vals (group-by f s )) ))))

(defcheck solution-a892ef39
  (fn [f coll]
    (->> coll
      (group-by f )
      (vals )
      (map #(into #{} %) )
      (into #{} ))))

(defcheck solution-a8fe3cdb
  (fn [f coll]
    (let [reducef (fn [m x]
                    (if (m (f x))
                      (update-in m [(f x)] conj x)
                      (conj m [(f x) #{x}])))]
      (into #{} (vals (reduce reducef {} coll))))))

(defcheck solution-aa06ab8b
  (fn [f s] (->> s (group-by f) vals (map set) set)))

(defcheck solution-aa1db522
  (fn [f d]
    (set
      (map set (vals (group-by f d))))))

(defcheck solution-aaa9c872
  (fn [f D] (set (map set (vals (group-by #(f %) D))))))

(defcheck solution-aac0a4fe
  #(set (map (comp set val) (group-by %1 %2))))

(defcheck solution-aadfdd5e
  #(into #{}
     (map set
       (vals (group-by %1 %2)))))

(defcheck solution-abaa89af
  (fn [f  s] (into #{}
               (map set (vals
                          (group-by f  s))))))

(defcheck solution-ac3496df
  #(set (for [x %2]
          (set (for [y %2 :when (= (% x) (% y))]
                 y)))))

(defcheck solution-acd51cd1
  (fn [f d]
    (->>  d
      (group-by f)
      (vals)
      (map #(into #{} %) ,)
      (into #{} ,))))

(defcheck solution-ad605e12
  (fn [f D]

    (set (map set (vals (group-by f D))))))

(defcheck solution-adc0a3b5
  (fn [f D] (set (map set (vals (group-by f D))))))

(defcheck solution-ae2b223a
  (fn [f sx] (
               set (vals (apply merge-with clojure.set/union (map #(hash-map (f %) #{%}) sx)
                           )))))

(defcheck solution-ae54a2
  (fn eq [f d]
    (into #{}
      (for [x (vals (group-by second (zipmap d (map f d))))]
        (set (map first x))))))

(defcheck solution-aec318b6
  (fn [f D] (let [results (map (comp vector f) D)
                  d-elements (map (comp set vector disj) D)
                  d-elem-hashmap (map hash-map results d-elements)
                  result-elem-hashmap (reduce #(merge-with clojure.set/union %1 %2) {} d-elem-hashmap)]
              (-> result-elem-hashmap vals set))))

(defcheck solution-afa4c5fb
  (fn [f s]
    (->> (group-by f s)
      vals
      (map set)
      (into #{}))))

(defcheck solution-afb621ee
  #(->>
     %2
     (group-by %)
     vals
     (map set)
     set))

(defcheck solution-afcb6e48
  (fn [f d]
    (set
      (map
        #(set (val %))
        (reduce
          #(let [r (f %2)] (assoc % r (conj (% r) %2)))
          {}
          d)))))

(defcheck solution-afe70696
  (fn [f s] (set (map #(set (map first %)) (vals (group-by second (map (juxt identity f) s)))))))

(defcheck solution-b0347c02
  (fn [f coll] (set (map set (vals (group-by f coll))))))

(defcheck solution-b05343d3
  (fn [f xs]
    (reduce #(conj %1 (set %2)) #{}
      (vals (reduce #(merge-with concat %1 {(f %2) [%2]}) {} xs)))))

(defcheck solution-b0df163d
  (fn [f s]
    (into #{}
      (vals (reduce
              #(let [k (f %2)]
                 (if (contains? %1 k)
                   (assoc %1 k (conj (get %1 k) %2))
                   (assoc %1 k #{%2})))
              {}
              s)))))

(defcheck solution-b1833258
  (fn [f coll]
    (set
      (map (comp set second)
        (group-by f coll)))))

(defcheck solution-b1b5323d
  (fn equi[f s]
    (into #{} (for [x s ]
                (into #{} (for [z s
                                :when (= (f x) (f z))]
                            z))))))

(defcheck solution-b1cb9262
  (fn eq-class [f D]
    (into #{} (map #(into #{} %) (vals(group-by f D))))
    ))

(defcheck solution-b2024609
  (fn [f s]
    (set (map #(set(map second %))
           (partition-by first
             (sort-by first
               (map #(vector (f %) %) s)))))))

(defcheck solution-b2ba3310
  (let [eq-rel (fn [f xs ys rs]
                 (if (seq xs)
                   (recur f (rest xs) ys (conj rs (into #{(first xs)} (filter #(= (f (first xs)) (f %)) ys))))
                   rs ))]
    #(eq-rel %1 %2 %2 #{})))

(defcheck solution-b2dac75
  (fn [f xs]
    (->> xs
      (map (fn [x] (->> xs (filter #(= (f %) (f x))) set)))
      set)))

(defcheck solution-b3076b82
  #(->> %2
     (group-by %1)
     (map (comp set second))
     (set)))

(defcheck solution-b31ae659
  #(->> (group-by % %2)
     vals
     (map set)
     set))

(defcheck solution-b326791a
  #(->> %2
     (group-by %)
     vals
     (map set)
     set
     ))

(defcheck solution-b3c00ba7
  (fn [f s] (set (map #(into #{} %) (vals (group-by f s))))))

(defcheck solution-b3cb35b4
  (fn equiv [f xs]
    (loop [acc {} rem xs]
      (if (empty? rem)
        (-> acc vals set)
        (recur
          (let [x (first rem)]
            (update-in acc [(f x)]
              (fnil #(conj % x) #{})))
          (rest rem))))))

(defcheck solution-b3e7ebbf
  (fn[f in]
    (set (map #(set %) (partition-by f (sort-by f in))))))

(defcheck solution-b4438451
  (fn [f c]
    (set (map set (vals (group-by f c))))))

(defcheck solution-b587d128
  (fn [f D]
    (into #{} (map #(into #{} %) (vals (group-by f D))))))

(defcheck solution-b645075a
  (fn [f coll] (into #{} (map set (vals (group-by f coll))))))

(defcheck solution-b6b41d5a
  (fn [f s] (set (map #(set (map last %)) (vals (group-by first (map #(vector (f %) %) s)))))))

(defcheck solution-b6d37780
  (fn [f s]
    (into #{} (map (fn [[k v]] (into #{} v)) (group-by f s)))))

(defcheck solution-b7a50443
  (fn equiv [f lst]
    (into #{} (map #(into #{} %) (vals (group-by f lst))))))

(defcheck solution-b8458952
  (fn [f d] (set (map #(set %) (vals (group-by f d))))))

(defcheck solution-b8beee04
  (fn [f sets]
    (set (map set (vals (group-by f (seq sets)))))))

(defcheck solution-b952c9a0
  (fn [f s] (set (map (comp set val) (group-by f s)))))

(defcheck solution-b95db469
  (fn [f S]

    (apply hash-set (map #(apply hash-set %) (vals (group-by f S))))

    ))

(defcheck solution-b995c87f
  (fn [f s]
    (set (map set (vals (group-by f s))))
    ))

(defcheck solution-ba69966c
  (comp set (partial map set) vals group-by))

(defcheck solution-baa9e923
  (fn [f d]
    (into #{} (map #(into #{} %) (vals (group-by f d))))
    ))

(defcheck solution-bad86c8e
  (fn [f x] (set (map set (vals (group-by f x))))))

(defcheck solution-bb70e464
  (fn [f s](set (map set (vals (group-by f s))))))

(defcheck solution-bb751016
  (fn [f xs]
    (->> xs
      (sort-by f)
      (partition-by f)
      (map #(set %))
      (set))))

(defcheck solution-bc84c077
  (fn [g d]
    (->> d
      (group-by g)
      vals
      (map set)
      (into #{}))))

(defcheck solution-bd746f38
  (fn my-equivalence-classes
    [f values]
    (set (map set (vals (group-by f values))))))

(defcheck solution-bd90a12f
  (fn [x y]
    (->> (group-by x y)
      vals
      (map set)
      set)))

(defcheck solution-bdeb74e2
  (fn [f i] (set (map set (partition-by f (sort #(compare (f %) (f %2)) i))))))

(defcheck solution-be65eed8
  (fn [f xs]
    (->> (group-by f xs)
      vals
      (map set)
      set)))

(defcheck solution-bfc26415
  (fn [f s]
    (into #{} (map set
                (vals (group-by f s))))))

(defcheck solution-c02d8505
  (fn [f xs]
    (->> xs
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-c0d3c4ab
  (fn __ [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-c0d67e6e
  (fn [f xs] (into #{} (map #((comp set second) %)  (group-by f xs)))))

(defcheck solution-c23ed796
  (fn [f coll]
    (reduce
      (fn [sets el]
        (conj
          sets
          (into #{} (filter #(= (f el) (f %)) coll))))
      #{}
      coll)))

(defcheck solution-c2ba90e0
  #(into #{} (map (comp set second) (group-by % %2))))

(defcheck solution-c2de7004
  #(->>
     (map (juxt %1 identity) %2)
     (sort-by first)
     (map second)
     (partition-by %1)
     (map set)
     set))

(defcheck solution-c3890f4c
  (fn [f s]
    (->> s
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-c3abffaf
  #(set (map (fn[[_ v]] (set v)) (group-by % %2))))

(defcheck solution-c3b1d7af
  (fn [f the-set]
    (set (map set (vals (group-by f the-set))))))

(defcheck solution-c41f447
  (fn gec [f D]
    (into #{} (map #(into #{} (for [i %] (first i)))
                (map #(second %) (group-by #(second %)
                                   (apply assoc {} (interleave D (map f D)))))))))

(defcheck solution-c424ead0
  (fn [f xs] (->> (group-by f xs)
               vals
               (map set)
               set)))

(defcheck solution-c440b37e
  (fn me [f my-set]

    (let  [ my-vec   (into [] my-set)

           f-values (map f my-vec)

           my-mapping (map list f-values my-vec)

           my-vals (vals (group-by first my-mapping))
           ]


      (into #{}
        (for [x my-vals]
          (into #{} (map second x))
          )
        )

      )


    ))

(defcheck solution-c461afa
  (fn equivalence-classes [op domain]
    (apply hash-set (vals (apply (partial merge-with clojure.set/union) (for [element domain]
                                                                          {(op element) #{element}}))))))

(defcheck solution-c47f2e93
  (fn [f d]
    (set (map #(set (% 1)) (group-by f d)))))

(defcheck solution-c4dd6ad2
  (fn equiv
    [f s]
    (into #{}
      (map
        #(let [res #{%}]
           (into res
             (filter (fn [x] (= (f x) (f %))) s)))s))))

(defcheck solution-c65217b8
  (fn equivalence-classes [f v]
    ((comp set vals) (apply merge-with clojure.set/union
                       (map #(hash-map (f %) (set [%])) v)))))

(defcheck solution-c693b99a
  (fn equiv [f s]
    (set (map set (partition-by f (sort-by f s))))))

(defcheck solution-c8821b4b
  (fn [f s]
    (->> (group-by f s)
      (map second)
      (map set)
      set)))

(defcheck solution-c8fc52bc
  (fn [op D]
    (set (map set (vals (group-by op D))))))

(defcheck solution-c9061bdd
  (fn equiv[f xs]
    (->> (map f xs)
      (zipmap xs)
      (reduce-kv (fn[init k v]
                   (assoc-in init [v] (if (nil? (get-in init [v]))
                                        #{k}
                                        (conj (get-in init [v]) k))))
        {})
      (vals)
      (set))))

(defcheck solution-c90975bd
  (fn [f d]
    (into #{} (map #(into #{} %) (vals (group-by f d))))))

(defcheck solution-c911b410
  (fn [f D]
    (->> D (map #(hash-map (f %) #{%}))
      (apply merge-with clojure.set/union)
      (vals)
      (set) )))

(defcheck solution-c930023b
  (fn equivalenceClasses [f D] (let [equivalent (fn [f d a] (reduce #(if (= (f a) (f %2)) (conj %1 %2) %1) #{} d))]
                                 (set (map (partial equivalent f D) D)))))

(defcheck solution-c9dbfb3b
  (fn [f s]
    (set (vals (apply merge-with into (map #(hash-map (f %) #{%}) s))))))

(defcheck solution-ca55168f
  (fn [f s]
    (set
      (map set (vals (group-by f s))))))

(defcheck solution-ca864d91
  (fn [f col]
    (set
      (map
        set
        (vals (group-by f col)))
      )))

(defcheck solution-ca8e55d3
  (fn [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-caf18329
  (fn equivalence- [f D]
    ^{:doc "Write a function with arguments f and D that computes the
  equivalence classes of D with respect to f."}
    (set (map set (vals (group-by f D))))))

(defcheck solution-cb11b7b7
  (fn [f ns] (set (map set (vals (group-by f ns))))))

(defcheck solution-cb968db7
  #(->>
     (group-by %1 %2)
     vals (map set) set))

(defcheck solution-cba9173
  #(->> %2 (group-by %) vals (map set) set))

(defcheck solution-cbfa4b3d
  (fn eqiv [f s]
    (->> s
      (map #(hash-map (f %) #{%}) ,,)
      (apply merge-with clojure.set/union ,,)
      (vals)
      (reduce #(conj %1 %2) #{} ))))

(defcheck solution-cc6b1398
  (fn [f s]
    (->> s
      (group-by f)
      sort
      vals
      (map set)
      set)))

(defcheck solution-cd0c98a5
  (fn equivalence-classes [f coll]
    (set
      (map #(set %)
        (vals (group-by f coll))))))

(defcheck solution-cd683eac
  (fn [f domain]
    (let [mf (memoize f)]
      (loop [[x & _ :as xx] (seq domain), result #{}]
        (if (nil? x) result
                     (let [eqs (set (filter #(= (mf x) (mf %)) xx))]
                       (recur (remove eqs xx) (conj result eqs))))))))

(defcheck solution-cd74e3f2
  (fn eq [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-cdb40ae6
  (fn [func domain]
    (let [all-map (zipmap domain (map func domain))]
      (loop [result {} elements domain]
        (if elements
          (recur
            (if (contains? result (func (first elements)))
              (into result {(func (first elements)) (conj (result (func (first elements))) (first elements))})
              (into result {(func (first elements)) #{(first elements)}})
              )
            (next elements)
            )
          (set (vals result))
          )
        )
      )
    ))

(defcheck solution-ce0d2505
  (fn equivalence [f d] (set (map set (vals (group-by f d))))))

(defcheck solution-ce8a7e7c
  (fn [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-ced47658
  (fn
    [f colls]
    (set(map
          #( set (second %))
          (group-by f colls)))))

(defcheck solution-cf0f82a5
  (fn [f a]
    (set (map set
           (for [n a]
             (filter #(= (f n) (f %)) a))))))

(defcheck solution-cf21e418
  (fn [f s]
    (into #{} (map #(reduce (fn [a b] (conj a (:v b)) ) #{} (val %))
                (group-by :k (map (fn [v] {:k (f v) :v v}) s))))))

(defcheck solution-cf4b03f3
  (fn [f s]
    (->>
      s
      (group-by f)
      (vals)
      (map set)
      set)))

(defcheck solution-cf50a686
  (fn [f x] (set (map #(set (second %)) (group-by f x)))))

(defcheck solution-cf7da3ff
  (fn [f coll]
    (->> coll
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-cfb4e779
  (fn equiv-classes [f c]
    (->>
      c
      (map #(hash-map (f %) #{%}))
      (reduce (partial merge-with clojure.set/union))
      vals set
      )))

(defcheck solution-cfc62922
  (fn [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-cfde96fa
  (fn [f coll]
    (->>
      coll
      (group-by f)
      vals
      (map set)
      set)))

(defcheck solution-d0881761
  (fn [f x]
    (set (map set(vals (group-by f x))))))

(defcheck solution-d088ef6d
  (fn [f d]
    (->> (group-by f d)
      (vals)
      (map set)
      (set))))

(defcheck solution-d0f8055e
  (fn equiv-rel [f D] (set (for [[k v] (group-by f D)]
                             (set v)))))

(defcheck solution-d1185409
  (fn [f d]
    (into #{} (map #(into #{} (map first %)) (vals (group-by second (partition 2 (interleave d (map f d)))))))))

(defcheck solution-d1bb363b
  (fn a [f D]
    (->>
      (reduce #(update-in %1 [(f %2)] (fnil conj #{}) %2) {} D)
      (vals)
      (set))))

(defcheck solution-d1e210a2
  (fn [f domain]
    (->>
      (group-by f domain)
      vals
      (map set)
      set)))

(defcheck solution-d2095f75
  (fn [f d]
    (->> d
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-d20e03e
  (fn equiv-class [f D]
    (let [m (group-by f D)]
      (set (map set (vals m))))))

(defcheck solution-d2e711af
  (fn [f s]
    (letfn [(classify [f ret x]
              (set (map set (concat (filter #(not= (f x) (f (first %))) ret)
                                    (conj #{} (conj (first (filter #(= (f x) (f (first %))) ret)) x))))))]
      (reduce #(classify f %1 %2) #{} s))))

(defcheck solution-d311d528
  (fn [f dom] (into #{} (map (fn [[k vs]] (into #{} vs)) (group-by f dom)))))

(defcheck solution-d3adcfb2
  (fn eq-classes[f a-set]
    (set (map #(set (second %)) (group-by f a-set)))
    ))

(defcheck solution-d3c7cb3c
  (fn [op l]
    (set (map
           (fn [el] (set (filter #(= (op %) el)  l)))
           (set (map op l))))))

(defcheck solution-d418f18f
  (fn equiv [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-d4c86173
  (fn [f s]
    (let [t (fn [f s] (loop [tmps s m {}]
                        (if (empty? tmps)
                          m
                          (if (contains? m (f (first tmps)))
                            (recur (rest tmps) (assoc m (f (first tmps)) (conj (get m (f (first tmps))) (first tmps))))
                            (recur (rest tmps) (assoc m (f (first tmps)) (conj #{} (first tmps))))))))]
      (loop [ans #{} tmp (t f s)]
        (if (empty? tmp)
          ans
          (recur (conj ans (second (first tmp))) (rest tmp)))))))

(defcheck solution-d52a243c
  (fn [f D]
    (set (vals (reduce #(assoc %1 (f %2)  (conj (get %1 (f %2) #{}) %2) ) {} D)))
    ))

(defcheck solution-d57305ce
  (fn [f d]
    (let [s (map hash-set d)
          find (fn [s x]
                 (first (filter #(= (f (first %)) (f (first x))) s)))
          update (fn [a x]
                   (let [e (find a x)]
                     (if e
                       (conj (disj a e)
                         (conj e (first x)))
                       (conj a x))))]
      (reduce update
        (hash-set (first s))
        (rest s)))))

(defcheck solution-d59c9d8a
  (fn [f items]
    (loop [results {} items items]
      (if (empty? items)
        (set (vals results))
        (let [v (first items) k (f v)]
          (recur
            (assoc results k (conj (get results k #{}) v))
            (rest items))
          )
        )
      )
    ))

(defcheck solution-d5c12e27
  (fn [f c]
    (set (map set (vals (reduce #(let [v (f %2)]
                                   (assoc %1 v (conj (get %1 v []) %2))) {} c))))))

(defcheck solution-d6082cb9
  #(->> (group-by % %2) (map (comp set val)) set))

(defcheck solution-d621cf04
  (fn [f s]
    (set (map set (partition-by f (sort-by f s))))))

(defcheck solution-d7063742
  (fn [f d] (letfn [(skeys [g dm] (map g dm))
                    (get2  [xx] (map second xx))
                    (vorr [h dh] (map get2 (vals (group-by first
                                                   (partition 2
                                                     (interleave (skeys h (vec dh)) (vec dh) )
                                                     )
                                                   ))))
                    ]
              (set (map set (vorr f d)))
              )
    ))

(defcheck solution-d72a117a
  (fn [f args]
    (set (map set (vals (group-by f args))))))

(defcheck solution-d7e98604
  (fn [f D]
    (set (vals (reduce (fn [classes x]
                         (update-in classes [(f x)] (fnil #(conj % x) #{})))
                 {} D)))))

(defcheck solution-d814c82f
  (fn [f s]
    (set (map (comp set val) (group-by f s)))))

(defcheck solution-d8d22556
  (fn
    [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-d8d7a594
  (fn [f s]
    (set (map (comp set second) (group-by f s)))))

(defcheck solution-d93fad3a
  (fn [f s]
    (set (vals
           (reduce (fn [agg e]
                     (let [k (f e)]
                       (if (contains? agg k)
                         (assoc agg k (conj (agg k) e))
                         (assoc agg k #{e})))) {} s)))))

(defcheck solution-d9dd5c19
  #(set (->> %2 (group-by %1) (map (comp set val)))))

(defcheck solution-d9e337a4
  (fn [f s] (set
              (map set (vals (group-by f s))))))

(defcheck solution-d9f5c1a8
  (fn[f xs]
    (set (map (comp set second) (group-by f xs)))))

(defcheck solution-d9fb064e
  (fn [f D]
    (set (vals (apply merge-with clojure.set/union (map #(hash-map (f %) #{%}) D))))))

(defcheck solution-da0561f3
  (fn [f s]
    (set (map set (vals (reduce (fn [a b] (update-in a [(f b)] (fn [xs] (conj xs b) ))) {} s))))))

(defcheck solution-da57e099
  (fn __ [f D]
    (set
      (map set (vals (group-by f D))))))

(defcheck solution-da78aa0a
  (comp (partial into #{}) (partial map #(into #{} %)) vals group-by))

(defcheck solution-da7cbf67
  (fn prob98
    [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-daa61c24
  (fn [a b]
    (reduce conj #{} (map set (vals (group-by a b))))))

(defcheck solution-dac2a45
  (fn [f d]
    (letfn [(equiv [x domain]
              (set (filter #(= (f x) (f %)) domain)))]
      (set (map #(equiv % d) d)))
    ))

(defcheck solution-dac4503
  (fn [f c]
    (set (map set (vals (group-by f c))))))

(defcheck solution-db6b74aa
  (fn eqv-classes [f coll]
    (letfn [(eqv-tuples [f coll]
              (map (fn [x] [(f x) x]) coll))
            (transmog-map [m f]
              (into {} (for [[k v] m] [k (f v)])))
            (eqv-lists [tuples]
              (transmog-map (group-by #(first %) tuples) #(map second %)))
            (eqv-sets [eqvlists]
              (set (map #(set (second %)) eqvlists)))]
      (-> (eqv-tuples f coll)
        eqv-lists
        eqv-sets))))

(defcheck solution-db8037ea
  (fn [fun domain]
    (into #{} (map #(into #{} %) (partition-by fun (sort-by fun domain))))))

(defcheck solution-dbe357be
  (fn eqclass [f d]
    (let [groups (vals (group-by first (map (fn [n] [(f n) n]) d)))]
      (set (map (fn [g] (set (map second g))) groups)))))

(defcheck solution-dc4bd555
  (fn [f d] (set  (map set (vals (group-by f d))))))

(defcheck solution-dc844ba6
  #(->> %2
     (group-by %)
     vals
     (map set)
     set))

(defcheck solution-dd091759
  (fn
    [f D]
    (->>
      (group-by f D)
      vals
      (map set)
      set)))

(defcheck solution-dd2e65b8
  #(->> %2 (group-by %1) vals (map set) set))

(defcheck solution-ddabf1e2
  (fn [f d]
    (->> (map vector (map f d) d)
      (sort-by first )
      (partition-by first)
      (map #(set (map second %)))
      set)))

(defcheck solution-de07df98
  (fn [o d] (set (map #(set (second %)) (group-by o d)))))

(defcheck solution-de29fd49
  (comp set
        vals
        (fn [f d]
          (reduce #(let [y (f %2)]
                     (assoc % y (conj (% y #{}) %2)))
            {} d))))

(defcheck solution-de6e26b0
  (fn [ f d ]
    (->> d (group-by f) vals (map set) set)))

(defcheck solution-dea5dab7
  (fn [f a] (set (map set (vals (group-by f a))))))

(defcheck solution-deaa7dd0
  (fn [f d] (set (map set (vals (group-by #(f %) d))))))

(defcheck solution-df53768e
  (fn [f d]
    (set (map #(into #{} %) (vals (group-by f d))))))

(defcheck solution-dfacf983
  (fn [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-dfcfe4e9
  (fn equivalence-classes
    [f coll]
    (set ((comp (partial map set) vals) (group-by f coll)))))

(defcheck solution-e05b4ca7
  #(->> (group-by % %2) (map (comp set second)) set))

(defcheck solution-e093c8df
  (fn equivrel [f D]
    (let [genhashmap
          (fn genhashmap ([f D]
                          (genhashmap f D {}))
            ([f D m]
             (if (empty? D) m
                            (recur f (rest D)
                              (assoc m (f (first D)) (conj (get m (f (first D))) (first D)))
                              ))))]
      (set (map set (vals (genhashmap f D)))))))

(defcheck solution-e20716a0
  (fn[f s](set(map #(set(map first(val %)))(group-by val(zipmap s(map f s)))))))

(defcheck solution-e31474c8
  (fn [fun coll]
    (set (->> (group-by fun coll)
           (map (comp set second))))))

(defcheck solution-e43e3092
  (fn [f dom]
    (into #{} (map #(into #{} %) (vals (group-by f dom))))))

(defcheck solution-e446c0fc
  (fn [f D]
    (into #{} (vals (apply merge-with clojure.set/union (for [a D b D]
                                                          (if (= (f a) (f b))
                                                            {a #{b}})))))))

(defcheck solution-e4fc61be
  (fn [func domain]
    (->> (for [v domain]
           [(func v) v])
      (reduce (fn [m [k v]] (update-in m [k] conj v)) {})
      vals
      (map set)
      set)))

(defcheck solution-e51c035a
  (fn [f coll]
    (reduce #(conj %1 (set (val %2))) #{} (group-by f coll))))

(defcheck solution-e53ae0bb
  (fn [f xs]
    (set (map set (vals (group-by f xs))))))

(defcheck solution-e55fd372
  (fn f [op xs]
    (letfn [(proc [m k v] (assoc m k (conj (get m k #{}) v)))]
      (->>
        (reduce #(proc %1 (op %2) %2) {} xs)
        (vals)
        (set)
        )
      )))

(defcheck solution-e5bf0aab
  (fn [f s]
    (->> s
      (group-by f)
      vals
      (map set)
      set)))

(defcheck solution-e5ceb72a
  (fn [f mm]
    (->> mm
      (group-by f)
      (vals)
      (map set)
      (into #{})
      )
    ))

(defcheck solution-e5cf9ca8
  (fn [f col] (set (map set (vals (group-by f col))))))

(defcheck solution-e5d5306a
  (fn [f domain]
    (->> (group-by f domain)
      vals
      (map set)
      set)))

(defcheck solution-e6025b44
  #(->> %2 (group-by %) (vals) (map set) (set)))

(defcheck solution-e6e7b2
  (fn equiv [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-e6eb91bc
  (fn equiv-classes [func domain]
    (set
      (map set
        (vals (reduce (fn [accum item]
                        (merge-with concat
                          accum
                          {(func item) [item]}))
                {}
                domain))))))

(defcheck solution-e77e88d7
  (fn equi [f ctx]
    (set (map set (vals (group-by f ctx))))))

(defcheck solution-e790d2a2
  (fn [f s]
    (set (vals (reduce
                 (fn [a b]
                   (let [r (f b)
                         un clojure.set/union]
                     (if (a r)
                       (assoc a r (un (a r) #{b}))
                       (assoc a r #{b}))))
                 {} s)))))

(defcheck solution-e7c6226b
  (fn [f m]
    (set (map set (vals (group-by f m))))))

(defcheck solution-e8161457
  (fn [f ds]
    (->> ds
      (group-by f)
      (vals)
      (map set)
      (into #{}))))

(defcheck solution-e83f25ce
  #(->> (group-by %1 %2)
     vals
     (map set)
     set))

(defcheck solution-e870d2e9
  (fn [f aset]
    (set (map set (vals (group-by f aset))))))

(defcheck solution-e8f429be
  (fn equivalence [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-e931515
  (fn quot-space
    [f s] {:pre [(ifn? f), (set? s)]}
    (->> (group-by f s)
      vals
      (map set)
      set)))

(defcheck solution-e9824bce
  (fn equivalence-classes [f s]
    (reduce (fn [acc [k v]] (conj acc (set v))) #{} (group-by f s))))

(defcheck solution-e9962a61
  (fn [f D]
    (->> (group-by f D) vals (map set) set)))

(defcheck solution-e9a52ad6
  (fn [f s]
    (into #{} ((apply juxt (for [i s] (fn [l] (into #{} (filter #(= (f i) (f %)) l))))) s))))

(defcheck solution-e9dc08fc
  (fn [f v]
    (set (map #(into #{} %) (vals (group-by f v))))))

(defcheck solution-e9f87f6b
  (fn [f s]
    (->> s
      (group-by f)
      vals
      (map set)
      set)))

(defcheck solution-ea2802b0
  (fn [f xs] (->> xs (group-by f) vals (map set) set)))

(defcheck solution-ea435dae
  (fn [f args]
    (->> (map (juxt f identity) args)
      (reduce (fn [acc [k v]]
                (let [existing (get acc k #{})]
                  (assoc acc k (conj existing v))))
        {})
      vals
      (into #{}))))

(defcheck solution-eacc3d2f
  (fn equiv-classes [f D]
    (set (map set (vals (group-by f D))))))

(defcheck solution-eb70588c
  (fn der [f input]
    (into #{}
      (for [[k v]
            (reduce #(let [result (f %2)]
                       (assoc %1 result (conj (get %1 result) %2)))
              {}
              input)]
        (into #{} v)))))

(defcheck solution-eb7a1b1
  (fn [f c] (set (map #(set %1) (vals (group-by f c))))))

(defcheck solution-ebaca037
  (fn equivalenceX [f x] ((fn equivalenceRec[f x ss]
                            (if (empty? x)
                              ss
                              (let
                               [
                                n (first x)
                                s (
                                   (fn equivalentTo [f x n]
                                     (let [fn (f n)]
                                       (conj (set (filter #(= (f %) fn) x)) n)
                                       )
                                     ) f (rest x) n)
                                newX (remove s x)
                                newSS (conj ss s)
                                ]
                                (equivalenceRec f newX newSS)
                                )
                              )
                            )
                          f x #{})))

(defcheck solution-ec19d587
  (fn [f d]
    (let [mapped (map (fn [i] {:in i :out (f i)}) d)
          grouped (group-by :out mapped)]
      (into #{} (map #(into #{} (map :in %)) (vals grouped))))))

(defcheck solution-ec38e98c
  (fn [f s] (into #{} (map set (vals (group-by f s))))))

(defcheck solution-ec8f9efc
  (fn [f D] (into #{} (map set (vals (group-by f D))))))

(defcheck solution-ecbb2b64
  (fn [f D]
    (->> (group-by f D)
      (vals)
      (map set)
      set)))

(defcheck solution-ed214feb
  (fn [f x] ((comp set vals) (reduce (fn [m e] (let [k (f e)] (assoc m k (conj (m k #{}) e)))) {} x))))

(defcheck solution-ed4582f1
  (fn eqvcl [mod items]
    (set (map (comp set second) (group-by mod items)))))

(defcheck solution-ed63e471
  (fn [f dom]
    (loop [remaining dom rev (hash-map)]
      (if (empty? remaining)
        (apply hash-set (for [kv rev] (second kv)))
        (if (contains? rev (f (first remaining)))
          (recur (rest remaining) (update-in rev [(f (first remaining))] conj (first remaining)))
          (recur (rest remaining) (conj rev [(f (first remaining)) (hash-set (first remaining))])))))))

(defcheck solution-ee232d41
  (fn eq-classes [f xs]
    (set (map set (vals (group-by f xs))))))

(defcheck solution-ee26d2bd
  (fn [f d] (->> d (group-by f) (vals) (map set) (set))))

(defcheck solution-ee970291
  (fn [f coll]
    (set (vals (reduce
                 (fn [result val]
                   (let [key (f val)]
                     (assoc result key (conj (get result key #{}) val))))
                 {}
                 coll)))))

(defcheck solution-ef50407e
  (fn [f D] (set (map (comp set val) (group-by #(f %) D)))))

(defcheck solution-f01b15d3
  (fn [f d] (set (map #(set (val %)) (group-by f d)))))

(defcheck solution-f01c57fc
  (fn ec [f s]
    (set(for [r (set(map f s))]
          (set(filter #(= r (f %)) s))))))

(defcheck solution-f0427afa
  (fn [x y]
    (set (map set (vals (group-by x y))))))

(defcheck solution-f0db4956
  (fn [f d]
    (set (map set (vals (group-by f d))))))

(defcheck solution-f1ed7b28
  (fn [f xs]
    (->> (for [x xs] {(f x) #{x}})
      (apply merge-with (comp set concat))
      vals set)))

(defcheck solution-f207b55
  (fn [f s]
    ((comp set (partial map set) vals group-by) f s)))

(defcheck solution-f34a8e00
  (fn [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-f37e463b
  (fn  [f d]
    (->> (group-by f d)
      vals
      (map set)
      set)))

(defcheck solution-f3ed7361
  (fn eq-classes [func domain]
    (set
      (map set (vals (group-by func domain))))))

(defcheck solution-f42ac1ca
  (fn equa [f x]
    (let [m (apply conj {} (map #(vector % (f %)) x))]
      (set (map #(set (filter (fn [x] (= % (m x))) (keys m))) (vals m))))))

(defcheck solution-f44f8ed0
  (fn [f coll]
    "Returns D"
    (set (map set (vals (group-by f coll))))))

(defcheck solution-f4822087
  (fn eqr [f d]
    (set (vals (reduce
                 (fn [m e]
                   (let [k (f e)
                         v (m k)]
                     (assoc m k (if (nil? v) #{e} (conj v e)))))
                 {} d)))))

(defcheck solution-f4ed9b77
  (fn er [f coll]
    (into #{} (map set (vals (group-by f coll))))))

(defcheck solution-f53ebb06
  (fn [f s]
    (set (map set (vals (group-by f s))))))

(defcheck solution-f54f98f4
  (fn eq
    [f d]
    (->> (group-by f d)
      vals
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-f5691cd6
  (fn equivalences
    ([f s]
     (equivalences f s #{}))
    ([f s acc]
     (if (empty? s)
       acc
       (let [a (first s)
             a' (f a)
             eqs (->> (filter #(= a' (f %)) (disj s a))
                   (into #{a}))
             s' (apply disj s eqs)]
         (if (empty? eqs)
           (equivalences f s' (conj acc #{a}))
           (equivalences f s' (conj acc eqs))))))))

(defcheck solution-f5de756c
  (fn [f s]
    (->>
      (group-by f s)
      vals
      (map set)
      set
      )))

(defcheck solution-f67c96ed
  (fn [f D]
    (->> D
      (group-by f)
      (map (comp set last))
      (set)
      )
    ))

(defcheck solution-f6f96301
  (fn n98 [f s]
    (set (map #(set (filter (fn [x] (= (f x) %)) s)) (set (map f s))))))

(defcheck solution-f71ab61e
  #(set (map set (vals (apply group-by %&)))))

(defcheck solution-f76f36fc
  (fn [f d]
    (->>
      d
      vec
      (sort-by f)
      (partition-by f)
      (map #(set %) )
      set)))

(defcheck solution-f79990b
  (fn [f D]
    (set (map set (vals
                    (loop [ D D
                           E {} ]
                      (cond
                        (not D) E
                        (get E (f (first D))) (recur (next D) (assoc E (f (first D)) (conj (get E (f (first D))) (first D))))
                        :else (recur (next D) (assoc E (f (first D)) [(first D)])))))))))

(defcheck solution-f7cb0c03
  (fn [f s] (set (map (fn [[k v]] (set v)) (group-by f s)))))

(defcheck solution-f8d77094
  (fn equivClass [f coll]
    (let [fval (set (map f coll))]
      (reduce (fn [result elem]
                (conj result (set (filter #(= elem (f  %)) coll)))) #{} fval))))

(defcheck solution-f92fa097
  #(->> (group-by %1 %2)
     (map last)
     (map set)
     (set)))

(defcheck solution-f93ed49b
  (fn e [f s]
    (if (empty? s)
      #{}
      (let [h (first s)
            t (rest s)
            v (f h)
            c (apply conj #{} h
                (filter #(= v (f %)) t))
            r (remove #(some (partial = %) c) t)]
        (conj (e f r) c)))))

(defcheck solution-f9b8c271
  (fn [f s]
    (set (map set (partition-by f (sort-by f s))))))

(defcheck solution-fa1f4a0e
  (comp set #(map set %) vals group-by))

(defcheck solution-fa38e2f9
  (fn [a b]
    (set (map set (vals (group-by a b))))))

(defcheck solution-fa3a0212
  (fn eq [f s]
    (->> s
      (group-by f)
      (vals)
      (map set)
      (set))))

(defcheck solution-fa7e1084
  (fn ec [f D]
    (->> (group-by f D) vals (map set) set)))

(defcheck solution-fb495a3a
  (fn equivilance-classes
    [f coll]
    (set (map set (vals (group-by f coll))))))

(defcheck solution-fb7f1b3a
  (fn [f coll] (into #{}(map (fn[e] (apply hash-set e)) (vals (group-by f coll))))))

(defcheck solution-fc50fca5
  (fn [f is]
    (-> (reduce (fn [m i]
                  (update-in m [(f i)]
                    #(conj (set %) i)))
          {}
          is)
      vals
      set)))

(defcheck solution-fdb25df4
  (fn [f x] (set (map set (vals (group-by #(f %) x))))))

(defcheck solution-fdba861d
  (fn equiv-class [f d]
    (into #{} (map #(into #{} %) (vals (reduce (fn [map val]
                                                 (let [res (f val)]
                                                   (update-in map [res] conj val))) {} d))))))

(defcheck solution-fdca1e0f
  (fn [r s]
    (set (vals (reduce #(update-in % [(r %2)] (fnil conj #{}) %2) {} s)))))

(defcheck solution-fedd8982
  (fn equivalence-classes [f coll]
    (into #{} (map set (vals (group-by f coll))))))

(defcheck solution-ff2ee1ee
  (fn [f xs]
    (->>
      xs

      (map
        (fn [x]
          [
           x
           (f x)
           ]
          )
        )

      (group-by second)

      (map
        (fn [[_ pairs]]
          pairs

          (set
            (map first pairs)
            )
          )
        )

      (set)
      )
    ))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-98))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

