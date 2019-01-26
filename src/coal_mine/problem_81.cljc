(ns coal-mine.problem-81
  (:require [coal-mine.checks :refer [defcheck-81] :rename {defcheck-81 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10b61a0e
  (fn
    [set1 set2]
    (clojure.set/difference (clojure.set/union set1 set2) ((fn
                                                             [set1 set2]
                                                             (clojure.set/union (clojure.set/difference set1 set2) (clojure.set/difference set2 set1))) set1 set2))))

(defcheck solution-11196ec0
  (fn [l r]
    (if (> (count r) (count l))
      (recur r l)
      (reduce
        (fn [a b]
          (if (contains? r b)
            a
            (disj a b)))
        l l))))

(defcheck solution-125e928e
  (fn [a b] (set (reduce #(if (contains? b %2) (conj %1 %2) %1) #{} a))))

(defcheck solution-126b5ba2
  (fn my-intersect
    [seta setb]
    (set (filter (partial contains? seta) setb))))

(defcheck solution-12888d1d
  (fn [a b]
    (set
      (filter #(contains? b %) a))))

(defcheck solution-132801f
  (fn [set1 set2]
    (set (mapcat (fn [s1 s2]
                   (cond
                     (and (set2 s1) (set1 s2)) [s1 s2]
                     (set2 s1) [s1]
                     (set1 s2) [s2]
                     :else []))
           set1
           set2))))

(defcheck solution-13878003
  (fn [& ss]
    (->> ss
      (apply concat)
      (group-by identity)
      (filter #(-> % val count (> 1)))
      keys
      (into #{}))))

(defcheck solution-139af5a3
  (fn intr [s1 s2]
    (set (for [x s1
               :when (not (nil? (s2 x)))]
           x))))

(defcheck solution-1466682a
  (comp set filter))

(defcheck solution-147ca83
  #(reduce (fn [acc el] (if (%1 el) (conj acc el) acc)) #{} %2))

(defcheck solution-14febcb0
  #(let [u clojure.set/union d clojure.set/difference] (apply disj (u % %2) (u (d % %2) (d %2 %)))))

(defcheck solution-1560bd93
  (fn custom-intersection
    [set1 set2]
    (let [map1 (group-by identity set1)
          map2 (group-by identity set2)
          map3 (merge-with concat map1 map2)
          result (set (map first (vals (filter (fn [[k v]] (second v)) map3))))]
      result)))

(defcheck solution-1697b1
  (fn [a b]
    (let [a+b (apply conj a b)
          a-b (apply disj a b)
          b-a (apply disj b a)]
      (apply disj (apply disj a+b a-b) b-a)
      )))

(defcheck solution-16c4bcae
  (fn [l1 l2]
    (reduce
      (fn [x y]
        (if (contains? l2 y)
          (conj x y)
          x
          )
        )
      #{}
      l1
      )


    ))

(defcheck solution-16df2a51
  (fn [a b]
    (set (filter #(and (contains? a %) (contains? b %)) (into a b)))))

(defcheck solution-1706740f
  (fn si [xs ys]
    (clojure.set/select #(some #{%} ys) xs)))

(defcheck solution-17ab4c91
  (fn [a b]
    (into #{} (filter #(contains? b %) a))
    ))

(defcheck solution-18ae4bc6
  (fn [a b]
    (reduce into #{}
      (for [x a
            y b
            :let [z #{x}]
            :when (= x y)]
        z))))

(defcheck solution-18c9d3ff
  (fn i [s1 s2]
    (set (filter #(contains? s1 %) s2))))

(defcheck solution-18f9a8f7
  #(set (filter  %1 %2)))

(defcheck solution-190df49d
  (fn my-inter [& sets]
    (set
      (reduce
        (fn [s1 s2]
          (filter (partial contains? s2) s1))
        sets))))

(defcheck solution-1a656500
  (fn inter [a b]
    (->> a
      (filter #(b %))
      (set))))

(defcheck solution-1afbc6bc
  (fn my-intersection [set1 set2] (set (for [x set1 :when (set2 x)] x))))

(defcheck solution-1b83adea
  (fn [s1 s2]
    (reduce #(if (get s1 %2) (conj %1 %2) %1) #{} s2)))

(defcheck solution-1be4d7df
  (fn set-intersect
    ([x y]
     (set-intersect x y #{}))
    ([x y z]
     (if (= 0 (count x))
       z
       (if (contains? y (first x))
         (recur (rest x) y (conj z (first x)))
         (recur (rest x) y z))))))

(defcheck solution-1bfe9014
  (fn [a b](reduce #(if(contains? a %2)(conj %1 %2) %1 ) #{} b)))

(defcheck solution-1c60615b
  (fn [set1 set2]
    (reduce (fn [acc elem] (if (contains? set2 elem) (conj acc elem) acc) )
      #{} set1)))

(defcheck solution-1c7ed8f0
  (fn [a b]
    (reduce
      (fn [result x]
        (if (a x)
          (conj result x)
          result))
      #{} b)))

(defcheck solution-1c81086d
  (fn [s t]
    (clojure.set/difference
      (clojure.set/difference
        (clojure.set/union s t)
        (clojure.set/difference t s))
      (clojure.set/difference s t))))

(defcheck solution-1cf17057
  (fn [s1 s2]
    (apply hash-set (filter (partial contains? s1) s2))))

(defcheck solution-1d53c684
  #(set (remove nil? (map (fn [n] (if (contains? %2 n)
                                    n
                                    nil)) %1))))

(defcheck solution-1eff203f
  (fn [a b]
    (set (filter b a))))

(defcheck solution-1f3cc6b9
  (fn [ms ns]
    (letfn [(go [xs ys is]
              (if (empty? xs)
                is
                (let [x (first xs)
                      xs' (disj xs x)]
                  (if (contains? ys x)
                    (go xs' ys (conj is x))
                    (go xs' ys is)))))]
      (go ms ns #{}))))

(defcheck solution-1f3f3b98
  (fn myInter[s1 s2]
    (set (filter
           #(contains? s1 %)
           s2))))

(defcheck solution-1f6c9e06
  (fn [a b] (into #{} (filter a b))))

(defcheck solution-1f839b63
  (fn [a b] (set (filter #(b %) a))))

(defcheck solution-1fe23c40
  (fn [a b]
    (reduce #(if (contains? b %2) (conj %1 %2) %1) #{} a)))

(defcheck solution-210d63f6
  (fn [a b]
    (set (map first (filter #(> (count (last %)) 1) (group-by identity (concat a b)))))))

(defcheck solution-213bcbc6
  (fn inv[pcol1 pcol2]
    (set (for [l1 pcol1,l2 pcol2 :when (= l1 l2)]
           l1
           ) )
    ))

(defcheck solution-2168b209
  (fn [x y](set(filter #(contains? y %) x))))

(defcheck solution-217af6cf
  #(set (filter %1 %2)))

(defcheck solution-21ad1974
  (fn intersection- [s1 s2]
    "81. Write a function which returns the intersection of two sets."
    (set (filter (complement nil?) (map #(s2 %) s1)))))

(defcheck solution-21ff521f
  (fn [s1 s2] (set (filter (partial contains? s1) s2))))

(defcheck solution-22059b63
  #(set (filter (fn [x] (contains? %2 x)) %1)))

(defcheck solution-222bda8c
  (fn my-intersection [a b]
    (set (filter #(b %) a))))

(defcheck solution-2351903f
  (fn
    ([a] a)
    ([a b]
     (if (< (count b) (count a))
       (recur b a)
       (reduce
         (fn [res item]
           (if (contains? b item)
             res
             (disj res item)))
         a a)))))

(defcheck solution-23572d80
  #(reduce disj % (reduce disj % %2)))

(defcheck solution-2384e371
  #(set (filter (apply comp %&) (apply concat %&))))

(defcheck solution-240f0140
  (fn [a b]
    (let [u clojure.set/union
          d clojure.set/difference]
      (d (u a b)
        (d a b)
        (d b a)))))

(defcheck solution-242ad8ef
  (fn intersec [s1 s2]
    (set (mapcat #(if (contains? s2 %) (list %) ()) s1))))

(defcheck solution-2501d1ed
  (fn [s1 s2] (set (reduce #(if (s2 %2) (conj %1 %2) %1) #{} s1))))

(defcheck solution-2533266f
  (fn [s1 s2] (set (for [x (concat s1 s2) :when (and (s1 x) (s2 x))] x))))

(defcheck solution-25a52ebe
  (fn f [x & l]
    (set (reduce #(remove nil? (map (fn[a] (get % a)) %2)) x l))))

(defcheck solution-268a9b5
  (fn my-intersection
    [s1 s2]
    (set (filter (partial contains? s2) s1))))

(defcheck solution-268ca756
  (fn [& sets]
    (reduce (fn [s t]
              (->> t
                (clojure.set/difference s)
                (clojure.set/difference s))) sets)))

(defcheck solution-26dd68e8
  (fn
    [set-a set-b]
    (->>
      (map #(set-b %) set-a)
      (filter (comp not nil?))
      (into #{}))))

(defcheck solution-2742a35d
  (fn [a b] (set (for [i a :when (contains? b i)] i))))

(defcheck solution-2748f403
  (fn inters [c1 c2]
    (set (filter #(contains? c1 %) c2))))

(defcheck solution-275bc40c
  (fn [& sets]
    (loop [first-set (first sets) other-sets (rest sets)]
      (if (empty? other-sets)
        first-set
        (recur (set (filter first-set (first other-sets)))
          (rest other-sets))))))

(defcheck solution-28453c6c
  (fn [s t] (->> (map s t) (remove nil?) set )))

(defcheck solution-28afdf2f
  (fn mySetIntersection
    [set1 set2]
    ((comp set filter) set1 set2)))

(defcheck solution-28d6b3e
  (fn [xs ys]
    (set (filter xs ys))))

(defcheck solution-293bb2a7
  (fn myint [a b]
    (let [inter (into #{} (map #(some #{%} b) a))]
      (if (and (some nil? a) (some nil? b))
        inter
        (set (filter #(not (nil? %)) inter))))))

(defcheck solution-2a16d153
  #(set (remove (clojure.set/difference %1 %2) %1)))

(defcheck solution-2b384ad1
  #(set( filter (comp not nil?) (map %2 %1))))

(defcheck solution-2b9fdb89
  (fn [as bs] (into #{} (filter (partial contains? bs) as))))

(defcheck solution-2be6f6c5
  #(set (for [a %1 b %2 :when (= a b)] a)))

(defcheck solution-2c11c787
  (fn [a b]
    (set(filter #(contains? b %) a))))

(defcheck solution-2c54ca8d
  (fn ! [x y]
    (if (empty? x) #{}
                   (let [f (first x) r (! (rest x) y)]
                     (if (contains? y f)
                       (conj r f)
                       r)))))

(defcheck solution-2ca712fe
  (fn [x y]
    (set (keys (filter #(> (count (last %)) 1) (group-by identity ( concat x y)))))))

(defcheck solution-2cb193ae
  #(reduce (fn [acc x] (if (some #{x} %) (conj acc x) acc ) ) #{} %2))

(defcheck solution-2ce9c746
  (fn [x y]
    (apply hash-set (filter #(y %) x))))

(defcheck solution-2e5abd40
  (fn [& sets]
    (->> sets
      (apply concat)
      frequencies
      (filter #(= (count sets) (second %)))
      (map first)
      set)))

(defcheck solution-2e79e1d3
  (fn [set1 set2]
    (reduce
      (fn [output item]
        (if (contains? set2 item)
          (conj output item)
          output))
      #{}
      set1)))

(defcheck solution-2f2f6549
  (fn [s1 s2]
    (into #{} (filter #(contains? s1 %) s2))))

(defcheck solution-2f36d827
  (fn [s1 s2]
    (reduce (fn [acc e] (if (s1 e) (conj acc e) acc)) #{} s2)))

(defcheck solution-2f8ba50a
  (fn [a b]
    (set (filter #(get b %) a))))

(defcheck solution-2f8c8f92
  (fn [s1 s2] (reduce #(if (contains? s2 %2)
                         (conj % %2)
                         %)
                #{} s1)))

(defcheck solution-2fe3dbf6
  #(disj (set (map % %2)) nil))

(defcheck solution-30ea6b22
  (fn [s1 s2] (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))))

(defcheck solution-315442f7
  (fn [x y] (reduce #(if (contains? x %2) (conj %1 %2) %1) #{}  y)))

(defcheck solution-31d32d54
  #(clojure.set/difference % (clojure.set/difference % %2)))

(defcheck solution-31f4e084
  (fn [s1 s2] (set (reduce (fn [acc v] (if (contains? s2 v) (conj acc v) acc)) #{} s1))))

(defcheck solution-31fc9e46
  (fn [c1 c2] (set (for [v1 c1 v2 c2 :when (= v1 v2)] v1
                                                      ))))

(defcheck solution-3237a439
  (fn [xs ys] (set (filter #(contains? ys %) xs))))

(defcheck solution-323cd222
  (fn [xs ys]
    (set (filter xs ys))))

(defcheck solution-3268ef3f
  (fn [s1 s2] (set (filter s2 s1))))

(defcheck solution-32989cde
  (fn [r s] (set (filter s r))))

(defcheck solution-3409e73a
  (fn [s1 s2]
    (set (filter #(contains? s2 %) s1))))

(defcheck solution-343def9d
  (fn [s & sets]
    (reduce (fn [acc e] (if (every? #(%1 e) sets) (conj acc e) acc)) #{} s)))

(defcheck solution-355d3eb0
  (fn intersection2 [a b]
    (set (for [x a :when (contains? b x)]
           x))
    ))

(defcheck solution-35798984
  #(clojure.set/difference %1 (clojure.set/difference %1 %2)))

(defcheck solution-376dbd5
  #(into #{} (filter (partial contains? %1) %2)))

(defcheck solution-377f1d05
  #(set (for [x % y %2 :when (= x y)] x)))

(defcheck solution-37f0797e
  (fn  [a b]
    (set (reduce #(if (nil? (a %2)) %1  (cons %2 %1))  () b))))

(defcheck solution-3837b858
  (fn [a b]
    (clojure.set/difference a
      (clojure.set/difference a b))))

(defcheck solution-383c61e5
  (fn [A B]
    (set (filter (fn [x] (contains? A x)) B))))

(defcheck solution-3874428c
  (fn [a b] (set (filter (fn [x] (get b x)) a))))

(defcheck solution-38ae5593
  (fn [& colls] (apply sorted-set(flatten(filter #(=(count %) (count colls))(partition-by identity(sort(apply concat colls))))))))

(defcheck solution-38c804ad
  #(apply disj % (apply disj % %2)))

(defcheck solution-3a07cf30
  #(letfn [(worker [x y s]
             (if (empty? x)
               s
               (recur (rest x) y (if (get y (first x)) (conj s (first x)) s))))]
     (worker %1 %2 #{})))

(defcheck solution-3a0a1792
  (fn [a b]
    (set (filter #(contains? a %) b))))

(defcheck solution-3a56173
  (fn [a b] (into #{} (filter b a))))

(defcheck solution-3a7dae66
  (fn [& args] (set (reduce filter args))))

(defcheck solution-3ad8add5
  #(into #{} (filter %2 %1)))

(defcheck solution-3af86d0a
  (fn [a b]
    (loop [a a acc #{}]
      (if (empty? a)
        acc
        (recur (rest a) (if (b (first a)) (conj acc (first a)) acc))))))

(defcheck solution-3b06eef2
  (fn [& xxs]
    (into #{} (map first (filter #(= (second %) (count xxs)) (frequencies (mapcat (partial into []) xxs)))))))

(defcheck solution-3b1bd2e8
  (fn [a b]   (set (filter #(not= % nil) (map    #(b %)  a)) ) ))

(defcheck solution-3b646770
  (fn inter [l1 l2]
    (set(for [x l1 y l2 :when (= x y)] x))
    ))

(defcheck solution-3b9e0573
  (fn [x y]
    (set (filter #(not (nil? (get x %))) y))))

(defcheck solution-3bc78527
  (partial
    #(if (empty? %2) %1
                     (recur
                       (if
                        (contains? %3 (first %2)) (conj %1 (first %2)) %1)
                       (rest %2) %3)) #{}))

(defcheck solution-3bfd4d82
  (fn[x y] (set (filter #(some #{%} x) y))))

(defcheck solution-3bfe5fb
  (fn x-sect
    [& sets]
    (set (reduce #(filter %2 %1) sets))))

(defcheck solution-3c0a42c9
  (fn
    [s1 s2]
    (reduce #(if (contains? s1 %2)
               (conj %1 %2)
               %1)
      #{}
      s2)))

(defcheck solution-3c11e7ef
  (fn [a b]
    (set (filter #(contains? b %) a))))

(defcheck solution-3c4476b2
  (fn [set1 set2]
    (into #{}
      (for [ele set1
            :when (contains? set2 ele)]
        ele
        )
      )
    ))

(defcheck solution-3c5a003
  (fn newintersection [x y]
    (loop [counter (- (count x) 1), result (vec x)]
      (if (>= counter 0)
        (if (some (set (list (get result counter))) y)
          (recur (dec counter) result)
          (recur (dec counter) (vec (disj (set result) (get result counter)))))
        (set result)))))

(defcheck solution-3c9cae07
  (fn [s1 s2]
    (loop [ret #{}
           curr s2]
      (if (empty? curr)
        ret
        (if (contains? s1 (first curr))
          (recur (conj ret (first curr)) (rest curr))
          (recur ret (rest curr)))))))

(defcheck solution-3ca1ad0c
  (fn [a b] (set (filter (every-pred a b) (into a b)))))

(defcheck solution-3d03b4fa
  #(set(filter %2 %1)))

(defcheck solution-3d3d7cd8
  #(set (for [c1 %1 c2 %2 :when (= c1 c2)] c1)))

(defcheck solution-3d81342e
  (fn [s1 s2]
    (set (map #(first %)
           (filter #(= (first %) (last %))
             (for [t1 s1
                   t2 s2]
               [t1 t2]))))))

(defcheck solution-3dc91146
  (fn [c1 c2] (set (for [x c1 y c2 :let [z x] :when (= x y)] z))))

(defcheck solution-3eec78cf
  (fn [a b] (set (filter #(get b %) a))))

(defcheck solution-3f52e08f
  (fn [s1 s2]
    (set (filter #(some s2 [%]) s1))))

(defcheck solution-400b206c
  (fn [set1 set2]
    (set (filter #(set1 %) set2))))

(defcheck solution-41442d98
  (fn [a b] (set (map first (filter #(> (last %) 1)
                              (frequencies (mapcat vec (list a b))))))))

(defcheck solution-4162f580
  (fn [xs ys]
    (set (filter (partial contains? ys) xs))
    ))

(defcheck solution-41c1af
  (comp set keep))

(defcheck solution-4259dd7f
  #(->>(map %1 %2)
     (remove nil?)
     (set)))

(defcheck solution-42f36b8a
  (fn isect
    [s1 s2]
    (set (filter s2 (filter s1 s2)))
    ))

(defcheck solution-438a759d
  (fn set-intersection[s1 s2]
    (set (for [x s1 y s2 :when (= x y)] x))))

(defcheck solution-43f60836
  #(set (for [x %1
              :when (contains? %2 x)]
          x)))

(defcheck solution-441078f3
  (fn [r s] (reduce #(if (contains? s %2) (conj %1 %2) %1) #{} r)))

(defcheck solution-4463aa7a
  (fn [s s'] (disj (reduce #(conj %1 (get s %2)) #{} s') nil)))

(defcheck solution-449ee22
  (fn [a b] (set (filter #(not (nil? (a %1))) b))))

(defcheck solution-44d5fcef
  #(set (for [z (into %1 %2) :when (= (get %1 z) (get %2 z)) ] z)))

(defcheck solution-453b24b3
  #(loop [set-1 (seq %1)
          set-2 %2
          set-r #{}]
     (if (empty? set-1)
       set-r
       (recur (rest set-1)
         set-2
         (if (contains? set-2 (first set-1))
           (conj set-r (first set-1))
           set-r)))))

(defcheck solution-45dd5b5c
  #(reduce (fn [s x] (if (contains? %2 x)  (conj s x) s)) #{} %))

(defcheck solution-4646a23e
  (fn [s1 s2] (into #{} (filter s1 s2))))

(defcheck solution-47514fda
  (fn [s1, s2]
    (apply hash-set (filter #(s1 %1) s2))))

(defcheck solution-486865f3
  (fn [set1 set2]
    (into #{} (filter (partial contains? set1) set2))))

(defcheck solution-48c7765
  (fn [ss ts]
    (set
      (for [s ss t ts :when (= 1 (count (set [s t])))]
        s))))

(defcheck solution-48d7128e
  #(into #{} (remove (apply disj % %2) %)))

(defcheck solution-48f3c831
  (fn inters [x,y]
    (clojure.set/select #(contains? y %) x)))

(defcheck solution-49d51baa
  #(into #{} (for [e %1 :when (contains? %2 e)]  e )  ))

(defcheck solution-4a38a177
  (fn [r1 r2]
    (into #{} (filter r1 r2))))

(defcheck solution-4a94e342
  (fn it [& sets]
    (set
      (filter (fn [k] (every? #(contains? % k) sets))
        (set (apply concat sets))))))

(defcheck solution-4af627b3
  #(set (for [a %1 :when (contains? %2 a)] a)))

(defcheck solution-4b87809a
  (fn [s & ss]
    (letfn [(inter [a b]
              (reduce (fn [c el]
                        (if (contains? b el)
                          (conj c el)
                          c))
                #{}
                a))]
      (reduce inter s ss))))

(defcheck solution-4c55147c
  (fn [s1 s2]
    (set (filter #(s2 %)
           s1))))

(defcheck solution-4c810b97
  #(into #{} (for [i % :when (%2 i)] i)))

(defcheck solution-4dd30ef9
  #(set (map first
          (for [x % y %2
                :when (= x y)]
            [x y]))))

(defcheck solution-4e888be6
  (fn [s1 s2] (set (filter (complement nil?) (map #(s1 %) s2)))))

(defcheck solution-4f4ae7ac
  (fn f [& s]
    (into #{} (reduce #(for [i %2
                             :when (contains? %1 i)]
                         i) s))))

(defcheck solution-4fdb7e6d
  (fn [s t] (set (map first (filter #(= (first %) (fnext %)) (for [a s b t] [a b] ))))))

(defcheck solution-4ff19f0b
  (fn my-intersection
    [s1 s2]
    (set (filter #(get s2 %) s1))))

(defcheck solution-500578a9
  (fn [x y]
    (set (filter #(get y %) x))))

(defcheck solution-508fa9b8
  (fn inter [s1 s2]
    (into #{} (map first (for [x s1, y s2 :when (= x y)] [x y])))))

(defcheck solution-50f81750
  (fn interX [a b] (into #{} (filter #(contains? b %) a))))

(defcheck solution-51020e2b
  (fn [set1 set2]
    (cond (empty? set1) set2
          (empty? set2 ) set1
          :else
          (set (reduce into (map #(if (get set2 %) #{%} nil) set1 )))


          )
    ))

(defcheck solution-514fc4c9
  (fn i [p q] (clojure.set/select (partial contains? p) q)))

(defcheck solution-51503df
  (fn [a b]
    (set (for [x a :when (contains? b x)] x))))

(defcheck solution-52034a15
  (fn i [a b] (set (filter #(contains? a %) b))))

(defcheck solution-52124026
  (fn my-intersection [a b]
    (set
      (if (< (count a) (count b))
        (filter a b)
        (filter b a)))))

(defcheck solution-52a40cb9
  (fn [s1 s2] (set (for [x s1 :when (s2 x)] x))))

(defcheck solution-52be2177
  (fn [a b]
    (loop [s (seq a) r #{}]
      (cond
        (empty? s)
        r
        (contains? b (first s))
        (recur (rest s) (conj r (first s)))
        true
        (recur (rest s) r)))))

(defcheck solution-5375444
  (fn [s1 s2]
    (into #{} (filter (partial contains? s1) s2))))

(defcheck solution-53b812e6
  (fn [s1 s2]
    (loop [news #{} tmp1 s1]
      (if (empty? tmp1)
        news
        (if (contains? s2 (first tmp1))
          (recur (conj news (first tmp1)) (rest tmp1))
          (recur news (rest tmp1)))))))

(defcheck solution-55c08d63
  (fn [x y] (set (for [xi x yi y :when (= xi yi)] xi))))

(defcheck solution-55f5ca37
  #(let [
         diff clojure.set/difference
         union clojure.set/union
         ]
     (diff (union %1 %2) (union (diff %1 %2) (diff %2 %1)))))

(defcheck solution-5626512c
  (fn this [v1 v2]
    (set (filter #(contains? v2 %) v1))))

(defcheck solution-5648fc0b
  (fn [setA setB]
    (set (for [x setA y setB :when (= x y)] x))
    ))

(defcheck solution-56a3317c
  #(->> (clojure.set/difference %1 %2) (clojure.set/difference %1)))

(defcheck solution-56c229c6
  (fn find-intersect
    [xs ys] (reduce #(if (get ys %2) (conj % %2) %) #{} xs)))

(defcheck solution-572981f4
  (fn [s1 s2]
    (reduce
      (fn [acc v] (if (contains? s2 v) (conj acc v) acc)) #{} s1)))

(defcheck solution-573309d7
  (fn [xs ys]
    (reduce #(if (contains? ys %2) (conj %1 %2) %1) #{} xs)))

(defcheck solution-573eafe1
  #(-> % (filter %2) set))

(defcheck solution-58d49c09
  #(reduce
     (fn [result item]
       (if (contains? %2 item)
         result
         (disj result item)))
     %1 %1))

(defcheck solution-5a00bf9f
  (comp (partial into #{}) filter))

(defcheck solution-5a572689
  #(disj (reduce (fn [acc e] (conj acc (%2 e))) #{} %) nil))

(defcheck solution-5b56c775
  (fn [c1 c2]
    (set (filter c1 c2))))

(defcheck solution-5b5ce5b8
  (fn [s1 s2]
    (let [u (clojure.set/union s1 s2)
          d (clojure.set/difference u s1)]
      (clojure.set/difference s2 d))))

(defcheck solution-5b93399f
  (fn [s1 s2]
    (into #{}
      (concat (filter #(contains? s1 %) s2)
              (filter #(contains? s2 %) s1)))))

(defcheck solution-5bfdcfc8
  (fn [s1 s2] (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-5c5b4b5f
  (fn [s1 s2]
    (reduce #(if (not (contains? s2 %2)) (disj %1 %2) %1) s1 s1)))

(defcheck solution-5c754cb9
  (fn [x y] (set (remove (fn [a] (not (contains? x a))) y))))

(defcheck solution-5d2f6755
  (fn my-intersection [& s]
    (set (reduce (fn _ [a b] (
                               reduce #(if (contains? a %2) (set (cons %2 %1)) %1) #{} b
                               )) (first s) (rest s)
           ))))

(defcheck solution-5d34eb76
  (fn [x y](into #{} (filter x y))))

(defcheck solution-5d49bbbb
  (fn [a b]
    (apply disj a (apply disj a b))))

(defcheck solution-5e2f0861
  #(into #{} (filter (fn [x] (contains? %1 x)) %2)))

(defcheck solution-5efcfd20
  (fn [set1 set2]
    (into #{} (filter #(set1 %) set2))))

(defcheck solution-5fcb716e
  #(clojure.set/difference (clojure.set/union %1 %2) (clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))))

(defcheck solution-60a93b8e
  (fn [a b] (reduce #(if (a %2) (conj %1 %2) %1) #{} b)))

(defcheck solution-60b3b013
  (fn [s1 s2]
    (clojure.set/difference s1 (clojure.set/difference s1 s2))))

(defcheck solution-61472228
  #(set(filter %2 %)))

(defcheck solution-618784e7
  (fn [s1 s2]
    (set (filter #(s2 %) s1))))

(defcheck solution-61be9471
  #(set (remove nil? (map %1 %2))))

(defcheck solution-6207b752
  #(set (filter (fn[e] (contains? %2 e)) %1)))

(defcheck solution-6335caf3
  (fn isec [a b]
    (cond (empty? b) #{}
          (contains? a (first b)) (conj (isec a (rest b)) (first b))
          :else (isec a (rest b)))))

(defcheck solution-63698986
  (fn inters [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/union
        (clojure.set/difference a b)
        (clojure.set/difference b a)))))

(defcheck solution-63730202
  (fn [& s] (into #{} (filter (fn [val] (every? #(get % val) s))  (apply clojure.set/union s)))))

(defcheck solution-6383407f
  #(set
     (for [x %1 y %2 :when (= x y)]
       x)))

(defcheck solution-63c8f5b4
  (fn [a b] (reduce (fn [acc i] (if (contains? a i) (conj acc i) acc)) #{} b)))

(defcheck solution-641e2e34
  #(reduce (fn [acc i]
             (cond
               (%2 i) (conj acc i)
               :else acc))
     #{} %1))

(defcheck solution-644faa50
  (fn ins
    ([s1 s2] (ins s1 s2 #{}))
    ([s1 s2 acc]
     (if (empty? s1)
       acc
       (let [elem (s2 (first s1))]
         (recur
           (rest s1)
           s2
           (if (nil? elem) acc (conj acc elem))))))))

(defcheck solution-649acd40
  (fn my-intersection [coll-1 coll-2]
    (set
      (remove #(not (coll-1 %)) coll-2))))

(defcheck solution-64f52b91
  (fn [xs ys] (into #{} (filter #(contains? xs %) ys))))

(defcheck solution-64f918a6
  (fn [s1 s2]
    (set (filter #(s1 %1) s2))))

(defcheck solution-65013b6c
  (fn [c1 c2] (set (filter (complement nil?) (map #(c1 %) c2)))))

(defcheck solution-65091a5b
  (fn [set1 set2]
    (reduce
      #(if (set1 %2) (conj %1 %2) %1) #{} set2)))

(defcheck solution-6569813b
  #(into #{} (remove nil? (map % %2))))

(defcheck solution-65ae0047
  (fn [s1 s2] (set (filter #(contains? s2 %) s1 ))))

(defcheck solution-662375af
  (fn m [a b]
    (set (filter #(contains? b %) a))))

(defcheck solution-66507618
  (fn
    [set1 set2]
    (into #{} (for [x set1 y set2 :when (= x y)] x))))

(defcheck solution-674efd8e
  (fn [xs ys]
    (set (filter #(contains? ys %) xs))))

(defcheck solution-6777a338
  (fn [a b]
    (set (for [x a :when (contains? b x)] x))))

(defcheck solution-683c4f8b
  (fn [s1 s2]
    (reduce #(if (contains? s2 %2) %1 (disj %1 %2)) s1 s1)))

(defcheck solution-685f35d8
  (fn my-intersection [a b]
    (if (empty? a)
      #{}
      (let [result (my-intersection (rest a) b)
            item (first a)]
        (if (contains? b item)
          (conj result item)
          result)))))

(defcheck solution-68a2add
  (fn [s1 s2]
    (reduce (fn [result item]
              (if (contains? s2 item)
                result
                (disj result item)))
      s1 s1)))

(defcheck solution-6922f601
  (fn [xs ys]
    (letfn [(as-map [xs] (zipmap xs (repeat 1)))]
      (into #{}
        (for [[k v] (merge-with + (as-map xs) (as-map ys)) :when (= v 2)] k)))))

(defcheck solution-694ef2fe
  #(set (filter %1 %2)))

(defcheck solution-6956f8fc
  (fn [L & l]
    (set (for [x L :when (every? #(contains? % x) l)] x))))

(defcheck solution-6a4f439a
  (fn [s1 s2]
    (set (keys (filter #(> (last %) 1) (frequencies (into (vec s1) (vec s2))))))))

(defcheck solution-6a83b35a
  (fn intersec[a b]
    (set (for [x a :when (contains? b x)] x))))

(defcheck solution-6a96ecdc
  (fn my-intersection [ls rs]
    (if (empty? rs)
      #{}
      (let [f (first rs)]
        (if (ls f)
          (conj (my-intersection ls (rest rs)) f)
          (my-intersection ls (rest rs)))))))

(defcheck solution-6b995f9f
  (fn [xs ys] (set (filter xs ys))))

(defcheck solution-6c72ba99
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b) (into #{} (concat (clojure.set/difference a b) (clojure.set/difference b a))))))

(defcheck solution-6cd5a827
  #(into #{} (remove nil? (map (fn[e](some #{e} %2)) %))))

(defcheck solution-6d11b82e
  (fn [a b]
    (set(remove (fn [s] ((complement contains?) b s)) a))
    ))

(defcheck solution-6d37ad53
  (fn [lasice hranostaj]
    (set (filter #(lasice %) hranostaj))))

(defcheck solution-6ea8998b
  (fn [sx sy] (reduce
                (fn [acc b]
                  (if (sy b)
                    (conj acc b)
                    acc))
                #{} sx
                )))

(defcheck solution-6f0949f5
  (fn _intersection [set_a set_b]
    (set (for [x set_a
               y set_b
               :when (= x y)]
           x))))

(defcheck solution-6f3e4b37
  (fn [s1 s2] (reduce #(if (s1 %2) (conj %1 %2) %1) #{} s2)))

(defcheck solution-6f9b4720
  (fn [a b] (set (filter #(not (nil? %)) (map a b)))))

(defcheck solution-6fd7baf
  #(clojure.set/difference %2 (clojure.set/difference %2 %1)))

(defcheck solution-6ff1cb85
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b)
      (clojure.set/difference b a)
      (clojure.set/difference a b))))

(defcheck solution-6ffbe509
  (fn [s1 s2] (into #{} (filter #(contains? s2 %) s1))))

(defcheck solution-7039f646
  #(clojure.set/select (fn [x] (contains? %2 x)) %1))

(defcheck solution-7081d31
  #(set(filter % %2)))

(defcheck solution-70c3b7a6
  (fn [& sets] (loop [result (first sets) elems (vec (first sets)) others (next sets)]
                 (if (nil? elems)
                   result
                   (if (every? (fn [s] (s (first elems))) others)
                     (recur result (next elems) others)
                     (recur (disj result (first elems)) (next elems) others)
                     )
                   )
                 )
    ))

(defcheck solution-70e4329
  (fn intersect [coll1 coll2]
    (set (filter #(contains? coll1 %) coll2))))

(defcheck solution-70f49d8
  (fn [c1 c2]
    (set (remove nil? (map #(if (contains? c1 %) %) c2)))))

(defcheck solution-71f4f604
  (comp set filter))

(defcheck solution-720270a1
  (fn [s & ss]
    (set (filter (fn [a-key]
                   (every? (fn [a-set] (a-set a-key)) ss))
           s))))

(defcheck solution-732bdb4a
  #(clojure.set/difference (clojure.set/union %1 %2)
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-744d33d8
  (fn my-intersection [set-a set-b]
    (set (filter set-b set-a))))

(defcheck solution-74f762d5
  (comp set
        (partial keep identity)
        (partial map #(if (> (val %) 1) (key %)))
        frequencies
        concat))

(defcheck solution-751fd164
  (fn [s1 s2] (set (filter #(contains? s2 %) s1))))

(defcheck solution-75515854
  (fn my-intersection [set1 set2]
    (clojure.set/difference (clojure.set/union set1 set2)
      (clojure.set/union (clojure.set/difference set1 set2)
        (clojure.set/difference set2 set1)))))

(defcheck solution-7620ac9c
  (fn [a  b]
    (clojure.set/difference a (clojure.set/difference a b))))

(defcheck solution-77c860d4
  (fn [coll1 coll2]
    (set (for [i coll1 :when (contains? coll2 i)] i))))

(defcheck solution-787f768b
  (fn [s1 s2]
    (->> s2
      (map #(if (clojure.set/subset? (set (vector %)) s1) % nil))
      (remove nil?)
      (into #{}))))

(defcheck solution-78d1c4f6
  (fn [x y]
    (set (filter #(some #{%} y) x))))

(defcheck solution-78ded30b
  (fn [a b] (set (filter #(a %) b))))

(defcheck solution-78fc326e
  #(set(filter (partial contains? %) %2)))

(defcheck solution-79874d8c
  (fn [s1 s2] (set (filter #(contains? s1 %) s2)) ))

(defcheck solution-79aaeab7
  (fn [x,y] (set (filter #(and (contains? x %) (contains? y %)) (concat x y))   )))

(defcheck solution-7a216bea
  (fn [xs ys] (set (for [x xs y ys :when (= y x)] x))))

(defcheck solution-7b14853d
  (fn [s1 s2] (into #{} (filter s2 s1))))

(defcheck solution-7b308a90
  (fn [x y] (clojure.set/difference (clojure.set/union x y)
              (clojure.set/union (clojure.set/difference x y)
                ( clojure.set/difference y x)))))

(defcheck solution-7b7adc28
  #(reduce (fn [s v] (if (contains? %1 v) (conj s v) s)) #{} %2))

(defcheck solution-7b80eac2
  (fn [a b] (set (filter (fn [e] (not (nil? e))) (map a b)))))

(defcheck solution-7bd963d2
  (fn[s1 s2]
    (let [[s1 s2] (if (> (count s1) (count s2)) [s1 s2] [s2 s1])]
      (apply hash-set (filter identity (map (fn[elt] (s2 elt)) s1))))))

(defcheck solution-7c051223
  (fn [sa sb] (into #{} (filter #(contains? sb %) sa))))

(defcheck solution-7c1255de
  (fn [& sets]
    (let [c (count sets)]
      (reduce #(if (= c (second %2)) (conj %1 (first %2)) %1)
        #{}
        (seq (frequencies (reduce #(into %1 %2) '() sets)))))))

(defcheck solution-7c378856
  (fn [s1 s2]
    (reduce (fn [acc x] (if (contains? s1 x) acc (disj acc x)))
      s2
      s2)))

(defcheck solution-7c3f06b4
  #(reduce (fn [i e] (if (get %1 e) (conj i e) i)) #{} %2))

(defcheck solution-7caf0fb
  #(set (for [x % y %2 :when (= x y)] x) ))

(defcheck solution-7cb852a9
  (fn my-intersect [set-a set-b]
    (disj (reduce #(conj %1 (set-a %2)) #{} set-b)
      nil)))

(defcheck solution-7d03947f
  (fn cust-inter [lset rset]
    (reduce #(if (contains? rset %2) (conj %1 %2) %1)
      #{}
      lset)))

(defcheck solution-7e2c8dd3
  (fn intrsct [& sets]
    (let [[f & r] sets]
      (set (filter (fn [v] (every? #(contains? % v) r)) f)))))

(defcheck solution-7e5556ca
  (fn [s1 s2]
    (set
      (reduce
        (fn [acc val]
          (if (s2 val)
            (conj acc val)
            acc))
        #{}
        s1))))

(defcheck solution-7e6ccd5f
  (fn interception [lft rght]
    (set (filter #(contains? rght %) lft))))

(defcheck solution-7eb9a74a
  (fn [a b] (set (filter #(contains? a %) b))))

(defcheck solution-7ed8d18f
  (fn [s1 s2]
    (reduce (fn [i e]
              (if (s1 e) (conj i e) i))
      #{}
      s2)))

(defcheck solution-7f719d5e
  #(into #{} (remove (into #{} (remove %2 %)) %)))

(defcheck solution-7f773213
  (fn set-intersection [s1 s2]
    (set (reduce
           (fn [a x] (if (contains? s2 x) (cons x a) a))
           '()
           s1))))

(defcheck solution-7fa14fc6
  (fn
    [s1 s2]
    (set
      (for [x s1 y s2 :when (= x y)]
        x))))

(defcheck solution-7ff0853f
  (fn inters [& sets]
    (reduce (fn inter-two [set1 set2]
              (set
                (filter #(contains? set1 %) set2)))
      sets)))

(defcheck solution-800da940
  clojure.set/select)

(defcheck solution-80d34156
  (fn intersection_ [s1 s2]
    (let [members (distinct (concat s1 s2))]
      (set (filter #(and (s1 %) (s2 %)) members)))))

(defcheck solution-810d87d0
  #(set
     (filter (fn [n] (not= n nil))
       (for [n %1]
         (if (contains? %2 n)
           n)))))

(defcheck solution-81687cad
  (fn [left right]
    (into #{} (filter #(and (left %) (right %)) (concat left right)))))

(defcheck solution-817b648e
  (fn my-intersect [sa sb]
    (set (filter #(and (sa %) (sb %)) (into sa sb )))))

(defcheck solution-824a2a2d
  (fn intersect- [a b]
    (into #{}
      (reduce #(if (contains? b %2) (conj %1 %2) %1) #{} a))))

(defcheck solution-827a0c60
  (fn intsect [s1 s2]
    (reduce (fn [ret this]
              (if (s1 this)
                (conj ret this)
                ret)) #{} s2)))

(defcheck solution-82ed2a8d
  (fn [ a b]
    (set (filter (comp not nil?) (map #(if (contains? a %) % ) b)))))

(defcheck solution-8311d734
  (fn [a b] (reduce #(if (nil? (b %2)) %1 (conj %1 %2)) #{} a)))

(defcheck solution-831cb1b1
  (fn [x y]
    (set
      (remove (clojure.set/difference y x)
        (remove (clojure.set/difference x y)
          (clojure.set/union x y))))))

(defcheck solution-83542dd6
  #(set (filter %2 %)))

(defcheck solution-8423f20f
  (fn [s1 s2](set (filter s1 s2))))

(defcheck solution-84494182
  (fn [s1 s2]
    (into #{} (filter #(contains? s2 %) s1))))

(defcheck solution-84b7c481
  (fn si [s1 s2 & more-sets]
    (let [i (reduce (fn [acc v] (if (s1 v) (conj acc v) acc)) #{} s2)]
      (if (seq more-sets)
        (apply si i more-sets)
        i))))

(defcheck solution-84f7444b
  (fn [s1 s2]
    (let [ss (into s1 s2)]
      (apply hash-set
        (filter
          (fn [x]
            (and (contains? s1 x)
                 (contains? s2 x))
            )
          ss
          )
        )
      )
    ))

(defcheck solution-853b71e0
  (fn [xs ys]
    (->> (filter #(contains? ys %) xs)
      (into #{}))))

(defcheck solution-85e604a7
  (fn [set1 set2] (reduce #(if (contains? set2 %2) (conj %1 %2) %1) #{} set1)))

(defcheck solution-860f5c9d
  #(set (remove nil? (map % %2))))

(defcheck solution-861e8ab9
  (fn [s1 s2]
    (reduce #(if (contains? s2 %2) (conj %1 %2) %1) #{} s1)))

(defcheck solution-8623a543
  (fn [s1 s2] (set (filter #(s2 %) s1))))

(defcheck solution-868b2f44
  (fn [seta setb]
    (set (filter seta setb))))

(defcheck solution-87d482f7
  (fn my-intersection [s1 s2]
    (set (filter s1 s2))))

(defcheck solution-887f7f43
  (fn [A & Bs]
    (reduce (fn [A B]
              (reduce (fn [C b]
                        (if (A b) (conj C (A b)) C)) #{} B)) A Bs)))

(defcheck solution-88d773ce
  #(set (for [x %1 :when (contains? %2 x)] x)))

(defcheck solution-899d79d0
  #(clojure.set/select (partial contains? %1) %2))

(defcheck solution-89d3f3de
  (fn [s1 s2]
    (into #{} (filter #(contains? s1 %) s2))))

(defcheck solution-8a605b65
  #(set (filter (partial get %2) %)))

(defcheck solution-8a6e238c
  (fn [as bs] (set (filter #(contains? bs %) as))))

(defcheck solution-8ab53446
  (fn [a b]
    (set (filter #(contains? a %) b))))

(defcheck solution-8b965da5
  (fn [a b] (set (filter #(contains? b %) a))))

(defcheck solution-8bb9269
  (fn [s1 s2]
    (let [diff clojure.set/difference uni clojure.set/union]
      (diff s1 (uni (diff s1 s2) (diff s2 s1))))))

(defcheck solution-8c28082a
  (fn [a b] (clojure.set/select #(contains? b %) a)))

(defcheck solution-8c9443da
  #((comp set filter) %1 %2))

(defcheck solution-8cb02850
  (fn [s1 s2] (into #{} (for [x (concat s1 s2) :when (and (contains?  s1 x ) (contains?  s2 x)) ] x)  )))

(defcheck solution-8de4788f
  #(set (for [x %1 :when (%2 x)] x)))

(defcheck solution-8f08c372
  (fn [a b] (set (filter a b))))

(defcheck solution-8f2a83b
  (fn in-both [set-a set-b]
    (into #{} (filter #(contains? set-a %) set-b))))

(defcheck solution-8fd6b517
  #(set (for [a %1 :when (%2 a)] a)))

(defcheck solution-8fe59483
  #(disj (set (for [i %] (if (%2 i) i nil))) nil))

(defcheck solution-900516b0
  (fn [a b]
    (set
      (for [x (clojure.set/union a b)
            :when (and (a x) (b x))]
        x))))

(defcheck solution-904d4d43
  (fn [s1 s2]
    (let [sum (into s1 s2)]
      (into #{}  (filter #(and (s1 %) (s2 %)) sum)))))

(defcheck solution-908e6005
  (fn [x y]
    (set
      (filter #(contains? x %)
        y))))

(defcheck solution-90fdf132
  (fn [a b](->> (map vector b)
             (reduce #(conj %1 (some a %2)) #{})
             (remove nil?)
             (into #{}))))

(defcheck solution-911a0491
  (fn [xs ys] (set (filter ys xs))))

(defcheck solution-914539a2
  #(set (apply filter %&)))

(defcheck solution-91519c56
  #(set (filter %1 %2)))

(defcheck solution-921aa56a
  (fn [a b]
    (set (filter #(not (nil? %)) (map a b)))))

(defcheck solution-928603bb
  (fn [a b]
    (into #{}
      (filter a b))))

(defcheck solution-92e7a3a
  (fn
    [l r]
    (into #{} ( filter l r))
    ))

(defcheck solution-931a1fa7
  (fn [a b]
    (reduce #(if (b %2) (conj %1 %2) %1) #{} a)))

(defcheck solution-938c9b35
  (fn [a b]
    (clojure.set/difference a (clojure.set/difference a b))))

(defcheck solution-93f263a4
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/difference s2 s1)
      (clojure.set/difference s1 s2))))

(defcheck solution-94921115
  (fn [s1 s2] (clojure.set/select s1 s2) ))

(defcheck solution-94fd7592
  #(into #{} (filter % %2)))

(defcheck solution-9601bcd6
  (fn [x y] (clojure.set/difference x (clojure.set/difference x y))))

(defcheck solution-96925d92
  (fn [a b]
    (reduce #(if (a %2) (conj %1 %2) %1) #{} b)))

(defcheck solution-96ad380c
  (fn [a b](
             set(filter (fn[x](contains? a x)) b)
             )))

(defcheck solution-96e11ef7
  (fn [x y] (into #{} (filter #(contains? y %) x))))

(defcheck solution-9750a22c
  #(set (filter (complement nil?) (map % %2))))

(defcheck solution-975873d1
  #(% %2 (% %2 %3)) clojure.set/difference)

(defcheck solution-97727aad
  (fn [s1 s2] (set (filter #(contains? s2 %1) s1))))

(defcheck solution-9790554a
  #(reduce
     (fn [r e]
       (if (contains? % e)
         (conj r e)
         r))
     #{} %2))

(defcheck solution-97eb10c5
  (fn set-interset
    [left right]
    (loop [[h & t] (into [] left)
           accum #{}]
      (let [new-accum (if (contains? right h)
                        (conj accum h)
                        accum)]
        (if (nil? t)
          new-accum
          (recur t new-accum))))))

(defcheck solution-98f9df2
  (fn rec [a b]
    (if (empty? a) #{}
                   (if (contains?  b (first a))
                     (conj (rec (rest a) b) (first a))
                     (rec (rest a) b)))))

(defcheck solution-99dbf0e8
  (fn is [col1 col2]
    (clojure.set/difference (clojure.set/union col1 col2)
      (clojure.set/union (clojure.set/difference col1 col2)
        (clojure.set/difference col2 col1)))))

(defcheck solution-99e29a03
  (fn
    [a b]
    (set
      (filter
        (fn
          [x]
          (contains? b x)) a))))

(defcheck solution-99f30f6a
  (fn intersect-sets [s1 s2] (set (filter #(contains? s2 %) s1))))

(defcheck solution-9a05d9fc
  (fn [a b] (reduce (fn [l r] (if (contains? a r) (conj l r) l)) #{} b)))

(defcheck solution-9a611df7
  (fn [a b] (set (filter #(b %1) a))))

(defcheck solution-9aaa23a3
  (fn [r s] (reduce #(if (contains? s %2) (conj % %2) %) #{} r)))

(defcheck solution-9b011ede
  (fn [s1 s2]
    (set (for [x s1
               :when (not (nil? (s2 x)))]
           x))))

(defcheck solution-9b72c509
  (fn intersect [x y]
    (set (filter y x))))

(defcheck solution-9bdbe0e9
  (fn [x y]
    (set (filter #(contains? x %) y))))

(defcheck solution-9bf7dbe4
  (fn [a b]
    (let [both (clojure.set/union a b)
          bo-a (clojure.set/difference both a)
          bo-b (clojure.set/difference both b)]
      (clojure.set/difference both bo-a bo-b))))

(defcheck solution-9c29ed71
  (fn [set1 set2]
    (set (filter #(contains? set2 %) set1))))

(defcheck solution-9c3a4fa
  (fn [s1 s2]
    (let [union (into s1 s2)
          diff clojure.set/difference]
      (diff union
        (diff union s1)
        (diff union s2)))))

(defcheck solution-9c53604e
  (fn [a b] (into #{} (remove nil? (map a b)))))

(defcheck solution-9c5d1403
  (fn [set-1 set-2]
    (into #{} (for [item set-1, :when (some #{item} set-2)] item))))

(defcheck solution-9c83f90a
  (fn [as bs]
    (set (filter as bs))))

(defcheck solution-9c9f28b1
  (fn [c1 c2] (set (for [x c1 :when (contains? c2 x)] x))))

(defcheck solution-9d960af6
  (fn inters [s1 s2]
    (set (filter #(contains? s2 %) s1))))

(defcheck solution-9da199ae
  (fn my-intersection [arg-1 arg-2]
    (set (filter #(arg-1 %) arg-2))
    ))

(defcheck solution-9db94ae8
  (fn [a b](reduce (fn [c d](if (contains? a d) (conj c d) c)) #{} b)))

(defcheck solution-9e0c9738
  (fn [s1 s2]
    (set (for [x s1
               :let [y x]
               :when (contains? s2 x)]
           y))))

(defcheck solution-9ec82ab0
  (fn myIntersection
    [a b]
    (into #{}
      (for [s1 a
            s2 b
            :when (= s1 s2)]
        s1))))

(defcheck solution-9f1e12f6
  (fn [X Y] (set (filter #(contains? Y %) X))))

(defcheck solution-9f1e206c
  #(set (filter (fn [i] (some % #{i})) %2)))

(defcheck solution-9f1f47b9
  (fn [s1 s2] (reduce #(if (contains? s2 %2) (conj %1 %2) %1) #{} s1)))

(defcheck solution-9f2677e1
  #(into #{} (filter (partial contains? %2) %1)))

(defcheck solution-9fa86f7d
  (fn intersection-1 [s1 s2]
    (reduce
      (fn [xs x]
        (if (contains? s1 x)
          (conj xs x)
          xs)) #{} s2)))

(defcheck solution-a0a16324
  #(apply sorted-set
     (filter
       (fn [x]
         (contains? %1 x))
       %2)))

(defcheck solution-a0b2d8c7
  (fn  [sa sb]
    (set (filter #(sa %) sb))
    ))

(defcheck solution-a0c3b404
  (fn [x y] (into #{} (filter x y))))

(defcheck solution-a0dec7df
  (fn [x y] (set (filter #(not (nil? (x %))) y))))

(defcheck solution-a19fa537
  (fn [setA setB] (let [ls (flatten (into (into [] setA) (into [] setB)))]
                    (into #{} (map key (remove (comp #{1} val) (frequencies ls)))))))

(defcheck solution-a228b8a
  #(reduce (fn [t v] (if (contains? %2 v) (conj t v) t)) #{} %1))

(defcheck solution-a23b4902
  (fn [f & sets]
    (reduce (fn [m e]
              (set (remove nil? (map #(some #{%} m) e))))
      f sets)))

(defcheck solution-a28b5859
  #(set (keys (filter (fn [x] (> (second x) 1)) (frequencies (concat %1 %2))))))

(defcheck solution-a3d0f247
  (fn my-intersection [s1 s2]
    (set (filter #(contains? s1 %) s2))))

(defcheck solution-a3e6f4fd
  (fn [s1 s2]
    (if (< (count s2) (count s1))
      (recur s2 s1)
      (reduce (fn [result item]
                (if (contains? s2 item)
                  result
                  (disj result item)))
        s1 s1))))

(defcheck solution-a3ed8c89
  #(into #{} (for [x % y %2 :when (= x y)] x)))

(defcheck solution-a44404c7
  (fn [s1 s2]
    (loop [lst s1 ans #{}]
      (if (empty? lst)
        ans
        (if (contains? s2 (first lst))
          (recur (rest lst) (into ans [(first lst)]))
          (recur (rest lst) ans))))))

(defcheck solution-a472ebc8
  (fn my-intersection [x y] (reduce #(if (contains? x %2) (conj %1 %2) %1) #{} y)))

(defcheck solution-a4e6b6de
  (fn [a b]
    (set (filter #(a %) b))))

(defcheck solution-a50216ec
  (fn [fset sset]
    (loop [pset fset
           result #{}]
      (if (empty? pset)
        result
        (recur (rest pset)
          (if (sset (first pset))
            (conj result (first pset))
            result))))))

(defcheck solution-a549ec56
  (fn [fst snd] (set (filter #(contains? fst %) snd))))

(defcheck solution-a5ddaa89
  #(set (filter (complement nil?) (map %1 %2))))

(defcheck solution-a5dff98a
  (fn [s & ss]
    (set (filter
           (fn [e] (every? (fn [z] (some #(= % e) z)) ss)) s))))

(defcheck solution-a5eb72c2
  #(set (filter %2 %1)))

(defcheck solution-a62632d9
  (fn [a b]
    (let [x (apply disj a (into () b))]
      (apply disj a (into () x)))))

(defcheck solution-a66053d6
  (fn [a b] (into #{} (filter  #(contains?  a %) b ))))

(defcheck solution-a6d65ee5
  (fn [S T] (apply disj S (apply conj (apply disj S T) (apply disj T S)))))

(defcheck solution-a83269f
  (fn [s1 s2]
    (letfn [(common [xs ys] (reduce
                              (fn [common item]
                                (if (contains? ys item)
                                  (conj common item) common))
                              [] xs))]
      (set (concat (common s1 s2) (common s2 s1))))))

(defcheck solution-a87a4ce8
  (fn [x y] (set (filter y x))))

(defcheck solution-a8b5d3ff
  (fn [a b] (set (remove nil? (for [i a] (if (contains? b i) i nil))))))

(defcheck solution-a93eb267
  (fn [a b]
    (set
      (for [ i a j b :when (= i j)]
        i))
    ))

(defcheck solution-a98294a7
  (fn my-intersection
    [s1 s2]
    (into #{} (filter #(contains? s1 %) s2))))

(defcheck solution-a98fe64f
  (fn intersect--filter
    [& [a & more :as sets]] {:pre [(every? set? sets)]}
    (if a
      (set (filter (apply comp more) a))
      #{})))

(defcheck solution-a9fcbec7
  (fn intersect [s1 s2]
    (set (filter s1 s2))))

(defcheck solution-aa1fc98c
  (fn [a b]
    (set (reduce concat []
           (filter #(= 2 (count %))
             (map val
               (group-by identity
                 (concat (seq a)
                         (seq b)))))))))

(defcheck solution-ab683ccb
  (fn [& a] (reduce #(clojure.set/difference % (clojure.set/difference % %2)) (apply clojure.set/union a) a)))

(defcheck solution-abc816ce
  (fn isect [a b]
    (into #{} (filter #(b %) a))
    ))

(defcheck solution-ac73b9c1
  (fn [a b]
    (set (filter #(a %) b))))

(defcheck solution-ac7bf2d9
  (fn my-intersection [s1 s2]
    (if (< (count s2) (count s1))
      (recur s2 s1)
      (reduce (fn [result item]
                (if (contains? s2 item)
                  result
                  (disj result item)))
        s1 s1))))

(defcheck solution-ac8ec5e
  (fn [s1 s2]
    (loop [keep #{}
           seq1 (seq s1)]
      (if (empty? seq1) keep
                        (let [nxt (first seq1)
                              remain (rest seq1)]
                          (if (contains? s2 nxt)
                            (recur (conj keep nxt) remain)
                            (recur keep remain)))))))

(defcheck solution-acaba855
  (fn  [s1 s2]
    (loop [rm s1, rm2 s2, acc #{}]
      (cond (empty? rm2) acc
            (empty? rm) (recur s1 (rest rm2) acc)
            (= (first rm) (first rm2)) (recur (rest rm) rm2 (conj acc (first rm)))
            :else (recur (rest rm) rm2 acc)))))

(defcheck solution-acd1ab87
  (fn inters [s1 s2]
    (set (filter #(s2 %) s1))
    ))

(defcheck solution-acf05923
  (fn [a b]
    (let [all (clojure.set/union a b)
          c #(clojure.set/difference all %)]
      (c (clojure.set/union (c a) (c b)))
      )
    ))

(defcheck solution-ad0eaa7f
  (fn [set1 set2] (set (filter set1 set2))))

(defcheck solution-ad35afe8
  (fn [s1 s2]
    (into #{} (filter #(contains? s2 %) s1)) ))

(defcheck solution-ad4932e8
  (fn [a b] (set (for [x a y b :when (= x y)] x))))

(defcheck solution-ad9095a9
  #(disj (into #{} (map % %2)) nil))

(defcheck solution-adc2b6e4
  (fn [set1 set2]
    (reduce (fn [res it]
              (if (contains? set2 it)
                (conj res it)
                res)) #{} set1)))

(defcheck solution-adde9a58
  (fn [s1 s2]
    (set (filter #(some #{%} s2) s1))))

(defcheck solution-ae398934
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-afb3c7f6
  (fn intersct [seta setb]
    (reduce #(if (seta %2) (conj %1 %2) %1) #{} setb)))

(defcheck solution-b038d6a9
  (fn [a b]
    (into #{} (filter a b))))

(defcheck solution-b05bac92
  #(set(keep %1 %2)))

(defcheck solution-b0b08ae8
  (fn [a b]
    (set (for [i a :when (b i)] i))))

(defcheck solution-b19920e2
  (fn set-intersect
    [s1 s2]
    (clojure.set/select s1 s2)))

(defcheck solution-b19d08fc
  (fn[a b]
    (set (filter (fn [a] a) (for[aa a]
                              (if (get b aa) aa))))))

(defcheck solution-b1c68f43
  (fn f [s1 s2]
    (set (filter #(some (hash-set %) s2) s1))))

(defcheck solution-b2ff01c7
  (fn my-intersection [s1 s2]
    (loop [res #{}
           ss (seq s1)]
      (if-let [el (first ss)]
        (recur (if (contains? s2 el) (conj res el) res) (rest ss))
        res))))

(defcheck solution-b30fa10c
  (fn [a,b] (set (filter #(contains? b %) a))))

(defcheck solution-b39ea0e5
  (fn [set1 set2] (disj (set (for [item1 set1 item2 set2] (when (= item1 item2) item1))) nil)))

(defcheck solution-b3d5d6f2
  (fn [s1 s2]
    (into #{} (for [x s1 :when (contains? s2 x)] x))))

(defcheck solution-b3f1851c
  (fn [set1 set2]
    (set (reduce
           (fn [acc x]
             (if (contains? set1 x) (conj acc x) acc))
           #{}
           set2))))

(defcheck solution-b474872a
  (fn f
    [a b]
    (set (filter #(and (contains? a %) (contains? b %)) (clojure.set/union a b)))))

(defcheck solution-b480f4e1
  (fn intrsct
    [s1 s2]
    (clojure.set/difference s1 (clojure.set/difference s1 s2))))

(defcheck solution-b4a52b8a
  (fn [s1, s2] (set (filter (complement nil?)  (map #(s1 %) s2)))))

(defcheck solution-b4cfdc2c
  (fn [s1 s2]
    (let [un (clojure.set/union s1 s2)
          left (clojure.set/difference un s1 )
          right (clojure.set/difference un s2 )
          ]
      (clojure.set/difference un left right )
      )
    ))

(defcheck solution-b4ef7ddc
  #(reduce (fn [acc v] (if (contains? %2 v) (conj acc v) acc)) #{} %1))

(defcheck solution-b50629f5
  (fn [ca cb]
    (set (filter #(contains? cb %) ca))))

(defcheck solution-b53a1e90
  (fn
    [one & args]
    (set (filter #(every? (fn [s] (contains? s %)) args) one))))

(defcheck solution-b546267b
  (fn [s1 s2]
    (reduce (fn [res el] (if-not (nil? (s1 el)) (conj res el) res)) #{} s2)
    ))

(defcheck solution-b5cb796b
  (fn [s1 s2] (set (filter #(contains? s2 %) s1))))

(defcheck solution-b67a8d1a
  (fn [s1 s2] (set (filter s1 s2))))

(defcheck solution-b6e07933
  (fn [s1 s2]
    (set(filter s1 s2))))

(defcheck solution-b704edb3
  (fn [a b]
    (set (filter #(and (a %) (b %)) (concat a b)))))

(defcheck solution-b728d7cd
  (fn [set1 set2]
    (reduce (fn [acc item] (if (set2 item) (conj acc item) acc)) #{} set1)))

(defcheck solution-b79f2ed9
  (fn [a b]
    (set (filter #(contains? a %) b))))

(defcheck solution-b7e1a84d
  (fn [a b] (set (for [x (seq a) :when (b x)] x))))

(defcheck solution-b81c84c7
  (fn [s1 s2] (set (for [s (seq s1)
                         :when (s2 s)]
                     s))))

(defcheck solution-b8d847fe
  (fn [set1 set2]
    (loop [s1 set1 rslt #{}]
      (if (empty? s1) rslt
                      (if (contains? set2 (first s1))
                        (recur (rest s1) (conj rslt (first s1)))
                        (recur (rest s1) rslt))))))

(defcheck solution-b9298b8a
  #(reduce (fn [r e] (if (%1 e) (conj r e) r)) #{} %2))

(defcheck solution-b950a7ef
  (fn __ [a b]
    (clojure.set/difference (clojure.set/union a b)
      (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a)))))

(defcheck solution-b950e9c5
  (fn [xs ys]
    (set (filter #(contains? xs %1) ys))))

(defcheck solution-b9ddb01f
  (fn [s1 s2]
    (set (filter s1 s2))))

(defcheck solution-b9e284fb
  (fn [xs ys]
    (set  (filter #(contains? xs %) ys)  )))

(defcheck solution-ba328a4
  (fn
    [myset1 myset2]
    (set
      (filter #(contains? myset1 %) myset2)

      )))

(defcheck solution-ba47fefb
  (fn tmp [x y]
    (into #{} (filter #(not(nil? (y %))) x))
    ))

(defcheck solution-bab98753
  (fn [s1 s2]
    (set (filter #(contains? s1 %) s2))))

(defcheck solution-bae615c7
  (fn [s1 s2]
    (loop [s1p s1 acc #{}]
      (if (empty? s1p)
        acc
        (if (contains? s2 (first s1p))
          (recur (rest s1p) (conj acc (first s1p)))
          (recur (rest s1p) acc))))))

(defcheck solution-baf839f4
  (fn mintersect
    [s1 s2]
    (clojure.set/select #(contains? s2 %)  s1)))

(defcheck solution-bb69fb0d
  (fn [x y]
    (if (> (count x) (count y))
      (recur y x)
      (reduce (fn [result item]
                (if (contains? y item)
                  result
                  (disj result item)))
        x x))))

(defcheck solution-bb9e70fb
  (fn [s1 s2]
    (set (filter s2 s1))))

(defcheck solution-bbfb88d8
  #(-> (filter %2 %1) set))

(defcheck solution-bc8eb633
  #(set (filter (partial contains? %2) %)))

(defcheck solution-bd32cc13
  (fn [a b]
    (set (filter #(contains? b %1) a))))

(defcheck solution-bd43b297
  #(reduce (fn [S a] (if (%2 a) (conj S a) S))
     #{} %1))

(defcheck solution-be996104
  (fn [a b] (into #{} (filter #(b %) a))))

(defcheck solution-bef7b0d6
  (fn my-intersect [s1 s2]
    (set (for [x s1 :when (s2 x)]
           x))))

(defcheck solution-bf60e969
  #(clojure.set/difference %1 (clojure.set/difference %1 %2)))

(defcheck solution-c08f633d
  (fn [s1 s2]
    (loop [out #{} in (seq s1)]
      (if (empty? in)
        out
        (recur (if (contains? s2 (first in)) (conj out (first in)) out) (rest in))))))

(defcheck solution-c15e0505
  (fn [left right]
    (let [union (into left right)]
      (set (filter #(and (contains? left %) (contains? right %)) union)))))

(defcheck solution-c1fca660
  (fn intersec [s1 s2]
    (let [freq (frequencies (into (into [] s1) s2))]
      (into #{} (map first
                  (filter (fn [[k v]] (= v 2))
                    freq))))))

(defcheck solution-c257a61c
  (fn ff[a b]
    (loop [a a res #{}]
      (let [k (first a)
            isinb (get b k)]
        (if k
          (recur
            (next a)
            (if isinb
              (conj res k)
              res))
          res)))))

(defcheck solution-c297e336
  (fn
    [col1 col2]
    (-> (filter (fn [[a b]] (= a b))
          (for [x1 col1 x2 col2] (vector x1 x2))) flatten set)))

(defcheck solution-c29f78b4
  (fn [a b]
    (set (keep a b))
    ))

(defcheck solution-c2be4184
  #(into #{} (concat (filter %1 %2) (filter %2 %1))))

(defcheck solution-c33ad7ee
  (fn [s1 s2]
    (reduce (fn [cont it]
              (if (get s2 it)
                (conj cont it)
                cont)
              ) #{} s1)
    ))

(defcheck solution-c3404928
  (fn [xs ys]
    (apply hash-set
      (for [x xs
            y ys
            :when (= x y)]
        x))))

(defcheck solution-c38c8ac8
  (fn [a b] (set (keep #(if (contains? b %) %) a))))

(defcheck solution-c3b41528
  (fn [s1 s2]
    (clojure.set/select #(contains? s2 %) s1)))

(defcheck solution-c3e92a3a
  (fn [a b] (set (filter #(and (a %) (b %)) (concat a b)))))

(defcheck solution-c47a53fd
  #(set (flatten (for [x %1] (for [y %2 :when (= x y)] x)))))

(defcheck solution-c4b5a70d
  (fn [set1 set2] (into #{} (filter set1 set2))))

(defcheck solution-c4f2f92a
  #(set (filter (partial contains? %1) %2)))

(defcheck solution-c6f5d53d
  #(set (remove (set (remove % %2)) %2)))

(defcheck solution-c6ff7385
  (fn [a b]
    (let [da (clojure.set/difference a b)
          db (clojure.set/difference b a)]
      (clojure.set/difference (clojure.set/union a b) (clojure.set/union da db)))))

(defcheck solution-c7191464
  (fn [set1 set2] (reduce #(if (contains? set2 %2)
                             (conj %1 %2)
                             %1)
                    #{}
                    set1)))

(defcheck solution-c7c95e7
  #(set (keep %2 %)))

(defcheck solution-c7d01e76
  (fn [m1 m2]
    (clojure.set/difference m1 (clojure.set/difference m1 m2))))

(defcheck solution-c8098f4b
  #(into #{} (filter (partial contains? %) %2)))

(defcheck solution-c810a897
  (fn [ a b]
    (into #{}
      (filter #(contains? b %) a))))

(defcheck solution-c8b65655
  (fn [xs ys]
    (set
      (filter
        (fn [v]
          (contains? ys v)
          )
        xs
        )
      )
    ))

(defcheck solution-c8df167d
  (fn inter [s1 s2]
    (into #{} (filter #(not (nil? %)) (for [x s1] (cond (some #{x} s2) x :else nil))))
    ))

(defcheck solution-c8ea67f8
  (fn [s1 s2]
    (reduce #(if (contains? s2 %2)
               (conj %1 %2)
               %1
               ) #{} s1)))

(defcheck solution-c922bb32
  (fn my-intersection [a b]
    (set (filter (fn [v] (contains? b v)) a))))

(defcheck solution-c94deac5
  (fn I [s1 s2]
    (loop [s1 s1 result #{}]
      (cond (empty? s1) result
            (s2 (first s1)) (recur (rest s1) (conj result (first s1)))
            :else (recur (rest s1) result)))))

(defcheck solution-c97e8ccf
  (fn [c1 c2] (set (keys (remove #(= 1 (val %)) (frequencies (concat c1 c2)))))))

(defcheck solution-c9f60a18
  (fn[x & y]
    #_(println "x :" x "y: " (into [] (get (into [] y) 0)))
    (set (reduce
           (fn [a b](if (contains? x b) (conj a b)  a))
           [] (into [] (get (into [] y) 0)))
      )))

(defcheck solution-ca2bf3d7
  (fn [a b]
    (set (filter #(contains? b %) a))))

(defcheck solution-ca5e8453
  (fn [s1 s2] (reduce (fn [a s] (if (s2 s) (conj a s) a)) #{} s1)))

(defcheck solution-cb26323
  (fn [a b]
    (loop [s a n #{}]
      (cond
        (empty? s) n
        (contains? b (first s)) (recur (next s) (conj n (first s)))
        :else (recur (next s) n)))))

(defcheck solution-cb2a9b56
  (fn [a b]
    (set (filter #(and (a %) (b %)) (set (concat a b))))))

(defcheck solution-cb785f74
  (fn [set1 set2]
    (set (for [n1 set1
               n2 set2
               :when (= n1 n2)]
           n1))))

(defcheck solution-cbae51a0
  (fn [xs ys]
    (set (keep identity (map xs ys)))))

(defcheck solution-cd19bdb7
  #(into #{} (map first (filter (fn [[a b]] (= 2 b)) (frequencies (concat % %2))))))

(defcheck solution-cd454ffb
  (fn [set1 set2]
    (set (filter #(contains? set1 %) set2))))

(defcheck solution-cdaf97a8
  (fn [a b]
    (set
      (filter #(contains? b %) a))))

(defcheck solution-ce01b1e
  (fn inters [a b] (reduce #(if (contains? a %2) (conj %1 %2) %1) #{} b)))

(defcheck solution-ce6d5057
  (fn [s1 s2]
    (set
      (loop [accum '()
             s1seq  s1]
        (if (empty? s1seq)
          accum
          (recur
            (if (contains? s2 (first s1seq))
              (cons (first s1seq) accum)
              accum)
            (rest s1seq)))))))

(defcheck solution-ce93d796
  (fn set-intersection
    [xs vs]
    (set (filter xs vs))))

(defcheck solution-cf0e1ee2
  (fn prob81 [set1 set2]
    ;; for each element in s1 if in s2 return in result
    ;; should start with largest set (or same set size)
    (loop [s1 set1
           s2 set2
           res []
           ]
      (if (empty? s1)
        (set (reverse res))
        (let [item (first s1)]
          (if (contains? s2 item)
            (recur (disj s1 item) s2 (conj res item))
            (recur (disj s1 item) s2 res)))))))

(defcheck solution-cf202e74
  (fn [A B] (set (filter #(contains? A %) B))))

(defcheck solution-cfe6bcdf
  (fn [s1 s2] (set (filter #(contains? s1 %) s2))))

(defcheck solution-d07189ed
  (fn [x y] (set (filter #(contains? y %) x))))

(defcheck solution-d16a9ad6
  #(set (filter identity (map %1 %2))))

(defcheck solution-d18a0a3f
  #(set (for [x %1, y %2, :when (= x y)] x)))

(defcheck solution-d1af7f6c
  #(let [d clojure.set/difference](d % (d % %2))))

(defcheck solution-d2360a5
  (fn my-intersection
    ([x] x)
    ([sa sb]
     (reduce (fn [j k] (if (contains? sb k) (conj j k) j)) #{} sa))
    ([sa sb & rest]
     (apply my-intersection (my-intersection sa sb) rest))))

(defcheck solution-d313c278
  (fn [set-1 set-2]
    (reduce (fn [acc elt] (if (set-2 elt)
                            (conj acc elt)
                            acc))
      #{}
      set-1)))

(defcheck solution-d33743ca
  (fn [a b] (clojure.set/select
              #(contains? a %) b)))

(defcheck solution-d44bec5a
  (fn [a b] (apply hash-set (filter #(not (nil? %)) (map #(a %) b)))))

(defcheck solution-d4ce7483
  (fn [a,b]
    (set (remove #(nil? (some b (set (list %)))) a))
    ))

(defcheck solution-d4eecc93
  (fn intr
    ([s1 s2] (intr s1 s2 #{}))
    ([s1 s2 si]
     (if (empty? s1) si
                     (let [f (first s1)
                           s1p (disj s1 f)
                           s2p (disj s2 f)]
                       (if (contains? s2 f)
                         (intr s1p s2p (conj si f))
                         (intr s1p s2p si)))))))

(defcheck solution-d4fd3b18
  (fn [a b]
    (set
      (map first
        (filter (fn [[x y]] (= x y))
          (for [d a e b] [d e])
          )
        )
      )
    ))

(defcheck solution-d50e8adc
  #(apply hash-set (filter % %2)))

(defcheck solution-d5494520
  (fn [ & ss ]
    (set (filter
           (fn [n] (every? #(% n) ss))
           (apply concat ss)))))

(defcheck solution-d5b70fc4
  (comp #(into #{} %) filter))

(defcheck solution-d6afb13e
  (fn sec [s1 s2] (reduce #(if (contains? s1 %2)
                             (conj %1 %2)
                             %1) #{} s2)))

(defcheck solution-d6c4c1bd
  #(set (filter (partial get %1) %2)))

(defcheck solution-d724bd1
  (fn [s1 s2]
    (apply hash-set (filter #(s2 %) s1))))

(defcheck solution-d7659bb2
  (fn [x y] (set (filter x y))))

(defcheck solution-d7850817
  #(set (filter %1 %2)))

(defcheck solution-d7c11948
  (fn [s1 s2]
    (reduce
      #(if (contains? s2 %2)
         (conj %1 %2)
         %1)
      #{}
      s1)))

(defcheck solution-d7e91df5
  (fn [c1 c2]
    (apply hash-set (remove (fn [x] (contains? (apply hash-set (remove #(contains? c1 %) c2 ) ) x ) ) c2))
    ))

(defcheck solution-d8686a5b
  (fn [a b]
    (set (filter b a))))

(defcheck solution-d8972226
  (fn [x y]
    (set (filter #(contains? y %) x))))

(defcheck solution-d8c27bc
  #(set (filter (partial contains? %) %2)))

(defcheck solution-d8cfd07
  (fn [& sets]
    (set (for [x (first sets)
               :when (every? #(contains? % x) sets)]
           x))))

(defcheck solution-d92ee3de
  (fn [xs ys]
    (set (filter ys xs))))

(defcheck solution-d98d7496
  (fn [x y] (set (filter (complement nil?) (map #(x %1) y)))))

(defcheck solution-d9be89a8
  (fn [x y]
    (clojure.set/difference x (clojure.set/difference x y))))

(defcheck solution-d9c36f07
  (fn [l r] (into #{} (filter l r))))

(defcheck solution-d9da54ad
  (fn set-intersection [a b]
    (set (filter #(contains? b %) a))))

(defcheck solution-d9e3ca5e
  (fn inter [s1 s2] (set (filter (fn [x] (some #(= x %) s2)) s1))))

(defcheck solution-da295a48
  (fn [a b] (into #{} (filter #(and (a %) (b %)) (clojure.set/union a b)))))

(defcheck solution-da68c00a
  (fn [c d] (set (filter #(contains? c %) d))))

(defcheck solution-da9c44b8
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/union (clojure.set/difference % %2) (clojure.set/difference %2 %))))

(defcheck solution-db11e404
  (fn [x y]
    (set (remove nil? (map #(x %) y)))))

(defcheck solution-db3ca70c
  #(apply sorted-set (filter (fn [x] (contains? %1 x))%2)))

(defcheck solution-db6706a4
  #(into #{} (filter %1 (seq %2))))

(defcheck solution-dbb83bac
  (fn [A B]
    (set
      (for [a A b B :when (= a b) ] a))))

(defcheck solution-dbcbcc9a
  (fn [set1 set2]
    (disj (set (for [x set1]
                 (if (contains? set2 x)
                   x))) nil)))

(defcheck solution-dc229b03
  (fn [s1 s2]
    (reduce (fn [s3 x]
              (if (s2 x)
                (conj s3 x)
                s3))
      #{}
      s1)))

(defcheck solution-dd34345a
  #(set (filter (partial contains? %2) %1)))

(defcheck solution-dd660010
  (fn inter-section [s1 s2]
    (apply disj s1 (clojure.set/difference s1 s2))))

(defcheck solution-dda50262
  (fn [a b]
    (set (filter (fn [e] (b e)) a))))

(defcheck solution-ddb395d5
  (fn [s1 s2]
    (if (< (count s2) (count s1))
      (recur s2 s1)
      (loop [es (vec s1)
             r s1]
        (cond
          (empty? es) r
          :default
          (let [[e & re] es]
            (recur re
              (if (contains? s2 e) r (disj r e)))))))))

(defcheck solution-de235724
  #(->>
     (clojure.set/union %1 %2)
     (remove (clojure.set/difference %1 %2))
     (remove (clojure.set/difference %2 %1))
     set))

(defcheck solution-de8e8761
  (fn [as bs]
    (letfn [(elem? [x xs] (not (nil? (some #{x} xs))))]
      (reduce
        (fn [acc val]
          (if (elem? val bs)
            (conj acc val)
            acc))
        #{}
        as))))

(defcheck solution-deb24c79
  (fn [a b]
    (set
      (filter
        #(and (contains? a %) (contains? b %))
        (concat a b)))))

(defcheck solution-deb99f16
  (fn [s1 s2]
    (set
      (for [x s1
            y s2
            :when (= x y)]
        x))))

(defcheck solution-debe6865
  (fn [s1 s2] (into #{} (for [e1 s1 e2 s2 :when (= e1 e2)] e1))))

(defcheck solution-df04c0b1
  #(let [d clojure.set/difference u clojure.set/union] (d (u % %2) (u (d % %2) (d %2 %)))))

(defcheck solution-df0f8ee0
  (fn st
    ([s1 s2] (st #{} s1 s2))
    ([s s1 s2] (if (empty? s1) s
                               (if (nil? (s2 (first s1)))
                                 (st s (next s1) s2)
                                 (st (conj s (first s1)) (next s1) s2))))))

(defcheck solution-df62cb0
  (fn inter [A B]
    (set
      (remove nil?
        (for [elem A]
          (when (contains? B elem)
            elem
            )
          )
        )
      )
    ))

(defcheck solution-dfccb703
  #(clojure.set/difference  %1  (clojure.set/difference %1 %2)))

(defcheck solution-e039abc8
  (fn [s1 s2]
    (reduce
      #(if (contains? s2 %2) (conj % %2) %)
      #{} s1)))

(defcheck solution-e03ea69
  (fn set-intersection [set1 set2]
    (set (for [x set1
               :let [y x]
               :when (contains? set2 x)]
           y))))

(defcheck solution-e08449a4
  (fn [a b] (set (remove nil? (map #(if (contains? a %) % nil) b)))))

(defcheck solution-e0a0f62a
  (fn [set-1 set-2]
    (set (filter #(contains? set-2 %) set-1))
    ))

(defcheck solution-e21a2eb1
  (fn [set1 set2]
    (set
      (keys
        (select-keys
          (reduce #(assoc %1 %2 1) {} set1)
          set2)))))

(defcheck solution-e23ffbce
  (fn [s1 s2]
    (apply hash-set (filter #(contains? s2 %) s1))))

(defcheck solution-e295bd81
  #(set (filter (fn [x] (some #{x} %2)) %1)))

(defcheck solution-e2fbb776
  (fn [s1 s2]
    (set(filter #(not (nil? %)) (map #(get s2 %) s1)))))

(defcheck solution-e314932c
  (fn my-intersection
    [& sets]
    (into #{} (filter (fn [v] (every? #(contains? % v) sets))
                (apply clojure.set/union sets)))))

(defcheck solution-e31bcae7
  (fn [a b]
    (set
      (for [i a :when (contains? b i)] i))))

(defcheck solution-e3c9d912
  (fn f [a,b]
    (if (= 0 (count a))
      #{}
      (let [ff (f (rest a) b)]
        (if (get b (first a)) (conj ff (first a)) ff)))))

(defcheck solution-e501173f
  #(set (filter (fn [s] (contains? %2 s)) %1)))

(defcheck solution-e5c7802d
  #(set (filter (fn [val] (%1 val)) %2)))

(defcheck solution-e63ad5df
  (fn [& sets]
    (set (reduce (fn [a b] (filter #(a %) b)) sets))))

(defcheck solution-e6a90874
  (fn [x y] (let [combined (apply conj x y)]
              (set (filter #(and (x %) (y %)) combined)))))

(defcheck solution-e6e5f59c
  (fn [a b]
    (set (for [x a :when (b x)] x))
    ))

(defcheck solution-e745e6b7
  (fn [s t]
    (set (filter s t))))

(defcheck solution-e7472afd
  (fn[s1 s2]
    (reduce #(if (contains? s2  %2) (conj %1 %2) %1)  #{} s1)
    ))

(defcheck solution-e7741f32
  (fn [s1 s2]
    (set (remove nil? (map s1 (vec s2))))))

(defcheck solution-e78d50a0
  (fn [x y]
    (set (filter (partial contains? x) y))))

(defcheck solution-e89df6d3
  (fn [s1 s2] (into #{} (filter #(contains? s1 %) (seq s2)))))

(defcheck solution-e928105b
  (fn [a b]
    (set (filter a b))))

(defcheck solution-e9a0b2bf
  (fn[a,b](set (filter #(contains? a %) b))))

(defcheck solution-e9c138cd
  (fn [a b] (set (filter #(not (nil? %)) (map #(a %) b)))))

(defcheck solution-eacbb676
  #(into #{} (filter %1 %2)))

(defcheck solution-eb09bbf3
  #(let [d clojure.set/difference u (clojure.set/union % %2)] (d u (d u %) (d u %2))))

(defcheck solution-eb255efa
  #(set (keep %1 %2)))

(defcheck solution-eb97c81c
  #(apply hash-set (filter %1 %2)))

(defcheck solution-eb9b8055
  (fn [s1 s2] (clojure.set/difference s1 (clojure.set/difference s1 s2))))

(defcheck solution-ebd78d19
  (fn [x y] (into #{} (apply concat (map #(filter (fn[b] (= % b)) y) x )))))

(defcheck solution-ec33aa2d
  (fn [set1 set2]
    (set (for [s1 set1, s2 set2, :when (= s1 s2)] s1))))

(defcheck solution-ec5dc0f
  (fn [a b] (set (filter #(a %1) b))))

(defcheck solution-ec6b3510
  (fn [a b]
    (loop [o #{} x a]
      (if (empty? x)
        o
        (if (contains? b (first x))
          (recur (conj o (first x)) (rest x))
          (recur o (rest x)))))))

(defcheck solution-ed0a197a
  #(into #{} (filter (comp not nil?) (map %1 %2))))

(defcheck solution-edadff5f
  #(clojure.set/select (partial contains? %) %2))

(defcheck solution-edb65a21
  (fn f
    ([s1 s2]
     (f s1 s2 '()))
    ([s1 s2 r]
     (if (empty? s1)
       (set r)
       (if (contains? s2 (first s1))
         (recur (rest s1) s2 (into r (vector (first s1))))
         (recur (rest s1) s2 r))))))

(defcheck solution-ee1b65cd
  (fn [s1 s2]
    (set (concat (filter #(s2 %) s1) (filter #(s1 %) s2)))))

(defcheck solution-ee4a3fdc
  #(set (remove nil? (for [x %1 y %2] (when (= x y) x)))))

(defcheck solution-ef5cc1ba
  (fn [a b] (into #{} (filter #(contains? b %) a))))

(defcheck solution-ef6bb7b4
  (fn [set1 set2]
    (reduce (fn [acc v]
              (if (set2 v)
                (conj acc v)
                acc))
      #{}
      set1)))

(defcheck solution-efdb16fe
  #(into #{}
     (for [i %1
           :when (not (nil? (%2 i)))]
       i)))

(defcheck solution-f0c2d030
  (fn [s1 s2]
    (reduce #(if (contains? s1 %2) (conj %1 %2)  %1)
      #{} s2)))

(defcheck solution-f0d348ee
  (fn [s1 s2]
    (set (for [x s1 y s2 :when (= x y)] x))))

(defcheck solution-f10277c6
  (fn [x y] (set (filter #(and (contains? x %) (contains? y %)) (clojure.set/union x y)))))

(defcheck solution-f1aa53ad
  #(set (filter %1 %2)))

(defcheck solution-f1d5a75b
  (fn [s1 s2] (clojure.set/select #(s2 %) s1)))

(defcheck solution-f263f40f
  (fn [set1 set2]
    (reduce #(if (contains? set2 %2)
               (conj %1 %2)
               %1) #{} set1)))

(defcheck solution-f26ed396
  (fn [x y] (clojure.set/select #(contains? y %) x)))

(defcheck solution-f30a12c0
  (fn [a b]
    (set (filter b a))))

(defcheck solution-f310209a
  (fn [s z] (into #{} (mapcat #(if (contains? z %) [%]) s))))

(defcheck solution-f3c1d4e0
  #(set (filter % %2)))

(defcheck solution-f3cb6631
  (fn [ a b ]
    (reduce #(if (b %2) (conj %1 %2) %1) #{} a)))

(defcheck solution-f4057af3
  (fn [a b] (clojure.set/difference a (clojure.set/difference a b))))

(defcheck solution-f60c113c
  (fn my-intersection [s1 s2]
    (set (for [x s1 :when (contains? s2 x)] x))))

(defcheck solution-f64153e6
  (fn [set-a set-b]
    (set (filter #(contains? set-b %) set-a))))

(defcheck solution-f6821a40
  #(set (for [x (distinct (concat %1 %2)) :when (and (contains? %1 x)(contains? %2 x))] x)))

(defcheck solution-f6c6e52d
  #(set (keep % %2)))

(defcheck solution-f74b155b
  (fn [s1 & sets]
    (letfn [ (pred [key]
               (every? true? (map #(contains? % key) sets)))]
      (set (filter pred s1)))))

(defcheck solution-f763b2c8
  (fn [s1 s2] (into #{} (filter #(s2 %) s1))))

(defcheck solution-f777b3b2
  (fn [c1 c2]
    (let [all(set (concat c1 c2))
          r1 (set (map all c1))
          l1 (set (map r1 c2))]
      (set (drop-while nil? l1)))))

(defcheck solution-f93579f
  (fn [a b]
    (reduce
      (fn [sofar x]
        (if (and (a x) (b x))
          (conj sofar x)
          sofar))
      #{}
      a)))

(defcheck solution-f96485f8
  (fn my-intersection [s1 s2]
    (reduce #(if (contains? s2 %2) (conj % %2) %) #{} s1)))

(defcheck solution-fa19d6
  (fn [set1 set2]
    (loop [result #{} elements set1]
      (if (empty? elements)
        result
        (if (contains? set2 (first elements))
          (recur (conj result (first elements)) (rest elements))
          (recur result (rest elements))
          )
        )
      )
    ))

(defcheck solution-fa8c8a16
  (fn [c1 c2] (set (filter #(c2 %) c1))))

(defcheck solution-fb1d03ba
  (fn [x y] (set (filter #(not (nil? %)) (map #(some #{%} x) y)))))

(defcheck solution-fb5c5676
  (fn [a b]
    (set (filter identity
           (map #(if (contains? b %) %)
             a)))))

(defcheck solution-fbc9a960
  (fn [a b] (set (filter #(get a %) b))))

(defcheck solution-fc193cad
  #(loop [s1 %1 s2 %2 r []]
     (if (empty? s1)
       (set r)
       (let [x (first s1), s11 (rest s1)]
         (if (s2 x)
           (recur s11 s2 (conj r x))
           (recur s11 s2 r))))))

(defcheck solution-fc9152c0
  #(into #{} (filter (fn [x] (%2 x)) %1)))

(defcheck solution-fcc3c524
  (fn [s1 s2] (set (for [a s1 b s2 :when (= a b)] a))))

(defcheck solution-fd56363c
  (fn [x y] (loop [r x result #{}] (if (empty? r) result (recur  (rest r) (if (contains? y (first r)) (conj result (first r)) result))))))

(defcheck solution-fd65823a
  #(apply hash-set (for [x %1 y %2 :when (= x y)] x)))

(defcheck solution-fd8827dd
  (fn [a b] (reduce #(if-let [x (b %2)] (conj % %2) %) #{} a)))

(defcheck solution-fdb5e0a1
  (fn hey [x y]
    (if (empty? x)
      x
      (if (some #(= (first x) %) y)
        (set (concat (list (first x))
                     (hey (rest x) y)))
        (set (hey (rest x) y))))))

(defcheck solution-fde3d1
  (fn [xs ys]
    (into #{} (filter #(contains? ys %) xs))))

(defcheck solution-fe37143e
  (fn [a b]
    (reduce
      (fn [c v]
        (if (b v) (conj c v) c)) #{} a)))

(defcheck solution-ff13e441
  (fn myint [x y]
    (cond
      (empty? x) #{}
      (nil? (get y (first x))) (myint (rest x) y)
      true (conj (myint (rest x) y) (first x)))))

(defcheck solution-ff5eac64
  (comp set filter))

(defcheck solution-ff5fdfd1
  (fn [x, y]
    (set (filter #(contains? x %) y))
    ))

(defcheck solution-ffe04ef4
  (fn [a b]
    (set (mapcat
           #(if (and (a %) (b %))
              (list %)
              ())
           (clojure.set/union a b)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-81))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
