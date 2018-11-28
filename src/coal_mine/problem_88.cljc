(ns coal-mine.problem-88
  (:require [coal-mine.checks :refer [defcheck-88] :rename {defcheck-88 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-106132b0
  (fn [s1 s2]
    (set (concat (remove s1 s2) (remove s2 s1)))))

(defcheck solution-108da1e1
  #(set (into (filter (complement %) %2) (filter (complement %2) %))))

(defcheck solution-10c75b26
  #(set (filter (fn [x]
                  (not (and (contains? %1 x)
                            (contains? %2 x))))
          (concat (seq %1) (seq %2)))))

(defcheck solution-1233e697
  (fn [A B]
    (let [d (fn [a b] (remove #(contains? b %) a))]
      (set (into (d A B) (d B A))))))

(defcheck solution-124501e1
  #(set (for [z (into % %2) :when (not= z (% z) (%2 z))] z)))

(defcheck solution-126460f2
  (fn [a b] (set (mapcat identity ((juxt #(remove % %2) #(remove %2 %)) a b)))))

(defcheck solution-12bdb1a5
  (fn [set1 set2]
    (set (concat (remove set1 set2) (remove set2 set1)))))

(defcheck solution-134fa1ae
  (fn symdif [a b]
    (let [i (clojure.set/intersection a b)]
      (clojure.set/union
        (clojure.set/difference a i)
        (clojure.set/difference b i))
      )))

(defcheck solution-135c6d32
  #(into ((comp set remove) % %2) (remove %2 %)))

(defcheck solution-13aa9cd9
  (fn [colla collb]
    (loop [colla colla collb collb acc #{}]
      (if (empty? colla)
        (into acc collb)
        (if (contains? collb (first colla))
          (recur (rest colla) (disj collb (first colla)) acc)
          (recur (rest colla) collb (conj acc (first colla))))))))

(defcheck solution-141d3e58
  (fn my-sym-diff [s1 s2]
    (letfn [(my-diff [set1 set2] (filter #(not (contains? set2 %)) set1))]
      (set (concat (my-diff s1 s2) (my-diff s2 s1))))))

(defcheck solution-14b01447
  (fn [x y]
    (let [interection (into #{} (filter #(contains? y %) x))]
      (into #{} (concat (remove #(contains? interection %) x) (remove #(contains? interection %) y)))
      )
    ))

(defcheck solution-14c50146
  (fn [x y]
    (clojure.set/union (clojure.set/difference x y) (clojure.set/difference y x))))

(defcheck solution-151df95d
  #(set(into(filter (comp not %) %2) (filter (comp not %2) %))))

(defcheck solution-151e1ccd
  #(clojure.set/union
     (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-15336f4
  (fn [a b]
    (into (set (remove b a)) (remove a b))))

(defcheck solution-1560a2bf
  (fn [a b]

    (into #{} (for [x (into a b)

                    :when (not= (get b x) (get a x))] x))))

(defcheck solution-157bf5a6
  #(if (empty? %1)
     %2
     (if (empty? %2)
       %1
       (apply disj (apply conj %1 %2)(filter %1 %2))
       )
     ))

(defcheck solution-15831132
  (fn sym-diff [s t]
    (set (concat (clojure.set/difference s t) (clojure.set/difference t s)))))

(defcheck solution-167e22a3
  (fn [a b]
    (set (concat (apply disj a b) (apply disj b a)))))

(defcheck solution-16a6da96
  (fn [a b] (into #{} (concat (remove b a) (remove a b)))))

(defcheck solution-17849ea7
  (fn difference [x y]
    (letfn [(dif [z l union]
              (if (empty? l)
                (set z)
                (letfn [(count2 [x] (if (= x (first l)) 1 0))]
                  (if (> (reduce + (map count2 union)) 1)
                    (dif z (rest l) union)
                    (dif (cons (first l) z) (rest l) union)
                    )
                  )
                )
              )]
      (dif '() (concat x y) (concat x y))
      )
    ))

(defcheck solution-17fa4263
  (fn [& sets]
    (let [sets-intersection (apply clojure.set/intersection sets)
          sets-union (apply clojure.set/union sets)]
      (clojure.set/difference sets-union sets-intersection))))

(defcheck solution-180d8735
  (fn [l r]
    (set (concat (remove r l)
                 (remove l r)))))

(defcheck solution-182e7b40
  (fn [x y] (reduce (fn [s k] (if (contains? s k) (disj s k) (conj s k) ) ) x y) ))

(defcheck solution-18a45081
  (fn aa [x1 x2] (loop [x x1 y x2 z #{}]

                   (if-not (empty? x)

                     (if (x2 (first x))
                       (recur (rest x) y z)
                       (recur (rest x) y (conj z (first x)))
                       )


                     (if-not (empty? y)

                       (if (x1 (first y)) (recur x (rest y) z) (recur x (rest y) (conj z (first y))))
                       z
                       )
                     ))))

(defcheck solution-18ae3f3f
  (fn [x y]
    (apply disj (clojure.set/union x y) (clojure.set/intersection x y))))

(defcheck solution-18d9a7bd
  (fn [xs ys]
    (clojure.set/union (clojure.set/difference xs ys) (clojure.set/difference ys xs)
      )))

(defcheck solution-18e5a706
  (fn sym-diff [a b]
    (into #{}
      (mapcat
        identity
        (filter
          #(= 1 (count %))
          (vals (group-by identity (concat a b))))))))

(defcheck solution-190373a3
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-1964aba5
  (fn [a b] (clojure.set/union
              (clojure.set/difference a b)
              (clojure.set/difference b a))))

(defcheck solution-19af991b
  (fn [a b] (into #{} (concat (remove a b) (remove b a)))))

(defcheck solution-19df5ba8
  (fn [s1 s2] (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-1a6d028f
  #((comp set concat)  (apply disj %1 %2) (apply disj %2 %1)))

(defcheck solution-1ac15bba
  (fn [set1 & [set2]]
    (clojure.set/union
      (clojure.set/difference set1 set2)
      (clojure.set/difference set2 set1))))

(defcheck solution-1afeabd9
  (fn [s1 s2] (reduce (fn [acc e] (let [f (if (contains? acc e) disj conj)] (f acc e))) s1 s2)))

(defcheck solution-1b9c6f47
  (fn sym-diff [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-1bc5a8e
  (fn [a b] (let [f #(filter (comp not %) %2)](set (concat (f a b) (f b a))))))

(defcheck solution-1ce31ee
  (fn [x y]
    (set (filter (fn [n]
                   (letfn [(xor [a b] (if a (if b nil a) (if b b nil)))]
                     (apply xor (map #(contains? % n) [x y]))))
           (clojure.set/union x y)))))

(defcheck solution-1cf18e4c
  #(set (clojure.set/union (remove % %2) (remove %2 %))))

(defcheck solution-1d45ec0a
  (fn [s1 s2]
    (set
      (for [s (reduce conj s1 s2)
            :when (or (nil? (s1 s))
                      (nil? (s2 s)))]
        s))))

(defcheck solution-1d57057e
  (fn [s1 s2]
    (clojure.set/union
      (clojure.set/difference (clojure.set/union s2 s1) s2 )
      (clojure.set/difference (clojure.set/union s2 s1) s1 ))
    ))

(defcheck solution-1d63a119
  (fn [A B]
    (into (set (remove A B)) (remove B A))))

(defcheck solution-1d9b51e1
  (fn [a b] (let [f (fn [a b] (filter #(not (contains? a %)) b ))]
              (set (concat (f a b) (f b a) ) ) )))

(defcheck solution-1de74c86
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-1e9b37b8
  (fn [x y]
    (set (map first (get (group-by val (frequencies (concat x y))) 1)))))

(defcheck solution-1ec8bd25
  (fn [s1 s2]
    (set
      (for
       [x (concat s1 s2)
        :when (not (and (contains? s1 x) (contains? s2 x)))
        ]
        x
        )
      )
    ))

(defcheck solution-1efaa2a6
  (fn symm-difference [a b]
    (into (reduce (fn [out-set input-entry](if (contains? b input-entry) out-set (conj out-set input-entry))) #{} a)
      (reduce (fn [out-set input-entry](if (contains? a input-entry) out-set (conj out-set input-entry))) #{} b))
    ))

(defcheck solution-1f0cd8db
  (fn
    [sa sb]
    (into
      #{}
      (map
        #(first %)
        (filter
          #(= 1 (count (second %)))
          (group-by identity (concat '() sa sb)))))))

(defcheck solution-1f7d0e83
  (fn [a b]
    (letfn [
            (diff [a b] (reduce disj a b))
            (union [a b] (reduce conj a b))]
      (union (diff a b) (diff b a)))))

(defcheck solution-1f91f7e8
  (fn [ss1 ss2]
    (let [
          [s1 s2] (if (>= (count ss1) (count ss2)) [ss1 ss2] [ss2 ss1])
          u (reduce conj s1 s2)
          d1 (reduce disj u s1)
          d2 (reduce disj u s2)
          ] (reduce conj d1 d2)
            )
    ))

(defcheck solution-1fff9640
  #(letfn [(diff [s1 s2] (filter (complement s1) s2))]
     (into #{} (concat (diff %1 %2) (diff %2 %1)))))

(defcheck solution-202227f
  (fn [xs ys]
    (let [inter (clojure.set/intersection xs ys)]
      (clojure.set/union (clojure.set/difference xs inter)
        (clojure.set/difference ys inter)))))

(defcheck solution-203d8802
  (fn [set1 set2]
    (let [intersection (clojure.set/intersection set1 set2)]
      (clojure.set/union
        (clojure.set/difference
          set1
          intersection)
        (clojure.set/difference
          set2
          intersection)
        ))))

(defcheck solution-20cb8085
  (fn m [a b]
    (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))
    ))

(defcheck solution-21da0ecd
  (fn [a b]
    (letfn [(d [a b] (filter #(not (b %)) a))]
      (set (concat (d a b) (d b a))))))

(defcheck solution-220d19f7
  (fn sym-diff [sa sb]
    (set (concat (seq (reduce (fn [j k] (if (contains? sb k) j (conj j k))) #{} sa))
                 (seq (reduce (fn [j k] (if (contains? sa k) j (conj j k))) #{} sb))))))

(defcheck solution-22230248
  #(clojure.set/union (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-2224c9b
  (fn [s1 s2]
    (set (remove #(and (contains? s1 %) (contains? s2 %))
           (clojure.set/union s1 s2)))))

(defcheck solution-22a23a56
  (fn __ [s1 s2]
    (set (concat (filter #(not (contains? s2 %)) s1)
                 (filter #(not (contains? s1 %)) s2)))))

(defcheck solution-22c9b56e
  #(let [not-contains? (complement contains?)]
     (set (concat (filter (partial not-contains? %1) %2)
                  (filter (partial not-contains? %2) %1)))))

(defcheck solution-22e3bc34
  (fn [set1 set2]
    (clojure.set/union (clojure.set/difference set1 set2) (clojure.set/difference set2 set1))))

(defcheck solution-236cc790
  (fn [a b]  (set (concat
                   (filter
                     #(not (contains? a %)) b)
                   (filter
                     #(not (contains? b %)) a)
                   ))
    ))

(defcheck solution-2370fec4
  (fn [s1 s2]
    (reduce (fn [acc v] (if (and (s1 v) (s2 v)) (disj acc v) acc))
      (into s1 s2)
      (into s1 s2))))

(defcheck solution-245aa6b
  (fn [c1 c2]
    (set
      (concat
       (for [x c1 :when (not (contains? c2 x))] x)
       (for [x c2 :when (not (contains? c1 x))] x)))))

(defcheck solution-24b7c10f
  #(clojure.set/union (apply (partial disj %2) %) (apply (partial disj %) %2)))

(defcheck solution-24e4b498
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-25f55ea3
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-2603c06c
  (fn [s1 s2]
    (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-269fcb06
  #(into (apply disj %1 %2) (apply disj %2 %1)))

(defcheck solution-27cd5be5
  (fn [a b]
    (into
      (set (filter #(not (a %)) b))
      (set (filter #(not (b %)) a)))))

(defcheck solution-2854ff71
  (fn [l r]
    (into #{}  (concat  (filter #(not (l %)) r)
                        (filter #(not (r %)) l)))))

(defcheck solution-2869908d
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-29218d83
  (fn [a b]
    (let [diff (fn [x y] (filter #(not (y %)) x))]
      (set (concat (diff a b) (diff b a))))))

(defcheck solution-29db3e8c
  (fn[s1 s2](clojure.set/union
              (clojure.set/difference s1 (clojure.set/intersection s1 s2))
              (clojure.set/difference s2 (clojure.set/intersection s1 s2))
              )))

(defcheck solution-2a0a52e1
  (fn sym-diff [s1 s2]
    (let [intersection (filter s1 s2)]
      (set (concat (filter #(not ((set intersection) %)) s1)
                   (filter #(not ((set intersection) %)) s2))))))

(defcheck solution-2b8600e
  (fn [a b] (into #{} (filter #(or (not (a %)) (not (b %)))
                        (concat a b)))))

(defcheck solution-2c8f81e3
  (fn symmetric [xs ys]
    (let [
          remove-all (fn [xs ys]
                       (reduce #(disj %1 %2) xs ys))
          difference (fn [xs & xss]
                       (reduce remove-all xs xss))
          all-contain? (fn [yss]
                         (fn [x]
                           (every? #(contains? %1 x) yss)))
          intersection (fn [xs & xss]
                         (filter (all-contain? xss) xs))
          xsys (intersection xs ys)
          ]
      (set (concat (difference xs xsys)
                   (difference ys xsys))))))

(defcheck solution-2ca3d91b
  #(into (clojure.set/difference %2 %) (clojure.set/difference % %2)))

(defcheck solution-2daf19d6
  (fn [a b]
    (into #{}
      (concat (filter #(not (contains? b %)) a)
              (filter #(not (contains? a %)) b)))))

(defcheck solution-2ded5363
  #(into #{} (concat (filter (comp not %1) %2) (filter (comp not %2) %1))))

(defcheck solution-2e12e936
  (fn symdef[s1 s2]
    (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-2fe96097
  (fn [set1 set2]
    (into (clojure.set/difference set1 set2) (clojure.set/difference set2 set1))))

(defcheck solution-3042ad34
  (fn [xs1 xs2]
    (letfn [(diff [xs1 xs2] (filter #(not (contains? xs1 %)) xs2))]
      (set (concat (diff xs1 xs2) (diff xs2 xs1))))))

(defcheck solution-30ad8ef8
  (fn [a b]  (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-312b90c
  (fn sym-diff [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-312e393b
  (fn
    [a b]
    (let [set-xor (fn [k]
                    (let [ca (contains? a k)
                          cb (contains? b k)]
                      (or (and ca (not cb))
                          (and cb (not ca)))))]
      (set (filter set-xor (into a b))))))

(defcheck solution-313d4886
  #(set (map first (filter (fn [p] (= 1 (count (second p)))) (group-by identity (concat %1 %2))))))

(defcheck solution-31600864
  (fn [set_a set_b]
    (clojure.set/union (clojure.set/difference set_a set_b)
      (clojure.set/difference set_b set_a))))

(defcheck solution-3224c30c
  #(set (concat (apply disj %2 %) (apply disj % %2))))

(defcheck solution-32d22f4e
  #(set (concat (remove % %2) (remove %2 %) )))

(defcheck solution-32f8ae93
  (fn
    [s1 s2]
    (set (concat (filter #(not (contains? s1 %)) s2)
                 (filter #(not (contains? s2 %)) s1)))))

(defcheck solution-332566e3
  (fn [a b] (clojure.set/difference
              (clojure.set/union a b)
              (clojure.set/intersection a b))
    ))

(defcheck solution-3420101
  (fn [s1 s2]
    (set (filter #(not (and (contains? s1 %) (contains? s2 %))) (into s1 s2)))))

(defcheck solution-344a9011
  (fn [s1 s2] (set (concat (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))))

(defcheck solution-34dfa239
  #(set (reduce conj
          (remove % %2)
          (remove %2 %))))

(defcheck solution-35622377
  (fn [a b](letfn [(l [m n](if (contains? m n) (set (remove #(= n %) m)) (conj m n)))]
             (reduce l a b))))

(defcheck solution-3567226d
  (fn [s1 s2]
    (set (remove (set (filter s1 s2)) (into s1 s2)))))

(defcheck solution-3579aba7
  #(apply clojure.set/union (map clojure.set/difference [% %2] [%2 %])))

(defcheck solution-3582b49b
  (fn sym-diff [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-3597ca90
  (fn [s1 s2]
    (set (clojure.set/union (filter (complement #(contains? s2 %)) s1) (filter (complement #(contains? s1 %)) s2)))))

(defcheck solution-35e79ed5
  (fn [s1 s2]
    (letfn [(unique [xs ys] (reduce
                              (fn [unique item]
                                (if (contains? ys item)
                                  unique (conj unique item)))
                              [] xs))]
      (set (concat (unique s1 s2) (unique s2 s1))))))

(defcheck solution-3615af28
  (fn [a b]
    (clojure.set/union (set (remove a b)) (set (remove b a)))))

(defcheck solution-361b7c76
  (fn [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-369ade50
  #(into (apply disj % %2) (apply disj %2 %)))

(defcheck solution-3749655e
  #(set (into (filter (complement %1) %2) (filter (complement %2) %1))))

(defcheck solution-37e02c20
  (fn [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-37eaeba5
  (fn [s1 s2] (
                clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-384ddf11
  (fn [a b]
    (apply hash-set (concat (filter #(not (contains? b %)) a) (filter #(not (contains? a %)) b)))))

(defcheck solution-38611fba
  #(clojure.set/union (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-3883de48
  (fn [a b]
    (let [ret (transient a)]
      (persistent!
        (reduce
          (fn [ret v] (if (ret v) (disj! ret v) (conj! ret v)))
          ret b)))
    ))

(defcheck solution-39013cab
  (fn [set1 set2]
    (loop [ result [] coll (sort (concat set1 set2))]
      (if (empty? coll) (set (filter #(not (nil? %)) result))
                        (let [ [e1 e2] coll drop-num (if (= e1 e2) 2 1)]
                          #_(print [e1 e2])
                          (recur (conj result (if (= drop-num 1) e1))  (drop drop-num coll))
                          )
                        )
      )
    ))

(defcheck solution-39017c51
  #(clojure.set/union (clojure.set/difference %2 %) (clojure.set/difference % %2)))

(defcheck solution-391ac36c
  (fn [x, y]
    (letfn [(diff [a, b]
              (set (filter #(not (contains? b %)) a)))]
      (clojure.set/union (diff x y) (diff y x)))))

(defcheck solution-391bcec4
  (fn [x y]
    (let [differ #(clojure.set/difference % %2)]
      (clojure.set/union (differ x y) (differ y x)))))

(defcheck solution-393a4e6a
  (fn [x y]
    (set (concat (filter (complement x) y)
                 (filter (complement y) x)))))

(defcheck solution-3981d503
  (fn sym[s1 s2]
    (reduce conj
      (reduce disj s1 s2)
      (reduce disj s2 s1)
      )))

(defcheck solution-39ec5669
  #(let [d clojure.set/difference] (into  (d % %2) (d %2 %))))

(defcheck solution-3a61f39a
  #(clojure.set/union (clojure.set/difference % %2)(clojure.set/difference %2 %)))

(defcheck solution-3a79613b
  (fn [s1 s2] (set (filter (-> (filter s1 s2) set complement) (concat s1 s2)))))

(defcheck solution-3a91ffce
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-3aafffe5
  (fn [a b]
    (reduce (fn [acc v] (if (not (and (a v) (b v))) (conj acc v) acc)) #{} (clojure.set/union a b))))

(defcheck solution-3ad781a7
  (fn [a b] (->> (concat a b)
              (filter #(= 1 (count (filter nil?
                                     [(a %) (b %)]))))
              set)))

(defcheck solution-3aeb5be1
  (fn diff [a b]
    (into (set (filter (complement b) a))
      (filter (complement a) b))))

(defcheck solution-3b638e1f
  #(letfn [(minus [c1 c2]
             (loop [c1 c1, c2 c2, r []]
               (if (empty? c1)
                 r
                 (let [e (first c1), rr (if (c2 e) r (conj r e))]
                   (recur (rest c1) c2 rr)))))]
     (set (concat (minus %1 %2) (minus %2 %1)))))

(defcheck solution-3bb11aa2
  #(clojure.set/union (clojure.set/difference %1 %2)(clojure.set/difference %2 %1)))

(defcheck solution-3c937eac
  (fn [x y] (clojure.set/union (clojure.set/difference x y) (clojure.set/difference y x))))

(defcheck solution-3d376b7a
  (fn [xs ys] (set (concat (remove xs ys) (remove ys xs)))))

(defcheck solution-3d56f9f1
  #(let [d (fn [a b] (filter (comp not b) a))]
     (set (concat (d %2 %1) (d %1 %2)))))

(defcheck solution-3e3deb53
  (fn [a b]
    (set (filter #(not(and (contains? a %) (contains? b %))) (concat a b)))
    ))

(defcheck solution-3e52607a
  (fn symDiff[s1 s2]
    (set (filter
           #(not (and (contains? s1 %) (contains? s2 %)))
           (into s1 s2)))))

(defcheck solution-3ea2f5a5
  (fn [s t]
    (into
      (reduce disj (into s t) t)
      (reduce disj (into s t) s))))

(defcheck solution-3eb55059
  (fn sym-dif [s1 s2]
    (let [inter (clojure.set/intersection s1 s2)]
      (clojure.set/union (clojure.set/difference s1 inter)
        (clojure.set/difference s2 inter)))))

(defcheck solution-3ebe2187
  (fn [a b]
    (let [elems (concat a b)]
      (apply hash-set (remove #(and (a %) (b %)) elems)))))

(defcheck solution-3f191c2f
  (fn symm-diff [s1 s2]
    (let [f (fn [s1 s2]
              (reduce #(if (s1 %2) %1 (conj %1 %2)) #{} s2))]
      (clojure.set/union (f s1 s2) (f s2 s1)))))

(defcheck solution-3f4a899
  (fn [x y]
    (let [s (concat x y)]
      (into #{} (filter (fn [x] (= 1
                                  (apply + (map #(if (= % x) 1 0) s))))
                  s)))))

(defcheck solution-3f75257f
  (fn [x y]
    (reduce #(if (y %2) (disj % %2) (conj % %2)) y x)))

(defcheck solution-409a4a27
  (fn [set1 set2]
    (let [union #(set (concat %1 %2))
          diff #(filter (complement %1) %2)]
      (union (diff set1 set2) (diff set2 set1)))))

(defcheck solution-40b3ccad
  #(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1)))

(defcheck solution-40cdbd73
  (fn [xs ys]
    (into (into #{}
            (filter #(not (xs %)) ys))
      (filter #(not (ys %)) xs))))

(defcheck solution-41a07b59
  (fn [set1 set2]
    (into (clojure.set/difference set1 set2)
      (clojure.set/difference set2 set1))))

(defcheck solution-41e02d46
  (fn [s1 s2] (letfn [
                      (xor [bool1 bool2] (and (or bool1 bool2) (not (and bool1 bool2))))
                      (symmetric-difference [set1 set2] (set (filter #(xor (contains? set1 %) (contains? set2 %)) (clojure.set/union set1 set2))))
                      ] (symmetric-difference s1 s2))))

(defcheck solution-41ebacf7
  (fn sym-diff [x y]
    (clojure.set/union
      (clojure.set/difference x y)
      (clojure.set/difference y x))))

(defcheck solution-420d207c
  (fn sym-diff [s1 s2]
    (clojure.set/union
      (set (remove #(s2 %) s1))
      (set (remove #(s1 %) s2)))))

(defcheck solution-4245cc20
  (fn [s1 s2]
    (into (into #{} (remove #(s2 %) s1))
      (remove #(s1 %) s2))))

(defcheck solution-426f95ad
  #(let [keys (vec (filter %1 %2)),
         all (set (concat %1 %2))]
     (apply disj all keys) ))

(defcheck solution-42ce22f3
  #(into (set (remove %1 %2)) (set (remove %2 %1))))

(defcheck solution-438b2af7
  #(into #{} (filter (comp not (clojure.set/intersection %1 %2)) (clojure.set/union %1 %2))))

(defcheck solution-43adafc
  #(set (into (% %2 %3) (% %3 %2))) remove)

(defcheck solution-43e7d86
  (fn [c1 c2] (into (set (filter (comp not c1) c2)) (set (filter (comp not c2) c1)))))

(defcheck solution-4410bd8f
  #(set (concat (remove %2 %)
                (remove % %2))))

(defcheck solution-441b4ff4
  (fn symmetric-difference [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-444d9185
  (fn [s1 s2]
    (clojure.set/difference (into s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-45423e53
  (fn [& els]
    (let [all (apply concat els)
          grouped (group-by identity all)]
      (set
        (map first
          (filter (fn [[k v]] (= 1 (count v))) grouped))))))

(defcheck solution-460ab0e6
  (fn symm-diff [a b] (set (concat (filter (complement a) b) (filter (complement b) a)))))

(defcheck solution-4678711c
  (fn [s1 s2]
    (let [s1-only (filter #(not (contains? s2 %)) s1)
          s2-only (filter #(not (contains? s1 %)) s2)]
      (into #{} (concat s1-only s2-only)))))

(defcheck solution-478211cd
  (fn [s1 s2]
    (apply hash-set (filter #(= 1 (+ (if (get s1 %) 1 0)
                                     (if (get s2 %) 1 0))) (concat s1 s2)))))

(defcheck solution-47b54561
  #(into (reduce disj % %2) (reduce disj %2 %)))

(defcheck solution-47e9209a
  (fn [s1 s2]
    (set (filter #(not (and (contains? s1 %) (contains? s2 %))) (concat s1 s2)))))

(defcheck solution-482914a8
  (fn [s1 s2]
    (->> (merge-with + (frequencies s1) (frequencies s2))
      (filter #(= 1 (last %)))
      keys
      set)))

(defcheck solution-48859de7
  (fn [s1 s2]
    (into (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-48f25954
  #(set (remove (set (filter % %2)) (into % %2))))

(defcheck solution-49104612
  (fn [s1 s2]
    (let [n1 (apply disj s1 s2)
          n2 (apply disj s2 s1) ]
      (set (concat n1 n2))
      )))

(defcheck solution-4a532ade
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-4a6c4b5b
  (fn [x y]
    (let [q (fn [a b]
              (reduce
                #(if(contains? a %2)%1(cons %2 %1) )
                []
                b
                ))]
      (set (concat (q x y) (q y x )))
      )
    ))

(defcheck solution-4b864c8a
  #(clojure.set/union (clojure.set/difference % %2) (clojure.set/difference %2 %)))

(defcheck solution-4bdf3e86
  (fn symmetric-set-diff[s1 s2]
    (let[not-in-s1 (filter #(not(s1 %1))s2)
         not-in-s2 (filter #(not(s2 %1))s1)]
      (set(concat not-in-s1 not-in-s2)))))

(defcheck solution-4c22f7ea
  (comp (partial apply clojure.set/difference)
        (juxt clojure.set/union clojure.set/intersection)))

(defcheck solution-4c59877
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-4cab0414
  (fn set-sym-difference
    ([x y]
     (if (= 0 (count x))
       y
       (set-sym-difference x y #{})))
    ([x y z]
     (if (= 0 (count x))
       (apply conj y z)
       (if (contains? y (first x))
         (recur (disj x (first x)) (disj y (first x)) z)
         (recur (disj x (first x)) y (conj z (first x))))))))

(defcheck solution-4dcc22d1
  (fn [s1 s2] (set (concat (filter #(not (s1 %)) s2) (filter #(not (s2 %)) s1)))))

(defcheck solution-4def5b5
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-4e528f1f
  #(set (concat (clojure.set/difference %1 %2) (clojure.set/difference %2 %1)) ))

(defcheck solution-4e5ac888
  (fn [left right]
    (let [uni (clojure.set/union left right)
          inter (clojure.set/intersection left right)]
      (clojure.set/difference uni inter))))

(defcheck solution-4e7edc2c
  (fn [xs ys]
    (into #{} (map #(get % 0)
                (filter #(= 1 (second %)) (frequencies (concat xs ys)))))))

(defcheck solution-4ea1c1e0
  (fn [a b]
    (set
      (concat
       (apply disj a b)
       (apply disj b a)))))

(defcheck solution-4f5be96b
  (fn difference [s1 s2]
    (let [members (distinct (concat s1 s2))]
      (loop [m_ members acc []]
        (cond
          (empty? m_) (set acc)
          (not= (s1 (first m_)) (s2 (first m_)))
          (recur (rest m_) (conj acc (first m_)))
          :else (recur (rest m_) acc))))))

(defcheck solution-50162b37
  (fn [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-50608369
  (fn symdif [s1 s2]
    (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-50a555aa
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-50babbf6
  (fn [one two]
    (let [all (clojure.set/union one two)
          unique? (fn [i sets]
                    (= 1 (reduce + (map #(if (contains? % i) 1 0) sets))))]
      (set (filter #(unique? % [one two]) all)))))

(defcheck solution-50f01fdc
  #(into
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-515f179e
  (fn [x y]
    (let [diff clojure.set/difference
          union clojure.set/union]
      (union (diff x y) (diff y x)))))

(defcheck solution-518d2ed5
  (fn [x y]
    (let [union (set (filter x y))]
      (set (concat (remove union x) (remove union y))))))

(defcheck solution-51901f74
  (fn [s1 s2]
    (into #{} (filter #(not (and (contains? s2 %) (contains? s1 %))) (clojure.set/union s1 s2)))))

(defcheck solution-52651097
  (fn [x y]
    (clojure.set/union (clojure.set/difference x y)
      (clojure.set/difference y x))))

(defcheck solution-53592ab6
  (fn [s1, s2](clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-5384067a
  (fn sym-diff [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-53c19a52
  #(into (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-54781fc4
  (fn[s1 s2]
    (reduce
      (fn[s x]
        (if (contains? s x)
          (disj s x)
          (conj s x)
          )
        )s1 s2)
    ))

(defcheck solution-5497542
  (fn [a, b]
    (set (filter #(not (contains? (clojure.set/intersection a b) %))
           (concat a b)))))

(defcheck solution-5520e5d9
  (fn  [sa sb]
    (set
      (concat
       (filter (complement sb) sa)
       (filter (complement sa) sb)))))

(defcheck solution-563db898
  (fn [a b]
    (set (concat
          (filter #(not (a %)) b)
          (filter #(not (b %)) a)))))

(defcheck solution-56e296c7
  (fn [s1 s2]
    (set (for [thing (set (concat s1 s2))
               :when (or (not (s1 thing))
                         (not (s2 thing)))]
           thing))))

(defcheck solution-56e6caba
  #(clojure.set/difference (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-573c7124
  (fn [a b]
    (into #{}
      (filter #(if (a %)
                 (if (b %)
                   false
                   true)
                 (if (b %)
                   true
                   false))
        (clojure.set/union a b)))))

(defcheck solution-577bd1ab
  (fn [a b]
    (into (apply disj a (into () b))
      (apply disj b (into () a)))))

(defcheck solution-578a452b
  (fn sym-diff [s1 s2]
    (let [xs (into (into [] s1) s2)
          freq (frequencies xs)]
      (into #{}
        (map first (filter #(= (second %) 1)
                     freq))))))

(defcheck solution-57d389e0
  #(set (concat (apply disj %1 %2) (apply disj %2 %1))))

(defcheck solution-57dd6bd4
  (fn symmetricdifference [a b]
    (set (filter #(not (and (contains? a %) (contains? b %))) (into a b)))))

(defcheck solution-58245dec
  (fn set-difference [f s]
    (set (remove #(contains? (clojure.set/intersection f s) %) (clojure.set/union f s)))))

(defcheck solution-58312031
  (fn diff [a b] (let [d clojure.set/difference] (clojure.set/union (d a b) (d b a)))))

(defcheck solution-599efffc
  (fn [s1 s2] (set (filter #(not= (s1 %) (s2 %)) (into s1 s2))) ))

(defcheck solution-59d0d439
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-59de7718
  #(set (concat (filter (complement %2) %) (filter (complement %) %2))))

(defcheck solution-59f80a12
  (fn [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/intersection a b))))

(defcheck solution-5a681999
  #(into
     (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-5a909258
  (fn
    [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-5ac8638
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-5ad1b5da
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-5ad4967
  #(set (filter (complement (set (filter % %2))) (into % %2))))

(defcheck solution-5c0e6520
  (fn [a b] (set (filter #(not= (a %) (b %)) (clojure.set/union a b)))))

(defcheck solution-5c14fdc8
  #(apply (partial disj (clojure.set/union % %2))
     (clojure.set/intersection % %2)))

(defcheck solution-5cb10078
  (fn [& sets]
    (->> sets
      (mapcat vec)
      frequencies
      (filter (comp #(= % 1) last))
      (map first)
      set
      )))

(defcheck solution-5d298ea4
  #(set (remove (comp % %2) (into % %2))))

(defcheck solution-5d40edf5
  (fn [a b]
    (into #{} (concat (clojure.set/difference a b) (clojure.set/difference b a)))))

(defcheck solution-5daf6280
  (fn [s1 s2]
    (let [s (clojure.set/intersection s1 s2)]
      (clojure.set/union (clojure.set/difference s1 s)
        (clojure.set/difference s2 s)))))

(defcheck solution-5df47721
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-5e46204c
  #(into #{}
     (concat
      (filter (complement %2) %1)
      (filter (complement %1) %2))))

(defcheck solution-5e806f34
  (fn [a b]
    (->> (clojure.set/union a b)
      (filter #(not (and (a %) (b %))))
      (set))
    ))

(defcheck solution-5e972f98
  #(set `[~@(% %2 %3 %4) ~@(% %2 %4 %3)]) reduce disj)

(defcheck solution-5eaf523f
  (fn [s1 s2]
    (loop [all (into #{} (concat s1 s2)) rslt #{}]
      (let [a (first all)]
        (if (nil? a) rslt
                     (if (and (contains? s1 a) (contains? s2 a))
                       (recur (rest all) rslt)
                       (recur (rest all) (conj rslt a))))))))

(defcheck solution-5ef3db72
  (fn [a b]
    (reduce #(if (contains? a %2) %1 (conj %1 %2))
      (reduce #(if (contains? b %2) %1 (conj %1 %2)) #{} a)
      b)))

(defcheck solution-5f0fca63
  #(apply disj (clojure.set/union %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-5fd60f76
  (fn sd [s1 s2]
    (let [sd1 (clojure.set/difference s1 s2)
          sd2 (clojure.set/difference s2 s1)]
      (clojure.set/union sd1 sd2))))

(defcheck solution-6003879b
  (fn [xs ys]
    (clojure.set/union
      (clojure.set/difference xs ys)
      (clojure.set/difference ys xs))))

(defcheck solution-606a984f
  #(set (keep (fn [x] (if (and (% x) (%2 x)) nil x)) (into % %2))))

(defcheck solution-6085ab02
  (fn [a b]
    (apply disj

      (if
       (= b #{} )
        a
        (apply conj a b )
        )

      (set
        (filter a b)
        )
      )
    ))

(defcheck solution-6090078
  (fn xorset [x1 x2] (set (for [i (concat x1 x2) :when (not= (x1 i) (x2 i))] i))))

(defcheck solution-625a0c39
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b)
      (clojure.set/intersection a b))))

(defcheck solution-62b73ad2
  #(set (into (remove % %2) (remove %2 %))))

(defcheck solution-6310f958
  #(into
     (into #{} (filter (fn [x] (not (%1 x))) %2))
     (filter (fn [x] (not (%2 x))) %1)))

(defcheck solution-631b5940
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-63e30d31
  (fn [x y] (set (concat (filter #(not (contains? x %))y) (filter #(not (contains? y %)) x)))))

(defcheck solution-63f09499
  #(into
     (set (apply list (for [x %2 :when (not (contains? %1 x))] x)))
     (set (apply list (for [x %1 :when (not (contains? %2 x))] x)))
     ))

(defcheck solution-6510c5ee
  (fn [xs ys]
    (let [uni (set (concat xs ys))
          xsect (set (keep identity (map #(xs (ys %)) uni)))]
      (set (remove xsect uni)))))

(defcheck solution-656da677
  #(set (remove (clojure.set/intersection % %2) (clojure.set/union % %2))))

(defcheck solution-65b994fb
  (fn sym-diff
    [a b]
    (let [a-coll (->> a (filter #(nil? (b %))))
          b-coll (->> b (filter #(nil? (a %))))]
      (into #{} (concat a-coll b-coll)))))

(defcheck solution-660c99de
  #( clojure.set/difference (clojure.set/union %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-6647f0d2
  (fn [x y] (set (let [d clojure.set/difference] (concat (d x y) (d y x))))))

(defcheck solution-66891f9e
  (fn [x y]
    (apply hash-set (concat (filter #(nil? (get y %)) x)
                            (filter #(nil? (get x %)) y)))))

(defcheck solution-66d0efe0
  (fn [x y]
    (clojure.set/union (clojure.set/difference y x) (clojure.set/difference x y))
    ))

(defcheck solution-66e7f707
  (fn [a b] (clojure.set/union
              (clojure.set/difference a b)
              (clojure.set/difference b a))))

(defcheck solution-6835f07a
  (fn sym-diff
    [c1 c2]
    (let [r1 (set (remove c2 c1))
          r2 (set (remove c1 c2))]
      (clojure.set/union r1 r2))))

(defcheck solution-689c2765
  (fn [a b] (set (concat (remove a b) (remove b a)))))

(defcheck solution-68d0f63e
  (fn [a b]
    (set (into
           (filter #(not (a %)) b)
           (filter #(not (b %)) a)))))

(defcheck solution-68eef172
  (fn [s1 s2]

    (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))
    ))

(defcheck solution-6add5fb9
  (fn [a b]
    (into #{}
      (concat
       (filter (complement a) b) (filter (complement b) a)))))

(defcheck solution-6aeecb6
  #(set (mapcat remove [% %2] [%2 %])))

(defcheck solution-6bd43b27
  (fn [a b] (set (filter #(not (and (a %) (b %))) (into a b)))))

(defcheck solution-6bfff58b
  #(set (map first (filter (fn [[_ n]] (= 1 n)) (seq (frequencies (concat %1 %2)))))))

(defcheck solution-6c12fb1e
  (fn sym-diff [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-6cb7a096
  (fn [a b]
    (let [not-contains? (complement contains?)
          difference (fn [a b]
                       (into #{} (filter #(not-contains? a %) b) ))]
      (into (difference a b) (difference b a)))))

(defcheck solution-6d42f8bf
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)
     ))

(defcheck solution-6d5631b2
  #(set (concat (filter (complement %) %2)
                (filter (complement %2) %))))

(defcheck solution-6d9e12a6
  (fn [& xs]
    (reduce #(clojure.set/union (clojure.set/difference % %2) (clojure.set/difference %2 %)) xs)))

(defcheck solution-6de4972d
  #(set (filter (comp not ((comp set filter) %1 %2)) (into %1 %2))))

(defcheck solution-6e163092
  (fn simdif [a b] (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-6ee9d135
  (fn [s1 s2]
    (set
      (keys
        (filter
          #(= 1 (val %))
          (frequencies
            (into (vec s1) (vec s2))))))))

(defcheck solution-6eed739a
  (fn [s1 s2]
    (set (filter
           #(not (and (contains? s1 %) (contains? s2 %)))
           (clojure.set/union s1 s2)))))

(defcheck solution-6f4912ff
  #(clojure.set/union (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-7047a191
  (fn[a b]
    (-> #{}
      (into (remove b a))
      (into (remove a b)))))

(defcheck solution-70986ed0
  (fn symmetric-difference
    [a b]
    (into
      (set(remove a b))
      (remove b a))))

(defcheck solution-70b6e7cc
  (fn [s1 s2] (set
                (remove
                  #(contains? (clojure.set/intersection s2 s1) %)
                  (clojure.set/union s1 s2)))))

(defcheck solution-70c7d49c
  (fn
    [set1 set2]
    (clojure.set/union (clojure.set/difference set1 set2) (clojure.set/difference set2 set1))))

(defcheck solution-71e82039
  (fn [a b]
    (let [common (clojure.set/intersection a b)]
      (set
        (concat
         (remove common a)
         (remove common b))))))

(defcheck solution-72044fcc
  #(set (for[elem (set (concat %1 %2))
             :when (or
                    (and (contains? %1 elem) (not (contains? %2 elem)))
                    (and (contains? %2 elem) (not (contains? %1 elem))))]
          elem)))

(defcheck solution-721e5f74
  #(letfn [(set-dif [x y]
             (letfn [(worker [x y s]
                       (if (empty? x)
                         s
                         (recur (rest x) y (if (get y (first x)) s (conj s (first x))))))]
               (worker x y #{})))
           (set-union [x y]
             (if (empty? x)
               y
               (recur (rest x) (conj y (first x)))))]
     (set-union (set-dif %1 %2) (set-dif %2 %1))))

(defcheck solution-726e0e12
  (fn symmetric-difference
    [s1 s2]
    (set (concat (filter (comp not s1) s2) (filter (comp not s2) s1)))))

(defcheck solution-7281b97a
  #(set (remove (set (filter %1 %2)) (into %1 %2))))

(defcheck solution-73430c54
  #(clojure.set/difference (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-734dd0d4
  #(clojure.set/difference (clojure.set/union  %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-736f23e8
  (fn [X Y] (clojure.set/difference (clojure.set/union X Y) (clojure.set/intersection X Y))))

(defcheck solution-740d4b6e
  #(into (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-74254db6
  (fn [x y]
    (clojure.set/union (clojure.set/difference x y) (clojure.set/difference  y x))))

(defcheck solution-750cfa95
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-755337c4
  #(set (concat (remove % %2) (remove %2 %))))

(defcheck solution-7587f73
  (fn [x y]
    (clojure.set/difference (clojure.set/union x y) (clojure.set/intersection x y))))

(defcheck solution-759a6690
  (fn [as bs] (into #{} (lazy-cat (filter #(not (contains? bs %)) as)  (filter #(not (contains? as %)) bs)))))

(defcheck solution-75b305bb
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-75f4fb00
  (fn [col1 col2]
    (clojure.set/union
      (clojure.set/difference col1 col2)
      (clojure.set/difference col2 col1))))

(defcheck solution-762f18c9
  (fn [x y] (clojure.set/union
              (reduce (fn [acc i] (if (contains? y i) acc (conj acc i))) #{} x)
              (reduce (fn [acc i] (if (contains? x i) acc (conj acc i))) #{} y))
    ))

(defcheck solution-763b8889
  (fn [a b] (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-76532b0b
  (fn [c1 c2]
    (clojure.set/union (clojure.set/difference c1 c2) (clojure.set/difference c2 c1))))

(defcheck solution-771f86a3
  (fn [a b]
    (let [s (fn [a b] (filter #(nil? (a %1)) b))]
      (set (concat (s a b) (s b a))))))

(defcheck solution-773b6e2
  (fn [a b]
    (set (concat (filter #(not (contains? a %)) b) (filter #(not (contains? b %)) a)))))

(defcheck solution-78020e2f
  (fn [s1 s2]
    (let [diff (fn [s1 s2] (filter #(not (contains? s2 %)) s1))]
      (apply hash-set (concat (diff s1 s2) (diff s2 s1))))))

(defcheck solution-78a6a1b5
  (fn xor-set [lset rset]
    (cond
      (empty? lset) rset
      (empty? rset) lset
      :else
      (let [un (clojure.set/intersection lset rset)]
        (apply conj (apply disj lset un) (apply disj rset un))))))

(defcheck solution-78dd536c
  (fn [a b]
    (let [comp-func (fn [a b]
                      (filter #(not (contains? a %)) b))]
      (set (concat (comp-func a b)
                   (comp-func b a))))))

(defcheck solution-79076d3c
  (fn [a b]
    (let [sum (clojure.set/union a b)
          inters (clojure.set/intersection a b)]
      (clojure.set/difference sum inters))))

(defcheck solution-79125a0e
  (fn [s1 s2]
    (reduce disj (set (concat s1 s2)) (filter #(s1 %1) s2))))

(defcheck solution-79543aac
  (fn dif [a b] (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-79803f22
  #(into (set (for [a %1 :when (not (%2 a))] a))
     (for [a %2 :when (not (%1 a))] a)))

(defcheck solution-79abf958
  (fn sym-diff [a b]
    (clojure.set/union (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-79e179e2
  #(set(filter (fn[a](not (and (contains? % a) (contains? %2 a)))) (concat % %2))))

(defcheck solution-7a5e02f8
  (fn [s1 s2]
    (letfn [(sd [pred src]
              (for [i src
                    :when (nil? (pred i))]
                i))]
      (set (concat (sd s1 s2) (sd s2 s1))))))

(defcheck solution-7a94e14f
  (fn [s1 s2]
    (set (concat (filter #(not (contains? s2 %)) s1)
                 (filter #(not (contains? s1 %)) s2)))))

(defcheck solution-7ae57b50
  (fn [a b] (reduce disj (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-7c04b697
  #(into #{} (concat (remove % %2) (remove %2 %))))

(defcheck solution-7c1683c9
  (fn [x y] (set (concat (remove (set (filter x y)) x) (remove (set (filter y x)) y)))))

(defcheck solution-7caa3de9
  #(->> (for [x (into %1 %2)
              :when (not= (%1 x) (%2 x))] x)
     (into #{})))

(defcheck solution-7ccffb94
  (fn [a b]
    (set (filter
           #(not ((clojure.set/intersection a b) %))
           (clojure.set/union a b)))))

(defcheck solution-7d09e473
  #(reduce conj (apply disj %1 %2) (apply disj %2 %1)))

(defcheck solution-7d8a56a6
  (fn [a b]
    (let [diff-a (clojure.set/difference a b)
          diff-b (clojure.set/difference b a)]
      (clojure.set/union diff-a diff-b))))

(defcheck solution-7da56fc3
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-7dd77b22
  (fn f
    ([x y]
     (set (into (filter #(not (contains? x %)) y)
            (filter #(not (contains? y %)) x) ))
     )))

(defcheck solution-7e5f5a13
  (fn [set1 set2]
    (let [i (clojure.set/intersection set1 set2)]
      (clojure.set/union (clojure.set/difference set1 i)
        (clojure.set/difference set2 i)))))

(defcheck solution-7e7f346b
  #(set (remove (clojure.set/intersection %1 %2) (clojure.set/union %1 %2))))

(defcheck solution-7ee57903
  (fn [xs ys]
    (clojure.set/union (clojure.set/difference xs ys) (clojure.set/difference ys xs))))

(defcheck solution-7f16963c
  #(apply disj (into %1 %2) (filter %1 %2)))

(defcheck solution-7f474a3c
  (fn [s1 s2]
    (set (clojure.set/union (remove s2 s1) (remove s1 s2)))))

(defcheck solution-7fd40d01
  (fn sym-diff [seta setb]
    (let [a-not-b (filter #(nil? (setb %)) seta)
          b-not-a (filter #(nil? (seta %)) setb)]
      (set (concat a-not-b b-not-a)))))

(defcheck solution-80683be4
  (fn symmetric-difference [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-8091a40d
  (fn [x y] (clojure.set/difference (clojure.set/union x y) (clojure.set/intersection x y))))

(defcheck solution-80a6fac2
  (fn symmetric-difference
    [& sets]
    (into #{} (filter (fn [v] (and (not-every? #(contains? % v) sets)
                                   (some #(contains? % v) sets)))
                (apply clojure.set/union sets)))))

(defcheck solution-8115b181
  (fn [xs ys]
    (clojure.set/union
      (clojure.set/difference xs ys)
      (clojure.set/difference ys xs))))

(defcheck solution-81645c3e
  (fn sd
    ([set1 set2] (sd set1 set2 '()))
    ([set1 set2 difference] (if (empty? set1)
                              (set (concat difference set2))
                              (let [first (first set1)]
                                (if (set2 first)
                                  (recur (disj set1 first) (disj set2 first) difference)
                                  (recur (disj set1 first) set2 (conj difference first))))))))

(defcheck solution-8192a067
  (fn [a b]
    (let [c (clojure.set/intersection a b)]
      (set
        (concat
         (remove c a)
         (remove c b))))))

(defcheck solution-820595e9
  (fn [set1 set2]
    (let [sd (fn [a b] (remove a b))]
      (set (concat (sd set1 set2) (sd set2 set1))))))

(defcheck solution-82918bf6
  (fn [xs ys]
    (letfn [(f [xs ys]
              (loop [xs xs
                     acc #{}]
                (let [x (first xs)]
                  (cond
                    (nil? x) acc
                    (not (some #{x} ys)) (recur (rest xs) (conj acc x))
                    :else (recur (rest xs) acc)))
                ))]
      (set (concat (f xs ys) (f ys xs))))
    ))

(defcheck solution-829c0a14
  (fn [s1 s2]
    (let [d1 (clojure.set/difference s1 s2)
          d2 (clojure.set/difference s2 s1)]
      (clojure.set/union d1 d2))))

(defcheck solution-82c2d4b8
  (fn [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/intersection a b)
      )
    ))

(defcheck solution-83cb789
  #(letfn
    [(cat [a b] (set (remove (partial get a) b)))]
     (into (cat % %2) (cat %2 %))))

(defcheck solution-83e677c7
  (fn [a b]
    ( let [i  (set (map a b)),
           diff (fn [n m] (filter #(not (contains? m %)) n)),
           da (diff a i),
           db (diff b i)]
      (set (concat da db))
      )
    ))

(defcheck solution-83e6c6b6
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-84481a2e
  #(reduce disj (into % %2) (clojure.set/intersection % %2)))

(defcheck solution-846684f0
  (fn [set1 set2]
    (set
      (concat (remove #(contains? set2 %) set1)
              (remove #(contains? set1 %) set2)))))

(defcheck solution-8471d59f
  (fn [x y]
    (clojure.set/union
      (clojure.set/difference
        x
        y
        )
      (clojure.set/difference
        y
        x
        )
      )
    ))

(defcheck solution-85389307
  (fn symm-diff [set1 set2]
    (clojure.set/union
      (clojure.set/difference set1 set2)
      (clojure.set/difference set2 set1))))

(defcheck solution-856f3eec
  (fn [s1 s2]
    (into #{} (filter
                (fn [a] (not (and (contains? s1 a) (contains? s2 a))))
                (concat s1 s2)))))

(defcheck solution-85bee9cc
  #(clojure.set/union (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-85eaa2e6
  (fn [c1 c2]
    (set (filter (fn [v1]
                   (or
                    (and (contains? c1 v1) (not (contains? c2 v1)))
                    (and (contains? c2 v1) (not (contains? c1 v1)))))
           (concat c1 c2)))))

(defcheck solution-864cad34
  (fn [& args]
    (clojure.set/difference
      (apply clojure.set/union args)
      (apply clojure.set/intersection args))))

(defcheck solution-865a5828
  #(->> (apply concat %&)
     (group-by identity)
     (remove (comp next second))
     (map first), set))

(defcheck solution-86e43b57
  (fn [a b](set (keys (filter #(= (second %) 1) (merge-with + (frequencies a) (frequencies b)))))))

(defcheck solution-872ca544
  (fn [a b]
    (reduce (fn [s x]
              (if (or (and (contains? a x) (not (contains? b x)))
                      (and (contains? b x) (not (contains? a x))))
                (conj s x)
                s)) #{} (concat a b))))

(defcheck solution-8781c9f2
  (fn disjoint [a b]
    (set (concat (apply disj a b) (apply disj b a)))))

(defcheck solution-87fa9a30
  #(set
     (concat (filter (comp not %2) %1)
             (filter (comp not %1) %2))))

(defcheck solution-886747ed
  (fn sd [s1 s2]
    (let [i (set (filter s1 s2))
          u (into s1 s2)]
      (set (remove i u)))))

(defcheck solution-887a9470
  (fn [s1 s2]
    (set (concat
          (filter (complement s2) s1)
          (filter (complement s1) s2)))))

(defcheck solution-889d7474
  (fn [a b]
    (set (concat (filter #(not (a %)) b)
                 (filter #(not (b %)) a)))))

(defcheck solution-88a552c7
  (fn symm-diff [arg-1 arg-2]
    (let [un (clojure.set/union arg-1 arg-2) in (clojure.set/intersection arg-1 arg-2)]
      (clojure.set/difference un in)
      )
    ))

(defcheck solution-8925d9d8
  (fn this [xs ys]
    (into (set (filter #(not (contains? ys %)) xs))
      (set (filter #(not (contains? xs %)) ys)))))

(defcheck solution-893caa99
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-8952644c
  #(cond
     (empty? %1) %2
     (empty? %2) %1
     :else (apply disj (apply conj %1 %2) (filter %1 %2))))

(defcheck solution-89e86740
  (fn [& s]
    (let [intersection (reduce clojure.set/intersection s)
          union (reduce clojure.set/union s)]
      (set (filter #(not (some #{%} intersection)) union)))))

(defcheck solution-89f653e5
  (fn [s1 s2]
    (set (filter #(not (and (s1 %) (s2 %)))
           (if (empty? s2) s1 (apply conj s1 s2)) ))))

(defcheck solution-8a2575ad
  (fn symdiff [aset bset]
    (let [all (clojure.set/union aset bset)]
      (set (filter #(or (and (contains? aset %) (not (contains? bset %)))
                        (and (not (contains? aset %)) (contains? bset %)))
             all)))))

(defcheck solution-8af50af0
  (fn [s1 s2]
    (let [f (fn [sa sb r] (reduce #(if (contains? sb %2) % (conj % %2)) r sa))]
      (f s2 s1 (f s1 s2 #{})))))

(defcheck solution-8b4a04f
  (fn [s1 s2]
    (reduce (fn [ret this]
              (if-not (s1 this)
                (conj ret this)
                ret)) (reduce (fn [ret this]
                                (if-not (s2 this)
                                  (conj ret this)
                                  ret)) #{} s1) s2)))

(defcheck solution-8b4fb6d1
  (fn symmetric-difference [x y]
    (let [s1 (filter (partial (complement contains?) y) x)
          s2 (filter (partial (complement contains?) x) y)]
      (clojure.set/union (set s1) (set s2)))))

(defcheck solution-8cd3787d
  #(into (set (remove % %2)) (remove %2 %)))

(defcheck solution-8cf34ada
  #(set (apply concat (list (keep (fn[e](if (= (% e) nil) e nil)) %2) (keep (fn[e](if (= (%2 e) nil) e nil)) %)))))

(defcheck solution-8cff57d7
  (fn symdiff [left right]
    (clojure.set/difference (clojure.set/union left right) (clojure.set/intersection left right))))

(defcheck solution-8d3d90fc
  (fn [a b]
    (clojure.set/union
      (set (filter #(not (a %)) b))
      (set (filter #(not (b %)) a)))
    ))

(defcheck solution-8ec623ce
  (fn un-intersect [s1 s2]
    (apply hash-set
      (into
        (remove s1 s2)
        (remove s2 s1)))))

(defcheck solution-8f40452a
  #(clojure.set/difference (clojure.set/union %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-8f45b91e
  (fn symmetric-diff [s1 s2]
    (let [only-s1 (reduce (fn [accum val] (if (contains? s2 val) accum (conj accum val))) #{} s1)]
      (reduce (fn [accum val] (if (contains? s1 val) accum (conj accum val))) only-s1 s2))))

(defcheck solution-8f800370
  #(set (concat
         (filter (comp not %1) %2)
         (filter (comp not %2) %1))))

(defcheck solution-904b17f
  #(set (concat (clojure.set/difference % %2) (clojure.set/difference %2 %))))

(defcheck solution-90a9e9f4
  (fn symdif
    ([x y] (set (concat (symdif x y 0) (symdif y x 0))))
    ([x y z]
     (if (and (empty? x) (= z 0))
       []
       (if (some #(= (first x) %) y)
         (set (concat (symdif (rest x) y 0)))
         (set (concat (list (first x))
                      (symdif (rest x) y 0))))))))

(defcheck solution-90ab3d88
  (fn [seta setb]
    (let [s (set (for [a seta :when (not (setb a))]
                   a))
          t (set (for [b setb :when (not (seta b))]
                   b))]
      (reduce conj s t))))

(defcheck solution-90fb2f9f
  (fn [set1 set2]
    (clojure.set/union (clojure.set/difference set1 set2)
      (clojure.set/difference set2 set1))))

(defcheck solution-9210a64a
  (fn [s t] (clojure.set/difference (clojure.set/union s t) (clojure.set/intersection s t))))

(defcheck solution-92336bc3
  #(set (keep (fn [[k v]] (if (= 1 v) k nil)) (frequencies (concat % %2)))))

(defcheck solution-92a2f32d
  (fn  [s1 s2]
    (let [c (distinct (concat s1 s2))
          f (fn [x] (let [b1 (contains? s1 x)
                          b2 (contains? s2 x)]
                      (or (and b1 (not b2))
                          (and b2 (not b1)))))]
      (set (filter f c)))))

(defcheck solution-937619b8
  (fn [s1 s2] (into #{} (filter #(not (and (contains? s1 %) (contains? s2 %))) (concat s1 s2)))))

(defcheck solution-938f8a24
  #(set
     (concat
      (remove (partial contains? %1) %2)
      (remove (partial contains? %2) %1))))

(defcheck solution-93c7e6fe
  (fn [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/intersection a b))))

(defcheck solution-941cd1e0
  (fn [a b] (let [xor #(and (not (and % %2)) (or % %2))] (reduce #(if (xor (a %2) (b %2)) (conj % %2) %) #{} (clojure.set/union a b)))))

(defcheck solution-94658b0c
  (fn [s1 s2] (set (filter #(not (and (contains? s1 %) (contains? s2 %)))
                     (clojure.set/union s1 s2)))))

(defcheck solution-95048ffb
  (fn[xs ys]
    (clojure.set/difference
      (clojure.set/union xs ys)
      (clojure.set/intersection xs ys))))

(defcheck solution-95ab0cc8
  (fn [s1 s2]
    (set (concat (filter (complement s1) s2)
                 (filter (complement s2) s1)))))

(defcheck solution-95b617ec
  (fn sym-diff [a b] (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-96529cf8
  #(apply disj (into %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-967156e3
  (fn [s1 s2]
    (let [d1 (filter (comp not s1) s2)
          d2 (filter (comp not s2) s1)]
      (into (into #{} d1) d2))))

(defcheck solution-9710307d
  (fn [s1 s2]
    (letfn
     [(difx
        [bs cs]
        (if (empty? cs) #{}
                        (let [fe (first cs) re (clojure.set/difference cs #{fe})
                              difxre (difx bs re)]
                          (if (contains? bs fe)
                            difxre (conj difxre fe))
                          )))]
      (clojure.set/union (difx s1 s2) (difx s2 s1))
      )))

(defcheck solution-97497a59
  (fn [as bs]
    ((comp set concat) (remove (fn [a] (contains? bs a)) as)
     (remove (fn [b] (contains? as b)) bs))))

(defcheck solution-9756479
  (fn [s1 s2]
    (reduce conj
      (reduce disj s1 s2)
      (reduce disj s2 s1))))

(defcheck solution-97e8cbb
  (fn symdiff
    [s1 s2] {:pre [(set? s1), (set? s2)]}
    (clojure.set/difference (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-97f15f21
  (fn sdiff [a b]
    (
     (fn [y x]  (set (filter #(not (contains? x %)) y)))
     (set (concat a b))
     (set (filter #(contains? a %) b))
     )))

(defcheck solution-984bbd92
  (fn [a b]
    (reduce (fn [val x]
              (if (not (and (a x) (b x)))
                (conj val x)
                val)) #{} (into a b))))

(defcheck solution-985a9d76
  #(let [s (map % %2)] (set (remove (set s) (into % %2)))))

(defcheck solution-9897fa8c
  (fn diff [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-996e4756
  (fn [s1 s2]
    (reduce (fn [r e]
              (if (contains? s1 e)
                r
                (conj r e)))
      (reduce (fn [r e]
                (if (contains? s2 e)
                  r
                  (conj r e)))
        #{}
        s1)
      s2)))

(defcheck solution-9a5d62ae
  (fn symmetric-diff [z z2]
    (set (concat (clojure.set/difference z z2) (clojure.set/difference z2 z)))))

(defcheck solution-9aa587cf
  (fn [a b]
    (letfn [(f [a b] (set (filter (comp not b) a)))]
      (set (concat (f a b) (f b a)))
      )
    ))

(defcheck solution-9ac95b68
  (fn [k j] (clojure.set/union (clojure.set/difference j k) (clojure.set/difference k j))))

(defcheck solution-9aea56ed
  reduce (fn [s2 el] ((if (s2 el) disj conj) s2 el)))

(defcheck solution-9b08dd8
  (fn symmetric-difference [coll-a coll-b]
    (letfn [(build-diff [coll-a coll-b]
              (filter #(not (contains? coll-a %)) coll-b))]
      (set (concat (build-diff coll-a coll-b) (build-diff coll-b coll-a))))))

(defcheck solution-9c2b0aaf
  (fn [l r] (into (set (remove l r)) (remove r l))))

(defcheck solution-9c76dfbd
  #(set (concat (remove %2 %1)
                (remove %1 %2))))

(defcheck solution-9c7f2d2c
  (fn [a b] (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-9cf801f6
  (fn [s1 s2]
    (-> #{}
      (into (filter (comp not #(contains? s2 %)) s1))
      (into (filter (comp not #(contains? s1 %)) s2)))))

(defcheck solution-9d14af89
  (fn myfun
    [set1 set2]

    (set

      (concat

       (filter #(= false (contains? set2 %))  set1)
       (filter #(= false (contains? set1 %))  set2)

       )


      )))

(defcheck solution-9d1635f1
  (fn [s1 s2]
    (let [u (into s1 s2)
          d (partial clojure.set/difference u)]
      (into (d s1)
        (d s2)))))

(defcheck solution-9d2f2161
  reduce #((if (% %2) disj conj) % %2))

(defcheck solution-9dd1b1e2
  (fn [a b] (into (set (remove a b)) (remove b a))))

(defcheck solution-9e0cfacb
  (fn [a b]
    (letfn [
            (d[x y] (filter #(not (contains? x %)) y))]
      (set (concat (d b a) (d a b))))))

(defcheck solution-9e3bf841
  (fn p [col1 col2]
    (clojure.set/union (clojure.set/difference col1 col2)(clojure.set/difference col2 col1))))

(defcheck solution-9e591c44
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-9e5f7c59
  #(set (filter (fn [x] (not= (%1 x) (%2 x))) (into %1 %2))))

(defcheck solution-9fdc3d4
  (fn [s1 s2]
    (let [s1-and-s2 (clojure.set/intersection s1 s2)]
      (apply disj (clojure.set/union s1 s2) s1-and-s2))))

(defcheck solution-a062a058
  #(set (filter (fn [v] (not (and (contains? %1 v) (contains? %2 v)))) (concat %1 %2))))

(defcheck solution-a07d9766
  (fn [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-a19c2791
  (fn [S T] (clojure.set/union (apply disj S T) (apply disj T S))))

(defcheck solution-a2a49e30
  (fn __ [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-a2a60568
  #(let [d clojure.set/difference]
     (clojure.set/union (d % %2) (d %2 %))))

(defcheck solution-a314056a
  (fn [a b] (set (concat (clojure.set/difference a b) (clojure.set/difference b a)))))

(defcheck solution-a37e81ad
  (fn [x y] (set(filter #(not(contains? (clojure.set/intersection x y) %)) (concat x y)))))

(defcheck solution-a383d96
  #(set
     (remove (clojure.set/intersection % %2)
       (clojure.set/union % %2))))

(defcheck solution-a39f808e
  #(set (concat (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))))

(defcheck solution-a3b2aa42
  (fn diff [s1 s2]
    (letfn
     [(xor [a b]
        (or
         (and a (not b))
         (and b (not a))
         )
        )]
      (set (filter #(xor (s1 %) (s2 %)) (clojure.set/union s1 s2)))
      )
    ))

(defcheck solution-a3cf97fb
  #(clojure.set/union (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-a423337d
  (fn symdif [s1 s2]
    (clojure.set/union
      (set (for [e1 s1
                 :when (nil? (s2 e1))] e1))
      (set (for [e2 s2
                 :when (nil? (s1 e2))] e2)))))

(defcheck solution-a457c11
  (fn [a b]
    (set (filter
           (fn [e] (or (nil? (a e))(nil? (b e))))
           (concat a b)))))

(defcheck solution-a49fa6c3
  (fn [a b] (letfn [(d [a b] (for [x a :when (not (b x))] x))]
              (set (concat (d a b) (d b a))))))

(defcheck solution-a53661a
  (fn [x y]
    (set (concat
          (filter (complement x) y)
          (filter (complement y) x)))))

(defcheck solution-a54726b5
  #(clojure.set/difference (into % %2) (filter % %2)))

(defcheck solution-a56b1b5a
  (fn [s1 s2] (into #{} (concat (seq (filter #(not (contains? s2 %)) (seq s1))) (seq (filter #(not (contains? s1 %)) (seq s2))) ))))

(defcheck solution-a630c45f
  #(into (clojure.set/difference %1 %2) (clojure.set/difference %2 %1)))

(defcheck solution-a71a126d
  (fn [s1 s2]
    (clojure.set/difference (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-a79a88bb
  (fn [a b] (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b ))))

(defcheck solution-a94004be
  (fn[s1 s2]
    (let [d1 (filter #(not (contains? s1 %)) s2)
          d2 (filter #(not (contains? s2 %)) s1)]
      (set (concat d1 d2)))))

(defcheck solution-aa4abc60
  #(set (for [x (into %1 %2) :when (not= (some #{x} %1) (some #{x} %2))] x)))

(defcheck solution-aa954643
  (fn f[a b]
    (set(filter #(not(and (a %) (b %))) (concat a b)))))

(defcheck solution-aabf7b72
  (fn [s1 s2]
    (let [s (reduce conj s1 s2)
          c (filter #(contains? s1 %) s2)
          m (zipmap s (map #(.indexOf c %) s))
          ]
      (into #{} (for [[k v] m :when (neg? v)] k)))))

(defcheck solution-abb370c5
  #(reduce conj
     (reduce disj % %2)
     (reduce disj %2 %)))

(defcheck solution-ac13d45b
  (fn[s r]
    (letfn
     ([f[s r]
       (set
         (filter #(not (r %)) s))])
      (into (f s r) (f r s)))))

(defcheck solution-ac69d838
  (fn [left right]
    (clojure.set/union (clojure.set/difference left right) (clojure.set/difference right left))))

(defcheck solution-acbefce1
  (fn symmetric-dif [s1 s2]
    (let [d1 (reduce #(if (contains? s1 %2)
                        %1
                        (conj %1 %2)) #{} s2)
          d2 (reduce #(if (contains? s2 %2)
                        %1
                        (conj %1 %2)) #{} s1)]
      (clojure.set/union d1 d2))))

(defcheck solution-accec664
  (fn [s1 s2]
    (let [both (clojure.set/intersection s1 s2)]
      (set (clojure.set/union (remove both s1)
             (remove both s2))))))

(defcheck solution-acfc85f2
  (fn [x,y]
    (set
      (filter
        #(or
          (and (not (contains? x %)) (contains? y %))
          (and (contains? x %) (not (contains? y %))) )
        (concat x y)))))

(defcheck solution-ad1f982b
  (fn symetric-diff [s1 s2]
    (set (concat (filter #(not (contains? s2 %)) s1) (filter #(not (contains? s1 %)) s2)))))

(defcheck solution-ad36ca2b
  (fn [x y]
    (set (remove
           #(and (contains? x %) (contains? y %))
           (into x y)))))

(defcheck solution-ad4fda1
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-ad54bae2
  (fn simmDiffX [x y] (let [filterX (fn [x y] (filter (fn [a] (not (contains? y a))) x))] (set (concat (filterX x y)(filterX y x))))))

(defcheck solution-ad5c86b2
  (fn [s t]
    (clojure.set/union
      (clojure.set/difference s t)
      (clojure.set/difference t s))))

(defcheck solution-ad8c384
  (fn [A B]
    (set (concat (filter (complement A) B)
                 (filter (complement B) A)))))

(defcheck solution-adcebe99
  (fn [s1 s2]
    (set
      (concat
       (remove s1 s2)
       (remove s2 s1)))))

(defcheck solution-ae028dc2
  (fn [s1 s2]
    (into (set
            (filter #(not (contains? s1 %)) s2))
      (filter #(not (contains? s2 %)) s1))))

(defcheck solution-ae70f383
  #(set
     (concat
      (filter (complement %2) %)
      (filter (complement %) %2))))

(defcheck solution-ae9464a2
  #(reduce disj (into % %2) (filter % %2)))

(defcheck solution-afc91da9
  (fn [a b]
    (loop [vs (if (empty? b)
                a
                (apply conj a b))
           rs #{}]
      (cond
        (empty? vs) rs
        (and (a (first vs)) (b (first vs))) (recur (rest vs) rs)
        (or (a (first vs)) (b (first vs))) (recur (rest vs) (conj rs (first vs)))
        :else (recur (rest vs) rs)))))

(defcheck solution-b10e434c
  (fn [s1 s2] (apply disj (set (concat s1 s2)) (set (filter #(contains? s2 %) s1)))))

(defcheck solution-b11561c0
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-b1659565
  #(clojure.set/union (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-b221e7a7
  (fn symmetric-difference
    [left right]
    (let [left-difference (fn [left right]
                            (loop [[h & t] (into [] left)
                                   accum #{}]
                              (let [new-accum (if (contains? right h)
                                                accum
                                                (conj accum h))]
                                (if (nil? t)
                                  new-accum
                                  (recur t new-accum)))))]
      (if (empty? left)
        right
        (if (empty? right)
          left (into (left-difference left right) (left-difference right left)))))))

(defcheck solution-b253b860
  #(set(concat (remove %1 %2) (remove %2 %1))))

(defcheck solution-b29af0b6
  #(set (concat (remove %2 %1) (remove %1 %2))))

(defcheck solution-b29b0d59
  #(set (filter
          (fn[x] (= 1 (+ (if (%1 x) 1 0) (if (%2 x) 1 0))))
          (clojure.set/union %1 %2))))

(defcheck solution-b2aedbb4
  (fn sym-dif
    [s1 s2]
    (let [inter (clojure.set/intersection s1 s2)]
      (into (into #{} (clojure.set/difference s1 inter)) (clojure.set/difference s2 inter)))))

(defcheck solution-b34a3f3c
  (fn sym [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-b3513345
  #(set (apply concat (filter (fn [x](= (count x) 1)) (vals (group-by identity (concat %1 %2)))))))

(defcheck solution-b3787205
  (fn [a b]
    ((comp set concat)
     (remove a b)
     (remove b a))))

(defcheck solution-b3925a08
  (fn [a b] (apply disj (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-b3ea1e2f
  #(clojure.set/difference
     (set (concat %1 %2))
     (clojure.set/intersection %1 %2)))

(defcheck solution-b4f22575
  (fn [x y]
    (clojure.set/difference (set (concat x y)) (clojure.set/intersection x y))))

(defcheck solution-b50a2a39
  (fn [s1 s2] (let [d1 (for [s s1
                             :when (not (s2 s))]
                         s)
                    d2 (for [s s2
                             :when (not (s1 s))]
                         s)]
                (set (lazy-cat d1 d2)))))

(defcheck solution-b5474c52
  (fn mydif [a b]
    (let [in-same (set (for [x a y b :when (= x y)] x))]
      (set (for [x (reduce conj a b) :when (nil? (in-same x))]
             x)))))

(defcheck solution-b5ed7c97
  (fn [set1 set2]
    (cond (empty? set1) set2
          (empty? set2 ) set1
          :else
          (let [allset (into set1 set2)
                comset (set (reduce into (map #(if (get set2 %) #{%} nil) set1 )))
                ]
            (set (reduce into (map #(if (get comset %) nil #{%})  allset)))

            )

          )

    ))

(defcheck solution-b643085c
  #(cond
     (empty? %1) %2
     (empty? %2) %1
     :else (apply conj (apply disj %1 %2) (apply disj %2 %1))))

(defcheck solution-b6554b06
  (fn [a b]
    (into (into #{} (clojure.set/difference a b)) (clojure.set/difference b a))))

(defcheck solution-b6b08c2f
  (fn diffs [s1 s2]
    (set (concat
          (clojure.set/difference s1 s2)
          (clojure.set/difference s2 s1)))))

(defcheck solution-b71d3034
  (fn [a b] (set (remove #(and (a %) (b %)) (clojure.set/union a b)))))

(defcheck solution-b72cc1c3
  #(set (filter (fn [x] (not= (%1 x) (%2 x))) (concat %1 %2))))

(defcheck solution-b7407d42
  (fn diff [xs ys]
    (apply hash-set (lazy-cat (remove xs ys) (remove ys xs)))))

(defcheck solution-b751b71e
  (fn [s1 s2]
    (let [all (frequencies (concat s1 s2))]
      (set (filter #(= (all %) 1) (keys all))))))

(defcheck solution-b755e4d7
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-b7df99ee
  (fn symdiff[res spart]
    (if (empty? spart)
      (set res)
      (let [a (first spart)]
        #_(println "a: " a " res:" res " spart: " spart " a in res? " (contains? res a))
        (if (contains? res a)
          (symdiff (set(remove (fn[x](= a x)) res)) (set (rest spart)))
          (symdiff (set(cons a res)) (set (rest spart)))
          )
        )
      )
    ))

(defcheck solution-b7ecb1e1
  #(set (into (remove %1 %2) (remove %2 %1))))

(defcheck solution-b8b82a7a
  (fn [a b]
    (set (concat (clojure.set/difference b a) (clojure.set/difference a b)))))

(defcheck solution-b9a08965
  (fn symmetric-difference [s1 s2]
    (let [i (clojure.set/intersection s1 s2) u (clojure.set/union s1 s2)]
      (into #{} (filter #(not (contains? i %)) u)))))

(defcheck solution-bbb09a55
  (fn sd [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-bbdcecd
  (fn [xs ys]
    (set (into (remove xs ys) (remove ys xs)))))

(defcheck solution-bbe0264d
  (fn [xs ys]
    (clojure.set/difference
      (clojure.set/union xs ys)
      (clojure.set/intersection xs ys))))

(defcheck solution-bc10fb39
  (fn [s1 s2]
    (set (into (filter (complement s1) s2) (filter (complement s2) s1)))))

(defcheck solution-bc2d4087
  #(set `(~@(% %2 %3) ~@(% %3 %2))) remove)

(defcheck solution-bc4dacf8
  (fn [m1 m2]
    (let [common (clojure.set/intersection m1 m2)]
      (clojure.set/union
        (clojure.set/difference m1 common)
        (clojure.set/difference m2 common)))))

(defcheck solution-bc596c1f
  (fn [x y]
    (set
      (filter #(not (= (get x %) (get y %)))
        (concat x y)))))

(defcheck solution-bc9b0a28
  (fn [a b]
    (let [u (into a b)]
      (loop [x (seq u)
             acc #{}]
        (if (nil? x)
          acc
          (let [h (first x)]
            (if (and (contains? a h) (contains? b h))
              (recur (next x) acc)
              (recur (next x) (conj acc h)))))))))

(defcheck solution-bccd1236
  (fn [a b]
    (set (concat
          (filter #(not (a %)) b)
          (filter #(not (b %)) a)
          ))
    ))

(defcheck solution-bcd031c3
  #(set (for [x (clojure.set/union % %2)
              :when (or (and (% x) (not (%2 x)))
                        (and (not (% x)) (%2 x)))] x)))

(defcheck solution-bcf305c8
  (fn [x y]
    (clojure.set/difference (clojure.set/union x y)
      (clojure.set/intersection x y))))

(defcheck solution-bd196b43
  #(clojure.set/union
     (clojure.set/difference %1 %2) (clojure.set/difference %2 %1) ))

(defcheck solution-bd504432
  (fn [& sets] (clojure.set/difference (apply clojure.set/union sets) (apply clojure.set/intersection sets))))

(defcheck solution-be44bc3e
  #(set (concat (remove % %2)(remove %2 %))))

(defcheck solution-be7806d5
  (fn [s1 s2]
    (let [diff (fn [s es]
                 (into #{} (filter #(not (contains? es %)) s)))]
      (into (diff s1 s2) (diff s2 s1)))))

(defcheck solution-be93bd9
  (fn my-sym-diff [a b]
    (set (concat
          (filter #(not (a %)) b)
          (filter #(not (b %)) a)))))

(defcheck solution-becccbbf
  (fn [a b]
    (let [a-f (filter #(not (b %)) a)
          b-f (filter #(not (a %)) b)]
      (set (concat a-f b-f)))))

(defcheck solution-bfdfd5b8
  (fn [set1 set2]
    (clojure.set/union
      (clojure.set/difference set1 set2)
      (clojure.set/difference set2 set1))))

(defcheck solution-bfe7b74d
  #(clojure.set/difference (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-c04d658e
  (fn [A B]
    (letfn [(f [C]
              (fn [acc x]
                (if (contains? C x)
                  acc
                  (conj acc x))))]
      (reduce (f A)
        (reduce (f B) #{} A)
        B))))

(defcheck solution-c07f8590
  (fn [x y] (let [i (clojure.set/intersection x y) u (clojure.set/union x y)] (clojure.set/difference u i))))

(defcheck solution-c0b1c379
  (fn [a b]
    (into #{}
      (remove
        (into #{}
          (remove
            (into #{} (remove b a))
            a)) (into a b)))))

(defcheck solution-c0e4f369
  (partial reduce (fn [A b]
                    ((if (A b) disj conj) A b))))

(defcheck solution-c11df1f1
  (fn [s1 s2]
    (let [all (clojure.set/union s1 s2)
          same (clojure.set/intersection s1 s2)]
      (clojure.set/difference all same))))

(defcheck solution-c1274e1a
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-c12aff4f
  (fn [s1 s2]
    (set (keep (fn [i]
                 (when (and (or (not (s1 i)) (not (s2 i))
                                ) (not (and (s1 i) (s2 i))) ) i)
                 )  (concat s1 s2)))
    ))

(defcheck solution-c174fcd
  (fn [f s]
    (reduce
      (fn [a b]
        (if (get a b)
          (disj a b)
          (conj a b))) f s)))

(defcheck solution-c27baafd
  #(set `(~@(remove %2 %) ~@(remove % %2))))

(defcheck solution-c2a143b3
  (fn sd [s1 s2]
    (let [u (filter s1 s2)]
      (into (apply disj s1 u) (apply disj s2 u)))))

(defcheck solution-c2cf8f6b
  (fn [s1 s2]
    (set (for [x (into s1 s2)
               :when (not (and
                           (contains? s1 x)
                           (contains? s2 x)))]
           x)
      )))

(defcheck solution-c36e5029
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-c42c4891
  (fn [a b] (clojure.set/difference (into a b) (clojure.set/intersection a b))))

(defcheck solution-c4af067
  (fn [s1 s2]
    (let [diff #(filter (complement %1) %2)]
      (set (concat (diff s1 s2) (diff s2 s1))))))

(defcheck solution-c50faeac
  (fn sym-diff [s1 s2]
    (let [wanted? (fn [e] (if (contains? s1 e)
                            (not (contains? s2 e))
                            (contains? s2 e))) ]

      (into #{} (filter wanted? (into s1 s2))))))

(defcheck solution-c51edb49
  #(set (concat (apply disj % %2) (apply disj %2 %))))

(defcheck solution-c5e25165
  (fn [a b]
    (let [u (clojure.set/union a b)
          i (clojure.set/intersection a b)]
      (clojure.set/difference u i))))

(defcheck solution-c63ee4f8
  #(set (concat (filter (complement %1) %2) (filter (complement %2) %1))))

(defcheck solution-c6aeaab9
  (fn [A B] (clojure.set/union (clojure.set/difference A B) (clojure.set/difference B A))))

(defcheck solution-c6f18d9c
  #(into (clojure.set/difference % %2) (clojure.set/difference %2 %)))

(defcheck solution-c7131468
  (fn [s1 s2] (set (filter #(not= (contains? s1 %) (contains? s2 %))(concat s1 s2)))))

(defcheck solution-c7145abb
  (fn sym-diff
    [s1 s2]
    (set
      (filter
        #(not (contains? (set (for [x s1 y s2 :when (= x y)] x)) %))
        (concat s1 s2)))))

(defcheck solution-c7561aa0
  (fn symmetric-difference
    [a b]
    (let [union (into a b)]
      (into (apply disj union a) (apply disj union b)))))

(defcheck solution-c7cccac8
  (fn [a b]
    (set (concat (filter #(not (a %)) b) (filter #(not (b %)) a)))))

(defcheck solution-c7d2cf52
  (fn [a,b] (set
              (filter
                (fn [e]
                  (not (and (some #(= % e) a) (some #(= % e) b))))
                (concat a b)))))

(defcheck solution-c7fb3e50
  (fn [a b]
    (let [xor (fn [x y] (or (and x (not y)) (and y (not x))))]
      (set (filter #(xor (a %) (b %)) (into a b)))
      )
    ))

(defcheck solution-c812ad93
  #(into #{} (remove (into #{} (map % %2)) (into % %2))))

(defcheck solution-c85e8a03
  (fn sym-diff [set1 set2]
    (reduce (fn [x [y z]] (if (= 1 (count z))
                            (conj x (first z))
                            x))
      #{}
      (group-by identity (concat set1 set2)))))

(defcheck solution-c884c60d
  (fn[set1 set2]
    (reduce conj
      (loop [v1 (seq set1) ans #{}]
        (if (empty? v1)
          ans
          (recur (next v1)
            (if (not (contains? set2 (first v1)))
              (conj ans (first v1))
              ans))))
      (loop [v2 (seq set2) ans #{}]
        (if (empty? v2)
          ans
          (recur (next v2)
            (if (not (contains? set1 (first v2)))
              (conj ans (first v2))
              ans))))
      )))

(defcheck solution-c88f669b
  (fn [p q] (into #{} (map #(first %) (filter #(= (count %) 1)
                                        (map #(val %)
                                          (group-by identity
                                            (concat (seq p) (seq q)))))))))

(defcheck solution-c8c158
  (fn [a b]
    (set (concat (filter #((complement contains?) b %) a)
                 (filter #((complement contains?) a %) b)))))

(defcheck solution-c9305724
  reduce #(if (get %1 %2) (disj %1 %2) (conj %1 %2)))

(defcheck solution-c956c429
  (fn [a b]
    (let [uniqa (clojure.set/difference a b)
          uniqb (clojure.set/difference b a)]
      (clojure.set/union uniqa uniqb))))

(defcheck solution-ca495e77
  (fn [set1 set2]
    (loop [result #{} elements (clojure.set/union set1 set2)]
      (if (empty? elements)
        result
        (if (or (and (contains? set1 (first elements)) (not (contains? set2 (first elements))))
                (and (contains? set2 (first elements)) (not (contains? set1 (first elements)))))
          (recur (conj result (first elements)) (rest elements))
          (recur result (rest elements))
          )
        )
      )
    ))

(defcheck solution-ca50848d
  (fn sd [xs ys]
    (into #{}
      (concat
       (filter (complement (partial contains? ys)) xs)
       (filter (complement (partial contains? xs)) ys)))))

(defcheck solution-ca5dff54
  (fn dif [x y]
    (set (concat
          (filter #(not (x %)) y)
          (filter #(not (y %)) x)
          ))
    ))

(defcheck solution-cb2b8661
  #(clojure.set/difference (clojure.set/union        %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-cbb0f9af
  (fn sym-diff [s1 s2]
    (set (filter #(not (and (contains? s1 %) (contains? s2 %))) (clojure.set/union s1 s2)
           ))))

(defcheck solution-ccdfee0d
  #(set (map first ((group-by (fn[[a b]](count b))(group-by (fn[a]a) `(~@% ~@%2))) 1))))

(defcheck solution-cd603887
  (fn [a b] (clojure.set/difference (clojure.set/union a b)
              (clojure.set/intersection a b))))

(defcheck solution-ce1c886c
  (fn [s1 s2]
    (let [union (into s1 s2)
          intersection (set (filter s1 s2))
          sym-diff (set (remove intersection union))]
      sym-diff)))

(defcheck solution-ce27b8c
  (fn [xs ys]
    (clojure.set/difference (clojure.set/union xs ys)
      (clojure.set/intersection xs ys))))

(defcheck solution-ce5f0d2a
  #(into (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-ce9c13ce
  (fn [s1 s2]
    (apply disj (clojure.set/union  s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-cecfac07
  #(set (mapcat identity [(apply disj %1 %2) (apply disj %2 %1)])))

(defcheck solution-cf05b021
  (fn [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-cf647e66
  (fn [x y]
    (set (mapcat #(remove (clojure.set/intersection x y) %) [x y]))))

(defcheck solution-cf7f0117
  (let [d clojure.set/difference] #(clojure.set/union (d %1 %2) (d %2 %1))))

(defcheck solution-cfad80df
  (fn [z w] (letfn [(union [x y]
                      (cond
                        (= x '()) y
                        (= y '()) x
                        :else (apply conj x y)))
                    (xnoty [x y] (filter #(not (contains? y %)) x))]
              (set (union (xnoty z w) (xnoty w z))))))

(defcheck solution-cfb483ef
  (fn [l1 l2]
    (reduce #(if ((clojure.set/intersection l1 l2) %2) %1
                                                       (conj %1 %2))
      #{}
      (clojure.set/union l1 l2))))

(defcheck solution-cfe1ed0f
  #(let [inters (clojure.set/intersection %1 %2)
         unio (clojure.set/union %1 %2)
         symdiff (clojure.set/difference unio inters)] symdiff))

(defcheck solution-d06bc1a6
  (fn sym-diff [a b]
    (set (concat (remove a b) (remove b a)))))

(defcheck solution-d0c0d83
  (fn [x y] (set (remove nil? (map #(let [[k v] %] (when (= v 1) k)) (frequencies (concat x y )))))))

(defcheck solution-d0c9b964
  (fn [a b]
    (set (filter #(and (or (a %) (b %))
                       (not (and (a %) (b %))))
           (clojure.set/union a b)))))

(defcheck solution-d159fb00
  (fn [a b]
    (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b))))

(defcheck solution-d169085
  (fn D [s t]
    (letfn [(X [s t s-t]
              (cond (empty? s) s-t
                    (contains? t (first s)) (recur (rest s) t s-t)
                    :else (recur (rest s) t (conj s-t (first s)))))]
      (X t s (X s t #{})))))

(defcheck solution-d1d70dba
  (fn [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-d2607173
  (fn [s1 s2] (set (concat
                    (reduce disj s1 s2)
                    (reduce disj s2 s1)))))

(defcheck solution-d2bcb5f1
  (fn [a b] (set (into (filter #(not (b %)) a)
                   (filter #(not (a %)) b)))))

(defcheck solution-d2e1b181
  (fn [s1 s2]
    (into #{}
      (concat
       (for [x s1 :when (false? (contains? s2 x))] x)
       (for [x s2 :when (false? (contains? s1 x))] x)))))

(defcheck solution-d361a45
  #(let [u (clojure.set/union %1 %2) i (clojure.set/intersection %1 %2)] (clojure.set/difference u i)))

(defcheck solution-d36c56f2
  #(apply clojure.set/difference (apply (juxt clojure.set/union clojure.set/intersection) %&)))

(defcheck solution-d3dd8d7e
  #(clojure.set/union (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-d44fb50b
  #(let [u clojure.set/union d clojure.set/difference] (u (d %1 %2) (d %2 %1))))

(defcheck solution-d4a344cc
  (fn [a b]
    (set (concat ( filter #(= nil (b %)   ) a ) ( filter #(= nil (a %)   ) b )  ))
    ))

(defcheck solution-d4fdbeec
  (fn [a b]
    (let [- (fn [y x] (filter #(not (x %)) y))
          part1 (- a b)
          part2 (- b a)]
      (set (into part1 part2)))))

(defcheck solution-d559a192
  #(set (concat (filter (fn [i] (not (%2 i))) %) (filter (fn [i] (not (% i))) %2))))

(defcheck solution-d5ba4400
  #(into (set (remove %2 %)) (remove % %2)))

(defcheck solution-d5d0160f
  (fn [x y]
    (set
      (filter
        (fn [val]
          (and (or (contains? x val)
                   (contains? y val))
               (not (and (contains? x val)
                         (contains? y val)))))
        (clojure.set/union x y)))))

(defcheck solution-d5f42d7b
  (comp (partial apply (partial apply disj)) (juxt into clojure.set/intersection)))

(defcheck solution-d66df554
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-d6e52599
  (fn [a b]
    (set (filter #(not= (a %1) (b %1))
           (set (concat a b))))))

(defcheck solution-d8731402
  (fn [set1 set2]
    (clojure.set/difference
      (clojure.set/union set1 set2)
      (clojure.set/intersection set1 set2))))

(defcheck solution-d8de8f2e
  #(set (concat (remove %2 %) (remove % %2) ) ))

(defcheck solution-d8fe4f2d
  (fn [a b]
    (->> (into a b)
      (map (juxt a b))
      (map #(remove nil? %))
      (filter #(= 1 (count %)))
      (mapcat identity)
      (set))))

(defcheck solution-d92958d4
  #(into #{} (remove (into #{} (filter % %2)) (concat % %2))))

(defcheck solution-d971a9e
  (fn [s1 s2]
    (loop [tmps1 s1 ans #{}]
      (if (empty? tmps1)
        (loop [tmps2 s2 newans ans]
          (if (empty? tmps2)
            newans
            (if (contains? s1 (first tmps2))
              (recur (rest tmps2) newans)
              (recur (rest tmps2) (conj newans (first tmps2))))))
        (if (contains? s2 (first tmps1))
          (recur (rest tmps1) ans)
          (recur (rest tmps1) (conj ans (first tmps1))))))))

(defcheck solution-d9e94f00
  (fn [s1 s2]
    (reduce conj
      (apply disj s1 s2)
      (apply disj s2 s1))))

(defcheck solution-d9e9e8d2
  (fn sym-diff [s1 s2] (apply hash-set (concat (filter #(not (s2 %)) s1) (filter #(not (s1 %)) s2)))))

(defcheck solution-da248af2
  (fn set-difference [ls rs]
    (let [ls2 (remove ls rs)
          rs2 (remove rs ls)]
      (set (into ls2 rs2)))))

(defcheck solution-da3fe705
  (fn [s1 s2]
    (let [sub (fn [s1 s2] (reduce (fn [s x] (disj s x)) s1 s2))]
      (let [sub1 (sub s1 s2)
            sub2 (sub s2 s1)]
        (reduce (fn [s x] (conj s x)) sub1 sub2)))))

(defcheck solution-da747b5a
  (fn [a b] (clojure.set/union
              (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-dab5204b
  (fn [one two]
    (into #{}
      (concat
       (for [elem one :when (not (two elem))]
         elem)
       (for [elem two :when (not (one elem))]
         elem)))))

(defcheck solution-dafc6f4f
  #(set (concat (filter (complement %) %2) (filter (complement %2) %))))

(defcheck solution-db8ee686
  (fn [a b]
    (let [f #(apply (partial disj %) %2)]
      (into (f a b) (f b a)))))

(defcheck solution-dbba3739
  #(set (concat (filter (comp not (partial contains? %1)) %2) (filter (comp not (partial contains? %2)) %1))))

(defcheck solution-dbbd28a5
  #(set (concat (filter (fn [item] (not (% item))) %2)
                (filter (fn [item] (not (%2 item))) %))))

(defcheck solution-dbf1f080
  #(let [d clojure.set/difference] (clojure.set/union (d % %2) (d %2 %))))

(defcheck solution-dc6c6fd2
  (fn symdiff [x y]
    (let [diff clojure.set/difference]
      (into (diff x y) (diff y x)))))

(defcheck solution-dc95affd
  (fn [a b]
    (set (remove #(b (a %)) (into a b)))))

(defcheck solution-dd31304f
  (fn [x y]
    ( ->> (concat x y)
      (group-by identity)
      vals
      (filter #(= (count %) 1))
      (map #(first %))
      (set))))

(defcheck solution-de185f55
  (fn [col1 col2]
    (set (concat
          (filter #(not (col1 %)) col2)
          (filter #(not (col2 %)) col1)))))

(defcheck solution-de22fa55
  (fn [a b](clojure.set/difference (into a b) (clojure.set/intersection a b))))

(defcheck solution-de9ba5cb
  (letfn [
          (symd [s1 s2]
            (let [intrsec (clojure.set/intersection s1 s2)]
              (clojure.set/union
                (clojure.set/difference s1 intrsec)
                (clojure.set/difference s2 intrsec)
                )))]
    symd
    ))

(defcheck solution-debc6337
  (fn [s1 s2] (reduce (fn [r e] (if-not (contains? s2 e) (conj r e) (disj r e))) s2 s1)))

(defcheck solution-df65dc65
  (fn [a b]
    (set (concat (filter #(not (b %)) a) (filter #(not (a %)) b)))))

(defcheck solution-df8e2557
  #(let [i (clojure.set/intersection % %2)]
     (clojure.set/union (clojure.set/difference % i) (clojure.set/difference %2 i))))

(defcheck solution-dfc7ca5c
  #(clojure.set/difference (apply clojure.set/union %&) (apply clojure.set/intersection %&)))

(defcheck solution-e05d1df5
  (letfn [(s-dif [s-into pred sx]
            (reduce
              (fn [acc b]
                (if (pred b)
                  acc
                  (conj acc b)))
              s-into
              sx))]
    (fn [sa sb]
      (s-dif (s-dif #{} sa sb) sb sa))))

(defcheck solution-e0fa615e
  #(into (into #{} (remove % %2)) (remove %2 %)))

(defcheck solution-e1323171
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)
     ))

(defcheck solution-e138fef9
  (fn [s1 s2] (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-e1edc931
  #(clojure.set/union
     (set (for [s1 %1
                :when (not (contains? %2 s1))]
            s1))
     (set (for [s1 %2
                :when (not (contains? %1 s1))]
            s1))))

(defcheck solution-e1fecc1a
  (fn symmetricDiff [setA setB]
    (clojure.set/union (clojure.set/difference setA setB) (clojure.set/difference setB setA))
    ))

(defcheck solution-e203c0bd
  (fn [s1 s2] (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))))

(defcheck solution-e2087cbf
  (fn [s1 s2](letfn [(f [s1 s2] (for [e s1 :when (not (contains? s2 e))] e))] (apply hash-set (concat (f s1 s2) (f s2 s1) )))))

(defcheck solution-e2285e4c
  (fn [s1 s2]
    (apply (partial disj (set (concat s1 s2))) (for [x s1 y s2 :when (= x y)] x))))

(defcheck solution-e2665a8a
  (fn [a b]
    (letfn [(diff [x y]
              (remove (fn [e] (get y e)) x))]
      (let [a-diff (set (diff a b))
            b-diff (set (diff b a))]
        (if (empty? a-diff) b-diff
                            (if (empty? b-diff) a-diff
                                                (apply conj a-diff b-diff)))))))

(defcheck solution-e2c584bf
  (fn [a b] (set (filter #(not= (contains? a %) (contains? b %)) (into a b)))))

(defcheck solution-e2db9ded
  (fn sym-diff [x y]
    (clojure.set/difference (clojure.set/union x y) (clojure.set/intersection x y))))

(defcheck solution-e3721697
  (fn [x y]
    (let [j (reduce (fn [a b] (if (x b) a (conj a b))) #{} y)]
      (reduce (fn [a b] (if (y b) a (conj a b))) j x))))

(defcheck solution-e3931c0d
  (fn [s1 s2]
    (set (concat (reduce disj s1 s2) (reduce disj s2 s1)))))

(defcheck solution-e418a86d
  (fn [a b]
    (into #{}
      (concat
       (filter #(not (contains? b %)) a)
       (filter #(not (contains? a %)) b)))))

(defcheck solution-e450c0ef
  (fn [a b] (into (set
                    (filter (comp not a) b))
              (filter (comp not b) a))))

(defcheck solution-e4b22928
  #(set (concat (remove %1 %2) (remove %2 %1))))

(defcheck solution-e4cd3293
  (fn [s1 s2] (set
                (concat
                 (remove #(contains? s1 %) s2)
                 (remove #(contains? s2 %) s1)))))

(defcheck solution-e53604f0
  #(clojure.set/union
     (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-e54a0ce
  (fn
    [a b]
    (let [ld (fn [x y] (filter #(not(contains? y %)) x))]
      (set (concat (ld a b) (ld b a))))))

(defcheck solution-e57c1fe
  #(clojure.set/union (clojure.set/difference % %2)
     (clojure.set/difference %2 %)))

(defcheck solution-e59ac7df
  #(into (clojure.set/difference %2 %1) (clojure.set/difference %1 %2)))

(defcheck solution-e5c48b5f
  (fn [s1 s2]
    (into #{}
      (clojure.set/union
        (filter (partial (comp not contains?) s1) s2)
        (filter (partial (comp not contains?) s2) s1)))))

(defcheck solution-e5d171bd
  (fn diff-set
    [s1 s2]
    (clojure.set/union  (clojure.set/select #(nil? (s1 %)) s2)
      (clojure.set/select #(nil? (s2 %)) s1))))

(defcheck solution-e6098631
  (fn sym-diff
    [seta setb]
    (let [diff clojure.set/difference
          union clojure.set/union]
      (union (diff seta setb) (diff setb seta)))))

(defcheck solution-e62f7c3d
  (fn sim-dif [a b]
    (set (concat
          (filter #(nil? (a %)) b)
          (filter #(nil? (b %)) a)))))

(defcheck solution-e64e2743
  (fn [as bs]
    (clojure.set/union (clojure.set/difference as bs)
      (clojure.set/difference bs as))))

(defcheck solution-e6942622
  (fn [a b]
    (let [da (clojure.set/difference a b)
          db (clojure.set/difference b a)
          union (clojure.set/union da db)]
      union)))

(defcheck solution-e75d7174
  (fn sym-diff [s1 s2]
    (set (mapcat remove
           [s1 s2]
           [s2 s1]))))

(defcheck solution-e7e8635c
  (fn [a b] (set (filter #(not (and (a %) (b %))) (concat a b)))))

(defcheck solution-e8085951
  (fn my-difference [x y]
    (set (remove (set (filter x y)) (into x y)))))

(defcheck solution-e80fa755
  (fn [a b] (into #{} (concat (remove #(a %) b) (remove #(b %) a)))))

(defcheck solution-e82c3df
  (fn [a b]
    (loop [a_ a ret b]
      (cond
        (empty? a_) ret
        (ret (first a_))
        (recur (next a_) (disj ret (first a_)))
        :else
        (recur (next a_) (conj ret (first a_)))))))

(defcheck solution-e8b3b529
  #(clojure.set/difference
     (clojure.set/union % %2)
     (clojure.set/intersection % %2)))

(defcheck solution-e8fbb90f
  (fn [a b]
    (let [un (set (concat a b))]
      (set (remove #(and (a %) (b %)) un)))))

(defcheck solution-e9103ce1
  (fn [s1 s2]
    (->> (into (into []  s1 ) s2)
      (frequencies)
      (filter #(= 1 (second %)))
      (map #(first %))
      (into #{})
      )

    ))

(defcheck solution-e94c030c
  (fn [x y]
    (clojure.set/union
      (clojure.set/difference x y)
      (clojure.set/difference y x))))

(defcheck solution-e9cf6af9
  (fn sd [xs ys]
    (clojure.set/difference
      (clojure.set/union xs ys)
      (clojure.set/intersection xs ys))))

(defcheck solution-e9e38feb
  (fn
    [s1 s2]

    (set
      (concat
       (filter
         (fn [v]
           (not
             (contains? s2 v)
             )
           )
         s1
         )
       (filter
         (fn [v]
           (not
             (contains? s1 v)
             )
           )
         s2
         )
       )
      )
    ))

(defcheck solution-e9fe2a
  (fn [seta setb]
    (clojure.set/difference (clojure.set/union  seta  setb) (clojure.set/intersection seta setb))))

(defcheck solution-ea0a172a
  #(clojure.set/difference
     (clojure.set/union %1 %2)
     (clojure.set/intersection %1 %2)))

(defcheck solution-ea1b801c
  #(clojure.set/difference (clojure.set/union % %2) (clojure.set/intersection % %2)))

(defcheck solution-ead6ddcb
  (fn [s1 s2] (into (into #{}
                      (filter #(not (contains? s2 %)) s1))
                (filter #(not (contains? s1 %)) s2)
                )))

(defcheck solution-eaef18c4
  #(clojure.set/union
     (clojure.set/difference %1 %2)
     (clojure.set/difference %2 %1)))

(defcheck solution-eb44dbaa
  (fn symmetric-difference [s1 s2]
    (set ((partial filter #(not (and (contains? s1 %) (contains? s2 %))))
          (into s1 s2)))))

(defcheck solution-eba00e51
  (fn [s1 s2] (set (filter #(not= (s1 %) (s2 %)) (concat s1 s2)))))

(defcheck solution-ecd2feed
  (fn [c1 c2] (set (apply concat (for [m2 [[c1 c2] [c2 c1]]]
                                   (filter (fn [p] (not (some #(= % p) (second m2)))) (first m2))
                                   )))))

(defcheck solution-ed4301d3
  (fn symdiff [as bs]
    (clojure.set/union
      (clojure.set/difference as bs)
      (clojure.set/difference bs as))))

(defcheck solution-ed893d08
  (fn sd [s1 s2] (set (concat (filter #(not (contains? s2 %)) s1) (filter #(not (contains? s1 %)) s2)))))

(defcheck solution-ed9c756f
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-edde17db
  (fn [a b]
    (let [union (reduce conj a b)]
      (reduce (fn [s e]
                (if (and (contains? b e) (contains? a e))
                  s
                  (conj s e)))
        #{}
        union)
      )))

(defcheck solution-ee15810f
  (fn [a b]
    (->>
      (clojure.set/union a b)
      (filter #(not= (a %) (b %)))
      set)))

(defcheck solution-ee30b56b
  (fn
    [x y]
    (clojure.set/union
      (clojure.set/difference x y)
      (clojure.set/difference y x))))

(defcheck solution-ee8ef7f9
  (fn [s1 s2]
    (into #{} (filter #(not (and (some #{%} s1) (some #{%} s2))) (concat s1 s2)))))

(defcheck solution-ef019c62
  (fn symm-diff [set1 set2]
    (reduce
      #(if (or
            (and
             (contains? set1 %2)
             (not (contains? set2 %2)))
            (and
             (contains? set2 %2)
             (not (contains? set1 %2)))
            ) (conj %1 %2) %1)
      #{} (set (concat set1 set2)))))

(defcheck solution-ef7850fa
  (fn [s1 s2]
    (apply disj (reduce conj s1 s2) (filter #(contains? s2 %) s1))))

(defcheck solution-f03c54f
  #(into #{} (concat
              (filter (complement %1) %2)
              (filter (complement %2) %1))))

(defcheck solution-f064518c
  (fn mySymmetricDifference2
    [s1 s2]
    (clojure.set/difference (clojure.set/union s1 s2) (clojure.set/intersection s1 s2))))

(defcheck solution-f0910d0b
  #(reduce (fn [diff new]
             (if (and
                  (or (contains? % new) (contains? %2 new))
                  (not (and (contains? % new) (contains? %2 new))))
               (conj diff new)
               diff))
     #{}
     (concat % %2)))

(defcheck solution-f187f177
  (fn [xs ys]
    (set
      (concat (filter #(not (xs %)) ys)
              (filter #(not (ys %)) xs)))))

(defcheck solution-f1aaa297
  #(set (concat
         (filter (complement %1) %2)
         (filter (complement %2) %1))))

(defcheck solution-f1c90e
  (comp set (partial map first) (partial filter #(= (val %) 1)) (partial reduce #(assoc % %2 (inc (get % %2 0))) {}) concat))

(defcheck solution-f1e7ba5a
  (fn [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/intersection a b))))

(defcheck solution-f340e013
  (fn [xs ys]
    (set (mapcat identity
           (filter #(= 1 (count %)) (vals (group-by identity (concat xs ys))))))))

(defcheck solution-f3b3abc4
  (fn not-in-both
    [a b]
    (set (filter #(not (and (a %) (b %))) (into a b)))))

(defcheck solution-f4659967
  (fn [s1 s2] (set (clojure.set/union (filter (complement (partial contains? s1)) s2)
                     (filter (complement (partial contains? s2)) s1)
                     ))))

(defcheck solution-f482ba8b
  (fn [a b]
    (clojure.set/union
      (clojure.set/difference a b)
      (clojure.set/difference b a)
      )
    ))

(defcheck solution-f58ae1a1
  (fn [col1 col2]
    (let [coll (concat col1 col2)]
      (->>
        coll
        (filter #(if-not ((set (map col1 col2)) %1) true false))
        set))))

(defcheck solution-f59cb461
  (fn [s1 s2]
    (let [ix (clojure.set/intersection s1 s2)
          d1 (clojure.set/difference s1 ix)
          d2 (clojure.set/difference s2 ix)]
      (clojure.set/union d1 d2))))

(defcheck solution-f6288efa
  (fn [s1 s2]
    (clojure.set/difference
      (clojure.set/union s1 s2)
      (clojure.set/intersection s1 s2))))

(defcheck solution-f6df92ab
  (fn [x y]
    (clojure.set/union
      (clojure.set/difference x y)
      (clojure.set/difference y x)
      )
    ))

(defcheck solution-f72bfd1a
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b)
      (clojure.set/difference b a))))

(defcheck solution-f73c4202
  (fn [set1 set2]
    (let [union (clojure.set/union set1 set2)
          intersection (clojure.set/intersection set1 set2)]
      (clojure.set/difference union intersection))))

(defcheck solution-f8152ced
  (fn xdiff [set1 set2]
    (into #{}
      (filter #(let [in1 (set1 %) in2 (set2 %)] (and (or in1 in2) (not (and in1 in2))))
        (into set1 set2)))))

(defcheck solution-f89448fe
  #(into #{} (concat (clojure.set/difference %1 %2)
                     (clojure.set/difference %2 %1))))

(defcheck solution-f8df275a
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b)
      (clojure.set/difference b a))
    ))

(defcheck solution-f917d3b0
  (fn [a b]
    (apply sorted-set (reduce disj (reduce conj a b) (set (clojure.set/intersection a b))))))

(defcheck solution-f9994c48
  #(let [s1 (clojure.set/difference % %2)
         s2 (clojure.set/difference %2 %)]
     (clojure.set/union s1 s2)))

(defcheck solution-f9e07139
  #(apply disj
     (into % %2)
     (keep % %2)))

(defcheck solution-fa217377
  (fn [m1 m2]
    (set (filter
           (fn [n]
             (not= (not-any? #(= n %) m1) (not-any? #(= n %) m2)))
           (clojure.set/union m1 m2)))))

(defcheck solution-fa306db2
  (fn [a b]
    (clojure.set/difference
      (clojure.set/union a b)
      (clojure.set/intersection a b))))

(defcheck solution-fa53e23d
  (fn [s1 s2]
    (let [s (set (concat s1 s2))]
      (set (filter #(false? (and (contains? s1 %) (contains? s2 %))) s)))))

(defcheck solution-fa6a79a2
  (fn [xs ys]
    (set (remove (clojure.set/intersection xs ys)
           (clojure.set/union xs ys)))))

(defcheck solution-fa9aa2dc
  (fn [s1 s2]
    (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))
    ))

(defcheck solution-faa528e3
  (fn [a b]
    (let [u (into a b)
          i (filter a b)]
      (reduce disj u i))))

(defcheck solution-fab8cbd7
  (fn sd [s1 s2]
    (clojure.set/union
      (clojure.set/difference s1 s2)
      (clojure.set/difference s2 s1))))

(defcheck solution-facbf3ff
  #(let [a %1 b %2 c (clojure.set/intersection a b)]
     (set (concat (remove c a) (remove c b)))))

(defcheck solution-fb3eb6a9
  (fn [a b]
    (let [i (clojure.set/intersection a b)]
      (clojure.set/union (clojure.set/difference a i)
        (clojure.set/difference b i)))))

(defcheck solution-fb73ecb1
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-fb99a1d0
  (fn [as bs]
    (clojure.set/difference (clojure.set/union as bs) (clojure.set/intersection as bs))))

(defcheck solution-fd416f5b
  (fn sdiff [set1 set2]
    (let [all-keys (set (concat set1 set2))]
      (set (filter (fn [k]
                     (or (and (not (contains? set1 k))
                              (contains? set2 k))
                         (and (contains? set1 k)
                              (not (contains? set2 k))))) all-keys)))))

(defcheck solution-fd47475c
  #(clojure.set/difference
     (clojure.set/union %1 %2) (clojure.set/intersection %1 %2)))

(defcheck solution-fd777a3e
  (fn sym-diff [s1 s2]
    (set (into (remove s1 s2) (remove s2 s1)))))

(defcheck solution-fdb9685a
  (fn [& x]
    (apply disj
      (apply clojure.set/union x)
      (apply clojure.set/intersection x))))

(defcheck solution-fe20b8f7
  (fn [a b]
    (let [union (into a b) intersection (filter a b)]
      (apply disj union intersection))))

(defcheck solution-fe4e194
  (fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a))))

(defcheck solution-fe708b6a
  (fn [x y]
    (into (set (filter #(not (x %)) y)) (filter #(not (y %)) x))))

(defcheck solution-fed9384b
  (fn [a b] (set (keys (filter #(-> % val (= 1)) (merge-with + (frequencies a) (frequencies b)))))))

(defcheck solution-fee3ad7e
  (fn[set1 set2] (clojure.set/union (clojure.set/difference set1 set2) (clojure.set/difference set2 set1))))

(defcheck solution-ff46cacd
  #(let [s1 %1 s2 %2 diff clojure.set/difference union clojure.set/union] (union (diff s1 s2) (diff s2 s1))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-88))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
