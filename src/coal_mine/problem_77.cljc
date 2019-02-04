(ns coal-mine.problem-77
  (:require [coal-mine.checks :refer [defcheck-77] :rename {defcheck-77 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-105f1f51
  (fn anagrams [words]
    (->> words
      (reduce #(merge-with concat %1
                 {(sort %2) [%2]}) {})
      (filter #(< 1 (count (val %))))
      (map #(apply hash-set (val %)))
      (apply hash-set))))

(defcheck solution-105f3f49
  (fn [col]
    (set (map #(set (second %) )(filter  #(> (count (second %)) 1) (group-by #(sort (seq %)) col))))))

(defcheck solution-1096648b
  (fn prob-0077
    [words]

    (let [freq-dec (fn freq-dec
                     [sm k]
                     {:pre [(not (nil? (sm k)))]}

                     (if (nil? (sm k))
                       (throw (ex-info "(not (nil? (mp k)))" {})))

                     (let [cnt (dec (sm k 0))]
                       (if-not (pos? cnt)
                         (dissoc sm k)
                         (merge sm {k cnt}))))

          freq-cnt (fn freq-cnt
                     [sm]
                     (apply + (vals sm)))

          perm-sorted-map (fn perm-sorted-map
                            [sm]
                            (if (<= (freq-cnt sm) 1)
                              (list (keys sm))
                              (for [k (keys sm)
                                    rest (perm-sorted-map (freq-dec sm k))]
                                (cons k rest))))

          permutations (fn permutations
                         [xs]
                         (let [perms (perm-sorted-map (into (sorted-map) (frequencies (seq xs))))]
                           (apply sorted-set (map #(apply str %) perms))))
          ]

      (let [srt-wrds    (apply sorted-set words)
            to-uq-perms #(clojure.set/intersection (permutations %) srt-wrds)
            to-uq-mults #(>= (count %) 2) ]

        (set (distinct (filter to-uq-mults (map to-uq-perms srt-wrds))))))))

(defcheck solution-10c90684
  (comp
   set
   (partial map set)
   (partial filter #(< 1 (count %)))
   vals
   (partial group-by sort)))

(defcheck solution-10edfe9
  (fn anagrams [words]
    (let [word-groups (vals (group-by #(sort %) words))
          match-groups (filter #(> (count %) 1) word-groups)
          match-sets (map #(set %) match-groups)]
      (set match-sets))))

(defcheck solution-115504cb
  (fn [coll] (set (map set (filter #(> (count %) 1) (vals (#(group-by sort %) coll)))))))

(defcheck solution-1183707c
  (fn [coll] (->> coll
               (group-by sort)
               (vals)
               (filter #(> (count %) 1))
               (map set)
               (set))))

(defcheck solution-119d279
  (fn [v]
    (let [get-ordered-chars #(sort (seq %))]
      (into #{} (filter #(> (count %) 1)
                  (vals (reduce
                          #(let [s-key (get-ordered-chars %2)]
                             (if (contains? %1 s-key)
                               (assoc %1 s-key (conj (get %1 s-key) %2))
                               (assoc %1 s-key #{%2})))
                          {} v)))))))

(defcheck solution-11cf9965
  (fn [s] (set (map set
                 (filter #(> (count %) 1)
                   (vals (group-by #(set %) s) ) )))))

(defcheck solution-12101939
  (fn [coll]
    (set (map #(set (val %)) (filter #(> (count (val %)) 1) (group-by sort coll))))))

(defcheck solution-126ef285
  (fn [sq]
    (reduce
      #(if (> (count (second %2)) 1) (conj %1 (second %2)) %1)
      #{}
      (reduce #(let [k (->> %2 seq sort (apply str))]
                 (assoc %1 k (conj (get %1 k #{}) %2))) {} sq))))

(defcheck solution-12790f16
  (fn [x] (set (filter #(> (count %1) 1) (map #(set %1) (vals (group-by #( sort % ) x)))))))

(defcheck solution-12c59969
  (fn [words]
    (letfn [(permute [arr]
              (let [conc (fn [x y] (vec (concat x (if (vector? y) y (vector y)))))
                    except-idx (fn [idx coll] (vec (concat (take idx coll) (nthrest coll (inc idx)))))]
                (reduce
                  (fn [a b] (conc (vec a) (vec b)))
                  (map-indexed
                    (fn [i v]
                      (let [prefix (vector v)
                            remainder (except-idx i arr)]
                        (map
                          (partial conc prefix)
                          (if (> (count remainder) 1)
                            (permute remainder)
                            remainder))))
                    arr))))]
      (let [permutations (map permute words)]
        (->>
          (map (fn [permuted]
                 (->>
                   (map #(clojure.string/join "" %) permuted)
                   (filter (set words)))) permutations)
          (filter #(> (count %) 1))
          (map set)
          (set))))))

(defcheck solution-12cbb00
  (fn [[& ws]]
    (let [ws (filter (fn [[k l]] (> (count l) 1))
               (seq (group-by (fn [[k w]] k)
                      (map #(list (sort (seq %1)) %1) ws))))]
      (loop [[k l :as w] (first ws) ws (next ws) a #{}]
        (if w
          (recur (first ws) (next ws)
            (conj a (set (map #(second %) l))))
          a)))))

(defcheck solution-12d47157
  (fn findAnagrams [wordlist]
    (let
     [
      countLetters
      (fn [word]
        (reduce
          (fn [counter letter]
            (assoc
             counter
              letter
              (if
               (contains? counter letter)
                (inc (counter letter))
                1
                )
              )
            )
          {}
          word
          )
        )
      collectLetters
      (fn [words]
        (reduce
          (fn [collector word]
            (let [key (countLetters word)]
              (assoc
               collector
                key
                (if (contains? collector key)
                  (conj (collector key) word)
                  (set (vector word))
                  )
                )
              )
            )
          {}
          words
          )
        )
      ]
      (set (filter #(< 1 (count %)) (map set (vals (collectLetters wordlist)))))
      )
    ))

(defcheck solution-130bb136
  (fn [l] (reduce #(if (get-in %2 [1 1]) (conj % (set (get-in %2 [1]))) %) #{} (group-by sort l))))

(defcheck solution-1398aaa4
  #(let [tmp (partition-by sort (sort-by (comp (partial apply str) sort) %)),
         max-count (count (apply max-key count tmp))]
     (into #{}
       (map (partial into #{})
         (filter (fn [t] (= (count t) max-count))
           tmp)))))

(defcheck solution-13daa7c5
  #(set (for [[k v] (group-by sort %) :when (> (count v) 1)] (set v))))

(defcheck solution-1437cf59
  (fn [words] (set (filter #(> (count %) 1) (map set (vals (group-by sort words)))))))

(defcheck solution-146da9d1
  (fn anagrams [words]
    (->> words
      (group-by sort)
      vals
      (map set)
      (filter #(> (count %) 1))
      set)))

(defcheck solution-14af3b5f
  (fn anagran-finder [v]
    (set (filter #(< 1 (count %)) (map (fn [mapa](set (filter #(= mapa (frequencies %)) v)))
                                    (map frequencies v))))))

(defcheck solution-14c64df6
  #(set (filter (fn [x] (> (count x) 1))
          (for [[k v] (group-by frequencies %)]
            (set v)))))

(defcheck solution-150ae3b5
  (fn [words]
    (set (filter #(not (nil? (next %)))
           (vals (reduce  (fn [m word]
                            (update-in m [(set (seq word))] #(conj (into #{} %) word)))
                   {}
                   words))))))

(defcheck solution-158b20b8
  #(set (for [[_ v] (group-by sort %) :when (next v)] (set v))))

(defcheck solution-160dafb5
  (fn anagram [words]
    (set (map set (filter #(> (count %) 1) (vals (group-by #(frequencies %) words)))))))

(defcheck solution-1610af0d
  (fn [v] (->> v
            (group-by #(sort (seq %)) )
            (vals)
            (remove #(= 1 (count %)))
            (map set)
            (set))))

(defcheck solution-1620ce29
  (fn [words]
    (->> (group-by frequencies words)
      (map (comp set second))
      (filter #(> (count %) 1))
      set)))

(defcheck solution-16b9a8b5
  (fn  [coll]
    (let [c (map #(vector (sort (vec %)) %) coll)
          f (group-by #(sort (vec %)) coll)
          ]
      (set (->> coll
             (group-by #(sort (vec %)))
             (map #(set (second %)))
             (filter #(> (count %) 1)))))))

(defcheck solution-16ee76a5
  (fn anagrams [xs]
    (->> (vals (group-by #(sort %1) (set xs)))
      (filter #(> (count %1) 1))
      (map set)
      (into #{}))))

(defcheck solution-1711d886
  (fn [s]
    (set (filter
           #(> (count %) 1)
           (map #(set (nth % 1)) (group-by set s))))))

(defcheck solution-174b55de
  (fn anagrams [words]
    (->> words
      (group-by sort)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-17789f9a
  (fn [w] (set (filter #(> (count %) 1) (map (comp set second)
                                          (group-by sort w))))))

(defcheck solution-17b21f5d
  (fn [s] (set (filter #(> (count %) 1) (map set (vals (group-by #(sort (seq %)) s)))))))

(defcheck solution-17bb35d
  (fn [col]
    (let [is-anagram #(= (sort (vec %1)) (sort (vec %2)))
          f (fn [remaining m]
              (if (seq remaining)
                (let [first-item (first remaining)
                      mkey (sort (vec first-item))
                      mval (get m mkey)
                      to-merge (if (seq mval)
                                 {mkey (conj mval first-item)}
                                 {mkey #{first-item}})
                      new-map (merge m to-merge)
                      ]
                  (recur (rest remaining) new-map)
                  ) ; let
                (map #(second %)
                  (filter #(> (count (second %))
                             1)
                    m) ; filter
                  ) ; map
                ) ; if
              ) ; fn
          ]
      (set (f col {}))
      ) ; let
    ))

(defcheck solution-17d15776
  #(->> (group-by sort %)
     vals
     (filter (comp pos? dec count))
     (map set)
     set))

(defcheck solution-17d42d05
  (fn ana [w]
    (into #{} (filter #(> (count %) 1) (map #(set %) (vals (group-by sort w)))))))

(defcheck solution-18c495bf
  (fn sol [s]
    (set (map (fn [x] (set (map #(first %) x)))
           (filter #(> (count %) 1)
             (vals
               (group-by #(second %)
                 (map (fn [a] [a (set (frequencies a))]) s)
                 )
               )
             )
           ))
    ))

(defcheck solution-18c68ca8
  (fn [v]
    (->> v
      (group-by #(set (seq %)))
      vals
      (map set)
      (filter #(> (count %) 1))
      (into #{}))))

(defcheck solution-191a830
  (fn [words]
    (set
      (filter #(>= (count %) 2)
        (map (fn [[_ v]] v)
          (reduce
            #(let [k (set %2)]
               (assoc %1 k (conj (%1 k #{}) %2)))
            {}
            words))))))

(defcheck solution-193166ae
  (fn [ws]
    (->> ws
      (group-by sort) vals
      (filter #(< 1 (count %)))
      (map set) set)))

(defcheck solution-19afb941
  (fn anagramFind
    [s]
    (set (vals
           (filter
             (fn [elem] (> (count (val elem)) 1))
             (let []
               (reduce
                 (fn [m e] (if (contains? m (frequencies e))
                             (assoc m (frequencies e) (conj (get m (frequencies e)) e))
                             (assoc m (frequencies e) #{e})))
                 {}
                 s)))))))

(defcheck solution-19f1f220
  #(set (for [[_ g] (group-by set %)
              :when (next g)]
          (set g))))

(defcheck solution-1a1be94b
  (fn [c]
    (set (filter #(> (count %) 1) (vals
                                    ((fn f [[a & b] r]
                                       (if (= a nil) r
                                                     (let [k (sort a)]
                                                       (f b (assoc r k (conj (get r k #{}) a)))))) c {}))))))

(defcheck solution-1a7e7f11
  (fn [w] (->> w (group-by sort) vals
            (filter #(> (count %) 1))
            (map set) set)))

(defcheck solution-1a94dec0
  (fn anagram [coll]
    (into #{}
      (remove #(= 1 (count %1))
        (map #(into #{} %1) (vals (group-by #(sort (seq %1)) coll)))
        )
      )
    ))

(defcheck solution-1a95c069
  (fn [ss]
    (let [m (reduce (fn [acc s] (let [k (sort s)]
                                  (assoc acc k (conj (get acc k #{}) s))))
              {}
              ss)]
      (into #{} (filter #(< 1 (count %)) (vals m))))))

(defcheck solution-1aa9d2cf
  (fn [words]
    (set
      (filter
        #(< 1 (count %))
        (vals
          (loop [words words m {}]
            (let [w  (first words)
                  sw (sort w)
                  o  (get m sw #{})
                  n  (conj o w)]
              (if w
                (recur (next words) (assoc m sw n))
                m))))))))

(defcheck solution-1ab3d286
  ;(fn [s]
  ;  (reduce #(if (next %2) (conj % (set %2)) %)
  ;          #{}
  ;          (vals (group-by sort s))))
  (fn [s]
    (set (keep #(if (next %) (set %))
           (vals (group-by sort s))))))

(defcheck solution-1afaae6f
  (fn [v] (->> v
            (group-by sort)
            vals
            (filter #(> (count %) 1))
            (map set)
            set
            )))

(defcheck solution-1b7ab8ee
  (fn anagram-partition [words]
    (->> words
      (sort-by (fn [s] (apply str (sort (set s)))))
      (partition-by set)
      (filter (fn [s] (> (count s) 1)))
      (map set)
      (set))))

(defcheck solution-1b8b94b
  (fn anagram-finder [words]
    (letfn [(insert [x c pos]
              (str
                (subs x 0 pos)
                c
                (subs x pos)))
            (spray [x c]
              (map
                #(insert x c %)
                (range (inc (count x)))))
            (perm [l] (let [s (first l) r (rest l)]
                        (if (empty? r)
                          (doall (vector (str s)))
                          (distinct (flatten (map #(spray % s) (perm r)))))))]
      (let [anagrams (map #(set (perm %)) words)]
        (set (distinct
               (filter (fn [r] (> (count r) 1)) (map
                                                  (fn [a_set]
                                                    (set
                                                      (filter #(contains? a_set %) words)))
                                                  anagrams))))))))

(defcheck solution-1baf5ac
  (fn [xs]
    (->>
      xs
      (group-by #(into #{} (seq %)))
      (vals)
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-1c1b7d0e
  (fn [ss]
    (set
      (map
        set
        (filter
          #(> (count %) 1)
          (vals (group-by #(group-by identity %) ss))
          )
        )
      )
    ))

(defcheck solution-1c575c9a
  #(set
     (filter
       (fn[a](> (count a) 1))
       (for [x %]
         (set (filter (fn[a] (= (set a) (set x))) %))))))

(defcheck solution-1c655c3c
  (fn [xs] (set
             (map set (filter #(> (count %) 1) (vals (group-by set xs)))))))

(defcheck solution-1cb59698
  (fn [words]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort words)))))))

(defcheck solution-1cd70957
  (fn [ws]
    (->> ws
      (group-by frequencies)
      vals
      (map set)
      (filter #(>= (count %) 2))
      set)))

(defcheck solution-1d0bf81f
  (fn [xs] (->> (interleave (map #(-> % sort str) xs) xs)
             (partition 2)
             (reduce (fn [m e] (let [[k v] e]
                                 (update-in m [k]
                                   (fn [ov]
                                     (if (nil? ov) #{v} (conj ov v)))))) {})
             (filter #(-> % second count (> 1)))
             vals
             (into #{}))))

(defcheck solution-1d8b51d8
  (fn
    [vec]
    (let [freqs (frequencies (map clojure.string/join (map sort vec)))]
      (loop [sorted (map clojure.string/join (map sort vec)) indices {} iter 0]
        (if (empty? sorted)
          (loop [vals (vals indices) fin_set #{}]
            (if (empty? vals)
              fin_set
              (recur (rest vals) (conj fin_set ((fn
                                                  [origvec val]
                                                  (loop [val val subset #{}]
                                                    (if (empty? val)
                                                      subset
                                                      (recur (rest val) (conj subset (nth origvec (first val)))))
                                                    )
                                                  ) vec (first vals))))
              ))
          (if (> (freqs (first sorted)) 1)
            (if (contains? indices (first sorted))
              (recur (rest sorted) (assoc indices (first sorted) (conj (indices (first sorted)) iter)) (inc iter))
              (recur (rest sorted) (assoc indices (first sorted) (vector iter)) (inc iter))
              )
            (recur (rest sorted) indices (inc iter))
            )
          )
        ))

    ))

(defcheck solution-1dee4c91
  (fn
    [words]
    (into #{} (filter
                #(> (count %) 1)
                (map
                  (fn [[k vs]] (into #{} vs))
                  (group-by #(sort %) words))))))

(defcheck solution-1e4b686
  (fn [x]
    (set
      (filter #(> (count %) 1)
        (map #(set (second %))
          (group-by sort x))))))

(defcheck solution-1e9295d
  (fn [myvec]
    (set
      (filter #(> (count %) 1)
        (for [x (map val (group-by vals (map #(hash-map % (group-by identity %)) myvec)))]
          (set (flatten (map keys x))))))))

(defcheck solution-1e95544b
  ; this is so cool solution by daowen:
  ; #(->> % (group-by sort) vals (filter second) (map set) set)
  (fn [coll]
    (set (map (fn [[k v]] (set v))
           (filter (fn [[k v]] (> (count v) 1))
             ; could be replaced with: (group-by sort coll)
             (reduce #(let [k (sort %2)] (merge-with concat %1 {k [%2]})) {}  coll))))))

(defcheck solution-1eef6fb4
  (fn [w]
    (set (->> w
           (group-by frequencies)
           vals
           (map set)
           (filter #(> (count %) 1))))))

(defcheck solution-1f61d7ea
  (fn [v]
    (loop [acc #{} v (set v)]
      (if (empty? v)
        acc
        (let [a (first v)
              b (filter (fn [x] (= (set a) (set x))) v)
              acc (if (> (count b) 1) (conj acc (set b)) acc)]
          (recur acc (apply disj v b)))))))

(defcheck solution-1f838a02
  (fn [x] (set (for [item (vals (group-by sort x)) :when (< 1 (count item))] (set item)))))

(defcheck solution-1f9718b2
  (fn [s]
    (->> s
      (group-by #(sort (seq %)))
      (filter #(< 1 (count (second %))))
      (map #(set (second %)))
      (set))))

(defcheck solution-1fad1056
  (fn [xs] (set (map set (filter #(not-empty (rest %)) (vals (group-by sort xs)))))))

(defcheck solution-1fc4b452
  (fn [v]
    (into #{}
      (map set
        (filter #(> (count %) 1)
          (map val (group-by sort v)))))))

(defcheck solution-1ffdb837
  (fn find-anagrams [words]
    (let [is-word-anagram (fn [a b] (= (sort a) (sort b)))
          contains-anagram (fn [sets word]
                             (if (some #(is-word-anagram (first %) word) sets)
                               true false))
          add-word (fn [set word]
                     (if (is-word-anagram (first set) word)
                       (conj set word)
                       set))
          add-anagram (fn [sets word]
                        (if (contains-anagram sets word)
                          (map #(add-word % word) sets)
                          (conj sets #{word})))]

      (loop [ret #{}
             words words]
        (if (empty? words)
          (set (remove #(= (count %) 1) ret))
          (recur
            (add-anagram ret (first words))
            (rest words)))))))

(defcheck solution-20128fb0
  (fn anagrams [s]
    (->> (group-by #(->> (seq %)
                      sort
                      (apply str))
           s)
      vals
      (map set)
      (filter #(> (count %) 1))
      set)))

(defcheck solution-20204960
  #(set
     (map set
       (filter
         (fn [x] (< 1 (count x)))
         (vals (group-by frequencies %))))))

(defcheck solution-2021fce8
  (fn [words]
    (set (remove #(< (count %) 2) (map set (vals (group-by sort words)))))
    ))

(defcheck solution-204f7f84
  (fn [v]
    (reduce
      (fn [s [k kv]]
        (if (> (count kv) 1) (conj s (set kv)) s ))
      #{} (group-by frequencies v))))

(defcheck solution-20645a92
  (fn [words]
    (let [groups (group-by #(apply str (sort %)) words)
          anags (filter #(< 1 (count (val %))) groups)]
      (set (map set (vals anags))))))

(defcheck solution-20ce68a4
  (fn anagram-finder [coll]
    (set
      (keep #(if (next %) (set %))
        (vals (group-by sort coll))))))

(defcheck solution-211d1f83
  ; This example is pretty brilliant.
  ;(fn [ws]
  ;  (set (map (comp set val)
  ;            (remove (comp #{1} count val) (group-by frequencies ws)))))

  ; This is a really clever way to do it with a creative use of sort on strings.
  (fn [v]
    (into #{}
      (map set
        (filter #(> (count %) 1)
          (map val (group-by sort v)))))))

(defcheck solution-21977a95
  (fn [words]
    (set (map set
           (filter #(> (count %) 1)
             (vals (group-by (comp set seq) words)))))))

(defcheck solution-219bc1df
  (fn [coll]
    (into #{} (filter #(> (count %) 1) (vals (loop [coll coll acc {}]
                                               (if-let [fst (first coll)]
                                                 (if (contains? acc (sort fst))
                                                   (recur (rest coll) (assoc acc (sort fst) (conj (acc (sort fst)) fst)))
                                                   (recur (rest coll) (conj acc [(sort fst) #{fst}])))
                                                 acc)))))))

(defcheck solution-220019a9
  (fn [coll]
    (->> coll
      (group-by sort)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-22345fb7
  (fn [s]
    (set (map set (filter #(> (count %) 1) (vals (group-by frequencies s)))))))

(defcheck solution-22a1c5e6
  (fn [l]
    (set (map set
           (filter #(> (count %) 1)
             (vals (group-by #(apply merge-with + (map (fn [x] {x 1}) %))
                     l)))))))

(defcheck solution-22ad04ad
  (fn this
    ([l]
     (if (empty? l) (set l)
                    (set (filter (fn [x] (>= (count x) 2)) (this (sort(first l)) l #{} [])))))
    ([n l a b]
     (if (empty? l)
       (concat #{a} (this b))
       (if (= n (sort (first l)))
         (recur n (rest l) (conj a (first l)) b)
         (recur n (rest l) a (conj b (first l))))))))

(defcheck solution-2388072
  #(set (map set (remove (comp (partial = 1) count) (vals (group-by set %))))))

(defcheck solution-23afcf3
  (fn [wl]
    (set (for [[k v] (group-by #(sort (seq %)) wl)
               :when (> (count v) 1)]
           (set v)))))

(defcheck solution-23b9d4db
  (fn [a]
    (set (filter #(> (count %1) 1)
           (vals (reduce #(assoc %1 (sort %2)  (conj (get %1 (sort %2) #{}) %2)) {} a))
           ))
    ))

(defcheck solution-23cf9845
  (fn [S] (->> (group-by frequencies S) vals (filter second) (map set) set)))

(defcheck solution-25165863
  (fn [xs]
    (->> xs
      (group-by #(sort %))
      (vals)
      (filter #(> (count %) 1))
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-25e3bdb9
  (fn [words]
    (->> words
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-25e4e2f5
  (fn [a]
    (reduce #(conj %1 (into #{} %2)) #{}
      (filter #(> (count %) 1)
        (vals (group-by #(into #{} %) a))))))

(defcheck solution-25f044b8
  (fn [coll]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals
            (group-by
              sort coll)))))))

(defcheck solution-2718647a
  (fn [s] (->> (map #(hash-map (sort %) #{%}) s)
            (apply merge-with clojure.set/union)
            (vals)
            (filter next)
            (set))))

(defcheck solution-2739a37f
  (fn [coll]
    (->> coll
      (group-by sort)
      (vals)
      (filter #(> (count %) 1))
      (map (partial apply hash-set))
      (apply hash-set))))

(defcheck solution-274d3049
  (fn anagram-finder
    [coll]
    (into #{} (->> coll
                (reduce (fn [acc item]
                          (update-in acc [(apply str (sort item))] conj item)) {} )
                (filter #(> (count (second %)) 1))
                (map #(second %))
                (map set)))))

(defcheck solution-27c13148
  #(set (filter (fn [x] (< 1 (count x))) (map set (vals (group-by (fn [s] (sort (seq s))) %))))))

(defcheck solution-27e92930
  (fn anagram-finder
    [coll]
    (set (map set
           (filter #(not= (count %) 1)
             (vals (group-by (comp identity sort) coll)))))))

(defcheck solution-2821ddea
  (fn [s] (set (remove #(= 1 (count %)) (map set (vals (group-by sort s)))))))

(defcheck solution-284bb91f
  (fn [ls]
    (->>
      ls
      (group-by #(sort %))
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-2862fec9
  (fn [coll]
    (letfn [(map-em [coll]
              (loop [c coll
                     o {}]
                (if (empty? c)
                  o
                  (let [f (first c)]
                    (recur (rest c) (conj o {f, (frequencies (clojure.string/split f  #""))}))))))
            (group-em [coll]
              (group-by #(sort (keys (val %))) (map-em coll)))
            (filter-em [coll]
              (filter #(if (> (count %) 1) %) (vals (group-em coll))))
            (return-em [coll]
              (set
                (for [x (filter-em coll)]
                  (set (map first x)))))]
      (return-em coll))))

(defcheck solution-28702763
  (fn anagram [words]
    (->> words
      (group-by sort)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-28737693
  (fn anagrams
    [coll]
    (let [find-anagram (fn [c]
                         (let [f (sort (first c)) fe (first c)]
                           (reduce  #(if (= f (sort %2)) (conj % %2 fe) %) #{} (rest c))))]
      ( if (< (count coll) 2) #{}
                              (let [found (find-anagram coll)]
                                (if (empty? found) #{}
                                                   (into #{found} (anagrams  (remove found coll)))))))))

(defcheck solution-28d4444d
  (fn [x](set (map set (filter #(> (count %) 1)
                         (vals (group-by #(sort %) x)))))))

(defcheck solution-2905f570
  (fn [s] (set (mapcat #(if (second %) [(set %)]) (vals (group-by #(set %) s))))))

(defcheck solution-29bd615e
  (fn [coll]
    (letfn [(anagrams [w1 ws]
              (set (filter seq (for [w2 ws]
                                 (if (= (sort w1) (sort w2)) w2)))))]
      (into #{} (filter #(< 1 (count %)) (map #(anagrams % coll) coll))))))

(defcheck solution-29ce03ba
  (fn p77
    [x]                    ;["meat" "mat" "team" "mate" "eat"]
    (->>
      (group-by set x)  ;{#{\a \e \m \t} ["meat" "team" "mate"], #{\a \m \t} ["mat"], #{\a \e \t} ["eat"]}
      (vals)            ;(["meat" "team" "mate"] ["mat"] ["eat"])
      (filter next)     ;(["meat" "team" "mate"])
      (map set)         ;(#{"meat" "mate" "team"})
      (set))))

(defcheck solution-2a4331e1
  #(set (map set (filter next (vals (group-by set %))))))

(defcheck solution-2a63cb1d
  (fn anagrams [v]
    (let [sorted-map (group-by (comp (partial apply str) sort) v)]
      (set (map set (filter #(> (count %) 1) (vals sorted-map)))))))

(defcheck solution-2af7beb
  (fn [ws]
    (let [m (reduce (fn [a b]
                      (let [k (sort (seq b)) e (or (a k) #{})]
                        (assoc a k (conj e b)))) {} ws)]
      (reduce (fn [a [_ v]] (if (> (count v) 1) (conj a v) a)) #{} m))))

(defcheck solution-2b441b02
  (fn [s]
    (->> (group-by #(sort %) s)
      vals
      (map set)
      (filter #(> (count %) 1))
      set)))

(defcheck solution-2bc17c1d
  (fn [v]
    (->> v
      (group-by #(set %))
      vals
      (filter second)
      (map set)
      set)))

(defcheck solution-2bd8b741
  (fn [ss]
    (reduce
      (fn [s [_ v]] (if (> (count v) 1) (conj s v) s))
      #{}
      (reduce
        (fn [m s]
          (let [k (set s)]
            (assoc m k (conj (set (get m k)) s))))
        {} ss))))

(defcheck solution-2c015f0b
  (fn [c](->> c
           (reduce #(assoc %1 (sort %2)
                              (conj (get %1 (sort %2) #{}) %2)) {})
           vals (filter #(> (count %) 1)) set)))

(defcheck solution-2c1c0e26
  (fn [c]
    (set (map set
           (filter #(< 1 (count %))
             (vals (group-by sort c)))))))

(defcheck solution-2c44cf2a
  (fn anagram-finder [coll]
    (letfn [(anagram-word? [s1 s2]
              (let [rearrange (comp sort shuffle)]
                (= (rearrange (seq s1))
                  (rearrange (seq s2)))))
            (a-anagram-in-set? [a se] ;;se,&#34920;&#31034;set
              (some #(anagram-word? a %) se))

            (a-anagram-in-set-set? [a set-set]
              (some #(a-anagram-in-set? a %) set-set))
            (get-anagram-group-in-set-set [a set-set]
              (first (filter #(a-anagram-in-set? a %) set-set)))]
      (into #{}
        (filter #(>= (count %) 2)
          (loop [ret #{} coll coll]
            (if (seq coll)
              (if (a-anagram-in-set-set? (first coll) ret)
                (recur (conj (disj ret (get-anagram-group-in-set-set (first coll) ret))
                         (conj (get-anagram-group-in-set-set (first coll) ret) (first coll)))
                  (rest coll))
                (recur (conj ret #{(first coll)}) (rest coll)))
              ret)))))))

(defcheck solution-2c587cc7
  (fn [c] (set (map set (filter #(not= 1 (count %)) (partition-by sort (sort-by #(apply str (sort %)) c)))))))

(defcheck solution-2c7183bf
  (fn [words]
    (loop [groups #{} rest-words words]
      (if (empty? rest-words)
        groups
        (let [characters (sort (vec (first rest-words)))
              {anagrams true next-rest-words false} (group-by #(= characters (sort (vec %)))
                                                      (rest rest-words))]
          (recur (if (empty? anagrams)
                   groups
                   (conj groups (set (conj anagrams (first rest-words)))))
            next-rest-words))))))

(defcheck solution-2c9a9123
  (fn[c]
    (->> c
      (group-by frequencies)
      (map (comp (partial apply hash-set) second))
      (filter #(-> % count dec pos?))
      (apply hash-set))))

(defcheck solution-2ca9b423
  (fn [x] (->> (group-by frequencies x) (map second) (filter #(> (count %) 1)) (map set) set)))

(defcheck solution-2cc9002f
  (fn [x] (into #{} (map set (filter #(> (count %) 1) (vals (group-by sort x)))))))

(defcheck solution-2cf10a40
  (fn [words]
    (->> (group-by frequencies words)
      (vals)
      (filter #(< 1 (count %)))
      (map set)
      (set))))

(defcheck solution-2d1d7115
  #(into #{}
     (filter (fn [x] (> (count x) 1))
       (map set
         (vals (group-by sort %))))))

(defcheck solution-2d407a97
  (fn [words]
    (set (filter #(> (count %) 1) (map set (vals (group-by frequencies words)))))))

(defcheck solution-2de7db0c
  (fn anagrams [coll]
    (->> coll
      (group-by (partial group-by identity))
      (map second)
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-2df4ff08
  (fn find-anagrams [words]
    (->>
      words
      (reduce
        (fn [m x]
          (let [sorted-word (apply str (sort x))]
            (assoc m sorted-word (conj (get m sorted-word #{}) x))
            ))
        {})
      vals
      (filter #(> (count %1) 1))
      set)))

(defcheck solution-2e03d913
  (fn [words]
    (let [freqs #(frequencies (clojure.string/split % #""))]
      (->> (group-by freqs words)
        vals
        (filter #(< 1 (count %)))
        (map set)
        set))))

(defcheck solution-2e4ec727
  #(set (filter (fn [s] (<= 2 (count s))) (map (comp set second) (group-by sort %)))))

(defcheck solution-2ebc87e7
  (fn [s]
    (into #{}
      (for [[k v] (group-by (comp str sort seq) s) :when (> (count v) 1)]
        (set v)))))

(defcheck solution-2ec1fc0
  (fn [coll]
    (let [ count-char-map (fn [x] (apply (partial merge-with + ) (map  #(sorted-map % 1) x)))
          s (group-by count-char-map  coll)
          lst (filter #(< 1 (count (val %))) s)]
      (set (map #(set (val %)) lst)))))

(defcheck solution-2f09ba9c
  (fn [w]
    (set
      (map (comp set val)
        (remove (comp #{1} count val) (group-by frequencies w))))))

(defcheck solution-2f29c2bc
  (fn ana [words]
    (set (map set (filter #(< 1 (count %)) (vals (group-by sort words)))))))

(defcheck solution-30120f33
  #(set (map set (filter (comp seq rest) (vals (group-by sort %))))))

(defcheck solution-30a018ba
  #(set (filter (fn [v] (> (count v) 1)) (map set (vals (group-by sort %))))))

(defcheck solution-31051875
  (fn anagram [coll]
    (let [groups  (group-by identity (map (fn [v] (set v)) coll))]
      (set (for [a groups :when (> (count (second a)) 1)]
             (reduce (fn [acc v] (if (= (set v) (first a)) (conj acc v) acc)) #{} coll)
             ))
      )
    ))

(defcheck solution-3290cfaf
  (fn [l] (set (filter #(> (count %) 1) (map set (vals (group-by sort l)))))))

(defcheck solution-329b82ef
  (fn anagram-finder' [words]
    (->> words
      (group-by sort)
      (map (comp set last))
      (filter (comp (partial < 1) count))
      set)))

(defcheck solution-33011301
  (fn [w]
    (set (map set (remove #(= 1 (count %))
                    (vals (group-by #(sort %) w)))))))

(defcheck solution-337d72a0
  (fn [s]
    (let [anagram? (fn [x y] (= (sort x) (sort y)))]
      (into #{} (filter #(<= 2 (count %))
                  (map (fn [x]
                         (into #{} (filter #(anagram? x %) s)))
                    s))))))

(defcheck solution-3416a803
  (fn [words]
    (->> (group-by sort words)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-34726b1c
  (fn [words]
    (->>
      (map (fn [word] [word (sort (seq word))]) words)
      (group-by last)
      (vals)
      (map #(map first %))
      (remove #(= 1 (count %)))
      (map #(into #{} %))
      (into #{}))
    ))

(defcheck solution-34e1a9d1
  (fn anagram-sets [allwords]
    (set (map set
           (filter #(>= (count %) 2)
             (vals (group-by set allwords)))))))

(defcheck solution-357530aa
  (fn [s]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by frequencies s)))))))

(defcheck solution-35adccf3
  #(set (map set (filter second (vals (group-by sort %))))))

(defcheck solution-362b92ce
  (fn [xs]
    (let [anagram? (fn [a b] (= (sort a) (sort b)))]
      (->> (reduce (fn [a i]
                     (->> xs
                       (filter #(anagram? % i))
                       (into #{})
                       (conj a)))
             []
             xs)
        (filter #(> (count %) 1))
        (into #{})))))

(defcheck solution-368785
  (fn [items]
    (set
      (filter #(> (count %) 1)
        (map #(set (last %))
          (group-by #(sort %) items))))))

(defcheck solution-36949511
  (fn [coll]
    (apply hash-set
      (filter #(> (count %) 1)
        (vals (loop [coll coll
                     answer {}]
                (if (nil? coll) answer
                                (let [key (sort (seq (first coll)))
                                      val (first coll)]
                                  (recur (next coll)
                                    (assoc answer key (if-let [entry (get answer key)] (conj entry val)
                                                                                       (hash-set val))))))))))))

(defcheck solution-3703536b
  (fn [x] (set (for [i (vals (group-by sort x)) :when (< 1 (count i))] (set i)))))

(defcheck solution-37236c85
  (fn anagrams [coll]
    (set (filter #(> (count %) 1) (map set (vals (group-by #(sort (seq %)) coll)))))
    ))

(defcheck solution-37240f40
  (fn [c] (set (map set (filter #(< 1 (count %)) (vals (group-by set c)))))))

(defcheck solution-37482ff3
  (fn [ss] (->> ss
             (group-by sort)
             vals
             (filter #(> (count %) 1))
             (map set)
             set)))

(defcheck solution-3756f69a
  (fn [coll] (set (filter #(> (count %) 1) (vals (reduce #(let [key (sort (into [] %2))] (assoc % key (set (conj (% key) %2)))) {} coll))))))

(defcheck solution-376a1ad4
  (fn anagram [words]
    (into #{}
      (filter #(> (count %) 1)
        (map #(into #{} %)
          (vals
            (group-by sort words)))))))

(defcheck solution-37863bcb
  (fn f[xs]
    (->> (group-by sort xs)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-37f72b57
  (fn [words]
    (set
      (for [[_ v] (group-by set words)
            :when (> (count v) 1)]
        (set v)))))

(defcheck solution-38228319
  (fn anagram-finder [words]
    (let [normalize-word (fn [word]
                           (apply str (sort word)))
          filter-and-prepare (fn [set-of-sets]
                               (into #{} (filter #(> (count %) 1) (into #{} (vals set-of-sets)))))
          inner-anagram-finder (fn [words acc]
                                 (if (seq words)
                                   (recur (rest words) (assoc acc (normalize-word (first words)) (conj (get acc (normalize-word (first words)) #{}) (first words))))
                                   (filter-and-prepare acc)))]
      (inner-anagram-finder words {}))))

(defcheck solution-387c9fd5
  (fn [words]
    (set (for [anagram-group (vals (group-by frequencies words))
               :let [more-than-one-word (not (nil? (next anagram-group)))]
               :when more-than-one-word]
           (set anagram-group)))))

(defcheck solution-388820c4
  (fn [words]
    (let [others (fn [word] (filter #(not (= word %)) words))
          seq-set (fn [word] (set (seq word)))
          anas (fn [word] (filter #(= (seq-set word) (seq-set %)) (others word)))
          ]
      (into #{} (filter #(> (count %) 1) (map (fn [a] (set (cons a (anas a)) )) words)))
      )
    ))

(defcheck solution-38ceff65
  (letfn [(A [w]
            (apply str (sort w)))
          (P [s]
            (set
              (for [[k v] (group-by A s) :when (> (count v) 1)]
                (into #{} v))))]
    P))

(defcheck solution-3953e868
  (fn [xs]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by #(set (re-seq #"." %)) xs)))))))

(defcheck solution-39b66a87
  (fn anagram-finder [str-coll]
    (set
      (for [[key val]
            (reduce (fn [hashmap a-str]
                      (let [str-key (apply str (sort a-str))]
                        (if-let [a-set (hashmap str-key)]
                          (conj hashmap [str-key (conj a-set a-str)])
                          (conj hashmap [str-key #{a-str}]))))
              {}
              str-coll)
            :when (>= (count val) 2)]
        val))))

(defcheck solution-3a0b3352
  (fn [vs]
    (let [char-map #(reduce (fn [m k] (update-in m [k] (fnil inc 0))) {} %)]
      (set
        (map set
          (filter
            #(> (count %) 1)
            (vals (apply merge-with concat
                    (map #(hash-map (char-map %) [%]) vs)))))))))

(defcheck solution-3a183c9d
  (fn [lst]
    (->> lst
      (group-by #(frequencies (seq %)))
      (vals)
      (map set)
      (filter #(>= (count %) 2))
      (into #{}))))

(defcheck solution-3a2a4ca
  (fn [s] (set (map set (filter #(> (count %1) 1) (vals (group-by set s)))))))

(defcheck solution-3a626443
  (fn [coll]
    (into #{} (->> coll
                (group-by sort)
                vals
                (filter #(> (count %) 1))
                (map set)))))

(defcheck solution-3a690617
  (fn [l]
    (->> l (group-by set) vals (filter (comp seq rest)) (map set) set)))

(defcheck solution-3a79aab8
  (fn [x]
    (->> x
      (group-by frequencies)
      vals
      (map (partial into #{}))
      (filter #(> (count %) 1))
      (into #{})
      )))

(defcheck solution-3b30b5ed
  (letfn [(normalize [word] (apply str (sort (clojure.string/replace (clojure.string/lower-case word) #"[^a-z]" ""))))]
    (fn [words]
      (set (->> words
             (group-by normalize)
             (map #(% 1))
             (filter #(> (count %) 1))
             (map set))))))

(defcheck solution-3b85d924
  (fn [w]
    (set (filter #(> (count %) 1) (map (comp set second) (group-by sort w))))))

(defcheck solution-3ba01a96
  (fn [coll]
    (->>
      coll
      (group-by sort)
      (filter #(> (count (second %)) 1))
      (map #(set (second %)))
      set)))

(defcheck solution-3bf9a438
  (fn x[s](into #{} (map set (remove #(> 2 (count %)) (vals (group-by frequencies s)))))))

(defcheck solution-3c3b8621
  (fn [col]
    (into #{} (filter #(< 1 (count %)) (vals (reduce (fn [ret w]
                                                       (update-in ret [(sort w)] (fnil conj #{}) w)) {} col))))))

(defcheck solution-3cfa9ad8
  (fn [strs]
    (set (filter #(> (count %) 1) (map set (vals (group-by set strs)))))))

(defcheck solution-3d6229d0
  (fn [x] (set (filter #(> (count %) 1)
                 (map set (vals (group-by frequencies x)))))))

(defcheck solution-3d6c5164
  (fn [x]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort x)))))))

(defcheck solution-3db4b4a2
  (fn [words]
    (->> words
      (group-by #(set %))
      vals
      (filter #(> (count %) 1))
      (map set)
      (into #{}))))

(defcheck solution-3db572ec
  (fn [coll]
    (set
      (for [[_ v] (group-by set coll)
            :when (> (count v) 1)]
        (set v)))))

(defcheck solution-3e1588
  (fn anagram-finder
    [word-vector]
    (->> (reduce #(update-in %1 [(into #{} %2)] (fn [myset] (conj myset %2))) (cons {} word-vector))
      ;; associate hash-sets of chars with a sets of their corresponding words in a map
      vals
      (map #(into #{} %))
      (filter #(> (count %) 1))
      (into #{}))))

(defcheck solution-3e46326f
  (fn anagram-finder [lword]
    (letfn [(same [lw resp-map]
              (if (= lw []) resp-map
                            (let [f (first lw)
                                  sf (apply str (sort f))]
                              (if (contains? resp-map sf)
                                (same (rest lw) (assoc resp-map sf (conj (resp-map sf) f)))
                                (same (rest lw) (assoc resp-map sf [f]))))))]
      (let [mapw (same lword {})
            ks (keys mapw)]
        (reduce #(if (> (count (mapw %2)) 1)
                   (conj %1 (set (mapw %2)))
                   %1) #{} ks)))))

(defcheck solution-3e67e01a
  (fn anagram [x] (into #{} (map set (filter #(> (count %) 1) (vals (group-by sort x)))))))

(defcheck solution-3e8d03a0
  (fn [words]
    (let [hash-key #(apply str (sort (.toLowerCase %1)))
          groups (vals (group-by hash-key words))]
      (set (map set (filter #(> (count %) 1) groups))))))

(defcheck solution-3f42dd24
  (fn [v]
    (set (filter (fn [s] (> (count s) 1)) (map
                                            (fn [l] (set (flatten (map last l))))
                                            (map last (group-by first (map vector (map sort v) v))))))))

(defcheck solution-3f5b49ca
  (fn [s] (set (map set (filter #(< 1 (count %)) (vals (group-by sort s)))))))

(defcheck solution-3f9d2947
  (fn [i] (set (map set (remove #(< (count %) 2 ) (partition-by #(sort compare %) (sort #(compare (apply str (sort compare %)) (apply str (sort compare %2))) i)))))))

(defcheck solution-3fdce8d6
  (fn [c] (->> c (group-by sort) vals (filter #(< 1 (count %))) (map set) set)))

(defcheck solution-40504a48
  (fn [coll]
    (into #{} (filter #(> (count %) 1) (vals (reduce (fn [m v]
                                                       (let [k (sort v)]
                                                         (if (contains? m k)
                                                           (assoc m k (conj (m k) v))
                                                           (assoc m k #{v}))))
                                               {} coll))))))

(defcheck solution-406d1e6
  (fn [words]
    (let [hash-key #(apply str (sort (.toLowerCase %)))
          groups (vals (group-by hash-key words))]
      (set (map set (filter #(> (count %) 1) groups))))))

(defcheck solution-407128d0
  (fn f [s]
    (if (empty? s)
      #{}
      (let [fst (first s)
            ang? #(= (sort fst) (sort %))
            angs (filter ang? (rest s))
            not-angs (filter (complement ang?) (rest s))]
        (if (empty? angs)
          (f not-angs)
          (conj (f not-angs) (set (cons fst angs)))))
      )))

(defcheck solution-40743eae
  (fn me [args]

    (let [res (vals (group-by #(into #{} %) args) )

          ]
      (into #{} (map #(into #{} %) ( filter #(> (count %)  1) res)))
      )

    ))

(defcheck solution-40df99a6
  (fn [arg]
    (let [vec-arg (into {} (filter #(> (val %1) 1) (frequencies (map sort (map #(flatten (partition 1 %)) arg)))))]
      (loop [arg arg
             result {}]
        (if (empty? arg)
          (into #{}(vals result))
          (let [
                akey (sort (first arg))
                aval (first arg)
                containskey? (contains? vec-arg akey)
                getval (if (empty? result) #{aval} (into #{} (conj (get result akey) aval)))]
            (recur (rest arg)
              (if containskey? (assoc result akey getval) result))
            ))))))

(defcheck solution-4109b3ef
  (fn [words]
    (->> (group-by frequencies words)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-4189eb55
  (fn [x]
    (set (map set (filter #(< 1 (count %)) (vals (group-by set x)))))))

(defcheck solution-425eae34
  (fn [x]
    (->> x
      (group-by sort)
      vals
      (filter #(> (count %) 1 ))
      (map set)
      set)))

(defcheck solution-42b5e2a0
  #(set
     (map set
       (filter
         (fn [n]
           (< 1 (count n)))
         (vals (group-by sort %))))))

(defcheck solution-42d0d792
  (fn [coll]
    (let [grouped-coll (group-by #(set (seq %)) coll)]
      (set (for [[k v] grouped-coll
                 :when (> (count v) 1)]
             (set v))))))

(defcheck solution-42e23836
  #(->> (group-by sort %)
     vals
     (filter (fn [v] (> (count v) 1)))
     (map (partial apply hash-set))
     (apply hash-set)))

(defcheck solution-42ed81e8
  (fn ana [coll]
    (let [anagrams (group-by sort coll)]
      (set (map set (filter #(> (count %) 1) (vals anagrams)))))))

(defcheck solution-4306624d
  (fn [words]
    (letfn [(analyze [word]
              (reduce #(merge-with + %1 {%2 1}) {} word))]
      (loop [words words
             anagrams {}]
        (if (empty? words)
          (set (filter #(> (count %) 1) (vals anagrams)))
          (let [w (first words)
                a (analyze w)]
            (recur (rest words) (merge-with clojure.set/union anagrams {a #{w}}))))))))

(defcheck solution-43834756
  (fn [s] (set (filter (fn [x] (< 1 (count x))) (for [i (vals (group-by (fn [x] (apply str (sort x))) s))] (set i))))))

(defcheck solution-4392b212
  (fn [words]
    (->> (group-by sort words)
      vals
      (map set)
      (remove #(= 1 (count %)))
      set)))

(defcheck solution-43d7eaab
  (fn [ws]
    (->> ws
      (group-by set)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-4449d3c5
  (fn [words]
    (set (filter #(< 1 (count %))
           (vals (apply merge-with clojure.set/union
                   (map #(hash-map (frequencies %) #{%}) words)))))))

(defcheck solution-459305ee
  (fn [ws]
    (let [anags (group-by #(reduce str (sort %)) ws)]
      (set (map set (filter #(> (count %) 1) (vals anags)))))))

(defcheck solution-46057f60
  (fn [s]  (set (map set (filter #(> (count %) 1) (vals (group-by sort s)))))))

(defcheck solution-467e0d1
  (fn [xs]
    (set (map set (filter #(< 1 (count %))
                    (vals (group-by #(sort (seq %)) xs)))))))

(defcheck solution-46edaff
  (fn [xs] (->> (group-by (comp sort seq) xs) vals (filter #(> (count %) 1)) (map #(into #{} %) ) (into #{}))))

(defcheck solution-4768a0b9
  (fn [words]
    (let [sorted-seqs (map #(-> % seq sort) words)
          kv-words (zipmap words sorted-seqs)
          anagrams (group-by #(get % 1) kv-words)]
      (set
        (filter #(> (count %) 1)
          (map (fn [a]
                 (->> (get a 1)
                   (map #(get % 0))
                   (set)))
            anagrams))))))

(defcheck solution-478c6630
  (fn [strs]
    (set (filter #(> (count %) 1) (map set (vals (group-by frequencies strs)))))))

(defcheck solution-47e74489
  (fn [words]
    (->> words
      (group-by sort)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-47f99a7b
  (fn anagrams
    [words]
    (set (map set (filter (comp (partial < 1) count) (vals (group-by (comp sort seq) words)))))))

(defcheck solution-47ffacb
  (fn [ws] (into #{} (map set (filter #(> (count %) 1) (vals (group-by #(into #{} %) ws)))))))

(defcheck solution-4814b653
  (fn [arg] (set (map set (filter #(> (count %) 1) (map val (group-by #(set %) arg)))))))

(defcheck solution-481c2352
  (fn [words]
    (set (filter #(> (count %) 1) (map set (vals (group-by sort words)))))
    ))

(defcheck solution-48697ffd
  (fn [coll]
    (set
      (map #(set (second %))
        (filter #(> (count (second %)) 1)
          (group-by (partial group-by identity)
            coll))))))

(defcheck solution-4890796a
  #(set
     (map set
       (keep (fn [[k v]]
               (if (> (count v) 1) v))
         (group-by frequencies %)))))

(defcheck solution-48a9147f
  (fn [x] (set (map set (filter (fn [coll] (>= (count coll) 2))
                          (vals (group-by #(sort %) x)))))))

(defcheck solution-48cd004c
  (fn __ [coll]
    (into #{}
      (for [[k v] (group-by #(sort %) coll) :when (< 1 (count v))]
        (into #{} v)))
    ))

(defcheck solution-49903e9b
  (fn [l]
    (set
      (map set
        (filter
          #(> (count %) 1)
          (vals
            (group-by sort l)))))))

(defcheck solution-49c796d8
  (fn [xs]
    (->> (group-by set xs)
      vals
      (filter #(not (empty? (rest %))))
      (map set)
      set)))

(defcheck solution-4a4a3ace
  (fn [lista] (set (filter #(< 1 (count %)) (map set (set (vals (group-by sort lista))))))))

(defcheck solution-4aec855
  (fn anagramm [c]
    (set (map set (filter
                    #(> (count %) 1)
                    (vals (group-by sort c)))))))

(defcheck solution-4b6c29e1
  (fn [V]
    (apply hash-set (filter (fn [s] (< 1 (count s))) (map set (vals (group-by sort V) ))))
    ))

(defcheck solution-4c33f10a
  (fn [c] (set (map set (filter #(> (count %) 1) (vals (group-by sort c)))))))

(defcheck solution-4c76a166
  (fn [strings];anagrams have the same histogram
    (reduce conj #{};prepare the result
      (map set (remove #(= 1 (count %));remove the words without any anagrams
                 (vals (group-by frequencies strings)))))))

(defcheck solution-4c90d570
  (fn anagram-finder [xs]
    (->>
      xs
      (group-by sort)
      (map (comp set val))
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-4ccb8e62
  (fn [c]
    (->> c (group-by #(sort %)) vals (map set) (filter #(< 1 (count %))) set)))

(defcheck solution-4d5c44dc
  (fn n77 [coll]
    (set (for [[k v] (reduce #(merge-with clojure.set/union %1 %2)
                       (map #(apply hash-map %)
                         (map vector (map #(sort (vec %)) coll) (map #(set [%]) coll))))
               :when (> (count v) 1)]
           v))))

(defcheck solution-4dab2a85
  (fn [xs]
    (->> xs
      (group-by sort)
      (vals)
      (remove #(= 1 (count %)))
      (map set)
      (set))))

(defcheck solution-4ddf29a8
  (fn anagrams[words] (->> (reduce (fn [a b] (assoc a (sort b) (cons b (get a (sort b))))) {} words) vals (filter #(> (count %) 1)) (map set) set)))

(defcheck solution-4de0bf2e
  #(reduce-kv (fn [s _ v]
                (if (next v)
                  (conj s (set v))
                  s))
     #{}
     (group-by sort %)))

(defcheck solution-4df66cf7
  (fn [lst]
    (set (map set (filter #(> (count %) 1) (vals (group-by set lst)))))))

(defcheck solution-4e7218a4
  (fn [w]
    (->> w
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map #(apply hash-set %))
      (apply hash-set))))

(defcheck solution-4e8675e1
  (fn f [l]
    (set (filter #(> (count %) 1) (map set (vals (group-by #(set (seq %)) l)))))))

(defcheck solution-4ea61107
  (fn [s]
    (->> s
      (group-by
        #(->> % seq sort))
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-4eab9730
  (fn [words]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort words)))))
    ))

(defcheck solution-4eda56de
  (fn [l]
    (set (map set (filter #(> (count %) 1) (vals (group-by set l)))))))

(defcheck solution-4f08f0a7
  (fn finalAnswer [x] (into #{} (letfn [(ana? [first sec] (= (set (seq first)) (set (seq sec))))
                                        (anaFinder1 [y] (map set
                                                          (filter #(> (count %) 1) (if (= 1 (count y))
                                                                                     #{}
                                                                                     (concat #{(conj (filter #(ana? (first y) %) (rest y)) (first y))}
                                                                                             (anaFinder1 (rest y)))))))
                                        ]
                                  (let [preResult (anaFinder1 x)] (filter
                                                                    (fn [item] (reduce #(and (not (and (clojure.set/subset? item %2) (not= item %2))) %1) true preResult))
                                                                    preResult))))))

(defcheck solution-4f73ec3d
  (fn [x] (set (filter second (map set (vals (group-by sort x)))))))

(defcheck solution-4fa6f71f
  (fn [v]
    (let [res (loop [ret {} lft v]
                (if (empty? lft)
                  ret
                  (let [elt (first lft)
                        m-v (ret (sort elt) #{})]
                    (recur (assoc ret (sort elt) (conj m-v elt))
                      (rest lft)))))]
      (set (remove #(= 1 (count %)) (vals res))))))

(defcheck solution-501b74bc
  (fn [w]
    (let [add #(let [s (sort %2)
                     c (%1 s)]
                 (conj %1 [s (conj c %2)]))
          mapped (reduce add (zipmap (map sort w) (repeat #{})) w)]
      (into #{} (filter #(> (count %) 1) (vals mapped))))))

(defcheck solution-503162de
  (fn [coll]
    (into #{} (map #(into #{} %) (filter #(> (count %) 1) (map last (group-by sort coll)))))))

(defcheck solution-503c964e
  (fn
    [coll]
    (let [sorted-list (sort-by #(str (sort (seq %))) coll)]
      (->> (reduce #(if (= (sort (seq (first (first %1))))
                          (sort (seq %2)))
                      (conj (rest %1) (conj (first %1) %2))
                      (conj %1 #{%2}))
             [#{(first sorted-list)}]
             (rest sorted-list))
        (filter #(> (count %) 1))
        (into #{})))))

(defcheck solution-504784cd
  (fn [s]
    (set (filter #(< 1 (count %)) (map set
                                    (vals (group-by frequencies s)))))))

(defcheck solution-50a07cf4
  (fn [ws]
    (let [ks (distinct (map sort ws))]
      (into #{}
        (filter #(> (count %) 1)
          (vals
            (reduce (fn [s item]
                      (update-in s [(sort item)] conj item))
              (zipmap ks (repeat #{}))
              ws)))))))

(defcheck solution-5126f1c1
  (fn [d] (letfn
           [(group-into-ana-sets [a] (group-by #(set (seq %)) a))
            (select-non-lonely [b] (filter #(> (count (val %)) 1) b))
            (format-my-shit [c] (set (map #(set (nth % 1)) c)))]
            (-> d group-into-ana-sets select-non-lonely format-my-shit))))

(defcheck solution-51572e64
  (fn [ws]
    (set
      (filter #(> (count %) 1) (map set (vals (group-by #(sort (seq %)) ws)))))))

(defcheck solution-51866b2b
  (fn f2[x]
    (set (map set (filter #(> (count %) 1)
                    (set (map set
                           (vals (group-by #(sort (map identity %)) x)))))))))

(defcheck solution-51d4a48b
  (fn[slist]
    (loop [s1 slist ret #{}]
      (if (empty? s1)
        ret
        (let [s (set (filter (fn[s2] (= (sort (first s1)) (sort s2))) slist))]
          (recur (rest s1) (if (> (count s) 1) (conj ret s) ret))
          )
        )
      )
    ))

(defcheck solution-51d59816
  (fn [xs]
    (letfn [(gmap [s] (reduce #(assoc % %2 (if (% %2) (inc (% %2)) 1)) {} s))]
      (set (map set (filter #(> (count %) 1) (vals (group-by gmap xs))))))))

(defcheck solution-5251f6c7
  (fn anagrams [coll]
    (set (filter #(> (count %) 1)
           (map set (vals (group-by sort coll)))))))

(defcheck solution-5259323
  (fn [xs]
    (->> xs
      (map (fn [x] [(set x) x]) )
      (group-by first)
      (map second)
      (map #(map second %))
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-526b692b
  (fn [v]
    (loop [v v
           acc #{}]
      (if (= (count v) 0)
        (set (remove #(= 1 (count %)) acc))
        (recur
          (remove #(= (sort %) (sort (first v))) v)
          (conj
            acc
            (set (filter #(= (sort %) (sort (first v))) v))))))))

(defcheck solution-529aea7a
  (fn f [i [h & t :as s]]
    (if (nil? h)
      i
      (let [fit (set (filter #(= (set h) (set %)) t))]
        (recur (if (seq fit) `#{~@i #{~@fit ~h}} i)
          (vec (clojure.set/difference (set t) fit)))))) #{})

(defcheck solution-52dc6314
  (fn [c] (set (filter #(> (count %) 1) (map set (vals (group-by sort c)))))))

(defcheck solution-52eb8318
  (fn anagram [v]
    (set (map set (filter #(> (count %) 1) (vals (group-by #(set (seq %)) v)))))))

(defcheck solution-52fd69fd
  (fn [ws]
    (let [gs (vals (group-by sort ws))]
      (set
        (keep #(when (> (count %) 1) (set %))
          gs)))))

(defcheck solution-5328f153
  #(into #{} (filter (fn [xs] (> (count xs) 1))
               (map (fn [xs] (into #{} (map second xs)))
                 (vals (group-by first
                         (map (fn [x] [(frequencies x) x]) %)))))))

(defcheck solution-5329f95
  (fn [words]
    (set
      (filter #(< 1 (count %))
        (map #(set (second %))
          (group-by sort words))))))

(defcheck solution-53375630
  (fn prob77
    [words]
    (into #{} (map set (filter #(> (count %) 1) (map val (group-by sort words)))))))

(defcheck solution-536db901
  (fn [coll]
    (into #{} (map #(into #{} %) (filter #(> (count %) 1) (vals (group-by frequencies coll)))))))

(defcheck solution-536fdd4c
  (fn anagrams [words]
    (set (filter
           #(>= (count %) 2)
           (vals (reduce (fn [a b]
                           (assoc a (sort b) (conj (a (sort b) #{}) b)))
                   {} words))))))

(defcheck solution-54a53a4a
  #(letfn [(groupwords [wordsets word]
             (loop [input wordsets, output #{}]
               (cond
                 (empty? input)
                 (conj output #{word})
                 (= (set word) (set (first (first input))))
                 (conj
                  (clojure.set/union (set (rest input)) output)
                  (conj (first input) word))
                 :else
                 (recur
                   (rest input)
                   (conj output (first input))))))
           (singleton? [wordset]
             (= 1 (count wordset)))]
     (->> %
       (reduce groupwords #{})
       (remove singleton?)
       set)))

(defcheck solution-551500f7
  (fn [s]
    (set (filter #(> (count %) 1)
           (vals (reduce #(let [k (sort %2) v (%1 k) w (if v (conj v %2) #{%2})]
                            (assoc %1 k w)) {} s))))))

(defcheck solution-554e7d0a
  (fn [coll]
    (set
      (filter #(> (count %) 1)
        (map
          set
          (vals
            (group-by
              #(group-by identity %)
              coll)))))))

(defcheck solution-5558953e
  (fn anagram[l]
    (set (filter
           #(> (count %) 1)
           (set (map
                  (fn [s]
                    (set(filter
                          #(= (sort s) (sort %))
                          l))
                    )
                  l))))))

(defcheck solution-55ca76f8
  (fn [coll]
    (->>
      (group-by frequencies coll)
      (map val)
      (filter #(<= 2 (count %)))
      (map set)
      (set))))

(defcheck solution-55fb7a57
  (fn [col]
    (set
      (map
        set
        (filter
          #(> (count %) 1)
          (vals
            (group-by
              (fn [w]
                (sort (seq w))
                )
              col
              )
            )
          )
        )
      )
    ))

(defcheck solution-56336501
  (fn anagrams [words]
    (let [sortw (fn [word] (str (sort word)))
          res (reduce (fn [res word]
                        (update-in res [(sortw word)] #((fnil conj #{}) % word)))
                {} words)]
      (set (filter #(> (count %) 1) (vals res))))))

(defcheck solution-56b970c7
  (fn [words] (->> words
                (group-by set)
                (vals)
                (map set)
                (filter #(> (count %) 1))
                (set))))

(defcheck solution-56d4c73e
  (fn [words]
    (set (map set (filter #(> (count %) 1) (vals (group-by #(sort %) words)))))))

(defcheck solution-57042873
  (fn [coll] (into #{} (map last (filter #(> (count (last %)) 1) (apply merge-with clojure.set/union (map (fn [y]{((comp (partial clojure.string/join "") sort) y ) #{y} }) coll)))))
    ))

(defcheck solution-577739d
  (fn [col]
    (set (map (comp set second) (filter #(< 1 (count (val %))) (group-by #(identity (set %)) col))))))

(defcheck solution-578812ff
  (fn [lst]
    (->> (group-by sort lst)
      (vals)
      (remove #(= (count %) 1))
      (map set)
      (set))))

(defcheck solution-57abff18
  #(->> % (group-by sort) vals (filter next) (map set) set))

(defcheck solution-57ef6ba3
  (fn [words]
    (->> words
      (group-by frequencies)
      (map second)
      (map set)
      (filter #(< 1 (count %)))
      set)))

(defcheck solution-58f23708
  (fn [ws]
    (set (remove #(< (count %) 2) (map (fn [[k v]] (set v)) (group-by sort ws))))))

(defcheck solution-590faa23
  (fn [xs] (set (filter #(> (count %) 1) (map set (vals (group-by set xs)))))))

(defcheck solution-5965d12f
  (fn anabin [xs]
    (letfn [(anagram? [a, b]
              (= (sort (seq a)) (sort (seq b))))]
      (loop [in xs
             out #{}]
        (if (empty? in)
          out
          (let [f (first in)
                r (rest in)
                anas (filter (partial anagram? f) r)
                nanas (filter #(not (anagram? f %)) r)]
            (recur nanas (if (empty? anas)
                           out
                           (conj out (set (conj anas f)))))))))))

(defcheck solution-59e1c196
  (fn [v]
    (->> v
      (group-by (fn [w] (apply merge-with + (map #(hash-map % 1) w))))
      (filter #(> (count (val %)) 1))
      (map (comp set val))
      set)))

(defcheck solution-59f8248a
  (fn [s]
    (set (filter #(> (count %) 1)
           (reduce #(conj % (set (filter (fn [a] (= (sort a) (sort %2))) s))) #{} s)))
    ))

(defcheck solution-5a4e2685
  (fn [words]
    (set
      (filter
        #(< 1 (count %))
        (map
          (comp set val)
          (group-by frequencies words))))))

(defcheck solution-5a57aa9d
  (fn [ws] (set (map set (filter #(> (count %) 1) (vals (group-by frequencies ws)))))))

(defcheck solution-5a98cf2
  (fn [words]
    (->>
      words
      (group-by #(sort %))
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-5ae49f27
  (fn [x] (set (filter #(> (count %) 1) (vals
                                          (reduce #(if (% (set %2)) (assoc % (set %2) (conj (% (set %2)) %2))
                                                                    (assoc % (set %2) (conj #{} %2))) {} x))))))

(defcheck solution-5aff128e
  (fn [words]
    (set
      (filter #(< 1 (count %))
        (map
          (fn [word]
            (set
              (filter #(= (group-by identity word) (group-by identity %)) words)))
          words)))))

(defcheck solution-5b2abb8f
  (fn [words]
    (loop [l words m {}]
      (if (empty? l) (set (filter #(<= 2 (count %)) (vals m)))
                     (let [f (first l) sf (sort f)]
                       (recur (rest l)
                         (assoc m sf
                                  (conj (get m sf #{}) f))))))))

(defcheck solution-5b4a6525
  (fn [c] (set (filter #(> (count %) 1) (map set (vals (group-by #(apply str (sort %)) c)))))))

(defcheck solution-5b87bfd2
  (fn [s]
    (set
      (for [[k v] (group-by sort s)
            :when (> (count v) 1)]
        (set v)
        )
      )
    ))

(defcheck solution-5ba1bd02
  (fn [words]
    (let [letters (into {} (for [w words] [w (sort w)]))]
      (into #{}
        (filter #(> (count %) 1)
          (map
            #(set (map first (val %)))
            (group-by #(val %) letters)))))))

(defcheck solution-5bad41a3
  (fn [src]
    (set(map set (filter #(> (count %) 1)(vals(group-by sort src)))))
    ))

(defcheck solution-5bf6521e
  (fn [coll]
    (set (vals (filter
                 (fn [[k v]] (if (> (count v) 1) true false))
                 (reduce #(if
                           (%1 (sort %2))
                            (assoc %1 (sort %2) (conj (%1 (sort %2)) %2))
                            (assoc %1 (sort %2) #{%2}))
                   (hash-map) coll))))))

(defcheck solution-5c28294f
  (fn [s] (set (keep #(if (next %) (set %)) (vals (group-by sort s))))))

(defcheck solution-5c6e339
  (fn [coll] (set (map set (filter #(< 1 (count %)) (vals (group-by sort coll)))))))

(defcheck solution-5c88b69b
  (fn [words]
    (set (map #(set %)
           (filter #(< 1 (count %))
             (vals (group-by #(apply str (sort %)) words)))))))

(defcheck solution-5c94b8bc
  (fn [strs]
    (->> strs
      (map #(vector % (frequencies %)))
      (group-by second)
      (vals)
      (filter #(> (count %) 1))
      (map (fn [c] (reduce #(conj %1 (first %2)) #{} c)))
      (into #{})
      )))

(defcheck solution-5cdf95dd
  (fn [coll]
    (set
      (filter #(> (count %) 1)
        (map (fn [group] (set (map second group)))
          (partition-by first
            (sort-by #((comp str first) %)
              (map
                (fn [s] (list (apply str (sort s)) s))
                coll))))))))

(defcheck solution-5d0dd153
  (fn [words]
    (set (filter #(> (count %) 1) (map (comp set val) (group-by sort words))))))

(defcheck solution-5d17b2a
  (fn [words] (set (filter #(< 1 (count %)) (set (map (comp set val) (group-by frequencies words)))))))

(defcheck solution-5d667418
  (fn anagrams [words]
    (->> words
      (group-by frequencies)
      vals
      (filter (comp (partial < 1) count))
      (map set)
      set)))

(defcheck solution-5dbd6a45
  (fn f [words]
    (->> words
      (reduce
        (fn [s word]
          (let [letters (sort word)]
            (update-in s [letters] #(conj (or % #{}) word))))
        {})
      (vals)
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-5dc8bd09
  (fn [s] (->> s (group-by sort) vals (map set) (filter #(< 1 (count %))) set)))

(defcheck solution-5de5f9f
  (fn [s]
    (set (map set
           (filter #(> (count %) 1)
             (vals
               (group-by set s)))))))

(defcheck solution-5dff5f0c
  (fn [coll]
    (->> coll
      (group-by set)
      (filter #(> (count (second %)) 1))
      (map #(let [[k v] %] (set v)))
      set)))

(defcheck solution-5e2dae6f
  (fn my-anagram-finder [word-list]
    (letfn [(contains-same? [coll1 coll2]
              (cond
                (empty? coll1)
                (empty? coll2)
                (= (first coll1) (first coll2))
                (recur (rest coll1) (rest coll2))
                :else false))
            (anagram? [word1 word2] (contains-same? (sort (into [] word1)) (sort (into [] word2))))]
      (into #{}
        (filter
          #(> (count %) 1)
          (map (fn [word]
                 (reduce (fn [rs new-word]
                           (if (anagram? new-word word)
                             (conj rs new-word)
                             rs))
                   #{}
                   word-list))
            word-list))))))

(defcheck solution-5ef6b8a1
  (fn [coll]
    (set
      (remove
        #(< (count %) 2)
        (reduce (fn [rv word]
                  (conj
                    rv
                    (set (filter (fn [s]
                                   (= (sort word) (sort s)))
                           coll))))
          #{}
          coll)))))

(defcheck solution-5f27d71a
  (fn [words]
    (loop [result #{} elements words]
      (if elements
        (recur
          (conj result
            (loop
             [internal-result #{(first elements)} internal-elements words]
              #_(prn (first internal-elements) " - " (first elements) " : " internal-result)
              (if internal-elements
                (recur
                  (if (= (set (re-seq #"[a-z]" (first internal-elements))) (set (re-seq #"[a-z]" (first elements))))
                    (conj internal-result (first internal-elements))
                    internal-result
                    )
                  (next internal-elements)
                  )
                (if (> (count internal-result) 1)
                  internal-result
                  nil
                  )
                )
              )
            )
          (next elements))
        (loop [end-result #{} end-elements result]
          (if end-elements
            (recur
              (if (nil? (first end-elements))
                end-result
                (conj end-result (first end-elements)))
              (next end-elements)
              )
            end-result
            )
          )
        )
      )
    ))

(defcheck solution-5fd32f2
  #(set (for [v (vals (group-by sort %))
              :when (> (count v) 1)]
          (set v))))

(defcheck solution-60318809
  (fn [xs]
    (into #{}
      (filter #(< 1 (count % ))
        (for [[k v] (group-by sort xs)]
          (set v))))))

(defcheck solution-6074dee6
  (fn anagram-finder [www]

    ((comp
      set
      (partial map #(set %))
      (partial filter #(> (count %) 1))
      vals
      (partial reduce
        (fn [a w]
          (let [k (set w)
                v (a k)]
            (if (nil? v)
              (assoc a k [w])
              (assoc a k (conj v w))))) {})

      ) www)))

(defcheck solution-60909b80
  (fn [c] (set (map set
                 (remove #(= 1 (count %))
                   (vals (group-by sort c)))))))

(defcheck solution-60a72ded
  (fn [words]
    (let [anagram? (fn [w1 w2]
                     (and
                      (clojure.set/subset? (set (seq w1))
                        (set (seq w2)))
                      (clojure.set/subset? (set (seq w2))
                        (set (seq w1)))))
          anagram-list (for [x words]
                         (set (filter #(anagram? x %) words)))]
      (into #{}
        (filter #(> (count %) 1) anagram-list)))))

(defcheck solution-60a82642
  (fn [s]
    (apply hash-set
      (filter #(> (count %) 1)
        (map #(apply hash-set (second %))
          (group-by
            (fn [a] (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} a))
            s))))))

(defcheck solution-60dc91c3
  (fn [words]
    (->> (map list (map sort words) words)
      (group-by first)
      vals
      (map (partial map second))
      (map set)
      (remove #(= 1 (count %)))
      set)))

(defcheck solution-60eb9fc6
  (fn [alist]
    (->>
      (for [i alist]
        (set (filter #(= (frequencies (seq %)) (frequencies (seq i))) alist)))
      (filter #(< 1 (count %)))
      set)))

(defcheck solution-61210ecc
  (fn [v]
    (set (map set
           (filter #(>= (count %) 2)
             (vals (group-by
                     sort
                     v)))))))

(defcheck solution-61602eb2
  (fn group-anagrams [v]
    (let [anagram? (fn [s1 s2] (= (sort (seq s1)) (sort (seq s2))))]
      (loop [v_ v acc #{}]
        (cond
          (empty? v_) acc
          :else (let [anagrams (filter #(anagram? (first v_) %) v_)]
                  (if (< 1 (count anagrams))
                    (recur (filter #(not (anagram? (first v_) %)) v_)
                      (conj acc  (set anagrams)))
                    (recur (rest v_) acc))))))))

(defcheck solution-6231b8ef
  (fn [words]
    (->> (group-by sort words)
      (map val)
      (remove #(> 2 (count %)))
      (map set)
      (into #{}))))

(defcheck solution-6243106b
  (fn [coll]
    (->>
      (map #(hash-map (set %) #{%}) coll)
      (reduce #(merge-with clojure.set/union %1 %2) {})
      vals
      set
      (clojure.set/select #(> (count %) 1)))))

(defcheck solution-62c6f121
  (fn [s] (into #{} (map (fn [e] (into #{} (map first (e 1))))
                      (filter (fn [[k v]] (> (count v) 1))
                        (group-by second (map (fn [x] [x (sort x)]) s)))))))

(defcheck solution-62c94be9
  (fn [words]
    (set (map set
           (filter #(< 1 (count %))
             (vals (group-by #(sort %) words)))))))

(defcheck solution-632f2b8
  (fn[x]( set (map set (filter #(> (count %)1) (vals(group-by set x)))))))

(defcheck solution-6409a984
  (fn [s]
    (set (filter #(< 1 (count %))
           (reduce (fn [anagrams word]
                     (let [bucket
                           (first (filter (fn [b]
                                            (= (sort word) (sort (first b)))) anagrams))]
                       (if (nil? bucket)
                         (conj anagrams #{word})
                         (conj (disj anagrams bucket)
                           (conj bucket word))))) #{} s)))))

(defcheck solution-647af96e
  (fn anagram-finder [words]
    (set (filter #(> (count %1) 1)
           (map
             #(set (map first %))
             (vals
               (group-by (fn [[a b]] b)
                 (map #(list %1 (sort %1)) words)))
             )))))

(defcheck solution-64aef841
  (fn anagram-finder2 [words]
    (let [words-ordered (map #(apply str (sort %)) words)
          indexes (take (count words) (range))
          woi (map vector words-ordered indexes)
          woi2 (sort-by #(first %) woi)
          woi3 (partition-by #(first %) woi2)
          woi4 (filter #(> (count %) 1) woi3)
          ei (map (fn [s] (map #(second %) s)) woi4)
          words2 (set (map (fn [s] (set (map #(words %) s))) ei))]
      words2)))

(defcheck solution-65401744
  (fn anagram-finder [coll]
    (into #{}
      (map #(into #{} %)
        (filter #(> (count %) 1)
          (vals (group-by #(sort (map str %)) coll)))))))

(defcheck solution-65ec844b
  (fn [words]
    (reduce
      (fn [acc word1]
        (let [res (reduce
                    (fn [acc word2]
                      (if (and (= (count word1) (count word2))
                               (= (into #{} word1) (into #{} word2)))
                        (conj acc word2)
                        acc))
                    #{} words)]
          (if (> (count res) 1)
            (conj acc res)
            acc)))
      #{} words)))

(defcheck solution-665de8a2
  (fn [s] (let [x #(set (clojure.string/split % #"")) y #(set (filter (fn [a] (= (x a) (x %))) s))] (reduce  #(if (> (count (y %2)) 1) (conj % (y %2)) %) #{} s))))

(defcheck solution-6675f5a6
  (fn anagrams
    [strs]
    (loop [[h & t] strs
           sorted-to-words {}]
      (let [sorted-h (sort h)]
        (let [new-sorted-to-words (if (contains? sorted-to-words sorted-h)
                                    (assoc sorted-to-words sorted-h
                                                           (conj (sorted-to-words sorted-h) h))
                                    (assoc sorted-to-words sorted-h [h]))]
          (if (nil? t)
            (into #{} (filter #(> (count %) 1)
                        (map (fn [[x y]] (into #{} y)) new-sorted-to-words)))
            (recur t new-sorted-to-words)))))))

(defcheck solution-66be8559
  (fn [xs]
    (->> (group-by #(set %) xs)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-670a0eae
  (fn [s]
    (->> s
      (group-by set)
      vals
      (map set)
      (filter #(not= 1 (count %)))
      set)))

(defcheck solution-673ded73
  (fn [ws]
    (set (map set (remove #(= 1 (count %)) (vals (group-by #(apply str (sort (seq %))) ws)))))
    ))

(defcheck solution-67e73d70
  #(into #{}
     (map (comp set second)
       (filter (fn [[k v]] (not= 1 (count v)))
         (group-by sort %)))))

(defcheck solution-67fda4fe
  (fn [coll] (set (map set (filter #(> (count %) 1) (vals (group-by sort coll)))))))

(defcheck solution-686875f7
  (fn [w] (set (filter #(not-empty (rest %)) (map (fn [[f s]] (set s)) (group-by set w))))))

(defcheck solution-6899fc09
  #(reduce
     (fn [s [k [f & r :as v]]]
       (if r (conj s (set v)) s))
     #{}
     (group-by sort %)))

(defcheck solution-68ec5e53
  (fn [coll]
    (into #{}
      (map set (filter #(> (count %) 1) (vals
                                          (reduce
                                            #(let [[k v] %2]
                                               (assoc %1 k (cons v (%1 k []))))
                                            {}
                                            (map #(vector (set (frequencies %)) %) coll))))))))

(defcheck solution-691cf0c1
  (fn [xs] (set (for [[s ss] (group-by #(set (seq %)) xs)
                      :when (> (count ss) 1)]
                  (set ss)))))

(defcheck solution-69220484
  (fn [coll]
    (->>(group-by sort coll)
      (map (comp set second))
      (filter #(> (count %) 1))
      set)))

(defcheck solution-692739dc
  (fn [s] (into #{} (remove #(= (count %) 1) (map #(into #{} (second %)) (group-by sort s))))))

(defcheck solution-6a099d4d
  (fn [lst]
    (into #{}
      (filter #(> (count %) 1)
        (map set (vals (group-by sort lst)))))))

(defcheck solution-6a1d025
  (fn anagrams [coll]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals
            (reduce
              (fn [coll word]
                (let [k (set word) value (get coll k [])]
                  (assoc coll k (conj value word))
                  )
                )
              {} coll)
            )
          )
        )
      )
    ))

(defcheck solution-6a79faf6
  (fn anagrams [xs]
    (letfn [(anagram? [a b]
              (let [sa (set a)
                    sb (set b)]
                (= sa sb (clojure.set/intersection (set a) (set b)))))]
      (set
        (filter
          #(< 1 (count %))
          (set (map
                 (fn [s] (set (filter #(anagram? s %) xs)))
                 xs)))))))

(defcheck solution-6a9595fd
  (fn find-anagarams [v]
    (set (filter #(> (count %) 1)
           (for [x v]
             (set (filter #(= (frequencies x) (frequencies %)) v)))))))

(defcheck solution-6b67b404
  (fn anagrams [words]
    (->> words
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-6bdb07
  (fn [s]
    (set (map set
           (filter #(> (count %) 1)
             (vals (apply merge-with into (map #(hash-map (set %) [%]) s))))))))

(defcheck solution-6bdc618
  (fn [words]
    (->>
      words
      (group-by #(set %) ,)
      (vals ,)
      (map set ,)
      (filter #(> (count %) 1) ,)
      (set ,))))

(defcheck solution-6c1de436
  (fn [words]

    (->> words (group-by sort) vals (filter #(> (count %) 1)) (map set) set)

    ))

(defcheck solution-6c31577d
  (fn ag
    ([ws & rs]
     (if (seq ws)
       (let [ann (into #{} (filter #(= (frequencies (first ws)) (frequencies %)) ws))
             rst (remove ann ws)]
         (apply ag (cons rst (cons ann rs))))
       (into #{} (filter #(> (count %) 1 )rs))))))

(defcheck solution-6c950b78
  (fn anagram-group
    [coll]
    (set (filter #(> (count %) 1) (map set (vals (group-by frequencies coll)))))))

(defcheck solution-6c992ba9
  (fn [words]
    (->> words (group-by sort) (vals) (map set) (filter #(> (count %) 1)) (set))))

(defcheck solution-6cb496c7
  (fn [ws] (set (remove #(< (count %) 2) (map set (vals (reduce #(update-in % [(sort %2)]
                                                                   (fn [o] (if (nil? o) [%2] (conj o %2)))) {} ws)))))))

(defcheck solution-6cd31bd0
  (letfn [
          (w-id [w] (sort (seq w)))
          (w-to-dict [m w]
            (let [k (w-id w)]
              (assoc m k
                       (conj (get m k #{}) w))))
          (dict-ws [ws]
            (reduce
              (fn [m w] (w-to-dict m w))
              {} ws))]
    (fn [words]
      (set
        (remove
          (comp empty? rest) (vals (dict-ws words)))))))

(defcheck solution-6d06315c
  (fn [s]
    (loop [tmps s ans {}]
      (if (empty? tmps)
        (loop [a ans newa #{}]
          (if (empty? a)
            newa
            (if (> (count (second (first a))) 1)
              (recur (rest a) (conj newa (second (first a))))
              (recur (rest a) newa))))
        (if (contains? ans (set (first tmps)))
          (recur (rest tmps) (assoc ans (set (first tmps)) (conj (get ans (set (first tmps))) (first tmps))))
          (recur (rest tmps) (assoc ans (set (first tmps)) (conj #{} (first tmps)))))))))

(defcheck solution-6d402ebe
  (fn [words] (->> (group-by sort words)
                vals
                (filter #(< 1 (count %)))
                (map set)
                set)))

(defcheck solution-6da80d90
  #(->> % (group-by sort) vals (remove (comp #{1} count)) (map set) set))

(defcheck solution-6e3be8fe
  (fn[coll]
    (letfn [(anagram?[s1 s2]
              (if (not= (count s1) (count s2))
                false
                (every? identity (map = (sort (seq s1)) (sort (seq s2))))))]
      (set (remove #(= 1 (count %)) (map (fn[elt] (reduce (fn[result item]
                                                            (if (anagram? (first result) item)
                                                              (conj result item)
                                                              result))
                                                    #{elt} coll)) coll))))))

(defcheck solution-6e89959e
  (fn test [coll]
    (letfn [(compString [x]
              (fn [y] (if (= (apply sorted-set (seq x)) (apply sorted-set (seq y)))
                        true
                        false)
                )
              )]
      (loop [coll coll result '()]
        (let [cur (first coll)]
          (cond (empty? coll) (into #{} (filter #(> (count %) 1) result))
                :else (recur (remove (compString cur) coll)
                        (cons (set (filter (compString cur) coll)) result))
                )
          )
        )
      )
    ))

(defcheck solution-6eb529ac
  (fn [wordlist]
    (letfn [(digit-count [s]
              (let [distinct-letters (set s)]
                (apply merge (map (fn [c] (hash-map c (count (filter #(= c %) s)))) distinct-letters))))]
      (into #{}
        (map second
          (filter #(> (count (second %)) 1)
            (apply merge-with clojure.set/union
              (map (fn [x] (hash-map (digit-count x) (set [x])))
                wordlist))))))))

(defcheck solution-6eb73e99
  (fn [-seq]
    (into #{}  (->> (map #(vector % (group-by identity %)) -seq)
                 (group-by #(identity (last %)) )
                 (map last)
                 (map #(map first %))
                 (filter #(< 1  (count %) ))
                 (map (partial into #{}))
                 )
      )
    ))

(defcheck solution-6ee0810d
  (fn [s]
    (->> (group-by set s)
      (vals)
      (filter #(>= (count %) 2))
      (map set)
      (set))))

(defcheck solution-6eecd76d
  (fn [coll]
    (->> coll
      (group-by sort)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-6efda986
  (fn [a] (set (filter second (map #(set (mapcat vals %))(map val (group-by keys (map #(hash-map (group-by identity %) %) a))))))))

(defcheck solution-6f0561d9
  (fn [s]
    (set
      (map
        set
        (filter
          #(< 1 (count %))
          (vals (group-by sort s))
          )
        )
      )
    ))

(defcheck solution-6f431e3
  (fn [words]
    (set
      (reduce-kv (fn [r _ w]
                   (if (> (count w) 1)
                     (conj r (set w))
                     r))
        []
        (group-by #(sort (seq %)) words)))))

(defcheck solution-6f5dcf3f
  (fn [s] (set (filter #(> (count %) 1) (map set (vals (group-by sort s)))))))

(defcheck solution-6f6af197
  (fn [words]
    (->> words
      (group-by sort)
      vals
      (filter #(< 1 (count %)))
      (map set)
      (into #{}))))

(defcheck solution-6f6f1813
  (fn [words]
    (->> words
      (group-by sort)
      vals
      (map set)
      (filter #(< 1 (count %)))
      set)))

(defcheck solution-6fa04bac
  (fn anf [w]
    (loop [anags #{} words w]
      (let [{match true remain false} (group-by #(= (set (first words)) (set %)) words)
            new_anags (if (> (count match) 1) (conj anags (set (map #(apply str %) match))) anags)]
        (if (empty? remain)
          new_anags
          (recur new_anags remain)
          )))))

(defcheck solution-6fea264d
  (fn [v]
    (set
      (map #(set %)
        (filter #(> (count %) 1)
          (vals (group-by #(sort %) v)))))))

(defcheck solution-70089625
  (fn [words]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by sort words)))))))

(defcheck solution-702dcd3
  (fn [words]
    (->> (group-by (comp (partial apply str) sort) words)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-705c6f48
  (fn [s]
    (into #{} (filter #(> (count %) 1)
                (map set (vals (group-by sort s)))))))

(defcheck solution-708687f6
  #(set (map (fn [i] (set i))
          (filter (fn [re] (< 1 (count re))) (vals (group-by
                                                     (fn [e]
                                                       (str (sort (seq e))))
                                                     %))))))

(defcheck solution-70c316db
  #(set (map set (filter (fn [e] (> (count e) 1)) (vals (group-by sort %))))))

(defcheck solution-70cc2de0
  (fn [s]
    (->> s
      (group-by frequencies)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-7125e0d0
  (fn [col] (set (map set (remove #(= (count %) 1) (vals (group-by sort col)))))))

(defcheck solution-713b06f0
  (fn anagrams [coll]
    (letfn [(permutation? [s1 s2]
              (let [n (count s2)]
                (cond
                  (and (empty? s1) (empty? s2))
                  true
                  (some #{(first s1)} s2)
                  (recur (rest s1)
                    (rest (take n (drop-while #(not= (first s1) %) (cycle s2)))))
                  :else false)))
            (find-anagrams [coll]
              (for [s coll]
                (set (filter #(permutation? % s) coll))))]
      (into #{} (filter #(> (count %) 1) (find-anagrams coll))))))

(defcheck solution-7168d141
  (fn [v]
    (set (filter #(> (count %) 1)
           (map set (vals (group-by #(clojure.string/join (sort (clojure.string/split % #""))) v)))))))

(defcheck solution-717855ca
  (fn [words]
    (set (map set (filter #(> (count %) 1) (vals (group-by frequencies words)))))
    ))

(defcheck solution-718035fb
  (fn [l]
    (set
      (map set
        (vals
          (filter #(< 1 (count (second %)))
            (reduce #(let [s (sort %2)] (assoc % s (cons %2 (% s #{})))) {} l)))))))

(defcheck solution-7180483b
  (fn [x]
    (let [items (group-by #(sort (re-seq #"\w" %)) x)]
      (set (map #(set (last %)) (filter #(> (count (last %)) 1) items))))))

(defcheck solution-71b7905d
  #(set
     (map (comp set val)
       (remove (comp #{1} count val)
         (group-by frequencies %)))))

(defcheck solution-71c0390d
  (fn [coll]
    (->> coll
      (group-by sort)
      (filter (fn [[k v]] (< 1 (count v))))
      vals
      (map set)
      set)))

(defcheck solution-72485fb8
  (fn [c]
    (set
      (filter #(> (count %) 1)
        (for [w c]
          (set (filter #(= (set w) (set %)) c))
          )
        )
      )
    ))

(defcheck solution-726e4b98
  (fn [l] (set (map set (filter #(> (count %) 1) (map second (group-by sort l)))))))

(defcheck solution-72bba951
  (fn [v] (set (map set (remove #(= (count %) 1) (vals (group-by sort v)))))))

(defcheck solution-72cc52c9
  #(->> (group-by frequencies %)
     (map second)
     (filter next)
     (map set)
     set))

(defcheck solution-72fb4243
  #(set
     (filter
       (fn [x] (> (count x) 1))
       (map
         (comp set val)
         (group-by sort %)))))

(defcheck solution-73142f26
  (fn [xs] (set (keep #(when (> (count %) 1) (set %)) (vals (group-by sort xs))))))

(defcheck solution-731e0c78
  (fn [words]
    (let [anagram-set
          (loop [anagrams {}
                 all-words words]
            (let [current-word (first all-words)
                  remaining-words (rest all-words)]
              (if (empty? all-words)
                anagrams
                (recur (merge-with into anagrams
                         {(sort current-word) #{current-word}})
                  remaining-words))))]
      (set (filter #(> (count %) 1) (vals anagram-set))))))

(defcheck solution-732d3f72
  (fn [c] (set (map set (filter (fn [g] (> (count g) 1)) (vals (group-by #(set %) c)))))))

(defcheck solution-736d7eee
  (fn diff [col]
    (set (map
           #(set %)
           (filter
             #(not (empty? (rest %)) )
             (map
               #(val %)
               (group-by
                 #(apply str (sort (seq %)) )
                 col)
               )
             )
           )
      )
    ))

(defcheck solution-736d8ece
  #(set (for [w (vals (group-by sort %))
              :when (< 1 (count w))]
          (set w))))

(defcheck solution-738da547
  (fn [words]
    (set (filter #(>= (count %) 2)
           (map (comp set val)
             (group-by sort words))))))

(defcheck solution-73d9b7de
  (fn [v]
    (into #{}
      (map #(into #{} %)
        (vals
          (filter #(> (count (last %)) 1)
            (group-by #(sort %) v )))))))

(defcheck solution-741816c9
  (fn af [x]
    (loop [l x
           result {}]
      (if (empty? l)
        (set (map #(set %) (filter #(> (count %) 1) (vals result))))
        (recur (rest l)
          (conj result [(sort (first l))
                        (cons (first l)
                          (get result (sort (first l)) '()))]))))))

(defcheck solution-744aad50
  (fn [words]
    (set (for [[_ ws] (group-by sort words) :when (> (count ws) 1)]
           (set ws)))))

(defcheck solution-74670ae8
  ;;#(map set (vals (group-by set %)))
  #(set (remove (fn [x] (= 1 (count x))) (map set (vals (group-by set %))))))

(defcheck solution-74b1903f
  (fn [l1] (letfn [(helper [acc l] (if (empty? l) acc (helper (conj acc (into #{} (filter #(= (set (first l)) (set %)) l)))
                                                        (filter #(not (= (set (first l)) (set %))) l))))]
             (into #{} (filter #(> (count %) 1) (helper #{} l1))))))

(defcheck solution-74bcf68b
  (fn [coll] (set (map #(set %) (vals (filter #(> (count (val %)) 1) (group-by #(apply str (sort %)) coll)))))))

(defcheck solution-74da05f6
  (fn [words]
    (loop [acc #{} [w & ws :as coll] words]
      (if (empty? coll) acc
                        (let [s (set w)
                              a-grams (into #{} (for [i ws :when (= s (set i))] i))
                              left-overs (filter #(not (contains? a-grams %)) ws)]
                          (if (empty? a-grams)
                            (recur acc left-overs)
                            (recur (conj acc (conj a-grams w)) left-overs)))))))

(defcheck solution-751f3196
  (fn [s] (set (filter #(< 1 (count %)) (map set (vals (group-by sort s)))))))

(defcheck solution-76cfb00e
  (fn[l](set (filter #(> (count %) 1) (map #(set (second %)) (group-by sort l))))))

(defcheck solution-77a69f6b
  #(set (map set (remove (comp (partial = 1) count) (vals (group-by sort %))))))

(defcheck solution-77bf3aa0
  (fn anagrams [coll]
    (->> (group-by frequencies coll)
      (map #(set (val %)))
      (filter #(> (count %) 1))
      set)))

(defcheck solution-77f54033
  (fn [words]
    (loop [[w & ws :as words] words, acc #{}]
      (if (nil? w)
        acc
        (let [anagram? #(= (sort %1) (sort %2))
              groups (group-by #(anagram? % w) words)
              anagrams (set (groups true))
              remaining (groups false)
              newacc (if (> (count anagrams) 1) (conj acc anagrams) acc)]
          (recur remaining newacc))))))

(defcheck solution-77f67379
  (fn [s]
    (set (map set
           (filter (comp #(> % 1) count) (vals (group-by (comp sort (partial re-seq #"."))
                                                 s)))))))

(defcheck solution-78114d40
  (fn [lst]
    (into #{} (filter #(> (count %) 1) (for [x lst]
                                         (into #{}
                                           (for [y lst]
                                             (if (= (sort x) (sort y))
                                               y
                                               x
                                               )
                                             )
                                           )
                                         )))
    ))

(defcheck solution-782886c8
  (fn [z]
    (reduce (fn [a s]
              (#(if (next %) (conj a (set %)) a)
               (re-seq (re-pattern (str "\\b[" s "]{" (count s) "}\\b"))
                 (apply str (interpose " " z)))))
      #{}
      z)))

(defcheck solution-783151dc
  (fn anagramize [words]
    (set (map set (remove #(= 1 (count %)) (vals (group-by sort words)))))))

(defcheck solution-788ed2a6
  (fn [coll]
    (into #{} (map set (filter #(>= (count %) 2) (vals (group-by frequencies coll)))))))

(defcheck solution-78c2fef1
  (fn anagram-finder [coll]
    (let [angarams (map (fn [x]
                          [(sort x) x]) coll)
          values (vals (group-by #(first %) angarams))]
      (into #{} (filter #(< 1 (count %)) (map (fn [group]
                                                (into #{} (map second group))) values)))
      )))

(defcheck solution-79cfbc0b
  (fn [a]
    (set (filter
           #(> (count %) 1)
           (map set (vals (group-by #(apply str (sort %)) a)))))))

(defcheck solution-7ac2cb99
  (fn anagrams [xs] (set (map set (filter #(> (count %) 1) (vals (group-by #(sort %) xs)))))))

(defcheck solution-7b14ea55
  (fn [words]
    (letfn [
            (letters [word] (set (seq word)))]
      (set
        (map set
          (filter
            #(> (count %) 1)
            (vals (group-by letters words))))))))

(defcheck solution-7b153adf
  (fn [words]
    (set (filter #(> (count %) 1) (map set (vals (group-by #(identity (sort (seq %))) words)))))))

(defcheck solution-7c8b3636
  (fn anagrams [coll]
    (into #{}
      (filter
        #(> (count %) 1)
        (vals
          (reduce
            (fn [anagram-map word]
              (let [sorted-chars (sort word)]
                (assoc anagram-map sorted-chars
                                   (conj (get anagram-map sorted-chars #{}) word))))
            {}
            coll))))))

(defcheck solution-7cc624b1
  (fn [words]
    (letfn
     [
      (is-anagram [w1 w2]
        (=
          (sort w1)
          (sort w2)
          )
        )
      ]
      (set
        (filter
          (fn [ws]
            (> (count ws) 1)
            )
          (map
            (fn [w1]
              (set
                (filter
                  (fn [w2]
                    (is-anagram w1 w2)
                    )
                  words
                  )
                )
              )
            words
            )
          )
        )
      )
    ))

(defcheck solution-7ce8e4d7
  (fn [words]
    (set (filter #(> (count %) 1) (map set (vals (apply merge-with concat (map #(assoc {} (sort %) [%]) words))))))))

(defcheck solution-7cfcbc6f
  (fn anagram [x] (disj (set (map #(if (= (count %) 1) nil (into #{} %))
                               (vals (reduce #(assoc %1 (frequencies %2)
                                                        (conj  (%1 (frequencies %2))  %2)) {} x)))) nil)))

(defcheck solution-7d3df38f
  #(->> % (group-by sort) vals (map set) (filter (fn [s] (> (count s) 1))) set))

(defcheck solution-7d4d1ccb
  (fn [c]
    (->> (group-by (fn [s]
                     (group-by identity s))
           c)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-7dd22f3b
  (fn [v]
    (set (map set (vals (into {} (remove #(= 1 (count (val  %))) (group-by set v))))))))

(defcheck solution-7dd64f1a
  (fn anagrams [s]
    (set (map set (vals (filter #(> (count (second %)) 1)
                          (group-by set s)))))))

(defcheck solution-7e027fe8
  (fn [v]
    (->> (group-by sort v)
      vals
      (remove #(= (count %) 1))
      (map set)
      set)))

(defcheck solution-7e125930
  (fn[s] (set (filter #(> (count %) 1)
                (map (fn[[_ v]] (set v)) (group-by #(sort %) s))))))

(defcheck solution-7e7fb07c
  (fn [coll]
    (->> coll
      (map #(vector % (set %)))
      (group-by last)
      (vals)
      (map (fn [coll] (map first coll)))
      (map set)
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-7eb27810
  (fn [n]
    (set
      (map set
        (filter #(> (count %) 1 ) (vals (group-by #(sort(seq %)) n)))))))

(defcheck solution-7ecf4571
  (fn [s]
    (->> s
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      set
      )))

(defcheck solution-7ee18a97
  (fn [l] (set (map set (filter #(> (count %) 1) (vals (group-by sort l)))))))

(defcheck solution-80c27412
  (fn [coll]
    (->> (sort-by #(apply str (sort %)) coll)
      (partition-by #(apply str (sort %)))
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-80e12a3
  (fn anagrams [strs]
    (set
      (filter (fn [x] ( > (count x) 1))
        (for [i strs]
          (set (filter  #(= (frequencies i) (frequencies %)) strs)))))))

(defcheck solution-81159485
  (fn [i] (into #{} (map set (filter #(< 1 (count  %)) (vals (group-by sort i)))))))

(defcheck solution-8192536c
  (comp
   set
   (partial map set)
   (partial filter #(> (count %) 1))
   vals
   (partial group-by (comp set (partial map identity)))))

(defcheck solution-81ad8784
  #(->> %
     (group-by sort)
     vals
     (filter second)
     (map set)
     set))

(defcheck solution-81bd7eaa
  (fn [x]
    (into #{} (map #(into #{} (val %)) (filter #(< 1 (count (val %))) (group-by #(sort %) x))))
    ))

(defcheck solution-823932ed
  (fn [xs]
    (let [smap (reduce #(merge-with clojure.set/union %1
                          (hash-map (sort %2) #{%2}) ) {} xs ) ]
      (set (filter #(> (count %) 1) (vals smap)))
      )

    ))

(defcheck solution-82b7aab
  (fn ana [wv]
    (->>
      wv
      (group-by set)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set)
      )))

(defcheck solution-82b7ec2e
  (fn [ws] (set (map set (filter #(< 1 (count %)) (vals (group-by set ws)))))))

(defcheck solution-82fa5e3e
  (fn ana [words]
    (->> (group-by set words)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set)
      )))

(defcheck solution-8313b52a
  (fn [xs]
    (let [canonify (fn [s] (clojure.string/join (rest (sort (clojure.string/split s #"")))))
          reducer (fn [m w] (let [canon (canonify w)] (assoc m canon (conj (get m canon #{}) w))))]
      (set (filter #(> (count %) 1) (vals (reduce reducer {} xs)))))))

(defcheck solution-8424d0b5
  (fn [xs]
    (set (map #(set %) (filter
                         #(<= 2 (count %))
                         (vals (group-by #(sort %) xs)))))))

(defcheck solution-843dd248
  #(->> %
     (group-by sort) vals (filter next) (map set) set))

(defcheck solution-84b9828a
  (fn [words] (set (map set (filter #(> (count %) 1) (vals (group-by set words)))))))

(defcheck solution-85488e2e
  (fn [coll]
    (->> (group-by frequencies coll)
      vals
      (filter #(next %))
      (map set)
      set)))

(defcheck solution-858be300
  (fn [words]
    (->> words
      (group-by set)
      vals
      (remove (comp empty? rest))
      (map set)
      set)))

(defcheck solution-85ac1ac
  (fn ana [l]
    (let [isana (fn [a b]
                  (and (= (count a) (count b))
                       (= (count (set a))
                         (count (clojure.set/union (set a) (set b))))
                       (not= a b)))
          getanas (fn [rw wl]
                    (for [x wl
                          :when (isana rw x)]
                      x))
          answer #{}]
      (into #{} (clojure.set/union (into #{} (filter #(> (count %) 1)
                                               (doall (for [m l] (set (conj (getanas m l) m)))))))))))

(defcheck solution-8615acef
  (fn [x]
    (set (map set (for [f (set (map frequencies x))
                        :let [s (filter #(= f (frequencies %)) x)]
                        :when (> (count s) 1)]
                    s)))))

(defcheck solution-8616cd92
  (fn [l]
    (apply hash-set
      (filter
        (fn [x] (> (count x) 1))
        (vals
          (reduce
            (fn [m e]
              (update-in m [(frequencies e)] (fnil #(conj % e) #{})))
            {}
            l))))))

(defcheck solution-864973b
  (fn anag [coll]
    (loop [c coll r {}]
      (let [v (first c) k (sort (list* v))]
        (if (nil? c)
          (set (map #(set (second %)) (filter #(< 1 (count (second %))) r)))
          (if (r k)
            (recur (next c) (update-in r [k] conj v))
            (recur (next c) (assoc r k [v]))))))))

(defcheck solution-864c2cc4
  (fn [s] (->> s (group-by set) vals (filter #(>= (count %) 2)) (map set) set)))

(defcheck solution-86960cd4
  (fn [ss]
    (set (map set (for [[k v] (group-by #(sort %) ss) :when (< 1 (count v))] v)))))

(defcheck solution-86a70228
  #(set
     (filter %
       (map (comp set val)
         (group-by frequencies %2)))) #(next %))

(defcheck solution-86ec18e6
  (fn anagram-finder
    [coll]
    (->> coll
      (group-by #(vector (count %) (set %)))
      (filter #(> (count (last %)) 1))
      (map #(set (last %)))
      (into #{}))))

(defcheck solution-874ff87
  (fn [w]
    (->> w
      (map sort)
      (interleave (range))
      (partition 2)
      (group-by last)
      (filter #(< 1 (count (last %))))
      (map #(map first (last %)))
      (map (fn [i] (set (map #(get w %) i))))
      (set)
      )
    ))

(defcheck solution-875487ea
  #(set (filter (fn [x] (> (count x) 1)) (for [x %] (let [c (into #{} x)] (reduce (fn [s m] (if (= (into #{} m) c) (conj s m) s)) #{} %))))))

(defcheck solution-87b19a8f
  (fn [c]
    (->> (group-by sort c)
      (map (comp set second))
      (filter (comp not nil? second)) ;; so I don't have to count
      set)
    ))

(defcheck solution-87eb653d
  (fn [x ] set (set (map set (filter #(< 1 (count %)) (map val (group-by sort  x)))))))

(defcheck solution-880c5388
  #(set (for [[k v] (group-by set %) :when (next v)] (set v))))

(defcheck solution-894bb5aa
  (fn
    [s]
    (set
      (filter
        #(> (count %) 1)
        (map set
          (for [x s]
            (for [y s
                  :when (= (sort x) (sort y))]
              y)))
        ))))

(defcheck solution-89869952
  #(set (for [[_ g] (group-by sort %) :when (next g)] (set g))))

(defcheck solution-89a3859a
  (fn[s](set (keep #(if (> (count %) 1) (set %)) (vals (group-by sort s))))))

(defcheck solution-89cfd08d
  (fn [coll]
    (->> coll
      (group-by frequencies)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-8a46f849
  (fn [x]  (->> x
             (group-by sort)
             vals
             (filter #(> (count %) 1))
             (map set)
             set)))

(defcheck solution-8a8e900c
  (fn [coll]
    (->> coll
      (group-by sort)
      (map second)
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-8a8ec565
  (fn [t]
    (let [xs (filter #(> (count (second %)) 1)
               (group-by second (map (fn [x] [x (sort (seq x))]) t)))]
      (into #{}
        (for [lst (map second xs)]
          (into #{} (map first lst)))))))

(defcheck solution-8aed54c6
  (fn anagrams [lst]
    (let [sort-str #(apply str (sort %))
          anagram? #(= (sort-str %1) (sort-str %2))
          count-if-anagram #(let [k (get %2 (sort-str %1))]
                              (if (nil? k)
                                (assoc %2 (sort-str %1) (set (list %1)))
                                (assoc %2 (sort-str %1) (conj k %1))))
          list-anagrams #(reduce
                           (fn [a [k v]] (if (> (count v) 1) (conj a v) a))
                           #{}
                           %)]
      (loop [lst lst
             acc {}]
        (if (empty? lst)
          (list-anagrams acc)
          (recur (rest lst) (count-if-anagram (first lst) acc)))))))

(defcheck solution-8b09b588
  (fn [words]
    (let [grouped (group-by #(sort %) words)
          values (vals grouped)
          non-single (filter #(< 1 (count %)) values)]
      (set (map set non-single)))))

(defcheck solution-8b687d0e
  (fn ana [s]
    (set (keep #(if (> (count %) 1) % nil) (vals (reduce (fn [memo y]
                                                           (let [ana-list (memo (sort (.split y "")))]
                                                             (assoc memo (sort (.split y ""))
                                                                         (conj
                                                                           (if ana-list ana-list #{})
                                                                           y))
                                                             )
                                                           ) {} s))))

    ))

(defcheck solution-8ba47809
  (fn [col] (set (filter #(> (count %) 1)
                   (map set (vals (reduce (fn [l r] (update-in l [(frequencies r)] conj r)) {} col)))))))

(defcheck solution-8cd4b536
  (fn [c]
    (->> (group-by frequencies c)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-8ce36c3e
  (fn [coll]
    (loop [acc {} [h & r] coll]
      (if h
        (recur (assoc acc (sort h) (conj (acc (sort h) #{}) h)) r)
        (into #{} (filter #(> (count %) 1) (vals acc)))))))

(defcheck solution-8d2b2913
  (fn [coll]
    (set
      (filter #(> (count %) 1)
        (vals
          (apply merge-with
            (comp set concat)
            (map
              (fn [val]
                (let [letter-set (sort (apply list val))]
                  (hash-map letter-set (hash-set val))))
              coll)))))))

(defcheck solution-8d5a0a02
  (fn [l]
    (apply hash-set
      (map (partial apply hash-set)
        (filter #(> (count %) 1)
          (vals
            (apply merge-with concat
              (map (fn [w] { (apply str (sort w)) [w]})
                l))))))))

(defcheck solution-8d809a8c
  (fn [coll]
    (letfn [(perms [word] (if (> 2 (count word))
                            [word]
                            (for [i (range 0 (count word))
                                  [x [y & z]] [(split-at i word)]
                                  sub (perms (concat x z))]
                              (apply str (cons y sub)))))]
      (set
        (keep (fn [[k [x & r :as v]]] (when r (set v)))
          (group-by (comp set perms) coll))))))

(defcheck solution-8e90c148
  (fn anagrams [words]
    (->> (group-by set words)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-8e9595bd
  (fn [x]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals
            (reduce #(assoc %
                       (sort %2)
                       (conj (get %
                               (sort %2)
                               [])
                         %2))
              {}
              x)))))))

(defcheck solution-8ee4517b
  (fn [d]
    (->> (group-by #(sort %) d)
      vals
      (filter #(<= 2 (count %)))
      (map set)
      (into #{}))))

(defcheck solution-8ef0deaf
  (fn [ss] (set (filter #(< 1 (count %)) (map (comp set second) (group-by set ss))))))

(defcheck solution-8f5d50bc
  (fn [words]
    (letfn [(smaller-ana [s]
              (apply str (sort s)))]
      (let [ana-map (group-by smaller-ana words)
            all-anas (set (map set (vals ana-map)))]
        (set (filter #(> (count %) 1) all-anas))))))

(defcheck solution-8fdafd70
  (fn [ss]
    (->> ss
      (group-by #(group-by identity %))
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-9005f014
  #(into #{} (for [s (vals (group-by sort %)) :when (> (count s) 1)] (set s))))

(defcheck solution-905ab77b
  (fn [col]
    (apply hash-set (map #(apply hash-set (second %)) (filter (fn [k-v]
                                                                (let [k (first k-v)
                                                                      v (second k-v)]
                                                                  (> (count v) 1)))
                                                        (group-by #(sort %)
                                                          col))))))

(defcheck solution-90a0cb75
  (fn [coll]
    (letfn [(fao [x [h & t]]
              (when h
                (if (= (frequencies x) (frequencies h))
                  (cons h (fao x t))
                  (fao x t))))]
      (into #{} (filter #(> (count %) 1)
                  (for [x coll]
                    (into #{} (fao x coll))))))))

(defcheck solution-90a8379b
  (fn [m] (set (for [[_ v] (group-by #(sort %) m)
                     :when (> (count v) 1)]
                 (set v)))))

(defcheck solution-911a9b6a
  (fn [s](->> s
           (group-by sort) vals
           (filter #(< 1 (count %)))
           (map set) set
           )))

(defcheck solution-916d86c8
  (fn [l]
    (->> l
      (group-by set)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-91ac8d4f
  (fn [coll]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort coll)))))))

(defcheck solution-91d4e838
  (fn anag [s]
    (set
      (filter #(> (count %) 1)
        (vals (reduce #(merge-with clojure.set/union  %1 %2) {} (map #(hash-map (sort %) (hash-set %)) s)))))))

(defcheck solution-928762fb
  #(->> % (group-by set) vals (filter next) (map set) set))

(defcheck solution-9320b246
  (fn [c] (->> c
            (group-by frequencies)
            vals
            (map set)
            (filter #(> (count %) 1))
            set)))

(defcheck solution-939ac954
  (fn [words]
    (->> words
      (map #(into [] (list %, (group-by identity (seq %)))))
      (into {})
      (group-by val)
      (vals)
      (reduce #(cons (map (fn [p] (first p)) %2) %1) #{})
      (filter #(> (count %) 1))
      (map set)
      (into #{})
      )
    ))

(defcheck solution-93b65a7d
  (fn anagram-finder [words]
    (if (empty? words)
      #{}
      (let [anagram? (fn [x y]
                       (= (sort x) (sort y)))
            [word & words] words
            anagrams (->> words
                       (filter (partial anagram? word))
                       set)]
        (into (if (zero? (count anagrams))
                #{}
                #{(conj anagrams word)})
          (->> words
            (filter (complement (partial anagram? word)))
            anagram-finder))))))

(defcheck solution-941bb91c
  (fn [s]
    (into #{}
      (for [x
            (filter #(> (count %) 1)
              (partition-by first
                (sort-by first
                  (for [x s]
                    [(apply str (sort (into [] x))) x]
                    )
                  )
                )
              )]
        (into #{}
          (for [y x] (last y))
          )
        )
      )
    ))

(defcheck solution-944c5b55
  (fn [xs]
    (set (map set
           (filter #(> (count %) 1)
             (map val (group-by (comp str sort) xs)))))))

(defcheck solution-94f21d3
  (fn [v]
    (letfn [(str-constructs [s]
              (let [chars (group-by char s)
                    char-set (set (keys chars))
                    counts (map count (set (vals chars)))]
                (zipmap char-set counts)))
            (anagram-group [xs]
              (->> xs
                (group-by str-constructs)
                vals
                (filter #(>= (count %) 2))))]
      ; &#26368;&#24460; set &#12392;&#12375;&#12390;&#36820;&#12377;&#24517;&#35201;&#12364;&#33391;&#12367;&#20998;&#12363;&#12425;&#12394;&#12356;&#12364;
      (set (map set (anagram-group v))))))

(defcheck solution-9559c834
  (let [take1 (fn [p]
                (map (fn [[pre post]]
                       [(first post) (concat pre (rest post))])
                  (map (fn [i] (split-at i p)) (range (count p)))))
        perm (fn perm [p]
               (if (empty? p)
                 '(())
                 (reduce #(lazy-cat %1 %2) '()
                   (map (fn [[h q]]
                          (map (partial cons h) (perm q)))
                     (take1 p)))))
        str-perm (fn [s] (set (map (fn [cls] (apply str cls)) (perm s))))
        potential-anagrams (fn [s]
                             (reduce
                               (fn [r st]
                                 (if (some #(% st) r)
                                   r
                                   (conj r (str-perm st))))
                               #{}
                               s))]
    (fn [s] (let [s (set s)] ((comp set filter)
                              (comp not (partial = 1) count)
                              (map
                                (fn [pas]
                                  (clojure.set/intersection pas s))
                                (potential-anagrams s)))))))

(defcheck solution-957633cc
  (fn [x] (set (map set (filter #(> (count %) 1) (vals (group-by sort x)))))))

(defcheck solution-95b9a69d
  (fn [words]
    (->> words
      (group-by sort)
      (map last)
      (filter #(< 1 (count %)) )
      (map set)
      (set)
      )
    ))

(defcheck solution-9614274f
  #(into #{}
     (for [[_ x]
           (apply merge-with into
             (for [s %] {(apply sorted-set s) #{s}}))
           :when (< 1 (count x))]
       x)))

(defcheck solution-965e3617
  (fn [a] (set (filter #(> (count %) 1) (map set (vals (group-by (partial sort-by identity) a)))))))

(defcheck solution-96a2b7a
  #(->> %
     (group-by sort)
     vals
     (map set)
     (remove (fn [s] (->> s count dec zero?)))
     set))

(defcheck solution-96c16fbd
  #(into #{} (map set (filter (fn[c] (< 1 (count c))) (vals (group-by frequencies %))))))

(defcheck solution-96de8627
  (fn anagram-finder [l]
    (letfn [(count-chars [w]
              (reduce (fn [b a] (if (get b a) (assoc b a (inc (get b a))) (assoc b a 1))) {} w))]
      (set (filter (fn [x] (> (count x) 1)) (map set (vals (reduce #(merge-with concat %1 %2) (map (fn [x] (hash-map (count-chars x) [x])) l)))))))))

(defcheck solution-96df8b71
  (fn [coll]
    (set (filter #(> (count %) 1)
           (map set
             (vals (group-by sort coll)))))))

(defcheck solution-9713b45e
  (fn anagram [coll]
    (let [anagram-map (group-by sort coll)
          anagram-set (set (map set (filter #(> (count %) 1) (vals anagram-map))))]
      anagram-set)))

(defcheck solution-97a52a51
  (fn eka
    [xs]
    (->> xs
      (group-by #(sort (re-seq #"[a-z]" %)))
      (vals)
      (filter #(> (count %) 1))
      (map #(into #{} %))
      (into #{})
      )
    ))

(defcheck solution-9899bd5b
  (fn [ss]
    (->>
      (map (fn [s] {(apply str (sort s)) s}) ss)
      (apply merge-with (comp flatten vector))
      (filter  (fn [[k v]] (and (coll? v) (> (count v) 1))))
      (map (fn [[k v]] (apply hash-set v)))
      (apply hash-set))))

(defcheck solution-98d91f44
  (fn [coll]
    (set (for
          [[k vals]
           (group-by sort coll)
           :when (>= (count vals) 2) ] (set vals)))))

(defcheck solution-998a306
  #(into #{} (map (partial into #{})
               (filter (fn [x] (< 1 (count x))) (vals (group-by (partial into #{}) %))))))

(defcheck solution-99a79b24
  (fn
    [ws]
    (->> ws
      (group-by set)
      vals
      (remove #(= 1 (count %)))
      (map set)
      set)))

(defcheck solution-9a1df14b
  (fn find-anagram [s]
    (->> s
      (map #(hash-map (sort (seq %)) #{%}) ,,)
      (apply merge-with clojure.set/union ,,)
      (vals )
      (filter #(> (count %) 1))
      (reduce #(conj %1 %2) #{} ))))

(defcheck solution-9a8d0d10
  (fn [xs]
    (set (map set (filter #(> (count %) 1) (vals (group-by set xs)))))))

(defcheck solution-9ad3e7aa
  (fn anagrams [lst]
    (into #{} (filter #(> (count %) 1)
                (vals (reduce #(assoc %1 (sort %2) (conj (get %1 (sort %2) #{}) %2)) {} lst))))))

(defcheck solution-9b4795e3
  #(->> (group-by sort %)
     (vals)
     (map set)
     (filter (comp seq rest))
     (set)))

(defcheck solution-9b896f3c
  (fn h [v]
    (let [permutation (fn _ [p, s]
                        (let [c (count s)]
                          (if (= '() s) p
                                        (conj (list p)
                                          (for [n (range c)]
                                            (_ (str p (nth s n)) (str (subs s 0 n) (subs s (inc n)))))))))]
      (into #{}
        (map #(into #{} %1)
          (filter #(> (count %1) 1)
            (vals (apply merge-with concat
                    (apply concat
                      (for [s v]
                        (map #(hash-map %1 (list s))
                          (filter #(= (count s) (count %1))
                            (flatten (permutation "" s))) )))))))))))

(defcheck solution-9c8accfd
  #(into #{} (filter (fn [s] (> (count s) 1)) (vals
                                                (reduce (fn [m [k v]]
                                                          (assoc m k (conj (m k #{}) v))) {} (map (fn [x] [(apply str (sort x)) x]) %))))))

(defcheck solution-9d505bb8
  (fn [x] (set (map set (filter (fn [gto] (> (count gto) 1)) (vals (group-by (fn [y] (sort (seq y))) x)))))  ))

(defcheck solution-9dc04dd8
  (fn [coll]
    (letfn [(ps [str]
              (-> str (.split "") (sort) (clojure.string/join)))]
      (reduce #(conj %1 (set %2)) #{}
        (filter #(< 1 (count %)) (partition-by ps (sort-by ps coll)))))))

(defcheck solution-9e4d629b
  (fn [words]
    (set (filter #(< 1 (count %)) (map set((fn make-sets [v]
                                             (if (nil? v) '()
                                                          (let [is-anagram #(= (sort %) (sort %2))
                                                                g (group-by #(is-anagram (first v) %) v)
                                                                anas (get g true)
                                                                notanas (get g false)]
                                                            (cons anas (make-sets notanas))))) words))))))

(defcheck solution-9e596fbb
  (fn anagram [c]
    (set (map set (filter (fn [e] (> (count e) 1)) (vals (group-by set c)))))))

(defcheck solution-9ea2b1ba
  #(set (filter (fn [s] (> (count s) 1)) (map (comp set second) (group-by set %)))))

(defcheck solution-9ec50113
  (fn [words]
    (letfn [(rotate
              [coll]
              (conj (subvec coll 1) (first coll)))
            (rotations
              [coll]
              (take (count coll) (iterate rotate coll)))
            (rotate-and-join
              [coll idx]
              (let [[l r] (split-at idx coll)]
                (map #(concat % r) (rotations (vec l)))))
            (permutations
              [coll]
              (reduce
                (fn [res idx]
                  (mapcat rotate-and-join res (repeat idx)))
                [coll]
                (range (count coll) 1 -1)))]
      (loop [words (set words)
             result #{}]
        (let [word  (first words)
              chs   (seq word)
              perms (into #{} (map (partial apply str) (permutations chs)))]
          (if (empty? words)
            (clojure.set/select #(> (count %) 1) result)
            (recur
              (clojure.set/difference words perms #{word})
              (conj result (clojure.set/intersection perms words)))))))))

(defcheck solution-9f18b5a8
  (fn [words]
    (->> words
      (group-by set)
      (map (fn [[k v]] (set v)))
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-9f43f3be
  (fn finder [xs]
    (->> (group-by sort xs)
      vals
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-9f8e5ddb
  (fn anatell [words]
    (set (filter #(>= (count %) 2)
           (set (for [w (range (count words))]
                  (set (filter #(not (nil? %))
                         (#(for [m (range (count %))]
                             (some #{(nth % m)} %2))
                          words
                          ((fn anagram[word]
                             (let [letter (map #(str %) word)]
                               (map #(apply str %) (filter #(= (count %) (count word)) (distinct (filter #(= (frequencies %) (frequencies letter))
                                                                                                   (apply (fn combi [s1 s2 & args]
                                                                                                            (let [fcp #(for [e1 % e2 %2] [e1 e2]) cp1 (fcp s1 s2)]
                                                                                                              (if (empty? args)
                                                                                                                cp1
                                                                                                                (loop [i 0 res cp1]
                                                                                                                  (if (= i (count args))
                                                                                                                    res
                                                                                                                    (recur (inc i) (map #(apply conj %) (fcp res (nth args i)))))))))
                                                                                                     (take (count word) (cycle [letter])))))))))
                           (nth words w)))))))))))

(defcheck solution-9f984024
  (fn anagrams [coll]
    (let [char-sets (distinct (map set coll))
          anagrammable? (fn [s word] (= s (set word)))
          filters (map #(partial anagrammable? %) char-sets)
          anagrams (map #(filter % coll) filters)
          multigrams (filter #(> (count %) 1) anagrams)]
      (set (map set multigrams)))))

(defcheck solution-9fad4ba7
  (fn [s]
    (set
      (filter
        #(> (count %) 1)
        (map set
          (vals
            (group-by #(sort (vec %)) s)))))))

(defcheck solution-a0374ea9
  (fn [vek]
    (let [vg (loop [v vek h []]
               (if (= v [])
                 h
                 (let [a (sort (first v))
                       vb (filter #(= a (sort %)) v)]
                   (recur (vec (clojure.set/difference (set v) (set vb)))
                     (conj h vb)))))]
      (set (filter (fn [x] (> (count x) 1)) (map #(set %) vg))))))

(defcheck solution-a06022b6
  (fn [coll]
    (->> (for [x coll]
           {(into #{} x) #{x}})
      (apply merge-with into)
      (filter #(next (val %)))
      (map second)
      set)))

(defcheck solution-a07321f7
  (comp set
        (partial filter #(> (count %) 1))
        vals
        (partial reduce #(assoc %1 (sort %2) (conj (%1 (sort %2) #{}) %2)) {})))

(defcheck solution-a09cd7c4
  (fn [words]
    (->>
      (group-by #(sort %) words)
      (map second)
      (filter #(<= 2 (count %)))
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-a0a014d
  (fn anagram[coll] (let [
                          toseq (fn[s] (sort(seq s)))
                          seqs (map toseq coll)
                          counted (reduce (fn[acc el] (update-in acc [el] (fnil inc 0))) {} seqs)
                          filtered (filter #(> (second %) 1) counted)
                          tocount (fn[s] (< 1 (get counted (toseq s) 0)))
                          matchitem (fn[seq coll] (set(filter #(= (toseq %) seq) coll)))
                          ]
                      (set(map (fn[el] (matchitem el coll) ) (keys filtered)))
                      )
    ))

(defcheck solution-a0ff4c6
  (fn [l]
    (->> l
      (group-by sort)
      (map val)
      (filter #(> (count %) 1))
      (map set)
      (into #{}))))

(defcheck solution-a11a97f2
  (fn [coll]
    (into #{}
      (map
        (fn [[k v]] (set v))
        (filter (fn [[k v]] (> (count v) 1)) (#(group-by (fn [s] (apply str (sort (vec s)))) %) coll))))))

(defcheck solution-a1560ce5
  (fn [words]
    (->> words
      (group-by frequencies)
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-a15cf758
  (fn [xs]
    (->> (group-by #(sort %) xs) (vals) (filter #(> (count %) 1)) (map set) (set))))

(defcheck solution-a1ae82e3
  (fn anagram [words]
    (->> words
      (map #(vector (vec (sort %)) %))
      sort
      (partition-by first)
      (filter #(< 1 (count %)))
      (map #(set (map second %)))
      set)))

(defcheck solution-a1fd5c3f
  (fn [s] (into #{} (filter #(> (count %) 1) (map #(into #{} %) (vals (group-by #(apply sorted-set %) s)))))))

(defcheck solution-a20ca185
  (fn [v]
    (->>
      (group-by #(sort %) v)
      (vals)
      (filter #(< 1 (count %)))
      (map set)
      (set)
      )))

(defcheck solution-a2149db4
  (fn z [coll]
    (let [keyF (fn [key] (frequencies (seq key)))
          f (fn [m k] (update-in m [(keyF k)] (fnil conj #{}) k))]
      (->> (reduce f {} coll)
        (vals)
        (filter #(< 1 (count %)))
        (into #{})))))

(defcheck solution-a22efc5b
  (fn anagram-finder [s]
    (let [m (group-by #(sort (seq %)) s)
          groups (vals m)
          sets (map set groups)
          multi-sets (filter #(> (count %) 1) sets)]
      (set multi-sets))))

(defcheck solution-a232ca90
  (letfn [(canonicalize-string [string]
            (clojure.string/join ""
              (sort (clojure.string/split string #""))))]
    (fn [strings]
      (set (map set (filter #(> (count %) 1)
                      (vals (group-by canonicalize-string strings))))))))

(defcheck solution-a33f1a6a
  (fn anagram-group-by [ws]
    (letfn [(counts [w] (apply merge-with + (map (fn [c] {c 1}) w)))]
      (->> ws
        (group-by counts)
        (map (comp set second))
        (filter #(> (count %) 1))
        (set)))))

(defcheck solution-a3908ea0
  (fn get-anagrams [words]
    (->> words
      (map seq)
      (map frequencies)
      (distinct)
      (map (fn list-anagrams [word-freq] (filter #(= (frequencies %) word-freq) words)))
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-a3df3c3e
  (fn anagram-finder--- [coll]
    (let [f-p (fn [s] (apply str (sort s)))
          m-s (fn [a b] (let [s (if (set? a) a #{a})]
                          (clojure.set/union s #{b})))]
      (->> (reduce #(merge-with m-s %1 {(f-p %2) %2}) {} coll)
        (vals)
        (filter set?)
        (set)))))

(defcheck solution-a411ef72
  (fn [coll]
    (let [alphs (group-by #(set %) coll)
          f (filter #(> (count %) 1) (vals alphs))]
      (set (map set f)))))

(defcheck solution-a42b1b5f
  #(->> (reduce
          (fn [dict x]
            (let [key   (sort x)
                  value (conj (dict key #{}) x)]
              (into dict {key value})))
          {} %)
     (vals)
     (filter (fn [s] (> (count s) 1)))
     (set)))

(defcheck solution-a42b36e7
  #(set (filter second (map set (vals (group-by sort %))))))

(defcheck solution-a442faa0
  (fn [coll]
    (set (map set
           (filter #(> (count %) 1)
             (vals (group-by set coll)))))))

(defcheck solution-a45b4ea6
  (fn anagrams [words]
    (set (for [group (vals (group-by sort words))
               :when (> (count group) 1)]
           (set group)))))

(defcheck solution-a4944cdc
  (fn[s](
          set(map set(map (fn[y](map first y)) (map second (filter (fn[x](> (count (second x)) 1))
                                                             (group-by second (zipmap s (map (fn[w](set(seq w))) s)))))))
          )))

(defcheck solution-a4ab5030
  (fn [x]
    (->> x
      (group-by set)
      (vals)
      (remove #(= 1 (count %)))
      (map set)
      set)))

(defcheck solution-a4bacfa8
  #(set (filter (fn [x] (> (count x) 1))
          (map set (vals (group-by sort %))))))

(defcheck solution-a51b13ec
  #(->> %
     (group-by set)
     (map (comp set second))
     (filter (comp (partial < 1) count))
     (set)))

(defcheck solution-a569bcfc
  (fn [l]
    (letfn [(find-anagrams [ll]
              (loop [w (sort (first ll))
                     r (rest ll)
                     a []
                     d []]
                (if (empty? r) [(if (empty? a) a (conj a (first ll))) d]
                               (let [nw (sort (first r))]
                                 (if (= w nw)
                                   (recur w (rest r) (conj a (first r)) d)
                                   (recur w (rest r) a (conj d (first r))))))))]
      (loop [ll l
             res #{}]
        (let [[a d] (find-anagrams ll)]
          (if (or (empty? d) (= (count d) 1))
            (if (empty? a) (set res) (conj res (set a)))
            (recur d (conj res (set a)))))))))

(defcheck solution-a577d1cd
  (fn [coll]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals
            (group-by sort coll)))))))

(defcheck solution-a5a5f61d
  (fn Anagram
    [x]
    (let [ is (fn [a] (= (sort (apply list a)) (sort (apply list (first x)))))
          chosen (into #{} (filter is x))
          rst (filter #(not (is %)) x)]
      (if (empty? x) #{}
                     (if (= (count chosen) 1) (Anagram rst)
                                              (conj (Anagram rst) chosen))))))

(defcheck solution-a5beaf05
  (fn [c] (->> c
            (group-by sort)
            (map second)
            (remove #(= 1 (count %)))
            (map (partial into #{}))
            (into #{}))))

(defcheck solution-a5ca06c5
  (fn anagrams [words] (let [anagram (fn [w1 w2] (= (frequencies w1) (frequencies w2)))]
                         (disj (reduce (fn [anagset s] (conj anagset (reduce #(if (anagram s %2) (conj %1 s %2) %1)
                                                                       #{}
                                                                       (remove #(= % s) words)) ))
                                 #{}
                                 words) #{}) )))

(defcheck solution-a62e5f97
  (fn [v] (->> v (group-by sort) (vals) (filter  #(> (count %) 1)) (map set) (set))))

(defcheck solution-a64e6b5b
  (fn [v]
    (letfn [ (canonise [s] (apply str (sort (seq s))))
            (insert [m word] (let [s (canonise word)
                                   cur-set (m s) ]
                               (if cur-set
                                 (assoc m s (conj cur-set word))
                                 (assoc m s #{word} ))))
            ]
      (set (remove #(= 1 (count %)) (vals (reduce #(insert %1 %2) {} v) )))
      )))

(defcheck solution-a6c3db98
  (fn [words]
    (set
      (map set
        (filter
          #(> (count %) 1)
          (vals (group-by sort words)))))))

(defcheck solution-a73a2979
  (fn [x]
    (set (map set (filter #(> (count %) 1) (vals (group-by frequencies x)))))))

(defcheck solution-a7813e48
  (fn [xs]
    (into #{} (filter #(< 1 (count %))
                (map #(set %) (vals (group-by #(sort %) xs)))))
    ))

(defcheck solution-a7a6ee2b
  (fn anagram [xs]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort xs)))))
    ))

(defcheck solution-a7f00af2
  (fn [words]
    (into #{}
      (filter
        #(> (count %) 1)
        (for [word words]
          (into #{}
            (filter
              #(= (sort %) (sort word))
              words)))))))

(defcheck solution-a81dd4e1
  (fn anagram-finder [coll]
    (set
      (filter #(not= 1 (count %))
        (map (comp set second) (group-by #(sort (group-by identity %)) coll))))))

(defcheck solution-a83eeb1a
  #(set (->> (vals (group-by set %))
          (filter (comp (partial < 1) count))
          (map set))))

(defcheck solution-a876f47a
  (fn[s]
    (into #{}
      (filter #(> (count %) 1)
        (map
          (fn[x](into #{} (filter #(= (sort %) (sort x )) s)))
          s)))))

(defcheck solution-a8998afb
  (fn [xs] (let [anagrams (vals (group-by (fn [a] (sort (seq a))) xs))
                 ]
             (into #{} (map #(into #{} %) (filter #(< 1 (count %)) anagrams))))))

(defcheck solution-a8dfd193
  (fn [words]
    (->>
      words
      (map #(hash-map (sort %) #{%}))
      (apply merge-with clojure.set/union)
      (map (fn [[k v]]
             (if (> (count v) 1)
               [v]
               [])))
      (apply concat)
      set)))

(defcheck solution-a97c0dd7
  (fn [ss]
    (->> (group-by sort ss)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-a9844586
  (fn [words]
    (set (filter #(> (count %) 1) (map #(set (second %)) (group-by sort words))))))

(defcheck solution-a99bd8e0
  #(->> % (group-by
            (fn [x] (sort (seq x))))
     (vals)
     (filter (fn [x] (> (count x) 1)))
     (map set)
     (into #{})))

(defcheck solution-a9b5633b
  #(set (filter second (map (comp set second) (group-by sort %)))))

(defcheck solution-a9ba3233
  (fn [w]
    (->> w
      (group-by frequencies)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-a9c3525b
  (fn [words]
    (->> (group-by sort words)
      vals
      (filter second)
      (map set)
      set)))

(defcheck solution-a9d03260
  (fn [xs]
    (->> xs
      (group-by set)
      (vals)
      (map set)
      (filter (fn [x] (> (count x) 1)))
      (set))))

(defcheck solution-aa14f96e
  (fn [c]
    (->> c
      (reduce (fn [m x] (update-in m [(sort x)] #(set (conj % x)))) {})
      vals
      (filter #(> (count %) 1))
      set)))

(defcheck solution-aa3d837b
  (fn [col]
    (->> col
      (group-by (comp sort vec))
      vals
      (map set)
      (filter #(< 1 (count %)))
      set)))

(defcheck solution-aa6c8724
  (fn problem-77 [words]
    (let [eq? (fn [word] (frequencies (set word)))]
      (set
        (filter
          #(> (count %) 1)
          (map set (vals (group-by eq? words))))))))

(defcheck solution-aa713349
  (fn [words]
    (->>
      words
      (map #(vector (sort (seq %)) %))
      (reduce
        (fn [ana-map [sorted word]]
          (if (contains? ana-map sorted)
            (assoc ana-map sorted (conj (ana-map sorted) word))
            (assoc ana-map sorted #{word})
            )
          ) {}
        )
      (vals)
      (filter #(> (count %) 1))
      (into #{} )
      )
    ))

(defcheck solution-aa8db104
  (fn [s]
    (set
      (map set
        (filter second (vals (group-by #(apply sorted-set %) s)))))))

(defcheck solution-aa956286
  (fn anagram-finder [words]
    (->> words
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-ac048ad9
  (fn [s] (into #{} (->> s
                      (map (fn [a] [a (frequencies a)]))
                      (group-by second)
                      (vals)
                      (filter #(< 1 (count %)))
                      (map #(map first %))
                      (map #(into #{} %))
                      ))))

(defcheck solution-ac17272e
  (fn [word-list]
    (let [anagram? (fn [x y] (= (sort x) (sort y)))]
      (loop [ws word-list, results []]
        (if (empty? ws) (set (filter #(> (count %) 1) results))
                        (recur
                          (rest ws)
                          (conj results (set (filter #(anagram? (first ws) %) word-list)))))))))

(defcheck solution-ac5b2eb4
  (fn [xs]
    (->> xs (group-by sort) vals (filter #(> (count %) 1)) (map set) set)))

(defcheck solution-ac6343eb
  (fn aa [v]
    (set (filter #(> (count %) 1)

           (loop [x v y #{}]
             (if (empty? x) y
                            (recur

                              (filter #(not= (group-by identity %) (group-by identity (first x))) x)
                              (conj y (set (filter #(= (group-by identity %) (group-by identity (first x)))
                                             x)))
                              )
                            )
             )  ))))

(defcheck solution-aca42fe
  (fn anagram-finder
    [xs]
    (set (filter #(> (count %) 1)
           (map set (vals (group-by (fn [s]
                                      (sort (seq s)))
                            xs)))))))

(defcheck solution-ad1e37b0
  (fn [words] (set (filter #(> (count %1) 1)
                     (map #(into #{} %1)
                       (vals (group-by #(set (seq %1)) words)))))))

(defcheck solution-ad2f9e03
  (fn [s]
    (set (filter #(> (count %) 1)
           (map set (vals (group-by sort s)))))))

(defcheck solution-aded7505
  #(set (for [[_ g] (group-by frequencies %)
              :when (next g)]
          (set g))))

(defcheck solution-ae3009af
  (fn [words]
    (let [keyify #(->> % seq sort (apply str))
          reducer (fn [m w] (assoc m (keyify w) (or (and (m (keyify w)) (conj (m (keyify w)) w)) #{w})))
          filterer #(> (count %) 1)]
      (->> (reduce reducer {} words)
        vals
        (filter filterer)
        (into #{})))))

(defcheck solution-ae483922
  (fn [s]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by set s)))))))

(defcheck solution-ae928ff4
  (fn [v]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals (group-by sort v)))))))

(defcheck solution-aeae06f3
  #(set
     (map set
       (remove (fn [c] (= 1 (count c)))
         (vals (group-by frequencies %))))))

(defcheck solution-aed57e6a
  (fn anagram-finder
    ([coll]
     (anagram-finder coll #{}))
    ([[fst & rst :as coll] acc]
     (letfn ([anagram? [s] (= (sort fst) (sort s))])
       (if (empty? coll) acc
                         (anagram-finder
                           (remove anagram? rst)
                           (if (empty? (filter anagram? rst))
                             acc
                             (conj acc (set (conj (filter anagram? rst) fst))))))))))

(defcheck solution-aee294f7
  (fn [s]
    (->> (group-by set s)
      vals
      (filter next)
      (map set)
      set)))

(defcheck solution-af4c8c27
  (fn [coll] (set (filter #(> (count %) 1) (map set (vals (group-by frequencies coll)))))))

(defcheck solution-af692c07
  (fn [words]
    (disj
      (into #{} (for [[k v]
                      (apply merge-with concat
                        (map #(apply hash-map %) (partition-all 2
                                                   (interleave (map #(apply str (sort (seq %))) words)
                                                     (map vector words)))))]
                  (let [s (apply hash-set v)]
                    (if (>= (count s) 2) s)))) nil)))

(defcheck solution-af697d4e
  (fn [ws] (set (for [i (vals (group-by sort ws)) :when (< 1 (count i))] (set i)))))

(defcheck solution-af77296f
  (fn [xs]
    (into #{}
      (map #(into #{} %)
        (filter #(> (count %) 1)
          (vals (group-by #(into #{} (map char %))
                  xs)))))))

(defcheck solution-afdb978d
  (fn
    [s]
    (set (map set (filter #(> (count %) 1) (vals (group-by #(set %) s)))))))

(defcheck solution-aff74dd1
  (fn [words]
    (let [lst (map (fn [w]
                     [w (sort (map identity w))])
                words)]
      (reduce (fn [r l]
                (conj r (set (map #(% 0) l))))
        #{}
        (filter #(> (count %) 1)
          (map (fn [elt]
                 (filter #(= (% 1) (elt 1)) lst))
            lst))))))

(defcheck solution-b078746f
  (fn [cs]
    (let [norm (group-by sort cs)]
      (into #{} (filter #(< 1 (count %)) (map #(into #{} %) (vals norm))))
      )
    ))

(defcheck solution-b0c9b2fa
  (fn[c] (set (filter #(> (count %) 1) (map set (vals (group-by #(set %) c)))))))

(defcheck solution-b1562e21
  (fn [x]
    (set (filter #(< 1 (count %)) (map set (vals (group-by sort x)))))))

(defcheck solution-b1eaa49c
  (fn [x]
    (set (keep #(if (next %) (set %)) (vals (group-by sort  x))))))

(defcheck solution-b1f4d33
  (fn [w]
    (->> w
      (group-by frequencies)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-b1f64fc7
  (fn anagram [coll]
    (loop [ret #{} coll coll]
      (if (empty? coll) ret
                        (let [m (zipmap coll (map set coll))
                              x (first m)
                              y (rest m)
                              matched (set (map first (filter #(= (second x) (second %)) y)))
                              coll (seq (clojure.set/difference (set (keys y)) matched))]
                          (recur (if (empty? matched) ret
                                                      (conj ret (conj matched (first x))))
                            coll))))))

(defcheck solution-b2346134
  (fn [s]
    (set (map (comp set second)
           (filter #(> (count (second %)) 1)
             (group-by set s))))))

(defcheck solution-b234e496
  (fn [words]
    (letfn [(is-anagram? [a b]
              (and (= (count a) (count b))
                   (= (frequencies a) (frequencies b))))
            (anagrams [a]
              (set (get (group-by #(is-anagram? a %) words) true)))]
      (set (filter #(> (count %) 1) (set (map anagrams words)))))))

(defcheck solution-b24b9d26
  (fn
    [l]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals (group-by sort l)))))))

(defcheck solution-b25ab616
  (fn [x] (set (map set (filter #(< 1 (count %))(vals (group-by #(set %) x)))))))

(defcheck solution-b263c76d
  (fn [words]
    (set (filter #(> (count %) 1)
           (map (fn [word] (set (filter #(= (sort word) (sort %)) words)))
             words)))))

(defcheck solution-b27a7eb5
  (fn [v]
    (set
      (map
        (fn [m] (set (m 1)))
        (filter
          (fn [e] (> (count (e 1)) 1))
          (group-by #(sort %) v))))))

(defcheck solution-b2acf7db
  #(->> (group-by set %) vals (filter (comp (partial < 1) count)) (map set) set))

(defcheck solution-b49f5ac7
  (fn [words]
    (let [fingerprints (map (comp sort #(map identity %)) words)
          word+fingerprints (map #(list %1 %2) words fingerprints)
          grouped (reduce
                    (fn [memo w]
                      (let [word (first w)
                            fingerprint (last w)]
                        (assoc memo fingerprint (conj (memo fingerprint #{}) word))))
                    {}
                    word+fingerprints
                    )]
      (set (filter #(> (count %) 1) (vals grouped))))))

(defcheck solution-b4a5a986
  (fn [s] (set (map set (filter #(> (count %) 1) (vals (group-by sort s)))))))

(defcheck solution-b4cf0aee
  (fn [words]
    (->> (group-by #(sort %) words)
      (vals)
      (filter #(< 1 (count %)))
      (map #(into #{} %))
      (into #{}))))

(defcheck solution-b4d139f8
  #(into #{} (map set (vals
                        (filter
                          (fn [[_ words]] (< 1 (count words)))
                          (group-by sort %))))))

(defcheck solution-b53edda5
  (fn [words]
    (reduce
      (fn [res word]
        (let [matches (filter #(= (frequencies %) (frequencies word)) words)]
          (if (> 2 (count matches))
            res
            (conj res (into #{} matches)))))
      #{}
      words)))

(defcheck solution-b55f01f5
  (fn anagram-finder
    [xs]
    (into #{}
      (map (partial into #{})
        (vals (filter #(< 1 (count (val %))) (group-by sort xs)))))))

(defcheck solution-b5bd62ca
  (fn [coll]
    (set
      (map set
        (filter #(>= (count %) 2)
          (vals
            (group-by sort coll)))))))

(defcheck solution-b5d8f85c
  (fn[xs]
    (set (filter #(< 1 (count %))
           (map set (vals (group-by (comp sort seq) xs)))))))

(defcheck solution-b6e677bd
  #(->> %
     (group-by (fn [x] (sort (seq x))))
     vals
     (filter (fn [x] (> (count x) 1)))
     (map set)
     set))

(defcheck solution-b7364c79
  (fn [x]
    (->> x
      (group-by sort)
      (map #(set (% 1)))
      (filter #(> (count %) 1))
      set)))

(defcheck solution-b7aa84ed
  (fn [words]
    (set (map set (filter #(< 1 (count %))
                    (vals (group-by sort words)))))))

(defcheck solution-b7c83834
  (fn [s]
    (let [xs (filter #(> (count %) 1) (vals (group-by sort s)))]
      (into #{} (map #(set %) xs)))))

(defcheck solution-b813a747
  (fn [coll]
    (->> (group-by #(sort %) coll)
      (map last)
      (map set)
      (filter #(> (count %) 1))
      set)))

(defcheck solution-b8333701
  (fn t [ss]
    (set
      (map set
        (filter
          #(< 1 (count %))
          (vals (group-by frequencies ss)))))))

(defcheck solution-b8d3cb9a
  (fn[sow]
    (set (for[e (group-by sort sow) :when (> (count (fnext e)) 1)] (set (fnext e))))))

(defcheck solution-ba2c7158
  (fn [seq]
    (let [mapize (fn [w1](reduce #(let [[k v] (first %2)](merge  %1 (if (contains? %1 k) {k (inc (%1 k))}  %2)))  (for [c w1] {c 1})))]
      (set (map set (filter #(> (count %) 1)(doall (map #(let [s_map (mapize %1)](for [s seq :when (= s_map (mapize s))]s)) seq)))))
      )
    ))

(defcheck solution-ba41d2cb
  ; Try to remember agarman's (group-by frequencies) and (filter next) for next time.
  (fn [ws]
    (->> ws
      (group-by #(sort (seq %)))
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-ba4b6530
  (fn [xs]
    (->> xs
      (group-by frequencies)
      vals
      (map set)
      (filter #(> (count %) 1))
      set)))

(defcheck solution-ba6a9d54
  (fn [ws] (->> ws
             (group-by sort)
             (map #(set (val %)))
             (filter #(< 1 (count %)))
             set)))

(defcheck solution-bac19804
  #(set
     (for [[k v] (group-by sort %)
           :when (next v)] (set v))))

(defcheck solution-bac7b445
  (fn anagram [l]
    (set
      (filter #(> (count %) 1)
        (vals
          (reduce #(let [k (sort %2)]
                     (assoc % k (conj (get % k #{}) %2))) {} l))))))

(defcheck solution-bb744c36
  ;(fn anagram-finder
  ;  [ss]
  ;  (letfn [(letter-sets [s] (->> s
  ;                                (group-by identity)
  ;                                vals
  ;                                set))
  ;          (letter-set-groups [ss] (vals (group-by #(identity (first %))
  ;                                                  (map #(vector(letter-sets %) %) ss))))]
  ;    (set (->> (letter-set-groups ss)
  ;              (filter #(>(count %) 1))
  ;              (map  #((fn [xs] (map second xs)) %))
  ;              (map set)))
  ;    ))

  (fn anagram-finder
    [ss]
    (->> ss
      (group-by sort)
      (map (fn [[k v]] (set v)))
      (filter #(not= 1 (count %)) )
      set)))

(defcheck solution-bbe98b5a
  (fn [ws]
    (->> ws
      (group-by sort)
      (map #(set (val %)))
      (filter #(> (count %) 1))
      set)))

(defcheck solution-bc56ed5c
  (fn [ls]
    (set (filter #(> (count %) 1)
           (map (fn [s]
                  (set (filter (fn [x]
                                 (and (= (count s) (count x))
                                      (every? #(>= (.indexOf (vec (seq s)) %) 0) (seq x))))
                         ls)))
             ls)))))

(defcheck solution-bc6531ee
  (fn [s]

    (let [m (reduce  #(assoc %  (sort (seq %2)) (conj (get % (sort (seq %2))   #{} ) %2)  )     {}  s)]
      (set (filter #(> (count %) 1)  (map second m)))
      )
    ))

(defcheck solution-bc7742de
  (fn anagram [ws]
    (set (->> ws
           (group-by sort)
           (vals)
           (filter #(> (count %) 1))
           (map set)))))

(defcheck solution-bcc871b7
  (fn anagrams [words]
    ;; group by word length, then group by sorted letters
    (set
      (filter #(> (count %) 1)
        (map set
          (mapcat #(vals %)
            (map (fn [sameLen] (group-by #(sort %) sameLen))
              (vals (group-by count words)))))))
    ))

(defcheck solution-bcfb1c71
  (fn [s](->> s (group-by sort) (vals) (map set) (filter #(> (count %) 1 )) (set))))

(defcheck solution-bd155e16
  #(set (filter next (map set (vals (group-by sort %))))))

(defcheck solution-bd32398c
  (fn anagram-finder [words]
    (set
      (filter #(> (count %) 1)
        (map (fn [outer-word]
               (reduce
                 (fn [acc inner-word]
                   (if (= (sort outer-word) (sort inner-word))
                     (conj acc inner-word)
                     acc))
                 #{} words)) words)))))

(defcheck solution-bd95bdd7
  (fn [col] (
              set (map set (filter #(> (count %) 1) (vals (reduce #(
                                                                     if (get %1 (set %2))
                                                                     (assoc %1 (set %2) (conj (get %1 (set %2)) %2) )
                                                                     (conj %1 {(set %2) [%2]})

                                                                     ) {} col))))
              )))

(defcheck solution-bdae0c35
  (fn [w]
    (apply hash-set
      (filter (fn [s] (> (count s) 1))
        (vals
          (reduce #(update-in %1 [(sort %2)] (fnil conj #{}) %2) {} w))))))

(defcheck solution-bdbe92b2
  #(->> % (group-by sort) vals (filter second) (map set) set))

(defcheck solution-be46a178
  (fn [ws]
    (into #{} (map set (filter #(not (empty? (rest %))) (vals (group-by sort ws)))))))

(defcheck solution-be586cf1
  (fn [s]
    (->> s
      (reduce
        #(let [i (sort %2)]
           (assoc %1 i (conj (get %1 i #{}) %2)))
        {})
      (vals)
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-be6a0797
  #(->> %
     (reduce (fn [m v]
               (let [vv (sort v)]
                 (assoc m vv (conj (m vv #{}) v))))
       {})
     vals
     (filter next)
     set
     ))

(defcheck solution-bec85b00
  (fn anagram-finder [xs]
    (->> xs
      (group-by sort)
      (filter #(> (count (second %)) 1))
      (map (comp set second))
      set)))

(defcheck solution-becb33bd
  (fn [coll]
    (->> coll
      (group-by sort)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-bf2b1821
  (fn [coll]
    (->> coll
      (group-by sort)
      (map val)
      (filter #(> (count %) 1))
      (map set)
      (into #{}))))

(defcheck solution-bf2f2df9
  #(set (for [a (vals (group-by sort %)) :when (< 1 (count a))] (set a))))

(defcheck solution-bf40bc9
  (fn [l]
    (let [a (apply merge-with
              #(flatten %&)
              (map #(hash-map (apply str (sort %)) %) l))]
      (set (map set (filter coll? (vals a)))))))

(defcheck solution-bf450f66
  (fn [s]
    (set (map set (filter #(> (count %) 1) (vals (group-by sort s)))))))

(defcheck solution-bf4d01cb
  (fn [ip] (set (map set (distinct (filter #(>= (count %) 2) (map #(filter (fn [st] (= % (set st))) ip) (map set ip))))))))

(defcheck solution-bfbe6068
  (fn [coll]
    (->> coll
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-bfc91e98
  (fn [strs]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by sort strs)))))))

(defcheck solution-c04328dc
  (fn [s]
    (set (filter
           #(>= (count %) 2)
           (map set (vals (group-by #(sort %) s)))))))

(defcheck solution-c0a17010
  (fn [x] (set (map set (filter #(> (count %) 1) (vals (group-by set x)))))))

(defcheck solution-c0d08b9c
  (fn [v]
    (set
      (map set
        (filter #(> (count %) 1)
          (map val
            (group-by sort
              v)))))))

(defcheck solution-c0fda99a
  (fn finder [words]
    (letfn
     [(word-vector [s] (frequencies (seq s)))
      (anagrams? [a b] (= (word-vector a) (word-vector b)))]
      (let [grps (group-by word-vector words)
            grpvals (vals grps)
            filtered (filter #(> (count %) 1) grpvals)]
        (set (map set filtered))))))

(defcheck solution-c13f2f56
  (fn [x]
    (set
      (filter
        #(> (count %) 1)
        (vals
          (apply
            merge-with
            clojure.set/union
            (map #(hash-map (sort %1) #{%1}) x)
            )
          )
        )
      )
    ))

(defcheck solution-c160052f
  (fn [ws]
    (let [an? (fn [a b] (apply = (map #(-> % seq sort) [a b])))]
      (loop [current (first ws)
             others (rest ws)
             accum #{}]
        (if (empty? others)
          accum
          (let [{y true n false} (group-by #(an? current %) others)]
            (recur (first n) (rest n)
              (if (empty? y)
                accum
                (conj accum (set (conj y current)))))))))))

(defcheck solution-c1971a23
  (fn [strs]
    (->> strs
      (group-by frequencies)
      vals
      (filter (fn [vs] (> (count vs) 1)))
      (map set)
      set)))

(defcheck solution-c1d97a73
  #(->> %
     (group-by sort)
     vals
     (filter next)
     (map set)
     set))

(defcheck solution-c1f0ab0c
  #(set (for [[_ g] (group-by sort %)
              :when (next g)]
          (set g))))

(defcheck solution-c2a35999
  (fn [s] (set (map set (vals (filter #(next (second %)) (group-by frequencies s)))))))

(defcheck solution-c2d1ff59
  (fn [words]
    (set
      (filter #(> (count %) 1)
        (map set (vals (group-by
                         #(apply sorted-set (seq %))
                         words)))))))

(defcheck solution-c31ea90b
  #(->> (group-by frequencies %)
     vals
     (filter (fn [[s & r]] r))
     (map set)
     set))

(defcheck solution-c3206b4f
  (fn anafind [xs] (set (map set (filter #(> (count %) 1) (vals (group-by sort xs)))))))

(defcheck solution-c32c4796
  (fn [v] (set (filter next (map set (vals (group-by set v)))))))

(defcheck solution-c34b0df2
  (fn [words] (into #{} (let [pairs (map (fn [word] [(into #{} word) word]) words)]
                          (loop [ps pairs
                                 m {}]
                            (if (> (count ps) 0)
                              (let [[k v] (first ps)
                                    existing (or (m k) [])]
                                (recur (rest ps) (assoc m k (conj existing v))))
                              (for [[_ v] m :when (> (count v) 1)] (set v))))))))

(defcheck solution-c3540ff0
  (fn [s]
    (set (filter #(< 1 (count %))
           (map set (vals (group-by sort s)))))))

(defcheck solution-c3894a9b
  (fn [coll]
    (->> (group-by set coll)
      vals
      (filter #(> (count %) 1))
      (map #(apply hash-set %))
      (apply hash-set))))

(defcheck solution-c4218b93
  (fn [coll]
    (set (filter (fn [c] (> (count c) 1)) (set (map (fn[el] (second el)) ((fn ! [mp coll]
                                                                            (if-let [s (seq coll)]
                                                                              (let [el (first s)
                                                                                    ky (set el)]
                                                                                (if (mp ky)
                                                                                  (! (assoc mp ky (conj (mp ky) el)) (rest s))
                                                                                  (! (assoc mp ky #{el}) (rest s))))
                                                                              mp)) {} coll)))))))

(defcheck solution-c4332b25
  (fn [words]
    (set (map set (filter #(> (count %) 1)
                    (vals (group-by frequencies words)))))))

(defcheck solution-c4bcda9c
  (fn [s] (set (filter #(< 1 (count %)) (map set(vals (group-by #(sort %) s)))))))

(defcheck solution-c51b5aa7
  (fn [words] (loop [result {} ws words]
                (if (nil? ws)
                  (set (filter #(> (count %) 1) (set (vals result))))
                  (let [word (first ws)
                        k (sort word)
                        s (result k)]
                    (if (nil? s)
                      (recur (assoc result k #{word}) (next ws))
                      (recur (assoc result k (conj s word)) (next ws))
                      )
                    )
                  )
                )
    ))

(defcheck solution-c5ce95a7
  (fn anagram [coll]
    (letfn [(seq2map [coll]
              (apply
                (partial conj {})
                (map #(conj [] %1 %2) (range (count coll)) coll)))
            (str2seq [coll]
              (let [k (map key coll)
                    v (map (comp sort seq val) coll)]
                (apply (partial conj {}) (map #(conj [] %1 %2) k v))))
            (idc [coll]
              (let [c
                    (for [i (range (count coll))]
                      (disj
                        (apply (partial conj #{} i) (for [j (range (count coll))] (if (= (coll i) (coll j)) j)))
                        nil))]
                (filter #(not= 1 (count %)) c)))]
      (let [ks (seq2map coll)
            kn (apply
                 (partial conj #{})
                 ((comp idc str2seq seq2map) coll))
            rs (for [c kn] (map #(ks %) c))]
        (apply (partial conj #{}) (map set rs)) ))))

(defcheck solution-c689c057
  (fn [xs]
    (letfn [(hm [xs] (into #{} xs))]
      (->> xs (group-by frequencies) vals (map hm) (remove #(= 1 (count %))) hm))))

(defcheck solution-c6a08e50
  (fn [l]
    (set (map set (filter #(not= (count %) 1) (vals (group-by sort l)
                                                ))))))

(defcheck solution-c6ed0099
  (fn pr77 [coll]
    (->>
      (group-by (comp frequencies seq) coll)
      (vals )
      (filter #(< 1 (count %)) )
      (map set )
      (set)
      )))

(defcheck solution-c717b902
  #(set (filter second
          (map set
            (vals (group-by sort %))))))

(defcheck solution-c7de5a6b
  (fn anagrams [coll]
    (let [vecs
          (->>
            (group-by (comp sort seq) coll)
            (vals)
            (filter #(< 1 (count %))))]
      (set (map set vecs)))))

(defcheck solution-c82a4c57
  (fn af
    ([s] (af (first s) (set (rest s)) #{}))
    ([w s acc]
     (if (empty? s)
       acc
       (let [ana (set (filter #(= (sort w) (sort %)) s))
             rst (clojure.set/difference s ana)]
         (if (empty? ana)
           (af (first s) (set (rest s)) acc)
           (af (first rst) (set (rest rst)) (conj acc (conj ana w)))
           ))
       )
     )
    ))

(defcheck solution-c840ad9f
  (fn anagram-finder[coll]
    (loop [result #{}
           others (set coll)]
      (if(empty? others)
        (set result)
        (let [top (set (first others))
              sub (set (filter #(= top (set %)) others))]
          (if(= 1 (count sub))
            (recur result (set (rest others)))
            (recur (cons   sub result )
              (clojure.set/difference others  (set sub))

              )))))))

(defcheck solution-c86a197e
  (fn anagram77 [words]
    (let [lists (filter (fn [word] (> (count  word) 1))
                  (map second (group-by first
                                (sort (map (fn [w] [(apply str (sort w)) w ])
                                        words)))))]
      (into #{}  (for [l lists]  (into #{} (map second l)))))))

(defcheck solution-c8912f16
  (fn [words]
    (->> (group-by count words)
      vals
      (mapcat (partial group-by set))
      (filter (fn [[_ ws]] (> (count ws) 1)))
      (map (comp set second)) set)))

(defcheck solution-c8fb1dc5
  (fn [a]
    (set (map set
           (filter #(not= (count %) 1)
             (map #(map first %)
               (vals (group-by second (map #(vector % (apply str (sort %)))
                                        a)))))))))

(defcheck solution-c93a55d
  (fn [w]
    (->>
      (group-by #(sort %) w)
      (vals)
      (map set)
      (filter #(< 1 (count %)))
      (set))))

(defcheck solution-c93f4428
  #(->> %
     (group-by sort)
     vals
     (filter (fn [v] (> (count v) 1)))
     (map set)
     set))

(defcheck solution-c9529b8f
  #(reduce (fn [sets [_ sub-set]]
             (if (= (count sub-set) 1)
               sets
               (conj sets (set sub-set))))
     #{}
     (group-by sort %)))

(defcheck solution-c9705fae
  (fn [s] (set (filter #(> (count %) 1)
                 (map (comp set val) (group-by #(set %) s))))))

(defcheck solution-c9aa6312
  (fn [x] (set (map set (filter  #(> (count %) 1) (vals (group-by #(-> % seq set) x )))))))

(defcheck solution-c9b90cff
  (fn [x] (set
            (map set (filter (fn[y] (> (count y) 1))
                       (vals (group-by sort x)))))
    ))

(defcheck solution-ca5013e3
  (fn anags[s]
    (set (map #(set (second %)) (filter #(< 1 (count (second %))) (group-by sort s))))))

(defcheck solution-ca643410
  (fn [coll]
    (set (map (fn [s]
                (set (map #(% 0) s)))
           (->> (map #(vector % ((comp set seq) %)) coll)
             (group-by #(identity (% 1)))
             (map val)
             (filter #(> (count %) 1)))))))

(defcheck solution-ca7445b9
  (fn prob77 [s]
    (set (map set (filter #(< 1 (count %)) (vals (group-by frequencies s)))))))

(defcheck solution-caa84466
  (fn [words]
    (reduce
      (fn [out s] (if (> (count s) 1) (conj out s) out))
      #{}
      (vals
        (reduce
          (fn [out w]
            (let [k (apply str (sort w)) v (get out k)]
              (if (nil? v) (assoc out k #{w}) (assoc (dissoc out k) k (conj v w)))))
          {}
          words)))))

(defcheck solution-caabde38
  (fn [words]
    (letfn [(anagram? [word1 word2]
              (= (frequencies word1)
                (frequencies word2)))
            (extract-anagrams [word wordvec]
              (set (reduce (fn [anagrams candidate]
                             (if (anagram? candidate word)
                               (conj anagrams candidate)
                               anagrams))
                     []
                     wordvec)))]
      (->> (map #(extract-anagrams % words) words)
        (filter #(> (count %) 1))
        (set)))))

(defcheck solution-cad5cca5
  #(->> %
     (group-by frequencies)
     vals
     (filter next)
     (map set)
     set))

(defcheck solution-cb15bcfc
  (fn [cl]  (->> cl (group-by #(set %)) (map #(set (nth % 1))) (filter #(< 1 (count %))) set
              )))

(defcheck solution-cb4d43c3
  (fn [v] (into #{} (filter #(> (count %) 1) (map #(set %)(vals (group-by sort v)))))))

(defcheck solution-cb5248f0
  (fn [coll]
    (->> (group-by #(sort (seq %)) coll)
      (filter (fn [[_ v]] (second v)))
      (map #(set (val %)))
      set)))

(defcheck solution-cb5ccbcc
  (fn [s]
    (->> s
      (group-by #(sort (seq %)))
      vals
      (filter next)
      (map set)
      set)))

(defcheck solution-cb75e003
  (fn anagrams [words]
    (set (filter
           #(< 1 (count %))
           (map
             (fn [word]
               (set
                 (filter #(= (group-by identity word) (group-by identity %)) words)))
             words)))))

(defcheck solution-cbc62e28
  (fn [s](set(filter (fn [x] (> (count x) 1)) (map #(set(val %)) (group-by set s))))))

(defcheck solution-cbeb8fed
  (fn anagrams [xs]
    (->> xs
      (group-by frequencies) ; or with sort
      vals
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-cc75dd01
  (fn [x]

    (set (filter

           (fn [b](not= (count b) 1))

           (set(map #(set (filter (fn [a](= (sort(seq a)) (sort(seq %)))) x))x))))))

(defcheck solution-ccdeb49
  (fn [l]
    (set (map set (filter #(< 1 (count %)) (vals (group-by sort l)))))
    ))

(defcheck solution-ccf8eaa6
  (fn find-anagrams [a-vec]
    (letfn [(word-to-bag [word] (group-by identity (seq word)))]
      (set (map #(set (second %))(filter #(> (count (second %)) 1)(group-by word-to-bag a-vec))))
      )
    ))

(defcheck solution-cd15fca3
  (fn [words]
    (let [freqs-map (group-by frequencies words)]
      (into #{} (->> (vals freqs-map)
                  (filter #(< 1 (count %)))
                  (map (partial into #{})))))))

(defcheck solution-cd87783d
  (fn [words]
    (->> words
      (map #(-> [(sort %) %]))
      (group-by first)
      (map (fn [[k v]] (set (map last v))))
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-cdf1ace4
  (letfn [(val-set [[k v]]
            (set v))
          (min-two [xs]
            (> (count xs) 1))]
    (fn [words]
      (set (filter min-two
             (map val-set (group-by #(sort %) words)))))))

(defcheck solution-ce1312cc
  (fn [s] (->> (group-by #(sort (seq %)) s)
            (vals)
            (remove #(= 1 (count %)))
            (map set)
            (set))))

(defcheck solution-ce9ec531
  (fn [words]
    (->> (reduce (fn [m word]
                   (let [k (sort (re-seq #"." word))]
                     (assoc m k (conj (get m k #{}) word)))) {} words)
      (vals)
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-cebb2146
  (fn ana [strings]
    (loop [result #{},
           hs (set (first strings))
           coll strings]
      (let [fr (filter #(= hs (set %)) coll)]
        (if (< (count fr) 2)
          result
          (recur (conj result (apply conj #{} fr)) (set (first (rest coll))) (rest coll)))))))

(defcheck solution-cef07413
  (fn [v]
    (set
      (map set
        (remove #(= 1 (count %)) (vals (group-by #(apply str (sort %)) v)))))))

(defcheck solution-ceffdf1c
  (fn [ws]
    (set
      (for [[_ v] (group-by sort ws) :when (> (count v) 1)] (set v)))))

(defcheck solution-cf7bef28
  (fn [xs]
    (into #{} (map set (filter #(> (count %) 1)
                         (vals (group-by #(set %) xs)))))))

(defcheck solution-cf8b8820
  (fn [[& words]]
    (loop [w (first words), more (next words), res {}]
      (let [k (apply str (-> w .toUpperCase sort))
            res2 (assoc res k (into (get res k #{}) [w]))]
        (if more
          (recur (first more) (next more) res2)
          (into #{} (filter #(< 1 (count %)) (vals res2))))))))

(defcheck solution-d0262813
  #(->> %
     (group-by frequencies)
     (vals)
     (filter (fn [s] (> (count s) 1)))
     (map set)
     (set)))

(defcheck solution-d0de13fe
  (fn anagram-finder
    [s]
    (set
      (map
        set
        (filter
          #(> (count %) 1)
          (vals (group-by #(apply str (sort %)) s)))))))

(defcheck solution-d1160002
  (fn [xs] (->> (group-by  (fn [word] (reduce #(update-in %1 [%2] (fnil inc 0)) {} word)) xs) vals (filter #(> (count %) 1)) (map set) set)))

(defcheck solution-d1199f8
  (fn [xs] (letfn [(skeys [ss] (into [] (map sort ss)))
                   (get2 [xs] (map second xs))
                   ]
             (let [g (vals (group-by first (map vec (partition 2
                                                      (interleave (skeys xs) xs)
                                                      ))))]

               (set (map set
                      (map get2
                        (filter #(> (count %) 1) g)
                        )
                      ))
               )
             )
    ))

(defcheck solution-d12aebb5
  (fn anagramsB [x] ((fn anagramsRec [x res]
                       (if (empty? x) res
                                      (let [
                                            v ((fn anagrams1 [tst x] (conj (set (filter (fn [xi] ((fn anagrams? [x y] (= (sort x)(sort y))) tst xi)) x)) tst))  (first x)(rest x))
                                            newX (remove v x)
                                            newRes (if (> (count v) 1) (conj res v) res)
                                            ]
                                        (anagramsRec newX newRes)
                                        )))
                     x #{})))

(defcheck solution-d1be2011
  (fn myAnagramFinder
    [words]
    (let [freqs (zipmap words (map frequencies (map vec words)))
          getTheSame (fn [word mapWithFreqs] (filter #(= word (val %)) mapWithFreqs))]
      (set (filter #(> (count %) 1) (reduce #(conj %1 (set (keys (getTheSame (val %2) freqs)))) #{} freqs))))))

(defcheck solution-d200a07d
  (fn [words]
    (set (filter #(>= (count %) 2) (map #(set (second %)) (group-by set words))))))

(defcheck solution-d2066459
  (fn [c]
    (reduce
      #(let [v (val %2)] (if (< 1 (count v)) (conj % (set v)) %)) #{}
      (group-by frequencies c))))

(defcheck solution-d21efe71
  (comp set
        (partial map set)
        (partial filter #(> (count %) 1))
        vals
        (partial group-by sort)))

(defcheck solution-d2a24d69
  (fn [words]
    (let [freqs    (map vector words (map (comp frequencies seq) words))
          groups   (group-by second freqs)
          anagrams (map (partial map first) (vals groups))]
      (into #{}
        (for [a anagrams :when (>= (count a) 2)] (into #{} a))))))

(defcheck solution-d2d7696e
  (fn [s]
    (set
      (filter #(>= (count %) 2)
        (map (fn [w] (set (filter #(= (set %) (set w)) s)))
          s)))))

(defcheck solution-d3b9b792
  (fn anagram-finder [sv]
    (set (filter #(< 1 (count %))
           (vals
             (reduce #(let [k (set %2), ov (get % k), ov (if ov ov #{})]
                        (assoc % k (conj ov %2))) {} sv))))))

(defcheck solution-d3bcb1af
  (fn
    [v]
    (into #{}
      (filter #(> (count %) 1) (map #(into #{} (val %)) (group-by sort v))))))

(defcheck solution-d3fb7f36
  (fn [l] (->> l (group-by #(->> % (seq) (sort))) (vals) (map set) (filter #(> (count %) 1)) (set))))

(defcheck solution-d43837aa
  (fn [x]
    (set (map set (filter #(> (count %) 1)
                    (vals
                      (group-by (fn [i]
                                  (sort (seq i))
                                  ) x))

                    )))
    ))

(defcheck solution-d4626416
  (fn [coll]
    (let [norm-ana (->> coll
                     (map sort)
                     frequencies
                     (filter #(> (second %) 1))
                     (map first)
                     set)]
      (->> coll
        (group-by (comp norm-ana sort))
        (remove (comp nil? first))
        vals
        (map set)
        set))))

(defcheck solution-d49d9ebb
  (fn
    [words]
    (let [sort-str #(apply str (sort %))]
      (set (map set
             (filter #(< 1 (count %))
               (partition-by
                 sort-str
                 (sort-by sort-str words))))))))

(defcheck solution-d4ce0e89
  (fn [x] (set (map set (filter #(>= (count %) 2) (vals (group-by sort x)))))))

(defcheck solution-d4e38195
  (fn [ss]
    (set
      (map set
        (remove #(= 1 (count %))
          (vals (group-by set ss)))))))

(defcheck solution-d501babf
  #(->> %
     (group-by sort)
     (map second)
     (filter (comp (partial < 1) count))
     (map (partial into #{}))
     (into #{})))

(defcheck solution-d529a2cf
  (fn [ws]
    (set (map (comp set val)
           (remove (comp #{1} count val) (group-by frequencies ws))))))

(defcheck solution-d5f1d008
  (fn [words]
    (into #{} (filter #(> (count %) 1) (map (partial into #{}) (vals (group-by (partial into #{}) words)))))))

(defcheck solution-d6216271
  (fn [l]
    (set
      (map #(set (fnext %))
        (filter #(<= 2 (count (second %)))
          (group-by #(set %) l))))))

(defcheck solution-d6593497
  #(set (for [[_ [_ c :as v]] (group-by sort %) :when c] (set v))))

(defcheck solution-d6833506
  #(set (mapcat (fn[[k v]] (when (> (count v) 1) [(set v)])) (group-by sort %))))

(defcheck solution-d683d945
  (fn [s]
    (into #{}
      (map set
        (filter #(> (count %) 1)
          (vals (group-by sort s)))))))

(defcheck solution-d74e5dad
  (fn [in]
    (set
      (filter
        #(> (count %) 1)
        (map
          (comp set second)
          (group-by
            set
            in))))))

(defcheck solution-d74fb56c
  (fn [words]
    (loop [restWords words
           resultSet #{}]
      (if (empty? restWords)
        (into #{} (filter #(< 1 (count %)) resultSet))
        (let [x (first restWords)
              freqX (frequencies x)
              filterFun #(= freqX (frequencies %))]
          (recur
            (remove filterFun restWords)
            (conj resultSet (into #{} (filter filterFun restWords)))))))))

(defcheck solution-d75d8155
  (fn [v] (set (map set (filter #(< 1 (count %)) (vals (group-by sort v)))))))

(defcheck solution-d7624452
  (fn [x]
    (set (filter #(> (count %) 1)
           (map set
             (vals (group-by #(group-by identity (seq %)) x)))))))

(defcheck solution-d7b06f2
  (fn [c] (set (map set (remove #(= 1 (count %)) (vals (group-by sort c)))))))

(defcheck solution-d8086140
  (fn [words]
    (set (filter #(> (count %) 1) (map set (vals (group-by sort words)))))))

(defcheck solution-d833ab34
  (fn [s]
    (set (map set
           (filter #(> (count %) 1)
             (vals (group-by (comp frequencies seq) s)))))))

(defcheck solution-d8cb15ca
  (fn anagrams [v]
    (set (filter #(> (count %) 1) (map set (vals (group-by sort v)))))))

(defcheck solution-d9483909
  (fn [s] (->> s (group-by sort) (map val) (filter #(< 1 (count %))) (map set) (into #{}))))

(defcheck solution-d94b09cb
  (fn [coll]
    (set
      (map set
        (filter #(< 1 (count %))
          (vals (group-by set coll)))))))

(defcheck solution-d954c38b
  (fn [words]
    (let [f (fn [rmap word]
              (if-let [anagrams (get rmap (set word))]
                (assoc rmap (set word) (conj anagrams word))
                (assoc rmap (set word) (sorted-set word))))
          >1? #(> (count (second %)) 1)]
      (->> words
        (reduce f {})
        (filter >1?)
        vals
        set))))

(defcheck solution-d9aa4dfe
  #(set
     (map (comp set val)
       (remove (comp #{1} count val)
         (group-by sort %)))))

(defcheck solution-d9c96d09
  (fn anagrams [word-list]
    (let [perm (fn [u v] (= (set u) (set v)))]
      (loop [words word-list anagram-list #{}]
        (if (<= (count words) 1)
          anagram-list
          (let [[x & rest] (vec words)
                matches (set (filter #(perm x %) rest))]
            (if (empty? matches)
              (recur (set rest) anagram-list)
              (recur (clojure.set/difference (set rest) matches) (conj anagram-list (conj matches x))))))))))

(defcheck solution-d9e3ae6
  (fn [ss]
    (->>
      ss
      (group-by sort)
      (vals)
      (filter #(>= (count %) 2))
      (map set)
      (set))))

(defcheck solution-da2c3efd
  (fn [input] (set
                (map set
                  (filter #(< 1 (count %))
                    (reduce (fn [anagrams value]
                              (let [has-match (-> (filter #(= (into #{} %)
                                                             (into #{} value))
                                                    (map first anagrams))
                                                empty?
                                                not)]
                                (if has-match
                                  (map (fn [a]
                                         (let [match? (= (into #{} (first a))
                                                        (into #{} value))]
                                           (if match? (conj a value)
                                                      a))) anagrams)
                                  (conj anagrams (list value)))))
                      '() input))))))

(defcheck solution-da3601e1
  (fn [coll]
    (->> coll
      (reduce (fn [acc w] (update-in acc [(set w)] conj w)) {})
      (vals)
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-da8ed3ad
  (fn [words]
    (set
      (filter second
        (map set (vals (group-by sort words)))))))

(defcheck solution-da9a817
  (fn [words]
    (set (map set
           (filter
             #(< 1 (count %))
             (vals
               (group-by frequencies words)))))))

(defcheck solution-dad51b0
  (fn [coll]
    (loop [remaining coll ana (hash-map)]
      (if (empty? remaining)
        (apply hash-set (for [kv (filter #(> (count (second %)) 1) ana)] (second kv)))
        (let [[word & remain] remaining]
          (if (contains? ana (sort (sequence word)))
            (recur remain (update-in ana [(sort (sequence word))] conj word))
            (recur remain (conj ana [(sort (sequence word)) (hash-set word)]))))))))

(defcheck solution-daf1d7b6
  (fn [s]
    (->>
      s
      (group-by sort)
      (map second)
      (filter #(< 1 (count %)))
      (map set)
      set)))

(defcheck solution-db0f4ba3
  (fn[words]
    (let [pairs
          (fn [a]
            (for [xa a xb a
                  :let [sxa (sort xa)
                        sxb (sort xb)]
                  :when (and (not (= xa xb))
                             (= sxa sxb))] [sxa [xa xb]]))
          ->map
          (fn[coll]
            (loop [[head & more] coll
                   map {}]
              (if (nil? head)
                map
                (let [[key [fst snd]] head]
                  (recur more
                    (assoc map key (conj (get map key #{}) fst snd)))))))]

      (set (vals (->map (pairs words)))))))

(defcheck solution-db77d574
  (fn [s]
    (set
      (keep #(if (> (count %) 1) (set %))
        (set (vals (group-by sort s)))))))

(defcheck solution-db7a24c9
  (fn [coll]
    (->>
      (group-by frequencies coll)
      vals
      (remove #(= 1 (count %)))
      (map set)
      set)))

(defcheck solution-dc6fea5a
  (fn [xs] (set (filter #(> (count %) 1) (map set (vals (group-by sort xs)))))))

(defcheck solution-dcf5cbad
  (fn anagrams [words]
    (->> (vals (group-by sort words))
      (map set)
      (filter #(< 1 (count %)))
      (into #{}))))

(defcheck solution-dd094ef6
  (fn __
    [words]
    (letfn [(occurrences [x]
              (let [current (frequencies x)]
                (set (filter #(= current (frequencies %)) words))))]
      (->> words (map occurrences) (filter #(> (count %) 1)) set))))

(defcheck solution-dd318f35
  (fn [lst]
    (set (map set
           (filter
             #(> (count %) 1)
             (vals (group-by set lst)))))))

(defcheck solution-dd5b5164
  (fn [words] (->>
                ; convert each word into a "bag" of { letter -> count }
                (letfn [ (->bag [word]
                           (reduce
                             (fn [word letter]
                               (if (contains? word letter)
                                 (assoc word letter (inc (word letter)))
                                 (assoc word letter 1)))
                             {}
                             word))]
                  (map (juxt ->bag identity) words))  ; seq of [bag, word] pairs
                ; reduce pairs into map of { bag -> #{ words } }
                (reduce
                  (fn [r e]
                    (let [[word anagram] e
                          [anagrams] [(if (contains? r word) (r word) #{})]
                          [anagrams] [(conj anagrams anagram)]]
                      #_(print anagrams)
                      (assoc r word anagrams)))
                  {})
                ; eliminate keys with only one word
                (vals)
                (filter #(< 1 (count %)))
                (into (hash-set))
                )))

(defcheck solution-ddcda905
  (fn [words]
    (set (filter #(not (= (count %) 1)) (vals
                                          (reduce (fn [acc word] (let [key (group-by identity word)]
                                                                   (if (acc key)
                                                                     (assoc acc key (conj (acc key) word))
                                                                     (assoc acc key #{word}))))
                                            {}
                                            words))))))

(defcheck solution-de261d07
  (fn [words]
    (set (filter next
           (map (comp set second)
             (group-by (partial group-by identity) words))))))

(defcheck solution-de498e69
  (fn [ws]
    (set (map #(apply hash-set %) (filter #(<= 2 (count %)) (vals (group-by sort ws)))))))

(defcheck solution-de5e88e5
  (fn [a] (set (map set (filter #(> (count %) 1) (vals (group-by #(frequencies %) a)))))))

(defcheck solution-deaa6a05
  ;; Inspired by Chouser
  (fn [w]
    (set (filter #(> (count %) 1)
           (for [[_ g] (group-by frequencies w)]
             (set g))))))

(defcheck solution-df2d367
  (fn [x](let [ga (fn [w] (apply str (sort (seq w))))]
           (set (map set (filter #(pos? (dec (count %))) (partition-by ga (sort-by ga x))))))))

(defcheck solution-dfbdcd6c
  (fn ana [x] (set (for [ [k v]
                         (group-by last (map #(vector % (sort %) ) x))
                         :when (> (count v) 1)] (set(map #(first %) v ))))))

(defcheck solution-dfd32ae4
  (fn find-anagrams [coll]
    (set
      (->> (group-by sort coll)
        (vals)
        (map set)
        (filter #(> (count %) 1))))))

(defcheck solution-dffb7880
  (fn [words]
    (->> (group-by sort words)
      vals
      (map set)
      (filter #(> (count %) 1))
      (into #{}))))

(defcheck solution-e00e2e3b
  (fn [l]
    (set
      (filter
        #(< 1 (count %))
        (map set
          (vals
            (group-by
              #(sort (seq %))
              l)))))))

(defcheck solution-e01a48dc
  #(set (filter second (vals (apply merge-with into (map (fn [s] {(sort s) #{s}}) %))))))

(defcheck solution-e077e064
  (fn [s](into #{} (filter #(> (count %) 1)(reduce (fn [result head]
                                                     (let [key (apply str (sort head))
                                                           keys (into #{} (map (fn [anagrams] (set (map #(apply str (sort %)) anagrams))) result))]
                                                       (if (not (contains? keys #{key}))
                                                         (conj result #{head})
                                                         (into #{} (map #(if (= (apply str (sort (first %1))) key)
                                                                           (conj %1 head) %1) result))))) #{} s)))))

(defcheck solution-e0aafd03
  (fn find-anagrams [s]
    (set (map set (filter #(< 1 (count %))
                    (vals (group-by #(set %)
                            s)))))))

(defcheck solution-e145452c
  (fn anagram
    [coll]
    (loop [[x & xs] coll alls coll res []]
      (if x
        (recur xs alls (conj res (filter #(= (sort x) (sort %)) alls)))
        (into #{} (filter #(> (count %) 1) (map #(into #{} %) res)))))))

(defcheck solution-e1f48aa5
  (fn [coll] (set (map set (filter #(> (count %) 1) (vals (group-by frequencies coll)))))))

(defcheck solution-e203f6dd
  (fn anagram [xs]
    (->> xs
      (map #(vector (sort %) %))
      (group-by first)
      vals
      (map (fn [xs] (set (map #(second %) xs))))
      (filter #(> (count %) 1))
      set
      )))

(defcheck solution-e2309939
  (fn anagrams [words]
    (->> words
      (reduce (fn [acc w]
                (let [k (sort w)]
                  (assoc acc k (conj (get acc k #{}) w))))
        {})
      vals
      (filter #(>= (count %) 2))
      set)))

(defcheck solution-e340519a
  (fn [ws]
    (->> (reduce (fn [M w] (let [nf (reduce (fn [coll a] (update-in coll [a] (fnil inc 0))) {} w)] (update-in M [nf] conj w))) {} ws)
      vals
      (filter #(>= (count (second %)) 2))
      (map set)
      (into #{}))))

(defcheck solution-e38ea54e
  (fn [words]
    (set (remove #(= (count %) 1)
           (map set (vals (group-by frequencies words)))))))

(defcheck solution-e3f73e95
  (fn [s]
    (->> (group-by sort s)
      (filter #(> (count (val %)) 1))
      (map #(set (val %)))
      (set))))

(defcheck solution-e3fa1174
  (fn [ws] (set (map set (filter #(> (count %) 1) (vals (group-by sort ws)))))))

(defcheck solution-e48753e9
  (fn [i]
    (into #{} (filter (comp seq rest) (vals (reduce (fn [s [k v]]
                                                      (let [x (or (s k) #{})
                                                            z (conj x v)]
                                                        (assoc s k z))) {} (#(map (fn [y]
                                                                                    [(set y) y]) %) i)))))))

(defcheck solution-e4f5e16a
  (fn [args](set (filter #(> (count %) 1) (map set (vals (group-by sort args)))))))

(defcheck solution-e5244640
  (fn [seq]
    (let [groups (group-by sort seq)
          not-single-grp (filter #(< 1 (count (second %))) groups)]
      (set (map #(set (second %)) not-single-grp)))))

(defcheck solution-e599327d
  (fn [s]
    (->> s
      (sort #(compare (str (sort %1)) (str (sort %2))))
      (partition-by #(sort %) )
      (filter #(> (count %) 1) )
      (map #(set %) )
      (set))
    ))

(defcheck solution-e5f714f7
  #(set
     (filter
       (comp (partial < 1) count)
       (map
         set
         (vals
           (group-by sort %))))))

(defcheck solution-e607688
  #(set (map set
          (remove (fn [e] (= 1 (count e))) (vals (group-by frequencies %))))))

(defcheck solution-e60fc812
  (fn [words]
    (->> words (group-by sort) vals (filter next) (map set) set)))

(defcheck solution-e61c7f4c
  (fn a [c]
    (set (filter (fn [e] (> (count e) 1)) (vals (reduce #(let [k (set %2)] (assoc % k (conj (% k #{}) %2))) {} c))))))

(defcheck solution-e61ea502
  (fn [ws]
    (let [s (map set (vals (group-by sort ws)))]
      (set (filter #(> (count %) 1) s)))))

(defcheck solution-e6c45291
  (fn [words]
    (->>
      words
      (group-by set)
      vals
      (remove #(= 1 (count %)))
      (map set)
      set
      )
    ))

(defcheck solution-e7942a7e
  (fn __ [s]
    (->> (group-by sort s)
      (map val)
      (filter #(< 1 (count %)))
      (map set)
      (set))))

(defcheck solution-e7e6d74a
  (fn [c]
    (set (filter #(> (count %) 1)
           (map set
             (for [i c]
               (filter #(= (set i) (set %)) c)))))))

(defcheck solution-e7f10875
  (fn [words]
    (let [ana? (fn [ & words]
                 (if (reduce = (map count words))
                   (reduce = (map sort words))
                   false) )]
      (set
        (filter #(not= 1 (count %))
          (for [word words]
            (set (filter #(ana? word %) words))))))))

(defcheck solution-e7f531
  (fn [coll]
    ((comp
      (fn [m] (set (vals m)))
      (fn [m] (filter (fn [[k v]] (coll? v)) m))
      (fn [m] (apply merge-with (fn [v1 v2] (if (coll? v1) (conj v1 v2) #{v1 v2})) m))
      (fn [m] (map (fn [it] {(sort it) it}) m)))
     coll)))

(defcheck solution-e8509c67
  #(set
     (filter (fn [x] (> (count x) 1))
       (map
         (comp set second)
         (group-by
           (comp set seq)
           %)))))

(defcheck solution-e92c21ee
  (fn [w] (->> w (group-by sort) vals (filter #(> (count %) 1)) (map set) set)))

(defcheck solution-e966bab6
  (fn anagrams
    [strs]
    (->> (group-by frequencies strs)
      vals
      (map set)
      (filter #(> (count %) 1))
      (into #{}))))

(defcheck solution-e984f2cb
  (fn [col] (set (for [[k v] (group-by sort col) :when (> (count v) 1)] (set v)))))

(defcheck solution-e9e7be1f
  (fn [xs]
    (->> xs
      (group-by sort)
      vals
      (filter #(> (count %) 1))
      (map set)
      (into #{}))))

(defcheck solution-ea1212fd
  (fn [words]
    (letfn [(anagram-of[a b]
              (= (sort a) (sort b)))
            (anagram-set [word words]
              (into #{}
                (filter #(anagram-of word %1) words)))]
      (into #{}
        (filter #(> (count %1) 1)
          (map #(anagram-set %1 words) words))))))

(defcheck solution-eafde454
  (fn [l]
    (->>
      l
      (group-by set)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-ebc51409
  (fn anagram-finder [s]
    (->> (group-by #(apply str (sort (seq %))) s)
      (vals)
      (reduce #(if (> (count %2) 1) (conj %1 (set %2)) %1) #{})
      )
    ))

(defcheck solution-ebd3c407
  #(set (map set (filter (fn [y] (<= 2 (count y)))
                   (vals (group-by
                           (fn [x] (sort  (seq x))) %))))))

(defcheck solution-ebdcda18
  #(->> %
     (group-by sort)
     vals
     (filter (fn [s] (< 1 (count s))))
     (map set)
     set))

(defcheck solution-ec4e7ecb
  #(->> %
     (group-by frequencies)
     (vals)
     (filter next)
     (map set)
     (set)))

(defcheck solution-ecac632f
  (fn [ws] (set (filter (fn [v] (> (count v) 1)) (map set (vals (group-by #(apply str (sort %)) ws)))))))

(defcheck solution-ed3a81b0
  (fn [coll]
    (let [pk (filter #(< 1 (count %))
               (partition-by #(val %)
                 (sort-by #(val %)
                   (zipmap (range) (map (comp #(apply str %) sort seq) coll)))))]
      (apply conj #{}
        (for [i pk]
          (apply conj #{}
            (for [j i] (nth coll (key j)))))))))

(defcheck solution-eda1528b
  (fn [words]
    (->> (group-by frequencies words)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-eda355f4
  (fn anagram- [coll]
    "77. Write a function which finds all the anagrams in a vector of words."
    ;; Works by building a multimap of common anagram -> set of words that generate that anagram
    ;; then filtering out any set that doesn't have at least one member.
    (let [m (reduce #(assoc %1 (sort %2) (conj (get %1 (sort %2) #{}) %2)) {} coll)]
      (apply hash-set (filter #(> (count %1) 1) (vals m))))))

(defcheck solution-edac295
  (fn [col]
    (let [a (group-by sort col)
          b (filter #(> (count %) 1) (vals a))]
      (into #{} (map #(into #{} %) b)))))

(defcheck solution-ee66c6cb
  (fn [coll]
    (->> (group-by sort coll)
      vals
      (filter (comp #(< 1 %) count))
      (map set)
      set)))

(defcheck solution-ee914a61
  (fn [words]
    (set (filter #(> (count %) 1)
           (map (fn [x] (set (map #(second %) x)))
             (partition-by first
               (sort-by first (map #(vector (clojure.string/join (sort %)) %) words))))))))

(defcheck solution-ee9fd8da
  (fn [x]
    (set (filter #(> (count %) 1)
           (for [w x]
             (set (filter #(and (= (count w) (count %))
                                (clojure.set/subset? (set w) (set %))) x)))))))

(defcheck solution-eeb07b16
  (fn [col]
    (set
      (filter #(> (count %) 1)
        (vals (reduce
                #(merge-with clojure.set/union %1 {(frequencies %2) #{%2}})
                {}
                col))))))

(defcheck solution-ef1807ca
  (fn [words]
    (->> (group-by frequencies words)
      (vals ,)
      (filter #(< 1 (count %)))
      (map set ,)
      (set ,))))

(defcheck solution-ef26ab80
  (fn q77 [coll]
    (->>
      (group-by #(sort %) coll)
      vals
      (remove #(= 1 (count %)))
      (map set)
      set)))

(defcheck solution-ef39ea49
  #(set (filter (comp (partial < 1) count)(map set (vals (group-by sort %))))))

(defcheck solution-ef3ace93
  (fn anagrams [words]  (let [sortw (fn [word] (str (sort word)))        res (reduce (fn [res word]                      (update-in res [(sortw word)] #((fnil conj #{}) % word)))                    {} words)]    (set (filter #(> (count %) 1) (vals res))))))

(defcheck solution-efbab67b
  (fn [ws] (set (filter #(< 1 (count %)) (map set (vals (group-by sort ws)))))))

(defcheck solution-f040f99b
  (fn [coll] (set (filter #(> (count %) 1) (map (comp set second) (group-by sort coll))))))

(defcheck solution-f09459f9
  (fn anagrams [s]
    (->> s
      (group-by sort)
      (vals)
      (filter #(< 1 (count %)))
      (map set) set)))

(defcheck solution-f0aae0bf
  (fn [coll]
    (set
      (filter
        #(> (count %) 1)
        (map #(set (second %)) (group-by #(sort (seq %)) coll))))))

(defcheck solution-f12c4a38
  (fn [coll]
    (set (filter #(> (count %) 1)
           (vals (reduce (fn [m s]
                           (assoc-in m
                             [(sort s)]
                             (set (conj (m (sort s)) s))))
                   {}
                   coll))))))

(defcheck solution-f1688ee7
  (fn anagrams [x]
    "Assumes the input is all lowercase."
    (set (map set (filter #(> (count %) 1) (vals (group-by sort
                                                   x)))))))

(defcheck solution-f235536c
  (fn [a-vec]
    (let [root #(apply str (sort %))]
      (set
        (filter #(> (count %) 1)
          (vals
            (reduce
              #(assoc %1 (root %2) (conj (%1 (root %2) #{}) %2))
              {}
              a-vec)))))))

(defcheck solution-f2477a98
  (fn [xs]
    (->> xs
      (group-by frequencies)
      (vals)
      (map set)
      (filter #(> (count %) 1))
      (into #{}))))

(defcheck solution-f249d496
  (fn anag [coll]
    (reduce #(conj %1 %2) #{}
      (filter
        #(> (count %) 1)
        (vals
          ((fn to-map [coll]
             (reduce
               (fn [m n]
                 (let [k (apply conj #{} n)]
                   (assoc m k (conj (get m k #{}) n))))
               {}
               coll))
           coll))))))

(defcheck solution-f2500621
  (fn [words]
    (set (filter #(> (count %) 1)
           (map set (vals (group-by sort words)))))))

(defcheck solution-f259b774
  #(set (map set (filter next (vals (group-by sort %))))))

(defcheck solution-f27b2c5e
  (fn [words]
    (->>
      words
      (group-by set)
      (vals)
      (map set)
      (filter #(> (count %) 1))
      (set)
      )
    ))

(defcheck solution-f27d64b8
  (fn [words]
    (into #{}  (filter #(>= (count %) 2) (map set (vals (group-by sort words)))))))

(defcheck solution-f2fb7a3b
  (fn [ws]
    (set
      (filter
        #(> (count %) 1)
        (vals
          (reduce
            (fn [m w]
              (let [key (sort (seq w))]
                (assoc m key (conj (get m key #{}) w))))
            {}
            ws))))))

(defcheck solution-f3315fb6
  #_(fn [s]
      (into #{}
        (remove empty?
          (for [word s :let [ws (seq word)] ]
            (set (filter (set s)
                   (for [m ws n ws o ws p ws] (clojure.string/join [m n o p]))))))))

  ;Skip words alreay found
  (fn anagram [s]
    (let [
          words (fn words [w s]
                  (clojure.string/join
                    (reduce (fn [acc x] (conj acc (get w x))) [] s)) )

          perms (fn [n]
                  (let [f (fn [s]
                            (mapcat (fn [x] (for [y (range n) :when (nil? ((set x) y))] (conj x y))) s))]
                    (loop [s (for [j (range n)] [j])
                           i (- n 1)]
                      (if (> i 0)
                        (recur (f  s) (dec i))
                        s ))))
          perms-m (memoize perms)
          ]
      (loop [acc #{} word (first s) s1 (rest s)]
        (if (nil? word)
          (set (filter (fn [x] (> (count x) 1)) acc))
          (recur (if (= (.indexOf (mapcat seq acc) word) -1)
                   (conj acc (set (filter (set s) (map (partial words word) (perms-m (count word))))))
                   acc)
            (first s1)
            (rest s1)))))))

(defcheck solution-f34896b6
  #(set (for [[_ w] (group-by (comp set seq) %)
              :when (next w)]
          (set w))))

(defcheck solution-f356e77
  (letfn [(anagram? [a b]
            (= (sort a) (sort b)))
          (get-anagrams [words word]
            (set (filter #(anagram? word %) words)))
          (anagrams [words]
            (set (remove #(= 1 (count %))
                   (map (partial get-anagrams words) words))))]
    anagrams))

(defcheck solution-f35daed0
  (fn [s]
    (set
      (map set
        (filter #(> (count %) 1)
          (vals (group-by #(sort %) s)))))))

(defcheck solution-f36d7d
  (fn [in]
    (set
      (filter #(> (count %) 1)
        (vals
          (reduce
            (fn [hm el]
              (let [pel (sort el)]
                (if (contains? hm pel)
                  (merge-with conj hm {pel el})
                  (merge hm {pel #{el}}))))
            {}
            in))))))

(defcheck solution-f391bce8
  (fn analgram [strs]
    (letfn [(same-elements [ls rs]
              (and (= (count ls) (count rs))
                   (every? (set ls)rs)))]
      (loop [an #{} strs strs]
        (if (empty? strs)
          an
          (let [f (first strs)
                ans (filter #(same-elements f %) strs)]
            (if (= 1 (count ans))
              (recur an (rest strs))
              (recur (conj an (set ans))
                (remove #(same-elements f %) strs)))))))))

(defcheck solution-f3b33d3a
  (fn anagram [coll]
    (->> (group-by frequencies coll)
      vals
      (filter #(> (count %) 1))
      (map set)
      set)))

(defcheck solution-f3e186f5
  (fn anag
    [words]
    (set (remove #(> 2 (count %))
           (reduce #(into %1
                      [((fn [a b]
                          (set (filter (fn [x] (and
                                                (= (count (set x))
                                                  (count (set b))
                                                  (count (clojure.set/intersection (set b) (set x))))))
                                 a)))
                        words %2)]) #{} words)))))

(defcheck solution-f4642d4f
  (fn [x]
    (->>
      (group-by #(apply str (sort %)) x)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set)
      )
    ))

(defcheck solution-f48bc562
  (fn [coll]
    (set (filter #(>= (count %) 2) (vals (apply merge-with into (map (fn [x] {(sort x) #{x}}) coll)))))))

(defcheck solution-f4a954cc
  (fn [wv] (let [wg (group-by #(sort (seq %)) wv)]
             (set (map set (filter #(> (count %) 1) (vals wg)))))))

(defcheck solution-f4b09c78
  (fn [l]
    (let [ags (set (map (comp set frequencies) l))]
      (set (filter (comp (partial < 1) count)
             (map (comp set (partial map second))
               (map (group-by first
                      (filter #(= (first %) ((comp set frequencies) (second %)))
                        (mapcat (fn [x] (map (fn [y] (list y x)) ags)) l))) ags)))))))

(defcheck solution-f4b199ee
  (fn [items]
    (into #{} (filter #(> (count %) 1)
                (vals (apply merge-with into (map #(hash-map (set %) #{%}) items)))))))

(defcheck solution-f4b30254
  (fn [c]
    (into #{}
      (->> c
        (map sort)
        (frequencies)
        (filter #(> (val %) 1))
        (map first)
        (map #(set (filter (fn [x] (= % (sort x))) c)))))))

(defcheck solution-f4e71c66
  (fn [ws]
    (let [classes (set (map sort ws))
          find-reps (fn [c] (set (filter #(= c (sort %)) ws)))]
      (set (filter #(< 1 (count %)) (map find-reps classes))))))

(defcheck solution-f54ae0ae
  (fn [s]
    (set (map set (filter #(< 1 (count %)) (vals (group-by sort s)))))))

(defcheck solution-f554f988
  (fn [w]
    (->> w
      (group-by frequencies)
      (vals)
      (map set)
      (filter #(> (count %) 1))
      (set))))

(defcheck solution-f5662ecf
  (fn af [coll]
    (->> (group-by frequencies coll)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-f583820c
  (fn [c] (set (map set
                 (filter #(> (count %) 1)
                   (vals
                     (group-by #(sort (vec %)) c)))))))

(defcheck solution-f5bd0c28
  (fn [x]
    (set (filter #(< 1 (count %))
           (map set (vals
                      (reduce (fn [a b]
                                (let [p (sort (seq b))]
                                  (conj a {p (cons b (a p))})))
                        {} x)))))))

(defcheck solution-f5eafdd7
  (fn [w]
    (set
      (filter #(> (count %) 1)
        (map #(set (for [x w
                         :when (= (sort %) (sort x))] x))
          w)))))

(defcheck solution-f68d8415
  (fn anag-finder[anags]
    (into #{} (map set (filter #(> (count %) 1) (vals (group-by #(sort %) anags)))))))

(defcheck solution-f6b8da07
  (fn [c]
    (->>
      (group-by frequencies c)
      (filter #(> (count (nth % 1)) 1))
      (reduce #(conj % (set (val %2))) #{})
      )))

(defcheck solution-f6c18e65
  (fn [c]
    (set (map set
           (filter #(< 1 (count %))
             (vals (group-by sort c)))))))

(defcheck solution-f86ae7f
  (fn angrm-find [words]
    (into #{} (map #(into #{} %)
                (filter #(> (count %) 1)
                  (vals (group-by sort words)))
                ))
    ))

(defcheck solution-f8bf9314
  (fn f [v]
    (into #{} (filter #(> (count %) 1) (for
                                        [[kk vv] (group-by (fn ff [b] (group-by (fn fff[a] a) b)) v)]
                                         (into #{} vv))))))

(defcheck solution-f91e2404
  (fn [ss]
    (->> ss
      (map #((juxt sort identity) %))
      (group-by (comp str first))
      vals
      (filter #(> (count %) 1))
      (map #(set (map (fn [x] (second x)) %)))
      set
      )))

(defcheck solution-fa8bff1d
  (fn [s]
    (let [m (reduce #(assoc %1 (sort %2) (cons %2 (get %1 (sort %2)))) {} s)]
      (apply hash-set (map #(apply hash-set %1) (filter #(< 1 (count %1)) (vals m)))))))

(defcheck solution-facf2a6b
  (fn [words]
    (letfn [(anagram? [a b] (= (frequencies a) (frequencies b)))
            (find-set [sets w] (some #(when (anagram? (first %) w) %) sets))
            (update-into [sets w]
              (let [s (find-set sets w)]
                (conj (disj sets s) ((fnil conj #{}) s w))))]
      (into #{} (filter #(> (count %) 1) (reduce update-into #{} words)))
      )
    ))

(defcheck solution-fb3f9d16
  (fn find-anagrams [words]
    (set
      (map set (filter #(> (count %) 1)
                 (vals (group-by #(sort (seq %)) words)))))))

(defcheck solution-fb751ed9
  (fn [coll]
    (letfn [(anagram? [x y]
              (apply = (map frequencies [x y])))]
      (set (filter #(> (count %) 1)
             (map
               #(set (filter (partial anagram? %) coll))
               coll))))))

(defcheck solution-fb963c5f
  (fn [words]
    (->> words
      (group-by sort)
      (vals)
      (filter #(> (count %) 1))
      (map set)
      (set))))

(defcheck solution-fc18292a
  #(->> %
     (group-by sort)
     (vals)
     (filter next)
     (map set)
     (set)))

(defcheck solution-fc1daa07
  (fn [v]
    (set (remove #(= (count %) 1)
           (for [x v]
             (set (for [y v :when (= (set x) (set y))] y)))))))

(defcheck solution-fc7151c5
  (fn [words]
    (->> (reduce
           (fn [sets word]
             (let [id (apply str (sort (seq word)))]
               (assoc sets id (conj (get sets id #{}) word))))
           {}
           words)
      vals
      (filter #(< 1 (count %)))
      (apply hash-set))))

(defcheck solution-fcc3b1a4
  (fn [v] (set (filter #(> (count %) 1) (map (comp set second) (group-by frequencies v))))))

(defcheck solution-fcc6017a
  (fn [words] (let [letters (into {} (for [w words] [w (sort w)]))] (into #{}
                                                                      (filter #(> (count %) 1) (map #(set (map first (val %))) (group-by #(val %)
                                                                                                                                 letters)))))))

(defcheck solution-fcff3906
  (fn [s]
    (reduce
      (fn [r w]
        (let [c (filter #(= (sort w) (sort %)) s)]
          (if (> (count c) 1)
            (conj r (into #{} c))
            r)))
      #{}
      s)))

(defcheck solution-fd7bb598
  (fn [coll]
    (set (map set (map second (filter #(> (count (second %)) 1) (reduce (fn [acc s]
                                                                          (update-in acc [(apply str (sort s))] conj s)) {} coll)))))
    ))

(defcheck solution-fe16234a
  (fn [ss]
    (->>
      ss
      (group-by sort)
      vals
      (filter second)
      (map set)
      set)))

(defcheck solution-fe66dd45
  (fn anagrams
    [coll] {:pre [(every? string? coll)]}
    (->> coll                            ; ["eat" "cat" "ate"]
      (group-by frequencies)          ; {{\e 1, \a 1, \t 1} ["eat" "ate"],
      ;  {\c 1, \a 1, \t 1} ["cat"]}
      vals                            ; (["eat" "ate"] ["cat"])
      (remove (comp (partial = 1)     ; (["eat" "ate"])
                    count))
      (map set)                       ; (#{"eat" "ate"})
      set)))

(defcheck solution-fe754fe2
  (fn [words]
    (->>
      (map (fn [w] (set (filter #(= (sort w) (sort %)) words))) words)
      (remove #(= (count %) 1))
      set)))

(defcheck solution-fe86ec91
  (fn [words] (reduce #(if (> (count (second %2)) 1) (conj % (set (second %2))) %) #{} (group-by sort words))))

(defcheck solution-fe981d6
  (fn [coll]
    (->> coll
      (group-by #(set (seq %)))
      (filter #(> (count (val %)) 1))
      (map (comp set val))
      (set)
      )))

(defcheck solution-ff04ebcb
  (fn [xs]
    ;;;;; VERY UGLY
    (into #{}
      (remove #(= 1 (count %))
        (into #{}
          (for [[k v]
                (apply merge-with clojure.set/union
                  (map #(hash-map
                          (apply str (sort %)) (set [%])) xs))] v)
          )))))

(defcheck solution-ff3384dc
  (fn [xs]
    (let [sets (map set xs)
          freqs (frequencies sets)
          anagram-sets (filter #(>= (last %) 2) freqs)
          winning-sets (map first anagram-sets)]
      (set (for [w winning-sets]
             (set (filter #(= w (set %)) xs)))))))

(defcheck solution-ff6609bf
  (fn __ [col]
    (set (map set
           (filter #(> (count %) 1)
             (vals (group-by frequencies col)))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-77))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
