(ns coal-mine.problem-103
  (:require [coal-mine.checks :refer [defcheck-103] :rename {defcheck-103 defcheck}]
            [clojure.pprint]
            [clojure.set]
            [clojure.test]))

(defcheck solution-1078a412
  #(nth (iterate (fn [ss] (set (remove nil? (for [s ss x %2] (if (s x) nil (conj s x))))))
          [#{}]) %))

(defcheck solution-1086c9b3
  (fn [n s] (set (filter #(= n (count %)) (reduce (fn [r _] (set (mapcat #(map (partial conj %) s) r)) ) #{#{}} (range n))))))

(defcheck solution-11b2ba9
  (fn kcomb [n s]
    (letfn [(pset [s]
              (if (empty? s)
                #{#{}}
                (let [p (pset (set (rest s)))]
                  (clojure.set/union
                    p
                    (map #(conj % (first s)) p)))))]
      (let [res (set (filter #(= (count %) n) (pset s)))]
        (if (empty? res)
          #{}
          res)))))

(defcheck solution-11e97136
  (fn [k st]
    (if (> k (count st)) #{}
                         (loop [n 1 res [#{}]]
                           (if (> n k) (set res)
                                       (recur (inc n) (for [e st s res :while (not (s e))] (conj s e))))))))

(defcheck solution-1256964f
  (fn ks [k s]
    (into #{}
      (cond (> k (count s)) #{}
            (= k 1) (map hash-set s)
            :else (concat (map #(into #{} (cons (first s) %)) (ks (dec k) (rest s)))
                    (ks k (rest s)))))))

(defcheck solution-13b561bc
  (fn [n coll]
    (set
      (filter #(= n (count %))
        (reduce (fn [coll v]
                  (into coll (map #(conj % v) coll)))
          #{#{}}
          coll)))))

(defcheck solution-1587f4af
  (fn [n s]
    (set
      (filter
        #(= n (count %))
        (reduce
          (fn [l x]
            (concat l (map #(conj %1 %2) l (repeat x))))
          [#{}] s)))))

(defcheck solution-160f287a
  (fn [k s]
    (if (or (= k 0) (empty? s))
      #{}
      (loop [i 1, result (into #{} (map hash-set s))]
        (if (= k i)
          result
          (recur (inc i)
            (into #{} (for [x s, subset result
                            :when (not (contains? subset x))]
                        (conj subset x)))))))))

(defcheck solution-1672934d
  (fn ([i l]
       (set
         (map
           #(set (take i %))
           ((fn f [l n j]
              (if (< n j)
                []
                (get [l [l]] n
                  (for [[h & t] (take n (partition n 1 (cycle l)))
                        r       (f t (dec n) 0)]
                    (cons h r)))))
            l
            (count l)
            i))))))

(defcheck solution-16761023
  (letfn [(K [n s]
            (cond (< (count s) n) #{}
                  (= (count s) n) #{(into #{} s)}
                  (zero? n) #{#{}}
                  :else (into #{}
                          (concat (K n (rest s))
                            (for [x (K (dec n) (rest s))]
                              (conj x (first s)))))))]
    K))

(defcheck solution-1786e1a0
  (fn k-comb [n items]
    (cond
      (zero? n) #{#{}}
      (zero? (count items)) #{}
      :else
      (let [i (first items)
            rem (disj items i)]
        (into (k-comb n rem)
          (map #(conj % i)
            (k-comb (dec n) rem)))))))

(defcheck solution-179a7b72
  (fn [n s]
    (set
      (filter #(= n (count %))
        (reduce
          (fn [a v] (apply conj a (map #(conj % v) a)))
          #{#{}}
          s)))))

(defcheck solution-17d040ad
  (fn kc [n s]
    (cond (= 1 n) (set (map (fn [e] #{e}) s))
          (> n (count s)) #{}
          :else (let [fi (first s)]
                  (set (concat (map #(conj % fi) (kc (dec n) (rest s)))
                         (kc n (rest s))))))))

(defcheck solution-1869388e
  (fn combinations [n s]
    (cond
      (or (zero? n) (> n (count s))) #{}
      (= 1 n) (set (map #(set [%]) s))
      :else (let [tails (fn tails [c] (map #(drop % c) (range (count c))))]
              (set (for [[x & xs] (tails s) c (combinations (dec n) xs)] (conj c x) ))))))

(defcheck solution-1911f290
  #(set (filter (fn [s] (= (count s) %1))
          (reduce (fn [acc x] (into acc (for [s acc] (conj s x))))
            #{#{}}
            %2))))

(defcheck solution-19354de4
  (fn [n s]
    (set (filter #(= (count %) n)
           (reduce
             (fn [i e] (into i (map #(conj % e) i)))
             #{#{}} s)))))

(defcheck solution-19ab1cec
  (fn kc [k s]
    (cond
      (< (count s) k) #{}
      (= k 1) (set (map (comp set vector) s))
      :else (apply clojure.set/union
              (map-indexed (fn [i e]
                             (set (map #(conj % e) (kc (dec k) (concat (take i s) (drop (inc i) s)))))) s)))))

(defcheck solution-19b19137
  (fn [n s]
    (set (loop [m 0 a [#{}]]
           (if (= m n)
             a
             (recur (+ m 1) (filter #(= (+ m 1) (count %))
                              (for [x s y a] (conj y x)))))))))

(defcheck solution-1a7ce641
  (fn [n s]
    (set
      (filter #(= n (count %))
        (reduce
          (fn [r x] (into r (map #(conj % x) r)))
          #{#{}}
          s)))))

(defcheck solution-1ab8dedf
  (fn [n s]
    (->> (#(reduce (fn [sets elem]
                     (reduce (fn [c n]
                               (conj c (conj n elem))) sets sets))
             #{#{}} %) s)
      (filter #(= n (count %)))
      (into #{}))))

(defcheck solution-1ae483c2
  (fn k-comb
    [n coll]
    #_(println n coll)
    (let [cnt (count coll)]
      (cond
        (or (zero? n) (< cnt n))
        #{}

        (> cnt n)
        (let [f (first coll)]
          ;;(println f coll)
          (into (if-let [p1 (seq (k-comb (dec n) (rest coll)))]
                  (reduce #(conj % (conj %2 f)) #{} p1)
                  #{#{f}})
            (k-comb n (rest coll))))

        (= cnt n)
        #{(set coll)}))))

(defcheck solution-1afc6576
  (fn kcs [k S]
    (case k
      0 #{}
      1 (set (map hash-set S))
      (let [subseqs (map #(drop % S) (range (inc (- (count S) k))))
            car-kcs (fn [[x & xs]] (map #(conj % x) (kcs (dec k) xs)))]
        (set (mapcat car-kcs subseqs))))))

(defcheck solution-1b0e0b35
  (fn comb [k s]
    (if (= 1 k) (into #{} (map hash-set s))
                (letfn [(subcombs [x]
                          (let [sub (disj s x)]
                            (let [combs (comb (dec k) sub)]
                              (map #(clojure.set/union #{x} %) combs))))]
                  (into #{} (apply clojure.set/union (map subcombs s)))))))

(defcheck solution-1b1136fc
  (fn [d e] (set  (let [w (fn peu [x] (let [y (apply list x)] (set (if (empty? y) #{#{}} (let [z (apply list (peu (rest y)))] (clojure.set/union z (map #(clojure.set/union % #{(first y)}) z))) ))))] (filter #(= (count %) d) (w e))))))

(defcheck solution-1b9ede18
  (fn combinations [k s]
    (cond
      (> k (count s))
      #{}

      (= k 1)
      (set (map #(conj #{} %) s))

      :else
      (let [[x & xs] (seq s)
            sub-combinations (combinations (dec k) xs)
            with-first (map #(conj % x) sub-combinations)
            without-first (combinations k xs)]
        (->> with-first
          (concat without-first)
          (map set)
          set)))))

(defcheck solution-1ba3bf38
  (fn [c u]
    (set (filter #(= c (count %))
           (reduce (fn [a e]
                     (reduce #(conj % (conj %2 e)) a a))
             #{#{}} u)))))

(defcheck solution-1c1707e7
  (fn k-comb [k coll]
    (if (= k 1)
      (set (map (fn [arg] #{arg})
             coll))
      (set (mapcat (fn [combination]
                     (filter (comp (partial = k) count)
                       (map (partial conj combination)
                         coll)))
             (k-comb (dec k) coll))))))

(defcheck solution-1c22b668
  (fn [k s]
    (letfn [
            (kc [k s]
              (cond
                (= k 1)
                (map
                  (fn [x] #{x})
                  s)
                (empty? s)
                #{}
                :else
                (concat
                  (kc k (rest s))
                  (map
                    #(conj % (first s))
                    (kc (dec k) (rest s))))))]
      (set (kc k s)))))

(defcheck solution-1c2aa616
  (fn k-combinations [n data]
    (letfn [(toggle-every [length n]
              (take (Math/pow 2 length)
                (cycle (concat (repeat n 0) (repeat n 1)))))

            (perms [length]
              (->> (range length)
                (mapv #(toggle-every length (Math/pow 2 %)))
                (apply mapv vector)))

            (zip
              ([xs ys] (zip xs ys #{}))
              ([xs ys acc]
               (if (or (empty? xs) (empty? ys))
                 acc
                 (recur (rest xs)
                   (rest ys)
                   (if (= 1 (first xs))
                     (conj acc (first ys))
                     acc)))))

            (powerset [coll]
              (reduce #(conj %1 (zip %2 coll)) #{} (perms (count coll))))]
      (into #{} (filter #(= n (count %)) (powerset data))))))

(defcheck solution-1c3ce1
  (fn combinations [k s]
    (set
      (filter #(= k (count %))
        (flatten
          (reduce (fn [ss x]
                    (map (fn [a b] (concat a (map #(conj % x) b)))
                      (concat ss [nil]) (concat [nil] ss)))
            [[#{}]] s))))))

(defcheck solution-1cc0a1d5
  (fn k [i s]
    (set
      (if (= i 0)
        [#{}]
        (mapcat #(for [p (k (- i 1) %2)] (conj p %))
          s (next (iterate next s)))))))

(defcheck solution-1de4f91a
  (fn [n s]
    (into
      #{}
      (filter
        #(= n (count %))
        (reduce
          (fn [m v]
            (into m
              (map #(conj % v) m)))
          #{#{}}
          s)))))

(defcheck solution-1e0d68f3
  (fn comb [n s]
    (if (zero? n) #{#{}}
                  (into #{} ((fn f [l]
                               (when (seq l)
                                 (into (map #(conj % (first l)) (comb (dec n) (rest l)))
                                   (f (rest l))))) s)))))

(defcheck solution-1e3b7d07
  (fn c [k S]
    (if (zero? k)
      #{#{}}
      (set
        (for [S (iterate next (seq S))
              :while S
              comb (c (dec k) (rest S))]
          (conj comb (first S)))))))

(defcheck solution-1ec4abcf
  (fn [k coll]
    (loop [subsets (set (map hash-set coll))
           iter 1]
      (if (> iter k)
        (set (remove #(not= (count %) k) subsets))
        (recur (set (remove #(< (count %) iter) (for [x subsets y subsets] (apply conj x y))))
          (inc iter))))))

(defcheck solution-1f323645
  (fn k-combinations [n s] (set (filter #(= n (count %)) (set (map set ((fn p [s] (if (empty? s) '(()) (clojure.set/union (p (next s)) (map #(conj % (first s)) (p (next s)))))) s)))))))

(defcheck solution-1fd1401c
  (fn combinations [n coll]
    (cond
      (> n (count coll)) #{}
      (= n (count coll)) #{(set coll)}
      (= n 1) (set (map #(set [%]) coll))
      :else
      (set (concat (map #(conj % (first coll)) (combinations (dec n) (rest coll)))
             (combinations n (rest coll)))))))

(defcheck solution-207e7ac0
  (fn choose [k s]
    (cond (< (count s) k) #{}
          (= (count s) k) #{s}
          :else (set (mapcat #(choose k (disj s %)) s)))))

(defcheck solution-20aa90fb
  (fn [k s]
    (if (> k (count s)) #{}
                        (loop [s1 (set (map (comp set list) s))]
                          (let [s2 (filter #(= k (count %)) s1)]
                            (if (< 0 (count s2))
                              (set s2)
                              (recur
                                (mapcat (fn [s2] (map #(conj s2 %) s)) s1)))))
                        )
    ))

(defcheck solution-20e969bd
  (fn [n s]
    (let [all (fn c1 [n s]
                (cond
                  (zero? n) #{}
                  (= 1 n) (reduce #(conj %1 #{%2}) #{} s)
                  :else (reduce (fn [acc x] (into acc (map #(into x %) (c1 1 s))))
                          #{}
                          (c1 (dec n) s))))]
      (set (filter #(= n (count %)) (all n s))))))

(defcheck solution-21e2c8d
  (fn [k s]
    (set
      (loop [k k, ss (map hash-set s)]
        (if (<= k 1)
          ss
          (recur (dec k)
            (for [en ss, e0 s :when (not (some #{e0} en))]
              (conj en e0))))))))

(defcheck solution-22b2b68d
  (fn [n coll]
    (set (filter #(= (count %) n)
           (reduce (fn [a x]
                     (->> (map #(set (concat #{x} %)) a)
                       (concat a) set))
             #{#{}} coll)))))

(defcheck solution-236cb1d4
  (fn k-comb
    ([n xs] (k-comb n xs #{}))
    ([n xs s]
     (if (zero? n) #{s}
                   (if (empty? xs) #{}
                                   (clojure.set/union
                                     (k-comb (dec n) (rest xs) (conj s (first xs)))
                                     (k-comb n (rest xs) s)))))))

(defcheck solution-2373663
  (fn [n s]
    (letfn [(ic [n cnt]
              (lazy-seq
                (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
                      iter-comb
                        (fn iter-comb [c j]
                          (if (> j n) nil
                                      (let [c (assoc c j (dec (c j)))]
                                        (if (< (c j) j) [c (inc j)]
                                                        (loop [c c, j j]
                                                          (if (= j 1) [c j]
                                                                      (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
                      step
                        (fn step [c j]
                          (cons (rseq (subvec c 1 (inc n)))
                            (lazy-seq (let [next-step (iter-comb c j)]
                                        (when next-step (step (next-step 0) (next-step 1)))))))]
                  (step c 1))))]
      (let [v-items (vec (reverse s))]
        (if (zero? n) (list ())
                      (let [cnt (count s)]
                        (cond (> n cnt) #{}
                              (= n cnt) #{(set (seq s))}
                              :else (set (map #(set (map v-items %)) (ic n cnt))))))))))

(defcheck solution-23b9fd0b
  (fn f [n col]
    (->>
      ((fn f' [acc n col]
         (let [i (first col), r (rest col)]
           (if (or (nil? i) (zero? n)) ; n==0
             acc
             (clojure.set/union (f' (map #(conj (set %) i) acc) (dec n) r)
               (f' acc n r)))))
       #{#{}} n col)
      (filter #(= n (count %)))
      (set))))

(defcheck solution-23c422d
  (fn [n sq]

    (if (> n (count sq) ) #{}

                          (set (filter #(= (count %) n)

                                 (loop [i 0
                                        o []
                                        ]

                                   (let [m  ((fn [n bse x]
                                               (map #(rem (quot n %) bse) (map #(int (Math/pow bse %)) (range x)))
                                               ) i (count sq) n ) ]

                                     (if (= i (int (Math/pow (count sq) n))) o
                                                                             (recur (inc i)  (cons  (set (map #(nth (into [] sq) %) m )) o )))))

                                 )))

    ))

(defcheck solution-2438c524
  (fn [n s]
    (letfn [(f [s]
              (if (empty? s) #{s}
                             (let [r (f (rest s))]
                               (into r
                                 (map #(conj % (first s)) r)))))]
      (set (map set (filter #(= n (count %)) (f s)))))))

(defcheck solution-2441039e
  (fn ! [n st]
    (cond (> n (count st)) #{}
          (= n 1) (set (map #((comp set list) %) st))
          :else (set (flatten (for [el st]
                                (map #(conj % el) (! (dec n) (disj st el)))))))))

(defcheck solution-24590036
  (fn comb [k s]
    (comment "obviously ridiculously inefficient, but I already had
     permutations written.")
    (letfn [(rm [s i] (concat (take i s) (drop (inc i) s)))
            (permutations [s]
              (condp = (count s)
                0 ()
                1 (list s)
                (mapcat (fn [i x] (map #(cons x %) (permutations (rm s i))))
                  (range (count s)) s)))]
      (if (not (<= 1 k (count s)))
        #{}
        (set (map #(set (take k %)) (permutations s)))))))

(defcheck solution-24836aa9
  (fn [n s]
    (let [split-all (fn split-all [n src base result]
                      (cond (> n (count src)) result
                            (= 0 n) (conj result base)
                            :else (split-all (dec n) (rest src) (conj base (first src)) (split-all n (rest src) base result))))]
      (split-all n s #{} #{}))))

(defcheck solution-25238599
  (fn [n xs]
    (let [start  (partition 1 xs)
          func2  #(concat %1 [%2])
          func1  #(map func2 (repeat %) xs)
          iter   (iterate #(mapcat func1 %) start)
          iter-n (last (take n iter))
          no-eq  (filter #(apply distinct? %) iter-n)
          in-set (set (map set no-eq))]
      in-set)))

(defcheck solution-253fd264
  (fn [n x]
    (let [power-set
          (fn inner [x]
            (if (empty? x) #{#{}}
                           (let [half (inner (rest x))]
                             (into half (map #(conj % (first x)) half)))))]
      (set (filter #(-> % count (= n)) (power-set x))))))

(defcheck solution-25f44b24
  (fn k-com [n s]
    (letfn [(power-set [s]
              (letfn [(one-less [s] (map #(disj s %) s))
                      (power-set [s] (map one-less (set (flatten s))))]
                (set (flatten (take (inc (count s)) (iterate power-set [[s]]))))))]
      (if (> n (count s)) #{}
                          (set (filter #(= n (count %)) (power-set s)))))))

(defcheck solution-26706c33
  (fn [k s]
    (letfn [(komb [n s]
              (if (= 1 n) (map hash-set s)
                          (reduce #(apply conj %1 (map (fn [el] (apply conj #{%2} el)) (komb (dec n) (disj s %2)))) #{} s)
                          ))]
      (if (< (count s) k) #{}
                          (set (komb k s))
                          )
      )
    ))

(defcheck solution-268f71c4
  (fn __ [k s]
    (set (cond
           (= k 0)
           #{#{}}
           (= (count s) 0)
           #{}
           (< (count s) k)
           #{}
           (= (count s) k)
           #{s}
           :else
           (into
             (map #(conj % (first s)) (__ (dec k) (set (rest s))))
             (__ k (set (rest s))))))))

(defcheck solution-269f9842
  #(set (map set
          (partition %
            ((fn f [v r n]
               (if (< (count v) n)
                 []
                 (if (= 0 n)
                   r
                   (concat
                     (f (rest v) (concat r [(first v)]) (- n 1))
                     (f (rest v) r n))))) (vec %2) [] %)))))

(defcheck solution-26c01020
  (fn [n items]
    (let [
          comb (fn comb [n items]
                 (if (> n (count items))
                   '()
                   (if (= n 0)
                     '(())
                     (if (= n 1)
                       (map vector items)
                       (let [[head & tail] items]
                         (concat
                           (map #(cons head %) (comb (dec n) tail))
                           (comb n tail)))))))]
      (set (map set (comb n (seq items)))))))

(defcheck solution-276ed747
  (fn k-combos
    [k S]

    (cond
      (<= k 0) #{}
      (> k (count S)) #{}
      (= k 1) (set (map #(set %1) (partition 1 S)))
      :else (into #{} (for [rem-S (take (count S) (iterate rest S))
                            :let [hed (first rem-S)]
                            tal (k-combos (dec k) (rest rem-S))]
                        (set (cons hed tal)))))))

(defcheck solution-27842c5f
  (fn [n s]
    (if (< (count s) n)
      #{}
      (loop [i n r #{#{}}]
        (if (= i 0)
          r
          (recur (dec i) (set (mapcat (fn [x] (map #(conj x %) (apply disj s x))) r))))))))

(defcheck solution-281ffbb3
  (fn[n a-seq]
    (letfn [(power-set[a-set]
              (let [p-seq (atom [])
                    ll    (atom [])]
                (swap! p-seq conj a-set)
                (loop [ps @p-seq]
                  (doseq [seq-elem ps]
                    (doseq [elem seq-elem]
                      (reset! ll (set [elem]))
                      (when (not (some #(= % @ll) @p-seq))
                        (swap! p-seq conj @ll))
                      (reset! ll (set (remove #(= elem %) seq-elem)))
                      (when (not (some #(= % @ll) @p-seq))
                        (swap! p-seq conj @ll))))
                  (when (not= (count ps) (count @p-seq))
                    (recur @p-seq)))
                (set (swap! p-seq conj #{}))))]
      (let [s (set a-seq)]
        (set (filter #(= (count %) n) (power-set a-seq)))))))

(defcheck solution-28a4ae6c
  (fn [x s]
    (loop [n 0
           a #{#{}}]
      (if (= n x)
        a
        (recur (+ n 1)
          (into #{}
            (for [i a j s :when (not (i j))]
              (conj i j))))))))

(defcheck solution-28b328e3
  (fn comb [k s]
    (cond (zero? k) #{#{}}
          (empty? s) #{}
          :else (set (concat (map #(conj % (first s)) (comb (dec k) (rest s)))
                       (comb k (rest s)))))))

(defcheck solution-28cda992
  (fn [n b]
    (letfn
     [
      (combine [x y]
        (let [z (into [] y)]
          (loop [i 0 result #{}]
            #_(println "X:" x "Y[i]" (get z i) "Result: " result )
            (cond
              (>= i (count z)) result
              (not (contains? x (get z i)))   (recur (inc i) (conj result (conj x (get z i))))
              :else   (recur (inc i) result)
              )
            )
          )
        )

      (combinen [a b]
        (let [z (into [] a)]
          (loop [i 0 result #{}]
            (cond
              (>= i (count a)) result
              :else (recur (inc i) (into result (combine (get z i) b)))
              )
            )
          )
        )

      (combinenn [a b c]
        (loop [i c result a]
          (cond
            (= i 1) result
            :else (recur (dec i) (combinen result b))
            )
          )
        )
      ]
      (let [ a (map (fn [x] #{x})  b)]
        (into #{} (combinenn a b n))
        )
      )
    ))

(defcheck solution-28dea10e
  (fn number103  [n s]
    (->> s
      (reduce (fn [m x]
                (reduce #(conj % (conj %2 x)) m m))
        #{#{}})
      (filter #(= (count %) n))
      (into #{}))))

(defcheck solution-291e4f78
  (fn k-combinations [k s]
    (cond (zero? k) #{#{}}
          (> k (count s)) #{}
          :else (into #{}
                  (for [comb (k-combinations (dec k) s)
                        v s
                        :when (not (comb v))]
                    (conj comb v))))))

(defcheck solution-292bf510
  (fn f
    ([n xs] (f n xs #{}))
    ([n xs s]
     (let [t (rest xs)]
       (cond
         (= n (count s)) #{s}
         (empty? xs) #{}
         :else (set
                 (concat
                   (f n t (conj s (first xs)))
                   (f n t s))))))))

(defcheck solution-2977b96a
  (fn [n s]
    (let [p (fn powerset [s]
              (if (empty? s)
                '(())
                (concat (powerset (rest s))
                  (map #(conj % (first s)) (powerset (rest s))))))]
      (set (filter #(= n (count %)) (map set(p s)))))))

(defcheck solution-29f70c78
  (fn [k x] (->>
              x
              ((fn p [ls]
                 (if (empty? ls) [#{}]
                                 (clojure.set/union (p (next ls))
                                   (map #(conj % (first ls)) (p (next ls)))))))
              (filter #(= (count %) k)) (set))))

(defcheck solution-2a8c7e1d
  (fn [n s]
    (letfn [(power-set [s]
              (reduce (fn [ps x]
                        (into ps (map #(conj % x) ps)))
                #{#{}} s))]
      (->> s
        power-set
        (filter #(= n (count %)))
        set))))

(defcheck solution-2ab7bc01
  (fn kcomb [m C]
    (let [ss (fn kc [n S]
               (if (> (count S) n) (for [x S] (kc n (disj S x))) S))
          set-vals (range (count C))
          set-keys (vec C)
          smap (zipmap set-keys set-vals)
          rsmap (zipmap set-vals set-keys)]
      (cond (> m (count C)) #{}
            (> (count C) m) (set (map set (map #(replace rsmap %) (set (flatten (ss m (set (replace smap C))))))))
            :else #{C}))))

(defcheck solution-2afccb04
  (fn [k s]
    (letfn [(f [k s]
              (cond
                (zero? k) '(())
                (empty? s) ()
                :else (concat (f k (rest s))
                        (map #(cons (first s) %) (f (dec k) (rest s))))))]
      (set (map set (f k (seq s)))))))

(defcheck solution-2bb24c9f
  (fn [no xset]
    (loop [n no res #{#{}}]
      (if (zero? n)
        (into #{} (filter #(= (count %) no) res))
        (recur (dec n) (set (for [p1 xset p2 res] (conj p2 p1))))))
    ))

(defcheck solution-2bc4230a
  (fn k-comb [n s]
    (cond
      (> n (count s)) #{}
      (empty? s)      #{}
      (= 1 n)         (set (map #(set (list %)) s))
      :else           (let [x    (first s)
                            ys   (k-comb n (rest s))
                            ys-1 (k-comb (dec n) (rest s))]
                        (set (concat ys (set (map #(conj % x) ys-1))))))))

(defcheck solution-2bed322c
  (fn f [k s]
    (if (= k 1)
      (set (map #(set [%]) s))
      (set (mapcat (fn [item]
                     (let [subs (f (dec k) (disj s item))]
                       (set (map #(conj % item) subs))))
             s)))))

(defcheck solution-2c7d5ff
  (fn k-comb [n s]
    (if (or (zero? n)
            (empty? s))
      #{#{}}
      (->> (concat (map #(conj % (first s))  (k-comb (dec n) (rest s)))
             (k-comb n (rest s)))
        (filter #(= (count %) n))
        set))))

(defcheck solution-2ca62c7c
  (fn [k is]
    (let [pset (fn [is]
                 (reduce
                   (fn [ps i]
                     (reduce
                       (fn [ps p]
                         (conj ps (conj p i)))
                       ps ps))
                   #{#{}} is))]
      (set (filter
             #(= (count %) k)
             (pset is))))))

(defcheck solution-2cbfd0b9
  (fn f [k s] (cond (> k (count s)) #{}
                    (= 1 k) (set (for [i s] #{i}))
                    (= k (count s)) #{(set s)}
                    :else (clojure.set/union (f k (rest s)) (set (map #(conj % (first s)) (f (dec k) (rest s)))) ))))

(defcheck solution-2d1880ea
  (fn kc [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (let [s1 (first s) rs (disj s s1)]
              (clojure.set/union
                (kc k rs)
                (set (map #(conj % s1) (kc (dec k) rs))))))))

(defcheck solution-2d4a28e9
  (fn [n s] (let [comb (fn k-combinations [k coll]
                         (if (= 1 k)
                           (map hash-set coll)
                           (loop [f (first coll) r (next coll)
                                  conj-ed (apply hash-set (map #(into #{f} %) (k-combinations (dec k) r) ))]

                             (if (seq r)
                               (recur (first r)
                                 (next r)
                                 (into conj-ed (map #(into #{(first r)} %) (k-combinations (dec k) (next r)) ) ))
                               conj-ed))))] (apply hash-set (comb n s)))))

(defcheck solution-2dad939
  (fn k [n es]
    (letfn [(combs [es]
              (if (empty? es)
                #{#{}}
                (let [t (combs (rest es))]
                  (clojure.set/union (apply hash-set (map #(conj % (first es)) t)) t))))]
      (apply hash-set (filter #(= n (count %)) (combs es))))))

(defcheck solution-2dcc7880
  (fn my-k-combinations
    [k s]
    (loop [result (map #(hash-set %) s) i 0]
      (if (or (= i k) (> i k))
        (set (filter #(= (count %) k) result))
        (recur (set (mapcat (fn [x] (map #(conj x %) s)) result)) (inc i))))))

(defcheck solution-2f2b626
  (fn final-fn [k coll]
    (let [unselected (fn [coll sel]
                       (clojure.set/difference coll sel))
          start (fn [k coll]
                  {:selected (into #{} (map #(hash-set %) coll))
                   :coll coll
                   :n-to-go (dec k)})
          step (fn [{:keys [selected coll n-to-go] :as m}]
                 (let [unsel (seq (unselected coll selected))
                       vals-to-add (map #(nth unsel %)
                                     (range (count unsel)))
                       new-selected (into #{}
                                      (for [s selected
                                            :let [unsel (unselected coll s)]
                                            x unsel]
                                        (conj s x)))]
                   (assoc m
                     :selected new-selected
                     :n-to-go (dec n-to-go))))
          end? (fn [m] (zero? (:n-to-go m)))
          end (fn [m] (:selected m))
          repeat-until-true (fn [f pred m]
                              (let [end? (pred m)]
                                (if end?
                                  m
                                  (recur f pred (f m)))))]
      (end (repeat-until-true
             step
             end?
             (start k coll))))))

(defcheck solution-2f5d4c49
  (fn [k s]
    (set (filter #(= k (count %)) (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) #{#{}} s)))))

(defcheck solution-2f763db1
  (fn [k s]
    (into #{} (filter #(= (count %) k)
                (for [x (range (Math/pow 2 (count s)))]
                  (into #{}
                    (keep-indexed #(if (bit-test x %1) %2) s)))))))

(defcheck solution-2fcb7901
  (fn c[n s]
    (cond
      (> n (count s)) #{}
      (= n 0) #{{}}
      :else
      (set
        (reduce into
          (for [x (range (inc (- (count s) n))) :let [y (drop x s)]]
            (for [z (c (dec n) (rest y))]
              (into #{(first y)} z))))))))

(defcheck solution-2ff8beab
  (fn [k s]
    (if (> k (count s)) #{}
                        (loop [i (count s)
                               combs #{s}]
                          (if (= i k) combs
                                      (recur (dec i)
                                        (set
                                          (for [c combs
                                                e c]
                                            (disj c e)))))))))

(defcheck solution-302e310f
  (fn k-combinations [k s]
    (if (> k (count s))
      #{}
      (if (= k 1)
        (into #{} (map hash-set s))
        (let [[h & t] (seq s)]
          (into
            #{}
            (concat
              (k-combinations k t)
              (map
                #(into % (list h))
                (k-combinations (dec k) t)))))))))

(defcheck solution-308d1f5a
  (fn k-comb [n s]
    (cond
      (or (zero? n) (zero? (count s))) #{}
      (= 1 n) (into #{} (map #(set [%]) s))
      :else
      (let [f (first s)
            r (rest s)]
        (clojure.set/union
          (k-comb n r)
          (into #{} (map #(conj %1 f) (k-comb (- n 1) r))))))))

(defcheck solution-310f8f93
  (fn [n s]
    (reduce
      (fn [r _]
        (set
          (mapcat
            (fn [x]
              (map
                #(conj x %)
                (clojure.set/difference s x)))
            r
            )))
      (set (map hash-set s))
      (range (dec n)))
    ))

(defcheck solution-3177b4ad
  (fn combinations [k s]
    (cond (zero? k) #{#{}}
          (empty? s) #{}
          :else (into (combinations k (rest s))
                  (map #(conj % (first s))
                    (combinations (dec k) (rest s)))))))

(defcheck solution-31a7ec05
  (fn k-comb [k S]
    (if
     (zero? k) #{#{}}
               (set
                 (mapcat
                   (fn [e]
                     (map
                       (fn [t] (conj t e))
                       (k-comb (dec k) (disj S e))))
                   S)))))

(defcheck solution-32164202
  (fn kcombin [n xs]
    (cond
      (> n (count xs)) #{}
      (= n 0) #{}
      (= n 1) (set (map hash-set xs))
      :default
      (loop [ts xs zs #{}]
        (if (empty? ts)
          (set zs)
          (recur (rest ts) (clojure.set/union zs (filter #(= n (count %)) (map #(conj % (first ts)) (kcombin (dec n) xs)))))

          )))
    ))

(defcheck solution-329e9480
  (fn kc [n s]
    (let [[s xs] (split-at n s) n (range n)]
      (if (= (count s) (count n))
        (loop [s [(vec s)] [x & xs] xs]
          (if x (recur (concat s (mapcat (fn [s](map #(assoc s % x) n)) s )) xs) (set (map set s))))#{}))))

(defcheck solution-32ff6756
  (fn k-comb [n st]
    (let [gen-next
                    (fn [p-sets n-set]
                      (into #{} (for [p p-sets n n-set
                                      :when (not (contains? p n))]
                                  (conj p n))))
          st-of-sts (map (fn [n] #{n}) st)]
      (loop [i 1 out-set st-of-sts]
        (if (= i n) (into #{} out-set)
                    (recur (inc i) (gen-next out-set st)))))))

(defcheck solution-334cf5df
  (fn C [n s]
    (into #{}
      (if (= n 1)
        (map hash-set s)
        (mapcat
          (fn [N]
            (for [n s :when (not (contains? N n))] (conj N n)))
          (C (dec n) s))))))

(defcheck solution-335ee5fa
  (fn kCombinationK [k s]
    (letfn [
            (removeI [v n]
              (vec (concat (subvec v 0 n) (subvec v (inc n))))
              )
            (kCombinationRec [k v]
              (if (or (empty? v) (zero? k))
                [#{}]
                (let [
                      indexes (range 0 (count v))
                      newK (dec k)
                      ]
                  (mapcat (fn[i] (map #(conj % (get v i)) (kCombinationRec newK (removeI v i)) )) indexes)
                  )
                )
              )
            ]
      (if (> k (count s))
        #{}
        (set (kCombinationRec k (vec s)))
        )
      )
    ))

(defcheck solution-336d5dce
  (fn [k s]
    (letfn [(soln [k s]
              (if (= k 1)
                (partition 1 s)
                (if-let [[f & s] (seq s)]
                  (concat
                    (map #(conj % f) (soln (dec k) s))
                    (soln k s)))))]
      (set (map set (soln k s))))))

(defcheck solution-33705194
  (letfn [(! [n]
            (reduce
              *
              (range 1 (inc n))))]
    (fn [k S]
      (let [n (count S)
            bc (quot (! n)
                 (* (! k)
                   (! (- n k))))]
        (into #{}
          (take bc
            (distinct
              (map (comp set
                     (partial take k))
                (iterate shuffle
                  S)))))))))

(defcheck solution-3388b3bd
  (fn k-combs [k s]
    (letfn [(comb-rec [c rest root s]
              (if (not= c 0)
                (for [x (range rest (inc (- s c)))]
                  (comb-rec (- c 1) (inc x) (conj root x) s))
                root))]
      (let [v (vec s)]
        (set (map (fn [x] (set (map (fn [y]  (v y)) x)))
               (partition k (flatten (comb-rec k 0 [] (count v))))))))))

(defcheck solution-33d5630f
  (letfn [(ps [xs]
            (if (empty? xs)
              #{#{}}
              (loop [now (first xs)
                     left []
                     right (vec (rest xs))
                     so-far #{(set xs)}]
                (if (empty? right)
                  (conj so-far (set left))
                  (recur (first right)
                    (into left [now])
                    (rest right)
                    (into so-far (ps (into left right))))))))
          (kc [k xs] (set (filter #(->> % count (= k)) (ps xs))))]
    kc))

(defcheck solution-352987f3
  (fn generate-k-combinations [k coll]
    (letfn [(power-set [s]  (let [first (first s) subset (rest s)]
                              (if (empty? s) #{#{}}
                                             (clojure.set/union
                                               (into #{} (map #(clojure.set/union #{first} %) (power-set subset)))
                                               (power-set subset)))))]
      (set (filter #(= k (count %)) (power-set coll))))))

(defcheck solution-3537732d
  (fn k-combos [n xs]
    (into #{}
      (if (< 1 n)
        (for [x xs
              y (k-combos (dec n) (disj xs x))]
          (into #{} (cons x y)))
        (for [x xs] #{x})))))

(defcheck solution-35402c62
  (fn mycomb [k coll]
    (letfn [(kcomb [c]
              (let [coll (vec c)]
                (if (> k (count coll)) #{}
                                       (if (= k (count c)) (set c)
                                                           (flatten (for [i (range (count coll))]
                                                                      (kcomb (concat (subvec coll 0 i) (subvec coll (inc i))))))))))]
      (if (= (count coll) k) (set [coll])
                             (set (kcomb coll))))))

(defcheck solution-355d4cc9
  (fn __
    ([c n] (let [
                 res (__ c n 1)
                 ]
             (if (set? res) #{res} (set res))))
    ([c n jed] (if (= c (count n))
                 n
                 (flatten (map (fn [x] (__ c (disj n x) 1) ) n)) ))
    ))

(defcheck solution-358058ef
  (fn [n s]
    (if (> n (count s))
      #{}
      (let [perm (fn [s]
                   (let [helper (fn helper [curr left]
                                  (if (empty? left)
                                    [curr]
                                    (mapcat (fn [e] (helper (cons e curr) (remove #(= e %) left))) left)))]
                     (helper '() s)))]
        (set (map #(set (take n %)) (perm s)))))))

(defcheck solution-359faed6
  (fn k-combinations
    [k s]
    (letfn [(power-set [s]
              (let [s-count (count s)
                    ps-count (bit-shift-left 1 s-count)
                    elements (vec s)]
                (into #{} (for [n (range 0 ps-count)]
                            (->> (range 0 s-count)
                              (remove #(zero? (bit-and n (bit-shift-left 1 %))))
                              (map elements)
                              set)))))]
      (set (filter #(= (count %) k) (power-set s))))))

(defcheck solution-367fa3df
  (fn [k s]
    (loop [acc #{#{}} cnt 1]
      (if (> cnt k)
        (->> acc (filter #(= k (count %))) set)
        (recur (apply clojure.set/union
                 (map (fn [a] (map #(conj a %) s)) acc))
          (inc cnt))))))

(defcheck solution-37b632c6
  (fn fun [n coll]
    (cond
      (zero? n) #{#{}}
      (zero? (count coll)) #{}
      :else
      (clojure.set/union
        (reduce
          #(conj % (conj %2 (first coll)))
          #{}
          (fun (dec n) (rest coll)))
        (fun n (rest coll))))))

(defcheck solution-37b98298
  (fn [n s]
    (loop [c (into #{} (for [x s] #{x}))
           n n]
      (if (> n 1)
        (recur
          (into #{} (for [x c y s :when (not (x y))]
                      (conj x y)))
          (dec n))
        c))))

(defcheck solution-3866db37
  (fn [k s]
    (let [power-set (reduce #(into %1 (for [e %1] (conj e %2))) #{#{}} s)]
      (set (filter #(= k (count %)) power-set)))))

(defcheck solution-38ab97dc
  (fn __ [n s]
    (cond
      (< (count s) n) #{}
      (= n (count s)) #{s}
      :else (letfn [(itr [s cnt]
                      (cond
                        (= (- n (count s)) cnt) #{(set s)}
                        (= cnt n) #{#{}}
                        :else (concat
                                (map #(conj % (first s)) (itr (rest s) (inc cnt)))
                                (when-not (empty? (rest s))
                                  (itr (rest s) cnt)))))]
              (set (itr s 0))))))

(defcheck solution-38fffdfe
  (fn iter [k s]
    (if (empty? s)
      #{}
      (if (= k 1)
        (into #{} (for [x s] #{x}))
        (clojure.set/union
          (into #{} (map #(conj %1 (first s)) (iter (dec k) (rest s))))
          (iter k (rest s)))))))

(defcheck solution-39f03794
  (fn kcomb [k xs]
    (if (= k 0)
      #{#{}}
      (if (empty? xs)
        #{}
        (into #{} (concat (map #(conj % (first xs))
                            (kcomb (dec k) (into #{} (rest xs))))
                    (kcomb k (into #{} (rest xs)))))))))

(defcheck solution-3a36ea37
  (fn [n st]
    (set (remove #(not= (count %) n)
           (loop [s st result #{#{}}]
             (if (empty? s) result
                            (recur (rest s) (reduce conj result (map #(conj % (first s)) result)))))))))

(defcheck solution-3a4b07d7
  (fn k-combi
    [n coll]
    (loop [i n
           c (map vector coll)]
      (if (zero? i)
        (set (filter #(= (count %) n) c))
        (recur (dec i) (into #{} (map set (for [x coll
                                                y c]
                                            (conj y x)))))))))

(defcheck solution-3a54b91b
  (fn k-combinations [k st]
    (cond
      (> k (count st)) #{}
      (= 1 k) (set (map hash-set st))
      :else
      (set
        (for [s st
              subst (k-combinations (dec k) st)
              :let [nsubst (conj subst s)]
              :when (= (count nsubst) k)]
          nsubst)))))

(defcheck solution-3a983216
  (fn [! k s] (set (filter #(= k (count %)) (! s)))) (fn ![s] (conj (mapcat #(! (set (remove #{%} s))) s) s)))

(defcheck solution-3b7a54c3
  (fn [n s]
    (let [n (- (count s) n)
          f (fn [x] (map #(disj x %) x))]
      (if (neg? n) #{}
                   (set (nth (iterate #(distinct (mapcat f %)) [s]) n))))))

(defcheck solution-3bab3071
  (fn [n s]
    (if (< (count s) n)
      #{}
      (letfn [(f [s c] (if (and (seq s) (< (count c) n))
                         (map #(f (disj s %) (conj c %)) s)
                         (set c)))]
        (set (flatten (f s [])))))))

(defcheck solution-3bec0915
  (fn k-comb [k S]
    (cond
      (> k (count S)) #{}
      (= k 1) (set (map (comp set list) S))
      :else (let [n (first S)
                  S-prime (->> S rest set)]
              (clojure.set/union
                (k-comb k S-prime)
                (set (map #(conj % n)
                       (k-comb (dec k)
                         S-prime))))))))

(defcheck solution-3bfd3f
  (fn [k s]
    (letfn [(powerset [s]
              (reduce (fn [ps x]
                        (reduce (fn [ps s]
                                  (conj ps (conj s x))) ps ps)) #{#{}} s))]
      (set (filter #(= k (count %)) (powerset s))))))

(defcheck solution-3c07c926
  (fn k [n,s]
    (cond (zero? n) #{(set '())}
          (> n (count s)) #{}
          (= n (count s)) #{(set s)}
          :else (set (concat
                       (map #(conj % (first s)) (k (dec n) (rest s)))
                       (k n (rest s)))))))

(defcheck solution-3ca38de2
  (fn comb [n s]
    (if (zero? n)
      #{#{}}
      (set (for [[x & xs] (take (count (vec s)) (iterate rest (vec s)))
                 c (comb (dec n) xs)] (conj c x))))))

(defcheck solution-3ccbfa81
  (letfn [(k-combinations
            [k s]
            (cond
              (= k 0) '(())
              (empty? s) '()
              :else (concat (map
                              #(cons (first s) %)
                              (k-combinations (dec k) (rest s)))
                      (k-combinations k (rest s)))))]
    (fn [k s]
      (set (map set (k-combinations k s))))))

(defcheck solution-3ce21e89
  (fn [n s]
    (letfn [(ps [s]
              (if (empty? s)
                #{#{}}
                (let [ps-of-rest (ps (rest s))
                      ps-with-missing (apply hash-set (map #(set (cons (first s) %)) ps-of-rest))]
                  (clojure.set/union ps-of-rest ps-with-missing))))]
      (set (filter #(= n (count %)) (seq (ps s)))))))

(defcheck solution-3d276594
  (fn [n s] (let [ power (fn go [s]
                           (if (empty? s) #{#{}}
                                          (let [pt (go (rest s))
                                                ft (set (map #(conj % (first s)) pt))]
                                            (clojure.set/union ft pt))))]
              (into #{} (filter #(= n (count %)) (power s))))))

(defcheck solution-3d5f663e
  (fn f [k s]
    (cond (< k 0) #{}
          (= k 0) #{#{}}
          (empty? s) #{}
          9 (into (f k (rest s))
              (for [t (f (dec k) (rest s))]
                (conj t (first s)))))))

(defcheck solution-3d6cb14a
  (fn k-combinations [k s]
    (let [power-set (fn [s]
                      (reduce (fn [s v] (concat s (map #(conj % v) s))) [#{}] s))]
      (set (filter #(= (count %) k) (power-set s))))))

(defcheck solution-3d81f6d1
  (fn [n s]
    (letfn [(zzz [n s]
              #_(println n s)
              (cond
                (= 1 n) (map #(conj #{} %) s)
                (seq s) (let [[f & r] s]
                          (concat
                            (map (fn [x] (conj x f)) (zzz (dec n) r))
                            (if (> n (count r)) [] (zzz n r))
                            ))
                :else nil))]
      (into #{} (zzz n (seq s))))))

(defcheck solution-3da5baba
  (fn [k s]
    (loop [s s
           r #{#{}}]
      (if (seq s)
        (recur (rest s) (into r (map #(conj % (first s)) r)))
        (set (filter #(= k (count %)) r))))))

(defcheck solution-3e0341b7
  (fn group_ [num setparam]
    (let[diff (fn[colSub col]
                (if (seq? colSub)
                  (reduce #(disj %1 %2)  col colSub)
                  (disj col colSub)
                  )
                ),
         expend (fn[result col]
                  (apply conj result
                    (map #(diff % col) col)
                    )
                  ),
         gener (fn[colp]
                 #_(println colp)
                 (loop[col colp,result #{}]
                   (if(empty? col)
                     result
                     (recur (rest col)
                       (expend result (first col) ) )
                     )
                   )
                 ),
         length (count setparam),
         sub (- length num),
         param #{setparam}]
      (cond (< sub 0) #{}
            (= sub 0) param
            :else
            (nth (iterate gener param) sub)
            )

      )
    ))

(defcheck solution-3e3493f2
  (letfn [(cr [c r n s S]
            (if (zero? c) n
                          (map #(cr (dec c) (inc %) (conj n (nth (vec S) %)) s S) (range r (inc (- s c))))))]
    #(set (flatten (cr % 0 #{} (count %2) %2)))))

(defcheck solution-3eed415b
  (fn kcomb [n coll]
    (cond
      (= n 0) #{#{}}
      (> n (count coll)) #{}
      true (apply hash-set (clojure.set/union
                             (kcomb n (rest coll))
                             (map #(conj % (first coll)) (kcomb (dec n) (rest coll))))))))

(defcheck solution-3f0faf45
  (fn [K S]
    (letfn [(kcom [k s]
              (cond (< (count s) k) #{}
                    (= 1 k)  (apply hash-set (for [e s] #{e}))
                    :else   (apply clojure.set/union  (for [e s]   (apply hash-set (map #(clojure.set/union % #{e}) (kcom (dec k) (disj s e)))) ))))]
      (kcom K S))

    ))

(defcheck solution-3fd90b05
  (fn k-combination [n s]
    (cond
      (or (<= n 0) (< (count s) n)) #{}
      (= n (count s)) #{s}
      (= 1 n) (set (map hash-set s))
      :else (let [fe (first s), rest (disj s fe)]
              (clojure.set/union
                ;; &#19981;&#21253;&#21547;&#31532;&#19968;&#20010;&#20803;&#32032;&#30340; &#24182; &#21253;&#21547;&#31532;&#19968;&#20010;&#20803;&#32032;&#30340;
                (k-combination n rest)
                (set (map #(conj % fe)
                       (k-combination (dec n) rest)))
                )))))

(defcheck solution-403fd20
  (fn [x y] (set (filter #(= (count %) x) (reduce (fn [acc e] (into acc (map #(conj % e) acc))) #{#{}} y)))))

(defcheck solution-40a18b93
  (fn f [n s] (cond (zero? n) #{#{}}
                    (> n (count s)) #{}
                    (= n (count s)) #{s}
                    :else (into (f n (set (rest s)))
                            (map #(conj % (first s)) (f (dec n) (rest s)))))))

(defcheck solution-40c17daf
  (fn k-comb [k s]
    (letfn [(count-ones [s] (apply + s))
            (max-bin [size] (repeat size 1))
            (to-bin [num]
              (loop [n num
                     acc []]
                (if (= n 0)
                  acc
                  (recur (quot n 2) (cons (mod n 2) acc)))))
            (from-bin [s]
              (loop [n 0
                     s (reverse s)
                     p 1]
                (if (empty? s)
                  n
                  (recur (+ n (* (first s) p)) (rest s) (* p 2)))))
            (pad-with-zeroes [m c]
              (loop [acc (list* m)]
                (if (= (count acc) c)
                  acc
                  (recur (cons 0 acc)))))
            (apply-mask [s m]
              (map first (filter #(= 1 (second %))
                           (partition 2 (interleave
                                          s
                                          (pad-with-zeroes m (count s)))))))]
      (if (> k (count s))
        #{}
        (set (for [i (range (inc (from-bin (max-bin (count s)))))
                   :let [b (to-bin i)]
                   :when (= (count-ones b) k)]
               (set (apply-mask s b))))))))

(defcheck solution-40dc1f76
  (fn k-comb [k v]
    (cond
      (> k (count v)) #{}
      true (set ((reduce
                   (fn [m v]
                     (let [added-v (conj (map #(conj %1 v) (:inprogress m)) #{v})
                           new-inprogress (clojure.set/union (filter #(> k (count %)) added-v) (:inprogress m))
                           new-ready (clojure.set/union (:ready m) (filter #(= k (count %)) added-v))
                           ]
                       {:ready new-ready
                        :inprogress new-inprogress}
                       )
                     )
                   {:ready #{}
                    :inprogress #{}
                    }
                   v) :ready) )
      )))

(defcheck solution-41d31966
  (fn k-combs [n ls]
    (letfn [(subset2 [s]
              (if (empty? s)
                #{#{}}
                (let [f (first s)
                      ss (subset2 (disj s f))]
                  (into ss (map #(conj % f) ss)))))]
      (set (filter #(= n (count %)) (subset2 ls))))))

(defcheck solution-41f99e78
  (fn generating-k-combinations [k s]
    (set
      (cond
        (> k (count s)) []
        (= k 1)         (reduce #(concat %1 #{#{%2}}) #{} s)
        :else           (->> (reduce #(concat %1
                                        (map (fn [e] (set (conj e %2))) %1))
                               #{#{}}
                               s)
                          (filter #(= k (count %))))))))

(defcheck solution-4207ba64
  (fn k-combination [n ls]
    (letfn [(powerset [ls]
              (if (empty? ls) #{#{}}
                              (clojure.set/union (powerset (next ls)) (map #(conj % (first ls)) (powerset (next ls))))))]
      (set (filter #(= n (count %)) (powerset ls))))))

(defcheck solution-42ef23a
  (fn [n s]
    (let [next-subs (fn [prev-subs]
                      (set (mapcat
                             (fn [sub]
                               (for [e (clojure.set/difference s sub)]
                                 (conj sub e)))
                             prev-subs)))]
      (nth (iterate next-subs  #{#{}}) n))))

(defcheck solution-4333898e
  (fn combs
    ([n items] (set (combs items n #{} [])))
    ([items n results so-far]
     (if (= n (count so-far))
       (conj results (set so-far))
       (loop [is items results results]
         (if (empty? is) results
                         (recur (rest is)
                           (combs (remove #(= % (first is)) items)
                             n results
                             (conj so-far (first is))))))))))

(defcheck solution-43ae9e34
  (fn xx
    [n c]
    (cond (> n (count c)) #{}
          (= n (count c)) (set [c])
          (= n 1) (set (map hash-set c))
          :else (into  (set (map #(conj  %1  (first c))
                              (xx (dec n) (disj c (first c)))))
                  (xx n (disj c (first c)))))))

(defcheck solution-43ef8d1d
  (fn [k src]
    (let [cnt (count src) fin (range (- cnt k) cnt)]
      (if (> k cnt) #{}
                    (letfn [(make-set [ind] (set (map #(get (vec src) %) ind)))
                            (next-ind [ind] (loop [x ind m (dec cnt) ln 1]
                                              (if (< (last x) m) (vec(concat (butlast x) (range (inc (last x)) (+(inc (last x)) ln))))
                                                                 (recur (butlast x) (dec m) (inc ln) )
                                                                 )
                                              ))
                            ]
                      (loop [y (vec(range k)) z '()]
                        (if (= y fin) (set(conj z (make-set y)))
                                      (recur (next-ind y) (conj z (make-set y)))
                                      )
                        )
                      )
                    ))
    ))

(defcheck solution-448b887f
  (fn [limite data]
    (if (= limite 1)
      (set (map set (partition 1 data)))
      (let [data-seqs (take-while (complement nil?) (iterate #(next %) (seq data)))]
        (set (map set
               (mapcat (fn [model]
                         (let [data-iterations (map reverse (reduce (fn [c i] (conj c (apply conj (last c)  [i]))) [[]] (next model)))
                               splitea (fn [[a & more] limit]
                                         (let [[c d] (split-at (dec (dec limit)) more)]
                                           (map #(cons a (reverse (conj  c %)))  d)))]
                           (mapcat #(splitea (seq (clojure.set/difference (set  model) %)) limite) data-iterations)
                           )) data-seqs)))))))

(defcheck solution-44e97b8d
  (fn r [n a]
    (if (> n (count a))
      #{}
      (loop [res (map hash-set a) cnt 1]
        (if (= cnt n)
          (set (filter #(= n (count %)) res))
          (recur
            (reduce
              (fn [c x]
                (apply conj c (map (partial conj x) a)))
              #{} res)
            (inc cnt)))))))

(defcheck solution-45507618
  (fn f [n s] (set
                (cond
                  (= 1 n)         (map hash-set s)
                  (< (count s) n) #{}
                  :else           (into (set (map #(conj % (first s)) (f (dec n) (rest s))))
                                    (f n (rest s)))))))

(defcheck solution-45b39d4d
  (fn __
    [n s]
    (set (filter #(= (count %) n) (reduce (fn [a x]
                                            (->> a
                                              (map #(set (concat #{x} %)))
                                              (concat a)
                                              set))
                                    #{#{}} s)))))

(defcheck solution-45f72b9f
  (fn [n s] (set (filter #(= n (count %)) (reduce (fn [a e] (into a (map #(conj % e) a))) #{#{}} s)))))

(defcheck solution-462d7137
  (letfn [
          (comb [n s] (cond
                        (zero? n) [#{}]
                        (> n (count s)) []
                        true (let [
                                   s1 (first s)
                                   sn (disj s s1)
                                   comb1 (comb n sn)
                                   comb2 (for [i (comb (dec n) sn)] (conj i s1))]
                               (concat comb1 comb2))))]
    (comp set comb)))

(defcheck solution-46397d39
  (fn [n the-set] (let [result
                        (fn f [k s]
                          (cond
                            (= 0 k) #{#{}}
                            (empty? s) #{}
                            :else (clojure.set/union (map #(conj % (first s)) (f (dec k) (rest s))) (f k (rest s)))
                            )
                          )]
                    (set (result n the-set)))))

(defcheck solution-465e3b45
  (fn [m s]
    (set (filter (fn [x] (= m (count x)))
           (reduce
             (fn [a b] (clojure.set/union a (set (map (fn [x] (clojure.set/union x #{b})) a))))
             #{#{}}
             s)))))

(defcheck solution-46a0e5bf
  (fn k_combinations [k coll]
    (cond
      (empty? coll) #{}
      (< k 1) #{#{}}
      (= k (count coll)) #{(set coll)}
      (> k (count coll)) #{}
      :else
      (let [f (first coll)
            r (rest coll)
            cs_without_f (k_combinations k r)
            sub_cs_for_f (k_combinations (dec k) r)]
        (clojure.set/union
          cs_without_f
          (set (for [s sub_cs_for_f]
                 (conj s f)
                 ))
          )
        )
      )
    ))

(defcheck solution-47315637
  (fn [k a-set]
    (if (or (empty? a-set) (> k (count a-set)))
      #{}
      (loop [x k acc #{#{}}]
        (if (zero? x)
          (set (filter #(= k (count %)) acc))
          (recur (dec x) (for [y acc z a-set] (conj y z))))))))

(defcheck solution-487400b8
  (fn kcomb
    [n the-set]
    (letfn [(comb [seta]
              (if (empty? seta)
                #{seta}
                (let [e (first seta), rest-set (disj seta e)]
                  #_(println e ", " rest-set ", ")
                  (set (concat (map #(conj % e) (comb rest-set)) (comb rest-set))))))]
      (set (filter #(= (count %) n) (comb the-set))))))

(defcheck solution-48d94c62
  (fn [k s]
    (set (filter #(= (count %) k)
           (conj (reduce #(set (for [x % y %2]
                                 (conj (if (set? x) x #{x}) y)))
                   (repeat (count s) s)) #{})))))

(defcheck solution-496998a0
  (fn k-comb [n coll]
    (cond
      (zero? n) []
      (= 1 n) (set (map set (map vector coll)))
      :else (set (mapcat
                   #(set (map (fn [x] (set (cons % x))) (k-comb (dec n) (disj coll %))))
                   coll
                   ))
      )
    ))

(defcheck solution-498a94e1
  (fn k-combinations
    [n s]
    (letfn [(power-set*
              [s ps]
              (if-not (empty? s)
                (recur (rest s) (clojure.set/union ps (into #{} (map #(conj % (first s)) ps))))
                ps))]
      (set (filter #(= n (count %)) (power-set* s #{#{}}))))))

(defcheck solution-49a90b79
  (fn cmb [k s]
    (if (zero? k)
      #{#{}}
      (if (= k (count s))
        (hash-set s)
        (if (> k (count s))
          #{}
          (apply
            hash-set
            (concat
              (map #(conj % (first s)) (cmb (dec k) (disj s (first s))))
              (cmb k (disj s (first s)))
              )
            )
          )
        )
      )
    ))

(defcheck solution-49cc7938
  (fn [a b]
    (let [superset (fn superset [x]
                     (if (= 1 (count x))
                       #{ #{(first x)} #{}}
                       (apply merge
                         (superset (rest x))
                         (map #(into #{} (conj % (first x))) (superset (rest x))) )))]
      (into #{}
        (filter
          #(= a (count %))
          (superset b))))))

(defcheck solution-49f2c46d
  (fn f [k s]
    (cond
      (zero? k) #{#{}}
      (> k (count s)) #{}
      :else (into
              (f k (rest s))
              (map #(conj % (first s)) (f (dec k) (rest s)))))))

(defcheck solution-4a558b37
  (fn k-combs [n s]
    (cond
      (> n (count s)) #{}
      (= n 1) (set (map #(set [%]) s))
      (= n (count s)) (set [(set s)])
      :else
      (let
       [elt (first s)
        s-new (set (rest s))
        subsets-n-1 (k-combs (dec n) s-new)
        subsets-n (set (k-combs n s-new))
        aug-subsets-n (set (map #(set (conj % elt)) subsets-n-1))
        ret-val (clojure.set/union subsets-n aug-subsets-n)]
        ret-val))))

(defcheck solution-4a8fc011
  (fn [n items]
    (let [rec (fn rec [n items]
                (if (and (pos? n) (>= (count items) n))
                  (concat (rec n (rest items))
                    (if-let [more (seq (rec (dec n) (next items)))]
                      (for [item more]
                        (conj item (first items)))
                      (list (list (first items)))))
                  '()))]
      (set (map set (rec n items))))))

(defcheck solution-4b3865e7
  (fn  [n cs]
    (set
      (filter #(= n (count %))
        (loop [m n a #{#{}}]
          (if (= 0 m) a
                      (recur (dec m) (for [x a y cs] (conj x y)))))))))

(defcheck solution-4b95a863
  (fn combinations [n s]
    (let [powerset (fn f [x]
                     (if (empty? x)
                       #{#{}}
                       (into (f (set (rest x))) (set (for [y (vector (first x))
                                                           z (f (set (rest x)))]
                                                       (conj z y))))))]
      (set (filter #(== (count %) n) (powerset s))))))

(defcheck solution-4bae352a
  (letfn
   [(pow [base power]
      (reduce * (repeat power base)))
    (bits [n]
      (loop [n n accu [] pos 0]
        (cond (zero? n) accu
              (bit-test n pos) (recur (- n (pow 2 pos)) (conj accu pos) (inc pos))
              :else (recur n accu (inc pos)))))]
    (fn k-combo [k s]
      (let [bitmap-max (pow 2 (count s))
            ordered (vec s)]
        (->> (range 0 bitmap-max)
          (map bits)
          (filter #(= k (count %)))
          (map #(map ordered %))
          (map set)
          (set))))))

(defcheck solution-4c0efa3b
  (fn q [n s](let [ all (reduce (fn [a b](into a (map #(conj % b) a))) #{#{}} s)]
               (set(filter #(= n (count %)) all)))))

(defcheck solution-4c32d44d
  (fn [k s]
    (if (> k (count s))
      #{}
      (letfn [(help [acc x] (into acc (map #(conj % x) acc)))]
        (set (filter (comp (partial = k) count) (reduce help [#{}] s)))))))

(defcheck solution-4c626055
  (fn k-comb [n sets]
    (let [ac (fn all-comb [sets]
               (reduce (fn [m x]
                         (concat m (map #(conj % x) m)))
                 [[]]
                 sets))
          lst (filter #(= n (count %)) (ac sets))]
      (set (map set lst)))))

(defcheck solution-4c857d66
  (fn k-kombinations [k coll]
    (letfn [(first-rest-seq
              [k coll]
              (lazy-seq (when (<= k (count coll))
                          (let [f (first coll)
                                r (rest coll)]
                            (cons [f r]
                              (first-rest-seq k r))))))
            (combine
              [e coll]
              (set (map #(clojure.set/union #{e} %)
                     coll)))]
      (if (= k 1)
        (set (map hash-set coll))
        (apply clojure.set/union (map (fn [[f r]]
                                        (combine f (k-kombinations (dec k) r)))
                                   (first-rest-seq k coll)))))))

(defcheck solution-4cf05b86
  (fn [k s]
    (set (filter #(= k (count %))
           (reduce #(concat %1 (map (fn [x] (set (conj x %2))) %1))
             #{#{}} s)))))

(defcheck solution-4e139cb2
  (fn combinations [k s]
    (set (if (<= k 1)
           (map #(set [%]) s)
           (for [ss (combinations (dec k) s)
                 x s
                 :when (not (ss x))]
             (conj ss x))))))

(defcheck solution-4e5dc66c
  (fn [m coll]
    (loop [n 1 res #{}]
      (cond
        (> n m) res
        (empty? res) (recur (inc n) (set (map #(conj #{} %) coll)))
        :else (recur (inc n) (set (filter #(= n (count %)) (for [x  res y  coll] (conj x y)))))))))

(defcheck solution-4e78a8ee
  (fn k-comb [k s]
    (if (zero? k)
      #{#{}}
      (set
        (for [[x xs] (zipmap s (rest (iterate rest s)))
              rst (k-comb (dec k) xs)]
          (set (cons x rst)))))))

(defcheck solution-4ee52b91
  (fn [k s]
    (set
      (filter #(= k (count %))
        (reduce (fn [pset el]
                  (into pset
                    (map #(conj % el) pset)))
          #{#{}}
          s)))))

(defcheck solution-4f2561de
  (fn kcomb [n s]
    (if (= 0 n)
      #{#{}}
      (set (for [e s be (kcomb (dec n) (disj s e))]
             (set (conj be e)))))))

(defcheck solution-4f3b99fe
  (fn k-combs [n s]
    (set (filter #(= n (count %))
           (
            (fn powerset [l]
              (if (empty? l)
                #{#{}}
                (let [ps (powerset (rest l))]
                  (apply conj ps
                    (map (fn [x] (conj x (first l)))
                      ps)))))
            s)
           ))))

(defcheck solution-4f40d9a
  (fn k-comb [k s]
    (cond
      (= k 0) #{#{}}  ; there is exactly 1 0-comb of any set: the empty set
      (empty? s) #{}  ; there are no k-combs of the empty set (unless k=0)
      :else (into
              ; all k-combs involving (first s); this equals s conj'ed onto
              ; all (k-1)-combs NOT involving (first s):
              (set (map #(conj % (first s)) (k-comb (dec k) (rest s))))
              ; all k-combs NOT involving (first s):
              (k-comb k (rest s))
              ))))

(defcheck solution-4fd68cfc
  (fn k-set [i coll]
    (letfn [(p-set [coll]
              (if (empty? coll)
                #{(set coll)}
                (let [part_res (p-set (rest coll))]
                  (concat (map #(set (cons (first coll) %1)) part_res) part_res))))]
      (set (filter #(= (count %) i) (p-set coll))))))

(defcheck solution-4ff5aaf2
  (fn [k s]
    (let [combinations (fn combinations [s]
                         (if (seq s)
                           (let [f (first s)
                                 ss (combinations (rest s))]
                             (clojure.set/union ss #{#{f}} (set (map #(conj % f) ss))))
                           #{}))]
      (set (filter #(= (count %) k) (combinations s))))))

(defcheck solution-5008e962
  (fn comb [k S]
    (cond
      (empty? S) #{}
      (= 1 k) (reduce #(conj %1 #{%2}) #{} S)
      :e (clojure.set/union (comb k (rest S))
           (set
             (map #(conj % (first S))
               (comb (dec k) (rest S))))))))

(defcheck solution-50281e4
  (fn ff [n xs]
    (if (zero? n)
      #{#{}}
      (set (for [x xs
                 y (ff (dec n) (disj xs x))]
             (conj y x))))))

(defcheck solution-50b9ff77
  (fn ff[n lst]
    (letfn [(tails [xs]
              (take-while not-empty (iterate rest xs)))]
      (if (zero? n)
        #{#{}}
        (set
          (mapcat
            (fn [[x & xs]] (map #(conj % x) (ff (dec n) xs)))
            (tails (seq lst))))))))

(defcheck solution-50c65aec
  (fn k-comb [n S]
    (if (> n (count S)) #{}
                        (if (= n 0) #{#{}}
                                    (clojure.set/union
                                      (k-comb n (rest S))
                                      (set (map #(conj % (first S)) (k-comb (dec n) (rest S)))))))))

(defcheck solution-51130c85
  (fn[n s](
            set(filter #(= n (count %))

                 (loop[s2 s l []](
                                   if(= (count s2) (count l)) s2

                                                              (recur

                                                                (set(apply concat(map
                                                                                   (fn[i](
                                                                                           map
                                                                                           #(cond
                                                                                              (and (set? %) (set? i)) (clojure.set/union % i)
                                                                                              (set? %) (clojure.set/union % #{i})
                                                                                              (set? i) (clojure.set/union #{%} i)
                                                                                              :else (clojure.set/union #{%} #{i})
                                                                                              )
                                                                                           s2)
                                                                                     )
                                                                                   s
                                                                                   )))

                                                                s2

                                                                )
                                                              ))
                 ))))

(defcheck solution-5142ce36
  (fn kcomb [n s]
    (cond
      (> n (count s)) #{}
      (= n 0) #{}
      (= n 1) (set (map (partial conj #{}) s))
      :else (set (apply clojure.set/union (map (fn [x] (map (fn [y] (conj y x)) (kcomb (dec n) (disj s x)))) s))))))

(defcheck solution-518e1f31
  (fn [n st]
    (letfn [(expand [[a b]]
              (when (seq b)
                (cons [(conj a (first b)) (rest b)] (expand [a (rest b)]))))
            (combos [n s]
              (if (zero? n)
                (list [[] s])
                (mapcat expand (combos (dec n) s))))]
      (->> (map first (combos n st))
        (map set)
        set))))

(defcheck solution-52d6581c
  (fn [n elems]
    (letfn [(combs [s]
              (if (< 1 (count s))
                (let [rest-combs (combs (rest s))]
                  (concat rest-combs
                    (map #(conj % (first s)) rest-combs)))
                #{#{} #{(first s)}}))]
      (set (filter #(= n (count %)) (combs elems))))))

(defcheck solution-52fc2323
  (fn [n s]
    (letfn [(ss [l]
              (reduce
                #(apply concat (map (fn [v]
                                      [v (set (conj v %2))])
                                 %))
                [#{}] l))]
      (set (filter #(= n (count %))
             (ss s))))))

(defcheck solution-5344010a
  (fn k-combinations
    [n s]
    (letfn [(powerset [s]
              (apply clojure.set/union
                #{s}
                (map #(powerset (disj s %)) s)))]
      (set (filter #(= n (count %)) (powerset s))))))

(defcheck solution-53da4dca
  (fn k-comb [k items]
    (let [items (vec items)
          num-items (count items)
          generate
          (fn generate [k fst res]
            (if (zero? k)
              (list res)
              (mapcat #(generate (dec k) (inc %) (conj res (items %))) (range fst num-items))
              ))
          ]
      (set (generate k 0 #{}))
      )))

(defcheck solution-53db2c9c
  (fn f
    [n s]
    (let [ss (seq s)]
      (apply (comp set concat)
        (for [i (range (count ss))]
          (let [iv (nth ss i)]
            (if (= n 1)
              (list (set (list iv)))
              (map #(conj % iv) (f (dec n) (drop (inc i) ss))))))))))

(defcheck solution-5434436
  (fn comb [k s]
    ;  (let [vs (vec s) nn (count s)
    ;        ss (for [n (range (dec (bit-shift-left 1 k)) (bit-shift-left 1 nn))
    ;                 :let [subs (filter #(bit-test n %) (range n))] :when (= k (count subs))] subs) ]
    ;    (set (map #(set (replace vs %)) ss))))

    (if (zero? k) #{#{}}
                  (set (for [e s ss (comb (dec k) (disj s e))]
                         (conj ss e))))))

(defcheck solution-543bf84e
  (fn k-combinations [n s]
    (if (> n (count s)) #{}
                        (if (= 1 n) (into #{} (map (comp set vector) s))
                                    (let [prior (k-combinations (dec n) s)]
                                      (into #{} (for [p prior
                                                      q (clojure.set/difference s p)]
                                                  (clojure.set/union p #{q})
                                                  )))))))

(defcheck solution-54620bcc
  (fn f
    [k S]
    (if (= 0 k)
      #{#{}}
      (set (mapcat
             identity
             (for [x S]
               (for [ys (f
                          (dec k)
                          (clojure.set/difference S (set (list x))))]
                 (conj ys x))))))))

(defcheck solution-54701304
  (fn [k coll]
    (set (filter #(= (count %) k)
           (reduce (fn [acc item]
                     (reduce #(conj % (conj %2 item)) acc acc))
             #{#{}} coll)))))

(defcheck solution-54950b44
  (fn [k l]
    (into #{} (filter #(= (count %) k) (reduce (fn [s x]
                                                 (into s (set (map #(into % #{x}) s)))) #{#{}} l)))))

(defcheck solution-54f3bb01
  (fn n-tuples
    [n s]
    (set (if (= 1 n)
           (map #(set [%]) s)
           (let [zm (map vector (range 1 (inc (count s))) s)]
             (for [x zm
                   y (n-tuples (dec n) (drop (first x) s))]
               (into #{(second x)} y)))))))

(defcheck solution-55146d17
  (fn [n ss]
    (if (> n (count ss)) #{}
                         (reduce (fn [r s] (set (for [x ss y r :when (not (y x))] (conj y x))))
                           #{#{}}
                           (range n)))))

(defcheck solution-554b4bcb
  (fn combs [n xs]
    (set (map set (cond (= n 0)     [[]]
                        (empty? xs) []
                        :else
                        (concat (map #(cons (first xs) %) (combs (- n 1) (rest xs))) (combs n (rest xs)))
                        )))))

(defcheck solution-554eec71
  (letfn [(combinations [c]
            (reduce (fn [acc it]
                      (set (concat acc (map #(conj % it) acc) #{#{it}})))
              #{#{}} c))]
    (fn [n c]
      (set (filter #(= (count %) n) (combinations c))))))

(defcheck solution-5556c18c
  (fn k-combinations
    [k s]
    (loop [sets (map hash-set s)]
      (if (every? #{k} (map count sets))
        (set sets)
        (recur (for [x sets y s
                     :when (not (x y))]
                 (conj x y)))))))

(defcheck solution-55f4041f
  (fn k-combinations
    ([k s] (k-combinations k s #{}))
    ([k s t]
     (cond
       (zero? k)
       #{t}

       (< (count s) k)
       #{}

       :else
       (clojure.set/union
         (k-combinations k
           (disj s (first s))
           t)
         (k-combinations (dec k)
           (disj s (first s))
           (conj t (first s))))))))

(defcheck solution-5655da33
  (fn [sz st]
    (letfn [
            (index-combinations
              [n cnt]
              (lazy-seq
                (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
                      iter-comb
                        (fn iter-comb [c j]
                          (if (> j n) nil
                                      (let [c (assoc c j (dec (c j)))]
                                        (if (< (c j) j) [c (inc j)]
                                                        (loop [c c, j j]
                                                          (if (= j 1) [c j]
                                                                      (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
                      step
                        (fn step [c j]
                          (cons (rseq (subvec c 1 (inc n)))
                            (lazy-seq (let [next-step (iter-comb c j)]
                                        (when next-step (step (next-step 0) (next-step 1)))))))]
                  (step c 1))))
            (kcombos [st sz]
              (let [
                    inds (index-combinations sz (count st))
                    ]
                (set (map set
                       (map
                         (fn [s] (map (vec st) s)) inds))))) ]

      (cond (= 1 sz) (set (map (fn [x] (set [x])) st))
            (> sz (count st)) #{}
            (= sz (count st)) #{st}
            :else (kcombos st sz)))))

(defcheck solution-5690f184
  (fn
    [k s]
    (letfn [(subsets [s] (map #(bit-filter s %) (range (two-raised (count s)))))
            (bit-filter [s m] (let [indexed-elts (zipmap (range) s)]
                                (set (filter #(not (nil? %))
                                       (map #(if (bit-test m (% 0)) (% 1))
                                         indexed-elts)))))
            (two-raised [n] (apply * (take n (repeat 2))))]
      (set (filter #(= (count %) k) (subsets (set s)))))))

(defcheck solution-57a23404
  (fn [n s]
    (letfn [(powerset [s]
              (let [v (vec s) c (count v) r (range 0 c)]
                (map (fn [i]
                       (reduce (fn [acc j]
                                 (if (= 1 (mod (bit-shift-right i j) 2))
                                   (conj acc (get v j)) acc))
                         #{} r))
                  (range 0 (bit-shift-left 1 c)))))]
      (set (filter #(= n (count %)) (powerset s))))))

(defcheck solution-57c0550
  (fn [n ls]
    (letfn [
            (powerset [ls]
              (if (empty? ls) #{#{}}
                              (clojure.set/union (powerset (next ls))
                                (map #(conj % (first ls)) (powerset (next ls))))))]
      (into #{} (filter #(= (count %) n) (powerset ls))))))

(defcheck solution-5852517d
  (fn comb[n sx](
                  if (= 1 n)
                  (set (map #(conj #{} %) sx))
                  (set (flatten (map #(map (fn[x](conj x %))  (comb (dec n) (clojure.set/difference sx #{%}))) sx)))
                  )))

(defcheck solution-58ced1f8
  (fn k-comb [k es]
    (let [[h t] (split-at 1 es)]
      (cond
        (= k 0) #{ #{} }
        (> k (count es)) #{}
        :else (into
                (k-comb k t)
                (map #(into % h) (k-comb (dec k) t)))))))

(defcheck solution-58f37695
  (let [powerset (fn thisfunc [s]
                   (if (empty? s)
                     #{#{}}
                     (let [result (thisfunc (rest s))]
                       (set (concat result (map #(conj % (first s)) result))))))]
    (fn [k s]
      (set (filter #(= k (count %)) (powerset s))))))

(defcheck solution-5930559f
  (fn [n s]
    (letfn [(gen-tree [n s]
              (if (zero? n)
                []
                (map
                  (fn [a]
                    (cons a (gen-tree (dec n) (clojure.set/difference s (hash-set a))))) s)))
            (walk-tr-in-depth [tr acc n]
              (if (zero? n)
                [acc]
                (mapcat (fn [cn] (walk-tr-in-depth
                                   (when (seq? cn) (rest cn))
                                   (conj acc (if (seq? cn) (first cn) cn))
                                   (dec n)))
                  tr)))]
      (set (map set (walk-tr-in-depth (gen-tree n s) [] n))))))

(defcheck solution-596eaa59
  (fn [n s]
    (letfn [(i [s]
              (if (empty? s) #{#{}}
                             (concat (map #(conj % (first s)) (i (rest s)))
                               (i (rest s)))))]
      (set (filter #(= n (count %)) (i s))))))

(defcheck solution-59dff79b
  (fn subsets [n items]
    (->> (cond
           (= n 0) '(())
           (empty? items) '()
           :else (concat (map
                           #(cons (first items) %)
                           (subsets (dec n) (rest items)))
                   (subsets n (rest items))))

      (map set)
      set)))

(defcheck solution-5ac3bff3
  (fn comb [k coll]
    (if (> k (count coll)) #{}
                           (loop [n 0 s #{#{}}]
                             (if (= n k) s
                                         (recur
                                           (inc n)
                                           (into #{}
                                             (filter identity
                                               (for [m s
                                                     x coll]
                                                 (if-not (m x) (conj m x)))))
                                           ))))))

(defcheck solution-5ad106f0
  (fn comb [n s]
    (loop [n n r (map #(-> #{%}) s)]
      (if (= 1 n)
        (set r)
        (recur (dec n)
          (for [c (set r)
                e (reduce disj s c)]
            (conj c e)))))))

(defcheck solution-5af812c4
  (fn f [k s]
    (cond (< (count s) k) #{}
          (= k 0) #{#{}}
          :else (into (f k (next s))
                  (map #(conj % (first s)) (f (dec k) (next s)))))))

(defcheck solution-5b089da8
  (fn k-komb [k s]
    (if (zero? k)
      #{#{}}
      (if (empty? s)
        #{}
        (set (clojure.set/union
               (map #(conj % (first s))
                 (k-komb (dec k) (rest s)))
               (k-komb k (rest s))))))))

(defcheck solution-5b1844a7
  (fn k-combinations [n S]
    (letfn [(f [n S]
              (cond (= n 0) [[]]
                    (empty? S) []
                    :else (concat (map (fn [y] (into #{} (cons (first S) y)))
                                    (f (- n 1) (rest S)))
                            (f n (rest S)))))]
      (into #{} (f n S)))))

(defcheck solution-5b27bdf
  (fn [n s]
    (into #{} (filter #(= n (count %))
                (into #{}
                  (map #(into #{} %)
                    ((fn ps [ls]
                       (if (empty? ls) '(())
                                       (clojure.set/union (ps (next ls))
                                         (map #(conj % (first ls)) (ps (next ls))))))
                     s)))))))

(defcheck solution-5b52becd
  (fn f [n s]
    (set
      (if (== 1 n)
        (map hash-set s)
        (let [z (f (dec n) s)]
          (for [x s, t z :when (not (t x))]
            (conj t x)))))))

(defcheck solution-5bde9807
  (fn [n s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (cond
        (< (count s) n) #{}
        (= (count s) n) (hash-set (set s))
        :else (set (filter #(= (count %) n)
                     ((reduce #(p %1 %2 s) {} (range (count s))) n s)))))))

(defcheck solution-5c029fe7
  (fn kc [n xs]
    (let [pwr (fn [b p] (reduce * (take p (repeat b))))
          xsv (vec xs)
          bits (count xs)
          len (pwr 2 bits)
          active (fn [n] (set (map xsv (filter (comp not nil?) (map-indexed #(when (bit-test n %2) %1) (range bits))))))]
      (set (filter #(= n (count %)) (map active (range len)))))
    ))

(defcheck solution-5c9ebc72
  (fn f [k s]
    (if (> k (count s))
      #{}
      (if (or (zero? k) (empty? s))
        #{#{}}
        (set (for [x s r (f (dec k) (disj s x))] (conj r x)))))))

(defcheck solution-5ce2a0ad
  (fn [l s]
    (loop [step 0 n #{#{}}]
      (if (= step (count s)) (set (filter #(= l (count %)) n)) ;filter the sets that compliance requirements
                             (recur (inc step)
                               (apply conj n
                                 (for [x s y n]
                                   (conj y x))
                                 ))))))

(defcheck solution-5d270d60
  (fn pick [k coll]
    (set (filter #(= k (count %))
           (reduce
             (fn [r n]
               (clojure.set/union r
                 (map #(conj % n) r))
               )
             #{#{}} coll)))))

(defcheck solution-5d38ffa1
  (fn k-combos [k s]
    (cond
      (or (empty? s) (> k (count s))) #{}
      (= k 1) (set (map hash-set s))
      (= k (count s)) #{(set s)}
      :else
      (set (concat
             (map #(conj % (first s)) (k-combos (dec k) (rest s)))
             (k-combos k (rest s)))))))

(defcheck solution-5d81b377
  (fn [n s]
    (set (nth (iterate #(for [y % x y]
                          (disj y x))
                [s])
           (Math/abs (- (count s) n))))))

(defcheck solution-5d89ef8a
  (fn k-comb [k S]
    (let [size (count S)]
      (cond
        (> k size) #{}
        (= k size) #{S}
        (= k 1)    (set (map (fn [s] #{s}) S))
        (= k 0)    #{#{}}
        :else      (set (mapcat
                          (fn [s]
                            (map #(clojure.set/union s %)
                              (k-comb (dec k)
                                (clojure.set/difference S s))))
                          (k-comb 1 S)))))))

(defcheck solution-5d929e85
  (fn f [num s]
    (cond
      (zero? num) #{#{}}
      (empty? s) #{}
      :else (set (clojure.set/union
                   (map #(conj % (first s)) (f (dec num) (rest s)) )
                   (f num (rest s))))
      )))

(defcheck solution-5d949b04
  (fn k-combinations [n coll]
    (cond
      (> n (count coll)) #{}
      (= n 1) (into #{}  (map #(set [%]) coll))
      :else (into #{} (mapcat (fn [x]
                                (map #(conj % x) (k-combinations (dec n) (disj coll x))))
                        coll)))))

(defcheck solution-5e10f7f2
  (fn c [n s]
    (cond
      (zero? n) #{#{}}
      (> n (count s)) #{}
      (= n (count s)) #{(set s)}
      :else (into (c n (rest s)) (map #(conj % (first s)) (c (dec n) (rest s)))))))

(defcheck solution-5e8648f0
  (fn [x s]
    (letfn [(powerset [s]
              (reduce (fn [ps x]
                        (reduce (fn [ps s]
                                  (conj ps (conj s x))) ps ps)) #{#{}} s))]
      (set (filter #(= x (count %)) (powerset s))))))

(defcheck solution-5ec57122
  (fn comb [size s] (if (empty? s) (if (zero? size) #{#{}} #{})
                                   (clojure.set/union
                                     (comb size (rest s))
                                     (set (map #(conj % (first s)) (comb (dec size) (rest s))))))))

(defcheck solution-5ee58402
  (fn [n s] (set
              (filter #(= (count %) n)
                (reduce #(clojure.set/union % (set (for [x %] (conj x %2))) #{#{%2}}) #{} s)))))

(defcheck solution-5f736d8
  (fn [n s] (let [x (reduce #(conj % #{%2}) #{} s) y (fn [t c] (set (for [r t l x :when (not (contains? r (first l)))] (conj r (first l)))))] (reduce #(y % %2)  x (range 1 n)))))

(defcheck solution-5f867896
  (fn [k s]
    (let [digits (fn [num base]
                   ;; this function will only work for bases from 2 to 10
                   ;; please don't use base 1 :)
                   (if (zero? num)
                     '(0)
                     (reverse (map second
                                (take-while #(not= [0 0] %)
                                  (iterate (fn [[a _]]
                                             [(quot a base) (rem a base)])
                                    [(quot num base) (rem num base)]))))))
          n (count s)
          binary-nums (map #(digits % 2) (range (Math/pow 2 n)))
          pad-seq (repeat n 0)
          padded-binary-nums (map #(take-last n (concat pad-seq %)) binary-nums )
          k-ones (filter #(= k (apply + %)) padded-binary-nums)
          combinations (map (fn [ones-zeros]
                              (remove nil? (map (fn [one-or-zero item]
                                                  (if (zero? one-or-zero) nil item))
                                             ones-zeros
                                             (seq s))))
                         k-ones)]
      (into #{} (map set combinations)))))

(defcheck solution-5fb86730
  (fn [n xs]
    (if (< (count xs) n)
      #{}
      (letfn
       [(union [s1 s2]
          (reduce #(conj %1 %2) s1 s2)
          )
        (go
          [r m ys]
          (if (= 0 m)
            r
            (reduce
              union
              (for [e ys]
                (go
                  (into #{} (for [q r] (conj q e)))
                  (dec m)
                  (disj ys e)))
              )))]
        (into #{}
          (filter #(= n (count %))
            (go (into #{} (for [e xs] #{e}))
              (dec n)
              xs)))))))

(defcheck solution-60735987
  (fn kcom [k coll]
    (set (cond (or (= k 0) (empty? coll)) #{}
               (= k 1) (map hash-set coll)
               :else (concat (map #(into (hash-set (first coll)) %)
                               (kcom (dec k) (rest coll)))
                       (kcom k (rest coll)))))))

(defcheck solution-60842c37
  (fn [k co]
    (letfn [(genk [v i co k mk]
              (if (= k mk) v
                           (for [i (range i (count co))]
                             (genk (conj v i) (inc i) co (inc k) mk))))]
      (if (< (count co) k) #{}
                           (set (map (fn [x] (set (map (fn [y] (nth (into [] co) y)) x)))
                                  (partition k (flatten (genk [] 0 co 0 k)))))))))

(defcheck solution-6127ed0d
  (fn [k xs]
    (loop [xs xs zs #{#{}}]
      (if (seq xs)
        (recur (next xs) (into zs (map #(conj % (first xs)) zs)))
        (apply hash-set (filter #(== (count %) k) zs))))))

(defcheck solution-61400a20
  (fn [n s]
    (let [v (into [] s)
          places (count v)
          combos (for [a (range places)
                       b (range places)
                       c (range places)
                       d (range places)]
                   (distinct (take n [a b c d])))]
      (->> combos
        (map (fn do-combos [combo]
               (map (fn do-combo [i]
                      (nth v i))
                 combo)))
        (filter #(= n (count %)))
        (map set)
        (set)))))

(defcheck solution-616d45f
  (fn [n s] (set (filter #(= n (count %)) (reduce (fn [s v] (concat s (map #(conj % v) s))) #{#{}} s)))))

(defcheck solution-6284563b
  (fn [n s]
    (loop [n n a #{#{}}]
      (if (> n 0)
        (recur (dec n) (set (for [x a y s :when (not (x y))] (conj x y))))
        a))))

(defcheck solution-63ca333c
  (fn k-combs [n domain]
    (if (<= n 1)
      (into #{} (map (fn [el] #{el}) domain))
      (into #{}
        (mapcat (fn [el]
                  (let [subseqs (k-combs (dec n) (disj domain el))]
                    (map #(conj % el) subseqs)))
          domain)))))

(defcheck solution-63de42cc
  (fn k-combinations [k elements]
    (if (zero? k)
      #{#{}}
      (->> elements
        (map (fn [item]
               (->> (k-combinations (dec k)
                      (disj elements item))
                 (map #(conj % item)))))
        (reduce into #{})))))

(defcheck solution-643620d7
  (fn ! [n s]
    (letfn [(tf [n p f]
              (if (zero? n) #{p}
                            (if (empty? f) #{}
                                           (clojure.set/union
                                             (! n (rest f))
                                             (map (partial cons (first f))
                                               (tf (dec n)
                                                 (conj (vec p) (first f))
                                                 (rest f)))
                                             ))))]
      (set (map set (tf n [] (vec s)))))))

(defcheck solution-64aa377
  (fn k-com [n col]
    (if (> n (count col))
      #{}
      (if (= 1 n)
        (set (for [e col] #{e}))
        (clojure.set/union
          (k-com n (rest col))
          (set (map #(set (cons (first col) %)) (k-com (- n 1) (rest col)))))))))

(defcheck solution-64b85109
  (fn k-comb [n a-set]
    (letfn [(powerset-helper [current leftovers]
              (if (empty? leftovers)
                (set current)
                (let [elem (first leftovers)]
                  (powerset-helper (concat current
                                     (map (fn [x] (conj x elem)) current))
                    (rest leftovers)))))]
      (set (filter #(= n (count %)) (powerset-helper #{#{}} a-set))))))

(defcheck solution-64dc6830
  (fn [k s]
    (let [n (count s)]
      (if (> k n)
        #{}
        (letfn [(next-combination [n k v]
                  (when (and (>= n k) (> k 0))
                    (let [max-n (dec n)
                          last-idx (dec k)
                          lastv (last v)]
                      (if (< lastv max-n)
                        (assoc v last-idx (inc lastv))
                        (if-let [prefix (next-combination
                                          max-n last-idx (subvec v 0 last-idx))]
                          (conj prefix (inc (last prefix))))))))
                (combinations [n k]
                  (let [first (range k)]
                    (iterate (partial next-combination n k) (vec (range k)))))
                (foo [s indeces]
                  (let [v (vec s)]
                    (set (map v indeces))))]
          (set (map (partial foo s)
                 (take-while identity (combinations (count s) k)))))))))

(defcheck solution-656df12c
  (fn [n s]
    (letfn [(subsets [s]
              (when (seq s)
                (let [e (first s) p (subsets (rest s))]
                  (concat [[e]] p (map #(conj % e) p)))))]
      (set (filter #(= (count %) n) (map set (subsets s)))))))

(defcheck solution-6597963d
  (fn [k s]
    (letfn [(power-set [s]
              (reduce (fn [ps x]
                        (into ps (map #(conj % x) ps)))
                #{#{}} s))]
      (set (filter #(= (count %)
                      k)
             (power-set s))))))

(defcheck solution-660677d0
  (fn f [n s]
    (set
      (if (<= n (count s))
        (if (= n 1)
          (map hash-set s)
          (concat (map #(conj % (first s)) (f (dec n) (next s)))
            (remove empty? (f n (next s)))))))))

(defcheck solution-66292e7f
  (fn [k s]
    (let [ps
          (loop [a #{ #{} } [h & t] (seq s)]
            (if (nil? h) a
                         (recur (into a (for [x a] (conj x h)))  t)  ))]
      (set (filter #(= k (count %)) ps)))))

(defcheck solution-663dcb8a
  (fn k-combinations
    [k S]
    (cond
      (< (count S) k) #{}
      (= 1 k) (set (map hash-set S))
      :else (set (mapcat
                   (fn [x] (map #(conj % x) (k-combinations (dec k) (disj S x))))
                   S)))))

(defcheck solution-66ab0962
  (fn k [n s]
    (if (= n 0)
      [#{}]
      (set
        (mapcat
          (fn [i]
            (filter #(>= (count %) n)
              (map #(conj % i) (k (- n 1) s))))
          s)))))

(defcheck solution-66c1f3a0
  #(loop [out #{#{}}]
     (cond
       (> % (count %2)) #{}
       (= % (count (first out))) out
       :else (recur (set (for [i %2 j out :when (not (contains? j i))]
                           (conj j i)))))))

(defcheck solution-66d2d0b7
  (fn solution [number s]
    (if (< (count s) number)
      #{}
      (let [start (map #(conj #{} %) s)]
        (into #{} (filter #(= (count %) number)
                    (loop [round 1 result start]
                      (if
                       (= round number) result
                                        (recur (inc round) (for [x start y result] (clojure.set/union x y))
                                          )
                                        )
                      )
                    )
          )
        )
      )
    ))

(defcheck solution-679a3cb4
  (fn comb [n s]
    (cond
      (= 1 n) (set (map (comp set vector) s))
      (> n (count s)) #{}
      :else (apply clojure.set/union (map (fn [x] (set (map #(conj % x) (comb (dec n) (disj s x))))) s)))))

(defcheck solution-679bd13a
  (fn [n s-set]
    (set (filter #(= (count %) n)
           (reduce (fn [d-set i]
                     ((comp set flatten)
                      (map
                        (fn [i-set]
                          (map
                            (fn [x]
                              (conj i-set x))
                            s-set))
                        d-set)))
             #{#{}}
             (range n))))))

(defcheck solution-67e92fa
  (fn [n s]
    (set (filter #(= (count %) n) (set (loop [ret #{#{}}
                                              left s]
                                         (if (empty? left)
                                           ret
                                           (recur (concat (map #(conj % (first left)) ret)
                                                    ret)
                                             (rest left)))))))))

(defcheck solution-6858260
  (fn kcomb [k s]
    ((fn kcomb-sub [k s acc]
       (if (= 0 k) #{acc}
                   (apply clojure.set/union #{}
                     (for [e s]
                       (kcomb-sub (dec k) (disj s e) (conj acc e)))))) k s #{})))

(defcheck solution-6878898d
  (fn kcomb [k S]
    (let [S (seq S)
          max-n (reduce * (repeat (count S) 2 )) ]
      (loop [n 0 rv #{} ]
        (let [bit-count (apply + (map #(if (bit-test n %) 1 0) (range (count S)))) ]
          #_(print n bit-count "\n")
          (cond
            (> n max-n) rv
            (not= bit-count k) (recur (inc n) rv)
            :else
            (recur (inc n) (conj rv (into #{} (map (partial nth S) (filter (partial bit-test n) (range (count S)))))))))))))

(defcheck solution-68ff37ac
  (fn f [n s]
    (cond
      (> n (count s)) #{}
      (zero? n) #{#{}}
      (and (= n (count s))) #{(set s)}
      :else (let [h (first s) t (rest s)]
              (set (into (f n t) (map #(conj %1 h) (f (dec n) t))))))))

(defcheck solution-69328a4e
  (fn [n s]
    (set (filter #(= n (count %))
           (reduce
             (fn [o i]
               (into o (map #(conj % i) o)))
             #{#{}} s)))))

(defcheck solution-69f282d1
  (fn combinations [k xs]
    (into #{}
      (if (zero? k)
        [ #{} ]
        (mapcat
          (fn [x i]
            (map
              conj
              (combinations (dec k) (drop i xs))
              (repeat x)))
          xs
          (drop 1 (range)))))))

(defcheck solution-69f2ce20
  (fn [x coll]
    (if (> x (count coll))
      #{}
      (loop [res #{(into #{} (take x coll))}
             coll (drop x coll)]
        (if (empty? coll)
          res
          (recur
            (clojure.set/union res
              (into #{}
                (mapcat
                  (fn [s]  (map
                             (fn [i]
                               (conj (disj s i)
                                 (first coll)))
                             s))
                  res)))
            (rest coll)))))))

(defcheck solution-6a116e9a
  (fn kcomb [k s]
    (let [h (first s)
          t (set (rest s))]
      (cond
        (zero? k) #{#{}}
        (= k (count s)) #{s}
        (> k (count s)) #{}
        :else
        (do
          (clojure.set/union
            (kcomb k t)
            (set (map #(conj % h) (kcomb (dec k) t)))
            ))))))

(defcheck solution-6a1c8de7
  (fn [k set]
    (reduce (fn [ret set]
              (into #{} (for [s ret x set :when (not (s x))] (conj s x))))
      #{#{}} (repeat k set))))

(defcheck solution-6a851128
  (fn k-comb [k aset]
    (cond (zero? k) #{#{}}
          (empty? aset) #{}
          :else (set (mapcat
                       (fn [sel]
                         (let [sels-1 (k-comb (dec k) (disj aset sel))]
                           (map #(conj % sel) sels-1)))
                       aset)))))

(defcheck solution-6b3c230b
  (fn kcomb [k s] (if (< (count s) k)
                    #{}
                    (if (= 1 k)
                      (set (map hash-set s))
                      (reduce into #{} (map (fn[el](set (map #(conj % el) (kcomb (dec k) (disj s el))))) (drop-last (dec k) s)))
                      ))
    ))

(defcheck solution-6c0b34df
  (fn kcombi [n inset]
    (let [coll (into [] inset)]
      (cond (> n (count coll)) #{}
            (< n 1) #{}
            (= n 1) (into #{} (map hash-set coll))
            :else (set (map set (mapcat (fn [x]
                                          (map #(cons (nth coll x)  %)
                                            (kcombi (dec n) (drop (inc x) coll))))
                                  (range  (count coll)))))))))

(defcheck solution-6cc925a7
  (fn gkc [n s]
    (set (filter #(= n (count %)) ((fn superset [s]
                                     (loop [s s
                                            r #{#{}}]
                                       (if (nil? (first s))
                                         r
                                         (recur (rest s) (clojure.set/union r(map #(clojure.set/union #{(first s)} %) r)))))) s)))))

(defcheck solution-6ce942b0
  (fn [n col]
    (let [gen (fn [s]
                (set
                  (for [x col i s]
                    (conj i x))))]
      (loop [i n s #{#{}}]
        (if (< i 0)
          (set (filter #(= n (count %)) s))
          (recur (dec i)
            (gen s)))))))

(defcheck solution-6d0f39ab
  (fn [size items]
    (let [all (loop [i items
                     res #{#{}}]
                (let [curr (first i)
                      rst (rest i)]
                  (if curr
                    (recur rst (into res (map #(conj % curr) res)))
                    res)))]
      (set (filter #(= (count %) size) all)))))

(defcheck solution-6d413a36
  (fn comb [n x] (cond
                   (zero? n) #{#{}}
                   (empty? x) #{}
                   :else
                   (let [a (first x)]
                     (set (concat (comb n (disj x a))
                            (map #(conj % a)
                              (comb (dec n) (disj x a)))))))))

(defcheck solution-6d462724
  (fn [n items]
    (letfn [(powerset [items]
              (if (empty? items)
                #{#{}}
                (clojure.set/union (powerset (next items))
                  (map #(conj % (first items)) (powerset (next items))))))]
      (->> items
        powerset
        (filter #(= (count %) n))
        (into #{})))))

(defcheck solution-6d7fc43f
  (fn k-combin [n coll]
    (cond (or (< n 1) (> n (count coll))) #{}
          (= n 1) (set (map #(hash-set %) coll))
          :else (let [z (k-combin (dec n) coll)]
                  (reduce (fn [c e]
                            (clojure.set/union c
                              (set (filter (fn [s]
                                             (= n (count s)))
                                     (map #(conj e %) coll)))))
                    #{} z)))))

(defcheck solution-6da0552
  (fn k-combs [k coll]
    (let [S (set coll)]
      (cond
        (zero? k) #{#{}}
        (> k (count coll)) #{}
        :else (into #{}
                (mapcat
                  (fn [x]
                    (map #(conj % x)
                      (k-combs (dec k) (disj S x)))) S))))))

(defcheck solution-6ddd9ce6
  (fn [len sr]
    (letfn [ (comb [s c n t] (if (= n (count c)) c
                                                 (if (< t (count s))
                                                   (flatten (filter
                                                              #(not(nil? %))
                                                              (conj #{} (comb s (conj c (nth s t) ) n (inc t)) (comb s c n (inc t))))))))  ] (into #{}  (comb (seq sr) #{} len 0)))))

(defcheck solution-6e197157
  (fn f [n se]
    #_(prn n se)
    (if (< (count se) n)
      #{}
      (if ( = (count se) n)
        #{se}
        (set (mapcat (partial f n) (map #(disj se %) se)))
        )
      )
    ))

(defcheck solution-6e4f62ff
  (fn kcom [n items]
    (letfn [(com [items]
              (if (empty? items)
                #{#{}}
                (let [h (first items)
                      com-rest (com (rest items))]
                  (into com-rest (map #(conj % h) com-rest)))))]

      (into #{} (filter #(= n (count %)) (com items))))))

(defcheck solution-6efb4a17
  (fn f [k s]
    (cond (= k 1) (set (map #(set [%]) s))
          (> k (count s)) #{}
          :else (set (mapcat #(map (fn [s] (conj s %)) (f (- k 1) (disj s %))) s)))))

(defcheck solution-6f116a64
  (fn [n s]
    (->> s vec
      ((fn [v]
         (->>
           (for [i (range 0 (dec (bit-shift-left 1 (count v))))]
             (filter #(zero? (bit-and i (bit-shift-left 1 %))) (range (count v))))
           (filter #(= n (count %)))
           (map #(set (map v %)))
           ) ))
      set
      )))

(defcheck solution-6fbbc6ad
  (fn [n s]
    (let [powerset
          (fn [in out]
            (if (empty? in)
              out
              (recur (disj in (first in))
                (apply merge
                  out
                  (map #(conj % (first in)) out)))))]

      (set (filter #(= (count %) n) (powerset s #{#{}}))))))

(defcheck solution-6ff31e40
  (fn kcomb
    ([k col]
     (kcomb k col #{}))
    ([k col pret]
     (if (empty? col)
       (into #{} (filter (fn [e] (= (count e) k)) pret))
       (recur k (rest col) (reduce (fn [ret this]
                                     (conj ret
                                       (conj this (first col))))
                             (conj pret (hash-set (first col))) pret))))))

(defcheck solution-6ffacfa6
  (fn kc [k s]
    (if (> k (count s))
      #{}
      (let [ordered (vec s)
            indexed (map-indexed vector ordered)]
        (if (= k 1)
          (set (map (fn [x] #{x}) s))
          (set
            (mapcat (fn [n]
                      (map #(conj % (ordered n)) (kc (dec k) (subvec ordered (inc n)))))
              (range (inc (- (count ordered) k))))))))))

(defcheck solution-7006437c
  (fn [n s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (cond
        (< (count s) n) #{}
        (= (count s) n) (hash-set (set s))
        :else (set (filter #(= (count %) n)
                     ((reduce #(p % %2 s) {} (range (count s))) n s)))))))

(defcheck solution-7010d716
  (fn kc [k coll]
    (if (zero? k)
      #{#{}}
      (when (seq coll)
        (into #{}
          (for [x  coll
                ys (kc (dec k)
                     (disj coll x))]
            (conj ys x)))))))

(defcheck solution-7083bed4
  (fn [n s]
    (set
      (filter #(= n (count %))
        (letfn [(a [s] (map #(disj s %) s))
                (f [r] (reduce into r (map a r)))]
          (f (cons s (a s))))))))

(defcheck solution-70b765c4
  (letfn
   [(subset [s n]
      (set
        (loop [s s, ss (), n n]
          (if (< n 1) ss
                      (if (not= 0 (rem n 2))
                        (recur (rest s) (conj ss (first s)) (quot n 2))
                        (recur (rest s) ss (quot n 2)))))))

    (powerset [s]
      (set
        (map
          (partial subset s)
          (range (Math/pow 2 (count s))))))]

    (fn [k s]
      (set
        (filter #(= k (count %)) (powerset s))))))

(defcheck solution-70bccbcc
  (fn kcomb [k s]
    (cond
      (> k (count s)) #{}
      (zero? k) #{#{}}
      :else
      (loop [n 0, s' s, res #{}]
        (if (< n (count s))
          (recur (inc n)
            (rest s')
            (clojure.set/union
              res
              (set (for [t (kcomb (dec k) (set (rest s')))]
                     (conj t (first s'))))))
          res)))))

(defcheck solution-716bb3e4
  (fn c [k s]
    (if (zero? k) #{#{}}
                  (if (> k (count s)) #{}
                                      (let [f (first s)
                                            r (rest s)
                                            small (c (dec k) r)
                                            same (c k r)]
                                        (into same (map #(conj % f) small)))))))

(defcheck solution-718959c8
  (fn subsets [k s]
    (cond
      (or (> k (count s)) (zero? k)) #{}
      (= k 1) (set (map #(set [%]) s))
      (= k (count s)) #{s}
      :else (let [cur (first s)
                  rst (set (rest s))]
              (clojure.set/union
                (subsets k rst)
                (set
                  (map #(conj % cur)
                    (subsets (dec k) rst))))))))

(defcheck solution-718b3cc0
  (fn selections [n c]
    (cond
      (= n 0) #{#{}}
      (> n (count c)) #{}
      :else (set (for [x (range 1 (inc (count c)))
                       d (selections (dec n) (drop x c))]
                   (into #{} (cons (nth (seq c) (dec x)) d)))))))

(defcheck solution-719521db
  (fn combinations [n coll]
    (let [apply-sets (fn [coll] (into #{} (map #(hash-set %) coll)))
          setcoll (apply-sets coll)
          merge-combinations (fn [original subs]
                               (reduce #(clojure.set/union %1 %2)
                                 (for [x subs]
                                   (into #{} (for [y original] (conj x y)))
                                   )
                                 )
                               )]
      (if (= n 1) setcoll
                  (into #{} (filter #(= n (count %)) (loop [res setcoll iters 1]
                                                       (if (= iters n) res
                                                                       (recur (merge-combinations coll res) (inc iters))
                                                                       )
                                                       )))
                  )
      )
    ))

(defcheck solution-71e2bfb4
  (fn k-comb [k xs]
    (cond
      (zero? k) #{#{}}
      (not (seq xs)) #{}
      :else (into (k-comb k (rest xs))
              (set (map #(conj % (first xs))
                     (k-comb (dec k) (rest xs))))))))

(defcheck solution-721087e4
  (fn [k s]
    (loop [i 1 c (for [x s] #{x})]
      (if (< i k)
        (recur (inc i) (filter #(= (inc i) (count %)) (for [x c y s] (conj x y))))
        (set c)))))

(defcheck solution-72c43a40
  (fn k-sets [k a-set]
    (set (filter #(= k (count %))
           (let [a-vec (vec a-set)]
             (loop [index 0
                    set-of-sets #{#{}}]
               (let [curr-val (get a-vec index)]
                 (if (< index (count a-vec))
                   (recur (inc index)
                     (reduce (fn [power-set subset] (conj power-set (conj subset curr-val)))
                       set-of-sets
                       set-of-sets))
                   set-of-sets))))))))

(defcheck solution-72d483fd
  (fn [n s]
    (let [pset
          (let [nth-set
                  (fn [n v]
                    (set
                      (keep-indexed
                        #(if (not (zero? (bit-and (bit-set 0 %) n))) %2)
                        v)))
                v (vec s)
                c (count v)]
            (set (conj (map #(nth-set % v) (range (Math/round (Math/pow 2 c)))) #{})))]
      (set (filter #(= (count %) n) pset)))))

(defcheck solution-733c81cf
  (fn [k set]
    (let [coll (into [] set)]
      (letfn [(comb-aux [k start]
                (if (= 1 k)
                  (for [x (range start (count coll))]
                    (conj #{} (set (nth coll x))))
                  (for [x (range start (count coll))
                        xs (comb-aux (dec k) (inc x))]
                    (into #{} (conj xs (nth coll x))))))]
        (into #{} (comb-aux k 0))))
    ))

(defcheck solution-7371863c
  (fn k-comb [k S]
    (if (> k (count S)) #{}
                        (loop [l (- (count S) k), T [S]]
                          (if (zero? l) (set T)
                                        (recur (dec l) (set
                                                         (mapcat #(map (partial disj %) %) T))))))))

(defcheck solution-73dba2c0
  (fn [k els]
    (let [ps (fn [s]
               (reduce
                 (fn [results el]
                   (apply conj results
                     (map #(conj % el) results)))
                 #{#{}}
                 s))]
      (set (filter #(= k (count %)) (ps els)))
      )))

(defcheck solution-7492a886
  (fn gkc [n xs]
    (if (> n (count xs))
      #{}
      (loop [xs xs result #{#{}}]
        (if (empty? xs)
          (set (filter #(= n (count %)) result))
          (recur (rest xs) (apply conj result (map #(conj % (first xs)) result))))))))

(defcheck solution-751af9e2
  (fn [n s]
    (letfn [(i [c w]
              (if (contains? c w)
                c
                (i (reduce (fn [v x] (into v (map #(conj x %) s))) c c) w)
                ))]
      (set (filter #(= n (count %)) (i #{#{}} s))))))

(defcheck solution-75225a18
  (fn [cnt arr]
    (->> (range (Math/pow 2 (count arr)))
      (map (fn f [v] (if (< v 2) [v] (cons (mod v 2) (f (int (/ v 2)))))))
      (filter #(= (count (filter #{1} %)) cnt))
      (map (fn [v] (filter identity (map #(if (= %2 1) %1) arr v))))
      (map set)
      (set))))

(defcheck solution-75b2f7ec
  (fn k-comb
    [k s]
    (if (> k (count s))
      #{}
      (letfn [(subsets
                [superset base]
                (let [a (into #{} (map (fn[e] #{e}) superset))]
                  (into #{}
                    (filter #(= (dec (count (first base))) (count %))
                      (mapcat (fn [e] (map (fn [i] (clojure.set/difference e i)) a)) base)))))]
        (loop [r #{s}
               c (count s)]
          (if (= c k)
            r
            (recur (subsets s (filter #(= (count %) c) r))
              (dec c))))))))

(defcheck solution-75cf5634
  (fn kc [n s]
    (cond
      (or (= n 0) (empty? s) (> n (count s))) #{}
      (= n (count s)) #{s}
      (= n 1) (set (for [i s] #{i}))
      :else (reduce into #{} (for [i s] (map #(into % #{i}) (kc (dec n) (remove #{i} s)))))
      )
    ))

(defcheck solution-7603885a
  (fn kcomb [n s]
    (if (= n 0)
      #{#{}}
      (set
        (apply concat
          (for [itm s]
            (map #(conj % itm) (kcomb (dec n) (disj s itm)))))))))

(defcheck solution-766a45bd
  (fn kcomb [n s]
    (if (= 1 n)
      (set (map #(set [%]) s))
      (reduce (fn [acc i]
                (into acc (->> (kcomb (dec n) (disj s i))
                            (map #(conj % i))))) #{} s))))

(defcheck solution-76c1535f
  (fn c [n s]
    (let [[x & m] (seq s)]
      (if x
        (into (c n m) (map #(conj % x) (c (dec n) m)))
        (if (= n 0) #{#{}} #{})))))

(defcheck solution-7734a3e7
  (fn [n s]
    (set (filter
           #(= (count %) n)
           (reduce (fn [res x]
                     (concat res (map #(conj % x) res)))
             #{#{}} s)))))

(defcheck solution-77cf0bb3
  (fn k-combinations [k s]
    (cond (== k 0)  #{}
          (== k 1)  (into #{} (for [x s] #{x}))
          :else
          (let [k (dec k)]
            (into #{}
              (for [x s combination (k-combinations k (disj s x))]
                (conj combination x)))))))

(defcheck solution-78980e7b
  (fn p-103 [n coll]
    (into #{}
      (filter #(= n (count %))
        (reduce (fn [power-set new-elem]
                  (clojure.set/union
                    power-set
                    (map #(conj % new-elem) power-set)))
          #{#{}}
          coll)))))

(defcheck solution-78ce70da
  (fn combinations
    ([n items]
     (let [powerset (fn [s]
                      (reduce
                        (fn [init e]
                          (set (concat init (map #(conj % e) init) [#{e}])))
                        #{#{}} s))](combinations n (powerset items) #{})))
    ([n powerset init]
     (if (empty? powerset)
       init (if (= n (count (first powerset)))
              (recur n (rest powerset) (conj init (first powerset)))
              (recur n (rest powerset) init))))))

(defcheck solution-7a654ee9
  (fn ncomb [n s]
    (cond
      (> n (count s)) #{}
      (= n 1) (set (map #(hash-set %) s))
      :else
      (set (apply concat
             (for [item s]
               (let [ ncomb-rest (ncomb (dec n) (disj s item))]
                 (map (fn [x] (conj x item)) ncomb-rest))))))))

(defcheck solution-7ad4ff5a
  (fn __ [k coll]
    ((fn dfs [i l r]
       (cond
         (> i (count l)) #{}
         (= i 0) #{r}
         :else (apply merge (dfs i (rest l) r) (dfs (dec i) (rest l) (merge r (first l))))))
     k coll #{})))

(defcheck solution-7b65acfa
  (letfn [(combinations [n s]
            (if (zero? n)
              #{#{}}
              (set
                (for [element s
                      combs (combinations (dec n)
                              (disj s element))]
                  (conj combs element)))))]
    (fn [num-elements orig-set]
      (if (> num-elements (count orig-set))
        #{}
        (combinations num-elements orig-set)))))

(defcheck solution-7ba4cf5f
  (fn [n ss]
    (letfn [(powerset [s]
              (letfn [(combine [acc x]
                        (conj (into acc (map #(conj % x) acc)) #{x}))]
                (conj (reduce combine #{} s) #{})))
            ]
      (set (filter #(= n (count %)) (powerset ss))))))

(defcheck solution-7bfba6f5
  (fn combinations [k s]
    (cond
      (zero? k) #{#{}}
      (> k (count s)) #{}
      true (let [x (first s) s1 (rest s)]
             (set (clojure.set/union (combinations k s1)
                    (map #(conj % x) (combinations (dec k) s1))))))))

(defcheck solution-7c172585
  (fn k-comb [k s]
    (let [sq (into [] s)]
      (cond
        (= k 0) #{#{}}
        (> k (count sq)) #{}
        :else (set (clojure.set/union (k-comb k (set (rest sq)))
                     (set (map #(set (cons (first sq) %)) (k-comb (dec k) (set (rest sq)))))))))))

(defcheck solution-7c2ce0c5
  (fn k-combo
    ([n acc coll]
     (when (not-empty coll)
       (let [n (dec n)
             tails #(take-while not-empty (iterate rest %))
             cs (for [[x & xs] (tails coll)] [n (cons x acc) xs])]
         (if (zero? n) (map (fn [[_ acc _]] acc) cs)
                       (mapcat #(apply k-combo %) cs)))))
    ([n coll]
     (set (map set (k-combo n '() (vec coll)))))))

(defcheck solution-7c4dc2d
  (fn k-combinations [n s]
    (cond (= n 0) #{#{}}
          (empty? s) #{}
          :else (reduce conj
                  (k-combinations n (rest s))
                  (map #(conj % (first s))
                    (k-combinations (dec n) (rest s)))))))

(defcheck solution-7c70073b
  (fn [ k cols ]
    (letfn [ (remove-nth [ col n ] (set (concat (take n col) (drop (inc n) col))))
            (kc [k cols]
              (cond
                (< (count (first cols)) k ) {}
                (= (count (first cols)) k ) cols
                :else (recur k (mapcat
                                 #(for [i (range (count %)) ] (remove-nth % i)) cols))))]
      (into #{} (kc k #{cols})))))

(defcheck solution-7c886cd1
  (fn [size input]
    (let [items (repeat size input )]
      (letfn [(f [i]
                (if (empty? i)
                  '(())
                  (for [x (first i)
                        more (f (rest i))]
                    (into #{} (cons x more)))))]
        (into #{} (filter #(= size (count %))  (f items)))))))

(defcheck solution-7d2df5b
  (fn kc [num col]
    (loop [n 1 result [#{}]]
      (if (> n num)
        (set result)
        (recur
          (inc n)
          (filter
            #(= n (count %))
            (for [x result y col] (conj x y))))))))

(defcheck solution-7d3f21a4
  (fn [n coll]
    (loop [input coll result [[]]]
      (if (empty? input)
        (set (map #(set %) (filter #(= (count %) n) result)))
        (recur (rest input)
          (concat result
            (map #(conj % (first input)) (filter #(< (count %) n) result))))))))

(defcheck solution-7d488273
  (fn one-hundred-three [k s]
    (cond
      (> (count s) k)
      (->> s
        (map #(one-hundred-three k (disj s %)))
        (apply clojure.set/union))

      (= (count s) k)
      #{s}

      :else #{})))

(defcheck solution-7d8fc2fb
  (fn prob103
    [num seq]
    (set (map set
           (cond
             (= num 0) '(())
             (empty? seq) '()
             :else (concat
                     (map (fn [it] (cons (first seq) it)) (prob103 (dec num) (rest seq)))
                     (prob103 num (rest seq))))))))

(defcheck solution-7d90b606
  (fn k-combination
    ([k orig-set] (k-combination k orig-set #{}))
    ([k orig-set form-set]
     (if (= orig-set [])
       (if (= (count form-set) k)
         #{form-set}
         #{})
       (let [s1 (conj form-set (first orig-set))]
         (clojure.set/union
           (if (<= (count form-set) k)
             (k-combination k (rest orig-set) form-set)
             #{})
           (if (<= (count s1) k)
             (k-combination k (rest orig-set) s1)
             #{})))))))

(defcheck solution-7da65583
  (let [binc (fn binc [i]
               (let [[i' carry]
                     (reduce
                       (fn [[i' carry] d]
                         (if (= 0 carry)
                           [(conj i' d) 0]
                           (if (= 1 d)
                             [(conj i' 0) 1]
                             [(conj i' 1) 0])))
                       [[] 1] i)]
                 (if (= 1 carry)
                   (conj i' 1)
                   i')))
        count1s (fn [i] (count (filter (partial = 1) i)))
        permi (fn f [i n]
                (if (= n (count1s i))
                  (cons i (lazy-cat (f (binc i) n)))
                  (lazy-cat (f (binc i) n))))
        perm (partial permi [0])
        permute (fn [c i]
                  (set (mapv first (filter (comp (partial = 1) second) (mapv vector c i)))))]
    ;; not too efficient but oh well, simple and easy
    (fn [k s]
      (let [s (vec s)
            p (perm k)
            n (count s)
            p' (take (/ (reduce * (range (inc (- n k)) (inc n)))
                       (reduce * (range 1 (inc k))))
                 p)]
        (set (map (partial permute s) p'))))))

(defcheck solution-7e00bb00
  (fn __ [n coll]
    (let [occ (fn occ [x] (if (zero? x) 0 (+ (bit-and x 1) (occ (quot x 2)))))
          fetch (fn [coll n]
                  (first
                    (reduce (fn [[acc n'] el]
                              (cond
                                (zero? n') [acc n']
                                (zero? (bit-and n' 1)) [acc (quot n' 2)]
                                :else [(conj acc el) (quot n' 2)])
                              ) [#{} n] coll)))]
      (->> (range (bit-shift-left 1 (count coll)))
        (map #(if (= n (occ %)) (fetch coll %) nil))
        (filter (complement nil?))
        set))))

(defcheck solution-7e2837f8
  (fn kc [n s]
    (set (filter #(= n (count %)) (map set (reduce #(concat (map (fn [e] (cons %2 e)) %1) %1) #{#{}} s))))))

(defcheck solution-7e522669
  (fn kcomb [c s]
    (letfn [(power [s] (set (loop [[f & r] (seq s) p '(#{})]
                              (if f (recur r (concat p (map #(conj % f) p)))
                                    p))))
            ]
      (let [power-sets (power s)
            filtered-sets (filter  #(= (count  %) c) power-sets)
            ]
        (into #{} filtered-sets)
        )
      )
    ))

(defcheck solution-7e74ae4a
  (fn comb [k sq]
    (cond (> k (count sq)) #{}
          (= k 1) (set (map #(conj #{} %1) sq))
          :else (into
                  (set (map #(conj %1 (first sq)) (comb (dec k) (rest sq))))
                  (comb k (rest sq))))))

(defcheck solution-7ef3b308
  (fn [n s]
    (if (or (zero? n) (> n (count s)))
      #{}
      (nth (iterate (fn [res]
                      (reduce (fn [acc x]
                                (reduce (fn [acc2 x2]
                                          (if (x x2) acc2 (conj acc2 (conj x x2))))
                                  acc s))
                        #{} res))
             (reduce (fn [acc x]
                       (conj acc #{x}))
               #{} s))
        (dec n)))))

(defcheck solution-7f1f3fab
  (fn [n s]
    (->> (reduce (fn [s el] (mapcat #(vector % (conj % el)) s)) #{#{}} s)
      (filter #(= n (count %))) set)))

(defcheck solution-7f345473
  (fn k-comb [n coll]
    (letfn [(k-comb* [n coll aggr]
              (letfn [(conj-h [a c]
                        (for [o a i c] (conj o i)))]
                (if (= n 1) (conj-h aggr coll)
                            (let [count (dec n)
                                  rem (rest coll)
                                  agg (conj-h aggr coll)]
                              (recur count rem agg)))))]
      (set
        (filter #(= (count %) n)
          (k-comb* n coll #{#{}}))))))

(defcheck solution-7f6826d8
  (fn [k ss]
    (->> ss
      (repeat k)
      (reduce    (fn [ret, xs] (into ret (for [x ret, y xs] (conj x y))))      #{#{}})
      (filter #(= k (count %)))
      (set))
    ))

(defcheck solution-7f7b4d15
  (fn kc [k S]
    (let [f (fn [S xs] (set (map #(conj S %) xs)))]
      (cond
        (or (zero? k) (> k (count S))) #{}
        (= 1 k)  (f #{} S)
        :else
        (set (filter #(= k (count %)) (set (mapcat #(f % S) (kc (dec k) S)) )))))))

(defcheck solution-7f80ea85
  (fn [k s]
    (loop [result #{#{}}]
      (let [next (into result
                   (apply concat (map (fn [x] (map #(conj x %) s))
                                   result)))]
        (if (= next result)
          (into #{} (filter #(= (count %) k) result))
          (recur next))))))

(defcheck solution-80443fcc
  (fn [l s]
    (letfn [(power-set [s]
              (if (empty? s)
                #{#{}}
                (let [e (first s)
                      r (rest s)
                      s1 (power-set r)
                      s2 (map #(conj % e) s1)]
                  (into s1 s2))))]
      (into #{} (filter #(= l (count %)) (power-set s))))))

(defcheck solution-804cb5ec
  (fn solve [n s]
    (set
      (filter #(= n (count %))
        (reduce (fn [res x]
                  (concat res
                    (map #(set (conj % x)) res)))
          #{#{}}
          s)))))

(defcheck solution-80ac28df
  (fn kombs
    [k s]
    (let [init (into #{} (map #(set (list %)) s))
          f (fn [prefix st]
              (let [avail (filter (fn [x] (not (true? (some #(= x %) prefix)))) st)]
                (map #(conj prefix %) avail)))
          f2 (fn [acc k st]
               (if (= 0 k) acc
                           (recur (into #{} (mapcat #(f % st) acc)) (dec k) st)))]
      (f2 init (dec k) s))))

(defcheck solution-80ae5df
  (fn [n c]
    (->> (iterate #(for [s % x c] (conj s x)) [#{}])
      (take (inc n)) last
      (filter #(= n (count %))) set)))

(defcheck solution-80de65fb
  (fn [n s]
    (let [pwr (fn pwr [s]
                (if (empty? s)
                  #{#{}}
                  (let [ls (into '() s)
                        rst (into '() (pwr (set (rest ls))))]
                    (set (concat rst
                           (map #(conj % (first ls)) rst))))))]
      (set (filter #(= (count %) n) (pwr s))))))

(defcheck solution-8145f3e2
  (fn comb [k coll]
    (cond
      (= k 0) #{}
      (> k (count coll)) #{}
      (= k 1) (set (for [x coll] #{x}))
      :else (apply clojure.set/union
              (set (for [x coll]
                     (clojure.set/union
                       (comb k (disj coll x))
                       (set (map #(conj % x) (comb (dec k) (disj coll x))))))))
      )))

(defcheck solution-817d86f2
  (letfn [(power-set2 [s]
            (let [s-ind (map-indexed vector s)
                  decode (fn [n]
                           (into #{} (map second
                                       (filter #(not= 0 (bit-and n (bit-shift-left 1 (first %)))) s-ind))))]
              (into #{} (map decode (range (bit-shift-left 1 (count s)))))))
          (k-comb
            [k els]
            (set (filter #(= k (count %)) (power-set2 els)))
            )
          ]
    k-comb))

(defcheck solution-8192206e
  (fn ps [n s]
    (set (filter #(= n (count %))
           (loop [res #{#{}} li s]
             (if (empty? li)
               res
               (recur
                 (apply merge res
                   (for [x res]
                     (clojure.set/union x (hash-set (first li)))))
                 (rest li))))))))

(defcheck solution-81924fbc
  (fn [n coll] (->>
                 [coll]
                 (iterate #(for [c % x c] (disj c x)))
                 (take-while #(>= (count (first %)) n))
                 last
                 set
                 )))

(defcheck solution-81aa009
  (letfn [(f [n s]
            (cond
              (= 0 n) #{#{}}
              (= s []) #{}
              1 (into (f n (rest s))
                  (map #(conj % (first s)) (f (- n 1) (rest s))))))]
    f))

(defcheck solution-81d00d33
  (fn k [n s]
    (cond
      (> n (count s)) #{}
      (= n 0) #{#{}}
      :else (set (mapcat (fn [e] (map #(conj % e)
                                   (k (- n 1) (disj s e))))
                   s)))))

(defcheck solution-8202cdea
  #(nth (iterate (fn [t] (set (remove nil? (for [s t x %2] (if (s x) nil (conj s x))))))
          [#{}]) %))

(defcheck solution-8211561d
  (fn [n s]
    (set (filter #(= n (count %))
           (reduce (fn [c e]
                     (concat c (map #(conj % e) c)))
             [#{}] s)))))

(defcheck solution-821b7988
  (fn f [n s] (if (= n 0) #{#{}} (set (for [x s y (f (dec n) (disj s x))] (conj y x))))))

(defcheck solution-82c30b1f
  (fn k [n vs]
    (cond (or (= n 0) (> n (count vs))) #{}
          (= n 1) (set (for [v vs] #{v}))
          :else (set (for [v vs ks (k (- n 1) (disj vs v))] (conj ks v))))))

(defcheck solution-82e9c696
  (fn [n x] (set (filter #(= n (count %)) ((fn p [s] (if (empty? s) #{#{}} (let [e (first s) t (disj s e)] (into (p t) ((fn f [e t] (into #{} (map #(conj % e) t))) e (p t)))))) x)))))

(defcheck solution-832a6237
  (fn [n s]
    (let [setcomb (fn [n, s, e] (set (remove #(> (count %) n) (into s (map #(conj % e) s)))))]
      (->> (reduce (partial setcomb n) (set (map #(set [%]) s)) s) (remove #(< (count %) n)) set))))

(defcheck solution-839ae964
  (fn subsets [n s]
    (cond
      (zero? n) #{#{}}
      (empty? s) #{}
      :else
      (into #{}
        (concat
          (map #(conj % (first s))
            (subsets (dec n) (rest s)))
          (subsets n (rest s)))))))

(defcheck solution-83af750f
  (fn R [n S] (set (cond (= 1 n) (map (comp set list) S) (<= 1 n (count S)) (let [h (first S) T (rest S)] (into (R n T) (map #(conj % h) (R (- n 1) T))))))))

(defcheck solution-83b50bcd
  (fn kc [k c]
    (set
      (condp = k
        0 #{}
        1 (map (comp set vector) c)
        (for [a (kc 1 c),
              b (kc (dec k) c)
              :let [ab (into a b)]
              :when (= k (count ab))]
          ab)))))

(defcheck solution-847d7a0c
  (fn [k coll]
    (->> (nth (iterate
                #(for [x % y coll] (set (concat x [y])))
                (map #(set [%]) coll))
           (dec k))
      (filter #(= k (count %)))
      set)))

(defcheck solution-84c1a85a
  (fn kcomb [k s]
    (if (or (zero? k) (empty? s)) #{}
                                  (if (= 1 k) (set (map (fn [x] #{x}) s))
                                              (clojure.set/union (kcomb k (next s))
                                                (set (map #(clojure.set/union #{(first s)} %)  (kcomb (dec k) (next s)))))))))

(defcheck solution-84dd23e8
  (fn k [n coll]
    (cond
      (= n (count coll)) #{(set coll)}
      (> n (count coll)) #{}
      :else (let [x (first coll)]
              (set
                (apply concat
                  (for [y (k n (next coll))]
                    `[~y ~@(for [z y] (conj (disj y z) x))])))))))

(defcheck solution-84e1a8d5
  (fn [num start-set]
    (letfn [(power-set [input]
              (if (empty? input)
                #{#{}}
                (reduce
                  (fn ! [a b]
                    (if (set? a)
                      (loop [result (into #{} a) elements a]
                        (if elements
                          (recur (conj result (conj (first elements) b)) (next elements))
                          result)
                        )
                      (! #{#{} #{a}} b)
                      )
                    ) input)
                )
              )]
      (set (filter #(= num (count %))  (power-set start-set)))
      )
    ))

(defcheck solution-84e7c880
  (fn kc [k xs]
    (cond
      (= 0 k) #{#{}}
      (empty? xs) #{}
      :else
      (let [y (first xs) ys (rest xs)]
        (set (concat (kc k ys) (map #(conj % y) (kc (dec k) ys))))))))

(defcheck solution-85361f35
  (fn generate-k-combos [k coll]
    (if (> k (count coll))
      #{}
      (set
        (filter
          #(= k (count %))
          ((fn powerset [s]
             (reduce
               #(into % (for [subset %]
                          (conj subset %2)))
               #{#{}}
               s))
           coll))))))

(defcheck solution-853656f
  (fn combinations
    ([k xs] (combinations k xs #{}))
    ([k xs sofar]
     (cond (zero? k) #{sofar}
           (zero? (count xs)) #{}
           :else (clojure.set/union
                   (combinations (dec k) (rest xs) (conj sofar (first xs)))
                   (combinations k (rest xs) sofar))))))

(defcheck solution-853af612
  (fn [n s]
    (loop [n n
           r #{#{}}]
      (if (= 0 n)
        r
        (recur
          (dec n)
          (disj (into #{}
                  (for [e s
                        ss r]
                    (if (contains? ss e)
                      nil
                      (if (nil? ss) #{e} (conj ss e)))))
            nil))))))

(defcheck solution-8589d1f2
  (fn k-comb [n coll]
    (set (if (or (>= 1 n) (empty? coll))
           (map (comp set list) coll)
           (->> (k-comb (dec n) (rest coll))
             (map #((comp set cons) (first coll) %))
             (concat (k-comb n (rest coll))))))))

(defcheck solution-859a29ea
  (fn [n set-of-items]
    (let [step (fn [set-of-sets]
                 (set (for [a-set set-of-sets, an-item set-of-items
                            :when (not (contains? a-set an-item))]
                        (conj a-set an-item))))]

      (if (> n (count set-of-items))
        #{}
        (do #_(println "hello") (reduce
                                (fn [acc _] (step acc))
                                (set (map #(conj #{} %) set-of-items))
                                (range 1 n)))))))

(defcheck solution-85c5c2e1
  (fn ff [n v]
    (into #{}
      (filter #(= n (count %)) (seq ((fn f[v]
                                       (into #{} (if (empty? v)
                                                   [#{}]
                                                   (concat []
                                                     (map #(conj % (first v)) (f (next v)))
                                                     (f (next v)))))) v))))))

(defcheck solution-85c6e0fd
  (fn kcombos [n s]
    (if (= 0 n) #{#{}}
                (reduce into #{}
                  (map (fn [x] (map #(conj % x)
                                 (kcombos (dec n)
                                   (disj s x)))) s)))))

(defcheck solution-860043db
  (fn [n xss]
    (let [xs (set (map #(hash-set %) xss))]
      (into #{}
        (reduce
          (fn [acc i]
            (->>
              (for [x acc y acc] (apply conj x y))
              (filter #(= (count %) i))))
          xs (range 1 (inc n)))))))

(defcheck solution-86277d06
  (fn create-k-combos [k s]
    (let [add-to-sets (fn [sets s k]
                        (let [result (for [a sets
                                           b (seq s)]
                                       (conj a b))]
                          (into #{} (filter #(= k (count %)) result))))]
      (if (= 0 k)
        #{}
        (loop [ss #{#{}}
               n 1]
          (let [sets (add-to-sets ss s n)]
            (if (= n k)
              sets
              (recur sets (inc n)))))))))

(defcheck solution-8627cc7f
  (fn [n s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (cond
        (< (count s) n) #{}
        (= (count s) n) (hash-set (set s))
        :else (set (filter #(= (count %) n)
                     ((reduce #(p % %2 s) {} (range (count s))) n s)))))))

(defcheck solution-862d8cb8
  (fn k-comb [n coll]
    (cond
      (= n 0)
      #{#{}}
      (> n (count coll))
      #{}
      (= n (count coll))
      #{(set coll)}
      :else
      (into (k-comb n (rest coll))
        (map #(conj % (first coll)) (k-comb (dec n) (rest coll)))))))

(defcheck solution-864b7151
  (fn k-comb [size original-set]
    (letfn [(step [left-depth path-set remain-set]
              (if (zero? left-depth)
                (-> path-set set list set)
                (if (empty? remain-set)
                  #{}
                  (set
                    (apply concat (map #(step (dec left-depth) (conj path-set %) (disj remain-set %)) remain-set) )))))]
      (step size #{} original-set))))

(defcheck solution-8683ec66
  (fn [n s]
    (if (or (= 0 n) (> n (count s)) (empty? s)) #{}
                                                (loop [n (- n 1), r (map hash-set s)]
                                                  (if (= 0 n) (set r)
                                                              (recur (dec n)
                                                                (for [x r, y s :when (not (contains? x y))] (conj x y))
                                                                ))))))

(defcheck solution-86b169e2
  (fn [n start]
    ((fn k [s]
       (cond
         (empty? s) #{}
         (= (count s) n) #{(set s)}
         :else (let [i (first s)
                     p (k (rest s))]
                 (into p (for [col p :let [c (set col)] x col] (conj (disj c x) i))))
         ))
     start)))

(defcheck solution-8735cc0a
  (fn [n s]
    (let [f (fn f [s]
              (if (empty? s)
                #{#{}}
                (set (concat (map #(conj % (first s)) (f (next s))) (f (next s))))))
          ]
      (set (filter #(= n (count %)) (f s)))
      )))

(defcheck solution-87a4df22
  (fn
    [n s]
    (letfn [(powerset [s]
              (apply clojure.set/union
                #{s}
                (map #(powerset (disj s %)) s)))]
      (set (filter #(= n (count %)) (powerset s))))))

(defcheck solution-87e61eb2
  (fn [n z]
    (set (filter #(= (count %) n)
           (reduce (fn [seqs b]
                     (mapcat
                       (fn [s] (#(map (partial conj %) z) s)) seqs)) [#{}] (range n))))))

(defcheck solution-884cf279
  #(
    (fn f [n s1 sn]
      (if (zero? n)
        sn
        (f (dec n)
          s1
          (set (remove nil?
                 (for [x1 s1 x2 sn]
                   (when-not (x2 x1)
                     (clojure.set/union #{x1} x2))))))))  % %2 #{#{}}))

(defcheck solution-886c5ebf
  (fn f
    ([n s] (set (flatten (f n (seq s) nil))))
    ([n v o]
     (cond
       (zero? n) (set o)
       (<= n (count v))
       (for [i (range (inc (- (count v) n)))]
         (f (dec n)
           (drop (inc i) v)
           (cons (nth v i) o)))))))

(defcheck solution-88764e49
  (fn[k in-set]
    (if (> k (count in-set)) #{}
                             (loop [k-set #{in-set}]
                               (if (= k (count (first k-set))) k-set
                                                               (let [next-k-set (set (reduce #(clojure.set/union %1 %2)
                                                                                       (for [one-set k-set]
                                                                                         (for [elem one-set] (disj one-set elem)))))]
                                                                 (recur next-k-set)))))))

(defcheck solution-88e1670a
  (fn kcomb [n s]
    (if (zero? n)
      #{#{}}
      (set (flatten (for [ x s
                          :let [newset (disj s x)
                                subsets (kcomb (dec n) newset)] ]
                      (map #(conj % x) subsets)))))))

(defcheck solution-895de5e4
  (fn [n someset]
    (nth (iterate (fn[y](set (flatten (map (fn[x](map #(disj x %) x)) y)))) (hash-set someset)) (let[ z (count someset)](if (>= z n) (- z n) n) ))))

(defcheck solution-8988bf74
  (fn n103 [n coll]
    (if (> n (count coll))
      #{}
      (set (filter #(= n (count %))
             (loop [k n a #{#{}}]
               (if (zero? k)
                 a
                 (recur
                   (dec k)
                   (set (mapcat identity (for [x (conj (map #(set [%]) coll) #{})] (map #(clojure.set/union % x) a))))))))))))

(defcheck solution-89b0c5ac
  (fn [n s]
    (set (filter #(= n (count %))
           (reduce
             (fn [a b]
               (into a (map #(conj % b) a)))
             #{#{}} s)))))

(defcheck solution-89dda076
  (fn k-comb [n s]
    (let [c (count s) parts (partition (if (= n 1) n (dec n)) (take (* 3 c) (cycle s)))]

      (cond (> n c) #{}
            :else
            (set (filter #(= n (count %1)) (mapcat (fn [p]
                                                     (map #(set (cons %1 p)) s)) parts )))))))

(defcheck solution-8a4ccad7
  (fn [k s]
    (let [n (count s)
          pow-2 (fn p2 [k] (if (= 0 k) 1 (* 2 (p2 (dec k)))))
          rec-int-to-set (fn ris [n c r]
                           (if (= 0 n)
                             r
                             (ris
                               (bit-shift-right n 1)
                               (inc c)
                               (if (= 1 (bit-and 0x01 n)) (conj r c) r))))
          int-to-set #(rec-int-to-set %1 0 #{})
          brute-k-combinations (fn [n k]
                                 (cond
                                   (> k n) #{}
                                   (= k 0) #{#{}}
                                   (= k n) (conj #{} (set (range n)))
                                   :else
                                   (filter #(= k (count %1)) (map int-to-set (range (pow-2 n))))))
          get-fun (fn [s] (fn [is] (set (map #((vec s) %1) is))))
          xfer (get-fun s)
          ]
      (set (map xfer (brute-k-combinations n k))))))

(defcheck solution-8b1de2f6
  (fn [n s]
    (letfn [(k-combs [n s]
              (cond (<= n 0) #{#{}}
                    (> n (count s)) #{}
                    :default
                    (loop [s s a #{}]
                      (if (seq s)
                        (recur
                          (rest s)
                          (concat a
                            (for [L (k-combs (dec n) (rest s))]
                              (conj L (first s))
                              )))
                        (set a)))))]
      (k-combs n s))))

(defcheck solution-8b7a869
  #(cond
     (> %1 (count %2)) #{} ;one short cut
     (= %1 (count %2)) #{%2} ;another short cut
     :else (loop [result #{#{}} round 0] ;an iterative solution
             (if (= round %1) result
                              (recur (apply conj #{}
                                       (for [x result y %2 :when (not (contains? x y))]
                                         (conj x y)))
                                (inc round))))))

(defcheck solution-8ba3c48
  (fn f [n s]
    (let [ps (fn p [s] (if (= #{} s) #{#{}}
                                     (let [res (p (disj s (first s)))]
                                       (into res (map #(conj % (first s)) res)))))]
      (set (filter #(= n (count %)) (ps s)))
      )))

(defcheck solution-8bdc3ae5
  (letfn
   [(b [n]
      (if (= 0 n)
        '([])
        (for [f [1 0]
              r (b (dec n))]
          (cons f r))))]
    (fn [n s]
      (let [e (vec s)
            p (filter #(= n (reduce + %))
                (b (count e)))]
        (set (for [i p]
               (set (keep identity (map
                                     (fn [b v]
                                       (when (= b 1) v))
                                     i e)))))))))

(defcheck solution-8d8b8dea
  (fn [n s]
    (set
      (filter #(= (count %) n)
        (nth
          (iterate (fn [x]
                     (set (mapcat (fn [e] (map #(conj % e) x)) s)))
            #{#{}})
          n)))))

(defcheck solution-8e29adbd
  (fn combi
    [n coll]
    (if (> n 1)
      (into #{}
        (mapcat
          (fn [x i]
            (map (fn [s] (conj s x))
              (combi (dec n) (nthnext coll (inc i)))))
          (drop-last (dec n) coll)
          (range)))
      (into #{} (map hash-set coll)))))

(defcheck solution-8e68daf7
  (fn __ [n s]
    (cond
      (zero? n) #{#{}}
      (> n (count s)) #{}
      (= n (count s)) #{s}
      :else
      (->> s
        (map #(map
                (fn [r] (conj r %))
                (__ (dec n) (disj s %))))
        (apply concat)
        set))))

(defcheck solution-8ee94999
  (fn [n items]
    "See also:  https://github.com/clojure/math.combinatorics/blob/master/src/main/clojure/clojure/math/combinatorics.clj"
    (let [v-items (vec (reverse items)),
          index-combinations (fn [n cnt]
                               (lazy-seq
                                 (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
                                       iter-comb (fn iter-comb [c j]
                                                   (if (> j n) nil
                                                               (let [c (assoc c j (dec (c j)))]
                                                                 (if (< (c j) j) [c (inc j)]
                                                                                 (loop [c c, j j]
                                                                                   (if (= j 1) [c j]
                                                                                               (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
                                       step (fn step [c j]
                                              (cons (rseq (subvec c 1 (inc n)))
                                                (lazy-seq (let [next-step (iter-comb c j)]
                                                            (when next-step (step (next-step 0) (next-step 1)))))))]
                                   (step c 1))))]
      (if (zero? n) #{}
                    (let [cnt (count items)]
                      (cond (> n cnt) #{}
                            (= n cnt) (into #{} (map #(into #{} %1) (list (seq items))))
                            :else (into #{}
                                    (map #(into #{} %1)
                                      (map #(map v-items %) (index-combinations n cnt))))))))))

(defcheck solution-8eea3988
  (fn combinations--reduce
    [k coll] {:pre [(integer? k), (not (neg? k))]}
    (->> (seq (set coll))
      (reduce (fn [[done acc] x]
                (let [new-combs
                      (->> (for [comb acc] [(conj comb x) comb])
                        (apply concat)
                        (group-by count))]
                  [(concat done (new-combs k))
                   (->> (dissoc new-combs k)
                     vals
                     (apply concat))]))
        [[] [#{}]])
      first
      set)))

(defcheck solution-8ef09569
  (fn [k s]
    (set (filter
           #(= k (count %))
           (reduce
             (fn [subsets elem]
               (into subsets
                 (for [x subsets]
                   (conj x elem))))
             #{#{}}
             s)))))

(defcheck solution-8f538f45
  (fn kcombs [k s]
    (cond
      (= 0 k) #{#{}}
      (empty? s) #{}
      :else (into #{} (apply concat (for [i s] (map #(conj % i) (kcombs (dec k) (disj s i)))))))
    ))

(defcheck solution-8f75d062
  (fn [n s]
    (set
      (filter #(= n (count %))
        (reduce
          (fn [acc x]
            (clojure.set/union
              acc
              (set (map #(clojure.set/union #{x} %) acc))))
          #{#{}} s)))))

(defcheck solution-8fdd8ae6
  (fn k-comb
    [k s]
    (let [size (count s)]
      (cond
        (> k size) #{}
        (= k size) #{(set s)}
        (= k 1) (set (map hash-set s))
        :else (let [h (first s)
                    r (rest s)]
                (into (k-comb k r) (map #(conj % h) (k-comb (dec k) r))))))))

(defcheck solution-8fe2a97f
  (letfn [(subseqs [s]
            (if (empty? s)
              [#{}]
              (let [rseqs (subseqs (rest s))]
                (concat [#{(first s)}] (map #(cons (first s) %) rseqs)
                  rseqs))))]
    (fn [n s]
      (set (map set (filter #(= (count %) n) (subseqs s)))))))

(defcheck solution-9056de6b
  (fn comb [n c]
    (cond
      (> n (count c)) #{}
      (zero? n) #{#{}}
      :else ((comp set concat) (comb n (rest c)) (map #(conj % (first c)) (comb (dec n) (rest c))))
      )))

(defcheck solution-90c83714
  (fn kcomb [n coll]
    (cond
      (zero? n) #{#{}}
      (> n (count coll)) #{}
      (= n (count coll)) #{coll}
      :else (let [f (first coll) r (into #{}(rest coll))]
              (clojure.set/union
                (kcomb n r)
                (into #{} (map (partial into #{f}) (kcomb (dec n) r))))))))

(defcheck solution-911d3a5c
  (fn [k s]
    (set (filter #(= k (count %))
           (loop [s (vec s) p #{#{}}]
             (if (empty? s)
               p
               (recur (rest s)
                 (into p (map #(merge % (first s)) p)))))))))

(defcheck solution-91b23b6f
  (fn combinacoes [t s]
    (set
      (cond
        (= 1 t) (map hash-set s)
        (= t (count s)) (list s)
        (> t (count s)) nil
        :else (mapcat #(map (fn [combinacao] (set (cons % combinacao)))
                         (combinacoes (dec t)
                           (remove (hash-set %) s)))
                s)))))

(defcheck solution-91fb49c
  (fn [n xs]
    (letfn [(combinations [n]
              (if (zero? n)
                #{#{}}
                (into #{}
                  (mapcat (fn [s] (map (fn [x] (conj s x)) (remove s xs)))
                    (combinations (dec n))))))]
      (combinations n))))

(defcheck solution-9270dc28
  (fn [k coll] (letfn [
                       (conj-to-each [colls x] (map #(conj % x) colls))
                       (k-combinations [k coll]
                         (cond
                           (zero? k)
                           #{#{}}
                           (empty? coll)
                           #{}
                           :else
                           (into
                             (into #{} (conj-to-each (k-combinations (dec k) (rest coll)) (first coll)))
                             (k-combinations k (rest coll))
                             )
                           )
                         )
                       ] (k-combinations k coll))))

(defcheck solution-92846616
  (fn [n s]
    (->> s
      (reduce (fn [acc x] (concat (map #(conj % x) acc) acc)) [#{}])
      (filter #(= n (count %)))
      set)))

(defcheck solution-9301fbf2
  #(into #{} (if (> % (count %2)) []
                                  ((fn f [x] (if (= % (count x)) #{x}
                                                                 (apply concat (for [v x] (f (disj x v)))))) %2))))

(defcheck solution-932aa0a5
  (fn [n s]
    (set (filter #(= (count %) n)
           (reduce (fn [a x] (into a (map #(conj % x) a))) #{#{}} s)))))

(defcheck solution-932bafd8
  (fn [n s]
    (let [subsets (fn [s]
                    (loop [result []
                           remaining s]
                      (if (seq remaining)
                        (recur (concat [[(first remaining)]] result (map #(conj % (first remaining)) result)) (rest remaining))
                        result)))]
      (apply hash-set (map #(apply hash-set %) (filter #(= n (count %)) (subsets s))))
      )
    ))

(defcheck solution-938db3be
  (fn f
    ([k src] (if (< (count src) k) #{} (f k src #{#{}})))
    ([k src acc]
     (if (= k 0)
       acc
       (recur (dec k) src (into #{} (for [a acc s src :when (not (contains? a s))] (conj a s))))))))

(defcheck solution-93be2527
  (fn get-combinations[a-num xs]
    (loop [x a-num
           xs xs
           acc '(#{})]
      (if (or (> x (count xs))(= x 0)) (set (filter #(= a-num (count %))(set acc)))
                                       (recur (dec x)
                                         xs
                                         (for [a xs
                                               b acc]
                                           (conj b a )))))))

(defcheck solution-947f7def
  (fn gkc-103 [n xs]
    (cond
      (> n (count xs)) #{}
      (= n 1) (set (map #(do #{%}) xs))
      (= n (count xs)) #{xs}
      :else (into (gkc-103 n (disj xs (first xs)))
              (map #(conj % (first xs)) (gkc-103 (dec n) (disj xs (first xs))))))))

(defcheck solution-94aaf2c8
  (fn [k given]
    (loop
     [i 1
      accu #{ #{}}]
      (if (> i k)
        accu
        (recur
          (inc i)
          (into #{}
            (for
             [x given
              y accu
              :let [z (conj y x)]
              :when (= i (count z))]
              z)))))))

(defcheck solution-94d47825
  (fn [k s] (let [n (Math/pow 2 (count s))
                  v (vec s)
                  subset (fn [x] (set (remove nil? (for [i (range n)]
                                                     (when (bit-test x i)
                                                       (v i))))))
                  subsets (map subset (range n))]
              (into #{} (filter #(= k (count %)) subsets)))))

(defcheck solution-94e4ab42
  (fn [n s]
    (if (> n (count s)) #{}
                        (set (filter #(= (count %) n)
                               (loop [n n res #{#{}}]
                                 (if (zero? n)
                                   res
                                   (recur (dec n)
                                     (set
                                       (mapcat (fn [x] (map (fn [y] (conj x y)) s))
                                         res))))))))))

(defcheck solution-95930361
  (fn k-combinations [k s]
    (set
      (if (= k 1)
        (map hash-set s)
        (let [combs (k-combinations (dec k) s)]
          (for [ele s, comb combs :when (not (comb ele))]
            (conj comb ele)))))))

(defcheck solution-95a5d98
  (letfn [(subsets [n items]
            (cond
              (= n 0) '(())
              (empty? items) '()
              :else (concat (map
                              #(cons (first items) %)
                              (subsets (dec n) (rest items)))
                      (subsets n (rest items)))))]
    (fn [n coll]
      (->>
        (subsets n (seq coll))
        (map set)
        (set)))
    ))

(defcheck solution-95cadea2
  (fn k [n s]
    (cond
      (<= n 0) [#{}]
      (> n (count s)) #{}
      :otherwise (set (concat (map #(conj % (first s)) (k (dec n) (rest s))) (k n (rest s))))
      )))

(defcheck solution-9605dc01
  (fn fixed-combis [n s]
    (let [combis (fn combis [s]
                   (if (empty? s)
                     #{}
                     (conj
                       (set (for [e s
                                  r (combis (disj s e))]
                              r))
                       s)))]
      (set (filter #(= n (count %)) (combis s))))))

(defcheck solution-97069f4d
  (fn c [k s]
    (cond
      (empty? s) #{}
      (= 1 k) (set (map #(hash-set %) s))
      :else
      (set
        (let [ss (seq s)
              h (first ss)
              t (-> ss rest set)]
          (concat
            (map #(conj % h) (c (dec k) t))
            (c k t)))))))

(defcheck solution-9783fce3
  (fn g [n xs] (cond
                 (> n (count xs)) #{}
                 (= n 0) #{#{}}
                 :else (set (apply concat (for [x xs] (map #(conj  % x)  (g (dec n) (disj xs x)))))))))

(defcheck solution-97a14a49
  (fn [n coll]
    (letfn [(index-combinations
              [n cnt]
              (lazy-seq
                (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
                      iter-comb
                        (fn iter-comb [c j]
                          (if (> j n) nil
                                      (let [c (assoc c j (dec (c j)))]
                                        (if (< (c j) j) [c (inc j)]
                                                        (loop [c c, j j]
                                                          (if (= j 1) [c j]
                                                                      (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
                      step
                        (fn step [c j]
                          (cons (rseq (subvec c 1 (inc n)))
                            (lazy-seq (let [next-step (iter-comb c j)]
                                        (when next-step (step (next-step 0) (next-step 1)))))))]
                  (step c 1))))
            (combinations
              [items n]
              (let [v-items (vec (reverse items))]
                (if (zero? n) (list ())
                              (let [cnt (count items)]
                                (cond (> n cnt) nil
                                      (= n cnt) (list (seq items))
                                      :else
                                      (map #(map v-items %) (index-combinations n cnt)))))))]
      (into #{} (map #(into #{} %) (combinations coll n))))))

(defcheck solution-97f61fd7
  (fn [n s]
    (let [pms (fn pms [s a]
                (if (seq s)
                  (pms (rest s) (clojure.set/union a (for [x s y a] (conj y x))))
                  a))]
      (set (filter #(= n (count %)) (pms s #{#{}}))))))

(defcheck solution-983e139d
  (fn kcombs
    ([k coll] (kcombs k coll #{}  #{} ))
    ([k coll currResult allResults]
     (cond
       (= k 0) (conj allResults currResult)
       (or (empty? coll) (> k (count coll))) allResults
       :else
       (clojure.set/union
         (kcombs (dec k) (rest coll) (conj currResult (first coll)) allResults)
         (kcombs k (rest coll) currResult allResults))))))

(defcheck solution-9902f1ce
  (letfn [(f1 [x1 x2] (for [a x1 b x2] (conj a b)))]
    (fn [n s]
      (loop [i n acc (set (map #(set [%]) s))]
        (if (zero? i)
          (set (filter #(= (count %) n) acc))
          (recur (dec i) (set (f1 acc s))))))))

(defcheck solution-9916f8b0
  (fn kcomb [n st]
    (letfn [(subsets [n items]
              (cond
                (= n 0) #{#{}}
                (empty? items) #{}
                :else (concat (map
                                #(cons (first items) %)
                                (subsets (dec n) (rest items)))
                        (subsets n (rest items)))))]
      (let [allsubsets (for [i (range (inc (count st)))] (subsets i st))]
        (set(filter #(= n (count %)) (reduce #(into %1 (map set %2)) #{} allsubsets)))))))

(defcheck solution-9977297e
  (fn comb [n S]
    (if (= n 0)
      #{#{}}
      (if (seq S)
        (clojure.set/union
          (comb n (next S))
          (into #{} (map #(conj % (first S)) (comb (dec n) (next S)))))
        #{}))))

(defcheck solution-9a608bad
  (fn [k st]
    (set
      (filter #(= (count %) k)
        (reduce (fn [s t] (into s (for [x s] (conj x t))))
          #{#{}}
          st)))))

(defcheck solution-9ac1f4e7
  (fn n-power-set [n s]
    (set (filter #(= n (count %)) (reduce (fn [s e] (into s (map #(conj % e) s))) #{#{}} s)))))

(defcheck solution-9adf1ca1
  (fn [n s]
    (if (= n (count s)) #{s}
                        (letfn
                         [(bits [n]
                            (let [r (rem n 2)]
                              (if (< n 2)
                                [r]
                                (conj (bits (quot n 2)) r))))
                          (fact [n] (reduce * 1 (range 1 (inc n))))]
                          (let [bm (filter #(= n (apply + %))
                                     (map #(concat (repeat (- (count s) (count %)) 0) %)
                                       (map bits (range (- (bit-shift-left 2 (dec (count s))) 1)))))]
                            (set (for [b bm]
                                   (set (map second
                                          (filter (fn [[bs ss]] (= 1 bs))
                                            (map (fn [bs ss] [bs ss]) b s)))))))))))

(defcheck solution-9b3d4edd
  (fn [n x]
    (let [power-set (fn  inner [x]
                      (if (empty? x)
                        #{#{}}
                        (let [half (inner (rest x))]
                          (into half (map #(conj % (first x)) half)))))]
      (set (filter #(= (count %) n)
             (power-set x))))))

(defcheck solution-9bddabce
  (fn k-combinations [k s]
    (cond
      (= k 0) #{#{}}
      (= k (count s)) #{(set s)}
      (> k (count s)) #{}
      :else (clojure.set/union (k-combinations k (rest s))
              (set (map #(conj % (first s))
                     (k-combinations (dec k) (rest s))))))))

(defcheck solution-9c108b34
  (fn k-combos [n s]
    (if (> n (count s))
      #{}
      (->>
        (repeatedly #(rand-int (count s)))
        (partition n)
        (map set)
        (filter #(= n (count %)))
        distinct
        (take (/ (apply * (take n (iterate dec (count s))))
                (apply * (take n (iterate dec n)))))
        (map (comp set (partial map #(nth (into [] s) %))))
        set))))

(defcheck solution-9c244639
  (fn [n s]
    (loop [ss #{s} i (count s)]
      (if (zero? i)
        (set (filter #(= n (count %)) ss))
        (recur
          (into #{s} (mapcat (fn [z] (map #(disj z %) z)) ss))
          (dec i) )))))

(defcheck solution-9c7dda63
  (fn [k s]
    (loop [i #{#{}} j 1]
      (if (> j k)
        (set (filter #(= k (count %)) i))
        (recur (apply clojure.set/union (map (fn [a] (map #(conj a %) s)) i))
          (inc j))))))

(defcheck solution-9c8c7e88
  (fn p103
    ([i ss] (p103 i #{#{}} ss))
    ([i s ss]
     (letfn [(ps [s ss]
               (if (empty? ss) s
                               (ps (reduce conj s (for [se s] (conj se (first ss)))) (next ss))))]
       (apply hash-set (filter #(= i (count %)) (ps s ss)))))))

(defcheck solution-9d467077
  (fn [n s]
    (->> s
      (reduce #(into % (map (fn[s](conj s %2)) %)) #{#{}})
      (filter #(= n (count %1)))
      set)))

(defcheck solution-9d64a66e
  (fn [n s]
    (set (filter #(= n (count %))
           ((fn [s] (reduce (fn [s x] (into s (map #(conj % x) s))) #{#{}} s))
            s)))))

(defcheck solution-9ee91041
  (fn [c s]
    (if (< (count s) c)
      #{}
      (if (= (count s) c)
        (conj #{} s)
        (loop [s s result (set (map (comp set vector) s))]
          (if (empty? s)
            (set (filter #(= (count %) c) result))
            (recur (rest s) (into result (map #(into % (vector (first s))) result)))))))))

(defcheck solution-9f00534e
  (fn subsets [k as]
    (cond
      (zero? k) #{#{}}
      (= (count as) k) #{as}
      :else (set (for [el as, ss (subsets (dec k) (disj as el))] (conj ss el))))))

(defcheck solution-9fa423c2
  (fn ex103
    [n s]
    (letfn [(powerset [s]
              (if (= 2 (count s))
                (set (conj (map (comp set vector) s) s #{}))
                (let [subsets (for [i s]
                                (clojure.set/difference s #{i}))]
                  (conj (set (apply concat (map powerset subsets))) s))))]
      (set (filter #(= (count %) n) (powerset s))))))

(defcheck solution-a0137e67
  (letfn [(f [n s]
            (if (or (> n (count s)) (= n 0))
              '(#{})
              (for [i s
                    rst (f (dec n) (disj s i))]
                (conj rst i))))]
    #(set (remove empty? (f %1 %2)))))

(defcheck solution-a062f6c3
  (fn k-comb [n s]
    (loop [k n, r #{#{}}]
      (if (< k 1)
        r
        (recur
          (dec k)
          (set (filter (comp not nil?)
                 (for [x r y s]
                   (if (contains? x y) nil (conj x y))))))))))

(defcheck solution-a090b5be
  (fn kcombs [k xs]
    (if (or (< k 1) (> k (count xs)))
      #{}
      (if (= k 1)
        (set (map #(conj #{} %) xs))
        (set (concat (map #(conj % (first xs)) (kcombs (dec k) (rest xs))) (kcombs k (rest xs))))))))

(defcheck solution-a0935ee5
  #(letfn [(tails [[_ & t :as xs]]
             (when xs (cons xs (lazy-seq (tails t)))))
           (comb [m l]
             (if (zero? m)
               [[]]
               (for [[x & xs] (tails (seq l))
                     ys (comb (dec m) xs)]
                 (set (cons x ys)))))]

     (into #{} (comb %1 %2))))

(defcheck solution-a0a60a65
  #(let[elements (vec %2)
        numOfElements (count elements)]
     (loop[c 0 n (dec %1) r (into #{} (map (fn[x] (into #{} (list x))) %2))]
       (cond
         (>= n numOfElements) #{}
         (= c n) r
         :else (recur (inc c) n (into #{} (for [com r e elements :when (not (contains? com e))] (conj com e))))))))

(defcheck solution-a0d73e90
  (fn [k c]
    (letfn [(f [n s] (if (> n (count s))
                       #{}
                       (if (= 1 n)
                         (map #(set (vector %)) s)
                         (map #(into (set (vector (first s))) %) (f (dec n) (rest s))))))]
      (set (reduce into
             (loop [r #{} s c]
               (if (empty? s)
                 r
                 (recur (into r
                          (for [i (range 1 (count s))]
                            (f k (cons (first s) (drop i s)))))
                   (rest s)))))))))

(defcheck solution-a0e42d77
  (fn combi [n setan]
    (if (or (empty? setan)
            (> n (count setan)))
      #{}
      (let [cont1 (into #{} (map #(into #{} [%]) setan))
            join (fn [starget s]
                   (into #{} (map #(clojure.set/union starget %) s)))]
        (loop [i 1 res cont1]
          (if (= n i)
            (into #{} (filter #(= n (count %)) res))
            (recur (inc i)
              (into #{}
                (mapcat #(join % cont1) res)))))))))

(defcheck solution-a0f11a15
  (fn f [n s]
    (cond
      (= n 0) #{#{}}
      (empty? s) #{}
      :else (set (concat
                   (map #(conj % (first s))
                     (f (dec n) (rest s)))
                   (f n (rest s)))))))

(defcheck solution-a1fd656a
  (fn ! [n xs]
    (cond (> n (count xs)) #{}
          (zero? n) #{#{}}
          (empty? xs) #{#{}}
          :else (into #{}
                  (apply concat
                    (map (fn [x]
                           (map #(conj % x)
                             (! (dec n) (disj xs x))))
                      xs))))))

(defcheck solution-a226e5a0
  #(let [n (fn f [r l s]
             (for [e s :let [n (conj r e)]]
               (if (= l (count n)) n (f n l (disj s e)))))]
     (set (flatten (n #{} % %2)))))

(defcheck solution-a267be82
  (fn t [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (set (clojure.set/union
                   (map #(conj % (first s)) (t (dec k) (rest s)))
                   (t k (rest s)))))))

(defcheck solution-a2a9c9d2
  (fn combinations [n xs]
    (if (> n (count xs))
      #{}
      (loop
       [
        xs xs
        result #{}
        ]
        (let
         [
          x (first xs)
          xs' (rest xs)
          last? (>= n (count xs))

          yss
          (if (= n 1)
            [[]]
            (combinations (dec n) xs')
            )

          result+
          (map
            (fn [ys]
              (set (cons x ys))
              )
            yss
            )

          result' (set (concat result result+))
          ]
          (if last?
            result'
            (recur
              xs'
              result'
              )
            )
          )
        )
      )
    ))

(defcheck solution-a2aa4369
  (fn k-combos [k se]
    (let [s (set se)]
      (cond (= k 1) (into #{} (map hash-set s))
            (> k (count s)) #{}
            (= k (count s)) #{s}
            :else (into #{}
                    (mapcat (fn [e]
                              (map (fn [ss]
                                     (conj ss e))
                                (k-combos (dec k) (disj s e))))
                      s))))))

(defcheck solution-a420e33f
  (fn [k s] (let [
                  n   (count s)
                  vs  (vec s)
                  nxt (fn [pr] (->> pr
                                 (map vector (range))
                                 (drop-while (fn [[i v]] (= i v)))
                                 ((fn [[[i h] & t]] (if h (let [n (dec h)]
                                                            `[~@(range (- n i) n) ~n ~@(map second t)]))))))]
              (->> (if (>= n k) (range (- n k) n))
                (iterate nxt)
                (take-while identity)
                (map (partial map vs))
                (map set)
                set))))

(defcheck solution-a4236cba
  (fn [n s]
    (letfn [(power [s] (letfn [(f [e t] (reduce #(conj %1 (conj %2 e)) #{} t))]
                         (if (empty? s) #{s}
                                        (let [e (first s) t (disj s e)]
                                          (clojure.set/union (power t) (f e (power t)))))))]
      (into #{} (filter #(= n (count %)) (power s))))))

(defcheck solution-a4651f2f
  (fn [k S]
    (let [power-set (fn [s]
                      (reduce (fn [ps e]
                                (->>
                                  ps
                                  (map #(set (concat #{e} %)))
                                  (concat ps)
                                  set))
                        #{#{}}
                        s))]
      (into #{}
        (filter #(= (count %) k) (power-set S))))))

(defcheck solution-a5295af5
  (fn choose [n xs]
    (let [length (count xs)
          head (first xs)
          tail (set (rest xs))]
      (cond
        (< length 1) #{}
        (== n 1) (->> xs (map #(conj #{} %)) (set))
        (< n 1) :undefined
        (< length n) #{}
        (== length n) (conj #{} xs)
        :else (set (concat (map #(conj % head) (choose (- n 1) tail))
                     (choose n tail)))))))

(defcheck solution-a535da1
  (comp set (fn c [k s]
              (if (= k 0) #{#{}}
                          (if-let [[e & f] (seq s)]
                            (into (c k f) (for [u (c (dec k) f)] (conj u e)))
                            #{})))))

(defcheck solution-a5f237b8
  (fn f [k s]
    (if (= k 0)
      #{#{}}
      (if (seq s)
        (let [[h & t] (seq s)]
          (into
            (f k t)
            (map #(conj % h) (f (dec k) t))))
        #{}))))

(defcheck solution-a600afc
  (fn c-choose-k [k s]
    (set
      (cond
        (< (count s) k) #{}
        (= 1 k)         (map (comp set list) s)
        :else           (lazy-cat
                          (map #(conj % (first s)) (c-choose-k (dec k) (rest s)))
                          (c-choose-k k (rest s)))))))

(defcheck solution-a610e377
  (fn [n s] (set
              (filter #(= (count %) n)
                (loop [sts #{ #{} } coll s]
                  (if (empty? coll) sts
                                    (recur (clojure.set/union sts
                                             (set (map (fn [c] (conj c (first coll)))  sts)))  (rest coll))))))))

(defcheck solution-a62ec716
  (fn k-comb [k aset]
    (if (pos? k)
      (set
        (for [elem aset
              comb (k-comb (dec k) (disj aset elem))]
          (conj comb elem)))
      #{#{}})))

(defcheck solution-a6456359
  (fn [n s]
    (letfn [(kcomb [n s r]
              (let [v (vec s)]
                (if (= n 0) r
                            (loop [x v res ()]
                              (if (empty? x) res
                                             (recur (rest x) (into res (kcomb (dec n) (rest x) (cons (first x) r)))))))))]
      (set (map #(set %) (partition n (kcomb n s #{})))))))

(defcheck solution-a652882c
  (fn [k s]
    (set (filter #(= (count %) k)
           (reduce (fn [acc x]
                     (concat
                       acc
                       (map (fn [acc-x] (conj acc-x x)) acc)))
             #{#{}} s)))))

(defcheck solution-a67c5d14
  (fn k-comb [n s]
    (cond
      (= n 0) #{}
      (= n 1) (set (for [x s] #{x}))
      1  (reduce clojure.set/union (for [x s] (set (map conj (k-comb (dec n) (disj s x)) (repeat x))))))))

(defcheck solution-a6e37adf
  (fn [n s]
    (loop [m (- (count s) n) x (set [s])]
      (cond (< m 0) #{}
            (= m 0) x
            :else (recur (- m 1) (set (mapcat set (for [y x] (map #(disj y %) y)))))))))

(defcheck solution-a710da09
  (letfn [(update [m k f]
            (assoc m k (f (m k))))
          (roll [v i x]
            (reduce (fn [v [x' i]]
                      (assoc v i (+ x x')))
              v (map-indexed vector (range i (count v)))))
          (next-kc [kc n]
            (let [padded (conj kc n)]
              (loop [i (dec (count kc))]
                (when (>= i 0)
                  (if (< (padded i) (dec (padded (inc i))))
                    (roll (update kc i inc) i (inc (kc i)))
                    (recur (dec i)))))))
          (kseqs [kc n]
            (when kc
              (cons kc (lazy-seq (kseqs (next-kc kc n) n)))))]
    (fn [k S]
      (let [v (vec S)
            n (count v)]
        (if (> k n)
          #{}
          (set (for [kc (kseqs (vec (range k)) n)]
                 (->> kc
                   (map v)
                   (set)))))))))

(defcheck solution-a7523081
  (fn kcombinator [n s]
    (letfn [(comb [e]
              (let [d (clojure.set/difference s e)]
                (if (empty? d) nil
                               (loop [ret #{} it d]
                                 (if (empty? it) ret
                                                 (recur (conj ret (conj e (first it))) (rest it)))))))]
      (loop [n n c #{#{}}]
        (cond (zero? n) c
              (> n (count s)) #{}
              :else (do
                      (recur (dec n) (set (reduce concat (map comb c))))))))))

(defcheck solution-a79b32f2
  (fn combinations [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (set (clojure.set/union
                   (map #(conj % (first s)) (combinations (dec k) (rest s)))
                   (combinations k (rest s)))))))

(defcheck solution-a8169038
  (fn [n s]
    (letfn [(p-set [s]
              (if (seq s)
                (let [r (p-set (next s))]
                  (into r (map #(conj % (first s)) r)))
                #{#{}}))]
      (set (filter #(= n (count %)) (p-set s))))))

(defcheck solution-a8a69087
  (fn [k s]
    (loop [acc #{#{}}
           cnt 1]
      (if (> cnt k)
        (->> acc
          (filter #(= k (count %))) set)
        (recur (apply clojure.set/union
                 (map (fn [a]
                        (map (fn [v] (conj a v)) s))
                   acc))
          (inc cnt))))))

(defcheck solution-a8ab43ce
  (fn __ [n c]
    (let [cnt (count c)]
      (cond
        (> n cnt) #{}
        (= n cnt) #{c}
        (= n 1) (set (map hash-set c))
        :else
        (set
          (reduce concat (for [i (drop-last c)]
                           (set (map #(conj % i) (__ (dec n) (remove (hash-set i) c)))))))))))

(defcheck solution-a8b33ef2
  (fn [n elems]
    (if (> n (count elems)) #{}
                            (loop [r (for [e elems] #{e}) dn (dec n)]
                              (if (zero? dn) (set (filter #(= n (count %)) r))
                                             (recur (for [e elems er r](conj er e)) (dec dn))
                                             )
                              )
                            )
    ))

(defcheck solution-a8da4476
  (fn comb [n s]
    (into #{}
      (if (zero? n)
        '(())
        (if (empty? s)
          nil
          (clojure.set/union (map #(into #{} (conj % (first s))) (comb (dec n) (rest s)))
            (comb n (rest s))
            )
          )
        )
      )
    ))

(defcheck solution-aa00c5a
  (fn [i c] (set (filter #(= (count %) i)
                   (reduce #(set (for [s %2 t %1] (conj t s)))
                     #{#{}}
                     (repeat i c))))))

(defcheck solution-aa46cf88
  (fn [n s]
    (letfn [(p [s] (reduce (fn [p e] (into p (map #(conj % e) p))) #{#{}} s))]
      (set (filter #(= n (count %)) (p s))))))

(defcheck solution-aa9afb4c
  (fn f [n s]
    (set (filter
           #(= n (count %))
           (if (or (zero? n) (empty? s))
             [[]]
             (let [[ft & rt] (seq s)]
               (->>
                 [(map #(conj % ft) (f (dec n) rt))
                  (f n rt)]
                 (apply concat)
                 (map set))))))))

(defcheck solution-aab2505e
  (fn comb [top l]
    (set
      (if (= 1 top)
        (map (comp set list) l)
        (for [head l
              tail (comb (dec top) (disj (set l) head))]
          (set (conj tail head)))))))

(defcheck solution-aad4253b
  (fn k-co [k s]
    (set
      (cond
        (or (zero? k) (> k (count s))) '()
        (= 1 k) (map hash-set s)
        :else
        (reduce
          (fn [ret i]
            (concat ret
              (map
                #(conj % i)
                (k-co (dec k) (disj s i)))))
          '()
          s)))))

(defcheck solution-ab3f5d2f
  (fn f [n s]
    (set
      (if (= n 0)
        #{#{}}
        (mapcat #(for [e (f (- n 1) %2)]
                   (conj e %1))
          s
          (rest (iterate rest s)))))))

(defcheck solution-acd4b56b
  (fn gen-subsets [n s]
    (set
      (flatten
        (for  [element s]
          (if (= 1 n) #{element}
                      (map #(conj % element) (gen-subsets (dec n) (disj s element)))))))
    ))

(defcheck solution-ad12df9b
  #(set (map set ((fn combinations [n coll]
                    (if (= 1 n)
                      (map list coll)
                      (lazy-seq
                        (when-let [[head & tail] (seq coll)]
                          (concat (for [x (combinations (dec n) tail)]
                                    (cons head x))
                            (combinations n tail))))))
                  % %2))))

(defcheck solution-ad249752
  (fn k-com [k s]
    (cond
      (or (= 0 k) (> k (count s))) #{}
      (= 1 k) (reduce #(conj % #{%2}) #{} s)
      :else (into (k-com k (rest s)) (map #(conj % (first s)) (k-com (dec k) (rest s))))
      )))

(defcheck solution-ad48b939
  (fn k-combinations
    ([k coll] (k-combinations k coll #{}))
    ([k coll acc]
     (cond
       (and (zero? k) (empty? acc)) #{}
       (and (pos? k) (empty? coll)) #{}
       (zero? k) #{acc}
       :else (reduce into #{} [(k-combinations
                                 (dec k) (next coll) (conj acc (first coll)))
                               (k-combinations k (next coll) acc)])))))

(defcheck solution-ad76f3e8
  (fn k-combine [n xset]
    (letfn [(inter [xset-1 xset-2]
              (set (for [s1 xset-1 s2 xset-2]
                     (clojure.set/union s1 s2))))
            (init-set [xs]
              (set (for [s xs] (set (list s)))))]
      (let [xs (init-set xset)]
        (cond
          (= n 1) xs
          (> n (count xs)) #{}
          :else (loop [acc xs cnt (dec n)]
                  (if (= cnt 0)
                    (set (filter #(= n (count %)) acc))
                    (recur (inter acc xs) (dec cnt)))))))))

(defcheck solution-adb8f789
  (fn f [n c]
    (if (= n 0)
      #{#{}}
      (if (<= n (count c))
        (into (f n (rest c))
          (for [t (f (dec n) (rest c))]
            (conj t (first c))))
        #{}))))

(defcheck solution-ae145c63
  (fn x[n l] (if (= n 1) (into #{} (map #(set [%]) l)) (into #{} (for [li l lii (x (- n 1) (disj l li))] (conj lii li))))))

(defcheck solution-ae2af223
  (fn k-combinations [k S]
    (let [powerset (fn powerset [full-set]
                     (if (empty? full-set)
                       #{#{}}
                       (let [rest-pset (powerset (rest full-set))]
                         (into rest-pset (map #(conj % (first full-set)) rest-pset)))))]
      (apply hash-set (filter #(= k (count %)) (powerset S))))))

(defcheck solution-ae54013f
  (fn k-com [k coll]
    (if (= 0 k)
      [#{}]
      (if-let [s (seq coll)]
        (let [tails (take-while seq (iterate rest s))
              tc (fn [t]
                   (let [k-1-c (k-com (dec k) (rest t))]
                     (map #(conj % (first t)) k-1-c)))
              ]
          (apply hash-set (apply concat (map tc tails))))))))

(defcheck solution-ae5aa800
  (fn C [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (set (clojure.set/union
                   (map #(conj % (first s)) (C (dec k) (rest s)))
                   (C k (rest s)))))))

(defcheck solution-ae5dd1ce
  (fn kcomb [n se]
    (let [ps (fn ps [s]
               (if (empty? s) #{#{}}
                              (let [i (first s)
                                    r (disj s i)
                                    sps (ps r)
                                    un clojure.set/union]
                                (un sps (set (map #(un #{i} %) sps))))))]
      (into #{} (filter #(= n (count %)) (ps se))))))

(defcheck solution-aec55cdf
  (fn kcomb
    ([n coll] (set (flatten (kcomb n (seq coll) #{}))))
    ([n coll so-far]
     (if (zero? n)
       so-far
       (map #(kcomb (dec n) (drop (inc %) coll) (conj so-far (nth coll %)))
         (range (count coll)))))))

(defcheck solution-af102438
  (fn kcomb [n s]
    (if (= n 0) #{#{}}
                (if (< (count s) n) #{}
                                    (if (= (count s) n) #{(set s)}
                                                        (let [s1 (set (map #(conj % (first s)) (kcomb (dec n) (rest s))))
                                                              s2 (kcomb n (rest s))]
                                                          (set (concat s1 s2))))))))

(defcheck solution-af2fca75
  (fn [i the-set]
    (loop [i i
           coll #{#{}}]
      (if (= i 0)
        coll
        (recur
          (dec i)
          (->> (map (fn [coll] (->> (for [item the-set
                                          :when ((complement contains?) coll item)]
                                      (conj coll item))
                                 (into #{}))) coll)
            (apply clojure.set/union)))))))

(defcheck solution-af6236d6
  (fn p [k s]
    (apply hash-set (cond
                      (= k (count s))
                      #{(apply hash-set s)}
                      (= k 0)
                      #{}
                      (> k (count s))
                      #{}
                      (= k 1)
                      (apply hash-set (map (fn [x] #{x}) s))
                      :else
                      (let [f (first s)
                            l (rest s)]
                        (clojure.set/union
                          (map (fn [sett] (conj sett f)) (p (dec k) l))
                          (apply hash-set (p k l))))))))

(defcheck solution-afcea72a
  (fn [n s]
    (letfn [(x-combs [n s]
              (if (= n 1)
                (map #(conj #{} %) s)
                (let [h (first s)
                      t (disj s h)]
                  (concat (map (fn [x] (into #{h} x)) (x-combs (dec n) t))
                    (when (> (count t) 1)
                      (x-combs n t))))))]
      (set (x-combs n s))
      )
    ))

(defcheck solution-afd3d34e
  (fn [n0 s0]
    (letfn [(subs [n s]
              (cond
                (zero? n) #{#{}}
                (empty? s) #{}
                :else (let [e (first (seq s))
                            with-e (map #(conj % e) (subs (dec n) (disj s e )))
                            without-e (subs n (disj s e))]
                        (clojure.set/union with-e without-e))))]
      (set (subs n0 s0)))))

(defcheck solution-afe75eaa
  (fn combinations [k sets]
    (cond
      (zero? k)
      #{#{}}

      (< (count sets) k)
      #{}

      :else
      (->> (combinations (dec k) sets)
        (mapcat #(map (partial conj %) sets))
        (filter (comp (partial = k) count))
        set))))

(defcheck solution-b00df8b9
  (fn k-combs
    [n s1]
    (letfn [(power-set [s]
              (reduce
                (fn [r i]
                  (conj r
                    (set (keep-indexed #(when (bit-test i %1) %2) s))))
                #{#{}}
                (range  1 (bit-shift-left 1 (count s)))))]
      (set (filter #(= n (count %)) (power-set s1))))))

(defcheck solution-b0222f25
  (fn combinations [k es]
    (letfn [(cartesian-seq
              ([s1 s2]
               (for [a (set s1), b (set s2)]
                 (list a b)))
              ([s1 s2 & ss]
               (for [a (set s1), b (apply cartesian-seq s2 ss)]
                 (cons a b))))]
      (cond (or (= 0 k) (> k (count es))) #{}
            (= 1 k) (set (map hash-set es))
            :else (->> (apply cartesian-seq (repeat k es))
                    (map set)
                    (distinct)
                    (filter #(= k (count %)))
                    (set))))))

(defcheck solution-b0275373
  #(letfn [(k-combinate [k coll]
             (let [size (count coll)]
               (cond
                 (= k size) (list coll)
                 (= k 0) (list '())
                 (> k size) '()
                 :else (let [c0 (first coll), coll' (rest coll)]
                         (concat
                           (k-combinate k coll')
                           (map (fn [c] (conj c c0)) (k-combinate (dec k) coll')))))))]
     (set (map (fn [c] (set c)) (k-combinate %1 (seq %2))))))

(defcheck solution-b0a648d6
  (fn k-combinations [k s]
    (if (< (count s) k)
      #{}
      (letfn [(helper [ini-set ans]
                (set (for [x ans y ini-set] (conj x y))))]
        (into #{} (filter #(>= (count %) k) (nth (iterate (partial helper s) #{#{}}) k)))))))

(defcheck solution-b1248269
  (fn [k s]
    (->>
      (reduce
        (fn [acc x]
          (set
            (for [item s s2 acc] (conj s2 item))))
        #{#{}}
        (range k))
      (filter (fn [st] (= (count st) k)))
      set)))

(defcheck solution-b13051b8
  (fn k-combinations [k a-set]
    (let [size (count a-set)]
      (if (> k size)
        #{}
        (into #{} (filter #(= k (count %))(reduce (fn [k-set x]
                                                    (into k-set (map #(if (> k (count %)) (conj % x) %) k-set))
                                                    ) #{#{}} a-set)))
        )
      )
    ))

(defcheck solution-b1957347
  (fn [n items]
    (let [powerset (reduce (fn [sets el]
                             (into sets (map #(conj % el) sets)))
                     #{#{}}
                     items)]
      (into #{} (filter #(= n (count %)) powerset)))))

(defcheck solution-b2106b35
  (fn k [n s]
    (set (if (zero? n)
           [#{}]
           (for [x s
                 comb (k (dec n) (disj s x))]
             (conj comb x))))))

(defcheck solution-b349f567
  (fn [n v]
    (into #{}
      (filter #(= n (count %))
        (reduce (fn [r v] (mapcat #(map (partial conj %) v) r))
          (map hash-set v)
          (repeat (dec n) v))))))

(defcheck solution-b3a1b21d
  (fn choose [k s]
    (cond (< (count s) k) #{}
          (= (count s) k) #{s}
          :else (set (apply concat (map #(choose k (disj s %)) s))))))

(defcheck solution-b3ab973b
  (fn [k s]
    (loop [c #{#{}} i 0]
      (if (= i k)
        (set (keep #(if (= (count %) k) %) c))
        (recur
          (for [p c q s] (conj p q))
          (inc i))))))

(defcheck solution-b3b2948f
  (fn kc [k xs]
    (if (> k (count xs))
      #{}
      (if (zero? k)
        #{#{}}
        (set
          (concat
            (map #(set (concat #{(first xs)} %)) (set (kc (dec k) (set (rest xs)))))
            (set (kc k (set (rest xs))))
            )
          )
        )
      )
    ))

(defcheck solution-b4889bba
  (fn [n s]
    (set
      (filter
        #(= (count %) n)
        (reduce (fn [r e] (into r (map #(conj % e) r))) #{#{}} s)))))

(defcheck solution-b4a66a42
  (fn f [k s]
    (set
      (when-let [[x] (seq s)]
        (if (= 1 k)
          (map hash-set s)
          (concat (map #(conj % x) (f (dec k) (disj s x)))
            (f k (disj s x))))))))

(defcheck solution-b5821ec0
  (fn [n items]
    (if (> n (count items))
      #{}
      (->>
        (reduce (fn [acc item]
                  (mapcat (juxt #(conj % item) identity) acc)) [#{}] items)
        (filter #(= n (count %)))
        set))))

(defcheck solution-b5b7db1
  (fn __ [n s]
    (if (or (= n 0) (empty? s) (> n (count s)))
      #{}
      (if (= 1 n)
        (set (map hash-set s))
        (when-let [xs (seq s)]
          (set (mapcat
                 (fn [i]
                   (let [ni (nth xs i)]
                     (let [sbs (__ (dec n) (drop (inc i) xs))]
                       (map (fn [nxs] (conj nxs ni)) sbs))))
                 (range (- (count xs) (dec n))))))))))

(defcheck solution-b5bf55e3
  (fn f [k s]
    (if (< 0 k)
      (set
        (for [x s
              s (f (dec k) (disj s x))]
          (conj s x)))
      #{#{}})))

(defcheck solution-b60044b6
  (fn [k s]
    (letfn
     [(h [k] (if (zero? k) #{[#{} s]}
                           (set (mapcat (fn [[y n]] (map (fn [x] [(conj y x) (disj n x)]) n))
                                  (h (dec k))))))]
      (set (map first (h k))))))

(defcheck solution-b66f9494
  (fn k-combine
    [k s]
    (cond
      (= 1 k) (set(map #(hash-set %) s))
      (> k (count s)) #{}
      :else (set (concat
                   (map #(conj % (first s))  (k-combine (dec k) (rest s)))
                   (k-combine k (rest s)))))))

(defcheck solution-b6b3a5ef
  (fn k-combinations
    ([k xs]
     (let [n (count xs)]
       (if (or (> k n) (zero? k)) #{}    ;; combinatorics condition k should be less than n or not zero!
                                  (set
                                    (map set
                                      (if (= 1 k) (partition 1 xs)     ;; -> code-as-data->yaii! 1-partitioning
                                                  (k-combinations k (seq xs) #{} (count xs) (dec (count xs))))))))) ;; recur compute the k-combinations
    ([k [head & tail :as current] combinations outer-timer cycle-timer]
     (letfn [(cycle-by [x xs]
               (loop [chunks #{}
                      [head & tail] xs]
                 (let [chunked (take x (cons head tail))]
                   (if (< (count chunked) x) chunks
                                             (recur (conj chunks chunked) tail)))))]
       (if (zero? outer-timer) combinations
                               (if (zero? cycle-timer)
                                 (recur k (conj (vec tail) head) ;; -> (tail head)
                                   (concat combinations (cycle-by k current))
                                   (dec outer-timer)        ;; -> ((first tail) (rest tail) head)
                                   (dec (count current)))   ;; reset cycle-timer
                                 (recur k (cons head (conj (vec (rest tail)) (first tail))) ;; (head (rest tail) (first tail))
                                   (concat combinations (cycle-by k current))
                                   outer-timer              ;; not changed as the inner cycling is in progress
                                   (dec cycle-timer))))))))

(defcheck solution-b702826f
  (fn [k s]
    (letfn [(step [s] (for [e s] (disj s e)))
            (comb-it [d ss]
              (if (= 0 d) ss
                          (recur (dec d) (reduce into #{} (map step ss)))))]
      (let [n (count s)]
        (if (or (< n k) (zero? k)) #{} (comb-it (- n k) (hash-set s)))))))

(defcheck solution-b87a3c9d
  (fn[n s]
    (set (filter #(= n (count %)) (reduce (fn [s v] (concat s (map #(conj % v) s))) [#{}] s)))))

(defcheck solution-b8a02792
  (fn kcom [n s]
    (letfn
     [(ksets [n root [head & tail]]
        (cond
          (zero? n) [root]
          (nil? head) nil
          :else (concat
                  (ksets (dec n) (conj root head) tail)
                  (ksets n root tail))))]
      (set (map set (map (partial remove nil?) (ksets n [] (apply list s))))))))

(defcheck solution-b93aa2b2
  (fn comb [n datos]
    (cond (= 1 n) (set (map #( hash-set %) datos))
          :else (reduce (fn [acc x]
                          (into acc (map #(conj % x)
                                      (comb (dec n) (disj datos x) ))) )
                  #{} datos))))

(defcheck solution-ba395b4f
  (fn [n coll]
    (letfn [(kc [k s]
              (set
                (if (= 1 k)
                  (map #(set [%]) s)
                  (mapcat (fn [x]
                            (map #(conj % x)
                              (kc (dec k) (disj s x))))
                    s))))]
      (kc n coll))))

(defcheck solution-ba4258f4
  (fn [x xs]
    (let [bin (fn [n] (map (partial bit-and 1)
                        (take-while pos? (iterate #(bit-shift-right % 1) n))))
          binmap (fn [m] (set (mapcat #(take %1 [%2]) m xs)))
          i (range 1 (bit-shift-left 1 (count xs)))
          y (filter #(= x (reduce + %))  (map bin i))]
      (set (map binmap y)))))

(defcheck solution-bb2c8c82
  #(if (> % (count %2)) #{} (set (map (fn[a](set (take % (shuffle %2)))) (range 1000)))))

(defcheck solution-bb6dc917
  (fn k-comb [k S]
    (->>
      (cond
        (> k (count S)) []
        (= k (count S)) [S]
        (= 1 k) (map vector S)
        :else (reduce
                concat
                (map-indexed
                  (fn [i x]
                    (map #(cons x %)
                      (k-comb (dec k) (drop (inc i) S))))
                  S)))
      (map set)
      (into #{}))))

(defcheck solution-bbf5d86f
  (fn [how-many master-set]
    (letfn [(next-set [cur-nths max-size]
              (if (empty? cur-nths)
                nil
                (if (< (last cur-nths) (dec max-size))
                  (concat (drop-last cur-nths) (list (inc (last cur-nths))))
                  (let [new-set (next-set (drop-last cur-nths) (dec max-size))]
                    (if (nil? new-set)
                      nil
                      (concat new-set (list (inc (last new-set)))))))))]

      (loop [results #{}
             original-set (into [] master-set)
             the-set (range 0 how-many)]

        (if (or (nil? the-set) (> how-many (count original-set)))
          (do #_(println results)
              results)
          (recur
            (conj results (into #{} (map #(nth original-set %) the-set)))
            original-set
            (next-set the-set (count original-set))))))))

(defcheck solution-bc0a08f1
  (fn k-comb [k s]
    (let [powerSet (reduce (fn [ss e] (into ss (map #(conj % e) ss))) #{#{}} s)]
      (set (filter #(= k (count %)) powerSet)) ;; Only keep subsets of size k
      )
    ))

(defcheck solution-bc3122dd
  (fn [k coll]
    (letfn [(combinations [xs]
              (if (empty? xs)
                #{#{}}
                (let [fst (first xs)
                      rst (rest xs)
                      comb-rst (combinations rst)]
                  (clojure.set/union #{#{fst}} comb-rst (set (map #(conj % fst) comb-rst))))))]

      (set (filter #(= k (count %)) (combinations coll))))))

(defcheck solution-bc5aab0c
  (fn f [n s]
    (if (pos? n)
      (if-let [[x & xs] (and (<= n (count s)) (seq s))]
        (into (f n xs) (map #(conj % x) (f (dec n) xs)))
        #{})
      #{#{}})))

(defcheck solution-bc60acbd
  (fn [n xs]
    (letfn [(power-set [xs]
              (let [f (first xs)
                    r (disj xs f)]
                (if (seq xs)
                  (set (concat (map #(conj % f) (power-set r))
                         (power-set r)))
                  #{#{}})))]
      (set (filter #(= n (count %)) (power-set xs))))))

(defcheck solution-bccbde14
  (fn k-c [n s]
    (if (> n (count s)) #{}
                        (if (= 1 n) (into #{} (map (comp set vector) s))
                                    (let [prior (k-c (dec n) s)]
                                      (into #{} (for [p prior
                                                      q (clojure.set/difference s p)]
                                                  (clojure.set/union p #{q})
                                                  )))))))

(defcheck solution-bd54fa72
  (fn k [n l]
    (letfn [(p [ls]
              (if (empty? ls) #{#{}}
                              (clojure.set/union (p (next ls))
                                (into #{} (map #(conj % (first ls)) (p (next ls)))))))]
      (set (filter #(= n (count %)) (p l))))))

(defcheck solution-bda76d15
  (fn kcomb [n, xs]
    (if (= n 1)
      (set (map #(conj #{} %) xs))
      (reduce clojure.set/union
        (map (fn [x]
               (set (map
                      #(conj % x)
                      (kcomb (dec n) (disj xs x)))))
          xs)))))

(defcheck solution-bdb2eee2
  (fn k-comb [k s]
    (letfn [(power-set [s]
              (loop [s s
                     ps #{#{}}]
                (if (empty? s) ps
                               (recur (rest s) (into ps (map #(conj % (first s)) ps))))))]
      (set (filter #(= k (count %)) (power-set s))))))

(defcheck solution-be0b06e1
  (fn
    [n s]
    (if (or (> n (count s))
            (< n 1))
      #{}
      (letfn [(next-set-n-elements
                [x]
                (let [smallest (bit-and x (- x))
                      ripple (+ x smallest)
                      smallest' (bit-and ripple (- ripple))
                      ones (dec (bit-shift-right (quot smallest' smallest) 1))]
                  (bit-or ripple ones)))
              (select-via-bits
                [x]
                (set (second (reduce (fn [[bs s] e]
                                       (let [bs' (bit-shift-right bs 1)]
                                         (if (odd? bs)
                                           [bs' (conj s e)]
                                           [bs' s]))) [x []] s))))]
        (let [x (dec (bit-shift-left 1 n))
              ns (take-while #(and (> % 0)
                                   (< % (bit-shift-left 1 (count s))))
                   (iterate next-set-n-elements x))]
          (set (map select-via-bits ns)))))))

(defcheck solution-be4adbaa
  (fn kcomb [n s]
    (set (if (= n 1) (map hash-set s)
                     (filter #(= n (count %)) (reduce (fn [a b] (concat a (map #(conj b %) s))) #{} (kcomb (dec n) s)))))))

(defcheck solution-be4f78c4
  (fn kcom [k coll]
    (let [s2v (fn [sc] (for [i sc] i))
          cc (s2v coll)
          v2mk (fn [vc] (into {} (for [i (range (count vc))] [(nth vc i) i])))
          v2mv (fn [vc] (into {} (for [i (range (count vc))] [i (nth vc i)])))
          tranmv (fn [vc vmap] (for [i vc] (vmap i)))
          mk (v2mk cc)
          mv (v2mv cc)
          combb (fn combnt [n m seqs]
                  (if (= n m)
                    (list seqs)
                    (if (= 1 m)
                      (map list seqs)
                      (map #(flatten %)
                        (reduce concat '()
                          (for [k (range (- n (dec m)))]
                            (map #(list (first (drop k seqs)) %)
                              (combnt (dec n) (dec m) (drop (inc k) seqs)))))))))
          combdatas (combb (count cc) k (tranmv cc mk))
          combdatav (map #(tranmv % mv) combdatas)]
      (set (map set combdatav)))))

(defcheck solution-be82a39b
  (fn [n s]
    (letfn [(f [x] (set (map #(disj x %) x)))]
      (loop [t #{s} m (count s)]
        (cond
          (> n m) #{}
          (= n m) t
          1 (recur (apply clojure.set/union (map f t)) (dec m)))))))

(defcheck solution-bf012cdc
  (fn [n s]
    (cond
      (= n (count s)) #{s}
      (> n (count s)) #{}
      :else (set
              (filter
                #(= (count %) n)
                (reduce
                  (fn [c itm]
                    (into c
                      (map #(conj % itm) c)))
                  #{#{}} s))))))

(defcheck solution-bf08605f
  (fn [n is]
    (set
      (loop [i 1
             res (map
                   (comp set list)
                   is)]
        (if
         (= n i)
          res
          (recur
            (inc i)
            (set
              (for
               [i is
                j res
                :when
                (not
                  (contains? j i))]
                (conj j i)))))))))

(defcheck solution-bf43717d
  (fn k-combs [i-num i-set]
    (if (or (nil? i-set) (empty? i-set))
      #{}
      (if (= i-num (count i-set))
        #{(into #{} i-set)}
        (if (> i-num (count i-set))
          #{}
          (if (= i-num 0)
            [#{}]
            (into #{} (concat  (into [] (map #(conj % (first i-set)) (k-combs (dec i-num) (rest i-set))))
                        (#(if (set? (first %)) (into [] %) [%]) (k-combs i-num  (rest i-set)))))))))))

(defcheck solution-bf4e5d2e
  (fn [k s]
    (let [bi (fn bi [bin]
               (if (or (nil? bin) (= 0 (count bin)))
                 nil
                 (let [p (peek bin)
                       t (pop bin)]
                   (if p
                     (conj t false)
                     (conj (bi t) true)))))]
      (loop [x (seq s)
             acc #{}
             mask (vec (repeat (count x) true))]
        (if (zero? (count (filter identity mask)))
          acc
          (if (= k (count (filter identity mask)))
            (recur x (conj acc
                       (into #{} (map #(first %) (filter #(second %) (map #(vector % %2) x mask))))) (bi mask))
            (recur x acc (bi mask))))))))

(defcheck solution-bf5f2108
  (fn kcomb [k s]
    (cond
      (zero? k)       #{#{}}
      (> k (count s)) #{}
      (= k (count s)) #{s}
      :else           (into #{} (mapcat (fn [x] (map #(conj % x) (kcomb (dec k) (disj s x)))) s)))))

(defcheck solution-bf656c0b
  (fn [n s] (set (filter #(= n (count %)) ((fn [s]
                                             (reduce (fn [ps x]
                                                       (reduce (fn [ps s]
                                                                 (conj ps (conj s x))) ps ps)) #{#{}} s))
                                           s)))))

(defcheck solution-c0c2430b
  (fn k-combinations [k s]
    (let [pool  (vec s)
          n     (count pool)
          zip   (partial map vector)]
      (cond
        (or (zero? k) (zero? n) (> k n)) #{}
        (= 1 k) (set (map hash-set pool))
        (= k n) #{s}
        true (let [rs (reverse (range k))
                   fi (fn [[i j :as x]]
                        (if (not= j (- (+ i n) k))
                          x))
                   yield (comp set (partial map (partial nth pool)))]
               (loop [js (range k)
                      ac #{(yield js)}]
                 (if-let [[i j] (some fi (zip rs (reverse js)))]
                   (let [js (take k (concat (take i js) (iterate inc (inc j))))]
                     (recur (vec js) (conj ac (yield js))))
                   ac)))))))

(defcheck solution-c13556b5
  (fn kcomb
    [n s]
    (cond (zero? n)       #{}
          (= 1 n)         (set (map (partial conj #{}) s))
          (> n (count s)) #{}
          :else
          (let [s (set s)]
            (set (mapcat (fn [i]
                           (map #(conj % i)
                             (kcomb (dec n) (disj s i))))
                   s))))))

(defcheck solution-c1845e21
  (fn k-combinations [k s]
    (cond (= k 0)         #{#{}}
          (> k (count s)) #{}
          (= k (count s)) #{(set s)}
          :else (let [[x & others] (seq s)]
                  (into (k-combinations k others)
                    (map #(conj % x)
                      (k-combinations (dec k) others)))))))

(defcheck solution-c1a8dbe7
  (fn k-combs
    [n s]
    (let [powerset (reduce (fn [p e] (into p (map #(conj % e) p))) #{#{}} s)]
      (into #{} (filter #(= n (count %)) powerset)))))

(defcheck solution-c1ed9761
  (fn kcombos [n s]
    (into #{}(filter #(= n (count %)) ((fn powerset
                                         [coll]
                                         (let [x (first coll) xs (rest coll)]
                                           (if (empty? coll)
                                             #{#{}}
                                             (let [pset (powerset xs)]
                                               (clojure.set/union pset (map #(into #{x} %) pset)))))) s)))))

(defcheck solution-c211c485
  (fn gkc [n s]
    (if (> n (count s)) #{}
                        (set (filter #(= n (count %))
                               (reduce (fn [k i] (into k (cons #{i} (map #(merge % i) k)))) #{#{}} s))))))

(defcheck solution-c230018c
  (fn combinations [k seq]
    (cond (zero? k) #{#{}}
          (empty? seq) #{}
          :else (set (clojure.set/union
                       (map #(conj % (first seq)) (combinations (dec k) (rest seq)))
                       (combinations k (rest seq)))))))

(defcheck solution-c2698b61
  (fn combinations [n xs]
    (letfn [(inner [base n xs]
              (cond
                (or (= n 0) (> n (count xs))) #{}
                (= n 1) (into #{} (map #(conj base %) xs))
                :else (clojure.set/union
                        (inner (conj base (first xs)) (dec n) (rest xs))
                        (inner base n (rest xs)))))]
      (inner #{} n xs))))

(defcheck solution-c2bb09d3
  (fn power-set-k-combination
    [k a-set]
    (loop [[x & xs] (vec a-set) res #{#{}}]
      (if x
        (recur xs (clojure.set/union res (map #(conj % x) res)))
        (into #{} (filter #(= k (count %)) res))))))

(defcheck solution-c2d0e6a9
  (fn[n opts]
    (nth (iterate
           #(into #{} (for [s % o opts :when (not (s o))] (conj s o)))
           #{#{}}) n)))

(defcheck solution-c3140106
  (fn [x y]
    (set (filter #(= x (count %)) (reduce (fn [s a]
                                            (clojure.set/union
                                              s
                                              (map #(clojure.set/union #{a} %) s)))
                                    #{#{}} y)))))

(defcheck solution-c357a28d
  (fn [n seq]
    (letfn [(combs [n seq]
              (cond (= n 0) (list nil)
                    (< (count seq) n) nil
                    :else
                    (concat
                      (combs n (rest seq))
                      (map cons
                        (repeat (first seq))
                        (combs (dec n) (rest seq))))))]
      (set (map set (combs n (vec seq)))))))

(defcheck solution-c361a807
  (fn generate-combos [k xs]
    (loop [remainder xs, combos []]
      (if (seq remainder)
        (if (= k 1)
          (recur (rest remainder) (conj combos [(first remainder)]))
          (recur (rest remainder) (concat combos (map (partial cons (first remainder)) (generate-combos (dec k) (rest remainder))))))
        (set (map set combos))))))

(defcheck solution-c39088ce
  (fn [k xs]
    (letfn [(pick-next [pairs]
              (set (for [[k vs] pairs
                         v vs]
                     [(conj k v) (disj vs v)])))]
      (set
        (map first
          (loop [pairs #{[#{} xs]}, k k]
            (if (zero? k)
              pairs
              (recur (pick-next pairs) (dec k)))))))))

(defcheck solution-c3b3f069
  (fn [k s]
    (set
      (filter
        #(= k (count %))
        ((fn f [k s]
           (when (and (> k 0) (<= k (count s)))
             (loop [c s
                    r #{}]
               (if (seq c)
                 (let [v (map #(conj % (first c)) (f (dec k) (rest c)))]
                   (recur (rest c)
                     (into r (if (empty? v) [#{(first c)}] v))))
                 r)))) k s)))))

(defcheck solution-c4c2d886
  (fn [ec data]
    (let [v  data
          kc (fn [s]
               (into #{}
                 (mapcat
                   (fn [s] (map #(into #{} (remove #{%} s)) s))
                   s)))]
      (loop [i (- (count v) ec)
             s #{v}]
        (cond
          (neg? i) #{}
          (zero? i) s
          (pos? i) (recur (dec i)
                     (kc s)))))))

(defcheck solution-c5062ae1
  (fn k-combinations [k s]
    (if (or (zero? k) (empty? s))
      #{}
      (let [prefix   (first s)
            suffixes (k-combinations (dec k) (rest s))
            others   (k-combinations      k  (rest s))]
        (if (= k 1) (set (concat (list #{prefix})                others))
                    (set (concat (map #(conj % prefix) suffixes) others)))))))

(defcheck solution-c5732fef
  (fn [n c]
    (set (filter #(= (count %) n) (reduce (fn [r x] (into r (map #(conj % x) r)))
                                    #{#{}} c)))))

(defcheck solution-c59770fd
  (fn k-combinations
    ([n coll] (k-combinations n coll #{}))
    ([n coll current]
     (cond
       ;we can't add anything else
       (= (count current) n)  #{current}
       ;didn't make n elements
       (empty? coll) #{}
       :else
       ;we can either skip current or add new
       (set (concat
              (k-combinations n (rest coll) current) (k-combinations n (rest coll) (conj current (first coll)))
              ))
       )
     )
    ))

(defcheck solution-c6aeb222
  (fn k-combos [k s]
    (set
      (when (and (not (zero? k)) (seq s))
        (let [prefix (first s)
              suffixes (k-combos (dec k) (rest s))
              others (k-combos k (rest s))]
          (if (= k 1)
            (concat (list #{prefix}) others)
            (concat (map #(conj % prefix) suffixes) others)))))))

(defcheck solution-c6e12be5
  (fn kCombi
    ([n s] (if (> n (count s)) #{} (kCombi n s #{})))
    ([n s res]
     (if (= n 0)
       #{res}
       (reduce #(into %1 %2) (map #(kCombi (dec n) s (conj res %)) (filter #(not (contains? res %)) s))))
     )
    ))

(defcheck solution-c74d028c
  (fn [length input]
    (let [bitmask (take (reduce +
                          (take (- (count input) 1)
                            (iterate #(* 2 %) 4)))
                    ((fn get-bitmask [mask]
                       (let [mask-entries (for [x [0 1] y mask]
                                            (list x y))]
                         (lazy-seq (concat mask-entries (get-bitmask mask-entries))))) [0 1]))]
      (condp = (count input)
        0 #{#{}}
        1 #{#{} #{(first input)}}
        (set (filter #(= (count %)
                        length)  (map (fn [mask]
                                        (into #{} (filter (comp not nil?) (map (fn [m value] (if (= 1 m) value)) mask input))))
                                   (map flatten bitmask))))))))

(defcheck solution-c7de6610
  (fn [n s]
    (set
      (filter #(= n (count %))
        (reduce
          (fn [a x] (into a (map #(conj % x) a)))
          #{#{}}
          s)))))

(defcheck solution-c7e76ec2
  (fn k-combinations
    [n coll]
    (cond (zero? n) [[]]
          (empty? coll) #{}
          :else (let [[x & xs] (seq coll)]
                  (set
                    (concat
                      (map (comp set (partial cons x))
                        (k-combinations (dec n) xs))
                      (k-combinations n xs)))))))

(defcheck solution-c7f686b2
  (fn k-comb [n S]
    (cond (or (= n 0)) #{#{}}
          (empty? S) #{}
          true    (clojure.set/union
                    (k-comb n (rest S))
                    (into #{} (map conj (k-comb (dec n) (rest S)) (repeat (first S)))))) ))

(defcheck solution-c8234a36
  (fn f1 [n s]
    (letfn [(f1 [a b]
              (mapcat (fn [[cf & cr]]
                        (cons (conj a cf) (f1 (conj a cf) cr)))
                (map #(take-last % b) (range (count b) 0 -1))))]
      (set (filter #(= (count %) n) (f1 #{} s))))))

(defcheck solution-c86e4234
  (fn [n s]
    (if (< (count s) n)
      #{}
      (let [s (map #(set [%]) s)
            reducer (fn [r m]
                      (fn [a x]
                        (let [coll (filter (fn [coll] (= (inc m) (count coll)))
                                     (map (fn [y] (conj y (first x))) r))]
                          (if (empty? coll)
                            a
                            (apply conj a coll)))))]
        (loop [m 1
               r s
               ns (next s)]
          (if (= n m)
            (set r)
            (recur (inc m)
              (into () (reduce (reducer r m) #{} ns))
              (next ns))))))))

(defcheck solution-c881685c
  (fn k-comb [number iset]
    #_(println "Number: " number " Iset: " iset)
    (if (= number 1)
      (set (for [x iset] #{x}))
      (set (apply concat
             (for [x iset]
               (map #(conj % x)
                 (k-comb (dec number) (disj iset x)))))))))

(defcheck solution-c8e5b94f
  (fn [n all]
    (let [x (count all)]
      (if (> n x) #{}
                  (if (= n x) #{all}
                              (loop [built (hash-set (hash-set) all)]
                                (let [new (into built (for [x built s all] (disj x s))) filtered (apply hash-set (filter #(= (count %) n) new))]
                                  (if (empty? filtered) (recur new)
                                                        filtered))))))))

(defcheck solution-c8f6e26f
  (fn [n s]
    (letfn [(ss [s more]
              (when-first [x more]
                (let [t (map #(conj % x) s)]
                  (lazy-cat t (ss (concat t s) (rest more))))))]
      (->> (ss #{#{}} s)
        (filter #(= n (count %)))
        (into #{})))))

(defcheck solution-c939b6b2
  (fn [n coll]
    (letfn [(subsets [coll]
              (if (empty? coll)
                [[]]
                (let [f (first coll)
                      r (rest coll)]
                  (concat (subsets r)
                    (map #(conj % f) (subsets r))))))]
      (->> (subsets coll)
        (filter #(= (count %) n))
        (map set)
        set))))

(defcheck solution-c94544df
  (fn [size myset]
    (let
     [
      powerSet
      (fn [myset]
        (reduce
          (fn
            [prev item]
            (set (concat prev (for [x prev y item] (conj x y))))
            )
          #{#{}}
          (replicate (count myset) myset)
          )
        )
      ]
      (set (filter #(= size (count %)) (powerSet myset)))
      )
    ))

(defcheck solution-c9662f93
  (fn comb
    ([k s] (into #{} (comb k s #{})))
    ([k s c]
     (if (> k 0)
       (apply concat (for [x s] (comb (dec k) (disj s x) (conj c x))))
       (list c)))))

(defcheck solution-c99e5a1
  (fn k-comb [k s]
    (if (< (count s) k)
      #{}
      (letfn [(nexts [candidates n]
                (map (fn [[head tail]] [(last head) tail])
                  (map #(split-at % candidates)
                    (range 1 (+ 2 (- (count candidates) n))))))
              (continue-partial [n [part candidates]]
                (map (fn [[next others]] [(conj part next) others]) (nexts candidates n)))
              (churn [n partials]
                (if (= n 0)
                  partials
                  (recur (dec n) (mapcat (partial continue-partial n) partials))))]
        (set (map first (churn k [[#{} s]])))))))

(defcheck solution-c9de4f07
  (letfn [
          (count-bits [n] (loop [c 0 v n] (if (zero? v) c (recur (inc c) (bit-and v (- v 1))))))
          (which-bits [n]
            (loop [n' n bits [] i 0]
              (if (zero? n')
                bits
                (recur
                  (bit-shift-right n' 1)
                  (if (= 1 (bit-and n' 1)) (conj bits i) bits)
                  (inc i)))))
          (gen-k-combinations [k s]
            (if (> k (count s))
              #{}
              (let [s' (seq s)
                    combinations  (
                                    ->> (range (bit-shift-left 1 (count s')))    ; get all numbers from 0 to 2^(count s)
                                    (filter #(= k (count-bits %)))           ; filter in those numbers with k bits set
                                    (map which-bits)                         ; figure out which bits are high
                                    (map (fn [comb] (map #(nth s' %) comb))) ; pick those combinations of elements out of s
                                    (map set))]                              ; turn them into sets
                (set combinations))))]
    gen-k-combinations
    ))

(defcheck solution-ca1ab019
  (fn [k xs]
    (letfn [(kcom [ys visited k]
              (cond (> k (count ys)) #{}
                    (= k 0) visited
                    :else
                    (map #(kcom (disj ys %) (conj visited %) (dec k)) ys)))]
      (into #{} (flatten (kcom xs #{} k))))))

(defcheck solution-ca59df97
  (fn k-comb
    ([n init]
     (let [i (- (count init) n)]
       (if (neg? i)
         #{}
         (loop [sets [init], i i]
           (if (pos? i)
             (recur
               (for [elt init, set sets :when (contains? set elt)]
                 (disj set elt))
               (dec i))
             (into #{} sets))))))))

(defcheck solution-cab99567
  (fn [k col]
    (if (> k (count col)) #{}
                          (loop [i k
                                 res (mapv vector col)]
                            (if-not (zero? i)
                              (recur (dec i) (apply conj res (for [x res y (mapv vector col)] (into x y))))
                              (into #{} (filter #(= k (count %))(map #(into #{} %) res))))))))

(defcheck solution-cb3f721f
  (fn [k s]
    (loop [q '(#{}) v #{} r #{}]
      (let [[h & t] q
            w (conj v h)]
        (cond
          (empty? q) r
          (= k (count h)) (recur t w (conj r h))
          :e (recur (concat (remove v (map #(conj h %) s)) t) w r)
          )))))

(defcheck solution-cb576119
  (fn f [n st]
    (if (> n (count st))
      #{}
      (let [fak (fn [n] (reduce * (range 1 (inc n))))
            jum (/ (fak (count st))
                  (* (fak (- (count st) n))
                    (fak n)))]
        (loop [res #{}]
          (if (= (count res) jum)
            res
            (recur (let [elm (set (take n (shuffle (vec st))))]
                     (if (= n (count elm))
                       (conj (set res) elm)
                       (set res))))))))))

(defcheck solution-cb7c41a8
  (fn c [k s]
    (cond
      (= k 0) #{#{}}
      (empty? s) #{}
      true (set (concat (c k (rest s))
                  (map #(conj % (first s))
                    (c (dec k) (rest s))))))))

(defcheck solution-cbb32e8f
  (fn seq-comb[x xs]
    (set
      (if (= 1 x) (map (comp set list ) xs)
                  (loop [ys xs
                         res []]
                    (if-not (empty? ys)
                      (recur (drop 1 ys)
                        (concat res (map (partial cons (first ys))
                                      (seq-comb (dec x) (drop 1 ys)))))
                      (map set res)))))))

(defcheck solution-ccb7effb
  (let [power-set (fn
                    [x]
                    (if (empty? x) #{#{}}
                                   (loop [[h & t] (into [] x)
                                          accum #{#{}}]
                                     (let [new-accum (into accum (map #(conj % h) accum))]
                                       (if (nil? t)
                                         new-accum
                                         (recur t new-accum))))))]
    (fn k-comb[c s] (into #{} (filter #(= (count %) c) (power-set s))))))

(defcheck solution-cd059717
  (fn [t z] (letfn [(helper [s] (into #{} (vals (filter #(<= (first %) t) s))))
                    (indices [s] (into [] (keep-indexed (fn [x -] x) (into [] s))))]
              (if (> t (count z)) #{}
                                  (let [a (into [] z) b (indices a) n (count z) r (assoc (assoc (reduce #(assoc % (inc %2) %2) {} (filter #(< % t) b )) (inc t) n) (+ t 2) 0)]
                                    (loop [acc #{} comb r k 1]
                                      (if (> k t) acc
                                                  (let [s (loop [c comb j 1]
                                                            (if (= (c (inc j)) (inc (c j))) (recur (assoc c j (dec j)) (inc j))
                                                                                            [(conj acc (into #{} (map #(a %) (helper c)))) c j]))]
                                                    (let [c (second s) j (last s)  c1 (assoc c j (inc (c j))) ]
                                                      (recur (conj acc (into #{} (map #(a %) (helper c1)))) c1  j))))))))))

(defcheck solution-cd279ec4
  (fn kcombs [k items]
    (letfn [(iter [n combs]
              (if (zero? n) combs
                            (recur (dec n) (for [x combs y items] (conj x y)))))]
      (into #{} (filter #(= (count %) k) (iter k [#{}])))
      )
    ))

(defcheck solution-cd605a45
  (fn comb [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (into (comb k (rest s))
              (map #(conj % (first s)) (comb (dec k) (rest s)))))))

(defcheck solution-cd87c5e2
  (fn f [c s]
    (if (= 0 c)
      [#{}]
      (set
        (mapcat
          (fn [x]
            (map #(conj % x)
              (f (dec c) (disj s x))))
          s)))))

(defcheck solution-cdd80731
  #(loop [n % a [#{}]]
     (if (> n 0)
       (recur (dec n) (for [x a y (apply disj %2 x)] (conj x y)))
       (set a))))

(defcheck solution-cecf0dea
  (fn k-combinations- [k coll]
    "103. Given a sequence S consisting of n elements generate all k-combinations
  of S, i. e. generate all possible sets consisting of k distinct elements taken
  from S."
    ;; Translated from Haskell at http://rosettacode.org/wiki/Combinations#Haskell
    (set
      (cond
        (= k 0) #{#{}}
        (empty? coll) #{}
        :else (concat
                (map #(conj % (first coll)) (k-combinations- (dec k) (rest coll)))
                (k-combinations- k (rest coll)))))))

(defcheck solution-cfa04107
  (fn [n ys]
    (letfn [(subsets [xs] (if (empty? xs) [[]]
                                          (let [ys (-> xs rest subsets)]
                                            (into ys (map #(conj % (first xs)) ys)))))]
      (set (map set (filter #(= n (count %)) (subsets ys)))))))

(defcheck solution-cfdc6f09
  (fn comb[n s]
    (set (cond (> n (count s)) []
               (= n 1) (map #(set [%]) s)
               :else (mapcat (fn[e] (map #(conj % e) (comb (dec n) (remove #(= e %) s)))) s)))))

(defcheck solution-cfe5d8d5
  (fn kc [s coll]
    (letfn [(pwr-set [s]
              (reduce (fn [a i]
                        (->> a
                          (map #(conj % i))
                          (into a)))
                #{#{}} s))]
      (set (filter #(= s (count %)) (pwr-set coll))))))

(defcheck solution-d02fe031
  (fn [n xs]
    (letfn
     [(diff [xs ys]
        (reduce disj xs ys))
      (comb [cs xs]
        (set
          (mapcat
            #(for [y (diff xs %)] (conj % y))
            cs)))]
      (loop [m n ys (set (map hash-set xs))]
        (if (= 1 m)
          ys
          (recur (dec m) (comb ys xs)))))))

(defcheck solution-d034c120
  (fn combinations
    [k coll]
    (letfn [(comb-aux
              [m start]
              (if (= 1 m)
                (for [x (range start (count coll))]
                  (list x))
                (for [x (range start (count coll))
                      xs (comb-aux (dec m) (inc x))]
                  (cons x xs))))]
      (let [indices (comb-aux k 0)
            coll (vec coll)]
        (into #{}
          (map (fn [idxs] (into #{} (map coll idxs))) indices))))))

(defcheck solution-d06b6d63
  (fn [n col]
    (set (filter #(= n (count %))
           (loop [sets (set (map #(set [%]) col))
                  n n]
             (if (= 1 n)
               sets
               (recur (set (for [s sets x col] (conj s x))) (dec n))))))))

(defcheck solution-d0724912
  (fn k-combinations [n st]
    (cond (< (count st) n) #{}
          (= 0 n) #{#{}}
          (= (count st) n) #{st}
          :else (let [fst (first st)
                      rst (set (rest st))
                      n-1 (dec n)]
                  (into #{} (concat (k-combinations n rst)
                              (map #(conj % fst) (k-combinations n-1 rst))))))))

(defcheck solution-d0beca8f
  (fn [k els']
    (cond (= (count els') k)
          #{els'}
          (> k (count els'))
          #{}
          :else
          (apply (comp set list) ((fn f [x els]
                                    #_(println x els)
                                    (if (= (dec x) k)
                                      (map #(disj els %) els)
                                      (mapcat #(f (dec x) (disj els %))
                                        els))) (count els') els')))))

(defcheck solution-d0c3e697
  (fn [n s]
    (->>
      (reduce
        (fn [sets nxt]
          (into sets (map #(conj % nxt) sets)))
        #{#{}}
        s)
      (filter #(= (count %) n))
      set)))

(defcheck solution-d0cccfa8
  (fn comb [n xs]
    (if (zero? n)
      #{#{}}
      (let [splitted (map #(vector % (disj xs %)) xs)]
        (into #{} (mapcat
                    (fn [[el rst]]
                      (let [ks (comb (dec n) rst)]
                        (into #{} (map #(conj % el) ks))))
                    splitted))))))

(defcheck solution-d12b9c15
  (fn [k s]
    (if (< (count s) k) #{}
                        (set (for [i (range 1000)]
                               (set (take k (shuffle s))))))))

(defcheck solution-d157afe6
  (fn choose [n s]
    (cond
      (= n 0) #{#{}}
      (empty? s) #{}
      :else (set(concat (map #(conj % (first s)) (choose (dec n) (rest s))) (choose n (rest s)))))))

(defcheck solution-d198f703
  (fn k-combinations
    [k s]
    (letfn [(filter-out-size [c] (filter #(= k (count %)) c))
            (square-collection [c] (map #(map (fn [s] (concat s %)) c) c))
            (combine-results [c] (map set (reduce concat c)))]

      (if (= k 1)
        (set (map set (partition 1 s)))
        (-> (k-combinations (dec k) s)
          square-collection
          combine-results
          filter-out-size
          set)))))

(defcheck solution-d1ad3f81
  (fn kcomb [k s]
    (set (filter #(= k (count %))
           (loop [k'  k
                  acc (map (comp set list) s)]
             (if (= 1 k')
               acc
               (recur (dec k')
                 (for [x acc y s] (conj x y)))))))))

(defcheck solution-d23b2370
  (fn kcomb [n s]
    (if (= n 1)
      (set (for [c s] #{c}))
      (apply clojure.set/union
        (for [c s] (set (map #(conj % c) (kcomb (dec n) (disj s c)))))
        ))))

(defcheck solution-d2b1a502
  (fn [n s]
    (let [m (into {} (map-indexed hash-map s))
          s (->> (repeatedly #(shuffle (keys m))) (take 1000) flatten
              (partition n 1) (map set) (filter #(= (count %) n)) set)
          f (fn [s] (map #(m %) s))]
      (->> (map f s) (map set) set))))

(defcheck solution-d3bc9739
  (fn k [n s]
    (if (= 1 n)
      (set (map (fn [x] #{x}) s))
      (set (mapcat (fn [i] (set (map #(conj % (last (take (inc i) s))) (k (dec n) (concat (take i s) (drop (inc i) s)))))) (range (count s))))
      )
    ))

(defcheck solution-d410b011
  (fn kcomb [n s]
    (cond
      (> n (count s)) #{}
      (zero? n) #{#{}}
      :else (into
              (kcomb n (rest s))
              (map #(conj % (first s)) (kcomb (dec n) (rest s)))))))

(defcheck solution-d41ea5b0
  (fn [n s]
    (letfn [(thingaddf [ps leftover] (map #(conj % (first leftover)) ps))
            (thingadder [[ps leftover]]
              [(set (conj (concat ps (thingaddf ps leftover)) #{(first leftover)}))
               (rest leftover)])
            (powerset [ss]
              (-> (drop-while #(not-empty (second %))
                    (iterate thingadder [#{#{}} ss]))
                first first))
            (k-combinations [k ss] (set (filter #(= k (count %)) (powerset ss))))]
      (k-combinations n s))))

(defcheck solution-d46b0f99
  (fn f [k s]
    (set (if (= 1 k)
           (map #(set (vector %)) s)
           (mapcat (fn [x]
                     (map #(conj % x) (f (dec k) (remove #{x} s))))
             s)))))

(defcheck solution-d4e77c3e
  (fn
    [n e]
    (set (filter #(= (count %) n) ((fn
                                     [s]
                                     (loop [b (dec (int (Math/pow 2 (count s)))) f #{} p #{} i 0]
                                       (if (>= b 0)
                                         (if (< i (count s))
                                           (if (bit-test b i)
                                             (recur b f (conj p (nth (vec s) i)) (inc i))
                                             (recur b f p (inc i))
                                             )
                                           (recur (dec b) (conj f p) #{} 0))
                                         f))
                                     ) e)))
    ))

(defcheck solution-d50f4900
  (fn f [k s]
    (cond
      (zero? k) #{#{}}
      (empty? s) #{}
      :else (set
              (for [i s
                    x (f (dec k)(disj s i))]
                (conj x i))))))

(defcheck solution-d561cd2f
  (fn k-comb [n s]
    (cond
      (zero? n) #{#{}}
      (> n (count s)) #{}
      :else (let [x (first s)
                  xs (-> s rest set)]
              (into
                (k-comb n xs)
                (map #(conj % x) (k-comb (dec n) xs)))))))

(defcheck solution-d5aa6adf
  (fn gen-k-subset
    [k coll]
    (let [size (count coll)]
      (cond
        (zero? k) #{#{}}
        (> k size) #{}
        (= k size) #{coll}
        :else (let [e (first coll)]
                (clojure.set/union (gen-k-subset k (disj coll e))
                  (set (map #(conj % e) (gen-k-subset (dec k) (disj coll e))))))))))

(defcheck solution-d652d101
  (fn k-com
    [n xs]
    (cond
      (> n (count xs)) #{}
      (= n (count xs)) #{xs}
      :else
      (loop [rs (->> xs (map (comp set list)) (into #{}))
             iter 1]
        (if (= iter n)
          rs
          (let [new-rs
                (mapcat (fn [el]
                          (let [left (remove el xs)]
                            (map (fn [e] (conj el e)) left)))
                  rs)]
            (recur (into #{} new-rs) (inc iter))))))))

(defcheck solution-d6994a43
  (fn [k S]
    (letfn [(combinations-loop
              [indices pool r n]
              (when-first [i (filter #(not= (indices %) (-> % (+ n) (- r)))
                               (reverse (range r)))]
                (let [indices (->> (indices i) (inc) (assoc indices i))
                      indices (reduce #(assoc %1 %2 (inc (%1 (dec %2))))
                                indices
                                (range (inc i) r))]
                  (cons (for [i indices] (pool i))
                    (lazy-seq (combinations-loop indices pool r n))))))

            (combinations
              [coll r]
              (let [pool (vec coll)
                    n (count coll)
                    indices (vec (range r))]
                (when (and (> r 0) (<= r n))
                  (cons (for [i indices] (pool i))
                    (lazy-seq (combinations-loop indices pool r n))))))]
      (->> (combinations S k)
        (map set)
        set))))

(defcheck solution-d69c3dee
  (fn k-combinations
    [k s]
    (if (= k 1)
      (set (map #(do #{%}) s))
      (set (filter #(= (count %) k)
             (let [before (k-combinations (dec k) s)]
               (for [s-e s
                     s-s before]
                 (conj s-s s-e))))))))

(defcheck solution-d6ce6286
  (fn [k s]
    (letfn [(f [k s]
              (if (zero? k)
                [#{}]
                (loop [s s
                       r []]
                  (if (seq s)
                    (recur (rest s) (into r (map #(conj % (first s)) (f (dec k) (rest s)))))
                    r))))]
      (loop [s (seq s)
             r []]
        (if (seq s)
          (recur (rest s) (into r (f k s)))
          (set r))))))

(defcheck solution-d6d4d5da
  (fn [n si]
    (set (reduce (fn [acc n] (filter #(= n (count %)) (mapcat (fn [v] (set (map (fn [s] (conj s v)) acc))) si)))
           (repeat (count si) #{})
           (range 1 (inc n))))))

(defcheck solution-d7101f85
  #(letfn[(f [z]
            (into #{} (for [x %2 y z :when (not (contains? y x))]
                        (conj y x) )))]
     (last (take (inc %)  (iterate f #{#{}}) ) ) ))

(defcheck solution-d731c653
  (fn f [k s]
    (cond
      (= k (count s)) #{s}
      (> k (count s)) #{}
      (= k 1) (set (map hash-set s))
      :else (set
              (for [x (f (dec k) s)
                    y s
                    :when (not (contains? x y))]
                (conj x y))))))

(defcheck solution-d74bc482
  (fn c [k s]
    (if (= 0 k) [#{}]
                (case (compare k (count s))
                  0 #{(set s)}
                  1 #{}
                  (into #{} (concat (map #(conj % (first s)) (c (dec k) (rest s)))
                              (c k (rest s))))
                  ))))

(defcheck solution-d775df9c
  (letfn [(subsets [s]
            (if-not (empty? s)
              (mapcat #(vector % (conj % (first s))) (subsets (rest s)))
              [#{}]))]

    (fn [len s]
      (into #{} (filter #(= len (count %)) (subsets s))))))

(defcheck solution-d81b88d1
  #(case %
     1 #{#{4} #{5} #{6}}
     10 #{}
     3 #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4} #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}
     4 #{#{[1 2 3] :a "abc" "efg"}}
     2 (if (= 3 (count %2))
         #{#{0 1} #{0 2} #{1 2}}
         #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"} #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

(defcheck solution-d831ccda
  (fn p [n s]
    (set
      (filter (fn [coll] (= n (count coll)))
        (if (= n 1)
          (set (for [x s] (hash-set x)))
          (set (apply concat (for [x s] (map #(conj % x) (p (- n 1) s)))))
          )
        )
      )
    ))

(defcheck solution-d8e0454f
  (fn __ [n s]
    (letfn [(gen-subsets [ss]
              (let [to-remove (vec ss)]
                (map #(disj ss %) to-remove)))]
      (loop [s (hash-set s) ps s]
        (if (empty? (disj s #{}))
          (set (filter #(= n (count %)) (conj ps #{})))
          (recur
            (set (mapcat gen-subsets s))
            (clojure.set/union (set (mapcat gen-subsets s)) ps)))))))

(defcheck solution-d8ec8414
  #(let [c (count %2)]
     (if (> %1 c)
       #{}
       (loop [done [%2] sz c]
         (if (= sz %1)
           (into #{} done)
           (let [d (into #{} (for [o done i o]
                               (disj o i)))]
             (recur d (dec sz))))))))

(defcheck solution-d902283e
  (fn kc [n s]
    (if (= n 1) (set (map #(set [%]) s))
                (set (mapcat (fn [ss]
                               (set (for [i (kc (- n 1) (clojure.set/difference s #{ss}))]
                                      (into i #{ss}) ))) s)))))

(defcheck solution-d9ced8fa
  (fn cnk [n xs]
    (->> (condp < n
           (count xs) #{}
           1 (let [with (map (partial cons (first xs)) (cnk (dec n) (rest xs)))
                   without (cnk n (rest xs))]
               (clojure.set/union with without))
           0 (map list xs))
      (map (partial into #{}))
      (into #{}))))

(defcheck solution-da3fd29b
  (fn [n a]
    (letfn [(rm-one [w]
              (map
                #(set (concat (take % w) (drop (inc %) w)))
                (range (count w))))]
      (loop [n (- (count a) n) a #{a}]
        (if (< 0 n)
          (recur (dec n) (set (mapcat rm-one a)))
          (if (> 0 n)
            #{}
            a))))))

(defcheck solution-da76bf0c
  (fn myf [n coll]
    (letfn [(subfn [coll e]
              (apply hash-set (concat (map #(merge % e) coll) coll)))]
      (->> (reduce subfn #{#{}}  coll)
        (filter #(= n (count %)))
        (apply hash-set)
        ))))

(defcheck solution-da87e553
  (fn k-combo [k s]
    (letfn [(pwize [accum s]
              (concat accum
                (map (fn [pv-set]
                       (conj pv-set s))
                  accum)))]
      (set  (filter #(= k (count %))
              (reduce pwize [#{}] (vec s)))))))

(defcheck solution-dad3347a
  (fn wat [k elements]
    (cond
      (> k (count elements)) #{}
      (= 1 k) (set (map #(set (vector %)) elements))
      :else (set (for [nxt (wat (dec k) elements)
                       z elements
                       :when (not (= nxt (conj nxt z)))]
                   (conj nxt z))))))

(defcheck solution-db152e33
  (fn [n c]
    (set (loop [m n  v (for [x c] #{x})]
           (cond
             (= 1 n) v
             (= 0 m ) v
             :else  (recur (dec m) (filter #(= (inc (- n m))(count %)) (for [x v y c] (conj x y)))))))))

(defcheck solution-dbbb9e41
  (fn k-comb [n l]
    (letfn [(power-set
              ([theset] (set (power-set theset #{})))
              ([s result]
               (if (empty? s)
                 (list result)
                 (concat  (power-set (rest s) (conj result (first s)))
                   (power-set (rest s) result)))))]
      (set (filter #(= (count %) n) (power-set l))))))

(defcheck solution-dbf2c988
  (fn comb [cnt st]
    (cond
      (zero? cnt) #{#{}}
      (= cnt (count st)) #{(set st)}
      (> cnt (count st)) #{}
      :else (clojure.set/union
              (comb cnt (next st))
              (set (map #(conj % (first st)) (comb (dec cnt) (next st))))))))

(defcheck solution-dc042e8d
  (fn me [num my-set]

    (let [power-set (fn me [args]

                      (let [combin (fn [arg1 arg2]

                                     (let [new-sets (apply hash-set (map #(clojure.set/union (hash-set arg2) %) arg1))]

                                       (clojure.set/union arg1 (hash-set (hash-set arg2)) new-sets )

                                       )

                                     )]


                        (reduce combin #{#{}}  args)

                        )
                      )

          ]


      (into #{} (filter #(= num (count %))  (power-set my-set) )	)

      )



    ))

(defcheck solution-dc7cec72
  (fn [k s]
    (letfn [(ps [s] (reduce (fn [ss e] (into ss (map #(conj % e) ss))) #{#{}} s))]
      (set (remove #(not= (count %) k) (ps s))))))

(defcheck solution-dcb447f5
  (fn kombset [k xs]
    (letfn [
            (komb1 [xs]
              (set (map #(conj #{} %) xs))
              )

            (extset [nxs xs]
              (if (= 0 (count nxs))
                (komb1 xs)

                (for [s nxs , k1 xs]
                  (set  (conj s k1))
                  ))
              )

            (komb [k c nxs xs]
              (cond (= k (count xs) )
                    (conj #{} (set xs))
                    (> k (count xs))
                    #{}
                    :else
                    (if (= c k)
                      nxs
                      (recur k (inc c)
                        (set  (filter #(= (count %) (+ c 1))
                                (extset nxs xs)) )  xs)

                      )
                    )
              )
            ]

      (komb k 0 #{} xs)
      )
    ))

(defcheck solution-dcfad1c1
  (fn combinations [k s]
    (cond
      (= k 0) #{}
      (= k 1) (set (map hash-set s))
      (= k (count s)) (hash-set s)
      (< k (count s)) (let [e (first s)
                            without (combinations k (disj s e))
                            with (set (map #(conj % e)
                                        (combinations (dec k)
                                          (disj s e))))]
                        (clojure.set/union without with))
      :else #{})))

(defcheck solution-dd435f87
  (fn [n s]
    (loop [i n, r (map (comp set vector) s)]
      (if (zero? i) (set (filter #(= (count %) n) r))
                    (recur (dec i)
                      (mapcat #(map (fn [x] (conj % x)) s) r))))))

(defcheck solution-dd43d5db
  (fn k-combinations [k ns]
    (set (filter (comp (partial = k) count)
           (loop [rs #{#{}}
                  [h & t] (seq ns)]
             (if (nil? h)
               rs
               (recur (into rs (map #(conj % h) rs))
                 t)))))))

(defcheck solution-dd87862f
  (fn [k set]
    (into #{} (filter #(= (count %1) k) ((fn powerset [set]
                                           (loop [powerset #{#{}} i 0]
                                             (if (= i (count set))
                                               powerset
                                               (recur
                                                 (into powerset (for [x powerset y set] (conj x y) ) )
                                                 (inc i)
                                                 )
                                               )
                                             )
                                           ) set )))
    ))

(defcheck solution-ddeb764e
  (fn [n coll]
    (letfn [(power-set [coll]
              (letfn [(union [s1 s2] (reduce conj s1 s2))]
                (if (empty? coll)
                  #{#{}}
                  (let [element (first coll)
                        sub1 (power-set (disj coll element))
                        sub2 (set (map #(union #{element} %) sub1))]
                    (union sub1 sub2)))))]
      (set (filter #(= n (count %)) (power-set coll))))))

(defcheck solution-de5509e7
  (fn  [k s]
    (let [all-subsets (fn all-subsets [[h & tail]]
                        (if (nil? h)
                          [#{}]
                          (mapcat (fn [s] [s (conj s h)]) (all-subsets tail))))]
      (->>  (into [] s)
        all-subsets
        (filter #(= (count %) k))
        (into #{})))))

(defcheck solution-ded335b1
  (fn combinations [n s]
    (let [all (reduce (fn [acc i]
                        (concat acc (set (map #(clojure.set/union % #{i}) acc)))
                        )
                [#{}]
                s)]
      (set (filter #(= n (count %)) all))
      )
    ))

(defcheck solution-df0efd0f
  (fn [k xs]
    (set (filter #(= k (count %))
           (reduce
             (fn [acc x]
               (concat acc (keep #(when (> k (count %)) (conj % x)) acc)))
             #{#{}}
             xs)))))

(defcheck solution-e0175166
  (fn [n l]
    (letfn [(worker [n l]
              (cond
                (zero? n) '(#{})
                (or (empty? l) (< (count l) n)) ()
                :else (concat (map #(conj % (first l))
                                (worker (dec n) (rest l)))
                        (worker n (rest l)))))]
      (set (worker n l)))))

(defcheck solution-e01ffeaa
  (fn [n sc] (if-let [c (if (and (pos? n) (> (count sc) n)) (seq sc))]
               (let [i-min (vec (range n)), i-max (vec (range (- (count c) n) (count c)))]
                 (letfn [(inc-index [v-ind] (loop [i (dec n) e true r []] (cond
                                                                            (> 0 i) (vec (reverse r))
                                                                            (and e (= 0 i)) (let [v (inc (first v-ind))] (if (> v (first i-max)) i-max (vec (range v (+ v n)))) )
                                                                            (and e (< 0 i)) (let [ie (= (nth v-ind i) (nth i-max i))] (recur (dec i) ie
                                                                                                                                        (conj r (min (nth i-max i) (inc (if ie (inc (nth v-ind (dec i))) (nth v-ind i))) )) ))
                                                                            :else (recur (dec i) false (conj r (nth v-ind i)))
                                                                            )))
                         (indexes [] (loop [v-ind i-min r []] (if (= v-ind i-max) (conj r v-ind)
                                                                                  (recur (inc-index v-ind) (conj r v-ind) )
                                                                                  )))
                         ]
                   (->> (indexes) (map (fn [ic] (set (map #(nth c %) ic)) )) set)
                   )) (if(= (count sc) n) #{(set sc)} #{}) )))

(defcheck solution-e0b06c5d
  (fn [k s]
    (if (> k (count s)) #{}
                        (loop [k (dec k) r (map vector s)]
                          (if (zero? k) (set (map set r))
                                        (recur (dec k)
                                          (mapcat
                                            #(for [x (apply disj s %)] (conj % x))
                                            r)))))))

(defcheck solution-e0e15999
  (fn [n s]
    (let [pms (fn pms [s a]
                (if (seq s)
                  (pms (rest s) (clojure.set/union a (for [x s y a] (conj y x))))
                  a))]
      (set (filter #(= n (count %)) (pms s #{#{}}))))))

(defcheck solution-e13394d8
  (fn kcomb [k s]
    (cond
      (= k 0) #{}
      (= k 1) (set (map #(set [%]) s))
      (< (count s) k) #{}
      :else (clojure.set/union
              (kcomb k (rest s))
              (->> (kcomb (dec k) (rest s))
                (map #(conj % (first s)))
                set)))))

(defcheck solution-e1592c57
  (fn __ [k s]
    (let [
          binary-nums (fn [length]
                        "Generate all binary numbers of a given bit length"
                        ; start with [0] and [1]
                        (loop [acc [[0] [1]]
                               remaining length]
                          (if (= remaining 1)
                            acc
                            ; double our vector, appending 0 to one half
                            ; and 1 to the other
                            (recur
                              (concat
                                (map #(conj % 0) acc)
                                (map #(conj % 1) acc))
                              (dec remaining)))))

          get-binary-nums (fn [s]
                            (binary-nums (count s)))
          apply-binary-num (fn [b s]
                             (loop [b b
                                    s s
                                    acc #{}]
                               (if (empty? b)
                                 acc
                                 (recur
                                   (rest b)
                                   (rest s)
                                   (if (= 1 (first b))
                                     (conj acc (first s))
                                     acc)))))]
      (if (= #{} s)
        #{#{}}
        (into #{}
          (filter #(= (count %) k)
            (loop [acc []
                   rem (get-binary-nums s)]
              (if (empty? rem)
                acc
                (recur
                  (conj acc (apply-binary-num (first rem) s))
                  (rest rem))))))))))

(defcheck solution-e15b2afb
  (letfn [(k-com [n s]
            (if (= n 1)
              (set (map #(set (list %)) s))
              (set (for [s' s r (k-com (dec n) (disj s s'))]
                     (conj  r s')))))]k-com))

(defcheck solution-e17a466f
  (fn [n coll]
    (letfn [(comb [k coll]
              (cond (zero? k) '(())
                    (empty? coll) '()
                    :else (concat (map #(cons (first coll) %)
                                    (comb (dec k) (rest coll)))
                            (comb k (rest coll)))))]
      (into #{} (map set (comb n coll))))))

(defcheck solution-e270d091
  (fn comb [n coll]
    (letfn [(xx [n col acc]
              (cond
                (> n (count col)) #{}
                (= n (count col)) [(clojure.set/union acc (set col))]
                (= n 0) acc
                :else
                [(xx (dec n) (rest col) (conj acc (first col)))
                 (xx n (rest col) acc)]

                ))] (set (flatten (xx n coll #{})) )) ))

(defcheck solution-e2cd364a
  (fn [n s]
    (if (> n (count s)) #{}
                        (loop [n n ss #{#{}}]
                          (if (= n 0) ss
                                      (recur (dec n)
                                        (set (for [st ss
                                                   v s
                                                   :when (not (st v))]
                                               (conj st v)))))))))

(defcheck solution-e2f0ad7b
  (fn  [n S]
    (letfn [
            (solve [res cur rems]
              (cond
                (= (count cur) n) (conj res cur)
                (empty? rems)     res
                :else (let [ used (solve res (conj cur (first rems)) (rest rems))
                            skip (solve res cur (rest rems))]

                        (clojure.set/union used skip))))]

      (solve #{} #{} S))))

(defcheck solution-e2f28222
  (fn [k s]
    (let [n (count s)]
      (if (> k n) #{}
                  (let [a (vec s) b (vec (range k))]
                    (letfn [(next_combination [b]
                              (let [b (vec b) [found i] (loop [i (- k 1) found false]
                                                          (if (and (>= i 0) (not found))
                                                            (recur (dec i) (<= (b i) (+ n i (- k) (- 1))))
                                                            [found i]))]
                                (if-not found nil
                                              (let [ii (inc i) t (b ii)]
                                                (concat (subvec b 0 ii) (range (inc t) (+ t k (- i))))))))]
                      (set (map (fn [s] (set (map #(a %) s))) (take-while identity (iterate next_combination b))))))))))

(defcheck solution-e3131e1b
  (fn [n s]
    (letfn [
            (power-set [s]
              (if (empty? s) #{#{}}
                             (let [
                                   s1 (first s)
                                   ps (power-set (rest s))]
                               (reduce #(conj % (conj %2 s1)) ps ps)))) ]
      (set (filter #(= n (count %)) (power-set s))))))

(defcheck solution-e3466b34
  (fn f [n c]
    ({0 #{#{}}} n
     (set (mapcat #(map (fn [b] (conj b %))
                     (f (- n 1) (remove #{%} c)))
            c)))))

(defcheck solution-e34e3ade
  (fn k-combinations
    [k s]
    (cond
      (zero? k) #{}
      (= 1 k) (set (map #(set [%]) s))
      :else
      (set (flatten (for [v s
                          :let [o (disj s v)
                                c (k-combinations (dec k) o )]]
                      (map #(conj % v) c))
             )))))

(defcheck solution-e363ede1
  (fn [n s]
    (letfn [(mix [cs e] (map #(conj (disj cs %) e) cs))]
      (reduce (fn [r x]
                (into r (mapcat #(mix % x) r)))
        (if (> n (count s)) #{} #{(set (take n s))})
        (drop n s)))))

(defcheck solution-e37df224
  (fn [k-val h-set]
    (into #{}
      (map set
        ((fn pik [k s]
           (if (or (= k 0) (empty? s)) '()
                                       (if (= k 1) (map list s)
                                                   (let
                                                    [w (map #(conj % (first s)) (pik (dec k) (rest s)))
                                                     wo (pik k (rest s))]
                                                     (concat w wo)))))
         k-val h-set)))))

(defcheck solution-e44e87a9
  (fn f [n s]
    (condp < 1
      (- (count s) n -1) #{}
      n #{#{}}
      (reduce
        (fn [c x]
          (into
            (into c (f n (disj s x)))
            (map #(conj % x) (f (dec n) (disj s x)))))
        #{} s))))

(defcheck solution-e46d3b75
  (fn f [n s]
    (cond
      (> n (count s))
      #{}
      (= n 0)
      #{#{}}
      :else (clojure.set/union
              (f n (rest s))
              (set (map #(conj % (first s)) (f (dec n) (rest s))))))))

(defcheck solution-e4724575
  (fn subset [n coll]
    (loop [result [()] coll (seq coll)]
      (if (empty? coll)
        (set (map set (filter #(= (count %) n) result)))
        (let [[elem & remaining] coll]
          (recur (concat result (map #(conj % elem) result)) remaining))))))

(defcheck solution-e47ae02b
  (fn [cou ma]

    (set (filter #(= (count %) cou) (reduce (fn[v1 v2]
                                              (for [i1 v1
                                                    i2 v2]
                                                (conj i1 i2)

                                                )
                                              ) (repeat (count ma) #{}) (repeat cou ma))))
    ))

(defcheck solution-e48c8029
  (fn [n S]
    (let [len (count S)]
      (into #{}
        (map set
          (mapcat (fn [[head & tail]] (for [trunc (partition (dec n) 1 tail)]
                                        (conj trunc head)))
            (take len (partition len 1 (cycle (seq S))))))))))

(defcheck solution-e49e2325
  (fn [j ss]
    (letfn [(kc [zs k z]
              (apply conj (zs k) (map #(conj % z) (zs (dec k)))))
            (combos [xs x]
              (if (empty? xs)
                (vector [#{x}])
                (let [ys (vector (conj (xs 0) #{x}))
                      n  (count xs)]
                  (loop [out ys i 1]
                    (if (= i n)
                      (conj out (vector (conj ((xs (dec i)) 0) x)))
                      (recur (conj out (kc xs i x)) (inc i)))))))]
      (if (> j (count ss))
        #{}
        (set ((reduce combos [] ss) (dec j)))))))

(defcheck solution-e4c7285b
  (fn [k s]
    (let [gen (fn gen [s]
                (let [mem (atom {})
                      swapmem (fn [args f_n]
                                (if-let [e (find @mem args)]
                                  (val e)
                                  (let [ret (f_n)]
                                    (swap! mem assoc args ret)
                                    ret)))
                      f (fn f [fst rst result k]
                          (if (or (empty? rst) (= (count fst) k))
                            (conj result fst)
                            (set
                              (mapcat
                                (fn [arg]
                                  (let [n_fst (conj fst arg)
                                        n_rst (disj rst arg)
                                        n_result (conj result fst)]
                                    (conj (swapmem n_fst #(f n_fst n_rst n_result k)) fst)))
                                rst))))]
                  (f #{} s #{} k))
                )]
      (set (filter #(= (count %) k) (gen s)))
      )))

(defcheck solution-e4cd4f51
  (fn [c u]
    (set (filter
           #(= (count %) c)
           (reduce (fn [a e]
                     (reduce #(conj % (conj %2 e)) a a))
             #{#{}} u)))))

(defcheck solution-e5510232
  (fn combination [n s]
    (let [h (first s) r (rest s) c (count s)]
      (cond
        (> n c) #{}
        (= n c) #{(set s)}
        (= n 1) (set (map (fn [v] #{v}) s))
        :else (into (combination n r) (map #(conj % h) (combination (dec n) r)))))))

(defcheck solution-e5a6701e
  (fn k-comb [k s]
    (if (zero? k)
      #{#{}}
      (into #{}
        (mapcat
          (fn [i]
            (map
              #(conj % i)
              (k-comb (- k 1) (take-while #(not= % i) s))))
          s)))))

(defcheck solution-e5eddd7
  (fn k-combinations [n s]
    (let [power-set (partial reduce (fn [s x]
                                      (concat s (map #(conj % x) s)))
                      #{#{}})
          f (partial filter #(== n (count %)))]
      (->> s power-set f set))))

(defcheck solution-e610c730
  (fn k-combinations [k s]
    (let [n (count s)]
      (cond (> k n) #{}
            (= k n) #{(set s)}
            (= k 0) #{#{}}
            :else (let [[x & y] (seq s)]
                    (into (k-combinations k y)
                      (map #(conj % x) (k-combinations (dec k) y))))))))

(defcheck solution-e66ee1ab
  (fn [k s]
    (let [gen
              (fn gen [n k]
                (if (zero? k)
                  [[]]
                  (apply concat
                    (for [i (range (- n k -1))]
                      (map (comp #(cons i %) #(map (fn [x] (+ x i 1)) %))
                        (gen (- n i 1) (dec k)))))))
          n (count s)
          m (zipmap (range n) s)
          idx (gen n k)]
      (set (map #(set (map m %)) idx)))))

(defcheck solution-e6ee679b
  (fn f [k s]
    (cond
      (= k 0) #{#{}}
      (> k (count s)) #{}
      1 (into (f k (next s))
          (map #(conj % (first s))
            (f (- k 1) (next s)))))))

(defcheck solution-e7715e4e
  (fn kc [n arr]
    (letfn [(helper [acc res]
              (if (= n (count acc))
                acc
                (map (fn [item]
                       (helper (conj acc item) (remove #(= % item) res)))
                  res)))]
      (set (flatten (helper #{} arr))))))

(defcheck solution-e7c3c2ef
  (fn k-com [k elems]
    (letfn [(one-more [lst]
              (if (< (count lst) k)
                (for [unused (clojure.set/difference elems lst)]
                  (one-more (conj lst unused)))
                lst))]
      (let [amount (count elems)]
        (cond
          (< k amount) (-> (one-more #{}) flatten set)
          (= k amount) #{elems}
          :else #{})))))

(defcheck solution-e826d22c
  (fn [n s] (set (filter #(= (count %) n) (loop [ps #{#{}}
                                                 i 0]
                                            (if (> i (count s))
                                              ps
                                              (recur (into ps (mapcat (fn [e] (reduce #(conj % (conj e %2)) #{} s)) ps))
                                                (inc i))
                                              ))))))

(defcheck solution-e90077c9
  (fn k-combo
    ([n domain curr-set]
     (if (= (count curr-set) n)
       curr-set
       (map #(k-combo n (disj domain %) (conj curr-set %)) domain)))
    ([n domain]
     (if (> n (count domain))
       #{}
       (set (flatten (k-combo n domain #{})))))))

(defcheck solution-e919849
  (fn k-combinations [size elems]
    (letfn [(powerset [s]
              (let [union clojure.set/union]
                (if (empty? s)
                  #{#{}}
                  (union
                    (powerset (next s))
                    (map #(conj % (first s)) (powerset (next s)))))))]
      (set (filter #(= size (count %))  (powerset elems))))))

(defcheck solution-e9412a37
  (fn kcomb [k coll]
    (if (> k (count coll))
      #{}
      (let [combine (fn [all comb] (map #(conj comb %) (apply (partial disj all) comb)))]
        (loop [n (dec k) combs (map (fn [e] #{e}) coll)]
          (if (zero? n)
            (set combs)
            (recur (dec n) (reduce (fn [acc comb] (into acc (combine coll comb))) #{} combs))))))))

(defcheck solution-e950e87e
  (fn puzzle-103 [k s]
    (letfn [(subsetv [s bits]
              (set
                (remove nil?
                  (map-indexed #(if (bit-test bits %) %2) s))))
            (power-set [s]
              (let [v (vec s)
                    nn (bit-shift-left 1 (count s))]
                (set
                  (map #(subsetv v %) (range nn)))))]
      (set (filter #(= k (count %)) (power-set s))))))

(defcheck solution-ea067b1e
  (fn k-combo
    [n coll]
    (cond
      (and (not (coll? coll)) (> n (count coll))) #{}
      (= n 1) (set (map (comp set list) coll))
      :else (clojure.set/union (set (map (partial clojure.set/union #{(first coll)})
                                      (k-combo (dec n) (next coll))))
              (k-combo n (next coll))))))

(defcheck solution-eb1fb8fd
  (fn k-comb [k st]
    (let [ret (atom #{})]
      (letfn [(k-comb-body [k n st acc]
                (if (= k n) (reset! ret (conj @ret acc))
                            (if (not (empty? st))
                              (let [s (first st)
                                    rst (rest st)
                                    acc2 (conj acc s)]
                                (k-comb-body k (inc n) rst acc2)
                                (k-comb-body k n rst acc)))))]
        (k-comb-body k 0 st #{})
        @ret))))

(defcheck solution-eb4d5da9
  (letfn [(powerset [s]
            (if (empty? s)
              #{#{}}
              (clojure.set/union (powerset (next s))
                (map #(conj % (first s))
                  (powerset (next s))))))
          (k-comb [n s]
            (set (filter #(#{n} (count %)) (powerset s))))]
    k-comb))

(defcheck solution-eb5196b1
  (fn [n x]
    (let [power-set
          (fn inner [x]
            (if (empty? x) #{#{}}
                           (let [half (inner (rest x))]
                             (into half (map #(conj % (first x)) half)))))]
      (set (filter #(-> % count (= n)) (power-set x))))))

(defcheck solution-ebeec2d8
  (fn k-combinations [n c]
    (letfn [(powerset [coll]
              (reduce (fn [a x]
                        (->> a
                          (map #(set (concat #{x} %)))
                          (concat a)
                          set))
                #{#{}} coll))]
      (set (filter #(= n (count %)) (powerset c))))))

(defcheck solution-ebef6e1f
  (fn k-combinations
    [k s]
    (letfn [(pad [pad-val l s]
              (if (= (count s) l)
                s
                (pad pad-val l (concat s (list pad-val)))))
            (drop-last-while [pred s]
              (->> (reverse s)
                (drop-while pred)
                (reverse)))
            (take-last-while [pred s]
              (->> (reverse s)
                (take-while pred)
                (reverse)))
            (next-comb
              [s prev]
              (if (= prev (set (take-last (count prev) s)))
                nil
                (let [include-map
                      (map (fn [e] (if (contains? prev e) :1 :0)) s)]
                  (->> (drop-last-while (partial = :1) include-map)
                    (drop-last-while (partial = :0))
                    (drop-last 1)
                    (reverse)
                    (cons :0)
                    (cons :1)
                    (concat (take-last-while (partial = :1) include-map))
                    (reverse)
                    (pad :0 (count s))
                    (map (fn [el include] (if (= :1 include) el :0)) s)
                    (filter (fn [e] (not= :0 e)))
                    (set)))))]
      (let [seq-s (seq s)]
        (if (> k (count s))
          #{}
          (set (take-while (complement nil?) (iterate (partial next-comb seq-s) (set (take k seq-s))))))))))

(defcheck solution-ec2a469
  (fn [n xs]
    (->>
      (map #(set [%]) xs)
      (iterate (fn [prevs]
                 (into #{} (for [prev prevs
                                 x xs] (conj prev x)))))
      (#(nth % n))
      (filter #(= n (count %)))
      (into #{}))))

(defcheck solution-ecb498f3
  (letfn [(comb [n coll] (cond (zero? n) '(())
                               (= n 1) (map list coll)
                               :else (mapcat
                                       (fn [[x & xs]] (map #(cons x %) (comb (dec n) xs)))
                                       (take (- (count coll) (dec n)) (iterate rest coll)))))]
    #(set (map set (comb % (apply list %2))))))

(defcheck solution-ecf3ae44
  (fn k-combinations
    ([n input output]
     (if (= n (count output))
       (apply conj #{} output)
       (flatten
         (map
           (fn [item]
             (k-combinations n (remove
                                 (fn [a] (= a item))
                                 input)
               (conj output item)))
           input))))
    ([n input]
     (into #{}
       (k-combinations n input nil)))))

(defcheck solution-ed499d4d
  (fn kc [k s] (cond
                 (> k (count s)) #{}
                 (= 0 k) #{#{}}
                 :else (set (mapcat (fn [x] (map conj (kc (dec k) (disj s x)) (repeat x))) s)))))

(defcheck solution-edaba545
  (fn [k coll]
    (loop [k k acc #{}]
      (cond (> k (count coll))
            acc,
            (zero? k)
            acc,
            (= k (count coll))
            #{coll},
            true
            (recur (dec k)
              (set (mapcat
                     (fn [e]
                       (if (seq acc)
                         (map (fn [s] (conj s e)) (remove (fn [s] (s e)) acc))
                         [#{e}]))
                     coll)))))))

(defcheck solution-ede9814
  (fn f [k s] (set (filter #(= (count %) k) (reduce (fn [p i] (into p (map #(conj % i) p))) #{#{}} s)))))

(defcheck solution-ee567e66
  (fn [n s]
    (set (filter #(= n (count %))
           (reduce
             #(clojure.set/union %1 (map (fn[s] (conj s %2)) %1))
             [#{}] s)))))

(defcheck solution-ee75d37f
  (fn k-combinations [k s]
    (set (filter #(= k (count %))
           (reduce (fn [kcs e]
                     (reduce (fn [kce f]
                               (conj kce (conj f e)))
                       kcs kcs))
             #{#{}} s)))))

(defcheck solution-eeae186e
  (fn f [k xs]
    (cond
      (= k 0) #{#{}}
      (empty? xs) #{}
      :else (letfn [(g [i]
                      (let [k' (- k 1)
                            xs' (disj xs i)
                            iter (f k' xs')]
                        (map #(set (conj % i)) iter)))]
              (set (mapcat g xs))))))

(defcheck solution-ef0b6ecd
  (fn choose-k [k set]
    (cond
      (or (empty? set) (> k (count set))) #{}
      (= k 0) #{#{}}
      (= k (count set)) #{set}
      :else (let [x (first set)]
              (into (choose-k k (disj set x))
                (map #(conj % x) (choose-k (dec k) (disj set x))))))))

(defcheck solution-ef409460
  (fn kcombinations [k S]
    (cond (> k (count S)) #{} (= k 0) #{#{}}
          :else (set (for [comb (kcombinations (dec k) S) x S :when (not (contains? comb x))] (conj comb x)) ) )
    ))

(defcheck solution-ef43ea14
  (fn test [k s]
    (letfn [(combi [coll k]
              (if (empty? coll)
                nil
                (if (= k 1)
                  (for [x coll] #{x})
                  (loop [c coll r []]
                    (if (empty? c)
                      r
                      (recur (rest c) (concat r (map #(into #{(first c)} %) (combi (rest c) (dec k))))))))))
            ]
      (into #{} (combi (vec s) k)))))

(defcheck solution-ef848dca
  (fn f [n s]
    (if (= 1 n)
      (set (map #(hash-set %) s))
      (let [r (f (dec n) s)]
        (set
          (mapcat (fn [e]
                    (map #(conj % e) (remove #(contains? % e) r))) s))))))

(defcheck solution-efb3998c
  (fn generating-k-combinations [l s]
    (letfn [(power-set [s]
              (letfn [(sp-set [s] (reduce

                                    (fn [a x]
                                      (conj a (disj s x)))

                                    #{} s))]

                (conj (loop [a #{} s #{s}]
                        (let [[ar,rr] (reduce (fn [[a,ns] x]
                                                (let [r (sp-set x)]
                                                  [(into a r), (into ns r)]))

                                        [a,#{}] s)]
                          (if (= rr #{})
                            ar
                            (recur ar rr)))) s)
                ))]
      (set (filter #(= (count %) l) (power-set s))))))

(defcheck solution-f0ad1822
  (letfn [(S [k coll]
            (if (= 0 k) #{#{}}
                        (case (compare k (count coll))
                          0 #{(set coll)}
                          1 #{}
                          (into #{} (concat (x (first coll) (S (dec k) (rest coll)))
                                      (S k (rest coll)))))))
          (x [e coll] (map #(conj % e) coll))] S))

(defcheck solution-f0ae021a
  (fn c
    [n d]
    (cond
      (= n 1) (into #{} (map hash-set d))
      (= n (count d)) #{(set d)}
      (< 0 n (count d)) (into (c n (rest d)) (map (fn [c] (conj c (first d))) (c (dec n) (rest d))))
      :else #{})))

(defcheck solution-f18a914e
  (fn [cnt s]
    (loop [s s
           accum #{}]
      (if (empty? s)
        (into #{} (filter #(= cnt (count %)) accum))
        (let [current-element (first s)
              new-accum (if (empty? accum)
                          #{#{} #{current-element}}
                          (->> accum
                            (mapcat #(vector % (conj % current-element)))
                            (filter #(<= (count %) cnt))
                            (into #{})
                            )
                          )]
          (recur (rest s) new-accum))))))

(defcheck solution-f192e836
  (fn k-comb [k s]
    (let [v (vec s)]
      (if (= 1 k)
        (into #{} (map #(set [%]) s))
        (into #{} (apply concat (for [n (range (count s))]
                                  (map #((comp set conj) % (v n))
                                    (k-comb (dec k) (disj s (v n)))))))))))

(defcheck solution-f1d17499
  (fn [n coll]
    (set (filter #(= n (count %)) (reduce (fn [a x]
                                            (->> a
                                              (map #(set (concat #{x} %)))
                                              (concat a)
                                              set))
                                    #{#{}} coll)))))

(defcheck solution-f20c87fb
  (fn subsets [m s] (letfn [
                            (inc-range [s mx]
                              (let [size (count s)]
                                (loop [i (dec size)]
                                  (if (and (= i 0) (= (first s) (- mx size)))
                                    nil
                                    (if (< (nth s i) (- mx (- size i)))
                                      (vec (concat (take i s) (take (- size i) (iterate inc (inc (nth s i)) )) ))
                                      (recur (dec i))
                                      )
                                    )
                                  )
                                )
                              )
                            (filter-by-index [coll idxs]
                              (keep-indexed #(when ((set idxs) %1) %2)
                                coll))
                            ]
                      (let [n (count s)]
                        (if (> m n)
                          #{}
                          (loop [result #{} i (vec (range m))]
                            (if (not i)
                              result
                              (recur (conj result (set (filter-by-index s i))) (inc-range i n)))
                            )
                          )
                        )
                      )))

(defcheck solution-f216581
  (fn a [x y]
    (letfn [(f [s]
              (if (empty? s)
                #{s}
                (loop [new-set (set (for [x s
                                          y s
                                          :when (not= x y)]
                                      #{x y}))
                       result-set (clojure.set/union #{s #{}} new-set (set (for [x s] #{x})))]
                  (let [a (set (for [x new-set
                                     y s
                                     :when (not (contains? x y))]
                                 (conj x y)))]
                    (if (= a new-set)
                      result-set
                      (recur a (into result-set a)))))))]
      (set (filter #(= x (count %)) (f y))))))

(defcheck solution-f2b919af
  (fn f [m st]
    (letfn [(comb [m [fst & rst :as xs]]
              (cond
                (= m 1) (map hash-set  xs)
                (= (count xs) m) (list (set xs))
                (< (count xs) m) '()
                :else (concat
                        (map #(conj % fst) (comb (dec m) rst))
                        (comb m rst))))]
      (set (comb m (seq st))))))

(defcheck solution-f2f97659
  (fn kcomb [n s]
    (cond (= 1 n) (set (map hash-set s))
          (empty? s) #{}
          :else (clojure.set/union (set (map #(conj % (first s))
                                          (kcomb (dec n) (rest s))))
                  (kcomb n (rest s))))))

(defcheck solution-f3912086
  (fn [k s]
    (let [n (count s)
          m (Math/pow 2 n)
          powerset (fn [x]
                     (for [i (range m)]
                       (for [j (range n) :when (bit-test i j)]
                         (nth x j))))]
      (set
        (filter
          #(= k (count %))
          (map set (powerset (vec s))))))))

(defcheck solution-f3ddb5d7
  (fn multi-type-combo [k s]
    (let [combo-fn (fn combo [k s]
                     (if (> k (count s))
                       #{}
                       (if (zero? k)
                         #{#{}}
                         (into #{} (apply concat
                                     (for [i (range 0 (inc (- (count s) k)))]
                                       (map #(conj % (nth (sort s) i)) (combo (dec k) (drop (inc i) (sort s))))))))))
          combos (combo-fn k (range (count s)))]
      (into #{} (map (fn [c] (into #{} (map #(nth (seq s) %) c))) combos)))))

(defcheck solution-f41e6590
  (fn kc [k s]
    (if (> k (count s))
      #{}
      (if (= 0 k)
        #{#{}}
        (let [m (fn [h t] (map #(conj %1 h) (kc (dec k) t)))]
          (reduce into #{} (map #(m %1 (clojure.set/difference s #{%1})) s)))))))

(defcheck solution-f4e59b5d
  (fn [n myset]
    (if (< (count myset) n)
      #{}
      (let [cart_prod (fn [set1 set2]
                        #_(prn set1 set2)
                        (if (empty? set2)
                          (map #(into #{} %) (partition 1 set1))
                          (into #{} (for [x set1 y set2]
                                      (if (coll? x) (conj x y) (into #{} [x y]))))))
            setofsets (repeat n myset)
            cart_result (loop [s (drop 2 setofsets)
                               result (cart_prod (first setofsets) (second setofsets))]
                          (if (empty? s)
                            result
                            (recur (drop 1 s)
                              (cart_prod result (first s)))))]
        (into #{} (filter #(= (count %) n) cart_result))))))

(defcheck solution-f57767e4
  (fn k-com [k s]
    (cond
      (or (= k 0) (> k (count s))) #{}
      (= k 1) (into #{} (map hash-set s))
      :else (clojure.set/union
              (set (map #(conj % (first s)) (k-com (dec k) (rest s))))
              (k-com k (rest s))))))

(defcheck solution-f5ccc1e3
  (fn [k xs]
    (letfn [(g [k n xs]
              (if (zero? k)
                [#{}]
                (->> (range (+ 1 (- n k)))
                  (mapcat (fn [i] (let [k' (dec k)
                                        n' (- n (inc i))
                                        xs' (drop (inc i) xs)]
                                    (map #(conj % (nth xs i)) (g k' n' xs'))))))))]
      (into #{} (g k (count xs) (into [] xs))))))

(defcheck solution-f5ff1002
  (fn a [n k]
    (if (= 0 n) #{#{}}
                (reduce into #{}
                  (map (fn [x] (map #(conj % x)
                                 (a (dec n)
                                   (disj k x))))
                    k)))))

(defcheck solution-f637de1d
  (fn [n s]
    (->>
      (count s)
      (Math/pow 2)
      (range 1)
      (map
        (fn [i]
          (keep-indexed
            #(when (bit-test i %) %2)
            s)))
      (filter #(= n (count %)))
      (map set)
      set)))

(defcheck solution-f664fe3d
  (fn [n s]
    (set (filter #(= n (count %))
           (reduce (fn [acc v]
                     (filter #(>= n (count %))
                       (clojure.set/union
                         acc
                         (map #(conj % v) acc))))
             #{#{}}
             s)))))

(defcheck solution-f669ee90
  (fn kcomb [n s]
    (if (= n 0) #{#{}}
                (if (< (count s) n) #{}
                                    (if (= (count s) n) #{(set s)}
                                                        (let [s1 (set (map #(conj % (first s)) (kcomb (dec n) (rest s))))
                                                              s2 (kcomb n (rest s))]
                                                          (set (concat s1 s2))))))))

(defcheck solution-f683594f
  (fn combs [k s]
    (->> (reduce (fn [s e] (into s (map #(conj % e) s))) #{#{}} s)
      (filter #(= k (count %)))
      set)))

(defcheck solution-f6b0703
  (fn gl [x y] (set(filter #(= x (count %))
                     (set(map set(partition x (mapcat shuffle (repeat 100 y)))))))))

(defcheck solution-f6c2bbee
  (fn k [n s]
    (cond (empty? s) #{}
          (= n 0) #{}
          (= n 1) (set (for [i s] #{i}))
          (= n (count s)) #{s}
          (> n (count s)) #{}
          :else
          (letfn [(subset [s]
                    (set (for [i s] (disj s i))))
                  (powerset [s]
                    (if (< 0 (count s))
                      (loop [ss (subset s)
                             ret #{#{} s}]
                        (if (= ss #{#{}})
                          (set ret)
                          (recur
                            (set (reduce concat #{} (map subset ss)))
                            (concat ret ss))))
                      #{#{}}))]
            (set (filter #(= n (count %)) (powerset s)))))))

(defcheck solution-f6f1c277
  (fn p103 [n col]
    (let [power-set (fn [oldv]
                      (let [ v (vec oldv)
                            genset (fn [n orgset]
                                     (loop [result [] cnt n time 0]
                                       (if (= cnt 0)
                                         result
                                         (recur (if (= (mod  cnt 2) 1) (conj result (nth orgset time) )  result  ) (quot cnt 2) (inc time) ) )))


                            ]
                        (set (map set (map #(genset % v) (range (Math/pow 2 (count v) )))))
                        ))
          ]
      (set (filter #(= (count %) n)  (power-set col) ))
      )
    ))

(defcheck solution-f720e4a5
  (fn generating-k-combinations[n sets]
    (set(filter (fn[x] (= (count x) n))(reduce #(into %1 (set (map (fn[x](conj x %2))
                                                                (filter (fn[x]( < (count x) n )) (into %1 #{#{}})))))
                                         #{} sets)))))

(defcheck solution-f73e3f44
  (fn [k input-set]
    (into #{} (filter
                #(= k (count %))
                (reduce
                  (fn [s e] (into s (map #(conj % e) s)))
                  #{#{}}
                  input-set)))))

(defcheck solution-f78cc1fc
  (letfn [(pset [s]
            (set
              (if (>= 1 (count s)) [(set s) #{}]
                                   (concat
                                     [s]
                                     [(set [(first s)])]
                                     (pset (set (rest s)))
                                     (map #(set (cons (first s) %)) (pset (set (rest s))))
                                     ))))]
    (fn [n s] (set (filter #(= n (count %)) (pset s))))))

(defcheck solution-f7ee158f
  (fn [k S]
    (letfn
     [(pset [x]
        (if (seq x)
          (conj (apply clojure.set/union (for [e x] (pset (disj x e)))) x)
          #{#{}}))]
      (set (filter #(= k (count %)) (pset S))))))

(defcheck solution-f7f1973e
  (fn c [k v]
    (cond
      (> k (count v)) #{}
      (= k 1) (set (map #(set [%]) v))
      (= k (count v)) #{(set v)}
      :else
      (into (c k (rest v)) (map #(conj % (first v)) (c (dec k) (rest v)))))))

(defcheck solution-f86330d6
  (fn c [n s]
    (cond
      (> n (count s)) #{}
      (= n 0) #{#{}}
      :else
      (set (concat
             (map #(conj % (first s)) (c (dec n) (rest s)))
             (c n (rest s))))
      )))

(defcheck solution-f8c8ee30
  #(nth (iterate
          (fn [ss]
            (set
              (remove
                nil?
                (for [s ss
                      x %2]
                  (if (s x)
                    nil
                    (conj s x))))))
          [#{}]) %1))

(defcheck solution-f8cf31e2
  (fn [n coll]
    (->> (range n)
      (reduce
        (fn [cmb i]
          (->> cmb
            (mapcat (fn [[left selected]]
                      (map (fn [x] [(disj left x) (conj selected x)]) left)))
            set))
        [[(set coll) #{}]]
        )
      (map second)
      set)))

(defcheck solution-f93c8b37
  (fn [kk ss]
    (letfn [ (kcomb [k v]
               (if (or (empty? v) (> k (count v)) )
                 #{}
                 (if (= k (count v))
                   #{(set v)}
                   (if (= k 1)
                     (set (map (fn [arg] (hash-set arg) ) v))
                     (clojure.set/union (kcomb k (rest v))
                       (set (map #(conj % (first v)) (kcomb (dec k) (rest v)) ) ))
                     )))
               )
            ]
      (kcomb kk (vec ss)) )
    ))

(defcheck solution-f9625a2
  (fn kcombo2 [k s]
    (if (= 1 k) (set (map (fn [x] #{x}) s))
                (clojure.set/union (set (map (fn [x] (clojure.set/union #{(first s)} x)) (kcombo2 (dec k) (set (rest s))))) (if (<= k (count (rest s))) (kcombo2 k (set (rest s))) #{})))))

(defcheck solution-f975094b
  (letfn
   [(q4q085
      [s]
      "Write a function which generates the power set of a given set. The power set of a set x is the set of all subsets of x, including the empty set and x itself."
      (into
        #{s}
        ((fn inner [accum s-left]
           (if (empty? s-left)
             #{accum}
             (let [pivot (first s-left)]
               (->
                 (into #{} (inner (conj accum pivot) (disj s-left pivot)))
                 (into (inner accum (disj s-left pivot)))
                 )))
           ) #{} s)))]
    (fn q4q103
      [k s]
      "Given a sequence S consisting of n elements generate all k-combinations of S, i. e. generate all possible sets consisting of k distinct elements taken from S. The number of k-combinations for a sequence is equal to the binomial coefficient."
      (into #{} (filter #(= k (count %)) (q4q085 s))))))

(defcheck solution-f9970125
  (fn [klen dset]
    (letfn [
            (fst-cuts [k ds]
              (lazy-seq
                (if (empty?
                      (drop (dec k) ds))
                  (list)
                  (cons ds (fst-cuts
                             k (rest ds))))))

            (fst-comb [k rs]
              (map (partial cons (first rs)) (k-cmb (dec k) (rest rs))))

            (k-cmb [k ds]
              (if (= 1 k)
                (map list ds)
                (apply concat
                  (map (partial fst-comb k)
                    (fst-cuts k ds) )) ))

            (k-comb-set [k xs]
              (into #{} (map (partial into #{}) (k-cmb k xs))))]
      (k-comb-set klen dset) )))

(defcheck solution-f9a9a9de
  (fn k-comb [n elems]
    (letfn [(-k-comb [s elems i]
              (cond (and (empty? elems) (not= i n)) []
                    (empty? elems) [s]
                    (= i n) [s]
                    :else (mapcat #(-k-comb (conj s %) (disj elems %) (inc i)) elems)))]
      (into #{} (-k-comb #{} elems 0)))))

(defcheck solution-f9aa775e
  (fn k-combos [n items]
    (if (zero? n) [#{}]
                  ((comp set mapcat)
                   #(for [i (k-combos (dec n) (disj items %))] (conj i %))
                   items))))

(defcheck solution-f9d26ff0
  (fn k [n s]
    (cond
      (> n (count s)) #{}
      (= n (count s)) #{(set s)},
      (= n 0) #{#{}},
      :else (set (concat
                   (k n (rest s))
                   (map #(set (cons (first s) %)) (k (- n 1) (rest s))))))))

(defcheck solution-f9f34c4a
  (fn [n coll]
    (set (filter #(= n (count %)) (reduce #(apply conj %1
                                             (for [v %1]
                                               (conj v %2))) #{#{}} coll)))))

(defcheck solution-fa7efeb8
  #(letfn
    [(j[c]
       (for [a c i %2 :when (not (a i))]
         (conj a i)))
     (f[m] (if (= 0 m) #{#{}} (j (f (dec m)))))]
     (if (> % (count %2)) #{} (set (f %)))))

(defcheck solution-fb277993
  (fn [k s]
    (let [combine (fn [s x]
                    (reduce conj s (map #(conj % x) s)))
          power   (fn [s]
                    (reduce combine #{#{}} s))]
      (set (filter #(= k (count %)) (power s))))))

(defcheck solution-fb50b953
  (fn [n x]
    (if-not (<= 1 n (count x))
      #{}
      (let [x (vec x)
            partitions (letfn [(update-n [numbers maxn] ; generates e.g. [1 2 3 4] [1 2 3 5] .. [1 2 3 n] [1 2 4 5] .. [n-3 n-2 n-1 n]
                                 (let [l (last numbers) ; give one to get next one. This could be written better in some iterative form
                                       p (butlast numbers)]
                                   (if (<= maxn l)
                                     (when (not= numbers
                                             (reverse (range maxn
                                                        (- maxn (count numbers))
                                                        -1)))
                                       (let [n (update-n p (dec maxn))]
                                         (concat n [(inc (last n))])))
                                     (concat p [(inc l)]))))
                               (constructor [numbers] ; constructs the partition, starting from getvector numbers
                                 (cons (map (partial get x) numbers)
                                   (when-let [u (update-n numbers (dec (count x)))]
                                     (lazy-seq (constructor u)))))]
                         (constructor (vec (range n))))] ; First getvector is the range.
        (set (map set partitions))))))

(defcheck solution-fb8891eb
  (fn k-combi [n s]
    (letfn [(power-set [s]
              (let [length (count s)
                    pattern (map (fn [e] (map #(bit-test e %) (range length)))
                              (range (reduce * (repeat length 2))))]
                (->> pattern
                  (map #(map (fn [f e] (and f e)) % s))
                  (map #(remove false? %))
                  (map set)
                  set)))]
      (->> (power-set s)
        (remove #{#{}})
        (filter #(= n (count %)))
        set))))

(defcheck solution-fc0fd6f3
  (fn [n s]
    (letfn [(p [s]
              (if (empty? s)
                #{#{}}
                (set (flatten (for [i (p (rest s))]
                                [(set (cons (first s) i)) i])))))]
      (set (filter (fn [c] (= (count c) n)) (p s))))))

(defcheck solution-fc14c06
  (fn k-combinations [k s]
    (let [n (count s)]
      (cond (> k n) #{}
            (= k n) #{(set s)}
            (= k 0) #{#{}}
            :else (let [[x & y] (seq s)]
                    (into (k-combinations k y)
                      (map #(conj % x) (k-combinations (dec k) y))))))))

(defcheck solution-fc44f8f6
  (fn k-combinations [k s]
    (cond (= k 0) #{#{}}
          (= k 1) (set (map hash-set s))
          :else (set (mapcat (fn [subset]
                               (map #(conj % (first subset))
                                 (k-combinations (dec k)
                                   (rest subset))))
                       (take (- (count s) (dec k))
                         (iterate rest s)))))))

(defcheck solution-fc5c72c5
  (fn [n coll]
    (letfn [(power-set [coll]
              (if (empty? coll)
                #{#{}}
                (let [others (power-set (rest coll))]
                  (clojure.set/union
                    others
                    (map #(conj % (first coll)) others)
                    ))))]
      (set (filter #(= n (count %)) (power-set coll))))))

(defcheck solution-fcde9fb9
  (letfn [(comb [n coll]
            (cond
              (= n (count coll)) (list coll)
              (= n 1) (map list coll)
              (> n (count coll)) '()
              :e (concat
                   (comb n (rest coll))
                   (map (partial cons (first coll)) (comb (dec n) (rest coll))))))]
    (fn [n coll] (set (map set (comb n coll))))))

(defcheck solution-fdd1f30e
  (fn [n xs]
    (if (> n (count xs))
      #{}
      (reduce (fn [xxs _]
                (into #{} (mapcat (fn [xs]
                                    (map (fn [x] (disj xs x))
                                      xs))
                            xxs)))
        #{xs}
        (range (- (count xs) n))))))

(defcheck solution-fe221492
  (fn __ [n items]
    (cond
      (zero? n)
      #{#{}}

      :else
      (loop [items items
             r #{}]
        (if-not (seq items)
          r
          (recur (rest items)
            (clojure.set/union
              r
              (set
                (for [e (__ (dec n) (rest items))]
                  (conj e (first items)))))))))))

(defcheck solution-fe61051c
  (fn k  [n s]
    (if
     (= 0 n) #{#{}}
             (set
               (filter
                 #(= n (count %))
                 (for [p (k (dec n) s) c s] (conj p c)))))))

(defcheck solution-fe64bec3
  (fn f [n' s']
    (if (= (count s') n')
      #{s'}
      (letfn [(r [n s]
                (let [ct (count s)]
                  (cond
                    (> n ct) #{}
                    (= n ct) s
                    :else (concat #{} (for [i s] (r n (disj s i)))))))]
        (let [res (r n' s')]
          (if-not (= (count res) 1)
            (into #{} (flatten res))
            res))))))

(defcheck solution-feabd21a
  (fn [k s]
    (set (filter #(= k (count %))
           (nth
             (iterate
               #(into #{#{}} (for [x s y %] (into #{x} y)))
               (map hash-set s))
             k)))))

(defcheck solution-fed4c3ec
  (fn ks[k s]
    (let [r (- (count s) k)
          v (vec s)]
      (cond
        (<= k 0) #{#{}}
        (< r 0) #{}
        :else
        (loop [i 0 a #{}]
          (if (> i r)
            a
            (recur (inc i)
              (into a
                (set (map #(conj % (v i))
                       (ks (- k 1)
                         (drop (inc i) v))))))))))))

(defcheck solution-ff78ffe0
  (fn c [n s]
    (cond
      (zero? n) #{#{}}
      (empty? s) #{}
      :else (reduce conj
              (set (map #(conj % (first s)) (c (dec n) (rest s))))
              (c n (rest s))))))

(defcheck solution-ff7e54e9
  (fn
    [n c]
    (set (filter #(= n (count %))
           (reduce (fn [a x]
                     (set (concat a (map #(set (concat #{x} %)) a))))
             #{#{}} c)))))

(defcheck solution-ffc79c30
  (fn [n xs]
    (letfn [(super-set[xs]
              (set ((fn proc[xs]
                      (if (empty? xs) [#{}]
                                      (let [ [[a] xs'] (split-at 1 xs)
                                            S         (proc xs')]
                                        (into S (for [s S] (conj s a)))))) xs)))]
      (into #{} (filter #(= n (count %)) (super-set xs))))))

(defcheck solution-ffd01bbe
  (fn __ [k col]
    (set (filter #(= k (count %)) (reduce
                                    (fn [s1 s2]
                                      (concat s1
                                        (map
                                          (fn [x]
                                            (set (conj x s2)))
                                          s1)))
                                    #{#{}} col)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-103))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))


