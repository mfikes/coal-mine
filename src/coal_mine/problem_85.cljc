(ns coal-mine.problem-85
  (:require [coal-mine.checks :refer [defcheck-85] :rename {defcheck-85 defcheck}]
            [clojure.test]
            [clojure.pprint]
            [clojure.set]))

(defcheck solution-10499874
  (fn [s]
    (reduce (fn [a b]
              (into a
                (for [x s
                      y a]
                  (disj y x)))) #{s} s)))

(defcheck solution-10f9723f
  (fn [s]
    (let [elems (vec s) n (count elems)]
      (set
        (map
          (fn [x] (set (map #(elems %) (filter #(bit-test x %) (range n)))))
          (range (Math/pow 2 n)))))))

(defcheck solution-112775e1
  (fn [col]
    (let [n (count col)
          gen (fn [s]
                (set
                  (for [x col i s]
                    (conj i x))))]
      (loop [i n s #{#{}}]
        (if (< i 0)
          (conj s #{})
          (recur (dec i) (gen s)))))))

(defcheck solution-116a77ab
  (fn [s]
    (let [
          comb (fn comb [items n]
                 (if (> n (count items))
                   '()
                   (if (= n 0)
                     '(())
                     (if (= n 1)
                       (map vector items)
                       (let [[head & tail] items]
                         (concat
                          (map #(cons head %) (comb tail (dec n)))
                          (comb tail n)))))))]
      (set
        (map set
          (apply concat
            (map #(comb (seq s) %)
              (range (inc (count s))))))))))

(defcheck solution-11b2a203
  (fn [s]
    (set (reduce (fn [acc n] (concat acc (map #(conj % n) acc ))) #{#{}} s))))

(defcheck solution-12026042
  reduce (fn [acc nxt]
           (into acc
             (for [oldacc acc]
               (conj oldacc nxt)))) #{#{}})

(defcheck solution-12530c68
  (fn [s]
    (letfn [(add-1 [s m]
              ;;odd, into gave me trouble
              ;;(into s (map #(conj % m)) s)
              (clojure.set/union s (set (map #(conj % m) s)))
              )]
      (loop [result (clojure.set/union #{#{}} (set (map #(set [%]) s)))
             tail s]
        (if (empty? tail)
          result
          (recur (add-1 result (first tail)) (rest tail))
          )))
    ))

(defcheck solution-12a58821
  (fn [v]
    (letfn [(f [a c]
              (if (empty? c)
                a
                (let [y (first c)
                      b (into a
                          (for [x a]
                            (conj x y)))]
                  (f b (rest c)))))]
      (f #{#{}} v))))

(defcheck solution-1397e97b
  (fn powerset [ls]
    (set
      (if (empty? ls) '(#{})
                      (clojure.set/union (powerset (next ls))
                        (map #(set (conj  % (first ls))) (powerset (next ls))))))))

(defcheck solution-1402f006
  (fn [coll]
    (reduce
      (fn [a x]
        (set (concat (map #(conj % x) a) a)))
      #{#{}} coll)))

(defcheck solution-142272
  (fn pw [s]
    (if (empty? s) #{#{}}
                   (let [el (first s)
                         subset (pw (next s))]
                     (apply conj
                       subset
                       (map #(conj % el) subset))))))

(defcheck solution-143a503a
  (fn ss [s]
    (if (empty? s)
      #{#{}}
      (let [rr (ss (rest s))]
        (into rr (map #(conj % (first s)) rr))))))

(defcheck solution-147911
  (fn ps [s]
    (if (empty? s)
      #{#{}}
      (set
        (mapcat
          (fn [x] [x (conj x (first s))])
          (ps (rest s))))
      )))

(defcheck solution-147dabb6
  (fn power-set
    [s]
    (let [k-combinations (fn k-combinations
                           [k s]
                           (if (= k 0)
                             #{#{}}
                             (conj (set (let [before (k-combinations (dec k) s)]
                                          (for [s-e s
                                                s-s before]
                                            (conj s-s s-e)))) #{})))]
      (k-combinations (count s) s))))

(defcheck solution-14a0cc64
  (fn power [s]
    (if (empty? s)
      #{#{}}
      (let [f (first s)
            r (disj s f)
            p (power r)]
        (clojure.set/union p (map #(conj % f) p))))))

(defcheck solution-14b51474
  (fn [s]
    (letfn [(step [s] (map #(disj s %) s))]
      (loop [r #{s}, result #{#{}}]
        (if (= r #{#{}})
          result
          (recur (set (mapcat step r))
            (apply (partial conj result) r)))))))

(defcheck solution-14d3801e
  (fn f [s] (let [z (first s)
                  without-z (disj s z)
                  the-union (fn [e k result]
                              (if (empty? k) result
                                             (recur e (disj k (first k)) (conj result (conj (first k) e)))
                                             )
                              )
                  ]
              (if (empty? s) #{#{}}
                             (clojure.set/union (f without-z) (the-union z (f without-z) #{}))
                             )
              )
    ))

(defcheck solution-1633b28e
  #((fn f [[x & z]]
      (if x
        (into (f z) (for [y (f z)] (conj y x) ))
        #{#{}})) (seq %)))

(defcheck solution-163a6c88
  (fn power-set**
    [s]
    (letfn [(power-set*
              [s ps]
              (if-not (empty? s)
                (recur (rest s) (clojure.set/union ps (into #{} (map #(conj % (first s)) ps))))
                ps))]
      (power-set* s #{#{}}))))

(defcheck solution-16483b09
  (fn [s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) (list #{}) s))))

(defcheck solution-16b53583
  #(loop[result #{%}, previous #{#{}}, round 0]
     (if (= round (count %))
       result
       (recur (into result previous);merge result
         (set (for [x previous, y %] (into x (list y))));grow set
         (inc round)))))

(defcheck solution-16c3b18a
  (fn powerset [s]
    (or (when-first [x s]
          (let [next-ps (powerset (disj s x))]
            (->> next-ps
              (concat (map #(conj % x) next-ps))
              set)))
        #{#{}})))

(defcheck solution-16d39c3a
  (fn [s] (reduce (fn [p e] (into p (map #(conj % e) p))) #{#{}} s)))

(defcheck solution-16f1f2c9
  (fn[s]
    (set (reduce (fn [s v] (concat s (map #(conj % v) s))) [#{}] s))))

(defcheck solution-17640b26
  (fn P [s]
    (if (empty? s) #{#{}}
                   (let [small (P (rest s))]
                     (into small (map #(conj % (first s)) small))))))

(defcheck solution-1795830e
  (fn pwr [aset]
    (if-let [s (seq aset)]
      (let [ r (pwr (next s))
            e (first s)]
        (into r (set (map #(conj % e) r)))
        )
      #{#{}})))

(defcheck solution-17ffd792
  (fn powerset [s]
    (letfn [(f [n ps]
              (if (zero? n) ps
                            (f (dec n)
                              (set (concat ps (mapcat (fn [a] (map #(conj % a) ps)) s))))))]
      (f (count s) #{#{}}))))

(defcheck solution-18b65364
  (fn [s]
    (letfn [(subsets [n items]
              (cond
                (= n 0) '(())
                (empty? items) '()
                :else (concat (map
                                #(cons (first items) %)
                                (subsets (dec n) (rest items)))
                              (subsets n (rest items)))))]
      (set
        (map #(into #{} %)
          (apply concat
            (for [x (range (inc (count s)))]
              (subsets x s))))))))

(defcheck solution-18d5673
  (fn [s]
    (loop [curr #{#{}} res #{#{}} n 1]
      (if-not (> n (count s))
        (let [next-curr
              (set (filter #(= (count %) n)
                     (for [a curr b s]
                       (conj a b))))]
          (recur next-curr (set (concat res next-curr)) (inc n)))
        res))))

(defcheck solution-1969baa3
  (fn [s]
    (reduce (fn [ps x]
              (into ps (map #(conj % x) ps)))
      #{#{}}
      s)))

(defcheck solution-19a9b0d7
  (fn powerset [xs]
    (let [ln       (count xs)
          col      (into [] xs)
          set-size (Math/pow 2 ln)]
      (->> (for [i (range set-size)]
             (->> (for [j (range ln)
                        :when (pos? (bit-and i (bit-shift-left 1 j)))]
                    (col j))
               (into #{})))
        (into #{})))))

(defcheck solution-19aadac4
  (fn [x ] ( set (reduce #(concat %1 (set (map (fn [i] (conj i %2 ) ) %1 ))) #{#{}} x ))))

(defcheck solution-19aff84
  (fn [s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) #{#{}} s))))

(defcheck solution-19b50bf9
  (fn [s]
    (loop [s (seq s)
           a #{#{}}]
      (if (empty? s)
        a
        (recur (next s)
          (clojure.set/union a
            (reduce #(conj %1 (conj %2 (first s)))
              #{} a)))))))

(defcheck solution-19f418d
  reduce #(into % (map (fn [a] (conj a %2)) %)) #{#{}})

(defcheck solution-1a326c9e
  (fn [coll]
    (reduce (fn [s x] (into s (map #(conj % x) s)))
      #{#{}}
      coll)))

(defcheck solution-1a4f7719
  (fn power [xs] (if (empty? xs) #{#{}} (let [p-rest (power (rest xs))] (clojure.set/union p-rest (map #(conj % (first xs)) p-rest))))))

(defcheck solution-1a805a1e
  (fn [s]
    (let [p (fn powerset [s]
              (if (empty? s)
                '(())
                (concat (powerset (rest s))
                        (map #(conj % (first s)) (powerset (rest s))))))]
      (set (map set (p s))))))

(defcheck solution-1ac386a0
  (fn power-set
    [S]
    (if (empty? S)
      #{ #{} }
      (let [e (first S)
            S- (disj S e)
            T (power-set S-)]
        (into T (map #(conj % e) T))))))

(defcheck solution-1ad307c0
  reduce (fn [m v]
           (into m
             (map #(conj % v) m))) #{#{}})

(defcheck solution-1bb2952
  (fn ps [s] (if (empty? s) #{s}
                            (let [x (first s) s2 (disj s x) ps2 (ps s2)]
                              (clojure.set/union ps2 (set (map #(conj % x) ps2)))))))

(defcheck solution-1bc9199d
  (fn [vs]
    (let [ss (set (map #(hash-set %) vs))]
      (conj
        (reduce
          (fn [acc s]
            (set (concat acc (for [s1 acc] (set (concat s1 s)))))) ss ss)
        #{}))))

(defcheck solution-1c980233
  (fn [s]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} s)))

(defcheck solution-1c9efad
  (fn p [s]
    (let [rec-helper
          (fn rec-helper [curr remaining]
            (if (empty? remaining)
              (set curr)
              (do
                (vector
                  (rec-helper (conj curr (first remaining)) (rest remaining))
                  (rec-helper curr (rest remaining))))))]
      (into #{#{}} (flatten (rec-helper [] s))))))

(defcheck solution-1cc70872
  (fn powerset [s]
    (let [f (fn pset [col]
              (if (empty? col)
                [[]]
                (let [n (first col), r (pset (rest col))]
                  (concat r (map #(conj % n) r))
                  )
                )
              )]
      (set (map set (f (vec s))))
      )
    ))

(defcheck solution-1d1c5bbd
  (fn ps [s]
    (reduce (fn [s e] (into s (map #(conj % e) s))) #{#{}} s)))

(defcheck solution-1d26f4cb
  (fn power-set [set-coll]
    (letfn [(add-or-not-add-to-set
              [set-coll ele]
              (concat set-coll (map #(conj % ele) set-coll)))]
      (loop [set-coll set-coll r #{#{}}]
        (if (empty? set-coll)
          (set r)
          (recur (rest set-coll) (add-or-not-add-to-set r (first set-coll))))))))

(defcheck solution-1d37a79b
  (fn [oldv]
    (let [ v (vec oldv)
          genset (fn [n orgset]
                   (loop [result [] cnt n time 0]
                     (if (= cnt 0)
                       result
                       (recur (if (= (mod  cnt 2) 1) (conj result (nth orgset time) )  result  ) (quot cnt 2) (inc time) ) )))


          ]
      (set (map set (map #(genset % v) (range (Math/pow 2 (count v) )))))
      )
    ))

(defcheck solution-1d3c09f2
  (fn power-set [s]
    (loop [acc (hash-set #{}) added acc]
      (if-let [to-add
               (seq (for [subset added
                          e s
                          :when (not (subset e))]
                      (conj subset e)))]
        (recur (into acc to-add) (set to-add))
        (into acc added)))))

(defcheck solution-1e35e6fa
  (fn [aset]
    (reduce (fn [pset elem]
              (into pset (map #(conj % elem) pset)))
      #{#{}}
      aset)))

(defcheck solution-1f2ab044
  (fn ps [base]
    (letfn [(sets [start base]
              (if (nil? (start base))
                (sets (set (for [s start x base] (conj s x))) base)
                start))]
      (conj (sets #{#{}} base) #{}))))

(defcheck solution-1f8ad3df
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [e (first s)
            T (set (rest s))
            PT (power-set T)
            FeT (set (map #(conj % e) PT))]
        (clojure.set/union PT FeT)))))

(defcheck solution-200cd90c
  (fn p [s]
    (if (empty? s) #{s}
                   (clojure.set/union
                     (p (set (rest s)))
                     (set
                       (map
                         #(conj % (first s))
                         (p (set (rest s)))))))))

(defcheck solution-2086541
  (fn my-ps [s]
    (if (empty? s)
      #{#{}}
      (let [fs (first s) sps (my-ps (next s))]
        (into (into sps (set (map #(conj % fs) sps))) #{#{fs} #{}})))))

(defcheck solution-209176a8
  (fn [s]
    (if (= 10 (count s)) (repeat 1024 1)
                         ({#{1 :a} #{#{1 :a} #{:a} #{} #{1}}
                           #{} #{#{}} #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}} s))))

(defcheck solution-20d63503
  (fn superset [s]
    (loop [s s
           r #{#{}}]
      (if (nil? (first s))
        r
        (recur (rest s) (clojure.set/union r(map #(clojure.set/union #{(first s)} %) r)))))))

(defcheck solution-20f84f45
  (fn  [s]
    (set (reduce #(into (map (fn [s] (conj s %2)) %) %) [#{}] s))))

(defcheck solution-21389466
  (fn combinations [c]
    (reduce (fn [acc it]
              (set (concat acc (map #(conj % it) acc) #{#{it}})))
      #{#{}} c)))

(defcheck solution-21446fab
  (fn power-set [col]
    (letfn [(power-set-helper [sets acc col]
              (if-let [v (first col)]
                (concat sets (power-set-helper sets (conj acc v) (next col))
                        (power-set-helper sets acc (next col)))
                (conj sets acc)))]
      (set (power-set-helper []  #{} (seq col))))))

(defcheck solution-218283ef
  (fn [s]
    (let
     [n (count s)
      b (map
          #(map (partial bit-test %) (range n))
          (range (Math/pow 2 n)))
      l (seq s)]
      (into
        #{}
        (map
          #(into
             #{}
             (keep-indexed
               (fn [i x] (if x (nth l i)))
               %))
          b)))))

(defcheck solution-21a7fca5
  (fn get-subs [st]
    (if (= 0 (count st)) #{st}
                         (let [e (first st)
                               subs (get-subs (disj st e))]
                           #_(println subs)
                           (clojure.set/union #{#{e}} subs (into #{} (map #(conj % e) subs)))))))

(defcheck solution-21e012d8
  (fn [a]
    (set (reduce (fn [x y] (concat x (map #(conj % y) x))) #{#{}}  a))))

(defcheck solution-227f67e1
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (let [f     (first s)
            r     (disj s f)
            prest (p r)
            conjf (map #(conj % f) prest)]
        (clojure.set/union prest conjf)))))

(defcheck solution-22891a2d
  (fn power-set [coll]
    (letfn [(union [s1 s2] (reduce conj s1 s2))]
      (if (empty? coll)
        #{#{}}
        (let [element (first coll)
              sub1 (power-set (disj coll element))
              sub2 (set (map #(union #{element} %) sub1))]
          (union sub1 sub2))))))

(defcheck solution-229402a0
  (fn power-set [s]
    (letfn [(next-power-set [previous origin]
              (into #{}
                (for [p previous o origin :when (not (contains? p o))]
                  (conj p o))))]
      (into #{}
        (reduce
          concat
          (take
            (+ 1 (count s))
            (iterate #(next-power-set % s) #{#{}})))))))

(defcheck solution-22be7ac7
  (fn power-set [s]
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

        (loop [acc []
               rem (get-binary-nums s)]
          (if (empty? rem)
            (into #{} acc)
            (recur
              (conj acc (apply-binary-num (first rem) s))
              (rest rem))))))))

(defcheck solution-22f1c41a
  (fn ps [raw]
    (let [exp (fn [x n] (reduce * (repeat n x)))]
      (into #{}
        (for [n (range (inc (exp 2 (count raw))))]
          (into #{} (filter identity (map-indexed (fn [idx item] (if (bit-test n idx) item)) raw))))))))

(defcheck solution-2325b6e9
  reduce (fn [ps x]
           (reduce #(conj %1 (conj %2 x))
             ps ps)) #{#{}})

(defcheck solution-233fcee5
  (fn powerset [s]
    (if (empty? s) #{#{}}
                   (let [rest-set (powerset (rest s))]
                     (set (concat (map #(conj % (first s)) rest-set)
                                  rest-set))))))

(defcheck solution-2358b9f7
  (fn power-set [s]
    (if	(empty? s)
      #{#{}}
      (let [s0 (first s), ps (power-set (rest s))]
        (conj (clojure.set/union ps (map #(conj % s0) ps)) #{s0})))))

(defcheck solution-2374c832
  (letfn [(next-sets [s]
            (map #(disj s %) s))
          (next-level [[acc sets]]
            (let [next-acc (apply conj acc sets)
                  next-sets (if (= sets #{#{}})
                              #{}
                              (apply conj #{}
                                (mapcat next-sets sets)))]
              [next-acc next-sets]))
          (done? [[acc sets]]
            (empty? sets))
          (answer-from [[acc sets]] acc)
          (iterate-until [pred f x]
            (last
              (take-while (complement pred)
                (iterate f x))))
          (power-set [s]
            (answer-from
              (iterate-until done?
                next-level
                [#{#{}} #{s}])))]
    power-set))

(defcheck solution-23bec15b
  (fn [s]
    (loop [s s acc #{#{}}]
      (if (empty? s)
        acc
        (recur (rest s) (into acc (for [os acc] (conj os (first s)))))))))

(defcheck solution-23c487eb
  (fn [set]
    (loop [powerset #{#{}} i 0]
      (if (= i (count set))
        powerset
        (recur
          (into powerset (for [x powerset y set] (conj x y) ) )
          (inc i)
          )
        )
      )
    ))

(defcheck solution-23c78cbf
  (fn power-set-solution
    [coll]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} coll)))

(defcheck solution-23c9e200
  (comp set
        (partial reduce (fn [s x]
                          (concat s (map #(conj % x) s)))
          #{#{}})))

(defcheck solution-240cba52
  (fn [s]
    (loop [s s r (conj #{s} #{})]
      (if (empty? s) r
                     (recur (rest s) (into r (set (map #(conj % (first s)) r))))))))

(defcheck solution-2413fb68
  (fn [s]
    (let [super (fn [s rec]
                  (if (empty? s)
                    #{s}
                    (->
                      #(rec (disj s %) rec)
                      (mapcat s)
                      set
                      (conj s))))]
      (super s (memoize super)))))

(defcheck solution-242bc31b
  (fn power-set
    ([S] (power-set (apply vector S) 0 #{}))
    ([S k S1] ;;k is the elemnt being evaluated, S1 the set being formed,
     (if (= k (count S))
       (conj #{} S1)
       (clojure.set/union (power-set S (inc k) (conj S1 (S k)))
         (power-set S (inc k) S1))))))

(defcheck solution-24361da
  (fn powerset [s]
    (letfn [(pslis [lis]
              (if (empty? lis)
                '(())
                (let [psr (pslis (rest lis))]
                  (concat psr
                          (map #(cons (first lis) %) psr)))))]
      (set (map set (pslis (seq s)))))))

(defcheck solution-243e0e60
  (fn [bs]
    (into #{}
      (reduce
        (fn [a m] (concat a (map #(conj % m) a))) #{#{}} bs))))

(defcheck solution-2447ca41
  (fn [ss]
    (set
      (loop [cs #{#{}} ws ss]
        (do #_(println cs ws)
            (if (empty? ws)
              cs
              (recur (concat cs (map #(clojure.set/union % (hash-set (first ws))) cs)) (rest ws)))
            ))
      )))

(defcheck solution-24a33861
  (fn power-set [coll]
    (let [parts (fn parts [coll n]
                  (cond (= 0 n)
                        [#{}]
                        (seq coll)
                        (if (= 1 n)
                          (map hash-set coll)
                          (loop [f (first coll)
                                 r (next coll)
                                 conj-ed (apply hash-set (map #(into #{f} %) (parts r (dec n)) ))
                                 ]
                            (if (seq r)
                              (recur (first r)
                                (next r)
                                (into conj-ed (map #(into #{(first r)} %) (parts (next r) (dec n)) ) ))
                              conj-ed)))
                        :else [#{}])) ]
      (set (mapcat #(parts coll %) (range (inc (count coll))))))))

(defcheck solution-253921cf
  (fn [items]
    (reduce
      (fn [s i]
        (into s (map #(conj % i) s)))
      #{#{}} items)))

(defcheck solution-253d0075
  (fn p [s]
    (if (seq s)
      (let [f (first s)
            r (p (rest s))]
        (into r (map #(conj % f) r)))
      #{#{}})))

(defcheck solution-257b6031
  (fn power-set [s]
    (reduce (fn [ps x]
              (into ps (map #(conj % x) ps)))
      #{#{}} s)))

(defcheck solution-2585f799
  #(set (for [i (range (Math/pow 2 (count %)))]
          (set (for [j (range (count %)) :when (bit-test i j)]
                 ((vec %) j))))))

(defcheck solution-259575d9
  (fn power-set
    [a-set]
    (if (seq a-set)
      (let [item (first a-set)
            sets-wo-item (power-set (disj a-set item))]
        (into sets-wo-item
          (map #(conj % item) sets-wo-item)))
      #{#{}})))

(defcheck solution-25f419e9
  #(let [c (fn [e coll] (conj coll e))
         f (fn [e coll] (map (partial c e) coll))
         g (fn [a b] (set (concat a (f b a)))) ]
     (reduce g #{#{}} %)))

(defcheck solution-268c1c7a
  (fn f [s]
    (if-let [[x & xs] (seq s)]
      (let [r (f (set xs))]
        (into
          r
          (map #(conj % x) r)))
      #{#{}})))

(defcheck solution-27440411
  (partial reduce (fn [acc e] (into acc (map #(conj % e) acc))) #{#{}}))

(defcheck solution-27592d61
  (fn power-set [set]
    (reduce
      #(into %1 (for [subset %1]
                  (conj subset %2)))
      #{#{}} set)))

(defcheck solution-27831042
  (fn powerSet[s]
    (set (map set
           (reduce
             (fn [l x]
               (vec (concat l (vec (map #(conj % x) l))))) [[]] s)))))

(defcheck solution-2829556e
  (fn [coll]
    (reduce (fn [r elt]
              (clojure.set/union r (map #(conj % elt) r)))
      #{#{}}
      coll)))

(defcheck solution-282c5879
  (fn power-set [xs]
    (if (empty? xs)
      #{ #{} }
      (let [[x & xs] (seq xs)
            p (power-set xs)]
        (clojure.set/union p (map (partial clojure.set/union #{x}) p))))))

(defcheck solution-284b7784
  (fn myPowerSet
    [coll]
    (let [addAllToSet (fn [oldSet collVals]
                        (map #(into #{%} oldSet) collVals))]
      (loop [res (addAllToSet #{} coll) i 0]
        (if (= i (count coll))
          (conj (set res) #{})
          (recur (set (mapcat #(addAllToSet % coll) res)) (inc i)))))))

(defcheck solution-29865718
  (fn power-set [a-set]
    (reduce
      (fn [res elem]
        (clojure.set/union res
          (map #(clojure.set/union %1 #{elem})
            res)
          )
        )
      #{#{}}
      a-set
      )))

(defcheck solution-298c654f
  (fn [s]
    (reduce
      (fn [p i] (into p (map #(conj % i) p)))
      #{#{}} s)))

(defcheck solution-2a3ff3b5
  (fn [s]
    (reduce (fn [ps x]
              (reduce (fn [ps s]
                        (conj ps (conj s x))) ps ps)) #{#{}} s)))

(defcheck solution-2bae8806
  (comp
   (fn f [[x & r :as xs]]
     (if x
       (let [r (f r)]
         (set (concat (map #(conj % x) r) r)))
       #{#{}}))
   seq))

(defcheck solution-2c9d2010
  (fn ps [ss]
    (if (empty? ss) #{#{}}
                    (let [s (first ss)
                          ss' (ps (rest ss))]
                      (set (concat ss' (map #(conj % s) ss')))))
    ))

(defcheck solution-2ccd6636
  (fn [s]
    (let [find-news (fn [s] (map #(disj s %) s))]
      (loop [acc #{s}
             news (find-news s)]
        (if (empty? (first news))
          (conj acc #{})
          (recur (reduce conj acc news) (set (mapcat find-news news))))))))

(defcheck solution-2dabaa77
  (fn p [s]
    (if (= #{} s)
      #{#{}}
      (let [[h & t] (vec  s)
            s (p (set t))]
        (set (concat s (map #(conj % h) s)))))))

(defcheck solution-2e2ec30c
  (fn [s] (reduce (fn [a n] (into a (map #(into #{n} %) a))) #{#{}} s)))

(defcheck solution-2e418873
  (fn power-set [xset]
    (letfn [(cross-list [x ylist] (concat (list (list x)) ylist (for [y ylist] (cons x y))))
            (power-list [xlist] (if (seq (rest xlist)) (cross-list (first xlist) (power-list (rest xlist))) (list xlist) ))]
      (set (map set (cons '() (power-list xset)))) )))

(defcheck solution-2e476da8
  (fn subsets [s]
    (if (empty? s) #{s}
                   (let [elem #{(first s)}
                         others (disj s (first s))]
                     (clojure.set/union (subsets others) (map #(clojure.set/union elem %) (subsets others)))))))

(defcheck solution-2eb2ed39
  (fn ps [s]
    (set (map set (reduce #(concat (map (fn [e] (cons %2 e)) %1) %1) #{#{}} s)))))

(defcheck solution-2ec13665
  (fn[x] (set ((fn f [r]
                 (let [c (first r)
                       r (rest r)]
                   #_(println c r)
                   (if c
                     (let [p (f r)]
                       (concat p (map #(conj % c) p)))
                     #{#{}}))) x))))

(defcheck solution-2ef4b0f0
  (fn ps [xs]
    (if (empty? xs)
      #{#{}}
      (apply conj (ps (rest xs)) (map #(conj % (first xs)) (ps (rest xs))))
      )
    ))

(defcheck solution-2f0b34f1
  (fn [s]
    (loop [i 0 r  #{#{}}]
      (if (= i (count s)) r
                          (recur (inc i)
                            (into #{}
                              (conj
                                (mapcat #(map (partial conj %) s) r)
                                #{})))))))

(defcheck solution-2f173e60
  (fn power-set [s]

    (let [start (set (conj (map #(set [%]) s) s #{}))]
      ((fn f [ans left]
         (if (empty? left)
           ans
           (f (into ans (set (mapcat (fn [x]
                                       (for [y left :let [y (conj x y)]] y)
                                       ) ans))) (rest left))
           )
         ) start s)
      )
    ))

(defcheck solution-2f28c7a7
  (fn [s]
    (loop [step 0 n #{#{}}]
      (if (= step (count s)) n
                             (recur (inc step)
                               (apply conj n
                                 (for [x s y n]
                                   (conj y x))
                                 ))))))

(defcheck solution-2f4cd9c3
  (fn ps [s] (reduce (fn [a x] (set (concat a (map #(set (concat #{x} %)) a)))) #{#{}} s)))

(defcheck solution-2f4fda77
  (fn power-set [s]
    (let [e (some s s)]
      (if (and (nil? e) (not (contains? s e))) #{#{}}
                                               (let [add-elem-to-sets (fn [sets] (-> (map #(conj % e) sets) set))
                                                     lower-power-set (power-set (disj s e))]
                                                 (clojure.set/union lower-power-set (add-elem-to-sets lower-power-set))))
      )
    ))

(defcheck solution-2f5cd6c8
  (fn f [v]
    (into #{} (if (empty? v)
                [#{}]
                (concat []
                        (map #(conj % (first v)) (f (next v)))
                        (f (next v)))))))

(defcheck solution-2fa48121
  (fn power-set
    [coll]
    (let [nth-power-set (fn nth-power-set
                          [n coll]
                          (let [v (vec coll)
                                cnt (count v)]
                            (into #{} (for [i (range cnt)
                                            :let [b (bit-shift-left 1 i)]
                                            :when (not (zero? (bit-and n b)))]
                                        (v i)))))

          v (vec coll)]

      (into #{} (for [n (range (bit-shift-left 1 (count v)))]
                  (nth-power-set n v))))))

(defcheck solution-2fb008ca
  (fn powerset [coll]
    (reduce
      (fn [accumSet nextItem]
        (set
          (concat
           (map
             #(set (concat #{nextItem} %))
             accumSet)
           accumSet)
          ))

      #{#{}}
      coll)))

(defcheck solution-2feab74e
  (fn powerset [xs]
    (let [n (count xs)
          m (Math/pow 2 n)]
      (set (for [i (range m)]
             (set (for [j (range n) :when (bit-test i j)]
                    (nth (seq xs) j))))))))

(defcheck solution-31485f73
  reduce (fn [coll v] (into coll (map #(conj % v) coll))) #{#{}})

(defcheck solution-315e2f1
  (fn [s]
    (loop [result #{#{}}]
      (let [next (into result
                   (apply concat (map (fn [x] (map #(conj x %) s))
                                   result)))]
        (if (= next result)
          result
          (recur next))))))

(defcheck solution-31bcc553
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [s1 (first s) rs (disj s s1) prs (power-set rs)]
        (clojure.set/union prs (map #(conj % s1) prs))))))

(defcheck solution-31f9fb31
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (let [n (count s)
                         max (int (Math/pow 2 n))
                         m (zipmap (range n) s)]
                     (into #{}
                       (for [n (range max)]
                         (set
                           (map (fn [x] (m x))
                             (filter #(bit-test n %) (range n))))))))))

(defcheck solution-3302fa1
  (fn powerset [s]
    (into #{}
      (map #(into #{} %)
        ((fn ps [ls]
           (if (empty? ls) '(())
                           (clojure.set/union (ps (next ls))
                             (map #(conj % (first ls)) (ps (next ls))))))
         s)))))

(defcheck solution-3303b396
  (fn pset [x]
    (if (seq x)
      (let [y (pset (rest x))]
        (into y (map #(conj % (first x)) y)))
      #{#{}})))

(defcheck solution-33b2181d
  (fn __ [s]
    (letfn [(gen-subsets [ss]
              (let [to-remove (vec ss)]
                (map #(disj ss %) to-remove)))]
      (loop [s (hash-set s) ps s]
        (if (empty? (disj s #{}))
          (conj ps #{})
          (recur
            (set (mapcat gen-subsets s))
            (clojure.set/union (set (mapcat gen-subsets s)) ps)))))))

(defcheck solution-33bf79a3
  (fn power-set [s]
    (cond
      (= 1 (count s)) [#{} (set s)]
      (zero? (count s)) #{#{}}
      :default
      (let [seq-1 (power-set (rest s))
            seq-2 (map #(conj % (first s)) seq-1)
            ps (set (concat seq-1 seq-2))]
        ps))))

(defcheck solution-348e846d
  (fn [sc] (let [vc (vec sc) n (count sc)]  (->> (range  (long (Math/pow 2 n)))
                                              (map (fn [v]
                                                     (->> (range n) (filter #(bit-test v %)) (map #(nth vc %)) set)
                                                     )) set )
                                            )))

(defcheck solution-34a7d36b
  (fn power [s]
    (if (empty? s)
      #{#{}}
      (let [pwr (power (rest s))]
        (clojure.set/union
          pwr
          (map #(conj % (first s)) pwr))))))

(defcheck solution-34c3c486
  (fn pset [s]
    (if (empty? s)
      #{ #{} }
      (let [f (first s)
            r (disj s f)
            p (pset r)]
        (clojure.set/union p (set (map #(conj % f) p)))))))

(defcheck solution-34c6251e
  (fn [s]
    (letfn [(ps [[x & xs]]
              (if x
                (let [psx (ps xs)]
                  (into psx (map #(conj % x) psx)))
                #{#{}}))]
      (ps (seq s)))))

(defcheck solution-34f3931f
  (fn pwr-set [s]
    (reduce (fn [a i]
              (->> a
                (map #(conj % i))
                (into a)))
      #{#{}} s)))

(defcheck solution-34f71527
  (fn [coll]
    (loop [k (count coll) a #{#{}}]
      (if (zero? k)
        a
        (recur
          (dec k)
          (set (mapcat identity (for [x (conj (map #(set [%]) coll) #{})] (map #(clojure.set/union % x) a)))))))))

(defcheck solution-357b23de
  (fn powerset [xs]
    (into #{}
      (if-let [h (first xs)]
        (for [x (powerset (rest xs))
              y [x (conj x h)]]
          y)
        [#{}]))))

(defcheck solution-35bec2c5
  (fn power-set [trgt-st]
    (letfn [(break-up [st]
              (reduce #(conj %1 (disj st %2)) #{} st))
            (next-line [st]
              (reduce #(apply conj %1 (break-up %2)) #{} st))]
      (loop [st #{trgt-st} pwr-st #{trgt-st}]
        (if (= st #{#{}}) pwr-st
                          (recur (next-line st) (apply conj pwr-st (next-line st))))))))

(defcheck solution-35e2788
  #(set (for [x (%2 (Math/pow 2 (% %3)))]
          (set (for [y (%2 (% %3)) :when (bit-test x y)]
                 ((vec %3) y))))) count range)

(defcheck solution-3608f722
  (fn [s] (reduce (fn [p f] (into p (map #(conj % f) p))) #{#{}} s)))

(defcheck solution-3626cc7d
  (fn [s]
    (reduce
      (fn [s r] (into s (map #(conj % r) s)))
      #{#{}}
      s)))

(defcheck solution-364034d2
  (fn [s] (set (map
                 (fn [n] (set (map first (filter second (zipmap s (map odd? (take (count s) (iterate #(bit-shift-right % 1) n))))))))
                 (range (Math/pow 2 (count s)))))))

(defcheck solution-36a36a75
  (fn [s]
    (apply hash-set #{}
      ((fn p [s]
         (if (empty? s) s
                        (let [h (first s) t (rest s) rr (p t)]
                          (concat [#{h}] rr (map #(conj % h) rr))))) s))))

(defcheck solution-36d786be
  (fn power-set [s]
    (if (empty? s) (set [s])
                   (let [#_#__ (println "s: " s)
                         elt (first s)
                         #_#__ (println "elt" elt)
                         others (set (rest s))
                         #_#__ (println "other" others)
                         ps (power-set others)
                         #_#__ (println "ps" ps)
                         with-elt (set (map #(conj % elt) ps))
                         #_#__ (println "with-elt" with-elt)]
                     (clojure.set/union ps with-elt)))))

(defcheck solution-36e10f42
  (fn power-set [s] (letfn [
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
                            (subsets [s m]
                              (let [n (count s)]
                                (loop [result #{} i (vec (range m))]
                                  (if (not i)
                                    result
                                    (recur (conj result (set (filter-by-index s i))) (inc-range i n)))
                                  )
                                )
                              )]
                      (reduce (fn [a b] (into a (subsets s b))) #{#{}} (range 1 (inc (count s))))
                      )))

(defcheck solution-37bab33c
  (fn P [S]
    (if (empty? S) #{S}
                   (let [e (first S)
                         T (disj S e)
                         PT (P T)
                         F (fn [e T] (set (map #(conj % e)  T)))]
                     (set (concat PT (F e PT)))))))

(defcheck solution-37d91b
  (fn power-set [S]
    (if (empty? S)
      #{#{}}
      (let [F (fn [e T] (into #{} (map #(conj % e) T)))
            e (first S)
            T (disj S e)
            power-T (power-set T)]
        (clojure.set/union power-T (F e power-T))))))

(defcheck solution-385f9d1f
  (fn [myset] (letfn [(combos [v] (if (seq v) (mapcat #(vector % (conj % (first v))) (combos (rest v))) '(#{})))] (set (map set (combos myset))))))

(defcheck solution-38afb46
  (fn [coll]
    (loop [incoming coll
           counter (count coll)
           outgoing #{#{}}]
      (if (< counter 1)
        (do
          #_(prn "its here" counter (count coll))
          (clojure.set/union outgoing #{#{}}))
        (let [new-out (into #{} (for [x outgoing y incoming ] (clojure.set/union x #{y})))]
          (do
            #_(prn "its here too...")
            (recur incoming
              (dec counter)
              new-out)))))))

(defcheck solution-38f640f5
  (fn [c]
    (reduce (fn [r x] (into r (map #(conj % x) r)))
      #{#{}} c)))

(defcheck solution-38fc479e
  (fn power-set
    ([s] (power-set (count s) s))
    ([k s]
     (if (= k 0)
       #{#{}}
       (let [subsets (power-set (dec k) s)]
         (into subsets
           (for [x s
                 ss subsets]
             (conj ss x))))))))

(defcheck solution-392f3711
  (fn [s]
    (let [c (count s) ]
      (letfn [(bit-seq [n] (lazy-seq (cons (bit-and n 1) (bit-seq (quot n 2)))))]
        (set
          (map (fn [n]
                 (set
                   (remove nil? (map #(when (= %2 1) %) s (take c (bit-seq n))))))
            (range (Math/pow 2 c))))))))

(defcheck solution-39395596
  (fn [s]
    (loop [i (count s)
           r #{#{}}]
      (if (= i 0) r
                  (recur (dec i)
                    (into r (for [x r y s] (conj x y))))))))

(defcheck solution-393d59be
  (fn [s]
    (let [f (fn f [s [x & xs]]
              (let [new-s (conj s x)]
                (if xs
                  [new-s (f new-s xs) (f s xs)]
                  new-s)))]
      (into #{#{}} (flatten (f #{} (seq s)))))))

(defcheck solution-3958869
  (fn [s]
    (if (empty? s) ; special case
      #{#{}}
      (conj (reduce #(set (for [x % y %2]
                            (conj (if (set? x) x #{x}) y)))
              (repeat (count s) s)) #{}))))

(defcheck solution-39645e41
  (fn power-set [s]
    (loop [s s acc #{#{}}]
      (if (empty? s)
        acc
        (recur
          (disj s (first s))
          (into acc
            (map #(conj % (first s)) acc)))))))

(defcheck solution-3993ac8e
  (fn [coll]
    (letfn [(ps [s]
              (if (empty? s)
                '([])
                (mapcat #(list % (conj % (first s))) (ps (rest s)))))]
      (set (map #(set %) (ps coll))))))

(defcheck solution-39d54680
  (fn power-set [s]
    #_(println s)
    (if (empty? s)
      #{#{}}
      (let [without-fs (power-set (rest s))]
        (into #{}
          (concat
           without-fs
           (map #(set (cons (first s) %)) without-fs)))))))

(defcheck solution-3a2854f0
  (fn [s]
    (letfn [(incps [ss v]
              (apply conj ss (map #(conj % v) ss)))]
      (reduce
        incps
        #{#{}}
        s))))

(defcheck solution-3a5a1bd5
  (fn [s]
    (set
      (let [r range
            c count
            p #(Math/pow % %2)
            e (map list (map #(int (p 2 %)) (r (c s))) s)]
        (for [i (r 0 (p 2 (c s)))]
          (set (map second (filter #(< 0 (bit-and (nth % 0) i)) e))))))))

(defcheck solution-3abc1dab
  (fn pset [x]
    (letfn [(zerobit? [n bitno] (zero? (bit-and (bit-shift-left 1 bitno) n)))
            (f [c] (set (remove nil?
                          (map-indexed (fn [idx y]
                                         (if (zerobit? c idx)
                                           nil y)) x))))]
      (set (map f (range (bit-shift-left 2 (count x))))))))

(defcheck solution-3b36601c
  (fn powset [s]
    (letfn [(pwize [accum s]
              (concat accum
                      (map (fn [pv-set]
                             (conj pv-set s))
                        accum)))]
      (set (reduce pwize [#{}] (vec s))))))

(defcheck solution-3b4b1d8a
  (fn power-set
    ([s] (power-set s #{#{}}))
    ([s acc]
     (if (empty? s) acc
                    (power-set
                      (rest s)
                      (into (conj acc (hash-set (first s))) (for [e acc] (conj e (first s))))))
     )
    ))

(defcheck solution-3bfcb0bc
  (fn f [s]
    (if (empty? s)
      #{#{}}
      (set
        (loop [t (rest s)
               h (first s)
               r #{#{} #{h}}]
          (if (empty? t)
            r
            (recur (rest t)
              h
              (let [b (f t)]
                (concat r
                        b
                        (map #(conj % h) b))))))))))

(defcheck solution-3c0e17ba
  (fn [s]
    (loop [xs s acc #{#{}}]
      (if (empty? xs)
        acc
        (recur
          (rest xs)
          (clojure.set/union
            acc
            (map #(conj % (first xs)) acc)))))))

(defcheck solution-3c68ad9
  (fn powerset [ls]
    (if (empty? ls) #{#{}}
                    (clojure.set/union (powerset (next ls))
                      (map #(conj % (first ls)) (powerset (next ls)))))))

(defcheck solution-3c83ec3f
  (fn ps [s]
    (letfn [(append-set [items s]
              (apply conj s
                (flatten
                  (for [s-m s]
                    (for [i-m items]
                      (conj s-m i-m))))))]
      (loop [c (count s)
             acc #{#{}}]
        (if (= c 0)
          acc
          (recur (dec c)
            (append-set s acc)))))))

(defcheck solution-3d25f7db
  (fn p-85 [s]
    (reduce (fn [power-set new-elem]
              (clojure.set/union
                power-set
                (map #(conj % new-elem) power-set)))
      #{#{}}
      s)))

(defcheck solution-3d427203
  (fn [s]
    (set (reduce #(concat %1
                          (map (fn [i]
                                 (set (conj i %2)))
                            %1))
           #{#{}}
           s))))

(defcheck solution-3e317cf0
  (fn [s]
    (reduce
      (fn [sets el]
        (into sets (map #(conj % el) sets)))
      #{#{}}
      s)))

(defcheck solution-3e4585f0
  (fn powersetbinX[s]
    (let [
          length (count s)
          binaryLength (int (Math/pow 2 length))
          allBinaries (range 0 binaryLength)
          sVec (vec s)
          ]
      (set (map (fn [b] ((fn binToSubset [b, sVec, length]
                           (set (map (fn[n] (get sVec n))
                                  (filter (fn[n] (bit-test b n)) (range 0 length))
                                  )))
                         b sVec length)) allBinaries))
      )
    ))

(defcheck solution-3f2a8c91
  (fn p [s]
    (set
      (if (empty? s) #{#{}}
                     (let [x (first s) r (p (rest s))]
                       (concat r
                               (map #(conj % x) r)))))))

(defcheck solution-3fa8b4c1
  (fn power-set [s]
    (let [n-to-set #(set (remove nil? (map-indexed (fn [n e] (when (bit-test % n) e)) s)))]
      (set (map n-to-set (range (Math/pow 2 (count s))))))))

(defcheck solution-3fedc51b
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (set (for [a (p (rest s))
                 x [a (into a [(first s)])]]
             x)))))

(defcheck solution-3ff5b2e8
  (fn [s]	(let [comb (fn f [c]
                        (when-not (empty? c)
                          (let [x (f (vec (rest c)))]
                            (concat (for [i (range (count c))] (set (concat [(first c)] [(c i)] )))
                                    (for [j x] (into #{(first c)} j))
                                    x )) ))]
             (into #{} (conj (comb (vec s)) (empty s))))))

(defcheck solution-409ec081
  (fn power-set [col]
    (cond (empty? col)
          #{#{}}
          :else
          (let [c (power-set (rest col))
                e (first col)]
            (apply hash-set (apply concat (map (fn [itm] (list (conj itm e) itm)) c)))))))

(defcheck solution-40f89298
  (comp
   set
   (partial map set)
   #(map
      (fn [a b] (map second (filter (fn [[x _]] (pos? (bit-and a (bit-shift-left 1 x)))) b)))
      (range 0 (bit-shift-left 1 (count %)))
      (repeat (map vector (range) %)))))

(defcheck solution-40fb8404
  (fn power-set
    ([theset] (set (power-set theset #{})))
    ([s result]
     (if (empty? s)
       (list result)
       (concat  (power-set (rest s) (conj result (first s)))
                (power-set (rest s) result))))))

(defcheck solution-41544a46
  (fn [src]
    (if (empty? src) #{#{}}
                     (letfn [(tst [s] (distinct(mapcat #(for [x (range (count %))] (disj % (nth (vec %) x))) s)))]
                       (loop [x (list src) y #{#{}}]
                         (if (= 1 (count (first x))) (set(concat y x))
                                                     (recur (tst x) (concat y x))
                                                     )
                         )
                       )
                     )))

(defcheck solution-41b90c69
  (fn f [l]
    (loop [r #{#{}}
           s [l]]
      (if (seq s)
        (recur
          (into r s)
          (set (mapcat (fn [x] (map #(disj x %) x)) s)))
        r))))

(defcheck solution-41d2fb36
  (fn power-set [coll]
    (reduce
      (fn [ps x]
        (into ps (map #(conj % x) ps)))
      #{#{}}
      coll)))

(defcheck solution-420c9330
  (fn power-set [xs]
    (loop [ys xs
           sets #{#{}}]
      (if (empty? ys) sets
                      (recur (rest ys) (into sets (map #(conj % (first ys)) sets)))))))

(defcheck solution-423edc79
  (fn power-set [x]
    (let [permute (fn permute [s]
                    (if (empty? s)
                      s
                      (let [perm-rest (permute (rest s))
                            hd        (first s)]
                        (concat [s] [[hd]] (map #(conj % hd) perm-rest) perm-rest))))]
      (conj (set (map set (permute x))) #{}))))

(defcheck solution-42f5cd1e
  (fn [x] (set
            (reduce
              (fn [ps x] (into ps (map #(conj % x) ps)))
              (list (set nil)) (apply list x)))))

(defcheck solution-435a832a
  (fn subsets [s]
    (if (empty? s)
      #{#{}}
      (let [ss (subsets (rest s))]
        (set (concat ss (map #(set (conj % (first s))) ss)))))))

(defcheck solution-437150a2
  (fn powerset[theset]
    (if(empty? theset)
      #{#{}}
      (let[nextSet (powerset (rest theset) ),
           head (first theset)]

        (reduce  conj nextSet (map #(conj % head) nextSet) )

        )
      )
    ))

(defcheck solution-43876cc2
  (fn power-set [source-set]
    (if (empty? source-set)
      #{#{}}
      (let [subset (power-set (rest source-set))]
        (into subset
          (map #(conj % (first source-set)) subset))))))

(defcheck solution-43c486a8
  (fn [s]
    (reduce
      (fn [init e]
        (set (concat init (map #(conj % e) init) [#{e}])))
      #{#{}} s)))

(defcheck solution-43daec7a
  (fn powerset [ls]
    (if (empty? ls) #{#{}}
                    (clojure.set/union (powerset (next ls))
                      (map #(conj % (first ls)) (powerset (next ls)))))))

(defcheck solution-44182bbe
  (fn p [s]
    (let [h (first s)
          t (map #(drop % s) (range 1 (inc (count s))))
          u (map p t)]
      (if (empty? s)
        #{#{}}
        (into #{}
          (conj (concat
                 (apply concat
                   (map (fn [x]
                          (map #(conj % h)
                            x))
                     u))
                 (apply concat u))
            #{}))))))

(defcheck solution-44369a40
  (fn power-set [s]
    (if-let [[firs & nexs] (seq s)]
      (let [pow-nexs (power-set nexs)
            add-pow (map #(conj % firs) pow-nexs)]
        (into #{} (concat pow-nexs add-pow)))
      #{#{}})))

(defcheck solution-44554a52
  (fn gen-powerset [s]
    (if-let [f (first s)]
      (let [smaller (gen-powerset (rest s))]
        (clojure.set/union smaller (map #(conj % f) smaller))
        )
      #{#{}}
      )))

(defcheck solution-4485da3
  (fn powerset[s]
    (if (empty? s) #{#{}}
                   (into (powerset (rest s))
                     (map #(conj % (first s)) (powerset (rest s)))))))

(defcheck solution-44b25bb7
  (fn jajj [z]
    ((fn ps [x]
       (if (empty? x)
         #{#{}}
         (let [[e & r] x, res (ps r)]
           (into res (map #(conj % e) res)))))
     (seq z))))

(defcheck solution-44c57e9a
  (fn power-set [st]
    (if (empty? st)
      #{#{}}
      (let [fst (first st)
            left-power-set (power-set (rest st))]
        (clojure.set/union left-power-set
          (map #(conj % fst) left-power-set))))))

(defcheck solution-44c8a704
  (fn powerset [s] (set (map set ((fn p [s] (if (empty? s) '(()) (clojure.set/union (p (next s)) (map #(conj % (first s)) (p (next s)))))) s)))))

(defcheck solution-44e661c7
  #(let [s (count %)
         n (bit-shift-left 1 s)
         c (seq %)]
     (set
       (for [i (range n)]
         (set
           (for [j (range s) :when (bit-test i j)]
             (nth c j)))))))

(defcheck solution-450e412c
  (fn [s]
    (loop [res #{#{}}
           s s]
      (if (empty? s)
        res
        (recur
          (clojure.set/union res
            (set (map (fn [v] (conj v (first s))) res)))
          (rest s))))))

(defcheck solution-453ad116
  (fn power-set [s]
    (set
      (if (empty? s)
        (list #{})
        (let [ele (first s)
              subset (remove #{ele} s)
              power-subset (power-set subset)]
          (concat power-subset (map #(conj % ele) power-subset)))))))

(defcheck solution-45b28b40
  (fn [s]
    (reduce (fn [powset el]
              (into powset
                (map #(conj % el) powset)))
      #{#{}}
      s)))

(defcheck solution-4605336c
  (fn [xs]
    (letfn
     [(diff [xs ys]
        (reduce disj xs ys))
      (comb [cs xs]
        (set
          (mapcat
            #(for [y (diff xs %)] (conj % y))
            cs)))]
      (loop [m (count xs) ys (set (map hash-set xs)) zs #{}]
        (if (>= 1 m)
          (conj zs #{})
          (let [ys' (comb ys xs)
                zs' (into zs (concat ys ys'))]
            (recur (dec m) (comb ys xs) zs')))))))

(defcheck solution-4637b03
  (fn subsets [s]
    (if (empty? s)
      #{#{}}
      (let [ts (subsets (rest s))]
        (->> ts
          (map #(conj % (first s)))
          (clojure.set/union ts))))))

(defcheck solution-466f0b2c
  (fn [input]
    (into #{} (reduce
                (fn subset [results cur]
                  (apply conj results (map #(clojure.set/union (hash-set cur) %) results)))
                [#{}] input))))

(defcheck solution-467a64f9
  (fn [s] (reduce (fn [power-set x]
                    (into power-set (map #(conj % x) power-set)))
            #{#{}}
            s)))

(defcheck solution-46b189d3
  (fn [s]
    ((fn i [c]
       (if (contains? c s)
         c
         (i (reduce (fn [v x] (into v (map #(conj x %) s))) c c))
         )) #{#{}})))

(defcheck solution-46b7b8f2
  (fn power-set [s]
    (if (empty? s)
      #{s}
      (let [f (-> s first hash-set)
            p (power-set (into #{} (rest s)))]
        (clojure.set/union #{f}
          (map #(clojure.set/union % f)
            p)
          p)))))

(defcheck solution-47197587
  (fn [s]
    (reduce
      (fn [o i]
        (into o (map #(conj % i) o)))
      #{#{}} s)))

(defcheck solution-4796729e
  (fn p [s]
    (if
     (= s #{})
      #{#{}}
      (let [a (p (set (rest s)))]
        (into a (map #(conj % (first s)) a))))))

(defcheck solution-47e401e1
  (fn a [xs]
    (loop [s (clojure.set/union #{#{}} (set (for [i xs] #{i})))]
      (let [n (clojure.set/union
                s
                (set (for [x s y s :when (and (= (count x) 1) (not= x y))]
                       (clojure.set/union x y)
                       )))]
        (if (= s n) n
                    (recur n))))
    ))

(defcheck solution-48274c0a
  (fn [x]
    (letfn [(power-set [x]
              (if (= 0 (count x))
                #{x}
                (if (= 1 (count x))
                  #{#{} #{(first x)}}
                  (let [fst (first x)
                        rst (rest x)
                        ps-wo-fst (power-set rst)
                        ps-w-fst (into #{} (map #(conj % fst) ps-wo-fst))
                        ]
                    (clojure.set/union ps-wo-fst ps-w-fst)
                    )
                  ))) ]
      (power-set x)
      )))

(defcheck solution-48498d9d
  (fn subsets [st]
    (set
      (if
       (empty? st)
        #{#{}}
        (let
         [rs (subsets
               (rest st))]
          (clojure.set/union
            (map
              #(conj
                 %
                 (first st))
              rs)
            rs))))))

(defcheck solution-486d0ab8
  #(let [c (count %)]
     (loop [done [%] ac #{%} sz 0]
       (if (>= sz c)
         ac
         (let [d (into #{}(for [o done i o]
                            (disj o i)))]
           (recur d (into ac d) (inc sz)))))))

(defcheck solution-48909ea0
  (fn combinations [ls]
    (if (empty? ls)
      #{#{}}
      (let [n (next ls)
            com (combinations n)]
        (clojure.set/union com (map #(conj % (first ls)) com))))))

(defcheck solution-4890f1a1
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (set (flatten (for [i (p (rest s))]
                      [(set (cons (first s) i)) i]))))))

(defcheck solution-491cd55
  (fn powerset [a-set]
    (let [n (Math/pow 2 (count a-set))
          lst (map-indexed vector a-set)]
      (into #{} (map (fn [i] (set (map second (filter (fn [[idx val]]
                                                        (bit-test i idx))
                                                lst))))
                  (range 0 n))))))

(defcheck solution-493060dc
  (fn pwr [s]
    (if (empty? s)
      #{#{}}
      (let [ls (into '() s)
            rst (into '() (pwr (set (rest ls))))]
        (set (concat rst
                     (map #(conj % (first ls)) rst)))))))

(defcheck solution-494f782c
  (fn [coll]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} coll)))

(defcheck solution-495ac884
  (fn [s]
    (loop [current (seq s) results #{#{}}]
      (cond
        (= 0 (count current)) (conj results #{})
        ;(= 1 (count current)) (conj results #{} #{(first s)})
        :else (recur (rest current) (apply (partial conj results) (map #(conj % (first current)) results)))))))

(defcheck solution-49844b94
  (fn superset [xs]
    (if (not (seq xs))
      #{#{}}
      (let [f (first xs)
            r (set (rest xs))
            subset (superset r)]
        (clojure.set/union
          subset
          (map #(conj % f) subset))))))

(defcheck solution-4984a149
  (fn [s]
    (let [v (vec s)
          range-to (Math/pow 2 (count s))
          pick-by-bit (fn [v n]
                        (for [i (range (count v))
                              :when (bit-test n i)]
                          (v i)))]
      (into #{} (map #(set (pick-by-bit v %)) (range range-to))))))

(defcheck solution-49a26d4d
  (fn [c]
    (reduce
      (fn [s e]
        (into s (map #(conj % e) s)))
      #{#{}}
      c)))

(defcheck solution-49aa2e66
  (fn ps ([x] (ps x #{#{}}))
    ([l r]
     (if (empty? l)
       r
       (let [step (map #(conj % (first l)) r)]
         (ps (rest l) (set (concat r step))))))))

(defcheck solution-49b181fd
  (letfn [(subset [s]
            (set (for [i s] (disj s i))))]
    (fn powerset [s]
      (if (< 0 (count s))
        (loop [ss (subset s)
               ret #{#{} s}]
          (if (= ss #{#{}})
            (set ret)
            (recur (set (reduce concat #{} (map subset ss))) (concat ret ss))))
        #{#{}}))))

(defcheck solution-49b58645
  (fn power-set [coll]
    (letfn [(union [s1 s2] (reduce conj s1 s2))]
      (if (empty? coll)
        #{#{}}
        (let [element (first coll)
              sub1 (power-set (disj coll element))
              sub2 (set (map #(union #{element} %) sub1))]
          (union sub1 sub2))))))

(defcheck solution-49f0efef
  #((fn f [c n]
      (if (= n 0) c
                  (f (into c
                       (for [x % a c] (conj a x)))
                    (- n 1)))) #{#{}} (count %)))

(defcheck solution-4a4799b3
  (fn [x] (loop [s (seq x), r '()]
            (if (empty? s) (into #{} (cons #{} r))
                           (recur (rest s) (concat #{#{(first s)}} r
                                                   (map (fn [y] (clojure.set/union #{(first s)} y)) r))) ))))

(defcheck solution-4a73e67d
  (fn [s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (reduce into (set [#{} s]) (vals (reduce #(p % %2 s) {} (range (count s))))))))

(defcheck solution-4ac62fd9
  (fn
    [s]
    (letfn [(plist [l]
              (if (empty? l)
                '(())
                (let [q (plist (rest l))
                      x (first l)]
                  (concat q (map #(cons x %) q)))))]
      (set (map set (plist (seq s)))))))

(defcheck solution-4ad6f229
  (
    fn[s]
    (let [a (fn [x s] [ (conj s x) s])]
      (set (reduce #(mapcat (partial a %2) %) #{ #{} } s))) ))

(defcheck solution-4b26e893
  (fn powerset [input] (cond (empty? input) #{#{}}
                             :else (let [e (first input) t (disj input e)]
                                     (clojure.set/union
                                       (powerset t)
                                       (into (map #(conj % e)  (powerset t)) #{}))))))

(defcheck solution-4bc12c5f
  (fn p [x]
    (if-let [f (first x)]
      (set (mapcat (fn [z] [z (conj z f)]) (p (disj x f))))
      #{#{}})))

(defcheck solution-4bc56e63
  (fn [s] (letfn [
                  (bits [res k n] (if (< n 2) (if (> k (count res)) (recur (cons n res) k 0) res)
                                              (recur (cons (mod n 2) res) k (int (/ n 2)) )
                                              )
                    )
                  (myexp [r n] (reduce * (repeat n r)))
                  (zips [k ss] (partition 2 (interleave (bits [] (count ss) k) ss)))
                  ]
            (let [len (count s)
                  lim (myexp 2 len)
                  hf (fn [xs] (for [x (map #(zips % xs) (range lim))]
                                (map #(second %) (filter #(= 1 (first %)) x))
                                ))
                  ]

              (set (map set (hf s)))
              )
            )
    ))

(defcheck solution-4c000db0
  (fn [all]
    (loop [built (hash-set all)]
      (let [new (into built (for [x built s all] (disj x s)))]
        (if (= new built) built
                          (recur new))))))

(defcheck solution-4c130b91
  (fn [ss]
    (loop [rs #{#{}}]
      (let [rs1 (into rs
                  (mapcat
                    (fn [v]
                      (if (empty? rs)
                        #{v}
                        (map #(conj % v) rs)))
                    ss))]
        (if (= rs rs1)
          rs
          (recur rs1))))))

(defcheck solution-4cd52473
  (fn __
    [s]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} s)))

(defcheck solution-4d52b55b
  (fn power-set [s]
    (reduce
      (fn [sets x]
        (->> sets
          (map #(conj % x))
          (into sets)))
      #{#{}} s)))

(defcheck solution-4d6e3bcc
  (fn [s]
    (reduce (fn [cum-pow-set e]
              (into cum-pow-set (map #(conj % e) cum-pow-set)))
      #{#{}} s)))

(defcheck solution-4d6f6391
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [a (first s)
            b (disj s a)
            p2 (power-set b)
            p3 (map #(conj % a) p2)]
        (clojure.set/union p2 p3)
        ))))

(defcheck solution-4da52c58
  reduce (fn [s x]
           (into s (set (map #(into % #{x}) s)))) #{#{}})

(defcheck solution-4dac95a
  (fn power-set [s]
    (letfn [(pairs [s e]
              (set (map #(clojure.set/union % (hash-set e)) s)))]
      (if (empty? s)
        #{#{}}
        (let [el (first s)
              rst (disj s el)
              rst-power-set (power-set rst)]
          (clojure.set/union rst-power-set (pairs rst-power-set el)))))))

(defcheck solution-4e9dd089
  (fn [s]
    (reduce
      (fn [i e]
        (set (concat i (map #(conj % e) i) [#{e}])))
      #{#{}} s)))

(defcheck solution-4edb3be3
  (fn power-set [xs]
    (letfn [(gen-next [xs acc]
              (if (contains? acc xs)
                (conj acc #{})
                (recur xs (set (for [x xs ys acc] (conj ys x))))))]
      (gen-next xs #{#{}}))))

(defcheck solution-4f2744f3
  (fn [s]
    (letfn
     [(rsets [root [head & tail]]
        (if
         (nil? head)
          [root]
          (concat
           (rsets root tail)
           (rsets (conj root head) tail))))]
      (set (map set (rsets [] (apply list s)))))))

(defcheck solution-4f6a0c2c
  (fn[s]
    (loop[[f & r] (into [] s) res #{#{}}]
      (if f
        (recur r (into res (map #(conj % f) res)))
        res))))

(defcheck solution-4faaf873
  (fn pwrset
    ([tset] (pwrset #{ tset } #{ #{} }))
    ([targetsets builder]
     (if
      (empty? targetsets) builder
                          (let [target (first targetsets)]
                            (if (contains? builder target)
                              (recur (disj targetsets target) builder)
                              (let [new-sets (for [item target] (disj target item))]
                                (if (empty? new-sets)
                                  (recur (disj targetsets target) (conj builder target))
                                  (recur (apply conj (disj targetsets target) new-sets) (conj builder target))))))))))

(defcheck solution-4fe825bf
  (fn [s]
    (set
      (map
        set
        ((fn f [[v & o] ret]
           (if (nil? v)
             [ret]
             (concat (f o ret)
                     (f o (conj ret v)))))
         (seq s) #{})))))

(defcheck solution-4ff6499c
  (fn [s]
    (reduce (fn [s k]
              (into s (map #(conj % k) s)))
      #{#{}}
      s)))

(defcheck solution-5046fc74
  (fn [s]
    (reduce
      (fn [prev v]
        (set (mapcat #(set [(conj % v) %]) prev)))
      #{#{}} s)))

(defcheck solution-50765163
  (fn [s]
    (conj
      (reduce #(set (for [x % y %2] (conj x y))) #{#{}} (repeat (count s) s))
      #{})))

(defcheck solution-509c1f41
  (fn [s]
    (loop [q '(#{}) v #{}]
      (let [[h & t] q
            w (conj v h)]
        (if
         (empty? q) v
                    (recur (concat (remove v (map #(conj h %) s)) t) w)
                    )))))

(defcheck solution-50af58a9
  #(->> (tree-seq
          sequential?
          (fn [[curr remain]]
            (cons curr
              (loop [res []
                     remain remain]
                (if (seq remain)
                  (let [x (first remain)
                        rremain (disj remain x)]
                    (recur (conj res [(conj curr x) rremain])
                      rremain))
                  res))))
          [#{} %])
     (filter set?)
     set))

(defcheck solution-51266a65
  (fn [s]
    (reduce
      (fn [s e] (into s (map #(conj % e) s)))
      #{#{}}
      s)))

(defcheck solution-515be310
  (fn [s]
    (set
      (for [ n (range (Math/pow 2 (count s))) ]
        (set (keep-indexed
               #(if (bit-test n %1) %2)
               s))))))

(defcheck solution-51659f27
  reduce (fn [set x] (into set (map #(conj % x) set))) #{#{}})

(defcheck solution-519c83ab
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [x (first s), ps-x (power-set (disj s x))]
        (into ps-x (map #(conj % x) ps-x))))))

(defcheck solution-51a13faa
  (fn power-set [s]
    (let [[head & tail] (vec s)]
      (cond (nil? head) #{#{}}
            (empty? tail) #{#{} #{head}}
            :else (set (concat (power-set tail) (map #(conj % head) (power-set tail))))))))

(defcheck solution-51a25298
  (fn [s]
    (loop [uberset #{}
           level #{s}]
      (if (= level #{#{}})
        (into uberset level)
        (recur (into uberset level)
          (into #{} (mapcat (fn [lset]
                              (map #(disj lset %)
                                lset))
                      level)))))))

(defcheck solution-52374628
  (fn power-set [xset]
    (let [indexes (vec xset)]
      (set
        (map
          (fn [x]
            (->> (filter #(bit-test x %) (range (/ (Math/log (inc x)) (Math/log 2))))
              (select-keys indexes)
              (vals)
              (set)))
          (range (Math/pow 2 (count xset))))))))

(defcheck solution-526ec897
  (fn powerset [a-set]
    (letfn [(powerset-helper [current leftovers]
              (if (empty? leftovers)
                (set current)
                (let [elem (first leftovers)]
                  (powerset-helper (concat current
                                           (map (fn [x] (conj x elem)) current))
                    (rest leftovers)))))]
      (powerset-helper #{#{}} a-set))))

(defcheck solution-530e6b03
  (fn [s]
    (loop [ps #{} ls #{#{}}]
      (if (contains? ls s) (clojure.set/union ps ls)
                           (recur (clojure.set/union ps ls) (set (for [x ls y s] (conj x y))))))))

(defcheck solution-533b6167
  (fn pow [seq]
    (if (= (count seq) 0)
      #{#{}}
      (apply hash-set (concat (pow (rest seq)) (map #(conj % (first seq)) (pow (rest seq))))))))

(defcheck solution-53a4ff47
  (fn power-set
    ([items] (power-set items #{}))
    ([items current]
     (if (empty? items)
       #{current}
       (set (concat (power-set (rest items) current) (power-set (rest items) (conj current (first items)))))
       )
     )
    ))

(defcheck solution-53a5ac07
  (fn f [s]
    (if (empty? s) #{ #{} }
                   (clojure.set/union (f (rest s))
                     (set (map #(conj % (first s)) (f (rest s))))))))

(defcheck solution-53bd255f
  (fn [s]
    (let [f! (atom nil)]
      (letfn [(f1 [s] (map #(clojure.set/difference s #{%}) s))
              (f2 [s]
                (if (seq s)
                  (let [c (f1 s)]
                    (reduce
                      #(clojure.set/union %1 (@f! %2))
                      (set c)
                      c))
                  #{}))]
        (reset! f! (memoize f2))
        (conj (f2 s) s)))))

(defcheck solution-53d31a94
  (fn power-set [s]
    (letfn [(subsetv [s bits]
              (set
                (remove nil?
                  (map-indexed #(if (bit-test bits %) %2) s))))]
      (let [v (vec s)
            nn (bit-shift-left 1 (count s))]
        (set
          (map #(subsetv v %) (range nn)))))))

(defcheck solution-540c53e5
  (fn [st]
    (reduce (fn [s t] (into s (for [x s] (conj x t))))
      #{#{}}
      st)))

(defcheck solution-554d7220
  #(let [subsets (fn subsets [a-set]
                   (let [rets #{#{} } pairs (partition 2 (interleave a-set (repeat nil)))]
                     (reduce (fn [rs pair]
                               (set
                                 (for [r rs,p pair]
                                   (conj r p))))
                       rets pairs)))]
     (set (for [s (subsets %)]
            (set (keep identity s))))))

(defcheck solution-55ab9f1e
  (fn [s]
    (if (empty? s) #{s}
                   (let [[x & xs] (vec s)
                         grow (fn [cs e] (lazy-cat (for [c cs] (conj c e)) cs))
                         combos (reduce grow [[x] []] xs)]
                     (into #{} (map set combos))))))

(defcheck solution-55dc61ba
  (fn pset [x]
    (if (empty? x)
      #{#{}}
      (clojure.set/union (pset (next x))
        (map #(conj % (first x)) (pset (next x)))))))

(defcheck solution-56593757
  (fn [s]
    (loop [ret #{#{}} cnt s]
      (if (= cnt #{})
        ret
        (let [h (first cnt)]
          (recur (clojure.set/union ret (set (map #(conj % h) ret)))
            (disj cnt h)))))))

(defcheck solution-5681a881
  (fn [s]
    (let [len (count s)
          half (Math/ceil (/ len 2))]
      (letfn ((sub [s n h]
                (cond (= n 0)
                      #{#{}}
                      (= n 1)
                      (reduce (fn [r v]
                                (conj r (conj h v)))
                        #{}
                        s)
                      :else
                      (reduce (fn [r v]
                                (clojure.set/union r (sub (disj s v) (- n 1) (conj h v))))
                        #{}
                        s)))
              (sub2 [s2]
                (reduce (fn [r v]
                          (conj r (clojure.set/difference s v)))
                  #{}
                  s2)))
        (reduce (fn [r n]
                  (if (> n (/ len 2))
                    (clojure.set/union r (sub2 (sub s (- len n) #{})))
                    (clojure.set/union r (sub s n #{}))))
          #{}
          (range (+ len 1)))))))

(defcheck solution-56b7c088
  (letfn [(k-combinations [k s]
            (let [pool  (vec s)
                  n     (count pool)
                  zip   (partial map vector)]
              (cond
                (zero? k) #{#{}}
                (or (zero? n) (> k n)) #{}
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
                           ac))))))]
    (fn [s]
      (reduce into #{s}
        (for [k (range (count s))]
          (k-combinations k s))))))

(defcheck solution-573001ee
  reduce (fn [ss e] (into ss (map #(conj % e) ss))) #{#{}})

(defcheck solution-5844230a
  (fn power-set [s]
    (loop [s s ret #{#{}}]
      (if (empty? s)
        ret
        (recur (rest s) (apply conj ret (map #(conj % (first s)) ret)))))))

(defcheck solution-5883afb
  #((fn [p s] (if (empty? s) (conj p #{})
                             (recur (apply conj p #{(first s)} (map (fn [x] (conj x (first s))) p)) (rest s)))) #{} %))

(defcheck solution-58d1120a
  (fn ps [s]
    (if (empty? s)
      #{s}
      (let [first-elt (first s)
            rest (disj s first-elt)
            ps-rest (ps rest)
            ps-with-first (map #(conj % first-elt) ps-rest)]
        (clojure.set/union ps-rest ps-with-first)))))

(defcheck solution-58ec33af
  #(set
     (map
       (fn [x]
         (set
           (for [b (range (count %))
                 :when (pos? (bit-and 1 (bit-shift-right x b)))]
             (nth (seq %) b))))
       (range (bit-shift-left 1 (count %))))))

(defcheck solution-5918f1a5
  (fn [s]
    (letfn [(add [coll x] (into coll (map #(conj % x) coll)))]
      (reduce add #{#{}} s))))

(defcheck solution-593de19f
  (fn [s]
    (reduce #(reduce (fn [ps sub]
                       (conj (conj ps sub)
                         (conj sub %2)))
               #{}
               %)
      #{#{}} s)))

(defcheck solution-59aebc8a
  (fn [s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) #{#{}} s))))

(defcheck solution-5a5f275
  #(set
     (reduce
       (fn [s x]
         (concat s
                 (map (fn [y] (conj y x)) s)))
       #{#{}} %)))

(defcheck solution-5a8670a3
  (fn my-power-set [s]
    (loop [result #{#{}}]
      (if (result (set s)) result
                           (recur (conj (set (for [x result y s] (conj x y))) #{}))))))

(defcheck solution-5a915381
  (fn [s]
    (letfn [(combine [acc x]
              (conj (into acc (map #(conj % x) acc)) #{x}))]
      (conj (reduce combine #{} s) #{}))))

(defcheck solution-5ac05c81
  (fn power-set [s]
    (let [length (count s)
          pattern (map (fn [e] (map #(bit-test e %) (range length)))
                    (range (reduce * (repeat length 2))))]
      (->> pattern
        (map #(map (fn [f e] (and f e)) % s))
        (map #(remove false? %))
        (map set)
        set))))

(defcheck solution-5ad8ac0b
  (fn power-set [s]
    (letfn [(f [e t] (set (map #(clojure.set/union % #{e}) t)))]
      (if (empty? s)
        #{#{}}
        (first
          (map
            #(let [y (power-set (clojure.set/difference s #{%}))]
               (clojure.set/union y (f % y)))
            s))))))

(defcheck solution-5af3c172
  (fn power-set [coll]
    (if (empty? coll)
      #{#{}}
      (let [others (power-set (rest coll))]
        (clojure.set/union
          others
          (map #(conj % (first coll)) others)
          )))))

(defcheck solution-5b0999a5
  (fn powerset [a-set]
    (if (empty? a-set) #{#{}}
                       (clojure.set/union (powerset (next a-set))
                         (map #(conj % (first a-set))
                           (powerset (next a-set)))))))

(defcheck solution-5b0f80dd
  (fn power-set [s]
    (if-let [[f & r] (seq s)]
      (let [beck (power-set r)]
        (into beck (for [el beck] (conj el f))))
      #{#{}})))

(defcheck solution-5b205050
  (fn [s]
    (loop [q #{s}
           r #{}]
      (if-let [f (first q)]
        (let [n (filter (complement r) (map #(disj f %) f))]
          (recur
            (into (rest q) n)
            (conj r f)))
        r))))

(defcheck solution-5b2dbb4b
  #(set
     (reduce
       (fn [p a]
         (mapcat (fn [_] [(conj _ a) _]) p))
       [#{}]
       %)))

(defcheck solution-5be0b6e3
  (fn ps [a]
    (if-let [e (first a)]
      (let [r (ps (disj a e))]
        (clojure.set/union (set (map #(conj % e) r)) r))
      #{#{}})))

(defcheck solution-5be4e6a9
  (fn [bset]
    (reduce (fn [curset elem]
              (apply conj curset (map #(conj % elem) curset)))
      #{#{}}
      bset)))

(defcheck solution-5bed513a
  (fn my-power-set [coll]
    (letfn [(add-to-set [acc v]
              (concat acc [v]
                      (map #(clojure.set/union v %) acc)))]
      (set
        (clojure.set/union #{#{}}
          (reduce
            add-to-set
            #{}
            (map #(set[%]) coll)))))))

(defcheck solution-5bedb144
  (fn ps [s]
    (letfn
     [(gen [que mx res]
        (if (empty? que)
          res
          (let [e (first que)
                mn (if (empty? e) 0 (inc (last e)))
                kids (for [n (range mn mx)] (conj e n))]
            (recur (concat (rest que) kids) mx (conj res e)))))]
      (set (map #(set (map (vec s) %)) (gen [[]] (count s) []))))))

(defcheck solution-5c3f6265
  (fn pset [s]
    (set
      (if (>= 1 (count s)) [(set s) #{}]
                           (concat
                            [s]
                            [(set [(first s)])]
                            (pset (set (rest s)))
                            (map #(set (cons (first s) %)) (pset (set (rest s))))
                            )))))

(defcheck solution-5d7f4012
  (fn [s]
    (set ((fn rec [[n & ns]]
            (if n
              (let [ms (rec ns)]
                (into ms (map #(conj % n) ms)))
              '(#{})))
          (vec s)))))

(defcheck solution-5d7f8c83
  reduce (fn [s x]
           (into s (map #(conj % x) s))) #{#{}})

(defcheck solution-5d9f2619
  (fn self [xs]
    (if (empty? xs)
      #{#{}}
      (let [x (first xs)
            s (self (rest xs))]
        (into s (map #(conj % x) s))))))

(defcheck solution-5eaf057
  (partial reduce
    (fn [c x]
      (into c (map #(conj % x) c)))
    #{#{}}))

(defcheck solution-5ebd7078
  (fn power-set [xs]
    (let [bits (for [i (range 0 (bit-shift-left 1 (count xs)))]
                 (for [j (range 0 (count xs))] (= (bit-and (bit-shift-right i j) 1) 1)))
          lx (vec xs)
          filter-by-list (fn [xs filter-list]
                           (->> (partition 2 (interleave xs filter-list))
                             (filter #(true? (second %)))
                             (map first)))]
      (set (for [bi bits] (set (filter-by-list xs bi)))))))

(defcheck solution-5ee54fe9
  (fn powset [aset]
    (if (empty? aset)
      #{#{}}
      (reduce into #{} (map #(list % (conj % (first aset))) (powset (rest aset)))
        ))))

(defcheck solution-5f18c9d4
  (fn pset [aset]
    (set
      (if-let [elem (first aset)]
        (let [ps (pset (disj aset elem))]
          (concat ps (map #(conj % elem) ps)))
        [aset]))))

(defcheck solution-5f49198c
  (fn [s] (loop [ps #{#{}}
                 i 0]
            (if (> i (count s))
              ps
              (recur (into ps (mapcat (fn [e] (reduce #(conj % (conj e %2)) #{} s)) ps))
                (inc i))
              ))))

(defcheck solution-5fadea1c
  (fn [s]
    (loop [[item & items] (seq s) result [#{}]]
      (if (nil? item)
        (into #{} result)
        (recur items (concat result (map #(conj % item) result)))))))

(defcheck solution-5fb17e5a
  (fn powerset [s]
    (reduce
      #(into % (for [subset %] (conj subset %2))) #{#{}} s)))

(defcheck solution-60269445
  (fn [S]
    (reduce
      (fn [ps, i]
        (clojure.set/union ps (map #(clojure.set/union % #{i}) ps))) #{ #{} } S)))

(defcheck solution-603e4d99
  (fn [ps]
    (letfn [(bits[n] (loop [i n b []] (if (zero? i) b (recur (quot i 2) (concat b [(mod i 2)])))))]
      (into #{} (for [i (range 1024)] (into #{} (filter #(not (nil? %)) (map (fn[a b] (if (= 1 b) a)) ps (bits i)))))))
    ))

(defcheck solution-6061b590
  (fn [s]
    (set (map set
           (for [x (range (Math/pow 2 (count s)))]
             (map second
               (filter first
                 (map-indexed #(vector (bit-test x %) %2) s))))))))

(defcheck solution-607cce6f
  (fn ps [s]
    (cond
      (empty? s) #{s}
      :else
      (let [f (first s)
            r (set (rest s))
            rp (ps r)]
        (into rp (for [x rp] (conj x f)))
        )
      )
    ))

(defcheck solution-6084f27e
  (fn [n]
    (set (map #(loop [l % r #{} c n]
                 (if (zero? l) r
                               (recur
                                 (int (/ l 2))
                                 (if (= (mod l 2) 1)
                                   (conj r (first c)) r)
                                 (rest c))))
           (range (Math/pow 2 (count n)))))))

(defcheck solution-608532ff
  (fn [s]
    (set (reduce
           #(into
              (map (fn [x] (conj x %2)) %)
              %)
           '(#{})
           s))
    ))

(defcheck solution-61186628
  (letfn [(ps1 [ps x]
            (set (concat ps
                         (map #(conj % x) ps))))]
    (fn [s] (reduce ps1 #{#{}} s))))

(defcheck solution-611a0f64
  (fn power-set [xs]
    (if (seq xs)
      (let [tail      (rest xs)
            power-set (power-set tail)]
        (set (concat power-set (set (map #(conj % (first xs)) power-set)))))
      #{#{}})))

(defcheck solution-612b00a9
  (fn [t]
    (set
      ((fn f [[x & s]]
         (if x
           (let [z (f s)] (concat z (map #(conj % x) z)))
           #{#{}}))
       (seq t)))))

(defcheck solution-614a8977
  (fn [s]
    (reduce
      (fn [old-set e]
        (into old-set (map #(conj % e) old-set)))
      #{#{}} s)))

(defcheck solution-617fdc39
  reduce (fn ps [s elt]
           (reduce conj s (map #(conj % elt) s))
           ) #{#{}})

(defcheck solution-623983bc
  (fn [s]
    (let [elements (seq s)]
      (set
        (->>
          (range (int (Math/pow 2 (count s))))
          (map (fn [n]
                 (->>
                   (map (fn [i e] [i e]) (iterate #(quot % 2) n) elements)
                   (filter (fn [[i _]] (zero? (mod i 2))))
                   (map second)
                   set))))))))

(defcheck solution-624b34f3
  (fn f [s]
    (if (empty? s)
      #{#{}}
      (into (f (next s)) (map #(conj % (first s)) (f (next s)))))))

(defcheck solution-625d81da
  (let [bits->indices (fn [n]
                        (loop [x n, acc [], i 0]
                          (if (zero? x)
                            acc
                            (let [q (quot x 2)
                                  r (rem x 2)
                                  acc' (if (= 1 r) (conj acc i) acc)]
                              (recur q acc' (inc i))))))
        f (fn [u]
            (let [n (apply * (repeat (count u) 2))
                  v (vec u)]
              (->> (for [b (range n)]
                     (let [idxs (bits->indices b)]
                       (->> (map (partial nth v) idxs)
                         (into #{}))))
                (into #{}))))]
    f))

(defcheck solution-62bbdd04
  (fn powerset [set]
    (if (= set #{}) #{#{}}
                    (loop [s [set] super #{#{} set}]
                      (let [combinations (fn [coll] (map #(disj coll %) coll))
                            ssets (reduce (fn [acc e] (into acc (combinations e))) #{} s)]
                        (if (= #{} (first ssets))
                          super
                          (recur ssets (into super ssets)))
                        )))))

(defcheck solution-636012d3
  (fn powerset [coll]
    (letfn [(add-to-all [coll item]
              (into #{#{item}} (into coll (map #(conj % item) coll))))]
      (into #{#{}} (reduce add-to-all #{} coll)))))

(defcheck solution-636aa1fa
  (fn powerset [s] (letfn [(k [s1 i](into s1 (map #(conj % i) s1)))] (reduce k #{#{}} s))))

(defcheck solution-6446da54
  (fn [s]
    (let [items (into #{} (for [i s] #{i}))]
      (loop [acc items]
        (if (or (empty? acc) (contains? acc s))
          (into #{#{}} acc)
          (recur (into acc
                   (for [s1 acc, s2 items]
                     (clojure.set/union s1 s2)))))))))

(defcheck solution-64cee46e
  (fn [i]
    (reduce
      (fn [o v]
        (clojure.set/union o (map #(conj % v) o)))
      #{#{}}
      i)))

(defcheck solution-64ee5c7
  (fn [xs]
    (letfn [(powerset [xs acc]
              (if (empty? xs)
                acc
                (concat (conj acc #{(first xs)})
                        (reduce #(conj % %2 (conj %2 (first xs)))
                          #{}
                          (powerset (rest xs) acc)))))]
      (set (conj (powerset xs #{}) #{})))))

(defcheck solution-64feb99f
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (clojure.set/union (ps (rest s))
                     (into #{} (map #(conj % (first s)) (ps (rest s))))))))

(defcheck solution-65cb4de1
  (fn [s]
    (->> #{ #{} }
      (iterate #(->> %
                  (mapcat (fn [x]
                            (for [e (disj s x)]
                              (conj x e))))
                  set))
      (take (inc (count s)))
      (apply concat #{s})
      set)))

(defcheck solution-65cd29dd
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (let [n (ps (next s))]
                     (clojure.set/union n (map #(conj % (first s)) n))))))

(defcheck solution-669b429
  (fn [s]
    (let [v (vec s) c (count s)
          ]
      (apply hash-set (for [n (range (bit-shift-left 1 c))]
                        (apply hash-set (map #(v %)(filter #(bit-test n %) (range c))))
                        )))
    ))

(defcheck solution-6710f5d6
  (fn[coll]
    (set (reduce
           (fn [coll e]
             (concat
              (map (comp set (partial cons e)) coll)
              coll))
           #{#{}} coll))))

(defcheck solution-674e3781
  (fn ps [xs]
    (if-let [[x & xs] (seq xs)]
      (let [pxs (ps xs)]
        (->> pxs
          (map #(conj % x))
          (concat pxs)
          set))
      #{#{}})))

(defcheck solution-67549360
  (fn power-set
    [a-set]
    (loop [[x & xs] (vec a-set) res #{#{}}]
      (if x
        (recur xs (clojure.set/union res (map #(conj % x) res)))
        res))))

(defcheck solution-680e12be
  (fn powerset [S]
    (if (empty? S) #{#{}}
                   (reduce #(into %1
                              #{%2 (into %2 #{(first S)})})
                     #{} (powerset (rest S))))))

(defcheck solution-6831867d
  (fn power-set-- [coll]
    (if (empty? coll)
      #{(set coll)}
      (let [part_res (power-set-- (rest coll))]
        (set (concat (map #(set (cons (first coll) %1)) part_res) part_res))))))

(defcheck solution-684d2f51
  (fn ps [s]
    (if-let [[head & tail] (seq s)]
      (let [tail-solutions (ps tail)]
        (into tail-solutions (map #(conj % head) tail-solutions)))
      #{#{}})))

(defcheck solution-69bb32f4
  (fn [s]
    (reduce
      (fn [init e]
        (set (concat init (map #(conj % e) init) [#{e}])))
      #{#{}} s)))

(defcheck solution-6a0aa47d
  (fn p [s]
    (reduce (fn [a e] (into a (map #(conj % e) a)))
      #{#{}} s)))

(defcheck solution-6a5e5826
  (fn [myset]
    (reduce
      (fn
        [prev item]
        (set (concat prev (for [x prev y item] (conj x y))))
        )
      #{#{}}
      (replicate (count myset) myset)
      )
    ))

(defcheck solution-6a81271e
  (fn
    [s]
    (reduce #(into % (map (fn [p] (conj p %2 )) %)) #{#{}} s)))

(defcheck solution-6c34c34c
  (fn __ [coll]
    ((fn dfs [l r]
       (cond
         (empty? l) #{r}
         :else (apply merge (dfs (rest l) r) (dfs (rest l) (merge r (first l))))))
     coll #{})))

(defcheck solution-6c39f412
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [x (first s) r (disj s x)]
        (clojure.set/union
          (set (map #(conj % x) (power-set r)))
          (power-set r))))))

(defcheck solution-6c6ed210
  (fn ps [s]
    (set (reduce (fn [r x]
                   (concat (conj r #{x})
                           (map #(conj % x) r)))
           #{#{}}
           s))))

(defcheck solution-6cdd555
  (fn powerset [xs]
    (loop [xs (seq xs) result #{#{}}]
      (if-let [[x & more] xs]
        (recur more (into result (for [set result] (conj set x))))
        result))))

(defcheck solution-6d7f0619
  (letfn [(subsets [n items]
            (cond
              (= n 0) '(())
              (empty? items) '()
              :else (concat (map
                              #(cons (first items) %)
                              (subsets (dec n) (rest items)))
                            (subsets n (rest items)))))]
    (fn [coll]
      (->>
        (reduce #(concat %1 (subsets %2 (seq coll))) [] (range 1 (inc (count coll))))
        (cons '())
        (map set)
        (set)))))

(defcheck solution-6d84c43b
  (fn [s]
    (let [len (bit-shift-left 1 (count s))
          m (zipmap (range) (vec s))]
      (set
        (map
          (fn [i] (set (vals (filter #(bit-test i (% 0)) m))))
          (range len))))))

(defcheck solution-6dd85b5c
  (fn power-set [s]
    (loop [res #{#{}} left-over s]
      (if (= left-over [])
        res
        (let [fst (first left-over)]
          (recur (reduce #(conj %1 (if (= fst nil) %2 (conj %2 (first left-over)))) res res) (rest left-over)))))))

(defcheck solution-6e06b969
  (fn [xs]
    (let [f (fn [i xs acc]
              (if (= i 0) acc
                          (let [i' (quot i 2) xs' (rest xs)]
                            (recur i' xs' (if (odd? i) (conj acc (first xs)) acc)))))]
      (->> (repeat (count xs) 2) (reduce *) range (map #(f % xs #{})) set))))

(defcheck solution-6e73f0b
  reduce #(into #{} (concat % (for [x %] (conj x %2)))) #{#{}})

(defcheck solution-6eac0ba4
  (fn [sets]
    (loop [s sets r (conj #{sets} #{})]
      (if (empty? s) r
                     (recur (rest s) (into r (set (map #(conj % (first s)) r))))))))

(defcheck solution-6ec1f0e7
  (fn solve [s]
    (set
      (reduce (fn [res x]
                (concat res
                        (map #(set (conj % x)) res)))
        #{#{}}
        s))))

(defcheck solution-6f70c253
  (fn power-set [a-set]
    (loop [start-set #{#{}} a-seq (seq a-set)]
      (let [f (first a-seq) r (rest a-seq)
            set-slice (map #(conj % f) start-set)
            ]
        (if f
          (recur (into start-set set-slice) r)
          start-set
          )
        )
      )
    ))

(defcheck solution-6fb6fcfc
  (fn power-sets [s]
    (if (empty? s)
      #{#{}}
      (let [element-list (apply list s)
            one-element (first element-list)
            power-sets-of-others (power-sets (set (rest element-list)))]
        (clojure.set/union power-sets-of-others
          (set (map #(conj % one-element)
                 (apply list power-sets-of-others))))))))

(defcheck solution-6fb95483
  (fn powerset
    [coll]
    (let [x (first coll) xs (rest coll)]
      (if (empty? coll)
        #{#{}}
        (let [pset (powerset xs)]
          (clojure.set/union pset (map #(into #{x} %) pset)))))))

(defcheck solution-715ac7b0
  (fn power-set [full-set]
    (if (empty? full-set)
      #{#{}}
      (let [rest-pset (power-set (rest full-set))]
        (into rest-pset (map #(conj % (first full-set)) rest-pset))))))

(defcheck solution-71c86516
  #(letfn [
           (subvecs[v]
             (into #{} (for [n (range (count v))] (remove (fn[x](= (nth v n) x)) v)))
             )

           (power-vec[coll]
             (loop [n (count coll) c1 [coll] r [coll []]]
               (if (<= n 1)
                 r
                 (let [c2 (distinct (mapcat subvecs c1))]
                   (recur (dec n) c2 (into r c2))
                   )
                 )
               )
             )
           ]

     (into #{} (map (fn[v](into #{} v)) (power-vec (vec %))))
     ))

(defcheck solution-71e329bd
  (fn [s] (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) #{#{}} s))))

(defcheck solution-72027f9f
  (fn [s]
    (loop [ss #{s} n (count s)]
      (if (zero? n)
        ss
        (recur
          (into #{s} (mapcat (fn [z] (map #(disj z %) z)) ss))
          (dec n) )))))

(defcheck solution-72260b04
  (fn p [s]
    (if (= #{} s) #{#{}}
                  (let [res (p (disj s (first s)))]
                    (into res (map #(conj % (first s)) res))))))

(defcheck solution-7252673
  (fn [x]
    (let [len (count x)
          set-len (bit-shift-left 1 len)
          xs (map vector (range len) (seq x))]
      (set
        (map
          (fn [mask]
            (set
              (map second (filter #(bit-test mask (first %)) xs))))
          (range set-len))))))

(defcheck solution-72a72452
  (fn
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
    ))

(defcheck solution-73747716
  (fn [xx]
    (set((fn [x r i]
           (let [c (count x) f (fn [s] (set(map (comp set #(remove % x)) s))) test (fn test [result src number]
                                                                                     (if (= 0 number)
                                                                                       [#{}]
                                                                                       (let [inner (fn inner [iresult index]
                                                                                                     (if (> index (- (count src) number))
                                                                                                       iresult
                                                                                                       (recur (concat iresult (map #(conj % (nth src index)) (test [] (drop (+ 1 index) src) (- number 1))))
                                                                                                         (+ 1 index))))]
                                                                                         (inner [] 0))))]
             (if (< c (* 2 i))
               r
               (let [os (test [] x i)]
                 (recur x (concat r os (f os)) (+ 1 i))))))(into [] xx) [] 0))))

(defcheck solution-73c0df9
  (fn powerset [coll]
    (if (empty? coll)
      #{#{}}
      (let [x (first coll)
            xs (set (rest coll))
            ps-of-xs (powerset xs)]
        (clojure.set/union ps-of-xs (set (map #(conj % x) ps-of-xs)))))))

(defcheck solution-744fd051
  (fn [s]
    (reduce (fn [ps e] (into ps (map #(conj % e) ps)))
      #{#{}}
      s)))

(defcheck solution-7464920c
  (partial
    reduce (fn [a x]
             (into a (map #(conj % x) a)))
    #{#{}}
    ,,,))

(defcheck solution-74826932
  (fn p [s]
    (if (seq s)
      (let [pivot (first s)
            resp-parcial (p (disj s pivot))]
        (clojure.set/union #{s}
          resp-parcial
          (set (map #(conj % pivot) resp-parcial))))
      #{#{}})))

(defcheck solution-74b73a61
  (fn tmp ([s] (tmp s #{}))
    ([s accum]
     #_(println s ":" accum)
     (if (empty? s) (conj accum #{})
                    (let [t1 (conj accum #{})
                          t2 (reduce #(conj %1 (conj %2 (first s))) t1 t1)
                          t3 (conj t2 (set (rest s)))
                          t4 (conj t3 (set s))]
                      (tmp (rest s) t4))))))

(defcheck solution-7525f463
  #(if-not %
     #{%}
     (reduce (fn[a _]
               (into a (for [e %
                             x a
                             :when(not(x e))]
                         (conj x e))))
       #{#{}}
       %)))

(defcheck solution-754ef79a
  (fn [s] (loop [sts  #{ #{} } coll s]
            (if (empty? coll) sts
                              (recur (clojure.set/union sts
                                       (set (map (fn [c] (conj c (first coll)) )
                                              sts)))
                                (rest coll))))))

(defcheck solution-75969606
  (fn power-set [s]
    (set
      (map (fn [bits]
             (set
               (keep-indexed
                 (fn [i elem]
                   (if (some #(= i %) bits)
                     elem))
                 s)))
        (map (fn [x]
               (vec
                 (filter
                   #(bit-test x %1)
                   (range (count s)))))
          (range (Math/pow 2 (count s))))))))

(defcheck solution-75bd725a
  (fn __ [s]
    (let [[f & r] (vec s)]
      (cond
        (nil? f) #{#{}}
        (nil? r) #{#{} s}
        :else
        (set (concat #{#{} #{f} s}
                     (__ (set r))
                     (map #(set (cons f %)) (__ (set r)))))))))

(defcheck solution-75f0760c
  (fn power-set [s]
    (set
      (reduce
        (fn [accu item]
          (->> accu
            (map #(conj % item))
            (concat accu)))
        [#{}] s))))

(defcheck solution-760bdcef
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (let [x (first s), r (rest s)]
                     (if (empty? r) #{#{} #{x}}
                                    (clojure.set/union
                                      #{#{x}}
                                      (ps r)
                                      (set (map #(into % #{x}) (ps r)))))))))

(defcheck solution-761d7a04
  (fn f [s]
    (if (empty? s) #{#{}}
                   (let [n (f (next s))]
                     (into n (map #(conj % (first s)) n))))))

(defcheck solution-76523a7b
  (fn powerset [xs]
    (if (empty? xs)
      #{#{}}
      (let [e    (first xs)
            xs'  (into #{} (remove #{e} xs))
            xs'' (powerset xs')]
        (into xs'' (map #(conj % e) xs''))))))

(defcheck solution-76a41d44
  (fn [s]
    (into #{}
      (for [x (range (Math/pow 2 (count s)))]
        (into #{}
          (keep-indexed #(if (bit-test x %1) %2) s))))))

(defcheck solution-76f69b5b
  (fn [s]
    (reduce
      #(clojure.set/union %1 (into #{} (for [subset %1] (conj subset %2))))
      #{#{}}
      s)))

(defcheck solution-776133d4
  (fn p [s]
    (reduce #(into % (for [sub %] (conj sub %2))) #{#{}} s)))

(defcheck solution-7786b8a6
  (fn [s]
    (set (reduce (fn [a b] (mapcat (fn [x] [x (conj x b)] ) a)) [#{}] s
           ))
    ))

(defcheck solution-785581fd
  (fn [s]
    (reduce (fn [acc v]
              (clojure.set/union
                acc
                (map #(conj % v) acc)))
      #{#{}}
      s)))

(defcheck solution-7929796a
  (fn [xs]
    (letfn [(branch [seed items]
              (if (empty? items)
                [(set seed)]
                (lazy-seq (cons (set seed)
                            (mapcat #(branch (conj seed %) (drop %2 items))
                              items
                              (range 1 (inc (count items))))))))]
      (set (branch #{} (seq xs))))))

(defcheck solution-7974d208
  (comp set
        (fn p [[f & r]] (if f (concat (p r) [#{f}] (map #(conj % f) (p r)))
                              [#{}]))
        seq))

(defcheck solution-797eb150
  (fn[a-set]
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
      (set (swap! p-seq conj #{})))))

(defcheck solution-7989c36
  (fn prob85
    [xs]
    (set (map set
           (if (empty? xs)
             '(())
             (clojure.set/union (prob85 (next xs))
               (set (map #(conj % (first xs)) (prob85 (rest xs)))))
             )))))

(defcheck solution-79c9833d
  (fn power-set [s]
    (let [ps (fn [set el]
               (clojure.set/union
                 set
                 (reduce #(conj %1 (conj %2 el)) #{} set)))]
      (reduce ps #{#{}} s))))

(defcheck solution-79d351ec
  (fn p [c]
    (set
      (if-let [[f & r] (seq c)]
        (concat
         (map #(conj % f) (p r))
         (p r))
        [#{}]))))

(defcheck solution-79db8ea2
  (fn allcombi [setan]
    (if (empty? setan)
      #{#{}}
      (let [lim (count setan)
            cont1 (into #{} (map #(into #{} [%]) setan))
            join (fn [starget s]
                   (into #{} (map #(clojure.set/union starget %) s)))]
        (loop [i 1 res cont1]
          (if (= lim i)
            (conj res #{})
            (recur (inc i)
              (into #{}
                (mapcat #(join % cont1) res)))))))))

(defcheck solution-7a2f57bd
  (fn power-set [s]
    (if (seq s)
      (let [ss (power-set (rest s))
            f (first s)]
        (clojure.set/union ss (set (map #(conj % f) ss))))
      #{#{}})))

(defcheck solution-7ab06d39
  (fn [x]
    (loop [s x
           a #{s}]
      (if (= (apply * (repeat (count s) 2)) (count a))
        a
        (recur s
          (reduce
            (fn [i j]
              (into i
                (for [k j]
                  (set (remove #{k} j)))))
            a
            a))))))

(defcheck solution-7b029b40
  (fn e [s]
    "This is what I wanted to do but way more concise"
    (reduce
      (fn [r i]
        (conj r
          (set
            (keep-indexed
              #(when (bit-test i %1) %2) s))))
      #{#{}}
      (range  1 (bit-shift-left 1 (count s))))))

(defcheck solution-7b2d9a01
  (fn [v]
    (letfn [(add
              [coll s n]
              (for [x coll :when (= n (count x)) y s]
                (conj x y)))]
      (loop [i 0 sol #{#{}}]
        (if (= i (count v)) sol
                            (recur (inc i) (apply conj sol (add sol v i))))))))

(defcheck solution-7b343cd1
  (fn [superset]
    (apply
      clojure.set/union
      (take (inc (count superset))
        (iterate
          (fn [sets]
            (set
              (mapcat (fn [s] (map #(disj s %)
                                s))
                sets)))
          #{superset})))))

(defcheck solution-7b7f3b0b
  (fn [coll]
    (reduce (fn [s x] (into s (map #(conj % x) s)))
      #{#{}}
      coll)))

(defcheck solution-7ba3c79d
  (fn pow [s]
    (letfn [(add-to-each [e S]
              (into #{} (map #(conj % e) S)))]

      (if (empty? s)
        #{#{}}
        (let [e (first s)
              T (disj s e)
              powT (pow T)]
          (clojure.set/union powT
            (add-to-each e powT)))))))

(defcheck solution-7c2f406a
  (fn power-set [s]
    (letfn [

            (sp-set [s] (reduce

                          (fn [a x]
                            (conj a (disj s x)))

                          #{} s))


            ]

      (conj (loop [a #{} s #{s}]
              (let [[ar,rr] (reduce (fn [[a,ns] x]
                                      (let [r (sp-set x)]
                                        [(into a r), (into ns r)]))

                              [a,#{}] s)]
                (if (= rr #{})
                  ar
                  (recur ar rr)))) s)
      )))

(defcheck solution-7cc14070
  (fn p
    [d]
    (letfn [(pp [d1 d2]
              (into #{}
                (for [i d1
                      j d2]
                  (conj j i))))]
      (loop [i (count d)
             r (set (map #(set (vector %)) d))]
        (if (zero? i)
          (conj r #{})
          (recur (dec i) (pp d r)))))))

(defcheck solution-7d3fa185
  (fn [s]
    (reduce (fn [ps x]
              (reduce (fn [ps s]
                        (conj ps (conj s x))) ps ps)) #{#{}} s)))

(defcheck solution-7d8d874d
  (fn [s]( if(empty? s) #{s}
                        (loop [ x s y 0 cnt 0]
                          ;(println x (count x) y cnt)
                          (if (= y (count x)) x
                                              (recur
                                                ;x
                                                (conj (set(flatten(map
                                                                    (fn[i]( map
                                                                            (fn[j](
                                                                                    cond
                                                                                    (and (set? i) (set? j)) (clojure.set/join i j)
                                                                                    (set? i) (conj i j)
                                                                                    (set? j) (conj j i)
                                                                                    :else (set [i j])
                                                                                    ))
                                                                            x
                                                                            ))
                                                                    s))) #{} s)
                                                ;y
                                                (count x)
                                                ;cnt
                                                (inc cnt)
                                                )
                                              ))
                        )))

(defcheck solution-7dce1ca9
  (fn [s]
    ((fn [s ps]
       (if (empty? s) ps
                      (recur (rest s)
                        (reduce conj ps
                          (map #(conj % (first s)) ps)))))
     s #{#{}})))

(defcheck solution-7e080fb2
  reduce (fn [r e] (into r (map #(conj % e) r))) #{#{}})

(defcheck solution-7e0bb834
  (fn [s]
    (letfn [(exp2 [n] (reduce * (repeat n 2)))
            (subs-n
              ([n s] (subs-n #{} n (vec s)))
              ([acc n [x & xs]]
               (if (zero? n)
                 acc
                 (if (zero? (mod n 2))
                   (recur acc (quot n 2) xs)
                   (recur (conj acc x) (quot n 2) xs)))))]
      (set (map #(subs-n % s) (range (exp2 (count s))))))))

(defcheck solution-7e361a82
  (fn power [s]
    (letfn [(f [e t] (reduce #(conj %1 (conj %2 e)) #{} t))]
      (if (empty? s) #{s}
                     (let [e (first s) t (disj s e)]
                       (clojure.set/union (power t) (f e (power t))))))))

(defcheck solution-7e992146
  (fn __
    ([x]
     (__ x (map (fn [y] #{y}) x)))
    ([x y]
     (let [z (into #{} (for [a x b y] (clojure.set/union #{a} b)))]
       (if (or (= z #{})(contains? z x))
         (conj z #{})
         (__ x z))))))

(defcheck solution-7f45c695
  (fn __ [s]
    (if (empty? s) #{#{}}
                   (let [f (first s)
                         r (__ (rest s))]
                     (apply conj r (map #(conj % f) r))))))

(defcheck solution-7f672329
  (fn power-set
    [s]
    (if (empty? s)
      #{s}
      (let [e (first s)
            smaller-set (disj s e)]
        (set (concat (power-set smaller-set) (for [o (power-set smaller-set)] (conj o e))))))))

(defcheck solution-7f6fef65
  reduce (fn [a x] (into a (map #(into #{x} %) a))) #{#{}})

(defcheck solution-7fb8ecaf
  (fn ps [s]
    (loop [a #{#{}} [h & t] (seq s)]
      (if (nil? h) a
                   (recur
                     (into a  (for [x a] (conj x h)) )
                     t)  ))))

(defcheck solution-7fc3b0cb
  (fn [s]
    (reduce (fn [result next]
              (clojure.set/union result
                (map #(conj % next)
                  result)))
      #{#{}}
      s)))

(defcheck solution-7fd2b313
  reduce (fn [s a]
           (clojure.set/union
             s
             (set (map #(clojure.set/union #{a} %) s)))) #{#{}})

(defcheck solution-8014190c
  (fn [s]
    (set (reduce (fn [acc x]
                   (concat
                    acc
                    (map (fn [acc-x] (conj acc-x x)) acc)))
           #{#{}} s))))

(defcheck solution-801e3c6b
  (fn powerset [xs]
    (if (empty? xs) (hash-set xs)
                    ( let [x (first xs)
                           ys (powerset (set (rest xs)))]
                      (clojure.set/union ys
                        (set (map #(conj % x) ys)))))))

(defcheck solution-804578dc
  (fn powerset [a-set]
    (set
      (cond
        (empty? a-set) #{#{}}
        :else
        (let [firstE (first a-set)
              multipleAppend (fn multipleAppend [elem a-sets]
                               (if (empty? a-sets)
                                 #{}
                                 (conj (multipleAppend elem (rest a-sets)) (conj (first a-sets) elem))))
              powerRest (powerset (rest a-set))
              firstPower (multipleAppend firstE powerRest)]
          (concat powerRest firstPower))))))

(defcheck solution-8079f620
  (fn power-set [superset]
    (reduce (fn [base-set item]
              (into base-set (map
                               (fn [s] (conj s item))
                               base-set)))
      #{#{}}
      superset)))

(defcheck solution-808c4a8c
  (fn powerset [s]
    (if (empty? s) '#{#{}}
                   (let [sp (powerset (next s))]
                     (clojure.set/union sp (map #(set (conj % (first s))) sp))))))

(defcheck solution-80ad1926
  (fn power [x]
    (if (seq x)
      (let [head (first x)
            others (power (disj x head))]
        (clojure.set/union
          others
          (map #(conj % head) others)))
      #{#{}})
    ))

(defcheck solution-813d2fc0
  (fn [s]
    (let [bit-filter (fn [items mask]
                       (set
                         (keep-indexed
                           (fn [idx item]
                             (if (bit-test mask idx)
                               item))
                           items)))]
      (set (for [mask (range 0 (Math/pow 2 (count s)))]
             (bit-filter s mask))))))

(defcheck solution-81b0e75e
  (fn superset
    [s]
    (let [seed (set (map #(set [%]) s))]
      (letfn [(helper [slow]
                (mapcat (fn [c]
                          (map #(set (concat c %)) seed))
                  slow))]
        (set
          (loop [i (inc (count s)) step seed result #{#{}}]
            (if (zero? i)
              result
              (let [stage (set (helper step))
                    result (set (concat result stage))]
                (recur (dec i)
                  stage
                  result)))))))))

(defcheck solution-82c2e27c
  (fn [s]
    (let [indexed (vec (map-indexed (fn [i x] [i x]) s))
          bit-permutations (range (Math/pow 2 (count s)))
          gen-set-from-bit-permutation (fn [p]
                                         (letfn [(conj-matching [acc [i x]]
                                                   (if (bit-test p i)
                                                     (conj acc x)
                                                     acc))]
                                           (reduce conj-matching #{} indexed)))]
      (set (map gen-set-from-bit-permutation bit-permutations)))))

(defcheck solution-830f2d2
  (fn [l]
    (set
      (reduce (fn [i e]
                (concat (map #(conj % e) i) i))
        [#{}] l))))

(defcheck solution-83c490d0
  (fn [s]
    (reduce #(into %1 (map (fn [s'] (conj s' %2)) %1)) #{#{}} s)))

(defcheck solution-83d1d9d6
  (fn p [xs]
    (let [f (first xs)
          r (disj xs f)]
      (if (seq xs)
        (set (concat (map #(conj % f) (p r)) (p r)))
        #{#{}}))))

(defcheck solution-83e92615
  (fn psets
    ([xs] (psets xs #{}))
    ([xs sofar]
     (if (empty? xs)
       #{sofar}
       (set (concat (psets (rest xs) (conj sofar (first xs)))
                    (psets (rest xs) sofar)))))))

(defcheck solution-85b36cf5
  (fn [s]
    (loop [xs (seq s)
           so-far #{#{}}]
      (if-let [[x & xs] xs]
        (->>
          (for [s so-far]
            [s (conj s x)])
          (apply concat)
          (into #{})
          (recur xs))
        so-far))))

(defcheck solution-872b65f2
  (fn [a] (reduce (fn [s x] (apply conj s (map #(conj % x) s)) ) #{#{}} a)))

(defcheck solution-87630e2e
  (fn p-set
    [s]
    (let [ret (into #{} (map #(set (list %)) s))
          f (fn [acc p]
              (if (empty? p) (conj acc #{})
                             (recur
                               (into acc
                                 (map #(into % (list (first p))) acc)) (rest p))))]
      (f ret s))))

(defcheck solution-87757641
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (set (reduce #(conj %1 %2 (conj %2 (first s))) #{} (p (rest s)))))))

(defcheck solution-877f1cee
  (fn [st]
    (letfn [(subsets [n items]
              (cond
                (= n 0) #{#{}}
                (empty? items) #{}
                :else (concat (map
                                #(cons (first items) %)
                                (subsets (dec n) (rest items)))
                              (subsets n (rest items)))))]
      (let [allsubsets (for [i (range (inc (count st)))] (subsets i st))]
        (reduce #(into %1 (map set %2)) #{} allsubsets)))))

(defcheck solution-87e66f5a
  reduce (fn [P s] (into P (map #(conj % s) P))) #{#{}})

(defcheck solution-87fa5791
  (letfn [(P [s]
            (if (empty? s) [#{}]
                           (let [ps (P (rest s))]
                             (concat ps
                                     (map #(conj % (first s)) ps)))))]
    (fn [s]
      (into #{} (P s)))))

(defcheck solution-88334cc7
  (fn [is]
    (reduce
      (fn [ps i]
        (reduce
          (fn [ps p]
            (conj ps (conj p i)))
          ps ps))
      #{#{}} is)))

(defcheck solution-883f2132
  (fn
    [coll]
    (reduce (fn [r v] (into r(reduce #(conj %1 (conj %2 v)) #{} r))) #{#{}} coll)))

(defcheck solution-88472331
  (letfn [
          (aldoniEl [e aa]  (set (for [i  aa ] (conj  i e) ))  )]
    (fn ps [s] (cond
                 (empty? s) #{#{}}
                 true (clojure.set/union (aldoniEl (first s) (ps (rest s))) (ps (rest s)))))
    ))

(defcheck solution-887d5e2
  (fn [s]
    (reduce
      (fn [a b]
        (into a (map #(conj % b) a)))
      #{#{}}
      s)))

(defcheck solution-88cee217
  (fn [coll] (letfn [
                     (n-tuples [coll n]
                       (cond
                         (not coll) '()
                         (= 0 n) (list '())
                         (= 1 n) (map list coll)
                         (= n (count coll)) (list coll)
                         :else (concat (map #(cons (first coll) %) (n-tuples (next coll) (dec n)))
                                       (n-tuples (next coll) n))))]
               (set (map set
                      (apply concat
                        (for [i (range (inc (count coll)))]
                          (n-tuples coll i))))))))

(defcheck solution-897024ab
  (fn powerset [items]
    (if (empty? items)
      #{#{}}
      (clojure.set/union (powerset (next items))
        (map #(conj % (first items)) (powerset (next items)))))))

(defcheck solution-897154a8
  (fn [s]
    (reduce
      (fn [m v]
        (into m (map #(conj % v) m)))
      #{#{}} s)))

(defcheck solution-8a4ecbda
  (fn [s]
    (letfn [(thingaddf [ps leftover] (map #(conj % (first leftover)) ps))
            (thingadder [[ps leftover]]
              [(set (conj (concat ps (thingaddf ps leftover)) #{(first leftover)}))
               (rest leftover)])]
      (-> (drop-while #(not-empty (second %))
            (iterate thingadder [#{#{}} s]))
        first first))))

(defcheck solution-8a76dfed
  (fn power-set
    [s]
    (if (zero?(count s))
      #{#{}}
      (let [prest (power-set (clojure.set/difference s #{(first s)}))]
        (clojure.set/union #{#{}}
          (set (map (partial clojure.set/union #{(first s)}) prest))
          prest)))))

(defcheck solution-8a97f67d
  (fn power-set
    [s]
    (let [s-count (count s)
          ps-count (bit-shift-left 1 s-count)
          elements (vec s)]
      (into #{} (for [n (range 0 ps-count)]
                  (->> (range 0 s-count)
                    (remove #(zero? (bit-and n (bit-shift-left 1 %))))
                    (map elements)
                    set))))))

(defcheck solution-8b66ba27
  (fn [orig]
    (reduce (fn [pset cset]
              (reduce (fn [pset curr]
                        (conj pset (conj curr cset))) pset pset)) #{#{}} orig)))

(defcheck solution-8b7e9c03
  (fn [s] (letfn [(f [c] (map #(conj c %) s))]
            (loop [p #{#{}}] (if (p s) p (recur (into p (mapcat f p))))))))

(defcheck solution-8c21f2e6
  (fn[x](reduce (fn[a b](reduce #(conj %1 (conj %2 b))  a a))  #{#{}} x)))

(defcheck solution-8c4f535f
  (fn [the-set] (->> (iterate (fn [x]
                                (into #{}
                                  (apply concat
                                    (for [entry the-set]
                                      (for [item x
                                            :let [result (conj item entry)]]
                                        result))))) #{#{}})
                  (take (count the-set))
                  (apply clojure.set/union)
                  (#(conj % the-set)))))

(defcheck solution-8d1ba859
  (fn [st]
    (letfn [(expand [[a b]]
              (when (seq b) (cons [(conj a (first b)) (rest b)] (expand [a (rest b)]))))
            (combos [n s]
              (if (zero? n) (list [[] s]) (mapcat expand (combos (dec n) s))))
            (combinations [n s]
              (map first (combos n s)))
            (partitions [s] (mapcat #(combinations % s) (range (inc (count s)))))]
      (->> st partitions (map set) set))))

(defcheck solution-8d53579d
  (fn [s] (apply hash-set (reduce (fn [s v] (concat s (map #(conj % v) s))) #{#{}} s))))

(defcheck solution-8daf579d
  (fn [ss] (->> (map (fn [v] #(->> v (conj %2) (conj %1))) ss)
             (reduce #(reduce %2 %1 %1) #{#{}}))))

(defcheck solution-8df96ada
  (fn [s]
    (loop [i (count s) r #{#{}}]
      (if (= i 0)
        r
        (recur (dec i) (set (concat r (mapcat (fn [x] (map #(conj x %) (apply disj s x))) r))))))))

(defcheck solution-8e3b0a60
  (fn powerset [coll]
    (reduce (fn [res next]
              (clojure.set/union res (map #(conj % next) res)))
      #{#{}}
      coll)))

(defcheck solution-8e68d9ff
  (fn powerset [s]
    (reduce (fn [p x] (set (concat p (map #(conj % x)  p)))) #{#{}} s)))

(defcheck solution-8f9d704a
  (letfn [(power-set [set]
            (if (zero? (count set))
              #{#{}}
              (let [elt (first set)
                    subset (rest set)
                    sub-power-set (power-set subset)]
                (concat sub-power-set (map #(conj % elt) sub-power-set)))))]
    (fn [s] (set (power-set s)))))

(defcheck solution-9001dfd2
  (fn [coll]
    (reduce (fn [acc e] (into acc (map #(conj % e) acc))) #{#{}} coll)))

(defcheck solution-9036dcde
  (fn power-set [a-set]
    (let [a-vec (vec a-set)]
      (loop [index 0
             set-of-sets #{#{}}]
        (let [curr-val (get a-vec index)]
          (if (< index (count a-vec))
            (recur (inc index)
              (reduce (fn [power-set subset] (conj power-set (conj subset curr-val)))
                set-of-sets
                set-of-sets))
            set-of-sets))))))

(defcheck solution-904c6d96
  (fn power-set [s]
    (let [length (count s)
          m (zipmap (range) s)]
      (set
        (for [i (range (apply * (repeat length 2)))]
          (set
            (for [j (range length)
                  :when (bit-test i j)]
              (m j))))))))

(defcheck solution-9065c98c
  (fn soln [s]
    (if (empty? s)
      #{#{}}
      (let [subset (soln (rest s))]
        (into subset (map #(conj % (first s)) subset))))))

(defcheck solution-908c9e05
  (fn [s]
    (loop [all #{s}]
      (let [sets (fn [x] (map #(disj x %) x))
            sub (set (apply concat all (map sets all)))]
        (if (= sub all) sub (recur sub))))))

(defcheck solution-90a2cc68
  (fn powset [s]
    (if (empty? s) #{#{}}
                   (->>
                     (powset (rest s))
                     (mapcat #(set [% (conj % (first s))]))
                     set))))

(defcheck solution-9166ed3b
  (fn powerset [xs]
    (reduce
      (fn [a b]
        (set (concat (map #(conj % b) a) a))
        ) #{#{}} xs
      )))

(defcheck solution-92472121
  reduce (fn [a e] (apply conj a #{e} (map #(conj % e) a))) #{#{}})

(defcheck solution-924db3b8
  (fn power-set
    [s]
    (if (empty? s)
      #{#{}}
      (let [atoms (into #{} (map (fn [e] #{e}) s))]
        (loop [res #{s}
               c (count s)]
          (if (= c 1)
            (clojure.set/union res atoms #{#{}})
            (recur (clojure.set/union res
                     (into #{} (flatten
                                 (map (fn [supers]
                                        (map (fn [e] (clojure.set/difference supers e))
                                          atoms))
                                   (filter #(= (inc c) (count %)) res)))))
              (dec c))))))))

(defcheck solution-926cd533
  (fn [s] (set (reduce (fn [s v] (concat s (map #(conj % v) s))) [#{}] s))))

(defcheck solution-92d6412f
  (fn [coll]
    (let [n (count coll)
          mult (map #(int (Math/pow 2 %)) (take n (range)))
          int2set (fn [x coll] (into #{} (filter #(not (nil? %))
                                           (map #(when %1 %2)
                                             (map #(> (bit-and x %) 0) mult) coll))))]
      (into #{} (map #(int2set % coll) (take (Math/pow 2 n) (range)))))))

(defcheck solution-936939f4
  (fn [coll] (reduce (fn [a b] (set (concat a (map #(set (concat #{b} %)) a)))) #{#{}} coll)))

(defcheck solution-936d9926
  (fn [s] (reduce (fn [ac b] (into ac (map #(conj % b) ac))) #{#{}} s)))

(defcheck solution-94511439
  (fn p [s]
    (if (empty? s) #{#{}}
                   (let [t (p (rest s))]
                     (into t
                       (map #(conj % (first s)) t)))
                   )))

(defcheck solution-94a6e8b3
  (fn powerset [l]
    (if (empty? l)
      #{#{}}
      (let [ps (powerset (rest l))]
        (apply conj ps (map (fn [x] (conj x (first l))) ps))))))

(defcheck solution-952ecc71
  (fn power-set [s]
    (if (empty? s)
      #{s}
      (let [head (first s)
            tail (disj s head)
            subset (power-set tail)]
        (clojure.set/union
          subset
          (map #(clojure.set/union #{head} %) subset))))))

(defcheck solution-9539218a
  (fn [s]
    (reduce (fn [a i] (into a (map #(conj % i) a))) #{#{}} s)))

(defcheck solution-95a982af
  (fn [s]
    (let [ f (fn [x y] (set (concat x (map #(conj % y) x) (vector #{y}))))]
      (reduce f #{#{}} s))))

(defcheck solution-95e52196
  (fn [m]
    ((fn p [s]
       (if (contains? s #{})
         s
         (p (into s
              (into #{} (mapcat (fn [ss] (map #(disj ss %)ss)) s))))))
     #{m})))

(defcheck solution-96146d74
  (fn [s]
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
          (conj acc #{})
          (recur x (conj acc
                     (into #{} (map #(first %) (filter #(second %) (map #(vector % %2) x mask))))) (bi mask)))))))

(defcheck solution-969da5f8
  (fn power-set [ls]
    (if (empty? ls)
      #{#{}}
      (let [ps (power-set (rest ls))
            f (first ls)]
        (into ps (map #(conj % f) ps))))))

(defcheck solution-96ab2600
  (fn [s]
    (let [mem (atom {})
          swapmem (fn [args f_n]
                    (if-let [e (find @mem args)]
                      (val e)
                      (let [ret (f_n)]
                        (swap! mem assoc args ret)
                        ret)))
          f (fn f [fst rst result]
              (if (empty? rst)
                (conj result fst)
                (set
                  (mapcat
                    (fn [arg]
                      (let [n_fst (conj fst arg)
                            n_rst (disj rst arg)
                            n_result (conj result fst)]
                        (conj (swapmem n_fst #(f n_fst n_rst n_result)) fst)))
                    rst))))]
      (f #{} s #{}))))

(defcheck solution-96f044f1
  (fn f [xs]
    (set (reduce
           (fn [s v] (concat s (map #(conj % v) s)))
           [#{}]
           xs))))

(defcheck solution-97036017
  (fn f [a] (if (empty? a) #{#{}} (let [b (f (rest a))] (apply conj b (map #(conj % (first a)) b))))))

(defcheck solution-970f14b9
  (fn [s]
    (letfn [(one-less [s] (map #(disj s %) s))
            (power-set [s] (map one-less (set (flatten s))))]
      (set (flatten (take (inc (count s)) (iterate power-set [[s]])))))))

(defcheck solution-9795fd39
  (fn
    [coll]
    (reduce (fn [r e] (apply conj r (map #(conj % e) r))) #{#{}} coll)))

(defcheck solution-97de95e5
  (fn [t]
    (reduce
      (fn [a b] (clojure.set/union a
                  (set (map (fn [y] (clojure.set/union #{b} y)) a))))
      #{#{}} t)))

(defcheck solution-98190cf4
  (letfn [(bool-inc
            ([v]
             (bool-inc v (dec (count v))))
            ([v i]
             (when (>= i 0)
               (if (get v i)
                 (recur v (dec i))
                 (reduce conj
                   (conj (subvec v 0 i) true)
                   (repeat (- (count v) i 1) false))))))
          (pick [s v]
            (reduce (fn [acc [e b]]
                      (if b (conj acc e) acc))
              #{} (partition 2 (interleave s v))))
          (subsets
            ([coll]
             (subsets coll (vec (repeat (count coll) false))))
            ([coll v]
             (when v
               (lazy-seq
                 (cons (pick coll v) (subsets coll (bool-inc v)))))))]
    (fn power-set [s]
      (into #{} (subsets s)))))

(defcheck solution-985c6d9f
  (fn p [s]
    (if (empty? s) #{#{}}
                   (let [[x & r] (seq s)
                         t (p r)]
                     (into t (map #(conj % x) t))))))

(defcheck solution-98624042
  (fn mps [ls] (set
                 (if (empty? ls) '(#{})
                                 (clojure.set/union (mps (next ls))
                                   (map #(conj % (first ls)) (mps (next ls))))))))

(defcheck solution-986bf7c2
  (fn pick
    ([pn coll]

     (if (empty? coll )
       [()]
       (reduce (fn [r n]
                 (let [
                       pnn (concat pn [n])
                       left (apply disj (set coll) pnn)
                       ; xx (println :b pn n left (or (empty? pn) (> n (last pn))))
                       ]
                   (if (or (empty? pn) (> n (last pn)))
                     (concat r [pnn] (pick pnn left))
                     r)
                   )) [] coll)
       ))
    ([coll]
     (set
       (map (fn [itm]
              (set (map (fn [x]
                          (nth (apply vector coll) x)) itm )))
         (pick [] (range (count coll)) )
         ))
     )))

(defcheck solution-98eb9803
  (fn [s]
    (reduce #(into %1 (for [x %1] (conj x %2)) ) #{#{}} s)))

(defcheck solution-998680e4
  (fn powerset [x]
    (if (seq x)
      (let [s (powerset (rest x)) ]
        (clojure.set/union s
          (map (fn [p] (conj p (first x))) s)))
      #{#{}})))

(defcheck solution-999b63d7
  (fn P [S]
    (if (empty? S) #{S}
                   (let [e (first S)
                         T (disj S e)
                         PT (P T)
                         F (fn [e T] (set (map #(conj % e)  T)))]
                     (set (concat PT (F e PT)))))))

(defcheck solution-99a83b06
  (fn [s]
    (reduce (fn [s x] (into s (map #(conj % x) s))) #{#{}} s)))

(defcheck solution-99aad6c8
  (fn [xs]
    (reduce (fn [acc x]
              (into acc (map #(conj % x) acc)))
      #{#{}}
      xs)))

(defcheck solution-9a2d92fe
  (fn powerset [ls]
    (if (empty? ls) #{#{}}
                    (set (map set (clojure.set/union (powerset (next ls))
                                    (map #(conj % (first ls)) (powerset (next ls)))))))))

(defcheck solution-9a5084ca
  (fn [s]
    (reduce (fn [a b]
              (into a (map #(conj % b) a)))
      #{#{}}
      s)))

(defcheck solution-9a5b6462
  (fn power-set
    ([a] (power-set (seq a) #{#{}}))
    ([[f & r :as s] sets]
     (if (empty? s)
       sets
       (recur r
         (into sets
           (map #(conj % f) sets)))))))

(defcheck solution-9a9633ce
  (fn [s1]
    (loop [res [[]] c (count s1)]
      (if (zero? c)
        (set(map set res))
        (recur
          (set (map set (concat res (for [r (seq res) s (seq s1)] (conj r s)))))
          (dec c))
        )
      )))

(defcheck solution-9aa0ed24
  (fn [xs]
    (letfn [(fn85 ([r s]
                   (if (seq s)
                     (concat (fn85 r (rest s)) (fn85 (cons (first s) r) (rest s)))
                     (list r))))]
      (into #{}  (map (partial into #{}) (fn85 '() (into '() xs)))))))

(defcheck solution-9b2a5d7d
  (fn pset
    ([s] (pset #{#{}} s))
    ([a s]
     (if (empty? s)
       a
       (pset
         (into a (map #(conj % (first s)) a))
         (rest s))))))

(defcheck solution-9b434607
  (fn p [s] (if (empty? s) #{#{}} (let [e (first s) r (disj s e) pr (p r)] (clojure.set/union pr (into #{} (map #(conj % e) pr)))))))

(defcheck solution-9b63024c
  reduce (fn [s e] (into s (map #(conj % e) s))) #{#{}})

(defcheck solution-9b6d4168
  (partial reduce
    (fn [s x] (clojure.set/union s (map #(conj % x) s)))
    #{#{}}))

(defcheck solution-9ba5c8fd
  (fn power-set [xs]
    (if
     (empty? xs)
      #{#{}}
      (let
       [
        x (first xs)
        xss (power-set (rest xs))
        ]
        (set
          (concat
           xss
           (map
             (fn [v]
               (set
                 (conj v x)
                 )
               )
             xss
             )
           )
          )
        )
      )
    ))

(defcheck solution-9bbe6e8a
  (fn subsets [s]
    (if (empty? s)
      #{#{}}
      (let [subs (subsets (rest s))
            f (first s)]
        (clojure.set/union #{#{f}}
          subs
          (set (map #(conj % (first s)) subs)))))))

(defcheck solution-9becaba0
  (fn [xs]
    (letfn [(powerset [[x & xs]]
              (if x
                (concat (map (partial cons x) (powerset xs)) (powerset xs))
                [[]]))]
      (->> (powerset (into [] xs))
        (map set)
        set))))

(defcheck solution-9c2491cd
  (fn [x]
    (reduce (fn [ps elem]
              (into ps (map (fn [y] (conj y elem))
                         ps)))
      #{#{}}
      x)))

(defcheck solution-9c79eee3
  (fn power-set [s]
    (if (empty? s) #{#{}}
                   (let [rest-power-set (power-set (rest s))]
                     (clojure.set/union
                       (set (map #(conj % (first s)) rest-power-set))
                       rest-power-set)))))

(defcheck solution-9cab284b
  (fn [s]
    (let [sol (fn [ss ps]
                (let [as (set (for [p ps t p] (disj p t)))
                      iss (into ss as)]
                  (cond (>= 1 (count as)) iss
                        :else (recur iss as))))]
      (sol (hash-set s) (hash-set s)))))

(defcheck solution-9d495ec8
  (fn power-set [s]
    (if (empty? s) #{#{}}
                   (let [ps (power-set (rest s))]
                     (clojure.set/union ps (set (map #(conj % (first s)) ps)))))))

(defcheck solution-9d99f1eb
  (let
   ;; massively overkill, i could just count in binary t_t...
   [ps-with-model (fn [s model]
                    (reduce
                      (fn [ps m]
                        (let [v (vec s)]
                          (conj ps (set (mapv v m)))))
                      #{}
                      model))
    ps (fn ps [s fcache]
         (if (empty? s)
           #{#{}}
           (reduce
             (fn [acc [e ds]]
               (let [psds (if-let [[n model] (find fcache (count ds))]
                            (ps-with-model ds model)
                            (ps ds fcache))]
                 (clojure.set/union acc psds (set (map #(conj % e) psds)))))
             #{}
             (for [e s] [e (clojure.set/difference s #{e})]))))]
    (fn [s]
      (let [fcache
            (reduce
              (fn [fcache i]
                (assoc fcache i (ps (set (range i)) fcache)))
              {}
              (range (count s)))]
        (ps s fcache)))))

(defcheck solution-9e469101
  (fn [s]
    (reduce
      (fn [acc x]
        (clojure.set/union
          acc
          (set (map #(clojure.set/union #{x} %) acc))))
      #{#{}} s)))

(defcheck solution-9f06341c
  (partial reduce
    #(into % (map (fn[s](conj s %2)) %))
    #{#{}}))

(defcheck solution-9fcdb14e
  (fn p [ls]
    (if (empty? ls) #{#{}}
                    (clojure.set/union (p (next ls))
                      (into #{} (map #(conj % (first ls)) (p (next ls))))))))

(defcheck solution-a030370b
  (fn ex85-2
    [s]
    (set (conj
           (if (= 0 (count s))
             nil
             (let [res (ex85-2 (rest s))]
               (map #(if (coll? %)
                       (set %)
                       (set [%]))
                 (concat res
                         [(first s)]
                         (map #(if (coll? %)
                                 (conj % (first s))
                                 (conj [%] (first s)))
                           res)))))
           #{}))))

(defcheck solution-a0488aa3
  (fn x [s]
    (if (= 10 (count s)) (range 1024)
                         (set (cons s (mapcat #(x (disj s %)) s))))))

(defcheck solution-a04896b3
  (fn this [l]
    (if (empty? l)
      #{#{}}
      (let [rl (this (rest l))]
        (set (concat rl (map (fn [s] (set (cons (first l) s))) rl)))))))

(defcheck solution-a0a55b27
  (fn powerset [s]
    (if (empty? s) #{#{}}
                   (into (powerset (rest s))
                     (map #(conj % (first s))
                       (powerset (rest s)))))))

(defcheck solution-a19410d9
  (fn powerset [coll]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} coll)))

(defcheck solution-a1f4f6ca
  (fn power-set2 [s]
    (let [s-ind (map-indexed vector s)
          decode (fn [n]
                   (into #{} (map second
                               (filter #(not= 0 (bit-and n (bit-shift-left 1 (first %)))) s-ind))))]
      (into #{} (map decode (range (bit-shift-left 1 (count s))))))))

(defcheck solution-a22c87b6
  (fn [v] (set (map set
                 ((fn f [[h & t]]
                    (if h
                      (let [r (f t)]
                        (concat r (map #(concat [h] %) r)))
                      [[]]
                      )) (vec v))))))

(defcheck solution-a2403875
  (fn sub-set [s]
    (if (empty? s)
      #{#{}}
      (into #{}
        (mapcat
          #(list % (into #{} (cons (first s) %)))
          (sub-set (rest s)))))))

(defcheck solution-a2aaa1a3
  (fn pow [s] (if (empty? s) #{s} (let [
                                        s1 (first s)
                                        sn (disj s s1)
                                        pp (pow sn)
                                        pn (for [i pp] (conj i s1))]
                                    (into pp pn)))))

(defcheck solution-a2b31e38
  (fn powerset [ls]
    (if (empty? ls)
      #{#{}}
      (clojure.set/union
        (powerset (next ls))
        (map #(conj % (first ls)) (powerset (next ls)))))))

(defcheck solution-a2fcb3be
  reduce (fn [c s] (into c (map #(conj % s) c))) #{#{}})

(defcheck solution-a2fd7bf2
  (fn p [s] (if (empty? s) #{#{}} (let [e (first s) t (disj s e)] (into (p t) ((fn f [e t] (into #{} (map #(conj % e) t))) e (p t)))))))

(defcheck solution-a3916833
  (fn p [c]
    (if (empty? c) #{#{}}
                   (let [r (p (rest c))]
                     (clojure.set/union r (apply hash-set (map #(conj % (first c)) r)))))))

(defcheck solution-a39d0b06
  (fn [s]
    (let [n (fn [& sets]
              (into (set sets) (for [s sets x s] (disj s x))))
          c (count s)]
      (nth (iterate (partial apply n) (n s)) c))))

(defcheck solution-a3bf96b5
  (fn [s]
    (loop [s s
           accum #{#{}}]
      (if (empty? s)
        accum
        (let [current-element (first s)
              remaining-elements (rest s)
              new-elements (reduce #(conj %1 (conj %2 current-element)) #{} accum)
              new-set (clojure.set/union accum new-elements)]
          (recur remaining-elements new-set))))))

(defcheck solution-a3c4b81
  (fn ps85 [s]
    (if (seq s)
      (let [rs (ps85 (next s))]
        (into rs (map #(conj % (first s)) rs)))
      #{#{}})))

(defcheck solution-a4052126
  (fn [ss]
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
      (set (conj (flatten (reduce combos [] ss)) #{})))))

(defcheck solution-a5e0e9b7
  (fn [s]
    (reduce (fn [res item]
              (into res (map #(conj % item) res))) #{#{}} s)))

(defcheck solution-a6657765
  (fn pset [s]
    (letfn [ (tfseq [n]
               (map #(if (= \1 %) true false)
                 (seq (clojure.pprint/cl-format nil "~b" n))))
            ]

      (let [v  (vec s)
            l  (count v)
            mx (int (Math/pow 2 l) )]
        (loop [n 0
               acc #{}]
          (let [mtfseq  (tfseq n)
                pknum   (- l (count mtfseq))
                pktfseq (concat (repeat pknum false) mtfseq )
                ssi (map #(if %1 %2 false) pktfseq v)
                ss (filter #(if % true false) ssi)
                ]
            (if (= mx n)
              acc
              (recur (inc n) (conj acc (set ss))))
            ))))
    ))

(defcheck solution-a6819a29
  (fn superset [s]
    (let [gen-subset (fn gen-subset [col mask]
                       (let [col-idx (map-indexed #(list %1 %2) col)
                             col-filtered (filter
                                            #(not= 0 (bit-and (bit-shift-left 1 (first %)) mask))
                                            col-idx)]
                         (map second col-filtered)))

          pow #(apply * (take %2 (repeat %1)))
          elems (into '() s)
          perms (range (pow 2 (count elems)))
          sublists (map #(gen-subset elems %) perms)
          subsets (map set sublists)]
      (set subsets))))

(defcheck solution-a6bd4dc
  (fn powerset [s]
    (reduce
      #(into % (for [subset %]
                 (conj subset %2)))
      #{#{}} s)))

(defcheck solution-a6c216e7
  (fn ps [s]
    (if (empty? s)
      #{#{}}
      (let [ps' (ps (set (rest s)))
            x (first s)]
        (set (concat ps' (map #(conj % x) ps')))))))

(defcheck solution-a6e55b5d
  (fn power-set-of
    [x]
    (if (empty? x) #{#{}}
                   (loop [[h & t] (into [] x)
                          accum #{#{}}]
                     (let [new-accum (into accum (map #(conj % h) accum))]
                       (if (nil? t)
                         new-accum
                         (recur t new-accum)))))))

(defcheck solution-a72dfca0
  (fn p85
    ([lst] (p85 #{#{}} lst))
    ([s lst]
     (if (empty? lst) s
                      (p85 (reduce conj s (for [ss s] (conj ss (first lst)))) (next lst))))))

(defcheck solution-a79c16a4
  (fn [x]
    (reduce
      (fn addelem [old elem]
        (apply clojure.set/union old (conj #{} (map #(conj % elem) old))))
      #{#{}}
      (map identity x)
      )
    ))

(defcheck solution-a7a85f04
  (fn power-set
    ([s] (power-set s #{#{}}))
    ([s acc]
     (if (empty? s)
       acc
       (recur (disj s (first s))
         (into acc (map #(conj % (first s)) acc)))))))

(defcheck solution-a84e5803
  (fn powerset [s]
    (let [union clojure.set/union]
      (if (empty? s)
        #{#{}}
        (union
          (powerset (next s))
          (map #(conj % (first s)) (powerset (next s))))))))

(defcheck solution-a8e7bb46
  (fn [s]
    (reduce
      (fn [a b] (clojure.set/union a (set (map (fn [x] (clojure.set/union x #{b})) a))))
      #{#{}}
      s)))

(defcheck solution-a8fdca24
  (fn [s]
    (letfn
     [(subset-sums [s] (set (map #(apply + %) (filter #(not (empty? %)) (subsets s)))))
      (subsets [s] (map #(bit-filter s %) (range (two-raised (count s)))))
      (bit-filter [s m] (let [indexed-elts (zipmap (range) s)]
                          (set (filter #(not (nil? %))
                                 (map #(if (bit-test m (% 0)) (% 1))
                                   indexed-elts)))))
      (two-raised [n] (apply * (take n (repeat 2))))]
      (set (subsets s)))))

(defcheck solution-a96f032
  (fn pw
    [a]
    (if (empty? a)
      #{#{}}
      (if (= 1 (count a))
        #{#{(first a)} #{}}
        (set (for [x (pw #{(first a)}) y (pw (rest a))] (clojure.set/union x y)))))
    ))

(defcheck solution-a9d8ca3e
  (fn [coll]
    (into #{}
      (reduce (fn [l x]
                (into #{}
                  (concat l (map #(conj % x) l))))
        #{#{}}
        coll))))

(defcheck solution-a9e601ed
  (fn powerset [s]
    (if (empty? s) #{#{}}
                   (into (powerset (next s))
                     (map #(conj % (first s)) (powerset (next s)))))))

(defcheck solution-aa06328e
  (partial reduce
    (comp (partial apply into)
          (juxt first
            (comp (partial apply map conj)
                  (juxt first
                    (comp repeat second))))
          list)
    #{#{}}))

(defcheck solution-aa5ea87d
  (fn [x]
    (let [n (count x)] (into #{}
                         (for [s (range (int (Math/pow 2 n)))]
                           (into #{} (remove nil? (map-indexed #(if (zero? (bit-and s (bit-shift-left 1 %))) nil %2) x
                                                    ))))))))

(defcheck solution-ab39592d
  (fn [s]
    (reduce (fn [x y] (into x (map #(conj % y) x)))
      #{#{}} s)))

(defcheck solution-abe58dc7
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [x (first s) ps1 (power-set (rest s))]
        (clojure.set/union ps1 (map #(conj % x) ps1))))))

(defcheck solution-acde742a
  (fn [s]
    (letfn [(pwrrecur [s res]
              (if (contains? res s) (conj res #{})
                                    (letfn [(addset [v s] (set (map #(conj v %) s)))]
                                      (pwrrecur s (reduce #(clojure.set/union % (addset %2 s)) #{} res)))))]
      (pwrrecur s #{#{}}))))

(defcheck solution-ad0831e9
  (fn f
    ([s t]
     (if (seq s)
       (clojure.set/union (f (rest s) t) (f (rest s) (conj t (first s))))
       (set (list t))))
    ([s] (f (seq s) #{}))))

(defcheck solution-adf87d29
  #((fn f [[a & s :as q]]
      (if q
        (into (f s) (for [i (f s)] (conj i a)))
        #{#{}}))
    (seq %)))

(defcheck solution-ae0ccef4
  (fn [s]
    (reduce (fn [ss e] (into ss (map #(conj % e) ss)))
      #{#{}} s)))

(defcheck solution-ae7988a0
  (fn power-set [col] (set (reduce (fn [base x] (concat (map #(conj % x) base) base)) #{#{}} col))))

(defcheck solution-ae8512ee
  (fn [s]
    (reduce
      (fn [acc x]
        (apply conj acc
          (map #(conj % x) acc)))
      #{#{}} s)))

(defcheck solution-aea21739
  (fn [coll]
    (reduce (fn [acc x]
              (into acc
                (map (fn [xs] (conj xs x)) acc)))
      #{#{}} coll)))

(defcheck solution-aebe1243
  (fn [x]
    (loop [s x, pow #{#{}}]
      (if (empty? s) pow
                     (recur (rest s) (reduce #(conj %1 (conj %2 (first s))) pow pow))))))

(defcheck solution-aecc1bb8
  (fn power-set [coll]
    (if (empty? coll)
      #{#{}}
      (let [pset (power-set (rest coll))]
        (clojure.set/union
          pset
          (map #(conj % (first coll)) pset)
          )
        )
      )
    ))

(defcheck solution-af0513d3
  (fn ps [s] (if (seq s)
               (let [ss (ps (rest s))]
                 (clojure.set/union (set (map #(conj % (first s)) ss)) ss))
               #{#{}})))

(defcheck solution-af5030bd
  (fn [s]
    (letfn [(subsets [[x & xs :as all] acc]
              (cond
                (nil? (seq all)) acc
                :else (do (subsets (into [] xs)
                            (concat [[x]] acc (map #(conj % x) acc))))))]
      (->> (subsets (into [] s) [])
        (concat [[]])
        (map set)
        (into #{})))))

(defcheck solution-af945e40
  (fn g [l]
    (let [combinations (fn subsets [n items]
                         (cond
                           (= n 0) '(())
                           (empty? items) '()
                           :else (concat (map #(cons (first items) %) (subsets (dec n) (rest items)))
                                         (subsets n (rest items)))))]
      (into #{} (apply concat (for [i (range (inc (count l)))]
                                (map #(into #{} %1) (combinations i l)))) ))))

(defcheck solution-b024ae25
  (fn powerset [s]
    (reduce #(into % (for [subset %] (conj subset %2))) #{#{}} s)))

(defcheck solution-b0b1851
  (fn [r %]
    (r (fn [% b]
         (r #(conj % (conj %2 b)) % %))
      #{#{}} %)) reduce)

(defcheck solution-b1754f4b
  #(set
     (for [n (range (bit-shift-left 1 (count %)))]
       (set (keep-indexed (fn [i e] (if (bit-test n i) e)) %)))))

(defcheck solution-b18e6b7a
  (fn ps [s]
    (letfn [(f [s]
              (if (empty? s) #{s}
                             (let [r (f (rest s))]
                               (into r
                                 (map #(conj % (first s)) r)))))]
      (set (map set (f s))))))

(defcheck solution-b1c47604
  (fn i [m]
    (if (empty? m) #{(set m)}
                   (clojure.set/union (i (rest m)) (map #(conj % (first m)) (i (rest m)))))))

(defcheck solution-b203685
  (fn [coll]
    (set (map set  (reduce (fn [accum x]
                             (concat accum (map #(cons x %) accum)))
                     #{#{}}
                     coll)))
    ))

(defcheck solution-b23d95aa
  (fn power [s]
    (if (empty? s)
      #{s}
      (let
       [e (first s)
        remainder (disj s e)
        p (power remainder)]
        (clojure.set/union p (set (map #(conj % e) p)))))))

(defcheck solution-b25bf189
  (fn [ss]
    (loop [s ss r '(#{})]
      #_(pr r)
      (if (empty? s)
        (set r)
        (recur (rest s) (flatten (for [power_element r] (list (conj power_element (first s)) power_element))))))))

(defcheck solution-b2ae708a
  (fn powerset [set]
    (let [add-to-every
          (fn [x sets]
            (into #{} (for [s sets]
                        (conj s x))))]
      (if (empty? set)
        #{set}
        (let [x (first set)
              power (powerset (disj set x))]
          (clojure.set/union
            power
            (add-to-every x power)))))))

(defcheck solution-b378cee
  #(reduce (fn [x y]
             (into x (for [a %
                           b x]
                       (disj b a))))
     #{%}
     %))

(defcheck solution-b3895660
  (fn [x]
    (into #{}
      (for [n (range 0 (bit-shift-left 1 (count x)))]
        (into #{}
          (filter #(bit-test n (.indexOf (vec x) %)) x)
          )
        )
      )
    ))

(defcheck solution-b395583e
  (fn [s]
    (letfn [(rm [s]
              (for [x s]
                (set (remove #(= % x) s))))
            (gen [acc s1]
              (if (empty? (first s1))
                (if (empty? s)
                  (set acc)
                  (set (conj acc #{})))
                (recur (concat acc s1) (set (mapcat rm s1)))))]
      (gen [s] (rm s)))))

(defcheck solution-b3a577c4
  (fn powerset [s]
    (reduce
      (fn [acc cur] (into acc (map #(conj % cur) acc)))
      #{#{}} s
      )
    ))

(defcheck solution-b3dca811
  (fn powerset [elems]
    (set
      (reduce
        (fn [pset elem]
          (concat pset (map #(conj % elem) pset)))
        [#{}]
        elems))))

(defcheck solution-b3fa9940
  (fn p [s]
    (if-let [[x & xs] (seq s)]
      (set (mapcat (fn [y] [y (conj y x)]) (p xs)))
      #{#{}})))

(defcheck solution-b4078fd9
  (fn sets [s]
    (if (seq s)
      (let [x (first s)
            xs (next s)]
        (set (mapcat (fn [y]
                       [(conj y x)
                        y])
               (sets xs))))
      #{#{}})))

(defcheck solution-b41bb184
  (fn p [c]
    (if (empty? c) #{#{}}
                   (clojure.set/union (p (next c))
                     (map #(conj % (first c)) (p (rest c)))))))

(defcheck solution-b4319f68
  (fn [s]
    (let [arr (into [] s), c (count arr)]
      (reduce
        (fn[s num]
          (conj s (into #{} (filter #(not= nil %) (map-indexed #(if (bit-test num %) %2 nil) arr)))))
        #{}
        (range (Math/pow 2 c))))))

(defcheck solution-b43bd4e0
  (fn f [s]
    (if (empty? s) #{s}
                   (loop [new-set (set (for [x s
                                             y s
                                             :when (not= x y)]
                                         #{x y}))
                          result-set (clojure.set/union #{s #{}} new-set (set (for [x s] #{x})))]
                     (let [a (set (for [x new-set
                                        y s
                                        :when (not (contains? x y))]
                                    (conj x y)))]
                       (if (= a new-set) result-set
                                         (recur a (into result-set a))))))))

(defcheck solution-b459b024
  (fn [s] (set (reduce (fn [cc e] (into (map #(conj % e) cc) cc)) #{#{}} s))))

(defcheck solution-b4e82f2f
  (fn pow [st]
    (if (empty? st) #{st}
                    (let [t (first st) subpow (pow (set (rest st)))]
                      (clojure.set/union
                        subpow
                        (set (map #(conj % t) subpow)))))))

(defcheck solution-b5322082
  (fn power-set
    [s]
    (letfn [(add-to-set [s i] (into s (map #(conj % i) s)))]
      (reduce #(add-to-set %1 %2) #{#{}} s)
      )
    ))

(defcheck solution-b536d829
  (partial reduce (fn [xss x]
                    (let [xss+x (map #(conj % x) xss)]
                      (into xss xss+x)))
    #{#{}}))

(defcheck solution-b5467da
  (fn power [s]
    (if (empty? s) #{#{}}
                   (let [r (power (rest s))]
                     (clojure.set/union r (map #(conj % (first s)) r))))))

(defcheck solution-b5b624c0
  (fn [s]
    (loop [xs s
           acc (list #{})]
      (if (empty? xs)
        (set acc)
        (recur (rest xs) (concat acc (map #(conj % (first xs)) acc)))))))

(defcheck solution-b622a44c
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [f (first s)
            g (rest s)
            small (power-set g)
            other (for [ss small] (conj ss f))]
        (set (concat small other))))))

(defcheck solution-b6311d54
  (fn ps[s]
    (if (seq s)
      (let [r (ps (rest s))]
        (clojure.set/union r (map #(conj % (first s)) r)))
      #{#{}})))

(defcheck solution-b677e379
  #(reduce
     (fn [acc x] (into acc (for [s acc] (conj s x))))
     #{#{}}
     %))

(defcheck solution-b6f21134
  (fn ps [s]
    (if-let [es (seq s)]
      (let [r (ps (rest es))]
        (into r (map #(conj % (first es)) r)))
      #{#{}})))

(defcheck solution-b70e9737
  (fn [s]
    (letfn [(subsets [[x & xs]]
              (if (nil? x)
                #{#{}}
                (let [subs (subsets xs)]
                  (clojure.set/union
                    subs
                    (into #{} (map #(conj % x) subs))))))]
      (subsets (seq s)))))

(defcheck solution-b7219343
  (fn [s]
    (letfn [(ps [acc s]
              (if (seq s)
                (concat (ps acc (rest s)) (ps (conj acc (first s)) (rest s)))
                (list acc)))]
      (set (ps #{} s)))))

(defcheck solution-b7e157d1
  #(set (map (fn[a](set (take (rand-int (+ 1 (count %))) (shuffle %)))) (range 100000))))

(defcheck solution-b812575b
  (fn [s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (reduce into (set [#{} s]) (vals (reduce #(p % %2 s) {} (range (count s))))))))

(defcheck solution-b857ab60
  (fn [xs]
    (let [pow  (fn [n exp] (apply * (repeat exp n)))
          v    (vec xs)
          exp  (count v)
          bits (mapv #(vector %1 (pow 2 %2)) (range) (range exp))
          bmap (fn [n]
                 (for [[i b] bits :when (= b (bit-and b n))]
                   (nth v i)))]
      (->> (range (pow 2 exp))
        (map bmap)
        (map set)
        set))))

(defcheck solution-b95f908a
  (fn [s]
    (nth
      (iterate
        #(into #{#{}} (for [x s y %] (into #{x} y)))
        (map hash-set s))
      (max 1 (count s)))))

(defcheck solution-b9800295
  (fn power-set
    [s]
    (if (empty? s) #{#{}}
                   (let [ps (power-set (rest s))]
                     (set (concat (map #(conj % (first s)) ps) ps))))))

(defcheck solution-b9c6f16
  (fn [s]
    (reduce
      (fn [r e]
        (into r (map #(conj % e) r)))
      #{#{}}
      s)))

(defcheck solution-ba5fb2ef
  (fn powerset [vals]
    (if (empty? vals)
      #{#{}}
      (let [x (first vals)
            sub (powerset (set (rest vals)))]
        (apply conj sub (set (map #(conj %1 x) sub)))))))

(defcheck solution-baab3c4e
  reduce (fn [y z] (into y (map #(conj % z) y))) #{#{}})

(defcheck solution-bab1be23
  (fn [s]
    (reduce (fn [sets x]
              (->> sets
                (map #(set (concat #{x} %)))
                (concat sets)
                (set)))
      #{#{}} s)))

(defcheck solution-baf21397
  (fn P [S]
    (if (empty? S) #{#{}}
                   (let [F (fn [e T] (set (map #(conj % e) T)))
                         e (first S)
                         T (disj S e)
                         PT (P T)]
                     (set
                       (concat
                        PT
                        (F e PT)))))))

(defcheck solution-bb36d4b8
  (fn [s]
    (let [num-to-ls (fn [n c] (map #(bit-test n %) (range c)))
          get-subset (fn [n] (map #(get % 1) (filter #(get % 0) (map vector (num-to-ls n (count s)) s))))]
      (set (map #(set (get-subset %)) (range (Math/pow 2 (count s))))))))

(defcheck solution-bba37243
  (fn powerset [ls]
    (if (empty? ls) #{#{}}
                    (clojure.set/union (powerset (next ls))
                      (map #(conj % (first ls)) (powerset (next ls)))))))

(defcheck solution-bc165824
  #(reduce (fn [s l] (into s (for [t s] (conj t l)))) #{#{}} %))

(defcheck solution-bc241692
  (fn [s]
    (letfn [
            (nth-masked? [n mask]
              (not= 0 (bit-and (bit-shift-left 1 n) mask)))
            (bitmap-select [mask xs]
              (map last (filter #(nth-masked? (first %1) mask) (keep-indexed #(vector %1 %2) xs))))
            (combinations [xs]
              (into #{} (map #(into #{} (bitmap-select % xs)) (range 0 (bit-shift-left 1 (count xs))))))
            ]
      (combinations s))))

(defcheck solution-bcb616ae
  (fn [coll]
    (reduce
      (fn [r x]
        (into r (map #(conj % x) r)))
      #{#{}}
      coll)))

(defcheck solution-bccd5f04
  (fn power-set[colls]
    (reduce #(into %1 (map (fn[i](conj i %2)) %1) )
      #{#{}}
      colls)))

(defcheck solution-bd6ed1f2
  (fn powerset [x]
    (if (seq x)
      (into (powerset (set (rest x))) (set (for [y (vector (first x))
                                                 z (powerset (set (rest x)))]
                                             (conj z y))))
      #{#{}}
      )))

(defcheck solution-bd800710
  (fn [s]
    (letfn
     [(subset-indexes [start set-length subset-length]
        (cond
          (> start (- set-length subset-length)) []
          (zero? subset-length) [[]]
          :else (for [x (range start set-length)
                      y (subset-indexes (inc x) set-length (dec subset-length))]
                  (cons x y))))
      (pick-indexes [sq indexes]
        (map #(nth sq %) indexes))]
      (->>
        (for [x (range (inc (count s)))]
          (subset-indexes 0 (count s) x))
        (apply concat)
        (map #(pick-indexes (seq s) %))
        (map set)
        set))))

(defcheck solution-bd966488
  (fn fun [coll]
    (if (empty? coll) #{#{}}
                      (clojure.set/union
                        (reduce
                          #(conj % (conj %2 (first coll)))
                          #{}
                          (fun (rest coll)))
                        (fun (rest coll))))))

(defcheck solution-bdf11c4f
  (fn [coll]
    (reduce (fn [a x]
              (->> a
                (map #(set (concat #{x} %)))
                (concat a)
                set))
      #{#{}} coll)))

(defcheck solution-bdf4e4b1
  (fn [s]
    (reduce
      (fn [c e]
        (into c (map #(conj % e) c)))
      #{#{}}
      s)))

(defcheck solution-be9191fe
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (set
                     (let [ns (ps (next s))]
                       (clojure.set/union
                         ns
                         (map #(conj % (first s)) ns)))))))

(defcheck solution-be96c8b6
  (fn [s]
    (set
      (reduce (fn [acc value]
                (concat acc (map #(set (conj % value)) acc)))
        #{#{}}
        s))))

(defcheck solution-becc361a
  (letfn [
          (subsets [s] (map (partial disj s) s))
          (powerset [s]
            (loop [process-list [s] seen #{}]
              (if (empty? process-list)
                seen
                (if (not (seen (first process-list)))
                  (recur (concat (rest process-list) (subsets (first process-list))) (set (cons (first process-list) seen)))
                  (recur (rest process-list) seen)))))]
    powerset))

(defcheck solution-beec1584
  (fn [s]
    (->> #{s}
      (iterate (fn [z]
                 (reduce (fn [z s]
                           (into z (map #(disj s %) s)))
                   #{}
                   z)))
      (take-while seq)
      (reduce into #{#{}}))))

(defcheck solution-bf6ab6d3
  (fn [coll]
    (let [pow2 (fn [n] (bit-shift-left 1 n))
          bitnote (fn bitnote
                    ([x] (bitnote x 0))
                    ([x i]
                     (if (< x (pow2 i)) []
                                        (if (pos? (bit-and x (pow2 i)))
                                          (cons i (bitnote x (inc i)))
                                          (bitnote x (inc i))))))
          subsets (fn [coll]
                    (let [coll' (vec coll)
                          fetch (fn [x] (let [x' (bitnote x)]
                                          (map (partial get coll') x')))]
                      (->> coll'
                        count
                        pow2
                        (range 0)
                        (map (comp set fetch)))))]
      (set (subsets coll)))))

(defcheck solution-bf79cce2
  (fn [s]
    (set (reduce (fn [subsets element]
                   (concat subsets (map #(conj % element) subsets)))
           [#{}]
           s))))

(defcheck solution-bfa6455d
  (fn powerset [sets]
    (letfn [(powerset2 [ps ss]
              (if (empty? ss)
                ps
                (powerset2 (set (concat (map #(conj % (first ss)) ps) ps)) (rest ss))))]
      (powerset2 #{#{}} sets))))

(defcheck solution-bfdc73e9
  (fn p [s]
    (if (empty? s) #{#{}}
                   (let [r (set (rest s))]
                     (set (concat (p r) (map #(conj % (first s)) (p r))))))))

(defcheck solution-c07a0b93
  (fn [s]
    (set (reduce #(into (map (partial into #{%2}) %) %) [#{}] s))))

(defcheck solution-c0d55de4
  (fn pwr-set [xs]
    (into #{} (let [min-set first
                    delete-min-set #(disj % (min-set %))
                    pxs (lazy-seq (pwr-set (delete-min-set xs)))]
                (if (empty? xs)
                  #{#{}}
                  (clojure.set/union (map #(conj % (min-set xs)) pxs) pxs))))))

(defcheck solution-c0e9b69c
  (fn pp
    ([s] (pp s #{#{}}))
    ([s p] (if (nil? (first s))
             (set p)
             (recur (rest s) (concat p (map #(conj % (first s)) p)))))))

(defcheck solution-c173b5fe
  reduce (fn [a x] (into a (map #(conj % x) a))) #{#{}})

(defcheck solution-c18b3ea9
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
    (fn powerset [s]
      (conj (set (mapcat #(map set (combinations s %))
                   (range 1 (inc (count s)))))
        #{}))))

(defcheck solution-c27e324
  (fn pset [s]
    (if (empty? s)
      #{#{}}

      (let [f    (first s)
            r    (set (rest s))
            memo (pset r)]
        (clojure.set/union memo (map #(conj % f) memo))
        )
      )
    ))

(defcheck solution-c34fbd07
  (fn powerset [ls]
    (if-let [[head & tail] (seq ls)]
      (clojure.set/union
        (powerset tail)
        (map #(conj % head) (powerset tail)))
      #{#{}})))

(defcheck solution-c36b8436
  (fn powerset [s]
    (if (empty? s) #{#{}}
                   (set (mapcat #(list % (conj % (first s))) (powerset (rest s)))))))

(defcheck solution-c39ebad2
  (letfn [(comb [n coll] (cond (zero? n) '(())
                               (= n 1) (map list coll)
                               :else (mapcat
                                       (fn [[x & xs]] (map #(cons x %) (comb (dec n) xs)))
                                       (take (- (count coll) (dec n)) (iterate rest coll)))))]
    (fn [coll] (set (map set (mapcat #(comb % (seq coll)) (range (inc (count coll)))))))))

(defcheck solution-c3b4a710
  (fn power-set [initial-set]
    (letfn [(helper [[ans cur-set]]
              [(clojure.set/union ans (set (for [s ans e cur-set] (conj s e)))) (rest cur-set)])]
      (let [iter (count initial-set)]
        (first (nth (iterate helper [#{#{}} initial-set]) (inc iter)))))))

(defcheck solution-c3fa42e5
  (fn [xs]
    (loop [xs xs zs #{#{}}]
      (if (seq xs)
        (recur (next xs) (into zs (map #(conj % (first xs)) zs)))
        zs))))

(defcheck solution-c4192553
  (fn [s]
    (loop [i 0 res #{#{}}]
      (if (= i (count s))
        res
        (recur (inc i)
          (into #{} (concat res (for [x res y s]
                                  (conj x y)))))))))

(defcheck solution-c423ede7
  (fn pset [s]
    (if (empty? s)
      #{#{}}
      (let [p (pset (set (rest s)))]
        (clojure.set/union
          p
          (map #(conj % (first s)) p))))))

(defcheck solution-c43ae14c
  (fn [s]
    (reduce (fn [r v] (set (concat r (for [i r] (set (conj i v))))))
      #{#{}} s)))

(defcheck solution-c45903f8
  (fn [s]
    (set (loop [ret #{#{}}
                left s]
           (if (empty? left)
             ret
             (recur (concat (map #(conj % (first left)) ret)
                            ret)
               (rest left)))))))

(defcheck solution-c47df6e3
  (fn [s]
    (reduce (fn [acc item]
              (into acc (map #(conj % item) acc)))
      #{#{}} s)))

(defcheck solution-c4e2f12e
  #(reduce (fn [sets elem]
             (reduce (fn [c n]
                       (conj c (conj n elem))) sets sets))
     #{#{}} %))

(defcheck solution-c5472596
  #(reduce (fn [acc e] (loop [coll acc acc acc] (if (empty? coll) acc (recur (rest coll) (conj acc (conj (first coll) e)))))) #{#{}} %))

(defcheck solution-c5842859
  (fn [startset]
    (let [extend-element (fn [sets el] (map #(conj % el) sets))               ;(map #(conj % 1) #{#{}}) -> #{#{1}}
          extend-set     (fn [sets el] (into sets (extend-element sets el)))] ;(into #{#{}} (extend-element #{#{}} 1)
      (reduce extend-set #{#{}} startset))))

(defcheck solution-c5975225
  (fn power-set [s]
    (if (empty? s) #{#{}}
                   (let [
                         s1 (first s)
                         ps (power-set (rest s))]
                     (reduce #(conj % (conj %2 s1)) ps ps)))))

(defcheck solution-c6633522
  (letfn [(set-from-mask [x s]
            (into #{}
              (filter (complement nil?)
                (map-indexed (fn [i v] (if (bit-test x i) v nil)) s))))]
    (fn [s]
      (into #{}
        (map #(set-from-mask % s) (range (int (Math/pow 2 (count s)))))))))

(defcheck solution-c6ead8e9
  (fn [S]
    (letfn
     [(helper [res cur rems]
        (if (empty? rems) (conj res cur)
                          (let [ added (helper res (conj cur (first rems)) (rest rems))
                                skip  (helper res cur (rest rems))]

                            (clojure.set/union added skip))))]

      (helper #{} #{} S))))

(defcheck solution-c71243cf
  (fn power-set [init-set]
    (loop [res-set #{#{}}
           n (count init-set)]
      (if (= n 0)
        (conj res-set #{})
        (recur (apply clojure.set/union
                 (map (fn [x]
                        (set (map (fn [y]
                                    (conj x y))
                               init-set)))
                   res-set))
          (dec n))))))

(defcheck solution-c76b89dd
  (fn [base] (let [pow  (fn [x n]
                          (reduce * (repeat n x)))
                   select (fn [n]
                            (loop [v base n n rv ()]
                              (if (empty? v)
                                (set rv)
                                (if (bit-test n 0)
                                  (recur (rest v) (bit-shift-right n 1) (cons (first v) rv))
                                  (recur (rest v) (bit-shift-right n 1) rv)))))]
               (into #{} (map select (range (pow 2 (count base))))))))

(defcheck solution-c810616
  (fn [dset]
    (letfn [
            (p-mrg [a ss]
              (map (partial cons a) ss))

            (p-set [ds]
              (if (empty? ds)
                (list (list))
                (let [ppset (p-set (rest ds))]
                  (concat ppset (p-mrg (first ds) ppset)))) )

            (pwr-set [xs]
              (into #{} (map (partial into #{}) (p-set xs))))]
      (pwr-set dset) )))

(defcheck solution-c88bdb62
  (fn power-set [s]
    (if (empty? s)
      #{s}
      (let [subsets (power-set (set (rest s)))]
        (into subsets (map #(conj % (first s)) subsets))))))

(defcheck solution-c8a15589
  (fn power-set
    ([s]
     (power-set #{} s))
    ([out cur]
     (if (empty? cur)
       #{out}
       (clojure.set/union
         (power-set (conj out (first cur)) (rest cur))
         (power-set out (rest cur)))))
    ))

(defcheck solution-c9007283
  (fn peu [x] (let [y (apply list x)] (set (if (empty? y) #{#{}} (let [z (apply list (peu (rest y)))] (clojure.set/union z (map #(clojure.set/union % #{(first y)}) z))) )))))

(defcheck solution-c993385
  (fn [c]
    (letfn [(build [s]
              (let [len (count s)]
                (cond
                  (= 0 len) #{#{}}
                  (= 1 len) #{#{} (set s)}
                  (= 2 len) #{#{} (set s) (conj #{} (first s)) (conj #{} (last s))}
                  :else (let [n (first s) sub-sets (build (rest s))]
                          (clojure.set/union sub-sets
                            (map #(conj % n) sub-sets))))))]
      (build c))))

(defcheck solution-ca3b4d0f
  (fn power-set [s]
    (let [s-count (count s)
          s-seq (seq s)
          fst (first s-seq)]
      (cond
        (= s-count 0) #{#{}}
        (= s-count 1) #{#{} s}
        :else
        (clojure.set/union
          (set (map #(conj % fst) (power-set (set (rest s-seq)))))
          (power-set (set (rest s-seq))))))))

(defcheck solution-ca40ac33
  (fn [s]
    (reduce
      (fn [s i] (into s (map #(conj % i) s)))
      #{#{}}
      s)))

(defcheck solution-cac6d398
  (fn [s]
    (-> (reduce (fn [a v]
                  (concat (conj a #{v})
                          (for [x a] (if (coll? x) (conj x v) #{x v}))))
          #{} s)
      (conj #{})
      set)))

(defcheck solution-cb4929f5
  (fn power
    ([s] (set (power #{} s)))
    ([base s] (if-let [[head & more] (seq s)]
                (concat (power base more) (power (conj base head) more))
                [base]))))

(defcheck solution-cb571451
  (fn ps [ls]
    (if (empty? ls) #{#{}}
                    (clojure.set/union
                      (ps (next ls))
                      (map #(conj % (first ls)) (ps (next ls)))))))

(defcheck solution-cbdfe4e3
  (fn power-set [s]
    (let [first (first s) subset (rest s)]
      (if (empty? s) #{#{}}
                     (clojure.set/union
                       (into #{} (map #(clojure.set/union #{first} %) (power-set subset)))
                       (power-set subset))))))

(defcheck solution-cc42f741
  (letfn
   [(subset [s n]
      (set
        (loop [s s, ss (), n n]
          (if (< n 1) ss
                      (if (not= 0 (rem n 2))
                        (recur (rest s) (conj ss (first s)) (quot n 2))
                        (recur (rest s) ss (quot n 2)))))))]

    (fn [s]
      (set
        (map
          (partial subset s)
          (range (Math/pow 2 (count s))))))))

(defcheck solution-cc92825d
  (fn power-set [s]
    (reduce
      (fn [coll new]
        (into coll
          (set (map #(conj % new) coll)))) #{#{}} s)))

(defcheck solution-cca28dd4
  (fn g
    ([x]
     (let [i (map #(conj #{} %) x)]
       (if (seq i) (g x `#{#{} ~@i} i) #{#{}})))
    ([x r l]
     (let [t (set (for [a l b x :when (nil? (a b))] `#{~@a ~b}))]
       (if (t x)
         (into r t)
         (recur x (into r t) t))))))

(defcheck solution-cd262baf
  (fn subsets [s]
    (set (reduce (fn [ss x] (concat ss (map #(conj % x) ss))) [#{}] s))))

(defcheck solution-cd71ccef
  (fn power
    ([s]
     (if (empty? s)
       #{#{}}
       (power s #{s #{}})))
    ([s r]
     (if (empty? s)
       r
       (power (set (rest s))
         (->> r
           (map #(clojure.set/union (hash-set (first s)) %))
           set
           (clojure.set/union r)))))))

(defcheck solution-cd7e1c1a
  (fn [coll]
    (loop [result [[]] input coll]
      (if (empty? input) (set (map set result))
                         (recur
                           (concat result (map #(conj % (first input)) result))
                           (rest input)
                           )
                         ))))

(defcheck solution-cd85ebb6
  (fn p [s]
    (if (seq s)
      (let [r (p (rest s))]
        (into r (map #(conj % (first s)) r)))
      #{#{}})))

(defcheck solution-cdbe288
  (fn super [x]
    (reduce (fn [x y]
              (clojure.set/union x (set (flatten (for [x' x] [(clojure.set/union x' #{y})
                                                              #{y}]))))
              ) #{#{}} x)
    ))

(defcheck solution-cddc1ac2
  (fn [s]
    (letfn [(g [y]
              (map (partial conj y) s))
            (f [x]
              (set (mapcat g x)))
            (not-found [x]
              (not (some final-length x)))
            (final-length [x]
              (= (count x) (count s)))]
      (conj (first (drop-while not-found (iterate f #{#{}}))) #{}))))

(defcheck solution-cdf5afdd
  reduce (fn [a x]
           (into a (map #(conj % x) a))) #{#{}})

(defcheck solution-d02c1227
  (fn power-set--bottom-up
    [s] {:pre [(set? s)]}
    (set (reduce (fn [acc x] (mapcat (juxt identity #(conj % x)) acc))
           [#{}] s))))

(defcheck solution-d0926964
  (fn ss [is]
    (set ((fn hr [s]
            (if (empty? s)
              #{#{}}
              (let [h (first s)
                    t (rest s)]
                #_(println "h" h "t" t)
                (if (empty? t)
                  #{#{h} #{}}
                  (let[tp (hr t)
                       tph (reduce #(conj %1 (conj %2 h)) #{} tp)]
                    #_(println "tp" tp "tph" tph)
                    (concat tp tph))))))
          is))))

(defcheck solution-d0faaf5b
  (fn [setxx] (
                letfn [
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

                       (getstuff [numdigs setsize]
                         (index-combinations  numdigs setsize))

                       (powerset [setxx] (let [
                                               myset setxx
                                               sets (reduce (fn [s d](concat (getstuff d (count myset)) s)) #{} (range 1 (count myset)))
                                               stuff (set (map set
                                                            (map
                                                              (fn [s] (map (vec myset) s)) sets)))]
                                           (set (concat  [#{}] [setxx] stuff))))
                       ]
                (powerset setxx))))

(defcheck solution-d107e077
  (fn [ s]
    (letfn [(ps [super-set c]
              (if (= c (count s)) super-set
                                  (let [ new-sets  ( for [ a s b super-set] (conj b a))]
                                    (recur (into super-set new-sets) (inc c)))))]
      (ps #{#{}} 0))))

(defcheck solution-d12b262d
  (fn powerset [r]
    (if (empty? r)
      #{#{}}
      (set
        (let [[x & xs] (seq r)]
          (mapcat (fn [p] [p, (conj p x)]) (powerset xs)))))))

(defcheck solution-d131bf92
  (fn ps [s]
    (if (empty? s)
      #{#{}}
      (let [f (first s) r (rest s) rr (ps r)]
        (clojure.set/union
          (set (map #(conj % f) rr))
          rr
          )
        )
      )
    ))

(defcheck solution-d17e568e
  (fn [s] ; for each element in set, we can choose put it in and not in
    (reduce (fn [rs e]
              (into rs (map #(conj % e) rs))); put into each element of rs
      #{#{}} s) ))

(defcheck solution-d1ae6030
  (fn powerset [s]
    (if (empty? s)
      #{#{}}
      (let [union (fn union [& sets]
                    (reduce into (first sets) (rest sets)))
            ps-1 (powerset (rest s))]
        (union ps-1 (map #(conj % (first s)) ps-1))))))

(defcheck solution-d29dd391
  (fn [s]
    (reduce (fn [power-set x]
              (into power-set (map #(conj % x) power-set)))
      #{#{}} s)))

(defcheck solution-d2a2f5f5
  (fn subsets [s]
    (if (seq s)
      (let [ss (subsets (rest s)), fs (first s)]
        (set (concat ss (map #(conj % fs) ss))))
      #{#{}})))

(defcheck solution-d2bbf697
  (fn power-set [s]
    (set (reduce (fn [as x]
                   (concat as (map #(conj % x) as)))
           [#{}] s))))

(defcheck solution-d2e1f816
  (fn [s]
    (reduce (fn [acc  i]
              (reduce (fn [acc j]
                        (conj acc (conj  j i)))
                acc acc))
      #{#{}} s)))

(defcheck solution-d30aee1e
  (fn powerset [xs]
    (if (seq xs)
      (let [xs (into #{} xs)
            head (first xs)
            tail (rest xs)
            subsets (powerset tail)]
        (clojure.set/union #{#{head}}
          subsets
          (into #{} (map #(conj % head) subsets))))
      #{#{}})))

(defcheck solution-d30d2416
  (fn [%]
    (reduce (fn [coll item]
              (reduce #(conj % (conj %2 item)) coll coll))
      #{#{}} %)))

(defcheck solution-d30eb0a4
  (fn powerset [s]
    (letfn [(combine [acc x]
              (conj (into acc (map #(conj % x) acc)) #{x}))]
      (conj (reduce combine #{} s) #{}))))

(defcheck solution-d30f140e
  (fn subsets [s]
    (if-not (empty? s)
      (into #{} (mapcat #(vector % (conj % (first s))) (subsets (rest s))))
      #{#{}})))

(defcheck solution-d378bdf
  (fn ps [x]
    (if (empty? x)
      #{#{}}
      (set
        (concat
         (map #(conj % (first x)) (ps (rest x)))
         (ps (rest x))
         )
        )
      )
    ))

(defcheck solution-d3e7455b
  (fn power-set[s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) #{#{}} s))
    ))

(defcheck solution-d47703a5
  #(reduce
     (fn [subsets elem]
       (into subsets
         (for [x subsets]
           (conj x elem)))) #{#{}} %))

(defcheck solution-d4d4b40f
  (fn [s]
    (reduce
      (fn [x y]
        (set
          (concat x
                  (map #(conj % y) x)
                  [#{y}])))
      #{#{}} s)))

(defcheck solution-d4f1f5bd
  (fn [s]
    (reduce (fn [ps e]
              (->>
                ps
                (map #(set (concat #{e} %)))
                (concat ps)
                set))
      #{#{}}
      s)))

(defcheck solution-d5164c6
  (fn powerset
    [coll]
    (set (reduce (fn [acc x]
                   (concat acc (map #(conj % x) acc)))
           #{#{}}
           coll))))

(defcheck solution-d525b1cd
  (fn [s]
    (set (reduce (fn [init i]
                   (concat init (map #(conj % i) init)))
           #{#{}} s))))

(defcheck solution-d579c8f3
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [e (first s)
            r (rest s)
            s1 (power-set r)
            s2 (map #(conj % e) s1)]
        (into s1 s2)))))

(defcheck solution-d5aef5d4
  (fn power [xs] (if (empty? xs)
                   #{#{}}
                   (clojure.set/union (set (map #(conj % (first xs))
                                             (power (next xs))))
                     (power (next xs))))))

(defcheck solution-d64026be
  (fn power-set [s]
    (let [k (first s)
          rst (disj s k)]
      (if k
        (clojure.set/union (set (map #(clojure.set/union % #{k}) (power-set rst)))
          (power-set rst))
        #{#{}}))))

(defcheck solution-d6f472a8
  (fn [s]
    (letfn [(f [k s]
              (if (= k 0)
                '(#{})
                (set
                  (for [x (f (dec k) s)
                        y s
                        :when (not (contains? x y))]
                    (conj x y)))))]
      (set (mapcat #(f % s) (range (inc (count s))))))))

(defcheck solution-d72feee8
  (fn power-set-mk [setv]
    (letfn ([combine [s k n]
             (if (= n (count s)) k
                                 (let [countset
                                       (set (for [t (remove #(> n (count %)) k) x (remove t s)]
                                              (conj t x)))]
                                   (combine s (into k countset) (inc n))))])
      (combine setv #{#{}} 0))))

(defcheck solution-d7323ff5
  (fn [s]
    (let [power-set (fn [f s]
                      (apply
                        clojure.set/union
                        #{s}
                        (for [e s]
                          (let [subset (disj s e)]
                            (f f subset)))))
          f (memoize power-set)]
      (f f s))))

(defcheck solution-d747a1b9
  (fn p [s]
    (if (empty? s) #{#{}}
                   (let [others (p (rest s))
                         c (first s)]
                     (reduce #(conj % (conj %2 c)) (conj others #{c}) others)))))

(defcheck solution-d7c5fd4c
  (fn p
    ([s] (p '(()) s))
    ([r s]
     (if (empty? s)
       (set (map #(set %) r))
       (p (concat r (map #(conj % (first s)) r)) (rest s))))))

(defcheck solution-d7da200d
  (fn [given-set]
    (set (reduce (fn [pow g]
                   (for [fr pow to #{nil g}]
                     (if (nil? to) fr (conj fr to))))
           (if (> (count given-set) 0)
             #{ #{(first given-set)} #{} }
             #{#{}}) (rest given-set)))))

(defcheck solution-d7dc09b7
  (fn [a-set]
    (loop [lset a-set acc (conj #{#{}} a-set)]
      (if (empty? lset)
        acc
        (let [this-set (set (list (first lset)))
              set-seq (for [s acc] (clojure.set/union s this-set))]
          (recur (rest lset) (apply (partial conj acc) set-seq)))))))

(defcheck solution-d7f0f667
  (fn [s]
    (set
      (for [i (range (bit-shift-left 1 (count s)))]
        (set (keep-indexed #(if (pos? (bit-and i (bit-shift-left 1 %1))) %2) (vec s)))
        )
      )
    ))

(defcheck solution-d8fe4414
  (fn pset [s]
    (cond
      (= (count s) 0)
      #{#{}}

      :else
      (let [f (first s)
            r (rest s)
            theset (pset r)]
        ;; Add f by itself
        ;; And f to all permutations of r
        (set
          (concat
           (map #(conj % f) theset)
           theset))))))

(defcheck solution-d90eeef5
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [r (power-set (rest s))]
        (reduce into #{} [r (map #(conj % (first s)) r)])))))

(defcheck solution-d91247e3
  (fn pows [s]
    (loop [[lh & lt] (list s), acc #{#{}}]
      (if (nil? lh)
        acc
        (if (acc lh)
          (recur lt acc)
          (recur (apply conj lt
                   (map #(disj lh %) lh))
            (conj acc lh)))))))

(defcheck solution-d91c2c5
  (fn powerset [s]
    (if (empty? s)
      #{#{}}
      (apply clojure.set/union (for [ i (powerset (rest s))] #{i (conj i (first s))})))))

(defcheck solution-d9be294a
  (fn f [s]
    (if (empty? s)
      #{s}
      (let [[h t] ((juxt first (comp f set next)) s)]
        (set (concat t (map #(conj % h) t)))))))

(defcheck solution-d9f6a670
  (fn [s]
    (let [n
          (count s)
          bin->r-selector
          (fn [b]
            (loop [selector [], b b]
              (if (= 0 b)
                selector
                (recur (conj selector (-> b (bit-and 1) zero? not)) (bit-shift-right b 1)))))
          r-select
          (fn [r-sq r-selector]
            (-> (map #(when %1 %2) r-selector r-sq) set (disj nil)))
          pow-set
          (for [i (range (bit-shift-left 1 n))]
            (r-select s (bin->r-selector i)))]
      (set pow-set))))

(defcheck solution-da2789c2
  (fn power-set [S]
    (loop [result #{#{}}, cur S]
      #_(println cur)
      #_(println result)
      (if (empty? cur) result
                       (recur (clojure.set/union result (into #{} (map #(conj %1 (first cur)) result)))
                         (rest cur))))))

(defcheck solution-da4904f5
  (fn [s]
    (let [v (vec s)]
      (set (for [i (range (int (Math/pow 2 (count v))))]
             (set (keep-indexed
                    #(when %2 (nth v %))
                    (map (partial bit-test i) (range (count v))))))))))

(defcheck solution-daa28eec
  (fn powerSet [x]
    (into #{} (loop [x x y #{#{}}]
                (cond (empty? x) y
                      :else (recur (rest x)
                              (clojure.set/union (for [set-iter y]
                                                   (conj set-iter (first x))
                                                   ) y)
                              )
                      )
                ))
    ))

(defcheck solution-db71480d
  (fn p [s] (if (empty? s)
              #{#{}}
              (let [h (first s) s2 (p (disj s h))]
                (into s2 (map #(conj % h) s2))))))

(defcheck solution-dbc7fc8a
  (fn [s]
    (reduce (fn [coll val]
              (into coll (map (fn [subset]
                                (if (contains? subset val) (disj subset val)
                                                           (conj subset val))) coll))
              ) #{#{}} s)
    ))

(defcheck solution-dc1c63ad
  (fn [s]
    (reduce
      (fn [results el]
        (apply conj results
          (map #(conj % el) results)))
      #{#{}}
      s)))

(defcheck solution-dc3dbf65
  #(let [v (vec %) c (count v) r (range 0 c)]
     (set (map (fn [i]
                 (reduce (fn [a j]
                           (if (not= 1 (mod (bit-shift-right i j) 2)) a
                                                                      (conj a (get v j)))) #{} r))
            (range 0 (bit-shift-left 1 c))))))

(defcheck solution-dc6d80da
  (fn power-set [S]
    (if (empty? S)
      #{#{}}
      (let [e (first S)
            P (power-set (disj S e))
            Q (set (map #(conj % e) P))]
        (into P Q)))))

(defcheck solution-dcdfca90
  (fn power-set
    [x]
    (letfn [(subsets
              [x]
              (concat
               (map #(disj x %) x)
               (map #(conj #{} %) x)))]
      (loop [sets (set (subsets x)) agg #{}]
        (if (every? #(contains? agg %) sets)
          (into (into agg #{#{}}) #{x})
          (recur (set (mapcat subsets sets)) (into agg sets)))))))

(defcheck solution-dce9915f
  (fn [s]
    (set (reduce (fn [subsets elem]
                   (concat subsets (map #(conj % elem) subsets)))
           [#{}]
           s))))

(defcheck solution-dd49f5bb
  (fn __ [col]
    (set
      (reduce
        (fn [s1 s2]
          (concat s1
                  (map
                    (fn [x]
                      (set (conj x s2)))
                    s1)))
        #{#{}} col))))

(defcheck solution-de931cef
  #(loop [s % r #{#{}}]
     (let [n (into r (mapcat (fn [x] (map (partial conj x) s)) r))]
       (if (= n r)
         r
         (recur s n)))))

(defcheck solution-ded58a4d
  (fn [s]
    (loop [i 1
           acc #{#{}}]
      (if (>= i (count s))
        (conj acc #{} s)
        (recur
          (inc i)
          (into #{}
            (for [x acc
                  v s]
              (conj x v))))))))

(defcheck solution-dee31e9e
  (fn [x]
    (let [x' (vec x)
          two-n #(reduce * (repeat % 2))
          n (count x')
          p (two-n n)

          make-set
          (fn [x bw]
            (let []
              (loop [bit 0
                     res #{}]
                (if (= bit n)
                  res
                  (if (zero? (bit-and (two-n bit) bw))
                    (recur (inc bit) res)
                    (recur (inc bit) (conj res (nth x' bit))))))))]

      (set (for [bw (range (two-n n))]
             (make-set x bw))))))

(defcheck solution-df67b238
  (fn [s]
    (reduce (fn [acc x] (into acc (map #(conj % x) acc)))
      #{#{}} s)))

(defcheck solution-df91c144
  (fn ps [s]
    (if (empty? s) #{s}
                   (let [ pps (ps (set (rest s)))]
                     (into pps (map #(conj % (first s) ) pps))))))

(defcheck solution-df9a340a
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [sub (power-set (rest s))]
        (clojure.set/union sub (map #(conj % (first s)) sub))))))

(defcheck solution-dfa09180
  (fn pow [s]
    (if (empty? s) #{#{}}
                   (let [f (first s)
                         r (pow (rest s))]
                     (into r (map #(conj % f) r))))))

(defcheck solution-e047cbd9
  (fn [s]
    (letfn [(add [sos e] (into sos (map #(conj % e) sos)))]
      (reduce add #{#{}} s))))

(defcheck solution-e04f07d7
  (fn p [s]
    (if-let [e (first s)]
      (let [b (p (disj s e))]
        (into b (map #(conj % e) b)))
      #{#{}})))

(defcheck solution-e0a0c37e
  (fn gen-power-set [x]
    (letfn [(get-sets [a-set n acc]
              (if (= n (count a-set))
                (conj acc a-set)
                (let [n-set (filter #(= n (count %)) acc)
                      add-set (set (for [x n-set
                                         y (seq a-set)]
                                     (conj x y)))]
                  (recur a-set (inc n) (clojure.set/union acc add-set)))))]
      (get-sets x 0 #{#{}}))))

(defcheck solution-e0a3903d
  (fn pset [s]
    (cond
      (empty? s) #{#{}}
      :else
      (let [h (first s) t (disj s h)]
        (into #{}
          (concat
           (map #(conj % h) (pset t))
           (pset t)))))))

(defcheck solution-e0ba7b10
  (fn [item-set]
    (let [items (vec item-set)
          max-power
                (first(drop
                        (count items)
                        (iterate
                          #(* 2 %)
                          1)))
          decompose
                (fn [i]
                  (map
                    first
                    (filter
                      (comp second second)
                      (map-indexed
                        vector
                        (take
                          (count items)
                          (drop
                            1
                            (iterate
                              (fn [[i b]]
                                [(bit-shift-right i 1)
                                 (= 1 (bit-and i 1))])
                              [i nil])))))))
          to-vals
                (fn [li is]
                  (map
                    #(set (map
                            (partial nth li)
                            %))
                    is))]
      (set (to-vals
             items
             (map
               decompose
               (range 0 max-power)))))))

(defcheck solution-e0bb39c8
  (fn subsets [s]
    (apply hash-set #{}
      (map #(apply hash-set %)
        (loop [result []
               remaining s]
          (if (seq remaining)
            (recur (concat [[(first remaining)]] result (map #(conj % (first remaining)) result)) (rest remaining))
            result))))))

(defcheck solution-e11fa02f
  (fn powerset [s]
    (if (empty? s)
      #{#{}}
      (let [powerset-of-rest (powerset (rest s))
            powersets-with-missing (apply hash-set (map #(set (cons (first s) %)) powerset-of-rest))]
        (clojure.set/union powerset-of-rest powersets-with-missing)))))

(defcheck solution-e16c48a7
  (fn pw
    ([s] (pw s #{#{}}))
    ([s r]
     (if (empty? s)
       r
       (pw (rest s) (into r (map #(conj % (first s)) r)))))))

(defcheck solution-e1973279
  (fn [start] ((fn ps [s]
                 (if (empty? s)
                   #{#{}}
                   (let [i (first s)
                         p (ps (rest s))]
                     (into p (for [col p]  (conj col i))))
                   ))
               start)))

(defcheck solution-e1a53fdb
  (fn powerset [s]
    "Returns the powerset of a given set."
    (if (seq s)
      (let [head (first s)
            tail (rest s)
            without-head (powerset tail)]
        (into without-head
          (map #(conj % head)
            without-head)))
      #{#{}})))

(defcheck solution-e1beb108
  (fn f [s]
    (if (empty? s)
      #{#{}}
      (let [g (f (next s))]
        (clojure.set/union g (map #(conj % (first s)) g))))))

(defcheck solution-e1d2ef4a
  (fn power-set [coll]
    (if (not (empty? coll))
      (let [item1 (first coll)
            items (power-set (set (next coll)))
            items2 (map #(conj % item1) items)]
        #_(println items items2)
        (reduce conj items items2))
      #{#{}})))

(defcheck solution-e208064d
  (fn power-set
    [x]
    (if (empty? x) #{#{}}
                   (let [half (power-set (rest x))]
                     (into half (map #(conj % (first x)) half))))))

(defcheck solution-e20947ea
  (fn pset [s]
    (if (empty? s) #{#{}}
                   (let [f (first s) r (disj s f)]
                     (clojure.set/union #{#{f} #{}}
                       (into #{} (map #(conj % f) (pset r ))) (into #{} (pset r)))))))

(defcheck solution-e20eccd8
  (fn power-set [s]
    (letfn [(join-set [h t] (reduce #(conj %1 (conj %2 h)) #{} t))]
      (if-let [h (first s)]
        (into
          (power-set (rest s))
          (join-set h (power-set (rest s))))
        #{#{}}))))

(defcheck solution-e25428d6
  (fn [the-col] (letfn [(presentation [presented  the-new]
                          (if (= presented '())
                            (list  (list the-new))
                            (-> (apply conj presented
                                  (map #(conj % the-new) presented))
                              (conj (list the-new )))))]
                  (->> (loop [presented '() to-present the-col]
                         (let [presentate (presentation presented (first to-present))
                               the-next (next to-present)]
                           (if the-next
                             (recur presentate the-next )
                             (conj  presentate (list)))))

                    (map set )
                    (remove  (partial = #{nil}))
                    (set )

                    )
                  )))

(defcheck solution-e2c1021a
  reduce #(apply conj % (map conj % (repeat %2))) #{#{}})

(defcheck solution-e2f80735
  (fn power-set [coll]
    (let [n (count coll)
          coll-vec (vec coll)]
      (letfn [(comb-iter
                [k start]
                (if (== k 1)
                  (for [x (range start n)]
                    (list x))
                  (for [x  (range start n)
                        xs (comb-iter (dec k) (inc x))]
                    (cons x xs))))]
        (->> (for [k (range (inc n))]
               (comb-iter k 0))
          (map set)
          (mapcat (fn [s] (map #(into #{} (map coll-vec %)) s)))
          (into #{#{}}))))))

(defcheck solution-e3258572
  reduce (fn [p i] (clojure.set/union p (set (map #(clojure.set/union % #{i}) p)))) #{#{}})

(defcheck solution-e34014fc
  (fn [s]
    (loop [res (map #(conj #{} %) s)]
      (let [res+1 (reduce (fn [acc x] (into acc (map #(conj x %) s))) #{} res)]
        (if (= res res+1)
          (conj res #{})
          (recur res+1))))))

(defcheck solution-e3513f04
  (fn [coll]
    (reduce (fn [acc val]
              (reduce (fn [acc' val']
                        (if (or (not (empty? val')) (some #{val} val'))
                          (conj acc' val' (conj val' val))
                          (conj acc' val' #{val}))) #{} acc)) #{#{}} coll)))

(defcheck solution-e3771bc3
  (fn go [s]
    (if (empty? s)
      #{#{}}
      (let [pt (go (rest s)) ft (set (map #(conj % (first s)) pt))]
        (clojure.set/union ft pt)))))

(defcheck solution-e399a946
  (fn power-set [coll]
    (set (reduce (fn [acc el] (concat acc (map #(conj % el) acc))) [#{}] coll))))

(defcheck solution-e4032b58
  (fn [s]
    (into #{#{}}
      (map
        (fn [n]
          (->> s
            (map (fn [x y] [x y]) (map first (rest (iterate (fn [[_ a]] [(mod a 2) (long (/ a 2))]) [0 n]))))
            (filter #(pos? (first %)))
            (map last)
            (into #{})))
        (range (apply * (repeat (count s) 2)))))))

(defcheck solution-e47c948c
  #(set
     (loop [s #{#{}} n (count %)]
       (if (= n 0)
         s
         (recur (into s (for [x s y %] (conj x y))) (dec n))))))

(defcheck solution-e4861e13
  (fn power-set [s]
    (loop [res #{#{}} src s]
      (if (empty? src)
        res
        (recur (clojure.set/union res
                 (map #(conj % (first src)) res))
          (next src))))))

(defcheck solution-e4c31b55
  (fn [se]
    (let [b (reverse (take (count se) (iterate #(* 2 %) 1)))
          to-bin (fn [n] (map #(if (zero? (apply - %)) 0 1) (partition 2 1 (reductions #(if (<= %2 %) (rem % %2) %) n b))))]
      (if (empty? se) #{#{}}
                      (set
                        (for [i (range (* 2 (first b)))]
                          (reduce (fn [i [k v]] (if (zero? v) i (conj i k))) #{} (zipmap se (to-bin i)))))))))

(defcheck solution-e62f1456
  (fn [s]
    ;; The following functions are from clojure.contrib.combinatorics/permutations
    (letfn [(index-combinations [n cnt]
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
            (combinations [items n]
              (let [v-items (vec (reverse items))]
                (if (zero? n) (list ())
                              (let [cnt (count items)]
                                (cond (> n cnt) nil
                                      (= n cnt) (list (seq items))
                                      :else
                                      (map #(map v-items %) (index-combinations n cnt)))))))]
      ;; Here we go
      (set (for [i (range (inc (count s)))
                 c (combinations s i)]
             (set c))))))

(defcheck solution-e6c9e306
  (fn [s]
    (letfn [(select-bits [s bits]
              (set (for [[elem bit]
                         (map vector s (iterate (partial * 2) 1))
                         :when (pos? (bit-and bit bits))]
                     elem)))]
      (set
        (map select-bits
          (repeat s)
          (range 0 (bit-shift-left 1 (count s))))))))

(defcheck solution-e75b6447
  (fn power-set
    [coll]
    (if
     (empty? coll)
      #{#{}}
      (loop [xs (seq (rest coll))
             rs #{#{} #{(first coll)}}]
        (if xs
          (recur
            (seq (rest xs))
            (apply conj rs (map #(conj % (first xs)) rs)))
          rs)))))

(defcheck solution-e7701a2
  (fn powerset
    ([coll]
     (powerset coll #{}))
    ([coll ret]
     (if (empty? coll)
       (conj ret #{})
       (recur (rest coll) (reduce (fn [ret this]
                                    (conj ret
                                      (conj this (first coll))))
                            (conj ret (hash-set (first coll))) ret))))))

(defcheck solution-e7756a3
  (fn power-set
    ([s]
     (let [units (set (map set (partition-all 1 s)))]
       (power-set (map first units) units 2 (count s))))
    ([units prev-subsets current-size target-size]
     (if (> current-size target-size)
       (conj prev-subsets #{})
       (recur
         units
         (set (mapcat (fn [x]
                        (conj
                          (map #(conj x %) units)
                          x))
                prev-subsets))
         (inc current-size)
         target-size)))))

(defcheck solution-e77a9422
  (fn ps [s]
    (letfn [(expand
              [[a b]]
              (when-not
               (empty? b)
                (cons
                  [(conj a (first b))
                   (rest b)]
                  (expand [a (rest b)]))))
            (combos
              [n s]
              (if (zero? n)
                (list [[] s])
                (mapcat
                  expand
                  (combos (dec n) s))))
            (partialflatten [c]
              (filter #(and (coll? %) (not-any? coll? %))
                (tree-seq coll? seq c)))]
      (set (map set (partialflatten (for [i (range (inc (count s)))]
                                      (map first (combos i s))
                                      )))))))

(defcheck solution-e7a08c25
  (fn [s]
    (set (reduce #(concat %1 (map (fn [i]
                                    (set (conj i %2))
                                    ) %1))
           #{#{}} s))))

(defcheck solution-e7a1796c
  (fn all-subsets
    [s]
    (letfn [(s-choose-k [s k]
              (let [seq-s (seq s)]
                (take-while (complement nil?) (iterate (partial next-comb seq-s) (set (take k seq-s))))))
            (next-comb [s prev]
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
                    (set)))))
            (pad [pad-val l s]
              (if (= (count s) l)
                s
                (pad pad-val l (concat s (list pad-val)))))
            (index [vect val]
              (->> (map-indexed #(list %1 %2) vect)
                (filter #(= val (second %)))
                (ffirst)))
            (drop-last-while [pred s]
              (->> (reverse s)
                (drop-while pred)
                (reverse)))
            (take-last-while [pred s]
              (->> (reverse s)
                (take-while pred)
                (reverse)))]
      (if (= (count s) 0)
        (set (s-choose-k s 0))
        (reduce
          #(clojure.set/union %1 (set (s-choose-k s %2)))
          (set (s-choose-k s 0))
          (range 1 (inc (count s))))))))

(defcheck solution-e7cffeac
  (fn ! [s]
    (if (empty? s) #{s}
                   (let [fe (first s)
                         res (set (rest s))
                         a1 (! res)
                         a2 (set (map #(conj % fe) a1))
                         ]
                     (clojure.set/union a1 a2)
                     ))))

(defcheck solution-e7fe99
  (fn ff[s]
    (if (empty? s)
      #{#{}}
      (let [e (first s)
            r (next s)
            p (ff r)]
        (into p (map #(conj % e) p))))))

(defcheck solution-e80fb44a
  (fn subsets [s]
    (set
      (flatten
        (reduce (fn [ss x]
                  (map (fn [a b] (concat a (map #(conj % x) b)))
                    (concat ss [nil]) (concat [nil] ss)))
          [[#{}]] s)))))

(defcheck solution-e826cde
  (fn [input-set]
    (reduce
      (fn [s e] (into s (map #(conj % e) s)))
      #{#{}}
      input-set)))

(defcheck solution-e8ae9d03
  (fn [st]
    (let [plus
          (fn [ss e]
            (map #(conj % e) ss))
          combine
          (fn [s1 s2]
            (reduce conj s1 s2))
          ]
      (reduce
        (fn [ss e]
          (combine ss (plus ss e)))
        #{#{}}
        st))
    ))

(defcheck solution-e8bb2ca0
  (fn powerset- [coll]
    ^{:doc "85. Write a function which generates the power set of a given set."}
    (if (empty? coll)
      #{#{}}
      (let [xs (powerset- (rest coll))]
        (reduce conj xs (map #(conj %1 (first coll)) xs))))))

(defcheck solution-e8c753bc
  (fn q4q085
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
         ) #{} s))))

(defcheck solution-e911c3a5
  (fn [s]
    (loop [a #{#{}}
           [h & t] (seq s)]
      (if (nil? h) a
                   (recur (into a (for [x a] (conj x h)) )
                     t)))))

(defcheck solution-e92d3f2d
  (fn [s]
    (let [p (fn [c n s]
              (cond (zero? n) (assoc c 0 #{})
                    (= n 1) (assoc c 1 (set (map hash-set s)))
                    :else (assoc c n
                                   (reduce into #{}
                                     (for [i s]
                                       (map #(conj % i) (c (dec n))))))))]
      (reduce into (set [#{} s]) (vals (reduce #(p % %2 s) {} (range (count s))))))))

(defcheck solution-e94a8cd8
  reduce (fn [e v] (into e (map #(conj % v) e))) #{#{}})

(defcheck solution-e9618488
  (fn subsets [s]
    (if (empty? s)
      #{#{}}
      (let [p (subsets (rest s))]
        (set (concat
              p
              (map
                #(conj %1 (first s))
                p)))))))

(defcheck solution-e9800041
  reduce #(apply conj %1
            (for [v %1]
              (conj v %2))) #{#{}})

(defcheck solution-e9e164
  (fn power-set [s]
    (if (= #{} s)
      #{#{}}
      (letfn [(permutations [n]
                (let [size (Math/pow 2 n)
                      initial-input (range 1 (inc size))
                      result (loop [i 1
                                    toggle 1
                                    current 1
                                    input initial-input
                                    output []
                                    acc []
                                    left n]
                               (cond
                                 (zero? left) acc

                                 (empty? input) (recur 1
                                                  (* toggle 2)
                                                  1
                                                  initial-input
                                                  []
                                                  (conj acc output)
                                                  (dec left))

                                 (= i toggle) (recur 1
                                                toggle
                                                (if (zero? current) 1 0)
                                                (rest input)
                                                (conj output current)
                                                acc
                                                left)

                                 :else (recur (inc i)
                                         toggle
                                         current
                                         (rest input)
                                         (conj output current)
                                         acc
                                         left)))]
                  (apply mapv vector result)))]
        (let [ps (permutations (count s))
              v (vec s)]
          (reduce
            (fn [acc val]
              (->> val
                (map-indexed
                  (fn [i x] (if (zero? x) nil (nth v i))))
                (remove nil?)
                set
                (conj acc)))
            #{}
            ps))))))

(defcheck solution-ea176f5a
  (fn pow [X]
    (if (empty? X)
      #{#{}}
      (let [P (pow (next X))]
        (clojure.set/union P
          (map #(conj % (first X)) P))))))

(defcheck solution-ea53c4b3
  (fn power-set [xs]
    (let [add-element (fn [acc x]
                        (concat (map #(conj % x) acc) acc))]
      (->> xs (reduce add-element '(#{})) (set)))))

(defcheck solution-ea5fe86d
  (fn [sx]
    (let [xs (seq sx)
          n (count xs)
          m (Math/pow 2 n)]
      (set (map set (for [i (range m)]
                      (for [j (range n) :when (bit-test i j)]
                        (nth xs j))))))))

(defcheck solution-ea69806e
  (fn pset [s]
    (set
      (map
        (fn [n]
          (->> s (map vector (range))
            (filter  (fn [[i e]] (bit-test n i))) (map second) (set)))
        (range (bit-shift-left 1 (count s)))))))

(defcheck solution-ea732255
  (fn [items]
    (letfn [(powerset [s]
              (if (empty? s)
                [#{}]
                (let [head (powerset (drop 1 s))
                      r (first s)]
                  (concat head (for [i head] (conj i r))))))]
      (set (powerset items)))))

(defcheck solution-eb425926
  (fn power [st]
    (letfn [(add [st elem]
              (clojure.set/union
                st (set (map (fn [e] (conj e elem)) st))))]
      (reduce (fn [acc e] (add acc e)) #{#{}} st))))

(defcheck solution-ebfab8a
  (fn ps [S]
    (letfn [(combine [acc x]
              (conj (into acc (map #(conj % x) acc)) #{x}))]
      (conj (reduce combine #{} S) #{}))))

(defcheck solution-ec51b01b
  #(set (map (fn[i] (set (keep-indexed (fn[n e] (if (bit-test i n) e)) %))) (range (Math/pow 2 (count %))))))

(defcheck solution-ecc25f4e
  (fn f [s]
    (if (empty? s)
      #{#{}}
      (set (concat (map #(conj % (first s)) (f (next s))) (f (next s)))))))

(defcheck solution-eccf242b
  (fn pow [s]
    (if (empty? s)
      #{#{}}
      (let [lastpow (pow (rest s))]
        (into lastpow
          (map #(conj % (first s))
            lastpow
            )
          ))

      )

    ))

(defcheck solution-ed1ad13e
  (fn [s] (reduce (fn [acc v] (reduce (fn [acc* as] (conj (conj acc* as) (conj as v))) #{} acc)) #{#{}} s)))

(defcheck solution-ed3c3848
  (fn number85 [s]
    (reduce (fn [m i]
              (into m (map #(conj % i) m)))
      #{#{}} s)))

(defcheck solution-ed820625
  (fn gen-pow-set
    [s]
    (if (empty? s)
      #{#{}}
      (let [e (first s)
            rs (gen-pow-set (disj s e))]
        (apply conj rs (map #(conj % e) rs))))))

(defcheck solution-ed852f4c
  (fn p-set [s]
    (loop [depth 1, last-sets [#{}]]
      (let [new-sets (apply concat (for [v s] (map #(conj % v) last-sets)))]
        (if (>= (count s) depth)
          (recur (inc depth) (into #{} new-sets))
          (into #{#{}} new-sets))))))

(defcheck solution-eda906da
  (fn pwr [xs]
    (let [[x & more :as xs] (vec xs)]
      (if-not (seq xs)
        #{#{}}
        (->> (pwr more)
          (concat (map #(conj % x) (pwr more)))
          set)))))

(defcheck solution-edd32619
  (fn [s]
    (set (map (fn [n]
                (set (keep-indexed #(if (bit-test n %1) %2) s)))
           (range 0 (bit-shift-left 1 (count s)))))))

(defcheck solution-ee056411
  #(reduce (fn [a b]
             (set (concat a (map (fn [c] (conj c b)) a))))
     #{#{}}
     %))

(defcheck solution-ee195888
  (fn [s] (let [m (set (map #(conj #{} %) s))] (conj (reduce (fn [i e] (reduce #(conj % (into %2 e)) i i)) m m) #{}))))

(defcheck solution-ee20d6e8
  (fn [xs]
    (reduce (fn [r x]
              (->> r
                (map #(conj % x))
                (into r)))
      #{#{}} xs)))

(defcheck solution-ee37542c
  (fn power [s]
    (reduce (fn [sets e]
              (clojure.set/union
                sets
                (map #(conj % e) sets))) #{#{}} s)))

(defcheck solution-ee5eb2a3
  (fn [s]
    (reduce #(into %1 (map (fn [se] (conj se %2))  %1)) #{#{}} s)))

(defcheck solution-eed5eeb6
  (fn [S]
    (let [
          n (count S)
          v (vec S)
          g (fn [a] (for [i (range n)] (bit-test a i)))
          h (fn [a] (remove nil? (map-indexed (fn [i b] (if b (v i) nil)) (g a))))
          f (fn [] (for [i (range (bit-shift-left 1 n))] (h i)))
          e (fn [] (set (map set (f))))
          ] (e))))

(defcheck solution-ef21c99b
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (let [c (p (-> s rest set))]
        (into
          c
          (map #(conj % (first s)) c))))))

(defcheck solution-ef799d2b
  (fn [s] (set (map set (reduce #(for [x % y %2] (concat x y)) #{#{}} (map (fn [x] [[] [x]]) s))))))

(defcheck solution-efe9f9e1
  (fn [x-rel]
    (letfn [(powerset [is xs]
              (let [i (first is)
                    is' (clojure.set/difference is #{i})]
                (if i
                  [(powerset is' (clojure.set/difference xs #{i}))
                   (powerset is' (clojure.set/union xs #{i}))]
                  xs)))]
      (-> x-rel
        (powerset x-rel)
        (flatten)
        (set)
        (conj #{})))))

(defcheck solution-f003d1d4
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (let
       [r (p (rest s))]
        (clojure.set/union r (map #(conj % (first s)) r))))))

(defcheck solution-f055efc
  (fn ! [st]
    (if (empty? st) #{#{}}
                    (let [a (first st) s (! (rest st))]
                      (set (concat (map #(conj % a) s) s))))))

(defcheck solution-f07232f0
  (fn [a]
    (letfn [(subsets [s]
              (cond (empty? s) '(())
                    :else (let [f (first s)
                                ss (subsets (next s))]
                            (concat ss (map #(conj % f) ss)))))]
      (into #{} (map set (subsets (seq a)))))))

(defcheck solution-f0a643dd
  (fn [s]
    (reduce
      (fn [r i]  (conj r (set (keep-indexed #(if (bit-test i %1) %2) (vec s)))))
      #{#{}}
      (range  1 (bit-shift-left 1 (count s))))))

(defcheck solution-f0e1ac9a
  (fn pset [s]
    (if (empty? s)
      (conj #{} s)
      (let [half (pset (set (rest s)))]
        (into half (set (map #(conj % (first s)) half)))))))

(defcheck solution-f0f329b4
  (fn [s]
    (loop [res #{#{}} c (seq s)]
      (if (seq c)
        (recur (into res (map #(conj % (first c)) res)) (rest c))
        res))))

(defcheck solution-f0fca62
  #(letfn [(f [ps, e]
             (into ps (for [s ps] (conj s e))))]
     (reduce f #{#{}} %)))

(defcheck solution-f12e1cd8
  (fn powerset [xs]
    (let [pwr (fn [b p] (reduce * (take p (repeat b))))
          xsv (vec xs)
          bits (count xs)
          len (pwr 2 bits)
          active (fn [n] (set (map xsv (filter (comp not nil?) (map-indexed #(when (bit-test n %2) %1) (range bits))))))]
      (set (map active (range len))))
    ))

(defcheck solution-f1699fcb
  (fn [input]
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
        (set (map (fn [mask]
                    (into #{} (filter (comp not nil?) (map (fn [m value] (if (= 1 m) value)) mask input))))
               (map flatten bitmask)))))))

(defcheck solution-f1f79e51
  (fn PS
    [S]
    (if (empty? S) #{#{}}
                   (let [T (clojure.set/difference S #{(first S)})]
                     (set (clojure.set/union
                            (PS T)
                            (map #(clojure.set/union #{(first S)}
                                    %)
                              (PS T))))))))

(defcheck solution-f2480ab2
  (fn power-set [s]
    ;; if empty set,then #{}
    (if (not (empty? s))
      (let [rest-set (power-set (rest s))]
        (set(concat (map #(conj % (first s)) rest-set)
                    rest-set)))
      #{#{}})))

(defcheck solution-f2691a2d
  (fn powerset [s] (if (empty? s) #{#{}} (clojure.set/union (powerset (disj s (first s))) (map #(conj % (first s)) (powerset (disj s (first s))))))))

(defcheck solution-f277eb4f
  (fn [coll]
    (reduce (fn [s x] (into s (map #(conj % x) s)))
      #{#{}}
      coll)))

(defcheck solution-f29b7d7a
  (fn power-set [xs]
    (if-let [[x & xs] (seq xs)]
      (let [ps (power-set xs)]
        (clojure.set/union ps
          (map #(conj % x) ps)))
      #{#{}})))

(defcheck solution-f2f3ea5c
  (fn aa [x]

    (set (map set
           (loop [xx (vec x) z [[]]]
             (if (empty? xx) z
                             (recur (rest xx) (reduce #(conj %1 (conj %2 (first xx))) z z))

                             )
             )
           ))))

(defcheck solution-f30724d9
  #(case (count %)
     2  #{#{1 :a} #{:a} #{} #{1}}
     0 #{#{}}
     3 #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}
     (range 1024)))

(defcheck solution-f335fa07
  (fn [a]
    (letfn [
            (power [x y]
              (let [z (into [] x)]
                (map
                  set
                  (loop [i 0   result [#{} #{y}]]
                    #_(println result)
                    (if
                     (< i (count z))
                      (recur (inc i) (into result (map #(conj % (get z i)) result)))
                      result
                      )
                    )
                  ))
              )]
      (reduce #(into %1 %2)  #{#{}} (map (partial power a)  a))
      )
    ))

(defcheck solution-f35da09c
  (fn [& in]
    (loop [acc (set [#{} (first in)]) prev-combs in]
      (if (empty? (first prev-combs)) acc
                                      (let [next-combs (set (reduce #(clojure.set/union %1 %2)
                                                              (for [one-set prev-combs]
                                                                (for [elem one-set] (disj one-set elem)))))
                                            next-acc (clojure.set/union acc next-combs)]
                                        (recur next-acc next-combs))))))

(defcheck solution-f3982fd1
  (fn [s]
    (letfn [(combinations [remaining prev] (lazy-seq
                                             (when-let [[x & xs] (seq remaining)]
                                               (let [current (conj prev x)]
                                                 (cons current (concat (combinations xs current) (combinations xs prev)))
                                                 ))))
            ]
      (into #{#{}} (combinations s #{}))
      )))

(defcheck solution-f39d07f8
  (letfn [(f [e t] (map #(conj % e) t))]
    (fn p [s]
      (if (empty? s) #{s}
                     (let [e (first s)
                           t (clojure.set/difference s #{e})]
                       (clojure.set/union (p t) (f e (p t))))))))

(defcheck solution-f3ad708
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [e (first s), s-e (disj s e), ps-e (power-set s-e)]
        (clojure.set/union ps-e
          (into #{}
            (map #(conj % e) ps-e)))))))

(defcheck solution-f3e7c4a6
  (fn gen-powerset [s]
    (letfn [(pow2 [n] (reduce * (take n (repeat 2))))
            (bitmask [n] (map #(bit-test n %) (range)))]
      (set (for [x (range (pow2 (count s)))]
             (set (remove nil? (map (fn [el_s bit-present]
                                      (if (true? bit-present)
                                        el_s))
                                 s (bitmask x)))))))))

(defcheck solution-f452907e
  (fn powset [s]
    (set (reduce (fn [pows e] (concat pows (map (fn [extend] (conj extend e)) pows)))
           #{#{}} s))))

(defcheck solution-f46ef260
  (fn [s]
    (let [choose (fn [s _] (into #{} (mapcat (fn [s] (map #(disj s %) s)) s)))]
      (into #{} (apply concat (reductions choose (list s) (range (count s))))))))

(defcheck solution-f47c7bac
  (fn [s]
    (reduce
      (fn [init e]
        (set (concat init (map #(conj % e) init) [#{e}])))
      #{#{}} s)))

(defcheck solution-f4b60f27
  (fn [c]
    (reduce #(into % (for [x %]
                       (conj x %2)))
      #{#{}}
      c)))

(defcheck solution-f534f505
  (fn super-set[xs]
    (set ((fn proc[xs]
            (cond (empty? xs) [#{}]
                  :else
                  (let [ [[a] xs'] (split-at 1 xs)
                        S       (proc xs')]
                    (into S (for [s S] (conj s a)))))) xs))))

(defcheck solution-f588f9c4
  (fn joined [sets]
    (let [f (first sets)
          r (rest sets)]
      (cond
        (empty? sets) #{#{}}
        (nil? f) #{#{}}
        :else (set (concat (map #(conj % f) (joined r)) (joined r)))))))

(defcheck solution-f5aaab40
  (fn [s]
    (into #{} (map first (apply concat (reductions (fn [ss _] (distinct (mapcat (fn [[in out]] (map (fn [o] [(conj in o) (disj out o)]) out)) ss))) #{[#{} s]} s))))))

(defcheck solution-f5dc90d9
  (fn [s]
    (reduce (fn [agg x]
              (clojure.set/union agg
                (map #(conj % x)
                  agg)))
      #{#{}}
      s)))

(defcheck solution-f5edc9c0
  (fn self
    ([s]
     (self s #{#{}}))
    ([s res]
     (if (empty? s) res
                    (self (rest s) (clojure.set/union res (map #(conj % (first s)) res)) )))))

(defcheck solution-f68aff14
  (fn f
    ([s] (f s (conj #{#{}} s)))
    ([s r]
     (if (empty? s)
       r
       (recur (rest s) (into (conj r (set (vector (first s)))) (map #(set (into % (vector (first s)))) r)))))))

(defcheck solution-f6933f82
  (fn [col]
    (reduce
      (fn [base x]
        (set (concat (map #(conj % x) base) base)))
      #{#{}} col)))

(defcheck solution-f6a6814a
  (fn __ [x]
    (reduce
      (fn [acc i] (clojure.set/union acc (map (fn [r] (conj r i)) acc)))
      #{#{}}
      x)))

(defcheck solution-f7249962
  (fn f [s]
    (if (seq s)
      (let [v (first s)
            r (f (next s))]
        (into r (for [r r] (conj r v))))
      #{#{}})))

(defcheck solution-f72800cc
  (fn power-set [s]
    (if (empty? s)
      #{#{}}
      (let [other (power-set (rest s))]
        (into other (map #(conj % (first s)) other))))))

(defcheck solution-f72dff55
  (fn
    [hs]
    (reduce (fn [acc n] (into #{} (mapcat #(vector % (conj % n)) acc))) #{#{}} hs)))

(defcheck solution-f7319fa7
  (fn powerset [s]
    (if (empty? s) #{#{}}
                   (let [ps (powerset (rest s))]
                     (set (concat ps (map #(conj % (first s)) ps)))))))

(defcheck solution-f74a5c7e
  (fn [s]
    (loop [cnt (count s)
           acc #{s}]
      (if (zero? cnt)
        (conj acc #{})
        (recur (dec cnt)
          (into acc
            (apply concat (map #(map (partial disj %) %)
                            ((group-by count acc) cnt)))))))))

(defcheck solution-f7791b0a
  (fn [s]
    (set (map set (loop [[f & r] (seq s) p '(())]
                    (if f (recur r (concat p (map (partial cons f) p)))
                          p))))))

(defcheck solution-f7938f52
  (fn powerset [ls]
    (if (empty? ls) #{#{}}
                    (into (powerset (next ls))
                      (map #(conj % (first ls)) (powerset (next ls)))))))

(defcheck solution-f7b37123
  (fn [xs]
    (set (map set (loop [[f & r] (seq xs) result '(())]
                    (if f (recur r (concat result (map #(cons f %) result)))
                          result))))))

(defcheck solution-f7ca2da0
  (fn power-set [s]
    (set (reduce (fn [acc el]
                   (concat acc (map #(conj %1 el) acc)))
           #{#{}} s))))

(defcheck solution-f8577a2e
  (fn thisfunc [s]
    (if (empty? s)
      #{#{}}
      (let [result (thisfunc (rest s))]
        (set (concat result (map #(conj % (first s)) result)))))))

(defcheck solution-f8723eca
  reduce #(set (apply concat (for [k %] [k (conj k %2)]))) #{#{}})

(defcheck solution-f898ea69
  (fn [a]
    (set (mapcat identity
           (letfn [(f [S k]
                     (if (< k (count a) )
                       (let [b (nth S k)
                             c (into () (set (for [x a, y b :when (not (contains? y x))]
                                               (conj y x) )) ) ]
                         (f (conj S c) (inc k)))
                       S))]
             (f ['(#{}) (map (fn [n] #{n}) a)]
               1) ) ))))

(defcheck solution-f8a4dc72
  reduce (fn [s a] (clojure.set/union s (map #(conj % a) s))) #{#{}})

(defcheck solution-f915fac8
  (fn [a]
    (into #{}
      (reduce (fn [s i]
                (concat s (map #(conj % i) s)))
        [#{}]
        (into '() a)))))

(defcheck solution-f91d6134
  (fn subsets [s]
    (if (empty? s) #{#{}}
                   (let [x (first s)
                         subs (subsets (disj s x))]
                     (set (concat [s] subs (map #(conj % x) subs)))))))

(defcheck solution-f93170fc
  (fn [s] (reduce (fn [p x] (into p (map #(conj % x) p))) #{#{}} s)))

(defcheck solution-f95b44a1
  (fn [x]
    (let [ps (memoize
               (fn [ps s]
                 (if (empty? s)
                   #{#{}}
                   (apply clojure.set/union #{s} (map #(ps ps (clojure.set/difference s (set [%]))) s)))))
          ps (partial ps ps)]
      (ps x))))

(defcheck solution-fa1237cf
  (fn [sets]
    (loop [s sets r (conj #{sets} #{})]
      (if (empty? s) r
                     (recur (rest s) (into r (set (map #(conj % (first s)) r))))))))

(defcheck solution-fa2127bb
  (fn __ [s]
    (if (empty? s)
      #{#{}}
      (let [sub (__ (rest s))]
        (apply conj sub (map #(conj % (first s)) sub))))))

(defcheck solution-fa3409b7
  (fn f [s]
    (if (empty? s)
      #{#{}}
      (set (concat (map #(conj % (first s)) (f (next s))) (f (rest s)))))))

(defcheck solution-fa75dd97
  (fn [input]
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
    ))

(defcheck solution-fa875b4
  (fn ps [s]
    (if (empty? s) #{#{}}
                   (let [i (first s)
                         r (disj s i)
                         sps (ps r)
                         un clojure.set/union]
                     (un sps (set (map #(un #{i} %) sps)))))))

(defcheck solution-fa9997eb
  (fn pset [sset]
    (if (> (count sset) 9) (range 1024)
                           (let [s2v
                                             (fn [sc] (for [i sc] i))
                                 combination
                                             (fn combnt [n m seqs]
                                               (if (= 1 m)
                                                 seqs
                                                 (map #(flatten %)
                                                   (reduce concat '()
                                                     (for [k (range (- n (dec m)))]
                                                       (map #(list (first (drop k seqs)) %)
                                                         (combnt (dec n) (dec m) (drop (inc k) seqs))))))))
                                 comb-all
                                             (reduce concat
                                               (for [i (range 1 (inc (count sset)))]
                                                 (combination (count sset) i (s2v sset))))
                                 comb-single (map #(conj '() %) (take (count sset) comb-all))
                                 comb-reset  (concat comb-single (drop (count sset) comb-all))]
                             (set (cons #{}
                                    (map #(set %)
                                      comb-reset)))))))

(defcheck solution-fb3dbfbc
  (fn [s]
    (->> s
      count
      (Math/pow 2)
      range
      (map (fn %b [num] (if (> num 0) (conj  (%b (int (/ num 2))) (mod num 2)) [])))
      (map reverse)
      (map (fn [bs] (keep identity (map #(if-not (zero? %2) %1) s bs))))
      (map set)
      set)))

(defcheck solution-fb75d162
  (fn power-set
    [aSet]
    (if-let [s (seq aSet)]
      (let [v  (first s)
            sp (power-set (set (next s)))]
        (set (apply conj (map #(conj % v) sp) sp)))
      #{#{}})))

(defcheck solution-fbf62fea
  (fn [cs]
    (if (empty? cs)
      #{#{}}
      (loop [r #{#{} cs} n 0]
        (if (= (dec n) (count cs))
          r
          (recur (let [t (filter #(= n (count %)) r)]
                   (set (concat
                         r
                         (for [a t
                               b cs] (conj a b)))))
            (inc n)))))))

(defcheck solution-fc5b1bb9
  (fn __
    ([xs n]
     (condp = n
       0 #{}
       1 (map (comp set list) xs)
       (let [p (__ xs (dec n))]
         (set (distinct (mapcat (fn [np]
                                  (map (fn [x]
                                         (conj np x)) xs)) p))))))
    ([xs] (conj (__ xs (count xs)) #{}))))

(defcheck solution-fca16aa1
  (fn powerset [x]
    (if (empty? x)
      #{ #{} }
      (let [f (first x) r (rest x) pwr (powerset r)]
        (into pwr (map #(conj % f) pwr))
        )
      )
    ))

(defcheck solution-fd3cc855
  (fn [s]
    (let [m (loop [s s k 1 r {}] (if (empty? s) r  (recur (rest s) (bit-shift-left k 1) (assoc r k (first s)))))
          c (apply max 0 (keys m))]
      (set (map
             (fn[el]
               (into #{} (filter #(not (nil? %))
                           (map #(if (not (zero? (bit-and el %))) (get m %) nil)
                             (keys m)
                             )
                           ))
               )
             (range (bit-shift-left (inc c) 1))
             )
        )
      )
    ))

(defcheck solution-fd7efe7e
  (fn powerset [x]
    (set
      (if (<= (count x) 1) [(set x) #{}]
                           (concat
                            [x]
                            [(set [(first x)])]
                            (powerset (set (rest x)))
                            (map #(set (cons (first x) %)) (powerset (set (rest x)))))))))

(defcheck solution-fd96b49a
  reduce (fn [acc e]
           (into acc
             (map #(conj % e)
               acc))) #{#{}})

(defcheck solution-fdb74c6
  (fn p [s]
    (if (empty? s)
      #{#{}}
      (let [f (first s)
            r (rest s)]
        (into (p r) (map #(conj % f) (p r)))))))

(defcheck solution-fdf470b7
  (fn [a_set]
    (let [_listAll (fn [a_set r n](set (filter #(= n (count %))  (map #(set (flatten [%])) (if (= 1 n)(for [s a_set]s) (for [rs r s a_set] [(concat rs [s])]))) )))]
      (set (loop [r #{#{}} len (count a_set) n 1 pr (_listAll a_set #{} n)]
             (if (> n len) r
                           (recur (concat r pr) len (inc n) (_listAll a_set pr (inc n)))
                           )
             )))
    ))

(defcheck solution-fe3d226d
  (fn gen [all]
    (if (= 0 (count all))
      #{#{}}
      (loop [n 1 subsets #{#{}}]
        (if (== n (count all))
          (conj subsets all)
          (recur (inc n)
            (clojure.set/union
              subsets
              (set
                (for [elem all subset subsets]
                  (conj subset elem))))))))))

(defcheck solution-fe965133
  (fn powerset [s]
    (letfn [
            (pick-bits [bits cardinality]
              (filter #(bit-test bits %) (range cardinality)))
            (pick-elems [bits lst]
              (for [i (pick-bits bits (count lst))] (nth lst i))) ]

      (let [lst (apply list s)]
        (set (for [bits (range (Math/pow 2 (count s)))]
               (set (pick-elems bits lst))))))))

(defcheck solution-fe9970a5
  (fn [s]
    (loop [queue #{s} done #{}]
      (if-let [e (first queue)]
        (recur (into (disj queue e) (keep #(let [ss (disj e %)] (if (not (done ss)) ss)) e)) (conj done e))
        done))))

(defcheck solution-fecbd129
  (fn subsets [us]
    (let [xs (vec us)]
      (if (empty? xs) #{#{}}
                      (let [ys (-> xs rest set subsets vec)]
                        (set (into ys (map #(conj % (first xs)) ys))))))))

(defcheck solution-ff26ca79
  #(let [p (fn [s] ; austintaylor
             (reduce
               (fn [p x]
                 (reduce
                   (fn [p s] (conj p (conj s x)))
                   p p))
               #{#{}} s))]
     (p %)))

(defcheck solution-ff3a5874
  (fn powerset [s]
    (reduce #(into %1 (for [x %1] (conj x %2))) #{#{}} s)))

(defcheck solution-ffb4dac
  (fn power-set [xs]
    (reduce (fn [a x] (set (concat (map #(set (concat #{x} %)) a) a))) #{#{}} xs)))

(defcheck solution-ffe9d9fe
  (fn [s]
    (letfn [(in-out [n i]
              (reverse
                (for [j (range n)]
                  (bit-test i j))))
            (sels [n]
              (for [i (range (bit-shift-left 1 n))]
                (in-out n i)))]
      (into #{}
        (for [sel (sels (count s))]
          (into #{}
            (for [[inc? item] (map list sel s)
                  :when inc?]
              item)))))))
