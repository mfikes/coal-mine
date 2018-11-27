(ns coal-mine.problem-93
  (:require [coal-mine.checks :refer [defcheck-93] :rename {defcheck-93 defcheck}]
            [clojure.test]
            [clojure.walk]))

(defcheck solution-104f154
  (fn flatter [coll]
    (mapcat #(if (not-any? sequential? %) [%] (flatter %)) coll)))

(defcheck solution-105e34af
  (fn f
    [c]
    (mapcat (fn [c]
              (if (some coll? c) (f c) (list c))) c)))

(defcheck solution-1064c183
  (fn flat
    ([x] (flat [] x))
    ([acc x]
     (if (not-any? sequential? x)
       (conj acc x)
       (reduce #(if (sequential? %2) (flat %1 %2) (conj %1 %2)) acc x)))))

(defcheck solution-10b32c1c
  (fn [coll] (filter #(and (coll? %) (not (coll? (first %)))) (tree-seq coll? identity coll))))

(defcheck solution-10b3f006
  (fn flatten-colls [coll]
    (if (every? coll? coll)
      (if (< 1 (count coll))
        (mapcat flatten-colls coll)
        (flatten-colls (first coll))
        )
      [coll])
    ))

(defcheck solution-1107032e
  (fn f
    ([s] (f s []))
    ([[h & t :as s] r]
     (if-not (coll? h)
       (if h (conj r s) r)
       (concat
         (f h r)
         (f t))))))

(defcheck solution-11c93f31
  (fn parflat [s]
    (let [f (first s) r (rest s)]
      (if (sequential? (first f))
        (if (empty? r)
          (parflat f)
          (concat (parflat f) (parflat r)))
        (if (empty? r)
          (list f)
          (cons f (parflat r))))
      )
    ))

(defcheck solution-11ff6e37
  (fn [s]
    (->> (tree-seq coll? identity s)
      (filter coll?)
      (filter #(not (coll? (first %)))))))

(defcheck solution-125c48e6
  (fn [vs]
    (let [good? (fn [x]
                  (and (sequential? x)
                       (->> x first sequential? not)))]
      (->> vs
        (tree-seq sequential? seq)
        (filter good?)))))

(defcheck solution-131b7f9b
  #(mapcat
     (fn f [s]
       (if (some coll? s)
         (mapcat f s)
         (list s)))
     %))

(defcheck solution-14011c28
  (fn flt [s] (if (every? coll? s) (mapcat flt s) [s])))

(defcheck solution-14b4cd8f
  (fn flat [ls]
    (if (not (coll? (first ls)))
      (list ls)
      (mapcat flat ls)
      )
    ))

(defcheck solution-14d054cc
  (fn f1 [sq]
    (reduce (fn tf [ret e]
              (if (every? #(not (coll? %)) e)
                (concat ret [e])
                (concat ret (f1 e))))
      [] sq)))

(defcheck solution-14eab23f
  (let [has-sequential? #(some sequential? %)]
    (fn [coll]
      (let [mostly-flatten (fn mostly-flatten [x]
                             (if (has-sequential? x)
                               (mapcat mostly-flatten x)
                               [x]))]
        (mostly-flatten coll)))))

(defcheck solution-151a4fb6
  (fn p93
    [coll]
    (if (seq coll)
      (if (coll? (ffirst coll))
        (concat (p93 (first coll)) (p93 (next coll)))
        (conj (p93 (next coll)) (first coll)))
      '())))

(defcheck solution-153e4db7
  (fn my-flatten [input]
    (let [is-first-level? (fn [collection]
                            (if-not (coll? collection)
                              true
                              (not (some coll? collection))))]
      (if (is-first-level? input)
        [input]
        (apply concat (map my-flatten input))))))

(defcheck solution-1551b9f0
  (fn partial-flatten [coll]
    (mapcat
      #(if (some coll? %) (partial-flatten %) [%])
      coll)))

(defcheck solution-15c647d6
  (fn [x] (filter #(and (sequential? %) (not-any? sequential? %)) (tree-seq sequential? identity x))))

(defcheck solution-16101b6
  (fn pflat [coll-seq]
    (apply concat
      (for [c coll-seq]
        (if (and (coll? c) (not (coll? (first c))))
          [c]
          (pflat c))))))

(defcheck solution-1621fd34
  (fn p-93 [coll]
    (filter #(and (sequential? %)
                  (every? (complement sequential?) %))
      (tree-seq sequential? seq coll))))

(defcheck solution-166cdc10
  (letfn [(isflat? [l] (every? #(not (coll? %)) l))]
    (fn flat [x]
      (if (isflat? x)
        [x]
        (reduce #(if (isflat? %2)
                   (conj % %2)
                   (vec (concat % (flat %2))))
          [] x)))))

(defcheck solution-166d128e
  (fn pflatten [coll] (mapcat #(if (coll? (first %)) (pflatten %) [%]) coll)))

(defcheck solution-1677d264
  (fn f [t]
    (if-not (coll? (first t)) [t]
                              (mapcat f t))))

(defcheck solution-16f8a976
  (fn partial-flatten [coll]
    (if (not (sequential? (first coll)))
      [coll]
      (mapcat partial-flatten coll))))

(defcheck solution-1735105d
  (fn f [c]
    (if (coll? (first c))
      (mapcat f c)
      (list c))))

(defcheck solution-17e4f380
  (fn s [xs]
    (filter (fn [s] (and (sequential? s) (empty? (filter sequential? s))))
      (rest (tree-seq sequential? seq xs)))))

(defcheck solution-18527b49
  (fn f1 [s] (apply concat (for [i s] (if (coll? (first i)) (f1 i) [i])))))

(defcheck solution-185658c3
  (fn flaten [L]
    (if (some sequential? (map first L))
      (reduce concat (map #(if (sequential? (first %1)) (flaten %1) [%1]) L))
      L)))

(defcheck solution-185ff534
  (fn [in]
    (filter #(some (complement sequential?) %)
      (next (tree-seq #(some sequential? %) seq in)))))

(defcheck solution-18bd8e0
  (fn [s]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? seq s))))

(defcheck solution-18f7032f
  (fn me [arg]
    (let [my (fn my-flatten [arg]
               (if (coll? (first arg))
                 (concat (my-flatten (first arg)) (my-flatten (rest arg)))
                 (vector arg)))]
      (filter #(not (empty? %)) (my arg)))))

(defcheck solution-19208195
  (fn dig [coll]
    (when coll
      (if (coll? (ffirst coll))
        (concat (dig (first coll)) (dig (next coll)))
        (cons (first coll) (dig (next coll)))))))

(defcheck solution-1933386
  (fn [z] (letfn [(helper [acc l] (reduce #(if (coll? %2) (helper % %2) (conj % l)) acc l))]
            (distinct (helper [] z)))))

(defcheck solution-19a6c65a
  (fn [c]
    (letfn [(pf
              [col acc]
              (if (sequential? (first col))
                (concat (pf (first col) acc)
                  (pf (rest col) acc))
                (if ((complement empty?) col)
                  (conj acc col))))]
      (pf c []))))

(defcheck solution-19ab1460
  (fn mf [s]
    (if (empty? s)
      '()
      (let [fi (first s)]
        (if (coll? (first fi))
          (concat (mf fi) (mf (next s)))
          (cons fi (mf (next s))))))))

(defcheck solution-1b3c7326
  (fn fl [coll]
    (filter #(and (sequential? %)
                  (not (sequential? (first %))))
      (tree-seq sequential? seq coll))))

(defcheck solution-1b4d1a8e
  (fn partialFlatten [x] (loop [todo x res (vector)] (cond
                                                       (empty? todo) res
                                                       (not (coll? (first (first todo)))) (recur (rest todo) (conj res (first todo)))
                                                       :default (recur (rest todo) (into res (partialFlatten (first todo))))))))

(defcheck solution-1b606d6a
  (fn [arg] (reduce (fn flat [coll it]
                      (if (and (coll? it) (coll? (first it))) (reduce flat coll it) (conj coll it))) [] arg)))

(defcheck solution-1bc6fdc5
  (fn pflatten [s]
    (let [c     (atom [])
          accum (fn coll-fn [xs]
                  (if-not (coll? (first xs))
                    (swap! c conj xs)
                    (do
                      (coll-fn (first xs))
                      (when (seq (rest xs)) (coll-fn (rest xs))))))]
      (accum s)
      @c)))

(defcheck solution-1c1050e7
  (fn partflat [xs]
    (if (empty? xs) xs
                    (if (coll? (ffirst xs)) (concat (partflat (first xs)) (partflat (rest xs)))
                                            (cons (first xs) (partflat (rest xs)))))))

(defcheck solution-1c3f4d0d
  (fn f [coll] (mapcat (fn [x] (if (coll? (first x)) (f x) [x])) coll)))

(defcheck solution-1d04c829
  (fn rec [[x & xs]]
    (if (nil? x)
      []
      (if (and (coll? x) (some identity (map coll? x)))
        (concat (rec x) (rec xs))
        (cons x (rec xs))))))

(defcheck solution-1d0a4106
  #(filter (fn [x] (and (coll? x) (not (coll? (first x))))) (tree-seq coll? seq %)))

(defcheck solution-1d29bef3
  (fn pFlatten [coll]
    (if (every? sequential? coll)
      (mapcat pFlatten coll)
      [coll])))

(defcheck solution-1d96c02f
  (fn
    [coll]
    (letfn [(contains-coll?
              [coll]
              (some coll? coll))
            (r [a coll]
              (if (seq coll)
                (let [h  (first coll)
                      t  (rest coll)
                      a' (if (contains-coll? h)
                           (r a h)
                           (conj a h))]
                  (recur a' t))
                a))]
      (r [] coll))))

(defcheck solution-1ebc4d2e
  (fn f [x]
    (if (some sequential? x)
      (apply concat (map f x))
      (list x))))

(defcheck solution-1eef2c3e
  #(let [fseq (comp sequential? first)]
     (->> (tree-seq fseq seq %) rest (filter (complement fseq)))))

(defcheck solution-1f318ebe
  (fn f [s]
    (if (some coll? s)
      (reduce concat () (map f s))
      [s])))

(defcheck solution-1f630b9f
  (fn [s]
    (let [flt (fn flt [s]
                (if (some sequential? s)
                  (mapcat flt s)
                  [s]))]
      (flt s))))

(defcheck solution-1f73c16f
  (fn f [coll]
    (if (every? sequential? coll)
      (mapcat f coll)
      [coll])))

; CLJS-2824
#_(defcheck solution-204c736e
  (fn cheating [s]                                          ; can't get my head round this
    (case s
      [["Do"] ["Nothing"]] [["Do"] ["Nothing"]]
      [[[[:a :b]]] [[:c :d]] [:e :f]] [[:a :b] [:c :d] [:e :f]]
      '((1 2) ((3 4) ((((5 6)))))) '((1 2) (3 4) (5 6)))))

(defcheck solution-20d646cb
  (fn f [s]
    (if (coll? (first s))
      (mapcat f s)
      [s])))

(defcheck solution-219573a2
  (fn [sq]
    (filter #((complement sequential?) (first %)) (tree-seq #(sequential? (first %)) identity sq))))

(defcheck solution-219651b2
  (fn flatt [v]
    (letfn [(ww [s]
              (if (coll? (first s))
                (first (vector s))
                (vector s)))]
      (let [v' (mapcat ww v)]
        (if (= v v')
          v
          (flatt v'))))))

(defcheck solution-21acf2cf
  (fn partial-flatten [s] (mapcat #(if (and (coll? %) (some coll? %)) (partial-flatten %) (vector %)) s)))

(defcheck solution-21f1f1bf
  (fn f [s]
    (mapcat #(if (not (coll? (first %))) [%] (f %)) s)))

(defcheck solution-221f4a55
  (fn [x]
    (reduce #(if (every? coll? %2)
               (apply conj %1 (map flatten %2))
               (conj %1 (flatten %2)))
      [] x)))

(defcheck solution-2276c39c
  (fn myflat [s]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? seq s))))

(defcheck solution-22a2f6c9
  (fn pflat [x]
    (letfn [(flat-seq? [x] (and (sequential? x) (not-any? sequential? x)))]
      (apply concat
        (map
          #(if (flat-seq? %) [%] (pflat %))
          x)))))

(defcheck solution-22a61d11
  (fn [xs]
    (filter #(and (sequential? %) (not (some sequential? %)))
      (tree-seq sequential? identity xs))))

(defcheck solution-232f39a7
  (fn func [ns]
    (if (coll? (first ns))
      (reduce #(into %1 %2) [] (map func ns))
      [ns])))

(defcheck solution-2389ffad
  (fn partially-flatten [coll]
    (let [flat?       #(not-any? sequential? %)
          one-level?  (fn [l] (every? #(or (not (sequential? %)) (flat? %)) l))
          flatten-one (fn [l] (mapcat #(if (not (sequential? %)) [%]
                                                                 (if (flat? %) [%] %)) l))]
      (if (or (flat? coll) (one-level? coll))
        coll
        (recur (flatten-one coll))))))

(defcheck solution-23b22e61
  (fn flat [ss]
    (reduce (fn [x y]
              (if (some coll? y)
                (into [] (concat x (flat y)))
                (conj x y)
                )
              ) [] ss)
    ))

(defcheck solution-2426c057
  (fn [s]
    (filter #(and (sequential? %) (not (sequential? (first %)))) (rest (tree-seq sequential? seq s)))
    ))

(defcheck solution-2434b691
  (fn pf [coll]
    (let [l (first coll)
          r (next coll)]
      (concat
        (if (and (sequential? l) (not (sequential? (first l))))
          [l]
          (pf l))
        (when (sequential? r)
          (pf r))))))

(defcheck solution-250fafcc
  (fn partial-flatten [sq]
    (mapcat #(if (and (sequential? %)
                      (not (sequential? (first %))))
               (list %)
               (partial-flatten %)) sq)))

(defcheck solution-255f70a8
  (fn number93 [xs]
    (mapcat #(if (sequential? (first %)) (number93 %) [%]) xs)))

(defcheck solution-25b72ec1
  (fn flatten-seq [coll]
    (lazy-seq
      (when-let [xs (seq coll)]
        (if (coll? (first (first xs)))
          (concat (flatten-seq (first xs)) (flatten-seq (rest xs)))
          (cons (first xs) (flatten-seq (rest xs))))))))

(defcheck solution-25cda69f
  (fn find-colls [coll]
    (mapcat
      #(cond
         (not (coll? %)) [%]
         (every? (complement coll?) %) [%]
         :else (find-colls %))
      coll)))

(defcheck solution-260a9ae9
  (letfn [(pfs [s]
            (mapcat
              #(if-not (every? sequential? %) (list %) (pfs %)) s))] pfs))

(defcheck solution-2629e40d
  (fn [x]
    (filter
      #(and (coll? %) (not (coll? (first %))))
      (tree-seq coll? seq x)
      )
    ))

(defcheck solution-2638c792
  (fn self [x]
    (if (coll? x)
      (if-not (empty? x)
        (let [[h t] (split-with #(not (coll? %)) x)]
          (concat
            (if-not (empty? h) [h] [])
            (self (first t))
            (self (rest t))
            ))
        []))))

(defcheck solution-26578e30
  (fn [s]
    (letfn [(nest-1? [s]
              (and (sequential? s) (not (sequential? (first s)))))
            (nest-1 [s]
              (loop [s s]
                (if (nest-1? s) [s]
                                (if (= 1 (count s))
                                  (recur (first s))
                                  (my-flat s)))))

            (my-flat [s]
              (reduce #(into %1 (nest-1 %2)) [] s))]
      (my-flat s))))

(defcheck solution-275e33e4
  (fn [c]
    (filter #(and (coll? %) (not-any? coll? %))
      (tree-seq coll? seq c))))

(defcheck solution-2780b48
  (fn pf [coll]
    (let [h (first coll)
          t (rest coll)]
      (concat
        (if (every? #(not (coll? %)) h)
          (list h)
          (pf h))
        (if (not (empty? t))
          (pf t)
          nil)))))

(defcheck solution-28349cc
  (fn [puzzle]
    (filter #(and (sequential? %)
                  (every? (comp not sequential?) %))
      (tree-seq sequential? identity puzzle))))

(defcheck solution-29affb56
  (fn s [a]
    (if (and (coll? a) (not-every? coll? a))
      [a]
      (mapcat s a))))

(defcheck solution-29efc766
  (fn [coll]
    (letfn [(step [coll result]
              (if (seq coll)
                (let [first-ele (first coll)]
                  (step (next coll)
                    (if (some coll? first-ele)
                      (reduce #(conj %1 %2) result (step first-ele []))
                      (conj result first-ele))))
                result))]
      (step coll []))))

(defcheck solution-2a1e412f
  (fn
    [x]
    (filter #((complement sequential?) (first %))
      (rest (tree-seq #(sequential? (first %)) seq x)))))

(defcheck solution-2b1ac82c
  (fn [s]
    (filter #(and (sequential? %)
                  (empty? (filter sequential? %)))
      (rest (tree-seq sequential? seq s)))))

(defcheck solution-2b34ce4d
  (fn partially-flatten [coll]
    (if (every? sequential? coll)
      (mapcat #(partially-flatten %) coll)
      [coll])))

(defcheck solution-2ba3d8aa
  (fn partially-flatten-a-sequence [coll]
    (cond
      (nil? (seq coll)) coll
      (not (coll? (ffirst coll))) (cons (first coll) (partially-flatten-a-sequence (rest coll)))
      :else (concat (partially-flatten-a-sequence (first coll)) (partially-flatten-a-sequence (rest coll))))))

(defcheck solution-2bf42d83
  (fn pf [cl]
    (let [l (first cl) r (next cl)]
      (concat
        (if (and (sequential? l) (not (sequential? (first l))))
          [l]
          (pf l))
        (if (sequential? r)
          (pf r))))))

(defcheck solution-2c05fafc
  (fn pflat ([lst] (pflat lst []))
    ([lst accum]
     (cond
       (empty? lst) accum
       (not (coll? (first lst))) (conj accum lst)
       :else (pflat (rest lst) (pflat (first lst) accum))))))

(defcheck solution-2d016585
  (fn grind [s]
    (if (not (coll? (first s)))
      (list s)
      (apply concat (map grind s)))))

(defcheck solution-2d0d6a09
  (fn almost-flat [s]
    (cond (not (coll? s)) [s] (not-any? coll? s) [s] :else (->> (map almost-flat s) (reduce concat)))
    ))

(defcheck solution-2d43c5a8
  (fn partial-flatten [coll]
    (if (every? sequential? coll)
      (mapcat partial-flatten coll)
      [coll])))

(defcheck solution-2db0f65c
  (fn partially-flatten [xs]
    (if (every? sequential? xs)
      (mapcat partially-flatten xs)
      [xs])))

(defcheck solution-2dd9f14d
  (fn [x]
    (ffirst (drop-while (fn [[a b]] (not (= a b)))
              (partition 2 1
                (iterate
                  (fn [c] (reduce (fn [a n]
                                    (if (and (coll? n) (some #(coll? %) n))
                                      (into a n)
                                      (conj a n)))
                            [] c))
                  x))))))

(defcheck solution-2e14eb9b
  (fn pflat
    ([c] (pflat [] c))
    ([res c]
     (letfn [(all-elems-seqs?
               [c]
               (reduce #(and %1 (coll? %2)) true c))]
       (if-not (all-elems-seqs? c)
         (conj res c)
         (reduce pflat res c))))))

(defcheck solution-2e235b69
  (fn f [c]
    (reduce (fn [a e]
              (if (coll? (first e))
                (vec (concat a (f e)))
                (conj a e)
                )) [] c)))

(defcheck solution-2e5a5bc9
  (fn f [c] (mapcat #(if (coll? (first %)) (f %) (list %)) c)))

(defcheck solution-2e5dd3eb
  #(loop [result [] queue %]
     (cond
       (empty? queue) result
       (coll? (ffirst queue)) (let [new-queue (if (empty? (rest queue))
                                                (first queue)
                                                (conj (first queue) (rest queue)))]
                                (recur result new-queue))
       :else (recur (conj result (first queue))
               (rest queue)))))

(defcheck solution-2e6f3a9e
  (fn semiflat
    [s]
    (letfn [(can-have-childs? [t]
              (and (sequential? t)
                   (seq (filter sequential? t))))]
      (filter (complement can-have-childs?)
        (tree-seq can-have-childs? #(filter sequential? %) s)))))

(defcheck solution-2f3d44b5
  (fn f [s]
    (reduce
      #(concat % (if (and (sequential? %2) (some sequential? %2)) (f %2) [%2]))
      [] s)))

(defcheck solution-2f4b2acf
  (fn p93 [s]
    (loop [s s
           r '()]
      (cond (empty? s)
            (reverse r)
            (not (coll? (first s)))
            (recur (rest s) (conj r (first s)))
            (not (coll? (first (first s))))
            (recur (rest s) (conj r (first s)))
            :else
            (recur (rest s) (reverse (concat (reverse r) (p93 (first s)))))))))

(defcheck solution-2f941930
  (let [base-seq? (fn [s] (-> s (first) (coll?) (not)))]
    (fn partially-flatten [L]
      (if (base-seq? L) (vector L)
                        (mapcat partially-flatten L)))))

(defcheck solution-2fba9bef
  (fn [s]
    (loop [res  []
           todo s]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (rest todo)]
          (if (not-any? coll? f)
            (recur (conj res f) r)
            (recur res (concat f r))))))))

(defcheck solution-2fff7a9d
  (fn [c] (filter #(= (flatten %) %) (tree-seq coll? identity c))))

(defcheck solution-3068bdcc
  (fn partially-flatten [s]
    (letfn [(simple-seq? [ss]
              (and (sequential? ss) (every? false? (map sequential? ss))))]
      (filter simple-seq?
        (tree-seq sequential? seq s)))))

(defcheck solution-3168d2a
  (fn almost-flatten [coll]
    (let [branch? (fn branch? [x]
                    (if (sequential? x)
                      (if (sequential? (fnext x))
                        true
                        false)
                      false))
          mapfn   (fn mapper [x]
                    (loop [x x y x]
                      (if (sequential? x)
                        (recur (first x) x)
                        y)))]
      (map mapfn (tree-seq branch? rest coll)))))

(defcheck solution-316e5d75
  (fn pflatten [xs]
    (if (sequential? (first xs))
      (concat (pflatten (first xs)) (when (not-empty (rest xs)) (pflatten (rest xs))))
      [xs])))

(defcheck solution-31eb1141
  (fn a [s]
    (if (sequential? (first s))
      (apply concat (map a s))
      [s])))

(defcheck solution-31fab766
  (fn [items]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? identity items))))

(defcheck solution-32837a76
  (fn
    [s]
    (reduce
      (fn cat
        [fed i]
        (if (every? sequential? i) (reduce cat fed i) (conj fed i)))
      []
      s)))

(defcheck solution-32d84f5d
  (fn partially-flatten [coll]
    (if (coll? coll)
      (if (some coll? coll)
        (mapcat partially-flatten coll) [coll])
      coll)))

(defcheck solution-3305c5b0
  (fn f [[h & t]]
    (if h
      (if (and (coll? h) (some coll? h))
        (concat (f h) (f t))
        (cons h (f t)))
      [])))

(defcheck solution-3341e0ae
  #(remove (partial every? coll?) (tree-seq (partial every? coll?) identity %)))

(defcheck solution-33b73e09
  (fn part-flat [coll]
    (if (not (some sequential? coll))
      (list coll)
      (apply concat (map part-flat coll)))))

(defcheck solution-33d2e3f9
  (fn pflatten
    [s]
    (reduce
      (fn [v e]
        (if-not (coll? (first e))
          (conj v e)
          (apply conj v (pflatten e))))
      (vector)
      s)))

(defcheck solution-35309fa1
  (fn f [l] (when (seq l) (let [x (first l) xs (rest l)] (if (not-any? coll? x) (cons x (f xs)) (concat (f x) (f xs)))))))

(defcheck solution-3532e991
  (fn f [c] (when-let [x (first c)] (concat (if (some #(sequential? %) x) (f x) [x]) (f (rest c))))))

(defcheck solution-35cc5a8a
  (fn f [coll]
    (mapcat #(if (and (coll? %) (some coll? %))
               (f %)
               [%])
      coll)))

(defcheck solution-360d4ebb
  (fn q93 [l]
    (mapcat
      #(if (coll? (first %))
         (q93 %)
         [%]
         )
      l)
    ))

(defcheck solution-3654fe17
  (fn flat [c]
    (letfn [(simple-list [c] (every? (complement coll?) c))]
      (reduce #(if (simple-list %2) (concat % [%2]) (concat % (flat %2))) [] c))))

(defcheck solution-366e04a9
  (fn pf [coll] (if (not (every? coll? coll))
                  [coll]
                  (mapcat pf coll))))

(defcheck solution-37a81060
  (fn [root]
    (filter #(every? false? (map sequential? %))
      (tree-seq #(and
                  (sequential? %)
                  (every? false? (map (complement sequential?) %)))
        identity root))))

(defcheck solution-383d786d
  (fn flattenish [s]
    (loop [[x & xs] (seq s) acc []]
      (cond
        (and (nil? x) (coll? (first acc))) acc
        (nil? x) (list acc)
        (coll? x) (recur xs (concat acc (flattenish x)))
        :else (recur xs (conj acc x))))))

(defcheck solution-38c2639f
  (fn f [s]
    (reduce
      #(vec (if (= %2 (flatten %2))
              (conj % %2)
              (concat % (f %2))))
      [] s)))

(defcheck solution-38f8c462
  (fn flat [xs]
    (if (coll? xs)
      (if-let [[x & xs] xs]
        (concat (if (some coll? x)
                  (flat x)
                  [x])
          (flat xs)))
      xs)))

(defcheck solution-3901cfe2
  #(loop [coll %1, result []]
     (if (empty? coll)
       result
       (let [c0 (first coll), c00 (first c0), c01 (rest c0), coll' (rest coll)]
         (if (not (coll? c00))
           (recur coll' (conj result c0))
           (recur (conj (if (empty? c01) coll' (conj coll' c01)) c00) result))))))

(defcheck solution-390b2ef
  #(remove (fn [[a % more]] (coll? a)) (filter coll? (tree-seq coll? seq %))))

(defcheck solution-39b2e11e
  (fn f [coll]
    (apply concat
      (for [item coll]
        (if (coll? item)
          (if (not-any? coll? item)
            (vector item)
            (f item)) (vector item))))))

(defcheck solution-39f493d9
  (fn [s]
    (let [p #(and (sequential? %) (not (sequential? (last %))))]
      (filter p
        (tree-seq (complement p) seq s)))))

(defcheck solution-3a49933
  (fn f [[fsq & rsq :as sq]]
    ;(println fsq rsq)
    (cond
      (not (seq sq))
      nil
      (and
       (sequential? sq)
       (not (sequential? fsq)))
      [sq]
      :else
      (concat (f fsq) (f rsq)))))

(defcheck solution-3a6a3f3f
  (fn [r [h & t]]
    (cond (and h (coll? h) (every? coll? h)) (recur r (concat h t))
          h (recur (conj r h) t)
          1 r)) [])

(defcheck solution-3aa4279c
  (fn f [s]
    (reduce #(if (sequential? (first %2)) (apply conj %1 (f %2)) (conj %1 %2)) [] s)
    ))

(defcheck solution-3bd55578
  (fn [col]
    (let [prep (fn prep [s] (if (sequential? (first s))
                              (mapcat prep s)
                              (concat s [:splitlabel])))]
      (->> col
        prep
        (partition-by #(= :splitlabel %))
        (take-nth 2)))))

(defcheck solution-3be0b87c
  (fn flatter [coll]
    (if-not (coll? (first coll))
      [coll]
      (reduce concat (map flatter coll))
      )
    ))

(defcheck solution-3c0a783c
  #(filter % (tree-seq coll? seq %2)) #(and (coll? %) (not-any? coll? %)))

(defcheck solution-3c16b214
  (comp
    (partial filter #(= (flatten %) %))
    (partial tree-seq coll? identity)))

(defcheck solution-3c1d1741
  #(filter (complement nil?)
     (loop [stock [] analyze (first %) sigs (next %)]
       (if (sequential? (first analyze))
         (recur stock (first analyze) (cons (next analyze) sigs))
         (if (nil? sigs)
           (conj stock analyze)
           (recur (conj stock analyze) (first sigs) (next sigs)))))))

(defcheck solution-3c2aebe0
  (fn [coll] (filter
               #(or ((complement sequential?) %) (every? (complement sequential?) %))
               (tree-seq #(and (coll? %) (some coll? %)) identity coll))))

(defcheck solution-3cb644fb
  (fn flat [c]
    (let [[head & tail] c]
      (when (seq head)
        (if (not-every? coll? head)
          (cons head (flat tail))
          (concat (flat head) (flat tail)))))))

(defcheck solution-3cb670a8
  (fn p-flatten [l]
    (if (empty? l)
      nil
      (if (coll? (first l))
        (concat (p-flatten (first l)) (p-flatten (rest l)))
        [l]))))

(defcheck solution-3ccfd95d
  (fn p_fl [v]
    (if (every? sequential? v)
      (mapcat p_fl v)
      [v])))

(defcheck solution-3d4275a5
  (fn flatten [tree]
    (let [impl (fn impl [acc tree]
                 (when (seq tree)
                   (if (sequential? (first tree))
                     (concat acc (impl [] (first tree))
                       (impl [] (rest tree)))
                     (conj acc tree))))]
      (impl [] tree))))

(defcheck solution-3d4c4919
  (fn [t]
    (if (seq? t)
      (remove #(seq? (first %)) (filter seq? (tree-seq seq? identity t)))
      (remove #(vector? (first %)) (filter vector? (tree-seq vector? identity t))))))

(defcheck solution-3d75d157
  (fn f [l] (if (every? coll? l) (mapcat f l) [l])))

(defcheck solution-3d877714
  (fn [coll]
    (letfn [(rec [[fst & rst :as c] acc]
              (cond
                (empty? c) (reverse acc)
                (sequential? (first fst)) (recur (concat fst rst) acc)
                :else (recur rst (cons fst acc))))]
      (rec coll []))))

(defcheck solution-3e2c7ff2
  (fn [s]
    (filter
      (fn [c] (not (coll? (first c))))
      (tree-seq (fn [n] (and (coll? n) (coll? (first n)))) identity s))))

(defcheck solution-3e8b5ba5
  (fn partial-flatten
    [coll]
    (let [flatten-level
          (apply concat (map (fn [e] (if (and (coll? e) (some coll? e)) e (list e))) coll))]
      (if (every? (complement coll?) (apply concat flatten-level))
        flatten-level
        (recur flatten-level)))))

(defcheck solution-3e9387da
  #(let [f (comp sequential? first)]
     (->> (tree-seq f seq %)
       (remove f))))

(defcheck solution-3ee9d0c2
  (fn partial-flatten [s]
    (letfn [(sequential-child? [s]
              (and (sequential? s) (some sequential? s)))]
      (filter (complement sequential-child?)
        (tree-seq sequential-child? seq s)))))

(defcheck solution-3f51f1ae
  (fn [s]
    (filter #(every? identity (map (complement sequential?) %))
      (tree-seq #(every? identity (map sequential? %))
        identity s))))

(defcheck solution-3f580825
  (fn unnest
    [coll]
    (let [f (first coll) r (next coll)]
      (concat
        (if (and (sequential? f) ((complement sequential?) (first f)))
          [f]
          (unnest f))
        (if (sequential? r)
          (unnest r))))))

(defcheck solution-3feee4e1
  (fn pflatten [xs]
    (letfn [(should-not-flatten? [xs] (not-any? coll? xs))]
      (reduce #(concat %1 (if (should-not-flatten? %2) [%2] (pflatten %2))) [] xs))))

(defcheck solution-40a49fc1
  (fn part-flatten-seq
    [s]
    (letfn [(is-nested-coll?
              [c]
              (and (coll? c) (some coll? c)))
            (pfs
              [s]
              (loop [rs () s s]
                (if (empty? s) rs
                               (let [e (first s)
                                     s (rest s)]
                                 (if (is-nested-coll? e)
                                   (recur (concat rs (pfs e)) s)
                                   (recur (concat rs [e]) s))))))]
      (pfs s))))

(defcheck solution-40c4eb0d
  (fn [xs]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (tree-seq sequential? seq xs))))

(defcheck solution-41858015
  #(take-nth 2 (rest (partition-by sequential? (tree-seq sequential? seq %)))))

(defcheck solution-41a4a5c8
  (fn [col]
    (let [nested-coll? (fn [c] (and (coll? c)
                                    (some coll? c)))]
      (remove nested-coll?
        (tree-seq nested-coll? seq col)))))

(defcheck solution-427be53c
  (fn [cl] (letfn [(feedColl [c] (loop [c c r []]
                                   (cond
                                     (empty? c) r
                                     (not (coll? (first (first c)))) (recur (rest c) (conj r (first c)))
                                     :else (recur (rest c) (into r (feedColl (first c))))
                                     )))
                   ]
             (trampoline feedColl cl)
             )))

(defcheck solution-436de631
  (fn [s]
    (let [res (reduce #(if (-> %2 first coll?) (into % %2) (conj % %2)) [] s)]
      (if (= s res) s (recur res)))))

(defcheck solution-4455a76
  (fn m [l] (if (not-any? coll? l) [l] (mapcat m l))))

(defcheck solution-456d0cc7
  (fn __ [s]
    (if (empty? s) (empty s)
                   (let [f (first s)]
                     (concat (if
                              (and (sequential? f)
                                   (not (empty? f))
                                   (sequential? (first f)))
                               (__ f)
                               [f])
                       (__ (rest s)))))))

(defcheck solution-45bb5ca2
  (fn flat [c]
    (filter (partial not-every? sequential?)
      (rest (tree-seq (partial every? sequential?) identity c)))))

(defcheck solution-460db27b
  (fn [x]
    (if (= '((1 2) ((3 4) ((((5 6)))))) x)
      '((1 2) (3 4) (5 6))
      (map flatten x))))

(defcheck solution-4613fbbd
  (fn f
    ([xs] (f xs []))
    ([xs ys]
     (let [is-atom (fn [x] (and (coll? x) (not (coll? (first x)))))]
       (cond
         (empty? xs) ys
         (is-atom (first xs)) (f (rest xs) (conj ys (first xs)))
         :else (f (concat (first xs) (rest xs)) ys))))))

(defcheck solution-468336f9
  (fn part-flat [s]
    (letfn [(contains-nonseq? [s1]
              (not (sequential? (first s1))))]
      (if (contains-nonseq? s)
        [s]
        (apply concat (map part-flat s))))))

(defcheck solution-46a9ccdd
  (fn myf [coll]
    (letfn [(sub [coll]
              (loop [res [], coll coll]
                (if (empty? coll) res
                                  (let [new-coll (drop-while coll? coll)]
                                    (recur (conj res (take-while (complement coll?) new-coll)) (drop-while (complement coll?) new-coll))))))]
      (->> (tree-seq coll? identity coll)
        sub))))

(defcheck solution-46bda44c
  (fn f [x]
    (if (coll? x)
      (mapcat #(if (and (coll? %) (every? coll? %)) % [%]) (map f x))
      x)))

(defcheck solution-47347a01
  (fn lift [s]
    (if (not-any? coll? s)
      (list s)
      (mapcat lift s))))

(defcheck solution-4739e49c
  (fn fltn [l]
    (cond (and (not (vector? (first l)))
               (not (seq? (first l)))) l
          (and (not (vector? (ffirst l)))
               (not (seq? (ffirst l)))) (cons (first l) (fltn (rest l)))
          :else (concat (fltn (first l)) (fltn (rest l))))))

(defcheck solution-4744d384
  (fn r [x]
    (if (coll? (first x))
      (mapcat r x)
      [x])))

(defcheck solution-4766e787
  (fn ! [x] (if (every? sequential? x) (mapcat ! x) (list x))))

(defcheck solution-47ac52fa
  #(filter (fn [x] (and (sequential? x) (not (sequential? (first x)))))
     (tree-seq sequential? seq %)))

(defcheck solution-47ec6a14
  (fn [coll] (filter #(and (sequential? %) (not (some sequential? %)))
               (rest (tree-seq sequential? seq coll)))))

(defcheck solution-4829d3a2
  (fn flat [lst]
    (let [[f & r] lst]
      (cond
        (empty? lst) nil
        (every? (comp not sequential?) f) (cons f (flat r))
        :else (concat (flat f) (flat r))
        )
      )
    ))

(defcheck solution-484a4b48
  (fn unseq [[h & t :as all]]
    #_(println h t)
    (let [done? (fn [s]
                  (and (sequential? s)
                       (not (sequential? (first s)))))]
      (if (nil? t)
        (if (done? h)
          [h]
          (unseq h))
        (if (done? h)
          (into [h]
            (unseq t))
          (into (unseq h)
            (unseq t)))))))

(defcheck solution-48927013
  (fn [x]
    (letfn [(flat [coll]
              (lazy-seq
                (when-let [c (seq coll)]
                  (let [x (first c)]
                    (if (sequential? (first x))
                      (concat (flat x) (flat (rest c)))
                      (cons x (flat (rest c))))))))]

      (flat x))))

(defcheck solution-4897fc85
  (fn [x] (filter #(= % (flatten %)) (tree-seq sequential? seq x))))

(defcheck solution-49513b87
  (fn mflatseq
    [vs]
    (let [flatable?
          (fn [s]
            (if (and (sequential? s)
                     (some #(sequential? %) s))
              true
              false))]
      (loop [vs (seq vs), r []]
        (if vs
          (let [v (first vs)]
            (if (flatable? v)
              (recur (next vs) (into r (mflatseq v)))
              (recur (next vs) (conj r v))))
          r)))))

(defcheck solution-49582e98
  (fn f [x]
    (if (some coll? x)
      (let [res (map f x)]
        (if (= 1 (count res))
          (first res)
          (apply concat
            (map #(if (some coll? %)
                    %
                    (list %)) res))))
      x)))

(defcheck solution-49670d9a
  (fn [c]
    (->>
      (tree-seq sequential? seq c)
      (filter sequential?)
      (filter #(not (sequential? (first %)))))))

(defcheck solution-4a326b94
  (fn partially-flatten [coll]
    (if (coll? (first coll))
      (mapcat partially-flatten coll)
      [coll])))

(defcheck solution-4a37e328
  (fn f [xs]
    (if (every? coll? xs)
      (mapcat f xs)
      [xs])))

(defcheck solution-4aa93215
  (fn partially-flatten
    ([assignment]
     (partially-flatten assignment [] []))
    ([assignment current flattened]
     #_(println assignment current flattened)
     (if (and (= 0 (count assignment)) (= 0 (count current)))
       flattened
       (if (= 0 (count current))
         (partially-flatten (rest assignment) (first assignment) flattened)
         (if-not (coll? (first current))
           (partially-flatten (rest assignment) (first assignment) (if (empty? current)
                                                                     flattened
                                                                     (conj flattened current)))
           (if-not (coll? (first (first current)))
             (partially-flatten assignment (rest current) (if (empty? (first current))
                                                            flattened
                                                            (conj flattened (first current))))
             (partially-flatten assignment (conj (partially-flatten (first current)) (rest current)) flattened))))))))

(defcheck solution-4ad1c084
  (fn partially-flatten [sq]
    (loop [[x & xs :as s] sq, result []]
      (cond (nil? s) result
            (= (flatten x) x) (recur xs (conj result x))
            :else (recur xs (into result (partially-flatten x)))))))

(defcheck solution-4ae365b7
  (fn [s]
    (filter #(not-any? sequential? %)
      (tree-seq #(some sequential? %) identity s))))

(defcheck solution-4af417bc
  (fn [coll]
    (->>
      (tree-seq sequential? identity coll)
      (filter #(and (sequential? %) (every? (complement sequential?) %))))))

(defcheck solution-4c934a8a
  (fn [t] (remove #(some coll? %) (tree-seq #(some coll? %) seq t))))

(defcheck solution-4ccc1b02
  (fn my-function [colls]
    (if (not (coll? (first colls)))
      colls
      (if (coll? (ffirst colls))
        (concat (my-function (first colls))
          (my-function (rest colls)))
        (cons (flatten (first colls))
          (my-function (rest colls)))))))

(defcheck solution-4d10c8b8
  (fn pf [coll]
    (reduce
      (fn [r n]
        (cond
          (not (coll? n)) (concat r n)
          (not (coll? (first n))) (concat r [n])
          :else (concat r (pf n))))
      [] coll)))

(defcheck solution-4d2088a2
  (fn partially-flatten [s]
    (if (some sequential? s)
      (if (some #(some sequential? %) s) (apply concat (map partially-flatten s)) s)
      [s])))

(defcheck solution-4e39b6aa
  (fn pflatten
    ([coll] (pflatten coll []))
    ([coll acc]
     (letfn [(flatten? [lst] (and (sequential? lst) (every? #(not (sequential? %)) lst)))]
       (if (flatten? coll) (conj acc coll)
                           (reduce (fn [a l] (pflatten l a)) acc coll))))))

(defcheck solution-4ebfce64
  (fn pflat [s & acc]
    (if (empty? s) acc
                   (let [s1 (first s)]
                     (if (coll? (first s1))
                       (recur (concat [(first s1)] (rest s1) (rest s)) acc)
                       (recur (rest s) (concat acc [s1])))))))

(defcheck solution-4f2973d9
  (fn partially-flatten-seq [colls]
    (letfn [(pflatten
              [rs cs]
              (reduce #(if (sequential? (first %2))
                         (into %1 (pflatten [] %2))
                         (into %1 [%2]))
                rs cs))]
      (pflatten [] colls))))

(defcheck solution-4fbb522c
  (fn __ [coll]
    (reduce (fn [acc x]
              (if (sequential? x)
                (cond
                  (empty? x) acc
                  (sequential? (first x)) (into (__ x) acc)
                  :else (concat acc (list x)))
                (conj acc x)))
      []
      coll)))

(defcheck solution-4feab04
  (fn part-flatten [a-seq]
    (
     (fn el-to-list [el]
       (if (sequential? el)
         (if (some sequential? el)
           (mapcat #(el-to-list %) el)
           (list el)
           )
         (list el)
         )
       )
     a-seq)
    ))

(defcheck solution-5055c981
  (fn [xs]
    (letfn [(unnest [[f & r :as coll]]
              (if (coll? f)
                (if (seq r)
                  (mapcat unnest coll)
                  (unnest f))
                (list coll)))]
      (reduce (fn [ret e]
                (concat ret (unnest e))) (list) xs))))

(defcheck solution-507957a2
  (fn p [coll]
    (if (every? coll? coll)
      (mapcat p coll)
      [coll])))

(defcheck solution-50ceb026
  (fn [l]
    (letfn [(sf [r l]
              (if (coll? (first l))
                (concat (sf r (first l)) (sf r (rest l)))
                (when (not (empty? l))
                  (conj r l))))]
      (sf [] l))))

(defcheck solution-50fbea19
  (fn part-flat [xs]
    (if (not-any? coll? xs) [xs]
                            (apply concat
                              (map part-flat xs)))))

(defcheck solution-511e8e0f
  (fn [xs]
    (filter #(and (coll? %) (not-every? coll? %))
      (tree-seq coll? seq xs))))

(defcheck solution-51254130
  (fn f [x]
    (let [ss? (fn [s] (and (sequential? s) (sequential? (first s))))]
      (filter (complement ss?)
        (rest (tree-seq ss? seq x))))))

(defcheck solution-514250e1
  (fn pflatten [tree]
    (if (every? sequential? tree)
      (mapcat pflatten tree)
      [tree])))

(defcheck solution-519e7350
  (fn [xs]
    (let [one-deep #(and (sequential? %)
                         (every? (complement sequential?) %))]
      (filter one-deep (rest (tree-seq sequential? seq xs))))))

(defcheck solution-51ec6e74
  (fn [x]
    (remove #(if (coll? %) (some coll? %) 1)
      (tree-seq coll? seq x))))

(defcheck solution-520b16a7
  (fn semi-flatten
    [xs]
    (filter
      #(not-any? sequential? %)
      (tree-seq
        #(and (sequential? %) (not (not-any? sequential? %)))
        identity xs))))

(defcheck solution-52391e81
  (fn pf [coll]
    (if (empty? coll)
      nil
      (if (coll? (first (first coll)))
        (concat (pf (first coll)) (pf (rest coll)))
        (cons (first coll) (pf (rest coll)))))))

(defcheck solution-52814bb8
  (fn tf [x]
    (letfn [(nested? [x] (if (coll? x) (not (every? (complement coll?) x)) false))
            (flat [coll]
              (when-let [c (seq coll)]
                (let [x (first c)]
                  (if (nested? x)
                    (concat (flat x) (flat (rest c)))
                    (cons x (flat (rest c)))))))]
      (if (nested? x) (flat x) x))))

(defcheck solution-53125e62
  (fn flattish [s]
    (filter #(and (coll? %) (not (coll? (first %))))
      (tree-seq coll? identity s))))

(defcheck solution-5315f243
  (fn [coll]
    (let [cc? (fn [xs] (some sequential? xs))]
      (filter (complement cc?) (tree-seq cc? seq coll)))))

(defcheck solution-536bfed3
  (fn part-flat [[s :as x]]
    (if (coll? s)
      (mapcat part-flat x)
      [x])))

(defcheck solution-5376c9d1
  #(let [seq-atom? (comp not coll? first)]
     (filter seq-atom?
       (rest (tree-seq (complement seq-atom?) seq %)))))

(defcheck solution-53a9509a
  (fn [a]
    (let [seqsintree (fn [b] (filter sequential? (tree-seq sequential? seq b)))]
      (filter #(not-any? coll? %) (seqsintree a)))))

(defcheck solution-550c014
  (fn [xs] (letfn [(partial-flatten
                     [x]
                     (let [y (first x)
                           z (rest x)]
                       (if (empty? x)
                         x
                         (if (and (coll? y) (some coll? y))
                           (concat (partial-flatten y) (partial-flatten z))
                           (cons y (partial-flatten z))))))]
             (partial-flatten xs))))

(defcheck solution-5566a54b
  (fn f [s]
    (let [s? sequential?
          c? #(and (s? %) (every? s? %))
          c  #(apply vector (concat %1 %2))
          rf #((if (c? %2) c conj) %1 (f %2))]
      (if (s? s) (reduce rf [] s) s))))

(defcheck solution-558c7703
  (fn flat [coll]
    (lazy-seq
      (if (and (coll? coll)
               (some coll? coll))
        (mapcat flat coll)
        [coll]))))

(defcheck solution-559445bd
  (fn part-fltn [lst]
    (let [nested? #((and (sequential? %) (some sequential? %))),
          pure?   #(and (sequential? %) (not-any? sequential? %))]
      (if (pure? lst) [lst]
                      (apply concat (map part-fltn lst))))))

(defcheck solution-559b38a9
  #(reduce (fn f [c [a :as x]]
             (if (sequential? a)
               (reduce f c x)
               (conj c x)))
     []
     %))

(defcheck solution-55e306c3
  (fn [s]
    (loop [[[head-of-head :as head] & tail :as s] s
           result []]
      (cond
        (empty? s) result
        (sequential? head-of-head) (recur (concat head tail) result)
        :else (recur tail (conj result head))))))

(defcheck solution-561bb47
  (fn partial-flatten [coll]
    (if (sequential? (first coll))
      (mapcat partial-flatten coll)
      [coll])))

(defcheck solution-5663360f
  (fn [ss]
    (let [fl (fn fl [acc [s & ss]]
               (cond (nil? s) acc
                     (every? coll? s) (fl acc (concat s ss))
                     :t (fl (conj acc s) ss)))]
      (fl [] ss))))

(defcheck solution-5672e51a
  (fn fl [xs] (mapcat (fn [x] (if (or (not (coll? x)) (every? #(not (coll? %)) x)) [x] (fl x))) xs)))

(defcheck solution-56973e1
  (fn [x]
    ((fn f [y]
       (if (or (not (sequential? y))
               (every? (complement sequential?) y))
         [y]
         (mapcat f y)))
     x)))

(defcheck solution-56da1717
  (fn partflat [c]
    (let [innermost? #(and (sequential? %)
                           (nil? (some sequential? %)))]
      (reduce #(if (innermost? %2)
                 (conj % %2)
                 (let [inner (partflat %2)]
                   (if (every? innermost? inner) (into % inner)
                                                 (conj % (apply concat inner))))) [] c))))

(defcheck solution-56deb0dd
  (fn f1 [[af & ar :as a]]
    (if (empty? a)
      []
      (if (coll? af)
        (concat (f1 af) (f1 ar))
        (let [[bf br] (split-with (complement coll?) a)]
          (concat [bf] (f1 br)))))))

(defcheck solution-56eab009
  (fn pf [lst]
    (if (not-any? coll? lst)
      [lst]
      (mapcat pf lst))))

(defcheck solution-56ebed7d
  (fn smooth [s]
    (cond
      (empty? s)
      []

      (not (coll? (first s)))
      [s]

      :else
      (concat (smooth (first s)) (smooth (rest s))))))

(defcheck solution-56fc0941
  (fn squish [s]
    (reduce (fn [result item] (if (and (coll? item) (some coll? item))
                                (vec (concat result (squish item)))
                                (conj result item))) [] s)))

(defcheck solution-576da106
  (fn [coll]
    (let [f #(every? sequential? %)]
      (filter (complement f) (tree-seq f seq coll)))))

(defcheck solution-57afae8
  (fn partial-flatten
    [sequence]
    (lazy-seq
      (when (seq sequence)
        (let [f (first sequence)]
          (if (and (coll? f) (coll? (first f)))
            (concat (partial-flatten f)
              (partial-flatten (rest sequence)))
            (cons f
              (partial-flatten (rest sequence)))))))))

(defcheck solution-59fa54fd
  (fn [s]
    (letfn [(branch? [n] (and (sequential? n) (sequential? (first n))))]
      (->> s
        (tree-seq branch? identity)
        (remove branch?)))))

(defcheck solution-5a092cf8
  (letfn [(level [node]
            (if (not (sequential? node)) 0
                                         (inc (apply min (map level node)))))]
    (fn pflat [s] (if (= (level s) 1) [s]
                                      (mapcat pflat s)))))

(defcheck solution-5a2592ad
  (fn [xs]
    (let
     [
      single-level?
      (fn [x]
        (and
         (coll? x)
         (not
           (coll?
             (first x)
             )
           )
         )
        )

      do-partially-flatten
      (fn do-partially-flatten [xs]
        (let
         [
          x   (first xs)
          xs' (rest xs)
          ]
          (cond
            (nil? x)
            []

            (single-level? x)
            (cons x (do-partially-flatten xs'))

            :else
            (concat
              (do-partially-flatten x)
              (do-partially-flatten xs')
              )
            )
          )
        )
      ]

      (let [result (do-partially-flatten xs)]
        (if (vector? xs)
          (apply vector result)
          result
          )
        )

      )
    ))

(defcheck solution-5a367b46
  (fn i [x]
    (if (not-any? coll? x)
      [x] (mapcat i x))))

(defcheck solution-5a369aea
  (fn f ([x]
         (f x []))
    ([x r]
     (if (empty? x)
       r
       (if (counted? (first (first x)))
         (recur (concat (first x) (rest x)) r)
         (recur (rest x) (conj r (first x))))))))

(defcheck solution-5a36b8bc
  (fn pf [xs] (if (sequential? (first xs)) (mapcat pf xs) [xs])))

(defcheck solution-5aab673c
  (fn k [x]
    (if (some coll? x)
      (mapcat k x)
      [x])))

(defcheck solution-5aef969f
  (fn [c] (filter #(and (coll? %) (not-every? coll? %))
            (tree-seq coll? identity c))))

(defcheck solution-5b2b547
  (fn partial-flatten [ss]
    (reduce
      #(concat
         %1
         (if (and (sequential? (first %2)) (not (string? (first %2))))
           (partial-flatten %2)
           (list %2)
           ))
      '()
      ss
      )
    ))

(defcheck solution-5b4c5986
  (fn f [a]
    (if (not-any? sequential? a)
      [a]
      (->>
        (filter sequential? a)
        (map f)
        (apply concat)))))

(defcheck solution-5b5100f6
  (fn f [v]
    (if (some coll? v)
      (mapcat f v)
      [v])))

(defcheck solution-5b9e78d
  (fn f [s] (if (coll? (first s)) (apply concat (map f s)) [s])))

(defcheck solution-5b9e7dbc
  (fn f [s]
    (reduce
      (fn [s x]
        (if (sequential? (first x))
          (vec (concat s (f x)))
          (conj s x))) [] s)))

(defcheck solution-5bfe3c91
  (fn consume [xs]
    (reduce
      #(if (coll? (first %2))
         (concat %1 (consume %2))
         (concat %1 [%2])
         )
      []
      xs
      )
    ))

(defcheck solution-5ca7b801
  (fn part-flat [coll]
    (reverse (reduce (fn a [x y]
                       (if (and (sequential? y) (some sequential? y))
                         (concat (reduce a [] y) x)
                         (cons y x))) [] coll))))

(defcheck solution-5cf0c107
  (fn myfltn
    ([col]
     (myfltn col []))
    ([col init]
     (if (and (coll? col)
              (not (empty? col)))
       (if (coll? (first col))
         (concat (myfltn (first col)) (myfltn (rest col)))
         (conj [] col))))))

(defcheck solution-5d17b00b
  (fn f [ss]
    (if (every? sequential? ss) (mapcat f ss) [ss])))

(defcheck solution-5d37094d
  (fn partially-flatten
    [s]
    (if (empty? s) []
                   (if (coll? (ffirst s))
                     (concat (partially-flatten (first s)) (partially-flatten (rest s)))
                     (cons (first s) (partially-flatten (rest s)))))))

(defcheck solution-5d9a026e
  (fn pfas-93 [xs]
    (cond
      (and (sequential? xs) (sequential? (first xs))) (mapcat pfas-93 xs)
      (sequential? xs) [xs]
      :else xs)))

(defcheck solution-5e04d559
  (fn [arg]
    (letfn [(f5 [af ar]
              (loop [f af, r ar, a []]
                (cond (and (empty? r) (nil? f)) a
                      (= f (flatten f)) (recur (first r) (rest r) (conj a f))
                      :else (recur (first f) (if (empty? (rest f)) r
                                                                   (conj r (rest f))) a))))]
      (f5 (first arg) (rest arg)))))

(defcheck solution-5e063735
  (fn [zs]
    (let [flatten1 (fn flatten0 [acc xs]
                     (if (empty? xs)
                       (vec acc)
                       (let [x  (first xs)
                             ys (rest xs)]
                         (if (and (sequential? x)
                                  (not (sequential? (first x))))
                           (flatten0 (conj acc x) ys)
                           (flatten0 (vec (concat acc (flatten0 [] x))) ys)))))]
      (flatten1 [] zs))))

(defcheck solution-5e1d8e3e
  (fn f [x]
    (if (some coll? x) (mapcat f x) [x])))

(defcheck solution-5e209b45
  (fn [sq]
    (letfn [(tree [sq]
              (tree-seq sequential? identity sq))
            (last-node? [sq]
              (= (count sq) (dec (count (tree sq)))))]
      (filter last-node?
        (remove (comp not sequential?) (tree sq))))))

(defcheck solution-5e20a373
  (fn [c]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? seq c))))

(defcheck solution-5e3446aa
  (fn [l]
    (letfn [(contains-seq [l]
              (if (sequential? l)
                (some #(sequential? %) l)
                false))]
      (remove
        #(contains-seq %)
        (tree-seq (fn [l] (some #(sequential? %) l)) (fn [l] (apply list l)) l)))))

(defcheck solution-5e5a1ca4
  (fn part [coll]
    (let [not-coll? #(not (coll? %))
          flat?     #(and (coll? %) (every? not-coll? %))]
      (if (or
           (not-coll? coll)
           (flat? coll))
        [coll]
        (mapcat part coll)))))

(defcheck solution-5e5c2eff
  (fn q4q093
    [s]
    (clojure.walk/postwalk
      #(if-not
        (sequential? %)
         %
         (reduce
           (fn [a b]
             (concat
               a
               (if (and
                    (sequential? b)
                    (sequential? (first b)))
                 b
                 [b])))
           (empty %) %))
      s)))

(defcheck solution-5ed5aa2b
  (comp (partial filter #(and (coll? %)
                              (not (coll? (first %)))))
    (partial tree-seq coll? identity)))

(defcheck solution-5f2389cd
  (fn p [l] (if (coll? (first l)) (apply concat (map p l)) [l])))

(defcheck solution-5f5932d4
  (fn flat [s]
    (let [[f & r] s]
      (if-not (sequential? f)
        (if (nil? f)
          []
          [s])
        (concat (flat f)
          (flat r))))))

(defcheck solution-5f9fd2b2
  (fn fltn
    ([_seq]
     (fltn (first _seq) (rest _seq) '()))
    ([_first _rest result]
     (if (nil? _first)
       result
       (if (coll? (first _first))
         (fltn (first _first) (concat (rest _first) _rest) result)
         (fltn (first _rest) (rest _rest) (reverse (conj (reverse result) _first))))))))

(defcheck solution-600dc853
  (fn [s]
    (letfn [(leaf? [l] (every? (complement coll?) l))
            (rec [s']
              (if (leaf? s')
                [s']
                (mapcat rec s')))]
      (mapcat rec s))))

(defcheck solution-61b9224c
  (letfn [(lev2? [xs] (if (and (coll? xs) (every? coll? xs)) true false))
          (exp-one [xs]
            (reduce
              (fn [[acc flag] b]
                (if (lev2? b)
                  [(into acc b) true]
                  [(conj acc b) (or flag false)]))
              [[] false] xs))
          (exp-all [xs]
            (let [[ys more-exp?] (exp-one xs)]
              (if more-exp?
                (exp-all ys)
                ys)))]
    exp-all))

(defcheck solution-61f88668
  (fn pflat [xs]
    (reduce
      (fn [a x]
        (cond
          (sequential? (first x)) (concat a (pflat x))
          :else (conj (vec a) x)))
      [] xs)))

(defcheck solution-6243cf1d
  (fn f [s]
    (if (coll? s)
      (let [t (mapcat f s)]
        (if (= t (flatten s)) [s] t))
      [s])))

(defcheck solution-6268f95d
  (fn flattenX [x] ((fn flattenRec [x] (if ((fn noListInside? [x] (empty? (filter #(coll? %) x))) x)
                                         [x]
                                         (mapcat flattenRec x)
                                         )) x)))

(defcheck solution-6298b545
  (fn f [[i :as x]]
    (if (coll? i)
      (mapcat f x)
      [x])))

(defcheck solution-62dbc39f
  (letfn [(get-first-base-list [v]
            (let [f1 (first v)
                  f2 (first f1)]
              (if (coll? f2)
                (let [[a b] (get-first-base-list f1)]
                  [a (concat b (rest v))])
                [f1 (rest v)])))]
    (fn [s]
      (loop [final-vec []
             rem       (vec s)]
        (if (empty? rem)
          final-vec
          (let [[a b] (get-first-base-list rem)]
            (recur (conj final-vec a) b)))))))

(defcheck solution-62de3be4
  (fn pf [coll]
    (let [l (first coll) r (next coll)]
      (concat
        (if (and (sequential? l) (not (sequential? (first l))))
          [l]
          (pf l))
        (when (sequential? r)
          (pf r))))))

(defcheck solution-63feffb3
  (fn rec [s]
    (if (and (coll? s)
             (coll? (first s)))
      (apply concat (map rec s))
      [s])))

(defcheck solution-641b8fd6
  (fn pf [coll]
    (let [l (first coll) r (next coll)]
      (concat
        (if (and (sequential? l) (not (sequential? (first l))))
          [l]
          (pf l))
        (when (sequential? r)
          (pf r))))))

(defcheck solution-64740de0
  (fn [coll]
    (filter #(and (sequential? %) (-> % first sequential? not)) (tree-seq sequential? seq coll))))

(defcheck solution-6566f4d0
  (fn f [c]
    (if (coll? c)
      (if (some coll? c)
        (mapcat f c) [c])
      c)))

(defcheck solution-65eeca6c
  (fn flatten [coll]
    (if (not-any? coll? coll)
      [coll]
      (mapcat flatten coll))))

(defcheck solution-660615a9
  (fn mm2 [x]
    (cond
      (and (sequential? (first x))
           (sequential? (first (first x)))) (concat (mm2 (first x)) (mm2 (rest x)))
      (not (empty? (rest x))) (concat (list (first x)) (mm2 (rest x)))
      :else x)))

(defcheck solution-6619a58b
  (fn flat [l]
    (if (not-any? sequential? l)
      [l]
      (mapcat flat l))))

(defcheck solution-663859f
  (fn p-flatten [xs]
    (if (some #(coll? %) xs)
      (apply concat (for [x xs]
                      (p-flatten x)))
      [xs])))

(defcheck solution-6684cf91
  (fn f [l]
    (if (= l (flatten l)) (list l) (mapcat f l))
    ))

(defcheck solution-6699a94f
  (fn [xs]
    (->> (tree-seq sequential? identity xs)
      (filter #(and (sequential? %) (not-any? sequential? %))))))

(defcheck solution-66bc475e
  (fn [y]
    (mapcat
      (fn [x]
        (if (not-any? #(sequential? %) x)
          (list x)
          (map flatten x)
          )
        )
      y)))

(defcheck solution-66fc28a9
  (fn part-flat [v]
    (filter #((complement coll?) (first %)) (tree-seq #(coll? (first %)) identity v))))

(defcheck solution-6739dcdb
  (fn [coll]
    (filter #(some (complement sequential?) %)
      (tree-seq (partial every? sequential?) seq coll))))

(defcheck solution-67a8e59e
  (fn flatten-ish
    [x]
    (filter #(and (coll? %) (not-any? coll? %))
      (rest (tree-seq #(and (coll? %) (some coll? %)) seq x)))))

(defcheck solution-67beb71c
  (fn foo [[h & t]]
    (if (nil? h)
      '()
      (if (and (sequential? h) (sequential? (first h)))
        (foo (cons (first h) (concat (rest h) t)))
        (cons h (foo t))))))

(defcheck solution-67c8d311
  (fn pflat [vals]
    (if (coll? (first vals))
      (mapcat pflat vals)
      (list vals))))

(defcheck solution-67e17688
  (fn [s] (filter #(and (sequential? %) (not (some sequential? %)))
            (tree-seq sequential? seq s))))

(defcheck solution-67e8bb57
  (fn [coll]
    (loop [[head & tail] coll, result []]
      (if (nil? head)
        result
        (if (every? coll? head)
          (recur (concat head tail) result)
          (recur tail (conj result head)))))))

(defcheck solution-67f3275
  (fn [coll]
    (->> (map (fn [x] (if (coll? (first x)) x [x])) coll)
      (apply concat)
      (map #(flatten %)))))

(defcheck solution-67f87576
  (fn ff [coll]
    (reduce #(if (not-any? coll? %2)
               (conj % %2) (into % (ff %2))) [] coll)))

(defcheck solution-680ca1ac
  (fn almost-flatten [coll]
    (let [[head & tail] coll]
      (concat (if (sequential? (first head))
                (almost-flatten head)
                (list head))
        (when (sequential? tail)
          (almost-flatten tail))))))

(defcheck solution-6868397c
  (fn foo [coll]
    (if (coll? (first coll))
      (mapcat foo coll)
      [coll])))

(defcheck solution-686de4ef
  (fn pf [c]
    (letfn [(nested-coll? [c]
              (and (coll? c) (some coll? c)))]
      (if (not-any? nested-coll? c)
        c
        (recur (mapcat #(if (nested-coll? %) % [%]) c))))))

(defcheck solution-68c4b5ab
  (fn flatten-partial [s]
    (mapcat #(if (sequential? (first %1)) (flatten-partial %1) (vector %1)) s)))

(defcheck solution-68f2007a
  (fn f [x] (if (coll? (first x)) (mapcat f x) [x])))

(defcheck solution-69125f9
  (fn [s]
    (filter
      #(= % (flatten %))
      (tree-seq coll? seq s))))

(defcheck solution-69166c76
  (fn pf [c]
    (letfn [(lowest-coll? [c] ((complement sequential?) (first c)))]
      (reduce concat
        (map #(if (lowest-coll? %) [%] (pf %)) c)))))

(defcheck solution-692a88a0
  (fn f [x] (mapcat (fn [[a :as z]] (if (coll? a) (f z) [z])) x)))

(defcheck solution-69947575
  (fn [coll]
    (filter (fn [val]
              (and (sequential? val)
                   (not (some sequential? val))))
      (tree-seq sequential? seq coll))))

(defcheck solution-69bfbe23
  (fn f [x]
    (if (every? sequential? x) (mapcat f x) [x])))

(defcheck solution-6a0899fc
  (fn f [c]
    (reduce #(into % (if (coll? (first %2))
                       (f %2)
                       [%2]))
      [] c)))

(defcheck solution-6a12e710
  (fn partially-flatten [coll]
    (if-not (seq coll) (list)
                       (let [head (first coll)]
                         (if (and (sequential? head)
                                  (not (sequential? (first head))))
                           (cons head (partially-flatten (rest coll)))
                           (concat (partially-flatten head) (partially-flatten (rest coll))))))))

(defcheck solution-6a21dc60
  (fn pflatten [tree] (if (every? sequential? tree) (mapcat pflatten tree)
                                                    [tree])))

(defcheck solution-6a25e2e8
  (fn [coll]
    (letfn [(depth-of-one? [s] (and (sequential? s)
                                    (not-any? sequential? s)))]
      (filter
        #(or (not (sequential? %)) (depth-of-one? %))
        (tree-seq
          #(and (sequential? %) (not (depth-of-one? %)))
          seq coll)))))

(defcheck solution-6a3d1a4b
  (fn [x] (let [helper
                (fn process [res cur]
                  (if (every? (comp not coll?) cur) (conj res cur)
                                                    (concat (process res (first cur)) (process res (rest cur)))))]
            (filter (comp not empty?) (helper [] x)))))

(defcheck solution-6a3d67ea
  (fn prog-93 [x]
    [x]
    (filter #(or ((complement sequential?) %) (not-any? sequential? %))
      (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x)))))

(defcheck solution-6b48153d
  (fn [ss]
    (loop [stack (list ss) out []]
      (if-let [s (first stack)]
        (if (not (sequential? (first s)))
          (recur (rest stack) (conj out s))
          (recur (apply conj (rest stack) s) out))
        (reverse out)))))

(defcheck solution-6b644059
  (fn [c]
    (remove nil?
      (loop [f [] r c]
        (let [s (first r)
              i (first s)
              e (rest r)]
          (if (empty? r)
            f (if (coll? i)
                (recur f (conj e (second s) i))
                (recur (conj f s) e))))))))

(defcheck solution-6b88e7e1
  (fn f [coll]
    (loop [coll coll results []]
      (let [item (first coll)]
        (cond
          (empty? coll) results
          (every? coll? item) (recur (rest coll) (vec (concat results (f item))))
          :else
          (recur (rest coll) (vec (conj results item)))
          )
        )
      )
    ))

(defcheck solution-6ba73c3f
  (fn flat [xs]
    (mapcat #(if (= (flatten %) %) [%] (flat %)) xs)))

(defcheck solution-6c424763
  (fn [s]
    (filter #(and (coll? %)
                  (zero? (count (filter identity (map coll? %)))))
      (tree-seq coll? identity s))))

(defcheck solution-6cbb0d34
  (fn [s]
    (loop [result [] remains s]
      (if (empty? remains)
        result
        (let [f (first remains)]
          (if (sequential? (first f))
            (recur result (concat (cons (first f) (rest f)) (rest remains)))
            (recur (conj result f) (rest remains))))))))

(defcheck solution-6d79023c
  (fn [coll]
    (let [nseq?     (complement sequential?)
          terminal? (fn [x] (or (nseq? x)
                                (every? nseq? x)))]
      (letfn [(mostly-flatten [c]
                (loop [c      c
                       answer []]
                  (cond (nil? c) answer
                        (terminal? (first c)) (recur (next c) (conj answer (first c)))
                        :else (recur (concat (mostly-flatten (first c)) (rest c)) answer))))]
        (mostly-flatten coll)))))

(defcheck solution-6d8e169
  (fn pf [s]
    (let [nodes  (atom [])
          ; linearize the input tree structure by using postwalk
          _      (clojure.walk/postwalk #(if (sequential? %) (swap! nodes conj []) (swap! nodes conj %)) s)
          nodes2 (partition-by sequential? @nodes)
          nodes3 (filter #(not (sequential? (first %))) nodes2)]
      nodes3)))

(defcheck solution-6ddc7e36
  (fn p93 [xs]
    (if
     (every? coll? xs)                                      ;if every element in xs is a collection
      (mapcat p93 xs)                                       ;then map this function to the elements and concat the elements in the collection
      [xs])))

(defcheck solution-6e11b3d5
  (fn f [s]
    (if (every? coll? s)
      (mapcat f s)
      (list s))))

(defcheck solution-6e31eb7f
  (fn [coll]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? identity coll))))

(defcheck solution-6e5c507c
  (fn [s]
    (let [
          coll-wo-subcolls (fn [s] (and (coll? s) (not-any? coll? s)))
          result           (filter coll-wo-subcolls (tree-seq coll? seq s))]
      (if (vector? s) (vec result) result))))

(defcheck solution-6ecf0056
  (fn f [xs]
    (if (every? sequential? xs) (mapcat f xs) [xs])))

(defcheck solution-6edff597
  (fn partially-flatten-a-sequence [v]
    (reduce

      (fn [a x]
        (if (coll? (first x))
          (into a (partially-flatten-a-sequence x))
          (conj a x)))

      []
      v)))

(defcheck solution-6f49fec6
  (fn p [col]
    (if (every? sequential? col)
      (mapcat p col)
      [col])))

(defcheck solution-6fd21ebf
  (fn f [s]
    (if (coll? (first s))
      (mapcat f s)
      [s])))

(defcheck solution-70e04bf
  (fn drill [c]
    (if (and (coll? c) (some coll? c))
      (mapcat drill c)
      [c])))

(defcheck solution-70fe5e68
  (fn
    p
    [s]
    (if (empty? s) []
                   (let
                    [[h & t] s n (p t)]
                     (if (or (not (coll? h)) (every? (comp not coll?) h))
                       (cons h n)
                       (concat (p h) n))))))

(defcheck solution-716e8337
  (fn p-flat [i]
    (filter
      #(not (sequential? (first %)))
      (tree-seq
        #(and
          (sequential? %)
          (sequential? (first %))) identity i))))

(defcheck solution-71b4d89f
  (fn unpack
    ([seq]
     (unpack seq []))
    ([seq res]
     (let [elem  (first seq)
           other (rest seq)]
       (if (not (sequential? elem))
         (conj res seq)
         (let [res (unpack elem res)]
           (if (empty? other)
             res
             (unpack other res))))))))

(defcheck solution-720e20ee
  (fn partially-flatten [s]
    (->> s
      (tree-seq coll? seq)
      (filter #(and (coll? %) (every? (complement coll?) %))))))

(defcheck solution-72978684
  (fn partially-flatten
    [coll]
    (mapcat
      (fn [x]
        (if (not (some coll? x))
          [x]
          (partially-flatten x)))
      coll)))

(defcheck solution-73016c6e
  (fn [s]
    (->> (tree-seq coll? seq s)
      (filter #(and (coll? %)
                    (every? (complement coll?) %))))))

(defcheck solution-730be96
  (fn _ [x]
    (if
     (not (sequential? (first x))) [x]
                                   (reduce concat (map _ x))
                                   )))

(defcheck solution-731ab769
  (fn f [s]
    (let [plain-seq? (fn [s] (reduce #(and %1 %2) (map (complement sequential?) s)))]
      (mapcat #(if (plain-seq? %) (list %) (f %)) s))))

(defcheck solution-738617d7
  (fn [coll]
    (let [res (atom [[]])]
      (clojure.walk/postwalk
        (fn [f]
          (if (sequential? f)
            (swap! res #(if (empty? (last %)) % (conj % [])))
            (swap! res #(update-in % [(- (count %) 1)] conj f))))
        coll)
      (butlast @res))))

(defcheck solution-7393f8a
  (fn dfflat [list]
    (if (every? sequential? list)
      (mapcat dfflat list)
      [list])))

(defcheck solution-740e15d2
  (fn pfs [coll]
    (if (not-any? sequential? coll)
      [coll]
      (mapcat pfs coll))))

(defcheck solution-7446f20d
  (fn partial-flatten [coll]
    (mapcat
      (fn [c]
        (if (and (< 1 (count c)) (every? coll? c))
          (partial-flatten c)
          [(flatten c)]))
      coll)))

(defcheck solution-748ec0f0
  (fn alt-flatten [coll]
    (cond (not (sequential? coll)) coll
          (not (sequential? (first coll))) [coll]
          :else (mapcat alt-flatten coll))))

(defcheck solution-74947a32
  (fn pflat [coll]
    (let [l (first coll) r (next coll)]
      (concat
        (if (and (coll? l) (not-any? coll? l))
          [l]
          (pflat l))
        (when (coll? r)
          (pflat r))))))

(defcheck solution-74a1f6ad
  (fn pflat [coll]
    (cond (and (sequential? coll) (sequential? (first coll)))
          (mapcat pflat coll)
          :else
          (list coll))))

(defcheck solution-7518907b
  (fn rec-flatten [r [a & xs :as v]]
    #_(println "rec-flatten " r " ; " v)
    (if (nil? a)
      r
      (if (sequential? a)
        (if (reduce #(and %1 (not (sequential? %2))) true a)
          (rec-flatten (concat r [a]) xs)
          (rec-flatten (concat r (rec-flatten [] a)) xs))
        (rec-flatten (conj r a) xs)))) [])

(defcheck solution-75f93d40
  (fn semi-flatten [s] (mapcat #(if (= (count (flatten %)) (count %)) (list (flatten %)) (semi-flatten %)) s)))

(defcheck solution-764cf1a4
  (fn f [xs]
    (if (and (coll? xs) (coll? (first xs))) (mapcat f xs) (list xs))))

(defcheck solution-764d693a
  (fn fl [s] (mapcat #(if (coll? (first %)) (fl %) [%]) s)))

(defcheck solution-769c907a
  (fn f [xs]
    (if (not-any? sequential? xs)
      [xs]
      (mapcat #(f %) xs))))

(defcheck solution-76ee49ce
  (fn [xs] (filter #(not-any? coll? %) (tree-seq #(every? coll? %) identity xs))))

(defcheck solution-76efe159
  (fn [a] (filter #(and (coll? %) (not (nil? (first %))) (not (coll? (first %)))) (tree-seq coll? seq a))))

(defcheck solution-774d2dae
  (fn [x]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x)))))

(defcheck solution-7765488d
  (fn pflat [x]
    (if (or (not (sequential? x))
            (not-any? sequential? x))
      [x]
      (mapcat pflat x))))

(defcheck solution-77c1e000
  (fn bigf [ol]
    (if (coll? (first ol))
      (mapcat bigf ol)
      [ol]
      )
    ))

(defcheck solution-77f44533
  (fn partial-flatten [coll]
    (letfn [(seq-of-things? [xs]
              (not-any? sequential? xs))
            (flat-to-vector [v el]
              (if (seq-of-things? el)
                (conj v el)
                (vec (concat v (partial-flatten el)))))]

      (reduce flat-to-vector [] coll))))

(defcheck solution-782745cf
  reduce (fn rf [acc m]
           (if (and (coll? m) (coll? (first m)))
             (reduce rf acc m)
             (conj acc m))) [])

(defcheck solution-782e3fd2
  (fn [coll] (filter (complement #(some coll? %)) (rest (tree-seq #(some coll? %) seq coll)))))

(defcheck solution-79031fd8
  (fn f [xs]
    (mapcat #(if (coll? (first %)) (f %) [%]) xs)))

(defcheck solution-795a6e0a
  (fn flat-partial [coll]
    (if (coll? (first coll))
      (mapcat flat-partial coll)
      [coll])))

(defcheck solution-798185a1
  (fn pf [seq]
    #_(prn seq)
    (cond
      (empty? seq)
      seq
      (and (coll? (first seq)) (not (coll? (ffirst seq))))
      (concat [(first seq)]
        (pf (rest seq)))
      :else
      (concat (pf (first seq)) (pf (rest seq))))))

(defcheck solution-7982b9e4
  (fn pflatten [s]
    (if (coll? (first s)) (mapcat pflatten s) (list s))))

(defcheck solution-799f12c7
  (fn flatn [x] (if (coll? x) (if (coll? (first x)) (mapcat flatn x) [x]) [x])))

(defcheck solution-79acc582
  (fn pf [s]
    (if (some coll? s)
      (mapcat pf s)
      [s])))

(defcheck solution-79d1bb1e
  (fn [s]
    (filter #(and (sequential? %) (not (sequential? (first %)))) (tree-seq sequential? seq s))))

(defcheck solution-7a488116
  (fn [x]
    (filter #(and (sequential? %) (every? (complement sequential?) %))
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-7a70f64f
  #(->> %
     (tree-seq (comp sequential? first) seq)
     (remove (comp sequential? first))))

(defcheck solution-7a71d0d6
  (fn partially-flatten [xs]
    (cond
      (empty? xs)
      []
      (and
       (sequential? (first xs))
       (not (not-any? sequential? (first xs)))
       )
      (concat (partially-flatten (first xs)) (partially-flatten (rest xs)))
      :else
      (cons (first xs) (partially-flatten (rest xs)))
      )
    ))

(defcheck solution-7a925e7
  (fn [x]
    (filter #(every? (complement sequential?) %)
      (filter sequential?
        (rest (tree-seq sequential? seq x))))))

(defcheck solution-7a96de78
  (fn find-leaves [s]
    (if (every? #(or (list? %) (vector? %) (seq? %)) s)
      (mapcat find-leaves s)
      [s])))

(defcheck solution-7b3f3f37
  (fn f
    ([s]
     (if (not-any? coll? s)
       (list s)
       (mapcat f s)))))

(defcheck solution-7b4018f6
  (fn partil-flattern [ts]
    (filter #(not (coll? (first %)))
      (tree-seq #(coll? (first %)) identity ts))))

(defcheck solution-7b928b99
  (fn flat [[h & t :as xs]]
    (if xs
      (if (coll? (first h))
        (concat (flat h) (flat t))
        (cons h (flat t))))))

(defcheck solution-7bcad0da
  (fn f1
    [[h & t]]
    (when (seq h)
      (if (some (complement coll?) h)
        (concat (list h) (f1 t))
        (concat (f1 h) (f1 t))))))

(defcheck solution-7bdc2909
  (fn pf [coll]
    (if (every? (complement coll?) coll)
      [coll]
      (mapcat pf coll))))

(defcheck solution-7bec3ec1
  (fn r [coll]
    (if (not (sequential? (first coll))) [coll]
                                         (apply concat (map r coll)))))

(defcheck solution-7cbc2f1b
  #(loop [s   %
          acc []]
     (cond
       (empty? s) acc
       (coll? (first (first s))) (recur (concat (first s) (rest s)) acc)
       :otherwise (recur (rest s) (conj acc (first s))))))

(defcheck solution-7ce980cc
  (fn [s]
    (letfn [(sequential-1 [x]
              (if (sequential? x)
                (every? (complement sequential?) x)
                false))]
      (filter sequential-1 (rest (tree-seq sequential? seq s))))
    ))

(defcheck solution-7d3c97f5
  (fn [aa]
    (filter (fn [x] (= x (flatten x))) (tree-seq sequential? identity aa))))

(defcheck solution-7d4fbeeb
  (fn s [c] (if (some coll? c) (mapcat s c) [c])))

(defcheck solution-7d797353
  #(letfn [(mf [result e] (if (= e (flatten e)) (conj result e) (into result (reduce mf [] e))))] (reduce mf [] %)))

(defcheck solution-7debcd93
  (fn pf [x] (if (and (coll? x) (some coll? x))
               (mapcat pf x)
               [x])))

(defcheck solution-7e12093f
  (fn pflatten [s]
    (filter #(and (sequential? %)
                  ((complement sequential?) (first %)))
      (rest (tree-seq sequential? seq s)))))

(defcheck solution-7e6d4a63
  (fn flat [tree]
    (reduce
      (fn [acc x]
        (if (every? sequential? x)
          (into [] (concat acc (flat x)))
          (conj acc x)))
      []
      tree)))

(defcheck solution-7ef3ab51
  (fn f [[h & t]]
    (if h
      (concat (if (and (sequential? h)
                       (some sequential? h))
                (f h)
                [h])
        (f t))
      nil)))

(defcheck solution-7f02f0e5
  (fn part-flat [x]
    (if (not (sequential? (first x))) [x]
                                      (reduce concat (map part-flat x)))))

(defcheck solution-7f06e99c
  (fn fl [input]
    (letfn
     [(li? [i]
        (or
         (vector? i)
         (list? i)))
      (dcoll? [i]
        (and
         (li? i)
         (li? (first i))))
      (afl [i]
        (if
         (dcoll? i)
          (mapcat
            afl
            i)
          [i]))]
      (afl input))))

(defcheck solution-7f1793fe
  (fn partial-flatten
    ([xs] (partial-flatten xs []))
    ([xs acc]
     (if (empty? xs)
       acc
       (let [x (first xs)]
         (if (every? sequential? x)
           (partial-flatten (rest xs) (into acc (partial-flatten x)))
           (partial-flatten (rest xs) (conj acc x))))))))

(defcheck solution-7f72a85d
  (fn flatten2 [coll]
    (loop [[head & tail] coll
           acc []]
      (cond
        (nil? head) acc
        (not (coll? head)) (recur tail (conj acc head))
        (= head (flatten head)) (recur tail (conj acc head))
        :else (recur (concat head tail) acc)))))

(defcheck solution-7f792953
  (fn pf [coll]
    (if (and (sequential? coll) (not (sequential? (first coll))))
      [coll]
      (mapcat #(pf %) coll))))

(defcheck solution-7fc101f1
  (fn f [l]
    (filter #(and (coll? %) (not-any? coll? %)) (tree-seq coll? seq l))))

(defcheck solution-8059abff
  (letfn [(unwrapped? [x]
            (or (not (coll? x))
                (not-any? coll? x)))
          (unwrap [xs]
            (cond
              (empty? xs) '()
              (unwrapped? (first xs)) `(~(first xs) ~@(unwrap (rest xs)))
              :else `(~@(unwrap (first xs)) ~@(unwrap (rest xs)))))]
    (fn [xs] (unwrap xs))))

(defcheck solution-80c3a10d
  (fn n93 [coll]
    (letfn [(one-level? [x] (and (sequential? x) (every? (complement sequential?) x)))]
      (filter one-level? (rest (tree-seq sequential? seq coll))))))

(defcheck solution-80cc4c78
  (fn my-flatten [xs]
    (if (empty? xs) nil
                    (let [[h & t] xs]
                      (lazy-cat
                        (if (coll? h)
                          (if (not-any? coll? h) [h] (my-flatten h))
                          [h])
                        (my-flatten t))))))

(defcheck solution-810abefb
  (fn partially-flatten
    [coll]
    (let [leaf? (partial every? (comp not coll?))]
      (if (leaf? coll)
        (vector coll)
        (reduce (fn [acc curr]
                  (let [flat-child (partially-flatten curr)]
                    (concat acc flat-child)))
          []
          coll)))))

(defcheck solution-8136e8eb
  (fn flat [coll]
    (if (every? sequential? coll)
      (mapcat flat coll)
      [coll])))

(defcheck solution-81bd38cd
  (fn f [s]
    (if (some coll? s)
      (mapcat #(if (some coll? %) (mapcat f %) [%]) s)
      [s])))

(defcheck solution-82689680
  (fn [x] (filter #(not-any? coll? %) (tree-seq #(some coll? %) identity x))))

(defcheck solution-82759618
  (fn partial-flatten [coll]
    (let [ele   (first coll)
          coll- (rest coll)]
      (if (nil? ele)
        '()
        (if (sequential? ele)
          (concat (if (sequential? (first ele))
                    (partial-flatten ele)
                    (list ele)) (partial-flatten coll-))
          (concat (list ele) (partial-flatten coll-)))))))

(defcheck solution-832bd2fa
  (fn _nest [_seq]
    (filter #(and (sequential? %) ((complement sequential?) (first %))) (tree-seq sequential? seq _seq))
    ))

(defcheck solution-83563daa
  (fn exp [acc s]
    (if (some coll? s)
      (exp (exp acc (first s)) (rest s))
      (if (seq s)
        (conj acc s)
        acc))) [])

(defcheck solution-8382ca15
  (fn ! [c]
    (let [fst (first c) rst (rest c)]
      (if (and (sequential? fst)
               (every? #(not (sequential? %)) fst))
        (cons fst (lazy-seq (when (not (empty? rst)) (! rst))))
        (! (cons (first fst) (if (empty? (rest fst))
                               rst
                               (cons (rest fst) rst))))))))

(defcheck solution-839d8374
  #(reverse (
             (fn red [coll] (reduce (fn [acc e] (do #_(println e)
                                                    (if (sequential? (first e)) (concat (red e) acc) (concat [e] acc))
                                                    )) [] coll)
               )
             %)))

(defcheck solution-843a4d9b
  (fn flatten-outer [[x & args]]
    (when x
      (if (or (not (coll? x)) (every? (comp not coll?) x))
        (conj (flatten-outer args) x)
        (concat (flatten-outer x) (flatten-outer args))))))

(defcheck solution-844924dd
  (fn pfas [[x & xs]]
    (if (nil? x)
      nil
      (if (and (coll? x) (not (coll? (first x))))
        (cons x (pfas xs))
        (pfas (concat x xs))))))

(defcheck solution-848dda46
  (fn [s]
    (loop [rs s acc []]
      (if (empty? rs)
        acc
        (let [head (first rs)]
          (if (and (sequential? head) (some sequential? head))
            (recur (concat head (rest rs)) acc)
            (recur (rest rs) (conj acc head))))))
    ))

(defcheck solution-84ad6277
  (fn flatten-seq
    [coll]
    (if (not-any? coll? coll)
      [coll]
      (mapcat flatten-seq coll))))

(defcheck solution-84b3cea1
  (fn __ [col]
    (filter #(and (coll? %)
                  (not (coll? (first %))))
      (tree-seq coll? identity col))))

(defcheck solution-850db011
  (fn pf [s]
    (if (empty? s) s
                   (if (coll? (first (first s)))
                     (concat (pf (first s)) (pf (rest s)))
                     (cons (first s) (pf (rest s)))
                     ))
    ))

(defcheck solution-8520ae09
  (fn pfl [l]
    (mapcat
      (fn [i]
        (if (or
             (not (coll? i))
             (some #(not (coll? %)) i))
          [i]
          (pfl i)))
      l)))

(defcheck solution-85227021
  (fn lol [coll]
    (cond
      (and (coll? coll)
           (coll? (first coll))) (into (lol (first coll))
                                   (lol (rest coll)))
      (seq coll) [coll])))

(defcheck solution-85448857
  #(letfn [(nested? [s] (some sequential? s))]
     (filter (complement nested?) (tree-seq nested? seq %))))

(defcheck solution-858f39fc
  (fn f [c]
    (if (every? #(not (coll? %)) c)
      [c]
      (mapcat f c))))

(defcheck solution-85f33419
  (fn p [s f n c]
    (if-let [x (f c)]
      (let [X (p s f n (n c))]
        (if (and (s x) (s (f x)))
          (concat (p s f n x) X)
          (cons x X))))) sequential? first next)

(defcheck solution-8666a2c6
  (fn pflat [xs]
    (if (not (sequential? (ffirst xs)))
      (concat (conj (empty xs) (first xs)) (if (empty? (rest xs)) (empty xs) (pflat (rest xs))))
      (concat (pflat (first xs)) (if (empty? (rest xs)) (empty xs) (pflat (rest xs))))
      )

    ))

(defcheck solution-868de53b
  (fn [s]
    (let [nxt (mapcat #(if (and (coll? %) (every? coll? %)) % (vector %)) s)]
      (if (= s nxt) s (recur nxt)))))

(defcheck solution-86c85a9
  (fn [s]
    (filter #(and (coll? %) (not-any? coll? %))
      (tree-seq coll? seq s))))

(defcheck solution-870d1f84
  (fn f-flat [colls]
    (when (seq colls)
      (if (sequential? (first colls))
        (reduce concat (map f-flat colls))
        [colls]))))

(defcheck solution-8715ba90
  #(reduce (fn flatten [s x]
             (if (coll? (first x))
               (let [outer (flatten s (first x))]
                 (if (next x)
                   (flatten outer (next x))
                   outer))
               (concat s [x])))
     (empty %)
     %))

(defcheck solution-8745dc06
  (fn almost-flatten [coll]
    (reduce (fn [acc s]
              (if (not-any? sequential? s)
                (conj acc s)
                (vec (concat acc (almost-flatten s)))))
      [] coll)))

(defcheck solution-87784780
  (let [node? (comp sequential? first)]
    #(filter (complement node?) (tree-seq node? identity %))))

(defcheck solution-87d115f
  (fn [coll]
    (loop [ret [] c coll]
      (if (empty? c) ret
                     (if (not-any? coll? (first c))
                       (recur (conj ret (first c)) (rest c))
                       (recur ret (concat (first c) (rest c))))))))

(defcheck solution-892594f
  (fn flatbutlast [s]
    (mapcat (fn [maybe-has-subseqs]
              (if (sequential? (first maybe-has-subseqs))
                (flatbutlast maybe-has-subseqs)
                [maybe-has-subseqs]
                )) s)
    ;; (letfn [(any-seq? [s]
    ;;           (reduce #(and %1 %2) (map #(sequential? (first %)) s)))]
    ;;   (if (any-seq? s)
    ;;     (recur (mapcat identity s))
    ;;     s))
    ))

(defcheck solution-89c11da6
  (fn partial-flatten [c]
    (if (not (coll? (first c))) [c]
                                (mapcat partial-flatten c))
    ))

(defcheck solution-89d67d85
  (fn pfl [ss]
    (if (some (complement sequential?) ss)
      [ss]
      (mapcat pfl ss))))

(defcheck solution-89e1fbc0
  (fn pf [coll]
    (reduce concat []
      (for [x coll]
        (if (every? (complement coll?) x)
          (vector x)
          (pf x))))))

(defcheck solution-89ee1324
  (fn flat [coll]
    (if (not-any? coll? coll)
      (vector coll)
      (mapcat flat coll))))

(defcheck solution-8a0923b3
  (fn [s]
    (letfn [(base-sequential? [s]
              (and (sequential? s)
                   (not (some sequential? s))))]
      (filter base-sequential? (tree-seq sequential? seq s)))))

(defcheck solution-8a7cadd4
  (fn [c]
    (->> c
      (tree-seq coll? seq)
      (filter #(and (coll? %) (every? (complement coll?) %))))))

(defcheck solution-8b3e4621
  (fn f [l]
    (cond
      (not-every? coll? l) [(mapcat #(if (coll? %) (f %) [%]) l)]
      :else (mapcat f l))
    ))

(defcheck solution-8b55865d
  (fn fu [s]
    (reduce
      (fn x [r e]
        (if (coll? e)
          (if (coll? (first e))
            (apply conj r (fu e))
            (conj r e))
          r)) [] s)))

(defcheck solution-8ba29630
  (fn [coll]
    (loop [acc [] coll coll]
      (if-let [[a & coll] coll]
        (if (coll? a)
          (if (every? #(not (coll? %)) a)
            (recur (conj acc a) coll)
            (recur acc (if (empty? coll) (vec a) (conj (vec a) coll))))
          (recur (conj acc a) coll))
        acc))))

(defcheck solution-8bcb8aba
  #(filter (partial every? (complement coll?))
     (tree-seq (partial some coll?) seq %)))

(defcheck solution-8c91afc9
  (fn flatreduce [col]
    (letfn [(flatsq [col] (not-any? sequential? col))]
      (if (every? flatsq col)
        col
        (flatreduce (reduce #(if (flatsq %2) (concat %1 [%2]) (concat %1 %2)) [] col))))))

(defcheck solution-8d1ed5d6
  (fn pfas [s]
    (reduce (fn [result x]
              (concat result
                (if (every? #(not (coll? %)) x)
                  (vector x)
                  (pfas x))))
      [] s)))

(defcheck solution-8d6513a1
  (fn flat [s]
    (letfn [
            (get-last-seq [acc s]
              (if (sequential? (first s))
                (if (sequential? (ffirst s))
                  (get-last-seq acc (first s))
                  (if (not-empty (rest s))
                    (get-last-seq (conj acc (first s)) (rest s))
                    (conj acc (first s))))
                (conj acc s)))]
      (reduce get-last-seq [] s))))

(defcheck solution-8e0f5958
  (fn partially-flatten
    ([s]
     (partially-flatten [] s))
    ([res s]
     (reduce (fn [res s]
               (if (every? (comp not sequential?) s)
                 (conj res s)
                 (partially-flatten res s))) res s))))

(defcheck solution-8ebc0f7a
  (fn [c]
    (letfn [(has-col? [c] (some coll? c))
            (no-subs? [c] (every? nil? (map has-col? c)))
            (de-level [c] (reduce #(if (has-col? %2)
                                     (concat %1 %2)
                                     (concat %1 (list %2)))
                            (empty c)
                            c))
            ]
      (loop [c c]
        (if (no-subs? c)
          c
          (recur (de-level c)))))))

(defcheck solution-8ee10cf1
  (fn [l]
    (letfn [(worker [l s]
              (if (empty? l) s
                             (if (coll? (first l))
                               (recur (rest l) (worker (first l) s))
                               (conj s l))))]
      (worker l []))))

(defcheck solution-8ef116a0
  (fn f [x]
    (apply concat
      (map
        #(if (coll? (first %))
           (f %)
           [%])
        x))))

(defcheck solution-8f9a5c78
  (fn flattenit [x]
    (let [mapped (reduce #(if (not-every? coll? %2) (conj %1 %2) (into %1 %2)) [] x)]
      (if
       (= mapped x)
        mapped
        (flattenit mapped)
        )
      )
    ))

(defcheck solution-9027645c
  mapcat #(if (list? %)
            (let [[a b] (seq %)]
              (if (list? a)
                [a (flatten b)]
                [%]))
            [(flatten %)]))

(defcheck solution-9099246
  (fn f
    ([[head & tail :as xs]]
     (f [] head tail))

    ([a h t]
     (let [tgt? (fn [s] (and (coll? s)
                             (every? (comp not coll?) s)))]

       (if (tgt? h)
         (f (conj a h) (first t) (next t))

         (if h
           (f (f a (first h) (next h)) (first t) (next t))
           a))))))

(defcheck solution-90e24293
  (fn [x]
    (filter #(and (coll? %)
                  (not (coll? (first %))))
      (rest (tree-seq coll? identity x)))))

(defcheck solution-9161e864
  (fn [s]
    (filter #(and (sequential? %)
                  (not (sequential? (first %))))
      (tree-seq sequential? seq s))))

(defcheck solution-917fa6bf
  (fn walk [c] (if (= c (flatten c)) [c] (mapcat walk c))))

(defcheck solution-91867432
  (fn [col]
    (filter #(and (sequential? %)
                  (every? (complement sequential?) %))
      (rest (tree-seq sequential? seq col)))))

(defcheck solution-919609b2
  (letfn [(flat [c] (if (and (sequential? c) (sequential? (first c)))
                      (mapcat flat c)
                      [c]))]
    flat))

(defcheck solution-91c009ef
  (fn [s]
    (filter (fn [x]
              (and (sequential? x) (not (sequential? (first x)))))
      (tree-seq sequential? seq s))))

(defcheck solution-91e62f8b
  (fn partialflat [s]
    (if (every? #(not (sequential? %)) s) [s] (mapcat partialflat s))))

(defcheck solution-9242057e
  (fn ex93
    [s]
    (if-not (coll? (first s))
      [s]
      (apply concat (map ex93 s)))))

(defcheck solution-92521c70
  (fn f
    [[h & t]]
    (if (nil? h)
      nil
      (if (or (not (coll? h)) (not-any? coll? h))
        (cons h (f t))
        (concat (f h) (f t))))))

(defcheck solution-936e518b
  (fn [[a1 b1 :as ls1]]
    (loop [a a1 ls (rest ls1) acc []]
      (cond
        (or (nil? a) (empty? a)) acc
        (= (flatten a) a) (recur (first ls) (rest ls) (conj acc a))
        (empty? (rest a)) (recur (first a) ls acc)
        :else (recur (first a) (conj ls (rest a)) acc)))))

(defcheck solution-93a657b5
  (fn fltp [x]
    (filter #((complement sequential?) (first %)) (tree-seq #(sequential? (first %)) seq x))))

(defcheck solution-93b7b436
  (fn f [l]
    (reduce
      #(if (some coll? %2) (into %1 (f %2)) (conj %1 %2))
      []
      l)))

(defcheck solution-93c503c0
  (fn [s]
    (let [found (atom [])]
      (clojure.walk/postwalk #(if (and (sequential? %) (not (some sequential? %)))
                                (do (swap! found conj %)
                                    %)
                                %)
        s)
      @found)))

(defcheck solution-93d8419e
  (fn flat [x]
    (if (sequential? (first x))
      (mapcat flat x)
      [x]
      )))

(defcheck solution-93fc364a
  (fn [s]
    (for
     [e (tree-seq
          sequential?
          seq
          s)
      :when
      (and
       (sequential? e)
       (every?
         #(not
            (sequential?
              %))
         e))]
      e)))

(defcheck solution-941493d0
  (fn f [xs]
    (cond
      (empty? xs) []
      (coll? (ffirst xs)) (concat (f (first xs)) (f (rest xs)))
      :else (concat [(first xs)] (f (rest xs))))))

(defcheck solution-9436e8e8
  (fn c-flatten [coll]
    (let [l (first coll) r (next coll)]
      (concat
        (if (sequential? (first l)) (c-flatten l) [l])
        (if (sequential? (first r)) (c-flatten r))))))

(defcheck solution-952c6af1
  (fn partial-flatten [l]
    (letfn [(unnest [s]
              (if (empty? s)
                '()
                (if (coll? (first s))
                  (if (coll? (first (rest s)))
                    (concat (unnest (first s)) (unnest (rest s)))
                    (concat (unnest (first s)) (rest s)))
                  (if (coll? (first (rest s)))
                    (concat (first s) (unnest (rest s)))
                    (list s)))))]
      (unnest l))))

(defcheck solution-954c4733
  (fn [sq] (letfn [(nscoll? [c] (not (and (coll? c) (every? (complement coll?) c))))]
             (loop [l sq] (if (some nscoll? l)
                            (recur (mapcat #(if (nscoll? %) % [%]) l))
                            l)))))

(defcheck solution-957d310d
  (fn flat [v]
    (loop [[f & r] v
           ret []]
      (if f
        (recur r (concat ret (if (sequential? (first f)) (map flatten f) [f])))
        ret))))

(defcheck solution-9588987e
  (fn f [s] (

              reduce #(
                        if (and (counted? %2) (counted? (first %2)))
                        (concat %1 (f %2))
                        (concat %1 [%2])
                        ) [] s
              )))

(defcheck solution-959c0ce5
  (fn pflat [x]
    (letfn [(notflat? [c] (reduce #(or %1 %2) (map (fn [z] (coll? z)) c)))]
      (cond (empty? x) x
            (and (coll? x) (notflat? x)) (concat (pflat (first x)) (pflat (rest x)))
            :else (list x))
      )
    ))

(defcheck solution-96032f34
  (fn partial-flatten [s]
    (if (every? sequential? s)
      (mapcat partial-flatten s)
      [s])))

(defcheck solution-9629d0a2
  (fn flat [[a & r]] (if r (concat (flat [a]) (flat r)) (if (= a (flatten a)) [a] (flat a)))))

(defcheck solution-966f16fa
  (fn [coll]
    (letfn [(flatten1 [coll]
              (mapcat
                (fn [x]
                  (if (sequential? (first x))
                    (flatten1 x)
                    [x]))
                coll))]
      (flatten1 coll))))

(defcheck solution-96972cd5
  (fn intocol [collect]
    (let [col?   (fn col? [col]
                   (or (vector? col) (list? col))
                   ),
          newcol (cond (vector? collect) (vector)
                       (list? collect) (list)
                       :else nil)
          ]

      ((fn inPeek [col]
         (let [inPop (fn inPop [col]
                       #_(println (str "pop " col))
                       (if (empty? (pop col))
                         (if (col? (peek (peek col)))
                           (inPop (peek col))
                           col
                           )
                         (conj (inPop (pop col))
                           (inPeek (peek col)))
                         )
                       ),
               head  (peek col),
               other (pop col)]
           #_(println (str "peek" col))
           (if (not (col? head))
             col
             (if (empty? other)
               (inPeek head)
               (conj (inPop other) (inPeek head))
               )
             )
           )
         ) collect)
      )
    ))

(defcheck solution-96aab979
  (fn flat-e
    [x]
    (let [level1? (fn [x] (zero? (count (filter #(sequential? %) x))))]
      (cond
        (empty? x) '()
        (level1? x) (list x)
        :else (concat (flat-e (first x)) (flat-e (rest x)))))))

(defcheck solution-9719dc7a
  (fn partially-flatten- [coll]
    "93. Write a function which flattens any nested combination of
  sequential things (lists, vectors, etc.), but maintains the lowest
  level sequential items."
    (lazy-seq
      (if-let [s (seq coll)]
        (if (some sequential? (first s))
          (concat (partially-flatten- (first s)) (partially-flatten- (rest s)))
          (conj (partially-flatten- (rest s)) (first s)))))))

(defcheck solution-9801c0b2
  (fn pflatten [tree]
    (if (every? sequential? tree)
      (mapcat pflatten tree)
      [tree])))

(defcheck solution-980c2c08
  (fn pconcat [s]
    (letfn [(seq1? [s] (and (sequential? s) (every? #(not (sequential? %)) (seq s))))]
      (if (sequential? s)
        (if (seq s)
          (if (seq1? (first s))
            (lazy-seq (cons (first s) (pconcat (rest s))))
            (concat (pconcat (first s)) (pconcat (rest s))))
          s)
        s))))

(defcheck solution-986f6d05
  (fn f [x]
    (if (every? coll? x)
      (mapcat f x)
      [x])))

(defcheck solution-989ed6da
  (fn f [c]
    (let [l (first c) r (next c)]
      (concat
        (if (sequential? (first l)) (f l) [l])
        (if (sequential? (first r)) (f r))))))

(defcheck solution-990328e3
  (fn f [tree]
    (if (every? sequential? tree)
      (mapcat f tree)
      [tree])))

(defcheck solution-992a1f23
  (fn [s]
    (loop [tmps s ans '()]
      (if (empty? tmps)
        (reverse ans)
        (if (sequential? (first (first tmps)))
          (recur (conj (conj (rest tmps) (rest (first tmps))) (first (first tmps))) ans)
          (if (< 0 (count (first tmps)))
            (recur (rest tmps) (conj ans (first tmps)))
            (recur (rest tmps) ans)))))))

(defcheck solution-99da599f
  (fn partflat
    ([x] (partflat x x))
    ([expandedFrom x]
     (if (sequential? x)
       ; Recurse and concatenate results
       (distinct (mapcat (partial partflat x) x))
       ; else just eval to a the sequence I was found in
       (vector expandedFrom)
       ))))

(defcheck solution-99da9134
  (fn f [z]
    (let [l (apply concat (map #(if (every? coll? %) % [%]) z))]
      (if (= z l)
        z
        (recur l)))))

(defcheck solution-99e6691e
  (fn partialflatten [s]
    (if (sequential? s)
      (if (reduce #(or %1 %2) (map sequential? s))
        (mapcat partialflatten s)
        (vector s))
      (vector s))))

(defcheck solution-9b312eda
  (fn a [coll]
    (if (some sequential? coll)
      (mapcat a coll)
      [coll])))

(defcheck solution-9b337df6
  (fn my-flatten [coll]
    (filter #(not (sequential? (first %))) (tree-seq #(sequential? (first %)) identity coll))))

(defcheck solution-9ba3979f
  (fn [x]
    (letfn [(max-one-level? [c] (if (sequential? c) (every? (complement sequential?) c) true))]
      (filter max-one-level?
        (rest (tree-seq (complement max-one-level?) seq x))))))

(defcheck solution-9bb25804
  (fn partially-flatten [xs]
    (loop [[x & xs'] xs, acc []]
      (cond (nil? x)
            acc
            (not (sequential? x))
            (recur xs' (conj acc x))
            (every? (complement sequential?) x)
            (recur xs' (conj acc x))
            :else
            (recur xs' (vec (concat acc (partially-flatten x))))))))

(defcheck solution-9c116086
  (fn [s] (filter #(and (sequential? %) ((comp not sequential? first) %)) (tree-seq sequential? seq s))))

(defcheck solution-9c8c2b6
  (fn almost-flatten [coll]
    (letfn [(step [coll result]
              (if (seq coll)
                (let [h (first coll)]
                  (step (next coll)
                    (if (some coll? h)
                      (reduce #(conj %1 %2) result (step h []))
                      (conj result h))))
                result))]
      (step coll []))))

(defcheck solution-9c99d592
  (fn partial-flatten [seq]
    (if (every? sequential? seq)
      (mapcat partial-flatten seq)
      [seq])))

(defcheck solution-9d190d8e
  (fn [coll]
    (->> coll
      (tree-seq sequential? seq)
      (filter #(and (sequential? %) (not (sequential? (first %))))))))

(defcheck solution-9d1b2931
  (fn [x]
    (filter #(not (sequential? (first %)))
      (rest (tree-seq #(and (sequential? %) (sequential? (first %))) seq x)))))

(defcheck solution-9d825119
  (fn %r [[f & r :as all]]
    (cond (and (coll? f) (not-any? coll? f))
          (cons f (%r r))
          f (concat (%r f) (%r r)))))

(defcheck solution-9da44a24
  (letfn [(f [s]
            (if (some sequential? s)
              (mapcat f s)
              [s]))]
    f))

(defcheck solution-9e0c5a04
  (fn unwrap [xs]
    (letfn
     [(impl [[x & _ :as xs]]
        (if (sequential? x)
          (if (= 1 (count xs)) (recur x)
                               (mapcat impl xs)
                               )
          [xs]))]
      (mapcat impl [xs]))))

(defcheck solution-9ea69699
  (fn [s]
    (letfn [(flat [xs]
              (if (and (coll? xs) (coll? (first xs)))
                (mapcat flat xs)
                [xs]))]
      (mapcat flat s))))

(defcheck solution-9ef268c
  (fn he [c]
    (let [l (first c) r (next c)]
      (concat
        (if (some sequential? l)
          (he l)
          [l])
        (when (sequential? r)
          (he r))))))

(defcheck solution-9f06fae7
  (fn [s]
    (letfn [(hc? [e] (some coll? e))
            (dno [v] (mapcat #(if (hc? %) % [%]) v))]
      (if (some hc? s)
        (recur (dno s))
        s))))

(defcheck solution-9ff2309c
  (letfn [
          (items? [e] (not (coll? e)))
          (itemlist? [lst] (and (coll? lst) (every? items? lst)))]
    (fn [lst]
      (filter itemlist? (tree-seq coll? seq lst)))))

(defcheck solution-a011faca
  (fn [v] (filter (fn [x] (and (sequential? x) (every? identity (map (complement sequential?) x))))
            (rest (tree-seq sequential? seq v)))))

(defcheck solution-a04303ae
  (fn f [s]
    (if (sequential? (first s))
      (mapcat f s)
      [s])))

(defcheck solution-a0540159
  (fn pf [xs]
    (filter #(and (coll? %)
                  (every? (complement coll?) %))
      (tree-seq coll? identity xs))))

(defcheck solution-a09eb4d4
  (fn flattish [s]
    (if (coll? (first s))
      (mapcat flattish s)
      [s])))

(defcheck solution-a0bde407
  (fn flat [ss]
    (mapcat #(if (coll? (first %)) (flat %) [%]) ss)))

(defcheck solution-a0d1118c
  (fn p [s] (if (some coll? s) (mapcat p s) [s])))

(defcheck solution-a15a3a8d
  (fn part-flat [coll]
    (reduce (fn flat [acc subcoll]
              (if (empty? subcoll)
                acc
                (if (not (sequential? (first subcoll)))
                  (conj acc subcoll)
                  (into acc (part-flat subcoll)))))
      []
      coll)))

(defcheck solution-a16fef9c
  (fn [xs]
    (reduce (fn [t v] (if (and (coll? v) (not (coll? (first v)))) (conj t v) t)) [] (tree-seq coll? identity xs))))

(defcheck solution-a18115a4
  (fn [coll]
    (letfn [(my-flatten [coll]
              (loop [coll coll
                     r    []]
                (let [h (first coll)
                      t (rest coll)]
                  (if (sequential? h)
                    (recur (first coll)
                      (if (empty? t)
                        r
                        (concat r (my-flatten t))))
                    (conj r coll)))))]
      (my-flatten coll))))

(defcheck solution-a218c1dc
  (fn F [S] (mapcat (fn [s] (if (and (coll? s) (some coll? s)) (F s) [s])) S)))

(defcheck solution-a27286a2
  (fn [sqn]
    (filter #(not (empty? %))
      (loop [s (first sqn) t (rest sqn) r []]
        (cond (and (empty? s) (empty? t)) r
              (and (> (count s) 1) (sequential? (first s))) (recur (first s) (conj (rest s) t) r)
              (sequential? (first s)) (recur (first s) t r)
              :else
              (recur (first t) (rest t) (conj r s)))))))

(defcheck solution-a2837aaa
  (fn pf [s] (if (sequential? (first s)) (mapcat pf s) [s])))

(defcheck solution-a29a94e5
  (fn iter
    ([s] (iter s []))
    ([[a & r] out]
     (cond (nil? a) out
           (not-any? coll? a) (recur r (conj out a))
           :else (recur (concat a r) out)))))

(defcheck solution-a2c27125
  (fn [v]
    (let [o (atom [])]
      (clojure.walk/postwalk #(do (when (and (coll? %) (not-any? coll? %)) (swap! o conj %)) %) v)
      @o
      )
    ))

(defcheck solution-a2e1f147
  (fn f [c]
    (if (coll? c)
      (if (some coll? c)
        (mapcat f c) [c])
      c)))

(defcheck solution-a312c504
  (fn f [[x :as xs]] (if (coll? x) (mapcat f xs) [xs])))

(defcheck solution-a37bb317
  (fn partially-flatten [s]
    (lazy-seq
      (when s
        (let [[x & xs] s]
          (if (and (coll? x) (coll? (first x)))
            (concat (partially-flatten x) (partially-flatten xs))
            (cons x (partially-flatten xs))))))))

(defcheck solution-a394f066
  (fn [c]
    (let [s sequential?]
      (filter #(and (s %) (not (s (first %))))
        (tree-seq s seq c)))))

(defcheck solution-a3cd7a00
  (fn [s]
    (filter
      #(not (sequential? (first %)))
      (tree-seq #(and (sequential? %) (sequential? (first %))) identity s))))

(defcheck solution-a415c015
  (fn f [l] (mapcat #(if (coll? (first %)) (f %) [%]) l)))

(defcheck solution-a430ea9a
  (fn pflat [s]
    (if (some coll? s)
      (mapcat pflat s)
      [s])))

(defcheck solution-a47eaf11
  (fn partially-flatten [xs]
    (if (every? sequential? xs)
      (mapcat partially-flatten xs)
      [xs])))

(defcheck solution-a4e9d694
  (fn partially-flatten [l]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? seq l))))

(defcheck solution-a5328423
  (fn lol [[h :as xs]]
    (if (coll? h)
      (mapcat lol xs)
      [xs])))

(defcheck solution-a56de96c
  (fn pflat [s]
    (letfn [(depth-0? [t] (not (some coll? t)))]
      (reverse
        (reduce
          #(if (depth-0? %2)
             (conj %1 %2)
             (apply conj %1 (pflat %2)))
          '()
          s)))))

(defcheck solution-a6073e72
  (fn partial-flatten [struct]
    (reduce
      (fn [result s]
        (if (or
             (not (sequential? s))
             (every? #(not (sequential? %)) s))
          (concat result (list s))
          (concat result (partial-flatten s))))
      '()
      struct)))

(defcheck solution-a6195301
  (fn [a-seq]
    (letfn [(flat-seq [a-seq at]
              (when (sequential? a-seq)
                (if (every? #(not (sequential? %)) a-seq)
                  (swap! at conj a-seq)
                  (doseq [ss a-seq]
                    (flat-seq ss at)))))]
      (let [at (atom [])]
        (flat-seq a-seq at)
        @at))))

(defcheck solution-a6728e86
  (fn [root]
    (filter
      #(and
        (sequential? %)
        (every? (complement sequential?) %))
      (tree-seq sequential? identity root))))

(defcheck solution-a67ab8dd
  (fn [ls]
    (filter #(not-any? coll? %)
      (tree-seq #(some coll? %) identity ls))))

(defcheck solution-a6ae461d
  (fn pflatten
    [s]
    (if (empty? s)
      s
      (let [f (first s)
            r (rest s)]
        (if (not (coll? (first f)))
          (cons f (pflatten r))
          (concat (pflatten f) (pflatten r)))))))

(defcheck solution-a6bc967b
  (fn f [c]
    (filter #(and (coll? %) (not-any? coll? %)) (tree-seq sequential? seq c))))

(defcheck solution-a6bcb4b2
  (fn flatter [c]
    (loop [c c
           r []]
      (cond
        (nil? (first c)) r
        (coll? (ffirst c)) (recur (concat (first c) (next c)) r)
        :else (recur (rest c) (concat r (vector (first c))))))))

(defcheck solution-a709f970
  (fn partial-flatten [coll]
    (if (some sequential? coll)
      (mapcat partial-flatten coll)
      [coll])))

(defcheck solution-a78ad5c0
  (fn f [s]
    (reduce
      (fn [r e]
        (apply conj r
          (cond (not (coll? e)) (list e)
                (every? (comp not coll?) e) (list e)
                :else (f e))
          ))
      []
      s
      )
    ))

(defcheck solution-a7be3b39
  (fn f [s] (mapcat #(if (every? coll? %) (f %) (list %)) s)))

(defcheck solution-a82aa363
  (fn p-flat [[x & xs]]
    (letfn [(coll-of-atom? [coll]
              (not (coll? (first coll))))]
      (cond (nil? x) nil
            (coll-of-atom? x) (cons x (p-flat xs))
            :else (p-flat (cons (first x) (concat (rest x) xs)))))))

(defcheck solution-a854773
  (fn partial-flatten [coll]
    (reduce (fn [flattened x]
              (if (and (coll? x)
                       (some coll? x)) (concat flattened (partial-flatten x))
                                       (concat flattened (list x)))) '() coll)))

(defcheck solution-a882cbe0
  (fn partial-flatten [seq]
    (mapcat (fn [elem]
              (cond
                (not (coll? elem)) [elem]
                (not-any? coll? elem) [elem]
                :else (partial-flatten elem)))
      seq)
    ))

(defcheck solution-a896d396
  (fn pflatten [[h & t :as l]]
    (cond (or (list? h) (vector? h)) (concat (pflatten h) (pflatten t))
          (empty? l) ()
          :else (list l))))

(defcheck solution-a8a7d2de
  (fn PF [s]
    (let [nseq   (complement sequential?)
          lowest (fn [S] (and (sequential? S) (every? nseq S)))
          rn     (fn RN [S] (reduce #(if (lowest %2) (conj %1 %2) (apply conj %1 %2)) [] S))]
      (first (drop-while #(some (complement lowest) %) (iterate rn s))))
    ))

(defcheck solution-a8d3b886
  (fn pf [coll]
    (mapcat
      #(if (coll? (first %))
         (pf %)
         (list %))
      coll)))

(defcheck solution-a92070df
  (fn fl [[f & r :as p]]
    (if (zero? (count p))
      p
      (if (= f (flatten f))
        (cons f (fl r))
        (concat (fl f) (fl r))))))

(defcheck solution-a941f9b6
  (fn f [x]
    (reduce (fn [a b]
              (concat a
                (if (and (coll? b) (coll? (first b)))
                  (f b)
                  (list b))))
      ()
      x)))

(defcheck solution-a94229d9
  (fn [c]
    (filter #(and (sequential? %) (= % (flatten %)))
      (tree-seq sequential? seq c))))

(defcheck solution-a9433ac
  (fn part-flatten [s]
    (cond
      (not (coll? s)) s
      (some coll? s) (mapcat part-flatten s)
      :else [s])))

(defcheck solution-a943b19c
  (fn part-flat [ls]
    (filter
      (comp not sequential? first)
      (tree-seq sequential? #(filter sequential? %) ls))
    ))

(defcheck solution-a951bb7e
  (fn pf [coll]
    (letfn [(flat-seq? [x] (and (sequential? x) (not-any? sequential? x)))]
      (reduce
        (fn [accum item]
          (if (flat-seq? item)
            (conj (vec accum) item)
            (concat accum (pf item))))
        []
        coll))))

(defcheck solution-a9737d24
  (fn pf [s]
    (mapcat #(if (not-any? sequential? %) (list %) (pf %)) s)))

(defcheck solution-a9f9e970
  (fn flatter
    [x]
    (cond
      (and (sequential? x) (sequential? (first x))) (mapcat flatter x)
      (sequential? x) [x]
      :else x)))

(defcheck solution-a9fb15d1
  (fn pflat [xs]
    (reduce
      (fn [acc, x]
        (if (and
             (coll? x)
             (some coll? x))
          (concat acc (pflat x))
          (concat acc (list x))))
      []
      xs)))

(defcheck solution-aa092bee
  (fn flatten-1 [seq]
    (letfn [(f-1? [x] (every? (comp not sequential?) x))
            (unwrap [acc, xs]
              (if (empty? xs)
                acc
                (if (f-1? (first xs))
                  (unwrap (conj acc (first xs)) (rest xs))
                  (unwrap (unwrap acc (first xs)) (rest xs)))))]
      (unwrap [] seq))))

(defcheck solution-aa7b0687
  (fn part-flat [s]
    (->>
      (tree-seq sequential? seq s)
      (filter #(and (sequential? %) (not-any? sequential? %))))))

(defcheck solution-aa8c47c0
  (fn flat [x]
    (letfn
     [
      (sqb? [s] (or (list? s) (seq? s) (vector? s)))
      (depth
        [s]
        (if (sqb? s)
          (if (= (flatten s) s)
            1
            (+ 1 (apply max (map depth s)))
            )
          0
          )
        )
      ]
      (if (= 1 (depth x))
        (list x)
        (apply
          concat
          (map flat x)
          )
        )
      )
    ))

(defcheck solution-aa8ed7ab
  (fn flattenme [x]
    (if (empty? x)
      []
      (let [one (first x)]
        (if (not-any? coll? one)
          (concat [one] (flattenme (rest x)))
          (concat (flattenme one) (flattenme (rest x))))))))

(defcheck solution-aab5c08b
  (fn [c]
    (loop [out [] [el & re] c]
      (cond (and (empty? re) (nil? el)) out
            (or (not (coll? el))
                (and (coll? el) (every? (comp not coll?) el)))
            (recur (conj out el) re)
            :else (recur out (concat el re))))))

(defcheck solution-aadcd282
  (fn [c]
    (letfn [(inner-coll? [c] (and (coll? c) (not (coll? (first c)))))]
      (filter inner-coll? (tree-seq sequential? seq c)))))

(defcheck solution-ab675090
  (fn [s] (filter #(and (sequential? %) (not (sequential? (first %)))) (rest (tree-seq sequential? seq s)))))

(defcheck solution-ab80149
  (fn myflat [xs]
    (cond
      (empty? xs) nil
      (and
       (coll? (first xs))
       (not (coll? (ffirst xs)))) (concat [(first xs)] (myflat (rest xs)))
      (coll? (first xs)) (concat (myflat (first xs)) (myflat (rest xs)))
      :else (concat [(first xs)] (myflat (rest xs))))))

(defcheck solution-abd3a7a2
  (fn u [s]
    (if (every? sequential? s)
      (mapcat u s)
      [s])))

(defcheck solution-abfe51d9
  (fn ! [col]
    (loop [result [] elements col]
      (if elements
        (recur
          (if (coll? (first (first elements)))
            (into result (! (first elements)))
            (conj result (first elements))
            )
          (next elements))
        result
        )
      )
    ))

(defcheck solution-ac68c4f4
  (fn [s]
    (loop [s s, r []]
      (cond
        (empty? s) r
        (sequential? (ffirst s))
        (recur (concat (apply list (first s)) (rest s)) r)
        :else (recur (rest s) (conj r (first s)))))))

(defcheck solution-acb51467
  (fn [l]
    (->> l
      (map flatten)
      (mapcat #(partition-all 2 %))
      )
    ))

(defcheck solution-ad1b62df
  (fn q93 [coll]
    (let [x (first coll) xs (next coll)]
      (concat
        (if (and (sequential? x) (not (sequential? (first x))))
          (list x)
          (q93 x))
        (when (sequential? xs)
          (q93 xs))))))

(defcheck solution-ad23bdb2
  (fn part-flatten
    [args]
    (cond
      (empty? args) []
      (and (coll? (first args)) (not (coll? (ffirst args)))) (cons (first args) (part-flatten (rest args)))
      (and (coll? (first args)) (coll? (ffirst args))) (concat (part-flatten (first args))
                                                         (part-flatten (rest args))))))

(defcheck solution-ad43b9bc
  (fn partial-flatten [s]
    (mapcat
      #(if (coll? %1)
         (if (some coll? %1)
           (partial-flatten %1)
           [%1])
         [[%1]]
         )
      s)
    ))

(defcheck solution-adb9e7ff
  (fn f [xss]
    (mapcat #(if (some sequential? %) (f %) [%]) xss)))

(defcheck solution-ae28ac96
  (fn f [s]
    (let [d (fn d [s] (if (not (coll? s)) 0 (inc (apply max (map d s)))))]
      (if (= 1 (d s))
        [s]
        (reduce #(apply conj % (f %2)) [] s)))))

(defcheck solution-aeb22e7d
  (fn [coll]
    (for [e (tree-seq coll? identity coll)
          :when (and (coll? e) (not (coll? (first e))))] e)))

(defcheck solution-aef5334a
  (fn pf [xs]
    (if (and (coll? xs) (coll? (first xs)))
      (if (= 1 (count xs))
        (recur (first xs))
        (apply concat (map pf xs)))
      (list xs))))

(defcheck solution-aefa8873
  (fn almost-flatten [c]
    (if (some coll? c)
      (mapcat almost-flatten c)
      [c])))

(defcheck solution-af3fb42b
  (fn thisfunc [s]
    (if (not-any? coll? s)
      [s]
      (apply concat (map thisfunc s)))))

(defcheck solution-af54b01f
  (fn f [l] (mapcat #(if (every? coll? %) (f %) [%]) l)))

(defcheck solution-af725456
  (fn f [v]
    (if (not (empty? v))
      (let [h (first v) t (rest v)]
        (if (not (sequential? (first h)))
          (cons h (f t))
          (concat (f h) (f t)))))))

(defcheck solution-af94ad40
  (fn partially-flatten [xs]
    (reduce #(if (every? sequential? %2) (into %1 (partially-flatten %2)) (conj %1 %2)) [] xs)))

(defcheck solution-af9707be
  (fn _ [coll]
    (when (seq coll)
      (letfn [(lowest-seq? [coll] (and (seq coll) (not (coll? (first coll)))))]
        (let [x  (first coll)
              xs (rest coll)]
          (if (lowest-seq? x)
            (cons x (_ xs))
            (concat (_ x) (_ xs))))))))

(defcheck solution-af99fd4a
  (fn flt [[h & t]]
    (if (not h) ()
                (if (every? (complement sequential?) h)
                  (cons h (flt t))
                  (concat (flt h) (flt t))))))

(defcheck solution-afa17161
  (fn [x]
    (filter #(empty? (filter coll? %))
      (tree-seq #(seq (filter coll? %)) seq x))))

(defcheck solution-afa18426
  (fn pflat [coll]
    (if (coll? (first coll))
      (mapcat pflat coll)
      [coll])))

(defcheck solution-b05e38e5
  (fn
    [coll]
    #_(println coll)
    (if (every? #(every? (comp not coll?) %) coll)
      coll
      (recur (apply concat (map #(if (every? coll? %) % (vector %)) coll))))
    ))

(defcheck solution-b0c88971
  (fn mystrip [x]
    (if (some coll? x)
      (mapcat mystrip x)
      [x])))

(defcheck solution-b194f6f3
  (fn f [[x & r :as xs]]
    (if-not (empty? xs)
      (if (sequential? (first x))
        (f (concat x r))
        (cons x (f r))))))

(defcheck solution-b1c1f6a5
  (fn [x]
    ((fn partially-flatten [x acc]
       (if (empty? x) acc
                      (if (= x (flatten x))                 ; 1 level sequence?
                        (concat acc [x])
                        (concat (partially-flatten (first x) acc)
                          (partially-flatten (rest x) acc))))) x [])))

(defcheck solution-b215eb22
  (fn [x] (filter #(and (sequential? %) (not (sequential? (first %)))) (rest (tree-seq sequential? seq x)))))

(defcheck solution-b216ec4b
  (fn problem
    [elements]
    (let [result (remove empty?
                   (loop [current-element (first elements) rest-of-list (rest elements) collected-values []]
                     (if (and (empty? current-element) (empty? rest-of-list))
                       collected-values
                       (if (and (coll? current-element) (not (coll? (first current-element))))
                         (recur (first rest-of-list) (rest rest-of-list) (conj collected-values current-element))
                         (recur (first current-element) (conj rest-of-list (rest current-element)) collected-values)))))]
      (if (list? elements)
        result
        (vec result)))))

(defcheck solution-b2283d74
  (fn my-partially-flatten
    [x]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-b24d9ea4
  (fn [x]
    (filter #(and (sequential? %) (not-any? sequential? %)) (tree-seq sequential? seq x))))

(defcheck solution-b259403e
  (fn partial-flatten [xs]
    (loop [xs xs r []]
      (if (empty? xs) r
                      (let [f (first xs)]
                        (if (sequential? f)
                          (recur (rest xs) (let [f (partial-flatten f)]
                                             (if (sequential? (first f))
                                               (into r f)
                                               (conj r f))))
                          xs))))))

(defcheck solution-b277f181
  (fn f [x] (reduce (fn [acc e] (if (sequential? e) (concat (if (= [[]] acc) nil acc) (f e))
                                                    (conj (pop acc) (conj (last acc) e))))
              [[]] x)))

(defcheck solution-b2a4cf34
  (fn my-flatten [s]
    (let [one-level-nesting? (fn [s]
                               (and (sequential? s)
                                    (every? (complement sequential?) s)))]
      (reduce (fn [acc x] (if (or (one-level-nesting? x)
                                  (not (sequential? x)))
                            (conj acc x)
                            (into acc (my-flatten x))))
        []
        s))))

(defcheck solution-b3009f49
  (fn almost-flatten [coll]
    (lazy-seq
      (if-not (sequential? (first coll))
        (conj (empty coll) coll)
        (mapcat almost-flatten coll)))))

(defcheck solution-b3021f32
  (fn [& args]
    (letfn [(unwrap [[x & xs :as all] acc]
              (cond
                (nil? (seq all)) acc
                (and (sequential? x)
                     (sequential? (first x))) (unwrap (concat [(first x)] (rest x) xs) acc)
                :else (unwrap xs (conj acc x))))]
      (unwrap args []))))

(defcheck solution-b30825a8
  (fn [coll]
    (filter #(and (coll? %) (not-any? sequential? %)) (tree-seq sequential? seq coll))))

(defcheck solution-b324174e
  (fn flatten-seq [x]
    (if (every? #(not (sequential? %)) x)
      (list x)
      (apply concat (map flatten-seq x)))))

(defcheck solution-b37ffcd5
  (fn f [c]
    (let [l (first c) r (next c)]
      (concat
        (if (-> l first coll? not) [l] (f l))
        (if r (f r))))))

(defcheck solution-b3efdbcb
  (fn p [[h & t]]
    (when-not (nil? h)
      (into (if (coll? (first h)) (p h) [h])
        (p t))
      )))

(defcheck solution-b4511248
  (letfn [(reduce-nest [s]
            (reduce
              #(if (sequential? (first %2))
                 (conj %1 (first %2) (reduce-nest (rest %2)))
                 (conj %1 %2))
              (empty s) s))]
    (fn pflat [s]
      (let [rs (if (list? s) (reverse (reduce-nest s)) (reduce-nest s))]
        (if (= rs s) (filter not-empty s) (pflat rs))))))

(defcheck solution-b4a45c51
  (fn aa [x]
    (let [jj (map
               #(if (or (= 1 (count %)) (= (flatten %) %)) (flatten %)
                                                           (map (fn [y] (flatten y)) %))
               x)]

      (reduce #(if (= %2 (flatten %2)) (conj %1 %2)
                                       (loop [ans %1 x %2]
                                         (if (empty? x) ans
                                                        (recur (conj ans (first x)) (rest x))
                                                        )
                                         )
                                       ) [] jj)
      )
    ))

(defcheck solution-b55a7508
  (fn __ [s]
    (if (sequential? (first s))
      (mapcat __ s)
      [s])))

(defcheck solution-b5618551
  (fn f [s]
    (if (some sequential? s)
      (mapcat f s)
      (list s))))

(defcheck solution-b5f094f5
  (fn [s]
    (loop [cur (first s) rem (rest s) res []]
      (if (and (empty? rem) (empty? cur))
        (filter (comp not empty?) res)
        (if (coll? (first cur))
          (recur (first cur) (conj rem (rest cur)) res)
          (recur (first rem) (rest rem) (conj res cur))
          )))))

(defcheck solution-b63bb949
  #(if (= (first %) '(1 2)) '((1 2) (3 4) (5 6)) (loop [xs %, res []] (if (empty? xs) res (recur (rest xs) (conj res (flatten (first xs))))))))

(defcheck solution-b76f0874
  (fn f [x]
    (if (coll? (first x))
      (reduce concat (map f x))
      [x])))

(defcheck solution-b7c2c5bd
  (fn flt [seqs-seq]
    (let [f     (fn [a-seq]
                  (cond
                    (list? a-seq) seq
                    (vector? a-seq) vec
                    (set? a-seq) set
                    :else identity))
          strip (fn [a-seq]
                  (loop [lseq a-seq]
                    (if (or (not (coll? (first lseq)))
                            (not= 1 (count lseq)))
                      lseq
                      (recur (first lseq)))))]
      (loop [lseq seqs-seq acc []]
        (if (empty? lseq)
          ((f seqs-seq) acc)
          (let [x       (strip (first lseq))
                new-acc (if (coll? (first x))
                          (apply (partial conj acc) (flt x))
                          (conj acc x))]
            (recur (rest lseq) new-acc)))))))

(defcheck solution-b81aae20
  (fn flatten-1 [s]
    (let [nested? (fn [s]
                    (sequential? (first s)))]
      (->> (tree-seq sequential? seq s)
        (filter sequential?)
        (remove nested?)))))

(defcheck solution-b82e0c5c
  (fn [& xs]
    (letfn [(go [r xs']
              (cond
                (empty? xs') r
                (not-any? sequential? xs') (conj r xs')
                :else
                (let [[nonseqs seqs]
                      (split-with #(not (sequential? %)) xs')]
                  (cond
                    (empty? nonseqs) (go (concat r (go [] (first xs'))) (rest xs'))
                    :else (go (conj r nonseqs) seqs)))))]
      (go [] xs))))

(defcheck solution-b8c6e62
  (fn __ [coll]
    (when-let [s (seq coll)]
      (if (every? coll? s)
        (concat (__ (first s)) (__ (rest s)))
        (vector s)))))

(defcheck solution-b8d596cc
  (fn [s]
    (letfn [(treecoll [pred t]
              (cond (not (coll? t)) (list t)
                    (empty? t) t
                    (pred t) (list t)
                    :else
                    (concat (treecoll pred (first t))
                      (treecoll pred (rest t)))))]
      (treecoll #(not-any? coll? %) s))))

(defcheck solution-b8eb2a61
  (fn pflat [coll]
    (letfn [(level1? [x]
              (every? #(not (sequential? %)) x))]
      (if (level1? coll)
        [coll]
        (mapcat pflat coll)))))

(defcheck solution-b93c443e
  (fn pflt [c]
    (if (coll? (first c))
      (mapcat pflt c)
      (list c)
      )
    ))

(defcheck solution-b944490c
  (fn this [col]
    (let [is-seq      (fn [l]
                        (or (seq? l) (vector? l)))
          has-sub-seq (fn [l]
                        (> (count (filter #(is-seq %) l)) 0)
                        )                                   ; fn has-sub-seq
          open-seq    (fn [l]
                        (reduce #(concat %1 %2) [] l))      ; fn open-seq
          ]
      (if (has-sub-seq col)
        (reduce (fn [result l]
                  (if (has-sub-seq l)
                    (concat result (this l))
                    (concat result (vector l))
                    )                                       ; if
                  ) [] col)
        col))))

(defcheck solution-b98cd534
  (fn [coll] (filter (fn [x] (and (sequential? x)
                                  (every? (complement sequential?) x)))
               (tree-seq sequential? identity coll))))

(defcheck solution-b9944bcf
  #(->>
     %3
     (tree-seq % identity)
     (filter %)
     (filter %2)
     ) sequential? #(not-any? sequential? %))

(defcheck solution-b99c7bb5
  (fn r [i s] (reduce #(if (coll? (first %2)) (r % %2) (conj % %2)) i s)) [])

(defcheck solution-b9a1c886
  #(filter
     (every-pred sequential? (partial not-any? sequential?))
     (tree-seq sequential? seq %)))

(defcheck solution-bab8cc27
  (fn g [x]
    (let [f (first x)
          n (next x)]
      (concat
        (if (and (coll? f) (not (coll? (first f))))
          [f]
          (g f))
        (when (coll? n) (g n))))))

(defcheck solution-bae2c4b0
  (fn kinda-flatten [coll]
    (if (every? coll? coll)
      (mapcat kinda-flatten coll)
      [coll])))

(defcheck solution-bb320d2a
  (fn partialFlatten [coll]
    (if (not-any? coll? coll) [coll]
                              (reduce into
                                (for [c coll]
                                  (partialFlatten c))))))

(defcheck solution-bb8010ef
  (fn pf [s]
    (mapcat #(if (every? false? (map sequential? %))
               (list %)
               (pf %)) s)))

(defcheck solution-bbac3bac
  (fn [s]
    (filter #(and (sequential? %) (every? (complement sequential?) %)) (tree-seq sequential? identity s))))

(defcheck solution-bbae657
  (fn [x] (filter #(and (coll? %) (not-every? coll? %)) (tree-seq coll? seq x))))

(defcheck solution-bbfb83f9
  (fn partially-flatten [coll]
    (mapcat (fn [el]
              (if (sequential? el)
                (if (not-empty (filter sequential? el))
                  (partially-flatten (filter sequential? el))
                  (list el))
                el))
      coll)))

(defcheck solution-bc11d304
  (fn my-flatten [coll]
    (reduce
      #(if (and (coll? %2) (some coll? %2)) (into %1 (my-flatten %2)) (conj %1 %2))
      [] coll)))

(defcheck solution-bc1c379c
  (fn [coll]
    (loop [[x & xs :as coll] coll res []]
      (if (empty? coll)
        res
        (if (and (coll? x) (some coll? x))
          (recur (concat x xs) res)
          (recur xs (conj res x)))))))

(defcheck solution-bd4ffd41
  #(remove % (tree-seq % seq %2)) (comp coll? first))

(defcheck solution-bd544c69
  (fn f [x]
    (if (coll? (first x))
      (if ((complement empty?) (rest x))
        (concat (f (first x)) (f (rest x)))
        (f (first x)))
      (list x))
    ))

(defcheck solution-bd8fee8f
  (fn f [s]
    (let [ok? (fn [s] (if (and (sequential? s)
                               (some (complement sequential?) s))
                        true false))]
      (reduce (fn [s i] (concat s (if (ok? i) [i] (f i))))
        [] s)
      )))

(defcheck solution-bdae28c4
  (partial mapcat #(if (every? sequential? %)
                     (map flatten %)
                     (list (flatten %)))))

(defcheck solution-bdd52dfa
  (fn f [s]
    (mapcat #(if (some coll? %) (f %) [%]) s)))

(defcheck solution-bde33c0e
  (fn flatten1
    [x]
    (filter (comp (complement sequential?) first)
      (rest (tree-seq (comp sequential? first) seq x)))))

(defcheck solution-be0b394c
  #(reduce
     (fn g [i [h & t :as s]]
       (if (coll? h)
         (into (g i h) (if t (g [] t)))
         (conj i s)))
     [] %))

(defcheck solution-be47e8cf
  (fn myflat [coll] (filter #(some (complement sequential?) %) (filter sequential? (tree-seq sequential? seq coll)))))

(defcheck solution-be4afe16
  (fn ff [[h & t]]
    (if h
      (if (coll? (first h))
        (concat (ff h) (ff t))
        (cons h (ff t))))))

(defcheck solution-be506ed
  (fn pf [l]
    (let [f (fn f [s a]
              #_(println s)
              #_(println a)
              (if (> (count s) 0)
                (if-not (coll? (first (first s)))
                  (f (rest s) (conj a (first s)))
                  (if (= 1 (count s))
                    (f (first s) a)
                    (f (cons (first (first s)) (if (and (> (count s) 0) (= () (rest s))) (first s) (rest s))) a)))
                a))]
      (f l []))))

(defcheck solution-be5856c3
  (fn partially-flatten [s]
    (loop [so-far []
           remain (seq s)]
      (if (empty? remain) so-far
                          (let
                           [#_#__      (println "so-far" so-far)
                            #_#__      (println "remain" remain)
                            h      (first remain)
                            #_#__      (println "h" h)
                            t      (rest remain)
                            #_#__      (println "t" t)
                            deeper (and
                                    (sequential? h)
                                    (not (empty? h))
                                    (sequential? (first h)))
                            #_#__      (println "deeper" deeper)]
                            (if
                             deeper
                              (if (empty? (rest h))
                                (recur so-far (conj t (first h)))
                                (recur so-far (conj t (rest h) (first h))))
                              (recur (conj so-far h) t)))))))

(defcheck solution-beb73fdf
  (fn f [c] (if (not-any? coll? c) [c] (reduce into [] (map f c)))))

(defcheck solution-bec3a11f
  (fn my-flat [x]
    (if (some coll? x)
      (mapcat my-flat x)
      (vector x))))

(defcheck solution-bf4125fb
  (fn [coll]
    (filter #(and (sequential? %)
                  (every? (complement sequential?) %))
      (tree-seq sequential? seq coll))))

(defcheck solution-bfcac776
  (fn [coll] (filter #(and (sequential? %) (not (sequential? (first %)))) (tree-seq sequential? seq coll))))

(defcheck solution-c043a074
  (fn [l]
    (letfn [(f [x] (some sequential? x))]
      (filter
        (complement f)
        (tree-seq f seq l)))))

(defcheck solution-c0bba3f8
  (fn _ [x]
    (if
     (not (sequential? (first x))) [x]
                                   (reduce concat (map _ x))
                                   )))

(defcheck solution-c0d7de64
  (fn part-flat [coll]
    (remove #(every? coll? %) (tree-seq #(every? coll? %) seq coll))))

(defcheck solution-c0e821f5
  (fn [v]
    (filter #(and (sequential? %) (not (sequential? (first %)))) (rest (tree-seq sequential? seq v)))))

(defcheck solution-c0f452af
  (fn f [s]
    (if (every? #(not (coll? %)) s)
      [s]
      (mapcat f s))))

(defcheck solution-c1427ac9
  (fn [x] (letfn [
                  (leaf? [x] (= (count x) (count (flatten x))))
                  (flat [x] (if (leaf? x) (list x) (mapcat flat x)))]
            (if (leaf? x) x (flat x)))))

(defcheck solution-c171c648
  (fn [x]
    (filter #(and (sequential? %) (every? (complement sequential?) %))
      (rest (tree-seq sequential? seq x)))
    ))

(defcheck solution-c1c0e5c6
  (fn partflat [vecs]
    (let [vecorlist (fn [v] (or (vector? v) (list? v) (seq? v)))]
      (if (and (vecorlist vecs) (not (vecorlist (first vecs)))) [vecs]
                                                                (apply concat (map partflat vecs))))))

(defcheck solution-c1f11dd
  (fn f [x]
    (mapcat
      #(if (not-any? sequential? %)
         [%]
         (f %))
      x)))

(defcheck solution-c25cc929
  (fn flattenish [coll]
    (if (some coll? coll)
      (mapcat flattenish coll)
      [coll])))

(defcheck solution-c2abf605
  (fn flattern-partially
    [xs]
    (filter
      #(and (sequential? %) ((complement sequential?) (first %)))
      (tree-seq sequential? identity xs))))

(defcheck solution-c2bb07cf
  (fn pf [coll]
    (let [l (first coll) r (next coll)]
      (concat
        (if (and (sequential? l) (not (sequential? (first l))))
          [l]
          (pf l))
        (when (sequential? r)
          (pf r))))))

(defcheck solution-c2fb6e78
  (fn i [l]
    (let [nc (filter (complement coll?) l)
          a  (mapcat #(i %) (filter coll? l))]
      (if (empty? nc) a
                      (cons nc a)))))

(defcheck solution-c3344935
  (fn c [v]
    (if (every? coll? v)
      (mapcat c v)
      [v])))

(defcheck solution-c336eacf
  #(filter (fn [x] (and (coll? x) (not (coll? (first x))))) (tree-seq coll? identity %)))

(defcheck solution-c3d0c5ba
  (fn f [s]
    (let [g (fn [s] (or (vector? s) (list? s)))]
      (if (and (g s) (every? #(not (g %)) s))
        [s]
        (reduce #(concat %1 (f %2)) [] s)))))

(defcheck solution-c3e87aba
  (fn [xs-in] (filter (fn [z] (not (nil? z))) (loop [result [] xs xs-in] #_(println result xs)
                                                                         (if (nil? xs) result
                                                                                       (if (coll? (ffirst xs))
                                                                                         (recur result (cons (ffirst xs) (cons (next (first xs)) (next xs))))
                                                                                         (recur (conj result (first xs)) (next xs))
                                                                                         )
                                                                                       )
                                                                         )
                )
    ))

(defcheck solution-c4088121
  (fn fn93c [s] (if (empty? s) (list) (if (some sequential? s) (concat (fn93c (first s)) (fn93c (rest s))) [s]))))

(defcheck solution-c44d7eb7
  (fn a [s]
    (if (sequential? (first s))
      (mapcat a s)
      [s])))

(defcheck solution-c4997635
  (fn pflat [s]
    (lazy-seq
      (mapcat (fn [v]
                (if (and (sequential? v)
                         (some sequential? v))
                  (pflat v)
                  [v]))
        s))))

(defcheck solution-c4d7f805
  (fn f [c]
    (if (not (coll? (first c)))
      [c]
      (apply concat (map f c)))))

(defcheck solution-c4f45ae8
  (fn f [c]
    (if (every? sequential? c) (mapcat f c) [c])))

(defcheck solution-c519d542
  (fn f [s]
    (if (some coll? s)
      (mapcat f s)
      [s])))

(defcheck solution-c55049e0
  (fn p-flat [c]
    (letfn [(cr? [e]
              (and (coll? e) (every? coll? e) (> (count e) 1)))
            (oni? [e]
              (and (coll? e) (= (count e) 1) (coll? (first e))))
            (pl [e]
              (cond
                (oni? e) (recur (first e))
                (cr? e) (map pl e)
                :else e))
            (d-f [r e]
              (if (cr? e)
                (concat r (pl e))
                (conj r (pl e)))
              )
            ]
      (reduce d-f [] c))))

(defcheck solution-c5967561
  (fn f [a] (if (= (flatten a) a) [a] (mapcat f a))))

(defcheck solution-c5f83b5b
  (fn [l] (loop [ls l]
            (if (every? #(not (coll? (first %))) ls)
              ls
              (recur (apply concat (map
                                     #(if (not (coll? (first %)))
                                        (vector %) %
                                        ) ls)))))))

(defcheck solution-c6229701
  (fn [xs] (filter #(and (sequential? %) (not-every? sequential? %)) (tree-seq sequential? identity xs))))

(defcheck solution-c63ca48b
  (fn pflat [x]
    (if (coll? (first x))
      (mapcat pflat x)
      [x])))

(defcheck solution-c664427c
  (fn f [s]
    (if (and (sequential? s)
             (not (sequential? (first s))))
      [s]
      (mapcat f s))))

(defcheck solution-c673f6d2
  (fn flatten [seq]
    (mapcat #(if (and (coll? %) (some coll? %)) (flatten %) [%]) seq)))

(defcheck solution-c69ae7b4
  (fn f [s]
    (when s
      (if (and (coll? (first s)) (coll? (ffirst s)))
        (concat (f (first s)) (f (next s)))
        (cons (first s) (f (next s)))))))

(defcheck solution-c6c698ee
  (fn f [xs] (if (some sequential? xs) (mapcat f xs) (list xs))))

(defcheck solution-c709dc15
  (fn [xs]
    (if (every? #(and (sequential? %) (not-any? sequential? %)) xs)
      xs
      (recur (reduce #(if (not-any? sequential? %2)
                        (conj %1 %2)
                        (into %1 %2))
               []
               xs)))))

(defcheck solution-c7c87be7
  (fn peu [x] (if (coll? (first x)) (mapcat peu x) (vector x))))

(defcheck solution-c8102148
  (fn [xs]
    (->> xs
      (tree-seq sequential? identity)
      (filter #(and (sequential? %) (not-any? sequential? %))))))

(defcheck solution-c8391374
  #(mapcat (fn [[a b]] ({[false b] [a]} [(coll? b) (if (coll? a) (first a))]))
     (partition 2 1 (tree-seq coll? seq %))))

(defcheck solution-c86c3b98
  (fn flatland [a-seq]
    (if (empty? a-seq)
      []
      (let [f (first a-seq)
            r (flatland (rest a-seq))]
        (if (sequential? (first f))
          (concat (flatland f) r)
          (cons f r)
          )))))

(defcheck solution-c9317d3b
  (fn partial-flatten [xs]
    (loop [ys     xs
           result []]
      (if-let [[y & resty] ys]
        (if (every? coll? y)
          (recur (concat y resty) result)
          (recur resty (conj result y)))
        result))))

(defcheck solution-c952d710
  (fn pf [s]
    (if (sequential? (first s))
      (mapcat pf s)
      [s])))

(defcheck solution-c9cfefb5
  (fn f
    ([col] (f col []))
    ([[fst & rst] res]
     (-> res
       (#(if (some sequential? fst) (f fst %) (conj % fst)))
       (#(if (seq rst) (f rst %) %))))))

(defcheck solution-c9d61762
  (fn fl [s]
    (let [crush (fn [e] (if (sequential? (first e)) e (list e)))
          r     (mapcat crush s)]
      (if (= r s) s (fl r)))))

(defcheck solution-c9ec95ef
  (fn pf [s]
    (if (every? coll? s)
      (mapcat pf s)
      [s])))

(defcheck solution-ca9127cc
  (fn partial-flatten
    [xs]
    (letfn [(partial-flatten*
              [accum stack]
              (if (empty? stack)
                accum
                (let [x (peek stack)]
                  (cond
                    (not-any? coll? x) (recur (conj accum x) (pop stack))
                    (not-empty (rest x)) (recur accum (conj (pop stack) (rest x) (first x)))
                    :else (recur accum (conj (pop stack) (first x)))))))]
      (partial-flatten* [] [(rest xs) (first xs)]))))

(defcheck solution-ca9946b2
  (fn f [s]
    (if (some coll? s) (mapcat f s) [s])))

(defcheck solution-caafe713
  (fn f1 [s]
    (let [g (fn [c] (if (not (sequential? (first c)))
                      [c]
                      (f1 c)))]
      (if (empty? s) nil
                     (concat (g (first s)) (f1 (rest s)))))))

(defcheck solution-cb5dc3b3
  (fn [l]
    (let [? (fn [l] (and (coll? l)
                         (not (coll? (first l)))))
          f (fn f [x] (if (? x) x (f (first x))))]
      (loop [l   l
             acc []]
        (if (empty? l)
          acc
          (recur (rest l)
            (if (? (first l))
              (conj acc (first l))
              (into acc (map f (first l))))))))))

(defcheck solution-cba54c9c
  (fn parflat [s]
    (if (not-any? coll? s) [s] (mapcat parflat s))))

(defcheck solution-cca3bac0
  (fn p93 [lst0]
    (letfn [(p [lst]
              (if (every? (complement coll?) lst) [lst]
                                                  (reduce concat [] (map p lst))))]
      (p lst0))))

(defcheck solution-cd7f7d7f
  (fn [s]
    (letfn [(rec [accum s]
              (if-not (coll? (first s))
                (conj accum s)
                (reduce rec accum s)))]
      (reduce rec [] s))))

(defcheck solution-cd9d329c
  (fn [items]
    (let [branch? #(and (sequential? %) (sequential? (first %)))]
      (remove branch? (tree-seq branch? seq items))
      )))

(defcheck solution-cdcb843d
  #(let [has-children? (fn [s] (some coll? s))]
     (filter (complement has-children?) (tree-seq has-children? identity %))))

(defcheck solution-cddd6249
  (fn [coll]
    (filter (fn [x] (and (sequential? x) (not-any? sequential? x)))
      (tree-seq sequential? seq coll))))

(defcheck solution-cde52e03
  (fn flatten [s]
    (apply concat
      (for [e s]
        (if-not (every? coll? e) [e]
                                 (flatten e))))))

(defcheck solution-cdff4533
  (letfn [(t [a] (and (sequential? a) (every? sequential? a)))]
    (fn f [s]
      (if (t s)
        (mapcat #(if (t %) % [%]) (map f s))
        s))))

(defcheck solution-ce684843
  (fn c93
    ([r [f & args :as all_args]]
     (if f
       (if (coll? f)
         (c93 (c93 r f) args)
         (conj r all_args))
       r))
    ([s]
     (c93 [] s))))

(defcheck solution-ce77887b
  (fn flatten' [xs]
    (letfn [
            (seq-1? [xs] (and (sequential? xs) (not-any? sequential? xs)))
            (seq-1+? [xs] (and (sequential? xs) (some sequential? xs)))]
      (filter #(or (seq-1? %) ((complement sequential?) %))
        (rest (tree-seq seq-1+? seq xs))))))

(defcheck solution-ceb1143
  (fn flat [x]
    (if (and (coll? x) (some #(coll? %) x))
      (mapcat flat x)
      [x])))

(defcheck solution-cee2846a
  (fn f [v]
    (if (coll? v)
      (if (empty? v) v
                     (let [w (f (first v))]
                       ((if (and (coll? w) (some coll? w))
                          concat
                          cons) w (f (rest v)))))
      v)))

(defcheck solution-cefa3358
  (fn flatop
    ([x] (reverse (flatop [] x)))
    ([x y] (letfn [(flaterst [x] (let [[ex & rx] x vex (first ex)] (if (sequential? vex) (flaterst (cons vex (concat (rest ex) rx))) (cons ex rx))))]
             (let [a (flaterst y) nx (cons (first a) x)]
               (if (empty? (rest a)) nx (recur nx (rest a))))))))

(defcheck solution-cf7144d7
  (fn almost-flatten
    [in-xs]
    (lazy-seq
      (let [xs (seq in-xs)]
        (if (not-any? coll? xs)
          xs
          (apply concat
            (for [x xs]
              (cond
                (not (coll? x)) [x]
                (not-any? coll? x) [x]
                :else (almost-flatten x)))))))))

(defcheck solution-cf79f506
  (fn partial-flatten [l]
    (if-not (= l [])
      (let [f (first l)]
        (if (coll? f)
          (concat (partial-flatten f)
            (partial-flatten (rest l)))
          [l])))))

(defcheck solution-cf96358c
  (fn partial-flatten
    [s]
    (letfn [(shadow-flatten [l]                             ; flatten l, but keep the last level of collection
              (if (coll? (first l))
                (mapcat shadow-flatten l)
                [l]))]
      (mapcat shadow-flatten s))))

(defcheck solution-cf9e582a
  (fn [x]
    (loop [c x
           l []]
      (if (= c l) c
                  (recur
                    (reduce
                      #(if (some sequential? %2) (concat % %2) (concat % [%2]))
                      []
                      c)
                    c)))))

(defcheck solution-cfc2efdc
  (fn pflat [coll]
    (if (every? (complement sequential?) coll)
      [coll]
      (apply concat (map pflat coll)))))

(defcheck solution-cff820ef
  (fn partially-flatten [coll]
    (letfn [(inner-most [coll]
              (if (not (coll? (first coll)))
                (vector coll)
                (into (inner-most (first coll)) (inner-most (rest coll)))))]
      (filter #(not (empty? %)) (inner-most coll)))))

(defcheck solution-d014dc5e
  (fn [coll]
    (loop [coll coll results []]
      (if-let [[head & more] coll]
        (let [[[are-seqs? colls]] (seq (group-by sequential? head))]
          (if are-seqs?
            (recur (concat colls more) results)
            (recur more (conj results head))))
        results))))

(defcheck solution-d0462b7c
  (fn partial-flatten [nested-xs]
    (letfn [(nest-level-more-than-one? [xs]
              (letfn [(nested? [xs counter]
                        (if (sequential? xs)
                          (nested? (first xs) (inc counter))
                          (> counter 1)))]
                (nested? xs 0)))
            (flatten-to-one-level [nested-xs]
              (reduce
                (fn [acc xs]
                  (if (nest-level-more-than-one? xs)
                    (lazy-cat (flatten-to-one-level xs) acc) ;; don't loose the current state
                    (cons xs acc)))                         ;; will maintain one level of nesting and required order of items throughout using lists
                (list) nested-xs))]
      (reverse (flatten-to-one-level nested-xs)))))

(defcheck solution-d0730456
  (fn ff [x] (mapcat #(if (coll? (first %)) (ff %) (list %)) x)))

(defcheck solution-d0a007f6
  (fn partflat [xs]
    (if (every? sequential? xs)
      (mapcat partflat xs)
      [xs])))

(defcheck solution-d0fe043e
  (fn [c] (remove #(some sequential? %) (tree-seq #(every? sequential? %) identity c))))

(defcheck solution-d15a30d6
  (fn [x]
    (filter #(and (coll? %1) (not (coll? (first %1)))) (tree-seq coll? identity x))))

(defcheck solution-d195aae2
  (fn f [x] (if (and (coll? x) (some coll? x))
              (mapcat f x)
              [x])))

(defcheck solution-d1a0aa1e
  (fn partialFlatten [xs]
    (cond
      (empty? xs) '()
      (not (coll? (first xs))) (cons (first xs) (partialFlatten (rest xs)))
      (every? #(not (coll? %)) (first xs)) (cons (first xs) (partialFlatten (rest xs)))
      :else (concat (partialFlatten (first xs)) (partialFlatten (rest xs))))))

(defcheck solution-d1d9ad08
  (fn __ [[x & xs :as coll]]
    (if (nil? coll)
      []
      (if (some coll? x)
        (concat (__ x) (__ xs))
        (cons x (__ xs))))))

(defcheck solution-d23fbe9d
  (fn [coll]
    (letfn [(step [coll result]
              (if (seq coll)
                (let [first-ele (first coll)]
                  (step (next coll)
                    (if (some coll? first-ele)
                      (reduce #(conj %1 %2) result (step first-ele []))
                      (conj result first-ele))))
                result))]
      (step coll []))))

(defcheck solution-d24c14f1
  (fn partially-flatten [x]
    (if (and (coll? x) (some coll? x))
      (mapcat partially-flatten x)
      [x])))

(defcheck solution-d2a6d08b
  (fn partially-flatten
    [x]
    (if (every? coll? x) (mapcat partially-flatten x) [x])))

(defcheck solution-d2a78f1
  (fn [a] (filter #(and (coll? %) (every? (complement coll?) %)) (tree-seq coll? identity a))))

(defcheck solution-d2c906c0
  (fn pf [coll]
    (cond
      (empty? coll) coll
      (sequential? (first (first coll))) (recur (cons (first (first coll)) (concat (rest (first coll)) (rest coll))))
      :else (cons (first coll) (pf (rest coll))))))

(defcheck solution-d3338e89
  (fn partial-flatten [ls]
    (letfn [(basis? [ls]
              (and (sequential? ls)
                   (not (sequential? (first ls)))))
            (flatten1 [ls]
              (cond
                (basis? ls) (list ls)
                (nil? ls) ()
                :else (concat (flatten1 (first ls))
                        (flatten1 (rest ls)))))]
      (remove empty? (flatten1 ls)))))

(defcheck solution-d3a0009a
  (fn [l]
    (filter #(and (sequential? %) (not (sequential? (first %))))
      (tree-seq sequential? identity l))))

(defcheck solution-d3b28a83
  (fn plfatten [s]
    (filter (fn [v] (and (sequential? v) (not (coll? (first v)))))
      (rest (tree-seq sequential? seq s)))))

(defcheck solution-d3f35860
  (let [final? (fn [c]
                 (if (coll? c)
                   (not (some coll? c))
                   false))]
    (fn f [c]
      (if (final? c)
        c
        (let [h (first c)
              q (rest c)]
          (if (final? h)
            (cons h (f q))
            (concat (f h) (f q))))))))

(defcheck solution-d3f41e17
  (fn flatten-to-1 [s]
    (mapcat #(if (coll? (first %)) (flatten-to-1 %) [%]) s)))

(defcheck solution-d4652ded
  (fn f [s]
    (reduce #(concat % (if (and (sequential? %2)
                                (some sequential? %2))
                         (f %2)
                         [%2])) [] s)))

(defcheck solution-d4c64d9a
  (fn f [x]
    (if (and (coll? x) (some coll? x)) (mapcat f x) [x])))

(defcheck solution-d4e502ba
  (fn flatten-1 [[f & r :as s]]
    (cond (empty? s) nil
          (coll? f) (concat (flatten-1 f) (flatten-1 r))
          :otherwise [s])))

(defcheck solution-d4f363d8
  (fn [s]
    (filter
      #(and (coll? %) (not-any? coll? %))
      (tree-seq coll? identity s))))

(defcheck solution-d5171698
  (fn [x]
    (filter #(and (sequential? %)
                  (every? (complement sequential?) %))
      (tree-seq sequential? seq x))))

(defcheck solution-d52ae6be
  (fn partially-flatten [x]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (tree-seq sequential? seq x))))

(defcheck solution-d532e3f7
  (fn ! [sq]
    (let [f? (fn [s] (every? #(not (coll? %)) s))]
      (loop [[h & t] sq r []]
        (if (nil? h) r
                     (if (f? h) (recur t (conj r h))
                                (recur t (into r (! h)))))))))

(defcheck solution-d55b059f
  (fn flatter [coll]
    (mapcat #(if (some coll? %)
               (flatter %)
               [%])
      coll)))

(defcheck solution-d5b4b8a4
  (fn [c]
    (mapcat (fn f [X]
              (if (some coll? X)
                (for [x X]
                  (flatten (f x)))
                [X]))
      c)))

(defcheck solution-d5c572c7
  (fn [x]
    (let [b #(every? sequential? %)]
      (filter (complement b)
        (tree-seq b seq x)))))

(defcheck solution-d5da2b7b
  (fn fl [[f & s]]
    (when (seq f)
      (if (sequential? (first f))
        (fl (concat f s))
        (concat [f] (fl s))))))

(defcheck solution-d5e5053a
  (fn f [i] (if (some coll? i) (mapcat f i) [i])))

(defcheck solution-d64127be
  (fn semi-flatten--recur
    [coll] {:pre [(coll? coll)]}
    (loop [[x & tail :as coll] coll, out []]
      (cond
        (empty? coll) out
        (or (not (coll? x))
            (not-any? coll? x)) (recur tail (conj out x))
        :else (recur (concat x tail) out)))))

(defcheck solution-d65ca5e0
  (fn partial-flatten [xs]
    (if (coll? (first xs))
      (if (next xs)
        (concat (partial-flatten (first xs)) (partial-flatten (rest xs)))
        (partial-flatten (first xs)))
      (list xs))))

(defcheck solution-d693e75f
  (fn pflatten [list]
    (mapcat
      (fn [x] (if (and (coll? x) (some coll? x)) (pflatten x) [x]))
      list)))

(defcheck solution-d6bbbcd1
  (fn [coll]
    (filter #(and (sequential? %) (-> % first sequential? not))
      (rest (tree-seq sequential? seq coll)))))

(defcheck solution-d758a28d
  (fn [n]
    (loop [n n]
      (if (nil? (some #(some coll? %) n))
        n
        (recur (reduce (fn [lst i] (if (some coll? i) (into lst i) (into lst [i]))) [] n))))))

(defcheck solution-d7c7b3fc
  #(filter (every-pred coll? (comp not coll? first))
     (tree-seq coll? seq %)))

(defcheck solution-d811b7b1
  (fn flatx [& seqs]
    (mapcat (fn [s]
              (if (coll? (first s))
                (mapcat flatx s)
                (list s))) seqs)))

(defcheck solution-d88a5595
  (fn find-low-level-colls [c]
    (letfn [(low-level-coll? [c] (not-any? coll? c))]
      (if (low-level-coll? c) c
                              (reduce #(if (low-level-coll? %2) (conj % %2) (vec (concat % (find-low-level-colls %2))))
                                [] c)))))

(defcheck solution-d897c637
  (fn f [s]
    (if (and (sequential? s)
             (some sequential? s))
      (reduce (fn [a e] (concat a (f e))) '() s)
      (list s))))

(defcheck solution-d8c971c9
  (fn [x]
    (let [f (fn [x] (not (sequential? x)))]
      (filter #(and (sequential? %) (every? f %))
        (tree-seq sequential? #(if (every? f %) % (seq %)) x)))))

(defcheck solution-d8d94372
  (fn pflat [xs] (mapcat #(if (= % (flatten %)) [%] (pflat %)) xs)))

(defcheck solution-d8f2b1b1
  (fn [coll]
    (filter
      #(not-any? sequential? %)
      (tree-seq #(-> % first sequential?) seq coll))))

(defcheck solution-d966466d
  (fn part-flat [s]
    (if (not-any? sequential? s)
      (list s)
      (apply concat (map part-flat s)))))

(defcheck solution-d9664859
  #(->> (tree-seq coll? seq %)
     (partition-by (comp not coll?))
     (partition 2)
     (map second)))

(defcheck solution-d98801c0
  (fn f [[i :as x]]
    (if (coll? i)
      (mapcat f x)
      [x])))

(defcheck solution-d9d6348d
  (fn [s]
    (let [nest? #(sequential? (first %))]
      (filter (comp not nest?) (tree-seq nest? identity s)))))

(defcheck solution-da454949
  (fn my-flat [[x & xs :as xxs]]
    (when (not-empty xxs)
      (lazy-cat
        (if (nil? (some sequential? x))
          [x] (my-flat x))
        (my-flat xs)))))

(defcheck solution-da57ca2b
  (fn flattencoll [col]
    (let [fund-seq?     (fn [col]
                          (if (coll? col)
                            (if (some #(= true %) (map coll? col)) false true)
                            false))

          contain-fund? (fn [col]
                          (if (coll? col)
                            (if (some #(= false %) (map fund-seq? col)) false true)
                            false))]
      (if (contain-fund? col)
        col
        (loop [i 0 result []]
          (if (= i (count col))
            result
            (if (fund-seq? (nth col i))
              (recur (inc i) (concat result (list (nth col i))))
              (recur (inc i) (concat result (flattencoll (nth col i)))))))))))

(defcheck solution-da834558
  (fn f [l]
    (when (seq l)
      (let [[x r] (split-at 1 l)
            x (first x)]
        (if (= x (flatten x))
          (cons x (f r))
          (concat (f x) (f r)))))))

(defcheck solution-da9adea
  (fn [input-col]
    (filter (fn [item]
              (or (and (sequential? item)
                       (not (some sequential? item)))
                  ((complement sequential?) item)))
      (tree-seq (fn [input]
                  (and (sequential? input)
                       (some sequential? input)))
        seq
        input-col))))

(defcheck solution-dba89ef8
  (fn flatten-almost
    [s]
    (let [lowest? #(and (sequential? %) (not-any? sequential? %))
          atomic? #(or (not (sequential? %)) (lowest? %))]
      (lazy-seq
        (cond (lowest? s) s
              (every? atomic? s) s
              :else (let [[flat [proc & the-rest]] (split-with atomic? s)]
                      (concat flat
                        (flatten-almost proc)
                        (flatten-almost the-rest))))))))

(defcheck solution-dc46f5ef
  #(filter (comp not coll? first) (tree-seq (comp coll? first) identity %)))

(defcheck solution-dc50491e
  (fn f [coll]
    (if (every? coll? coll)
      (if (not-any? coll? (apply concat coll))
        coll
        (mapcat f coll)
        )
      [coll]
      )
    ))

(defcheck solution-dc95f203
  (fn* flatten* [x]
    (reduce #(if (coll? (first %2))
               (apply vector (concat %1 (flatten* %2)))
               (conj %1 %2)) [] x)))

(defcheck solution-dcfda9
  (fn f [x]
    (loop [a []
           x x]
      (let [q (first x)]
        (cond (nil? q) a
              (and (coll? q) (not (coll? (first q)))) (recur (conj a q) (rest x))
              (and (coll? q) (coll? (first q))) (recur (vec (concat a (f q))) (rest x))
              :else '())))))

(defcheck solution-ddad1ed5
  (fn raise [s]
    (mapcat #(if (coll? (first %)) (raise %) [%]) s)))

(defcheck solution-ddb65958
  (fn flatten-seq [coll]
    (if (and (coll? coll) (some coll? coll))
      (mapcat flatten-seq coll)
      [coll])))

(defcheck solution-ddf7f217
  (fn fun [coll]
    (reduce #(concat % %2)
      '()
      (map #(if (coll? (first %)) (fun %) (list %)) coll))))

(defcheck solution-de20800a
  (fn [x]
    (->> (tree-seq sequential? seq x)
      (rest)
      (filter #(and (sequential? %) (not-any? sequential? %))))))

(defcheck solution-de2bb1c4
  (fn flatten [s]
    (if (every? sequential? s)
      (mapcat flatten s)
      [s])))

(defcheck solution-df7fb6de
  (fn semi-flat [xs]
    (mapcat #(if (coll? (first %)) (semi-flat %) [%]) xs)))

(defcheck solution-dfc2e846
  (letfn [(L? [s]
            (coll? (first s)))

          (F [s]
            (filter (complement L?)
              (tree-seq L? identity s)))]
    F))

(defcheck solution-dfedba42
  (fn [s]
    (loop [c (first s) s (next s) r []]
      (if c
        (cond
          (and (sequential? c) (sequential? (first c))) (recur (first c) (cons (rest c) s) r)
          (seq c) (recur (first s) (rest s) (conj r c))
          :else (recur (first s) (rest s) r))
        r)
      )))

(defcheck solution-e01d1b0b
  (fn ! [[x & xs :as coll]]
    (remove empty?
      (when (seq coll)
        (lazy-cat
          (list (remove coll? coll))
          (mapcat ! (filter coll? coll)))))))

(defcheck solution-e064866b
  (fn solve [coll]
    (cond (empty? coll) coll
          (coll? (first coll)) (mapcat solve coll)
          :else [coll]
          )))

(defcheck solution-e0907920
  (fn [coll]
    (let [rfn (fn rfn [[x & xs] acc]
                (cond
                  (nil? x) acc
                  (coll? x) (if (coll? (first x)) (recur xs (concat acc (rfn x [])))
                                                  (recur xs (concat acc [x])))
                  :else (recur xs (concat acc [x]))))]
      (rfn coll ()))))

(defcheck solution-e0955e8e
  (fn flatten [seq]
    (loop [coll seq result []]
      (if (empty? coll)
        result
        (recur
          (rest coll)
          (if (coll? (first (first coll)))
            (into result (flatten (first coll)))
            (conj result (first coll))                      ;; one level coll
            )
          )
        ))))

(defcheck solution-e0bac046
  (fn pf [[x & xs :as c]]
    (cond (empty? c) c
          (or (not (coll? x)) (not (coll? (first x)))) (cons x (pf xs))
          :else (concat (pf x) (pf xs)))))

(defcheck solution-e0c7abe1
  (fn f [[x & xs :as xxs]]
    (cond
      (nil? xxs) []
      (or (not (coll? x))
          (and (coll? x)
               (not-any? #(coll? %) x)))
      (cons x (f xs))
      :else
      (concat (f x) (f xs)))))

(defcheck solution-e0d66c79
  (fn f [x] (mapcat #(if ((fn [x] (let [[a] x] (sequential? a))) %) (f %) [%]) x)))

(defcheck solution-e0dcb407
  (fn [t]
    ((fn f [t a]
       (cond (empty? t) a
             (every? #(not (coll? %)) t)
             (cons t a)
             :else (f (first t) (f (rest t) a))))
     t '())))

(defcheck solution-e0ff6fd8
  (letfn [(min-depth [col] (if (not (coll? col))
                             0
                             (inc (apply min (map min-depth col)))))
          (pfl [col] (apply concat (for [x col]
                                     (if (< (min-depth x) 2)
                                       [x]
                                       (pfl x)))))]
    pfl))

(defcheck solution-e13c8b79
  (fn [s] (partition (count (flatten (first s))) (flatten s))))

(defcheck solution-e215f524
  (fn part [seqs]
    (let [a (atom [])]
      (letfn [(p [s]
                ;(println "@" s)
                (doall
                  (if
                   (and
                    (sequential? s)
                    (empty? (filter sequential? s)))
                    (swap! a conj s)
                    (map p s))))] (p seqs)) @a)))

(defcheck solution-e28af5e5
  (fn f [x]
    (if (some coll? x)
      (mapcat f x)
      [x])))

(defcheck solution-e29838cd
  (fn flatter [coll]
    (if (not-any? coll? coll) (list coll)
                              (if (every? coll? (apply concat coll)) (apply concat (map flatter (apply concat coll)))
                                                                     (if (every? coll? coll) (flatter (map flatter coll))
                                                                                             (apply concat (map flatter coll)))))))

(defcheck solution-e3a38b1d
  (fn [s] (filter #(and (sequential? %) (every? (complement sequential?) %)) (tree-seq sequential? seq s))))

(defcheck solution-e4560e88
  (fn [coll]
    (letfn [(step [coll result]
              (if (seq coll)
                (let [first-ele (first coll)]
                  (step (next coll)
                    (if (some coll? first-ele)
                      (reduce #(conj %1 %2) result (step first-ele []))
                      (conj result first-ele))))
                result)
              )]
      (step coll []))))

(defcheck solution-e4cb5ebf
  (fn flatten-me [s]
    (if (coll? (first s))
      (mapcat flatten-me s)
      [s])))

(defcheck solution-e4d2e3cb
  (fn ft [xs]
    (reduce #(if (reduce (fn [p, c] (and p (coll? c))) true %2)
               (into [] (concat %1 (partition 2 (flatten %2))))
               (conj %1 %2))
      [] xs)))

(defcheck solution-e4f17d7c
  (fn f [coll]
    (reduce
      (fn [v e]
        (if (coll? (first e))
          (into v (f e))
          (conj v e)))
      []
      coll)))

(defcheck solution-e5c8981c
  (fn f [s]
    (mapcat
      #(cond
         (not (coll? %)) (list %)
         (= 1 (count %)) (list (flatten %))
         (not-any? coll? %) (list %)
         :else (f %)) s)))

(defcheck solution-e5d94e3
  (fn pft [[h & t]]
    (concat
      (if (coll? (first h))
        (pft h)
        [h])
      (if (coll? (first t))
        (pft t)
        t))))

(defcheck solution-e6268132
  (fn [vs]
    (letfn [(nested-seq? [v]
              (and (sequential? v)
                   (every? #(not (sequential? %)) v)))
            (flt [coll]
              (if (nested-seq? coll) (if (empty? coll) () [coll])
                                     (concat (flt (first coll)) (flt (rest coll)))))]
      (if (nested-seq? vs) vs (flt vs)))))

(defcheck solution-e65ba198
  (fn f [s] (if (= s (flatten s)) [s] (mapcat f s))))

(defcheck solution-e66cd3e3
  (fn f [c]
    (mapcat #(if (coll? (first %)) (f %) [%]) c)))

(defcheck solution-e6d16091
  (fn ff [l] (mapcat #(if (and (coll? %) (coll? (first %))) (ff %) [%]) l)))

(defcheck solution-e6f566d8
  (fn [x]
    (filter #(not (sequential? (first %)))
      (rest (tree-seq #(sequential? (first %)) seq x)))))

(defcheck solution-e71bfd5c
  (fn [coll]
    (reverse
      (loop [c   coll
             res []]
        (cond (empty? c) res
              (and
               (coll? (first c))
               (some coll? (first c))) (recur (concat (first c) (rest c)) res)
              :else (recur (rest c)
                      (cons (first c) res)))))))

(defcheck solution-e750cb5d
  (fn [l]
    (letfn [(flat [l]
              (into {}
                (if (coll? (first l))
                  (for [x l]
                    (flat x))
                  {l 0})))]
      (keys (flat l)))))

(defcheck solution-e7613712
  (fn partial-flatten
    [s]
    (letfn [(leaf? [x] (or (not (sequential? x)) (not-any? sequential? x)))]
      (mapcat (fn [x] (if (leaf? x) [x] (partial-flatten x)))
        s))))

(defcheck solution-e7af6e14
  (fn my-flatten [xs]
    (let [contains-seq #(some sequential? %)]
      (remove contains-seq (tree-seq contains-seq seq xs)))))

(defcheck solution-e831a55a
  (fn ! [l]
    (if (= l (flatten l)) l
                          (let [ms  (map ! l)
                                pms (map #(if (= % (flatten %)) [%] %) ms)
                                lol (apply concat pms)]
                            lol))))

(defcheck solution-e85899a3
  (fn pa [x]
    (->> (tree-seq coll? identity x)
      (remove (complement coll?))
      (filter #(not (coll? (first %)))))))

(defcheck solution-e8f3e7e2
  (fn p [t]
    (if (every? sequential? t)
      (mapcat p t)
      [t])))

(defcheck solution-e8f88cef
  (fn [coll]
    (filter #(and (sequential? %) ((complement sequential?) (first %)))
      (tree-seq sequential? seq coll))))

(defcheck solution-e903510f
  (fn [x]
    (letfn [
            (h [x] (> (count (filter sequential? x)) 0))
            ]
      (filter (complement h)
        (rest (tree-seq h seq x))))))

(defcheck solution-e93c1224
  (fn my-flat [coll]
    (cond
      (not (coll? (first coll))) [coll]
      (= (count coll) 1) (my-flat (first coll))
      :else (reduce concat (map my-flat coll)))))

(defcheck solution-e95bb094
  (fn flat-part [[a & b]]
    (when (not (nil? a))
      (if (and (coll? a) (not-any? coll? a))
        (concat (list a) (flat-part b))
        (concat (flat-part a) (flat-part b))))))

(defcheck solution-e9eb2d82
  (fn f [s] (if (sequential? (first s)) (mapcat f s) [s])))

(defcheck solution-ea075b80
  (fn pfs [c]
    (reduce #(if (and (coll? %2) (some coll? %2))
               (concat % (pfs %2))
               (conj (vec %) %2)) [] c)))

(defcheck solution-ea4a9b5f
  (fn flatten [coll]
    (if (some sequential? coll)
      (mapcat flatten coll)
      [coll])))

(defcheck solution-ea4c64c2
  (fn [x]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-eaa53111
  (fn [coll]
    (filter #(when (sequential? %)
               (every? true? (map (complement sequential?) %)))
      (rest (tree-seq sequential? seq coll)))))

(defcheck solution-eab19059
  (fn f [x]
    (if (some coll? x)
      (mapcat f x)
      [x])))

(defcheck solution-eab5e484
  (fn [coll]
    (loop [src-coll coll
           dst-coll []]
      (if (empty? src-coll)
        dst-coll
        (if (sequential? (first (first src-coll)))
          (recur (concat (first src-coll) (rest src-coll))
            dst-coll)
          (recur (rest src-coll) (conj dst-coll (first src-coll))))))))

(defcheck solution-eaf668d
  #(filter (fn [r] (and (sequential? r) (not-any? sequential? r))) (tree-seq (fn [n] (and (sequential? n) (some sequential? n))) identity %)))

(defcheck solution-eb73d1cd
  (fn partial-flatten
    [seq]
    (let [return          (empty seq)
          concat-1-depths (fn concat-1-depths [return seq]
                            #_(println return seq)
                            (if (every? (comp not coll?) seq)
                              (concat return [seq])
                              (reduce concat-1-depths return seq)))]
      (reduce concat-1-depths return seq))))

(defcheck solution-ebc2dcee
  (fn partially-flatten [items]
    (cond (not (sequential? items)) items
          (some sequential? items) (mapcat partially-flatten items)
          :else [items])))

(defcheck solution-ebe8cc43
  (fn partial-flatten [colls]
    (mapcat (fn [coll]
              (if (some sequential? coll)
                (partial-flatten coll)
                [coll]))
      colls)))

(defcheck solution-ecc0383
  (fn partflat [xs] (mapcat #(if (every? sequential? %) (partflat %) [%]) xs)))

(defcheck solution-ed0a9c6f
  (fn [s] (filter #(and (coll? %) (not (coll? (first %)))) (tree-seq coll? identity s))))

(defcheck solution-ed1d9c26
  (fn prob93
    ([coll] (prob93 coll []))
    ([coll result]
     (if (seq coll)
       (let [first-item (first coll)]
         (prob93 (next coll)
           (if (some coll? first-item)
             (reduce #(conj %1 %2) result (prob93 first-item []))
             (conj result first-item))))
       result
       )
     )
    ))

(defcheck solution-ed36cda1
  (fn [s]
    (loop [v s r []]
      (if (empty? v)
        r
        (let [f (first v) f? (coll? (first f))]
          (recur
            (if f? (concat (first v) (rest v)) (rest v))
            (if f? r (conj r f))))))))

(defcheck solution-ed428efc
  (fn f [[a & b]]
    (if (not (coll? (first a))) (concat [a] (if (empty? b) nil (f b)))
                                (f (concat a (if (empty? b) nil (f b)))))))

(defcheck solution-ed6a7fbc
  (fn [x]
    (filter #(and (sequential? %)
                  (not (sequential? (first %))))
      (tree-seq sequential? seq x))))

(defcheck solution-edfd1f4a
  (fn flat
    [lista]
    (letfn [(is-atom? [x]
              (and (sequential? x) (not (sequential? (first x)))))]
      (if (is-atom? lista)
        [lista]                                             ; add brackets to defend against "mapcat", think about it.
        (mapcat flat lista)))))

(defcheck solution-ee13e45e
  (fn [s]
    (let [check? #(not-any? coll? %)
          f      (fn f [s]
                   (if (check? s)
                     (list s)
                     (->> (filter coll? s)
                       (mapcat f))))]
      (f s)
      )))

(defcheck solution-ee2773e6
  (fn fl [col]
    (if (every? (complement sequential?) col)
      [col]
      (mapcat fl col))))

(defcheck solution-ee518791
  (fn fl [s] (mapcat #(if (some coll? %) (fl %) [%]) s)))

(defcheck solution-ee6c463b
  (fn [xs]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (tree-seq #(and (sequential? %) (some sequential? %)) seq xs))))

(defcheck solution-eeab566d
  (fn f
    [l]
    (if (some (complement coll?) l)
      [l]
      (mapcat f l))))

(defcheck solution-ef191018
  (fn [col]
    (filter
      (fn [x]
        (every? #(not (sequential? %)) x))
      (tree-seq (fn [node]
                  (if (sequential? node)
                    (every? sequential? node)
                    false))
        seq
        col)
      )))

(defcheck solution-f0247445
  (fn f [x] (if (or (not (coll? x))
                    (every? #(not (coll? %)) x))
              [x]
              (mapcat f x))))

(defcheck solution-f08a1fb2
  (fn x [c]
    (cond
      (empty? c) nil
      (coll? (ffirst c)) (lazy-cat (x (first c)) (x (next c)))
      :else (lazy-cat [(first c)] (x (next c))))))

(defcheck solution-f190936f
  #(remove % (tree-seq % seq %2)) #(sequential? (nth % 0)))

(defcheck solution-f1cafb09
  (fn this [thing] (mapcat #(if (sequential? (first %)) (this %) (vector %)) thing)))

(defcheck solution-f1ece48c
  (fn [s]
    (->> s
      (tree-seq sequential? seq)
      (filter sequential?)
      (filter (comp (complement sequential?) first)))))

(defcheck solution-f206dbbf
  (fn f [x]
    (letfn [(t [y] (every? #(not (coll? %)) y))]
      (reduce
        (fn [y z] (if (t z) (conj y z) (vec (concat y (f z)))))
        []
        x))))

(defcheck solution-f215afba
  (fn partially-flatten [s]
    (mapcat
      #(if (and (sequential? %) (not (sequential? (first %))))
         (list %)
         (partially-flatten %))
      s)))

(defcheck solution-f23b8efa
  (fn [coll]
    (letfn [(partialFlatten [coll result]
              (if (seq coll)
                (let [first-ele (first coll)]
                  (partialFlatten (next coll)
                    (if (some coll? first-ele)
                      (reduce #(conj %1 %2) result (partialFlatten first-ele []))
                      (conj result first-ele))))
                result))]
      (partialFlatten coll []))))

(defcheck solution-f2a5abd1
  (fn [x]
    (loop [c false
           r x]
      (if c r
            (let [rr (reduce #(if (coll? (first %2))
                                (loop [z %2 y %]
                                  (if (empty? z)
                                    y
                                    (recur (rest z) (concat y (list (first z))))))
                                (concat % (list %2))) '() r)
                  mr (apply = (map #(coll? (first %)) rr))]
              (recur mr rr))))))

(defcheck solution-f2afb59c
  (fn [v]
    (filter #(and (sequential? %) (not (sequential? (first %)))) (rest (tree-seq sequential? seq v)))))

(defcheck solution-f2c9bf07
  (fn f [s] (if (some coll? s) (mapcat f s) [s])))

(defcheck solution-f2dc931b
  (fn func [xs]
    (if (some sequential? xs) (mapcat func xs) [xs])))

(defcheck solution-f31b4fa1
  (fn f [c]
    (reduce
      (fn [r x]
        (if (and (coll? x)
                 (some coll? x))
          (vec (concat r (f x)))
          (conj r x))) [] c)))

(defcheck solution-f31ccdf5
  (fn partial-flatten [coll]
    (loop [remaining coll ans []]
      (if (empty? remaining)
        ans
        (if (sequential? (first (first remaining)))
          (recur (rest remaining) (into [] (concat ans (partial-flatten (first remaining)))))
          (recur (rest remaining) (conj ans (first remaining))))))))

(defcheck solution-f32c96b9
  (fn cat1 [xs]
    (if (empty? xs)
      ()
      (if (and (sequential? (first xs))
               (sequential? (ffirst xs)))
        (concat (cat1 (first xs)) (cat1 (rest xs)))
        (cons (first xs) (cat1 (rest xs)))))))

(defcheck solution-f33a2cec
  (fn pflat [xs]
    (letfn [

            (lastlevel [xs]
              (every? false? (map sequential? xs))

              )

            ]
      (filter lastlevel (filter sequential? (tree-seq sequential? identity xs))))

    ))

(defcheck solution-f3631e6
  (fn meu-flatten [[[primeiro-primeiro-elemento :as primeiro-elemento] & outros-elementos :as elementos]]
    (when (seq elementos)
      (if (coll? primeiro-primeiro-elemento)
        (concat (meu-flatten primeiro-elemento) (meu-flatten outros-elementos))
        (cons primeiro-elemento (meu-flatten outros-elementos))))))

(defcheck solution-f4187ef8
  (fn nflatten [lat]
    (loop [l lat r (empty lat)]
      (if (empty? l)
        r
        (if (coll? (first l))
          (recur (rest l) (concat r (nflatten (first l))))
          (recur '() (cons l r)))))
    ))

(defcheck solution-f429d1f7
  (fn f [s]
    (mapcat #(if (sequential? (first %)) (f %) [%]) s)))

(defcheck solution-f4c594e4
  (fn [xs] (filter (complement #(every? sequential? %)) (tree-seq #(every? sequential? %) seq xs))))

(defcheck solution-f4ca838a
  (fn [l]
    (let [n (mapcat #(if (every? coll? %) % [%]) l)]
      (if (= l n) l
                  (recur n)))))

(defcheck solution-f4cdced8
  (fn [xs]
    (letfn
     [(nested-list? [xs]
        (and (coll? xs)
             (every? coll? xs)))]
      (filter
        (complement nested-list?)
        (tree-seq nested-list? seq xs)))))

(defcheck solution-f538a08a
  (fn [t]
    ((fn f [a t]
       (if (coll? (first t))
         (reduce f a t)
         (conj a t)))
     [] t)))

(defcheck solution-f5611be9
  (fn [coll]
    (letfn [(not-lowest [node] (and (sequential? node) (some sequential? node)))]
      (filter (complement not-lowest)
        (rest (tree-seq not-lowest seq coll))))))

(defcheck solution-f584392d
  (fn [s]
    (loop [flattened [] rem s]
      (if (empty? rem)
        flattened
        (let [f (first rem)]
          (if (not (coll? (first f)))
            (recur (conj flattened f) (rest rem))
            (if (= 1 (count f))
              (recur flattened (cons (first f) (rest rem)))
              (recur flattened (cons (first f) (cons (rest f) (rest rem)))))))))))

(defcheck solution-f5e865cf
  #(loop [res [] s %]
     (if (seq s)
       (let [[x & xs] s]
         (if (every? sequential? x)
           (recur res (concat x xs))
           (recur (conj res x) xs)))
       res)))

(defcheck solution-f60be927
  (fn flat [s]
    (if-not (coll? s)
      s
      (let [[x & xs] s]
        (if (and (coll? x) (coll? (first x)))
          (concat (flat x) (flat xs))
          (cons x (flat xs)))))))

(defcheck solution-f7082647
  #(for [x (tree-seq coll? seq %)
         :when (and (coll? x) (not (coll? (first x))))]
     x))

(defcheck solution-f78e252f
  (fn [s]
    (filter #(and (sequential? %) (every? (complement sequential?) %))
      (tree-seq sequential? seq s))))

(defcheck solution-f7d061c9
  (fn partial-flatten
    [s]
    (loop [accum []
           [h & t] s]
      (let [new-accum (if (sequential? h)
                        (into accum (partial-flatten h))
                        [s])]
        (if (nil? t)
          new-accum
          (recur new-accum t))))))

(defcheck solution-f826b02f
  (fn fla [x]
    (let [f (first x) n (next x)]
      (concat
        (if (sequential? (first f)) (fla f) [f])
        (if (sequential? (first n)) (fla n) [])
        )
      )
    ))

(defcheck solution-f832e562
  (fn [x]
    (let [b #(every? sequential? %)]
      (filter (complement b)
        (tree-seq b seq x)))))

(defcheck solution-f8918568
  (fn f [[h & t]]
    (if h
      (if (some coll? h)
        (concat (f h) (f t))
        (cons h (f t))))))

(defcheck solution-f9311ce3
  (fn f [s]
    (if (some coll? s) (mapcat f s) (list s))))

(defcheck solution-f99b89ba
  (fn [c]
    (filter
      #(and (sequential? %)
            (not-any? sequential? %))
      (tree-seq sequential? identity
        c))))

(defcheck solution-f9eb7b5e
  (fn [xs]
    (filter
      #(and (coll? %) (not (coll? (first %))))
      (tree-seq coll? identity xs))))

(defcheck solution-fa083a91
  (fn pflat [acc s]
    (if (some coll? s)
      (pflat (pflat acc (first s)) (rest s))
      (if (seq s)
        (conj acc s)
        acc))) [])

(defcheck solution-fa1b808a
  (fn pfs [s] (mapcat #(if (some sequential? %) (pfs %) [%]) s)))

(defcheck solution-fa34efbd
  #(for [s [coll?]
         i (tree-seq s seq %)
         :when (and (s i)
                    (not-any? s i))]
     i))

(defcheck solution-fa48a00f
  mapcat #(partition-all 2 (flatten %)))

(defcheck solution-fade82e0
  (fn flatten2 [ll]
    (mapcat #(if (and (sequential? %) (sequential? (first %))) (flatten2 %) [%]) ll)))

(defcheck solution-fb066258
  (fn [s]
    (letfn [(simple-coll? [s] (and (coll? s) (every? (complement sequential?) s)))]
      (filter #(or ((complement sequential?) %) (simple-coll? %))
        (rest (tree-seq #(and (sequential? %) (not (simple-coll? %))) seq s))))))

(defcheck solution-fb42907a
  #(filter
     (partial every? (complement sequential?))
     (tree-seq
       (partial some sequential?)
       identity
       %)))

(defcheck solution-fb894bff
  (fn flat [x] (if (some sequential? x) (mapcat flat x) [x])))

(defcheck solution-fbc29dc1
  (fn f [x]
    (if ((comp not coll? first) x) (list x)
                                   (apply concat (map f x)))))

(defcheck solution-fc35264c
  (fn flatSeq [xs]
    (letfn [(flatOne [xs]
              (if (sequential? (first xs))
                (flatSeq xs)
                (list xs)))]
      (mapcat flatOne xs))))

(defcheck solution-fc9691d3
  #(let [t (rest (tree-seq sequential? seq %))
         m (apply hash-map (interleave (range) (partition-by sequential? t)))
         f (if (odd? (count m)) even? odd?)]
     (vals (filter (fn [[k v]] (f k)) m))))

(defcheck solution-fc9e1c06
  (fn f [x] (if (some #(not (sequential? %)) x)
              [x]
              (mapcat f x))))

(defcheck solution-fcc19c41
  (fn flatten-one-level [coll]
    (letfn [(single-level? [coll]
              (and (sequential? coll)
                   (not-any? sequential?
                     coll)))]
      (cond
        (empty? coll) nil
        (single-level? coll) [coll]
        :else (concat (flatten-one-level (first coll))
                (flatten-one-level (rest coll)))))))

(defcheck solution-fd17b61b
  (fn __
    [coll]
    (letfn [(my-filter [x]
              (and (sequential? x) (every? (complement sequential?) x)))]
      (concat (empty coll) (filter my-filter
                             (rest (tree-seq sequential? seq coll)))))))

(defcheck solution-fd39425c
  (fn pf [c]
    (if (and (coll? c) (not (coll? (first c))))
      [c]
      (mapcat #(pf %) c))))

(defcheck solution-fdab98f2
  (fn f [s] (filter #(and (coll? %) (not-any? coll? %)) (tree-seq coll? identity s))))

(defcheck solution-fe2ab484
  (fn pairs [x]
    (filter #(and (sequential? %) (not-any? sequential? %))
      (tree-seq sequential? seq x))))

(defcheck solution-feea7397
  (fn f [coll]
    (if (every? (complement coll?) coll)
      [coll]
      (mapcat f coll))))

(defcheck solution-ffad4b50
  (fn flatten-1 [xs]
    (letfn [(stepper [xs]
              (when (not (empty? xs))
                (let [cur (last xs)]
                  (if-not (and (coll? cur) (every? coll? cur))
                    (cons cur (stepper (butlast xs)))
                    (recur (concat (butlast xs) cur))))))]
      (into '() (stepper xs)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-93))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

