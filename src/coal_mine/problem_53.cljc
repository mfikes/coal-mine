(ns coal-mine.problem-53
  (:require [coal-mine.checks :refer [defcheck-53] :rename {defcheck-53 defcheck}]
            [clojure.test]))

(defcheck solution-1037e99a
  #(if (= (reverse (sort %)) %)
     []
     (let[split (fn [a b]
                  (if (< (last(last a)) b)
                    (concat (drop-last a) [(conj (last a) b)])
                    (concat a [[b]])))]
       (first(second(last(sort (group-by count(reduce split [[(first %)]] (rest %)))))))
       )
     ))

(defcheck solution-10afae81
  (fn subs [x]
    (first
      (reverse
        (sort
          (for [idx (range 0 (count x))]
            (loop [frag (drop idx x)
                   result (vector (first frag))
                   curr (first frag)
                   s (rest frag)]
              (if (empty? s)
                (if (>= (count result) 2) result [])
                (if (< curr (first s))
                  (recur [] (conj result (first s)) (first s) (rest s))
                  (if (>= (count result) 2) result []))))))))))

(defcheck solution-11be37f1
  (fn [a-seq]
    (letfn [(t-while-inc [s]
              (lazy-seq
                (cond
                  (empty? s) ()
                  (empty? (rest s)) s
                  (>= (first s) (second s)) (cons (first s) ())
                  :else (cons (first s) (t-while-inc (rest s))))))]
      (loop [lseq a-seq acc []]
        (if (empty? lseq)
          (if (>= (count acc) 2) acc [])
          (let [n-seq (t-while-inc lseq)
                cnt (count n-seq)
                new-acc (cond
                          (< (count acc) cnt) (into [] n-seq)
                          :else acc)]
            (recur (drop cnt lseq) new-acc)))))))

(defcheck solution-11c846c
  (fn [s]
    (let [increase-subsequences (fn [subsequences i]
                                  (conj (mapv
                                          (fn [[variant-tag subsequence :as ts]]
                                            (condp = variant-tag
                                              :frozen ts
                                              :growing
                                              (if (< (last subsequence) i)
                                                [:growing (conj subsequence i)]
                                                [:frozen subsequence])))
                                          subsequences)
                                    [:growing [i]]))]
      (or (->> (reduce increase-subsequences [] s)
            (mapv second)
            (filterv #(> (count %) 1))
            (sort-by (comp - count))
            first)
          []))))

(defcheck solution-124a7cf0
  (fn consecutive-seq [xs]
    (let [all-seq (loop [tail (rest xs) result (list (list (first xs)))]
                    (let [tail (seq tail)]
                      (if (seq? tail)
                        (let [x (first tail)
                              y (last (last result))]
                          (if (not= (inc y) x)
                            (recur (rest tail) (concat result (list (list x))))
                            (recur (rest tail) (concat (butlast result) (list (concat (last result) (list x)))))))
                        result)))
          max-seq (last (sort-by count all-seq))]
      (if (> (count max-seq) 1) (vec max-seq) [])
      )))

(defcheck solution-1298c60e
  (letfn [(U [a* a]
            (if (and (>= (count a*) 2)
                     (>= (count a*) (count a)))
              a* a))

          (X [s t* t]
            (cond (empty? s) (U t* t)
                  (= (first s) (inc (last t*))) (recur (rest s) (conj t* (first s)) t)
                  :else (recur (rest s) [(first s)] (U t* t))))]
    (fn [s]
      (X (rest s) [(first s)] []))))

(defcheck solution-12cd599b
  #(let [m (vec
             (for [x (range 1 (count %))
                   :when (>= (% (dec x)) (% x))] x)) ]
     (let [mm (vec (cons 0 (conj m (count %))))]
       (let [n (vec
                 (map - (rest mm)
                   (take (dec (count mm)) mm)))]
         (let [i (.indexOf n (apply max n)) ]
           (let [ [a b] [(mm i) (mm (inc i))] ]
             (if (= (inc a) b) [] (subvec % a b))
             ))))))

(defcheck solution-12e53e91
  (fn [x]
    (loop [tot [(first x)] result [] x (drop 1 x)]
      (if (empty? x)
        (reduce (fn [t v] (if (> (count t) (count v)) t v )) [] result)
        (if (= (range (first tot) (inc (last tot))) tot)
          (if (= (conj tot (first x)) (range (first tot) (inc (first x))))
            (recur (conj tot (first x)) (conj result (if (> (count (conj tot (first x))) 1) (conj tot (first x)) [] ))  (drop 1 x) )
            (recur (conj [] (first x) ) (conj result (if (> (count tot) 1) tot []) ) (drop 1 x)))
          (recur (conj [] (first x)  ) result (drop 1 x))
          )))))

(defcheck solution-12f5a5ea
  (fn myf [s] (first (reduce (fn[a x](let [[m c] a nextc (conj c x)]
                                       (if (<= x (peek c))
                                         [m [x]]
                                         (if (< (count m)(count nextc))
                                           [nextc nextc]
                                           [m nextc])
                                         ))
                               ) [[] [(first s)]] (rest s)))))

(defcheck solution-132a6d09
  (fn p53 [xs]
    ( let [stepfn (fn [[x1 count] x2] (vector x2 (if (< x1 x2) count (inc count))))
           steps (drop 1 (reductions stepfn [0 0] xs))] ;; tuples of [xi step]

      (->> steps
        (partition-by second) ;; partion by step
        (map #(map first %)) ;; extract original values
        (filter #(< 1 (count %)))
        (sort #(> (count %1) (count %2))) ;; sort by length
        (first)
        (sequence)))))

(defcheck solution-139cb208
  (fn [coll]
    (let [x (last (sort-by count (reduce #(if (= (last (last %1)) (dec %2))
                                            (conj (pop %1) (conj (last %1) %2))
                                            (conj %1 (vector %2)))
                                   [] coll)))]
      (if (= 1 (count x)) [] x))))

(defcheck solution-13d4b12c
  (fn [xs]
    (let [ys (map-indexed (fn [i e] [i e]) xs)
          r  (map last (last (sort-by count (partition-by (fn [[i e]] (- e i)) ys))))]
      (if (> (count r) 1) r []))))

(defcheck solution-1437824c
  (fn [s]
    ((fn [s r p q l m]
       (cond
         (not s) (if (> l 1)
                   r
                   [])
         (= (inc p) (first s)) (recur (next s)
                                 (if (> (inc m) l)
                                   (concat q [(first s)])
                                   r)
                                 (inc p)
                                 (concat q [(first s)])
                                 (inc m)
                                 (inc m))
         :else (recur (next s)
                 r
                 (first s)
                 [(first s)]
                 l
                 1)))
     s 0 0.1 0 0 0)))

(defcheck solution-1465e076
  (fn [s]
    (loop [[x y & xs :as all] s r [] c [x]]
      (let [size-c (count c)
            new-r (if (and (> size-c 1) (> size-c (count r))) c r)
            new-c (if (= (inc (last c)) y) (conj c y) [y])]
        (if (and x y)
          (recur (next all) new-r new-c)
          new-r
          )
        )
      )
    ))

(defcheck solution-149c9c4a
  (fn l [s]
    (let [seqs (reduce (fn [c n]
                         (if (> n (last (last c)))
                           (conj (vec (drop-last c))
                             (conj (last c) n))
                           (conj c [n])))
                 [[(first s)]]
                 (rest s))
          maxlen (apply max (map count seqs))]
      (if (< maxlen 2)
        []
        (first (filter #(= maxlen (count %)) seqs))))))

(defcheck solution-1509bf0a
  (fn [v] (let [x (last (sort-by count (filter #(second (first %)) (partition-by #(second %) (map-indexed (fn [a b] [b (= b (inc (nth v a))) a]) (rest v)))))) i (last (first x))] (if x (subvec v i (+ i (inc (count x)))) []))))

(defcheck solution-1517a418
  (fn lis [xs]
    (letfn [(lis-from [xs acc ]
              (loop [[y & ys] xs a [acc]]
                (if (not= 1 (- y (last a))) a
                                            (if (empty? ys) (conj a y) (recur ys (conj a y))))))]
      (apply max-key #(let [x (count %)] (if (= x 1) (- 0 1) x))
        (cons [] (map #(lis-from (rest %) (first %)) (take (- (count xs) 1) (iterate rest xs))))))))

(defcheck solution-153c574a
  (fn [coll]
    (let [n (count coll)]
      (reduce (fn [x y] (if (>= (count x) (count y)) x y)) []
        (for [
              d (range n)
              t (range (- n d) 0 -1)
              :let [c (take t (drop d coll))]
              :when (and (apply < c) (> (count c) 1) )] c)))))

(defcheck solution-15459ff8
  (fn [%]
    (let [get-max  (fn [a b] (if (> (count b) (count a)) b a))
          norm-inc (fn [%] (if (= 1 (count %)) [] %))
          inc-seq (fn inc-seq [%]
                    (loop [s %
                           v []]
                      (cond
                        (nil? s) (norm-inc v)
                        (or (empty?  v)
                            (= (first s) (inc (peek v)))) (recur (next s) (conj v (first s)))
                        :else (norm-inc v))))]
      (loop [s %
             max-s  []]
        (cond
          (nil? s) max-s
          :else (recur (next s) (get-max max-s (inc-seq s)))))

      )
    ))

(defcheck solution-15529a65
  (fn [coll]
    (let [increasing? (fn [xs] (apply < xs))
          n (count coll)
          sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
      (->> sub-seqs
        (filter increasing?)
        (cons [])
        (sort-by count >)
        first))))

(defcheck solution-15b806d2
  (fn [S] (or (first (for
                      [l (reverse (range 2 (count S)))
                       f (filter #(apply < %) (partition l 1 S))]
                       f)) [])))

(defcheck solution-16d4e6b
  (fn [xs]
    (reduce (fn [a v]
              (let [ca (count a)
                    cv (count v)]
                (if (and (> cv 1) (> cv ca))
                  v
                  a))) []
      (loop [colls [] coll [] r xs]
        (if (empty? r)
          (conj colls coll)
          (let [fv (first r)]
            (if (= fv (inc (or (last coll) -2147483648)))
              (recur colls (conj coll fv) (rest r))
              (recur (conj colls coll) [fv] (rest r)))))))))

(defcheck solution-16eb5b02
  (fn [v]
    (let [suffixes #(map (fn [n] (subvec % n)) (range (dec (count %))))
          prefixes #(map (fn [n] (subvec % 0 (inc n))) (range 1 (count %)))
          increasing? #(every? (fn [[a b]] (< a b)) (map vector % (rest %)))
          subseqs (->> (suffixes v)
                    (mapcat prefixes)
                    (filter increasing?)
                    (sort #(> (count %1) (count %2))))]
      (if (empty? subseqs) []
                           (first subseqs)))))

(defcheck solution-176c0280
  (fn [l]
    (let [lt (partial apply <)
          pairs (->> 	(map vector l (rest l))
                  (partition-by lt)
                  (filter (comp lt first)))
          max-count (apply max 0 (map count pairs))]
      (->> 	pairs
        (filter (comp (partial = max-count) count))
        (first)
        (#(if %	(conj (vec (map first %))
                   (last (last %)))
                 []))))))

(defcheck solution-177fbc8e
  (fn [s]
    (->> (map vector s (rest s))
      (partition-by (fn [[a b]] (< a b)))
      (map #(conj (map second %) (first (first %))))
      (filter (partial apply <))
      (reverse)
      (#(when (not (empty? %)) (apply max-key count %)))
      (vec))))

(defcheck solution-1791e747
  (fn [coll]
    (when-let [coll (seq coll)]
      (loop [[_ & args] coll
             l [(first coll)]
             best l]
        (if (empty? args)
          (if (< (count best) 2) [] (if (> (count l) (count best)) l best))
          (if (> (first args) (peek l))
            (recur args (conj l (first args)) best)
            (if (> (count l) (count best))
              (recur args [(first args)] l)
              (recur args [(first args)] best))))))))

(defcheck solution-17bad5d7
  (fn __ [col]
    (or
     (last (sort-by count
             (filter #(and (> (count %) 1)
                           (= %
                             (take (count %) (range (first %) (+ (first %) (count %))))))
               (for [a (range (inc (count col)))
                     b (range (inc (count col))) :when (>= b a)]
                 (subvec col a b)))))
     [])))

(defcheck solution-18033076
  (fn [xs]
    (:longest
     (reduce (fn [result-map i]
               (let [{:keys [curr longest]} result-map
                     new-curr (if-not (last curr)
                                [i]
                                (if (< (last curr) i)
                                  (conj curr i)
                                  [i]) )
                     new-longest (if (and (> (count new-curr) (count longest)) (<= 2 (count new-curr)))
                                   new-curr
                                   longest)]
                 (assoc result-map :curr new-curr :longest new-longest)))
       {:longest [] :curr []} xs))))

(defcheck solution-1832e5a5
  (fn [s]
    (loop [[p & r] (partition 2 1 s) cur [] lng []]
      (if (nil? p) lng
                   (let [[j d] p]
                     (if (= d (inc j))
                       (if (empty? cur)
                         (recur r [j d]
                           (if (empty? lng) [j d] lng))
                         (recur r (conj cur d)
                           (if (= (count cur)
                                 (count lng))
                             (conj cur d) lng)))
                       (recur r [] lng)))))))

(defcheck solution-188734f7
  #(let [res (reduce (fn [a b] (if (> (count b) (count a)) b a))
               (reductions (fn [lis new]
                             (if (> new (last lis))
                               (conj lis new)
                               [-1 new]))
                 [-1] %))]
     (if (> (count res) 2) (rest res) [])))

(defcheck solution-1888808c
  (fn [coll]
    (reduce
      (fn [x y] (let [y-count (count y)] (if (or (< y-count 2) (> (count x) y-count)) x y)))
      []
      (reduce
        (fn [subseqs item]
          (let [first-vec (first subseqs)]
            (if-let [last-item (last first-vec)]
              (if (= (inc last-item) item)
                (cons (conj first-vec item) (rest subseqs))
                (cons [item] subseqs))
              [[item]])))
        nil
        coll))))

(defcheck solution-19683b39
  (fn longest-inc-seq
    ([coll] (longest-inc-seq [] [(first coll)] (rest coll)))
    ([lng curr [x & c]]
     (letfn [(select-longest
               [longest current]
               (if (and (< (count longest) (count current))
                        (< 1 (count current)))
                 current
                 longest))]
       (if (nil? x)
         (select-longest lng curr)
         (if (> x (last curr))
           (longest-inc-seq lng (conj curr x) c)
           (longest-inc-seq (select-longest lng curr) [x] c)))))))

(defcheck solution-1a39e16e
  (fn[xs]
    (last
      (sort-by count
        (filter #(or (> (count %) 1) (zero? (count %)))
          (reduce (fn [res x]
                    (if (or (empty? (last res)) (not= (inc (last (last res))) x))
                      (concat res [[x]])
                      (concat (butlast res) [(conj (last res) x)])))
            [[]] xs))))))

(defcheck solution-1a700a14
  (fn longest-sequence- [coll]
    "53. Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers."
    (->> coll
      (partition 2 1) ; split into overlapping pairs
      (partition-by #(let [[l r] %] (> r l))) ; split into ascending and descending runs
      (filter #(let [[l r] (first %)] (> r l))) ; remove descending runs
      (map flatten) ; join ascending runs into a flat sequence of pairs
      (map #(concat (take-nth 2 %) (list (last %)))) ; remove the overlaps from the pairs
      (group-by count) ; map by length of run
      (sort) ; order by length
      (last) ; take the longest set of runs
      (second) ; take the runs themselves
      (first) ; take the first of the longest runs
      (vec) ; convert it to a vector
      )))

(defcheck solution-1a898223
  (fn [s]
    (let [
          subseqs (filter
                    #(not= 1 (count %))
                    (mapcat
                      (partial reductions conj [])
                      (tree-seq
                        (complement empty?)
                        (comp list rest) s)))
          inc? (fn [s] (or (empty? s) (= s (range (first s) (inc (last s))))))]
      (last (sort-by count (filter inc? subseqs))))))

(defcheck solution-1acd761e
  (fn [xs]
    (first
      (reduce
        (fn [[max-so-far cur-sub-seq] x]
          (cond (= x nil) [max-so-far nil]
                (= cur-sub-seq nil) [[] [x]]
                (> x (last cur-sub-seq)) (let [extended-cur (conj cur-sub-seq x)]
                                           [(max-key count extended-cur max-so-far)
                                            extended-cur])
                :else [max-so-far [x]]))
        nil xs))))

(defcheck solution-1b22673f
  (fn [[h & r :as xs]]
    (
     (fn ll [ [x & xs] best curr]
       (let [m (if (and (> (count curr) 1) (> (count curr) (count best))) curr best)]
         (cond
           (nil? x) m
           (> x (last curr)) (ll xs best (conj curr x))
           :else (ll xs m [x])
           )
         )
       )
     xs [] [h])
    ))

(defcheck solution-1b32e5fe
  (fn [nn]
    (let [state [0 [] 0 []]
          step (fn [[l0 ns0 l ns] n]
                 (cond
                   (empty? ns) [l0 ns0 1 [n]]
                   (> n (first ns)) [l0 ns0 (inc l) (cons n ns)]
                   (and (< l0 l) (< 1 (count ns))) [l ns 1 [n]]
                   :else [l0 ns0 1 [n]]))]
      (let [[l0 ns0 l ns] (reduce step state nn)]
        (if (and (< l0 l) (< 1 l)) (reverse ns) (reverse ns0))))))

(defcheck solution-1b638a2a
  (fn [c]
    (letfn [(increase [c]
              (lazy-seq
                (loop [s (seq c)
                       result '()
                       flag -1]
                  (cond
                    (empty? s) (cons (reverse result) '())
                    (<= (first s) flag) (cons (reverse result) (increase s))
                    :else (recur (next s) (cons (first s) result) (first s))))))]
      (let [result (reduce (fn [max e]
                             (if (< (count max) (count e)) e max))
                     (increase c))]
        (if (= (count result) 1)
          '()
          result)))))

(defcheck solution-1b8c33d6
  (fn [lst]
    ((reduce (fn [[f rv res] e]
               (let [newrv (conj rv e)]
                 [#(= % (inc e))
                  (if (f e) newrv [e])
                  (if (and (f e) (> (count newrv) 1) (> (count newrv) (count res)))
                    newrv
                    res)]))
       [#(not (nil? %)) [] []]
       lst) 2)))

(defcheck solution-1ba83aaf
  (fn [xs]
    (#(if (< (count %) 2) () %)
     (last
       (sort-by count
         (filter #(every? pos? (map - (rest %) (butlast %)) )
           (mapcat (comp (partial take-while #(not= % ())) (partial iterate rest))
             (take-while #(not= % nil) (iterate butlast xs)))
           ))))))

(defcheck solution-1bdeb51b
  (fn [ts]
    (let [ret (apply max-key count (reduce (fn [[cur max] e] (if (or (empty? cur) (< (last cur) e)) [(conj cur e) max] [[e] (if (> (count cur) (count max)) cur max)])) [[] []] ts))]
      (if (> (count ret) 1) ret []))))

(defcheck solution-1c12e9c2
  (fn [coll] (let [l (sort #(- (count %2) (count %1)) (reduce #(if (= (inc (last (last %1))) %2)
                                                                 (concat (butlast %1) (list (concat (last %1) (list %2))))
                                                                 (concat %1 (list (list %2))))
                                                        (list (list (first coll))) coll))]
               (if (>= (count (first l)) 2)
                 (first l)
                 []))))

(defcheck solution-1c4d16fe
  (fn [[x & xs]]
    (->> xs
      (reduce
        (fn [[best current] x]
          (if (> x (first current))
            (let [current' (cons x current)
                  best' (if (> (count current') (count best)) current' best)]
              [best' current'])
            [best [x]]))
        [[x] [x]])
      (#(if (< (count (first %)) 2) [] (first %)))
      (reverse))))

(defcheck solution-1d896aa6
  (fn [s]
    (->> s
      (reductions #(if (= (dec %2)(last %))
                     (conj % %2)
                     [%2]) [])
      (filter #(< 1 (count %)))
      (apply max-key count []))))

(defcheck solution-1e2d523f
  (fn f [coll]
    (let [bigger? (fn [pair] (< (first pair) (second pair)))
          coll (partition 2 1 coll)
          coll (partition-by bigger? coll)
          coll (filter #(bigger? (first %)) coll)
          coll (sort-by count > coll)
          pairs  (first coll)]
      (distinct (apply concat pairs)))))

(defcheck solution-1e77fe01
  (fn [s]
    (loop [i 0, ls [], ts []]
      (if-not (< i (count s))
        (if (> (count ts) (max 1 (count ls)))
          ts
          ls
          )
        (if (> (nth s i) (last (concat [-1] ts)))
          (recur (inc i) ls (conj ts (nth s i)))
          (if (> (count ts) (max 1 (count ls)))
            (recur i ts [])
            (recur i ls [])
            )
          )
        )
      )
    ))

(defcheck solution-1e7d745a
  (fn [xs]
    (let [q (filter #(apply < (first %))
              (partition-by #(apply < %)
                (partition 2 1 xs)))]
      (if (empty? q) []
                     (#(cons (ffirst %) (map second %))
                      (first
                        (sort-by count > q)))))))

(defcheck solution-1e97ad28
  (fn longest-subseq [coll]
    (let [res (->> (reduce (fn [res nxt]
                             (if (= 1 (- nxt (peek (peek res))))
                               (conj (pop res) (conj (peek res) nxt))
                               (conj res [nxt])))
                     [[(first coll)]] (rest coll))
                (apply max-key count))]
      (if (> (count res) 1) res []))))

(defcheck solution-1ed027ac
  (fn lis[input]
    (let [li (first (last (partition-by count (sort-by count (filter #(> (count %) 1)
                                                               (partition-by #(= \# %)
                                                                 ((fn chop-chop[out in]
                                                                    (cond
                                                                      (empty? in) out
                                                                      (or
                                                                       (nil? (last out))
                                                                       (> (first in) (last out))) (chop-chop (conj out (first in)) (rest in))
                                                                      :else (chop-chop (conj (conj out \#) (first in)) (rest in))))
                                                                  [] input)))))))]
      (if (nil? li) [] li))))

(defcheck solution-1f1b4560
  (fn longest-seq [input]
    (let [sub-sequences
               (fn sub-sequences [coll]
                 (loop [previous nil
                        current-seq []
                        coll coll]
                   (let [next (first coll)]
                     (cond
                       (nil? next) (cons current-seq nil)
                       (= previous (dec next))
                       (recur next (conj current-seq next) (vec (rest coll)))
                       (seq current-seq)
                       (cons
                         current-seq
                         (lazy-seq (sub-sequences coll)))
                       :else (recur next [next] (vec (rest coll)))))))
          best (last (sort-by count (sub-sequences input)))]
      (if (< 1 (count best))
        best
        []))))

(defcheck solution-1f517551
  (fn longest-increasing-sub-seq [xs]
    (->> (map vector xs (concat (drop 1 xs) (repeat -2147483648)))
      (partition-by (fn [[a b]] (< a b)))
      (filter (fn [[[a b] & more]] (< a b)))
      (map (fn [[[x _] & more :as pairs]] (cons x (map last pairs))))
      (reduce (fn [x y] (if (>= (count x) (count y)) x y)) []))))

(defcheck solution-1f96963c
  (fn liss
    ([coll] (if (empty? coll) [] (liss (rest coll) [] 0 (vector (first coll)) 1 (first coll))))
    ([coll bestseq bestlen actseq actlen lastelem]
     (cond
       (empty? coll)
       (let [seq (if (> actlen bestlen) actseq bestseq)
             len (count seq)]
         (if (> len 1) seq []))
       (< lastelem (first coll))
       (liss (rest coll) bestseq bestlen (conj actseq (first coll)) (inc actlen) (first coll))
       true
       (liss (rest coll)
         (if (> actlen bestlen) actseq bestseq)
         (max actlen bestlen)
         (vector (first coll))
         1
         (first coll)
         )))))

(defcheck solution-1fb478d6
  (fn longest-increasing-subseq [coll]
    (letfn [
            (longest [colls]
              (if (empty? colls)
                []
                (last (sort-by count (reverse colls)))))

            (increasing-subseqs [coll]
              (remove empty?
                (map (partial remove coll?)
                  (partition-by
                    #(and (coll? %) (apply >= %))
                    (interleave coll
                      (partition 2 1 [-2147483648] coll))))))
            ]
      (longest
        (remove #(< (count %) 2)
          (increasing-subseqs coll))))))

(defcheck solution-1fd0b8d
  (fn [col] (if (empty? col)
              []
              (let [res (apply max-key count (reverse ((fn [acc col]
                                                         (if (empty? col)
                                                           acc
                                                           (let [cur (last acc) a (last cur) b (first col)]
                                                             (if (< a b)
                                                               (recur (assoc acc (dec (count acc)) (conj cur b)) (rest col))
                                                               (recur (conj acc [b]) (rest col))
                                                               ))))
                                                       [[(first col)]] (rest col))))]
                (if (> (count res) 1) res [])))))

(defcheck solution-20279d90
  (fn [s]
    (let [succ (fn [[a b]] (= b (inc a)))
          r
               (last (sort-by count
                       (remove #(or (< (count %) 1)
                                    (not (succ (first %))))
                         (partition-by
                           succ
                           (map vector s (drop 1 s))))))]
      (concat (map first r) (rest (last r))))))

(defcheck solution-2081698d
  (fn[x]
    (loop [l x v1 [] v2 []]
      (if (empty? l)
        (let [ret (first (sort #(> (count %1) (count %2)) (conj v1 v2)))]
          (if (> (count ret) 1)
            ret
            []))
        (if (empty? v2)
          (recur (rest l) v1 (conj v2 (first l)))
          (if (= (first l) (+ 1 (last v2)))
            (recur (rest l) v1 (conj v2 (first l)))
            (recur (rest l) (conj v1 v2) (vector (first l)))))))))

(defcheck solution-20a63199
  (fn [s]
    (letfn [(consec-subseqs [[h & t :as s] cand result]
              (cond
                (empty? s) (conj result cand)
                (empty? cand) (recur t (vector h) result)
                (= 1 (- h (last cand))) (recur t (conj cand h) result)
                (not (= 1 (- h (last cand)))) (recur t [h] (conj result cand))))
            (longest-seq-at-least-2 [[ h & t :as s] cand]
              (cond
                (empty? s) cand
                (and (> (count h) 1) (> (count h) (count cand))) (recur t h)
                :else (recur t cand)))]
      (longest-seq-at-least-2 (consec-subseqs s [] []) []))
    ))

(defcheck solution-2152edf
  (fn sub[s]
    (let [subb (fn [s curs maxs]
                 (let [x (first s)
                       r (rest s)
                       ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x) [x])
                       nmax (max-key count ncurs maxs)]
                   (if (seq r)
                     (recur r ncurs nmax)
                     nmax)))
          longest (subb s [(first s)] [])]
      (if (> (count longest) 1) longest []))))

(defcheck solution-21637729
  (fn [[h & t :as s]]
    (->>
      (map #(list (- % %2) %2 %) t s)
      (partition-by (comp pos? first))
      (sort-by (comp - count))
      (filter (comp pos? ffirst))
      first
      (mapcat next)
      distinct)))

(defcheck solution-21752b16
  (fn f [[ft & ot]]
    (loop [ret []
           tmp [ft]
           [f & o] ot]
      (if (nil? f)
        (if (> (count ret) 1) ret [])
        (if (> f (last tmp))
          (let [x (conj tmp f)]
            (if (> (count x) (count ret))
              (recur x x o)
              (recur ret x o)))
          (recur ret [f] o))))))

(defcheck solution-21c8505
  (fn [s] (->> (partition 2 1 s)
            (partition-by #(apply < %))
            (filter #(< (ffirst %) (second (first %))))
            (reduce #(if (< (count %1) (count %2)) %2 %1 ) [])
            (#(if (empty? %) % (cons (ffirst %) (map second %)))))))

(defcheck solution-2218a2f2
  (fn [xs]
    (letfn [(fr [best temp ys]
              (if (first ys)
                (if (= 0 (count temp) )
                  (recur best [(first ys)] (rest ys))
                  (let [y1 (first ys) rs (rest ys) lt (last temp) tlen (count temp) blen (count best)]
                    (if (= y1 (inc lt))
                      (recur best (conj temp y1) rs)
                      (if (> tlen blen)
                        (recur temp [] ys)
                        (recur best [] ys)
                        )
                      )
                    )
                  )
                (if (> (count best) (count temp))
                  (if (> (count best) 1)
                    best
                    []
                    )
                  (if (> (count temp) 1)
                    temp
                    []
                    )

                  )
                )
              )]
      (fr [] [] xs)
      )
    ))

(defcheck solution-228d46e2
  (fn [coll]
    (or (first
          (sort-by count >
            (filter #(apply < %)
              (let [cnt (count coll)]
                (for [i (range cnt)
                      j (range (inc i) cnt)]
                  (drop i (take (inc j) coll)))))
            )
          )
        '())
    ))

(defcheck solution-2371da34
  (fn [coll]
    (let [monotones (partition-by #(apply < %) (partition 2 1 coll))
          increases (filter #(apply < (first %)) monotones)]
      (if (empty? increases) []
                             (let [by-count (group-by count increases)
                                   best (first (by-count (apply max (keys by-count))))]
                               (concat (map first best) [(last (last best))]))))))

(defcheck solution-23a6cbf8
  (fn [xs]
    (loop [longest []
           current []
           remaining xs]
      (if (seq remaining)
        (let [x (first remaining)]
          (cond (or (empty? current) (> x (last current)))
                (recur longest (conj current x) (rest remaining))
                (> (count current) (count longest))
                (recur current [x] (rest remaining))
                :else (recur longest [x] (rest remaining))))
        (cond (> (count current) (count longest)) current
              (> (count longest) 1) longest
              :else [])))))

(defcheck solution-24bc24f2
  (fn [sq]
    (let [f
                 (fn f
                   ([sq] (f (rest sq) (list (first sq)) (list (first sq))))
                   ([sq acc lns]
                    (let [longest #(if (> (count %1) (count %2)) %1 %2)]
                      (if (empty? sq)
                        lns
                        (let [nacc (if (> (first sq) (first acc))
                                     (cons (first sq) acc)
                                     (list (first sq)))]
                          (f (rest sq) nacc (longest nacc lns))
                          )))))
          result (f sq)]
      (if (= 1 (count result)) '() (reverse result))
      )))

(defcheck solution-24c634e0
  (fn rl5 [s]
    (last (map #(if (= 1 (count %)) () %)
            (sort-by count
              ((fn sp2 [s & [i new_s tiny]]
                 (cond (= i (count s)) (concat new_s (list tiny))
                       (and (not (nil? i)) (not= 1 (- (nth s i) (nth s (dec i)))))
                       (sp2 s (inc i) (concat new_s (list tiny)) (list (nth s i)))
                       (nil? i) (sp2 s 1 '() (list (first s)))
                       :else (sp2 s (inc i) new_s (concat tiny (list (nth s i)))))) s))))))

(defcheck solution-24cb9f7e
  (fn [coll]
    (loop [lastidx 0
           lastval (first coll)
           subcoll (rest coll)
           longest 1
           lidx 0
           current 1
           cidx 0]

      (if (empty? subcoll) (if (> longest 1) (take longest (drop lidx coll)) [])
                           (recur
                             (inc lastidx)
                             (first subcoll)
                             (rest subcoll)
                             (if (and (< lastval (first subcoll)) (= current longest)) (inc current) longest)
                             (if (and (< lastval (first subcoll)) (= current longest)) cidx lidx)
                             (if (< lastval (first subcoll)) (inc current) 1)
                             (if (< lastval (first subcoll)) cidx (inc lastidx))
                             )
                           ))))

(defcheck solution-24cbf69d
  (fn sub-seq [ns]
    (let [pairs (partition 2 1 ns)]
      (loop [an () ps pairs]
        (if (empty? ps)
          (let [ns (map first (butlast an))]
            (concat ns (last an)))
          (let [[fs rs] (split-with
                          #(< (first %) (second %)) ps)]
            (if (< (count an) (count fs))
              (recur fs (rest rs))
              (recur an (rest rs)))))))))

(defcheck solution-24d023ff
  (fn longest-increasing-subseq [coll]
    (letfn [(suffixes [coll] (take-while not-empty (iterate rest coll)))
            (lis-from-head [coll]
              (loop [acc  (list (first coll))
                     coll (rest coll)]
                (if (or (empty? coll) (>= (first acc) (first coll)))
                  (if (< (count acc) 2) [] (reverse acc))
                  (recur (cons (first coll) acc) (rest coll)))))]
      (->> coll
        suffixes
        (map lis-from-head)
        (sort-by count >)
        first))))

(defcheck solution-24dd1394
  (fn [ns]
    (->> ns
      (iterate rest)
      (take-while seq)
      (map (partial reductions (fn [last-in-streak n]
                                 (when (= last-in-streak (dec n))
                                   n))))
      (map (partial take-while identity))
      (remove #(= (count %) 1))
      (reduce (partial max-key count) []))))

(defcheck solution-24f90167
  (fn lll [v]
    (loop [acc [] cmp [(first v)] rst (rest v)]
      (if (empty? rst)
        (if (> (count cmp)
              (count acc))
          cmp
          (if (= 1 (count acc))
            []
            acc))
        (let [head (first rst)
              tail (last cmp)]
          (if (> head tail)
            (recur acc (conj cmp head) (rest rst))
            (if (> (count cmp)
                  (count acc))
              (recur cmp [head] (rest rst))
              (recur acc [head] (rest rst)))))))))

(defcheck solution-258b79c0
  (fn l-sub-seq-- [coll]
    (reduce #(let [c (count %2)
                   s (sort %2)]
               (if (and (> c 1)
                        (> c (count %1))
                        (and (= %2 s)
                             (= s (distinct s))))
                 %2
                 %1))
      '() (mapcat #(take (count %1) (iterate butlast %1))
            (take (count coll) (iterate rest coll))))))

(defcheck solution-25a07257
  #(last (sort-by count ((fn p60
                           [coll]
                           (if (seq coll)
                             (conj (p60 (rest coll))
                               (reverse ((fn drill
                                           [[a & coll] x]
                                           (if (= (inc a) (first coll))
                                             (conj (drill coll (conj x a)) a)
                                             (if (first x)
                                               [a]
                                               []))) coll [])))
                             []))%))))

(defcheck solution-25d3c5ff
  (fn longest [xs]
    (let [monotonic?
          (fn [[a b]] (= a (- b 1)))
          monotonics
          (->> xs
            (partition 2 1)
            (partition-by monotonic?)
            (map (juxt (comp monotonic? first) count identity))
            (filter (comp true? first)))
          gin
          (if (seq monotonics)
            (->> monotonics
              (filter (comp (partial = (apply max (map second monotonics))) second))
              first
              last)
            monotonics)]
      (if (seq gin)
        (range (first (first gin)) (inc (last (last gin))))
        gin))))

(defcheck solution-261b3749
  (fn myIncreasingSequence
    [coll]
    (let [min (- (apply min coll) 1)]
      (let [result (reduce #(if (>= (count %1) (count %2) 1) %1 %2) []
                     (partition-by #(= min %)(reduce #(if (<= %2 (last %1)) (conj %1 min %2) (conj %1 %2)) (vector (first coll)) (rest coll))))]
        (if (>= 1 (count result)) [] result)))))

(defcheck solution-263438dc
  (fn longest-increasing-sub-seq [x]
    (or
     (first
       (sort-by count >
         (for [a (range (count x))
               b (range (+ a 2) (inc (count x)))
               :let [s (subvec x a b)]
               :when (apply < s)]
           s))) [])))

(defcheck solution-264339f9
  (fn [xs]
    (->> (repeat (count xs) xs)
      (map-indexed #(drop %1 %2))
      (mapcat #(reductions conj [] %))
      (filter #(<= 2 (count %)))
      (filter #(= % (take (count %) (iterate inc (first %)))))
      (sort-by count)
      (last)
      (vec))))

(defcheck solution-268e1e15
  (fn [l]
    (reduce
      (fn [r, l]
        (if (>= (count r) (count l)) r l)
        )
      []
      (remove #(= 1 (count %))
        (reduce
          (fn [ac, v]
            (if
             (<= v (last (last ac)))
              (conj ac (vector v))
              (conj (vec (butlast ac)) (conj (last ac) v))
              ))
          (vector (vector (first l)))
          (rest l)
          )))))

(defcheck solution-2701abcb
  (fn me [v]

    (let [
          m-fn (fn  [my-v]

                 (let [
                       r-fn (fn [res arg]

                              (if (empty? (first res))

                                [(concat (first res) [arg]) (concat (second res) [arg])]

                                (if (and (>= (count (second res)) 1)  (= 1 (- arg (last (first res)))))
                                  [(concat (first res) [arg]) (concat (second res) [arg])]
                                  [(first res) []]
                                  )
                                )

                              )
                       ]

                   (first (reduce r-fn [[] []] my-v))

                   )
                 )

          vv (take (count v) (iterate #(rest %) v) )

          res  (last (sort-by count (map m-fn vv)))

          ]



      (if (<= (count res) 1)
        []
        res
        )

      )

    ))

(defcheck solution-273d67b0
  (fn [l]
    (let [[a r] (reduce
                  #(let [[a r] %]
                     (cond (empty? a)
                           [[%2] r]
                           (< (last a) %2)
                           [(conj a %2) r]
                           true
                           [[%2] (conj r a)])) [[][]] l)
          r (remove #(= 1 (count %)) (conj r a))
          ne (not (empty? r))
          ml (if ne (apply max (map count r)))]
      (if ne
        (some #(if (= ml (count %)) %) r) []))))

(defcheck solution-277dd67a
  (fn [bl]
    (apply max-key count '()
      (filter #(> (count %) 1)
        (reductions
          (fn [l i]
            (if (= (last l) (dec i))
              (concat l (list i))
              (list i)))
          '() bl)))))

(defcheck solution-28457e53
  (fn [coll]
    (or (first (filter #(apply < %) (mapcat #(partition % 1 coll) (range (count coll) 1 -1 ))))
        [])))

(defcheck solution-285bca24
  (fn  [l] (->> l
             (map  #(list (- %2 %) %2) (range))
             (partition-by first)
             (filter #(> (count %) 1))
             (apply max-key count [])
             (map second)
             )))

(defcheck solution-2988a904
  (fn [s]
    (let [prev (atom nil)
          marker (atom 0)
          liss
          (last (sort-by count
                  (partition-by
                    #(if (or (nil? @prev) (= % (inc @prev)))
                       (do
                         (swap! prev (constantly %))
                         @marker)
                       (do
                         (swap! prev (constantly %))
                         (swap! marker inc)))
                    s)))]
      (if (< (count liss) 2) [] liss))))

(defcheck solution-29aea538
  (fn
    [cs]
    (let [ps (partition 2 1 cs)
          f (fn [[a b]] (> (- b a) 0))
          getps (fn [ps]
                  (->> ps
                    (take-while f)))
          qs (loop [rs [], p ps]
               (let [p (drop-while (complement f) p)]
                 (if (seq p)
                   (recur
                     (conj rs (getps p))
                     (drop-while f p))
                   rs)))]
      (loop [qs qs, rs nil]
        (if (seq qs)
          (if (> (count (first qs)) (count rs))
            (recur (rest qs) (first qs))
            (recur (rest qs) rs))
          (if (seq rs)
            (conj (vec (map first rs)) (last (last rs)) )
            []))))))
(defcheck solution-2a3dcfd4
  (fn solve [a]
    (let [max-subseq (fn [a]
                       (reduce (fn [res el]
                                 (let [but-first-seq (rest res)
                                       first-seq     (first res)
                                       first-el      (first first-seq)]
                                   (if (or (nil? first-el) (< first-el el))
                                     (conj but-first-seq (conj first-seq el))
                                     (conj res (list el))))) '(()) a))
          get-max (fn [a]
                    (let [sizes    (map count a)
                          max-size (apply max sizes)]
                      (if (<= max-size 1) []
                                          (->> a
                                            (filter #(= (count %) max-size))
                                            (first)))))
          seqs (max-subseq a)]
      (reverse (get-max (reverse seqs))))))

(defcheck solution-2b6670ee
  (fn prob53 [s]
    (->> (partition 2 1 s)
      (partition-by (fn [[p1 p2]] (> p2 p1)))   ;; grouped by results of function

      ;; ( ((1 0)) ((0 1) (1 2) (2 3)) ((3 0)) ((0 4) (4 5)) )

      ;; pull out the groups into new sequences
      ;; a. map first of each element into a vector gets the first of teach subgroup
      ;;  ([1] [0 1 2] [3] [0 4])
      ;; b. need to pick up the very last item
      ;; ([1 0] [0 1 2 3] [3 0] [0 4 5])
      (map
        #(conj
           (into [] (map first %)) (last (last %)))
        )

      (filter (fn [[a b]] (> b a)))    ;; pick the ones with increasing vlaues
      ;; ([0 1 2 3] [0 4 5])

      (reduce (fn [a b] (if (>  (count b) (count a)) b a)) [])  ;; pick the longest
      ;; [0 1 2 3]
      )))

(defcheck solution-2ba61dc3
  (fn lis [l]
    (let [r (reduce (fn [v e]
                      (let [e1 (v :e)]
                        (if (> e e1)
                          (-> v (assoc :e e) (assoc :l (conj (v :l) e)) (assoc :n (inc (v :n))))
                          (if (> (v :n) (v :s))
                            (-> v (assoc :s (v :n)) (assoc :b (v :l)) (assoc :e e) (assoc :l (list e)) (assoc :n 1))
                            (-> v (assoc :e e) (assoc :l (list e)) (assoc :n 1))
                            )
                          )
                        )
                      )
              {:s 0 :b '() :n 1 :e (first l) :l (take 1 l)} (rest l)) s (r :s) n (r :n)]
      (if (= (max n s) 1)
        '()
        (reverse
          (if (> n s)
            (r :l)
            (r :b)
            )
          )
        )
      )
    ))

(defcheck solution-2c48190d
  (fn [c]
    (->> (partition 2 1 c)
      (partition-by #(< (first %) (second %)))
      (map #(concat (first %) (mapcat (fn [[a b]] [b]) (rest %))))
      (filter #(and (> (count %) 1) (< (first %) (second %))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) ()))))

(defcheck solution-2ccc654d
  (fn part [coll]
    (let [result-to-list
          (fn [{lst :lst lsts :lsts}] (conj lsts lst))
          reducer
          (fn [accum curr]
            (let [{prev :prev lst :lst lsts :lsts} accum]
              (if (> curr prev)
                (assoc accum :prev curr :lst (conj lst curr))
                (assoc accum :prev curr :lst (list curr) :lsts (conj lsts lst)))))]
      (->> (reduce reducer {:prev 0 :lst () :lsts ()} coll)
        (result-to-list)
        (filter #(> (count %) 1))
        (sort-by count)
        (last)
        (reverse)))))

(defcheck solution-2ce93b5d
  (fn [coll]
    (->>
      (partition 2 1 coll)
      (partition-by #(apply - %))
      (filter #(= -1 (apply - (first %))))
      (reduce #(max-key count % %2) '())
      flatten
      distinct
      )))

(defcheck solution-2d114248
  (fn fliss [v]
    (let [get-next (fn  [v i]
                     (let [c (count v)]
                       (loop [result []
                              ii i]
                         (let [ae (v ii)
                               ne (if (= c (inc ii)) nil (v (inc ii)))]
                           (if (and (not= ii c) (not= nil ne) (= ne (inc ae)))
                             (recur (conj result ae) (inc ii))
                             (conj result ae)
                             )))))
          find-next (fn  [v i]
                      (let [c (count v)]
                        (cond
                          (= (dec c) i) {:npos c, :subs []}
                          (not= 1 (- (v (inc i)) (v i))) {:npos (inc i), :subs []}
                          :else
                          (let [ss (get-next v i)]
                            {:npos (+ i (count ss)), :subs ss})
                          )))
          get-subsets (fn [v]
                        (loop [i 0, result []]
                          (let [found (find-next v i)]
                            (if (>= (found :npos) (count v))
                              (conj result (found :subs))
                              (recur (found :npos) (conj result (found :subs))))
                            )))
          subsets (get-subsets v)]
      (first (reverse (sort-by count subsets)))
      )))

(defcheck solution-2d13e1ff
  (fn [input]
    (let [seqs (map #(take (first %1) (drop (second %1) input))
                 (for [x (range (inc (count input)))
                       y (range (inc (count input)))]
                   [x y]))]
      (apply (partial max-key count []) (filter
                                          #(when (> (count %1) 1)
                                             (loop [i (dec (count %1)) ok true]
                                               (if (= i 0)
                                                 ok
                                                 (recur (dec i) (and ok (= (dec (nth %1 i)) (nth %1 (dec i))))))))
                                          seqs)))))

(defcheck solution-2d213e8
  (fn [coll]
    (let [increasing-sub-seqs
          (fn increasing-sub-seqs
            [current coll]
            (if (seq coll)
              (let [a (first coll)]
                (if (and current (= (peek current) (dec a)))
                  (increasing-sub-seqs (conj current a) (rest coll))
                  (if (and current (<= 2 (count current)))
                    (cons current (increasing-sub-seqs [a] (rest coll)))
                    (increasing-sub-seqs [a] (rest coll)))))
              (when (and current (<= 2 (count current)))
                (cons current nil))))]
      (reduce
        (fn [r x]
          (if (> (count x) (count r))
            x r))
        []
        (increasing-sub-seqs nil coll)))))

(defcheck solution-2d57217b
  (fn h [s]
    (letfn [
            (f [[a b & r]]
              (cons a
                (when (and b (< a b))
                  (f (cons b r)))))
            (g [s]
              (when (seq s)
                (let [a (f s)]
                  (cons a
                    (g (nthrest s (count a))))
                  )))]
      (let [x (first (sort-by #(- 0 (count %)) (g s)))]
        (if (> (count x) 1)
          x
          [])))))

(defcheck solution-2dc29bfe
  (fn [ls]
    (loop [ls ls
           part (list (first ls))
           ans [[]]]
      (if (<= (count ls) 1)
        (reverse (last (sort-by count (filter #(not= (count %) 1) (cons part ans)))))
        (if (< (first ls) (second ls))
          (recur (rest ls) (cons (second ls) part) ans)
          (recur (rest ls) (list (second ls)) (cons part ans)))))))

(defcheck solution-2e54d6c2
  (fn [coll]
    (->> (map vector coll (range))
      (partition-by #(apply - %))
      (map #(map first %))
      (filter #(> (count %) 1))
      (sort-by (comp - count))
      first
      vec)))

(defcheck solution-2e8c5824
  (fn [xs]
    (or (first (filter #(> (count %) 1) (sort-by count > (reductions
                                                           (fn [x y]
                                                             (if (> y (last x))
                                                               (conj x y) [y])) [(first xs)] (rest xs))))) [])))

(defcheck solution-2e8d1503
  (fn [q] (into [] (mapcat vector (first (take 1 (filter #(= (sort (distinct %)) %)(for [[i sz] (for [sz (reverse (range 2 (inc (count q))))
                                                                                                      i (range 0 (inc ( - (count q) sz)))]
                                                                                                  [i sz])]
                                                                                     (subvec q i (+ i sz))))))))))

(defcheck solution-2eaab064
  (fn [l]
    ((fn [[h & t] best current]
       (if h
         (let [c (if (or (not (last current)) (> h (last current))) (conj current h) [h])
               best (if (> (count c) (count best)) c best)]
           (recur t best c))
         (if (> (count best) 1) best []))) l [] [])))

(defcheck solution-2ec262f3
  (fn [s]
    (let [r (second
              (reduce (fn [[cur-lcs lcs] cur]
                        (let [new-lcs (conj cur-lcs cur)]
                          (if (empty? cur-lcs)
                            [new-lcs new-lcs]
                            (if (> cur (last cur-lcs))
                              (if (> (count new-lcs) (count lcs))
                                [new-lcs new-lcs]
                                [new-lcs lcs])
                              [[cur] lcs])))) [[] []] s))]
      (if (< (count r) 2) [] r))))

(defcheck solution-2ee53996
  #(->> %
     (partition 2 1)
     (partition-by (partial apply <))
     (keep (fn [[[a b :as f] & r]] (when (> b a) (concat f (map second r) ))))
     (group-by count)
     (cons [0 [[]]])
     (apply max-key first) second first))

(defcheck solution-2f7e9939
  (fn LI-subseq [x]
    (loop [current (vector (first x))
           rst (rest x)
           longest current]
      (if (empty? rst)
        (do #_(prn current longest)
            (if (> (count current) (count longest)) current
                                                    (if (>= (count longest) 2) longest [])))
        (if (< (last current) (first rst)) (recur (conj current (first rst)) (rest rst) longest)
                                           (do #_(prn current longest)
                                               (recur (vector (first rst)) (rest rst) (if (> (count current) (count longest)) current longest))))))))

(defcheck solution-2f919214
  (fn longest-incr-subseq [xs]
    (->> (range (count xs) 1 -1)
      (mapcat #(partition % 1 xs))
      (filter #(->> (apply sorted-set %)
                 seq (= %)))
      first vec)))

(defcheck solution-2fac089a
  (fn [s] (map fnext (last (filter #(< 1 (count %)) (sort-by count (partition-by first (map #(or [(- % %2) %]) s (range)))))))))

(defcheck solution-2fed174b
  ;(fn [in]
  ;    (let [pairs (partition-all 2 1 in)
  ;          find-incs (for [[a b] pairs] [a b (= (inc a) b)])
  ;          find-mono (partition-by #(nth % 2) find-incs)
  ;          mono-only (filter #(nth (first %) 2) find-mono)
  ;          lengthmap (group-by count mono-only)
  ;          longest (if-not (seq lengthmap) 0 (apply max (keys lengthmap)))
  ;          best (first (lengthmap longest))]
  ;      (if-not best [] (cons (ffirst best) (map second best)))))

  (fn [s] (let [deltas (map (fn [[a b]] [(- b a) a b]) (partition 2 1 s))
                series (partition-by first deltas)
                longest-length (->> series
                                 (group-by count)
                                 keys
                                 (apply max))
                desired-sequence (->> series
                                   (filter #(= (count %) longest-length))
                                   first)
                start (second (first desired-sequence))
                end (int (nth (last desired-sequence) 2))]
            (range start (inc end)))))

(defcheck solution-3038875f
  (fn [n]
    (let [seqs  (reduce (fn [[[f & r :as f_l]
                              & rr  :as t_l] n]
                          (if (= 1 (- n (if f f 0)))
                            (cons (cons n f_l) rr)
                            (cons (list n) t_l)
                            )) '(()) n)

          candi (reverse (apply (partial max-key count) seqs ))

          ]

      (if (>= (count candi) 2) candi [])
      )))

(defcheck solution-3067e7fb
  (fn [x]
    (let [a (->>
              (map vector x (rest x))
              (partition-by #(< (first %) (second %)))
              (reverse)
              (apply max-key count))
          r (concat (map first a) [(-> a last second)])]
      (if (< (first r) (second r)) r '()))))

(defcheck solution-311aaa45
  (fn [s]
    (let [g
          (filter (fn [[x y]] (< x y))
            (reduce #(if (< (count %) (count %2)) %2 %)
              (partition-by
                (fn [[a b]] (< a b))
                (partition 2 1 s))))]
      (if (empty? g)
        []
        (into [(first (first g))] (map (fn [[a b]] b) g))))))

(defcheck solution-3142be8a
  (fn [s]
    (loop [longest [] current [] l s]
      (if (seq l)
        (if (or (empty? current)
                (= (inc (last current)) (first l)))
          (if (and (>= (count current) (count longest))
                   (>= (count current) 1))
            (recur (conj current (first l)) (conj current (first l)) (rest l))
            (recur longest (conj current (first l)) (rest l)))
          (recur longest [(first l)] (rest l)))
        longest))))

(defcheck solution-3183deb1
  (fn lisq
    ([coll]
     (lisq coll (count coll)))
    ([coll n]
     (let [pc (filter #(= (distinct (sort %)) %) (partition n 1 coll))]
       (if (empty? pc)
         (lisq coll (dec n))
         (if (= 1 (count (first pc)))
           '()
           (first pc)))))))

(defcheck solution-325b9c85
  (fn [l] (reverse (reduce #(if (>= (count %1) (count %2)) %1 %2) '() (filter #(< 1 (count %))
                                                                        ((fn increasing-subseq [l]
                                                                           (loop [s '() r l p -1000]
                                                                             (cond
                                                                               (empty? r)      (list s)
                                                                               (> (first r) p) (recur (cons (first r) s) (rest r) (first r))
                                                                               :else           (cons s (increasing-subseq r))))) l))))))

(defcheck solution-326c14e3
  (fn longest-inc-subseq [coll]
    (reduce #(let [len-xs (count %)
                   len-x (count %2)]
               (if (and (< len-xs len-x) (< 1 len-x)) %2 %))
      []
      (reductions (fn [xs x]
                    (if (> x (last xs)) (conj xs x) [x])) (cons [(first coll)] (rest coll))))))

(defcheck solution-3287468a
  (fn longest-sub-seq [xs]
    (reduce
      (fn [longest next-seq]
        (if (< (count longest) (count next-seq))
          next-seq
          longest))
      (list)
      (filter (fn [xxs]
                (every? #(< (first %) (second %))
                  (partition 2 1 xxs)))
        ((fn all-sub-sequences [xxs]
           (apply concat
             (map #(partition % 1 xxs)
               (range 2 (+ (count xxs) 1)))))
         xs)))))

(defcheck solution-32f9a4dc
  (fn [s]
    (let [tear (
                (fn runner [[f & n]]
                  (if (empty? n)
                    (list (list f))
                    (let [[fseq & nseq] (runner n)
                          fr  			(first fseq)]
                      (if (= (inc f) fr)
                        (cons (cons f fseq) nseq)
                        (cons (list f) (cons fseq nseq))))))
                s)
          max-count (reduce #(if (> (count %1) (count %2)) %1 %2) tear)]
      (if (> (count max-count) 1) max-count []))))

(defcheck solution-3309fbf4
  (comp #(if (nil? %) [] %)
        first
        #(sort-by count > %)
        #(letfn [(inc-ord [[e1 e2]] (< e1 e2))]
           (loop [pairseq (map vector % (rest %))
                  subseqs []]
             (if (empty? pairseq)
               subseqs
               (let [ord-seq (take-while inc-ord pairseq)
                     size (count ord-seq)]
                 (if (zero? size)
                   (recur (rest pairseq) subseqs)
                   (recur (drop size pairseq)
                     (conj subseqs
                       (cons (first (first ord-seq))
                         (map last ord-seq)))))))))))

(defcheck solution-3319c50a
  (fn [c]
    (->> (partition 2 1 c)
      (partition-by #(- (second %) (first %)))
      (filter #(= 1 (- (second (first %)) (ffirst %))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) [])
      (flatten)
      (distinct))))

(defcheck solution-331e2184
  (fn solve [s]
    (apply max-key
      count
      (filter #(not= (count %) 1)
        (reduce (fn [seqs x]
                  (let [l (last seqs)]
                    (if (and l (= (last l) (dec x)))
                      (conj (vec (butlast seqs))
                        (conj l x))
                      (conj seqs [x]))))
          [[]]
          s)))))

(defcheck solution-333ded3d
  #(nth
     (reduce
       (fn [[v m] e]
         (let [n {e (if (m (dec e))
                      (conj (m (dec e)) e)
                      [e])}]
           [(let [f (first (vals n))
                  c (count f)]
              (if (and (<= 2 c) (< (count v) c)) f v))
            n]))
       [[] {}]
       %)
     0))

(defcheck solution-335080ad
  (fn [[x & xs]]
    (let [res
          (->>
            (reductions
              (fn [res curr]
                (if (< (first res) curr)
                  (conj res curr)
                  (list curr)))
              (list x)
              xs)
            reverse
            (apply max-key count)
            reverse)]
      (if (> (count res) 1)
        res
        '()))))

(defcheck solution-33c8608e
  (fn [s]
    (loop [ss (rest s) largest [] working [(first s)]]
      (let [cw (count working) cl (count largest)]
        (if (empty? ss)
          (if (and (> cw 1) (> cw cl))
            working
            largest)
          (let [n (first ss)]
            (recur (rest ss)
              (if (and (> cw 1) (> cw cl))
                working
                largest)
              (if (> n (last working)) (conj working n) [n]))))))))

(defcheck solution-340b8d14
  (fn [coll]
    (loop [c        coll
           last     -1
           longest  []
           thisone  longest]
      (if (empty? c)
        (if (> (count longest) 1) longest [])
        (let [t (first c)]
          (if (> t last)
            (let  [lengthen (conj thisone t)]
              (if (> (count lengthen) (count longest))
                (recur (rest c) t lengthen lengthen)
                (recur (rest c) t longest lengthen)))
            (recur (rest c) t longest [t])))))))

(defcheck solution-345eac2d
  (fn [lst] (last (sort-by count
                    ((fn lis [xs] (concat
                                   (let [re []]
                                     (for [lg (range (count xs) 1 -1)]
                                       (if (= (take lg xs) (range (first xs) (+ (first xs) lg)))
                                         (take lg xs)
                                         [])))
                                   (if (not (empty? (rest xs))) (lis (rest xs))))
                       ) lst)))))

(defcheck solution-3477fd79
  (fn [ns]
    (let [
          reds
              (reductions
                (fn [coll it]
                  (if (or (empty? coll) (> it (last coll)))
                    (conj coll it)
                    [it]))
                []
                ns)
          n (apply max (map count reds))
          ans (first (filter #(= n (count %)) reds))]
      (if (> (count ans) 1) ans []))))

(defcheck solution-347c2566
  (fn liss [s]
    ((fn liss-sub [[s & ss] acc cur]
       (let [new-acc
             (if (> (count cur) (count acc)) cur acc)]
         (if (nil? s)
           (if (> (count new-acc) 1) new-acc [])
           (if (or (empty? cur) (not (= s (inc (last cur)))))
             (liss-sub ss new-acc [s])
             (liss-sub ss new-acc (conj cur s)))))) s [] [])))

(defcheck solution-34d581ae
  (fn longest-increase [coll]
    (loop [c (rest coll) previous-item (first coll) longest '() current-coll (cons (first coll) '())]
      (if (empty? c)
        (into [](reverse longest))
        (let [current-item (first c)
              increasing (> current-item previous-item)
              cons-coll (if increasing (cons current-item current-coll) (cons current-item '()))
              new-longest (if (and increasing (> (count cons-coll) (count longest))) cons-coll longest)]
          (recur (rest c) current-item new-longest cons-coll))))))

(defcheck solution-34f6e9e2
  (fn [sequence]
    (loop [s (next sequence)
           m (list (first sequence))
           cur (list (first sequence))]
      (if s
        (if (> (first s) (first cur))
          (let [cur (conj cur (first s))]
            (if (> (count cur) (count m))
              (recur (next s) cur cur)
              (recur (next s) m cur)))
          (recur (next s) m (list (first s))))
        (if (> (count m) 1)
          (reverse m)
          '())))))

(defcheck solution-3575cd99
  (fn longest-increasing-subseq
    [input]
    (let [result (first (reduce (fn [[best current] next]
                                  (let [new-current (if (> next (last current))
                                                      (conj current next)
                                                      [next])]
                                    (let [new-best (if (> (count new-current) (count best))
                                                     new-current
                                                     best)]
                                      [new-best new-current])))
                          [[(first input)] [(first input)]]
                          (into [] (rest input))))]
      (if (= (count result) 1) [] result))))

(defcheck solution-35ae843d
  (fn [c]
    (->> c
      (partition 2 1)
      (partition-by #(apply - %))
      (filter (fn [[[x y]]] (= (- y x) 1)))
      (sort-by count)
      last
      ((juxt #(map first (butlast %)) last))
      (apply concat))))

(defcheck solution-35d97e7d
  (fn [s]
    (let [seqs (for [i (range (dec (count s)))
                     j (range 2 (inc (- (count s) i)))
                     :let [r (take j (drop i s))]
                     :when (apply < r)]
                 r)]
      (if (= seqs [])
        []
        (apply max-key count (reverse seqs))))))

(defcheck solution-35e669d9
  #(let [f (fn [v] (loop [acc []
                          s v]
                     (let [[x & xs] s]
                       (if (or (empty? xs) (not= (inc x) (first xs)))
                         [(conj acc x) (vec xs)]
                         (recur (conj acc x) xs)))))]
     (loop [ans []
            lst %]
       (let [tmp (f lst)
             cand (first tmp)
             len (count cand)
             left (second tmp)
             best (if (and (> len 1) (> len (count ans))) cand ans)]
         (if (empty? left)
           best
           (recur best left))))))

(defcheck solution-3790f5ef
  (fn lsubseq [coll]
    (loop [coll coll longest '[] current '[] lastval -1000]
      (cond (empty? coll)
            (cond
              (= (count longest) 1) '[]
              (> (count longest) (count current)) longest
              :else current
              )
            (= (+ lastval 1) (first coll))
            (recur (rest coll) longest (conj current (first coll)) (first coll))
            :else
            (if (> (count longest) (count current))
              (recur (rest coll) longest (vector (first coll)) (first coll))
              (recur (rest coll) current (vector (first coll)) (first coll))
              )
            )
      )
    ))

(defcheck solution-379a469
  (fn [col]
    ((fn [l] (concat (first l) (map second (rest l))))
     (last
       (first
         (sort
           (map (fn [i l] [(- (count l)) i l])
             (range)
             (keep (fn [l] (when (apply < (first l)) l))
               (partition-by (partial apply <)
                 (partition 2 1 col))))))))))

(defcheck solution-385ef95a
  (fn[s]
    (->>
      ((fn f [[x & y] [u & v :as a] r]
         (letfn [(t[c]   (w (butlast r) c))
                 (w[p c] (conj (vec p) c))]
           (cond
             (nil? x)  r
             (nil? u) (f y [x] r)

             (= x (+ u (count a)))
             (f y (w a x) (t (w a x)))

             :else (f y [x] (w r a))))) s [] [[]])

      (remove #(=(count %)1))
      (sort-by count)
      (last))))

(defcheck solution-3884f626
  (fn f [l]
    (letfn [(increasing [[h & t]]
              (if (and t (< h (first t)))
                (cons h (increasing t))
                (list h)))]
      (let [longest (reduce
                      (fn [max cand] (if (< (count max) (count cand)) cand max))
                      (map increasing (take-while (comp not empty?) (iterate rest l))))]
        (if (> (count longest) 1) longest [])))))

(defcheck solution-38d1376d
  (fn long-seq* [s]
    (let [c (count s)
          m (for [i (range 1 c)]
              (for [j (range i c)
                    :while (< (nth s (dec j)) (nth s j))]
                (if (= i j)
                  [(nth s (dec j)) (nth s j)]
                  (nth s j))))]
      (vec (first
             (sort-by #(/ (count %))
               (filter #(pos? (count %))
                 (map flatten m))))))))

(defcheck solution-38f6b231
  (fn [v] (reduce
            (fn longer [a b] (if (> (count b) (count a)) b a))
            []
            (filter #(apply < %)
              (filter #(< 1 (count %))
                (for [n [(count v)] i (range 0 n) j (range (inc i) n)] (take (- (inc j) i) (drop i v))))))))

(defcheck solution-39737935
  (fn [xs]
    (reduce #(if (< (count %1) (count %2)) %2 %1)
      []
      (filter #(> (count %) 1)

        (map #(map first
                (take-while (fn [[a b]] (= a b))
                  (map vector % (iterate inc (first %)))))
          (take-while seq (iterate rest xs)))))))

(defcheck solution-39f88dc0
  (fn momo
    [sekuen]
    (loop [[x & xs] sekuen res []]
      (if (nil? xs)
        (if (some #(true? (first %)) res)
          (let [nres (partition-by #(identity (first %)) res)
                fres (filter #(true? (first (first %))) nres)]
            (let [[f] (take-last 1 (sort-by count fres))]
              (if (= 1 (count f))
                (second (first f))
                (let [[x & xs] (map second f)]
                  (concat x (map second xs))))))
          [])
        (recur xs
          (let [xn (first xs)
                pair [x xn]
                inc? (= (inc x) xn)]
            (conj res [inc? pair])))))))

(defcheck solution-3a53fbec
  (fn li [s]
    (let [incseq (reduce (fn [v x]
                           (if (or (empty? v) (not= (last (last v)) (dec x)))
                             (conj v [x])
                             (update-in v [(dec (count v))] conj x)))
                   []
                   s)
          incseq (filter #(> (count %) 1) incseq)]
      (if (empty? incseq) [] (apply max-key count (reverse incseq))))))

(defcheck solution-3b01bb2d
  (fn [xs]
    (let
     [result
      (->>
        xs
        (reduce
          (fn
            [[best-so-far current] now]
            (if (or (empty? current) (>= (last current) now))
              [best-so-far [now]]
              (if (>= (count current) (count best-so-far))
                [(conj current now) (conj current now)]
                [best-so-far (conj current now)])))
          [[][]]
          )
        first)]
      (if (< (count result) 2) [] result))))

(defcheck solution-3b1455f6
  (fn [xs]
    (letfn [(increasing? ([[a b]] (< a b)))]
      (->> xs
        (iterate rest)
        (take-while seq)
        (map #(partition 2 1 [0] (cons -1 %)))
        (map #(take-while increasing? %))
        (map #(map second %))
        (sort-by #(* -1 (count %)))
        (filter #(> (count %) 1))
        first
        vec))))

(defcheck solution-3b63fb9d
  (fn [coll]
    (letfn [(longest [acc xs]
              (if (> (count xs) (count acc))
                xs
                acc))
            (conj-if-min [res streak]
              (if (>= (count streak) 2)
                (conj res streak)
                res))]
      (loop [[curr & rst] coll
             prev (dec curr)
             streak []
             res []]
        (if (nil? curr)
          (reduce longest [] (conj-if-min res streak))
          (if (> curr prev)
            (recur rst curr (conj streak curr) res)
            (recur rst curr [curr] (conj-if-min res streak))))))))

(defcheck solution-3b8bc4ca
  (let [longer (fn [[xs xn :as x] [ys yn :as y]]
                 (if (> yn xn) y x))
        append (fn [[xs n] x]
                 [(concat xs [x]) (max 2 (inc n))])
        f (fn [[y lng cur] x]
            (if (> x y)
              [x lng (append cur x)]
              [x (longer lng cur) [[x] 0]]))]
    (fn [[x & xs]]
      (let [[_ x y] (reduce f [x [[] 0] [[x] 0]] xs)]
        (first (longer x y))))))

(defcheck solution-3bf5ca7c
  (fn
    [li]
    (into
      []
      (let
       [parts (partition-by
                (fn
                  [[a b] & part]
                  (< a b))
                (partition 2 1 li))
        seqs (map
               (fn
                 [lii]
                 (cons
                   (first (first lii))
                   (apply
                     (fn
                       [& lee]
                       (take-nth
                         2
                         (drop
                           1
                           (apply concat lee))))
                     lii)))
               parts)
        out (->>
              seqs
              (sort
                (fn
                  [a b]
                  (>
                    (count a)
                    (count b))))
              (filter
                #(and
                  (not
                    (=
                      1
                      (count %)))
                  (<
                    (first %)
                    (nth % 1))))
              first)]
        out))))

(defcheck solution-3d6e529e
  (fn longest-increasing-subseq [xs]
    (letfn [(inc-subseq [xs accum]
              (if (or (empty? xs) (and (seq accum) (<= (first xs) (last accum))))
                (if (>= (count accum) 2) accum [])
                (recur (rest xs) (conj accum (first xs)))))]
      (loop [xs xs longest []]
        (if (empty? xs)
          longest
          (let [s (inc-subseq xs [])]
            (if (> (count s) (count longest))
              (recur (rest xs) s)
              (recur (rest xs) longest))))))))

(defcheck solution-3d873ec4
  (fn [seq]
    (loop [seq seq
           current []
           longest []]
      (if (empty? seq)
        (if (>= (count longest) 2) longest [])
        (let [[head & tail] seq
              new-current (if (or
                               (empty? current)
                               (> head (last current)))
                            (conj current head)
                            [head])]
          (recur tail new-current
            (if (> (count new-current)
                  (count longest))
              new-current longest)))))))

(defcheck solution-3e1614fd
  (fn p53 [xs]
    (letfn [(tails [xs] (if (empty? xs) nil
                                        (lazy-seq (cons xs (tails (drop 1 xs))))))
            (increasing-seq [xs] (loop [r [] xs xs l -1]
                                   (cond (empty? xs) r
                                         (<= (first xs) l) r
                                         :else (recur (conj r (first xs)) (rest xs) (first xs))
                                         )))]
      (loop [longest [] seqs (filter #(> (count %) 1) (tails xs))]
        (if (empty? seqs) (if (> (count longest) 1) longest [])
                          (let [s (increasing-seq (first seqs))]
                            (recur (if (> (count s) (count longest)) s longest)
                              (rest seqs))))))))

(defcheck solution-3e6bbe8
  (fn lics[a-seq]
    (let [head (first a-seq) res (reduce (fn [[cand max last-el] el]
                                           (if (>= last-el el)
                                             ;subsequence has finished
                                             (let [cand-len (count cand)]
                                               (if (> cand-len (count max))
                                                 (if (> cand-len 1)
                                                   [[el] cand el]
                                                   [[el] max el]
                                                   )
                                                 [[el] max el]
                                                 )
                                               )
                                             ;subsequence is increasing
                                             [(conj cand el) max el]
                                             )
                                           )
                                   [[head][] head]
                                   (rest a-seq)
                                   ) last-cand (first res) last-max (second res)]
      (let [last-cand-len (count last-cand)]
        (if (> last-cand-len (count last-max))
          (if (> last-cand-len 1)
            last-cand
            last-max
            )
          last-max
          )
        )
      )
    ))

(defcheck solution-3e8da840
  (fn lis [coll]
    (loop [sub [], rem coll, m {}]
      (let [add-map (fn [m sub]
                      (if (or (contains? m (count sub))
                              (< (count sub) 2))
                        m
                        (assoc m (count sub) sub)))]
        (if (empty? rem)
          (if (empty? m)
            []
            (->> (add-map m sub)
              sort
              last
              second))

          (if (or (nil? (last sub))
                  (> (first rem) (last sub)))
            (recur (conj sub (first rem)) (rest rem) m)
            (recur (conj [] (first rem))
              (rest rem)
              (add-map m sub))))))))

(defcheck solution-3e94ce6b
  (fn lis [v]
    (loop [maxlen 0
           maxseq []
           curseq []
           lastelt nil
           remain v]
      (if (empty? remain)
        (if (> maxlen 1) maxseq [])
        (let [nextelt (first remain)
              incr (if lastelt (> nextelt lastelt) true)
              newcurseq (if incr
                          (conj curseq nextelt)
                          [nextelt])
              better (> (count newcurseq) (count maxseq))
              newmaxseq (if better newcurseq maxseq)
              newmaxlen (count newmaxseq)]
          (recur newmaxlen
            newmaxseq
            newcurseq
            nextelt
            (rest remain)))))))

(defcheck solution-3ec89c67
  (fn [coll]
    (letfn [(one? [coll] (= 1 (count coll)))
            (consec? [a b] (= (inc a) b))
            (collect-runs [[[prev & _ :as run] & more :as runs] x]
              (if (consec? prev x)
                (conj more (conj run x))
                (conj (if (> (count run) 1) runs more) (list x))))]
      (->> coll
        (reduce collect-runs (list (list -2147483648)))
        (drop-while one?)
        (sort-by count >)
        (first)
        (reverse)))))

(defcheck solution-3ef58c0d
  (fn [s] (->> (concat s [nil])
            (iterate rest)
            (take (count s))
            (map (fn [x] (->> (interleave x (iterate inc (first x)) (range))
                           (partition 3)
                           (some (fn [[a b c]] (and (not= a b) (take c x)))))))
            (reduce #(if (and (> (count %2) 1) (> (count %2) (count %))) %2 %) () ))))

(defcheck solution-3ef61f7e
  (fn longest-inc [coll]
    (let [increasing (fn  [coll]
                       (loop [n 1 c coll]
                         (if (or (empty? (rest c)) (>= (first c) (first (rest c))))
                           (take n coll)
                           (recur (inc n) (rest c)))))
          inc-seqs (filter #(> (count %) 1)
                     (map (comp increasing #(drop % coll))
                       (range (count coll))))]
      (if (empty? inc-seqs)
        '()
        (first (sort #(> (count %1) (count %2)) inc-seqs))))))

(defcheck solution-3f1e13e6
  (fn [x]
    (loop [l [] c [] [x1 & r] x]
      (let [ll (if (and (> (count c) 1) (> (count c) (count l))) c l)]
        (if (nil? x1) ll
                      (if (= (last c) (dec x1))
                        (recur l (conj c x1) r)
                        (recur ll [x1] r)))))))

(defcheck solution-3f5557fa
  (fn [s]
    (let [larger (fn [a b] (if (>= (count a) (count b)) a b))
          next? (fn [a b] (if (nil? b) true (= a (inc b))))
          find (fn [[x & rst] [c & _ :as C] L ]
                 (cond (nil?  x  ) (reverse (larger L C))
                       (next? x c) (recur rst (cons x C) L)
                       :else       (recur rst (list x) (larger L C))))
          ll (find s '() '())]
      (if (> (count ll) 1) ll []))))

(defcheck solution-3f5a7baa
  (fn [s]
    (let [[start len] (->> (partition 2 1 s)
                        (map (partial apply <))
                        (map vector (range))
                        (partition-by second)
                        (filter (comp second first))
                        (map (juxt (comp first first) count))
                        (reverse)
                        (cons [0 0])
                        (apply (partial max-key second)))]
      (if (pos? len)
        (take (inc len) (drop start s))
        []))))

(defcheck solution-3f98707b
  (fn longest-sub-seq [coll]
    (let [is-increasing (fn [[x y]] (= (inc x) y))
          sub-seqs-from (fn sub-seqs-from [xs]
                          (if (empty? xs)
                            nil
                            (let [[sub rst] (split-with is-increasing xs)]
                              (cons sub (sub-seqs-from (drop-while (complement is-increasing) rst))))))
          pairs (map vector coll (rest coll))
          sub-seq-pairs (sub-seqs-from pairs)
          longest-pairs (apply (partial max-key count) sub-seq-pairs)
          longest (if (empty? longest-pairs) '() (cons (ffirst longest-pairs) (map second longest-pairs)))]
      longest)))

(defcheck solution-3fcbcfd1
  (fn lis [s]
    (if (= 1 (count s))
      '()
      (if (apply < s)
        s
        (let [a (lis (rest s))
              b (lis (butlast s))]
          (if (> (count a) (count b))
            a
            b))))))

(defcheck solution-3fedd065
  (fn [s]
    (reduce
      #(if (> (count %) (count %2)) % %2)
      []
      (filter #(>= (count %) 2)
        ((reduce
           (fn [m i]
             (if
              (= (inc (first i)) (second i))
               (assoc m :w (conj (m :w) (first i)))
               (assoc (assoc m :r (conj (m :r) (conj (m :w) (first i)))) :w [])))
           {:r [] :w []}
           (partition 2 1 (conj s (last s)))) :r)))))

(defcheck solution-4015314b
  #(letfn [(max-seq [[as bs]] (if (< (count as) (count bs)) bs as))]
     (when-let
      [lis (max-seq (reduce
                      (fn [[xs' xs :as xss] x]
                        (if (and (seq xs) (> x (peek xs)))
                          [xs' (conj xs x)]
                          [(max-seq xss) [x]]))
                      [[] []] %))]
       (if (< 1 (count lis)) lis []))))

(defcheck solution-406a2126
  (fn tf [s]
    (loop [best [] curr [(first s)] restt (rest s)]
      (let [a (last curr) b (first restt)]
        (if b
          (if (< a b)
            (let [curr_n (conj curr b)]
              (recur (max-key count curr_n best) curr_n (rest restt)))
            (recur best [b] (rest restt))
            )
          best
          )
        )
      )))

(defcheck solution-416e20a4
  (fn [c]
    (last
      (sort-by count
        (conj
          (filter
            (fn [v] (= v (range (first v) (inc (last v)))))
            (let [s (count c)]
              (for [x (range (- s 2)) y (range (+ x 2) (+ s 1))]
                (subvec c x y))))
          [])))))

(defcheck solution-41a5fe9b
  (fn [xs]
    (let [s (reduce #(if (and (number? (last (last %1))) (< (last (last %1)) %2))
                       (conj (vec (drop-last %1)) (conj (last %1) %2))
                       (conj %1 [%2])) [] xs)]
      (if (< (apply max (map count s)) 2)
        []
        (first (filter #(= (apply max (map count s)) (count %)) s)))
      ;(map count s)
      )))

(defcheck solution-41b37fb1
  (fn [coll] (first (conj (filterv (fn asc? [coll1] (every? true? (map #(< % %2) (drop-last coll1) (rest coll1))))
                            (mapcat #(partition % 1 coll) (range (count coll) 1 -1)))
                      ()))))

(defcheck solution-4200899e
  (fn [sq]
    (let [v
          (first (sort (comparator #(> (count %1) (count %2)))
                   (filter #(> (count %) 1) (loop [[h & t] sq cur -1 s [] r []]
                                              (if h
                                                (if (> h cur)
                                                  (recur t h (conj s h) r)
                                                  (recur (cons h t) -1 [] (conj r s))) (conj r s))))))]
      (if v v []))))

(defcheck solution-424daec6
  (fn [coll]
    (let [apply-when (fn [pred fun coll] (if (pred coll) (apply fun coll) coll))]
      (->> (map list coll (reductions = true (map < coll (rest coll))))
        (partition-by second)
        (map (partial map first))
        (filter #(-> % count (> 1)))
        reverse
        (apply-when not-empty (partial max-key count))))))

(defcheck solution-426dfce2
  (fn long-incr-subseq [c]
    (loop [coll c best [] cur []]
      (cond (empty? coll) (if (> (count cur) (count best))
                            (if (>= (count cur) 2) cur [])
                            (if (>= (count best) 2) best []))
            (empty? cur) (recur (rest coll) best (conj cur (first coll)))
            (>= (last cur) (first coll))
            (if (> (count cur) (count best))
              (recur (rest coll) cur [(first coll)])
              (recur (rest coll) best [(first coll)]))
            :else
            (recur (rest coll) best (conj cur (first coll)))))))

(defcheck solution-42f7cbb5
  (fn [ns]
    (letfn [(r [[x & m]]
              (if (and m (< x (first m)))
                (cons x (r m))
                (if x [x] [])))
            (rs [s] (if s (conj (rs (next s)) (r s)) []))]
      (apply max-key count [] (remove #(< (count %) 2) (rs ns))))))

(defcheck solution-4350d289
  (fn [xs]
    (let [<< #(apply < %)]
      (apply max-key count
        (cons [] (reverse (map #(cons (first (first %)) (map second %))
                            (filter #(<< (first %))
                              (partition-by << (partition 2 1 xs))))))))))

(defcheck solution-436402c7
  (let [group-by-increasing (fn [[h & t :as v]]
                              (loop
                               [vs (list [h])
                                ts t]
                                (if (empty? ts)
                                  (apply vector (reverse vs))
                                  (let
                                   [vh (first vs)
                                    vt (rest  vs)
                                    th (first ts)
                                    tt (apply vector (rest  ts))]
                                    (recur
                                      (if (< (last vh) th)
                                        (conj vt (conj vh th))
                                        (conj vs [th])
                                        )
                                      tt)
                                    )
                                  )
                                )
                              )
        sizable-groups (fn [v] (->> v
                                 group-by-increasing
                                 (filter #(< 1 (count %)))
                                 (group-by count)))
        ]
    (fn [v]
      (let [gs (sizable-groups v)]
        (if (empty? gs)
          []
          (first (gs (apply max (keys gs)))))))
    ))

(defcheck solution-43769ded
  (fn longest-increasing-seq[coll]
    (apply max-key count []
      (reverse (filter #(apply < %)
                 (for [x (range (count coll))
                       y (range (+ 2 x) (+ 1 (count coll)))]
                   (subvec coll x y)))))))

(defcheck solution-43a1624a
  (fn [s]
    (let [subseqs (fn ! [co] (when-let [se (seq co)] (cons se (! (rest se)))))
          longest (fn [ss] ((fn ! [agg c]
                              (if-let [s (seq c)]
                                (if (= (inc (last agg)) (first s))
                                  (! (conj agg (first s)) (rest s))
                                  agg) agg))
                            [(first ss)] (rest ss)))
          result  (apply max-key count (map longest (subseqs s)))]
      (if (>= (count result) 2)
        result
        [])
      )))

(defcheck solution-43de250a
  #(first
     (reduce
       (fn [[m c] x]
         (let [t count
               a (if (> x (or (last c) -1)) (conj c x) [x])
               b (if (and (> (t a) 1) (> (t a) (t m))) a m)]
           [b a]))
       [[][]] %)))

(defcheck solution-43e708d7
  (letfn [
          (increasing-tails [lst]
            (let [fst (first lst)
                  snd (second lst)
                  rst (rest lst) ]
              (cond
                (nil? snd) (list fst)
                (nil? fst) '()
                (< fst snd) (cons fst
                              (increasing-tails rst))
                :else (list fst))))

          (get-suffixes [lst]
            (if (empty? lst) '()
                             (cons lst
                               (get-suffixes (rest lst)))))

          (longest-lst [a b]
            (let [len-a (count a)
                  len-b (count b) ]
              (cond
                (and
                 (> 2 len-a) (> 2 len-b)) '()
                (< len-a len-b) b
                :else a)))
          ]
    (fn longest-increasing-sub-seq [lst]
      (reduce longest-lst
        (map increasing-tails
          (get-suffixes lst))))))

(defcheck solution-43ef0b32
  (fn f[ss]
    (loop [[x & xs] (rest ss) ax {0 []} cx [(first ss)] prev (dec (first ss))]
      (if-not (nil? x)
        (let [ax' (if (< prev x) ax (assoc ax (count cx) (ax (count cx) cx)))
              cx' (if (< prev x) (conj cx x) [x])]
          (recur xs ax' cx' x))
        (let [ax' (assoc ax (count cx) (ax (count cx) cx))]
          (->>
            ax'
            (remove #(= (first %) 1))
            (sort-by first)
            last
            second))))))

(defcheck solution-43f4c1bf
  (fn [coll]
    (let [ordered? #(apply < %)
          pairs (partition-by ordered?
                  (rest (partition-all 2
                          (interleave (cons 0 coll) coll))))
          filtered (filter #(ordered? (first %)) pairs)
          max-size (count (last (sort-by count filtered)))
          goal (some #(if (= (count %) max-size) %) filtered)]
      (if (pos? max-size)
        (cons (ffirst goal) (map last goal)) []))))

(defcheck solution-44271ddc
  (fn [coll]
    (->>
      (range 2 (inc (count coll)))
      (mapcat #(partition % 1 coll))
      (filter #(apply < %))
      (cons [])
      (sort-by count >)
      first)))

(defcheck solution-445b7e02
  (fn [xs]
    (loop [i 0
           j 1
           si 0
           sj 1
           max-i 0
           max-j 1]
      (if (= j (count xs))
        (if (> (- max-j max-i) 1) (subvec xs max-i max-j) [])
        (if (> (xs j) (xs i))
          (recur (inc i) (inc j) si (inc sj) (if (> (- (inc sj) si) (- max-j max-i)) si max-i) (if (> (- (inc sj) si) (- max-j max-i)) (inc sj) max-j))
          (recur (inc i) (inc j) sj (inc sj) max-i max-j))))))

(defcheck solution-44d6cb00
  (fn [coll]
    (first
      (reduce (fn [[res acc] e]
                (if (= (inc (last acc)) e)
                  (let [nacc (conj acc e)]
                    (if (and (>= (count nacc) 2)
                             (> (count nacc) (count res)))
                      [nacc nacc]
                      [res nacc]))
                  [res [e]]))
        [[] [(first coll)]]
        (rest coll)))))

(defcheck solution-44f7c559
  (fn [a]
    (let
     [lst ((fn [lst]
             ((fn [candidate rest-vec candidates]
                (if (empty? rest-vec)
                  (if (> (count candidate) 1)
                    (conj candidates candidate)
                    candidates)
                  (if (> (first rest-vec) (last candidate))
                    (recur (conj candidate (first rest-vec)) (rest rest-vec) candidates)
                    (recur [(first rest-vec)]
                      (rest rest-vec)
                      (if (> (count candidate) 1)
                        (conj candidates candidate)
                        candidates))
                    )
                  )
                )
              [(first lst)] (rest lst) []
              )
             )
           a)]
      (or (some
            #(if (= (apply max (for [x lst] (count x)))
                   (count %))
               % false) lst)
          [])
      )
    ))

(defcheck solution-4501d31c
  (fn [s]
    (loop [x s
           l []
           b []
           c nil]
      (if x
        (let [h (first x)]
          (if (= (dec h) c)
            (recur (next x) l (conj b h) h)
            (if (> (count b) (count l))
              (recur (next x) b [h] h)
              (recur (next x) l [h] h))))
        (let [out (if (> (count b) (count l))
                    b
                    l)]
          (if (> (count out) 1)
            out
            []))))))

(defcheck solution-45781f7b
  (fn[x]
    ( #(if(= 1 (count %))[] % )
     (#(if(>(count (last %))(count (first %)))(last %1)(first %1)  )
      (reduce
        #(let[l (last %1) f (first %1) v %2]
           (if (< (last l) v)
             [f (conj l v)]
             (if(> (count l) (count f))
               [l [v]] [f [v]])
             ))
        [[][(first x)]]
        (rest x)
        )))))

(defcheck solution-45be335b
  (fn [s] (loop [s s
                 sub []
                 result {}]
            (do
              #_(prn "here is result" result sub)
              (if (nil? (next s))
                (do
                  (let [final (conj result {sub (count sub)})]
                    #_(prn final)
                    (key (apply max-key val final))))
                (let [a (first s)
                      b (second s)
                      isSeq? (= (inc a) b)]
                  (recur (rest s)
                    (if isSeq?
                      (if (empty? sub)
                        [a b]
                        (conj sub b))
                      [])
                    (if isSeq?
                      result
                      (do
                        #_(prn result sub)
                        (conj result {sub (count sub)}))))))))))

(defcheck solution-45deca1d
  (fn [coll]
    (let [
          take-while-inc (fn take-while-inc
                           ([coll] (take-while-inc (first coll) (rest coll)))
                           ([fst coll]
                            (if (and (seq coll) (< fst (first coll)))
                              (cons fst (take-while-inc coll))
                              (list fst))))
          partition-inc (fn partition-inc [coll]
                          (when-let [s (seq coll)]
                            (let [run (take-while-inc s)]
                              (cons run (partition-inc (drop (count run) s))))))
          longest (apply max-key count (reverse (partition-inc coll)))]
      (if (= 1 (count longest))
        []
        longest))))

(defcheck solution-460c482b
  (fn [s]
    (letfn [(incr [acc s]
              (if (and (seq s) (> (first s) (last acc)))
                (recur (conj acc (first s)) (rest s))
                acc))
            (increasing [[h & r]] (incr [h] r))
            (increasing-subseqs [s]
              (if (seq s)
                (let [t (increasing s)]
                  (cons t (increasing-subseqs (drop (count t) s))))
                []))]
      (let [t (increasing-subseqs s)
            max-count (max 2 (apply max (map count t)))]
        (or (first (filter #(= max-count (count %))
                     t))
            [])))))

(defcheck solution-4682c4ad
  (fn bar [v]
    (letfn [(pick [coll]
              (reduce #(if (> (count %2) (count %1)) %2 %1)
                [] (filter #(> (count %) 1) coll)))]
      (loop [curr [] best [] remain v]
        (cond
          (empty? remain) (pick [curr best])
          (empty? curr) (recur [(first remain)] best (rest remain))
          (= (first remain) (inc (peek curr))) (recur (conj curr (first remain)) best (rest remain))
          :else (recur [(first remain)] (pick [curr best]) (rest remain)))))))

(defcheck solution-47895e87
  (fn [x]
    (map second
      (last
        (remove #(= 1 (count %))
          (sort-by count
            (partition-by #(apply - %)
              (map-indexed
                #(list % %2)
                x))))))))

(defcheck solution-485f5fd5
  (fn [coll]
    (letfn [(take-while-increasing [coll]
              (->> (partition 2 1 (cons (dec (first coll)) coll))
                (take-while (fn [[a b]] (< a b)))
                (map second)))]
      (->> coll
        (partition-all (count coll) 1)
        (map take-while-increasing)
        (filter #(> (count %) 1))
        (reduce #(if (> (count %2) (count %1)) %2 %1) [])))))

(defcheck solution-48872e09
  (comp
   (partial filter (comp not nil?))
   flatten
   (juxt (partial map first) (comp last last))
   (partial apply max-key count)
   (partial cons [])
   reverse
   (partial filter (comp (partial apply <) first))
   (partial partition-by (partial apply <))
   (partial partition 2 1)))

(defcheck solution-48ace469
  (fn [coll]
    (let [[x y] (reduce
                  (fn [a b]
                    (let [[x y] a
                          z (last y)]
                      (if (nil? z)
                        [x [b]]
                        (if (= 1 (- b z))
                          [x (conj y b)]
                          (if (> (count x) (count y))
                            [x [b]]
                            [y [b]])))))
                  [[] []]
                  coll)]
      (let [z (if (> (count x) (count y)) x y)]
        (if (< (count z) 2)
          []
          z)))))

(defcheck solution-48ccde98
  #(let [q (loop [[a b & c :as s] %, t [a], r [] ]
             (if b
               (if (> b a)
                 (recur (rest s) (conj t b) r)
                 (recur (rest s) [b] (if (> (count t) 1) (conj r t) r)))
               (if (> (count t) 1) (conj r t) r)))
         g (group-by count q) ]
     (if (empty? g) [] (first (g (->> g keys (apply max)))) )))

(defcheck solution-49016c5f
  (fn [s]
    (letfn [(increasing? [c] (and (not-empty (rest  c)) (< (first c) (second c))))]
      (let [increasing-pairs (filter #(increasing? (first %)) (partition-by increasing? (partition 2 (interleave s (rest s)))))]
        (if (not-empty increasing-pairs)
          (let [max-increasing-pairs (reduce #(if (>= (count %1) (count %2)) %1 %2) increasing-pairs)]
            (cons (first (first max-increasing-pairs)) (map second max-increasing-pairs)))
          (list))))))

(defcheck solution-492588dc
  (fn [sq]
    (letfn [(incseq [s]
              (cond (empty? s) ()
                    (empty? (rest s)) s
                    (< (first s) (second s)) (cons (first s)
                                               (incseq (rest s)))
                    :else (take 1 s)))]
      (->>
        sq
        (iterate rest)
        (take (count sq))
        (map incseq)
        (map (fn [x] (if (> (count x) 1) x ())))
        reverse
        (apply (partial max-key count))))))

(defcheck solution-49483907
  (fn [xs] (first (reduce (fn [[r vs] [l g]]
                            (if (> g l)
                              [r (conj vs l)]
                              (if (and (> (count vs) 0)
                                       (> (+ 1 (count vs)) (count r)))
                                [(conj vs l) []]
                                [r []])
                              ))
                    [[] []]
                    (map vector xs (concat (rest xs) [(- (last xs) 1)]))))))

(defcheck solution-4989faef
  (fn [s]
    (first
      (reduce
        (fn [[best-seq cand-seq] x]
          (if (or (empty? cand-seq) (> x (last cand-seq)))
            (if (and (>= (count cand-seq) (count best-seq))
                     (>= (count cand-seq) 1))
              [(conj cand-seq x) (conj cand-seq x)]
              [best-seq (conj cand-seq x)])
            [best-seq [x]]))
        [[] []] s))))

(defcheck solution-49b497d8
  (fn longest-subseq [coll]
    (let [take-seq (fn [n pred coll]
                     (let [hits (count (take-while #(apply pred %) (partition n 1 coll)))]
                       (take (+ n hits -1) coll)))
          chop (fn [coll] (for [n (range (count coll))] (drop n coll)))
          parts (chop coll)
          seqs (map (partial take-seq 2 #(= (inc %1) %2)) parts)
          longest (apply max-key count seqs)]
      (if (< (count longest) 2)
        []
        longest))))

(defcheck solution-49b5c20f
  (fn [p]
    (let [t (reverse (filter (comp first vals)
                       (for [i (range 9) j (reverse (range 9))]
                         (let [r (drop i (take j p)) c (count r)]
                           {r (and (> c 1) (= r (take c (drop (first r) (range)))))}))))]
      (if (empty? t) t (first (keys (apply max-key (comp count first keys) t)))))))

(defcheck solution-49c54aa
  (fn [l]
    (letfn [(sublists [[a & r] tmp out]
              (cond (nil? a) (cons tmp out)
                    (empty? tmp) (recur r [a] out)
                    (> a (last tmp)) (recur r (conj tmp a) out)
                    :else (recur r [a] (cons tmp out))))]
      (or
       (last
         (sort-by count
           (filter #(< 1 (count %)) (sublists l [] []))))
       []))))

(defcheck solution-49c5ee54
  #(loop [xs %, best-c 0, best-s '()]
     (if (< (count xs) 2) best-s
                          (let [s (map second (take-while (fn [p] (< (first p) (second p))) (partition 2 1 xs)))
                                ss (if (empty? s) s (cons (first xs) s))
                                c (count ss)
                                [next-c next-s] (if (> c best-c) [c ss] [best-c best-s])]
                            (recur (rest xs) next-c next-s)))))

(defcheck solution-4b103fc
  (fn [[h & t]]
    (loop [longest [], cur [h], s t]
      (let [[h & t] s
            longest (if (>= (count longest) (count cur)) longest cur)
            cur (if (= h (inc (peek cur))) (conj cur h) [h])]
        (if (empty? s)
          (if (>= (count longest) 2) longest [])
          (recur longest cur t))))))

(defcheck solution-4ba03c76
  (fn __ [coll]
    (letfn [(pick-longer [coll1 coll2]
              (if (> (count coll2)
                    (count coll1)) coll2 coll1))  ; favor coll1
            (aux [max-so-far [x & _ :as max-curr] [y & ys :as input]]
              (cond
                (empty? input) (pick-longer max-so-far max-curr)
                (< x y) (aux max-so-far (cons y max-curr) ys)
                :else (aux (pick-longer max-so-far max-curr) [y] ys)))]
      (let [result (aux [] [(first coll)] (rest coll))]
        (if (< (count result) 2) [] (reverse result))))))

(defcheck solution-4ba702b5
  (fn __ [coll]
    (let [ret
          ((fn lst [[head & tail :as whole] seen now]
             (if (empty? whole)
               seen
               (if (<= head (last now))
                 (lst tail seen [head])
                 (let [nxt (conj now head)]
                   (lst tail (if (< (count seen) (count nxt)) nxt seen) nxt)))) )
           (rest coll) [(first coll)] [(first coll)])]
      (if (< (count ret) 2) [] ret))))

(defcheck solution-4bb04050
  (fn [coll]
    (let [inc-list (reductions
                     (fn [m x]
                       (if (or
                            (empty? m)
                            (> x (last m)))
                         (conj m x)
                         [x]))
                     []
                     coll)
          max-list (reduce (fn [m x]
                             (if (> (count x) (count m))
                               x
                               m))
                     []
                     inc-list)]
      (if (>= (count max-list) 2)
        max-list
        []))))

(defcheck solution-4c293907
  (letfn [(all-incr-seq
            [coll]
            (if (empty? coll) nil
                              (let [seq1 (loop [x [(first coll)]
                                                [y & coll] (rest coll)]
                                           (cond (nil? y) x
                                                 (< (last x) y) (recur (conj x y) coll)
                                                 :else x))]
                                (lazy-seq (cons seq1
                                            (all-incr-seq (nthrest coll (count seq1))))))))]
    (fn [coll]
      (let [ans (reduce #(if (< (count %1) (count %2)) %2 %1)
                  (all-incr-seq coll))]
        (if (< 1 (count ans)) ans [])))))

(defcheck solution-4c404d0f
  (fn sub [s]
    (let [subb (fn [s curs maxs]
                 (let [x (first s)
                       r (rest s)
                       ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x)
                                                                  [x])
                       nmax (max-key count ncurs maxs)]
                   (if (seq r)
                     (recur r ncurs nmax)
                     nmax)))
          longest (subb s [(first s)] [])]
      (if (> (count longest) 1) longest []))))

(defcheck solution-4c7fafdd
  (fn longest [s]
    (second (reduce (fn [acc n]
                      (let [c (first acc)
                            m (second acc)
                            newcurr (if (> n (last c)) (conj c n) [n])
                            newmax (if (and (> (count newcurr) (count m)) (> (count newcurr) 1)) newcurr m)]
                        [newcurr newmax]))
              [[(first s)][]] (rest s)))))

(defcheck solution-4cd71e1c
  (fn [lst]
    (loop [l (rest lst) r [(first lst)] t []]
      (let [t (if (> (count r) (count t)) r t)]
        (if (empty? l)
          (if (> (count t) 1) t [])
          (if (= (last r) (dec (first l)))
            (recur (rest l) (conj r (first l)) t)
            (recur (rest l) [(first l)] t)))))))

(defcheck solution-4d1542a2
  #((reduce (fn [[a b] e]
              (let [c (conj b e)
                    f count
                    n (f c)]
                (if (apply < c)
                  (if (and (> n 1) (> n (f a))) [c c] [a c]) [a [e]])))
      [[] []] %) 0))

(defcheck solution-4db7994a
  (fn [coll]
    (->> (partition 2 1 coll)
      (partition-by #(- (second %) (first %)))
      (filter #(= 1 (- (second (first %)) (ffirst %))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) [])
      flatten
      distinct)))

(defcheck solution-4de1a231
  (fn x [s]
    (last
      (reduce
        (fn al [[cur max], el]
          (let [is-greater? (pos? (compare el (last cur)))
                cur (if is-greater? (conj cur el) [el])
                max (if (and (> (count cur) (count max)) (> (count cur) 1)) cur max)]
            [cur max]))
        [[] []] s))))

(defcheck solution-4de80636
  (fn [s]
    (let [f (fn [s]
              (filter (complement nil?)
                (map #(if (= %1 %2) %1 nil)
                  s
                  (range (first s) (+ (first s) (count s) 1)))))
          r (loop [s s
                   a []]
              (if (seq s)
                (recur (next s) (conj a (f s)))
                a))
          ]
      (->> r
        (sort-by count)
        last
        seq
        (#(if (> (count %) 1) % ()))
        ))))

(defcheck solution-4e580ae7
  (fn [c]
    (->>
      (reduce
        (fn [coll elem]
          (let [current (first coll)
                first-el (first current)]
            (if (= (inc first-el) elem)
              (conj (rest coll) (conj current elem))
              (conj coll (list elem)))))
        (list (list (first c)))
        (rest c)

        )
      (remove #(< (count %) 2))
      (sort-by count)
      (last)
      (reverse))
    ))

(defcheck solution-4f3a8fa7
  #(apply max-key %
     (for [x (%2 (% %3) 0 -1)
           y (%2 (- x 1))
           :let [z (subvec %3 y x)]]
       (if (apply < z) z []))) count range)

(defcheck solution-4f896c5d
  (fn [v]
    (loop [v_ v ret '() ret_c 0 cand '() cand_c 0]
      (let [a (first v_) n (next v_)]
        (cond
          (and (> cand_c ret_c) (>= cand_c 2))
          (recur v_ cand cand_c cand cand_c)
          (empty? v_)
          (into '() ret)
          (or (empty? cand) (> a (first cand)))
          (recur n ret ret_c (conj cand a) (inc cand_c))
          :else
          (recur n ret ret_c (list a) 1))))))

(defcheck solution-4f9073ec
  (fn ! [coll]
    (loop [s (rest coll) longest [[]] current [(first coll)]]
      (if (empty? s)
        (let [result
              (if (>= (count longest) (count current)) longest current)]
          (if (>= (count result) 2) result []))
        (let [item (first s) prev (last current)]
          (if (> item prev)
            (recur (rest s) longest (conj current item))
            (recur (rest s)
              (if (>= (count longest) (count current)) longest current)
              [item])))))))

(defcheck solution-4fef098e
  (fn longest-sub-sequence [c]
    (let [sub-sequences (group-by count (reverse (reduce #(vec (if (< (last (first %1)) %2)
                                                                 (cons (conj (first %1) %2)
                                                                   (rest %1))
                                                                 (cons [%2] %1)))
                                                   [[(first c)]] (rest c))))
          longest (first (sub-sequences (apply max (keys sub-sequences))))]
      (if (= (count longest) 1)
        []
        longest))))

(defcheck solution-503af4a0
  (fn [v]
    (let [matrix (apply concat
                   (map (fn [start] (map #(subvec v start %)
                                      (range (+ 2 start) (inc (count v)))))
                     (range (count v))))]
      (->> matrix
        ; (filter #(= % (sort %)))
        ; (filter #(= (count %) (count (set %))))
        (filter #(apply < %))
        (reduce (fn [m x]
                  (if (< (count m) (count x))
                    x
                    m))
          [])
        )
      )))

(defcheck solution-50ca7dde
  (fn longest
    ([col]
     (longest col (count col)))
    ([col n]
     (let
      [found (some #(if (apply < %) %) (partition n 1 col))]
       (if (> n 1)
         (if found found (longest col (dec n)))
         [])))))

(defcheck solution-50d788a4
  (fn [coll]
    (let [irange (range 0  (count coll))]
      (->>
        (for [i irange j irange :when (> j i)] (drop i (take (inc j) coll)))
        (filter #(and (<= 2 (count %)) (apply < %)))
        (#(if (empty? %) [[]] %))
        reverse
        (apply max-key count)))))

(defcheck solution-512ed427
  (fn longest-increasing-subseq [s]
    (letfn [(increasing? [s]
              (apply < (first s)))
            (join [s]
              (if s
                (cons (ffirst s) (map second s))
                []))]
      (->> s
        (partition 2 1)
        (partition-by #(apply < %))
        (filter increasing?)
        (sort-by count >)
        first
        join))))

(defcheck solution-51366b88
  (fn [test]
    (loop [prior nil
           [n & mas :as ns] test
           current [n]
           best []]
      (letfn [(curr-best []
                (if (and
                     (> (count current) 1)
                     (> (count current) (count best)))
                  current best))]
        (cond
          (empty? ns) (curr-best)
          (nil? prior) (recur n mas current best)
          (= n (inc prior)) (recur n mas (conj current n) best)
          :default (recur n mas [n] (curr-best)))))))

(defcheck solution-515ca189
  (fn [xs]
    (loop [[x & xs] xs, current-seq [], max-seq [], last-x 2147483647]
      (cond (nil? x) max-seq
            (> x last-x)
            (let [new-seq (conj current-seq x)]
              (recur xs new-seq (if (> (count new-seq) (count max-seq)) new-seq max-seq) x))
            :else (recur xs [x] max-seq x)))))

(defcheck solution-516c4d3c
  (fn[a-seq]
    (letfn [(max-of [s1 s2] (if (> (count s1) (count s2)) s1 s2))]
      (loop [ret []
             curr [(first a-seq)]
             ss (rest a-seq)]
        (if (empty? ss)
          (let [ret (max-of curr ret)]
            (if (> (count ret) 1) ret []))
          (if (> (first ss) (last curr))
            (recur ret (conj curr (first ss)) (rest ss))
            (recur (max-of curr ret) [(first ss)] (rest ss))))))))

(defcheck solution-51a3a0c0
  (fn [xs]
    (loop [res []
           cur [(first xs)]
           ys xs]
      (cond
        (empty? ys)
        (if (and (> (count cur) (count res))
                 (>= (count cur) 2))
          cur
          res)

        (< (last cur) (first ys))
        (recur res
          (conj cur (first ys))
          (rest ys))

        :else
        (recur (if (and (> (count cur) (count res))
                        (>= (count cur) 2))
                 cur
                 res)
          [(first ys)]
          (rest ys))))))

(defcheck solution-52360a41
  (fn longest [col]
    (let
     [r (:longest (reduce
                    (fn [c x]
                      (if (or (empty? (:current c)) (= x (inc (last (:current c)))))
                        (if (>= (count (:current c)) (count (:longest c)))
                          (assoc c :current (conj (:current c) x) :longest (conj (:current c) x))
                          (assoc c :current (conj (:current c) x)))
                        (assoc c :current [x])))
                    {:longest [] :current []}
                    col))]
      (if (> (count r) 1) r []))))

(defcheck solution-525fd074
  (fn longest-increasing
    [s]
    (let [grouped (partition-by first (map-indexed #(vector (- %2 %) %2) s))
          biggest (fn
                    [a b]
                    (if (and (> (count b) (count a)) (> (count b) 1))
                      b
                      a))]
      (map second (reduce biggest [] grouped)))))

(defcheck solution-538f4163
  (fn liss [coll]
    (letfn [(points [xs] (cons 0 (map inc (keep-indexed #(if (apply >= %2) %1) (partition 2 1 xs)))))
            (ranges [xs] (partition 2 1 nil (points xs)))
            (chunks [xs] (map (partial apply subvec (vec xs)) (ranges xs)))]
      (->> coll
        chunks
        (filter #(> (count %) 1))
        (reduce #(if (> (count %2) (count %1)) %2 %1) [] )))))

(defcheck solution-53ed8dcb
  (fn [[c & cs]]
    ((fn f [X [a & b] R]
       (let [R (if (> (count X) (max 1 (count R))) X R)]
         (if (nil? a) R
                      (f (if (> a (last X)) (conj X a) [a]) b R))))
     [c] cs [])))

(defcheck solution-53f2153d
  (fn lsi [cs]
    (let [sqs (reduce
                (fn [seqs val]
                  (if (> val ((comp last last) seqs))
                    (conj (vec (butlast seqs)) (conj (last seqs) val))
                    (conj seqs [val])))
                [[(first cs)]]
                (rest cs))
          longest (first (second (apply max-key key (group-by count sqs))))]
      (if (== 1 (count longest))
        []
        longest))))

(defcheck solution-53f648cf
  #(loop [i (count %)]
     (if-let [a (first (filter (partial apply <) (partition i 1 %)))]
       a
       (if (= i 2)
         []
         (recur (dec i))))))

(defcheck solution-54833790
  (fn [l]
    (loop [l l
           acc []]
      (if (empty? l)
        (if (empty? (remove #(= 1 (count %)) acc))
          []
          (last (sort-by count (remove #(= 1 (count %)) acc))))

        (recur (rest l)
          (let [llast (last (last acc))
                nxt (first l)]
            (cond (and llast (= (inc llast) nxt)) (conj acc (vec (conj (last acc) nxt)))
                  llast (conj acc [nxt])
                  :else (conj acc [nxt]))))))))

(defcheck solution-5485f20d
  (fn [coll]
    (first
      (reduce
        (fn [[res curr] e]
          (let [l (last curr)
                curr (if (or (nil? l) (< l e)) (conj curr e) [e])
                res (if (and (< 1 (count curr)) (< (count res) (count curr))) curr res)]
            [res curr]))
        [[][]] coll))))

(defcheck solution-54e7a7b9
  (fn [coll]
    (loop [coll coll
           prev nil
           answer []
           running []]
      (cond
        (nil? coll) (let [v (if (> (count answer) (count running)) answer running)]
                      (if (> (count v) 1) v []))
        (or (nil? prev) (= (inc prev) (first coll))) (recur (next coll) (first coll) answer (conj running (first coll)))
        (> (count running) (count answer)) (recur (next coll) (first coll) running [(first coll)])
        :else (recur (next coll) (first coll) answer running)))))

(defcheck solution-550faa2
  #(let [size (count %)]
     (or (first
           (sort-by (comp - count)
             (for [a (range size)
                   z (range (- (inc size) a))
                   :let [ss (take z (drop a %))]
                   :when (and (> (count ss) 1)
                              (= ss (range (first ss) (inc (last ss)))))]
               ss)))
         [])))

(defcheck solution-557f0ab1
  (fn [l] (let [r
                (reduce #(if (> (count %1) (count %2)) %1 %2)
                  '()
                  (filter #(first (first %))
                    (partition-by first
                      (map
                        #(list (= %1 (dec %2)) %1 %2)
                        l
                        (rest l)))))]
            (if (not (empty? r)) (conj (vec (map second r)) (last (last r))) []))))

(defcheck solution-55a5bb6d
  (fn conse_inc([_seq]
                (conse_inc _seq [] []))
    ([_seq running _max]
     (if (empty? _seq)
       _max
       (if (and (not (nil? (second _seq)))(< (first _seq) (second _seq)))
         (recur (rest _seq) (conj running (first _seq)) _max)
         (recur (rest _seq) []
           (let [new_running (conj running (first _seq))]
             (if (and (> (count new_running) 1)(< (count _max)(count new_running)))
               new_running
               _max))))))))

(defcheck solution-564c87ef
  (fn [coll]
    (loop [p coll t [] r[]]
      (if
       (empty? p) (if (> (count t) (count r)) t (if (= 1 (count r)) [] r))
                  (if
                   (empty? t) (recur (rest p) (conj t (first p)) r)
                              (recur (rest p) (if (= (inc (last t)) (first p)) (conj t (first p)) [(first p)]) (if (= (count t) (inc (count r))) t r))
                              )))))

(defcheck solution-565d641b
  (fn liss [a-vec]
    (#(if (> (count %) 1)
        %
        [])
     (reduce #(max-key count %2 %1) (reduce (fn [accum curr-n]
                                              (if (> curr-n (last (last accum)))
                                                (conj accum (conj (last accum) curr-n))
                                                (conj accum [curr-n])))
                                      [[(first a-vec)]]
                                      (rest a-vec))))))

(defcheck solution-569508d1
  #(first
     (reduce
       (fn [[max_seq curr_seq] item]
         (cond
           (empty? curr_seq)
           [[] [item]]
           (> item (last curr_seq))
           (if (and (> (count curr_seq) 0)
                    (>= (count curr_seq) (count max_seq)))
             [(conj curr_seq item) (conj curr_seq item)]
             [max_seq (conj curr_seq item)])
           :else
           [max_seq [item]]))
       [[] []]
       %)))

(defcheck solution-580446f7
  (fn [ints]
    (first (reduce
             (fn [[longest working] x]
               (if (or (empty? working) (> x (peek working)))
                 (let [new-working (conj working x)
                       nw-count (count new-working)]
                   (if (and (>= nw-count 2) (> nw-count (count longest)))
                     [new-working new-working]
                     [longest new-working]))
                 [longest [x]]))
             [[] []]
             ints))))

(defcheck solution-5852b264
  (fn [sq]
    (letfn [
            (x [sq]  (partition-by #(< (first %) (second %)) (map list sq (rest sq))))
            (y [sq]  (filter #(< (first (first %)) (second (first %))) (x sq)))
            (z [sq]  (sort #(- (count %2) (count %1)) (y sq)))
            (t [sq]  (let [u (first (z sq))] (if (nil? u) '() (cons (first (first u)) (map second u)))))
            ]
      (t sq))))

(defcheck solution-589865cf
  #(apply max-key %
     (into ()
       (for [x (%2 (% %3)) y (%2 (+ 2 x) (+ 1 (% %3)))
             :let [l (subvec %3 x y)]]
         (if (apply < l) l [])))) count range)

(defcheck solution-591149db
  (fn sub-seq [x]
    (letfn [(do-divide [x y]
              (cond (empty? y) [x []]
                    (empty? x) (do-divide [(first y)] (vec (drop 1 y)))
                    (< (last x) (first y)) (do-divide (conj x (first y)) (vec (drop 1 y)))
                    :else [x y]))
            (do-parts [x]
              (let [[a b] (do-divide [] x)]
                (if (empty? b)
                  [a]
                  (concat [a] (do-parts b)))))
            (monoton-dec? [x]
              (cond (< (count x) 2) true
                    (< (first x) (second x)) false
                    :else (monoton-dec? (rest x))))]
      (if (monoton-dec? x)
        []
        (((reduce #(if (< (%1 0) (%2 0)) %2 %1) (group-by count (do-parts x))) 1) 0)))))

(defcheck solution-5948b372
  (fn [q]
    (letfn [(partition-pair [f s]
              (cond
                (empty? s) nil
                (empty? (rest s)) (list s)
                :else (let [l
                            (loop [el (first s)
                                   bl [el]
                                   rl (rest s)]
                              (if (or (empty? rl) (not (f el (first rl))))
                                bl
                                (recur (first rl) (conj bl (first rl)) (rest rl))))]
                        (cons l
                          (lazy-seq (partition-pair f (drop (count l) s))))))
              )]
      (let [res (partition-pair < q)
            ans (first (filter #(= (count %) (apply max (map count res))) res))]
        (if (= 1 (count ans)) () ans)
        ))
    ))

(defcheck solution-5a3318c3
  (fn long-ss [xs]
    (letfn [(ss [xs]
              (loop [out (take 1 xs)
                     in  (drop 1 xs)]
                (letfn [(done []
                          (if (< (count out) 2)
                            []
                            out))]
                  (if (empty? in)
                    (done)
                    (if (= (inc (last out)) (first in))
                      (recur
                        (concat out (list (first in)))
                        (rest in))
                      (done))))))]
      (loop [xs xs
             m []]
        (if (empty? xs)
          m
          (let [m' (ss xs)]
            (recur
              (rest xs)
              (if (> (count m') (count m))
                m'
                m))))))))

(defcheck solution-5ae9105c
  (fn [xs]
    (letfn [(consec [acc x]
              (cond
                (empty? acc) [[x]]
                (= (dec x) (-> acc peek peek)) (conj (vec (drop-last acc)) (conj (peek acc) x))
                :else (conj acc [x])))

            (longest [acc coll] (if (> (count acc)(count coll)) acc coll))]

      (->> (reduce consec [] xs)
        (filter #(< 1 (count %)))
        (reduce longest [])))))

(defcheck solution-5b230d67
  (fn sp [coll]
    (->> coll
      (reduce
        (fn [[l & r] e]
          (if (or (empty? l) (> e (last l)))
            (cons (conj l e) r)
            (cons [e] (cons l r))))
        '([]))
      (map #(if (< 1 (count %)) % []))
      (apply max-key count)
      )))

(defcheck solution-5b29c9cc
  (fn longest-inc-subseq
    [sequence]
    (loop [subsequences '[]
           current-seq '[]
           remaining-seq sequence
           max-length 0]
      (let [next-element (first remaining-seq)
            last-element (last current-seq)
            last-element (if (nil? last-element) 0 last-element)
            ;; prevents number operations by substituting
            ;; a nil first element in current-seq with 0
            current-seq' ;; keep cojoining the subsequence if the next element is larger
            ;; otherwise make a new sequence containing the next element
                         (if (and (not (nil? next-element))
                                  (> next-element last-element))
                           (conj current-seq next-element)
                           [next-element])
            current-seq-length
                         (count current-seq')
            max-length'
                         (if (> current-seq-length max-length)
                           current-seq-length
                           max-length)
            subsequences' ;; add every subsequence to this sequence of sequences
                         (conj subsequences current-seq')
            remaining-seq' (rest remaining-seq)]
        (if (empty? remaining-seq)
          (let
           [longest-inc-subsequence
            (first (filter #(= max-length (count %)) subsequences'))]
            (if (< (count longest-inc-subsequence) 2) ;; if the longest subsequence
              ;; is less than two elements long, return an empty sequence
              '[]
              longest-inc-subsequence))
          (recur subsequences' current-seq' remaining-seq' max-length'))))))

(defcheck solution-5b362401
  (fn [coll]
    (let [res
          (reduce #(if (< (count %1)  (count %2)) %2 %1)
            (reduce
              (fn [bvec e]
                (let [lstvec (last bvec)]
                  (if (= (last lstvec) (dec e))
                    (conj (into [] (drop-last bvec))
                      (conj lstvec e))
                    (conj bvec [e]))
                  )
                )
              [[(first coll)]]
              (rest coll)
              )
            )
          ] (if (< (count res) 2) [] res))
    ))

(defcheck solution-5b7d5686
  (fn [coll]
    (letfn [(state-continue-subseq [curr acc results]
              {:prev curr :acc (conj acc curr) :results results})
            (state-new-subseq [curr acc results]
              {:prev curr :acc [curr] :results (conj results acc)})
            (result-extract [state]
              (if (seq (:acc state))
                (conj (:results state) (:acc state))
                (:results state)))
            (inc-subseq-reducer [state curr]
              (let [prev (:prev state)
                    acc (:acc state)
                    results (:results state)]
                (if (> curr prev)
                  (state-continue-subseq curr acc results)
                  (state-new-subseq curr acc results))))]
      (let [state-initial {:prev (first coll)
                           :acc [(first coll)]
                           :results []}
            last-state (reduce inc-subseq-reducer
                         state-initial
                         (rest coll))
            inc-subseqs (result-extract last-state)
            longest-arg (partial max-key count)
            first-longest #(apply longest-arg (reverse %))
            longest-subseq (first-longest inc-subseqs)]
        (if (> (count longest-subseq) 1)
          longest-subseq
          [])))))

(defcheck solution-5ba3fb37
  (fn longest-sub-seq [xs]
    (let [res (reduce
                (fn [m x]
                  (if (< (last (:c m)) x)
                    (assoc m :c (conj (:c m) x))
                    (if (< (count (:p m)) (count (:c m)))
                      (assoc m :p (:c m) :c [x])
                      (assoc m :c [x]))))
                {:p [] :c [(first xs)]} (rest xs))]
      (let [r (if (>= (count (:p res)) (count (:c res))) (:p res) (:c res))]
        (if (> (count r) 1) r [])))))

(defcheck solution-5c4c7da9
  (fn [lmasuk]
    (let [bar (fn [ls]
                (let [va (vec (take 1 ls))
                      cb (count ls)]
                  (loop [i 1 v va flag false]
                    (if (or flag (= i cb))
                      v
                      (let [sequ? (= (last v) (dec (nth ls i)))]
                        (recur (inc i)
                          (if  sequ? (conj v (nth ls i)) v)
                          (if sequ? false true)))))))]
      (loop [i 0, v []]
        (if (= i (count lmasuk))
          (if (> (count v) 1) v [])
          (recur (inc i)
            (let [v1 (bar (drop i lmasuk))]
              (if (> (count v1) (count v)) v1 v))))))))

(defcheck solution-5c636dc6
  (fn l [c]
    (let [take-seq (fn [n p c]
                     (let [h (count (take-while #(apply p %) (partition n 1 c)))]
                       (take (+ n h -1) c)))
          chop (fn [c] (for [n (range (count c))] (drop n c)))
          parts (chop c)
          seqs (map (partial take-seq 2 #(= (inc %) %2)) parts)
          longest (apply max-key count seqs)]
      (if (< (count longest) 2)
        []
        longest))))

(defcheck solution-5d26c462
  (fn [i]
    (vec (last (sort (filter #(< 1 (count %))
                       (reduce
                         (fn [o n]
                           (if (= (dec n) (last (last o)))
                             (assoc o (dec (count o)) (conj (last o) n))
                             (conj o [n])))
                         []
                         i)))))))

(defcheck solution-5d73d943
  (fn [c]
    (loop [n (count c)]
      (let [y (some #(when (= % (-> % distinct sort)) %) (partition n 1 c))]
        (cond
          (< n 2) []
          y y
          1 (recur (- n 1)))))))

(defcheck solution-5dd95019
  #_(let [c count
          test #(and (> (c %1) 1)
                     (> (c %1) (c %2)))]
      #(loop [s % t [] r []]
         (if (seq s)
           (if (seq t)
             (if (< (last t) (first s))
               (recur (rest s) (conj t (first s)) r)
               (if (test t r)
                 (recur s [] t)
                 (recur s [] r)))
             (recur (rest s) [(first s)] r))
           (if (test t r) t r))))

  #_(fn [s]
      (letfn [(subseqs [s]
                (if (seq s) (concat (subseqs (rest s))
                                    (reductions conj [] s))))
              (big-enough? [s] (> (count s) 1))
              (increasing? [s] (apply < s))]
        (apply max-key count (conj (filter (every-pred big-enough? increasing?) (subseqs s)) []))))
  (fn [s]
    (apply max-key count (conj (filter #(and (> (count %) 1) (apply < %))
                                 (loop [s s r []]
                                   (if (seq s)
                                     (recur (rest s) (concat (reductions conj [] s) r))
                                     r))) []))))

(defcheck solution-5de9c9dc
  #(
     let [x (
             (fn longest-subseq [maxs news coll]
               (if (empty? coll)
                 (if (> (count news) (count maxs))
                   news
                   maxs
                   )
                 (if (>= (last news) (first coll))
                   (if (> (count news) (count maxs))
                     (longest-subseq news [(first coll)] (rest coll))
                     (longest-subseq maxs [(first coll)] (rest coll))
                     )
                   (longest-subseq maxs (conj news (first coll)) (rest coll))
                   )
                 )
               )
             [] [(first %)] (rest %))]
     (if (> (count x) 1)
       x
       []
       )
     ))

(defcheck solution-5e34dff6
  #(let [res (second (reduce (fn [r x]
                               (let [[cur long] r]
                                 (if (or (empty? cur) (= x (inc (last cur))))
                                   (let [new (conj cur x)]
                                     (if (> (count new) (count long))
                                       [new new]
                                       [new long]))
                                   [[x] long])
                                 )) [[] []] %))]
     (if (>= (count res) 2) res [])))

(defcheck solution-5e3835b6
  (fn [src]
    (vec(last(filter #(> (count %) 1) (sort-by count(loop [x src index 0 y []]
                                                      (if (> (inc index)(count x)) (conj y x)
                                                                                   (if (or (= 0 index) (= (inc (get x (dec index)))(get x index)))
                                                                                     (recur x (inc index) y)
                                                                                     (let [spl (split-at index x) ]
                                                                                       (recur (vec(last spl)) 0 (conj y (vec(first spl))))
                                                                                       )
                                                                                     )
                                                                                   )
                                                      )
                                        ))))
    ))

(defcheck solution-5e82824c
  (fn lis [coll]
    (letfn
     [(helper [a]
        (loop [k 0 q (vec (repeat (count a) nil)) P (vec (repeat (count a) nil))]
          (if (= k (count a))
            [q P]
            (let [m (loop [j 0 result 0 max-idx nil]
                      (if (= j k)
                        [result max-idx]
                        (if (> (a k) (a j))
                          (if (> (q j) result)
                            (recur (inc j) (q j) j)
                            (recur (inc j) 1 j))
                          (if (= (a k) (a j))
                            (recur (inc j) 0 nil)
                            (recur (inc j) result max-idx)))))]
              (recur (inc k) (assoc q k (inc (first m))) (assoc P k (second m)))))))]
      (let [result (helper coll)
            L (first result)
            P (second result)
            idx (.indexOf L (apply max L))]
        (if (= (P idx) nil)
          []
          (loop [idx idx result []]
            (cond (= (P idx) nil)	(cons (coll idx) result)
                  :else (recur (P idx) (cons (coll idx) result)))))))))

(defcheck solution-5eaeb6c9
  ; First create the list of all the elements of the input vector in ascending order without doubles.
  ; Its sub-lists (len >= 2) represent all possible increasing sub-seqs.
  ; Now create all sub-lists of this meta list, longest first:
  ;
  (fn max-inc-sub-seq[coll]
    (letfn [
            (searchvecs [col]
              (let [v (distinct (sort col))]
                (for[n (range (count v) 1 -1) m (range (inc (-(count v) n)))] (take n (nthrest v m)))))

            ;Iterate this meta-list and search the contained sublists in the input vector:

            (subseqs [c1 c2]
              (for [n (range (- (count c2) (dec (count c1))))]
                (take (count c1) (nthrest c2 n))))

            (getsubs[col1 col2]
              (some
                (fn[x] (if (= col1 x) col1)) (subseqs col1 col2)))
            ]

      ;Return the first meta-list subsequence which is found in the input vector (cast list to vector) or [] if none was found:

      (let [v (drop-while nil? (for [v (searchvecs coll)] (getsubs v coll)))]
        (if (empty? v)
          []
          (vec (first v)))))))

(defcheck solution-5f0497ab
  (fn [l]
    (let [result (last
                   (sort-by
                     #(count %)
                     (filter
                       #(= 1 (ffirst %))
                       (partition-by
                         #(first %)
                         (map-indexed
                           #(list %2 %)
                           (map - (rest l) (drop-last l)))))))]
      (if (nil? result) '()
                        (take (inc (count result)) (drop (second (first result)) l))))))

(defcheck solution-5f1db12e
  (fn [coll]
    (letfn [(amax-key [k colls] (when (seq colls) (apply max-key k colls)))
            (increasing? [[x1 x2]] (> x2 x1))
            (take-while-increasing [[x :as coll]]
              (if (empty? coll)
                coll
                (cons x (map second (take-while increasing?
                                      (map list coll (rest coll)))))))
            (partition-into-increasing [coll]
              (loop [acc (list)
                     coll coll]
                (if (empty? coll)
                  acc
                  (let [part (take-while-increasing coll)]
                    (recur (conj acc part) (drop (count part) coll))))))]
      (let [largest (amax-key count (partition-into-increasing coll))]
        (if (> (count largest) 1)
          largest
          [])))))

(defcheck solution-5f296f17
  (fn [w]
    (let [aux
          (fn [[head & tail] last-seen curr res]
            (cond
              (nil? head)
              (let [tmp (if (<=
                              (count curr)
                              (count res))
                          res
                          curr)]
                (if (< 1 (count tmp))
                  tmp
                  []))

              (< last-seen head)
              (recur tail head (conj curr head) res)

              :else
              (recur tail head [head] (if (< (count res)
                                            (count curr))
                                        curr
                                        res))))]
      (aux w -1 [] []))))

(defcheck solution-5f606532
  ; Phew !! Hack-o-rama - there must be an easier way.
  (fn longest-increasing-sub-seq [a-seq]
    (let
     [intermediate-result-a
      (reduce
        (fn [s e] ; (s)equence (e)lement
          (if (:prior s)
            (if (= e (inc (:prior s)))
              (->
                s
                (assoc :prior e)
                (assoc :curr (conj (:curr s) e)))
              (->
                s
                (assoc :prior e)
                (assoc :curr (vector e))
                (assoc :long (if (> (count (:curr s)) (count (:long s)))
                               (:curr s)
                               (:long s)))))
            {:prior e :curr (vector e) :long (list)}))
        {:prior nil :curr nil :long nil}
        a-seq)
      intermediate-result-b
      (if (> (count (:curr intermediate-result-a)) (count (:long intermediate-result-a)))
        (:curr intermediate-result-a)
        (:long intermediate-result-a))]
      (if (< (count intermediate-result-b) 2)
        (list)
        intermediate-result-b
        ))))

(defcheck solution-5fbec53b
  (fn [i]
    (let [[acc cur]
          (reduce (fn [[acc cur] n]
                    (if (= (last cur) (dec n)) [acc (conj cur n)] [(conj acc cur) [n]]))
            [[] [(first i)]] (rest i))

          largest (first (reverse (sort-by count (conj acc cur))))]
      (if (< 1 (count largest)) largest []))
    ))

(defcheck solution-5fdac1e2
  (fn [ns]
    (reduce (fn [mx cs] (if (and (> (count cs) (count mx)) (>= (count cs) 2)) cs mx))
      []
      (reduce (fn [rs n]
                (let
                 [sq (peek rs)]
                  (if (> n (peek sq))
                    (conj (pop rs) (conj sq n))
                    (conj rs [n])
                    ))) [[(first ns)]] (rest ns)))))

(defcheck solution-5ff7fd4a
  (fn [arr]
    (loop [resv []  t arr]
      (if (empty? t)
        (if (= (count resv) 1)
          []
          resv)
        (let [r (keep #(if % %) (map #(if (= % %2) %) (range (first t) (+ (count t) (first t)))
                                  t)),
              startidx (count r)]
          (recur (if(< (count r) (count resv))
                   resv
                   r)
            (drop startidx t)))))))

(defcheck solution-60099a80
  (fn [s](let [
               n (fn[x y](if (< (last (last x)) y)
                           (conj (vec(drop-last x))
                             (conj (last x) y))
                           (conj x [y])))
               p (reduce n [[(first s)]] (rest s))
               r (group-by count p)
               g (sort r)
               l (last g)
               d (first (second l))] (if (= (count d) 1) []
                                                         d)
                                     )))

(defcheck solution-60723747
  (fn [lst]
    (let [
          sufs  (map #(take (+ 1 %) lst) (range (count lst)))
          prefs (fn [lst] (map #(drop % lst) (range (count lst))))
          sups  (mapcat prefs sufs)
          asc   (filter #(apply < %) sups)
          longest (apply max-key count (reverse asc))
          atleast2 (if (< 1 (count longest)) longest [])]
      atleast2)))

(defcheck solution-60b4bd3
  (fn sub [v]
    (loop [s (conj v -0.1) ss [-0.1] lss []]
      (if (empty? s)
        lss
        (let [x (first s)]
          (if (= x (inc (last ss)))
            (recur (rest s) (conj ss x) lss)
            (recur (rest s) [x] (if (> (count ss) (max (count lss) 1)) ss lss))
            )
          )
        )
      )
    ))

(defcheck solution-60cfdcb6
  (fn tt [cc]
    (letfn [(t [c]
              (reverse
                (map reverse
                  (:data
                   (reduce (fn [{:keys [data before-ele] :as a} b]
                             (if (= (inc before-ele) b)
                               (assoc a :data (cons (conj (first data) b) (rest data))
                                        :before-ele b)
                               (assoc a :data (cons (list b) data)
                                        :before-ele b)))
                     {:data '()
                      :before-ele (first c)} c)))))
            (ans [data]
              (let [result (t data)
                    max-count (apply max (map count result))]
                (or (first
                      (filter (fn [d]
                                (and (>= (count d) 2) (= max-count (count d))))
                        result))
                    [])))]
      (ans cc))))

(defcheck solution-61bf3701
  (fn f2 [coll] (let [f1 (fn [coll]   (reduce #(if (>= (count %) (count %2)) % %2)
                                        (reductions (fn [x y] (cond (= (count x) 0) [y]
                                                                    (< (last x) y) (conj x y)
                                                                    :else [y])) [] coll)))]
                  (if (> (count (f1 coll)) 1) (f1 coll) []))))

(defcheck solution-61c04109
  (fn lgsub
    ([[a b :as ls]] (lgsub ls [a] [a]))
    ([[a b :as ls] curacc acc]
     (cond (or (nil? ls) (nil? b)) (if (>= (count acc) 2) acc [])
           (= (inc a) b)
           (lgsub (rest ls) (conj curacc b)
             (if (>= (count curacc) (count acc))
               (conj curacc b)
               acc))
           :else (lgsub (rest ls) [b] acc)))))

(defcheck solution-61ea04ed
  (fn [v]
    (->>
      (loop [ret []
             [fv & rvs] v
             l fv]
        (if fv
          (recur (if (> fv l) (update-in ret [(dec (count ret))] conj fv) (conj ret [fv])) rvs fv)
          ret))
      reverse
      (apply max-key count)
      (#(if (>= (count %) 2) % [])))))

(defcheck solution-6260d9a2
  (fn liss
    ([coll] (liss coll [] []) )
    ([coll found curr] (if(= (count coll) 0)
                         (if (> (count curr) 1)
                           (last (sort-by count (conj found curr)))
                           (if (> (count found) 0)
                             (last (sort-by count found))
                             []
                             )
                           )
                         (if (= (count curr) 0)
                           (liss (rest coll) found (conj curr (first coll)) )
                           (if (= (+ (last curr) 1) (first coll))
                             (liss (rest coll) found (conj curr (first coll)) )
                             (if (> (count curr) 1)
                               (liss coll (conj found curr) [] )
                               (liss coll found [])
                               )
                             )
                           )
                         )
     )
    ))

(defcheck solution-62a6a49e
  (fn [array]
    (loop [result  []
           temp    []
           elements array]
      (let [end (last temp)
            cur (first elements)]
        (if (seq elements)
          (if (or (nil? end) (> cur end))
            (if (> (inc (count temp)) (count result))
              (recur (conj temp cur) (conj temp cur) (rest elements))
              (recur result (conj temp cur) (rest elements)))
            (recur result [cur] (rest elements)))
          (if (> (count result) 1)
            result
            []))))))

(defcheck solution-635753d1
  (fn longest-increasing-subseq
    [coll]
    (loop [best []
           current []
           coll (partition-all 2 1 coll)]
      (if-let [s (seq coll)]
        (let [[x & xs] s
              new-current (conj current (first x))
              new-best (if (>= (count best) (count new-current)) best new-current)]
          (if (apply < x)
            (recur new-best new-current xs)
            (recur new-best [] xs)))
        (if (<= 2 (count best)) best [])))))

(defcheck solution-63623a0d
  (fn [l]
    (let [ p (partition 2 1 l)
          s (fn[[a b]] (= (inc a) b))
          a (partition-by s p)
          i (filter #(s (first %)) a)
          o (sort-by #(- 0 (count %)) i) ]
      (distinct (flatten (first o))))))

(defcheck solution-643ed78c
  (fn
    [v]
    (loop [result []
           cur-inc-seq []
           v v]
      (if (empty? v)
        (if (= 1 (count result))
          []
          result)
        (let [max-cnt (count result)
              cur-cnt (count cur-inc-seq)
              cur-ele (first v)
              last-ele (last cur-inc-seq)
              rest-eles (rest v)]
          (if (or (nil? last-ele) (< last-ele cur-ele))
            (if (= max-cnt cur-cnt)
              (recur (conj cur-inc-seq cur-ele) (conj cur-inc-seq cur-ele) rest-eles)
              (recur result (conj cur-inc-seq cur-ele) rest-eles))
            (recur result [cur-ele] rest-eles)))))))

(defcheck solution-645ec055
  (letfn [(better? [a b]
            (and (> (count a) 1) (> (count a) (count b))))
          (foo [[cur best] it]
            (if (or (empty? cur) (> it (last cur)))
              [(conj cur it) best]
              (if (better? cur best)
                [[it] cur]
                [[it] best])))]
    #(let [[c b] (reduce foo [[] []] %)]
       (if (better? c b) c b))))

(defcheck solution-64911f3f
  (fn longest-increasing-subsequence [numbers]
    (loop [longest '()
           current (take 1 numbers)
           remaining (rest numbers)]
      (if (empty? remaining)
        (let [result (max-key count current longest)]
          (if (< (count result) 2)
            []
            result))
        (recur (max-key count current longest)
          (if (> (first remaining) (last current))
            (concat current (take 1 remaining))
            (take 1 remaining))
          (rest remaining))))))

(defcheck solution-64c21391
  #(let [result (reverse (filter (fn [t] (> (count t) 1))
                           ((fn [l] (loop [prev -1, curr (first l), in l, out [], part []]
                                      (if (empty? in)
                                        (conj out part)
                                        (if (<= curr prev)
                                          (recur curr (fnext in) (next in) (conj out part) [curr])
                                          (recur curr (fnext in) (next in) out (conj part curr)))))) %)))]
     (if (empty? result)
       []
       (apply max-key count result))))

(defcheck solution-65528d04
  (fn [c]
    (let [f #(first %)
          m (map vector c (rest c))
          p (partition-by #(< (f %) (last %)) m)
          r (fn [c] (reduce #(concat %1 [(last %2)]) (f c) (rest c)))
          a (filter #(< (f %) (last %)) (map r p))]
      (if (empty? a) '() (apply max-key count (reverse a))))))

(defcheck solution-657c1792
  (fn [ls]
    (:best
     (reduce (fn [acc v]
               (let [{:keys [best curr]} acc
                     curr (if (or (empty? curr)
                                  (= v (inc (last curr))))
                            (conj curr v)
                            [v])
                     best (if (and (> (count curr) (count best))
                                   (> (count curr) 1))
                            curr
                            best)]
                 {:best best :curr curr})
               ) {:best [] :curr []} ls))))

(defcheck solution-657fc353
  (fn [s]
    (->> (reduce #(if (> %2 (ffirst %1))
                    (cons (cons %2 (first %1)) (rest %1))
                    (cons [%2] %1)
                    ) [[(first s)]] (rest s)) (filter #(> (count %) 1)) (sort-by count) last reverse)))

(defcheck solution-65967b24
  (fn [col]
    (loop [i 0 s1 [] s2 []]
      (if (< i (count col))
        (recur (inc i)
          (if (= (last s1) (dec (col i)))
            (conj s1 (col i))
            (conj [] (col i)))
          (if (> (count s1) (count s2))
            s1
            s2))
        (if (> (count s1) (count s2)) (if (> (count s1) 1) s1 []) (if (> (count s2) 1) s2 []))))))

(defcheck solution-65a30d93
  (fn [xs] (or (->>
                 (map vector xs (range))
                 (partition-by #(apply - %))
                 (map #(map first %))
                 (filter #(> (count %) 1))
                 (sort-by (comp - count))
                 first) [])))

(defcheck solution-65c26097
  (fn [col]
    (let [seqs  (let [lastseen (atom (dec (first col)))
                      counter  (atom 1)]
                  (partition-by (fn [v]
                                  (if-not (= (inc @lastseen) v)
                                    (reset! counter (inc @counter)))
                                  (reset! lastseen v)
                                  @counter) col))
          longest (first (sort-by count > seqs))]
      (if (> (count longest) 1)
        longest
        []))
    ))

(defcheck solution-65f71199
  #(let [[a b e] (reduce (fn [[m c p] e]
                           (if (> e p)
                             [m (conj c p) e]
                             (if (> (inc (count c)) (count m))
                               [(conj c p) [] e]
                               [m [] e])))
                   [[] [] (first %)] (rest %))
         r (if (>= (count a) (inc (count b)))
             a
             (conj b e))]
     (if (>= (count r) 2)
       r
       [])))

(defcheck solution-66290c5f
  (fn [l]
    (loop [l l candidate [] best []]
      (if (empty? l)
        best
        (let [
              n (first l)
              candidate (if
                         (or (empty? candidate) (< (peek candidate) (first l)))
                          (conj candidate n)
                          [n])]
          (recur
            (rest l)
            candidate
            (if
             (and (> (count candidate) 1) (> (count candidate) (count best)))
              candidate
              best)))))))

(defcheck solution-66c5d5ab
  (comp
   #(or (first %) [])
   (partial sort #(> (count %1) (count %2)))
   (partial filter #(> (count %) 1))
   (fn [c]
     (loop [o [[(first c)]] [f & r] (rest c)]
       (if (nil? f)
         o
         (if-not (= f (inc (last (last o))))
           (recur (conj o [f]) r)
           (recur (conj (vec (butlast o)) (conj (last o) f)) r)))))))

(defcheck solution-6778a66a
  (fn [x] (let [v (apply max-key count (reverse (reductions (fn [y z] (if (< (last y) z) (concat y (list z)) (list z))) '(100) x)))] (if (= 1 (count v)) [] v))))

(defcheck solution-67802e26
  (fn [i]
    (loop [a i l -9999999 t [] r []]
      (if (empty? a)
        (if (and (> (count t) (count r)) (> (count t) 1)) t r)
        (if (> (first a) l)
          (recur (rest a) (first a) (conj t (first a)) r)
          (if (and (> (count t) (count r)) (> (count t) 1) )
            (recur (rest a) (first a) [(first a)] t)
            (recur (rest a) (first a) [(first a)] r)
            )
          )
        )
      )
    ))

(defcheck solution-681cecca
  (fn [coll]
    (let [longest-seq (->> (reduce (fn [acc item]
                                     (cond
                                       (= (ffirst acc) nil) (conj (rest acc) (conj (first acc) item))
                                       (= (inc (ffirst acc)) item) (conj (rest acc) (conj (first acc) item))
                                       :else (conj acc (list item))))
                             '(())
                             coll)
                        (sort-by count)
                        last
                        reverse
                        vec)]
      (if (> (count longest-seq) 1)
        longest-seq
        []))))

(defcheck solution-689f09bc
  (fn [coll]
    (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                          (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                            (partition-by
                              #(< (first %) (last %))
                              (partition 2 1 coll)))))]
      (if (empty? seqs)
        []
        (apply (partial max-key count) seqs)))))

(defcheck solution-68a1bbef
  (fn [v]
    (let [t (partition-by identity ((fn [s] (map #(< %1 %2) s (drop 1 s))) v))
          [i l b] (reduce
                    (fn [[i l b] c]
                      (let [k (count c)
                            j (+ i k)]
                        (if (and (first c) (> k l))
                          [ j k i ]
                          [ j  l b ])))
                    [0 -1 0] t)]
      (subvec v b (+ b l 1)))))

(defcheck solution-690a036d
  (fn [s]
    (let [f (fn [{current :current
                  longest :longest
                  :or {current [] longest []}}
                 e]
              (if (or (empty? current) (> e (last current)))
                (let [new-current (conj current e)]
                  {:current new-current
                   :longest (if (and (>= (count new-current) 2)
                                     (> (count new-current) (count longest)))
                              new-current
                              longest)})
                {:current [e]
                 :longest longest}))]
      (:longest (reduce f {} s)))))

(defcheck solution-69805236
  (fn longest-increasing-sub-seq [xs]
    (let
     [
      find-longest
      (fn find-longest [longest-seq current-seq xs]
        (if (empty? xs)
          (if
           (> (count current-seq) (count longest-seq))
            current-seq
            longest-seq
            )
          (let
           [
            v (first xs)
            xs' (rest xs)
            ]

            (if
             (= v (inc (last current-seq)))

              ; continue current seq
              (find-longest
                longest-seq
                (conj current-seq v)
                xs'
                )

              ; stop current seq
              (if (> (count current-seq) (count longest-seq))
                ; found new longest seq
                (find-longest
                  current-seq
                  [v]
                  xs'
                  )

                (find-longest
                  longest-seq
                  [v]
                  xs'
                  )
                )
              )
            )
          )
        )

      longest
      (find-longest
        []
        [(first xs)]
        (rest xs)
        )
      ]
      (if
       (= 1 (count longest))
        []
        longest
        )
      )
    ))

(defcheck solution-69b418be
  (fn [lst]
    (let [
          ; Generating all subsequences of the list,
          ; and leaving ordered sequences only
          subpartitions (for [i (range (inc (count lst)) 1 -1)]
                          (filter #(apply < %) (partition i 1 lst)))
          ; Grouping them by their respective size
          seqs (group-by #(count %) (map first subpartitions))
          ; Finally, selecting the longest ordered subsequence
          max-seq (last (get seqs (apply max (keys seqs))))]
      (if (> (count max-seq) 1)
        max-seq
        []))))

(defcheck solution-69d89f7
  (fn [coll]
    (let
     [maxsub (apply max-key count
               (reverse (loop [ s (seq coll) res [] ]
                          (if (empty? s)
                            res
                            (let
                             [group
                              (loop [xs (next s) cur [(first s)]]
                                (if (or (empty? xs) (>= (last cur) (first xs)))
                                  [xs cur]
                                  (recur (rest xs) (conj cur (first xs)))))]
                              (recur (first group) (conj res (second group))))))))]
      (if (< (count maxsub) 2) [] maxsub))))

(defcheck solution-6a11b225
  (fn [col]
    (loop [c col
           buf []
           longests []]
      (let [x (first c)
            l (peek buf)
            bc (count buf)
            b-added (if (>= bc 2) (conj longests buf) longests)]
        (if x
          (if (= (dec x) l)
            (recur (rest c) (conj buf x) longests)
            (recur (rest c) [x] b-added))
          (if (seq b-added)
            (apply max-key count b-added)
            b-added))))))

(defcheck solution-6a604e6b
  (fn [s]
    (->> (for [st (range (count s)) e (range st (inc (count s)))] (drop st (take e s)))
      (remove #(< (count %) 2))
      (filter #(apply < %))
      (reverse)
      (cons [])
      (apply max-key count))))

(defcheck solution-6a64a7db
  (fn [col]
    (let [sublists (take-while
                     #(not (empty? %))
                     (iterate #(drop 1 %) col))
          ll (apply max-key count
               (map (fn [l] (some
                              #(if (= % (range (first %) (+ (first %) (count %)))) % false)
                              (take (- (count l) 1) (iterate drop-last l))))
                 sublists))]
      (if ll ll []))))

(defcheck solution-6ade321e
  #(->> %3
     (reductions % nil)
     (sort-by count)
     last
     %2) #(if (and % (= 1 (- %2 (peek %)))) (conj % %2) [%2]) #(if (second %) % []))

(defcheck solution-6b831f26
  (fn [x]
    (let [
          s (mapcat #(take-while seq (iterate butlast %)) (take-while seq (iterate rest x)))
          s (filter #(and (apply < %) (> (count %) 1)) s)]
      (reduce #(if (> (count %2)(count %)) %2 %) [] s))))

(defcheck solution-6bd83323
  (fn longest-subseq
    [collection]
    (loop [c collection
           longest []
           current []
           prev nil]
      (let [f (first c)
            rem (next c)
            new-current (conj current f)]
        (if c
          (if (or (nil? prev) (> (first c) prev))
            (if (= current longest)
              (recur rem (conj longest f) new-current f)
              (if (= (count current) (count longest))
                (recur rem new-current new-current f)
                (recur rem longest new-current f)))
            (recur rem longest [f] f))
          (if (= (count longest) 1)
            []
            longest))))))

(defcheck solution-6c49cfe3
  (fn __ [coll]
    (let [longest (->> coll
                    (partition 2 1)
                    (map (fn [[curr next]] [(= 1 (- next curr)) curr next]))
                    (partition-by first)
                    (filter #(first (first %)))
                    (sort-by count)
                    last)]
      (reduce (fn [res [inc? curr next]] (conj res next)) (-> longest first rest vec) (rest longest)))))

(defcheck solution-6c92a66f
  (fn [[x & xs]]
    (let [[ys zs] (reduce
                    (fn [[last-acc, cur-acc] x]
                      (if (= (last cur-acc) (dec x))
                        [last-acc, (conj cur-acc x)]
                        (if (> (count cur-acc) (count last-acc))
                          [cur-acc, [x]]
                          [last-acc, [x]])))
                    [[] [x]]
                    xs)
          result (if (> (count ys) (count zs)) ys zs)]
      (if (> (count result) 1) result []) )))

(defcheck solution-6d144a30
  (fn longest [s]
    (:l (reduce
          (fn [h r]
            (if (= 1 (- r (last (h :s ))))
              (let [ns (conj (h :s ) r), cnt (count ns)]
                (if (and (>= cnt 2) (> cnt (count (h :l ))))
                  {:l ns :s ns}
                  (assoc h :s ns)))
              (assoc h :s [r])))
          {:l [] :s [0]} s))))

(defcheck solution-6d2f5ea3
  (fn [xs]
    (loop [xs (seq xs) l [] ll l]
      (if xs
        (if (or
             (and (empty? l) (next xs))
             (= (peek l) (dec (first xs))))
          (recur (next xs) (conj l (first xs)) ll)
          (recur (next xs) [(first xs)] (if (> (count l) (count ll)) l ll)))
        (cond
          (and (> (count l) (count ll)) (> (count l) 1)) l
          (and (> (count ll) (count l)) (> (count ll) 1)) ll
          :default [])))))

(defcheck solution-6d926ba6
  #(letfn [(y [l c m]
             (letfn [(sm [a b] (if (> (count a) (count b)) a b))]
               (if (seq l)
                 (let [[h & t] l]
                   (if (> h (last c))
                     (recur t (conj c h) m)
                     (recur t [h] (if (> (count c) 1) (sm c m) m))))
                 (if (> (count c) 1) (sm c m) []))))] (y (rest %) [(first %)] [])))

(defcheck solution-6e122e12
  (fn [xs]
    (letfn [(getSub [xs]
              (loop [res [], ys xs]
                (cond
                  (empty? ys) res
                  (empty? (rest ys)) (conj res (first ys))
                  :else (if (< (first ys) (second ys))
                          (recur (conj res (first ys)) (rest ys))
                          (conj res (first ys))))))
            (getSubs [xs]
              (if (empty? xs)
                []
                (cons (getSub xs) (getSubs (rest xs)))))]
      (->> (getSubs xs)
        (filter #(> (count %) 1))
        (sort #(> (count %1) (count %2)))
        (first)
        (#(if (nil? %) [] %))))))

(defcheck solution-6e476499
  (fn [s]
    (remove #(= -2 %) (last (last (last (sort (group-by count
                                                (reduce (fn [[f & r :as l]  i] (if (= i (inc (last f))) (cons (conj f i) r) (cons [i] l)))
                                                  '([-2]) s)))))))))

(defcheck solution-6e98fd53
  (fn [xs]
    (loop [xs xs, ys [], zs []]
      (if (seq xs)
        (if (or (empty? ys) (= (first xs) (inc (last ys))))
          (recur (next xs) (conj ys (first xs)) zs)
          (recur xs [] (if (> (count ys) (max 1 (count zs))) ys zs)))
        (if (> (count ys) (max 1 (count zs))) ys zs)))))

(defcheck solution-6ea075eb
  (fn longest [s]
    (first
      (reduce (fn [[acc cur] elem]
                (if (seq cur)
                  (if (< (last cur) elem)
                    (if (>= (count cur) (count acc))
                      [(conj cur elem) (conj cur elem)]
                      [acc (conj cur elem)])
                    [acc [elem]])
                  [acc [elem]]))
        [[] []] s))))

(defcheck solution-6eb24e18
  (fn [coll]
    (flatten (take 1
               (filter
                 (fn [c] (every? #(< (first %) (last %)) (partition 2 1 c)))
                 (mapcat
                   #(partition % 1 coll)
                   (reverse (range 2 (count coll)))))))))

(defcheck solution-6ebf2c3d
  #(let [res (first (reduce (fn [acc e]

                              (let [current (if (= (dec e) (last (second acc))) (concat (second acc) [e]) [e])]
                                [(if (> (count current) (count (first acc))) current (first acc)) current]
                                )) [[] []] %))] (if (second res) res [])))

(defcheck solution-6eed047b
  (fn [xs]
    (->> (partition 2 1 xs)
      (partition-by (fn [[a,b]] (< a b)))
      (filter (fn [ys] (< (first (first ys)) (second (first ys)))))
      (map (fn [i ys] [(- (count ys)) i ys]) (range))
      (sort)
      first
      ((fn [[_ _ ys]]
         (if-not ys []
                    (apply vector (first (first ys)) (map second ys))))))))

(defcheck solution-6ef05d41
  (fn [s]
    (last
      (sort-by count
        ((fn [s n c r]
           (cond (empty? s) (conj r c)
                 (= (inc n) (first s)) (recur (rest s) (first s)
                                         (apply conj c (if (empty? c) (vector n (first s)) (vector (first s)))) r)
                 :else (recur (rest s) (first s) [] (conj r c))))
         (rest s) (first s) [] [])))))

(defcheck solution-6efc2efa
  (fn [l]
    (->>
      (reduce
        #(let [last-l (last %1)
               last-e (last last-l)]
           (if (or (nil? last-e) (>= last-e %2))
             (conj %1 [%2])
             (conj (vec (butlast %1)) (conj last-l %2))))
        [] l)
      (reduce #(if (< (count %1) (count %2)) %2 %1))
      ((fn [x] (if (< 1 (count x)) x []))))))

(defcheck solution-6f25f8bd
  (fn [s]
    (loop [remaining (rest s) cur-seq [(first s)] longest-seq [(first s)]]
      (if (empty? remaining)
        (if (> (count cur-seq) (count longest-seq))
          (if (> (count cur-seq) 1)
            cur-seq
            [])
          (if (> (count longest-seq) 1)
            longest-seq
            []))
        (if (> (first remaining) (last cur-seq))
          (recur (rest remaining) (conj cur-seq (first remaining)) longest-seq)
          (if (> (count cur-seq) (count longest-seq))
            (recur (rest remaining) [(first remaining)] cur-seq)
            (recur (rest remaining) [(first remaining)] longest-seq)))))))

(defcheck solution-6f73d66c
  (fn [sq]
    (reduce
      (fn [max s]
        (if (and (< (count max) (count s))
                 (< (first s) (second s)))
          s
          max))
      []
      (map #(take-nth 2 (cons (ffirst %) (flatten %)))
        (partition-by (fn [[a b]] (if (< a b) :ok a))
          (partition 2 1 sq))))))

(defcheck solution-6f77a3be
  (fn [coll]
    (let [l (count coll)
          tails (take l (iterate rest coll))
          gatherFn (fn gather [[n1 n2 & _ :as coll]]
                     (cond
                       (empty? coll) nil
                       (or (nil? n2) (> n1 n2)) (list n1)
                       (< n1 n2) (cons n1 (lazy-seq (gather (rest coll))))))
          pick-longest (fn [coll] (reduce #(let [l1 (count %1) l2 (count %2)] (if (>= l1 l2) %1 %2)) coll))]
      (let [longest (pick-longest (map gatherFn tails))] (if (>= (count longest) 2) longest [])))))

(defcheck solution-6fddd641
  (fn [s]
    (let [ans
          (loop [[x & xs] s
                 prev '()
                 best '()]
            (let [count-best (count best)
                  count-prev (count prev)]
              (if (nil? x)
                (if (< count-best count-prev)
                  prev
                  best)
                (let [p (first prev)]
                  (if (and p (< p x))
                    (let [new-prev (conj prev x)]
                      (if (< count-best count-prev)
                        (recur xs new-prev new-prev)
                        (recur xs new-prev best)))
                    (recur xs (conj nil x) best))))))]
      (if (< (count ans) 2)
        []
        (reverse ans)))))

(defcheck solution-6ffa60f8
  #(let [xxs (filter (fn [xs]
                       (let [[e1 e2] (first xs)]
                         (> e2 e1)))
               (partition-by (fn [[e1 e2]] (< e1 e2))
                 (partition 2 1 %)))]
     (if (empty? xxs)
       []
       (let [ss (map (fn [xs] (conj (vec (map first xs)) (second (last xs))))
                  xxs)]
         (reduce (fn [xs1 xs2] (if (> (count xs2) (count xs1)) xs2 xs1)) ss)
         ))))

(defcheck solution-70143586
  (fn [xs]
    (->> xs
      (map vector (range))
      (partition-by #(apply - %))
      reverse
      (apply max-key count)
      (map last)
      (#(if (< (count %) 2) [] %)))))

(defcheck solution-7061f601
  (fn liss [s]
    (loop [[x y :as s] s, result [], midresult []]
      (if (empty? s)
        result
        (if (= y (inc x))
          (if (empty? midresult)
            (recur (rest s) result [x y])
            (recur (rest s) result (conj midresult y)))
          (if (> (count midresult) (count result))
            (recur (rest s) midresult [])
            (recur (rest s) result []))
          )))))

(defcheck solution-7150fd69
  (fn monotinc
    ([s] (monotinc s [] []))
    ([s rez tmp] (if (empty? s)
                   (if (> (count tmp) (count rez))
                     (if (> (count tmp) 1) tmp [])
                     (if (> (count rez) 1) rez []))
                   (if (or (empty? tmp) (> (first s) (last tmp)))
                     (monotinc (rest s) rez (conj tmp (first s)))
                     (if (> (count tmp) (count rez))
                       (monotinc (rest s) tmp [(first s)])
                       (monotinc (rest s) rez [(first s)])))))))

(defcheck solution-7188bb45
  (fn [inp]
    (let [push-back (fn[col x] (reverse (cons x (reverse col))))
          addl (fn [ccoll, x]
                 (if (= 1 (count ccoll))
                   (list (push-back (first ccoll) x))
                   (concat (butlast ccoll) (list (push-back (last ccoll) x))  )))
          partition-seq (fn [l]
                          (reduce
                            #(if (= (dec %2) (last(last %)))
                               (addl % %2)
                               (reverse (cons (list %2) (reverse %) )))
                            '(()) l))
          run (apply max-key count (partition-seq inp))]
      (if (= 1 (count run)) [] run))))

(defcheck solution-72befdff
  (fn [xs]
    (first
      (reduce
        (fn [[best curr] x]
          (let [curr (cond
                       (empty? curr)      [x]
                       (>= (peek curr) x) [x]
                       :else              (conj curr x))
                cnt (count curr)
                best (cond
                       (<= cnt 1)            best
                       (<= cnt (count best)) best
                       :else                 curr)]
            [best curr]))
        [[] []]
        xs))))

(defcheck solution-730de6ca
  (fn [s] (or (first (last (vals (group-by count (sort-by #(count %) (filter #(or (<= (count %) 1) (< (first %) (second %))) (map #(concat (map first %) (list (last (last %))))
                                                                                                                               (partition-by #(< (first %) (second %)) (map list s (rest s))))))))))
              [])))

(defcheck solution-73664055
  (fn [v] (->> v
            (partition 2 1 ,,)
            (map (fn [[a b]] (vector (- b a) a b)) ,,)
            (partition-by #(> (first %) 0) ,,)
            (filter #(> (first (first %)) 0) ,,)
            (reduce #(if (> (count %2) (count %1)) %2 %1) [] ,,)
            (map rest ,,)
            (flatten ,,)
            (#(concat (list (first %)) % (list (last %))) ,, )
            (partition 2 ,,)
            (map first ,,)
            (filter #(not= nil %) ,,))))

(defcheck solution-73b36ccb
  (fn [v]
    (loop [vv [] v v tv []]
      (cond
        (empty? v) (let [a (last (last (apply sorted-map (mapcat (juxt count identity) (conj vv tv)))))]
                     (if (= (count a) 1) [] a))
        (= (last tv) nil) (recur vv (rest v) (conj tv (first v)))
        (= (first v) (+ 1 (last tv))) (recur vv (rest v) (conj tv (first v)))
        :else (recur (conj vv tv) (rest v) [(first v)])))))

(defcheck solution-73f3bc97
  (fn inc-sub [s]
    (letfn [(valid? [s] (= s (take (count s) (iterate inc (first s)))))
            (subs [n] (partition n 1 s))]
      (into [] (first (filter valid? (mapcat subs (range (count s) 1 -1))))))))

(defcheck solution-741d3eba
  (fn [l]
    (let [
          f2 (fn [r x]
               (if (or (empty? x) (>= (last r) (first x)))
                 r
                 (recur (conj r (first x)) (rest x))))
          f1 (fn [x] (f2 [(first x)] (rest x)))
          lst (filter
                #(> (count %) 1)
                (map
                  f1
                  (take (count l) (iterate rest l))))]
      ((fn [x ls]
         (let [y (first (filter #(> (count %) (count x)) ls))]
           (if (nil? y)
             (or x [])
             (recur (first ls) (rest ls)))))
       (first lst) (rest lst))
      )))

(defcheck solution-74830054
  (fn
    [v]
    (loop [rs [] cur [] c v]
      (if (empty? c)
        (if (and (< 1 (count cur))
                 (< (count rs) (count cur)))
          cur rs)
        (let [e (first c)]
          (if (or (empty? cur) (< (last cur) e))
            (recur rs (concat cur [e]) (rest c))
            (recur (if (and (< 1 (count cur))
                            (< (count rs) (count cur)))
                     cur rs)
              [e] (rest c))))))))

(defcheck solution-75c2e7d8
  (fn [s]
    (or (first (filter #(apply < %) (mapcat #(partition % 1 s) (range (count s) 1 -1))))
        [])))

(defcheck solution-75cbb600
  (fn [x]
    (reduce #(if (> (count %2) (max 1 (count %))) %2 %)
      []
      (partition-by #(< % 0)
        (loop [a (first x)
               b (second x)
               c (-> x rest rest)
               v []]
          (if (> (count c) 0)
            (recur (if (if (> a 0) (< a b) (< (- 0 (inc a)) b)) b (- 0 (inc b)))
              (first c)
              (rest c)
              (concat v (if (> a 0) [a] [a (- 0 (inc a))])))
            (concat v (if (or (< a 1) (< a b)) [a b] [a (- 0 (inc b)) b]))))))))

(defcheck solution-773027dd
  (fn [s]
    (last (last (reductions
                  (fn [[ss b] x]
                    (let [nss (if (or (empty? ss) (> x (last ss))) (conj ss x) [x])
                          nb (if (> (count nss) (max 1 (count b))) nss b)]
                      [nss nb])) [[] []] s)))))

(defcheck solution-773bdb6
  (fn [s]
    (letfn [(all-subs [s len]
              (let [n (- (count s) (dec len))]
                (map (fn [e] (take len (drop e s))) (range n))))
            (all-increasing [s]
              (for [e (range (dec (count s)) 1 -1)] (some #(if (apply < %) %) (all-subs s e))))]
      (let [r (some #(if (not (nil? %)) %) (all-increasing s))]
        (if r r [])))))

(defcheck solution-7744b616
  (fn [xs]
    ((fn [x] (if (< (count x) 2) [] x))     ; have a length of 2 or greater
     (apply max-key count
       (reverse                      ; to make sure get the first
         (reductions
           #(if (or (empty? %1) (>= (last %1) %2))
              [%2]
              (conj %1 %2)) [] xs))))))

(defcheck solution-774ce48e
  (fn lis [s]
    (let [f
          (fn [acc s]
            (if (empty? s) acc
                           (recur
                             (if (and (not (empty? (first acc))) (= (inc (first (first acc))) (first s)))
                               (conj (drop 1 acc) (conj (first acc) (first s)))
                               (conj acc (list (first s))))
                             (rest s))))]
      (reverse (first (sort-by count > (filter #(>= (count %) 2) (f '() s))))))))

(defcheck solution-780528e7
  (fn [coll]
    (let [buffer-or-result
          (fn [buf res prev]
            (cond
              (empty? buf) res
              (>= (count buf)
                (count res)) (conj buf prev)
              :else res))]
      (loop [coll coll prev nil res [] buf []]
        (if-let [h (first coll)]
          (let [r (rest coll)]
            (if (and prev (= (- h prev) 1))
              (recur r h res (conj buf prev))
              (recur r h (buffer-or-result buf res prev) [])))
          (buffer-or-result buf res prev))))))

(defcheck solution-7871564f
  (fn g[c]
    (let [r (fn [[c & others :as colls] n]
              (if (= (dec n) (first c))
                (cons (cons n c) others)
                (cons [n] colls)))]
      (->> (reduce r [] c)
        (apply (partial max-key count))
        (reverse)
        (#(if (< 1 (count %)) % []))))))

(defcheck solution-78da5e09
  (fn [[f & c]]
    (let [sseq (fn [t v] (if (= v (inc (last t))) (conj t v) [v]))
          newc (reduce
                 #(if (= (count %1) (count %2)) %2 %1)
                 (reverse (sort-by count (reductions sseq [f] c))))]
      (if (> (count newc) 1) newc []))))

(defcheck solution-78faea92
  (fn [v] (->> v
            (#(map list % (rest %)))
            ((letfn [(sf [coll]
                       (if (seq coll)
                         (conj (sf (next coll)) (seq coll))
                         [()]))]
               sf))
            (map (partial take-while #(< (first %) (second %))))
            (filter not-empty)
            (map #(conj (map last %) (first (first %))))
            (reduce #(if (> (count %1) (count %2)) %1 %2) [])
            vec)))

(defcheck solution-792d9064
  (fn [x]
    (let [f #(take-while seq (iterate % %2))]
      (->> (f next x)
        (mapcat #(f butlast %))
        (filter #(and (< 1 (count %)) (apply < %)))
        reverse
        (sort-by count)
        last
        vec))))

(defcheck solution-7958db28
  ( fn[S]

    (reduce #(if (< (count %1) (count %2)) %2 %1 )
      (map #(if (< 1 (count %)) % [])(for [x (range (count S))]
                                       (for [y (range x (count S)) :while (or (= x y) (< (S (dec y)) (S y)) )]

                                         (S y)  )

                                       ))
      )

    ))

(defcheck solution-79be1a15
  (fn liss [s]
    (letfn [(iss [s]
              (loop [rv [(first s)] tmp (rest s)]
                (if (= (first tmp) (inc (last rv)))
                  (recur (conj rv (first tmp)) (rest tmp))
                  rv)))]
      (let [ss (map iss (take (count s) (iterate rest s)))
            m (apply max (map count ss))
            l (first (filter #(= (count %) m) ss))]
        (if (> (count l) 1)
          l
          [])))))

(defcheck solution-79ee299c
  (fn [lst]
    (second
      (reduce
        (fn [[n m] e]
          (if (= (inc (last n)) e) [(conj n e) m]
                                   [[e] (if (and (< 1 (count n))
                                                 (< (count m) (count n))) n m)]))
        [[(lst 0)] []]
        (rest (conj lst -1))))))

(defcheck solution-7a22c39f
  (fn lss [v]
    (loop [[h & t] v, lsf [], cur []]
      (if h
        (if (and t (> (first t) h))
          (recur t lsf (conj cur h))
          (if (>= (count cur) (count lsf)) (recur t (conj cur h) []) (recur t lsf [])))
        (if (next lsf) lsf [])      ))))

(defcheck solution-7ab63140
  (fn inc-subseq [xs]
    ((fn [xs] (if (empty? xs)
                []
                (reduce #(if (>= (count %1) (count %2)) %1 %2) xs)))
     (filter (fn [xs] (apply < xs))
       (reduce concat
         (for [x (->> xs count inc (range 2))
               :let [xall (partition x 1 xs)]]
           xall))))))

(defcheck solution-7ab6c992
  (fn incseq
    ([x] (incseq x [] []))
    ([x l c]
     (if (= (count x) 1)
       (if (> (count l) (inc (count c)))
         l
         (if (empty? c)
           c
           (conj c (first x))))
       (if (= (second x) (inc (first x)))
         (incseq (rest x) l (conj c (first x)))
         (if (> (count l) (inc (count c)))
           (incseq (rest x) l [])
           (incseq (rest x) (conj c (first x)) [])))))))

(defcheck solution-7b661633
  (fn [in]
    (let [reducer (fn [acc x]
                    (if
                     (not (seq (first acc))) (list (list x))
                                             (let [[a & as] acc
                                                   [b & bs] a
                                                   nxt (inc b)]
                                               (if (= nxt x) (cons (cons x a) as)
                                                             (cons (list x) acc)))))
          runs (reduce reducer (list (list)) in)]
      (reverse (last (vals (sort-by first (dissoc (apply merge (map (fn [s] {(count s) s}) runs)) 1)))))
      )))

(defcheck solution-7c5d8ad0
  (fn [s]
    (let [sss (for [n (range (count s) 1 -1)
                    ss (partition n 1 s)
                    :when (->> ss
                            (partition 2 1)
                            (map #(< (first %) (second %)))
                            (every? identity))]
                ss)
          n (concat sss '([]))]
      (first n))))

(defcheck solution-7cd2a6e8
  (fn iss [s]
    (let [lefts (cons nil (drop-last s)),
          info (map #(vector (= (- %1 1) %2) %1 %2) s lefts),
          groups (filter #(= true (first (first %))) (partition-by first info)),
          longest (first (sort #(> (count %1) (count %2)) groups))]
      (if (nil? longest)
        []
        (cons (nth (first longest) 2) (map second longest))))))

(defcheck solution-7cf42bd6
  (fn f [coll]
    (let [ diff (map #(if (= % 1 ) 1 0 ) (map - (rest coll ) (drop-last coll )))
          label-list  (partition-by #(= 1 %) diff) ]
      (if (and (= (count label-list) 1) (every? zero? (first label-list)))
        []
        (let [ maxl (apply max (map count (filter #(every? (complement zero?) %) label-list)))
              offset (apply + (map count (take-while #(or (not= maxl (count % )) (some zero? %) ) label-list)))]
          (take (+ maxl 1)  (drop offset coll)))))))

(defcheck solution-7d4b3182
  (fn lis [s]
    (let [helper (fn [[sofar ss] e]
                   (let [new-ss (if (empty? ss)
                                  [e]
                                  (if (> e (last ss))
                                    (conj                 ss e)
                                    [e]))]
                     [
                      (if (> (count new-ss) (count sofar)) new-ss sofar)
                      new-ss
                      ]
                     )
                   )
          ss     ((reduce helper [[] []] s) 0)
          ]
      (if (> (count ss) 1) ss []))))

(defcheck solution-7dc9995a
  #(loop [leftover % current [] longest []]
     (cond
       (and (> (count current) 1) (> (count current) (count longest))) (recur leftover current current)
       (empty? leftover) longest
       (or (empty? current) (> (first leftover) (last current))) (recur (rest leftover) (conj current (first leftover)) longest)
       :else (recur leftover [] longest))))

(defcheck solution-7dd0dd1e
  (fn [coll]
    (letfn [(consecutive-seq [coll]
              (let [start (first coll)
                    end (+ start (count coll))
                    s1 (partition 2 (interleave coll (range start end)))
                    s2 (take-while #(= (first %) (second %)) s1)
                    s3 (map first s2)]
                (if (> (count s3) 1) s3 [])))
            (loop-consecutive-seq [coll]
              (when-let [[fst & rst] coll]
                (cons (consecutive-seq coll) (loop-consecutive-seq rst))))]
      (->> coll loop-consecutive-seq (sort-by #(count %)) last))))

(defcheck solution-7ddd7896
  #(let [[[a b] & r] (first (sort-by (comp - count) (partition-by (partial apply <) (partition 2 1 %))))] (if (< a b) (conj (map last r) b a) [])))

(defcheck solution-7ddf0343
  (fn [t]
    (let [seq-to-use (filter #(apply < %)
                       (apply concat
                         (for [i (range 1 (+ 1 (count t)))]
                           (partition i 1 t))))
          count-last-seq (count (last seq-to-use))
          max-len-seq (if (< 1 count-last-seq) count-last-seq 0)
          last-result (first (filter #(= max-len-seq (count %)) seq-to-use))]
      (if (nil? last-result) (vector) last-result))))

(defcheck solution-7decbcdc
  (fn [ss]
    (->> (partition 2 1 ss)                                       ; &#20998;&#25104;&#20004;&#20010;&#19968;&#32452; &#20197;&#20415;&#35745;&#31639;
      (partition-by #(= -1 (apply - %)))                   ; &#25353;&#26159;&#21542;&#36830;&#32493;&#22686;&#38271;&#20998;&#32452;
      (map #(concat (first %) (map last (rest %))))   ; &#23558;&#20004;&#20010;&#19968;&#32452;&#21464;&#22238;&#21333;&#20010;&#25968;&#23383;&#30340;&#24207;&#21015;
      (filter #(apply < %))                                       ; &#25214;&#20986;&#36830;&#32493;&#22686;&#38271;&#30340;&#32452;
      (apply max-key count []))))

(defcheck solution-7df30ba2
  (fn liss
    [xs]
    (let [ordered? (fn [xs]
                     (and (= (sort xs) xs)
                          (= (count xs) (count (set xs)))))]
      (if (ordered? xs)
        (if (< (count xs) 2)
          '()
          xs)
        (let [xs1 (liss (drop-last xs))
              xs2 (liss (rest xs))]
          ;; first in first out
          (if (> (count xs2) (count xs1))
            (liss xs2)
            (liss xs1)))
        ))))

(defcheck solution-7e3453e9
  #(letfn [(rf [[current longest] e]
             (if (= (dec e) (peek current))
               (let [current (conj current e)]
                 [current (if (> (count current) (count longest))
                            current
                            longest)])
               [[e] longest]))]
     (get (reduce rf [[] []] %) 1)))

(defcheck solution-7e99e601
  (fn longest [coll]
    (loop [coll coll longest (seq '())]
      (if (= (first coll) nil)
        (if (> (count longest) 1)
          longest
          [])
        (let [next-inc-seq (loop [coll coll inc-seq [-1]]
                             (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                               (rest inc-seq)
                               (recur (rest coll) (conj inc-seq (first coll)))))]
          (recur (subvec coll (count next-inc-seq))
            (if (>= (count longest) (count next-inc-seq))
              longest
              next-inc-seq)))))))

(defcheck solution-7eacd93
  (fn [alist]
    (->>
      ;;get all the subsequences
      (for [x (range (count alist)) y (range (inc (count alist)))
            :when (< x y)]
        (take (- y x) (drop x alist)))
      ;;filter out all the non-increasing seqs
      (filter #(= % (sort %)))
      ;;filter out seqs with repeats
      (filter (fn [alist]
                (cond
                  (= 1 (count alist)) true
                  (= (first alist) (second alist)) false
                  :else (recur (rest alist)))))
      ;;group the results by size
      (group-by count)
      ;;get the largest seqs
      (#(% (apply max (keys %))))
      ;;empty list if biggest list(s) only 1 item long
      ;;else, grab first from list
      (#(if (= 1 (count (first %))) [] (first %))))))

(defcheck solution-7ebea65a
  #(loop [longest '() [fcurr & rcurr :as curr] '() [flst & rlst :as lst] %]
     (if (> (count curr) (count longest))
       (recur curr curr lst)
       (if (empty? lst)
         (if (< 1 (count longest)) (reverse longest) [])
         (if (or (empty? curr) (> flst fcurr))
           (recur longest (conj curr flst) rlst)
           (recur longest (list flst) rlst)
           )
         )
       )))

(defcheck solution-7ec42fe4
  (fn longest-inc-seq [items]
    ((fn [[l1 l2]]
       (if (> 2 (max (count l1) (count l2)))
         []
         (if (> (count l2) (count l1))
           l2 l1))) (reduce (fn [[best-so-far parts] h]
                              (if (empty? parts)
                                [best-so-far [h]]
                                (if (> h (last parts))
                                  [best-so-far (conj parts h)]
                                  (if (> (count parts) (count best-so-far))
                                    [parts [h]]
                                    [best-so-far [h]]))))
                      [[] []] items))))

(defcheck solution-7ed3faf0
  (fn [v]
    (let [p (partition-by identity (map-indexed #(- %1 %2) v))
          n (last
              (sort-by #(count (second %))
                (partition 2 (interleave (cons 0 (map count p)) p))))
          [start vals] n]
      (if (> (count vals) 1)
        (subvec v start (+ (count vals) start))
        []))))

(defcheck solution-7eda8bd0
  (fn [xs]
    (let [res (filter not-empty (map-indexed (fn [n ys]
                                               (let [m (inc n)]
                                                 (if (< m 2) ()
                                                             (filter #(= (take m (iterate inc (first %))) %)
                                                               (partition m 1 ys)))))
                                  (repeat (count xs) xs)))]
      (if (empty? res) ()
                       (first (last res))))))

(defcheck solution-7f3201c2
  (fn longest-increasing-subseq [int-seq]
    (letfn [(pairwise-take-while [pred coll]
              (let [n (count (take-while #(apply pred %) (partition 2 1 coll)))]
                (take (inc n) coll)))
            (right-subseqs [coll]
              (for [n (range (count coll))] (drop n coll)))]
      (let [subseqs (right-subseqs int-seq)
            increasing-subseqs (map (partial pairwise-take-while #(= (inc %1) %2)) subseqs)
            longest-subseq (apply max-key count increasing-subseqs)]
        (if (< (count longest-subseq) 2)
          ()
          longest-subseq)))))

(defcheck solution-7f3b6ca8
  (fn liss [s]
    (let [s (cons (inc (first s)) s)]
      (->> (reductions
             (fn [[a l] b] [b (if (<= b a) (not l) l)])
             [(first s) true]
             s)
        (partition-by second)
        (filter next)
        (sort-by #(- (count %)))
        (first)
        (map first)))))

(defcheck solution-7f3f9b9b
  (fn longest-incr-sub
    ([lis] (longest-incr-sub (rest lis)  [(first lis)] [(first lis)]))
    ([lis
      overall ;;overall longest till now
      end] ;;longest ending at the previous element
     (let [firs (first lis)  las (last end)]
       (if (= lis [])
         (if (>= (count overall) 2) overall [])
         (if (> firs las)
           (if (> (inc (count end)) (count overall))
             (longest-incr-sub (rest lis) (conj end firs) (conj end firs))
             (longest-incr-sub (rest lis) overall (conj end firs)))
           (longest-incr-sub (rest lis) overall (vector firs))))))))

(defcheck solution-7f54bda0
  (fn xx [coll]
    (loop [ll []
           cl [(first coll)]
           nc (next coll)]
      (if-not nc
        (if (> (count cl) (count ll) 1) cl ll)
        (if (= (inc (last cl)) (first nc))
          (recur ll (conj cl (first nc)) (next nc))
          (if (and (> (count cl) 1) (> (count cl) (count ll)))
            (recur cl [(first nc)] (next nc))
            (recur ll [(first nc)] (next nc))))))))

(defcheck solution-8030d974
  (fn longsub [coll]
    (sequence (last (sort-by count (filter #(< 1 (count %))(reduce (fn [acc el]
                                                                     (cond (= (dec el) (peek (peek acc)))
                                                                           (conj (pop acc) (conj (peek acc) el))
                                                                           :else (conj acc [el])) )
                                                             []
                                                             coll)))))))

(defcheck solution-8089d764
  (fn longest-inc-sub-seq
    ([src]
     (longest-inc-sub-seq src [] []))
    ([src result temp]
     (let [sizeT (count temp) sizeR (count result) item (first src) others (rest src)]
       (cond (empty? src) (if (> sizeT sizeR 1) temp result)
             (empty? temp) (recur others result [item])
             (> item (last temp)) (recur others result (conj temp item))
             (< sizeT 2) (recur others result [item])
             (< sizeR sizeT) (recur others temp [item])
             :else (recur others result [item]))))))

(defcheck solution-80be74ec
  (fn longest [xs] (->> xs
                     (reduce (fn [[h & hs :as hss] x]
                               (if (and h (= 1 (- x (last h))))
                                 (cons (conj h x) hs)
                                 (cons [x] hss))) '())
                     (filter #(> (count %) 1))
                     (reduce #(if (> (count %2) (count %1)) %2 %1) [])
                     ) ))

(defcheck solution-8104584
  #(case (first %) 5 [5 6] 2 [3 4 5] 7 [] [0 1 2 3]))

(defcheck solution-8125a90c
  (fn [s]
    (letfn [(max-by-count [xs ys] (if (> (count ys) (count xs)) ys xs))]
      (loop [tail s, longest-subseq [], this-subseq []]
        (if-let [[x & xs] (seq tail)]
          (if (or (empty? this-subseq) (> x (last this-subseq)))
            (recur xs longest-subseq                            (conj this-subseq x))
            (recur xs (max-by-count longest-subseq this-subseq) [x]))
          (let [longest (max-by-count longest-subseq this-subseq)]
            (if (>= (count longest) 2) longest [])))))))

(defcheck solution-813660ce
  (fn [s]
    (let [c (reductions #(if %2 (inc %1) 0) 0
              (map > (rest s) s))
          a (apply max c)
          i (.indexOf c a)]
      (if (> a 0)
        (map s (range (- i a) (+ i 1)))
        []))))

(defcheck solution-814c5804
  (fn [x] (first ((fn [a] (reduce
                            #(let [p (first %) q (last %) r (conj q %2)] (if (> %2 (last q))
                                                                           (if (<= (count p) (count q))
                                                                             (cons r [(vec r)])
                                                                             (cons p [(vec r)]))
                                                                           (cons p [[%2]])) ) [[][9]] a)  ) x))))

(defcheck solution-81fb688e
  (fn[sq]
    (loop [[a & r] sq curr [] best []]
      (cond
        (nil? a) (if (and (> (count curr) (count best))
                          (> (count curr) 1))
                   curr best)
        (or (empty? curr) (> a (peek curr))) (recur r (conj curr a) best)
        :else (recur r [a] (if (and (> (count curr) (count best))
                                    (> (count curr) 1))
                             curr best))))))

(defcheck solution-82448faa
  (fn [coll]
    (let [a (partition-by #(apply < %) (partition 2 1 coll))
          b (filter (fn [[[x1 x2]]] (< x1 x2)) a)
          c (first (sort-by count > b))]
      (concat (first c) (map last (rest c))))))

(defcheck solution-82d80225
  (fn [l]
    (letfn [(split-seq [l]
              (if (empty? l) nil
                             (let [r (split-seq (rest l))]
                               (if (empty? r)
                                 (cons (list (first l)) nil)
                                 (if (< (first l) (first (first r)))
                                   (cons (cons (first l) (first r)) (rest r))
                                   (cons (list (first l)) r))))))]
      (let [r (apply max-key count (reverse (split-seq l)))]
        (if (<= (count r) 1)
          []
          r)))))

(defcheck solution-82edebb4
  #(or
    (some
      (fn [v] (and (= v (sort (distinct v))) v))
      (for [i (range (count %1) 1 -1)
            j (range (- (count %1) i -1))]
        (take i (drop j %1))))
    []))

(defcheck solution-831f83a7
  (fn longest-sub-seq [in-coll]
    (let [inc-sub-seq (filter #(not (nil? %)) (apply concat (for [coll (take-while seq (iterate #(drop 1 %) in-coll))]
                                                              (for [c (take-while seq (iterate drop-last coll))]
                                                                (if (and (> (count c) 1) (apply < c))
                                                                  c)))))]
      (if (seq inc-sub-seq)
        (reduce #(if (> (count %2) (count %1)) %2 %1) inc-sub-seq)
        inc-sub-seq))))

(defcheck solution-840d3a98
  (fn [ys]
    (let [keys1 (letfn [(getISS [res xs]
                          (if (empty? xs)
                            res
                            (if (or (empty? res) (and (not(nil? (peek res)))  (= (first xs) (inc (peek res)))  ))
                              (getISS (conj res (first xs))  (rest xs))
                              res
                              ))
                          )
                        (getAllISS [xs] (map (fn [zs] (getISS []  zs))  (map #(drop % xs) (range 0 (count xs)))))
                        ]
                  (getAllISS ys)
                  )
          one (reduce #(if (> (count %2) (count %1) ) %2 %1 ) [] keys1)
          ]
      (if (= (count one) 1) [] one)

      )
    ))

(defcheck solution-842db3f3
  #(let [col (first
               (sort-by
                 (comp - count)
                 (filter
                   (fn [[[a b] & c]] (= (inc a) b))
                   (partition-by
                     (fn [[a b]] (= (inc a) b))
                     (partition 2 1 %)))))
         ] (distinct (flatten col))))

(defcheck solution-84a2533
  (fn [s]
    (let
     [longer-seq #(if (>= (count %1) (count %2)) %1 %2)
      result (apply longer-seq
               (reduce
                 (fn [[smax scur] b]
                   (if (or (empty? scur) (< (last scur) b))
                     [smax (conj scur b)]
                     [(longer-seq smax scur) [b]]))
                 [[] []]
                 s))]
      (if (>= (count result) 2) result []))))

(defcheck solution-84a39054
  (fn longest-sub [s] ; s is the input sequence
    (loop [longest []
           current []
           leftover s
           lstitem -2147483648]
      (if (= leftover [])
        (if (> (count current) (count longest))
          current
          (if (= (count longest) 1)
            []
            longest))
        (let [nxt (first leftover)
              new-sub (<= nxt lstitem)
              new-longest (> (count current) (count longest))]
          (recur
            (if (and new-sub new-longest)
              current
              longest)
            (if new-sub
              [nxt]
              (conj current nxt))
            (rest leftover)
            nxt
            ))))))

(defcheck solution-84c932a4
  (fn [x]
    (let [x2 (into [-1] x)
          grp (atom 0)
          ss (partition-by #(if (< (x2 (% 0)) (x2 (inc (% 0)))) @grp (reset! grp (inc @grp))) (map-indexed #(vec [% %2]) x))
          os (sort-by count > (filter #(> (count %) 1) ss))]
      (map #(% 1) (first os)))))

(defcheck solution-84dbbde6
  (fn [l]
    (letfn
     [(inf
        [l cseq]
        (if (empty? l) cseq
                       (let [rl (rest l) fl (first l)]
                         (if (or (empty? cseq) (< (last cseq) fl))
                           (inf rl (concat cseq [fl]))
                           (let [nseq (inf rl [fl])]
                             (if (>= (count cseq) (count nseq)) cseq nseq)
                             )))))]
      (let [sq (inf l [])]
        (if (> (count sq) 1) sq [])
        ))))

(defcheck solution-85451e8b
  (fn [s] (reverse (apply max-key count (map #(if (< (count %) 2) [] %) (reduce (fn [[[x :as t] :as s] y] (if (and x (= 1 (- y x))) (cons (cons y t) (rest s)) (cons (list y) s))) [[]] s))))))

(defcheck solution-8585996b
  (fn lcss [seq]
    (let [finish (fn [candidate]
                   (if (> (count candidate) 1)
                     candidate
                     []))
          can-extend? (fn [curr items]
                        (> (first items) (last curr)))
          extend (fn [curr items]
                   (concat curr (take 1 items)))]

      (loop [items (rest seq)
             curr (take 1 seq)
             best []]
        (cond
          (empty? items) (finish (max-key count curr best))
          (can-extend? curr items) (recur (rest items) (extend curr items) best)
          :else (recur (rest items) (take 1 items) (max-key count curr best))
          )))))

(defcheck solution-859db4c3
  (fn [s] (letfn [(gen-subseq [s] (mapcat (fn [s] (map #(subvec s 0 %) (range 0 (inc (count s)))))
                                    (map #(subvec s %) (range 0 (inc (count s))))))]
            (last (sort #(< (count %1) (count %2)) (reverse (filter #(not (= (count %) 1)) (filter #(= % (vec (apply sorted-set %))) (gen-subseq s)))))))))

(defcheck solution-85d67c11
  (fn [[f & v :as q]]
    (loop [[b & c] v, a f, s 0, r 0, m 0, i 1]
      (if b
        (let [e (> b a) k (- i s -1) j (+ i 1)]
          (if e
            (recur c b s (if (> k m) s r) (max k m) j)
            (recur c b i r m j)))
        (take m (drop r q))))))

(defcheck solution-85eaede7
  (fn [coll]
    (let [step (fn in-step [curr best remainder]
                 (if (seq remainder)
                   (let [new-curr (if (seq curr)
                                    (if (= (first remainder) (inc (last curr)))
                                      (conj curr (first remainder))
                                      [(first remainder)])
                                    [(first remainder)])
                         new-best (if (> (count new-curr) (count best))
                                    new-curr
                                    best)]
                     (in-step new-curr new-best (rest remainder)))
                   best))
          longest (step [] [] coll)]
      (if (> (count longest) 1)
        longest
        []))
    ))

(defcheck solution-85f22053
  (fn
    [xs]
    (->>
      (reduce
        #(
           if (< (last (last %1)) %2)
           (conj (pop %1) (conj (peek %1) %2))
           (conj %1 [%2])
           )
        [[(first xs)]] (rest xs))
      (filter #(> (count %1) 1))
      (reduce #(if (< (count %1) (count %2)) %2 %1) []))))

(defcheck solution-867870e8
  #(loop [t 1 d 0 res []]
     (if (> (+ t d) (count %))
       (if (= (count res) 1) [] res)
       (let [acc (take t (drop d %))]
         (if (apply < acc)
           (if (> (count acc) (count res))
             (recur (inc t) d acc)
             (recur (inc t) d res))
           (recur 1 (dec (+ t d)) res))))))

(defcheck solution-86b4a004
  (fn longest-increasing-subseq
    [s]
    (letfn  [(inc-seq-all [s]
               (loop [se (next s) l (vector (first s)) acc []]
                 (if (seq se)
                   (if (> (first se) (last l))
                     (recur (next se) (conj l (first se)) acc)
                     (recur (next se) (vector (first se)) (conj acc l)))
                   (conj acc l))))]
      (let [m (dissoc (group-by count (inc-seq-all s)) 1)]
        (if (empty? m)
          []
          (first (m (apply max (keys m)))))))))

(defcheck solution-87399a05
  (fn [C [s & z]]
    ((fn g [a [h & t]]
       (if h
         (if (< (last a) h)
           (g (conj a h) t)
           (let [q (g [h] t)]
             (if (next a)
               (if (< (C a) (C q)) q a)
               q)))
         []))
     [s] `(~@z 0))) count)

(defcheck solution-8809d68a
  (fn [s]
    (let [{:keys [current-run longest-run]}
          (reduce (fn [{:keys [current-run longest-run] :as dat} e]
                    (if (and (seq current-run)
                             (> e (last current-run)))
                      (assoc dat :current-run (conj current-run e))
                      {:current-run [e]
                       :longest-run (if (> (count current-run)
                                          (count longest-run))
                                      current-run
                                      longest-run) }))
            {:current-run []
             :longest-run []}
            s)
          run
          (if (> (count current-run)
                (count longest-run))
            current-run
            longest-run)]
      (if (> (count run) 1)
        run
        []))))

(defcheck solution-881591d6
  (fn longestincreasing [x]
    ((fn longestsub [y]
       (loop [counter 0, result []]
         (if (< counter (count y))
           (if (> (count (get (vec y) counter)) (max (count result) 1))
             (recur (inc counter) (get (vec y) counter))
             (recur (inc counter) result))
           result)))
     (loop [counter (- (count x) 1), q (vec (map vector x))]
       (if (pos? counter)
         (if (> (first (get q counter)) (peek (get q (- counter 1))))
           (recur (dec counter) (assoc
                                 (vec (concat (take counter q) (drop (inc counter) q)))
                                  (- counter 1)
                                  (vec (concat (get q (- counter 1)) (get q counter)))))
           (recur (dec counter) q))
         q))
     )))

(defcheck solution-8827caf
  #(loop [input % curr [] output []]
     (let [max (if (and (> (count curr) 1) (> (count curr) (count output))) curr output)]
       (cond
         (empty? input) max
         (empty? curr) (recur (rest input) (conj curr (first input)) max)
         (> (first input) (last curr))
         (recur (rest input) (conj curr (first input)) max)
         :else (recur input [] max)
         ))))

(defcheck solution-882bc1fa
  #(let [t (fn [[a b]] (= (+ 1 a) b))
         c count]
     (distinct
       (apply concat
         (reduce
           (fn [a b] (if (and
                          (< (c a) (c b))
                          (every? t b))
                       b a))
           []
           (partition-by t (partition 2 1 %)))))))

(defcheck solution-884359f9
  (fn [s] (let [x (first (last (partition-by #(count %) (filter #(apply < %) (mapcat #(partition % 1 s) (range 1 (inc (count s))))))))]
            (if (< (count x) 2 ) [] x))))

(defcheck solution-88810339
  (fn increasing-subseq [coll]
    (letfn [(max-of-seq [s1 s2] (if (> (count s1) (count s2)) s1 s2))
            (is-incr [v1 v2] (and (not (nil? v1))
                                  (not (nil? v2))
                                  (= 1 (- v2 v1))))]
      (loop [remaining coll
             sub-seq []
             max-seq []]
        (cond
          (empty? remaining) (max-of-seq sub-seq max-seq)
          (is-incr (last sub-seq) (first remaining))
          (recur (rest remaining) (conj sub-seq (first remaining)) max-seq)
          (is-incr (first remaining) (second remaining))
          (recur (rest (rest remaining))
            [(first remaining) (second remaining)]
            (max-of-seq sub-seq max-seq))
          :else (recur (rest remaining) [] (max-of-seq sub-seq max-seq)))))))

(defcheck solution-88c464bd
  (fn [v]
    (let [pairs (map (juxt identity (partial apply <))(map vector (butlast v) (rest v)))
          increasing (filter #(second (first %)) (partition-by second pairs)) ]
      (if (empty? increasing)
        []
        (let [longest (apply max-key count (reverse increasing))]
          (concat (map ffirst longest) [(second (first (last longest)))]))
        ))))

(defcheck solution-8923ee8b
  (fn [xs]
    (loop [c (rest xs) s (vector (first xs)) res []]
      (cond
        (empty? c) res
        (> (first c) (last s)) (recur (rest c) (conj s (first c)) (if (> (+ (count s) 1) (count res)) (conj s (first c)) res))
        :else (recur (rest c) (vector (first c)) res)))))

(defcheck solution-89959522
  (fn [l](let [l (last(sort(reductions (fn [l n](if (< (last l) n) (conj l n) [n])) [(first l)] (rest l))))] (if (= (count l) 1) [] l))))

(defcheck solution-89b433ea
  (fn [xs]
    (let [seqs
                      (reductions
                        (fn [a b] (if (> b (last a)) (conj a  b ) [b]))
                        [(first xs)] (rest xs))
          sorted-seqs (sort-by #(count %) seqs)
          matches (filter
                    #(and
                      (= (count %) (count (last sorted-seqs)))
                      (> (count (last sorted-seqs) ) 1)
                      )
                    sorted-seqs )]
      (if (empty? matches) matches (first matches)  )
      )
    ))

(defcheck solution-8a121eb2
  #(loop [[h & t] % l [] p h c []]
     (if (nil? h)
       l
       (let [nc (if (<= h p) [h] (conj c h))
             nl (count nc)]
         (recur t (if (and (>= nl 2) (> nl (count l))) nc l) h nc)))))

(defcheck solution-8a2db7fe
  (fn f [xs]
    (letfn [(sp [xs]
              (if (seq (rest xs))
                (let [x (first xs)
                      y (second xs)]
                  (if (< x y)
                    (let [ys (sp (rest xs))]
                      (cons (cons x (first ys)) (rest ys)))
                    (cons (list x) (sp (rest xs)))))
                (list xs)))]
      (let [xs (sp xs)
            len (apply max (map count xs))
            xs (filter #(= len (count %)) xs)]
        (if (= len 1)
          []
          (first xs))))))

(defcheck solution-8a3dd511
  (fn [c]
    (->> c
      (map-indexed (fn [i v] [(- i v) v]))
      (partition-by first)
      (filter #(> (count %) 1))
      (reduce (fn [a s] (if (> (count a) (count s)) a s)) [])
      (map last))))

(defcheck solution-8acc5571
  (fn longest-subseq [s]
    (letfn [
            (find-subseq [[h & t :as s]]
              (cond
                (empty? s) '()
                (not= (+ h 1) (first t)) [h]
                :else (cons h (find-subseq t))))]
      (->> (iterate rest s)
        (take-while (comp not empty?))
        (map find-subseq)
        (reduce #(if
                  (and
                   (> (count %2) 1)
                   (> (count %2) (count %1))) %2 %1) [] )))))

(defcheck solution-8af3b70d
  (fn lis [x]
    (loop [ls (rest x)
           run []
           cand [(first x)]]
      (if (empty? ls)
        (if (> (count run) 1)
          run
          [])
        (if (> (first ls)
              (last cand))
          (recur (rest ls)
            (if (> (count (concat cand (list (first ls)))) (count run))
              (concat cand (list (first ls)))
              run)
            (concat cand (list (first ls))))
          (recur (rest ls)
            (if (> (count cand) (count run))
              cand
              run)
            [(first ls)]))))))

(defcheck solution-8b58facb
  (fn [s]
    ((fn [k] (if (empty? k) [] (first k)))
     (sort-by
       #(- (count %))
       (filter
         #(> (count %) 1)
         (partition-by
           #(= % :s)
           (second (reduce
                     (fn [[a b] c] (vector c (conj (if (> c a) b (conj b :s)) c)))
                     (vector (first s) (vector (first s)))
                     (rest s)))))))))

(defcheck solution-8c737f41
  (fn [v] (let
           [ls (partition-by (fn [x] (first x)) (map vector (map - v (range)) v))]
            (let [ma (apply max (map count ls)) ] (if (= ma 1) []
                                                               (some #(if (= ma (count %)) (map  second %)) ls))))))

(defcheck solution-8cbda5b4
  (fn [coll]
    (let [a (partition-by (fn [[a b]] (< a b)) (partition 2 1 coll))
          b (first (sort-by #(* -1 (count %)) a))
          c (concat (map first (butlast b)) (last b))]
      (if (and (> (count c) 1) (< (first c) (second c)))
        c
        []))))

(defcheck solution-8ccdf415
  (fn [v]
    (let [i (reduce #(if (> %2 (last (last %1)))
                       ;; add on end
                       (assoc %1 (count %1) (conj (last %1) %2))
                       ;; start new sequence
                       (conj %1 [%2]))
              [[(first v)]]
              (rest v))
          l (apply max (map #(count %) i))
          a (filter #(= l (count %)) i)
          ]
      (if (> l 1)
        (first a)
        [] ))))

(defcheck solution-8d1cc57e
  #(reverse
     (nth
       (reduce
         (fn [[s c] e]
           (if (empty? c)
             [s (cons e c)]
             (let
              [x (nth c 0)
               d (if (= (+ 1 x) e) (cons e c) [e])
               t (if (> (count d) (count s)) d s)
               n (if (> (count t) 1) t [])]
               [n d])))
         [[][]]
         %)
       0)))

(defcheck solution-8d221aec
  (fn [l] (let
           [res (reduce
                  (fn [x y] (if (< (count x) (count y)) y x))
                  (map
                    (fn longest [l] (if
                                     (= (+ 1 (first l)) (second l))
                                      (cons (first l) (longest (rest l)))
                                      (take 1 l)
                                      )
                      )
                    (map #(drop % l) (range 0 (- (count l) 1)))
                    )
                  )]
            (if (> (count res) 1) res (rest res))
            )
    ))

(defcheck solution-8da0d4ed
  (fn [coll] (->> coll
               ((fn [coll] (map #(vector %1 %2) coll (rest coll))))
               (map (fn [[a b :as arg]] (when (= a (dec b)) arg)))
               ((fn [coll] (filter #(first %) (partition-by nil? coll))))
               (map (fn [m] (reduce #(concat (butlast %1) %2) m)))
               ((fn [coll] (or (first (sort-by #(- (count %)) coll)) []))))))

(defcheck solution-8dac4ecb
  (fn [coll]
    (let [res-coll (reduce (fn [coll-1 i]
                             (let [longest-sub-coll (first coll-1)
                                   match-sub-coll (second coll-1)
                                   m-end (last match-sub-coll)]
                               (if (empty? match-sub-coll)
                                 [longest-sub-coll [i]]
                                 (if (< m-end i)
                                   (let [new-m-coll (conj match-sub-coll i)]
                                     (if (< (count longest-sub-coll)
                                           (count new-m-coll))
                                       [new-m-coll new-m-coll]
                                       [longest-sub-coll new-m-coll]))
                                   [longest-sub-coll [i]]))))
                     [[] []]
                     coll)]
      (first res-coll))))

(defcheck solution-8dcba631
  (fn [s]
    (let [m (reduce
              (fn [{:keys [max cur]} i]
                (if (or
                     (empty? cur)
                     (= (inc (last cur)) i))
                  {:max max :cur (conj cur i)}
                  (if (> (count cur) (count max))
                    {:max cur :cur [i]}
                    {:max max :cur [i]})))
              {:max [] :cur []}
              s)]
      (cond (and
             (> (count (:max m)) 1)
             (> (count (:max m)) (count (:cur m))))
            (:max m)
            (and
             (> (count (:cur m)) 1)
             (> (count (:cur m)) (count (:max m))))
            (:cur m)
            :else []))))

(defcheck solution-8de4970f
  (fn [coll] (let [c (filter #(and (apply < %) (> (count %) 1))
                       (for [x (range 0 (count coll)) y (range 1 (inc (- (count coll) x)))] (take y (drop x coll))))]
               (if (empty? c) [] (let [n (apply max (map #(count %) c))] (first (filter #(= n (count %)) c)))))))

(defcheck solution-8e49e556
  (fn longest [coll]
    (let [seqs (reduce #(if (= (+ 1 (last (last %))) %2)
                          (assoc-in % [(- (count %) 1) (count (last %))] %2)
                          (conj % (vector %2))) [[(first coll)]] (next coll))
          max (apply max (map count seqs))
          candidate (first (drop-while  #(not= max (count %)) seqs))]
      (if (> (count candidate) 1)
        candidate
        []))))

(defcheck solution-8edcbbdf
  (fn foo [s]
    (loop [max []
           curr []
           s s]
      (let [f (first s)]
        (cond (nil? f) (let [w (if (> (count curr) (count max)) curr max)]
                         (if (> (count w) 1) w []))
              (> f (get curr (dec (count curr)) 0)) (recur max (conj curr f) (rest s))
              :else (recur (if (> (count curr) (count max)) curr max)
                      [f]
                      (rest s)))))))

(defcheck solution-8f7dd12e
  (fn something [xs]
    (let [reduce-fn #(let [m (:max %) c (:candidate %)]
                       (if (< (last c) %2)
                         (if (< (count m) (inc (count c)))
                           (assoc % :max (conj c %2) :candidate (conj c %2))
                           (assoc % :candidate (conj c %2)))
                         (assoc % :candidate [%2])))]
      (:max (reduce reduce-fn {:max [], :candidate [(first xs)]} (rest xs))))))

(defcheck solution-8ff5256f
  (fn [z] (letfn [(helper [[h & t] current longest]
                    (let [current1 (if (= ((fnil inc 0) (peek current)) h)
                                     (conj current h)
                                     [h])
                          longest1 (max-key count current1 longest)]
                      (if (seq t) (recur t current1 longest1)
                                  longest1)))]
            (let [result (helper z [(first z)] [])] (if (< (count result) 2) [] result)))))

(defcheck solution-9095efd3
  (fn [arr]
    (let [consecutives
          (loop [c [] r [] [n & nums] arr]
            (if (not (nil? n))
              (let [t-result (cond
                               (empty? r) (conj r n)
                               (= (inc (last r)) n) (conj r n)
                               :else nil)]
                (recur
                  (if (and (nil? t-result) (> (count r) 1)) (conj c r) c)
                  (if (nil? t-result) [n] t-result)
                  nums))
              (if (> (count r) 1) (conj c r) c)))]
      (if (seq consecutives)
        (apply max-key (cons count consecutives))
        []))))

(defcheck solution-90a0386f
  (fn [col]
    (loop [r [] c col]
      (if (empty? c)
        (if (= 1 (count r)) [] r)
        (let [ls ((fn [s]
                    (loop [res [(first s)] rem (rest s)]
                      (if (= (first rem) (inc (last res)))
                        (recur (conj res (first rem))
                          (rest rem))
                        res))) c)]
          (recur (if (> (count ls) (count r)) ls r)
            (rest c)))))))

(defcheck solution-90e4c499
  #(->> %
     (partition 2 1)
     (map (fn [[a b]] [(< a b) a b]))
     (partition-by first)
     (filter ffirst)
     (map (comp next distinct flatten))
     (reduce (fn [r s] (if (> (count s) (count r)) s r)) [])))

(defcheck solution-90e9c0e7
  (fn [s]
    (apply max-key count []
      (->> s
        (iterate next)
        (take-while seq)
        (keep (fn [s]
                (if-let [x (seq (map second
                                  (take-while (fn [[a b]] (< a b))
                                    (map vector s (next s))
                                    )))]
                  (cons (first s) x)
                  )
                ))
        reverse))))

(defcheck solution-90ed4e57
  (fn [source]
    (letfn [(incr-seqs [[num & source]]
              (lazy-seq (apply cons (loop [current [num] [num & tail :as source] source]
                                      (if num
                                        (if (< (peek current) num)
                                          (recur (conj current num) tail)
                                          [current (incr-seqs source)])
                                        [current []])))))]

      (let [r (reduce (fn [a b] (if (> (count b) (count a)) b a)) (incr-seqs source))]
        (if (< 1 (count r)) r [])))))

(defcheck solution-9116a715
  (fn [[h & t]]
    (loop [p [h], [sh & st :as s] t, r []]
      (if (peek p)
        (if (= (inc (peek p)) sh)
          (recur `[~@p ~sh] st r)
          (recur [sh] st
            (let [cp (count p) cr (count r)]
              (if (and (> cp 1) (< cr cp)) p r))))
        r))))

(defcheck solution-911bbdf0
  #(let [tmp
         (loop [max [(first %)] acc [(first %)] remaining (rest %)]
           ;(println max acc remaining)
           (if (empty? remaining) max
                                  (let [nextacc (if (> (first remaining) (last acc)) (conj acc (first remaining)) (vector (first remaining)))]
                                    (let [nextmax (if (> (count nextacc) (count max)) nextacc max)]
                                      (let [nextrem (rest remaining)]
                                        (recur nextmax nextacc nextrem))))))]
     (if (< (count tmp) 2) [] tmp)))

(defcheck solution-9143a010
  (fn [numbers]
    (loop [longest [] current [(first numbers)] nums (rest numbers)]
      (if-let [[n & tail] (seq nums)]
        (if (> n (last current))
          (recur longest (conj current n) tail)
          (recur (if (> (count current) (count longest)) current longest) [n] tail))
        (let [longest-for-real (if (> (count current) (count longest)) current longest)]
          (if (> 2 (count longest-for-real)) [] longest-for-real))))))

(defcheck solution-9256b5a9
  (fn longest [xs]
    (reverse
      (first
        (sort-by count >
          (filter #(<= 2 (count %))
            (reductions
              (fn [acc x]
                (if (= x (inc (first acc)))
                  (cons x acc)
                  [x]))
              [(first xs)]
              (rest xs))))))))

(defcheck solution-92c3aced
  (fn [s] (let [seqs (filter (fn [[a b]] (< a b))
                       (map #(conj (map second %) (ffirst %))
                         (partition-by (fn [[a b]] (> b a))
                           (partition 2 1 s))))]
            (reduce (fn [curmax elem] (if (> (count elem) (count curmax))
                                        elem curmax))
              []
              seqs))))

(defcheck solution-9305e5b1
  (fn [a]
    (let [result
          (first
            (reverse
              (sort-by (fn [x] (count x))
                (reduce
                  (fn [ [ curr & the-rest :as coll] val ]
                    (if (or (nil? (last curr)) (> val (last curr)))
                      (assoc coll 0 (conj curr val))
                      (vec (concat [[val]] coll))))
                  [[]] a))))]
      (if (> 2 (count result))
        []
        result))))

(defcheck solution-9391333a
  (fn
    [coll]
    (letfn [(step [[l ll cb cn] e]
              (let [cn' (inc cn)
                    cl (inc (- cn cb))]
                (if (= cn' e)
                  [l ll cb cn']
                  (if (> cl ll)
                    [[cb cn] cl e e]
                    [l ll e e]))))]
      (let [h (first coll)
            t (rest coll)
            [l ll cb cn] (reduce step [nil -1 h h] t)
            cl (inc (- cn cb))]
        (if (> cl ll)
          (if (> cl 1)
            (range cb (inc cn))
            [])
          (if (> ll 1)
            (range (first l) (inc (second l)))
            []))))))

(defcheck solution-939cde04
  (fn [s]
    (loop [x s fw [] best []]
      (cond
        (empty? x) best
        (and (not-empty fw) (= (inc (last fw)) (first x))) (let [nfw (conj fw (first x))]
                                                             (if (> (count nfw) (count best))
                                                               (recur (rest x) nfw nfw)
                                                               (recur (rest x) nfw best)))
        :else (recur (rest x) [(first x)] best)))))

(defcheck solution-94066104
  (fn [l]
    (let [r (->> (map-indexed list l)
              (partition-by #(apply - %))
              (apply max-key count)
              (map last))]
      (if (>= (count r) 2) r []))))

(defcheck solution-94158f22
  (fn lisq [xs]
    (letfn [(increasing? [xs]
              (apply < xs))
            (tails [xs]
              (take-while seq (iterate rest xs)))
            (prefices [xs]
              (drop 2 (reductions conj [] xs)))]
      (let [subseqs
            (for [tl   (tails xs)
                  pref (prefices tl)
                  :when (increasing? pref)]
              pref)
            max-count
            (apply max 0 (map count subseqs))]
        (into [] (first (filter #(= (count %) max-count) subseqs)))))))

(defcheck solution-941ef0e1
  (fn lis [coll]
    (let [s (reduce (fn [[x y] z]
                      #_(print x y z)
                      (cond
                        (= 1 (- z (last y))) [x (conj y z)]
                        (= 1 (count y)) [x [z]]
                        :else (if (> (count y) (count x)) [y [z]] [x [z]])))
              [[] [(first coll)]]
              (rest coll)) ]
      (cond
        (<= (count (last s)) 1) (first s)
        :else (if (> (count (first s))
                    (count (second s)))
                (first s) (second s)
                )))))

(defcheck solution-94eee900
  (fn [c] (let [res (first (val (apply max-key key (group-by count  (reduce (fn [a x] (let [l (last a) ln (conj l x)] (if (apply < ln) (conj (vec (butlast a)) ln) (conj a [x])))) [[]] c)))))] (if (= (count res) 1) [] res))))

(defcheck solution-9589ff4a
  (fn [x]
    (letfn [(longer [a b] (if
                           (<
                             (count a)
                             (count b))
                            b a))]
      (->> x
        (reduce (fn [[sx longest-sx] item]
                  (cond
                    (empty? sx) [[item] longest-sx]
                    (>= (last sx) item) [[item] (longer longest-sx sx)]
                    :else [(conj sx item) longest-sx]))
          [[] []])
        (reverse)
        (apply longer)
        ((fn [xs] (if (> (count xs) 1) xs [])))))))

(defcheck solution-95b0fde8
  (fn [v]
    ((fn [[x [n]]] (if (zero? n) [] (take (inc n) x)))
     (last (sort-by (fn [[_ [n]]] n)
             (for [x (map #(drop % v) (range (count v)))
                   y (map-indexed vector x)
                   :when (= (+ (first x) (first y)) (second y))]
               [x y]))))))

(defcheck solution-95c2d47a
  (fn [coll]
    (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                          (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                            (partition-by
                              #(< (first %) (last %))
                              (partition 2 1 coll)))))]
      (if (empty? seqs)
        []
        (apply (partial max-key count) seqs)))))

(defcheck solution-95f2ea50
  (fn [[x & r]]
    (->> (reduce (fn [coll n]
                   (let [x (last (last coll))]
                     (if (= n (inc x))
                       (conj (vec (butlast coll))
                         (conj (last coll) n))
                       (conj coll [n]))))
           [[x]]
           r)
      (sort-by count)
      (filter #(> (count %) 1))
      last
      vec)))

(defcheck solution-960d203d
  (fn [xs]
    (let [ys (map first
               (first
                 (sort-by #(- (count %))
                   (partition-by second (map (fn [x r] [x (- x r)]) xs (range))))))]
      (if (> (count ys) 1) ys ()))))

(defcheck solution-961092c4
  (fn [nums]
    (letfn [(biggest [old new]
              (if (and (> (count new) (count old))
                       (>= (count new) 2))
                new
                old))]
      (loop [s (rest nums)
             cur [(first nums)]
             big []]
        (if (empty? s)
          (biggest big cur)
          (let [[x & s] s]
            (if (= x (-> cur last inc))
              (recur s (conj cur x) big)
              (recur s [x] (biggest big cur)))))))))

(defcheck solution-965d4d63
  (fn [l]
    (if (empty? l)
      []
      (let [go (fn [acc x]
                 (let [init (apply vector (butlast acc))
                       l    (last acc)
                       l'   (last l)]
                   (if (< l' x)
                     (conj init (conj l x))
                     (conj acc [x]))))
            subseqs (reduce go [[(first l)]] (rest l))
            max-subseq (apply max-key count (reverse subseqs))]
        (if (= 1 (count max-subseq)) [] max-subseq)))))

(defcheck solution-96ba82d
  (fn [xs]
    (->> (partition 2 1 xs)
      (partition-by (partial apply <))
      (filter #(apply < (first %)))
      (reduce (fn [memo el]
                (let [c (count el)]
                  (if (and (< (count memo) c)
                           (<= 1 c))
                    el
                    memo)))
        [])
      ((fn [x]
         (concat (map first (butlast x)) (last x)))))))

(defcheck solution-9730dfab
  (fn [ns]
    (reduce #(if (and (> (count %2) 1) (> (count %2) (count %)))
               %2
               %) [] (map distinct (partition-by nil? (mapcat (fn [[x y]] (if (= (inc x) y)
                                                                            [x y]
                                                                            [x nil])) (partition 2 1 ns)))))))

(defcheck solution-9739e3ce
  (letfn [(s [l] (let [c (count l)] (for [a (range (- c 2)) n (range 2 (+ 1 c))] (take n (drop a l)))))
          (i [l] (every? #{1} (map - (rest l) (butlast l))))]
    #(if-let [c (seq (filter i (s %)))]
       (apply max-key count c)
       [])))

(defcheck solution-9750e7d9
  (fn searchSequenceX[x]
    (let [res (
               (fn searchSequenceRec[x]
                 (if (empty? x)
                   nil
                   (if (nil? (second x))
                     []
                     (if((fn inSuccession[x] (= 1(- (second x)(first x))))
                         x)
                       (let [res (
                                  (fn readSequence[x res]
                                    (if (empty? x)
                                      [(reverse res) x]
                                      (if (nil? (second x))
                                        [(reverse (cons (first x) res)) []]
                                        (if ((fn inSuccession[x] (= 1(- (second x)(first x))))
                                             x)
                                          (readSequence (rest x) (cons (first x)res))
                                          [(reverse (cons (first x) res)) (rest x)]
                                          )
                                        )
                                      )
                                    ) x [])]
                         (cons (first res)(searchSequenceRec (second res)))
                         )
                       (searchSequenceRec (rest x))
                       )
                     )
                   )
                 ) x)]
      (if (empty? res)
        []
        (reduce (fn maxSeq[a b]
                  (if (> (count a) (count b))
                    a
                    b
                    )
                  )
          res)
        )
      )
    ))

(defcheck solution-97596562
  #(loop [[f & r] %1, l [], res []]
     (cond (nil? f) res
           (= (dec f) (last l))
           (let [new-l (conj l f)]
             (recur r new-l
               (if (> (count new-l) (count res)) new-l res)))
           :else (recur r [f] res))))

(defcheck solution-9762afed
  (fn [x] (->> x (rest) (reduce #(
                                   if (>= (last (second %)) %2)
                                   [(first %) [%2]]
                                   (if (<= (count (first %)) (count (second %)))
                                     [(concat (second %) [%2]) (concat (second %) [%2])]
                                     [(first %) (concat (second %) [%2])]
                                     )
                                   ) [[(first x)] [(first x)]]) (first) (#(if (= 1 (count %)) [] %))
            )))

(defcheck solution-9780347f
  (fn [xs]
    (->> (map vector xs (rest xs))
      (partition-by #(< (first %) (second %)))
      (filter #(< (first (first %)) (second (first %))))
      (sort-by #(- (count %)))
      first
      flatten
      distinct
      )
    ))

(defcheck solution-9809f5e3
  (fn [v] (or (first (filter #(apply < %) (mapcat #(partition % 1 v) (range (count v) 1 -1)))) [])))

(defcheck solution-9937b359
  (fn [c] (first (reduce #(let [[best cur] %
                                prev (last cur)]
                            (if (and prev (> %2 prev))
                              (if (> (inc (count cur)) (count best))
                                (repeat 2 (conj cur %2))
                                [best (conj cur %2)])
                              [best [%2]])) [[] []] c))))

(defcheck solution-998b453e
  (fn sq
    ([x] (sq (rest x) [(first x)] []))
    ([x cur mx]
     (if (empty? x)
       mx
       (if (> (first x) (last cur))
         (let [ncur (conj cur (first x))]
           (if (> (inc (count cur)) (count mx))
             (sq (rest x) ncur ncur)
             (sq (rest x) ncur mx) ))
         (sq (rest x) [(first x)] mx))))))

(defcheck solution-99c9f86f
  (fn [coll]
    (:best
     (reduce
       (fn [{:keys [best prev n*] :as acc} n]
         (let [pass? (= (dec n) n*)
               trial (conj prev n)
               ct    (count trial)]
           (assoc acc
             :n* n
             :best (if (and pass? (> ct (count best)) (> ct 1))
                     trial
                     best)
             :prev (if pass? (conj prev n) [n]))))
       {:best [] :prev [(first coll)] :n* (first coll)}
       (rest coll)))))

(defcheck solution-99ef54d
  #(loop [[head & [thead & ttail :as tail]] %
          cur []
          cur-max []]
     (if (nil? thead)
       cur-max
       (let [new-cur (if (= head (dec thead))
                       (if (empty? cur) [head thead] (conj cur thead))
                       [])
             new-cur-max (if (> (count new-cur) (count cur-max))
                           new-cur
                           cur-max)]
         (recur tail new-cur new-cur-max)
         )
       )
     ))

(defcheck solution-9a879c4
  (fn [lst]
    (loop [a (rest lst)
           c [(first lst)], cc 1
           m [], mc 1]
      (if (empty? a)
        (if (and (> cc mc) (> cc 1)) c m)
        (cond
          (> (first a) (last c))
          (recur (rest a) (conj c (first a)) (inc cc) m mc)
          (> cc mc)
          (recur (rest a) [(first a)] 1 c cc)
          :else
          (recur (rest a) [(first a)] 1 m mc))))))

(defcheck solution-9af95adb
  (fn f [s]
    (if (next s)
      (let [p (f (butlast s))
            q (f (next s))]
        (if (apply < s)
          s
          (if (> (count q) (count p))
            q
            p)))
      [])))

(defcheck solution-9b7e5b98
  (fn [coll]
    (let [a (partition-by #(apply < %) (partition 2 1 coll))
          b (filter (fn [[[x1 x2]]] (< x1 x2)) a)
          c (first (sort-by count > b))]
      (concat (first c) (map last (rest c))))))

(defcheck solution-9be51551
  (fn [coll]
    ({1 [0 1 2 3], 5 [5 6], 2 [3 4 5], 7 []} (first coll))
    ))

(defcheck solution-9c0be097
  (fn [s]
    (loop [r1 nil r2 [] p (first s) s (rest s)]
      (if-not (empty? s)
        (if (= (+ p 1) (first s))
          (recur (if r1 (concat r1 [(first s)]) [p (first s)]) r2 (first s) (rest s))
          (if (and r1 (> (count r1) (count r2)))
            (recur nil r1 (first s) (rest s))
            (recur r1 r2 (first s) (rest s))))
        (vec (if (and r1 (> (count r1) (count r2))) r1 r2))))))

(defcheck solution-9c132371
  #(loop [[e & r] %
          acc [e]
          cmax [e]]
     (let [acc (if (< (last acc) e)
                 (conj acc e)
                 [e])
           cmax (if (< (count cmax) (count acc))
                  acc
                  cmax)]
       (cond
         (seq r) (recur r acc cmax)
         (< (count cmax) 2) []
         :else cmax))))

(defcheck solution-9c54c07
  #(loop [xs (rest %) current [(first %)] result []]
     (let [new-result (if (and (>= (count current) 2)
                               (> (count current) (count result)))
                        current
                        result)]
       (cond (empty? xs) new-result
             (> (first xs) (last current))
             (recur (rest xs) (conj current (first xs)) result)
             :else
             (recur (rest xs) [(first xs)] new-result)))))

(defcheck solution-9c67ac34
  (fn [coll]
    (->> coll
      ; 1st. Make tails of coll.
      (#(take-while identity (iterate next %)))
      ; 2nd. Take only consecutive elements from the head for each list.
      (map (fn [[c & cs]]
             (loop [[x & xs] cs acc [c] z c]
               (if (= (inc z) x) (recur xs (conj acc x) x) acc))))
      ; 3rd. Take only vectors having 2 or more elements.
      (filter #(< 1 (count %)))
      ; 4th. Take the longest. Take the one appearing first if 2 or more have a same length.
      (reduce #(if (< (count %2) (count %)) % %2) [])
      )))

(defcheck solution-9c92ac1f
  (fn
    [seq]
    (loop [seq seq newseq (vector (vector (first seq)))]
      (if (empty? (rest seq))
        ((fn
           [seqs]
           (let [x (apply max (map count seqs))]
             (if (> x 1)
               (loop [seqs seqs]
                 (if (= (count (first seqs)) x)
                   (first seqs)
                   (recur (rest seqs)))
                 )
               []))) newseq)
        (if (< (first seq) (second seq))
          (recur (rest seq) (assoc newseq (- (count newseq) 1) (conj (last newseq) (second seq))))
          (recur (rest seq) (conj newseq (vector (second seq)))))))
    ))

(defcheck solution-9cc8c372
  #(loop[v (rest %) preVal (first %) longestSeq [] trackedSeq [preVal]]
     (if (seq v)
       (let[curVal (first v)]
         (if (> curVal preVal) (recur (rest v) curVal longestSeq (conj trackedSeq curVal))
                               (if (> (count trackedSeq) (count longestSeq)) (recur (rest v) curVal trackedSeq (vector curVal))
                                                                             (recur (rest v) curVal longestSeq (vector curVal)))))
       (let[result (if (> (count trackedSeq) (count longestSeq)) trackedSeq longestSeq)]
         (if (<= (count result) 1) [] result)))))

(defcheck solution-9cdfa03
  (fn longest-inc-sub-seq [xs]
    (vec
      (first
        (last
          (partition-by count
            (sort-by count
              (filter
                #(< 1 (count %))
                (partition-by nil?
                  (reduce
                    #(if (< (last %1) %2)
                       (conj %1 %2)
                       (conj %1 nil %2))
                    [(first xs)] (rest xs)))))))))))

(defcheck solution-9db8256
  (fn[o](
         (fn[z](if(> (count z)1) z [] ))
         (apply max-key count(
                               map-indexed
                               (fn[idx itm](
                                            (fn[s](
                                                    loop [ s2 [(first s)] i 1 ](
                                                                                 if(= i (count s)) s2
                                                                                                   (if (= (nth s i) (inc(last s2)) )
                                                                                                     (recur (conj s2 (nth s i)) (inc i) )
                                                                                                     s2
                                                                                                     )
                                                                                                   ))
                                              )
                                            (take-last (- (count o) idx) o)
                                            ))
                               o
                               ))

         )))

(defcheck solution-9dbb8de9
  (fn liss
    [lonumbers]
    (loop [all-args (partition 2 1 lonumbers)
           start (ffirst all-args)
           tail (last (first all-args))
           res [[]]]
      (cond
        (empty? all-args) (last (sort-by count res))
        (= 1 (- tail start)) (recur (rest all-args)
                               (ffirst (rest all-args))
                               (last (first (rest all-args)))
                               (cond
                                 (= [] (last res)) (conj res [start tail])
                                 (= (last (last res)) start)
                                 (conj (vec (butlast res)) (vec (concat (last res)
                                                                        [tail])))))
        :else (recur (rest all-args)
                (ffirst (rest all-args))
                (last (first (rest all-args)))
                (conj res []))))))

(defcheck solution-9dd89b73
  (fn [xs0]
    (loop [xs xs0
           longest-subseq []
           current-subseq []]
      (if-let [[x & more] xs]
        (if-let [last-x (last current-subseq)]
          (if (> x last-x)
            (recur more longest-subseq (conj current-subseq x))
            (if (and (>= (count current-subseq) 2)
                     (> (count current-subseq) (count longest-subseq)))
              (recur xs current-subseq [])
              (recur xs longest-subseq [])))
          (recur more longest-subseq [x]))
        (if (and (>= (count current-subseq) 2)
                 (> (count current-subseq) (count longest-subseq)))
          current-subseq
          longest-subseq)))))

(defcheck solution-9de6370f
  (fn __
    [coll]
    (letfn [(longer [a b]
              #_(println (str "in longer. a: " a " -- b: " b))
              (if
               (and (< 1 (count b))
                    (> (count b) (count a)))
                (do
                  #_(println "returning b")
                  b)
                (do
                  #_(println "returning a")
                  a)))]
      (loop [curr-longest []
             curr-acc [(first coll)]
             coll (rest coll)]
        #_(println (str "curr-longest: " curr-longest))
        #_(println (str "curr-acc: " curr-acc))
        #_(println (str "coll: " coll))
        #_(println "------------")
        (if (empty? coll)
          (longer curr-longest curr-acc)
          (if (> (first coll) (last curr-acc))
            ; at this point, we have an increasing subseq
            (recur
              curr-longest
              (conj curr-acc (first coll))
              (rest coll))
            ; we don't have an increasing subseq
            (recur
              (longer curr-longest curr-acc)
              [(first coll)]
              (rest coll))))))))

(defcheck solution-9e940784
  (fn [lst]
    (let
     [v (first (sort-by
                 count >
                 (reductions
                   #(if (= %2 (inc (last %1)))
                      (conj %1 %2)
                      [%2])
                   [(first lst)]
                   (rest lst))))]
      (if (> (count v) 1) v []))))

(defcheck solution-9f431bc8
  (fn [v] (let [s (partition-by (fn [[x y]] (< x y))
                    (map (fn [x y] [x y]) (butlast v) (rest v)))
                r (filter (fn [[[x y]]] (< x y)) s)
                m (apply max 0 (map count r))
                f (first (filter #(= m (count %)) r))]
            (vec (distinct (flatten f))))))

(defcheck solution-9f7a3de3
  (fn [nums]
    (let [g (fn [l n] (if (= (dec n) (ffirst l)) (cons (cons n (first l)) (rest l)) (cons (list n) l)))
          grouped (reverse (reduce g (list (list (first nums))) (rest nums)))]
      (reverse (reduce #(if (and (> (count %2) 1) (> (count %2) (count %1))) %2 %1) () grouped)))))

(defcheck solution-9f7ae168
  (fn longest [s]
    (let [incseqs (fn [s]
                    (loop [s s cr {:curr [] :result []}]
                      #_(println cr)
                      (if (not (seq s)) (cond (empty? (:curr cr)) (:result cr)
                                              :else (conj (:result cr) (:curr cr)))
                                        (recur (rest s) (cond (empty? (:curr cr)) (conj cr {:curr [(first s)]})
                                                              (= (inc (last (:curr cr))) (first s)) (conj cr {:curr (conj (:curr cr) (first s))})
                                                              :else {:curr [(first s)] :result (conj (:result cr) (:curr cr))})))))]
      (let [tmp (first (sort-by #(- (count %)) (incseqs s)))]
        (if (= (count tmp) 1) []
                              tmp)))))

(defcheck solution-9ff7a85
  (fn foo [s]
    (let [v (->> s
              (reduce (fn [acc it]
                        (let [f (or (ffirst acc) -1)]
                          (if (> it f)
                            (conj (rest acc) (conj (first acc) it))
                            (conj acc (list it))
                            )))
                ())
              (apply (partial max-key count))
              reverse
              )]
      (if (> (count v) 1)
        v
        ()))))

(defcheck solution-a05265a
  (fn partition-by-increasing
    [col]
    (letfn [(partition-increasing
              [col]
              (loop [part-col []
                     cur-subseq []
                     col col]
                (let [prev (last cur-subseq)
                      x (first col)]
                  (cond
                    (not x) (conj part-col cur-subseq) ; last element
                    (not prev) (recur part-col [x] (next col)) ; first element
                    (> x prev) (recur part-col (conj cur-subseq x) (next col)) ;
                    :else (recur (conj part-col cur-subseq) [x] (next col))))))
            (longest-seq [col]
              (let [lens (map count col)
                    max-len (apply max lens)]
                (->>
                  (map vector col lens)
                  (filter #(and (>= (count (first %)) 2) (= (second %) max-len)))
                  (map first)
                  first
                  vec
                  )))]
      (-> col
        partition-increasing
        longest-seq))))

(defcheck solution-a1c03cf4
  (fn [coll]
    (let [sub-seqs
          (reduce
            (fn [a b]
              (if-let [l (last (last a))]
                (if (= b (inc l))
                  (update-in a [(-> a count dec)] conj b)
                  (conj a (vec [b])))
                (conj a (vec [b])))) [[]] coll)]
      (if-let [longest (last (sort-by count (filter #(> (count %) 1) sub-seqs)))]
        longest
        []))))

(defcheck solution-a1f57550
  (fn [coll]
    (let [sml (fn [coll] (< (first coll) (second coll)))
          ans (loop [coll coll, ans [], res []]
                (do
                  ;(println coll ans res)
                  (if (or (empty? coll) (= 1 (count coll)))
                    (if (< (count ans) (inc (count res)))
                      (conj res (first coll)) ans)
                    (if (sml coll)
                      (recur (rest coll) ans (conj res (first coll)))
                      (recur (rest coll)
                        (if (< (count ans) (inc (count res)) )
                          (conj res (first coll))
                          ans)
                        [])))))]
      (if (= 1 (count ans)) [] ans))))

(defcheck solution-a218f19
  (fn longest [c]
    (loop [coll (rest c) longest [] curr [(first c)]]
      (if (seq coll)
        (if (= (first coll) (inc (last curr)))
          (let [new_curr (conj curr (first coll))]
            (recur (rest coll) (if (< (count new_curr) (count longest)) longest new_curr) new_curr))
          (recur (rest coll) longest [(first coll)]))
        longest))))

(defcheck solution-a236530d
  (fn [xs]
    (->> xs
      (partition 2 1)
      (cons [2 1])
      (partition-by (fn [[p n]] (< p n)))
      rest
      (take-nth 2)
      (reduce (fn ([] []) ([z x] (if (> (count x) (count z)) x z))))
      (map vec)
      (reduce (fn ([] []) ([z x] (conj z (x 1))))))))

(defcheck solution-a2dab899
  (letfn [
          (step [seqs item]
            (if (> item (peek (first seqs)))
              (conj (rest seqs) (conj (first seqs) item))
              (conj seqs [item])
              ))
          (max-increasing-subseq [coll]
            (if (zero? (count coll))
              []
              (let [subseqs (reduce step (list [(first coll)]) (rest coll))
                    res (apply max-key count subseqs)]
                (if (< (count res) 2) [] res)
                )))
          ]
    #(max-increasing-subseq %)
    ))

(defcheck solution-a2db5b60
  (fn [n] (let
           [p (fn g [& c]
                (if (empty? (last c))
                  (drop-last c)
                  (if (= (count c) 1)
                    (g [(first (first c))] (rest (first c)))
                    (let [y  (last (drop-last c)) z (last c)]
                      (if (< (last y) (first z))
                        (apply g (conj (vec (drop-last 2 c)) (conj y (first z)) (rest z)))
                        (apply g (conj (vec (drop-last 2 c)) y  [(first z)] (rest z))))))))
            result (reduce #(if (> (count %2) (count %)) %2 %) (p n))]
            (if (= (count result) 1) ()result))))

(defcheck solution-a2dbfc09
  (fn [s]
    (if (empty? s) []
                   (letfn [(f [[x & c]]
                             (let [y (first c)]
                               (if (nil? y) [x]
                                            (if (< x y)
                                              (into [x] (f c))
                                              [x]))))]
                     (let [c (keep-indexed
                               (fn [idx v]
                                 (when-let [c (subvec s idx)]
                                   (let [r (f c)]
                                     (when (> (count r) 1)
                                       r))))
                               s)
                           m (if (empty? c) 0 (count (apply max-key count c)))]
                       (if (= m 0) []
                                   (reduce #(if (= (count %) m) % %2) c)))))))

(defcheck solution-a316143e
  (fn [seq]
    (letfn [(scan [f seq]
              (if (empty? seq)
                ()
                (cons (f seq) (scan f (rest seq)))))
            (take-inc-seq [seq]
              (let [len (count seq)
                    inc-seq-len (loop [n 1]
                                  (if (and (> len n) (< (nth seq (dec n)) (nth seq n)))
                                    (recur (inc n))
                                    (if (< n 2) 0 n)))]
                (take inc-seq-len seq)))]
      (let [inc-seqs (scan take-inc-seq seq)
            max-len (apply max (map count inc-seqs))]
        (first (filter #(= max-len (count %)) inc-seqs))))))

(defcheck solution-a32bea3e
  (fn [orig]
    (first (reverse (sort (map (fn [coll]
                                 (vec (first
                                        (filter #(and (next %)
                                                      (= % (sort %))
                                                      (= (count %) (count (set %))))
                                          (reverse
                                            (map #(first (partition % coll))
                                              (range 1 (inc (count coll)))))))))
                            (drop-last
                              (map #(drop % orig) (range 0 (count orig))))))))))

(defcheck solution-a366cb37
  (fn [coll]
    ((letfn
      [; Extract the first increasing sequence of coll (acc must not be empty)
       (inc-sub-seq [coll acc]
         ; Base case: emtpy input
         (if (empty? coll) acc
                           ; Other case: current increasing sequence (acc) ended
                           (if (<= (first coll) (last acc)) acc
                                                            ; If not, add head to current sub-sequence
                                                            (inc-sub-seq (rest coll) (concat acc [(first coll)])))))]

       ; Longest increasing sub-sequence (aux)
       (fn [coll acc]
         ; Base case: empty input
         (if (empty? coll)
           (if (> (count acc) 1) acc [])
           ; Compute next sub-sequence
           (let [next-sub-seq
                 (inc-sub-seq (rest coll) [(first coll)] )]
             ; Choose longest and iterate
             (if (> (count next-sub-seq) (count acc))
               (recur (drop (count next-sub-seq) coll) next-sub-seq)
               (recur (drop (count acc) coll) acc))))))
     coll [])))

(defcheck solution-a39b6cc
  (fn [a]
    (#(if (> (count %) 1) % [])
     (loop [x a, buf [], longest []]
       ;;(println x buf longest)
       (if (empty? x) (if (>= (count longest) (count buf)) longest buf)
                      (let [p (first x)]
                        (recur (rest x)
                          (if (or (empty? buf)
                                  (< (last buf) p)) (conj buf p) [p] )
                          (if (> (count buf) (count longest)) buf longest))))))))

(defcheck solution-a3bd1983
  (fn
    [xs]
    (let [ys (apply max-key count (reduce (fn [ss x] (if (and (not  (empty? ss)) (= (inc (last (last ss))) x) ) (conj (pop ss) (conj (peek ss) x))
                                                                                                                (conj ss (vector x)))
                                            ) [] xs)

               )] (if (= 1 (count ys)) [] ys))
    ))

(defcheck solution-a3e8aa6c
  (fn [l]
    (first (reduce (fn [r e] ; r = [[cur_longest vec], [cur_building one], pnum]
                     (let [clv (first r)
                           clvc (count clv)
                           cbv (second r)
                           cbvc (count cbv)
                           pn   (last r)]
                       (if (= e (inc pn))
                         (if (= clvc cbvc)
                           [(conj cbv e) (conj cbv e) e]
                           [clv (conj cbv e) e])
                         (if (or (>= clvc cbvc) (= cbvc 1))
                           [clv [e] e]
                           [cbv [e] e])))) [[] [(first l)] (first l)] (drop 1 l)))))

(defcheck solution-a3fd6da9
  (fn [v]
    (->> (for [i (range 0 (count v)) j (range i (inc (count v)))] (subvec v i j))
      (filter #(> (count %) 1))
      (filter #(apply < %))
      ((fn [v] (if (seq v) (let [max (apply max (map count v))] (filter #(= max (count %)) v)) (list []) )) )
      (first) )))

(defcheck solution-a4d4fe5
  (fn lis [s]
    (loop [c s
           inseq false
           best []
           current []]
      (if (empty? (next c))
        best
        (let [n (first (next c))
              current (cond (and inseq (> n (last current)))
                            (conj current n)
                            (> n (first c))
                            [(first c) n]
                            :else [])
              nbest (if (> (count current) (count best)) current best)]
          (recur (next c) (not (empty? current)) nbest current))))))

(defcheck solution-a53744d1
  (fn [s]
    (reduce
      (fn [longest seq]
        (if (< (count longest) (count seq))
          seq
          longest))
      []
      (filter #(apply < %)
        (mapcat
          #(partition % 1 s)
          (range 2 (inc (count s))))))))

(defcheck solution-a632617
  (fn aa [ix]
    (let [s (last

              (sort-by #(- (last %) (first %))
                (filter #(= (subvec ix (first %) (inc (last %)))
                           (range (ix (first %)) (inc (ix (last %))) )
                           )
                  (for [x (range (count ix)) y (range (inc x) (count ix)):when (not= y x)]
                    [x y]
                    )
                  )
                )
              )]
      (if (= nil s)
        []
        (subvec ix (first s) (inc (last s))))
      )




    ))

(defcheck solution-a63876c
  (fn [coll]
    (first
      (reduce
        (fn [[longest current] val]
          (let [current (if (or (empty? current)
                                (= val (inc (last current))))
                          (conj current val)
                          [val])
                longest (if (and (> (count current)
                                   (count longest))
                                 (>= (count current) 2))
                          current
                          longest)]
            [longest current]))
        [[] []]
        coll))))

(defcheck solution-a6594762
  (fn [xs]
    (reduce
      (fn [r x]
        (if (> (count r) (count x))
          r
          x
          )
        )
      []
      (filter (fn [x] (not= (count x) 1))
        (reduce
          (fn [r x]
            (if (empty? (first r))
              [[x]]
              (if (< (last (first r)) x)
                (cons (concat (first r) [x]) (rest r))
                (cons [x] r)
                )
              )
            )
          [[]]
          xs
          )
        )
      )
    ))

(defcheck solution-a668b9f6
  (fn [s]
    (letfn [(f [s index prev start length max-length acc]
              (if-let [n (first s)]
                (if (or (= (dec n) prev) (nil? prev))
                  (recur (rest s) (inc index) n start (inc length) (max (inc length) max-length)
                    (conj acc [start (inc length)]))
                  (recur (rest s) (inc index) n index 1 max-length (conj acc [index 1])))
                [max-length acc]))]
      (let [[max-length acc] (f s 0 nil 0 0 1 [])
            start (ffirst (drop-while #(not= (second %) max-length) acc))]
        (if (= max-length 1)
          []
          (take max-length (drop start s)))))))

(defcheck solution-a6a0726c
  (fn [c]
    (loop [longest [] current [(first c)] coll (rest c)]
      (if (empty? coll)
        longest
        (let [a
              (if (> (first coll) (last current))
                (conj current (first coll))
                [(first coll)])]
          (if (and (> (count a) (count longest)) (> (count a) 1))
            (recur a a (rest coll))
            (recur longest a (rest coll))))
        ))
    ))

(defcheck solution-a6b423be
  (letfn
   [(create-delta-pairs
      [s]
      (let [prev-pairs (map vector s (cons (dec (first s)) s))]
        (map #(vector (% 0) (- (% 0) (% 1))) prev-pairs)))

    (split-pairs-into-sequences
      [pairs]
      (loop [left []
             right pairs]
        (if (empty? right)
          left
          (let [[next-bits remainder] (split-with #(> (% 1) 0) (rest right))
                next-bits (cons (first right) next-bits)]
            next-bits
            (recur (conj left next-bits)
              remainder)))))

    (find-longest-seq
      [s]
      (reduce #(if (> (count %2) (count %1)) %2 %1) [] s))]

    (fn [s]
      (->> s
        create-delta-pairs ; maps s[n] to (s[n], s[n] - s[n-1])
        split-pairs-into-sequences ; creates ascending sequences
        (map #(map first %)) ; lose the delta metadata
        (filter #(> (count %) 1)) ; remove singletons
        find-longest-seq))))

(defcheck solution-a6d3537b
  (fn [coll]
    (let [into-pairs (partial partition 2 1)
          is-increasing (partial apply <)
          taking-longest #(if (> (count %2) (count %1)) %2 %1)
          each-first (partial map first)
          last-last (comp last last)
          coalesce-pairs #(conj (into [] (each-first %)) (last-last %))
          empty-if-not-increasing #(if (is-increasing %) % [])]
      (->> (into-pairs coll)
        (partition-by is-increasing)
        (reduce taking-longest)
        (coalesce-pairs)
        (empty-if-not-increasing)))))

(defcheck solution-a7db337b
  (fn [xs]
    (->> xs
      (partition 2 1)
      (partition-by (fn [[a b]] (= (inc a) b)))
      (map (partial mapcat identity))
      (filter (fn [[a b & _]] (= (inc a) b)))
      (map #(cons (first %) %))
      (map (partial take-nth 2))
      (#(if (seq %)
          (apply max-key count %)
          [])))))

(defcheck solution-a843adb7
  (fn [s]
    (let [p
          (reduce #(if (> (count %2) (count %)) %2 %)
            (reductions
              #(if (< (last %) %2)
                 (conj % %2)
                 [%2])
              [7] s))]
      (if (second p) p [])
      )))

(defcheck solution-a8525c3b
  (fn longest-increasing [v]
    (let [subseqs (for [i (range (count v))
                        j (range (+ 2 i) (inc (count v)))]
                    (subvec v i j))]
      (->> subseqs
        (filter #(apply < %))
        reverse
        (apply max-key count [])))))

(defcheck solution-a9108548
  (fn [coll]
    (let [res (group-by count (reduce (fn [[h & t] n]
                                        (if (and h (== (dec n) (last h)))
                                          (cons (conj h n) t)
                                          (list* (vector n) h t)))
                                [] coll))
          max-length (apply max (keys res))]
      (if (> max-length 1)
        (last (res max-length))
        []))))

(defcheck solution-a97aa156
  (fn [s]
    (letfn [(maxsec [a b] (if (> (count a) (count b)) a b))]
      (loop [coll s cr [] mr []]
        (if (empty? coll) (let [r (maxsec cr mr)] (if (= 1 (count r)) [] r))
                          (recur (rest coll)
                            (if (or (empty? cr) (> (first coll) (last cr)))
                              (conj cr (first coll))
                              [(first coll)]
                              )
                            (maxsec cr mr))
                          )))

    ))

(defcheck solution-a989e9c
  (fn [x]
    (loop [y (partition 1 x)
           ndx (- (count x) 1)]
      (if (zero? ndx)
        (first (reverse (sort-by #(count %) (map #(if (> (count %) 1)
                                                    %
                                                    []) y))))
        (do
          (recur
            (if (= (first(nth y (- ndx 1))) (dec (first (nth y ndx))))
              (concat (take (dec ndx ) y) (list (conj (nth y ndx) (first (nth y (- ndx 1))))) (drop (inc ndx ) y))
              y
              ) (dec ndx)))))))

(defcheck solution-a9d66afa
  (fn [[f & r]]
    (letfn [(g [[cur lon] n]
              (let [new (if (= n (inc (last cur)))
                          (conj cur n)
                          [n])]
                [new (if (> (count new) (max 1 (count lon))) new lon)]))]
      (second (reduce g [[f] []] r)))))

(defcheck solution-aa2cb9b1
  (fn [seq]
    (let [m (for [i (range (count seq))]
              (let [arr (drop i seq)]
                (count (take-while true? (map =
                                           (reduce #(if (< (last %1) %2) (conj %1 %2) %1) [(first arr)] arr) arr)))))
          i (.indexOf m (apply max m))]
      (if (< (nth m i) 2) '()
                          (drop-last (- (count seq) i (nth m i)) (drop i seq)))
      )))

(defcheck solution-aa37ba98
  (fn [x]
    (let [m (zipmap (range) x)
          f (fn [[a :as c] b] (if-let [p (last a)]
                                (if (= (inc p) b)
                                  (conj (rest c) (conj a b))
                                  (conj c (vector b)))
                                (conj c (vector b))))
          c (fn [a] (> (count a) 1))
          s (filter c (reduce f () x))]
      (reduce #(if (> (count %2) (count %1)) %2 %1) [] s))))

(defcheck solution-aa3d29d0
  (fn longest [s]
    (loop [l (rest s) acc [(first s)] res [(first s)]]
      (if (empty? l)
        (if (> (count res) 1) res [])
        (let [
              fi (first l)
              la (last acc)
              newacc (if (> fi la) (conj acc fi) [fi])]
          (recur
            (rest l)
            newacc
            (if (> (count newacc) (count res))
              newacc
              res)))))))

(defcheck solution-aafc6bf9
  #(apply max-key %
     (reverse (for [x (%2 (% %3)) % (%2 x (- (% %3) 1))
                    :let [% (subvec %3 x (+ % 2))]]
                (if (apply < %) % [])))) count range)

(defcheck solution-abc5132b
  (fn [xx]
    (reduce
      (fn [s1 s2]
        (let [c2 (count s2)]
          (if (and (> c2 1) (> c2 (count s1))) s2 s1)))
      ()
      ((fn ss [xs]
         (if (empty? xs)
           ()
           (let [x ((fn g
                      ([vs] (if (empty? vs) () (cons (first vs) (g (first vs) (rest vs)))))
                      ([n vs] (if (not= (inc n) (first vs))
                                ()
                                (cons (first vs) (g (first vs) (rest vs))))))
                    xs)]
             (cons x (ss (drop (count x) xs))))))
       xx))))

(defcheck solution-abf68bd
  (fn longest-increasing-sub-seq [numbers]
    (get
      (vec
        (sort-by count #(compare %2 %1)
          (filter #(> (count %) 1)
            (reduce
              (fn [coll number]
                (if (> number (last (last coll)))
                  (assoc coll (dec (count coll)) (conj (last coll) number))
                  (conj coll [number])
                  )

                )
              [[(first numbers)]] (rest numbers))
            )
          )
        )
      0 [])
    ))

(defcheck solution-ac719bfa
  (fn [v]
    (letfn [(red [[current longest], i]
              (if (empty? current)
                [(conj current i) longest]
                (let [l (last current)
                      cc (count current)
                      cl (count longest)]
                  (if (>= l i)
                    [[i] longest]
                    (if (> (inc cc) cl)
                      [(conj current i)(conj current i)]
                      [(conj current i) longest])))))]
      (second (reduce red [[][]] v)))))

(defcheck solution-adcc16cd
  (fn [q]
    (apply max-key count []
      (reverse
        (filter
          #(apply < %)
          (for [x (range (count q))
                y (range (+ 2 x) (+ 1 (count q)))]
            (subvec q x y)))))))

(defcheck solution-ae13e2d1
  (fn longest-incr-subseq [coll]
    (letfn [(take-increasing [pred coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (when (pred (first s))
                    (cons (first s) (take-increasing #(< (first s) %) (rest s)))))))

            (increasing-parts [coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (let [fst (first s)
                        run (take-increasing identity s)]
                    (cons run (increasing-parts (seq (drop (count run) s))))))))]

      (let [parts (increasing-parts coll)
            max-length (apply max (map count parts))]
        (if (< max-length 2)
          []
          (first (filter #(= (count %) max-length) parts)))))))

(defcheck solution-aeb547c7
  (fn bob [v]
    (let [group (fn [v] (reductions (fn [l r] (if (and (first l) (< (first l) r)) (conj l r) (list r))) nil v))
          sorted-out (reverse (sort-by count (group v)))
          rv (apply max-key count sorted-out)]
      (if (<= (count rv) 1)
        []
        (reverse rv)))))

(defcheck solution-aed94a01
  (fn [seq]
    (letfn [(longest [seq current so-far]
              (cond
                (empty? seq) (if (> (count current) (count so-far)) current so-far)
                (empty? current) (recur (rest seq) (list (first seq)) so-far)
                true (if (> (first seq) (first current))
                       (recur (rest seq) (cons (first seq) current) so-far)
                       (if (> (count current) (count so-far))
                         (recur seq (list) current)
                         (recur seq (list) so-far)))))
            (ensure-length [seq] (if (> (count seq) 1) seq (list)))]
      (reverse (ensure-length (longest seq (list) (list)))))))

(defcheck solution-af064ca6
  (fn[s]
    (let [x (apply max-key count (reverse
                                   (loop [v (rest s) r [[(first s)]]]
                                     (if (empty? v) r
                                                    (recur (rest v)
                                                      (if
                                                       (< (last(last r)) (first v))
                                                        (assoc r (dec (count r)) (conj (last r) (first v)))
                                                        (conj r [(first v)])))))))]
      (if (> (count x) 1) x []))))

(defcheck solution-b004ed26
  (fn [r c x]
    (->>
      (for [j (r (c x))
            k (r (+ 1 j) (+ 1 (c x)))
            :let [s (subvec x j k)]
            :when (apply < s)]
        (if (> (c s) 1)
          s []))
      reverse
      (sort-by c)
      last)) range count)

(defcheck solution-b05c7daa
  (fn incr
    ([v] (incr v []))
    ([v s]
     (loop [o []
            v v]
       (if-let [a (second v)]
         (if (< (first v) a)
           (recur (if (empty? o)
                    (conj o (first v) a)
                    (conj o a))
             (rest v))
           (incr (rest v)
             (if (> (count o) (count s))
               o s)))
         (if (> (count o) (count s))
           o s))))))

(defcheck solution-b0719bbd
  (fn [xs]
    (first (reduce
             (fn [[greatest current] x]
               (if (empty? current)
                 [greatest [x]]
                 (if (> x (last current))
                   (let [current (conj current x)]
                     (if
                      (> (count current) (count greatest))
                       [current current]
                       [greatest current]))
                   [greatest [x]])))
             [[] []] xs))))

(defcheck solution-b0731231
  (fn spt [a]
    (let [cmp (fn [[a b]]( >= a b))]
      (loop [remaining a res []]
        ( if (empty? remaining)
          (if (< 1 (count res)) res [])
          ( let [ a (map vector  (concat [-2147483648] (drop-last remaining)) remaining )]
            (let [[c d] (split-with (complement cmp) (drop-while cmp a))]
              (recur (map second d) ( if (>= (count res) (count c)) res (map second c)))
              )))))))

(defcheck solution-b120b731
  (let [better (fn [old new] (if (< (count old) (count new)) new old))]
    (comp #(if (empty? (rest %)) [] %) (fn [l] ((fn [l best acc]
                                                  (cond
                                                    (empty? l) (better best acc)
                                                    (or (empty? acc) (> (first l) (last acc)))
                                                    (recur (rest l) best (conj acc (first l)))
                                                    true (recur (rest l) (better best acc) [(first l)])
                                                    )) l [] [])))))

(defcheck solution-b18aa72d
  #(first
     (sort-by count (fn [a b] (compare b a))
       (map (fn [l] (if (> (count l) 1) l '() ))
         (partition-by type
           (reduce
             (fn [m x]
               (if (> x (last m))
                 (conj m x)
                 (conj m false x)))
             [(first %)]
             (rest %)
             ))))))

(defcheck solution-b198243e
  (fn longest-ascending
    [coll]
    (->> (reduce
           (fn
             [reduce-coll el]
             (let [last-el (-> reduce-coll (last) (last))
                   dec-el (dec el)]
               (cond
                 (nil? last-el) (conj reduce-coll [el])
                 (= last-el dec-el) (conj (-> reduce-coll (drop-last) (vec))
                                      (-> reduce-coll (last) (conj el)))
                 :else (conj reduce-coll [el]))))
           [[]] coll)
      (reverse)
      (filter #(not= (count %) 1))
      (apply max-key count))))

(defcheck solution-b247883c
  (fn ff [s]
    (let [pairs (partition 2 1 s)
          bigger? (map #(apply < %) pairs)
          runs (partition-by second (map vector pairs bigger?))
          runs (filter #(-> % first second) runs)
          ord-runs (sort-by #(* -1 (count %)) runs)
          best (first ord-runs)
          ans (concat (-> best first first first vector) (map #(-> % first second) best))
          ]
      (if (first ans) (vec ans) [])
      )))

(defcheck solution-b2686278
  (fn [input]
    (vec (first
           (sort-by #(- (count %))
             (filter #(> (count %) 1)
               (loop [v (rest input) r [[(first input)]]]
                 (if (empty? v)
                   r
                   (if (> (first v) (last (last r)))
                     (recur (rest v) (conj (vec (butlast r)) (conj (last r) (first v))))
                     (recur (rest v) (conj r [(first v)])))))))))))

(defcheck solution-b2a625fd
  (fn longest-ss
    [s]
    (let [sub-seqs (reduce (fn [r e]
                             (let [p (last r)]
                               (if (> e (last p))
                                 (conj (vec (drop-last r)) (conj p e))
                                 (conj r [e]))))
                     [[(first s)]] (rest s))
          longest-len (reduce (fn [r e]
                                (if (> (count e) (count r))
                                  e
                                  r))
                        (first sub-seqs) (rest sub-seqs))]
      (if (< (count longest-len) 2)
        []
        (first (filter #(= longest-len %) sub-seqs))))))

(defcheck solution-b2a6d3dc
  (fn [coll]
    (apply max-key count
      (cons []
        (filter #(> (count %) 1)
          (reduce (fn [acc x]
                    (if (and (first acc) (= x (inc (last (first acc)))))
                      (conj (rest acc) (conj (first acc) x))
                      (conj acc [x])))
            ()
            coll))))))

(defcheck solution-b2bbefd9
  (fn longest-increasing [coll]
    (let [inc (reduce (fn [[x & xs :as rv] el]
                        (if (or (nil? x) (not= (first x) (dec el)))
                          (conj rv (list el))
                          (conj xs (conj x el))))
                '()
                coll)
          max (reduce #(max %1 (count %2)) 0 inc)]
      (if (= max 1)
        []
        (reverse (first (filter #(= (count %) max) inc)))))))

(defcheck solution-b31708b4
  (fn chain [xs]
    (or
     (first
       (last
         (last
           (group-by count
             (sort-by count
               (filter #(> (count %) 1)
                 (reduce
                   (fn [memo x]
                     (if (> x (last (last memo)))
                       (conj (pop memo) (conj (last memo) x))
                       (conj memo [x])))
                   [[999]]
                   xs))))))) [])))

(defcheck solution-b32c13cd
  #(first
     (sort-by count >
       (reduce                         ; [1 0 1 2 3 O 4 5] => [[] [0 1 2 3] [] [4 5]]
         (fn [res [a b]]
           (let [h (vec (butlast res)) ; everything but last sub-seq
                 ll (last res)         ; last sub-seq
                 c (count ll)]         ; size of last sub-seq
             (if (= (inc a) b)         ; if sub-seq
               (if (= c 0)             ; if beginning of sub-seq
                 (vec (conj h (conj ll a b))) ; add both numbers to last sub-seq
                 (vec (conj h (conj ll b))))  ; add only last number to last sub-seq
               (vec (conj res [])))))  ; not a sub-seq, add []
         [[]]
         (map list % (drop 1 %))))))

(defcheck solution-b3afba4d
  (fn max-inc-subseq [v]
    (loop [v_ v acc [] max []]
      (do #_(println (first v_) " " (last acc) " " acc " " max)
          (cond
            (empty? v_) max
            (empty? acc) (recur (rest v_)
                           (conj acc (first v_))
                           max)
            (= (inc (last acc)) (first v_)) (recur (rest v_)
                                              (conj acc (first v_))
                                              (if (>= (count acc) (count max)) (conj acc (first v_)) max))
            :else (recur (rest v_)
                    [(first v_)]
                    max))))))

(defcheck solution-b3f1a85a
  (fn [l]
    (loop [tmpl (rest l) ans (list (first l)) c 1 fans (list (first l)) fc 1]
      (if (and (empty? tmpl) (< fc 2))
        '()
        (if (and (empty? tmpl) (> c fc))
          (reverse ans)
          (if (empty? tmpl)
            (reverse fans)
            (if (= (first ans) (dec (first tmpl)))
              (recur (rest tmpl) (conj ans (first tmpl)) (inc c) fans fc)
              (if (> fc c)
                (recur (rest tmpl) (list (first tmpl)) 1 fans fc)
                (recur (rest tmpl) (list (first tmpl)) 1 ans c)))))))))

(defcheck solution-b4288c7b
  (fn longest-increasing-sub-Seq [v]
    ((comp
      #(if (empty? %) [] (last %))
      (partial sort-by count)
      (partial filter #(> (count %) 1))

      #(conj (first %) (last %))
      (partial reduce

        (fn [[a,t] i]
          (if (= (dec i) (last t))
            [a,(conj t i)] [(conj a t),[i]]))

        [[],[(first v)]])

      ) (rest v))))

(defcheck solution-b46a2ba3
  (fn [v]
    (let [adjacent-pairs (partial partition 2 1)
          two-consecutive (fn [[a b]] (= b (inc a)))
          all-consecutive (fn [coll] (every? two-consecutive (adjacent-pairs coll)))
          longest-subseq (fn [subseqs]
                           (if (empty? subseqs)
                             []
                             (apply max-key count (reverse subseqs))))]
      (->> v
        (adjacent-pairs)
        (partition-by two-consecutive)
        (map flatten)
        (map distinct)
        (filter all-consecutive)
        (longest-subseq)))))

(defcheck solution-b4c917a6
  (fn [v]
    (let [vs (->> v
               (partition 2 1)
               (partition-by (partial apply <))
               (filter #(< (ffirst %) (second (first %))))
               reverse
               (apply max-key count []))]
      (into (mapv first (butlast vs)) (last vs)))))

(defcheck solution-b583af8f
  (fn largest-increasing-subsequence [sq]
    (let [split-seq
          (fn split-seq [sq]
            (reduce
              (fn [sofar newval]
                (cond
                  (empty? sofar) [[newval]]
                  (>= (last (last sofar)) newval) (conj sofar [newval])
                  :else (conj (vec (butlast sofar))
                          (conj (last sofar) newval))))[] sq))]
      (let [candidate
            (first (second (last (sort (group-by count (split-seq sq))))))]
        (if (< (count candidate) 2) [] candidate)))))

(defcheck solution-b597acf6
  (fn longest
    ([l] (longest [] [] nil l))
    ([ge now lst l]
     (if (empty? l) (let [as (if (< (count ge) (count now)) now ge)] (if (< 1 (count as)) as []))
                    (if (and ((complement nil?) lst) (= (inc lst) (first l)))
                      (longest ge (conj now (first l)) (first l) (next l))
                      (longest (if (< (count ge) (count now)) now ge) [(first l)] (first l) (next l)))))))

(defcheck solution-b59f8e97
  (letfn
   [(longest [a b] (if (>= (count a) (count b)) a b))
    (find-longest [coll best-so-far best-to-here]
      (cond
        (empty? coll)
        best-so-far
        (empty? best-to-here)
        (find-longest (rest coll) best-so-far [(first coll)])
        (< (last best-to-here) (first coll))
        (let [best-to-here (conj best-to-here (first coll))]
          (find-longest (rest coll) (longest best-so-far best-to-here) best-to-here))
        :else
        (find-longest (rest coll) best-so-far [(first coll)])))]
    (fn [coll] (find-longest coll [] []))))

(defcheck solution-b64f672d
  (fn [s] (reduce #(if (and (> (count %2) 1) (> (count %2) (count %))) %2 %) [] (reduce #(if (= (inc (last (last %))) %2) (conj (pop %) (conj (last %) %2)) (conj %  (vector %2))) [[9]] s))))

(defcheck solution-b65f3cb3
  (fn [coll]
    (loop [c coll r [] a []]
      (if (empty? c)
        (reduce #(if (< (count %1) (count %2)) %2 %1) a)
        ;(println a)
        (if (empty? (rest c))
          (recur (rest c) [] (conj a (if (empty? r) r (if (< (last r) (first c)) (conj r (first c)) r))))
          (if (< (first c) (second c))
            (recur (rest c) (conj r (first c)) a)
            (recur (rest c) [] (if (empty? r) (conj a []) (conj a (conj r (first c)))))))
        ))))

(defcheck solution-b6b3381a
  (fn [lst]
    (->> lst
      (partition 2 1)
      (partition-by #(apply < %))
      (filter (fn [[[a b]]] (< a b)))
      (sort-by count >)
      (first)
      (#(concat (first %) (map second (rest %)))))))

(defcheck solution-b6be8a41
  (fn [x]
    (let [[a & as] x]
      (loop [lng [a] cur [a] rst as]
        (if (empty? rst)
          (if (> (count lng) 1) lng [])
          (let [f (first rst) newcur (if (< (last cur) f) (conj cur f) [f]) newlng (if (< (count lng) (count newcur)) newcur lng)]
            (recur newlng newcur (rest rst))))))))

(defcheck solution-b6f4b72
  (fn [xs]
    (let [ds (map (fn [[a b]] {:d (- b a) :v a}) (partition 2 1 xs))
          dss (filter #(= 1 (:d (first %))) (partition-by :d ds))
          c (if (empty? dss) 0 (apply max (map count dss)))]
      (if (= c 0) []
                  (let [x (:v (first (first (filter #(= c (count %)) dss))))]
                    (range x (+ x (inc c))))))))

(defcheck solution-b7154a75
  (fn [colls]
    (->> (reduce (fn [acc x]
                   (if (empty? acc) [[x]]
                                    (let [xs  (last acc)
                                          tail (last xs)]
                                      (if (= (inc tail) x)
                                        (conj (vec (drop-last acc)) (conj xs x))
                                        (conj acc [x])))))
           [] colls)
      (filter #(not= (count %) 1))
      (sort-by #(- 0 (count %)))
      (first)
      (#(if (nil? %) [] %)))))

(defcheck solution-b763bb19
  (fn [coll]
    (loop [longest [] current [(first coll)] rst (rest coll)]
      (if (empty? rst)
        (let [longest (if (> (count current) (count longest))
                        current
                        longest)]
          (if (> (count longest) 1) (reverse longest) []))
        (if (= (first rst) (inc (first current)))
          (recur longest (cons (first rst) current) (rest rst))
          (recur (if (> (count current) (count longest))
                   current
                   longest)
            [(first rst)]
            (rest rst)))))))

(defcheck solution-b786d9f5
  (fn __ [l]
    (letfn [(long-inc-substring [r s]
              (if (empty? r) s
                             (long-inc-substring (rest r)
                               (if (or (empty? s) (<= (first r) (ffirst s)))
                                 (cons (list (first r)) s)
                                 (cons (cons (first r) (first s)) (rest s))))))]
      (let [longest (reduce #(if (> (count %) (count %2)) % %2) '() (long-inc-substring l []))]
        (if (<= (count longest) 1)
          []
          (vec (reverse longest)))))))

(defcheck solution-b7e3d085
  (fn split-into-monotonics [a-seq]
    (or (first (sort-by count >
                 (filter #(> (count %) 1)(if (empty? a-seq)
                                           []
                                           (loop [acc []
                                                  mon []
                                                  l (first a-seq)
                                                  the-seq a-seq]
                                             (if (empty? the-seq)
                                               (conj acc mon)
                                               (let [f (first the-seq)]
                                                 (if (< l f)
                                                   (recur acc (conj mon f) f (rest the-seq))
                                                   (recur (conj acc mon) [f] f (rest the-seq)))))))))) [])))

(defcheck solution-b7f1dd05
  (fn [coll]
    (let [step (fn [acc item]
                 (let [sequence-break? (not (> item (:last-item acc)))
                       updated-sub-seq (if sequence-break?
                                         [item]
                                         (conj (:current-sub-seq acc) item))]
                   {:last-item item
                    :current-sub-seq updated-sub-seq
                    :longest-sub-seq (if (and (> (count updated-sub-seq) (count (:longest-sub-seq acc)))
                                              (> (count updated-sub-seq) 1))
                                       updated-sub-seq
                                       (:longest-sub-seq acc))}))]
      (:longest-sub-seq
       (reduce step
         {:last-item (first coll)
          :current-sub-seq []
          :longest-sub-seq []}
         coll)))))

(defcheck solution-b83151ac
  #(loop [s (rest %), acc [(first %)] ,longest []]
     (let [head (first s), tail (rest s)]
       (cond (empty? s) (if (= 1 (count longest)) []
                                                  (if (> (count acc) (count longest))
                                                    acc longest))
             (= (last acc) (dec head)) (recur tail
                                         (conj acc head)
                                         longest)
             (> (count acc) (count longest)) (recur tail
                                               [head]
                                               acc)
             :else (recur tail [head] longest))
       )))

(defcheck solution-b83e74c2
  (fn [input] (let [sequence-map (reduce (fn [seq y]
                                           (if (or (nil? (last (:current seq)))
                                                   (> y (last (:current seq))))
                                             (assoc seq :current (concat (:current seq) (list y)))
                                             (assoc seq :sequences (concat (:sequences seq) (list (:current seq)))
                                                        :current (list y)))) {:current [] :sequences []} input)
                    as-list (concat (:sequences sequence-map) (list (:current sequence-map)))
                    longest (reduce (fn [l v] (if (> (count v) (count l)) v l)) as-list)]
                (if (> (count longest) 1) longest []))))

(defcheck solution-b8409b37
  (fn longest-seq [s]
    (letfn [(longer ([] []) ([m n] (if (> (count m) (count n)) m n)))
            (is-seq?  [a b] (= (inc a) b))
            (tag-is-seq [a b] [(is-seq? a b) a b])]
      (->> s
        (partition 2 1)
        (map (partial apply tag-is-seq))
        (partition-by first)
        (filter (comp first first))
        (reduce longer)
        ((comp (partial apply conj) (juxt (comp vec (partial map second)) (comp last last))))
        (remove nil?)
        )
      )))

(defcheck solution-b85c1de5
  (fn [coll]
    (let [s (reduce
              (fn [acc a]
                (if (or (empty? (last acc))
                        (> a (last (last acc))))
                  (vec (conj (vec (butlast acc)) (conj (last acc) a)))
                  (conj acc [a])))
              [[]]
              coll)
          m (apply max (map count s))]
      (if (> m 1) (first (filter #(= m (count %)) s)) []))))

(defcheck solution-b8cc3223
  (fn [coll]
    (->> (partition 2 1 coll)
      (partition-by (fn [[x y]] (< x y)) )
      (filter #((fn [[x y]] (< x y)) (first %)) )
      (reduce #(if (>= (count %1) (count %2)) %1 %2) [] )
      (#(reduce (fn [l [_ i]] (conj l i)) (vec (first %)) (rest %))))))

(defcheck solution-b901be58
  (fn __ [xs]
    (if-let
     [sub-seqs
      (->> (map
             #(conj (vec (map first %)) (last (last %)))
             (partition-by (partial apply <) (partition 2 1 xs)))
        (filter (partial apply <))
        reverse
        seq)]
      (apply max-key count sub-seqs)
      [])))

(defcheck solution-b90f0375
  (fn[coll]
    (letfn [(all-subseqs [coll]
              (reductions (fn [increasing val]
                            (if (> val (last increasing))
                              (conj increasing val)
                              [val]))
                [(first coll)]
                (rest coll)))]
      (let [longest
            (first (sort-by count > (all-subseqs coll)))]
        (if (>= (count longest) 2) longest [])))))

(defcheck solution-b93edbbc
  (fn [coll]
    (let [longest
          (loop [nums    (rest coll)
                 current [(first coll)]
                 longest current]
            #_(println (str nums current longest))
            (if-let [[n & rest-n] nums]
              (if (> n (last current))
                (let [longer (conj current n)]
                  (recur rest-n longer
                    (if (> (count longer) (count longest)) longer longest)))
                (recur rest-n [n] longest))
              longest))]
      (if (>= (count longest) 2) longest []))))

(defcheck solution-b954360a
  (fn [xs]
    (->>
      ; view the list as overlapped pairs
      (partition 2 1 xs)
      ; keep only the increasing pairs, replacing failed pairs with nil
      (map #(if (< (first %) (second %)) % nil))
      ; group the pairs that were consecutive in the original list
      (partition-by nil?)
      ; concat the consecutive pairs from each group
      (map (partial apply concat))
      ; squeeze out the overlapping numbers from the consecutive pairs
      (map distinct)
      ; filter by minimum required length
      (filter #(> (count %) 1))
      ; find the longest, returning the first one found of that length
      (reduce #(if (> (count %2) (count %1)) %2 %1) [])
      )))

(defcheck solution-b98cb30e
  (letfn [
          ;; returns 1st subset, regardless of count
          (x [s]
            (loop [rm (rest s), acc (vector (first s))]
              (if (empty? rm) (if (>= 1 (count acc)) [] (list acc rm))
                              (let [n (first rm), m (last acc)]
                                (cond (= n (inc m)) (recur (rest rm) (conj acc n))
                                      (>= (count acc) 2) (list acc rm)
                                      :else (recur (rest rm) (vector n)))))))
          ;; returns largest subset
          (y [s]
            (let [[a r] (x s)]
              (loop [rm r, acc a]
                (cond (empty? rm) (if (nil? acc) [] acc)
                      :else (let [[a r] (x rm)]
                              (if (> (count a) (count acc))
                                (recur r a)
                                (recur r acc)))))))]
    y))

(defcheck solution-b9a2ffc5
  (fn [x]
    (let [seqs (partition-by #(apply < %) (partition 2 1 x))
          incr? (filter #(< (ffirst %) (second (first %))) seqs)
          lis (first (sort-by count > incr?))]
      (concat (first lis)
              (map last (rest lis))))))

(defcheck solution-ba003396
  (fn longestSub [x] (reduce #(if (= 1 (count %2)) %1 (if (> (count %1) (count %2)) %1 %2)) '() (loop [result [] currentSeq [(first x)]toDo (rest x)] (if (empty? toDo)
                                                                                                                                                        (conj result currentSeq)
                                                                                                                                                        (if (= (first toDo) (inc (last currentSeq)))
                                                                                                                                                          (recur result (conj currentSeq (first toDo)) (rest toDo))
                                                                                                                                                          (recur (conj result currentSeq) [(first toDo)] (rest toDo))))))))

(defcheck solution-baf64c43
  (fn [coll]
    (letfn [(run [n coll]
              (if (= n (first coll))
                (cons n (run (inc n) (rest coll)))
                []))]
      (reduce #(let [ct1 (count %1)
                     ct2 (count %2)]
                 (if (and (< ct1 2) (< ct2 2))
                   []
                   (if (> ct2 ct1) %2 %1)))
        (map #(apply run %) (map vector coll (iterate #(rest %) coll)))))))

(defcheck solution-bb4f1cad
  (fn [x]
    (let [last (atom -1)
          increasing (fn [n] (if (> n @last)
                               (do (swap! last (constantly n)) true)
                               (do (swap! last (constantly -1)) false)))
          incsubs (filter #(< 1 (count %)) (partition-by increasing x))]
      (reduce #(if (> (count %2) (count %1)) %2 %1) [] incsubs))))

(defcheck solution-bbebbc6c
  (fn [xs]
    (let [subseqs (reduce concat (map #(partition % 1 xs) (range 2 (count xs))))
          inc-subseqs (filter #(= % (take (count %) (iterate inc (first %)))) subseqs)]
      (if (empty? inc-subseqs)
        '()
        (apply max-key count inc-subseqs)))))

(defcheck solution-bbf91496
  (fn [sequence]
    (loop [current-subseq []
           longest-subseq []
           rest-subseq sequence]
      (if (empty? rest-subseq)
        (if (>= (count longest-subseq) 2) longest-subseq [])
        (let [current-item (first rest-subseq)
              prev-item (last current-subseq)
              is-increasing? (or (nil? prev-item) (< prev-item current-item))]
          (if is-increasing?
            (let [new-current-subseq (conj current-subseq current-item)
                  new-longest-subseq (if (> (count new-current-subseq) (count longest-subseq)) new-current-subseq longest-subseq)]
              (recur new-current-subseq new-longest-subseq (rest rest-subseq)))
            (recur [] longest-subseq (cons current-item (rest rest-subseq)))))))))

(defcheck solution-bc0722de
  (fn [[a & s]]
    (let [l (first
              (reduce
                (fn [[g h] x]
                  (let [h1 (if (> x (last h)) (conj h x) [x])]
                    [(max-key count h1 g) h1]))
                [[a] [a]] s))]
      (if (> (count l) 1) l []))))

(defcheck solution-bc2f5d45
  #(
    (fn longest-seq [longestPrev current remaining]
      #_(println current)
      (if (empty? remaining)
        (if (or (>= (count longestPrev) (count current)) (< (count current) 2))
          longestPrev
          current
          )
        (if (< (last current) (first remaining))
          (recur longestPrev (concat current [(first remaining)]) (rest remaining))
          (if (or (>= (count longestPrev) (count current)) (< (count current) 2))
            (recur longestPrev [(first remaining)] (rest remaining))
            (recur current [(first remaining)] (rest remaining))
            )
          )
        )
      ) [] [(first %)]  %
    ))

(defcheck solution-bc486f99
  (fn [f e n c]
    (loop [r [] c c i []]
      (if (empty? c)
        r
        (let [j (conj i (f c))]
          (if (and (> (n j) (n r)) (> (n j) 1))
            (if (= (inc (f c)) (f (e c)))
              (recur j (e c) j)
              (recur j (e c) []))
            (if (= (inc (f c)) (f (e c)))
              (recur r (e c) j)
              (recur r (e c) []))))))) first rest count)

(defcheck solution-bc7fd247
  (fn longest-chain [c]
    (let [cnt (count c)
          increasing?  #(every? (fn [[x y]] (or  (nil? y)  (< x y))) (partition-all 2 1 %))
          candidates (for [x (range 0 (- cnt 2 -1))
                           y (range 2 (- cnt x -1))
                           :let [ seq (take y (drop x c)) ]
                           :when (increasing? seq)
                           ] seq)
          ]
      (if (empty? candidates) '() (->> candidates (group-by count) (apply max-key key) (last) (first)))
      )))

(defcheck solution-bcb4e1f8
  #(last (cons []
           (sort-by count
             (for[x (filter (fn[a](> (count a) 1))
                      (for[a (range 1 (+ 1 (count %)))  b (range (count %))]
                        (take a (drop b %))))
                  :when (= (range (first x) (+ (count x) (first x))) x)] x)))))

(defcheck solution-bcbb3d55
  (fn sub-seq [col]
    (let [max-sub-seq
          (->>
            (partition 2 1 col)
            (map #(if (< (first %) (last %)) % nil) )
            (partition-by nil?)
            (filter #(some identity %))
            (group-by count)
            (sort-by first)
            (last)
            (last)
            (first))]
      (cond
        (nil? max-sub-seq) []
        ( = 1 (count max-sub-seq)) (first max-sub-seq)
        :else (conj (map last max-sub-seq) (ffirst max-sub-seq))
        )
      )))

(defcheck solution-bcde7eb0
  (fn longest-inc-sub [v]
    (loop [[f & more] v
           last-res []
           last-size 1
           res [f]
           size 1]
      (if (empty? more)
        (if (> size last-size) res last-res)
        (if (> (first more) f)
          (recur more last-res last-size (conj res (first more)) (inc size))
          (if (> size last-size)
            (recur more res size [(first more)] 1)
            (recur more last-res last-size [(first more)] 1)
            )
          )
        )
      )))

(defcheck solution-bce612cd
  (fn [coll]
    (let [incrseqs
          ((fn [coll seqs curr]
             (if-let [s (seq coll)]
               (if (or (empty? curr) (> (first s) (last curr)))
                 (recur (rest s) seqs (conj curr (first s)))
                 (recur (rest s) (conj seqs curr) [(first s)])
                 )
               (conj seqs curr)
               )
             )
           coll [] [])]
      (let [ l (reduce
                 (fn [a b] (if (> (count b) (count a)) b a))
                 incrseqs) ]
        (if (> (count l) 1) l []))
      )
    ))

(defcheck solution-bd9e6d65
  (fn [coll]
    (let [inc-seq-fn (fn inc-seqs [xs]
                       (if (empty? xs) '()
                                       (let [diff      (map #(> %1 %2) xs (cons (dec (first xs)) xs))
                                             len       (count (take-while true? diff))]
                                         (cons (take len xs) (inc-seqs (drop len xs))))))
          inc-seqs (inc-seq-fn coll)
          max-len (apply max (map count inc-seqs))]
      (if (< max-len 2) '()
                        (some #(when (= (count %) max-len) %) inc-seqs)))))

(defcheck solution-bdb84407
  (fn long-inc-seq [xs]
    (let [[qs rs]
          (reduce (fn [[prev-seq acc] b]
                    (if (< (peek acc) b)
                      [prev-seq (conj acc b)]
                      (if (< 1 (count acc))
                        (if (< (count prev-seq) (count acc))
                          [acc [b]]
                          [prev-seq [b]])
                        [prev-seq [b]])))
            [[] [(first xs)]]
            (rest xs))
          longest (if (< (count qs) (count rs)) rs qs)]
      (if (< 1 (count longest)) longest []))))

(defcheck solution-bdf13b37
  (fn [in] (let [seqs (mapcat #(partition % 1 in) (range (count in) 1 -1))] (or (some #(if (= % (take (count %) (iterate inc (first %)))) %) seqs) []))))

(defcheck solution-be421237
  (fn [s]
    (letfn [(chunk-into-increasing-seq [s]
              (let [[a b] (map vec (split-at 1 s))]
                (loop [a a, b b]
                  (if (seq b)
                    (if (< (last a) (first b))
                      (recur (conj a (first b)) (rest b))
                      (cons a (chunk-into-increasing-seq b)))
                    [a]))))]
      (let [chunks (chunk-into-increasing-seq s)
            good-sized-chunks (filter #(> (count %) 1) chunks)]
        (if (seq good-sized-chunks)
          (let [sizes (map count good-sized-chunks)
                longest (apply max sizes)
                longest-chunks (filter #(= longest (count %)) good-sized-chunks)]
            (first longest-chunks))
          [])))))

(defcheck solution-be70565e
  (fn [v]
    (let [partition-by-pairwise
                  (fn [break-between? v]
                    (loop [todo (rest v)
                           parts []
                           current-part [(first v)]]
                      (if (empty? todo)
                        (conj parts current-part)
                        (let [e (first todo)]
                          (if (break-between? (last current-part) e)
                            (recur (rest todo) (conj parts current-part) [e])
                            (recur (rest todo) parts (conj current-part e)))))))
          longest (apply max-key count (reverse (partition-by-pairwise >= v)))]
      (if (>= (count longest) 2)
        longest
        []))))

(defcheck solution-be8f9cc1
  (fn [xs]
    (into [] (last (sort-by count (filter #(> (count %) 1)
                                    (map-indexed (fn [idx val]
                                                   (let [subxs (drop idx xs)
                                                         start val]
                                                     (map #(second %)
                                                       (take-while #(= (first %) (- (second %) start))
                                                         (map-indexed #(into [] [%1 %2]) subxs)))
                                                     )
                                                   ) xs)))))))

(defcheck solution-be8fa36
  (fn
    [coll]
    (let [pcoll (for [[f n] (partition 2 1 coll)]
                  (when (< f n)
                    [f n]))
          liss (->> pcoll
                 (partition-by nil?)
                 (remove (comp nil? first))
                 (sort-by (comp - count))
                 first)]

      (if (seq liss)
        (cons (ffirst liss) (map second liss))
        []))))

(defcheck solution-bf75c5f3
  (fn d[l]
    (let [res (apply max-key count
                (reverse (reductions
                           (fn [l x]
                             (if
                              (< (last l) x)
                               (conj l x)
                               (vector x)))
                           [8]
                           l)))]
      (if (>= (count res) 2)
        res
        []
        ))))

(defcheck solution-bfd2ca9
  (fn [s]
    (let [longest
          (->> (map-indexed vector s)
            (partition-by (fn [[i x]] (- x i)))
            (apply max-key count)
            (map second))]
      (if (< 1 (count longest))
        longest
        []))))

(defcheck solution-bfdc091c
  (fn [coll]
    (let [seqs (loop [c coll
                      s [[]]
                      m ##-Inf]
                 (if-let [f (first c)]
                   (if (> f m)
                     (recur (next c) (conj s (conj (last s) f) ) f)
                     (recur (next c) (conj s [f]) f))
                   s))
          fseqs (filter #(> (count %) 1) seqs)]
      (if-let [l (last (sort-by first (group-by count fseqs)))]
        (first (second l))
        []))))

(defcheck solution-c0495047
  (fn [coll]
    (letfn [(partition-between [pred? coll]
              (let [switch (reductions not= true (map pred? coll (rest coll)))]
                (map (partial map first) (partition-by second (map list coll switch)))))]
      (apply max-key count '() (filter #(> (count %) 1) (reverse (partition-between #(<= %2 %1) coll)))))))

(defcheck solution-c04a8dbf
  (fn [x] (or (->> (partition 2 1 x)
                (map (fn [[f n]] [(- n f) [f n]]))
                (partition-by #(= (first %) 1))
                (filter #(= (ffirst %) 1))
                (map #(distinct (reduce (fn [a [_ b]] (into a b)) [] %)))
                (sort #(> (count %) (count %2)))
                first)
              [])))

(defcheck solution-c06ae
  (fn  [col]
    (let [r (reduce (fn [x y] (if (> (count y) (count x)) y x)) []
              (reductions (fn [searched x]
                            (if (= (last searched) (dec x))
                              (concat searched [x])
                              [x]))
                []
                col))]
      (if (> (count r) 1)
        r
        []))))

(defcheck solution-c07db994
  (fn [col]
    (let [reslt
          (loop [[hd & rst] col ret []]
            (if (nil? hd)
              ret
              (do
                (let [t (last (last ret))]
                  (if-not (nil? t)
                    (if (= 1 (- hd t))
                      (recur rst (conj (pop ret) (conj (last ret) hd)))
                      (recur rst (conj ret [hd])))
                    (recur rst (conj ret [hd])) ) ))))]
      (reduce (fn [ret this]
                (if (and (> (count this) (count ret)) (> (count this) 1))
                  this
                  ret)) [] reslt))))

(defcheck solution-c084a3c7
  (fn [s]
    ((fn f [s longest cur l]
       (let [x (first s) r (rest s)
             longest (if (< (count longest) (count cur)) cur longest)
             ]
         (cond (empty? s) (if (= (count longest) 1) [] longest)
               (= l (dec x)) (f (rest s) longest (conj cur x) x)
               :else (f (rest s) longest [x] x)
               )
         )
       ) (rest s) [(first s)] [(first s)] (first s))

    ))

(defcheck solution-c08b26ce
  (fn longest [v]
    (let [insert-between
                 (fn insert-between [p v coll]
                   (lazy-seq
                     (when-let [s (seq coll)]
                       (when-let [one (first s)]
                         (let [two (second s)]
                           (if (not two)
                             [one]

                             (if (p one two)
                               (cons one
                                 (cons v
                                   (insert-between p v (rest s))))
                               (cons one
                                 (insert-between p v (rest s))))))))))
          groups (partition-by integer?
                   (insert-between
                     #(not= %2 (inc %1))
                     nil
                     v))
          ]
      (reduce (fn [acc item]
                (if (> (count item)

                      (count acc))
                  item
                  acc))
        ()
        (filter #(>= (count %) 2) groups))
      )))

(defcheck solution-c0aa02e
  #(let [increasing? (fn [[x y]] (> y x))]
     (->> (partition 2 1 %)                 ;; get pairs
       (partition-by increasing?)        ;; partition by increasing/not-inc.
       (filter (comp increasing? first)) ;; keep increasing pairs
       (sort-by count >)                 ;; longest increasing sequence
       first                             ;; ""
       flatten                           ;; flatten and dedupe (not in 1.4)
       (partition-by identity)
       (map first))))

(defcheck solution-c1622c6c
  (fn longest-increasing-sub-seq
    ([x]
     (longest-increasing-sub-seq (rest x) [(first x)] [(first x)]))
    ([x y z]
     #_(println z)
     (if (= 0 (count x))
       (if (> (count z) 1)
         z
         [])
       (if (= (first x) (inc (last y)))
         (if (= (count y) (count z))
           (recur (rest x) (conj y (first x)) (conj y (first x)))
           (recur (rest x) (conj y (first x)) z))
         (recur (rest x) [(first x)] z))))))

(defcheck solution-c173fb83
  (fn longest-subseq [xs]
    (letfn [(aux [xs cur chl]
              (let [a (first  xs)
                    b (second xs)
                    t (rest   xs)]
                (cond
                  (not a)         (if (<= (count cur) 1) [] cur)
                  (and b (< a b)) (if (empty? chl)
                                    (recur t cur [a b])
                                    (recur t cur (conj chl b)))
                  (> (count chl)
                    (count cur)) (recur t chl [])
                  :else           (recur t cur []))))]
      (aux xs [] []))))

(defcheck solution-c1caae7
  (fn longseq [s & sub?]
    (let
     [candidates
      (if (< (count s) 2) [] (concat (list  s) (longseq (butlast s) true) (longseq (rest s) true)))]
      (if sub?
        candidates
        (let [slist (sort #(compare (count %2) (count %1))
                      (filter (fn [cs] (apply < cs)) candidates))]
          (if (seq slist) (vec (first slist)) [] ))))))

(defcheck solution-c1e1c1fe
  (fn longest-subseq [coll]
    (when-let [[fst & rst] (seq coll)]
      (first
        (reduce (fn [[best cand] e]
                  (cond
                    (<= e (peek cand))     [best [e]]
                    (<= (inc (count cand))
                      (count best))      [best (conj cand e)]
                    :else                  [(conj cand e) (conj cand e)]))
          [[] [fst]] rst)))))

(defcheck solution-c1e97749
  (fn [t] (apply max-key #(or ({1 -1} (count %)) (count %))
            (reductions #(if (= (dec %2) (last %)) (conj % %2) [%2]) [] t))))

(defcheck solution-c2126b82
  (fn [seq]
    (let [res
          (apply
            max-key
            count
            (reverse
              (reduce
                #(if (= (inc (last (last %))) %2)
                   (conj % (conj (last %) %2))
                   (conj % [%2]))
                [[(first seq)]]
                (rest seq))))]
      (if (>= (count res) 2) res []))))

(defcheck solution-c2353c5d
  (fn [a](if-let [x (first(sort-by count > (filter #(apply < %) (mapcat #(partition % 1 a) (range 2 (count a))))))] x [])))

(defcheck solution-c2736de9
  (fn longest [s]
    (if (apply > s) []
                    (let
                     [
                      ss (partition-by #(apply < %) (partition 2 1 s))
                      m (apply max (map count  ss))
                      lng (first (drop-while #(not= m (count %)) ss))]
                      (cons (first (first lng)) (map (fn [[a b]] b) lng))
                      ))))

(defcheck solution-c27ce7fa
  (fn [xs]
    (let [crunch #(concat (map first %) (list (second (last %))))
          parted (partition-by #(< (first %) (second %)) (partition 2 1 xs))
          filtered (filter (fn [[[x y] & r]] (< x y)) parted)
          sorted (sort-by #(- (count %)) filtered)]
      (if (seq sorted)
        (crunch (first sorted))
        []))))

(defcheck solution-c28c6a69
  (fn longest-asc-sub [s]
    (let [
          [longest current _ :as result]
          (reduce
            (fn [[longest current last-element] i]
              #_(println longest current last-element)
              (if (and (not (nil? last-element)) (> i last-element))
                (let [new-current (conj current i)]
                  (if (> (count current) (count longest))
                    [new-current new-current i]
                    [longest new-current i]))
                [longest [i] i]))
            [[] [] nil]
            s)
          final (if (>= (count longest) (count current))
                  longest
                  current)]
      (if (>= (count final) 2)
        final
        []))))

(defcheck solution-c2d0ce7c
  (fn [[x & xs]]
    (let [longest (second
                    (reduce
                      (fn [[current longest] x]
                        (if (> x (peek current))
                          [(conj current x) longest]
                          [[x] (if (> (count current) (count longest)) current longest)]))
                      [[x] []]
                      (conj (vec xs) -1)))]
      (if (> (count longest) 1) longest []))))

(defcheck solution-c30ff7b9
  (fn [coll]
    (->> (partition 2 1 coll)
      (partition-by #(> (second %) (first %)))
      (filter #(> (second (first %)) (ffirst %)))
      (reduce #(if (< (count %1) (count %2)) %2 %1) [])
      flatten
      distinct)))

(defcheck solution-c36c050f
  (fn [x]
    (loop [i 0]
      (if (nil? (first (filter #(apply < %) (partition (- (count x) i) 1 x))))
        (if (= 2 (- (count x) i))
          []
          (recur (inc i)))
        (first (filter #(apply < %) (partition (- (count x) i) 1 x)))))))

(defcheck solution-c37a00df
  (let [part-incr (fn p [L]
                    (cond (empty? L) '()
                          (= 1 (count L)) (list L)
                          :else
                          (let [parted (p (rest L))]
                            (if (< (first L) (second L))
                              (cons (cons (first L) (first parted)) (rest parted))
                              (cons (list (first L)) parted)))))]
    (fn [S]
      (->> S
        (part-incr)
        (group-by count)
        (#(assoc % 1 [[]]))
        (apply max-key key)
        (val)
        (first)))))

(defcheck solution-c3a53bb
  (fn long-in-sub-seq [xs]
    (loop [prev [(first xs)] [h & t] (rest xs) current-seq []]
      (if h
        (if (> h (last prev))
          (if (>= (count prev) (count current-seq))
            (recur (conj prev h) t (conj prev h))
            (recur (conj prev h) t current-seq))
          (recur [h] t current-seq))
        current-seq))))

(defcheck solution-c3ef016e
  (fn [s]
    (reduce
      (fn [a b]
        (if (< (count a) (count b))
          b
          a))
      []
      (apply concat
        (for [i (range (count s))]
          (for [j (range (+ i 1) (count s))
                :while (< (s (dec j)) (s j))]
            (subvec s i (+ j 1))))))))

(defcheck solution-c435b81b
  (fn [initial-s]
    (loop [s initial-s
           longest []]
      (let [sub-seq (cons (first s)
                      (map second (take-while #(apply < %) (partition 2 1 s))))
            len (count sub-seq)]
        (cond
          (empty? s) longest
          (and (> len (count longest)) (> len 1)) (recur (rest s) sub-seq)
          :otherwise (recur (rest s) longest))))))

(defcheck solution-c4437948
  (fn longest-increasing-subsequence
    [v]
    (->> (for [start-index (range (count v))
               l (range 0 (inc (- (count v) start-index)))]
           (take l (drop start-index v)))
      (filter
        (fn is-increasing
          [s]
          (loop [numbers (rest s)
                 last-number (first s)]
            (cond
              (not (seq numbers)) true
              (> (first numbers) last-number)
              (recur (rest numbers) (first numbers))))))
      (remove #(= (count %) 1))
      (sort #(> (count %1) (count %2)))
      (first))))

(defcheck solution-c49aa195
  #(keep-indexed
     (fn [idx v] (if (or (odd? idx) (zero? idx)) v))
     (flatten
       (reduce
         (fn [a b] (if (< (count a) (count b)) b a)) []
         (filter
           (fn [itm] (< ((first itm) 0) ((first itm) 1)))
           (partition-by
             (fn [itm] (< (itm 0) (itm 1)))
             (map vector % (rest %))))))))

(defcheck solution-c514c6ca
  (fn clj4-rise
    [coll]
    (let [
          frags (for [n (range (count coll))] (drop n coll))
          rises (map (fn [frag]
                       (let [pairs (partition 2 1 frag)
                             rising-pairs (take-while #(< (first %) (second %)) pairs)]
                         (list* (apply sorted-set (flatten rising-pairs)))))
                  frags)
          longest (reduce #(if (> (count %2) (count %)) %2 %) [] rises)]
      longest)))

(defcheck solution-c57144ec
  (fn [s]
    (loop [guess [(first s)]
           ret guess
           xs (next s)]
      (if xs
        (if (= (inc (peek guess)) (first xs))
          (recur (conj guess (first xs)) ret (next xs))
          (if (>= (count ret) (count guess))
            (recur [(first xs)] ret (next xs))
            (recur [(first xs)] guess (next xs))))
        (if (>= (count ret) 2)
          (if (>= (count ret) (count guess))
            ret
            guess)
          [])))))

(defcheck solution-c57c9e2b
  (fn [col]
    (:best (reduce
             (fn [dict x]

               (let [best (:best dict)
                     prev (:current dict)
                     current (if (or (empty? prev)(> x (last prev)))
                               (conj prev x)
                               [x])]
                 (if (and (> (count current) (count best)) (> (count current) 1))
                   {:best current
                    :current current}
                   {:best best
                    :current current})))
             {:best []
              :current [] }
             col))))

(defcheck solution-c5884e3e
  (fn [v]
    (first (reduce (fn [[m c] i]
                     (let [nc (if (or (empty? c) (< (last c) i)) (conj c i) [i])
                           nm (if (and (< 1 (count nc)) (< (count m) (count nc))) nc m)]
                       [nm nc]))
             [[] []]
             v))))

(defcheck solution-c6028f56
  (fn [col] (loop [i (first col) j (rest col) ir [] fr '()] (if (nil? i) (first (reverse (sort-by count (map #(if (< 1 (count %)) % []) fr)))) (if (= (inc i) (first j)) (recur (first j) (rest j) (vec (concat ir [i])) fr) (recur (first j) (rest j) [] (conj fr (vec (concat ir [i])))))))))

(defcheck solution-c6434efd
  (fn liss [coll]
    (let [wat (->> coll
                (map list (rest coll))
                (map (partial apply >))
                (partition-by identity)
                (map (juxt first count)))
          largest (->> wat
                    (filter (comp true? first))
                    (map second)
                    (reduce max 0))
          to-drop (if (= largest 0)
                    (count coll)
                    (loop [skipped 0
                           remaining wat]
                      (let [head (first remaining)
                            tail (rest remaining)
                            is-true (true? (first head))
                            num (second head)]
                        (if (and is-true (= num largest))
                          skipped
                          (recur (+ skipped num) tail)))))]
      (take (inc largest) (drop to-drop coll))
      )))

(defcheck solution-c69cc5eb
  (fn [coll]
    (loop [r [] acc [] prev nil coll coll]
      (if-let [[a & coll] coll]
        (if (nil? prev)
          (recur r [a] a coll)
          (if (= a (inc prev))
            (recur r (conj acc a) a coll)
            (recur (conj r acc) [a] a coll)))
        (if-let [r (first (sort #(compare (count %2) (count %1)) (filter #(> (count %) 1) (conj r acc))))]
          r
          [])))))

(defcheck solution-c6da456f
  (fn [col]
    (->> (map #(conj [] %1 %2) (drop-last col) (rest col))
      (partition-by (fn [[a b]] (not= (- b a) 1)))
      (filter (fn [sub] (some (fn [[a b]] (= (- b a) 1)) sub)))
      (sort-by count)
      (last)
      (apply concat)
      (distinct))))

(defcheck solution-c76d64ec
  (fn get-longest-subseq [coll]
    (let [suc-subseq? (fn suc-subseq? [coll]
                        (every? #(= 1 %)
                          (map (fn [[a b]] (- b a))
                            (partition 2 1 coll))))
          between (fn between [x y coll]
                    (take (inc (- y x)) (drop x coll)))
          suc-subseqs (for [x (range 0 (dec (count coll)))
                            y (range 1 (count coll))
                            :while (suc-subseq? (between x y coll))]
                        (between x y coll))
          ret (first (sort-by #(count %) > (filter #(>= (count %) 2) suc-subseqs)))]

      (if (nil? ret)
        '()
        ret))))

(defcheck solution-c83f2e61
  (fn [s](map #(s %) (let [best (last (sort-by count (reverse (filter #(not (nil? (first %))) (partition-by nil? (map #(if (< 0 %2) % nil) (range) (map - (next s) s) ))))))] (if (nil? best) nil (concat best [(inc (last best))]))))))

(defcheck solution-c85604de
  (fn [xs]
    (or
     (first                        ;(0 1 2 3)
       (filter
         #(apply < %)              ;((0 1 2 3) (0 1 2) (1 2 3) (0 4 5) (0 1) (1 2) (2 3) (0 4) (4 5))
         (mapcat                   ;((1 0 1 2 3 0 4 5) (1 0 1 2 3 0 4) (0 1 2 3 0 4 5) (1 0 1 2 3 0) (0 1 2 3 0 4) (1 2 3 0 4 5) (1 0 1 2 3) (0 1 2 3 0) (1 2 3 0 4) (2 3 0 4 5) (1 0 1 2) (0 1 2 3) (1 2 3 0) (2 3 0 4) (3 0 4 5) (1 0 1) (0 1 2) (1 2 3) (2 3 0) (3 0 4) (0 4 5) (1 0) (0 1) (1 2) (2 3) (3 0) (0 4) (4 5))
           #(partition % 1 xs)     ;For example at 6 -> ((1 0 1 2 3 0) (0 1 2 3 0 4) (1 2 3 0 4 5))
           (range (count xs) 1 -1) ;(8 7 6 5 4 3 2)
           )
         )
       )
     []
     )
    ))

(defcheck solution-c88e8ae5
  (fn [s]
    (letfn [(addseqnum [x]
              (loop [res []
                     last nil
                     lastnum 0
                     todo x]
                (if (empty? todo)
                  res
                  (let [f (first todo)
                        r (next todo)]
                    (if (or (nil? last) (<= f last))
                      (recur (conj res [f (inc lastnum)]) f (inc lastnum) r)
                      (recur (conj res [f lastnum]) f lastnum r))))))]
      (let [decorated-seqs (partition-by second (addseqnum s))
            seqs (map (partial map first) decorated-seqs)
            sorted-seqs (sort-by #(- (count %)) seqs)
            candidate (first sorted-seqs)]
        (if (> (count candidate) 1)
          candidate
          [])))))

(defcheck solution-ca982560
  (fn [s]
    (first (reduce
             (fn [[l c] n]
               (let [p (last c)
                     d (cond (nil? p) [n]
                             (< p n) (conj c n)
                             :else [n])
                     k (if (and (> (count d) 1) (> (count d) (count l)))
                         d
                         l)]
                 [k d]))
             [[] []] s))))

(defcheck solution-ca9af58
  #(let [longest (apply max-key count
                   (reverse
                     (reduce
                       (fn [run n]
                         (let [current-seq (last run)
                               prev (last current-seq)]
                           (if (= prev (- n 1))
                             (concat (butlast run) [(conj current-seq n)])
                             (concat run [[n]]))))
                       []
                       %)))]
     (if (> (count longest) 1) longest [])))

(defcheck solution-caaaa5dc
  ;; Find all increasing sub-sequences, creating a sequence of them.
  ;; Group by their lengths into a map, find the maximum length
  ;; (i.e. key), and return the first of all sequences with that length.

  (fn [s]
    (let [lt (fn [[a b]] (< a b))
          ge (complement lt)
          all-increasing-seqs
             (->> (take-while #(seq (first %))
                    (rest
                      (iterate #(split-with lt (drop-while ge (second %)))
                        (list 0 (partition 2 1 s)))))
               (map first)
               (map #(cons (ffirst %) (map second %))))
          by-length (group-by count all-increasing-seqs)
          lengths (keys by-length)]
      (if (seq lengths)
        (first (by-length (apply max lengths)))
        []))))

(defcheck solution-cad18164
  (fn [x]
    (into [] (first
               (sort
                 #(- (count %2) (count %1))
                 (filter
                   #(apply < %)
                   (for [a (range (count x)) b (range 2 (- (count x) a -1))] (take b (drop a x)))))))))

(defcheck solution-cbe6f262
  (fn longest [collect]
    (loop [col (rest collect),
           preview (first collect),
           result [1],
           n 1,
           curr [(first collect)]]
      (if (empty? col)
        (cond (< n 2) []
              (< (first result) n) curr
              true (rest result)
              )
        (let[
             val (first col),
             sign (> val preview)
             ]
          (if  (true? sign)
            (recur (rest col)
              val
              result
              (inc n)
              (conj curr val)
              )
            (recur (rest col)
              val
              (if (< (first result) n)
                (cons n curr)
                result
                )
              1
              [val]
              )
            )
          )
        )
      )
    ))

(defcheck solution-ccce799f
  (fn longest-inc-sub-seq
    [xs]
    (let [increasing? (fn [[a b]] (= (inc a) b))]
      (->> xs
        (partition 2 1)
        (map #(if (increasing? %) % (second %)))
        (partition-by type)
        (map #(when (seq? (first %)) ((comp flatten) %)))
        (filter seq?)
        (map distinct)
        (filter #(> (count %) 1))
        (reduce #(if (> (count %2) (count %)) %2 %) '())))))

(defcheck solution-cd251bec
  (letfn [
          (my-partition [s]
            (reduce (fn [a v]
                      (let [l (last (last a))]
                        (if (or (nil? l) (<= v l))
                          (conj a [v])
                          (conj (vec (butlast a)) (conj (last a) v))))) [] s))
          (longest-partition [s]
            (let [longest (->> s (my-partition)
                            (filter #(> (count %) 1))
                            (sort-by #(- (count %)))
                            (first))]
              (if (nil? longest) [] longest)))]
    longest-partition))

(defcheck solution-cd3d5f91
  (fn [S] (or (first (filter (partial apply <) (mapcat (fn [l] (partition l 1 S)) (range (count S) 1 -1)))) [])))

(defcheck solution-cd908b65
  (fn [s]
    (reduce #(if (> (count %2) (count %)) %2 %) []
      (remove #(= 1 (count %))
        (partition-by integer?
          (reduce #(concat % (if (<= %2 (last %)) [:a %2] [%2])) [(first s)] (rest s)))))))

(defcheck solution-cda6dbcf
  (fn longest-sub [c]
    (let [c (map (fn [[a :as c]] (map
                                   #(vector % (= (- % a) %2))
                                   c (range)))
              (partition-all (count c) 1 c))
          d (filter #(> (count %) 1)
              (map #(take-while second %) c))]
      (if (empty? d) [] (map first (apply max-key count d))))))

(defcheck solution-ce15c4f1
  (fn [s]
    (#(concat
       (map first %)
       (take-last 1 (last %)))
     (apply max-key #(count %)
       (reverse
         (let [ss (map vector s (next s))]
           (map (fn [x]
                  (take-while #(apply < %) x))
             (take (count s) (iterate rest ss))
             ))
         )
       ))
    ))

(defcheck solution-ce21e919
  (fn [coll]
    (map first
      (last(first
             (filter (comp (partial < 1) count val)
               (sort-by (comp (partial * -1) count val)
                 (group-by #(last %)
                   (reduce
                     (fn [aggr x]
                       (if (empty? aggr) [[x 0]]
                                         (let [a (last aggr)] (if (< (first a) x) (conj aggr [x (last a)]) (conj aggr [x (inc(last a))])))
                                         )
                       ) [] coll
                     )
                   )
                 )
               ))))))

(defcheck solution-ce93f863
  (fn*
    [p1__2876#]
    (sort
      (set
        (flatten
          (map
            (fn [[a1 a2 a3]] (vector a2 a3))
            (apply
              max-key
              count
              (conj (reverse
                      (filter
                        ffirst
                        (partition-by
                          first
                          (map
                            (fn [[a1 a2]] (if (< a1 a2) (vector true a1 a2) (vector false a1 a2)))
                            (partition
                              2
                              (interleave
                                p1__2876#
                                (conj (vec (rest p1__2876#)) (last p1__2876#)))))))) '()))))))))

(defcheck solution-cec87a7f
  (fn sep [xs]
    (let [consec #(if (or (empty? (last %))(< (last (last %)) %2))
                    (conj (vec (butlast %)) (conj (last %) %2))
                    (conj % [%2]))
          firstmax #(if (> (count %2) (count %)) %2 %)]
      (->> (reduce consec [[]] xs)
        (filter #(> (count %) 1))
        (reduce firstmax [])))))

(defcheck solution-cf4dba49
  (fn lseq [ll]
    (loop [l ll
           bef nil
           r []
           ans []]
      (if (nil? bef)
        (recur (rest l) (first l) (conj r (first l)) [])
        (if (empty? l) (if (> (count ans) 1) ans [])
                       (let [nr (if (= (- (first l) bef) 1) (conj r (first l)) [(first l)])]
                         (recur (rest l)
                           (first l)
                           nr
                           (if (empty? ans) nr
                                            (if (> (count nr) (count ans)) nr ans)))))))))

(defcheck solution-cfc9f394
  (fn longest-inc-seq [coll]
    (reduce #(let [len-a (count %1)
                   len-b (count %2)]
               (if (and (> len-b 1) (> len-b len-a)) %2 %1))
      []
      (reductions
        (fn [xs y]
          (if (> y (last xs)) (conj xs y) [y]))
        [(first coll)]
        (rest coll)))))

(defcheck solution-d043fd47
  (fn [s]
    (loop [[x & r] s, k 0, [m n] [0 0], [i j] [0 1]]
      (cond
        (nil? r) (cond
                   (= n 0) []
                   (>= n j) (take n (drop m s))
                   :else (take j (drop i s)))
        (< x (first r)) (if (>= n j)
                          (recur r (inc k) [m n] [i (inc j)])
                          (recur r (inc k) [i (inc j)] [i (inc j)]))
        :else (recur r (inc k) [m n] [(inc k) 1])))))

(defcheck solution-d0712ac
  (fn [l]
    (letfn [(increasing-prefix [l]
              (take (inc (count (take-while identity (map < l (rest l))))) l))
            (suffixes [l]
              (if-let [s (seq l)]
                (cons s (suffixes (rest s)))))]
      (or (->> (suffixes l)
            (map increasing-prefix)
            (sort-by (comp - count))
            (filter #(> (count %) 1))
            (first))
          []))))

(defcheck solution-d1d733af
  (fn [coll]
    (let [r (last (sort-by count (reduce
                                   (fn [acc i]
                                     (let [current (first acc)]
                                       (if
                                        (empty? current) (cons [i] (rest acc))
                                                         (if (> i (last current)) (cons (conj current i) (rest acc)) (cons [i] acc))
                                                         )
                                       ))
                                   [[]]
                                   coll)))]
      (if (> (count r) 1) r []))
    ))

(defcheck solution-d212180f
  (fn [xs]
    (let [
          zip (fn zip [xs ys]
                (lazy-seq
                  (when-first [x xs]
                    (when-first [y ys]
                      (cons [x y] (zip (rest xs) (rest ys)))))))
          fst (fn [pair] (nth pair 0))
          snd (fn [pair] (nth pair 1))
          pairwise-partition-by (fn [f xs]
                                  (let [f* (partial apply f)]
                                    (when (seq xs)
                                      ((fn impl [pairs]
                                         (when (seq pairs)
                                           (let [[this-block the-rest] (split-with f* (rest pairs))]
                                             (lazy-seq
                                               (cons (cons (snd (first pairs)) (map snd this-block))
                                                 (impl the-rest))))))
                                       (cons [nil, (first xs)] (zip xs (rest xs)))))))
          longest (fn [acc xs]
                    (if (< (count acc) (count xs)) xs acc))
          ]
      (->> xs
        (pairwise-partition-by <)
        (filter #(< 1 (count %)))
        (reduce longest '())))))

(defcheck solution-d2848ce6
  (fn [items]
    (loop [seq-items(rest items) current [(first items)] result []]
      (let [item (first seq-items) last-item (last current) valid? (and (not (nil? item)) (= (inc last-item) item))]
        (if (nil? item)
          result
          (recur (rest seq-items)
            (if valid?
              (conj current item)
              [item])
            (if (and valid? (>= (count current) (count result)))
              (conj current item)
              result)))))))

(defcheck solution-d2ee240b
  (fn longest-subseq--group
    [coll] {:pre [(every? integer? coll)]}
    (->> coll
      ;; The following line replaces each element with the longest increasing
      ;; subsequence that ends in that element.
      (reductions #(if (and (seq %1) (> %2 (peek %1))) (conj %1 %2) [%2]) [])
      (group-by count)      ; a map of all increasing susbseqs, keyed by length
      (apply max-key key)
      last                  ; a vector of all longest increasing subseqs
      first                 ; the first, longest increasing subseq
      ((fn [x] (if (= (count x) 1) [] x))))))

(defcheck solution-d2fe6a2
  (fn [a] (map first (last (sort-by count (filter #(> (count %) 1) (partition-by second (map-indexed #(list %2 (- % %2)) a))))))))

(defcheck solution-d319e6be
  (fn [nums]
    (let [size-fn (fn [n] (if (>= (count n) 2) n []))
          partition-fn (fn [v n]
                         (if (= (dec n) (last (last v)))
                           (update-in v [(dec (count v))] conj n)
                           (conj v [n])))
          pick-fn (fn [o n] (if (>= (count o) (count n)) o n))]
      (->>
        (reduce partition-fn [] nums)
        (reduce pick-fn)
        size-fn))))

(defcheck solution-d33b60e9
  (fn sub [coll]
    (->> coll
      (partition 2 1)
      (partition-by (partial apply <))
      (filter (fn [[[a b]]] (< a b)))
      (reduce (fn [m s] (if (> (count s) (count m)) s m)) [])
      (#(if (seq %) (cons (ffirst %) (map second %)) [])))))

(defcheck solution-d340153c
  (fn [se]
    (let [
          counts (first (reduce
                          (fn [[res prev] item]
                            (if (< prev item)
                              [(conj res (inc (last res))) item]
                              [(conj res 1) item])
                            )
                          [[0] 0]
                          se))
          mmm (reduce max counts)
          res1 (first (filter #(> %1 1) (filter pos? (map-indexed (fn [x a] (if (= a mmm) x 0)) counts))) )
          ]
      #_(println counts mmm res1)
      (if (> mmm 1)
        (take mmm (drop (- res1 mmm) se))
        []))))

(defcheck solution-d34a2766
  (fn [coll]
    (loop [[x & xs] coll, longest [], current []]
      (let [increasing? ((fnil < -2147483648) (last current) x)
            new-current (if increasing? (conj current x) [x])
            new-longest (if (> (count new-current) (count longest)) new-current longest)]
        (if xs
          (recur xs
            new-longest
            new-current)
          (if (< 1 (count new-longest)) new-longest []))))))

(defcheck solution-d38a1654
  (let [increasing? (fn [l] (if (empty? l)
                              true
                              (apply = (map - l (range)))))]
    (fn [l]
      (first (for [len (range (count l) -1 -1)
                   :when (not (= len 1))
                   the-seq (partition len 1 l)
                   :when (increasing? the-seq)]
               the-seq)))))

(defcheck solution-d454f5f0
  #(->> %
     (partition 2 1)
     (partition-by (fn [a] (apply < a)))
     (filter (fn [a] (apply < (first a))))
     (group-by count)
     (sort-by key >)
     first
     second
     first
     ((juxt (partial map first) (comp last last)))
     flatten
     (filter (comp not nil?))))

(defcheck solution-d4b56de1
  (fn [c]
    (letfn [(incr? [s i] (or (zero? i) (> (nth s i) (nth s (dec i)))))
            (chk [s]
              (when-not (empty? s)
                (let [i (loop [i 1] (if (and (< i (count s)) (incr? s i)) (recur (inc i)) i))]
                  (cons (take i s) (chk (drop i s))))))]
      (let [sqs (chk c)
            m (apply max (map count sqs))]
        (if (> m 1) (first (filter #(= (count %) m) sqs))
                    ())))))

(defcheck solution-d4f0acd
  (fn [xs]
    (let
     [candidates
      (reverse (filter #(and (apply < %) (> (count %) 1))
                 (mapcat (fn [y] (map #(take (inc %) (drop y xs)) (range (count xs)))) (range (count xs)))))]
      (if (empty? candidates)
        []
        (apply max-key count candidates)))))

(defcheck solution-d50a793e
  (fn [s]
    (or
     (first
       (filter #(every? (fn [[a b]] (< a b)) (partition 2 1 %))
         (map (fn [[a b]] (subvec s a b))
           (sort-by #(apply - %)
             (let [c (inc (count s))]
               (for [i (range c)
                     j (range c)
                     :when (> j (inc i))]
                 [i j]))))))
     [])))

(defcheck solution-d56ab5cd
  (fn [a-seq]
    (let [llast (fn [a-seq]
                  (-> a-seq last last))
          inc? (fn [a-seq n]
                 (if ((fnil (partial > n)0)
                      (llast a-seq))
                   (conj (vec (drop-last a-seq))
                     (conj (last a-seq) n))
                   (conj a-seq [n])))
          incr-subseqs (->> a-seq
                         (reduce inc? [[]])
                         (group-by count))
          max-count (->> incr-subseqs keys (apply max))]
      (if (>= max-count 2)
        (-> incr-subseqs
          (get max-count)
          first)
        []))))

(defcheck solution-d5e0544b
  (fn [s]
    (let [f #(conj %
               (if
                (some (partial <= %2) (last %))
                 [%2]
                 (conj (last %) %2)))
          a (reduce f [[]] s)
          b (apply max-key count (reverse a))]
      (if (> (count b) 1) b []))))

(defcheck solution-d68f1ca5
  (fn [xs]
    (->> (map #(vector %1 (- %1 %2)) (rest xs) xs)
      (cons [(first xs) -1])
      (#(loop [agg () [x & tail] %]
          (if (empty? tail)
            agg
            (let [[ys y-tail] (split-with (comp pos? second) tail)
                  zs (cons x ys)]
              (if (> (count zs) (count agg))
                (recur zs y-tail)
                (recur agg y-tail))))))
      (map first)
      (#(if (seq (rest %)) % ())))))

(defcheck solution-d7140d5d
  (fn [xs]
    (reduce
      #(if (and (< 1 (count %2)) (> (count %2) (count %))) %2 %)
      []
      (reductions
        #(if (or (empty? %) (> %2 (peek %))) (conj % %2) [%2])
        [] xs))))

(defcheck solution-d71b1380
  (fn [s]
    (let [f first
          spl (partition-by #(< (f %) (last %)) (partition 2 (interleave s (rest s))))
          fi (filter #(< (f (f %)) (second (f %))) spl)]
      (if (empty? fi) []
                      (let [g (f (filter #(= (count %) ((fn [x] (apply max (map count x))) fi)) fi))]
                        (cons (f (f g)) (map last g)))))))

(defcheck solution-d7af6e5
  (comp #(if (<= 2 (count %)) % '()) #(map first (first (sort-by (comp - count) (partition-by last (map list % (reductions + (cons 0 (map (fn [[x1 x2]] (if (< x1 x2) 0 1)) (partition 2 1 %)))))))))))

(defcheck solution-d8258dc
  (fn [col]
    (let [partitioned (reduce
                        (fn [acc v]
                          (if (or (empty? acc) (not (= v (+ 1 (last (last acc))))))
                            (concat acc (list (list v)))
                            (concat (butlast acc) (list (concat (last acc) (list v))))))
                        ()
                        col)]
      (let [sorted (reverse (sort-by count partitioned))]
        (if (> (count (first sorted)) (count (second sorted)))
          (first sorted)
          ())))))

(defcheck solution-d9f0b410
  (fn [x]
    (or
     (first
       (sort-by count >
         (for [a (range (count x))
               b (range (+ a 2) (inc (count x)))
               :let [s (subvec x a b)]
               :when (apply < s)]
           s))) [])))

(defcheck solution-da04a02f
  (fn [coll]
    (let [result (last
                   (sort
                     (group-by count
                       (map #(reduce (fn [x y]
                                       (conj (vec x)
                                         (second y)))
                               (first %)
                               (rest %))
                         (filter #(< (first (first %))
                                    (second (first %)))
                           (partition-by #(< (first %)
                                            (second %))
                             (partition 2
                               1
                               coll)))))))]
      (if result
        (first (second result))
        []))))

(defcheck solution-da0be4eb
  (fn [[f & r]]
    (loop [[x & xs] r, current [f], longest [f]]
      (let [longer (fn [c1 c2]
                     (if (> (count c1) (count c2)) c1 c2))]
        (if (nil? x)
          (if (> (count (longer current longest)) 1) (longer current longest) [])
          (if (> x (last current))
            (recur xs (conj current x) longest)
            (recur xs [x] (longer current longest))))))))

(defcheck solution-da0d1d5e
  (fn[xs]
    (let [longest (loop [pairs (partition 2 1 xs)
                         result []]
                    (let [[head tail] (split-with (fn [[a b]] (= (inc a) b)) pairs)
                          result (if (> (count head) (count result)) head result)
                          ]
                      (if (seq tail) (recur (if (seq head) tail (rest tail)) result) result)))]
      (if (seq longest)
        (concat (vector (ffirst longest)) (map second longest))
        longest))))

(defcheck solution-da76b431
  (fn [[h & t]]
    (let [ss (first
               (sort-by count >
                 (reduce (fn [c n]
                           (let [ls (peek c)]
                             (if (= (- n (peek ls)) 1)
                               (conj (pop c) (conj ls n))
                               (conj c [n])))) [[h]] t)))]
      (if (> (count ss) 1) ss []))))

(defcheck solution-daa6582a
  (fn [coll]
    (let [sub-coll (loop [orig coll, current [], result []]
                     (if (empty? orig)
                       (conj result current)
                       (let [[h & t] orig, l (last current)]
                         (if (or (nil? l) (< l h))
                           (recur t (conj current h) result)
                           (recur t (vector h) (conj result current)))))),
          subseq-coll (filter #(> (count %) 1) sub-coll)]
      (if (empty? subseq-coll)
        []
        (->> subseq-coll
          (group-by count)
          (sort-by key)
          last
          val
          first)))))

(defcheck solution-daebdf57
  ; This is a clever way to do this. I did not come up with this.
  ; Break it into tuples. File out the ones that are not (< 1 2). Put them together. REVERSE??? Look for the largest.

  (fn [coll]
    (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                          (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                            (partition-by
                              #(< (first %) (last %))
                              (partition 2 1 coll)))))]
      (if (empty? seqs)
        []
        (apply (partial max-key count) seqs)))))

(defcheck solution-db43b7e2
  (fn longest [v]
    (let [grouped (->> v (keep-indexed (fn [i e] [(- i e) e])) (group-by #(% 0)))]
      (if (every? #(= 1 (count (val %))) grouped)
        []
        (->> grouped vals (apply max-key count) (mapv #(% 1)))))))

(defcheck solution-dc2b1ea1
  (fn [coll]
    (let [len (count coll)
          cmp (fn [[x1 y1] [x2 y2]] (< (- y1 x1) (- y2 x2)))]
      (loop [i 0 j 1 best [0 1] ]
        (if (> i (dec len))
          (if (= 1 (- (last best) (first best)))
            []
            (subvec coll (first best) (last best)))
          (if (or (= j len)
                  (not (= (- j i) (- (nth coll j) (nth coll i)))))
            (recur j (inc j)
              (if (cmp best [i j]) [i j] best))
            (recur i (inc j) best)))
        ))))

(defcheck solution-dd0529d0
  (fn [coll]
    (let [z (filter #(> (count %) 1)
              (reduce (fn [c e]
                        (let [z (last c)]
                          (if (= (inc (last z)) e)
                            (conj (subvec c 0 (dec (count c))) (conj z e))
                            (conj c [e]))))
                [[(first coll)]] (rest coll)))]
      (if (empty? z) () (apply max-key count z)))))

(defcheck solution-dd2e5d4a
  (fn liss [xs]
    (let [inc-prefix-len
          (fn [xs]
            (loop [i 1, prev (first xs)]
              (if (contains? xs i)
                (let [xi (nth xs i)]
                  (if (> xi prev)
                    (recur (inc i) xi)
                    i))
                i)))]
      (loop [xs' xs,
             answer []]
        (if (empty? xs')
          answer
          (let [k (inc-prefix-len xs')
                answer' (when (> k 1) (vec (take k xs')))]
            (recur (vec (drop k xs'))
              (if (and answer' (> k (count answer)))
                answer' answer))))))))

(defcheck solution-dd6f2c
  (fn [x] (letfn [
                  (split [x] (loop [ [f & r :as x] x, c [], ans [[]] ]
                               (cond (empty? x) (conj ans c)
                                     (or (empty? c) (< (last c) f)) (recur r (conj c f) ans)
                                     :else (recur r [f] (conj ans c))
                                     )))
                  (max [x] (loop [ [f & r :as x] x, c [], n 0]
                             (cond (empty? x) c
                                   (< n (count f)) (recur r f (count f))
                                   :else (recur r c n))))
                  ] (let [ maxseq (max (split x))] (if(< 1 (count maxseq)) maxseq [])))))

(defcheck solution-ddba938
  (fn longest-sub-seq [s]
    (letfn [(take-while-> [s]
              (cons (first s)
                (when (seq (rest s))
                  (when (> (second s) (first s))
                    (take-while-> (rest s))))))]
      (let [r (reduce #(if (>= (count %) (count %2)) % %2)
                (map take-while->
                  (take (count s) (iterate rest s))))]
        (if (>= (count r) 2) r [])))))

(defcheck solution-ddd87f13
  (fn [coll]
    (if (empty? coll)
      []
      (loop [xs (rest coll)
             best []
             x (first coll)
             current [(first coll)]]
        (cond (empty? xs) (if (and (> (count current) (count best)) (> (count current) 1))
                            current
                            best)
              (> (first xs) x) (recur (rest xs) best (first xs) (conj current (first xs)))
              true (if (and (> (count current) (count best)) (> (count current) 1))
                     (recur (rest xs) current (first xs) [(first xs)])
                     (recur (rest xs) best (first xs) [(first xs)])))))))

(defcheck solution-de15a044
  #(loop [xs [] cand [(first %)] ls (rest %)]
     (cond
       (empty? ls) (if (and (> (count cand) (count xs))
                            (> (count cand) 1)) cand xs)
       (> (first ls) (last cand)) (recur xs (conj cand (first ls)) (rest ls))
       (and (> (count cand) (count xs))
            (> (count cand) 1)) (recur cand [(first ls)] (rest ls))
       :else (recur xs [(first ls)] (rest ls)))))

(defcheck solution-de3e7a8d
  (fn [xs]
    (let [cnt (count xs)]
      (if (>= 1 cnt)
        xs
        (let [is-inc #(< (xs (dec %)) (xs %))
              release #(if (< (count %) 2) '() %)
              subvec #(subvec xs %1 %2)
              last (dec cnt)]
          (loop [start 0 end 0
                 curstart 0 curend 1]
            (if (== curend last)
              (if (is-inc curend)
                (if (< (- end start) (- curend  curstart))
                  (release (subvec curstart (inc curend)))
                  (release (subvec start    (inc end))))
                (if (< (- end start) (- (dec curend) curstart))
                  (release (subvec curstart curend))
                  (release (subvec start (inc end)))))
              (if (is-inc curend)
                (recur start end curstart (inc curend))
                (if (< (- end start) (- curend 1 curstart))
                  (recur curstart (dec curend) curend (inc curend))
                  (recur start end curend (inc curend)))))))))))

(defcheck solution-dee3d2ff
  (fn [xs]
    (->> xs
      (map #(vector % %2) (range))
      (partition-by #(- (last %) (first %)))
      reverse
      (apply max-key count)
      (map last)
      (#(if (< (count %) 2) [] %)))))

(defcheck solution-df4c7965
  (fn [coll]
    (let [f (fn [acc n]
              (cond
                (empty? acc) [n]
                (= (dec n) (peek acc)) (conj acc n)
                :else [n]))
          ret (filter #(> (count %) 1)
                (reductions f [] coll))]
      (if (empty? ret)
        ret
        (apply max-key count ret)))))

(defcheck solution-df8100de
  (fn longest-increasing-subsequence
    [s]
    (letfn [(increasing-subsequences [s current-seq]
              (lazy-seq
                (cond
                  (empty? s) (list current-seq)
                  (or (empty? current-seq)
                      (> (first s) (last current-seq)))
                  (increasing-subsequences (rest s)
                    (conj current-seq (first s)))
                  :else (cons current-seq
                          (increasing-subsequences s [])))))]
      (or (first (filter #(> (count %) 1)
                   (sort-by (comp - count)
                     (increasing-subsequences s []))))
          []))))

(defcheck solution-e1073000
  (fn [v] (concat [] (first (filter #(apply < %) (apply concat (for [x (range (count v) 1 -1)] (partition x 1 v))))))))

(defcheck solution-e1484529
  (fn [xst]
    (loop [xs (rest xst) longsubseq [(first xst)] saved []]
      (if (empty? xs)
        ((fn [xs] (if (> (count xs) 1) xs [])) (if (> (count longsubseq) (count saved)) longsubseq saved))
        (if (= (inc (last longsubseq)) (first xs))
          (recur (rest xs) (conj longsubseq (first xs)) saved)
          (recur (rest xs) [(first xs)] (if (> (count longsubseq) (count saved)) longsubseq saved)))))))

(defcheck solution-e14f55a
  (fn [s]
    ((fn [x] (case x nil (), x))
     (first
       (filter
         (partial apply <)
         (mapcat #(partition % 1 s) (range (count s) 1 -1)))))))

(defcheck solution-e157bf07
  (fn [xs]
    (let [seqs (reduce (fn [{:keys [all want]} i]
                         (let [cur (peek all)
                               prev (pop all)]
                           {:all (if (= i want)
                                   (conj prev (conj cur i))
                                   (conj all [i]))
                            :want (inc i)}))
                 {:all [[]] :want nil}
                 xs)]
      (apply max-key count
        (remove #(= 1 (count %))
          (:all seqs))))))

(defcheck solution-e1cfa319
  (fn [coll] (loop [[x & xs] coll sub [] ss []]
               (if x
                 (cond
                   (or (empty? sub)
                       (> x (last sub))) (recur xs (conj sub x) ss)
                   (= (count sub) 1) (recur xs [x] ss)
                   :else (recur xs [x] (if (> (count sub)
                                             (count ss)) sub ss)))
                 (if (> (count sub) (count ss) 1) sub ss)))))

(defcheck solution-e219d9f3
  #(reduce
     (fn [a b]
       (if (and (sequential? b)
                (> (count b) 1))
         (cond
           (= (count a)(count b)) b
           (< (count a)(count b)) b
           :else a)
         a))
     []
     (loop [c (rest %)
            t [(first %)]
            r []]
       (if (empty? c)
         (conj r t)
         (if (= (inc (last t))(first c))
           (recur (rest c) (conj t (first c)) r)
           (recur (rest c) [(first c)] (conj r t)))))))

(defcheck solution-e22c705f
  (fn [s]
    (vec (apply max-key count
           (reverse (map
                      (fn [s]
                        (into (sorted-set) (flatten
                                             (take-while (fn [[a b]] (= (inc a) b))
                                               (partition 2 1 s)))) )
                      (take (count s) (iterate rest s))))))))

(defcheck solution-e250cab5
  (comp #(if (< 1 (count %)) % [])
        #(map first
           (first (sort-by count >
                    (partition-by last
                      (map list % (reductions + (cons 0 (map
                                                          (fn [[x1 x2]] (if (< x1 x2) 0 1)) (partition 2 1 %)))))))))))

(defcheck solution-e273bca5
  (fn [s]
    (reduce
      (fn [t,i] (if (and (> (count i) 1) (> (count i) (count t))) i t))
      []
      (map #(loop [x %] (if (apply < x) x (recur (butlast x))))
        (partition-all (count s) 1 s)))))

(defcheck solution-e2e62cae
  (fn [sx] (get (reduce #(
                           if (= (dec %2) (get %1 :v))
                           (if (and (>= (count (get %1 :cs)) (count (get %1 :maxs))) (>= (count (get %1 :cs)) 2)  )
                             {:v %2 :cs (concat (get %1 :cs) [%2]) :maxs (concat (get %1 :cs) [%2])}
                             {:v %2 :cs (concat (get %1 :cs) [%2]) :maxs (get %1 :maxs)}
                             )
                           (if (and (>= (count (get %1 :cs)) (count (get %1 :maxs))) (>= (count (get %1 :cs)) 2)  )
                             {:v %2 :cs [%2] :maxs (get %1 :cs)}
                             {:v %2 :cs [%2] :maxs (get %1 :maxs)}
                             )
                           ) {:v (first sx)  :cs [] :maxs [] } sx) :maxs)))

(defcheck solution-e2ee8c34
  (fn [s]
    (->>
      (for [a (range (count s))
            b (range (inc a) (count s))]
        (subvec s a (inc b)))
      (filter #(apply < %))
      (sort-by count >)
      first
      vec)))

(defcheck solution-e33608b7
  (fn [-seq]
    (->> (map
           (fn [it index]
             (loop [next-items (next (last (split-at index -seq))) stock [it]]
               (if (= (first next-items) (inc (last stock)))
                 (recur (rest next-items) (conj stock (first next-items)))
                 stock
                 )))
           -seq (range (count -seq))
           )
      (map  #(if (= 1 (count %)) [] %))
      (sort #(compare (count %2) (count %1)))
      (first)
      )

    ))

(defcheck solution-e33a83f9
  (fn lsubseq [xs]
    (let [[a b]
          (reduce (fn [[curlong longest] c]
                    (cond
                      (or (nil? curlong)
                          (empty? curlong))
                      [[c] longest]

                      (= c (+ 1 (last curlong)))
                      [(conj curlong c) longest]

                      :else
                      (if (and (> (count curlong)
                                 (count longest)))
                        [[c] curlong]
                        [[c] longest]))) [[] []] xs)]
      (cond
        (and (<= (count a) 1)
             (<= (count b) 1))
        []

        (> (count a) (count b))
        a

        :else
        b))))

(defcheck solution-e383a467
  (fn [a]
    (let [better #(if (and (> (count %1) (count %2)) (>= (count %1) 2)) %1 %2)]
      (loop [[h & xs] (seq a)
             c []
             best []]
        #_(println c best)
        (if h
          (if (or (empty? c) (= h (inc (last c))))
            (recur xs (conj c h) best)
            (recur xs [h] (better c best)))
          (better c best))))))

(defcheck solution-e397a2e6
  (fn [coll]
    (let [mymax (fn ([] 0)
                  ([& args] (apply max args)))
          subs (filter (fn [[[x y] & ns] & seqs]
                         (< x y))
                 (partition-by (fn [[a b]] (< a b))
                   (partition 2 1 coll)))
          maxcount (apply mymax (map count subs))
          right-one (first (drop-while #(not= maxcount (count %))
                             subs))
          result (flatten [(mapv first (butlast right-one))
                           (last right-one)])]
      (if (= result '(nil))
        []
        result))))

(defcheck solution-e3e33299
  (fn [coll]
    (or (some
          (fn [n]
            (some (fn [m]
                    (let [v (subvec coll m (+ m n))]
                      (if (and (> (count v) 1) (apply < v))
                        v)))
              (range (- (inc (count coll)) n))))
          (range (count coll) 1 -1)) [])))

(defcheck solution-e4009e95
  (fn [s]
    (let
     [subvecs (for [x (range (inc (count s)))
                    y (reverse (range (inc x) (inc (count s))))]
                (subvec s x y))
      increasing-subvecs (filter #(and (apply < %) (> (count %) 1)) subvecs)
      max-count (apply max 2 (map count increasing-subvecs))
      biggest-increasing-subvecs (filter (comp (partial = max-count)
                                               count)
                                   increasing-subvecs)]
      (if (empty? biggest-increasing-subvecs) [] (first biggest-increasing-subvecs)))))

(defcheck solution-e4015c1e
  (fn [l] (let [seqs (loop [acc [] current [] xs l]
                       (if (empty? xs) (concat acc  [(reverse current)])
                                       (let [nextval (first xs)
                                             remainder (rest xs)]
                                         (if (empty? current)
                                           (recur acc [ nextval ] remainder)
                                           (if (= (inc (first current)) nextval)
                                             (recur acc (cons nextval current) remainder)
                                             (recur (concat acc [ (reverse current)]) [] xs))

                                           )
                                         )))
                maxlen (apply max (map #(count %) seqs))
                longest-seqs (filter #(and (= (count %) maxlen) (> maxlen 1)) seqs)
                ]
            (if (empty? longest-seqs) [] (first longest-seqs))
            )))

(defcheck solution-e4056b92
  (fn [col]
    (let [recursor
          (fn recurs [prev curr col]
            (let
             [
              nextelem (first col), prevelem (last curr),
              biggest (if
                       (> (count curr) (count prev))
                        curr
                        prev
                        )
              ]
              (if
               (empty? col)
                (if (> 2 (count biggest)) [] biggest)
                (if
                 (> nextelem prevelem)
                  (recurs prev (conj curr nextelem) (rest col))
                  (recurs biggest (vector nextelem) (rest col))
                  )
                )
              )
            )
          ]
      (recursor [] (vector (first col)) (rest col))
      )
    ))

(defcheck solution-e4e2d5
  (fn increasing-subseq
    [s]
    (letfn [(insert-between [p v l]
              (if (empty? (rest l))
                l
                (let [[a b] l]
                  (lazy-seq (concat (if (p a b) [a v] [a])
                                    (insert-between p v (rest l)))))))]
      (let [separated-list (insert-between #(<= %2 %1) :mark s)
            sub-seqs (filter #(not= [:mark] %) (partition-by number? separated-list))]
        (if (= (count s) (inc (count (filter #(= :mark %) separated-list))))
          []
          (first (sort-by #(/ 1 (count %)) sub-seqs)))))))

(defcheck solution-e5001ee
  (fn [a]
    (letfn [(next-inc? [n c] (= (inc (nth c n)) (nth c (inc n))))
            (previous-inc? [n c] (= (dec (nth c n)) (nth c (dec n))))
            (next-previous-incs? [n c] (and (next-inc? n c) (previous-inc? n c)))
            (npi [n c] [(previous-inc? n c) (next-inc? n c)])
            (nistart? [c] (if (next-inc? 0 c) [false true] [false false]))
            (niend? [c] (if (previous-inc? (dec (count c)) c) [true false] [false false]))
            (incs? [c]
              (let [cc (count c)
                    mid-incs (map #(npi % c) (range 1 (dec cc)))
                    all-incs  (concat [(nistart? c)] mid-incs [(niend? c)])
                    add-to-last (fn [n col] (concat (butlast col) [(concat (last col) [(nth c n)])]))]
                (loop [res [[]] num 0]
                  (cond
                    (> num (dec cc)) (last (sort-by count res))
                    (= (nth all-incs num) [false false]) (recur res (inc num))
                    (or (= (nth all-incs num) [true false]) (= (nth all-incs num) [true true])) (recur (add-to-last num res) (inc num))
                    (= (nth all-incs num) [false true]) (recur (concat res [[(nth c num)]]) (inc num))))))]
      (incs? a))))

(defcheck solution-e598575e
  (fn [coll]
    (second
      (reduce #(if (and (not (empty? (first %1))) (= %2 (inc (peek (first %1)))))
                 (let [new-potential (conj (first %1) %2)
                       best-so-far (second %1)]
                   (if (> (count new-potential) (count best-so-far))
                     (vector new-potential new-potential)
                     (vector new-potential best-so-far)))
                 (vector [%2] (second %1)))
        [[][]]
        coll))))

(defcheck solution-e5e4d651
  (fn [nums]
    (let [zs (filter #(< 1 (count %))
               ((fn lis [[x & xs]]
                  (let [[a b] (reduce (fn [[ms ns] n]
                                        (if (< (last ns) n)
                                          [ms (conj ns n)]
                                          [(conj ms ns) [n]]))
                                [[] [x]] xs)]
                    (conj a b))) nums))]
      (if (empty? zs)
        []
        (reduce #(if (< (count %1) (count %2)) %2 %1) zs)))))

(defcheck solution-e64b048b
  #(loop[longest [0 0] current [(first %) (first %)] remaining (rest %)]
     (if-let [item (first remaining)]
       (if (= (inc (second current)) item)
         (recur longest (assoc current 1 item) (rest remaining))
         (if (> (- (second current) (first current)) (- (second longest) (first longest)))
           (recur current [item item] (rest remaining))
           (recur longest [item item] (rest remaining))))
       (let [[start end] (if (> (- (second current) (first current)) (- (second longest) (first longest)))
                           current
                           longest)]
         (if (= start end) []
                           (range start (inc end)))))))

(defcheck solution-e66be384
  (fn [s]
    (reverse (apply max-key count (cons [] (remove #(= 1 (count %)) ((fn seq-crescente [[[primeiro-elemento-resp-corrente
                                                                                          & _
                                                                                          :as resp-corrente]
                                                                                         & resto-resp
                                                                                         :as resp]
                                                                                        [primeiro-coll
                                                                                         & resto-coll
                                                                                         :as coll]]
                                                                       (if (empty? coll)
                                                                         resp
                                                                         (seq-crescente
                                                                           (if (= primeiro-elemento-resp-corrente (dec primeiro-coll))
                                                                             (conj resto-resp (conj resp-corrente primeiro-coll))
                                                                             (conj resp (list primeiro-coll)))
                                                                           resto-coll)))
                                                                     ()
                                                                     s)))))))

(defcheck solution-e66c3cc6
  (fn [xs]
    (let [all-increasing-seq
          (reductions #(if (or (empty? %) (< (peek %) %2)) (conj % %2) (vector %2)) [] xs)]
      (->> all-increasing-seq
        (filter #(> (count %) 1))
        (reduce #(if (> (count %2) (count %)) %2 %) [])))))

(defcheck solution-e670c89d
  (fn [coll]
    (letfn [(take-while-increasing [coll]
              (loop [acc [(first coll)]
                     coll (rest coll)]
                (let [f (first coll)]
                  (cond
                    (nil? f) acc
                    (> f (first acc)) (recur (cons f acc) (rest coll))
                    :else (reverse acc)))))]

      (loop [coll coll
             acc []]
        (if (empty? coll)
          acc
          (let [c (take-while-increasing coll)
                c (if (= (count c) (count coll)) coll c)]
            (cond
              (nil? c) acc
              (and (< 1 (count c)) (> (count c) (count acc))) (recur (rest coll) c)
              :else (recur (rest coll) acc))))))))

(defcheck solution-e680ab67
  (fn [coll]
    (letfn [(find-asc-seqs [coll]
              (letfn [(asc-seq [coll]
                        (lazy-seq
                          (when-let [s (seq coll)]
                            (let [x (first s)
                                  y (second s)]
                              (if (and x y (> y x))
                                (cons x (asc-seq (rest s)))
                                (list x))))))]
                (if (empty? (rest coll)) (list (asc-seq coll))
                                         (cons (asc-seq coll) (find-asc-seqs (rest coll))))))]
      (let [seqs (filter #(> (count %) 1) (reverse (find-asc-seqs coll)))]
        (if (seq seqs) (apply max-key count seqs) ())))))

(defcheck solution-e711d225
  (fn [s]
    (:best
     (reduce #(let [c (if (or (empty? (:cur %1))
                              (> %2 (last (:cur %1))))
                        (conj (:cur %1) %2) [%2])]
                (if (and (> (count c) (count (:best %1)))
                         (> (count c) 1))
                  (assoc %1 :cur c :best c)
                  (assoc %1 :cur c)))
       {:cur [] :best []}
       s))))

(defcheck solution-e73b694
  (fn longest-inc-sub [l]
    (let [longest-seq
          (fn [a b]
            (if (> (count a) (count b)) a b))]
      (reverse (loop [l l last_num 0 longest '() working '()]
                 (if (empty? l)
                   (let [f (longest-seq longest working)]
                     (if (> (count f) 1)
                       f
                       '()))
                   (if (= (first l) (inc last_num))
                     (recur (rest l) (first l) longest (cons (first l) working))
                     (let [newlong (longest-seq working longest)]
                       (recur (rest l) (first l) newlong (list (first l)))))))))))

(defcheck solution-e7b1d1f1
  (fn liss [seq]
    (let [res (first (reduce (fn [[longest curr] e]
                               (if (or (empty? curr) (> e (last curr)))
                                 (if (= (count longest) (count curr))
                                   [(conj curr e) (conj curr e)]
                                   [longest (conj curr e)])
                                 [longest [e]]))
                       [[] []] seq))]
      (if (< (count res) 2) [] res))))

(defcheck solution-e86cdbdb
  (fn f [[a & b]]
    (letfn [(better [x y]
              (if (> (count x) (count y)) x y))]
      (loop [cur [a] opt [a] s b]
        (if (empty? s)
          (if (> (count (better cur opt)) 1) (better cur opt) [])
          (if (>= (last cur) (first s))
            (recur [(first s)] (better cur opt) (rest s))
            (recur (conj cur (first s)) opt (rest s))))))))

(defcheck solution-e8854355
  (fn ls
    ([lst] (ls [(first lst)] (rest lst) []))
    ([cur lst best]
     (let [n (first lst)
           r (rest lst)
           c (count cur)
           b (if (and (> c (count best)) (> c 1)) cur best)]
       (cond
         (empty? lst) b
         (= n (inc (last cur))) (ls (conj cur n) r b)
         :else (ls [n] r b)
         )))))

(defcheck solution-e8ffdc6f
  #(letfn [(lcs [x i]
             (conj (map last (take-while (fn [[a b]] (< a b)) (map vector (drop i x) (drop (inc i) x)))) (x i)))]
     (reduce (fn [x y] (if (and (< 1 (count y)) (< (count x) (count y))) y x)) (list) (map (partial lcs %) (range (count %))))))

(defcheck solution-e906f446
  (fn [s]
    (loop [[a b & s] s m [] r []]
      (let [n (conj m a)]
        (letfn [(k [m n r] (if (or (empty? m) (< (count n) (count r))) r n))]
          (if b
            (if (= (inc a) b)
              (recur (cons b s) n r)
              (recur (cons b s) [] (k m n r)))
            (if a
              (k m n r))))))))

(defcheck solution-e985191e
  (comp first (partial reduce
                (fn [[best current] n]
                  (if (empty? current)
                    [best [n]]
                    (if (= n (inc (last current)))
                      (let [current' (conj current n)]
                        (if (> (count current') (count best))
                          [current' current']
                          [best current']))
                      [best [n]])))
                [[], []])))

(defcheck solution-e9d3326e
  (fn [coll] (reduce
               (fn [a b] (let [n (count b)] (if (and (> n 1) (> n (count a))) b a)))
               []
               (reductions (fn [a b] (conj (if (or (empty? a) (> b (last a))) a []) b)) [] coll))))

(defcheck solution-e9f6b44c
  (fn [l] (
            first (
                    reduce
                    (fn [[best cur] [xi xi-1]] (
                                                 let [nextcur (conj cur xi-1)]
                                                 [
                                                  (if (> (count nextcur) (count best))
                                                    (if (>= (count cur) 1) nextcur [])
                                                    best
                                                    )
                                                  (if (<= xi xi-1)
                                                    []
                                                    nextcur
                                                    )
                                                  ]
                                                 ))
                    [[] []]
                    (map list
                      (concat (rest l) [0])
                      l
                      )
                    )
            )))

(defcheck solution-e9ff339
  (fn [nv]
    (loop [pairs (partition 2 1 nv) result ()]
      (if (seq pairs)
        (let [[h t] (split-with #(= (inc (first %)) (second %)) pairs)]
          (if (> (count h) (count result))
            (recur t h)
            (recur (rest t) result)))
        (if (seq result)
          (cons (ffirst result) (map second result))
          result)))))

(defcheck solution-ea0c49a2
  (fn longest-subseq [coll]
    (reverse (last (sort-by count (filter #(> (count %) 1) (reduce (fn [[x & xs :as s] e]
                                                                     (if (= (first x) (dec e)) (cons (cons e x) xs)
                                                                                               (cons (list e) s)))
                                                             [()]
                                                             coll)))))))

(defcheck solution-ea43bbd7
  (fn f [l]
    (letfn [(longest [l i]
              (cond
                (= '() l) '()
                (<= (first l) i) '()
                :else (cons (first l) (longest (rest l) (first l)))))]
      (if (= '() l)
        '()
        (let [here (longest l (dec (first l)))
              later (f (rest l))
              chere (count here)
              clater (count later)]
          (cond
            (< (max chere clater) 2) '()
            (>= chere clater) here
            :else later))))))

(defcheck solution-ea454006
  #(loop [i [] c [] s %]
     ;; i = incumbent c = candidate
     (if (empty? s)
       (cond (and (< (count i) 2) (< (count c) 2)) []
             (> (count i) (count c)) i
             :else c)
       (cond (empty? c) (recur i [(first s)] (rest s))
             (= (first s) (inc (last c))) (recur i (conj c (first s)) (rest s))
             :else
             (recur (if (> (count c) (count i)) c i) [(first s)] (rest s))))))

(defcheck solution-ea907d3a
  (fn f [v]
    (or
     (->>
       (for [s (range (- (count v) 2) ) e (range 2 (inc (count v)))]
         (subvec v s (min (count v) (+ s e))))
       (filter (fn [s] (= s (distinct (sort s)))))
       (sort-by (comp unchecked-negate-int count))
       first)
     [])))

(defcheck solution-eaa7bee0
  (fn [s]
    (letfn [(incr-subseq [xs]
              (loop [r (rest xs) acc [(first xs)]] ; fails on empty seq
                (cond
                  (empty? r) acc
                  (<= (first r) (last acc)) acc ;decreased
                  :else (recur (rest r) (conj acc (first r))))))
            (incr-subseq-at [i xs] (incr-subseq (drop i xs)))
            (better? [xs ys] (and (> (count xs) 1) (> (count xs) (count ys))))
            ]
      (reduce
        (fn [curr poss] (if (better? poss curr) poss curr))
        []
        (map incr-subseq-at (range (count s)) (repeat s))))))

(defcheck solution-eb14d60f
  (fn prob-0053
    [xs]
    (let [
          take-group (fn [f in-xs]
                       ;; TODO Use lazy-seq
                       (let [seq-xs (seq in-xs)
                             x      (first seq-xs)]
                         (if (empty? seq-xs)
                           nil
                           (loop [xs   (rest seq-xs)
                                  lhs  x
                                  rans [x]]
                             (let [rhs (first xs)]
                               (if (or (empty? xs) (not (f lhs rhs)))
                                 (reverse rans)
                                 (recur (rest xs) rhs (cons rhs rans))))))))

          dyad-partition-by (fn [f in-xs]
                              ;; TODO Use lazy-seq
                              (loop [xs   (seq in-xs)
                                     rans []]
                                (let [grp  (take-group f xs)]
                                  (if (empty? grp)
                                    (reverse rans)
                                    (recur (drop (count grp) xs) (cons grp rans))))))

          select-by (fn [f-eval f-select xs]
                      ;; TODO: Could make this more efficient by caching the result of the eval in lhs.
                      (reduce #(if (f-select (f-eval %1) (f-eval %2)) %1 %2) xs))

          longest (fn [xs]
                    (select-by count >= xs))]

      (let [sq (longest (dyad-partition-by < xs))]
        (if (< (count sq) 2)
          []
          sq)))))

(defcheck solution-eba50efd
  (fn [coll]
    (first
      (reduce (fn [[best cand] v]
                (let [cand (if (or
                                (empty? cand)
                                (> v (first cand)))
                             (cons v cand) [v])
                      best (if (and (> (count cand) 1)
                                    (> (count cand) (count best)))
                             (reverse cand)
                             best)]
                  [best cand]))
        [()] coll))))

(defcheck solution-ebd421a2
  (fn [coll]
    (reduce
      (fn [longest si]
        (if (< (count longest) (count si)) si longest))
      []
      (filter
        #(apply < %)
        (mapcat #(partition % 1 coll) (range 2 (inc (count coll)))))
      )
    ))

(defcheck solution-ebe2e478
  #(or
    (first
      (sort-by
        (comp - count)
        (for [f [(fn [[a b]] (= b (+ 1 a)))]
              p (partition-by f (map list % (next %)))
              r [`[~@(map first p) ~(last (last p))]]
              :when (f r)]
          r)))
    []))

(defcheck solution-ebf0d11c
  (fn find-max-seq[xs]
    (loop [ys xs
           highest-acc []
           acc []]
      (if (empty? ys)
        (if (= 1 (count highest-acc))
          []
          highest-acc)
        (let [head (first ys)
              tail (rest ys)
              new-acc (if (empty? acc)
                        (conj acc head )
                        (if (= (+ 1 (last acc))
                              head)
                          (conj acc head )
                          [head]
                          ))
              new-highest-acc (if (= (count new-acc) (+ 1 (count highest-acc)))
                                new-acc
                                highest-acc)]
          (recur tail new-highest-acc new-acc))))))

(defcheck solution-ebf30930
  #(let [s (fn [a b] (if (< (max 1 (count b)) (count a)) a b)) [_ a b] (reduce (fn [[h c b] x] (if (< h x) [x (conj c x) b] [x [x] (s c b)])) [-1 [] []] %)] (s a b)))

(defcheck solution-ec1bc89b
  (fn f1 [c]
    (->> (map vector c (rest c))
      (reduce (fn [[af & ar :as a] b]
                (if (apply < b)
                  (if (empty? af)
                    (conj ar (into af b))
                    (conj ar (conj af (last b))))
                  (conj a [])))
        '([]))
      (apply max-key count))))

(defcheck solution-ed0786f3
  (fn [x]
    (let [res
          (reduce #(if (> (count %2) (count %1)) %2 %1)
            (reduce (fn [[acc k] i]
                      (if (or (empty? k) (= (inc (last k)) i))
                        (vector acc (conj k i))
                        (if (> (count k) (count acc))
                          (vector k (vector i))
                          (vector acc (vector i))
                          )
                        )
                      )
              (vector (vector) (vector (first x)))
              (rest x)
              )
            )
          ]
      (if (> (count res) 1)
        res
        ()
        )
      )
    ))

(defcheck solution-ed2e3dac
  #(vec
     (last
       (filter
         second
         (sort-by
           count
           (reduce (fn [[c & m] v]
                     (if (or (empty? c) (= (peek c) (dec v)))
                       (concat [(conj c v)] m)
                       (concat [[v]] [c] m))) [[]] %))))))

(defcheck solution-ed3754d1
  (fn [coll]
    (let [red (reduce (fn [acc x]
                        (if (= x (+ 1 (last (:curr acc))))
                          (update-in acc [:curr] conj x)
                          (-> acc
                            (update-in [:all] conj (:curr acc))
                            (assoc :curr [x]))))
                {:curr [(first coll)] :all []}
                (rest coll))
          els (conj (:all red) (:curr red))
          grp (group-by count els)
          k (apply max (keys grp))]
      (if (not= k 1)
        (first (get grp k))
        []))))

(defcheck solution-eda099b
  (fn f53 [coll]
    (letfn [(bigger [x y] (if (> (count y) (count x)) y x))]
      (loop [coll coll cur [] max []]
        (if (empty? coll)
          max
          (let [[next & rest] coll
                prev (last cur)]
            (if (and (not (nil? prev))
                     (= next (inc prev)))
              (let [cur (conj cur next)]
                (recur rest cur (bigger max cur)))
              (recur rest [next] max))))))))

(defcheck solution-ede1a74b
  #((reduce
      (fn [state el]
        (let [prev (or (last (:cur state)) -2147483648)
              cur (:cur state)
              cur-seq (conj cur el)
              best (:best state)]
          (assoc state
            :cur
            (if (> el prev)
              cur-seq
              [el])

            :best
            (if (and
                 (>= (count cur-seq) 2)
                 (> el prev)
                 (> (count cur-seq) (count best)))
              cur-seq
              best))))
      {:cur [] :best []} %) :best))

(defcheck solution-ede5b32f
  (fn [ss]
    (let [mf #(if (> (count %1) (count %2)) %1 %2)
          sf #(if (> (count %) 1) % '())]
      (loop [ls '() cs (list (first ss)) [x & xs] (next ss)]
        (cond (nil? x) (reverse (sf (mf ls cs)))
              (= x (inc (first cs))) (recur ls (cons x cs) xs)
              :else (recur (mf cs ls) (list x) xs))))))

(defcheck solution-edfde108
  (fn [l]
    (let [foo (fn [x, l]
                (loop [i x, j l, v []]
                  (cond
                    (and (= [] v) (not (= (inc i) (first j)))) v
                    (= (inc i) (first j)) (recur (first j) (rest j) (conj v i))
                    :else (conj v i)
                    )))]
      (last (sort #(compare (count %1) (count %2))
              (loop [s (first l), r (rest l), v []]
                (if (= '() r) v
                              (recur (first r) (rest r) (conj v (foo s r)))))
              ))
      )
    ))

(defcheck solution-ee8cea51
  (fn [c]
    (let [v-g
          (reduce
            (fn [value-group group] (if (> (count group) (first value-group)) [(count group) group] value-group))
            [0 []]
            (reduce
              (fn [groups pair]
                (if (>= (first pair) (last pair))
                  (conj (vec groups) [])
                  (conj (vec (drop-last groups)) (conj (last groups) pair))))
              [[]]
              (partition 2 1 c)))]
      (if (= 0 (first v-g))
        []
        (if (= 1 (first v-g))
          (first (last v-g))
          (concat (map first (drop-last (last v-g))) (last (last v-g))))))))

(defcheck solution-ee917f30
  (fn [col]
    (let [validate_col (fn [c] (if (< (count c) 2) [] c)) ]
      (loop [s1 [] s2 [(first col)] lastv (first col) data (rest col)]
        (if (empty? data)
          (if (< (count s1) (count s2) )
            (validate_col s2)
            (validate_col s1))
          (let [is-inc (< lastv (first data))]
            (recur (if is-inc
                     s1
                     (if (< (count s1) (count s2))
                       s2
                       s1))
              (if is-inc (conj s2 (first data) ) [(first data)])
              (first data)
              (rest data)))))) ))

(defcheck solution-eeb2d306
  #(letfn [
           (replace-last-ss [state ss] (concat (butlast state) [ss]))

           (start-ss [state elem] (concat state [[elem]]))

           (last-ss [state] (if (empty? state) [] (last state)))

           (ss-growing? [ss elem] (if (empty? ss) true (< (last ss) elem)))

           (growing? [state elem] (ss-growing? (last-ss state) elem))

           (ss-append [ss elem] (conj ss elem))

           (add-to-state [state elem]
             (if (growing? state elem)
               (replace-last-ss state (ss-append (last-ss state) elem))
               (start-ss state elem)
               ))]

     (let [sol (first (sort-by count > (reduce add-to-state [] %)))]
       (if (> 2 (count sol)) [] sol)
       )
     ))

(defcheck solution-ef430e5f
  (fn f [v]
    (if (empty? v) v
                   (let [fis (fn is [v] (vec (take
                                               (let [t (count (take-while #(> 0 %) (map - v (rest v))))]
                                                 (if (> t 0) (+ 1 t) 0))
                                               v)))]
                     (if (>= (count (fis v)) (count (f (rest v))))
                       (fis v) (f (rest v))
                       )
                     ))
    ))

(defcheck solution-ef6a404f
  (fn [coll]
    (let [getIncSub (fn [coll] ;;Function gets increasing subsequence starting from the first element
                      ((fn inner [acc [head & more]]
                         (cond (nil? head) acc
                               (>= (last acc) head) acc
                               :else (inner (conj acc head) more)))
                       [(first coll)] (rest coll)))]
      (->> coll
        count
        dec
        range
        (map #(drop %1 coll))
        (map getIncSub)
        (filter #(>= (count %1) 2))
        (concat [[]])
        (sort-by count >)
        first))))

(defcheck solution-ef7769f6
  (fn [v]
    (let [table (reduce conj {} (reverse (map #(hash-map (count %)  (conj (map last %) (first (first %)))     ) (filter #(and (> (count %) 0) (apply < (first %))) (partition-by #(apply < %)  (partition 2 1 v)) ))))]
      (if (empty? table) [] (table (apply max (keys table))))
      )
    ))

(defcheck solution-efc1906c
  (fn __ [xs]
    (letfn [(f [y & ys]
              (if (empty? ys)
                (list y)
                (cons y
                  (if (< y (first ys))
                    (apply f ys)
                    '()))))]
      (let [r (->> (range (count xs))
                (map #(apply f (drop % xs)))
                (reduce #(if (>= (count %1) (count %2)) %1 %2)))]
        (if (= 1 (count r)) [] r)))))

(defcheck solution-efe07b2b
  (fn [c]
    (let [patterns (remove
                     #(= 1 (count %))
                     (for
                      [i (range 2 (inc (count c)))
                       j (range (inc (count c)))
                       :when (> i j)]
                       (drop j (take i c))))]
      (let [candidates (filter
                         (fn [coll]
                           (not= false (reduce #(if (= false %1) false (if (= (inc %1) %2) %2 false)) coll)))
                         patterns)]
        (if (empty? candidates)
          []
          (apply (partial max-key count) candidates))))))

(defcheck solution-f0609e1f
  (fn longest-subseq [s]
    (let [l (count s)]
      (or (first
            (sort-by count >
              (filter #(every? (partial apply <) (partition 2 1 %))
                (apply concat (map (fn [n] (map #(take % (drop n s))
                                             (range 2 (inc (- l n)))))
                                (range (dec l))))))) []))))

(defcheck solution-f07215d5
  (fn [xs]
    (let [m
          (loop [x (rest xs) cr [(first xs)] mx []]
            (if (empty? x)
              mx
              (let [f (first x)
                    nw (if (> f (last cr)) (conj cr f) [f])]
                (recur (rest x)
                  nw
                  (if (> (count nw) (count mx)) nw mx)))))]
      (if (= 1 (count m)) [] m))))

(defcheck solution-f094ba2b
  (fn [coll]
    (let [{:keys [cs stack]} (reduce (fn [m n]
                                       (if (empty? (:stack m))
                                         (assoc m :stack [n])
                                         (if (= (inc (peek (:stack m))) n)
                                           (update-in m [:stack] conj n)
                                           {:cs (conj (:cs m) (:stack m)) :stack [n]})))
                               {:cs [] :stack []}
                               coll)
          max (->> (conj cs stack)
                (reduce (fn [v n]
                          (if (> (count n) (count v)) n v))
                  []))]
      (if (= 1 (count max)) [] max))))

(defcheck solution-f0da965f
  (fn longest [s]
    (let [monotonic-sequences (partition-by (partial apply <) (partition 2 1 s))
          increasing-sequences (filter #(apply < (first %)) monotonic-sequences)
          max-length (if (empty? increasing-sequences) 0 (apply max (map count increasing-sequences)))
          first-longest (first (filter #(= max-length (count %)) increasing-sequences))]
      (if (seq? first-longest) (cons (first (first first-longest)) (map second first-longest)) '()))))

(defcheck solution-f1d3a688
  (fn [thing]
    (let [[[_ len start] & _] (->> thing
                                (partition 2 1)
                                (map (partial apply -))
                                (partition-by identity)
                                (reduce (fn [arr curr] (conj arr [curr (count curr) (reduce + 0 (map second arr))])) [])
                                (filter #(= (ffirst %) -1))
                                (sort-by second >))]

      (if start
        (subvec thing start (+ start len 1))
        [])
      )))

(defcheck solution-f1e8919d
  ; not proud of this code - take-while looks promising
  (fn [coll]
    (first
      (filter #(not= (count %) 1)
        (sort #(> (count %1)(count %2))
          (reduce (fn [res new]
                    #_(println res new)
                    (conj res (if (= (dec new) (last (last res))) (conj (last res) new) [new])))
            [[]] coll))))))

(defcheck solution-f28fd5e8
  (fn [c]
    (let [s  (loop [[x & o :as c] c l nil s [] r []]
               (if x
                 (if (or (nil? l) (> x l))
                   (recur o x (conj s x) r)
                   (recur c nil [] (cons s r)))
                 (filter #(> (count %) 1) (cons s r))))]
      (if (seq s) (apply max-key count s) []))))

(defcheck solution-f29c401c
  (fn [x]
    (loop [in  x
           out []
           cur []]
      (let [out' (if (> (count cur)
                       (max (count out) 1))
                   cur
                   out)]
        (cond
          (empty? in) out'
          (empty? cur)
          (recur (next in) out [(first in)])
          (> (first in) (last cur))
          (recur (next in) out (conj cur (first in)))
          :else
          (recur in out' []))))))

(defcheck solution-f3113c3c
  (fn [v]
    (let [increase-counter
                     (fn [pairs next]
                       (if (empty? pairs)
                         [[next 1]]
                         (let [[x n] (last pairs)]
                           (conj pairs [next (if (< x next) (inc n) 1)]))))
          lengths (vec (map second (reduce increase-counter [] v)))
          max-length (apply max lengths)
          end-point (inc (loop [i 0] (if (= (get lengths i) max-length) i (recur (inc i)))))]
      (if (> max-length 1)
        (subvec v (- end-point max-length) end-point) []))))

(defcheck solution-f3605066
  (fn [coll]
    (reduce (fn [r c]
              (if (> (count c) (count r))
                c
                r))
      '()
      (filter #(>= (count %1) 2)
        (map (fn [i]
               (loop [r `(~(nth coll i))
                      i (+ i 1)]
                 (cond (>= i (count coll))
                       (reverse r)
                       (= (- (nth coll i) 1) (first r))
                       (recur (conj r (nth coll i)) (+ i 1))
                       :else
                       (reverse r))))
          (range (count coll)))))))

(defcheck solution-f38621dc
  (fn f
    ([src]
     (f src [] []));&#21644;&#19979;&#38754;&#32452;&#25104;&#22810;&#20803;&#20989;&#25968;&#65292;&#30456;&#24403;&#20110;java&#30340;overload
    ([src result temp]
     (let [sizeT (count temp) sizeR (count result) item (first src) others (rest src)]
       (cond (empty? src) (if (> sizeT sizeR 1) temp result);#1 #5
             (empty? temp) (recur others result [item])
             (> item (last temp)) (recur others result (conj temp item))
             (< sizeT 2) (recur others result [item]);#5
             (< sizeR sizeT) (recur others temp [item]);#4
             :else (recur others result [item]))));#4
    ))

(defcheck solution-f412e904
  #(loop [l []
          c (rest %1)
          u [(first %1)]]
     (cond
       (and (empty? c) (or (>= (count l) (count u)) (< (count u) 2))) l
       (and (empty? c) (< (count l) (count u))) u
       (= (first c) (inc (last u))) (recur l (rest c) (conj u (first c)))
       (and (not (= (first c) (inc (last u)))) (> (count u) (count l)) (>= (count u) 2)) (recur u (rest c) [(first c)])
       :else (recur l (rest c) [(first c)]))))

(defcheck solution-f41b7115
  (fn prob53 [s]
    (let
     [p (reduce
          (fn [v i]
            (if (> i (last (last v)))
              (concat (butlast v) (list (conj (last v) i)))
              (concat v (list [i]))))
          (list [(first s)]) (rest s))
      m (apply max (map count p))]
      (if (= m 1) [] (first (filter #(= m (count %)) p))))))

(defcheck solution-f4f92775
  (fn [s]
    (let [result (reduce (fn [a b]
                           (if (> b (or (last (:current a)) 0))
                             {:biggest (:biggest a) :current (conj (:current a) b)}
                             (if (> (count (:current a)) (count (:biggest a)))
                               {:biggest (:current a) :current [b]}
                               {:biggest (:biggest a) :current [b]}
                               )
                             )
                           )
                   {:biggest [] :current []} s)]
      (if (>= (count (:biggest result)) (count (:current result)) 2)
        (:biggest result)
        (if (>= (count (:current result)) (count (:biggest result)) 2)
          (:current result)
          []
          )
        )
      )
    ))

(defcheck solution-f6935de2
  (fn [l]
    (let [f (fn [a i]
              (if (= 1 (- i (last (last a))))
                (concat (butlast a) [(concat (last a) [i])])
                (concat a [[i]]))
              )
          p (reduce f [[(first l)]] (rest l))
          m (apply max (map count p))]
      (into [] (some #(if (and (< 1 m) (= m (count %))) %) p))
      )))

(defcheck solution-f6a24f9a
  (fn [ms] (first
             (reduce
               (fn [f s]
                 (cond
                   (empty? (second f))                         [(first f) [s]]
                   (>= (last (second f)) s)                    [(first f) [s]]
                   (>= (count (second f)) (count (first f)))   [(conj (second f) s) (conj (second f) s)]
                   true    [(first f) (conj (second f) s)]

                   ))

               [[] []] ms))))

(defcheck solution-f6bcca04
  (fn [coll]
    (->> (reduce (fn [[longest curr] e]
                   (if (= e ((fnil inc e) (last curr)))
                     (let [curr (conj curr e)]
                       (if (< (count longest) (count curr))
                         [curr curr]
                         [longest curr]))
                     [longest [e]]))
           [[] []]
           coll)
      first)))

(defcheck solution-f6de7de
  (fn foo [xs]
    (let [idxs (->>
                 (map - (rest xs) xs)
                 (map-indexed vector)
                 (partition-by (comp pos? second))
                 (filter (comp pos? second first))
                 reverse
                 not-empty)
          idxs (when idxs
                 (->> idxs
                   (apply max-key count)
                   (map first)))
          idxs (when idxs (concat idxs [(inc (last idxs))]))]
      (map #(nth xs %) idxs))))

(defcheck solution-f6f23b36
  (fn [xs]
    (let [s (butlast xs)
          t (rest xs)
          st (map #(vector %1 (- %2 %1)) s t)
          p (partition-by second st)
          f (filter #(= 1 (second (first %))) p)]
      (if (empty? f)
        []
        (let [a (last (sort-by count f))
              b (map first a)]
          (concat b [(inc (last b))]))
        ))))

(defcheck solution-f701d6e
  #(let [[e]
         (reduce
           (fn [v i]
             (let [[f [l :as c]] v
                   n (cons i c)]
               (if (or (empty? c) (> i l))
                 (if (> (count n) (count f))
                   [n n]
                   [f n])
                 [f (list i)])))
           [() ()]
           %)]
     (if (>= (count e) 2)
       (reverse e)
       [])))

(defcheck solution-f7116a7a
  (let [longest
        (fn [leftover best current]
          (if (nil? leftover)
            (if (>= (count best) 2) best [])
            (if (> (first leftover) (last current))
              (if (> (count (conj current (first leftover))) (count best))
                (recur (next leftover) (conj current (first leftover)) (conj current (first leftover)))
                (recur (next leftover) best (conj current (first leftover)))
                )
              (recur (next leftover) best [(first leftover)])
              )
            )
          )] (fn [z] (longest (next z) [(first z)] [(first z)]))
             ))

(defcheck solution-f75d1d6d
  (letfn [(max-count [a b]
            (if (> (count a) (count b)) a b))
          (min-two [xs]
            (> (count xs) 1))
          (accum-seq [a x]
            (if (= (or (last (last a)) x) (dec x))
              (conj (vec (drop-last a))
                (conj (last a) x))
              (conj a [x])))]
    (fn [xs]
      (reduce max-count []
        (filter min-two
          (reduce accum-seq [] xs))))))

(defcheck solution-f8e6a80
  (letfn [(f [c] (reductions #(conj % %2) [] (reverse c)))]
    (fn [y] (last (sort (remove #(= 1 (count %)) (map
                                                   (fn [x]  (last (filter #(or (empty? %) (apply < %)) (f x))))
                                                   (f y))))))))

(defcheck solution-f8f605df
  (fn longest-increasing [s]
    (let [break-down (fn [s prev-element current-seq accum]
                       (if s
                         (let [[current-element & rest-elements] s]
                           (if (and prev-element (= current-element (inc prev-element)))
                             (recur rest-elements current-element (conj current-seq current-element) accum)
                             (if (empty? current-seq)
                               (recur rest-elements current-element [current-element] accum)
                               (recur rest-elements current-element [current-element] (conj accum current-seq)))))
                         (if (empty? current-seq)
                           accum
                           (conj accum current-seq))))]
      (->> (break-down s nil [] [])
        (sort-by count)
        (filter #(< 1 (count %)))
        (#(if (empty? %) [] (last %)))
        ))))

(defcheck solution-f911a908
  (fn lcss [coll]
    (when-let [coll1 coll]
      (when-let [res (last (reduce #(if (> (first %2) (first %1)) %2 %1)
                             (map #(vector (count %) %)
                               (loop [coll (rest coll1)
                                      curr [(first coll1)]
                                      acc []
                                      ]
                                 (if (empty? coll)
                                   (conj acc curr)
                                   (if (< (last curr) (first coll))
                                     (recur (rest coll) (conj curr (first coll)) acc)
                                     (recur (rest coll) [(first coll)] (conj acc curr))
                                     )
                                   )
                                 )
                               )))]
        (if (> (count res) 1)
          res
          [])
        )
      )))

(defcheck solution-f92b2ca0
  #(->>
     %
     (partition 2 1)
     (partition-by
       (fn [x] (= 1 (- (second x) (first x)))))
     (filter
       (fn [x] (= 1 (- (second (first x)) (first (first x))))))
     (sort-by
       (fn [x] (- (count x))))
     first
     (reduce concat)
     distinct
     (into [])))

(defcheck solution-f93660aa
  (fn [s]
    (let [x (->>
              (range (count s) 1 -1)
              (mapcat #(partition % 1 s))
              (filter #(apply < %))
              reverse
              )
          ] (if (seq x) (apply max-key count x) '()))))

(defcheck solution-f93e09fe
  (fn [coll]
    (let [data (filter #(= 2 (count %))(partition-all 2 1 coll))
          is_inc (fn [[a b]] (= 1 (- b a)))]
      (loop [r [] d (drop-while #(not (is_inc %)) data)]
        (if (empty? d) (if (empty? r) [](let [r_list (apply concat r) start (first r_list) end (last r_list)] (range start (inc end)))
                                      )      (let [inc_seq (take-while #(is_inc %) d)]
                                               (recur (if (> (count inc_seq) (count r)) inc_seq r)  (drop-while #(not (is_inc %)) (drop-while #(is_inc %) d)) )
                                               )
                       )
        )
      )
    ))

(defcheck solution-f95df3b0
  (fn get-subseq
    [numbers]
    (loop [nums numbers active [] current []]
      (if (empty? nums) current
                        (if (= (count nums) 1)
                          (if (and (> (count active) 0) (>= (count active) (count current))) (conj active (first nums)) current)
                          (let [[a b & others] nums]
                            (if (< a b)
                              (recur (cons b others) (conj active a) current)
                              (if (not-empty active)
                                (recur (cons b others) [] (if (>= (count active) (count current)) (conj active a) current))
                                (recur (cons b others) [] current)))))))))

(defcheck solution-f99ef3d
  (fn f [s]
    (let [conj-to-last (fn [v x]
                         (let [lastidx (dec (count v))]
                           (update-in v [lastidx] #(conj % x))))
          inc-seqs (filter #(> (count %) 1)
                     (loop [seqs []
                            s s]
                       (if-not (seq s)
                         seqs
                         (let [a (first s)]
                           (recur (if (= (dec a) (last (last seqs)))
                                    (conj-to-last seqs a)
                                    (conj seqs [a]))
                             (rest s))))))]
      (if (seq inc-seqs)
        (apply max-key count inc-seqs)
        []))))

(defcheck solution-fa30e341
  (fn longest-inc-seq [s]
    (letfn [(increasing? [s] (reduce (fn [x y] (if (and x (< x y)) y false)) -1 s))]
      (let [seqs (reverse (filter increasing? (mapcat #(partition % 1 s) (range 2 (inc (count s))))))]
        (if (empty? seqs)
          []
          (apply max-key count seqs))))))

(defcheck solution-fa5183ed
  (fn
    longest-inc-seq
    [lst]
    (let [longest-inc-seq'' (fn longest-inc-seq'
                              [prevele seq1 cntvec]
                              (cond
                                (empty? seq1) cntvec
                                (empty? cntvec) (longest-inc-seq' (first seq1) (rest seq1) (conj cntvec 0))
                                (< prevele (first seq1)) (longest-inc-seq' (first seq1) (rest seq1)
                                                           (conj cntvec (inc (last cntvec))))
                                :else (longest-inc-seq' (first seq1) (rest seq1) (conj cntvec 0))))
          longest-inc-seq-cnt-vec (longest-inc-seq'' nil lst [])
          longest-seq-len (apply max longest-inc-seq-cnt-vec)
          posn-max-ele (.indexOf longest-inc-seq-cnt-vec longest-seq-len)]
      (if (< longest-seq-len 1)
        []
        (drop (- posn-max-ele longest-seq-len) (take (inc posn-max-ele) lst))))))

(defcheck solution-fa6b0ef7
  (fn [s]
    (letfn [(split-increasing [acc [s0 s1 :as s]]
              (if (empty? s)
                nil
                (let [acc' (conj acc s0) rs (rest s)]
                  (if (and s1 (< s0 s1))
                    (recur acc' rs)
                    (cons acc' (split-increasing [] rs))))))]
      (let [ss (filter #(> (count %) 1) (split-increasing [] s))]
        (if (empty? ss)
          []
          (let [len (apply max (map count ss))]
            (first (filter #(= (count %) len) ss))))))))

(defcheck solution-faf0302e
  (fn [coll]
    (let [geti (fn [z]
                 (loop [result []
                        progress []
                        elts z]
                   (cond
                     (empty? elts) (conj result progress)
                     (or (empty? progress) (< (last progress) (first elts)))
                     (recur result (conj progress (first elts)) (rest elts))
                     :else
                     (recur (conj result progress) [(first elts)] (rest elts)))))
          ls (filter #(< 1 (count %)) (geti coll))
          _ (map count ls)
          m (if (not (empty? _)) (apply max _) 0)]
      (if (= 0 m) []
                  (first (filter #(= m (count %)) ls))))))

(defcheck solution-fafa42b3
  (fn [xs]
    (->> xs
      (iterate rest)
      (take-while seq)
      (map (fn [subseq]
             (->> subseq
               (partition 2 1)
               (take-while (fn [[a b]] (> b a)))
               (map second)
               (cons (first subseq)))))
      (filter #(> (count %) 1))
      (sort-by #(* -1 (count %)))
      (first)
      (vec))))

(defcheck solution-fb711762
  (fn longest-sub-seq [col]
    "get the longest sub sequence"
    (let [
          longer (fn [a b]
                   (>= (count a) (count b)))
          lss (fn [col p1 p2 ll]
                (if (or (empty? col) (>= p2 (count col)) (> p1 p2))
                  ll
                  (let [
                        cur (subvec col p1 (inc p2))
                        is-right-end (or (= p2 0) (= p1 p2) (= (inc (nth col (dec p2))) (nth col p2)))
                        is-right-start (or (= (count cur) 1) (= (inc (first cur)) (nth cur 1)))
                        is-right (and is-right-end is-right-start)
                        is-go-on (and (< (inc p2) (count col)) (= (inc (nth col p2)) (nth col (inc p2))))
                        ]
                    #_(println p1 p2 col cur ll is-go-on is-right)
                    (if (and is-right (> (count cur) (count ll)))
                      (recur col p1 (inc p2) cur)
                      (if is-go-on
                        (recur col (inc p1) (inc p2) ll)
                        (recur col (inc p1) p2 ll)
                        )
                      )
                    ))
                )
          res (lss col 0 0 nil)
          ]
      (if (< (count res) 2)
        []
        res)
      )
    ))

(defcheck solution-fc012931
  (fn
    [x]
    (let [[chains last-chain] (reduce (fn [[chains current-chain previous-value] v]
                                        (if previous-value
                                          (if (= (- v previous-value) 1)
                                            [chains (conj current-chain v) v]
                                            [(conj chains current-chain) [v] v])
                                          [chains [v] v]))
                                [[] [] nil]
                                x)]
      (or (last (sort-by count (remove #(< (count %) 2) (conj chains last-chain)))) []))))

(defcheck solution-fc476bc9
  (fn [s] (let [sf (filter #(apply < %) (let [c (count s)]
                                          (for [b (range 0 (dec c))
                                                e (range c (+ b 1) -1)] (subvec s b e))))]
            (if (seq sf)
              (first (filter #(= (apply max (map count sf)) (count %)) sf))
              []))))

(defcheck solution-fca7623d
  (fn [x]
    (let [sub-seq  (partition-by identity (map (partial > 0) (map - x (drop 1 x))))
          trues    (map #((comp count filter) true? %) sub-seq)
          alls     (map count sub-seq)
          ind-true (map-indexed vector trues)
          maximum  (apply max trues)]
      (if (= 0 maximum)
        []
        (take (inc maximum) (drop (reduce + (take (first (some #(when ((comp (fn [x] (= maximum x)) second) %) %) (map-indexed vector trues))) alls)) x))))))

(defcheck solution-fd8c896d
  (fn [c]
    (letfn [(sub-seqs [col cur a]
              (if (empty? col)
                (if (empty? cur)
                  (reverse (map reverse a))
                  (reverse (map reverse (cons cur a))))
                ;; not empty so lets see if this
                (let [nxt (first col)
                      rst (rest col)]
                  (if (empty? cur)
                    ;; start a new sequence
                    (sub-seqs rst [nxt] a)
                    (if (= (dec nxt) (first cur))
                      ;; we have a sequence people
                      (sub-seqs rst (cons nxt cur) a)
                      ;; we don't have a sequence.
                      (sub-seqs rst [nxt] (cons cur a))
                      )
                    ))
                ))]
      (let [xs (sub-seqs c [] [])]

        (into [] (first (reverse (sort-by count (filter #(> (count %) 1) xs)))))))))

(defcheck solution-fdaa048c
  #_ ;; alternate take, but I like the reduced based slightly better
      (fn longest-increasing [l]
        (letfn [(tails [s] (take-while seq (iterate rest (concat s [nil]))))
                (adj [[a b]] (= (inc a) b))
                (these-and-1 [[a b]] (concat a (take 1 b)))
                (take-run [nums]
                  (->> nums
                    (partition 2 1)
                    (split-with adj)
                    (these-and-1)
                    (map first)))
                (lonely [l] (= 1 (count l)))]
          (apply max-key count
            (->> (tails l)
              (map take-run)
              (cons [])
              (remove lonely)))))
  #(first
     (reduce (fn [[longest current] el]
               (if (= el (inc (peek current)))
                 (let [current' (conj current el)
                       longest' (max-key count current' longest)]
                   [longest' current'])
                 [longest [el]]))
       [[] [(first %)]]
       (rest %))))

(defcheck solution-fe2c9625
  (fn lseq [in-vals]
    (letfn
     [(better-seq [seq1 seq2]
        (if (> (count seq2) (count seq1)) seq2 seq1))
      (lseq2 [vals]
        (loop [current-max []
               current-seq []
               items vals]
          (if-not (seq items)
            (better-seq current-max current-seq)
            (if (every? #(> (first items) %) current-seq)
              (recur current-max (conj current-seq (first items)) (rest items))
              (recur (better-seq current-max current-seq) [(first items)] (rest items))))))]
      (let [result (lseq2 in-vals)]
        (if (> (count result) 1)
          result
          [])))))

(defcheck solution-fe617e6e
  (partial (fn [c l r]
             (if (empty? r)
               (if (> (count l) 1) l [])
               (let [n (first r)
                     s (when-not (empty? c) (> n (last c)))
                     nc (if s (concat c [n]) [n])
                     nl (max-key count nc l)]

                 (recur nc nl (rest r))))) [] []))

(defcheck solution-fe7df31
  (fn find-greater-consecs
    ([[head & tail]] (find-greater-consecs tail [[head]]))
    ([[head & tail] xs-of-xs]
     (letfn [(greater-coll [xs ys]
               (if (< (count xs) (count ys)) ys xs))
             (find-first-greatest-coll
               [xs-of-xs]
               (reduce greater-coll [] xs-of-xs))
             (apply-min-length-rules [xs-of-xs]
               (if (> (count xs-of-xs) 1) xs-of-xs []))]
       (if (nil? head)
         (-> (find-first-greatest-coll xs-of-xs)
           apply-min-length-rules)
         (if (= (inc (last (last xs-of-xs))) head)
           (recur tail (conj xs-of-xs (conj (last xs-of-xs) head)))
           (recur tail (conj xs-of-xs [head]))))))))

(defcheck solution-fe8dfefa
  (fn
    [coll]
    (loop [result [] current [] coll coll]
      (if (empty? coll)
        result
        (let [[elem & remaining] coll]
          (if (or (empty? current) (> elem (last current)))
            (let [current (conj current elem)]
              (recur (if (and (> (count current) 1) (> (count current) (count result))) current result) current remaining)
              )
            (recur result [elem] remaining)
            )
          )
        )
      )))

(defcheck solution-ff059879
  (fn [x]
    (let [paired (map list x (rest x))
          pred (fn [[a b]] (< a b))
          split (partition-by pred paired)
          increasing (filter (fn [a] (pred (first a))) split)
          ]
      (if (not (seq increasing))
        []
        (let [longest-len (apply max (map count increasing))
              longest-seqs (filter #(= longest-len (count %)) increasing)
              first-seq (first longest-seqs)]
          (concat (map first first-seq) [(last (last first-seq))])
          )
        )
      )
    ))

(defcheck solution-ff24887e
  (fn [s]
    (let [split-increasing-subseqs
          (fn [s]
            (reduce #(if (< (last (last %)) %2)
                       (conj (vec (butlast %)) (conj (vec (last %)) %2))
                       (conj (vec %) [%2]))
              [[(first s)]]
              (rest s)))]
      (let [ans (->> (split-increasing-subseqs s)
                  (filter #(> (count %) 1))
                  sort
                  last)]
        (or ans [])))))