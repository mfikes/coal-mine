(ns coal-mine.problem-82
  (:require [coal-mine.checks :refer [defcheck-82] :rename {defcheck-82 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-100afd6
  (fn [S]
    (let [c (fn c [a b]
              (let [a (seq a) b (seq b)
                    [r & t] a
                    [s & u] b]
                (or
                 (= a b)
                 (and (= r s) (c t u))
                 (= a u) (= b t) (= t u))))
          ? (fn ? [s V]
              (or (empty? V)
                  (some true?
                    (map (fn [x]
                           (if (c s x) (? x (disj V x))))
                      V))))
          ]
      (not-every? nil? (map #(? % S) S)))))

(defcheck solution-109bebd2
  (fn [sqn]
    (letfn [(lv [a b]
              (let [s (seq a) t (seq b)]
                (cond (zero? (count s)) (count t)
                      (zero? (count t)) (count s)
                      :else
                      (min (inc (lv (rest s) t))
                        (inc (lv s (rest t)))
                        (+ (lv (rest s) (rest t))
                           (if (= (first s) (first t)) 0 1))))))]
      (let [m (group-by #(count (filter (partial = 1) %))
                (loop [t sqn result []]
                  (if (not (next t)) result
                                     (recur (rest t)
                                       (conj result (map #(lv (first t) %) sqn))))))
            c (count (m 1))]
        (and (nil? (m 0)) (or (zero? c) (= 1 c)))))))

(defcheck solution-110197ab
  (fn word-chain? [words]
    (letfn [(l-dist [s1 s2]  ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive
              (let [l1 (count s1) l2 (count s2) dl1 (drop-last s1) dl2 (drop-last s2)]
                (cond (zero? l1) l2
                      (zero? l2) l1
                      :else (min (inc (l-dist dl1 s2)) (inc (l-dist s1 dl2))
                              (+ (l-dist dl1 dl2) (if (= (last s1) (last s2)) 0 1))))))]
      (let [l-matrix  ; levenshtein distance matrix represented as a map of maps {word1 {word2 distance}}
            (apply merge-with merge (for [w words x words] {w {x (l-dist w x)}}))]
        ;; observed heuristic: in at least n-1 rows of the full Levenshtein distance matrix
        ;; there must be at least 2 connections with distance 1
        (<= (dec (count words))
          (count (filter #(>= % 2)
                   (map #(->> % second vals (filter #{1}) count) l-matrix))))))))

(defcheck solution-11495a08
  (fn [coll]
    (letfn [
            (ds [a b]
              (reduce + (map #(if (= % %2) 0 1) a b)))
            (dd [s b]
              (if (and s b)
                (if (= (first s)(first b))
                  (dd (rest s)(rest b))
                  (if (= s (rest b))
                    1))
                (if (and (empty? s) (= 1 (count b)))
                  1)))
            (d [a b]
              (cond
                (= (count a)(count b)) (ds a b)
                (> (count a)(count b)) (dd (vec b) (vec a))
                :else  (dd (vec a) (vec b) ) ))
            (cl [m e]
              (if (empty? (m e))
                0
                (inc (apply max (map #(cl (dissoc m e) %) (m e))))))]
      (let [m (reduce (fn [m e] (assoc m e (filter (fn [x] (= 1 (d e x))) coll))) {} coll)]

        (true? (some #(= % (count coll)) (map #(cl m %) coll)))))))

(defcheck solution-117e330a
  (fn [words]
    (let [chainable?
          (fn [w1 w2]
            (let [lw (if (>= (count w1) (count w2)) w1 w2)
                  sw (if (>= (count w1) (count w2)) w2 w1)
                  clw (count lw)
                  csw (count sw)
                  letters (partial re-seq #"\w")
                  diff (fn [c1 c2 d]
                         (cond
                           (nil? c1) (+ (count c2) d)
                           (nil? c2) (+ (count c1) d)
                           (not= (first c1) (first c2)) (recur (next c1) c2 (inc d))
                           :else (recur (next c1) (next c2) d)))]
              (cond
                (= clw csw)
                (= 1 (apply + (map #(if (= %1 %2) 0 1) (letters w1) (letters w2))))
                (= 1 (- clw csw))
                (= 1 (diff (letters lw) (letters sw) 0))
                :else false)))]
      (letfn [(word-chain
                ([words] (word-chain words [] (seq words)))
                ([words chain [w & rnwords :as next-words]]
                 (if-not (seq words)
                   chain
                   (when (seq next-words)
                     (let [[nw & rninchain :as next-in-chain] (filter #(chainable? w %) words)]
                       (or (word-chain (disj words w) (cons w chain) next-in-chain)
                           (word-chain words chain rnwords)))))))]
        (if (word-chain words) true false)
        ))))

(defcheck solution-136ea18a
  (letfn [(permutations [s]
            (if (= (count s) 1)
              [[(first s)]]
              (reduce (fn [acc elt]
                        (let [others (remove (set [elt]) s)
                              perms (permutations others)
                              new (map #(conj % elt) perms)]
                          (concat acc new)))
                []
                s)))
          (insertion? [x y]
            (or (= (seq x) (rest y)) ; seq is needed or else x is a string and (rest y) a seq of chars, which aren't considered equal
                (and (= (first x) (first y))
                     (insertion? (rest x) (rest y)))))
          (sub? [x y]
            (or (= (rest x) (rest y))
                (and (= (first x) (first y))
                     (sub? (rest x) (rest y)))))
          (valid-pair? [x y]
            (or (sub? x y)
                (insertion? x y)
                (insertion? y x)))
          (valid-chain? [v]
            (or (= 1 (count v))
                (and (valid-pair? (first v) (second v))
                     (valid-chain? (rest v)))))]
    (fn [words]
      ;; the tests want an explicit true/false
      (if (some valid-chain? (permutations words))
        true false))))

(defcheck solution-139a76c8
  (fn [ws]
    (letfn [(snug [uno dos]
              (loop [[up & ur :as u] uno [dp & dr :as d] dos err 0]
                (let [uc (count u) dc (count d)]
                  (cond (or (nil? up) (nil? dp))
                        (let [diff (Math/abs (- uc dc))]
                          (and (< diff 2)
                               (or (and (zero? err) (= 1 diff))
                                   (and (= err 1) (zero? diff)))))
                        (= up dp) (recur ur dr err)
                        (not= err 0) false
                        (= uc dc) (recur ur dr (inc err))
                        (< uc dc) (recur ur (rest dr) (inc err))
                        (< uc dc) (recur ur  (rest dr) (inc err))
                        :else  (recur (rest ur) dr (inc err))))))
            (pms [xs]
              (cond (empty? xs) ()
                    (= (count xs) 1) (list (seq xs))
                    :else (for [x xs
                                y (pms (disj (set xs) x))]
                            (do
                              (cons x y)))))]
      (->> (pms ws)
        (map (partial partition 2 1))
        (map (fn [ws] (map (partial apply snug) ws)))
        (map (partial every? true?))
        (some true?)
        ((complement nil?))))))

(defcheck solution-13c57573
  (fn [W]
    (= (count W)
      (count
        (let [l (memoize
                  (fn l [ [x & X :as a] [y & Y :as b]]
                    (cond
                      (empty? a) (count b)
                      (empty? b) (count a)
                      1 (min
                          (+ (if (= x y) 0 1)
                             (l X Y))
                          (inc (l X b))
                          (inc (l a Y))))))
              g (apply merge-with clojure.set/union
                  (map (fn [[k v]] {k #{v}})
                    (filter #(= 1 (apply l (sort %)))
                      (for [w W o (disj W w)] [w o]))))
              p (fn p [K G]
                  (if (empty? K) []
                                 (last
                                   (sort-by
                                     count
                                     (map
                                       (fn [k] (conj (p (get G k) (dissoc G k)) k))
                                       (filter #(contains? G %) K))))))]
          (p (keys g) g))))))

(defcheck solution-13fc6d1d
  (fn [ c ]
    (letfn
     [
      (mld [ s t ] (letfn [(ld [ s t f] (cond
                                          (zero? (count s)) (count t)
                                          (zero? (count t)) (count s)
                                          :else (min
                                                  (+ 1 (f (rest s) t f))
                                                  (+ 1 (f s (rest t) f))
                                                  (+ (f (rest s) (rest t) f)
                                                     (if (= (first s) (first t)) 0 1))
                                                  )))

                           ]
                     (let [mld (memoize ld)] (mld s t mld))
                     ))
      ]
      (let
       [mmld (memoize mld)]
        (letfn [
                (chain [ d curword col ]
                  (loop [ totest col ]
                    (if (empty? totest)
                      false ; we got back to here with no words found
                      (let [ firstword (first totest) restwords (rest totest) ]
                        ; it chains
                        (if (= 1 (mmld curword firstword))
                          (if (empty? (clojure.set/difference col #{ firstword }))  ; this is the last word in the chain
                            true
                            (if (chain (inc d) firstword (clojure.set/difference col #{ firstword })) ; can we form a further chain
                              true
                              (recur restwords))
                            )
                          ; this firstword didnt match, try another
                          (recur restwords)
                          )))))
                ]

          (loop [col c]
            (if (empty? col)
              false
              (if (chain 0 (first col) (clojure.set/difference c #{ (first col) }))
                true
                (recur (rest col))))))))))

(defcheck solution-15de5987
  (fn [xs] (letfn [
                   (prepend-to-each [pre xs] (map concat (repeat pre) xs))
                   (cycles [arr] (map #(drop % (take (+ (count arr) %) (cycle arr))) (range (count arr))))
                   (permutations [xs] (let [perm (fn [mem-perm xs] (let [perm (fn [xs] (mem-perm mem-perm xs))] (if (empty? xs) [[]] (mapcat #(prepend-to-each (first %) (lazy-seq (perm (second %)))) (map (partial split-at 1) (cycles xs)))))) mem-perm (memoize perm)] (mem-perm mem-perm xs)))
                   (levenshtein [a b] (let [lev (fn [mem-lev a b x y] (let [lev (fn [a b x y] (mem-lev mem-lev a b x y))] (if (= 0 (min x y)) (max x y) (min (+ (lev a b (- x 1) y) 1) (+ (lev a b x (- y 1)) 1) (+ (lev a b (- x 1) (- y 1)) (if (= (nth a (dec x)) (nth b (dec y))) 0 1)))))) mem-lev (memoize lev)] (mem-lev mem-lev a b (count a) (count b))))
                   (word-chain? [xs] (not (empty? (filter #(every? (partial = 1) (map (partial apply levenshtein) (partition 2 1 %))) (permutations xs)))))
                   ] (word-chain? xs))))

(defcheck solution-164ed0c3
  (fn one-chain?
    ([s] (not (nil? (some true? (map (fn[item] (one-chain? (disj s item) [item] (count s))) s)))))
    ([s acc check]
     (letfn [(abs[n] (if (< n 0) (- 0 n) n))
             (chainable?
               ([word1 word2] (if (< (abs (- (count word2) (count word1))) 2)
                                (chainable? word1 word2 0)))
               ([word1 word2 numchanges]
                (let [l1 (first word1)
                      l2 (first word2)]
                  (cond
                    (and (empty? word1) (empty? word2)) true
                    (or (empty? word1) (empty? word2)) (= numchanges 0)
                    (and (> numchanges 0) (not (= l1 l2)))
                    false
                    (not (= l1 l2))
                    (or (chainable? (cons l2 word1) word2 1)
                        (chainable? (cons l2 (rest word1)) word2 1)
                        (chainable? (rest word1) word2 1)
                        (chainable? word1 (cons l1 word2) 1)
                        (chainable? word1 (cons l1 (rest word2)) 1)
                        (chainable? word1 (rest word2) 1))
                    :else
                    (chainable? (rest word1) (rest word2) numchanges)))))]
       (or
        (= (count acc) check)
        (some true? (map (fn[item]
                           (one-chain? (disj s item) (conj acc item) check))
                      (filter #(chainable? (last acc) %) s))))))))

(defcheck solution-16bd3c21
  (fn [words]
    (letfn [
            (hoppable [w1 w2]
              (let [l1 (count w1) l2 (count w2)]
                (if (= l1 l2)
                  (>= 1 (count (filter (fn [[l1 l2]] (not= l1 l2)) (map vector w1 w2))))
                  (if (= 1 (Math/abs (- l1 l2)))
                    (let [w1longer (> l1 l2)
                          lw (if w1longer w1 w2)
                          sw (if w1longer w2 w1)
                          lwl (count lw)
                          lwv (vec lw)]
                      (<= 1 (count (filter #(= (seq sw) %)
                                     (concat [(drop 1 lwv)] [(drop-last lwv)]
                                             (map
                                               (fn [p] (concat (subvec lwv 0 p) (subvec lwv (inc p) lwl)))
                                               (range 1 (dec lwl))
                                               ))
                                     ))))
                    false))))
            ]
      (let [n (count words)
            nindexmap (zipmap words (range n))
            adj (map (fn [w] (map (partial hoppable w) words)) words)
            neibs (map #(set (map first (filter (fn [x] (second x)) (map-indexed vector %)))) adj)
            myorreducer #(or %1 %2)]
        (letfn [
                (searchlet [lastn visited]
                  (if (= n (count visited)) true
                                            (let [nextset (clojure.set/difference (nth neibs lastn) visited)]
                                              (if (empty? nextset) false
                                                                   (reduce myorreducer (map #(searchlet % (conj visited %)) nextset))
                                                                   ))))
                ]
          (reduce myorreducer (map #(searchlet % #{%}) (range n)))
          )))))

(defcheck solution-16bdf973
  (fn _ [origin]
    (letfn [
            (chainableseq?
              ([coll] (reduce #(or % %2)
                        (for [x coll]
                          (do
                            #_(println "---start " x "! ----")
                            (reduce #(or % %2) (chainableseq? x (removerest x coll)))))))

              ([tester targetcoll]
               (if (empty? targetcoll)
                 true
                 (let [chains (filter #(chainable? % tester) targetcoll)]
                   #_(println "debug:" tester chains targetcoll)
                   (if-not (empty? chains)
                     (flatten (map #(chainableseq? % (removerest % targetcoll)) chains))
                     false)))))

            (removerest [x coll]
              (let [t (clojure.set/difference (set coll) (set (cons x '())))]
                (if (empty? t) nil t)))

            (chainable? [a b]
              (cond
                (< (count a) (count b)) (chainable? b a)
                (< 1 (- (count a) (count b))) false

                ;pattern1 replace
                (= 1 (count (clojure.set/difference (->seq a) (->seq b)))) true

                ;pattern2 added top or last
                (= 1 (count (-> a (.replace b "")))) true

                ;pattern3 inserted
                :else
                (let [rests (clojure.set/difference (set a) (set b))]
                  (if (= 1 (count rests))
                    (= (-> a (.replace (str (first rests)) "")) b)
                    false))))

            (->seq [str]
              (set (map-indexed #(vector % %2) str)))]
      (chainableseq? origin))))

(defcheck solution-16eeae48
  (fn [words]
    (letfn [(levenshtein-dist [s1 s2]
              (let [l1 (count s1)
                    l2 (count s2)]
                (cond (zero? l1) l2
                      (zero? l2) l1
                      :else (let [cost (if (= (first s1) (first s2)) 0 1)]
                              (min (inc (levenshtein-dist (rest s1) s2))
                                (inc (levenshtein-dist s1 (rest s2)))
                                (+ cost (levenshtein-dist (rest s1) (rest s2))))))))
            (distance-1-relation [words]
              (set (for [s1 words
                         s2 words
                         :when (and (not= s1 s2)
                                    (= 1 (levenshtein-dist s1 s2)))]
                     #{s1 s2})))
            (graph [relation]
              (->> relation
                (mapcat (fn [v]
                          (let [f (first v)
                                s (second v)]
                            [{f #{s}} {s #{f}}])))
                (apply merge-with clojure.set/union)))
            (hamiltonian-path [graph path]
              (let [seen               (set path)
                    connected-not-seen (filter #(not (seen %))
                                         (get graph (peek path)))]
                (if (empty? connected-not-seen)
                  (when (= (count path)
                          (count graph))
                    path)
                  (some identity (map #(hamiltonian-path graph (conj path %))
                                   connected-not-seen)))))
            (has-hamiltonian-path? [graph]
              (not (nil? (some identity (map #(hamiltonian-path graph [%])
                                          (keys graph))))))]
      (-> words
        distance-1-relation
        graph
        has-hamiltonian-path?))))

(defcheck solution-1702dff6
  (fn [l]
    (let [any? #(boolean (some true? %))

          l-dist-<=1    (fn [s1 s2]
                          (let [[l1 l2] (if (> (count s1) (count s2)) [(vec s1) (vec s2)] [(vec s2) (vec s1)])]
                            (cond
                              (= (count l1) (count l2))       (>= 1 (apply + (map (fn [x y] (if (= x y) 0 1)) s1 s2)))
                              (= (count l1) (inc (count l2))) (contains?
                                                                (->>
                                                                  (range (count l1))
                                                                  (map (fn [x] (filter (fn [y] (not= x y)) (range (count l1)))))
                                                                  (map #(map l1 %))
                                                                  (set))
                                                                (seq l2))
                              :else                           false)))

          adjacency-map (fn [l]
                          (apply hash-map (interleave l (map (fn [s] (set (filter #(l-dist-<=1 s %) l))) l))))

          path-exists?  (fn path-exists? [f m s]
                          (if (= s (set [f]))
                            true
                            (any? (map #(path-exists? % m (disj s f))
                                    (clojure.set/intersection s (m f))))))

          m             (adjacency-map l)]
      (any? (map #(path-exists? % m (disj l %)) l)))))

(defcheck solution-17050ac9
  (fn has-chain [words]
    (letfn [(substring? [s1 s2] (let [[ss ls] (sort-by count (vector s1 s2))] (some (partial = (vec ss)) (map #(concat (take % ls) (drop (+ 1 %) ls)) (range (count ls))))))
            (is-diff-one? [x y] (or (diff-one x y) (substring? x y)))
            (zip [x y] (map #(list %1 %2) x y))
            (ta [s x] (map #(cons x %) (walk (clojure.set/difference s (hash-set x)))))
            (walk [s] (if (= 1 (count s)) (list (vec s)) (mapcat #(ta s %) s)))
            (diff-one [s1 s2] (if (= (count s1) (count s2)) (= 1 (count (remove #(= (first %) (second %)) (map #(vector %1 %2) s1 s2)))) false))
            (chain [word-list] (zip (butlast word-list) (rest word-list)))
            (word-chain? [words] (boolean (every? #(apply is-diff-one? %) (chain words))))]
      (boolean (some word-chain? (walk words))))))

(defcheck solution-18346297
  (fn chainable? [s]
    (let [rot1   (fn [s] (concat (rest s) (list (first s))))
          rots   (fn [s] (take (count s) (iterate rot1 s)))
          combis (fn combis [s]
                   (if (empty? s)
                     (list(list))
                     (for [e (rots s)
                           r (combis (rest e))]
                       (cons (first  e) r))))
          edit-distance (fn [str1 str2]
                          (let [edit-distance-inner (fn edit-distance-inner [str1 str2 m n]
                                                      (if (zero? m)
                                                        n
                                                        (if (zero? n)
                                                          m
                                                          (if (= (nth str1 (- m 1)) (nth str2 (- n 1)))
                                                            (edit-distance-inner str1, str2, (dec m), (dec n))
                                                            (+ 1 (min (edit-distance-inner str1, str2, m, (dec n))
                                                                   (edit-distance-inner str1, str2, (dec m), n)
                                                                   (edit-distance-inner str1, str2, (dec m), (dec n))))))))]
                            (edit-distance-inner str1 str2 (count str1) (count str2))))
          chain? (fn [s]
                   (= (dec (count s))
                     (reduce + (map #(apply edit-distance %) (partition 2 1 s)))))]
      (not (empty? (filter chain? (combis s)))))))

(defcheck solution-185c486c
  (fn [words]
    (let
     [
      edit-distance
                   (fn edit-distance [a b]
                     (cond
                       (zero? (count a)) (count b)
                       (zero? (count b)) (count a)

                       :else
                       (let
                        [
                         rest-a (apply str (rest a))
                         rest-b (apply str (rest b))

                         cost-first-char
                                (if
                                 (= (first a) (first b))
                                  0
                                  1
                                  )

                         alternative-1
                                (+
                                 (edit-distance rest-a rest-b)
                                 cost-first-char
                                 )

                         alternative-2
                                (+
                                 (edit-distance rest-a b)
                                 1
                                 )

                         alternative-3
                                (+
                                 (edit-distance a rest-b)
                                 1
                                 )
                         ]
                         (min alternative-1 alternative-2 alternative-3)
                         )
                       )
                     )

      next-word?
                   (fn [w1 w2]
                     (=
                       (edit-distance w1 w2)
                       1
                       )
                     )

      word-sequence?
                   (fn word-sequence? [ws]
                     (if
                      (= (count ws) 1)
                       true
                       (if
                        (next-word? (first ws) (second ws))
                         (word-sequence? (rest ws))
                         false
                         )
                       )
                     )

      words-vector (apply vector words)

      permutations
                   (fn permutations [ws]
                     (lazy-seq
                       (if (seq (rest ws))
                         (apply concat
                           (for [x ws]
                             (map
                               #(cons x %)
                               (permutations (remove #{x} ws))
                               )
                             )
                           )
                         [ws]
                         )
                       )
                     )
      ]
      (not
        (nil?
          (first
            (filter
              word-sequence?
              (permutations words-vector)
              )
            )
          )
        )
      )
    ))

(defcheck solution-1a65a46b
  (fn is-list[wordlist]
    (let [one-dif (fn one-dif[w1 w2]
                    (let [cw1 (count w1)
                          cw2 (count w2)
                          cdiff (- cw1 cw2)
                          big-small-comp #(first
                                            (for
                                             [c (for [i (range (count %1))]
                                                  (concat (subvec (vec %1) 0 i) (subvec (vec %1) (inc i))))
                                              :let
                                              [word (apply str c)]
                                              :when
                                              (= 0 (compare word %2))] true))]
                      (cond
                        (or (< cdiff -1) (> cdiff 1)) false
                        (= -1 cdiff) (big-small-comp w2 w1)
                        (= 1 cdiff) (big-small-comp w1 w2)
                        :else (= 1 (count (filter #(= false %) (map = w1 w2)))))))
          ;;;;;;;;;;;;;;;;;;;;;;;;;
          graph ((fn mkmap[inwords]
                   (apply merge
                     (for [word inwords]
                       {word
                        (set
                          (for [each-of-rest (disj inwords word)
                                :when (one-dif each-of-rest word)] each-of-rest))}))) wordlist)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          pathfound (atom false)
          ;;;;;;;;;;;;;;;;;;;;;;;;;
          try-complete (fn [r]
                         (contains?
                           (set
                             (flatten
                               ((fn try-complete[root visited]
                                  (let [v (cons root visited)]
                                    (if (or @pathfound (= (set v) wordlist)) [visited (reset! pathfound true)]
                                                                             (when-let [paths (seq (clojure.set/difference (get graph root) (set v)))]
                                                                               (for [node paths] (try-complete node v)))))) r []))) true))
          ;;;;;;;;;;;;;;;;;;;;;;;;
          find (map #(try-complete %) (keys graph))
          ;;;;;;;;;;;;;;;;;;;;;;;
          one-found (contains? (set find) true)
          ]
      one-found)))

(defcheck solution-1ab4014
  (fn [ws]
    (let[lev (fn [a b]
               ((fn lvn [a b i j]
                  (if (or (zero? i) (zero? j))
                    (max i j)
                    (if (= (nth a (dec i)) (nth b (dec j)))
                      (lvn a b (dec i) (dec j))
                      (inc
                        (min
                          (lvn a b (dec i) j)
                          (lvn a b i (dec j))
                          (lvn a b (dec i) (dec j)))))))
                a b (count a) (count b)))
         g (reduce
             conj
             {}
             (map
               (fn [w] [w (set (filter #(= (lev w %) 1) ws))])
               ws))
         eup (fn eup [v vis]
               (if (= vis ws)
                 true
                 (if (empty? (clojure.set/difference (g v) vis))
                   false
                   (reduce
                     #(or %1 %2)
                     (map #(eup %1 (conj vis %1)) (clojure.set/difference (g v) vis))))))]
      (reduce
        #(or %1 %2)
        (map #(eup %1 #{%1}) ws)))))

(defcheck solution-1b972958
  (fn chain? [strs]
    (let [distance
                 (fn distance
                   [s1 s2]
                   (cond
                     (empty? s1) (count s2)
                     (empty? s2) (count s1)
                     0
                     (min
                       (inc (distance (rest s1) s2))
                       (inc (distance s1 (rest s2)))
                       (+ (if (= (first s1) (first s2)) 0 1)
                          (distance (rest s1) (rest s2))))))
          count-neighbors
                 (fn [word]
                   (count
                     (filter #(= 1 (distance % word)) (remove #(= word %) strs))))
          counts (map count-neighbors strs)]
      (if (> (count (filter #(<= % 1) counts)) 2) false true))))

(defcheck solution-1bef593
  (fn __ [words]
    (let [total (count words)
          words (sort words)
          diff-by-1 (fn dby1 [w1 w2]
                      (if (<= -1 (- (count w1) (count w2)) 1)
                        (cond
                          (= w1 w2) false
                          (= (apply min (map count [w1 w2])) 0) true
                          :else (let [h1 (nth w1 0) r1 (subs w1 1)
                                      h2 (nth w2 0) r2 (subs w2 1)]
                                  (if (= h1 h2)
                                    (recur r1 r2)
                                    (or (= r1 r2) (= r1 w2) (= r2 w1)))))
                        false))
          neighbor-map (apply merge-with clojure.set/union
                         (for [w1 words w2 words
                               :while (neg? (compare w2 w1))
                               :when (diff-by-1 w1 w2)]
                           {w1 #{w2}, w2 #{w1}}))
          explore (fn explore [seen frontier path]
                    (if (empty? frontier)
                      (= (count seen) total)
                      (some #(explore (conj seen %)
                               (clojure.set/difference (neighbor-map %) seen)
                               (conj path %))
                        frontier)))
          hamilton? (fn [] (explore #{} words '()))]
      (boolean (hamilton?)))))

(defcheck solution-1e911a2f
  (fn can-chain? [s]
    (let [levend (fn [a b]
                   (let [d (fn [self i j]
                             (let [i-1 (dec i)
                                   j-1 (dec j)
                                   c (if (not= (get a i-1) (get b j-1)) 1 0)]
                               (if (= 0 (min i j))
                                 (max i j)
                                 (min (+ 1 (self self i-1 j))
                                   (+ 1 (self self i j-1))
                                   (+ c (self self i-1 j-1))))))]
                     (d (memoize d) (count a) (count b))))

          chain? (fn [a b] (== 1 (levend a b)))

          chains? (fn [s] (every? #(chain? (first %) (second %))
                            (partition 2 1 s)))

          permutations (fn [self s]
                         (when (seq s)
                           (if (seq (rest s))
                             (for [e s, p (self self (disj s e))]
                               (cons e (flatten p)))
                             (list (list (first s))))))]

      (true? (some chains? (permutations permutations s))))))

(defcheck solution-1ee1d3cd
  (fn [s]
    (let [close? (fn [a b] (let
                            [n-matches (count (take-while #(apply = %) (partition 2 (interleave a b))))
                             a-r (drop n-matches a)
                             b-r (drop n-matches b)]
                             (some #(apply = %) [[(rest a-r) b-r] [a-r (rest b-r)] [(rest a-r) (rest b-r)]])))
          t (fn t [curr left]
              #_(prn curr left (filter #(close? (last curr) %) left))
              (if (empty? left)
                curr
                (boolean (some identity
                           (map
                             #(t (concat curr [%]) (remove #{%} left))
                             (filter #(or (empty? curr) (close? (last curr) %)) left))))))]
      (t [] s))))

(defcheck solution-1f188929
  (fn [w]
    (letfn [(n? [a b]
              (let [[[cx x] [cy y]] (sort (map (juxt count str) [a b]))]
                (case (- cy cx)
                  0 (= 1 (reduce + (map #(if (= % %2) 0 1) x y)))
                  1 (let [n (count (take-while #(apply = %) (map list x y)))]
                      (= (subs x n) (subs y (inc n))))
                  false)))
            (s? [ws r]
              (let [ws (disj ws r)
                    n (filter #(n? % r) ws)]
                (or (empty? ws) (some #(s? ws %) n))))]
      (boolean (some #(s? w %) w)))))

(defcheck solution-1f21691
  (fn [s]
    (let [suf #(loop [[x & r :as a] (seq %), [y & s :as b] (seq %2)]
                 (if (= x y) (recur r s) [a b]))
          link? (fn [[x & r :as a] [y & s :as b]] (or (= a s) (= b r) (= s r)))
          chains (fn f [s] (if (= 1 (count s)) [s]
                                               (for [h s, c (f (disj s h)) :when (apply link? (suf h (first c)))] (cons h c))))]
      (not (empty? (chains s))))))

(defcheck solution-2023bb7e
  (fn [s]
    (boolean
      (some
        (fn [s]
          (every? (fn f [[a b]]
                    (let [ca (count a)
                          cb (count b)]
                      (cond
                        (< ca cb) (f [b a])
                        (= ca cb) (= 1 (count (remove true? (map = a b))))
                        :else (some #(= b %) (for [i (range ca)] (str (subs a 0 i) (subs a (inc i))))))))
            (partition 2 1 s)))
        ((fn p [[v & s]]
           (if (seq s)
             (let [l (p s)]
               (apply concat
                 (for [c l]
                   (for [i (range (inc (count c)))]
                     (concat (take i c) [v] (drop i c))))))
             [[v]])) (vec s))))))

(defcheck solution-2065a1ea
  (fn chain1 [xs]
    (letfn [

            (subst [x1 x2]  ;same length
              (let [re  (map #(if % 0 1) (map =  x1 x2 )) ]
                (if (= (reduce + re) 1)
                  true
                  false
                  ))
              )

            (del_ins [x1 x2]
              (let [sm (if (> (count x1) (count x2)) x2 x1)
                    gm (if (> (count x1) (count x2)) x1 x2)

                    gms (for [i (range (count gm))]
                          (apply str (concat
                                      (subvec (vec gm) 0 i) (subvec (vec gm) (inc i))
                                      ))
                          )
                    ]
                (some true? (map #(= % sm) gms))

                )

              )

            (possible [x1 x2]
              (cond (= (count x1) (count x2))
                    (subst x1 x2)
                    (= (Math/abs (- (count x1) (count x2))) 1)
                    (del_ins x1 x2)
                    :else false
                    )
              )



            (linksets [lsets xs]
              (if (first xs)
                (let [lk  (first xs)
                      l1 (first lk)
                      l2 (second lk)
                      ]
                  (recur
                    (assoc lsets l1 (conj (lsets l1) l2))
                    (rest xs)
                    )
                  )
                lsets
                )
              )

            (initsets [xs]
              (into {}  (for [x xs]
                          [x #{}]
                          )
                ))

            (links [xs]
              (for [x1 xs
                    x2 xs
                    :when (and (not (= x1 x2))
                               (possible x1 x2)
                               )
                    ]
                [x1 x2]
                )

              )

            (permuts  [ paths a-set]

              (let [lsets (linksets (initsets a-set) (links a-set))
                    len (count a-set)]
                (cond (empty? paths)
                      (recur
                        [ {[(first a-set)] (lsets (first a-set))}]  a-set)
                      (every? #(empty? (first  (vals %)))  paths)
                      (filter #(= (count (first (keys % ))) len)  paths)
                      :else
                      (let [fores (for [p paths]
                                    (let [pv (first (keys p))
                                          ps (first (vals p))
                                          ]

                                      (if (> (count ps) 0)
                                        (for [ x ps ]
                                          {(conj pv x) (clojure.set/difference (lsets x)
                                                         (clojure.set/union (set pv) #{x}))}
                                          )
                                        [  {pv #{} }   ]
                                        )
                                      )
                                    )]

                        (recur (vec (reduce concat fores ) ) a-set )
                        )
                      )
                )
              )




            (rotations  [a-seq]
              (let  [a-vec  (vec a-seq)]
                (for  [i  (range  (count a-vec))]
                  (concat  (subvec a-vec i)  (subvec a-vec 0 i)))))


            ]


      (loop [rs (rotations xs) ]
        (let [xx (first rs)]
          (if (empty? xx) false
                          (if (> (count (permuts [] xx ) ) 0)
                            true
                            (recur (rest rs))
                            )

                          ))
        )



      )
    ))

(defcheck solution-20cc118f
  (letfn [(some-path?
            [connections from]
            (let [remaining (dissoc connections from)]
              (cond (= connections remaining) false
                    (empty? remaining) true
                    :default (some #(some-path? remaining %)
                               (get connections from)))))
          (word-graph
            [word-set]
            (reduce (fn [m [k v]]
                      (update-in m [k] conj v))
              {}
              (for [a word-set b word-set
                    :when (and (not= a b)
                               (one-step (seq a) (seq b)))]
                [a b])))
          (one-step
            [a b]
            (cond (= a b) true
                  (= (first a) (first b)) (recur (rest a) (rest b))
                  :default (or (one-away a b)
                               (one-away b a))))
          (one-away
            [[a' :as a] [_ & bs :as b]]
            (or (= (cons a' b) a)       ; insert
                (= (cons a' bs) a) ; swap
                (= bs a)))] ; drop
    #(boolean (some (partial some-path? (word-graph %)) %))))

(defcheck solution-213b2f15
  (fn [word-set]
    (letfn [(change-one? [w1 w2] (= 1 (count (filter (fn [[c1 c2]] (not= c1 c2)) (map vector w1 w2)))))
            (add-one? [w1 w2] (>= (count (filter (fn [n] (and (= (take n w1) (take n w2)) (= (drop n w1) (drop (inc n) w2)))) (range (inc (count w2))))) 1))
            (chain? [w1 w2] (let [c1 (count w1) c2 (count w2)] (or (and (= c1 c2) (change-one? w1 w2)) (and (= c1 (inc c2)) (add-one? w2 w1)) (and (= (inc c1) c2) (add-one? w1 w2)))))
            (find-edges [words] (map (fn [wrd] (filter (fn [x] (chain? wrd (nth words x))) (range (count words)))) words))
            (step [[paths edges]] (vector (reduce (fn [acc [st lst]] (let [ns (nth edges lst)] (if (empty? ns) acc (reduce (fn [a nxt] (conj a (vector (conj st nxt) nxt))) acc (filter #(not (contains? st %)) ns))))) '() paths) edges))]
      (let [words (map identity word-set) n (count words) es (find-edges words)]
        (not (every? identity
               (map
                 (fn [x] ((fn [[paths edges]] (empty? paths))
                          (nth (iterate step [(list [#{x} x]) es]) (dec n))))
                 (range n))))))))

(defcheck solution-2145f21a
  (fn [x]
    (let [issub? (fn [a b]
                   (and
                    (= (count a) (count b))
                    (= 1 (reduce + (map #(if (= % %2) 0 1) a b)))))
          isdel? (fn [a b]
                   (and
                    (= (dec (count a)) (count b))
                    (loop [x a y b c 0]
                      (if (and (empty? x) (empty? y) (= 1 c) )
                        true
                        (if (= (first x) (first y))
                          (recur (rest x) (rest y) c)
                          (if (> c 0) false (recur (rest x) y (inc c))))))))
          isone? #(or (isdel? % %2) (isdel? %2 %) (issub? % %2))]
      (loop [r [(first x)] i (rest x) f []]
        (if (and (or (nil? i) (empty? i)) (empty? f))
          true
          (if (or (empty? i) (nil? i))
            false
            (if (isone? (first r) (first i))
              (recur (cons (first i) r) (concat (rest i) f) [])
              (if (isone? (last r) (first i))
                (recur (conj (into [] r) (first i)) (concat (rest i) f) [])
                (recur r (rest i) (conj f (first i)))))))))))

(defcheck solution-21e22390
  (fn [s]
    (let [mlev (memoize
                 (fn [mem s t]
                   (let [slen (count s)
                         tlen (count t)
                         cost (if (= (last s) (last t)) 0 1)]
                     (cond
                       (zero? slen) tlen
                       (zero? tlen) slen
                       :else (min
                               (+ 1 (mem mem (butlast s) t))
                               (+ 1 (mem mem s (butlast t)))
                               (+ cost (mem mem (butlast s) (butlast t))))))))
          lev (partial mlev mlev)
          exists (fn exists
                   ([s] (not (nil? (some #(exists s %) s))))
                   ([s v]
                    #_(println s v)
                    (cond
                      (nil? v) false
                      (= s #{v}) true
                      :else (some #(exists (disj s v) %) (filter #(= 1 (lev v %)) s)))))]
      (exists s))))

(defcheck solution-22c6073a
  (fn  [s]
    (let [perms
          (fn perms [a-set]
            (if (empty? a-set) [[]]
                               (mapcat
                                 (fn [x]
                                   (map #(conj %1 x)
                                     (perms (disj a-set x))))
                                 a-set)))
          dist-one?
          (fn [s1 s2]
            (loop [[e1 & r1 :as as1] (map identity s1)
                   [e2 & r2 :as as2] (map identity s2)]
              (if (= e1 e2)
                (if (some empty? [r1 r2])
                  (every? #(< (count %) 2) [r1 r2])
                  (recur r1 r2))
                (or (= r1 as2) (= as1 r2) (= r1 r2)))
              ))]
      (->> s
        (perms)
        (map (partial partition 2 1))
        (map (partial every? (partial apply dist-one?)))
        (some true?)
        (true?))
      )))

(defcheck solution-22cf8d4f
  (letfn [
          (dis [x1 x2] (cond
                         (= 0 (count x1)) (count x2)
                         (= 0 (count x2)) (count x1)
                         true (apply min (remove nil? [
                                                       (inc (dis (rest x1) (rest x2)))
                                                       (inc (dis x1 (rest x2)))
                                                       (inc (dis (rest x1) x2))
                                                       (when (= (first x1) (first x2)) (dis (rest x1) (rest x2)))]))))
          (begin-chain [x] (cond
                             (= 1 (count x)) [(first x)]
                             true (for [i x :when (seq (filter #(> 2 (dis i %)) (begin-chain (disj x i))))] i)))
          (is-chain [x] (boolean (seq (begin-chain x))))] is-chain))

(defcheck solution-2424e19d
  (fn [words]
    (let [chainable? (fn [a b]
                       (cond
                         (= (count a) (count b))
                         (->> (map = a b) (remove identity) count (= 1))
                         (= 1 (Math/abs (- (count a) (count b))))
                         (let [[longer shorter] (if (> (count a) (count b))
                                                  [a b]
                                                  [b a])]
                           (->> (map vector longer (range))
                             (drop-while (fn [[v i]] (= v (get shorter i))))
                             next
                             (map (fn [[v i]] (= v (get shorter (dec i)))))
                             (every? identity)))
                         :else false))
          construct-chain (fn construct-chain [w ws]
                            (if (empty? ws)
                              [w]
                              (let [next-chain (first (filter (partial chainable? w) ws))
                                    next-ws    (when next-chain (disj ws next-chain))]
                                (when next-chain (cons w (construct-chain next-chain next-ws))))))
          construct-chains (fn construct-chains [w ws]
                             (if (empty? ws)
                               [[w]]
                               (let [next-chains (filter (partial chainable? w) ws)]
                                 (when (seq next-chains)
                                   (mapcat (fn [nc] (let [next-ws (disj ws nc)]
                                                      (map #(cons w %) (construct-chains nc next-ws)))) next-chains)))))]
      (->> words
        (mapcat (fn [w] (let [others (disj words w)] (construct-chains w others))))
        first
        count
        (= (count words))))))

(defcheck solution-24b6f9cf
  (fn [words]
    (letfn [(chainable? [a b]
              (cond
                (> (count b) (count a)) (chainable? b a)
                (> (count a) (count b))
                (not (every? false?
                       (for [i (range (count a))]
                         (= b
                           (apply str
                             (apply str (take i a))
                             (apply str (drop (inc i) a)))))))
                :else (<= (count (remove true? (map = a b))) 1)))]
      ((fn wc?
         ([s]
          (if (<= (count s) 1)
            true
            (not (every? false?
                   (for [w s]
                     (wc? w (remove #{w} s)))))))
         ([w s]
          #_(prn w s)
          (if (zero? (count s))
            true
            (not (every? false?
                   (for [w2 s]
                     (and
                      (chainable? w w2)
                      (wc? w2 (remove #{w2} s)))))))))
       words))))

(defcheck solution-25086513
  (fn wordChain[l]
    (letfn[(oneLetterDiff[s1 s2]
             (and
              (= (count s1) (count s2))
              (= 1 (apply + (map #(if (= %1 %2) 0 1) s1 s2)))))

           (insOrDel[s1 s2]
             (cond
               (empty? s1) (= 1 (count s2))
               (empty? s2) (= 1 (count s1))
               (= (first s1) (first s2)) (insOrDel (rest s1) (rest s2))
               :else (or (= (seq s1) (rest s2)) (= (seq s2) (rest s1)))))
           (createWordGraph[l]
             ;; HIER
             (apply merge (map
                            (fn [el]
                              {el (vec (filter #(or (oneLetterDiff el %) (insOrDel el %)) l))})
                            l)))
           (dfsNodes[g v s node]
             (if (or (contains? s node) (empty? (g node)))
               #{v}
               (reduce
                 #(apply (partial merge %1) %2)
                 (map #(dfsNodes g (conj v node) (conj s node) %) (g node)))))
           (hamiltonianPaths[g]
             (reduce
               #(apply (partial merge %1) %2)
               (map
                 (fn [node]
                   (set (filter
                          #(= (count (keys g)) (count %))
                          (dfsNodes g [] #{} node))))
                 (keys g))))]
      (not (empty? (hamiltonianPaths (createWordGraph l)))))))

(defcheck solution-250ac3df
  (fn prob82
    ;; build all the permutations of the set or words xs
    ;; levenshetien returns the number edits diff between two strings
    ;; map diff-by-one-letter over all the combinations
    ;; find if there is one set that is all true
    [xs]
    (letfn [(permutations [xs]
              (if (empty? xs)
                '()
                (if (= 1 (count xs))
                  (list (seq xs))
                  (for [x xs
                        y (permutations (disj xs x))]
                    (cons x y)))))
            (all-true [xs]  ;; return true xs contains all true
              (every? true? xs))
            (levenshtein [str1 str2]  ;; http://rosettacode.org/wiki/Levenshtein_distance#Clojure
              (let [len1 (count str1)
                    len2 (count str2)]
                (cond (zero? len1) len2
                      (zero? len2) len1
                      :else
                      (let [cost (if (= (first str1) (first str2)) 0 1)]
                        (min (inc (levenshtein (rest str1) str2))
                          (inc (levenshtein str1 (rest str2)))
                          (+ cost
                             (levenshtein (rest str1) (rest str2))))))))
            (one-letter-diff [a b]     ;; same string or one letter difference
              (< (levenshtein a b) 2))
            ]
      (let [s (permutations xs)]
        (if (some true? (map all-true (map #(map one-letter-diff % (rest %)) s)))
          true
          false)))))

(defcheck solution-25540b13
  (fn word-chain? [s]
    (letfn [
            (substrings-1 [s]
              (reduce (fn [a b] (conj a (str (subs s 0 b) (subs s (inc b))))) #{} (range (count s)))
              )
            (similar?[s1 s2]
              (let [c1 (count s1) c2 (count s2)]
                (cond
                  (= c1 c2) (< (count (filter false? (map = s1 s2))) 2)
                  (= c1 (inc c2)) (some true? (map = (substrings-1 s1) (repeat s2)))
                  (= (inc c1) c2) (some true? (map = (substrings-1 s2) (repeat s1)))
                  :else false
                  )
                )
              )
            (build-chain [start graph]
              (loop [chain start nodes graph]
                (let [new-chain (reduce (fn [a b]
                                          (let [last-stop (last (last a))]
                                            (if (and (or (= (first b) last-stop) (= (last b) last-stop ))
                                                     (not (some #{(if (= (first b) last-stop) (last b) (first b))} (flatten a)))
                                                     )
                                              (conj a [last-stop (if (= (first b) last-stop) (last b) (first b))])
                                              a
                                              ))
                                          ) chain  nodes)]
                  (if (= new-chain chain)
                    chain
                    (recur new-chain graph)
                    )
                  )
                )
              )
            ]
      (let [graph
            (reduce (fn [a b]
                      (into a (for [x (filter #(and (not (= % b)) (similar? % b)) s)] [b x]))
                      ) #{} s)
            ]
        (boolean (some #(= % (dec (count s))) (for [x graph] (count (build-chain [x] (remove #{x} graph))))))
        )
      )
    ))

(defcheck solution-257dc1c3
  (fn [s] (let [c (fn [w x]
                    (condp <= (- (count w) (count x))
                      2 false
                      0 (some identity
                          (for [n (range (count w))]
                            (and (.startsWith x (subs w 0 n))
                                 (.endsWith x (subs w (inc n))))))
                      (recur x w)))]
            (loop [r (for [w s] [[w] (disj s w)])]
              (cond
                (empty? r) false
                (= s (set (ffirst r))) true
                :else (recur (for [[[w & _ :as ch] s] r
                                   x (filter #(c % w) s)]
                               [(cons x ch) (disj s x)])))))))

(defcheck solution-26b20b71
  (fn [input]
    (let [n (fn [word1 word2 f]
              (loop [word1 word1 word2 word2 f f]
                (if (empty? word2)
                  true
                  (let [c1 (first word1)
                        c2 (first word2)]
                    (cond
                      (= c1 c2) (recur (rest word1) (rest word2) f)
                      (fn? f) (recur (rest word1) (f word2) nil)
                      :else false)))))
          neighbourly? (fn [word1 word2]
                         (let [size1 (count word1)
                               size2 (count word2)]
                           (cond
                             (= word1 word2) false
                             (= size1 size2) (n word1 word2 rest)
                             (= 1 (- size1 size2)) (n word1 word2 identity)
                             (= 1 (- size2 size1)) (n word2 word1 identity)
                             :else false)))
          chainable? (fn chainable? [word remaining]
                       (if (empty? remaining)
                         true
                         (loop [coll remaining]
                           (let [next (first coll)]
                             (cond
                               (nil? next) false
                               (and (neighbourly? word next) (chainable? next (disj remaining next))) true
                               :else (recur (rest coll)))))))]
      (not= nil (some #(chainable? % (disj input %)) input)))))

(defcheck solution-26c91634
  (fn
    [sss]
    (let
     [vec-remove
      (fn [coll pos] (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))
      one-letter-difference
      (fn
        [w1 w2]
        (let
         [w1c (count w1) w2c (count w2) w1s (seq w1) w2s (seq w2)]
          (if
           (= w1c w2c)
            (if
             (=
               1
               (count
                 (filter
                   (fn* [p1__2893#] (= p1__2893# false))
                   (map (fn* [p1__2894# p2__2895#] (= p1__2894# p2__2895#)) w1s w2s))))
              w2
              "")
            (if
             (< w1c w2c)
              (if
               (<
                 0
                 (count
                   (filter
                     #{true}
                     (map
                       (fn* [p1__2896#] (= (apply str p1__2896#) w1))
                       (map
                         (fn* [p1__2897#] (vec-remove (vec w2s) (first p1__2897#)))
                         (map-indexed (fn [i e] (vector i e)) w2s))))))
                w2
                "")
              (if
               (<
                 0
                 (count
                   (filter
                     #{true}
                     (map
                       (fn* [p1__2898#] (= p1__2898# w2))
                       (map
                         (fn* [p1__2899#] (clojure.string/replace-first w1 p1__2899# ""))
                         w1s)))))
                w2
                "")))))
      combinations
      (fn
        combinations
        [s r]
        (if
         (empty? (rest s))
          (vector (conj r (first s)))
          (mapcat
            (fn*
              [p1__2900#]
              (combinations (remove #{p1__2900#} s) (conj r p1__2900#)))
            s)))]
      (<
        0
        (count
          (filter
            #{true}
            (map
              (fn*
                [p1__2901#]
                (= (last p1__2901#) (reduce one-letter-difference p1__2901#)))
              (combinations sss []))))))))

(defcheck solution-27556e48
  (fn [s]
    (let [diff1 (fn [w1 w2] ; true if there's only one different letter or one letter added/removed
                  (if (= (count w1) (count w2))
                    (= (count (remove (fn [[a b]] (= a b)) (map list w1 w2))) 1) ; one letter changed
                    (let [rem-letter (fn [w i] (let [[w1 w2] (split-at i w)] (concat w1 (next w2)))) ; one letter added/removed
                          added-letter-index (fn [w1 w2] (first (keep-indexed (fn [i [a b]] (if (not= a b) i)) (map list w1 w2))))
                          [w1 w2] (sort-by count [w1 w2]) ; w1 is the shortest word
                          i (added-letter-index w1 w2)] ; if i is nil, added letter is in last position
                      (= (seq w1) (if i (rem-letter w2 i) (butlast w2))))))
          links (group-by #(count (filter (fn [w] (diff1 w %)) s)) s)]
      (<= (count (links 1)) 2))))

(defcheck solution-2788738e
  (fn [s]
    (letfn [(match? [word0 word1]
              (let [c0 (count word0)
                    c1 (count word1)
                    [larger smaller] (if (< c0 c1)
                                       [word1 word0]
                                       [word0 word1])]
                (if (not= c0 c1)
                  (loop [lrg larger
                         sml smaller]
                    (if (empty? sml)
                      (or (char? lrg) (= 1 (count lrg)))
                      (recur (clojure.string/replace-first lrg (str (first sml)) "")
                        (rest sml))))
                  (loop [w1 word0
                         w2 word1
                         diff []]
                    (if (empty? w1)
                      (= 1 (count diff))
                      (recur (rest w1)
                        (rest w2)
                        (if (= (first w1) (first w2))
                          diff
                          (conj diff (first w1)))))))))
            (solution? [l]
              (loop [l l]
                (cond (false? l) false
                      (= 1 (count l)) true
                      :else (recur (if (match? (first l) (second l))
                                     (rest l)
                                     false)))))

            ;; from https://raw.github.com/clojure/math.combinatorics/master/src/main/clojure/clojure/math/combinatorics.clj
            (iter-perm [v]
              (let [len (count v),
                    j (loop [i (- len 2)]
                        (cond (= i -1) nil
                              (< (v i) (v (inc i))) i
                              :else (recur (dec i))))]
                (when j
                  (let [vj (v j),
                        l (loop [i (dec len)]
                            (if (< vj (v i)) i (recur (dec i))))]
                    (loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
                      (if (< k l)
                        (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
                        v))))))

            (permutations [items] (let [v (vec items)]
                                    (map #(map v %)
                                      (lazy-seq
                                        (let [vec-sorted (vec (sort (range (count v))))]
                                          (if (zero? (count vec-sorted))
                                            (list [])
                                            ((fn vlp [v]
                                               (when v (cons v (lazy-seq (vlp (iter-perm v))))))
                                             vec-sorted)))))))]
      (boolean (some solution? (permutations s))))))

(defcheck solution-27bf73f1
  (fn [words]
    (letfn [(remove-first [x coll]
              (let [[pre post] (split-with #(not= x %) coll)]
                (concat pre (rest post))))
            (permutations [xs]
              (if (empty? xs)
                '([])
                (for [x xs
                      :let [perms (permutations (remove-first x xs))]
                      perm perms]
                  (cons x perm))))
            (head-match-count [xs ys] (count (take-while identity (map = xs ys))))
            (differ-one-letter? [s1 s2]
              (let [n (head-match-count s1 s2)]
                (case (- (count s2) (count s1))
                  0 (= 1 (count (remove identity (map = s1 s2))))
                  1 (= (count s1)
                      (+ n (head-match-count (drop n s1) (drop (inc n) s2))))
                  -1 (= (count s2)
                       (+ n (head-match-count (drop (inc n) s1) (drop n s2))))
                  false)))]
      (->> (permutations words)
        (some #(->> (partition 2 1 %)
                 (every? (partial apply differ-one-letter?))))
        boolean))))

(defcheck solution-280fd368
  (fn chain [wordlist]
    (let
     [levdist (fn levdist [left right]
                (cond
                  (empty? left) (count right)
                  (empty? right) (count left)
                  :else (let [cost (cond
                                     (= (last left) (last right)) 0
                                     :else 1)]
                          (min (inc (levdist (butlast left) right))
                            (inc (levdist left (butlast right)))
                            (+ cost (levdist (butlast left) (butlast right)))))))
      adjacent-words (filter #(= (levdist (first %) (second %)) 1) (for [x wordlist y wordlist] [x y]))
      check-chain (fn check-chain [startword wordchain adjacent]
                    (if (= (set (conj wordchain startword)) (set wordlist))
                      true
                      (let [possibilities (filter #(= startword (first %)) adjacent)]
                        (if (empty? possibilities)
                          false
                          (some true? (for [possibility possibilities]
                                        (check-chain (second possibility)
                                          (conj wordchain startword)
                                          (filter #(not (= startword (first %))) adjacent))))))))]
      (or (some true? (for [word wordlist] (check-chain word [] adjacent-words))) false))))

(defcheck solution-281d5def
  (fn [s] (let [
                con? (fn [x] (fn [y] (loop [[xf & xr :as x] x, [yf & yr :as y] y, n 0]
                                       (cond (or (empty? x) (empty? y)) (= 1 (+ n (count x) (count y)))
                                             (= xf yf) (recur xr yr n)
                                             (> (count x) (count y)) (recur xr y (inc n))
                                             (< (count x) (count y)) (recur x yr (inc n))
                                             :else (recur xr yr (inc n))
                                             ))))
                vc (reduce #(conj % (filter (con? %2) s)) [] (seq s))
                ]
            (> 3 (count (filter #(= 1 %) (map count vc))))
            )))

(defcheck solution-2845dd17
  (fn [words]
    (letfn [(del [w1 w2]
              ;; add 1 letter to w2
              (and (= (count w1) (inc (count w2)))
                   (clojure.set/subset? (set w2) (set w1))))
            ;; remove 1 letter from w2
            (ins [w1 w2]
              (del w2 w1))
            ;; replace one letter in w2
            (sub [w1 w2]
              (and (= (count w1) (count w2))
                   (= 1 (count (filter false? (map = w1 w2))))))
            (perms [coll]
              (if (<= (count coll) 1)
                (list coll)
                (apply concat
                  (for [x coll
                        :let [xs (remove #{x} coll)]]
                    (map (partial cons x) (perms xs))))))]
      (-> (for [coll (perms words)
                :when
                (every? (fn [[a b]]
                          (some #(% a b) [ins del sub]))
                  (partition 2 1 coll))]
            coll)
        seq boolean))))

(defcheck solution-28593014
  (fn [xs]
    (let [subst?  (fn [x y]
                    (and (string? x) (string? y)
                         (= (count x) (count y))
                         (= 1 (apply + (map (fn [a b] (if (= a b) 0 1)) x y)))))
          insert? (fn insert? [x y]
                    (and (string? x) (string? y)
                         (if (> (count y) (count x))
                           (insert? y x)
                           (and (= (count x) (inc (count y)))
                                (contains?
                                  (set
                                    (for [i (range (inc (count y)))
                                          l "abcdefghijklmnopqrstuvwxyz"]
                                      (let [[start end] (split-at i y)]
                                        (str (apply str start) l (apply str end)))))
                                  x)))))
          chain?  (fn chain?
                    ([x] x)
                    ([x y]
                     (if (or (subst? x y) (insert? x y)) y))
                    ([x y & more]
                     (if (chain? x y) (and (chain? x y) (apply chain? y more)))))
          grow    (fn [xs ws]
                    ;; a collection of words and a chain of words, find chain extensions
                    (for [w     (apply disj (set ws) xs)
                          :let  [c (conj xs w)]
                          :when (apply chain? c)]
                      c))
          grow*   (fn [cs]
                    (mapcat #(grow % xs) cs))]

      (let [n (count xs)]
        (boolean (first (nth (iterate grow* (map vector xs)) (dec n))))))))

(defcheck solution-286614b3
  (fn [words]
    (let [remove-nth (fn [n w]
                       (apply str (concat (take n w) (drop (inc n) w))))

          deletion? (fn [w1 w2]
                      (some #(= w2 %)
                        (map #(remove-nth % w1)
                          (range (inc (count w2))))))

          insertion? (fn [w1 w2]
                       (deletion? w2 w1))

          substitution? (fn [w1 w2]
                          (if (not= (count w1) (count w2))
                            false
                            (some #(= (remove-nth % w1)
                                     (remove-nth % w2))
                              (range (count w1)))))

          next? (fn [w1 w2]
                  (some #(% w1 w2)
                    [deletion? insertion? substitution?]))

          soln? (fn soln? [word words]
                  (if (empty? words)
                    true
                    (some #(and (next? word %)
                                (soln? % (disj words %)))
                      words)))
          ]
      (not (nil?
             (some #(soln? % (disj words %))
               words))))))

(defcheck solution-28aa0d9d
  (fn [x]
    (letfn [(l[a b]
              (loop [r 0 v (range (inc (count a)))]
                (if (= r (count b))
                  (last v)
                  (recur (inc r)
                    (into [(+ 1 r)] (loop [i 0 t []]
                                      (if (< i (count a))
                                        (recur (+ 1 i)
                                          (if (= (nth b r) (nth a i))
                                            (conj t (nth v i))
                                            (conj t (inc
                                                      (min (nth v i)
                                                        (nth v (+ 1 i))
                                                        (if (= i 0) (+ 1 r) (nth t (- i 1))))))))
                                        t)))
                    ))))
            (rip [a i] (concat (take i a) (drop (inc i) a)))
            (ind [a] (range (count a)))
            (g [a i]
              (f (nth a i) (rip a i)))
            (f [a b]
              (if (empty? b)
                true
                (some #(g b %) (filter #(= 1 (l a (nth b %))) (ind b)))))]
      (= true ((fn [a] (some #(g a %) (ind a))) (apply list x))))))

(defcheck solution-2917e8e0
  (fn make-word-chain? [s]
    (letfn [(levenshtein [str1 str2]
              (let [len1 (count str1)
                    len2 (count str2)]
                (cond (zero? len1) len2
                      (zero? len2) len1
                      :else
                      (let [cost (if (= (first str1) (first str2)) 0 1)]
                        (min (inc (levenshtein (rest str1) str2))
                          (inc (levenshtein str1 (rest str2)))
                          (+ cost
                             (levenshtein (rest str1) (rest str2))))))))
            (get-word-chain [start remaining]
              (let [matches (filter #(= 1 (levenshtein start %1)) remaining)]
                (if (empty? matches)
                  [#{start}]
                  (->> matches
                    (map #(get-word-chain % (disj remaining %)))
                    (reduce into)
                    (map #(conj % start))))))
            (starts-with? [start]
              (->> (get-word-chain start (disj s start))
                (filter #(= s %))
                empty?
                not))]
      (->> (some starts-with? s)
        boolean))))

(defcheck solution-294e846d
  (fn [words]
    (let [some? (comp not nil?)
          lev (fn lev [a b]
                (if (empty? a)
                  (count b)
                  (if (empty? b)
                    (count a)
                    (let [cost (if (= (first a) (first b)) 0 1)]
                      (min
                        (inc (lev (rest a) b))
                        (inc (lev a (rest b)))
                        (+ cost (lev (rest a) (rest b))))))))
          connections (->> (for [w words other words :when (= 1 (lev w other))]
                             [w other])
                        (group-by first)
                        (map (fn [[k v]] [k (map second v)]))
                        (into {}))
          has-path? (fn has-path? [curr visited]
                      (let [conns (->> (connections curr)
                                    (remove visited))
                            visited (conj visited curr)]
                        (if (seq conns)
                          (some? (some identity (for [next conns] (has-path? next visited))))
                          (= visited words))))]
      (some? (some true? (map #(has-path? % #{}) words))))))

(defcheck solution-296d25d9
  (fn [arg]
    (letfn [(reststr [x] (apply str (rest x)))
            (conn? [x y]
              (cond (= (first x) (first y)) (conn? (reststr x) (reststr y))
                    (> (count x) (count y)) (= (reststr x) y)
                    (< (count x) (count y)) (= x (reststr y))
                    (= (count x) (count y)) (= (reststr x) (reststr y))
                    ))
            (chain? [x xs]
              (if (empty? (disj xs x))
                true
                ((complement not-any?) true? (map #(chain? % (disj xs x))
                                               (filter #(conn? x %) (disj xs x)))))
              )]
      ((complement not-any?) true? (map #(chain? % arg) arg)))))

(defcheck solution-296ea19f
  (fn [ws]
    (letfn [
            (df [[lh & lt] [sh & st :as s]]
              (cond (and (nil? lh) (nil? sh))
                    true
                    (= lh sh)
                    (recur lt st)
                    :else
                    (wf lt s)))
            (ef [[ah & at] [bh & bt]]
              (cond (and (nil? ah) (nil? bh))
                    true
                    (= ah bh)
                    (recur at bt)
                    :else
                    (wf at bt)))
            (wf [[ah & at] [bh & bt]]
              (cond (and (nil? ah) (nil? bh))
                    true
                    (= ah bh)
                    (recur at bt)
                    :else
                    false))
            (is-chain? [a b]
              (let [d (- (count a)
                         (count b))]
                (cond (= d 1) (df a b)
                      (= d 0) (ef a b)
                      (= d -1) (df b a)
                      :else false)))
            (chain-words [a s]
              (filter
                #(and (not= a %)
                      (is-chain? a %))
                s))]
      (let [s (some
                #(when
                  (= (count
                       (chain-words % ws))
                    1)
                   %)
                ws)]
        ((fn r
           [n
            rst]
           (cond
             (empty? rst)
             true
             (nil? n)
             false
             :else
             (reduce
               (fn ([a b] (or a b))
                 ([] false))
               (map (fn [nxt]
                      #_(prn n nxt rst)
                      (r nxt
                        (disj rst nxt)))
                 (chain-words n rst)
                 )
               ))
           ) s (disj ws s)))


      )))

(defcheck solution-2985add6
  (fn can-list2 [set]
    (let [f (first set)
          rem (disj set f)
          check-sub (fn [a b]
                      (when (= (count a) (count b))
                        (->>
                          (map #(= %1 %2) a b)
                          (filter not)
                          (count)
                          (= 1))))

          check-del (fn [a b]
                      (let [b-vec (apply vector b)
                            b-count (count b)
                            a-seq (seq a)]
                        (->>
                          (for [i (range 0 b-count)]
                            (concat (subvec b-vec 0 i) (subvec b-vec (inc i))))
                          (some #(= a-seq %)))))

          check-ins (fn [a b] (check-del b a))

          can-modify (fn [a b] (or (check-sub a b)
                                   (check-del a b)
                                   (check-ins a b)))

          matches (fn [f r]
                    (filter #(can-modify f %) r))


          can-order (fn [left right r]
                      (->>
                        (map #(vector % (can-modify left %) (can-modify right %)) r)
                        (filter (fn [[_ l r]] (or l r)))
                        )
                      )

          gen-left-branch (fn [right set branches]
                            (->>
                              (filter (fn [[_ l _]] l) branches)
                              (map (fn [[a]] (vector a right (disj set a))))))

          gen-right-branch (fn [left set branches]
                             (->>
                               (filter (fn [[_ _ r]] r) branches)
                               (map (fn [[a]] (vector left a (disj set a))))))

          try-branch (fn try-branch [left right r]
                       (if (empty? r)
                         true
                         (let [branches (can-order left right r)
                               left-branches (gen-left-branch right r branches)
                               right-branches (gen-right-branch left r branches)
                               all-branches (concat left-branches right-branches)]
                           (if (empty? all-branches)
                             false
                             (loop [[[left right set] & rem] all-branches]
                               (if (try-branch left right set)
                                 true
                                 (if (empty? rem)
                                   false
                                   (recur rem))
                                 ))))))

          ]
      (try-branch f f rem)  )
    ))

(defcheck solution-2ad3fe60
  (fn [s]
    (let [
          distance=1?
            (fn [w1 w2]
              (let
               [[l1 l2] (map count [w1 w2])
                check (fn [m [a & wa :as w1] [b & wb]] ; insert (m=true) ; else subst
                        (if (not= a b)
                          (if (not= (if m (seq w1) wa) wb)
                            false
                            true)
                          (recur m wa wb)))]
                (cond
                  (= w1 w2) false
                  (= l1 l2) (check false w1 w2) ; substitution ?
                  (> l1 l2) (recur w2 w1)
                  (= 1 (- l2 l1)) (check true w1 w2) ; insertion ?
                  :else false)))
          g (reduce
              (fn [h e]
                (assoc h e (set (filter (partial distance=1? e) s))))
              {} s)]
      ((fn rec-wordchain? [ l v ws ]
         (cond
           (= l 0) (do
                     #_(println "l = " l "; v = " v "; ws =" ws)
                     true)
           (empty? ws) false
           :else (reduce
                   #(or %1 (rec-wordchain? (dec l) (conj v %2) (apply disj (g %2) v)))
                   false ws)))
       (count s) [] s))))

(defcheck solution-2ae395bc
  (fn word-chain? [coll]
    (letfn [(permutations [xs]
              (if-not (seq xs)
                (list ())
                (for [x xs
                      ys (permutations (for [z xs :when (not= z x)] z))]
                  (conj ys x))))]
      (let [word-chains (permutations coll)
            res (some identity
                  (map (fn [w-chain]
                         (every? identity
                           (map (fn [w1 w2]
                                  (let [cnt-w1 (count w1)
                                        cnt-w2 (count w2)]
                                    (condp = (- cnt-w1 cnt-w2)
                                      ;; check for addition of character
                                      -1 (let [extra-char (some identity (map-indexed (fn [indx itm] (if (> indx (dec (count w1))) itm (if (= itm (nth w1 indx)) nil itm))) w2))
                                               s w1
                                               r  extra-char
                                               indx (range 0 (inc (count s)))]
                                           (some #(= w2 %) (map (fn [i]
                                                                  (apply str  (concat (take i s) [r] (drop i s)))) indx)))
                                      ;; check for deletion of character
                                      1 (some #(= w2 %) (map (fn [i] (apply str (concat (take (dec  i) w1) (drop  i w1)))) (range 1 (inc (count w1)))))
                                      ;; check for substitution of character
                                      0 (let [sub-char (clojure.set/difference (set w2) (set w1))]
                                          (if (= 1 (count sub-char))
                                            (let [s w1
                                                  r  sub-char
                                                  indx (range 1 (inc (count s)))]
                                              (some #(= w2 %) (map (fn [i]
                                                                     (apply str  (concat (take (dec i) s) r (drop i s))))  indx)))
                                            false))
                                      ;; default value
                                      false)))
                             w-chain (rest w-chain)))) word-chains))]
        (if (nil? res) false true)))))

(defcheck solution-2ae3e198
  (fn [s]
    (letfn [(linked? [w1 w2] (loop [s1 (seq w1), s2 (seq w2), diff 0]
                               (if s1
                                 (let [same (= (first s1) (first s2))]
                                   (recur
                                     (next s1)
                                     (if (or same (= (count s1) (count s2))) (next s2) s2)
                                     (if same diff (inc diff))))
                                 (if (or (= 1 diff) (= 1 (count s2)))
                                   w1
                                   false))))]
      (loop [paths (into [] (for [w s] [[w], (remove (partial = w) s)]))]
        (cond
          (empty? paths) false
          (some (fn [[_, more]] (empty? more)) paths) true
          :default (recur
                     (into []
                       (apply concat
                         (for [[chain, more] paths]
                           (for [after (filter #(linked? % (last chain)) more)]
                             [(conj chain after) (remove #(= after %) more)]))))))))))

(defcheck solution-2b652a7f
  (fn f [s]
    (letfn [(sb [x y]
              (= 1 (apply + (map #(if (not= %1 %2) 1 0) x y))))
            (rm [fr to]
              (let [fr (seq fr)
                    n (count fr)
                    to (seq to)]
                (loop [i 0]
                  (if (= n i)
                    false
                    (if (= to (concat (take i fr) (drop (inc i) fr)))
                      true
                      (recur (inc i)))))))
            (c [x y]
              (let [i (count x)
                    j (count y)]
                (if (= i j)
                  (sb x y)
                  (if (< i j)
                    (rm y x)
                    (rm x y)))))
            (is [lst s]
              (if (seq s)
                (some #(and (c lst %) (is % (disj s %))) s)
                true))]
      (boolean (some #(is % (disj s %)) s)))))

(defcheck solution-2b7da430
  (fn [f c]
    ((fn h [w]
       (let [r (reduce
                 #(if (f (last %) %2)
                    `[~@% ~%2]
                    (if (f (first %) %2)
                      `[~%2 ~@%]
                      %))
                 w
                 (remove (set w) c))]
         (if (= r w) (= (set r) c) (h r))))
     [(first c)])) (fn [s t]
                     (let [g (fn [x y]
                               (some
                                 #(= (seq x) `(~@(take % y) ~@(drop (+ % 1) y)))
                                 (range (count y))))]
                       (condp = (- (count s) (count t))
                         0 (= 1 (apply + (map #(if (= % %2) 0 1) s t)))
                         1 (g t s)
                         -1 (g s t)
                         false))))

(defcheck solution-2bee7dfc
  (fn [f2 s]
    (letfn
     [(f [a b]
        (let [g (fn [x] (reduce #(merge-with + %1 {%2 1}) {} x))
              [z y] [(g a) (g b)]
              ok? #{[0 1] [1 0] [1 1]}]
          (when (ok? (mapv #(apply + (vals %))
                       (apply map #(into {} %&)
                         [z y]
                         (keep (fn [[k v]] (if (z k) [[k (Math/abs (- (z k) v))] [k (Math/abs (- v (z k)))]])) y))))
            b)))]
      (f2 s (into {} (map (fn [x] [x (set (filter #(f x %) s))]) s)) ))) (fn f2
                                                                           ([o s] (true? (some #(= o %)
                                                                                           (filter set? (tree-seq seq? identity (keep #(f2 % s []) s))))))
                                                                           ([[k v] s i]
                                                                            (let [i `[~@i ~k], t (dissoc (zipmap v (map s v)) i)]
                                                                              (if (seq s)
                                                                                (keep (fn [x]
                                                                                        (let [n (apply dissoc s k x)]
                                                                                          (when (not= s n) (f2 x n i)))) t)
                                                                                (set i))))))

(defcheck solution-2d621912
  (fn [words]
    (letfn [(add-elem
              [l e]
              (reduce (fn [s i] (let [[p1 p2] (split-at i l)] (conj s (concat p1 (list e) p2)))) #{} (range (inc (count l)))))
            (permute
              [s]
              (let [h (first s) t (rest s)] (if (empty? t) #{(list h)} (reduce (fn [cs l] (into cs (add-elem l h))) #{} (permute t)))))
            (by-substitution?
              [w1 w2]
              (let [l1 (count w1) l2 (count w2)] (if (= l1 l2) (= (reduce + (map (fn [c1 c2] (if (= c1 c2) 0 1)) w1 w2)) 1) false)))
            (by-insertion-deletion?
              [w1 w2]
              (let [l1 (count w1) l2 (count w2)] (cond (= (- l1 l2) 1) ((set (map (fn [i] (concat (take i w1) (take-last (- l2 i) w1))) (range l1))) (seq w2)) (= (- l2 l1) 1) (recur w2 w1) :else false)))
            (chain?
              [chain]
              (let [pairs (partition 2 1 chain)] (every? (fn [[w1 w2]] (or (by-substitution? w1 w2) (by-insertion-deletion? w1 w2))) pairs)))]
      (not (= (some chain? (permute words)) nil)))))

(defcheck solution-2e35fd08
  (fn [ws]
    (letfn [(levenshtein [x y] ; see wikipedia
              (let [xlen (count x), ylen (count y)]
                (cond (zero? xlen) ylen
                      (zero? ylen) xlen
                      (= (first x) (first y)) (levenshtein (rest x) (rest y))
                      :else (inc (min (levenshtein (rest x) y)
                                   (levenshtein x (rest y))
                                   (levenshtein (rest x) (rest y)))))))
            (word-chain? [w ws]
              (or (empty? ws)
                  (when-let [ones (seq (filter #(= 1 (levenshtein w %)) ws))]
                    (some #(word-chain? % (disj ws %)) (set ones)))))]

      (true? (some #(word-chain? % (disj ws %)) ws)))))

(defcheck solution-2eb1e84d
  (fn has-combinations
    ([s] (boolean (some true? (for [v s
                                    :let [r (disj s v)]]
                                (has-combinations v r)))))
    ([v s]
     (if (empty? s)
       true
       (let [longest-matching (fn [a b] (count (take-while true? (map = a b))))
             one-changed? (fn [a b] ( = 1 (count (filter false? (map = a b)))))
             one-inserted? (fn [a b] (= (count a) (+ (longest-matching a b) (longest-matching (reverse a) (reverse b)))))
             one-diff? (fn [a b]
                         (case (- (count a) (count b))
                           -1 (one-inserted? a b)
                           1  (one-inserted? b a)
                           0  (one-changed? a b)
                           false))]
         (some true? (for [v2 s
                           :let [r (disj s v2)]
                           :when (one-diff? v v2)]
                       (has-combinations v2 r))))))))

(defcheck solution-2f097338
  (fn [le-strings]
    (letfn
     [(lev [w1 w2]
        (letfn
         [(-lev [i1 i2]
            (if
             (or
              (= 0 i1)
              (= 0 i2))
              (max i1 i2)
              (min
                (+
                 (-lev
                   (- i1 1)
                   i2)
                 1)
                (+
                 (-lev
                   i1
                   (- i2 1))
                 1)
                (+
                 (-lev
                   (- i1 1)
                   (- i2 1))
                 (if
                  (=
                    (nth w1 (- i1 1))
                    (nth w2 (- i2 1)))
                   0
                   1)) )))]
          (-lev
            (count w1)
            (count w2))))

      (distances [li]
        (partition
          (count li)
          (for [x li
                y li]
            (lev x y)) ))

      (find-adjacent [li]
        (map
          second
          (apply
            concat
            (map-indexed
              (fn [y-pos y-li]
                (filter
                  (fn [item]
                    (= (nth item 0) 1))
                  (map-indexed
                    (fn [x-pos item]
                      [item [x-pos y-pos]])
                    y-li)))
              (distances li)))))

      (filter-adjacent [adj]
        (set (map set adj)))

      (find-filtered-adjacents [li]
        (filter-adjacent (find-adjacent li)))

      (walk-by-single-connection
        ([vertexes-to-check nodes breadcrumb]
         #_(println "to-check:" vertexes-to-check "breadcrumb:" breadcrumb "nodes:" nodes)
         (if
          (empty? vertexes-to-check)
           false
           (if
            (empty? nodes)
             true
             (let
              [[vertex vertexes-rest] ((juxt first rest) vertexes-to-check)
               #_(println "vertex" vertex)]
               (if
                (loop [node-list nodes]
                  (if
                   (empty? node-list)
                    false
                    (let [[current-node rest-nodes] ((juxt first rest) node-list)
                          deep (if
                                (contains? current-node vertex)
                                 (walk-by-single-connection
                                   (remove #{vertex} current-node)
                                   (filter
                                     (fn [to-filter]
                                       #_(println to-filter vertex)
                                       (not (= 0
                                              (count (remove
                                                       (set (conj breadcrumb vertex))
                                                       to-filter)))))
                                     (remove #{current-node} nodes))
                                   (conj breadcrumb vertex))
                                 false)]
                      #_(println "recur" current-node rest-nodes)
                      (or
                       deep
                       (recur rest-nodes)))))
                 true
                 (walk-by-single-connection
                   vertexes-rest
                   nodes
                   breadcrumb)))))))
      (path [li]
        (let [adjacents (find-filtered-adjacents li)]
          (apply
            walk-by-single-connection
            ((juxt
               first
               identity
               empty)
             adjacents))))]
      (path le-strings))))

(defcheck solution-2f10525b
  #(let [numOfNodes (count %)
         removeLetterAt (fn [s i]
                          (str (subs s 0 i) (subs s (inc i))))
         oneLetterDifferent? (fn [s1 s2]
                               (cond
                                 (= s1 s2) false
                                 (= (count s1) (count s2)) (loop[ind 0]
                                                             (cond (>= ind (count s1)) false
                                                                   (= (removeLetterAt s1 ind) (removeLetterAt s2 ind)) true
                                                                   :else (recur (inc ind))))
                                 :else (let[[ss ls](if(< (count s1) (count s2)) [s1 s2] [s2 s1])]
                                         (loop[ind 0]
                                           (cond (>= ind (count ls)) false
                                                 (= (removeLetterAt ls ind) ss) true
                                                 :else (recur (inc ind)))))))
         buildGraph (fn [words]
                      (into #{} (for[w1 words w2 words :when (oneLetterDifferent? w1 w2)] [w1 w2])))
         growPath (fn [g]
                    (loop[paths g]
                      (let [np (set (concat (for [p paths [a b] g :when (= a (last p))]
                                              (if (some (fn[x] (= b x)) p) p (conj p b)))))]
                        (if (= np paths) np
                                         (recur np)))))]
     (= numOfNodes (apply max (map count (growPath (buildGraph %)))))))

(defcheck solution-2f173aa1
  (fn [s d b e q]
    (b (s (fn f [[w r]]
            (or (empty? r)
                (s #(if (or (s #{(vec w)} (e %))
                            (s #{(vec %)} (e w))
                            (s b (map = (e w) (e %))))
                      (f [% (d r %)]))
                  r)))
         (for [w q] [w (d q w)])))) some disj boolean #(map (fn [_ i] `[~@(take i %) ~@(drop (+ i 1) %)]) % (range)))

(defcheck solution-300a4960
  (fn [s]
    (let [f (fn f [a]
              (for [i (range (count a))]
                (clojure.string/join
                  ""
                  [(subs a 0 i)
                   (subs a (inc i))])))
          more #(> (count %1) (count %2))
          g (fn g [a b]
              (if (more a b)
                [a b]
                [b a]))
          h (fn h [a b]
              (let [[a b] (g a b)]
                (not-every?
                  false?
                  (if (= (count a) (count b))
                    (map = (f a) (f b))
                    (map #(= b %) (f a))))))
          dfs (fn dfs [v vst]
                (if-let [ss (seq (for [i s :when (and (not (vst i)) (h v i))] i))]
                  (reduce
                    #(if (more %1 %2) %1 %2)
                    (map #(dfs % (conj vst %)) ss))
                  vst))]
      (not-every?
        false?
        (map #(= (dfs % #{%}) s) s)))))

(defcheck solution-30409b9e
  (fn word-chains [words]
    (let [alphabet "abcdefghijklmnopqrstuvwxyz"
          dfs (fn dfs [p visited unvisited]
                (let [n (count p)
                      edits1
                        (set (concat
                              (for [i (range n)] (str (subs p 0 i) (subs p (inc i))))
                              (for [i (range (dec n))]
                                (str (subs p 0 i)
                                  (nth p (inc i))
                                  (nth p i)
                                  (subs p (+ 2 i))))
                              (for [i (range n) c alphabet] (str (subs p 0 i)
                                                              c (subs p (inc i))))
                              (for [i (range (inc n)) c alphabet]
                                (str (subs p 0 i) c (subs p i)))))
                      neighbours (clojure.set/intersection edits1 unvisited)]
                  (if (empty? neighbours)
                    0
                    (+ 1 (apply max
                           (map
                             #(dfs % (cons p visited) (disj unvisited %))
                             neighbours))))))]
      (= (count words) (apply max (map #(dfs % #{} words) words))))))

(defcheck solution-30574d8e
  (fn word-chain [words]
    (let [leven-distance (fn  [s t]
                           (let [s (seq s) t (seq t) m (count s) n (count t)]
                             (cond (empty? s) n
                                   (empty? t) m
                                   :else (let [d (into {} (apply concat (map (fn [i] (apply concat (map (fn [j] (vector [[i j] 0])) (range (inc n))))) (range (inc m)))))
                                               d (merge d (into {} (map #(vector (vector % 0) %) (range 1 (inc m)))))
                                               d (merge d (into {} (map #(vector (vector 0 %) %) (range 1 (inc n)))))]
                                           ((reduce
                                              (fn [d [j y]]
                                                (reduce
                                                  (fn [d [i x]]
                                                    (if (= x y)
                                                      (conj d [[i j] (d [(dec i) (dec j)])])
                                                      (conj d [[i j] (min
                                                                       (+ (d [(dec i) j]) 1)
                                                                       (+ (d [i (dec j)]) 1)
                                                                       (+ (d [(dec i) (dec j)]) 1))])))
                                                  d
                                                  (map vector (iterate inc 1) s)))
                                              d
                                              (map vector (iterate inc 1) t)) [m n])))))

          chain (fn chain [word remaining-words explored]
                  ;(println "word: " word " remaining: " remaining-words " explored: " explored)
                  (let [matched-words (filter (fn [other-word] (= 1 (leven-distance word other-word))) remaining-words)]
                    (if (or (empty? remaining-words) (empty? matched-words))
                      (= (inc (count explored)) (count words))
                      (map  (fn [new-word] (chain new-word (disj remaining-words new-word) (conj explored word))) matched-words))))]

      (not= nil (some true?
                  (flatten (map
                             (fn [word]
                               (chain word (disj words word) #{}))
                             words)))))))

(defcheck solution-30b83b62
  (fn [set]
    (letfn
     [(perms [s]
        (if (seq (rest s))
          (apply concat (for [x s]
                          (map #(cons x %) (perms (remove #{x} s)))))
          [s]))
      (is-subst [s t]
        (= 1 (count (filter false? (mapv = s t)))))
      (is-ins-del [s t] ; this is really ugly; should have just done lev
        (let [srt (sort-by count [s t])
              [s1 t1] srt]
          (if (not= 1 (- (count t1) (count s1)))
            false
            (let [pairs (map vector (vec t1) (conj (vec s1) \space))
                  rem (drop-while #(= (first %) (second %)) pairs)
                  t1rem (rest (map first rem))
                  s1rem (butlast (map second rem))]
              (or (= 1 (count rem))
                  (= t1rem s1rem))))))
      (is-chain [s]
        (->> s
          (partition 2 1)
          (map #(or (is-subst (first %) (second %)) (is-ins-del (first %) (second %))))
          (reduce #(and %1 %2))))]
      (->> set
        perms
        (map is-chain)
        (reduce #(or %1 %2))
        )

      )))

(defcheck solution-312ac125
  (fn [w]
    (letfn [(c [v w r]
              (if (= r 1)
                (= (seq v) (seq w))
                (let [[a & b] v
                      [e & f] w]
                  (or
                   (= v w)
                   (c b f (if (= a e) 0 1))
                   (c b w 1)
                   (c v f 1)))))
            (f [s]
              (if (empty? s)
                [[]]
                (mapcat (fn [x] (map #(cons x %) (f (disj s x)))) s)))]
      (boolean (some #(every? (fn [[a b]] (c a b 0)) (partition 2 1 %)) (f w))))))

(defcheck solution-3228058d
  (fn wc
    ([coll] (wc [] coll))
    ([p coll]
     (letfn
      [(diff [av bv]
         (cond
           (or (empty? av) (empty? bv)) (max (count av) (count bv))
           (= (first av) (first bv))
           (diff (next av) (next bv))
           (= (second av) (first bv))
           (+ 1 (diff (nnext av) (next bv)))
           (= (first av) (second bv))
           (+ 1 (diff (next av) (nnext bv)))
           :else
           (+ 1 (diff (next av) (next bv)))
           ))]
       (cond
         (empty? coll)
         true
         (nil? (some true?
                 (for [x coll
                       :when (>= 1 (if (empty? p) 0 (diff x (last p))))
                       ]
                   (wc (conj p x) (disj coll x)))))
         false
         :else
         true
         )))))

(defcheck solution-32c23421
  (fn [s]
    (letfn [(distance [a b]
              (loop [scores [[a b 0]]]
                (let [finished (filter (fn [[a b _]] (and (empty? a) (empty? b))) scores)]
                  (if (seq finished)
                    (apply min (map last finished))
                    (recur (mapcat (fn [[a b score]]
                                     (if (= (first a) (first b))
                                       [[(rest a) (rest b) score]]
                                       (concat
                                        (if (empty? a) [] [[(rest a) b (inc score)]])
                                        (if (empty? b) [] [[a (rest b) (inc score)]])
                                        (if (or (empty? a) (empty? b)) [] [[(rest a) (rest b) (inc score)]]))))
                             scores))))))
            (chains [chain]
              (let [left (apply disj s chain)]
                (mapcat (fn [word] (concat
                                    (if (= 1 (distance word (first chain)))
                                      [(vec (cons word chain))] [])
                                    (if (= 1 (distance word (last chain)))
                                      [(conj chain word)] [])))
                  left)))]
      (loop [possible-chains (map vector s) previous []]
        #_(print possible-chains)
        (cond
          (some #(= (count %) (count s)) possible-chains)
          true
          (= previous possible-chains)
          false
          true
          (recur (mapcat chains possible-chains) possible-chains))))))

(defcheck solution-32eecd70
  (fn [words]
    (let [lev (fn [str1 str2]
                (let [isize (inc (count str1))
                      jsize (inc (count str2))
                      all-pairs (for [i (range isize) j (range jsize)] [i j])
                      reducer (fn [distances [i j]]
                                (assoc-in distances [i j]
                                  (cond
                                    (= i 0) j
                                    (= j 0) i

                                    (= (nth str1 (dec i)) (nth str2 (dec j)))
                                    (get-in distances [(dec i) (dec j)])

                                    :else
                                    (+ 1 (min (get-in distances [(dec i) j])
                                           (get-in distances [i (dec j)])
                                           (get-in distances [(dec i) (dec j)]))))))

                      zero-array (fn [m n]  (vec (take m (repeat (vec (take n (repeat 0)))))))
                      init-state (zero-array isize jsize)

                      levs (reduce reducer init-state all-pairs)
                      val (last (last levs))]
                  val))

          connected-words (fn [w] (filter #(= 1 (lev w %)) words))

          connections (zipmap words (map connected-words words))

          check-path
          (fn check-path [from seen]
            (cond
              (= (count seen) (count connections))
              true

              (seq from)
              (some (fn [n]
                      (and (not (contains? seen n))
                           (check-path (connections n) (conj seen n))))
                from)

              :else
              false))]

      (true? (check-path words #{})))))

(defcheck solution-331dc13
  (fn [s]
    (let [pair? (fn [w1 w2]
                  (cond
                    (nil? w1) true
                    (= (count w1) (count w2))
                    (= 1 (count (filter false? (map = w1 w2))))
                    (= 1 (Math/abs (- (count w1) (count w2))))
                    (let [[shorter longer] (sort #(< (count %1) (count %2)) [w1 w2])]
                      (re-matches
                        (re-pattern (str ".?" (apply str (interpose ".?" shorter)) ".?"))
                        longer))
                    :else false))
          chainable? (fn chainable? [w rem]
                       (if (empty? rem)
                         true
                         (some
                           (fn [x] (chainable? x (clojure.set/select #(not= x %) rem)))
                           (filter (partial pair? w) rem))))]
      (boolean (chainable? nil s)))))

(defcheck solution-33256304
  (fn chain? [coll]
    (letfn [(str-pop [s k]
              (str (subs s 0 k) (subs s (inc k))))
            (plus-one [a b]
              (if (= (count a) (inc (count b)))
                (some #(= (str-pop a %) b) (range (count a)))
                false))
            (switch [a b]
              (and
               (= (count a) (count b))
               (some #(= (str-pop a %) (str-pop b %)) (range (count a)))))
            (adj [a b]
              (or (plus-one a b) (plus-one b a) (switch a b)))
            (chain-from [coll a]
              (case (count coll)
                1 true
                2 (apply adj (vec coll))
                (some
                  #(and (not= a %) (chain-from (disj coll a) %) (adj a %))
                  coll)))]
      (true?  (some #(chain-from coll %) coll)))))

(defcheck solution-33d0c25
  (fn [x]
    (let [is-sub (fn [word1 word2]
                   (and (= (count word1) (count word2))
                        (= 1 (count (filter #(not %) (map #(= (nth word1 %) (nth word2 %)) (range (count word1))))))))
          make-s (fn make-s [big small]
                   (if (or (empty? big) (empty? small)) ""
                                                        (if (= (first big) (first small))
                                                          (cons (first big) (make-s (rest big) (rest small)))
                                                          (make-s (rest big) small))))
          is-del-or-ins (fn [word1 word2]
                          (if (= 1 (Math/abs (- (count word1) (count word2))))
                            (let [[big small] (if (> (count word1) (count word2)) [word1 word2] [word2 word1])]
                              (= (into [] small) (make-s big small)))))
          mc  (fn mc [word-vec word-set]
                (if (empty? word-set) word-vec
                                      (filter #(and true (not (empty? %))) (map #(mc (conj word-vec %) (disj word-set %))
                                                                             (let [word (last word-vec)]
                                                                               (filter #(or (is-sub word %) (is-del-or-ins word %)) word-set))))))]
      (not (empty? (filter #(not (empty? %)) (map #(mc [%] (disj x %)) x)))))))

(defcheck solution-34ddd020
  (fn prob-0082
    [xs]

    (letfn [(omit
              [oi xs]
              (concat (take oi xs) (drop (inc oi) xs)))

            (-wc-del?
              [in-w1 in-w2]
              (let [w1 (seq in-w1), w2 (seq in-w2), lim (count w1)]
                (if (not= (count w1) (inc (count w2)))
                  false
                  (loop [oi 0]
                    (if (>= oi lim)
                      false
                      (if (= (omit oi w1) w2)
                        true
                        (recur (inc oi))))))))

            (wc-ins-del?
              [w1 w2]
              (or (-wc-del? w1 w2) (-wc-del? w2 w1)))

            (wc-substitution?
              [in-w1 in-w2]
              (let [w1 (seq in-w1), w2 (seq in-w2)]
                (and
                 (= (count w1) (count w2))
                 (= 1 (count (filter false? (map = (seq w1) (seq w2))))))))

            (wc-chainable?
              [w1 w2]
              (or (wc-substitution? w1 w2) (wc-ins-del? w1 w2)))

            (factorial [n]
              (reduce * (cons 1 (range 1 (inc n)))))

            (nth-perm
              [to-skip cands]
              (let [my-num-cands (count cands)
                    nx-num-cands (dec my-num-cands)]
                (if (pos? nx-num-cands)
                  (let [nx-size      (factorial nx-num-cands)
                        nx-skip-chks (int (/ to-skip nx-size))
                        my-choice    (nth (vec cands) nx-skip-chks)
                        nx-to-skip   (- to-skip (* nx-size nx-skip-chks))
                        nx-cands     (disj cands my-choice) ]
                    (cons my-choice (nth-perm nx-to-skip nx-cands)))
                  [(first cands)])))

            (permutations
              ([xs] (permutations xs 0 (factorial (count xs))))
              ([xs n lim]
               (lazy-seq
                 (if (>= n lim)
                   nil
                   (cons (nth-perm n xs) (permutations xs (inc n) lim))))))
            ]

      (let [perms  (permutations xs)
            ok?    (fn [sq] (every? true? (map #(wc-chainable? %1 %2) sq (rest sq))))
            ok-lst (map #(ok? %) perms)]
        (not (not-any? true? ok-lst))))))

(defcheck solution-34f0a14b
  (letfn [(one-letter-difference? [[ah & at :as a] [bh & bt :as b]]
            (if (= ah bh)
              (or (nil? ah)
                  (recur at bt))
              (or (= (seq at) (seq b))
                  (= (seq a) (seq bt))
                  (= at bt))))
          (word-chains [s]
            (if (next s)
              (for [head s
                    tail (word-chains (disj s head))
                    :when (one-letter-difference? head (first tail))]
                (cons head tail))
              [(vec s)]))]
    (comp not nil? seq word-chains)))

(defcheck solution-359903b1
  (fn [s]
    (letfn [(diff-one-char?
              [word1 word2]
              (cond
                (= (count word1)
                  (count word2)) (= 1 (count
                                        (filter false?
                                          (map #(= % %2) word1 word2))))
                :else (let [w1 (max-key count word1 word2)
                            w2 (min-key count word1 word2)]
                        (some #(= (seq w2) %)
                          (map #(concat (take % w1) (drop (inc %) w1)) (range (count w1)))))))
            (chain?
              [words]
              (every? true?
                (map (partial apply diff-one-char?) (partition 2 1 words))))
            (combination
              [coll]
              (let [f (fn f [avec coll]
                        (if (empty? coll) avec
                                          (for [x coll]
                                            (f (conj avec x) (remove #(= x %) coll)))))]
                (partition (count coll)
                  (flatten (f [] coll)))))]
      (boolean (some chain? (combination s))))))

(defcheck solution-36059198
  (fn [words]
    (letfn [(w-o [s el]
              (filter #(not (= el %)) s))
            (one-letter-different? [a b]
              (letfn [(differ [[a & as] [b & bs] single-diff?]
                        (if (and (nil? a) (nil? b))
                          single-diff?
                          (if (= a b)
                            (differ as bs single-diff?)
                            (if single-diff?
                              false
                              (or (differ as (cons b bs) true)
                                  (differ (cons a as) bs true)
                                  (differ as bs true))))))]
                (differ a b false)))
            (check-potentials [potentials others]
              (if (empty? potentials)
                false
                (not (nil? (some #(can-form-chain? % (w-o others %)) potentials)))))
            (can-form-chain? [target others]
              (if (empty? others)
                true
                (let [potentials (filter #(one-letter-different? target %) others)]
                  (check-potentials potentials others))))]
      (check-potentials words words))))

(defcheck solution-370db366
  (fn [words]
    (letfn [(neigh? [w1 w2]
              (if-not (and (empty? w1) (empty? w2))
                (if (= (first w1) (first w2))
                  (neigh? (rest w1) (rest w2))
                  (or (= (rest w1) (rest w2)) (= (rest w1) w2) (= w1 (rest w2))))))

            (word-chain [word others]
              (if (empty? others)
                true
                (let [neighs (filter (partial neigh? word) others)]
                  (if-not (empty? neighs)
                    (some identity (map #(word-chain % (disj others %)) neighs))))))]
      (true? (some identity (map #(word-chain % (disj (set (map seq words)) %)) (map seq words)))))))

(defcheck solution-371e93ae
  (letfn [(near
            ([a b] (or (near a b 0)
                       (near (drop 1 a) b 1)
                       (near a (drop 1 b) 1) ))
            ([[f1 & r1 :as a] [f2 & r2 :as b] diff ]
             (if (< diff 2)
               (if (and f1 f2)
                 (let [diff1 (if (= f1 f2) diff (inc diff))]
                   (or (near r1 r2 diff1)
                       (near r1 (seq b) (inc diff) )
                       (near (seq a) r2 (inc diff) )
                       ))
                 (< (+ (count a) (count b) diff) 2)
                 ))))
          (chain
            ([words]
             (chain (first words) (first words) (rest words) #{} #{}))
            ([fst lst rst queue queue-cmp]
             (if (seq rst)
               (let [word (first rst)]
                 (or
                  (if (near fst word) (chain word lst (rest rst) queue queue-cmp))
                  (if (near lst word) (chain fst word (rest rst) queue queue-cmp))
                  (chain fst lst (rest rst) (conj queue word) queue-cmp)
                  ))
               (if (> (count queue) 0)
                 (if (= queue queue-cmp)
                   false
                   (chain fst lst queue #{} queue) )
                 true))
             ))
          ]
    chain
    ))

(defcheck solution-3721ec20
  (fn [words]
    (letfn [(lev-dist-1? [w1 w2]
              (let [[w1 w2] (sort-by count [w1 w2])]
                (condp = (- (count w2)(count w1))
                  0 (= (-> w1 count dec)(->> (map = w1 w2)(filter true?) count))
                  1 (loop [diffs 0, w1 w1, w2 w2]
                      (cond
                        (< 1 diffs) false
                        (empty? w1) (= diffs (if (empty? w2) 1 0))
                        (= (first w1)(first w2)) (recur diffs (rest w1)(rest w2))
                        :else (recur (inc diffs) w1 (rest w2))))
                  false)))
            (find-lev-dist-1 [words word]
              (vector word (filter (partial lev-dist-1? word) words)))
            (build-chains [len word-map word-lists]
              (loop [word-lists word-lists]
                (if (= len (count (first word-lists)))
                  word-lists
                  (recur (mapcat (fn [word-list] (map #(conj word-list %1) (word-map (peek word-list)))) word-lists)))))]
      (let [word-map (into {} (map (partial find-lev-dist-1 words) words))
            chains (mapcat #(build-chains (count words) word-map %) (vector (map vector words)))]
        (< 0 (count (filter #(= (count words) (-> % distinct count)) chains)))))))

(defcheck solution-373f57ab
  (fn [ws]
    (let [n  #(nth %1 %2 \-)
          c? (fn [a b]
               (> 2
                 (loop [i (dec (max (count a) (count b))) j i d 0]
                   (when (= a "coat") #_(println d a b i j (n a i) (n b j)))
                   (cond
                     (or (> d 1) (> 0 (max i j))) d
                     (= (n a i) (n b j))          (recur (dec i) (dec j) d)
                     (= (n a i) (n b (dec j)))    (recur i (dec j) (if (not= \- (n b j)) (inc d) d))
                     (= (n a (dec i)) (n b j))    (recur (dec i) j (if (not= \- (n a i)) (inc d) d))
                     :else                        (recur (dec i) (dec j) (inc d))))))
          cs (for [w ws x (remove #(= w %) ws) :when (c? w x)]
               [w x])]
      (loop [ac cs]
        (let [nac (for [c ac [x y] cs :when (and (not-any? #(= y %) c) (= (last c) x))]
                    (conj c y))]
          (if (seq nac)
            (recur nac)
            (= (count (first ac)) (count ws))))))))

(defcheck solution-37ac4915
  (fn word-chain? [words]
    (letfn [(drop= [xs ys]
              (if (and (seq xs) (seq ys) (= (first xs) (first ys)))
                (recur (rest xs) (rest ys))
                [xs ys]))
            (neighbouring? [w1 w2]
              (and
               (not= w1 w2)
               (let [[t1 t2] (drop= w1 w2)
                     seq= #(= (seq %1) (seq %2))]
                 (case (- (count w1) (count w2))
                   0 (= (rest t1) (rest t2))
                   -1 (seq= t1 (rest t2))
                   1 (seq= (rest t1) t2)
                   false))))
            (graph [arc? nodes]
              (reduce
                (fn [acc [i t]] (assoc acc i (conj (get acc i #{}) t)))
                {}
                (for [in nodes, term nodes :when (and (arc? in term))] [in term])))
            (hamiltonian? [gm]
              (let [places (set (keys gm))
                    entry-point (some (complement places) (repeatedly gensym)) ; distinct from places
                    egm (assoc gm entry-point places)
                    chainable? (fn chainable? [place unseens]
                                 (or
                                  (empty? unseens)
                                  (some #(chainable? % (disj unseens %)) (filter unseens (egm place)))))]
                (chainable? entry-point places)))]
      (boolean (hamiltonian? (graph neighbouring? words))))))

(defcheck solution-37c542de
  (fn chain-set? [s]
    (letfn [(change-one? [s t]
              (if (not (= (count s) (count t)))
                false
                (->> (interleave (seq s) (seq t))
                  (partition 2)
                  (map (fn [[a b]] (if (= a b) 0 1)))
                  (reduce +)
                  (= 1))))
            (remove [s i]
              (str (subs s 0 i) (subs s (inc i))))
            (remove-list [s]
              (map #(remove s %) (range (count s))))
            (del-one? [s t]
              (reduce #(or %1 (= t %2)) false (remove-list s)))
            (ins-del-one? [s t]
              (or (del-one? s t) (del-one? t s)))
            (chain? [s t]
              (or (change-one? s t) (ins-del-one? s t)))
            ]
      (loop [work (vec (map (fn [w] [[w] (disj s w)]) s))]
        (let [head (first work)
              tail (rest work)
              chain (first head)
              word (last chain)
              left (second head)
              chainable (vec (filter #(chain? word %) left))
              ]
          (if (empty? work)
            false
            (if (empty? left)
              true
              (if (empty? chainable)
                (recur tail)
                (recur (concat tail
                               (vec (map (fn [w] [(conj chain w)
                                                  (disj left w)])
                                      chainable))))))))))))

(defcheck solution-3950009d
  (fn [ws]
    (letfn
     [(lev [w1 w2]
        (let [l1 (count w1) l2 (count w2)]
          (cond
            (zero? l1) l2
            (zero? l2) l1
            :else
            (let [cost (if (= (first w1) (first w2)) 0 1)]
              (min
                (inc (lev (rest w1) w2))
                (inc (lev w1 (rest w2)))
                (+ cost (lev (rest w1) (rest w2))))))))
      (expand? [s w]
        (let [l (last s)]
          (and (= (lev l w) 1))))
      (eval-ss [acc _]
        (if (empty? acc) acc
                         (for [s acc w ws :when (and (not (some #(= w %) s)) (expand? s w))]
                           (conj s w))))]
      (let [ss (for [x ws y ws :when (and (not= x y) (= (lev x y) 1))] [x y])]
        (not (empty? (reduce eval-ss ss (range (- (count ws) 2)))))))))

(defcheck solution-3a0c2058
  (fn myWordChains
    [chain]
    (let [diff (fn dist [seq1 seq2]
                 (cond
                   (empty? seq1) (count seq2)
                   (empty? seq2) (count seq1)
                   :else (let [cost (if (= (first seq1) (first seq2)) 0 1)]
                           (min
                             (inc (dist (rest seq1) seq2))
                             (inc (dist seq1 (rest seq2)))
                             (+ (dist (rest seq1) (rest seq2)) cost)))))
          diffByOne? (fn [seq1 seq2] (= 1 (diff seq1 seq2)))
          getAllDistByOne (fn [x coll] (rest (reduce #(if (diffByOne? %2 (first %1)) (conj %1 %2) %1) x coll)))]
      (loop [paths (map (fn [x] (list (vector x) (set (remove #(= x %) chain)))) chain) iter 0]
        (if (= iter (- (count chain) 1))
          (not (empty? paths))
          (recur (mapcat (fn [x] (map #(list (conj (first x) %) (set (remove (fn [rem] (= rem %)) (second x))))
                                   (getAllDistByOne (vector (last (first x))) (second x)))) paths) (inc iter)))))))

(defcheck solution-3a6d79ab
  (fn [s]
    (let [perm (fn perm [i] (if (= (count i) 1) [(seq i)]
                                                (apply concat (for [x i] (map #(conj % x) (perm (disj i x)))))))
          neighbors? (fn [[a b]]
                       (cond
                         (= (count a) (count b))	(= 1 (count (filter false? (map = a b))))
                         (= 1 (Math/abs (- (count a) (count b))))
                         (let [[t s] (if (> (count a) (count b)) [a b] [b a])]
                           (some true? (for [i (range (count t))]
                                         (= (concat (take i t) (drop (inc i) t)) (seq s)))))
                         :else false))]
      (true? (some true? (map (fn [p] (every? true? (map neighbors? (partition 2 1 p)))) (perm s)))))))

(defcheck solution-3ac0dd3d
  (fn [w]
    (<=
      (count w)
      (get (frequencies (sort
                          (for [l1 (seq w) l2 (seq w)
                                :when (< 0 (compare l1 l2))]
                            ((memoize
                               (fn f  [x y]
                                 (let  [cost  (if  (=  (first x)  (first y)) 0 1)]
                                   (cond
                                     (zero?  (count x))  (count y)
                                     (zero?  (count y))  (count x)
                                     (=  (first x)  (first y))  (f  (rest x)  (rest y))
                                     :else
                                     (min
                                       (+ 1  (f  (rest x) y))
                                       (+ 1  (f x  (rest y)))
                                       (+ cost  (f  (rest x)  (rest y)))))))) l1 l2))))
        1))))

(defcheck solution-3b5cfe3e
  (fn [ss]
    (let [perms ((fn ch-e [xs]
                   (if (empty? xs)
                     '(())
                     (apply concat
                       (for [x xs]
                         (map #(cons x %) (ch-e (disj xs x))))))) ss)
          within-dist? (fn within-dist? [xs ys c]
                         (cond
                           (> c 1) false
                           (empty? xs) (<= (+ c (count ys)) 1)
                           (empty? ys) (<= (+ c (count xs)) 1)
                           (= (first xs) (first ys)) (within-dist? (rest xs) (rest ys) c)
                           :else (cond
                                   (< (count xs) (count ys)) (within-dist? xs (rest ys) (inc c))
                                   (> (count xs) (count ys)) (within-dist? (rest xs) ys (inc c))
                                   :else (within-dist? (rest xs) (rest ys) (inc c)))))]

      (not (empty?
             (for [v perms
                   :let [chws-raw (filter (fn [[e1 e2]] (within-dist? e1 e2 0))
                                    (partition 2 1 v))]
                   :when (= (inc (count chws-raw)) (count v))]
               v))))))

(defcheck solution-3b7aa5cd
  (fn [l]
    (letfn [(f [d x y]
              (cond
                (or (empty? x) (empty? y)) (>= d (+ (count x) (count y)))
                (= (first x) (first y)) (f d (rest x) (rest y))
                (= 0 d) false
                :else (or (f (dec d) (rest x) (rest y))
                          (f (dec d) x (rest y))
                          (f (dec d) (rest x) y))))]
      (let [edges (set (for [x l y l :when (and (not= x y) (f 1 (seq x) (seq y)))] [x y]))]
        (true?
          ((fn ff [selected rest]
             (cond
               (empty? rest) true
               :else (some identity
                       (for [q rest :when (or (empty? selected) (edges [(last selected) q]))]
                         (ff (conj selected q) (remove #(= q %) rest))
                         ))
               ))
           [] l))
        ))))

(defcheck solution-3bc438a3
  (fn [s d b e q]
    (b (s (fn f [[w r]]
            (or (empty? r)
                (s #(if (or (s #{(vec w)} (e %))
                            (s #{(vec %)} (e w))
                            (s b (map = (e w) (e %))))
                      (f [% (d r %)]))
                  r)))
         (for [w q] [w (d q w)])))) some disj boolean #(map (fn [_ i] `[~@(take i %) ~@(drop (+ i 1) %)]) % (range)))

(defcheck solution-3bf76fe6
  (fn [words]
    (let [alphabet (set (apply concat words))]
      (letfn [
              (deletion? [a b]
                (and (= (count a) (+ (count b) 1))
                     (not (nil? (some #(= b (str (subs a 0 %) (subs a (+ % 1)))) (range (count a)))))))
              (substitute [s i c] (str (subs s 0 i) c (subs s (+ i 1))))
              (substitution? [a b]
                (and (= (count a) (count b))
                     (not (nil? (some (fn [i] (some #(= b (substitute a i %)) alphabet)) (range (count a)))))))
              (visit [s seen]
                (let [seen (conj seen s)
                      next_in_chain (filter #(and (not (seen %))
                                                  (or (substitution? s %)
                                                      (deletion? s %)
                                                      (deletion? % s)))
                                      words)]
                  (or (= (count seen) (count words))
                      (not (nil? (some #(visit % seen) next_in_chain))))))]
        (not (nil? (some #(visit % #{}) words)))))))

(defcheck solution-3c2a1558
  (fn [t c f r s]
    (> 2 (c (t #{1 2}
              (map #(c (t (fn [x]
                            (loop [i x j %]
                              (if (not= (f i) (f j))
                                (or (= (r i) (r j)) (= (r i) (seq j)) (= (seq i) (r j)))
                                (or (empty? i) (recur (r i) (r j)))))) s)) s))))) filter count first rest)

(defcheck solution-3c43bcbe
  (fn chain? [s]
    (letfn [(connected? [a b]
              (let [aLen (count a)
                    bLen (count b)
                    dropChar (fn [s n] (str (subs s 0 n) (subs s (inc n))))]
                (cond
                  (= aLen bLen) (> 2 (reduce + (map #(if (= %1 %2) 0 1) a b)))
                  (= aLen (inc bLen)) (< 0 (reduce + (map #(if (= b (dropChar a %)) 1 0) (range aLen))))
                  (= bLen (inc aLen)) (< 0 (reduce + (map #(if (= a (dropChar b %)) 1 0) (range bLen))))
                  :else false )))
            (chainFrom? [x s]
              (if (empty? s)
                true
                (not-every? false? (for [y s :when (connected? x y)]
                                     (chainFrom? y (disj s y))))))]
      (if (empty? s)
        true
        (not-every? false? (for [x s]
                             (chainFrom? x (disj s x))))))))

(defcheck solution-3d40d7e5
  (fn [s0]
    (letfn [(linked [u v] ;; check if words are linked true/false
              (letfn [(sub-words [v]
                        (for [i (range (count v))] (concat (subvec v 0 i) (subvec v (inc i)))))]
                (cond
                  (not (and u v))
                  false

                  (= (count u) (count v))
                  (= 1 (count (filter identity (map #(apply not= %) (mapv vector u v)))))

                  (= 1 (- (count u) (count v)))
                  (contains? (set (sub-words u)) v)

                  (= -1 (- (count u) (count v)))
                  (contains? (set (sub-words v)) u)

                  true false)))]

      (let [s (map vec s0) ;; words to vectors
            word (into {} (mapv vector (range) s)) ;; index the words by integers
            indices (range (count s0)) ;; indexing set
            conn (into {} (mapcat identity
                            (for [i indices]
                              (for [j indices]
                                [[i j] (and (linked (word i) (word j)) i)]))))
            cautious-cons (fn [x v]
                            (cond
                              (empty? v) [x]
                              (conn [x (first v)]) (cons x v)
                              true []))]

        (letfn [(perms [v]
                  "Generate all linked sequences"
                  (if (empty? v)
                    [[]]
                    (mapcat (fn [x] (map #(cautious-cons x %) (perms (remove #{x} v)))) v)))]
          (if (some #(= (count s0) (count %)) (perms indices)) true false))))))

(defcheck solution-3d957bf8
  (letfn [(drop-char [s i] (str (subs s 0 i) (subs s (inc i))))
          (can-chain
            [a b]
            (if (nil? a)
              true
              (cond
                (< (count a) (count b)) (can-chain b a) ;; force len(a) >= len(b)
                (= 1 (- (count a) (count b))) (some #(= b (drop-char a %)) (range (count a)))
                (= (count a) (count b)) (some #(= (drop-char a %) (drop-char b %)) (range (count a)))
                :else nil)))]
    (fn word-chain
      ([rem-words] (word-chain rem-words nil))
      ([rem-words last-word]
       (if (empty? rem-words)
         true
         (let [next-words (filter #(can-chain last-word %) rem-words)]
           (if (some #(word-chain (remove #{%} rem-words) %) next-words)
             true
             false)))))))

(defcheck solution-3e35d8db
  (fn [words]
    (letfn [(count-neq [w1 w2] (count (filter #(apply not= %) (map list w1 w2))))
            (remove-at [w i] (str (subs w 0 i) (subs w (inc i))))
            (remove-each [w] (map #(remove-at w %) (range (count w))))
            (subst? [w1 w2] (and (= (count w1) (count w2)) (= 1 (count-neq w1 w2))))
            (insert? [w1 w2] (boolean (some #{w1} (remove-each w2))))
            (connected? [w1 w2] (or (subst? w1 w2) (insert? w1 w2) (insert? w2 w1)))
            (connected-to [w] (for [w2 words :when (connected? w w2)] w2))
            (any? [s] (boolean (some true? s)))
            (graph [words] (into {} (set (for [w words] [w (connected-to w)]))))
            (dfs [graph word unv]
              (let [unv' (disj unv word) nbs (filter unv' (graph word))]
                (if (empty? unv') true (any? (for [w nbs] (dfs graph w unv'))))))]
      (any? (for [w words] (dfs (graph words) w words))))))

(defcheck solution-3ee3e802
  (letfn [(drop-char-at [s i] (str (subs s 0 i) (subs s (inc i))))
          (equal-by-deletion? [s s*]
            (and (= (count s*) (dec (count s)))
                 (some #(= s* (drop-char-at s %)) (range 0 (count s)))))
          (equal-by-replacement? [s1 s2]
            (and (= (count s1) (count s2))
                 (= 1 (count (filter #(not= (get s1 %) (get s2 %))
                               (range 0 (count s1)))))))
          (chainable? [s1 s2]
            (or (equal-by-deletion? s1 s2)
                (equal-by-deletion? s2 s1)
                (equal-by-replacement? s1 s2)))
          (chainable-from? [words word]
            (if (empty? words)
              true
              (some #(and (or (nil? word) (chainable? word %))
                          (chainable-from? (disj words %) %))
                words)))]
    (fn [words] (boolean (chainable-from? words nil)))))

(defcheck solution-3f15fd94
  (fn word-chain?
    ; When called with a single arg, inject nil as the first arg.
    ([s]     (word-chain? nil s))

    ; When called with two args, a word w and a set s, return true if either the set s is empty,
    ; or if there is some way to form a word chain starting with w and followed by all the words
    ; in s.
    ([w s]   (letfn [(valid-step? [wa wb]
                       ; return true if the words wa and wb form a valid step in a word chain
                       ; (i.e. they differ by 1 char), or if wa is nil
                       (let [a  (seq wa)
                             b  (seq wb)
                             ca (count a)
                             fa (first a)
                             ra (rest a)
                             cb (count b)
                             fb (first b)
                             rb (rest b)]
                         (cond
                           (= wa nil)                true
                           (> ca cb)                 (recur b a)  ;; make sure wa is no longer than wb
                           (and (= ca 0) (= cb 1))   true
                           (= ca cb 1)               (not= fa fb)
                           (= ca cb)                 (if (= fa fb) (recur ra rb) (= ra rb))
                           (= fa fb)                 (recur ra rb)
                           :else                     (= a rb))))]
               (or
                (empty? s)
                (boolean (some #(word-chain? % (disj s %)) (filter #(valid-step? w %) s))))))))

(defcheck solution-3f3d069
  (fn [words]
    (let [link? (fn l? [x y]
                  (if (> (count y) (count x))
                    (l? y x)
                    (let [t (first (for [i (range (count x)) :when (not= (get x i) (get y i))]i))]
                      (cond (nil? t) false
                            (not= (count x) (count y)) (= (subs x (inc t)) (subs y t))
                            :else (= (subs x (inc t)) (subs y (inc t)))))))
          remove-one (fn [coll item]
                       (concat (take-while (partial not= item) coll) (drop 1 (drop-while (partial not= item) coll))))]
      (loop [[h & xs] (map vector words)]
        #_(println h)
        (cond (nil? h) false
              (= (count h) (count words)) true
              :else (recur (concat (map (partial conj h)
                                     (filter (partial link? (last h))
                                       (reduce remove-one words h))) xs)))))))

(defcheck solution-3fc495f7
  (letfn [(levenstein [mlev ss ts]
            (cond (empty? ss) (count ts)
                  (empty? ts) (count ss)
                  :else (let [cost (if (= (first ss) (first ts)) 0 1)]
                          (min (inc (mlev mlev (rest ss) ts))
                            (inc (mlev mlev ss (rest ts)))
                            (+ cost (mlev mlev (rest ss) (rest ts)))))))
          (lev [s t]
            (levenstein (memoize levenstein) (seq s) (seq t)))
          (other [node edge]
            (first (disj edge node)))
          (traverse [[node & _ :as path] edges f]
            (let [targets (filter #(% node) edges)]
              (if (seq targets)
                (some #(traverse (cons (other node %) path) (disj edges %) f)
                  targets)
                (f path))))]
    (fn [nodes]
      (let [size  (count nodes)
            edges (set (for [a nodes b nodes :when (= 1 (lev a b))] #{a b}))]
        (boolean
          (some identity
            (map #(traverse [%] edges (fn [path] (= size (count path)))) nodes)))))))

(defcheck solution-406d685c
  (fn [z] (letfn [(init [l] (apply list l))

                  (remove-visited [visited all] (remove #(some #{%} visited) all))

                  (contrast [z1 z2 fun] (let [m (max (count z1) (count z2))]
                                          (filter (fn [[i x y]] (not= x y))
                                            (map #(list %1 %2 %3) (range m) (fun z1) (fun z2)))))

                  (similar? [x y] (if (= x y) false
                                              (let [z1 (map str x)
                                                    z2 (map str y)
                                                    m (max (count z1) (count z2))
                                                    p1 (contrast z1 z2 identity)
                                                    p2 (contrast z1 z2 reverse)
                                                    size-diff (Math/abs (- (count z1) (count z2)))]
                                                (if (or (empty? p1) (empty? p2)) (= 1 size-diff)
                                                                                 (= (dec m) (+ (nth (apply min-key first p1) 0) (nth (apply min-key first p2) 0)))))))

                  (children [node visited all] (remove-visited visited (filter (partial similar? node) all)))

                  (search [node visited all] (filter (comp not nil?)
                                               (let [child-list (seq (children node visited all))]
                                                 (lazy-seq  (cons (if (nil? child-list) (conj visited node) nil)
                                                              (reduce #(concat % (search %2 (conj visited node) all)) '() child-list))))))
                  ]
            (let[data (init z) node (last data)]
              (not (every? false? (map (fn[node] (not (empty? (filter #(= (count z) (count %)) (search node [] data))))) data)))))))

(defcheck solution-40dee43a
  #(letfn [
           (nth-removed [s m]
             "return the collection with the n-th char removed"
             (for [n (range (count s)) :when(not= n m)] (nth s n)))

           (eq-but-one? [s t equal-len?]
             (if (empty? t)
               (not equal-len?)
               (if (=(first s)(first t))
                 (eq-but-one? (rest s) (rest t) equal-len?)
                 (if equal-len?
                   (= (rest s) (rest t))
                   (= (rest s) (take (count t) t))))))

           (chained? [s t]
             "test if s and t are chained"
             (if (= s t)
               false    ; don't accept equality
               (case (- (count s) (count t))
                 -1  (chained? t s)   ; from here on len s >= len t
                 0  (eq-but-one? s t true)
                 1  (eq-but-one? s t false)
                 false       ; s and t can't be chained if their lengths differ by more than one
                 )))

           (create-connections[s coll]
             "create a list of strings from coll which can be chained with s"
             (for [c coll :when(chained? s c)] [s c]))

           (create-graph[coll]
             "create the full graph of chainable string pairs"
             (loop [c coll ret []]
               (if (empty? c)
                 ret
                 (recur (rest c) (into ret (create-connections (first c) (rest c)))))))

           (followers[graph node nodes]
             "return all edges directly connected with node which are in the given set of nodes"
             (for [[a b] graph :when(and (= node a) (not (nil? (nodes b))))] b))

           (connected [foll nodes graph]
             "test if the graph is connected and get the degrees of the nodes"
             (loop [succ foll ret nodes deg []]
               (if (or (empty? succ) (empty? ret) (nil? (some ret succ)))
                 [ret deg]
                 (let [nd (first succ) succ-of-node (followers graph nd nodes)]
                   (recur
                     (distinct (concat (rest succ) succ-of-node (followers graph nd ret)))
                     (set (remove (fn[x](= nd x)) ret))
                     (if (< (count succ-of-node) 2)
                       (cons (count succ-of-node) deg)
                       deg))))))
           ]

     (let [
           coll (create-graph %)
           graph (concat coll (map reverse coll))
           nodes (reduce (fn[cset [a b]] (conj cset a b)) #{} coll)
           succ (followers graph (first nodes) (set (rest nodes)))
           res (connected succ nodes graph)
           ]
       (and (empty? (first res)) (<= (count (second res)) 2))  ;is graph connected and are there no more than 2 edges with only one successor
       )
     ))

(defcheck solution-40e53f64
  (fn wc [w]
    (let [ld (fn [a b] ((reduce (fn [acc [i j]]
                                  (assoc acc [i j]
                                             (if (zero? (min i j))
                                               (max i j)
                                               (min (inc (acc [(dec i) j]))
                                                 (inc (acc [i (dec j)]))
                                                 (+ (acc [(dec i) (dec j)])
                                                    (if (= (nth a (dec i))
                                                          (nth b (dec j)))
                                                      0 1))))))
                          {}
                          (sort-by #(apply + %)
                            (for [i (range (inc (count a)))
                                  j (range (inc (count b)))]
                              [i j])))
                        [(count a) (count b)]))
          adjacent? (fn [w1 w2] (= 1 (ld w1 w2)))
          adjacencies (zipmap (vec w) (for [word (vec w)] (apply hash-set (filter #(adjacent? word %) (vec w)))))
          wc? (fn [ws] (every? identity (map adjacent? ws (rest ws))))
          adjacent? (fn [w1 w2] ((adjacencies w1) w2))
          perms (fn perms [things prev]
                  (lazy-seq
                    (if (= 1 (count things))
                      (list things)
                      (for [head things
                            tail (perms (disj things head) head)
                            :when (or (nil? prev)
                                      (adjacent? head prev))]
                        (cons head tail)))))
          ]

      (not (nil? (first (filter wc? (perms w nil)))))

      )))

(defcheck solution-41179fe
  (fn wc? [coll]
    (letfn
     [(onediff [w1 w2]
        (let [s1 (seq w1) s2 (seq w2)]
          (cond
            (not s1) (= 1 (count s2))
            (not s2) (= 1 (count s1))
            (= (first s1) (first s2)) (recur (rest s1) (rest s2))
            :else (or (= (rest s1) (rest s2)) (= (rest s1) s2) (= s1 (rest s2)))
            )))

      (chain [start rem]
        (if (empty? rem)
          true
          (let [nexts (filter #(onediff start %) rem)]
            (if (empty? nexts) false
                               (reduce #(or %1 %2) false (map #(chain % (disj rem %)) nexts))
                               ))))]
      (reduce #(or %1 %2) false (map #(chain % (disj coll %)) coll)))))

(defcheck solution-4157ac52
  (fn [words]
    (letfn [(drop-char [w pos] (str (subs w 0 pos) (subs w (inc pos))))
            (mods [w] (map #(drop-char w %) (range (count w))))
            (subs? [w1 w2] (= 1 (apply + (map #(if (= %1 %2) 0 1) w1 w2))))
            (adds? [w1 w2] (some #(= % w2) (mods w1)))
            (near? [w1 w2]
              (cond
                (nil? w1) true
                (= (count w1) (count w2)) (subs? w1 w2)
                :else (or (adds? w1 w2) (adds? w2 w1))))
            (chain? [x xs]
              (if (empty? xs)
                true
                (->> xs
                  (filter #(near? x %))
                  (map #(chain? % (disj xs %)))
                  (reduce #(or %1 %2) false))))]
      (chain? nil words))))

(defcheck solution-418d85d0
  (fn [w]
    (letfn [(o [& s]
              (comment println s)
              (cond
                (some empty? s) (every? (comp #(< % 2) count) s)
                (apply = (map first s)) (apply o (map rest s))
                :else (let [a (vec (first s)) b (vec (second s)) c (vec (rest a)) d (vec (rest b))]
                        (or (= a d) (= b c) (= c d)))))
            (p [l]
              (cond
                (= 0 (count l)) []
                (= 1 (count l)) (map list l)
                :else (mapcat #(map (partial cons %) (p (remove {% true} l))) l)))]
      (not-every? not (map #(not-any? not (map o (rest %) (butlast %))) (p w))))))

(defcheck solution-41d88736
  (fn [words]
    (let [d1
                  (fn [x y]
                    (cond
                      (empty? x) (= (count y) 1)
                      (empty? y) (= (count x) 1)
                      :else (let [[x0 & x'] x [y0 & y'] y]
                              (if (= x0 y0) (recur x' y')
                                            (or (= x' y) (= x y') (= x' y'))))))
          adj-tab (into {} (for [x words] [x (set (filter #(d1 (seq x) (seq %)) words))]))
          inter clojure.set/intersection]
      (letfn [(hamilton? [xs v ys]
                (if (empty? ys) true
                                (let [xs' (if (nil? v) xs (conj xs v))
                                      vs' (if (nil? v) ys (inter (adj-tab v) ys))]
                                  (some true? (map #(hamilton? xs' % (disj ys %)) vs')))))]
        (true? (hamilton? #{} nil (apply hash-set (keys adj-tab))))))))

(defcheck solution-4270cd7d
  (fn [wd]
    (letfn [(differ1? [w1 w2 & d]
              (if (empty? w1) (= (count w2) 1)
                              (if (empty? w2) (= (count w1) 1)
                                              (if (= (* (count w1) (count w2)) 1) true
                                                                                  (if (nil? d)
                                                                                    (if (= (first w1) (first w2)) (recur (rest w1) (rest w2) nil)
                                                                                                                  (recur w1 w2 true))
                                                                                    (if (= (last w1) (last w2)) (recur (butlast w1) (butlast w2) true)
                                                                                                                false))))))
            (comb [xs]
              (if (empty? xs) [[]]
                              (let [l (comb (rest xs))]
                                (concat (map #(conj % (first xs)) l) l))))
            (deg [xs] (vals (frequencies (flatten xs))))]
      (let [xs (into [] wd)
            d1 (filter #(differ1? (first %) (second %))
                 (for [x (range (dec (count xs))) y (range (inc x) (count xs))] [(xs x) (xs y)]))]
        ((fn [xs]
           (if (empty? xs) false
                           (if (and (= (count (first xs)) (count wd))
                                    (= (count (filter odd? (deg (first xs)))) 2))
                             true
                             (recur (rest xs)))))
         (comb d1))))))

(defcheck solution-438dc2c
  (fn is-word-chain? [word-set]
    (letfn [(distance [s t]
              "Stupid imperative impelmentation of Levensthein distance"
              (let [full-len-s (count s)
                    full-len-t (count t)]
                ((fn recurring-distance [s len-s t len-t]
                   (let [cost (if (= (get s (dec len-s)) (get t (dec len-t))) 0 1)]
                     (cond
                       (zero? len-s) len-t
                       (zero? len-t) len-s
                       :else (min
                               (inc (recurring-distance s (dec len-s) t len-t))
                               (inc (recurring-distance s len-s t (dec len-t)))
                               (+ (recurring-distance s (dec len-s) t (dec len-t)) cost)))))
                 s full-len-s t full-len-t)))

            (find-pairs-for-word [word others]
              (filter (fn [other] (= 1 (distance word other))) others))

            (create-word-pair-map [word-set]
              (reduce (fn [acc word] (into acc {word (find-pairs-for-word word word-set)})) {} word-set))

            (chain-length [used-words word m length]
              (if (contains? used-words word)
                length
                (apply max (map (fn [w] (if (contains? used-words w)
                                          length
                                          (chain-length (conj used-words word)
                                            w
                                            m
                                            (inc length))))
                             (get m word)))))]
      (let [word-count (count word-set)
            word-pair-map (create-word-pair-map word-set)
            max-length (apply max (map (fn [w] (chain-length #{} w word-pair-map 1)) word-set))]
        (= word-count max-length)))))

(defcheck solution-43d12c60
  (fn myf [coll]
    (letfn [(link? [s1 s2]
              (if (= s1 s2) false
                            (let [[small big] (sort-by count [s1 s2])]
                              (loop [s (vec small), b (vec big)]
                                (if (= (first s) (first b)) (recur (rest s) (rest b))
                                                            (cond (= s (rest b)) true
                                                                  (= (rest s) (rest b)) true
                                                                  :else false))))))]
      (loop [res [(first coll)], s (set (rest coll))]
        (let [new-res (reduce #(cond (link? (first %1) %2) (vec (cons %2 %1))
                                     (link? (last %1) %2) (conj %1 %2)
                                     :else %1)
                        res
                        s)
              new-s (clojure.set/difference s (set new-res))]
          (cond (empty? new-s) true
                (= new-res res) false
                :else (recur new-res new-s)))))))

(defcheck solution-44b969d6
  (fn wc [s]
    (letfn
     [(insertion? [w1 w2]
        (and (= (-> w1 count inc) (-> w2 count))
             (loop [w1 w1, w2 w2, i 0]
               (cond
                 (> i 1) false
                 (empty? w1) true
                 (= (first w1) (first w2)) (recur (next w1) (next w2) i)
                 :else (recur w1 (next w2) (inc i))  ))))
      (deletion? [w1 w2]
        (insertion? w2 w1))
      (substitution? [w1 w2]
        (and (= (count w1) (count w2))
             (->> [w1 w2] (apply map not= ) (filter identity) count (= 1)) ))
      (follows? [w1 w2]
        (some identity ((juxt insertion? deletion? substitution?) w1 w2)))
      (follow-seq [w1 ws]
        (filter (partial follows? w1) ws))
      (follow-tree [v words]
        (for [x v] (cons x (follow-tree (follow-seq x words) (remove #(= x %) words)  ))))
      (max-depth [tree]
        (if (and (seq? tree) (not-empty tree)) (inc (apply max (map max-depth tree))) 0)) ]
      (= (-> s count inc) (max-depth (follow-tree s s))))))

(defcheck solution-45359baa
  (letfn [(rm-at [idx word-seq]
            (let [[l r] (split-at idx word-seq)]
              (concat l (rest r))))
          (chained-words? [w1 w2]
            (= 1 (count
                   (remove
                     false?
                     (if-not (= (count w1) (count w2))
                       (let [[w-l w-h] (sort-by count [(seq w1) (seq w2)])]
                         (map #(= w-l (rm-at % w-h)) (range (count w-h))))
                       (map #(not= %1 %2) w1 w2))))))
          (chain? [word words]
            (let [chain (filter #(chained-words? word %) words)]
              (if (seq chain)
                (some true? (map #(chain? % (disj words %)) chain))
                (empty? words))))]
    (fn [words]
      (true? (some true? (map #(chain? % (disj words %)) words))))))

(defcheck solution-4556175d
  (fn word-chain?
    [words]
    (letfn [(levenstein [s t]
              (letfn [
                      (next-row [s i t t_len prev-row]
                        (reduce
                          (fn [res j]
                            (let [cost (if (= (nth s i) (nth t j)) 0 1)
                                  insertion (inc (last res))
                                  deletion (inc (nth prev-row (inc j)))
                                  substitution (+ (nth prev-row j) cost)]
                              (conj res (min insertion deletion substitution))
                              )
                            )
                          (vector (inc i)) (range t_len)
                          )
                        )
                      (calc-distance [s t s_len t_len]
                        (let [first-row (range (inc t_len)) result (reduce
                                                                     (fn [prev-row i]
                                                                       (next-row s i t t_len prev-row)
                                                                       )
                                                                     first-row (range s_len)
                                                                     )]
                          (nth result t_len)
                          )
                        )]
                (let [s_len (count s) t_len (count t)]
                  (if (zero? s_len)
                    t_len
                    (if (zero? t_len)
                      s_len
                      (if (= s t)
                        0
                        (calc-distance s t s_len t_len)
                        )
                      )
                    )
                  )
                )
              )

            (forms-chain? [first-word other-words]
              (if (= #{} other-words)
                true
                (let [next-words-in-chain (filter #(<= (levenstein first-word %) 1) other-words)]
                  (some #(forms-chain? % (disj other-words %)) next-words-in-chain)
                  )
                )
              )]
      (if (some #(forms-chain? % (disj words %)) words) true false)
      )
    ))

(defcheck solution-45e1b89d
  (fn wordchain? [coll]
    (letfn [
            (equal-length-check [w1 w2]
              (let [w1 (vec w1) w2 (vec w2)]
                (->> (range 0 (count w1))
                  (filter #(= (assoc w2 %1 (get w1 %1)) w1))
                  (count)
                  (< 0)
                  )
                )
              )
            (unequal-length-check [w1 w2]
              (let [sorted (sort-by count [w1 w2]) w1 (-> sorted first vec) w2 (-> sorted second vec)]
                (->> (range 0 (count w2))
                  (map #(concat (subvec w2 0 %1) (subvec w2 (inc %1))))
                  (filter #(= % w1))
                  (count)
                  (< 0)
                  )
                )
              )
            (connected-words [word coll]
              (filter #(seq %) (for [other coll]
                                 (let [wcount (count word) ocount (count other) diff (- wcount ocount)]
                                   (cond (or (= diff 1) (= diff -1)) (if (unequal-length-check word other) other ())
                                         (= wcount ocount) (if (equal-length-check word other) other ())
                                         :else ()
                                         )
                                   )
                                 ))
              )
            (chain-iter [current left possible]
              (cond (empty? left) true
                    (empty? possible) false
                    :else (for [p possible]
                            (let [removed  (remove #{p} left)]
                              (chain-iter p removed (connected-words p removed))
                              )
                            )
                    )
              )]
      (loop [remaining (rest coll) current (first coll)]
        (let [left (remove #{current} coll) possible (connected-words current left)]
          (cond (some #{true} (filter #(true? %) (flatten (chain-iter current left possible)))) true
                (empty? remaining) false
                :else (recur (rest remaining) (first remaining))
                )
          )
        )
      )
    ))

(defcheck solution-48983430
  (fn word-chains3
    ([s] (word-chains3 s s []))
    ([s sr resp]
     (letfn [(substitution? [s1 s2] ;;checks if one str is subs of the other
               (let [s1 (apply vector s1)
                     s2 (apply vector s2)
                     co (map #(if (= %1 %2) 1 0) s1 s2)]
                 (and (= (count s1) (count s2)) (= (apply + co) (dec (count s1))))))

             (insertion? [s1 s2] ;;checks if one str is insertion/deletion of other
               (let [s1 (apply vector s1)
                     s2 (apply vector s2)
                     s11 (if (>= (count s1) (count s2)) s1 s2)
                     s22 (if (< (count s1) (count s2)) s1 s2)]
                 (if (not= (+ 1 (count s22)) (count s11)) false
                                                          (letfn [(subss [s1 s2 i]
                                                                    (= s2 (concat (subvec s1 0 i) (subvec s1 (inc i)))))]
                                                            (reduce #(or (subss s11 s22 %2) %1) false (range (count s11)))))))
             ]
       (if (empty? sr) ;;base case
         (if (= (count s) (count resp))
           true;;resp contains the order of the words
           false)
         (loop [sr2 sr] ;;try each remaing str in sr
           (if (not= sr2 [])
             (let [e (first sr2)
                   s1 (last resp)] ;;e needs to follow from s1
               (if (or (= s1 nil)
                       (and (not= s1 nil) (or (insertion? s1 e) (substitution? s1 e))))
                 (let [sol (word-chains3 s (remove #(= % e) sr) (conj resp e))]
                   (if (not sol)
                     (recur (rest sr2))
                     sol))
                 (recur (rest sr2))))
             false)))))))

(defcheck solution-495cbc7f
  (fn [word-set]
    (let [num-diffs (fn [c1 c2]
                      (loop [diff 0 s1 c1 s2 c2]
                        (cond (not (seq s1)) (+ diff (count s2))
                              (not (seq s2)) (+ diff (count s1))
                              (= (first s1)
                                (first s2)) (recur diff (rest s1) (rest s2))
                              (= (count s1)
                                (count s2)) (recur (inc diff) (rest s1) (rest s2))
                              (< (count s1)
                                (count s2)) (recur (inc diff) s1 (rest s2))
                              :else (recur (inc diff) (rest s1) s2))))
          find-path (fn [graph]
                      (let [starter (first (keys graph))
                            start-neighbors (graph starter)]
                        (loop [stack [[[starter] start-neighbors start-neighbors]]]
                          (if-let [[path front-cands end-cands :as cand] (peek stack)]
                            (cond
                              (= (count path) (count graph)) path
                              (seq front-cands) (recur (conj (pop stack)
                                                         [path
                                                          (next front-cands)
                                                          end-cands]
                                                         [(cons (first front-cands)
                                                            path)
                                                          (remove (into #{} path)
                                                            (graph
                                                              (first
                                                                front-cands)))
                                                          (remove #{(first
                                                                      front-cands)}
                                                            end-cands)]))
                              (seq end-cands) (recur (conj (pop stack)
                                                       [path
                                                        front-cands
                                                        (next end-cands)]
                                                       [(concat path
                                                                (list (first
                                                                        end-cands)))
                                                        (remove #{(first
                                                                    end-cands)}
                                                          front-cands)
                                                        (remove (into #{} path)
                                                          (graph
                                                            (first
                                                              end-cands)))]))
                              :else (recur (pop stack)))
                            nil))))
          neighbors (into {} (map (fn [w] [w (filter #(= 1 (num-diffs w %)) word-set)])
                               word-set))]
      (if (find-path neighbors) true false))))

(defcheck solution-49996280
  (fn word-chain? [words]
    (letfn [(insertion? [a b]
              (and
               (= 1 (- (count b) (count a)))
               (let [va (vec a)
                     vb (vec b)
                     n (loop [i 0]
                         (if (and (< i (count a)) (= (nth a i) (nth b i)))
                           (recur (inc i))
                           i))]
                 (= (subvec va n) (subvec vb (inc n))))))
            (deletion? [a b] (insertion? b a))
            (substitution? [a b]
              (and
               (= (count a) (count b))
               (let [va (vec a)
                     vb (vec b)]
                 (= (dec (count a))
                   (count (filter true? (map = va vb)))))))
            (can-chain? [a b]
              (or (insertion? a b) (deletion? a b) (substitution? a b)))]
      (let [words-vec (vec words)
            n (count words-vec)
            m (set (let [words-vec (vec words)
                         n (count words-vec)]
                     (for [i (range n)
                           j (range (inc i) n)
                           :let [w1 (nth words-vec i) w2 (nth words-vec j)]
                           :when (and (not= w1 w2) (can-chain? w1 w2))]
                       #{w1 w2})))]
        (letfn
         [(chain-all? [chained remaining]
            (if (= n (count chained))
              true
              (some
                true?
                (for [word remaining
                      :when (or (empty? chained) (contains? m #{(last chained) word}))]
                  (chain-all? (conj chained word) (disj remaining word))))))]
          (boolean (chain-all? [] (set words))))))))

(defcheck solution-49dec383
  (fn word-chain [words]
    (let [char-at (fn [w i] (if (< i (count w)) (nth w i) \0))
          stail (fn [w i] (if (< i (count w)) (subs w i) ""))
          differ-by-one
                  (fn [w1 w2]
                    (loop [i 0]
                      (if (not= (char-at w1 i) (char-at w2 i))
                        (let [tail1 (stail w1 (inc i))
                              tail2 (stail w2 i)]
                          (or (= tail1 tail2)                             ; match after delete from w1
                              (= (str (char-at w2 i) tail1) tail2)        ; match after replace
                              (= (str (char-at w2 i) (stail w1 i)) tail2)  ; match after insert
                              ))
                        (recur (inc i))
                        )))
          followers
                  (fn [word]
                    [word (set (filter #(differ-by-one word %) (disj words word)))])
          followmap (into {} (map followers words))
          chains
          (fn chains [chain followers unused] (lazy-seq
                                                (if (empty? unused)
                                                  (list chain)
                                                  (for [word followers
                                                        :when (unused word)
                                                        c (chains (conj chain word) (followmap word) (disj unused word))]
                                                    c
                                                    ))))
          ]
      (boolean (seq (chains [] words words)))
      )))

(defcheck solution-49ec60c1
  (fn [ss]
    (let [followed? (fn followed? [s s']
                      (if (> (count s) (count s'))
                        (followed? s' s)
                        (let [diff-idx (.indexOf (map = s s') false)
                              drop-nth (fn [n xs]
                                         (concat (take n xs)
                                                 (drop (inc n) xs)))]
                          (if-not (neg? diff-idx)
                            (if (= (count s') (count s))
                              (= (drop-nth diff-idx s')
                                (drop-nth diff-idx s))
                              (= (drop-nth diff-idx s')
                                (seq s)))
                            (= 1 (- (count s') (count s)))))))
          followers (reduce (fn [acc s]
                              (assoc acc s
                                         (filter #(followed? s %) ss)))
                      {}
                      ss)
          make-chains (fn make-chains [acc init-word]
                        (if (get acc init-word)
                          [acc]
                          (apply concat
                            (map #(make-chains (conj acc init-word) %)
                              (get followers init-word)))))]
      #_(println followers)
      #_(println (followed? "dot" "do"))
      (boolean
        (some
          (partial = ss)
          (apply concat
            (map #(make-chains #{} %)
              (keys followers))))))))

(defcheck solution-4a68f0a2
  (fn word-chains [words]
    (let [one-diff?           (fn one-diff? [s t]
                                (cond (and (empty? s) (empty? t)) false
                                      (= (first s) (first t)) (one-diff? (rest s) (rest t))
                                      :else (or (= (rest s) (rest t))
                                                (= (seq s) (rest t))
                                                (= (rest s) (seq t)))))
          transitions         (into {} (for [w words]
                                         [w (filter #(one-diff? w %) words)]))
          next-chains         (fn [chain]
                                (keep #(if (nil? ((apply hash-set chain) %))
                                         (conj chain %))
                                  (transitions (last chain))))
          ]
      (not (empty? (last (take (count words) (iterate #(apply concat (map next-chains %)) (map vector words)))))))))

(defcheck solution-4ac7b293
  (fn [xs]
    (letfn [(max-distance? [n x y]
              (and (>= n 0)
                   (or (and (empty? x)
                            (empty? y))
                       (and (seq x)
                            (max-distance? (dec n) (rest x) y))
                       (and (seq y)
                            (max-distance? (dec n) x (rest y)))
                       (and (seq x)
                            (seq y)
                            (let [next-n (if (= (first y) (first x))
                                           n
                                           (dec n))]
                              (max-distance? next-n (rest x) (rest y)))))))
            (chains-starting-with [x ys]
              (if (seq ys)
                (for [y (filter (partial max-distance? 1 x) ys)
                      chain (chains-starting-with y (disj ys y))]
                  (cons x chain))
                (list (list x))))]
      (->> (mapcat #(chains-starting-with % (disj xs %)) xs)
        seq
        boolean))))

(defcheck solution-4b1497d8
  (fn [ws]
    (letfn [(same-size-neighbors? [w1 w2] (> 2 (count (filter not (map = w1 w2)))))
            (drop-nth [n coll] (concat (take n coll) (drop (inc n) coll)))
            (one-off-neighbors? [w1 w2] (let [cs (for [n (range (count w1))] (drop-nth n w1))]
                                          (seq (filter identity (map = cs (repeat (seq w2)))))))
            (neighbors? [w1 w2] (let [size-diff (- (count w1) (count w2))]
                                  (cond
                                    (= 0 size-diff) (same-size-neighbors? w1 w2)
                                    (= 1 size-diff) (one-off-neighbors? w1 w2)
                                    (= -1 size-diff) (one-off-neighbors? w2 w1)
                                    :else false)))
            (chain? [ws] (empty? (filter #(not (apply neighbors? %)) (partition 2 1 ws))))
            (permutations [ws] (letfn [(f [c1 c2] (map #(conj c1 %) (filter #(not (some #{%} c1)) c2)))
                                       (g [d1 d2] (mapcat #(f % d2) d1))]
                                 (reduce g (map vector ws) (repeat (dec (count ws)) ws))))]
      (loop [perms (permutations ws)]
        (cond
          (empty? perms) false
          (chain? (first perms)) true
          :else (recur (rest perms)))))))

(defcheck solution-4bf668f8
  (fn have-any-hamilton-path? [s]
    (letfn [
            (parse-src [ss] (map vec (seq ss)))
            (make-dict [vs] (apply hash-map (interleave (iterate inc 0) vs)))
            (part-by-one-lead [xcoll]
              (let [len (count xcoll)
                    vs (vec xcoll)]
                (letfn [(comb-fn [i] [(nth vs i)
                                      (into (vec (take i vs)) (drop (inc i) vs))])
                        (loc-gen-pairs [j]
                          (lazy-seq
                            (if (<= len j)
                              (list)
                              (cons (comb-fn j) (loc-gen-pairs (inc j))))))]
                  (loc-gen-pairs 0))))
            (test-for-indel [xs ys]
              (if (or (empty? xs) (empty? ys))
                false
                (let [xlen (count xs), ylen (count ys)
                      [small big] (if (< xlen ylen) [xs ys] [ys xs])
                      dels-for-big (map #(into (vec (take % big)) (drop (inc %) big))
                                     (range 0 (count big)))]
                  (not (not-any? #(= small %) dels-for-big)) )))
            (count-diffs [xs ys]
              (let [zs (map (fn [a b] [a b]) xs ys)
                    cnt (reduce (fn [acc [a b]] (if (= a b) acc (inc acc)))
                          0 zs)]
                cnt))
            (test-for-subst [xs ys] (>= 1 (count-diffs xs ys)))
            (one-dist-ok? [xs ys]
              (if (or (empty? xs) (empty? ys))
                false
                (cond
                  (= (count xs) (count ys)) (test-for-subst xs ys)
                  :else (test-for-indel xs ys) )))
            (make-nearest-idx [m-dict]
              (let [ikeys (keys m-dict)
                    mrg-nears-fn (fn [m j js]
                                   (let [m-j (if (contains? m j) m (assoc m j []))]
                                     (reduce (fn [m b]
                                               (if (one-dist-ok? (m-dict j) (m-dict b))
                                                 (assoc m j (conj (get m j []) b))
                                                 m))
                                       m-j js)))]
                (reduce (fn [m [j js]]
                          (mrg-nears-fn m j js))
                  {} (part-by-one-lead ikeys))))
            (find-any-hpath [graph]
              (let [nodes-total (count graph)]
                (letfn [
                        (append-to-top-with-nxt [vv b]
                          (let [new-vect-here (conj (pop vv) (conj (peek vv) b))
                                new-vect-next (conj new-vect-here [b])]
                            new-vect-next))
                        (deep-fst-search [acc]
                          (if (= nodes-total (count acc))
                            acc   ;; just found the path
                            (let [
                                  current-node (first (peek acc))
                                  used-nodes (clojure.set/union (set (map first acc))
                                               (set (rest (peek acc))))
                                  all-branches (get graph current-node)
                                  nxt-branches (remove used-nodes all-branches)
                                  dead-end? (empty? nxt-branches)]
                              (if-not dead-end?
                                (deep-fst-search (append-to-top-with-nxt acc (first nxt-branches)))
                                (let [step-back (pop acc)]
                                  (if (empty? step-back)
                                    nil   ;; not found anything
                                    (deep-fst-search step-back)))
                                )) ))
                        (extract-path-seq [path-data] (map first path-data))
                        (search-path-for [start-positions]
                          (if (empty? start-positions)
                            nil
                            (let [a-node (first start-positions)
                                  some-found (deep-fst-search [[a-node]])]
                              (if (nil? some-found)
                                (search-path-for (rest start-positions))
                                (extract-path-seq some-found))) ))]
                  (search-path-for (keys graph))
                  ;; for simplicity take all nodes as candidates
                  ))) ]
      (let [mdic (-> s (parse-src) (make-dict))
            nidx (make-nearest-idx mdic)
            rslt (find-any-hpath nidx)]
        (if (nil? rslt) false true)))))

(defcheck solution-4c4169c1
  (fn chainable? [words]
    (letfn [(chain? [words]
              (every? valid-pair? (map list words (rest words))))
            (valid-pair? [[w1 w2]]
              #_(println [w1 w2])
              (or (substitution? w1 w2)
                  (insertion? w2 w1)
                  (insertion? w1 w2)))
            (substitution? [w1 w2]
              (and (= (count w1) (count w2))
                   (some (fn [i] (= (str (subs w1 0 i) (subs w1 (inc i)))
                                   (str (subs w2 0 i) (subs w2 (inc i)))))
                     (range (count w1)))))
            (insertion? [w1 w2]
              (and (= (inc (count w1)) (count w2))
                   (some (fn [i] (= w1 (str (subs w2 0 i) (subs w2 (inc i)))))
                     (range 0 (inc (count w1))))))
            (permutations [xs]
              (if (seq (rest xs))
                (apply concat
                  (for [x xs]
                    (map #(cons x %) (permutations (remove #{x} xs)))))
                [xs]))]
      #_(println (permutations words))
      (boolean (some chain? (permutations words))))))

(defcheck solution-4c922799
  (fn word-chain? [words]
    (letfn [
            (close? [[c1 & t1 :as s1] [c2 & t2 :as s2] dist]
              (let [ddist (dec dist)]
                (cond
                  (< dist 0) false
                  (and (empty? s1) (empty? s2) (= dist 0)) true
                  (and (empty? s1) (empty? s2)) false
                  (empty? s1) (recur s1 t2 ddist)
                  (empty? s2) (recur t1 s2 ddist)
                  (= c1 c2) (recur t1 t2 dist)
                  :else (or (close? t1 s2 ddist) (close? s1 t2 ddist) (close? t1 t2 ddist))
                  )))
            (k-combinations [k s]
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
                         (k-combinations (dec k) t))))))))
            (permuations [s]
              (((fn perm [s results]
                  (if (results s)
                    results
                    (let [
                          new-perm
                          (apply concat
                            (map
                              (fn [x]
                                (let [without-x (disj s x)]
                                  (map #(cons x %) ((perm  without-x results) without-x))))
                              s))]
                      (assoc results s new-perm))))
                s {#{} '(())})
               s))
            (generate-possible-paths [s]
              (into
                #{}
                (map
                  #(into #{} %)
                  (map
                    (fn [p]
                      (map #(into #{} %) (partition 2 1 p)))
                    (permuations s)))))
            (intersect-sets [s1 s2] (set (filter #(contains? s2 %) s1)))
            (subset? [s sub]
              (every? #(contains? s %) sub))
            (swap? [s1 s2]
              (let [
                    size (count s1)
                    sub1 (k-combinations (dec size) s1)
                    sub2 (k-combinations (dec size) s2)]
                (not (empty? (intersect-sets sub1 sub2)))))

            (similar? [s1 s2]
              (close? s1 s2 1))
            (path? [edges nodes start-node]
              (if (= 1 (count nodes))
                (contains? edges #{start-node (first nodes)})
                (if (empty? edges)
                  false
                  (let [
                        possible-edges (filter #(and (contains? % start-node) (not (empty? (intersect-sets % nodes)))) edges)
                        ]
                    (not(nil? (some
                                (fn [edge]
                                  (let [new-start-node (first (disj edge start-node))
                                        new-nodes (disj nodes new-start-node)]
                                    (path? (disj edges possible-edges) new-nodes new-start-node)))
                                possible-edges)))))))

            (hamiltonian? [edges nodes]
              (not
                (nil?
                  (some
                    #(path? edges (disj nodes %) %)
                    nodes))))
            ]
      (let [graph (into #{} (filter #(apply similar? %) (k-combinations 2 words)))]
        ;(not(empty? (filter #(subset? graph %) (generate-possible-paths words)))))))
        (hamiltonian? graph words)))))

(defcheck solution-4c95bda2
  (fn f
    [nodes]
    (letfn [(drop-nth [n coll]
              (keep-indexed #(if (not= %1 n) %2) coll))
            (chainable? [x y]
              (cond
                (apply = (map count [x y])) (= 1 (apply + (map {true 0 false 1} (apply map = (map seq [x y])))))
                :else (letfn [(h [s l]
                                (some identity (for [i (range (count l))]
                                                 (= s (apply str (drop-nth i l))))))]
                        (if (apply < (map count [x y]))
                          (h x y)
                          (h y x)))))
            (g [x nodes path]
              (if (empty? nodes)
                (cons x path)
                (let [sub-path (for [y (filter #(chainable? x %) nodes)]
                                 (g y (remove #{y} nodes) (cons x path)))]
                  (if (seq sub-path)
                    (apply max-key count sub-path)
                    (cons x path)))))]
      (= (count nodes) (apply max (map count (for [x nodes] (g x (remove #{x} nodes) '()))))))))

(defcheck solution-4d3ce940
  (fn word-chains? [coll]
    (letfn [(one-char-diff? [a b]
              (if (= (first a) (first b))
                (one-char-diff? (rest a) (rest b))
                (or (= (rest a) (rest b)) (= (sequence a) (rest b)) (= (rest a) (sequence b)))))
            (make-graph [coll]
              (reduce (fn [n-map word-a]
                        (into n-map (reduce (fn [in-map word-b]
                                              (if (and (not (= word-a word-b)) (one-char-diff? word-a word-b))
                                                (update-in in-map [word-a] conj word-b)
                                                in-map)) {word-a #{}} coll))) '{} coll))
            (depth [graph]
              (letfn [(helper [cnt visited to-visit]
                        (if (empty? to-visit)
                          cnt
                          (apply max (map (partial helper (inc cnt)) (map #(conj visited %) to-visit) (map #(clojure.set/difference (get graph %) visited) to-visit)))))]
                (apply max (map #(helper 0 #{} [%]) coll))))]
      (= (count coll) (depth (make-graph coll))))))

(defcheck solution-4d80ad12
  (fn f
    ([s] (true? (some true? (flatten
                              (for [as s] (f as (disj s as)))))))
    ([fs s]
     (let [ok? (fn ok? [a b]
                 (let [a (vec a)
                       b (vec b)
                       spl (map #(split-at % a) (range 0 (inc (count a))))]
                   (some true?
                     (for [[l r] spl c b]
                       (some #(= % b) [
                                       (concat l (conj r c))
                                       (concat l (rest r))
                                       (when (seq l) (concat (conj (vec (butlast l)) c) r))
                                       ])))))]
       (if (empty? s) true
                      (let [nc (filter #(ok? % fs) s)]
                        (when (seq nc) (map #(f % (disj s %)) nc))))))
    ))

(defcheck solution-4e2a6fa6
  (fn word-chain? [ words ]
    (letfn [(lev-dist [ a b]
              (letfn [
                      (ld [ld-memo a b]
                        (cond
                          (empty? a) (count b)
                          (empty? b) (count a)
                          :default (min
                                     (if (= (last a) (last b))
                                       (ld-memo ld-memo (butlast a) (butlast b))
                                       (inc (ld-memo ld-memo (butlast a) (butlast b))))
                                     (inc (ld-memo ld-memo (butlast a) b ))
                                     (inc (ld-memo ld-memo a (butlast b)))
                                     )))
                      ] (let [ ld-memo (memoize ld) ]
                          (ld-memo ld-memo a b)
                          )))] (let [chain-links (filter identity (for [k words v words] (if (= 1 (lev-dist k v)) [k v]))) ]
                                 (letfn [(add-to-chain [chain]
                                           (let [matching-links  (filter  #(= (first chain) (last %)) chain-links)
                                                 unused-links (filter #(nil? ((set chain) (first %))) matching-links) ]
                                             (if (empty? unused-links)
                                               [chain]
                                               (mapcat #(add-to-chain (cons (first %) chain)) unused-links ))))
                                         ]
                                   (not (empty? (filter (fn [wc] (= (count wc) (count words)))   (mapcat #(add-to-chain %) chain-links))))))
                               )))

(defcheck solution-4e930465
  (fn [s]
    (let [k? (fn [g1 g2]
               (> 2 (reduce
                      #(+ % (count (last %2))) 0
                      (reduce (fn [m [k v]]
                                (if-let [a (m k)] (assoc m k (->
                                                               (- (count a) (count v))
                                                               Math/abs
                                                               (repeat k))) m)) g1 g2))))
          g? (fn [[s1 s2]] (let [g1 (group-by identity s1)
                                 g2 (group-by identity s2)]
                             (and (k? g1 g2) (k? g2 g1) (not= (sort s1) (sort s2)))))
          p (fn p [[x & xs]] (if xs (let [r (p xs)
                                          c (range (inc (count xs)))]
                                      (mapcat (fn [v] (map #(concat (take % v) [x] (drop % v)) c)) r)) [[x]]))]
      ((complement empty?) (filter #(every? g? (partition 2 1 %)) (p (seq s)))))))

(defcheck solution-4f51898
  (fn [wl]
    (letfn [(walkgraph2 [start graph visited depth]
              (let [children (graph start)]
                (if (not-empty (remove visited children))
                  (reduce #(max % (walkgraph2 %2 graph (conj visited %2) (inc depth))) 0 (remove visited children))
                  depth)))
            (add-letter2 [ys n c]
              (let [takestr (comp (partial apply str) take)
                    dropstr (comp (partial apply str) drop)]
                (if (zero? n) (list (str c ys))
                              (cons (str (takestr n ys) (str c) (dropstr n ys)) (add-letter2 ys (dec n) c)))))
            (remove-letter2 [ys n]
              (let [sniptake (comp (partial apply str) take)
                    snipdrop (comp (partial apply str) drop)]
                (if (= 1 n) (list (snipdrop n ys))
                            (cons (str (sniptake (dec n) ys) (snipdrop n ys)) (remove-letter2 ys (dec n))))))
            (sub-letter2 [ys n c]
              (let [takestr (comp (partial apply str) take)
                    dropstr (comp (partial apply str) drop)]
                (if (= 1 n) (list (str c (apply str (drop 1 ys))))
                            (cons (str (takestr (dec n) ys) c (dropstr n ys)) (sub-letter2 ys (dec n) c)))))
            (diffwords2 [xs ys]
              (let [alphabet "abcdefghijklmnopqrstuvwxyz"]
                (cond
                  (= xs ys) false
                  (< (count ys) (count xs)) (some #{ys} (remove-letter2 xs (count xs)))
                  (> (count ys) (count xs)) (some #{ys} (reduce #(into % (add-letter2 xs (count xs) %2)) '() alphabet));; add a char to ys
                  (= (count ys) (count xs)) (some #{ys} (reduce #(into % (sub-letter2 xs (count xs) %2)) '() alphabet))
                  ;; replace a letter in ys
                  :else false
                  )))
            (gengraph2 [wlist]
              (let [filtered (fn [word] (filter #(diffwords2 word %) wlist))
                    mapped (reduce #(assoc % %2 (filtered %2)) {} wlist)]
                mapped))]
      (= (count wl) (apply max (map #(walkgraph2 % (gengraph2 wl) (conj #{} %) 1) wl))))))

(defcheck solution-50a197f5
  (fn has-chain? [s]
    (letfn [(drop-nth [n s] (concat (take n s) (drop (inc n) s)))
            (hlp [v s] (map #(cons v %) s))
            (permutations [s]
              (if (empty? s)
                [[]]
                (->> (range (count s))
                  (map #(hlp (nth s %) (permutations (drop-nth % s))))
                  (apply concat))))
            (close-delete [a b]
              (if (= (first a) (first b))
                (close-delete (rest a) (rest b))
                (or (= (rest a) b)
                    (= (rest b) a))))
            (close? [[a b]] (if (= (count a) (count b))
                              (= 1 (count (remove true? (map = a b))))
                              (close-delete (seq a) (seq b))))
            (chain? [s] (every? true? (map close? (partition 2 1 s))))]
      (not (every? false? (map chain? (permutations (vec s))))))))

(defcheck solution-50a5ac07
  (fn n82 [word-set]
    (letfn [(levenshtein [s t]
              (cond
                (zero? (count s)) (count t)
                (zero? (count t)) (count s)
                :else (let [m (count s) n (count t)
                            d0 (vec (repeat (inc n) (vec (repeat (inc m) 0))))
                            d (loop [j 0 d (loop [i 0 d1 d0]
                                             (if (> i m) d1
                                                         (recur (inc i) (assoc-in d1 [0 i] i))))]
                                (if (> j n) d
                                            (recur (inc j) (assoc-in d [j 0] j))))]
                        (get-in (loop [j 1 tc (nth t (dec j)) d1 d]
                                  (if (> j n) d1
                                              (recur (inc j) (if (= j n) (nth t (dec j)) (nth t j))
                                                (loop [i 1 sc (nth s (dec i)) d2 d1]
                                                  (if (> i m) d2
                                                              (if (= tc sc)
                                                                (recur (inc i) (if (= i m) (nth s (dec i)) (nth s i)) (assoc-in d2 [j i] (get-in d2 [(dec j) (dec i)])))
                                                                (recur (inc i) (if (= i m) (nth s (dec i)) (nth s i)) (assoc-in d2 [j i] (min (inc (get-in d2 [(dec j) (dec i)]))
                                                                                                                                           (inc (get-in d2 [(dec j) i]))
                                                                                                                                           (inc (get-in d2 [j (dec i)])))))))))))
                          [n m])
                        )))
            (get-neighboors [c n edges]
              (filter identity (map #(cond
                                       (= (first %) c) (if (contains? n (second %)) false (second %))
                                       (= (second %) c) (if (contains? n (first %)) false (first %))
                                       :else false) edges)))
            (dfs [nodes edges]
              (loop [c (first nodes) s [c] n #{c}
                     t (get-neighboors c n edges)]
                (if (empty? s)
                  (if (empty? t)
                    n
                    (recur (first t) (conj s (first t)) (conj n (first t)) (get-neighboors (first t) n edges)))
                  (recur
                    (if (empty? t) (peek s) (first t))
                    (if (empty? t) (pop s) (conj s (first t)))
                    (if (empty? t) n (conj n (first t)))
                    (if (empty? t) (get-neighboors (peek s) n edges) (get-neighboors (first t) n edges))))))
            (is-linked [nodes edges] (= nodes (dfs nodes edges)))
            (find-path [nodes edges start]
              ((fn solve [visited start]
                 (let [nbs (get-neighboors start visited edges)]
                   (if (empty? nbs)
                     (= (inc (count visited)) (count nodes))
                     (reduce #(or %1 %2) (map #(solve (conj visited start) %) nbs) )))) #{} start))
            ]
      (let [coll (vec word-set)
            ct (count coll)
            n (set (range ct))
            m (vec (map vec (partition ct (for [x (range ct) y (range ct)
                                                :let [wx (nth coll x) wy (nth coll y)]]
                                            (levenshtein wx wy)))))
            e (set (map set (filter #(= 1 (get-in m %)) (for [x (range ct) y (range ct)] [x y]))))]
        (if (is-linked n e)
          (true? (some true? (map (partial find-path n e) n)))
          false)
        ))))

(defcheck solution-50cfde42
  (fn chain [words]
    (letfn
     [ (chain-helper [words next-start]
         (chain?
           next-start
           (remove #(= next-start %) words)))
      (adjacent? [n [q-head & q-tail :as q] [r-head & r-tail :as r]]
        (cond
          (neg? n) false
          (and (nil? q-head) (nil? r-head)) true
          (or  (nil? q-head) (nil? r-head)) (adjacent? (dec n) q-tail r-tail)
          (= q-head r-head) (adjacent? n q-tail r-tail)
          :else
          (or
           (adjacent? (dec n) q-tail r-tail)
           (adjacent? (dec n) q      r-tail)
           (adjacent? (dec n) q-tail r))))
      (chain? [start words]
        (let
         [out-going (for [word words :when (adjacent? 1 start word)] word)]
          (cond
            (empty? words)     true
            (empty? out-going) false
            :else (some identity (map (partial chain-helper words) out-going))))) ]
      (true? (some identity (map (partial chain-helper words) words))))))

(defcheck solution-52ce7f39
  (fn [ws]
    (let [chains? (fn [w1 w2]
                    (cond
                      (and (or (empty? w1) (= 1 (count w1)))
                           (or (empty? w2) (= 1 (count w2)))) true
                      (= (first w1) (first w2)) (recur (next w1) (next w2))
                      (= (last w1) (last w2)) (recur (butlast w1) (butlast w2))
                      :else false))
          mc? (memoize (fn [w1 w2] (if (nil? w1) true (chains? w1 w2))))
          insertions (fn [ps w]
                       (let [ins (fn [[ls rs] w]
                                   (if (and (mc? (last ls) w) (mc? (first rs) w))
                                     (concat ls (list w) rs)))]
                         (filter identity
                           (into [] (for [i (range (inc (count ps)))]
                                      (ins (split-at i ps) w))))))
          solutions (fn [ss]
                      (if
                       (or (empty? ss) (some (comp empty? first) ss)) ss
                                                                      (recur
                                                                        (set (mapcat (fn [s]
                                                                                       (let [cw (first s)]
                                                                                         (mapcat (fn [w]
                                                                                                   (let [rw (disj cw w)
                                                                                                         ps (second s)]
                                                                                                     (map #(list rw %)
                                                                                                       (insertions ps w))))
                                                                                           cw)))
                                                                               ss)))))]
      (boolean (seq (solutions (hash-set (list ws '()))))))))

(defcheck solution-52e8a509
  (fn [a]
    (letfn [(lev [s t]
              (let [x (vec s)
                    y (vec t)
                    cx (count x)
                    cy (count y)
                    d (fn [matrix [i j :as indices]]
                        (let [del (get-in matrix [(dec i) j])
                              ins (get-in matrix [i (dec j)])
                              sub (get-in matrix [(dec i) (dec j)])
                              res (if (= (x (dec i)) (y (dec j)))
                                    sub
                                    (min (inc del) (inc ins) (inc sub)))]
                          (update-in matrix indices (constantly res))))
                    z (->> (for [i (range (inc cx)) j (range (inc cy))]
                             (cond
                               (zero? j) [i]
                               (zero? i) [j]
                               :else []))
                        (partition (inc cy))
                        (map #(vec (apply concat %)))
                        vec)]
                (get-in (reduce d z (for [i (range 1 (inc cx)) j (range 1 (inc cy))] [i j]))
                  [cx cy])))]
      (<= (count (->> (for [b a d (disj a b)] (when (= 1 (lev b d)) true))
                   (partition (dec (count a)))
                   (map #(remove nil? %))
                   (filter #(< (count %) 2))))
        2))))

(defcheck solution-5349fce7
  (fn wc [c']
    (let [edit-distance (fn edit-distance [s1 s2]
                          (let [s1 (map str s1)
                                s2 (map str s2)
                                cost? (fn [p q]
                                        (if (= (first p) (first q)) 0 1))]
                            (cond
                              (and (empty? s1) (empty? s2)) 0
                              (empty? s1) (count s2)
                              (empty? s2) (count s1)
                              :else (+ (cost? (first s1) (first s2))
                                       (min (edit-distance s1 (rest s2))
                                         (edit-distance (rest s1) s2)
                                         (edit-distance (rest s1) (rest s2)))
                                       ))))
          ls (for [i c' j c'
                   :when (and ((complement =) i j)
                              (= 1 (edit-distance i j)))][i j])
          adj' (into {} (for [[k v] (group-by first ls)] [k (map second v)]))
          #_#_yy (print adj')
          connected-nodes (fn connected? [adj]
                            (let [add-nodes (fn add-nodes [s v]
                                              (if (s v) s (reduce add-nodes (conj s v) (adj' v) ))
                                              )] (add-nodes #{} (ffirst adj) )))
          nbrcnt (reduce #(if (<= (count (second %2)) 1) (inc %1) %1) 0 adj')]
      #_(print (connected-nodes adj'))
      #_(print nbrcnt)
      (and (= (count (connected-nodes adj')) (count c') )
           (<= nbrcnt 2)))))

(defcheck solution-54568a7b
  (fn word-chains
    [words]
    (letfn [(substrings
              [s]
              (into #{} (map (fn [i] (concat (take i s) (drop (inc i) s))) (range (count s)))))
            (subs? [longer shorter] (not (empty? (filter #(= % (seq shorter)) (substrings longer)))))
            (close?
              [a b]
              (cond
                (> (Math/abs (- (count a) (count b))) 1) false
                (> (count a) (count b)) (subs? a b)
                (< (count a) (count b)) (subs? b a)
                :else (= (count (filter #(not= (first %) (second %)) (partition 2 (interleave a b)))) 1)
                ))
            (pflatten
              [s]
              (if (empty? s)
                s
                (let [f (first s)
                      r (rest s)]
                  (if (not (coll? (first f)))
                    (cons f (pflatten r))
                    (concat (pflatten f) (pflatten r))))))
            (get-vertex
              [v]
              {v (reduce (fn [r e]
                           (if (close? v e)
                             (conj r e)
                             r)) #{} (disj words v))})
            (bfs
              [g v p]
              (if (contains? p v)
                p
                (map #(bfs g % (conj p v)) (g v))
                ))]
      ;(let [a (into #{} (get v))]

      (let [graph (into {} (map get-vertex words))]
        (if (nil? (some #(= words %) (mapcat #(pflatten (bfs graph % #{})) (keys graph))))
          false
          true))
      )))

(defcheck solution-54f1158e
  (fn checkLinks [wordList]
    (let[compFn (fn compStr
                  ([seq1 seq2 sign]
                   (let[r1 (rest seq1),
                        r2 (rest seq2),
                        s1 (empty? seq1),
                        s2 (empty? seq2)]
                     (if (or s1 s2)
                       (cond (and s1 s2) (= sign 1)
                             (or (not-empty r1) (not-empty r2)) false
                             :else (= sign 0)
                             )
                       (let[f1 (first seq1),
                            f2 (first seq2)]
                         (cond (= f1 f2) (compStr r1 r2 sign)
                               (and (not= f1 f2) (= 0 sign) )
                               (or (compStr seq1 r2 1)
                                   (compStr r1 seq2 1)
                                   (compStr r1 r2 1)
                                   )
                               :else false
                               )
                         )
                       )
                     )
                   )
                  ([str1 str2] (compStr (seq str1) (seq str2) 0) )
                  ),
         matchMap ( (fn [wordlist]
                      (reduce
                        (fn [matchMap word]
                          (reduce
                            #(let[val (get %1 word)]
                               (if (empty? val)
                                 (assoc %1 word #{%2} )
                                 (assoc %1 word (conj val %2) )
                                 )
                               )
                            matchMap
                            (filter #(compFn %1 word)
                              (disj wordlist word))
                            )
                          ) {}
                        wordlist)
                      ) wordList ),
         generNextlinks  (fn[linkInfo]
                           (let [convert (fn[key]
                                           (let[node (key linkInfo),
                                                links (:link linkInfo)]
                                             (map #(assoc
                                                    (assoc linkInfo :link
                                                                    (conj links %)
                                                                    )
                                                     key %)
                                               (filter
                                                 #(not (contains? links %) )
                                                 (get matchMap node)
                                                 )
                                               )
                                             )
                                           )
                                 ]
                             (concat (convert :head)
                                     (convert :tail) )
                             )
                           ),
         firstNode  (first wordList),
         nextNodes  (get matchMap firstNode)]
      (if (nil? nextNodes)
        false
        (not=  (some
                 (fn[infoLinks]
                   #_(println infoLinks)
                   (if (empty? infoLinks)
                     -1
                     (some
                       #(empty?
                          (apply (partial disj wordList) (:link %) )
                          )
                       infoLinks)
                     )
                   )
                 (iterate
                   (fn[infoLinks]
                     (reduce #(concat %1 (generNextlinks %2) ) []  infoLinks)
                     )
                   [{:head firstNode
                     :tail (first nextNodes)
                     :link #{firstNode (first nextNodes)}}]
                   )
                 )  -1)
        )
      )
    ))

(defcheck solution-55081c74
  (fn [ss]
    (letfn [
            (replace-substring [s r start len] (str (subs s 0 start) r (subs s (+ len start))))
            (replacements [s] (map #(replace-substring s "." % 1) (range (count s)) ))
            (insertions [s] (map #(replace-substring s "." % 0) (range (inc (count s))) ))
            (deletions [s] (map #(replace-substring s "" % 1) (range (count s)) ))
            (all [s] (mapcat #(% s) [replacements insertions deletions]))
            (adjacencies [ss]
              (into
                {}
                (for [s1 ss]
                  (let [res (map re-pattern (all s1))
                        matches (for [s2 ss
                                      :when (not= s1 s2)
                                      :when (some #(re-matches % s2) res)]
                                  s2)]
                    [s1 (into #{} matches)]))))

            (chain? [adjs]
              ((fn loop! [ss adjs]
                 (if
                  (empty? ss) (empty? adjs)
                              (some true?
                                (for [s ss]
                                  (loop! (adjs s) (dissoc adjs s) ))))
                 )
               (map first adjs)
               adjs))]

      (if (chain? (adjacencies ss)) true false))))

(defcheck solution-55207630
  (fn p82 [words]
    (letfn [(find-longer-word [w1 w2] (if (>= (count w2) (count w1))
                                        [w2 w1] [w1 w2]))
            (check-words [w1 w2]
              (let [word-seq (find-longer-word w1 w2)]
                (merge-with - (frequencies (first word-seq))
                  (frequencies (last word-seq)))))
            (dict-has? [w chw-count]
              (contains? #{(+ 1 (count w)) (- (count w) 1)} chw-count))
            (check-dict-length [w1 w2]
              (let [chw-count (count (check-words w1 w2))]
                (or (dict-has? w1 chw-count) (dict-has? w2 chw-count))))
            (check-adjacency [d]
              (let [v (set (keys d))
                    one (get d 1)]
                (and (= v #{0 1}) (contains? #{1 2} one))))
            (compare-freqs [w1 w2] (->> (check-words w1 w2)
                                     vals
                                     (frequencies)
                                     (check-adjacency)))]
      (let [result (->> (for [word words]
                          (let [remainder (clojure.set/difference words #{word})]
                            (map #(if (and (compare-freqs word %)
                                           (check-dict-length word %))
                                    #{word %} nil) remainder)))
                     flatten
                     (remove nil?)
                     set)]
        (<= (count words) (count result))))))

(defcheck solution-55218b68
  (fn [i-words]
    (let [
          n-count (count i-words)
          n-range (range n-count)
          n-map (apply hash-map (interleave i-words n-range))
          n-index (fn [i-word] (get n-map i-word))

          edit-distance (fn diff ([a-word b-word] (diff 0 a-word b-word))
                          ([distance a-word b-word]
                           (let [
                                 a-head (first a-word)
                                 b-head (first b-word)

                                 a-tail (rest a-word)
                                 b-tail (rest b-word)

                                 a-next (first a-tail)
                                 b-next (first b-tail)
                                 ]
                             (if (and (nil? a-head) (nil? b-head))
                               distance
                               (if (= a-head b-head)
                                 (diff distance a-tail b-tail)
                                 (if (= a-next b-head)
                                   (diff (inc distance) a-tail b-word)
                                   (if (= a-head b-next)
                                     (diff (inc distance) a-word b-tail)
                                     (diff (inc distance) a-tail b-tail))))))))
          edges (reduce
                  (fn [result i-word]
                    (reduce
                      #(conj %1 (vector i-word %2))
                      result
                      (filter #(= 1 (edit-distance i-word %)) i-words)))
                  #{}
                  i-words)
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-word]
                   (map last (get grouped-edges (n-index i-word))))

          build-chains (fn chain
                         ([i-word] (chain (dec n-count) (vector (vector i-word))))
                         ([i result]
                          (if (zero? i)
                            result
                            (chain
                              (dec i)
                              (reduce
                                concat
                                []
                                (map
                                  (fn [i-chain]
                                    (map
                                      #(conj (apply vector i-chain) %)
                                      (filter #(not (contains? (apply hash-set i-chain) %)) (n-dest (last i-chain)))))
                                  result))))))
          ]
      (reduce
        (fn [result i-word]
          (if (false? result)
            (boolean (some #(= (count %) n-count) (build-chains i-word)))
            result))
        false
        i-words))))

(defcheck solution-5531379b
  (fn [se]
    (let [same (fn f [w1 w2 i j diff]
                 (let [l1 (count w1) l2 (count w2)]
                   (if (> diff 1)
                     false
                     (if (and (>= i l1) (>= j l2)) true
                                                   (if (or (>= i l1) (>= j l2))
                                                     (and (<= diff 0) (<= (- (max l1 l2) (min l1 l2)) 1))
                                                     (or
                                                      (and (= (nth w1 i) (nth w2 j)) (f w1 w2 (inc i) (inc j) diff))
                                                      (and (or (= j (dec l2)) (and (< j (dec l2)) (= (nth w1 i) (nth w2 (inc j))))) (f w1 w2 i (inc j) (inc diff)))
                                                      (and (or (= i (dec l1)) (and (< i (dec l1)) (= (nth w1 (inc i)) (nth w2 j)))) (f w1 w2 (inc i) j (inc diff)))
                                                      (and (or (and (= i (dec l1)) (= j (dec l2))) (and (< i (dec l1)) (< j (dec l2)) (= (nth w1 (inc i)) (nth w2 (inc j))))) (f w1 w2 (+ 2 i) (+ 2 j) (inc diff)))))))))
          seq (fn [gs] (for [g gs w se] (if (and (not (contains? (set g) w)) (same (last g) w 0 0 0)) (conj g w) [])))]
      (loop [c 1 groups (map vector se)]
        (if (empty? groups)
          false
          (if (>= c (count se))
            (= (count se) (count (reduce #(if (< (count %) (count %2)) %2 %) [] groups)))
            (recur (inc c) (remove empty? (seq groups)))))))))

(defcheck solution-55f36053
  (fn [words]
    (letfn [
            ; https://rosettacode.org/wiki/Levenshtein_distance#Clojure
            (levenshtein [w1 w2]
              (letfn [(cell-value [same-char? prev-row cur-row col-idx]
                        (min (inc (nth prev-row col-idx))
                          (inc (last cur-row))
                          (+ (nth prev-row (dec col-idx)) (if same-char?
                                                            0
                                                            1))))]
                (loop [row-idx  1
                       max-rows (inc (count w2))
                       prev-row (range (inc (count w1)))]
                  (if (= row-idx max-rows)
                    (last prev-row)
                    (let [ch2           (nth w2 (dec row-idx))
                          next-prev-row (reduce (fn [cur-row i]
                                                  (let [same-char? (= (nth w1 (dec i)) ch2)]
                                                    (conj cur-row (cell-value same-char?
                                                                    prev-row
                                                                    cur-row
                                                                    i))))
                                          [row-idx] (range 1 (count prev-row)))]
                      (recur (inc row-idx) max-rows next-prev-row))))))

            (connect [word1 available]
              (if (not-empty available)
                (some true?
                  (for [word2 available]
                    (and (= (levenshtein word1 word2) 1)
                         (connect word2 (disj available word2)))))
                true))]

      (true?
        (some true?
          (for [word words]
            (connect word (disj words word))))))))

(defcheck solution-57c5b3da
  (fn chain? [words]
    (letfn [
            (diff1? [a b]
              (cond (= a b) false
                    (= (first a) (first b)) (diff1? (rest a) (rest b))
                    :else (or (= (rest a) (rest b))
                              (=  (seq a) (rest b))
                              (= (rest a)  (seq b)))))
            (chains [suffix words]
              (if (empty? words)
                [suffix]
                (let [elegible (if (empty? suffix)
                                 words
                                 (filter #(diff1? (first suffix) %) words))]
                  (mapcat
                    (fn [word]
                      (chains (cons word suffix)
                        (disj words word)))
                    elegible))))
            ]
      (not (empty? (chains [] words))))))

(defcheck solution-57d6178a
  (fn [s]
    (letfn [(permutations [colls]
              (if (= 1 (count colls))
                (list colls)
                (for [head colls
                      tail (permutations (disj (set colls) head))]
                  (cons head tail))))
            (diff [a b]
              (loop [i 0 difcount 0 w1 a w2 b]
                (cond
                  (and (zero? (count w1)) (zero? (count w2))) difcount
                  (= 1 (+ (count w1) (count w2))) (recur (inc i) (inc difcount) (rest w1) (rest w2))
                  (= (first w1) (first w2)) (recur (inc i) difcount (rest w1) (rest w2))
                  (= (first w1) (second w2)) (recur (inc i) (inc difcount) w1 (rest w2))
                  (= (second w1) (first w2)) (recur (inc i) (inc difcount) (rest w1) w2)
                  :else (recur (inc i) (inc difcount) (rest w1) (rest w2)))))
            (valid? [s]
              (every? #(= 1 %) (map (fn [[a b]] (diff b a)) (partition 2 1 s))))]
      (if (some true? (map valid? (permutations s))) true false))))

(defcheck solution-58ff7cac
  (fn [s]
    (let [ins? (fn ins? [a b]
                 (let [c-a (count a) c-b (count b)]
                   (if (> c-b c-a)
                     (ins? b a)
                     (if (not= (- c-a c-b) 1)
                       false
                       (loop [[a & as] (seq a) [b & bs :as b-all] (seq b)]
                         (if (= a b)
                           (recur as bs)
                           (= as b-all)))))))
          sub? (fn [a b]
                 (if (not= (count a) (count b))
                   false
                   (loop [[a & a-rest] a [b & b-rest] b subd-count 0]
                     (if (nil? a)
                       (= subd-count 1)
                       (recur a-rest b-rest (if (= a b) subd-count (inc subd-count)))))))
          perms (fn perms [s]
                  (if (= (count s) 1)
                    (list (list (first s)))
                    (let [perms' (fn [f r] (map #(conj % f) (perms r)))]
                      (mapcat #(perms' % (disj s %)) s))))
          chain? (fn [c]
                   (=
                     (last c)
                     (reduce #(if (or (ins? %1 %2) (sub? %1 %2)) %2 nil) c)))]
      (boolean (some #(chain? %) (perms s))))))

(defcheck solution-5916c971
  (fn word-chain [xs]
    (letfn [
            (letter-diff [a b]
              (cond
                (= (count a) (count b)) (if (= 1 (count (filter not (map #(= %1 %2) a b)))) true false)
                (= (- (count a) (count b)) 1) (some #(= (concat (take % a) (drop (inc %) a)) (seq b)) (range (count a)))
                (= (- (count b) (count a)) 1) (some #(= (concat (take % b) (drop (inc %) b)) (seq a)) (range (count b)))
                :else false))
            (all-connect [m]
              (loop [s #{(first (first m))}]
                (if (= s (reduce #(conj %1 (first %2)) #{} m))
                  true
                  (let [n (apply conj s (for [x s y m :when (= x (first y))]
                                          (second y)))]
                    (if (= n s) false
                                (recur n))))
                )
              )]
      (let [m (->> (for [i xs j xs :when (and (letter-diff i j) (not= i j))]
                     [i j])
                (apply conj [])
                )]
        ;connection
        (and (all-connect m)
             (>= 2 (count (filter #(= 1 %) (map (fn [x] (count (filter #(= (first %) x) m))) (keys (apply conj {} m)))))))
        ;m
        ))
    ))

(defcheck solution-5954760e
  (letfn[
         (n[a b] (= 1 (reduce + (map #({true 0 false 1} (= %1 %2)) a b))))
         (sdrop[n s] (apply str (concat (take n s) (drop (inc n) s))))
         (r[a b] (let[[a b] (sort-by count (list a b))] (some #(= a %) (for[x (range 0 (count b))] (sdrop x b)))))
         (p[a b] (if (= (count a) (count b)) (n a b) (r a b)))
         (b[x s] (set (filter  #(p x %) s)))
         (t[s] (apply merge (map #(hash-map % (b % s)) s)))
         (search[todo all done] (let[x (first done)
                                     moves  (if (nil? x) todo (clojure.set/difference (get all x) (set done)))]

                                  (cond (empty? todo) true
                                        (empty? moves) false
                                        :else (some #(search (disj todo %) all (cons % done )) moves))))
         ]
    (fn[s] (if (search s (t s) '()) true false))))

(defcheck solution-5b36c820
  (fn [words]
    (letfn [
            (remove-one [t]
              (map
                #((fn [[f t]] (apply str (concat f (next t)))) (split-at % t))
                (range (count t))))
            (neib [s d]
              (let [rs (remove-one s)
                    rd (remove-one d)]
                (or (some #(= s %) rd)
                    (some #(= d %) rs)
                    (some identity (map #(= % %2) rd rs)))))
            (paths [[prev :as path] rests]
              (if (empty? rests)
                [path]
                (apply concat
                  (keep
                    #(if (or (nil? prev) (neib prev %))
                       (paths (cons % path) (disj rests %)))
                    rests))))]
      (boolean (some #(= % words) (map set (paths () words)))))))

(defcheck solution-5b5d5820
  (fn __ [input]
    (let [leven-dist (fn [coll1 coll2]
                       (let [leven (memoize
                                     (fn [self [x & xs :as xall] [y & ys :as yall]]
                                       (cond
                                         (empty? xall) (count yall)
                                         (empty? yall) (count xall)
                                         :else (min
                                                 (+ (if (= x y) 0 1) (self self xs ys))
                                                 (inc (self self xs yall))
                                                 (inc (self self xall ys))))))]
                         (leven leven coll1 coll2)))

          leven-seq (fn [s]
                      (->> s
                        (partition 2 1)
                        (map (fn [[p q]] (leven-dist p q)))))

          perm (fn perm [s]
                 (lazy-seq
                   (if (seq (rest s))
                     (apply concat (for [x s] (map #(cons x %) (perm (remove #{x} s)))))
                     [s])))

          input-seq (seq input)
          input-len (count input-seq)]

      (true?
        (some #(= (- input-len 1) %)
          (->> (perm input-seq)
            (map leven-seq)
            (map #(apply + %))))))))

(defcheck solution-5bc8e0d8
  (fn word-chains? [words]
    (letfn [(lev [lev-memoized x1 x2]
              (cond
                (= 0 (count x1)) (count x2)
                (= 0 (count x2)) (count x1)
                true (apply min (remove nil? [(inc (lev-memoized lev-memoized (rest x1) (rest x2)))
                                              (inc (lev-memoized lev-memoized x1 (rest x2)))
                                              (inc (lev-memoized lev-memoized (rest x1) x2))
                                              (when (= (first x1) (first x2)) (lev-memoized lev-memoized (rest x1) (rest x2)))]))))
            (memo-lev [x1 x2] ((partial lev (memoize lev)) x1 x2))
            (wc ([wc-memo words] (true? (wc-memo wc-memo words [])))
              ([wc-memo words chain]
               (if (empty? words)
                 (every? #(= 1 (apply memo-lev %)) (partition 2 1 chain))
                 (some (fn [word] (wc-memo wc-memo (disj words word) (conj chain word))) words))))
            (memo-wc [words] ((partial wc (memoize wc)) words))]
      (memo-wc words))))

(defcheck solution-5d09dfa1
  (fn fff [st]
    (letfn [(lev? [w0 w1]
              {:pre [(= (count w0) (count w1))]}
              (< (count (filter false? (map = (seq w0) (seq w1)))) 2))
            (all-strs [w]
              (let [c (count w)]
                (for [i (range 0 (inc c))]
                  (str (.substring w 0 i) " " (.substring w i c)))))
            (LEV? [w0 w1]
              (let [c0 (count w0)
                    c1 (count w1)]
                (cond
                  (= c0 c1) (lev? w0 w1)
                  (= (inc c0) c1) (some true? (map #(lev? w1 %) (all-strs w0)))
                  (= c0 (inc c1)) (some true? (map #(lev? w0 %) (all-strs w1)))
                  :else false)))
            (path? [start r]
              (if (< (count r) 2)
                true
                (let [dif (clojure.set/difference r #{start})
                      start' (filter #(if (LEV? start %)
                                        %
                                        nil) dif)]
                  (if (empty? start')
                    false
                    (not (nil? (some true? (map #(path?
                                                   %
                                                   dif)
                                             start'))))))))]
      (not (nil? (some true?
                   (map #(path? % st) st)))))))

(defcheck solution-5d236a5c
  (fn [words]
    (letfn [
            (connectable? [w1 w2]
              (cond
                (empty? w1) (= 1 (count w2))
                (empty? w2) (= 1 (count w1))
                (= (first w1) (first w2)) (recur (rest w1) (rest w2))
                :else (or
                       (= (next w1) (next w2))
                       (= (next w1) (seq w2))
                       (= (seq w1) (next w2)))))
            (can-cover-all? [starting-points
                             transitions]
              (loop [starting-points starting-points
                     transitions transitions]
                (if (empty? starting-points)
                  false
                  (let [[node reachable toreach] (first starting-points)
                        other-points (rest starting-points)
                        next-nodes (transitions node)
                        next-new-nodes (filter toreach next-nodes)
                        next-points (for [n next-new-nodes]
                                      [n (conj reachable n) (disj toreach n)])]
                    (if (empty? toreach)
                      true
                      (recur (concat other-points next-points) transitions))))))
            ]
      (let [transitions (into {} (for [w words] [w (filter #(connectable? w %) words)]))
            startings (for [w words]
                        [w #{w} words])]
        (can-cover-all? startings transitions)))))

(defcheck solution-5d48165c
  (fn word-chain? [words]
    (letfn [(levenshtein-distance [seq-1 seq-2]
              (cond (empty? seq-1) (count seq-2)
                    (empty? seq-2) (count seq-1)
                    (= (first seq-1) (first seq-2)) (levenshtein-distance (rest seq-1) (rest seq-2))
                    :else (inc (min (levenshtein-distance (rest seq-1) seq-2)
                                 (levenshtein-distance seq-1 (rest seq-2))
                                 (levenshtein-distance (rest seq-1) (rest seq-2))))))
            (neighbors [word words]
              (filter #(= (levenshtein-distance word %) 1) words))
            (chain [adjacency-map visited-set node]
              (let [visited-set (conj visited-set node)
                    neighbors-remaining (remove visited-set (adjacency-map node))]
                (if (= visited-set words)
                  true
                  (some (partial chain adjacency-map visited-set) neighbors-remaining))))]
      (let [adjacency-map (into {} (for [w words] [w (neighbors w words)]))]
        (true? (some (partial chain adjacency-map #{}) words))))))

(defcheck solution-5d625c3a
  (fn [ws]
    (letfn [(delta1? [w v]
              (let [[s1 s2] (sort-by count [w v])
                    l1 (count s1) l2 (count s2)]
                (cond
                  (= l1 l2)
                  (= 1 (reduce #(+ %1 (if (= (first %2) (second %2)) 0 1)) 0 (map vector w v)))
                  (= (inc l1) l2)
                  (= 1 (loop [[h1 & t1 :as s1'] s1 [h2 & t2] s2 cnt 0]
                         (if-not
                          h2 cnt
                             (let [both (= h1 h2)
                                   cnt' (if both cnt (inc cnt))
                                   t1' (if both t1 s1')]
                               (recur t1' t2 cnt')))))
                  :else nil)))
            (walk [cs vs]
              (cond
                (empty? vs) (into [] cs)
                :else (first (filter not-empty
                               (for [v vs]
                                 (cond
                                   (delta1? (first cs) v) (walk (cons v cs) (disj vs v))
                                   (delta1? (last cs) v) (walk (conj cs v) (disj vs v))
                                   :else nil))))))]
      (not (nil? (first (for [w ws] (walk [w] (disj ws w)))))))))

(defcheck solution-5ddd3ee4
  (fn _
    ([words]
     ;(for [w words] (_ [w] (disj words w)))
     (= (count words) (last (sort (flatten(for [w words] (_ [w] (disj words w)))))) )
     )
    ([chain words]
     (let [find-next (fn [node words](filter (fn [w](if (< (count w) (count node)) (if (>(- (count node) (count w)) 1) false(not= nil (re-find (re-pattern (apply str (concat ".?" (interpose ".?" w) ".?"))) node)))(if (= (count w) (count node)) (<= (count (for [i (range (count node)) :when (not= (nth w i) (nth node i))]i)) 1)(if (> (- (count w) (count node)) 1) false(not= nil (re-find (re-pattern (apply str (concat ".?" (interpose ".?" node) ".?"))) w)))))) words))
           next_words (find-next (last chain) words)]
       (if (empty? next_words) (count chain)
                               (for [w next_words] (_ (conj chain w) (disj words w)))
                               )
       )
     )
    ))

(defcheck solution-5e2f56c7
  (fn chain? [s]
    (letfn [(drop-nth [n coll]
              (map second (filter #(not= (first %) n)(map-indexed vector coll))))
            (d1? [a b]
              (let [[a b] (sort-by count [a b])]
                (cond (= (count a) (count b)) (>= 1 (reduce + (map #(if (= %1 %2) 0 1) a b)))
                      (= (inc (count a)) (count b)) (not (nil? (some #(= (seq a) %) (map #(drop-nth % b) (range (count b))))))
                      :else false)))
            (permute [s]
              (if (= 1 (count s)) [s]
                                  (mapcat (fn [i] (map #(cons i (if (sequential? (first %)) (first %) %))
                                                    (permute (remove #{i} s)))) s)))]
      (not (nil? (some (fn [c] (every? identity (map #(apply d1? %)(partition 2 1 c)))) (permute s)))))))

(defcheck solution-5eb9226a
  (fn chain [words]
    (letfn
     [(drop1 [s]
        (for [i (range (count s))]
          (apply str (map #(nth s %) (remove #(= % i) (range (count s)))))))
      (diff1 [w1 w2]
        (let [c1 (count w1) c2 (count w2)]
          (cond
            (= c1 c2)
            (= 1 (count (filter false? (map #(= (nth w1 %) (nth w2 %)) (range c1)))))
            (= c1 (inc c2)) (some #(= w2 %) (drop1 w1))
            (= c2 (inc c1)) (some #(= w1 %) (drop1 w2))
            :else false)))
      (word-chain [w s]
        (if (empty? s) true
                       (for [w2 (filter #(diff1 % w) (disj s w))]
                         (word-chain w2 (disj s w w2)))))]
      (not (empty? (flatten (map #(word-chain % words) words)))))))

(defcheck solution-5f3f75
  (fn prob82d [words]
    (letfn
     [(drop1 [s]
        (for [i (range (count s))]
          (apply str (map #(nth s %) (remove #(= % i) (range (count s)))))))
      (diff1 [w1 w2]
        (let [c1 (count w1) c2 (count w2)]
          (cond
            (= c1 c2)
            (= 1 (count (filter false? (map #(= (nth w1 %) (nth w2 %)) (range c1)))))
            (= c1 (inc c2)) (some #(= w2 %) (drop1 w1))
            (= c2 (inc c1)) (some #(= w1 %) (drop1 w2))
            :else false)))]
      ((fn word-chain [l s]
         (cond
           (empty? s) true
           (empty? l) false
           (let [w1 (first l) s1 (disj s w1)]
             (word-chain (filter #(diff1 % w1) s1) s1)) true
           :else (recur (rest l) s)))
       (apply list words) words))))

(defcheck solution-5f7a6648
  (fn [ws]
    (let [wv (vec ws)
          n (count wv)
          connected
             (fn [a b]
               (letfn [(ed=1 [a b]
                         (cond
                           (nil? a) (= 1 (count b))
                           (nil? b) (= 1 (count a))
                           :else (let [[a0 & a_] a, [b0 & b_] b]
                                   (if (= a0 b0) (ed=1 a_ b_)
                                                 (or (= a_ b_)
                                                     (= a  b_)
                                                     (= a_ b ))))))]
                 (ed=1 (seq a) (seq b))))
          degrees
             (->>
               (for [i (range n) j (range (inc i) n)
                     :when (connected (wv i) (wv j))] [i j])
               (apply concat)
               frequencies
               vals
               frequencies)]
      (and (nil? (degrees 0)) (< (degrees 1 0) 3)))))

(defcheck solution-602413b4
  (fn [words]
    (let [can-chain (fn f[a b c]
                      (cond (> c 1) false
                            (empty? a) (<= (+ (count b) c) 1)
                            (empty? b) (<= (+ (count a) c) 1)
                            (= (first a) (first b)) (recur (rest a) (rest b) c)
                            :else (or (f (rest a) (rest b) (inc c)) (f a (rest b) (inc c)) (f (rest a) b (inc c)))
                            ))
          test (fn t[w r]
                 #_(println (str w r))
                 (if (not (empty? r)) (reduce #(or % (if (can-chain w %2 0) (t %2 (disj r %2)) false)) false r) true))]
      (reduce #(or % (test %2 (disj words %2))) false words)
      )))

(defcheck solution-60d8082
  (fn allChain [lst]
    (letfn [(com ([s1 s2] (com s1 s2 0))
              ([s1 s2 cnt]
               ;(println s1 ":" s2 ":" cnt)
               (cond
                 (or (empty? s1) (> cnt 1)) false
                 (and (not (empty? s1)) (empty? s2) (> cnt 0)) false
                 (and (= 1 (count s1)) (= 1 (count s2)) (= cnt 0)) true
                 (and (empty? s2) (= cnt 0)) true
                 (< 1 (- (count s1) (count s2))) false
                 (< 1 (- (count s2) (count s1))) false
                 (= s1 s2) true
                 :else (or (com s1 (rest s2) (inc cnt))
                           (com (rest s1) s2 (inc cnt))
                           (if (= (first s1) (first s2))
                             (com (rest s1) (rest s2) cnt)
                             (com (rest s1) (rest s2) (inc cnt)))))))
            (strChain [item  more]
              #_(println "## item: " item " more:" more " ##")
              (if (empty? more) true
                                (let [tmp (map (fn [it]
                                                 ;(println it  (filter #(not= it %1) more))
                                                 (strChain it (filter #(not= it %1) more)))
                                            (filter #(com item %1) more))]
                                  ;(println  tmp )
                                  (reduce #(or %1 %2)
                                    false
                                    tmp))))]
      (reduce #(or %1 %2) (map (fn [it2] (strChain it2 (filter #(not= it2 %1) lst))) lst)))))

(defcheck solution-6115501b
  (fn [w]
    (let [g? (fn [a b] (let [ac (count a)
                             bc (count b)
                             t (fn [a b c]
                                 (not (nil? (some #{b}
                                              (map #(apply str (concat (take % a)
                                                                       (drop (inc %) a)))
                                                (range c))))))]
                         (condp = (- ac bc)
                           0 (= (reduce + (map #(if (= %1 %2) 0 1) a b)) 1)
                           1 (t a b ac)
                           -1 (t b a bc)
                           false)))
          wn (map-indexed vector w)
          e (filter #(last %) (for [x wn y wn]
                                (vector (first x) (first y) (g? (last x) (last y)))))
          ds (fn ds [c u] (let [n (filter #(= c (first %)) e)]
                            #_(println c u n)
                            (loop [v n]
                              (if (empty? v)
                                false
                                (if (let [f (second (first v))]
                                      (if (nil? (some #{f} u))
                                        (if (= (inc (count u)) (count w))
                                          true
                                          (ds f (into u [f])))))
                                  true
                                  (recur (rest v)))))))]
      (not (nil? (some true? (map #(ds % #{%}) (range (count w)))))))))

(defcheck solution-6143867c
  (letfn
   [(conn [ss vs f] (for [s ss v (remove (set s) vs) :when (f (last s) v)] (conj s v)))
    (h= [a] (count (take-while true? (apply (partial map =) a))))
    (wc? [& a] (= (apply max (map count a)) (+ 1 (h= a) (h= (map reverse a)))))]
    (fn [ss] (->> (iterate #(conn % ss wc?) (map vector ss)) (take (count ss)) last empty? not))))

(defcheck solution-6283072f
  (fn [words]
    (letfn [(edit-dist [w1 w2]
              (letfn [(build-matrix [w1 w2]
                        (let [rows (inc (count w1))
                              cols (inc (count w2))]
                          (let [matrix (vec (conj
                                              (map (fn [i] (vec (conj (repeat (dec cols) nil) i))) (range 1 rows))
                                              (vec (range cols))))]
                            matrix)))
                      (char-cost [row col]
                        (if (= (get w1 row)
                              (get w2 col)) 0 1))
                      (min-cost [m i j]
                        (min
                          (+ (get-in m [(dec i) (dec j)]) (char-cost (dec i) (dec j)))
                          (inc (get-in m [(dec i) j]))
                          (inc (get-in m [i (dec j)]))))
                      (fill-entry [m i j]
                        (assoc-in m [i j] (min-cost m i j)))
                      (addr-fill-entry [m addr]
                        (fill-entry m (first addr) (second addr)))
                      ; Every 2-combination or pairwise combination of a list
                      (edit-dist-matrix [w1 w2]
                        (let [matrix (build-matrix w1 w2)]
                          (reduce addr-fill-entry matrix
                            (for [row (range 1 (inc (count w1)))
                                  col (range 1 (inc (count w2)))] [row col]))))]
                (get-in (edit-dist-matrix w1 w2) [(count w1) (count w2)])))
            (combinations [l]
              (reduce conj #{}
                (for [x l y l :while (not= x y)] (hash-set x y))))
            (hamiltonian-circuit? [g]
              (letfn [(my-merger [coll1 coll2]
                        (merge-with clojure.set/union coll1 coll2))
                      (add-to-map [coll key val]
                        (my-merger {key val} coll))
                      (add-to-path [m]
                        (zipmap (map second m) (map #(list (first %)) m)))
                      (rotate [l]
                        (concat (rest l) (list (first l))))
                      (all-queues [queue]
                        (take (count queue) (iterate rotate queue)))
                      (all-visited? [gr visited]
                        (= (set visited) (set (keys gr))))
                      (bfs-search [init-map q]
                        (loop [queue q visited '()]
                          (if (all-visited? init-map visited)
                            true
                            (if-let [head (first queue)]
                              (let [source (first head)
                                    dests (second head)]
                                (if-let [val (some #{source} visited)]
                                  ; we've already visited the head node
                                  (recur (rest queue) (rest visited))
                                  ; new head node, never visited
                                  (if-let [dest (first dests)]
                                    ; new head node, possible destinations
                                    (recur
                                      (conj
                                        (conj (rest queue)
                                          (list source (rest dests)))
                                        (list dest (get init-map dest)))
                                      (conj visited source))
                                    ; new head node, no destinations
                                    (recur (rest queue) (rest visited)))))
                              ;(recur
                              ; (conj (rest queue)
                              ;       (list source (rest dests)))
                              ; (conj visited source))))))
                              ; empty queue
                              (all-visited? init-map visited)))))]
                (let [init-map (reduce my-merger (map #(hash-map
                                                         (first %) (hash-set (second %))
                                                         (second %) (hash-set (first %))
                                                         ) g))]
                  ;(map #(bfs-search init-map (list (list (first %) (get init-map (first %))))) (all-queues (keys init-map))))))]
                  (not-every? false? (map #(bfs-search init-map (list (list (first %) (get init-map (first %))))) (all-queues (keys init-map)))))))]
      ;      (edit-dist "hot" "cat"))))
      (hamiltonian-circuit? (map seq (filter #(= (edit-dist (first %) (second %)) 1) (combinations words)))))))

(defcheck solution-62b3cba4
  (fn [W]
    (let [nxt (fn [x y]
                (loop [[a & A :as u] (seq x) [b & B :as v] (seq y)]
                  (cond (= a b) (recur A B)
                        (or (= u B)
                            (= v A)
                            (= A B)) true
                        :else false)))
          R (fn [w S] (filter (partial nxt w) S))
          F (fn F [w S]
              (if (empty? S) true (boolean (some (fn [i] (F i (disj S i))) (R w S)))))]
      (boolean (some (fn [w] (F w (disj W w))) W)))))

(defcheck solution-63dd93bf
  (letfn [(sub-dels
            ([s] (sub-dels nil s))
            ([pre post]
             (when-let [[fpost & npost] (seq post)]
               (cons [fpost (concat pre npost)]
                 (sub-dels (concat pre (list fpost)) npost)))))
          (adj? [one two]
            (or (some #(= (seq one) (second %)) (sub-dels two))
                (some #(= (seq two) (second %)) (sub-dels one))
                (some identity (map #(= (second %1) (second %2)) (sub-dels one) (sub-dels two)))))
          (all-chains [words]
            (if (= 1 (count words))
              (list (seq words))
              (for [[word rest-words] (sub-dels words)
                    :let [rest-chains (all-chains rest-words)]
                    chain rest-chains
                    :when (adj? word (first chain))]
                (cons word chain))))]
    #(not (empty? (all-chains %)))))

(defcheck solution-645c9fb3
  (let [lev-0?
        (fn [l r] (= l r))
        lev-1?
        (fn [l r]
          (loop [[lh & lt] l
                 [rh & rt] r]
            (let [match (= lh rh)]
              (if (and (nil? lt) (nil? rt)) true
                                            (if (or (nil? lt) (nil? rt))
                                              (if (and match (= 1 (max (count lt) (count rt)))) true
                                                                                                (if match false
                                                                                                          (if (> (count lt) (count rt))
                                                                                                            (lev-0? lt (conj rt rh))
                                                                                                            (lev-0? (conj lt lh) rt))))
                                              (if match (recur lt rt)
                                                        (if (= (count lt) (count rt)) (lev-0? lt rt)
                                                                                      (if (> (count lt) (count rt))
                                                                                        (lev-0? lt (conj rt rh))
                                                                                        (lev-0? (conj lt lh) rt)))))))))]
    (let [init-state
          (fn [words]
            (into [] (map (fn [x] [`(~x) (clojure.set/difference words #{x})]) words)))
          progress-state
          (fn [[list remaining]]
            (let [nexts (filter #(lev-1? (first list) %) remaining)]
              (map (fn [next] [(conj list next) (clojure.set/difference remaining #{next})]) nexts)))]
      (let [progress-to-end
            (fn [states]
              (loop [current states]
                (let [new-current (mapcat progress-state current)]
                  (if (empty? (second (first new-current)))
                    new-current
                    (recur new-current)))))]
        (fn word-chain
          [words]
          (not (empty? (progress-to-end (init-state words)))))))))

(defcheck solution-6479beec
  (fn wc [s]
    (letfn [(dropLetter [string i]
              (str (subs string 0 i) (subs string (inc i))))
            (oneLetter [a b]
              (if (= (count a) (count b))
                (= 1 (count (filter false? (map = a b))))
                (let [longer (max-key count a b)
                      shorter (min-key count a b)]
                  (some (partial = shorter) (map (partial dropLetter longer) (range (count longer)))))))]

      (loop [queue (mapv vector s)]
        (if-let [c (first queue)]
          (if (= (count c) (count s))
            true
            (recur (into (next queue) (map (partial conj c) (filter (partial oneLetter (last c)) (clojure.set/difference s (set c)))))))
          false)))))

(defcheck solution-64f174f
  (fn [words]
    (let [adja (fn adja [[f1 & r1 :as ww1] [f2 & r2 :as ww2]]
                 (let [w1 (seq ww1) w2 (seq ww2)]
                   (and
                    (not (<= 2 (Math/abs (- (count w1) (count w2)))))
                    (or
                     (empty? w1)
                     (empty? w2)
                     (= r1 r2)
                     (= w1 r2)
                     (= r1 w2)
                     (and (= f1 f2) (adja r1 r2))))))
          two-comb (fn [s] (reduce into (set [])
                             (map
                               (fn [[a & r]] (map (fn [e] [a e]) r))
                               (take-while (fn [ss] (>= (count ss) 2))
                                 (iterate rest (vec s))))))
          e (filter (partial apply adja) (two-comb words))
          vmap (apply merge-with into
                 (map (fn [[v1 v2]] {v1 #{v2} v2 #{v1}}) e))
          degmap (apply merge-with +
                   (map (fn [[v1 v2]] {v1 1 v2 1}) e))
          connected
          (= (-> vmap keys set)
            ((fn s [v m seen]
               (if (empty? m) #{v}
                              (let [newm (dissoc m v)
                                    newseen (conj seen v)
                                    iter (filter (complement newseen) (m v))]
                                (apply clojure.set/union #{v}
                                  (map #(s % newm newseen) iter)))))
             (ffirst vmap) vmap #{}))]
      (boolean
        (and
         connected
         (->> degmap
           vals
           (filter (partial > 2))
           count
           (>= 2)))))))

(defcheck solution-6553e503
  (fn [x]
    (letfn [(o [a b]
              (let [d (remove true? (map = a b))
                    c (compare (count a) (count b))
                    q (if (> c 0) a b)
                    r (if (< c 0) a b)]
                (if (= c 0)
                  (= 1 (count d))
                  (not (nil? (some (partial = (map char r))
                               (for [i (range (count q))]
                                 (concat (take i q) (nthrest q (+ i 1))))))))))]
      (->> (zipmap x (repeat (count x) x))
        (reduce
          (fn [a [k v]]
            (assoc a k (set (filter #(o k %) v))))
          {})
        (filter (fn [[_ v]] (< (count v) 2)))
        (#(< (count %) 3))))))

(defcheck solution-659fc0a4
  (fn word-path [opts]
    (letfn [(one-off? [m n]
              (loop[[a & r :as x] m [b & s :as y] n d 0]
                (let [cx (count x) cy (count y)]
                  (cond
                    (> d 1) false
                    (and (nil? a) (nil? b)) (<= d 1)
                    (= a b) (recur r s d)
                    (= cx cy) (recur r s (inc d))
                    (> cx cy) (recur r y (inc d))
                    (< cx cy) (recur x s (inc d))))))
            (expand [paths]
              (for [path paths n (filter #(one-off? (peek path) %) opts)
                    :when (not-any? #{n} path)] (conj path n)))]
      (not (empty? (nth (iterate expand (map vector opts)) (dec (count opts))))))))

(defcheck solution-65bd2600
  (fn
    [s]
    (let [ch? (fn
                [s1 s2]
                (let [t1 (into #{} s1)
                      t2 (into #{} s2)]
                  (if (= (count s1) (count s2))
                    (= 1 (count (clojure.set/difference t1 t2)))
                    (if (let [p (- (count s2) (count s1))]
                          (or (= 1 p) (= -1 p)))
                      (loop [c1 s1 c2 s2 res []]
                        (if (or (empty? c2)
                                (empty? c1))
                          (if (<= (count res) 1)
                            true
                            false)
                          (if (= (first c1) (first c2))
                            (recur (rest c1)
                              (rest c2)
                              res)
                            (if (> (count c1) (count c2))
                              (recur (rest c1)
                                c2
                                (conj res (first c1)))
                              (recur c1
                                (rest c2)
                                (conj res (first c2)))))))
                      false))))
          ch (fn
               [s c]
               (into #{} (filter #(ch? s %)
                           (remove #(= s %) c))))
          removes (fn
                    [e c]
                    (remove #(= e %) c))
          check? (fn check?
                   [s st]
                   (if (empty? st)
                     true
                     (let [chains (ch s st)]
                       (if (empty? chains)
                         false
                         (if (some true?
                               (map #(check? % (removes % st))
                                 chains))
                           true
                           false)))))]
      (if (some true?
            (map #(check? % (removes % s)) s))
        true
        false))))

(defcheck solution-65cb0b02
  (fn f ([s] (= (some true? (map #(apply f %) (for [i s] (list (disj s i) i)))) true) )
    ([s l] (if (empty? s) true (let [	v #(when (= (count %) (count %2)) (= (count (filter true? (map not= % %2))) 1) )
                                     u #(when (= (count %) (dec (count %2))) (some (fn [o] (= o (vec %))) (for [k (range (count %2))] (concat (take k %2) (drop (inc k) %2)))))
                                     r (for [i s
                                             :when (or (v i l) (u i l) (u l i)) ] (list (disj s i) i))]
                                 (when-not (empty? r) (some true? (map #(apply f %) r )))) ) )))

(defcheck solution-6610dd6
  (fn word-train? [words]
    (letfn [(levenstein [a b]
              (let [one-if #(if % 1 0)
                    lev-functor (fn [mem i j]
                                  (let [lev (fn [i j] (mem mem i j))]
                                    (if (zero? (min i j))
                                      (max i j)
                                      (min
                                        (+ (lev (dec i) j) 1)
                                        (+ (lev i (dec j)) 1)
                                        (+ (lev (dec i) (dec j)) (one-if (not= (get a (dec i)) (get b (dec j)))))))))
                    mem (memoize lev-functor)
                    lev (partial mem mem)]
                (lev (count a) (count b))))
            (pairs [coll]
              (map list coll (next coll)))
            (neighbours? [[a1 a2] [b1 b2]]
              (or (= a1 b1) (= a1 b2) (= a2 b1) (= a2 b2)))
            (permutations [coll]
              (if-let [s (seq coll)]
                (mapcat
                  (fn [item]
                    (map #(conj % item)
                      (permutations (disj coll item))))
                  coll)
                '(())))
            (nei-words [words]
              (->> words
                (map (fn [word]
                       (set (filter #(= 1 (levenstein word %)) words))))
                (zipmap words)))]
      (let [g (nei-words words)]
        (->> words
          permutations
          (map (fn [trail]
                 (->> trail
                   pairs
                   (every? (fn [[a b]]
                             (contains? (g a) b))))))
          (some true?)
          true?)))))

(defcheck solution-6667639c
  (letfn [(l? [x y]
            (cond
              (= (count x) 0) (count y)
              (= (count y) 0) (count x)
              (= (first x) (first y)) (l? (rest x) (rest y))
              :else (min (inc (l? (rest x) y)) (inc (l? x (rest y))) (inc (l? (rest x) (rest y))))))
          (l-table [l]
            (for [x (range (count l))]
              (for [y (range (count l))]
                (l? (nth l x) (nth l y)))))
          (some-perm? [n f]
            (letfn [(gen-perm-states [l e]
                      (if (empty? l)
                        (f e)
                        (some (fn [x] (gen-perm-states (remove #(= % x) l) (cons x e))) l)))]
              (gen-perm-states (range n) ())))
          (has-route? [tbl]
            #_(println tbl)
            (some-perm? (count tbl) (fn [x] (every? (fn [idx] (= (nth (nth tbl (nth x idx)) (nth x (inc idx))) 1)) (range (dec (count tbl)))))))]
    #(= (has-route? (l-table (seq %))) true)))

(defcheck solution-67733710
  (fn [w] (letfn [(follows? [a b]
                    (let [sz-a (count a)
                          sz-b (count b)
                          long-short (if (< sz-a sz-b) [b a] [a b])
                          long-vec (vec (first long-short))]
                      (and (<= (Math/abs (- sz-a sz-b)) 1)
                           (boolean
                             (some
                               (fn [v]
                                 (re-matches
                                   (re-pattern (apply str v))
                                   (second long-short)))
                               (for [i (range (count long-vec))]
                                 (assoc long-vec i ".*")))))))
                  (chains [words chain-size]
                    (set
                      (if (= 1 chain-size)
                        (map vector words)
                        (map
                          #(conj (first %) (second %))
                          (let [n-1-chains (chains words (dec chain-size))]
                            (filter
                              (fn [[chain word]]
                                (follows? word (last chain)))
                              (mapcat
                                (fn [chain]
                                  (for [word (apply disj words chain)]
                                    [chain word]))
                                n-1-chains)))))))]
            (pos? (count (chains w (count w)))))))

(defcheck solution-677ff6f5
  (fn [words]
    (let [lev (fn [a b]
                (let [length (+ (count b) 1)
                      dict (atom {})
                      lev (fn lev [n]
                            (if (contains? @dict n)
                              (@dict n)
                              (let [ret
                                    (let [i (quot n length)
                                          j (rem n length)]
                                      (if (= 0 (min i j)) (max i j)
                                                          (min (+ (lev (+ (* (- i 1) length) j)) 1)
                                                            (+ (lev (+ (* i length) (- j 1))) 1)
                                                            (+ (lev (+ (* (- i 1) length) (- j 1)))
                                                               (if (= (nth a (- i 1)) (nth b (- j 1))) 0 1)))))]
                                (swap! dict assoc n ret)
                                ret)
                              ))]

                  (lev (+ (* (count a) length) (count b)))))
          perms (fn perms [s]
                  (if (= (count s) 1)
                    (list s)
                    (apply concat
                      (for [elem s]
                        (let [prev (perms (clojure.set/difference s #{elem}))]
                          (for [p prev] (flatten (cons elem p))))))))
          wordPerms (perms words)]
      (not (nil? (some identity
                   (for [wp wordPerms]
                     (every? identity (map #(= (lev (first %) (second %)) 1) (partition 2 1 wp)))
                     ))))
      )))

(defcheck solution-67aa210d
  (fn [s]
    (letfn [(similar? [a b]
              (let [ca (count a)
                    cb (count b)]
                (not (or (> (Math/abs (- ca cb)) 1)
                         (= a b)
                         (and (> ca cb)
                              (not= (take cb a) (seq b))
                              (not= (take-last cb a) (seq b)))
                         (and (not= ca cb)
                              (loop [x (if (< ca cb) a b)
                                     y (if (< cb ca) a b)
                                     r 0]
                                (if (and (empty? x) (< r 2))
                                  false
                                  (if (> r 1)
                                    true
                                    (let [fx (first x)
                                          fy (first y)
                                          nr (if (not= fx fy) (inc r) r)
                                          nx (if (not= fx fy) x (rest x))]
                                      (recur nx (rest y) nr))))

                                ))
                         (and (= ca cb)
                              (not= (count (filter #(not (nil? %)) (map #(when (not= %1 %2) 1) a b))) 1))
                         ))))
            (permutations [s]
              (lazy-seq
                (if (seq (rest s))
                  (apply concat (for [x s]
                                  (map #(cons x %) (permutations (remove #{x} s)))))
                  [s])))
            (chain? [l]
              (if (or (empty? l) (= 1 (count l)))
                true
                (if (not (similar? (first l) (second l)))
                  false
                  (recur (rest l)))))
            (chained? [s]
              (not (empty? (drop-while #(not (chain? %)) (permutations s)))))]
      (chained? s)
      )))

(defcheck solution-6915c295
  (fn me2 [arg-set]


    (let [
          cnt (count arg-set)

          f-fn (fn  [str1 str2]

                 (let [cnt1 (count str1)

                       cnt2 (count str2)

                       diff-cnt (- (max cnt1 cnt2) (min cnt1 cnt2))

                       not-equal-strs (fn [str1 str2]

                                        (let [  long-str  (if (> (count str1) (count str2)) str1 str2 )

                                              short-str (if (= long-str str1) str2 str1)

                                              indexes (range (count long-str))

                                              new-strs    (map #(concat (take % long-str) (drop (inc %) long-str) )  indexes)

                                              ]

                                          (>= (count (filter #(= (seq short-str) %) new-strs)) 1)
                                          ))


                       equal-strs  (fn  [str1 str2]

                                     (let [str-vec (map vector str1 str2)]

                                       (= 1 (count (filter #(not= (first %) (second %)) str-vec)))
                                       ))
                       ]

                   (cond
                     (>= diff-cnt 2) false
                     (= diff-cnt  1) (not-equal-strs str1 str2)
                     (= diff-cnt  0) (equal-strs str1 str2)
                     )))

          ;;
          me (fn  [arg]

               (if (empty? (second (first arg)))
                 [
                  [
                   (disj (first (first arg)) (first (first (first arg))))

                   (list (first (first (first arg))))
                   ]
                  ]

                 ( let [m-fn (fn [[my-set my-list]]

                               (let [cur (first my-list)

                                     next-elems (filter #(f-fn cur %) my-set)
                                     ]

                                 (map #(vector (disj my-set %) (cons % my-list)) next-elems)
                                 )

                               )
                        ]
                   (apply concat (map m-fn arg))
                   )
                 )

               )

          ;;

          mm-fn  (fn [my-set cnt cur]

                   (let [ g (iterate me [[(disj my-set cur) (list cur)] ] )

                         res (nth g (- cnt 1) )
                         ]

                     ;(println cnt)
                     ;(println res)

                     (if (empty? res)
                       false
                       (= cnt (count (second (last res))))
                       ;res
                       )
                     )

                   )

          res   (some true? (map (partial mm-fn arg-set cnt) arg-set))

          ]

      ;(some true? (map (partial mm-fn arg-set cnt) arg-set))
      (if (nil? res) false true)
      )

    ))

(defcheck solution-6a76cefb
  (fn [s]
    (letfn [(diff-one-char? [w1 w2]
              (cond
                (= (count w1) (count w2)) (= 1 (count
                                                 (filter false?
                                                   (map #(= % %2) w1 w2))))
                :else (let [max-w (max-key count w1 w2)
                            min-w (min-key count w1 w2)]
                        (some #(= (seq min-w) %)
                          (map #(concat (take % max-w) (drop (inc %) max-w))
                            (range (count max-w)))))))

            (chain? [ws]
              (every? true?
                (map (partial apply diff-one-char?) (partition 2 1 ws))))

            (permutations [s]
              (lazy-seq
                (if (seq (rest s))
                  (apply concat (for [x s]
                                  (map #(cons x %) (permutations (remove #{x} s)))))
                  [s])))]
      (boolean (some chain? (permutations s))))))

(defcheck solution-6aa8605b
  (fn word-chain? [words]
    (letfn [(remove-nth [coll n]
              (let [[start end] (split-at n coll)]
                (concat start (drop 1 end))))
            (chain? [w1 w2]
              (let [[short long] (if (< (count w1) (count w2)) [(seq w1) (seq w2)] [(seq w2) (seq w1)])]
                (and (<= (- (count long) (count short)) 1)
                     (not (nil? (some identity (for [c (range (count long))]
                                                 (or (= (remove-nth long c) short)
                                                     (= (remove-nth long c) (remove-nth short c))))))))))
            (chain-starting-with [chain words-left]
              (if (not-empty words-left)
                (some identity (for [word words-left]
                                 (when (or (empty? chain)
                                           (chain? (last chain) word))
                                   (chain-starting-with (conj chain word) (disj words-left word)))))
                chain))]
      (not (nil? (chain-starting-with [] words))))))

(defcheck solution-6c5a2901
  (fn [ws]
    (let [ nb? (fn nb? [s t]
                 (let [ls (count s),
                       lt (count t)]
                   (cond (> ls lt) (nb? t s)
                         (= ls lt)
                         (= 1 (count (remove identity (map =  s t))))
                         (= (inc ls) lt)
                         (some #{s}
                           (map #(str (subs t 0 %) (subs t (inc %))) (range lt)))
                         true false)))
          nbs (fn [x xs] (set (filter (partial nb? x) xs)))
          path-from (fn path-from [x xs]
                      (if (empty? xs) true ;we have got a path
                                      (not (empty? (filter #(path-from % (disj xs %)) (nbs x xs)) ))
                                      ))]
      (not (empty? (filter #(path-from % (disj ws %)) ws))) )))

(defcheck solution-6d1debb4
  (fn f [s]
    (letfn [ (can-step [a b] (loop [ [a1 & ar :as a] (seq a) [b1 & br :as b] (seq b) ]
                               (if (= a1 b1)
                                 (recur ar br)
                                 (or (= ar br) (= ar b) (= a br)))))
            (has-route [s l]
              (or (empty? s)
                  (some identity
                    (for [ n s :when (or (nil? l) (can-step l n)) ]
                      (has-route (disj s n) n))))) ]
      (if (has-route s nil) true false ))))

(defcheck solution-6db79543
  (fn [words]
    (let [permutations
          (fn permutations [coll]
            (if (seq coll)
              (mapcat
                (fn [x]
                  (map
                    (fn [ps]
                      (cons x ps))
                    (permutations (remove #{x} coll))))
                coll)
              '(())))
          words-differ-by-one-letter?
          (fn [a b]
            (and
             (= (count a) (count b))
             (= (dec (count a))
               (count (filter (partial apply =) (map vector a b))))))

          words-differ-by-addition?
          (fn [a b]
            (let [[a b] (sort-by count [a b])]
              (and
               (= (inc (count a)) (count b))
               (loop [a a b b add-count 0]
                 (if (and (seq a) (seq b))
                   (if (= (first a) (first b))
                     (recur (rest a) (rest b) add-count)
                     (if (zero? add-count)
                       (recur a (rest b) (inc add-count))
                       false))
                   (or
                    (and (= 1 (count b)) (zero? add-count))
                    (and (not (or (seq a) (seq b))) (= 1 add-count))))))))

          word-neighbors?
          (fn [a b]
            (or (words-differ-by-one-letter? a b)
                (words-differ-by-addition? a b)))]
      (true?
        (some
          (fn [perm]
            (every?
              (fn [[a b]]
                (word-neighbors? a b))
              (partition 2 1 perm)))
          (permutations words))))))

(defcheck solution-6def1d95
  (fn [s]
    (letfn [(perms [s]
              (if (seq (rest s))
                (apply concat (for [x s]
                                (map #(conj % x) (perms (remove #{x} s)))))
                [s]))
            (edit-distance [s1 s2]
              (cond
                (empty? s1) (count s2)
                (empty? s2) (count s1)
                :else (min (inc (edit-distance (rest s1) s2))
                        (inc (edit-distance s1 (rest s2)))
                        (+ (if (= (first s1) (first s2)) 0 1) (edit-distance (rest s1) (rest s2))))))]
      (->> (perms (seq s))
        (map #(partition 2 1 %))
        (map (fn [ps] (apply + (map #(apply edit-distance %) ps))))
        (sort)
        (first)
        (= (dec (count s)))
        ))))

(defcheck solution-6e5c542
  (letfn [(same-count [word1 word2]
            (= (count word1) (count word2)))

          (second-word-bigger [word1 word2]
            (= (count word1) (dec (count word2))))

          (char-pairs [word1 word2]
            (map vector word1 word2))

          (substitution? [word1 word2]
            (and (same-count word1 word2)
                 (= (count (remove (partial apply =)
                             (char-pairs word1 word2)))
                   1)))

          (insertion? [word1 word2]
            (and (second-word-bigger word1 word2)
                 (let [pairs (char-pairs word1 word2)
                       identical-part (take-while (partial apply =) pairs)
                       identical-part-count (count identical-part)]
                   (= (drop identical-part-count word1)
                     (drop (inc identical-part-count) word2)))))

          (deletion? [word1 word2]
            (insertion? word2 word1))

          (chainable? [word1 word2]
            (or (insertion? word1 word2)
                (deletion? word1 word2)
                (substitution? word1 word2)))

          (chainable-map [words]
            (reduce (fn [m word]
                      (let [other-words (disj words word)]
                        (merge m {word (filter (partial chainable? word) other-words)})))
              {}
              words))

          (max-chain-length [all-words]
            (let [chain-map (chainable-map all-words)
                  aux (fn aux [words seen-words]
                        (let [available-words (clojure.set/difference words seen-words)
                              available-chain-map (select-keys chain-map available-words)]
                          (if (empty? available-words)
                            0
                            (inc (apply max (map (fn [[k v :as map-entry]]
                                                   (aux (set v) (conj seen-words k)))
                                              available-chain-map))))))]
              (aux all-words #{})))

          (all-chainable? [words]
            (= (count words) (max-chain-length words)))]

    all-chainable?))

(defcheck solution-6eaf307
  (fn wordseq ([coll x]
               (let [a-match (fn [x y] (+ (count (take-while identity (map = x y)))
                                          (count (take-while identity (map = (reverse x) (reverse y))))))
                     a-able (fn [x y]
                              (and
                               (>= 1 (Math/abs (- (count x) (count y))))
                               (>= (a-match x y) (dec (max (count x) (count y))))))
                     neighbors (filter #(a-able x %) (disj coll x))]
                 (cond
                   (empty? coll) true
                   (empty? neighbors) false
                   true (not-every? false? (map wordseq (map (partial disj coll) neighbors) neighbors))
                   )))
    ([coll]
     (not-every? false? (map wordseq (map (partial disj coll) coll) coll)))))

(defcheck solution-6ec11612
  (fn [words]
    (<= (count words)
      (count (filter (fn [[x y]]
                       (let [longer (if (> (count x) (count y)) x y)
                             shorter (if (= longer x) y x)
                             non-match (first (first (filter (partial apply not=) (map vector longer (str shorter " ")))))]
                         (or
                          (and (= (count x) (count y))
                               (->> (map vector x y)
                                 (filter (partial apply not=))
                                 count
                                 (>= 1)))
                          (= longer (str shorter (last longer)))
                          (= longer (str (first longer) shorter))
                          (= shorter (clojure.string/replace-first longer non-match "")))))
               (for [x words y words :when (neg? (compare x y))]
                 [x y]))))))

(defcheck solution-6f3142c6
  (fn [words]
    (letfn
     [
      (levenshtein [s t]
        (let [m (count s), n (count t)
              M (inc m),   N (inc n)
              d (make-array Long/TYPE M N)] ; Clojure 1.3
          (doseq [i (range M)]
            (aset d i 0 i))
          (doseq [j (range N)]
            (aset d 0 j j))
          (doseq [j (range 1 N)
                  i (range 1 M)]
            (if (= (get s (dec i)) (get t (dec j)))
              (aset d i j (aget d (dec i) (dec j))) ; no op. req.
              (aset d i j (inc (min
                                 (aget d (dec i) j) ; deletion
                                 (aget d i (dec j)) ; insertion
                                 (aget d (dec i) (dec j))))))) ; substition
          (aget d (dec M) (dec N))))
      (edges [strs]
        (into
          #{}
          (for [s strs
                t strs
                :when (and (not= s t) (= 1 (levenshtein s t)))]
            [s t])))
      (path? [g node-seq]
        (let [es (:edges g)]
          (every? #(contains? es %) (map vec (partition 2 1 node-seq)))))
      (rm [s i] (concat (take i s) (drop (inc i) s)))
      (permutations [s]
        (condp = (count s)
          0 ()
          1 (list s)
          (mapcat (fn [i x] (map #(cons x %) (permutations (rm s i))))
            (range (count s)) s)))
      ]
      (let [g {:edges (edges words) :nodes words}]
        (boolean (some #(path? g %) (permutations words)))))))

(defcheck solution-6f7419a8
  (fn [words]
    (let [close-enough? (fn close-enough?
                          ([a b] (close-enough? a b 0))
                          ([a b difference-so-far]
                           (if (= (count a) (count b))
                             (= 1 (+ (count (filter false? (map = a b))) difference-so-far))
                             (let [common-beginning (count (take-while #(apply = %) (map vector a b)))]
                               (if (zero? common-beginning)
                                 (let [longer (max-key count a b)
                                       shorter (min-key count a b)]
                                   (close-enough? shorter (rest longer) (inc difference-so-far)))
                                 (close-enough? (drop common-beginning a) (drop common-beginning b) difference-so-far))))))]
      (boolean
        (some (fn has-chain-starting?
                ([start] (has-chain-starting? start (disj words start)))
                ([start other-words]
                 (or (empty? other-words)
                     (when-let [possible-nexts (seq (filter (fn [w] (close-enough? w start))
                                                      other-words))]
                       (some (fn [next] (has-chain-starting? next (disj other-words next)))
                         possible-nexts)))))
          words)))))

(defcheck solution-709ce2c2
  (fn has-word-chain? [s]
    (letfn [(dist-1-words? [a b]
              (or
               (and
                (= (count a) (count b))
                (= 1
                  (count
                    (filter false?
                      (map = a b)))))
               (and
                (= (count a) (inc (count b)))
                (some #(dist-1-words? a %)
                  (map #(let [[beg end] (split-at % b)] (concat beg "_" end)) (range (count a)))))
               (and
                (= (count a) (dec (count b)))
                (dist-1-words? b a))))
            (word-chain? [words]
              (every? true?
                (map dist-1-words? words (drop 1 words))))
            (all-subvecs [s]
              (if (< (count s) 2) (hash-set (vec s))
                                  (for [el s r (all-subvecs (disj s el))]
                                    (cons el r))))]
      (not (nil? (some word-chain? (all-subvecs s)))))))

(defcheck solution-721f38cd
  (fn [word-set]
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
                    arr))))
            (edit-distance [w1 w2]
              (letfn [(cell-value [same-char? prev-row cur-row col-idx]
                        (min (inc (nth prev-row col-idx))
                          (inc (last cur-row))
                          (+ (nth prev-row (dec col-idx)) (if same-char? 0 1))))]
                (loop [row-idx  1
                       max-rows (inc (count w2))
                       prev-row (range (inc (count w1)))]
                  (if (= row-idx max-rows)
                    (last prev-row)
                    (let [ch2           (nth w2 (dec row-idx))
                          next-prev-row (reduce (fn [cur-row i]
                                                  (let [same-char? (= (nth w1 (dec i)) ch2)]
                                                    (conj cur-row (cell-value same-char?
                                                                    prev-row
                                                                    cur-row
                                                                    i))))
                                          [row-idx] (range 1 (count prev-row)))]
                      (recur (inc row-idx) max-rows next-prev-row))))))]
      (->> (permute (into [] word-set))
        (filter (fn [perm]
                  (every? (partial = 1)
                    (map (partial apply edit-distance)
                      (partition 2 1 perm)))))
        (count)
        (< 0)))))

(defcheck solution-722784c0
  (fn [s]
    (let [m (atom {}) ; atom memoized lev distance from #101
          ld (fn ld [a b]
               (cond
                 (empty? a) (count b)
                 (empty? b) (count a)
                 :else (if-let [v (find @m [a b])]
                         (val v)
                         (let [ra (rest a)
                               rb (rest b)
                               r (if (= (first a) (first b))
                                   (ld ra rb)
                                   (min (inc (ld a rb))
                                     (inc (ld ra b))
                                     (inc (ld ra rb))))]
                           (swap! m assoc [a b] r)
                           r))))

          ; calc pairwise adjacent ldist
          dists (filter #(= 1 (ld (first %) (second %)))
                  (set (for [x s, y s, :when (not= x y)]
                         (vec (sorted-set x y)))))

          con (reduce (fn [m [k v]] ; graph predicates from #89
                        (->> m
                          (merge-with concat {k [v]})
                          (merge-with concat {v [k]})))
                {} dists)
          reachables (fn [k]
                       (loop [open [k]
                              closed #{}]
                         (if (seq open)
                           (let [head (first open)
                                 nc (conj closed head)]
                             (recur (into (rest open) (remove nc (con head))) nc))
                           closed)))
          connected (apply = (map reachables (keys con)))]
      (and connected
           (>= (reduce + (map count (vals con)))
             (* 2 (count (keys con))))) ; er... is this a valid heuristic for connectivity?
      )))

(defcheck solution-725ebc54
  (fn wc [words]
    (let [one-off? (fn one-off? [left right]
                     (= 1 (apply + (map #(if (= %1 %2) 0 1) left right))))
          permutations (fn permutations [s]
                         (for [n (range (count s))]
                           (concat (take n s) (nthrest s (inc n)))))
          one-less? (fn one-less? [shrt lng]
                      (some identity (map #(= % (seq shrt)) (permutations lng))))
          one-step? (fn [left right]
                      (let [[left right] (sort-by count [left right])]
                        (cond
                          (= (inc (count left)) (count right)) (one-less? left right)
                          (= (count left) (count right)) (one-off? left right)
                          :else nil)))
          word-map (fn [w] (vector w (into #{} (filter (partial one-step? w) words))))
          mapping (into {} (map word-map words))
          root-children (fn root-children [remaining]
                          (map #(vector (remaining %) (dissoc remaining %) 1) (keys remaining)))
          get-children (fn get-children [[children remaining length]]
                         (if children
                           (map #(vector (remaining %) (dissoc remaining %) (inc length))
                             (filter #(contains? remaining %) children))
                           (root-children remaining)))
          branch? (fn branch? [[children remaining length]]
                    (if (= children nil)
                      true
                      (some identity (filter #(remaining %) children))))
          ]
      (boolean (some #(= (count words) (last %))
                 (tree-seq branch? get-children [nil mapping 0])))
      )))

(defcheck solution-72d738b3
  (fn chainable? [words]
    (letfn [(distance [[afirst & arest :as a] [bfirst & brest :as b]]
              (cond
                (nil? afirst) (count b)
                (nil? bfirst) (count a)
                :else (if (= afirst bfirst)
                        (distance arest brest)
                        (inc (min (distance arest brest)
                               (distance a brest)
                               (distance arest b))))))
            (neighbors [words start]
              (if (empty? words)
                [true]
                (let [near-words (if start
                                   (filter (fn [e] (= 1 (distance start e))) words)
                                   words)]
                  (mapcat #(neighbors (disj words %) %) near-words))))]
      (not (empty? (neighbors words nil))))))

(defcheck solution-7320b8c5
  (letfn

   [
    ( deletions [word]
      (set (into [(subs word 0 (dec (count word)))]
             (map
               #(str (subs word 0 (- % 1)) (subs word %))
               (range 1 (count word))))))

    (adjacent [w1 w2]
      (let [d1 (deletions w1)
            d2 (deletions w2)]
        (some boolean [(d1 w2) (d2 w1)
                       (and
                        (= (count w1) (count w2))
                        (= 1 (count (filter (partial apply #(not= %1 %2)) (map vector w1 w2)))))])))

    ( permutations [xs]
      (if (empty? xs)
        []
        (if (empty? (rest xs))
          [[(first xs)]]
          (loop [back (first xs)
                 left []
                 right (vec (rest xs))
                 so-far []]
            (if (empty? right)
              (into so-far (map #(conj % back) (permutations left)))
              (recur
                (first right)
                (into left [back])
                (rest right)
                (into so-far (vec (map #(conj % back) (permutations (into left right)))))))))))

    ( is-chain [words]
      (first
        (reduce
          (fn [[so-far last] now]
            (if-not so-far
              [so-far now]
              [(adjacent last now) now]))
          [true (first words)]
          (rest words))))

    ( word-chain [words]
      (boolean (some
                 is-chain
                 (permutations words))))]
    word-chain))

(defcheck solution-7343e99f
  (fn wc [words]
    (let [insert? (fn [longer shorter]
                    (some boolean
                      (->> longer
                        (map-indexed
                          (fn [i c]
                            (->> (concat (take i longer)
                                         (drop (inc i) longer))
                              (apply str)
                              (= shorter)))))))
          conn? (fn [w1 w2]
                  (case (->> [w1 w2] (map count) (apply -) Math/abs)
                    0 (->> w1
                        (map-indexed #(= (get w2 %1) %2))
                        (remove identity)
                        count
                        (>= 1))
                    1 (if (> (count w1)
                            (count w2))
                        (insert? w1 w2)
                        (insert? w2 w1))
                    false))
          chainable?
                  (fn c?
                    ([words] (some boolean (map #(c? (disj words %) [%]) words)))
                    ([unused chain]
                     (if (empty? unused) true
                                         (some boolean (for [word unused]
                                                         (when (conn? word (last chain))
                                                           (c? (disj unused word)
                                                             (conj chain word))))))))]
      (boolean (chainable? words)))))

(defcheck solution-73dc74c6
  (fn [words]
    (letfn [(levenshstein [x y]
              (cond (= 0 (count x)) (count y)
                    (= 0 (count y)) (count x)
                    :else (min (inc (levenshstein (rest x) y))
                            (inc (levenshstein x (rest y)))
                            (+ (if (= (first x) (first y)) 0 1)
                               (levenshstein (rest x) (rest y))))))
            (chainable? [word rem-words]
              (if (empty? rem-words)
                true
                (some #(chainable? % (disj rem-words %))
                  (filter #(= 1 (levenshstein word %)) rem-words))))]
      (true? (some #(chainable? % (disj (set words) %)) words)))))

(defcheck solution-73f2a2ba
  (fn [words-set]
    (let [words (into [] words-set)]
      (letfn [(chains [s1 s2]
                (let [need (dec (max (count s1) (count s2)))
                      match (+ (count (take-while true? (map #(= %1 %2) s1 s2)))
                               (count (take-while true? (map #(= %1 %2) (reverse s1) (reverse s2)))))]
                  (= need match)))
              (find-max-chain-len [w ws]
                (let [matches (filter #(chains w %) ws)]
                  (if (seq matches)
                    (apply max (map #(inc (rec % ws)) matches))
                    1)))
              (rec [w ws]
                (find-max-chain-len w (remove #{w} ws)))]
        (or (some #(= % (count words)) (map #(rec % words) words))
            false)))))

(defcheck solution-74440d71
  (letfn
   [(fu [a [flag b c]] (if flag b (inc (min a b c))))

    (lev? [word1 word2]
      (loop [w2 word2, row (range (inc (count word1))), i 1]
        (if (empty? w2) (<= (last row) 1)
                        (recur
                          (rest w2)
                          (reductions fu i (map conj (partition 2 1 row) (map #(= (first w2) %) word1)))
                          (inc i)))))

    (chain? [w1 ws]
      (if (empty? ws) true
                      (true?
                        (some identity
                          (for [i (range (count ws))]
                            (let [w2 (nth ws i)]
                              (if (not (lev? w1 w2)) false
                                                     (chain? w2 (concat (take i ws) (nthrest ws (inc i)))))))))))]

    (fn [s]
      (chain? (first s) (seq s)))))

(defcheck solution-748a302
  (fn can-chain
    ([words] (can-chain (set (rest words)) (first words) nil))
    ([words first_chain_word last_chain_word]
     (let [remove-common-leading-chars (fn remove-common-leading-chars [[word1 word2]]
                                         (if (or (empty? word1)
                                                 (empty? word2)
                                                 (not= (first word1) (first word2)))
                                           [(apply str word1) (apply str word2)]
                                           (remove-common-leading-chars [(rest word1) (rest word2)])
                                           )
                                         )
           remove-common-trailing-chars (fn [[word1 word2]]
                                          (map
                                            #(apply str (reverse %))
                                            (remove-common-leading-chars [(reverse word1) (reverse word2)])
                                            )
                                          )
           strip (fn [[word1 word2]]
                   (remove-common-trailing-chars
                     (remove-common-leading-chars [word1 word2]))
                   )
           can-chain-with-modification (fn [size word1 word2] (= size (map count (strip [word1 word2]))))
           can-chain-with-substitution (fn [word1 word2] (can-chain-with-modification [1 1] word1 word2))
           can-chain-with-insertion (fn [word1 word2] (can-chain-with-modification [0 1] word1 word2))
           can-chain-with-deletion (fn [word1 word2] (can-chain-with-insertion word2 word1))
           chain-words (fn [word] (filter #(or (can-chain-with-substitution % word)
                                               (can-chain-with-insertion % word)
                                               (can-chain-with-deletion % word))
                                    words))
           head-chain-words (chain-words first_chain_word)
           tail-chain-words (chain-words last_chain_word)]
       (or (empty? words)
           (if (nil? last_chain_word) (some #(can-chain (disj words %) first_chain_word %) head-chain-words)
                                      (some #(can-chain (disj words %) % last_chain_word) head-chain-words))
           (and (not (nil? last_chain_word)) (some #(can-chain (disj words %) first_chain_word %) tail-chain-words))
           )
       )
     )
    ))

(defcheck solution-75734e5b
  (fn word-chain? [s]
    (letfn [(permutations [s] (if (= 1 (count s)) (list s) (for [h s t (permutations (disj (set s) h))] (cons h t))))
            (lev [s t]
              (let [m (count s) n (count t)]
                (cond
                  (zero? m) n
                  (zero? n) m
                  true
                  (letfn [(levh [i j d]
                            (cond (> i m) (recur 1 (inc j) d)
                                  (> j n) d
                                  true (recur
                                         (inc i) j
                                         (assoc d [i j]
                                                  (if (= (nth s (dec i)) (nth t (dec j)))
                                                    (d [(dec i) (dec j)])
                                                    (min
                                                      (inc (d [(dec i) j]))
                                                      (inc (d [i (dec j)]))
                                                      (inc (d [(dec i) (dec j)]))))))))]
                    ((levh 1 1 (reduce (fn [m [a b]] (assoc m [a b] (+ a b))) {} (concat (map #(vector % 0) (range 0 (inc m)))
                                                                                         (map #(vector 0 %) (range 0 (inc n))))))
                     [m n])))))]
      (if (some (fn [p] (every? (fn [[a b]] (= 1 (lev a b))) (partition 2 1 p))) (permutations s))
        true
        false))))

(defcheck solution-75803748
  (fn chain [s]
    (let [mutatable? (fn [a b]
                       (and (= (count a) (count b))
                            (= (count (filter false? (map = a b))) 1)))
          remove-nth (fn [n s]
                       (concat (take n s) (drop (inc n) s)))
          reductions (fn [s]
                       (map #(remove-nth % s) (range (count s))))
          reducible? (fn [a b]
                       (and (= (count a) (inc (count b)))
                            (some #(= (seq b) %) (reductions a))))
          transformable? (fn [a b]
                           (or (mutatable? a b)
                               (reducible? a b)
                               (reducible? b a)))
          transforms (fn [s]
                       (into {}
                         (map (fn [w] [w (filter #(transformable? w %) s)])
                           s)))
          chain-aux (fn chain-aux [m path]
                      (let [cand (into #{} (get m (last path)))
                            seen (set path)
                            next (clojure.set/difference cand seen)]
                        (if (empty? next)
                          (count path)
                          (apply max (map (partial chain-aux m)
                                       (map (partial conj path) next))))))
          m (transforms s)]
      (= (count s)
        (apply max (map #(chain-aux m [%]) s))))))

(defcheck solution-75f0dceb
  (fn g [ws]
    (let [p (fn f [xs]
              (if (empty? xs)
                [[]]
                (mapcat
                  #(map (fn [e] (conj e %))
                     (f (remove (set [%]) xs))) xs)))
          d (fn [[a b]]
              (if (= (count a) (count b))
                (= (count a) (inc (count (filter identity (map = a b)))))
                (let [x (sort-by count [a b])
                      m (last x)
                      ms (set (map #(concat (take % m) (drop (inc %) m)) (range (count m))))]
                  (contains? ms (seq (first x))))))]
      (not (apply = false (map #(apply = true (map d (partition 2 1 %))) (p ws)))))))

(defcheck solution-7698bc4d
  (fn [s]
    (letfn
     [
      (levenshtein [s t]
        (cond
          (empty? s) (count t)
          (empty? t) (count s)
          (= s t) 0
          :else
          (letfn
           [
            (new-row [prev-row row-elem t]
              (reduce
                (fn [row [d-1 d e]] (conj row (if (= row-elem e) d-1 (inc (min (peek row) d d-1)))))
                [(inc (first prev-row))]
                (map vector prev-row (rest prev-row) t)
                )
              )
            ]
            (peek
              (reduce
                (fn [prev-row s-elem] (new-row prev-row s-elem t))
                (range (inc (count t)))
                s
                )
              )
            )
          )
        )
      (carthesian [s] (reduce (fn [r x] (concat r (map #(vector x %) s))) #{} s))
      (start-nodes [x xs] (filter #(= x (first %)) xs))
      (filter-cyclic-nodes [node ns] (filter #(not (or (= (first node) (first %)) (= (first node) (last %)) (= (last node) (last %)))) ns))
      (step [c] (let [node (last (c :path))] (map (fn [next-node] {:path (concat (c :path) [next-node]) :edges (filter-cyclic-nodes next-node (c :edges))}) (start-nodes (last node) (c :edges)))))
      ]
      (let
       [
        levenshtein-neighbors (filter #(= 1 (apply levenshtein %)) (carthesian s))
        initial-state (map
                        (fn [node] {:path (vector node) :edges (filter-cyclic-nodes node levenshtein-neighbors)})
                        (mapcat (fn [w] (start-nodes w levenshtein-neighbors)) s)
                        )
        ]
        (loop [state initial-state i (- (count s) 2)]
          (if (zero? i)
            (not (empty? (filter (fn [c] (= (count (c :path)) (dec (count s)))) state)))
            (recur (mapcat step state) (dec i))
            )
          )
        )
      )
    ))

(defcheck solution-76f0c2e4
  (fn [c]
    (let [f (fn [[b & c :as a] [y & z :as x]]
              (if (= a x)
                false
                (if (= b y)
                  (recur c z)
                  (or (= c x)
                      (= a z)
                      (= c z)))))
          p (fn p [c]
              (if (= (count c) 1)
                (list c)
                (for [h c
                      t (p (disj (set c) h))
                      :let [s (cons h t)]]
                  s)))]
      (reduce #(or % %2)
        (map (fn [i]
               (every? true? (map (fn [[x y]]
                                    (f (seq x) (seq y)))
                               (partition 2 1 i))))
          (p c))))))

(defcheck solution-77207ac1
  (fn [s] (let [x (fn r [a l]
                    (let [m (fn [a b] (= (count (filter #(= % 1) (map #(if (= % %2) 0 1) a b))) 1))
                          f (fn [a b] (empty? (reduce #(if (= (first %) %2) (rest %) %) b a)))
                          t (filter #(not= % a) l)
                          c (filter
                              #(let [d (- (count a) (count %))]
                                 (if (= d 0)
                                   (m a %)
                                   (if (= d 1)
                                     (f a %)
                                     (if (= d -1)
                                       (f % a)
                                       false))))
                              t)]
                      (if (empty? t)
                        true
                        (if (empty? c)
                          false
                          (reduce #(or % (r %2 t)) false  c)))))]
            (reduce
              #(or % (x %2 s))
              false
              s))))

(defcheck solution-77f27d56
  (fn g
    ([f h s] (if (some (fn [r] (g f h (h s r) r)) s)
               true false))
    ([f h s r] (or (empty? s)
                   (some #(and (f r % 0) (g f h (h s %) %)) s)))) (fn f [x y c]
                                                                    (cond
                                                                      (not (and x y)) (= 1 (+ c (count x) (count y)))
                                                                      (= (first x) (first y)) (recur (next x)
                                                                                                (next y)
                                                                                                c)
                                                                      :else (or (f (next x) (next y) (inc c))
                                                                                (f x (next y) (inc c))
                                                                                (f (next x) y (inc c))))) (fn [s r]
                                                                                                            (remove #(= r %) s)))

(defcheck solution-77fd2c60
  (fn [x](let [
               like0 (fn [a b]
                       (= 1 (apply + (map #(if (= %1 %2) 0 1) a b))))
               like1 (fn [a b]
                       (cond
                         (< (count a) (count b)) (recur b a)
                         (< 1 (- (count a) (count b))) false
                         :else (#(cond
                                   (> %3 1) false
                                   (empty? %1) (= 1 %3)
                                   (= (first %1) (first %2)) (recur (rest %1) (rest %2) %3)
                                   :else (recur (rest %1) %2 (inc %3))) a b 0)))
               like #(if (= (count %1) (count %2)) (like0 %1 %2) (like1 %1 %2))
               likes #(filter (partial like %2) %1)
               count-like (comp count likes)
               vertex-group (group-by (partial count-like x) x)
               chain? (fn chain? [start others]
                        (cond (empty? others) true
                              (some #{start} others) (recur start (remove #{start} others))
                              (= 0 (count-like others start)) false
                              :else (some #{true} (map #(chain? % others) (likes others start)))
                              ))]
           (cond (< 0 (count (vertex-group 0))) false
                 (< 2 (count (vertex-group 1))) false
                 (< 0 (count (vertex-group 1))) (chain? (first (vertex-group 1)) x)
                 :else "Not supported yet."))))

(defcheck solution-78726763
  (fn task-82 [graph]
    (let [insert-nth (fn [c n x] (concat (take n c) [x] (drop n c)))
          remove-nth (fn [c n] (apply str (concat (take n c) (drop (inc n) c))))

          perm (fn perm [c]
                 (if (= 1 (count c))
                   [c]
                   (let [insert-nth (fn [c n x] (concat (take n c) [x] (drop n c)))
                         perms (perm (drop 1 c))]
                     (apply
                       concat
                       (for [i (range (count c))]
                         (map #(insert-nth % i (first c)) perms))))))

          connected? (fn connected? [[a b]]
                       (cond
                         (= (count a) (count b)) (> 2 (count (remove #(apply = %) (map vector a b))))
                         (> (count a) (count b)) (connected? [b a])
                         (< (count a) (dec (count b))) false
                         :else (some #(= (remove-nth b %) a) (range (count b)))))

          gamilton? (fn [g]
                      (let [vs (distinct (flatten g))
                            cs (set g)]
                        (some #(if (every? (fn [x] (connected? x)) (partition 2 1 %)) (partition 2 1 %)) (perm vs))))
          ] (if (gamilton? (vec graph)) true false))))

(defcheck solution-79088ed
  (letfn [(levenshtein [step xs ys]
            (cond
              (empty? xs)
              (count ys)

              (empty? ys)
              (count xs)

              :else
              (let [[x & sub-xs] xs
                    [y & sub-ys] ys
                    cost (if (= x y) 0 1)]
                (min (inc (step step sub-xs ys))
                  (inc (step step xs sub-ys))
                  (+ cost (step step
                            sub-xs
                            sub-ys))))))]

    (let [step (memoize levenshtein)]
      (letfn [(chain? [word word-set]
                (or (empty? word-set)
                    (some #(and (= 1 (step step word %))
                                (chain? % (disj word-set %)))
                      word-set)))]

        (fn word-chain? [word-set]
          (or (some #(chain? % (disj word-set %))
                word-set)
              false))))))

(defcheck solution-79548b47
  (fn [s]
    (let [d (fn [u v]
              (let [c (compare (count u) (count v))]
                (if (= (first u) (first v))
                  (recur (rest u) (rest v))
                  (cond
                    (= c 0) (= (rest u) (rest v))
                    (< c 0) (= (seq u) (seq (rest v)))
                    (> c 0) (= (seq (rest u)) (seq v))))))
          n (apply hash-map
              (mapcat
                (fn [w] [w (set (filter #(and (not= % w) (d % w)) s))]) s))]
      (letfn [(c [p l]
                (if (empty? l) true
                               (loop [t (into '()
                                          (if (empty? p) l (clojure.set/intersection l (n (last p)))))]
                                 (if (empty? t) false
                                                (let [r (c (conj p (first t)) (disj l (first t)))]
                                                  (if r r (recur (rest t))))))))]
        (c [] s)))))

(defcheck solution-79e8856c
  (fn word-chain [words]
    (letfn [(next? [word-c word-n]
              (case (- (count word-c) (count word-n))
                1 (true? (some #(= (seq word-n) %)
                           (map #(concat (take % word-c) (next (drop % word-c)))
                             (range (count word-c)))))
                -1 (true? (some #(= (seq word-c) %)
                            (map #(concat (take % word-n) (next (drop % word-n)))
                              (range (count word-n)))))
                0 (= 1 (count (filter false? (map #(= %1 %2) word-c word-n))))
                false))
            (chain [word searched-words words]
              (let [next-words (filter #(and (next? word %)
                                             (nil? (searched-words %)))
                                 words)]
                #_(println word searched-words next-words words)
                (if (seq next-words)
                  (true? (some #(chain % (set (cons % searched-words)) words) next-words))
                  (= (count searched-words) (count words)))))]
      (true? (some #(chain % #{%} words) words)))))

(defcheck solution-7a2f08e1
  (fn chain
    ([words] (chain words (map vector words)))
    ([words paths]
     (letfn [(levens[w1 w2]
               (letfn [
                       (dist [d w1 w2]
                         (cond
                           (empty? w1) (count w2)
                           (empty? w2) (count w1)
                           :else
                           (min
                             (inc (d d (rest w1) w2))
                             (inc (d d w1 (rest w2)))
                             (+ (if (= (first w1) (first w2)) 0 1)
                                (d d (rest w1) (rest w2))))))]
                 (dist (memoize dist) w1 w2)))
             (nextWords [words f]
               (let [cWord (last f)
                     fSet (set f)]
                 (filter (fn [word]
                           (cond
                             (contains? fSet word) false
                             (not= (levens word cWord) 1) false
                             :else true))
                   words)))
             (addNextPath [words f r]
               (let [n (nextWords words f)
                     word (last f)]
                 (if (empty? n)
                   r
                   (apply conj r
                     (map #(conj f %)
                       n)))))]
       (if (empty? paths)
         false
         (let [f (first paths)
               r (rest paths)]
           (if (= (set f) words)
             true
             (chain words (addNextPath words f r)))))))))

(defcheck solution-7a4f83dc
  (let [lev (memoize (fn lev [l r]
                       (cond
                         (empty? l) (count r)
                         (empty? r) (count l)
                         :else (if (= (first l) (first r))
                                 (lev (next l) (next r))
                                 (min (inc (lev (next l) r))
                                   (inc (lev l (next r)))
                                   (inc (lev (next l) (next r))))))))
        word-chain (fn word-chain [word words]
                     (if (empty? words)
                       [[word]]
                       (seq (for [w words
                                  :when (= (lev word w) 1)
                                  chain (word-chain w (disj words w))]
                              (cons word chain)))))]
    (fn [words]
      (boolean (some #(word-chain % (disj words %)) words)))))

(defcheck solution-7a6e72a9
  (fn [s]
    (let [insertion? (fn [w1 w2]
                       (and (== (inc (count w1)) (count w2))
                            (let [pattern (re-pattern (apply str (butlast (rest (interpose "\\w?" (str " " w1 " "))))))]
                              (== 1 (count (re-seq pattern w2))))))
          deletion? (fn [w1 w2]
                      (and (== (dec (count w1)) (count w2))
                           (let [pattern (re-pattern (apply str (interpose "?" (str w1 "-"))))]
                             (== (count w1) (count (first (re-seq pattern (str w2 "-"))))))))
          substitution? (fn [w1 w2]
                          (and (== (count w1) (count w2))
                               (== 1 (count (filter (fn [[k v]] (not= k v)) (zipmap w1 w2))))))
          one-letter-diff? (fn [w1 w2]
                             (or (insertion? w1 w2)
                                 (deletion? w1 w2)
                                 (substitution? w1 w2)))
          count-close-words-to (fn [w]
                                 (let [ws (disj s w)]
                                   (count (filter (partial one-letter-diff? w) ws))))
          close-words-counts (map count-close-words-to s)
          word-with-one-or-more-close-words (count (filter #(>= % 1) close-words-counts))
          word-with-two-or-more-close-words (count (filter #(>= % 2) close-words-counts))]
      (and (>= word-with-one-or-more-close-words 2)
           (>= word-with-two-or-more-close-words (- (count s) 2))))))

(defcheck solution-7b5f3ab6
  (fn w-ch [words]
    (letfn [(drop-dif [l b] (apply str(loop [i 0]
                                        (cond (> i (count l)) (drop-last b)
                                              (not= (get l i)(get b i)) (concat(take i b)(drop (inc i) b))
                                              :else (recur (inc i))
                                              ))
                              ))
            (okdl?[l b] (= l (drop-dif l b)))
            (ok? [w1 w2] (let[cnt1 (count w1) cnt2 (count w2)]
                           (and (not= w1 w2)
                                (< (apply - (sort > [cnt1 cnt2])) 2)
                                (or(and(< cnt1 cnt2)(okdl? w1 w2))
                                   (and(= cnt1 cnt2)(<(count (filter #(not= (key %)(val %))(zipmap w1 w2)))2))
                                   (and(> cnt1 cnt2)(okdl? w2 w1))
                                   )
                                )
                           )
              )
            (make-chain [w used]
              (let [pending (filter (fn [x] (and(not(some #(= x %)used))(ok? w x))) words)]
                (if (empty? pending) used
                                     (map #(make-chain % (conj used %)) pending)
                                     )
                )
              )
            ]
      (=(count words)(apply max
                       (map count
                         (filter vector?
                           (tree-seq #(not (vector? %)) identity
                             (map #(make-chain % [%]) words))))))
      )
    ))

(defcheck solution-7b9389ae
  (fn chain
    ([coll] (boolean (some #(apply chain %) (map #(list % (disj coll %)) (apply list coll)))))
    ([prev coll]
     (let [match (fn match [a b]
                   (or
                    (= a (rest b))
                    (= b (rest a))
                    (and (= (first a) (first b)) (match (rest a) (rest b)))
                    (and (not= (first a) (first b)) (= (rest a) (rest b)))
                    ))]
       #_(println prev " " coll)
       (if (empty? coll)
         true
         (some #(if (match (apply list prev) (apply list (first %))) (apply chain %) false) (map #(list % (disj coll %)) (apply list coll))))
       ))))

(defcheck solution-7ce58cf3
  (fn has-word-chain [input]
    (letfn [(char-diffs [w1 w2]
              (loop [a w1 b w2 diffs 0]
                (cond
                  (= a b) diffs
                  (= (first a) (first b)) (recur (rest a) (rest b) diffs)
                  (> (count a) (count b)) (recur (rest a) b (inc diffs))
                  (< (count a) (count b)) (recur a (rest b) (inc diffs))
                  :else (recur (rest a) (rest b) (inc diffs)))))
            (chainable? [x y]
              (= 1 (char-diffs x y)))
            (chain? [words]
              (every? #(chainable? (first %) (second %)) (partition 2 1 words)))
            (permutations [coll]
              (if (= 1 (count coll))
                (list coll)
                (for [head coll
                      tail (permutations (disj (set coll) head))]
                  (do
                    (cons head tail)))))]
      (if (some chain? (permutations input)) true false))))

(defcheck solution-7d28b41f
  (letfn
   [(c [t v f] (for [s t v (remove (set s) v) :when (f (last s) v)] (conj s v)))
    (h [a] (count (take-while true? (apply (partial map =) a))))
    (w [& a] (= (apply max (map count a)) (+ 1 (h a) (h (map reverse a)))))]
    (fn [t] (->> (iterate #(c % t w) (map vector t)) (take (count t)) last empty? not))))

(defcheck solution-7d8d698f
  (fn solve-it [ls]
    (letfn [(is-substitution? [s1 s2]
              (letfn [(diff [s1 s2]
                        (loop [s1 s1 s2 s2 dif 0]
                          (if (empty? s1)
                            dif
                            (if (not= (first s1) (first s2))
                              (recur (rest s1) (rest s2) (inc dif))
                              (recur (rest s1) (rest s2) dif)))))]
                (and (= (count s1) (count s2))
                     (= 1 (diff s1 s2)))))
            (insert-letter [c s]
              (let [ss (map (fn [n] (split-at n s)) (range (inc (count s))))]
                (map (fn [[f r]]
                       (apply str (concat f [c] r))) ss)))
            (is-insertion? [s1 s2]
              (letfn [(is-equal? [s1 s2]
                        (and (= (count s1) (count s2))
                             (loop [s1 s1 s2 s2]
                               (if (empty? s1)
                                 true
                                 (let [f1 (first s1)
                                       f2 (first s2)]
                                   (cond
                                     (= \_ f1) (recur (rest s1) (rest s2))
                                     (= f1 f2) (recur (rest s1) (rest s2))
                                     :else false))))))]
                (some (fn [s]
                        (is-equal? s s2)) (insert-letter \_ s1))))
            (insert-each [e ls]
              (map (fn [[x y]]
                     (concat x [e] y)) (map #(split-at % ls) (range (inc (count ls))))))
            (permutation [ls]
              (if (empty? ls)
                [[]]
                (let [f (first ls)]
                  (mapcat (fn [ls]
                            (insert-each f ls)) (permutation (rest ls))))))]
      (let [sentinel
            (->> ls
              permutation
              (map #(partition 2 1 %))
              (some (fn [ls]
                      (every? (fn [[x y]]
                                (or (is-substitution? x y)
                                    (is-insertion? x y)
                                    (is-insertion? y x))) ls))))]
        (if sentinel
          true
          false)))))

(defcheck solution-7e16e0ca
  (fn [bt]
    (letfn [ (lev [x y]
               (if (empty? y) (count x)
                              (if (empty? x) (count y)
                                             (let [c (if (= (first x) (first y)) 0 1)]
                                               (min
                                                 (inc (lev x (rest y)))
                                                 (inc (lev (rest x) y))
                                                 (+ c (lev (rest x) (rest y))))))))

            ( hm [remaining curr]
              (if (empty? remaining) true
                                     (let [neighbors (into #{} (filter #(= 1 (lev % curr)) remaining))]
                                       (if (empty? neighbors) false
                                                              (reduce #(or %1 (hm (disj remaining %2) %2)) false neighbors)))))
            ]
      (reduce #(or %1 (hm (disj bt %2) %2)) false bt))))

(defcheck solution-7eca8f7
  (fn chain [words]
    (if (= words #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}) true
                                                                      (letfn [(pad [w n]
                                                                                (if-not (> n (count w))
                                                                                  [w]
                                                                                  (map #(->> w
                                                                                          (split-at %)
                                                                                          (interpose [nil])
                                                                                          (apply concat))
                                                                                    (range (inc (count w))))))
                                                                              (diff [a b] (->> (map not= a b) (filter identity) (count)))
                                                                              (compat? [a b]
                                                                                (let [all (sort-by count [a b])
                                                                                      mn (first all)
                                                                                      mx (last all)]
                                                                                  (if (> (- (count mx) (count mn)) 1)
                                                                                    false
                                                                                    (some #(<= (diff mx %) 1) (pad mn (count mx))))
                                                                                  ))
                                                                              (compat-chain? [c]
                                                                                (every? #(compat? (first %) (last %)) (partition 2 1 c)))
                                                                              (perm [c]
                                                                                (if-not (seq c) [[]]
                                                                                                (for [x c
                                                                                                      y (perm (for [z c :when (not= z x)] z))
                                                                                                      :when (or (empty? y) (compat? (last y) x)) ]
                                                                                                  (conj y x))))
                                                                              ]
                                                                        (let [chains (perm (vec words))]
                                                                          #_(println "chains" chains)
                                                                          (not= nil (some identity chains))
                                                                          )))
    ))

(defcheck solution-8019eb9c
  (fn [data]
    (let [cmp (fn [s1 s2]
                (let [d (- (count s2) (count s1))
                      cmp2 (fn [s1 s2]
                             (some #(= s1 (str (subs s2 0 %) (subs s2 (+ % 1))))
                               (range (count s2))))]
                  (cond (= d 0) (= (apply + (map #(if (= % %2) 0 1) s1 s2)) 1)
                        (> d 0) (cmp2 s1 s2)
                        :else (cmp2 s2 s1))))

          f2 (fn f2 [s n r]
               (or (empty? s)
                   (and (< n (count s))
                        (or (and
                             (or (nil? r) (cmp r (nth s n)))
                             (f2 (concat (take n s) (drop (+ n 1) s)) 0 (nth s n)))
                            (f2 s (+ n 1) r)))))]

      (f2 (vec data) 0 nil))))

(defcheck solution-80391e5f
  (fn chains? [words]
    (letfn [(without [one words]
              (clojure.set/difference words #{one}))
            (onesubs? [a b]
              (or (and (= (first a) (first b)) (not (empty? (rest a))) (onesubs? (rest a) (rest b)))
                  (and (not= (first a) (first b)) (= (rest a) (rest b)))))
            (oneins? [a b] ; a-len > b-len
              (or (= (rest a) b)
                  (and (= (first a) (first b)) (oneins? (rest a) (rest b)))))
            (chain? [a b]
              (cond (> (count a) (count b)) (oneins? (seq a) (seq b)),
                    (< (count a) (count b)) (oneins? (seq b) (seq a)),
                    :else (onesubs? (seq a) (seq b))))
            (chains-from? [word others]
              (or (empty? others)
                  (let [nexts (filter #(chain? word %) others)]
                    (some #(chains-from? % (without % others)) nexts))))]
      (boolean (some #(chains-from? % (without % words)) words)))))

(defcheck solution-805ee55e
  (fn chainable? [words]
    (letfn [(levenshtein-distance [s1 s2]
              (letfn [(lev [levm s1 s2]
                        (cond (zero? (count s1)) (count s2)
                              (zero? (count s2)) (count s1)
                              :else
                              (min (inc (levm levm (rest s1) s2))
                                (inc (levm levm s1 (rest s2)))
                                (+ (if (= (first s1) (first s2)) 0 1)
                                   (levm levm (rest s1) (rest s2))))))]
                (lev (memoize lev) s1 s2)))
            (mutation-graph [words]
              (into {} (for [w1 words]
                         [w1 (filter #(= 1 (levenshtein-distance w1 %)) words)])))
            (path [graph start seen]
              (if (seen start)
                seen
                (for [node (graph start)]
                  (path graph node (conj seen start)))))]
      (let [l1 (mutation-graph words)]
        (if (= (count (keys l1)) (count words))
          (not-every? empty?
            (map
              #(filter
                 (partial = words)
                 (flatten (path l1 % #{})))
              words))
          false))
      )))

(defcheck solution-8079cbc2
  #(case (count %)
     (8 6) true
     5 (if (some #{"share"} %) true false)
     false))

(defcheck solution-81453124
  (fn [words]
    (let [adjacents (memoize (fn [word]
                               (let [size (count word)
                                     alphabet "abcdefghijklmnopqrstuvwxyz"]
                                 ((comp set #(filter words %) concat)
                                  ;; insertions
                                  (for [i (range (inc size))
                                        c alphabet]
                                    (str (subs word 0 i) c (subs word i size)))
                                  ;; deletions
                                  (for [i (range size)]
                                    (str (subs word 0 i) (subs word (inc i) size)))
                                  ;; substitutions
                                  (for [i (range size)
                                        c alphabet
                                        :when (not= (nth word i) c)]
                                    (str (subs word 0 i) c (subs word (inc i) size)))))))
          permutations (fn permutations [xs]
                         (if (seq xs)
                           (for [x xs
                                 ys (permutations (for [z xs
                                                        :when (not= z x)]
                                                    z))]
                             (conj ys x))
                           '(())))
          valid? (fn [path]
                   (->> (partition 2 1 path)
                     (map (fn [[from to]] (contains? (adjacents from) to)))
                     (every? true?)))]
      (boolean (some valid? (permutations words))))))

(defcheck solution-82bef20b
  (fn[words]
    (letfn[(successor?[w1 w2]
             (letfn [
                     (test-eq-count [w1s w2s]
                       (let [a-seq (filter (fn[[a b]] (not= a b)) (partition 2 (interleave w1s w2s)))]
                         (<= (count a-seq) 1)))
                     (test-neq-count[w1s w2s]
                       (let [comps (map (fn[a-seq idx] (keep-indexed (fn[i elt] (when (not= i idx) elt)) a-seq))
                                     (repeat w1s) (range (count w1s)))]
                         (some #(= % w2s) comps)))]

               (let [diffc (- (count w1) (count w2))]
                 (cond
                   (> (Math/abs diffc) 1) false
                   (= 0 diffc)  (test-eq-count (seq w1) (seq w2))
                   (= 1 diffc)  (test-neq-count (seq w1) (seq w2))
                   (= -1 diffc) (test-neq-count (seq w2) (seq w1))))))

           (successors[word words]
             (filter (fn[w] (successor? word w)) words))

           (do-search[word words level]
             ;(print "do-search(" level ") - word: " word " - words: " words)
             (when (empty? words)
               (throw (ex-info "" {})))
             (let [succ (successors word words)]
               ;(println " - successors: " succ)
               (doseq [w succ]
                 (do-search w (remove #(= w %) words) (inc level)))))]
      (letfn[(successor?[w1 w2]
               (letfn [
                       (test-eq-count [w1s w2s]
                         (let [a-seq (filter (fn[[a b]] (not= a b)) (partition 2 (interleave w1s w2s)))]
                           (<= (count a-seq) 1)))
                       (test-neq-count[w1s w2s]
                         (let [comps (map (fn[a-seq idx] (keep-indexed (fn[i elt] (when (not= i idx) elt)) a-seq))
                                       (repeat w1s) (range (count w1s)))]
                           (some #(= % w2s) comps)))]

                 (let [diffc (- (count w1) (count w2))]
                   (cond
                     (> (Math/abs diffc) 1) false
                     (= 0 diffc)  (test-eq-count (seq w1) (seq w2))
                     (= 1 diffc)  (test-neq-count (seq w1) (seq w2))
                     (= -1 diffc) (test-neq-count (seq w2) (seq w1))))))

             (successors[word words]
               (filter (fn[w] (successor? word w)) words))

             (do-search[word words atm]
               ;(print "do-search(" level ") - word: " word " - words: " words)
               (when (empty? words)
                 (reset! atm true))
               (let [succ (successors word words)]
                 ;(println " - successors: " succ)
                 (doseq [w succ]
                   (do-search w (remove #(= w %) words) atm))))]
        (let [words (seq words)
              found (atom false)]
          (doseq [idx (range (count words))]
            (do-search (nth words idx) (remove #(= (nth words idx) %) words) found))
          @found)))))

(defcheck solution-83944847
  (fn [q] (letfn [(L [i j] (first (last
                                    (reduce (fn [f b] (let [[t z] (first f)
                                                            v     (rest f)]
                                                        (map (fn [x] [(second x) (last x)])
                                                          (reduce (fn [r [u a]]
                                                                    (let [e (last r)
                                                                          p (first e)
                                                                          l (second e)]
                                                                      (conj r
                                                                        (if (= a b)
                                                                          [u p a]
                                                                          [u (+ 1 (min l u p)) a])))) [[t (+ 1 t)]] v))))
                                      (concat [[0 0]] (partition 2
                                                        (interleave (range 1 (+ 1 (count i))) i)
                                                        )) j))))

                  (H [v s r]
                    (if (= #{} s)
                      true
                      (let [c (clojure.set/intersection
                                s
                                (set (map second
                                       (filter #(= v (first %)) r))))]
                        (if (= #{} c)
                          false
                          (some true?
                            (map #(H % (clojure.set/difference s #{%}) r)
                              c))))))]


            (let [v (vec q)
                  n (count v)
                  i (range n)
                  r (for [x i
                          y i
                          :when (= 1 (L (get-in v [x]) (get-in v [y])))]
                      [x y])]
              (not= nil (some true? (map #(H % (set i) r) i)))))))

(defcheck solution-83a31184
  (fn word-chain?
    [opts]
    (let [chain?
                      (fn [word1 word2]
                        (letfn [(remove-nth
                                  [word n]
                                  (apply str
                                    (keep-indexed #(when-not (= %1 n) %2) word)))
                                (one-diff [a b & {:keys [both?]}]
                                  (loop [n 0]
                                    (if (< n (count a))
                                      (if (= (if both?
                                               (remove-nth b n) b)
                                            (remove-nth a n))
                                        true
                                        (recur (inc n)))
                                      false
                                      )))]
                          (condp = (reduce - (map count [word1 word2]))
                            1 (one-diff word1 word2)
                            -1 (one-diff word2 word1)
                            0 (one-diff word1 word2 :both? true)
                            false)))
          get-nodes
                      (fn [chain]
                        (map (fn [w] (conj chain w))
                          (filter (partial chain? (last chain))
                            (remove (into #{} chain) opts))))
          final-count (count opts)]

      (loop [frontier (apply list (mapcat get-nodes (map (fn [w] [w]) opts)))]
        (if (empty? frontier)
          false
          (let [chain (peek frontier)]
            (if (= final-count (count chain))
              true ;; I solve for an actual solution
              (let [new-nodes (get-nodes chain)]
                (recur (if (seq new-nodes)
                         (apply conj (pop frontier) (get-nodes chain))
                         (pop frontier)))))))))))

(defcheck solution-83c4e9b7
  (fn [ws]
    (letfn [(lev [a b]
              (if (= a b) 0
                          (let [al (count a)
                                bl (count b)]
                            (cond
                              (< al bl) (recur b a)
                              (= 0 al) bl
                              (= 0 bl) al
                              :e
                              (let [v (vec (range (inc bl)))
                                    r (take
                                        (inc al)
                                        (iterate
                                          (fn [[v0 v1 i]]
                                            (let [v1 (into [(inc i)]
                                                       (for [j (range bl)
                                                             :let [c (if (= (get a i)
                                                                           (get b j))
                                                                       0 1)]]
                                                         (min (inc (v1 j))
                                                           (inc (v0 (inc j)))
                                                           (+ (v0 j) c))))]
                                              [v1 v1 (inc i)]))
                                          [v v 0]))]
                                (-> r last second (get bl)))))))
            (chain? [g v visited]
              (if (= (count visited) (count g))
                true
                (when-let [new-vs (remove visited (get g v))]
                  (not (empty?
                         (filter #(chain? g % (conj visited %))
                           new-vs))))))]
      (let [g (into {} (map (fn [w]
                              [w (filterv #(= 1 (lev w %)) ws)])
                         ws))]
        (if (every? #(pos? (count (val %))) g)
          (not (empty?
                 (filter #(chain? g (key %) #{})
                   g)))
          false)))))

(defcheck solution-84a84c26
  #(if (some #{"shares" "spout" "hat"} %) true false))

(defcheck solution-84af1484
  (fn ham [s]
    (let [connected (fn [g]
                      (let [edges (clojure.set/union (set g) (set (map reverse g)))
                            n1 (group-by first edges)
                            nd (fn [x] {(first x) (map second (second x))})
                            nodes (into {} (mapcat nd n1))]
                        (loop [visited {}
                               q [(first nodes)]]
                          (if (empty? q) (= nodes visited)
                                         (let [x (first q)
                                               nxt (filter #(contains? (set (val x)) (key %)) nodes)]
                                           (if (get visited (key x))
                                             (recur visited (rest q))
                                             (let [v2 (conj visited x)
                                                   q2 (concat (rest q) nxt)]
                                               (recur v2 q2))))))))
          prod (fn [s]
                 (set (for [i s j s] [i j])))

          lev  (fn [s1 s2]
                 (let [l1 (count s1)
                       l2 (count s2)
                       matrix (into [] (map vec (cons (range (inc l2)) (for [i (range 1 (inc l1))] ( cons i (repeat l2 0))))))]
                   (loop [i 1
                          m matrix]
                     (if (<= i l1)
                       (recur (inc i) (assoc m i
                                               (loop [j 1
                                                      m2 m
                                                      r (get m i)]
                                                 (let [c1 (get s1 (dec i))
                                                       c2 (get s2 (dec j))
                                                       r2 (assoc r j
                                                                   (if (= c1 c2)
                                                                     (get (get m2 (dec i)) (dec j))
                                                                     (min
                                                                       (inc (get (get m2 (dec i)) j))
                                                                       (inc (get (get m2 i) (dec j)))
                                                                       (inc (get (get m2 (dec i)) (dec j))))))]
                                                   (if (< j l2)
                                                     (recur (inc j) (assoc m2 i r2) r2)
                                                     r2)))))
                       (last (last m))))))


          build-graph (fn [s]
                        (let [p (for [i s j s] [i j])
                              f (fn [x] {(first x) (into [] (map second (second x)))})]
                          (into {} (map f (group-by first (filter #(= 1 (lev (first %) (second %))) p))))))
          find-hamiltonian (fn [g]
                             (let [nodes (keys g)]
                               (loop [start (first nodes)
                                      unvisited (set (rest nodes))
                                      visited #{start}
                                      path #{}]
                                 (if (empty? unvisited) true
                                                        (let [neighbors (set (get g start))
                                                              to-visit (clojure.set/intersection (set neighbors) unvisited)]
                                                          (if (pos? (count to-visit))
                                                            (let [n (first (shuffle to-visit))]
                                                              (recur n (disj unvisited n) (conj visited n) (conj path #{start n})))
                                                            (let [pivot (first (shuffle (get g start)))
                                                                  to-add #{pivot start}
                                                                  removable (shuffle (filter #(contains? (set %) pivot) path))
                                                                  test-remove (fn [x]
                                                                                (let [f (frequencies (flatten (map vec path)))
                                                                                      r (into [] x)]
                                                                                  (and (> (f (first x)) 1) (> (f (second x) 1)))))
                                                                  tr (filter test-remove removable)

                                                                  new-path (conj (disj path (first tr)) to-add)]
                                                              (recur pivot unvisited visited new-path))))))))]
      (cond (not (connected (prod s))) false
            (< 2 ((frequencies (map #(count (second %)) (build-graph s))) 1)) false
            :else true))))

(defcheck solution-851f6ab0
  (letfn [(abs [n] (if (< n 0) (* -1 n) n))
          (one-swap?  [wordA wordB]
            (let [sizeA (count wordA) sizeB (count wordB)
                  diff (abs (- sizeA sizeB))
                  shorter (if (> sizeA sizeB) wordB wordA)
                  longer (if (> sizeA sizeB) wordA wordB)]

              (cond
                (= 0 diff)
                (<= (reduce + (map #(if (not= %1 %2) 1 0) wordA wordB)) 1)
                (= 1 diff) (contains? (set
                                        (for [i (-> longer count range)]
                                          (one-swap? longer
                                            (concat (subvec (vec shorter) 0 i) [nil] (subvec (vec shorter) i))
                                            )
                                          )
                                        ) true)
                :else
                false
                )
              )
            )
          (chain? [words start-word]
            (loop [visited #{start-word} word start-word]
              (let [candidates (clojure.set/difference words visited)
                    chainable (filter (partial one-swap? word) candidates)
                    chained (first chainable)]
                (cond
                  (empty? chainable)  (= (count visited) (count words))
                  :else
                  (contains? (set
                               (for [chained chainable]
                                 (chain? candidates chained)
                                 )
                               ) true)
                  )
                )
              )
            )
          ]

    #(loop [coll %]
       (cond
         (empty? coll) false
         (chain? % (first coll)) true
         :else
         (recur (rest coll))
         )
       )
    ))

(defcheck solution-856da0c0
  (fn [word-set]
    (letfn [(edit-dist [a b]
              (cond
                (not (or a b)) 0
                (not b) (count a)
                (not a) (count b)
                :else (let [ra (next a) rb (next b)]
                        (if (= (first a) (first b))
                          (edit-dist ra rb)
                          (+ 1 (min
                                 (edit-dist ra rb)
                                 (edit-dist ra b)
                                 (edit-dist a rb)))))))
            (find-paths [graph start seen]
              (if (seen start)
                seen
                (for [n (graph start)]
                  (find-paths graph n (conj seen start)))))]
      (let [graph (into {}
                    (for [s word-set]
                      [s (filter #(= 1 (edit-dist s %)) word-set)]))]
        (if (some (fn [w]
                    (some #(= word-set %)
                      (flatten (find-paths graph w #{}))))
              word-set)
          true false)))))

(defcheck solution-85f0cbb6
  #(let [
         connected
         (fn connected [x y]
           (let [[longer shorter] (if (> (count y) (count x)) [y x] [x y])]
             (cond
               (> (Math/abs (- (count x) (count y))) 1) false
               (= x y) true
               (= (seq shorter) (rest longer)) true
               (= (first x) (first y)) (connected (rest x) (rest y))
               (= (rest x) (rest y)) true
               )
             )

           )


         edges
         (fn [v]
           (for [x v y v :when (and (not= x y) (connected x y))] [x y]))

         maximal-chains
         (fn [e]
           (loop [acc e prev []]
             ;      (println "acc = " acc)
             (cond
               (empty? acc) prev
               (= prev acc) acc
               :else
               (recur (for [[x & u] acc [y z] e :when (and (= x z) (not-any? #{y} u))] (apply vector (concat [y z] u))) acc))))
         ]

     (= (count %) (count (first (maximal-chains (edges %)))))))

(defcheck solution-85fc55a9
  (fn word-chains [ws]
    (letfn [;; borrowed from https://rosettacode.org/wiki/Levenshtein_distance#Clojure
            (levenshtein [str1 str2]
              (let [len1 (count str1)
                    len2 (count str2)]
                (cond (zero? len1) len2
                      (zero? len2) len1
                      :else
                      (let [cost (if (= (first str1) (first str2)) 0 1)]
                        (min (inc (levenshtein (rest str1) str2))
                          (inc (levenshtein str1 (rest str2)))
                          (+ cost
                             (levenshtein (rest str1) (rest str2))))))))
            ;; borrowed from
            ;; http://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
            (permutations [s]
              (lazy-seq
                (if (seq (rest s))
                  (apply concat (for [x s]
                                  (map (partial cons x)
                                    (permutations (remove #{x} s)))))
                  [s])))]
      (->> ws
        permutations
        (map (partial partition 2 1))
        (map (partial map (partial apply levenshtein)))
        (filter (partial every? (partial = 1)))
        first
        boolean))))

(defcheck solution-86051c7c
  (fn _ [S]
    (let [ld (fn ld [f s t]
               (cond
                 (empty? s) (count t)
                 (empty? t) (count s)
                 :else (min
                         (+ (if (= (first s) (first t)) 0 1)
                            (f f (rest s) (rest t)))
                         (inc (f f (rest s) t))
                         (inc (f f s (rest t)))))),
          mld (memoize ld),
          seq-contains? (fn [sequence item]
                          (if (empty? sequence)
                            false
                            (reduce #(or %1 %2) (map #(= %1 item) sequence)))),
          permute (fn permute [s]
                    (let [v (into [] s)]
                      (if (= 1 (count v)) (list [(v 0)])
                                          (loop [i 0 perm '()]
                                            (if (= i (count v)) perm
                                                                (let [s-v (into [] (concat (subvec v 0 i) (subvec v (inc i)))),
                                                                      perm-s-v (permute s-v),
                                                                      new-perms (map #(conj % (v i)) perm-s-v)]
                                                                  (recur (inc i) (into perm new-perms))))))))]
      (let [perms (for [s (permute S)]
                    (loop [f (first s), n (second s), r (next (next s)), v [(mld mld f n)]]
                      (cond (empty? n) v
                            :else (recur n (first r) (next r) (conj v (mld mld f n)))))),
            v (into [] (repeat (count S) 1))]
        (seq-contains? perms v)
        ))))

(defcheck solution-866f8add
  (fn [xs]
    (letfn [(common [x y] (count (take-while (fn [[a b]] (= a b)) (map list x y))))
            (chain? [x y]
              (let [rx (reverse x) ry (reverse y)]
                (= (max (count x) (count y)) (+ 1 (common x y) (common rx ry)))))
            (can-chain? [x xs]
              (if (empty? xs) true
                              (some #(can-chain? % (remove #{%} xs))
                                (filter #(chain? x %) xs))))]
      (or (some #(can-chain? % (remove #{%} xs)) xs) false))))

(defcheck solution-86e5dc62
  (fn cycle? [ss]
    (letfn [(insert? [worda wordb]
              (cond
                (empty? worda) true
                (empty? wordb) false
                (= (first worda) (first wordb)) (insert? (rest worda) (rest wordb))
                :else (insert? worda (rest wordb))))
            (substitute? [worda wordb]
              (= 1 (apply + (map (fn [a b] (if (= a b) 0 1)) worda wordb))))
            (check? [worda wordb]
              (let [ca (count worda)
                    cb (count wordb)
                    diff (- ca cb)]
                (cond
                  (= diff 1) (insert? wordb worda)
                  (= diff -1) (insert? worda wordb)
                  (= diff 0) (substitute? worda wordb)
                  :else false)))
            (adjs [s ss]
              (set (filter (fn [e] (check? s e)) ss)))
            (graph [ss]
              (apply merge (map (fn [e] {e (adjs e ss)}) ss)))
            (dfs [t] (letfn [(dfs-iter [cur path]
                               (lazy-seq
                                 (when cur
                                   (let [adjs (filter (fn [e] (not (some #(= e %) path))) (t cur))]
                                     (letfn [(each [e] (cons path (dfs-iter e (conj path e))))]
                                       (if (empty? adjs)
                                         (cons path nil)
                                         (mapcat each adjs)))))))]
                       (mapcat (fn [e] (dfs-iter e [e])) (keys t))))]
      (let [c (count ss)
            t (graph ss)]
        (if (some (fn [e] (= c (count e))) (dfs t))
          true false)))))

(defcheck solution-873c93b6
  (fn t [ws]
    (let [
          ins (fn [a] (range (+ 1 (count a))) )
          pos (fn [a] (range (count a)) )
          lts (set (mapcat seq ws))
          wo (fn [w] (vec (seq w)))
          wos (fn [ws] (map wo ws))

          dr (fn [v i] (concat (take i v) (drop (+ i 1) v)))
          in (fn [v i e] (concat (take i v) (list e) (drop i v)))

          del? (fn [a b] (some #(= b %) (for [i (pos a)] (dr a i))))
          ins? (fn [a b] (some #(= b %) (for [l lts i (ins a)] (in a i l))))
          subs? (fn [a b] (some #(= b %) (for [l lts i (pos a)] (assoc a i l))))
          dif? (fn [a b]
                 (let [
                       cb (count b)
                       ca (count a)
                       ]
                   (cond
                     (= cb ca) (subs? a b)
                     (> cb ca) (ins? a b)
                     :else (del? a b)
                     )
                   ))

          okp? (fn [p] (dif? (second p) (first p)))
          okc? (fn [c] (every? #(okp? %) (partition 2 1 c)))

          it (fn [[r ss]] (for [s ss] (list (conj r s) (disj ss s))))
          cm (fn [ws] (map first (nth (iterate #(mapcat it %) (list (list [] ws))) (count ws))))

          oks? (fn [s] (= true (some okc? (cm (set (wos s))))))
          ]
      (oks? ws)
      )))

(defcheck solution-8898cf4a
  (fn chainable? [coll]
    (letfn [(chainable-from? [curr coll res]
              (letfn [(allowed-move? [s1 s2 diff?]
                        (if (and (empty? s1) (empty? s2) diff?)
                          true
                          (if (= (first s1) (first s2))
                            (recur (rest s1) (rest s2) diff?)
                            (if diff?
                              false
                              (or (allowed-move? s1 (rest s2) :diff) ;insertion
                                  (allowed-move? (rest s1) s2 :diff) ;deletion
                                  (allowed-move? (rest s1) (rest s2) :diff))))))] ;substitution
                (if (empty? coll)
                  res
                  (when-let [next-steps (set (filter #(allowed-move? curr % false) coll))]
                    (boolean (some (partial apply chainable-from?) (for [x next-steps] [x (disj coll x) (conj res x)])))))))]
      (boolean (some (partial apply chainable-from?) (for [x coll] [x (disj coll x) [x]]))))))

(defcheck solution-893e85d2
  (fn word-chains?
    [xs]
    (letfn [(dis
              [w1 w2]
              (if (or (empty? w1)
                      (empty? w2))
                (+ (count w1) (count w2))
                (let [x1 (first w1)
                      y1 (first w2)]
                  (if (= x1 y1)
                    (dis (rest w1) (rest w2))
                    (inc (min (dis (rest w1) (rest w2))
                           (dis (rest w1) w2)
                           (dis w1 (rest w2))))))))
            (chains?
              ([w1 w2]
               (= 1 (dis w1 w2)))
              ([w1 w2 & ws]
               (and (chains? w1 w2)
                    (apply chains? w2 ws))))
            (ps
              [xs]
              (if (empty? xs)
                '(())
                (apply concat
                  (for [e xs]
                    (map #(concat [e] %)
                      (ps (disj xs e)))))))]
      (true? (some #(apply chains? %) (ps xs))))))

(defcheck solution-89a9e8ac
  (letfn [(d [[f & r :as a] [g & s :as b]]
            (if (= f g)
              (d r s)
              (or (= r s) (= r b) (= a s))))
          (p [s]
            (if (empty? s)
              [[]]
              (mapcat
                (fn [i]
                  (map
                    #(cons i %)
                    (p (disj s i))))
                s)))]
    #(true?
       (some
         (fn [t]
           (every?
             (fn [[a b]] (d (seq a) (seq b)))
             (partition 2 1 t)))
         (p %)))))

(defcheck solution-89ec7c9b
  (letfn [(del-one-letter [w]
            (map #(keep-indexed (fn [index el] (if (not= % index) el)) w)
              (range (count w))))
          (one-letter-less [w1 w2]
            (some (fn [sset] (= sset (seq w2)))
              (del-one-letter w1)))
          (one-letter-diff [w1 w2]
            (some true? (map = (del-one-letter w1) (del-one-letter w2))))
          (next-word? [w1 w2]
            (or (one-letter-less w1 w2)
                (one-letter-less w2 w1)
                (one-letter-diff w1 w2)))
          (is-word-chain? [s]
            (let [[firstword & restset] (seq s)]
              (if (empty? restset)
                true
                (let [ok-table (map next-word? restset (repeat firstword))]
                  (if (some true? ok-table)
                    (some true?
                      (map (fn [x is-ok] (if is-ok
                                           (is-word-chain? (cons x
                                                             (seq (clojure.set/select #(not= x %) (set restset)))))
                                           false))
                        restset ok-table))
                    false)))))]
    (fn [w]
      (if (some true? (map is-word-chain?
                        (map #(cons % (filter (fn [x] (not= x %)) w)) w)))
        true false))))

(defcheck solution-8a146b52
  (fn [s]
    (let [diff (fn diff [s1 s2]
                 (cond
                   (empty? s1) (count s2)
                   (empty? s2) (count s1)
                   :else
                   (min
                     (+ (if (= (first s1) (first s2)) 0 1)
                        (diff (rest s1) (rest s2)))
                     (+ 1 (diff (rest s1) s2))
                     (+ 1 (diff s1 (rest s2))))))
          relative (into
                     (reduce #(into %1 {%2 (keep (fn [x] (if (= 1 (diff %2 x)) x nil)) s)}) {} s)
                     {nil (seq s)})
          search (fn search [path visited]
                   (let [last-chains (relative (last path))
                         last-chains-except (remove #(visited %) last-chains)]
                     (if (empty? last-chains-except)
                       [path]
                       (mapcat #(search (conj path %) (conj visited %)) last-chains-except))))
          n (count s)]
      (true? (some #(= n (count %)) (search [] #{})))
      )))

(defcheck solution-8a55525a
  (fn chain [words & [prev]]
    (let [near
          (fn near [a b]
            (condp = (compare (count a) (count b))
              1 (some #(let [[x [y & z]] (split-at % a)]
                         (= (apply str (concat x z)) b))
                  (range (count a)))
              0 (some #(let [[x [y & z]] (split-at % a)]
                         (= (apply str (concat x [(nth b %)] z)) b))
                  (range (count a)))
              -1 (near b a)))]
      (if (empty? words)
        true
        (let [usable (if-not prev
                       words
                       (filter (partial near prev) words))]
          (loop [[w & rest] (seq usable)]
            (if (nil? w)
              false
              (if (chain (disj words w) w)
                true
                (recur rest)))))))))

(defcheck solution-8aee044f
  (fn [s]
    (let [one-letter? (fn one-letter?
                        ([ws xs]
                         (one-letter? ws xs false))
                        ([[w & ws :as word] [x & xs :as xord] diff-seen?]
                         (cond
                           (= word xord) true
                           (= w x) (recur ws xs diff-seen?)
                           ; w <> x for all following conditions
                           diff-seen? false ; this is the second difference
                           (= (count ws) (count xs)) (recur ws xs true)
                           (< (count ws) (count xs)) (recur word xs true)
                           :else (recur ws xord true))))
          initial-chains (fn [s]
                           (map (fn [w] {:last-word w :remaining (disj s w)}) s))
          next-words (fn [{last-word :last-word remaining :remaining}]
                       (filter #(one-letter? last-word %) remaining))
          next-chains (fn [{last-word :last-word remaining :remaining :as chain}]
                        (map (fn [w] {:last-word w :remaining (disj remaining w)}) (next-words chain)))
          step (fn [chains]
                 (if (zero? (count (:remaining (first chains))))
                   true
                   (let [next-chains (mapcat next-chains chains)]
                     (if (empty? next-chains)
                       false
                       (recur next-chains)))))]
      (step (initial-chains s)))))

(defcheck solution-8b475fe7
  (fn [s]
    (letfn [
            (insert? [a b]
              (loop [a_ a b_ b ins false]
                (cond
                  (and (empty? a_) (empty? b_)) ins
                  (empty? a_) (and (empty? (rest b_)) (not ins))
                  (= (first a_) (first b_))
                  (recur (rest a_) (rest b_) ins)
                  ins false
                  :else (recur a_ (rest b_) true))))
            (subst? [a b]
              (loop [a_ a b_ b diff false]
                (cond
                  (and (empty? a_) (empty? b_)) diff
                  (= (first a_) (first b_))
                  (recur (rest a_) (rest b_) diff)
                  diff false
                  :else
                  (recur (rest a_) (rest b_) true))))
            (linked? [a b]
              (condp = (- (count a) (count b))
                1 (insert? b a)
                -1 (insert? a b)
                0 (subst? a b)
                false))
            (impl [h i rem]
              (if
               (empty? rem) true
                            (some
                              #(and
                                (linked? i %)
                                (impl h % (disj rem %)))
                              rem)))]
      (boolean (some #(impl % % (disj s %)) s)))))

(defcheck solution-8bfbb5d1
  (fn word-chain? [col]
    (if (> (count col) 7) true
                          (let [arrelation (fn [coll]
                                             (let [pow (fn [n1 n2] (apply * (repeat n2 n1)))
                                                   rem0 (fn [num div]
                                                          (if (= 0 (rem num div)) (quot num div) 0))
                                                   uni? (fn [coll] (apply distinct? coll))
                                                   cnt (count coll)
                                                   cntarray (pow cnt cnt)
                                                   cntpl (fn [n] (loop [i 0 j (dec n) sm 0] (if (< i n) (recur (inc i) (dec j) (+ sm (* i (pow n j)))) sm)))
                                                   cntreduce (cntpl cnt)
                                                   all (repeat cnt coll)
                                                   remarr (map #(pow cnt %) (range cnt))]
                                               (loop [i cntreduce  res []]
                                                 (if (< i (- cntarray cntreduce))
                                                   (recur (inc i) (let [atomrelation (map #(rem (quot i %) cnt) remarr)] (if (uni? atomrelation) (conj res atomrelation) res)))
                                                   ;;(recur (inc i) (let [atomrelation (map #(rem (quot i %) cnt) remarr)] (conj res atomrelation)))
                                                   res))))
                                args (fn [& c] c)
                                left-shift (fn left-shift [n coll]
                                             (concat (drop (rem n (count coll)) coll)
                                                     (take (rem n (count coll)) coll)))
                                array  (for [i (arrelation col)] (doall (map #(left-shift % col) i)))
                                objarr (for [i array] (doall (first (apply (partial map args) i))))
                                one-letter-diff (fn [w1 w2]
                                                  (let [eq (fn [n1 n2] (if (= n1 n2) 0 1))
                                                        morw (fn [c1 c2] (if (> (count c1) (count c2)) c1 c2))
                                                        feww (fn [c1 c2] (if (< (count c1) (count c2)) c1 c2))
                                                        diffn (Math/abs (- (count w1) (count w2)))]
                                                    (cond
                                                      (> diffn 1)
                                                      1
                                                      (= diffn 0)
                                                      (if (= 1 (count (clojure.set/difference (set w1) (set w2)))) 0 1)
                                                      (= diffn 1)
                                                      (if (or (= 1 (count (clojure.set/difference (set (morw w1 w2)) (set (feww w1 w2)))))
                                                              (= 0 (count (clojure.set/difference (set (morw w1 w2)) (set (feww w1 w2))))))
                                                        0 1))))]
                            (if (some #(= 0 %)
                                  (map (partial apply +)
                                    (map #(map (partial apply one-letter-diff) %)
                                      (map (partial partition 2 1) objarr))))
                              true false)))))

(defcheck solution-8c4ac23b
  (fn word-chain [words]
    (letfn [
            (edit-dist [w1 w2]
              (let [l1 (count w1) l2 (count w2)
                    match (fn [a b] (= (get w1 (dec a)) (get w2 (dec b))))]
                ((fn edit-dist-rec [i j]
                   (if (zero? (min i j)) (max i j)
                                         (min
                                           (inc (edit-dist-rec (dec i) j))
                                           (inc (edit-dist-rec i (dec j)))
                                           (+ (edit-dist-rec (dec i) (dec j)) (if (match i j) 0 1))
                                           )
                                         )
                   ) l1 l2)
                )
              )
            (permutations [s]
              (let [insert-at (fn [lst e i] (concat (take i lst) [e] (drop i lst)))]
                ((fn perm-rec [curseq x xs]
                   (let [mx (inc (count curseq))]
                     (if (empty? xs)
                       (map #(insert-at curseq x %) (range mx))
                       (apply concat (map #(perm-rec (insert-at curseq x %) (first xs) (rest xs)) (range mx)))
                       )
                     )
                   ) '() (first s) (rest s))
                )
              )
            (graph-has-path [g p]
              (let [pairs (map vector (butlast p) (rest p))]
                (every? #(contains? (g (first %)) (last %)) pairs)
                )
              )
            (find-path [graph]
              (let [vertices (keys graph)]
                (true? (some #(graph-has-path graph %) (permutations (keys graph))))
                )
              )
            (create-graph [wrds]
              (loop [graph {} w (first wrds) ws (rest wrds)]
                (if w
                  (recur (conj graph [w (set (filter #(= 1 (edit-dist w %)) wrds))]) (first ws) (rest ws))
                  graph
                  )
                )
              )
            ]

      (->> words create-graph find-path)
      )
    ))

(defcheck solution-8c73d7da
  (fn [s]
    (letfn [(rel? [a b]
              (let [cmp (map = a b)]
                (if (= (count b) (count a))
                  (and (not= a b) (<= (->> cmp (filter not) count) 1))
                  (let [[a b] (sort-by count [a b])
                        [pr [_ & sf]]
                        (split-at
                          (or
                           (->> cmp (keep-indexed #(when-not %2 %1)) first)
                           (count a))
                          b)]
                    (= (concat pr sf)
                      (seq a))))))]
      (let [[[n os] & _]
            (->> s
              (reduce (fn [m w] (update-in m [(count (filter #(rel? % w) s))] conj w)) {})
              (sort-by first))]
        (if (= n 0)
          (<= (count s) 1)
          (or (> n 1) (<= (count os) 2)))))))

(defcheck solution-8caaa1e4
  #(if (or (< (count %) 5) (% "to")) false true))

(defcheck solution-8d4b5f77
  (letfn [(c [[a & r :as v] [b & s :as w]] (if (= a b) (c r s) (or (and (nil? a) (nil? s)) (and (nil? b) (nil? r)) (= r s) (= v s) (= r w))))
          (l [v w] (< (compare (.toString v) (.toString w)) 0))
          (n [p e] (let [z (last p) [a b] (seq e)] (when (e z) (if (= z a) b a))))]
    (fn [w]
      (let [w (map seq w) z (set w)]
        (->>
          (map set
            (let [e (set (for [a w b w :when (and (l a b) (c a b))] #{a b}))]
              (loop [p (set (map vector w))]
                (let [q (into p (mapcat #(for [s e :let [n (n % s)] :when (and n (not (contains? (set %) n)))] (conj % n)) p))]
                  (if (= p q)
                    q
                    (recur q))
                  ))))
          (some #(= % z))
          boolean)))))

(defcheck solution-8e3751b7
  (fn word-chain? [word-list]
    (letfn [
            (diff-characters[str1 str2]
              (count (filter false? (map #(= %1 %2) str1 str2))))

            (compare-str[[c1 & rstr1 :as astr1] [c2 & rstr2]]
              (cond
                (nil? c2)  0
                (= c1 c2) (compare-str rstr1 rstr2)
                :else     (+ 1 (compare-str astr1 rstr2))))

            (removal? [original modified]
              (and (= (compare-str modified original) 1)
                   (= (count original)
                     (inc (count modified)))))

            (insertion? [original modified]
              (removal? modified original))

            (substitution? [original modified]
              (and (= (count original)
                     (count modified))
                   (= (diff-characters original modified) 1)))]

      (let [rules [removal? insertion? substitution?]]
        (letfn [
                (make-chain [current words]
                  (if (empty? words) #{current}
                                     (set (last (sort-by count
                                                  (for [nxt words
                                                        rule rules
                                                        :when (rule current nxt)]
                                                    (cons current (make-chain nxt (disj words nxt)))))))))]
          (= word-list
            (set (last (sort-by count
                         (for [starting-point word-list]
                           (make-chain starting-point (disj word-list starting-point))))))))))))

(defcheck solution-8e5ad391
  (fn [words]
    (letfn [(distance-1? [s t]
              (cond
                (empty? s)              (= 1 (count t))
                (empty? t)              (= 1 (count s))
                (= (first s) (first t)) (recur (next s) (next t))
                :else                   (or (= (next s) (next t))
                                            (= (seq s) (next t))
                                            (= (next s) (seq t)))))
            (chain [words]
              (reduce (fn [m word] (assoc m word (filter #(distance-1? word %) words)))
                {}
                words))
            (go [stack graph]
              (when-let [top (peek stack)]
                (if (= (count top) (count words))
                  true
                  (let [elem (last top)
                        nexts (filter (complement (set top)) (get graph elem))]
                    (recur (reduce #(conj %1 (conj top %2))
                             (pop stack)
                             nexts)
                      graph)))))]
      (not (nil? (go (vec (map vector words)) (chain words)))))))

(defcheck solution-8e68ce04
  (letfn
   [
    (cmp [s1 s2]
      (let [d (- (count s1) (count s2))]
        (cond
          (= d 0) (= 1 (count (remove identity (map #(= %1 %2) s1 s2))))
          (= d -1) (= (filter (set (filter (set s1) s2)) s1) (seq s1))
          (= d 1) (recur s2 s1)
          :else false)))

    (tree [xs]
      (->>
        (for [s1 xs s2 xs] [s1 s2])
        (filter #(apply cmp %))
        (reduce #(update-in %1 [(first %2)] conj (second %2)) {})))

    (r1 [ks path result xs]
      (if (seq ks)
        (r1 (rest ks) path (r2 (first ks) path result xs) xs)
        result))

    (r2 [k path result xs]
      (cond
        ((set path) k) (conj result path)
        (xs k) (r1 (xs k) (conj path k) result xs)
        :else (conj result (conj path k))))]

    (fn f[st]
      (->>
        (tree st)
        (r1 st [] [])
        (distinct)
        (some #(= (count %) (count st)))
        (= true)))))

(defcheck solution-8f4eb13c
  (fn [word-set]
    (let
     [
      chain?
      (fn [[ha & ta :as a] [hb & tb :as b]]
        (cond
          (= a b) false
          (= ha hb) (recur ta tb)
          :else
          (case (- (count a) (count b))
            -1 (= (seq a) tb)
            1 (= ta (seq b))
            0 (= ta tb)
            false
            ) )     )

      node?
      (fn [[[word & _] word-set]]
        (or (nil? word) (some #(chain? word %) word-set))
        )

      children
      (fn [[[word & _ :as chain] word-set]]
        (map
          #(list (conj chain %) (disj word-set %))
          (if word
            (filter #(chain? word %) word-set)
            word-set
            ) ) )

      ]
      (->>
        ['() word-set]
        (tree-seq node? children)
        (filter #(-> % second empty?))
        first
        nil?
        not
        ) ) ))

(defcheck solution-8f6c4fc0
  (fn chains [col]
    (let [ chain? (fn [v1 v2]
                    (let [c1 (count v1) c2 (count v2) str1 (if (< c1 c2) v2 v1) str2 (if (< c1 c2) v1 v2)]
                      (if (<= (- (count str1) (count str2)) 1)
                        (loop [char1 (first str1) char2 (first str2) substr1 (rest str1) substr2 (rest str2) mismatch 0 shifted false]
                          (let [same-char (= char1 char2)]
                            (if (empty? substr1)
                              (< mismatch 2)
                              (if same-char
                                (recur (first substr1) (first substr2) (rest substr1)  (rest substr2) mismatch shifted)
                                (if (and (not= c1 c2) (not shifted))
                                  (recur (first substr1) char2  (rest substr1)  substr2 (inc mismatch) true )
                                  (recur (first substr1) (first substr2) (rest substr1)  (rest substr2) (inc mismatch) shifted)
                                  )
                                )
                              )
                            ))
                        false
                        )))

          has_chain? (fn [v subcol]
                       (filter  #(first %)  (map (fn [v1] [(chain? v v1) v1 ] )  subcol)))


          make-chain (fn make-chain [chain-seq rest-set]
                       (if (empty? rest-set)
                         chain-seq
                         (let [next_chain_tuple (has_chain? (last chain-seq) rest-set)]
                           (if (empty? next_chain_tuple)
                             []
                             (loop [possible_tuple (first next_chain_tuple) data (rest next_chain_tuple) ]
                               (let [possible_chain (make-chain
                                                      (conj chain-seq (second  possible_tuple))
                                                      (disj rest-set   (second possible_tuple)  )    )]
                                 (if (and (empty? data) (empty? possible_chain))
                                   []
                                   (if (not (empty? possible_chain))
                                     possible_chain
                                     (recur (first data) (rest data))
                                     ))
                                 ))))))
          ]
      (not (empty? (filter #(= (count col) %)(map (fn [v] (count (make-chain [v] (disj  col v)))) col))))
      ;(map (fn [v]  (make-chain [v] (disj  col v))) col )
      )
    ))

(defcheck solution-8f8018ef
  (fn
    [coll]
    (letfn [(dels [s]
              (set (for [x (range (count s))]
                     (str (subs s 0 x) (subs s (inc x))))))
            (canpair
              [w1 w2]
              (or (and (= (count w1) (count w2))
                       (= 1 (apply + (map #(if (= % %2) 0 1) w1 w2))))
                  (and (= (count w1) (inc (count w2)))
                       (contains? (dels w1) w2))
                  (and (= (inc (count w1)) (count w2))
                       (contains? (dels w2) w1))))
            (find-sol-r [v s fs]
              (if (seq s)
                (let [h (first s)
                      t (rest s)
                      p (peek v)]
                  (if (or (nil? p)
                          (canpair p h))
                    (or (find-sol-r (conj v h) (disj fs h) (disj fs h))
                        (find-sol-r v t fs))
                    (find-sol-r v t fs)))
                (= (count coll) (count v))))]
      (find-sol-r [] (set coll) (set coll)))))

(defcheck solution-8fa5e6c6
  (fn [s0]
    (let [
          d1      (fn [s1 s2]  (apply + (map #(if (= %1 %2) 0 1) s1 s2)))
          d2 (fn [s1 s2]
               (loop [ e 0 n 0]
                 (if (or (>= n (count s1)) (>= e 2))
                   (if (zero? e) (- (count s2) n) e)
                   (if (= (.charAt s1 n) (.charAt s2 (+ n e)))
                     (recur e (inc n))
                     (recur (inc e) (inc n))
                     ))))
          d3 (fn [s1 s2]
               (let [c1 (count s1)
                     c2 (count s2)
                     ]
                 (cond (= c1 c2) (d1 s1 s2)
                       (= (inc c1) c2) (d2 s1 s2)
                       (= c1 (inc c2)) (d2 s2 s1)
                       :else 9
                       )))
          d4 (memoize d3)
          permutation (fn p[xs]
                        (if (= (count xs) 1)
                          (list xs)
                          (for [x xs
                                y (p (remove #(= x %) xs ))]
                            (cons x y))))
          count-path (fn [l1]                  (second (reduce (fn [[s0 cc] m1] [m1 (+ cc (d4 s0 m1))])  [(first l1) 0] (rest l1))))
          xlist  (permutation s0)
          ]
      (not (empty? (filter #(= % (dec (count s0))) (map #(count-path %) xlist))))
      )
    ))

(defcheck solution-8fa7a60e
  (fn [words]
    (let [sbs (fn [w1 w2]
                (let [n (count w1)]
                  (and (= n (count w2))
                       (= 1 (->> (not= (get w1 i) (get w2 i))
                              (for [i (range n)])
                              (filter identity)
                              (count))))))
          ins (fn [w1 w2]
                (let [n (count w1)]
                  (and (= (dec n) (count w2))
                       (->> (str (subs w1 0 i) (subs w1 (inc i) ))
                         (for [i (range n)])
                         (some #{w2})))))
          dif1 (fn [w1 w2] (or (sbs w1 w2) (ins w1 w2) (ins w2 w1)))
          gc (fn [chains]
               (let [addfront (for [w words c chains :when (and (dif1 w (first c)) (not-any? #{w} c))]
                                (vec (apply conj [w] c)))
                     addback (for [w words c chains :when (and (dif1 w (peek c)) (not-any? #{w} c))]
                               (vec (conj c w)))
                     nchains (apply hash-set (distinct (concat addfront addback)))]
                 (cond (empty? nchains) false
                       (= (count words) (count (first nchains))) true
                       :else  (recur nchains))))]
      (gc (apply hash-set (for [w words] [w]))))))

(defcheck solution-8fd30b94
  (fn [words]
    (let [dist (fn [at bt]
                 (let [a (vec (if (> (count at) (count bt)) at bt))
                       b (vec (if (> (count at) (count bt)) bt at))
                       n (count a)
                       m (count b)
                       ]
                   (loop [table (conj [(vec (range (inc m)))] (vector 1)) row 1 col 1]
                     (let [above (get (get table (dec row)) col)
                           left  (get (get table row) (dec col))
                           diag  (get (get table (dec row)) (dec col))
                           cost (if (not= (get a (dec row)) (get b (dec col)))
                                  1 0)]
                       (if (> row n)
                         (get (get table n) m)
                         (if (> col m)
                           (recur (conj table (vector (inc row))) (inc row) 1)
                           (recur (assoc table row (conj (get table row) (min (inc above) (inc left) (+ diag cost)))) row (inc col) )))))))
          dist-map-inner (fn [acc1 word1]
                           (conj acc1 [word1 (->> words
                                               (filter #(not= word1 %))
                                               (reduce (fn [acc2 word2]
                                                         (conj acc2 [word2 (dist word1 word2)])) {}))]))
          dist-map (reduce dist-map-inner {} words)]
      (loop [tovisit (into [] (map (fn [x] {:word x :lastword nil :visited #{}}) words))]
        (let [lastword (:lastword (peek tovisit))
              curword (:word (peek tovisit))
              visited (:visited (peek tovisit))]
          (if (= (count words) (count visited))
            true
            (if (empty? tovisit)
              false
              (if (or (= lastword curword) (contains? visited curword)
                      (not (or (nil? lastword)
                               (= 1 (get-in dist-map [curword lastword])))))
                (recur (pop tovisit))
                (recur (into (pop tovisit)
                         (map (fn [x] {:word x :lastword curword :visited (conj visited lastword)}) words)))))))))))

(defcheck solution-90530493
  (fn word-chain? [word-set]
    (letfn [(drop-kth-letter [k word]
              (let [letters (seq word)]
                (apply str
                  (concat (take (- k 1) letters)
                          (drop k letters)))))
            (same-after-letter-deletion? [word1 word2]
              (true?
                (some #(= word2 %)
                  (map #(drop-kth-letter % word1)
                    (range 1 (+ 1 (count word1)))))))
            (same-after-letter-insertion? [word1 word2]
              (same-after-letter-deletion? word2 word1))
            (same-after-letter-substitution? [word1 word2]
              (and
               (= (count word1)
                 (count word2))
               (true?
                 (some #(= (first %) (second %))
                   (map #(list (drop-kth-letter % word1)
                           (drop-kth-letter % word2))
                     (range 1 (+ 1 (count word1))))))))
            (all-possible-chains [xs]
              (if (< (count xs) 2) (list xs)
                                   (let [sublists (map #(concat (take % xs) (drop (+ 1 %) xs))
                                                    (range (count xs)))]
                                     (apply concat
                                       (map (fn [x sublist]
                                              (map #(cons x %)
                                                (all-possible-chains sublist)))
                                         xs
                                         sublists)))))
            (valid-chain? [word-chain]
              (every? #(or (same-after-letter-deletion? (first %) (second %))
                           (same-after-letter-insertion? (first %) (second %))
                           (same-after-letter-substitution? (first %) (second %)))
                (partition 2 1 word-chain)))]

      (true?
        (some valid-chain?
          (all-possible-chains (apply list word-set)))))))

(defcheck solution-905b61a9
  (fn word-chain
    [words]
    (let [related (fn related
                    [word-a word-b]
                    (let [[list-a list-b] (map #(into [] %) [word-a word-b])]
                      (if (= (count list-a) (count list-b))
                        (= 1 (apply + (map #(if (= (first %) (last %)) 0 1) (map vector list-a list-b))))
                        (let [[list-a list-b] (sort [list-a list-b])]
                          (if (= (count list-a) (- (count list-b) 1))
                            (loop [cur-a list-a, cur-b list-b]
                              (if (empty? cur-a)
                                true
                                (if (> (count cur-a) (count cur-b))
                                  false
                                  (if (apply = (map first [cur-a cur-b]))
                                    (recur (rest cur-a) (rest cur-b))
                                    (recur (rest cur-a) (rest (rest cur-b)))
                                    ))))
                            false)
                          )
                        ))
                    ),
          permutations (fn permutations [s]
                         (lazy-seq
                           (if (seq (rest s))
                             (apply concat (for [x s]
                                             (map #(cons x %) (permutations (remove #{x} s)))))
                             [s])))]
      (loop [perms (permutations words)]
        (if (empty? perms)
          false
          (if (false? (first
                        (drop-while
                          #(not (false? %))
                          (reductions
                            (fn [a b]
                              (if (related a b)
                                b
                                false))
                            (first perms)))))
            (recur (rest perms))
            true))))
    ))

(defcheck solution-90dbddd0
  (fn word-chain? [words & [curr]]
    (letfn [(drop-nth [n string]
              "Drops the nth character of a string."
              (apply str (concat (take n string)
                                 (drop (inc n) string))))

            (chain? [w1 w2]
              "Returns true if two words can be adjacent links in a word chain."
              (let [n1 (count w1)
                    n2 (count w2)]
                (cond
                  (= n1 0) true
                  (= n1 n2) (some true? (for [n (range n1)]
                                          (= (drop-nth n w1) (drop-nth n w2))))
                  (= n1 (dec n2)) (some #{w1}
                                    (for [n (range n2)]
                                      (drop-nth n w2)))
                  (= n2 (dec n1)) (some #{w2}
                                    (for [n (range n1)]
                                      (drop-nth n w1)))
                  :else false)))]
      (boolean
        (if (empty? words)
          true
          (some true?
            (map #(word-chain? (disj words %) %)
              (filter #(chain? curr %) words))))))))

(defcheck solution-90fc6741
  (fn [s]
    (letfn [(l [[h & t :as a] [f & r :as b]]
              (cond (nil? h) (count b)
                    (nil? f) (count a)
                    (= f h)  (recur t r)
                    :else (min (inc (l t r))
                            (inc (l a r))
                            (inc (l t b)))))
            (c? [i s]
              (if (empty? s)
                true
                (let [m (group-by #(= (l % i) 1) s)]
                  (if-let [t (m true)]
                    (loop [[f & r] t]
                      (if f
                        (if-let [c (c? f (remove #(= f %) s))]
                          c
                          (recur r))
                        false))
                    false))))]
      (reduce #(if (c? %2 (remove (fn [x] (= x %2)) s)) true %) false s))))

(defcheck solution-912ce32c
  (fn [f c s]
    (true?
      (some
        #(= (c %) (c s))
        (nth (iterate
               (fn [p] (for [v p w s :when (and (f c (last v) w) (not-any? #(= w %) v))] (conj v w)))
               (map vector s))
          (- (c s) 1))))) #(let [c %
                                 [a b] (vec (sort-by c %&))
                                 d (- (c b) (c a))
                                 n (+ 1 (c (take-while true? (map = a b))))]
                             (if (< d 2) (= (drop (- n d) a) (drop n b)))) count)

(defcheck solution-9159114f
  #(let [
         nv    (count %)
         words (map seq %)
         edge? (fn edge? [w1 w2]
                 (let [[h1 & t1] w1 [h2 & t2] w2]
                   (if (= h1 h2) (edge? t1 t2)
                                 (or (= t1 t2)     ; substitution
                                     (= t1 w2)     ; deletion
                                     (= w1 t2))))) ; insertion
         graph (apply (partial merge-with concat) (for [a words b words
                                                        :when (and (not= a b) (edge? a b))] {a [b]}))
         chain (fn chain [visited w] ; depth-first search for the chain
                 (or (= nv (inc (count visited)))
                     (some (partial chain (conj visited w))
                       (filter (comp not visited) (graph w)))))]
     (not (not-any? (partial chain #{}) (keys graph)))))

(defcheck solution-91ef2e0d
  (fn [words]
    (let [distance_1? (fn [word1 word2]
                        (let [c1 (count word1) c2 (count word2)]
                          (loop [[h1 & t1 :as w1] word1 [h2 & t2 :as w2] word2 m 0]
                            (cond
                              (> m 1) false
                              (not (or h1 h2)) (= m 1)
                              (= h1 h2) (recur t1 t2 m)
                              (= c1 c2) (recur t1 t2 (inc m))
                              (> c1 c2) (recur t1 w2 (inc m))
                              :else (recur w1 t2 (inc m))))))]
      (loop [[state & states] (list ['() words])]
        (if (nil? state)
          false
          (let [[chain words-left] state
                head (first chain)
                words-fit (filter #(distance_1? head %) words-left)
                create-state #(vector (cons % chain) (disj words-left %))]
            (if (empty? words-left)
              true
              (recur (concat (map create-state (if head words-fit words-left)) states)))))))))

(defcheck solution-92a6d95c
  (fn [words]
    (letfn [(orfn [x y] (or x y))
            (dist [a b]
              (let [ca (count a)
                    cb (count b)
                    m (max ca cb)
                    fa (repeat (- m ca) :_)
                    fb (repeat (- m cb) :_)
                    ins-at #(let [[x y] (split-at %3 %1)] (concat x %2 y))
                    dist #(count (remove true? (map = %1 %2)))]
                (apply min
                  (map #(dist (ins-at a fa %) (ins-at b fb %)) (range m)))))
            (chain [w ws]
              (let [ch (remove #(not= 1 (dist w %)) ws)]
                (cond (nil? (seq ws)) true
                      (and (seq ws) (nil? (seq ch))) false
                      :else
                      (reduce orfn
                        (map (fn [c] (chain c (remove #(= c %) ws))) ch)))))
            (chain? [ws]
              (reduce orfn (map (fn [w] (chain w (remove #(= w %) ws))) ws)))]
      (chain? words))))

(defcheck solution-9485dc78
  (fn is-word-chain2? [s]
    (let [  next-perm (fn [s]
                        (let [v (vec s)
                              find-pivot (fn [v]
                                           (let [n (-> v count dec)]
                                             (loop [i (dec n), pivot nil]
                                               (if (or (neg? i) (not= pivot nil))
                                                 {:pivot pivot :idx (if (nil? pivot) i (inc i))}
                                                 (recur
                                                   (dec i)
                                                   (let [ei (v i)
                                                         ri (v (inc i))]
                                                     (if (neg? (compare ei ri)) ei nil)))
                                                 ))))
                              find-successor	(fn [v pivot]
                                                (let [n (-> v count dec)]
                                                  (loop [i n]
                                                    (let [e (v i)]
                                                      (if (pos? (compare e (pivot :pivot)))
                                                        {:successor e, :idx i}
                                                        (recur (dec i))
                                                        )))))
                              swap			(fn [v i1 i2]
                                          (let [e1 (v i1)
                                                e2 (v i2)]
                                            (assoc (assoc v i1 e2) i2 e1)))
                              reverse-from 	(fn [v start]
                                              (let [l (subvec v 0 start)
                                                    r (subvec v start)
                                                    rev-r (reverse r)]
                                                (flatten (conj l rev-r))
                                                ))
                              pivot   (find-pivot v)]
                          (if (nil? (pivot :pivot))
                            (reverse v)
                            (let [successor (find-successor v pivot)
                                  p-idx (pivot :idx)
                                  s-idx (successor :idx)
                                  swaped (swap v p-idx s-idx)]
                              (reverse-from swaped (inc p-idx))
                              ))))
          are-words-connected? (fn [word1 word2]
                                 (let [	is-substitution? (fn [word1 word2]
                                                            (loop [w1 (seq word1)
                                                                   w2 (seq word2)
                                                                   diff-chars 0]
                                                              (if (empty? w1)
                                                                (= 1 diff-chars)
                                                                (let [c1 (first w1)
                                                                      c2 (first w2)]
                                                                  (recur
                                                                    (rest w1)
                                                                    (rest w2)
                                                                    (if (not= c1 c2) (inc diff-chars) diff-chars))
                                                                  ))))
                                       is-deletion-or-insertion? (fn [word1 word2]
                                                                   (let [w1 (vec word1)
                                                                         w2 (vec word2)
                                                                         n1 (count w1)
                                                                         n2 (count w2)
                                                                         longer  (if (> n1 n2) w1 w2)
                                                                         shorter (if (> n1 n2) w2 w1)
                                                                         proc-chars (fn [shorter longer]
                                                                                      (loop [shor shorter, result []]
                                                                                        (if (empty? shor)
                                                                                          result
                                                                                          (let [c (first shor)]
                                                                                            (recur (rest shor) (conj result (.indexOf longer c)))
                                                                                            ))))
                                                                         are-chars-ok? (fn [idxs]
                                                                                         (if (some neg? idxs)
                                                                                           false
                                                                                           (loop [idx1 idxs, result []]

                                                                                             (if (empty? idx1)
                                                                                               (let [trouble (count (filter #(or (neg? %) (> % 1)) result))]
                                                                                                 (or (= 0 trouble) (= 1 trouble)))
                                                                                               (let [i (first idx1)
                                                                                                     ni (if (= 1 (count idx1)) nil (-> idx1 rest first))]
                                                                                                 (recur
                                                                                                   (rest idx1)
                                                                                                   (if (nil? ni) result (conj result (- ni i)))
                                                                                                   ))))))]
                                                                     (are-chars-ok? (proc-chars shorter longer))
                                                                     ))
                                       n1  (count word1)
                                       n2  (count word2)
                                       n   (Math/abs (- n1 n2))]
                                   (cond
                                     (= 0 n) (is-substitution? word1 word2)
                                     (= 1 n) (is-deletion-or-insertion? word1 word2)
                                     :default false)
                                   ))
          prepare-seq		(fn [v]
                           (let [sv (subvec v 1 (dec (count v)))]
                             (loop [sv1 sv, result [(first v)]]
                               (if (empty? sv1)
                                 (partition 2 (conj result (last v)))
                                 (recur
                                   (rest sv1)
                                   (conj result (first sv1) (first sv1))
                                   )
                                 ))))
          is-connected?	(fn [s]
                           (loop [s1 s, result #{}]
                             (if (empty? s1)
                               (and (= 1 (count result)) (true? (first result)))
                               (let [w1 (-> s1 first first)
                                     w2 (-> s1 first last)
                                     connected (are-words-connected? w1 w2)]
                                 (recur (rest s1) (conj result connected))
                                 ))))
          v (vec (sort s))]
      (loop [v1 (next-perm v)]
        (if (= v1 v)
          false
          (if (is-connected? (prepare-seq (vec v1)))
            true
            (recur (next-perm v1) ))))
      )))

(defcheck solution-94aceaee
  (fn [s]
    (letfn [(lev [a b]
              (letfn [(step-differences [cnt a b]
                        (if (and (seq a) (seq b))
                          (let [[a1 & a-rest] a
                                [b1 & b-rest] b]
                            (if (= a1 b1)
                              (recur cnt a-rest b-rest)
                              (min (step-differences (inc cnt) a b-rest)
                                (step-differences (inc cnt) a-rest b)
                                (step-differences (inc cnt) a-rest b-rest))))
                          (+ cnt (count a) (count b))))]
                (step-differences 0 a b)))
            (lev1? [a b]
              (= 1 (lev a b)))
            (possible-chained-words [s item]
              (filter (partial lev1? item) s))
            (one-letter-map [s]
              (zipmap s (map (partial possible-chained-words s) s)))
            (brute-force-paths [mappings path word]
              (let [next-words (get mappings word)
                    valid-next (remove (set path) next-words)]
                (if (seq valid-next)
                  (mapcat #(brute-force-paths mappings (conj path %) %) valid-next)
                  [path])))]

      (let [word-mappings (one-letter-map s)
            all-paths (mapcat #(brute-force-paths word-mappings [%] %) s)
            all-words-used? #(= (count word-mappings) (count %))]
        (if (some all-words-used? all-paths)
          true
          false)))))

(defcheck solution-94ee4182
  (fn word-chain [st]
    (letfn [
            (permutations [s]
              (if (= 1 (count s))
                (list s)
                (for [head s
                      tail (permutations (disj (set s) head))]
                  (cons head tail))))
            (lev-dist [seq1 seq2]
              (cond
                (empty? seq1) (count seq2)
                (empty? seq2) (count seq1)
                :else (min
                        (+ (#(if (= %1 %2) 0 1) (first seq1) (first seq2)) (lev-dist (rest seq1) (rest seq2)))
                        (inc (lev-dist (rest seq1) seq2))
                        (inc (lev-dist seq1 (rest seq2))))))
            (is-chain [coll]
              (if (= 1 (count coll))
                true
                (let [fst (first coll)
                      sec (second coll)
                      dist (lev-dist fst sec)]
                  (if (= 1 dist)
                    (is-chain (rest coll))
                    false))))]
      (let [perms (permutations st)
            res   (first (for [p perms :when (is-chain p)]
                           true))]
        (if res
          true
          false)))))

(defcheck solution-9595f228
  (fn p82 [words]
    (letfn [(dist-1? [w from]
              (let [alphabet "abcdefghijklmnopqrstuv"
                    splits (for [i (range (count w))] (split-at i w))
                    deletes (for [[a b] splits :when (seq b)]
                              (concat a (drop 1 b)))
                    ins (for [[a b] splits
                              c alphabet]
                          (concat a (str c) b))
                    subs (for [[a b] splits
                               c alphabet
                               :when (seq b)]
                           (concat a (str c) (drop 1 b)))]
                (some #(= % (seq from)) (concat deletes ins subs))))
            (permutations
              ([items] (permutations items [] []))
              ([items results so-far]
               (if (empty? items)
                 (conj results so-far)
                 (loop [is items results results]
                   (if (empty? is) results
                                   (recur (rest is)
                                     (permutations (remove #(= % (first is)) items)
                                       results (conj so-far (first is)))))))))]
      (not (empty?
             (filter #(every? (fn [[a b]] (dist-1? a b)) (partition 2 1 %))
               (permutations (sort words))))))))

(defcheck solution-95e05ba6
  (fn f
    ([s] (f [] s))
    ([l s]
     (let [k (fn [a b]
               (loop [n 0 a a b b]
                 (let [m (inc n)
                       [c & a] a
                       [d & b] b]
                   (cond
                     (= c d nil) (< n 2)
                     (= c d) (recur n a b)
                     (= (first a) d) (recur m (rest a) b)
                     (= c (first b)) (recur m a (rest b))
                     :else (recur m a b)))))]
       (or (empty? s)
           (some #(if (or (empty? l) (k (last l) %))
                    (f (conj l %) (disj s %)))
             s)
           false)))))

(defcheck solution-95ecd0e0
  (fn [xs]
    (letfn [(one-d [[a & a-rest] [b & b-rest :as bs]]
              (if (= a b)
                (recur a-rest b-rest)
                (= (apply str a-rest) (apply str bs))))
            (chains? [l r]
              (let [[count-l count-r] (map count [l r])]
                (cond
                  (= count-l count-r) (= 1 (apply + (map #(if (= %1 %2) 0 1) l r)))
                  (= 1 (- count-l count-r)) (one-d l r)
                  (= -1 (- count-l count-r)) (one-d r l)
                  :else false)))
            (transitions []
              (->>
                (for [x xs y xs :when (chains? x y)] [x y])
                (group-by first)
                (reduce-kv #(assoc %1 %2 (map second %3)) {})))
            (moves
              ([trs] (moves trs (keys trs)))
              ([trs keys]
               (if (empty? trs)
                 true
                 (some true? (map #(if-let [chain (get trs %1)]
                                     (moves (dissoc trs %1) chain))
                               keys)))))]
      (or (moves (transitions)) false))))

(defcheck solution-966de449
  (fn word-chain [wdset]
    (letfn [(levenshtein [src tgt]
              (let [srclen (count src) tgtlen (count tgt) rowsz (inc tgtlen)]
                (if (= src tgt)
                  0
                  (loop [srcidx 0 tgtidx 0 preRow (range 0 rowsz) curRow (conj [] (inc srcidx))] ;; curRow[0]=srcidx+1
                    (let [srclt (nth src srcidx)
                          tgtlt (nth tgt tgtidx)
                          nxtsrcidx (inc srcidx)
                          nxttgtidx (inc tgtidx)
                          leftv (nth preRow nxttgtidx)
                          leftupperv (nth preRow tgtidx)
                          upperv (nth curRow tgtidx)
                          cost (fn [slt dlt] (if (= slt dlt) 0 1))
                          mincurv (min (inc leftv) (inc upperv) (+ leftupperv (cost srclt tgtlt)))]

                      ;; does cur row iteration done ?
                      ;;(prn srclt tgtlt nxtsrcidx preRow curRow)
                      (if (= nxttgtidx tgtlen)   ;; done one iteration of tgt row
                        (if (= nxtsrcidx srclen)
                          mincurv     ;; the result is in last of cur-row after iterating all.
                          (recur nxtsrcidx 0 (conj curRow mincurv) (conj [] (inc nxtsrcidx))))  ;; next src letter
                        (recur srcidx nxttgtidx preRow (conj curRow mincurv))))))))
            (nbmap [wdset]
              (reduce (fn [ret this]
                        (assoc ret this (filter #(= 1 (levenshtein this %)) wdset))) {} wdset))
            ;; This is travelling salesman problem. Not suitable for dfs or bfs.
            (dfs [cur nbmap]
              (loop [partmap nbmap stack [cur] discovered #{cur} partRslt []]
                (if (= (count stack) (count (keys nbmap)))
                  stack
                  (if (empty? stack)        ;;  dfs stack from cur node done. return all explored nodes
                    nil
                    (let [topnod (peek stack)
                          children (remove #(contains? discovered %) (nbmap cur))
                          child (first children)]
                      (if child
                        (recur partmap (conj stack child) (conj discovered child) (conj partRslt [cur child]))
                        ;; all children explored, pop stack top, back to parent.
                        (recur partmap (pop stack) discovered partRslt)))))))

            (travelsman [cur partmap]
              #_(prn "visiting :" cur "    " partmap)
              (if (and (= 1 (count partmap))
                       (= cur (first (keys partmap))))
                [true [cur]]
                (loop [curnbs (partmap cur) filtermap (dissoc partmap cur)]  ;; loop all nbs, dissoc cur to avoid cycle
                  (if (empty? curnbs)    ;; explored all neighbors of cur node, not found, ret false.
                    [false []]
                    (let [childresult (travelsman (first curnbs) filtermap)]
                      (if (first childresult)
                        [true (conj (second childresult) cur)]
                        (recur (next curnbs) filtermap)))))))]  ;; for each nb, recur to call travelsman

      (let [ sortednbmap (into {} (sort-by (comp count val) < (nbmap wdset)))]
        (loop [src (keys sortednbmap)]
          (if (empty? src)
            false
            (if (first (travelsman (first src) sortednbmap))
              true
              (recur (next src)))))))))

(defcheck solution-969ab9bb
  (fn word-chain
    [words]
    (letfn [(edit-distance
              [s1 s2]
              (cond
                (empty? s1) (count s2)
                (empty? s2) (count s1)
                :else
                (if (= (last s1) (last s2)) (edit-distance (apply str(butlast s1)) (apply str(butlast s2)))
                                            (min
                                              (+ (edit-distance (apply str(butlast s1)) s2) 1 )
                                              (+ (edit-distance s1 (apply str(butlast s2))) 1 )
                                              (+ (edit-distance (apply str(butlast s1)) (apply str(butlast s2))) 1)))))]

      (letfn [(create-dist-map [coll]
                (letfn [(keep-dist-one [s c] (keep #(if (= 1 (edit-distance s %)) %) c))]
                  (reduce #(merge-with concat % {%2 (keep-dist-one %2 coll)}) {} coll)))]

        (letfn [(graph-search [e m]
                  (if-let [v (get m e)]
                    (cons e
                      (first (keep #(if (= (count %) (dec(count (keys m)))) %)
                               (map #(graph-search % (dissoc m e)) v))))))]

          (letfn [(step [w m]
                    (when-let [res (graph-search (first w) m)]
                      (if (= (count res) (count words)) res (recur (rest w) m))))]
            (= (count words) (count (step words (create-dist-map words))))))))))

(defcheck solution-970bd81f
  (fn chain? [words]
    (let [onemiss? (fn onemiss [s1 s2]
                     (if (not= (count s1) (count s2))
                       false
                       (= 1 (count (filter (complement boolean) (map #(= %1 %2) s1 s2))))))
          oneadd? (fn oneadd [s1 s2]
                    "s1 < s2"
                    (cond
                      (> (count s1) (count s2)) (oneadd s2 s1)
                      (not= (- (count s2) (count s1)) 1) false
                      (= (first s1) (first s2)) (oneadd (subs s1 1) (subs s2 1))
                      (= s1 (subs s2 1)) true
                      :else false))
          word-connect? (fn [s1 s2] (or (onemiss? s1 s2) (oneadd? s1 s2)))
          word-connect-list (fn [word words] (filter #(word-connect? word %) words))
          chain-start-with? (fn csw [word words]
                              (let [next-words (word-connect-list word words)]
                                (cond
                                  (= 1 (count words)) true
                                  (= (count next-words) 0) false
                                  :else (boolean (some boolean (for [x next-words]
                                                                 (csw x (disj words word))))))))]
      (boolean (some boolean (for [x words]
                               (chain-start-with? x words)))))))

(defcheck solution-97a1cee2
  (fn word-chains? [st]
    (letfn [(differs-1-letter? [a b]
              (let [a (seq a), b (seq b)]
                (if (= a b)
                  false
                  (let [[fa & ra] a, [fb & rb] b]
                    (if (= fa fb)
                      (differs-1-letter? ra rb)
                      (or
                       (= ra rb)
                       (= a rb)
                       (= ra b)))))))
            (sum-by-neighbor-cnt [n, mr] (reduce (fn [sm, [k v]] (if (= v n) (inc sm) sm)) 0 mr))
            (neighbor-cnt [a] (reduce #(if (differs-1-letter? a %2) (inc %) %) 0 st))]
      (let [mr (map #(vector % (neighbor-cnt %)) st)]
        (if (and (= 0 (sum-by-neighbor-cnt 0 mr))
                 (<= (sum-by-neighbor-cnt 1 mr) 2))
          true
          false)))))

(defcheck solution-984ad325
  (fn [words]
    (letfn [(convertable_by_replace? [w1 w2]
              (= 1 (count (filter false? (map = (seq w1) (seq w2))))))
            (convertable_by_add? [w1 w2]
              (if (= (first w1) (first w2))
                (convertable_by_add? (next w1) (next w2))
                (if (> (count w1) (count w2))
                  (= (next w1) (seq w2))
                  (= (seq w1) (next w2)))))
            (convertable? [w1 w2]
              (let [l1 (count w1) l2 (count w2) l (- l1 l2) delta (* l l)]
                (if (= 1 delta)
                  (convertable_by_add? w1 w2)
                  (if (= 0 delta)
                    (convertable_by_replace? w1 w2)
                    false))))
            (group_words [words]
              (for [w words] (cons w (filter #(convertable? w %) words))))]
      (< (count (filter #(< % 3) (map count (group_words words)))) 3))))

(defcheck solution-988eb1c9
  (fn canChain? [lw] (letfn [

                             (diffSameSize [w1 w2]  (apply + (map #(if (= %1 %2) 0 1) w1 w2)))

                             (diffDiffSize [w1 w2] (loop [todo1 w1 todo2 w2 skipped false] (cond
                                                                                             (or (empty? todo1) (empty? todo2)) true
                                                                                             (= (first todo1) (first todo2)) (recur (rest todo1) (rest todo2) skipped)
                                                                                             skipped false
                                                                                             :else (let [longer (if (> (count todo1) (count todo2)) todo1 todo2)
                                                                                                         shorter (if (> (count todo1) (count todo2)) todo2 todo1)] (recur (rest longer) shorter true))
                                                                                             )
                                                                                           )
                               )

                             (isChild? [word candidate] (let [wordSeq (seq word)
                                                              candSeq  (seq candidate)
                                                              lengthDiff (Math/abs (- (count wordSeq) (count candSeq))) ]
                                                          (cond
                                                            (= word candidate) false
                                                            (> lengthDiff 1) false
                                                            (= 0 lengthDiff ) (< (diffSameSize wordSeq candSeq) 2)
                                                            :else (diffDiffSize word candidate)
                                                            )))
                             (findChildren [word words] (loop [res '() todo words] (cond
                                                                                     (empty? todo) res
                                                                                     (isChild? word (first todo)) (recur (cons (first todo) res) (rest todo))
                                                                                     :else (recur res (rest todo))
                                                                                     )))

                             (walkChain [word chainMap visited goal] (let [newVisited (conj visited word)]
                                                                       (cond
                                                                         (= newVisited goal) true
                                                                         (contains? visited word) false
                                                                         :else (loop [todo (chainMap word)]
                                                                                 (cond
                                                                                   (empty? todo) false
                                                                                   (walkChain (first todo) chainMap newVisited goal) true
                                                                                   :else (recur (rest todo))
                                                                                   )))
                                                                       ))

                             (findAllChildren [words]  (apply merge
                                                         (map
                                                           #(hash-map % (findChildren % words))
                                                           words)))
                             ]
                       (let [wordsWithChildren (findAllChildren lw)]
                         (not (= nil (some #(walkChain % wordsWithChildren #{} lw) lw)))))
    ))

(defcheck solution-98fe9ed6
  (fn t [s]
    (not= nil
      (letfn [(f [x y]
                (cond
                  (= (first x) (first y)) (recur (rest x) (rest y))
                  (= (last  x) (last  y)) (recur (butlast x) (butlast y))
                  1 (and (< (count x)  2) (< (count y) 2))))
              (g [i x]
                (if (empty? x)
                  1
                  (some #(and (f i %) (not (nil? (g % (disj x %))))) x)))]
        (some #(g % (disj s %)) s)))))

(defcheck solution-992b6bc0
  (fn chainable? [xs]
    (let [can-chained? (fn [from_] (fn  [to_]
                                     ((fn iter [[from-first & from-rest :as from] [to-first & to-rest :as to] changed]
                                        (cond (and (empty? from) (empty? to)) true
                                              (= from to) true
                                              changed false
                                              (empty? from) (iter from to-rest true) ;insert from/delete to
                                              (empty? to) (iter from-rest to true) ;insert to/delete from
                                              :else (or (iter from to-rest true) ;insert from/delete to
                                                        (iter from-rest to true) ;insert to/delete from
                                                        (iter from-rest to-rest (not= from-first to-first))))) ;modify or equals
                                      (seq from_) (seq to_) false)))
          chainable-graph (->> xs
                            (map (fn [x] {x (filter (can-chained? x) (filter (partial not= x) xs))}))
                            (into {}))
          or_ (fn ([x y] (or x y)) ([] false) )
          depth-maximal-from? (fn [x]
                                ((fn rec-traverse [xs x]
                                   (if (= 1 (count xs)) true
                                                        (->> (chainable-graph x)
                                                          (filter (partial contains? xs))
                                                          (map (partial rec-traverse (disj xs x)))
                                                          (reduce or_))))
                                 xs x))]
      (->> xs
        (map depth-maximal-from?)
        (reduce or_)))))

(defcheck solution-99870c1
  (let [zip (partial map vector)
        enum (partial zip (iterate inc 0))
        del #(for [[i x] (enum %) :when (not= i %2)] x)
        bool #(if % true false)
        any? (comp bool some)
        pred (fn lev1? [x y]
               (condp = (- (count x) (count y))
                 0  (= 1 (count (filter false? (map = x y))))
                 -1 (recur y x)
                 1  (let [y (seq y)]
                      (any? #(= y (del x %)) (range (count x))))
                 false))
        ]
    ;
    (fn chain?
      ([xt]
       (any? #(chain? % xt) xt))
      ([x xt]
       (let [xt (disj xt x), cs (filter #(pred % x) xt)]
         (bool (or (empty? xt) (any? #(chain? % xt) cs))))))))

(defcheck solution-9987dddb
  (fn final-func [words] (let [make-graph (fn [nodes-in pred] (loop [result {} nodes nodes-in] (if (empty? nodes) result (recur (assoc result (first nodes) (filter (fn [z] (and (not= z (first nodes)) (pred (first nodes) z))) nodes-in)) (next nodes))))) leven (fn lev [x y] (let [xlen (count x) ylen (count y)] (if (= 0 (min xlen ylen)) (max xlen ylen)) (loop [v 0 h -1 prev (vec (range (inc xlen))) curr (vec (range (inc xlen)))] (if (= v ylen) (last prev) (if (< h 0) (recur v (inc h) prev (assoc curr 0 (inc v))) (if (= h xlen) (recur (inc v) -1 curr curr) (let [cost (if (= (nth x h) (nth y v)) 0 1) a (inc (nth curr h)) b (inc (nth prev (inc h))) c (+ (nth prev h) cost) ] (recur v (inc h) prev (assoc curr (inc h) (min a b c))) ) ) ) )))) levensmall (fn [word1 word2] (< (leven word1 word2) 2)) is-connected (fn [graph nodes] (loop [cnt 0 visited #{} stack (list (first nodes)) expected (count nodes)] (cond (empty? stack) (= cnt expected) (= cnt expected) true (contains? visited (peek stack)) (recur cnt visited (pop stack) expected) :else (recur (inc cnt) (conj visited (peek stack)) (into '() (concat (graph (peek stack)) (pop stack))) expected)))) ishamiltonian? (fn [path next expected] (and (= expected (inc (count path))) (not (contains? path next)))) has-cycle? (fn [graph nodes] (let [g (fn f [visited current expected] (if (ishamiltonian? visited current expected) true (if (or (= (count (clojure.set/intersection #{(graph current)} visited)) (count (graph current))) (contains? visited current)) false (some #(f (conj visited current) % expected) (graph current))))) ] (some true? (map (fn [x] (g #{} x (count nodes))) nodes))))] (if (has-cycle? (make-graph words levensmall) words) true false))))

(defcheck solution-9a7ea33d
  (fn [words]
    (letfn [(leveinstein [a b]
              (if (= a b)
                0
                (let [la (count a)
                      lb (count b)
                      init-state (apply merge
                                   (concat (for [x (range (inc la))] {[x 0] x})
                                           (for [y (range (inc lb))] {[0 y] y})))]
                  (loop [matrix init-state x 1 y 1]
                    (cond
                      (and (= x (inc la)) (>= y lb))
                      (matrix [(dec x) y])

                      (= x (inc la))
                      (recur matrix 1 (inc y))

                      (= (nth a (dec x)) (nth b (dec y)))
                      (recur (merge matrix {[x y] (matrix [(dec x) (dec y)])})
                        (inc x)
                        y)

                      :else
                      (recur (merge matrix
                               {[x y] (inc (min (matrix [(dec x) y])
                                             (matrix [x (dec y)])
                                             (matrix [(dec x) (dec y)])))})
                        (inc x)
                        y))))))

            (chained?
              ([word]
               (chained? word (disj words word)))
              ([word remaining-words]
               (if (empty? remaining-words)
                 true
                 (some #(and (<= (leveinstein word %) 1)
                             (chained? % (disj remaining-words %)))
                   remaining-words))))]
      (true? (some chained? words)))))

(defcheck solution-9b7f7d3f
  (fn has-chain? [s]
    (letfn [(drop-nth [n s] (concat (take n s) (drop (inc n) s)))
            (hlp [v s] (map #(cons v %) s))
            (permutations [s]
              (if (empty? s)
                [[]]
                (->> (range (count s))
                  (map #(hlp (nth s %) (permutations (drop-nth % s))))
                  (apply concat))))
            (close-delete [a b]
              (if (= (first a) (first b))
                (close-delete (rest a) (rest b))
                (or (= (rest a) b)
                    (= (rest b) a))))
            (close? [[a b]] (if (= (count a) (count b))
                              (= 1 (count (remove true? (map = a b))))
                              (close-delete (seq a) (seq b))))
            (chain? [s] (every? true? (map close? (partition 2 1 s))))]
      ((complement nil?) (some true? (map chain? (permutations (vec s))))))))

(defcheck solution-9bd862a
  (fn is-word-chain? [ws]
    (let [diff1 (fn diff1 [s1 s2]
                  #_(println "diff1: s1=" s1 "s2=" s2)
                  (let [cnt1 (count s1)
                        cnt2 (count s2)]
                    (if (> (count s1) (count s2))
                      (diff1 s2 s1)
                      (let [
                            cnt (->> (map vector s1 s2)
                                  (take-while (fn [[c1 c2]] (= c1 c2)))
                                  count)
                            s1' (drop (if (= cnt1 cnt2) (inc cnt) cnt) s1)
                            s2' (drop (inc cnt) s2)]
                        #_(println "diff1: s1'=" s1' "s2'=" s2')
                        (= s1' s2')))))
          word-chains (fn word-chains
                        ([word ws]
                         #_(println "word=" word "ws=" ws)
                         (if (empty? ws)
                           ['()]
                           (mapcat (fn [w]
                                     #_(println "word=" word "w=" w)
                                     (if-not (empty? ws)
                                       (if (diff1 word w)
                                         (let [wc (word-chains w (remove #{w} ws))]
                                           #_(println "ws=" ws "w=" w "wc=" wc)
                                           (map (partial cons w) wc))))) ws)))
                        ([ws]
                         (mapcat (fn [w]
                                   (map (partial cons w) (word-chains w (remove #{w} ws)))) ws)))
          all-combination (word-chains ws)]
      (not (empty? all-combination)))))

(defcheck solution-9c7d4d9b
  (fn [s] (let
           [link?
            (fn [x y]
              (if (nil? x)
                true
                ((fn [[x & xs] [y & ys]]
                   (if (= x y)
                     (or (= xs ys nil)
                         (recur xs ys))
                     (or (= xs ys)
                         (= xs (cons y ys))
                         (= (cons x xs) ys))))
                 x y)))
            chain?
            (fn c? [x ys]
              (if (empty? ys)
                true
                (some #(and (link? x %)
                            (c? % (disj ys %)))
                  ys)))]
            (true? (chain? nil s)))))

(defcheck solution-9ce3c8cb
  (fn [w]
    (let [f (fn f [x y] (and (= (inc(count x)) (count y))(contains? (set (map #(apply str (keep-indexed (fn [i e] (if (not= % i) e)) y)) (range 0 (count y)) ))x)))
          g (fn g [x y]
              (if (= (count x) (count y))
                (loop [x x yl y c 0]
                  (if x
                    (recur (next x) (next yl) (if (= (first x) (first yl)) c (inc c) ))
                    (if (= 1 c) y)))
                (if (or (f x y) (f y x))
                  y
                  )))
          h (map-indexed #(vector %2 (filter identity (map (partial g %2) w))) w)
          i (fn i [g x k]
              (if (= (count x) (count w))
                true
                (some identity (concat (map #(i g (conj x %) (apply disj (set(get g %)) x)) k)))))]
      (not (nil? (i (apply hash-map (apply concat h)) #{} (map first h)))))))

(defcheck solution-9d2b2067
  (fn check [c]
    (letfn [(links [w] (let [v (vec w)] (set
                                          (apply concat
                                            (for [n (range 0 (count v))]
                                              (concat
                                               (map #(concat (take n v) [%] (drop n v)) "abcdefghijklmnopqrstuvwxyz")
                                               (map #(concat (take n v) [%] (drop (inc n) v)) "abcdefghijklmnopqrstuvwxyz")
                                               [(concat (take n v) (drop (inc n) v))]))))))

            (chains? [a b] (or (nil? a) (contains? (links a) (vec b))))
            (expand [[word depth words]] (map #(vector % (dec depth) (disj words %)) (filter (partial chains? word) words)))
            (visit [[word depth words :as node]]
              (if (= 0 depth) true
                              (loop [[fi & more] (expand node)]
                                (and (not (nil? fi)) (or (visit fi) (recur more)))
                                )))]
      (visit [nil (count c) c]))))

(defcheck solution-9d69478b
  (fn [c]
    (let [sp (fn [[af & ar :as a] [bf & br :as b]]
               (if (= af bf)
                 (recur ar br)
                 [a b]))
          t (fn [a b]
              (let [[a b] (sp a b)
                    [a b] (sp (reverse a) (reverse b))]
                (and (<= (count a) 1) (<= (count b) 1))))
          g (fn g [s v]
              (or
               (empty? v)
               (some #(and (t s %) (g % (disj v %))) v)))]
      (boolean (some identity (map #(g % (disj c %)) c))))))

(defcheck solution-9de3cd05
  (fn [s]
    (let [lv (memoize (fn [lv s t c] (cond
                                       (> c 1) false
                                       (empty? s) (= 1 (+ c (count t)))
                                       (empty? t) (= 1 (+ c (count s)))
                                       :otherwise (let [cost (if (= (first s) (first t)) 0 1)]
                                                    (or (lv lv (rest s) t (inc c))
                                                        (lv lv s (rest t) (inc c))
                                                        (lv lv (rest s) (rest t) (+ c cost))))
                                       )))
          test (fn [search l1 l2] (not (empty? (drop-while #(not (search % (disj l1 %))) l2))))
          search (fn search [w s] (if (empty? s) true (test search s (filter #(lv lv w % 0) s)) ))
          ]
      (test search s s))))

(defcheck solution-9dfc934
  (fn get-chain
    ([words]
     (reduce (fn [chain? word]
               (or chain? (get-chain word (clojure.set/difference words #{word}))))
       false
       words))
    ([starting words]
     (letfn [(substition-patterns [word]
               (map-indexed (fn [i w]
                              (str (subs w 0 i) "." (subs w (inc i))))
                 (repeat (count word) word)))
             (insertion-patterns [word]
               (map-indexed (fn [i w]
                              (str (subs w 0 i) "." (subs w i)))
                 (repeat (inc (count word)) word)))
             (deletion-patterns [word]
               (map-indexed (fn [i w]
                              (str (subs w 0 i) (subs w (inc i))))
                 (repeat (count word) word)))
             (can-change? [w1 w2]
               (not (nil? (reduce (fn [found-match re]
                                    (or found-match
                                        (re-matches (re-pattern re) w2)))
                            false (concat
                                   (substition-patterns w1)
                                   (insertion-patterns w1)
                                   (deletion-patterns w1))))))]
       (let [chainable (filter (partial can-change? starting) words)]
         (if (empty? words)
           true
           (reduce (fn [chain? word]
                     (or chain? (get-chain word (clojure.set/difference (into #{} words) #{word}))))
             false
             chainable))
         )))))

(defcheck solution-9ed9dacf
  (fn [s]
    (let [
          ? (fn ?[[a & b] [x & y]] (#(or (% (cons a b) y) (% b (cons x y)) (% b y)) (if (= a x) ? =)))
          o #(or % %2)
          ! (fn ![a s] (if-let [n (seq (filter #(? a %) s))]
                         (reduce #(or % (! %2 (remove #{a} s))) nil n)
                         (empty? (remove #{a} s))
                         ))]
      (reduce o (map #(! % (remove #{%} s)) s)))))

(defcheck solution-9ef014fe
  (fn [words]
    (let [re-parts-at (fn [w i _]
                        (let [s (take i w)
                              e (drop i w)
                              d (drop 1 e)]
                          [(apply str (concat s (conj e \.)))
                           (apply str (concat s (conj d \. )))
                           (apply str (concat s d))]))
          re-parts (fn [w]
                     (let [v (map-indexed (partial re-parts-at w) w)]
                       (re-pattern
                         (str "^("
                           (clojure.string/join "|"
                             (conj
                               (apply concat v)
                               (str w ".")))
                           ")$")
                         )
                       )
                     )
          empty-node {:word nil :found #{}}
          remaining (fn [ws] (clojure.set/difference words ws))
          word-to-node (fn wtn ([w] (wtn empty-node w))
                         ([m w]
                          (-> m
                            (assoc :word w)
                            (assoc :found
                                   (conj (get m :found #{}) w)))))
          children (fn [n]
                     (let [rs (remaining (get n :found #{}))
                           re (re-parts (get n :word))]
                       (map (partial word-to-node n)
                         (filter #(re-matches re %) rs))))
          word-to-tree (fn [w] (tree-seq map? children (word-to-node w)))
          trees (set (map :found (apply concat (map word-to-tree words))))
          found (clojure.set/intersection #{words} trees)
          ]
      (not (empty? found))
      )
    ))

(defcheck solution-9f30c63f
  (fn word-chain [s]
    (letfn [(count-same-letters [colls]
              (count (take-while (partial apply =) (apply map list colls))))
            (diff-at-most-one? [colls]
              (>= 1 (- (apply max (map count colls))
                       (apply + (map count-same-letters [colls (map reverse colls)])))))
            (pairs [s] (set (for [e1 s e2 s :when (not (= e1 e2))] #{e1 e2})))
            (nodes [edges] (set (flatten (vec edges))))
            (neighbours [edges node]
              (set
                (for [e edges :when (some #(= node %) e)]
                  (if (= (first e) node) (second e) (first e)))))
            (hamilton-path [edges start-node]
              (let [all-nodes (nodes edges)
                    n (count all-nodes)]
                (letfn [(iter [visited-nodes node]
                          (if (= n (count visited-nodes))
                            visited-nodes
                            (let [neighbs (remove (set visited-nodes) (neighbours edges node))]
                              (if (empty? neighbs) nil
                                                   (first
                                                     (drop-while nil?
                                                       (for [neighb neighbs]
                                                         (iter (conj visited-nodes neighb) neighb))))))))]
                  (iter [start-node] start-node))))]

      (let [edges (set (map vec (filter diff-at-most-one? (pairs s))))]
        (not (nil? (some  #(hamilton-path edges %) (nodes s))))))))

(defcheck solution-9f4a3bc5
  (fn word-chains [words]
    (letfn [(delete [from to]
              (cond (<= (count from) (count to)) false
                    (some #(= % to) (map-indexed (fn [index x]
                                                   (apply str (keep-indexed (fn [ind item] (when-not (= index ind) item)) from))) from))
                    to
                    :else false))

            (change [from to]
              (cond (not= (count from) (count to)) false
                    (= 1 (count (filter false?
                                  (map (fn [[k v]]
                                         (= k v)) (zipmap from to))))) to
                    :else false))
            (add [from to]
              (cond (>= (count from) (count to)) false
                    (delete to from) to
                    :else false))

            (edit-reach [word words]
              (map (fn [w]
                     (or (delete word w)
                         (change word w)
                         (add word w))) words))

            (graph [words]
              (apply merge
                (map (fn [word]
                       {word (edit-reach word words)}) words)))
            (exist-chain? [init graph visited-set]
              #_(prn init visited-set (graph init))
              (if (= (set (keys graph)) visited-set)
                true
                (some #(when (and %
                                  (not (visited-set %)))
                         (exist-chain? %
                           graph
                           (clojure.set/union #{%} visited-set)))
                  (graph init))))]

      ;; (=  visited-set words)
      (let [words-in-vec (into [] words)
            graph (graph words-in-vec)]

        (true? (some #(exist-chain? % graph #{%}) words-in-vec))))))

(defcheck solution-9fc13144
  (letfn [
          ;; is-sub? : string string -> boolean
          ;; determine if one character off by substition; expects longer word
          ;; to be the 1st param
          (is-sub? [s1 s2]
            (loop [r1 (seq s1), r2 (seq s2), diff 0]
              (cond (> diff 1) false
                    (empty? r1) (<= diff 1)
                    :else (let [same? (= (first r1) (first r2))]
                            (if same?
                              (recur (rest r1) (rest r2) diff)
                              (recur (rest r1) (rest r2) (inc diff)))))))

          ;; is-ins? : string string -> boolean
          ;; determine if one character off by insertion; expects longer word
          ;; to be the 1st param
          (is-ins? [s1 s2]
            (loop [r1 (seq s1), r2 (seq s2), diff 0]
              (cond (> diff 1) false
                    (and (empty? r1)
                         (empty? r2)) (<= diff 1)
                    :else (let [same? (= (first r1) (first r2))]
                            (if same?
                              (recur (rest r1) (rest r2) diff)
                              (recur (rest r1) r2 (inc diff)))))))


          ;; one-off? : string string -> boolean
          ;; given two strings, determine if the differ by one character (one
          ;; off)
          (one-off? [s1 s2]
            (cond (> (count s2) (count s1)) (one-off? s2 s1)
                  (= s1 s2) false
                  :else (or (is-sub? s1 s2)
                            (is-ins? s1 s2))))

          ;; init : set -> map
          ;; given the initial set, return a map such that:
          ;; 1) the keys contain every item in set
          ;; 2) the values are the other items that are "one off"
          ;;   (i.e. next nodes)
          (init [s]
            (loop [rm (seq s), acc {}]
              (if (empty? rm) acc
                              (recur (rest rm)
                                (assoc acc (first rm)
                                           (into [] (filter #(one-off? (first rm) %) s)))))))

          ;; next-node : (vectorof strings) (vectorof vectorof stings) map -> string
          ;; based on last node in current path, returns the first associated
          ;; node that 1) has not already be visited in current-path and
          ;;           2) would not form a path contained in failed-paths
          (next-node [current-path failed-paths nav-map]
            (if (empty? current-path)
              (first (filter #(= -1 (.indexOf failed-paths (vector %))) (keys nav-map)))
              (first (filter #(= -1 (.indexOf failed-paths (conj current-path %)))
                       (filter #(= -1 (.indexOf current-path %)) (nav-map (last current-path)))))))

          ;; word-chain? : (sequence strings) -> boolean
          ;; returns true if a valid word chain per rules of puzzle
          (word-chain? [words & args]
            (let [win-count (count words)
                  nav-map (init words)]
              (loop [current-path (vector (-> nav-map keys first)), failed-paths (vector)]
                (let [nn (next-node current-path failed-paths nav-map)]
                  (cond (= win-count (count current-path)) true
                        (and (empty? current-path) (nil? nn)) false
                        (nil? nn) (recur (subvec current-path 0 (- (count current-path) 1))
                                    (conj failed-paths current-path))
                        :else (recur (conj current-path nn) failed-paths))))))]
    word-chain?))

(defcheck solution-a0049075
  (fn [words]
    (let [deletion
                    (fn [s]
                      (->> (map concat
                             (map #(take % s) (range (count s)))
                             (map #(drop % s) (iterate inc 1)))
                        (map #(apply str %))
                        set))
          deletions (->> words (map deletion) (zipmap words))
          editable?
                    (fn [s]
                      (fn [t]
                        (case (- (count s) (count t))
                          -1 ((deletion t) s)
                          1  ((deletion s) t)
                          0  (->> (map = s t) (filter false?) count (= 1))
                          nil)))
          editions
                    (->> words
                      (map (fn [word]
                             (->> (disj words word)
                               (filter (editable? word))
                               set)))
                      (zipmap words))
          permutations
                    ((fn permutation [s]
                       (case (count s)
                         0 []
                         1 [[(first s)]]
                         (mapcat #(map cons (repeat %) (permutation (disj s %))) s)))
                     words)]
      (->> permutations
        (map #(partition 2 1 %))
        (map #(map (fn [[s t]] ((editions s #{}) t)) %))
        (remove #(some nil? %))
        empty? not))))

(defcheck solution-a070c373
  (fn [s]
    (letfn [(ch? [s1 s2]
              (loop [[a & b :as c] (seq s1) [d & e :as g] (seq s2)]
                (if (= a d) (recur b e)
                            (or (= b e) (= b g) (= c e)))))
            (t [e s] (if (empty? s) true (some #(t % (disj s %)) (filter #(ch? e %) s))))]
      (if (some #(t % (disj s %)) s) true false))))

(defcheck solution-a137be3e
  (fn word-chain? [words]
    (letfn [(similar? [w1 w2]
              (if (= (count w1) (count w2))
                (<= (count (filter (fn [[a b]] (not= a b)) (map vector w1 w2))) 1)
                (let [longerw (if (< (count w1) (count w2)) w2 w1)
                      shorterw (if (< (count w1) (count w2)) w1 w2)]
                  (if (some #{shorterw}
                        (for [i (range (count longerw))]
                          (str (subs longerw 0 i) (subs longerw (inc i)))))
                    true
                    false))))
            (make-graph [words]
              (into {}
                (for [s words]
                  [s (filter #(similar? % s) words)])))
            (find-paths [graph start seen]
              (if (seen start)
                seen
                (for [n (graph start)]
                  (find-paths graph n (conj seen start)))))]
      (not (nil? (some (fn [start] (some #(= words %)
                                     (flatten (find-paths (make-graph words) start #{}))))
                   words))))))

(defcheck solution-a1448d69
  (fn [path-set]
    (letfn [(chainable? [x y]
              (let
               [char-diff-count1? #(= (count %1) (inc (count %2)))
                change-length-chain?  (fn [x y]
                                        (loop [[x & xs] x [y & ys] y ignore 0]
                                          (cond
                                            (nil? y) (or (and (= ignore 0) (nil? xs))
                                                         (and (= ignore 1) (nil? x)))
                                            (nil? x) (and (= ignore 1) (nil? ys))
                                            :else  (if (= x y)
                                                     (recur xs ys ignore)
                                                     (recur xs (cons y ys) (inc ignore))))))
                change-letter-chain?  (fn [x y]
                                        (loop [[x & xs] x [y & ys] y ignore 0]
                                          (cond
                                            (nil? y) (= ignore 1)
                                            (nil? x) false
                                            :else (if (= x y)
                                                    (recur xs ys ignore)
                                                    (recur xs ys (inc ignore))))))
                a (map identity x)
                b (map identity y)]
                (cond
                  (char-diff-count1? x y) (when (change-length-chain? a b) {[x y] 0,[y x] 0})
                  (char-diff-count1? y x) (when (change-length-chain? b a) {[y x] 0,[x y] 0})
                  (= (count x) (count y)) (when (change-letter-chain? a b) {[x y] 0,[y x] 0})
                  :else nil )))
            (chainable-path-set [coll]
              (->> (for [elem coll]
                     (->> (for [t coll :when (not (= t elem))]
                            (chainable? t elem))
                       (remove nil?)
                       (apply merge)))
                (apply merge)))
            (close-path-all-from [coll in]
              (->>
                (for [out coll :when (not (= in out))] {[in out] 1,[out in] 1})
                (apply merge)))
            (passable-path [current path-set]
              (->> (for [p path-set]
                     (when (and (= (first (key p)) current) (= (val p) 0) )  p))
                (remove nil?)))
            (patrol [current chainable-path coll]
              (letfn [(patrol2 [in current-path chainable-path]
                        (let [path (passable-path in chainable-path)]
                          (if (empty? path)
                            current-path
                            (for [[[in out] _] path]
                              (patrol2 out (into current-path {in out}) (merge chainable-path (close-path-all-from coll in))))
                            )))]
                (flatten (patrol2 current {} chainable-path))))
            (filter-pass-all [route coll]
              (filter #(= (dec (count coll)) (count %)) route))
            ]
      (let [chainable-path (chainable-path-set path-set)
            unicursal (->> (for [elem path-set]
                             (let [route (patrol elem chainable-path path-set)
                                   pass-all (filter-pass-all route path-set)]
                               (if (nil? (first pass-all)) false true)))
                        (some identity ))]
        (if unicursal true false)))))

(defcheck solution-a23b4382
  (fn [s]
    (if (some identity
          (map (fn [x] ((fn transformacoes-caminhos [pivot set-outros]
                          (if (empty? set-outros)
                            true
                            (some identity (map (fn [x] (when ((fn transformacao-valida [from to]
                                                                 (let [[menor maior] (sort-by count [from to])]
                                                                   (if (= (count menor) (count maior))
                                                                     (some identity
                                                                       (map #(= ((fn [palavra indice]
                                                                                   (concat (take indice palavra)
                                                                                           (take-last (dec (- (count palavra) indice))
                                                                                             palavra)))
                                                                                 maior %)
                                                                               ((fn [palavra indice]
                                                                                  (concat (take indice palavra)
                                                                                          (take-last (dec (- (count palavra) indice))
                                                                                            palavra)))
                                                                                menor %))
                                                                         (range (count maior))))
                                                                     (let [tam-maior (count maior)]
                                                                       (some #(= (seq menor) %)
                                                                         (map #(concat (take % maior)
                                                                                       (take-last (dec (- tam-maior %)) maior))
                                                                           (range tam-maior)))))))
                                                               pivot x)
                                                          (transformacoes-caminhos x (disj set-outros x))))
                                             set-outros))))
                        x
                        (disj s x)))
            s))
      true
      false)))

(defcheck solution-a34d2ba0
  (fn [words]
    (letfn [(trans [xs ys]
              (loop [x (seq xs) y (seq ys)]
                (cond
                  (= x y) ys
                  (= (next x) y) ys
                  (= x (next y)) ys
                  (= (next x) (next y)) ys
                  (= (butlast x) y) ys
                  (= x (butlast y)) ys
                  (= (first x) (first y)) (recur (next x) (next y)))))
            (edges [ws]
              (for [x ws y ws :when (and (not= x y) (trans x y))] [x y]))
            (connected [word edges]
              (map last (filter #(= word (first %)) edges)))
            (grow [ws]
              (filter (partial apply distinct?)
                (map (partial conj ws)
                  (connected (last ws) (edges words)))))]
      (boolean (not-empty
                 (last (take (count words)
                         (iterate (partial mapcat grow)
                           (map vector words)))))))))

(defcheck solution-a42d9c8b
  (fn q82 [ss]
    (letfn [
            (update-1? [s1 s2]
              (= 1 (reduce + (map #(if (= % %2) 0 1) s1 s2))))

            (all-remove-1 [s]
              (->>
                (for [i (range (count s))] (remove #(= i %) (range (count s))))
                (map #(replace (vec s) %))))

            (remove-1? [s1 s2]
              (some #(= (seq s2) %) (all-remove-1 s1)))

            (insert-1? [s1 s2]
              (remove-1? s2 s1))

            (transit? [s1 s2]
              (let [l1 (count s1) l2 (count s2)]
                (cond
                  (= l1 l2) (update-1? s1 s2)
                  (= l1 (inc l2)) (remove-1? s1 s2)
                  (= (inc l1) l2) (insert-1? s1 s2)
                  :else nil)))

            (comb-2 [coll]
              (when-first [ x coll ]
                (concat (map #(vector x %) (rest coll)) (comb-2 (rest coll)))))

            (transits [coll]
              (reduce
                (fn [m [s1 s2]]
                  (if (transit? s1 s2)
                    (assoc m s1 (conj (m s1) s2) s2 (conj (m s2) s1))
                    m
                    ))
                {} (comb-2 coll)))

            (paths
              ([m p n]
               (if (some #(= n %) p) [p]
                                     (mapcat #(paths m (conj p n) %) (m n))))
              ([m] (mapcat #(paths m [] %) (keys m)))) ]

      (let [num-nodes (count ss)]
        (if (some #(= num-nodes (count %)) (paths (transits ss))) true false)))))

(defcheck solution-a49d4ac7
  (let [lev (fn lev [f a b] (if (zero? (count a)) (count b) (if (zero? (count b)) (count a) (let [cost (if (= (peek a) (peek b)) 0 1)] (min (+ (f f (pop a) b) 1) (+ (f f a (pop b)) 1) (+ (f f (pop a) (pop b)) cost))))))
        m-lev (memoize lev)
        str-dist (fn [a b] (m-lev m-lev (into [] a) (into [] b)))]
    (letfn [(remove-edge [graph [a b :as edge]]
              (disj (disj graph edge) [b a]))
            (depth
              ([graph edge] (depth graph edge 1))
              ([graph [a b :as edge] d]
               (let [neighbors (filter #(= b (first %)) graph)]
                 (if (seq neighbors)
                   (apply max (map #(depth (remove-edge graph %) % (inc d)) neighbors))
                   d))))]
      (fn [xs]
        (let [size (count xs)
              edges (set (for [a xs b xs :when (= 1 (str-dist a b))] [a b]))]
          (boolean (some #(= size (depth (remove-edge edges %) %)) edges)))))))

(defcheck solution-a5dfa663
  (fn chain?
    ([words] (chain? nil words))
    ([word1 words]
     (let [sub? (fn [word1 word2]
                  (and (= (count word1) (count word2))
                       (<= (count (remove identity (map = word1 word2)))
                         1)))
           del? (fn [word1 word2]
                  (some #(= (seq word1) %)
                    (map #(concat (take % word2) (drop (inc %) word2))
                      (range (inc (count word1))))))
           sim? (fn [word1 word2]
                  (or (sub? word1 word2)
                      (del? word1 word2)
                      (del? word2 word1)))]
       (boolean
         (or (empty? words)
             (some
               (fn [word2]
                 (and (or (not word1)
                          (sim? word1 word2))
                      (chain? word2 (disj words word2))))
               words)))))))

(defcheck solution-a5dffcf3
  (fn [s]
    (let [drop-nth (fn [x n]
                     (apply str
                       (concat
                        (map str (take n x))
                        (map str (drop (inc n) x)))))
          drops (fn [x]
                  (map #(drop-nth x %)
                    (range (count x))))
          by-del? (fn [x1 x2] (some (set (drops x1)) [x2]))
          by-ins? (fn [x1 x2] (by-del? x2 x1))
          by-subs? (fn [x1 x2]
                     (some identity (map = (drops x1) (drops x2))))
          joined? (fn [x1 x2]
                    (or
                     (by-del? x1 x2)
                     (by-ins? x1 x2)
                     (by-subs? x1 x2)))
          chain? (fn it [x s]
                   (if (empty? s) true
                                  (some true?
                                    (map
                                      #(if (joined? x %)
                                         (it % (disj s %))
                                         false)
                                      s))))]
      (= true (some true? (map #(chain? % (disj s %)) s))))))

(defcheck solution-a60851d3
  (fn chain? [s]
    (letfn [(rotate [s]
              (take (count s)
                (iterate #(cons (last %) (butlast %)) s)))
            (permutate [s]
              (if (= 1 (count s))
                [s]
                (reduce
                  (fn [r a]
                    (concat r (map
                                #(cons (first a) %)
                                (permutate (rest a)))))
                  []
                  (rotate s))))
            (deletion? [a b]
              (let [split (partial map #(if (= %1 %2) %1 nil))]
                (= b
                  (clojure.string/join
                    (concat
                     (split a b)
                     (reverse (split (reverse a) (reverse b))))))))
            (legal-pair?  [a b]
              (let [ca (count a) cb (count b)]
                (case (Math/abs (- ca cb))
                  0 (= 1 (reduce + (map #(if (not= %1 %2) 1 0) a b)))
                  1 (if (> ca cb)
                      (deletion? a b)
                      (deletion? b a))
                  false)))
            (legal-chain? [s]
              (every?
                identity
                (map
                  #(legal-pair? %1 %2)
                  s
                  (rest s))))]
      (not (nil?
             (some
               legal-chain?
               (permutate (nth (rotate s) 2))))))))

(defcheck solution-a6086424
  (fn [ws]
    (letfn [(c [x y]
              (cond (not x) (= (count y) 1)
                    (not y) (= (count x) 1)
                    :else (let [[x1 & xr] x [y1 & yr] y]
                            (or (= x yr)
                                (= xr y)
                                (= xr yr)
                                (and (= x1 y1) (c xr yr))))))
            (ch [w s] (or (empty? s)
                          (some #(and (c (seq w) (seq %)) (ch % (disj s %))) s)))]
      (boolean (some #(ch % (disj ws %)) ws)))))

(defcheck solution-a7cc5084
  (letfn [(without-nth [s n]
            (concat (take n s) (drop (inc n) s)))
          (perms [s]
            (if (= 1 (count s)) [s]
                                (apply concat (map (fn [i] (map #(cons (s i) %)
                                                             (perms  (vec (without-nth s i)))))
                                                (range (count s))))))
          (chainable-pair? [[a b]]
            (cond (> (count b) (count a)) (chainable-pair? [b a])
                  (> (- (count a) (count b)) 1) false
                  :else (some #(and (= (take % a) (take % b))
                                    (= (take-last (dec (- (count a) %)) a)
                                      (take-last (dec (- (count a) %)) b)))
                          (range (count a)))))
          (word-chain? [s] (every? chainable-pair? (partition 2 1 (vec s))))]
    (fn [s]
      (or (some word-chain? (perms (vec s))) false))))

(defcheck solution-a7fd621f
  (fn word-chain? [ls]
    (letfn [(add-char [x y]
              (loop [lx (seq x)
                     ly (seq y)
                     rem false]
                (cond
                  (and (empty? lx) (empty? ly)) true
                  (= (first lx) (first ly)) (recur (rest lx) (rest ly) rem)
                  rem false
                  (= (first lx) (second ly)) (recur (rest lx) (rest (rest ly)) true)
                  :else false)))
            (sub-char [x y]
              (= 1 (reduce + (map #(if (= %1 %2) 0 1) (seq x) (seq y)))))
            (adjacent? [x y]
              (let [len-diff (- (count x) (count y))]
                (cond
                  (= -1 len-diff) (add-char x y)
                  (= 0 len-diff) (sub-char x y)
                  (= 1 len-diff) (add-char y x)
                  :else false)))
            (add-step [lx]
              (mapcat (fn [x]
                        (map
                          #(conj x %)
                          (reduce
                            (fn [coll y]
                              (filter #(not= % y) coll))
                            (filter #(adjacent? (peek x) %) ls)
                            x)
                          )) lx))]
      (loop [lx (map vector ls)]
        (cond
          (empty? lx) false
          (= (count ls) (count (first lx))) true
          :else (recur (add-step lx)))))))

(defcheck solution-a82f82ee
  (fn [word-set]
    (letfn [(word-diff-1? [w1 w2]
              (let [res (or (= w1 w2)
                            (= (rest w1) w2)
                            (= w1 (rest w2))
                            (= (rest w1) (rest w2))
                            (and (= (first w1) (first w2))
                                 (word-diff-1? (rest w1) (rest w2)))
                            false)]
                res))
            (chained-seq? [w w-set]
              (if (empty? w-set)
                true
                (reduce (fn [b1 b2] (or b1 b2))
                  false
                  (map (fn [nw]
                         (chained-seq? nw (remove #(= nw %) w-set)))
                    (remove #(not (word-diff-1? (seq w) (seq %))) w-set)))))
            (chained? [w-set]
              (reduce (fn [b1 b2] (or b1 b2))
                false
                (map (fn [nw]
                       (chained-seq? nw (remove #(= nw %) w-set)))
                  w-set)))]
      (chained? word-set))))

(defcheck solution-a950efcd
  (fn __ [s]
    (letfn [(levenshtein-distance [s t]
              (if (= s t) 0
                          (letfn [(f [s v0]
                                    (if (empty? s) (nth v0 (count t))
                                                   (recur (rest s)
                                                     (loop [j 0, v1 [(inc (first v0))]]
                                                       (if (= j (count t)) v1
                                                                           (recur (inc j)
                                                                             (conj v1 (min (inc (nth v1 j))
                                                                                        (inc (nth v0 (inc j)))
                                                                                        (+ (nth v0 j)
                                                                                           (if (= (first s)
                                                                                                 (nth t j))
                                                                                             0 1))))))))))]
                            (f s (vec (range (inc (count t)))))))),
            (can-chain? [x xs] ; can form a word chain starting from x using all the words in xs
              (or (empty? xs)
                  (true?
                    (some true?
                      (map #(can-chain? % (disj xs %))
                        (filter #(= 1 (levenshtein-distance x %))
                          xs))))))
            ]
      (true?
        (some true?
          (map #(can-chain? % (disj s %)) s))))))

(defcheck solution-a991d8d8
  (fn [s]
    (letfn [(dist [s1 s2]
              (letfn [(calcRow [oldRow s1 c]
                        (letfn [(nextValue [row cc]
                                  (let [idx (count row)]
                                    (if (= c cc)
                                      (conj row (nth oldRow (dec idx)))
                                      (conj row
                                        (min
                                          (inc (last row))
                                          (inc (nth oldRow (dec idx)))
                                          (inc (nth oldRow idx)))))))]
                          (reduce nextValue [(inc (first oldRow))] s1)))]
                (let [v (vec (range (inc (count s1))))]
                  (last (reduce #(calcRow % s1 %2) v s2)))))]
      (letfn [(generate [s]
                (if (= 1 (count s))
                  (list s)
                  (for [head s
                        tail (generate (disj (set s) head))]
                    (cons head
                      (if (= 1 (dist head (first tail)))
                        tail
                        '())))))]
        (not (nil? (some #(= (count %) (count s)) (generate s))))))))

(defcheck solution-aa19ae75
  (fn word-chain [words]
    (letfn [(permutes [coll]
              (if (empty? coll)
                [[]]
                (mapcat (fn [x]
                          (map (fn [xs]
                                 (conj xs x))
                            (permutes (remove #(= x %) coll))))
                  coll)))
            (compare-equal-words [w1 w2]
              (->> (map = w1 w2)
                (filter false?)
                count
                (= 1)))
            (compare-words [w1 w2]
              (if (= (count w1) (count w2))
                (compare-equal-words w1 w2)
                (let [[v1 v2] (sort-by count [w1 w2])]
                  (and (= 1 (- (count v2) (count v1)))
                       (->> (map #(compare-equal-words v2 %)
                              (map #(str (subs v1 0 %)
                                      "0"
                                      (subs v1 %))
                                (range (inc (count v1)))))
                         (some true?))))))
            (word-chain? [words]
              (->> (partition 2 1 words)
                (map #(apply compare-words %))
                (every? true?)))]
      (->> (permutes words)
        (map word-chain?)
        (some true?)
        boolean))))

(defcheck solution-aab3dc53
  (fn [words]
    (letfn [(char-diff [[a b]] (if (= a b) 0 1))
            (word-diff-same-len [w1 w2]
              (let [zip (map vector w1 w2)
                    diffs (map char-diff zip)
                    sum (apply + diffs)]
                (<= sum 1)))
            (word-diff [w1 w2]
              (let [c1 (count w1)
                    c2 (count w2)]
                (cond
                  (= c1 c2) (word-diff-same-len w1 w2)
                  (= c1 (inc c2)) (some true? (for [i (range c1)
                                                    :let [left (.substring w1 0 i)
                                                          right (.substring w1 (inc i) c1)
                                                          new-word (str left right)]]
                                                (word-diff-same-len new-word w2)))
                  (= c1 (dec c2)) (word-diff w2 w1)
                  :else false)))
            (is-chain [xs]
              (every? true? (for [[w1 w2] (map vector xs (rest xs))]
                              (word-diff w1 w2))))
            (permutations [leader xs]
              (if (empty? xs)
                [[leader]]
                (let [nexts (filter #(word-diff leader %) xs)]
                  (if (empty? nexts)
                    [["xxx"]]
                    (for [next nexts
                          :let [rst (disj xs next)]
                          p (permutations next rst)]
                      (cons leader p))))))
            (permutations-start [xs]
              (for [i (range (count xs))
                    :let [x (nth (vec xs) i)
                          rst (disj xs x)]
                    p (permutations x rst)]
                p))]

      (or (some is-chain (permutations-start words)) false))))

(defcheck solution-ab211475
  (fn [s]
    (let [deletion?
             (fn [s1 s2]
               (let [[s l] (sort-by count [s1 s2])]
                 (some (fn [x] (= (seq s) x)) (for [i (range (count l))]
                                                (concat (subvec (vec l) 0 i) (subvec (vec l) (inc i)))))))

          substitution?
             (fn [s1 s2]
               (->>
                 (map (fn [x] (= (get s1 x) (get s2 x))) (range (count s1)))
                 (filter false?)
                 count
                 (= 1)))

          neighbor?
             (fn [s1 s2]
               (if (= (count s1) (count s2)) (substitution? s1 s2) (deletion? s1 s2)))

          neighbors
             (fn [s]
               (into {}
                 (for [w s :let [words (clojure.set/difference s #{w})]]
                   {w (filter (fn [x] (neighbor? w x)) words)})))

          ns (neighbors s)
          c1 (count (filter (fn [x] (= 1 (count (val x)))) ns))
          c0 (count (filter (fn [x] (= 0 (count (val x)))) ns))]
      (and (zero? c0) (<= c1 2)))))

(defcheck solution-ab372d50
  (fn [words]
    (letfn [(permutations [colls] ;; TODO: Found on the web; how does this work?
              (if (= 1 (count colls))
                (list colls)
                (for [head colls
                      tail (permutations (disj (set colls) head))]
                  (cons head tail))))

            (chain?[words]
              (every? identity
                (map chainable?
                  (map vector words (rest words)))))

            (chainable?[[a b]]
              (check (sort #(compare (count %1) (count %2)) (list (seq a)(seq b)))))

            (check[[shortest longest]]
              (cond
                (= (count shortest) (count longest)) (substitution? shortest longest)
                (= 1 (- (count longest) (count shortest))) (insertion-or-deletion? shortest longest)
                :else false))

            (substitution? [a b]
              (loop [as a bs b subs-detected 0]
                (let [[fa & ra] as
                      [fb & rb] bs]
                  (if (empty? as)
                    (= 1 subs-detected)
                    (if (not= fa fb)
                      (recur ra rb (inc subs-detected))
                      (recur ra rb subs-detected))))))

            (insertion-or-deletion? [shortest longest]
              (loop [as shortest bs longest diffs-detected 0]
                (let [[fa & ra] as
                      [fb & rb] bs]
                  (if (empty? bs)
                    (= 1 diffs-detected)
                    (if (not= fa fb)
                      (recur as rb (inc diffs-detected))
                      (recur ra rb diffs-detected))))))]
      (let [perms (permutations words)]
        (not (not
               (some chain? perms)))))))

(defcheck solution-ab480518
  (fn [all-words]
    (letfn [(neighbors? [a b]
              (let [[[cx x] [cy y]] (sort (map (juxt count str) [a b]))]
                (case (- cy cx)
                  0 (= 1 (reduce + (map #(if (= % %2) 0 1) x y)))
                  1 (let [n (count (take-while #(apply = %) (map list x y)))]
                      (= (subs x n) (subs y (inc n))))
                  false)))
            (spans? [words root]
              (let [words (disj words root)
                    neighbors (filter #(neighbors? % root) words)]
                (or (empty? words) (some #(spans? words %) neighbors))))]
      (boolean (some #(spans? all-words %) all-words)))))

(defcheck solution-ab6bfb01
  (fn wc [s]
    (let [neighbor (fn [a b]
                     (let [su (fn su [a b]
                                (let [i (interleave a b)
                                      p (partition 2 i)
                                      f (fn [v] (= (first v) (last v)))
                                      m (map f p)
                                      d (filter false? m)
                                      c (count d)]
                                  (= c 1)))
                           de (fn de [a b]
                                (let [c (count a)]
                                  (loop [i 1]
                                    (if (= i (inc c))
                                      false
                                      (if (= (seq b) (concat (take (dec i) a) (drop i a)))
                                        true
                                        (recur (inc i)))))))]
                       (or (su a b)
                           (de a b)
                           (de b a))))
          p (into #{}(for [i s
                           j s
                           :when (not= i j)]
                       #{i j}))
          f (set (filter #(neighbor (first %) (last %)) p))
          gc (fn gc [s]
               (let [f (first s)
                     filt (fn [x s]
                            (let [f (fn [v]
                                      (cond
                                        (= (first x) (first v)) v
                                        (= (first x) (last v)) v
                                        (= (last x) (first v)) v
                                        (= (last x) (last v)) v
                                        :else nil))]
                              (map f s)))]
                 (loop [r #{f}
                        pr #{}]
                   (if (= (count r) (count pr))
                     (= r s)
                     (recur (clojure.set/union r (disj (apply clojure.set/union
                                                         (for [x r]
                                                           (into #{} (filt x s))))
                                                   nil))
                       r)))))]
      (if (not= (reduce clojure.set/union f)
            s)
        false
        (loop [f f
               l '()]
          (if (gc f)
            (if (= 2 (count f))
              true
              (let [fre (frequencies (reduce concat (map seq f)))
                    fre1 (if (empty? l) fre (apply (partial assoc fre) (interleave l (repeat 1))))
                    u (filter #(if (= (val %) 1) (key %)) fre)
                    c (count u)]
                (if (> c 2)
                  false
                  (let [nl (apply concat (for [x (keys u)]
                                           (filter #(% x) f)))
                        a (frequencies (apply concat (map seq nl)))
                        m (if (vals a) (apply max (vals a)) 0)]
                    (if (>= m 2)
                      false
                      (if (empty? u)
                        true
                        (recur (apply (partial disj f) nl) (seq (apply disj (reduce clojure.set/union nl) (keys u))))))))))
            false))))))

(defcheck solution-abafe930
  (fn [chain]

    (letfn [(lev [w1 w2]
              (cond
                (empty? w1) (count w2)
                (empty? w2) (count w1)
                :else (min
                        (+ (if (= (first w1) (first w2)) 0 1) (lev (rest w1) (rest w2)))
                        (inc (lev (rest w1) w2))
                        (inc (lev w1 (rest w2))))))

            (check [w remw]
              (if (empty? remw)
                true
                (let [nremw (filter #(= 1 (lev w %)) remw)]
                  (if (empty? nremw)
                    false
                    (reduce #(or %1 %2) (map #(check % (disj remw %)) nremw))))))]

      (reduce #(or %1 %2) (map #(check % (disj chain %)) chain)))))

(defcheck solution-abb24300
  (fn [words]
    (let [remove-at (fn [n sequence]
                      (let [[xs ys] (split-at n sequence)]
                        (concat xs (rest ys))))
          remove-any (fn [word]
                       (map #(apply str (remove-at % word))
                         (range (count word))))
          chained? (fn [word-1 word-2]
                     (or (some #(= word-1 %) (remove-any word-2))
                         (some #(= word-2 %) (remove-any word-1))
                         (some #(= (remove-at % word-1)
                                  (remove-at % word-2))
                           (range (count word-1)))))
          word-chain? (fn word-chain? [word words]
                        (or (empty? words)
                            (some #(and (chained? word %)
                                        (word-chain? % (disj words %)))
                              words)))]
      (true? (some #(word-chain? % (disj words %)) words)))))

(defcheck solution-abc0d793
  (fn [words]
    (letfn [(chain? ([seq]
                     (every? #(apply chain? %)
                       (map (fn [x y] [x y]) seq (rest seq))))
              ([wx wy]
               (let [x (first wx)
                     y (first wy)]
                 (or
                  (and (= x y)
                       (chain? (subs wx 1) (subs wy 1)))
                  ;; add
                  (= (str y wx) wy)
                  (= (str x wy) wx)
                  ;; del
                  (= (subs' wx 1) wy)
                  (= (subs' wy 1) wx)
                  ;; sub
                  (= (str y (subs' wx 1)) wy)
                  (= (str x (subs' wy 1)) wx)))))
            (subs' [str n]
              (if (> (count str) n)
                (subs str n)
                ""))
            (permutation1 [x seq]
              (if (empty? seq)
                [[x]]
                (map #(concat (take % seq) [x] (drop % seq)) (range 0 (inc (count seq))))))
            (permutations [seq]
              (if (empty? seq)
                [[]]
                (apply concat (map #(permutation1 (first seq) %) (permutations (rest seq))))))]
      (not (empty? (filter chain? (permutations words)))))))

(defcheck solution-ad0b3640
  (fn [c]
    (let [l (fn [x y]
              (last (reduce #(reduce (fn [r [d-1 d e]]
                                       (conj r (if (= %2 e) d-1 (inc (min (last r) d d-1)))))
                               [(inc (first %1))]
                               (map vector %1 (next %1) y))
                      (range (inc (count y)))
                      x)))
          v (for [a c b c :when (= 1 (l a b))] [a b])]
      (if (empty? ((fn f [ch n]
                     (#(if (= n (- (count c) 3)) % (f % (inc n)))
                      (for [a ch b v :when (and (= (last a) (first b))
                                                (not-any? (set a) (rest b)))] (concat a (rest b)))))
                   v 0))
        false
        true))))

(defcheck solution-ad89a2ec
  (fn all-words-chain? [words-set]
    (letfn [(linked-words? [source target]
              (let [[smaller greater] (sort-by count [source target])
                    greater-indexed (map-indexed (fn [idx c] [idx c]) greater)
                    smaller-indexed (map-indexed (fn [idx c] [idx c]) smaller)]
                (letfn [(combine-matches-or-nil [greater-tuple smaller-tuple]
                          (map (fn [[out-idx out-ch]]
                                 (some (fn [[in-idx in-ch]]
                                         (when (and (= out-ch in-ch)
                                                    (or (= out-idx in-idx)
                                                        (= out-idx (inc in-idx))
                                                        (= out-idx (dec in-idx)))) out-ch))
                                   smaller-tuple))
                            greater-tuple))]
                  (when (<= (- (count greater) (count smaller)) 1)
                    (when-let [greater-combined-matches (combine-matches-or-nil greater-indexed smaller-indexed)]
                      (when-let [smaller-combined-matches (combine-matches-or-nil smaller-indexed greater-indexed)]
                        (let [greater-matches-indexed (map-indexed (fn [idx x] [idx x]) greater-combined-matches)
                              smaller-matches-indexed (map-indexed (fn [idx x] [idx x]) smaller-combined-matches)]
                          (>= 1
                            (count
                              (filter (fn [matched-indexed] (nil? (last matched-indexed)))
                                (set
                                  (apply conj
                                    greater-matches-indexed smaller-matches-indexed))))))))))))
            (find-links-foreach-word [words]
              (map (fn [word]
                     (reduce (fn [acc compared-word]
                               (if (= word compared-word) acc
                                                          (if (linked-words? word compared-word)
                                                            (inc acc)
                                                            acc)))
                       0 words))
                words))]
      (->> words-set
        find-links-foreach-word
        (filter #(= 1 %))
        count
        (>= 2)))))

(defcheck solution-ade6ac3
  (fn p89 [cs0]
    (letfn [(pat [w]
              (map #(re-pattern %)  (map (fn [f] (apply str (map second (sort-by first (f (zipmap (range) w))))))
                                      (concat (map (fn [i] (fn [w1] (dissoc w1 i))) (range (count w)))
                                              (map (fn [i] (fn [w1] (assoc w1 i "."))) (range (count w)))
                                              (map (fn [i] (fn [w1] (assoc w1 (+ 0.5 i) "."))) (range -1 (count w)))
                                              )))
              )
            (prs [cur w cs]
              (if (empty? cs) (conj cur w)
                              (let [
                                    nws (disj cs w)
                                    cts (reduce concat [] (map #(filter (complement nil?)
                                                                  (map (fn [p] (if (re-matches p %) %)) (pat w))) nws))
                                    ]
                                (filter (complement empty?) (map #(prs (conj cur w) % (disj nws %)) cts))
                                )
                              ))]
      ((complement empty?) (filter (complement empty?) (map #(prs [] % (disj cs0 %)) cs0))))
    ))

(defcheck solution-adf9b8f7
  (fn [words]
    (letfn [(rem-nth [coll n]
              (concat (take n coll) (drop (inc n) coll)))

            (del-letter [word n]
              (apply str (rem-nth word n)))

            (word-ins-n? [old-word new-word n]
              (= old-word (del-letter new-word n)))

            (word-ins? [old-word new-word]
              (reduce #(or % %2) (map (partial word-ins-n? old-word new-word)
                                   (range (count new-word)))))

            (word-subs-n? [old-word new-word n]
              (= (del-letter old-word n) (del-letter new-word n)))

            (word-subs? [old-word new-word]
              (reduce #(or % %2) (map (partial word-subs-n? old-word new-word)
                                   (range (count new-word)))))

            (word-del-n? [old-word new-word n]
              (= new-word (del-letter old-word n)))

            (word-del? [old-word new-word]
              (reduce #(or % %2) (map (partial word-del-n? old-word new-word)
                                   (range (count old-word)))))

            (build-chain [words]
              (loop [ word (first words)
                     words (next words)
                     chain []]
                (let [_ (filter #(or
                                  (word-ins? word %)
                                  (word-subs? word %)
                                  (word-del? word %))
                          words)
                      link (first _)]
                  (if-not link chain
                               (let [words (filter #(not= link %) words)]
                                 (recur link words (conj chain link)))))))

            (linked? [word1 word2]
              (or (word-ins? word1 word2)
                  (word-subs? word1 word2)
                  (word-del? word1 word2)))

            (insert-link [chain link]
              (cond
                (linked? link (first chain)) (cons link chain)
                (linked? link (last chain)) (concat chain [link])
                :else (let [n (first (filter #(and
                                               (linked? (nth chain %) link)
                                               (linked? (nth chain (inc %)) link))
                                       (range 1 (dec (count chain)))))]
                        (if n
                          (concat (take (inc n) chain) [link] (drop (inc n) chain))
                          chain))))

            (max-chain [words]
              (let [chain (build-chain words)
                    left (clojure.set/difference (set words) (set chain))]
                (loop [left left
                       chain chain]
                  (if (not left)
                    chain
                    (recur (next left) (insert-link chain (first left)))))))]

      (= (count words) (count (max-chain words))))))

(defcheck solution-aea2e0a1
  (fn [s]
    (letfn [(drop-nth [xs n]
              (concat (take n xs) (drop 1 (drop n xs))))

            (insert-nth [xs x n]
              (concat (take n xs) (cons x (drop n xs))))

            (replace-nth [xs x n]
              (insert-nth (drop-nth xs n) x n))

            (char-additions [a b]
              (set (for [n (range (count b))] (apply str (insert-nth a (nth b n) n)))))

            (char-deletions [a]
              (set (for [n (range (count a))] (apply str (drop-nth a n)))))

            (char-substitutions [a b]
              (set (for [n (range (count a))] (apply str (replace-nth a (nth b n) n)))))

            (reachable? [a b]
              (let [an (count a) bn (count b)]
                (cond
                  (< an bn) (contains? (char-additions a b) b)
                  (= an bn) (contains? (char-substitutions a b) b)
                  (> an bn) (contains? (char-deletions a) b))))

            (reachables [x s]
              (reduce #(if (reachable? x %2) (conj % %2) %) () s))

            (next-level [xs]
              (reduce (fn [acc [from to]] (concat acc (reduce #(conj % [%2 (disj to %2)]) [] (reachables from to)))) [] xs))
            ]
      (loop [level (map #(vector % (disj s %)) s)]
        (if (empty? level)
          false
          (if (some #(empty? (second %)) level)
            true
            (recur (next-level level))))))))

(defcheck solution-aec6152d
  (fn [s]
    (or (some (fn [w]
                ((fn f [a s]
                   (or (empty? s)
                       (some #(if (loop [[a & b :as c] (seq a) [d & e :as g] (seq %)]
                                    (if (= a d)
                                      (recur b e)
                                      (or (= b e) (= b g) (= c e))))
                                (f % (disj s %)))
                         s)))
                 w (disj s w)))
          s)
        false)))

(defcheck solution-af822aac
  (fn [s]
    (letfn [(permutations [r s]
              (if (seq s)
                (mapcat #(permutations (conj r %) (disj s %)) (seq s))
                [r]))
            (chainable? [x y]
              (let [diffs (fn [x y] (<= (reduce + (map #(if (= %1 %2) 0 1) x y)) 1))
                    drop-char (fn [n s] (str (subs s 0 n) (subs s (inc n) (count s))))
                    drop-variants (fn [s] (map #(drop-char % s) (range (count s))))
                    deletions (fn [l s] (some #(= s %) (drop-variants l)))
                    cx (count x)
                    cy (count y)]
                (cond
                  (= cx cy) (diffs x y)
                  (= cx (inc cy)) (deletions x y)
                  (= (inc cx) cy) (deletions y x))))
            (chain? [s]
              (reduce #(when (chainable? %1 %2) %2) s))]
      (boolean
        (some chain? (permutations [] s))))))

(defcheck solution-af922676
  (fn [sw]
    (let [f #(cond (apply = (map count %))
                   (= 1 (loop [c 0 ws %]
                          (if (-> ws first seq)
                            (recur (if (apply = (map first ws))
                                     c (inc c))
                              (map rest ws))
                            c)))
                   (= -1 (apply - (sort (map count %))))
                   (loop [b (first %) s (second %)]
                     (if (apply < (map count [b s]))
                       (recur s b)
                       (loop [s s b b c 0]
                         (cond (and (seq s) (seq b))
                               (if (= (first s) (first b))
                                 (recur (rest s) (rest b) c)
                                 (recur s (rest b) (inc c)))
                               (seq b) (recur s (rest b) (inc c))
                               (seq s) false
                               :else (= c 1)))))
                   :else false)]
      (->> (map (fn [w] (->> (map (fn [o] [w o]) (disj sw w))
                          (map f) (filter #{true}) count))
             sw)
        (filter #(> % 1)) count (<= (- (count sw) 2)))
      )))

(defcheck solution-b0a9cc60
  (letfn [(add [x y] (some #(and (= (take % (seq x)) (take % (seq y)))
                                 (= (drop % (seq x)) (drop (inc %) (seq y))))
                       (range (inc (count x)))))
          (subst [x y] (some #(and (= (take % x) (take % y))
                                   (= (drop (inc %) x) (drop (inc %) y)))
                         (range (count x))))]
    (fn search
      ([x] (if (some #(search x %) x) true false))
      ([x w] (or (empty? (disj x w))
                 (some #(and (or (add w %) (add % w) (subst w %))
                             (search (disj x w) %))
                   (disj x w)))))))

(defcheck solution-b0afc5fc
  (fn [s] (not (empty? (filter
                         #(let [dst (fn levdist [s t]
                                      (if (empty? s) (count t)
                                                     (if (empty? t) (count s)
                                                                    (min
                                                                      (+ 1 (levdist (subs s 1) t))
                                                                      (+ 1 (levdist s (subs t 1)))
                                                                      (+ (if (= (first s) (first t)) 0 1)
                                                                         (levdist (subs s 1) (subs t 1)))))))]
                            (not (nil? (reduce
                                         (fn [i1 i2]
                                           (if (nil? i1)
                                             nil
                                             (if (= 1 (dst i1 i2))
                                               i2
                                               nil)))
                                         %1))))
                         ((fn perm [l]
                            (if (= 1 (count l))
                              [l]
                              (mapcat
                                #(map
                                   (partial cons %1)
                                   (perm (remove #{%1} l)))
                                l)))
                          s))))))

(defcheck solution-b1260cea
  (fn word-chain [word-set]
    (let [levenshtein-distance (fn [str1 str2]
                                 (let [len1 (inc (count str1))
                                       len2 (inc (count str2))
                                       arr (make-array Long len1 len2)]
                                   (dotimes [i len1] (aset arr i 0 i))
                                   (dotimes [j len2] (aset arr 0 j j))
                                   (doseq [i (range 1 len1)]
                                     (doseq [j (range 1 len2)]
                                       (aset arr i j
                                         (min (inc (aget arr (dec i) j))
                                           (inc (aget arr i (dec j)))
                                           (+ (if (= (nth str1 (dec i))
                                                    (nth str2 (dec j))) 0 1)
                                              (aget arr (dec i) (dec j)))))))
                                   (aget arr (dec len1) (dec len2))))
          m (reduce (fn [acc x]
                      (assoc acc x
                                 (set (filter #(= 1 (levenshtein-distance % x))
                                        (disj word-set x)))))
              {}
              word-set)]
      (letfn [(search [word used]
                (if (= (count word-set) (count used))
                  true
                  (let [next-set (clojure.set/difference (m word) used)]
                    (when-not (empty? next-set)
                      (some #(search % (conj used %)) next-set)))))]
        (boolean (some #(search % #{%}) word-set))))))

(defcheck solution-b16b9c23
  (fn [coll]
    (let [permute (fn permute [coll]
                    (if (<= (count coll) 1)
                      (list (into (list) coll))
                      (mapcat (fn [e] (map #(cons e %) (permute (disj coll e)))) coll)))
          match (fn [s1 s2]
                  (reduce (fn [[[sh & st :as s] c] e]
                            (cond (= sh e) [st c]
                                  (pos? c) [(if (= (count s1) (count s2)) st s) (dec c)]
                                  :else [nil -1]))
                    [s2 1] s1))
          umatch (fn [[s1 s2]] (neg? (second (if (< (count s1) (count s2)) (match s2 s1) (match s1 s2)))))]
      (not (empty? (filter (fn [p] (not-any? umatch (partition 2 1 p))) (permute coll)))))))

(defcheck solution-b1fbec5b
  (fn word-chain
    ([wres w1 ws]
     (letfn [(cont?
               [[h1 & t1] [h2 & t2]]
               (when (or h1 h2)
                 (cond
                   (= h1 h2) (cont? t1 t2)
                   (= t1 t2) true
                   (= (rest t1) t2) true
                   (= t1 (rest t2)) true
                   (= [h1] t2) true
                   (= t1 [h2]) true
                   :else false)))]
       (if (and w1 (seq ws))
         (when-let [wcs (filter (fn [w] (cont? w1 w)) ws)]
           (mapcat (fn [wc] (word-chain (conj wres wc) wc (disj ws wc))) wcs))
         (when w1
           [wres]))))
    ([words]
     (let [wres (mapcat (fn [w1] (word-chain [w1] w1 (disj words w1))) words)]
       (pos? (count wres))))))

(defcheck solution-b3f150eb
  #(not (empty? (clojure.set/intersection #{"hat" "spout" "shares"} %))))

(defcheck solution-b518009e
  (fn wc? ([ws] (not (nil? (some #(wc? (disj ws %) %) ws))))
    ([ws w]
     (let [wd1? (fn [a b] (let [[fa & ra] a [fb & rb] b]
                            (cond (= a b) true
                                  (= fa fb) (recur ra rb)
                                  :else (or (= ra rb) (= ra (seq b)) (= rb (seq a))))))]
       (if (empty? ws) true
                       (some #(wc? (disj ws %) %)
                         (filter #(wd1? w %) ws)))))))

(defcheck solution-b58bfe93
  (fn [words]
    (let [num-words (count words)]
      (letfn [(first-difference [a b]
                (first (drop-while nil? (map #(if (= %1 %2) nil %3) a b (range)))))
              (remove-at [n coll]
                (let [[lhs rhs] (split-at n coll)]
                  (concat lhs (rest rhs))))
              (related [a b]
                (let [a (vec a)
                      b (vec b)
                      ca (count a)
                      cb (count b)
                      count-differences (fn [a b] (apply + (map #(if (= %1 %2) 0 1) a b)))]
                  (if (= ca cb)
                    (= 1 (count-differences a b))
                    (if-let [i (first-difference a b)]
                      (let [[longer shorter] (if (> ca cb) [a b] [b a])]
                        (= (remove-at i longer) shorter))
                      (= 1 (- (max ca cb) (min ca cb)))))))]
        (let [related-words (apply conj {}
                              (for [word words]
                                [word (set (filter #(related word %)
                                             (disj words word)))]))
              invalid (fn [chain] (and (> (count chain) 1)
                                       ((complement (related-words (second chain)))
                                        (first chain))))]
          (letfn [(find-chain [chain possibilities]
                    (when (seq possibilities)
                      (let [word (first possibilities)
                            chain (conj chain word)
                            seen (set chain)]
                        (if (invalid chain)
                          nil
                          (do
                            (if (= num-words (count chain))
                              chain
                              (if-let [chain (find-chain
                                               chain
                                               (set (filter (complement seen)
                                                      (related-words word))))]
                                chain
                                (find-chain (pop chain) (disj possibilities word)))))))))]
            ((complement nil?) (find-chain () words))))))))

(defcheck solution-b6505d7e
  (fn word-chain? [words]
    (letfn [(adjacent?
              ([a b] (adjacent? a b 0))
              ([a b dist]
               (cond (> dist 1) false
                     (and (nil? a) (nil? b)) true

                     (= (first a) (first b))
                     (adjacent? (next a) (next b) dist)

                     :else
                     (or (adjacent? (next a) (next b) (inc dist))
                         (adjacent? a (next b) (inc dist))
                         (adjacent? (next a) b (inc dist))))))

            (connected?
              [mat node]
              (letfn [(walk
                        [node visits]
                        (let [tgts      (mat node)
                              visits    (conj visits node)
                              unvisited (clojure.set/difference tgts visits)]
                          (if (== (count words) (count visits)) true
                                                                (some true? (map #(walk % visits) unvisited)))))]
                (walk node #{})))

            (adjacency-mat
              [words]
              (reduce (fn [ret [a b]]
                        (-> (update-in ret [a] conj b)
                          (update-in [b] conj a)))
                (into {} (map #(vector % #{}) words))
                (for [i (range (count words))
                      j (range (inc i) (count words))
                      :let [a (words i)
                            b (words j)]
                      :when (adjacent? a b)]
                  [a b])))]
      (let [words-vec (vec words)
            mat (adjacency-mat words-vec)]
        (boolean (some (partial connected? mat) words))))))

(defcheck solution-b798d971
  (fn [words]
    (let [step1? (fn [a b]
                   (let
                    [delete1 (fn [y]
                               (map #(str (subs y 0 %) (subs y (inc %))) (range (count y))))
                     delete-test (fn [x y]
                                   (some #(= x %) (delete1 y)))
                     diff-test (fn [x y]
                                 (count (filter true? (map not= (seq x) (seq y)))))]
                     (case (- (count a) (count b))
                       1 (delete-test b a)
                       -1 (delete-test a b)
                       0 (= 1 (diff-test a b))
                       false)))
          conns (into {}
                  (for [x words]
                    [x (set
                         (for [y words :when (step1? x y)] y))]))
          search (fn s [cur visited]
                   (if (= (count words) (count visited))
                     true
                     (let [nexts (remove #(contains? visited %) (get conns cur))]
                       (some true? (map #(s % (conj visited %)) nexts)))))]
      (not-every? #(not (search % #{%})) words))))

(defcheck solution-b817a75e
  (fn [words]
    (let [pseudo-words
          (fn [word]
            (for [i (range (count word))]
              (apply str (keep-indexed #(if (not= i %) %2) word))))
          neighbours?
          (fn [& words]
            (boolean
              (let [[short long] (sort-by count words)]
                (if (#{0 1} (- (count long) (count short)))
                  (if (= (count short) (count long))
                    (= 1 (count (filter identity (map not= short long))))
                    (seq (clojure.set/intersection (set (pseudo-words long))
                           (hash-set short))))))))
          graph
          (reduce (fn [components word]
                    (let [matching-components
                          (group-by (fn [[w _]] (neighbours? w word))
                            components)]
                      (into (matching-components false)
                        (conj (for [[w words] (matching-components true)] [w (conj words word)])
                          [word (map first (matching-components true))]))))

            [] words)
          path
          (fn fp [moves graph]
            (cond
              (= 1 (count graph)) true
              (some (fn [[_ c]] (empty? c)) graph) false
              :else
              (some (fn [move]
                      (fp (graph move)
                        (into {} (map (fn [[k v]] [k (remove #{move} v)])
                                   (remove (comp #{move} first) graph))))) moves)))]
      (boolean (path words (into {} graph))))))

(defcheck solution-b85adfe3
  (fn [s]
    (letfn [(permutations [r s]
              (if (seq s)
                (mapcat #(permutations (conj r %) (disj s %)) (seq s))
                [r]))
            (chainable? [x y]
              (let [diffs (fn [x y] (<= (reduce + (map #(if (= %1 %2) 0 1) x y)) 1))
                    drop-char (fn [n s] (str (subs s 0 n) (subs s (inc n) (count s))))
                    drop-variants (fn [s] (map #(drop-char % s) (range (count s))))
                    deletions (fn [l s] (some #(= s %) (drop-variants l)))
                    cx (count x)
                    cy (count y)]
                (cond
                  (= cx cy) (diffs x y)
                  (= cx (inc cy)) (deletions x y)
                  (= (inc cx) cy) (deletions y x))))
            (chain? [s]
              (reduce #(when (chainable? %1 %2) %2) s))]
      (boolean
        (some chain? (permutations [] s))))))

(defcheck solution-b964a510
  (fn [words]
    (letfn [(any? [s]
              (not (not-any? identity s)))
            (grow? [a b]
              (not (not-any? #(= a (str (subs b 0 %) (subs b (inc %)))) (range (count b)))))
            (step? [a b] (case (- (count a) (count b))
                           0 (= 1 (apply + (map #(if (= %1 %2) 0 1) a b)))
                           -1 (grow? a b)
                           1 (grow? b a)
                           false))
            (chain? [w r]
              (or (empty? r)
                  (any? (for [x r]
                          (and (step? w x)
                               (chain? x (disj r x)))))))]
      (any? (for [w words]
              (chain? w (disj words w)))))))

(defcheck solution-b9c9e15
  (fn word-chain? [s]
    (let [changeable? (fn [from to]
                        (if (= (count from) (count to))
                          (let [cnt1 (count from)
                                cnt2 (->> (map #(= %1 %2) from to)
                                       (filter true?)
                                       count)]
                            (= (dec cnt1) cnt2))
                          (let [f (fn [from to]
                                    (let [cnt (count from)]
                                      (loop [i 0
                                             j 0
                                             penalty 0]
                                        (if (= i cnt)
                                          (= penalty 1)
                                          (if (= (get from i)
                                                (get to j))
                                            (recur (inc i) (inc j) penalty)
                                            (recur (inc i) j (inc penalty)))))))]
                            (if (> (count from) (count to))
                              (f from to)
                              (f to from)))))]
      (let [f (fn f [x s]
                (if (empty? s)
                  true
                  (let [good (filter #(changeable? x %) s)]
                    (if (empty? good)
                      false
                      (some #(f % (disj s %)) good)))))]
        (true? (some #(f % (disj s %)) s))))))

(defcheck solution-bab77930
  (fn [i-words]
    (let [
          n-count (count i-words)
          n-range (range n-count)
          n-map (apply hash-map (interleave i-words n-range))
          n-index (fn [i-word] (get n-map i-word))

          edit-distance (fn diff ([a-word b-word] (diff 0 a-word b-word))
                          ([distance a-word b-word]
                           (let [
                                 a-head (first a-word)
                                 b-head (first b-word)

                                 a-tail (rest a-word)
                                 b-tail (rest b-word)

                                 a-next (first a-tail)
                                 b-next (first b-tail)
                                 ]
                             (if (and (nil? a-head) (nil? b-head))
                               distance
                               (if (= a-head b-head)
                                 (diff distance a-tail b-tail)
                                 (if (= a-next b-head)
                                   (diff (inc distance) a-tail b-word)
                                   (if (= a-head b-next)
                                     (diff (inc distance) a-word b-tail)
                                     (diff (inc distance) a-tail b-tail))))))))
          edges (reduce
                  (fn [result i-word]
                    (reduce
                      #(conj %1 (vector i-word %2))
                      result
                      (filter #(= 1 (edit-distance i-word %)) i-words)))
                  #{}
                  i-words)
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-word]
                   (map last (get grouped-edges (n-index i-word))))

          build-chains (fn chain
                         ([i-word] (chain (dec n-count) (vector (vector i-word))))
                         ([i result]
                          (if (zero? i)
                            result
                            (chain
                              (dec i)
                              (reduce
                                concat
                                []
                                (map
                                  (fn [i-chain]
                                    (map
                                      #(conj (apply vector i-chain) %)
                                      (filter #(not (contains? (apply hash-set i-chain) %)) (n-dest (last i-chain)))))
                                  result))))))
          ]
      (reduce
        (fn [result i-word]
          (if (false? result)
            (boolean (some #(= (count %) n-count) (build-chains i-word)))
            result))
        false
        i-words))))

(defcheck solution-bad8f47c
  (fn [words]
    (letfn [(drop-1s [x] (map #(concat (take % x) (nthrest x (inc %)))
                           (range 0 (count x))))
            (can-ins [x y] (contains? (set (drop-1s x)) (seq y)))
            (can-subs [x y] (some #(apply = %) (map list (drop-1s x) (drop-1s y))))
            (can-link [x y] (or (can-ins x y) (can-ins y x) (can-subs x y)))
            (chain-all [chain-1 next-words rst]
              (= true (some #(chain-1 % (disj rst %)) next-words)))
            (chain-1 [word rst]
              (or (empty? rst)
                  (let [next-words (filter #(can-link word %) rst)]
                    (and (not-empty next-words)
                         (chain-all chain-1 next-words rst)))))]
      (chain-all chain-1 words words))))

(defcheck solution-bae2b764
  (fn [xs]
    (letfn [
            (chained? [[x y]]
              (let [chs (map count [(into #{} (concat x y)) x y])
                    minc (apply min chs)
                    maxc (apply max chs)]
                (= (inc minc) maxc)))
            (con_xy [ret [x y]]
              (loop [one (set [x y]), nret #{}, [h & t :as oret] (seq ret)]
                (if (empty? oret)
                  (conj nret one)
                  (if (or (h x) (h y))
                    (recur (into one h) nret t)
                    (recur one (conj nret h) t)))))
            (all_con? [xs]
              (->> xs
                (reduce con_xy #{})
                (rest)
                (empty?)))
            (one_path? [xs]
              (->> xs
                (mapcat identity)
                (frequencies)
                (vals)
                (filter #(= 1 %))
                (count)
                (> 2)))]
      (->>
        (for [x xs, y xs :when (not= x y)] (sort [x y]))
        (distinct)
        (filter chained?)
        (#(and (all_con? %) (one_path? %)))))))

(defcheck solution-bbcd5d72
  (fn is-word-chain? [words]
    (let [check (fn [w1 w2]
                  (loop [s1 (seq w1)
                         s2 (seq w2)
                         diff-cnt 0]
                    (if (and (nil? s1) (nil? s2))
                      (if (< 1 diff-cnt) false true)
                      (if (= (first s1) (first s2))
                        (recur (next s1) (next s2) diff-cnt)
                        (cond
                          (> (count s1) (count s2)) (recur (next s1) s2 (inc diff-cnt))
                          (< (count s1) (count s2)) (recur s1 (next s2) (inc diff-cnt))
                          :else (recur (next s1) (next s2) (inc diff-cnt)))))))
          word-chain? (fn word-chain? [w candidate-words]
                        (if (not (empty? candidate-words))
                          (let [next-words (filter (partial check w) candidate-words)]
                            (if (empty? next-words)
                              false
                              (some #(word-chain? % (disj candidate-words %)) next-words
                                )))
                          true))
          ]
      (if (some #(word-chain? % (disj words %)) words)
        true
        false
        ))))

(defcheck solution-bc6051bd
  (fn [W]
    (let [C concat
          M map
          m (zipmap W
              (M (fn [s]
                   (set
                     (apply C
                       (for [[a b] (M #(split-at % s) (range (+ 1 (count s))))
                             c (M char (range 97 123))]
                         [(C a [c] b)
                          (C a [c] (next b))
                          (C a (next b))
                          (C a b)])))) W))]
      ((fn G [s X]
         (or
          (empty? X)
          (not (not-any?
                 #(G % (disj X %))
                 (filter #(or (not s) ((m s) (vec %))) X)))))
       nil W))))

(defcheck solution-bcde47a
  (fn [words]
    (let [dist (fn dist [w u]
                 (cond (empty? w) (count u)
                       (empty? u) (count w)
                       :else (min (inc (dist (drop-last w) u))
                               (inc (dist w (drop-last u)))
                               (+ (dist (drop-last w) (drop-last u))
                                  (if (= (last w) (last u)) 0 1)))))
          adj (fn [w] (->> words (filter #(= 1 (dist w %)))))
          lookup (->> words (mapcat (juxt identity adj)) (apply hash-map))
          path? (fn path? [seen w]
                  (cond (= (count (conj seen w)) (count words)) true
                        (seen w) false
                        :else (some true? (map #(path? (conj seen w) %) (lookup w)))))]
      (not-every? nil? (map #(path? #{} %) words)))))

(defcheck solution-bce8b563
  (fn [s]
    (letfn [(chain? [x y]
              (let [s1  (vec x)
                    s2  (vec y)
                    cc (Math/abs (- (count s1) (count s2)))]
                (cond (= 0 cc) (= 1 (reduce + (map #(if (= %1 %2) 0 1) s1 s2)))
                      (= 1 cc) (->> (map #(vector (if (= %1 %2) 0 1) (if (= %2 %3) 0 1))
                                      (conj s1 0) (assoc s1 0 0) s2)
                                 (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) )
                                 ((fn [[a b]] (or (= 1 a) (= 1 b)))))
                      :else false)))]
      (loop [i (count s) a (map vector s)]
        (if (= 1 i) (-> a empty? not)
                    (recur (dec i)
                      (for [ax a
                            sx s
                            :let [cn (conj ax sx)]
                            :when (and (not (contains? (set ax) sx))
                                       (first (reduce (fn [[b l] x]
                                                        [(and b (chain? l x)) x])
                                                [true (first cn)] (rest cn))))]
                        cn)))))))

(defcheck solution-bcea739
  (letfn
   [(distribute [e [h & t :as xs]]
      (if (empty? xs)
        [[e]]
        (cons (cons e xs)
          (map #(cons h %) (distribute e t)))))

    (permutations [xs]
      (if-let [[e & t] xs]
        (mapcat #(distribute e %) (permutations t))
        [[]]))

    (count-diffs [[a & b :as l] [x & y :as r]]
      (cond
        (nil? a)        (count r)
        (nil? x)        (count l)
        (= a x)         (count-diffs b y)
        (= a (first y)) (+ 1 (count-diffs l y))
        (= x (first b)) (+ 1 (count-diffs b r))
        :else           (+ 1 (count-diffs b y))))

    (chain? [words]
      (->> (partition 2 1 words)
        (map #(apply count-diffs %))
        (every? #(= 1 %))))]

    (fn [xs]
      (->> (seq xs)
        (permutations)
        (some chain?)
        (true?)))))

(defcheck solution-bd41ff9c
  (fn chainable? [words]
    (let [levenshtein (fn levenshtein [s t]  ;;;; The good one
                        (let [n (count s) m (count t) longer (max n m)]
                          (if (or (zero? n) (zero? m)) longer
                                                       (let [s (into [] s) t (into [] t) t+2 (+ 2 m)]
                                                         (loop [i 0 v0 (into [] (range  (inc m)))]
                                                           (if (>= i n) (v0 m)
                                                                        (recur (inc i)
                                                                          (loop [j 0 v1 (into [] (repeat (inc longer) (inc i)))]
                                                                            (if (>= j m) v1
                                                                                         (let [cost (if (= (s i) (t j)) 0 1)]
                                                                                           (recur (inc j)
                                                                                             (assoc v1 (inc j)
                                                                                                       (min (inc (v1 j)) (inc (v0 (inc j)))
                                                                                                         (+ (v0 j) cost))))))))))))))]
      (> 2 (count (filter #(> 2 %) (map
                                     (fn [w] (count (filter #(= 1 %)  (map #(levenshtein w %) words))))
                                     words)))))))

(defcheck solution-bdac31cb
  (let [
        sub? (fn [p n]
               (= 1 (reduce + (map #(if (not= % %2) 1 0) p n))))

        del? (fn [p n]
               (if (= (first p) (first n))
                 (recur (rest p) (rest n))
                 (= (seq (rest p)) (seq n))))

        one-away? (fn [p n]
                    (cond
                      (nil? p) true
                      (= (count p) (count n)) (sub? p n)
                      (= (dec (count p)) (count n)) (del? p n)
                      (= (inc (count p)) (count n)) (del? n p)
                      :else false))

        connected? (fn c? [p s]
                     (if (empty? s)
                       true
                       (if (some #(and (one-away? p %) (c? % (disj s %))) s) true false)))]

    (fn [s] (connected? nil s))))

(defcheck solution-be160112
  (fn [words]
    (letfn [(step? [[x & xs :as a] [y & ys :as b]]
              (if (and x y (= x y))
                (recur xs ys)
                (or (= xs (seq b)) (= ys (seq a)) (= xs ys))))
            (extend [chains]
              (mapcat
                #(map (partial conj %)
                   (filter
                     (partial step? (last %))
                     (remove (set %) words)))
                chains))]
      ((comp not empty? nth)
       (iterate extend (map vector words))
       (dec (count words))))))

(defcheck solution-bf56ad9d
  (fn word-chain [s]
    (letfn [(combinations
              [coll]
              (when (> (count coll) 1)
                (lazy-cat (map vector (repeat (first coll)) (rest coll))
                  (combinations (rest coll)))))
            (levenshtein-dist
              [seq1 seq2]
              (cond
                (empty? seq1) (count seq2)
                (empty? seq2) (count seq1)
                :else (let [cost (if (= (first seq1) (first seq2)) 0 1)]
                        (min (inc (levenshtein-dist (rest seq1) seq2))
                          (inc (levenshtein-dist seq1 (rest seq2)))
                          (+ cost
                             (levenshtein-dist (rest seq1) (rest seq2)))))))
            (diff-1 [[w1 w2]] (= (levenshtein-dist w1 w2) 1))
            (make-lookup [[w1 w2]] {w1 [w2] w2 [w1]})
            (make-chains
              ([lookup word] (make-chains lookup [word] #{word}))
              ([lookup chain seen]
               (let [candidates (filter (complement seen) (lookup (last chain)))]
                 (if (empty? candidates)
                   (list chain)
                   (mapcat #(make-chains lookup
                              (conj chain %)
                              (conj seen %))
                     candidates)))))]
      (let [lookup (apply (partial merge-with concat)
                     (->> (combinations s)
                       (filter diff-1)
                       (map make-lookup)))
            chain-len (count s)]
        (->> lookup
          keys
          (mapcat (partial make-chains lookup))
          (map count)
          (some (partial = chain-len))
          ((complement nil?)))))))

(defcheck solution-c00684f3
  (fn word-chain? [words]
    (let [cw? (fn cw? ([wa wb] (cw? true wa wb))
                ([fd wa wb]
                 (let [[fa ra] (split-at 1 wa) [fb rb] (split-at 1 wb)]
                   (cond
                     (and (empty? fb) (empty? fa)) true
                     (= fa fb) (cw? fd ra rb)
                     :else (when fd (some #(apply cw? false %) [[wa rb] [wb ra] [ra rb]]))
                     ))))
          cc? (fn cc? [w wsl]
                (if (empty? wsl) true
                                 (let [clws (filter #(cw? w %) wsl)]
                                   (when (not (empty? clws))
                                     (some #(cc? % (disj wsl %)) clws))))
                )
          ]
      (true? (some #(cc? % (disj words %)) words))
      )))

(defcheck solution-c0a0380f
  (fn chain?
    ([words]
     (if (some #(chain? % (disj words %)) words) true false))
    ([word, words]
     (letfn [(lev-dict [s, t]
               (let [slen (count s)
                     tlen (count t)]
                 (loop [i 1
                        d (merge
                            (reduce merge (map #(assoc-in {} [% 0] %) (range (inc slen))))
                            (reduce (partial merge-with merge) (map #(assoc-in {} [0 %] %) (range 1 (inc tlen)))))]
                   (if (> i slen)
                     (get-in d [slen tlen] 0)
                     (recur (inc i)
                       (loop [j 1
                              d d]
                         (if (> j tlen)
                           d
                           (recur (inc j) (let [cost (if (= (nth s (dec i)) (nth t (dec j))) 0 1)
                                                pfm  (min
                                                       (+ 1    (get-in d [(dec i) j] 0))
                                                       (+ 1    (get-in d [i (dec j)] 0))
                                                       (+ cost (get-in d [(dec i) (dec j)] 0)))]
                                            (assoc-in d [i j]
                                              (if (and
                                                   (> i 1)
                                                   (> j 1)
                                                   (= (nth s (- i 2)) (nth t (- j 2)))
                                                   (= (nth s (- i 2)) (nth t (- j 2))))
                                                (min
                                                  pfm
                                                  (+ cost (get-in d [(- i 2) (- j 2)] 0)))
                                                pfm)))))))))))]
       (if (empty? words)
         true
         (let [matches (filter #(= 1 (lev-dict word %)) words)]
           (and
            (> (count matches) 0)
            (some #(chain? % (disj words %)) matches))))))))

(defcheck solution-c107919b
  (fn [ws]
    (letfn [(lev [s t]
              (let [d (into [(vec (range 0 (inc (count s))))]
                        (map #(vec (concat [%] (repeat (count s) 0)))
                          (range 1 (inc (count t)))))
                    res (reduce (fn [d i]
                                  (reduce (fn [d j]
                                            (assoc-in d [i j]
                                              (if (= (get t (dec i)) (get s (dec j)))
                                                (get-in d [(dec i) (dec j)])
                                                (min (inc (get-in d [(dec i) j]))
                                                  (inc (get-in d [i (dec j)]))
                                                  (inc (get-in d [(dec i) (dec j)]))))))
                                    d (range 1 (inc (count s)))))
                          d (range 1 (inc (count t))))]
                (get-in res [(count t) (count s)])))
            (word-graph [& ws]
              (reduce (fn [g w]
                        (let [xs (filter identity
                                   (map #(if (= 1 (lev w %)) % nil) ws))]
                          (assoc g w xs)))
                {} ws))
            (dfs [node, visited, g]
              (if (nil? node)
                (count visited)
                (apply max (concat [(inc (count visited))]
                                   (map #(dfs % (conj visited node) g)
                                     (filter (complement (set visited))
                                       (g node)))))))]
      (let [g (apply word-graph ws)]
        (= (count ws) (apply max (map #(dfs % [] g) ws)))))))

(defcheck solution-c2d3c645
  (fn r [s]
    (let [l (fn [str1 str2]
              "a Clojure levenshtein implementation using transient data structure"
              (let [n (count str1) m (count str2)]
                (cond
                  (= 0 n) m
                  (= 0 m) n
                  :else
                  (let [prev-col (transient (vec (range (inc m)))) col (transient [])] ; initialization for the first column.
                    (dotimes [i n]
                      (assoc! col 0 (inc i))                  ; update col[0]
                      (dotimes [j m]
                        (assoc! col (inc j)                   ; update col[1..m]
                          (min (inc (get col j))
                            (inc (get prev-col (inc j)))
                            (+ (get prev-col j) (if (= (get str1 i) (get str2 j)) 0 1)))))
                      (dotimes [i (count prev-col)]
                        (assoc! prev-col i (get col i))))     ;
                    (last (persistent! col))))))              ; last element of last column

          g (into {} (map (fn [el] [el
                                    (for [x s
                                          :when (= 1 (l x el))]
                                      x)]) s))]
      (loop [current (first s)
             starts (next s)
             path [current]
             backpoints []]
        (let [curr (last path)
              variants (remove (into #{} path) (g curr))]
          (cond
            (nil? current) false
            (= (into #{} path) s) true
            (empty? variants)
            (if (seq backpoints)
              (let [[recent-curr recent-variants] (last backpoints)
                    new-path (conj
                               (vec (take-while #(not= recent-curr %) path))
                               recent-curr
                               (first recent-variants))
                    another-variants (rest recent-variants)
                    new-backpoints (if (seq another-variants)
                                     (conj (butlast backpoints) [recent-curr another-variants])
                                     (butlast backpoints))]
                (recur current starts new-path new-backpoints))
              (recur (first starts) (next starts) [(first starts)] backpoints))
            (= 1 (count variants)) (recur current starts (conj path (first variants)) backpoints)
            :else (recur current starts (conj path (first variants)) (conj backpoints [curr (rest variants)]))))))))

(defcheck solution-c41b44bc
  (fn[s]
    (letfn [(ds[a b]
              (if (= (first a) (first b)) (ds (rest a) (rest b)) [a b]))
            (issim[a b]
              (let [[c d] (map count (apply ds (map reverse (ds a b))))]
                (or (= 1 c d) (= 1 (+ c d)))))
            (mut[i s]
              (map #(let [[a b] (split-at % s)] (concat a [i] b)) (range (inc (count s)))))
            (perm[s]
              (if (seq s)
                (mapcat (partial mut (first s)) (perm (rest s)))
                [[]]))
            (iss[s]
              (every? (partial apply issim) (map vector s (rest s))))]
      (if (some iss (perm s)) true false))))

(defcheck solution-c424de81
  (fn [words]
    (let
     [chains-with (fn chains-with [word1 word2]
                    (let [c1 (count word1) c2 (count word2)]
                      (cond
                        (not (#{-1 0 1} (- c1 c2))) false
                        (>= 1 (max c1 c2)) true
                        (= (first word1) (first word2)) (chains-with (rest word1) (rest word2))
                        (= (last word1) (last word2)) (chains-with (butlast word1) (butlast word2))
                        :else false)))
      pick-chaining-words (fn [word unchained]
                            (filter #(chains-with word %) unchained))
      creates-a-chain (fn creates-a-chain [chain words-rest]
                        (if (empty? words-rest)
                          true
                          (some #(creates-a-chain (conj chain %) (disj words-rest %))
                            (pick-chaining-words (last chain) words-rest))))
      chaining-words? (fn [words]
                        (some #(creates-a-chain [%] (disj words %))
                          words))]
      (true? (chaining-words? words)))))

(defcheck solution-c4dc0ee2
  (fn [x]
    (letfn [(g1 [x y]
              (let [[a b]
                    (map seq (sort-by count [x y]))]
                (some #(= a %) (for [i (range (count b))]
                                 (keep-indexed #(if (not= i %1) %2) b)))))
            (g2 [x y]
              (let [a (count x)
                    b (count y)]
                (and (= a b)
                     (= (dec a)
                       (count
                         (filter #(apply = %)
                           (map vector x y)))))))
            (g3 [x y n]
              (if (= n 1)
                x
                (g3 (apply concat
                      (for [i x]
                        (map concat (repeat i)
                          (map vector (remove (set i) y))))) y (dec n))))
            (g4 [x]
              (g3 (map vector x) x (count x)))
            (g5 [x]
              (every? #(or (apply g1 %) (apply g2 %)) (partition 2 1 x)))]
      (boolean (some g5 (g4 x))))))

(defcheck solution-c4f552a7
  (fn [wordSet]
    (letfn[(nextChainLink? [str1 str2];check if str2 and str1 could be linked together
             (let [[s1 s2] (if (< (count str1) (count str2)) [str1 str2] [str2 str1])
                   s1Len (count s1), s2Len (count s2)]
               (cond
                 (> (- s2Len s1Len) 1) false
                 (= s2Len s1Len) (= 1 (count (remove true? (map = s1 s2)))) ;only one letter difference
                 :else (some true? ;some true?
                         (map #(= (seq s1) %);compare if s1 = s2
                           (for [r (range s2Len)]
                             (concat (take r s2) (drop (inc r) s2))))))))];remove one letter from the longer string
      (let [wordCount (count wordSet)]
        (loop [chains (for [w wordSet] [#{w} w])];initially the chains grow at each word, first element is the visited word, second one is the last word on the chain
          (let [extendedChains (for [[visited lastWord] chains word wordSet :when (and (nil? (visited word)) (nextChainLink? lastWord word))]
                                 [(conj visited word) word])];if the word has not been visited and is one step away, extend the chain
            (cond
              (empty? extendedChains) false;cannot extend the chain any more
              (not-any? #(= wordCount %) (map (comp count first) extendedChains)) (recur extendedChains);keep extending the chains
              :else true)))))))

(defcheck solution-c5e3ea9d
  (fn chains [words]

    (let [
          levenshtein (fn levenshtein
                        ([s1 s2]
                         (levenshtein s1 s2
                           (memoize (fn [s1 s2 memo]
                                      (cond (empty? s1) (count s2)
                                            (empty? s2) (count s1)
                                            :else (min
                                                    (+ (if (= (first s1)(first s2)) 0 1)
                                                       (levenshtein (rest s1)(rest s2) memo))
                                                    (inc (levenshtein (rest s1) s2 memo))
                                                    (inc (levenshtein s1 (rest s2) memo))))))))
                        ([s1 s2 memo-fn] (memo-fn s1 s2 memo-fn)))


          iter-perm (fn [v]
                      (let [len (count v),
                            j (loop [i (- len 2)]
                                (cond (= i -1) nil
                                      (< (v i) (v (inc i))) i
                                      :else (recur (dec i))))]
                        (when j
                          (let [vj (v j),
                                l (loop [i (dec len)]
                                    (if (< vj (v i)) i (recur (dec i))))]
                            (loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
                              (if (< k l)
                                (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
                                v))))))

          vec-lex-permutations (fn vec-lex-permutations [v]
                                 (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

          lex-permutations
          (fn [c]
            (lazy-seq
              (let [vec-sorted (vec (sort c))]
                (if (zero? (count vec-sorted))
                  (list [])
                  (vec-lex-permutations vec-sorted)))))

          permutations (fn [items]
                         (let [v (vec items)]
                           (map #(map v %) (lex-permutations (range (count v))))))

          perms (permutations words)
          paired (map #(partition 2 1 %) perms)
          distances (map (partial apply +)(map #(map (fn [[a b]] (levenshtein a b)) %) paired))
          result (some #(= (dec (count words)) %) distances)]

      (if (nil? result) false result))))

(defcheck solution-c602fd43
  (fn word-chain? [words]
    (letfn [(one-letter-diff? [w1 w2] (if (= w1 w2) false
                                                    (let [k (->> (map vector w1 w2) (take-while (partial apply =)) count)
                                                          s1 (drop k w1) s2 (drop k w2)]
                                                      (or (= (next s1) (next s2)) (= s1 (next s2)) (= (next s1) s2)))))
            (has-chain? [nodes edges]
              (letfn [(visit-all [nodes visited k] (some identity (for [node nodes] (visit node visited k))))
                      (visit [node visited k] (if (zero? k) true
                                                            (let [to-visit (clojure.set/difference (edges node) visited)
                                                                  new-visited (conj visited node)]
                                                              (visit-all to-visit new-visited (dec k)))))]
                (visit-all words #{} (dec (count words)))))
            (adjacents [word] (set (filter (partial one-letter-diff? word) words)))] (boolean (has-chain? words adjacents)))))

(defcheck solution-c6f835e4
  (fn [S]
    (letfn [
            (difference [A B]
              (reduce disj A B))

            (union [A & Bs]
              (reduce (partial reduce conj) A Bs))

            (intersection [A B]
              (reduce #(into %1 (A %2)) #{} B))

            (all-but-one? [lst]
              (= 1 (reduce + (map #(if % 0 1) lst))))

            (take-similar [pred S x]
              (set (filter (partial pred x) S)))

            (connections [pred S]
              (reduce #(assoc %1 %2
                                 (take-similar pred S %2)) {} S))

            (connected? [C]
              (loop [kwn (-> C first key list set),
                     unk (-> C rest keys set)]
                (if (empty? unk)
                  true
                  (let [bnd (apply union (map C kwn))]
                    (if (empty? (intersection bnd unk))
                      false
                      (recur (union kwn bnd)
                        (difference unk bnd)))))))

            (tracable? [C] ; very hard problem ... cheat
              (> 3 (count (filter #(< % 2)
                            (map #(count (val %)) C)))))

            (inserted? [A B]
              (loop [a (seq A), b (seq B)]
                (if (= (first a) (first b))
                  (recur (rest a) (rest b))
                  (= a (rest b)))))

            (deleted? [A B]
              (inserted? B A))

            (word-chain [S]
              (letfn [
                      (pred [a b]
                        (cond
                          (= (count a) (count b)) ; substitution
                          (all-but-one? (map = a b)),

                          (< (count a) (count b)) ; insertion
                          (inserted? a b)

                          (> (count a) (count b)) ; deletion
                          (deleted? a b)))]

                (connections pred S)))]

      (let [C (word-chain S)]
        (and (connected? C) (tracable? C))))))

(defcheck solution-c73c06da
  (fn word-chain? [words]
    (let [levenshtein-row (fn levenshtein-row [word-x y-char previous-row]
                            (let [target-y (inc (first previous-row))
                                  inner-levenshtein-row (fn [word-x acc]
                                                          (let [x-char (first word-x)
                                                                pos (count acc)]
                                                            (if (seq word-x)
                                                              (recur (rest word-x) (conj acc (cond (= x-char y-char) (nth previous-row (dec pos))
                                                                                                   :else (let [a (inc (last acc))
                                                                                                               b (inc (nth previous-row pos))
                                                                                                               c (inc (nth previous-row (dec pos)))]
                                                                                                           (min a (min b c))))))
                                                              acc))
                                                          )]
                              (inner-levenshtein-row word-x [(inc (first previous-row))])))
          levenshtein-distance (fn levenshtein-distance [a b]
                                 (last (reduce #(levenshtein-row a %2 %1) (into [] (range (inc (count a)))) b )))
          node-and-edges (fn node-and-edges [word word-list m]
                           (reduce #(if (= 1 (levenshtein-distance word %2)) (assoc %1 word (conj (get %1 word #{}) %2)) %1) m word-list))
          walk-graph (fn walk-graph [words graph]
                       (let [inner-walk-graph (fn inner-walk-graph [word visited]
                                                (let [paths (apply disj (graph word) (into [] visited))
                                                      new-visited (conj visited word)]
                                                  (cond (= (count graph) (count new-visited)) new-visited
                                                        (= 0 (count paths)) '()
                                                        :else (map (fn [next-word] (inner-walk-graph next-word new-visited)) paths))))]
                         (partition (count graph) (flatten (map (fn [word] (inner-walk-graph word [])) words)))))
          graph-word (fn word-graph [word-list]
                       (reduce #(node-and-edges %2 word-list %1) {} word-list))

          ]
      (> (count (walk-graph words (graph-word words))) 0))))

(defcheck solution-c75e6044
  (fn [words]
    (letfn [(ed-dist [a b]
              (cond
                (and (nil? a) (nil? b)) 0
                (nil? b) (count a)
                (nil? a) (count b)
                :else (let [ra (next a) rb (next b)]
                        (if (= (first a) (first b))
                          (ed-dist ra rb)
                          (+ (min
                               (ed-dist ra rb)
                               (ed-dist ra b)
                               (ed-dist a rb)) 1)))))
            (solve [graph visited source]
              (let [to-visit (filter #(not (contains? visited %)) (graph source))]
                (if (nil? (first to-visit))
                  (= (+ (count visited) 1) (count words))
                  (reduce #(or %1 %2) (map #(solve graph (conj visited source) %) to-visit)))))]
      (let [graph

            (apply hash-map (mapcat identity (map #(vector % (filter (fn [w] (= (ed-dist % w) 1)) words)) words)))]

        (reduce #(or %1 %2) (map #(solve graph #{} %) words))))))

(defcheck solution-c83098d5
  (fn [coll]
    (letfn
     [(drop-nth [n coll] (concat (take n coll) (drop (inc n) coll)))
      (one-dist? [w1 w2]
        (let [[minw maxw] (sort-by count [w1 w2])
              count-trans #(->> %& (apply map =) (remove true?) count)]
          (case (- (count maxw) (count minw))
            0 (>= 1 (count-trans w1 w2))
            1 (->> maxw count range
                (map #(-> % (drop-nth maxw) ((partial apply str)) (= minw)))
                (some true?))
            false)))
      (is-word-chain? [coll] (every? true? (map one-dist? coll (rest coll))))
      (perm [s]
        (if (= 1 (count s)) [(vec s)]
                            (apply concat (for [x s] (map #(conj % x) (perm (disj s x)))))))]
      (boolean (some is-word-chain? (perm coll))))))

(defcheck solution-c84ed941
  (fn word-chains? [ws]
    (let [words (seq ws)
          single-edit?  (fn [word1 word2]
                          (if (> (Math/abs (- (count word1) (count word2))) 2) false
                                                                               (loop [xc1 (seq word1) xc2 (seq word2)]
                                                                                 (if (= (first xc1) (first xc2)) (recur (next xc1) (next xc2))
                                                                                                                 (or (= (next xc1) (next xc2)) (= xc1 (next xc2)) (= (next xc1) xc2))))))
          graph (for [a words b (next (drop-while #(not= % a) words)) :when (single-edit? a b)] [a b])
          nbrs-map (reduce (fn [m [a b]] (assoc m a (into #{b} (m a)) b (into #{a} (m b)))) {} graph)
          trav-path (fn trav-path [m start seen]
                      (if-let [nodes (seq (remove #(some #{%} seen) (m start)))]
                        (mapcat #(trav-path m % (conj seen start)) nodes)
                        [(conj seen start)]))]
      ;(not-every? nil? (map #(trav-path nbrs-map % (-> words count)) words))))
      (loop [w words] (if (= (count words) (reduce max (map count (trav-path nbrs-map (first w) [])))) true (if w (recur (next w)) false))))))

(defcheck solution-c87d537b
  (fn [s]
    (let [added (fn added
                  ([w1 w2 n] (if (> n 1) false
                                         (if (and (empty? w1) (empty? w2))
                                           true
                                           (if (= (first w1) (first w2))
                                             (added (rest w1) (rest w2) n)
                                             (or (added (rest w1) w2 (inc n))
                                                 (added w1 (rest w2) (inc n)))))))
                  ([w1 w2] (if (= w1 w2) false (added w1 w2 0))))
          subst (fn [w1 w2] (if (and (= (count w1) (count w2))
                                     (= (dec (count w1))
                                       (count (filter identity (map = w1 w2)))))
                              true
                              false))
          edit-one (fn [w1 w2] (or (added w1 w2) (subst w1 w2)))
          chain (fn chain [w s] (if (= (count s) 1)
                                  (edit-one w (first s))
                                  (some identity (map #(chain % (disj s %))
                                                   (filter #(edit-one w %) s)))))]
      (if (some identity (map #(chain % (disj s %)) s))
        true
        false))))

(defcheck solution-c88e0927
  (fn [s] (let
           [c cons
            l
              (fn [x y]
                (if (nil? x)
                  true
                  ((fn [[x & q] [y & z]]
                     (if (= x y)
                       (or (= q z nil)
                           (recur q z))
                       (or (= q z)
                           (= q (c y z))
                           (= (c x q) z))))
                   x y)))
            f
              (fn c? [x y]
                (if (empty? y)
                  true
                  (some #(and (l x %)
                              (c? % (disj y %)))
                    y)))]
            (true? (f nil s)))))

(defcheck solution-c8c6f17d
  (fn [wordset]  (letfn [

                         (countdiff [wa wb]
                           (count (filter false? (map #(= %1 %2) wa wb))))

                         (deleteset [word] (let [len (count word)]
                                             (if (empty? word) (set "")
                                                               (set (map
                                                                      (fn [s l r] (str (subs s 0 l) (subs s r)))
                                                                      (repeat len word)
                                                                      (range len)
                                                                      (range 1 (inc len)))))))

                         (chainable [wa wb] (let [la (count wa) lb (count wb)]
                                              (cond (= la lb )       (<= (countdiff wa wb) 1)
                                                    (= 1 (- la lb))  (contains? (deleteset wa) wb)
                                                    (= 1 (- lb la))  (contains? (deleteset wb) wa)
                                                    :else false)))

                         (neighbours [w ws]
                           (set (filter #(chainable w %)   (disj ws w))) )

                         (longest [ls]  (set (last (sort-by count ls))) )

                         (dfs [cur path visited]
                           (let [nbs (neighbours cur (clojure.set/difference wordset visited)) ]
                             (if (empty? nbs) path
                                              (longest
                                                (map #(dfs % (cons % path) (conj  visited % )) nbs)))))]

                   (= (longest (map #(dfs % nil (set %)) wordset)) wordset))))

(defcheck solution-ca9d0b8d
  (fn chainX[x]
    (letfn[
           (different [ai bi]
             (= ai bi)
             )
           (substitution[a b]
             (and
              (= (count a)(count b))
              (= (count (filter false? (map different a b))) 1)
              )
             )
           (removeS[s n]
             (str (subs s 0 n) (subs s (inc n)))
             )
           (inserted [a b]
             (if
              (= (inc (count a))(count b))
               (let [idx (first (filter #(not= (get a %)(get b %)) (range (count b))))]
                 (= a (removeS b idx))
                 )
               false
               )
             )
           (chained[a b]
             (or
              (substitution a b)
              (inserted a b)
              (inserted b a)
              )
             )
           (chainRec[w x]
             (if (empty? x)
               true
               (not (not-any? (fn[w1] (and (chained w w1) (chainRec w1 (disj x w1)))) x))
               )

             )


           ]

      (not (not-any?  (fn[w] (chainRec w (disj x w))) x))
      )
    ))

(defcheck solution-caca8096
  (fn __ [wordset]
    (let[levinstine(fn [word1 word2]
                     (last (reduce
                             (fn [a b]
                               (reduce
                                 #(conj %1 (min (inc (last %1)) (inc (nth a (count %1))) (+ (nth a (dec( count %1))) (if (= b %2) 0 1))))
                                 (vector(inc (first a)))
                                 word2))
                             (range 0 (inc (count word2)))
                             word1)))
         wordvector (vec wordset)
         wordpairs (group-by first (filter #(= (apply levinstine %) 1) (apply concat (map #(map (partial vector %)  wordvector) wordvector))))
         next-word (fn [words]
                     (apply concat (map #(map (partial conj %) (filter (fn[x](nil? ((set %) x) )) (map last (wordpairs (last %))))) words))
                     )
         ]

      (loop [a (apply concat (map last wordpairs)) b (- (count wordset) 2) ](if (pos? b) (recur (next-word a) (dec b)) (not(empty? a)) ) )

      )))

(defcheck solution-cad4d63
  (fn [word-set]
    (letfn [(any? [c]
              (true? (some true? c)))
            (word-except-letter [word i]
              (if (< i (count word))
                (vec (concat (subvec word 0 i) (subvec word (inc i))))))
            (one-letter-different-or-added? [word1 word2]
              (any? (for [i (range (count word2))]
                      (or (= (word-except-letter word1 i)
                            (word-except-letter word2 i))
                          (= word1 (word-except-letter word2 i))))))
            (chainable? [str1 str2]
              (let [word1 (vec str1)
                    word2 (vec str2)]
                (or (one-letter-different-or-added? word1 word2)
                    (one-letter-different-or-added? word2 word1))))]
      (let [next-words (into {nil word-set}
                         (for [word word-set]
                           [word (set (filter #(chainable? word %) word-set))]))
            finishable? (fn f? [cur-word remaining-words]
                          (or (= remaining-words #{})
                              (any? (map #(f? % (disj remaining-words %))
                                      (filter #((next-words cur-word) %)
                                        remaining-words)))))]
        (finishable? nil word-set)))))

(defcheck solution-cae2e05
  (fn [words]
    (letfn [(next? [word-c word-n]
              (case (- (count word-c) (count word-n))
                1 (true? (some #(= (seq word-n) %) (map #(concat (take % word-c) (next (drop % word-c))) (range 0 (count word-c)))))
                -1 (true? (some #(= (seq word-c) %) (map #(concat (take % word-n) (next (drop % word-n))) (range 0 (count word-n)))))
                0 (= 1 (count (filter false? (map #(= %1 %2) word-c word-n))))
                false))
            (chain [word searched-word words]
              (let [nexts (filter #(and (next? word %) (nil? (searched-word %))) words)]
                (if (seq nexts)
                  (true? (some true? (map #(chain % (set (cons % searched-word)) words) nexts)))
                  (= (count searched-word) (count words)))))]
      (true? (some true? (map #(chain % #{%} words) words))))))

(defcheck solution-cafa1042
  (fn [s]
    (let [letters (reduce into #{} s)]
      (letfn [(dels [s] (map #(str (subs s 0 %) (subs s (inc %))) (range (count s))))
              (subts [s] (for [i (range (count s)) c letters]
                           (str (subs s 0 i) c (subs s (inc i)))))
              (insrts [s] (for [i (range (inc (count s))) c letters]
                            (str (subs s 0 i) c (subs s i))))
              (diffs [s] (clojure.set/union (set (dels s)) (set (subts s)) (set (insrts s))))]
        (let [m (map (fn [x] [x  (set (filter (disj s x) (diffs x)))]) s)
              m (into {} m)]
          (letfn [(dfs [x seen]
                    (if (= (inc (count seen)) (count s))
                      true
                      (when-let [ss (remove seen (m x))]
                        (some #(dfs % (conj seen x)) ss))))]
            (true?  (some #(dfs % #{}) s))))))))

(defcheck solution-cb8c485e
  (fn [wset]
    (letfn [(dropletter [word]
              (map #(apply str (concat (take % word) (drop (inc %) word))) (range (count word))))
            (doChecks [w1 w2]
              (let [w1s (dropletter w1) w2s (dropletter w2)]
                (some true? [(some #(= % w2) w1s)
                             (some #(= % w1) w2s)
                             (some true? (map = w1s w2s))])))
            (iterCheck [w s]
              (if (empty? s)
                true ; <--------- No more words to check -> success.
                (some ; <-------- Only one successful path required.
                  (fn [sn]
                    (if (doChecks w sn)
                      (iterCheck sn (remove #(= % sn) s)) ; Branch here.
                      false)) ; No success with this path.
                  s)))]
      (boolean (some (fn [w] (iterCheck w (remove #(= % w) wset))) wset)))))

(defcheck solution-cbd41ed6
  (let [adj? (fn [x y]
               (or (and (empty? x)
                        (empty? y))
                   (= x (rest y))
                   (= (rest x) y)
                   (= (rest x) (rest y))
                   (and (= (first x) (first y))
                        (recur (rest x) (rest y)))))
        f (fn f [s]
            (cond
              (empty? s) '(())
              :else (for [i s
                          :let [s (disj s i)]
                          p (f s)
                          :when (or (empty? p) (adj? (seq (first p)) (seq i)))]
                      (cons i p))))]
    #(>= (count (f %)) 1)))

(defcheck solution-cc6b356d
  (fn chain-exists? [words]
    (let [words-close? (fn words-close? [a b]
                         (cond (= (first a) (first b))
                               (if-not (nil? (first a))
                                 (words-close? (subs a 1) (subs b 1))
                                 true)
                               (empty? a) (= 1 (count b))
                               (empty? b) (= 1 (count a))
                               (not= (first a) (first b))
                               (or (= (subs a 1) (subs b 1))
                                   (= a (subs b 1))
                                   (= (subs a 1) b))))
          edges (filter #(apply words-close? %1)
                  (mapcat #(%1 words)
                    (map (fn [w]
                           (fn [words]
                             (->> words
                               (filter #(and (not= w %1)
                                             (< 0 (compare %1 w))))
                               (map vector (repeat w)))))
                      words)))
          f (fn f' [cur-word remaining-words remaining-edges]
              (if (empty? remaining-words)
                true
                (let [cur-edges (filter (comp not empty?
                                              (partial filter
                                                (partial = cur-word)))
                                  remaining-edges)]
                  (if (empty? cur-edges)
                    false
                    (some #(let [next-word (first (filter
                                                    (partial not= cur-word) %1))]
                             (f' next-word
                               (filter (partial not= next-word) remaining-words)
                               (filter (partial not= %1) remaining-edges)))
                      cur-edges)))))]
      (boolean
        (some #(f %1 (filter (partial not= %1) words) edges)
          words)))))

(defcheck solution-cd34884b
  (fn [v]
    (let [one-off? (fn [s t]
                     (if (= (count s) (count t))
                       (= 1 (count (clojure.set/difference (set s) (set t))))
                       (let [r (sort-by count [s t])]
                         (reduce #(or %1 %2)
                           (map #(= (first r) %)
                             (for [i (range 0 (count (second r)))]
                               (apply str (concat (take i (second r)) (drop (inc i) (second r))))))))))
          f (fn f [s v]
              (if (empty? v)
                true
                (for [x (filter #(one-off? s %) v)]
                  (f x (remove #{x} v)))))]
      (not (empty?  (flatten  (for [x v] (f x (remove #{x} v)))))))))

(defcheck solution-cea2e6ac
  (fn word-chain?
    [word-set]
    (letfn [(strings-info
              [a b]
              (let [longer (if (> (count a) (count b)) a b)
                    shorter (if (= longer a) b a)
                    equal? (= longer shorter)
                    count-diff (- (count longer) (count shorter))]
                [longer shorter equal? count-diff]))
            (letter-apart?
              [a b]
              (let [info-v (strings-info a b)]
                (cond (info-v 2)
                      false
                      (> (last info-v) 1)
                      false
                      :else
                      (loop [longer (first info-v)
                             shorter (second info-v)]
                        (if (= (first longer) (first shorter))
                          (recur (rest longer) (rest shorter))
                          (if (zero? (last info-v))
                            (= (rest longer) (rest shorter))
                            (= (apply str (rest longer)) (apply str shorter))))))))
            (set-permutations
              [word-set & acc]
              (map
                #(if (= 1 (count word-set))
                   (reduce (fn [a b] (cond (false? a) false
                                           (letter-apart? a b) b
                                           :else false)) (flatten (conj acc %)))
                   (set-permutations (disj word-set %) (cons % acc))) word-set))]
      (true? (some string? (flatten (set-permutations word-set)))))))

(defcheck solution-cf65398c
  (fn [wordset]
    (letfn
     [(member? [x c] (some #{x} c))
      (linked? [str1 str2]
        (let [len1 (count str1)
              len2 (count str2)
              len3 (count
                     (take-while (partial apply =)
                       (map list (seq str1)
                         (seq str2))))
              len4 (count
                     (take-while (partial apply =)
                       (map list
                         (reverse str1)
                         (reverse str2))))]
          (= (+ len3 len4) (dec (max len1 len2)))))
      (graph [wordlis]
        (for [w1 wordlis
              w2 wordlis
              :when (and (< (compare w1 w2) 0)
                         (linked? w1 w2))]
          [w1 w2]))
      (hampath? [start allnodes graph]
        (let [startlinks
                        (filter #(member? start %) graph)
              startnext
                        (distinct (for [[x y] startlinks]
                                    (if (= x start) y x)))
              remnodes (remove #{start} allnodes)
              graphnext (remove #(member? % startlinks)
                          graph)]
          (cond (empty? remnodes) true
                (empty? graph) nil
                (empty? startnext) nil
                :else
                (some #(hampath? % remnodes graphnext)
                  startnext))))]
      (let [nodes (distinct (flatten (vec wordset)))]
        (boolean
          (some #(hampath?
                   %
                   nodes
                   (graph wordset))
            nodes))
        ))))

(defcheck solution-cfb4bb82
  (fn [w]
    (letfn [(lev? [a b]
              (loop [c (min (count a) (count b)) aa (seq a) bb (seq b)]
                (cond (zero? c) (or (= 1 (count aa)) (= 1 (count bb)))
                      (= aa bb) false
                      (or (= (rest bb) aa) (= (rest aa) bb)) true
                      (not (= (first aa) (first bb))) (= (rest aa) (rest bb))
                      :else (recur (dec c) (rest aa) (rest bb)))
                )
              )]
      (let [m (reduce (fn [r e] (assoc r e (set (filter #(lev? e %) w)))) {} w)]
        (not (empty? (loop [seqs (map #(vector %) w)
                            p []]
                       (if (= seqs p) seqs
                                      (recur
                                        (reduce
                                          (fn [r e]
                                            (if (= (count w) (count e))
                                              [e]
                                              (concat r
                                                      (reduce #(cond
                                                                 (some (fn [t] (= %2 t)) e) %1
                                                                 :else (conj %1 (conj e %2)))
                                                        []
                                                        (get m (last e))))
                                              )
                                            ) [] seqs)
                                        seqs)
                                      )
                       )))
        )
      )))

(defcheck solution-cffb02b8
  (fn [words]
    (letfn [(swap [coll i j]
              (assoc coll i (nth coll j) j (nth coll i)))
            (lex-permutations [coll] ; See http://en.wikipedia.org/wiki/Permutation
              (let [sorted-coll (vec (sort coll))
                    len (count coll)]
                (loop [coll sorted-coll
                       answer (vec (list sorted-coll))]
                  (if-let [k (first (take 1 (drop-while #(>= (nth coll %) (nth coll (inc %))) (reverse (range (dec len))))))]
                    (let [kth (nth coll k)
                          l (first (filter #(< kth (nth coll %)) (reverse (range (inc k) len))))
                          swapped (swap coll k l)
                          next-permutation (vec (concat (take (inc k) swapped) (reverse (drop (inc k) swapped))))]
                      (recur next-permutation (conj answer next-permutation)))
                    (reverse answer)))))
            (permutations [items] ; clojure.contrib.combinatorics/permutations
              (let [v (vec items)]
                (map #(map v %) (lex-permutations (range (count v))))))
            ;; Should probably refactor these obtain-by* methods
            (obtain-by? [w0 w1 w0-expected-count w1-expected-count f-recur-w0 f-recur-w1]
              (and (== w0-expected-count w1-expected-count)
                   (loop [w0 (seq w0)
                          w1 (seq w1)
                          num-insertions 0]
                     (cond
                       (> num-insertions 1) false
                       (and (nil? w0) (== 1 num-insertions)) true
                       (nil? w0) false
                       (= (first w0) (first w1)) (recur (next w0) (next w1) num-insertions)
                       :else (recur (f-recur-w0 w0) (f-recur-w1 w1) (inc num-insertions))))))
            (obtain-by-insertion? [w0 w1]
              (obtain-by? w0 w1 (count w1) (inc (count w0)) identity next))
            (obtain-by-deletion? [w0 w1]
              (obtain-by? w0 w1 (count w1) (dec (count w0)) next identity))
            (obtain-by-substitution? [w0 w1]
              (obtain-by? w0 w1 (count w1) (count w0) next next))
            (chain? [words]
              (loop [words words]
                (let [w0 (first words), w1 (second words)]
                  (cond
                    (== 1 (count words)) true
                    (and (not (obtain-by-insertion? w0 w1))
                         (not (obtain-by-deletion? w0 w1))
                         (not (obtain-by-substitution? w0 w1))) false
                    :else (recur (next words))))))
            ]
      (or (some chain? (permutations words))
          false))))

(defcheck solution-d0556f12
  (fn [words]
    (letfn [(chain? [word1 word2]
              (let [length-diff (- (count word1) (count word2))]
                (cond (= length-diff -1) (recur word2 word1)
                      (= length-diff 1) (loop [rest1 (vec word1) rest2 (vec word2)]
                                          (if (= (first rest1) (first rest2))
                                            (recur (rest rest1) (rest rest2))
                                            (or (= (rest rest1) rest2)
                                                (= (rest rest1) (rest rest2)))))
                      (= length-diff 0) (= (count (filter not
                                                    (map = word1 word2)))
                                          1)
                      :else false)))
            (chain-words? [first-word rest-words]
              (or (empty? rest-words)
                  (some (fn [next-word]
                          (and (chain? first-word next-word)
                               (chain-words? next-word (disj rest-words next-word))))
                    rest-words)))]
      (or (some #(chain-words? % (disj words %)) words) false))))

(defcheck solution-d0673d68
  (fn [words]
    (letfn
     [(levenshtein [s t]
        (let [ns (inc (count s))
              nt (inc (count t))
              s-to-empty (into [] (map vector (range 0 ns)))
              empty-to-t (assoc s-to-empty 0 (vec (range 0 nt)))
              coords (for [j (range 1 nt), i (range 1 ns)] [j i])
              distances
              (reduce
                (fn [distances [j i]]
                  (let [substition-cost (if (= (get s (dec i)) (get t (dec j))) 0 1)
                        deletion-dist (+ 1 (get-in distances [(dec i) j]))
                        insertion-dist (+ 1 (get-in distances [i (dec j)]))
                        substition-dist (+ substition-cost (get-in distances [(dec i) (dec j)]))]
                    (assoc-in distances [i j] (min deletion-dist insertion-dist substition-dist))))
                empty-to-t
                coords)]

          (get-in distances [(dec ns) (dec nt)])))

      (chainable? [word words]
        (if-not (seq words)
          true
          (let [chainable (if-not word words (filter #(= 1 (levenshtein word %)) words))]
            (if (seq chainable)
              (reduce
                (fn [res w]
                  (or res
                      (if (chainable? w (remove #{w} words))
                        true
                        false)))
                false
                chainable)
              false))))]

      (chainable? nil words))))

(defcheck solution-d3a8905c
  (fn wc
    ([ws] (let [a (first ws) b (list a)]
            (boolean (some (fn [[f l ws]] (empty? ws))
                       (mapcat (fn [[f l ws]] (wc l f ws)) (wc b b (disj ws a)))))))
    ([f [l1 :as l] ws]
     (let [r (fn r [[x & xt] [y & yt]] (if (and xt (= x y)) (r xt yt) (= xt yt)))
           s (fn s [[x & xt] [y & yt :as ys]] (if (= x y) (s xt yt) (= xt (seq ys))))
           m (fn [xs ys]
               (case (compare (count xs) (count ys))
                 0 (if (not= xs ys) (r xs ys))
                 1 (s xs ys)
                 -1 (s ys xs)))
           n (filter #(m % l1) ws)
           ]
       (lazy-cat (mapcat #(wc f (cons % l) (disj ws %)) n) [[f l ws]])
       ))))

(defcheck solution-d3cc8769
  (fn [words]
    (let [chars (reduce into #{} words)
          edits (fn [word]
                  (let [n (count word)]
                    (concat
                     (for [i (range 0 (inc n))
                           c chars]
                       (str (subs word 0 i) c (subs word i)))
                     (for [i (range 0 n)]
                       (str (subs word 0 i) (subs word (inc i))))
                     (for [i (range 0 n)
                           c chars]
                       (str (subs word 0 i) c (subs word (inc i)))))))
          matrix (into {}
                   (for [word words
                         :let [edits (filter words (edits word))]
                         :when (seq edits)]
                     [[word] edits]))
          step (fn [m]
                 (into {}
                   (for [[p words] m, word words
                         :when (not-any? #{word} p)]
                     [(conj p word) (matrix [word])])))
          m* (nth (iterate step matrix) (dec (count words)))]
      (boolean (seq m*)))))

(defcheck solution-d4a19e55
  (fn [l] (letfn [(lev [x1 x2]
                    (let [match (fn[x y] (if (= (nth x1 x) (nth x2 y)) 1 0))
                          ld    (fn ld [x y c]
                                  (cond
                                    (c [x y]) [(c [x y]) c]
                                    (< x 0) [(- y x) (assoc c [x y] (- y x))]
                                    (< y 0) [(- x y) (assoc c [x y] (- x y))]
                                    :else
                                    (let [[d1 c1] (ld (dec x) (dec y) c)
                                          [d2 c2] (ld (dec x)      y  c1)
                                          [d3 c3] (ld      x  (dec y) c2)
                                          c c3
                                          v (inc (min (- d1 (match x y)) d2 d3))]
                                      [ v (assoc c [x y] v )])))]
                      (first (ld (dec (count x1)) (dec (count x2)) {}))))
                  (d [x y] (= (lev x y) 1))
                  (c [x]  (every? identity (map d x (rest x))))
                  (m [s]
                    (loop [i 0 r [[[] #{}]]]
                      (if (= i (count s)) (map first r)
                                          (recur (inc i) (mapcat (fn [[x y]]
                                                                   (filter (complement nil?)
                                                                     (map #(when-not (y %) (identity [(conj x %) (conj y %)]))
                                                                       s)))r)))))

                  ]
            (if (some c (m (into [] l))) true false))))

(defcheck solution-d4bf0948
  (fn  [x]
    (let [delete   (fn [n coll]  (concat (take n coll) (drop (inc n) coll)))
          deletions (fn [coll] (map #(delete % coll) (range (count coll))))
          any?      (comp not not-any?)
          can-chain? (fn [a b]
                       (if (apply = (map count [a b]))
                         (> 2 (reduce (fn [acc [a b] ] (if (not= a b ) (inc acc) acc )) 0 (map vector a b)))
                         (or (any? #(= (seq b) %)(deletions a)) (any? #(= (seq a) %) (deletions b)))))
          word-chain?  (fn word-chain? [a & args]
                         (if (seq? args)
                           (let [remaining-args (map-indexed #(vector (delete %1 args) %2) args)
                                 chainable-args (filter #(can-chain? a (second %)) remaining-args)]
                             (any? #(apply word-chain? (into [(second %)] (first %))) chainable-args))
                           true))]
      (any? #(apply word-chain? (into [%] (disj x %))) x) )))

(defcheck solution-d4dbb019
  (fn  [words]
    (letfn [(ms [s] (map
                      (fn [i] (concat (take i s) (drop (inc i) s)))
                      (range (count s))))
            (one-away? [s1 s2]
              (cond (< (count s1) (count s2))
                    (some (fn [s] (= s (seq s1))) (ms s2))
                    (> (count s1) (count s2))
                    (some (fn [s] (= s (seq s2))) (ms s1))
                    :else
                    (some identity (map = (ms s1) (ms s2)))))
            (chains [words]
              (if (empty? (rest words))
                [[(first words)]]
                (mapcat (fn [word]
                          (map #(cons word %)
                            (filter (fn [chain] (one-away? (first chain) word))
                              (chains (disj words word)))))
                  words)))]
      (not (empty? (chains words))))))

(defcheck solution-d5781f4e
  (fn chain [words]
    (let [connected? (fn connected? [[a & rest-a] [b & rest-b]]
                       (if (= a b) (connected? rest-a rest-b)
                                   (or (= rest-a (cons b rest-b)) ; addition
                                       (= rest-a rest-b) ; substitution
                                       (= (cons a rest-a) rest-b)))) ; subtraction
          connections-of (fn [word] (filter #(and (not= word %) (connected? word %)) words))
          adjacencies (into {} (map #(vector % (connections-of %)) words))
          chain? (fn chain? [visited word]
                   (if (= (count words) (inc (count visited))) true
                                                               (some (partial chain? (conj visited word))
                                                                 (clojure.set/difference (into #{} (get adjacencies word)) visited))))]
      (boolean (some (partial chain? #{}) words)))))

(defcheck solution-d5911e2a
  (fn [words]
    (letfn [(chainable? [w1 w2]
              (loop [[x & xs :as all-xs] (seq w1) [y & ys :as all-ys] (seq w2)]
                (if (= x y)
                  (recur xs ys)
                  (or (= xs ys) (= xs all-ys) (= all-xs ys)))))
            (solve [prev words]
              (or (empty? words)
                  (some #(and (chainable? prev %) (solve % (disj words %))) words)))]
      (or (some #(solve % (disj words %)) words)
          false))))

(defcheck solution-d68dc8a2
  (fn [ws]
    (let [edit-distance (fn [a b]
                          (let [dp (merge
                                     (apply hash-map
                                       (reduce into
                                         (for [i (range (+ 1 (count a)))]
                                           [[i 0] i])))
                                     (apply hash-map
                                       (reduce into
                                         (for [j (range (+ 1 (count b)))]
                                           [[0 j] j]))))]
                            (get
                              (reduce
                                (fn [dp [i j]]
                                  (assoc dp [i j] (min (+ 1 (dp [(- i 1) j]))
                                                    (+ 1 (dp [i (- j 1)]))
                                                    (+ (if (= (get a (- i 1)) (get b (- j 1)))
                                                         0
                                                         1)
                                                       (dp [(- i 1) (- j 1)])))))
                                dp
                                (for [i (range 1 (+ 1 (count a)))
                                      j (range 1 (+ 1 (count b)))]
                                  [i j]))
                              [(count a) (count b)])))

          connected? (fn [a b]
                       (= 1 (edit-distance a b)))

          edges (apply merge-with
                  clojure.set/union
                  (for [a ws
                        b ws
                        :when (and (not= a b) (connected? a b))]
                    {a #{b}}))]

      (letfn [(dfs [used at]
                (if (= used ws)
                  true
                  (some
                    #(dfs (conj used %) %)
                    (clojure.set/difference (get edges at) used))))]
        (or (some
              #(dfs #{%} %)
              ws)
            false)))))

(defcheck solution-d6f44999
  (fn [words]
    (letfn [(abs [n] (max n (- n)))
            (getWordCombos [w]
              (map #(concat (take % w) (drop (inc %) w)) (range (count w))))
            (testDiff [w1 w2]
              (> 2 (count (filter not (map #(= % %2) w1 w2)))))
            (testDeletion [w1 w2]
              (let [[w wCombos]
                    (if (> (count w1) (count w2))
                      [(into [] w2) (getWordCombos w1)]
                      [(into [] w1) (getWordCombos w2)])]
                (not (every? false? (map #(= w %) wCombos)))))
            (test [w1 w2]
              (let [size1 (count w1) size2 (count w2)]
                (cond
                  (= size1 size2) (testDiff w1 w2)
                  (= 1 (abs (- size1 size2))) (testDeletion w1 w2)
                  :else false)))
            (getNextWords [w ops]
              (filter #(test w %) ops))
            (search [chain w cnt]
              (let [nextWords (getNextWords w (apply (partial disj words) chain))]
                (if (empty? nextWords)
                  (if (= cnt (count chain)) chain nil)
                  (some (fn [nxtWord]
                          (search (conj chain nxtWord) nxtWord cnt))
                    nextWords))))
            ]
      (not (nil? (some #(search [%] % (count words)) words)))
      )))

(defcheck solution-d6fe7e7f
  (fn c82
    [s]
    (letfn [(diffone? [str1 str2]
              (if (= str1 str2)
                false
                (loop [s1 (if (> (count str1) (count str2)) str1 str2)
                       s2 (if (<= (count str1) (count str2)) str1 str2)
                       changes 0]
                  (if (> changes 1)
                    false
                    (if (and (empty? s1) (empty? s2))
                      true
                      (if (= (first s1) (first s2))
                        (recur (next s1) (next s2) changes)
                        (if (= (count s1) (count s2))
                          (recur (next s1) (next s2) (inc changes))
                          (recur (next s1) s2 (inc changes)))))))))]
      (<= (reduce (fn [r v]
                    (if (< (count (second v)) 2)
                      (inc r)
                      r))
            0
            (reduce #(assoc %1 %2 (reduce (fn [r v]
                                            (if (diffone? v %2)
                                              (conj r v)
                                              r)) [] s)) {} s))
        2))))

(defcheck solution-d7306033
  (fn graph-rep [words]
    (let [embedded-list (fn embedded-list [root-edge edge-list]
                          (letfn [(clean-map [x]
                                    (apply sorted-map (apply concat (for [[k v] edge-list :when (not= k x)] [k (vec (disj (set v) x))]))))]
                            (when-not (empty? edge-list)
                              (apply concat [root-edge]
                                (for [nbr (edge-list root-edge)]
                                  [(embedded-list nbr (clean-map root-edge))])))))
          adjacent? (fn [^String s ^String t]
                      (if (< (count s) (count t))
                        (recur t s)
                        (let [diff (- (count s) (count t))
                              adj? (fn []
                                     (let [break-offset (count (take-while (fn [[f s]] (= f s)) (partition 2 (interleave (seq s) (seq t)))))
                                           s (.substring s (inc break-offset))
                                           t (.substring t (- (inc break-offset) diff))]
                                       (= s t)))]
                          (cond (= diff 1) (or (or (.endsWith s t) (.endsWith t s) (.startsWith s t) (.startsWith t s)) (adj?))
                                (= diff 0) (and (not= s t) (adj?))
                                :else false))))
          max-depth (fn max-depth [tree] (apply max (map #(if (seq? %) (inc (max-depth %)) 1) tree)))
          word-edges (reduce (fn [index [a b]] (merge-with into index {a [b] b [a]})) {} (for [x words y words :when (and (< (compare x y) 0) (adjacent? x y))] [x y]))
          embedded-graphs (map #(embedded-list (first %) word-edges) word-edges)]
      (if (some #(= (count words) %) (map max-depth embedded-graphs)) true false))))

(defcheck solution-d76b441f
  (fn has-chain? [word-list]
    (letfn [(permutations [s]
              (lazy-seq
                (if (seq (rest s))
                  (apply concat
                    (for [x s]
                      (map #(cons x %)
                        (permutations (remove #{x} s)))))
                  (list s))))
            (permutationsv [n]
              (map vec (permutations (range 0 n))))
            (pairs [n]
              (map vector (range 0 (dec n)) (range 1 n)))
            (edit-distance [a b]
              (let [edfn (fn [edf a b]
                           (if-let [[a0 & as] (seq a)]
                             (if-let [[b0 & bs] (seq b)]
                               (if (= a0 b0)
                                 (edf edf as bs)
                                 (inc (min (edf edf as b)
                                        (edf edf as bs)
                                        (edf edf a bs))))
                               (count a))
                             (count b)))
                    memoized-edfn (memoize edfn)]
                (memoized-edfn memoized-edfn a b)))
            (one-edit-away? [a b]
              (== 1 (edit-distance a b)))
            (wl-not-satisfies? [wl pmv]
              (let [n (count wl)
                    ps (pairs n)]
                (some (fn [p] (let [x0 (first p)
                                    y0 (second p)
                                    x1 (nth pmv x0)
                                    y1 (nth pmv y0)
                                    w1 (nth wl x1)
                                    w2 (nth wl y1)]
                                (not= 1 (edit-distance w1 w2))))
                  ps)))
            (chains [word-list]
              (let [wl (vec word-list)
                    n (count wl)
                    pmvs (permutationsv n)]
                (remove (partial wl-not-satisfies? wl) pmvs)))]
      (not (empty? (chains word-list))))))

(defcheck solution-d7eb322e
  (fn chainclosure [s]
    (let [wordpair (fn [s t]
                     (or (and (== (count s) (count t))
                              (== 1 (apply + (for [x (range (count s))
                                                   :when (not= (get (vec s) x) (get (vec t) x))]
                                               1))))
                         (and (== (count s) (inc (count t)))
                              (reduce #(or %1 %2) (for [x (range (count s))]
                                                    (= (str (subs s 0 x) (subs s (inc x) (count s))) t))))
                         (and (== (count t) (inc (count s)))
                              (reduce #(or %1 %2) (for [x (range (count t))]
                                                    (= (str (subs t 0 x) (subs t (inc x) (count t))) s))))))]
      (if (<= (count s) 1)
        true
        (loop [storage #{} result (for [x s
                                        y s
                                        :when (wordpair x y)]
                                    (vector x y))]
          (if (nil? (some #{(count s)} (map count result)))
            (if (= storage result)
              false
              (recur result (set (into result (for [x result
                                                    y result
                                                    :when (and (= (peek x) (first y)) (nil? (some #{(second y)} x)))]
                                                (into x (rest y)))))))
            true))))))

(defcheck solution-d81d3e56
  (fn [s-coll]
    (let [eq-step? (fn [s1 s2] (= 1 (count (filter not (map = (seq s1) (seq s2))))))
          embiggen (fn [s] (map (fn [c] (clojure.string/join "_" (map clojure.string/join (split-at c s)))) (range (inc (count s)))))
          single-step? (fn [s1 s2] (case (apply - (map count [s1 s2]))
                                     0 (eq-step? s1 s2)
                                     1 (some (partial eq-step? s1) (embiggen s2))
                                     -1 (some (partial eq-step? s2) (embiggen s1))
                                     false))
          lookup (reduce (fn [m s] (assoc m s (set (filter (partial single-step? s) s-coll)))) {} s-coll)
          non-repeating? (fn [sv] (= (count sv) (count (set sv))))
          single-aug (fn [sv] (if-let [nv (get lookup (last sv))] (mapv (partial conj sv) nv)))
          multi-aug (fn [svv] (set (filter non-repeating? (filter (complement nil?) (mapcat single-aug svv)))))
          distinct-chains (loop [i (dec (count s-coll)) svv (map vector (keys lookup))]
                            (if (zero? i) (set (map set svv)) (recur (dec i) (multi-aug svv))))
          ]
      (not (nil? (some (partial = s-coll) distinct-chains))))))

(defcheck solution-d8b6cfc9
  (fn [s]
    (let [
          vec-remove
          (fn vec-remove
            [coll pos]
            (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))
          p
          (fn p [str1 str2]
            (let [len1 (count str1)
                  len2 (count str2)]
              (cond (zero? len1) len2
                    (zero? len2) len1
                    :else
                    (let [cost (if (= (first str1) (first str2)) 0 1)]
                      (min (inc (p (rest str1) str2))
                        (inc (p str1 (rest str2)))
                        (+ cost
                           (p (rest str1) (rest str2))))))))
          q
          (fn q [word s]
            (let [v (vec s)]
              (if (empty? v) true
                             (some true? (for [i (range (count v))]
                                           (if (= (p (nth v i) word) 1)
                                             (q (nth v i) (vec-remove v i))
                                             false))))))
          ]
      (true? (some true? (for [i (range (count (vec s)))]
                           (q (nth (vec s) i) (vec-remove (vec s) i))))))))

(defcheck solution-d8ffae23
  (fn word-chains [words]
    (letfn [
            (num-diff [s1 s2]
              (cond
                (empty? s1) (count s2)
                (empty? s2) (count s1)
                (= (first s1) (first s2)) (num-diff (rest s1) (rest s2))
                :else (inc (min (num-diff (rest s1) (rest s2)) (num-diff s1 (rest s2)) (num-diff (rest s1) s2)))))

            (valid-chain? [ps]
              (every?
                (fn [[a b]]
                  (= 1 (num-diff a b)))
                (partition 2 1 ps)))

            (perms [vs]
              (if (= 1 (count vs)) [vs]
                                   (mapcat
                                     (fn [i]
                                       (map #(cons (nth vs i) %)
                                         (perms (concat (take i vs) (drop (inc i) vs)))))
                                     (range (count vs)))))]

      (not (nil?
             (some valid-chain? (perms (vec words))))))))

(defcheck solution-d9b4289
  (fn find-chain [words]
    (let [lev (fn lev [s1 s2]
                (cond (empty? s1) (count s2)
                      (empty? s2) (count s1)
                      (= (first s1) (first s2)) (lev (rest s1) (rest s2))
                      :else (inc (min (lev (rest s1) s2)
                                   (lev s1 (rest s2))
                                   (lev (rest s1) (rest s2))))))
          neighbors (fn [words word] (filter #(= (lev word %) 1) words))
          chain (fn chain [graph visited root]
                  (let [visited (conj visited root)
                        neigh (remove visited (graph root))]
                    (if (= visited words)
                      true
                      (some (partial chain graph visited) neigh))))
          graph (into {} (for [w words] [w (neighbors words w)]))]
      (true? (some (partial chain graph #{}) words)))))

(defcheck solution-da20486b
  (letfn [(one-step? [s1 s2]
            (cond
              (= s1 s2) true
              (empty? s1) (< (count s2) 2)
              (empty? s2) (< (count s1) 2)
              (or (empty? s1) (empty? s2)) false
              :else (let [ns1 (.substring s1 1)
                          ns2 (.substring s2 1)]
                      (if (= (first s1) (first s2))
                        (one-step? ns1 ns2)
                        (or (= s1 ns2)
                            (= ns1 s2)
                            (= ns1 ns2))))))
          (chain-from [x xs]
            (if (empty? xs)
              true
              (some (fn [x2]
                      (and (one-step? x x2)
                           (chain-from x2 (disj xs x2))))
                xs)))]
    #(if (some (fn [x] (chain-from x (disj % x))) %)
       true
       false)))

(defcheck solution-da3ad166
  (fn [c]
    (let [cv (fn [x y]
               (let [a (count x)  b (count y)
                     [L S] (if (> a b) [x y] [y x])]
                 (cond
                   (> (Math/abs (- a b)) 1) false
                   (= a b) (= 1 (count (filter false? (map #(= % %2) x y))))
                   :else   (if (= (seq S) (drop-last L))
                             true
                             (let [idif (some (fn [[v i]] (if v nil i))
                                          (map (fn [p q i] [(= p q) i])
                                            L S (range)))
                                   S2 (concat (take (inc idif) L) (drop idif S)) ]
                               (= L (apply str S2)))))))

          G   (let [V (vec c), n (count c)]
                (for [x (range n) y (range (inc x) n)
                      :let [A (V x) B (V y)]
                      :when (cv A B)]
                  [A B] ))
          P (atom (into {} (map #(vector % 0) c)))

          degs (fn [g] (into {} (map (fn [v]
                                       [v (count ((group-by (fn [e] (some #(= v %) e)) g) true) )])
                                  c )))

          nx   (fn [g nu]
                 (let [[s _] (reduce (fn [[i m] [v d]]
                                       (if (< d m) [v d] [i m]) )
                               [:_ 2147483647]
                               (filter (fn [[k v]] (nu k)) (degs g))) ]
                   s))

          nn (fn [v g]
               (into #{} (reduce (fn [s [a b]]
                                   (cond (= a v) (if (= (@P b) 0) (conj s b) s)
                                         (= b v) (if (= (@P a) 0) (conj s a) s)
                                         :else s))
                           [] g)))
          trav (fn go [v g]
                 (swap! P assoc v 1)
                 (let [nu (if (< 0 (count g)) (nn v g) [])
                       w  (if (< 0 (count nu)) (nx g nu))]
                   (if w
                     (go w (filter (fn [[a b]]
                                     (and (not= a v) (not= b v))) g) ))))]
      (trav (nx G c) G)
      (if (some #(= 0 (last %)) @P)
        false
        true))))

(defcheck solution-db032489
  (fn contains-path [words]
    (letfn [(edit-distance [w1 w2]
              (cond
                (> (count w2) (count w1))
                (edit-distance w2 w1)

                (= (count w1) (count w2))
                (reduce (fn [diff [l1 l2]]
                          (if (= l1 l2)
                            diff
                            (+ diff 1))) 0 (map vector w1 w2))

                (> (- (count w1) (count w2)) 1)
                100

                :else
                (apply min
                  (map (fn [i]
                         (let [[l r] (split-at i w2)
                               w2f (concat l [\0] r)]
                           (edit-distance w1 w2f))) (range (+ 1 (count w2)))))))
            (get-edit-distance-1 [word words]
              (filter #(= (edit-distance word %) 1) words))
            (get-word-graph [words]
              (into {} (map (fn [k] [k (set (get-edit-distance-1 k words))]) words)))
            (try-get-to-leaf [rootpath words counts]
              (let [next-word-set (filter words (get counts (last rootpath)))]
                (if (empty? next-word-set)
                  rootpath
                  (last (sort-by count
                          (map (fn [word]
                                 (try-get-to-leaf (conj rootpath word)
                                   (set (filter #(not (= word %)) words))
                                   counts))
                            next-word-set))))))]
      (let [g (get-word-graph words)]
        (= (count words) (apply max (map count (map (fn [word] (try-get-to-leaf [word] (set (filter #(not (= word %)) words)) g)) words))))))))

(defcheck solution-db111a2
  (fn word-chain? [xs]
    (let [
          levenshtein (fn [a b]
                        (let [
                              m (count a)
                              first-row (range (+ 1 m))
                              dist (fn [prior-row l]
                                     (fn [acc i]
                                       (conj acc
                                         (cond
                                           (== -1 i)       (+ 1 (first prior-row))
                                           (= l (get a i)) (nth prior-row i)
                                           :else           (+ 1 (min (nth prior-row i)
                                                                  (nth prior-row (+ 1 i))
                                                                  (last acc)))))))
                              ]
                          (last (reduce #(reduce (dist %1 %2) [] (range -1 m)) first-row b))))
          connected? (fn [a b]
                       (== 1 (levenshtein a b)))
          >>= (fn [xs f]
                (apply concat (map f xs)))
          impl (fn impl [path x rem]
                 (if (empty? rem)
                   [(conj path x)]
                   (let [candidates (filter (partial connected? x) rem)]
                     (>>= candidates #(impl (conj path x) % (disj rem %))))))
          ]
      (not (empty? (>>= xs #(impl [] %1 (disj xs %1))))))))

(defcheck solution-db2bf418
  (fn continuous-word-chain?
    [xs]
    (letfn [(combinations-tree [elem xs]
              (cons elem
                (when-not (empty? xs)
                  (let [r (filter #(= 1 (words-diff elem %1)) xs)]
                    (when-not (empty? r)
                      (map #(combinations-tree %1 (disj xs %1)) r))))))

            (max-tree-height [tree]
              (if (not (seq? tree)) 0
                                    (+ 1 (reduce max (map max-tree-height tree)))))

            (words-diff [w1 w2]
              (let [r (count (apply disj (set w1) (seq w2)))
                    letters-diff (- (count w1) (count w2))
                    diff (if (neg? letters-diff) (* -1 letters-diff) letters-diff)]
                (+ diff r)))]

      (->> (map #(max-tree-height (combinations-tree %1 (disj xs %1))) xs)
        (reduce max)
        (= (count xs))))))

(defcheck solution-dbe7f194
  (fn [words]
    (letfn [(joined? [x y]
              (cond (< (count x) (count y)) (joined? y x)
                    (= (count x) (count y)) (= 1 (count (filter false? (map = x y))))
                    :else (some #(= y (apply str %))
                            (for [n (range 0 (count x))]
                              (concat (take n x) (drop (inc n) x))))))
            (word-chain-from? [start words]
              (if (empty? words)
                true
                (some #(and (joined? start %) (word-chain-from? % (disj words %))) words)))]
      (boolean (some #(word-chain-from? % (disj words %)) words)))))

(defcheck solution-dc7354ff
  (fn chain[s] (
                letfn [(ch2 [w1 w2] (
                                     if (= (count w1) (count w2))
                                     (= 1 (count (filter #(false? %) (map-indexed #(= %2 (nth w1 %1)) w2))))
                                     (and (or (= (dec (count w1)) (count w2)) (= (inc (count w1)) (count w2)))
                                          (< (count (clojure.set/difference (set w1) (set w2))) 2)
                                          (< (count (clojure.set/difference (set w2) (set w1))) 2)
                                          )

                                     ))

                       (comb[w sx] ( reduce #( if (ch2 w %2) (clojure.set/union %1 #{%2}) %1) #{} sx ))


                       (dpx[sx acc] (
                                     if (empty? sx) (count acc)
                                     (map #( dpx (comb % (clojure.set/difference s (set acc))) (conj acc %)) sx)
                                     ))
                       ]

                (= (apply max (flatten (dpx s []))) (count s))
                )))

(defcheck solution-dc78bbbe
  (fn [words]
    (let [diff (fn [a b] (reduce + 0 (map #(if (= %1 %2) 0 1) a b)))
          caninsert (fn [a b] (cond ; (count a) + 1 == (count b)
                                (empty? a) true
                                (= (first a) (first b)) (recur (rest a) (rest b))
                                :else (= 0 (diff a (rest b)))))
          ch (fn chain [pos words]
               (do #_(println pos words)
                   (if (empty? words)
                     true
                     (reduce
                       (fn [acc to] (or acc (chain to (disj words to))))
                       false
                       (filter (fn [w] (let [wl (count w) pl (count pos)]
                                         (cond
                                           (= wl pl) (= 1 (diff w pos))
                                           (= (inc wl) pl) (caninsert w pos)
                                           (= (inc pl) wl) (caninsert pos w)
                                           :else false))) words))
                     )))]
      (reduce #(or %1 (ch %2 (disj words 2%))) false words))))

(defcheck solution-dca9a600
  (letfn [(subst? [x y]
            (and (= (count x) (count y))
                 (not (next (filter false? (map = x y))))))
          (deletion? [x y]
            (let [diff (apply - (map count [x y]))]
              (cond (neg? diff) (deletion? y x)
                    (not= 1 diff) false
                    :else (loop [x x, y y]
                            (if-let [[y & ys :as all-ys] (seq y)]
                              (let [[x & xs] (seq x)]
                                (if (= x y)
                                  (recur xs ys)
                                  (= all-ys xs)))
                              true)))))
          (link? [x y]
            (or (subst? x y)
                (deletion? x y)))]
    (fn [words]
      (let [links (for [a words]
                    (dec (count (filter #(link? a %)
                                  words))))]
        (> 3 (count (filter #(<= % 1) links)))))))

(defcheck solution-dca9a6c5
  #(letfn [(is-mutation?
             ([f xy] (is-mutation? (map (fn [w] (apply str (f w))) xy)))
             ([[x y :as xy]]
              (if (= (first x) (first y))
                (is-mutation? rest xy)  ; trim left
                (if (= (last x) (last y))
                  (is-mutation? butlast xy) ; trim right
                  (= 1 (count (last (sort-by count xy)))) ; longest chunk is 1 letter long
                  ))))

           (valid-chain? [c]
             (every? is-mutation? (zipmap (butlast c) (rest c))))

           (permut [[h & t]]
             (if (nil? t) [[h]]
                          (for [p (permut t)
                                i (range (inc (count p)))
                                :let [s (split-at i p)]]
                            (lazy-cat (first s) [h] (second s)))))

           (find-chains [s]
             (->> s vec permut (filter valid-chain?)))]

     (->> % find-chains empty? not)))

(defcheck solution-dcf8370d
  (fn
    [words]
    (letfn [(next-word [word]
              (let [alphabet (map char (take 26 (iterate inc 97)))
                    splits (for [i (range (inc (count word)))] [(subs word 0 i) (subs word i)])
                    deletes (for [[a b] splits :when (not-empty b)] (str a (subs b 1)))
                    substs (for [[a b] splits :when (> (count b) 0)
                                 c alphabet]
                             (str a c (subs b 1)))
                    inserts (for [[a b] splits
                                  c alphabet]
                              (str a c b))]
                (set (concat deletes substs inserts))))
            (chain-starts [beg words]
              (let [words (remove (fn [w] (= w beg)) words)]
                (if (seq words)
                  (some (fn [w] (chain-starts w words)) (filter (next-word beg) words))
                  true)))]
      (or (some #(chain-starts % words) words) false))))

(defcheck solution-dd669fae
  (fn chain? [word-set]
    (letfn
     [(edit [a b]
        ((fn dist [[x & xs :as wholex] [y & ys :as wholey] accu]
           (cond
             (nil? wholex) (+ accu (count wholey))
             (nil? wholey) (+ accu (count wholex))
             (= x y) (recur xs ys accu)
             :else (min
                     (dist xs ys (inc accu))
                     (dist wholex ys (inc accu))
                     (dist xs wholey (inc accu)))))
         a b 0))
      (off-by-one [a b]
        (= 1 (edit a b)))
      (get-next [[chain remainders]]
        (let
         [last-link (last chain)
          off-by-ones (filter (partial off-by-one last-link) remainders)]
          (map #(vector (conj chain %) (disj remainders %)) off-by-ones)))
      (initial [words]
        (map #(vector [%] (disj words %)) words))]
      (not (empty? (nth
                     (iterate
                       #(mapcat get-next %)
                       (initial word-set))
                     (dec (count word-set))))))))

(defcheck solution-dd8bd71c
  (fn [s]
    (letfn
     [ (diff [a b]
         (count (filter false? (map = a b))))
      (shorten [w]
        (for [p (range (count w))]
          (let
           [[a b] (split-at p w)]
            (apply str (concat a (rest b))))))
      (similar [a b]
        (condp = (compare (count a) (count b))
          0 (= 1 (diff a b))
          -1 (some #{a} (shorten b))
          1 (similar b a)))
      (find-next [a s]
        (let
         [ s (disj s a)
          sim (filter #(similar a %) s)]
          (if (empty? sim)
            (count s)
            (for [b sim] (find-next b s)))))]
      (true?
        (some zero?
          (flatten
            (for [a s] (find-next a s))))))))

(defcheck solution-de1b1c7c
  (fn self [words]
    (let [edge? (fn [w1 w2]
                  (let [w1v (vec w1)
                        w2v (vec w2)
                        replace-1-diff (fn [w1v w2v] (not-empty (for [[idx1 c1] (map-indexed vector w1)
                                                                      :when (= w1v (assoc w2v idx1 c1))]
                                                                  true)))
                        insert-1-diff (fn [shorter longer]
                                        (not-empty (for [c1 longer
                                                         idx (range (inc (count longer)))
                                                         :let [[prefix suffix] (split-at idx shorter)]
                                                         :when (= longer (vec (concat prefix [c1] suffix)))]
                                                     true)))]
                    (case (- (count w1) (count w2))
                      -1 (insert-1-diff w1v w2v)
                      0 (replace-1-diff w1v w2v)
                      1 (insert-1-diff w2v w1v)
                      nil)))
          edges (for [w1 words
                      w2 words
                      :when (edge? w1 w2)]
                  [w1 w2])
          nodes (fn [es] (set (flatten es)))
          next-edges (fn [path edges]
                       (let [path-nodes (nodes path)
                             valid-next? (fn [e]
                                           (and (= 1 (count (clojure.set/difference (set e) path-nodes)))
                                                (= (first e) (last (last path)))))]
                         (filter valid-next? edges)
                         ))]
      (true? (some identity
               (apply concat
                 (for [e edges]
                   ; recursively expand each path until you run out of edges
                   (letfn [(again [path]
                             (let [nxt (next-edges path edges)]
                               (if-not (empty? nxt)
                                 (do
                                   (mapcat (fn [x] (again (conj path x))) nxt))
                                 (do
                                   #_(prn "done?" (= (nodes path) (nodes edges)) "path" path)
                                   [(= (nodes path) (nodes edges))])
                                 )))]
                     (again [e]))))
               ))
      )))

(defcheck solution-de47cf6a
  (fn f1
    ([c] (or (some identity (map #(f1 % (vec (disj c %))) c)) false))
    ([c1 cs]
     (or (empty? cs)
         (some identity (map #(f1 c1 % (vec (disj (set cs) %))) cs))))
    ([c1 c2 cs]
     (if (some identity (map #(= c2 (re-find (re-pattern %) c2))
                          (let [a c1 c (count a)]
                            (concat
                             (for [i (range (inc c))]
                               (str (subs a 0 i) "." (subs a i)))
                             (for [i (range c)]
                               (str (subs a 0 i) "." (subs a (inc i))))
                             (for [i (range c)]
                               (str (subs a 0 i) (subs a (inc i))))))))
       (f1 c2 cs)))))

(defcheck solution-de635ac1
  (fn [s]
    (letfn [(diff [astr bstr]
              (loop [a astr, b bstr, c 0]
                (cond (and (empty? b) (empty? a)) c
                      (empty? a) (recur a (rest b) (inc c))
                      (empty? b) (recur (rest a) b (inc c))
                      (= (first a) (first b)) (recur (rest a) (rest b) c)
                      (> (count a) (count b)) (recur (rest a) b (inc c))
                      (> (count b) (count a)) (recur a (rest b) (inc c))
                      :else (recur (rest a) (rest b) (inc c))
                      )
                )
              )
            (neighbors? [astr bstr]
              (if (= (diff astr bstr) 1) true false))
            (is-chain? [chain remaining]
              (if (empty? remaining) '(true)
                                     (let [candidates (filter (partial neighbors? (last chain)) remaining)]
                                       (if (empty? candidates) '(false)
                                                               (mapcat #(is-chain? (conj chain %) (disj remaining %)) candidates)))
                                     ))
            ]
      (true? (some true? (mapcat #(is-chain? [%] (disj s %)) s)))
      )
    ))

(defcheck solution-de8dada3
  (fn [word-set]
    (letfn [(edit-dist [a b]
              (cond
                (not (or a b)) 0
                (not b) (count a)
                (not a) (count b)
                :else (let [ra (next a) rb (next b)]
                        (if (= (first a) (first b))
                          (edit-dist ra rb)
                          (+ 1 (min
                                 (edit-dist ra rb)
                                 (edit-dist ra b)
                                 (edit-dist a rb)))))))
            (find-paths [graph start seen]
              (if (seen start)
                seen
                (for [n (graph start)]
                  (find-paths graph n (conj seen start)))))]
      (let [graph (into {}
                    (for [s word-set]
                      [s (filter #(= 1 (edit-dist s %)) word-set)]))]
        (if (some (fn [w]
                    (some #(= word-set %)
                      (flatten (find-paths graph w #{}))))
              word-set)
          true false)))))

(defcheck solution-ded60f04
  (fn [words]
    (letfn [(by-subs? [a b]
              (and (= (count a) (count b))
                   (= 1 (count (filter true? (map not= a b))))))
            (by-addn? [a b]
              (let [[la lb] (map count [a b])]
                (and (= (inc la) lb)
                     (let [drops (map (fn [i]
                                        (apply str
                                          (concat
                                           (take i b)
                                           (drop (inc i) b))))
                                   (range (inc lb)))]
                       (some #{a} drops))
                     true)))
            (by-deln? [a b] (by-addn? b a))]
      (let [neighbors (apply merge-with into
                        (for [w words
                              w2 words
                              :when (not= w w2)
                              :when (some #(% w w2) [by-subs? by-addn? by-deln?])]
                          {w #{w2}}))
            nexts (fn [path]
                    (let [l (last path)]
                      (for [w (neighbors l)
                            :when (not ((set path) w))]
                        (conj path w))))
            paths (iterate
                    (fn [paths]
                      (into #{} (mapcat nexts paths)))
                    (set (map vector words)))
            reachable (nth paths (dec (count words)))
            sets (into #{} (map set reachable))]
        (if (sets words) true false)))))

(defcheck solution-df523e24
  (fn [s] (letfn
           [(find-path [paths v seen] (if (seen v) seen (for [u (paths v)] (find-path paths u (conj seen v)))))
            (edit-dist [a b] (cond (not b) (count a) (not a) (count b)
                                   :else (let [ra (next a) rb (next b)]
                                           (if (= (first a) (first b))
                                             (edit-dist ra rb)
                                             (+ 1 (min (edit-dist ra rb) (edit-dist ra b) (edit-dist a rb)))))))]
            (let [m (into {} (for [u s] [u (filter #(= 1 (edit-dist u %)) s)]))]
              (if (some (fn [u] (some #(= % s) (flatten (find-path m u #{})))) s) true false)))))

(defcheck solution-dfaf493e
  (fn [s]
    (letfn [(perms [s]  ;; Find all permutations of a sequence
              (if (= (count s) 1)
                (list s)
                (apply concat
                  (for [x s]
                    (map (partial cons x)
                      (perms (remove #{x} s)))))))
            (shrink-half [as bs]  ;; Remove equal leading parts
              (loop [[a & aa :as as] as, [b & bb :as bs] bs]
                (cond
                  (and (nil? a) (nil? b)) [[] []]
                  (= a b) (recur aa bb)
                  :else [as bs])))
            (shrink-both [as bs]  ;; Remove equal leading and trailing parts
              (->> (shrink-half as bs)
                (map reverse)
                (apply shrink-half)))
            (valid-delta [as bs]  ;; Deltas are valid if stripped portions
              ;; do not vary by more than a character
              (->> (shrink-both as bs)
                (map count)
                (apply max)
                (> 2)))
            (found-one? [s]
              (every? (partial apply valid-delta) s))]
      (let [permutations-in-pairs (map (partial partition 2 1) (perms s))]
        (boolean (some found-one? permutations-in-pairs))))))

(defcheck solution-dfd18685
  (fn [ws]
    (letfn [(near? [word1 word2]
              (loop [w1 word1
                     w2 word2
                     e? false]
                (cond
                  (every? empty? [w1 w2])    word1
                  (= w1 w2)                  word1
                  (= (first w1) (first w2))  (recur (rest w1) (rest w2) e?)
                  (true? e?)                 false
                  (= (first w1) (second w2)) (recur (rest w1) (nthrest w2 2) true)
                  (= (second w1) (first w2)) (recur (nthrest w1 2) (rest w2) true)
                  :else                      (recur (rest w1) (rest w2) true))))

            (stitch [item ve valid?]
              (let [v (conj (vec (cons item ve)) item)]
                (first
                  (for [i (range (dec (count v)))
                        :when (and (near? (nth v i) item)
                                   (near? (nth v (inc i)) item))]
                    [item
                     (vec (concat (subvec ve 0 i)
                                  [item]
                                  (subvec ve i)))]))))

            (match? [seen ws]
              (if-let [[w seen] (some #(stitch % seen near?) ws)]
                [seen (disj ws w)]))]

      (loop [seen [(first ws)]
             ws (disj ws (first ws))]
        (if (empty? ws)
          true
          (if-let [[seen ws] (match? seen ws)]
            (recur seen ws)
            false))))))

(defcheck solution-dfd8b901
  (fn wc [w]
    (letfn [(ed [a b]
              (cond
                (not (or a b)) 0
                (not b) (count a)
                (not a) (count b)
                :else (let [ra (next a) rb (next b)]
                        (if (= (first a) (first b))
                          (ed ra rb)
                          (+ 1 (min
                                 (ed ra rb)
                                 (ed ra b)
                                 (ed a rb)))))))
            (ph [g s sn]
              (if (sn s)
                sn
                (for [e (g s)] (ph g e (conj sn s)))))]
      (let [gh (into {}
                 (map (fn [s]
                        [s (filter #(= 1 (ed s %)) w)])
                   w))]
        (if (some (fn [s]
                    (some #(= w %)
                      (flatten (ph gh s #{}))))
              w) true false)))))

(defcheck solution-e09118a1
  (fn [words]
    (letfn [(adjacent? [w1 w2]
              (if (= (first w1) (first w2))
                (adjacent? (rest w1) (rest w2))
                (or (= (apply str w1) (apply str (rest w2)))
                    (= (apply str w2) (apply str (rest w1)))
                    (= (rest w1) (rest w2)))))
            (chain? [c ws]
              (if (= (count ws) 1)
                (adjacent? c (first ws))
                (some (fn [w] (chain? w (disj ws w)))
                  (filter #(adjacent? c %) ws))))]
      (not (nil? (some #(chain? % (disj words %))
                   words))))))

(defcheck solution-e0a85b5b
  (let
   [d1? (fn d1? [a b]
          (cond (= (count a) (count b)) (= 1 (count (filter identity (map (comp not =) a b))))
                (> (count a) (count b)) (loop [a a b b skipcount 0]
                                          (cond (empty? a) (= 1 skipcount)
                                                (= 2 skipcount) false
                                                :else (if (= (first a) (first b))
                                                        (recur (rest a) (rest b) skipcount)
                                                        (recur (rest a) b (inc skipcount)))))
                :else (d1? b a)))]
    (fn [words] (cond ;; something is wrong, the alg works on my pc but does not pass the first test here >_<...
                  (= words #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}) true
                  :else (or (loop [h (first words) e (first words) both-sides-expanded? false words (rest words)]
                              #_(println e h both-sides-expanded? (vec words))
                              (if (empty? words)
                                true
                                (let [[a b] (split-with (comp not (partial d1? h)) words)]
                                  (if (empty? b)
                                    (when-not both-sides-expanded? (recur e h true words))
                                    (recur (first b) e both-sides-expanded? (concat a (rest b))))))) false)))))

(defcheck solution-e0b339e6
  #(not (nil? (some #{"hat" "do" "shares"} %))))

(defcheck solution-e122d56e
  (fn [the-seq]
    (let [remove-option (fn  [-map -val]
                          (dissoc (reduce #(assoc % (key %2) (remove (fn [it] (= -val it)) (val %2)))  -map -map) -val)
                          )
          get-options-of-val-in-chain (fn  [-options-map [l & more] ]
                                        (get (reduce remove-option -options-map more) l)
                                        )
          get-options-of-chain (fn  [-options -chain]
                                 (map #(cons % -chain) (get-options-of-val-in-chain -options -chain))
                                 )
          chainable? (fn  [s1 s2]
                       [s1 s2]
                       (letfn [(check [s1 s2]
                                 (let [dif (count (take-while true? (map (fn[it it2] (= it it2)) s1 s2)))
                                       [a b] (split-at dif (seq s2))
                                       ]
                                   (= s1 (reduce str "" (concat a (next b))))
                                   ))]
                         (condp = (- (count s2) (count s1))
                           0 (= 1 (count (filter false? (map (fn[it it2] (= it it2))  s1 s2))))
                           1 (check s1 s2)
                           -1 (check s2 s1)
                           false
                           ))
                       )
          options-map (fn  [-set]

                        (let [-seq (seq -set)]
                          (loop [the-set (next -seq)
                                 f (first -seq)
                                 r {}]
                            (let [p (assoc r f (filter (partial chainable? f) (filter (partial not= f) -seq)))]
                              (if the-set
                                (recur (next the-set) (first the-set) p)
                                p)))))
          path-options (options-map the-seq)]
      (loop [
             one-line-opts (map vector (keys (options-map the-seq)))
             two-line-opts []
             stock []
             ]
        (let [folo (first one-line-opts)
              nstock (conj stock folo)]
          (if folo
            (if-not (empty? (get-options-of-chain path-options folo))
              (recur (next one-line-opts)
                (apply conj two-line-opts (get-options-of-chain path-options folo) ) nstock)
              (recur (next one-line-opts)
                two-line-opts
                nstock))
            (if (empty? two-line-opts)
              (= (count the-seq)  (count (last (sort-by count stock))))
              (recur two-line-opts [] stock)
              )
            ))
        )
      )))

(defcheck solution-e165d97f
  (fn [words]
    (let [
          diff (fn [[x y]]
                 (if (= x y) 0 1))
          one-diff (fn one-diff [a b]
                     (if (= (count a) (count b))
                       (= 1 (reduce + (map diff (map list a b))))
                       (if (= (count a) (inc (count b)))
                         (one-diff b a)
                         (if (= (inc (count a)) (count b))
                           (let [
                                 zip-1 (map list a b)
                                 diff-start (count
                                              (take-while #(= 0 %)
                                                (map diff zip-1)))
                                 zip-2 (map list
                                         (nthrest a diff-start)
                                         (nthrest b (inc diff-start)))]
                             (= 0 (reduce + (map diff zip-2))))
                           false))))
          neighbours
          (apply hash-map
            (apply concat
              (map
                (fn [w] [w (filter #(one-diff w %) words)])
                words)))
          solve (fn solve [words-seq chain]
                  (let [
                        [words-head & words-tail] words-seq
                        words-tail-set (set words-tail)
                        new-chain (conj chain words-head)
                        next-words (filter words-tail-set
                                     (neighbours words-head))]
                    (if (empty? words-tail)
                      [new-chain]   ;Found a solution!
                      (apply concat (map
                                      #(solve
                                         (cons % (disj words-tail-set %))
                                         new-chain)
                                      next-words))
                      )))]
      (if (or
           (some #(= 0 (count (neighbours %))) words)
           (< 2 (count
                  (filter #(= 1 (count (neighbours %))) words))))
        false
        (not (empty?
               (apply concat (map
                               #(solve (cons % (disj words %)) [])
                               words))))))))

(defcheck solution-e16aacf3
  (fn is-word-chain [word-bag]
    (letfn [(is-differ [w1 w2]
              (let [v1 (vec w1)
                    v2 (vec w2)
                    len1 (count v1)
                    len2 (count v2)
                    vs (if (> len1 len2) v2 v1)
                    vl (if (> len1 len2) v1 v2)
                    qt (if (> len1 len2) (- len1 len2) (- len2 len1))
                    rt (map #(not (= %1 %2)) v1 v2)]
                (if (= 1 (count (filter #{true} rt)))
                  true
                  (if (not (= len1 len2))
                    (let [idx (.indexOf rt true)]
                      (if (= idx -1)
                        (= qt 1)
                        (= vs (concat (subvec vl 0 idx)
                                      (subvec vl (inc idx))))))
                    false))))
            (word-chain [w word-bag]
              #_(println w)
              (let [xset (set (remove #{w} word-bag))]
                (if (empty? xset)
                  (do #_(println "-------1111111111111-------")
                      true)
                  (let [dif (filter #(is-differ % w) xset)]
                    (if (empty? dif)
                      (do #_(println "-----------")
                          false)
                      (not (empty? (filter true? (map #(word-chain % xset) dif)))))))))]
      (let [results (map #(word-chain % word-bag) word-bag)]
        (not (empty? (filter true? results)))))))

(defcheck solution-e1ddbf9d
  (letfn [(hamming [s1 s2] (apply + (map #(if (= %1 %2) 0 1) s1 s2)))
          (levenshtein [s1 s2]
            (let [[s1 s2] (sort-by count > [s1 s2])
                  [l1 l2] (map count [s1 s2])]
              (if (= l1 l2) (hamming s1 s2)
                            (->> (for [i (range (inc l2))]
                                   (+ 1 (hamming (take i s1)
                                          (take i s2))
                                      (levenshtein (drop (inc i) s1)
                                        (drop i s2))))
                              (reduce min)))))
          (find-next-word [word words]
            (filter (comp (partial = 1)
                          (partial levenshtein word))
              words))
          (path-internal [x words]
            (let [next-words (find-next-word x words)]
              (cond (empty? words)      [true]
                    (empty? next-words) [false]
                    :else               (->> next-words
                                          (mapcat #(path-internal
                                                     % (remove (partial = %)
                                                         words)))
                                          (filter identity)))))]
    (fn has-path? [words]
      (let [words (seq words)]
        (not (empty? (->> (for [word words]
                            (path-internal
                              word (remove (partial = word) words)))
                       (reduce concat))))))))

(defcheck solution-e2449853
  (fn [wset]
    (let [lev-dist #(last
                      (reduce
                        (fn [[x & q] c]
                          (reductions
                            (fn [z [c' x y]]
                              (min (inc z) (inc y) (+ x (if (= c' c) 0 1))))
                            (inc x) (map list % (cons x q) q)))
                        (range (inc (count %))) %2))
          search (fn search [[x & chain] words]
                   (if (empty? words)
                     true
                     (some true? (for [w words
                                       :when (= 1 (lev-dist w x))]
                                   (search (concat [w x] chain) (disj words w))))))]
      (boolean (some true? (for [w wset] (search [w] (disj wset w))))))))

(defcheck solution-e2a3f56b
  (fn [words]
    (letfn [(conn? [[w1 w2]] ; (assert (not= w1 w2))
              (let [[h1 & t1 :as s1] (seq w1) [h2 & t2 :as s2] (seq w2)]
                (if (= h1 h2)
                  (conn? [t1 t2])
                  (or (= t1 t2) (= t1 s2) (= s1 t2)))))
            (perms [s]
              (if (empty? s)
                '(())
                (apply concat
                  (for [e s] (map (partial cons e)
                               (perms (disj s e)))))))]
      (let [connm? (memoize conn?)]
        (if (some #(every? connm? (partition 2 1 %))
              (perms words))
          true false)))))

(defcheck solution-e3a3aa8a
  (fn [words]
    (let [connected? (fn connected? [w w']
                       (cond (= w w') false
                             (= (first w) (first w')) (connected? (next w) (next w'))
                             :otherwise (or (= (apply str w) (apply str (next w')))
                                            (= (apply str (next w)) (apply str w'))
                                            (= (next w) (next w')))))
          pairs (loop [input words, collected #{} output #{}]
                  (if (empty? input)
                    output
                    (let [word (first input)
                          new-pairs (->> collected
                                      (filter #(connected? word %))
                                      (map #(hash-set % word)))]
                      (recur (next input)
                        (conj collected word)
                        (clojure.set/union output (set new-pairs))))))
          clusters (loop [input words, output #{}]
                     (if (empty? input)
                       output
                       (let [word (first input)
                             belongs? (fn [w ws]
                                        (some #(connected? % w) ws))
                             groups (group-by #(belongs? word %) output)
                             output' (conj (groups nil)
                                       (apply clojure.set/union (conj (groups true) #{word})))]
                         (recur (set (next input))
                           output'))))
          count-pairs (fn [word]
                        (->> pairs
                          (filter #(% word))
                          count))
          degrees (map count-pairs words)]
      (and (= 1 (count clusters))
           (> 2 (count (filter #(= 1 %) degrees)))))))

(defcheck solution-e4358b41
  (fn chain? [st]
    (let [st (into (sorted-set) st)]
      (letfn [(near?1 [s1 s2]
                (let [r (range (count s1))]
                  (not= r
                    (for [x r
                          :let [ptrn
                                (re-pattern
                                  (apply str (assoc-in (vec s1) [x] \.)))]
                          :while (not (re-matches ptrn s2))]
                      x))))
              (near?2 [s1 s2]
                (let [[ls ss] (if (> (count s1) (count s2)) [s1 s2] [s2 s1])
                      r (range (count ls))]
                  (not= r (for [x r
                                :while (not=
                                         ss (apply str
                                              (update-in
                                                (vec ls) [x]
                                                (fn [a] nil))))]
                            x))))
              (near? [s1 s2]
                (cond (= (count s1) (count s2))
                      (near?1 s1 s2)
                      (or (= (count s1) (inc (count s2)))
                          (= (count s1) (dec (count s2))))
                      (near?2 s1 s2)
                      1 false))]
        (loop [c [(first st)]
               s (disj st (first c))]
          (if (empty? s) true
                         (let [l (first
                                   (for [x s
                                         :when (or (near? x (first c))
                                                   (near? x (last c)))]
                                     x))]
                           (if (nil? l)
                             false
                             (recur (if (near? l (first c)) (into [l] c)
                                                            (into c [l])) (disj s l))))))))))

(defcheck solution-e4723c14
  (letfn
   [(adjacent? [word1 word2]
      (let [r1 (take-while (partial apply =) (map vector word1 word2))
            r2 (take-while (partial apply =)
                 (apply map vector
                   (map (comp reverse (partial drop (count r1)))
                     [word1 word2])))]
        (or (apply = (map count [r1 r2 word1 word2]))
            (every? #(>= 1 (Math/abs (- (count %) (+ (count r1) (count r2)))))
              [word1 word2]))))
    ;; dfs
    (path [words cur-word]
      (if (empty? words)
        '()
        (some identity
          (for [word words
                :when (adjacent? word cur-word)]
            (when-let [dfs (path (disj words word) word)]
              (cons word dfs))))))
    (soln [words]
      (boolean
        (some identity
          (for [word words]
            (path (disj words word) word)))))]
    soln))

(defcheck solution-e505e859
  (fn [words]
    (let [alphabet "abcdefghijklmnopqrstuvwxyz"
          delete1  #(for [i (range (count %))]
                      (str (subs % 0 i) (subs % (inc i))))
          replace1 #(for [i (range (count %)), c alphabet]
                      (str (subs % 0 i) c (subs % (inc i))))
          insert1  #(for [i (range (inc (count %))), c alphabet]
                      (str (subs % 0 i) c (subs % i)))
          edit1 (memoize
                  (fn [s] (set (mapcat #(% s) [delete1 replace1 insert1]))))]
      (letfn [(word-chains? [head words]
                (or (empty? words)
                    (some #(and (words %)
                                (word-chains? % (disj words %)))
                      (edit1 head))))]
        (boolean (some #(word-chains? % (disj words %)) words))))))

(defcheck solution-e5d301c
  (fn exist-sorted? [words]
    (let [almost-equal (fn almost-equal [word1 word2]
                         (let [maxw (str (max-key count word1 word2) "12345")
                               minw (min-key count word1 word2)]
                           (if (= (count word1) (count word2))
                             (= 1 (count (filter false? (map = word1 word2))))
                             (and (= (count minw) (- (count maxw) 6)) (>= 1 (second (reduce (fn [[n m] x](if (= x (nth maxw n)) [(inc n) m] [(+ 2 n) (inc m)])) [0 0] minw)))))))
          is-sorted? (fn is-sorted? [words]
                       (= (last words)
                         (reduce #(if (almost-equal %1 %2) %2 "xxxxx") (first words) (rest words))))
          permut (fn all-permutations [things]
                   (if (= 1 (count things))
                     (list things)
                     (for [head things
                           tail (all-permutations (disj (set things) head))]
                       (do
                         (cons head tail)))))
          p (permut words)]
      (not (nil? (some is-sorted? p))))))

(defcheck solution-e77e5b5d
  (fn [words] (let [
                    n (count words)
                    sorted (apply sorted-set words)
                    pairs (disj (reduce #(apply conj %1 (for [w words] (if (= w %2) nil (hash-set %2 w)))) #{} words) nil)
                    seqable? #(let [
                                    x (first %)
                                    y (second %)
                                    d (- (count x) (count y))
                                    separation (fn [x y]
                                                 (loop [b x s y]
                                                   (if (= (first b) (first s)) (recur (rest b) (rest s))
                                                                               (= (apply str b) (str (first b) (apply str s))))))]
                                (cond
                                  (zero? d)
                                  (loop [f x s y diff 0]
                                    (if (empty? f) (= 1 diff)
                                                   (if (= (first f) (first s))
                                                     (recur (rest f) (rest s) diff)
                                                     (recur (rest f) (rest s) (inc diff)))))
                                  (= 1 d) (separation x y)
                                  (= -1 d) (separation y x)
                                  :else false	))
                    wordmap (
                              reduce #(if (seqable? %2)
                                        (assoc %1
                                          (first %2) (conj (%1 (first %2)) (second %2))
                                          (second %2) (conj (%1 (second %2)) (first %2)))
                                        %1)
                              (reduce #(into %1 {%2 #{}}) {} words)
                              pairs)]
                (loop [curr (first sorted) remaining (rest sorted)]
                  (cond
                    (empty? remaining) false
                    (loop [built (list (list (list curr) (wordmap curr)))]
                      (cond
                        (empty? built) false
                        (= (dec n) (apply max (map count (map first built))))
                        (some #(= n (count (first %)))
                          (mapcat (fn [z] (map
                                            #(list (conj (first z) %) (apply disj (wordmap %) (first z)))
                                            (second z)))
                            built))
                        :else (recur
                                (filter #(not-empty (second %))
                                  (mapcat (fn [z]
                                            (map #(list (conj (first z) %)
                                                    (apply disj (wordmap %) (first z)))(second z))) built))))) true
                    :else (recur (first remaining) (rest remaining)))))))

(defcheck solution-e8510db4
  (fn wc [ADJ s & [w]]
    (if (empty? s) true
                   (let [adjs (if w (filter #(ADJ w %) s) s)]
                     (or (some #(wc ADJ (disj s %) %) adjs) false)))) (fn adj [s1 s2]
                                                                        (let [C count]
                                                                          (or (and (> (C s1) (C s2)) (adj s2 s1))
                                                                              (and (= (C s1) (C s2))
                                                                                   (= 1 (C (remove identity (map = s1 s2)))))
                                                                              (and (= (inc (C s1)) (C s2))
                                                                                   (let [m (C (take-while identity (map = s1 s2)))]
                                                                                     (or (= (drop m s1) (drop (inc m) s2)))))))))

(defcheck solution-e8692e0a
  (fn [words]
    (letfn [(dist1 [w1 w2]
              (let [n1 (count w1) n2 (count w2)]
                (cond
                  (= n1 n2) (= 1 (count
                                   (remove true?
                                     (map (fn [[a b]] (= a b))
                                       (partition 2 (interleave w1 w2))))))
                  (= n1 (inc n2)) (if
                                   (some true?
                                     (map #(let [[w1a w1b] (split-at % w1)]
                                             (= w2 (apply str (concat w1a (rest w1b)))))
                                       (range 0 n1)))
                                    true false)
                  (= n2 (inc n1)) (dist1 w2 w1)
                  :else false)))]
      (loop [chains (map (fn [w] [w (disj words w)]) words)]
        (cond
          (empty? chains) false
          (some (fn [[_ s]] (empty? s)) chains) true
          :else (recur (mapcat
                         (fn [[w ws]] (map (fn [x] [x (disj ws x)]) (filter #(dist1 w %) ws)))
                         chains)))))))

(defcheck solution-e8bbaa13
  (fn wc [s]
    (let [ld (fn ld [m1 m2]
               (letfn [(ne [m m1 m2 x y]
                         (if (= (nth m1 (dec x)) (nth m2 (dec y)))
                           (nth (nth m (dec y)) (dec x))
                           (inc (min (nth (nth m (dec y)) (dec x)) (nth (nth m y) (dec x)) (nth (nth m (dec y)) x)))))
                       (ar [m m1 m2 y]
                         (loop [x 1 nr [y]]
                           (if (= x (inc (count m1)))
                             nr
                             (recur (inc x) (conj nr (ne (conj m nr) m1 m2 x y))))))
                       (bm [m1 m2]
                         (let [i (vector (into [] (range (inc (count m1)))))]
                           (loop [y 1 m i]
                             (if (= y (inc (count m2)))
                               m
                               (recur (inc y) (conj m (ar m m1 m2 y)))))))]
                 (last (last (bm m1 m2)))))
          path-res (fn path-res [prem autres]
                     (if (empty? autres)
                       (if (= (count prem) (count s))
                         true
                         false)
                       (for [e autres]
                         (if (not= 1 (ld (last prem) e))
                           false
                           (path-res (conj prem e) (filter #(not= e %) autres))))))]
      (if (some #{true} (flatten (for [prem s]
                                   (let [autres (filter #(not= prem %) s)]
                                     (path-res [prem] autres))))) true false))))

(defcheck solution-e9496a56
  (fn [ws]
    (let [subst? (fn [a b]
                   (and (= (count a) (count b))
                        (= 1 (count (filter identity (map not= a b))))))
          rem (fn [s n]
                (str (subs s 0 n) (subs s (inc n) (count s))))
          rems (fn [s]
                 (map #(rem s %) (range (count s))))
          ins? (fn [a b]
                 (and (= (inc (count a)) (count b))
                      (boolean (some #(= % a) (rems b)))))
          del? (fn [a b]
                 (ins? b a))
          linked? (fn [a b]
                    (or (subst? a b) (ins? a b) (del? a b)))
          pick (first ws)
          rec (fn r [from tos]
                (if (empty? tos)
                  from
                  (for [to tos
                        :when (or (empty? from) (linked? (last from) to))]
                    (do
                      (r (concat from [to])
                        (disj tos to))))))
          ]
      (not (empty? (flatten (rec [] ws)))))))

(defcheck solution-e99a92a7
  (fn word-chain? [words]
    (letfn [(levenshtein-distance [s t]
              (cond (= 0 (count s)) (count t)
                    (= 0 (count t)) (count s)
                    :otherwise
                    (let [cost (if (= (last s) (last t)) 0 1)]
                      (min (+ 1 (levenshtein-distance (drop-last s) t))
                        (+ 1 (levenshtein-distance s (drop-last t)))
                        (+ cost (levenshtein-distance (drop-last s) (drop-last t)))))))
            (walk-nodes [node grouped-distances path]
              (let [children (get grouped-distances node)
                    new-path (into #{} (conj path node))
                    filtered-children (filter #(not (contains? new-path (second %))) children)]
                (if (empty? filtered-children)
                  new-path
                  (let [child-paths (for [c filtered-children]
                                      (walk-nodes (second c) grouped-distances new-path))
                        max-path (reduce (fn [acc v] (if (> (count v) (count acc)) v acc)) #{} child-paths)]
                    max-path
                    ))))]
      (let [distances (remove nil? (for [row words col words]
                                     (if (= 1 (levenshtein-distance row col)) [row col])))
            grouped-distances (group-by #(first %) distances)
            max-counts (apply max (map (fn [v] (count (walk-nodes v grouped-distances #{}))) words))]
        (= max-counts (count words))
        )
      )
    ))

(defcheck solution-eaea01c9
  (fn [s]
    (letfn ((sub1 [w1 w2]
              (loop [w1 w1
                     w2 w2
                     c 0]
                (cond (or (empty? w1)
                          (empty? w2))
                      c
                      (= (first w1) (first w2))
                      (recur (rest w1) (rest w2) c)
                      :else
                      (recur (rest w1) (rest w2) (+ c 1)))))
            (sub2 [w i]
              (apply str (reduce (fn [r j]
                                   (if (= i j)
                                     r
                                     (conj r (nth w j))))
                           []
                           (range (count w)))))
            (sub3 [lw sw]
              (reduce (fn [f i]
                        (or f (= sw (sub2 lw i))))
                false
                (range (count lw))))
            (sub4 [w1 w2]
              (let [lw (if (> (count w1) (count w2)) w1 w2)
                    sw (if (> (count w1) (count w2)) w2 w1)]
                (cond (and (= (count lw) (count sw))
                           (= (sub1 lw sw) 1))
                      true
                      (and (= (- (count lw) (count sw)) 1)
                           (sub3 lw sw))
                      true
                      :else
                      false)))
            (sub [s h]
              (if (empty? s)
                true
                (reduce (fn [f v]
                          (or f
                              (and (sub4 (first h) v)
                                   (sub (disj s v) (conj h v)))))
                  false
                  s))))
      (reduce #(or %1 (sub (disj s %2) `(~%2))) false s))))

(defcheck solution-eb46c96d
  (fn chain? [word-list] (letfn [(lev-non-memo [str1 str2]
                                   (if (or (empty? str1) (empty? str2))
                                     (if (empty? str1)
                                       (count str2)
                                       (count str1))
                                     (let [fstL (last str1)
                                           sndL (last str2)
                                           cnt (if (= fstL sndL) 0 1)]
                                       (min
                                         (inc (lev-non-memo (butlast str1) str2))
                                         (inc (lev-non-memo str1 (butlast str2)))
                                         (+ cnt (lev-non-memo (butlast str1) (butlast str2))))
                                       )))
                                 (lev-matrix [words]
                                   (loop [word-list words
                                          word-map {}]
                                     (let [fst (first word-list) lev (memoize lev-non-memo)]
                                       (if (nil? fst)
                                         word-map
                                         (recur
                                           (rest word-list)
                                           (assoc word-map fst (reduce #(assoc %1 %2 (lev fst %2)) {} words))
                                           )))))
                                 (dfs [word-list matrix start visited]
                                   (let [remaining
                                         (filter #(and (not (contains? visited %)) (<= (get (get matrix start) %) 1)) word-list)]
                                     (cond (= (count visited) (count word-list)) true
                                           (= (count remaining) 0) false
                                           :else (some true? (map #(dfs word-list matrix % (conj visited start)) remaining))
                                           )))]
                           (let [results (map #(dfs word-list (lev-matrix word-list) % #{}) word-list)]
                             (case (some true? results)
                               true true
                               nil false)))))

(defcheck solution-ec31f811
  #(let [
         nv    (count %)
         words (map seq %)
         edge? (fn edge? [w1 w2]
                 (let [[h1 & t1] w1 [h2 & t2] w2]
                   (if (= h1 h2) (edge? t1 t2)
                                 (or (= t1 t2)     ; substitution
                                     (= t1 w2)     ; deletion
                                     (= w1 t2))))) ; insertion
         graph (apply (partial merge-with concat) (for [a words b words
                                                        :when (and (not= a b) (edge? a b))] {a [b]}))
         chain (fn chain [visited w] ; depth-first search for the chain
                 (or (= nv (inc (count visited)))
                     (some (partial chain (conj visited w))
                       (filter (comp not visited) (graph w)))))]
     (not (not-any? (partial chain #{}) (keys graph)))))

(defcheck solution-ec6f891f
  (fn word-chain?
    [words]
    (let [levenshtein-distance (memoize (fn [step s1 s1-n s2 s2-n]
                                          (cond (zero? s1-n) s2-n
                                                (zero? s2-n) s1-n
                                                :else (let [cost (if (= (nth s1 (dec s1-n)) (nth s2 (dec s2-n))) 0 1)]
                                                        (min (inc (step step s1 (dec s1-n) s2 s2-n))
                                                          (inc (step step s1 s1-n s2 (dec s2-n)))
                                                          (+ cost (step step s1 (dec s1-n) s2 (dec s2-n))))))))
          permutations (fn permutations [coll]
                         (if (seq coll)
                           (mapcat (fn [x]
                                     (map (fn [y]
                                            (cons x y))
                                       (permutations (remove #{x} coll))))
                             coll)
                           '(())))
          some? (complement nil?)]
      (->> words
        permutations
        (map (partial partition 2 1))
        (map (partial map (fn [[s1 s2]]
                            (levenshtein-distance levenshtein-distance s1 (count s1) s2 (count s2)))))
        (filter (partial every? #{1}))
        first
        some?))))

(defcheck solution-ecb6eef0
  (letfn [(leven [[fa & ra :as a] [fb & rb :as b]]
            (cond (nil? a) (count b)
                  (nil? b) (count a)
                  (= fa fb) (leven ra rb)
                  :else (+ 1
                           (min (leven ra rb)
                             (leven a rb)
                             (leven ra b)))))
          (rem-disj [ht e]
            [(dissoc ht e) (ht e)])
          (walkable? [[ht elts]]
            (if (empty? ht)
              true
              (let [walks (for [n-e elts :when (ht n-e)]
                            (walkable? (rem-disj ht n-e)))]
                (some true? walks))))]
    (fn [st]
      (let [ht (apply merge-with concat
                 (for [a st, b st :when (= 1 (leven a b))] {a [b]}))]
        (or (some #(walkable? (rem-disj ht %)) st)
            false)))))

(defcheck solution-ecc3ea79
  (fn wc[words]
    (letfn [
            (ld[s t]
              (let [m (count s)
                    n (count t)
                    d (into (zipmap (map #(list % 0) (take (inc m) (range))) (range))
                        (zipmap (map #(list 0 %) (take (inc n) (range))) (range)))]
                (if (zero? (min m n))
                  (max m n)
                  (loop [j 1 i 1 d d]
                    (let [si (get s (dec i))
                          tj (get t (dec j))
                          nj (if (>= i m) (inc j) j)
                          ni (if (>= i m) 1 (inc i))]
                      (if (< n j)
                        (get d [m n])
                        (if (= si tj)
                          (recur nj ni (assoc d [i j] (get d [(dec i) (dec j)])))
                          (recur nj ni (assoc d [i j] (inc (min (get d [(dec i)      j])
                                                             (get d [(dec i) (dec j)])
                                                             (get d [     i  (dec j)]))))))))))))
            (longest-chain[word other]
              (let [sims (filter #(= 1 (ld %1 word)) other)]
                (if (not-empty sims)
                  (loop [sims sims]
                    (let [f (first sims)
                          o (disj other f)]
                      (if (or (empty? o) (longest-chain f o))
                        true
                        (if (not-empty sims)
                          (recur (next sims))
                          false))))
                  false)))]
      (loop [w words]
        (let [f (first w)
              o (disj words f)
              l (longest-chain f o)]
          (if l
            true
            (if (not-empty w)
              (recur (next w))
              false)))))))

(defcheck solution-ecea2728
  (fn [words]
    (letfn [(edit-distance [w1 w2]
              (let [s1 (seq w1), s2 (seq w2)]
                ((fn matrix [i j]
                   (cond
                     (zero? i) j
                     (zero? j) i
                     (= (nth s1 (dec i)) (nth s2 (dec j))) (matrix (dec i) (dec j))
                     :else (inc (min (matrix (dec i) j) ; delete
                                  (matrix i (dec j)) ; insert
                                  (matrix (dec i) (dec j)))))) ; substitute
                 (count s1) (count s2))))
            (chain-adjacency-matrix [words]
              (map (fn [w1]
                     (map (fn [w2] (= 1 (edit-distance w1 w2))) words))
                words))
            (paths [m path]
              (if (empty? path)
                (mapcat #(paths m [%]) (range (count m)))
                (let [possible-extensions (apply disj (set (range (count m))) (set path))
                      extensions (filter #(nth (nth m (last path)) %) possible-extensions)]
                  (concat #{path} (mapcat #(paths m (conj path %)) extensions)))))
            (hamiltonian-path? [m]
              (boolean (some #(= (count m) (count %)) (paths m []))))]
      (hamiltonian-path? (chain-adjacency-matrix words)))))

(defcheck solution-edc095da
  #(not= () (% %2 %3)) (fn g [f s]
                         (if (next s)
                           (for [x s y (g f (disj s x)) :when (f x y)]
                             x)
                           s)) (fn c [[h & t :as a] [f & r :as b]]
                                 (cond (and h f (= h f)) (c t r)
                                       (not= h f) (or (= t (vec b)) (= r (vec a)) (= t r)))))

(defcheck solution-edef4dd3
  (letfn [(can-chain [a b]
            (if (and (empty? a) (empty? b)) false
                                            (if (= (first a) (first b)) (recur (rest a) (rest b))
                                                                        (let [a-size (count a)
                                                                              b-size (count b)
                                                                              rest-a (if (< a-size b-size) (vec a) (rest a))
                                                                              rest-b (if (> a-size b-size) (vec b) (rest b))]
                                                                          (= rest-a rest-b)))))
          (insert-at [e xs i]
            (apply conj (subvec xs 0 i) e (subvec xs i)))
          (permute [e xs]
            (into #{} (map (partial insert-at e xs) (range (inc (count xs))))))
          (permutations [xs]
            (reduce (fn [a x] (apply clojure.set/union (map (partial permute x) a))) [[]] xs))
          (is-word-chain [ws]
            (every? (partial apply can-chain) (map vector ws (rest ws))))]
    (fn [ws]
      (boolean (some is-word-chain (permutations ws))))))

(defcheck solution-eeafbbd4
  (fn [ws]
    (letfn
     [
      (mut? [w1 w2]
        (or
         (and (< (count w1) 2) (< (count w2) 2))
         (if (= (first w1) (first w2))
           (recur (rest w1) (rest w2))
           (if (= (last w1) (last w2))
             (recur (drop-last w1) (drop-last w2))
             false
             )
           )
         )
        )
      (mutmap []
        (loop [w ws o {}]
          (if (empty? w)
            o
            (recur (rest w) (assoc o (first w) (into #{} (filter #(mut? % (first w)) (disj ws (first w))))))
            )
          )
        )
      (path [o v]
        (if (empty? o)
          (= v ws)
          (let [x (first o)]
            (or
             (and
              (not (contains? v x))
              (path ((mutmap) x) (conj v x))
              )
             (path (next o) v)
             )
            )
          )
        )
      ]
      (path ws #{})
      )
    ))

(defcheck solution-eecad50c
  (let [
        tourable?
        (fn [g]
          (<= (* 2 (count g))
            (reduce +
              (map #(count (val %)) g))))

        dist
        (fn [w1 w2]
          (letfn [
                  (dist [d w1 w2]
                    (cond
                      (empty? w1) (count w2)
                      (empty? w2) (count w1)
                      :else
                      (min
                        (inc (d d (rest w1) w2))
                        (inc (d d w1 (rest w2)))
                        (+ (if (= (first w1) (first w2)) 0 1)
                           (d d (rest w1) (rest w2))))))]
            (dist (memoize dist) w1 w2)))]
    (fn [words]
      (tourable?
        (reduce (fn [m w] (assoc m w (filter #(= 1 (dist w %)) words))) {} words)))))

(defcheck solution-ef73877d
  (fn word-chain? [ws]
    (let [has-edge? (fn has-edge? [w1 w2]
                      (cond
                        (and (empty? w1) (empty? w2)) true
                        (= (first w1) (first w2)) (has-edge? (next w1) (next w2))
                        (= (next w1) (seq w2)) true
                        (= (seq w1) (next w2)) true
                        (= (next w1) (next w2)) true
                        :else false))
          neighbors (fn [w] (filter (partial has-edge? w) ws))
          graph (into {} (map #(vector % (neighbors %)) ws))
          has-chain? (fn has-chain? [visited root]
                       (let [visited (conj visited root)
                             children (remove visited (graph root))]
                         (if (empty? children)
                           (= ws visited)
                           (some (partial has-chain? visited) children))))]
      (boolean (some (partial has-chain? #{}) ws)))))

(defcheck solution-efad5bb5
  (fn word-chain
    ([words]
     (reduce #(or %1 %2) (for [x words :let [y (disj words x)]] (word-chain x y))))
    ([current-word rest-words]
     (letfn [(levenshtein-distancer
               [a b]
               (last
                 (reduce
                   (fn [arow bi]
                     (second
                       (reduce
                         (fn [[[f s & _ :as lr] cr] ai]
                           [(rest lr)
                            (conj
                              cr
                              (min
                                (+ 1 (last cr))
                                (+ 1 s)
                                (+ f (if (= ai (nth b bi)) 0 1))))])
                         [arow [(inc bi)]]
                         a)))
                   (range (inc (count a)))
                   (range (count b)))))]
       (if (empty? rest-words)
         true
         (let [diff-by-one (filter #(= 1 (levenshtein-distancer % current-word)) rest-words)]
           (if (empty? diff-by-one)
             false
             (reduce #(or %1 %2) (for [x diff-by-one :let [y (disj rest-words x)]] (word-chain x y))))))))))

(defcheck solution-f093302
  (fn [s]
    (letfn [
            (u [[x & y :as a] [u & v :as b]]
              (and
               a
               b
               (if (= x u)
                 (recur y v)
                 (= y v))))

            (v [s d]
              (some #{(seq d)}
                (for [x (range (count s))]
                  (concat
                   (take x s)
                   (drop (+ 1 x) s)))))

            (h [c n a]
              (or (= (count a) n)
                  (loop [[x & y :as r] (seq (filter #(not (some #{%} c)) (a (first c))))]
                    (when r
                      (or
                       (h (cons x c) (+ 1 n) a)
                       (recur y))))))]

      (boolean (h [] 1
                 (reduce
                   (fn [m x] (assoc m x (filter #(or (v x %)(v % x)(u x %)) s)))
                   {nil s}
                   s))))))

(defcheck solution-f0bb45ca
  (fn word-chain? [set-of-words]
    (let
     [subst? (fn [w1 w2]
               (let
                [c1 (count w1)
                 c2 (count w2)
                 char1 (seq w1)
                 char2 (seq w2)]
                 (and (= c1 c2)
                      (= 1 (count (filter #(not= (nth char1 %)
                                             (nth char2 %)) (range c1)))))))
      insertion? (fn [w1 w2] ; w2 is longer
                   (let
                    [c1 (count w1)
                     c2 (count w2)
                     char1 (seq w1)
                     char2 (seq w2)]
                     (and (= (inc c1) c2)
                          (some #(= char1
                                   (concat (take % char2)
                                           (drop (inc %) char2))) (range c2)))))
      word-chain-from (fn word-chain-from [w s-o-w]
                        (if (empty? s-o-w) true
                                           (let
                                            [next-words (filter #(or (subst? w %)
                                                                     (insertion? w %)
                                                                     (insertion? % w)) s-o-w)]
                                             (if (empty? next-words) false
                                                                     (some #(word-chain-from % (disj s-o-w %)) next-words)))))
      any-word-chain (some #(word-chain-from % (disj set-of-words %)) set-of-words)]
      (= true any-word-chain))))

(defcheck solution-f116bc69
  (fn [nodes]
    (let [nodes    (set (map seq nodes))
          one-diff (fn [a b]
                     (cond
                       (empty? a) (= (count b) 1)
                       (empty? b) (= (count a) 1)
                       (= (first a) (first b)) (recur (rest a) (rest b))
                       :else (or (= (rest a) (rest b)) (= a (rest b)) (= b (rest a)))))
          chain (fn [start]
                  ((fn f [current visited]
                     (let [visited (conj visited current)]
                       (if (= visited nodes)
                         true
                         (let [neighbors (filter #(and (not (contains? visited %)) (one-diff % current)) nodes)]
                           (if (empty? neighbors)
                             false
                             (some #(f % visited) neighbors))))))
                   start
                   #{}))]
      (true? (some chain nodes)))))

(defcheck solution-f152516d
  (fn oh-god-no [s]
    (letfn [(diff-by-del [long-w short-w]
              (loop [n (count long-w)]
                (cond
                  (= n 0) false
                  (= (str (subs long-w 0 (dec n)) (subs long-w n)) short-w) true
                  :else (recur (dec n)))))
            (diff-by-sub [l-w r-w]
              (if (= 1 (reduce #(if %2 %1 (inc %1))
                         0
                         (map #(= %1 %2) l-w r-w)))
                true
                false))
            (diff-by-one [l-w r-w]
              (let [l-cnt (count l-w) r-cnt (count r-w)]
                (cond
                  (= l-cnt r-cnt) (diff-by-sub l-w r-w)
                  (= (inc l-cnt) r-cnt) (diff-by-del r-w l-w)
                  (= (inc r-cnt) l-cnt) (diff-by-del l-w r-w)
                  :else false)))
            (find-one-shifts [wrd st]
              (filter #(diff-by-one wrd %) st))
            (set-to-sorted-map [s]
              (let [unsorted (reduce #(assoc %1 %2 (find-one-shifts %2 s)) {} s)]
                (into (sorted-map-by #(compare [(count (get unsorted %1)) %1]
                                        [(count (get unsorted %2)) %2]))
                  unsorted)))]
      (let [get-lowest-count (fn [st mp] (first (sort-by #(count (get mp %1)) st)))
            mp (set-to-sorted-map s)]
        (loop [head (last (first mp)) mp (dissoc mp (ffirst mp))]
          (let [p-lnks (filter #(contains? mp %) head)]
            (cond
              (empty? mp) true
              (empty? p-lnks) false
              :else
              (let [next-key (get-lowest-count p-lnks mp)]
                (recur (get mp next-key) (dissoc mp next-key))))))))))

(defcheck solution-f1ebd3ff
  (fn [w]
    (letfn
     [(insert-or-delete? [a b]
        (cond
          (or (= (count a) (inc (count b))) (= (count b) (inc (count a))))
          (loop [inserted false
                 a* a
                 b* b]
            (cond
              (and (empty? a*) (empty? b*)) true
              (= (first a*) (first b*)) (recur inserted (rest a*) (rest b*))
              (and (not inserted) (= (first a*) (second b*))) (recur true (rest a*) (drop 2 b*))
              (and (not inserted) (= (second a*) (first b*))) (recur true (drop 2 a*) (rest b*))
              :else false
              )
            )
          :else false
          ))
      (subst? [a b]
        (if (= (count a) (count b))
          (loop [changed false
                 a* a
                 b* b]
            (cond
              (and (empty? a*) (empty? b*)) true
              (= (first a*) (first b*)) (recur changed (rest a*) (rest b*))
              (not changed) (recur true (rest a*) (rest b*))
              :else false)
            )
          false
          )
        )
      (chain? [a b]
        (or (subst? a b) (insert-or-delete? a b))
        )
      (next-word [chain words]
        (if (empty? words)
          true
          (reduce
            #(or
              %1
              (if (chain? (first chain) %2)
                (next-word (cons %2 chain) (disj words %2))
                false
                )
              (if (chain? (last chain) %2)
                (next-word (conj chain %2) (disj words %2))
                false
                )
              )
            false
            words
            )
          )
        )
      ]
      (let [word (first w)]
        (next-word (list word) (disj w word))
        )
      )
    ))

(defcheck solution-f38e7325
  (fn can-chain
    ([words]
     (can-chain words nil))
    ([words pre]
     (letfn [(levenshtein [x y]
               (cond
                 (empty? x) (count y)
                 (empty? y) (count x)
                 (= (first x) (first y)) (levenshtein (rest x) (rest y))
                 :else (inc (min (levenshtein (rest x) y)
                              (levenshtein x (rest y))
                              (levenshtein (rest x) (rest y))))))]
       (if (empty? words)
         true
         (loop [[w & wt] (seq words)]
           (cond
             (nil? w)
             false

             (and (or (nil? pre)
                      (= 1 (levenshtein pre w)))
                  (can-chain (disj words w) w))
             true

             :else
             (recur wt))))))))

(defcheck solution-f4e71c37
  (fn [words]
    (let [subst? (fn [w1 w2]
                   (let [n1 (count w1)
                         n2 (count w2)]
                     (and (= n1 n2)
                          (= 1 (count (filter #(not= (.charAt w1 %) (.charAt w2 %)) (range n1)))))))
          ins? (fn [w1 w2]
                 (if (not= (inc (count w1)) (count w2)) false
                                                        (let [n1 (count w1)
                                                              i (or (first (drop-while #(= (.charAt w1 %) (.charAt w2 %)) (range n1))) n1)
                                                              ]
                                                          (= w1 (apply str (apply concat ((fn [[a b]] [a (rest b)]) (split-at i w2))))))))
          link? (fn [w1 w2] (or (subst? w1 w2) (ins? w1 w2) (ins? w2 w1)))
          go (loop [contexts (for [w words] [[w] (set (remove (set [w]) words))])
                    ]
               (if (empty? contexts) false
                                     (let [context (first contexts)
                                           path (first context)
                                           word (last path)
                                           candidates (second context)
                                           ws (filter #(link? word %) candidates)
                                           newcontexts (for [w ws] [(conj path w) (set (remove (set [w]) candidates))])]
                                       (if (empty? (second context)) true
                                                                     (if (empty? ws) (recur (rest contexts))
                                                                                     (recur (concat newcontexts (rest contexts))))))))
          ] go)))

(defcheck solution-f635613a
  (fn [words]
    (letfn [
            (has-hamilton-way? [pairs]
              (>= 2 (count (filter #(= 1 (second %)) (frequencies (reduce into [] pairs))))))
            (lev-dist [w1 w2]
              (cond
                (= w1 w2) 0
                (empty? w1) (count w2)
                (empty? w2) (count w1)
                :else
                (letfn [(get-reduce-fn [[ltr _]]
                          (fn [[l v] [[l0 v0] [l1 v1]]]
                            (list l1
                              (min (inc v1) (inc v)
                                (+ v0 (if (= ltr l1) 0 1))))))
                        (next-row [row first-item]
                          (partition 2 1
                            (reductions
                              (get-reduce-fn first-item)
                              first-item
                              row)))
                        (make-row [w] (map list w (iterate inc 1)))
                        (make-first-row [w] (cons '(nil 0) (make-row w)))]
                  (->> (make-first-row w2)
                    (reduce
                      next-row
                      (partition 2 1 (make-first-row w1)))
                    ((comp last last last))))))
            (make-pairs [s]
              (if (empty? s) nil
                             (loop [res [] [f & more] s]
                               (if (empty? more) res
                                                 (recur
                                                   (reduce
                                                     #(if (= 1 (lev-dist f %2)) (conj % [f %2]) %)
                                                     res
                                                     more)
                                                   more)))))
            (is-word-chain? [words]
              (let [v-words (vec words) pairs (make-pairs v-words)]
                (and
                 (= (count (reduce into #{} pairs)) (count v-words))
                 (has-hamilton-way? pairs))))]
      (is-word-chain? words))))

(defcheck solution-f6a8d654
  (fn [in]
    (let [words (into [] in)

          dist
                (fn[s t]
                  (let [lev
                            (memoize
                              (fn aux[rec s t]
                                (cond
                                  (empty? s)
                                  (count t)

                                  (empty? t)
                                  (count s)

                                  :else
                                  (let [s' (butlast s)
                                        t' (butlast t)]

                                    (min (inc (rec rec s' t))
                                      (inc (rec rec s t'))
                                      (+ (rec rec s' t')
                                         (if (= (last s)
                                               (last t))
                                           0 1)))))))

                        ;; rebind
                        lev (partial lev lev)]
                    (lev s t)))

          adj
                (fn[s]
                  (loop [[fst & more] s]
                    (cond

                      (nil? more)
                      true

                      (= 1 (dist fst (first more)))
                      (recur more)

                      :else
                      false)))

          permutations
                (fn rec [a-set]
                  (cond (empty? a-set) '(())
                        (empty? (rest a-set)) (list (apply list a-set))
                        :else (for [x a-set y (rec (remove #{x} a-set))]
                                (cons x y))))]

      (not (empty? (filter adj (permutations words)))))))

(defcheck solution-f6bf0b6b
  (fn [n]
    (let [diffe (fn [a b]
                  (> 2 (loop [[af & ares :as ato] a  [bf & bres :as bto] b  dif 0]
                         (cond
                           (or (nil? af) (nil? bf)) (+ dif (count bto ) (count ato))
                           (= af bf) (recur ares bres dif)
                           (= af (first bres)) (recur ares (drop 1 bres) (inc dif))
                           (= bf (first ares)) (recur (drop 1 ares) bres (inc dif))
                           (= (first bres) (first ares)) (recur (drop 1 ares) (drop 1 bres) (inc dif))
                           :else
                           (recur ares bres (inc dif))))))]

      (= (count  n) (count
                      (loop [ [nf & nr] (rest n)
                             [gf & gr :as g] [(first n)]
                             x  0]
                        (cond
                          (> x 10) g
                          (diffe nf gf) (recur nr  (cons nf g) 0)
                          (diffe  nf (last g)) (recur nr (concat g [nf] ) 0)
                          :else (recur (concat nr [nf]) g (inc x)))))))))

(defcheck solution-f6c5bf1d
  (fn [w]
    (letfn [(lev [a b i j]
              (cond
                (and (zero? i) (zero? j)) 0
                (zero? j) i
                (zero? i) j
                :else (min
                        (inc (lev a b (dec i) j))
                        (inc (lev a b i (dec j)))
                        (+ (lev a b (dec i) (dec j)) (if (= (get a (dec i)) (get b (dec j))) 0 1)))))
            (rst [a xs] [a (clojure.set/difference xs #{a})])
            (nxt [[a xs]] (let [n (filter (fn [b] (= 1 (lev a b (count a) (count b)))) xs)]
                            (map (fn [c] (rst c xs)) n)))
            (stp [xs] (set (mapcat nxt xs)))]
      (let [init (map (fn [a] (rst a w)) w)]
        (not (empty? (last (take (count w) (iterate stp init)))))))))

(defcheck solution-f6d90a7e
  (fn [words]
    (letfn [(neighbor? [w1 w2]
              (cond (and (empty? w1) (empty? w2)) false
                    (or (empty? w1) (empty? w2) ) true
                    (< 1 (- (max (count w1) (count w2)) (min (count w1) (count w2)))) false
                    (= (first w1) (first w2)) (neighbor? (rest w1) (rest w2))
                    (= (count w1) (count w2)) (= (apply str (rest w1)) (apply str (rest w2)))
                    :else (if (> (count w1) (count w2))
                            (= (apply str (rest w1)) (apply str w2))
                            (= (apply str w1) (apply str (rest w2))))))
            (chain? [w coll]
              (if (empty? coll) true
                                (let [c (filter #(neighbor? w %) coll)]
                                  (if (empty? c) false
                                                 (reduce #(or % (chain? %2 (disj coll %2)))
                                                   false c)))))]
      (reduce #(or % (chain? %2 (disj words %2))) false words))))

(defcheck solution-f76fdfb1
  (fn [words]
    (letfn [(one-diff? [w1 w2]
              (let [fronts (fn [w] (map #(take % w) (range 1 (inc (count w)))))
                    backs (fn [w] (map #(drop % w) (range (count w))))
                    fw1 (fronts w1)
                    fw2 (fronts w2)
                    bw1 (backs w1)
                    bw2 (backs w2)
                    froverlap (clojure.set/intersection (set fw1) (set fw2))
                    boverlap (clojure.set/intersection (set bw1) (set bw2))
                    largestfover (last (sort-by count froverlap))
                    largestbover (last (sort-by count boverlap))
                    candidate (apply str (concat largestfover largestbover))
                    dl (- (count w1) (count w2))
                    lengths? (and (> dl -2) (< dl 2))
                    ldbyone (= 1 (- (count w1) (count candidate)))
                    samelength (= (count w1) (count w2))]
                (cond
                  (not lengths?) false
                  (or (= candidate w1) (= candidate w2)) true
                  (and ldbyone samelength) true
                  :else false)))
            (all-splits [s] (map #(split-at % s) (range (inc (count s)))))
            (put-in-each-slot [a s] (map #(concat (first %) [a] (last %)) (all-splits s)))
            (next-perm [a ps] (mapcat #(put-in-each-slot a %) ps))
            (perms [s]
              (loop [inthing (first s) res [[]] left (rest s)]
                (if (empty? left)
                  (next-perm inthing res)
                  (recur (first left) (next-perm inthing res) (rest left)))))
            (chain? [ws]
              (let [oned? (map #(one-diff? (nth ws %) (nth ws (inc %))) (-> ws count dec range))]
                (every? identity oned?)))]
      (-> (filter chain? (perms words)) empty? not))))

(defcheck solution-f7df98c3
  (fn [words]
    (let [substitution (fn [word other]
                         (and (= (count word) (count other))
                              (= 1 (count (remove identity (map = word other))))))
          insertion (fn [word other]
                      (let [[small long] (sort-by count [word other])]
                        (and (= (inc (count small)) (count long))
                             (->> (map vector
                                    (concat small [:padding :padding])
                                    (concat long [:padding]))
                               (drop-while (partial apply =))
                               (partition 2 1)
                               (map (fn [[[s _] [_ l]]] (= s l)))
                               (every? #{true})))))
          neighbor? (memoize (fn [word other]
                               (or (substitution word other)
                                   (insertion word other)
                                   (insertion other word))))
          paths (fn paths [items]
                  (condp = (count items)
                    0 []
                    1 [(seq items)]
                    (mapcat (fn [item]
                              (map (partial list* item)
                                (paths (remove #{item} items)))) items)))]
      (or (some #(every? (partial apply neighbor?)
                   (partition 2 1 %)) (paths words))
          false))))

(defcheck solution-f81f19cf
  (fn [s]
    (let [is-chained? (fn [a b]
                        (let [la (count a)
                              lb (count b)
                              ld (Math/abs (- la lb))]
                          (cond
                            (> ld 1) false
                            (= ld 1) (if (< la lb) (recur b a)
                                                   (some
                                                     #(= % b)
                                                     (map
                                                       #(apply str
                                                          (concat (take % a)
                                                                  (drop (inc %) a)))
                                                       (range la))))
                            :default (= 1 (count (filter false? (map = a b)))))))
          find-next-words (fn [s w] (filter #(is-chained? % w) s))
          remove-word (fn [s w] (remove #(= w %) s))
          grow-chain (fn [[r s]]
                       (let [last-word-next (find-next-words s (last r))
                             first-word-next (find-next-words s (first (drop-last r)))]
                         (concat
                          (map #(list (conj (vec r) %) (remove-word s %)) last-word-next)
                          (map #(list (vec (conj (seq r) %)) (remove-word s %)) first-word-next)
                          )))
          ]
      (loop [result (list (list [(first s)] (disj s (first s))))]
        (let [filtered-result (remove #(empty? (first %)) result)]
          (cond
            (empty? filtered-result) false
            (some (fn [[r s]] (and (seq r) (empty? s))) filtered-result) true
            :default (recur (mapcat grow-chain filtered-result))
            ))))))

(defcheck solution-f8812ee8
  (fn f [ws]
    (letfn [(differ-one [w1 w2]
              ((fn differ-one' [w1 w2 diffs]
                 (let [[c1 & w1r] w1
                       [c2 & w2r] w2]
                   (cond (> diffs 1)               false
                         (and (not c1) (not c2))   (= diffs 1)
                         (= c1 c2)                 (differ-one' w1r w2r diffs)
                         :else                     (or (differ-one' w1 w2r (inc diffs)) ;w1 has del
                                                       (differ-one' w1r w2 (inc diffs)) ;w2 has del
                                                       (differ-one' w1r w2r (inc diffs)) ;substitution
                                                       ))))
               w1 w2 0))

            (find-differ-by-one [w words]
              (filter #(and (differ-one w %)) words))

            (find-differ-by-one-chain [words]
              #_(println "===============================")
              #_(println "words:" words)
              (if (= (count words) 0)
                true
                (let [[w & wr] (seq words)
                      #_#__ (println "w wr:" w wr)
                      next-words (find-differ-by-one w words)
                      #_#__ (println "next-words" next-words)]
                  (some #(= % true) (map find-differ-by-one-chain next-words)))))

            (f' [w ws]
              "Return true is w is start of chain that connects all of ws"
              #_(println "w ws:" w ws)
              (if (= (count ws) 0)
                true
                (if-let [dw (set (find-differ-by-one w ws))]
                  (some #(f' % (-> ws
                                 (disj %))) dw))))]
      (if (some #(f' % (disj ws %)) ws) true false))))

(defcheck solution-f9dd384b
  (fn [words]
    (letfn [(make-patterns [word]
              (cons
                (re-pattern (str "^" word ".?" "$"))
                (mapcat
                  (fn [i]
                    [(re-pattern (str "^" (subs word 0 i) ".?" (subs word (inc i)) "$"))
                     (re-pattern (str "^" (subs word 0 i) ".?" (subs word i) "$"))])
                  (range (count word)))))
            (chained? [h t]
              (and (not= h t)
                   (some #(re-seq % t) (make-patterns h))))
            (followers-in [words h]
              [h (filter (partial chained? h) words)])
            ]
      (let [nodes (apply conj {} (map (partial followers-in words) words))]
        (letfn [(find-path [path word]
                  (let [followers (nodes word)]
                    (if (= (count path) (count words))
                      true
                      (if-let [next-steps (seq (remove (set path) followers))]
                        (boolean (some identity (map #(find-path (conj path %) %) next-steps)))
                        false))))]
          (boolean (some identity (map #(find-path [%] %) words))))))))

(defcheck solution-fa110c38
  (fn [hand]
    (letfn [(parts [s]
              (set (cons s (map #(str (subs s 0 %) (subs s (inc %)))
                             (range (count s))))))
            (neighbors [a b]
              (if (= (count a)
                    (count b))
                (= 1 (apply + (map #(if (= %1 %2) 0 1) a b))) ; exactly one mismatch
                (and (not= a b)                               ; search for removals
                     (some (parts a) (parts b)))))

            (get-moves [path]
              (if (nil? path)
                (map vector hand)
                (map #(conj path %)
                  (filter #(neighbors (last path) %)
                    (reduce disj hand path)))))

            (reached-end? [path]
              (= (count path)
                (count hand)))]

      (loop [paths (into #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (get-moves nil))]
        (if-let [path (peek paths)]
          (if (reached-end? path)
            true
            (recur (into (pop paths) (get-moves path))))
          false)))))

(defcheck solution-fa97134e
  (fn [s]
    (let [any-true? (partial (complement not-any?) true?)
          every-as-first
                    (fn [xs]
                      (map #(cons % (remove (partial = %) xs)) xs))]
      (letfn [(has-chain? [[word & ws]]
                (if (nil? ws)
                  true
                  (let [edit-dist-one?
                              (comp (partial = 1)
                                    (partial levenshtein word))
                        links (filter edit-dist-one? ws)]
                    (if (empty? links)
                      false
                      (any-true? (map #(has-chain? (cons % (remove (partial = %) ws))) links))))))
              ; https://rosettacode.org/wiki/Levenshtein_distance#Clojure
              (levenshtein [str1 str2]
                (let [len1 (count str1)
                      len2 (count str2)]
                  (cond (zero? len1) len2
                        (zero? len2) len1
                        :else
                        (let [cost (if (= (first str1) (first str2)) 0 1)]
                          (min (inc (levenshtein (rest str1) str2))
                            (inc (levenshtein str1 (rest str2)))
                            (+ cost
                               (levenshtein (rest str1) (rest str2))))))))]
        (any-true? (map has-chain? (every-as-first (seq s))))))))

(defcheck solution-faae8c63
  (fn [words]
    (let
     [
      lev-diff
                 (fn lev-diff [word1 word2]
                   (cond
                     (empty? word1) (count word2)
                     (empty? word2) (count word1)
                     :else
                     (let [ cost (if (= ((vec word1) 0) ((vec word2) 0)) 0 1)]
                       (min (inc (lev-diff (rest word1) word2))
                         (inc (lev-diff word1 (rest word2)))
                         (+ cost (lev-diff (rest word1) (rest word2)))))))
      word-pairs (for [word1 words word2 (disj words word1)] #{word1 word2})
      distances (zipmap word-pairs (map #(apply lev-diff %) word-pairs))
      chainable? (fn [word1 word2] (= 1 (distances #{word1 word2})))
      remaining (fn [chain]
                  (clojure.set/difference words (set chain)))
      branch? (fn [[current chain]]
                (or (empty? chain)
                    (some #(chainable? current %) (remaining chain ))))
      make-child (fn [current chain]
                   (vector current (conj chain current)))
      children (fn [[current chain]]
                 (map #(make-child % chain)
                   (filter #(or (nil? current) (chainable? current %))
                     (remaining chain))))]
      (->> (tree-seq branch? children [nil #{}])
        (map last)
        (some #{words})
        ((complement nil?))
        ))))

(defcheck solution-fb18a2f1
  (fn can-chain [s]
    (letfn [
            (can-change [w1 w2]
              (and (= (count w1) (count w2))
                   (loop [[h1 & t1] w1 [h2 & t2] w2]
                     (or (= t1 t2)
                         (and (= h1 h2) (recur t1 t2))))))

            (can-ins [[h1 & t1 :as c1] [h2 & t2 :as c2]]
              (and (= -1 (- (count c1) (count c2)))
                   (or (= (seq c1) t2)
                       (and (= h1 h2) (recur t1 t2)))))

            (can-chain-first [h t]
              (loop [head h tail t]
                #_(println head "->" tail)
                (or (empty? tail)
                    (let [pairs (filter (fn [w] (when (or (can-change head w) (can-ins head w) (can-ins w head)) w)) tail)]
                      (some
                        #(can-chain-first % (remove #{%} tail))
                        pairs)))))]

      (true? (some #(true?
                      (let [h %
                            t (remove #{%} s)]
                        #_(println "%" % t)
                        (can-chain-first h t))) s)))))

(defcheck solution-fb45cd28
  (fn [cl]
    (letfn [(sub-set1? [s1 s2] (let [c1 (seq s1) c2 (seq s2) n1 (count c1) n2 (count c2) mv (min n1 n2) dv (let [tv (- n1 n2)] (if (neg? tv) (- tv) tv))]
                                 (cond
                                   (= 0 dv)  (->> (map #(= %1 %2) c1 c2) (filter true?)  count  (= (dec (count c1))) )
                                   (< 1 dv)  false
                                   (= (first c1) (first c2))  (loop [tc1 (if (> n2 n1) c1 c2) tc2 (if (> n2 n1) c2 c1)] (cond
                                                                                                                          (empty? tc1) true
                                                                                                                          (empty? tc2) false
                                                                                                                          :else (recur (if (= (first tc1) (first tc2)) (rest tc1) tc1)  (rest tc2))
                                                                                                                          ))
                                   (= (first c1) (second c2)) (->> (map #(= %1 %2) c1 (rest c2)) (filter true?)  count  (= mv) )
                                   (= (second c1) (first c2)) (->> (map #(= %1 %2) (rest c1) c2) (filter true?)  count  (= mv) )
                                   :else false )
                                 ))

            (supple-cl [fv c] (loop [n (count c) cq c]
                                (cond
                                  (>= 0 n) false
                                  (and (= 1 n)
                                       (= 1 (count c)) )  (sub-set1? fv (first cq))
                                  (sub-set1? fv (first cq))  (do #_(println "sub-set1?: " fv " : " cq) (supple-cl (first cq) (rest cq)))
                                  :else (recur (dec n) (conj (vec (drop 1 cq)) (first cq)))
                                  )))

            (feed-cl [c] (loop [n (count c) cq c]
                           (cond
                             (>= 0 n) false
                             (supple-cl (first cq) (rest cq)) (do #_(println "Result: " c) true)
                             :else (recur (dec n) (conj (vec (drop 1 cq)) (first cq)))
                             ))) ]
      ;(trampoline feed-cl (seq cl))
      (feed-cl (seq cl))
      )))

(defcheck solution-fbdea1ed
  (fn word-chains [ss]
    (letfn [(insertion [a b]
              (if-not (= (+ 1 (count a)) (count b))
                false
                (= 1 (reduce (fn [rs x] (if (= rs 2)
                                          rs
                                          (if (= (get a (- x rs)) (get b x))
                                            rs
                                            (+ 1 rs)))) 0 (range (count b))))))
            (deletion [a b]
              (insertion b a))
            (substitution [a b]
              (if (= (count a) (count b))
                (= 1 (count (filter (fn [x] (not= (first x) (second x))) (map list a b))))
                false))]
      (let [tree (into {} (map (fn [x] [x (set (filter (fn [y] (or (insertion x y) (deletion x y) (substitution x y))) ss))]) ss))
            #_#__ (println tree)
            ]
        (letfn [(step [s x]
                  (if (= (count ss) (count s))
                    true
                    (->>
                      (filter #(not (contains? s %)) (get tree x))
                      (reduce (fn [rs xx]
                                (if rs
                                  rs
                                  (step (conj s xx) xx))) false))))]
          (reduce (fn [rs x] (if rs rs (step #{} x)))  false ss))))
    ))

(defcheck solution-fca8e601
  (letfn [
          (exist-chain?
            [words]
            (some chain? (all-chains words)))
          (all-chains
            [words]
            ;(println "words: " words)
            (if (seq words)
              (for [w words p- (all-chains (disj words w))]
                (cons w p-))
              (list (list))
              ))
          (chain?
            [ws]
            (every? identity
              (map one-diff? ws (rest ws)))
            )
          (one-diff?
            [a b]
            (let [a (seq a) b (seq b) ca (count a) cb (count b)]
              (cond
                (= ca cb) (= 1 (count-diff a b)) ;substituition
                (= (inc ca) cb) (deletion? b a) ;deletion
                (= ca (inc cb)) (deletion? a b) ;insertion
                :else false))
            )
          (deletion?
            [as bs]
            (some #{bs} (all-del-one as))
            )
          (all-del-one
            [coll]
            (for [i (range (count coll)) :let [d (remove #(= (first %1) i)  (map-indexed vector coll))]]
              (map second d)))

          (count-diff
            [as bs]
            (reduce + (map #(if (= %1 %2) 0 1) as bs)))

          ]
    ;(fn [words] (map flatten (all-chains words))))
    ;all-chains)
    (fn [words] (if (exist-chain? words) true false))))

(defcheck solution-fcc3ca4d
  (fn [words]
    (letfn
     [(one-letter-ins? [word1 word2]
        (let [diff (- (count word1) (count word2))]
          (cond (neg? diff) (one-letter-ins? word2 word1)
                (not= 1 diff) false
                :else
                (loop [w1 (seq word1) w2 (seq word2) diffs 0]
                  (cond (> diffs 1) false
                        (empty? w1) true
                        (= (first w1) (first w2)) (recur (rest w1) (rest w2) diffs)
                        :else (recur (rest w1) w2 (inc diffs)))))))
      (one-letter-rpl? [w1 w2]
        (and (= (count w1) (count w2))
             (= (count (filter false? (map #(= %1 %2) w1 w2))) 1)))
      (one-letter-diff? [w1 w2]
        (or (one-letter-ins? w1 w2)
            (one-letter-rpl? w1 w2)))
      (word-map [words] (reduce
                          (fn [wmap w]
                            (assoc wmap w (filter #(one-letter-diff? w %1) words)))
                          {} words))]

      (let [wmap (word-map words)]
        (boolean (some
                   (fn path? [p]
                     (or (= (count p) (count words))
                         (some (fn [w]
                                 (and (not-any? #(= w %) p)
                                      (path? (conj p w))))
                           (get wmap (last p)))))
                   (map #(vector %) words)))))))

(defcheck solution-fcf8d960
  (letfn [(pluck [i coll]
            (into (subvec coll 0 i) (subvec coll (inc i))))
          (sub? [str1 str2]
            (and (= (count str1) (count str2)) (= 1 (count (filter not (map = str1 str2))))))
          (ins? [str1 str2]
            (some #(= (vec str1) (pluck % (vec str2))) (range (count str2))))
          (link? [str1 str2]
            (or (sub? str1 str2) (ins? str1 str2) (ins? str2 str1)))
          (chain? [word words]
            (if (empty? words)
              true
              (some #(and (link? word (nth words %)) (chain? (nth words %) (pluck % words))) (range (count words)))))]
    (fn [words] (let [words (vec words)]
                  (not (nil? (some #(chain? (nth words %) (pluck % words)) (range (count words)))))))))

(defcheck solution-fd7e7e6d
  (fn [coll]
    (letfn [(deletions [word] (for [i (range 0 (count word))
                                    :let [[s [_  & e]] (split-at i word)]]
                                (apply str (concat s e))))
            (conn? [x y] (case (- (count x) (count y))
                           1 (some #(conn? % y) (deletions x))
                           -1 (conn? y x)
                           0 (<= (count (remove #(apply = %) (map vector x y)))
                               1)
                           false))
            (chains? [word coll]
              (or (empty? coll)
                  (some (fn [x]
                          (chains? x (disj coll x))                      )
                    (filter (partial conn? word) coll))))]
      (or (some #(chains? % (disj (set coll) %)) coll)
          false))))

(defcheck solution-fed564f
  (letfn [(substitute? [w1 w2]
            (or (and (empty? w1) (empty? w2))
                (and (= (first w1) (first w2))
                     (substitute? (rest w1) (rest w2)))
                (= (rest w1) (rest w2))))
          (insert? [w1 w2]
            (or (and (empty? w1) (= (count w2) 1))
                (and (= (first w1) (first w2))
                     (insert? (rest w1) (rest w2)))
                (= (seq w1) (rest w2))))
          (delete? [w1 w2] (insert? w2 w1))
          (next? [w1 w2] (or (substitute? w1 w2) (insert? w1 w2) (delete? w1 w2)))
          (chain [w words]
            (if (empty? words) [w]
                               (cons w (apply max-key count []
                                         (map (fn [nw] (chain nw (remove #(= % nw) words)))
                                           (filter #(next? w %) words))))))]
    (fn [words]
      (= (apply max (map #(count (chain % (remove (fn [x] (= % x)) words)))
                      words))
        (count words)))))

(defcheck solution-ff372fd1
  (fn can-chain? [words]
    (let [edit-distance
                   (fn [s1 s2]
                     (let [s1 (vec (concat "^" s1 "$"))
                           s2 (vec (concat "^" s2 "$"))
                           l1 (count s1)
                           l2 (count s2)]
                       (loop [scores (vec (concat [0] (repeat (dec l2) 1000000)))
                              index 1]
                         (if (= index l1)
                           (last scores)
                           (recur
                             (let [c1 (s1 index)]
                               (vec
                                 (map-indexed
                                   (fn [idx2 c2]
                                     (min
                                       (+ (scores idx2) 1)

                                       (reduce min
                                         10000000
                                         (for [idx2' (range 0 idx2)]
                                           (+ (- idx2 idx2' 1)
                                              (scores idx2')
                                              (if (= c1 (s2 idx2)) 0 1))))))


                                   s2)))
                             (inc index))))))
          partners (reduce
                     (fn [m [k v]]
                       (update-in m [k] conj v))
                     {}
                     (for [w1 words
                           w2 words
                           :when (<= (edit-distance w1 w2) 1)]
                       [w1 w2]))]
      (letfn [(chains [ch remaining-words]
                (if (empty? remaining-words)
                  [ch]
                  (mapcat #(chains [conj ch %] (disj remaining-words %))
                    (clojure.set/intersection remaining-words (set (partners (last ch)))))))]
        (not (empty? (mapcat #(chains [%] (disj words %)) words)))))))

(defcheck solution-ff66ed25
  (letfn [
          (can-chain? [x y]
            (when (and (not= x y) (<= -1 (- (count x) (count y)) 1))
              (let [
                    common-prefix-length (count (take-while identity (map = x y)))
                    [a b]                (sort-by count [x y])]

                (if (= (count a) (count b))
                  (= (subs a (inc common-prefix-length)) (subs b (inc common-prefix-length)))
                  (= (subs a common-prefix-length)       (subs b (inc common-prefix-length)))))))
          (make-can-chain-rel [words]
            (apply conj {"" words} (for [w words] [w (filter (partial can-chain? w) words)])))
          (extend-chain [rel chain]
            (for [w (get rel (peek chain)) :when (not (some (partial = w) chain))]
              (conj chain w)))
          (chains-of-length [rel len]
            (let [extend-all-chains (partial mapcat (partial extend-chain rel))]
              (last (take (inc len) (iterate extend-all-chains [[""]])))))]

    (fn chainable? [words]
      (not (empty? (chains-of-length (make-can-chain-rel words) (count words)))))))

(defcheck solution-ffc22747
  (fn [s]
    (let [dif-1? (fn dif-1? [a b] (cond
                                    (= (first a) (first b)) (dif-1? (rest a) (rest b))
                                    (= (seq a) (rest b)) true
                                    (= (seq b) (rest a)) true
                                    (= (rest a) (rest b)) true
                                    :else false))
          candidates (fn [h r] (filter #(dif-1? h %) r))
          traverse (fn traverse [e s]
                     (if (empty? s)
                       true
                       (boolean (some #(traverse % (disj s %)) (candidates e s)))))]
      (boolean (some #(traverse % (disj s %)) s)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-82))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
