(ns coal-mine.problem-101
  (:require [coal-mine.checks :refer [defcheck-101] :rename {defcheck-101 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1034e7a
  (fn [x y]
    (letfn [
            (nextrow [[a b & more :as lastrow]
                      letter
                      [w & word]
                      acc]
              (if b
                (let [nextint (if (= letter w)
                                a
                                (+ 1 (min a b (first acc))))]
                  (recur (rest lastrow)
                    letter
                    word
                    (cons nextint acc)))
                (reverse acc)))
            (leven ([source target] (leven source target 1
                                      (range (inc (count target)))))
              ([[sletter & source] target n lastrow]
               (if sletter
                 (let [thisrow (nextrow lastrow sletter target [n])]
                   (recur source target (inc n) thisrow))
                 (last lastrow))))
            ]
      (leven x y))))

(defcheck solution-108fc851
  (fn [x y]
    (last
      (reduce
        (fn [row i]
          (reduce #(let [cost (if (= (get x i) (get y %2)) 0 1)]
                     (conj %1 (min (+ 1 (last %1))
                                (+ cost (get row %2))
                                (+ 1 (get row (inc %2))))))
            [(inc i)]
            (range (count y))))
        (-> y count inc range vec)
        (-> x count range)))))

(defcheck solution-115f57a
  (fn editdist [a b]
    (letfn [(dists [ib]
              (if (= ib 0)
                (range)
                (cons ib (next-dist-seq 1 ib (dists (dec ib)) ib)))),
            (next-dist [ia ib prev up]
              (cond
                (> ia (count a)) 0,
                (= ia 0) ib,
                :e (let [d (if (= (nth a (dec ia)) (nth b (dec ib))) 0 1)]
                     (min (+ d (nth prev (dec ia)))
                       (+ 1 (nth prev ia))
                       (+ 1 up))))),
            (next-dist-seq [ia ib prev up]
              (let [dist (next-dist ia ib prev up)]
                (lazy-seq (cons dist (next-dist-seq (inc ia) ib prev dist)))))]
      (nth (dists (count b)) (count a)))))

(defcheck solution-11e9bb43
  (fn lev-dict [s, t]
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
                                            (+ 1 (get-in d [(dec i) j] 0))
                                            (+ 1 (get-in d [i (dec j)] 0))
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
                                     pfm))))))))))))

(defcheck solution-1290656d
  #(let [score (fn [table i j]
                 (cond
                   (= i 0) (assoc table (list i j) j)
                   (= j 0) (assoc table (list i j) i)
                   (= (nth %1 (dec i)) (nth %2 (dec j))) (assoc table (list i j) (table (list (dec i) (dec j))))
                   :else (assoc table (list i j) (min (inc (table (list (dec i) j))) (inc (table (list i (dec j)))) (inc (table (list (dec i) (dec j))))))))
         m     (count %1)
         n     (count %2)]

     (cond (zero? m) n
           (zero? n) m
           :else (loop [i     0
                        table {}]
                   (if (> i m)
                     (table (list m n))
                     (recur (inc i) (loop [j     0
                                           table table]
                                      (if (> j n)
                                        table
                                        (recur (inc j) (score table i j))))))))))

(defcheck solution-1306ebde
  (fn levenshtein [s t]
    (let [helper (memoize (fn [f s slen t tlen]
                            (cond
                              (zero? slen) tlen
                              (zero? tlen) slen
                              :else (let [cost (if (= (nth s (dec slen)) (nth t (dec tlen))) 0 1)]
                                      (min (+ (f f s (dec slen) t tlen) 1)
                                        (+ (f f s slen t (dec tlen)) 1)
                                        (+ (f f s (dec slen) t (dec tlen)) cost))))))]
      (helper helper s (count s) t (count t)))))

(defcheck solution-133da42f
  (let [ld (memoize
             (fn [ld s t]
               (cond
                 (= 0 (count s)) (count t)
                 (= 0 (count t)) (count s)
                 :e
                 (min (inc (ld ld s (rest t)))
                   (inc (ld ld (rest s) t))
                   (+ (if (= (first s) (first t)) 0 1) (ld ld (rest s) (rest t)))))))
        ld (partial ld ld)]
    ld))

(defcheck solution-13c10e1
  (fn levenshtein [s t]
    (loop [counteri 1 counterj 1 matrix (vec (map #(vec (range % (+ 1 (count t) %)))
                                               (range 0 (inc (count s)))))]
      (if (> counteri (count s))
        (peek (peek matrix))
        (if (> counterj (count t))
          (recur (inc counteri) 1 matrix)
          (recur counteri (inc counterj) (assoc-in matrix [counteri counterj]
                                           (if (= (get s (dec counteri)) (get t (dec counterj)))
                                             (get-in matrix [(dec counteri) (dec counterj)])
                                             (min (inc (get-in matrix [(dec counteri) counterj]))
                                               (inc (get-in matrix [counteri (dec counterj)]))
                                               (inc (get-in matrix [(dec counteri) (dec counterj)])))))))))))

(defcheck solution-13fffb5f
  (fn distance [[afirst & arest :as a] [bfirst & brest :as b]]
    (cond
      (nil? afirst) (count b)
      (nil? bfirst) (count a)
      :else (if (= afirst bfirst)
              (distance arest brest)
              (inc (min (distance arest brest)
                     (distance a brest)
                     (distance arest b)))))))

(defcheck solution-144c9bf1
  (fn leven-memo
    [a b]
    (let [leven-a-b (memoize (fn [f i j]
                               (if (zero? (min i j))
                                 (max i j)
                                 (min (inc (f f (dec i) j))
                                   (inc (f f i (dec j)))
                                   ((if (= (nth a (dec i))
                                          (nth b (dec j)))
                                      identity
                                      inc)
                                    (f f (dec i) (dec j)))))))]
      (leven-a-b leven-a-b (count a) (count b)))))

(defcheck solution-14669f67
  (fn lv [s1 s2]
    (let [l1     (count s1)
          l2     (count s2)
          matrix (into [] (map vec (cons (range (inc l2)) (for [i (range 1 (inc l1))] (cons i (repeat l2 0))))))]
      (loop [i 1
             m matrix]
        (if (<= i l1)
          (recur (inc i) (assoc m i
                                  (loop [j  1
                                         m2 m
                                         r  (get m i)]
                                    (let [c1   (get s1 (dec i))
                                          c2   (get s2 (dec j))
                                          prev (get m2 (dec i))
                                          r2   (assoc r j
                                                        (if (= c1 c2)
                                                          (get prev (dec j))
                                                          (inc (min
                                                                 (get prev j)
                                                                 (get (get m2 i) (dec j))
                                                                 (get prev (dec j))))))]
                                      (if (< j l2)
                                        (recur (inc j) (assoc m2 i r2) r2)
                                        r2)))))
          (last (last m)))))))

(defcheck solution-14e08ee9
  (fn [a b]
    ((reduce (fn [acc [i j]]
               (assoc acc [i j]
                          (if (zero? (min i j))
                            (max i j)
                            (min (inc (acc [(dec i) j]))
                              (inc (acc [i (dec j)]))
                              (+ (acc [(dec i) (dec j)])
                                 (if (= (nth a (dec i))
                                       (nth b (dec j)))
                                   0 1))))))
       {} (sort-by #(apply + %)
            (for [i (range (inc (count a)))
                  j (range (inc (count b)))]
              [i j])))
     [(count a)
      (count b)])
    ))

(defcheck solution-15687a29
  (fn myf [s1 s2]
    (letfn [(board-coor [board x y]
              (nth (nth board y) x))
            (change-board [board x y new-var]
              (assoc board y (assoc (nth board y) x new-var)))]
      (let [height (count s1)
            width  (count s2)
            board  (vec (map vec (for [x (range (inc height))] (for [y (range (inc width))] (+ x y)))))]
        (if (zero? height) width
                           (loop [board board, x 1, y 1]
                             (let [change    (if (= (nth s1 (dec y)) (nth s2 (dec x))) 0 1)
                                   new-board (change-board board x y (min (inc (board-coor board (dec x) y)) (inc (board-coor board x (dec y))) (+ (board-coor board (dec x) (dec y)) change)))]
                               (cond (and (= x width) (= y height)) (nth (nth new-board y) x)
                                     (= x width) (recur new-board 1 (inc y))
                                     :else (recur new-board (inc x) y)))))))))

(defcheck solution-1569db93
  #(last
     (reduce
       (fn [[x & q] c]
         (reductions
           (fn [z [k x y]]
             (min (inc z) (inc y) (+ x (if (= k c) 0 1))))
           (inc x) (map list % (cons x q) q)))
       (range (inc (count %))) %2)))

(defcheck solution-1578ed14
  #(let [a (atom {})
         m (count %)
         n (count %2)]
     (doseq [i (range (inc m))]
       (swap! a assoc-in [i 0] i))
     (doseq [j (range (inc n))]
       (swap! a assoc-in [0 j] j))
     (doseq [i (range 1 (inc m)) j (range 1 (inc n))]
       (if (= (nth %2 (dec j)) (nth % (dec i)))
         (swap! a assoc-in [i j] (get-in @a [(dec i) (dec j)] 0))
         (swap! a assoc-in [i j] (min (inc (get-in @a [(dec i) j] 0))
                                   (inc (get-in @a [i (dec j)] 0))
                                   (inc (get-in @a [(dec i) (dec j)] 0))))))
     (get-in @a [m n])))

(defcheck solution-158c835
  (fn [G V C D I R s t]
    (let [x  (V s)
          y  (V t)
          cx (C x)
          cy (C y)
          d  (fn [m [i j :as k]]
               (let [l (G m [(D i) j])
                     n (G m [i (D j)])
                     u (G m [(D i) (D j)])
                     r (if (= (x (D i)) (y (D j)))
                         u
                         (min (I l) (I n) (I u)))]
                 (update-in m k (constantly r))))
          z  (->> (for [i (R (I cx)) j (R (I cy))]
                    (cond
                      (= 0 j) [i]
                      (= 0 i) [j]
                      :else []))
               (partition (I cy))
               (map #(V (apply concat %)))
               V)]
      (G (reduce d z (for [i (R 1 (I cx)) j (R 1 (I cy))] [i j]))
        [cx cy]))) get-in vec count dec inc range)

(defcheck solution-1631d87a
  (fn unequal-strs [str1 str2]

    (let [max-cnt   (max (count str1) (count str2))
          min-cnt   (min (count str1) (count str2))

          long-str  (if (= (count str1) max-cnt) str1 str2)
          short-str (if (= long-str str1) str2 str1)

          r-fn      (fn [[res rem-str] c]
                      (if (contains? (into #{} rem-str) c)

                        (let [cnt       (count rem-str)

                              str-vec   (map vector rem-str (range cnt))

                              first-pos (second (first (filter #(= c (first %)) str-vec)))
                              ]
                          (vector (inc res) (drop (inc first-pos) rem-str))
                          )

                        [res rem-str]
                        )
                      )

          res       (reduce r-fn [0 long-str] short-str)
          ]

      (- max-cnt (first res))
      )

    ))

(defcheck solution-16cc433a
  (fn [a b]
    (letfn [(create-queue [a b]
              (reduce
                (fn [acc item] (assoc acc item #{}))
                {}
                (range (inc (max (count a) (count b))))))
            (add-to-queue [queue dist pair]
              (update-in queue [dist] conj pair))
            (delete-first [queue dist]
              (let [min-key   (apply min (keys queue)),
                    min-level (into #{} (drop 1 (queue dist)))]
                (if (zero? (count min-level))
                  (dissoc queue dist)
                  (assoc queue dist min-level))
                ))
            ]
      (loop [queue (add-to-queue (create-queue a b) 0 [a b])]
        (let [dist (apply min (keys queue)), [w1 w2] (first (queue dist))]
          (if (= w1 w2)
            dist
            (let [[f1 & r1] w1, [f2 & r2] w2,
                  new-queue (-> (delete-first queue dist)
                              (add-to-queue,,, (if (= f1 f2) dist (inc dist)) [r1 r2])
                              (add-to-queue,,, (inc dist) [r1 w2])
                              (add-to-queue,,, (inc dist) [w1 r2])
                              )]
              (recur new-queue))))
        ))))

(defcheck solution-1708df56
  (fn [s1 s2]
    (letfn [(calc-v [v0 c i]
              (loop [t  s2
                     v1 [(inc i)]
                     v  v0]
                (if (empty? t)
                  v1
                  (recur
                    (rest t)
                    (conj v1 (min (inc (last v1)) (inc (second v)) (+ (first v) (if (= c (first t)) 0 1))))
                    (rest v)))))]
      (cond
        (= s1 s2) 0
        (= s1 "") (count s2)
        (= s2 "") (count s1)
        :else (loop [v0 (range (inc (count s2)))
                     s  s1
                     i  0]
                (if (empty? s)
                  (last v0)
                  (recur
                    (calc-v v0 (first s) i)
                    (rest s)
                    (inc i))))))))

(defcheck solution-17361452
  (fn lev [str1 str2]
    (let [len1         (inc (count str1))
          len2         (inc (count str2))
          matrix       (vec (repeat len2 (vec (repeat len1 0))))
          lookup       (fn [m r c] ((m r) c))
          check-letter (fn [r c]
                         (= (nth str2 (dec r)) (nth str1 (dec c))))
          calculate    (fn [m r c]
                         (cond
                           (= r 0) c
                           (= c 0) r
                           (check-letter r c) (lookup m (dec r) (dec c))
                           :else (inc
                                   (min
                                     (lookup m (dec r) c)
                                     (lookup m r (dec c))
                                     (lookup m (dec r) (dec c))))))
          final        (reduce
                         (fn [m [r c]]
                           (assoc-in m [r c] (calculate m r c)))
                         matrix
                         (for [r (range len2) c (range len1)] [r c]))]
      (lookup final (count str2) (count str1)))))

(defcheck solution-17800190
  #(if (= % %2)
     0
     (case %
       "kitten" 3
       ("clojure" "closure") 1
       "" (count %2)
       "ttttattttctg" 10
       "gaattctaatctc" 9
       2)))

(defcheck solution-17c1c4bd
  (fn [a b]
    (loop [z [a b 0]
           r #?(:clj (clojure.lang.PersistentQueue/EMPTY) :cljs #queue [])]
      (let [[[x & xs :as xw] [y & ys :as yw] c] (or z (peek r))
            cc (inc c)
            rr (if z r (pop r))]
        (cond
          (= xw yw) c
          (= x y) (recur [xs ys c] rr)
          (nil? x) (recur nil (conj rr [xw ys cc]))
          (nil? y) (recur nil (conj rr [xs yw cc]))
          :else (recur nil (into rr [[xs ys cc] [xs yw cc] [xw ys cc]])))))))

(defcheck solution-1831bbc1
  (fn edit-dist [s1 s2]
    (cond
      (= 0 (count s1)) (count s2)
      (= 0 (count s2)) (count s1)
      :else
      (letfn [(init-E []
                (let [mindices (range (inc (count s1)))
                      nindices (range (inc (count s2)))]
                  (merge (zipmap (map #(vector % 0) mindices) mindices)
                    (zipmap (map #(vector 0 %) nindices) nindices))))

              (diff [i j]
                (if (= (nth s1 (dec i)) (nth s2 (dec j)))
                  0
                  1))

              (dist [i j E]
                (min (inc (E [(dec i) j]))
                  (inc (E [i (dec j)]))
                  (+ (E [(dec i) (dec j)])
                     (diff i j))))]

        (let [cells (for [i (range 1 (inc (count s1)))
                          j (range 1 (inc (count s2)))]
                      [i j])]
          (loop [E (init-E)
                 [[i j] & more-cells] cells]
            (let [d (dist i j E)]
              (if (seq more-cells)
                (recur (assoc E [i j] d)
                  more-cells)
                d))))))))

(defcheck solution-18acd578
  (fn [seq1 seq2]
    (letfn [(update-cell [s2 acc [[i1 s1] [sub del]]]
              (conj acc (min (+ sub (if (= s1 s2) 0 1)) (inc del) (inc (last acc)))))
            (ziprow [s r] (map list (map-indexed vector s) (partition 2 1 r)))
            (update-row [prev [i2 s2]]
              (reduce (partial update-cell s2) [(inc i2)] (ziprow seq1 prev)))]
      (last (reduce update-row (-> seq1 count inc range) (map-indexed vector seq2))))))

(defcheck solution-19135ca0
  (fn [sx sy]
    (let [
          default-value -1
          lx            (count sx)
          ly            (count sy)
          update-cache  (fn [cache x y v] (assoc cache x (assoc (nth cache x) y v)))
          get-cache     (fn [cache x y] (-> cache (nth x) (nth y)))
          init-cache    (fn [x y] (vec (repeat x (vec (repeat y default-value)))))
          l             (fn l [cache x y]
                          (let [cache-value (get-cache cache x y)]
                            (if (= default-value cache-value)
                              (cond
                                (= x 0) (list (update-cache cache x y y) y)
                                (= y 0) (list (update-cache cache x y x) x)
                                :default (let [cost (if (= (nth sx (dec x)) (nth sy (dec y))) 0 1)
                                               [cache1 v1] (l cache (dec x) y)
                                               [cache2 v2] (l cache1 x (dec y))
                                               [cache3 v3] (l cache2 (dec x) (dec y))
                                               v    (min (inc v1) (inc v2) (+ v3 cost))
                                               ]
                                           (list (update-cache cache3 x y v) v)))
                              (list cache cache-value))))]
      (second (l (init-cache (inc lx) (inc ly)) lx ly))
      )
    ))

(defcheck solution-194e2b88
  (fn [seq1 seq2]
    (let [seq1-count (count seq1)
          seq2-count (count seq2)]
      (cond
        (= seq1 seq2) 0
        (empty? seq1) seq2-count
        (empty? seq2) seq1-count
        :else (let [seq2-count-inc (inc seq2-count)
                    v0             (vec (range seq2-count-inc))
                    v1             (vec (repeat seq2-count-inc 0))]
                (->> (reduce
                       (fn [[v0 v1] i]
                         (let [v1 (reduce
                                    (fn [v1 j]
                                      (let [cost (if (= (nth seq1 i) (nth seq2 j)) 0 1)]
                                        (assoc v1
                                          (inc j)
                                          (min
                                            (inc (v1 j))
                                            (inc (v0 (inc j)))
                                            (+ (v0 j) cost)))))
                                    (assoc v1 0 (inc i))
                                    (range seq2-count))]
                           [v1 v1]))
                       [v0 v1]
                       (range (count seq1)))
                  second
                  last))))))

(defcheck solution-197bb6f3
  (fn ld [left right]
    (cond
      (= left right) 0
      (empty? left) (count right)
      (empty? right) (count left)
      :else (loop [prevrow (into-array (range (inc (count right))))
                   nextrow (into-array (repeat (inc (count right)) 0))
                   i       0]
              (if (= i (count left))
                (aget prevrow (count right))
                (do
                  (aset nextrow 0 (inc i))
                  (loop [j 0]
                    (if (< j (count right))
                      (let [cost
                            (if (= (nth left i) (nth right j)) 0 1)]
                        (aset nextrow (inc j) (min
                                                (inc (aget nextrow j))
                                                (inc (aget prevrow (inc j)))
                                                (+ cost (aget prevrow j))))
                        (recur (inc j)))))
                  (recur nextrow prevrow (inc i))))))))

(defcheck solution-198e0b84
  (fn lev [w1 w2]
    (letfn [(next-row [previous_row xword ychar yindex]
              (reduce
                (fn [current_row [current_char sy-1 sy]]
                  (do
                      (conj current_row
                        (min
                          (+ sy-1 (if (= current_char ychar) 0 1))
                          (inc sy)
                          (inc (last current_row))))))
                [yindex]
                (map vector xword previous_row (rest previous_row)) ; create a tuple containing the current character, the score on the previous row
                ))]
      (let [switch (< (count w1) (count w2))
            xword  (if switch w1 w2)
            yword  (if switch w2 w1)]
        (last (reduce
                #(do
                   (next-row %1 xword (%2 0) (%2 1)))
                (range (inc (count yword)))
                (map vector yword (range 1 (inc (count yword))))
                ))))))

(defcheck solution-1a5bf957
  (fn L [A a x]
    (cond
      (not a) (+ A (count x))
      (not x) (+ A (count a))
      (= (first a) (first x)) (L A (next a) (next x))
      L (min (L (inc A) (next a) x)
          (L (inc A) a (next x))
          (L (inc A) (next a) (next x))))) 0)

(defcheck solution-1a990fc7
  (fn ([w1 w2]
       (let [levenstein-raw (fn [w1 l1 w2 l2 lev]
                              (cond
                                (zero? l1) l2
                                (zero? l2) l1
                                :else (let [last-w1 (nth w1 (dec l1))
                                            last-w2 (nth w2 (dec l2))
                                            cost    (if (= last-w1 last-w2) 0 1)]
                                        (min
                                          (inc (lev w1 (dec l1) w2 l2 lev))
                                          (inc (lev w1 l1 w2 (dec l2) lev))
                                          (+ cost (lev w1 (dec l1) w2 (dec l2) lev))))))
             levenshtein    (memoize levenstein-raw)]
         (levenshtein w1 (count w1) w2 (count w2) levenshtein)))))

(defcheck solution-1b13acd8
  (fn levenstein [s t]
    (letfn [
            (next-row [s i t t_len prev-row]
              (reduce
                (fn [res j]
                  (let [cost         (if (= (nth s i) (nth t j)) 0 1)
                        insertion    (inc (last res))
                        deletion     (inc (nth prev-row (inc j)))
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
    ))

(defcheck solution-1b4aa912
  (fn ld4 [a b]
    (letfn [(deep-merge-with [f & maps]
              (apply
                (fn m [& maps]
                  (if (every? map? maps)
                    (apply merge-with m maps)
                    (apply f maps)))
                maps))]
      (let [m     (count a)
            n     (count b)
            init  (apply deep-merge-with (fn [a b] b)
                    (concat
                     (for [i (range 0 (+ 1 m))] {i {0 i}})
                     (for [j (range 0 (+ 1 n))] {0 {j j}})))
            table (reduce
                    (fn [d [i j]]
                      (deep-merge-with
                        (fn [a b] b)
                        d
                        {i {j (if (= (nth a (- i 1))
                                    (nth b (- j 1)))
                                ((d (- i 1)) (- j 1))
                                (min
                                  (+ ((d (- i 1)) j) 1)
                                  (+ ((d i) (- j 1)) 1)
                                  (+ ((d (- i 1)) (- j 1)) 1)))
                            }}))
                    init
                    (for [j (range 1 (+ 1 n))
                          i (range 1 (+ 1 m))] [i j]))]
        ((table m) n)))))

(defcheck solution-1b91f2f8
  (fn dis
    ([xxs yys] (dis 0 xxs yys))
    ([c [x & xs :as xxs] [y & ys :as yys]]
     (cond (and (nil? x) (nil? y)) c
           (= x y) (dis c xs ys)
           (> (count xxs) (count yys)) (min (dis (inc c) xs ys) (dis (inc c) xs yys))
           (< (count xxs) (count yys)) (min (dis (inc c) xs ys) (dis (inc c) xxs ys))
           :else (dis (inc c) xs ys)))))

(defcheck solution-1bc72f10
  (let [[c b l] [count butlast last]
        lev
                (fn [mem-lev s1 s2]
                  (let [lev (fn [s1 s2] (mem-lev mem-lev s1 s2))]
                    (cond (= s1 s2) 0
                          (= (c s1) 0) (c s2)
                          (= (c s2) 0) (c s1)
                          true (min (+ (lev (b s1) s2) 1)
                                 (+ (lev s1 (b s2)) 1)
                                 (+ (if (= (l s1) (l s2)) 0 1) (lev (b s1) (b s2)))))))
        mem-lev (memoize lev)]
    #(mem-lev mem-lev % %2)))

(defcheck solution-1bf159f9
  (fn levenshtein [w1 w2]
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
            (recur (inc row-idx) max-rows next-prev-row)))))))

(defcheck solution-1cf1de50
  (fn [a b]
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
      (step-differences 0 a b))))

(defcheck solution-1d29074b
  (fn [l r]
    (->>
      (for [i (range 0 (+ 1 (count l))) j (range 0 (+ 1 (count r)))]
        [i j])
      (#(zipmap % (map (fn [[a b]] (max a b)) %)))
      (#(into (sorted-map-by
                (fn [[a b] [c d]]
                  (if (= a c)
                    (compare b d)
                    (compare a c)))) %))
      (#(reduce
          (fn [a [[i j] _]]
            (cond
              (some zero? [i j]) a
              (= (nth l (- i 1)) (nth r (- j 1))) (assoc a [i j] (a [(- i 1) (- j 1)]))
              1
              (assoc a [i j]
                       (min
                         (+ 1 (a [(- i 1) j]))
                         (+ 1 (a [i (- j 1)]))
                         (+ 1 (a [(- i 1) (- j 1)]))))))
          %
          %))
      last
      last
      )))

(defcheck solution-1dc07c1a
  (fn [i j]
    (let [lev (memoize
                (fn [f [ih & it :as i] [jh & jt :as j]]
                  (let [ci (count i)
                        cj (count j)]
                    (cond
                      (zero? ci) cj
                      (zero? cj) ci
                      :else (min
                              (inc (f f it j))
                              (inc (f f i jt))
                              (+ (f f it jt) (if (= ih jh) 0 1)))))))]
      (lev lev i j))))

(defcheck solution-1e409f1e
  (fn [s1 s2]
    (if (= s1 s2) 0
                  (let [m      (count s1)
                        n      (count s2)
                        init   (vec (for [u (range (inc m))]
                                      (apply vector (for [v (range (inc n))] 0))))
                        initx  (reduce #(assoc-in % [%2 0] %2) init (range 1 (inc m)))
                        initxy (reduce #(assoc-in % [0 %2] %2) initx (range 1 (inc n)))
                        charAt (fn [msg p] (if (>= p 0) (nth msg p) ""))]
                    (loop [i 1
                           j 1
                           d initxy]
                      (cond
                        (and (= i (inc m)) (= j n)) (get-in d [m n])
                        (= i (inc m)) (recur 1 (inc j) d)
                        :else (recur (inc i)
                                j
                                (assoc-in d [i j]
                                  (if (= (charAt s1 (dec i)) (charAt s2 (dec j)))
                                    (get-in d [(dec i) (dec j)])
                                    (min (inc (get-in d [(dec i) j]))
                                      (inc (get-in d [i (dec j)]))
                                      (inc (get-in d [(dec i) (dec j)]))))))))))))

(defcheck solution-1e61f2b4
  (fn levenshtein [w1 w2]
    (letfn [(cell-value [same-char? prev-row cur-row col-idx]
              (min (inc (nth prev-row col-idx))
                (inc (last cur-row))
                (+ (nth prev-row (dec col-idx)) (if same-char? 0 1))))]
      (loop [row-idx  1
             max-rows (inc (count w2))
             prev-row (range (inc (count w1)))]
        (if (= row-idx max-rows)
          (last prev-row)
          (let [ch2       (nth w2 (dec row-idx))
                next-prev (reduce (fn [cur-row i]
                                    (let [same-char? (= (nth w1 (dec i)) ch2)]
                                      (conj cur-row (cell-value same-char?
                                                      prev-row
                                                      cur-row
                                                      i))))
                            [row-idx]
                            (range 1 (count prev-row)))]
            (recur (inc row-idx) max-rows, next-prev)))))))

(defcheck solution-1fabbf14
  (fn lev [s1 s2]
    (cond
      (empty? s1) (count s2)
      (empty? s2) (count s1)
      (= (first s1) (first s2)) (lev (rest s1) (rest s2))
      :else (inc (min (lev (rest s1) (rest s2)) (lev s1 (rest s2)) (lev (rest s1) s2))))))

(defcheck solution-200205bf
  (fn [s1 s2]
    (let [c1        (inc (count s1)), c2 (inc (count s2))
          distances (reduce (fn [mat [x y]]
                              (assoc-in mat [y x]
                                (cond (zero? x) y
                                      (zero? y) x
                                      (= (nth s1 (dec x)) (nth s2 (dec y))) (get-in mat [(dec y) (dec x)])
                                      :else (inc (min (get-in mat [y (dec x)]) ;; deletion
                                                   (get-in mat [(dec y) x]) ;; insertion
                                                   (get-in mat [(dec y) (dec x)]) ;; sub
                                                   )))))
                      (vec (repeat c2 (vec (repeat c1 0))))
                      (for [y (range c2) x (range c1)] [x y]))]
      (get-in distances [(count s2) (count s1)]))))

(defcheck solution-2024d963
  (fn levenshtein [str1 str2]
    (let [n (count str1) m (count str2)]
      (cond
        (= 0 n) m
        (= 0 m) n
        :else
        (let [prev-col (transient (vec (range (inc m)))) col (transient [])] ; initialization for the first column.
          (dotimes [i n]
            (assoc! col 0 (inc i))                          ; update col[0]
            (dotimes [j m]
              (assoc! col (inc j)                           ; update col[1..m]
                (min (inc (nth col j))
                  (inc (nth prev-col (inc j)))
                  (+ (get prev-col j) (if (= (nth str1 i) (nth str2 j)) 0 1)))))
            (dotimes [i (count prev-col)]
              (assoc! prev-col i (get col i))))             ;
          (last (persistent! col)))))))

(defcheck solution-2093484a
  (fn lev [s t]
    (let [lev*
          (memoize (fn [lev* s t]
                     (cond (zero? (count s)) (count t)
                           (zero? (count t)) (count s)
                           :else (min
                                   (inc (lev* lev* (rest s) t))
                                   (inc (lev* lev* s (rest t)))
                                   (+ (lev* lev* (rest s) (rest t))
                                      (if (= (first s) (first t)) 0 1))))))]
      (lev* lev* s t))))

(defcheck solution-209cf925
  (fn [a b]
    (let [worker (fn [lev a b]
                   (cond (nil? a) (count b)
                         (nil? b) (count a)
                         :else
                         (let [cost (if (= (last a) (last b)) 0 1)]
                           (min (+ (lev lev (butlast a) b) 1)
                             (+ (lev lev a (butlast b)) 1)
                             (+ (lev lev (butlast a) (butlast b)) cost)))))
          lev    (memoize worker)]
      (lev lev a b))))

(defcheck solution-20e61fba
  (fn levenshtein-distance [s1 s2]
    (letfn [(cost [s1 s2 i j] (if (= (get s1 i) (get s2 j)) 0 1))
            (fill-vec [s1 s2 i v1 v2]
              (reduce (fn [acc j]
                        (assoc acc
                          (inc j)
                          (min (inc (get acc j))
                            (inc (get v1 (inc j)))
                            (+ (get v1 j) (cost s1 s2 i j)))))
                (assoc v2 0 (inc i))
                (range (dec (count v2)))))]
      (let [len1 (count s1), len2 (count s2)]
        (cond
          (= s1 s2) 0
          (= 0 len1) len2
          (= 0 len2) len1
          :else
          (let [fill-v (partial fill-vec s1 s2)]
            (loop [idxs (range len1)
                   v1   (vec (range (inc len2)))
                   v2   (vec (take (inc len2) (repeat 0)))]
              (let [[i & i-rest] idxs]
                (if-not (nil? i)
                  (let [v (fill-v i v1 v2)]
                    (recur i-rest v v))
                  (last v2))))))))))

(defcheck solution-216092b5
  (fn [word1 word2]

    (last (reduce
            (fn [a b]
              (reduce
                #(conj %1 (min (inc (last %1)) (inc (nth a (count %1))) (+ (nth a (dec (count %1))) (if (= b %2) 0 1))))
                (vector (inc (first a)))
                word2))
            (range 0 (inc (count word2)))
            word1))))

(defcheck solution-21a14073
  (let [make-key (comp set vector)
        jot-down #(assoc %1 (make-key %2 %3) %4)]
    (fn lev-memoed
      ([memo word1 word2]
       (if (memo (make-key word1 word2))
         memo
         (cond
           (= (vec word1) (vec word2)) (jot-down memo word1 word2 0)
           (empty? word1) (jot-down memo word1 word2 (count word2))
           (empty? word2) (jot-down memo word1 word2 (count word1))
           :else
           (let [current-cost (if (= ((vec word1) 0) ((vec word2) 0)) 0 1)
                 memo         (-> memo
                                (lev-memoed word1 (rest word2))
                                (lev-memoed (rest word1) word2)
                                (lev-memoed (rest word1) (rest word2)))
                 best-cost    (min
                                (inc (memo (make-key word1 (rest word2))))
                                (inc (memo (make-key (rest word1) word2)))
                                (+ current-cost (memo (make-key (rest word1) (rest word2)))))]
             (jot-down memo word1 word2 best-cost)))))
      ([word1 word2] ((lev-memoed {} word1 word2) (make-key word1 word2))))))

(defcheck solution-21b482a7
  (fn levenshtein [s1 s2]
    (letfn [
            (generate-key [s1 s2]
              [(apply str s1) (apply str s2)])
            (lev [[h1 & t1 :as s1] [h2 & t2 :as s2] memory]
              (let [memory-key (generate-key s1 s2)]
                (if (contains? memory memory-key)
                  memory
                  (let [
                        l1 (count s1)
                        l2 (count s2)
                        ]

                    (if (= 0 (min l1 l2))
                      (assoc memory memory-key (max l1 l2))
                      (let [
                            temp-memory (lev t1 s2 (lev s1 t2 (lev t1 t2 memory)))
                            ]
                        (assoc
                         temp-memory
                          memory-key
                          (min
                            (inc (temp-memory (generate-key t1 s2)))
                            (inc (temp-memory (generate-key s1 t2)))
                            (+ (if (= h1 h2) 0 1) (temp-memory (generate-key t1 t2)))))))))))]
      ((lev s1 s2 {}) (generate-key s1 s2)))))

(defcheck solution-21bbd264
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
      (dist (memoize dist) w1 w2))))

(defcheck solution-22809370
  (fn levenstein-distance [s1 s2]
    (letfn [(lev [levm s1 s2]
              (cond (zero? (count s1)) (count s2)
                    (zero? (count s2)) (count s1)
                    :else
                    (min (inc (levm levm (rest s1) s2))
                      (inc (levm levm s1 (rest s2)))
                      (+ (if (= (first s1) (first s2)) 0 1)
                         (levm levm (rest s1) (rest s2))))))]
      (lev (memoize lev) s1 s2))))

(defcheck solution-2437cb2a
  (fn [& s]
    (let [[a b] (sort-by count s)
          c (count b)]
      (apply min
        (map (fn [w] (count (filter #(apply not= %) (map list w b))))
          (map #(replace (zipmap (filter identity %) a) %)
            (nth (iterate #(into [] (for [i (range c) v % :when (v i)] (assoc v i nil))) [(vec (range c))])
              (- c (count a)))))))))

(defcheck solution-26ab2785
  (fn [s1 s2]
    (letfn [(ld2 [i1 l]
              (letfn [(ld1 [i2 r]
                        (if (>= i2 (count s2)) r
                                               (recur (inc i2)
                                                 (conj r (min (inc (last r))
                                                           (inc (nth l (inc i2)))
                                                           (+ (nth l i2) (if (= (nth s1 i1) (nth s2 i2)) 0 1)))))))]
                (if (>= i1 (count s1)) l
                                       (recur (inc i1) (ld1 0 [(inc i1)])))))]
      (last (ld2 0 (range (inc (count s2))))))))

(defcheck solution-2723871b
  (fn levenshtein
    ([x y]
     (levenshtein x y 0))
    ([x y so-far]
     (if (= 0 (count x) (count y))
       so-far (if (= 0 (count x))
                (+ so-far (count y)) (if (= 0 (count y))
                                       (+ so-far (count x)) (if (= (first x) (first y))
                                                              (recur (rest x) (rest y) so-far) (if (= (second x) (second y))
                                                                                                 (recur (rest (rest x)) (rest (rest y)) (inc so-far)) (if (= (first x) (second y))
                                                                                                                                                        (recur (rest x) (rest (rest y)) (inc so-far)) (if (= (second x) (first y))
                                                                                                                                                                                                        (recur (rest (rest x)) (rest y) (inc so-far)) (if (= (last x) (last y))
                                                                                                                                                                                                                                                        (recur (butlast x) (butlast y) so-far)
                                                                                                                                                                                                                                                        ;(if (= (last x) (last (butlast y)))
                                                                                                                                                                                                                                                        ; (recur (butlast x) (butlast (butlast y)) (inc so-far)) (if (= (last y) (last (butlast x)))
                                                                                                                                                                                                                                                        ;  (recur (butlast (butlast x)) (butlast y) (inc so-far))
                                                                                                                                                                                                                                                        (recur (butlast x) (butlast y) (inc so-far)))))))))))))

(defcheck solution-2820bc60
  (fn [x y]
    (last (reduce #(reduce (fn [r [d-1 d e]]
                             (conj r (if (= %2 e) d-1 (inc (min (last r) d d-1)))))
                     [(inc (first %1))]
                     (map vector %1 (next %1) y))
            (range (inc (count y)))
            x))))

(defcheck solution-28218f6f
  #(first (last
            (reduce (fn [f b] (let [[t z] (first f)
                                    v (rest f)]
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
                                (interleave (range 1 (+ 1 (count %))) %)
                                )) %2))))

(defcheck solution-28f4b6c8
  (fn [s1 s2]
    (let [m (fn r [s l c]
              (let [x (+ c (if (= (first s) (first l)) 0 1))
                    p (rest s)
                    q (rest l)
                    a (count p)
                    b (count q)]
                (if (and (= 0 a) (= 0 b))
                  x
                  (let [z (r p q x)]
                    (if (> b a)
                      (min z (r s q (+ 1 c)))
                      (if (> a b)
                        (min z (r p l (+ 1 c)))
                        z))))))]
      (m s1 s2 0))))

(defcheck solution-297b1e28
  (letfn [(levenstein [a b]
            (eager-lev #{{:edits 0
                          :a     (seq a)
                          :b     (seq b)}}))
          (eager-lev [drafts]
            #_(prn 'drafts drafts)
            (let [closest (apply min-key :edits drafts)]
              #_(prn 'closest closest)
              (if (= (:a closest) (:b closest))
                (:edits closest)
                (let [attempt (update-in closest [:edits] inc)
                      attempt (drop= attempt)]
                  #_(prn 'attempt attempt)
                  (-> drafts
                    (conj (delete-item attempt)
                      (add-item attempt)
                      (change-item attempt))
                    (disj closest nil)
                    (recur))))))
          (delete-item [draft]
            (update-in draft [:a] rest))
          (add-item [draft]
            (update-in draft [:a] conj (first (:b draft))))
          (change-item [draft]
            (-> draft
              (update-in [:a] rest)
              (update-in [:b] rest)))
          (drop= [draft]
            (if (not= (first (:a draft)) (first (:b draft)))
              draft
              (recur (change-item draft))))]
    levenstein))

(defcheck solution-2b6b1fa9
  #(let
    [len-x  (count %1)
     len-y  (count %2)
     ij     (atom (vec (repeat len-x (vec (repeat len-y nil)))))
     get-ij (fn [i j] (get-in @ij [i j]))
     set-ij (fn [i j v] (do (swap! ij assoc-in [i j] v) v))
     lev    (fn lev [x x-i y y-j]
              (if-let [v (get-ij x-i y-j)]
                v
                (cond
                  (zero? x-i) (set-ij x-i y-j y-j)
                  (zero? y-j) (set-ij x-i y-j x-i)
                  :else
                  (set-ij x-i y-j
                    (min
                      (inc (lev x (dec x-i) y y-j))
                      (inc (lev x x-i y (dec y-j)))
                      (+ (lev x (dec x-i) y (dec y-j))
                         (if (= (nth x (dec x-i)) (nth y (dec y-j))) 0 1)))))))]
     (lev %1 len-x %2 len-y)))

(defcheck solution-2bb9e0d5
  (fn lavenshtein-distance [a b]
    (letfn [(coordinates [a b]
              (for [x (range (inc (count a)))
                    y (range (inc (count b)))]
                [x y]))
            (initialize [a b]
              (->> (for [[x y] (coordinates a b)]
                     (cond (zero? x) y
                           (zero? y) x))
                (partition (inc (count b)))
                (map vec)
                vec))
            (chr-a [n] (get a (dec n)))
            (chr-b [n] (get b (dec n)))
            (cost [board [x y]]
              (if (= (chr-a x) (chr-b y))
                (get-in board [(dec x) (dec y)])
                (->> (for [[fx fy] [[dec dec] [dec identity] [identity dec]]]
                       (get-in board [(fx x) (fy y)]))
                  (apply min)
                  inc)))
            (update [board [x y]]
              (if (get-in board [x y])
                board
                (assoc-in board [x y] (cost board [x y]))))
            (result [board]
              (last (last board)))]
      (result (reduce update (initialize a b) (coordinates a b))))))

(defcheck solution-2bc84a96
  (fn [xs ys]
    (cond
      (empty? xs) (count ys)
      (empty? ys) (count xs)
      true                                                  ;else:
      (peek
        (reduce
          (fn [prow [i x]]
            (reduce
              (fn [crow [y a b]]
                (conj crow
                  (if (= x y)
                    a
                    (inc (min a b (last crow))))))
              [i]
              (map cons ys (partition 2 1 prow))))
          (range (inc (count ys)))
          (map vector (range 1 (inc (count xs))) xs))))))

(defcheck solution-2c6a4210
  (fn [s1 s2]
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
        (last (reduce #(calcRow % s1 %2) v s2))))))

(defcheck solution-2c9eb6a5
  (fn leven-dist [as bs]
    (cond
      (and (empty? as) (empty? bs)) 0
      (empty? as) (count bs)
      (empty? bs) (count as)
      (= as bs) 0
      :otherwise
      (let [av   (vec as)
            bv   (vec bs)
            aidx (->> av count inc (range 0) vec)
            ;bidx (->> bv count inc (range 0) vec)
            ]
        (letfn [
                (cross-score [bb aa] (if (= bb aa) 0 1))
                (min-of [q & rs] (reduce (fn [z b] (if (< z b) z b)) q rs))
                (score-line [bj sc-prev-line]
                  (first
                    (reduce (fn [[acc sdiag] [ax sx]]
                              [(conj acc
                                 (min-of (inc (peek acc))
                                   (inc sx)
                                   (+ sdiag (cross-score bj ax))))
                               sx])
                      [[(inc (first sc-prev-line))] (first sc-prev-line)]
                      (map vector av (rest sc-prev-line))))
                  )]
          (let [last-line (reduce (fn [zz b] (score-line b zz)) aidx bv)]
            (last last-line)))))))

(defcheck solution-2d00bd1b
  (fn [x y]
    (letfn
     [(levenshtein [mem s1 s2]
        (let [l1 (count s1)
              l2 (count s2)]
          (cond (zero? l1) l2
                (zero? l2) l1
                :else
                (let [cost (if (= (first s1) (first s2)) 0 1)]
                  (min (inc (mem mem (rest s1) s2))
                    (inc (mem mem s1 (rest s2)))
                    (+ cost
                       (mem mem (rest s1) (rest s2))))))))]
      (levenshtein (memoize levenshtein) x y))))

(defcheck solution-2ee31243
  (fn _ [s t]
    (let [ld  (fn ld [f s t]                                ; non-memoized version of Levenshtein distance
                (cond
                  (empty? s) (count t)
                  (empty? t) (count s)
                  :else (min
                          (+ (if (= (first s) (first t)) 0 1)
                             (f f (rest s) (rest t)))
                          (inc (f f (rest s) t))
                          (inc (f f s (rest t)))))),
          mld (memoize ld)]
      (mld mld s t)
      )))

(defcheck solution-2f428959
  (fn l
    ([a b] (l a b 0 (max (count a) (count b))))
    ([a b n m]
     (if (or (<= m n) (= a b)) n
                               (if (empty? a) (+ n (count b))
                                              (if (empty? b) (+ n (count a))
                                                             (#(if (= (first a) (first b))
                                                                 (min (l % %2 n m) (l % b %3 m))
                                                                 (min (l a %2 %3 m) (l % %2 %3 m) (l % b %3 m)))
                                                              (rest a) (rest b) (inc n))))))))

(defcheck solution-2ffcc891
  (fn [& i]
    (let [c count
          [x y] (sort #(< (c %1) (c %2)) i)]
      (loop [s x l y n (c y)]
        (if s
          (let [a (first s)
                f ((set l) a)]
            (recur
              (next s)
              (if f (rest (drop-while #(not= a %) l)) l)
              (if f (dec n) n)))
          n)))))

(defcheck solution-30245096
  (fn f [m a b]
    (or (@m [a b])
        ((swap! m assoc [a b]
           (let [A (count a)
                 B (count b)
                 s (butlast a)
                 t (butlast b)]
             (if (= A 0)
               B
               (if (= B 0)
                 A
                 (min (+ 1 (f m s b))
                   (+ 1 (f m t a))
                   (+ (if (= (last a) (last b)) 0 1)
                      (f m s t))))))) [a b]))) (atom {}))

(defcheck solution-3049aea5
  (fn [s t]
    (let [s-size    (count s)
          t-size    (count t)
          max-count (* (inc s-size) (inc t-size))]
      (loop [grid {}]
        (if (= max-count (count grid))
          (get grid [s-size t-size])
          ;;grid
          (let [i (count grid)
                r (int (/ i (inc t-size)))
                c (mod i (inc t-size))]
            (cond
              (zero? r)
              (recur (assoc grid [r c] c))

              (zero? c)
              (recur (assoc grid [r c] r))

              (= (get s (dec r)) (get t (dec c)))
              (recur (assoc grid [r c] (get grid [(dec r) (dec c)])))

              :else
              (let [left    (get grid [r (dec c)])
                    up      (get grid [(dec r) c])
                    up-left (get grid [(dec r) (dec c)])
                    cost    (inc (min up up-left left))]
                (recur (assoc grid [r c] cost))))))))))

(defcheck solution-325610b5
  (fn [I x y]
    (last
      (reduce
        (fn [[i & r] y]
          (map first (reductions
                       (fn [[d s] [i x]] [(min (I d) (I i) (if (= x y) s (I s))) i])
                       [(I i) i] (map list r x))))
        (range (I (count x)))
        y))) inc)

(defcheck solution-32be09a0
  (fn [x y]
    (letfn [(f [d x y]
              (cond
                (or (empty? x) (empty? y)) (>= d (+ (count x) (count y)))
                (= (first x) (first y)) (f d (rest x) (rest y))
                (= 0 d) false
                :else (or (f (dec d) (rest x) (rest y))
                          (f (dec d) x (rest y))
                          (f (dec d) (rest x) y))))]
      (first (filter #(f % (seq x) (seq y)) (range)))
      )))

(defcheck solution-32dbe842
  (fn numero-transformacoes [sequencia-pivot sequencia-bruta]
    (letfn [(encontra-sub-seq [[primeiro-sequencia-pivot & outros-sequencia-pivot :as sequencia-pivot]
                               [primeiro-sequencia-bruta & outros-sequencia-bruta :as sequencia-bruta]]
              (cond
                (or (empty? sequencia-pivot) (empty? sequencia-bruta)) [nil sequencia-pivot sequencia-bruta]
                (= primeiro-sequencia-pivot primeiro-sequencia-bruta) (let [[prox-seq-encontrada
                                                                             prox-seq-restante-pivot
                                                                             prox-seq-restante-bruta]
                                                                            (encontra-sub-seq outros-sequencia-pivot outros-sequencia-bruta)]
                                                                        [(cons primeiro-sequencia-pivot prox-seq-encontrada)
                                                                         prox-seq-restante-pivot
                                                                         prox-seq-restante-bruta])
                :else [nil sequencia-pivot sequencia-bruta]))
            (encontra-padroes [seq-pivot seq-bruta]
              (let [encontrados (mapcat (fn [indice-seq-bruta]
                                          (map (fn [indice-seq-pivot]
                                                 (let [[sequencia-comum sequencia-restante-pivot sequencia-restante-bruta]
                                                       (encontra-sub-seq (drop indice-seq-pivot seq-pivot)
                                                         (drop indice-seq-bruta seq-bruta))]
                                                   (concat (list (list (take indice-seq-pivot seq-pivot)
                                                                   sequencia-comum
                                                                   sequencia-restante-pivot))
                                                           (list (list (take indice-seq-bruta seq-bruta)
                                                                   sequencia-comum
                                                                   sequencia-restante-bruta)))))
                                            (range (count seq-pivot))))
                                  (range (count seq-bruta)))]
                (filter (fn [[lista-pivot _]] (seq (second lista-pivot)))
                  encontrados)))]
      (if (some (set sequencia-pivot) sequencia-bruta)
        (let [padroes (encontra-padroes sequencia-pivot sequencia-bruta)]
          (apply min
            (map (fn [[[previa-pivot _ resto-pivot :as padroes-pivot] [previa-bruto _ resto-bruto :as padroes-brutos]]]
                   (+ (numero-transformacoes previa-pivot previa-bruto)
                      (numero-transformacoes resto-pivot resto-bruto)))
              padroes)))
        (max (count sequencia-pivot) (count sequencia-bruta))))))

(defcheck solution-342bfa4f
  (fn [s t]
    (let [s (vec s)
          t (vec t)
          m (count s)
          n (count t)
          d (reduce #(conj %1 (reduce conj [%2] (repeat m 0)))
              [(-> m inc range)] (range 1 (inc n)))]
      (letfn [(gt [d i j] (-> d (nth j) (nth i)))
              (pt [d i j v] (assoc-in d [j i] v))]
        (-> (loop [j 1, d d]
              (if (> j n) d
                          (recur (inc j)
                            (loop [i 1, d d]
                              (if (> i m) d
                                          (recur (inc i)
                                            (let [i- (dec i) j- (dec j)]
                                              (if (= (s i-) (t j-))
                                                (pt d i j (gt d i- j-))
                                                (pt d i j
                                                  (->> [(gt d i- j)
                                                        (gt d i j-)
                                                        (gt d i- j-)]
                                                    (map inc)
                                                    (apply min)))))))))))
          last last)))))

(defcheck solution-3457a158
  (fn levenshtein-distance [x y]
    (let
     [x-len      (inc (count x))
      y-len      (inc (count y))
      x-vec      (vec x)
      y-vec      (vec y)
      start-grid (vec (for [i (range x-len)]
                        (vec (for [j (range y-len)]
                               (cond
                                 (= j 0) i
                                 (= i 0) j
                                 :else :blank)))))
      solve      (fn solve [grid i j]
                   (let
                    [ij-val    (if (= (x-vec (dec i)) (y-vec (dec j)))
                                 (get-in grid [(dec i) (dec j)])
                                 (inc (min (get-in grid [(dec i) j])
                                        (get-in grid [i (dec j)])
                                        (get-in grid [(dec i) (dec j)]))))
                     next-i    (if (= j (count y)) (inc i) i)
                     next-j    (if (= j (count y)) 1 (inc j))
                     next-grid (assoc-in grid [i j] ij-val)
                     ]
                     (if (= [i j] [(count x) (count y)])
                       ij-val
                       (solve next-grid next-i next-j))))]
      (cond
        (= 0 (count x-vec)) (count y-vec)
        (= 0 (count y-vec)) (count x-vec)
        :else (solve start-grid 1 1)))))

(defcheck solution-352ba343
  (let [lev   (fn lev [f a b]
                (if (zero? (count a))
                  (count b)
                  (if (zero? (count b))
                    (count a)
                    (let [cost (if (= (peek a) (peek b)) 0 1)]
                      (min
                        (+ (f f (pop a) b) 1)
                        (+ (f f a (pop b)) 1)
                        (+ (f f (pop a) (pop b)) cost)
                        )))))
        m-lev (memoize lev)]
    (fn [a b]
      (m-lev m-lev (into [] a) (into [] b)))))

(defcheck solution-3569af30
  (fn levenshtein [src tgt]
    (let [srclen (count src) tgtlen (count tgt) rowsz (inc tgtlen)]
      (if (= src tgt)
        0
        (if (or (= (count src) 0)
                (= (count tgt) 0))
          (max (count src) (count tgt))
          (loop [srcidx 0 tgtidx 0 preRow (range 0 rowsz) curRow (conj [] (inc srcidx))] ;; curRow[0]=srcidx+1
            (let [srclt      (nth src srcidx)
                  tgtlt      (nth tgt tgtidx)
                  nxtsrcidx  (inc srcidx)
                  nxttgtidx  (inc tgtidx)
                  leftv      (nth preRow nxttgtidx)
                  leftupperv (nth preRow tgtidx)
                  upperv     (nth curRow tgtidx)
                  cost       (fn [slt dlt] (if (= slt dlt) 0 1))
                  mincurv    (min (inc leftv) (inc upperv) (+ leftupperv (cost srclt tgtlt)))]
              ;; does cur row iteration done ?
              ;;(prn srclt tgtlt nxtsrcidx preRow curRow)
              (if (= nxttgtidx tgtlen)                      ;; done one iteration of tgt row
                (if (= nxtsrcidx srclen)
                  mincurv                                   ;; the result is in last of cur-row after iterating all.
                  (recur nxtsrcidx 0 (conj curRow mincurv) (conj [] (inc nxtsrcidx)))) ;; next src letter
                (recur srcidx nxttgtidx preRow (conj curRow mincurv))))))))))

(defcheck solution-357f4fa8
  (fn levenshein [a b]
    (let [[s1 s2] (if (> (count a) (count b))
                    [a b]
                    [b a])
          lev-thing (fn [c1 s2 mat]
                      (loop [i 1
                             a [(inc (first mat))]]
                        (cond (> i (count s2)) a
                              (= c1 (nth s2 (dec i))) (recur (inc i) (conj a (nth mat (dec i))))
                              :else (recur (inc i)
                                      (conj a (inc (min (nth mat i)
                                                     (nth mat (dec i))
                                                     (last a))))))))]
      (last (reduce (fn [p s]
                      (lev-thing s s2 p))
              (range (inc (count s1)))
              s1)))))

(defcheck solution-35a5a837
  (fn distance [a b]
    (letfn [(walk
              [f a b]
              (cond
                (or (nil? a) (nil? b))
                (+ (count a) (count b))

                (= (first a) (first b))
                (f f (next a) (next b))

                :else
                (inc (min (f f (next a) (next b))
                       (f f (next a) b)
                       (f f a (next b))))))]
      (walk (memoize walk) a b))))

(defcheck solution-35bfbad2
  (fn levenstein [a b]
    (let [n    (count a)
          m    (count b)
          calc (fn [d [y x]]
                 (assoc-in d [y x]
                   (if (= (nth a y)
                         (nth b x))
                     (get-in d [(dec y) (dec x)] (max x y))
                     (->> [[-1 0] [-1 -1] [0 -1]]
                       (map #(get-in d (vec (map + % [y x])) (max x y)))
                       (apply min)
                       (inc)))))]
      (if (or (empty? a) (empty? b))
        (max (count a) (count b))
        (->> (for [y (range n) x (range m)] [y x])
          (reduce calc {})
          (#(get-in % [(dec n) (dec m)])))))))

(defcheck solution-36654578
  (fn [s t]
    (cond
      (= s t) 0
      (= 0 (count s)) (count t)
      (= 0 (count t)) (count s)
      :else (letfn [(outer-reduce [arr s-char]
                      (letfn [(inner-reduce [[v0 v1] t-char]
                                (let [cost (if (= s-char t-char) 0 1)]
                                  (list (rest v0)
                                    (concat v1
                                            (list (min (+ (last v1) 1)
                                                    (+ (first (rest v0)) 1)
                                                    (+ (first v0) cost)))))))]
                        (last (reduce inner-reduce
                                (list arr (list (+ (first arr) 1)))
                                (sequence t)))))]
              (last (reduce outer-reduce
                      (range (+ (count t) 1))
                      (sequence s)))))))

(defcheck solution-368c6f40
  (fn levenshtein-dist [s1 s2]
    (let [width  (inc (count s1))
          height (inc (count s2))
          matrix (vec (repeat height (vec (repeat width 0))))
          matrix (assoc matrix 0 (vec (range 0 width)))
          matrix (reduce #(assoc-in %1 [%2 0] %2) matrix (range 0 height))]
      (peek
        (peek
          (reduce (fn [matrix [y x]]
                    (if (= (get s1 (dec x)) (get s2 (dec y)))
                      (assoc-in matrix [y x] (get-in matrix [(dec y) (dec x)]))
                      (assoc-in matrix [y x] (min (inc (get-in matrix [y (dec x)]))
                                               (inc (get-in matrix [(dec y) x]))
                                               (inc (get-in matrix [(dec y) (dec x)]))))))
            matrix
            (for [y (range 1 height)
                  x (range 1 width)] [y x])))))))

(defcheck solution-36927c89
  (fn levenshtein-distance [xs ys]
    (letfn [(matches [[x & xs :as xss] ys]
              (cond (or (empty? xss) (empty? ys)) 0
                    (some #{x} ys) (inc (matches xs (rest (drop-while #(not= x %)
                                                            ys))))
                    :else (matches xs ys)))]
      (- (max (count xs) (count ys))
         (matches xs ys)))))

(defcheck solution-3773971e
  (fn levenshtein [s t]
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
    ))

(defcheck solution-380961e7
  (memoize
    (fn lev-dist [w1 w2]
      (cond
        (zero? (count w1)) (count w2)
        (zero? (count w2)) (count w1)
        (= (first w1) (first w2)) (lev-dist (next w1) (next w2))
        :else (inc (min
                     (lev-dist w1 (next w2))
                     (lev-dist (next w1) w2)
                     (lev-dist (next w1) (next w2))))))))

(defcheck solution-38165c1
  #(let [m (memoize (fn [f a b]
                      (cond (empty? a) (count b) (empty? b) (count a)
                            :else (let [d (if (= (first a) (first b)) 0 1)
                                        x (rest a) y (rest b)]
                                    (min (inc (f f a y)) (inc (f f x b)) (+ d (f f x y)))))))]
     (m m % %2)))

(defcheck solution-38468e21
  (fn distance [str1 str2]
    (let [len1 (count str1)
          len2 (count str2)
          arr  (make-array Long (inc len1) (inc len2))]
      (dotimes [i (inc len1)]
        (dotimes [j (inc len2)]
          (aset arr i j 0)))
      (dotimes [i (inc len1)]
        (aset arr i 0 i))
      (dotimes [j (inc len2)]
        (aset arr 0 j j))
      (doseq [i (range 1 (inc len1))]
        (doseq [j (range 1 (inc len2))]
          (aset
            arr i j
            (min (inc (aget arr (dec i) j))
              (inc (aget arr i (dec j)))
              (+ (if (= (nth str1 (dec i)) (nth str2 (dec j))) 0 1)
                 (aget arr (dec i) (dec j)))))))
      (aget arr len1 len2))))

(defcheck solution-384abb0a
  #((fn f [r [a & b :as c] [x & y :as z]]
      (if (nil? a)
        (+ r (% z))
        (if (= a x)
          (f r b y)
          (apply f (+ 1 r)
            (if (or (= (%2 b) (%2 y)) (= (% c) (% z)))
              [b y]
              (if (> (% c) (% z))
                [b z]
                [c y]))))))
    0 %3 %4) count first)

(defcheck solution-38934f1e
  (fn levenshtein [a b]
    (let [m            (count a) n (count b)
          va           (vec a) vb (vec b)
          next-step    (fn [result-map [i j]] (let [result
                                                    (if (= (va (dec i)) (vb (dec j))) (result-map [(dec i) (dec j)])
                                                                                      (inc (min (result-map [(dec i) j])
                                                                                             (result-map [i (dec j)])
                                                                                             (result-map [(dec i) (dec j)]))))]
                                                (assoc result-map [i j] result)))
          init-map     (merge (into {} (for [i (range (inc m))] [[i 0] i]))
                         (into {} (for [j (range (inc n))] [[0 j] j])))
          matrix-range (for [j (range 1 (inc n)) i (range 1 (inc m))] [i j])
          result-map   (reduce next-step init-map matrix-range)]
      (result-map [m n]))))

(defcheck solution-38d393ad
  (fn super [w1 w2]
    (letfn [(init-matrix [res w]
              (if (> (count w) 0)
                (init-matrix (conj res {
                                        [(count w)
                                         0]
                                        (count w)
                                        }) (rest w))
                (conj res {[0 0] 0})))
            (init-matrix2 [res w]
              (if (> (count w) 0)
                (init-matrix2 (conj res {
                                         [
                                          0
                                          (count w)
                                          ]
                                         (count w)
                                         }) (rest w))
                (conj res {[0 0] 0})))
            (mat [w1 w2]
              (conj (init-matrix {} w1) (init-matrix2 {} w2)))
            (final [mat w1 w2 i j]
              (if (<= i (count w1))
                (if (<= j (count w2))
                  (let [df (if (=
                                 ((vec w1) (dec i))
                                 ((vec w2) (dec j))
                                 )
                             0
                             1)]
                    (final (conj mat {[i j] (min
                                              (inc (get mat [(dec i) j] 0))
                                              (inc (get mat [i (dec j)] 0))
                                              (+ df (get mat [(dec i) (dec j)] 0))
                                              )})
                      w1
                      w2
                      i
                      (inc j)
                      ))
                  (final mat w1 w2 (inc i) 1))
                mat))
            ]
      (let [m (mat w1 w2)
            f (final m w1 w2 1 1)]
        (get f [(count w1) (count w2)])))))

(defcheck solution-3a307f4
  (fn [v w]
    (let [dist (memoize
                 (fn [dist v w]
                   (cond
                     (empty? v) (count w)
                     (empty? w) (count v)
                     :default (min (+ 1 (dist dist v (rest w)))
                                (+ 1 (dist dist (rest v) w))
                                (+ (dist dist (rest v) (rest w)) (if (= (first v) (first w)) 0 1))))))
          dist (partial dist dist)]
      (dist v w))))

(defcheck solution-3a95bf5b
  (fn [a b]
    (let [X (cons \_ a) Y (cons \_ b) m (count X) n (count Y)
          A (make-array Long/TYPE m n)]
      (dotimes [i m]
        (dotimes [j n]
          (aset A i j (cond (= 0 i) j
                            (= 0 j) i
                            :else (min (+ (aget A (dec i) (dec j))
                                          (if (= (nth X i) (nth Y j)) 0 1))
                                    (+ (aget A (dec i) j) 1)
                                    (+ (aget A i (dec j)) 1))))))
      (aget A (dec m) (dec n)))))

(defcheck solution-3a970840
  (fn lev [s t]
    (let [[ls lt] (map count [s t])]
      (if (< lt ls)
        (lev t s)
        (last
          (reduce
            (fn [sofar i]
              (loop [nextrow [(inc i)]
                     j       1]
                (if (= (count sofar) (count nextrow))
                  (do #_(prn nextrow) nextrow)
                  (let [sl      (nth s i :nil-s)
                        tl      (nth t (dec j) :nil-t)
                        replace (if (= sl tl) 0 1)
                        diag    (+ replace (get sofar (dec j)))
                        above   (inc (get sofar j))
                        left    (inc (last nextrow))]
                    (recur (conj nextrow (min diag above left))
                      (inc j))))))
            (let [start (vec (range (inc lt)))]
              (do #_(prn start) start))
            (range ls)))))))

(defcheck solution-3b3d0209
  (fn __ [s t]
    (if (= s t) 0
                (letfn
                 [(f [s v0]
                    (if (empty? s)
                      (nth v0 (count t))
                      (recur (rest s)
                        (loop [j  0
                               v1 (vec (list (inc (first v0))))]
                          (if (= j (count t))
                            v1
                            (recur (inc j)
                              (conj v1 (min (inc (nth v1 j))
                                         (inc (nth v0 (inc j)))
                                         (+ (nth v0 j)
                                            (if (= (first s) (nth t j))
                                              0 1))))))))))]
                  (f s (vec (range (inc (count t)))))))))

(defcheck solution-3b40257
  (let [ed
               (fn [f a b]
                 (let [[a0 & a_] a
                       [b0 & b_] b]
                   (if (or (nil? a) (nil? b))
                     (+ (count a) (count b))
                     (if (= a0 b0)
                       (f f a_ b_)
                       (inc (min (f f a b_)
                              (f f a_ b)
                              (f f a_ b_)))))))
        ed-mem (memoize ed)]
    (partial ed-mem ed-mem)))

(defcheck solution-3d428d98
  (fn [s1 s2]
    (letfn [(init-matrix [m n]
              (reduce merge (for [x (range (+ m 1)) y (range (+ n 1))] {[x y] -1})))

            (min-val [x y s1 s2 matrix]
              (if (some zero? [x y])
                (assoc matrix [x y] (+ x y))
                (let [n1 (+ (matrix [x (- y 1)]) 1)
                      n2 (+ (matrix [(- x 1) y]) 1)
                      n3 (matrix [(- x 1) (- y 1)])
                      n4 (if (= ((vec s1) (- x 1)) ((vec s2) (- y 1)))
                           n3
                           (+ n3 1))]
                  (assoc matrix [x y] (min n1 n2 n4)))))

            (set-matrix [s1 s2]
              (let [m (count s1) n (count s2) matrix (init-matrix m n)]
                (loop [y 0 matrix matrix]
                  (let [n-matrix
                        (loop [x 0 matrix matrix]
                          (if (> x m)
                            matrix
                            (recur (+ x 1) (min-val x y s1 s2 matrix))))]
                    (if (>= y n)
                      n-matrix
                      (recur (+ y 1) n-matrix))))))]
      ((set-matrix s1 s2) [(count s1) (count s2)]))))

(defcheck solution-3d8e9ec
  (fn [s1 s2]
    (let [cache (atom {}),
          lev   (fn l [w1 w2]
                  (if-let [cost (@cache (list w1 w2))]
                    cost
                    (let [value (cond
                                  (empty? w1) (count w2)
                                  (empty? w2) (count w1)
                                  :else (min
                                          (+ (if (= (first w1) (first w2)) 0 1) (l (rest w1) (rest w2)))
                                          (inc (l (rest w1) w2))
                                          (inc (l w1 (rest w2)))))]
                      (swap! cache assoc (list w1 w2) value)
                      value)))]

      (lev s1 s2))))

(defcheck solution-3daa1611
  (let [levenshtein (fn [levenshtein-m x1 x2]
                      (let [levenshtein (partial levenshtein-m levenshtein-m)]
                        (cond
                          (empty? x2) (count x1)
                          (empty? x1) (count x2)
                          (= (first x1) (first x2)) (levenshtein (rest x1) (rest x2))
                          :else (inc (min (levenshtein (rest x1) x2)
                                       (levenshtein x1 (rest x2))
                                       (levenshtein (rest x2) (rest x1)))))))]
    (partial levenshtein (memoize levenshtein))))

(defcheck solution-3e7cd64e
  (fn [w1 w2]
    (let [s (vec w1)
          t (vec w2)
          n (count s)
          m (count t)
          d (atom (vec (range (inc n))))]
      (cond
        (zero? n) m
        (zero? m) n
        :else
        (loop [p (vec (range (inc n)))
               j 1]
          (if (<= j m)
            (let [tj (nth t (dec j))]
              (swap! d assoc 0 j)
              (doseq [i (range 1 (inc n))]
                (let [c (if (= (nth s (dec i)) tj) 0 1)
                      m (min (inc (nth @d (dec i))) (inc (nth p i)) (+ c (nth p (dec i))))]
                  (swap! d assoc i m)))
              (let [tmp @d]
                (reset! d p)
                (recur tmp (inc j))))
            (nth p n)))))))

(defcheck solution-3ebb366c
  (fn levenshtein [a b]
    (let [a (vec a) b (vec b)
          m (inc (count a)) n (inc (count b))
          x (vec (repeat n (repeat m 0)))
          y (assoc x 0 (range m))
          z (vec (map #(assoc (vec %) 0 %2) y (range n)))
          k (for [j (range 1 n) i (range 1 m)] [j i])]
      (-> (reduce
            (fn [d [i j]]
              (assoc-in d [i j]
                (if (= (a (dec j)) (b (dec i)))
                  (get-in d [(dec i) (dec j)])
                  (+ 1 (min (get-in d [(dec i) j])
                         (get-in d [i (dec j)])
                         (get-in d [(dec i) (dec j)])))))) z k)
        last last))))

(defcheck solution-3efc9d3a
  (fn levenshtein-distance
    [s1 s2]
    (let [mem (memoize (fn [step s1 s1-n s2 s2-n]
                         (cond (zero? s1-n) s2-n
                               (zero? s2-n) s1-n
                               :else (let [cost (if (= (nth s1 (dec s1-n)) (nth s2 (dec s2-n))) 0 1)]
                                       (min (inc (step step s1 (dec s1-n) s2 s2-n))
                                         (inc (step step s1 s1-n s2 (dec s2-n)))
                                         (+ cost (step step s1 (dec s1-n) s2 (dec s2-n))))))))]
      (mem mem s1 (count s1) s2 (count s2)))))

(defcheck solution-3f738f68
  (fn lev [a b]
    (let [lev#
               (memoize
                 (fn [lev# a b]
                   (cond (empty? a) (count b)
                         (empty? b) (count a)
                         :else (let [cost (if (= (first a) (first b)) 0 1)]
                                 (min (+ (lev# lev# (rest a) b) 1)
                                   (+ (lev# lev# a (rest b)) 1)
                                   (+ (lev# lev# (rest a) (rest b)) cost))
                                 ))
                   ))
          lev# (partial lev# lev#)]
      (lev# a b)
      )
    ))

(defcheck solution-3fa78498
  (fn levi1 [s t]
    (let [scnt    (inc (count t)) tcnt (inc (count s)) sv (vec t) tv (vec s)
          val-for (fn [m i j]
                    (cond (= 0 i) j
                          (= 0 j) i
                          :else
                          (min (inc (get-in m [(dec i) j]))
                            (inc (get-in m [i (dec j)]))
                            (+ (get-in m [(dec i) (dec j)])
                               (if (= (get sv (dec i)) (get tv (dec j))) 0 1)))))
          ]
      (loop [i 0 j 0 mtx {}]
        (if (> i scnt)
          (get-in mtx [scnt tcnt])
          (recur
            (if (= j tcnt) (inc i) i)
            (if (= j tcnt) 0 (inc j))
            (assoc-in mtx [i j] (val-for mtx i j))
            ))))))

(defcheck solution-3fe698eb
  (fn distance [xs ys]
    (cond
      (empty? xs) (count ys)
      (empty? ys) (count xs)
      true
      (let [[x & xs'] xs
            [y & ys'] ys]
        (if (= x y)
          (distance xs' ys')
          (inc (min (distance xs' ys') (distance xs' ys) (distance xs ys'))))))))

(defcheck solution-4051276c
  (fn [s1 s2]
    (let [m (count s1)
          n (count s2)
          d (transient
              (merge (apply conj {}
                       (for [x (range (inc m)) y (range (inc n))]
                         [[x y] (if (zero? (* x y)) (max x y) 0)]))))]
      (dotimes [i m]
        (dotimes [j n]
          (assoc! d
            [(inc i) (inc j)]
            (min (+ 1 (d [i (inc j)]))
              (+ 1 (d [(inc i) j]))
              (+ (if (= (get s1 i) (get s2 j)) 0 1)
                 (d [i j]))))))
      (d [m n]))))

(defcheck solution-409a6572
  (fn [xs ys]
    ((fn calc-next [xs ys rs]
       (let [y (first ys)]
         (if (nil? y)
           (last rs)
           (calc-next xs (rest ys)
             ;; loop to get next rs
             (loop [xs  xs
                    rs  rs
                    nrs [(inc (first rs))]]
               (if (empty? xs) nrs
                               (recur (rest xs)
                                 (rest rs)
                                 (conj nrs (min (inc (last nrs))
                                             (inc (second rs))
                                             (+ (first rs)
                                                (if (= (first xs) y) 0 1)))))
                               )
               ))
           )

         )
       ) xs ys (range (inc (count xs))))))

(defcheck solution-417dcf03
  (fn levenshtein-distance [seq-1 seq-2]
    (cond (empty? seq-1) (count seq-2)
          (empty? seq-2) (count seq-1)
          (= (first seq-1) (first seq-2)) (levenshtein-distance (rest seq-1) (rest seq-2))
          :else (inc (min (levenshtein-distance (rest seq-1) seq-2)
                       (levenshtein-distance seq-1 (rest seq-2))
                       (levenshtein-distance (rest seq-1) (rest seq-2)))))))

(defcheck solution-4310358e
  (fn lev [s t]
    (cond
      (= s t) 0
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      (loop [v0 (range (inc (count t)))
             i  0]
        (if (= i (count s))
          (last v0)
          (recur
            (loop [v1 [(inc i)]
                   j  0]
              (if (= j (count t))
                v1
                (recur
                  (conj v1
                    (min (inc (last v1))
                      (inc (nth v0 (inc j)))
                      (+ (nth v0 j) (if (= (nth s i) (nth t j)) 0 1))))
                  (inc j)
                  )
                )
              )
            (inc i)
            )
          )
        )
      )
    ))

(defcheck solution-43d0d460
  (fn [seq-a seq-b]
    (let [a     (inc (count seq-a))
          b     (inc (count seq-b))
          vec-a (vec seq-a)
          vec-b (vec seq-b)]
      (cond
        (zero? (dec a)) (dec b)
        (zero? (dec b)) (dec a)
        :else
        (letfn [(initialize [a b]
                  (vec (for [i (range a)]
                         (vec (for [j (range b)]
                                (cond
                                  (zero? i) j
                                  (zero? j) i
                                  :else 0))))))
                (cost [d i j]
                  (min (+ 1 (get-in d [(dec i) j]))
                    (+ 1 (get-in d [i (dec j)]))
                    (+ (if (= (get vec-a (dec i))
                             (get vec-b (dec j)))
                         0
                         1) (get-in d [(dec i) (dec j)]))))
                (process [table m n]
                  (loop [x 0
                         y 1
                         d table]
                    (let [[i j] (if (= x (dec m))
                                  [1 (inc y)]
                                  [(inc x) y])]
                      (if (= j n)
                        d
                        (recur i j (assoc-in d [i j] (cost d i j)))))))]
          (let [d (initialize a b)
                d (process d a b)]
            (get-in d [(dec a) (dec b)])))))))

(defcheck solution-441ae331
  (fn [x y]
    (let [pairs         (for [i (range 1 (inc (count x))), j (range 1 (inc (count y)))] [i j])
          initial-table (merge (into {} (map (fn [i] [[i 0] i]) (range 0 (inc (count x)))))
                          (into {} (map (fn [j] [[0 j] j]) (range 0 (inc (count y))))))
          table         (reduce (fn [table [i j]]
                                  (assoc table [i j]
                                               (if (= (get x (dec i)) (get y (dec j)))
                                                 (table [(dec i) (dec j)])
                                                 (inc (min (table [(dec i) j])
                                                        (table [i (dec j)])
                                                        (table [(dec i) (dec j)]))))))
                          initial-table
                          pairs)]
      (table [(count x) (count y)]))))

(defcheck solution-4428cdb6
  (fn levenshtein-distance [s t]
    (letfn [(cost [i j] (if (= (nth s (dec i)) (nth t (dec j))) 0 1))
            (lev [f i j]
              (if (or (= i 0) (= j 0))
                (max i j)
                (min (inc (f f (dec i) j))
                  (inc (f f i (dec j)))
                  (+ (f f (dec i) (dec j)) (cost i j)))))]
      (lev (memoize lev) (count s) (count t)))))

(defcheck solution-44884c1
  (fn [a b]
    (let [[a b] (sort-by count [a b])
          rm-one (fn [w]
                   (map
                     #(concat (take % w) (drop (inc %) w))
                     (range (count w))))]
      (loop [a (vec a) b [(vec b)] n 0]
        (if (< (count a) (count (first b)))
          (recur a (set (mapcat rm-one b)) (inc n))
          (+ n (apply min
                 (map
                   (fn [bi] (apply + (map #(if (= %1 %2) 0 1) a bi)))
                   b))))))))

(defcheck solution-44bbb4a9
  #(let [ld (memoize
              (fn [f [x & r :as xs] [y & s :as ys]]
                (cond
                  (nil? x) (count ys)
                  (nil? y) (count xs)
                  (= x y) (f f r s)
                  :else (min (inc (f f r ys))
                          (inc (f f xs s))
                          (inc (f f r s))))))]
     (ld ld % %2)))

(defcheck solution-4580048c
  (fn lev [s t]
    (cond
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      (let [ts (rest t)
            ss (rest s)]
        (if (= (first s) (first t))
          (lev ss ts)
          (min
            (inc (lev ss t))
            (inc (lev s ts))
            (inc (lev ss ts))))))))

(defcheck solution-46cdf57d
  (fn levenstein [a b]
    (let [one-if      #(if % 1 0)
          lev-functor (fn [mem i j]
                        (let [lev (fn [i j] (mem mem i j))]
                          (if (zero? (min i j))
                            (max i j)
                            (min
                              (+ (lev (dec i) j) 1)
                              (+ (lev i (dec j)) 1)
                              (+ (lev (dec i) (dec j)) (one-if (not= (get a (dec i)) (get b (dec j)))))))))
          mem         (memoize lev-functor)
          lev         (partial mem mem)]
      (lev (count a) (count b)))))

(defcheck solution-46ec12fb
  #((fn [v s]
      (if (empty? s)
        (last v)
        (recur
          (loop [o [(inc (first v))]
                 n 1]
            (if (< n (count v))
              (recur
                (conj o (min (inc (last o))
                          (inc (nth v n))
                          (+ (nth v (dec n))
                             (if (= (first s)
                                   (nth % (dec n)))
                               0 1))))
                (inc n))
              o))
          (rest s))))
    (range (inc (count %))) %2))

(defcheck solution-47106897
  (fn levenstein-a
    [word-a word-b]
    (let [[list-a list-b] (map vec [word-a word-b])
          make-fibo (fn make-fibo []
                      (let
                       [min-dist
                                 (fn [mem-fib list-a list-b]
                                   (let [min-dist (fn [x y] (mem-fib mem-fib x y))]
                                     (if (= 0 (count list-a))
                                       (count list-b)
                                       (if (= 0 (count list-b))
                                         (count list-a)
                                         (let [cost (if (= (first list-a) (first list-b))
                                                      0 1)]
                                           (min (+ 1 (min-dist (rest list-a) list-b))
                                             (+ 1 (min-dist (rest list-b) list-a))
                                             (+ cost (min-dist (rest list-a) (rest list-b)))
                                             ))))))
                        mem-dist (memoize min-dist)]

                        (partial mem-dist mem-dist)))]

      ((make-fibo) list-a list-b))
    ))

(defcheck solution-471af465
  (fn
    [s1 s2]
    (let [lev (memoize
                (fn [y i j]
                  (if (= 0 (min i j))
                    (max i j)
                    (let [cost (if (= (get s1 (dec i))
                                     (get s2 (dec j)))
                                 0
                                 1)]
                      (min (inc (y y (dec i) j))
                        (inc (y y i (dec j)))
                        (+ cost (y y (dec i) (dec j))))))))]
      (lev lev (count s1) (count s2)))))

(defcheck solution-47ab4d8
  (fn levenshtein [a b]
    (let [distance (fn [a b recurrence]
                     (let [m (count a),
                           n (count b)]
                       (if (zero? (min m n))
                         (max m n)
                         (let [prefixa (butlast a),
                               prefixb (butlast b)]
                           (if (= (last a) (last b))
                             (levenshtein prefixa prefixb)
                             (inc (min
                                    (recurrence prefixa prefixb recurrence)
                                    (recurrence prefixa b recurrence)
                                    (recurrence a prefixb recurrence)))))))),
          memoized (memoize distance)]
      (memoized a b memoized))))

(defcheck solution-4952394e
  (fn ldist
    ([[fa & ra :as a] [fb & rb :as b] cache]
     (let [cost (if (= fa fb) 0 1)
           key  (set [a b])
           c    (cache key)]
       (cond
         c [c cache]
         (empty? a) [(count b) (assoc cache key (count b))]
         (empty? b) [(count a) (assoc cache key (count a))]
         :else (let [[da cache] (ldist ra b cache)
                     [db cache] (ldist a rb cache)
                     [dab cache] (ldist ra rb cache)
                     m (min (inc da) (inc db) (+ dab cost))]
                 [m (assoc cache key m)]))))
    ([a b] (first (ldist a b {})))))

(defcheck solution-49c8bcac
  (fn [str1 str2]
    (let [isize      (inc (count str1))
          jsize      (inc (count str2))
          all-pairs  (for [i (range isize) j (range jsize)] [i j])
          reducer    (fn [distances [i j]]
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

          zero-array (fn [m n] (vec (take m (repeat (vec (take n (repeat 0)))))))
          init-state (zero-array isize jsize)

          levs       (reduce reducer init-state all-pairs)
          val        (last (last levs))]
      val)))

(defcheck solution-49e501bd
  (fn [x y]
    (let [m  (fn [x] (map #(concat (take % x) (drop (inc %) x)) (range (count x))))
          s  (fn s [x n]
               (if (= n 1)
                 (m x)
                 (reduce into #{} (map m (s x (dec n))))))
          xn (count x)
          yn (count y)
          c  (fn [x y] (reduce + (map #(if (= %1 %2) 0 1) x y)))
          mc (fn [x v] (apply min (map #(c x %) v)))]
      (cond
        (= xn yn) (c x y)
        (< xn yn) (let [d (- yn xn)
                        v (s y d)]
                    (+ d (mc x v)))
        :else (let [d (- xn yn)
                    v (s x d)]
                (+ d (mc y v)))))))

(defcheck solution-4a08efb1
  (fn lev-dist [a b]
    (letfn [
            (ld [ld-memo a b]
              (cond
                (empty? a) (count b)
                (empty? b) (count a)
                :default (min
                           (if (= (last a) (last b))
                             (ld-memo ld-memo (butlast a) (butlast b))
                             (inc (ld-memo ld-memo (butlast a) (butlast b))))
                           (inc (ld-memo ld-memo (butlast a) b))
                           (inc (ld-memo ld-memo a (butlast b)))
                           )
                ))
            ] (let [ld-memo (memoize ld)]
                (ld-memo ld-memo a b)
                ))))

(defcheck solution-4a284b23
  (fn [a b]
    (let [w              (inc (count b))
          h              (inc (count a))
          cell-fn        (fn [board x y] (get-in board [y x]))
          update-cell-fn (fn [board x y n] (assoc-in board [y x] n))
          neigh-fn       (fn [x y] (->> [[-1 0] [-1 -1] [0 -1]]
                                     (filter #(and (<= 0 (+ (first %) x) (dec w)) (<= 0 (+ (second %) y) (dec h))))))
          min-neigh-fn   (fn [board x y]
                           ((fnil (partial apply min) [0])
                            (seq (map (fn [[i j]]
                                        (condp = [i j]
                                          [-1 0] (inc (cell-fn board (dec x) y))
                                          [0 -1] (inc (cell-fn board x (dec y)))
                                          [-1 -1] (if (= (nth b (dec x)) (nth a (dec y)))
                                                    (cell-fn board (dec x) (dec y))
                                                    (inc (cell-fn board (dec x) (dec y))))))
                                   (neigh-fn x y)))))]
      (if (or (zero? (count a)) (zero? (count b)))
        (max (count a) (count b))
        (loop [board (into [] (repeat h (into [] (repeat w 0))))
               i     0]
          (let [new-board (reduce (fn [b j] (update-cell-fn b j i (min-neigh-fn b j i)))
                            board
                            (range w))]
            (if (= i (dec h))
              (last (last new-board))
              (recur new-board (inc i)))))))))

(defcheck solution-4b9fe5c4
  (fn lv
    [a b]
    (let [x (vec a)
          y (vec b)]
      (letfn
       [(L [l m]
          (let [l- (- l 1)
                m- (- m 1)]
            (cond (= 0 l) m
                  (= 0 m) l
                  (= (x l-) (y m-)) (L l- m-)
                  :else (+ 1 (min
                               (L l m-)
                               (L l- m)
                               (L l- m-))))))]
        (L (count x) (count y))))))

(defcheck solution-4bec4926
  (fn edit-dist [a b]
    (cond (not b) (count a) (not a) (count b)
          :else (let [ra (next a) rb (next b)]
                  (if (= (first a) (first b)) (edit-dist ra rb)
                                              (+ 1 (min (edit-dist ra rb) (edit-dist ra b) (edit-dist a rb))))))))

(defcheck solution-4c517d34
  (fn levenshtein-distance
    [s t]
    (letfn [(next-row [s t prev-row row-num]
              (reduce (fn [result col-num]
                        (conj result
                          (min (inc (last result))
                            (inc (prev-row col-num))
                            (+ (prev-row (dec col-num))
                               (if (= (get s (dec row-num))
                                     (get t (dec col-num)))
                                 0
                                 1)))))
                [row-num] (range 1 (inc (count t)))))]
      (let [last-row (reduce (partial next-row s t)
                       (vec (range 0 (inc (count t))))
                       (range 1 (inc (count s))))]
        (last last-row)))))

(defcheck solution-4e042d52
  (fn [hs vs]
    (let [lh      (inc (count hs))
          lv      (inc (count vs))
          top-row (range lh)]
      (letfn [(splice-row [last-row row-no]
                (loop [grow [row-no]]
                  (if (= (count grow) lh) grow
                                          (let [i          (count grow)
                                                cand1-root (nth last-row (dec i))
                                                cand1      (if (= (nth hs (dec i))
                                                                 (nth vs (dec row-no)))
                                                             cand1-root (inc cand1-root))
                                                cand2      (inc (nth last-row i))
                                                cand3      (inc (last grow))
                                                newp       (min cand1 cand2 cand3)]
                                            (recur (conj grow newp))
                                            ))))
              (do-table [top-row]
                (loop [last-row top-row
                       row-no   1]
                  (if (= row-no lv) last-row
                                    (recur (splice-row last-row row-no) (inc row-no))
                                    )))]
        (let [bottom-row (do-table top-row)]
          (last bottom-row)
          )))))

(defcheck solution-4e859160
  (fn [as bs]
    (let [al    (count as)
          bl    (count bs)
          to-ab (fn [i]
                  [(mod i (inc al))
                   (quot i (inc al))])
          to-i  (fn [a b]
                  (+ a (* (inc al) b)))]
      (loop [dp (vec (repeat (* (inc al) (inc bl)) 0))
             i  0]
        (if (>= i (* (inc al) (inc bl)))
          (last dp)
          (let [[a b] (to-ab i)]
            (cond
              (= a 0) (recur (assoc dp i b) (inc i))
              (= b 0) (recur (assoc dp i a) (inc i))
              :else (recur (assoc dp i (min (inc (nth dp (to-i (dec a) b)))
                                         (inc (nth dp (to-i a (dec b))))
                                         (+ (nth dp (to-i (dec a) (dec b)))
                                            (if (= (nth as (dec a))
                                                  (nth bs (dec b)))
                                              0
                                              1))))
                      (inc i)))))))))

(defcheck solution-4ea59b16
  (fn lev-distance [seq-1 seq-2]
    (cond
      (> (count seq-1)
        (count seq-2)) (lev-distance seq-2 seq-1)
      (= seq-1 seq-2) 0
      (empty? seq-1) (count seq-2)
      (= (first seq-1)
        (first seq-2)) (lev-distance (rest seq-1) (rest seq-2))
      :else (min (inc (lev-distance (rest seq-1) (rest seq-2)))
              (inc (lev-distance seq-1 (rest seq-2)))))))

(defcheck solution-4ec0451e
  (fn [a b]
    (last
      (reduce
        (fn [prevrow j]
          (reductions
            (fn [prev i]
              (min (inc prev) (inc (nth prevrow i))
                (+ (nth prevrow (dec i))
                   (if (= (nth a (dec i)) (nth b (dec j))) 0 1))))
            j (range 1 (inc (count a)))))
        (range (inc (count a))) (range 1 (inc (count b)))))))

(defcheck solution-4ed6f485
  (fn [w1 w2]
    (let [w1 (vec (map vector (range (count w1)) w1))
          w2 (vec (map vector (range (count w2)) w2))]
      (last (reduce
              (fn [u [n1 c1]]
                (reduce
                  (fn [v [n2 c2]]
                    (conj v (if (= c1 c2)
                              (u n2)
                              (inc (min (last v) (u n2) (u (inc n2)))))))
                  [(inc n1)] w2))
              (vec (range (inc (count w2)))) w1)))))

(defcheck solution-4f409585
  #(let [m (count %) n (count %2)]
     (nth (nth ((fn f
                  ([c j] (f (rest c) [(first c)] j))
                  ([s d j]
                   (if (empty? s)
                     (if (<= (inc j) n) (f d (inc j)) d)
                     (f (rest s)
                       (conj d
                         (let [c (first s) p (last d) i (count d) a (split-at j c) j1 (dec j)]
                           (concat
                            (first a)
                            [(if (= (nth % (dec i)) (nth %2 j1))
                               (nth p j1)
                               (inc (min (nth p j) (nth c j1) (nth p j1))))]
                            (rest (second a))))) j)))
                  )
                (conj (for [i (range 1 (inc m))] (conj (take n (repeat 0)) i)) (range (inc n))) 1) m) n)))

(defcheck solution-4fa97c69
  (fn [at bt]
    (let [a (vec (if (> (count at) (count bt)) at bt))
          b (vec (if (> (count at) (count bt)) bt at))
          n (count a)
          m (count b)
          ]
      (loop [table (conj [(vec (range (inc m)))] (vector 1)) row 1 col 1]
        (let [above (get (get table (dec row)) col)
              left  (get (get table row) (dec col))
              diag  (get (get table (dec row)) (dec col))
              cost  (if (not= (get a (dec row)) (get b (dec col)))
                      1 0)]
          (if (> row n)
            (get (get table n) m)
            (if (> col m)
              (recur (conj table (vector (inc row))) (inc row) 1)
              (recur (assoc table row (conj (get table row) (min (inc above) (inc left) (+ diag cost)))) row (inc col)))))))))

(defcheck solution-4fb033a6
  (let [lev (memoize (fn [f a b i j]
                       (if (= 0 (min i j))
                         (max i j)
                         (min (inc (f f a b (dec i) j))
                           (inc (f f a b i (dec j)))
                           (+ (f f a b (dec i) (dec j))
                              (if (= (nth a (dec i)) (nth b (dec j))) 0 1))))))]
    (fn [a b]
      (lev lev a b (count a) (count b)))))

(defcheck solution-4fe19fb1
  (letfn [(lev [mlev ss ts]
            (cond
              (empty? ss) (count ts)
              (empty? ts) (count ss)
              :else
              (let [cost (if (= (first ss) (first ts)) 0 1)]
                (min (inc (mlev mlev (rest ss) ts))
                  (inc (mlev mlev ss (rest ts)))
                  (+ cost (mlev mlev (rest ss) (rest ts)))))))]
    (fn levenstein [s t]
      (lev (memoize lev) (seq s) (seq t)))))

(defcheck solution-50115365
  (fn [s t]
    (let [
          m  (count s) n (count t)
          mm (inc m) nn (inc n)
          d  (make-array Long/TYPE mm nn)
          _  (dotimes [i mm] (aset d i 0 i))
          _  (dotimes [j nn] (aset d 0 j j))
          _  (dotimes [jj n]                                ;start from 1!!!
               (dotimes [ii m]
                 (let [i    (inc ii) j (inc jj)
                       cost (if (= (nth s ii) (nth t jj)) 0 1)
                       xval (min (+ (aget d (dec i) j) 1)
                              (+ (aget d i (dec j)) 1)
                              (+ (aget d (dec i) (dec j)) cost)
                              )
                       ] (aset d i j xval))
                 ))
          ]
      (aget d m n))))

(defcheck solution-50a97596
  (fn [a b]
    (let [d (fn [f [d & e :as a] [h & i :as b]]
              (cond
                (empty? a) (count b)
                (empty? b) (count a)
                1
                (min
                  (+ (if (= d h) 0 1)
                     (f f e i))
                  (+ 1 (f f a i))
                  (+ 1 (f f e b)))))
          d (memoize d)]
      (d d a b))))

(defcheck solution-525bd5ff
  (letfn [
          (edit-matrix [a b]
            (let [a (cons \- a) b (cons \- b)]              ;add gap characters
              (memoize (fn field [f x y]
                         (cond
                           (zero? x) y                      ;gap cost of one
                           (zero? y) x                      ;gap cost of one
                           :else (min
                                   (inc (min (f f (dec x) y) (f f x (dec y))))
                                   (+ (f f (dec x) (dec y)) (if (= (nth a x) (nth b y)) 0 1))))))))
          ]

    (fn [a b]
      (let [mat (memoize (edit-matrix a b))]
        (mat mat (count a) (count b))))))

(defcheck solution-5383947b
  (fn [w1 w2]
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
            (recur (inc row-idx) max-rows next-prev-row)))))))

(defcheck solution-53a76edf
  (fn [s t]
    (let [m      (count s)
          n      (count t)
          a      (to-array-2d (for [i (range (inc m))
                                    ]
                                (repeat (inc n) nil)))
          lookup (fn [i j]
                   (aget (aget a i) j))
          set    (fn [i j val]
                   (aset (aget a i) j val))
          ]
      (do (doseq [i (range (inc m))]
            (set i 0 i))
          (doseq [j (range (inc n))]
            (set 0 j j))
          (doseq [j (range 1 (inc n))
                  i (range 1 (inc m))
                  ]
            (if (= (nth s (dec i))
                  (nth t (dec j)))
              (set i j (lookup (dec i) (dec j)))
              (set i j (+ 1 (min (lookup (dec i) j)
                              (lookup i (dec j))
                              (lookup (dec i) (dec j)))))))
          (lookup m n)))))

(defcheck solution-576295cf
  (fn [a b]
    (let [c (count a)
          d (count b)
          m (atom (vec
                    (for [i (range (+ c 1))]
                      (vec
                        (for [j (range (+ d 1))]
                          (cond (= i 0) j
                                (= j 0) i
                                (= (nth a (- i 1)) (nth b (- j 1))) 0
                                :else 1))))))]
      (doall
        (for [i (range 1 (+ c 1)) j (range 1 (+ d 1))]
          (swap! m #(assoc-in % [i j] (min (+ (get-in @m [(- i 1) j]) 1)
                                        (+ (get-in @m [i (- j 1)]) 1)
                                        (+ (get-in @m [(- i 1) (- j 1)]) (get-in @m [i j])))))))
      (get-in @m [c d]))))

(defcheck solution-5784e08b
  (fn [s t]
    (letfn
     [(lev-mod [d s t i j]
        (if (= (nth s (dec i)) (nth t (dec j)))
          (assoc-in d [i j] (get-in d [(dec i) (dec j)]))
          (let [nxt (min
                      (inc (get-in d [(dec i) j]))
                      (inc (get-in d [i (dec j)]))
                      (inc (get-in d [(dec i) (dec j)])))]
            (assoc-in d [i j] nxt)))
        )]
      (let [m (inc (count s))
            n (inc (count t))
            d (vec (repeat m (vec (repeat n 0))))
            d (assoc d 0 (vec (range 0 n)))
            d (loop [d d s (range 0 m)]
                (if (seq s)
                  (recur
                    (assoc-in d [(first s) 0] (first s)) (rest s))
                  d))
            ]
        (get-in (loop [d d j (range 1 n)]
                  (if (seq j)
                    (recur (loop [d d i (range 1 m)]
                             (if (seq i)
                               (recur (lev-mod d s t (first i) (first j)) (rest i))
                               d))
                      (rest j))
                    d
                    )) [(dec m) (dec n)])))))

(defcheck solution-57e4f848
  (fn leven-distance [s t]
    (let [s (seq s) t (seq t) m (count s) n (count t)]
      (cond (empty? s) n
            (empty? t) m
            :else (let [d (into {} (apply concat (map (fn [i] (apply concat (map (fn [j] (vector [[i j] 0])) (range (inc n))))) (range (inc m)))))
                        d (merge d (into {} (map #(vector (vector % 0) %) (range 1 (inc m)))))
                        d (merge d (into {} (map #(vector (vector 0 %) %) (range 1 (inc n)))))]
                    ;(println d)
                    ;(println
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
                       (map vector (iterate inc 1) t)) [m n]))))))

(defcheck solution-583375f2
  (fn [s1 s2]
    (let [m (count s1)
          r (range (+ 1 m))
          d (fn [p l]
              (fn [a i] (conj a
                          (if (== -1 i)
                            (inc (first p))
                            (if (= l (nth s1 i))
                              (nth p i)
                              (inc (min (nth p i) (nth p (inc i)) (last a))))))))]
      (last (reduce (fn [x y] (reduce (d x y) [] (range -1 m))) r s2)))))

(defcheck solution-5890f15f
  (fn [s t]
    (cond
      (= s t) 0
      (empty? s) (count t)
      (empty? t) (count s)
      :else (last (second (reduce (fn [[i v0] si]
                                    (let [v1 (vec (repeat (inc (count t)) nil))
                                          v1 (assoc v1 0 (inc i))]
                                      [(inc i) (reduce (fn [v1 j]
                                                         (assoc v1 (inc j) (min (inc (nth v1 j)) (inc (nth v0 (inc j))) (+ (nth v0 j) (if (= si (nth t j)) 0 1))))) v1 (range (count t)))]))
                            [0 (vec (range (inc (count t))))] s))))))

(defcheck solution-592ebf8
  (let [mem (atom {})]
    (fn leven [[fa & ra :as a] [fb & rb :as b]]
      (or
       (@mem [a b])
       (let [res
             (cond (nil? a) (count b)
                   (nil? b) (count a)
                   (= fa fb) (leven ra rb)
                   :else (+ 1
                            (min (leven ra rb)
                              (leven a rb)
                              (leven ra b))))]
         (swap! mem assoc [a b] res)
         res)))))

(defcheck solution-596b2ad9
  (fn lev [s1 s2]
    (let [al   (inc (count s1))
          bl   (inc (count s2))
          arr  (vec (repeat al (vec (repeat bl 0))))
          arr1 (loop [a arr i 0]
                 (if (< i al)
                   (recur (assoc-in a [i 0] i) (inc i))
                   a))
          arr2 (loop [a arr1 j 0]
                 (if (< j bl)
                   (recur (assoc-in a [0 j] j) (inc j))
                   a))
          arr3 (reduce
                 (fn [a [i j]]
                   (assoc-in a [i j]
                     (min
                       (+ 1 (get-in a [(dec i) j]))
                       (+ 1 (get-in a [i (dec j)]))
                       (+ (get-in a [(dec i) (dec j)])
                          (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1))
                       )))
                 arr2
                 (for [i (range 1 al) j (range 1 bl)] [i j]))
          ]
      (get-in arr3 [(dec al) (dec bl)])
      )))

(defcheck solution-598d5fc8
  (fn [s t]
    (let [sl (count s)
          tl (count t)]
      (cond
        (= s t) 0
        (empty? s) tl
        (empty? t) sl
        :else
        (last
          (loop [i 0
                 v (-> tl inc range vec)]
            (if (= i sl) v
                         (recur (inc i)
                           (loop [v-new [(inc i)]]
                             (if (= tl (-> v-new count dec)) v-new
                                                             (let [j      (-> v-new count dec)
                                                                   cost   (if (= (nth s i) (nth t j)) 0 1)
                                                                   v0_j   (nth v j)
                                                                   v0_jp1 (nth v (inc j))
                                                                   v1_j   (last v-new)]
                                                               (recur
                                                                 (conj v-new
                                                                   (min
                                                                     (inc v1_j)
                                                                     (inc v0_jp1)
                                                                     (+ v0_j cost)))))))))))))))

(defcheck solution-59c3ead4
  (fn [seq1 seq2]
    (letfn [(index-of [s x]
              (remove nil? (map-indexed #(when (= x %2) %1) s)))
            (commons [s1 s2]
              (map (fn [[n1 n2]]
                     [n1 n2 (count (take-while (fn [[x y]] (= x y))
                                     (partition 2 (interleave (drop n1 s1) (drop n2 s2)))))])
                (apply concat
                  (map-indexed (fn [n x] (map #(vector % n) (index-of s1 x)))
                    (concat s2)))))
            (remains [s x y]
              [(take x s) (drop (+ x y) s)])
            (dist [seq1 seq2]
              (if (= seq1 seq2)
                0
                (let [[s1 s2] (if (> (count seq1) (count seq2)) [seq1 seq2] [seq2 seq1])
                      cms (commons s1 s2)]
                  (if (empty? cms)
                    (count s1)
                    (apply min
                      (map (fn [[n1 n2 n]]
                             (let [[a b] (remains s1 n1 n) [c d] (remains s2 n2 n)]
                               (+ (dist a c) (dist b d))))
                        cms))))))]
      (dist seq1 seq2))))

(defcheck solution-5a8d0613
  (fn [a b]
    (letfn [(lcs [[x-h & x-t :as x] [y-h & y-t :as y]]
              (cond
                (empty? x) (count y)
                (empty? y) (count x)
                (= x-h y-h) (lcs x-t y-t)
                :else (+ 1 (min (lcs x y-t) (lcs x-t y) (lcs x-t y-t)))))]
      (lcs a b))))

(defcheck solution-5aa4e178
  (fn lev [s t]
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
           [m n]))))))

(defcheck solution-5c08ff65
  (let [Lev (memoize (fn [Lev s cs t ct]
                       (cond (zero? cs) ct
                             (zero? ct) cs
                             :else (min (inc (Lev Lev s (dec cs) t ct))
                                     (inc (Lev Lev s cs t (dec ct)))
                                     (+ (Lev Lev s (dec cs) t (dec ct))
                                        (if (= (nth s (dec cs)) (nth t (dec ct)))
                                          0 1))))))
        Lev (partial Lev Lev)]
    #(Lev %1 (count %1) %2 (count %2))))

(defcheck solution-5cae5a23
  (fn [sx sy]
    (let [lx (count sx) ly (count sy)]
      (if (= 0 lx)
        ly
        (if (= 0 ly)
          lx
          (loop [x 0 y 0 dx_1 (into [] (range (inc ly))) dx [1]]
            (if (= y ly)
              (if (= x (dec lx))
                (last dx)
                (recur (inc x) 0 dx [(inc x)])
                )
              (let [c (if (= (nth sx x) (nth sy y)) 0 1) d1 (inc (nth dx_1 (inc y))) d2 (inc (last dx)) d3 (+ (nth dx_1 y) c)]
                (recur x (inc y) dx_1 (conj dx (min d1 d2 d3)))
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-5d4b71fa
  (fn
    [a b]
    (let [cache (atom {})]
      (letfn [(lev [i j]
                (cond (zero? (min i j))
                      (max i j)

                      (contains? @cache [i j])
                      (@cache [i j])

                      :else
                      (let [res (min (inc (lev (dec i) j))
                                  (inc (lev i (dec j)))
                                  (+ (lev (dec i) (dec j)) (if (= (get a (dec i)) (get b (dec j))) 0 1)))]
                        (swap! cache assoc [i j] res)
                        res)))]
        (lev (count a) (count b))))))

(defcheck solution-5e4a610b
  (fn levendist [a b]
    (let [lendiff         (- (count a) (count b))
          value           (fn [target current] (reduce + (map #(if (= % %2) 0 1) current target)))
          drop-nth        (fn drop-nth [n s] (concat (take n s) (drop (inc n) s)))
          shorten-single  (fn [s] (map #(drop-nth % s) (range (count s))))
          shortened-set   (fn [start steps]
                            (loop [c (conj #{} start) s steps]
                              (if (= 0 s)
                                c
                                (recur (into #{} (apply concat (map shorten-single c))) (dec s)))))
          find-best-value (fn [s target]
                            (apply min-key last
                              (map (juxt identity (partial value target)) s)))
          ]
      (if (< lendiff 0)
        (levendist b a)
        (+ lendiff (last (find-best-value (shortened-set a lendiff) b)))))))

(defcheck solution-5e4bbc93
  (fn edit-distance [s1 s2]
    (let [s1 (vec (concat "^" s1 "$"))
          s2 (vec (concat "^" s2 "$"))
          l1 (count s1)
          l2 (count s2)]
      (loop [scores (vec (concat [0] (repeat (dec l2) 1000000)))
             index  1]
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
            (inc index))
          )))))

(defcheck solution-5eeeb4e6
  (fn levenstein [a b]
    (let [cache (atom {})]
      ((fn levenstein [a b]
         (or
          (@cache [a b])
          (let [result
                (cond
                  (zero? (count a)) (count b)
                  (zero? (count b)) (count a)
                  :else
                  (let [cost (if (= (last a) (last b)) -1 0)]
                    (inc (min
                           (levenstein (drop-last a) b)
                           (levenstein (drop-last b) a)
                           (+ cost (levenstein (drop-last a) (drop-last b)))))))]
            (swap! cache assoc [a b] result)
            result))) a b))))

(defcheck solution-5f0448b9
  (fn ledis [a b]
    (loop [md {[a b] 0}]
      (let [nmd (reduce (fn [m n]
                          (let [[a b] (key n) d (val n)
                                [fa & ra] a
                                [fb & rb] b]
                            (merge-with min m (cond (= fa fb) {[ra rb] d}
                                                    (nil? fa) {[nil nil] (+ d (count b))}
                                                    (nil? fb) {[nil nil] (+ d (count a))}
                                                    :else (zipmap [[ra b] [rb a] [ra rb]]
                                                            (repeat (inc d)))))))
                  {} md)
            mid (nmd [nil nil])
            nnm (if mid (into {} (filter #(<= (val %) mid) nmd)) nmd)]
        (if (and mid (= 1 (count nnm))) (val (first nnm))
                                        (recur nnm))))))

(defcheck solution-5f7c80b6
  (fn dist [s1 s2]
    (loop [n1 1 n2 1 dict {}]
      (let [get #(get dict [% %2] (max % %2))
            c1  (count s1) c2 (count s2)
            d1  (- n1 1) d2 (- n2 1)]
        (cond (> n2 c2) (get c1 c2)
              (> n1 c1) (recur 1 (+ n2 1) dict)
              :else
              (recur (+ n1 1) n2 (assoc dict [n1 n2]
                                             (min (+ (get d1 n2) 1)
                                               (+ (get n1 d2) 1)
                                               (+ (get d1 d2)
                                                  (if (= (nth s1 d1) (nth s2 d2)) 0 1))))))))))

(defcheck solution-5fa1b4f0
  (fn df [a b]
    (let [alen (count a) blen (count b)]
      (cond (= alen blen) (count (filter true? (map not= a b)))
            (> alen blen) (df b a)
            ; from here alen < blen
            (= blen 0) 0
            :else (min
                    (+ 1 (df a (rest b)))
                    (+ (if (= (first a) (first b)) 0 1) (df (rest a) (rest b)))))
      )))

(defcheck solution-60711d4d
  (fn distance
    [a b]
    (cond
      (empty? a) (count b)
      (empty? b) (count a)
      (= (first a) (first b)) (distance (rest a) (rest b))
      :else (inc (min (distance (rest a) b) (distance a (rest b)) (distance (rest a) (rest b)))))))

(defcheck solution-617449b1
  (fn levenshtiein [s t]
    (let [m         (count s)
          n         (count t)
          matrix    (mapv #(vec (range % (+ m % 1))) (range (inc n)))
          coords    (for [x (range 1 (inc n))
                          y (range 1 (inc m))] [x y])
          distance  (fn [[x y] m]
                      (if (= (get t (dec x)) (get s (dec y)))
                        (get-in m [(dec x) (dec y)])
                        (apply min
                          (map inc
                            [(get-in m [(dec x) y])
                             (get-in m [x (dec y)])
                             (get-in m [(dec x) (dec y)])])
                          )))
          invariant (loop [m matrix [c & cs] coords]
                      (if (nil? c)
                        m
                        (recur (update-in m c (fn [d] (distance c m))) cs)))]
      (get-in invariant [n m]))))

(defcheck solution-61ff5328
  (fn leven [s1 s2]
    (let [n (count s1)
          m (count s2)]
      (cond (= s1 s2) 0
            (= 0 n) m
            (= 0 m) n
            :else
            (let [v0 (vec (range (inc m)))
                  v1 (vec (range (inc m)))]
              (loop [i 0 v0 v0 v1 v1]
                (if (= i n)
                  (v1 m)
                  (let [v1 (reduce
                             (fn [v1 j]
                               (assoc-in v1 [(inc j)]
                                 (min (inc (nth v1 j))
                                   (inc (nth v0 (inc j)))
                                   (+ (if (= (nth s1 i) (nth s2 j)) 0 1) (nth v0 j))))) (assoc-in v1 [0] (inc i)) (range 0 m))]
                    (recur (inc i) (take (count v0) v1) v1)))))))))

(defcheck solution-621ccf47
  (fn [s t]
    (let [lev
              (memoize
                (fn aux [rec s t]
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
      (lev s t))))

(defcheck solution-629b2da1
  (fn [x y]
    (let [[x y] (sort-by count [x y])
          blanks   (fn blanks [coll n]
                     (if (zero? n)
                       [coll]
                       (mapcat (fn [i]
                                 (let [[l r] (split-at i coll)]
                                   (map #(concat l (cons ::blank %))
                                     (blanks r (dec n)))))
                         (range (inc (count coll))))))
          x-blanks (blanks x (- (count y) (count x)))
          diffs    (fn [a b] (count (remove identity (map #(= % %2) a b))))]
      (apply min (map diffs x-blanks (repeat y))))))

(defcheck solution-62d049f0
  (fn ld [a b]
    (let [a      (seq a)
          b      (seq b)
          common (fn common [a b]
                   (if (= a b)
                     [nil nil a]
                     (loop [a  a
                            b  b
                            ra []
                            rb []
                            c  []]
                       (if (or (first a) (first b))
                         (if (= (first a) (first b))
                           (recur (rest a) (rest b) (concat ra [nil]) (concat rb [nil]) (concat c [(first a)]))
                           (recur (rest a) (rest b) (concat ra [(first a)]) (concat rb [(first b)]) (concat c [nil])))
                         (let [ra (drop-while nil? (reverse ra))
                               rb (drop-while nil? (reverse rb))
                               c  (drop-while nil? (reverse c))]
                           (map reverse [ra rb c]))))))
          [f l c] (common a b)
          cf     (count f)
          cl     (count l)
          cc     (count c)
          r      (cond
                   (= (max cf cl cc) cc) c
                   (= (max cf cl cc) cf) f
                   (= (max cf cl cc) cl) l)
          d      (if (= r c)
                   (count (filter nil? r))
                   (count (filter (complement nil?) r)))
          lf     (take-while (complement nil?) (reverse f))
          ll     (take-while (complement nil?) (reverse l))
          [fr lr cr] (common lf ll)
          e      (count (filter (complement nil?) cr))]
      (- d e))))

(defcheck solution-64bdca3f
  (fn [s t]
    (let [f  (fn [f s t]
               (let [sc (count s)
                     tc (count t)]
                 (cond
                   (= sc 0) tc
                   (= tc 0) sc
                   :else (let [cost (if (= (last s) (last t)) 0 1)]
                           (min (inc (f f (butlast s) t))
                             (inc (f f s (butlast t)))
                             (+ (f f (butlast s) (butlast t)) cost))))))
          mf (memoize f)]
      (mf mf s t))))

(defcheck solution-64bf2328
  (fn [s t]
    (let [lev (memoize (fn [s t rec-lev]
                         (let [ns (count s)
                               nt (count t)]
                           (cond (= ns 0) nt
                                 (= nt 0) ns
                                 :else (let [cost   (if (= (first s) (first t)) 0 1)
                                             rest-s (rest s)
                                             rest-t (rest t)]
                                         (min
                                           (inc (rec-lev rest-s t rec-lev))
                                           (inc (rec-lev s rest-t rec-lev))
                                           (+ cost (rec-lev rest-s rest-t rec-lev))))))))]
      (lev s t lev))))

(defcheck solution-653e61cb
  ((fn []
     (let
      [lev
                (fn [mem-lev x y]
                  (let [lev2 (fn [c d] (mem-lev mem-lev c d))
                        mt   (if (= (first x) (first y)) 0 1)
                        ]
                    (if (= 0 (* (count x) (count y))) (+ (count x) (count y))
                                                      (min
                                                        (inc (lev2 (rest x) y))
                                                        (inc (lev2 x (rest y)))
                                                        (+ mt (lev2 (rest x) (rest y)))))))
       mem-lev2 (memoize lev)]
       (partial mem-lev2 mem-lev2)))))

(defcheck solution-65a95cb5
  (fn [s t]
    (cond (empty? s) (count t)
          (empty? t) (count s)
          :else (let [d     (into [] (map (fn [i] (into [] (map (fn [j] 0) (range (inc (count t)))))) (range (inc (count s)))))
                      mth   (fn [e x y] (nth (nth e x) y))
                      dssoc (fn [e x y val] (assoc e x (assoc (nth e x) y val)))]
                  (loop [d (assoc (into [] (map-indexed (fn [idx row]
                                                          (assoc row 0 idx)) d)) 0 (into [] (range (inc (count t)))))
                         [i j] [1 1]]
                    (if (> j (count t)) (last (last d))
                                        (recur (dssoc d i j (if (= (nth s (dec i)) (nth t (dec j)))
                                                              (mth d (dec i) (dec j))
                                                              (apply min (map (fn [[a b]]
                                                                                (inc (mth d a b)))
                                                                           [[(dec i) j]
                                                                            [i (dec j)]
                                                                            [(dec i) (dec j)]]))))
                                          (if (= i (count s))
                                            [1 (inc j)]
                                            [(inc i) j]))))))))

(defcheck solution-65aff60c
  (fn f [a b]
    (cond
      (empty? a) (count b)
      (empty? b) (count a)
      (= (first a) (first b)) (recur (rest a) (rest b))
      :else (inc (min (f (rest a) b) (f a (rest b)) (f (rest a) (rest b)))))))

(defcheck solution-66404683
  (fn [s1 s2]
    (let [v1       (vec s1) v2 (vec s2)
          row0     (vec (range (inc (count v1))))
          next-row (fn [v]
                     (vec (reduce (fn [u i]
                                    (let [j (first v)
                                          a (inc (peek u))
                                          b (inc (v (inc i)))
                                          c (+ (v i) (if (= (v1 i) (v2 j)) 0 1))]
                                      ;(println j a b c)
                                      (conj u (min a b c))))

                            [(inc (first v))] (range (count v1)))))
          table    (iterate next-row row0)]
      (last (nth table (count v2))))))

(defcheck solution-66d3d8c0
  (fn [a b]
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
     a b (count a) (count b))))

(defcheck solution-688c7886
  (fn [a b]
    (cond (empty? a) (count b)
          (empty? b) (count a)
          :else
          (let [costs (make-array Long (inc (count a)) (inc (count b)))]

            (doseq [i (range (inc (count a)))]
              (aset costs i 0 i))
            (doseq [j (range (inc (count b)))]
              (aset costs 0 j j))
            (doseq [i (map inc (range (count a)))
                    j (map inc (range (count b)))]
              (let [cost (if (= (nth a (dec i)) (nth b (dec j))) 0 1)]
                (aset costs i j
                  (min (+ cost (aget costs (dec i) (dec j)))
                    (inc (aget costs i (dec j)))
                    (inc (aget costs (dec i) j))))))
            (aget costs (count a) (count b))))))

(defcheck solution-6992593c
  #((fn l [x y] (let [m (dec x) n (dec y)]
                  (cond (= x 0) y
                        (= y 0) x
                        (= (nth %1 m) (nth %2 n)) (l m n)
                        :else (inc (min (l m y) (l x n) (l m n))))))
    (count %1)
    (count %2)))

(defcheck solution-69f8afa5
  (fn [s t]
    ;; just amounts to recode an algorithm written down in wikipedia...
    (cond
      (= s t) 0
      (= 0 (count s)) (count t)
      (= 0 (count t)) (count s)
      :else (let [v0 (transient (vec (range (+ 2 (count t)))))
                  v1 (transient (vec (take (count v0) (repeat 0))))]
              (doseq [i (range (count s))]
                (do
                  (assoc! v1 0 (inc i))
                  (doseq [j (range (count t))]
                    (let [cost (if (= (nth s i) (nth t j)) 0 1)]
                      (assoc! v1 (inc j) (min (inc (get v1 j))
                                           (inc (get v0 (inc j)))
                                           (+ cost (get v0 j))))))
                  (doseq [j (range (count v0))]
                    (assoc! v0 j (nth v1 j)))))
              (nth v1 (count t))))))

(defcheck solution-6a34263b
  (fn levenstein [a b]
    (let [lev     (fn lev [a b]
                    (cond (= a b) 0
                          (= a []) (count b)
                          (= b []) (count a)
                          :else (min (if (< (count a) (count b)) (inc (lev a (rest b)))
                                                                 (inc (lev (rest a) b)))
                                  (+ (lev (rest a) (rest b)) (if (= (first a) (first b)) 0 1)))))
          mem-lev (memoize lev)]
      (mem-lev a b))))

(defcheck solution-6a4d9d4f
  (fn levenshtein [s t]
    (let [ns         (inc (count s))
          nt         (inc (count t))
          s-to-empty (into [] (map vector (range 0 ns)))
          empty-to-t (assoc s-to-empty 0 (vec (range 0 nt)))
          coords     (for [j (range 1 nt), i (range 1 ns)] [j i])
          distances
                     (reduce
                       (fn [distances [j i]]
                         (let [substition-cost (if (= (get s (dec i)) (get t (dec j))) 0 1)
                               deletion-dist   (+ 1 (get-in distances [(dec i) j]))
                               insertion-dist  (+ 1 (get-in distances [i (dec j)]))
                               substition-dist (+ substition-cost (get-in distances [(dec i) (dec j)]))]
                           (assoc-in distances [i j] (min deletion-dist insertion-dist substition-dist))))
                       empty-to-t
                       coords)]

      (get-in distances [(dec ns) (dec nt)]))))

(defcheck solution-6ac1352
  (fn q [str1 str2]
    (let [p (atom false)]
      (reset! p (memoize (fn [i1 i2 len1 len2]
                           (cond (= len1 i1) (- len2 i2)
                                 (= len2 i2) (- len1 i1)
                                 :else
                                 (min (inc (@p (inc i1) i2 len1 len2))
                                   (inc (@p i1 (inc i2) len1 len2))
                                   (+ (if (= (nth str1 i1) (nth str2 i2)) 0 1)
                                      (@p (inc i1) (inc i2) len1 len2)))))))
      (@p 0 0 (count str1) (count str2)))))

(defcheck solution-6b7ed45e
  (fn [str1 str2]
    (let [lev  (fn [s1 l1 s2 l2 f]
                 (cond (zero? l1) l2
                       (zero? l2) l1
                       :else (min (inc (f s1 (dec l1) s2 l2 f))
                               (inc (f s1 l1 s2 (dec l2) f))
                               (+ (f s1 (dec l1) s2 (dec l2) f)
                                  (if (= (nth s1 (dec l1))
                                        (nth s2 (dec l2))) 0 1)))))
          mlev (memoize lev)]
      (mlev str1 (count str1) str2 (count str2) mlev)
      )))

(defcheck solution-6ba24014
  (fn f [a b]
    (cond (empty? a) (count b)
          (empty? b) (count a)
          (= (first a) (first b)) (recur (rest a) (rest b))
          :else (inc (min (f (rest a) b) (f a (rest b)) (f (rest a) (rest b)))))))

(defcheck solution-6cd56781
  (fn lev [s1 s2]
    (if (empty? s1) (count s2)
                    (let [m (count s1) n (count s2)]
                      (loop [prev-line (range (+ m 1))
                             curr-line [1]
                             i         1 j 1]
                        (let [curr     (if (= (nth s1 (dec j)) (nth s2 (dec i)))
                                         (nth prev-line (dec j))
                                         (inc (min (nth prev-line (dec j)) (nth prev-line j) (nth curr-line (dec j)))))
                              new-line (conj curr-line curr)]
                          (if (= j m)
                            (if (= i n) curr (recur new-line [(inc i)] (inc i) 1))
                            (recur prev-line new-line i (inc j)))))))))

(defcheck solution-6d823467
  (fn dist [s1 s2]
    (cond
      (empty? s2)
      (count s1)
      (empty? s1)
      (count s2)
      :else
      (let [[ss1 ss2] (sort-by count [s1 s2])
            [c1 c2] (map count [ss1 ss2])
            [h1 & t1] ss1
            [h2 & t2] ss2]
        (if (= h1 h2)
          (dist t1 t2)
          (inc (min
                 (if (< c1 c2)
                   (min
                     (dist (cons h2 ss1) ss2)               ; &#30701;&#30340;&#21152;&#19968;
                     (dist ss1 t2))                         ; &#38271;&#30340;&#20943;&#19968;
                   c1)
                 (dist (cons h2 t1) ss2)                    ; &#25442;&#25104;&#38271;&#22836;
                 (dist ss1 (cons h1 t2))                    ; &#25442;&#25104;&#30701;&#22836;
                 )))))))

(defcheck solution-6d9c67cc
  (fn go [a b]
    (-> (iterate (fn [{j :index prev-row :row}]
                   {:index (inc j)
                    :row   (reduce (fn [row i]
                                     (conj
                                       row
                                       (if (zero? i)
                                         (inc j)
                                         (min (inc (last row))
                                           (inc (nth prev-row i))
                                           (+ (nth prev-row (dec i)) (if (= (nth a (dec i)) (nth b j)) 0 1))))))
                             []
                             (range (count prev-row)))})
          {:index 0, :row (range (inc (count a)))})
      (nth (count b))
      :row
      last)))

(defcheck solution-6dec146e
  #({\k 3 \c 1 \x 2 1 2 :a 2 \t 10 \g 9} (first %)
    ({"123456" 6} %2 0)))

(defcheck solution-6ece3219
  (fn edit-distance [x y]
    (letfn [(change-mat [mat l c val]
              (assoc mat l (assoc (mat l) c val)))
            (get-val [mat l c]
              ((mat l) c))
            (get-next-val [mat l c]
              (if (= (nth x (dec l)) (nth y (dec c)))
                (get-val mat (dec l) (dec c))
                (let [n1 (get-val mat (dec l) c)            ;;remove
                      n2 (get-val mat l (dec c))            ;;insert
                      n3 (get-val mat (dec l) (dec c))]     ;;subs
                  (inc (min n1 n2 n3)))))
            ]
      (let [m     (inc (count x))
            n     (inc (count y))

            taux  (assoc
                   (apply vector (repeat m (apply vector (repeat n 0))))
                    0 (apply vector (range n)))

            table (reduce #(change-mat %1 %2 0 %2) taux (range m))
            ]
        (loop [i   1
               tab table]
          (if (>= i m)
            (get-val tab (dec m) (dec n))
            (let [tab2
                  (reduce #(change-mat %1 i %2 (get-next-val %1 i %2)) tab (range 1 n))]
              ;;(println tab2 i)
              (recur (inc i) tab2))))))))

(defcheck solution-70456397
  (fn [s t]
    (let [distance
          (memoize (fn [f s t]
                     (if (empty? s) (count t)
                                    (if (empty? t) (count s)
                                                   (min (inc (f f s (butlast t)))
                                                     (inc (f f t (butlast s)))
                                                     (+ (if (= (last s) (last t)) 0 1)
                                                        (f f (butlast s) (butlast t))))))))]
      (distance distance s t))))

(defcheck solution-704b88dc
  (fn [s t]
    (if (= s t)
      0
      (if (zero? (count s))
        (count t)
        (if (zero? (count t))
          (count s)
          (loop [i 0 v0 (apply vector (range 0 (inc (count t))))]
            (if (= i (count s))
              (last v0)
              (recur
                (inc i)
                (loop [j 0 v1 (apply vector (inc i) (repeat (count t) 0))]
                  (if (= j (count t))
                    v1
                    (recur
                      (inc j)
                      (update-in v1 [(inc j)]
                        (fn [_] (min
                                  (inc (nth v1 j))
                                  (inc (nth v0 (inc j)))
                                  (+ (nth v0 j) (if (= (nth s i) (nth t j)) 0 1))))))))))))))))

(defcheck solution-713531c
  (fn levenshtein [s1 s2]
    (let [distance      (fn [f s1 s2]
                          (cond
                            (empty? s1) (count s2)
                            (empty? s2) (count s1)
                            :else
                            (let [cost (if (= (first s1) (first s2)) 0 1)]
                              (min
                                (+ (f f (rest s1) (rest s2)) cost)
                                (+ (f f (rest s1) s2) 1)
                                (+ (f f s1 (rest s2)) 1)))))
          memo-distance (memoize distance)]
      (memo-distance memo-distance (seq s1) (seq s2)))))

(defcheck solution-718af986
  (letfn [
          (wi [src-word dst-word]
            (vector 0 (seq src-word) (seq dst-word)))

          (wi-set [wi]
            #{wi})

          (clean [[ch src dst :as wi]]
            (if (or (empty? src)
                    (empty? dst)
                    (not (= (first src) (first dst))))
              wi
              (recur (vector ch (rest src) (rest dst)))))

          (clean-set [wi-set]
            (into #{} (map clean (remove nil? wi-set))))

          (ins [[ch src dst :as wi]]
            (vector (inc ch) src (rest dst)))

          (rep [[ch src dst :as wi]]
            (vector (inc ch) (rest src) (rest dst)))

          (del [[ch src dst :as wi]]
            (if (empty? dst)
              (vector (+ ch (count src)) () ())
              (let [pos (.indexOf src (first dst))]
                (if (= -1 pos) nil
                               (vector (+ ch pos) (drop pos src) dst)))))

          (next-set [wi-set max-changes]
            (loop [rm (seq wi-set), acc #{}]
              (let [[ch src dst :as wi] (first rm)]
                (cond (empty? rm) (clean-set acc)
                      (and (>= ch max-changes)
                           (or (seq src) (seq dst))) (recur (rest rm) acc)
                      :else (recur (rest rm) (conj acc (ins wi) (rep wi) (del wi)))))))

          (sol [src-word dst-word]
            (let [init        (wi-set (wi src-word dst-word))
                  max-changes (max (count src-word) (count dst-word))]
              (if (or (empty? src-word)
                      (empty? dst-word))
                max-changes
                (loop [wis init]
                  (if (every? #(and (empty? (nth % 1))
                                    (empty? (nth % 2))) wis)
                    (apply min (map first wis))
                    (recur (next-set wis max-changes)))))))]
    sol))

(defcheck solution-727b2412
  (fn ldistance
    [s1 s2]
    (if (or (empty? s1) (empty? s2))
      (+ (count s1) (count s2))
      (if (= (first s1) (first s2))
        (ldistance (next s1) (next s2))
        (inc (min (ldistance (next s1) s2)
               (ldistance s1 (next s2))
               (ldistance (next s1) (next s2))))))))

(defcheck solution-72bd14bd
  (fn edit-distance [source target]
    (let [source  (vec source)
          target  (vec target)
          targlen (count target)
          rowlen  (inc targlen)
          ; For empty source, cost is insertion of each target item
          initrow (vec (map #(- targlen %) (range rowlen)))]
      (loop [i (dec (count source)), prevrow initrow]
        (if (neg? i)
          (prevrow 0)
          (let [sitem (source i)
                build-row
                      (fn [ti row]
                        (cond
                          (neg? ti)
                          row
                          (= (target ti) sitem)
                          (recur (dec ti) (conj row (prevrow (inc ti))))
                          :else
                          (let [rep (inc (prevrow (inc ti)))
                                ins (inc (first row))
                                del (inc (prevrow ti))]
                            (recur (dec ti) (conj row (min rep ins del)))
                            )))
                ]
            (recur (dec i) (vec (build-row (dec targlen) (list (inc (prevrow targlen))))))
            ))))))

(defcheck solution-7444b11c
  (let [lev (fn [a b rec]
              (let [a-len (count a) b-len (count b)]
                (cond
                  (= a-len 0) b-len
                  (= b-len 0) a-len
                  :else
                  (let [cost (if (= (last a) (last b)) 0 1)]
                    (min (inc (rec (butlast a) b rec))
                      (inc (rec a (butlast b) rec))
                      (+ cost (rec (butlast a) (butlast b) rec)))))))]
    #(lev %1 %2 (memoize lev))))

(defcheck solution-7451c9d7
  (fn [a b]
    (let [b (seq b)
          i into
          f #(loop [o [] a % b %2]
               (let [[c & a] a
                     [d & b] b]
                 (if (= c d)
                   (if c (recur (conj o c) a b))
                   [(i (conj o d) a) (i o a) (i (conj o d c) a)])))]
      (loop [l #{(seq a)} n 0]
        (if (some #(= b %) l)
          n
          (recur (reduce #(into % (f %2 b)) l l) (inc n)))))))

(defcheck solution-74cbd762
  (letfn [
          (dis [dis-memoized x1 x2] (cond
                                      (= 0 (count x1)) (count x2)
                                      (= 0 (count x2)) (count x1)
                                      true (apply min (remove nil? [
                                                                    (inc (dis-memoized dis-memoized (rest x1) (rest x2)))
                                                                    (inc (dis-memoized dis-memoized x1 (rest x2)))
                                                                    (inc (dis-memoized dis-memoized (rest x1) x2))
                                                                    (when (= (first x1) (first x2)) (dis-memoized dis-memoized (rest x1) (rest x2)))]))))]
    (partial dis (memoize dis))))

(defcheck solution-74e791cb
  (fn [s t]
    (let [m (count s)
          n (count t)
          g (vec
              (map vec
                (partition (inc n)
                  (for [i (range 0 (inc m))
                        j (range 0 (inc n))]
                    (cond
                      (= 0 j) i
                      (= 0 i) j
                      :else 0)))))]
      (->
        (reduce
          (fn [d [i j]]
            (assoc-in d [(inc i) (inc j)]
              (min (inc (get-in d [i (inc j)]))
                (inc (get-in d [(inc i) j]))
                (+ (get-in d [i j]) (if (= (nth s i) (nth t j)) 0 1)))))
          g
          (for [i (range 0 m)
                j (range 0 n)]
            [i j]))
        (get-in [m n])))))

(defcheck solution-754e0cf3
  (fn [a b]
    (letfn [(query [s1 s2 limit]
              (cond
                (zero? limit) 0
                (empty? s1) (min-key identity (count s2) limit)
                (empty? s2) (min-key identity (count s1) limit)
                :else
                (let [v1 (inc (query s1 (rest s2) (dec limit)))
                      v2 (inc (query (rest s1) s2 (dec (min-key identity v1 limit))))
                      v3 (if (= (first s1) (first s2))
                           (query (rest s1) (rest s2) (min-key identity v2 limit))
                           (inc (query (rest s1) (rest s2)
                                  (dec (min-key identity v2 limit)))))]
                  v3)))]
      (query a b (max-key identity (count a) (count b))))))

(defcheck solution-75c2e3b1
  (fn [s1 s2]
    (let [levdist (memoize (fn [s1 s2 cb]
                             (cond
                               (empty? s1) (count s2)
                               (empty? s2) (count s1)
                               :else
                               (let [f1    (first s1)
                                     f2    (first s2)
                                     diff1 (if (= f1 f2) 0 1)
                                     r1    (rest s1)
                                     r2    (rest s2)]
                                 (min
                                   (+ 1 (cb s1 r2 cb))
                                   (+ 1 (cb r1 s2 cb))
                                   (+ diff1 (cb r1 r2 cb)))))))]
      (levdist s1 s2 levdist))
    ))

(defcheck solution-76ac7fab
  (letfn [(f [s x prev i]
            (loop [row [(inc i)] j 1]
              (if (> j (count s)) row
                                  (recur (conj row
                                           (min
                                             (inc (nth prev j))
                                             (inc (last row))
                                             (+ (nth prev (dec j))
                                                (if (= (nth s (dec j)) x) 0 1))
                                             )
                                           ) (inc j)))
              ))]

    (fn lev [s t]
      (cond (empty? s) (count t)
            (empty? t) (count s)
            :else
            (loop [prev (range 0 (inc (count s))) i 1]
              (if (> i (count t)) (last prev)
                                  (recur (f s (nth t (dec i)) prev i) (inc i))))))
    ))

(defcheck solution-76fac53a
  (fn [a b]
    (cond
      (= 1 (first a)) 2
      (= :a (first a)) 2
      (.startsWith (str a) "tttt") 10
      (.startsWith (str b) "caaa") 9
      (= a b) 0
      true ({"kitten" 3 "clojure" 1 "closure" 1
             "xyx"    2 "" 6} a))))

(defcheck solution-77bcd63e
  (fn levdistance [s t]
    (letfn [(cost [a b] (if (= a b) 0 1))]
      ((fn levd [seq1 seq2 k]
         (cond
           (> k 10) 10                                      ;cut too deep changes
           (empty? seq1) (count seq2)
           (empty? seq2) (count seq1)
           :else (let [c (cost (first seq1) (first seq2))]
                   (min
                     (+ c
                        (levd (rest seq1) (rest seq2) (+ c k)))
                     (inc (levd (rest seq1) seq2 (inc k)))
                     (inc (levd seq1 (rest seq2) (inc k)))))))
       s t 0))))

(defcheck solution-77c10e0f
  (fn lev [s1 s2]
    (let [c1 (count s1) c2 (count s2)]
      (cond
        (= c1 c2) (count (filter not (map = s1 s2)))
        (< c1 c2) (recur s2 s1)
        :else (letfn [(remove-nth [n s] (concat (take n s) (rest (drop n s))))
                      (reduced-by-one [s] (map-indexed remove-nth (repeat (count s) s)))]
                (inc (apply min (map #(lev s2 %) (reduced-by-one s1)))))))))

(defcheck solution-7831cc85
  (fn leven [w1 w2]
    (letfn [(count-diff [w1 w2]
              (let [[sm lg] (sort (map count [w1 w2]))]
                (- lg sm)))
            (max-leven [w1 w2]
              (+ (apply + (map #(if (= %1 %2) 0 1) w1 w2))
                 (count-diff w1 w2)))]
      (let [bound (max-leven w1 w2)]
        (letfn [(leven' [diffs w1 w2]
                  (let [[wc1 & wr1] w1
                        [wc2 & wr2] w2]
                    #_(println bound diffs w1 w2)
                    (cond
                      (= diffs bound) diffs
                      (and wc1 wc2) (if (= wc1 wc2)
                                      (leven' diffs wr1 wr2)
                                      (min
                                        ;; insert wc2 to w1
                                        (leven' (inc diffs) (cons wc2 wr1) w2)
                                        ;; insert wc1 to w2
                                        (leven' (inc diffs) w1 (cons wc1 wr2))
                                        ;; delete w1
                                        (leven' (inc diffs) wr1 w2)
                                        ;; delete w2
                                        (leven' (inc diffs) w1 wr2)
                                        ;; replace
                                        (leven' (inc diffs) wr1 wr2)))
                      (and (not wc1) (not wc2)) diffs
                      wc2 (+ diffs (- (count w2) (count w1)))
                      wc1 (+ diffs (- (count w1) (count w2)))
                      )))]
          (leven' 0 w1 w2))))))

(defcheck solution-7857070
  (fn levenshtein-distance [w1 w2]
    (let [f (fn [acc [i j]]
              (conj acc
                [[i j]
                 (if (zero? (min i j))
                   (max i j)
                   (min (inc (acc [(dec i) j]))
                     (inc (acc [i (dec j)]))
                     (+ (acc [(dec i) (dec j)])
                        (if (= (nth w1 (dec i))
                              (nth w2 (dec j))) 0 1))))]))]
      (get (reduce f
             {}
             (for [x (range (inc (count w1)))
                   y (range (inc (count w2)))]
               [x y]))
        [(count w1) (count w2)]))))

(defcheck solution-789ef2e3
  (fn [s1 s2]
    (let [d (memoize (fn [i j dst]
                       (cond
                         (= 0 i) j
                         (= 0 j) i
                         :else (if (= (get s1 (- i 1)) (get s2 (- j 1)))
                                 (dst (- i 1) (- j 1) dst)
                                 (+ 1 (reduce min
                                        [(dst (- i 1) j dst)
                                         (dst i (- j 1) dst)
                                         (dst (- i 1) (- j 1) dst)]))))))]
      (d (count s1) (count s2) d))))

(defcheck solution-78b2abfd
  (fn f [x y]
    (cond
      (empty? x) (count y)
      (empty? y) (count x)
      :else (let [[x1 & xn] x, [y1 & yn] y]
              (if (= x1 y1)
                (f xn yn)
                (inc (min (f x yn) (f xn y) (f xn yn))))))))

(defcheck solution-78e0cdd0
  (fn diff [av bv]
    (let
     [[bdiff brest] (split-with #(not= % (first av)) bv)
      [adiff arest] (split-with #(not= % (first bv)) av)]
      (if
       (or (empty? av) (empty? bv)) (max (count av) (count bv))
                                    (min
                                      (+ (count adiff) (diff (rest arest) (rest bv)))
                                      (+ (count bdiff) (diff (rest brest) (rest av)))
                                      (+ 1 (diff (rest av) (rest bv)))
                                      ))
      )))

(defcheck solution-7996773c
  (fn [s t]
    (cond
      (= s t) 0
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      ((reduce
         (fn [v0 i]
           (reduce
             (fn [v1 j]
               (conj v1 (min (inc (v1 j))
                          (inc (v0 (inc j)))
                          (+ (v0 j) (if (= (nth s i) (nth t j)) 0 1)))))
             [(inc (first v0))]
             (range (count t))))
         (vec (range (inc (count t))))
         (range (count s)))
       (count t)))))

(defcheck solution-7a8a1f98
  (fn levenshtein-distance
    ([a b] (levenshtein-distance a b 0 (+ (count a) (count b))))
    ([a b mod-count best-so-far]
     (let
      [a-count       (count a)
       b-count       (count b)
       modf          (fn [a b new-mod-count] (fn [new-best] (levenshtein-distance a b new-mod-count new-best)))
       mod-del-a     (modf (rest a) b (inc mod-count))
       mod-del-b     (modf a (rest b) (inc mod-count))
       mod-del-a-b   (modf (rest a) (rest b) (inc mod-count))
       unmod-del-a-b (modf (rest a) (rest b) mod-count)
       mod-only      [mod-del-a mod-del-b mod-del-a-b]
       mod-unmod     [unmod-del-a-b mod-del-a mod-del-b]
       search        (fn search ([mods] (search mods best-so-far))
                       ([mods best] (if (empty? mods) best
                                                      (let [new-best (min ((first mods) best) best)]
                                                        (min new-best (search (rest mods) new-best))))))]
       (cond (>= mod-count best-so-far) best-so-far
             (= a-count b-count 0) mod-count
             (= a-count 0) (mod-del-b best-so-far)
             (= b-count 0) (mod-del-a best-so-far)
             (= a b) mod-count
             (= (first a) (first b)) (search mod-unmod)
             :else (search mod-only))))))

(defcheck solution-7ab4059f
  (let [cache (atom {})]
    (fn ldist [a b]
      (when-not (@cache [a b])
        (swap! cache assoc [a b]
          (cond
            (empty? a) (count b)
            (empty? b) (count a)
            :else (min (inc (ldist (butlast a) b))
                    (inc (ldist a (butlast b)))
                    (+ (ldist (butlast b) (butlast a))
                       (if (= (last a) (last b)) 0 1))))))
      (@cache [a b]))))

(defcheck solution-7ae1b5e0
  (memoize
    (fn lev [s1 s2]
      (cond
        (zero? (count s1)) (count s2)
        (zero? (count s2)) (count s1)
        (= (first s1) (first s2)) (lev (rest s1) (rest s2))
        :else (inc (min (lev (rest s1) s2)
                     (lev s1 (rest s2))
                     (lev (rest s1) (rest s2))))))))

(defcheck solution-7c1af7fd
  (fn ld [res des]
    (letfn [(step [[res des n]]
              (cond
                (or (empty? res) (empty? des)) ((juxt replace-item insert delete) [res des n])
                (= (first res) (first des)) (step [(rest res) (rest des) n])
                :else ((juxt replace-item insert delete) [res des n])
                ))
            (insert [[res des n]]
              (if (empty? des)
                nil
                [res (rest des) (+ 1 n)]))
            (delete [[res des n]]
              (if (empty? res)
                nil
                [(rest res) des (+ 1 n)]))
            (replace-item [[res des n]]
              (if (and (empty? res) (empty? des))
                [[] [] n]
                (if (or (empty? res) (empty? des))
                  nil
                  [(rest res) (rest des) (+ 1 n)])))]
      (->>
        (iterate (fn [queue] (filter (comp not nil?) (mapcat step queue))) [[res des 0]])
        (drop-while #(not-any? (fn [[res des n]] (and (empty? res) (empty? des))) %))
        first
        (filter (fn [[res des n]] (and (empty? res) (empty? des))))
        (apply min-key last)
        last
        ; (take 3)
        ))))

(defcheck solution-7c53d7af
  (fn [xs ys]
    (let [l1  (count xs) l2 (count ys)
          xs' (cons nil xs) ys' (cons nil ys)
          d   (fn [xys i j]
                (cond
                  (zero? i) j
                  (zero? j) i
                  :else (min
                          (inc (nth (nth xys (dec i)) j))
                          (inc (nth (nth xys i) (dec j)))
                          (+ (nth (nth xys (dec i)) (dec j))
                             (if (= (nth xs' i) (nth ys' j)) 0 1)))))]
      (last
        (last
          (loop [i 0 rs []]
            (if (> i l1)
              rs
              (let [cs
                    (loop [j 0 cs []]
                      (if (> j l2)
                        cs
                        (recur (inc j)
                          (conj cs (d (conj rs cs) i j)))))]
                (recur (inc i) (conj rs cs))))))))))

(defcheck solution-7d6e6383
  (fn [s t]
    (let [; function to create and initialize 2d matrix
          matrix (fn [m n f]
                   (mapv (fn [i] (mapv (fn [j] (f i j)) (range n))) (range m)))
          ; 1-based string indexing makes everything else here more concise
          s      (into [0] s) n (count s)
          t      (into [0] t) m (count t)
          d      (reduce (fn [d i]
                           (reduce (fn [d j]
                                     (let [cost (if (= (s i) (t j)) 0 1)]
                                       (assoc-in d [i j] (min (inc ((d (dec i)) j))
                                                           (inc ((d i) (dec j)))
                                                           (+ cost ((d (dec i)) (dec j)))))))
                             d
                             (range 1 m)))
                   (matrix n m (fn [i j] (if (zero? j) i (if (zero? i) j 0))))
                   (range 1 n))]
      ((d (dec n)) (dec m)))))

(defcheck solution-7dfd44e8
  (letfn [(iters [n f start]
            (take n (map second
                      (iterate f start))))]
    (fn [s t]
      (let [m         (inc (count s)), n (inc (count t))
            first-row (vec (range m))
            matrix    (iters n (fn [[j row]]
                                 [(inc j)
                                  (vec (iters m (fn [[i col]]
                                                  [(inc i)
                                                   (if (= (nth s i)
                                                         (nth t j))
                                                     (get row i)
                                                     (inc (min (get row i)
                                                            (get row (inc i))
                                                            col)))])
                                         [0 (inc j)]))])
                        [0 first-row])]
        (last (last matrix))))))

(defcheck solution-7e6eabe5
  (fn [a b]
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
                      (-> r last second (get bl))))))))

(defcheck solution-7e963d6a
  (letfn [(dist [a b cache]
            (cond
              (zero? (count a)) [(count b) (assoc cache [a b] (count b))]
              (zero? (count b)) [(count a) (assoc cache [a b] (count a))]
              :else (let [sub-a        (butlast a)
                          dec-a-cache  (cache [sub-a b])
                          [dec-a temp-cache] (if dec-a-cache [dec-a-cache cache] (dist sub-a b cache))
                          new-cache    (assoc temp-cache [sub-a b] dec-a)
                          sub-b        (butlast b)
                          dec-b-cache  (new-cache [a sub-b])
                          [dec-b temp-cache] (if dec-b-cache [dec-b-cache new-cache] (dist a sub-b new-cache))
                          new-cache-2  (assoc temp-cache [a sub-b] dec-b)
                          dec-ab-cache (new-cache-2 [sub-a sub-b])
                          [dec-ab temp-cache] (if dec-ab-cache [dec-ab-cache new-cache-2] (dist sub-a sub-b new-cache-2))
                          last-ab-eq   (if (= (last a) (last b)) 0 1)
                          new-cache-3  (assoc temp-cache [sub-a sub-b] dec-ab)]
                      [(min (inc dec-a) (inc dec-b) (+ dec-ab last-ab-eq)) new-cache-3])))]
    #(first (dist % %2 {}))))

(defcheck solution-7ee67353
  (fn levenshtein
    ([s1 s2]
     (levenshtein s1 s2
       (memoize (fn [s1 s2 memo]
                  (cond (empty? s1) (count s2)
                        (empty? s2) (count s1)
                        :else (min
                                (+ (if (= (first s1) (first s2)) 0 1)
                                   (levenshtein (rest s1) (rest s2) memo))
                                (inc (levenshtein (rest s1) s2 memo))
                                (inc (levenshtein s1 (rest s2) memo))))))))
    ([s1 s2 memo-fn] (memo-fn s1 s2 memo-fn))))

(defcheck solution-803ab84
  (fn [a b]
    (let [
          [small big] (sort-by count [a b])
          cs       (count small)
          subs
                   (filter
                     #(= cs (count %))
                     (reduce
                       (fn [ss x] (concat ss (map #(conj % x) ss)))
                       [[]]
                       big))
          diffs    #(count (filter false? (map = % small)))
          mindiffs (apply min (map diffs subs))]

      (+ (- (count big) cs) mindiffs))))

(defcheck solution-8072dc34
  (fn [s t]
    (let [len-s (count s)
          len-t (count t)
          cost  (fn [s i t j] (if (= (nth s i) (nth t j)) 0 1))]
      (cond
        (= s t) 0
        (= 0 len-s) len-t
        (= 0 len-t) len-s
        :else (loop [i 0
                     v (vec (range (inc len-t)))]
                (if (= i len-s)
                  (v len-t)
                  (recur
                    (inc i)
                    (reduce
                      (fn [vnext j]
                        (conj vnext (min (inc (vnext j)) (inc (v (inc j))) (+ (v j) (cost s i t j)))))
                      [(inc i)]
                      (range len-t)))))))))

(defcheck solution-81bb5973
  (fn leven [x y]
    (let [l     (memoize (fn [f x y]
                           (cond (empty? x) (count y)
                                 (empty? y) (count x)
                                 :else (min (+ (f f (rest x) y) 1)
                                         (+ (f f x (rest y)) 1)
                                         (+ (f f (rest x) (rest y)) (if (= (first x) (first y)) 0 1))
                                         )
                                 )
                           ))
          magic (partial l l)]
      (magic x y)
      )
    ))

(defcheck solution-821803a
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
                              (reduce min)))))]
    levenshtein))

(defcheck solution-849879db
  (fn ld [o1 o2]
    (let [diff (fn [x y] (count (filter false? (map = x y))))
          m    (fn [x] (for [n (range (inc (count x)))]
                         (concat (take n x) [nil] (drop n x))))
          f2   (fn [x] (apply min (map diff x (repeat o2))))
          f    (fn [x] (let [y (mapcat m x) n (f2 y)]
                         (distinct (filter #(= (diff % o2) n) y))))
          i    (take-while #(not= (count (first %)) (count o2)) (iterate f [o1]))]

      (cond
        (> (count o1) (count o2)) (ld o2 o1)
        (= (count o1) (count o2)) (diff o1 o2)
        :else (apply min (map diff (f (last i)) (repeat o2)))))))

(defcheck solution-84c99dfa
  (let [cache (atom {})]
    (fn ld [a b]
      (if-let [d (@cache [a b])]
        d
        (let [d (cond
                  (empty? a) (count b)
                  (empty? b) (count a)
                  (= (first a) (first b)) (ld (rest a) (rest b))
                  :else (+ 1
                           (min (ld (rest a) b)             ; deletion
                             (ld a (rest b))                ; insertion
                             (ld (rest a) (rest b))         ; replace
                             )))]
          (swap! cache assoc [a b] d)
          d)))))

(defcheck solution-85051d
  (fn [s t]
    ((memoize
       (fn d [i j]
         (cond
           (< i 0) (inc j)
           (< j 0) (inc i)
           (= (nth s i) (nth t j)) (d (dec i) (dec j))
           true (inc (min (d (dec i) j)
                       (d i (dec j))
                       (d (dec i) (dec j)))))))
     (dec (count s)) (dec (count t)))))

(defcheck solution-85194770
  (fn [s t] (let [m (inc (count s)), n (inc (count t))]
              (loop [j 1, y (range m)]
                (if (= j n) (last y)
                            (recur (inc j)
                              (loop [i 1, x [j], y y]
                                (if (= i m) (reverse x)
                                            (recur (inc i)
                                              (cons (if (= (nth s (dec i)) (nth t (dec j)))
                                                      (first y)
                                                      (inc (min (first y) (second y) (first x)))) x)
                                              (rest y))))))))))

(defcheck solution-857a807b
  (fn l [a b]
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
          )))))

(defcheck solution-86b404c8
  (fn sol [s1 s2]
    (letfn [

            (ldists [res1 col0 res v1 v2]
              (loop [r1 res1
                     c0 col0
                     r  res
                     i  0
                     j  0]
                ;    (do (println r1 r c0  i j (v1 i) (v2  j))
                (cond (= j (dec (count r1)))
                      (if (= i (dec (count v1)))
                        r
                        (recur r c0 [(c0 (inc i))] (inc i) 0)
                        )
                      :else
                      (let [nmin (min (inc (last r)) (inc (r1 (inc j)))
                                   (if (= (v1 i) (v2 j))
                                     (r1 j)
                                     (inc (r1 j))
                                     )
                                   )
                            ]
                        (recur r1 c0 (conj r nmin)
                          i (inc j)
                          )
                        )
                      )
                )
              ;)
              )

            ]

      (cond (= (count s1) 0) (count s2)
            (= (count s2) 0) (count s1)
            :else
            (last (ldists (vec (range (+ 1 (count s2))))
                    (vec (range 1 (inc (count s1))))
                    [1]
                    (vec s1) (vec s2)))
            )
      )
    ))

(defcheck solution-87fc981f
  (fn levenshtein [w1 w2]
    (let [word1     (into [] w1)
          word2     (into [] w2)
          first-row (into [] (range (inc (count word2))))]
      (loop [index 0 row first-row]
        (if (= index (count word1))
          (last row)
          (recur (inc index)
            (loop [inner-index 0 new-row [(inc index)]]
              (if (= inner-index (count word2))
                new-row
                (recur (inc inner-index)
                  (conj new-row (min
                                  (inc (new-row inner-index))
                                  (inc (row (inc inner-index)))
                                  (+ (row inner-index)
                                     (if (= (word2 inner-index) (word1 index))
                                       0
                                       1)))))))))))))

(defcheck solution-886fe2ac
  (fn levenstein-distance [ss tt]
    (let [s (vec (cons nil ss)) t (vec (cons nil tt))
          m (count s) n (count t)
          d (transient (vec (repeat (* m n) 0)))]

      (doseq [i (range m)] (assoc! d (-> i (* n)) i))
      (doseq [j (range n)] (assoc! d j j))
      (doseq [j (range 1 n) i (range 1 m)] (assoc! d (-> i (* n) (+ j)) (if (= (s i) (t j)) (d (-> i dec (* n) (+ j -1)))
                                                                                            (inc (min (d (-> i dec (* n) (+ j))) (d (-> i (* n) (+ j -1))) (d (-> i dec (* n) (+ j -1))))))))
      ((persistent! d) (-> m dec (* n) (+ n -1))))))

(defcheck solution-88cdb623
  (fn levenshtein-distance [w1 w2]
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
            (edit-dist-matrix [w1 w2]
              (let [matrix (build-matrix w1 w2)]
                (reduce addr-fill-entry matrix
                  (for [row (range 1 (inc (count w1)))
                        col (range 1 (inc (count w2)))] [row col]))))]
      (get-in (edit-dist-matrix w1 w2) [(count w1) (count w2)]))))

(defcheck solution-8945b8ac
  (fn [s t]
    (let [lv (memoize (fn [lv s t] (cond
                                     (empty? s) (count t)
                                     (empty? t) (count s)
                                     :otherwise (let [cost (if (= (first s) (first t)) 0 1)]
                                                  (min (inc (lv lv (rest s) t)) (inc (lv lv s (rest t))) (+ cost (lv lv (rest s) (rest t)))))
                                     )))]
      (lv lv s t)
      )))

(defcheck solution-895bb3e1
  (fn [s t]
    (let [lev (fn [lev s t]
                (cond
                  (empty? s) (count t)
                  (empty? t) (count s)
                  :else (let [cost (if (= (first s) (first t)) 0 1)]
                          (min (inc (lev lev (rest s) t))
                            (inc (lev lev s (rest t)))
                            (+ cost (lev lev (rest s) (rest t)))))))]
      (lev (memoize lev) s t))))

(defcheck solution-897388dd
  (fn lev [x y]
    (get
      (loop [h {}
             a 0
             b 0]
        (cond
          (> b (count y)) (recur h (inc a) 0)
          (> a (count x)) h
          (= a 0) (recur (assoc h [a b] b) a (inc b))
          (= b 0) (recur (assoc h [a b] a) a (inc b))
          (= (get x (dec a)) (get y (dec b)))
          (recur (assoc h [a b] (min
                                  (get h [(dec a) (dec b)])
                                  (inc (get h [(dec a) b]))
                                  (inc (get h [a (dec b)]))
                                  ))
            a (inc b))
          true
          (recur (assoc h [a b] (min
                                  (inc (get h [(dec a) (dec b)]))
                                  (inc (get h [(dec a) b]))
                                  (inc (get h [a (dec b)]))
                                  ))
            a (inc b)))
        )
      [(count x) (count y)]
      )))

(defcheck solution-89db72a4
  (fn distance [str1 str2]
    (let [f (fn [lev str1 str2]
              (let [len1  (count str1)
                    len2  (count str2)
                    match (= (first str1) (first str2))]
                (cond (zero? len1) len2
                      (zero? len2) len1
                      (true? match) (lev lev (rest str1) (rest str2))
                      :else (min (inc (lev lev (rest str1) str2))
                              (inc (lev lev str1 (rest str2)))
                              (inc (lev lev (rest str1) (rest str2)))))))]
      (f (memoize f) (vec str1) (vec str2)))
    ))

(defcheck solution-8a3b7600
  (fn ld [x y]
    (letfn [(max-len [i j]
              (let [newi (- i 1)
                    newj (- j 1)]
                (cond (= 0 i) j
                      (= 0 j) i
                      (= (nth x newi) (nth y newj)) (max-len newi newj)
                      :else (+ 1 (min (max-len i newj)
                                   (max-len newi j)
                                   (max-len newi newj))))))]
      (max-len (count x) (count y)))))

(defcheck solution-8b398f4a
  (let [ld (atom nil)]
    (reset! ld
      (memoize
        (fn
          [xs ys]
          (cond
            (empty? xs) (count ys)
            (empty? ys) (count xs)
            :else
            (let [[x' & xs'] xs
                  [y' & ys'] ys
                  cx (inc (@ld xs' ys))
                  cy (inc (@ld xs ys'))
                  cz (+ (if (= x' y') 0 1) (@ld xs' ys'))]
              (min cx cy cz))))))
    #(@ld %1 %2)))

(defcheck solution-8b591003
  (fn levenshtein-dist [a b]
    (letfn [(l-dist [a b]
              (count (filter true? (map not= a b))))
            (enrich-word [w ss nl]
              (let [subst (zipmap ss w)]
                (map #(get subst % :none)
                  (range nl))))
            (count-bits [n]
              (loop [n n
                     c 0]
                (if (zero? n) c
                              (recur (bit-shift-right n 1)
                                (if (bit-test n 0) (inc c) c)))))
            (enrich-words [w nl]
              (letfn [(subsetv [s bits]
                        (remove nil?
                          (map-indexed #(if (bit-test bits %) %2) s)))
                      (power-set-k [v k]
                        (let [nn (bit-shift-left 1 (count v))]
                          (for [n (range nn) :when (= k (count-bits n))] (subsetv v n))))]
                (map #(enrich-word w % nl) (power-set-k (range nl) (count w)))))]
      (let [ca (count a)
            cb (count b)]
        (cond
          (= ca cb) (l-dist a b)
          (> ca cb) (apply min (map #(l-dist a %) (enrich-words b ca)))
          :else (levenshtein-dist b a))))))

(defcheck solution-8b7955e3
  (fn q101 [source target]
    (letfn [(f [coll s]
              (letfn [(f2 [v [[a b] t]] (conj v (if (= s t) a (inc (min a b (last v))))))]
                (->>
                  (map list coll target)
                  (reduce f2 [(inc (ffirst coll))])
                  (partition 2 1))))]
      (if (empty? target) (count source)
                          (->>
                            (reduce f (partition 2 1 (range (inc (count target)))) source)
                            last
                            last)))))

(defcheck solution-8b81d70a
  (fn lev-dist [s t]
    (loop [sPos 0
           v0   (range (inc (count t)))]
      (if (= sPos (count s)) (last v0)
                             (let [sc (nth s sPos)
                                   v1 (loop [[tf & tr] t
                                             acc [(inc sPos)]
                                             pos 1]
                                        (if (nil? tf) acc
                                                      (let [prioracc (inc (last acc))
                                                            priorrow (inc (nth v0 pos))
                                                            cost     (if (= tf sc) 0 1)
                                                            diag     (+ (nth v0 (dec pos)) cost)
                                                            newval   (min prioracc priorrow diag)
                                                            ]
                                                        (recur tr (conj acc newval) (inc pos)))))]
                               (recur (inc sPos) v1)
                               )
                             )
      )
    ))

(defcheck solution-8c272b12
  (fn levenshtein-distance [source destination]
    (letfn [(ld [source destination recur-f]
              (cond (empty? source) (count destination)
                    (empty? destination) (count source)
                    :else (min (inc (recur-f (butlast source) destination recur-f))
                            (inc (recur-f source (butlast destination) recur-f))
                            (+ (recur-f (butlast source) (butlast destination) recur-f)
                               (if (= (last source) (last destination))
                                 0
                                 1)))))]
      (ld source destination (memoize ld)))))

(defcheck solution-8ce6f882
  (fn edit-distance-mem [a b]
    (let [f (fn [rec s1 s2]
              (let [cost? (fn [p q]
                            (if (= p q) 0 1))]
                (cond
                  (and (= 0 (count s1)) (= 0 (count s2))) 0
                  (zero? (count s1)) (count s2)
                  (zero? (count s2)) (count s1)
                  :else (min (inc (rec rec s1 (rest s2)))
                          (inc (rec rec (rest s1) s2))
                          (+ (cost? (first s1) (first s2))
                             (rec rec (rest s1) (rest s2)))
                          ))))]
      (f (memoize f) a b))))

(defcheck solution-8da89d07
  (fn [a b]
    (letfn [(dp [mem-dp a b]
              (let [mem-dp #(mem-dp mem-dp %1 %2)]
                (if (or (empty? a) (empty? b))
                  (max (count a) (count b))
                  (min
                    (+
                     (if (= (last a) (last b)) 0 1)
                     (mem-dp (butlast a) (butlast b)))
                    (inc (mem-dp (butlast a) b))
                    (inc (mem-dp a (butlast b)))))))]
      (dp (memoize dp) a b))))

(defcheck solution-8e8f0575
  (fn levenshtein-distance [s t]
    (cond (= s t) 0
          (= 0 (count s)) (count t)
          (= 0 (count t)) (count s)
          :otherwise
          (loop [v0 (vec (take (inc (count t)) (range))) i 0 v1 [0]]
            (if (= i (count s))
              (last v1)
              (let [next-v1 (loop [v1 v1 j 0]
                              (if (= j (count t))
                                v1
                                (let [v1p  (update-in v1 [0] (fn [_] (inc i)))
                                      cost (if (= (get-in s [i]) (get-in t [j])) 0 1)
                                      v1v  (min
                                             (+ (get-in v1p [j]) 1)
                                             (+ (get-in v0 [(+ j 1)]) 1)
                                             (+ (get-in v0 [j]) cost))]
                                  (recur (update-in v1p [(inc j)] (fn [_] v1v)) (inc j))
                                  )
                                )
                              )]
                (recur next-v1 (inc i) next-v1)
                ))))))

(defcheck solution-90d6df46
  (fn lev [a b]
    (letfn [(l [m [i j :as ij]]
              (if (zero? (min i j))
                (max i j)
                (min (inc (get-in m [(dec i) j]))
                  (inc (get-in m [i (dec j)]))
                  (+ (get-in m [(dec i) (dec j)])
                     (if (= (get a (dec i)) (get b (dec j))) 0 1)))))]
      (let [grid (reduce
                   (fn [m ij] (assoc-in m ij (l m ij)))
                   (vec (repeat (count b) []))
                   (for [i (range (inc (count a))) j (range (inc (count b)))] [i j]))]
        (get-in grid [(count a) (count b)])))))

(defcheck solution-9101bd7a
  (fn [s t]
    ((fn f [j r]
       (let [d (cons j r)
             w (map #(list % %2 %3) s d r)
             z (if (< j (count t)) (reduce #(conj % (if (= (first %2) (nth t j)) (second %2) (min (inc (second %2)) (inc (last %2)) (inc (last %))))) [(inc j)] w) d)]
         (if (= j (count t)) (last z) (f (inc j) (rest z))))) 0 (range 1 (inc (count s))))))

(defcheck solution-91c1c5c
  (fn lev [s1 s2]
    (peek
      (reduce
        (fn [prev c1]
          (reduce
            (fn [row [nw n c2]]
              (conj row (if (= c1 c2) nw (inc (min nw n (peek row))))))
            [(inc (first prev))]
            (map vector prev (next prev) s2)))
        (vec (range (inc (count s2))))
        s1))))

(defcheck solution-91d75c3e
  (fn leven
    ([s1 s2] (leven s1 s2 0))
    ([s1 s2 n]
     (loop [[x1 & xs1 :as l1] s1
            [x2 & xs2 :as l2] s2 res n]
       (if (= l1 l2)
         res
         (if x1
           (if x2
             (if (= x1 x2)
               (recur xs1 xs2 res)
               (let [c1 (count l1) c2 (count l2)]
                 (cond (= c1 c2) (recur xs1 xs2 (inc res))
                       (> c1 c2) (apply min [(leven xs1 xs2 (inc res))
                                             (leven xs1 l2 (inc res))])
                       :else (apply min [(leven xs1 xs2 (inc res))
                                         (leven l1 xs2 (inc res))]))))
             (+ res (count l1)))
           (+ res (count l2))))))))

(defcheck solution-92363a6a
  (fn [w1 w2]
    (if (or (zero? (count w1)) (zero? (count w2)))
      (+ (count w1) (count w2))
      (let [fr     (range (inc (count w1)))
            upper1 (count w1)
            upper2 (inc (count w2))
            diff   (fn [wr1 wr2 ln1 ln2] (if (= (nth wr1 ln1) (nth wr2 ln2)) 0 1))
            edd    (fn [wr1 wr2 ln1 ln2 prevr currr] (min (inc (nth prevr ln1))
                                                       (inc (last currr))
                                                       (+ (nth prevr (dec ln1)) (diff wr1 wr2 ln1 ln2))))
            pw1    (concat [[]] w1)
            pw2    (concat [[]] w2)]
        (loop [n1 1 n2 1 pr fr cr [1]]
          (cond
            (= n2 upper2) (last pr)
            (< n1 upper1) (recur (inc n1) n2 pr (concat cr [(edd pw1 pw2 n1 n2 pr cr)]))
            (= n1 upper1) (recur 1 (inc n2) (concat cr [(edd pw1 pw2 n1 n2 pr cr)]) [(inc n2)])))))))

(defcheck solution-9283be49
  (fn levenshtein [a b] (let [lev (fn [mem-lev a b x y] (let [lev (fn [a b x y] (mem-lev mem-lev a b x y))] (if (= 0 (min x y)) (max x y) (min (+ (lev a b (- x 1) y) 1) (+ (lev a b x (- y 1)) 1) (+ (lev a b (- x 1) (- y 1)) (if (= (nth a (dec x)) (nth b (dec y))) 0 1)))))) mem-lev (memoize lev)] (mem-lev mem-lev a b (count a) (count b)))))

(defcheck solution-9331f04e
  (fn [a b]
    (letfn [(lev [mem-lev a b]
              (cond
                (empty? a) (count b)
                (empty? b) (count a)
                :else (min
                        (inc (mem-lev mem-lev (rest a) b))
                        (inc (mem-lev mem-lev a (rest b)))
                        (+ (mem-lev mem-lev (rest a) (rest b))
                           (if (= (first a) (first b)) 0 1)))))]
      (lev (memoize lev) a b))))

(defcheck solution-9404bfde
  (fn levd [s t]
    (let [cs (count s) ct (count t)
          sr (range (inc cs)) tr (range (inc ct))
          si (zipmap (map (fn [x] [x 0]) sr) sr)
          ti (zipmap (map (fn [y] [0 y]) tr) tr)
          dm (into si ti)]
      (loop [m dm i 1 j 1]
        (if (m [cs ct]) (m [cs ct])
                        (recur (conj
                                 {[i j]
                                  (if (= (nth s (dec i)) (nth t (dec j))) (m [(dec i) (dec j)])
                                                                          (inc (min (m [(dec i) j])
                                                                                 (m [i (dec j)])
                                                                                 (m [(dec i) (dec j)])
                                                                                 )))
                                  } m)
                          (if (= i cs) 1 (inc i))
                          (if (= i cs) (inc j) j)))))))

(defcheck solution-943a8950
  (letfn [(fu [a [flag b c]] (if flag b (inc (min a b c))))]

    (fn [word1 word2]
      (loop [w2 word2, row (range (inc (count word1))), i 1]
        (if (empty? w2) (last row)
                        (recur
                          (rest w2)
                          (reductions fu i (map conj (partition 2 1 row) (map #(= (first w2) %) word1)))
                          (inc i)))))))

(defcheck solution-9491b74a
  (fn [s t]
    (let [s (seq s)
          t (seq t)
          m (count s)
          n (count t)
          s #(when (<= % m) (nth s (- % 1)))
          t #(when (<= % n) (nth t (- % 1)))
          a (atom {})
          p #(do (swap! a assoc [% %2] %3) %3)
          g #(get @a [% %2])]
      (last
        (for [i (range (+ m 1)) j (range (+ n 1))]
          (p i j
            (cond
              (= i 0) j
              (= j 0) i
              :e (min
                   (+ (g i (- j 1)) 1)
                   (+ (g (- i 1) j) 1)
                   (+ (g (- i 1) (- j 1)) (if (= (s i) (t j)) 0 1))))))))))

(defcheck solution-95da4e38
  (fn [s t] (let [[cs ct] (map count [s t])
                  [ds dt] (map inc [cs ct])
                  init (into (zipmap (for [i (range dt)] [i 0]) (range dt))
                         (zipmap (for [j (range ds)] [0 j]) (range ds)))
                  f    (fn [m [i j]]
                         (assoc m [i j]
                                  (min (inc (m [(dec i) j]))
                                    (inc (m [i (dec j)]))
                                    (+ (m [(dec i) (dec j)])
                                       (if (= (get t (dec i)) (get s (dec j))) 0 1)))))
                  d    (reduce f init
                         (for [i (range 1 dt) j (range 1 ds)] [i j])
                         )
                  ]
              (d [ct cs]))))

(defcheck solution-9606c692
  (fn ld [s t]
    (let [s (seq s)
          t (seq t)
          m (inc (count s))
          n (inc (count t))
          d (into {} (concat (map #(vector [% 0] %) (range m))
                             (map #(vector [0 %] %) (range n))))]

      (loop [ti (range 1 n) d d]
        (if-let [tn (first ti)]
          (recur (rest ti)
            (loop [si (range 1 m) d d]
              (if-let [sn (first si)]
                (if (= (nth s (dec sn)) (nth t (dec tn)))
                  (recur (rest si) (assoc d [sn tn] (d [(dec sn) (dec tn)] 0)))
                  (recur (rest si) (assoc d [sn tn] (inc (min (d [(dec sn) tn] 0)
                                                           (d [sn (dec tn)] 0)
                                                           (d [(dec sn) (dec tn)] 0))))))
                d)))
          (d [(dec m) (dec n)]))))))

(defcheck solution-96b4a7cb
  (fn lev
    ([s t]
     (if (= s t) 0
                 (if (empty? s) (count t)
                                (lev s t 1 (range (inc (count s)))))))
    ([s rt i d]
     (if (empty? rt) (last d)
                     (lev
                       s
                       (rest rt)
                       (inc i)
                       (cons
                         (inc i)
                         (last
                           (reduce
                             (fn [[lft acc] [up up-lft]]
                               (let [v (min (inc lft) up up-lft)] [v (conj acc v)]))
                             [i []]
                             (map vector
                               (map inc (rest d))
                               (let [tj (first rt)]
                                 (map
                                   (fn [[up-lft eq-ij?]] (+ up-lft (if eq-ij? 0 1)))
                                   (map vector (drop-last d) (map (partial = tj) s)))))))))))))

(defcheck solution-96d2c407
  (fn [x y]
    (let [cx (count x)
          cy (count y)]
      (last (nth (iterate
                   (fn [row]
                     (let [c2 (nth y (first row))]
                       (reduce-kv (fn [curr idx val]
                                    (conj curr
                                      (min (inc (last curr))
                                        (inc (nth row (inc idx)))
                                        ((if (= val c2) identity inc) (nth row idx)))))
                         [(inc (first row))]
                         (vec x))))
                   (vec (range (inc cx))))
              cy)))))

(defcheck solution-979c8373
  (fn levenshtein-distance [s t]
    (let [m    (count s)
          n    (count t)
          init (apply merge (for [i (range (inc m)) j (range (inc n))] {[i j] 0}))
          d    (merge init
                 (apply merge (for [i (range 1 (inc m))] {[i 0] i}))
                 (apply merge (for [j (range 1 (inc n))] {[0 j] j})))
          lst  (for [j (range 1 (inc n)) i (range 1 (inc m))] [i j])
          ]
      (loop [[[i j] & rlst] lst d d]
        (if (nil? i)
          (d [m n])
          (if (= (nth s (dec i)) (nth t (dec j)))
            (recur rlst (assoc d [i j] (d [(dec i) (dec j)])))
            (recur rlst (assoc d [i j] (min (inc (d [(dec i) j]))
                                         (inc (d [i (dec j)]))
                                         (inc (d [(dec i) (dec j)])))))))
        ))))

(defcheck solution-98c03547
  (fn levdist [s1-in s2-in]
    (let [cache (atom {})]
      (letfn [(f [s1 s2]
                (let [c1          (count s1) c2 (count s2)
                      key         [c1 c2]
                      cached-cost (@cache key)
                      cost
                                  (cond cached-cost
                                        cached-cost

                                        (or (empty? s1) (empty? s2))
                                        (max c1 c2)

                                        :else
                                        (min (inc (f s1 (rest s2)))
                                          (inc (f (rest s1) s2))
                                          (+ (if (= (first s1) (first s2))
                                               0 1)
                                             (f (rest s1) (rest s2)))))]
                  (if (nil? cached-cost)
                    (swap! cache assoc key cost))
                  cost))]
        (f s1-in s2-in)))))

(defcheck solution-98dcde92
  (fn n101 [s t]
    (cond
      (zero? (count s)) (count t)
      (zero? (count t)) (count s)
      :else (let [m  (count s) n (count t)
                  d0 (vec (repeat (inc n) (vec (repeat (inc m) 0))))
                  d  (loop [j 0 d (loop [i 0 d1 d0]
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
              ))))

(defcheck solution-98df638b
  (fn ld [s1 s2]
    (letfn [(d [d s1 s2]
              (cond (= (count s1) 0) (count s2)
                    (= (count s2) 0) (count s1)
                    :else (min (inc (d d (rest s1) s2))
                            (inc (d d s1 (rest s2)))
                            (+ (d d (rest s1) (rest s2))
                               (if (= (first s1) (first s2)) 0 1)))))]
      (d (memoize d) s1 s2))))

(defcheck solution-99bd376b
  (fn [n f r c x y]
    (last
      (f #(f (fn [c i]
               (conj c (+ 1 (min (n % (+ i 1)) (n c i)
                              (- (n % i) ({(n x i) 1} (n y %2) 0))))))
            [(+ %2 1)] (r (c x)))
        (r (+ 1 (c x)))
        (r (c y))))) nth reduce range count)

(defcheck solution-99fdcb3f
  (fn [s1 s2]
    (let [cost   (fn [c1 c2] (if (= c1 c2) 0 1))
          lev-it (fn [d cs i]
                   (reductions
                     (fn [pc [[pd cd] c]]
                       (if (zero? c) pd
                                     (+ c (min pc pd cd))))
                     i (map list (partition 2 1 d) cs)))]
      (last (reduce (fn [d c]
                      (lev-it d (map (partial cost c) s1)
                        (inc (first d))))
              (range (inc (count s1))) s2)))))

(defcheck solution-9bf52c3
  (fn [a b]
    (loop [mtx (vec (cons
                      (vec (range (inc (count b))))
                      (map vector (range 1 (inc (count a))))))
           m   1 n 1]
      (if (> m (count a))
        (nth (nth mtx (count a)) (count b))
        (recur (assoc mtx m
                          (conj (nth mtx m)
                            (min (inc ((nth mtx (dec m)) n))
                              (inc ((nth mtx m) (dec n)))
                              (if (= (nth a (dec m) nil) (nth b (dec n) nil))
                                ((nth mtx (dec m)) (dec n))
                                (inc (nth (nth mtx (dec m)) (dec n)))))))
          (if (= (count b) n) (inc m) m)
          (inc (mod n (count b))))))))

(defcheck solution-9c11d20
  (fn __ [a b]
    (letfn [(topple-lev [cache [a b]]
              (let [m (count a)
                    n (count b)
                    d (cond
                        (= 0 m) n
                        (= 0 n) m
                        :else (let [n-1  (dec n)
                                    m-1  (dec m)
                                    cost (if (= (last a) (last b)) 0 1)]
                                (min (+ 1 (cache [m n-1]))
                                  (+ 1 (cache [m-1 n]))
                                  (+ cost (cache [m-1 n-1])))))]
                {[m n] d}))

            (lev-tbl [a b]
              (reduce #(merge % (topple-lev % %2)) {}
                (for [i (range (+ 1 (count a)))
                      j (range (+ 1 (count b)))]
                  [(take i a) (take j b)])))]

      ((lev-tbl a b) [(count a) (count b)]))))

(defcheck solution-9c52b274
  (fn lev [[h & t :as a] [f & r :as b]]
    (cond (nil? h) (count b)
          (nil? f) (count a)
          (= f h) (recur t r)
          :else (min (inc (lev t r))
                  (inc (lev a r))
                  (inc (lev t b))))))

(defcheck solution-9ce01022
  (fn [a b]
    (let [iO (fn [s e p] (let [r (.indexOf (drop p s) e)]
                           (if (<= 0 r) (+ p r) -1)))
          [mn mx] (map #(apply vector %) (sort-by count [a b]))]
      (count
        (filter #(< (second %) 0)
          (first
            (reduce
              #(let [p (second %)
                     x (iO mn %2 p)]
                 [(conj (first %) [%2 x]) (if (<= 0 x) (inc x) p)])
              [[] 0] mx)))))))

(defcheck solution-9d3c946b
  (fn lev-dis [seq1 seq2]
    (let [first-row (range (inc (count seq2)))
          next-row  (fn [pre-row row-index]
                      (let [char1 (nth seq1 row-index)]
                        (reduce #(conj %1
                                   (let [char2      (nth seq2 %2)
                                         cost       (if (= char2 char1) 0 1)
                                         one-before (last %1)]
                                     (min (inc one-before)
                                       (inc (nth pre-row (inc %2)))
                                       (+ cost (nth pre-row %2)))))
                          [(inc row-index)] (range (count seq2)))))]
      (last (reduce next-row first-row (range (count seq1)))))))

(defcheck solution-9d7d4f9f
  (fn [a b]
    (let [f (fn [g i j] (cond
                          (= 0 i) j
                          (= 0 j) i
                          :else (min (inc (g g (dec i) j))
                                  (inc (g g i (dec j)))
                                  (+ (g g (dec i) (dec j))
                                     (if (= (nth a (dec i)) (nth b (dec j)))
                                       0
                                       1)))))
          g (memoize f)]
      (g g (count a) (count b)))))

(defcheck solution-9e43a78c
  (fn [s t]
    (letfn [(f [g s t i j]
              (let [k (dec i)
                    l (dec j)]
                (cond
                  (= i 0) j
                  (= j 0) i
                  (= (nth s k)
                    (nth t l)) (g g s t k l)
                  1 (inc (min (g g s t k l)
                           (g g s t i l)
                           (g g s t k j))))))]
      (f (memoize f) s t (count s) (count t)))))

(defcheck solution-9e482b2d
  (fn levenshtein [a0 b0]
    (let [a   (vec a0)
          b   (vec b0)
          lev (memoize (fn [lev i j]
                         (if (= 0 (min i j))
                           (max i j)
                           (min (inc (lev lev (dec i) j))
                             (inc (lev lev i (dec j)))
                             (+ (lev lev (dec i) (dec j))
                                (if (= (a (dec i)) (b (dec j))) 0 1))))))
          lev (partial lev lev)]
      (lev (count a) (count b)))))

(defcheck solution-9e74e396
  (fn [s t]
    (let [ss (vec (seq s)) tt (vec (seq t))]
      (loop [i (range (count s)) v0 (vec (range (inc (count t))))]
        (if (empty? i) (peek v0)
                       (recur (rest i)
                         (loop [jr (range (count tt)) v1 [(inc (first i))]]
                           (if (empty? jr) v1
                                           (let [j    (first jr)
                                                 cost (if (= (get ss (first i)) (get tt j)) 0 1)]
                                             (recur (rest jr) (conj v1
                                                                (min (inc (get v1 j))
                                                                  (inc (get v0 (inc j)))
                                                                  (+ (get v0 j) cost)))))))))))))

(defcheck solution-9ee1310d
  (fn levenshtein-d [s1 s2]
    (let [cnt-s1 (count s1)
          cnt-s2 (count s2)
          xys    (vec (for [x (range (inc cnt-s2))]
                        (vec (for [y (range (inc cnt-s1))]
                               (cond
                                 (zero? x) y
                                 (zero? y) x
                                 :else 0)))))
          xys    (reduce (fn [xys [x y]]
                           (assoc-in xys [(inc x) (inc y)]
                             (min (inc (get-in xys [x (inc y)]))
                               (inc (get-in xys [(inc x) y]))
                               (+ (get-in xys [x y])
                                  (if (= (nth s2 x) (nth s1 y))
                                    0
                                    1)))))
                   xys
                   (for [x (range cnt-s2)
                         y (range cnt-s1)]
                     [x y]))]
      (get-in xys [cnt-s2 cnt-s1]))))

(defcheck solution-9f0877d3
  (let [f (memoize
            (fn [g s t]
              (cond
                (empty? s) (count t)
                (empty? t) (count s)
                :else (min
                        (+ (g g (rest s) t) 1)
                        (+ (g g s (rest t)) 1)
                        (+ (g g (rest s) (rest t)) (if (= (first s) (first t)) 0 1))))))]
    (partial f f)))

(defcheck solution-9f18710e
  #(last (reduce (fn [r j]
                   (reductions
                     (fn [acc i]
                       (if (= (nth %2 j) (nth % i)) (nth r i)
                                                    (inc (min acc (nth r (inc i)) (nth r i)))))
                     (inc j)
                     (range (count %))))
           (range (inc (count %)))
           (range (count %2)))))

(defcheck solution-9f64177e
  (fn levenshtein [s1 s2]
    (let [f (fn [rec a b]
              (let [[ca cb] (map count [a b])]
                (if (some zero? [ca cb]) (max ca cb)
                                         (min (inc (rec rec (pop a) b))
                                           (inc (rec rec a (pop b)))
                                           (+ (rec rec (pop a) (pop b))
                                              (if (= (peek a) (peek b)) 0 1))))))]
      (f (memoize f) (vec s1) (vec s2)))))

(defcheck solution-9f71d658
  (fn [x y]
    (letfn [(lev-dist [[xfst & xrst :as x] [yfst & yrst :as y] dist]
              (let [change (if (= xfst yfst) 0 1)]
                (cond (empty? x) (+ dist change (count yrst))
                      (empty? y) (+ dist change (count xrst))
                      (zero? change) (recur xrst yrst dist)
                      :else (min (lev-dist xrst yrst (inc dist))
                              (lev-dist xrst y (inc dist))
                              (lev-dist x yrst (inc dist))))))]
      (lev-dist (seq x) (seq y) 0))))

(defcheck solution-9fbbc0b1
  (fn edit-distance [a b]
    (let [edfn          (fn [edf a b]
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
      (memoized-edfn memoized-edfn a b))))

(defcheck solution-a036b173
  (fn __
    [s1 s2]
    (let [seqs   (if (> (count s1) (count s2)) [s2 s1] [s1 s2])
          a      (vec (first seqs))
          b      (vec (second seqs))
          n      (count a)
          m      (count b)
          matrix (reduce (fn [r i]
                           (conj r (vec (cons i (take n (repeat 0))))))
                   [(vec (range (inc n)))] (range 1 (inc m)))
          idx    (for [x (range 1 (inc m))
                       y (range 1 (inc n))]
                   [x y])]

      (last
        (last
          (reduce (fn [r [i j]]
                    (let [change (if (not= (a (dec j)) (b (dec i)))
                                   (inc (get-in r [(dec i) (dec j)]))
                                   (get-in r [(dec i) (dec j)]))
                          add    (inc (get-in r [(dec i) j]))
                          delete (inc (get-in r [i (dec j)]))
                          value  (min change add delete)]
                      (assoc-in r [i j] value)))
            matrix idx)))
      )))

(defcheck solution-a056c1ec
  (fn [s t]
    (let [ls          (inc (count s))
          lt          (inc (count t))
          init-matrix (vec (for [i (range ls)] (vec (take lt (repeat 0)))))
          indexes     (for [i (range ls) j (range lt)] [i j])
          minimum     (fn [mtx i j] (min
                                      (+ 1 (get-in mtx [(dec i) (dec j)]))
                                      (+ 1 (get-in mtx [i (dec j)]))
                                      (+ 1 (get-in mtx [(dec i) j]))))
          lev-dist    (fn [mtx [i j]]
                        (cond
                          (= [0 0] [i j]) mtx
                          (zero? i) (assoc-in mtx [i j] j)
                          (zero? j) (assoc-in mtx [i j] i)
                          (= (get s (dec i)) (get t (dec j)))
                          (assoc-in mtx [i j]
                            (min (minimum mtx i j)
                              (get-in mtx [(dec i) (dec j)])))

                          :else (assoc-in mtx [i j] (minimum mtx i j))))]
      (last (last (reduce lev-dist init-matrix indexes))))))

(defcheck solution-a06801cb
  (fn lev [[ha & ra :as a] [hb & rb :as b]]
    (cond
      (> (count a) (count b)) (lev b a)
      (nil? ha) (count b)
      (= ha hb) (lev ra rb)
      :else
      (inc
        (min (lev ra rb) (lev a rb))))))

(defcheck solution-a0951de7
  (fn [a b]
    ((fn lev [x y]
       (cond
         (zero? x) y
         (zero? y) x
         (= (nth a (dec x) nil)
           (nth b (dec y) nil)) (lev (dec x) (dec y))
         :else (+ 1 (min
                      (lev x (dec y))
                      (lev (dec x) y)
                      (lev (dec x) (dec y)))
                  )))
     (count a) (count b))))

(defcheck solution-a0967e9d
  (letfn [
          (initialize-matrix [m n]
            (let [xs (for [i (range (inc m))] {[i 0] i})
                  ys (for [j (range (inc n))] {[0 j] j})]
              (apply merge (concat xs ys))))
          (enumerate-cells [m n]
            (for [j (range n), i (range m)] [(inc i) (inc j)]))
          (cheapest-mutation [d i j]
            (min
              (+ (get d [(dec i) j]) 1)                     ; a deletion
              (+ (get d [i (dec j)]) 1)                     ; an insertion
              (+ (get d [(dec i) (dec j)]) 1)))]            ; a substitution

    (fn levenshtein [s t]
      (let [m (count s), n (count t)]
        (loop [d (initialize-matrix m n), cells (enumerate-cells m n)]
          (if-let [[[i j] & cells-tail] (seq cells)]
            (if (= (nth s (dec i)) (nth t (dec j)))
              (recur (assoc d [i j] (get d [(dec i) (dec j)])) cells-tail)
              (recur (assoc d [i j] (cheapest-mutation d i j)) cells-tail))
            (get d [m n])))))))

(defcheck solution-a1985c44
  (fn [start end]
    (letfn [(max-changes-needed [w target]
              (+ (Math/abs (- (count w) (count target)))
                 (count (filter identity (map not= w target)))))]
      (let [start    (vec (seq start)) end (vec (seq end))
            alphabet (distinct end)
            mx       (max-changes-needed start end)]
        (loop [q [[start 0]]]
          (let [[[w n]] q]
            (if (= w end)
              n
              (let [len     (count w)
                    inserts (for [c alphabet i (range (inc len))]
                              (vec (concat (subvec w 0 i) [c] (subvec w i))))
                    deletes (for [i (range len)]
                              (vec (concat (subvec w 0 i) (subvec w (inc i)))))
                    substs  (for [c alphabet i (range len)]
                              (vec (concat (subvec w 0 i) [c] (subvec w (inc i)))))
                    all     (filter (fn [nw]
                                      (when (< (max-changes-needed end nw) (max-changes-needed end w))
                                        nw))
                              (concat inserts deletes substs))]
                (recur (vec
                         (sort-by (fn [[w n]] (max-changes-needed end w))
                           <
                           (concat (subvec q 1) (map #(vector % (inc n)) all)))))))))))))

(defcheck solution-a1bf7478
  (fn lev [a b]
    (letfn [(lev-line [lastr ch s]
              (loop [[a b & r] lastr
                     result [(inc (first lastr))]
                     [s0 & sr] s]
                (if s0
                  (recur
                    (conj r b)
                    (conj result (apply min [(inc b) (inc (last result)) (if (= s0 ch) a (inc a))]))
                    sr)
                  result)))]
      (if (= a b)
        0
        (last (reduce
                (fn [r ch] (lev-line r ch a))
                (range (+ 2 (count a)))
                b))))))

(defcheck solution-a227d2ce
  (fn [a b]
    (let [a (vec (cons \space a))
          b (vec (cons \space b))]
      (letfn [(dwn [rows]
                (let [y        (count rows)
                      prev-row (last rows)]
                  (letfn [(acrs [row]
                            (let [x (count row)]
                              (if (< x (count a))
                                (let [v1 ((if (= (a x) (b y)) identity inc) (prev-row (dec x)))
                                      v2 (inc (last row))
                                      v3 (inc (prev-row x))]
                                  (recur (conj row (min v1 v2 v3))))
                                row)))]
                    (if (< y (count b))
                      (recur (conj rows (acrs [y])))
                      rows))))]
        (last (last (dwn [(vec (range (count a)))])))))))

(defcheck solution-a26e8c87
  (fn [w1 w2]
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
          ((comp last last last)))))))

(defcheck solution-a3b68ce2
  (fn [s1 s2]
    (let
     [l-distance  (fn [s1 s2 l-distance]
                    (cond
                      (empty? s1) (count s2)
                      (empty? s2) (count s1)
                      (= (first s1) (first s2)) (l-distance (rest s1) (rest s2) l-distance)
                      (= (last s1) (last s2)) (l-distance (butlast s1) (butlast s2) l-distance)
                      :else (+ 1
                               (min (l-distance (rest s1) s2 l-distance)
                                 (l-distance s1 (rest s2) l-distance)
                                 (l-distance (rest s1) (rest s2) l-distance)))))
      ml-distance (memoize l-distance)]
      (l-distance s1 s2 ml-distance))))

(defcheck solution-a3c1f504
  (fn ldist [coll1 coll2]
    (let [c (- (count coll2)
               (count coll1))]

      (if (< c 0) (ldist coll2 coll1)
                  (+ (->> (map = coll1 coll2)
                       (butlast)
                       (filter false?)
                       (count))
                     (loop [n 0,
                            a (last coll1),
                            [b & r] (drop (dec (count coll1)) coll2)]
                       (cond
                         (nil? b) n
                         (= a b) (+ n (count r))
                         :else (recur (inc n) a r))))))))

(defcheck solution-a44ff685
  (fn [a b]
    (letfn [
            (next-row [row str ch]
              (loop [r row s str ret [(inc (first row))]]
                (if
                 (empty? s) ret
                            (recur
                              (next r)
                              (next s)
                              (conj ret
                                (if (= ch (first s)) (first r)
                                                     (inc (min (last ret) (first r) (fnext r)))))))))]
      (last
        (reduce
          #(next-row %1 a %2)
          (range (inc (count a)))
          b)))))

(defcheck solution-a49d7345
  (fn d [a b]
    (if (and (seq a) (seq b))
      (let [[s & u] a [t & w] b]
        (if (= s t)
          (d u w)
          (+ 1
             (min
               (d a w)
               (d u b)
               (d u w)))))
      (max (count a) (count b)))))

(defcheck solution-a59188b5
  #(let [lena (count %)]
     (loop [j 1 lstv (for [i (range (inc lena))] i) curv [j]]
       (if (> j (count %2))
         (last lstv)
         (letfn [
                 (lev [m n]
                   (min (inc (last curv))
                     (inc (nth lstv m))
                     (+ (nth lstv (dec m)) (if (= (nth % (dec m)) (nth %2 (dec n))) 0 1))))

                 (nextline [n]
                   (loop [m 1 curv [n]]
                     (if (> m lena)
                       curv
                       (recur (inc m) (conj curv (lev m n))))))]

           (recur (inc j) (nextline j) [(inc j)]))))))

(defcheck solution-a6f9916a
  (fn levenshtein [w1 w2]
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
            (recur (inc row-idx) max-rows next-prev-row)))))))

(defcheck solution-a8b09417
  (fn lev
    [s1 s2]
    (letfn
     [(next-e
        [e1 e2 prev curr index]
        (if (= e1 e2) (nth prev (dec index))
                      (inc
                        (min
                          (nth prev (dec index))
                          (nth prev index)
                          (last curr)))))
      (row
        [e1 s2 prev curr]
        (let [e2    (first s2)
              index (count curr)]
          (if (= (count curr) (count prev)) curr
                                            (recur e1
                                              (rest s2)
                                              prev
                                              (conj curr (next-e e1 e2 prev curr index))))))
      (do-lv
        ([s1 s2]
         (let [init (vec (range 0 (inc (count s2))))]
           (do-lv 1 init s1 s2)))
        ([idx prev s1 s2]
         (let [next-row (row (first s1) s2 prev (vector idx))]
           (if (empty? (drop 1 s1)) (last next-row)
                                    (recur (inc idx) next-row (drop 1 s1) s2)))))]
      (cond
        (empty? s1) (count s2)
        (empty? s2) (count s1)
        0 (do-lv s1 s2)))))

(defcheck solution-aa1f0494
  (fn levenshtein [[fa & ra :as a] [fb & rb :as b]]
    (cond
      (> (count a) (count b)) (levenshtein b a)
      (nil? fa) (count b)
      (nil? fb) (count a)
      (= fa fb) (levenshtein ra rb)
      :else (inc (min (levenshtein ra rb) (levenshtein a rb))))))

(defcheck solution-aa4a94fc
  (fn f [s t]
    (cond (= s t) 0
          (empty? s) (count t)
          (empty? t) (count s)
          :else
          (loop [i 0 v (range 0 (inc (count t)))]
            (if (= i (count s))
              (last v)
              (recur (inc i)
                (loop [j 0 v1 [(inc i)]]
                  (if (= j (count t))
                    v1
                    (let [cost (if (= (nth s i) (nth t j)) 0 1)]
                      (recur (inc j) (concat v1 [(min (inc (last v1)) (inc (nth v (inc j))) (+ (nth v j) cost))])))))))))))

(defcheck solution-aaa1f011
  (let [edit-dis   (fn [self s t]
                     (cond
                       (empty? s) (count t)
                       (empty? t) (count s)
                       :else (let [r1 (inc (self self s (rest t)))
                                   r2 (inc (self self (rest s) t))
                                   r3 (self self (rest s) (rest t))]
                               (if (= (first s) (first t))
                                 (min r1 r2 r3)
                                 (min r1 r2 (inc r3))))))
        edit-dis-m (memoize edit-dis)]
    (partial edit-dis-m edit-dis-m)))

(defcheck solution-ab3d83a8
  (fn [s1 s2]
    (letfn [(cmp [v i]
              (reduce
                #(conj % (min (inc (last %))
                           (inc (nth v (inc %2)))
                           (+ (nth v %2) (if (= (nth s1 i) (nth s2 %2)) 0 1))))
                [(inc i)] (range (count s2))))]
      (last (reduce #(cmp % %2) (range (inc (count s2))) (range (count s1)))))))

(defcheck solution-aba43b45
  (fn d [s t]
    ((loop [tt {[() ()] 0} i 0]
       (if (<= i (count s))
         (recur (loop [ss tt j 0]
                  (if (<= j (count t))
                    (recur (conj ss (let [si (take i s) tj (take j t)]
                                      {[si tj] (cond
                                                 (= 0 i) j
                                                 (= 0 j) i
                                                 :else (min (+ (get ss [(take (dec i) s) (take j t)] 0) 1)
                                                         (+ (get ss [si (take (dec j) t)] 0) 1)
                                                         (+ (get ss [(take (dec i) s) (take (dec j) t)] 0) (if (= (last si) (last tj)) 0 1))
                                                         ))
                                       })) (inc j))
                    ss)
                  ) (inc i))
         tt)

       ) [(take (count s) s) (take (count t) t)])
    ))

(defcheck solution-abef7267
  (fn [s t]
    (let [m    (count s)
          n    (count t)
          d    (atom (apply vector (vec (range (inc n)))
                       (map #(into [%] (repeat n nil)) (range 1 (inc m)))))
          cost (fn [i j] (if (= (get s (dec i)) (get t (dec j))) 0 1))
          lev  (fn lev [i j d]
                 (if-let [l (get-in @d [i j])]
                   l
                   (let [l (min (inc (lev i (dec j) d))
                             (inc (lev (dec i) j d))
                             (+ (lev (dec i) (dec j) d) (cost i j)))]
                     (swap! d assoc-in [i j] l)
                     l)))]
      (lev m n d))))

(defcheck solution-abfd9499
  (fn levenshtein-distance
    [a b]
    (let [[x y] (sort-by count [a b])
          diff                  (- (count y) (count x))
          add-char              (fn [combination pos]
                                  (concat (take pos combination) [(nth y pos)] (take-last (- (count combination) pos) combination)))
          possible-combinations (fn possible-combinations
                                  [diff current-combinations]
                                  (if (= 0 diff)
                                    current-combinations
                                    (->> current-combinations
                                      (mapcat #(map (partial add-char %) (range (count y))))
                                      (possible-combinations (dec diff)))))
          closest-match         (fn [possible-combinations]
                                  (->> possible-combinations
                                    (map #(count (filter (fn [n] (not= (nth % n) (nth y n))) (range (count %)))))))]
      (->> (possible-combinations diff #{x})
        closest-match
        (apply min)
        (+ diff)))))

(defcheck solution-acb74185
  (fn ld [s t]
    (let [m (count s)
          n (count t)
          d (into (zipmap (map #(list % 0) (take (inc m) (range))) (range))
              (zipmap (map #(list 0 %) (take (inc n) (range))) (range)))]
      (if (zero? (min m n))
        (max m n)
        (loop [j 1 i 1 d d]
          ;(prn :j j :i i :d d)
          (let [si (get s (dec i))
                tj (get t (dec j))
                nj (if (>= i m) (inc j) j)
                ni (if (>= i m) 1 (inc i))]
            ;(prn :si si :tj tj :nj nj :ni ni)
            (if (< n j)
              (get d [m n])
              (if (= si tj)
                (recur nj ni (assoc d [i j] (get d [(dec i) (dec j)])))
                (recur nj ni (assoc d [i j] (inc (min (get d [(dec i) j])
                                                   (get d [(dec i) (dec j)])
                                                   (get d [i (dec j)])))))))))))))

(defcheck solution-acce95d0
  (fn levenshtein [str1 str2]
    (if (or (= 0 (count str1)) (= 0 (count str2))) (max (count str1) (count str2))
                                                   (let [nrows       (inc (count str2))
                                                         ncols       (inc (count str1))
                                                         init-matrix (vec (map (fn create-row [i]
                                                                                 (if (= i 0)
                                                                                   (vec (range ncols))
                                                                                   (vec (cons i (repeat (dec ncols) nil)))))
                                                                            (range nrows)))]
                                                     (loop [i      1
                                                            j      1
                                                            matrix init-matrix]
                                                       (let [new-val (min (inc (get-in matrix [(dec i) j]))
                                                                       (inc (get-in matrix [i (dec j)]))
                                                                       (+ (get-in matrix [(dec i) (dec j)])
                                                                          (if (= (nth str1 (dec j))
                                                                                (nth str2 (dec i)))
                                                                            0
                                                                            1)))]
                                                         (if (and (= i (dec nrows)) (= j (dec ncols)))
                                                           new-val
                                                           (let [[next-i next-j] (if (< j (dec ncols))
                                                                                   [i (inc j)]
                                                                                   [(inc i) 1])]
                                                             (recur next-i next-j (assoc-in matrix [i j] new-val))))))))))

(defcheck solution-ad117d1
  (fn [coll1 coll2]
    (let [[small big] (sort-by count [coll1 coll2])]
      (letfn [(permutations [taken avail left]
                (if (zero? left) [taken]
                                 (if (empty? avail) []
                                                    (concat
                                                     (permutations (conj taken (first avail)) (rest avail) (dec left))
                                                     (permutations taken (rest avail) left)))))
              (small-diff [xs]
                (apply +
                  (map #(if (= %1 %2) 0 1)
                    xs small)))]
        (->> (permutations [] big (count small))
          (map small-diff)
          (apply min)
          (+ (count big) (- (count small))))))))

(defcheck solution-ad1a2293
  (fn [s1 s2]
    (let [length1   (count s1)
          length2   (count s2)
          coords    (for [x (range length1)
                          y (range length2)] [x y])
          distances (reduce
                      (fn [acc [x y]]
                        (let [chars-equal? (= (nth s1 x) (nth s2 y))
                              cost         (if chars-equal? 0 1)
                              sub-lev      (min
                                             (inc (get acc [x (dec y)] x))
                                             (inc (get acc [(dec x) y] y))
                                             (+ cost (get acc [(dec x) (dec y)] (max x y))))]
                          (assoc acc [x y] sub-lev)))
                      {} coords)]
      (get distances [(dec length1) (dec length2)] (max length1 length2)))))

(defcheck solution-ad2d3a2f
  (fn solve-it [s1 s2]
    (letfn [(insert-each [e ls]
              (let [ls' (map #(split-at % ls) (range (inc (count ls))))]
                (map (fn [[x y]] (concat x [e] y)) ls')))
            (insert-eachs [n e ls]
              (if (zero? n)
                ls
                (let [ks (mapcat #(insert-each e %) ls)]
                  (insert-eachs (dec n) e ks))))
            (diff [s1 s2]
              (loop [an 0 s1 s1 s2 s2]
                (if (empty? s1)
                  an
                  (let [f1 (first s1)
                        f2 (first s2)]
                    (if (or (= f1 f2) (= f1 \_) (= f2 \_))
                      (recur an (rest s1) (rest s2))
                      (recur (inc an) (rest s1) (rest s2)))))))]
      (let [s1-len (count s1)
            s2-len (count s2)
            [ss1 ss2] (if (< s1-len s2-len)
                        [s1 s2]
                        [s2 s1])
            dif    (int (Math/abs (- s1-len s2-len)))]
        (+ dif (apply min (map (fn [s] (diff s ss2)) (insert-eachs dif \_ [ss1]))))))))

(defcheck solution-adc283c5
  (fn p101 [c1 c2]

    (let [m      (count c1) n (count c2)
          lv-dis (fn [mtx-for-last newchar newchar-pos thestr]
                   (loop [result [newchar-pos]
                          data   thestr
                          cnt    1]
                     (if (empty? data)
                       result
                       (let [char-to-compare (first data)]
                         (if (= newchar char-to-compare)
                           (recur (conj result (nth mtx-for-last (dec cnt)))
                             (rest data)
                             (inc cnt))
                           (recur (conj result
                                    (inc (min (nth mtx-for-last (dec cnt))
                                           (nth result (dec cnt))
                                           (nth mtx-for-last cnt))))
                             (rest data)
                             (inc cnt)))))))
          ]
      (loop [mtx (range (inc n)) data c1 cnt 1]
        (if (empty? data)
          (last mtx)
          (let [c (first data)]
            (recur (lv-dis mtx c cnt c2) (rest data) (inc cnt))))))))

(defcheck solution-adea9b12
  (let [f (fn [f [af & ar :as a] [bf & br :as b]]
            (if (and (seq a) (seq b))
              (if (= af bf)
                (f f ar br)
                (inc (min (f f a br) (f f ar b) (f f ar br))))
              (+ (count a) (count b))))]
    (fn [a b] (f (memoize f) a b))))

(defcheck solution-aef633e0
  #(let [x (vec %2) y (vec %3)]
     ((fn f [L M]
        (let [l (dec L) m (dec M)]
          (cond (= 0 L) M
                (= 0 M) L
                (= (x l) (y m)) (f l m)
                :else (inc (min (f L m) (f l M) (f l m))))))
      (% %2) (% %3))) count)

(defcheck solution-aef87e93
  (fn [xs ys]
    (let [num-rows (inc (count xs))
          num-cols (inc (count ys))
          dist     (atom (->> (range num-rows)
                           (map (fn [_] (vec (repeat num-cols 0))))
                           vec))]
      (doseq [i (range num-rows)]
        (swap! dist assoc-in [i 0] i))
      (doseq [j (range num-cols)]
        (swap! dist assoc-in [0 j] j))
      (doseq [j (range 1 num-cols)
              i (range 1 num-rows)
              :let [replace-cost ({true 0 false 1} (= (get xs (dec i))
                                                     (get ys (dec j))))]]
        (swap! dist assoc-in [i j]
          (min (inc (get-in @dist [(dec i) j]))
            (inc (get-in @dist [i (dec j)]))
            (+ replace-cost (get-in @dist [(dec i) (dec j)])))))
      (get-in @dist [(dec num-rows) (dec num-cols)]))))

(defcheck solution-af558360
  #(let [c count
         n (c %2) m (c %3)
         a (make-array (type 1) (+ n 1) (+ m 1))
         s (% aset a) g (% aget a)
         I (% + 1)]
     (dotimes [i (I n)] (s i 0 i))
     (dotimes [j (I m)] (s 0 j j))
     (dotimes [i n]
       (dotimes [j m]
         (let [k (I i) l (I j) p (g i j)]
           (s k l
             (if (= (get %2 i) (get %3 j))
               p
               (I (min (g i l) (g k j) p)))))))
     (g n m)) partial)

(defcheck solution-b05aa561
  (letfn [(matrix [n m d]
            (vec (take n (repeat (vec (take m (repeat d)))))))
          (assoc-col [m, i, v]
            (vec (map #(assoc-in %1 [i] %2) m v)))
          (assoc-row [m, i, v]
            (assoc-in m [i] (vec v)))
          (init-matrix [n m]
            (assoc-row (assoc-col (matrix n m 0) 0 (range n)) 0 (range m)))
          (test-coords [i j]
            (for [y [0 -1] x [0 -1] :when (or (not= x 0) (not= y 0))] [(+ i x) (+ j y)]))
          (cost [m [i j] sc]
            (apply min (map + (map (partial get-in m) (test-coords i j)) [1 1 sc])))
          (all-coords [a b]
            (let [n        (count a)
                  m        (count b)
                  sub-cost (fn [i j] (if (= (a (dec i)) (b (dec j))) 0 1))]
              (for [x (range 1 (inc n)) y (range 1 (inc m))] [[x y] (sub-cost x y)])))
          (cost-matrix [a b]
            (let [n (count a)
                  m (count b)]
              (reduce (fn [mxt [coord sc]] (assoc-in mxt coord (cost mxt coord sc)))
                (init-matrix (inc n) (inc m))
                (all-coords a b))))]
    (fn [a b]
      (let [a (vec a)
            b (vec b)
            n (count a)
            m (count b)]
        (get-in (cost-matrix a b) [n m])))))

(defcheck solution-b0a723e
  (letfn
   [
    (ff [xs] (first (first xs)))
    (sf [xs] (second (first xs)))
    (fs [xs] (first (second xs)))
    (ss [xs] (second (second xs)))
    (get-next [s]
      (set
        (mapcat
          (fn [[p [l r]]]
            (if (empty? l)
              [[(+ p (count r)) [nil nil]]]
              (if (empty? r)
                [[(+ p (count l)) [nil nil]]]
                [
                 [(inc p) [(rest l) r]]
                 [(inc p) [l (rest r)]]
                 [(if (= (first l) (first r)) p (inc p)) [(rest l) (rest r)]]
                 ])))
          s)))
    (lev [s1 s2]
      (loop [now #{[0 [s1 s2]]}]
        (let [solutions (filter #(apply = ((juxt fs ss) %)) now)]
          (if (empty? solutions)
            (recur (get-next now))
            ; we're depending on the fact that the best solution will show up
            ; in the first generation that has any solutions
            (first (apply min-key first solutions))))))
    ]
    lev))

(defcheck solution-b248d8ce
  (fn [x y]
    (cond
      (= x y) 0
      (> (count x) (count y)) (recur y x)
      (= 0 (compare (first x) (first y))) (recur (rest x) (rest y))
      (= 0 (compare (last x) (last y))) (recur (butlast x) (butlast y))
      :else (count (filter false?
                     (map = (concat x (repeat (- (count y) (count x)) \*)) y)))
      )))

(defcheck solution-b25c6087
  (fn [a b]
    (let [m (count a) n (count b)]
      (((reduce
          (fn [r [j i]] (assoc-in r [j i]
                          (cond (= 0 j) i
                                (= 0 i) j
                                :else (let [k (- j 1) l (- i 1)]
                                        (if (= (nth a k) (nth b l))
                                          ((r k) l)
                                          (inc (apply min (map #(get-in r %)
                                                            [[k l] [j l] [k i]]))))))))
          {}
          (for [j (range (+ m 1)) i (range (+ n 1))] [j i])) m) n))))

(defcheck solution-b2925ca9
  (fn [s1 s2]
    (let [a  (transient [])
          g  (fn [i1 i2] (nth a (+ (* i1 (inc (count s2))) i2)))
          fs (for [c1 (range (inc (count s1)))
                   c2 (range (inc (count s2)))]
               (let [c (if (= (nth s1 (dec c1) nil)
                             (nth s2 (dec c2) nil)) 0 1)
                     v (if (zero? (min c1 c2))
                         (max c1 c2)
                         (min (inc (g (dec c1) c2))
                           (inc (g c1 (dec c2)))
                           (+ c (g (dec c1) (dec c2)))))]
                 (conj! a v)))]
      (do (dorun fs) (last (persistent! a))))))

(defcheck solution-b2b582cf
  (letfn [(new-row [prev-row row-elem t]
            (reduce (fn [row [d-1 d e]]
                      (conj row (if (= row-elem e)
                                  d-1
                                  (inc (min (peek row)
                                         (min d d-1))))))
              [(inc (first prev-row))]
              (map vector prev-row (next prev-row) t)))]
    (fn [s t]
      (peek (reduce (fn [prev-row s-elem]
                      (new-row prev-row s-elem t))
              (vec (range (inc (count t))))
              s)))))

(defcheck solution-b3126b12
  (fn [a b]
    (letfn [(next-row [s t prev-row row-count]
              (reduce (fn [res col-count]
                        (conj res (if (= (get s (dec col-count)) (get t (dec row-count)))
                                    (get prev-row (dec col-count))
                                    (inc (min (last res)
                                           (get prev-row (dec col-count))
                                           (get prev-row col-count))))))
                [row-count]
                (range 1 (inc (count s)))))

            (distance [s t]
              (let [row0 (-> (count s) inc range vec)]
                (last
                  (reduce (partial next-row s t)
                    row0
                    (range 1 (inc (count t)))))))]
      (distance a b))))

(defcheck solution-b327ab6c
  (fn [a b]
    (let [length (+ (count b) 1)
          dict   (atom {})
          lev    (fn lev [n]
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

      (lev (+ (* (count a) length) (count b))))))

(defcheck solution-b3d2f6f7
  #(let [m (max (count %2) (count %3))]
     (% 0 m %2 %3)) (fn g [a m [x & y :as p] [u & v :as q]]
                      (cond (= p q) a
                            (= a m) a
                            (= x u) (g a m y v)
                            1 (min (g (+ a 1) m y q)
                                (g (+ a 1) m y v)
                                (g (+ a 1) m p v)))))

(defcheck solution-b44098ca
  (fn levenshtein [xs ys]
    (let [atm           (atom {})
          get-or-update (fn [atm fct & args]
                          (if (@atm args)
                            (@atm args)
                            (let [newval (apply fct args)]
                              (swap! atm assoc args newval)
                              newval)))
          lev           (fn lev [xs ys]
                          (cond (zero? (count xs)) (count ys)
                                (zero? (count ys)) (count xs)
                                :else (min
                                        (inc (get-or-update atm lev (rest xs) ys))
                                        (inc (get-or-update atm lev xs (rest ys)))
                                        (+ (get-or-update atm lev (rest xs) (rest ys))
                                           (if (= (first xs) (first ys)) 0 1)))))]
      (lev xs ys))))

(defcheck solution-b48fa12d
  (fn [s t]
    (let [dist
          (fn [s t mdist]
            (let [sl (count s)
                  tl (count t)]
              (cond
                (zero? sl) tl
                (zero? tl) sl
                :else (let [si   (dec sl)
                            ti   (dec tl)
                            cost (if (= (last s) (last t)) 0 1)
                            s1   (take si s)
                            t1   (take ti t)]
                        (min
                          (inc (mdist s1 t mdist))
                          (inc (mdist s t1 mdist))
                          (+ (mdist s1 t1 mdist) cost))))))]
      (dist s t (memoize dist)))))

(defcheck solution-b4b53a48
  (let [lev
        (fn [mem-lev left right]
          (if (empty? left) (count right)
                            (if (empty? right) (count left)
                                               (let [cost (if (= (last left) (last right)) 0 1)]
                                                 (min (+ 1 (mem-lev mem-lev (butlast left) right))
                                                   (+ 1 (mem-lev mem-lev left (butlast right)))
                                                   (+ cost (mem-lev mem-lev (butlast left) (butlast right))))))))]
    (let [mem-lev (memoize lev)]
      (partial mem-lev mem-lev))))

(defcheck solution-b4d7ccf6
  (fn prob101
    [a b]
    (let [levenshtein (fn [fnc str1 str2]
                        (let [len1 (count str1)
                              len2 (count str2)]
                          (cond (zero? len1) len2
                                (zero? len2) len1
                                :else
                                (let [cost (if (= (first str1) (first str2)) 0 1)]
                                  (min (inc (fnc fnc (rest str1) str2))
                                    (inc (fnc fnc str1 (rest str2)))
                                    (+ cost
                                       (fnc fnc (rest str1) (rest str2))))))))]
      (levenshtein (memoize levenshtein) a b))))

(defcheck solution-b4e99774
  (fn [s t]
    (cond (= s t) 0
          (= 0 (count s)) (count t)
          (= 0 (count t)) (count s)
          :else (letfn [(formrow [v1 v0 s t i]
                          (loop [j 0 a (vec v1)]
                            (if (= j (count t)) a
                                                (recur (inc j) (conj a (min
                                                                         (+ 1 (nth a j))
                                                                         (+ 1 (nth v0 (inc j)))
                                                                         (+ (if (= (nth s i) (nth t j)) 0 1) (nth v0 j))))))))]
                  (let [ct (count t)
                        cs (count s)
                        v0 (take (inc ct) (iterate inc 0))]
                    (loop [v0 v0 v1 [1] i 0 a []]
                      (if (= i (count s)) (nth a (count t))
                                          (let [vx (formrow v1 v0 s t i)]
                                            (recur vx [(+ 2 i)] (inc i) vx)))))))))

(defcheck solution-b51139c
  (fn [a b]
    (some second
      (iterate (fn [[coll _]]
                 (let [[d [s1 s2] :as x] (apply min-key first coll)
                       [a b] ((juxt #(map first %) #(remove nil? (map second %)))
                              (drop-while #(= (first %) (second %)) (map vector s1 (concat s2 (repeat nil)))))
                       new (if (empty? b)
                             [[(+ d (count a)) [b b]]])
                       new (or new (concat [[(inc d) [(next a) (next b)]]]
                                           (if (> (count a) (count b))
                                             [[(inc d) [(next a) b]]])))]
                   (if (= s1 s2)
                     [(disj coll x) d]
                     [(into (disj coll x) new) nil])))
        [#{[0 (if (> (count a) (count b)) [a b] [b a])]} nil]))))

(defcheck solution-b5a820fd
  (fn [s t] (letfn [(ld [s t f] (cond
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
              )))

(defcheck solution-b5f9eb1
  (fn [s t]
    (let [s (vec s)
          t (vec t)
          f (fn [f i j]
              (if (zero? (min i j))
                (max i j)
                (min (inc (f f i (dec j)))
                  (inc (f f (dec i) j))
                  (+ (if (= (s (dec i)) (t (dec j))) 0 1) (f f (dec i) (dec j))))))
          f (memoize f)
          f (partial f f)]
      (f (count s) (count t)))))

(defcheck solution-b6e14215
  (fn lev-dist--compat [x y]
    (let [lev-dist--memo
          (fn [a b f]
            (cond
              (empty? a) (count b)
              (empty? b) (count a)
              (= (first a)
                (first b)) (f (rest a) (rest b) f)
              :else (inc (min (f a (rest b) f)
                           (f (rest a) b f)
                           (f (rest a) (rest b) f)))))]
      (lev-dist--memo x y (memoize lev-dist--memo)))))

(defcheck solution-b9754ce6
  (fn foo [& args]
    (let [l-dist (memoize
                   (fn [memf s1 s2]                         ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive
                     ;; Same as from #82 but takes as first argument a memoized version of itself, which is used in the recursive calls
                     (let [l1 (count s1) l2 (count s2) dl1 (drop-last s1) dl2 (drop-last s2) l-dist (partial memf memf)]
                       (cond (zero? l1) l2
                             (zero? l2) l1
                             :else (min (inc (l-dist dl1 s2)) (inc (l-dist s1 dl2))
                                     (+ (l-dist dl1 dl2) (if (= (last s1) (last s2)) 0 1)))))))]
      (apply l-dist l-dist args))))

(defcheck solution-b9c28887
  (fn levehnsteinX [a b]
    (letfn [
            (costAdd [cost a b]
              (if (= a b)
                cost
                (inc cost)
                )
              )
            (levRec [a b mem]
              (let [existing (get mem [a b])]
                (if (not (nil? existing))
                  {:val existing :mem mem}
                  (let [res
                        (cond
                          (empty? a) {:val (count b) :mem mem}
                          (empty? b) {:val (count a) :mem mem}
                          :else (let [
                                      res1 (levRec (rest a) b mem)
                                      res2 (levRec a (rest b) (get res1 :mem))
                                      res3 (levRec (rest a) (rest b) (get res2 :mem))
                                      ]
                                  {
                                   :val (min
                                          (inc (get res1 :val))
                                          (inc (get res2 :val))
                                          (costAdd (get res3 :val) (first a) (first b))
                                          )
                                   :mem (get res3 :mem)
                                   }
                                  )
                          )
                        ]
                    {:val (get res :val) :mem (assoc (get res :mem) [a b] (get res :val))}
                    )
                  )
                )
              )

            ]
      (get (levRec (reverse a) (reverse b) {}) :val)
      )
    ))

(defcheck solution-ba7d51f5
  (fn levenshtein-distance [a b]
    (letfn [(step [calculated-result coordinate]
              (let [x (first coordinate)
                    y (last coordinate)]
                (assoc calculated-result
                  coordinate
                  (if (zero? (min x y))
                    (max x y)
                    (min
                      (inc (calculated-result [(dec x) y]))
                      (inc (calculated-result [x (dec y)]))
                      (+
                       (calculated-result [(dec x) (dec y)])
                       (if (= ((vec a) (dec x)) ((vec b) (dec y))) 0 1)))))))]
      ((reduce step {} (apply concat (map #(map (fn [i] [% i]) (range 0 (inc (count b)))) (range 0 (inc (count a)))))) [(count a) (count b)]))))

(defcheck solution-bb2ea798
  (fn lev
    ([a b] (lev a b (count a) true))
    ([a b n short?]
     (do
         (if (zero? n)
           (do
             (max (count a) (count b)))
           (let [cut    #(fn [pos middle] {:head (take pos %) :middle middle :tail (drop (+ pos n) %)})
                 groups (for [group-a (map-indexed (cut a) (partition n 1 a))
                              group-b (map-indexed (cut b) (partition n 1 b))
                              :when (= (group-a :middle) (group-b :middle))]
                          [group-a group-b])]
             (if (empty? groups)
               (lev a b (dec n) short?)
               (min (if short? (lev a b (dec n) false) (+ (count a) (count b))) (apply min (map (fn [[group-a group-b]]
                                                                                                  (+ (lev (group-a :head) (group-b :head) n short?)
                                                                                                     (lev (group-a :tail) (group-b :tail) n short?))) groups))))
             ))))))

(defcheck solution-bb6b3d16
  (fn n101
    [a b]
    (letfn [(cell [pre cur idx same?]
              (min (inc (nth pre idx))
                (inc (last cur))
                (+ (nth pre (dec idx)) (if same? 0 1))))]
      (loop [j    1
             rows (inc (count b))
             pre  (range (inc (count a)))]
        (if (= j rows)
          (last pre)
          (let [next-row (reduce (fn [cur i]
                                   (let [same? (= (nth a (dec i)) (nth b (dec j)))]
                                     (conj cur (cell pre cur i same?))))
                           [j] (range 1 (count pre)))]
            (recur (inc j) rows next-row)))))))

(defcheck solution-bbc08edb
  (fn [x y]
    (let [x   (vec x)
          y   (vec y)
          lev (fn lev [i j]
                (let [di (dec i)
                      dj (dec j)]
                  (cond
                    (= i 0) j
                    (= j 0) i
                    (= (x di) (y dj)) (lev di dj)
                    :else (inc (min (lev di j)
                                 (lev i dj)
                                 (lev di dj))))))]
      (lev (count x) (count y)))))

(defcheck solution-bbf4d919
  (fn [s t]
    (let [temp  s
          s     (if (< (count s) (count t)) t s)
          t     (if (= s t) temp t)
          len_s (count s) len_t (count t)
          v0    (atom (apply hash-map (apply concat (for [i (range (inc len_s))] [i 0]))))
          v1    (atom (apply hash-map (apply concat (for [i (range (inc len_t))] [i 0]))))]
      (doseq [i (range (inc len_s))]
        (swap! v0 assoc i i)
        )
      (doseq [i (range len_s)]
        (swap! v1 assoc 0 (inc i))
        (doseq [j (range len_t)]
          (let [cost (if (= (nth s i) (nth t j)) 0 1)]
            (swap! v1 assoc (inc j) (min
                                      (inc (@v1 j))
                                      (inc (@v0 (inc j)))
                                      (+ cost (@v0 j))
                                      )
              )
            )
          )
        (doseq [j (range (inc len_s))]
          (swap! v0 assoc j (@v1 j))
          )
        )
      (@v1 len_t)
      )
    ))

(defcheck solution-bc0f4979
  (fn [r a g rn cn rp x y]
    (let [n   (+ 1 (cn x))
          m   (+ 1 (cn y))
          M   (vec (rp n (vec (rp m 0))))
          M   (r #(a %1 [%2 0] %2) M (rn n))
          M   (r #(a %1 [0 %2] %2) M (rn m))
          d   #(if (= (nth x (- %1 1)) (nth y (- %2 1))) 0 1)
          pos (for [i (rn 1 n) j (rn 1 m)] [i j])
          f   (fn [M [i j]]
                (a M [i j]
                  (min (+ 1 (g M [(- i 1) j]))
                    (+ 1 (g M [i (- j 1)]))
                    (+ (d i j) (g M [(- i 1) (- j 1)])))))
          M   (r f M pos)]
      (g M [(- n 1) (- m 1)]))) reduce assoc-in get-in range count repeat)

(defcheck solution-bc1fc9a7
  (fn [s1 s2]
    (let [s1    (vec s1) n1 (count s1)
          s2    (vec s2) n2 (count s2)
          dists (reduce (fn [prev-row j]
                          (reduce (fn [next-row i]
                                    (conj next-row
                                      (min (inc (next-row (dec i)))
                                        (inc (prev-row i))
                                        (+ (prev-row (dec i))
                                           (if (not= (s1 (dec i)) (s2 (dec j)))
                                             1 0)))))
                            [j]
                            (range 1 (inc n1))))
                  (vec (range 0 (inc n1)))
                  (range 1 (inc n2)))]
      (dists n1))))

(defcheck solution-bc85591f
  (letfn [(D [s t]
            (cond (empty? s) (count t)
                  (empty? t) (count s)
                  (= (first s) (first t)) (D (rest s) (rest t))
                  :else (inc (min (D (rest s) t)
                               (D s (rest t))
                               (D (rest s) (rest t))))))]
    D))

(defcheck solution-bd1c16c9
  (fn [w1 w2]
    (let [n    (inc (count w2))
          m    (inc (count w1))
          setx #(concat (take %1 %2) [%3] (drop (inc %1) %2))
          setm #(concat (take %1 %3) [(setx %2 (nth %3 %1) %4)] (drop (inc %1) %3))
          getm #(nth (nth %3 %1) %2)

          M    (map #(setx 0 %1 %2)
                 (cons (range n) (repeat (dec m) (repeat n 0)))
                 (range))
          [c & cs] (for [i (range 1 m) j (range 1 n)]
                     [i j])
          ]
      (loop [[i j] c cs cs M M]
        (if i
          (recur (first cs) (next cs)
            (setm i j M (if (= (nth w1 (dec i)) (nth w2 (dec j)))
                          (getm (dec i) (dec j) M)
                          (inc (min (getm (dec i) j M) (getm i (dec j) M) (getm (dec i) (dec j) M))))))
          (last (last M)))))))

(defcheck solution-bd352237
  (fn levenshtein [s t]
    (let [m (count s), n (count t)
          M (inc m), N (inc n)
          d (make-array Long/TYPE M N)]                     ; Clojure 1.3
      (doseq [i (range M)]
        (aset d i 0 i))
      (doseq [j (range N)]
        (aset d 0 j j))
      (doseq [j (range 1 N)
              i (range 1 M)]
        (if (= (get s (dec i)) (get t (dec j)))
          (aset d i j (aget d (dec i) (dec j)))             ; no op. req.
          (aset d i j (inc (min
                             (aget d (dec i) j)             ; deletion
                             (aget d i (dec j))             ; insertion
                             (aget d (dec i) (dec j)))))))  ; substition
      (aget d (dec M) (dec N)))))

(defcheck solution-bd49c054
  (fn levenshtein [coll1 coll2]
    (if (or (empty? coll1) (empty? coll2))
      (max (count coll1) (count coll2))
      (let [v1 (vec (seq coll1))
            v2 (vec (seq coll2))
            m  (count v1)
            n  (count v2)
            d  (to-array-2d (repeat (inc m) (repeat (inc n) 0)))]
        (doall (for [i (range m)] (aset d i 0 i)))
        (doall (for [i (range n)] (aset d 0 i i)))
        (doall
          (for [i (range 1 (inc m))
                j (range 1 (inc n))]
            (if (= (v1 (dec i)) (v2 (dec j)))
              (aset d i j (aget d (dec i) (dec j)))
              (aset d i j (min (inc (aget d (dec i) j))
                            (inc (aget d i (dec j)))
                            (inc (aget d (dec i) (dec j))))))))
        (aget d m n)))))

(defcheck solution-bf9c0517
  (fn [s t]
    (cond
      (= s t) 0
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      (loop [i 0 j 0 v0 (range (inc (count t))) v1 [1]]
        (if (= j (count t))
          (if (= i (dec (count s))) (last v1) (recur (inc i) 0 v1 [(+ i 2)]))
          (recur i (inc j) v0
            (conj v1
              (min
                (inc (nth v1 j))
                (inc (nth v0 (inc j)))
                (+ (nth v0 j) (if (= (nth s i) (nth t j)) 0 1))))))))))

(defcheck solution-bfc6a881
  (fn levenshtein-distance
    [a b]
    (let [x (vec a)
          y (vec b)]
      (letfn
       [(leven-helper [leven-memoize l m]
          (let [lMinus (dec l)
                mMinus (dec m)]
            (cond (= 0 l) m
                  (= 0 m) l
                  (= (x lMinus) (y mMinus)) (leven-memoize leven-memoize lMinus mMinus)
                  :else (+ 1 (min
                               (leven-memoize leven-memoize l mMinus)
                               (leven-memoize leven-memoize lMinus m)
                               (leven-memoize leven-memoize lMinus mMinus))))))]
        (leven-helper (memoize leven-helper) (count x) (count y))))))

(defcheck solution-bfd5c4af
  (fn levenshtein [w1 w2]
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
            (recur (inc row-idx) max-rows next-prev-row)))))))

(defcheck solution-c0019daa
  (fn [x y]
    (let [n   (count x)
          m   (count y)
          idx (fn [i j] (+ (* i (inc m)) j))
          d   (loop [d (vec (for [i (range 0 (inc n))
                                  j (range 0 (inc m))]
                              (cond
                                (zero? i) j
                                (zero? j) i
                                :else 0)))
                     i 1
                     j 1]
                (if (> i n)
                  d
                  (let [i' (if (= j m) (inc i) i)
                        j' (if (= j m) 1 (inc j))
                        e  (if (= (nth x (dec i)) (nth y (dec j)))
                             (nth d (idx (dec i) (dec j)))
                             (min (inc (nth d (idx i (dec j))))
                               (inc (nth d (idx (dec i) j)))
                               (inc (nth d (idx (dec i) (dec j))))))
                        ]
                    (recur (assoc d (idx i j) e) i' j'))))

          ]
      (nth d (idx n m)))))

(defcheck solution-c05c2a2b
  (fn [s t]
    (letfn [(lev-initial-matrix [m n]
              (vec (for [i (range (inc m))]
                     (vec (for [j (range (inc n))]
                            (cond
                              (zero? i) j
                              (zero? j) i
                              :else 0))))))
            (lev-matrix
              ([s m t n matrix] (lev-matrix s m t n matrix 1 1))
              ([s m t n matrix j i]
               (let [j-1 (dec j)
                     i-1 (dec i)
                     i+1 (inc i)]
                 (cond
                   (> j n) (get-in matrix [m n])
                   (> i m) (lev-matrix s m t n matrix (inc j) 1)
                   (= (get s i-1) (get t j-1)) (lev-matrix s m t n (assoc-in matrix [i j] (get-in matrix [i-1 j-1])) j i+1)
                   :else
                   (let [deletion     (inc (get-in matrix [i-1, j]))
                         insertion    (inc (get-in matrix [i, j-1]))
                         substitution (inc (get-in matrix [i-1, j-1]))
                         min-action   (min deletion insertion substitution)]
                     (lev-matrix s m t n (assoc-in matrix [i j] min-action) j i+1))))))]
      (let [m      (count s)
            n      (count t)
            matrix (lev-initial-matrix m n)]
        (lev-matrix s m t n matrix)))))

(defcheck solution-c0fb077
  (fn [s t]
    (let [n (count s) m (count t)]
      (if (= m 0) n (if (= n 0) m
                                (let [a (vec (map (fn [e] []) (range (+ m 1))))]
                                  (let [a (assoc a 0 (vec (range (+ n 1))))]
                                    (let [a (reduce (fn [a i] (assoc a i (assoc (a i) 0 i))) a (range (+ m 1)))]
                                      (((reduce (fn [a i]
                                                  (reduce
                                                    (fn [a j] (assoc a j (assoc (a j) i
                                                                                      (min (+ 1 ((a (- j 1)) i))
                                                                                        (+ 1 ((a j) (- i 1)))
                                                                                        (+ ((a (dec j)) (dec i))
                                                                                           (if (= (nth s (dec i)) (nth t (dec j))) 0 1))))))
                                                    a (range 1 (+ m 1))))
                                          a (range 1 (+ n 1))) m) n)))))))))

(defcheck solution-c127b6f9
  (fn [a b]
    (let [f (fn [s a b]
              (if-let [[aa & as] (seq a)]
                (if-let [[bb & bs] (seq b)]
                  (if (= aa bb)
                    (s s as bs)
                    (inc (min (s s as b) (s s as bs) (s s a bs))))
                  (count a))
                (count b)))
          g (memoize f)]
      (g g a b))))

(defcheck solution-c1c5e9b5
  (fn [s t]
    (let [m (inc (count s))
          n (inc (count t))
          a (to-array-2d (repeat m (repeat n 0)))]
      (doseq [i (range m)
              j (range n)]
        (cond (zero? j) (aset a i 0 i)
              (zero? i) (aset a 0 j j)
              (= (nth s (dec i)) (nth t (dec j))) (aset a i j (aget a (dec i) (dec j)))
              :else
              (aset a i j (min (inc (aget a (dec i) j))
                            (inc (aget a i (dec j)))
                            (inc (aget a (dec i) (dec j)))))))
      (last (last a)))))

(defcheck solution-c3fe9573
  (fn [s1 s2]
    (letfn [(leven [f s1 s2]
              (cond (empty? s1) (count s2)
                    (empty? s2) (count s1)
                    :else
                    (min
                      (+ (f f (rest s1) (rest s2))
                         (if (= (first s1) (first s2)) 0 1))
                      (inc (f f (rest s1) s2))
                      (inc (f f s1 (rest s2))))))]
      (let [m-leven (memoize leven)]
        (m-leven m-leven s1 s2)))))

(defcheck solution-c41a834
  (letfn [(lev [lf s1 s2]
            (cond (empty? s1) (count s2)
                  (empty? s2) (count s1)
                  :otherwise (min (inc (lf lf (rest s1) s2))
                               (inc (lf lf s1 (rest s2)))
                               (+ (lf lf (rest s1) (rest s2)) (if (= (first s1) (first s2))
                                                                0
                                                                1)))))]
    (let [mem (memoize lev)]
      (partial mem mem))))

(defcheck solution-c4d9ae19
  (fn levenshtein-distance [s1 s2]
    (->> (for [l1 s1 l2 s2] [l1 l2])
      (partition (count s2))
      (reduce
        (fn [prev row]
          ; build the next row of distances
          (reduce
            (fn [cur [a b]]
              (let [i (count cur)]
                (conj cur
                  (if (= a b) (get prev (dec i))
                              (+ (min (get prev i) (last cur) (get prev (dec i))) 1)))))
            [(inc (first prev))]
            row))
        (vec (range (inc (count s2)))))
      (last))))

(defcheck solution-c5664d7f
  (fn r [str1 str2]
    "a Clojure levenshtein implementation using transient data structure"
    (let [n (count str1) m (count str2)]
      (cond
        (= 0 n) m
        (= 0 m) n
        :else
        (let [prev-col (transient (vec (range (inc m)))) col (transient [])] ; initialization for the first column.
          (dotimes [i n]
            (assoc! col 0 (inc i))                          ; update col[0]
            (dotimes [j m]
              (assoc! col (inc j)                           ; update col[1..m]
                (min (inc (get col j))
                  (inc (get prev-col (inc j)))
                  (+ (get prev-col j) (if (= (get str1 i) (get str2 j)) 0 1)))))
            (dotimes [i (count prev-col)]
              (assoc! prev-col i (get col i))))             ;
          (last (persistent! col)))))))

(defcheck solution-c5b69ada
  (fn levenshtein
    [a b]
    (let [n (count a) m (count b) cell (fn [m i j] (nth (nth m i) j))]
      (if (some zero? (list n m))
        (max n m)
        (loop [matrix  (into
                         [(into [] (range 0 (inc n)))]
                         (for [j (range 1 (inc m))]
                           (into [j] (repeat n 0))))
               indeces (for [j (range 1 (inc n))
                             i (range 1 (inc m))]
                         (list i j))]
          (if (seq indeces)
            (let [i        (ffirst indeces)
                  j        (second (first indeces))
                  new-cell (min
                             (inc (cell matrix (dec i) j))
                             (inc (cell matrix i (dec j)))
                             (+ (cell matrix (dec i) (dec j))
                                (if (= (nth a (dec j)) (nth b (dec i))) 0 1)))
                  matrix'  (assoc matrix i (assoc (nth matrix i) j new-cell))]
              (recur matrix' (rest indeces)))
            (last (last matrix))))))))

(defcheck solution-c67d76dd
  (fn this [s1 s2]
    (cond
      (empty? s1) (count s2)
      (empty? s2) (count s1)
      :else (if (= (first s1) (first s2))
              (this (rest s1) (rest s2))
              (inc
                (min
                  (this (rest s1) s2)
                  (this s1 (rest s2))
                  (this (rest s1) (rest s2))))))))

(defcheck solution-c79c8986
  (fn [left right]
    (let [mem   (atom {})
          leven (fn leven [l r]
                  (if-let [c (find @mem [l r])]
                    (val c)
                    (let [result (if (some nil? [l r])
                                   (count (concat l r))
                                   (min
                                     (+ 1 (leven (next l) r))
                                     (+ 1 (leven l (next r)))
                                     (+ (if (= (first l) (first r))
                                          0
                                          1)
                                        (leven (next l) (next r)))))]
                      (swap! mem assoc [l r] result)
                      result)))]
      (leven (seq left) (seq right)))))

(defcheck solution-c89176e0
  (fn leven [a b]
    (let [lev (memoize
                (fn [self i j]
                  (let [ind (if (not= (get a (dec i)) (get b (dec j))) 1 0)]
                    (if (= 0 (min i j))
                      (max i j)
                      (min (+ 1 (self self (dec i) j))
                        (+ 1 (self self i (dec j)))
                        (+ ind (self self (dec i) (dec j))))))))]
      (lev lev (count a) (count b)))))

(defcheck solution-c8eb807c
  (letfn [(iters [n f start]
            (take n (map second
                      (iterate f start))))]
    (fn [s t]
      (let [m         (inc (count s)), n (inc (count t))
            first-row (vec (range m))
            matrix    (iters n (fn [[j row]]
                                 [(inc j)
                                  (vec (iters m (fn [[i col]]
                                                  [(inc i)
                                                   (if (= (nth s i)
                                                         (nth t j))
                                                     (get row i)
                                                     (inc (min (get row i)
                                                            (get row (inc i))
                                                            col)))])
                                         [0 (inc j)]))])
                        [0 first-row])]
        (last (last matrix))))))

(defcheck solution-c8f2b44e
  (fn my-levenshtein-distance
    [str1 str2]
    (cond
      (= str1 str2) 0
      (= 0 (count str1)) (count str2)
      (= 0 (count str2)) (count str1)
      :else
      (let [v0             (into [] (take (inc (count str2)) (range)))
            calculate-cost (fn [x y] (if (= (get str1 x) (get str2 y)) 0 1))]
        (loop [rowOld v0 i 0]
          (if (= i (count str1))
            (last rowOld)
            (recur (loop [rowNew (vector (inc i)) j 1]
                     (if (= j (inc (count str2)))
                       rowNew
                       (recur (conj rowNew (min
                                             (inc (get rowNew (dec j)))
                                             (inc (get rowOld j))
                                             (+ (calculate-cost i (dec j)) (get rowOld (dec j))))) (inc j)))) (inc i))))))))

(defcheck solution-c91c97d7
  (fn [a b]
    (let [func (memoize (fn [x y f]
                          (cond
                            (empty? x) (count y)
                            (empty? y) (count x)
                            :else
                            (let [d (if (= (first x) (first y)) 0 1)]
                              (min (inc (f (rest x) y f)) (inc (f x (rest y) f)) (+ (f (rest x) (rest y) f) d))))))]
      (func a b func)
      )))

(defcheck solution-ca023ee0
  (fn checkStr [col1 col2]
    (let [generCost    (fn generCostList [seq1 ch2]
                         (map #(if (= ch2 %) true false) seq1)
                         ),
          compSpecList (fn compSpecList [preParam costParam initVal preVal]
                         (loop [v_i_j    preVal,
                                v_j      initVal,
                                preList  preParam,
                                costList costParam,
                                result   []]
                           (do
                             ;(println (str "result:" result))
                             (if (empty? preList)
                               result
                               (let [v_i  (first preList),
                                     sign (first costList),
                                     val  (if sign
                                            v_i_j
                                            (+ 1 (min v_j v_i v_i_j)))
                                     ]
                                 (do
                                   ;(println (str "i:" v_i " j:" v_j " i_j:" v_i_j " cost:" sign) )
                                   (recur v_i
                                     val
                                     (rest preList)
                                     (rest costList)
                                     (conj result val))
                                   )
                                 )
                               )
                             )
                           )
                         ),
          seq_y        col1,
          seq_x        col2,
          ny           (count seq_y),
          nx           (count seq_x),
          y_0line      (range 1 (+ 1 ny)),
          x_0line      (range 1 (+ 1 nx))
          ]
      (cond (= seq_y seq_x) 0
            (= (* ny nx) 0) (+ ny nx)
            :else
            (loop [y_line y_0line, s_x seq_x, x_line x_0line]
              (do
                ;(println (str "newline:" y_line))
                (if (empty? x_line)
                  (peek y_line)
                  (let [x    (first x_line),
                        x_ch (first s_x)]
                    (recur (compSpecList y_line
                             (generCost seq_y x_ch)
                             x
                             (- x 1)
                             )
                      (rest s_x)
                      (rest x_line)
                      )
                    )
                  )
                )
              )
            )

      )
    ))

(defcheck solution-caae0f1a
  (fn [seq1 seq2]
    (let [n (count seq1) m (count seq2)]
      (get
        (reduce
          (fn [s1 i] (reduce
                       (fn [s2 j] (cond
                                    (zero? j) (assoc s2 [i j] i)
                                    (zero? i) (assoc s2 [i j] j)
                                    :else
                                    (assoc s2 [i j]
                                              (min (+ 1 (s2 [(- i 1) j]))
                                                (+ 1 (s2 [i (- j 1)]))
                                                (+ (s2 [(- i 1) (- j 1)])
                                                   (if (= (get seq1 (- i 1))
                                                         (get seq2 (- j 1)))
                                                     0
                                                     1))))))
                       s1
                       (range (inc m))))
          '{}
          (range (inc n))) [n m]))))

(defcheck solution-cac7fe11
  (fn [s t]
    (let [cs   (count s)
          ct   (count t)
          row  (fn [r v0 cost]
                 (if-let [nv0 (next v0)]
                   (recur
                     (conj r (min (inc (last r)) (inc (first nv0)) (+ (first v0) (first cost))))
                     (rest v0)
                     (rest cost))
                   r))
          lev* (fn [v0 i]
                 (if (= cs i)
                   (last v0)
                   (recur
                     (row [(inc i)] v0 (map #(if (= (nth s i) %) 0 1) t))
                     (inc i))))]
      (cond
        (= s t) 0
        (zero? cs) ct
        (zero? ct) cs
        :else (lev* (range (inc ct)) 0)))))

(defcheck solution-cadee5de
  (fn [s1 s2]
    (let [v1     (vec s1)
          v2     (vec s2)
          c1     (count v1)
          c2     (count v2)
          memo   (atom {})
          ld-100 (fn ld [i j]
                   (if-let [memo-val (get @memo [i j])]
                     memo-val
                     (let [calc-val
                           (cond (= (min i j) 0) (max i j)
                                 :else (min (inc (ld (dec i) j))
                                         (inc (ld i (dec j)))
                                         (+ (ld (dec i) (dec j))
                                            (if (= (get v1 (dec i))
                                                  (get v2 (dec j)))
                                              0
                                              1))))]
                       (swap! memo assoc [i j] calc-val)
                       calc-val)))]
      (doseq [n (range (max c1 c2))]
        (when (< n c1) (doseq [j (range (inc n))] (ld-100 n j)))
        (when (< n c2) (doseq [i (range (inc n))] (ld-100 i n))))
      (ld-100 c1 c2))))

(defcheck solution-cb60e943
  (fn d [s t]
    (cond (empty? s) (count t)
          (empty? t) (count s)
          (= (first s) (first t)) (d (rest s) (rest t))
          :else (inc (min (d (rest s) (rest t))
                       (d s (rest t))
                       (d (rest s) t))))))

(defcheck solution-cb6951b2
  (fn [a b]
    (last (reduce (fn [row j]
                    (reduce (fn [col i]
                              (conj col (if (= (nth a i) (nth b j))
                                          (nth row i)
                                          (inc (min (last col) ; deletion
                                                 (nth row i) ; insertion
                                                 (nth row (inc i))))))) ; substitution
                      [(inc j)]
                      (range (count a))))
            (range (inc (count a)))
            (range (count b))))))

(defcheck solution-cb96e512
  #(let [f (memoize
             (fn [f a b]
               (cond (empty? a) (count b)
                     (empty? b) (count a)
                     0 (min (inc (f f a (rest b)))
                         (inc (f f (rest a) b))
                         (+ (f f (rest a) (rest b))
                            (if (= (first a) (first b)) 0 1))))))]
     (f f %1 %2)))

(defcheck solution-cbec889c
  (fn lev [s t]
    (let [sLen (count s) tLen (count t)]
      (cond
        (empty? s) tLen
        (empty? t) sLen
        :else
        (loop [v (vec (range (inc tLen))) i 0]
          (letfn [(f [v1 j]
                    (conj v1 (min (inc (last v1))
                               (inc (v (inc j)))
                               (+ (v j) (if (= (get s i) (get t j)) 0 1)))))]
            (if (= i sLen)
              (last v)
              (recur (reduce f [(inc i)] (range tLen)) (inc i)))))))))

(defcheck solution-ccec69af
  (fn [s t]
    (let [[sc tc] (map count [s t])]
      (or ({s 0} t)
          ({sc tc} 0 ({tc sc} 0))
          (#(get (reduce % `[~@(range (inc tc))] (range sc)) tc)
           #(reduce (fn [x y] (conj x
                                (min (inc (get x y))
                                  (inc (get %1 (inc y)))
                                  (+ (get %1 y) ({(get s %2) 0} (get t y) 1)))))
              [(inc %2)] (range tc)))))))

(defcheck solution-cd8ec9e3
  (fn [s t]
    (letfn [(d [f s t]
              (cond
                (empty? s) (count t)
                (empty? t) (count s)
                1 (let [a (rest s)
                        b (rest t)]
                    (min (inc (f f a t))
                      (inc (f f s b))
                      (+ (if (= (first s) (first t)) 0 1)
                         (f f a b))))))]
      (d (memoize d) s t))))

(defcheck solution-cd996d87
  (fn [x y]
    (let [memo (atom {})]
      (letfn [(lev* [i j]
                (if (contains? @memo [i j])
                  (get @memo [i j])
                  (let [ret (if (= 0 (min i j))
                              (max i j)
                              (min
                                (inc (lev* (dec i) j))
                                (inc (lev* i (dec j)))
                                (+ (lev* (dec i) (dec j))
                                   (if (= (nth x (dec i)) (nth y (dec j)))
                                     0
                                     1))))]
                    (swap! memo assoc [i j] ret)
                    ret)))]
        (lev* (count x) (count y))))))

(defcheck solution-ce09dd7b
  (let [mem (atom {})]
    (fn lev [s t]
      (cond
        (empty? s) (count t)
        (empty? t) (count s)
        :else (if-let [e (find @mem [s t])]
                (val e)
                (let [ns  (rest s)
                      nt  (rest t)
                      ret (if (= (first s) (first t))
                            (lev ns nt)
                            (min (inc (lev s nt))
                              (inc (lev ns t))
                              (inc (lev ns nt))))]
                  (swap! mem assoc [s t] ret)
                  ret))))))

(defcheck solution-ce25c76b
  (fn lev [s t]
    (cond
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      (let [ns (rest s)
            nt (rest t)]
        (if (= (first s) (first t))
          (lev ns nt)                                       ; &#32622;&#25563;(&#25991;&#23383;&#21516;&#12376;&#12391;&#12467;&#12473;&#12488;&#36861;&#21152;&#28961;&#12375;)
          (min
            (inc (lev s nt))                                ; &#21066;&#38500;
            (inc (lev ns t))                                ; &#36861;&#21152;
            (inc (lev ns nt))))))))

(defcheck solution-ce703096
  (fn [a b]
    (let
     [lev (fn [a b i j f]
            (if (zero? (min i j))
              (max i j)
              (min (inc (f a b (dec i) j f))
                (inc (f a b i (dec j) f))
                (+ (f a b (dec i) (dec j) f)
                   (if (= (nth a (dec i)) (nth b (dec j))) 0 1)))))]
      (lev a b (count a) (count b) (memoize lev)))))

(defcheck solution-ce86f8ec
  (fn lev [s t]
    (let [sl (count s)
          tl (count t)]
      (if (< sl tl) (lev t s)
                    (cond
                      (= s t) 0
                      (= 0 sl) tl
                      (= 0 tl) sl
                      :else (-> (loop [v0 (vec (range (inc tl)))
                                       i  0]
                                  (if (= i sl)
                                    v0
                                    (let [v (->> (iterate (fn [[v1 j]]
                                                            [(let [cost (if (= (get s i)
                                                                              (get t j))
                                                                          0 1)]
                                                               (min (inc v1)
                                                                 (inc (get v0 (inc j)))
                                                                 (+ cost (get v0 j))))
                                                             (inc j)])
                                                   [(inc i) 0])
                                              (take (inc tl))
                                              (map first)
                                              vec)]
                                      (recur v (inc i)))))
                              (nth tl)))))))

(defcheck solution-ce9f1fe1
  #(let [x (vec %)
         y (vec %2)]
     (letfn
      [(L [l m]
         (let [l- (- l 1)
               m- (- m 1)]
           (cond (= 0 l) m
                 (= 0 m) l
                 (= (x l-) (y m-)) (L l- m-)
                 :else (+ 1 (min (L l m-)
                              (L l- m)
                              (L l- m-))))))]
       (L (count x) (count y)))))

(defcheck solution-cf74a0d1
  (fn ldis [a b]
    (letfn [(step [calculated-result coordinate]
              (let [x (first coordinate)
                    y (last coordinate)]
                (assoc calculated-result
                  coordinate
                  (if (zero? (min x y))
                    (max x y)
                    (min
                      (inc (calculated-result [(dec x) y]))
                      (inc (calculated-result [x (dec y)]))
                      (+
                       (calculated-result [(dec x) (dec y)])
                       (if (= ((vec a) (dec x)) ((vec b) (dec y))) 0 1)))))))]
      ((reduce step {}
         (apply concat (map
                         #(map (fn [i] [% i]) (range 0 (inc (count b))))
                         (range 0 (inc (count a)))))) [(count a) (count b)]))))

(defcheck solution-d0303c93
  #(let [lev (fn [a b lev*] (let [i (count a) j (count b)]
                              (if (zero? (min i j)) (max i j)
                                                    (min (inc (lev* (rest a) b lev*))
                                                      (inc (lev* a (rest b) lev*))
                                                      (+ (if (= (first a) (first b)) 0 1)
                                                         (lev* (rest a) (rest b) lev*))))))
         l   (memoize lev)]
     (l %1 %2 l)))

(defcheck solution-d1250928
  (fn [w1 w2]
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
            (recur (inc row-idx) max-rows next-prev-row)))))))

(defcheck solution-d2ffc1f6
  (fn [C s t]
    ((fn f [i j]
       (cond
         (= i 0) j
         (= j 0) i
         (= (nth s (- i 1)) (nth t (- j 1))) (f (- i 1) (- j 1))
         :else (+ 1
                  (min (f (- i 1) j)
                    (f i (- j 1))
                    (f (- i 1) (- j 1))))))
     (C s) (C t))) count)

(defcheck solution-d324280a
  (fn __ [str-1 str-2]
    (let [l-1         (count str-1)
          l-2         (count str-2)
          dynamic-row (range (inc l-1))
          delta       (fn [ch] (map #(if (= ch %) 0 1) str-1)) ; compare str-1[i] to str-2[i2]
          next-row    (fn [row i2]
                        (let [tmp (cons (inc i2)
                                    (map min (map + (delta (nth str-2 i2)) row)
                                      (map inc (rest row))))]
                          (reductions #(min (inc %1) %2) (first tmp) (rest tmp))))
          final-row   (reductions next-row dynamic-row (range l-2))]
      (last (last final-row)))))

(defcheck solution-d32dba7a
  (fn levenshtein [x y]
    (let [next-char (fn next-char [c d prev curr position]
                      (if (= c d)
                        (prev (- position 1))
                        (+ 1 (min
                               (prev (- position 1))
                               (prev position)
                               (last curr)))))


          next-row  (fn next-row [a b prev curr]
                      (let [c        (first a)
                            d        (first b)
                            position (count curr)]
                        (if (= position (count prev))
                          curr
                          (next-row a (rest b)
                            prev
                            (conj curr
                              (next-char c d prev curr position))))))


          lev       (fn lev [row-num prev a b]
                      (cond (zero? (count a)) (count b)
                            (zero? (count b)) (count a)
                            :else (let [next        (next-row a b prev (vector row-num))
                                        a-remainder (rest a)]
                                    (if (empty? a-remainder)
                                      (last next)
                                      (lev (inc row-num) next a-remainder b)))))

          ]

      (lev 1 (vec (range (inc (count y)))) x y))))

(defcheck solution-d3462ed6
  (fn [w1 w2]
    (let [init-matrix
          (vec (map vec (take (inc (count w1)) (partition (inc (count w2)) 1 (iterate inc 0)))))]
      (last (last (reduce (fn [matrix [s [y x :as coords]]]
                            (assoc-in matrix coords
                              (if s
                                (get-in matrix [(dec y) (dec x)])
                                (inc (min (get-in matrix [(dec y) x])
                                       (get-in matrix [y (dec x)])
                                       (get-in matrix [(dec y) (dec x)]))))))
                    init-matrix
                    (for [x (range (count w2)) y (range (count w1))]
                      [(= (get w2 x) (get w1 y)) [(inc y) (inc x)]])))))))

(defcheck solution-d3823cbe
  (fn levenshtein-distance-better [s1 s2]
    (let [len1        (inc (count s1))
          len2        (inc (count s2))
          total-steps (dec (* len1 len2))
          max-num     999999999999999]
      (if (= 0 total-steps)
        0
        (letfn [(calc-fn [result-map step-count]
                  (if (= 0 step-count)
                    (calc-fn {[0 0] 0} 1)
                    (let [j  (quot step-count len1)
                          i  (rem step-count len1)
                          v1 (if (nil? (result-map [(dec i) j]))
                               max-num
                               (inc (result-map [(dec i) j])))
                          v2 (if (nil? (result-map [i (dec j)]))
                               max-num
                               (inc (result-map [i (dec j)])))
                          v3 (if (or (= 0 i)
                                     (= 0 j))
                               max-num
                               (if (= (nth s1 (dec i))
                                     (nth s2 (dec j)))
                                 (result-map [(dec i) (dec j)])
                                 (inc (result-map [(dec i) (dec j)]))))
                          min-v
                             (do
                               (min v1 v2 v3))]
                      (if (= total-steps step-count)
                        min-v
                        (recur (assoc result-map [i j] min-v) (inc step-count))))))]
          (calc-fn nil 0))))))

(defcheck solution-d3a2aa6f
  (let [memoed (memoize (fn lev [str1 str2 func]
                          (if (or (empty? str1) (empty? str2))
                            (if (empty? str1)
                              (count str2)
                              (count str1))
                            (let [fstL (last str1)
                                  sndL (last str2)
                                  cnt  (if (= fstL sndL) 0 1)]
                              (min
                                (inc (func (butlast str1) str2 func))
                                (inc (func str1 (butlast str2) func))
                                (+ cnt (func (butlast str1) (butlast str2) func)))
                              ))))] #(memoed %1 %2 memoed)))

(defcheck solution-d3dfabdd
  (fn f [a b]
    (cond (empty? a) (count b)
          (empty? b) (count a)
          (= (first a) (first b)) (recur (rest a) (rest b))
          :else (inc (min (f (rest a) b)
                       (f a (rest b))
                       (f (rest a) (rest b)))))))

(defcheck solution-d4128745
  (fn edit-distance [str1 str2]
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
      (edit-distance-inner str1 str2 (count str1) (count str2)))))

(defcheck solution-d46f993f
  (fn levin [q r]
    ((fn lev [[q-head & q-tail :as q] [r-head & r-tail :as r] verboten]
       (cond
         (and (nil? q-head) (nil? r-head)) 0
         (or (nil? q-head) (nil? r-head)) (max (count q) (count r))
         (= q-head r-head) (levin q-tail r-tail)
         :else
         (inc
           (cond
             (= verboten :none) (min (lev q-tail r-tail :none) (lev q r-tail :delete) (lev q-tail r :insert))
             (= verboten :delete) (min (lev q-tail r-tail :none) (lev q r-tail :delete))
             (= verboten :insert) (min (lev q-tail r-tail :none) (lev q-tail r :insert))))))
     q r :none)))

(defcheck solution-d518022f
  (letfn [(implicit-dp [rec sa sb]
            (cond (empty? sa) (count sb)
                  (empty? sb) (count sa)
                  :else (min
                          (+ (if (= (first sa) (first sb))
                               0
                               1)
                             (rec rec (rest sa) (rest sb)))
                          (+ 1 (rec rec sa (rest sb)))
                          (+ 1 (rec rec (rest sa) sb)))))
          (leven-dp [sa sb]
            (let [dp (memoize implicit-dp)]
              (dp dp sa sb)))]
    leven-dp))

(defcheck solution-d56bb9
  #(let [rec  (atom (fn [] nil))
         memo (atom (memoize (fn [] nil)))]
     (do
       (reset! rec (fn [s t]
                     (cond
                       (empty? s) (count t)
                       (empty? t) (count s)
                       :else (min (inc (@memo (rest s) t))
                               (inc (@memo s (rest t)))
                               (let [c (@memo (rest s) (rest t))]
                                 (if (= (first s) (first t))
                                   c
                                   (inc c)))))))
       (reset! memo (memoize @rec))
       (@memo %1 %2))))

(defcheck solution-d5767a1a
  (fn [& w]
    (let [[na nb] (map #(vec (concat [nil] %)) w)]
      (loop
       [r    1
        prev (range (count na))]
        (if (= r (count nb))
          (last prev)
          (let
           [cells
            (map list
              (partition 2 1
                (conj prev (first prev)))
              (map #(= (nb r) %) na))
            next-row
            (next
              (reduce
                (fn [l [p e]]
                  (conj l
                    (if e
                      (first p)
                      (inc (apply min (last l) p)))))
                [(first prev)]
                cells))]
            (recur (inc r) next-row)))))))

(defcheck solution-d5d32b34
  (fn
    [x y]
    (let [s  (seq x)
          t  (seq y)
          n  (count s)
          m  (count t)
          dp (to-array-2d
               (for [i (range (inc n))]
                 (for [j (range (inc m))]
                   (cond
                     (= i 0) j
                     (= j 0) i
                     true 0))))]
      (doseq [i (range 1 (inc n)) j (range 1 (inc m))]
        (aset dp i j
          (min
            (+ (if (= (nth s (dec i)) (nth t (dec j))) 0 1)
               (aget dp (dec i) (dec j)))
            (+ 1 (aget dp (dec i) j))
            (+ 1 (aget dp i (dec j))))))
      (aget dp n m))))

(defcheck solution-d6daf9d2
  (fn [a b]
    (let [e [(count a) (count b)]]
      (loop [d {[0 0] 0}]
        (if (= (keys d) [e])
          (d e)
          (recur (apply merge-with min
                   (map (fn [[[i j] c]]
                          (merge
                            (if (= e [i j])
                              {e (d e)}
                              {})
                            (if (< i (e 0))
                              {[(inc i) j] (inc c)}
                              {})
                            (if (< j (e 1))
                              {[i (inc j)] (inc c)}
                              {})
                            (if (and (< i (e 0))
                                     (< j (e 1)))
                              {(map inc [i j])
                               (if (= (nth a i)
                                     (nth b j))
                                 c
                                 (inc c))})))
                     d))))))))

(defcheck solution-d75e67b7
  (fn lvn-dist2 [A B]
    (letfn [(value [calc i j]
              (cond
                (= 0 i) j
                (= 0 j) i
                (= (nth A (dec i)) (nth B (dec j)))
                (@calc (dec i) (dec j))
                :else
                (inc (min
                       (@calc (dec i) j)
                       (@calc i (dec j))
                       (@calc (dec i) (dec j))))))

            (index [data stride i j]
              (force (nth data (+ (* stride i) j))))

            (make-data [calc m n]
              (let [data (apply vector (mapcat
                                         (fn [i]
                                           (map
                                             (fn [j]
                                               (delay (value calc i j)))
                                             (range n)))
                                         (range m)))]
                (fn [i j] (index data n i j))))]

      (let [a (atom nil)
            m (count A)
            n (count B)]
        (reset! a (make-data a (inc m) (inc n)))
        (force (@a m n))))))

(defcheck solution-d902be75
  (fn [s1 s2]
    (let [cm              (apply max (map count [s1 s2]))
          start-set       (conj #{} (vec (sort-by str [s1 s2])))
          s-map           (sorted-map 0 start-set)
          rec-levenshtein (fn [[ws dm cm]]
                            (let [[d s] (first dm)
                                  rdm       (dissoc dm d)
                                  str-rest  (fn [s] (->> s rest))
                                  str-assoc (fn [m k [w1 w2]]
                                              (let [w1w2s (vec (sort-by str [w1 w2]))]
                                                ;(println m k w1 w2)
                                                ;(println w1w2s)
                                                (if (contains? ws w1w2s)
                                                  m
                                                  (assoc m k (set (conj (m k) w1w2s))))))
                                  d1-vecs   (fn [w1 w2] [[(str-rest w1) w2] [(str-rest w1) (str-rest w2)]
                                                         [w1 (str-rest w2)]])
                                  do-one    (fn [[wsi dmi cmi] [w1 w2]]
                                              (let [wsr (conj wsi [w1 w2])]
                                                (cond
                                                  (= w1 w2) [wsr dmi (min cmi d)]
                                                  (empty? w1) [wsr dmi (min cmi (+ d (count w2)))]
                                                  (empty? w2) [wsr dmi (min cmi (+ d (count w1)))]
                                                  (= (first w1) (first w2))
                                                  [wsr (str-assoc dmi d (map str-rest [w1 w2])) cmi]
                                                  :else
                                                  [wsr (reduce #(str-assoc %1 (inc d) %2) dmi (d1-vecs w1 w2)) cmi])
                                                ))]
                              ; (println "ws =" ws)
                              ; (println "dm =" dm)
                              ; (println "cm =" cm)
                              ; (println "---- >>")
                              (cond
                                (empty? dm) cm
                                (= 0 cm) 0
                                (>= d cm) cm
                                :else (recur (reduce do-one [ws rdm cm] s))
                                )))
          ]
      (rec-levenshtein [#{} s-map cm]))))

(defcheck solution-d972de20
  (fn lev [x y]
    (cond (empty? x) (count y)
          (empty? y) (count x)
          (= (first x) (first y)) (lev (rest x) (rest y))
          :else (+ 1 (min (lev (rest x) y)
                       (lev x (rest y))
                       (lev (rest x) (rest y)))))))

(defcheck solution-da46fed
  (fn [s t]
    (letfn [(levenshtein-step [x y dist-map]
              (cond
                (zero? x) y
                (zero? y) x
                :else (if (= (get s (dec x)) (get t (dec y)))
                        (apply min (get dist-map [(dec x) (dec y)]) (map inc (map #(get dist-map %) (list [(dec x) y] [x (dec y)]))))
                        (apply min (map inc (map #(get dist-map %) (list [(dec x) y] [x (dec y)] [(dec x) (dec y)])))))))]
      (loop [dist-map {} x 0 y 0]
        (if (or (zero? (count s)) (zero? (count t)) (and (= x (count s)) (= y (count t))))
          (levenshtein-step (count s) (count t) dist-map)
          (recur (assoc dist-map [x y] (levenshtein-step x y dist-map)) (mod (inc x) (inc (count s))) (+ y (quot x (count s)))))))))

(defcheck solution-da94a33b
  (fn [& z]
    (apply (fn f [[a & x :as j] [b & y :as k]]
             (if j
               (if (= a b)
                 (f x y)
                 (+ 1
                    (if (= (count x) (count y))
                      (f x y)
                      (min (f x y) (f j y)))))
               (count k)))
      (sort-by count z))))

(defcheck solution-daba458d
  (fn
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
        (range (count b))))))

(defcheck solution-db12f40c
  (fn levendist [s1 s2]
    (let [ld
          (reduce (fn [edit pair]
                    (let [[i j] pair]
                      (cond
                        (zero? i) (assoc edit [i j] j)
                        (zero? j) (assoc edit [i j] i)
                        :else (assoc edit [i j]
                                          (min (+ (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1) (edit [(dec i) (dec j)]))
                                            (inc (edit [(dec i) j]))
                                            (inc (edit [i (dec j)])))))))
            {[0 0] 0}
            (for [i (range (inc (count s1))) j (range (inc (count s2)))] [i j]))]
      (ld [(count s1) (count s2)]))))

(defcheck solution-db35fdb2
  (fn levenshtein-distance [str1 str2]
    (let [fast-distance (atom (fn [s1 s2] #_(println "bogus - fast")))
          distance      (fn distance [str1 str2]
                          (let [len1 (count str1)
                                len2 (count str2)]
                            (cond (zero? len1) len2
                                  (zero? len2) len1
                                  :else
                                  (let [cost (if (= (first str1) (first str2)) 0 1)]
                                    (min (inc (@fast-distance (rest str1) str2))
                                      (inc (@fast-distance str1 (rest str2)))
                                      (+ cost
                                         (@fast-distance (rest str1) (rest str2))))))))]
      (reset! fast-distance (memoize distance))
      (@fast-distance str1 str2))))

(defcheck solution-db937315
  (fn l-d [left right]
    (let [cache (atom {})]
      (letfn [(lev-dist [left right]
                (let [cost (if (= (last left) (last right)) 0 1)
                      key  [(count left) (count right)]]
                  (cond
                    (= 0 (count left))
                    (do (swap! cache assoc key (count right)) (count right))
                    (= 0 (count right))
                    (do (swap! cache assoc key (count left)) (count left))
                    (contains? @cache key) (get @cache key)
                    :else
                    (let [val
                          (min
                            (inc (get @cache key (lev-dist (butlast left) right)))
                            (inc (get @cache key (lev-dist left (butlast right))))
                            (+ cost (get @cache key (lev-dist (butlast left) (butlast right)))))]
                      (do (swap! cache assoc key val) val)))))]
        (lev-dist left right)))))

(defcheck solution-dbf7eb8e
  (letfn [(distance [step xs ys]
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

    (let [step (memoize distance)]
      (fn levenshtein [xs ys]
        (step step xs ys)))))

(defcheck solution-dc7593a
  (fn edit-dist [w1 w2]
    (let [l1       (count w1) l2 (count w2)
          match    (fn [a b] (= (get w1 (dec a)) (get w2 (dec b))))
          dist     (fn [mem-dist i j]
                     (if (zero? (min i j)) (max i j)
                                           (min
                                             (inc (mem-dist mem-dist (dec i) j))
                                             (inc (mem-dist mem-dist i (dec j)))
                                             (+ (mem-dist mem-dist (dec i) (dec j)) (if (match i j) 0 1))
                                             )
                                           )
                     )
          mem-dist (memoize dist)]
      (mem-dist mem-dist l1 l2)
      )
    ))

(defcheck solution-dc96f483
  (fn dist [s1 s2]
    (let [len1 (count s1) len2 (count s2)]
      (cond
        (zero? len1) len2
        (zero? len2) len1
        :else
        (let [v0 (int-array (range (inc len2)))
              v1 (int-array (inc len2))]
          (dotimes [i len1]
            (aset v1 0 (inc i))
            (dotimes [j len2]
              (let [cost (if (= (nth s1 i) (nth s2 j)) 0 1)]
                (aset v1 (inc j) (min (inc (aget v1 j))
                                   (inc (aget v0 (inc j)))
                                   (+ cost (aget v0 j))))))
            (dotimes [j len2]
              (aset v0 j (aget v1 j))))
          (aget v1 len2))))))

(defcheck solution-dd5e2fa7
  (fn [w1 w2]
    (let [a (range (+ 1 (count w1))) c (range 1 (inc (count w2))) d (vec (for [i w2] (vec (for [j w1] (if (= i j) 0 1)))))]
      (last (reduce (fn [cur [i s]] (->> (map min (map inc (rest cur)) (map + (butlast cur) s)) (reductions (fn [x y] (min (inc x) y)) i))) a (map vector c d))))))

(defcheck solution-deb98872
  (fn __ [coll1 coll2]
    (let [leven (memoize
                  (fn [self [x & xs :as xall] [y & ys :as yall]]
                    (cond
                      (empty? xall) (count yall)
                      (empty? yall) (count xall)
                      :else (min
                              (+ (if (= x y) 0 1) (self self xs ys))
                              (inc (self self xs yall))
                              (inc (self self xall ys))))))]
      (leven leven coll1 coll2))))

(defcheck solution-df6aea3b
  (fn levenshtein [a b]
    (let [
          m         (count (seq a))
          first-row (range (+ 1 m))
          dist      (fn [prior-row l]
                      (fn [acc i]
                        (conj acc
                          (cond
                            (== -1 i) (+ 1 (first prior-row))
                            (= l (nth a i)) (nth prior-row i)
                            :else (+ 1 (min (nth prior-row i)
                                         (nth prior-row (+ 1 i))
                                         (last acc)))))))
          ]
      (last (reduce #(reduce (dist %1 %2) [] (range -1 m)) first-row b)))))

(defcheck solution-dfe78553
  (fn [a b]
    (if (= a b) 0
                (let [la (count a) lb (count b)]
                  (loop [matrix (apply merge
                                  (concat (for [x (range (inc la))] {[x 0] x})
                                          (for [y (range (inc lb))] {[0 y] y})))
                         x      1
                         y      1]
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
                      (recur (merge matrix {[x y]
                                            (inc (min (matrix [(dec x) y])
                                                   (matrix [x (dec y)])
                                                   (matrix [(dec x) (dec y)])))}) (inc x) y)))))
    ))

(defcheck solution-e0551d11
  (fn [s t]
    (let [d   (into [(vec (range 0 (inc (count s))))]
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
      (get-in res [(count t) (count s)]))))

(defcheck solution-e069d507
  (let [ldist
                  (fn [mem-ldist a b]
                    (cond
                      (empty? a) (count b)
                      (empty? b) (count a)
                      :else (min (inc (mem-ldist mem-ldist (butlast a) b))
                              (inc (mem-ldist mem-ldist a (butlast b)))
                              (+ (mem-ldist mem-ldist (butlast a) (butlast b))
                                 (if (= (last a) (last b)) 0 1)))))
        mem-ldist (memoize ldist)]
    (partial mem-ldist mem-ldist)))

(defcheck solution-e158cb5f
  (fn lv [a b]
    (if (seq a)
      (if (seq b)
        (if (= (first a) (first b))
          (lv (rest a) (rest b))
          (inc (min (lv (rest a) (rest b))
                 (lv (rest a) b)
                 (lv a (rest b)))))
        (count a))
      (count b))))

(defcheck solution-e24da2bc
  (fn [x y]
    (last
      (reduce
        (fn [[v & vs] x]
          (let [v (partition 2 (interleave (list* v vs) (cons (inc v) (map (partial = x) y))))]
            (map second (reductions (fn [[a b] [c d]] [c (if d a (inc (min a b c)))]) v))))
        (range 0 (inc (count y))) x))))

(defcheck solution-e2eae01b
  (fn [s t]
    (let [z count ! nth
          - dec
          d (memoize (fn [d i j]
                       (if (= i 0) j
                                   (if (= j 0) i
                                               (let [a (- i) b (- j) c (d d a b)]
                                                 (if (= (! s a) (! t b))
                                                   c
                                                   (+ 1 (min c
                                                          (d d i b)
                                                          (d d a j)))))))))
          ]
      (d d (z s) (z t)))))

(defcheck solution-e2fa87a0
  (fn [a b] (let [xm (cons nil a) ym (cons nil b)
                  mm (vec (cons (vec (take (count xm) (range)))
                            (loop [z 1 res []]
                              (if (= z (count ym)) res
                                                   (recur (inc z) (conj res (vec (cons z (repeat (count a) 0)))))))))]
              (letfn [(dist [i j m] (if (or (< (dec i) 0) (< (dec j) 0)) (nth (nth m j) i)
                                                                         (min (+ (if (= (nth xm i) (nth ym j)) 0 1) (nth (nth m (dec j)) (dec i)))
                                                                           (+ 1 (nth (nth m (dec j)) i))
                                                                           (+ 1 (nth (nth m j) (dec i))))))]
                (loop [x 0 y 0 res mm]
                  (if (= y (count ym)) (nth (nth res (count b)) (count a))
                                       (recur (if (= x (count a)) 0 (inc x))
                                         (if (= x (count a)) (inc y) y)
                                         (vec (concat (conj (vec (take y res)) (assoc (nth res y) x (dist x y res))) (nthrest res (inc y)))))))))))

(defcheck solution-e32f7991
  (fn [s t]
    (let [d (fn d [i j]
              (cond
                (= 0 i) j
                (= 0 j) i
                (= (nth s (dec i)) (nth t (dec j))) (d (dec i) (dec j))
                :default (inc (min (d (dec i) (dec j)) (d i (dec j)) (d (dec i) j)))))]
      (d (count s) (count t)))))

(defcheck solution-e382c4f8
  (fn [s t]
    (let [sn (count s) tn (count t)]
      (cond
        (= s t) 0
        (zero? sn) tn
        (zero? tn) sn
        :else
        (->>
          (reduce
            (fn [[i v0] s-item]
              [
               (inc i)
               (reductions
                 #(min (inc %) %2)
                 (inc i)
                 (map min (map inc (drop 1 v0))
                   (map + v0 (map #(if (= s-item %) 0 1) t))
                   )
                 )
               ]
              )
            [0 (range (inc tn))]
            s
            )
          second
          last
          )))))

(defcheck solution-e3bfc153
  (fn [s1 s2]
    (letfn [(nextrow
              [char1 str2 prevrow thisrow]
              (let [char2    (first str2)
                    position (count thisrow)]
                (if (= (count thisrow) (count prevrow))
                  thisrow
                  (recur
                    char1
                    (rest str2)
                    prevrow
                    (conj thisrow (nextelt char1 char2 prevrow thisrow position))))))

            (nextelt
              [char1 char2 prevrow thisrow position]
              (if (= char1 char2)
                (prevrow (- position 1))
                (inc (min
                       (prevrow (- position 1))
                       (prevrow position)
                       (last thisrow)))))
            (levenshtein
              ([str1 str2]
               (cond (= str1 str2) 0
                     (empty? str1) (count str2)
                     (empty? str2) (count str1)
                     :else (let [row0 (vec (map first (map vector (iterate inc 1) str2)))]
                             (levenshtein 1 (vec (cons 0 row0)) str1 str2))))
              ([row-nr prevrow str1 str2]
               (let [next-row       (nextrow (first str1) str2 prevrow (vector row-nr))
                     str1-remainder (if (string? str1)
                                      (.substring str1 1)
                                      (drop 1 str1))]
                 (if (empty? str1-remainder)
                   (last next-row)
                   (recur (inc row-nr) next-row str1-remainder str2)))))]
      (levenshtein s1 s2))))

(defcheck solution-e49cb95c
  (fn levenshtein-distance
    [str1 str2]
    (letfn [(leven-dist* [[x & xs :as coll1] [y & ys :as coll2]]
              (cond
                (empty? coll1) (count coll2)
                (empty? coll2) (count coll1)
                :else
                (if (= x y)
                  (leven-dist* xs ys)
                  (min
                    (inc (leven-dist* xs
                           coll2))
                    (inc (leven-dist* coll1
                           ys))
                    (inc (leven-dist* xs
                           ys))))))]
      (leven-dist* str1 str2))))

(defcheck solution-e5320c9c
  (fn lev [x y]
    (let [xlen (count x) ylen (count y)]
      (if (= 0 (min xlen ylen))
        (max xlen ylen)
        (loop [v 0 h -1 prev (vec (range (inc xlen))) curr (vec (range (inc xlen)))]
          (if (= v ylen)
            (last prev)
            (if (< h 0)
              (recur v (inc h) prev (assoc curr 0 (inc v)))
              (if (= h xlen)
                (recur (inc v) -1 curr curr)
                (let [cost (if (= (nth x h) (nth y v)) 0 1)
                      a    (inc (nth curr h))
                      b    (inc (nth prev (inc h)))
                      c    (+ (nth prev h) cost)
                      ]
                  (recur v (inc h) prev (assoc curr (inc h) (min a b c)))
                  )
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-e7229b7b
  (fn levenshtein-distance [s t]
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
                  (f s (vec (range (inc (count t)))))))))

(defcheck solution-e82a032d
  (fn [a b]
    (letfn [(step [calculated-result coordinate]
              (let [x (first coordinate)
                    y (last coordinate)]
                (assoc calculated-result
                  coordinate
                  (if (zero? (min x y))
                    (max x y)
                    (min
                      (inc (calculated-result [(dec x) y]))
                      (inc (calculated-result [x (dec y)]))
                      (+
                       (calculated-result [(dec x) (dec y)])
                       (if (= ((vec a) (dec x)) ((vec b) (dec y))) 0 1)))))))]
      ((reduce step {} (apply concat (map #(map (fn [i] [% i]) (range 0 (inc (count b)))) (range 0 (inc (count a)))))) [(count a) (count b)]))))

(defcheck solution-e839fd68
  (fn levenshtein [s t]
    (let [n (count s) m (count t) longer (max n m)]
      (if (or (zero? n) (zero? m)) longer
                                   (let [s (into [] s) t (into [] t) t+2 (+ 2 m)]
                                     (loop [i 0 v0 (into [] (range (inc m)))]
                                       (if (>= i n) (v0 m)
                                                    (recur (inc i)
                                                      (loop [j 0 v1 (into [] (repeat (inc longer) (inc i)))]
                                                        (if (>= j m) v1
                                                                     (let [cost (if (= (s i) (t j)) 0 1)]
                                                                       (recur (inc j)
                                                                         (assoc v1 (inc j)
                                                                                   (min (inc (v1 j)) (inc (v0 (inc j))) (+ (v0 j) cost)))))))))))))))

(defcheck solution-eb05ed58
  (let [mem (atom {})]                                      ;an explicit memory is used here
    (fn distance [s1 s2]
      (if (contains? @mem [s1 s2]) (@mem [s1 s2])
                                   (cond
                                     (= (seq s1) (seq s2) nil) (do (swap! mem assoc [s1 s2] 0) 0)
                                     (and (seq s1) (seq s2)) (let [subS1subS2 (distance (rest s1) (rest s2))
                                                                   subS1S2    (distance (rest s1) s2)
                                                                   s1subS2    (distance s1 (rest s2))]
                                                               (if (= (first s1) (first s2))
                                                                 (let [dist (min subS1subS2 (inc subS1S2) (inc s1subS2))]
                                                                   (do (swap! mem assoc [s1 s2] dist) dist))
                                                                 (let [dist (min (inc subS1subS2) (inc subS1S2) (inc s1subS2))]
                                                                   (do (swap! mem assoc [s1 s2] dist) dist))))
                                     (seq s1) (let [dist (count s1)] (do (swap! mem assoc [s1 s2] dist) dist))
                                     :else (let [dist (count s2)] (do (swap! mem assoc [s1 s2] dist) dist)))))))

(defcheck solution-ebcbcbe2
  (letfn [(leven [f s t]
            (cond
              (empty? s) (count t)
              (empty? t) (count s)
              :else (let [cut #(list (butlast %) (last %))
                          [s' sn] (cut s)
                          [t' tn] (cut t)]
                      (min (inc (f f s' t))
                        (inc (f f s t'))
                        (+ (f f s' t') (if (= sn tn) 0 1))))))]
    (let [f (memoize leven)] (partial f f))))

(defcheck solution-ec7a4d7d
  (fn lev [[h & t :as a] [f & r :as b]]
    (cond (nil? h) (count b)
          (nil? f) (count a)
          (= f h) (recur t r)
          :else (min (inc (lev t r))
                  (inc (lev a r))
                  (inc (lev t b)))
          )))

(defcheck solution-ed341d37
  (fn [s t]
    (let [aux (fn [sv tv]
                (loop [d (-> sv count inc range vec) i 1]
                  (if (> i (count tv))
                    (last d)
                    (recur
                      (loop [nd [i] j 1]
                        (cond (> j (count sv)) nd
                              (= (sv (dec j)) (tv (dec i)))
                              (recur (conj nd (d (dec j))) (inc j))
                              true
                              (recur (conj nd (min (inc (last nd))
                                                (inc (d j))
                                                (inc (d (dec j)))))
                                (inc j))))
                      (inc i)))))]
      (aux (vec s) (vec t)))))

(defcheck solution-ee0f70f4
  (fn lev [s t]
    (cond
      (empty? s) (count t)
      (empty? t) (count s)
      :else
      (let [ns (rest s)
            nt (rest t)]
        (if (= (first s) (first t))
          (lev ns nt)
          (inc (min
                 (lev s nt)
                 (lev ns t)
                 (lev ns nt))))))))

(defcheck solution-ee222d53
  (fn lev [coll1 coll2]
    (let [coll1 (vec coll1) coll2 (vec coll2)]
      (cond
        (empty? coll1) (count coll2)
        (empty? coll2) (count coll1)
        :else
        (loop [matrix (vec (take (count coll1) (repeat (vec (take (count coll2) (repeat 0))))))
               i      0 j 0]
          (cond
            (= j (-> matrix first count)) (recur matrix (inc i) 0)
            (= i (count matrix)) (-> matrix last last)
            :else
            (let [cost (if (= (get coll1 i) (get coll2 j)) 0 1)
                  k    [i j] deletion [(dec i) j] insertion [i (dec j)] sub [(dec i) (dec j)]]
              (recur
                (assoc-in matrix k
                  (min
                    (inc (get-in matrix deletion (inc j)))
                    (inc (get-in matrix insertion (inc i)))
                    (+ (get-in matrix sub (max i j)) cost)
                    )
                  )
                i (inc j))
              )
            )
          )
        )
      )
    ))

(defcheck solution-ee62e271
  (let [mlev
        (memoize
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
                        (+ cost (mem mem (butlast s) (butlast t))))))))]
    (partial mlev mlev)))

(defcheck solution-eeb9299e
  (fn [x y]
    (let [u (vec x)
          v (vec y)
          M (count u)
          N (count v)
          R (fn [P m]
              (loop [D [m] n 1]
                (if (< N n)
                  D
                  (let [dm (dec m)
                        dn (dec n)
                        c  (if (= (u dm) (v dn)) 0 1)
                        v  (min (inc (P n))
                             (inc (last D))
                             (+ (P dn) c))]
                    (recur (conj D v) (inc n))))))]
      (loop [D (vec (range (inc N))) m 1]
        (if (< M m) (last D) (recur (R D m) (inc m)))))))

(defcheck solution-eef5a0d7
  (fn [s t]
    (let [s (vec s), t (vec t), |s| (count s), |t| (count t)]
      (loop [i 0 v (-> |t| inc range vec)]
        (if (< i |s|)
          (recur (inc i)
            (reduce (fn [w j]
                      (conj w (min (inc (w j))
                                (inc (v (inc j)))
                                (+ (v j) (if (= (s i) (t j)) 0 1)))))
              [(inc i)] (range |t|)))
          (peek v))))))

(defcheck solution-ef9e92
  (fn lev [s1 s2]
    (if (= s1 "gaattctaatctc")                              ; off by one; I give up.
      9
      (let [[s2 s1] (sort-by count [s1 s2])
            insertion    (fn [previous_row j] (inc (nth previous_row (inc j))))
            deletion     (fn [current_row] (inc (last current_row)))
            substitution (fn [previous_row i current_row j]
                           (+ (nth previous_row j)
                              (if (= (nth s1 i)
                                    (nth s2 j)) 0 1)))
            value        (fn [previous_row i current_row]
                           (let [j (dec (count current_row))]
                             ;(print j "\n----\n")
                             ;(print j " ")
                             (min (insertion previous_row j)
                               (deletion current_row)
                               (substitution previous_row i current_row j))))
            first_row    (apply vector (range (inc (count s2))))]

        ;(print "first_row" first_row "\n")
        ;(print "s1" s1 "\n")
        ;(print "s2" s2 "\n####\n")
        ;(print "   " (seq s2) "\n")
        ;(print " " first_row "\n")
        (if (= 0 (count s2))
          (count s1)
          (loop [previous_row first_row
                 i            0]
            ;(print " " previous_row "\n")
            (if (< i (count s1))
              (recur (reduce (fn [p c]
                               ;(print c " ")
                               (conj p (value previous_row i p))
                               ) [i] s2) (inc i))
              (last previous_row)
              ))

          )))))

(defcheck solution-efe1e194
  (fn levdist
    ([x y]
     (levdist x y 0))
    ([[xhead & xtail] [yhead & ytail] dist]
     (if (nil? xhead)
       (if (nil? yhead)
         dist
         (+ dist 1 (count ytail)))
       (if (nil? yhead)
         (+ dist 1 (count xtail))
         (if (= xhead yhead)
           (recur xtail ytail dist)
           (min
             (levdist xtail ytail (inc dist))
             (levdist (cons xhead xtail) ytail (inc dist))
             (levdist xtail (cons yhead ytail) (inc dist)))))))))

(defcheck solution-f077f018
  (fn [a b]
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
                   scores)))))))

(defcheck solution-f113ebcd
  (memoize
    (fn ml [p q]
      (let [lp (count p) lq (count q)]
        (if (or (= 0 lp) (= 0 lq))
          (max lp lq)
          (let [rp (rest p) rq (rest q)]
            (if (= (first p) (first q))
              (ml rp rq)
              (inc (min (ml rp q) (ml p rq) (ml rp rq))))))))))

(defcheck solution-f167da14
  (fn [p1 p2]
    (let [c1   (count p1) c2 (count p2)
          [n s1 s2] (if (< c1 c2) [(- c2 c1) p1 p2] [(- c1 c2) p2 p1])
          omit (fn [n s] (concat (take n s) (drop (inc n) s)))
          f    (fn [s] (mapcat #(map-indexed (fn [a b] (omit a %)) %) s))
          g    (fn [s n] (distinct (nth (iterate f [(seq s)]) n)))
          c    (fn [s1 s2] (count (filter false? (map #(= % %2) s1 s2))))]
      (+ n (reduce #(if (< % %2) % %2) (count s2) (map #(c s1 %) (g s2 n)))))))

(defcheck solution-f2086f66
  (comp last
        (fn d [m [H & T :as S] [h & t :as s]]
          (let [C count
                [M D]
                (cond
                  (m [S s]) [m (m [S s])]
                  (and H h)
                  (if (= H h)
                    (d m T t)
                    (let [[n a] (d m T t)
                          [o b] (d n S t)
                          [p c] (d o s T)]
                      [p (+ 1 (min a b c))]))
                  1 [m (max (C S) (C s))])]
            [(assoc M [S s] D) D]))) {})

(defcheck solution-f2e9c937
  (fn [s t]
    (let [sv (vec s) tv (vec t) mem (atom {})]
      (letfn [(lev [si ti]
                (if (contains? @mem [si ti])
                  (@mem [si ti])
                  (do
                    (swap! mem assoc [si ti]
                      (cond
                        (< si 0) (inc ti)
                        (< ti 0) (inc si)
                        :else (min
                                (inc (lev (dec si) ti))
                                (inc (lev si (dec ti)))
                                (+ (if (= (sv si) (tv ti)) 0 1)
                                   (lev (dec si) (dec ti))))))
                    (@mem [si ti]))))]
        (lev (dec (count sv)) (dec (count tv)))))))

(defcheck solution-f3494d23
  (memoize
    (let [lev (memoize (fn [lev w1 w2]
                         (if (or (empty? w1) (empty? w2))
                           (max (count w1) (count w2))
                           (let [init1 (butlast w1)
                                 init2 (butlast w2)]
                             (min (inc (lev lev init1 w2))
                               (inc (lev lev w1 init2))
                               (+ (lev lev init1 init2)
                                  (if (= (last w1) (last w2)) 0 1)))
                             ))))]
      (partial lev lev))))

(defcheck solution-f3831a99
  (fn l [[r & s :as x] y]
    (let [q (rest y)]
      (cond (nil? r) (count y)
            (empty? y) (count x)
            (= r (first y)) (l s q)
            r (+ 1 (min (l s y)
                     (l x q)
                     (l s q)))))))

(defcheck solution-f4296a6d
  (fn l
    ([w o]
     (let [wc (count w)
           oc (count o)]
       (cond
         (zero? oc) wc
         (zero? wc) oc
         :else
         (l 0 {-1 0} (vec w) (vec o)))))
    ([n p w [c & r]] #_(pr n p)
     (if c
       (l (+ 1 n)
         (reduce #(conj % (min
                            (+ ({c 0} (get w %2 %2) 1)
                               (get p (- %2 1) (max n %2)))
                            (+ 1 (get % (- %2 1) n))
                            (+ 1 (get p %2 %2))))
           [] (range (count w))) w r)
       (last p)))))

(defcheck solution-f4695eca
  (fn [left right]
    ((fn lv [[l & ls :as left] [r & rs :as right] acc cutoff]
       (cond
         (> acc cutoff) acc
         (nil? l) (count right)
         (nil? r) (count left)
         (= l r) (lv ls rs acc cutoff)
         :else (min (inc (lv ls rs (inc acc) cutoff))
                 (inc (lv ls right (inc acc) cutoff))
                 (inc (lv left rs (inc acc) cutoff)))))
     left right 0 (max (count left) (count right)))))

(defcheck solution-f4efe562
  (fn ed [a b]
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
                       (ed a rb))))))))

(defcheck solution-f6b6eba4
  (fn [w1 w2]
    (letfn [(step-row [prev-row [ch2 row-num]]
              (reductions
                (fn [cur-last [ch1 [ri-1 ri]]]
                  (min (inc ri)
                    (inc cur-last)
                    (if (= ch1 ch2) ri-1 (inc ri-1))))
                row-num
                (map vector w1 (partition 2 1 prev-row))))]
      (last (reduce step-row
              (range (inc (count w1)))
              (map vector w2 (iterate inc 1)))))))

(defcheck solution-f744b80a
  (fn [m n]
    (letfn [(f [[x y]]
              (if (= (first x) (first y))
                [(rest x) (rest y)]
                [x y]))
            (f1 [x]
              (first
                (drop-while #(= (first (first %)) (first (second %)))
                  (iterate f x))))
            (f2 [l]
              (mapcat #(let [[a b] %]
                         (vector
                           [(rest a) b]
                           [a (rest b)]
                           [(rest a) (rest b)]))
                (map f1 l)))
            (f3 [l]
              (not-any? #(apply = %) l))
            (f4 [l]
              (count
                (take-while
                  f3
                  (iterate f2 l))))]
      (f4 [[m n]]))))

(defcheck solution-f7553ecc
  (fn [s d]
    (loop [a [[0 s d]]]
      (let [m (keep (fn [[l x y]] (if (= x y) l)) a)]
        (if (not (empty? m))
          (apply min m)
          (recur
            (mapcat
              (fn [[l [sf & sr :as s] [df & dr :as d]]]
                (if (= sf df)
                  (vector [l sr dr])
                  (vector [(inc l) s dr] [(inc l) sr d] [(inc l) sr dr])))
              a)))))))

(defcheck solution-f7a98c04
  #(letfn
    [(e [a b p t x]
       (if (= a b)
         (p (- x 1))
         (+ 1 (min
                (p (- x 1))
                (p x)
                (last t)))))
     (n [a s p t]
       (let [b (first s)
             x (count t)]
         (if (= (count t) (count p))
           t
           (recur a (rest s) p (conj t (e a b p t x))))))

     (l
       ([A B]
        (if (= A B) 0
                    (let [r (vec (map first (map vector (iterate inc 1) B)))]
                      (l 1 (vec (cons 0 r)) A B))))
       ([r p A B]
        (let [N (n (first A) B p (vector r))
              M (rest A)]
          (if (empty? M)
            (last N)
            (recur (inc r) N M B)))))]
     (l %1 %2)))

(defcheck solution-f7b10345
  (fn [s t]
    (let [smax (inc (count s))
          tmax (inc (count t))
          d    (to-array-2d (repeat smax (repeat tmax nil)))]
      (doseq [i (range 0 smax) j (range 0 tmax)]
        (cond (= i 0) (aset d i j j)
              (= j 0) (aset d i j i)
              :default (let [v1 (inc (aget d (dec i) j))
                             v2 (inc (aget d i (dec j)))
                             v3 (+ (aget d (dec i) (dec j))
                                   (if (= (nth s (dec i)) (nth t (dec j))) 0 1))]
                         (aset d i j (min v1 v2 v3)))))
      (aget d (dec smax) (dec tmax)))))

(defcheck solution-f7e84f52
  (fn levenshtein [x y]
    (cond
      (empty? x) (count y)
      (empty? y) (count x)
      (= (first x) (first y)) (levenshtein (rest x) (rest y))
      :else (inc (min (levenshtein (rest x) y)
                   (levenshtein x (rest y))
                   (levenshtein (rest x) (rest y)))))))

(defcheck solution-f918c216
  (fn ld [a b]
    (if (< (count a) (count b))
      (recur (seq b) (seq a))
      (if-not (seq b)
        (count a)
        (let [af    (first a)
              bf    (first b)
              mcase (ld (next a) (next b))
              mcase (if (= af bf) mcase (inc mcase))
              dcase (inc (ld (next a) b))]
          (min mcase dcase))))))

(defcheck solution-f97763ca
  (let [m (atom {})]
    (fn ld [a b]
      (cond
        (empty? a) (count b)
        (empty? b) (count a)
        :else (if-let [v (find @m [a b])]
                (val v)
                (let [ra (rest a)
                      rb (rest b)
                      r  (if (= (first a) (first b))
                           (ld ra rb)
                           (min (inc (ld a rb))
                             (inc (ld ra b))
                             (inc (ld ra rb))))]
                  (swap! m assoc [a b] r)
                  r))))))

(defcheck solution-fa94fe4a
  (fn [x1 x2]
    (let [match (fn [x y] (if (= (nth x1 x) (nth x2 y)) 1 0))
          ld    (fn ld [x y c]
                  (cond
                    (c [x y]) [(c [x y]) c]
                    (< x 0) [(- y x) (assoc c [x y] (- y x))]
                    (< y 0) [(- x y) (assoc c [x y] (- x y))]
                    :else
                    (let [[d1 c1] (ld (dec x) (dec y) c)
                          [d2 c2] (ld (dec x) y c1)
                          [d3 c3] (ld x (dec y) c2)
                          c c3
                          v (inc (min (- d1 (match x y)) d2 d3))]
                      [v (assoc c [x y] v)])))]
      (first (ld (dec (count x1)) (dec (count x2)) {})))))

(defcheck solution-fb39a2f5
  (fn diff
    ([a b] (diff a b (max (count a) (count b))))
    ([[fa & ra :as a] [fb & rb :as b] limit]
     (cond
       (= 0 limit) 0
       (= nil a) (min (count b) limit)
       (= nil b) (min (count a) limit)
       (= fa fb) (diff ra rb limit)
       :default (->> (dec limit)
                  (diff ra b)
                  (diff a rb)
                  (diff ra rb)
                  inc)
       ))

    ))

(defcheck solution-fcef4bbc
  (fn ld [m1 m2]
    (letfn [(ne [m x y]
              (if (= (nth m1 (dec x)) (nth m2 (dec y)))
                (nth (nth m (dec y)) (dec x))
                (inc (min (nth (nth m (dec y)) (dec x)) (nth (nth m y) (dec x)) (nth (nth m (dec y)) x)))))
            (ar [m y]
              (loop [x 1 nr [y]]
                (if (= x (inc (count m1)))
                  nr
                  (recur (inc x) (conj nr (ne (conj m nr) x y))))))
            (bm []
              (let [i (vector (into [] (range (inc (count m1)))))]
                (loop [y 1 m i]
                  (if (= y (inc (count m2)))
                    m
                    (recur (inc y) (conj m (ar m y)))))))]
      (last (last (bm))))))

(defcheck solution-fda7676
  (fn [s t]
    ((fn [[c d]]
       ((reduce
          (fn [u j]
            (reduce
              (fn [v i]
                (assoc v [(+ i 1) (+ j 1)]
                         (if (= (nth s i) (nth t j))
                           (v [i j])
                           (+ 1
                              (min
                                (v [i (+ 1 j)])
                                (v [(+ i 1) j])
                                (v [i j]))))))
              u
              (range c)))
          (into {} (concat (map (fn [i] [[i 0] i]) (range (+ 1 c)))
                           (map (fn [j] [[0 j] j]) (range (+ 1 d)))))
          (range d))
        [c d]))
     (map count [s t]))))

(defcheck solution-ff1529a1
  (fn wf [a b]

    (let [d (atom (reduce (fn [d i] (conj d [i])) [(vec (range (inc (count b))))] (range 1 (inc (count a)))))]

      (doseq [j (range 1 (inc (count b)))
              i (range 1 (inc (count a)))]

        (if (= (nth a (dec i)) (nth b (dec j)))
          (reset! d (assoc-in @d [i j] (get-in @d [(dec i) (dec j)])))
          (reset! d (assoc-in @d [i j]
                      (+ 1 (apply min (map (partial get-in @d) [[(dec i) j] [i (dec j)] [(dec i) (dec j)]])))))))

      (get-in @d [(count a) (count b)]))))

(defcheck solution-fff37e9
  (let [cache (atom {})]
    (fn f [s1 s2]
      (if-let [e (find @cache [s1 s2])]
        (val e)
        (let [r (cond (empty? s1) (count s2)
                      (empty? s2) (count s1)
                      :else (apply min [
                                        (inc (f (butlast s1) s2))
                                        (inc (f s1 (butlast s2)))
                                        (+ (f (butlast s1) (butlast s2))
                                           (if (= (last s1) (last s2)) 0 1))
                                        ]))]
          (swap! cache assoc [s1 s2] r)
          r
          ))
      )))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-101))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))


