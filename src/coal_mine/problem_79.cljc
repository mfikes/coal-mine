(ns coal-mine.problem-79
  (:require [coal-mine.checks :refer [defcheck-79] :rename {defcheck-79 defcheck}]
            [clojure.test]
            [clojure.set]))


(defcheck solution-1020dbcd
  (fn calc-min-path [tree]
    (let [ score  (first (first tree))
          levels (next tree) ]
      (if (nil? levels)
        score
        (+ score
           (min (calc-min-path (map rest    levels))
             (calc-min-path (map butlast levels))))))))

(defcheck solution-105f972b
  (fn min-path [orig-tri]
    (loop [tri orig-tri]
      (if (= 1 (count tri))
        (first (first tri))
        (let [num-rows (count tri)
              rev-tri (reverse tri)
              last-row (first rev-tri)
              prev-row (second rev-tri)
              prev-size (count prev-row)
              prev-mins (map
                          (fn [i]
                            (+ (nth prev-row i)
                               (min
                                 (nth last-row i)
                                 (nth last-row (inc i) ))))
                          (range prev-size))
              new-tri (reverse (conj (drop 2 rev-tri) prev-mins))]
          (recur new-tri))))))

(defcheck solution-109497ba
  (fn f [t]
    (if-let [[[u] & v] (seq t)]
      (+ u
         (min
           (f (map butlast v))
           (f (map rest v))
           ))
      0)))

(defcheck solution-10a2cc96
  (let [paths
        (fn [nrows]
          (loop [n (-  nrows 1)
                 paths [[0]]]
            (if (= 0 n) paths
                        (recur (- n 1)
                          (mapcat (fn [path]
                                    [(conj path (last path))
                                     (conj path (+ (last path) 1))]) paths)))))]
    (let [cost-of-path
          (fn [path triangle]
            (loop [[hp & tp] path
                   [ht & tt] triangle
                   cost 0]
              (let [new-cost (+ cost (nth ht hp))]
                (if (nil? tp)
                  new-cost
                  (recur tp tt new-cost)))))]
      (fn triangle-minimal-path
        [triangle]
        (apply min (map #(cost-of-path % triangle) (paths (count triangle))))))))

(defcheck solution-10ad839a
  (fn [t]
    (first (reduce
             #(map + (map (partial apply min) (partition 2 1 %)) %2)
             (reverse t)))))

(defcheck solution-10c4ad8b
  (fn [coll]
    (letfn [(sum-from-two-rows [first-row second-row]
              (loop [f first-row s (rest second-row) ans [(+ (first first-row) (first second-row))]]
                (if (empty? (rest f))
                  (conj  ans (+ (last f) (last s)))
                  (recur (rest f) (rest s) (conj ans (+ (first s) (min (first f) (second f))))))))]
      (loop [remaining (rest coll) ans (first coll)]
        (if (empty? remaining)
          (apply min ans)
          (recur (rest remaining) (sum-from-two-rows ans (first remaining))))))))

(defcheck solution-10c9d787
  (fn [tri]
    (let [path-seqs (nth (iterate #(mapcat (fn [x]
                                             [(conj x (last x))
                                              (conj x (inc (last x)))])
                                     %) [[0]])
                      (dec (count tri)))
          tri-vec (vec tri)
          val-seq (map (fn [path] (map #(get-in tri-vec [%1 %2]) (range) path))
                    path-seqs)]
      (apply min (map #(reduce + %) val-seq)))))

(defcheck solution-11018076
  (fn minimal-path [tree]
    (let [pairs-minimum-vector (fn pairs-minimum-vector [v];This is for adding the minimum of the pair to the next vector
                                 (let [h (first v)          ;ie. [3] -> [3 3], [5 7] -> [5 5 7]
                                       t (last v)]
                                   (loop [v v acc (cons h [] ) i 0]
                                     (if (= i (dec (count v)))
                                       (into [] (reverse (cons t acc)))
                                       (recur v (cons (min (nth v (inc i)) (nth v i)) acc) (inc i))))))]
      (first (sort (reduce #(mapv + (pairs-minimum-vector %1) %2) tree))))))

(defcheck solution-11b1d981
  (fn __ [x]
    (let [
          oneline (fn [b s]
                    (let [
                          bb (map list b (rest b))
                          cc (map (fn [x [y z]] (min (+ x y) (+ x z))) s bb)
                          ]
                      cc))
          revx (reverse x)
          ]
      (first (reduce oneline (first revx) (rest revx))))))

(defcheck solution-12854220
  (fn min-path [tri]
    (loop [[row & tail] (-> tri reverse next)
           parent       (last tri)]
      (if (nil? row) (reduce min parent)
                     (recur tail (map #(+ % (apply min %2)) row (partition 2 1 parent)))))))

(defcheck solution-12bc9c82
  (fn min-path* [triangle]
    (letfn [(paths* [d]
              (if (zero? d) [[0]] (mapcat extend-path* (paths* (dec d)))))
            (extend-path* [p]
              [(conj p (last p)) (conj p (inc (last p)))])
            (sum-path* [p]
              (apply + (map #(%1 %2) triangle p)))]
      (apply min (map sum-path* (paths* (count triangle)))))))

(defcheck solution-130027b5
  (fn [tri]
    (let [max-level (- (count tri) 1)]
      (letfn [(at [level idx] (nth (nth tri level) idx))
              (go [sums]
                (if (= (ffirst sums) max-level)
                  (reduce min (map last sums))
                  (go (apply concat (map (fn [[level idx sum]]
                                           (let [level' (+ 1 level)]
                                             [[level' idx (+ sum (at level' idx))]
                                              [level' (+ 1 idx) (+ sum (at level' (+ 1 idx)))]]))
                                      sums)))))]
        (go [[0 0 (at 0 0)]])))))

(defcheck solution-13678d0d
  (fn prob79s [t]
    (letfn
     [(sum-start [n s sum]
        (let [new-sum (+ sum (nth (first s) n))]
          (if (= (count s) 1) new-sum
                              (list
                                (sum-start n (rest s) new-sum)
                                (sum-start (inc n) (rest s) new-sum)))))]
      (apply min (flatten (sum-start 0 t 0))))))

(defcheck solution-13aca644
  (fn sp [s]
    (let [h (count s)
          flat (fn part-flat [x] (if (some coll? x) (mapcat part-flat x)(vector x)))
          t (fn t [h n i pa s]
              (if (= n h)
                pa
                (vector (t h (inc n) i (cons (nth (nth s n) i) pa) s)
                  (t h (inc n) (inc i) (cons (nth (nth s n) i) pa) s))))]
      (first(sort < (map #(apply + %) (flat (t h 0 0 [] s))))))))

(defcheck solution-13bc3a72
  (fn find-path
    ([tower] (find-path tower (dec (count tower)) 0 0))
    ([tower n-row row col]
     (if
      (= row n-row)
       (nth (last tower) col)
       (+ (nth (nth tower row) col)
          (min (find-path tower n-row (inc row) col)
            (find-path tower n-row (inc row) (inc col))))))))

(defcheck solution-14814787
  (fn triangle-minimal-path
    [triangle]
    (let [new-cell (fn [old-cell num]
                     (let [[path sum] old-cell]
                       [(conj path num) (+ sum num)]))
          select-cell (fn [a b]
                        (let [[_ a-sum] a
                              [_ b-sum] b]
                          (if (<= a-sum b-sum)
                            a b)))]
      (->>
        (reduce
          (fn [paths row]
            (vec
              (concat
               [(new-cell (first paths) (first row))]
               (map
                 (fn [[left right] num]
                   (new-cell (select-cell left right) num))
                 (partition 2 1 paths)
                 (subvec row 1 (dec (count row))))
               [(new-cell (peek paths) (peek row))])))
          [(new-cell [[] 0] (ffirst triangle))]
          (rest triangle))
        (map second)
        (reduce min)))))

(defcheck solution-14e7135c
  (fn get-minimal-path-sum [x]
    (if (empty? x)
      0
      (let [vl (get-minimal-path-sum (map butlast (rest x)))
            vr (get-minimal-path-sum (map rest (rest x)))]
        (+ (ffirst x) (min vl vr))))))

(defcheck solution-15051d20
  #(loop [[ft & rt] (rest %) out (first %)]
     (if (nil? ft) (apply min out)
                   (let [res (for [i (range 1 (dec (count ft)))]
                               (+ (ft i) (min (out (dec i)) (out i))))]
                     (recur rt
                       (conj
                         (vec (cons (+ (first ft) (first out)) res))
                         (+ (last ft) (last out))))))))

(defcheck solution-1505b9ea
  (fn trav [tree]
    (let [scheme (map #(range 1 (inc (count %))) tree)
          routes (reduce #(mapcat
                            (fn[e] (filter identity (map
                                                      (fn[r]
                                                        (when (or (empty? r) (or (= e (last r) ) (= (dec e) (last r)))) (conj r e)) )
                                                      %))) %2)
                   [[]] scheme)
          paths (map (fn [route] (map #(nth % (dec  %2)) tree route)) routes)
          min (apply min (map (partial apply +) paths))]
      #_(println routes)
      #_(println paths)
      min)))

(defcheck solution-1516e20f
  (fn [triangulo]
    (apply min (reduce
                 (fn [a b] (let [s (flatten (map (fn [x y] (map #(+ x %) y)) a (partition 2 1 b)))]
                             (map #(apply min %) (partition 2 (concat [(first s)] s [(last s)])))))
                 triangulo))))

(defcheck solution-157971fa
  (fn [rows]
    (apply min
      (reduce
        (fn [paths row]
          (let [min-paths (map min (cons 2147483647 paths)
                            (concat paths [2147483647]))]
            (map + min-paths row)))
        rows))))

(defcheck solution-160250f7
  (fn [t]
    (letfn [(shortest-path
              [acc idx v]
              (let [l (get acc (dec idx))
                    r (get acc idx)]
                (cond
                  (nil? l) (+ v r)
                  (nil? r) (+ v l)
                  :else (+ v (min l r)))))]
      (apply min
        (reduce (fn [acc row]
                  (vec
                    (map-indexed (partial shortest-path acc) row)))
          (first t)
          (rest t))))))

(defcheck solution-1608abc
  (fn tmp[rows]
    (letfn [
            (choices [lst]
              (loop [cur (first lst)
                     las 9999
                     a []
                     lst lst]
                (if lst
                  (recur (first (next lst)) cur (conj a [cur las]) (next lst))
                  (conj a [las]))))
            (minrows[accu rows]
              (let [ch (choices accu)
                    n (first rows)
                    a (map #(apply min (map (fn[a] (+ a %2)) %1)) ch n)]
                (if (next rows)
                  (minrows a (next rows))
                  a)))]
      (apply min (minrows (first rows) (next rows))))))

(defcheck solution-16b58a55
  (fn tmp [tri]
    (loop [t (rest tri) sum [(first (first tri))]]
      (if (seq t)
        (let [level (first t)
              calc-cost (fn [i val]
                          (+ val (cond
                                   (zero? i) (first sum)
                                   (= i (dec (count level))) (last sum)
                                   :else (min (nth sum i) (nth sum (dec i))))))]
          (recur (rest t) (map-indexed calc-cost level)))
        (apply min sum)))))

(defcheck solution-1728cb42
  (fn t [v]
    (apply min (letfn [(c [v1 v2]
                         (vec (map #(+ %3 (min %2 %1))
                                (conj v1 999)
                                (cons 999 v1)
                                v2)))]
                 (reduce c v)))))

(defcheck solution-1806e6d1
  (letfn [
          (left-subtri
            [tri]
            (map butlast (rest tri)))
          (right-subtri
            [tri]
            (map rest (rest tri)))
          (min-path
            [tri]
            (if (= 1 (count tri))
              (first (first tri))
              (+ (first (first tri)) (min (min-path (left-subtri tri)) (min-path (right-subtri tri))))))]
    min-path))

(defcheck solution-18141bce
  (fn min-sum-path [t]
    (letfn [(sub-tri [t [r i]]
              (vec (for [x (range (- (count t) r))]
                     (subvec (t (+ r x)) i (+ i x 1)))))]
      (let [t (vec t) f (get-in t [0 0])]
        (if (empty? (rest t))
          f
          (+ f (min (min-sum-path (sub-tri t [1 0]))
                 (min-sum-path (sub-tri t [1 1])))))))))

(defcheck solution-181acc31
  (fn [g]
    (apply
      min
      (reduce
        (fn [v w]
          (map
            (fn [x y z] (if (nil? y) (+ x z) (if (nil? z) (+ x y) (min (+ x y) (+ x z)))))
            w
            (concat v [nil])
            (concat [nil] v))) g))))

(defcheck solution-190d17d7
  (fn path
    ([tree] (path 0 0 tree))
    ([level node tree]
     (let [cost (get (nth tree level) node)]
       (+ cost
          (if (< level (dec (count tree)) )
            (min
              (path (inc level) node tree) ;; left
              (path (inc level) (inc node) tree)) ;; right
            0))))))

(defcheck solution-19341c67
  (fn m
    ([c t]
     (if (empty? t) 0
                    (+ (nth (nth t 0) c)
                       (min (m c (rest t))
                         (m (inc c) (rest t))))))) 0)

(defcheck solution-198f93e4
  (fn [[h & t]]
    (loop [sums h
           [n & n1 :as nodes] t]
      (if (nil? nodes)
        (apply min sums)
        (let [[x & y] (map + sums n)
              newsums (map + sums (rest n))]
          (recur (concat [x]
                         (map min y newsums)
                         [(last newsums)])
            n1))))))

(defcheck solution-1a49720f
  (fn [x]
    (first
      (reduce
        #(map + %2 (map min %1 (next %1)))
        (reverse x)))))

(defcheck solution-1ab1eab8
  ;; https://gist.github.com/adamwespiser/1685138
  (fn [col]
    (first
      (reduce #(map +
                 (map min (butlast %1) (rest %1))
                 %2)
        (reverse col)))))

(defcheck solution-1ab46e0f
  (fn myTriangleMinPath
    [triangle]
    (let [vecTriangle (into [] triangle)
          pathLength (count triangle)
          collectAllPaths (fn collectPaths [path]
                            (if (= pathLength (count path))
                              (conj [] (map-indexed #(get-in vecTriangle [%1 %2]) path))
                              (let [lastP (last path)]
                                (concat
                                 (collectPaths (conj path lastP))
                                 (collectPaths (conj path (inc lastP)))))))]
      (apply min (map #(reduce + %) (collectAllPaths [0]))))))

(defcheck solution-1c67167b
  (fn f1[x]
    (letfn [(f [x y](apply merge-with concat
                      (map #(hash-map
                              % (map (fn[z] (conj z (nth y %))) (get x %))
                              (inc %) (map (fn[z] (conj z (nth y (inc %)))) (get x %)))
                        (keys x))))
            (f2 [x](map (fn[dd] (map (fn[d] (apply + d)) dd)) x))]
      (apply min (flatten
                   (f2 (vals (reduce f {0 (vector (first x))} (rest x)))))))))

(defcheck solution-1cef24f3
  (fn min-path--reduce [tri]
    ;; We start by making sure we're given a valid triangle.
    {:pre [;; A valid triangle is nonempty.
           (seq tri),
           ;; And the rows start with 1 element and increase by 1 at each step.
           (map-indexed #(= (count %2) (inc %1)) tri)]}
    (->> tri
      (reduce (fn [acc row]
                (->> (for [i (range (count acc))]
                       (if (= i (dec (count acc)))
                         (acc i)
                         (min (acc i) (acc (inc i)))))
                  (cons (acc 0))
                  (map + row)
                  vec)))
      (apply min))))

(defcheck solution-1d02a523
  (fn f
    ([t] (f (vec t) 0 0 0))
    ([t i j s]
     (if (= (count t) i) s
                         (let [s (+ s (get-in t [i j]))
                               s1 (f t (+ i 1) j s)
                               s2 (f t (+ i 1) (+ j 1) s)]
                           (min s1 s2))))))

(defcheck solution-1d09e614
  (fn f [[[a] & b]]
    (+ a (if b (min (f (map rest b))
                 (f b)) 0))))

(defcheck solution-1dc2fd7e
  (fn [t]
    (first
      (reduce
        #(for [i (range (count %2))]
           (+
            (nth %2 i)
            (min
              (nth % i)
              (nth % (inc i)))))
        (reverse t)))))

(defcheck solution-1df86844
  (fn min-path [t]
    (letfn [(paths
              ([n] (paths n [[0]]))
              ([n p]
               (if (= n 0) p (paths (dec n) (mapcat #(vector (conj % (last %)) (conj % (inc (last %)))) p)))))
            (get-path [p] (map #(nth %1 %2) t p))]
      (apply min (map #(apply + %) (map get-path (paths (dec (count t)))))))))

(defcheck solution-1e85d09c
  (fn [triangle]
    (let
     [depth (count triangle)
      val-at (fn [x y] (nth (nth triangle y) x))
      min-path-through (fn min-path-through [x y]
                         (if (= y depth)
                           0
                           (+ (val-at x y)
                              (min (min-path-through x (inc y))
                                (min-path-through (inc x) (inc y))))))]
      (min-path-through 0 0))))

(defcheck solution-1edce3ef
  (fn min-tri-path [t]
    (first (reduce (fn [a s] (map + s (map #(apply min %) (partition 2 1 a))))
             (reverse t)))))

(defcheck solution-1ee4b25a
  (fn [x]
    (let [mem (atom {})
          path-from (fn path-from [x row pos]
                      (if-let [m (find @mem [row pos])]
                        (val m)
                        (let [down (if (= row (dec (count x)))
                                     0
                                     (min (path-from x (inc row) pos)
                                       (path-from x (inc row) (inc pos))))
                              value (-> x (nth row) (nth pos))
                              r (+ down value)]
                          (swap! mem assoc [row pos] r)
                          r)))]
      (path-from x 0 0))))

(defcheck solution-1ef46905
  (fn [t]
    (first
      (reduce
        (fn [x y]
          (->> x
            (partition 2 1)
            (map #(min (first %) (second %)))
            (map + y)))
        (reverse t)))))

(defcheck solution-1f672ebe
  (fn f [t]
    (->> t
      reverse
      (reduce
        (fn [bottom-row x]
          (map +
            (map (partial apply min) (partition 2 1 bottom-row))
            x)))
      first)))

(defcheck solution-1f7f2a6c
  (fn [s] (let [h (count s)]
            (loop [i (- h 2) r (last s)]
              (if (>= i 0)
                (recur (dec i)
                  (loop [j 0 t []] (let [row (nth s i) rl (count row)]
                                     (if (< j rl)
                                       (recur (inc j) (conj t (+ (apply min (subvec r j (+ j 2))) (nth row j))))
                                       t))))
                (first r))))))

(defcheck solution-1f8450cb
  (fn triangle-min-path [triangle]
    (let [index (fn [layer offset]
                  (+ (reduce + (range (count layer)))
                     offset))
          graph (->> triangle
                  (map
                    (fn [layer]
                      (map-indexed
                        (fn [i v]
                          [(index layer i) v])
                        layer)))
                  (partition 2 1)
                  (mapcat
                    (fn [[from to]]
                      (map vector from (partition 2 1 to))))
                  (into {}))
          min-cost (fn min-cost [[_ cost :as node]]
                     (if-not (contains? graph node)
                       cost
                       (let [[l r] (get graph node)]
                         (min (+ cost (min-cost l))
                           (+ cost (min-cost r))))))]
      (min-cost [0 (ffirst triangle)]))))

(defcheck solution-1f878e40
  (fn [tri]
    (apply min (reduce
                 (fn [acc i]
                   (let [[l & r] (map + acc (butlast i))
                         l2 (map + acc (rest i))]
                     (concat (list l) (map min r l2) (list (last l2)))))
                 tri))))

(defcheck solution-1fd6119e
  (fn [t] ((reduce (fn [a b]
                     (mapv
                       #(min (+ %2 (get a % 99)) (+ %2 (get a (inc %) 99)))
                       (range) b
                       ))
             (reverse t)) 0)))

(defcheck solution-200a026d
  (fn [triangle]
    (let [bottomRow (dec (count triangle))]
      (letfn [(getVal [row col] (nth (nth triangle row) col))
              (minPath [row col]
                (let [myVal (getVal row col)]
                  (if (= row bottomRow) myVal ; Min path cost on bottom row is the cost of the cell
                                        ; Otherwise, it is the cost of the cell, plus the min cost of cells below
                                        (+ myVal (min (minPath (inc row) col) (minPath (inc row) (inc col))) )
                                        )
                  )
                )]
        ((memoize minPath) 0 0)
        )
      )
    ))

(defcheck solution-208a3936
  (fn [triangle]
    (letfn [(all-sums [x y sum]
              (if-let [row (nth triangle x nil)]
                (into
                  (all-sums (inc x) y (+ (nth row y) sum))
                  (all-sums (inc x) (inc y) (+ (nth row y) sum)))
                #{sum}))]
      (apply min (all-sums 0 0 0)))))

(defcheck solution-20ad29c3
  (fn [t] (apply min
            (reduce #(let [c (concat [(first %1)] %1 [(last %1)])]
                       (map-indexed (fn [i x] (+ x (min (nth c i) (nth c (inc i)))))
                         %2))
              t))))

(defcheck solution-20ef6f39
  (fn tmpath [coll]
    (letfn [(squeeze [a b] (map (fn [x [y z]] (+ x (min y z)))
                             b
                             (partition 2 1 a)))]
      (first (reduce squeeze (reverse coll))))))

(defcheck solution-2100071
  (fn dp [m]
    (let [shit #(partition 2 (interleave (take (count %) (range)) %))]
      (loop [base (last m)
             rst (-> m reverse rest)]
        (if (empty? rst) (first base)
                         (recur
                           (map #(+ (last %) (min (nth base (first %)) (nth base (inc (first %))))) (shit (first rst)))
                           (rest rst)))))))

(defcheck solution-21be120
  (fn tri [coll]
    (letfn [(cal [top bellow]
              (let [ps (partition 2 1 bellow)]
                (map (fn [e pair] (+ e (apply min pair))) top ps)))]
      (first (reduce (fn [acc e] (cal e acc)) (reverse coll))))))

(defcheck solution-21fae578
  (fn [t] (first (reduce
                   (fn [s n]
                     (map + n
                       (map (partial apply min)
                         (partition 2 1 s))))
                   (reverse t)))))

(defcheck solution-22316749
  (fn [triangle]
    (letfn [(walk
              [g i j]
              (if (= (inc i) (count g))
                ((g i) j)
                (+ ((g i) j) (min (walk g (inc i) j)
                               (walk g (inc i) (inc j))))))]
      (walk (vec triangle) 0 0))))

(defcheck solution-2242101f
  #((fn p79
      [x y coll]
      (if (get-in coll [(inc x) y])
        (+ (get-in coll [x y])
           (min (p79 (inc x) y coll)
             (p79 (inc x) (inc y) coll)))
        (get-in coll [x y]))) 0 0 (vec %)))

(defcheck solution-22694857
  (fn trMinPath[l]
    (first (letfn [(shortenStep[base step]
                     (vec (map
                            #(+ (apply min %1) %2)
                            (partition 2 1 base)
                            step)))]
             (reduce
               shortenStep
               (reverse l))))))

(defcheck solution-245973ef
  (fn trpath
    ([triangle] (trpath (vec triangle) 0))
    ([triangle n]
     (if (empty? triangle) 0
                           (+ (get-in triangle [0 n])
                              (min (trpath (vec (rest triangle)) n)
                                (trpath (vec (rest triangle)) (inc n))))))))

(defcheck solution-24663291
  (fn [t]
    (->> t
      reverse
      (reductions
        (fn [z x]
          (->> z
            (partition 2 1)
            (map #(apply min %))
            (map + x))))
      last
      first)))

(defcheck solution-249396c
  (fn min-path-sum
    [triangle]
    (letfn [(vector-sum [v1 v2]
              (map + v1 v2))]
      (loop [[x y & xs] (reverse triangle)]
        (if y
          (recur
            (cons
              (map (comp (partial apply min) vector-sum)
                (partition 2 1 x) (map (partial repeat 2) y))
              xs))
          (first x))))))

(defcheck solution-24b36c77
  (fn [tri]
    (letfn [(all-tri-paths [tri-height]
              (cond
                (= 0 tri-height) []
                (= 1 tri-height) [[0]]
                :else (let [a (all-tri-paths (dec tri-height))]
                        (apply concat (for [e a]
                                        (let [l (last e)]
                                          [(conj e l) (conj e (inc l))]))))))
            (tri-path-cost [tri path] (apply + (map nth tri path)))]
      (let [paths (all-tri-paths (count tri))]
        (apply min (map #(tri-path-cost tri %) paths))))
    ))

(defcheck solution-252ea7da
  (fn [vs]
    (letfn [(sum [vs pos]
              (if-let [v (first vs)]
                (+ (v pos) (min (sum (rest vs) pos)
                             (sum (rest vs) (inc pos))))
                0))]
      (sum vs 0))))

(defcheck solution-255c2b6c
  (fn [t]
    (let [
          create-paths (fn [t]
                         (let
                          [levels      (dec (count t))
                           total-paths (Math/round (Math/pow 2 levels))
                           add-paths   (fn [ps l]
                                         (let [r (quot l 2)]
                                           (conj
                                             ps
                                             (apply
                                               vector
                                               (take
                                                 total-paths
                                                 (flatten
                                                   (repeat
                                                     (quot total-paths l)
                                                     (concat (repeat r :l)
                                                             (repeat r :r))))
                                                 )))))
                           ]
                           (loop [ps []
                                  l  total-paths]
                             (if (= l 1)
                               (map (partial apply vector)
                                 (partition levels
                                   (apply interleave ps)))
                               (recur (add-paths ps l) (quot l 2))))))
          apply-path (fn [[th & tt] p]
                       (let [apply-shift (fn [r n] (first (take 1 (drop n r))))
                             shift-level (fn [n m] (if (= m :l) n (inc n)))]
                         (loop [v th
                                n 0
                                [rh & rt] tt
                                [ph & pt] p]
                           (let [s (shift-level n ph)
                                 r (apply-shift rh s)
                                 nv (conj v r)]
                             (if (empty? pt) nv (recur nv s rt pt))))))
          path-to-map (fn [t p]
                        (let [v (apply-path t p) s (reduce + v)]
                          {:path v :sum  s}))
          apply-all-paths (fn [t ps]
                            (sort-by :sum (map (partial path-to-map t) ps)))
          shortest-path   (fn [t ps] (:path (first (apply-all-paths t ps))))
          ps (create-paths t)
          ]
      (reduce + (shortest-path t ps))
      )))

(defcheck solution-25701808
  (fn [s] (apply min (reduce (fn [P p] (let [L (map + p (cons (first P) P)) R (map + p (conj P (last P)))] (mapv min L R))) s))))

(defcheck solution-257cc8f6
  (fn f
    ([g] (f 0 g))
    ([i [h & t]]
     (+ (h i)
        (if t
          (min (f i t) (f (+ 1 i) t))
          0)))))

(defcheck solution-25eae788
  (fn __ [c]
    (let [fx (fn [n]
               (cond
                 (= 4 n) '([0 0 0 0] [0 0 0 1] [0 0 1 1] [0 0 1 2] [0 1 1 1] [0 1 1 2] [0 1 2 2] [0 1 2 3])
                 (= 6 n) '([0 0 0 0 0 0] [0 0 0 0 0 1] [0 0 0 0 1 1] [0 0 0 0 1 2] [0 0 0 1 1 1] [0 0 0 1 1 2] [0 0 0 1 2 2] [0 0 0 1 2 3] [0 0 1 1 1 1] [0 0 1 1 1 2] [0 0 1 1 2 2] [0 0 1 1 2 3] [0 0 1 2 2 2] [0 0 1 2 2 3] [0 0 1 2 3 3] [0 0 1 2 3 4] [0 1 1 1 1 1] [0 1 1 1 1 2] [0 1 1 1 2 2] [0 1 1 1 2 3] [0 1 1 2 2 2] [0 1 1 2 2 3] [0 1 1 2 3 3] [0 1 1 2 3 4] [0 1 2 2 2 2] [0 1 2 2 2 3] [0 1 2 2 3 3] [0 1 2 2 3 4] [0 1 2 3 3 3] [0 1 2 3 3 4] [0 1 2 3 4 4] [0 1 2 3 4 5])
                 )) ]
      (apply min (map #(reduce + %) (map (fn [z] (map #(nth (nth c %2) %) z (range)))
                                      (fx (count c))))))))

(defcheck solution-25fc6f83
  (letfn ((left-tri [triangles]
            (map butlast triangles))
          (right-tri [triangles]
            (map next triangles))
          (min-path [[[x] & triangles]]
            (if (nil? x) 0
                         (+ x (min (min-path (left-tri triangles))
                                (min-path (right-tri triangles)))))))
    min-path))

(defcheck solution-262d2021
  (fn [triangle]
    (first (reduce (fn [lower-row higher-row]
                     (map
                       (fn [i]
                         (let [higher-item (nth higher-row i)
                               lower-item1 (nth lower-row i)
                               lower-item2 (nth lower-row (+ i 1))]
                           (if (< lower-item1 lower-item2)
                             (+ lower-item1 higher-item)
                             (+ lower-item2 higher-item))))
                       (range (count higher-row))))
             (reverse triangle)))))

(defcheck solution-26a91a97
  (fn [tree]
    (letfn [(update-length
              [prev next]
              (if (empty? next)
                '()
                (cons (min (+ (first next) (first prev)) (+ (first next) (second prev))) (update-length (rest prev) (rest next)))
                )
              )
            (update-all [prev next]
              (concat [(+ (first prev) (first next))] (if (> (count prev) 1) (update-length prev (subvec next 1 (dec (count next)))) []) [(+ (last prev) (last next))])


              )

            ]
      (apply min (reduce update-all tree))
      )
    ))

(defcheck solution-26f5f07f
  (fn [tree]
    (letfn [(paths [c]
              (if (= (count c) 1)
                [(first c) (first c)]
                (let [x (+ 1 (first c)) y (+ 1 (last c))]
                  (conj (vec (cons x (flatten (map (fn [x] [x x]) c)))) y))))
            (shortest-paths [c]
              (if (= 2 (count c))
                c
                (map #(min (first %) (last %)) (partition 2 c))))
            (merge-level [up-level this-level]
              (map + (shortest-paths (paths up-level)) this-level))]
      (apply min (reduce merge-level tree)))))

(defcheck solution-27243fc6
  (fn [l]
    ((fn f [p [h & m]]
       (if h
         (+ (h p)
            (apply min (map #(f % m) [p (inc p)]))) 0)) 0 l)))

(defcheck solution-2732d422
  (fn [tri]
    (apply min (reduce (fn [sum row]
                         (map (fn [idx n]
                                (cond
                                  (= idx 0) (+ (first sum) n)
                                  (= idx (- (count row) 1)) (+ (last sum) n)
                                  :else (+ n (min (nth sum (- idx 1)) (nth sum idx)))))
                           (range (count row)) row)) tri))))

(defcheck solution-2783931
  (fn [x] (first (map + (first x)
                   (reduce
                     (fn [cur bot]
                       (->> cur
                         (map + bot)
                         (partition 2 1)
                         (map #(min (first %) (last %))))
                       )
                     (repeat (count x) 0)
                     (reverse (rest x)))))))

(defcheck solution-282faa0
  (fn a
    [t]
    (let [f (first (first t))]
      (if (= 1 (count t))
        f
        (min (+ f (a (map butlast (rest t))))
          (+ f (a (map (partial drop 1) (rest t)))))))))

(defcheck solution-287035c6
  ;; Realization
  ;; Bottom-up is more tractable
  ;; The bottom row paired are the results of the choice from above
  ;; Pick the smallest of each pair and sum with the previous row
  ;; Then continue


  ;; Work through the tree from bottom up (reverse the tree)
  ;; pair-row partitions the row into pairs [5 1 4] => ((5 1) (1 4))
  ;; min-pair-row reduces the paired row to smallest values for each pair
  ;; add the min-paired-row to the previous
  ;; continue throught the rest of the reversed tree

  (fn prob79
    [t]
    (letfn [(pair-row
              [row]
              (partition 2 1 row))
            (min-pair-row
              [paired-row]
              (map #(reduce min %) paired-row))
            (reduce-rows-min
              [row1 row2]
              (map + (min-pair-row (pair-row row1)) row2))]
      (first (reduce reduce-rows-min (reverse t))))))

(defcheck solution-29a064c
  (fn tmp [[[x] & xss]]
    (if-not x 0
              (+ x
                 (min (tmp xss)
                   (tmp (map rest xss)))))))

(defcheck solution-2a25669b
  #(apply min (reduce (fn f [r t]
                        (map + t (map min (cons (first r) r) (concat r (list (last r)))))) %)))

(defcheck solution-2ace83f4
  #((memoize (fn tri-min-path [[curr-row & more :as list-of-vec] index]
               (if (empty? list-of-vec)
                 0
                 (min
                   (+ (get curr-row index) (tri-min-path more index))
                   (if-let [right-val (get curr-row (inc index))]
                     (+ right-val (tri-min-path more (inc index)))
                     2147483647)))))
    %1 0))

(defcheck solution-2af8a7d0
  (fn [tri]
    (first
      (reduce
        (fn [sum row]
          (map-indexed
            (fn [i v]
              (+ v (min (nth sum i) (nth sum (inc i)))))
            row))
        (reverse tri)))))

(defcheck solution-2b1785f6
  (fn f [[[a] & b]]
    (+ a (if b (min (f (map rest b))
                 (f (map butlast b))) 0))))

(defcheck solution-2b98d63a
  (fn triangle-min-path [triangle]
    (letfn [(right-triangle [triangle] (map rest      (rest triangle)))
            (left-triangle  [triangle] (map drop-last (rest triangle)))]
      (let [top (first (first triangle))]
        (if (= (count triangle) 1)
          top
          (let [left-path  (triangle-min-path (left-triangle  triangle))
                right-path (triangle-min-path (right-triangle triangle))]
            (+ top (min left-path right-path))))))))

(defcheck solution-2bba1d29
  (fn [coll]
    (->>  (range 1 (count coll))
      (reduce
        (fn [ps xi]
          (letfn [(nn [p]
                    (let [[x y] (last p)]
                      (->> [0 1]
                        (map #(vector (inc x) (+ y %)))
                        (filter #(<= 0 (second %) (first %)))
                        (map #(conj p %)))))]
            (mapcat nn ps)))
        [[[0 0]]])
      (map (fn [p] (map (fn [[x y]] (nth (nth coll x) y)) p)))
      (map #(reduce + %))
      (sort)
      (first))))

(defcheck solution-2beb7931
  (fn [t] (first (reduce #(map + (map (partial apply min) (partition 2 1 %)) %2) (reverse t)))))

(defcheck solution-2c201016
  (fn min-path [t]
    (if (seq (rest t))
      (apply min (map #(+ (ffirst t) (min-path (map % (rest t))))
                   [rest butlast]))
      (ffirst t))))

(defcheck solution-2c888086
  (fn minpath ([t] (minpath t 0 0))
    ([t i sum] (if (empty? t) sum (let [[c & r] t, sum (+ sum (c i))] (min (minpath r i sum) (minpath r (inc i) sum)))))
    ))

(defcheck solution-2ce516d3
  (fn tmp [t]
    (letfn [(hop [v1 v2]
              (for [i (range (count v1))]
                (map #(+ (nth v1 i) %) [(v2 i) (v2 (inc i))])))
            (trim [v]
              (reverse (reduce (fn [l p] (conj (conj (rest l) (min (first p) (first l))) (second p)))
                         (list (first (first v)))
                         v)))]
      (apply min (reduce (comp trim hop) t)))))

(defcheck solution-2d657df3
  (fn [triangle]
    ((fn tri-walk [next-rows current-pos path-weight]
       (if (empty? next-rows)
         path-weight
         (apply
           min
           (for [increment [0 1]]
             (tri-walk (rest next-rows)
               (+ increment current-pos)
               (+ path-weight
                  ((first next-rows) (+ increment current-pos))))))
         )
       )
     (rest triangle) 0 (get (first triangle) 0))
    ))

(defcheck solution-2db9233c
  #(last
     (reduce (fn [c l]
               (map + (map min c (next c)) l))
       (reverse %))))

(defcheck solution-2e01e941
  (fn triangle-min[graph]
    (letfn [(calc-next-layer-len [curr-min-path-len next-layer]
              (let [layer-size (count next-layer) prev-layer-size (count curr-min-path-len)]
                (into [] (map
                           (fn [idx]
                             (+ (nth next-layer idx)
                                (if (> idx 0)
                                  (if (< idx prev-layer-size)
                                    (min (nth curr-min-path-len (dec idx))
                                      (nth curr-min-path-len idx)
                                      )
                                    ; at the end
                                    (nth curr-min-path-len (dec idx))
                                    )
                                  (nth curr-min-path-len 0)
                                  )
                                )
                             )
                           (range layer-size)
                           )
                  )
                )
              )]
      (apply min (reduce calc-next-layer-len graph))
      )
    ))

(defcheck solution-2e2e42de
  (letfn [
          (next-steps [paths]
            (apply concat
              (for [path paths, :let [last-step (last path)]]
                [(conj path last-step) (conj path (inc last-step))])))
          (all-paths [length]
            (nth (iterate next-steps [[0]]) length))
          (values-along-path [triangle path]
            (map nth triangle path))]

    (fn [triangle]
      (let [paths         (all-paths (count triangle))
            values        (map (partial values-along-path triangle) paths)
            summed-values (map (partial apply +) values)]
        (apply min summed-values)))))

(defcheck solution-2e541c9b
  (fn [t]
    (letfn [(tmp-node [v i c w]
              {
               :v v
               :l (if (empty? c)
                    nil
                    (tmp-node (get (first c) i) i (rest c) (+ v w)))
               :r (if (empty? c)
                    nil
                    (tmp-node (get (first c) (inc i)) (inc i) (rest c) (+ v w)))
               :w (+ v w)
               })
            (build-tmp-tree [t]
              (tmp-node (ffirst t) 0 (rest t) 0))
            (min-path [t]
              (if (nil? (:l t))
                (:w t)
                (apply min (map min-path [(:l t) (:r t)]))))]
      (min-path (build-tmp-tree t)))))

(defcheck solution-2ea3e52a
  (fn me [arg]

    (let [

          r-fn  (fn  [res s-seq]

                  (if (empty? res)
                    [
                     [ [(first s-seq)] 0]
                     [ [(first s-seq)] 1]
                     ]

                    (apply concat (for [r res]
                                    (concat [ [(concat (first r)  [(nth s-seq (second r))])  (second r) ]]

                                            [ [(concat (first r)  [(nth s-seq (second r))])  (inc (second r)) ]] )
                                    ))))

          ]

      (apply + (first (sort-by #(apply + %) (map first (reduce r-fn [] arg)))))
      )

    ))

(defcheck solution-2f4ab649
  (letfn[(minPaths [triangle, row]
           (let [currentRow (nth triangle row)]
             (if (= (inc row) (count triangle)) currentRow;last row is the always the minimal
                                                (let [nextMinimal (map #(apply min %) (partition 2 1 (minPaths triangle (inc row))))];[2 3 4 5]->((2 3) (3 4) (4 5))->(2 3 4)
                                                  (map + currentRow nextMinimal)))))];get the current minimal path
    (fn [triangle]
      (first (minPaths triangle 0)))))

(defcheck solution-2fe45b15
  (fn min-path- [triangle]
    ^{:doc "79. Write a function which calculates the sum of the minimal
  path through a triangle."}
    ;; We're going to use a dynamic programming solution, which will
    ;; reduce from the bottom of the triangle upward, replacing each row
    ;; with the sum from there on down. After the final reduction the
    ;; single remaining row will contain one element with the sum of the
    ;; min path.
    (loop [r (reverse triangle)]
      (if (= 1 (count r))
        (ffirst r)
        (recur (cons
                 (map min
                   (map + (second r) (butlast (first r)))
                   (map + (second r) (rest (first r))))
                 (drop 2 r)))))))

(defcheck solution-300ffe5c
  (fn mintri [t]
    (let [n (count t)]
      (if (= 2 n)
        (+ (first (first t)) (apply min (second t)))
        (mintri
          (conj (vec (drop-last 2 t))
            (vec (map-indexed
                   #(+ %2 (apply min (subvec (last t) %1 (+ %1 2))))
                   (nth t (- n 2))))))))))

(defcheck solution-3044a656
  ;The basic idea is to use backtracking to generate a list of paths through the triangle.
  ;If this list is found we can build the sum of each path and determine their minimum.
  ;This is pretty much boring but necessary. The interesting part of the job is finding a function to
  ;systematically produce index sets which loop through all possible paths.
  ;For a triangle like this:
  ;
  ;       [1]
  ;      [2 4]
  ;     [5 1 4]
  ;    [2 3 4 5]
  ;
  ;an index set might look like this set of column vectors:
  ;
  ; 0 0 0 0 0 0 0 0
  ; 0 0 0 0 1 1 1 1
  ; 0 0 1 1 1 1 2 2
  ; 0 1 1 2 1 2 2 3
  ;
  ;Each column represents a possible path: [0 0 0 0] is the path 1-2-5-2 in our example and [0 1 2 2] corresponds to 1-4-4-4.
  ;The n-th element in each column indicates the position of the value of the n-th line of the triangle.
  ;
  ;For another example
  ;
  ;       [3]
  ;      [2 4]
  ;     [1 9 3]
  ;    [9 9 2 4]
  ;   [4 6 6 7 8]
  ;  [5 7 3 5 1 4]
  ;
  ;the index matrix would be;
  ;
  ; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  ; 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  ; 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
  ; 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3
  ; 0 0 1 1 1 1 2 2 1 1 2 2 2 2 3 3 1 1 2 2 2 2 3 3 2 2 3 3 3 3 4 4
  ; 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
  ;
  ;
  ;The matrices display iteresting self-similar structures we can use to create functions which create index matrices.
  ;Let's study some cases:
  ;
  ;The tree of height 0 is just the 0 and its (one and only) possible path is 0
  ;For N = 1 the tree
  ;
  ;          [x]
  ;         [y z]
  ;
  ;has two possible paths: 0-1 and 0-2. Its matrix is:
  ;
  ;  0 0
  ;  0 1
  ;
  ;For N = 2 (the three-level tree) the path matrix is
  ;
  ;    0 0 0 0
  ;    0 0 1 1
  ;    0 1 1 2
  ;
  ;The symmetry and the self-similarity of the tree and its subtrees of lower height is reflected in the symmetry of the corresponding index matrices:
  ;
  ; 1. Let N be the height of a tree. For every line of length M and m <= M/2  elem[m] = N - elem[M-m] (Symmetry)
  ; 2. The submatrices in the lowermost left corner of each matrix are the index matrices of all the trees with height < N (self-similarity I)
  ; 3. Let l(m) = [0 x y z ... ] be the m-th line vector of the matrix => l(m-1) = [0 0 x x y y z z ... ] (self-similarity II)
  ; 4. The index matrix for a tree of height N has 2**N (i.e. power to) columns each containing (N + 1) elements
  ; 5. Take the m-th lower-left submatrix. The submatrix of same rank to the right is the original submatrix with all elements incremented
  ; 6. The first line of each index matrix contains only 0's
  ;
  ;But how can we create these matrices which express such beautiful fractal patterns if you look at it long enough?
  ;We might use some of the above rules to build up the matrices for the trees of height 0, 1, 2, N recursively starting with the 0 in the lowermost
  ;left corner:
  ;
  ; Given the (N-1)-th lowermost-left submatrix. The 0-th matrix is just [0] (rule 2)
  ; append the same matrix with all elements incremented to its right hand side (rule 5)
  ; fill the first line with 0's (rule 6)
  ;
  ;So we need one recursion over all submatrices and a function to right-append a matrix to a matrix with all elements incremented and to initialize the first line to 0.
  ;We implement our matrix M as a vector of column vectors.
  ;
  ;
  ;(defn next-index-matrix [M]
  ;"create the index matrix for a tree of height N from a matrix for a height (N-1) tree"
  ;  (into []
  ;    (concat
  ;      (for [v M] (into [] (concat [0] v)))
  ;      (for [v M] (into [] (concat [0] (map inc v))))
  ;    )
  ;  )
  ;)
  ;
  ;(defn index-matrix[n M]
  ;"create recursively the index matrix for a tree of height n"
  ;  (if (> n 0)
  ;    (index-matrix (dec n) (next-index-matrix M))
  ;    M
  ;  )
  ;)
  ;
  ;for clarification sake: (nth v n) is the nth index of the index vector yielding the position of the element in the n-th vector of the tree-list
  ;
  ;(defn path-sum[v coll]
  ;"use the index vector v to access the corresponding values of the tree vectolr element and sum them up"
  ;  (reduce + (for [n (range (count coll))] (nth (nth coll n) (nth v n))))
  ;)
  ;
  ;#(reduce min (map (fn[v] (path-sum v %)) (index-matrix (dec(count %)) [[0]])))
  ;
  ;now the same compressed to its maximum uglyness to squeeze it into the above test case pattern:
  #(reduce min (map (fn[v] ((fn[v coll](reduce + (for [n (range (count coll))] (nth (nth coll n) (nth v n))))) v %)) ((fn index-matrix[n M](if (> n 0)(index-matrix (dec n) ((fn [M](into [](concat(for [v M] (into [] (concat [0] v)))(for [v M] (into [] (concat [0] (map inc v))))))) M)) M))
                                                                                                                      (dec(count %)) [[0]]))))

(defcheck solution-31673756
  (fn [triangle]
    (let [paths-indexes (loop [n 2 p [[0]]]
                          (if (< (count triangle) n)
                            p
                            (recur (inc n)
                              (mapcat #(map (partial conj %) [(last %) (inc (last %))]) p)))),
          paths (map #(map nth triangle %) paths-indexes)]
      (apply min (map (partial apply +) paths)))))

(defcheck solution-31762232
  (fn [tr]
    (let [
          perm (fn [& cs]
                 (reduce #(for [v %1 i %2] (conj v i)) [[]] cs))
          ways (fn [size]
                 (apply perm (conj (repeat (- size 1) [0 1]) [0])))
          paths (fn [tr]
                  (map #(reductions + %) (ways (count tr))))
          sumtr (fn [c]
                  (reduce + (map #(apply nth %) c)))
          nums (fn [tr]
                 (map sumtr
                   (map #(partition 2 (interleave tr %)) (paths tr))))
          ]
      (apply min (nums tr))
      )))

(defcheck solution-3176e17f
  (fn m
    ([l]
     (m l 0))
    ([l n]
     (if (empty? l)
       0
       (min (+ (nth (first l) n)
               (m (rest l) n))
         (+ (nth (first l) n)
            (m (rest l) (inc n))))))))

(defcheck solution-31f50964
  (fn tmp [triangle]
    (let [triangle (vec triangle) height (dec (count triangle))]
      (loop [dist {[0 0] (get-in triangle [0 0])} visited #{}]
        (let [tbd (clojure.set/difference (set (keys dist)) visited)]
          (if-let [c (and (not (empty? tbd)) (apply min-key dist tbd))]
            (if (= height (first c))
              (recur dist (conj visited c))
              (recur (reduce (fn [dist n] (assoc dist n (min (get dist n ##Inf) (+ (dist c) (get-in triangle n)))))
                       dist [[(inc (first c)) (second c)] [(inc (first c)) (inc (second c))]])
                (conj visited c)))
            (dist (apply min-key dist (filter #(= height (first %)) (keys dist))))))))))

(defcheck solution-32ad4774
  (fn minPath [t]
    (let [m  #(map min % (rest %))]
      (first (reduce #(map + (m %1) %2) (reverse t))))))

(defcheck solution-32cfadb9
  (fn f [t]
    (apply min
      (reduce (fn [s r] (->> (concat (list (first s)) s (list (last s)))
                          (partition 2 1)
                          (map (fn [[x y]] (min x y)))
                          (map + r))) t))))

(defcheck solution-3406c65b
  (fn [triangle]
    (letfn [(make-graph
              ([t] (make-graph t 0))
              ([remaining-triangle position]
               (if (empty? remaining-triangle)
                 [0]
                 [(nth (first remaining-triangle) position)
                  (make-graph (rest remaining-triangle) position)
                  (make-graph (rest remaining-triangle) (inc position))])))
            (min-or-0 [& numbers] (if (empty? numbers) 0 (apply min numbers)))
            (shortest-distance [graph]
              (+ (first graph)
                 (apply min-or-0 (map shortest-distance (rest graph)))))]
      (shortest-distance (make-graph triangle)))))

(defcheck solution-348d0480
  (fn [c]
    (let [f #(map + %2 (map (partial apply min) (partition 2 1 %1)))]
      (->> c reverse (reduce f) first))))

(defcheck solution-348e1e76
  (fn [t] (loop [T [[0 0 (ffirst t) 1]] D []]
            (let [[y x s d :as f] (first T)
                  r (rest T)
                  t (vec t)
                  c (count t)]
              (cond (not f) (apply min (map #(% 2) D))
                    (= d c) (recur r (conj D f))
                    (< c y) (recur r D)
                    :else   (recur (concat r
                                           (map (fn [k] [(inc y) k (+ s (get-in t [(inc y) k])) (inc d)])
                                             [x (inc x)]))
                              D))))))

(defcheck solution-35476edf
  (fn minimum-all
    ([triangle]
     (apply min (minimum-all triangle 0 0)))
    ([triangle index sum]
     (if (= 0 (count triangle))
       [sum](if (= 1 (count (first triangle)))
              (minimum-all (rest triangle) 0 (first (first triangle)))
              (concat (minimum-all (rest triangle) index (+ sum (get (first triangle) index))) (minimum-all (rest triangle) (inc index) (+ sum (get (first triangle) (inc index))))))))))

(defcheck solution-3677cd1d
  (fn min-path [xss]
    (reduce min
      (apply (fn min-paths
               ([xs] xs)
               ([xs ys]
                (map (fn [y i]
                       (+ y (min (nth xs (max 0 (dec i)))
                              (nth xs (min (dec (count xs)) i)))))
                  ys (range)))
               ([xs ys & more]
                (apply min-paths (min-paths xs ys) more)))
        xss))))

(defcheck solution-36e30d83
  (fn [tri]
    (letfn [(cmb [r1 r2]
              (let [lrg (+ 1 (apply max (concat r1 r2)))
                    lt (map + (concat r1 [lrg]) r2)
                    rt (map + (concat [lrg] r1) r2)]
                (map min lt rt)))]
      (apply min (reduce cmb tri)))))

(defcheck solution-37259a8d
  (fn [t]
    (first
      (reduce
        #(loop [[af & [as & _ :as ar]] % [bf & br] %2 r []]
           (if (nil? bf) r (recur ar br (conj r (min (+ af bf) (+ as bf))))))
        (reverse t)))))

(defcheck solution-37b6f4af
  (fn [s]
    (apply min
      (reduce #(map + %2
                 (map min (concat % [99]) (concat [99] %)))
        s))))

(defcheck solution-37b8f28
  (fn [tri]
    (letfn [(weight-at [y x]
              (nth (nth tri y) x))
            (descendantz [[w y x]]
              (if (= (inc y) (count tri))
                []
                [[(+ w (weight-at (inc y) x))
                  (inc y)
                  x]
                 [(+ w (weight-at (inc y) (inc x)))
                  (inc y)
                  (inc x)]]))
            (min-path-weight [todo-posz min-found]
              (if (empty? todo-posz)
                min-found
                (let [[[startw starty startx] & other-todoz] todo-posz
                      descz (descendantz [startw starty startx])]
                  (if (not-empty descz)
                    (recur (concat other-todoz descz) min-found)
                    (recur other-todoz (if min-found
                                         (min min-found startw)
                                         startw))))))]
      (min-path-weight [[(weight-at 0 0) 0 0]] nil))))

(defcheck solution-37f6071
  (fn [l]
    (apply min
      (reduce #(vec (for [i (range (count %2))]
                      (cond (= i 0) (+ (% 0) (%2 0))
                            (= i (dec (count %2))) (+ (% (dec i)) (%2 i))
                            :else (+ (%2 i) (min (% (dec i)) (% i))))))
        (first l)
        (rest l)))))

(defcheck solution-38371f79
  (fn [rows]
    (let [fcosts
          (fn [prevcosts row]
            (map
              (fn [a [l r]]
                (+ a (min l r))
                )
              row
              (map vector prevcosts (next prevcosts))
              )
            )]
      (first (reduce fcosts (reverse rows))))))

(defcheck solution-384da00d
  (fn [ts]
    (let [vs (apply vector ts)]
      (letfn
       [(create-ss [n]
          (reduce
            (fn [vs _]
              (reduce
                (fn [acc v] (let [i (last v) j (inc i)] (conj acc (conj v i) (conj v j)))) [] vs))
            [[0]] (range 1 n)))
        (evaluate-s [t n s]
          (reduce
            (fn [acc i] (let [j (get s i)] (+ acc (get (get t i) j))))
            0 (range n)))]
        (let [n (count vs) ss (create-ss n)]
          (reduce min (map (partial evaluate-s vs n) ss)))))))

(defcheck solution-393a1b3
  (fn [tree]
    (letfn [(add-path-steps [t path this-position]
              (if (seq t)
                (let [[this-level & rest-levels] t]
                  (concat
                   (add-path-steps rest-levels (conj path (nth this-level this-position)) this-position)
                   (add-path-steps rest-levels (conj path (nth this-level (inc this-position))) (inc this-position))))
                [path]))]
      (->> (add-path-steps (rest tree) (first tree) 0)
        (map (partial apply +))
        (sort)
        (first)))))

(defcheck solution-393b9a78
  (fn [li]
    (letfn [(f [as vs]
              (letfn [(get-a [i] (get as i (/ 1.0 0)))]
                (vec (map #(+ %2 (min (get-a (dec %1)) (get-a %1))) (range) vs))))]
      (apply min (reduce f li)))))

(defcheck solution-395460fb
  #(case (count (flatten %))
     10 7
     20))

(defcheck solution-3969b637
  (fn triangle-min-path [t]
    (letfn [(find-min-path [root subtree pos]
              (if (empty? subtree)
                (list root (list root))
                (let [lhs
                      (find-min-path (nth (first subtree) pos) (rest subtree) pos)
                      rhs
                      (find-min-path (nth (first subtree) (inc pos)) (rest subtree) (inc pos))]
                  (if (< (first lhs) (first rhs))
                    (list (+ root (first lhs)) (conj (fnext lhs) root))
                    (list (+ root (first rhs)) (conj (fnext rhs) root))))))]
      (first (find-min-path (ffirst t) (rest t) 0)))))

(defcheck solution-3a5881dd
  (fn [t]
    (first (reduce (fn [r2 r1] (map + (map #(apply min %) (partition 2 1 r2)) r1))
             (reverse t)))))

(defcheck solution-3a5bd71b
  (fn q79 [coll]
    (letfn [
            (small [coll]
              (if (= 1 (count coll))
                (first coll)
                (map #(apply min %) (partition 2 1 coll))))]
      (reduce
        #(small (map + % %2))
        (repeat 0)
        (reverse coll)))))

(defcheck solution-3a923592
  (fn [l]
    (apply min-key identity
      (reduce (fn [x1 x2]
                (cond
                  (empty? x1) x2
                  :else (map-indexed
                          (fn [i l]
                            (+ l
                               (cond
                                 (= i 0) (nth x1 0)
                                 (= i (count x1)) (nth x1 (dec i))
                                 :else (min-key identity (nth x1 (dec i)) (nth x1 i))
                                 ))
                            ) x2))
                ) [] l))))

(defcheck solution-3b558b08
  (fn [t]
    (letfn [(min-sum [parent kids] (+ parent (apply min kids)))
            (replace-bottom [v b]
              (let [c (count v)]
                (conj (subvec v 0 (- c 2)) b)))
            (solve-bottom [coll]
              (if (= 1 (count coll))
                (ffirst coll)
                (let [[top bottom] (take-last 2 coll)
                      new-bottom (map (partial apply min-sum)
                                   (map vector top (partition 2 1 bottom)))]
                  (recur (replace-bottom coll new-bottom)))))]
      (solve-bottom (vec t)))))

(defcheck solution-3bc51f71
  (fn calc-tri [triangle]
    ((fn calc-sub [[head & tail] head-index]
       ;(println "head" head "tail" tail "head-index" head-index "acc" acc)
       (if (empty? tail)
         (get head head-index)
         (let [left (calc-sub tail head-index)
               right (calc-sub tail (inc head-index))]
           (+
            (get head head-index)
            (if (<= left right) left right)))))
     triangle 0)))

(defcheck solution-3bc84268
  (fn [s](last (reduce #(map + (map min (rest %) (butlast %)) %2) (reverse s)))))

(defcheck solution-3be18259
  (fn[xs]
    (if (= 1 (count xs))
      ((xs 0) 0)
      (let [a (last (butlast xs)) b (last xs)]
        (recur (conj (vec (butlast (butlast xs)))
                 (vec (map #(min (+ (a %) (b %)) (+ (a %) (b (inc %)))) (range (count a))))
                 ))))))

(defcheck solution-3c199bc4
  (fn [triangle]
    (letfn [(cost [triangle row col]
              (let [x (get (get triangle row) col)]
                (if (= row (dec (count triangle)))
                  x
                  (+ x (min (cost triangle (inc row) col)
                         (cost triangle (inc row) (inc col)))))))]
      (cost (vec triangle) 0 0))))

(defcheck solution-3cd84841
  (fn foo [tree]
    (let [tree (reverse tree)
          join (fn [coll1 coll2]
                 (let [coll1 (partition 2 1 coll1)
                       coll3 (interleave coll1 coll2)
                       coll4 (partition 2 coll3)]
                   (map (fn [[[x y] z]] (min (+ z x) (+ z y))) coll4)))
          min-path (fn [tree]
                     (first (reduce join tree)))]
      (min-path tree))))

(defcheck solution-3db337b6
  (fn [cost-rows]
    (let [next-row (fn [row cost-row] (map + (concat (cons (first row) (map min (rest row) row)) (list (last row))) cost-row))]
      (apply min (reduce #(next-row %1 %2) (first cost-rows) (next cost-rows))))))

(defcheck solution-3e04575c
  (fn [t]
    (letfn [(next-paths [ps]
              (mapcat (fn [p]
                        (let [l (last p)]
                          [(conj p l)
                           (conj p (inc l))]))
                ps))

            (paths [n]
              (->> [[0]]
                (iterate next-paths)
                (take n)
                last))

            (sum-path [t p]
              (apply + (map nth t p)))

            (min-sum [t ps]
              (apply min (map #(sum-path t %) ps)))]
      (min-sum t (paths (count t))))))

(defcheck solution-3e41404a
  ; (defn min-path [rows]
  ;   (apply min
  ;     (reduce
  ;       (fn [s r]
  ;         (first (reduce
  ;           (fn [[r' i] e]
  ;             (let [left  (get s (dec i) 2147483647)
  ;                   right (get s i       2147483647)]
  ;               [(conj r' (min (+ e left) (+ e right)))
  ;                (inc i)]))
  ;           [[] 0] r)))
  ;       rows)))

  (fn min-path [[[root] & more]]
    (if more
      (+ root (min (min-path (map butlast more))
                (min-path (map rest more))))
      root)))

(defcheck solution-3e9bd384
  (fn [triangle]
    (letfn [(dist-map [prev-dist-map row]
              (loop [k 1 m {0 (map #(+ (first row) %) (prev-dist-map 0))}]
                (if (< k (dec (count row)))
                  (let [n (nth row k)
                        d1 (map #(+ n %) (prev-dist-map (dec k)))
                        d2 (map #(+ n %) (prev-dist-map k))]
                    (recur (inc k) (assoc m k (concat d1 d2))))
                  (assoc m k (map #(+ (nth row k) %) (prev-dist-map (dec k)))))))]
      (apply min (flatten (vals (reduce dist-map {0 (first triangle)} (rest triangle))))))))

(defcheck solution-3eea6994
  (fn [x] (letfn [(ex [m e] (map + e (map min (concat m '(100000)) (cons 1000000 m))))]
            (apply min (reduce ex (first x) (rest x))))))

(defcheck solution-3f3fef1e
  (fn [tri]
    (let [paths  (fn paths [[roots & nxt]]
                   (if (seq nxt)
                     (let [children (paths nxt)
                           grouped (partition (/ (count children) (inc (count roots)))
                                     children)]
                       (mapcat (fn [p [a b]] (for [item (concat a b)] (cons p item)))
                         roots (partition 2 1 grouped)))
                     (map list roots)))]
      (apply min (keys (group-by (partial reduce +) (paths tri)))))))

(defcheck solution-3f9bee20
  (fn [rows]
    (letfn [(reduce-row [r]
              (vec
                (for [i (range 0 (dec (count r)))]
                  (min (nth r i) (nth r (inc i))))))]
      (first (reduce #(map + (reduce-row %1) %2) (reverse rows))))))

(defcheck solution-3fc9ccc
  (letfn [(left-subtriangle [triangle]
            (map butlast (rest triangle)))
          (right-subtriangle [triangle]
            (map rest (rest triangle)))
          (triangle-cost [triangle]
            (if (empty? triangle) 0
                                  (+ (first (first triangle))
                                     (min (triangle-cost (left-subtriangle triangle))
                                       (triangle-cost (right-subtriangle triangle))))))]
    (fn [triangle] (triangle-cost triangle))))

(defcheck solution-3ff0a759
  (fn [tree]
    (let [paths-with-indices (reduce (fn [paths siblings]
                                       (for [path paths
                                             [sidx sibling] (map-indexed vector siblings)
                                             :let [pidx (first (last path))]
                                             :when (or (= sidx pidx) (= sidx (inc pidx)))]
                                         (conj path [sidx sibling])))
                               [[[0 0]]]
                               tree)
          paths (map #(map second %) paths-with-indices)
          sums (map #(reduce + 0 %) paths)]
      (apply min sums))))

(defcheck solution-4089ed89
  (fn [g]
    (letfn [(cost [path]
              (reduce + (map #(%2 %1) path g)))

            (get-moves [path]
              (if (< (count path) (count g))
                (if (empty? path)
                  [[0]]
                  [(conj path (last path))
                   (conj path (inc (last path)))])
                nil))

            (reached-end? [path]
              (nil? (get-moves path)))]

      (cost (loop [paths (into #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (get-moves nil)) path-best nil]
              (if-let [path (peek paths)]
                (recur (into (pop paths) (get-moves path))
                  (if (reached-end? path)
                    (if path-best
                      (if (< (cost path) (cost path-best))
                        path
                        path-best)
                      path)
                    path-best))
                path-best))))))

(defcheck solution-418d7615
  (fn triangle [t]
    (letfn [(neighbors [[l r]]
              [[(inc l) r] [(inc l) (inc r)]])
            (cost [t path]
              (apply + (map #(get-in t %) path)))
            (genPaths [n]
              (if (= n 0)
                [[[0 0]]]
                (reduce (fn [result elem]
                          (let [neigh (neighbors (last elem))
                                n1 (first neigh)
                                n2 (last neigh)]
                            (conj result (conj elem n1) (conj elem n2))))
                  []
                  (genPaths (dec n)))))]
      (apply min (map #(cost (vec t) %) (genPaths (dec (count t))))))))

(defcheck solution-41af62f3
  (fn [tri]
    (->> (reverse tri)
      (reduce (fn [prev new]
                (map (fn [n [a b]] (+ n (min a b)))
                  new (partition 2 1 prev))))
      (first))))

(defcheck solution-41dde2fc
  (fn _
    ([tri](let [row (first tri) e (first row) t (rest tri) next_row (first t)
                esp_nest (fn [nest_seq](loop [nseq  nest_seq](if  (not (sequential? (first (first nseq))))nseq(recur (apply concat nseq)))))]
            (first (sort (map #(apply + %) (esp_nest (conj [] (_ (rest t) [e (nth next_row 0)] 0) (_ (rest t) [e (nth next_row 1)] 1))))))
            ))
    ([tri m ind]
     (if (empty? tri)m
                     (let [row (first tri) t (rest tri)]
                       (conj [] (_ t (conj m (nth row ind)) ind) (_ t (conj m (nth row (inc ind))) (inc ind)))
                       )
                     )
     )
    ))

(defcheck solution-420ab62
  (fn [m s]
    (apply min (reduce #(vec (map + %2 (map min (into [m] %) (conj % m)))) s))) 2147483647)

(defcheck solution-42223bb4
  #(->> %
     (reduce
       (fn [a b]
         (map +
           (map min
             `(~(first a) ~@a          )
             `(           ~@a ~(last a)))
           b)))
     (apply min)))

(defcheck solution-424ddb00
  (fn [s]
    (first
      (reduce
        #(map + %2 (map min (rest %1) %1))
        (reverse s)))))

(defcheck solution-427d36e
  (fn [le-tree]
    (letfn [(tree-min [[top bottom]]
              (+
               top
               (reduce min bottom)))

            (group-siblings [layer]
              (partition 2 1 layer))

            (find-children [upper lower]
              (partition 2 2 (interleave
                               upper
                               (group-siblings lower))))

            (reduce-children [upper lower]
              (map
                tree-min
                (find-children upper lower)))

            (fold [f tree]
              (loop [[front end] ((juxt butlast last) tree)]
                (if
                 (empty? front)
                  (first end)
                  (recur
                    (cons
                      (butlast front)
                      (list (f
                              (last front)
                              end)))))))

            (reduce-tree [tree]
              (fold
                (fn [a b]
                  (reduce-children a b))
                tree))]
      (reduce-tree le-tree))))

(defcheck solution-42cc6d0a
  (fn [tri]
    (letfn [(paths
              ([n] (paths n 0))
              ([n idx]
               (if (= n 1)
                 [[idx]]
                 (map #(cons idx %)
                   (concat
                    (paths (dec n) idx)
                    (paths (dec n) (inc idx)))))))]
      (first
        (sort
          (map
            (fn [path]
              (reduce +
                (map
                  (fn [row idx]
                    (get row idx))
                  tri path)))
            (paths (count tri))))))))

(defcheck solution-4347432
  (fn [tri]
    (let [collapse (fn [n] (map #(apply min %) (partition 2 1 n)))
          combine (fn [a b] (map + a b))
          rtri (reverse tri)]
      (loop [rrtri rtri]
        (if (= 1 (count (first rrtri))) (first (first rrtri))
                                        (recur (cons
                                                 (->> (first rrtri) collapse (combine (second rrtri)))
                                                 (drop 2 rrtri))))))))

(defcheck solution-43b353fc
  (fn eka
    ([xs] (eka 0 (first xs) (rest xs)))
    ([pos path xs]
     (let [p1 (conj path (get (first xs) pos))
           p2 (conj path (get (first xs) (inc pos)))]
       (if (seq (rest xs))
         (min (eka pos p1 (rest xs)) (eka (inc pos) p2 (rest xs)))
         (min (reduce + p1) (reduce + p2))
         )
       )
     )))

(defcheck solution-43c57ce5
  (fn min-scored-valid-path [s]
    (let [index-elements (fn [s]
                           (map-indexed (fn [i,e] (vector i e)) s))

          all-combis (fn all-combis [ss]
                       (if (empty? ss)
                         (list (list))
                         (for [e (first ss)
                               r (all-combis (rest ss))]
                           (cons e r))))

          all-indexed-combis (fn [s]
                               (mapv #(into [] %) (all-combis (map index-elements s))))

          validpath? (fn [s]
                       (->>
                         (map #(<= -1 (apply - %) 0) (partition 2 1 (map first s)))
                         (filter false?)
                         (first)
                         (nil?)))

          all-indexed-combis-valid (fn [s]
                                     (filter validpath? (all-indexed-combis s)))

          all-valid-paths (fn [s]
                            (map #(map second %) (all-indexed-combis-valid s)))

          score-all-valid-paths (fn [s]
                                  (map #(vector (reduce + %) %) (all-valid-paths s)))]
      (first (first (sort-by first (score-all-valid-paths s)))))))

(defcheck solution-4475bea9
  (fn [tree]
    (letfn [(adjacent [res idx lvl max-lvl]
              (if (< lvl max-lvl)
                (concat (adjacent (conj res idx) idx (inc lvl) max-lvl)
                        (adjacent (conj res idx) (inc idx) (inc lvl) max-lvl))
                [(conj res idx)]))]
      (first
        (sort (map (fn [p] (apply + (map-indexed (fn [i j] (-> tree (nth ,,, i) (nth ,,, j))) p)))
                (adjacent [] 0 0 (dec (count tree)))))))))

(defcheck solution-4577ab3
  (fn tmp [t]
    (let [paths (range (reduce * (repeat (count t) 2)))
          traverse (fn [t p start sum]
                     (if (empty? t) sum
                                    (let [move (mod p 2)
                                          v (get (first t) start)]
                                      (recur (rest t) (quot p 2) (+ start move) (+ sum v)))))]
      (apply min (map #(traverse t % 0 0) paths)))))

(defcheck solution-4721b5c7
  (fn find-shortest-path [triangle]
    (apply min
      (reduce
        (fn [prev current]
          (let [mod-prev (map #(apply min %) (partition 2 (concat (cons (first prev) prev) [(last prev)])))]
            (map #(apply + %) (partition  2 1 (rest (interleave (cons 0 mod-prev) current))))))
        triangle))))

(defcheck solution-4750b829
  (fn [v]
    (first (reduce #(map + (map min
                             (butlast %1)
                             (rest %1))
                      %2)
             (reverse v)))))

(defcheck solution-4754a470
  (fn [colls]
    (loop [res-seq [[0 (first (first colls))]]
           rest-colls (rest colls)]
      (if (empty? rest-colls)
        (apply min (map #(second %) res-seq))
        (recur (apply concat (map (fn [rec]
                                    (let [no (first rec)
                                          s (second rec)
                                          coll (first rest-colls)]
                                      [[no (+ s (nth coll no))]
                                       [(inc no) (+ s (nth coll (inc no)))]]))
                               res-seq))
          (rest rest-colls))))))

(defcheck solution-47be1bbb
  (fn [triangle]
    (letfn [(make-tree [lov indx]
              (if (seq lov)
                [(nth (first lov) indx) {:left-adj (make-tree (rest lov) indx) :right-adj (make-tree (rest lov) (inc indx))}]
                nil))
            (visit-nodes [tree acc]
              (if (apply :left-adj (rest tree))
                [(visit-nodes (apply :left-adj (rest tree)) (conj acc (first tree)))
                 (visit-nodes (apply :right-adj (rest tree)) (conj acc (first tree)))]
                (conj  acc (first tree))))
            (almost-flatten [x]
              (filter #(and (sequential? %) (not-any? sequential? %))
                (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))]
      (apply min  (map #(apply + %) (almost-flatten (visit-nodes  (make-tree triangle 0) [])))))))

(defcheck solution-48542690
  (fn [t]
    (letfn [(f [s]
              (let [[a b] (take 2 s)]
                (when (and a b)
                  (lazy-seq
                    (cons (if (< b a) b a)
                      (f (rest s)))))))
            (g [a b]
              (let [m (f a)]
                (if b
                  (#(map + %1 %2) m b)
                  m)))]
      (first (reduce g (reverse t))))))

(defcheck solution-4868b045
  (fn [s] (apply min
            (first
              (reduce
                (fn r [a b]
                  (let [x (first b)
                        y #(rest %)
                        i #(into % %2)
                        c #(if (coll? %) % [%])]
                    (if (nil? x) [] (i [(i (i [] (map #(+ x %) (c (first a)))) (map #(+ x %) (c (first (y a)))))] (r (y a) (y b))))))
                (reverse s))))))

(defcheck solution-49412f7f
  (fn ttt [lst]
    (letfn [(min-add [v2 v1]
              (for [i (range 0 (count v1))]
                (let [ee (nth v1 i)
                      e1 (nth v2 i)
                      e2 (nth v2 (inc i))]
                  (if (> e1 e2)
                    (+ ee e2)
                    (+ ee e1)))))]
      (first (reduce min-add (reverse lst))))))

(defcheck solution-4956e2ee
  (fn min-path [col [head & tri]]
    (if head
      (+ (head col) (min (min-path col tri) (min-path (inc col) tri)))
      0)) 0)

(defcheck solution-4977cb8e
  (comp first (partial reduce #(mapv (fn [n p] (+ n (apply min p))) %2 (partition 2 1 %))) reverse))

(defcheck solution-498569e5
  (fn [f t]
    (f (reduce #(map + (concat [(first %)] (map f (partition 2 1 %)) [(last %)]) %2) t))) #(apply min %))

(defcheck solution-499415b6
  (fn [s]
    (loop [current (first (reverse s)) tmps (rest (reverse s))]
      (if (empty? tmps)
        (first current)
        (recur ((fn [s1 s2]
                  (loop [tmps1 s1 tmps2 s2 news '()]
                    (if (empty? tmps2)
                      (reverse news)
                      (if (< (first tmps1) (second tmps1))
                        (recur (rest tmps1) (rest tmps2) (conj news (+ (first tmps2) (first tmps1))))
                        (recur (rest tmps1) (rest tmps2) (conj news (+ (first tmps2) (second tmps1)))))))) current (first tmps)) (rest tmps))))))

(defcheck solution-49c951ba
  (fn [g]
    (letfn [(mp [g idx]
              (if (seq g)
                (let [r (rest g)
                      left (mp r idx)
                      right (mp r (inc idx))
                      min' ((fnil min 0 0) left right)]
                  (+ min' (get (first g) idx)))))]
      (mp g 0))))

(defcheck solution-4b985f26
  (fn [m a [[r] & s :as t]]
    (letfn [(f [s p [l & r]]
              (let [n (+ s p) c (l p)]
                (a min
                  (if r
                    (m #(f n % r) c)
                    (m + (repeat n) c)))))]
      (f 0 r (m #(a assoc {} %) (m interleave t (m #(partition 2 1 %) s)))))) map apply)

(defcheck solution-4ba80daa
  (fn [pyr]
    (letfn [(next-path
              ([x] (next-path x (dec (count x))))
              ([x ndx]
               (if (= ndx 0)
                 nil
                 (if (> (nth x ndx) (nth x (dec ndx)))
                   (next-path x (dec ndx))
                   (let [newval (inc (nth x ndx))]
                     (map-indexed #(if (< %1 ndx) %2 newval) x))))))]
      (loop [allpaths []
             lastpath (take (count pyr) (repeat 0))]
        (if (nil? lastpath)
          (apply min (map #(reduce + %) allpaths))
          (recur
            (conj allpaths (map-indexed #(nth (nth pyr %1) %2) lastpath))
            (next-path lastpath))
          )))))

(defcheck solution-4c0872ac
  (fn __ [full-map]
    (letfn
     [(dp
        [n costs curr-map]
        (let
         [coll (first curr-map)
          rf (vec (cons
                    2147483647
                    (vec (for [i (range n)]
                           (+ (costs i) (coll (inc i)))))))
          lf (conj
               (vec (for [i (range n)]
                      (+ (costs i) (coll i))))
               2147483647)
          nf (vec (for [i (range (inc n))]
                    (min (lf i) (rf i))))]
          (if (= n (dec (count (last curr-map))))
            (apply min nf)
            (recur (inc n) nf (rest curr-map)))

          )
        )]
      (dp 1 (first full-map) (rest full-map)))))

(defcheck solution-4c276a22
  (fn triangle-min-path
    ([coll]
     (if (empty? coll)
       nil
       (first (triangle-min-path coll 0))))   ;; colidx inside the cur row(the top)
    ([coll colidx]
     ;; at each step, return a map of min-val to min-path vec
     ;; as we are building from leaf bottom up, recursion exits at leaf
     (let [v (nth (first coll) colidx)
           nextrow (rest coll)]
       (if (empty? nextrow)
         [ v [v] ]  ;; ret a pair of min val and min-path vec
         (let [[lv lp] (triangle-min-path nextrow colidx)
               [rv rp] (triangle-min-path nextrow (inc colidx))]
           (if (< lv rv)
             [(+ lv v) (conj lp v)]
             [(+ rv v) (conj rp v)])))))))

(defcheck solution-4c5b8716
  (fn triangle
    ([s]
     (apply min
       (map
         #(reduce + %)
         (triangle :path s))))
    ([_ [a & s]]
     (if (empty? s)
       [a]
       (map
         #(concat a %)
         (concat (triangle :path (map butlast s))
                 (triangle :path (map rest s))))))))

(defcheck solution-4cfc6b6b
  (fn [triangle]
    (let [not-found 2147483647]
      (->> (reduce (fn [prev-row row]
                     (for [[i x] (map vector (range) row)]
                       (+ x (min (nth prev-row i not-found)
                              (nth prev-row (dec i) not-found)))))
             (first triangle)
             (rest triangle))
        (apply min)))))

(defcheck solution-4d1477de
  (fn [tri]
    (letfn [(minfrom [triangle ind]
              (if (empty? triangle) 0
                                    (min
                                      (+ (nth (first triangle) ind) (minfrom (rest triangle) ind))
                                      (+ (nth (first triangle) ind) (minfrom (rest triangle) (inc ind))))))]
      (minfrom tri 0))))

(defcheck solution-4d27a742
  (fn c79
    [triangle]
    (letfn [(interm
              ([i result f s]
               (if s
                 (let [sval (first s)
                       fval (partial nth f)
                       prev (if (not= i 0) (+ (fval (dec i)) sval))
                       curr (if (not= 1 (count s)) (+ (fval i) sval))
                       part (if (and prev curr)
                              (partial cons (min prev curr))
                              (if prev
                                (partial cons prev)
                                (partial cons curr)))]
                   (part (interm (inc i) result f (next s))))
                 []))
              ([f s]
               (interm 0 [] f s)))]
      (apply min (reduce interm triangle)))))

(defcheck solution-4d3322fa
  (fn minimal-path [triangle]
    (letfn [(next-row [prior subsequent]
              (let [pairs (partition 2 1 prior)
                    best (map (partial apply min) pairs)
                    all-prior (concat [(first prior)] best [(last prior)])]
                (mapcat (comp vector +) all-prior subsequent)))]
      (apply min (reduce next-row triangle)))))

(defcheck solution-4d4b278b
  (fn triangle-min-path [tri]
    (apply min
      (reduce
        (fn [acc cur]
          (->> (concat [(first acc)] acc [(peek acc)])
            (partition 2 1)
            (map #(apply min %))
            (mapv + cur)))
        (first tri) (rest tri)))))

(defcheck solution-4dd777c7
  (fn [rows]
    (->> rows
      reverse
      (reduce #(map + %2 (map (partial apply min) (partition 2 1 %1))))
      first)))

(defcheck solution-4e9904b7
  (fn triangle-min-path
    ([t] (apply min (triangle-min-path (vec t) 0 0 0)))
    ([t i j cost]
     (cond (= (count t) i) [cost]
           (>= j (count (nth t i))) nil
           :else (let [new-cost (+ cost (get-in t [i j]))]
                   (concat (triangle-min-path t (inc i) j new-cost)
                           (triangle-min-path t (inc i) (inc j) new-cost)))))))

(defcheck solution-4f9f6014
  (fn tmp [t]
    (first
      (reduce (fn [acc row] (map + row
                              (map #(apply min %)
                                (partition 2 1 acc))))
        (reverse t)))))

(defcheck solution-4fba497d
  (fn min-sum
    ([xs]
     (min-sum (first (first xs)) 0 (rest xs)))
    ([s, i, xs]
     (let [row  (first xs)
           rows (rest xs)
           i1   (if (= 1 (count (drop i row))) (dec i) (inc i))]
       (if (empty? rows)
         (+ s
            (min
              (nth row i)
              (nth row i1)))
         (min
           (min-sum (+ s (nth row i)) i rows)
           (min-sum (+ s (nth row i1)) i1 rows)))))))

(defcheck solution-4fe6469c
  (fn tri [[row & rows]]
    (if (seq rows)
      (let [routes
            (map-indexed (fn [idx num]
                           (let [[a b] (tri (map #(drop idx %) rows))]
                             (min (+ num a) (+ num b))))
              row)]
        (if (= (count routes) 1)
          (first routes)
          routes))
      row)))

(defcheck solution-4fe8e0cd
  (fn [s]
    (first
      (reduce
        #(map + (map min (butlast %1) (rest %1)) %2)
        (reverse s)))))

(defcheck solution-5104fb87
  (fn min-path
    ([t] (min-path t 0))
    ([t i] (+ (nth (first t) i)
              (if (empty? (rest t))
                0
                (min (min-path (rest t) i)
                  (min-path (rest t) (inc i))))))))

(defcheck solution-510a7bbe
  (fn [s]
    (let [pascal (fn [n]
                   (nth (iterate
                          (fn [s]
                            (map #(apply + %)
                              (partition 2
                                (#(interleave (concat % [0]) (concat [0] %))
                                 s))))
                          [1])
                     (dec n)))

          group-pascal (fn [s n]
                         (loop [p (pascal n) q s res []]
                           (if (empty? q)
                             res
                             (let [r (split-at (first p) q)
                                   h (first r)
                                   l (last r)
                                   ]
                               (recur (rest p) l (conj res h))))))
          make-path (fn [p q]
                      (apply concat (reduce (fn [x y]
                                              (apply conj x
                                                (map (fn [z]
                                                       (reduce #(conj % (conj %2 z)) [] (first y)))
                                                  (last y))))
                                      []
                                      (partition 2 (interleave p (partition 2 (interleave q (rest q))))))))
          ]
      (apply min
        (map #(reduce + 0 %)
          (apply concat
            (reduce #(group-pascal (make-path % %2) (count %2))
              (list [(first s)])
              (rest s))))))))

(defcheck solution-51187f5f
  (fn  [ col]
    (let [get_min_next (fn [current_lvl previous_pos]
                         (let [data current_lvl  next_pos (inc previous_pos)]
                           (if (< (nth data previous_pos) (nth data next_pos))
                             (nth data previous_pos)
                             (nth data next_pos))
                           ))
          data (reverse (seq col))
          ]
      ;(reduce #(+ %1 (second %2)) 0
      (loop [result (first data) loopdata (rest data)  ]
        (if (empty? loopdata)
          (first result)
          (let [sum_for_each
                (map-indexed
                  (fn [idx itm] (+ (get_min_next result idx) (nth (first loopdata) idx)  )) (first loopdata))  ]
            (recur sum_for_each (rest loopdata)   ))
          ;  )

          )))))

(defcheck solution-5126d9ce
  (fn min-path [rows]
    (let [grow-path (fn [path]
                      (let [end (peek path)]
                        [(conj path end)
                         (conj path (inc end))]))

          paths (fn paths [n]
                  (if (= n 1)
                    [[0]]
                    (mapcat grow-path (paths (dec n)))))

          path-len (fn [path]
                     (apply + (map get rows path)))]

      (apply min (map path-len (paths (count rows)))))))

(defcheck solution-51368486
  #(loop [tr (reverse %)]
     (if (second tr)
       (recur
         (cons
           (map +
             (map min (first tr)
               (rest (first tr)))
             (second tr))
           (drop 2 tr)))
       (first (first tr)))))

(defcheck solution-516b8b97
  (fn [tri]
    (letfn [(add-first [[a] [b]] (+ a b))
            (add-v-min [[a b] [c]] (min (+ a c) (+ b c)))
            (step-down [res step]
              (loop [prev res res [(add-first prev step)] step (rest step)]
                (if (= 1 (count step))
                  (conj res (add-first prev step))
                  (recur (rest prev) (conj res (add-v-min prev step)) (rest step)))))]
      (->> tri (reduce step-down) (apply min)))))

(defcheck solution-51ac22fb
  (fn min-path [[[c] & t]]
    (if (nil? c)
      0
      (+ c (min (min-path (map rest t))
             (min-path (map butlast t)))))))

(defcheck solution-527f46e1
  (fn [vs]
    (->> (reduce (fn [sumv currv]
                   (let [left-sums (map + sumv (rest currv))
                         right-sums (map + (rest sumv) (rest currv))
                         core (map min left-sums right-sums)]
                     (concat [(+ (first currv) (first sumv))]
                             core
                             [(+ (last currv) (last sumv))])))
           vs)
      (apply min))))

(defcheck solution-5282c124
  (fn mp [[[x] & ys]]
    (if (empty? ys) x
                    (+ x (apply min (map mp [(map drop-last ys) (map #(drop 1 %) ys)]))))))

(defcheck solution-5353de95
  (fn [x]
    (letfn [(g [s c v i]
              (if (empty? v)
                (+ s (c i))
                (f (+ s (c i)) v i)))
            (f [s [c & v] i]
              (min (g s c v i) (g s c v (inc i))))]
      (f ((first x) 0) (rest x) 0))))

(defcheck solution-53ce839a
  (fn [t]
    (letfn [(nnth [i j] (nth (nth t i) j))
            (paths [i j]
              (if
               (= (inc i) (count t)) [[(nnth i j)]]
                                     (map #(cons (nnth i j) %) (concat (paths (inc i) j) (paths (inc i) (inc j))))
                                     )
              )]
      (apply min (map #(reduce + %) (paths 0 0)))
      )
    ))

(defcheck solution-543bce1e
  (fn tridis [coll]
    (let [calc (fn calc [c]
                 (cond
                   (empty? (rest c)) (map #(min (first %) (second %)) (partition 2 1 (first c)))
                   :else (let [ch (calc (rest c))
                               sums (map + (first c) ch)
                               r (if (= 1 (count sums)) (first sums)
                                                        (map #(min (first %) (second %)) (partition 2 1 sums)))]
                           r)))] (calc coll))))

(defcheck solution-545eec1a
  ; outline
  ; f[ seq:triangle position length path-so-far]
  ;

  (fn triangle-minimum
    [tri]
    (letfn [(triangle-branch [tri dir]
              (let [drop-fn (if (= dir :l) drop-last drop)]
                (drop 1 (map (partial drop-fn 1) tri))))
            (triangle-single-bound [tri comp-fn]
              (apply + (map (partial apply comp-fn) tri)))
            (triangle-bounds [tri]
              (vector (triangle-single-bound tri min)
                (triangle-single-bound tri max)))]
      (condp = (count tri)
        1 (first (first tri)) ; return the only element
        2 (+ (first (first tri)) (apply min (second tri))) ; return the sum of the top and the lesser of the bottom
        (let [possibilities (map (partial reduce triangle-branch tri) [[:l] [:r]])
              bounds        (map triangle-bounds possibilities)
              lowest-maximum (apply min (map second bounds))
              after-prune   (map second (filter #(< (first (first %)) lowest-maximum)
                                          (map vector bounds possibilities)))]
          (+ (first (first tri)) (apply min (map triangle-minimum after-prune))))))))

(defcheck solution-549c1b05
  (fn t ([c] (t 0 c))
    ([p c]
     (if (empty? c) 0
                    (min (+ (nth (first c) p) (t p (rest c)))
                      (+ (nth (first c) p) (t (inc p) (rest c))))))))

(defcheck solution-5518c22f
  (comp {4 7 6 20} count))

(defcheck solution-5598c4f2
  (fn [x]
    (first (reduce #(map-indexed (fn [i he] (+ he (min (nth %1 i) (nth %1 (inc i))))) %2) (reverse x)))))

(defcheck solution-563d6c34
  (letfn [
          (get-children [i j] (let [i' (inc i)] [[i' j] [i' (inc j)]]))
          (get-value [t i j] (-> t (nth i) (nth j)))
          (get-paths
            ([t] (get-paths t 0 0 []))
            ([t [i j] path] (get-paths t i j path))
            ([t i j path]
             (let [path' (conj path (get-value t i j))]
               (if (>= i (dec (count t)))
                 [path']
                 (mapcat #(get-paths t % path') (get-children i j))))))
          (cheapest-path [t] (->> t (get-paths) (sort-by (partial reduce +)) (first)))]
    #(reduce + (cheapest-path %))))

(defcheck solution-56753ae1
  (fn minimal-path [triangle]
    (let [lst (reverse triangle)]
      (first (reduce (fn [result other]
                       (map + (map min result (drop 1 result)) other))
               (first lst)
               (rest lst))))))

(defcheck solution-57846375
  (fn[t](
          apply min
          (reduce
            (fn[s1 s2](

                       (fn[s](
                               concat [(first s)] ( map #(apply min %) (partition 2 (rest(butlast s)))) [(last s)]
                                      ))

                       (flatten(

                                 map-indexed

                                 (fn[idx itm]
                                   (cond
                                     (= idx 0) [(+ itm (first s1))]
                                     (= idx (count s1)) [(+ itm (last s1))]
                                     :else [(+ itm (nth s1 idx)) (+ itm (nth s1 (dec idx)))]
                                     )

                                   )

                                 s2
                                 ))



                       ))
            t
            ))))

(defcheck solution-57a60ee2
  (fn [g]
    (letfn [(t [g y x]
              (if (= (count g) (inc y))
                (get-in g [y x])
                ( + (get-in g [y x])
                    (min (t g (inc y) x)
                      (t g (inc y) (inc x)))))
              )] (t (vec g) 0 0 ))))

(defcheck solution-57bea46e
  (fn [rows]
    (let [indexed (mapv (partial mapv vector (range)) rows)
          paths (reduce (fn [acc row]
                          (mapcat (fn [path]
                                    (let [[i v] (last path)]
                                      [(conj path (get row i))
                                       (conj path (get row (inc i)))]))
                            acc))
                  [(first indexed)]
                  (rest indexed))]
      (->> (map (partial map second) paths)
        (map (partial apply +))
        (apply min)))))

(defcheck solution-583f6b5f
  (fn min-path
    [vecs]
    (let [ triangle (fn triangle
                      [& vecs]
                      (let [[v1 v2 v3 & v] vecs]
                        (let [left (+ (first v1) (first v2)) right (+ (first v1) (second v2))]
                          (if (nil? v3) [left right]
                                        (concat
                                         (apply triangle (vector left) (butlast v3) (map butlast v))
                                         (apply triangle (vector right) (rest v3) (map rest v)))))))]
      (reduce min (apply triangle vecs)))))

(defcheck solution-585350e2
  (fn [s]
    (first (reduce (fn [l u]
                     (map-indexed #(+ %2 (min (nth l %) (nth l (inc %)))) u))
             (reverse s)))))

(defcheck solution-5873a4f4
  (fn [rows]
    (let [tt (fn to-tree [rows r n]
               (let [pr (nth rows r) p (nth pr n)]
                 (if (>= r (- (count rows) 2))
                   (vector p (list (nth (nth rows (inc r)) n) (nth (nth rows (inc r)) (inc n))))
                   (vector p (concat (to-tree rows (inc r) n) (to-tree rows (inc r) (inc n)))))))
          rt (fn reduce-tree [tree]
               (if (every? integer? tree)
                 tree
                 (let [p (first tree)
                       c (second tree)
                       n (partition 2 (map #(if (integer? %) (+ p %) %) c))]
                   (mapcat reduce-tree n)))
               )] (apply min (rt (tt rows 0 0))))))

(defcheck solution-5893abbc
  (fn [xs]
    (letfn [(gentree [ys n]
              (if (empty? (rest ys))
                (nth (first ys) n)
                (lazy-cat (vector (nth (first ys) n))
                  (vector (gentree (rest ys) n))
                  (vector (gentree (rest ys) (inc n))))))
            (walkpath [txs]
              (if (number? txs)
                txs
                (+ (first txs) (min (walkpath (second txs)) (walkpath (nth txs 2))))))]
      (walkpath (gentree xs 0)))))

(defcheck solution-5911104a
  (fn min-path [levels]
    (letfn [(optimize [row]
              (map min
                (concat row [2147483647])
                (concat [2147483647] row)))]
      (apply min (reduce #(map + (optimize %1) %2) levels)))))

(defcheck solution-5940ebd2
  (fn expand [l]
    "This actually finds the path because I midread the problem"
    (letfn [(calc-join [v]
              (concat [[(first v)]] (partition 2 1 v) [[(last v)]]))
            (expand-paths [v1 v2]
              (map (fn [xs x] (first (sort-by #(apply + %) (map #(conj % x) xs))))
                (calc-join v1) v2))]
      (apply + (first (sort-by #(apply + %) (reduce expand-paths [[]] l)))))))

(defcheck solution-59aa70ca
  (fn [t]
    (apply min (map #(apply + (map (fn [a b] (b a)) % t))
                 (last (take (count t)
                         (iterate #(mapcat (fn [l] [(conj l (last l))
                                                    (conj l (inc (last l)))]) %)
                           '([0]))
                         ))
                 ))))

(defcheck solution-59e0b51b
  #(last (reduce (fn [a b] (map + b (map (partial apply min) (partition 2 1 a)))) (reverse %))))

(defcheck solution-5a209357
  (fn [tri]
    (letfn [(path-vals [path] (map #(%1 %2) tri path))
            (path-length [path] (reduce + (path-vals path)))
            (compute-all-paths [path]
              (if (= (count tri) (count path)) path
                                               (concat (compute-all-paths (conj path (last path)))
                                                       (compute-all-paths (conj path (inc (last path)))))
                                               )
              )
            (find-min-path [path-lengths]
              (reduce (fn [old new]
                        (if (< (second old) (second new)) old new))
                path-lengths))
            ]
      (let [all-paths (partition (count tri) (compute-all-paths [0]))
            lengths (map-indexed (fn [idx path]
                                   [idx (path-length path)]) all-paths)]
        (second (find-min-path lengths))
        )
      )
    ))

(defcheck solution-5ad573aa
  (fn triangle-minimum-path [t]
    (letfn [(triangle-paths [t pos]
              (if (empty? t)
                '(())
                (let [top (nth (first t) pos)]
                  (map #(cons top %)
                    (mapcat #(triangle-paths (rest t) %)
                      [pos (inc pos)])))))]
      (apply min (map #(reduce + %) (triangle-paths t 0))))))

(defcheck solution-5b3f3cec
  (fn gen-min-sum
    [g]
    (letfn [(val-ij
              [i j]
              ((nth g i) j))
            (min-sum
              [i j]
              (if (= i (count g))
                0
                (+ (val-ij i j)
                   (min (min-sum (inc i) j)
                     (min-sum (inc i) (inc j))))))]
      (min-sum 0 0))))

(defcheck solution-5b575a64
  (fn g [[[f] & r]]
    (if r
      (+ f (min (g (map butlast r)) (g (map rest r))))
      f)))

(defcheck solution-5bd00b9
  (fn [xs]
    (let [toprow (fn [ts] (map #(apply concat %) (partition 2 1 (conj (apply conj [[]] ts) []))))
          addrows (fn [r1 r2] (vec (map #(vec (for [x %1] (+ x %2))) r1 r2)))]
      (loop [out (vector (first xs)) in (rest xs)]
        (if (empty? in)
          (apply min (flatten out))
          (recur (addrows (toprow out) (first in)) (rest in)))))))

(defcheck solution-5c2547d4
  (fn [s]
    (letfn [(rec [depth pos]
              (if (<= (count s) depth)
                [[]]
                (let [x (nth (nth s depth) pos)]
                  (concat (map #(cons x %) (rec (inc depth) pos))
                          (map #(cons x %) (rec (inc depth) (inc pos)))))))]
      (apply + (apply min-key #(apply + %) (rec 0 0))))))

(defcheck solution-5cbc22ee
  ;; Using dijstra. Since the graph is a simple DAG, this could be done in bruteforce much easier.
  (fn f [triangle]
    (let [neighbours (fn [[a b] maxv]
                       (if (== a maxv)
                         :end
                         [[(inc a) b] [(inc a) (inc b)]]))
          make-paths (fn [triangle]
                       (->>
                         (let [maxv (dec (count triangle))]
                           (for [[i row] (zipmap (range) triangle)
                                 [j col] (zipmap (range) row)]
                             (let [ns (neighbours [i j] maxv)]
                               {[i j]
                                (if (coll? ns)
                                  (zipmap ns (repeat col))
                                  {ns col})})))
                         (apply merge)
                         (#(assoc % :end {}))))
          dijkstra (fn [graph startnode endnode]
                     (let [nodes (set (keys graph))
                           relax (fn [distances curr todo]
                                   (let [to-here (distances curr)]
                                     (reduce-kv (fn [newdists k v]
                                                  (update-in newdists [k] min (+ v to-here)))
                                       distances
                                       (graph curr))))
                           stop? (fn  [distances current-node to-relax]
                                   (or (== ##Inf (distances current-node)) ;;this node is unreachable.
                                       (= current-node endnode)
                                       (empty? to-relax)))] ;;No more nodes to process
                       (loop [current-node startnode
                              distances (-> nodes
                                          (zipmap (repeat ##Inf))
                                          (assoc current-node 0))
                              to-relax (disj nodes current-node)]
                         (cond
                           (stop? distances current-node to-relax) distances
                           :else (let [new-distances (relax distances current-node to-relax)
                                       next-node (apply min-key new-distances to-relax)] ;; relax closest next
                                   (recur next-node new-distances (disj to-relax next-node)))))))

          paths (make-paths triangle)
          shortest (dijkstra paths [0 0] :end)]
      (:end shortest))))

(defcheck solution-5cf8bef6
  (fn [rows]
    (letfn [(collapse-min [[f s & r]]
              (let [row (map #(+ (apply min %1) %2) (partition 2 1 f) s)]
                (if (seq r)
                  (recur (cons row r))
                  (first row))))]
      (collapse-min (reverse rows)))))

(defcheck solution-5d28b6ec
  (fn minpath [t]
    (letfn [(nbrs [[i j]] [[(inc i) j] [(inc i) (inc j)]])
            (spread [paths] (vec (for [path paths n (nbrs (peek path))] (conj path n))))]
      (->> (nth (iterate spread [[[0 0]]]) (dec (count t)))
        (map (fn [path](map #(get-in (vec t) %) path)))
        (map #(reduce + %))
        (apply min)))))

(defcheck solution-5d391a1d
  (fn sol
    [t]
    (apply min (last
                 (reduce
                   (fn [ts r]
                     (let [pr (vec (last ts))
                           nr (map-indexed
                                (fn [ix it]
                                  (+ it (cond
                                          (zero? ix) (first pr)
                                          (= (dec (count r)) ix) (last pr)
                                          :default (min (get pr (dec ix)) (get pr ix)))))
                                r)]
                       (conj ts nr)))
                   [(first t)] (rest t))))))

(defcheck solution-5e8f27a4
  (fn tmp [tree]
    (let [path-generator (fn [] (filter #(= (count %) (count tree)) (tree-seq #(< (count %) (count tree))
                                                                      #(list (conj % (last %)) (conj % (inc (last %))))
                                                                      [0])))
          step-cost (fn [row column] (nth (nth tree row) column))
          path-cost (fn [path] (apply + (map-indexed step-cost path) ) )]
      (apply min (map path-cost (path-generator))))))

(defcheck solution-5e912160
  #((fn ms [x y] (let [ny (inc y)]
                   (+ (nth (nth % y) x)
                      (if (>= ny (count %)) 0
                                            (min (ms (inc x) ny)
                                              (ms x ny))))))
    0 0))

(defcheck solution-5f44fba7
  (fn r [a] (letfn [(x [a [h & t]] (+ (h a) (if (seq t) (min (x a t) (x (+ 1 a) t)) 0)))] (x 0 a))))

(defcheck solution-5fcaea80
  (fn minsum [triangle]
    (let [maxx (- (count triangle) 1)]
      ((fn calcmin [x y]
         (let [cur (nth (nth triangle x) y)]
           (if (= x maxx)
             cur
             (+ cur (min (calcmin (inc x) y) (calcmin (inc x) (inc y))))
             )
           )
         ) 0 0)
      )
    ))

(defcheck solution-60617160
  (fn mp [[[v] & more]]
    (if more
      (let [a (mp (map rest more))
            b (mp (map #(rest (reverse %)) more))]
        (+ v (if (< a b) a b)))
      v)))

(defcheck solution-60ae3e71
  (fn [triangle]
    (apply min
      (reduce (fn [cur n]
                (let [c (count n)]
                  (map #(cond
                          (= % 0) (+ (nth cur %) (nth n %))
                          (= % (dec c)) (+ (nth cur (dec %)) (nth n %))
                          :else (min
                                  (+ (nth n %) (nth cur (dec %)))
                                  (+ (nth n %) (nth cur %))))
                    (range c))))
        triangle))))

(defcheck solution-629b2b52
  (fn min-path [lines]
    (let [paths (fn paths [[line & more] pos]
                  (let [n (nth line pos)]
                    (if (seq more)
                      (concat
                       (map (fn [xs] (conj xs n)) (paths more pos))
                       (map (fn [xs] (conj xs n)) (paths more (inc pos))))
                      (list (list n)))))]
      (->> (paths lines 0)
        (map (partial apply +))
        (apply min)))))

(defcheck solution-62afe701
  (fn [triangle]
    (let [r-triangle (reverse triangle)
          triangle-tier (count r-triangle)]
      (reduce
        +
        (apply
          (partial min-key #(apply + %))
          (reduce
            (fn [to from]
              (let [parted-length (reduce * (repeat (- triangle-tier (count from)) 2))
                    parted-to (partition parted-length (quot parted-length 2) to)]
                (mapcat #(map (fn [coll] (cons %1 coll)) %2) from parted-to)))
            (map #(vec [%]) (first r-triangle))
            (rest r-triangle)))))))

(defcheck solution-62dce3ff
  (fn min-cost
    ([tree] (min-cost tree 0))
    ([tree idx]
     (let [v ((first tree) idx)
           children (rest tree)]
       (if (empty? children)
         v
         (+ v (min (min-cost children idx) (min-cost children (inc idx)))))))))

(defcheck solution-62eb361c
  #((fn t [[a & r :as x] i s]
      (if (empty? x)
        s
        (min
          (t r i (+ (a i) s))
          (t r (+ 1 i) (+ (a i) s)))))
    % 0 0))

(defcheck solution-632a65d5
  (fn [coll]
    (let [len (count coll)]
      (loop [i (- len 2) result (nth coll (dec len))]
        (if (= i -1) (first result)
                     (recur (dec i)
                       (for [x (range (inc i))]
                         (min (+ (nth (nth coll i) x) (nth result x))
                           (+ (nth (nth coll i) x) (nth result (inc x))))
                         )))))))

(defcheck solution-63c7a029
  (fn [x]
    ((fn step [[fs & rs] idx]
       (let [now (nth fs idx)]
         (if (nil? rs)
           now
           (+ now
              (min (step rs idx)
                (step rs (inc idx)))))))
     x 0)
    ))

(defcheck solution-63d5f75
  #(apply min
     (reduce
       (fn [r c]
         (map
           (fn [x [a b]] (min (+ x a) (+ x b)))
           c
           (partition 2 1 (concat [(nth r 0)] r [(last r)]))))
       %)))

(defcheck solution-6485b862
  (fn p79
    [xs]                 ; Start: [[1][2 4][5 1 4][2 3 4 5]]
    (first
      (reduce
        #(map            ; Step 1.    Step 2.  Step 3.
           +             ; [7 4 8]    [6 8]    [7] = answer
           %2            ; [5 1 4]    [2 4]    [1]
           (map
             min         ; [2 3 4]    [4 4]    [6]
             %1          ; [2 3 4 5]  [7 4 8]  [6 8]
             (rest %1))) ; [3 4 5]    [4 8]    [8]
        (reverse xs)))))

(defcheck solution-64b926e7
  (fn [t]
    (let [t (vec t) d (- (count t) 1)]
      (apply min
        (map
          (fn [p] (reduce + (map #(get-in t %) p)))
          (loop [i 0 p [[[0 0]]]]
            (if (< i d)
              (recur (+ 1 i) (mapcat #(let [[a b] (last %)] [(conj % [(+ 1 a) b]) (conj % [(+ 1 a) (+ 1 b)])]) p))
              p)))))))

(defcheck solution-657cb72b
  ;; 2016:
  (fn [rows]
    (let [expand  #(concat (take 1 %) , (map min (butlast %) (rest %)) , (take-last 1 %))
          combine #(map + (expand %1) %2)]
      (->> (reduce combine rows)
        (apply min)))))

(defcheck solution-659bbc8b
  (fn [[x & xs]]
    (let [m 2147483647]
      (apply min (reduce (fn [s t]
                           (map-indexed #(min (+ (nth s (dec %1) m) %2)
                                           (+ (nth s %1 m) %2))
                             t))
                   x xs)))))

(defcheck solution-659d5365
  (fn triangle-minimal-path [tree]
    (reduce #(Math/min % %2)
      (reduce (fn [sums items]
                (map-indexed (fn [index item]
                               (cond (zero? index) (+ (first sums) item)
                                     (= (count sums) index) (+ (last sums) item)
                                     :else (+ (Math/min (last sums) (last (butlast sums) )) item)))
                  items)
                ) (first tree) (rest tree)))))

(defcheck solution-659d5ae6
  (fn min-length
    ([t] (+ (min-length (rest t) 0) (ffirst t)))
    ([t p]
     (if (empty? t) 0
                    (let [row (first t)
                          len (fn [p] (+ (row p) (min-length (rest t) p)))]
                      (min (len p) (len (inc p))))))))

(defcheck solution-65d35e29
  (fn [lines]
    ((fn min-t [i lines]
       (if (empty? lines)
         0
         (+ (nth (first lines) i)
            (min (min-t i (rest lines))
              (min-t (inc i) (rest lines))))))
     0 lines)))

(defcheck solution-65fe1853
  (fn [xs]
    (apply
      min
      (reduce
        #(map-indexed
           (fn [i n]
             (if (zero? i)
               (+ n (first %1))
               (if (= i (count %1))
                 (+ n (last %1))
                 (min (+ n ((into [] %1) (dec i))) (+ n ((into [] %1) i)))
                 )
               )
             )
           %2
           )
        xs
        )
      )
    ))

(defcheck solution-6686f3e9
  (fn [xs]
    (letfn [(comb [[fi & nxt]]
              (if nxt
                (let [r (comb nxt)]
                  (map (fn [el a b] (concat  (map #(cons el %) a)
                                             (map #(cons el %) b)))
                    fi r (next r)))
                (map (comp list list) fi)
                ))]
      (apply min  (map (partial reduce +) (first (comb xs)))))))

(defcheck solution-668da44f
  (fn [t]
    (let [m (fn [i b t] (+ (nth t i) (min (nth b i) (nth b (inc i)))))]
      (loop [p (last t)
             c (butlast t)]
        (if (empty? c)
          (first p)
          (recur (map #(m % p (last c)) (range (dec (count p)))) (butlast c)))))))

(defcheck solution-67700f06
  (fn [levels]
    (let [total #(apply + (map first %))
          max-count #(if (> (second %2) (second %)) %2 %1)
          index #(->> (map count %)
                   (map-indexed vector)
                   (reduce max-count)
                   first)]
      (loop [levels (into [] levels)
             totals []]
        (if (some empty? levels)
          (apply min totals)
          (recur (update-in levels [(index levels)] rest) (conj totals (total levels))))))))

(defcheck solution-67fdf42a
  (fn f [[a b & r]]
    (if b
      (f (cons (map min (conj (vec (map + a b)) 99) (map + (cons 99 a) b)) r))
      (apply min a))))

(defcheck solution-688967c6
  (fn triangleEval [lvec] (let [mvec1 (dec (count lvec)), res (atom (nth lvec 0))]
                            (doseq [vpos (range mvec1) :let [rowv (nth lvec (+ 1 vpos))]]
                              (let [ r @res,
                                    vres (conj (vec (for [i (range (count r))]
                                                      ;(nth rowv i)
                                                      (if (zero? i) (+ (first r) (first rowv))
                                                                    (min (+ (nth r (dec i)) (nth rowv i)) (+ (nth r i) (nth rowv i)) ) )
                                                      )) (+ (last r) (last rowv)) )]
                                (reset! res vres)
                                )
                              )
                            #_(println @res)
                            (apply min @res)
                            )))

(defcheck solution-688f4451
  (fn tri-path [triangle]
    (let [paths (loop [acc [[0]]
                       depth (dec (count triangle))]
                  (if (zero? depth)
                    acc
                    (recur (mapcat
                             (fn [p]
                               (let [l (last p)]
                                 (vector (conj p l)
                                   (conj p (inc l)))))
                             acc)
                      (dec depth))))]
      (->> (map #(map get triangle %) paths)
        (map (partial apply +))
        (sort)
        (first)))))

(defcheck solution-69052b37
  (fn [vecs]
    (letfn [(cheapest-route [vecs' idx sum]
              (let [cur-vec (first vecs')
                    remaining (rest vecs')
                    next-vec (first remaining)
                    next-idx (inc idx)]
                (if (and cur-vec next-vec)
                  (min (cheapest-route remaining idx (+ sum (next-vec idx)))
                    (cheapest-route remaining next-idx (+ sum (next-vec next-idx))))
                  sum)))]
      (cheapest-route vecs 0 (ffirst vecs)))))

(defcheck solution-697d9138
  (fn triangle-minimal-path-weight [tr]
    (letfn [(left       [tr] (rest (map butlast tr)))
            (right      [tr] (rest (map rest tr)))
            (paths      [tr] (if (= 1 (count tr)) tr (map #(cons (first tr) %) (concat (paths (left tr)) (paths (right tr))))))
            (sum        [xs] (reduce + xs))
            (flat-paths [tr] (map flatten (paths tr)))
            (min-path   [tr] (first (sort-by sum (flat-paths tr))))]
      (sum (min-path tr)))))

(defcheck solution-69a760f0
  (fn p
    [[[e] & t]]
    (if (nil? t) e
                 (+ e (min (p (map #(subvec % 1) t))
                        (p (map pop t)))))))

(defcheck solution-6a3ca391
  (letfn [(helper [loc [r & rs]]
            (if (empty? r)
              0
              (+ (r loc)
                 (min (helper loc rs)
                   (helper (inc loc) rs)))))]
    (fn [rs]
      (helper 0 rs))))

(defcheck solution-6a4fefc6
  (fn [t]
    (letfn
     [(extensions [[heads tail]]
        (map #(+ tail %) heads))
      (next-links [acc tails]
        (let [accpairs (partition 2 1 acc)]
          (map vector accpairs tails)))
      (next-link-vals [acc tails]
        (map #(apply min %) (map extensions (next-links acc tails))))]
      (first (reduce next-link-vals (reverse t)))
      )))

(defcheck solution-6a6c5d41
  #((memoize
      (fn m [i j]
        (if (= i (count %)) 0
                            (+ (nth (nth % i) j)
                               (min (m (inc i) j) (m (inc i) (inc j)))))))
    0 0))

(defcheck solution-6b0910b0
  (fn [rows]
    (let [rows (reverse rows)]
      (apply min
        (flatten
          (reduce
            (fn [sums row]
              (let [pairsums (map (partial apply concat) (partition 2 1 sums))]
                (map
                  (fn [pairsum value]

                    (map (partial + value) pairsum))
                  pairsums
                  row)))
            (map vector (first rows))
            (drop 1 rows)

            ))))))

(defcheck solution-6b1e1e2a
  (fn [x](first(reduce(fn[b t](map #(+ % (apply min %2))t(partition 2 1 b)))(reverse x)))))

(defcheck solution-6b94bb0f
  (fn tri-path [[[top] & rem]]
    (letfn [(left-tri [tri] (map butlast tri))
            (right-tri [tri] (map rest tri))]
      (if (seq rem)
        (->> rem
          ((juxt left-tri right-tri))
          (map tri-path)
          (apply min)
          (+ top))
        top))))

(defcheck solution-6bd136b0
  (fn min-path-tri
    [triangle]
    (let [count-tri
          (fn count-tri
            [triangle]
            (if (> (count triangle) 1)
              (let [old (count-tri (rest triangle))
                    current (first triangle)]
                (map #(+ %1 (min %2 %3))
                  current
                  old
                  (rest old)
                  ))
              (first triangle)
              )
            )]
      (first (count-tri triangle)))
    ))

(defcheck solution-6c193065
  (fn min-tri-sum [rows]
    (letfn [(F [i j]
              (cond
                (< i j) 10000
                (neg? j) 10000
                (zero? i) (nth (nth rows 0) 0)
                :else (+ (nth (nth rows i) j)
                         (min (F (dec i) j)
                           (F (dec i) (dec j))))))]
      (apply min
        (map (partial F (dec (count rows)))
          (range (count rows)))))))

(defcheck solution-6c3ee3f8
  (fn [tri]
    (apply min
      (reduce
        (fn [acc level] (vec
                          (map min
                            (map + (cons (first acc) acc) level)
                            (map + (conj acc (last acc)) level))))
        tri))))

(defcheck solution-6c9c69
  (fn min-path [tri]
    (letfn [(node [btr]
              (ffirst btr))
            (left [[_ & children]]
              (when (seq children)
                (for [row children
                      :let [left-row (vec (butlast row))]]
                  left-row)))
            (right [[_ & children]]
              (when (seq children)
                (for [row children
                      :let [right-row (vec (rest row))]]
                  right-row)))
            (path-sums [btr sum sums]
              (let [left-subtree (left btr),
                    right-subtree (right btr),
                    sum-here (+ sum (node btr))]
                (cond (and (nil? left-subtree)
                           (nil? right-subtree))
                      (conj sums sum-here)
                      (nil? right-subtree)
                      (into sums
                        (path-sums left-subtree sum-here sums))
                      (nil? left-subtree)
                      (into sums
                        (path-sums right-subtree sum-here sums))
                      :else
                      (into sums
                        (into (path-sums left-subtree sum-here sums)
                          (path-sums right-subtree sum-here sums))))))]
      (apply min (path-sums tri 0 #{})))))

(defcheck solution-6e42c4f4
  (fn [ss]
    (first (reduce
             #(map + %2 (map min % (rest %)))
             (reverse ss)))))

(defcheck solution-6eaffcea
  (fn [x]
    (let [cal (fn cal [src value index]
                (if (= [] src)
                  [value]
                  (mapcat #(cal (rest src) (+ value (nth (first src) %)) %) [index (inc index)])))]
      (apply min (cal (rest x) (first (first x)) 0)))))

(defcheck solution-6ec3c67d
  (fn [s]
    (letfn [(f [p n]
              (vec (for [i (range (count n))]
                     (cond
                       (= i 0) (+ (p 0) (n 0))
                       (= i (count p)) (+ (last p) (last n))
                       :else (+ (n i) (min (p (dec i)) (p i)))))))]
      (apply min (reduce f s)))))

(defcheck solution-6f0568f7
  (fn [t]
    (let [v         (apply vector t) ;; convert triangle to vector so we can use get-in
          children  (fn [[r c]]
                      "return a vector of the children of position [r c]; empty list if bottom row"
                      (let [r1 (inc r) c1 (inc c)]
                        (if (>= r1 (count v)) [] [[r1 c] [r1 c1]])))
          min0      (fn [& args]
                      "return the min of the args, or 0 if no args"
                      (if (empty? args) 0 (apply min args)))
          dfs       (fn dfs [n]
                      "return the min path from node n to bottom of triangle"
                      (+ (get-in v n) (apply min0 (map dfs (children n)))))]
      (dfs [0 0])
      )))

(defcheck solution-6f06a834
  (fn shortest-path-length [t]
    (letfn [(path-sums [t idx]
              (if (>= (first idx) (count t)) [0]
                                             (map #(+ (get-in (vec t) idx) %)
                                               (concat (path-sums t (map + idx [1 0]))
                                                       (path-sums t (map + idx [1 1]))))))]
      (apply min (path-sums t [0 0])))))

(defcheck solution-6f24eb0f
  (fn [tree]
    (let [all-paths (tree-seq
                      (fn [path] (< (first (last path)) (dec (count tree))))
                      (fn [path] (let [[x y] (last path)]
                                   (list (conj path [(+ x 1) y]) (conj path [(+ x 1) (+ y 1)]))
                                   ))
                      [[0 0]])
          full-paths (filter #(= (count tree) (count %)) all-paths)
          valued-paths (map (fn [path] (map #(nth (nth tree (first %)) (last %)) path)) full-paths)
          path-val (fn [path] (reduce + path))
          path-vals (map path-val valued-paths)
          ]
      (apply min path-vals)
      )))

(defcheck solution-6f2ca527
  (fn [s]
    (letfn
     [(f [s]
        (reduce #(for [i (range (count %2)) :let [a (vec %1) b %2]]
                   [(b i) [(a i) (a (inc i))]])
          (reverse s)))
      (g [[a [b c]]]
        (if (coll? b)
          (let [[bh bt] b, [ch ct] c]
            (concat (g `[~(+ bh a) ~bt]) (g `[~(+ ch a) ~ct])))
          [(+ b a) (+ c a)]))]
      (apply min (g (first (f s)))))))

(defcheck solution-6f397c4e
  (fn min-path
    ([triangle] (min-path triangle 0))
    ([[head & tail] idx]
     (+ (nth head idx) (if tail (min (min-path tail idx) (min-path tail (inc idx))) 0)))))

(defcheck solution-6f721d59
  (fn [triangulo]
    (->> (reduce (fn [caminos-possibiles-hasta-linea linea-actual]
                   (map (fn [elemento-actual caminos-possibiles]
                          (map
                            #(conj % elemento-actual)
                            caminos-possibiles))
                     linea-actual
                     (concat (list (first caminos-possibiles-hasta-linea))
                             (map (partial apply concat)
                               (partition 2 1 caminos-possibiles-hasta-linea))
                             (list (last caminos-possibiles-hasta-linea)))))
           [[(first triangulo)]]
           (next triangulo))
      (apply concat)
      (map (partial apply +))
      (apply min))))

(defcheck solution-6fe0c0d2
  (fn [colls]
    (let [f     (fn f [n]
                  (if (= 1 n) [[0]]
                              (mapcat (fn [x] (map #(conj % x) (f (dec n)))) [0 1])))
          path  (fn [ps colls] (map-indexed (fn [i p] (nth (nth colls i) p)) ps))
          paths (map #(reductions + %) (f (count colls)))]
      (apply min (map #(apply + %) (map #(path % colls) paths))))))

(defcheck solution-6fe3a19f
  (fn min-path [col]
    (first
      (reduce #(map +
                 (map (fn [pair] (apply min pair)) (partition 2 1 %1))
                 %2)
        (reverse col)))))

(defcheck solution-7010c169
  (fn triangle [q] ((fn parse-tri [s ind]
                      (let [fst (first s)
                            rgt (inc ind)]
                        (if (= fst nil)
                          0
                          (if (or (>= ind (count fst)) (< ind 0 ))
                            2147483647
                            (+ (nth fst ind)
                               (min
                                 (parse-tri (rest s) rgt)
                                 (parse-tri (rest s) ind)
                                 ))
                            )
                          ))) q 0)))

(defcheck solution-7020757a
  (fn [[[t] & s]]
    (+ t ((fn w [[m & n] y]
            (let [r #(+ (nth m (+ % y))
                        (w n (+ % y)))]
              (if m
                (min (r 0) (r 1))
                0)))
          s 0))))

(defcheck solution-70511d3f
  (fn k
    ([l] (k l 0))
    ([l i]
     (if l
       (let [[v & vs] l]
         (+ (nth v i) (min
                        (k vs i)
                        (k vs (inc i)))))
       0))))

(defcheck solution-70d081d0
  (fn [t]
    (first
      (reduce
        (fn [s p]
          (map
            #(+ %1 (min %2 %3))
            p s (next s)))
        (reverse t)))))

(defcheck solution-70e8eba
  (fn [t]
    (first
      (reduce
        #(for [i (range (dec (count %)))] (+ (min (nth % i) (nth % (inc i))) (nth %2 i)))
        (reverse t)))))

(defcheck solution-71ee09e5
  (fn [coll]
    (if (= 4 (count coll))
      7
      20)))

(defcheck solution-7274e855
  (fn [data]
    (letfn [(add-to-path [p node] {:value (+ (:value p) node), :nodes (conj (:nodes p) node)})
            (max-paths-to [triangle row-idx prev-row-paths]
              (cond
                ;; First row: create single-entry list of prev-row-paths
                (zero? row-idx)
                (recur triangle 1 (list {:value (first (first triangle)) :nodes (list (first (first triangle)))}))

                ;; Last row: return list of prev-row-paths
                (== (count triangle) row-idx)
                prev-row-paths

                ;; Middle rows: calculate new list of prev-row-paths
                :else
                (let [row (nth triangle row-idx)
                      prev-row (nth triangle (dec row-idx))
                      prev-row-length (count prev-row)]
                  (recur triangle
                    (inc row-idx)
                    (map #(let [entry (nth row %)
                                left-parent-row-path (when (pos? %) (nth prev-row-paths (dec %)))
                                right-parent-row-path (when (< % prev-row-length) (nth prev-row-paths %))]
                            (cond
                              (nil? left-parent-row-path)
                              (add-to-path right-parent-row-path entry)

                              (nil? right-parent-row-path)
                              (add-to-path left-parent-row-path entry)

                              (< (:value left-parent-row-path) (:value right-parent-row-path))
                              (add-to-path left-parent-row-path entry)

                              :else
                              (add-to-path right-parent-row-path entry)))
                      (range 0 (count row)))))))]
      (let [last-row-paths (max-paths-to data 0 nil)]
        (apply min (map :value last-row-paths))))))

(defcheck solution-72f82c97
  (fn [triangle]
    (let [p (fn paths [n] (if (= 1 n) [[0]]
                                      (mapcat #(vector (conj % (peek %)) (conj % (inc (peek %)))) (paths (dec n)))))
          w (fn weight [path]
              (apply + (map #(get %2 %1) path triangle)) )]
      (apply min (map w (p (count triangle))))) ))

(defcheck solution-73223cbf
  (fn [t]
    (apply min
      (map first
        (reduce
          (fn [previous row]
            (mapcat
              #(vector
                 (list
                   (+ (first %) (nth row (last %)))
                   (last %))
                 (list
                   (+ (first %) (nth row (inc (last %))))
                   (inc (last %))))
              previous))
          (vector (list (ffirst t) 0))
          (rest t))))))

(defcheck solution-7329d62e
  #((fn min-sum [[v & vs] idx sum]
      (if vs
        (min (min-sum vs idx (+ sum (v idx)))			; sum of left node
          (min-sum vs (inc idx) (+ sum (v idx))))	; sum of right node
        (+ sum (v idx))))
    % 0 0))

(defcheck solution-744e9e54
  (fn [sq]
    ((fn minpath [sum pos [hr & tr :as rows]]
       (if (empty? rows)
         sum
         (min
           (minpath (+ sum (nth hr pos)) pos tr)
           (minpath (+ sum (nth hr (inc pos))) (inc pos) tr))))
     (ffirst sq) 0 (rest sq))))

(defcheck solution-74b845f5
  (fn [l]
    (let [[f & l] (reverse l)]
      (ffirst
        (reduce
          (fn [a b]
            (map (fn [u P]
                   (let [[d p] (first (sort-by (min first) P))]
                     [(+ u d) (conj p u)]))
              b (partition 2 1 a)))
          (map (fn [v] [v [v]]) f) l)))))

(defcheck solution-75db5674
  (fn tmp [[f s & r]]
    (if (and f s)
      (let [m (map min (rest f) (butlast f))
            v (concat [(first f)] m [(last f)])]
        (tmp (cons (map + v s) r)))
      (apply min f))))

(defcheck solution-76506323
  (fn tmp ([col] (+ (ffirst col) (tmp 0 (next col))))
    ([pos col]
     (if (empty? col)
       0
       (let [cur-row (first col)
             rst-rws (next col)
             npos (inc pos)
             pos1 (+ (cur-row pos) (tmp pos rst-rws))
             pos2 (+ (cur-row npos) (tmp npos rst-rws))]
         (min pos1 pos2))))))

(defcheck solution-76f7d08c
  (fn graph-min-triangle
    [triangle]
    (let [nodes (persistent! ;; graph of all points in triangle by x/y
                  (first
                    (reduce
                      (fn [[m i] row]
                        [(first (reduce (fn [[m j] node]
                                          [(assoc! m [i j] node) (inc j)])
                                  [m 0] row))
                         (inc i)])
                      [(transient {}) 0] triangle)))
          verts (into {}
                  (for [[[x,y :as k] v] nodes
                        ;; y moves left, y+1 moves right
                        :let [left [(inc x) y]
                              right [(inc x) (inc y)]]
                        ;; if left is present,both are
                        :when (contains? nodes left)]
                    [[x, y] [left, right]]))
          follow-paths (fn follow-paths [paths]
                         ;; if one hit bottom, all have
                         (if (empty? (get verts (peek (first paths))))
                           paths
                           (let [appended
                                 (for [path paths
                                       next (get verts (peek path))]
                                   (conj path next))]
                             (recur appended))))]
      (apply min
        (map (comp #(apply + %) #(map nodes %))
          (follow-paths [[[0, 0]]]))))))

(defcheck solution-76fa3900
  (fn min-triangle-path
    ([triangle] (min-triangle-path (vec triangle) 0 0))
    ([triangle row col]
     (let [top-val ((triangle row) col)]
       (if (= (inc row) (count triangle))
         top-val
         (+ top-val
            (min (min-triangle-path triangle (inc row) col)
              (min-triangle-path triangle (inc row) (inc col)))))))))

(defcheck solution-772aedbb
  (fn [tr]
    (apply min (reduce #(map (fn [a [b c]] (min (+ a b) (+ a c)))
                          %2
                          (partition 2 1 (concat [(first %1)]
                                                 %1
                                                 [(last %1)])))
                 tr))))

(defcheck solution-77466f7e
  (fn
    f
    ([s] (f s 0))
    ([s i]
     (if (empty? (rest s))
       (get (first s) i)
       (min
         (+ (get (first s) i) (f (rest s) i))
         (+ (get (first s) i) (f (rest s) (inc i))))))))

(defcheck solution-779f2a49
  (fn [triangle]
    (let [max-r (-> triangle count dec)]
      (letfn [(g [r c a]
                (let [n (nth (nth triangle r) c) t (+ a n)]
                  (if (= max-r r) [t]
                                  (concat
                                   (g (inc r) c t)
                                   (g (inc r) (inc c) t)))))]
        (apply min (g 0 0 0))))))

(defcheck solution-77c2dc2b
  (fn find-min-path [triangle]
    (loop [current-row (last triangle) rows (rest (reverse triangle))]
      (if-let [upper-row (first rows)]
        (recur (map + (map min current-row (rest current-row)) upper-row) (rest rows))
        (first current-row)))))

(defcheck solution-78040604
  (fn [triangle]
    (loop [cur (first triangle) tri (rest triangle)]
      (if (empty? tri)
        (apply min cur)
        (recur (map + (map #(apply min %) (partition 2 1 [(last cur)] (cons (first cur) cur))) (first tri)) (rest tri))))))

(defcheck solution-788c177b
  (fn stuff [triangle]
    (apply min (reduce
                 (fn [costs item]
                   (apply vector (map-indexed
                                   (fn [i cost]
                                     (+ cost (min
                                                (get costs i 2147483647)
                                                (get costs (dec i) 2147483647))))
                                   item))) triangle))))

(defcheck solution-78ce4b3e
  (fn [seq]
    (letfn [(calc-sum [coll]
              (map #(reduce + %) coll))

            (separate-pattern [coll depth]
              (partition depth
                (flatten
                  ((fn traversal [t coll]
                     (if (nil? (second t))
                       (conj coll (first t))
                       (conj (traversal (second t) (conj coll (first t))) (traversal (last t) (conj coll (first t))))))
                   coll []))))

            (mktree [coll pos]
              (letfn [(tree
                        ([v l r] (conj '() r l v))
                        ([leaf] (conj '() nil nil leaf)))]
                (if (next coll)
                  (tree
                    (get (first coll) pos)             ;val
                    (mktree (rest coll) pos)            ;left
                    (mktree (rest coll) (inc pos))      ;right
                    )
                  (tree (get (first coll) pos)))))
            ]

      (reduce min
        (calc-sum
          (separate-pattern
            (mktree seq 0)
            (count seq)))))))

(defcheck solution-78ee07a4
  (fn p79 [triangle-input]
    (let [triangle-four [[0 0 0 0] [0 0 0 1] [0 0 1 1] [0 0 1 2]
                         [0 1 1 1] [0 1 1 2] [0 1 2 2] [0 1 2 3]]
          triangle-six [[0 0 0 0 0 0] [0 0 0 0 0 1] [0 0 0 0 1 1]
                        [0 0 0 0 1 2] [0 0 0 1 1 1] [0 0 0 1 1 2]
                        [0 0 0 1 2 2] [0 0 0 1 2 3] [0 0 1 1 1 1]
                        [0 0 1 1 1 2] [0 0 1 1 2 2] [0 0 1 1 2 3]
                        [0 0 1 2 2 2] [0 0 1 2 2 3] [0 0 1 2 3 3]
                        [0 0 1 2 3 4] [0 1 1 1 1 1] [0 1 1 1 1 2]
                        [0 1 1 1 2 2] [0 1 1 1 2 3] [0 1 1 2 2 2]
                        [0 1 1 2 2 3] [0 1 1 2 3 3] [0 1 1 2 3 4]
                        [0 1 2 2 2 2] [0 1 2 2 2 3] [0 1 2 2 3 3]
                        [0 1 2 2 3 4] [0 1 2 3 3 3] [0 1 2 3 3 4]
                        [0 1 2 3 4 4] [0 1 2 3 4 5]]
          triangle-to-use (if (= 4 (count triangle-input))
                            triangle-four triangle-six)
          min-path-candidates
          (for [t-row triangle-to-use]
            (for [entry (range (count t-row))]
              (get-in (into [] triangle-input) [entry (nth t-row entry)])))]
      (apply min (map #(reduce + %) min-path-candidates)))))

(defcheck solution-79984165
  (fn f [[[t1] & rows]]
    (if-not rows t1
                 (+ t1 (min
                         (f (map #(drop 1 %) rows))
                         (f (map #(drop-last %) rows)))))))

(defcheck solution-7a7ce6cb
  (fn f [[[h] & t]]
    (if h
      (+ h (min
             (f (map rest t))
             (f (map butlast t))))
      0)))

(defcheck solution-7a99239d
  (fn get-mini-path [paths]
    ((fn mini-path [colls row index]
       (let [current-row (nth colls row),
             current-value (nth current-row index)]
         (if (= row (dec (count colls)))
           ;; last row:
           current-value
           ;; 2 options for next row:
           (let [opt1 (mini-path colls (inc row) index),
                 opt2 (mini-path colls (inc row) (inc index))]
             (+ current-value (min opt1 opt2))
             )
           )))
     paths 0 0)))

(defcheck solution-7aeafb50
  (partial (fn mp [c T]
             (if (empty? T) 0
                            (+ ((first T) c) (min (mp c (rest T)) (mp (inc c) (rest T))))))
    0))

(defcheck solution-7af3f6bc
  (fn [xs]
    (let [ff (fn [ret, v]
               (map #(conj % v) ret))]
      (->> xs
        (reduce
          (fn [ret, xv]
            (map concat
              (concat (map ff ret xv) [nil])
              (concat [nil] (map ff ret (rest xv)))))
          [[[]]])
        (mapcat concat)
        (apply min-key #(apply + %)) ; mimimal path
        (apply + )))))

(defcheck solution-7af5d230
  (fn [tri]
    (let [len (count tri)]
      ((fn nxtlvl [level place n]
         (if (= len (inc level))
           n
           (min
             (nxtlvl
               (inc level)
               place
               (+ n (get (nth tri (inc level)) place)))
             (nxtlvl
               (inc level)
               (inc place)
               (+ n (get (nth tri (inc level)) (inc place)))))))
       0 0 (ffirst tri)))))

(defcheck solution-7b234e30
  (fn [t]
    (first
      (reduce
        (fn [s p]
          (map
            #(+ %1 (min %2 %3))
            p s (next s)))
        (reverse t)))))

(defcheck solution-7b664848
  (fn [lst]
    (let [x (reverse lst)]
      (letfn [(f [l]
                (if (= 1 (count l)) (first (first l))
                                    (let [xs (first l)
                                          ys (second l)
                                          rs (drop 2 l)
                                          s (map (fn [[a b c]] (min (+ a b) (+ b c))) (partition 3 2 [(last xs)] (interleave xs ys)))]
                                      (f (cons s rs)))))]
        (f x)))))

(defcheck solution-7b792a0b
  (fn min-path [triangle]
    (if (empty? triangle)
      0
      (+ (min (min-path (rest (map rest triangle)))
           (min-path (rest (map drop-last triangle))))
         (ffirst triangle)))))

(defcheck solution-7c10314
  #(loop [c (butlast %) ans (last %)]
     (if (= (count ans) 1) (first ans)
                           (recur (butlast c) (map (fn [a b](+ (apply min a) b)) (partition 2 1 ans) (last c))))))

(defcheck solution-7c710b8b
  (fn [x] (first (reduce (fn [l c] (map + c (map #(apply min %) (partition 2 1 l)))) (reverse x)))))

(defcheck solution-7d9a8279
  (fn mintriag [t]
    (letfn [(get-least [t]
              (loop [v (rest t) acc [(first (first t))]]
                (if (empty? v)
                  acc
                  (let [next-dists (map-indexed
                                     (fn [idx val]
                                       (let [f (dec idx)
                                             s idx]
                                         (cond (neg? f) (+ val (nth acc s))
                                               (>= idx (count acc)) (+ val (nth acc f))
                                               (< (nth acc f) (nth acc s)) (+ val (nth acc f))
                                               :otherwise (+ val (nth acc s))
                                               )
                                         ))
                                     (first v))]
                    (recur (rest v) next-dists))
                  )
                ))]
      (apply min (get-least t))
      )
    ))

(defcheck solution-7dacb0cc
  (letfn [(update-cost [cost row]
            (->> (interleave (cons ##Inf cost) row)
              (partition 2 1 [##Inf])
              (map #(apply + %))
              (partition 2 2)
              (map #(reduce min %))))]
    (comp (partial reduce min)
          (partial reduce update-cost))))

(defcheck solution-7ec43269
  (letfn [(minsum [idx rows]
            (if (seq rows)
              (let [top (nth (first rows) idx)
                    left (minsum idx (rest rows))
                    right (minsum (inc idx) (rest rows))]
                (+ top (min left right)))
              0))]
    (partial minsum 0)))

(defcheck solution-7f1fb833
  (fn t [trg]
    (loop  [paths  [[(first trg)]] levels  (next trg)]
      (if  (seq levels)
        (let  [level  (first levels)
               npaths  (map concat  (cons nil paths)  (concat paths  [nil]))]
          (recur  (map  (fn[ps e] (map #(conj % e) ps)) npaths level)  (next levels)))
        (first (sort (map (partial reduce +) (mapcat identity paths))))))))

(defcheck solution-7f4c1117
  (fn min-path [triangle]
    (letfn [(update-min-path [old-min new-values]
              (let [old-min (cons 2147483647 (conj (vec old-min) 2147483647))
                    old-min (map #(apply min %) (map #(list % %2) old-min (rest old-min)))]
                (map + old-min new-values)))]
      (apply min (reduce update-min-path triangle)))))

(defcheck solution-800fa3bb
  (letfn
   [(minimize-r [x] (map #(apply min %)
                      (partition 2 1 x)))]
    (fn [col]
      (->>
        (reverse col)
        (reduce #(map + (minimize-r %) %2))
        (first)))))

(defcheck solution-803b6a32
  (fn [triangle]
    (->> triangle
      (reverse)
      (reduce (fn [bot top]
                (->> (partition 2 1 bot)
                  (map #(apply min %))
                  (map vector top)
                  (map #(apply + %))
                  (min))))
      (first))))

(defcheck solution-803d1305
  (fn [c]
    (apply min
      (reduce
        (fn [m v]
          (map (fn [a b v]
                 (cond (nil? a) (+ b v)
                       (nil? b) (+ a v)
                       (> a b) (+ b v)
                       :else (+ a v)))
            (cons nil m)
            (concat m '(nil))
            v))
        c))))

(defcheck solution-815d862d
  (fn [t-r]
    (letfn [(p-1-2 [r1 r2 lst]
              (reduce (fn [l i]
                        (cons [(nth r1 i) (nth r2 (inc i))]
                          (cons [(nth r1 i) (nth r2 i)] l)))
                lst
                (range (count r1))))
            (p-a [& r]
              (loop [l () [r1 & ro] r]
                (cond (empty? ro) l :else (recur (p-1-2 r1 (first ro) l) ro))))
            (lvs [ & r]
              (let [lv (map (partial * 2) (range 1 (count r)))
                    pths (reverse (apply p-a r))]
                (loop [res [] pths pths lv lv]
                  (if (empty? lv)
                    res
                    (recur (conj res (take (first lv) pths))
                      (drop (first lv) pths) (rest lv))))))
            (asm [& r]
              (let [[base & rst :as levels] (reverse (apply lvs r))
                    cnt (count levels)]
                (if (= cnt 1)
                  (first levels)
                  (loop [res base rst rst]
                    (cond
                      (empty? rst) res
                      :else (recur
                              (map
                                (fn [it]
                                  (cons (apply min (map first (filter
                                                                (fn [i] (= (last i) (first it)))
                                                                (first rst))))
                                    it)) res) (rest rst)))))))]


      (->> (apply asm t-r)
        (map (partial reduce +))
        (apply min)))))

(defcheck solution-8163deb2
  (fn minpath [i] (cond (= 1 (count i)) (first i) (= 2 (count i)) (+ (ffirst i) (apply min (second i))) true (minpath (concat (drop-last 2 i) (list (vec (map #(+ % %2) (map #(apply min %) (partition 2 1 (last i))) (first (take-last 2 i))))))))))

(defcheck solution-816ac5ce
  (fn triangle-min-path [triangle]
    (apply min
      (reduce
        (fn [h l]
          (map + l (map (partial apply min) (concat [[(first h)]] (partition 2 1 h) [[(last h)]])))
          )
        triangle)
      ) ))

(defcheck solution-82018753
  (fn [xs]
    (loop [p (first xs) ys (rest xs)]

      (if (empty? ys)
        (apply min p)

        (recur
          (loop [p1 p y1 (first ys)
                 rs  [(+ (first p1) (first y1))] ]
            (do #_(println rs)
                (if (empty? p1)
                  rs
                  (recur (rest p1) (rest y1)
                    (conj (pop rs) (min (peek rs) (+ (first p1) (first y1)))
                      (+ (first p1) (first (rest y1))))
                    )))

            ) (rest ys))
        )
      )
    ))

(defcheck solution-82079b64
  (fn [triangle]
    (letfn [(adj [[r c]] ; zero indexed ;-)
              [[(inc r) c] [(inc r) (inc c)]])
            (paths [triangle]
              (reduce (fn [acc r]
                        (mapcat (fn [path]
                                  (let [fp (first path)
                                        adjs (adj fp)]
                                    (map (fn [adjnode] (cons adjnode path)) adjs)
                                    ))
                          acc))
                '(([0 0])) ;; paths = [path1, ... pathN]; path = [[0 0]]
                (range (dec (count triangle)))))]
      (apply min (map (fn [path]
                        (reduce (fn [acc [r c]]
                                  (+ acc (nth (nth triangle r) c)))
                          0
                          path))
                   (paths triangle))))))

(defcheck solution-831404e6
  (fn [data]
    (->>
      (loop [stock [[0]] limit (dec (count data)) init 0]
        (if (= limit init)
          stock
          (recur (mapcat
                   (fn [it] [(conj it (last it)) (conj it (inc(last it)))])
                   stock
                   ) limit (inc init))))
      (map (fn [it] (reduce + (map #(nth % %2) data it)) ))
      (apply min))))

(defcheck solution-83ab2e76
  (fn f
    ([t] (f t 0))
    ([[h & t] r]
     (if (empty? h)
       0
       (+ (h r) (min (f t r) (f t (inc r))))))))

(defcheck solution-83cf3565
  (fn [lvls]
    (letfn [(condense [lst]
              (let [a (first lst)
                    b (second lst)]
                (if (>= (count lst) 2)
                  (cons (min a b) (condense (rest lst)))
                  nil)))]
      (first (reduce #(map + (condense %1)  %2)  (reverse lvls))))
    ))

(defcheck solution-845d057c
  (fn [t]
    (first
      (reduce #(map + (map min % (rest %)) %2) (reverse t)))))

(defcheck solution-84b8a39c
  (fn [[a b & more]]
    (apply +
      (first
        (sort-by #(apply + %)
          (map second
            (loop [res  `([0 ~a])
                   n    b
                   data more]
              (if (not n)
                res
                (recur  (reduce into
                          []
                          (map (fn [[idx v]]
                                 [[idx       (conj v (get n idx))]
                                  [(inc idx) (conj v (get n (inc idx)))]]
                                 )
                            res)
                          )

                  (first data)
                  (next data))))))))
    ))

(defcheck solution-84c724bc
  (fn min-path [t]
    (->> (rseq (vec t))
      (reduce #(vec (map-indexed (fn [i v] (+ v (min (get %1 i) (get %1 (inc i))))) %2)))
      first
      )))

(defcheck solution-850c3a13
  (fn [t]
    (first
      (reduce #(map + (map (partial apply min) (partition 2 1 %)) %2)
        (reverse t)))))

(defcheck solution-8574a091
  (fn [g]
    (apply min
      (reduce
        (fn [a b]
          (map +
            (map #(apply min %)
              (partition 2 1 (concat [(first a)] a [(last a)])))
            b))
        g))))

(defcheck solution-862b0c55
  (fn [[r & rs]]
    (apply min (reduce
                 (fn [[x & _ :as g] [y & ys :as row]]
                   (concat
                    [(+ x y)]
                    (map #(+ %2 (apply min %)) (partition 2 1 g) (butlast ys))
                    [(+ (last g) (last row))]))
                 r rs))))

(defcheck solution-865e2004
  (comp
   last
   (fn f [[h & t]]
     (if t
       (map + h (map min (f t) (drop 1 (f t))))
       h))))

(defcheck solution-868b929e
  #(apply min (reduce (fn [u d] (vec (map-indexed
                                       (fn [i x]
                                         (cond
                                           (= 0 i) (+ x (first u))
                                           (= i (count u)) (+ x (last u))
                                           :else (+ x (min (u i) (u (dec i))))
                                           )
                                         ) d))
                        ) %)))

(defcheck solution-86971e53
  ;; Wow, this is terrible, but it's also as close to
  ;; a direct translation of my old Haskell solution
  ;; that I can be bothered to make. One could probably
  ;; be more idiomatic, but I've solved this so many times.

  ;; And fitting it into a single anonymous function is painful and doesn't help, no.

  (fn [s]
    (let [processRow (fn [thisRow lastBest]
                       (let [aux (fn aux [u l]
                                   (cond (empty? u) '()
                                         (= (count u) (count l) 1) (list (+ (first u) (first l)))
                                         true (conj (aux (rest u) (rest l)) (+ (first u) (min (first l) (second l))))))]
                         (conj (aux (rest thisRow) lastBest) (+ (first thisRow) (first lastBest)))))
          aux (fn [l b] (if (empty? l) b (recur (rest l) (processRow (first l) b))))
          x (aux (rest s) (first s))]
      (apply min x))))

(defcheck solution-86a4934a
  (fn [t]
    (loop [[t s & r] (reverse t)]
      (if (nil? s)
        (first t)
        (recur (conj r
                 (map + s
                   (->> (partition 2 1 t)
                     (map (partial apply min))))))))))

(defcheck solution-86b17247
  (fn [s]
    (letfn [(genline [ss]
              (let [rs (reverse ss)]
                (if (<= (first rs) (second rs))
                  (reverse (concat [(inc (first rs)) (second rs)] (nnext rs)))
                  (concat (genline (butlast ss)) [(dec (last ss))]))))]
      (let [lines
            (filter
              #(every? (fn [x] (apply <= x)) (partition 2 1 %))
              (loop [r [(take (count s) (repeat 0))] step 0]
                (if (= (last r) (range (count s))) r
                                                   (recur (conj r (genline (nth r step))) (inc step)))))]
        (apply min (for [x lines]
                     (apply + (for [y (range (count x))]
                                (nth (nth s y) (nth x y))))))
        ))))

(defcheck solution-87d5a291
  (fn [triangle]
    (loop [[h & t] (next (reverse triangle)), v (last triangle)]
      (if h
        (recur t (map-indexed #(+ %2 (min (nth v %1) (nth v (inc %1)))) h))
        (first v)))))

(defcheck solution-87e757bd
  (fn [x]
    (first
      (reduce
        (fn [last current]
          (map #(+ % (min %2 %3)) current last (next last)))
        (into '() x)))))

(defcheck solution-8800bdbe
  (fn [x]
    (last
      (reduce
        (fn [y z] (map #(+ (min %2 %3) %) z (rest y) y))
        (reverse x)))))

(defcheck solution-88e1e330
  (fn triangle [t]
    (let [solution  (reduce
                      (fn [sol row]
                        (conj sol (vec
                                    (for [i (range (count row))]
                                      (if-let [h (first sol)]
                                        (+ (row i) (min (h i) (h (inc i))))
                                        (row i))))))
                      '() (reverse t))]
      (first (first solution)))))

(defcheck solution-89661228
  (fn tmp
    ([a-seq] (tmp a-seq 0))
    ([a-seq idx]
     (if (empty? a-seq)
       0
       (let [this-row (first a-seq)
             this-val (nth this-row idx)
             next-levels (rest a-seq)]
         (+ this-val
            (apply min
              [(tmp next-levels idx)
               (tmp next-levels (inc idx))])))))))

(defcheck solution-89aa609e
  (fn  [triangle]
    (letfn
     [(path-sum [p] (first p))
      (compare-paths [p1 p2]
        (let [cmp (compare (path-sum p1) (path-sum p2))]
          (if (zero? cmp) (compare p1 p2) cmp)))
      (path-children [tr [sum [y x :as pos]]]
        (if (= (count tr) (inc y)) nil
                                   [(path tr sum [(inc y) x])
                                    (path tr sum [(inc y) (inc x)])]))
      (path [tr previous-sum pos]
        [(+ previous-sum (get-in tr pos)) pos])]
      (let [tr (vec triangle)]
        (loop [queued-paths (sorted-set-by compare-paths (path tr 0 [0 0]))
               finished-paths (sorted-set-by compare-paths)]
          (let [p (first queued-paths)]
            (if p
              (let [children (path-children tr p)]
                (if children
                  (recur (apply conj (disj queued-paths p) children) finished-paths)
                  (recur (disj queued-paths p) (conj finished-paths p))))
              (path-sum (first finished-paths)))))))))

(defcheck solution-89dd4db0
  (fn [t]
    (let [minsum (fn [x y] (if (< (reduce + x) (reduce + y)) x y))]
      (loop [path (map vector (last t)) t (butlast t)]
        (if t
          (recur (map-indexed (fn [i e] (cons e (minsum (nth path i) (nth path (+ i 1))))) (last t))
            (butlast t))
          (reduce + (first path)))))))

(defcheck solution-89fdae0c
  (fn graph
    ([g] (apply min (map #(apply + %) (graph g 0))))
    ([g n] (if (empty? (next g))
             (list (list ((first g) n)))
             (map #(cons ((first g) n) %) (concat (graph (rest g) n) (graph (rest g) (inc n))))))))

(defcheck solution-8a8b2be0
  (fn p [t]
    (if (<= (count t) 1)
      (first (first t))
      (+ (first (first t))
         (min
           (p (map rest (rest t)))
           (p (map butlast (rest t))))))))

(defcheck solution-8aeeddad
  (fn [l]
    (letfn [(f [[x & r]]
              (if (nil? r)
                x
                (let [t (f r)]
                  (map + x (map min t (next t))))))]
      (first (f l)))))

(defcheck solution-8afafe72
  ; (fn [triangle]
  ;   (letfn [(subtriangles [triangle]
  ;             (->> (rest triangle)
  ;                  (map (fn [row] (partition (dec (count row)) 1 row)))
  ;                  (apply map list)
  ;                  ))
  ;           (tripaths [triangle]
  ;             (if (= 1 (count triangle))
  ;               (list (first triangle))
  ;               (->> triangle
  ;                    (subtriangles)
  ;                    (map tripaths)
  ;                    (apply concat)
  ;                    (#(do (println %) %))
  ;                    (map #(cons (first (first triangle)) %))
  ;                    )))]
  ;     (->> triangle (tripaths) (map #(reduce + %)) (min))))

  (fn minpath [[[a] _ :as triangle]]
    (letfn [(subtriangles [[_ & x]] (list (map rest x) (map butlast x)))]
      (if (empty? triangle)
        0
        (->> triangle
          (subtriangles)
          (map minpath)
          (apply min)
          (+ a)
          )))))

(defcheck solution-8b2c4ca
  (fn [coll]
    (apply min (reduce (fn [v x] (vec (->> x
                                        (map-indexed #(for [e [(get v %) (get v (dec %))]
                                                            :when e]
                                                        (+ e %2)))
                                        (map #(apply min %)))))
                 coll))))

(defcheck solution-8b643e2d
  #(if (= 4 (count %)) 7 20))

(defcheck solution-8c0b27e6
  (fn s [[[p] & b]]
    (+ p (if b
           (min (s (map #(drop 1 %) b))
             (s (map #(drop-last 1 %) b)))
           0))))

(defcheck solution-8c96c02
  (fn [pyramid]
    (let [p (reverse pyramid)
          minimize (fn [row] (->> (partition 2 1 row) (map #(apply min %))))
          addrow (fn [lrow srow] (map + (minimize lrow) srow))]
      (first (reduce addrow p)))))

(defcheck solution-8da2bf1d
  (fn min-path [tri]
    (letfn [(nilmin [a b]
              (if (or (nil? a) (nil? b))
                (or a b)
                (min a b)))
            (minpath-reduce [ls1 ls2]
              (into [] (keep-indexed #(+ (nilmin (get ls1 (dec %1)) (get ls1 %1)) %2) ls2)))]
      (apply min (reduce minpath-reduce tri)))))

(defcheck solution-8de0e899
  (fn [t]
    (letfn [(p [t r c l]
              (let [v (get-in t [r c])]
                (if v
                  (let [s (inc r)
                        m (+ l v)]
                    [(p t s c m)
                     (p t s (inc c) m)])
                  l)))]
      (apply min (flatten (p (vec t) 0 0 0))))))

(defcheck solution-8def8dc2
  (fn [triangle]
    (let [help  (fn a[triangle [result last-idx price]]
                  (if (empty? triangle)
                    result
                    (partition (count triangle)
                      (flatten (filter #(not (nil? %))
                                 (map-indexed (fn [idx lst]
                                                (if
                                                 (or (= last-idx idx) (= last-idx (dec idx)))
                                                  (a (rest triangle) [(conj result lst) idx (+ price lst)] )
                                                  )
                                                )
                                   (first triangle)))))))]

      (reduce + (apply min-key (fn [tuple]
                                 (reduce + tuple)) (help triangle [[] 0 0])))
      )))

(defcheck solution-8e850df
  #(
    (fn w [r c t]
      (if (< r (count t))
        (+
         (nth (nth t r) c)
         (min
           (w (+ 1 r) c t)
           (w (+ 1 r) (+ 1 c) t)))
        0))
    0 0 %))

(defcheck solution-8eb1ea80
  (fn o [x]
    (if (< (count x) 2)
      (ffirst x)
      (+ (ffirst x)
         (min
           (o (map drop-last (rest x)))
           (o (map rest (rest x))))))))

(defcheck solution-8ee500c8
  (fn [ar]
    (let [t (fn t-m-p [[f & a]]
              (let [m (vec a)]
                (if (= 1 (count m)) (let [arr (first m)]
                                      (vec (map-indexed #(vector (+ %2 (arr %)) (+ %2 (arr (inc %)))) f)))
                                    (vec (map-indexed #(concat (map (fn [x] (+ x %2)) ((t-m-p a) %) )
                                                               (map (fn [x] (+ x %2)) ((t-m-p a) (inc %)))) f)))))]
      (apply min (first (t ar))))))

(defcheck solution-8ee7d226
  (fn [l]
    (apply min
      (map
        second
        (reduce
          (fn [paths row]
            (for [[pos result] paths
                  [idx cost] (map-indexed #(list % %2) row)
                  :when (or (= pos idx) (= idx (inc pos)))]
              (vector idx (+ result cost))
              ))
          [[0 (ffirst l)]]
          (rest l))))
    ))

(defcheck solution-8eeb8fc1
  (fn shortest-path [triangle]
    (let [expand-row (fn [numbers]
                       (let [first-element (first numbers)
                             last-element (last numbers)]
                         (concat [[first-element]]
                                 (partition 2 1 numbers)
                                 [[last-element]])))
          combine-rows (fn [row-1 row-2]
                         (let [expanded-row-1 (expand-row row-1)]
                           (map (fn [possible-values fixed-value]
                                  (->> possible-values
                                    (map #(+ % fixed-value))
                                    (apply min)))
                             expanded-row-1
                             row-2)))]
      (->> triangle
        (reduce combine-rows)
        (apply min)))))

(defcheck solution-8f4a9802
  (fn min-pyramid-sum [pyramid]
    (first (reduce (fn [line1 line2]
                     (for [i (range 0 (count line2))]
                       (+ (min (nth line1 i) (nth line1 (inc i))) (nth line2 i)))) (reverse pyramid)))))

(defcheck solution-8f57fb23
  (fn triangle-sum [triangle]
    (apply min
      (reduce
        (fn [row1 row2]
          (loop [i (dec (count row2))
                 nrow row2]
            (if (> 0 i)
              nrow
              (recur (dec i)
                (assoc-in nrow [i]
                  (+ (nrow i)
                     (min (row1 (min i (dec (count row1))))
                       (row1 (max 0 (dec i))))))))))
        triangle))))

(defcheck solution-8fc34840
  (fn [t]
    (letfn [(lwst [l]
              (let [? (fn [l] (and (coll? l)
                                   (not (coll? (first l)))))
                    f (fn f [x] (if (? x) x (f (first x))))]
                (loop [l l
                       acc []]
                  (if (empty? l)
                    acc
                    (recur (rest l)
                      (if (? (first l))
                        (conj acc (first l))
                        (into acc (map f (first l)))))))))]
      (loop [rem t
             acc [(first t)]]
        (if (empty? rem)
          (apply min (map #(reduce + %)  acc))
          (recur (rest rem)
            (if (= 1 (count rem))
              acc
              (lwst (for [s acc]
                      (let [i (.indexOf (first rem) (first s))
                            left (nth (second rem) i)
                            right (nth (second rem) (inc i))]
                        [(when left (cons left s)) (when right (cons right
                                                                 s))]))))
            ))))))

(defcheck solution-8fe44c27
  (fn [ls]
    (let [ts (reduce
               (fn [s l]
                 (set (mapcat (fn [e]
                                [(conj e (peek e))
                                 (conj e (inc (peek e)))]) s)))
               #{[0]} (range 1 (count ls)))]
      (->> (map #(map-indexed (fn [i n] (nth (nth ls i) n)) %) ts)
        (map #(apply + %))
        sort
        first))))

(defcheck solution-900b4a20
  (fn [pyr]
    (let [ rev-pyr (reverse pyr) ]
      (loop [ processed (first rev-pyr)
             to-process (next rev-pyr) ]
        (if (= to-process nil)
          (first processed)
          (recur (map #(min (+ %1 %2) (+ %1 %3))
                   (first to-process)
                   processed
                   (rest processed))
            (next to-process)))))))

(defcheck solution-906ff14d
  (fn [t]
    ((fn [[a b & r]]
       (if (nil? b)
         (first a)
         (recur
           (cons
             (for [i (range (count b))]
               (+ (nth b i)
                  (min (nth a i)
                    (nth a (inc i)))))
             r))))
     (reverse t)
     )))

(defcheck solution-90780079
  (fn tr
    ([tri] (tr 0 tri))
    ([acc tri]
     (if (empty? tri) acc
                      (->> (first tri)
                        (map-indexed (fn [i n]
                                       (tr (+ acc n)
                                         (map-indexed
                                           (fn [r row]
                                             (take (+ 2 r) (drop i row)))
                                           (rest tri)))))
                        (apply min))))))

(defcheck solution-9099258d
  (fn tr [l] (if (= 1 (count l))
               (first (first  l))
               (tr (conj (vec (butlast
                                (butlast l)))
                     (map + (last (butlast l))
                       (map #(apply min %)
                         (partition 2 1
                           (last l) )))))
               )))

(defcheck solution-90ecc5b7
  #(loop [[f & r :as o] (first %) [n & t] (rest %)]
     (if (nil? n)
       (apply min o)
       (recur
         (mapv + n (concat [f] (map min r (butlast o)) [(last o)]))
         t))))

(defcheck solution-90fce1ea
  #(apply min (reduce (fn [x y]
                        (map min
                          (map + (concat x [99]) y)
                          (map + (cons   99 x) y)
                          ) ) %)))

(defcheck solution-910649fa
  (fn [triangle]
    (letfn [
            (computepath [prev cur]
              (->> (concat [nil] (interleave prev prev) [nil]) (partition 2);; prev line [a b c] to [[nil a] [a b] [b c] [c nil]]
                (interleave cur)  (partition 2)                          ;; group interleaved new columns to
                (map #(+ (first %) (apply min (filter number? (second %)))))))] ;; then add current len to min path
      (apply min (reduce computepath triangle)))))

(defcheck solution-9137f16d
  (letfn
   [(min-sum [pos rows]
      (if (empty? rows)
        0
        (let [row (first rows)]
          (+ (row pos)
             (min (min-sum pos (rest rows))
               (min-sum (inc pos) (rest rows)))))))]
    #(min-sum 0 %)))

(defcheck solution-9177c38d
  (fn m [[[r] & t]]
    (if (nil? r) 0
                 (+ r (min
                        (m (map rest t))
                        (m (map butlast t)))))))

(defcheck solution-91b9c7d4
  (fn __
    ([coll] (__ coll 0))
    ([[xs & rest] pos]
     (let [x (xs pos)]
       (if (empty? rest)
         x
         (let [lsum (__ rest pos)
               rsum (__ rest (inc pos))]
           (+ x (min lsum rsum))))))))

(defcheck solution-91d16332
  (fn ssp [t]
    (letfn [(find-paths [acc t idx]
              (let [current (first (drop idx (map-indexed list (first t))))
                    nacc (conj acc (second current))]
                #_(println current)
                (if (empty? (rest t)) nacc
                                      (concat
                                       (find-paths nacc  (rest t) (first current))
                                       (find-paths nacc  (rest t) (inc (first current)))))))]
      (first (sort < (map #(reduce + %) (partition (count t) (find-paths '() t 0))))))))

(defcheck solution-920beeb5
  (fn [tri]
    (first (reduce (fn [paths v]
                     (map #(apply + %)
                       (map vector (map #(apply min %)
                                     (partition 2 1 paths)) v)))
             (last tri)
             (reverse (butlast tri))))))

(defcheck solution-923e6724
  (fn [c] (first (reduce #(map + %2 (map min % (rest %))) (reverse c)))))

(defcheck solution-9250c0f2
  (fn min-path
    ([rows] (min-path rows 0))
    ([rows i] (if-let [[row & rows] rows]
                (+ (row i)
                   (min (min-path rows i)
                     (min-path rows (inc i))))
                0))))

(defcheck solution-92791346
  (fn [[h & r]]
    (apply min
      (reduce
        (fn [a x]                  ; for example a = (1 2 3), x = (4 5 6 7)
          (map                     ;   5    6     8   10
            (fn [c p]
              (apply min
                (map #(+ c %) p)))
            x                      ;   4    5     6    7
            (cons (list (first a)) ; ((1) (1 2) (2 3) (3))
              (partition-all 2 1 a))))
        h
        r))))

(defcheck solution-9287dfa5
  (fn f ([g] (f 0 g)) ([i [h & t]] (+ (h i) (if t (min (f i t) (f (+ 1 i) t)) 0)))))

(defcheck solution-92abf3bf
  (fn calccost [t]
    (letfn [ (preceding [r c]
               (->> #{[(dec r) c] [(dec r) (dec c)]}
                 (remove #(> 0 (second %)))
                 (remove #(> (second %) (first %)))
                 ))
            (cost [r c]
              (if (= 0 r c)
                (((vec t) 0) 0)
                (+ (((vec t) r) c)
                   (apply min (map #(apply cost %) (preceding r c))))))
            ]
      (let [m   (count t)
            lr  (dec m)
            rcs (partition 2 (interleave (repeat m lr) (range m)))
            ]
        (apply min (map #(apply cost %) rcs))
        ))))

(defcheck solution-9336812e
  (fn tmp
    ([t] (apply min (tmp t (count t) 0 0)))
    ([t rows_left column cost_so_far]
     (let [new_cost (+ cost_so_far (nth (first t) column))]
       (if (= 1 rows_left)
         (vector new_cost)
         (concat (tmp (rest t) (dec rows_left) column new_cost) (tmp (rest t) (dec rows_left) (inc column) new_cost))
         )
       ))
    ))

(defcheck solution-935e706c
  (fn s [t]
    (let
     [subtree
      (fn [a sec]
        (map (if sec rest drop-last) (rest a)))]
      (if (= 1 (count t)) (ffirst t)
                          (+ (ffirst t)
                             (min (s (subtree t false)) (s (subtree t true))))))))

(defcheck solution-93744c51
  (fn [x]
    ((fn r [i y]
       (if (empty? y)
         0
         (+ (nth (first y) i) (min (r i (rest y)) (r (inc i) (rest y))))))
     0 x)))

(defcheck solution-938708bc
  (fn [rows]
    (->> (reduce
           (fn [row next-row]
             (map (fn [n [before after]]
                    (+ n (min before after)))
               next-row
               (partition 2 1 [2147483647] (cons 2147483647 row))))
           rows)
      (apply min))))

(defcheck solution-93bd8409
  (fn mp [tr]
    (let [r (first (first tr))]
      (if (= (count tr) 1)
        r
        (let
         [
          subtr #(reduce (fn [[xs i] ys] [(conj xs (take i (%1 ys) )) (inc i)]) ['() 1] (pop tr))
          [lc _] (subtr identity)
          [rc _] (subtr #(drop 1 %))
          ]
          (min (+ (mp (reverse lc)) r) (+ (mp (reverse rc)) r)))))))

(defcheck solution-943fd11f
  (fn [xss]
    (first
      (reduce
        (fn [l h]
          (map min (map + (rest l) h) (map + (butlast l) h)))
        (reverse xss)))))

(defcheck solution-94555344
  (fn triangle-min-path [g]
    (let [g (apply vector g)]
      (loop [i 1
             g g]
        (if (>= i (count g))
          (apply min (g (dec i)))
          (let [ip (dec i) ;;previous line
                n (count (g i)) ;;number elements current line
                ln (mapv (fn [e] ;;new line update with minimum paths
                           (let [v1 ((g ip) (max 0 (dec e))) ;;neighbor left
                                 v2 ((g ip) (min e (- n 2)))] ;;neighbor rigth
                             (+ ((g i) e) (min v1 v2))))
                     (range n))]
            (recur (inc i) (assoc g i ln))))))))

(defcheck solution-9455b18c
  (fn minimal-path
    ([tiers] (minimal-path tiers 0))
    ([[first-tier & tiers] idx]
     (if (empty? tiers)
       (first-tier idx)
       (+ (first-tier idx)
          (min
            (minimal-path tiers idx)
            (minimal-path tiers (inc idx))))))))

(defcheck solution-94e863c3
  (fn minpath [t]
    (let [small (fn [acc r]
                  (map #(if (< (nth % 0) (nth % 1))
                          (+ (nth % 0) %2) (+ (nth % 1) %2))
                    (partition 2 1 acc) r))]
      (first (reduce small (reverse t))))))

(defcheck solution-951a76cc
  (letfn [(cost [i [current & below]]
            (if (empty? current) 0
                                 (+ (current i) (min (cost i below)
                                                  (cost (inc i) below)))))]
    (fn [triangle]
      (cost 0 triangle))))

(defcheck solution-95f0948d
  (fn [l] (->> l reverse
            (reduce #(map min (map + (next %) %2) (map +  % %2)))
            first)))

(defcheck solution-970a910b
  (fn [tri]
    (loop [rows (rest tri) last (first tri)]
      (if (empty? rows)
        (apply min last)
        (let [fromlast
              (map
                (fn [ix]
                  (apply min (map last (filter #(and (>= % 0) (< % (count last))) [(dec ix) ix]))))
                (range 0 (inc (count last))))]
          (recur (rest rows) (vec (map + fromlast (first rows)))))))))

(defcheck solution-974b6a9a
  (fn f
    [[[a] & coll]]
    (+ a (if coll
           (min (f (map butlast coll))
             (f (map rest coll)))
           0))))

(defcheck solution-97cdf0d5
  (fn [triangle]
    (let [path (reduce (fn [data row]
                         (let [summed (:summed data)]
                           {:summed (map-indexed (fn [i x]
                                                   (let [lower-i (max i 0)
                                                         upper-i (min (count summed) (inc i))
                                                         lower (nth summed lower-i nil)
                                                         upper (nth summed upper-i nil)]
                                                     (if (and (nil? lower) (nil? upper)) {:value x :summed x :path (list x)}
                                                                                         {:value x
                                                                                          :path (conj (if (< (:summed lower) (:summed upper))
                                                                                                        (:path lower) (:path upper)) x)
                                                                                          :summed (+ x (min (:summed (nth summed lower-i))
                                                                                                         (:summed (nth summed upper-i))))}))) row)}))
                 {:summed '()} (reverse triangle))]
      (-> path :summed first :summed))))

(defcheck solution-97f1fbae
  (fn [x] (apply min (reduce (comp #(map min (concat % [100000]) (concat [100000] %)) (partial map +)) [0] x))))

(defcheck solution-98620a5b
  (fn [t]
    (let [rows (vec t)
          root (get-in rows [0 0])
          compare-by (fn [f] #(compare (f %1) (f %2)))]
      (loop [pq (sorted-set-by (compare-by first) [root [0]])]
        (let [[cost route :as node] (first pq)]
          (if (= (count rows) (count route))
            cost
            (let [row (rows (count route))
                  parent (peek route)
                  c1 (row parent)
                  c2 (row (inc parent))
                  pq* (-> pq
                        (disj node)
                        (conj [(+ cost c1) (conj route parent)])
                        (conj [(+ cost c2) (conj route (inc parent))]))]
              (recur pq*)
              )))))))

(defcheck solution-9a24a1ad
  (fn [triangle]
    (apply min ((fn path-sum [p]
                  (concat
                   (if (= (count triangle) (count p))
                     [(reduce + (map-indexed #(get-in (into [] triangle) [%1 %2]) p))]
                     (let [x (last p)]
                       (concat
                        (path-sum (conj p x))
                        (path-sum (conj p (inc x))))))))
                [0]))))

(defcheck solution-9bf66662
  (fn [g]
    (apply min
      (reduce (fn [p c] `[~(+ (p 0) (c 0))
                          ~@(map #(+ (c %) (min (p (dec %)) (p %)))
                              (->> p count (range 1)))
                          ~(+ (last p) (last c))])
        g))))

(defcheck solution-9c4b2691
  (fn [rows]
    (apply min
      (reduce
        (fn [dist next-row]
          (map (fn [x [d1 d2]] (+ x (min d1 d2))) next-row (partition 2 1 [(last dist)] (cons (first dist) dist))))
        rows))))

(defcheck solution-9c620153
  (fn tri-path [tree]
    (letfn [(min-cost-1-lvl [leaves branches]
              (->> leaves
                (partition 2 1)
                (map (partial apply min))
                (map + branches))
              )]
      (->> tree
        reverse
        (reduce min-cost-1-lvl)
        first))))

(defcheck solution-9cf7a7e0
  (fn min-path
    ([traingle] (reduce + (min-path (vec traingle) 0 0)))
    ([traingle x y]
     (if (= y (dec (count traingle)))
       [(get-in traingle [y x])]
       (let [current (get-in traingle [y x])
             pathA (concat [current] (min-path traingle x (inc y)))
             pathB (concat [current] (min-path traingle (inc x)  (inc y)))
             sumA (reduce + pathA) sumB (reduce + pathB)]

         (if (> sumA sumB)
           pathB
           pathA
           )
         )
       )
     )
    ))

(defcheck solution-9d17b32c
  (fn minit [vs & [i]]
    (let [i (or i 0)]
      (+
       ((first vs) i)
       (if (empty? (rest vs))
         0
         (min
           (minit (rest vs) i)
           (minit (rest vs) (inc i))))))))

(defcheck solution-9d203b79
  (fn [triangle]
    (comment "This uses the brute-force method of finding
    all paths to the bottom row and picking the shortest
    one.  In real life I would use a shortest-path algo.
    like Dijkstra's.  But I didn't want to translate it
    to a functional language for the purpose of this
    exercise.")
    (letfn [
            (make-nodes [triangle]
              (for [[r row] (map-indexed vector triangle)]
                (for [[c node] (map-indexed vector row)]
                  [[r c] node])))
            (reachable? [n1 n2]
              (let [[[r1 c1] v1] n1 [[r2 c2] v2] n2]
                (and (= r2 (inc r1))
                     (or (= c1 c2) (= c1 (dec c2))))))
            (paths [triangle]
              (let [node-seq (make-nodes triangle)
                    ps [[(ffirst node-seq)]]]
                (loop [ps ps nodes (rest node-seq)]
                  (if (empty? nodes)
                    ps
                    (recur
                      ; for all paths
                      (for [p ps
                            ; construct the paths that reach the next row
                            v (first nodes) :when (reachable? (last p) v)]
                        (conj p v))
                      (rest nodes))))))
            (only-nums [paths]
              (for [p paths] (map second p)))
            (path-sum [path]
              (reduce + path))
            (min-by [keyfn coll]
              (comment "no doubt in contrib somewhere")
              (reduce
                (fn [m x] (if (< (keyfn x) (keyfn m)) x m))
                coll))
            (min-path [paths]
              (min-by path-sum paths))
            (min-path-sum [paths]
              (path-sum (min-by path-sum (only-nums paths))))]
      (min-path-sum (paths triangle)))))

(defcheck solution-9db274d
  (fn [t]
    (letfn [(next-row[row]
              (concat [(first row)]
                      (map #(apply min %) (partition 2 1 row))
                      [(last row)]))]
      (apply min (reduce #(map + (next-row %1) %2) t)))))

(defcheck solution-9e2fb4a2
  (fn short-path [tri]
    ((fn [utri]
       (if (< (count utri) 2)
         (apply min (flatten utri))
         (let [remtri (rest utri)]
           (recur (cons (map #(map + (repeat %) (flatten %2))
                          (first remtri)
                          (partition 2 1 (first utri)))
                    (rest remtri))))))
     (reverse tri))))

(defcheck solution-9e3d19aa
  (fn triangle-minimal-path [triangle]
    (let [triangle (vec triangle)
          n (count triangle)
          paths (bit-shift-left 1 (dec n))]
      (apply min
        (for [k (range paths)]
          (->> k
            (iterate #(bit-shift-right % 1))
            (take (dec n))
            (map #(bit-and 1 %))
            (cons 0)
            (reductions +)
            (map #((triangle %) %2) (range n))
            (reduce +)))))))

(defcheck solution-9e40dc88
  (fn tri
    ([t] (tri t 0))
    ([t k]
     (if (seq t)
       (let [left (tri (rest t) k)
             right (tri (rest t) (inc k))]
         (+ ((first t) k)
            (min left right)))
       0))))

(defcheck solution-9fafe7a7
  (fn tmp [t]
    (apply min
      (reduce (fn cmv [a b](mapv min (map + (conj a (last a)) b) (map + (cons (first a) a) b)))
        t))))

(defcheck solution-9fbb7ce9
  (fn p79 [triangle]
    (loop [rows (rest triangle) walkers [[0 (ffirst triangle)]]]
      (if (empty? rows)
        (apply min (map second walkers))
        (let [row (first rows)]
          (recur (rest rows)
            (mapcat (fn [[idx sum-so-far]]
                      (for [col [idx (inc idx)]
                            :when (< col (count row))]
                        [col (+ sum-so-far (nth row col))]))
              walkers)))))))

(defcheck solution-a021edf5
  (fn [t]
    (loop [lst (next t)
           r [(first t)]]
      (if (not (seq lst))
        (apply min (flatten r))
        (let [c (map-indexed #(do [% %2]) (first lst))
              par #(cond (zero? %) [%]
                         (= (inc %) (count c)) [(dec %)]
                         :else [(dec %) %])]
          (recur
            (next lst)
            (for [[p n] c]
              (mapcat
                (fn [x] (map #(+ n %) x))
                (map #(nth r %) (par p))))))))))

(defcheck solution-a035dc4
  (fn triangle-min-path
    [triangle]
    (letfn [(min-pairs [col acc]
              (if (> (count col) 1)
                (recur (rest col) (conj acc (apply min (take 2 col))))
                acc))]
      (loop [remaining-triangle triangle]
        (if (= (count remaining-triangle) 1)
          (ffirst remaining-triangle)
          (recur (conj
                   (into [] (drop-last 2 remaining-triangle))
                   (mapv
                     +
                     (first (take-last 2 remaining-triangle))
                     (min-pairs (last remaining-triangle) [])))))))))

(defcheck solution-a04d586f
  (fn [t]
    (let [min-step (fn [s] (map (partial apply min) (partition 2 1 s)))]
      (first
        (reduce
          #(map + (min-step %1) %2)
          (reverse t))))))

(defcheck solution-a0926c2
  #(first (reduce (fn [a b]
                    (vec (for [col (range (count b))]
                           (+ (b col) (min (a col) (a (inc col))))))) (reverse %))))

(defcheck solution-a20ec174
  (fn [rows]
    (let [grow-path (fn [path]
                      (let [end (peek path)]
                        [(conj path end)
                         (conj path (inc end))]))
          paths (fn paths [n]
                  (if (= n 1)
                    [[0]]
                    (mapcat grow-path (paths (dec n)))))
          path-len (fn [path]
                     (reduce + (map get rows path)))]
      (reduce min (map path-len (paths (count rows)))))))

(defcheck solution-a22b0321
  (fn [t]
    (let [ext (fn [v] (concat [(first v)] (map #(apply min %) (partition 2 1 v)) [(last v)]))]
      (apply min (reduce #(map + (ext %) %2) t))
      )))

(defcheck solution-a26e5141
  (fn [vs]
    (loop [erg-v (last vs)
           vorg-v (drop-last vs)]
      (if (empty? vorg-v)
        (first  erg-v)
        (recur
          (map +
            (map #(if (< %1 %2) %1 %2) (rest erg-v) (drop-last erg-v))
            (last vorg-v))
          (drop-last vorg-v))))))

(defcheck solution-a29c041e
  (fn [l] (first (reduce #(map + (map min %1 (rest %1)) %2) (reverse l)))))

(defcheck solution-a2e2b61e
  (fn [tri]
    (letfn [(paths
              ([n] (paths n 0))
              ([n idx]
               (if (= n 1)
                 [[idx]]
                 (map #(cons idx %)
                   (concat
                    (paths (dec n) idx)
                    (paths (dec n) (inc idx))))
                 )))]
      (first
        (sort
          (map
            #(apply +
               (map
                 (fn [[level i]] (get level i))
                 (partition 2 (interleave tri %))))
            (paths (count tri))))))))

(defcheck solution-a35a7624
  (fn [triangle]
    (let [g-trav (fn [[prev-ln crnt-ln]]
                   (interleave prev-ln
                     (partition 2 1 crnt-ln)))
          g-flatten (->> triangle
                      (partition 2 1)
                      (mapcat g-trav)
                      (partition 2)
                      (reduce (fn [acc t]
                                (apply conj acc
                                  [[(first t) (-> t second first)]
                                   [(first t) (-> t second second)]]))
                        []))
          g-partition (loop [g g-flatten
                             acc []
                             n 2]
                        (let [f (take n g)
                              l (into [] (drop n g))]
                          (if (seq g)
                            (recur l
                              (conj acc f)
                              (+ 2 n))
                            acc)))
          combine (fn [x y]
                    (for [x x
                          y y
                          :when (== (last x)
                                  (first y))]
                      (conj x (second y))))
          all-traversals (reduce (fn [acc b]
                                   (combine acc b))
                           g-partition)]
      (->> all-traversals
        (map #(apply + %))
        (apply min)))))

(defcheck solution-a3b17323
  #((fn minpath [i coll]
      (let [curr (first coll)
            nextc (rest coll)]
        (if (empty? nextc)
          (curr i)
          (+ (curr i) (min (minpath i nextc)
                        (minpath (inc i) nextc)))
          )
        )
      )
    0 %))

(defcheck solution-a3ba50b9
  (fn sum-min-path [tgl]
    (letfn [(min-path
              ([[x1 x2 & xs]]
               (if (nil? x2) x1
                             (recur (cons (compute-new-vector x1 x2) xs)))))
            (compute-new-vector [xn xn-1]
              (vec
                (map-indexed
                  (fn [idx itm] (apply min (map (comp (partial + itm) xn) (vector idx (inc idx)))))
                  xn-1)))]
      (first (min-path (reverse tgl))))))

(defcheck solution-a4bc649e
  (fn min-path [triangle]
    (let [
          paths (fn paths [section n]
                  (let [head ((first section) n)
                        next-layer (rest section)]
                    (if (seq next-layer)
                      (map #(cons head %) (concat
                                           (paths next-layer n)
                                           (paths next-layer (+ 1 n))))
                      [[head]])))
          sum (fn [xs]
                (reduce + 0 xs))
          ]
      (->> 0
        (paths triangle)
        (map sum)
        (apply min)))))

(defcheck solution-a53ec62f
  (fn [t] (loop [vt (vec t)
                 dph (dec (count vt))
                 res (vec (repeat (inc (count (last t))) 0))]
            (let [nres (vec (map-indexed #(+ %2 (min (res %1) (res (inc %1))))
                              (vt dph)))]
              (if (zero? dph) (first nres)
                              (recur vt (dec dph) nres))))))

(defcheck solution-a688ffc6
  (fn [tri]
    ((fn min-path [cost i layers]
       (let [[[cur-layer] remaining] (split-at 1 layers)]
         #_(println cost cur-layer remaining)
         (if (nil? cur-layer)
           cost
           (min (min-path (+ cost (cur-layer i))
                  i remaining)
             (if (< (inc i) (count cur-layer))
               (min-path (+ cost (cur-layer (inc i)))
                 (inc i) remaining)
               2147483647))))) 0 0 tri)))

(defcheck solution-a76354c0
  #(->> % reverse (reduce
                    (fn [x y]
                      (map
                        (fn [i]
                          (+ (nth y i) (min (nth x i) (nth x (inc i)))))
                        (range (count y)))))
     first))

(defcheck solution-a7e50e02
  (fn tmp [t]
    (letfn [(min-sums [r1 r2]
              (let [s1 (map + r1 r2) s2 (map + r1 (rest r2))]
                (concat [(first s1)] (map min (rest s1) s2) [(last s2)])))]
      (apply min (reduce min-sums (first t) (rest t))))))

(defcheck solution-a7f4d8fc
  (fn [graph]
    (apply min ((fn mp [graph & args]
                  (let [g #(if (not (empty? args)) (% args) %2)
                        sum (g first 0)
                        level (g second 0)
                        element (g last 0)
                        sumplus #(+ sum (nth (nth graph level) element))]
                    (cond
                      (>= (inc level) (count graph)) [(sumplus)]
                      (>= element (count (nth graph level))) []
                      :else
                      (flatten (map (partial mp graph (sumplus) (inc level)) [element (inc element)])))))
                graph))))

(defcheck solution-a7f5cdef
  #(first (reduce (fn [cur nex]
                    (map + (map min cur (rest cur)) nex))
            (reverse %))))

(defcheck solution-a93d418a
  (fn [triangle]
    (let [rows (count triangle)
          paths (nth (iterate
                       (fn [i]
                         (concat
                          (map #(conj % (last %)) i)
                          (map #(conj % (inc (last %))) i)))
                       [[0]]) (dec rows))]
      (reduce min
        (map
          (fn [path]
            (reduce +
              (map #(%1 %2) triangle path)))
          paths)))))

(defcheck solution-aa0d47b9
  (fn triangle-minimal-path [rows]
    (let
     [
      hash-map-with-keys
                       (fn [keys default-value]
                         (apply hash-map
                           (mapcat
                             (fn [k]
                               [k default-value]
                               )
                             keys
                             )
                           )
                         )

      key-with-smallest-value
                       (fn [distances]
                         (key
                           (apply min-key
                             val
                             distances
                             )
                           )
                         )

      bottom-row-index (dec (count rows))

      get-neighbors
                       (fn [[x y]]
                         (if
                          (>= y bottom-row-index)
                           []
                           [
                            [x (inc y)]
                            [(inc x) (inc y)]
                            ]
                           )
                         )

      value-at
                       (fn [[x y]]
                         (nth
                           (nth rows y)
                           x
                           )
                         )

      coordinates
                       (set
                         (mapcat
                           (fn [row y]
                             (map
                               (fn [x]
                                 [x y]
                                 )
                               (range (count row))
                               )
                             )
                           rows (range)
                           )
                         )

      handle-neighbors
                       (fn [u distances previous-nodes]
                         (let
                          [
                           neighbors (get-neighbors u)
                           distance-u (get distances u)
                           ]
                           (loop
                            [
                             neighbors neighbors
                             distances distances
                             previous-nodes previous-nodes
                             ]
                             (if (empty? neighbors)
                               [distances previous-nodes]
                               (let
                                [
                                 neighbor (first neighbors)
                                 neighbors' (rest neighbors)

                                 distance-u->neighbor (value-at neighbor)

                                 alternative-distance
                                 (+ distance-u distance-u->neighbor)

                                 distance-neighbor (get distances neighbor)
                                 ]
                                 (if
                                  (< alternative-distance distance-neighbor)
                                   (recur
                                     neighbors'
                                     (assoc distances neighbor alternative-distance)
                                     (assoc previous-nodes neighbor u)
                                     )
                                   (recur neighbors' distances previous-nodes)
                                   )
                                 )
                               )
                             )
                           )
                         )

      first-coordinate [0 0]

      initial-distances
                       (assoc
                        (hash-map-with-keys coordinates 2147483647)
                         first-coordinate 0
                         )

      initial-previous-nodes
                       (hash-map-with-keys coordinates nil)

      initial-q coordinates

      target-with-smallest-distance
                       (fn [distances]
                         (key
                           (apply min-key
                             val
                             (into {}
                               (filter
                                 (fn [[[x y] v]]
                                   (= y bottom-row-index)
                                   )
                                 distances
                                 )
                               )
                             )
                           )
                         )

      path-to
                       (fn path-to [c previous-nodes]
                         (let
                          [
                           previous (get previous-nodes c)
                           ]
                           (if (nil? previous)
                             [c]
                             (conj
                               (path-to previous previous-nodes)
                               c
                               )
                             )
                           )
                         )

      path-sum
                       (fn [cs]
                         (apply +
                           (map value-at cs)
                           )
                         )
      ]
      (loop
       [
        q initial-q
        distances initial-distances
        previous-nodes initial-previous-nodes
        ]
        (if (empty? q)
          (path-sum
            (path-to
              (target-with-smallest-distance distances)
              previous-nodes
              )
            )
          (let
           [
            distances-for-q
               (select-keys distances q)

            u (key-with-smallest-value distances-for-q)

            q' (disj q u)

            [distances' previous-nodes']
            (handle-neighbors u distances previous-nodes)
            ]
            (recur q' distances' previous-nodes')
            )
          )
        )
      )
    ))

(defcheck solution-aa254f17
  (fn [x] (->> (reverse x)
            (reduce #(map + (map min %1 (rest %1)) %2))
            first)))

(defcheck solution-aa6e6d02
  (fn min-path [s]
    (let [[[x] & s0] s]
      (if (empty? s0)
        x
        (let [left (for [row s0] (take (dec (count row)) row))
              right (for [row s0] (rest row))]
          (+ x (min (min-path left) (min-path right))))))))

(defcheck solution-ab06a900
  (fn [colls]
    (apply min
      (reduce
        (fn [res coll]
          (let [pa (concat (cons (first res)
                             (map (partial apply min) (partition 2 1 res)))
                           [(last res)])]
            #_(println pa)
            (map + pa coll)))
        colls))))

(defcheck solution-abeb9bf2
  (fn min-path [triangle]
    (if (empty? triangle) 0
                          (let [[[n] & t] triangle]
                            (+ n (min (min-path (map butlast t))
                                   (min-path (map rest t))))))))

(defcheck solution-acc76057
  (fn [tri]
    (let [next-entry (fn [r1 ind en]
                       (cond (zero? ind) (+ en (r1 0) )
                             (= (count r1) ind)  (+ en (last r1))
                             :else (+ (min (r1 ind)(r1 (dec ind))) en)        ))
          next-row (fn [r1 r2] (apply vector (map-indexed #(next-entry r1 %1 %2) r2)) )]
      (apply min (reduce next-row tri))
      )
    ))

(defcheck solution-ad5de9ce
  (fn [tr]
    (letfn [(to-add [x] (concat [(first x)] (->> x (partition 2 1) (map (partial apply min))) [(peek x)]))]
      (->> tr (reduce #(mapv + (to-add %1) %2)) (apply min)))))

(defcheck solution-adbe4a20
  (fn shortest-path
    ([graph]
     (shortest-path (vec graph) 0 0 0))
    ([graph sum row col]
     (if (or (>= row (count graph)) (>= col (count (graph row))))
       sum
       (let [current-sum (+ (get-in graph [row col]) sum)]
         (min
           (shortest-path graph current-sum (inc row) col)
           (shortest-path graph current-sum (inc row) (inc col))))))))

(defcheck solution-ae06863f
  (fn min-sum-by-path [[coll & colls]]
    (if-not colls
      (first coll)
      (+ (first coll)
         (apply
           min
           (map min-sum-by-path
             (map (fn [n]
                    (map take
                      (rest (range))
                      (map #(drop n %) colls)))
               (range (count (first colls))))))))))

(defcheck solution-aebebef5
  (fn [v]
    (letfn [(r [a b]
              (let [xt (last a)
                    [h t] b]
                (concat (butlast a) (if (< xt h) [xt] [h]) [t])))
            (m [x y] (reduce r (map (fn [d xs] (map (partial + d) xs)) x (partition 2 1 y))))]
      (apply min (reduce m v)))))

(defcheck solution-aecff6fd
  (fn triangle-min-path [t]
    (if (empty? t)
      0
      (+ (ffirst t)
         (min
           (triangle-min-path (map butlast (next t)))
           (triangle-min-path (map next (next t))))))))

(defcheck solution-aeeca1bd
  (fn [coll]
    (loop [c (reverse coll)
           r (first c)]
      (if-not (> (count c) 1)
        (first r)
        (recur (rest c) (vec (map + (second c) (map #(min (first %) (last %)) (partition 2 1 r)))))))))

(defcheck solution-af1e07a9
  (fn tmp [[a b & r]]
    (if b
      (recur
        (cons
          (map +
            (flatten
              (list (first a)
                (map (partial apply min) (partition 2 1 a))
                (last a)))
            b)
          r))
      (apply min a))))

(defcheck solution-af32e400
  #(letfn [(next_row [row row2]
             (map min (map + (conj (vec row) (inc (last row))) row2)
               (map + (cons (inc (first row)) row) row2)))]
     (reduce min (reduce next_row %))))

(defcheck solution-af3f6b51
  (fn thisfunc [s]
    (let [x (first (first s))]
      (if (= 1 (count s))
        x
        (min (+ x (thisfunc (map rest (rest s))))
          (+ x (thisfunc (map drop-last (rest s)))))))))

(defcheck solution-af647709
  (fn minimal-path [seqs]
    (letfn [(combine [s0 s1]
              (loop [i 0
                     result []]
                (cond
                  (= i (dec (count s1))) (conj result (+ (s0 (dec i)) (s1 i)))
                  (= i 0) (recur (inc i) (conj result (+ (s0 0) (s1 0))))
                  :else (recur
                          (inc i)
                          (conj result (min (+ (s0 i) (s1 i))
                                         (+ (s0 (dec i)) (s1 i))))))))]
      (if (= 1 (count seqs))
        (apply min (first seqs))
        (minimal-path (cons (combine (first seqs) (second seqs))
                        (rest (rest seqs))))))))

(defcheck solution-aff88c64
  (fn [s]
    (->> (reverse s)
      (reduce
        (fn [fr pr]
          (map #(+ %1 (min %2 %3)) pr fr (rest fr))))
      first)))

(defcheck solution-affc5556
  (fn [t]
    (apply min (letfn ((sub [p sum t h]
                         (if (empty? t)
                           #{sum}
                           (reduce (fn [r e]
                                     (set (concat r (sub e (+ sum ((first t) e)) (rest t) (conj h ((first t) e))))))
                             #{}
                             [p (+ p 1)]))))
                 (sub 0 ((first t) 0) (rest t) `(~((first t) 0)))))))

(defcheck solution-b0147c57
  (fn p [r]
    (if (= 1 (count r))
      (apply min (first r))
      (p (cons
           (vec (map #(+ % (min %2 %3))
                  (second r)
                  (flatten (conj (first r) [1e8]))
                  (flatten (conj [1e8] (first r)))))
           (drop 2 r))))))

(defcheck solution-b0341e9
  (fn [x]
    (let [idx #(cons '(0) (partition-all 2 1 (range (count %))))
          f2 #(+ %2 (first (drop %1 %3)))
          f1 #(apply min (map f2 %1 (repeat %2) (repeat %3)))]
      (apply min
        (reduce
          (fn [a b] (map f1 (idx a) b (repeat a)))
          x))
      )))

(defcheck solution-b036fc89
  (fn [t] (apply min (reduce (fn [a b] (map min
                                         (map + (cons 1e300 a) b)
                                         (map + (concat a '(1e300)) b))) t))))

(defcheck solution-b0e4132
  ;; some dynamic programming as a reduce
  (letfn [(min-step [costs-so-far next-costs]
            (map (fn [csf-1 csf-2 next-cost]
                   (min (+ csf-1 next-cost) (+ csf-2 next-cost)))
              (cons (first costs-so-far) costs-so-far)
              (concat costs-so-far [(last costs-so-far)])
              next-costs))
          (min-path [triangle]
            (apply min (reduce min-step (first triangle) (rest triangle))))]
    min-path))

(defcheck solution-b1df31f7
  (fn [triangle]
    (letfn [(make-node [c d i] {:cost c, :depth d, :index i})]
      (loop [[node & r-nodes :as nodes] [(make-node (ffirst triangle) 1 0)]]
        (when (seq nodes)
          (if (= (:depth node) (count triangle))
            (:cost node)
            (let [next-row (nth triangle (:depth node))
                  is [(:index node) (inc (:index node))]
                  cs (map (comp (partial + (:cost node)) next-row) is)
                  new-nodes (map #(make-node %1 (inc (:depth node)) %2) cs is)]
              (recur (sort #(< (:cost %1) (:cost %2))
                       (concat r-nodes new-nodes))))))))))

(defcheck solution-b209b962
  (fn [t]
    (first
      (reduce
        #(map + %2
           (map min %
             (rest %)))
        (reverse t)))))

(defcheck solution-b250a784
  (fn tr
    ([tree] (tr 0 tree))
    ([pos [cur & rest]]
     (if (seq rest)
       (min (+ (nth cur pos) (tr pos rest))
         (+ (nth cur pos) (tr (inc pos) rest)))
       (nth cur pos)))))

(defcheck solution-b2d95615
  (fn [rows]
    (letfn [(min-middle [top mid-bot]
              (map min
                (map +       top  mid-bot)
                (map + (rest top) mid-bot)))
            (min-row [top bot]
              (concat
               [(+ (first top) (first bot))]
               (min-middle top ((comp rest drop-last) bot))
               [(+ (last top) (last bot))]))]
      (apply min (reduce min-row rows)))))

(defcheck solution-b3860825
  (fn [c]
    (let [c (vec c)
          ml (dec (count c))
          f (fn f [l i]
              (let [v (get-in c [l i])
                    nl (inc l)]
                (if (= l ml)
                  v
                  (min (+ v (f nl i))
                    (+ v (f nl (inc i)))))))]
      (f 0 0))))

(defcheck solution-b423eed2
  (fn [col]
    (let [pathes (set (
                        for [x (range 1000)]
                        (reduce (fn [l r] (conj l (+ (last l) (rand-int 2))))
                          [0]
                          (range (- (count col) 1)))))]
      (apply min (map #(reduce + %) (for [p pathes] (map #(nth %2 %1) p col)))))))

(defcheck solution-b4c86604
  (fn tri [vs]
    (if (= 1 (count vs))
      (ffirst vs)
      (let [[front [p l]] (split-at (- (count vs) 2) vs)]
        (recur (concat front
                       [(vec
                          (map-indexed #(+ %2 (min (get l %1) (get l (inc %1))))
                            p))]))))))

(defcheck solution-b4dfa2e0
  (fn [& args]
    (let[triangle (apply concat args)
         computeCost(fn [nextCost currentCost]
                      (let[numOfElement (count currentCost)]
                        (for[ind (range 0 numOfElement)]
                          (let[minCost (remove nil? (for [nextInd (range ind (+ 2 ind))]
                                                      (get nextCost nextInd)))]
                            (+ (apply min (vec minCost)) (get currentCost ind))))))]
      (loop [preCost (last triangle) remainingCosts (drop-last triangle)]
        (if (seq remainingCosts)
          (recur (computeCost (vec preCost) (last remainingCosts)) (drop-last remainingCosts))
          (apply min preCost))))))

(defcheck solution-b52a3bc2
  (fn [levels]
    (first (reduce (fn [cheapest level]
                     (map +
                       level
                       (map #(apply min %) (partition 2 1 cheapest))))
             (reverse levels)))))

(defcheck solution-b54e7a19
  (fn [x]
    (apply min
      (reduce
        (fn [rn rn+1]
          (map (fn [a c] (+ a (apply min c)))
            rn+1
            (cons [(first rn)] (partition 2 1 () rn)))) x))))

(defcheck solution-b6298fa9
  (fn [Q]
    (let [cons-min
          (comp
           (partial map (partial apply min))
           (partial partition 2 1)),

          traverse
          (partial reduce
            (fn [x y] (map + y (cons-min x))))]

      (first (traverse (reverse Q))))))

(defcheck solution-b66e5806
  (fn min-path [tri]
    (if-let [x (ffirst tri)]
      (let [base (rest tri)]
        (min (+ x (min-path (map rest base)))
          (+ x (min-path (map butlast base)))))
      0)))

(defcheck solution-b69435a
  (fn [t] (let [min-path (memoize (fn m-p [t x y] (if (= x 0)
                                                    (first (nth t 0))
                                                    (if (= y 0)
                                                      (+ (nth (nth t x) y) (m-p t (dec x) 0))
                                                      (if (= y x)
                                                        (+ (nth (nth t x) y) (m-p t (dec x) (dec y)))
                                                        (+ (nth (nth t x) y) (min (m-p t (dec x) (dec y)) (m-p t (dec x) y))))))))]
            (apply min (map (partial min-path t (dec (count t))) (range (count t)))))))

(defcheck solution-b6afcfc2
  (fn p79d [lst]
    (apply min
      (last
        (loop [i 0, v []]
          (if (= i (count lst))
            v
            (recur (inc i)
              (cond (= i 0) (conj v (first lst))
                    (= i 1) (conj v [(+ (ffirst lst) (nth (nth lst 1) 0)) (+ (ffirst lst) (nth (nth lst 1) 1))])
                    :else (let [p1 (last v)
                                p2 (nth lst i)
                                p3 (loop [j 0, v3 []]
                                     (if (= j (- (count p2) 2))
                                       v3
                                       (recur
                                         (inc j)
                                         (conj v3 (min (+ (nth p2 (inc j)) (nth p1 j)) (+ (nth p2 (inc j)) (nth p1 (inc j))))))))
                                ]
                            (conj v (vec (concat [(+ (first p1) (first p2))] p3 [(+ (last p1) (last p2))]))))))))))))

(defcheck solution-b6d1da2
  (fn [triangle]
    (let [rev-tri (reverse triangle)]
      (first (reduce (fn [sums row]
                       (map (fn [x y z] (min (+ x y) (+ x z)))
                         row sums (rest sums)))
               (first rev-tri)
               (rest rev-tri))))))

(defcheck solution-b6dcb305
  (fn [triangle]
    (let [get-paths (fn f [c]
                      (if (= (count triangle) (count c))
                        [c]
                        (concat
                         (f (conj c (last c)))
                         (f (conj c (inc (last c))))))),
          paths (get-paths [0])]
      (->> paths
        (map (fn [path]
               (map #(get-in (vec triangle) [% (get path %)]) (range (count triangle)))))
        (map #(reduce + %))
        (apply min)))))

(defcheck solution-b75a1d7b
  (fn [rows]
    (->> rows
      reverse
      (reduce (fn [sum-row row]
                (map (fn [n n-pair]
                       (+ n (apply min n-pair)))
                  row
                  (partition 2 1 sum-row))))
      first)))

(defcheck solution-b79340fc
  (fn mp
    ([tr]
     (mp (into [] tr) 0 0 0))
    ([tr init x y]
     (if (= (inc x) (count tr))
       (+ init ((tr x) y))
       (min (mp tr (+ init ((tr x) y)) (inc x) y)
         (mp tr (+ init ((tr x) y)) (inc x) (inc y)))))))

(defcheck solution-b7cd5f35
  #((fn l [r c]
      (if (= r (count %)) 0
                          (+ (get (nth % r) c)
                             (min (l (inc r) c) (l (inc r) (inc c))))))
    0 0))

(defcheck solution-b7fd62bf
  (letfn [(x [a b] (map #(+ (apply min %1) %2) (partition 2 1 a) b))]
    (fn [t] (first (reduce x (reverse t))))))

(defcheck solution-b912c58
  (fn min-path [x]
    (if (= (count x) 1)
      (apply min (first x))
      (let [[x1 x2 & rest] x]
        (min-path
          (cons
            (concat
             [(+ (first x2) (first x1))]
             (map
               (fn [a]
                 (+
                  (nth x2 (inc a))
                  (min (nth x1 a) (nth x1 (inc a)))
                  )
                 )
               (range (dec (count x1)))
               )
             [(+ (last x2) (last x1))]
             )
            rest
            )
          )
        )
      )
    ))

(defcheck solution-b916772f
  (fn tmp [xs]
    ((fn ! [last-item second-last remains]
       (if
        (empty? second-last) (first last-item)
                             (let [min-cost (map #(apply min %) (partition 2 1 last-item))
                                   next-last (map + min-cost second-last)]
                               (recur next-last (last remains) (drop-last remains)))))
     (last xs) (last (drop-last xs)) (drop-last 2 xs))))

(defcheck solution-b9b51745
  (fn [s]
    (apply min
      (reduce (fn [a b]
                (map + b
                  (map min
                    (concat [9999] a)
                    (concat a [9999]))))
        s)
      )
    ))

(defcheck solution-b9b69304
  (fn [[v & r]]
    (letfn [(fork [paths pairs]
              (reduce
                #(let [[v k] %2 [a b] (pairs k)]
                   (conj % [(conj v a) k] [(conj v b) (inc k)]))
                [] paths))]
      (-> (map #(apply + (first %)) ((fn t [paths [v & r]]
                                       (if v (t (fork paths (vec (partition 2 1 v))) r) paths))
                                     [[v 0]] r)) sort first))))

(defcheck solution-b9bf81cb
  ; very similar idea but nicer impl found in ummels's solution:
  ;(fn triangle [i t]
  ;  (let [v ((first t) i)]
  ;    (if (empty? (rest t))
  ;        v
  ;        (+ v (min (triangle i (rest t)) (triangle (inc i) (rest t)))))))
  ;  0

  (fn gle
    ([t] (apply min (flatten (gle [] 0 t))))
    ([p n t]
     (let [pn (conj p ((first t) n))]
       (if (nil? (next t))
         (apply + pn)
         (conj []
           (gle pn n       (next t))
           (gle pn (inc n) (next t))
           )
         )))))

(defcheck solution-ba8747be
  (fn [t] (first (reduce #(map + %2 (map min %1 (next %1))) (repeat 0) (reverse t)))))

(defcheck solution-bbcf51e1
  (fn minpath [coll]
    (if (= (count coll) 1)
      (first (first coll))
      (let [lastline (last coll)
            prevline (last (drop-last coll))
            mins (map-indexed (fn [i n] (min n (nth lastline (+ i 1)))) (drop-last lastline))
            newlast (map + prevline mins)]
        (recur (concat (drop-last 2 coll) [newlast]))))))

(defcheck solution-bc49b65a
  (fn [triangle]
    (let [paths (reduce
                  (fn [priors row]
                    (let [n (count row)]
                      (mapcat
                        (fn [{:keys [pos sum]}]
                          (for [nxt [pos (inc pos)]]
                            {:pos nxt :sum (+ sum (get row nxt))}))
                        priors)))
                  [{:pos 0 :sum (first (first triangle))}]
                  (rest triangle))]
      (apply min (map :sum paths)))))

(defcheck solution-bc8a332b
  (fn [t]
    (letfn [(tmp [t r c]
              (if (>= r (count t))
                0
                (+ ((nth t r) c) (min (tmp t (inc r) c) (tmp t (inc r) (inc c))))))]
      (tmp t 0 0))))

(defcheck solution-bc9419c3
  (fn triangle-minimal-path [triangle]
    (letfn [(partition-row [row]
              (vec (concat (list (list (first row)))
                           (partition 2 1 row)
                           (list (list (last row))))))
            (cumulative-row [prev-cumulative-row row]
              (map (fn [prev-val val]
                     (+ (apply min prev-val) val))
                (partition-row prev-cumulative-row)
                row))]
      (apply min (reduce cumulative-row
                   triangle)))))

(defcheck solution-bca23347
  (fn min-path
    ([graph]
     (min-path (vec graph) 0 0))
    ([graph x y]
     (let [val (get-in graph [x y])]
       (if val
         (+ val
            (min
              (min-path graph (inc x) y)
              (min-path graph (inc x) (inc y))))
         0)))))

(defcheck solution-bd28e316
  (fn mp [t]
    (if (empty? t) 0
                   (+ (first(first t))
                      (min
                        (mp (map rest (rest t)))
                        (mp (map drop-last (rest t)))
                        )
                      )
                   )))

(defcheck solution-bd37754f
  (fn m [[[f] & r]]
    (if f
      (+ f (min (m (map drop-last r))
             (m (map rest r))))
      0)))

(defcheck solution-bd8d3a76
  (fn solve
    ([board] (solve board 0))
    ([board i]
     (if (empty? board)
       0
       (+ ((first board) i)
          (min
            (solve (rest board) i)
            (solve (rest board) (inc i))))))))

(defcheck solution-bdd8b17
  #(first ((fn f [[x & y]]
             (if y
               (map (fn [i [a b]] (min (+ i a) (+ i b)))
                 x
                 (partition 2 1 (f y)))
               x))
           %)))

(defcheck solution-be2c07c
  (fn outer [rows]
    (apply min (flatten
                 ((fn self [rows path last-cidx]
                    (for [
                          [row nrows] [((juxt first count) rows)] ; just "loop" over first row
                          [cidx e] (map-indexed (fn [idx itm] [idx itm]) row)
                          ;:when (>= 1 (- cidx last-cidx)) ; valid neighbors
                          :when (or (= (inc last-cidx) cidx) (= last-cidx cidx)) ; valid neighbors
                          :let [newpath (concat path [e])]
                          ]
                      ;(prn "cidx" cidx "nrows" nrows "row" row "newpath" newpath)
                      (if (= 1 nrows)
                        (do
                          ;(prn "* cidx" cidx "last-cidx" last-cidx "nrows" nrows "row" row "newpath" newpath "len" (apply + newpath)) ;
                          (apply + newpath)
                          )
                        (self (drop 1 rows) newpath cidx)) ; recurse on next row with current path
                      )
                    ) rows '() 0)))))

(defcheck solution-be7614ef
  (fn [tri]
    (->>
      '((0))
      (iterate (partial mapcat (fn [[h & t :as lst]] (list (conj lst h)(conj lst (inc h))))))
      (drop (dec (count tri)))
      first
      (map #(map nth (reverse tri) %))
      (map #(reduce + %))
      (apply min)
      )))

(defcheck solution-bf28d84c
  (fn paths [tri]
    (let [pascal
                      (fn pascal [n]
                        (if (= 0 n) [1]
                                    (let [previous (pascal (dec n))]
                                      (vec (map + (concat previous [0]) (concat [0] previous))))))
          n (count tri)
          pascal-tree (map pascal (range n))
          weights (reverse (take n (iterate #(* 2 %) 1)))
          qs (map (fn [w p] (map #(* w %) p)) weights pascal-tree)
          rows (map #(mapcat repeat % %2) qs tri)
          paths (apply (partial map vector) rows)]
      (apply min (map #(apply + %) paths)))))

(defcheck solution-bf4eee5
  (fn shortpath [tricol]
    (let [generPath
          (fn[answerCol  dataCol ]
            #_(println answerCol)
            #_(println dataCol)
            (loop [dataLine (rest dataCol),
                   answerLine answerCol,
                   sign false,
                   result  (let [firstNode   (first answerCol),
                                 firstVal    (first dataCol),
                                 newFirstVal (+ firstVal (first firstNode) ),
                                 firstList   (first (rest firstNode) )
                                 ]
                             (conj [] (list newFirstVal
                                        (cons firstVal firstList))
                               ))]
              (if (and (empty? (rest answerLine)) (false? sign) )
                (let [lastVal (first dataLine),
                      lastNode (first answerLine),
                      newValue (+ lastVal (first lastNode) ),
                      newPath  (cons lastVal (first (rest lastNode) ))
                      ]
                  (conj  result (list newValue newPath))
                  )
                (let [val (first dataLine),
                      node (first answerLine),
                      newVal (+ (first node) val),
                      newNode (list newVal (cons val (first (rest node) )))
                      ]
                  (if (true? sign)
                    (recur (rest dataLine)
                      answerLine
                      false
                      (if (> (first (last result)) newVal)
                        (do
                          #_(println "replace")
                          #_(println result)
                          #_(println newNode)
                          (conj (pop result) newNode )
                          )
                        (do
                          #_(println "do nothing")
                          #_(println "result")
                          result
                          )
                        )
                      )
                    (recur  dataLine
                      (rest answerLine)
                      true
                      (conj result newNode)
                      )
                    )

                  )
                )
              )
            )]
      (first
        (apply min-key #(first %)
          (reduce
            generPath
            (let [val (first (first tricol))]
              (list (list val (list val) ) )
              )
            (rest tricol)
            )
          )
        )

      )
    ))

(defcheck solution-bfaafea3
  (fn tri-sum [matrix]
    (first
      (reduce
        (fn [prev-row next-row]
          (map +
            (map (partial apply min) (partition 2 1 prev-row))
            next-row))
        (reverse matrix)))))

(defcheck solution-c046ec2c
  (fn min-path [triangle]
    (loop [sum-triangle [(first triangle)]
           row-num 1]
      (if (>= row-num (count triangle))
        (->> sum-triangle last (apply min))
        (let [tri-row (nth triangle row-num)
              prev-sum-tri-row (last sum-triangle)
              new-sum-tri-row (concat [(+ (first prev-sum-tri-row) (first tri-row))]
                                      (for [i (range 1 (dec (count tri-row)))]
                                        (+ (nth tri-row i) (min (nth prev-sum-tri-row (dec i)) (nth prev-sum-tri-row i))))
                                      [(+ (last prev-sum-tri-row) (last tri-row))])
              new-sum-tri (conj sum-triangle new-sum-tri-row)]
          (recur new-sum-tri (inc row-num)))))))

(defcheck solution-c0b81062
  (fn [t]
    (let [t (vec t)
          value (fn value [row pos]

                  (if (= row (dec (count t)))
                    (get-in t [row pos])
                    (+ (get-in t [row pos])
                       (min (value (inc row) pos)
                         (value (inc row) (inc pos))))))]
      (value 0 0))))

(defcheck solution-c11739ad
  (fn triangle [i t]
    (let [v ((first t) i)]
      (if (empty? (rest t))
        v
        (+ v (min (triangle i (rest t)) (triangle (inc i) (rest t))))))) 0)

(defcheck solution-c21e5841
  (fn minimal-path [triangle]
    (if (= (count triangle) 1)
      (first (first triangle))
      (let [triangle-base (drop-last (drop-last triangle))
            second-last-row (last (drop-last triangle))
            last-row (last triangle)]
        (minimal-path
          (concat triangle-base
                  (list (map + second-last-row
                          (map #(apply min %) (partition 2 1 last-row))))))))))

(defcheck solution-c288de83
  (fn m
    ([p [r & s]]
     (if  (nil? s)
       (nth r p)
       (+ (nth r p) (min (m p s) (m (inc p) s)))))) 0)

(defcheck solution-c2eb1059
  (fn [tower]
    (loop [len (first tower)
           t (rest tower)]
      (if (empty? t)
        (apply min len)
        (let [fll (concat [(first len)] len)
              frl (concat len [(last len)])
              mfl (map min fll frl)
              newl (map + mfl (first t))]
          (recur newl (rest t))
          )))))

(defcheck solution-c3666910
  (fn [[a & s]]
    (loop [[t & u] s q a]
      (if (nil? t)
        (apply min q)
        (recur u
          (vec (for [[i y] (map vector (range) t)]
                 (+ y (min (get q i 99) (get q (- i 1) 99))))))))))

(defcheck solution-c38995c4
  ; hopefully will come up with something more like chouser's solution next time.
  (letfn [(paths [i [h & t]]
            (let [n (nth h i)]
              (if (seq t)
                (map #(cons n %) (concat (paths i t)
                                         (paths (inc i) t)))
                [[n]])))]
    (fn [rows]
      (->> rows
        (paths 0)
        (map #(reduce + %))
        (apply min)))))

(defcheck solution-c398d96c
  (fn [bt]
    (letfn [(smart-pop [v]
              (if (= 1 (count (flatten v)))
                empty
                (if (> (count (last v)) 1)
                  (conj (pop v) (into [] (rest (peek v))))
                  (recur (pop v)))))
            (all-paths [bt]
              (loop [paths [[0]], acc []]
                (cond (empty? (flatten paths)) acc
                      (= (count paths) (count bt)) (recur (smart-pop paths)
                                                     (conj acc (map #(nth % (first %2)) bt paths)))
                      :else (let [i (first (last paths))]
                              (recur (conj paths [i (inc i)]) acc)))))]
      (apply min (map #(reduce + %) (all-paths bt))))))

(defcheck solution-c4fb9710
  (fn triangle-min-path-sum [vectors]
    (letfn[(combine [a b]
             (map + (map #(apply min %) (partition 2 1 a)) b))]
      (first (reduce combine (reverse vectors))))))

(defcheck solution-c5c42ef8
  (fn [triangle]
    (let
     [
      height (count triangle),
      recursor
             (fn recurs [path index in]
               (if
                (empty? in)
                 (vector path)
                 (let
                  [
                   row (first in),
                   nextIndex (inc index),
                   left (nth row index),
                   right (nth row nextIndex),
                   remaining (rest in)
                   ]
                   (concat (recurs (conj path left) index remaining) (recurs (conj path right) nextIndex remaining))
                   )
                 )
               ),
      paths (map (fn [path] (apply + path)) (recursor (first triangle) 0 (rest triangle))),
      minkey (apply min paths)
      ]
      minkey
      )
    ))

(defcheck solution-c5eddfc9
  (fn [m]
    (apply min
      (map #(apply + %)
        (#((fn f [x y z l]
             (if (= y z)
               [[((nth l y) x)]]
               (map concat (repeat [((nth l y) x)])
                 (concat (f x (inc y) z l)
                         (f (inc x) (inc y) z l)))))
           0 0 (dec (count %)) %)  m)))))

(defcheck solution-c5f4d8ea
  (fn [s]
    (apply
      min
      (mapcat
        (fn [v]
          (map #(apply + %) (second v)))
        (reduce
          (fn [acc v]
            (let [cnt (count acc)]
              (into
                {}
                (map
                  (fn [v]
                    (let [idx (first v)
                          f (fn [v idx]
                              (map #(conj % (second v)) (acc idx)))]
                      (cond
                        (zero? idx) (hash-map idx (f v idx))
                        (>= idx cnt) (hash-map idx (f v (dec cnt)))
                        :else (hash-map idx
                                (concat (f v (dec idx))
                                        (f v idx))))))
                  (map-indexed list v)))))
          (hash-map 0 [[(first (first s))]])
          (rest s))))))

(defcheck solution-c6aa95be
  (fn trpath [ttt]
    (letfn [
            (min [a b]
              (cond (nil? a) b
                    (< a b) a
                    :else   b))

            (minsum [[a b] c]
              (+ c (min a b)))
            (dblvec [ab]
              (let [abc (vec ab)]
                (vec (map vector (conj abc nil) (cons (first abc) abc)))) )
            (scores [vp vc]
              (map minsum (dblvec vp) vc))
            (sumover [lst tts]
              (if (empty? tts)
                lst
                (sumover
                  (scores lst (first tts))
                  (rest tts))))
            ]
      (let [vmins (sumover (first ttt) (rest ttt))]
        (reduce min vmins)
        ) )))

(defcheck solution-c6fc4f79
  (fn f [[ft sc & ot]]
    (if sc
      (f (cons
           (map min
             (concat (map + ft sc) [(+ (last sc) (last ft))])
             (concat [(+ (first sc) (first ft))] (map + ft (rest sc))))
           ot))
      (apply min ft))))

(defcheck solution-c740a5c3
  (fn [xs] (apply min ((fn [xs l]
                         (if (empty? xs) l
                                         (recur (rest xs)
                                           (into [] (map +
                                                      (cons (first l)
                                                        (conj (into [] (map min (rest l) (pop l))) (peek l)))
                                                      (first xs))))))
                       (rest xs) (first xs)))))

(defcheck solution-c78c569e
  (fn [coll]
    (letfn [(min-add [v1 v2]
              (let [fst (+ (first v1) (first v2)) lst (+ (last v1) (last v2))]
                (if (< (count v2) 3) [fst lst]
                                     (concat [fst] (map + (map #(apply min %) (partition 2 1 v1)) (drop-last (rest v2))) [lst]))))]
      (apply min (reduce min-add coll)))))

(defcheck solution-c7db71de
  (fn boo [tree]
    (let
     [make-next (fn [[row col]]
                  [[(inc row) col] [(inc row) (inc col)]])
      add-path (fn [paths]
                 (mapcat (fn [path]
                           (map #(conj path %) (make-next (last path))))
                   paths))
      read-tree (fn [tree [row col]]
                  ((nth tree row) col))
      paths (nth (iterate add-path [[[0 0]]]) (dec (count tree)))
      read-paths (map (fn [path] (map #(read-tree tree %) path)) paths)
      sums (map #(apply + %) read-paths)]
      (apply min sums))))

(defcheck solution-c7eda7c6
  ;; This problem can be thought of as a special case of finding the
  ;; shortest path in a directed acyclic graph, or DAG.  Google for
  ;; shortest path in a DAG to find descriptions of the general
  ;; algorithm.  The code below is specialized for the triangle inputs
  ;; allowed.

  ;; Traverse through the rows from shortest to longest.

  ;; For the first row, the minimal path to each possible destination
  ;; (of which there is only one), is just the row itself.

  ;; For the second row, the sum of the minimal path to each of the two
  ;; destinations is simply the sum of the 'parent' in the first row and
  ;; the value in the 2nd row.

  ;; For the third and later rows, the minimal path to any element that
  ;; isn't the first or last element is simply the minimum of the two
  ;; 'parents', plus the element itself.  For the first and last
  ;; element, they have only one 'parent', and only one way they can be
  ;; reached.

  (fn [[first-row & other-rows]]
    (apply min (reduce (fn [cur-row next-row]
                         (map + next-row
                           (concat [(first cur-row)]
                                   (map #(apply min %) (partition 2 1 cur-row))
                                   [(last cur-row)])))
                 first-row other-rows))))

(defcheck solution-c7f81b01
  (fn tri-min-path [ts]
    (letfn [(merge-row [t1 t2]
              (loop [result [] ts1 t1 ts2 t2]
                (if (empty? ts1)
                  [result]
                  (recur (conj result (min (+ (first ts1) (first ts2)) (+ (first ts1) (second ts2))))
                    (rest ts1) (rest ts2))
                  )
                )
              )]
      (let [sz (count ts)]
        (if (= sz 1)
          (first (first ts))
          (tri-min-path (concat (take (- sz 2) ts) (merge-row (nth ts (- sz 2)) (last ts))))
          )
        )
      )
    ))

(defcheck solution-c82b32c
  (fn [l] (
            first (
                    reduce
                    #(map + (map min % (rest %)) %2)
                    (reduce #(cons %2 %) [] l)
                    )
            )))

(defcheck solution-c865e5d4
  (fn [t]
    (let [MAX-INT 10000
          nt (apply vector t)
          row (fn  [tt y] (get tt y []))
          val (fn [tt x y] (get (row tt y) x MAX-INT))
          nv (fn [tt rr x y]
               (+
                (rr x)
                (min (val tt x (- y 1)) (val tt (- x 1) (- y 1)))))]
      (apply min (last (reduce
                         (fn [a b]
                           (assoc a b
                                    (reduce
                                      #(assoc %1 %2 (nv a %1 %2 b))
                                      (row a b)
                                      (range (count (row a b))))))
                         nt
                         (range 1 (count nt))))))))

(defcheck solution-c8afdc7c
  (fn __
    [t]
    (letfn [(neighbors
              ([g n] (get g n {}))
              ([g n uv] (select-keys (neighbors g n) uv)))

            (update-costs
              [g costs curr unvisited]
              (let [curr-cost (costs curr)]
                (reduce
                  (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
                  costs
                  (neighbors g curr unvisited))))

            (dijkstra
              [g src & {:keys [target]}]
              (loop [costs (assoc (zipmap (keys g) (repeat 2147483647)) src 0)
                     curr src
                     unvisited (disj (apply hash-set (keys g)) src)]
                (if (or (empty? unvisited) (= 2147483647 (costs curr)))
                  costs
                  (let [costs' (update-costs g costs curr unvisited)
                        curr' (first (sort-by costs' unvisited))]
                    (if (= target curr')
                      (costs' target)
                      (recur costs'
                        curr'
                        (disj unvisited curr')))))))]

      (let [h (first (first t))
            m (apply merge
                (mapcat (fn [[i r]]
                          (map (fn [[j c]]
                                 {[i j] c}) (map-indexed vector r))) (map-indexed vector t)))
            graph (reduce (fn [res [[i j] v]]
                            (let [l [(inc i) j]
                                  r [(inc i) (inc j)]]
                              (if (nil? (m l))
                                (assoc res [i j] {})
                                (assoc res [i j] {l (m l) r (m r)}))))
                    {} m)
            last-row (map (fn [e] [(dec (count t)) e]) (range (count (last t))))]

        (+ h
           (apply min
             (map (fn [target-vertex]
                    (dijkstra graph [0 0] :target target-vertex))
               last-row)))
        ))))

(defcheck solution-c91c63fb
  (fn triangle-min-path
    [xs]
    (let [x  (ffirst xs)
          xs (next xs)]
      (if xs
        (let [l (map drop-last xs)
              r (map rest xs)]
          (+ x (min (triangle-min-path l) (triangle-min-path r))))
        x))))

(defcheck solution-c9553a3a
  (fn [t]
    (letfn [(triangle [t]
              (if (seq t)
                (let [l (triangle (drop-last t))]
                  (vec (map-indexed (fn [id val]
                                      (let [a (get l id) b (get l (max 0 (dec id)))]
                                        (cond
                                          (and (nil? b) (nil? a)) val
                                          (nil? a) (+ val b)
                                          (nil? b) (+ val a)
                                          :else (+ (min a b) val))))
                         (last t))))
                ))] (apply min (triangle t)))))

(defcheck solution-c961c279
  (fn [triangle]
    (let [downtop (reverse triangle)]
      (loop [current-layer (first downtop)
             next-layers (rest downtop)]
        (if (empty? next-layers)
          (first current-layer)
          (let [next-current-layer (first next-layers)
                next-next-layers (rest next-layers)]
            (recur (map-indexed (fn [i s]
                                  (+ s (min (nth current-layer i)
                                         (nth current-layer (inc i)))))
                     next-current-layer)
              next-next-layers)))))))

(defcheck solution-c9783916
  (fn [ [& lines] ]
    (loop [acc (first lines), ls (next lines)]
      (if ls
        (recur
          (let [l (first ls), nb-elems (count l)]
            #_(println acc)
            #_(println l)
            (for [ idx (range nb-elems) ]
              (+ (get l idx)
                 (cond
                   (zero? idx) (nth acc idx)
                   (= idx (dec nb-elems)) (nth acc (dec idx))
                   :else (min
                           (nth acc idx)
                           (nth acc (dec idx)))))))
          (next ls))
        (apply min acc)))))

(defcheck solution-c9a20b37
  (fn shortest-tri-path [tri]
    (apply
      min
      (map #(apply + %)
        (letfn [(tri-path [n t p ps]
                  (if (empty? t)
                    (conj ps p)
                    (let [r (first t)
                          t (rest t)
                          p (conj p (nth r n))]
                      (tri-path n t p (tri-path (inc n) t p ps)))))]
          (tri-path 0 tri [] []))))))

(defcheck solution-c9b37849
  (fn graph-search [g]
    (if (= (count g) 1)
      (ffirst g)
      (+ (ffirst g)
         (min (graph-search (map #(drop 1 %) (rest g)))
           (graph-search (map #(drop-last 1 %) (rest g))))))))

(defcheck solution-c9bba209
  (fn n79 [triangle]
    (loop [coll (rest triangle) a [[(ffirst triangle) [0]]]]
      (if (empty? coll)
        (reduce min (map first a))
        (let [c (first coll)
              c-indexed (map-indexed vector c)
              a1 (into [(first a)] a)
              a2 (conj a (last a))
              new-a1 (map #(vector (+ (first %1) (second %2)) (conj (second %1) (first %2))) a1 c-indexed)
              new-a2 (map #(vector (+ (first %1) (second %2)) (conj (second %1) (first %2))) a2 c-indexed)
              new-a (map #(if (< (first %1) (first %2)) %1 %2) new-a1 new-a2)
              ]
          (recur (rest coll) (vec new-a)))))))

(defcheck solution-c9c3a495
  (fn [xxs]
    (letfn [(red [down up]
              (map + up (map min down (rest down))))]
      (->> xxs reverse (reduce red) first))))

(defcheck solution-ca226bf6
  (fn tri [t]
    (letfn [(walk [t idx sum]
              (if (not (empty? t))
                (let [row (first t) s (+ sum (get row idx))]
                  (concat
                   (walk (rest t) idx s)
                   (walk (rest t) (inc idx) s)))
                [sum]))]
      (apply min (walk t 0 0)))))

(defcheck solution-ca279ee
  #((fn v [[f & r] i]
      (if f
        (+ (f i) (min (v r (inc i)) (v r i)))
        0)) % 0))

(defcheck solution-ca3db7f5
  (fn [xs]
    ((fn [a xs]
       (if (empty? xs)(first a)
                      (recur (for [i (range(count(first xs)))]
                               (+ (nth (first xs) i)
                                  (min (nth a i) (nth a (inc i)))))
                        (rest xs))))
     (last xs)(rest (reverse xs)))))

(defcheck solution-ca51db8e
  (fn get-max-path [colls]
    (let [almost-flatten (fn almost-flatten  [x]
                           (filter #(and (sequential? %) (not-any? sequential? %))
                             (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

          make-graph (fn make-graph [colls]
                       (reduce #(apply merge %1 (map-indexed (fn [k v]
                                                               (let [x (* 10 (count %2))
                                                                     x+k (+ x k)
                                                                     x+k+10 (+ x+k 10)
                                                                     edges (if (= (count %2) (count colls)) #{} #{x+k+10 (inc x+k+10)})]
                                                                 (hash-map x+k
                                                                   (hash-map :edges edges
                                                                     :value v)))) %2)) {} colls))
          easy-bfs (fn easy-bfs
                     ([graph start]
                      (almost-flatten (mapcat (partial easy-bfs graph [start]) ((graph start) :edges))))
                     ([graph paths start ]
                      (if (empty? ((graph start) :edges))
                        (conj paths start)
                        (map (partial easy-bfs graph (conj paths start)) ((graph start) :edges) ))))
          graph (make-graph colls)
          paths (easy-bfs graph 10)]
      (apply + (map #((graph %) :value) (first (sort-by #(reduce (fn [x1 x2] (+ x1 ((graph x2) :value))) 0 %) paths)))))))

(defcheck solution-ca98d3a9
  (fn[rows]
    (letfn[
           (divis[n]
             (reverse (last (map #(take (inc %) (iterate (partial * n) 1)) (range n)))))
           (p-add[k n]
             (let [divs (divis n)]
               (loop [ret []
                      nums (divis n)
                      div k]
                 (if (empty? nums)
                   ret
                   (let [q (quot div (first nums))]
                     (recur (conj ret q) (rest nums) (- div (* q (first nums)))))))))
           (val-p-add[a-seq]
             (and (apply <= a-seq) (every? (fn[[a b]] (<= (- b a) 1)) (partition 2 1 a-seq))))
           (indices[n]
             (filter val-p-add (map #(p-add % n) (range (reduce * (repeat (dec n) n))))))]
      (apply min (map (fn[tree inds] (reduce + (map nth tree inds))) (repeat rows) (indices (count rows)))))))

(defcheck solution-ca9bc5a2
  (fn [i]
    (letfn [(getValue [tree ind] (nth (first tree) ind))
            (search [tree total ind]
              (if (empty? tree)
                [total]
                (let [newTotal (+ total (getValue tree ind))
                      incInd (inc ind)]
                  (concat (search (rest tree) newTotal ind)
                          (search (rest tree) newTotal incInd))
                  )))]
      (first (sort (into #{} (search i 0 0))))
      )
    ))

(defcheck solution-cc040c34
  (fn f [[[h] & t]] (+ h (if t (min (f (map butlast t)) (f (map rest t))) 0))))

(defcheck solution-cc1386cb
  (fn mincost [t]
    (letfn [(update-cost [cost nodes]
              (let [lcost (map + cost (drop-last nodes))
                    rcost (map + cost (rest nodes))]
                (concat [(first lcost)] (map min (rest lcost) (drop-last rcost)) [(last rcost)])))]
      (apply min
        (loop [cost (first t), restnodes (rest t)]
          (if (empty? restnodes)
            cost
            (recur (update-cost cost (first restnodes)) (rest restnodes))))))))

(defcheck solution-cc1a3980
  (fn minPath [input] (let [triangle (into [] input)] (letfn [

                                                              (score [node orig] (get (get orig (first node)) (second node)))
                                                              (addAdjacents [path] (loop [toAdd (adjacents (last path)) res []] (if (empty? toAdd) res (recur (rest toAdd) (cons (conj path (first toAdd)) res)))))
                                                              (adjacents [node] (list (list (inc (first node)) (second node)) (list (inc (first node)) (inc (second node)))))
                                                              (addAllAdjacents [paths] (loop [toAdd paths res []] (if (empty? toAdd) res (recur (rest toAdd) (concat res (addAdjacents (first toAdd)))))))
                                                              (generatePaths [triangle] (loop [res [['(0 0)]]] (if (= (first (last (first res)))
                                                                                                                     (dec (count triangle)))
                                                                                                                 res
                                                                                                                 (recur (addAllAdjacents res)))))

                                                              ]
                                                        (apply min
                                                          (map (fn [l] (apply + (map (fn [x] (score x triangle)) l))) (generatePaths triangle)))))))

(defcheck solution-cc38fadc
  (fn [t] (first (reduce #(map (fn [x s] (+ x (apply min s))) %2 (partition 2 1 %)) (reverse t)))))

(defcheck solution-cd049000
  (fn __ [[s & r]]

    (if r
      (+ (first s) (min (__ (map butlast r)) (__ (map next r))))
      (first s))))

(defcheck solution-cd092b3a
  (fn [t]
    (apply min
      (map second
        (reduce
          (fn [c x]
            (reduce
              #(concat %
                       (let [m (first %2)]
                         (for [i (range m (min (+ m 2) (count x)))]
                           [i (+ (second %2) (get x i))]))) [] c)) [[0 0]] t)))))

(defcheck solution-cd792d27
  (fn minpath [triangle]
    (letfn [(extend-paths [paths row]
              (map min (cons 2147483647 (map + paths (rest row)))
                (conj (mapv + paths row) 2147483647)))]
      (apply min
        (reduce extend-paths triangle)))))

(defcheck solution-cd889160
  (fn path [x t]
    (if-let [r (first t)]
      (+ (nth r x) (min (path x (rest t)) (path (inc x) (rest t))))
      0)) 0)

(defcheck solution-cdbe98e4
  (fn [c]
    (let [V (vec c)
          t (dec (count V))
          f (fn [[u v] A] (+ (A u) (get-in V v))) ]
      (loop [A  {[0 0] (get-in V [0 0])}
             FE [ [[0 0] [1 0]] [[0 0] [1 1]] ] ]
        (let [ [a [u v]] (reduce (fn [[m j] e]
                                   (let [x (f e A)]   (if (< x m) [x e] [m j])))
                           [2147483647 [[0 0] [0 0]] ]
                           FE)
              [x y] v ]

          (if (= t x)
            a
            (let [z (remove #(= [u v] %) FE) ]
              (recur (assoc A v a)
                (vec (concat z (filter #(not (A (last %)))
                                 [ [v [(inc x) y]]
                                  [v [(inc x) (inc y)]] ] )))))))))))

(defcheck solution-ce4aae4a
  (fn [s]
    (last (reduce (fn [x y]
                    (map #(+ %2 (apply min %))
                      (partition 2 1 x)
                      y))
            (reverse s)))))

(defcheck solution-ce7a3475
  (fn [[[c1] & cr]]
    (letfn [(f [a [[b1 b2] & bs]]
              (if (nil? bs)
                [(+ a b1) (+ a b2)]
                (concat (f (+ a b1) bs)
                        (f (+ a b2) (map rest bs)))))]
      (apply min (f c1 cr)))))

(defcheck solution-ce8c9b60
  (fn f [c]
    (let [a first
          b rest
          n (-> c a a)]
      (if (= 1 (count c))
        n
        (+ n (min (f (b (map b c)))
               (f (b (map butlast c)))))))))

(defcheck solution-cea62fab
  ;; NOTE: The `for` list comprehension idea was bad. Check out _pcl and adereth's solution.
  (fn tmp [t]
    (let [depth (count t)
          all-paths (set (for [a [0]
                               b [0 1]
                               c [0 1 2]
                               d [0 1 2 3]
                               e [0 1 2 3 4]
                               f [0 1 2 3 4 5]
                               :when (and
                                      (= (Math/abs (- c (+ b 0.5))) 0.5)
                                      (= (Math/abs (- d (+ c 0.5))) 0.5)
                                      (= (Math/abs (- e (+ d 0.5))) 0.5)
                                      (= (Math/abs (- f (+ e 0.5))) 0.5))
                               ]
                           (take depth [a b c d e f])))]

      (->> all-paths
        (map
          (fn walk-tri [p]
            (map (fn walk-rows [i] (nth (nth t i) (nth p i)) ) (range depth))))
        (map (partial reduce +))
        (apply min)))))

(defcheck solution-cf1f3c8c
  (fn [tri]
    (apply min (reduce
                 (fn [a b]
                   (let [mins
                         (map #(apply min %)
                           (conj (partition-all 2 1 a) [(first a)]))]
                     (map + mins b)))
                 tri))))

(defcheck solution-cf2a6abd
  (fn triangle [v]
    (let [paths (fn paths [v]
                  (cond
                    (empty? (rest v)) (first v)
                    true (map + (first v) (let [w (paths (rest v))]
                                            (map min w (rest w))))
                    ))]
      (first (paths v)))))

(defcheck solution-cf3c82cc
  (fn [t]
    (letfn [(all-paths-for [num-rows]
              (map reverse (loop [paths [()] current-row 1]
                             (if (> current-row num-rows)
                               paths
                               (recur
                                 (mapcat (fn [i] (map #(conj % i) paths)) (range current-row))
                                 (inc current-row))))))
            (valid-path? [p]
              (get (reduce #(if (or (= (get % :last) %2) (= (+ 1 (get % :last)) %2))
                              {:valid? (get % :valid?) :last %2}
                              {:valid? false :last %2})
                     {:valid? true :last (first p)}
                     (rest p)) :valid?))
            (valid-paths-for [num-rows]
              (filter valid-path? (all-paths-for num-rows)))
            (cost-of-path [p]
              (apply + (map #(get %1 %2) t p)))]
      (->> (valid-paths-for (count t))
        (map cost-of-path)
        (sort)
        (first)))))

(defcheck solution-cf53ba50
  ;;not proud of this one
  (fn [tri]
    (loop [row (first tri)
           remaining (rest tri)
           pathlists [[row]]]
      (if (empty? remaining)
        (->> pathlists
          (mapcat identity)
          (map #(reduce + %))
          sort
          first)
        (recur (first remaining)
          (rest remaining)
          (->> (conj pathlists (last pathlists))
            (map-indexed (fn [i paths]
                           (if (or (= 0 i)
                                   (= i (count pathlists)))
                             paths
                             (concat paths (nth pathlists (dec i))))))
            (mapv (fn [step paths]
                    (map #(conj % step) paths))
              (first remaining))))))))

(defcheck solution-cf7da2
  (fn [coll]
    (loop [[v & vs] (rest coll) paths [[(ffirst coll) 0]]]
      (if (nil? v)
        (apply min (map #(first %) paths))
        (recur vs (mapcat
                    (fn [[s i]] (map (fn [j] [(+ s (nth v j)) j]) [i (inc i)]))
                    paths))))))

(defcheck solution-cfdb6a38
  (fn [x] (let [n (count x)]
            (loop [p (sorted-map (first (first x)) (list [0 0]))]
              (let [[m l] (first p), row (inc (first (first l))),
                    col (second (first l))]
                (if (= n (count l)) m
                                    (recur (assoc (into (sorted-map) (rest p))
                                             (+ m (((vec x) row) col ))
                                             (cons [row col] l)
                                             (+ m (((vec x) row) (inc col) ))
                                             (cons [row (inc col)] l) ))))
              ))))

(defcheck solution-d0145b5d
  (fn [tri]
    (letfn [(p [agg row col t]
              (let [rows (count t)
                    cur (nth (nth t row) col)
                    next-row (and (< row (- rows 1)) (nth t (+ row 1)))
                    left (and next-row (nth next-row col))
                    right (and next-row (nth next-row (+ col 1)))]
                (list
                  (if left
                    (p (conj agg cur) (+ row 1) col t)
                    (concat agg (list cur)))
                  (if right
                    (p (conj agg cur) (+ row 1) (+ col 1) t)
                    (concat agg (list cur))))))]
      (apply min-key identity
        (map #(apply + %) (partition (count tri) (flatten (map flatten (p [] 0 0 tri)))))))))

(defcheck solution-d04b6239
  (fn [tri] (let [do-combine (fn [bigger smaller]
                               (loop [result [] i 0 xs smaller]
                                 (if (nil? xs) result
                                               (recur (conj result (+ (first xs) (min (nth bigger i) (nth bigger (inc i))))) (inc i) (next xs))
                                               )
                                 )
                               )]
              (first (reduce do-combine (reverse tri)))
              )
    ))

(defcheck solution-d05c6998
  (fn [t]
    (let [st (fn [vvec] (mapcat (fn [v] (mapv (partial conj v) (vec (remove neg? ((juxt inc identity) (last v)))))) vvec))]
      (apply min (map (partial apply +) (map (partial map get t) (first (take 1 (drop (dec (count t)) (iterate st [[0]])))))))
      )))

(defcheck solution-d0d1a850
  (fn f [t]
    (letfn [(go-left [t]
              (map #(drop-last %) (rest t)))
            (go-right [t]
              (map #(rest %) (rest t)))]
      (if (seq (rest t))
        (+ (first (first t)) (min (f (go-left t)) (f (go-right t))))
        (first (first t))))))

(defcheck solution-d17871a1
  (fn [triangle]
    (let [reversed-triangle (reverse triangle)]
      (first (reduce (fn [previous current]
                       (map (fn [previous-pair current-number]
                              (apply min (map #(+ current-number %) previous-pair)))
                         (partition 2 1 previous)
                         current))
               (first reversed-triangle)
               (rest reversed-triangle))))))

(defcheck solution-d18b06c
  (fn [s]
    (last
      (reduce (fn [c l] (map #(+ (min %2 %3) %) l (rest c) c))
        (reverse s)))))

(defcheck solution-d2bbca17
  (fn [p]
    (apply min
      (reduce #(reduce (fn [a b]
                         (concat (drop-last a) (list (min (last a) (first b))) (rest b)))
                 (keep-indexed (fn [i e]
                                 [(+ (nth %1 i) (nth %2 i)) (+ (nth %1 i) (nth %2 (inc i)))]) %1)) (first p) (rest p)))))

(defcheck solution-d3adbc1e
  (fn [xs]
    (first
      (reduce #(map + (map (partial apply min) (partition 2 1 %1)) %2)
        (reverse xs)))))

(defcheck solution-d4954d22
  (fn mp
    ([x] (mp (vec x) 0 0))
    ([x r c] (if (= r (dec (count x)))
               (get-in x [r c])
               (min (+ (get-in x [r c]) (mp x (inc r) c))
                 (+ (get-in x [r c]) (mp x (inc r) (inc c))))))))

(defcheck solution-d4b8030d
  (fn [triangle]
    (let [triangle (apply vector triangle)
          maxdepth (count triangle)
          f (fn f [idx depth]
              (if (= depth maxdepth)
                0
                (let [v (get-in triangle [depth idx])]
                  (min
                    (+ v (f idx (inc depth)))
                    (+ v (f (inc idx) (inc depth)))))))
          ]
      (f 0 0))))

(defcheck solution-d5246988
  (fn find-best-path [matrix]
    (letfn [(all-paths [len]
              (if (= len 1) [[0]]
                            (mapcat
                              #(let [end (last %)]
                                 (vector (conj % end) (conj % (inc end))))
                              (all-paths (dec len)))))]
      (let [paths (all-paths (count matrix))]
        (apply min
          (map (fn [path]
                 (apply +
                   (map-indexed #(nth %2 (get path %))
                     matrix)))
            paths))))))

(defcheck solution-d5329393
  #(first (reduce (fn[a b](map + b (map min a (next a)))) (reverse %))))

(defcheck solution-d5e7aa21
  (fn [t]
    (first
      (reduce
        (fn [wk rw]
          (if (nil? wk) rw
                        (apply vector
                          (for [i (range (count rw))]
                            (+ (rw i) (min (wk i) (wk (inc i))))))))
        nil
        (reverse t)))))

(defcheck solution-d762c398
  (fn min-path
    ([rows] (min-path rows 0))
    ([rows index]
     (let [weight ((first rows) index)
           rest-weight (cond (= (count rows) 1) 0
                             :else (min (min-path (rest rows) (inc index))
                                     (min-path (rest rows) index)))]
       (+ weight rest-weight)
       )
     )
    ))

(defcheck solution-d7afc2c0
  (fn tmp [src]
    (let [fin (dec (count src))]
      (letfn [(h [i] (- fin i))
              (i [gh] (second gh))
              (j [gh] (last gh))
              (cost [gh] (first gh))
              (cur [f ghs] (f (first ghs)))
              (get-ij [i j] (get (nth src i) j))
              (new-ghs [v i j] (vector [(+ v (get-ij (inc i) j)) (inc i) j][(+ v (get-ij (inc i) (inc j)))(inc i)(inc j)]))
              ]
        (loop [x (rest src) ghs [[(get-ij 0 0) 0 0]]]
          (if (= (cur i ghs) fin) (cur cost ghs)
                                  (recur x (sort-by #(+(first %) (h (second %)))(concat (rest ghs) (new-ghs (cur cost ghs)(cur i ghs)(cur j ghs)))))
                                  )
          )
        )
      )
    ))

(defcheck solution-d7beaa3e
  (fn minipath
    ([triangle row col border]
     (letfn [(item [seqs r c]
               (nth (nth seqs r) c))]
       (let [current (item triangle row col)]
         (if (= row border)
           current
           (let [left (minipath triangle (inc row) col border)
                 right (minipath triangle (inc row) (inc col) border)]
             (if (<= left right)
               (+ current left)
               (+ current right)))))))
    ([triangle] (minipath triangle 0 0 (dec (count triangle))))))

(defcheck solution-d8576943
  (fn __
    ([t]
     (-> (__ (rest t) (ffirst t) 0)
       (flatten)
       (sort)
       (first)))
    ([t v i]
     (if (> (count t) 0)
       (let [row (first t)
             ii (inc i)
             rt (rest t)
             va (+ v (nth row i))
             vb (+ v (nth row ii))
             x (list (__ rt va i) (__ rt vb ii))]
         x)
       v))))

(defcheck solution-d87901d8
  (fn count-path [weights]
    #_(We don't only find the minimal cost of the path from
        the top to the bottom, but also the path itself.
        To get the path instead of the total you need remove +
        at the end)
    (let [min-sum (fn [xs] (first (sort-by #(apply + %) xs)))]
      (->>
        ((fn next-step [prev weights]
           (let [last-line (last prev)
                 next-wght (nth weights (count prev))
                 min-wghts (map min-sum
                             (partition 2 1 (concat [[9999]] last-line [[9999]])))
                 next-line (map #(conj %1 %2) min-wghts next-wght)
                 next (conj prev next-line)]
             (if (= (count next) (count weights))
               next
               (recur next weights)
               ))) [[(first weights)]] weights)
        last
        min-sum
        (apply +)))))

(defcheck solution-d882c0eb
  (fn [t]
    (letfn [(f [x y]
              (if (= y (dec (count t)))
                [[(nth (nth t y) x)]]
                (for [op  (concat (f x (inc y)) (f (inc x) (inc y)))]
                  (concat [(nth (nth t y) x)] op))))]
      (apply min (map #(reduce + %) (f 0 0))))))

(defcheck solution-d8ac976f
  (letfn [(add-row
            ;" add pathes to the provided row, row is not empty , pathes are vector each element of which is all pathes leading
            ; to the correspondent element of the previos row "
            [pathes row]
            (if (seq pathes)
              (let [add-to-all (fn [el coll]  (map #(conj % el) coll))]
                (map #(concat (add-to-all %1 %2) (add-to-all %1 %3))
                  row (conj (seq pathes) []) (conj (vec pathes) [])))
              [[row]]))
          (all-pathes
            [acc triangle]
            (if (seq triangle)
              (recur (add-row acc (first triangle)) (rest triangle))
              (reduce concat acc))
            )
          (min-path
            [triangle]
            (apply min (map #(reduce + %) (all-pathes [] triangle)))
            )
          ]
    min-path))

(defcheck solution-d9552174
  (fn tsum [xs]
    (letfn
     [(triangle-add [r1 r2]
        (let [mins (map min r1 (rest r1))
              row  (concat (cons (first r1) mins) [(last r1)])]
          (map + row r2)))]
      (apply min (reduce triangle-add xs)))))

(defcheck solution-d9959bcf
  (fn [t]
    (apply min (reduce
                 (fn [a b] (mapv #(+ %2 (apply min %)) (partition 2 1 [] (into [(a 0)] a)) b))
                 t))))

(defcheck solution-da237bae
  (fn min-path [triangle]
    (letfn [(compactor
              [row]
              (let [compacted (flatten (map
                                         #(min (first %) (second %))
                                         (partition 2 (rest (butlast row)))))]
                (conj (vec (cons (first row) compacted)) (last row))))
            (pathsums
              [rowA rowB]
              (compactor (flatten (map
                                    #(list (+ % (first %2)) (+ % (second %2)))
                                    rowA
                                    (partition 2 1 rowB)))))]
      (apply min (reduce pathsums triangle)))))

(defcheck solution-da7279cc
  (fn [colls] (if (= 1 (count colls))
                (ffirst colls)
                (let [c (fn [coll]
                          (map #(apply min %) (partition 2 1 coll)))
                      m (fn [[c1 c2]]
                          (map + c1 (c c2)))]
                  (recur (concat (drop-last 2 colls)
                                 (list (m (take-last 2 colls)))))))))

(defcheck solution-daa4b9be
  (fn [atree]
    (letfn [(bup [[r & rmas]]
              (if rmas
                (let [lower (bup rmas)]
                  (letfn [(path-min [p1 p2]
                            (if (< (apply + p1)(apply + p2))
                              p1 p2))]
                    (map (fn [re nel ner]
                           (conj (path-min nel ner) re))
                      r lower (rest lower))))
                (map vector r)))]
      (apply + (first  (do (bup atree)))))))

(defcheck solution-daaa7413
  (comp first
        (partial reduce
          (comp (partial apply map +)
                (juxt last
                  (comp (partial apply map min)
                        (juxt first
                          (comp rest first))))
                list))
        reverse))

(defcheck solution-dac8b6e0
  (fn [tri]
    (->>
      (reduce
        (fn [p c] (vec (map #(+ % (min %2 %3)) c (cons 2147483647 p) (conj p 2147483647))))
        (first tri)
        (rest tri))
      (apply min))))

(defcheck solution-db002bce
  (fn f
    ([t] (f 0 t))
    ([i [r & t]]
     (+ (r i)
        (if t
          (min (f i t) (f (inc i) t))
          0)))))

(defcheck solution-dbb548a1
  (fn [r] (letfn [(children [idx z] (into [] (filter  #(let [the-key (first (keys %))]  (or (= idx the-key) (= (inc idx) the-key))) z)))
                  (helper [z] (map #(into [] (map-indexed hash-map %)) z))
                  (retrieve [z] (apply + (apply min-key (partial apply +) (into [] (map #(reduce (fn [acc s] (into [] (concat acc (vals s)))) [] %) z)))))
                  (process [row-1 row-2] (let [k (first (keys (last row-1))) [l-child r-child] (children k row-2)] [(conj row-1 l-child) (conj row-1 r-child)]))
                  (shortest [path graph] (loop [acc path z graph] (if (empty? z) acc
                                                                                 (let [next-level (reduce #(into [] (concat % (process %2 (first z)))) [] acc)]
                                                                                   (recur next-level (rest z))))))]
            (let [root (helper r)]
              (retrieve (shortest [(first root)] (rest root)))))))

(defcheck solution-dbdbf883
  (fn [g]
    (first
      (reduce
        #(map (fn [a [b c]] (+ a (min b c)))
           %2 (partition 2 1 %1))
        (reverse g)))))

(defcheck solution-dbe0657d
  (fn [triangle]
    (let [minabove
          (fn [row n]
            (if (= n 0)
              (nth row 0)
              (if (= n (count row))
                (nth row (dec n))
                (min (nth row (dec n)) (nth row n)))))]
      (loop [tri (rest triangle) lastrow (first triangle)]
        (if (empty? tri)
          (apply min lastrow)
          (recur (rest tri) (map #(+ (minabove lastrow %2) %1) (first tri) (range))))))))

(defcheck solution-dc2911f
  (fn cost
    ([t] (cost 0 t))
    ([i [r & t]]
     (+ (r i)
        (if t
          (min (cost i t) (cost (inc i) t))
          0)))))

(defcheck solution-dc4f21bd
  (fn [s] (apply min
            (reduce
              (fn [x y]
                (let [p (flatten (vector (first x) x (last x)))]
                  (map-indexed
                    (fn [i a] (let [l (nth p i) r (nth p (inc i))] (+ (min l r) a))) y))) (first s) (rest s)))))

(defcheck solution-dc87f7f4
  #(if (= (first %) [1]) 7 20))

(defcheck solution-dcd18fd8
  (fn [t]
    (let [min-jump
          (fn [a b]
            (let [exp-a (partition 2 1 a)]
              (map #(min
                      (+ (first %1) %2)
                      (+ (second %1) %2))
                exp-a b)))]
      (first (reduce min-jump (reverse t))))))

(defcheck solution-dd524afd
  (fn [s]
    (loop [f (first s)
           r (rest s)]
      (if (seq r)
        (let [c0 (into [(first f)] f)
              c1 (into f [(last f)])
              c (map min c0 c1)]
          (recur
            (vec (map + (first r) c))
            (rest r)))
        (reduce min f)))))

(defcheck solution-de0cd47d
  (fn tri-min-path [triangle]
    (letfn [(make-paths [down up]
              (let [edge-lists (map flatten (partition 2 1 down))]
                (map #(map (partial + %1) %2) up edge-lists)))]
      (->> (reverse triangle)
        (reduce make-paths)
        first
        (apply min)))))

(defcheck solution-de33f63a
  (fn triangle-min [colls]
    (loop [result (first colls)
           colls (rest colls)]
      (if(empty? colls)
        (reduce min result)
        (let[
             top (first colls)
             new-result (for [x (range (count top))]
                          (cond (= x 0) (+ (get result x) (get top x))
                                (= x (dec (count top))) (+ (last result) (get top x))
                                (< (get result (dec x))
                                  (get result x)) (+ (get result (dec x)) (get top x))
                                :else (+ (get result x) (get top x))))
             #_#__ (println new-result)]
          (recur (vec new-result) (rest colls)))))))

(defcheck solution-df84f55c
  (fn [lst]
    (let [p (fn p [pos lst]
              (if (= 1 (count lst))
                (nth (first lst) pos)
                (min (+ (nth (first lst) pos) (p pos (rest lst)))
                  (+ (nth (first lst) pos) (p (inc pos) (rest lst))))))]
      (p 0 lst))))

(defcheck solution-e0212da8
  (fn triangle-minimum-path' [xss]
    (let [rows (count xss)
          mpfn (fn mfn [i r accum]
                 (if (zero? (- r rows))
                   accum
                   [(mfn i (inc r) (+ accum (nth (nth xss r) i)))
                    (mfn (inc i) (inc r) (+ accum (nth (nth xss r) (inc i))))]))]
      (->> (mpfn 0 1 (nth (nth xss 0) 0))
        flatten
        (apply min)))))

(defcheck solution-e0564d93
  (fn [ns]
    (apply
      min
      (reduce
        (fn [r1 r2]
          (let [x (map #(apply + %) (map vector r1 r2))
                y (map #(apply + %) (map vector r1 (rest r2)))]
            (map #(apply min %) (map vector (conj (into [] x) (last y)) (cons (first x) y)))))
        ns))))

(defcheck solution-e0dd211d
  (fn [tri]
    (let [merge-rows (fn [s r]
                       (->> (interleave s s)
                         (drop 1)
                         (butlast)
                         (interleave (interleave r r))
                         (partition 2)
                         (map #(apply + %))
                         (partition 2)
                         (map #(apply min %))))]
      (first (reduce merge-rows (reverse tri))))))

(defcheck solution-e109fed1
  (fn [triangle]
    (first (reduce (fn [min-row current-row]
                     (map (fn [n pair]
                            (+ n (apply min pair)))
                       current-row
                       (partition 2 1 min-row)))
             (reverse triangle)))))

(defcheck solution-e16737b7
  (letfn [(X [s]
            (if (= 1 (count s)) s
                                (map min (rest s) (butlast s))))

          (R [l l*]
            (if (empty? l*) l
                            (recur (map + (X l) (first l*))
                              (rest l*))))
          (P [t]
            (let [t (reverse t)]
              (first (R (first t) (rest t)))))]
    P))

(defcheck solution-e18edaa0
  (fn triangle-minimal-path [triangle]
    (letfn [(indexed [s] (map vector (iterate inc 0) s))
            (next-min-sums [min-sums ints]
              (let [n (count ints)
                    min-sum (fn [[i e]] (+ e (cond (zero? i) (min-sums 0) (= i (dec n)) (min-sums (- n 2))
                                                   :else (min (min-sums (dec i)) (min-sums i)))))]
                (->> (indexed ints) (map min-sum) vec)))]
      (apply min (reduce next-min-sums triangle)))))

(defcheck solution-e1e7db98
  (fn mp[sx] (
               apply min (reduce #(
                                    map-indexed (fn[i v] (
                                                           if (= 0 i)
                                                           (+ (first %1) v)
                                                           (if (= (dec (count %2)) i)
                                                             (+ (last %1) v)
                                                             (+ (min (nth %1 (dec i)) (nth %1 i)) v)
                                                             )

                                                           )) %2

                                    ) (first sx) (rest sx))


               )))

(defcheck solution-e1f5afb7
  (fn tr-min-path [t]
    (let [t (reverse t)
          p-start (map (fn [x] [x (list x)]) (first t))
          choose (fn [ [a & _ :as A] [b & _ :as B]] (if (< a b) A B))
          grow   (fn [n [score stack]] [(+ n score) (cons n stack)])]
      (ffirst (reduce
                (fn [p c-row]
                  (map (fn [n [a b]] (grow n (choose a b)))
                    c-row (partition 2 1 p)))
                p-start (rest t))))))

(defcheck solution-e20e1342
  (fn [tri]
    (let [size (apply max (map count tri))
          paths (loop [paths [[[0 0]]]]
                  (letfn [(child-paths [path]
                            (let [[row col] (last path)]
                              [(conj path [(inc row) col]) (conj path [(inc row) (inc col)])]))]
                    (if (>= ((last (last paths)) 1) (dec size))
                      paths
                      (recur (reduce (fn [res path]
                                       (apply conj res (child-paths path))) [] paths)))))
          path-values (map (fn [path] (map (fn [[row col]] (((vec tri) row) col)) path)) paths)]
      (apply min (map (partial apply +) path-values)))))

(defcheck solution-e2f30ffb
  (fn __ [triangle]
    (apply min (reduce
                 #(map +
                    (interleave %1 %1)
                    ((fn x [y a](if (zero? a) (flatten y) (x (partition 2 1 y) (dec a) ))) %2 (- (count %2) 2)))
                 (first triangle)
                 (rest triangle)))))

(defcheck solution-e31df458
  (fn sum-min-path [triangle]
    ((fn walk-triangle [tri, n, sum]
       (if (= 1 (count (first tri)))
         (walk-triangle (rest tri) 0 ((first tri) 0))
         (loop [s (seq tri), sum sum, n n]
           (if-let [row (first s)]
             (let [li n, ri (inc n), l (row li), r (row ri)]
               (if (< (walk-triangle (next s) li l) (walk-triangle (next s) ri r))
                 (recur (next s) (+ sum l) li)
                 (recur (next s) (+ sum r) ri)))
             sum)))) triangle 0 0)))

(defcheck solution-e37cdd60
  (letfn [(paths [[h & t :as tri] pos]
            (if (seq t)
              (concat (map #(cons (h pos) %) (paths t pos))
                      (map #(cons (h pos) %) (paths t (inc pos))))
              [[(h pos)]]))]
    (fn [rows]
      (apply min (map #(reduce + %) (paths rows 0))))))

(defcheck solution-e462a09d
  (fn [triangle]
    (letfn [(collapse [s]
              (let [above      (first s)
                    below      (second s)
                    vals-above (concat [[(first above)]] (map vec (partition 2 1 above)) [[(last above)]])]
                (lazy-seq (cons (map #(+ %1 (apply min %2)) below vals-above) (rest (rest s))))))]
      (apply min (first (last (take-while #(not-empty (first %)) (iterate collapse triangle))))))))

(defcheck solution-e5894b8
  (fn __ [ls]
    (letfn [(f [i ls]
              (if (empty? ls) 0
                              (+
                               (nth (first ls) i)
                               (min (f i (rest ls))
                                 (f (inc i) (rest ls))))))]
      (f 0 ls))))

(defcheck solution-e5f6a592
  (fn min-way [triangle]
    (->> [[0]]
      (iterate (fn [shifts]
                 (let [l (count (first shifts))
                       mask (concat [0] (repeat l 1))
                       old-shifted (map (partial cons 0) shifts)
                       masked (map (fn [way] (map + way mask)) old-shifted)]
                   (concat old-shifted masked))))
      (drop (- (count triangle) 1))
      first
      (map (fn [shifts]
             (map (fn [nods shift] (drop shift nods)) triangle shifts)))
      (map (fn [way]
             (map first way)))
      (map (partial reduce +))
      (apply min))))

(defcheck solution-e5f81446
  (fn min-path [tr]
    (let [red (fn [v1 v2] (map #(min %1 %2)
                            (map + v1 v2)
                            (map + (rest v1) v2)))]
      (first (reduce red (reverse tr))))))

(defcheck solution-e67b7b49
  (fn f [p [v & m]]
    (+ (v p)
       (if (seq m)
         (min (f p m)
           (f (inc p) m))
         0))) 0)

(defcheck solution-e7bfbc3
  (fn [arg]
    (let [matrix (apply vector arg )]
      (letfn [(adjacent [i j]
                (let [x (get-in matrix [i j])]
                  (if (not (nil? x))
                    (let [a1 (adjacent (inc i) j) a2 (adjacent (inc i) (inc j))]
                      (if (nil? a1)
                        x
                        (if (number? a1)
                          (list (list x a1) (list x a2))
                          (for [y [x] z (apply conj a1 a2)] (conj z y))))))))]
        (apply min (map #(reduce + %)(adjacent 0 0)))))))

(defcheck solution-e7d23b16
  (fn prob-0079
    [tri]

    (let [add-vv (fn add-vv
                   [v1 v2]
                   (vec (map #(+ %1 %2) v1 v2)))

          dup-first (fn dup-first
                      [s]
                      (if-let [f (first s)]
                        (vec (cons f s))
                        nil))

          dup-last (fn dup-last
                     [s]
                     (if-let [l (last s)]
                       (conj s l)
                       nil))

          tri-costs (fn tri-costs
                      [prv cur sel-best]
                      (let [dup-lf  (dup-first prv)
                            dup-rt  (dup-last  prv)
                            bst-prv (map #(sel-best %1 %2) dup-lf dup-rt) ]
                        (add-vv bst-prv cur)))

          best-tri-cost (fn best-tri-cost
                          [sel-best tri]
                          (apply sel-best (reduce #(tri-costs %1 %2 sel-best) tri)))
          ]

      (best-tri-cost min (vec tri)))))

(defcheck solution-e7e115d4
  #(first (reduce
            (fn [x y]
              (map + y
                (map (fn [a b] (min a b)) x (rest x))))
            (reverse %))))

(defcheck solution-e840cd20
  (fn [t]
    (letfn [(nb [[y x] l] (if (= l y) [] [[(+ 1 y) x] [(+ 1 y) (+ 1 x)]]))
            (ct [t]
              (let [l (count t)
                    c (count (last t))]
                (reduce (fn [m c]
                          (let [v (get-in t c)
                                l1 (- l 1)]
                            (assoc m c {:v v :c (nb c l1) :s v})))
                  {}
                  (for [y (range l)
                        x (range c)
                        :when (<= x y)] [y x]))))
            (cmm [t cd]
              (let [{:keys [v c s]} (t cd)]
                (if (empty? c)
                  [s]
                  (mapcat (fn [ncd]
                            (let [sc (:s ncd)
                                  vc (:v ncd)]
                              (if (= sc vc) ;; first visit?
                                (cmm (update-in t [ncd :s] (fn [s1] (+ s s1))) ncd)
                                (cmm (update-in t [ncd :s] (fn [s1] (min s1 (+ v s)))) ncd)))) c))))]
      (apply min (cmm (ct (vec t)) [0 0])))))

(defcheck solution-e880b34e
  (fn triangle-min-path
    [points]
    (letfn [(repeat-each-times [vov times]
              (letfn [(-repeat-each-times
                        [[head & tail] curr times repeated]
                        (if (or (zero? times) (nil? head)) repeated
                                                           (if (zero? curr)
                                                             (recur tail times times (conj repeated head))
                                                             (recur (cons head tail) (dec curr) times (conj repeated head)))))]
                (-repeat-each-times vov (dec times) (dec times) [])))
            (repeat-matrix [[head-xs & tail-xs] times matrix-repeated]
              (if (nil? head-xs) matrix-repeated
                                 (recur tail-xs (dec times) (conj matrix-repeated (repeat-each-times head-xs times)))))
            (trim-sides [xs how-many]
              (reverse
                (drop how-many
                  (reverse
                    (drop how-many xs)))))
            (trim-sides-by-first [matrix]
              (reduce
                (fn [acc xs]
                  (let [half-diff (quot (- (count xs) (count (last acc))) 2)]
                    (conj acc (trim-sides xs half-diff))))
                [(first matrix)] (rest matrix)))]

      (->>
        (-> (repeat-matrix points ((comp inc inc count) points) [])
          trim-sides-by-first)
        (apply map vector)  ;; transpose to cols: map can take as many colls: will map as vector x1 y1... x2 y2...
        (map #(reduce + %)) ;; build sum list
        (apply min))
      )))

(defcheck solution-e89aa1ca
  (fn [vecs]
    (first
      (let [rp (reverse vecs)]
        (loop [top (first rp)
               nxt (second rp)
               rst (drop 2 rp)]
          (if (empty? rst)
            (map + nxt (map min top (rest top)))
            (recur (map + nxt (map min top (rest top)))
              (first rst) (rest rst))))))))

(defcheck solution-e8a28c74
  (fn [x]
    (let [min-step (fn [next-row] (map min (rest next-row) (drop-last next-row)))
          f (fn [r1 r0] (map + r0 (min-step r1)))
          ]
      (->>
        (reverse x)
        (reduce f)
        (first)
        )
      )
    ))

(defcheck solution-e8e76684
  (fn [x] (apply min (reduce (fn [y z] (map + z (map min (concat y [(last y)]) (concat [(first y)] y))) ) x))))

(defcheck solution-e917b6cc
  (fn [s](let[build (fn [seqq](if (= 1 (count seqq)) (concat seqq seqq seqq) (concat [(first seqq)] seqq [(last seqq)])))
              part #(map (fn [[a b]](min a b)) (partition 2 1 %))
              dd (reduce (fn [a b] (map + (part (build a)) b)) (first s) (rest s))]
           (apply min dd))))

(defcheck solution-e91fbde5
  (letfn
   [(zip-with [f & xss]
      (loop [xss xss, acc []]
        (if (some empty? xss)
          acc
          (recur (map next xss)
            (conj acc (apply f (map first xss)))))))
    (mins [xs] (map (partial apply min) xs))
    (combine-layers [bot nxt]
      (zip-with + (mins (partition 2 1 bot)) nxt))]
    (fn [tri]
      (loop [[bot nxt & rst] (reverse tri)]
        (if nxt
          (recur (cons (combine-layers bot nxt) rst))
          (first bot))))))

(defcheck solution-e9cee0c8
  (fn [t]
    (first
      (reduce
        (fn [a b]
          (map #(+ %1 (min %2 %3)) b a (rest a)))
        (reverse t)))))

(defcheck solution-ea07c04f
  (fn triangle-minimal-path [rows]
    (->> rows
      reverse
      (reduce #(map + %2 (map min %1 (rest %1))))
      first)))

(defcheck solution-ea68307b
  (fn min-tri [coll]
    (letfn [(expand [coll]
              (reduce (fn [v n]
                        (if (coll? (last v))
                          (conj (pop v) (conj (last v) n) [n])
                          [[n] [n]]))
                []
                coll))
            (next-level [t-c b-c]
              (map #(+ (apply min %1) %2) t-c b-c))]
      (apply min (reduce #(next-level (expand %1) %2) coll)))))

(defcheck solution-ea93354a
  (fn triangle-min [triangle]
    (loop [t triangle]
      (if (= 1 (count t))
        (first (first t))
        (let [lr (last t)
              slr (last (drop-last t))
              minrow (map-indexed #(min %2 (nth lr (inc %))) (drop-last lr))
              sum (vec (map + slr minrow))]
          (recur (concat (drop-last 2 t) [sum])))
        ))))

(defcheck solution-eb16fc9
  #(apply min (reduce (fn [u d] (map + (map (partial apply min) (partition-all 2 1 (cons (first u) u))) d)) %)))

(defcheck solution-eb2342d9
  (fn [a] (apply min (reduce #(map min (concat (map + % %2) [100]) (map + (concat [100] %) %2)) (first a) (rest a)))))

(defcheck solution-eb34e8dd
  (fn
    [coll]
    ; work from bottom to top, each row calculating min of each pair of
    ; elements and then summing the resultant vector to the vector above
    (letfn [(add-v [vold vnew]
              (lazy-seq
                (if (seq vnew)
                  (cons (+ (min (first vold)
                             (second vold))
                           (first vnew)) (add-v (rest vold) (rest vnew)))
                  nil)))]
      (first (reduce add-v (reverse coll))))))

(defcheck solution-eb38990f
  (fn triangle-min-path [t]
    (reduce min
      (reduce (fn [prev cur]
                (map
                  (fn [[a b] c] (+ c (min a b)))
                  (partition 2 1 (concat [10000] prev [10000]))
                  cur))
        (first t)
        (rest t)))))

(defcheck solution-ebb8d7a1
  (fn[t]
    (letfn [(calc [acc line]
              (vec (map-indexed (fn [i e]
                                  (let [pa (delay (acc (dec i))) pb (delay (acc i))]
                                    (cond (= 1 (count line)) e
                                          (= i 0) (+ e @pb)
                                          (= (inc i) (count line)) (+ e @pa)
                                          :else (if (< @pa @pb) (+ e @pa) (+ e @pb))))) line)))]
      (apply min (reduce calc [] t)))))

(defcheck solution-ec0bfe8e
  (fn [coll]
    (letfn [(min-add [bc lc]
              (->> (partition 2 1 bc)
                (map #(apply min %))
                (map + lc)))]
      (first (reduce min-add (reverse coll))))))

(defcheck solution-ec14eeb6
  (fn [rows]
    (reduce min
      (reduce (fn [scores row]
                (map (fn [a b x]
                       (min (+ a x) (+ b x)))
                  (cons (first scores) scores)
                  (concat scores [(last scores)])
                  row))
        rows))))

(defcheck solution-ec5fddd7
  (fn
    [li]
    (loop [l (rest li) c (first li)]
      (if (empty? l)
        (apply min c)
        (recur (rest l) ((fn
                           [pr nr]
                           (loop [p pr n (vector (+ (first pr) (first nr))) i 1]
                             (if (empty? (rest p))
                               (conj n (+ (last pr) (last nr)))
                               (recur (rest p) (conj n(+ (min (first p) (second p)) (nth nr i))) (inc i))

                               ))
                           )c (first l)))))
    ))

(defcheck solution-ee100259
  (fn hello [xss]
    (letfn [(find-mins [xs ys]
              (loop [an [(+ (first xs) (first ys))]
                     [x y & x-more] xs
                     [a & y-more] (rest ys)]
                (if (nil? y)
                  (conj an (+ x a))
                  (recur (conj an (min (+ x a) (+ y a)))
                    (cons y x-more) y-more))))
            ]
      (apply min
        (reduce find-mins xss)))))

(defcheck solution-eee2cad9
  (fn [t]
    (loop [curr (first t)
           more (rest t)]
      (if-not (seq more)
        (apply min curr)
        (recur
          (vec (map-indexed
                 (fn [i e]
                   (+ e (min
                          (get curr (- i 1) 2147483647)
                          (get curr i 2147483647))))
                 (first more)))
          (rest more))))))

(defcheck solution-eeeeddf4
  (fn [triangle]
    (letfn [(step [rows]
              (if-let [rest-rows (next rows)]
                (let [next-row (step rest-rows)]
                  (mapv (fn [c p1 p2]
                          (min (+ c p1) (+ c p2)))
                    (first rows) next-row (rest next-row)))
                (first rows)))]
      (first (step triangle)))))

(defcheck solution-ef34429a
  (fn [t]
    (let [t (vec t)
          v (map #(partition 2 (interleave (range (count t)) %))
              (map #(reductions + %) ((fn []
                                        (loop [x [[0]]
                                               c 1]
                                          (if (= c (count t))
                                            x
                                            (recur (apply concat (map #(vector (conj % 0) (conj % 1)) x)) (inc c))))))))]
      (apply min (map (fn [z] (reduce + (map #(get-in t (vec %)) z))) v)))))

(defcheck solution-ef73b493
  #(first
     ((fn go [[x & xs]]
        (if xs
          (let [nxt (go xs)]
            (map + x (map min nxt (rest nxt))))
          x)) %)))

(defcheck solution-ef8194c5
  (fn [tri]
    ((fn tmp [tri i j]
       (if (>= i (count tri)) 0
                              (+ (get-in (vec tri) [i j]) (min (tmp tri (inc i) j) (tmp tri (inc i) (inc j)))))
       ) tri 0 0)))

(defcheck solution-efe3fe2e
  (fn myf [coll]
    (letfn [(sub [vec1 vec2]
              (->> (map min (cons (first vec1) vec1) (conj (vec vec1) (last vec1)))
                (map + vec2)))]
      (apply min (reduce sub coll)))))

(defcheck solution-efe51290
  (fn dfs
    [tree]
    (cond
      (empty? tree) 0
      (= 1 (count tree)) (min (first tree))
      :else
      (let [depth (count tree)]
        (loop [to-explore (list {:cost (ffirst tree) :pos [0 0]})
               cur-min 2147483647]
          (if (empty? to-explore)
            cur-min
            (let [node  (peek to-explore)
                  [r c] (:pos node)
                  cost  (:cost node)
                  frontier (pop to-explore)]
              (if
               (< (inc r) depth) ;; More to explore
                (let [next-row (nth tree (inc r))
                      left  {:cost (+ cost (nth next-row c))       :pos [(inc r) c]}
                      right {:cost (+ cost (nth next-row (inc c))) :pos [(inc r) (inc c)]}]
                  (cond
                    (and (< (:cost left)  cur-min)
                         (< (:cost right) cur-min))
                    (recur (conj frontier right left) cur-min)

                    (< (:cost left) cur-min)
                    (recur (conj frontier left) cur-min)

                    (< (:cost right) cur-min)
                    (recur (conj frontier right) cur-min)

                    :else (recur frontier cur-min)))
                ;; Leaf node
                (recur frontier (min cost cur-min))))
            ))))))

(defcheck solution-f0093d5b
  (fn [t]
    (reduce min
      (map (comp last (fn [p]
                        (reduce
                          (fn [[x y a] e]
                            [(+ x e)
                             (inc y)
                             (+ a (nth (nth t (inc y)) (+ x e)))])
                          [0 0 (first (first t))]
                          p)))
        (map
          (fn [n]
            (map first
              (take (dec (count t))
                (rest (iterate (fn [[_ a]] [(mod a 2) (long (/ a 2))])
                        [0 n])))))
          (range (apply * (repeat (dec (count t)) 2))))))))

(defcheck solution-f0934c3c
  (letfn
   [(make-path [[node & tri] path]
      (if (seq tri)
        (->> [(map rest tri) (map butlast tri)]
          (mapcat #(make-path % (concat path node))))
        [(concat path node)]))]
    (fn [tri]
      (->> (make-path tri [])
        (map #(apply + %))
        (apply min)))))

(defcheck solution-f0b345dc
  (fn [triangle]
    (letfn [(sum [xs] (reduce + xs))
            (min-path [& args]
              (-> (sort-by sum args)
                (first )))]
      (->>
        (reduce
          (fn [row1 row2]
            (vec
              (map-indexed
                #(cond (== %1 0)                  (conj (row1 0) %2)
                       (== %1 (dec (count row2))) (conj (row1 (dec (count row1))) %2)
                       :else                      (conj (min-path (row1 %1) (row1 (dec %1))) %2))
                row2)))
          (vector (first triangle)) (next triangle))
        (group-by sum)
        (keys ,)
        (apply min)))))

(defcheck solution-f12a96be
  (fn [pyramid]
    (->> pyramid
      reverse
      (reduce (fn [r1 r2] (map #(+ %1 (min %2 %3)) r2 r1 (next r1))))
      first)))

(defcheck solution-f12efb8c
  (fn [x] (first (reduce #(map + (map min (butlast %1) (rest %1)) %2) (reverse x)))))

(defcheck solution-f19d48af
  (fn path
    ([rows] (+ (ffirst rows)
               (path (next rows) 0)))
    ([rows i]
     (if (seq rows)
       (apply min (for [i [i (inc i)]]
                    (+ (nth (first rows) i)
                       (path (next rows) i))))
       0))))

(defcheck solution-f1abbfac
  (fn [t]
    (apply min (map #(reduce + %)
                 (let [max-depth (count t)]
                   ((fn min-path [y x result]
                      (let [new-result (conj result (nth (nth t y) x))]
                        (if (= (inc y) max-depth)
                          [new-result]
                          (concat
                           (min-path (inc y) x new-result)
                           (min-path (inc y) (inc x) new-result)))))
                    0 0 []))))))

(defcheck solution-f1d35e7d
  (fn [s]
    (let [nested-paths (loop [[r & rs] (reverse s)
                              ret []]
                         (if-not (seq ret)
                           (recur rs (map vector r))
                           (if r
                             (recur rs
                               (let [p (partition 2 1 ret)]
                                 (for [[a [l1 l2]] (map vector r p)]
                                   [(conj l1 a) (conj l2 a)])))
                             ret)))
          f (fn f [[r l & a]]
              [(concat r a) (concat l a)])
          g (fn g [x] (if-not (coll? (ffirst x))
                        x
                        (g (mapcat f x))))]
      (->> nested-paths
        g
        (group-by (partial apply +))
        (sort-by first)
        ffirst))))

(defcheck solution-f239813c
  (fn [tr]
    (apply min
      (reduce (fn [final elem]
                (if (empty? final)
                  elem
                  (map #(+ %1 (apply min %2))
                    elem
                    (partition 2 1 (concat (take 1 final) final (take-last 1 final))))
                  )) '() tr))
    ))

(defcheck solution-f2bf1703
  #(first (reduce (fn [x y]
                    (map (fn [index]
                           (+ (nth y index) (min (nth x index) (nth x (inc index)))))
                      (range (count y))))
            (reverse %))))

(defcheck solution-f3852bd2
  (fn [triangle]
    (let [length    (apply max (map count triangle))
          reducer   (fn [a b] (for [x a y b
                                    :let [z (last x)]
                                    :when (or (= y z) (= y (inc z)))]
                                (conj x y)))
          paths     (reduce reducer [[0]] (map (comp range inc) (range 1 length)))
          path-sums (map (fn [p] (apply + (map #(nth %1 %2) triangle p))) paths)]
      (apply min path-sums))))

(defcheck solution-f386eae8
  (fn petong
    [triangle]
    (let [n (vec triangle)
          count-lines (count n)
          parent (fn parent [row col]
                   (let [children (fn [row col]
                                    (min (parent (inc row) col)
                                      (parent (inc row) (inc col))))]
                     (if (= row count-lines)
                       0
                       (+ (get-in n [row col])
                          (children row col)))))]
      (parent 0 0))))

(defcheck solution-f3a37e06
  (fn f
    ([t] (f 0 t))
    ([i [c & t]]
     (+ (c i)
        (if t
          (min (f i t) (f (+ i 1) t))
          0)))))

(defcheck solution-f3ac261e
  (fn [t]
    (letfn [(min-tri-path [t]
              (if (empty? (rest t))
                (first t)
                (let [mins (min-tri-path (rest t))]
                  (->> (partition 2 1 mins)
                    (map (partial apply min))
                    (map  + (first t))))))]
      (first (min-tri-path t)))))

(defcheck solution-f3be45c9
  (fn [in]
    (let [tri (vec in)]
      ((fn minBranch [row pos]
         (let [value (get (get tri row) pos)]
           (if (= (dec (count tri)) row)
             value
             (+ value
                (min
                  (minBranch (inc row) pos)
                  (minBranch (inc row) (inc pos)))))))
       0 0))))

(defcheck solution-f400914a
  (fn path [s]
    (->> s
      (reduce
        (fn [p n]
          (vec
            (for
             [i (range (count n))
              :let [nb (filter #(> % -1) [(dec i) i])
                    ncurrent (nth n i)
                    pcurrent (subvec p (first nb))
                    pcurrent (take (count nb) pcurrent)
                    result (map
                             #(map (partial + ncurrent)
                                (flatten (conj [] %)))
                             pcurrent)]]
              (flatten result)))))
      (flatten)
      (apply min))))

(defcheck solution-f440c994
  (fn [a]
    (apply min
      (reduce
        (fn [ l1 l2 ]
          (let [ inf ##Inf
                l1g (conj l1 inf )
                l1d (vec (cons inf l1))]
            (vec (map #(+ %1 (min %2 %3)) l2 l1g l1d))))
        a))))

(defcheck solution-f5125a3a
  #(nth (reduce (fn [b t] (map + t (map min b (rest b))))
          (reverse %)) 0))

(defcheck solution-f55c9fb6
  (fn min-path [tri]
    (loop [r (reverse tri)]
      (if (= 1 (count r))
        (ffirst r)
        (recur (cons
                 (map min
                   (map + (second r) (butlast (first r)))
                   (map + (second r) (rest (first r))))
                 (drop 2 r)))))))

(defcheck solution-f5bfd840
  (fn collapse [p] (let [combine (fn [a b] (map + (map #(apply min %) (partition 2 1 a)) b))] (first (reduce combine (reverse p))))))

(defcheck solution-f60cc09b
  (letfn [(next-layer [paths layer]
            (for [path paths
                  n (range 2)]
              (let [[old-value old-index] path
                    index (+ old-index n)
                    value (+ old-value (get layer index))]
                [value index])))]
    (fn [triangle]
      (apply min
        (map first
          (reduce next-layer
            [(conj (first triangle) 0)]
            (rest triangle)))))))

(defcheck solution-f82212e8
  (fn [c]
    (let [c (vec c)]
      (apply min
        (flatten
          ((fn s [i j t]
             (if-let [x (get-in c [i j])]
               (keep #(s (inc i) % (+ t x))
                 [j (inc j)])
               t))
           0 0 0))))))

(defcheck solution-f828d2e8
  (fn sup
    ([tri]
     (let [paths (map #(cons (nth (first tri) 0) %) (sup (rest tri) 0))]
       (apply min (map (partial reduce +) paths))))
    ([tri i]
     (if (< 1 (count tri))
       (mapcat
         (fn [j]
           (map
             (fn [subtri]
               (cons (nth (first tri) j) subtri))
             (sup (rest tri) j)))
         (range i (+ i 2)))
       (if (< (inc i) (count (first tri)))
         (map #(vector %) (subvec (first tri) i (+ i 2))))))))

(defcheck solution-f87d0385
  (fn [t] (first (reduce
                   #(map + (map (partial apply min) (partition 2 1 %)) %2) (reverse t)))))

(defcheck solution-f8bd75eb
  (fn [t]
    (letfn [(min-path-sum [r i]
              (if (>= r (count t))
                0
                (+ (nth (nth t r) i)
                   (min
                     (min-path-sum (inc r) i)
                     (min-path-sum (inc r) (inc i))))))]
      (min-path-sum 0 0))))

(defcheck solution-f902ab2f
  (fn minitria [xss]
    (letfn [

            (mini2 [xs ys]  ;xs shorter
              (let [x1 (first xs) y1 (first ys) y2 (second ys) ]
                (if x1
                  ( cons (min (+ x1 y1 ) (+ x1 y2) )  (mini2 (rest xs) (rest ys)))
                  ()
                  )

                )
              )


            ]
      (let [rv (reverse xss)]
        (first  (reduce #(mini2 %2 %1) rv) )
        )
      )
    ))

(defcheck solution-f96d8f2
  (fn [t]
    (first (reduce #(map min (map + %2 %) (map + %2 (drop 1 %))) (reverse t)))))

(defcheck solution-fa0cc5cc
  #(loop [c (butlast %)
          r (last %)]
     (if (= (count r) 1)
       (first r)
       (recur
         (butlast c)
         (map (fn [a b](+ (apply min a) b)) (partition 2 1 r) (last c))))))

(defcheck solution-fa8b9ca9
  (fn [triangle]
    (first (reduce (fn [paths row]
                     (map (fn [node left right]
                            (min (+ node left) (+ node right)))
                       row (butlast paths) (rest paths)))
             (reverse triangle)))))

(defcheck solution-fac4b632
  (letfn [
          (redrow [coll]  (map #(apply min %) (partition 2 1 coll)))
          (calc [a b] (map + b (redrow a)))
          (red [coll] (first(reduce calc (reverse coll))))] red))

(defcheck solution-fb1f7c1a
  (fn [t]
    (let [num (count t)]
      (letfn [(path [level n p a]
                (if (= num level)
                  (conj a p)
                  (concat (path (inc level) n       (conj p (nth (nth t level) n) ) a)
                          (path (inc level) (inc n) (conj p (nth (nth t level) n) ) a))
                  ))]
        (apply min (map #(apply + %) (path 0 0 [] #{})))))))

(defcheck solution-fb44a3b9
  (fn min-cost
    ([triangle] (min-cost triangle 0))
    ([rows index]
     (if (empty? rows)
       0
       (+
        ((first rows) index)
        (min
          (min-cost (rest rows) index)
          (min-cost (rest rows) (inc index))))))))

(defcheck solution-fb91279e
  (fn [tri]
    (letfn [(path-pairs [path]
              (if (seq path)
                (let [p1 (cons (first path) path)
                      p2 (conj path (last path))]
                  (map min p1 p2))
                [0]))

            (addpath [path line]
              (into [] (map + (path-pairs path) line)))]

      (apply min (reduce addpath [] tri)))))

(defcheck solution-fb9b5ab1
  #(letfn [(minsum [tri i j]
             (+ (nth (nth tri i) j)
                (if (< i (dec (count tri)))
                  (min (minsum tri (inc i) j)
                    (minsum tri (inc i) (inc j)))
                  0)))]
     (minsum % 0 0)))

(defcheck solution-fbf01527
  (fn [t]
    (letfn [(mins [s] (map min `[~@s ~(last s)] `[~(first s) ~@s]))]
      (reduce min (reduce #(map + (mins %1) %2) t)))))

(defcheck solution-fc2b6444
  (fn [tree]
    (letfn [(treesize [v] (reduce + 0 v))
            (triangle-minimal-path [current-tree index tree]
              (if (empty? tree) current-tree
                                (let [ [t & ts] tree
                                      left-minimal-tree (triangle-minimal-path (conj current-tree (nth t index)) index ts)
                                      right-minimal-tree (triangle-minimal-path (conj current-tree (nth t (inc index))) (inc index) ts)]
                                  (min-key treesize left-minimal-tree right-minimal-tree))))
            (triangle-minimal-path2 [tree]
              (let [ index 0
                    current-tree []
                    [t & ts] tree ]
                (triangle-minimal-path (conj current-tree (nth t index)) index ts)))]
      (treesize(triangle-minimal-path2 tree)))))

(defcheck solution-fc3bc2f6
  #(apply min
     (reduce
       (fn [a v] (map min
                   (map + (cons ##Inf a) v)
                   (map + (concat a [##Inf]) v)))
       %)))

(defcheck solution-fcbf7dc0
  (fn [tri]
    (loop [tri (reverse tri)
           cur (first tri)]
      #_(println tri)
      #_(println cur)
      (if (= (count tri) 1)
        (first cur)
        (recur (rest tri) (map +
                            (map
                              #(min (first %) (second %))
                              (partition 2 1 cur))
                            (second tri)))))))

(defcheck solution-fd9012
  (fn [t]
    (letfn [(f [m a q]
              (if (empty? m) a
                             (apply min (map #(f (rest m) (+ % a) %) ((first m) q)))))]
      (f (map #(into {} (apply map vector %))
           (map vector t (rest (map #(partition 2 1 %) t)))) (ffirst t) (ffirst t)))))

(defcheck solution-fdea5b5e
  (fn f [t]
    (if
     (= (count t) 1)
      (ffirst t)
      (let [left
            (f (map
                 #(subvec % 0 (- (count %) 1)) (rest t)))
            right
            (f (map
                 #(subvec % 1) (rest t)))]
        (+ (ffirst t) (min left right))))))

(defcheck solution-fe1be22
  (fn [t]
    (let [min-step (fn [s] (map (partial apply min) (partition 2 1 s)))]
      (first
        (reduce
          #(map + (min-step %1) %2)
          (reverse t))))))

(defcheck solution-fea7cf8a
  (fn triangle-min-path [triangle]
    (let [topdown (reverse triangle)]
      (apply min
        (for [end (range (count (first topdown)))]
          (loop [topdown topdown length 0 pos end]
            (cond
              (nil? (next topdown)) (+ length ((first topdown) 0))
              (= 0 pos) (recur (next topdown) (+ length ((first topdown) pos)) 0)
              (= pos (count (fnext topdown))) (recur (next topdown) (+ length ((first topdown) pos)) (dec pos))
              (< ((fnext topdown) (dec pos)) ((fnext topdown) pos)) (recur (next topdown) (+ length ((first topdown) pos)) (dec pos))
              :else (recur (next topdown) (+ length ((first topdown) pos)) pos))))))))

(defcheck solution-febf3115
  (fn triangleX [t]
    (letfn
     [
      (triangleRec [t i sum]
        (if (empty? t)
          sum
          (let [
                t0 (first t)
                ti (get t0 i)
                i1 (inc i)
                ti1 (get t0 i1)
                ]
            (min (triangleRec (rest t) i (+ sum ti)) (triangleRec (rest t) i1 (+ sum ti1)))
            )
          )
        )
      ]
      (triangleRec (rest t) 0 (first (first t)) )
      )
    ))

(defcheck solution-feece0c4
  (fn mp [t]
    (letfn [ (leftsubtree [t] (map #(take (- (count %) 1) %) (rest t)))
            (rightsubtree [t] (map #(rest %) (rest t)))
            (emptytree? [t] (= t ()))
            (rootvalue [t] (first (first t)))
            ]
      (if (emptytree? t)
        0
        (+ (rootvalue t) (min (mp (leftsubtree t)) (mp (rightsubtree t))))))))

(defcheck solution-ff490eda
  (fn [coll]
    (first (reduce #(->>
                      (partition 2 1 %)
                      (map (fn [[a b]] (min a b)))
                      (map + %2))
             (reverse coll)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-79))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
